/* Copyright 2014 Kjetil S. Matheussen

This program is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public License
as published by the Free Software Foundation; either version 2
of the License, or (at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA. */

#include <math.h>


#include "nsmtracker.h"
#include "TimeData.hpp"

#include "realline_calc_proc.h"
#include "vector_proc.h"

#include "trackreallines2_proc.h"

int find_realline_for_end_pitch(const struct WBlocks *wblock, const Place *p){
  int realline = FindRealLineFor(wblock, 0, p);
  if (p->counter==0 && realline>=1)
    realline--;
  
  return realline;
}

/*
static void insert_place(Trs &trs, const TrackRealline2 &tr){
  for(int i=0 ; i<trs.size() ; i++){ // could be optimized by using binary search, but binary search is hard to get correct. That speedup is not needed for now anyway.
    const TrackRealline2 &tr2 = trs.at(i);
    
    if (PlaceLessThan(&tr.p, &tr2.p)) {
      trs.insert(i, tr);
      return;
    }
  }

  trs.push_back(tr);
}
*/

static void add_tr(const struct WBlocks *wblock, Trss &trss, const Place &place, TR2_Type type, struct Notes *note, int64_t node_id, float pitch, int chance, int pitchnum = -1, int realline_for_end = -1){
  TrackRealline2 tr = {};
  
  tr.p = place;
  tr.type = type;
  
  tr.note = note;
  tr.pitchnum = pitchnum;
  tr.node_id = node_id;
  tr.pitch = pitch;
  tr.chance = chance;
  tr.pitchnum = pitchnum;
  
  if (type!=TR2_STOP){
    R_ASSERT(tr.note!=NULL);
  }

  if (type==TR2_PITCH){
    
    //R_ASSERT(tr.note->pitches != NULL);
    R_ASSERT_NON_RELEASE(r::PitchTimeData::Reader(tr.note->_pitches).size()>0);
    R_ASSERT(tr.pitchnum >= 0);
    
  }else{
    
    R_ASSERT_NON_RELEASE(tr.pitchnum==-1);
    
  }

  int realline;

  if (type==TR2_NOTE_END)
    realline = find_realline_for_end_pitch(wblock, &tr.p);
  else if (realline_for_end >= 0)
    realline = R_MIN(realline_for_end, FindRealLineFor(wblock, 0, &tr.p));
  else
    realline = FindRealLineFor(wblock, 0, &tr.p);

  TRS_INSERT_PLACE(trss[realline], tr);
}
                   
static void add_pitch(const struct WBlocks *wblock, Trss &trss, struct Notes *note, const r::Pitch &pitch, int pitchnum, int realline_for_end){
  add_tr(wblock, trss, ratio2place(pitch._time), TR2_PITCH, note, pitch._id, pitch._val, pitch._chance, pitchnum, realline_for_end);
}

static void add_note(const struct WBlocks *wblock, Trss &trss, struct Notes *note){
  add_tr(wblock, trss, note->l.p, TR2_NOTE_START, note, -1, note->note, note->chance);

  const Place end_place = ratio2place(note->end);
  int realline_for_end = note->pitch_end <= 0 ? -1 : find_realline_for_end_pitch(wblock, &end_place);
  
  int pitchnum = 0;
  r::PitchTimeData::Reader reader(note->_pitches);
  for(const r::Pitch &pitch : reader){
    add_pitch(wblock, trss, note, pitch, pitchnum, realline_for_end);
    pitchnum++;
  }

  /*
  struct Pitches *pitch = note->pitches;
  while(pitch != NULL){
    add_pitch(wblock, trss, note, pitch, pitchnum);
    pitch = NextPitch(pitch);
    pitchnum++;
  }
  */
  
  if (note->pitch_end > 0) {
    add_tr(wblock, trss, ratio2place(note->end), TR2_NOTE_END, note, -1, note->pitch_end, -1);
  }
}

#if 1
static void add_stop(const struct WBlocks *wblock, Trss &trss, const r::Stop stop){
  add_tr(wblock, trss, ratio2place(stop._time), TR2_STOP, NULL, -1, NOTE_STP, -1);
}
#else
static void add_stop(const struct WBlocks *wblock, Trss &trss, struct Stops *stop){
  add_tr(wblock, trss, stop->l.p, TR2_STOP, NULL);
}
#endif

static int find_next_used_trackrealline(const struct WBlocks *wblock, Trss &trss, int realline){
  while(realline < wblock->num_reallines && trss.contains(realline)==false)
    realline++;
  return realline;
}

// 'tr' contains more than one element per line, so we spread it to several reallines. (point is to try to avoid 'MUL' notes)
// Before calling, we found that all lines between realline1 and realline2 are free.
static void distribute_trackreallines(const struct WBlocks *wblock, Trs trs, Trss &trss, int realline1, int realline2){  
  int num_elements      = trs.size();
  int num_lines         = realline2 - realline1;
  //int elements_per_line = R_MAX(1, num_elements / num_lines);
  int elements_per_line = ceilf((float)num_elements / (float)num_lines);
  R_ASSERT_RETURN_IF_FALSE(elements_per_line>0);
  
  int element_pos = 0;
  int num_elements_left = num_elements;

  int realline;
  for(realline=realline1 ; realline < realline2 ; realline++){
    int num_elements_to_copy = R_MIN(num_elements_left, elements_per_line);
    //printf("copying from %d: %d -> %d, into %d\n", realline1, element_pos, element_pos+num_elements_to_copy, realline);

    for(int i=0;i<num_elements_to_copy;i++)
      trss[realline].push_back(trs.at(element_pos+i));
                               
    element_pos += num_elements_to_copy;    
    num_elements_left -= num_elements_to_copy;

    if (num_elements_left==0)
      break;
  }
}

static void spread_trackreallines(const struct WBlocks *wblock, Trss &trss){
  int realline1 = 0;
  while(realline1 < wblock->num_reallines){
    if (trss.contains(realline1) && trss[realline1].count() > 1) {

      int realline2 = find_next_used_trackrealline(wblock, trss, realline1+1);
      //printf("next realline: %d -> %d\n",realline1,realline2);
      if (realline2 > realline1+1) {
        Trs trs = trss.value(realline1);
        trss.remove(realline1);
        distribute_trackreallines(wblock, trs, trss, realline1, realline2);
      }

      realline1 = realline2;

    } else {

      realline1++;

    }
  }
}

#if 0
static int get_notenum(vector_t *tr){
  if (tr->num_elements==0)
    return 0;

  if (tr->num_elements>1)
    return NOTE_MUL;

  TrackRealline2 *tr2 = (TrackRealline2*)tr->elements[0];
  
  if (tr2->pitch != NULL)
    return tr2->pitch->note;

  else if (tr2->note != NULL)
    return tr2->note->note;

  else
    return NOTE_STP;
}

static void TRSS_print(const struct WBlocks *wblock, Trss &trss){
  int realline;
  for(realline=0 ; realline<wblock->num_reallines ; realline++){
    vector_t *tr = &trss[realline];
    int notenum = get_notenum(tr);
    printf("%d: %d\n",realline,notenum);
  }
}
#endif

// Returns a pointer to AN ARRAY of vectors (one vector for each realline), not a pointer to a vector.
const Trss TRSS_get(const struct WBlocks *wblock, const struct WTracks *wtrack){
  Trss trss;
  
  struct Notes *note = wtrack->track->gfx_notes!=NULL ? wtrack->track->gfx_notes : wtrack->track->notes;
  while(note!=NULL){
    add_note(wblock, trss, note);
    note = NextNote(note);
  }

#if 1
  r::StopTimeData::Reader reader(wtrack->track->stops2);
  for(const r::Stop &stop : reader) {
    add_stop(wblock, trss, stop);
  }
#else
  struct Stops *stop = wtrack->track->stops;
  while(stop!=NULL){
    add_stop(wblock, trss, stop);
    stop = NextStop(stop);
  }
#endif
  
  spread_trackreallines(wblock, trss);
    
  //  if (wtrack->l.num==0)
  //   TRSS_print(wblock,trss);
  
  return trss;
}

const Trs TRS_get(const struct WBlocks *wblock, const struct WTracks *wtrack, int realline){
  R_ASSERT(realline<wblock->num_reallines);
  R_ASSERT(realline>=0);
  Trss trss = TRSS_get(wblock, wtrack);
  return trss[realline];
}
