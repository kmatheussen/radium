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
#include "realline_calc_proc.h"
#include "vector_proc.h"

#include "trackreallines2_proc.h"

int find_realline_for_end_pitch(const struct WBlocks *wblock, const Place *p){
  int realline = FindRealLineFor(wblock, 0, p);
  if (p->counter==0 && realline>=1)
    realline--;
  
  return realline;
}

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

static void add_tr(const struct WBlocks *wblock, Trss &trss, const TrackRealline2 &tr){
  int realline;

  if (tr.is_end_pitch)
    realline = find_realline_for_end_pitch(wblock, &tr.p);
  else
    realline = FindRealLineFor(wblock, 0, &tr.p);

  TRS_INSERT_PLACE(trss[realline], tr);
}
                   
static void add_pitch(const struct WBlocks *wblock, Trss &trss, struct Notes *note, struct Pitches *pitch){
  TrackRealline2 tr = {};
  tr.p = pitch->l.p;
  tr.note = note;
  tr.pitch = pitch;
  add_tr(wblock, trss, tr);
}

static void add_note(const struct WBlocks *wblock, Trss &trss, struct Notes *note){
  TrackRealline2 tr = {};
  tr.p = note->l.p;
  tr.note = note;
  add_tr(wblock, trss, tr);

  struct Pitches *pitch = note->pitches;
  while(pitch != NULL){
    add_pitch(wblock, trss, note, pitch);
    pitch = NextPitch(pitch);
  }

  if (note->pitch_end > 0) {
    TrackRealline2 tr = {};
    tr.p = note->end;
    tr.note = note;
    tr.is_end_pitch = true;
    add_tr(wblock, trss, tr);
  }
}

static void add_stop(const struct WBlocks *wblock, Trss &trss, struct Stops *stop){
  TrackRealline2 tr = {};
  tr.p = stop->l.p;
  tr.stop = stop;
  add_tr(wblock, trss, tr);
}

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

  struct Stops *stop = wtrack->track->stops;
  while(stop!=NULL){
    add_stop(wblock, trss, stop);
    stop = NextStop(stop);
  }

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
