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
                                
static void add_tr(const struct WBlocks *wblock, vector_t *trs, const TrackRealline2 *tr){
  int realline;

  if (tr->is_end_pitch)
    realline = find_realline_for_end_pitch(wblock, &tr->p);
  else
    realline = FindRealLineFor(wblock, 0, &tr->p);
      
  vector_t *v = &trs[realline];
  VECTOR_insert_place(v, &tr->p);
}
                   
static void add_pitch(const struct WBlocks *wblock, vector_t *trs, struct Notes *note, struct Pitches *pitch){
  TrackRealline2 *tr = talloc(sizeof(TrackRealline2));
  tr->p = pitch->l.p;
  tr->note = note;
  tr->pitch = pitch;
  add_tr(wblock, trs, tr);
}

static void add_note(const struct WBlocks *wblock, vector_t *trs, struct Notes *note){
  TrackRealline2 *tr = talloc(sizeof(TrackRealline2));
  tr->p = note->l.p;
  tr->note = note;
  add_tr(wblock, trs, tr);

  struct Pitches *pitch = note->pitches;
  while(pitch != NULL){
    add_pitch(wblock, trs, note, pitch);
    pitch = NextPitch(pitch);
  }

  if (note->pitch_end > 0) {
    TrackRealline2 *tr = talloc(sizeof(TrackRealline2));
    tr->p = note->end;
    tr->note = note;
    tr->is_end_pitch = true;
    add_tr(wblock, trs, tr);
  }
}

static void add_stop(const struct WBlocks *wblock, vector_t *trs, struct Stops *stop){
  TrackRealline2 *tr = talloc(sizeof(TrackRealline2));
  tr->p = stop->l.p;
  tr->stop = stop;
  add_tr(wblock, trs, tr);
}

static int find_next_used_trackrealline(const struct WBlocks *wblock, vector_t *trs, int realline){
  while(realline < wblock->num_reallines && trs[realline].num_elements==0)
    realline++;
  return realline;
}

static void distribute_trackreallines(const struct WBlocks *wblock, vector_t *tr, vector_t *trs, int realline1, int realline2){  
  int num_elements      = tr->num_elements;
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
    VECTOR_copy_elements(tr, element_pos, num_elements_to_copy, &trs[realline]);

    element_pos += num_elements_to_copy;    
    num_elements_left -= num_elements_to_copy;

    if (num_elements_left==0)
      break;
  }
}

static void spread_trackreallines(const struct WBlocks *wblock, vector_t *trs){
  int realline1 = 0;
  while(realline1 < wblock->num_reallines){
    if (trs[realline1].num_elements > 1) {

      int realline2 = find_next_used_trackrealline(wblock, trs, realline1+1);
      //printf("next realline: %d -> %d\n",realline1,realline2);
      if (realline2 > realline1+1) {
        vector_t *tr = VECTOR_move(&trs[realline1]);
        distribute_trackreallines(wblock, tr, trs, realline1, realline2);
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

static void TRS_print(const struct WBlocks *wblock, vector_t *trs){
  int realline;
  for(realline=0 ; realline<wblock->num_reallines ; realline++){
    vector_t *tr = &trs[realline];
    int notenum = get_notenum(tr);
    printf("%d: %d\n",realline,notenum);
  }
}
#endif

// Returns a pointer to an array of vectors (one vector for each realline), not a pointer to a vector.
vector_t *TRS_get(const struct WBlocks *wblock, const struct WTracks *wtrack){
  int num_reallines = wblock->num_reallines;
  vector_t *trs = talloc(sizeof(vector_t) * num_reallines);

  struct Notes *note = wtrack->track->notes;
  while(note!=NULL){
    add_note(wblock, trs, note);
    note = NextNote(note);
  }

  struct Stops *stop = wtrack->track->stops;
  while(stop!=NULL){
    add_stop(wblock, trs, stop);
    stop = NextStop(stop);
  }

  spread_trackreallines(wblock, trs);
    
  //  if (wtrack->l.num==0)
  //   TRS_print(wblock,trs);
  
  return trs;
}

vector_t *TR_get(const struct WBlocks *wblock, const struct WTracks *wtrack, int realline){
  vector_t *trs = TRS_get(wblock, wtrack);
  return &trs[realline];
}
