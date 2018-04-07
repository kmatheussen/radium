/* Copyright 2012 Kjetil S. Matheussen

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



#include "nsmtracker.h"
#include "placement_proc.h"
#include "vector_proc.h"
#include "windows_proc.h"
#include "clipboard_range_calc_proc.h"

#include "range_proc.h"

bool is_track_ranged(struct WBlocks *wblock, struct WTracks *wtrack){
  return wblock->isranged && wtrack->l.num>=wblock->rangex1 && wtrack->l.num<=wblock->rangex2;
}

bool is_realline_ranged(struct WBlocks *wblock, int realline){
  if (realline < 0 || realline >= wblock->num_reallines){
    R_ASSERT_NON_RELEASE(false);
    return false;
  }
  Place p = wblock->reallines[realline]->l.p;
  return wblock->isranged && p_Greater_Or_Equal(p, wblock->rangey1) && p_Less_Or_Equal(p, wblock->rangey2);
}

vector_t get_all_ranged_notes(struct WBlocks *wblock){
  vector_t v = {0};

  struct WTracks *wtrack = wblock->wtracks;
  while(wtrack!=NULL){
    struct Tracks *track = wtrack->track;
    if(is_track_ranged(wblock,wtrack)){
      struct Notes *note = track->notes;
      while(note!=NULL){
        if(IsPlaceRanged(wblock,&note->l.p))
          VECTOR_push_back(&v, note);
        note=NextNote(note);
      }
    }
    wtrack=NextWTrack(wtrack);
  }

  return v;
}


