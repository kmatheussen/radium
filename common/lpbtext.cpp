/* Copyright 2016 Kjetil S. Matheussen

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
#include "vector_proc.h"
#include "list_proc.h"
#include "realline_calc_proc.h"
#include "undo.h"
#include "undo_lpbs_proc.h"
#include "LPB_proc.h"
#include "data_as_text_proc.h"

#include "lpbtext_proc.h"


int LPBTEXT_subsubtrack(struct Tracker_Windows *window){
  if (window->curr_track != LPBTRACK)
    return -1;
  
  int curr_track_sub = window->curr_othertrack_sub;

  if (curr_track_sub < 0)
    return -1;

  if (curr_track_sub >= 2)
    return -1;

  return curr_track_sub;
}

bool LPBTEXT_keypress(struct Tracker_Windows *window, struct WBlocks *wblock, int realline, Place *place, int key){
  int subsubtrack = LPBTEXT_subsubtrack(window);

  if (subsubtrack==-1)
    return false;

  QVector<LPBs*> lpbs = LPBs_get(wblock, realline);

    
  if (lpbs.size() == 0) {

    // NO ELEMENTS

    if (key == EVENT_DEL)
      return true;
    
    data_as_text_t dat = DAT_get_newvalue(subsubtrack, key, root->lpb, LOGTYPE_HOLD, 1, 99, 1, 99, false, false, false);

    if (dat.is_valid==false)
      return false;

    ADD_UNDO(LPBs_CurrPos(window));
    SetLPB(wblock->block, place, dat.value);

  } else {

    // ONE ELEMENT (or more than one element)

    struct LPBs *lpb = lpbs.last();
  
    if (key == EVENT_DEL) {

      ADD_UNDO(LPBs_CurrPos(window));  
      RemoveLPB(wblock->block, lpb);
 
    } else {

      data_as_text_t dat = DAT_get_overwrite(lpb->lpb, LOGTYPE_HOLD, subsubtrack, key, 1, 99, 1, 99, false, false);

      if (dat.is_valid==false)
        return false;

      SetLPB(wblock->block, &lpb->l.p, dat.value);
      
    }    
  }

  return true;
}
  
