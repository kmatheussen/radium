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
#include "undo_swings_proc.h"
#include "swing_proc.h"
#include "data_as_text_proc.h"
#include "player_pause_proc.h"
#include "time_proc.h"

#include "swingtext_proc.h"


int SWINGTEXT_subsubtrack(const struct Tracker_Windows *window, const struct WTracks *wtrack){

  int curr_track_sub;
      
  if (wtrack!=NULL) {

    if (wtrack->swingtext_on == false)
      return -1;

    curr_track_sub = window->curr_track_sub;
    
  } else {
    
    if (window->curr_track != SWINGTRACK)
      return -1;
    
    curr_track_sub = window->curr_othertrack_sub;
    
  }
  
  if (curr_track_sub < 0)
    return -1;
  
  if (curr_track_sub > 2)
    return -1;
  
  return curr_track_sub;
}

bool SWINGTEXT_keypress(struct Tracker_Windows *window, struct WBlocks *wblock, struct WTracks *wtrack, int realline, const Place *place, int key){
    
  int subsubtrack = SWINGTEXT_subsubtrack(window, wtrack);

  if (subsubtrack==-1)
    return false;

  struct Blocks *block = wblock->block;
  struct Tracks *track = wtrack==NULL ? NULL : wtrack->track;

  QVector<Swing*> swings = Swings_get(wblock, track, realline);

    
  if (swings.size() == 0) {

    // NO ELEMENTS

    if (key == EVENT_DEL)
      return true;
    
    data_as_text_t dat = DAT_get_newvalue(subsubtrack, key, 1, LOGTYPE_HOLD, 1, 99, 1, 99, false, true, false);

    if (dat.is_valid==false)
      return false;

    ADD_UNDO(Swings_CurrPos(window));
    AddSwing(block, track, *place, dat.value, dat.logtype);

  } else {

    // ONE ELEMENT (or more than one element)

    struct Swing *swing = swings.last();
  
    if (key == EVENT_DEL) {

      ADD_UNDO(Swings_CurrPos(window));  
      RemoveSwing(block, track, swing);
 
    } else {

      data_as_text_t dat = DAT_get_overwrite(swing->weight, swing->logtype, subsubtrack, key, 1, 99, 1, 99, false, false);

      if (dat.is_valid==false)
        return false;

      if (dat.value==swing->weight)
        return true; // I.e. we eat the key.
          
      ADD_UNDO(Swings_CurrPos(window));  

      AddSwing(block, track, swing->l.p, dat.value, dat.logtype);
    }
  }

  return true;
}
  
