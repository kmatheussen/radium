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
#include "undo_tempos_proc.h"
#include "tempos_proc.h"
#include "data_as_text_proc.h"
#include "player_pause_proc.h"
#include "time_proc.h"

#include "bpmtext_proc.h"


int BPMTEXT_subsubtrack(struct Tracker_Windows *window){
  if (window->curr_track != TEMPOTRACK)
    return -1;
  
  int curr_track_sub = window->curr_othertrack_sub;

  if (curr_track_sub < 0)
    return -1;

  if (curr_track_sub > 3)
    return -1;

  return curr_track_sub;
}

bool BPMTEXT_keypress(struct Tracker_Windows *window, struct WBlocks *wblock, int realline, Place *place, int key){
  int subsubtrack = BPMTEXT_subsubtrack(window);

  if (subsubtrack==-1)
    return false;

  QVector<Tempos*> bpms = BPMs_get(wblock, realline);

    

  if (false && bpms.size()  > 1) {

    // MORE THAN ONE ELEMENT (treat last element instead)
    
    if (key == EVENT_DEL){

      PC_Pause();{
        ListRemoveElement3(&wblock->block->tempos, &(bpms.last()->l));
        UpdateSTimes(wblock->block);          
      }PC_StopPause(NULL);

      //const struct LocalZooms lz2 = realline==wblock->num_reallines-1 ? p_Create(wblock->block->num_lines, 0, 1) : *wblock->reallines[realline+1];
      //RemoveTempos(wblock->block, place, &lz2->l.p);
                   
    }else
      return false;
    
    
  } else if (bpms.size() == 0) {

    // NO ELEMENTS

    if (key == EVENT_DEL)
      return false;
    
    data_as_text_t dat = DAT_get_newvalue(subsubtrack, key, root->tempo, 1, 999, 1, 999, false, true, false);

    if (dat.is_valid==false)
      return false;

    ADD_UNDO(Tempos_CurrPos(window));
    SetTempo(wblock->block, place, dat.value, dat.logtype);

  } else {

    // ONE ELEMENT (or more than one element)

    struct Tempos *bpm = bpms.last();
  
    if (key == EVENT_DEL) {

      PC_Pause();{
        ADD_UNDO(Tempos_CurrPos(window));
        ListRemoveElement3(&wblock->block->tempos, &bpm->l);

        UpdateSTimes(wblock->block);          
      }PC_StopPause(NULL);

    } else {

      data_as_text_t dat = DAT_get_overwrite(bpm->tempo, bpm->logtype, subsubtrack, key, 1, 999, 1, 999, false, false);

      if (dat.is_valid==false)
        return false;
      printf("   LOg bef: %d, aft: %d, ", bpm->logtype, dat.logtype);
      SetTempo(wblock->block, &bpm->l.p, dat.value, dat.logtype);
      printf("   aft2: %d\n", bpm->logtype);

    }    
  }

  return true;
}
  
