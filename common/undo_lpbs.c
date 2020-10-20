/* Copyright 2000 Kjetil S. Matheussen

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
#include "undo.h"
#include "clipboard_tempos_copy_proc.h"
#include "placement_proc.h"
#include "LPB_proc.h"
#include "list_proc.h"
#include "time_proc.h"
#include "player_pause_proc.h"
#include "undo_sequencer_proc.h"

#include "undo_lpbs_proc.h"



static void *Undo_Do_LPBs(
                          struct Tracker_Windows *window,
                          struct WBlocks *wblock,
                          struct WTracks *wtrack,
                          int realline,
                          void *pointer
                          );


static void ADD_UNDO_FUNC(LPBs(
                        struct Tracker_Windows *window,
                        struct Blocks *block,
                        NInt tracknum,
                        int realline
                        )
                   )
{
  UNDO_OPEN_REC();{
    CALL_ADD_UNDO_FUNC(Seqblock_block_timing_state(block));
    Undo_Add(
             window->l.num,
             block->l.num,
             tracknum,
             realline,
             CB_CopyLPBs(block->lpbs),
             Undo_Do_LPBs,
             "LPBs"
             );
  }UNDO_CLOSE();
}

void ADD_UNDO_FUNC(LPBs_CurrPos(
                                struct Tracker_Windows *window
                                )
                   )
{
  CALL_ADD_UNDO_FUNC(LPBs(window,window->wblock->block,window->curr_track,window->wblock->curr_realline));
}

static void *Undo_Do_LPBs(
	struct Tracker_Windows *window,
	struct WBlocks *wblock,
	struct WTracks *wtrack,
	int realline,
	void *pointer
){
	struct LPBs *undo_lpbs=(struct LPBs *)pointer;
	struct LPBs *temp=wblock->block->lpbs;

        PC_Pause();{
          
          wblock->block->lpbs=undo_lpbs;

          TIME_block_LPBs_have_changed(wblock->block);

        }PC_StopPause(NULL);
        
	return temp;
}




