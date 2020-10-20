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
#include "tempos_proc.h"
#include "list_proc.h"
#include "time_proc.h"
#include "undo_sequencer_proc.h"

#include "undo_tempos_proc.h"



static void *Undo_Do_Tempos(
	struct Tracker_Windows *window,
	struct WBlocks *wblock,
	struct WTracks *wtrack,
	int realline,
	void *pointer
);


static void ADD_UNDO_FUNC(Tempos(
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
             CB_CopyTempos(block->tempos),
             Undo_Do_Tempos,
             "Block BPMs"
             );
  }UNDO_CLOSE();
}

void ADD_UNDO_FUNC(Tempos_CurrPos(
                                  struct Tracker_Windows *window
                                  )
                   )
{
  CALL_ADD_UNDO_FUNC(Tempos(window,window->wblock->block,window->curr_track,window->wblock->curr_realline));
}

static void *Undo_Do_Tempos(
	struct Tracker_Windows *window,
	struct WBlocks *wblock,
	struct WTracks *wtrack,
	int realline,
	void *pointer
){
	struct Tempos *undo_tempos=(struct Tempos *)pointer;
	struct Tempos *temp=wblock->block->tempos;

	wblock->block->tempos=undo_tempos;

        TIME_block_tempos_have_changed(wblock->block);

	return temp;
}




