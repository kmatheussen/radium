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
#include "Beats_proc.h"

#include "undo_lpbs_proc.h"



void *Undo_Do_LPBs(
	struct Tracker_Windows *window,
	struct WBlocks *wblock,
	struct WTracks *wtrack,
	int realline,
	void *pointer
);


void Undo_LPBs(
	struct Tracker_Windows *window,
	struct Blocks *block,
	NInt tracknum,
	int realline
){
	Undo_Add(
                 window->l.num,
                 block->l.num,
                 tracknum,
                 realline,
                 CB_CopyLPBs(block->lpbs),
                 Undo_Do_LPBs
	);
}

void Undo_LPBs_CurrPos(
	struct Tracker_Windows *window
){
	Undo_LPBs(window,window->wblock->block,window->curr_track,window->wblock->curr_realline);
}

void *Undo_Do_LPBs(
	struct Tracker_Windows *window,
	struct WBlocks *wblock,
	struct WTracks *wtrack,
	int realline,
	void *pointer
){
	struct LPBs *undo_lpbs=(struct LPBs *)pointer;
	struct LPBs *temp=wblock->block->lpbs;

	wblock->block->lpbs=undo_lpbs;

	//UpdateWLPBs(window,wblock);

	UpdateSTimes(wblock->block);
        UpdateBeats(wblock->block);

	return temp;
}




