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
#include "Signature_proc.h"
#include "list_proc.h"
#include "time_proc.h"

#include "undo_signatures_proc.h"



void *Undo_Do_Signatures(
	struct Tracker_Windows *window,
	struct WBlocks *wblock,
	struct WTracks *wtrack,
	int realline,
	void *pointer
);


void Undo_Signatures(
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
                 CB_CopySignatures(block->signatures),
                 Undo_Do_Signatures
	);
}

void Undo_Signatures_CurrPos(
	struct Tracker_Windows *window
){
	Undo_Signatures(window,window->wblock->block,window->curr_track,window->wblock->curr_realline);
}

void *Undo_Do_Signatures(
	struct Tracker_Windows *window,
	struct WBlocks *wblock,
	struct WTracks *wtrack,
	int realline,
	void *pointer
){
	struct Signatures *undo_signatures=(struct Signatures *)pointer;
	struct Signatures *temp=wblock->block->signatures;

	wblock->block->signatures=undo_signatures;

	//UpdateWSignatures(window,wblock);

	UpdateSTimes(wblock->block);

	return temp;
}




