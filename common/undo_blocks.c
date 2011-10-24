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
#include "clipboard_block_copy_proc.h"
#include "clipboard_block_paste_proc.h"

#include "undo_blocks_proc.h"


void *Undo_Do_Block(
	struct Tracker_Windows *window,
	struct WBlocks *wblock,
	struct WTracks *wtrack,
	int realline,
	void *pointer
);


void Undo_Block(
	struct Tracker_Windows *window,
	struct WBlocks *wblock,
	struct WTracks *wtrack,
	int realline
){
	Undo_New(
		window->l.num,
		wblock->l.num,
		wtrack->l.num,
		realline,
		CB_CopyBlock(wblock),
		Undo_Do_Block
	);
}

void Undo_Block_CurrPos(
	struct Tracker_Windows *window
){
	Undo_Block(window,window->wblock,window->wblock->wtrack,window->wblock->curr_realline);
}

void *Undo_Do_Block(
	struct Tracker_Windows *window,
	struct WBlocks *wblock,
	struct WTracks *wtrack,
	int realline,
	void *pointer
){
	struct WBlocks *undo_wblock=(struct WBlocks *)pointer;
	struct WBlocks *temp=CB_CopyBlock(wblock);

	CB_PasteBlock(window,undo_wblock,wblock);

	return temp;
}



