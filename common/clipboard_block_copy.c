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
#include "clipboard_track_copy_proc.h"
#include "clipboard_localzooms_proc.h"
#include "clipboard_tempos_copy_proc.h"
#include "list_proc.h"
#include "placement_proc.h"
#include <string.h>

#include "clipboard_block_copy_proc.h"


struct WBlocks *cb_wblock=NULL;



/* OBOY! This one use quite a bit of memory. */

struct WBlocks *CB_CopyBlock(
	struct WBlocks *wblock
){
	int lokke;
	struct WBlocks *towblock;
	struct WTracks *wtrack;
	struct Blocks *toblock;
	struct Blocks *block=wblock->block;

	towblock=talloc(sizeof(struct WBlocks));
	memcpy(towblock,wblock,sizeof(struct WBlocks));

	towblock->block=toblock=talloc(sizeof(struct Blocks));
	memcpy(toblock,block,sizeof(struct Blocks));

	towblock->l.next=NULL;

	towblock->localzooms=CB_CopyLocalZooms(wblock);
	towblock->reallines=NULL;
//	towblock->title=talloc(strlen(wblock->title)+1);
//	memcpy(towblock->title,wblock->title,strlen(wblock->title)+1);

	towblock->wtempos=NULL;
	towblock->wtemponodes=NULL;
	towblock->wlpbs=NULL;

	toblock->l.next=NULL;

	toblock->name=talloc_atomic(strlen(block->name)+1);
	memcpy(toblock->name,block->name,strlen(block->name)+1);

	toblock->times=NULL;

	towblock->wtracks=NULL;
	wtrack=wblock->wtracks;
	for(lokke=0;lokke<block->num_tracks;lokke++){
		ListAddElement1(
			&towblock->wtracks,
			(struct ListHeader1 *) CB_CopyTrack(towblock,wtrack)
		);
		wtrack=NextWTrack(wtrack);
	}

	toblock->lpbs=CB_CopyLPBs(block->lpbs);
	toblock->tempos=CB_CopyTempos(block->tempos);
	toblock->temponodes=CB_CopyTempoNodes(block->temponodes);

	return towblock;

}


void CB_CopyBlock_CurrPos(
	struct Tracker_Windows *window
){
	cb_wblock=CB_CopyBlock(window->wblock);
}















