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
#include "block_insert_proc.h"
#include "block_delete_proc.h"
#include "OS_Bs_edit_proc.h"
#include "list_proc.h"
#include "seqtrack_proc.h"

#include "undo_block_insertdelete_proc.h"

extern struct Root *root;

struct Undo_Block_InsertDelete{
  NInt blockpos;
  struct WBlocks *wblock;
  hash_t *sequencer_state;
};



void *Undo_Do_Block_Delete(
	struct Tracker_Windows *window,
	struct WBlocks *wblock,
	struct WTracks *wtrack,
	int realline,
	void *pointer
);

void *Undo_Do_Block_Insert(
	struct Tracker_Windows *window,
	struct WBlocks *wblock,
	struct WTracks *wtrack,
	int realline,
	void *pointer
);


void ADD_UNDO_FUNC(Block_Insert(NInt blockpos)){
	struct Tracker_Windows *window=root->song->tracker_windows;
	struct WBlocks *wblock=window->wblock;

	struct Undo_Block_InsertDelete *ubi=talloc(sizeof(struct Undo_Block_InsertDelete));
	ubi->blockpos=blockpos;
	ubi->wblock=NULL;
        ubi->sequencer_state=SEQUENCER_get_state();

	Undo_Add(
		window->l.num,
		wblock->l.num,
		window->curr_track,
		wblock->curr_realline,
		ubi,
		Undo_Do_Block_Insert,
                "Block insert"
	);
}


void ADD_UNDO_FUNC(Block_Delete(NInt blockpos)){
	struct Tracker_Windows *window=root->song->tracker_windows;
	struct WBlocks *wblock=window->wblock;

	struct Undo_Block_InsertDelete *ubi=talloc(sizeof(struct Undo_Block_InsertDelete));
	ubi->blockpos=blockpos;
	ubi->wblock=(struct WBlocks *)ListFindElement1(&window->wblocks->l,blockpos);
        ubi->sequencer_state=SEQUENCER_get_state();

	Undo_Add(
		window->l.num,
		wblock->l.num,
		window->curr_track,
		wblock->curr_realline,
		ubi,
		Undo_Do_Block_Delete,
                "Block delete"
	);
}


void *Undo_Do_Block_Insert(
	struct Tracker_Windows *window,
	struct WBlocks *wblock,
	struct WTracks *wtrack,
	int realline,
	void *pointer
){
	struct Undo_Block_InsertDelete *ubi=(struct Undo_Block_InsertDelete *)pointer;

	if(ubi->wblock!=NULL){
		return Undo_Do_Block_Delete(window,wblock,wtrack,realline,pointer);
	}

        SEQUENCER_create_from_state(ubi->sequencer_state);

	ubi->wblock=(struct WBlocks *)ListFindElement1(&window->wblocks->l,ubi->blockpos);
	DeleteBlock(ubi->blockpos);

	BS_UpdatePlayList();
        BS_UpdateBlockList();

	return ubi;
}

void *Undo_Do_Block_Delete(
	struct Tracker_Windows *window,
	struct WBlocks *wblock,
	struct WTracks *wtrack,
	int realline,
	void *pointer
){
	struct Undo_Block_InsertDelete *ubi=(struct Undo_Block_InsertDelete *)pointer;

	if(ubi->wblock==NULL){
		return Undo_Do_Block_Insert(window,wblock,wtrack,realline,pointer);
	}

	InsertBlock_IncBlockNums(ubi->blockpos);
	ListAddElement1(&root->song->blocks,&ubi->wblock->block->l);
	ListAddElement1(&window->wblocks,&ubi->wblock->l);

        SEQUENCER_create_from_state(ubi->sequencer_state);
        
	BS_UpdateBlockList();
	BS_UpdatePlayList();

	ubi->wblock=NULL;
	return ubi;
}



