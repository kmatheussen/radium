/* Copyright 2001 Kjetil S. Matheussen

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
#include "clipboard_block_copy_proc.h"
#include "clipboard_block_paste_proc.h"

#include "undo_block_mergesplit_proc.h"

extern struct Root *root;

struct Undo_Block_MergeSplit{
	struct WBlocks *wblock;
	struct WBlocks *nwblock;
	NInt blockpos;
};



void *Undo_Do_Block_Split(
	struct Tracker_Windows *window,
	struct WBlocks *wblock,
	struct WTracks *wtrack,
	int realline,
	void *pointer
);


void Undo_Block_Split_CurrPos(void){
	struct Tracker_Windows *window=root->song->tracker_windows;
	struct WBlocks *wblock=window->wblock;

	struct Undo_Block_MergeSplit *ubm=talloc(sizeof(struct Undo_Block_MergeSplit));

	ubm->wblock=CB_CopyBlock(wblock);
	ubm->blockpos=wblock->l.num;

	Undo_Add(
		window->l.num,
		wblock->l.num,
		window->curr_track,
		wblock->curr_realline,
		ubm,
		Undo_Do_Block_Split,
                "Block split"
	);
}



void *Undo_Do_Block_Split(
	struct Tracker_Windows *window,
	struct WBlocks *wblock,
	struct WTracks *wtrack,
	int realline,
	void *pointer
){
	struct Undo_Block_MergeSplit *ubm=(struct Undo_Block_MergeSplit *)pointer;
	struct WBlocks *wblocktemp=NULL;

	if(ubm->nwblock==NULL){

//Undo
		ubm->nwblock=CB_CopyBlock(wblock);
//		wblocktemp=CB_CopyBlock((struct WBlocks*)ListPrevElement1(&window->wblocks->l,&wblock->l));
		wblocktemp=CB_CopyBlock(NextWBlock(wblock));
		CB_PasteBlock(window,ubm->wblock,wblock);
		DeleteBlock(ubm->blockpos+1);
		ubm->wblock=wblocktemp;
	}else{


//Redo

		InsertBlock(ubm->blockpos,20,20,"n");
		wblocktemp=CB_CopyBlock(wblock);
		CB_PasteBlock(window,ubm->wblock,wblock);
		CB_PasteBlock(window,ubm->nwblock,(struct WBlocks*)ListPrevElement1(&window->wblocks->l,&wblock->l));
		ubm->wblock=wblocktemp;
		ubm->nwblock=NULL;
	}


	BS_UpdateBlockList();
	BS_UpdatePlayList();


	return ubm;
}

