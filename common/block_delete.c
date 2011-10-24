

#include "nsmtracker.h"
#include "list_proc.h"
#include "wblocks_proc.h"
#include "OS_Bs_edit_proc.h"
#include "undo_block_insertdelete_proc.h"
#include "player_proc.h"

#include "block_delete_proc.h"

extern struct Root *root;

void DeleteBlock(
	NInt blockpos
){
	struct Tracker_Windows *window=root->song->tracker_windows;
	struct WBlocks *wblock;
	struct Blocks *block=ListFindElement1(&root->song->blocks->l,blockpos);

	ListRemoveElement1(&root->song->blocks,&block->l);
	block=NextBlock(block);			//This is safe. Allso if normal unix free() is used in ListRemoveElement1. But, if GC is not used, take care.

	while(block!=NULL){
		block->l.num--;
		block=NextBlock(block);
	}

	root->song->num_blocks--;

	while(window!=NULL){
		wblock=ListFindElement1(&window->wblocks->l,blockpos);
		ListRemoveElement1(
			&window->wblocks,
			&wblock->l
		);
		wblock=NextWBlock(wblock);
		while(wblock!=NULL){
			wblock->l.num--;
			wblock=NextWBlock(wblock);
		}
		window=NextWindow(window);
	}
}


void DeleteBlock_CurrPos(
	struct Tracker_Windows *window
){
	struct WBlocks *wblock=window->wblock;
	NInt blockpos;

	PlayStop();

	if(wblock->l.next==NULL && wblock==window->wblocks) return;

	blockpos=window->wblock->l.num;

	Undo_Block_Delete(blockpos);

	DeleteBlock(blockpos);

	wblock=ListFindElement1_r0(&window->wblocks->l,blockpos);

	if(wblock==NULL){
		wblock=ListLast1(&window->wblocks->l);
	}

	SelectWBlock(window,wblock);

	BS_UpdateBlockList();
	BS_UpdatePlayList();

}


