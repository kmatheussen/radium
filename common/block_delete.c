

#include "nsmtracker.h"
#include "list_proc.h"
#include "wblocks_proc.h"
#include "OS_Bs_edit_proc.h"
#include "undo_block_insertdelete_proc.h"
#include "player_proc.h"
#include "player_pause_proc.h"
#include "sequencer_proc.h"

#include "block_delete_proc.h"

extern struct Root *root;

void DeleteBlock(
	NInt blockpos
){
	struct Tracker_Windows *window=root->song->tracker_windows;
	struct WBlocks *wblock;
	struct Blocks *removed_block=ListFindElement1(&root->song->blocks->l,blockpos);
	struct Blocks *nextblock=NextBlock(removed_block);

	ListRemoveElement1(&root->song->blocks,&removed_block->l);

        {
          struct Blocks *block = nextblock;
          while(block!=NULL){
            block->l.num--;
            block=NextBlock(block);
          }
        }

	root->song->num_blocks--;
        g_editor_blocks_generation++;

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

        SEQUENCER_remove_block_from_seqtracks(removed_block);
}


void DeleteBlock_CurrPos(
                         struct Tracker_Windows *window,
                         int blockpos
){
	struct WBlocks *wblock=window->wblock;
    
	if(wblock->l.next==NULL && wblock==window->wblocks) return;

        blockpos=blockpos==-1 ? window->wblock->l.num : blockpos;

        ADD_UNDO(Block_Delete(blockpos));

        PC_Pause();{

          DeleteBlock(blockpos);
          
          wblock=ListFindElement1_r0(&window->wblocks->l,blockpos);
          
          if(wblock==NULL){
            wblock=ListLast1(&window->wblocks->l);
          }

          SelectWBlock(window,wblock,true);
          
          BS_UpdateBlockList();
          BS_UpdatePlayList();
          
        }PC_StopPause(NULL);
}


