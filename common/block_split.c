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
#include "block_insert_proc.h"
#include "clipboard_block_paste_proc.h"
#include "list_proc.h"
#include "placement_proc.h"
#include "wblocks_proc.h"
#include "OS_Bs_edit_proc.h"
#include "lines_proc.h"
#include "player_pause_proc.h"
#include "undo_block_mergesplit_proc.h"

#include "block_split_proc.h"


extern struct Root *root;


static void BLOCK_Split(
	struct Blocks *block,
	int splitline
){
        R_ASSERT(is_playing()==false);
          
	struct Tracker_Windows *window=root->song->tracker_windows;

	struct WBlocks *wblock,*nwblock;
	NInt blocknum=block->l.num;

	int cutreallines=block->num_lines-splitline;

	InsertBlock(
		blocknum,
		block->num_tracks,
		block->num_lines,
		block->name
	);

	nwblock=ListFindElement1(&window->wblocks->l,blocknum+1);
	wblock=ListFindElement1(&window->wblocks->l,blocknum);

	CB_PasteBlock(window,nwblock,wblock);

	InsertLines(wblock->block,p_Create(splitline,0,1),make_ratio(-cutreallines,1));
	InsertLines(nwblock->block,p_Create(0,0,1),make_ratio(-splitline,1));

}


void BLOCK_Split_CurrPos(
	struct Tracker_Windows *window
){
	struct WBlocks *wblock=window->wblock;

        int splitline=wblock->reallines[wblock->curr_realline]->l.p.line;
          
        if(splitline<3) return;
        if(splitline>wblock->block->num_lines-3) return;

        PC_Pause();{
          
          ADD_UNDO(Block_Split_CurrPos());
          
          BLOCK_Split(wblock->block,splitline);
          
          SelectWBlock(window,window->wblock);
          BS_UpdateBlockList();
          BS_UpdatePlayList();
          
        }PC_StopPause(NULL);
}



