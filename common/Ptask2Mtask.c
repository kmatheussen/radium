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


/************* overview ********************************

	The player-task sometimes needs to let the main-task
	do some of its work. The Ptask2Mtask function is
	called from the player-task, and
	recieves a pointer to a function that must be called
	from the main-task with 'pointer' as argument.

	The player-task could do all this work by itself, but
	that would slow down and allso make it pretty complicated
	because semaphores or things like that has to be
	added.

	Used when scrolling, change of block, etc.

	NOTE. The main-task doesn't have to be finished processing
	the function before Ptask2Mtask is called again.

*******************************************************/


#include "nsmtracker.h"
#include "playerclass.h"
#include "wblocks_proc.h"
#include "cursor_updown_proc.h"
#include "list_proc.h"
#include "common_proc.h"
#include "blocklist_proc.h"
#include "OS_Bs_edit_proc.h"
#include "blts_proc.h"

extern PlayerClass *pc;
extern struct Root *root;


void P2MUpdateSongPosCallBack(void){
	struct Tracker_Windows *window=root->song->tracker_windows;
	bool setfirstpos=root->setfirstpos;
	NInt curr_block=root->curr_block;
	struct WBlocks *wblock;
	int till_curr_realline;

	if(pc->playtype==PLAYSONG) BS_SelectPlaylistPos(root->curr_playlist);

	while(window!=NULL){
		if(window->playalong==true){

		  Blt_markVisible(window);

			wblock=ListFindElement1(&window->wblocks->l,curr_block);
			till_curr_realline=wblock->till_curr_realline;

			if(window->curr_block!=curr_block){
				debug("change block\n");
				if(setfirstpos){
					wblock->curr_realline=0;
					SetWBlock_Top_And_Bot_Realline(window,wblock);
				}
				SelectWBlock(
					window,
					wblock
				);
			}

			if(setfirstpos){			// The player-routine (PEQblock.c) change this one.
				till_curr_realline=wblock->till_curr_realline=0;
			}

//			debug("currline: %d, tilline: %d\n",window->wblock->curr_realline,root->curr_realline);
			ScrollEditorToRealLine(window,wblock,till_curr_realline);

		  Blt_clearNotUsedVisible(window);
		  Blt_blt(window);

		}
		window=NextWindow(window);
	}

	root->setfirstpos=false;

}

void (*Ptask2MtaskCallBack)(void)= &P2MUpdateSongPosCallBack;


