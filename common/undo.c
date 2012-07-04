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
#include "list_proc.h"
#include "windows_proc.h"
#include "visual_proc.h"
#include "wblocks_proc.h"
#include "player_pause_proc.h"
#include "player_proc.h"

#define RADIUM_UNDOISCALLINGNOW
#include "undo.h"
#undef RADIUM_UNDOISCALLINGNOW


struct Undo UndoRoot={0};
struct Undo *CurrUndo=&UndoRoot;
extern struct Root *root;
int num_undos=0;
int max_num_undos=MAX_NUM_UNDOS;


void ResetUndo(void){
	CurrUndo=&UndoRoot;
	num_undos=0;
}

/***************************************************
  FUNCTION
    Insert a new undo-element.
***************************************************/
void Undo_New(
	NInt windownum,
	NInt blocknum,
	NInt tracknum,
	int realline,
	void *pointer,
	void *(*UndoFunction)(
		struct Tracker_Windows *window,
		struct WBlocks *wblock,
		struct WTracks *wtrack,
		int realline,
		void *pointer
	)
){
	struct Undo *undo;

	undo=talloc(sizeof(struct Undo));

	undo->prev=CurrUndo;
	CurrUndo->next=undo;
	CurrUndo=undo;

	undo->windownum=windownum;
	undo->blocknum=blocknum;
	undo->tracknum=tracknum;
	undo->realline=realline;

	undo->pointer=pointer;
	undo->UndoFunction=UndoFunction;

	num_undos++;

	if(num_undos!=0 && num_undos>max_num_undos){
		num_undos--;
		UndoRoot.next=UndoRoot.next->next;
		UndoRoot.next->prev=&UndoRoot;
	}

}

void Undo(void){
	struct Undo *undo=CurrUndo;

	struct Tracker_Windows *window;
	struct WBlocks *wblock;
	struct WTracks *wtrack=NULL;
	NInt blocknum;

	if(undo==&UndoRoot) return;

	PlayStop();

	blocknum=undo->blocknum;

	window=ListFindElement1(&root->song->tracker_windows->l,undo->windownum);
	wblock=ListFindElement1_r0(&window->wblocks->l,blocknum);

	if(wblock!=NULL){
		window->wblock=wblock;
		if(undo->tracknum<0){
			wtrack=wblock->wtracks;
		}else{
			wtrack=ListFindElement1_r0(&wblock->wtracks->l,undo->tracknum);
		}
		if(wtrack!=NULL){
			wblock->wtrack=wtrack;
		}
		wblock->curr_realline=undo->realline;
		window->curr_track=undo->tracknum;
	}

	undo->pointer=(*undo->UndoFunction)(window,wblock,wtrack,undo->realline,undo->pointer);

	CurrUndo=undo->prev;

	num_undos--;

        wblock=ListFindElement1_r0(&window->wblocks->l,blocknum);
        if(wblock==NULL)
          wblock=ListFindElement1_r0(&window->wblocks->l,blocknum-1);
        if(wblock==NULL){
          RError("undo.c: block %d does not exist. Using block 0.",blocknum-1);
          wblock=window->wblocks;
        }

	window->wblock=wblock;
	if(undo->tracknum<0){
		wtrack=wblock->wtracks;
	}else{
		wtrack=ListFindElement1_r0(&wblock->wtracks->l,undo->tracknum);
	}
	wblock->wtrack=wtrack;
	wblock->curr_realline=undo->realline;
	window->curr_track=undo->tracknum;

	SelectWBlock(
		window,
		wblock
	);

}


void Redo(void){

	if(CurrUndo->next==NULL) return;

	CurrUndo=CurrUndo->next;
	Undo();
	CurrUndo=CurrUndo->next;

	num_undos+=2;

}

void SetMaxUndos(struct Tracker_Windows *window){
	int newmax=0;
	char seltext[50];

	sprintf(seltext,"Max Undos (0=unlimited) (now: %d): ",max_num_undos);
	while(newmax==1 || newmax==2)
		newmax=GFX_GetInteger(window,NULL,seltext,0,2000000);
	if(newmax==-1) return;

	max_num_undos=newmax;
}





