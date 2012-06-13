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
#include "localzooms_proc.h"
#include "reallines_proc.h"
#include "wtracks_proc.h"
#include "common_proc.h"
#include "tempos_proc.h"
#include "LPB_proc.h"
#include "temponodes_proc.h"
#include "blocks_proc.h"
#include "windows_proc.h"
#include "OS_Bs_edit_proc.h"
#include "fxlines_proc.h"
#include "trackreallines_proc.h"
#include "cursor_proc.h"
#include "undo_block_insertdelete_proc.h"
#include "playerclass.h"
#include "blocklist_proc.h"
#include "player_proc.h"
#include "visual_proc.h"

#include "wblocks_proc.h"



extern struct Root *root;
extern PlayerClass *pc;


void CloseWBlock(struct Tracker_Windows *window, NInt blocknum){
	struct WBlocks *temp;

	temp=(struct WBlocks *)ListFindElement1(&window->wblocks->l,blocknum);

	if(temp==NULL) return;

	ListRemoveElement1(&window->wblocks,&temp->l);
}



bool WBlock_legalizeStartEndReallines(struct WBlocks *wblock,int *start_realline,int *end_realline){

  if(*start_realline<wblock->top_realline) *start_realline=wblock->top_realline;
  if(*start_realline<0) *start_realline=0;
  if(*end_realline>=wblock->num_reallines) *end_realline=wblock->num_reallines-1;
  if(*end_realline>wblock->bot_realline) *end_realline=wblock->bot_realline;

  if(*end_realline<*start_realline){
    return false;
  }

  return true;
}


/*********************************************************************
   FUNCTION
     Sentraliced place to set these two important variables. A lot
     of troubles have been discovered because of these two, and
     it was a lot of troubles setting then correct. By having just
     one place where they are set, it will at least be easier to
     once and for all find a proper way to set them. Seems like it
     works now, though.
*********************************************************************/
void SetWBlock_Top_And_Bot_Realline(
	struct Tracker_Windows *window,
	struct WBlocks *wblock
){
	wblock->bot_realline=
		wblock->curr_realline+(1+wblock->num_visiblelines)/2 - 1;

	/*
	wblock->top_realline=
		wblock->curr_realline+
		(wblock->t.y1 - GetCursorY1Pos(window,wblock))/window->fontheight;
	*/
	wblock->top_realline=wblock->bot_realline-wblock->num_visiblelines + 1;
}


void UpdateWBlockCoordinates(
	struct Tracker_Windows *window,
	struct WBlocks *wblock
){
	wblock->a.x1  = max(window->fontwidth+3,window->leftslider.width+1);
	wblock->a.y1  = 0;
	wblock->a.x2 = window->width;
	wblock->a.y2 = window->height - window->bottomslider.width -1;

	wblock->linearea.y  = wblock->a.y1+(window->org_fontheight*2);
	wblock->linearea.y2 = wblock->a.y2;

	wblock->zoomlevelarea.x  = wblock->a.x1;
	wblock->zoomlevelarea.x2 = wblock->a.x1             + wblock->zoomlevelarea.width;
	wblock->linenumarea.x    = wblock->zoomlevelarea.x2 + 3;
	wblock->linenumarea.x2   = wblock->linenumarea.x    + wblock->linenumarea.width;
	wblock->tempocolorarea.x = wblock->linenumarea.x2   + 3;
	wblock->tempocolorarea.x2= wblock->tempocolorarea.x + wblock->tempocolorarea.width;
	wblock->lpbTypearea.x    = wblock->tempocolorarea.x2+ 3;
	wblock->lpbTypearea.x2   = wblock->lpbTypearea.x    + wblock->lpbTypearea.width;
	wblock->lpbarea.x        = wblock->lpbTypearea.x2   + 3;
	wblock->lpbarea.x2       = wblock->lpbarea.x        + wblock->lpbarea.width;
	wblock->tempoTypearea.x  = wblock->lpbarea.x2       + 3;
	wblock->tempoTypearea.x2 = wblock->tempoTypearea.x  + wblock->tempoTypearea.width;
	wblock->tempoarea.x      = wblock->tempoTypearea.x2 + 3;
	wblock->tempoarea.x2     = wblock->tempoarea.x      + wblock->tempoarea.width;
	wblock->temponodearea.x  = wblock->tempoarea.x2     + 2;
	wblock->temponodearea.x2 = wblock->temponodearea.x  + wblock->temponodearea.width;

	wblock->reltempo.x1=0;
	wblock->reltempo.y1=wblock->a.y2+1;
	wblock->reltempo.x2=wblock->temponodearea.x2-3;
	wblock->reltempo.y2=window->height-1;

	wblock->t.x1 = wblock->temponodearea.x2+2;
	wblock->t.x2 = wblock->a.x2;
	wblock->t.y1 = wblock->linearea.y + 2;
	wblock->t.y2 = wblock->a.y2;

	wblock->num_visiblelines=(wblock->t.y2-wblock->t.y1)/window->fontheight;
	if((wblock->num_visiblelines-2)*window->fontheight+wblock->t.y1>=wblock->t.y2){
		wblock->num_visiblelines--;
	}

	SetWBlock_Top_And_Bot_Realline(window,wblock);

	UpdateAllWTracksCoordinates(window,wblock);
}

void UpdateAllWBlockCoordinates(
	struct Tracker_Windows *window
){
	struct WBlocks *wblock=window->wblocks;

	while(wblock!=NULL){
		UpdateWBlockCoordinates(window,wblock);
		wblock=NextWBlock(wblock);
	}
}

void UpdateWBlockWidths(struct Tracker_Windows *window,struct WBlocks *wblock){
	wblock->linenumarea.width   = window->fontwidth*3;
	wblock->tempocolorarea.width= window->fontwidth*3;
	wblock->lpbTypearea.width   = window->fontwidth;
	wblock->lpbarea.width       = window->fontwidth*2;
	wblock->tempoTypearea.width = window->fontwidth;
	wblock->tempoarea.width     = window->fontwidth*3;
	wblock->temponodearea.width = window->fontwidth*7/2;
}

void UpdateAllWBlockWidths(struct Tracker_Windows *window){
	struct WBlocks *wblock;
	wblock=window->wblocks;
	while(wblock!=NULL){
		UpdateWBlockWidths(window,wblock);
		wblock=NextWBlock(wblock);
	}
}

void NewWBlock(
	struct Tracker_Windows *window,
	struct WBlocks *wblock,
	struct Blocks *block
){

	wblock->block=block;
	wblock->l.num=block->l.num;

	wblock->title=talloc_atomic(128);

	wblock->num_reallines_last=wblock->num_reallines=wblock->block->num_lines;

	wblock->tempotrackonoff=1;

	wblock->zoomlevelarea.width = 0;
	UpdateWBlockWidths(window,wblock);

	wblock->left_subtrack= -1;

	wblock->reltempomax=2.0;

	UpdateWBlockCoordinates(window,wblock);
	UpdateWTracks(window,wblock);
	wblock->wtrack=wblock->wtracks;
	UpdateAllWTracksCoordinates(window,wblock);

	NewLocalZooms(window,wblock);

	UpdateRealLines(window,wblock);

	UpdateWTempos(window,wblock);
	UpdateWLPBs(window,wblock);
	UpdateWTempoNodes(window,wblock);

	wblock->isgfxdatahere=true;

	ListAddElement1(&window->wblocks,&wblock->l);
}


/*
  FUNCTION
     Make the WBlocks list the same as the Blocks list
     for window 'window'. Can be called after new window
     has been made, or _one_ block has been added.
*/

void UpdateWBlocks(struct Tracker_Windows *window){
	struct WBlocks *wblock=window->wblocks;
	struct Blocks *block=root->song->blocks;

	struct WBlocks *new;

	while(block!=NULL){
		if(wblock==NULL){
			wblock=talloc(sizeof(struct WBlocks));
			NewWBlock(window,wblock,block);
		}

		if(block->l.num!=wblock->l.num){
			new=talloc(sizeof(struct WBlocks));
			NewWBlock(window,new,(struct Blocks *)block);
			return;
		}

		block=NextBlock(block);
		wblock=NextWBlock(wblock);
	}
}


void SelectWBlock(struct Tracker_Windows *window,struct WBlocks *wblock){
	NInt newcurrtrack=window->curr_track;
	int newcurrtracksub=window->curr_track_sub;

	if(wblock==NULL) return;


	//#undef GFX_FilledBox
/*
	This command was removed for v0.60g. If this is readed many versions later,
	and there are no problem with gfx-garbage in this area, please remove all this.

	GFX_FilledBox(
		      window,0,
		      wblock->lpbTypearea.x+1,
		      wblock->a.y1,

		      window->wblock->temponodearea.x2+3,
		      window->wblock->t.y1-2
		      );
*/

	window->wblock=wblock;
	if( ! wblock->isgfxdatahere){
		UpdateWTempoNodes(window,wblock);
		UpdateAllFXNodeLines(window,wblock);
		UpdateAllTrackReallines(window,wblock);
		wblock->isgfxdatahere=true;
	}

	if(window->curr_track>=wblock->block->num_tracks){
		newcurrtrack=wblock->block->num_tracks-1;
	}
	if(wblock->curr_realline>=wblock->num_reallines){
		wblock->curr_realline=wblock->num_reallines-1;
	}

	wblock->wtrack=wblock->wtracks;

	newcurrtracksub=-1;
	window->curr_track=0;
	window->curr_track_sub=-1;

	SetCursorPosConcrete(window,wblock,newcurrtrack,newcurrtracksub);

	window->curr_block=wblock->l.num;

	DrawUpTrackerWindow(window);

	BS_SelectBlock(wblock->block);

	if( ! pc->isplaying){
		(*wblock->wtrack->track->instrument->PP_Update)(
			wblock->wtrack->track->instrument,
			wblock->wtrack->track->patch
		);
	}
}

void SelectPrevWBlock(struct Tracker_Windows *window){
	PlayStop();
	SelectWBlock(window,ListPrevElement1(&window->wblocks->l,&window->wblock->l));
}

void SelectNextWBlock(struct Tracker_Windows *window){
	PlayStop();
	SelectWBlock(window,NextWBlock(window->wblock));
}

void SelectPrevPlaylistWBlock(struct Tracker_Windows *window){
	struct Blocks *block=BL_GetBlockFromPos(root->curr_playlist-1);
	if(block==NULL) return;

	PlayStop();

	root->curr_playlist-=1;

	SelectWBlock(window,ListFindElement1(&window->wblocks->l,block->l.num));

	BS_SelectPlaylistPos(root->curr_playlist);
}

void SelectNextPlaylistWBlock(struct Tracker_Windows *window){
	struct Blocks *block=BL_GetBlockFromPos(root->curr_playlist+1);
	if(block==NULL) return;

	PlayStop();

	root->curr_playlist+=1;

	SelectWBlock(window,ListFindElement1(&window->wblocks->l,block->l.num));

	BS_SelectPlaylistPos(root->curr_playlist);
}

extern size_t allocated;

void AppendWBlock(struct Tracker_Windows *window){

	PlayStop();

	Undo_Block_Insert(root->song->num_blocks);

	AppendBlock();
	UpdateWBlocks(window);
	SelectWBlock(
		window,
		(struct WBlocks *)ListLast1(&window->wblocks->l)
	);
	BS_UpdateBlockList();

}

void AppendWBlock_spes(struct Tracker_Windows *window,int num_lines,NInt num_tracks){

	PlayStop();

	Undo_Block_Insert(root->song->num_blocks);

	AppendBlock_spes(num_lines,num_tracks);
	UpdateWBlocks(window);
	SelectWBlock(
		window,
		(struct WBlocks *)ListLast1(&window->wblocks->l)
	);
	BS_UpdateBlockList();

}















