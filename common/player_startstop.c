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
#include "playerclass.h"
#include "placement_proc.h"
#include "OS_Player_proc.h"
#include "PEQrealline_proc.h"
#include "PEQblock_proc.h"
#include "PEQnotes_proc.h"
#include "instruments_proc.h"
#include "blocklist_proc.h"
#include "OS_Ptask2Mtask_proc.h"
#include "time_proc.h"
#include "PEQ_clock_proc.h"
#include "gfx_upperleft_proc.h"
#include "OS_Bs_edit_proc.h"
#include "list_proc.h"
#include "clipboard_range_calc_proc.h"

#include "player_proc.h"


extern STime dastime;


/******************** NOTES ******************************

  Even more "methods" for the playerclass struct.

*********************************************************/

extern PlayerClass *pc;
extern struct Root *root;

extern void (*Ptask2MtaskCallBack)(void);

void PlayStopReally(bool doit){ 
	pc->isplaying=false;
	pc->initplaying=false;

	while(pc->peq!=NULL) OS_WaitForAShortTime(20);

	StopAllInstruments();

	if(doit) (*Ptask2MtaskCallBack)();

	dastime=0;

	GFX_UpdateQuantitize(root->song->tracker_windows,root->song->tracker_windows->wblock);
}

void PlayHardStop(void){
	if(! pc->isplaying){
		StopAllInstruments();
		return;
	}
	PlayStopReally(true);
}

void PlayStop(void){
	if(pc->isplaying){
		StopAllInstruments();
		pc->isplaying=false;
		pc->initplaying=false;

   		while(pc->peq!=NULL) OS_WaitForAShortTime(20);

		PlayStopReally(true);
	}
}


void PlayBlock(
	struct Blocks *block,
	Place *place
){

	pc->initplaying=true;

		pc->playpos=0;

		pc->playtype=PLAYBLOCK;

		pc->block=block;

		root->curr_block=pc->block->l.num;
		debug("root->curr_block: %d\n",root->curr_block);

		pc->isplaying=true;
		(*Ptask2MtaskCallBack)();
		pc->isplaying=false;

		InitPEQclock();
		InitPEQrealline(block,place);
		InitPEQblock(block,place);
		InitAllPEQnotes(block,place);

		StartPlayer();							// An OS spesific function.
		pc->isplaying=true;


	pc->initplaying=false;
}

void PlayBlockFromStart(struct Tracker_Windows *window){
	Place place;

	struct WBlocks *wblock=window->wblock;

	PlayStopReally(false);
	PlaceSetFirstPos(&place);

	root->setfirstpos=true;
	pc->seqtime=0;
	PlayBlock(wblock->block,&place);
}

void PlayBlockCurrPos(struct Tracker_Windows *window){
	struct WBlocks *wblock;
	Place *place;
	PlayStopReally(false);

	root->setfirstpos=false;

	wblock=window->wblock;

	if(wblock->curr_realline==0) root->setfirstpos=true;

	place=&wblock->reallines[wblock->curr_realline]->l.p;
	pc->seqtime=-Place2STime(wblock->block,place);

//	printf("contblock, time: %d\n",pc->seqtime);

	PlayBlock(wblock->block,place);
}


void PlayRangeCurrPos(struct Tracker_Windows *window){
	struct WBlocks *wblock;
	Place *place;

	PlayStopReally(false);

	wblock=window->wblock;

	if( ! wblock->isranged) return;

	root->setfirstpos=false;

	if(wblock->rangey1==0) root->setfirstpos=true;

	place=getRangeStartPlace(wblock);
	pc->seqtime=-Place2STime(wblock->block,place);

//	printf("playrange, time: %d\n",pc->seqtime);

	PlayBlock(wblock->block,place);
}

void PlaySong(
	Place *place,
	int playpos
){
	struct Blocks *block;

	debug("haaasfdfsafsa, root->song->length: %d\n\n\n",root->song->length);
	pc->initplaying=true;

		block=BL_GetBlockFromPos(playpos);

		debug("blocknum:%d\n",block->l.num);

		pc->playpos=playpos;

		root->curr_playlist=playpos;

		pc->playtype=PLAYSONG;

		pc->block=block;

		root->curr_block=block->l.num;
		pc->isplaying=true;
		(*Ptask2MtaskCallBack)();
		pc->isplaying=false;

		InitPEQclock();
		InitPEQrealline(block,place);
		InitPEQblock(block,place);
		InitAllPEQnotes(block,place);

		StartPlayer();							// An OS spesific function.
		pc->isplaying=true;


	pc->initplaying=false;
}


void PlaySongFromStart(struct Tracker_Windows *window){

	Place place;

	PlaceSetFirstPos(&place);

	PlayStopReally(false);

	BS_SelectPlaylistPos(0);
	debug("root->curr_block: %d\n",root->curr_block);
	root->setfirstpos=true;
	pc->seqtime=0;

	InitAllInstrumentsForPlaySongFromStart();

	PlaySong(&place,0);
}

void PlaySongCurrPos(struct Tracker_Windows *window){
	struct Blocks *block;
	struct WBlocks *wblock;
	Place *place;
	int playpos;
	bool changeblock=false;

	wblock=window->wblock;

	PlayStopReally(false);

	root->setfirstpos=false;

	playpos=root->curr_playlist;

	block=BL_GetBlockFromPos(playpos);
	if(block==NULL) return;

	if(wblock->l.num!=block->l.num){
		wblock=ListFindElement1(&window->wblocks->l,block->l.num);
		changeblock=true;
		root->setfirstpos=true;
	}

	if(
		! changeblock &&
		playpos==root->song->length-1 &&
		wblock->curr_realline==wblock->num_reallines
	){
		return;
	}

	if(wblock->curr_realline==0) root->setfirstpos=true;


	debug("contsong, playpos: %d , root->curr_block: %d\n",playpos,root->curr_block);

	if(changeblock){
		place=PlaceGetFirstPos();
		pc->seqtime=0;
	}else{
		place=&wblock->reallines[wblock->curr_realline]->l.p;
		pc->seqtime=-Place2STime(wblock->block,place);
	}
	debug("contsong, time: %d, playpos: %d , root->curr_block: %d\n",pc->seqtime,playpos,root->curr_block);

	place->line++;
	debug("nextline: %d\n",Place2STime(wblock->block,place));
	place->line--;

	PlaySong(place,playpos);
}










