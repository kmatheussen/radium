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
#include "player_proc.h"
#include "OS_Player_proc.h"
#include "PEQcommon_proc.h"
#include "placement_proc.h"
#include "PEQnotes_proc.h"
#include "PEQrealline_proc.h"
#include "PEQline_proc.h"
#include "PEQblock_proc.h"
#include "PEQfxs_proc.h"
#include "PEQ_clock_proc.h"
#include "PEQ_LPB_proc.h"
#include "PEQ_Signature_proc.h"
#include "PEQ_Beats_proc.h"

#include "player_pause_proc.h"


extern PlayerClass *pc;


extern void (*Ptask2MtaskCallBack)(void);

/*
void PC_Pause(void){
	PlayStop();
}
*/

void PC_Pause(void){

	if( ! ATOMIC_GET(pc->isplaying) ){
		return;					// There is only two threads, so its impossible to start playing
									// before PC_StopPause is called.
	}

#ifdef NOPAUSEPLAY
	PlayStop();
#else
	PausePlayer();				// Will not return before the player has really paused.

	PC_ReturnElements_fromPlayPos(pc->playpos);

	(*Ptask2MtaskCallBack)();
#endif
}


/*
void PC_StopPause(void){
	return;
}
*/

extern struct Root *root;

void PC_StopPause(void){
#ifdef NOPAUSEPLAY
  return;
#else
	Place place;
	STime pausetime;
	struct PEventQueue *peq;

	if( ! ATOMIC_GET(pc->isplaying)) return;

	pausetime=pc->pausetime;

	PlaceSetFirstPos(&place);

	InitPEQclock();
        InitPEQ_LPB(pc->block,place);
        InitPEQ_Signature(pc->block,place);
        InitPEQ_Beat(pc->block,place);
	InitPEQrealline(pc->block,&place);
	InitPEQline(pc->block,&place);
	InitPEQblock(pc->block,&place);
	InitAllPEQnotes(pc->block,&place);

	peq=pc->peq;
	while(peq!=NULL && pausetime>=peq->l.time){
		PC_RemoveFirst();

		(*peq->TreatMe)(pausetime,peq,0);

		peq=pc->peq;
	}

	root->setfirstpos=false;

	StopPausePlayer();
#endif
}




