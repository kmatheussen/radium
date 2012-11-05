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
#include "PEQcommon_proc.h"
#include "PEQmempool_proc.h"
#include "PEQfxs_proc.h"
#include "player_proc.h"
#include "OS_Ptask2Mtask_proc.h"
#include "placement_proc.h"
#include "time_proc.h"
#include "blocklist_proc.h"

#ifdef _AMIGA
#include <proto/exec.h>					//Has to be removed!
#endif

#include "list_proc.h"

#include "PEQblock_proc.h"




extern struct Root *root;
extern PlayerClass *pc;

void PlayerNewBlock(struct PEventQueue *peq,int doit);



void InitPEQblock(struct Blocks *block,Place *p){
	struct PEventQueue *peq;

	peq=GetPEQelement();
	peq->TreatMe=PlayerNewBlock;

	PC_InsertElement(peq,1,0);

	InitPEQBlockfxs(block,p);
}

int visit=0;

extern struct Task *mytask;

#ifdef _AMIGA
extern LONG clocksig;
#endif

/*******************************************************************
   Update gfx when changing block. Want to do this after all
   notes that is going to be started at the startpos of the block
   has been played, to avoid possible delay.
*******************************************************************/
void PlayerNewBlockGFX(struct PEventQueue *peq,int doit){

	// Update graphics.

	root->curr_block=pc->block->l.num;
	root->setfirstpos=true;
	if(doit){
		Ptask2Mtask(); // TODO: Add latency compensation. (not done since the code in this file is not quite clear)
	}

	// Insert next block-change event.

	peq->TreatMe=PlayerNewBlock;

	PC_InsertElement(peq,1,0);

}

/*******************************************************************
   When its time to change block.
*******************************************************************/

void PlayerNewBlock(struct PEventQueue *peq,int doit){
	Place firstplace;

	Pdebug("New block, peq->time: %d, time: %d, blocklength: %d, pc->seqtime: %d. visit: %d\n",peq->l.time,pc->start_time,getBlockSTimeLength(pc->block),pc->seqtime,visit++);

	// Is this the last block to be played?

	if(PC_GetPlayBlock(1)==NULL){
		pc->isplaying=false;
		ReturnPEQelement(peq);
#ifdef _AMIGA
		Signal(mytask,1L<<clocksig);
#endif
		return;
	}


	// Update playerclass and root information about current playpos and such.

	PC_GoNextBlock();

	// Init fxs for the block.
	PlaceSetFirstPos(&firstplace);
	InitPEQBlockfxs(pc->block,&firstplace);


	// Insert the block gfx-change event.

	peq->TreatMe=PlayerNewBlockGFX;

	ListAddElementP_a(&pc->peq,&peq->l);

	return;
}

/*
1-0. Start. 
2-0e. PNB
3-1	Go next
4-1e Insert
*/

