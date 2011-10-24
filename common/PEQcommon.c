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





/******************** OVERVIEW ******************************

  This file is, sort of, the playerclasses methods (see
  playerclass.h).

  This isn't oo code, but I like to pretend that it is
  to make it less messy. (yeah, hmm, well, something like that...)

  This file is the only one that access the playerclasse's
  "attributes", except player_startstop.c and player.c.

************************************************************/


#include "nsmtracker.h"
#include "playerclass.h"
#include "PEQmempool_proc.h"
#include "blocklist_proc.h"
#include "time_proc.h"
#include "list_proc.h"
#include "OS_Bs_edit_proc.h"

#include "PEQcommon_proc.h"




extern PlayerClass *pc;
extern struct Root *root;


__inline void PC_RemoveFirst(void){
	pc->peq=NextPEventQueue(pc->peq);
}

struct Blocks *PC_GetPlayBlock(int numfromcurrent){

	if(numfromcurrent==0) return pc->block;

	if(pc->playtype==PLAYBLOCK){
		if(numfromcurrent>1) return NULL;
		return pc->block;
	}

	return BL_GetBlockFromPos(root->curr_playlist+numfromcurrent);

}


__inline void PC_InsertElement_private(struct PEventQueue *peq, int addplaypos, STime addtime,bool before){
	int time=pc->seqtime;
	int playpos;

	if(addplaypos>0){
		if(pc->playtype==PLAYBLOCK){
			time+=getBlockSTimeLength(pc->block);	// When playblock, addplaypos can't be bigger than one.
		}else{
			for(
				playpos=root->curr_playlist;
				playpos<root->curr_playlist+addplaypos;
				playpos++
			){
				time+=
					getBlockSTimeLength(
						BL_GetBlockFromPos(playpos)
					);
			}
		}
	}

	peq->l.time=addtime+time;

	peq->playpos=pc->playpos+addplaypos;

	if(before){
		ListAddElementP(&pc->peq,&peq->l);
	}else{
		ListAddElementP_a(&pc->peq,&peq->l);
	}
}

void PC_InsertElement(
	struct PEventQueue *peq, int addplaypos, STime addtime
){
	PC_InsertElement_private(peq,addplaypos,addtime,true);
}

void PC_InsertElement_a(
	struct PEventQueue *peq, int addplaypos, STime addtime
){
	PC_InsertElement_private(peq,addplaypos,addtime,false);
}


__inline void PC_InsertElement2_private(struct PEventQueue *peq, int addplaypos, Place *p, bool before){

	PC_InsertElement_private(

		peq,addplaypos,

		Place2STime(
			PC_GetPlayBlock(addplaypos),
			p
		),

		before

	);

}

void PC_InsertElement2(struct PEventQueue *peq, int addplaypos, Place *p){
	PC_InsertElement2_private(peq,addplaypos,p,true);
}

void PC_InsertElement2_a(struct PEventQueue *peq, int addplaypos, Place *p){
	PC_InsertElement2_private(peq,addplaypos,p,false);
}


void PC_ReturnElements(void){
	struct PEventQueue *peq;
	struct PEventQueue *temp;

	/* Return all PEventQueue elements. */
	peq=pc->peq;

	if(peq!=NULL){
		while(peq!=NULL){
			temp=NextPEventQueue(peq);
	
			ReturnPEQelement(peq);
	
			peq=temp;
		}
	}

	pc->peq=NULL;

}


void PC_ReturnElements_fromPlayPos(int playpos){
	struct PEventQueue *peq;
	struct PEventQueue *temp;

	peq=pc->peq;

	while(peq!=NULL){
		temp=NextPEventQueue(peq);

		if(peq->playpos>=playpos){
			ListRemoveElementP(&pc->peq,peq);
			ReturnPEQelement(peq);
		}

		peq=temp;
	}

}


void PC_GoNextBlock(void){
	struct Tracks *track;
	pc->seqtime+=getBlockSTimeLength(pc->block);

	pc->block=PC_GetPlayBlock(1);

	Pdebug("PC_GoNextBlock\n");

	if(pc->block!=NULL){
		track=pc->block->tracks;
		while(track!=NULL){
			if(track->panonoff && track->patch!=NULL){
				(*track->patch->changeTrackPan)(track->pan,track);
			}
			track=NextTrack(track);
		}
	}

	pc->playpos++;
	if(pc->playtype==PLAYSONG) root->curr_playlist++;

}


bool PC_isPlayingBlock(void){
	if(pc->playtype==PLAYBLOCK) return true;
	return false;
}

bool PC_isPlayingSong(void){
	if(pc->playtype==PLAYSONG) return true;
	return false;
}


STime PC_TimeToRelBlockStart(STime time){
	return time-pc->seqtime;
}


