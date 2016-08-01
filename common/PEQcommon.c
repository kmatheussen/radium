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

        if(numfromcurrent>=1 && pc->playtype==PLAYBLOCK_NONLOOP)
          return NULL;

	if(pc->playtype==PLAYBLOCK){
          if(numfromcurrent>1) return NULL; // what?
          return pc->block;
	}

	return BL_GetBlockFromPos(ATOMIC_GET(root->curr_playlist)+numfromcurrent);
}

bool PC_GetNextNoteAfterCurrentBlock(NInt tracknum, int *playlistaddpos, struct Notes **note, struct Tracks **track, const struct Blocks **block){
  *playlistaddpos=0;

  for(;;){
    (*playlistaddpos)++;

    *block = PC_GetPlayBlock(*playlistaddpos);
    if(*block==NULL)
      break;

    *track = ListFindElement1_r0(&(*block)->tracks->l,tracknum);    
    if(*track != NULL) {
      *note = (*track)->notes;
      if(*note!=NULL)
        return true;
    }    
  }
  
  return false;
}

static void PC_InsertElement_private(struct PEventQueue *peq, int addplaypos, STime addtime,bool before,bool add_latency){
        STime time=ATOMIC_GET(pc->seqtime);
	int playpos;

	if(addplaypos>0){
		if(pc->playtype==PLAYBLOCK || pc->playtype==PLAYBLOCK_NONLOOP){
			time+=getBlockSTimeLength(pc->block);	// When playblock, addplaypos can't be bigger than one.
		}else{
                  int curr_playlistpos = ATOMIC_GET(root->curr_playlist);
                  for(
                      playpos=curr_playlistpos;
                      playpos<curr_playlistpos+addplaypos;
                      playpos++
                      )
                    {
                      time+=
                        getBlockSTimeLength(
                                            BL_GetBlockFromPos(playpos)
                                            );
                    }
		}
	}

	//peq->l.time=addtime + time + (add_latency ? LATENCY : 0); // This didn't work properly I don't quite understand the code.
	peq->l.time=addtime + time;

        if(peq->l.time < 0){
#if !defined(RELEASE)
          fprintf(stderr,"peq->l.time<0: %d. addtime: %d, time: %d\n",(int)peq->l.time,(int)addtime,(int)time);
          abort();
#endif
          ReturnPEQelement(peq);
          return;
        }
        
        // (time can be negative when starting to play.)
        // R_ASSERT_RETURN_IF_FALSE(peq->l.time >= 0);

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
  PC_InsertElement_private(peq,addplaypos,addtime,true,false);
}

void PC_InsertElement_latencycompencated(
	struct PEventQueue *peq, int addplaypos, STime addtime
){
  PC_InsertElement_private(peq,addplaypos,addtime,true,true);
}

void PC_InsertElement_a(
	struct PEventQueue *peq, int addplaypos, STime addtime
){
  PC_InsertElement_private(peq,addplaypos,addtime,false,false);
}


void PC_InsertElement_a_latencycompencated(
	struct PEventQueue *peq, int addplaypos, STime addtime
){
  PC_InsertElement_private(peq,addplaypos,addtime,false,true);
}


static void PC_InsertElement2_private(struct PEventQueue *peq, int addplaypos, const Place *p, bool before, bool add_latency){

	PC_InsertElement_private(

		peq,addplaypos,

		Place2STime(
			PC_GetPlayBlock(addplaypos),
			p
		),

		before,

		add_latency

	);

}

void PC_InsertElement2_latencycompencated(struct PEventQueue *peq, int addplaypos, const Place *p){
  PC_InsertElement2_private(peq,addplaypos,p,true,true);
}

void PC_InsertElement2(struct PEventQueue *peq, int addplaypos, const Place *p){
  PC_InsertElement2_private(peq,addplaypos,p,true,false);
}

void PC_InsertElement2_a(struct PEventQueue *peq, int addplaypos, const Place *p){
  PC_InsertElement2_private(peq,addplaypos,p,false,false);
}

void PC_InsertElement2_a_latencycompencated(struct PEventQueue *peq, int addplaypos, const Place *p){
  PC_InsertElement2_private(peq,addplaypos,p,false,true);
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
	ATOMIC_ADD(pc->seqtime, getBlockSTimeLength(pc->block));

        struct Blocks *next_block = PC_GetPlayBlock(1);

        if (pc->block != next_block) {
          if (next_block != NULL)
            ATOMIC_DOUBLE_SET(next_block->player_time, -100.0); // Any value less than -10 will delay rendering the new block. Instead we wait until player.c is called and a proper player_time value is calculated, to avoid jumpy graphics.
          
          atomic_pointer_write((void**)&pc->block, next_block);
        }
        
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
	if(pc->playtype==PLAYSONG)
          ATOMIC_ADD(root->curr_playlist, 1);
}


bool PC_isPlayingBlock(void){
	if(pc->playtype==PLAYBLOCK || pc->playtype==PLAYBLOCK_NONLOOP) return true;
	return false;
}

bool PC_isPlayingSong(void){
	if(pc->playtype==PLAYSONG) return true;
	return false;
}


STime PC_TimeToRelBlockStart(STime time){
  return time - ATOMIC_GET(pc->seqtime);
}
