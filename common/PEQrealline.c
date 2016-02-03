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
#include "time_proc.h"
#include "PEQmempool_proc.h"
#include "OS_Ptask2Mtask_proc.h"
#include "PEQcommon_proc.h"
#include "realline_calc_proc.h"
#include "list_proc.h"
#include "placement_proc.h"
#include "../OpenGL/Widget_proc.h"

#include "../audio/SoundPlugin.h"
#include "../audio/Pd_plugin_proc.h"

#include "PEQrealline_proc.h"

extern PlayerClass *pc;

extern struct Root *root;

void PlayerNewRealline(struct PEventQueue *peq,int doit);
static void PlayerFirstRealline(struct PEventQueue *peq,int doit);

void InitPEQrealline(struct Blocks *block,Place *place){
	int addplaypos=0;
	int realline;
	struct WBlocks *wblock;
	struct Tracker_Windows *window=root->song->tracker_windows;
	struct PEventQueue *peq=NULL;

	while(window!=NULL){

		wblock=(struct WBlocks *)ListFindElement1(&window->wblocks->l,block->l.num);

		realline=FindRealLineFor(wblock,0,place);

		if(realline>=wblock->num_reallines){
			realline=0;
			wblock=(struct WBlocks *)ListFindElement1(
                                                                  &window->wblocks->l,PC_GetPlayBlock(1)->l.num
                                                                  );
			addplaypos=1;
		}

		if(realline==0) {
                  peq=GetPEQelement();
                  peq->TreatMe=PlayerFirstRealline;
                  peq->block=block;
                  peq->wblock=wblock;
                  PC_InsertElement2_a_latencycompencated(
                                                         peq, addplaypos,&wblock->reallines[0]->l.p
                                                         );
                  realline = 1;
                }

		peq=GetPEQelement();
		peq->TreatMe=PlayerNewRealline;
		peq->window=window;
		peq->wblock=wblock;
		peq->realline=realline;

		PC_InsertElement2_latencycompencated(
			peq, addplaypos,&wblock->reallines[realline]->l.p
		);
		//printf("time: %d, addplaypos: %d, realline: %d, wblocknum: %d\n",(int)peq->l.time,(int)addplaypos,realline,wblock->l.num);
                //fflush(stdout);

		window=NextWindow(window);
	}
	debug("init. peq->realline: %d\n",peq->realline);
}


static void PlayerFirstRealline(struct PEventQueue *peq,int doit){
#ifdef WITH_PD
	Place firstplace;
	PlaceSetFirstPos(&firstplace);

        int64_t next_time = pc->seqtime + Place2STime(
                                                      pc->block,
                                                      &peq->wblock->reallines[1]->l.p
                                                      );

        RT_PD_set_realline(peq->l.time, next_time, &firstplace);
#endif

        ReturnPEQelement(peq);
}

/*
"peq->realline" can have illegal value:

$1 = (Place *) 0x8
(gdb) bt
#0  0x0000000000717b91 in RT_PD_set_realline (time=9591811, time_nextrealline=10057999, p=0x8) at audio/Pd_plugin.cpp:469
#1  0x00000000005a1e66 in PlayerNewRealline (peq=0x367ffb0, doit=1) at common/PEQrealline.c:151
#2  0x00000000005a1413 in PlayerTask (reltime=64) at common/player.c:116
#3  0x00000000007204ea in Mixer::RT_thread (this=this@entry=0x2d7c240) at audio/Mixer.cpp:459
#4  0x00000000007206bf in Mixer::RT_rjack_thread (arg=0x2d7c240) at audio/Mixer.cpp:502
#5  0x0000003b564163fd in Jack::JackClient::Execute (this=0x2d8f150) at ../common/JackClient.cpp:565
#6  0x0000003b56435910 in Jack::JackPosixThread::ThreadHandler (arg=0x2d8f2c0) at ../posix/JackPosixThread.cpp:59
#7  0x0000003b49c07d14 in start_thread () from /lib64/libpthread.so.0
#8  0x0000003b494f168d in clone () from /lib64/libc.so.6
(gdb) thread apply all bt

(should be fixed)
*/

void PlayerNewRealline(struct PEventQueue *peq,int doit){
	int addplaypos=0;
	int realline=peq->realline;
	//int orgrealline=realline;

#ifdef WITH_PD
        bool inserted_pd_realline = false;
        int64_t org_time = peq->l.time;
        Place *org_pos = NULL;

        if (realline < peq->wblock->num_reallines) // number of reallines can change while playing.
          org_pos = &peq->wblock->reallines[realline]->l.p;
#endif

        if (!ATOMIC_GET(root->play_cursor_onoff)){
          
          // Set current realline in main thread (main thread picks up till_curr_realline and sets curr_realline afterwards)          
          peq->wblock->till_curr_realline = realline;
          
          // Set current realline in opengl thread
          //printf("PEQ: set realline %d\n",realline);
          GE_set_curr_realline(realline);
          
        }

        Play_set_curr_playing_realline(realline, peq->wblock->l.num);        
        
	if(doit){
		Ptask2Mtask();
	}

	realline++;
	if(pc->playtype==PLAYRANGE){
		if(realline>=peq->wblock->rangey2){
			realline=peq->wblock->rangey1;
		}

                // sanity checks to avoid crash. May happen if editing reallines while playing.
                if (realline>=peq->wblock->num_reallines) // If outside range, first try to set realline to rangey1
                  realline = peq->wblock->rangey1;

                if (realline>=peq->wblock->num_reallines) // that didnt work, set realline to 0
                  realline = 0;

	}else{
		if(realline>=peq->wblock->num_reallines){
		        const struct Blocks *nextblock=PC_GetPlayBlock(1);
			if(nextblock==NULL){
				ReturnPEQelement(peq);
				return;
			}else{

                          Place firstplace;
                          PlaceSetFirstPos(&firstplace);

                          struct PEventQueue *peq2=GetPEQelement();
                          peq2->TreatMe=PlayerFirstRealline;
                          peq2->block=nextblock;
                          peq2->wblock=(struct WBlocks *)ListFindElement1(
                                                                          &peq->window->wblocks->l,nextblock->l.num
                                                                          );
                          PC_InsertElement2_a_latencycompencated(
                                                                 peq2, 1,&firstplace
                                                                 );
#ifdef WITH_PD
                          //printf("org_time: %f. next_time: %f\n",org_time/48000.0,peq2->l.time/48000.0);
                          if (org_pos != NULL)
                            RT_PD_set_realline(org_time, peq2->l.time, org_pos);
                          inserted_pd_realline=true;
#endif

                          realline=1;
                          peq->wblock= peq2->wblock;
                          peq->block=nextblock;
                          addplaypos=1;
			}
		}
	}

	peq->realline=realline;

        PC_InsertElement2_latencycompencated(
                                             peq, addplaypos ,&peq->wblock->reallines[realline]->l.p // Race condition? Can the reallines change between the above check and here?
                                             );

#ifdef WITH_PD
        if (org_pos != NULL)
          if(inserted_pd_realline==false)
            RT_PD_set_realline(org_time, peq->l.time, org_pos);
#endif


	//printf("NewRealline: %d, time: %d, nextrealline: %d, nexttime: %d, addplaypos: %d, pc->seqtime: %d\n",(int)orgrealline,(int)time,(int)peq->realline,(int)peq->l.time,(int)addplaypos,(int)pc->seqtime);
        //fflush(stdout);

	return;
}


