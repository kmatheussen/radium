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
#include "time_proc.h"
#include "PEQmempool_proc.h"
#include "OS_Ptask2Mtask_proc.h"
#include "PEQcommon_proc.h"
#include "realline_calc_proc.h"
#include "list_proc.h"
#include "placement_proc.h"

#include "../audio/SoundPlugin.h"
#include "../audio/Pd_plugin_proc.h"

#include "PEQline_proc.h"

extern PlayerClass *pc;

extern struct Root *root;

void PlayerNewLine(struct PEventQueue *peq,int doit);
static void PlayerFirstLine(struct PEventQueue *peq,int doit);

void InitPEQline(struct Blocks *block,Place *place){
	int addplaypos=0;
	struct PEventQueue *peq=NULL;

        int line = place->line;
        if (place->counter>0)
          line++;

        if(line>=block->num_lines){
          line=0;
          addplaypos=1;
        }

        if(line==0) {
          peq=GetPEQelement();
          peq->TreatMe=PlayerFirstLine;
          peq->block=block;
          peq->line=0;
          PC_InsertElement_a_latencycompencated(
                                                peq,
                                                addplaypos,
                                                block->times[line].time
                                                );
          line = 1;
        }
        
        peq=GetPEQelement();
        peq->TreatMe=PlayerNewLine;
        peq->block=block;
        peq->line=line;
        
        PC_InsertElement_latencycompencated(
                                            peq,
                                            addplaypos,
                                            block->times[line].time
                                            );
        //printf("time: %d, addplaypos: %d, realline: %d, wblocknum: %d\n",(int)peq->l.time,(int)addplaypos,realline,wblock->l.num);
        //fflush(stdout);
}


static void PlayerFirstLine(struct PEventQueue *peq,int doit){

#ifdef WITH_PD
  int64_t next_time = ATOMIC_GET(pc->seqtime) + peq->block->times[1].time;

  RT_PD_set_line(peq->l.time, next_time, 0);

  //printf("FirstLine: %d, time: %d, nextrealline: %d, nexttime: %d, addplaypos: %d, pc->seqtime: %d\n",(int)0,(int)peq->l.time,(int)peq->realline,(int)next_time,(int)0,(int)pc->seqtime);
  //fflush(stdout);
#endif

  ReturnPEQelement(peq);
}



void PlayerNewLine(struct PEventQueue *peq,int doit){
	int addplaypos=0;
	int line=peq->line;
        //int time = peq->l.time;

#ifdef WITH_PD
        int org_line = peq->line;
        bool inserted_pd_line = false;
        int64_t org_time = peq->l.time;
#endif

	line++;

	if(pc->playtype==PLAYRANGE){
          RError("When did this happen?"); // PLAYRANGE has never been implemented
	}else{
		if(line>=peq->block->num_lines){
		        const struct Blocks *nextblock=PC_GetPlayBlock(1);
			if(nextblock==NULL){
				ReturnPEQelement(peq);
				return;
			}else{

                          struct PEventQueue *peq2=GetPEQelement();
                          peq2->TreatMe=PlayerFirstLine;
                          peq2->block=nextblock;
                          PC_InsertElement_a_latencycompencated(
                                                                peq2,
                                                                1,
                                                                nextblock->times[0].time
                                                                );
#ifdef WITH_PD
                          //printf("org_time: %f. next_time: %f\n",org_time/48000.0,peq2->l.time/48000.0);
                          RT_PD_set_line(org_time, peq2->l.time, org_line);
                          inserted_pd_line=true;
#endif

                          line=1;
                          peq->block=nextblock;
                          addplaypos=1;
			}
		}
	}

	peq->line=line;

        PC_InsertElement_latencycompencated(
                                            peq,
                                            addplaypos,
                                            peq->block->times[line].time
                                            );

#ifdef WITH_PD
        if(inserted_pd_line==false)
          RT_PD_set_line(org_time, peq->l.time, org_line);
#endif

	//printf("NewLine: %d (n: %d), time: %d, nextrealline: %d, nexttime: %d, addplaypos: %d, pc->seqtime: %d\n",org_line, (int)line,(int)time,(int)peq->realline,(int)peq->l.time,(int)addplaypos,(int)pc->seqtime);
        //fflush(stdout);

	return;
}


