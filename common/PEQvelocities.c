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
#include "PEQ_calc_proc.h"
#include "time_proc.h"
#include "patch_proc.h"
#include "scheduler_proc.h"
#include "visual_proc.h"

#include "PEQvelocities_proc.h"

extern PlayerClass *pc;

// If patch max velocity is much lower than radium max velocity (MAX_VELOCITY), we avoid sending out two or more messages in a row with the same velocity value.
static STime PEQ_CalcNextVelocityEvent(
                                struct PEventQueue *peq,
                                STime time1,
                                STime time,
                                STime time2,
                                int x1,
                                int *x,
                                int x2,
                                int logtype
                                )
{
#if 0
  STime next_time=PEQ_CalcNextEvent(
                                time1,time,time2,
                                PATCH_radiumvelocity_to_patchvelocity(peq->track->patch, x1),
                                x,
                                PATCH_radiumvelocity_to_patchvelocity(peq->track->patch, x2)
                                );
  *x = PATCH_patchvelocity_to_radiumvelocity(peq->track->patch, *x); // convert back.
  return next_time;
#else
  return PEQ_CalcNextEvent(
                           time1,time,time2,
                           x1,
                           x,
                           x2,
                           logtype
                           );
#endif
}


static void PE_ChangeVelocityFromStart(struct PEventQueue *peq,int doit);
static void PE_ChangeVelocity(struct PEventQueue *peq,int doit);
static void PE_ChangeVelocityToEnd(struct PEventQueue *peq,int doit);
static void PE_ChangeVelocityFromStartToEnd(struct PEventQueue *peq,int doit);

void InitPEQvelocities(
	const struct Blocks *block,
	const struct Tracks *track,
	struct Notes *note,
	int playlistaddpos
){
	int x;
	struct PEventQueue *peq;

	const struct Velocities *velocity=note->velocities;

        if(track->patch==NULL)
          return;

	if(velocity==NULL && note->velocity == note->velocity_end)
          return;

	peq=GetPEQelement();
	peq->block=block;
	peq->track=track;
	peq->note=note;
	peq->time1=Place2STime(block,&note->l.p);

	if(velocity==NULL){
		peq->time2=Place2STime(block,&note->end);
		peq->TreatMe=PE_ChangeVelocityFromStartToEnd;
		PC_InsertElement(
			peq,playlistaddpos,
			PEQ_CalcNextVelocityEvent(
                                                  peq,
                                                  peq->time1,
                                                  peq->time1,
                                                  peq->time2,
                                                  note->velocity,
                                                  &x,
                                                  note->velocity_end,
                                                  note->velocity_first_logtype
			)
		);
		return;
	}

	peq->time2=Place2STime(block,&velocity->l.p);
	peq->TreatMe=PE_ChangeVelocityFromStart;
	peq->velocity=velocity;

	PC_InsertElement(
                         peq,playlistaddpos,
                         PEQ_CalcNextVelocityEvent(
                                                   peq,
                                                   peq->time1,
                                                   peq->time1,
                                                   peq->time2,
                                                   note->velocity,
                                                   &x,
                                                   velocity->velocity,
                                                   note->velocity_first_logtype
                                                   )
                         );

}


static void scheduled_change_velocity(int64_t time, const union SuperType *args){
  const struct Tracks *track = args[0].const_pointer;
  const struct Notes  *note  = args[1].const_pointer;
  int            x           = (int)args[2].int_num;

  //printf("   Change velocity to %d\n",x);

  RT_PATCH_change_velocity(track->patch,
                           create_note_t(note->id,
                                         note->note,
                                         TRACK_get_velocity(track,x),
                                         0,
                                         0),
                           time
                           );
}

static void SendVelocityChange(int x,struct PEventQueue *peq){
	if(peq->track->patch!=NULL && peq->track->onoff==1){
          union SuperType args[3];
          args[0].const_pointer = peq->track;
          args[1].const_pointer = peq->note;
          args[2].int_num = x;

          //printf("Sending velocity %x\n",x);
          // Note that the end velocity is never sent out at note_end time. If it had, those velocities must have been scheduled with priorith 0.
          SCHEDULER_add_event(peq->l.time, scheduled_change_velocity, &args[0], 3, SCHEDULER_VELOCITY_PRIORITY);

          /*
          RT_PATCH_change_velocity(peq->track->patch,
                                   peq->note->note,
                                   x,
                                   peq->track,
                                   peq->l.time
                                   );
          */
	}
}


static void PE_ChangeVelocityFromStart(struct PEventQueue *peq,int doit){
	int x;
	struct Velocities *next;

	STime btime=PC_TimeToRelBlockStart(pc->end_time);

	if(btime>=peq->time2){
		next=NextVelocity(peq->velocity);
		peq->time1=peq->time2;
		if(next==NULL){
			peq->time2=Place2STime(peq->block,&peq->note->end);
			peq->TreatMe=PE_ChangeVelocityToEnd;
			PE_ChangeVelocityToEnd(peq,doit);
		}else{
			peq->nextvelocity=next;
			peq->time2=Place2STime(peq->block,&next->l.p);
			peq->TreatMe=PE_ChangeVelocity;
			PE_ChangeVelocity(peq,doit);
		}
		return;
	}

	STime next_time=PEQ_CalcNextVelocityEvent(
                                                  peq,
                                                  peq->time1,
                                                  btime,
                                                  peq->time2,
                                                  peq->note->velocity,
                                                  &x,
                                                  peq->velocity->velocity,
                                                  peq->note->velocity_first_logtype
                                                  );
        
#if 0
	if(btime==next_time){
		Pdebug("btime==next_time, stopper, x: %d, btime: %d, x1: %d, x2: %d\n",x,btime,peq->note->velocity,peq->velocity->velocity);
		return;
	}
#endif
        
	if(next_time>peq->time2)
          next_time=peq->time2;

//	Pdebug("start->vel,Velocity: %d, time: %d, next_time: %d, btime: %d, time1: %d, time2: %d\n",x,time,next_time,btime,peq->time1,peq->time2);
	if(doit){
          SendVelocityChange(x,peq);
	}

        //printf("start. next_time: %d\n",(int)next_time);
	PC_InsertElement(peq,0,next_time);

	return;
}



static void PE_ChangeVelocity(struct PEventQueue *peq,int doit){
	int x;
	STime next_time,btime;
	struct Velocities *next;

	btime=PC_TimeToRelBlockStart(pc->end_time);

	if(btime>=peq->time2){
		next=NextVelocity(peq->nextvelocity);
		peq->time1=peq->time2;
		peq->velocity=peq->nextvelocity;

		if(next==NULL){
			peq->time2=Place2STime(peq->block,&peq->note->end);
			peq->TreatMe=PE_ChangeVelocityToEnd;
			PE_ChangeVelocityToEnd(peq,doit);
		}else{
			peq->nextvelocity=next;
			peq->time2=Place2STime(peq->block,&next->l.p);
			PE_ChangeVelocity(peq,doit);
		}
                //printf("change. btime>=peq->time2. btime: %d, peq->time2: %d\n",(int)btime,(int)peq->time2);
		return;
	}

	next_time=PEQ_CalcNextVelocityEvent(
                peq,
		peq->time1,
		btime,
		peq->time2,
		peq->velocity->velocity,
		&x,
		peq->nextvelocity->velocity,
                peq->velocity->logtype
	);

#if 0
	if(btime==next_time){      
#ifndef RELEASE
          GFX_Message(NULL, "change. Same time. Stopping. x: %d, next_time: %d\n",(int)x,(int)next_time);
#endif
          return;
	}
#endif
        
	if(next_time>peq->time2)
          next_time=peq->time2;

//	Pdebug("Player vel->vel, Velocity: %d, next_time: %d, btime: %d, time1: %d, time2: %d\n",x,next_time,btime,peq->time1,peq->time2);
	if(doit){
          SendVelocityChange(x,peq);
	}

        //printf("change. next_time: %d\n",(int)next_time);
	PC_InsertElement(peq,0,next_time);

	return;

}



static void PE_ChangeVelocityToEnd(struct PEventQueue *peq,int doit){
	int x;
	STime next_time,btime;

	btime=PC_TimeToRelBlockStart(pc->end_time);

	if(btime>=peq->time2){
		ReturnPEQelement(peq);
		return;
	}

	next_time=PEQ_CalcNextVelocityEvent(
                                        peq,
                                        peq->time1,
                                        btime,
                                        peq->time2,
                                        peq->velocity->velocity,
                                        &x,
                                        peq->note->velocity_end+1,		// Don't really know why I have to add 1, but it works...
                                        peq->velocity->logtype
                                        );

	if(next_time>=peq->time2){
		ReturnPEQelement(peq);
		return;
	}

//	Pdebug("Player vel->end, Velocity: %d, next_time: %d, btime: %d, time1: %d, time2: %d\n",x,next_time,btime,peq->time1,peq->time2);
	if(doit){
          SendVelocityChange(x,peq);
	}

        //printf("end. next_time: %d\n",(int)next_time);
	PC_InsertElement(peq,0,next_time);

	return;

}

static void PE_ChangeVelocityFromStartToEnd(struct PEventQueue *peq,int doit){
	int x;
	STime next_time,btime;

	btime=PC_TimeToRelBlockStart(pc->end_time);

	if(btime>=peq->time2){
		ReturnPEQelement(peq);
		return;
	}

	next_time=PEQ_CalcNextVelocityEvent(
                                        peq,
                                        peq->time1,
                                        btime,
                                        peq->time2,
                                        peq->note->velocity,
                                        &x,
                                        peq->note->velocity_end+1,		// Don't really know why I have to add 1, but it works...                                        
                                        peq->note->velocity_first_logtype
                                        );

	if(next_time>=peq->time2){
		ReturnPEQelement(peq);
		return;
	}


//	Pdebug("Player start->end, Velocity: %d\n",x);
	if(doit){
          SendVelocityChange(x,peq);
	}

        //printf("start->end. next_time: %d\n",(int)next_time);
	PC_InsertElement(peq,0,next_time);

	return;
}


