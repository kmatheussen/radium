/* Copyright 2000-2013 Kjetil S. Matheussen

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
#include "pitches_proc.h"
#include "patch_proc.h"
#include "scheduler_proc.h"

#include "PEQpitches_proc.h"

extern PlayerClass *pc;

// If patch max pitch is much lower than radium max pitch (MAX_PITCH), we avoid sending out two or more messages in a row with the same pitch value.
static STime PEQ_CalcNextPitchEvent(
                                struct PEventQueue *peq,
                                STime time1,
                                STime time,
                                STime time2,
                                float x1,
                                float *x,
                                float x2,
                                int logtype
                                )
{
  int int_x;
  STime ntime=PEQ_CalcNextEvent(
                                time1,time,time2,
                                scale(x1, 0,128,0,INT_MAX/2),
                                &int_x,
                                scale(x2, 0,128,0,INT_MAX/2),
                                logtype
                                );
  *x = scale(int_x, 0,INT_MAX/2, 0.0,128.0); // convert back.
  return ntime;
}


static void PE_ChangePitch(struct PEventQueue *peq,int doit);
static void PE_ChangePitchToEnd(struct PEventQueue *peq,int doit);

void InitPEQpitches(
	const struct Blocks *block,
	const struct Tracks *track,
	const struct Notes *note,
	int playlistaddpos
){

        if(track->patch==NULL)
          return;

	struct Pitches *pitch=note->pitches;

	if(pitch==NULL && note->pitch_end==0.0)
          return;


	struct PEventQueue *peq=GetPEQelement();
	peq->block=block;
	peq->track=track;
	peq->note=note;
        peq->pitch=pitch;
	peq->time1=Place2STime(block,&note->l.p);

        if (pitch==NULL) {

          peq->nextpitch = NULL;
          peq->time2=Place2STime(block,&note->end);
          peq->TreatMe=PE_ChangePitchToEnd;
          
        } else {
        
          peq->nextpitch = pitch;
          peq->time2=Place2STime(block,&pitch->l.p);
          peq->TreatMe=PE_ChangePitch;
          
        }
        
        int x;
	PC_InsertElement(
                         peq,playlistaddpos,
                         PEQ_CalcNextEvent(
                                           peq->time1,
                                           peq->time1,
                                           peq->time2,
                                           1,
                                           &x,
                                           20000000,
                                           note->pitch_first_logtype
                                           )
                         );
}


static void scheduled_change_pitch(int64_t time, const union SuperType *args){
  const struct Tracks *track = args[0].const_pointer;
  const struct Notes  *note  = args[1].const_pointer;
  float          x     = args[2].float_num;

  //printf("Sending pitch change %f\n",x);

  RT_PATCH_change_pitch(track->patch,
                        create_note_t2(note->id,
                                       x),
                        time
                        );
}

static void SendPitchChange(float x,struct PEventQueue *peq){
  if(peq->track->patch!=NULL && peq->track->onoff==1){

    union SuperType args[3];
    args[0].const_pointer = peq->track;
    args[1].const_pointer = peq->note;
    args[2].float_num = x;

    SCHEDULER_add_event(peq->l.time, scheduled_change_pitch, &args[0], 3, SCHEDULER_PITCH_PRIORITY);

    /*
      RT_PATCH_change_pitch(peq->track->patch,
      peq->note->note,
      x,
      peq->track,
      peq->l.time
      );
    */
  }
}


static void PE_ChangePitch(struct PEventQueue *peq,int doit){
	float x;
	STime ntime,btime;
	struct Pitches *next;

	btime=PC_TimeToRelBlockStart(pc->end_time);

	if(btime>=peq->time2){
		next=NextPitch(peq->nextpitch);
		peq->time1=peq->time2;
		peq->pitch=peq->nextpitch;

		if(next==NULL){
			peq->time2=Place2STime(peq->block,&peq->note->end);
			peq->TreatMe=PE_ChangePitchToEnd;
			PE_ChangePitchToEnd(peq,doit);
		}else{
			peq->nextpitch=next;
			peq->time2=Place2STime(peq->block,&next->l.p);
			PE_ChangePitch(peq,doit);
		}
		return;
	}

        float pitch1,pitch2,logtype;
        if(peq->pitch==peq->nextpitch){
          pitch1=peq->note->note;
          pitch2=peq->pitch->note;
          logtype=peq->note->pitch_first_logtype;
        }else{
          pitch1=peq->pitch->note;
          pitch2=peq->nextpitch->note;
          logtype=peq->pitch->logtype;
        }

	ntime=PEQ_CalcNextPitchEvent(
                peq,
		peq->time1,
		btime,
		peq->time2,
                pitch1,
		&x,
                pitch2,
                logtype
	);

	if(btime==ntime){
		Pdebug("Samme, stopper, x: %d\n",x);
		return;
	}

	if(ntime>peq->time2)
          ntime=peq->time2;

//	Pdebug("Player vel->vel, Pitch: %d, ntime: %d, btime: %d, time1: %d, time2: %d\n",x,ntime,btime,peq->time1,peq->time2);
	if(doit){
		SendPitchChange(x,peq);
	}

	PC_InsertElement(peq,0,ntime);

	return;

}

static void PE_ChangePitchToEnd(struct PEventQueue *peq,int doit){
	STime btime=PC_TimeToRelBlockStart(pc->end_time);

	if(btime>=peq->time2){
		ReturnPEQelement(peq);
		return;
	}

        const struct Notes *note = peq->note;

        float prev_pitch = peq->pitch!=NULL ? peq->pitch->note : note->note;
        int logtype      = peq->pitch!=NULL ? peq->pitch->logtype : note->pitch_first_logtype;
        
        float next_pitch = note->pitch_end;
        if(next_pitch<=1) // not supposed to happen though.
          next_pitch = 1;

        float x;

	STime ntime=PEQ_CalcNextPitchEvent(
                                           peq,
                                           peq->time1,
                                           btime,
                                           peq->time2,
                                           prev_pitch,
                                           &x,
                                           next_pitch,
                                           logtype
                                           );

	if(ntime>peq->time2){
		ReturnPEQelement(peq);
		return;
	}

//	Pdebug("Player vel->end, Pitch: %d, ntime: %d, btime: %d, time1: %d, time2: %d\n",x,ntime,btime,peq->time1,peq->time2);
	if(doit){
		SendPitchChange(x,peq);
	}

	PC_InsertElement(peq,0,ntime);

	return;

}
