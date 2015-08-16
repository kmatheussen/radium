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

#include "PEQfxs_proc.h"


extern PlayerClass *pc;

void PE_HandleFirstFX(struct PEventQueue *peq,int doit);
void PE_HandleFX(struct PEventQueue *peq,int doit);

static void InitPEQfxs(
	const struct Blocks *block,
	const struct Tracks *track,
	struct FXs *fxs
){
	struct PEventQueue *peq=GetPEQelement();
	peq->TreatMe=PE_HandleFirstFX;
	peq->block=block;
	peq->track=track;
	peq->fxs=fxs;
	peq->fxnodeline=fxs->fxnodelines;
	peq->nextfxnodeline=NextFXNodeLine(peq->fxnodeline);

	peq->time1=Place2STime(block,&peq->fxnodeline->l.p);
	peq->time2=Place2STime(block,&peq->nextfxnodeline->l.p);

	PC_InsertElement(peq,0,peq->time1);
}


void InitPEQBlockfxs(
	const struct Blocks *block,
	const Place *p
){
	struct Tracks *track=block->tracks;
	struct FXs *fxs;

	while(track!=NULL){
		fxs=track->fxs;

		while(fxs!=NULL){
			InitPEQfxs(block,track,fxs);
			fxs=NextFX(fxs);
		}
		track=NextTrack(track);
	}
}

static void scheduled_fx_change(int64_t time, const union SuperType *args){
  struct FX    *fx    = args[0].pointer;
  int           x     = args[1].int_num;
  int64_t       skip  = args[2].int_num;
  FX_when       when  = args[3].int_num;
  
  RT_FX_treat_fx(fx, x, time, skip, when);
  
  if(fx->slider_automation_value!=NULL)
    *fx->slider_automation_value = scale(x,fx->min,fx->max,0.0f,1.0f);
  if(fx->slider_automation_color!=NULL)
    *fx->slider_automation_color = fx->color; // There's a race condition here. But it's unlikely to happen and has no bad consequence if it should.
}

static void fxhandle(int x, struct PEventQueue *peq, int skip, FX_when when){
  struct FX *fx = peq->fxs->fx;
  struct Patch *patch = peq->track->patch;

  if (fx==NULL)
    return;
  
  if (patch != fx->patch) // This can happen if changing patch for a track while playing.
    return;
  
  if (peq->track->onoff==0)
    return;
  
  union SuperType args[4];
  args[0].pointer = fx;
  args[1].int_num = x;
  args[2].int_num = skip;
  args[3].int_num = when;
  
  SCHEDULER_add_event(peq->l.time, scheduled_fx_change, &args[0], 4, SCHEDULER_FX_PRIORITY);
}

void PE_HandleFirstFX(struct PEventQueue *peq,int doit){
	int x;
	STime ntime,btime;
	struct FXNodeLines *next;

//	Pdebug("fx, start: %d\n",peq->fxnodeline->val);
	if(doit){
          fxhandle(peq->fxnodeline->val,peq,0,FX_start);
	}

	btime=PC_TimeToRelBlockStart(pc->end_time);

	peq->TreatMe=PE_HandleFX;

	if(btime>=peq->time2){
		next=NextFXNodeLine(peq->nextfxnodeline);

		if(next==NULL){
//			Pdebug("fx, slutt: %d\n",peq->nextfxnodeline->val);
			if(doit){
                          fxhandle(peq->nextfxnodeline->val,peq,0,FX_end);
			}
			ReturnPEQelement(peq);
		}else{
			peq->fxnodeline=peq->nextfxnodeline;
			peq->nextfxnodeline=next;
			peq->time1=peq->time2;
			peq->time2=Place2STime(peq->block,&next->l.p);
			PE_HandleFX(peq,doit);
		}
		return;
	}

	ntime=PEQ_CalcNextEvent(
		peq->time1,
		btime,
		peq->time2,
		peq->fxnodeline->val,
		&x,
		peq->nextfxnodeline->val
	);

	if(ntime>peq->time2) ntime=peq->time2;

	PC_InsertElement(peq,0,ntime);

	return;
}


void PE_HandleFX(struct PEventQueue *peq,int doit){
	int x;
	STime ntime,btime;
	struct FXNodeLines *next;

	btime=PC_TimeToRelBlockStart(pc->end_time);

	if(btime>=peq->time2){
		next=NextFXNodeLine(peq->nextfxnodeline);

		if(next==NULL){
//			Pdebug("fx, slutt: %d\n",peq->nextfxnodeline->val);
			if(doit){
                          fxhandle(peq->nextfxnodeline->val,peq,0,FX_end);
			}
			ReturnPEQelement(peq);
		}else{
			peq->fxnodeline=peq->nextfxnodeline;
			peq->nextfxnodeline=next;
			peq->time1=peq->time2;
			peq->time2=Place2STime(peq->block,&next->l.p);
			PE_HandleFX(peq,doit);
		}
		return;
	}

	ntime=PEQ_CalcNextEvent(
		peq->time1,
		btime,
		peq->time2,
		peq->fxnodeline->val,
		&x,
		peq->nextfxnodeline->val
	);

	if(ntime>peq->time2) ntime=peq->time2;

//	Pdebug("fx: %d\n",x);
	if(doit){
          fxhandle(x,peq,1,FX_middle);
	}

	PC_InsertElement(peq,0,ntime);

	return;
}



















