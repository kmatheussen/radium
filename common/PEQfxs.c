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

#include "PEQfxs_proc.h"



void PE_HandleFirstFX(STime time,struct PEventQueue *peq,int doit);
void PE_HandleFX(STime time,struct PEventQueue *peq,int doit);

void InitPEQfxs(
	struct Blocks *block,
	struct Tracks *track,
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
	struct Blocks *block,
	Place *p
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


__inline void fxhandle(int x,struct PEventQueue *peq,int skip){
	if(peq->fxs->fx!=NULL && peq->track->onoff==1){
		(*peq->fxs->fx->treatFX)(peq->fxs->fx,x,peq->track,skip);
	}
}

void PE_HandleFirstFX(STime time,struct PEventQueue *peq,int doit){
	int x;
	STime ntime,btime;
	struct FXNodeLines *next;

//	Pdebug("fx, start: %d\n",peq->fxnodeline->val);
	if(doit){
		fxhandle(peq->fxnodeline->val,peq,0);
	}

	btime=PC_TimeToRelBlockStart(time);

	peq->TreatMe=PE_HandleFX;

	if(btime>=peq->time2){
		next=NextFXNodeLine(peq->nextfxnodeline);

		if(next==NULL){
//			Pdebug("fx, slutt: %d\n",peq->nextfxnodeline->val);
			if(doit){
				fxhandle(peq->nextfxnodeline->val,peq,0);
			}
			ReturnPEQelement(peq);
		}else{
			peq->fxnodeline=peq->nextfxnodeline;
			peq->nextfxnodeline=next;
			peq->time1=peq->time2;
			peq->time2=Place2STime(peq->block,&next->l.p);
			PE_HandleFX(time,peq,doit);
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


void PE_HandleFX(STime time,struct PEventQueue *peq,int doit){
	int x;
	STime ntime,btime;
	struct FXNodeLines *next;

	btime=PC_TimeToRelBlockStart(time);

	if(btime>=peq->time2){
		next=NextFXNodeLine(peq->nextfxnodeline);

		if(next==NULL){
//			Pdebug("fx, slutt: %d\n",peq->nextfxnodeline->val);
			if(doit){
				fxhandle(peq->nextfxnodeline->val,peq,0);
			}
			ReturnPEQelement(peq);
		}else{
			peq->fxnodeline=peq->nextfxnodeline;
			peq->nextfxnodeline=next;
			peq->time1=peq->time2;
			peq->time2=Place2STime(peq->block,&next->l.p);
			PE_HandleFX(time,peq,doit);
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
		fxhandle(x,peq,1);
	}

	PC_InsertElement(peq,0,ntime);

	return;
}



















