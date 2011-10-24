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

#include "PEQvelocities_proc.h"



void PE_ChangeVelocityFromStart(STime time,struct PEventQueue *peq,int doit);
void PE_ChangeVelocity(STime time,struct PEventQueue *peq,int doit);
void PE_ChangeVelocityToEnd(STime time,struct PEventQueue *peq,int doit);
void PE_ChangeVelocityFromStartToEnd(STime time,struct PEventQueue *peq,int doit);

void InitPEQvelocities(
	struct Blocks *block,
	struct Tracks *track,
	struct Notes *note,
	int playlistaddpos
){
	int x;
	struct PEventQueue *peq;

	struct Velocities *velocity=note->velocities;

	if(velocity==NULL && note->velocity == note->velocity_end) return;

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
			PEQ_CalcNextEvent(
				peq->time1,
				peq->time1,
				peq->time2,
				note->velocity,
				&x,
				note->velocity_end
			)
		);
		return;
	}

	peq->time2=Place2STime(block,&velocity->l.p);
	peq->TreatMe=PE_ChangeVelocityFromStart;
	peq->velocity=velocity;

	PC_InsertElement(
		peq,playlistaddpos,
		PEQ_CalcNextEvent(
			peq->time1,
			peq->time1,
			peq->time2,
			note->velocity,
			&x,
			velocity->velocity
		)
	);

}


__inline void SendVelocityChange(int x,struct PEventQueue *peq){
	if(peq->track->patch!=NULL && peq->track->onoff==1){
		(*peq->track->patch->changevelocity)(
			x,
			peq->track,
			peq->note
		);
	}
}


void PE_ChangeVelocityFromStart(STime time,struct PEventQueue *peq,int doit){
	int x;
	STime ntime,btime;
	struct Velocities *next;

	btime=PC_TimeToRelBlockStart(time);

	if(btime>=peq->time2){
		next=NextVelocity(peq->velocity);
		peq->time1=peq->time2;
		if(next==NULL){
			peq->time2=Place2STime(peq->block,&peq->note->end);
			peq->TreatMe=PE_ChangeVelocityToEnd;
			PE_ChangeVelocityToEnd(time,peq,doit);
		}else{
			peq->nextvelocity=next;
			peq->time2=Place2STime(peq->block,&next->l.p);
			peq->TreatMe=PE_ChangeVelocity;
			PE_ChangeVelocity(time,peq,doit);
		}
		return;
	}

	ntime=PEQ_CalcNextEvent(
		peq->time1,
		btime,
		peq->time2,
		peq->note->velocity,
		&x,
		peq->velocity->velocity
	);

	if(btime==ntime){
		Pdebug("btime==ntime, stopper, x: %d, btime: %d, x1: %d, x2: %d\n",x,btime,peq->note->velocity,peq->velocity->velocity);
		return;
	}

	if(ntime>peq->time2) ntime=peq->time2;

//	Pdebug("start->vel,Velocity: %d, time: %d, ntime: %d, btime: %d, time1: %d, time2: %d\n",x,time,ntime,btime,peq->time1,peq->time2);
	if(doit){
		SendVelocityChange(x,peq);
	}

	PC_InsertElement(peq,0,ntime);

	return;
}



void PE_ChangeVelocity(STime time,struct PEventQueue *peq,int doit){
	int x;
	STime ntime,btime;
	struct Velocities *next;

	btime=PC_TimeToRelBlockStart(time);

	if(btime>=peq->time2){
		next=NextVelocity(peq->nextvelocity);
		peq->time1=peq->time2;
		peq->velocity=peq->nextvelocity;

		if(next==NULL){
			peq->time2=Place2STime(peq->block,&peq->note->end);
			peq->TreatMe=PE_ChangeVelocityToEnd;
			PE_ChangeVelocityToEnd(time,peq,doit);
		}else{
			peq->nextvelocity=next;
			peq->time2=Place2STime(peq->block,&next->l.p);
			PE_ChangeVelocity(time,peq,doit);
		}
		return;
	}

	ntime=PEQ_CalcNextEvent(
		peq->time1,
		btime,
		peq->time2,
		peq->velocity->velocity,
		&x,
		peq->nextvelocity->velocity
	);

	if(btime==ntime){
		Pdebug("Samme, stopper, x: %d\n",x);
		return;
	}

	if(ntime>peq->time2) ntime=peq->time2;

//	Pdebug("Player vel->vel, Velocity: %d, ntime: %d, btime: %d, time1: %d, time2: %d\n",x,ntime,btime,peq->time1,peq->time2);
	if(doit){
		SendVelocityChange(x,peq);
	}

	PC_InsertElement(peq,0,ntime);

	return;

}



void PE_ChangeVelocityToEnd(STime time,struct PEventQueue *peq,int doit){
	int x;
	STime ntime,btime;

	btime=PC_TimeToRelBlockStart(time);

	if(btime>=peq->time2){
		ReturnPEQelement(peq);
		return;
	}

	ntime=PEQ_CalcNextEvent(
		peq->time1,
		btime,
		peq->time2,
		peq->velocity->velocity,
		&x,
		peq->note->velocity_end+1		// Don't really know why I have to add 1, but it works...
	);

	if(ntime>=peq->time2){
		ReturnPEQelement(peq);
		return;
	}

//	Pdebug("Player vel->end, Velocity: %d, ntime: %d, btime: %d, time1: %d, time2: %d\n",x,ntime,btime,peq->time1,peq->time2);
	if(doit){
		SendVelocityChange(x,peq);
	}

	PC_InsertElement(peq,0,ntime);

	return;

}

void PE_ChangeVelocityFromStartToEnd(STime time,struct PEventQueue *peq,int doit){
	int x;
	STime ntime,btime;

	btime=PC_TimeToRelBlockStart(time);

	if(btime>=peq->time2){
		ReturnPEQelement(peq);
		return;
	}

	ntime=PEQ_CalcNextEvent(
		peq->time1,
		btime,
		peq->time2,
		peq->note->velocity,
		&x,
		peq->note->velocity_end+1		// Don't really know why I have to add 1, but it works...
	);

	if(ntime>=peq->time2){
		ReturnPEQelement(peq);
		return;
	}


//	Pdebug("Player start->end, Velocity: %d\n",x);
	if(doit){
		SendVelocityChange(x,peq);
	}

	PC_InsertElement(peq,0,ntime);

	return;
}


