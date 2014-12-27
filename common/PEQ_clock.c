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










/****************************************************************

  The clock that is showed at the upper left of the window
  when playing.

  The reason why 'UpdateClock' doesn't read
  the system-clock directly, is for the user to easier see
  if she/he is trying to send out note-datas and fx-datas
  faster than the instrument can handle.

  The clock will then be delayed and not updated each second,
  because the the instrument is in that situation supposed to use
  longer time than normal.

  Perhaps not the best way to show the situation, but
  it has to be showed someway. Note, in that situation, the
  clock should show nearly the correct time, from time
  to time.

****************************************************************/




#include "nsmtracker.h"
#include "playerclass.h"
#include "PEQmempool_proc.h"
#include "list_proc.h"

#include "OS_Ptask2Mtask_proc.h"


#ifdef _AMIGA
#include <proto/exec.h>					//Has to be removed!
#endif

#include "visual_proc.h"

#include "PEQ_clock_proc.h"

static STime clock_time;

////////////////////////////////////////////////////////////////
//             Player-task part.
////////////////////////////////////////////////////////////////

extern PlayerClass *pc;

void PC_NewSecond(struct PEventQueue *peq,int doit);

void InitPEQclock(void){
	struct PEventQueue *peq=GetPEQelement();

	peq->l.time=LATENCY;

	peq->TreatMe=PC_NewSecond;

	ListAddElementP(&pc->peq,&peq->l);
}

#ifdef _AMIGA
extern struct Task *mytask;
LONG clocksig;
#endif

//RSemaphore *ClockSemaphore;

void PC_NewSecond(struct PEventQueue *peq,int doit){

//	ObtainRSemaphore(ClockSemaphore);

//	clock_time=time;
	clock_time=pc->therealtime;

#ifdef _AMIGA
	Signal(mytask,1L<<clocksig);
#else
	Ptask2Mtask();
#endif




//	ReleaseRSemaphore(ClockSemaphore);

	peq->l.time+=pc->pfreq*(pc->block->reltempo>1.0f?1.0f:pc->block->reltempo);

	ListAddElementP(&pc->peq,&peq->l);
}




//////////////////////////////////////////////////////////////////
//            Main-task part.
//////////////////////////////////////////////////////////////////


void UpdateClock(
	struct Tracker_Windows *window
){
#if 0
	int clock_minutes,clock_seconds;
	char temp[52];
	int time=clock_time;
        static int last_time = -1;

	if( ! pc->isplaying){
          if(last_time==-2)
            return;
          GFX_UpdateQuantitize(window,window->wblock);
          last_time = -2;
          return;
	}

        if(last_time==time)
          return;

	clock_minutes=time/(60*pc->pfreq);
	clock_seconds=time/pc->pfreq - (60*clock_minutes);

//	ObtainRSemaphore(ClockSemaphore);

	sprintf(temp,"%.02d:%.02d",clock_minutes,clock_seconds);

	GFX_Text(
		 window,2,temp,0,0,TEXT_IGNORE_WIDTH,TEXT_CLEAR,PAINT_DIRECTLY
	);

        last_time = time;

//	ReleaseRSemaphore(ClockSemaphore);

#endif
}


bool InitClock(void){
#ifdef _AMIGA
	clocksig=AllocSignal(-1);
	if(clocksig==-1){
		RError("Could not allocate signal\n");
		return false;
	}
#endif

//	ClockSemaphore=NewRSemaphore();
//	if(ClockSemaphore==NULL) return false;

	return true;
}

void CloseClock(void){
#ifdef _AMIGA
	FreeSignal(clocksig);
#endif
//	FreeRSemaphore(ClockSemaphore);
}


