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



/************* OVERVIEW ********************************
  This is the most important source-file. This is where
  the main-loop for the player is.
*******************************************************/



#include "nsmtracker.h"
#include "playerclass.h"
#include "PEQcommon_proc.h"
#include "time2place_proc.h"
#include "PEQ_type_proc.h"

#include "player_proc.h"



extern PlayerClass *pc;

STime dastime=0;
static STime addreltime=0;

extern LANGSPEC void OS_InitMidiTiming();

void PlayerTask(STime reltime){

	struct PEventQueue *peq;
	STime time;
	STime newreltime;

	if( ! pc->isplaying){
		if( ! pc->initplaying) PC_ReturnElements();
		return;
	}
	addreltime+=reltime;
	newreltime=addreltime*pc->block->reltempo;

	if(dastime==0){
		pc->therealtime=reltime;
                OS_InitMidiTiming();
	}else{
		pc->therealtime+=reltime;
	}

	if(newreltime<1)
          return;

        {
          int reltime_to_add_now = addreltime - newreltime;
          if(dastime==0)
            pc->reltime_to_add = reltime_to_add_now;
          else
            pc->reltime_to_add += reltime_to_add_now;
#if 0
          printf("pc->realtime_to_add: %d (%f), now: %d (%f). Actual time: %f\n",
                 pc->reltime_to_add,pc->reltime_to_add/(double)PFREQ,
                 reltime_to_add_now,reltime_to_add_now/(double)PFREQ,
                 (dastime+newreltime+pc->reltime_to_add)/(double)PFREQ
                 );
#endif
        }

	dastime+=newreltime;


	addreltime=0;

	time=dastime;


	peq=pc->peq;


// Main-loop for player.

	while(
		peq!=NULL
		&& time>=peq->l.time
		&& peq->l.time<time+(PFREQ*2)  // Dont want to run for more than two seconds.
		&& pc->isplaying // Dont remove this check.
	){
	  //	  printf("time: %d, peq->l.time: %d\n",time,peq->l.time);
		PC_RemoveFirst();
		(*peq->TreatMe)(time,peq,1);
		pc->pausetime=peq->l.time;
		peq=pc->peq;
	}

}




