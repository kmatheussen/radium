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
#include "PEQ_type_proc.h"
#include "../audio/Mixer_proc.h"
#include "../audio/Pd_plugin_proc.h"
#include "scheduler_proc.h"

#include "player_proc.h"

extern PlayerClass *pc;
extern struct Root *root;

extern LANGSPEC void OS_InitMidiTiming(void);

void PlayerTask(STime reltime){
        if (ATOMIC_GET(is_starting_up))
          return;


        Player_State player_state = ATOMIC_GET(pc->player_state);

        if (player_state==PLAYER_STATE_PROGRAM_NOT_READY){
          printf("player: program not ready\n");
          return;

        } else if (player_state==PLAYER_STATE_ENDING) {
          return;

        } else if (player_state==PLAYER_STATE_STOPPING) {
          PC_ReturnElements();
          SCHEDULER_clear();

          pc->end_time=0;
          pc->end_time_f=0;

          ATOMIC_SET(pc->player_state, PLAYER_STATE_STOPPED);
          return;
          
          //} else if (player_state==PLAYER_STATE_STOPPED) {
          //  return;
        }

        
        R_ASSERT(player_state==PLAYER_STATE_STARTING_TO_PLAY || player_state==PLAYER_STATE_PLAYING || player_state==PLAYER_STATE_STOPPED);
        
        
	static STime addreltime=0;
        //RError("hepp");
        pc->reltime     = reltime;
                   
        double reltempo = 1.0;

        struct Blocks *block = pc->block;          
        if(block!=NULL)
          reltempo = safe_volatile_float_read(&block->reltempo);

	addreltime+=reltime;

        double tempoadjusted_reltime_f = (double)addreltime * reltempo;
        STime tempoadjusted_reltime    = tempoadjusted_reltime_f;
        
        if(tempoadjusted_reltime<1) {
          pc->start_time_f += (double)reltime * reltempo;
          return;
        } else
          addreltime=0;

        if (player_state==PLAYER_STATE_STOPPED){
          SCHEDULER_called_per_block(tempoadjusted_reltime);
          return;
        }
        
        if(pc->end_time==0){
          pc->therealtime=reltime;
          OS_InitMidiTiming();
          OS_InitAudioTiming();
        }else{
          pc->therealtime+=reltime;
        }

#if 0
        // This debug print is helpful to understand the timing.
        printf("pc->realtime_to_add: %d (%f), now: %d (%f). Actual time: %f\n",
               (int)pc->reltime_to_add,pc->reltime_to_add/(double)pc->pfreq,
               reltime_to_add_now,reltime_to_add_now/(double)pc->pfreq,
               (pc->end_time+tempoadjusted_reltime+pc->reltime_to_add)/(double)pc->pfreq
               );
        fflush(stdout);
#endif
          
        pc->start_time  = pc->end_time;
        pc->end_time   += tempoadjusted_reltime;

        //printf("Setting new starttime to %f (%d)\n",pc->end_time_f,(int)pc->end_time);
        safe_double_write(&pc->start_time_f, pc->end_time_f);
        pc->end_time_f  += tempoadjusted_reltime_f;
        
#ifdef WITH_PD
        RT_PD_set_absolute_time(pc->start_time);
#endif
        
        //printf("time: %d. time of next event: %d\n",(int)time,(int)pc->peq->l.time);
        //fflush(stdout);

        pc->is_treating_editor_events = true; {

          struct PEventQueue *peq = pc->peq;
          
          while(
                peq!=NULL
                && peq->l.time < pc->end_time
                )
            {
              
              //printf("time: %d, peq->l.time: %d\n",(int)time,(int)peq->l.time);
              //fflush(stdout);
              PC_RemoveFirst();
              (*peq->TreatMe)(peq,1);
              pc->pausetime=peq->l.time;
              peq=pc->peq;
            }

          // Currently, there are two scheduling systems. The old linked list (PEQ), and this one.
          // This one, the SCHEDULER, is a priority queue. The plan is to shift things from PEQ into SCHEDULER.
          // Until everything is shifted from PEQ to SCHEDULER, and the PEQ-mess remains, things will be more complicated than necessary.
          SCHEDULER_called_per_block(tempoadjusted_reltime);

        } pc->is_treating_editor_events = false;


        if (player_state == PLAYER_STATE_STARTING_TO_PLAY)
          ATOMIC_SET(pc->player_state, PLAYER_STATE_PLAYING);
}

STime PLAYER_get_block_delta_time(STime time){
  if(time<pc->start_time || time>pc->end_time) // time may be screwed up if not coming from the player.
    return 0;

  if(is_playing()){
    STime ret = ((time - pc->start_time) * pc->reltime / (pc->end_time - pc->start_time)); // i.e. "scale(time, pc->start_time, pc->end_time, 0, pc->reltime)"
    if(ret<0){
      RWarning("ret<0: %d",ret);
      return 0;
    }
    if(ret>pc->reltime){
      RWarning("ret>pc->reltime: %d > %d",ret,pc->reltime);
      return 0;
    }
    return ret;
  }else
    return 0;
}

