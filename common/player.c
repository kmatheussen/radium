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
//#include "PEQcommon_proc.h"
//#include "PEQ_type_proc.h"
#include "../audio/Mixer_proc.h"
#include "../audio/Pd_plugin_proc.h"
#include "../Qt/Qt_AutoBackups_proc.h"
#include "scheduler_proc.h"

#include "player_proc.h"

extern PlayerClass *pc;
extern struct Root *root;

extern LANGSPEC void OS_InitMidiTiming(void);

static bool g_time_was_stopped = true;


void PlayerTask(STime reltime){

  
        if (ATOMIC_GET(is_starting_up))
          return;


        
        pc->reltime     = reltime;


        
        Player_State player_state = ATOMIC_GET(pc->player_state);


        
        if (player_state==PLAYER_STATE_PROGRAM_NOT_READY){
          //printf("player: program not ready\n");
          return;

        } else if (player_state==PLAYER_STATE_ENDING) {
          return;

        } else if (player_state==PLAYER_STATE_STOPPING) {
          //PC_ReturnElements();

          g_time_was_stopped = true;

          SCHEDULER_reset_all_timing();

          if (SCHEDULER_clear_all()) {
            ATOMIC_SET(pc->player_state, PLAYER_STATE_STOPPED);  // Finished. SCHEDULER_clear() cleared everything.
            //RT_BACKUP_reset_timer(); // Don't want to take backup right after stopping to play. It's quite annoying. (we handle this directly in Qt_AutoBackups instead)
            
            player_state = PLAYER_STATE_STOPPED;
            
          } else            
            return; // Must run SCHEDULER_clear() at least one more time. We don't want clear too much at once since it could cause CPU spikes.
          
          //} else if (player_state==PLAYER_STATE_STOPPED) {
          //  return;
        }

        
        
        R_ASSERT(player_state==PLAYER_STATE_STARTING_TO_PLAY || player_state==PLAYER_STATE_PLAYING || player_state==PLAYER_STATE_STOPPED);

        
        if (player_state != PLAYER_STATE_STOPPED)
          if(g_time_was_stopped){
            OS_InitMidiTiming();
            OS_InitAudioTiming();
            g_time_was_stopped = false;
          }



        bool is_finished = true;
        

        
        ALL_SEQTRACKS_FOR_EACH(){

            double reltempo = 1.0;

            struct SeqBlock *curr_seqblock = seqtrack->curr_seqblock;
            struct Blocks *block = curr_seqblock==NULL ? NULL : curr_seqblock->block;
            
            if(block!=NULL)
              reltempo = ATOMIC_DOUBLE_GET(block->reltempo);
            
            double seqreltime  = (double)reltime * reltempo;

            //if(reltempo!=1.0)
            //  printf("Curr_seqblock: %p. seqrelteim: %f\n", curr_seqblock,seqreltime);
                   
            pc->is_treating_editor_events = true; {
              
              if (SCHEDULER_called_per_block(seqtrack, seqreltime) > 0)
                is_finished = false;
              
            } pc->is_treating_editor_events = false;
          

            if (player_state != PLAYER_STATE_STOPPED){

              if (curr_seqblock != NULL) {
                
                bool set_player_time = false;

                if (pc->playtype==PLAYBLOCK && seqtrack==&root->song->block_seqtrack)
                  set_player_time = true;

                else if (pc->playtype==PLAYSONG && seqtrack==root->song->seqtracks.elements[root->song->curr_seqtracknum])
                  set_player_time = true;
                  
                if (set_player_time)
                  ATOMIC_DOUBLE_SET(block->player_time, seqtrack->start_time - curr_seqblock->time);
                //else ATOMIC_DOUBLE_SET(block->player_time, -100); // Not necessary (-100 is set in scheduler_seqtrack.c when switching block), and we also need to check if we are playing block, etc.
              }
              
            }
          
        }END_ALL_SEQTRACKS_FOR_EACH;

        
        
        if (player_state==PLAYER_STATE_STOPPED)
          return;        


        
        if(pc->playtype==PLAYSONG)
          ATOMIC_ADD(pc->song_abstime, reltime);

#ifdef WITH_PD
        RT_PD_set_absolute_time(ATOMIC_DOUBLE_GET(pc->song_abstime));
#endif

        
        if (player_state == PLAYER_STATE_STARTING_TO_PLAY)
          ATOMIC_SET(pc->player_state, PLAYER_STATE_PLAYING);

        
        //printf("num_scheduled: %d. state: %d\n",num_scheduled_events,player_state);
        if(player_state == PLAYER_STATE_PLAYING && is_finished)
          ATOMIC_SET(pc->player_state, PLAYER_STATE_STOPPING);
}


STime g_last_seq_time_converted_to_delta_time;

int PLAYER_get_block_delta_time(struct SeqTrack *seqtrack, STime time){
  g_last_seq_time_converted_to_delta_time = time;
  
  if(time<seqtrack->start_time || time>seqtrack->end_time) // time may be screwed up if not coming from the player.
    return 0;

  if(is_playing()){
    int ret = ((time - seqtrack->start_time) * pc->reltime / (seqtrack->end_time - seqtrack->start_time)); // i.e. "scale(time, seqtrack->start_time, seqtrack->end_time, 0, pc->reltime)"
    if(ret<0){
      RWarning("ret<0: %d",ret);
      return 0;
    }
    if(ret>=pc->reltime){
      RWarning("ret>pc->reltime: %d > %d",ret,pc->reltime);
      return (int)pc->reltime-1;
    }
    return ret;
  }else
    return 0;
}

