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
#include "seqtrack_proc.h"
#include "seqtrack_automation_proc.h"
#include "instruments_proc.h"
#include "seqblock_envelope_proc.h"

#include "../api/api_proc.h"

#include "player_proc.h"


extern PlayerClass *pc;
extern struct Root *root;

extern LANGSPEC void OS_InitMidiTiming(void);



void PlayerTask(double reltime, bool can_not_start_playing_right_now_because_jack_transport_is_not_ready_yet, float max_audio_cycle_fraction){

        if (ATOMIC_GET(is_starting_up))
          return;


        
        static bool g_time_was_stopped = true; // If SCHEDULER_clear_all() always returned true, this variable didn't have to be static.


        
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

          if (SCHEDULER_clear_all(max_audio_cycle_fraction)) {

            ATOMIC_SET(pc->player_state, PLAYER_STATE_STOPPED);  // Finished. SCHEDULER_clear() cleared everything.
            //RT_BACKUP_reset_timer(); // Don't want to take backup right after stopping to play. It's quite annoying. (we handle this directly in Qt_AutoBackups instead)
            
            player_state = PLAYER_STATE_STOPPED;
            //printf("************ PlayerTask finished clearing. fraction: %f. Max fraction: %f\n", MIXER_get_curr_audio_block_cycle_fraction(), max_audio_cycle_fraction);

            RT_StopAllInstruments();
            
          } else {
#if !defined(RELEASE)
            printf("************ PlayerTask not finished clearing yet. fraction: %f. Max fraction: %f\n", MIXER_get_curr_audio_block_cycle_fraction(), max_audio_cycle_fraction);
#endif
            return; // Must run SCHEDULER_clear() at least one more time. We don't want clear too much at once since it could cause CPU spikes.
          }
          
          //} else if (player_state==PLAYER_STATE_STOPPED) {
          //  return;
        }

        R_ASSERT(player_state==PLAYER_STATE_STARTING_TO_PLAY || player_state==PLAYER_STATE_PLAYING || player_state==PLAYER_STATE_STOPPED);


        if (player_state==PLAYER_STATE_STARTING_TO_PLAY && can_not_start_playing_right_now_because_jack_transport_is_not_ready_yet)
          return;

        
        if (player_state != PLAYER_STATE_STOPPED)
          if(g_time_was_stopped){
            OS_InitMidiTiming();
            OS_InitAudioTiming();
            g_time_was_stopped = false;
          }


        bool is_finished = true;
        
        RT_SEQBLOCK_ENVELOPE_called_before_scheduler();
        
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

                if (pc->playtype==PLAYBLOCK && seqtrack==root->song->block_seqtrack)
                  set_player_time = true;

                else if (pc->playtype==PLAYSONG && seqtrack==root->song->seqtracks.elements[ATOMIC_GET(root->song->curr_seqtracknum)])
                  set_player_time = true;
                  
                if (set_player_time)
                  ATOMIC_DOUBLE_SET(block->player_time, seqtrack->start_time - curr_seqblock->t.time);
                //else ATOMIC_DOUBLE_SET(block->player_time, -100); // Not necessary (-100 is set in scheduler_seqtrack.c when switching block), and we also need to check if we are playing block, etc.
              }
              
            }
          
        }END_ALL_SEQTRACKS_FOR_EACH;
        
        RT_SEQBLOCK_ENVELOPE_called_after_scheduler_and_before_audio();        
        
        if (player_state==PLAYER_STATE_STOPPED) {
          RSEMAPHORE_signal_all(g_player_stopped_semaphore);
            
          RT_SEQTRACK_AUTOMATION_called_when_player_stopped();
          RT_SEQBLOCK_ENVELOPE_called_when_player_stopped();
          return;
        }

        pc->absabstime += RADIUM_BLOCKSIZE;
        
        if(pc->playtype==PLAYSONG) {
          double song_abstime = ATOMIC_DOUBLE_GET(pc->song_abstime);
          double new_song_abstime = song_abstime + reltime;
          ATOMIC_DOUBLE_SET(pc->song_abstime, new_song_abstime);

        }
        
#ifdef WITH_PD
        RT_PD_set_absolute_time(ATOMIC_DOUBLE_GET(pc->song_abstime));
#endif

        
        if (player_state == PLAYER_STATE_STARTING_TO_PLAY)
          ATOMIC_SET(pc->player_state, PLAYER_STATE_PLAYING);

        
        //printf("num_scheduled: %d. state: %d\n",num_scheduled_events,player_state);
        if(player_state == PLAYER_STATE_PLAYING && is_finished){

          ATOMIC_SET(pc->player_state, PLAYER_STATE_STOPPING);
          
          if(pc->playtype==PLAYSONG && useJackTransport() && !SEQUENCER_is_looping())
            MIXER_TRANSPORT_stop(); // end of song
          
        }

        if(pc->playtype==PLAYSONG){
          if (SEQUENCER_is_looping()){
            if (ATOMIC_DOUBLE_GET(pc->song_abstime) >= SEQUENCER_get_loop_end()){
              ATOMIC_SET(pc->player_state, PLAYER_STATE_STOPPING);
            }
          }
        }
}


STime g_last_seq_time_converted_to_delta_time;

int PLAYER_get_block_delta_time(struct SeqTrack *seqtrack, STime time){
  g_last_seq_time_converted_to_delta_time = time;
  
  if(time<seqtrack->start_time || time>seqtrack->end_time) // time may be screwed up if not coming from the player.
    return 0;

  if(is_playing()){
    //int ret = ((time - seqtrack->start_time) * pc->reltime / (seqtrack->end_time - seqtrack->start_time)); // i.e. "scale(time, seqtrack->start_time, seqtrack->end_time, 0, pc->reltime)"
    int ret = ((time - seqtrack->start_time) * RADIUM_BLOCKSIZE / (seqtrack->end_time - seqtrack->start_time)); // i.e. "scale(time, seqtrack->start_time, seqtrack->end_time, 0, RADIUM_BLOCKSIZE)"
    if(ret<0){
      RWarning("ret<0: %d",ret);
      return 0;
    }
    //if(ret>=pc->reltime){
    if(ret>=RADIUM_BLOCKSIZE){
      RWarning("ret>pc->reltime: %d > %d",ret,RADIUM_BLOCKSIZE);
      return (int)RADIUM_BLOCKSIZE-1;
    }
    return ret;
  }else
    return 0;
}

