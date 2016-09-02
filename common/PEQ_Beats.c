/* Copyright 2000-2015 Kjetil S. Matheussen

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

#include <string.h>
#include <math.h>

#include "nsmtracker.h"
#include "playerclass.h"
#include "time_proc.h"
#include "PEQmempool_proc.h"
#include "PEQcommon_proc.h"
#include "placement_proc.h"
#include "scheduler_proc.h"
#include "../audio/Mixer_proc.h"
#include "patch_proc.h"
#include "PEQ_LPB_proc.h"

#include "PEQ_Beats_proc.h"


extern PlayerClass *pc;

static struct Beats *g_beat = NULL;
static Ratio g_last_valid_signature = {4,4};

static int g_last_played_note_num = -1;

static const int bar_note_num = 55;
static const int beat_note_num = 50;

static void stop_note(int64_t time, int note_num){
  int num_patches = 0;
  struct Patch **patches = RT_MIXER_get_all_click_patches(&num_patches);
  int i;
  for (i=0 ; i<num_patches ; i++){
    RT_PATCH_stop_note(patches[i],
                       create_note_t2(-1, g_last_played_note_num),
                       time);
  }
}

static void stop_last_played_note(int64_t time){
  if (g_last_played_note_num != -1) {
    stop_note(time, g_last_played_note_num);
    g_last_played_note_num = -1;
  }
}

static void play_note(int64_t time, int note_num){
  int num_patches = 0;
  struct Patch **patches = RT_MIXER_get_all_click_patches(&num_patches);
  int i;
  for (i=0 ; i<num_patches ; i++){
    RT_PATCH_play_note(patches[i],
                       create_note_t(-1,
                                     note_num,
                                     1.0,
                                     0.0,
                                     0),
                       time);
  }
  g_last_played_note_num = note_num;
}

static void scheduled_play_bar_note(int64_t time, const union SuperType *args){
  stop_last_played_note(time);
  //printf("** BAR\n");
  if (ATOMIC_GET(root->clickonoff))
    play_note(time, bar_note_num);
}

static void scheduled_play_beat_note(int64_t time, const union SuperType *args){
  stop_last_played_note(time);
  //printf("     BEAT **\n");
  if (ATOMIC_GET(root->clickonoff))
    play_note(time, beat_note_num);
}

double g_beat_position_of_last_bar_start = 0.0;

static bool new_beat_bar_set = false;

// Called from RT_LPB_set_beat_position, which is called from Mixer.cpp after events are calculated, and before audio is created.
void RT_PEQ_Beats_set_new_last_bar_start_value(double beat_position, bool just_started_playing){

  if (just_started_playing) {

    double num_beats_in_bar = 4 * (double)g_last_valid_signature.numerator / (double) g_last_valid_signature.denominator; // Convert to ppq

    // If we start playing in the middle of a block, spool back to closest position that could have been the beat start.
    beat_position = num_beats_in_bar * floor(beat_position/num_beats_in_bar);
                        
    g_beat_position_of_last_bar_start = beat_position;
    //printf("   Setting new bar start to %f. num_beats: %f  (sign: %d/%d)\n", g_beat_position_of_last_bar_start, num_beats_in_bar, g_beat->valid_signature.numerator, g_beat->valid_signature.denominator);
    
  } else if (new_beat_bar_set) {
    
    g_beat_position_of_last_bar_start = beat_position;
    
  }
  
  new_beat_bar_set = false;
}

static void handle_new_beat(struct PEventQueue *peq, int doit, struct Beats *beat){
  if (doit==0) // Is doit used anymore?
    return;

  g_last_valid_signature = beat->valid_signature;
#if !defined(RELEASE)
  if (g_last_valid_signature.denominator==0)
    abort();
#endif
  
  if (beat->beat_num==1)
    new_beat_bar_set = true;
  //g_beat_position_of_last_bar_start = RT_LPB_get_beat_position();
  
  //printf("%d %d. last bar: %f. signature: %d/%d\n", beat->bar_num, beat->beat_num, g_beat_position_of_last_bar_start,g_last_valid_signature.numerator, g_last_valid_signature.denominator);

  if (beat->beat_num==1)
    SCHEDULER_add_event(peq->l.time, scheduled_play_bar_note, NULL, 0, SCHEDULER_NOTE_ON_PRIORITY);
  else
    SCHEDULER_add_event(peq->l.time, scheduled_play_beat_note, NULL, 0, SCHEDULER_NOTE_ON_PRIORITY);
}

static void InitPEQ_Beat_new_block(const struct Blocks *block){
  R_ASSERT_RETURN_IF_FALSE(block->beats!=NULL);
  g_beat = block->beats;
  //handle_new_beat(g_beat); Not necessary. (and probably wrong too)
}


static void PlayerNextBeat_Block(struct PEventQueue *peq,int doit);
static void PlayerNextBeat(struct PEventQueue *peq, int doit);


static void InsertNextBeat_PEQ(struct PEventQueue *peq){
  if (g_beat==NULL) {

    peq->TreatMe = PlayerNextBeat_Block;
    peq->block   = PC_GetPlayBlock(1);
    
    if (peq->block == NULL)
      ReturnPEQelement(peq);
    
    else {
      
      PC_InsertElement_a( // need to use the "_a" version so that the block has a chance to update first. (there should be an "_aa" version for the block. Now fx/etc. can be called before beat block change)
                         peq,
                         1,
                         0
                          );
    }
    
  } else {

    peq->TreatMe=PlayerNextBeat;
    
    PC_InsertElement2(
                     peq,
                     0,
                     &g_beat->l.p
                     );

  }
}

void InitPEQ_Beat(struct Blocks *block,Place *place){
  g_last_played_note_num = -1;

  g_beat_position_of_last_bar_start = 0.0; // doesn't matter.
  
  
  // Here: Find all Sample Player / Click and store them globally.
  // (hmm, patches can be added/deleted while playing)

  InitPEQ_Beat_new_block(block);
    
  struct PEventQueue *peq = GetPEQelement();
  
  peq->TreatMe=PlayerNextBeat;
  peq->block=block;

  InsertNextBeat_PEQ(peq);
}

static void PlayerNextBeat_Block(struct PEventQueue *peq,int doit){
  //printf("PlayerNextBlock %d (%d)\n",(int)peq->l.time,(int)pc->start_time);
  InitPEQ_Beat_new_block(peq->block);
  
  InsertNextBeat_PEQ(peq);
}

static void PlayerNextBeat(struct PEventQueue *peq,int doit){
  R_ASSERT_RETURN_IF_FALSE(g_beat != NULL);
  
  handle_new_beat(peq, doit, g_beat);
  
  g_beat = NextBeat(g_beat);  
  
  InsertNextBeat_PEQ(peq);
}

