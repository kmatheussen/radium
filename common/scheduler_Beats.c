
#include <math.h>

#include "nsmtracker.h"
#include "patch_proc.h"
#include "placement_proc.h"

#include "../audio/Mixer_proc.h"


#include "scheduler_proc.h"



// Called from RT_LPB_set_beat_position, which is called from Mixer.cpp after events are calculated, and before audio is created.
void RT_Beats_set_new_last_bar_start_value(struct SeqTrack *seqtrack, double beat_position, bool just_started_playing){
  Beat_Iterator *iterator = &seqtrack->beat_iterator;
  
  if (just_started_playing) {

    double num_beats_in_bar = 4 * (double)iterator->last_valid_signature.numerator / (double) iterator->last_valid_signature.denominator; // Convert to ppq

    // If we start playing in the middle of a block, spool back to closest position that could have been the beat start.
    beat_position = num_beats_in_bar * floor(beat_position/num_beats_in_bar);
    
    iterator->beat_position_of_last_bar_start = beat_position;
    //printf("   Setting new bar start to %f. num_beats: %f  (sign: %d/%d)\n", g_beat_position_of_last_bar_start, num_beats_in_bar, g_beat->valid_signature.numerator, g_beat->valid_signature.denominator);
    
  } else if (iterator->new_beat_bar_set) {
    
    iterator->beat_position_of_last_bar_start = beat_position;
    
  }
  
  iterator->new_beat_bar_set = false;
}

static void RT_stop_last_played_note(struct SeqTrack *seqtrack, Beat_Iterator *iterator, int64_t time){
  if (iterator->last_played_metronome_note_num != -1) {
    RT_stop_click_note(seqtrack, time, iterator->last_played_metronome_note_num);
    iterator->last_played_metronome_note_num = -1;
  }
}

static void RT_play_click_note2(struct SeqTrack *seqtrack, Beat_Iterator *iterator, int64_t time, int note_num){
  RT_play_click_note(seqtrack, time, note_num);
  iterator->last_played_metronome_note_num = note_num;
}

static int64_t RT_scheduled_play_bar_note(struct SeqTrack *seqtrack, int64_t time, union SuperType *args){
  Beat_Iterator *iterator = args[0].pointer;
    
  RT_stop_last_played_note(seqtrack, iterator, time);
  //printf("** BAR\n");
  if (ATOMIC_GET(root->clickonoff))
    RT_play_click_note2(seqtrack, iterator, time, c_bar_note_num);

  return DONT_RESCHEDULE;
}

static int64_t RT_scheduled_play_beat_note(struct SeqTrack *seqtrack, int64_t time, union SuperType *args){
  Beat_Iterator *iterator = args[0].pointer;
    
  RT_stop_last_played_note(seqtrack, iterator, time);
  //printf("     BEAT **\n");
  if (ATOMIC_GET(root->clickonoff))
    RT_play_click_note2(seqtrack, iterator, time, c_beat_note_num);

  return DONT_RESCHEDULE;
}


static int64_t RT_scheduled_Beat(struct SeqTrack *seqtrack, int64_t time, union SuperType *args){
  
  const struct SeqBlock *seqblock = args[0].const_pointer;

  Beat_Iterator *iterator = &seqtrack->beat_iterator;
  R_ASSERT_RETURN_IF_FALSE2(iterator->is_active==true, DONT_RESCHEDULE); // Gotten one hit from this assertion via the crash reporter (linux, not custom build)). Don't know how that is possible.

  if(iterator->play_id != ATOMIC_GET(pc->play_id)){
    R_ASSERT(false);
    goto stop_scheduling;
  }

  {
    const struct Beats *beat = iterator->next_beat;
    if (beat==NULL){
      R_ASSERT(false);
      goto stop_scheduling;
    }
  
    iterator->last_valid_signature = beat->valid_signature;
#if !defined(RELEASE)
    if (iterator->last_valid_signature.denominator<=0)
      abort();
#endif
  
    if (beat->beat_num==1)
      iterator->new_beat_bar_set = true;
    //iterator->beat_position_of_last_bar_start = RT_LPB_get_beat_position();
  
    //printf("%d %d. last bar: %f. signature: %d/%d\n", beat->bar_num, beat->beat_num, iterator->beat_position_of_last_bar_start,iterator->last_valid_signature.numerator, iterator->last_valid_signature.denominator);

    bool schedule_metronome;
    if (root->song->use_sequencer_tempos_and_signatures)
      schedule_metronome = false;
    else if (is_playing_song())
      schedule_metronome = seqtrack==root->song->seqtracks.elements[0];
    else
      schedule_metronome = seqtrack==root->song->block_seqtrack;
    
    // Schedule metronome sound
    if(schedule_metronome){
      const int num_args = 1;
        
      union SuperType args[num_args];
      args[0].pointer = iterator;

      if (beat->beat_num==1)
        SCHEDULER_add_event(seqtrack, time, RT_scheduled_play_bar_note, &args[0], num_args, SCHEDULER_NOTE_ON_PRIORITY);
      else
        SCHEDULER_add_event(seqtrack, time, RT_scheduled_play_beat_note, &args[0], num_args, SCHEDULER_NOTE_ON_PRIORITY);
    }

    iterator->next_beat = NextBeat(beat);
  }
  
  // Schedule next beat
  if (iterator->next_beat != NULL)
    return get_seqblock_place_time(seqblock, iterator->next_beat->l.p);

 stop_scheduling:
  R_ASSERT(iterator->is_active==true);
  iterator->is_active = false;
  return DONT_RESCHEDULE;
}


void RT_schedule_Beats_newblock(struct SeqTrack *seqtrack,
                                const struct SeqBlock *seqblock,
                                const Place start_place)
{
  R_ASSERT_RETURN_IF_FALSE(seqblock->block != NULL);
  
  Beat_Iterator *iterator = &seqtrack->beat_iterator;
  memset(iterator, 0, sizeof(Beat_Iterator));
  iterator->last_played_metronome_note_num = -1;

  R_ASSERT(iterator->is_active==false);

  iterator->play_id = ATOMIC_GET(pc->play_id);

  const struct Blocks *block = seqblock->block;
    
  const struct Beats *next_beat = block->beats;

  R_ASSERT_RETURN_IF_FALSE(next_beat!=NULL);

  iterator->last_valid_signature = next_beat->valid_signature;
      
  // spool forward to the 'beat' that is used by 'start_place', and find iterator->last_valid_signature
  //
  while(PlaceGreaterThan(&start_place, &next_beat->l.p)){
    iterator->last_valid_signature = next_beat->valid_signature;
    next_beat = NextBeat(next_beat);
    if (next_beat==NULL)
      return;
  }

  iterator->next_beat = next_beat;
  
  {
    int num_args = 1;
    union SuperType args[num_args];
    args[0].const_pointer = seqblock;
    
    int64_t time = get_seqblock_place_time(seqblock, next_beat->l.p);

    R_ASSERT(iterator->is_active==false);
    iterator->is_active = true;
    SCHEDULER_add_event(seqtrack, time, RT_scheduled_Beat, &args[0], num_args, SCHEDULER_BEAT_PRIORITY);
  }
}
