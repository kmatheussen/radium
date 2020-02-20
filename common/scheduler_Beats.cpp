
#include <math.h>

#include "nsmtracker.h"
#include "patch_proc.h"
#include "placement_proc.h"
#include "list_proc.h"
#include "sequencer_timing_proc.h"
#include "OS_visual_input.h"

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

static int64_t RT_scheduled_play_bar_note(struct SeqTrack *seqtrack, int64_t time, union SuperType *args){
  //printf("** BAR\n");
  if (ATOMIC_GET(root->clickonoff))
    RT_play_click_note(seqtrack, time, c_bar_note_num);

  return DONT_RESCHEDULE;
}

static int64_t RT_scheduled_play_beat_note(struct SeqTrack *seqtrack, int64_t time, union SuperType *args){
  //printf("     BEAT **\n");
  if (ATOMIC_GET(root->clickonoff))
    RT_play_click_note(seqtrack, time, c_beat_note_num);

  return DONT_RESCHEDULE;
}

static bool is_timing_seqtrack(struct SeqTrack *seqtrack){
  if (is_playing_song()){
    if (root->song->use_sequencer_tempos_and_signatures)
      return false;
    else 
      return seqtrack==root->song->seqtracks.elements[0];
  } else {
    return seqtrack==root->song->block_seqtrack;
  }
}
  
#if 1
static int64_t RT_scheduled_play_click_between_seqblocks(struct SeqTrack *seqtrack, int64_t time, union SuperType *args){
  int64_t end_time = args[0].int_num;

  //int64_t bar_seqlength = ...;
  int64_t beat_seqlength = args[1].int_num;
  
  int signature_numerator = args[2].int32_num;
  //int signature_denominator = args[3].int32_num;

  int &barnum = args[3].int32_num;
  int &beatnum = args[4].int32_num;
  
  // 1. Set g_rt_beatnum/g_rt_barnum and play click.
  //
  
  bool is_bar = beatnum==1;

  g_rt_beatnum = beatnum;
  g_rt_barnum = barnum;
    
  if (ATOMIC_GET(root->clickonoff)){
    RT_play_click_note(seqtrack, time, is_bar ? c_bar_note_num : c_beat_note_num);
  }
  
  // 2. Prepare next
  //
  
  if (beatnum==signature_numerator){
    beatnum = 1;
    barnum++;
  } else {
    beatnum++;
  }

  int64_t next_time = time + beat_seqlength;

  if (end_time > 0 && next_time+20 >= end_time)
    return DONT_RESCHEDULE;
  
  return next_time;
}

static int64_t get_last_click_seqtime(void){
  int64_t ret = -1;
  
  VECTOR_FOR_EACH(const struct SeqTrack *, seqtrack, &root->song->seqtracks){
    
    int num_seqblocks = seqtrack->seqblocks.num_elements;
    
    if (num_seqblocks > 0){      
      const struct SeqBlock *last_seqblock = static_cast<const struct SeqBlock *>(seqtrack->seqblocks.elements[num_seqblocks-1]);
      ret = R_MAX(ret, last_seqblock->t.time2);
    }
    
  }END_VECTOR_FOR_EACH;

  if (ATOMIC_GET_RELAXED(root->song->looping.enabled))
    ret = R_MAX(ATOMIC_GET_RELAXED(root->song->looping.end), ret);

  if (ATOMIC_GET_RELAXED(root->song->punching.enabled))
    ret = R_MAX(ATOMIC_GET_RELAXED(root->song->punching.end), ret);
      
  return ret;
}

static void RT_schedule_beats_between_seqblocks2(struct SeqTrack *seqtrack, int64_t time,
                                                 const struct SeqBlock *seqblock1, const struct SeqBlock *seqblock2,
                                                 const StaticRatio &signature,
                                                 int64_t last_beatseqtime,
                                                 int barnum, int beatnum)
{
  R_ASSERT_RETURN_IF_FALSE(seqblock1 != NULL);
  R_ASSERT_NON_RELEASE(seqblock1->block != NULL);
  
  //struct SeqBlock *block1 = seqblock1->block;
  
  int64_t end_blockseqtime1 = seqblock1->t.time2;
  int64_t end_time = seqblock2==NULL ? get_last_click_seqtime() : seqblock2->t.time;

  //int64_t bar_seqlength = end_blockseqtime1 - last_barseqtime;
  int64_t beat_seqlength = end_blockseqtime1 - last_beatseqtime;

  /*
  if (bar_seqlength <= 0){
    return;
  }
  */
  
  if (beat_seqlength <= 0){
    return;
  }

  if (end_time > 0 && time+20 >= end_time)
    return;
  
  {
    int num_args = 5;
    union SuperType args[num_args];
    args[0].int_num = end_time;
    args[1].int_num = beat_seqlength;
    args[2].int32_num = signature.numerator;
    //args[3].int32_num = signature.denominator;    
    args[3].int32_num = barnum;
    args[4].int32_num = beatnum;
    
    SCHEDULER_add_event(seqtrack, time, RT_scheduled_play_click_between_seqblocks, &args[0], num_args, SCHEDULER_NOTE_ON_PRIORITY);
  }
}
#endif

void RT_schedule_beats_between_seqblocks(struct SeqTrack *seqtrack,
                                         int64_t now_time,
                                         const struct SeqBlock *seqblock1,
                                         const struct SeqBlock *seqblock2)
{
  R_ASSERT_RETURN_IF_FALSE(seqblock1!=NULL);
  
  const struct Blocks *block = seqblock1->block;
  R_ASSERT_RETURN_IF_FALSE(block!=NULL);
  R_ASSERT_RETURN_IF_FALSE(block->beats!=NULL);

  int64_t time = seqblock1->t.time2;

  const struct Beats *last_beat = static_cast<const struct Beats*>(ListLast3(const_cast<ListHeader3*>(&block->beats->l)));
  R_ASSERT_RETURN_IF_FALSE(last_beat!=NULL);

  int64_t last_beatseqtime = get_seqblock_place_time(seqblock1, last_beat->l.p, PLUGINS_AND_JACK_TRANSPORT_SWINGING_MODE);

  int64_t beat_seqlength = time - last_beatseqtime;
  
  if (beat_seqlength <= 0){
    R_ASSERT_NON_RELEASE(false);
    return;
  }

  int barnum = 1;
  int beatnum = 1;
  
  // Fix: This loop is not necessary. (time can be calculated directly)
  for(int i = 0 ; ; i++){

    if (time >= now_time)
      break;
    
    if (i==1000){ // Avoid using too much time in the player thread.
      RT_message("Could not start metronome");
      return;
    }

    time += beat_seqlength;
    
    beatnum++;
    if (beatnum==last_beat->valid_signature.numerator+1){
      beatnum=1;
      barnum++;
    }
  }

  RT_schedule_beats_between_seqblocks2(seqtrack, time,
                                       seqblock1, seqblock2,
                                       last_beat->valid_signature,
                                       last_beatseqtime,
                                       barnum, beatnum);
}

static int64_t RT_scheduled_Beat(struct SeqTrack *seqtrack, int64_t time, union SuperType *args){
  
  const struct SeqBlock *seqblock = static_cast<const struct SeqBlock *>(args[0].const_pointer);
  const struct SeqBlock *next_seqblock = static_cast<const struct SeqBlock *>(args[1].const_pointer);

  Beat_Iterator *iterator = &seqtrack->beat_iterator;
  R_ASSERT_RETURN_IF_FALSE2(iterator->is_active==true, DONT_RESCHEDULE); // Gotten one hit from this assertion via the crash reporter (linux, not custom build)). Don't know how that is possible.

  if(iterator->play_id != ATOMIC_GET(pc->play_id)){
    R_ASSERT(false);
    goto stop_scheduling;
  }

      
  {
    bool is_a_timing_seqtrack = is_timing_seqtrack(seqtrack);
    
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

    // Schedule metronome sound
    if(is_a_timing_seqtrack){

      g_rt_beatnum = beat->beat_num;
      g_rt_barnum = beat->bar_num;

      const int num_args = 1;
        
      union SuperType args[num_args];
      args[0].pointer = iterator;

      if (beat->beat_num==1)
        SCHEDULER_add_event(seqtrack, time, RT_scheduled_play_bar_note, &args[0], num_args, SCHEDULER_NOTE_ON_PRIORITY);
      else
        SCHEDULER_add_event(seqtrack, time, RT_scheduled_play_beat_note, &args[0], num_args, SCHEDULER_NOTE_ON_PRIORITY);
    }

    iterator->next_beat = NextBeat(beat);


    // Schedule next beat
    if (iterator->next_beat != NULL)
      return get_seqblock_place_time(seqblock, iterator->next_beat->l.p, PLUGINS_AND_JACK_TRANSPORT_SWINGING_MODE);
    
#if 0
    (void)next_seqblock;
    (void)RT_schedule_beats_between_seqblocks;
#else
    if (is_a_timing_seqtrack && seqtrack != root->song->block_seqtrack)
      RT_schedule_beats_between_seqblocks2(seqtrack, seqblock->t.time2, seqblock, next_seqblock, iterator->last_valid_signature, time, 1, 1);
#endif

  }
  
  
 stop_scheduling:
  R_ASSERT(iterator->is_active==true);
  iterator->is_active = false;
  return DONT_RESCHEDULE;
}


void RT_schedule_Beats_newblock(struct SeqTrack *seqtrack,
                                const struct SeqBlock *seqblock,
                                const struct SeqBlock *next_seqblock,
                                const Place start_place)
{
  R_ASSERT_RETURN_IF_FALSE(seqblock->block != NULL);
  
  Beat_Iterator *iterator = &seqtrack->beat_iterator;
  memset(iterator, 0, sizeof(Beat_Iterator));

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
    int num_args = 2;
    union SuperType args[num_args];
    args[0].const_pointer = seqblock;
    args[1].const_pointer = next_seqblock;
    
    int64_t time = get_seqblock_place_time(seqblock, next_beat->l.p, PLUGINS_AND_JACK_TRANSPORT_SWINGING_MODE);

    R_ASSERT(iterator->is_active==false);
    iterator->is_active = true;
    SCHEDULER_add_event(seqtrack, time, RT_scheduled_Beat, &args[0], num_args, SCHEDULER_BEAT_PRIORITY);
  }
}
