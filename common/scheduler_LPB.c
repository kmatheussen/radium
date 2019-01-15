
#include <math.h>

#include "nsmtracker.h"
#include "placement_proc.h"
#include "sequencer_timing_proc.h"

#include "scheduler_proc.h"

/*
  This file keeps track of PPQ and BPM, i.e. realtime timing and tempo.
 */


#define DEBUG_BUGS 0


static double get_num_beats(const struct SeqTrack *seqtrack, const struct SeqBlock *seqblock, LPB_Iterator *iterator, int audioframes_to_add, const char *from_where, bool *curr_num_beats_is_valid){
  struct Blocks *block = seqblock->block;

  double time = seqtrack->start_time - seqblock->t.time;

#if DEBUG_BUGS
  R_ASSERT_NON_RELEASE(time > -(RADIUM_BLOCKSIZE*ATOMIC_DOUBLE_GET(block->reltempo)));
#endif
  
  if (time < 0) // Happens when switching between two seqblocks at a non-audio block alignment (i.e. delta time > 0). To avoid this minor inaccuracy, it seems necessary to break the use of constant 64 frame audio block sizes, so it's probably not worth it.
    time = 0;
  
  double stime_to_add = (double)audioframes_to_add * ATOMIC_DOUBLE_GET(block->reltempo);

#if DEBUG_BUGS
  printf("Get num_beats. from_where: %s, time: %f, stime_to_add: %f, pc->start_time_f: %f, stime2place: %f\n",
         from_where,
         time,
         stime_to_add,
         ATOMIC_DOUBLE_GET(seqtrack->start_time_f),
         STime2Place_f(block, time+stime_to_add)
         );
#endif

  double block_time = seqtime_to_blocktime_double(seqblock, time+stime_to_add);
  
  if (block_time > getBlockSTimeLength(block)) // can happen when switching block
    *curr_num_beats_is_valid = false;
  
  return
    iterator->num_beats_played_so_far +
    scale_double(STime2Place_f(block,block_time),
                 iterator->place1_f, iterator->place2_f,
                 0.0, iterator->num_beats_between_place1_and_place2
                 );
}


static void set_new_num_beats_values(struct SeqTrack *seqtrack, const struct SeqBlock *seqblock, LPB_Iterator *iterator, int audioblocksize, bool *curr_num_beats_is_valid){

  //double curr_num_before = iterator->num_beats_played_so_far;

  bool just_started_playing = iterator->has_next_num_beats==false;
  
  if (iterator->has_next_num_beats==false){

    iterator->curr_num_beats = get_num_beats(seqtrack, seqblock, iterator, 0, "start1", curr_num_beats_is_valid);
    iterator->next_num_beats = get_num_beats(seqtrack, seqblock, iterator, audioblocksize, "start2", curr_num_beats_is_valid);

    iterator->has_next_num_beats=true;
    
    //fprintf(stderr, "  PEQ_LPB: Set new g_curr_num_beats to %f. Place: %f - %f. curr: %f, next: %f\n", iterator->curr_num_beats,iterator->place1_f, iterator->place2_f, iterator->curr_num_beats, iterator->next_num_beats);

  } else {

#if DEBUG_BUGS
    printf("Prev curr_num_beats: %f\n",iterator->curr_num_beats);
#endif
    
    iterator->curr_num_beats = iterator->next_num_beats; // Since the prev value might have been calculated using previous LPB values, this value sometimtes might not be 100% correct, but it should be good enough.
    iterator->next_num_beats = get_num_beats(seqtrack, seqblock, iterator, audioblocksize, "nextnumbeats", curr_num_beats_is_valid);

  }

#if DEBUG_BUGS
  printf("curr: %f, next: %f\n",iterator->curr_num_beats,iterator->next_num_beats);
  if (iterator->next_num_beats <= iterator->curr_num_beats)
    abort();
  R_ASSERT_NON_RELEASE(iterator->next_num_beats > iterator->curr_num_beats);
#endif
  
  RT_Beats_set_new_last_bar_start_value(seqtrack, iterator->curr_num_beats, just_started_playing);
}



// Called from Mixer.cpp after events are calculated, and before audio is created.
void RT_LPB_set_beat_position(struct SeqTrack *seqtrack, int audioblocksize){
  
  if (is_playing()==false)
    return;

  if (seqtrack->curr_seqblock==NULL)
    return;
  
  if (seqtrack->curr_seqblock->block==NULL){
    R_ASSERT_NON_RELEASE(false);
    return;
  }
  
  bool curr_num_beats_is_valid = true; // Might become false when switching block. If false, we must not set new curr_bpm value.

  LPB_Iterator *iterator = &seqtrack->lpb_iterator;
  
  set_new_num_beats_values(seqtrack, seqtrack->curr_seqblock, iterator, audioblocksize, &curr_num_beats_is_valid);
    
  double num_beats_till_next_time = iterator->next_num_beats - iterator->curr_num_beats;

#if DEBUG_BUGS
  R_ASSERT_NON_RELEASE(num_beats_till_next_time > 0);
#endif

  if (curr_num_beats_is_valid==true) {
    double bpm = num_beats_till_next_time * 60.0 * (double)pc->pfreq / (double)audioblocksize;
    //printf("bpm: %f, curr_num_beats: %f - %f (d: %f)\n", bpm,iterator->curr_num_beats,iterator->next_num_beats,num_beats_till_next_time);

    if (fabs(bpm-iterator->curr_bpm) > 0.001)
      iterator->curr_bpm = bpm;
  }

#if DEBUG_BUGS
  static double last = 0.0;
  printf("Num_beats: %f. (diff: %f). So far: %f\n", iterator->curr_num_beats, iterator->curr_num_beats-last, iterator->num_beats_played_so_far);
  last = iterator->curr_num_beats;
#endif
}

// Returns number of beats played so far. (actually returns the number of quarter notes)
//
double RT_LPB_get_beat_position(const struct SeqTrack *seqtrack){
  if(root->song->use_sequencer_tempos_and_signatures && is_playing_song())
    return g_rt_sequencer_ppq;

  return seqtrack->lpb_iterator.curr_num_beats;
}

double RT_LPB_get_current_BPM(const struct SeqTrack *seqtrack){
  if (ATOMIC_GET(is_starting_up))
    return 120.0;

  if (is_playing()){

    if (pc->playtype==PLAYSONG && root->song->use_sequencer_tempos_and_signatures)
      return g_rt_sequencer_bpm;
    else
      return seqtrack->lpb_iterator.curr_bpm;

  } else {

    struct Blocks *block = ATOMIC_GET(g_curr_block);
    
    if (block==NULL)
      return (double)root->tempo;
    else
      return (double)root->tempo * ATOMIC_DOUBLE_GET(block->reltempo);
    
  }
}



static void print_lpb_iterator_status(const struct Blocks *block, LPB_Iterator *iterator){
#if 0 //!defined(RELEASE)
  printf("\n\nplace: %f -> %f (%d)\nlpb_value: %d\n",iterator->place1_f,iterator->place2_f,block==NULL?-1:(int)getBlockSTimeLength(block),iterator->lpb_value);
  printf("num_beats_so_far: %f", iterator->num_beats_played_so_far);
  printf("num_beats_between: %f\n\n",iterator->num_beats_between_place1_and_place2);
  //printf("beat_position: %f\n\n",RT_LPB_get_beat_position());
#endif
}

static void set_num_beats_between_place1_and_place2(LPB_Iterator *iterator){
  double duration = (iterator->place2_f - iterator->place1_f);
  iterator->num_beats_between_place1_and_place2 = duration / (double)iterator->lpb_value;
}

static void set_iterator_data2(LPB_Iterator *iterator, const struct Blocks *block, Place p1, int lpb_value, const struct LPBs *next_lpb){
  iterator->lpb_value = lpb_value;
      
  iterator->place1   = p1;
  iterator->place1_f = GetDoubleFromPlace(&p1);
      
  if (next_lpb==NULL) {
    SetAbsoluteLastPlace(&iterator->place2, block);
    iterator->place2_f = block->num_lines;
  } else {
    iterator->place2 = next_lpb->l.p;
    iterator->place2_f = GetDoubleFromPlace(&next_lpb->l.p);
  }

  set_num_beats_between_place1_and_place2(iterator);
  
  print_lpb_iterator_status(block, iterator);
}

static void set_iterator_data(LPB_Iterator *iterator, const struct Blocks *block, const struct LPBs *lpb){  
  R_ASSERT_RETURN_IF_FALSE(lpb != NULL);

  set_iterator_data2(iterator, block, lpb->l.p, lpb->lpb, NextLPB(lpb));
}

static int64_t RT_scheduled_LPB(struct SeqTrack *seqtrack, int64_t time, union SuperType *args);

static void schedule_next_LPB(struct SeqTrack *seqtrack, const struct SeqBlock *seqblock, const struct LPBs *next_lpb){
  R_ASSERT_RETURN_IF_FALSE(next_lpb != NULL);
  R_ASSERT_RETURN_IF_FALSE(seqblock!=NULL);
  
  const int num_args = 1;
        
  union SuperType args[num_args];
  args[0].const_pointer = seqblock;

  LPB_Iterator *iterator = &seqtrack->lpb_iterator;
    
  iterator->next_lpb = next_lpb;

  int64_t time = get_seqblock_place_time(seqblock, next_lpb->l.p);
  
  SCHEDULER_add_event(seqtrack, time, RT_scheduled_LPB, &args[0], num_args, SCHEDULER_LPB_PRIORITY);
}

static int64_t RT_scheduled_LPB(struct SeqTrack *seqtrack, int64_t time, union SuperType *args){

  const struct SeqBlock *seqblock = args[0].const_pointer;

  LPB_Iterator *iterator = &seqtrack->lpb_iterator;
  
  iterator->num_beats_played_so_far += iterator->num_beats_between_place1_and_place2;

  const struct LPBs *lpb = iterator->next_lpb;
  set_iterator_data(iterator, seqblock->block, lpb);
  
  const struct LPBs *next_lpb = NextLPB(lpb);

  if (next_lpb != NULL)
    schedule_next_LPB(seqtrack, seqblock, next_lpb);

  return DONT_RESCHEDULE;
}

void RT_LPB_call_when_start_playing(struct SeqTrack *seqtrack){
  LPB_Iterator *iterator = &seqtrack->lpb_iterator;
  iterator->num_beats_played_so_far = 0.0;
  iterator->num_beats_between_place1_and_place2 = 0.0;
}

void RT_schedule_LPBs_newblock(struct SeqTrack *seqtrack,
                               const struct SeqBlock *seqblock,
                               const Place start_place)
{
  R_ASSERT_RETURN_IF_FALSE(seqblock->block != NULL);
    
  LPB_Iterator *iterator = &seqtrack->lpb_iterator;

  // Null out all fields, except num_beats_played_so_far.
  double num_beats_played_so_far = iterator->num_beats_played_so_far + iterator->num_beats_between_place1_and_place2;
  memset(iterator, 0, sizeof(LPB_Iterator));
  iterator->num_beats_played_so_far = num_beats_played_so_far;
  
  const struct Blocks *block = seqblock->block;

  const struct LPBs *lpb = block->lpbs;

  if (lpb==NULL) {

    set_iterator_data2(iterator, block, PlaceFirstPos, root->lpb, NULL);
    
  } else if (PlaceGreaterThan(&lpb->l.p, &start_place)){

    set_iterator_data2(iterator, block, PlaceFirstPos, root->lpb, lpb);      
    schedule_next_LPB(seqtrack, seqblock, lpb);
    
  } else {
    
    const struct LPBs *next_lpb = NextLPB(lpb);
    
    // spool forward to the 'lpb' that is used by 'start_place'
    //
    while(next_lpb != NULL){
      if (PlaceGreaterThan(&next_lpb->l.p, &start_place))
        break;
      
      lpb = next_lpb;
      next_lpb = NextLPB(lpb);
    }
    
    set_iterator_data(iterator, block, lpb);
    
    if (next_lpb != NULL)
      schedule_next_LPB(seqtrack, seqblock, next_lpb);
    
  }

  
  R_ASSERT(iterator->lpb_value != 0);  
}
