
#include <math.h>

#include "nsmtracker.h"
#include "placement_proc.h"

#include "scheduler_proc.h"

/*
  This file keeps track of PPQ and BPM, i.e. realtime timing and tempo.
 */


static double get_num_beats(const struct SeqBlock *seqblock, const LPB_Iterator *iterator, int audioframes_to_add){
  struct Blocks *block = pc->block;

  double time = ATOMIC_DOUBLE_GET(pc->start_time_f) - seqblock->time; //(double)ATOMIC_GET(pc->seqtime);

  //printf("start_time_f: %d / %d (%d), seqblock->time: %d\n",(int)ATOMIC_DOUBLE_GET(pc->start_time_f), (int)pc->start_time, (int)fabsf(ATOMIC_DOUBLE_GET(pc->start_time_f)-pc->start_time), (int)seqblock->time);
  
  R_ASSERT_NON_RELEASE(time > -(RADIUM_BLOCKSIZE*safe_volatile_float_read(&block->reltempo)));
  
  if (time < 0) // Happens when switching between two seqblocks at a non-audio block alignment (i.e. delta time > 0). To avoid this minor inaccuracy, it seems necessary to break the use of constant 64 frame audio block sizes, so it's not worth it.
    time = 0;
  
  double time_to_add = (double)audioframes_to_add * safe_volatile_float_read(&block->reltempo);

  //fprintf(stderr, "Time to add: %f. time: %f, 2place_f: %f. 1-2: %f - %f\n", time_to_add, time, STime2Place_f(block, time+time_to_add),ATOMIC_DOUBLE_GET(pc->start_time_f),pc->end_time_f);
  
  return
    iterator->num_beats_played_so_far +
    scale_double(STime2Place_f(block, time+time_to_add),
                 iterator->place1_f, iterator->place2_f,
                 0.0, iterator->num_beats_between_place1_and_place2
                 );
}


static void set_new_num_beats_values(struct SeqTrack *seqtrack, const struct SeqBlock *seqblock, LPB_Iterator *iterator, int audioblocksize){

  //double curr_num_before = iterator->num_beats_played_so_far;

  bool just_started_playing = iterator->has_next_num_beats==false;
  
  if (iterator->has_next_num_beats==false){

    iterator->curr_num_beats = get_num_beats(seqblock, iterator, 0);
    iterator->next_num_beats = get_num_beats(seqblock, iterator, audioblocksize);

    iterator->has_next_num_beats=true;
    
    //fprintf(stderr, "  PEQ_LPB: Set new g_curr_num_beats to %f. Place: %f - %f. curr: %f, next: %f\n", iterator->curr_num_beats,iterator->place1_f, iterator->place2_f, iterator->curr_num_beats, iterator->next_num_beats);

  } else {

    iterator->curr_num_beats = iterator->next_num_beats; // Since the prev value might have been calculated using previous LPB values, this value sometimtes might not be 100% correct, but it should be good enough.
    iterator->next_num_beats = get_num_beats(seqblock, iterator, audioblocksize);

  }

  R_ASSERT_NON_RELEASE(iterator->next_num_beats > iterator->curr_num_beats);

  RT_Beats_set_new_last_bar_start_value(seqtrack, iterator->curr_num_beats, just_started_playing);
}



// Called from Mixer.cpp after events are calculated, and before audio is created.
void RT_LPB_set_beat_position(struct SeqTrack *seqtrack, int audioblocksize){
  
  if (is_playing()==false)
    return;

  LPB_Iterator *iterator = &seqtrack->lpb_iterator;

  //R_ASSERT( (pc->end_time-pc->seqtime) >= iterator->time1);
  //R_ASSERT( (pc->start_time - pc->seqtime) < iterator->time2);
  
  set_new_num_beats_values(seqtrack, seqtrack->curr_seqblock, iterator, audioblocksize);
    
  double num_beats_till_next_time = iterator->next_num_beats - iterator->curr_num_beats;

  R_ASSERT_NON_RELEASE(num_beats_till_next_time > 0);

  double beats_per_minute = num_beats_till_next_time * 60.0 * (double)pc->pfreq / (double)audioblocksize;
  //printf("beats_per_minute: %f, curr_num_beats: %f - %f (d: %f)\n", beats_per_minute,iterator->curr_num_beats,iterator->next_num_beats,num_beats_till_next_time);

  iterator->curr_beats_per_minute = beats_per_minute;
}

// Returns number of beats played so far. (actually returns the number of quarter notes)
//
double RT_LPB_get_beat_position(const struct SeqTrack *seqtrack){
  return seqtrack->lpb_iterator.curr_num_beats;
}

double RT_LPB_get_current_BPM(const struct SeqTrack *seqtrack){
  if (ATOMIC_GET(is_starting_up))
    return 120.0;
  
  else if (is_playing())
    return seqtrack->lpb_iterator.curr_beats_per_minute;
  
  else {
    struct Blocks *block = ATOMIC_GET(g_curr_block);
    
    if (block==NULL)
      return (double)root->tempo;
    else
      return (double)root->tempo * safe_volatile_float_read(&block->reltempo);
  }

}



static void print_lpb_iterator_status(const struct Blocks *block, LPB_Iterator *iterator){
#if !defined(RELEASE)
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

static void RT_scheduled_LPB(int64_t time, const union SuperType *args);

static void schedule_next_LPB(struct SeqTrack *seqtrack, const struct SeqBlock *seqblock, const struct LPBs *next_lpb){
  R_ASSERT_RETURN_IF_FALSE(next_lpb != NULL);
  
  const int num_args = 2;
        
  union SuperType args[num_args];
  args[0].pointer       = seqtrack;
  args[1].const_pointer = seqblock;

  LPB_Iterator *iterator = &seqtrack->lpb_iterator;
    
  iterator->next_lpb = next_lpb;

  int64_t time = get_seqblock_place_time(seqblock, next_lpb->l.p);
  
  SCHEDULER_add_event(time, RT_scheduled_LPB, &args[0], num_args, SCHEDULER_LPB_PRIORITY);
}

static void RT_scheduled_LPB(int64_t time, const union SuperType *args){

  struct SeqTrack       *seqtrack = args[0].pointer;
  const struct SeqBlock *seqblock = args[1].const_pointer;

  LPB_Iterator *iterator = &seqtrack->lpb_iterator;
  
  iterator->num_beats_played_so_far += iterator->num_beats_between_place1_and_place2;

  const struct LPBs *lpb = iterator->next_lpb;
  set_iterator_data(iterator, seqblock->block, lpb);
  
  const struct LPBs *next_lpb = NextLPB(lpb);

  if (next_lpb != NULL)
    schedule_next_LPB(seqtrack, seqblock, next_lpb);
}

void RT_schedule_LPBs_newblock(struct SeqTrack *seqtrack,
                               const struct SeqBlock *seqblock,
                               const Place start_place)
{
  LPB_Iterator *iterator = &seqtrack->lpb_iterator;
  memset(iterator, 0, sizeof(LPB_Iterator));

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
