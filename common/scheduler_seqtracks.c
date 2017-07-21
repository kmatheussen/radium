
#include "nsmtracker.h"
#include "time_proc.h"
#include "playerclass.h"
#include "placement_proc.h"
#include "seqtrack_proc.h"
#include "../audio/Mixer_proc.h"

#include "scheduler_proc.h"

#define DO_DEBUG 0

#define G_NUM_ARGS 4

static int64_t RT_scheduled_seqblock(struct SeqTrack *seqtrack, int64_t time, union SuperType *args);

static int64_t RT_scheduled_end_of_seqblock(struct SeqTrack *seqtrack, int64_t seqtime, union SuperType *args){
  atomic_pointer_write_relaxed((void**)&seqtrack->curr_seqblock, NULL);

  return DONT_RESCHEDULE;
}

static void RT_schedule_new_seqblock(struct SeqTrack *seqtrack,
                                     struct SeqBlock *seqblock,
                                     int64_t seqtime,
                                     int64_t block_start_time,
                                     const Place *place_pointer,
                                     int playtype)
{

  R_ASSERT_RETURN_IF_FALSE(seqblock!=NULL);
  
  Place place;

  struct Blocks *block = seqblock->block;

  bool new_block = seqtrack->curr_seqblock != seqblock;

  if (new_block) {
    atomic_pointer_write_relaxed((void**)&seqtrack->curr_seqblock, seqblock); // bang!

    // Any value less than -10 will delay rendering the new block. Instead we wait until player.c is called and a proper player_time value is calculated.
    // To avoid jumpy graphics.
    ATOMIC_DOUBLE_SET(block->player_time, -100.0);
  }
  
  if (playtype==PLAYBLOCK) {

    R_ASSERT_RETURN_IF_FALSE(place_pointer != NULL);

    place = *place_pointer;
    
    seqblock->time = block_start_time; // We can not set seqblock->time before scheduling since seqblock->time is used various places when playing a block.
    
  } else {

    if (place_pointer==NULL)
      place = STime2Place(block, seqtime - seqblock->time);
    else
      place = *place_pointer;
  }
  
#if DO_DEBUG
  printf("  RT_schedule_new_seqblock called. place: %d + %d/%d\n", place.line,place.counter,place.dividor);
#endif
  
  // Schedule notes, fx, etc.
  {
    // Signature
    RT_schedule_Signature_newblock(seqtrack, seqblock, place);

    // Beats
    RT_schedule_Beats_newblock(seqtrack, seqblock, place);

    // LPB
    RT_schedule_LPBs_newblock(seqtrack, seqblock, place);
    
    // Reallines
    RT_schedule_reallines_in_block(seqtrack, seqblock, place);

    // Send new track pan values to patches (also assert that all tracks have track->times
    //
    struct Tracks *track=block->tracks;
    while(track!=NULL){
      R_ASSERT_RETURN_IF_FALSE(track->times!=NULL);
      if(track->panonoff && track->patch!=NULL){
        (*track->patch->changeTrackPan)(track->pan,track);
      }
      track=NextTrack(track);
    }

    // fx
    RT_schedule_fxs_newblock(seqtrack, seqblock, seqtime, place);
    
    // notes
    RT_schedule_notes_newblock(seqtrack, seqblock, seqtime, place);
  }

  // Schedule end block
  {
    union SuperType args[1];
    int64_t endblock_time = seqblock->time + getBlockSTimeLength(block);
    SCHEDULER_add_event(seqtrack, endblock_time, RT_scheduled_end_of_seqblock, &args[0], 0, SCHEDULER_ENDBLOCK_PRIORITY);
  }
    

  // Schedule next block, and set various data
  {
    
    int64_t next_time;
    struct SeqBlock *next_seqblock;

    if (playtype==PLAYBLOCK) {

      if (MIXER_is_saving()){
        next_seqblock = NULL;
        next_time = 0;
      } else {
        int64_t seqblock_duration = getBlockSTimeLength(block);
        next_seqblock = seqblock;
        next_time = seqblock->time + seqblock_duration;
      }
      
    } else {
      
      next_seqblock = get_next_seqblock(seqtrack, seqblock->time);
      if (next_seqblock != NULL)
        next_time = next_seqblock->time;
      else
        next_time = 0; // To avoid faulty gcc warning.
    }

    // Schedule next block
    //
    if (next_seqblock != NULL) {

      static const Place first_place = {0,0,1};

      union SuperType args[G_NUM_ARGS];

      args[0].pointer       = next_seqblock;
      args[1].int_num       = next_time;
      args[2].const_pointer = &first_place;
      args[3].int32_num     = playtype;

#if DO_DEBUG
      printf("  2. Scheduling RT_scheduled_seqblock at %f\n",next_time/MIXER_get_sample_rate());
#endif
      SCHEDULER_add_event(seqtrack, next_time, RT_scheduled_seqblock, &args[0], G_NUM_ARGS, SCHEDULER_INIT_BLOCK_PRIORITY);

    } else {
      
#if DO_DEBUG
      printf("    3. seqtrack finished.\n");
#endif
      
    }

  }
  

}

static int64_t RT_scheduled_seqblock(struct SeqTrack *seqtrack, int64_t seqtime, union SuperType *args){
  struct SeqBlock       *seqblock         = args[0].pointer;
  int64_t                block_start_time = args[1].int_num;
  const Place           *place            = args[2].const_pointer;
  int                    playtype         = args[3].int32_num;

#if DO_DEBUG
  printf("     RT_scheduled_seqblock called. time: %f\n", (double)seqtime/MIXER_get_sample_rate());
#endif
  RT_schedule_new_seqblock(seqtrack, seqblock, seqtime, block_start_time, place, playtype);

  return DONT_RESCHEDULE;
}


void start_seqtrack_song_scheduling(const player_start_data_t *startdata, int playtype){
  static Place static_place;

  static_place = startdata->place;
    
  R_ASSERT(ATOMIC_GET(pc->player_state)==PLAYER_STATE_STOPPED);

  {
    struct SeqBlock *seqblock = startdata->seqblock;
    struct Blocks *block = seqblock==NULL ? NULL : seqblock->block;

    if (block!=NULL)
      ATOMIC_DOUBLE_SET(block->player_time, -100.0); // Stop gfx rendering since we are soon going to change the values of seqtrack->end_time and friends.
  }
  
  
  int64_t seqtime = 0;
  int64_t abs_start_time;
  
  if (startdata->seqtrack == NULL) {
    abs_start_time = startdata->abstime;
  } else {
    R_ASSERT_RETURN_IF_FALSE(startdata->seqblock!=NULL);
    R_ASSERT_RETURN_IF_FALSE(startdata->seqblock->block!=NULL);
    STime block_stime = Place2STime(startdata->seqblock->block, &startdata->place);
    seqtime = startdata->seqblock->time + block_stime;
    abs_start_time = get_abstime_from_seqtime(startdata->seqtrack, startdata->seqblock, seqtime);
  }

  
  int64_t seq_start_times[root->song->seqtracks.num_elements];
  VECTOR_FOR_EACH(struct SeqTrack *seqtrack, &root->song->seqtracks){
    if (seqtrack==startdata->seqtrack)
      seq_start_times[iterator666] = seqtime;
    else
      seq_start_times[iterator666] = get_seqtime_from_abstime(seqtrack, NULL, abs_start_time); // Ab ab ab. Not quite working if starting to play in the middle of a block, I think.      
  }END_VECTOR_FOR_EACH;

  
  PLAYER_lock();{

    pc->playtype = playtype;
        
    R_ASSERT(SCHEDULER_num_events(RT_get_curr_seqtrack()->scheduler)==0);

    SCHEDULER_set_seqtrack_timing(root->song->block_seqtrack, 0, 0);
    RT_LPB_call_when_start_playing(root->song->block_seqtrack);
        
    ATOMIC_DOUBLE_SET(pc->song_abstime, abs_start_time);

    VECTOR_FOR_EACH(struct SeqTrack *seqtrack, &root->song->seqtracks){

      atomic_pointer_write_relaxed((void**)&seqtrack->curr_seqblock, NULL);

      int64_t seq_start_time = seq_start_times[iterator666];

      SCHEDULER_set_seqtrack_timing(seqtrack, seq_start_time, seq_start_time);
      RT_LPB_call_when_start_playing(seqtrack);
      
      // Schedule the first seqblock in each seqtrack.
      VECTOR_FOR_EACH(struct SeqBlock *seqblock, &seqtrack->seqblocks){
        
        int64_t seqblock_start_time = seqblock->time;
        int64_t seqblock_end_time   = seqblock_start_time + getBlockSTimeLength(seqblock->block);
        
        if (seq_start_time < seqblock_end_time){
          
          union SuperType args[G_NUM_ARGS];
          
          args[0].pointer       = seqblock;
          args[1].int_num       = seqblock->time;
          args[2].const_pointer = startdata->seqblock==seqblock ? &static_place : NULL;
          args[3].int_num       = PLAYSONG;

          int64_t seqtime = R_MAX(seqblock_start_time, seq_start_time);
#if DO_DEBUG
          printf("  Song: Scheduling RT_scheduled_seqblock at %f. seqtrack->start_time: %f\n",(double)seqtime/MIXER_get_sample_rate(), (double)seqtrack->start_time/MIXER_get_sample_rate());
#endif
          
          SCHEDULER_add_event(seqtrack, seqtime, RT_scheduled_seqblock, &args[0], G_NUM_ARGS, SCHEDULER_INIT_BLOCK_PRIORITY);
          
          break;
        }
        
      }END_VECTOR_FOR_EACH;
    
    }END_VECTOR_FOR_EACH;

  }PLAYER_unlock();
}


void start_seqtrack_block_scheduling(struct Blocks *block, const Place place, int playtype){
  static Place static_place;

  static_place = place;

  R_ASSERT_RETURN_IF_FALSE(block!=NULL);
  
  R_ASSERT_RETURN_IF_FALSE(ATOMIC_GET(pc->player_state)==PLAYER_STATE_STOPPED);
  
    
  ATOMIC_DOUBLE_SET(block->player_time, -100.0); // Stop gfx rendering since we are soon going to change the values of seqtrack->end_time and friends.

  int64_t seq_start_time = Place2STime(block, &place);
              
  PLAYER_lock();{

    pc->playtype = playtype;
      
    struct SeqTrack *seqtrack = root->song->block_seqtrack;

    SCHEDULER_set_seqtrack_timing(seqtrack, seq_start_time, seq_start_time);
    RT_LPB_call_when_start_playing(seqtrack);
    
#if DO_DEBUG
    printf("  Scheduling start-playing event at 0. Seqtrack: %p\n", RT_get_curr_seqtrack());
#endif

    R_ASSERT(SCHEDULER_num_events(seqtrack->scheduler)==0);

    atomic_pointer_write_relaxed((void**)&seqtrack->curr_seqblock, NULL);
    
    static struct SeqBlock seqblock = {0};
    seqblock.block = block;
    seqblock.time = 0;
 
    union SuperType args[G_NUM_ARGS];
    
    args[0].pointer = &seqblock;
    args[1].int_num = seqblock.time;
    args[2].const_pointer = &static_place;
    args[3].int32_num = PLAYBLOCK;

#if DO_DEBUG
    printf("  Scheduling RT_scheduled_seqblock at %d. seqtrack->start_time: %d\n",(int)seq_start_time, (int)seqtrack->start_time);
#endif
    SCHEDULER_add_event(seqtrack, seq_start_time, RT_scheduled_seqblock, &args[0], G_NUM_ARGS, SCHEDULER_INIT_BLOCK_PRIORITY);
    
  }PLAYER_unlock();
}


#if 0
void start_seqtrack_scheduling(const player_start_data_t *startdata){
  if (startdata->playtype==PLAYSONG) {
    
    start_seqtrack_song_scheduling(startdata);
    
  } else {

    start_seqtrack_block_sceduling(startdata->seqblock->block, startdata->place);
    
  }
    
}

#endif
