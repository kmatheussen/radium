
#include "nsmtracker.h"
#include "time_proc.h"
#include "playerclass.h"
#include "placement_proc.h"
#include "sequencer_proc.h"
#include "visual_proc.h"
#include "fxlines_proc.h"
#include "velocities_proc.h"

#include "../audio/Mixer_proc.h"

#include "scheduler_proc.h"

#define DO_DEBUG 0

#define G_NUM_ARGS 6

static int64_t RT_scheduled_seqblock(struct SeqTrack *seqtrack, int64_t time, union SuperType *args);

static int64_t RT_scheduled_end_of_seqblock(struct SeqTrack *seqtrack, int64_t seqtime, union SuperType *args){
  atomic_pointer_write_relaxed((void**)&seqtrack->curr_seqblock, NULL);

#if 1
  struct SeqBlock *seqblock = args[0].pointer;
  RT_SEQBLOCK_remove_all_playing_notes(seqblock);
#endif

  
  return DONT_RESCHEDULE;
}

static void RT_schedule_new_seqblock(struct SeqTrack *seqtrack,
                                     struct SeqBlock *seqblock,
                                     const struct SeqBlock *next_seqblock,
                                     int64_t seqtime,
                                     int64_t block_start_time,
                                     const Place *place_pointer,
                                     int playtype)
{

  R_ASSERT_RETURN_IF_FALSE(seqblock!=NULL);

  const struct Blocks *block = seqblock->block;
      
  Place place;

  if (playtype==PLAYBLOCK) {

    R_ASSERT_RETURN_IF_FALSE(place_pointer != NULL);

    place = *place_pointer;
    
    int64_t duration = SEQBLOCK_get_seq_duration(seqblock);
    seqblock->t.time = block_start_time; // We can not set seqblock->time before scheduling since seqblock->time is used various places when playing a block.
    seqblock->t.time2 = seqblock->t.time + duration;

    R_ASSERT_NON_RELEASE(seqtrack==root->song->block_seqtrack);

    // Need to call RT_EDITSEQBLOCK_call_each_block again here since last call to it hadn't updated t.time/t.time2 values.
    if (seqtrack==root->song->block_seqtrack){
      // FIX: Only call if seqtrack->start_time != last time. (Called twice at beginning for instance, double up notes.)

      // Commented out for now. Need fix to avoid double-up.
      //printf("   Call 1\n");
      //RT_EDITSEQBLOCK_call_each_block(seqtrack, &g_block_seqtrack_seqblock, seqtrack->start_time, seqtrack->end_time);
    }
    
  } else {

    if (place_pointer==NULL)
      place = STime2Place(block, seqtime_to_blocktime(seqblock, seqtime - seqblock->t.time), PLUGINS_AND_JACK_TRANSPORT_SWINGING_MODE); // not sure if place would ever be something other than 0,0,1 here.
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
    RT_schedule_Beats_newblock(seqtrack, seqblock, next_seqblock, place);

    // LPB
    RT_schedule_LPBs_newblock(seqtrack, seqblock, place);
    
    // Reallines
    RT_schedule_reallines_in_block(seqtrack, seqblock, place);

    // Send new track pan values to patches (also assert that all tracks have track->times
    //
    const struct Tracks *track=block->tracks;
    while(track!=NULL){
      R_ASSERT_RETURN_IF_FALSE(track->times!=NULL);
      if(track->panonoff && track->patch!=NULL){
        (*track->patch->changeTrackPan)(track->pan,track);
      }
      track=NextTrack(track);
    }

    // fx
    //RT_schedule_fxs_newblock(seqtrack, seqblock, seqtime, place);
    
    // notes
    RT_schedule_notes_newblock(seqtrack, seqblock, seqtime, place);
  }

  // Schedule end block
  {
    union SuperType args[1];
    args[0].pointer = seqblock;
    SCHEDULER_add_event(seqtrack, seqblock->t.time2, RT_scheduled_end_of_seqblock, &args[0], 1, SCHEDULER_ENDBLOCK_PRIORITY);
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
        next_seqblock = seqblock;
        next_time = seqblock->t.time2;
      }
      
    } else {
      
      next_seqblock = get_next_seqblock_block(seqtrack, seqblock->t.time);
      if (next_seqblock != NULL)
        next_time = next_seqblock->t.time;
      else
        next_time = 0; // To avoid faulty gcc warning.
    }

    // Schedule next block
    //
    if (next_seqblock != NULL) {

      union SuperType args[G_NUM_ARGS];

      args[0].pointer       = next_seqblock;
      args[1].int_num       = next_time;
      args[2].const_pointer = &seqblock->t.start_place;
      args[3].int32_num     = playtype;
      args[4].const_pointer = seqblock;
      args[5].const_pointer = NULL;
      
#if DO_DEBUG
      printf("  2. Scheduling RT_scheduled_seqblock at %f. Curr_time: %f\n",next_time/MIXER_get_sample_rate(), seqtime/MIXER_get_sample_rate());
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
  const struct SeqBlock *prev_seqblock    = args[4].const_pointer;
  const struct SeqBlock *next_seqblock    = args[5].const_pointer;

  R_ASSERT_RETURN_IF_FALSE2(seqblock->block!=NULL, DONT_RESCHEDULE);
  
#if DO_DEBUG
  printf("     RT_scheduled_seqblock called. time: %f\n", (double)seqtime/MIXER_get_sample_rate());
#endif

  bool new_block = seqtrack->curr_seqblock != seqblock;
  
  if (new_block) {
    atomic_pointer_write_relaxed((void**)&seqtrack->curr_seqblock, seqblock); // bang!
    
    // Any value less than -10 will delay rendering the new block. Instead we wait until player.c is called and a proper player_time value is calculated.
    // To avoid jumpy graphics.
    ATOMIC_DOUBLE_SET(seqblock->block->player_time, -100.0);
  }
  
  // Manually call GFX_ScheduleEditorRedraw() if playing the same block again but with settings in the the seqblocks that would cause editor to be rendered differently.
  if (prev_seqblock != NULL && seqblock!=prev_seqblock){
    const struct Blocks *block = seqblock->block;
    const struct Blocks *prev_block = prev_seqblock->block;
    if (block == prev_block){
      const bool *prev_disabled = prev_seqblock->track_is_disabled;
      const bool *disabled = seqblock->track_is_disabled;
      if (prev_disabled!=NULL && disabled!=NULL){
        for(int i=0;i<block->num_tracks;i++){
          if(prev_disabled[i] != disabled[i]){
#if DO_DEBUG
            printf("   RT_scheduled_seqblock: Calling GFX_ScheduleEditorRedraw\n");
#endif
            GFX_ForceScheduleEditorRedraw();
            break;
          }
        }
      }
    }
  }

  /*
  if (playtype==PLAYBLOCK)
    ATOMIC_ADD(pc->play_id, 1);
  */

  RT_schedule_new_seqblock(seqtrack, seqblock, next_seqblock, seqtime, block_start_time, place, playtype);

  return DONT_RESCHEDULE;
}


void start_seqtrack_song_scheduling(const player_start_data_t *startdata, int playtype){
  static Place static_place;

  static_place = startdata->place;
    
  R_ASSERT(ATOMIC_GET(pc->player_state)==PLAYER_STATE_STOPPED);

  int64_t seq_start_time;

  {
    const struct SeqBlock *seqblock = startdata->seqblock;
    struct Blocks *block = seqblock==NULL ? NULL : seqblock->block;

    if (block!=NULL)
      ATOMIC_DOUBLE_SET(block->player_time, -100.0); // Stop gfx rendering since we are soon going to change the values of seqtrack->end_time and friends.
    
    if (startdata->seqtrack == NULL) {
      seq_start_time = startdata->abstime;
    } else {
      R_ASSERT_RETURN_IF_FALSE(seqblock!=NULL);
      R_ASSERT_RETURN_IF_FALSE(block!=NULL);
      STime block_stime = Place2STime(block, &startdata->place, PLUGINS_AND_JACK_TRANSPORT_SWINGING_MODE);
      seq_start_time = seqblock->t.time + blocktime_to_seqtime(seqblock, block_stime);
    }
  }
  
  
  PLAYER_lock();{

    pc->playtype = playtype;

    // Commented out. This can happen when receiving from MIDI input.
    // Play to an instrument that has delayed start of notes (or perhaps sending to an instrument that is delayed because of plugin delay compensation).
    //R_ASSERT(SCHEDULER_num_events(RT_get_curr_seqtrack()->scheduler)==0);

    SCHEDULER_set_seqtrack_timing(root->song->block_seqtrack, 0, 0);
    RT_LPB_call_when_start_playing(root->song->block_seqtrack);

    ATOMIC_DOUBLE_SET(pc->song_abstime, seq_start_time);

    VECTOR_FOR_EACH(struct SeqTrack *seqtrack, &root->song->seqtracks){

      atomic_pointer_write_relaxed((void**)&seqtrack->curr_seqblock, NULL);

      SCHEDULER_set_seqtrack_timing(seqtrack, seq_start_time, seq_start_time);
      RT_LPB_call_when_start_playing(seqtrack);

      // Schedule the first seqblock in each seqtrack.
      VECTOR_FOR_EACH(struct SeqBlock *seqblock, &seqtrack->seqblocks){

        if (seqblock->block == NULL)
          continue;

        int64_t seqblock_start_time = seqblock->t.time;
        int64_t seqblock_end_time   = SEQBLOCK_get_seq_endtime(seqblock);

        const struct SeqBlock *next_seqblock = NULL;
        if(iterator666 < seqtrack->seqblocks.num_elements-1)
          next_seqblock = seqtrack->seqblocks.elements[iterator666 + 1];

        if (seq_start_time < seqblock_end_time){

          union SuperType args[G_NUM_ARGS];
          
          args[0].pointer       = seqblock;
          args[1].int_num       = seqblock->t.time;
          args[2].const_pointer = startdata->seqblock==seqblock ? &static_place : NULL;
          args[3].int_num       = PLAYSONG;
          args[4].const_pointer = NULL;
          args[5].const_pointer = next_seqblock;
          
          int64_t seqtime = R_MAX(seqblock_start_time, seq_start_time);
#if DO_DEBUG
          printf("  Song: Scheduling RT_scheduled_seqblock at %f. seqtrack->start_time: %f\n",(double)seqtime/MIXER_get_sample_rate(), (double)seqtrack->start_time/MIXER_get_sample_rate());
#endif
          
          SCHEDULER_add_event(seqtrack, seqtime, RT_scheduled_seqblock, &args[0], G_NUM_ARGS, SCHEDULER_INIT_BLOCK_PRIORITY);
          
          break;
          
        } else if (next_seqblock==NULL || seq_start_time < next_seqblock->t.time) {

          RT_schedule_beats_between_seqblocks(seqtrack, seq_start_time, seqblock, next_seqblock);
          
        }
        
      }END_VECTOR_FOR_EACH;
    
    }END_VECTOR_FOR_EACH;

  }PLAYER_unlock();
}


struct SeqBlock g_block_seqtrack_seqblock = {0};

void start_seqtrack_block_scheduling(struct Blocks *block, const Place place, int playtype){
  R_ASSERT(playtype==PLAYBLOCK);
  
  static Place static_place;

  static_place = place;

  R_ASSERT_RETURN_IF_FALSE(block!=NULL);
  
  R_ASSERT_RETURN_IF_FALSE(ATOMIC_GET(pc->player_state)==PLAYER_STATE_STOPPED);
  
    
  ATOMIC_DOUBLE_SET(block->player_time, -100.0); // Stop gfx rendering since we are soon going to change the values of seqtrack->end_time and friends.

  int64_t seq_start_time = Place2STime(block, &place, EDITOR_CURR_TRACK_SWINGING_MODE); // When playing block, seqtime==blocktime.

  SEQBLOCK_init(NULL, &g_block_seqtrack_seqblock, block, NULL, -1, NULL, 0);

  PLAYER_lock();{

    pc->playtype = playtype;
      
    struct SeqTrack *seqtrack = root->song->block_seqtrack;

    SCHEDULER_set_seqtrack_timing(seqtrack, seq_start_time, seq_start_time);
    RT_LPB_call_when_start_playing(seqtrack);
    
#if DO_DEBUG
    printf("  Scheduling start-playing event at 0. Seqtrack: %p\n", RT_get_curr_seqtrack());
#endif


    // Commented out. This can happen when receiving from MIDI input.
    // Play to an instrument that has delayed start of notes (or perhaps sending to an instrument that is delayed because of plugin delay compensation).
    //R_ASSERT(SCHEDULER_num_events(seqtrack->scheduler)==0);
    
    atomic_pointer_write_relaxed((void**)&seqtrack->curr_seqblock, NULL);
    
    union SuperType args[G_NUM_ARGS];
    
    args[0].pointer = &g_block_seqtrack_seqblock;
    args[1].int_num = g_block_seqtrack_seqblock.t.time;
    args[2].const_pointer = &static_place;
    args[3].int32_num = PLAYBLOCK;
    args[4].const_pointer = &g_block_seqtrack_seqblock;
    args[5].const_pointer = NULL;
    
#if DO_DEBUG
    printf("  Scheduling RT_scheduled_seqblock at %d. seqtrack->start_time: %d\n",(int)seq_start_time, (int)seqtrack->start_time);
#endif
    SCHEDULER_add_event(seqtrack, seq_start_time, RT_scheduled_seqblock, &args[0], G_NUM_ARGS, SCHEDULER_INIT_BLOCK_PRIORITY);
    
  }PLAYER_unlock();
}

