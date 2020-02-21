
#include "nsmtracker.h"
#include "patch_proc.h"

#include "scheduler_proc.h"

#define DO_DEBUG 0


#define g_num_velocities_args 7


static void RT_schedule_velocity(struct SeqTrack *seqtrack,
                                 int64_t current_time,
                                 const struct SeqBlock *seqblock,
                                 const struct Tracks *track,
                                 struct Notes *note,
                                 const struct Velocities *velocity1,
                                 bool first_val_has_been_sent
                                 );

static void RT_scheduled_hold_velocity_do(struct SeqTrack *seqtrack,
                                          int64_t time,
                                          const struct SeqBlock *seqblock,
                                          const struct Tracks *track,
                                          struct Notes *note,
                                          const struct Velocities *velocity1,
                                          bool first_val_has_been_sent)
{
  struct Patch *patch = track->patch;

  if (patch==NULL)
    return;

  if (first_val_has_been_sent==false && velocity1!=NULL) {
    
    int val = velocity1->velocity;

#if DO_DEBUG
    printf("  Sending HOLD velocity %x at %d\n",val,(int)time);
#endif

    note->has_sent_seqblock_volume_automation_this_block = true;

    note->curr_velocity = TRACK_get_velocity(track, val); // The logical behavior would be to use velocity1->velocity, but we don't have access to track in the function 'RT_PATCH_voice_volume_has_changed'.
    note->curr_velocity_time = time;

    RT_PATCH_change_velocity(seqtrack,
                             patch,
                             create_note_t(seqblock,
                                           note->id,
                                           note->note,
                                           note->curr_velocity * seqblock->curr_gain * seqtrack->note_gain * seqtrack->note_gain_muted,
                                           0,
                                           ATOMIC_GET(track->midi_channel),
                                           0,
                                           0
                                           ),
                             time
                             );
  }

  const struct Velocities *velocity2 = velocity1==NULL ? note->velocities : NextVelocity(velocity1);
  if (velocity2 != NULL){
    RT_schedule_velocity(seqtrack, time, seqblock, track, note, velocity2, false);
  }
}

static int64_t RT_scheduled_hold_velocity(struct SeqTrack *seqtrack, int64_t time, union SuperType *args){
  const struct SeqBlock   *seqblock  = args[0].const_pointer;
  const struct Tracks     *track     = args[1].const_pointer;
  struct Notes      *note      = args[2].pointer;
  const struct Velocities *velocity1 = args[3].pointer;

  RT_scheduled_hold_velocity_do(seqtrack, time, seqblock, track, note, velocity1, false);
  
  return DONT_RESCHEDULE;
}


static int64_t RT_scheduled_glide_velocity(struct SeqTrack *seqtrack, int64_t time, union SuperType *args){
  const struct SeqBlock   *seqblock  = args[0].const_pointer;
  const struct Tracks     *track     = args[1].const_pointer;
  struct Notes      *note      = args[2].pointer;
  const struct Velocities *velocity1 = args[3].pointer;
  int64_t                  time1     = args[4].int_num;
  int64_t                  time2     = args[5].int_num;
  int                      last_val  = args[6].int32_num;

  struct Patch *patch = track->patch;

  if (patch==NULL)
    return DONT_RESCHEDULE;
  
#if !defined(RELEASE)
  if (time < time1) // || time>time2) Note that time>time2 can happen since the scheduler ensures that the time is not in the past.
    RError("RT_scheduled_glide_velocity: time: %d, time1: %d, time2: %d", (int)time, (int)time1, (int)time2);
#endif
  
  if (time < time1)
    time = time1;
  if (time > time2)
    time = time2;
  
  const struct Velocities *velocity2 = velocity1==NULL ? note->velocities : NextVelocity(velocity1);

  int val1 = velocity1==NULL ? note->velocity     : velocity1->velocity;
  int val2 = velocity2==NULL ? note->velocity_end : velocity2->velocity;
    
  int val = time1==time2 ? val2 : scale(time, time1, time2, val1, val2); // We get divide by zero in scale() if time1==time2
  
  if (val != last_val || note->scheduler_must_send_velocity_next_block) {
#if DO_DEBUG
    printf("  Sending velocity %x at %d\n",val,(int)time);
#endif

    note->has_sent_seqblock_volume_automation_this_block = true;
    
    if (note->scheduler_must_send_velocity_next_block)
      note->scheduler_must_send_velocity_next_block = false;
    
    note->curr_velocity = TRACK_get_velocity(track, val);
    note->curr_velocity_time = time;
    
    RT_PATCH_change_velocity(seqtrack,
                             patch,
                             create_note_t(seqblock,
                                           note->id,
                                           note->note,
                                           note->curr_velocity * seqblock->curr_gain * seqtrack->note_gain * seqtrack->note_gain_muted,
                                           0,
                                           ATOMIC_GET(track->midi_channel),
                                           0,
                                           0
                                           ),
                             time
                             );
  }

  if (time >= time2) {
    
    if (velocity2 != NULL)
      RT_schedule_velocity(seqtrack, time, seqblock, track, note, velocity2, true);
    
    note->scheduler_may_send_velocity_next_block = false;
    
    return DONT_RESCHEDULE;
    
  } else {
    
    args[6].int32_num = val;

    note->scheduler_may_send_velocity_next_block = true;
    
    return R_MIN(time2, time + RADIUM_BLOCKSIZE);
    
  }
}

static void RT_schedule_velocity(struct SeqTrack *seqtrack,
                                 int64_t current_time,
                                 const struct SeqBlock *seqblock,
                                 const struct Tracks *track,
                                 struct Notes *note,
                                 const struct Velocities *velocity1,
                                 bool first_val_has_been_sent
                              )
{

  if(note->velocities==NULL && note->velocity == note->velocity_end)
    return;

  const struct Velocities *velocity2 = velocity1==NULL ? note->velocities : NextVelocity(velocity1);

  Place p1 = velocity1==NULL ? note->l.p : velocity1->l.p;
  Place p2 = velocity2==NULL ? note->end : velocity2->l.p;
  
  int64_t time1 = get_seqblock_place_time2(seqblock, track, p1);
  int64_t time2 = get_seqblock_place_time2(seqblock, track, p2);

  if (velocity2==NULL)
    time2--; // Can not send out velocity at the same time as note_end, since note_end events has higher priority than velocity events.
#if !defined(RELEASE)
  else
    R_ASSERT(time2 >= time1);
#endif
    
  if (time2 < time1)
    return;

  int logtype1 = velocity1==NULL ? note->velocity_first_logtype : velocity1->logtype;

  if (logtype1 == LOGTYPE_HOLD){

    if (current_time == time1) {
      
      RT_scheduled_hold_velocity_do(seqtrack, current_time, seqblock, track, note, velocity1, first_val_has_been_sent);
                                    
    } else {
        
      const int num_args = 4;
    
      union SuperType args[num_args];
      args[0].const_pointer = seqblock;
      args[1].const_pointer = track;
      args[2].const_pointer = note;
      args[3].const_pointer = velocity1;
      
      SCHEDULER_add_event(seqtrack, time1, RT_scheduled_hold_velocity, &args[0], num_args, SCHEDULER_VELOCITY_PRIORITY);
    }
      
  } else {

    //int64_t time = R_MIN(time2, time1 + RADIUM_BLOCKSIZE);
    
    union SuperType args[g_num_velocities_args];
    args[0].const_pointer = seqblock;
    args[1].const_pointer = track;
    args[2].const_pointer = note;
    args[3].const_pointer = velocity1;
    args[4].int_num       = time1;
    args[5].int_num       = time2;
    args[6].int32_num     = -1;

#if DO_DEBUG
    int val1 = velocity1==NULL ? note->velocity     : velocity1->velocity;
    int val2 = velocity2==NULL ? note->velocity_end : velocity2->velocity;
    
    printf(" Scheduling Velocity. %x -> %x, %d -> %d. seqtrack->start_time: %f\n", val1, val2, (int)time1, (int)time2, seqtrack->start_time);
#endif
  
    SCHEDULER_add_event(seqtrack, time1, RT_scheduled_glide_velocity, &args[0], g_num_velocities_args, SCHEDULER_VELOCITY_PRIORITY);

  }
}


void RT_schedule_velocities_newnote(int64_t current_time,
                                    struct SeqTrack *seqtrack,
                                    const struct SeqBlock *seqblock,
                                    const struct Tracks *track,
                                    struct Notes *note)
{
  if (track->patch==NULL)
    return;

  if(note->velocities==NULL && equal_floats(note->velocity_end, 0.0))
    return;

  RT_schedule_velocity(seqtrack, current_time, seqblock, track, note, NULL, true);
}
