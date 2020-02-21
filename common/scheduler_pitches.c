
#include "nsmtracker.h"
#include "patch_proc.h"

#include "scheduler_proc.h"

#define DO_DEBUG 0


#define g_num_pitches_args 7


static int rnd(int max){
  return rand() % max;
}


static void RT_schedule_pitch(struct SeqTrack *seqtrack,
                              int64_t current_time,
                              const struct SeqBlock *seqblock,
                              const struct Tracks *track,
                              struct Notes *note,
                              const struct Pitches *pitch1,
                              bool first_val_has_been_sent
                              );

static void RT_scheduled_hold_pitch_do(struct SeqTrack *seqtrack,
                                       int64_t time,
                                       const struct SeqBlock *seqblock,
                                       const struct Tracks *track,
                                       struct Notes *note,
                                       const struct Pitches *pitch1,
                                       bool doit
                                       )
{
  struct Patch *patch = track->patch;

  if (patch!=NULL && pitch1!=NULL && doit) {
    
    float val = pitch1->note;

#if DO_DEBUG
    printf("  Sending HOLD pitch %f at %d\n",val,(int)time);
#endif

    note->curr_pitch = val;
    note->curr_pitch_time = time;
    
    RT_PATCH_change_pitch(seqtrack,
                          patch,
                          create_note_t2(seqblock, note->id, note->curr_pitch),
                          time
                          );
  }

  const struct Pitches *pitch2 = pitch1==NULL ? note->pitches : NextPitch(pitch1);
  if (pitch2 != NULL)
    RT_schedule_pitch(seqtrack, time, seqblock, track, note, pitch2, false);
}

static bool get_doit(struct Patch *patch, const struct Pitches *pitch){
  bool doit;

  if (pitch==NULL || patch==NULL) {

    doit = true; // Not quite sure what's happening here.

  } else if (pitch->chance==0){

    doit = patch->last_chance_decision_value;

  } else {

    if (pitch->chance==0x100)
      doit = true;
    else if (pitch->chance > rnd(0x100))
      doit = true;
    else
      doit = false;

    patch->last_chance_decision_value = doit;

  }

  return doit;
}

static int64_t RT_scheduled_hold_pitch(struct SeqTrack *seqtrack, int64_t time, union SuperType *args){
  const struct SeqBlock   *seqblock  = args[0].const_pointer;
  const struct Tracks     *track     = args[1].const_pointer;
  struct Notes      *note      = args[2].pointer;
  const struct Pitches    *pitch1    = args[3].pointer;

  struct Patch *patch = track->patch;

  bool doit = get_doit(patch, pitch1);

  RT_scheduled_hold_pitch_do(seqtrack, time, seqblock, track, note, pitch1, doit);
  
  return DONT_RESCHEDULE;
}


static int64_t RT_scheduled_glide_pitch(struct SeqTrack *seqtrack, int64_t time, union SuperType *args){
  const struct SeqBlock   *seqblock  = args[0].const_pointer;
  const struct Tracks     *track     = args[1].const_pointer;
  struct Notes      *note      = args[2].pointer;
  const struct Pitches *pitch1 = args[3].pointer;
  int64_t                  time1     = args[4].int_num;
  int64_t                  time2     = args[5].int_num;
  bool                     doit      = args[6].bool_num;

  struct Patch *patch = track->patch;

  if (patch==NULL)
    return DONT_RESCHEDULE;

#if !defined(RELEASE)
  if (time < time1 || time>time2)
    RError("RT_scheduled_glide_pitch: time: %d, time1: %d, time2: %d", (int)time, (int)time1, (int)time2);
#endif
  
  if (time < time1)
    time = time1;
  if (time > time2)
    time = time2;

  const struct Pitches *pitch2 = pitch1==NULL ? note->pitches : NextPitch(pitch1);

  if (doit) {
    float val1 = pitch1==NULL ? note->note      : pitch1->note;
    float val2 = pitch2==NULL ? note->pitch_end : pitch2->note;
    
    float val = time1==time2 ? val2 : scale(time, time1, time2, val1, val2); // We get divide by zero in scale() if time1==time2
    
#if DO_DEBUG
    printf("  Sending pitch %f at %d\n",val,(int)time);
#endif

    if (time==time1 || !equal_floats(val1, val2) || note->scheduler_must_send_pitch_next_block) {

      if (note->scheduler_must_send_pitch_next_block)
        note->scheduler_must_send_pitch_next_block = false;

      note->curr_pitch = val;
      note->curr_pitch_time = time;

      RT_PATCH_change_pitch(seqtrack,
                            patch,
                            create_note_t2(seqblock, note->id, note->curr_pitch),
                            time
                            );
    }
  }
  
  if (time >= time2) {
    
    if (pitch2 != NULL)
      RT_schedule_pitch(seqtrack, time, seqblock, track, note, pitch2, true);

    note->scheduler_may_send_pitch_next_block = false;
    
    return DONT_RESCHEDULE;
    
  } else {

    if (doit)
      note->scheduler_may_send_pitch_next_block = true;
    
    return R_MIN(time2, time + RADIUM_BLOCKSIZE);
    
  }
}

static void RT_schedule_pitch(struct SeqTrack *seqtrack,
                              int64_t current_time,
                              const struct SeqBlock *seqblock,
                              const struct Tracks *track,
                              struct Notes *note,
                              const struct Pitches *pitch1,
                              bool first_val_has_been_sent
                              )
{

  if(note->pitches==NULL && equal_floats(note->pitch_end, 0.0)) // In case note is changed while playing
    return;

  const struct Pitches *pitch2 = pitch1==NULL ? note->pitches : NextPitch(pitch1);

  Place p1 = pitch1==NULL ? note->l.p : pitch1->l.p;
  Place p2 = pitch2==NULL ? note->end : pitch2->l.p;
  
  int64_t time1 = get_seqblock_place_time2(seqblock, track, p1);
  int64_t time2 = get_seqblock_place_time2(seqblock, track, p2);

  if (pitch2==NULL)
    time2--; // Can not send out pitch at the same time as note_end, since note_end events has higher priority than pitch events.
#if !defined(RELEASE)
  else
    R_ASSERT(time2 >= time1);
#endif

  if (pitch1!=NULL && pitch1->chance==0)
    time1++;
    
  if (pitch2!=NULL && pitch2->chance==0)
    time2++;
    
  if (time2 < time1)
    return;

  int logtype1 = pitch1==NULL ? note->pitch_first_logtype : pitch1->logtype;

  if (logtype1 == LOGTYPE_HOLD){

    if (current_time == time1) {
      
      RT_scheduled_hold_pitch_do(seqtrack, current_time, seqblock, track, note, pitch1, !first_val_has_been_sent);
                                    
    } else {
        
      const int num_args = 4;
    
      union SuperType args[num_args];
      args[0].const_pointer = seqblock;
      args[1].const_pointer = track;
      args[2].pointer = note;
      args[3].const_pointer = pitch1;
      
      SCHEDULER_add_event(seqtrack, time1, RT_scheduled_hold_pitch, &args[0], num_args, SCHEDULER_PITCH_PRIORITY);
    }
      
  } else {

    //int64_t time = R_MIN(time2, time1 + RADIUM_BLOCKSIZE);

    bool doit = get_doit(track->patch, pitch1);

    union SuperType args[g_num_pitches_args];
    args[0].const_pointer = seqblock;
    args[1].const_pointer = track;
    args[2].pointer = note;
    args[3].const_pointer = pitch1;
    args[4].int_num       = time1;
    args[5].int_num       = time2;
    args[6].bool_num      = doit;

#if DO_DEBUG
    float val1 = pitch1==NULL ? note->note      : pitch1->note;
    float val2 = pitch2==NULL ? note->pitch_end : pitch2->note;
    
    printf(" Scheduling Pitch. %f -> %f, %d -> %d\n", val1, val2, (int)time1, (int)time2);
#endif
  
    SCHEDULER_add_event(seqtrack, time1, RT_scheduled_glide_pitch, &args[0], g_num_pitches_args, SCHEDULER_PITCH_PRIORITY);

  }
}


void RT_schedule_pitches_newnote(int64_t current_time,
                                 struct SeqTrack *seqtrack,
                                 const struct SeqBlock *seqblock,
                                 const struct Tracks *track,
                                 struct Notes *note)
{
  if (track->patch==NULL)
    return;

  if(note->pitches==NULL && equal_floats(note->pitch_end, 0.0))
    return;

  RT_schedule_pitch(seqtrack, current_time, seqblock, track, note, NULL, true);
}
