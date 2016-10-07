
#include "nsmtracker.h"
#include "patch_proc.h"

#include "scheduler_proc.h"

#define DO_DEBUG 0


static const int g_num_pitches_args = 7;


static int rnd(int max){
  return rand() % max;
}


static void RT_schedule_pitch(int64_t current_time,
                                 const struct SeqBlock *seqblock,
                                 const struct Tracks *track,
                                 const struct Notes *note,
                                 const struct Pitches *pitch1,
                                 bool first_val_has_been_sent
                                 );

static void RT_scheduled_hold_pitch_do(int64_t time,
                                       const struct SeqBlock *seqblock,
                                       const struct Tracks *track,
                                       const struct Notes *note,
                                       const struct Pitches *pitch1,
                                       bool doit
                                       )
{
  struct Patch *patch = track->patch;

  if (patch==NULL)
    return;

  if (doit) {
    
    float val = pitch1->note;

#if DO_DEBUG
    printf("  Sending HOLD pitch %f at %d\n",val,(int)time);
#endif
    
    RT_PATCH_change_pitch(patch,
                          create_note_t2(note->id, val),
                          time
                          );
  }

  const struct Pitches *pitch2 = pitch1==NULL ? note->pitches : NextPitch(pitch1);
  if (pitch2 != NULL)
    RT_schedule_pitch(time, seqblock, track, note, pitch2, false);
}

static int64_t RT_scheduled_hold_pitch(int64_t time, union SuperType *args){
  const struct SeqBlock   *seqblock  = args[0].const_pointer;
  const struct Tracks     *track     = args[1].const_pointer;
  const struct Notes      *note      = args[2].pointer;
  const struct Pitches *pitch1 = args[3].pointer;

  bool doit;
  
  if (pitch1==NULL)
    doit = true;
  else
    doit = pitch1->chance==0x100 || pitch1->chance > rnd(0x100);

  RT_scheduled_hold_pitch_do(time, seqblock, track, note, pitch1, doit);
  
  return DONT_RESCHEDULE;
}


static int64_t RT_scheduled_glide_pitch(int64_t time, union SuperType *args){
  const struct SeqBlock   *seqblock  = args[0].const_pointer;
  const struct Tracks     *track     = args[1].const_pointer;
  const struct Notes      *note      = args[2].pointer;
  const struct Pitches *pitch1 = args[3].pointer;
  int64_t                  time1     = args[4].int_num;
  int64_t                  time2     = args[5].int_num;
  bool                     doit      = args[6].bool_num;

  struct Patch *patch = track->patch;

  if (patch==NULL)
    return DONT_RESCHEDULE;
  
  R_ASSERT_NON_RELEASE(time >= time1);
  R_ASSERT_NON_RELEASE(time <= time2);
  //R_ASSERT_NON_RELEASE(time2 > time1);

  const struct Pitches *pitch2 = pitch1==NULL ? note->pitches : NextPitch(pitch1);

  if (doit) {
    float val1 = pitch1==NULL ? note->note      : pitch1->note;
    float val2 = pitch2==NULL ? note->pitch_end : pitch2->note;
    
    float val = time1==time2 ? val2 : scale(time, time1, time2, val1, val2); // We get divide by zero in scale() if time1==time2
    
#if DO_DEBUG
    printf("  Sending pitch %f at %d\n",val,(int)time);
#endif
    
    RT_PATCH_change_pitch(patch,
                          create_note_t2(note->id, val),
                          time
                          );
  }
  
  if (time >= time2) {
    
    if (pitch2 != NULL)
      RT_schedule_pitch(time, seqblock, track, note, pitch2, true);
    
    return DONT_RESCHEDULE;
    
  } else {

    return R_MIN(time2, time + RADIUM_BLOCKSIZE);
    
  }
}

static void RT_schedule_pitch(int64_t current_time,
                                 const struct SeqBlock *seqblock,
                                 const struct Tracks *track,
                                 const struct Notes *note,
                                 const struct Pitches *pitch1,
                                 bool first_val_has_been_sent
                              )
{

  if(note->pitches==NULL && note->pitch_end == 0.0) // In case note is changed while playing
    return;

  const struct Pitches *pitch2 = pitch1==NULL ? note->pitches : NextPitch(pitch1);

  Place p1 = pitch1==NULL ? note->l.p : pitch1->l.p;
  Place p2 = pitch2==NULL ? note->end : pitch2->l.p;
  
  int64_t time1 = get_seqblock_place_time(seqblock, p1);
  int64_t time2 = get_seqblock_place_time(seqblock, p2);

  if (pitch2==NULL)
    time2--; // Can not send out pitch at the same time as note_end, since note_end events has higher priority than pitch events.
#if !defined(RELEASE)
  else
    R_ASSERT(time2 >= time1);
#endif
    
  if (time2 < time1)
    return;

  int logtype1 = pitch1==NULL ? note->pitch_first_logtype : pitch1->logtype;

  if (logtype1 == LOGTYPE_HOLD){

    if (current_time == time1) {
      
      RT_scheduled_hold_pitch_do(current_time, seqblock, track, note, pitch1, !first_val_has_been_sent);
                                    
    } else {
        
      const int num_args = 4;
    
      union SuperType args[num_args];
      args[0].const_pointer = seqblock;
      args[1].const_pointer = track;
      args[2].const_pointer = note;
      args[3].const_pointer = pitch1;
      
      SCHEDULER_add_event(time1, RT_scheduled_hold_pitch, &args[0], num_args, SCHEDULER_PITCH_PRIORITY);
    }
      
  } else {

    //int64_t time = R_MIN(time2, time1 + RADIUM_BLOCKSIZE);

    bool doit;

    if (pitch1==NULL)
      doit = true;
    else
      doit = pitch1->chance==0x100 || pitch1->chance > rnd(0x100);

    union SuperType args[g_num_pitches_args];
    args[0].const_pointer = seqblock;
    args[1].const_pointer = track;
    args[2].const_pointer = note;
    args[3].const_pointer = pitch1;
    args[4].int_num       = time1;
    args[5].int_num       = time2;
    args[6].bool_num      = doit;

#if DO_DEBUG
    float val1 = pitch1==NULL ? note->note      : pitch1->note;
    float val2 = pitch2==NULL ? note->pitch_end : pitch2->note;
    
    printf(" Scheduling Pitch. %f -> %f, %d -> %d\n", val1, val2, (int)time1, (int)time2);
#endif
  
    SCHEDULER_add_event(time1, RT_scheduled_glide_pitch, &args[0], g_num_pitches_args, SCHEDULER_PITCH_PRIORITY);

  }
}


void RT_schedule_pitches_newnote(int64_t current_time,
                                    const struct SeqTrack *seqtrack,
                                    const struct SeqBlock *seqblock,
                                    const struct Tracks *track,
                                    const struct Notes *note)
{
  if (track->patch==NULL)
    return;

  if(note->pitches==NULL && note->pitch_end==0.0)
    return;

  RT_schedule_pitch(current_time, seqblock, track, note, NULL, true);
}
