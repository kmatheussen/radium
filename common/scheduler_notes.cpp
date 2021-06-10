
#include "nsmtracker.h"
#include "TimeData.hpp"
#include "patch_proc.h"
#include "notes_proc.h"
#include "time_proc.h"
//#include "PEQcommon_proc.h"
#include "playerclass.h"
#include "list_proc.h"
#include "placement_proc.h"
#include "instruments_proc.h"
#include "velocities_proc.h"

#include "../audio/SoundPlugin.h"

#include "scheduler_proc.h"


static int rnd(int max){
  return rand() % max;
}


static int64_t RT_scheduled_stop_note(struct SeqTrack *seqtrack, int64_t time, union SuperType *args){
  struct Tracks *track = (struct Tracks *)args[0].pointer;
  struct Notes *note   = (struct Notes *)args[1].pointer;
  struct SeqBlock *seqblock   = (struct SeqBlock *)args[2].pointer;
  
  struct Patch *patch = track->patch;
  
  if (patch != NULL){

    note_t note2 = create_note_t3(seqblock,
                                  note->id,
                                  note->note,
                                  ATOMIC_GET(track->midi_channel)
                                  );

    note->curr_velocity_time = 0;
    note->curr_pitch_time = 0;
    RT_PATCH_stop_note(seqtrack, patch,note2,time);
    
  }

#if 1
  if (time < seqblock->t.time2) // when time==seqblock->t.time2, the playing_notes vector is cleared anyway. (and we get an assertion error when trying to remove something which is not in the vector)
    RT_SEQBLOCK_remove_playing_note(seqblock, track, note);
#endif

  return DONT_RESCHEDULE;
}

static const Place *RT_find_first_place(struct ListHeader3 *l, const Place place_start){
  while(l != NULL){
    if (p_Greater_Or_Equal(l->p, place_start))
      return &l->p;

    l = l->next;
  }

  return NULL;
}

static bool RT_find_next_note_stop_after_place(const struct Tracks *track, const Place place_start, Place &ret){
  const Place *note_place = track->notes==NULL ? NULL : (const Place *)RT_find_first_place(&track->notes->l, place_start);
  //const Place *stop_place = track->stops==NULL ? NULL : RT_find_first_place(&track->stops->l, place_start);


  bool has_stop = false;
  Place stop_place;

  {
    r::TimeData<r::Stop>::Reader reader(track->stops2);
    for(const r::Stop &stop : reader) {
      if (stop._time >= place2ratio(place_start)){
        has_stop = true;
        stop_place = ratio2place(stop._time);
        break;
      }
    }
  }

  if (note_place==NULL && has_stop==false){
    return false;
  }
      
  if (!has_stop) {
    ret = *note_place;
    return true;
  }

  if (note_place==NULL){
    ret = stop_place;
    return true;
  }

  if (p_Less_Than(*note_place, stop_place))
    ret = *note_place;
  else
    ret = stop_place;

  return true;
}

static bool RT_find_next_note_stop_after_seqblock_end(const struct SeqTrack *seqtrack, const struct SeqBlock **seqblock, int tracknum, Place &ret){
  VECTOR_FOR_EACH(const struct SeqBlock *, seqblock2, &seqtrack->seqblocks){
    if (seqblock2->block != NULL && seqblock2->t.time > (*seqblock)->t.time) {

      *seqblock = seqblock2;
      
      struct Tracks *track = (struct Tracks *)ListFindElement1_r0(&seqblock2->block->tracks->l, tracknum);

      if (track != NULL)
        if (RT_find_next_note_stop_after_place(track, seqblock2->t.start_place, ret))
          return true;
    }
    
  }END_VECTOR_FOR_EACH;

  return false;
}

static inline bool note_continues_next_seqblock(const struct SeqBlock *seqblock, const struct Notes *note){
  if (note->noend==0)
    return false;
  
  const struct Blocks *block = seqblock->block;

  if (p_Equal(seqblock->t.end_place, p_Absolute_Last_Pos(block)))
    return note_continues_next_block(block, note);
      
  return p_Greater_Than(note->l.p, seqblock->t.end_place);
}


static int64_t RT_schedule_end_note(struct SeqTrack *seqtrack,
                                    const struct SeqBlock *seqblock,
                                    const struct Tracks *track,
                                    const struct Notes *note,
                                    int64_t note_start_time
                                    )
{
  const int num_args = 3;
        
  union SuperType args[num_args];
  args[0].const_pointer = track;
  args[1].const_pointer = note;
  args[2].const_pointer = seqblock;

  SchedulerPriority priority = SCHEDULER_NOTE_OFF_PRIORITY;


  if (!note_continues_next_seqblock(seqblock, note)){
          
    int64_t time = get_seqblock_ratio_time2(seqblock, track, note->end);

    if (time < note_start_time){
      // RError("time >= note_start_time: %d - %d", (int)time, (int)note_start_time); // Can happen if seq_time < pc->start_time in schedule_event.
      time = note_start_time;
    }
          
    if (note_start_time == time)
      priority = SCHEDULER_LOWEST_NOTE_PRIORITY; // Make sure note is stopped after all other events, in case the note is stopped at the same time as it was started. (very corner case situtation)

    //printf(" Scheduling END note at %d. seqblock->time: %d\n",(int)time, (int)seqblock->time);
    SCHEDULER_add_event(seqtrack, time, RT_scheduled_stop_note, &args[0], num_args, priority);
          
    return time;
  }

        
  // The note continues playing into the next block. We need to find out when, and if, it ends.
  {
    int64_t addtime = 0;

    bool has_p = false;
    
    Place p;
    
    if(pc->playtype==PLAYBLOCK) {

      Place start_place = PlaceFirstPos;      
      has_p = RT_find_next_note_stop_after_place(track, start_place, p);

      addtime = SEQBLOCK_get_seq_duration(seqblock);

    } else {

      has_p = RT_find_next_note_stop_after_seqblock_end(seqtrack, &seqblock, track->l.num, p);
        
    }
          
    if (has_p == false)
      return -1; // I.e note never stops.

    //printf("P: %d %d %d\n",(int)p.line,(int)p.counter,(int)p.dividor);
    int64_t time = addtime + get_seqblock_place_time2(seqblock, track, p);

    if (note_start_time == time)
      priority = SCHEDULER_LOWEST_NOTE_PRIORITY; // Make sure note is stopped after all other events, in case the note is stopped at the same time as it was started. (extreme corner case situtation)

    SCHEDULER_add_event(seqtrack, time, RT_scheduled_stop_note, &args[0], num_args, priority);

    return time;
  }
}

static void RT_schedule_note(struct SeqTrack *seqtrack,
                             const struct SeqBlock *seqblock,
                             const struct Tracks *track,
                             struct Notes *note,
                             int64_t curr_time
                             );

static int64_t RT_scheduled_note(struct SeqTrack *seqtrack, int64_t time, union SuperType *args){
  struct SeqBlock *seqblock = (struct SeqBlock *)args[0].pointer;
  const struct Tracks *track = (const struct Tracks *)args[1].const_pointer;
  struct Notes *note = (struct Notes *)args[2].pointer;
  int64_t note_time = args[3].int_num;

  struct Patch *patch = track->patch;

  bool doit;  // Set this here, and not in RT_schedule_note, since note->chance might change between RT_schedule_note and RT_scheduled_note.

  if (patch==NULL){
    
    doit = true;

  } else if (note->chance==0){

    doit = patch->last_chance_decision_value;
    //printf("   track: %d. Using last decision %d\n", track->l.num, doit);

  } else {

    if (note->chance==MAX_PATCHVOICE_CHANCE)
      doit = true;
    else if (note->chance > rnd(MAX_PATCHVOICE_CHANCE))
      doit = true;
    else
      doit = false;

    patch->last_chance_decision_value = doit;
    //printf("   track: %d. Setting last decision to %d\n", track->l.num, doit);
  }
    
  if(doit && track->onoff==1 && patch!=NULL){

    int64_t sample_pos = R_MAX(0, time - note_time) / (pc->playtype==PLAYSONG ? 1.0 : ATOMIC_DOUBLE_GET(seqblock->block->reltempo));

    note->curr_velocity = TRACK_get_velocity(track,note->velocity); // The logical behavior would be to use note->velocity, but we don't have access to track in the function 'RT_PATCH_voice_volume_has_changed.
    note->curr_velocity_time = time;

    note->curr_pitch = note->note;
    note->curr_pitch_time = time;

    note->scheduler_may_send_velocity_next_block = false;
    note->scheduler_may_send_pitch_next_block = false;

    note_t note2 = create_note_t(seqblock,
                                 note->id,
                                 note->curr_pitch,
                                 note->curr_velocity,
                                 TRACK_get_pan(track),
                                 ATOMIC_GET(track->midi_channel),
                                 0,
                                 sample_pos
                                 );

    // Note: envelope volume is applied in RT_PATCH_play_note, not here. (Not quite sure why, but it's probably complicated)

    //printf("  scheduler_notes.c. Playing note at %d. Velocity: %f. Pitch: %f. Sample pos: %d\n",(int)time, note2.velocity, note2.pitch, (int)sample_pos);
    RT_PATCH_play_note(seqtrack, patch, note2, note, time);
    
    bool schedule_pitches_and_velocities = true;
    if (sample_pos>0 && (patch->instrument==get_audio_instrument() && ATOMIC_GET(((SoundPlugin*)patch->patchdata)->enable_sample_seek)==false))
      schedule_pitches_and_velocities = false; // Temporary hack to prevent CPU spike while starting to play in the middle of a block. Proper solution should be applied in the next release.
    
    if (schedule_pitches_and_velocities){
      RT_schedule_pitches_newnote(time, seqtrack, seqblock, track, note);
      //RT_schedule_velocities_newnote(time, seqtrack, seqblock, track, note);
    }

#if 1
    RT_SEQBLOCK_add_playing_note(seqblock, track, note);
#endif

    RT_schedule_end_note(seqtrack, seqblock, track, note, time);
  }

  struct Notes *next_note = NextNote(note);
  if (next_note != NULL)
    RT_schedule_note(seqtrack, seqblock, track, next_note, -1);
  
  return DONT_RESCHEDULE;
}

static void RT_schedule_note(struct SeqTrack *seqtrack,
                             const struct SeqBlock *seqblock,
                             const struct Tracks *track,
                             struct Notes *note,
                             int64_t curr_time
                             )
{
  const int num_args = 4;
  
  union SuperType args[num_args];
  args[0].const_pointer = seqblock;
  args[1].const_pointer = track;
  args[2].pointer = note;

  int64_t note_time = get_seqblock_place_time2(seqblock, track, note->l.p);
  args[3].int_num = note_time;
  
  int64_t time;
  
  if (curr_time==-1)
    time = note_time;
  else if (curr_time < note_time)
    time = note_time;
  else
    time = curr_time;

  
  if (note->chance==0){
    time++;
    args[3].int_num++;
  }
  

  //printf(" Scheduling note at %d. seqblock->time: %d, track %d\n",(int)time, (int)seqblock->time, track->l.num);
  SCHEDULER_add_event(seqtrack, time, RT_scheduled_note, &args[0], num_args, SCHEDULER_NOTE_ON_PRIORITY);
}


void RT_schedule_notes_newblock(struct SeqTrack *seqtrack,
                                const struct SeqBlock *seqblock,
                                int64_t start_time,
                                Place start_place)
{
  struct Tracks *track=seqblock->block->tracks;

  while(track!=NULL){
    R_ASSERT_NON_RELEASE(track->times!=NULL);
    //printf("scheduling track %d\n", track->l.num);
    int tracknum = track->l.num;

    bool doit = seqblock->track_is_disabled==NULL // i.e. playing block
      || tracknum >= MAX_DISABLED_SEQBLOCK_TRACKS // this seqblock track can not be disabled
      || !seqblock->track_is_disabled[tracknum]; // seqblock track is not disabled.
    
    if (doit){
      struct Notes *note=track->notes;

      Ratio start_ratio = place2ratio(start_place);
      
      while(note != NULL && note->end < start_ratio)
        note=NextNote(note);
    
      if(note!=NULL)
        RT_schedule_note(seqtrack,seqblock,track,note,start_time);
    }
    
    track=NextTrack(track);   
  }
}
