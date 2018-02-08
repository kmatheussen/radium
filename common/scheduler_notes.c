
#include "nsmtracker.h"
#include "patch_proc.h"
#include "notes_proc.h"
#include "time_proc.h"
//#include "PEQcommon_proc.h"
#include "playerclass.h"
#include "list_proc.h"
#include "placement_proc.h"
#include "instruments_proc.h"
#include "../audio/SoundPlugin.h"

#include "scheduler_proc.h"


static int rnd(int max){
  return rand() % max;
}


static int64_t RT_scheduled_stop_note(struct SeqTrack *seqtrack, int64_t time, union SuperType *args){
  struct Tracks *track = args[0].pointer;
  struct Notes *note   = args[1].pointer;
  const struct SeqBlock *seqblock   = args[2].const_pointer;
  
  struct Patch *patch = track->patch;
  
  if (patch != NULL){

    note_t note2 = create_note_t3(seqblock,
                                  note->id,
                                  note->note,
                                  ATOMIC_GET(track->midi_channel)
                                  );
    
    RT_PATCH_stop_note(seqtrack, patch,note2,time);
    
  }

  return DONT_RESCHEDULE;
}

static int64_t RT_schedule_end_note(struct SeqTrack *seqtrack,
                                    const struct SeqBlock *seqblock,
                                    const struct Tracks *track,
                                    const struct Notes *note,
                                    int64_t note_start_time
                                    )
{
  const struct Blocks *block = seqblock->block;
  NInt tracknum=track->l.num;

  const int num_args = 3;
        
  union SuperType args[num_args];
  args[0].const_pointer = track;
  args[1].const_pointer = note;
  args[2].const_pointer = seqblock;

  int priority = SCHEDULER_NOTE_OFF_PRIORITY;


  if (!note_continues_next_block(block, note)){
          
    int64_t time = get_seqblock_place_time2(seqblock, track, note->end);

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
    const struct Tracks *next_track = NULL;

    int64_t addtime = 0;

    // 1. Find track and seqblock with the next stop or note event.

    if(pc->playtype==PLAYBLOCK) {
            
      addtime = SEQBLOCK_get_seq_duration(seqblock);
      next_track = track;
            
    } else {

      VECTOR_FOR_EACH(struct SeqBlock *seqblock2, &seqtrack->seqblocks){
              
        if (seqblock2->t.time > seqblock->t.time) {

          seqblock = seqblock2;
                
          struct Tracks *track = ListFindElement1_r0(&seqblock->block->tracks->l, tracknum);
          if (track != NULL){
            if (track->notes!=NULL || track->stops!=NULL){
              next_track = track;
              break;
            }
          }
                
        }
              
      }END_VECTOR_FOR_EACH;
            
    }
          
    if (next_track == NULL)
      return -1;
          
    const Place *p = NULL;

    // 2. Find next place.

    if (next_track->notes!=NULL)
      p = &next_track->notes->l.p;
          
    if (next_track->stops!=NULL) {
      if (p==NULL)
        p = &next_track->stops->l.p;
      else
        p = PlaceMin(p, &next_track->stops->l.p);
    }
          
    if (p==NULL)
      return -1; // I.e note never stops.

    int64_t time = addtime + get_seqblock_place_time2(seqblock, track, *p);

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
  const struct SeqBlock *seqblock = args[0].const_pointer;
  const struct Tracks *track = args[1].const_pointer;
  struct Notes *note = args[2].pointer;
  int64_t note_time = args[3].int_num;

  struct Patch *patch = track->patch;

  bool doit;  // Set this here, and not in RT_schedule_note, since note->chance might change between RT_schedule_note and RT_scheduled_note.

  if (patch==NULL){
    
    doit = true;

  } else if (note->chance==0){

    doit = patch->last_chance_decision_value;
    //printf("   track: %d. Using last decision %d\n", track->l.num, doit);

  } else {

    if (note->chance==0x100)
      doit = true;
    else if (note->chance > rnd(0x100))
      doit = true;
    else
      doit = false;

    patch->last_chance_decision_value = doit;
    //printf("   track: %d. Setting last decision to %d\n", track->l.num, doit);
  }
    
  if(doit && track->onoff==1 && patch!=NULL){

    int64_t sample_pos = R_MAX(0, time - note_time) / (pc->playtype==PLAYSONG ? 1.0 : ATOMIC_DOUBLE_GET(seqblock->block->reltempo));

    note_t note2 = create_note_t(seqblock,
                                 note->id,
                                 note->note,
                                 TRACK_get_velocity(track,note->velocity),
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
      RT_schedule_velocities_newnote(time, seqtrack, seqblock, track, note);
    }
    
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
    
      while(note != NULL && p_Less_Than(note->end,start_place))
        note=NextNote(note);
    
      if(note!=NULL)
        RT_schedule_note(seqtrack,seqblock,track,note,start_time);
    }
    
    track=NextTrack(track);   
  }
}
