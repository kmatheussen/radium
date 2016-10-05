
#include "nsmtracker.h"
#include "patch_proc.h"
#include "notes_proc.h"
#include "time_proc.h"
//#include "PEQcommon_proc.h"
#include "playerclass.h"
#include "list_proc.h"
#include "placement_proc.h"

#include "scheduler_proc.h"


static int rnd(int max){
  return rand() % max;
}


static void RT_scheduled_stop_note(int64_t time, const union SuperType *args){
  struct Tracks *track = args[0].pointer;
  struct Notes *note   = args[1].pointer;
  
  struct Patch *patch = track->patch;
  
  if (patch != NULL){

    note_t note2 = create_note_t3(note->id,
                                  note->note,
                                  ATOMIC_GET(track->midi_channel)
                                  );
    
    RT_PATCH_stop_note(patch,note2,time);
    
  }
  
}

static int64_t RT_schedule_end_note(
                                    const struct SeqTrack *seqtrack,
                                    const struct SeqBlock *seqblock,
                                    const struct Tracks *track,
                                    const struct Notes *note,
                                    int64_t note_start_time
                                    )
{
  const struct Blocks *block = seqblock->block;
  NInt tracknum=track->l.num;

  const int num_args = 2;
        
  union SuperType args[num_args];
  args[0].const_pointer = track;
  args[1].const_pointer = note;

  int priority = SCHEDULER_NOTE_OFF_PRIORITY;


  if (!note_continues_next_block(block, note)){
          
    int64_t time = get_seqblock_place_time(seqblock, note->end);

    if (time < note_start_time){
      RError("time >= note_start_time: %d - %d", (int)time, (int)note_start_time);
      time = note_start_time;
    }
          
    if (note_start_time == time)
      priority = SCHEDULER_LOWEST_NOTE_PRIORITY; // Make sure note is stopped after all other events, in case the note is stopped at the same time as it was started. (very corner case situtation)

    printf(" Scheduling END note at %d. seqblock->time: %d\n",(int)time, (int)seqblock->time);
    SCHEDULER_add_event(time, RT_scheduled_stop_note, &args[0], num_args, priority);
          
    return time;
  }

        
  // The note continues playing into the next block. We need to find out when, and if, it ends.
  {
    const struct Tracks *next_track = NULL;

    int64_t addtime = 0;
          
    if(pc->playtype==PLAYBLOCK) {
            
      addtime = getBlockSTimeLength(block);
      next_track = track;
            
    } else {

      VECTOR_FOR_EACH(struct SeqBlock *seqblock2, &seqtrack->seqblocks){
              
        if (seqblock2->time > seqblock->time) {

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
          
    Place *p = NULL;
          
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

    int64_t time = addtime + get_seqblock_place_time(seqblock, *p);

    if (note_start_time == time)
      priority = SCHEDULER_LOWEST_NOTE_PRIORITY; // Make sure note is stopped after all other events, in case the note is stopped at the same time as it was started. (extreme corner case situtation)

    SCHEDULER_add_event(time, RT_scheduled_stop_note, &args[0], num_args, priority);

    return time;
  }
}

static void RT_schedule_note(
                             const struct SeqTrack *seqtrack,
                             const struct SeqBlock *seqblock,
                             const struct Tracks *track,
                             const struct Notes *note
                             );

static void RT_scheduled_note(int64_t time, const union SuperType *args){
  const struct SeqTrack *seqtrack = args[0].const_pointer;
  const struct SeqBlock *seqblock = args[1].const_pointer;
  const struct Tracks *track = args[2].const_pointer;
  const struct Notes *note = args[3].const_pointer;
  
  struct Patch *patch = track->patch;

  bool doit = note->chance==0x100 || note->chance > rnd(0x100); // Check this now, and not in RT_schedule_note, since note->chance might change between RT_schedule_note and RT_scheduled_note.
    
  if(doit && track->onoff==1 && patch!=NULL){
    
    note_t note2 = create_note_t(note->id,
                                 note->note,
                                 TRACK_get_velocity(track,note->velocity),
                                 TRACK_get_pan(track),
                                 ATOMIC_GET(track->midi_channel)
                                 );
    
    RT_PATCH_play_note(patch,note2,time);

    RT_schedule_pitches_newnote(time, seqtrack, seqblock, track, note);
    RT_schedule_velocities_newnote(time, seqtrack, seqblock, track, note);
    
    RT_schedule_end_note(seqtrack, seqblock, track, note, time);

    struct Notes *next_note = NextNote(note);
    if (next_note != NULL)
      RT_schedule_note(seqtrack, seqblock, track, next_note);
  }
  
}

static void RT_schedule_note(
                             const struct SeqTrack *seqtrack,
                             const struct SeqBlock *seqblock,
                             const struct Tracks *track,
                             const struct Notes *note
                             )
{
  const int num_args = 4;
  
  union SuperType args[num_args];
  args[0].const_pointer = seqtrack;
  args[1].const_pointer = seqblock;
  args[2].const_pointer = track;
  args[3].const_pointer = note;
  
  int64_t time = get_seqblock_place_time(seqblock, note->l.p);
    
  //printf(" Scheduling note at %d. seqblock->time: %d\n",(int)time, (int)seqblock->time);
  SCHEDULER_add_event(time, RT_scheduled_note, &args[0], num_args, SCHEDULER_NOTE_ON_PRIORITY);
}


void RT_schedule_notes_newblock(const struct SeqTrack *seqtrack,
                                const struct SeqBlock *seqblock,
                                int64_t start_time,
                                Place start_place)
{
  struct Tracks *track=seqblock->block->tracks;
  
  while(track!=NULL){
    
    struct Notes *note=track->notes;
    
    while(note != NULL && PlaceLessThan(&note->l.p,&start_place))
      note=NextNote(note);
        
    if(note!=NULL)
      RT_schedule_note(seqtrack,seqblock,track,note);
  
    track=NextTrack(track);   
  }

}
