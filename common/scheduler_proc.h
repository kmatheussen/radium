/* Copyright 2012 Kjetil S. Matheussen

This program is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public License
as published by the Free Software Foundation; either version 2
of the License, or (at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA. */




#ifndef COMMON_SCHEDULER_PROC_H
#define COMMON_SCHEDULER_PROC_H


#include "time_proc.h"

#include "sequencer_proc.h"


typedef int64_t (*SchedulerCallback)(struct SeqTrack *seqtrack, int64_t time, union SuperType *args);

#define DONT_RESCHEDULE INT64_MIN

//#define SCHEDULE_NOW (INT64_MIN/2)

// Note: lower priority number means higer priority.
enum SchedulerPriority{
  SCHEDULER_INIT_PRIORITY           = 0, // i.e. highest priority
  
  SCHEDULER_ENDBLOCK_PRIORITY       = 0,
  SCHEDULER_INIT_BLOCK_PRIORITY     = 1,
  
  SCHEDULER_SIGNATURE_PRIORITY      = 1,
  SCHEDULER_LPB_PRIORITY            = 1,
  SCHEDULER_BEAT_PRIORITY           = 1,

  SCHEDULER_FX_END_PRIORITY         = 2, // when==FX_end
  SCHEDULER_FX_PRIORITY             = 3, // when==FX_start || FX_middle
  
  SCHEDULER_RAWMIDIMESSAGE_PRIORITY = 3,
  
  SCHEDULER_NOTE_OFF_PRIORITY       = 4,
  SCHEDULER_NOTE_ON_PRIORITY        = 5,
  
  SCHEDULER_VELOCITY_PRIORITY       = 6, // Note that the end velocity is never sent out at note_end time. If it had, those velocities must have been scheduled with priority 0.
  SCHEDULER_PITCH_PRIORITY          = 6, // Same here, I think.
  SCHEDULER_PAN_PRIORITY            = 6, // Same here, I think.

  SCHEDULER_LOWEST_NOTE_PRIORITY    = 7, // Used when note off starts at the same time as note on. Must be higher than pitch and velocity priority.
};
#define SCHEDULER_NUM_PRIORITY_BITS 3


extern struct SeqTrack *g_RT_curr_scheduling_seqtrack;

extern struct SeqBlock g_block_seqtrack_seqblock; // The seqblock used when playing block.

extern LANGSPEC void SCHEDULER_add_event(struct SeqTrack *seqtrack, int64_t time_into_the_future, SchedulerCallback callback, union SuperType *args, int num_args, enum SchedulerPriority priority);

extern LANGSPEC bool SCHEDULER_called_per_block(struct SeqTrack *seqtrack, double reltime); // Returns true if there is more to play.

extern LANGSPEC int SCHEDULER_num_events(scheduler_t *scheduler);
extern LANGSPEC bool SCHEDULER_clear(scheduler_t *scheduler, float max_audio_cycle_fraction);
extern LANGSPEC bool SCHEDULER_clear_all(float max_audio_cycle_fraction);
extern LANGSPEC bool SCHEDULER_is_clear(scheduler_t *scheduler);
extern LANGSPEC bool SCHEDULER_all_is_clear(void);
extern LANGSPEC void SCHEDULER_set_seqtrack_timing(struct SeqTrack *seqtrack, double start_time, double end_time);
extern LANGSPEC void SCHEDULER_reset_all_timing(void);
extern LANGSPEC void SCHEDULER_init(void);
extern LANGSPEC scheduler_t *SCHEDULER_create(void);

static inline void put_note_into_args(union SuperType *args, const note_t note){
  args[0].float_num = note.pitch;
  args[1].int_num = note.id;
  args[2].float_num = note.velocity;
  args[3].float_num = note.pan;
  args[4].int_num = note.midi_channel << 8 | note.voicenum;
  args[5].const_pointer = note.seqblock;
  args[6].int_num = note.sample_pos;
}

static inline note_t create_note_from_args(const union SuperType *args){
  float   notenum      = args[0].float_num;
  int64_t note_id      = args[1].int_num;
  float   velocity     = args[2].float_num;
  float   pan          = args[3].float_num;
  int     channel      = (int)args[4].int_num;
  char    midi_channel = (char)(channel >> 8);
  char    voicenum     = channel & 0xff;
  const struct SeqBlock *seqblock = (struct SeqBlock*)args[5].const_pointer;
  int64_t sample_pos   = args[6].int_num;

  return create_note_t_plain(seqblock, note_id, notenum, velocity, pan, midi_channel, voicenum, sample_pos);
}


//  scheduler_notes_proc.h

extern LANGSPEC void RT_schedule_notes_newblock(struct SeqTrack *seqtrack,
                                                const struct SeqBlock *seqblock,
                                                int64_t start_time,
                                                Place start_place);

// scheduler_pitches_proc.h

extern LANGSPEC void RT_schedule_pitches_newnote(int64_t current_time,
                                                 struct SeqTrack *seqtrack,
                                                 const struct SeqBlock *seqblock,
                                                 const struct Tracks *track,
                                                 struct Notes *note);

// scheduler_velocities_proc.h

/*
extern LANGSPEC void RT_schedule_velocities_newnote(int64_t current_time,
                                                    struct SeqTrack *seqtrack,
                                                    const struct SeqBlock *seqblock,
                                                    const struct Tracks *track,
                                                    struct Notes *note);
*/

// scheduler_fxs_proc.h

extern LANGSPEC void RT_schedule_fxs_newblock(struct SeqTrack *seqtrack,
                                              const struct SeqBlock *seqblock,
                                              int64_t start_time,
                                              Place start_place);


// scheduler_seqtrack_proc.h
extern LANGSPEC void start_seqtrack_song_scheduling(const player_start_data_t *startdata, int playtype);
extern LANGSPEC void start_seqtrack_block_scheduling(struct Blocks *block, const Place place, int playtype);


// scheduler_realline_proc.h
extern LANGSPEC void RT_schedule_reallines_in_block(struct SeqTrack *seqtrack, struct SeqBlock *seqblock, const Place place); // Called from main player thread.
extern LANGSPEC void reschedule_reallines_because_num_reallines_have_changed_in_wblock(struct WBlocks *wblock); // Called from main thread.

// scheduler_lpb_proc.h
extern LANGSPEC void RT_LPB_call_when_start_playing(struct SeqTrack *seqtrack);
extern LANGSPEC void RT_schedule_LPBs_newblock(struct SeqTrack *seqtrack,
                                               const struct SeqBlock *seqblock,
                                               const Place start_place);
extern LANGSPEC double RT_LPB_get_current_BPM(const struct SeqTrack *seqtrack);
extern LANGSPEC double RT_LPB_get_beat_position(const struct SeqTrack *seqtrack);
extern LANGSPEC void RT_LPB_set_beat_position(struct SeqTrack *seqtrack, int audioblocksize);

// scheduler_Beats_proc.h
extern LANGSPEC void RT_schedule_beats_between_seqblocks(struct SeqTrack *seqtrack,
                                                         int64_t now_time,
                                                         const struct SeqBlock *seqblock1,
                                                         const struct SeqBlock *seqblock2);
extern LANGSPEC void RT_schedule_Beats_newblock(struct SeqTrack *seqtrack,
                                                const struct SeqBlock *seqblock,
                                                const struct SeqBlock *next_seqblock,
                                                const Place start_place);
extern LANGSPEC void RT_Beats_set_new_last_bar_start_value(struct SeqTrack *seqtrack, double beat_position, bool just_started_playing);
  
// scheduler_Signature_proc.h
extern LANGSPEC StaticRatio RT_Signature_get_current_Signature(const struct SeqTrack *seqtrack);
extern LANGSPEC void RT_schedule_Signature_newblock(struct SeqTrack *seqtrack,
                                                    const struct SeqBlock *seqblock,
                                                    const Place start_place);



#endif // COMMON_SCHEDULER_PROC_H

