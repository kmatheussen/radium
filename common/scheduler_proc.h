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

typedef int64_t (*SchedulerCallback)(int64_t time_into_the_future, union SuperType *args);

#define DONT_RESCHEDULE INT64_MIN

enum SchedulerPriority{
  SCHEDULER_INIT_PRIORITY           = 0,
  SCHEDULER_INIT_BLOCK_PRIORITY     = 0,
  SCHEDULER_SIGNATURE_PRIORITY      = 0,
  SCHEDULER_LPB_PRIORITY            = 0,
  SCHEDULER_BEAT_PRIORITY           = 0,
  SCHEDULER_LINE_PRIORITY           = 1,
  SCHEDULER_FX_PRIORITY             = 2,
  SCHEDULER_RAWMIDIMESSAGE_PRIORITY = 2,
  SCHEDULER_NOTE_OFF_PRIORITY       = 3,
  SCHEDULER_NOTE_ON_PRIORITY        = 4,
  SCHEDULER_VELOCITY_PRIORITY       = 5, // Note that the end velocity is never sent out at note_end time. If it had, those velocities must have been scheduled with priorith 0.
  SCHEDULER_PITCH_PRIORITY          = 5, // Same here, I think.

  SCHEDULER_LOWEST_NOTE_PRIORITY    = 6 // Used when note off starts at the same time as note on. Must be higher than pitch and velocity priority.

  // priority 7 is free (set SCHEDULER_NUM_PRIORITY_BITS to 4, to get 8 new priorities)
  
  /*
  SCHEDULER_ADDORDER_DOESNT_MATTER = 1,
  SCHEDULER_ADD_BEFORE_SAME_TIME   = 1,
  SCHEDULER_ADD_AFTER_SAME_TIME    = 2,
  */
};
#define SCHEDULER_NUM_PRIORITY_BITS 3


extern LANGSPEC void SCHEDULER_add_event(int64_t time_into_the_future, SchedulerCallback callback, union SuperType *args, int num_args, enum SchedulerPriority priority);
extern LANGSPEC int SCHEDULER_called_per_block(int64_t reltime);

extern LANGSPEC int SCHEDULER_num_events(void);
extern LANGSPEC bool SCHEDULER_clear(void);
extern LANGSPEC void SCHEDULER_init(void);

static inline void put_note_into_args(union SuperType *args, const note_t note){
  args[0].float_num = note.pitch;
  args[1].int_num = note.id;
  args[2].float_num = note.velocity;
  args[3].float_num = note.pan;
  args[4].int_num = note.midi_channel;
  args[5].float_num = note.block_reltempo;
}

static inline note_t create_note_from_args(const union SuperType *args){
  float   notenum      = args[0].float_num;
  int64_t note_id      = args[1].int_num;
  float   velocity     = args[2].float_num;
  float   pan          = args[3].float_num;
  int     midi_channel = (int)args[4].int_num;
  float   block_reltempo = args[5].float_num;

  return create_note_t_plain(note_id, notenum, velocity, pan, midi_channel, block_reltempo);
}


static inline int64_t get_seqblock_place_time(const struct SeqBlock *seqblock, Place p){
  return seqblock->time + Place2STime(seqblock->block, &p);
}
                                 

//  scheduler_notes_proc.h

extern LANGSPEC void RT_schedule_notes_newblock(const struct SeqTrack *seqtrack,
                                                const struct SeqBlock *seqblock,
                                                int64_t start_time,
                                                Place start_place);

// scheduler_pitches_proc.h

extern LANGSPEC void RT_schedule_pitches_newnote(int64_t current_time,
                                                 const struct SeqTrack *seqtrack,
                                                 const struct SeqBlock *seqblock,
                                                 const struct Tracks *track,
                                                 const struct Notes *note);

// scheduler_velocities_proc.h

void RT_schedule_velocities_newnote(int64_t current_time,
                                    const struct SeqTrack *seqtrack,
                                    const struct SeqBlock *seqblock,
                                    const struct Tracks *track,
                                    const struct Notes *note);

// scheduler_fxs_proc.h

extern LANGSPEC void RT_schedule_fxs_newblock(const struct SeqTrack *seqtrack,
                                              const struct SeqBlock *seqblock,
                                              int64_t start_time,
                                              Place start_place);


// scheduler_seqtrack_proc.h
extern LANGSPEC void start_seqtrack_scheduling(int64_t global_start_time, int64_t block_time, Place place, int playtype, int playlistpos);

// scheduler_realline_proc.h
extern LANGSPEC void RT_schedule_reallines_in_block(const struct SeqBlock *seqblock, const Place place);

// scheduler_lpb_proc.h
extern LANGSPEC void RT_schedule_LPBs_newblock(struct SeqTrack *seqtrack,
                                               const struct SeqBlock *seqblock,
                                               const Place start_place);
extern LANGSPEC double RT_LPB_get_current_BPM(const struct SeqTrack *seqtrack);
extern LANGSPEC double RT_LPB_get_beat_position(const struct SeqTrack *seqtrack);
extern LANGSPEC void RT_LPB_set_beat_position(struct SeqTrack *seqtrack, int audioblocksize);

// scheduler_Beats_proc.h
extern void RT_schedule_Beats_newblock(struct SeqTrack *seqtrack,
                                       const struct SeqBlock *seqblock,
                                       const Place start_place);
extern LANGSPEC void RT_Beats_set_new_last_bar_start_value(struct SeqTrack *seqtrack, double beat_position, bool just_started_playing);
  
// scheduler_Signature_proc.h
extern LANGSPEC Ratio RT_Signature_get_current_Signature(const struct SeqTrack *seqtrack);
extern LANGSPEC void RT_schedule_Signature_newblock(struct SeqTrack *seqtrack,
                                                    const struct SeqBlock *seqblock,
                                                    const Place start_place);

  
#endif // COMMON_SCHEDULER_PROC_H


#if 0

This is for the backtrace of a bug where two stopnotes are sent out instead of one.
Two stop notes are sent out at the top of the block, when played for the second time.
  It happens if:
1. The block has one note at the beginning of the track.
2. The note doesnt end.

It could seem like it happens in InitPEQendnote, where PC_InsertElement2 might not set correct time if playlistaddpos has the value 2.
However, the code is so hairy, that I hope it doesnt have any other consequences than the above scenario.
Furthermore, this scenario is not a big problem since sending out two stop notes for only one play note should in the best case be a dummy operation,
and in the worst case it could turning off all sound if two similar notes are playing at the same time.
But the worst case situation is not an important situation to handle properly, since you shouldnt play two similar notes at the same time anyway.


(gdb) bt
#0  SCHEDULER_add_event (seq_time=89999, callback=0x45301d <scheduled_stop_note>, args=0x7ffff0a0dbf0, num_args=2, priority=SCHEDULER_ADDORDER_DOESNT_MATTER) at common/scheduler.c:101
#1  0x00000000004530fa in PE_StopNote (peq=0x16a2b80, doit=1) at common/PEQnotes.c:290
#2  0x0000000000451e51 in PlayerTask (reltime=1024) at common/player.c:111
#3  0x000000000053662e in Mixer::RT_process (this=0xf4b310, num_frames=1024) at audio/Mixer.cpp:197
#4  0x00000000005366b8 in Mixer::RT_rjack_process (num_frames=1024, arg=0xf4b310) at audio/Mixer.cpp:214
#5  0x0000003113418f94 in Jack::JackClient::CallProcessCallback (this=0xf24eb0) at ../common/JackClient.cpp:590
#6  0x0000003113418e9c in Jack::JackClient::ExecuteThread (this=0xf24eb0) at ../common/JackClient.cpp:557
#7  0x0000003113416cf7 in Jack::JackClient::Execute (this=0xf24eb0) at ../common/JackClient.cpp:542
#8  0x000000311342ccbc in Jack::JackPosixThread::ThreadHandler (arg=0xf25020) at ../posix/JackPosixThread.cpp:59
#9  0x0000003595e07d14 in start_thread () from /lib64/libpthread.so.0
#10 0x0000003595af167d in clone () from /lib64/libc.so.6


(gdb) bt
#0  SCHEDULER_add_event (seq_time=179998, callback=0x45301d <scheduled_stop_note>, args=0x7ffff0a0dbf0, num_args=2, priority=SCHEDULER_ADDORDER_DOESNT_MATTER) at common/scheduler.c:101
#1  0x00000000004530fa in PE_StopNote (peq=0x16a2b80, doit=1) at common/PEQnotes.c:290
#2  0x0000000000451e51 in PlayerTask (reltime=1024) at common/player.c:111
#3  0x000000000053662e in Mixer::RT_process (this=0xf4b310, num_frames=1024) at audio/Mixer.cpp:197
#4  0x00000000005366b8 in Mixer::RT_rjack_process (num_frames=1024, arg=0xf4b310) at audio/Mixer.cpp:214
#5  0x0000003113418f94 in Jack::JackClient::CallProcessCallback (this=0xf24eb0) at ../common/JackClient.cpp:590
#6  0x0000003113418e9c in Jack::JackClient::ExecuteThread (this=0xf24eb0) at ../common/JackClient.cpp:557
#7  0x0000003113416cf7 in Jack::JackClient::Execute (this=0xf24eb0) at ../common/JackClient.cpp:542
#8  0x000000311342ccbc in Jack::JackPosixThread::ThreadHandler (arg=0xf25020) at ../posix/JackPosixThread.cpp:59
#9  0x0000003595e07d14 in start_thread () from /lib64/libpthread.so.0
#10 0x0000003595af167d in clone () from /lib64/libc.so.6

*******************

(gdb) p *peq
$1 = {l = {next = 0x16a2af0, time = 89999}, TreatMe = 0x453072 <PE_StopNote>, playpos = 1, window = 0xe5ce00, wblock = 0x10f2dc0, block = 0x105df60, track = 0x105df00, note = 0xe39a50, realline = 1, velocity = 
    0x0, nextvelocity = 0x0, fxs = 0x0, fxnodeline = 0x0, nextfxnodeline = 0x0, time1 = 0, time2 = 0}
(gdb) bt
#0  InitPEQendnote (block=0x105df60, track=0x105df00, note=0xe39a50, playlistaddpos=1) at common/PEQnotes.c:95
#1  0x0000000000452bbe in InitPEQnote (block=0x105df60, track=0x105df00, note=0xe39a50, playlistaddpos=0) at common/PEQnotes.c:117
#2  0x0000000000452ce3 in InitPEQnotesBlock (UsedTracks=0x7fffffffc330, block=0x105df60, p=0x7fffffffc390, playlistaddpos=0) at common/PEQnotes.c:150
#3  0x0000000000452d3b in InitAllPEQnotes (block=0x105df60, p=0x7fffffffc390) at common/PEQnotes.c:171
#4  0x00000000004538ee in PlayBlock (block=0x105df60, place=0x7fffffffc390) at common/player_startstop.c:126
#5  0x0000000000453979 in PlayBlockFromStart (window=0xe5ce00) at common/player_startstop.c:145
#6  0x000000000049f03c in playBlockFromStart (windownum=-1) at api/api_play.c:31
#7  0x0000000000436573 in ER_gotKey (key=110, a=512, down=true) at common/eventreciever.c:189
#8  0x0000000000436742 in EventTreater (in_tevent=0x827000, window=0xe5ce00) at common/eventreciever.c:239
#9  0x0000000000436b53 in EventReciever (in_tevent=0x827000, window=0xe5ce00) at common/eventreciever.c:349
#10 0x00000000004a10a6 in X11Event_KeyPress (keynum=110, keystate=64, window=0xe5ce00) at X11/X11_keyboard.c:263
#11 0x00000000004a1139 in X11_KeyPress (event=0x7fffffffcb10, window=0xe5ce00) at X11/X11_keyboard.c:274

---

(gdb) p *peq
$2 = {l = {next = 0x16a29d0, time = 89999}, TreatMe = 0x453072 <PE_StopNote>, playpos = 1, window = 0x0, wblock = 0x0, block = 0x105df60, track = 0x105df00, note = 0xe39a50, realline = 0, velocity = 0x0, 
  nextvelocity = 0x0, fxs = 0x0, fxnodeline = 0x0, nextfxnodeline = 0x0, time1 = 0, time2 = 0}
(gdb) bt
#0  InitPEQendnote (block=0x105df60, track=0x105df00, note=0xe39a50, playlistaddpos=1) at common/PEQnotes.c:95
#1  0x0000000000452e5a in PEQ_FindNextNoteAddPlayPos (peq=0x16a2a60) at common/PEQnotes.c:212
#2  0x0000000000452f1b in PEQ_FindNextNote (peq=0x16a2a60) at common/PEQnotes.c:236
#3  0x000000000045301a in PE_StartNote (peq=0x16a2a60, doit=1) at common/PEQnotes.c:264
#4  0x0000000000451e51 in PlayerTask (reltime=1024) at common/player.c:111
#5  0x000000000053662e in Mixer::RT_process (this=0xf4b310, num_frames=1024) at audio/Mixer.cpp:197
#6  0x00000000005366b8 in Mixer::RT_rjack_process (num_frames=1024, arg=0xf4b310) at audio/Mixer.cpp:214

b PEQnotes.c:93
#endif
