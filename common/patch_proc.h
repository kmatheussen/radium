/* Copyright 2000 Kjetil S. Matheussen

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

#ifndef _RADIUM_COMMON_PATCH_PROC_H
#define _RADIUM_COMMON_PATCH_PROC_H

extern LANGSPEC void PATCH_clean_unused_patches(void);

extern LANGSPEC void PATCH_remove_from_instrument(struct Patch *patch);
extern LANGSPEC void PATCH_add_to_instrument(struct Patch *patch);
extern LANGSPEC struct Patch *PATCH_get_from_id(instrument_t id);
extern LANGSPEC instrument_t PATCH_get_new_id(void);

extern LANGSPEC int PATCH_get_effect_num(const struct Patch *patch, const char *effect_name, char **error_message);

extern LANGSPEC void PATCH_remove_current(void);
extern LANGSPEC void PATCH_set_current(struct Patch *patch);
extern LANGSPEC struct Patch *PATCH_get_current(void); // Can only return NULL during startup.

extern LANGSPEC void PATCH_handle_fx_when_theres_a_new_patch_for_track(struct Blocks *block, struct Tracks *track, struct Patch *old_patch, struct Patch *new_patch, bool *has_paused);
extern LANGSPEC void PATCH_handle_editor_and_automation_when_replacing_patch(struct Patch *old_patch, struct Patch *new_patch);
extern LANGSPEC void PATCH_handle_fxs_when_fx_names_have_changed(struct Patch *patch, bool keep_unassigned_effects);
  
extern LANGSPEC void PATCH_reset_time(void);
extern LANGSPEC void PATCH_init_voices(struct Patch *patch);
extern LANGSPEC void PATCHVOICE_set_defaults(struct PatchVoice *patchvoice, int voicenum); // note that patchvoice must be nulled out before calling.
extern LANGSPEC hash_t *PATCH_get_state(const struct Patch *patch);
extern LANGSPEC hash_t *PATCHES_get_state(const vector_t *patches, bool put_in_array);
extern LANGSPEC struct Patch *PATCH_create_from_state(hash_t *state);
extern LANGSPEC bool PATCH_make_active_audio(struct Patch *patch, const char *type_name, const char *plugin_name, hash_t *state, bool set_as_current, float x, float y);
extern LANGSPEC void PATCH_init_audio_when_loading_song(struct Patch *patch, hash_t *state);
extern LANGSPEC struct Patch *PATCH_alloc(void);
extern LANGSPEC void PATCH_set_name(struct Patch *patch, const char *name);
extern LANGSPEC struct Patch *PATCH_create_main_pipe(void);
extern LANGSPEC struct Patch *PATCH_create_audio(const char *type_name, const char *plugin_name, const char *name, hash_t *state, bool set_as_current, float x, float y);
extern LANGSPEC struct Patch *PATCH_create_midi(const char *name);
extern LANGSPEC void PATCH_make_inactive(struct Patch *patch);
extern LANGSPEC void PATCH_force_make_inactive(struct Patch *patch);
extern LANGSPEC void PATCH_replace_main_pipe(struct Patch *new_main_pipe);
extern LANGSPEC void PATCH_call_very_often(void);
extern LANGSPEC void PATCH_reset(void);
extern LANGSPEC void PATCH_init(void);

extern LANGSPEC bool PATCH_add_event_receiver(struct Patch *source, struct Patch *destination);
extern LANGSPEC void PATCH_remove_event_receiver(struct Patch *source, struct Patch *destination);

#ifdef __cplusplus
extern void PATCH_remove_all_event_receivers(struct Patch *patch, radium::PlayerLockOnlyIfNeeded &lock);
#endif

extern LANGSPEC void RT_PATCH_send_play_note_to_receivers(struct SeqTrack *seqtrack, struct Patch *patch, const note_t note,STime time);
extern LANGSPEC void RT_PATCH_send_stop_note_to_receivers(struct SeqTrack *seqtrack, struct Patch *patch, const note_t note,STime time);
extern LANGSPEC void RT_PATCH_send_change_velocity_to_receivers(struct SeqTrack *seqtrack, struct Patch *patch, const note_t note,STime time);
extern LANGSPEC void RT_PATCH_send_change_pitch_to_receivers(struct SeqTrack *seqtrack, struct Patch *patch, const note_t note,STime time);
extern LANGSPEC void RT_PATCH_send_change_pan_to_receivers(struct SeqTrack *seqtrack, struct Patch *patch, const note_t note,STime time);
extern LANGSPEC void RT_PATCH_send_raw_midi_message_to_receivers(struct SeqTrack *seqtrack, struct Patch *patch, uint32_t msg, STime time);

extern LANGSPEC bool Patch_addPlayingVoice(linked_note_t **rootp, const note_t note, struct SeqTrack *seqtrack);
extern LANGSPEC void Patch_removePlayingVoice(linked_note_t **rootp, int64_t note_id, struct SeqTrack *seqtrack, const struct SeqBlock *seqblock);

extern LANGSPEC int64_t RT_PATCH_play_note(struct SeqTrack *seqtrack, struct Patch *patch, const note_t note, struct Notes *editor_note, STime time);
extern LANGSPEC void RT_PATCH_stop_note(struct SeqTrack *seqtrack, struct Patch *patch, const note_t note,STime time);
extern LANGSPEC void RT_PATCH_change_velocity(struct SeqTrack *seqtrack, struct Patch *patch,const note_t note,STime time);
extern LANGSPEC void RT_PATCH_change_pitch(struct SeqTrack *seqtrack, struct Patch *patch,const note_t note,STime time);
extern LANGSPEC void RT_PATCH_change_pan(struct SeqTrack *seqtrack, struct Patch *patch,const note_t note,STime time);
extern LANGSPEC void RT_PATCH_send_raw_midi_message(struct SeqTrack *seqtrack, struct Patch *patch, uint32_t msg, STime time);
extern LANGSPEC void PATCH_send_raw_midi_message(struct Patch *patch, uint32_t msg);
  
extern LANGSPEC int64_t PATCH_play_note(struct Patch *patch,const note_t note);
extern LANGSPEC void PATCH_stop_note(struct Patch *patch,const note_t note);
extern LANGSPEC void PATCH_change_velocity(struct Patch *patch,const note_t note);
extern LANGSPEC void PATCH_stop_all_notes(struct Patch *patch);
extern LANGSPEC void PATCH_playNoteCurrPos(struct Tracker_Windows *window, float notenum, int64_t note_id);
extern LANGSPEC void PATCH_stopNoteCurrPos(struct Tracker_Windows *window, float notenum, int64_t note_id);
extern LANGSPEC void PATCH_change_pitch(struct Patch *patch,const note_t note);
extern LANGSPEC void PATCH_change_pan(struct Patch *patch,const note_t note);


#define c_bar_note_num  55
#define c_beat_note_num 50

extern LANGSPEC void PATCH_silence_click_instruments(void);
extern LANGSPEC void RT_play_click_note(struct SeqTrack *seqtrack, int64_t time, int note_num);


extern LANGSPEC void RT_FX_treat_fx(struct SeqTrack *seqtrack, struct FX *fx,int val,STime time,int skip, FX_when when);
//extern LANGSPEC void FX_treat_fx(struct FX *fx,int val,int skip);
extern LANGSPEC void FX_call_me_before_starting_to_play_song(struct SeqTrack *seqtrack, const struct SeqBlock *seqblock, STime start_time);
  
extern LANGSPEC void PATCH_turn_voice_on(struct Patch *patch, int voicenum);
extern LANGSPEC void PATCH_turn_voice_off(struct Patch *patch, int voicenum);
//extern LANGSPEC void PATCH_change_voice_transpose(struct Patch *patch, int voicenum, float new_tranpose);
extern LANGSPEC void RT_PATCH_voice_volume_has_changed(struct Patch *patch, int voicenum);
extern LANGSPEC void RT_PATCH_voice_pitch_has_changed(struct Patch *patch, int voicenum);
extern LANGSPEC void RT_PATCH_voice_pan_has_changed(struct Patch *patch, int voicenum);
  
extern LANGSPEC int PATCH_get_peaks(struct Patch *patch,
                                    float notenum, 
                                    int ch, 
                                    const struct Tracks *track, 
                                    int64_t start_time, int64_t end_time, 
                                    float *min_value, float *max_value
                                    );

#endif
