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



extern LANGSPEC void PATCH_remove_from_instrument(struct Patch *patch);
extern LANGSPEC struct Patch *PATCH_get_from_id(int id);
extern LANGSPEC int PATCH_get_new_id(void);
extern LANGSPEC void PATCH_handle_fx_when_theres_a_new_patch_for_track(struct Tracks *track, struct Patch *old_patch, struct Patch *new_patch);
extern LANGSPEC void PATCH_reset_time(void);
extern LANGSPEC void handle_fx_when_theres_a_new_patch_for_track(struct Tracks *track, struct Patch *old_patch, struct Patch *new_patch);
extern LANGSPEC void PATCH_init_voices(struct Patch *patch);
extern LANGSPEC hash_t *PATCH_get_state(struct Patch *patch);
extern LANGSPEC bool PATCH_make_active_audio(struct Patch *patch, char *type_name, char *plugin_name, hash_t *state);
extern LANGSPEC void PATCH_init_audio_when_loading_song(struct Patch *patch, hash_t *state);
extern LANGSPEC struct Patch *PATCH_create_audio(char *type_name, char *plugin_name, const char *name, hash_t *state);
extern LANGSPEC struct Patch *PATCH_create_midi(const char *name);
extern LANGSPEC void PATCH_replace_patch_in_song(struct Patch *old_patch, struct Patch *new_patch);
extern LANGSPEC void PATCH_make_inactive(struct Patch *patch);
extern LANGSPEC void PATCH_force_make_inactive(struct Patch *patch);
extern LANGSPEC void PATCH_call_very_often(void);
extern LANGSPEC void PATCH_init(void);

extern LANGSPEC bool PATCH_add_event_receiver(struct Patch *source, struct Patch *destination);
extern LANGSPEC void PATCH_remove_event_receiver(struct Patch *source, struct Patch *destination);
extern LANGSPEC void PATCH_remove_all_event_receivers(struct Patch *patch);

extern LANGSPEC void RT_PATCH_send_play_note_to_receivers(struct Patch *patch, float notenum, int64_t note_id,float velocity,float pan,STime time);
extern LANGSPEC void RT_PATCH_send_stop_note_to_receivers(struct Patch *patch, float notenum, int64_t note_id,STime time);
extern LANGSPEC void RT_PATCH_send_change_velocity_to_receivers(struct Patch *patch, float notenum, int64_t note_id,float velocity,STime time);
extern LANGSPEC void RT_PATCH_send_change_pitch_to_receivers(struct Patch *patch, float notenum,int64_t note_id,float pitch,STime time);
extern LANGSPEC void RT_PATCH_send_raw_midi_message_to_receivers(struct Patch *patch, uint32_t msg, STime time);

extern LANGSPEC int64_t RT_PATCH_play_note(struct Patch *patch,float notenum, int64_t note_id,float velocity,float pan,STime time);
extern LANGSPEC void RT_PATCH_stop_note(struct Patch *patch,float notenum, int64_t note_id,STime time);
extern LANGSPEC void RT_PATCH_change_velocity(struct Patch *patch,float notenum, int64_t note_id,float velocity,STime time);
extern LANGSPEC void RT_PATCH_change_pitch(struct Patch *patch,float notenum, int64_t note_id,float pitch,STime time);
extern LANGSPEC void RT_PATCH_send_raw_midi_message(struct Patch *patch, uint32_t msg, STime time);

extern LANGSPEC int64_t PATCH_play_note(struct Patch *patch,float notenum, int64_t note_id,float velocity,float pan);
extern LANGSPEC void PATCH_stop_note(struct Patch *patch,float notenum, int64_t note_id);
extern LANGSPEC void PATCH_change_velocity(struct Patch *patch,float notenum, int64_t note_id,float velocity);
extern LANGSPEC void PATCH_stop_all_notes(struct Patch *patch);
extern LANGSPEC void PATCH_playNoteCurrPos(struct Tracker_Windows *window, float notenum, int64_t note_id);
extern LANGSPEC void PATCH_stopNoteCurrPos(struct Tracker_Windows *window, float notenum, int64_t note_id);

extern LANGSPEC void RT_FX_treat_fx(struct FX *fx,int val,STime time,int skip, FX_when when);
extern LANGSPEC void FX_treat_fx(struct FX *fx,int val,int skip);

extern LANGSPEC void PATCH_turn_voice_on(struct Patch *patch, int voicenum);
extern LANGSPEC void PATCH_turn_voice_off(struct Patch *patch, int voicenum);
extern LANGSPEC void PATCH_change_voice_transpose(struct Patch *patch, int voicenum, float new_tranpose);

extern LANGSPEC int PATCH_get_peaks(struct Patch *patch,
                                    float notenum, 
                                    int ch, 
                                    const struct Tracks *track, 
                                    int64_t start_time, int64_t end_time, 
                                    float *min_value, float *max_value
                                    );
