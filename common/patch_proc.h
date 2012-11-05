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




extern LANGSPEC struct Patch *PATCH_get_from_id(int id);
extern LANGSPEC int PATCH_get_new_id(void);
extern LANGSPEC void PATCH_init_voices(struct Patch *patch);
extern LANGSPEC struct Patch *NewPatchCurrPos(int patchtype, void *patchdata, const char *name);
extern LANGSPEC void PATCH_delete(struct Patch *patch);
extern LANGSPEC void PATCH_select_patch_for_track(struct Tracker_Windows *window,struct WTracks *wtrack, bool use_popup);
//extern LANGSPEC void SelectPatch(struct Tracker_Windows *window,struct Tracks *track,bool use_popup);
extern LANGSPEC void PATCH_init(void);
extern LANGSPEC int PATCH_radiumvelocity_to_patchvelocity(struct Patch *patch,int velocity);
extern LANGSPEC int PATCH_patchvelocity_to_radiumvelocity(struct Patch *patch,int velocity);

extern LANGSPEC void RT_PATCH_play_note(struct Patch *patch,int notenum,int velocity,struct Tracks *track,STime time);
extern LANGSPEC void RT_PATCH_stop_note(struct Patch *patch,int notenum,int velocity,struct Tracks *track,STime time);
extern LANGSPEC void RT_PATCH_change_velocity(struct Patch *patch,int notenum,int velocity,struct Tracks *track,STime time);

extern LANGSPEC void PATCH_play_note(struct Patch *patch,int notenum,int velocity,struct Tracks *track);
extern LANGSPEC void PATCH_stop_note(struct Patch *patch,int notenum,int velocity,struct Tracks *track);
extern LANGSPEC void PATCH_change_velocity(struct Patch *patch,int notenum,int velocity,struct Tracks *track);
extern LANGSPEC void PATCH_stop_all_notes(struct Patch *patch);
extern LANGSPEC void PATCH_playNoteCurrPos(struct Tracker_Windows *window,int notenum);
extern LANGSPEC void PATCH_stopNoteCurrPos(struct Tracker_Windows *window,int notenum);

extern LANGSPEC void RT_FX_treat_fx(struct FX *fx,int val,struct Tracks *track,STime time,int skip);
extern LANGSPEC void FX_treat_fx(struct FX *fx,int val,struct Tracks *track,int skip);

extern LANGSPEC void PATCH_turn_voice_on(struct Patch *patch, int voicenum);
extern LANGSPEC void PATCH_turn_voice_off(struct Patch *patch, int voicenum);
extern LANGSPEC void PATCH_change_voice_transpose(struct Patch *patch, int voicenum, int new_tranpose);
