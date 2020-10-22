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


#ifndef AUDIO_AUDIO_INSTRUMENT_PROC_H
#define AUDIO_AUDIO_INSTRUMENT_PROC_H

struct SoundPlugin;

extern LANGSPEC hash_t *AUDIO_get_audio_patch_state(const struct Patch *patch);
extern LANGSPEC void AUDIO_set_patch_attributes(struct Patch *patch, void *patchdata);
extern LANGSPEC bool AUDIO_InitPatch2(struct Patch *patch, const char *type_name, const char *plugin_name, hash_t *state, bool is_loading_song, bool set_as_current, float x, float y);
extern LANGSPEC int AUDIO_initInstrumentPlugIn(struct Instruments *instrument);

extern LANGSPEC void AUDIO_stop_all_notes(struct Patch *patch);

extern LANGSPEC bool AUDIO_maybe_warn_about_automating_bus_onoff(const struct Patch *patch, const char *effect_name, const char *what, const char *info);
extern LANGSPEC void DLoadAudioInstrument(struct Root *newroot, struct Song *song);

//extern LANGSPEC struct Patch *AUDIO_get_patch_for_plugin(struct SoundPlugin *plugin);

extern LANGSPEC void AUDIO_update_all_permanent_ids(void);
  
extern LANGSPEC bool AUDIO_is_permanent_patch(struct Patch *patch);
extern LANGSPEC struct Patch *AUDIO_get_the_replacement_for_old_permanent_patch(struct Patch *old_patch);
  
#endif
