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



#include "midi_instrument.h"


extern LANGSPEC void MIDI_treatFX(struct FX *fx,int val,const struct Tracks *track,int skip);
extern LANGSPEC void MIDI_closeFX(struct FX *fx,const struct Tracks *track);

extern LANGSPEC void MIDI_set_effect_value(const struct Patch *patch, STime time, int effect_num, float scaled_value); // MIDI version of SOUNDPLUGIN_set_effect_value

extern LANGSPEC int MIDI_get_effect_num(const struct Patch *patch, const char *effect_name, char **error_message);
  
extern LANGSPEC bool MIDISetTreatFX(struct FX *fx,struct MIDI_FX *midi_fx);

extern LANGSPEC struct FX *MIDI_createFX(const struct Tracks *track, struct Patch *patch, int effect_num);
extern LANGSPEC vector_t *MIDI_getFxNames(const struct Patch *patch);

#ifdef __cplusplus
void MIDIgetFX(struct Tracker_Windows *window,const struct Tracks *track, std::function<void(struct FX*)> callback);
#endif

extern LANGSPEC void *MIDI_CopyInstrumentData(const struct Tracks *track);
