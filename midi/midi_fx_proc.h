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







#include "midi_i_plugin.h"


extern void MIDI_treatFX(struct FX *fx,int val,const struct Tracks *track,int skip);
extern void MIDI_closeFX(struct FX *fx,const struct Tracks *track);

extern LANGSPEC void MIDI_set_effect_value(const struct Patch *patch, STime time, int effect_num, float scaled_value); // MIDI version of SOUNDPLUGIN_set_effect_value
  
extern bool MIDISetTreatFX(struct FX *fx,struct MIDI_FX *midi_fx);

struct FX *MIDI_createFX(const struct Tracks *track, struct Patch *patch, int effect_num);
vector_t *MIDI_getFxNames(const struct Patch *patch);

extern int MIDIgetFX(struct Tracker_Windows *window,const struct Tracks *track,struct FX *fx);

extern void *MIDI_CopyInstrumentData(const struct Tracks *track);




