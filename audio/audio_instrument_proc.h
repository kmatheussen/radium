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

extern LANGSPEC void AUDIO_InitPatch(struct Patch *patch, void *patchdata);
extern LANGSPEC int AUDIO_initInstrumentPlugIn(struct Instruments *instrument);

extern LANGSPEC void DLoadAudioInstrument(void);

extern LANGSPEC bool AUDIO_is_permanent_patch(struct Patch *patch);
//extern LANGSPEC struct Patch *AUDIO_get_patch_for_plugin(struct SoundPlugin *plugin);

#endif
