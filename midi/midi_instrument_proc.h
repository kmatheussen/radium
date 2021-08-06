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


#ifndef MIDI_I_PLUGIN_PROC_H
#define MIDI_I_PLUGIN_PROC_H

#include "midi_instrument.h"

extern LANGSPEC void MIDI_send3(struct PatchData *patchdata, const int byte1, const int byte2, const int byte3);
extern LANGSPEC void MIDI_send2(struct PatchData *patchdata, const int byte1, const int byte2);

extern LANGSPEC const char* MIDI_get_port_name(const struct PatchData *patchdata);

extern LANGSPEC bool MIDI_get_use_0x90_for_note_off(void);
extern LANGSPEC void MIDI_set_use_0x90_for_note_off(bool doit);

extern LANGSPEC int MIDI_initInstrumentPlugIn(struct Instruments *instrument);

extern LANGSPEC int MIDIgetStandardVelocity(struct Patch *patch);
//extern LANGSPEC int MIDIgetMaxVelocity(struct Patch *patch);
/*
extern LANGSPEC int MIDIgetPatch(
                                 struct Tracker_Windows *window,
                                 ReqType reqtype,
                                 const struct Tracks *track,
                                 struct Patch *patch,
                                 bool program_state_is_valid
                                 );
*/
extern LANGSPEC void MIDICloseInstrument(struct Instruments *instrument);
//extern LANGSPEC void MIDISelectTrackInstrument(const struct Tracks *track,struct Instruments *instrument);
//extern LANGSPEC void MIDIStopPlaying(struct Instruments *instrument);

//extern LANGSPEC void MIDIchangeTrackPan(int newpan,const struct Tracks *track);
//extern LANGSPEC void MIDIchangevelocity(int velocity,const struct Tracks *track,struct Notes *note,STime time);
//extern LANGSPEC void MIDIstopnote(int notenum,int velocity, const struct Tracks *track,struct Notes *note);
//extern LANGSPEC void MIDIplaynote(int notenum, int velocity, const struct Tracks *track,struct Notes *note);
//extern LANGSPEC void MIDIclosePatch(void);
extern LANGSPEC void MIDI_InitPatch(struct Patch *patch);

extern LANGSPEC void MIDI_init_track(struct Tracks *track);

extern LANGSPEC const char **MIDI_getPortNames(int *retsize, bool is_input);

extern LANGSPEC const char *MIDIrequestPortName(struct Tracker_Windows *window,ReqType reqtype, bool is_input, bool program_state_is_valid);
extern LANGSPEC struct MidiPort *MIDIgetPort(struct Tracker_Windows *window,ReqType reqtype,struct Patch *patch,const char *name,bool program_state_is_valid);
extern LANGSPEC void MIDISetPatchData(struct Patch *patch, const char *key, const char *value,bool program_state_is_valid);

extern LANGSPEC int MIDIResetAllControllers( void );
extern LANGSPEC int MIDILocalKeyboardOn( void );
extern LANGSPEC int MIDILocalKeyboardOff( void );
extern LANGSPEC int MIDIAllNotesOff( void );
extern LANGSPEC int MIDIAllSoundsOff( void );
extern LANGSPEC int MIDISetInputPort(bool program_state_is_valid);

#endif

