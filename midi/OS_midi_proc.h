/* Copyright 2003 Kjetil S. Matheussen

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



// OS_midi_spesific.h contains the definition of struct MidiLink.
#include <OS_midi_spesific.h>



// DeleteMidi(midinode);
//	CloseLibrary(CamdBase);
//	UninitMiniCamd();
//	if(inputsig!=-1) FreeSignal(inputsig);
extern void MIDI_Delete(void);


extern struct MyMidiLinks *MIDI_getMyMidiLink(struct Tracker_Windows *window,ReqType reqtype,char *name);

#if 0
extern struct MyMidiLinks *MIDI_getMyMidiLink(void);
// NOTE! When name==null, name is really "out.0" for camd.
extern struct MyMidiLinks *MIDI_GetMyMidiLink(char *name);
#endif


//GoodPutMidi(mymidilink->midilink,(ULONG)((cc<<24)|(data1<<16)|(data2<<8)),(ULONG)maxbuff);

extern void GoodPutMidi(struct MidiLink *midilink,
		   uint32_t msg,
		   uint32_t maxbuff
		   );

extern void PutMidi(struct MidiLink *midilink,
	       uint32_t msg
	       );

extern bool MIDI_New(struct Instruments *instrument);



