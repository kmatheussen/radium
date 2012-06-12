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

#include "midi_i_plugin.h"


void MIDIchangeTrackPan(int newpan,struct Tracks *track);
void MIDIchangevelocity(int velocity,struct Tracks *track,struct Notes *note);
void MIDIstopnote(int notenum,int velocity, struct Tracks *track,struct Notes *note);
void MIDIplaynote(int notenum, int velocity, struct Tracks *track,struct Notes *note);
void MIDIclosePatch(void);
void MIDI_InitPatch(struct Patch *patch);
struct MidiPort *MIDIgetPort(struct Tracker_Windows *window,ReqType reqtype,char *name);

void MyGoodPutMidi(
	struct MidiPort *midi_port,
	int cc,
	int data1,
	int data2,
	int maxbuff
);

void MyMyPutMidi(
	struct MidiPort *midi_port,
	int cc,
	int data1,
	int data2
);

void MyPutMidi(
	struct MidiPort *midi_port,
	int cc,
	int data1,
	int data2,
	int maxbuff,
	int skip
);




/***************** General ****************************/

#define PutMidi3(a,b,c,d,s) MyPutMidi(a,b,c,d,s,0)
//MyPutMidi(a,(ULONG)(((b)<<24)|((c)<<16)|((d)<<8)),s,0)
#define PutMidi2(a,b,c,s) MyPutMidi(a,b,c,0,s,0)
//MyPutMidi(a,(ULONG)(((b)<<24)|((c)<<16)),s,0)

#define PutMidi3_FX(a,b,c,d,s,skip) MyPutMidi(a,b,c,d,s,skip)
//MyPutMidi(a,(ULONG)(((b)<<24)|((c)<<16)|((d)<<8)),s,skip)
#define PutMidi2_FX(a,b,c,s,skip) MyPutMidi(a,b,c,0,s,skip)
//MyPutMidi(a,(ULONG)(((b)<<24)|((c)<<16)),s,skip)

//#define D_PutMidi3(a,b,c,d) GoodPutMidi(a,(ULONG)(((b)<<24)|((c)<<16)|((d)<<8)),1000)
//#define D_PutMidi2(a,b,c) GoodPutMidi(a,(ULONG)(((b)<<24)|((c)<<16)),1000)

#define D_PutMidi3(a,b,c,d) MyGoodPutMidi(a,b,c,d,1000)
#define D_PutMidi2(a,b,c) MyGoodPutMidi(a,b,c,0,1000)

//#define R_PutMidi3(a,b,c,d) PutMidi(a,(ULONG)(((b)<<24)|((c)<<16)|((d)<<8)))
//#define R_PutMidi2(a,b,c) PutMidi(a,(ULONG)(((b)<<24)|((c)<<16)))

#define R_PutMidi3(a,b,c,d) MyMyPutMidi(a,b,c,d)
#define R_PutMidi2(a,b,c) MyMyPutMidi(a,b,c,0)


#endif

