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

#ifndef MIDI_I_PLUGIN_H
#define MIDI_I_PLUGIN_H

#include "OS_midi_proc.h"

struct MidiPort;

struct PatchData{
	struct MidiPort *midi_port;

	int channel;
	signed char LSB;
	signed char MSB;
	signed char preset;

	bool volumeonoff;
	char volume;

	bool panonoff;
	char pan;

	bool ccsonoff[8];
	const char *ccnames[8];
#if defined(__aarch64__)
        signed char cc[8];
#else
	char cc[8];
#endif
	char ccvalues[8];
};



struct MIDI_FX{
	const char *name;
	int min;
	int max;
        int effect_num; // Contains the same value as fx->effect_num;
};

struct UsedTrackMidiCCs{
	struct UsedTrackMidiCCs *next;
	struct MIDI_FX *midi_fx;
};

struct TrackInstrumentData{
	struct UsedTrackMidiCCs *usmf;
};

#define PROGRAMCHANGE_CC 1001
#define CHANNELPREASSURE_CC 1002
#define PITCH7_CC 1003
#define PITCH14_CC 1004
#define OTHER_CC 1006

#define MIDI_NUM_FX 29













// Arg 0=patchnum, 1,2,3=Midi message
#define MIDIEVENT_SENDMIDI 4

#define MIDIEVENT_SETMIDIINPUT 5

// Arg 0=on/off (1/0)
#define MIDIEVENT_USE0x90FORNOTEOFF 6

#define MIDIEVENT_NEWPATCH 7
#define MIDIEVENT_CHANGECURRENTPATCH 8

// Let name be in /tmp/Radium-ChangePatchName.txt
#define MIDIEVENT_CHANGEPATCHNAME 9

#define MIDIEVENT_SETPORT 10
#define MIDIEVENT_CHANGEPORT 11
#define MIDIEVENT_SETCHANNEL 12
#define MIDIEVENT_SETMSB 13
#define MIDIEVENT_SETLSB 14
#define MIDIEVENT_SETPRESET 15
#define MIDIEVENT_PANNINGONOFF 16
#define MIDIEVENT_SETPANNING 17
#define MIDIEVENT_SETVOLONOFF 18
#define MIDIEVENT_SETVOL 19
#define MIDIEVENT_SETSTANDARDVEL 20
#define MIDIEVENT_CC_ONOFF 21
#define MIDIEVENT_CC_VAL 22

#define MIDIEVENT_CHANGECURRENTPORT 23


#endif

