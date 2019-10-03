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










#include "../common/nsmtracker.h"
#include "../common/settings_proc.h"
#include "../common/instruments_proc.h"

#include "midi_instrument.h"
#include "midi_instrument_proc.h"

#include "../common/visual_proc.h"

#include "midi_menues_proc.h"



#define APP_GetVars()                                            \
  struct Patch *patch=PATCH_get_current();                       \
  do{                                                            \
    if(patch->instrument!=get_MIDI_instrument())                 \
      return 0;                                                  \
  }while(0);




int MIDIResetAllControllers( void )
{
  printf("midiresetallcontrollers called\n");
	/* routine when (sub)item "Reset All Controllers" is selected. */

	APP_GetVars()
	struct PatchData *patchdata;
	struct MidiPort *midi_port;
	int channel;

	patchdata=(struct PatchData *)patch->patchdata;
	midi_port=patchdata->midi_port;

	for(channel=0;channel<16;channel++){
		R_PutMidi3(midi_port,0xb0|channel,121,0);
		midi_port->MSB[channel]=-1;
		midi_port->LSB[channel]=-1;
		midi_port->preset[channel]=-1;
	}

	return 0;
}

int MIDILocalKeyboardOn( void )
{
	/* routine when (sub)item "Local Keyboard On" is selected. */

	APP_GetVars()
	struct PatchData *patchdata;
	struct MidiPort *midi_port;
	int channel;

	patchdata=(struct PatchData *)patch->patchdata;
	midi_port=patchdata->midi_port;

	for(channel=0;channel<16;channel++){
		R_PutMidi3(midi_port,0xb0|channel,122,127);
	}

	return 0;
}

int MIDILocalKeyboardOff( void )
{
	/* routine when (sub)item "Local Keyboard Off" is selected. */

	APP_GetVars()
	struct PatchData *patchdata;
	struct MidiPort *midi_port;
	int channel;

	patchdata=(struct PatchData *)patch->patchdata;
	midi_port=patchdata->midi_port;

	for(channel=0;channel<16;channel++){
		R_PutMidi3(midi_port,0xb0|channel,122,0);
	}

	return 0;
}

int MIDIAllNotesOff( void )
{
	/* routine when (sub)item "All Notes Off" is selected. */

	APP_GetVars()
	struct PatchData *patchdata;
	struct MidiPort *midi_port;
	int channel;

	patchdata=(struct PatchData *)patch->patchdata;
	midi_port=patchdata->midi_port;

	for(channel=0;channel<16;channel++){
		R_PutMidi3(midi_port,0xb0|channel,123,0);
	}

	return 0;
}


int MIDIAllSoundsOff( void )
{
	/* routine when (sub)item "All Sounds Off" is selected. */

	APP_GetVars()
	struct PatchData *patchdata;
	struct MidiPort *midi_port;
	int channel;

	patchdata=(struct PatchData *)patch->patchdata;
	midi_port=patchdata->midi_port;

	for(channel=0;channel<16;channel++){
		R_PutMidi3(midi_port,0xb0|channel,120,0);
	}

	return 0;
}

extern struct MidiLink *inputmidilink;
extern struct MidiNode *midinode;
extern struct Root *root;

const char *g_input_port_name = NULL;

int MIDISetInputPort(bool program_state_is_valid){
  const char *portname = MIDIrequestPortName(root->song->tracker_windows,NULL,true,program_state_is_valid);
  if(portname!=NULL){
    MIDI_OS_AddInputPortIfNotAlreadyAdded(portname);
    SETTINGS_write_string("midi_input_port",portname);
    g_input_port_name = portname;
    PREFERENCES_update();
  }
  return 0;
}

#if 0
extern bool useOx90ForNoteOff;

int MIDIOx90ForNoteOff( void ){
	useOx90ForNoteOff=useOx90ForNoteOff==false?true:false;
	return 0;
}
#endif
