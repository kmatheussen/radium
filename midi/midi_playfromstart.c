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










#include "nsmtracker.h"
#include "midi_i_plugin.h"
#include "midi_i_plugin_proc.h"


#include "midi_playfromstart_proc.h"

void MIDIPlayFromStartHook(struct Instruments *instrument){

	struct Patch *patch=instrument->patches;

	while(patch!=NULL){
		struct PatchData *patchdata=(struct PatchData *)patch->patchdata;
		int channel=patchdata->channel;
		struct MidiPort *midi_port = patchdata->midi_port;

		if(patchdata->volumeonoff){
			R_PutMidi3(midi_port,0xb0|channel,0x7,patchdata->volume);
		}
		if(patchdata->panonoff){
			R_PutMidi3(midi_port,0xb0|channel,0xa,patchdata->pan);
		}

		{
			int lokke;
			for(lokke=0;lokke<8;lokke++){
				if(patchdata->cc[lokke]>=0 && patchdata->ccsonoff[lokke]){
					R_PutMidi3(midi_port,0xb0|channel,patchdata->cc[lokke],patchdata->ccvalues[lokke]);
				}
			}
		}

		patch=NextPatch(patch);
	}
}


