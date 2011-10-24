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

	struct PatchData *patchdata;
	struct MyMidiLinks *mymidilink;
	struct ChannelSpesific *cs;

	struct Patch *patch=instrument->patches;
	int channel;

	int lokke;

	while(patch!=NULL){
		patchdata=(struct PatchData *)patch->patchdata;
		channel=patchdata->channel;
		mymidilink=patchdata->mymidilink;

		cs=&mymidilink->channelspesific[channel];

		if(cs->volumeonoff){
			R_PutMidi3(mymidilink,0xb0|channel,0x7,cs->volume);
		}
		if(cs->panonoff){
			R_PutMidi3(mymidilink,0xb0|channel,0xa,cs->pan);
		}

		for(lokke=0;lokke<8;lokke++){
			if(mymidilink->standardccs[lokke]>=0 && cs->ccsonoff[lokke]){
				R_PutMidi3(mymidilink,0xb0|channel,mymidilink->standardccs[lokke],cs->ccvalues[lokke]);
			}
		}

		patch=NextPatch(patch);
	}
}


