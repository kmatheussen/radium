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
#include <proto/input.h>
#include <devices/inputevent.h>

#include <midi/camd.h>
#include "minicamd_proc.h"
#include "camd_i_plugin.h"
#include "camd_i_plugin_proc.h"
#include "../../common/notes_proc.h"
#include "../../common/blts_proc.h"

#include "camd_i_input_proc.h"

extern struct MidiLink *inputmidilink;
extern struct MidiNode *midinode;

extern struct Root *root;

bool GetMidiInput(int *note,int *velocity){
	struct Patch *patch;
	struct PatchData *patchdata;
	ULONG l0;

	MidiMsg msg;
	BOOL ret;

	for(;;){
		ret=GetMidi(midinode,&msg);
		if(ret==FALSE) return false;

		msg.mm_Status&=0xf0;

		patch=root->song->tracker_windows->wblock->wtrack->track->patch;

		if(patch!=NULL){
			patchdata=(struct PatchData *)patch->patchdata;
			l0=msg.l[0]&0xf0ffffff;
			l0|=patchdata->channel<<24;
			PutMidi(patchdata->mymidilink->midilink,l0);
		}

		if( ! root->editonoff) continue;

		if(msg.mm_Status==0x90 && msg.mm_Data2!=0){
			InsertNoteCurrPos(root->song->tracker_windows,msg.mm_Data1,
				(PeekQualifier())&IEQUALIFIER_LSHIFT?1:0
			);
		}
	}

	return true;
}


