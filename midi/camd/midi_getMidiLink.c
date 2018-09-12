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




#include "nsmtracker.h"
//#include <clib/midi_protos.h>
#include <midi/midi.h>
#include <midi/midibase.h>
#include <midi/mididefs.h>
#include <pragmas/midi_pragmas.h>
#include <string.h>

#include "minimidi_proc.h"

#include "midi_instrument.h"

#include "midi_MyMidiLink_proc.h"


#include "midi_getMidiLink_proc.h"

extern struct MidiNode *midinode;

extern struct MyMidiLinks *usedmidilinks;

extern struct MidiLink *AddMidiLink(struct MidiNode *mi, LONG type, Tag tag, ...);

#if 0
struct MyMidiLinks *MIDI_getMyMidiLink(char *clustername){
#else
struct MyMidiLinks *MIDI_getMyMidiLink(void){
#endif

	struct MyMidiLinks *temp;
	struct MidiLink *midilink;
	int lokke,lokke2;

#if 0
/* When going from minmidi to camd, some cluster name changes must be done here. */
	if(!strcmp("mmp.0",clustername)){
		clustername="mmp.out.0";
	}
	if(!strcmp("mmp.1",clustername)){
		clustername="mmp.out.1";
	}
	if(!strcmp("mmp.2",clustername)){
		clustername="mmp.out.2";
	}
	if(!strcmp("mmp.3",clustername)){
		clustername="mmp.out.3";
	}
/* End cluster name changes. */
#enidf


	temp=usedmidilinks;
	while(temp!=NULL){
		if(!strcmp(temp->name,clustername)) break;
		temp=temp->next;
	}

	if(temp==NULL){
		fprintf(stderr,"New midilink, name: -%s-\n",clustername);
		midilink=AddMidiLink(midinode,MLTYPE_Sender,
			MLINK_Name, "radium",
			MLINK_Location, clustername,
			NULL
		);
		if(midilink==NULL){
			fprintf(stderr,"No midilink\n");
			return NULL;
		}
		temp=talloc(sizeof(struct MyMidiLinks));
		temp->midilink=midilink;
		temp->name=talloc_atomic(strlen(clustername)+1);
		strcpy(temp->name,clustername);

		MIDI_initMyMidiLink(temp);

	}

	return temp;
}



