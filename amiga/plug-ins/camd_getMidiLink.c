

#include "nsmtracker.h"
//#include <clib/camd_protos.h>
#include <midi/camd.h>
#include <midi/camdbase.h>
#include <midi/mididefs.h>
#include <pragmas/camd_pragmas.h>
#include <string.h>

#include "minicamd_proc.h"

#include "camd_i_plugin.h"

#include "camd_getMidiLink_proc.h"

extern struct MidiNode *midinode;

extern struct MyMidiLinks *usedmidilinks;

extern struct MidiLink *AddMidiLink(struct MidiNode *mi, LONG type, Tag tag, ...);

struct MyMidiLinks *CAMD_getMyMidiLink(char *clustername){
	struct MyMidiLinks *temp;
	struct MidiLink *midilink;
	int lokke,lokke2;


/* When going from mincamd to camd, some cluster name changes must be done here. */
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

		temp->next=usedmidilinks;


		temp->standardccs[0]=0x5d;
		temp->standardccs[1]=0x5b;
		temp->standardccs[2]=0x49;
		temp->standardccs[3]=0x48;
		temp->standardccs[4]=0x4a;
		temp->standardccs[5]=0x47;
		temp->standardccs[6]=0x5e;
		temp->standardccs[7]=0x1;


		temp->ccnames[0]="Chorus";
		temp->ccnames[1]="Reverb";
		temp->ccnames[2]="Attack";
		temp->ccnames[3]="Release";
		temp->ccnames[4]="CutOff";
		temp->ccnames[5]="Resonance";
		temp->ccnames[6]="Variation Depth";
		temp->ccnames[7]="Modulation";


		for(lokke=0;lokke<16;lokke++){
			temp->channelspesific[lokke].MSB=-1;
			temp->channelspesific[lokke].LSB=-1;
			temp->channelspesific[lokke].preset=-1;

			temp->channelspesific[lokke].volumeonoff=false;
			temp->channelspesific[lokke].panonoff=false;

			temp->channelspesific[lokke].volume=100;
			temp->channelspesific[lokke].pan=0x40;

			for(lokke2=0;lokke2<8;lokke2++){
				temp->channelspesific[lokke].ccvalues[lokke2]=0;
				temp->channelspesific[lokke].ccsonoff[lokke2]=false;
			}
		}

		usedmidilinks=temp;
	}

	return temp;
}



