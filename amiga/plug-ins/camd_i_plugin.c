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
#include "../visual_proc.h"
#include "../../common/playerclass.h"
#include "camd_get_clustername_proc.h"
#include "camd_getMidiLink_proc.h"
#include "camd_playfromstart_proc.h"
#include "camd_fx_proc.h"
#include "disk_camd_fx_proc.h"

#include <proto/exec.h>
#include <proto/graphics.h>

#include "minicamd_proc.h"

#include <midi/camd.h>
#include <midi/mididefs.h>

#include <string.h>
#include "../instrprop/Amiga_instrprop_edit_proc.h"


#include "camd_i_plugin.h"

#include "camd_i_plugin_proc.h"

struct MidiNode *midinode=NULL;



struct MyMidiLinks *usedmidilinks=NULL;
struct MidiLink *inputmidilink;

extern LONG inputsig;

extern ULONG waitforamigatimersig;


/***** Following code ripped from playmf.c by Dan Baker/Commodore *****/

/* Compiler glue: stub functions for camd.library */
struct MidiNode *CreateMidi(Tag tag, ...)
{	return CreateMidiA((struct TagItem *)&tag );
}

/*
BOOL SetMidiAttrs(struct MidiNode *mi, Tag tag, ...)
{	return SetMidiAttrsA(mi, (struct TagItem *)&tag );
}
*/

struct MidiLink *AddMidiLink(struct MidiNode *mi, LONG type, Tag tag, ...)
{	return AddMidiLinkA(mi, type, (struct TagItem *)&tag );
}


/***** End of code ripped from playmf.c by Dan Baker/Commodore *****/



/* Wrappers, all midi goes thru here. */

void OnOffNotesTrack(
	struct MyMidiLinks *mymidilink,
	int cc,
	int data1,
	int data2
){
	if((cc&0xf0)==0x80 || (((cc&0xf0)==0x90 && data2==0))){
		if(mymidilink->channelspesific[cc&0xf].num_ons[data1]>0)
			mymidilink->channelspesific[cc&0xf].num_ons[data1]--;
	}else{
		if((cc&0xf0)==0x90){
			mymidilink->channelspesific[cc&0xf].num_ons[data1]++;
		}
	}
}

void MyGoodPutMidi(
	struct MyMidiLinks *mymidilink,
	int cc,
	int data1,
	int data2,
	int maxbuff
){
	GoodPutMidi(mymidilink->midilink,(ULONG)((cc<<24)|(data1<<16)|(data2<<8)),(ULONG)maxbuff);
	OnOffNotesTrack(mymidilink,cc,data1,data2);
}

void MyMyPutMidi(
	struct MyMidiLinks *mymidilink,
	int cc,
	int data1,
	int data2
){
	PutMidi(mymidilink->midilink,(ULONG)((cc<<24)|(data1<<16)|(data2<<8)));	
	OnOffNotesTrack(mymidilink,cc,data1,data2);
}


/****************** My put-midi function.  **************************/
/****************** Do only call from the player-task (nah).  *************/
extern PlayerClass *pc;

void MyPutMidi(
	struct MyMidiLinks *mymidilink,
//	ULONG msg,
	int cc,
	int data1,
	int data2,
	int maxbuff,
	int skip
){
	APTR driverdata;

	struct MidiLink *midilink=mymidilink->midilink;


	if(cc<0x80 || cc>0xef || (data1&0xff)>0x7f || (data2&0xff)>0x7f){
		RError("Error. Faulty midi-message. status: %x, data1: %x, data2: %x\n",cc,data1,data2);
		return;
	}

	driverdata=GoodPutMidi(midilink,(ULONG)((cc<<24)|(data1<<16)|(data2<<8)),(ULONG)maxbuff);

	if(driverdata!=NULL){
		while(Midi2Driver(driverdata,(ULONG)((cc<<24)|(data1<<16)|(data2<<8)),(ULONG)maxbuff)==FALSE){
			if(skip==1) break;
//			Pdebug("Waiting so hard\n");
			if(!pc->isplaying){
				WaitTOF();
			}else{
				Wait(waitforamigatimersig);
			}
		}
	}

	OnOffNotesTrack(mymidilink,cc,data1,data2);

}


/******************** notes **************************/

void CAMDplaynote(int notenum, int velocity, struct Tracks *track,struct Notes *note){
	const struct PatchData *patchdata=(struct PatchData *)track->patch->patchdata;
	struct MyMidiLinks *mymidilink=patchdata->mymidilink;
	const int channel=patchdata->channel;
	int maxbuf=70;

	/* Scale the velocity to the volume set by the track.*/
	if(track->volumeonoff){
		velocity=track->volume*velocity/MAXTRACKVOL;
	}


	/* Check if the patch has other LSB/MSB/Preset than is currently
	   set for the channel.
	*/
	if(
		(
			mymidilink->channelspesific[channel].LSB != patchdata->LSB ||
			mymidilink->channelspesific[channel].MSB != patchdata->MSB
		) &&
			patchdata->LSB!=-1
	){
		if(patchdata->MSB!=-1){
			PutMidi3(
				mymidilink,
				0xb0|channel,
				32,
				patchdata->MSB,
				10
			);
			mymidilink->channelspesific[channel].MSB = patchdata->MSB;
		}

		PutMidi3(
			mymidilink,
			0xb0|channel,
			32,
			patchdata->LSB,
			100000
		);
		mymidilink->channelspesific[channel].LSB = patchdata->LSB;

		PutMidi2(
			mymidilink,
			0xc0|channel,
			patchdata->preset,
			100000
		);
		mymidilink->channelspesific[channel].preset = patchdata->preset;
		maxbuf=100000;

	}else{
		if(
			patchdata->preset!=-1 &&
			mymidilink->channelspesific[channel].preset != patchdata->preset
		){
			PutMidi2(
				mymidilink,
				0xc0|channel,
				patchdata->preset,
				10
			);
			mymidilink->channelspesific[channel].preset = patchdata->preset;
			maxbuf=100000;
		}
	}

	PutMidi3(
		mymidilink,
		0x90|channel,
		notenum,
		velocity,
		maxbuf
	);


//	Pdebug("play, notenum: %d, velocity: %d, midilink: %x\n",notenum,velocity,patchdata->midilink);
}


bool useOx90ForNoteOff=false;

void CAMDstopnote(int notenum,int velocity, struct Tracks *track,struct Notes *note){
	struct PatchData *patchdata=(struct PatchData *)track->patch->patchdata;
	ULONG tem;

//	Pdebug("stop\n");

				tem=(ULONG)(((0x80)<<24)|((notenum)<<16)|((velocity)<<8));
				if(tem>>24!=0x80){
					Pdebug("NoteStopAI! %x, val: %x\n",tem,notenum);
					return;
				}

	PutMidi3(
		patchdata->mymidilink,
		(useOx90ForNoteOff==true?0x90:0x80)|patchdata->channel,
		notenum,
		useOx90ForNoteOff==true?0:velocity,
		10
	);

}

/******************* Velocity *************************/

void CAMDchangevelocity(int velocity,struct Tracks *track,struct Notes *note){
	struct PatchData *patchdata=(struct PatchData *)track->patch->patchdata;

	PutMidi3(
		patchdata->mymidilink,
		0xa0|patchdata->channel,
		note->note,
		velocity,
		10
	);
}



/******************** patch **************************/

void CAMDchangeTrackPan(int newpan,struct Tracks *track){
	struct PatchData *patchdata=(struct PatchData *)track->patch->patchdata;
	D_PutMidi3(
		patchdata->mymidilink,
		0xb0|patchdata->channel,
		10,
		boundaries(
			0,
			(127*(newpan+MAXTRACKPAN)/(MAXTRACKPAN*2)),
			127
		)
	);
/*
	RError("newpan: %d, pan: %d\n",
		newpan,
		boundaries(
			0,
			(127*(newpan+MAXTRACKPAN)/(MAXTRACKPAN*2)),
			127
		)
	);
*/
}
/*
-1000 - 1000
0 - 127
*/

void CAMDclosePatch(void){
	return;
}

int CAMDgetPatch(
	struct Tracker_Windows *window,
	ReqType reqtype,
	struct Tracks *track,
	struct Patch *patch
){
	struct MyMidiLinks *mymidilink;
	struct PatchData *patchdata;
	char *clustername;

	clustername=CAMD_getClusterName(reqtype);

	if(clustername==NULL) return PATCH_FAILED;

	mymidilink=CAMD_getMyMidiLink(clustername);

	if(mymidilink==NULL) return PATCH_FAILED;


//	debug("midilink: %x, ml_parserData:%x, ml_UserData: %x\n",midilink,midilink->ml_ParserData,midilink->ml_UserData);

	patchdata=talloc(sizeof(struct PatchData));
	patchdata->mymidilink=mymidilink;
	while(patchdata->channel==0){
		patchdata->channel=GFX_GetInteger(window,reqtype,"Channel: (1-16) ",1,16);
	}
	patchdata->channel--;

	patchdata->preset=GFX_GetInteger(window,reqtype,"Preset: (1-128) ",1,128);
	patchdata->preset--;
	patchdata->MSB=-1;
	patchdata->LSB=-1;

	patch->patchdata=patchdata;
	patch->minvel=0;
	patch->maxvel=127;
	patch->standardvel=100;

	patch->playnote=CAMDplaynote;
	patch->stopnote=CAMDstopnote;
	patch->changevelocity=CAMDchangevelocity;
	patch->closePatch=CAMDclosePatch;
	patch->changeTrackPan=CAMDchangeTrackPan;

	return PATCH_SUCCESS;
}



/******************* instrument ***********************/

int CAMDgetStandardVelocity(struct Tracks *track){
	return 0x50;
}

int CAMDgetMaxVelocity(struct Tracks *track){
	return 127;
}



void CAMDCloseInstrument(struct Instruments *instrument){

	if(midinode!=NULL) DeleteMidi(midinode);

#ifndef USEMINICAMD
	CloseLibrary(CamdBase);
#else
	UninitMiniCamd();
#endif

	if(inputsig!=-1) FreeSignal(inputsig);
}


void CAMDSelectTrackInstrument(struct Tracks *track,struct Instruments *instrument){
	struct TrackInstrumentData *tid;
	tid=talloc(sizeof(struct TrackInstrumentData));
	track->instrumentdata=tid;
}

void CAMDStopPlaying(struct Instruments *instrument){
	struct Patch *patch=instrument->patches;
	struct PatchData *patchdata;
	struct MyMidiLinks *mymidilink;
	int lokke;
	int lokke2;

	while(patch!=NULL){
		patchdata=(struct PatchData *)patch->patchdata;
		mymidilink=patchdata->mymidilink;

		for(lokke=0;lokke<16;lokke++){
			for(lokke2=0;lokke2<128;lokke2++){
				while(mymidilink->channelspesific[lokke].num_ons[lokke2] > 0){
					R_PutMidi3(mymidilink,0x80|lokke,lokke2,0x00);
				}
			}
		}
		patch=NextPatch(patch);
	}
}



char *inlinkname=NULL;


int InitInstrumentPlugIn(struct Instruments *instrument){

	FILE *file;
	char temp[100];

#ifdef USEMINICAMD
	if(InitMiniCamd()==FALSE){
		fprintf(stderr,"Can't open camd.library\n");
		return INSTRUMENT_FAILED;
	}
#endif

	inputsig=AllocSignal(-1);

	midinode=CreateMidi(
		MIDI_Name,"radium",
		MIDI_MsgQueue, 5000L,
		MIDI_SignalTask,FindTask(0),
		MIDI_RecvSignal,inputsig,
		NULL
	);

	if(midinode==NULL){
		fprintf(stderr,"Can't get midinode from CAMD\n");
#ifndef USEMINICAMD
		CloseLibrary(CamdBase);
#else
		UninitMiniCamd();
#endif
		return INSTRUMENT_FAILED;
	}

	inlinkname=talloc_atomic(100);

	file=fopen("radium:inlinkname.txt","r");
	if(file==NULL){
		sprintf(inlinkname,"%s","mmp.in.0");
		file=fopen("radium:inlinkname.txt","w");
		fprintf(file,"%s",inlinkname);
		fclose(file);
	}else{
		fgets(temp,90,file);
		sprintf(inlinkname,"%s",temp);
		fclose(file);
	}

	inputmidilink=AddMidiLink(midinode,MLTYPE_Receiver,
		MLINK_Name, "sortoftracker.in",
		MLINK_Location,inlinkname,
		MLINK_EventMask,CMF_Channel,
		NULL
	);

	instrument->instrumentname="CAMD instrument";
	instrument->getStandardVelocity= &CAMDgetStandardVelocity;
	instrument->getMaxVelocity= &CAMDgetMaxVelocity;
	instrument->getFX= &CAMDgetFX;
	instrument->getPatch= &CAMDgetPatch;
	instrument->CloseInstrument=CAMDCloseInstrument;
	instrument->SelectTrackInstrument=CAMDSelectTrackInstrument;
	instrument->StopPlaying=CAMDStopPlaying;
	instrument->PP_Update=CAMDPP_Update;
	instrument->CopyInstrumentData=CAMD_CopyInstrumentData;
	instrument->PlayFromStartHook=CAMDPlayFromStartHook;
	instrument->LoadFX=CAMDLoadFX;
	return INSTRUMENT_SUCCESS;

}






