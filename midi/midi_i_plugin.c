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
#include "../common/visual_proc.h"
#include "../common/playerclass.h"

//#include "midi_get_clustername_proc.h"
//#include "midi_getMidiLink_proc.h"
#include "midi_playfromstart_proc.h"
#include "midi_fx_proc.h"
#include "disk_midi_fx_proc.h"

#include <string.h>

//#include "../instrprop/Amiga_instrprop_edit_proc.h"
#include "OS_midi_proc.h"




#include "midi_i_plugin_proc.h"



struct MyMidiLinks *usedmidilinks=NULL;







/* Wrappers, all midi goes thru here. */

void OnOffNotesTrack(
	struct MyMidiLinks *mymidilink,
	int cc,
	int data1,
	int data2
){
        if(mymidilink==NULL)
          return;

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
        if(mymidilink==NULL)
          return;

	GoodPutMidi(mymidilink->midilink,(uint32_t)((cc<<24)|(data1<<16)|(data2<<8)),(uint32_t)maxbuff);
	OnOffNotesTrack(mymidilink,cc,data1,data2);
}

void MyMyPutMidi(
	struct MyMidiLinks *mymidilink,
	int cc,
	int data1,
	int data2
){
        if(mymidilink==NULL)
          return;

	PutMidi(mymidilink->midilink,(uint32_t)((cc<<24)|(data1<<16)|(data2<<8)));	
	OnOffNotesTrack(mymidilink,cc,data1,data2);
}


/****************** My put-midi function.  **************************/
/****************** Do only call from the player-task (nah).  *************/
extern PlayerClass *pc;

void MyPutMidi(
	struct MyMidiLinks *mymidilink,
//	uint32_t msg,
	int cc,
	int data1,
	int data2,
	int maxbuff,
	int skip
){
        if(mymidilink==NULL)
          return;

	struct MidiLink *midilink=mymidilink->midilink;


	if(cc<0x80 || cc>0xef || (data1&0xff)>0x7f || (data2&0xff)>0x7f){
		RError("Error. Faulty midi-message. status: %x, data1: %x, data2: %x\n",cc,data1,data2);
		return;
	}

	GoodPutMidi(midilink,(uint32_t)((cc<<24)|(data1<<16)|(data2<<8)),(uint32_t)maxbuff);

	OnOffNotesTrack(mymidilink,cc,data1,data2);
}



/******************** notes **************************/

void MIDIplaynote(int notenum, int velocity, struct Tracks *track,struct Notes *note){
	const struct PatchData *patchdata=(struct PatchData *)track->patch->patchdata;
	struct MyMidiLinks *mymidilink=patchdata->mymidilink;
	const int channel=patchdata->channel;
	int maxbuf=70;

        if(mymidilink==NULL)
          return;

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

void MIDIstopnote(int notenum,int velocity, struct Tracks *track,struct Notes *note){
	struct PatchData *patchdata=(struct PatchData *)track->patch->patchdata;
	uint32_t tem;

//	Pdebug("stop\n");

				tem=(uint32_t)(((0x80)<<24)|((notenum)<<16)|((velocity)<<8));
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

void MIDIchangevelocity(int velocity,struct Tracks *track,struct Notes *note){
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

void MIDIchangeTrackPan(int newpan,struct Tracks *track){
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

static struct PatchData *getPatchData(struct Patch *patch){
  return patch->patchdata;
}

void MIDISetPatchData(struct Patch *patch, char *key, char *value){
  if(false){

  }else if(!strcasecmp(key,"midilink")){
    getPatchData(patch)->mymidilink = MIDI_getMyMidiLink(NULL, NULL, !strcmp("",value) ? NULL : value);

  }else if(!strcasecmp(key,"channel")){
    getPatchData(patch)->channel = atoi(value);

  }else if(!strcasecmp(key,"LSB")){
    getPatchData(patch)->LSB = atoi(value);

  }else if(!strcasecmp(key,"MSB")){
    getPatchData(patch)->MSB = atoi(value);

  }else if(!strcasecmp(key,"preset")){
    getPatchData(patch)->preset = atoi(value);

  } else
    RWarning("MIDISetPatchData: Unknown key \"%s\" for midi instrument", key);
}

char *MIDIGetPatchData(struct Patch *patch, char *key){
  if(false){

  }else if(!strcasecmp(key,"midilink")){
    if(getPatchData(patch)->mymidilink==NULL) {
      RWarning("midilink has not been set");
      return "";
    }
    return getPatchData(patch)->mymidilink->name;

  }else if(!strcasecmp(key,"channel")){
    return talloc_numberstring(getPatchData(patch)->channel);

  }else if(!strcasecmp(key,"LSB")){
    return talloc_numberstring(getPatchData(patch)->LSB);

  }else if(!strcasecmp(key,"MSB")){
    return talloc_numberstring(getPatchData(patch)->MSB);

  }else if(!strcasecmp(key,"preset")){
    return talloc_numberstring(getPatchData(patch)->preset);

  } else
    RWarning("MIDIGetPatchData: Unknown key \"%s\" for midi instrument", key);

  return "";
}

void MIDIclosePatch(void){
	return;
}

int MIDIgetStandardVelocity(struct Tracks *track);
int MIDIgetMaxVelocity(struct Tracks *track);

static struct PatchData *createPatchData(void) {
  struct PatchData *patchdata=talloc(sizeof(struct PatchData));
  patchdata->MSB=-1;
  patchdata->LSB=-1;
  return patchdata;
}

void MIDI_InitPatch(struct Patch *patch) {
  patch->playnote=MIDIplaynote;
  patch->stopnote=MIDIstopnote;
  patch->changevelocity=MIDIchangevelocity;
  patch->closePatch=MIDIclosePatch;
  patch->changeTrackPan=MIDIchangeTrackPan;

  patch->patchdata = createPatchData();

  patch->minvel=0;
  patch->maxvel=MIDIgetMaxVelocity(NULL);
  patch->standardvel=MIDIgetStandardVelocity(NULL);
}

int MIDIgetPatch(
	struct Tracker_Windows *window,
	ReqType reqtype,
	struct Tracks *track,
	struct Patch *patch
){
	struct MyMidiLinks *mymidilink;
	struct PatchData *patchdata;

        MIDI_InitPatch(patch);

	mymidilink=MIDI_getMyMidiLink(window,reqtype,NULL);
	if(mymidilink==NULL) return PATCH_FAILED;

//	debug("midilink: %x, ml_parserData:%x, ml_UserData: %x\n",midilink,midilink->ml_ParserData,midilink->ml_UserData);

	patchdata=getPatchData(patch);
        patchdata->mymidilink = mymidilink;

	while(patchdata->channel==0){
		patchdata->channel=GFX_GetInteger(window,reqtype,"Channel: (1-16) ",1,16);
	}
	patchdata->channel--;

	patchdata->preset=GFX_GetInteger(window,reqtype,"Preset: (1-128) ",1,128);
	patchdata->preset--;

	return PATCH_SUCCESS;
}



/******************* instrument ***********************/

int MIDIgetStandardVelocity(struct Tracks *track){
	return 0x50;
}

int MIDIgetMaxVelocity(struct Tracks *track){
	return 127;
}



void MIDICloseInstrument(struct Instruments *instrument){
  MIDI_Delete();
}


void MIDISelectTrackInstrument(struct Tracks *track,struct Instruments *instrument){
	struct TrackInstrumentData *tid;
	tid=talloc(sizeof(struct TrackInstrumentData));
	track->instrumentdata=tid;
}

void MIDIStopPlaying(struct Instruments *instrument){
	struct Patch *patch=instrument->patches;
	struct PatchData *patchdata;
	struct MyMidiLinks *mymidilink;
	int lokke;
	int lokke2;

	while(patch!=NULL){
		patchdata=(struct PatchData *)patch->patchdata;
		mymidilink=patchdata->mymidilink;

                if(mymidilink!=NULL)
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

  if(MIDI_New(instrument)==false){
    return INSTRUMENT_FAILED;
  }

  instrument->instrumentname="MIDI instrument";
  instrument->getStandardVelocity= &MIDIgetStandardVelocity;
  instrument->getMaxVelocity= &MIDIgetMaxVelocity;
  instrument->getFX= &MIDIgetFX;
  instrument->getPatch= &MIDIgetPatch;
  instrument->CloseInstrument=MIDICloseInstrument;
  instrument->SelectTrackInstrument=MIDISelectTrackInstrument;
  instrument->StopPlaying=MIDIStopPlaying;

  instrument->CopyInstrumentData=MIDI_CopyInstrumentData;
  instrument->PlayFromStartHook=MIDIPlayFromStartHook;
  instrument->LoadFX=MIDILoadFX;

  instrument->setPatchData=MIDISetPatchData;
  instrument->getPatchData=MIDIGetPatchData;

  return INSTRUMENT_SUCCESS;
  
}






