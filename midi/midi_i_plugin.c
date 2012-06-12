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

#include "midi_i_plugin.h"

#include "midi_playfromstart_proc.h"
#include "midi_fx_proc.h"
#include "disk_midi_fx_proc.h"

#include <string.h>

//#include "../instrprop/Amiga_instrprop_edit_proc.h"
#include "OS_midi_proc.h"




#include "midi_i_plugin_proc.h"




/* Wrappers, all midi goes thru here. */

static void OnOffNotesTrack(
                            struct MidiPort *midi_port,
                            int cc,
                            int data1,
                            int data2
){
  int channel = cc & 0xf;
  int command = cc & 0xf0;

  if(command==0x80 || ((command==0x90 && data2==0))){
    if(midi_port->num_ons[channel][data1]>0)
      midi_port->num_ons[channel][data1]--;
  }else{
    if(command==0x90){
      midi_port->num_ons[channel][data1]++;
    }
  }
}

void MyGoodPutMidi(
                   struct MidiPort *midi_port,
                   int cc,
                   int data1,
                   int data2,
                   int maxbuff
){
  if(midi_port==NULL)
    return;

  GoodPutMidi(midi_port->port,(uint32_t)((cc<<24)|(data1<<16)|(data2<<8)),(uint32_t)maxbuff);
  OnOffNotesTrack(midi_port,cc,data1,data2);
}

void MyMyPutMidi(
                 struct MidiPort *midi_port,
                 int cc,
                 int data1,
                 int data2
){
        if(midi_port==NULL)
          return;

	PutMidi(midi_port->port,(uint32_t)((cc<<24)|(data1<<16)|(data2<<8)));	
	OnOffNotesTrack(midi_port,cc,data1,data2);
}


/****************** My put-midi function.  **************************/
/****************** Do only call from the player-task (nah).  *************/
extern PlayerClass *pc;

void MyPutMidi(
               struct MidiPort *midi_port,
               //	uint32_t msg,
               int cc,
               int data1,
               int data2,
               int maxbuff,
               int skip
){
        if(midi_port==NULL)
          return;

	if(cc<0x80 || cc>0xef || (data1&0xff)>0x7f || (data2&0xff)>0x7f){
		RError("Error. Faulty midi-message. status: %x, data1: %x, data2: %x\n",cc,data1,data2);
		return;
	}

	GoodPutMidi(midi_port->port,(uint32_t)((cc<<24)|(data1<<16)|(data2<<8)),(uint32_t)maxbuff);

	OnOffNotesTrack(midi_port,cc,data1,data2);
}



/******************** notes **************************/

void MIDIplaynote(int notenum,
                  int velocity,
                  struct Tracks *track,
                  struct Notes *note
){
	const struct PatchData *patchdata=(struct PatchData *)track->patch->patchdata;
        struct MidiPort *midi_port = patchdata->midi_port;
	const int channel=patchdata->channel;
	int maxbuf=70;

        if(midi_port==NULL)
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
			midi_port->LSB[channel] != patchdata->LSB ||
			midi_port->MSB[channel] != patchdata->MSB
		) &&
			patchdata->LSB!=-1
	){
		if(patchdata->MSB!=-1){
			PutMidi3(
				midi_port,
				0xb0|channel,
				32,
				patchdata->MSB,
				10
			);
			midi_port->MSB[channel] = patchdata->MSB;
		}

		PutMidi3(
			midi_port,
			0xb0|channel,
			32,
			patchdata->LSB,
			100000
		);
		midi_port->LSB[channel] = patchdata->LSB;

		PutMidi2(
			midi_port,
			0xc0|channel,
			patchdata->preset,
			100000
		);
		midi_port->preset[channel] = patchdata->preset;
		maxbuf=100000;

	}else{
		if(
			patchdata->preset!=-1 &&
			midi_port->preset[channel] != patchdata->preset
		){
			PutMidi2(
				midi_port,
				0xc0|channel,
				patchdata->preset,
				10
			);
			midi_port->preset[channel] = patchdata->preset;
			maxbuf=100000;
		}
	}

	PutMidi3(
		midi_port,
		0x90|channel,
		notenum,
		velocity,
		maxbuf
	);
}


bool useOx90ForNoteOff=false;

void MIDIstopnote(int notenum,
                  int velocity, 
                  struct Tracks *track,
                  struct Notes *note
){
  struct PatchData *patchdata=(struct PatchData *)track->patch->patchdata;

  uint32_t tem=(uint32_t)(((0x80)<<24)|((notenum)<<16)|((velocity)<<8));
  if(tem>>24!=0x80){
    Pdebug("NoteStopAI! %x, val: %x\n",tem,notenum);
    return;
  }

  PutMidi3(
           patchdata->midi_port,
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
		patchdata->midi_port,
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
		patchdata->midi_port,
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

  }else if(!strcasecmp(key,"port")){
    getPatchData(patch)->midi_port = MIDIgetPort(NULL, NULL, !strcmp("",value) ? NULL : value);

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

  }else if(!strcasecmp(key,"port")){
    if(getPatchData(patch)->midi_port==NULL) {
      RWarning("port has not been set");
      return "";
    }
    return getPatchData(patch)->midi_port->name;

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

  patchdata->standardccs[0]=0x5d;
  patchdata->standardccs[1]=0x5b;
  patchdata->standardccs[2]=0x49;
  patchdata->standardccs[3]=0x48;
  patchdata->standardccs[4]=0x4a;
  patchdata->standardccs[5]=0x47;
  patchdata->standardccs[6]=0x5e;
  patchdata->standardccs[7]=0x1;
  
  
  patchdata->ccnames[0]="Chorus";
  patchdata->ccnames[1]="Reverb";
  patchdata->ccnames[2]="Attack";
  patchdata->ccnames[3]="Release";
  patchdata->ccnames[4]="CutOff";
  patchdata->ccnames[5]="Resonance";
  patchdata->ccnames[6]="Variation Depth";
  patchdata->ccnames[7]="Modulation";
  
  
  patchdata->volumeonoff=false;
  patchdata->panonoff=false;
    
  patchdata->volume=100;
  patchdata->pan=0x40;
    
  int lokke2;
  for(lokke2=0;lokke2<8;lokke2++){
    patchdata->ccvalues[lokke2]=0;
    patchdata->ccsonoff[lokke2]=false;
  }

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

static struct MidiPort *g_midi_ports = NULL;

struct MidiPort *MIDIgetPort(struct Tracker_Windows *window,ReqType reqtype,char *name){
  if(name==NULL){
    int num_ports;
    char **portnames=MIDI_getPortNames(&num_ports);
    int sel=GFX_Menu(window,reqtype,"Select port",num_ports,portnames);
    name=portnames[sel];
  }

  struct MidiPort *midi_port = g_midi_ports;

  while (midi_port != NULL) {
    if(!strcmp(midi_port->name,name))
      return midi_port;
    midi_port = midi_port->next;
  }

  midi_port = talloc(sizeof(struct MidiPort));
  midi_port->name = talloc_strdup(name);
  midi_port->port = MIDI_getMidiPortOs(name);

  midi_port->next = g_midi_ports;
  g_midi_ports = midi_port;

  return midi_port;
}

int MIDIgetPatch(
	struct Tracker_Windows *window,
	ReqType reqtype,
	struct Tracks *track,
	struct Patch *patch
){
        MIDI_InitPatch(patch);

	struct MidiPort *midi_port = MIDIgetPort(window,reqtype,NULL);
	if(midi_port==NULL) return PATCH_FAILED;

	struct PatchData *patchdata=getPatchData(patch);
        patchdata->midi_port = midi_port;

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

  while(patch!=NULL){
    struct PatchData *patchdata=(struct PatchData *)patch->patchdata;
    struct MidiPort *midi_port = patchdata->midi_port;

    if(midi_port!=NULL) {
      int ch;
      for(ch=0;ch<16;ch++){
        int notenum;
        for(notenum=0;notenum<128;notenum++)
          while(midi_port->num_ons[ch][notenum] > 0)
            R_PutMidi3(midi_port,0x80|ch,notenum,0x00);
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






