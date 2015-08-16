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
#include "../common/vector_proc.h"
#include "midi_i_plugin.h"
#include "midi_i_plugin_proc.h"
#include "../common/visual_proc.h"
#include "../common/instruments_proc.h"
#include <string.h>
#include "disk_midi_fx_proc.h"

#include "midi_fx_proc.h"


static struct MIDI_FX MIDI_fxs[MIDI_NUM_FX]={
  {"Program Change",0,127,PROGRAMCHANGE_CC},

	{"-----------------------",0,0,-1},

	{"Channel Preassure",0,127,CHANNELPREASSURE_CC},

	{"-----------------------",0,0,-1},

	{"Pitch7",-64,63,PITCH7_CC},
	{"Pitch14",-0x2000,0x1fff,PITCH14_CC},

	{"-----------------------",0,0,-1},

	{"Modulation7.",0,127,1},
	{"Modulation14.",0,0x3fff,129},

	{"-----------------------",0,0,-1},

	{"Portamento7.",0,127,5},
	{"Portamento14.",0,0x3fff,5},

	{"-----------------------",0,0,-1},

	{"Volume7.",0,127,7},
	{"Volume14.",0,0x3fff,135},

	{"-----------------------",0,0,-1},

	{"Pan7.",-64,63,10},
	{"Pan14.",-0x2000,0x1fff,138},

	{"-----------------------",0,0,-1},

	{"Chorus.",0,127,93},
	{"Reverb.",0,127,91},
	{"Attack.",0,127,73},
	{"Release.",0,127,72},
	{"CutOff",0,127,74},
	{"Resonance",0,127,71},
	{"Tremolo",0,127,92},
	{"Phaser",0,127,95},

	{"-----------------------",0,0,-1},
	{"Other CC",0,0,OTHER_CC}
};

static char *midi_fxs_fullnames[MIDI_NUM_FX]={
	"Program Change",

	"-----------------------",

	"Channel Preassure",

	"-----------------------",

	"Pitch7",
	"Pitch14",

	"-----------------------",

	"Modulation7.  CC> 1",
	"Modulation14. CC> 1-34",

	"-----------------------",

	"Portamento7.  CC> 5",
	"Portamento14. CC> 5-37",

	"-----------------------",

	"Volume7.      CC> 7",
	"Volume14.     CC> 7-39",

	"-----------------------",

	"Pan7.         CC> 10",
	"Pan14.        CC> 10-42",

	"-----------------------",

	"Chorus.       CC> 93",
	"Reverb.       CC> 91",
	"Attack.       CC> 73",
	"Release.      CC> 72",
	"CutOff        CC> 74",
	"Resonance     CC> 71",
	"Tremolo       CC> 92",
	"Phaser        CC> 95",

	"-----------------------",
	"Other CC",
};



/***************** FX **********************************/


static void MIDI_treatFX_CC7(struct FX *fx,int val,STime time,int skip, FX_when when){
    struct Patch *patch = fx->patch;
    R_ASSERT_RETURN_IF_FALSE(patch!=NULL);
    
  //struct MIDI_FX *midi_fx=(struct MIDI_FX *)fx->fxdata;
	struct PatchData *patchdata;
	struct MidiPort *midi_port;
	int channel;

        R_ASSERT_RETURN_IF_FALSE(patch->instrument==get_MIDI_instrument());
                                 
	patchdata=(struct PatchData *)patch->patchdata;
        if (patchdata==NULL)
          return;
        
	midi_port=patchdata->midi_port;
	channel=patchdata->channel;

	PutMidi3_FX(midi_port,0xb0|channel,fx->effect_num,val,time,10,skip);
}


static void MIDI_treatFX_CC14(struct FX *fx,int val,STime time,int skip, FX_when when){
    struct Patch *patch = fx->patch;
    R_ASSERT_RETURN_IF_FALSE(patch!=NULL);

    //struct MIDI_FX *midi_fx=(struct MIDI_FX *)fx->fxdata;
	struct PatchData *patchdata;
	struct MidiPort *midi_port;
	int channel;

        R_ASSERT_RETURN_IF_FALSE(patch->instrument==get_MIDI_instrument());
        
	patchdata=(struct PatchData *)patch->patchdata;
        if (patchdata==NULL) // There is actually no way to delete a midi instrument, but it feels right to have this test anyway.
          return;
        
	midi_port=patchdata->midi_port;
	channel=patchdata->channel;

	PutMidi3_FX(midi_port,0xb0|channel,fx->effect_num-128,val>>7,time,10,skip);
	PutMidi3_FX(midi_port,0xb0|channel,fx->effect_num-128+32,val&127,time,10,skip);
}

static void MIDI_treatFX_Pan7(struct FX *fx,int val,STime time,int skip, FX_when when){
  struct Patch *patch = fx->patch;
  R_ASSERT_RETURN_IF_FALSE(patch!=NULL);

	struct PatchData *patchdata;
	struct MidiPort *midi_port;
	int channel;

        R_ASSERT_RETURN_IF_FALSE(patch->instrument==get_MIDI_instrument());
                
	patchdata=(struct PatchData *)patch->patchdata;
        if (patchdata==NULL)
          return;
        
	midi_port=patchdata->midi_port;
	channel=patchdata->channel;

	PutMidi3_FX(midi_port,0xb0|channel,10,val+0x40,time,10,skip);
}


static void MIDI_treatFX_Pan14(struct FX *fx,int val,STime time,int skip, FX_when when){
  struct Patch *patch = fx->patch;
  R_ASSERT_RETURN_IF_FALSE(patch!=NULL);

	struct PatchData *patchdata;
	struct MidiPort *midi_port;
	int channel;

        R_ASSERT_RETURN_IF_FALSE(patch->instrument==get_MIDI_instrument());
                
	patchdata=(struct PatchData *)patch->patchdata;
        if (patchdata==NULL)
          return;
        
	midi_port=patchdata->midi_port;
	channel=patchdata->channel;

	val+=0x2000;

	PutMidi3_FX(midi_port,0xb0|channel,10,val>>7,time,10,skip);
	PutMidi3_FX(midi_port,0xb0|channel,42,val&127,time,10,skip);
}

static void MIDI_treatFX_ProgramChange(struct FX *fx,int val,STime time,int skip, FX_when when){
  struct Patch *patch = fx->patch;
  R_ASSERT_RETURN_IF_FALSE(patch!=NULL);

	struct PatchData *patchdata;
	struct MidiPort *midi_port;
	int channel;

        R_ASSERT_RETURN_IF_FALSE(patch->instrument==get_MIDI_instrument());
        
	patchdata=(struct PatchData *)patch->patchdata;
        if (patchdata==NULL)
          return;
        
	midi_port=patchdata->midi_port;
	channel=patchdata->channel;

	PutMidi2(midi_port,0xc0|channel,val,time,50);
}

static void MIDI_treatFX_ChannelPreassure(struct FX *fx,int val,STime time,int skip, FX_when when){
  struct Patch *patch = fx->patch;
  R_ASSERT_RETURN_IF_FALSE(patch!=NULL);

	struct PatchData *patchdata;
	struct MidiPort *midi_port;
	int channel;

        R_ASSERT_RETURN_IF_FALSE(patch->instrument==get_MIDI_instrument());
        
	patchdata=(struct PatchData *)patch->patchdata;
        if (patchdata==NULL)
          return;
        
	midi_port=patchdata->midi_port;
	channel=patchdata->channel;

	PutMidi2_FX(midi_port,0xd0|channel,val,time,10,skip);
}

static void MIDI_treatFX_Pitch7(struct FX *fx,int val,STime time,int skip, FX_when when){
  struct Patch *patch = fx->patch;
  R_ASSERT_RETURN_IF_FALSE(patch!=NULL);

	struct PatchData *patchdata;
	struct MidiPort *midi_port;
	int channel;

        R_ASSERT_RETURN_IF_FALSE(patch->instrument==get_MIDI_instrument());

	patchdata=(struct PatchData *)patch->patchdata;
        if (patchdata==NULL)
          return;
        
	midi_port=patchdata->midi_port;
	channel=patchdata->channel;

	PutMidi3_FX(midi_port,0xe0|channel,0,val+0x40,time,10,skip);
}

static void MIDI_treatFX_Pitch14(struct FX *fx,int val,STime time,int skip, FX_when when){
  struct Patch *patch = fx->patch;
  R_ASSERT_RETURN_IF_FALSE(patch!=NULL);

	struct PatchData *patchdata;
	struct MidiPort *midi_port;
	int channel;

        R_ASSERT_RETURN_IF_FALSE(patch->instrument==get_MIDI_instrument());

	patchdata=(struct PatchData *)patch->patchdata;
        if (patchdata==NULL)
          return;
        
	midi_port=patchdata->midi_port;
	channel=patchdata->channel;

	val+=0x2000;

	PutMidi3_FX(midi_port,0xe0|channel,val&127,val>>7,time,10,skip);
}

static bool isFXUsed(struct TrackInstrumentData *tid,struct MIDI_FX *midi_fx){
	struct UsedTrackMidiCCs *usmf=tid->usmf;

	while(usmf!=NULL){
		if(midi_fx->effect_num==usmf->midi_fx->effect_num) return true;
		usmf=usmf->next;
	}

	return false;
}

void MIDI_closeFX(struct FX *fx,const struct Tracks *track){
	struct TrackInstrumentData *tid=(struct TrackInstrumentData *)track->midi_instrumentdata;
	struct UsedTrackMidiCCs *usmf=tid->usmf;
	struct UsedTrackMidiCCs *prev=NULL;
	struct MIDI_FX *midi_fx=(struct MIDI_FX *)fx->fxdata;

/*
	if(tid==NULL) printf("tid==NULL\n");
	if(usmf==NULL) printf("usmf==NULL\n");
*/

	while(usmf!=NULL){
		if(usmf->midi_fx==midi_fx){
			if(prev==NULL){
				tid->usmf=usmf->next;
			}else{
				prev->next=usmf->next;
			}
			return;
		}
		prev=usmf;
		usmf=usmf->next;
	}

	RError("Error in function 'MIDI_CloseFX' in file 'plug-ins/midi_fx.c'\n");
	return;
}

bool MIDISetTreatFX(struct FX *fx,struct MIDI_FX *midi_fx){
	switch(fx->effect_num){
		case PROGRAMCHANGE_CC:
			fx->treatFX=MIDI_treatFX_ProgramChange;
			break;
		case CHANNELPREASSURE_CC:
			fx->treatFX=MIDI_treatFX_ChannelPreassure;
			break;
		case PITCH7_CC:
			fx->treatFX=MIDI_treatFX_Pitch7;
			break;
		case PITCH14_CC:
			fx->treatFX=MIDI_treatFX_Pitch14;
			break;
		default:
			switch(midi_fx->max){
				case 127:
					fx->treatFX=MIDI_treatFX_CC7;
					break;
				case 0x3fff:
					fx->treatFX=MIDI_treatFX_CC14;
					break;
				case 0x3f:
					fx->treatFX=MIDI_treatFX_Pan7;
					break;
				case 0x1fff:
					fx->treatFX=MIDI_treatFX_Pan14;
					break;
				default:
					RError(
						"Error in function 'MIDIsetTreatFX' in file 'plug-ins/midi_fx.c'\n"
						"fx->effect_num: %d, midi_fx->max: %d\n",
						fx->effect_num,midi_fx->max
					);
					return false;
			}
			break;
	}
	return true;
}

int MIDIgetFX(struct Tracker_Windows *window,const struct Tracks *track,struct FX *fx){

	struct TrackInstrumentData *tid=(struct TrackInstrumentData *)track->midi_instrumentdata;
	struct UsedTrackMidiCCs *usmf;
	struct MIDI_FX *midi_fx;

	int lokke,selection;
	const char *menutitle="Select FX";

	ReqType reqtype;

        vector_t v={0};

	for(lokke=0;lokke<MIDI_NUM_FX;lokke++)
          VECTOR_push_back(&v,midi_fxs_fullnames[lokke]);

	for(;;){
                selection=GFX_Menu(window,NULL,menutitle,&v);
		if(-1==selection){
			return FX_FAILED;
		}
		midi_fx=&MIDI_fxs[selection];

		menutitle="FX already used";

		if(midi_fx->effect_num==-1) continue;

		if(midi_fx->effect_num==OTHER_CC){
			midi_fx=talloc(sizeof(struct MIDI_FX));

			reqtype=GFX_OpenReq(window,30,10,"Other CC select");

			midi_fx->effect_num=GFX_GetInteger(window,reqtype,"CC >",0,127);
			if(midi_fx->effect_num==-1){
				GFX_CloseReq(window,reqtype);
				return FX_FAILED;
			}

			midi_fx->max=127;

			if(midi_fx->effect_num<16){
                                int onlymsb=-1;
				while(onlymsb==-1){
                                  vector_t v={0};
                                  VECTOR_push_back(&v,"7");
                                  VECTOR_push_back(&v,"14");
                                  onlymsb=GFX_Menu(window,reqtype,"Resolution?",&v);
				}
				if(onlymsb==1){
					midi_fx->effect_num+=128;
					midi_fx->max=0x3fff;
				}
			}

			if(isFXUsed(tid,midi_fx)) continue;

			for(;;){
				midi_fx->name=GFX_GetString(window,reqtype,"Name >");
				if(midi_fx->name==NULL) continue;
				for(lokke=0;lokke<strlen(midi_fx->name);lokke++){
					if(midi_fx->name[lokke]==':' || midi_fx->name[lokke]=='/'){
						break;
					}
				}
				if(lokke<strlen(midi_fx->name)){
					midi_fx->name=GFX_GetString(window,reqtype,"(Name can not contain ':' or '/'. Press return.)");
				}else{
					break;
				}
			}
			GFX_CloseReq(window,reqtype);
			break;
		}

		if( ! isFXUsed(tid,midi_fx)) break;
	}

        fx->effect_num = midi_fx->effect_num;

	fx->fxdata=midi_fx;

	fx->num   = (NInt)fx->effect_num;
	fx->name    = midi_fx->name;
	fx->min     = midi_fx->min;
	fx->max     = midi_fx->max;
	fx->closeFX = &MIDI_closeFX;
	fx->SaveFX  = MIDISaveFX;

	if( ! MIDISetTreatFX(fx,midi_fx)){
		return FX_FAILED;
	}

	usmf          = talloc(sizeof(struct UsedTrackMidiCCs));
	usmf->next    = tid->usmf;
	tid->usmf     = usmf;
	usmf->midi_fx = midi_fx;

	return FX_SUCCESS;
}



/* Make a copy, necesarry for undo/redo and track-copy. */

void *MIDI_CopyInstrumentData(const struct Tracks *track){
	struct TrackInstrumentData *tid=(struct TrackInstrumentData *)track->midi_instrumentdata;
	struct UsedTrackMidiCCs *usmf=tid->usmf;
	struct UsedTrackMidiCCs *to_usmf;
	struct TrackInstrumentData *to=talloc(sizeof(struct TrackInstrumentData));
	struct UsedTrackMidiCCs **to_to_usmf=&to->usmf;

	while(usmf!=NULL){
		to_usmf=talloc(sizeof(struct UsedTrackMidiCCs));
		*to_to_usmf=to_usmf;
		to_usmf->midi_fx=usmf->midi_fx;
		to_to_usmf=&to_usmf->next;
		usmf=usmf->next;
	}

	return to;
}





