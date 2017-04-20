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
#include "camd_i_plugin.h"
#include "camd_i_plugin_proc.h"
#include "../visual_proc.h"
#include <midi/camd.h>
#include "minicamd_proc.h"
#include <string.h>
#include "disk_camd_fx_proc.h"

#include "camd_fx_proc.h"


struct CAMD_FX CAMD_fxs[CAMD_NUM_FX]={
	"Program Change",0,127,PROGRAMCHANGE_CC,

	"-----------------------",0,0,-1,

	"Channel Preassure",0,127,CHANNELPREASSURE_CC,

	"-----------------------",0,0,-1,

	"Pitch7",-64,63,PITCH7_CC,
	"Pitch14",-0x2000,0x1fff,PITCH14_CC,

	"-----------------------",0,0,-1,

	"Modulation7.",0,127,1,
	"Modulation14.",0,0x3fff,129,

	"-----------------------",0,0,-1,

	"Portamento7.",0,127,5,
	"Portamento14.",0,0x3fff,5,

	"-----------------------",0,0,-1,

	"Volume7.",0,127,7,
	"Volume14.",0,0x3fff,135,

	"-----------------------",0,0,-1,

	"Pan7.",-64,63,10,
	"Pan14.",-0x2000,0x1fff,138,

	"-----------------------",0,0,-1,

	"Chorus.",0,127,93,
	"Reverb.",0,127,91,
	"Attack.",0,127,73,
	"Release.",0,127,72,
	"CutOff",0,127,74,
	"Resonance",0,127,71,
	"Tremolo",0,127,92,
   "Phaser",0,127,95,

	"-----------------------",0,0,-1,
	"Other CC",0,0,OTHER_CC
};

char *camd_fxs_fullnames[CAMD_NUM_FX]={
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


void CAMD_treatFX_CC7(struct FX *fx,int val,struct Tracks *track,int skip){
	struct CAMD_FX *camd_fx=(struct CAMD_FX *)fx->fxdata;
	struct PatchData *patchdata;
	struct MyMidiLinks *mymidilink;
	int channel;

	if(track->patch==NULL) return;

	patchdata=(struct PatchData *)track->patch->patchdata;
	mymidilink=patchdata->mymidilink;
	channel=patchdata->channel;

	PutMidi3_FX(mymidilink,0xb0|channel,camd_fx->cc,val,10,skip);
}


void CAMD_treatFX_CC14(struct FX *fx,int val,struct Tracks *track,int skip){
	struct CAMD_FX *camd_fx=(struct CAMD_FX *)fx->fxdata;
	struct PatchData *patchdata;
	struct MyMidiLinks *mymidilink;
	int channel;

	if(track->patch==NULL) return;

	patchdata=(struct PatchData *)track->patch->patchdata;
	mymidilink=patchdata->mymidilink;
	channel=patchdata->channel;

	PutMidi3_FX(mymidilink,0xb0|channel,camd_fx->cc-128,val>>7,10,skip);
	PutMidi3_FX(mymidilink,0xb0|channel,camd_fx->cc-128+32,val&127,10,skip);
}

void CAMD_treatFX_Pan7(struct FX *fx,int val,struct Tracks *track,int skip){
	struct PatchData *patchdata;
	struct MyMidiLinks *mymidilink;
	int channel;

	if(track->patch==NULL) return;

	patchdata=(struct PatchData *)track->patch->patchdata;
	mymidilink=patchdata->mymidilink;
	channel=patchdata->channel;

	PutMidi3_FX(mymidilink,0xb0|channel,10,val+0x40,10,skip);
}


void CAMD_treatFX_Pan14(struct FX *fx,int val,struct Tracks *track,int skip){
	struct PatchData *patchdata;
	struct MyMidiLinks *mymidilink;
	int channel;

	if(track->patch==NULL) return;

	patchdata=(struct PatchData *)track->patch->patchdata;
	mymidilink=patchdata->mymidilink;
	channel=patchdata->channel;

	val+=0x2000;

	PutMidi3_FX(mymidilink,0xb0|channel,10,val>>7,10,skip);
	PutMidi3_FX(mymidilink,0xb0|channel,42,val&127,10,skip);
}

void CAMD_treatFX_ProgramChange(struct FX *fx,int val,struct Tracks *track,int skip){

	struct PatchData *patchdata;
	struct MyMidiLinks *mymidilink;
	int channel;

	if(track->patch==NULL) return;

	patchdata=(struct PatchData *)track->patch->patchdata;
	mymidilink=patchdata->mymidilink;
	channel=patchdata->channel;

	PutMidi2(mymidilink,0xc0|channel,val,50);
}

void CAMD_treatFX_ChannelPreassure(struct FX *fx,int val,struct Tracks *track,int skip){

	struct PatchData *patchdata;
	struct MyMidiLinks *mymidilink;
	int channel;

	if(track->patch==NULL) return;

	patchdata=(struct PatchData *)track->patch->patchdata;
	mymidilink=patchdata->mymidilink;
	channel=patchdata->channel;

	PutMidi2_FX(mymidilink,0xd0|channel,val,10,skip);
}

void CAMD_treatFX_Pitch7(struct FX *fx,int val,struct Tracks *track,int skip){
	struct PatchData *patchdata;
	struct MyMidiLinks *mymidilink;
	int channel;

	if(track->patch==NULL) return;


	patchdata=(struct PatchData *)track->patch->patchdata;
	mymidilink=patchdata->mymidilink;
	channel=patchdata->channel;

	PutMidi3_FX(mymidilink,0xe0|channel,0,val+0x40,10,skip);
}

void CAMD_treatFX_Pitch14(struct FX *fx,int val,struct Tracks *track,int skip){

	struct PatchData *patchdata;
	struct MyMidiLinks *mymidilink;
	int channel;

	if(track->patch==NULL) return;


	patchdata=(struct PatchData *)track->patch->patchdata;
	mymidilink=patchdata->mymidilink;
	channel=patchdata->channel;

	val+=0x2000;

	PutMidi3_FX(mymidilink,0xe0|channel,val&127,val>>7,10,skip);
}

bool isFXUsed(struct TrackInstrumentData *tid,struct CAMD_FX *camd_fx){
	struct UsedTrackMidiCCs *usmf=tid->usmf;

	while(usmf!=NULL){
		if(camd_fx->cc==usmf->camd_fx->cc) return true;
		usmf=usmf->next;
	}

	return false;
}

void CAMD_closeFX(struct FX *fx,struct Tracks *track){
	struct TrackInstrumentData *tid=(struct TrackInstrumentData *)track->instrumentdata;
	struct UsedTrackMidiCCs *usmf=tid->usmf;
	struct UsedTrackMidiCCs *prev=NULL;
	struct CAMD_FX *camd_fx=(struct CAMD_FX *)fx->fxdata;

/*
	if(tid==NULL) printf("tid==NULL\n");
	if(usmf==NULL) printf("usmf==NULL\n");
*/

	while(usmf!=NULL){
		if(usmf->camd_fx==camd_fx){
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

	RError("Error in function 'CAMD_CloseFX' in file 'plug-ins/camd_fx.c'\n");
	return;
}

bool CAMDSetTreatFX(struct FX *fx,struct CAMD_FX *camd_fx){
	switch(camd_fx->cc){
		case PROGRAMCHANGE_CC:
			fx->treatFX=CAMD_treatFX_ProgramChange;
			break;
		case CHANNELPREASSURE_CC:
			fx->treatFX=CAMD_treatFX_ChannelPreassure;
			break;
		case PITCH7_CC:
			fx->treatFX=CAMD_treatFX_Pitch7;
			break;
		case PITCH14_CC:
			fx->treatFX=CAMD_treatFX_Pitch14;
			break;
		default:
			switch(camd_fx->max){
				case 127:
					fx->treatFX=CAMD_treatFX_CC7;
					break;
				case 0x3fff:
					fx->treatFX=CAMD_treatFX_CC14;
					break;
				case 0x3f:
					fx->treatFX=CAMD_treatFX_Pan7;
					break;
				case 0x1fff:
					fx->treatFX=CAMD_treatFX_Pan14;
					break;
				default:
					RError(
						"Error in function 'CAMDsetTreatFX' in file 'plug-ins/camd_fx.c'\n"
						"camd_fx->cc: %d, camd_fx->max: %d\n",
						camd_fx->cc,camd_fx->max
					);
					return false;
			}
			break;
	}
	return true;
}

int CAMDgetFX(struct Tracker_Windows *window,struct Tracks *track,struct FX *fx){

	struct TrackInstrumentData *tid=(struct TrackInstrumentData *)track->instrumentdata;
	struct UsedTrackMidiCCs *usmf;
	struct CAMD_FX *camd_fx;

	int lokke,selection;
	char **menutext;
	char *menutitle="Select FX";

	ReqType reqtype;
	char *restext[2]={"7","14"};
	int onlymsb=-1;

	menutext=talloc(sizeof(char *)*CAMD_NUM_FX);

	for(lokke=0;lokke<CAMD_NUM_FX;lokke++){
		menutext[lokke]=camd_fxs_fullnames[lokke];
	}

	for(;;){
		selection=GFX_Menu(window,NULL,menutitle,CAMD_NUM_FX,menutext);
		if(-1==selection){
			return FX_FAILED;
		}
		camd_fx=&CAMD_fxs[selection];

		menutitle="FX already used";

		if(camd_fx->cc==-1) continue;

		if(camd_fx->cc==OTHER_CC){
			camd_fx=talloc(sizeof(struct CAMD_FX));

			reqtype=GFX_OpenReq(window,30,10,"Other CC select");

			camd_fx->cc=GFX_GetInteger(window,reqtype,"CC >",0,127);
			if(camd_fx->cc==-1){
				GFX_CloseReq(window,reqtype);
				return FX_FAILED;
			}

			camd_fx->max=127;

			if(camd_fx->cc<16){
				while(onlymsb==-1){
					onlymsb=GFX_Menu(window,reqtype,"Resolution?",2,restext);
				}
				if(onlymsb==1){
					camd_fx->cc+=128;
					camd_fx->max=0x3fff;
				}
			}

			if(isFXUsed(tid,camd_fx)) continue;

			for(;;){
				camd_fx->name=GFX_GetString(window,reqtype,"Name >");
				if(camd_fx->name==NULL) continue;
				for(lokke=0;lokke<strlen(camd_fx->name);lokke++){
					if(camd_fx->name[lokke]==':' || camd_fx->name[lokke]=='/'){
						break;
					}
				}
				if(lokke<strlen(camd_fx->name)){
					camd_fx->name=GFX_GetString(window,reqtype,"(Name can not contain ':' or '/'. Press return.)");
				}else{
					break;
				}
			}
			GFX_CloseReq(window,reqtype);
			break;
		}

		if( ! isFXUsed(tid,camd_fx)) break;
	}

	fx->fxdata=camd_fx;

	fx->l.num=(NInt)camd_fx->cc;
	fx->name=camd_fx->name;
	fx->min=camd_fx->min;
	fx->max=camd_fx->max;
	fx->closeFX= &CAMD_closeFX;
	fx->SaveFX=CAMDSaveFX;

	if( ! CAMDSetTreatFX(fx,camd_fx)){
		return FX_FAILED;
	}

	usmf=talloc(sizeof(struct UsedTrackMidiCCs));
	usmf->next=tid->usmf;
	tid->usmf=usmf;
	usmf->camd_fx=camd_fx;

	return FX_SUCCESS;
}



/* Make a copy, necesarry for undo/redo and track-copy. */

void *CAMD_CopyInstrumentData(struct Tracks *track){
	struct TrackInstrumentData *tid=(struct TrackInstrumentData *)track->instrumentdata;
	struct UsedTrackMidiCCs *usmf=tid->usmf;
	struct UsedTrackMidiCCs *to_usmf;
	struct TrackInstrumentData *to=talloc(sizeof(struct TrackInstrumentData));
	struct UsedTrackMidiCCs **to_to_usmf=&to->usmf;

	while(usmf!=NULL){
		to_usmf=talloc(sizeof(struct UsedTrackMidiCCs));
		*to_to_usmf=to_usmf;
		to_usmf->camd_fx=usmf->camd_fx;
		to_to_usmf=&to_usmf->next;
		usmf=usmf->next;
	}

	return to;
}





