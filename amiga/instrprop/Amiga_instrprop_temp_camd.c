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









#include <intuition/intuition.h>
#include <intuition/classes.h>
#include <intuition/classusr.h>
#include <intuition/imageclass.h>
#include <intuition/gadgetclass.h>
#include <libraries/gadtools.h>
#include <proto/gadtools.h>
#include <proto/intuition.h>
#include <stdio.h>
#include <midi/camd.h>
#include "Amiga_instrprop.h"

#include "../nsmtracker.h"
#include "../../common/list_proc.h"
#include "Amiga_instrprop_edit_proc.h"
#include "../../common/gfx_wtrackheaders_proc.h"
#include "../../common/patch_proc.h"
#include "../plug-ins/camd_i_plugin.h"
#include "../plug-ins/camd_i_plugin_proc.h"
#include "../plug-ins/camd_get_clustername_proc.h"
#include "../plug-ins/camd_getMidiLink_proc.h"
#include "../plug-ins/minicamd_proc.h"
#include "../../common/playerclass.h"
#include "../visual_proc.h"
#include "../../common/gfx_wtrackheaders_proc.h"

#include <string.h>



#define APP_GetVars() struct Patch *patch=currpatch;

extern struct Root *root;

extern PlayerClass *pc;

extern struct Patch *currpatch;


ULONG lastSeconds=0,lastMicros=0;

int nameClicked( void )
{
	/* routine when gadget "" is clicked. */

	char *name;
	size_t len;
	APP_GetVars()

	name=GetString(CPPWindowGadgets[GD_name]);
	len=strlen(name);

	if(len<1 || len>50){
		GT_SetGadgetAttrs(
			CPPWindowGadgets[GD_name],
			CPPWindowWnd,
			NULL,
			GA_Disabled,FALSE,
			GTST_String,patch->name,
			TAG_END
		);
		return 0;
	}

	patch->name=talloc_atomic(len+2);
	sprintf(patch->name,"%s",name);

	DrawAllWTrackHeaders(root->song->tracker_windows,root->song->tracker_windows->wblock);

	return 0;
}



int NumberClicked( void ){
	/* routine when gadget "" is clicked. */

	struct Instruments *instrument=root->song->instruments;
	struct Tracker_Windows *window=root->song->tracker_windows;

	NInt num=CPPWindowMsg.Code;
	struct Patch *patch=NULL;

	if(num!=0){
		patch=ListFindElement1(&instrument->patches->l,num-1);
	}

	if( ! pc->isplaying){
		window->wblock->wtrack->track->patch=patch;
		DrawWTrackHeader(window,window->wblock,window->wblock->wtrack);
	}

	CAMDPP_Update(instrument,patch);

	return 0;
}




int NewClicked( void ){
	/* routine when gadget "New" is clicked. */

	struct Tracker_Windows *window=root->song->tracker_windows;

	SelectPatch(window,window->wblock->wtrack->track);

	return 0;
}




int ClusterClicked( void )
{
	/* routine when gadget "Cluster" is clicked. */
	APP_GetVars()
	struct PatchData *patchdata=(struct PatchData *)patch->patchdata;
	struct MyMidiLinks *mymidilink=patchdata->mymidilink;
	struct MyMidiLinks *newmymidilink;

	struct MidiCluster *cluster=NULL;

	char *name=GetString(CPPWindowGadgets[GD_Cluster]);
	if(strlen(name)<1){
		GT_SetGadgetAttrs(
			CPPWindowGadgets[GD_Cluster],
			CPPWindowWnd,
			NULL,
			GA_Disabled,FALSE,
			GTST_String,mymidilink->name,
			TAG_END
		);
		return 0;
	}

//	lock=LockCAMD(CD_Linkages);

	for(;;){
		cluster=NextCluster(cluster);
		if(cluster!=NULL){
			if(!strcmp(cluster->mcl_Node.ln_Name,name)){
				break;
			}
		}else{
			name=CAMD_getClusterName(NULL);
			break;
		}
	}

	if(name==NULL){
		GT_SetGadgetAttrs(
			CPPWindowGadgets[GD_Cluster],
			CPPWindowWnd,
			NULL,
			GA_Disabled,FALSE,
			GTST_String,mymidilink->name,
			TAG_END
		);
	}else{
		newmymidilink=CAMD_getMyMidiLink(name);
		if(newmymidilink==NULL){
			GT_SetGadgetAttrs(
				CPPWindowGadgets[GD_Cluster],
				CPPWindowWnd,
				NULL,
				GA_Disabled,FALSE,
				GTST_String,mymidilink->name,
				TAG_END
			);
		}else{
			patchdata->mymidilink=newmymidilink;
		}
	}

//	UnlockCAMD(lock);

	CAMDPP_Update_doit(root->song->instruments,patch);

	return 0;
}



int MSBClicked( void )
{
	/* routine when gadget "MSB" is clicked. */

	APP_GetVars()
	struct PatchData *patchdata=(struct PatchData *)patch->patchdata;

	int num=GetNumber(CPPWindowGadgets[GD_MSB]);

	if(num>=-1 && num<128){
		patchdata->MSB=num;
	}else{
		GT_SetGadgetAttrs(
			CPPWindowGadgets[GD_MSB],
			CPPWindowWnd,
			NULL,
			GTIN_Number,patchdata->MSB,
			TAG_END
		);
	}

	return 0;
}


extern const int G2CC[8];
extern const int GB2CC[8];


int ChannelClicked( void )
{
	/* routine when gadget "Channel" is clicked. */
	APP_GetVars()
	struct PatchData *patchdata=(struct PatchData *)patch->patchdata;
	struct MyMidiLinks *mymidilink;
	struct ChannelSpesific *cs;
	int lokke;

	patchdata->channel=CPPWindowMsg.Code-1;

	mymidilink=patchdata->mymidilink;
	cs=&mymidilink->channelspesific[patchdata->channel];

	GT_SetGadgetAttrs(
		CPPWindowGadgets[GD_LSB],
		CPPWindowWnd,
		NULL,
		GA_Disabled,FALSE,
		GTIN_Number,patchdata->LSB,
		TAG_END
	);

	GT_SetGadgetAttrs(
		CPPWindowGadgets[GD_MSB],
		CPPWindowWnd,
		NULL,
		GA_Disabled,FALSE,
		GTIN_Number,patchdata->MSB,
		TAG_END
	);

	GT_SetGadgetAttrs(
		CPPWindowGadgets[GD_Preset],
		CPPWindowWnd,
		NULL,
		GA_Disabled,FALSE,
		GTIN_Number,patchdata->preset+1,
		TAG_END
	);

	GT_SetGadgetAttrs(
		CPPWindowGadgets[GD_Velocity],
		CPPWindowWnd,
		NULL,
		GTSL_Level,patch->standardvel,
		TAG_END
	);

	GT_SetGadgetAttrs(
		CPPWindowGadgets[GD_Volumeonoff],
		CPPWindowWnd,
		NULL,
		GA_Disabled,FALSE,
		GTCB_Checked,cs->volumeonoff?TRUE:FALSE,
		TAG_END
	);

	GT_SetGadgetAttrs(
		CPPWindowGadgets[GD_Volume],
		CPPWindowWnd,
		NULL,
		GA_Disabled,cs->volumeonoff?FALSE:TRUE,
		GTSL_Level,cs->volume,
		TAG_END
	);

	GT_SetGadgetAttrs(
		CPPWindowGadgets[GD_Pan],
		CPPWindowWnd,
		NULL,
		GA_Immediate,TRUE,
		GA_Disabled,cs->panonoff?FALSE:TRUE,
		GTSL_Level,cs->pan,
		TAG_END
	);
	GT_SetGadgetAttrs(
		CPPWindowGadgets[GD_Panonoff],
		CPPWindowWnd,
		NULL,
		GA_Disabled,FALSE,
		GTCB_Checked,cs->panonoff?TRUE:FALSE,
		TAG_END
	);

	for(lokke=0;lokke<8;lokke++){
		GT_SetGadgetAttrs(
			CPPWindowGadgets[GB2CC[lokke]],
			CPPWindowWnd,
			NULL,
			GA_Disabled,FALSE,
			TAG_END
		);
		GT_SetGadgetAttrs(
			CPPWindowGadgets[G2CC[lokke]],
			CPPWindowWnd,
			NULL,
			GA_Disabled,mymidilink->standardccs[lokke]==-1?TRUE:cs->ccsonoff[lokke]?FALSE:TRUE,		//Nice. :)
			GTSL_Level,cs->ccvalues[lokke],
			TAG_END
		);
	}


	return 0;
}





int PresetClicked( void )
{
	/* routine when gadget "Preset" is clicked. */


	APP_GetVars()
	struct PatchData *patchdata=(struct PatchData *)patch->patchdata;

	int num=GetNumber(CPPWindowGadgets[GD_Preset]);

	if(num>=0 && num<129){
		patchdata->preset=num-1;
	}else{
		GT_SetGadgetAttrs(
			CPPWindowGadgets[GD_Preset],
			CPPWindowWnd,
			NULL,
			GTIN_Number,patchdata->preset+1,
			TAG_END
		);
	}

return 0;}





int LSBClicked( void )
{
	/* routine when gadget "LSB" is clicked. */

	APP_GetVars()
	struct PatchData *patchdata=(struct PatchData *)patch->patchdata;

	int num=GetNumber(CPPWindowGadgets[GD_LSB]);

	if(num>=-1 && num<128){
		patchdata->LSB=num;
	}else{
		GT_SetGadgetAttrs(
			CPPWindowGadgets[GD_LSB],
			CPPWindowWnd,
			NULL,
			GTIN_Number,patchdata->LSB,
			TAG_END
		);
	}

	return 0;
}


char CPPwindowtitle[120];

int CC2G(int gadget){
	switch(gadget){
		case GD_Cr:
			return 0;
		case GD_Re:
			return 1;
		case GD_At:
			return 2;
		case GD_Rl:
			return 3;
		case GD_Cu:
			return 4;
		case GD_Rs:
			return 5;
		case GD_Va:
			return 6;
		case GD_Mo:
			return 7;
	}
}

void SliderClicked(int gadgetnum){
	APP_GetVars()
	struct PatchData *patchdata=(struct PatchData *)patch->patchdata;
	int channel=patchdata->channel;
	struct MyMidiLinks *mymidilink=patchdata->mymidilink;
	struct ChannelSpesific *cs=&mymidilink->channelspesific[channel];

	char *name;
	int cc;
	int val=CPPWindowMsg.Code;
	int lokke;

	switch(gadgetnum){
		case GD_Volume:
			name="Volume";
			cc=0x7;
			cs->volume=val;
			break;
		case GD_Pan:
			name="Pan";
			cc=0xa;
			cs->pan=val;
			break;
		default:
			lokke=CC2G(gadgetnum);
			name=mymidilink->ccnames[lokke];
			cc=mymidilink->standardccs[lokke];
			cs->ccvalues[lokke]=val;

	}

	D_PutMidi3(mymidilink,0xb0|channel,cc,val);

	sprintf(CPPwindowtitle,"%s: %d",name,val);
	SetWindowTitles(CPPWindowWnd,CPPwindowtitle,(UBYTE *)-1);
}


int VolumeClicked( void )
{
	/* routine when gadget "" is clicked. */

	SliderClicked(GD_Volume);


return 0;}

int PanClicked( void )
{
	/* routine when gadget "" is clicked. */

	SliderClicked(GD_Pan);

return 0;}

int VelocityClicked( void )
{
	/* routine when gadget "" is clicked. */
	if(currpatch==NULL){
		root->standardvel=CPPWindowMsg.Code;
	}else{
		currpatch->standardvel=CPPWindowMsg.Code;
	}

	sprintf(CPPwindowtitle,"Standard velocity: %d",CPPWindowMsg.Code);
	SetWindowTitles(CPPWindowWnd,CPPwindowtitle,(UBYTE *)-1);

return 0;}

int CrClicked( void )
{
	/* routine when gadget "Cr" is clicked. */
	SliderClicked(GD_Cr);

return 0;}

int ReClicked( void )
{
	/* routine when gadget "Re" is clicked. */
	SliderClicked(GD_Re);
return 0;}

int AtClicked( void )
{
	/* routine when gadget "At" is clicked. */
	SliderClicked(GD_At);
return 0;}

int RlClicked( void )
{
	/* routine when gadget "Rl" is clicked. */
	SliderClicked(GD_Rl);
return 0;}

int CuClicked( void )
{
	/* routine when gadget "Cu" is clicked. */
	SliderClicked(GD_Cu);
return 0;}

int RsClicked( void )
{
	/* routine when gadget "Rs" is clicked. */
	SliderClicked(GD_Rs);
return 0;}

int VaClicked( void )
{
	/* routine when gadget "Va" is clicked. */
	SliderClicked(GD_Va);
return 0;}

int MoClicked( void )
{
	/* routine when gadget "Mo" is clicked. */
	SliderClicked(GD_Mo);
return 0;}


int CC2GB(int gadget){
	switch(gadget){
		case GD_B1:
			return 0;
		case GD_B2:
			return 1;
		case GD_B3:
			return 2;
		case GD_B4:
			return 3;
		case GD_B5:
			return 4;
		case GD_B6:
			return 5;
		case GD_B7:
			return 6;
		case GD_B8:
			return 7;
	}
}

extern const int G2CC[8];

void CCselectClicked(int gadget){
	APP_GetVars()
	struct PatchData *patchdata=(struct PatchData *)patch->patchdata;
	int channel=patchdata->channel;
	struct MyMidiLinks *mymidilink=patchdata->mymidilink;
	struct ChannelSpesific *cs=&mymidilink->channelspesific[channel];
	struct Tracker_Windows *window=root->song->tracker_windows;
	int cc;

	int ccnum=CC2GB(gadget);

	ReqType reqtype;

	if(mymidilink->standardccs[ccnum]!=-1){
		if( ! DoubleClick(lastSeconds,lastMicros,CPPWindowMsg.Seconds,CPPWindowMsg.Micros)){
			cs->ccsonoff[ccnum]=cs->ccsonoff[ccnum]?false:true;
			GT_SetGadgetAttrs(
				CPPWindowGadgets[G2CC[ccnum]],
				CPPWindowWnd,
				NULL,
				GA_Disabled,cs->ccsonoff[ccnum]?FALSE:TRUE,
				GTSL_Level,cs->ccvalues[ccnum],
				TAG_END
			);
			lastSeconds=CPPWindowMsg.Seconds;
			lastMicros=CPPWindowMsg.Micros;
			return;
		}
	}

	lastSeconds=CPPWindowMsg.Seconds;
	lastMicros=CPPWindowMsg.Micros;

	cs->ccsonoff[channel]=true;

	reqtype=GFX_OpenReq(window,100,100,"CC slider config");

	cc=GFX_GetInteger(window,reqtype,"New CC: (-1 to turn off) ",-1,127);
	if(cc==-2){
		GFX_CloseReq(window,reqtype);
		return;
	}
	mymidilink->standardccs[ccnum]=cc;

	if(cc>=0){
		mymidilink->ccnames[ccnum]=NULL;

		while(mymidilink->ccnames[ccnum]==NULL){
			mymidilink->ccnames[ccnum]=GFX_GetString(
				window,reqtype,
				"New name: "
			);
		}
	}
	GT_SetGadgetAttrs(
		CPPWindowGadgets[G2CC[ccnum]],
		CPPWindowWnd,
		NULL,
		GA_Disabled,mymidilink->standardccs[ccnum]==-1?TRUE:FALSE,
		GTSL_Level,cs->ccvalues[ccnum],
		TAG_END
	);

	GFX_CloseReq(window,reqtype);

}

int B1Clicked( void )
{
	/* routine when gadget "Cr" is clicked. */

	CCselectClicked(GD_B1);

	return 0;
}

int B5Clicked( void )
{
	/* routine when gadget "Cu" is clicked. */
	CCselectClicked(GD_B5);
	return 0;
}

int B2Clicked( void )
{
	/* routine when gadget "Re" is clicked. */
	CCselectClicked(GD_B2);
	return 0;
}

int B6Clicked( void )
{
	/* routine when gadget "Rs" is clicked. */
	CCselectClicked(GD_B6);
	return 0;
}

int B3Clicked( void )
{
	/* routine when gadget "At" is clicked. */
	CCselectClicked(GD_B3);
	return 0;
}

int B7Clicked( void )
{
	/* routine when gadget "Va" is clicked. */
	CCselectClicked(GD_B7);
	return 0;
}

int B4Clicked( void )
{
	/* routine when gadget "Rl" is clicked. */
	CCselectClicked(GD_B4);
	return 0;
}

int B8Clicked( void )
{
	/* routine when gadget "Mo" is clicked. */
	CCselectClicked(GD_B8);
	return 0;
}

int PanonoffClicked( void )
{
	/* routine when gadget "" is clicked. */

	APP_GetVars()
	struct PatchData *patchdata=(struct PatchData *)patch->patchdata;
	int channel=patchdata->channel;
	struct MyMidiLinks *mymidilink=patchdata->mymidilink;
	struct ChannelSpesific *cs=&mymidilink->channelspesific[channel];

	cs->panonoff=cs->panonoff?false:true;

	GT_SetGadgetAttrs(
		CPPWindowGadgets[GD_Pan],
		CPPWindowWnd,
		NULL,
		GA_Immediate,TRUE,
		GA_Disabled,cs->panonoff?FALSE:TRUE,
		GTSL_Level,cs->pan,
		TAG_END
	);

	return 0;
}

int VolumeonoffClicked( void )
{
	/* routine when gadget "" is clicked. */

	APP_GetVars()
	struct PatchData *patchdata=(struct PatchData *)patch->patchdata;
	int channel=patchdata->channel;
	struct MyMidiLinks *mymidilink=patchdata->mymidilink;
	struct ChannelSpesific *cs=&mymidilink->channelspesific[channel];

	cs->volumeonoff=cs->volumeonoff?false:true;

	GT_SetGadgetAttrs(
		CPPWindowGadgets[GD_Volume],
		CPPWindowWnd,
		NULL,
		GA_Disabled,cs->volumeonoff?FALSE:TRUE,
		GTSL_Level,cs->volume,
		TAG_END
	);

	return 0;
}

