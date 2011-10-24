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










#include "../nsmtracker.h"

#include <midi/camd.h>
#include "../plug-ins/minicamd_proc.h"
#include "../plug-ins/camd_i_plugin.h"
#include "../plug-ins/camd_i_plugin_proc.h"
#include "../plug-ins/camd_get_clustername_proc.h"
#include "../../common/visual_proc.h"


#include "Amiga_instrprop.h"


extern struct Patch *currpatch;

#define APP_GetVars() struct Patch *patch=currpatch;




int CPPWindowResetAllControllers( void )
{
	/* routine when (sub)item "Reset All Controllers" is selected. */

	APP_GetVars()
	struct PatchData *patchdata;
	struct MyMidiLinks *mymidilink;
	int channel;

	if(patch==NULL) return 0;
	patchdata=(struct PatchData *)patch->patchdata;
	mymidilink=patchdata->mymidilink;

	for(channel=0;channel<16;channel++){
		R_PutMidi3(mymidilink,0xb0|channel,121,0);
		mymidilink->channelspesific[channel].MSB=-1;
		mymidilink->channelspesific[channel].LSB=-1;
		mymidilink->channelspesific[channel].preset=-1;
	}

	return 0;
}

int CPPWindowLocalKeyboardOn( void )
{
	/* routine when (sub)item "Local Keyboard On" is selected. */

	APP_GetVars()
	struct PatchData *patchdata;
	struct MyMidiLinks *mymidilink;
	int channel;

	if(patch==NULL) return 0;
	patchdata=(struct PatchData *)patch->patchdata;
	mymidilink=patchdata->mymidilink;

	for(channel=0;channel<16;channel++){
		R_PutMidi3(mymidilink,0xb0|channel,122,127);
	}

	return 0;
}

int CPPWindowLocalKeyboardOff( void )
{
	/* routine when (sub)item "Local Keyboard Off" is selected. */

	APP_GetVars()
	struct PatchData *patchdata;
	struct MyMidiLinks *mymidilink;
	int channel;

	if(patch==NULL) return 0;
	patchdata=(struct PatchData *)patch->patchdata;
	mymidilink=patchdata->mymidilink;

	for(channel=0;channel<16;channel++){
		R_PutMidi3(mymidilink,0xb0|channel,122,0);
	}

	return 0;
}

int CPPWindowAllNotesOff( void )
{
	/* routine when (sub)item "All Notes Off" is selected. */

	APP_GetVars()
	struct PatchData *patchdata;
	struct MyMidiLinks *mymidilink;
	int channel;

	if(patch==NULL) return 0;
	patchdata=(struct PatchData *)patch->patchdata;
	mymidilink=patchdata->mymidilink;

	for(channel=0;channel<16;channel++){
		R_PutMidi3(mymidilink,0xb0|channel,123,0);
	}

	return 0;
}


int CPPWindowAllSoundsOff( void )
{
	/* routine when (sub)item "All Sounds Off" is selected. */

	APP_GetVars()
	struct PatchData *patchdata;
	struct MyMidiLinks *mymidilink;
	int channel;

	if(patch==NULL) return 0;
	patchdata=(struct PatchData *)patch->patchdata;
	mymidilink=patchdata->mymidilink;

	for(channel=0;channel<16;channel++){
		R_PutMidi3(mymidilink,0xb0|channel,120,0);
	}

	return 0;
}

extern struct MidiLink *inputmidilink;
extern struct MidiNode *midinode;
extern struct Root *root;
extern char *inlinkname;

int CPPWindowSetInputLink( void ){
	char *clustername;
	ReqType reqtype;
	FILE *file;

	reqtype=GFX_OpenReq(root->song->tracker_windows,70,(int)(10),"Select Input MidiLink");

	clustername=CAMD_getClusterName(reqtype);

	if(clustername==NULL){
		GFX_CloseReq(root->song->tracker_windows,reqtype);
		return 0;
	}

	RemoveMidiLink(inputmidilink);

	sprintf(inlinkname,"%s",clustername);

	inputmidilink=AddMidiLink(midinode,MLTYPE_Receiver,
		MLINK_Name, "sortoftracker.in",
		MLINK_Location,inlinkname,
		MLINK_EventMask,CMF_Channel,
		NULL
	);

	GFX_CloseReq(root->song->tracker_windows,reqtype);

	if(inputmidilink==NULL){
		return 0;
	}

	file=fopen("radium:inlinkname.txt","w");
	if(file!=NULL){
		fprintf(file,"%s",inlinkname);
		fclose(file);
	}

	return 0;
}

extern bool useOx90ForNoteOff;

int CPPWindowOx90ForNoteOff( void ){
	useOx90ForNoteOff=useOx90ForNoteOff==false?true:false;
	return 0;
}







