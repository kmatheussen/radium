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
#include "visual_proc.h"
#include "list_proc.h"
#include "gfx_wtrackheaders_proc.h"
#include "player_proc.h"
#include "undo_tracks_proc.h"

#include "patch_proc.h"



void SelectPatch(struct Tracker_Windows *window,struct Tracks *track){
	struct Instruments *instrument=track->instrument;
	struct Patch *patch;
	char **menutext;
	NInt numpatches;
	int lokke;
	int selection;
	ReqType reqtype;

	PlayStop();

	numpatches=ListFindNumElements1(&instrument->patches->l);

	reqtype=GFX_OpenReq(window,70,(int)(numpatches+50),"Select Patch");

	if(instrument->patches==NULL){
		patch=talloc(sizeof(struct Patch));
		if((*instrument->getPatch)(window,reqtype,track,patch)==PATCH_FAILED){
			GFX_CloseReq(window,reqtype);
			return;
		}
		Undo_Track_CurrPos(window);
		ListAddElement1(&instrument->patches,&patch->l);
		track->patch=patch;
		while(patch->name==NULL) patch->name=GFX_GetString(window,reqtype,"Name: ");
		DrawWTrackHeader(window,window->wblock,window->wblock->wtrack);
		GFX_CloseReq(window,reqtype);
		(*instrument->PP_Update)(instrument,patch);
		return;
	}

	menutext=talloc((size_t)(sizeof(char *)*(numpatches+1)));

	patch=instrument->patches;

	lokke=0;
	while(patch!=NULL){
		menutext[lokke]=patch->name;
		patch=NextPatch(patch);
		lokke++;
	}

	menutext[lokke]="New patch";

	selection=GFX_Menu(window,reqtype,"Select Patch",(int)(numpatches+1),menutext);

	if(-1==selection){
		GFX_CloseReq(window,reqtype);
		return;
	}

	if(selection==numpatches){
		patch=talloc(sizeof(struct Patch));
		if((*instrument->getPatch)(window,reqtype,track,patch)==PATCH_FAILED){
			GFX_CloseReq(window,reqtype);
			return;
		}
		Undo_Track_CurrPos(window);
		ListAddElement1_ff(&instrument->patches,&patch->l);
		track->patch=patch;
		while(patch->name==NULL) patch->name=GFX_GetString(window,reqtype,"Name: ");
	}else{
		Undo_Track_CurrPos(window);
		lokke=0;
		patch=instrument->patches;
		while(lokke<selection){
			lokke++;
			patch=NextPatch(patch);
		}
	}

	GFX_CloseReq(window,reqtype);

	track->patch=patch;

	DrawWTrackHeader(window,window->wblock,window->wblock->wtrack);

	(*instrument->PP_Update)(instrument,patch);

}


void PATCH_playNoteCurrPos(struct Tracker_Windows *window,int notenum){
	struct Tracks *track=window->wblock->wtrack->track;
	struct Patch *patch=track->patch;

	if(patch==NULL || notenum<0 || notenum>127) return;

	(*patch->playnote)(notenum,patch->standardvel,track,NULL);
}


void PATCH_stopNoteCurrPos(struct Tracker_Windows *window,int notenum){
	struct Tracks *track=window->wblock->wtrack->track;
	struct Patch *patch=track->patch;

	if(patch==NULL || notenum<0 || notenum>127) return;

	(*patch->stopnote)(notenum,patch->standardvel,track,NULL);
}

