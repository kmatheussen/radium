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
#include "tracks_proc.h"

#include "disk.h"
#include "disk_notes_proc.h"
#include "disk_stops_proc.h"
#include "disk_fxs_proc.h"

#include "disk_track_proc.h"



void SaveTrack(struct Tracks *track){
if(track==NULL) return;
DC_start("TRACK");

	DC_SaveN(track->l.num);

	DC_SSI("onoff",track->onoff);

	DC_SSS("trackname",track->trackname);
	DC_SSI("volume",track->volume);
	DC_SSI("pan",track->pan);
	DC_SSB("volumeonoff",track->volumeonoff);
	DC_SSB("panonoff",track->panonoff);

	if(track->patch!=NULL){
		DC_SSN("patchnum",track->patch->l.num);
	}else{
		DC_SSN("patchnum",-1);
	}
//	DC_SSN("instrument",track->instrument->l.num);

	SaveNotes(track->notes);
	SaveStops(track->stops);
	SaveFXs(track->fxs,track);


DC_end();
SaveTrack(NextTrack(track));
}

extern struct Root *root;

struct Tracks *LoadTrack(void){
	int obsolete; //relvol
	static char *objs[3]={
		"NOTE",
		"STOPS",
		"FXS"
	};
	static char *vars[8]={
		"onoff",
		"trackname",
		"patchnum",
		"volume",
		"pan",
		"volumeonoff",
		"panonoff",
		"relvol"
	};
	struct Tracks *track=DC_alloc(sizeof(struct Tracks));
	track->l.num=DC_LoadN();
        InitTrack(track);

	GENERAL_LOAD(3,8)

var0:
	track->onoff=DC_LoadI();
	goto start;
var1:
	track->trackname=DC_LoadS();
	goto start;
var2:
	track->patch=DC_alloc_atomic(sizeof(struct Patch)); // Reason for atomic alloc: only l.num is used. track->patch is replaced in DLoadTracks
	track->patch->l.num=DC_LoadN();
	goto start;
var3:
	track->volume=DC_LoadI();
	goto start;
var4:
	track->pan=DC_LoadI();
	goto start;
var5:
	track->volumeonoff=DC_LoadB();
	goto start;
var6:
	track->panonoff=DC_LoadB();
	goto start;
var7:
	obsolete=DC_LoadI();
	goto start;

obj0:
	DC_ListAdd3_a(&track->notes,LoadNote());
	goto start;
obj1:
	LoadStops(&track->stops);
	goto start;
obj2:
	DC_ListAdd1(&track->fxs,LoadFXs(track));
	goto start;

var8:
var9:
var10:
var11:
var12:
var13:
var14:
var15:
var16:
var17:
var18:
obj3:
obj4:
obj5:
obj6:

error:
end:
	return track;
}


extern void DLoadInstrumentData(struct Root *newroot,struct Tracks *track);

void DLoadTracks(struct Root *newroot,struct Tracks *track){
if(track==NULL) return;

	if(track->patch->l.num==-1){
		track->patch=NULL;
	}else{
		track->patch=ListFindElement1(&newroot->song->instruments->patches->l,track->patch->l.num);
	}
	track->instrument=newroot->def_instrument;

DLoadTracks(newroot,NextTrack(track));
}


