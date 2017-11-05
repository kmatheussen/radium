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
#include "vector_proc.h"
#include "tracks_proc.h"
#include "instruments_proc.h"
#include "patch_proc.h"
#include "../audio/SoundPlugin.h"

#include "disk.h"
#include "disk_notes_proc.h"
#include "disk_stops_proc.h"
#include "disk_fxs_proc.h"
#include "disk_swing_proc.h"

#include "disk_track_proc.h"



void SaveTrack(struct Tracks *track, bool save_all){
if(track==NULL) return;
DC_start("TRACK");

	DC_SaveN(track->l.num);

	DC_SSI("onoff",track->onoff);

	DC_SSS("trackname",track->trackname);
	DC_SSI("volume",track->volume);
	DC_SSI("pan",track->pan);
	DC_SSB("volumeonoff",track->volumeonoff);
	DC_SSB("panonoff",track->panonoff);

        DC_SSI("midi_channel", ATOMIC_GET(track->midi_channel));
        
	if(track->patch!=NULL){
		DC_SSN("patchnum",track->patch->id);
	}else{
		DC_SSN("patchnum",-1);
	}

        DC_SSI("instrument_type",get_type_from_instrument(track->patch==NULL ? NULL : track->patch->instrument));

//	DC_SSN("instrument",track->instrument->l.num);

	SaveNotes(track->notes);
	SaveStops(track->stops);
	SaveFXs(&track->fxs,track);
        SaveSwings(track->swings);

DC_end();
if (save_all)
  SaveTrack(NextTrack(track), true);
}

extern struct Root *root;

struct Tracks *LoadTrack(void){
        static char *objs[4]={
          "NOTE",
          "STOPS",
          "FXS",
          "SWING"
        };
	static char *vars[10]={
		"onoff",
		"trackname",
		"patchnum",
		"volume",
		"pan",
		"volumeonoff",
		"panonoff",
                "midi_channel",
		"relvol",
                "instrument_type"
	};
	struct Tracks *track=DC_alloc(sizeof(struct Tracks));
	track->l.num=DC_LoadN();
        InitTrack(track);

	GENERAL_LOAD(4,10)

var0:
	track->onoff=DC_LoadI();
	goto start;
var1:
	track->trackname=DC_LoadS();
	goto start;
var2:
        if(track->patch==NULL){
          track->patch=PATCH_alloc(); // temporary object used during loading.
          track->patch->instrument = get_instrument_from_type(MIDI_INSTRUMENT_TYPE); // To support songs without instrument_type. (old songs)
        }
	track->patch->id=DC_LoadN();
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
        ATOMIC_SET(track->midi_channel, DC_LoadI());
        goto start;
        
var8:
	DC_LoadI(); // relvol is not used anymore
	goto start;

var9:
        if(track->patch==NULL)
          track->patch=PATCH_alloc(); // track->patch is replaced later. Only patch.id and instrument.id is used during loading.
        track->patch->instrument = get_instrument_from_type(DC_LoadI());
        goto start;

obj0:
	DC_ListAdd3_a(&track->notes,LoadNote());
	goto start;
obj1:
	LoadStops(&track->stops);
	goto start;
obj2:
        VECTOR_push_back(&track->fxs, LoadFXs(track));
	goto start;
obj3:
        DC_ListAdd3_a(&track->swings, LoadSwing());
        goto start;

var10:
var11:
var12:
var13:
var14:
var15:
var16:
var17:
var18:
var19:
 var20:
 var21:
        
obj4:
obj5:
obj6:

error:
end:
	return track;
}


extern void DLoadInstrumentData(struct Root *newroot,struct Tracks *track);

void DLoadTracks(const struct Root *newroot,struct Tracks *track, bool dload_all){
if(track==NULL) return;

         if(track->patch->id==-1){
           track->patch=NULL;
         }else{
           track->patch=PATCH_get_from_id(track->patch->id);
         }

        DLoadNotes(newroot, track, track->notes);
        
        DLoadFXs(newroot, track, &track->fxs);

if (dload_all)
  DLoadTracks(newroot,NextTrack(track), true);
}


