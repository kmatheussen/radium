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
#include "../../common/disk.h"
#include "camd_i_plugin.h"
#include "camd_fx_proc.h"

#include "disk_camd_fx_proc.h"



void CAMDSaveFX(struct FX *fx,struct Tracks *track){
	struct CAMD_FX *camd_fx=(struct CAMD_FX *)fx->fxdata;

DC_start("FXDATA");

	DC_SSI("cc",camd_fx->cc);

DC_end();
}


void *CAMDLoadFX(struct FX *fx,struct Tracks *track){
	static char **objs=NULL;
	static char *vars[1]={"cc"};

	struct TrackInstrumentData *tid=(struct TrackInstrumentData *)track->instrumentdata;
	struct UsedTrackMidiCCs *usmf;

	struct CAMD_FX *camd_fx=DC_alloc(sizeof(struct CAMD_FX));
	fx->closeFX=CAMD_closeFX;
	fx->SaveFX=CAMDSaveFX;

	camd_fx->name=fx->name;
	camd_fx->min=fx->min;
	camd_fx->max=fx->max;

	usmf=talloc(sizeof(struct UsedTrackMidiCCs));
	usmf->next=tid->usmf;
	tid->usmf=usmf;
	usmf->camd_fx=camd_fx;

	GENERAL_LOAD(0,1)

var0:
	camd_fx->cc=DC_LoadI();
	if( ! CAMDSetTreatFX(fx,camd_fx)){
		dc.success=false;
	}
	goto start;

var1:
var2:
var3:
var4:
var5:
var6:
var7:
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

obj0:
obj1:
obj2:
obj3:
obj4:
obj5:
obj6:

error:
end:
	return camd_fx;
}













