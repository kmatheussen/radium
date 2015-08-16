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
#include "disk.h"
#include "disk_fx_proc.h"
#include "disk_fxnodelines_proc.h"

#include "disk_fxs_proc.h"




void SaveFXs(struct FXs *fxs,struct Tracks *track){
if(fxs==NULL) return;
DC_start("FXS");

	DC_SaveN(fxs->l.num);

	SaveFX(fxs->fx,track);
	SaveFXNodeLines(fxs->fxnodelines);


DC_end();
SaveFXs(NextFX(fxs),track);
}

struct FXs *LoadFXs(struct Tracks *track){
	static char *objs[2]={
		"FX",
		"FXNODELINES"
	};
	static char **vars=NULL;

	struct FXs *fxs=DC_alloc(sizeof(struct FXs));

	printf("\tLoadFXs_start\n");
	fxs->l.num=DC_LoadN();

	GENERAL_LOAD(2,0)

obj0:
	fxs->fx=LoadFX(track);
        fxs->l.num = fxs->fx->effect_num; // Sometimes fx->effect_num is the only one containing the right value. (bug in previous version)
	goto start;
obj1:
	LoadFXNodeLines(&fxs->fxnodelines);
	goto start;


obj2:
obj3:
obj4:
obj5:
obj6:

var0:
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
var19:
 var20:

error:
	printf("\tLoadFXs_error\n");
end:

	printf("\tLoadFXs_end\n");
	return fxs;
}


void DLoadFXs(struct Root *newroot,struct Tracks *track, struct FXs *fxs){
if(fxs==NULL) return;

   DLoadFX(newroot, track, fxs, fxs->fx);
        
DLoadFXs(newroot,track, NextFX(fxs));
}



