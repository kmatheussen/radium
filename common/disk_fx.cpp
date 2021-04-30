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
#include "../config/config.h"
#include "TimeData.hpp"
#include "FX.hpp"
#include "disk.h"
#include "patch_proc.h"

#include "disk_fx_proc.h"


void SaveFX(struct FX *fx,struct Tracks *track){
if(fx==NULL) return;
DC_start("FX");
        DC_SaveN(fx->effect_num); // Not used, but we save effect_num to be compatible with earlier versions of radium.
	DC_SSS("name",fx->name);
	DC_SSI("color",fx->color);
	DC_SSI("min",fx->min);
	DC_SSI("max",fx->max);
        DC_SSB("is_enabled",fx->is_enabled);
	DC_SSI("effect_num",fx->effect_num);
        DC_SSL("patchnum",fx->patch->id.id);

	(*fx->SaveFX)(fx,track);

DC_end();
//SaveFX((struct FX *)fx->l.next,track);
}


struct FX *LoadFX(struct Tracks *track){

	const char *objs[1]={
		"FXDATA"
	};
	const char *vars[7]={
		"name",
		"color",
		"min",
		"max",
                "is_enabled",
                "effect_num",
                "patchnum"
	};
	struct FX *fx=(struct FX*)DC_alloc(sizeof(struct FX));
	DC_LoadN(); // ignore. Earlier, there was a field called fx->num, which was just a number containing fx->effect_num.

        fx->is_enabled = true;
        fx->patch = PATCH_alloc(); // temporary object used during loading.
        fx->patch->id = make_instrument(-1); // for loading older songs.
                
	GENERAL_LOAD(0,7)


var0:
	fx->name=DC_LoadSNoMatterWhat();
	goto start;
var1:
	fx->color=(ColorNums)DC_LoadI();
	if(dc.colorize==true){
		fx->color=(ColorNums)dc.startcolor;
		dc.startcolor++;
		if(dc.startcolor==256){
			dc.startcolor=4;
		}
	}
	goto start;
var2:
	fx->min=DC_LoadI();
	goto start;
var3:
	fx->max=DC_LoadI();
	goto start;
var4:
	fx->is_enabled=DC_LoadB();
	goto start;
        
var5:
        fx->effect_num=DC_LoadI();
	goto start;

var6:
        fx->patch->id = make_instrument(DC_LoadL());
        goto start;
        
obj0:
	fx->fxdata=(*track->patch->instrument->LoadFX)(fx,track);
	goto start;


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
var21:
        
obj1:
obj2:
obj3:
obj4:
obj5:
obj6:

error:
end:

	return fx;
}


void DLoadFX(const struct Root *newroot,struct Tracks *track, struct FXs *fxs, struct FX *fx){
  instrument_t id = fx->patch->id;
  
  if (id.id==-1)
    fx->patch = track->patch;
  else
    fx->patch = PATCH_get_from_id(id);
}

