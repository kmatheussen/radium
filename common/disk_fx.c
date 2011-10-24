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


void SaveFX(struct FX *fx,struct Tracks *track){
if(fx==NULL) return;
DC_start("FX");
	DC_SaveN(fx->l.num);
	DC_SSS("name",fx->name);
	DC_SSI("color",fx->color);
	DC_SSI("min",fx->min);
	DC_SSI("max",fx->max);

	(*fx->SaveFX)(fx,track);

DC_end();
SaveFX((struct FX *)fx->l.next,track);
}


struct FX *LoadFX(struct Tracks *track){

	static char *objs[1]={
		"FXDATA"
	};
	static char *vars[4]={
		"name",
		"color",
		"min",
		"max"
	};
	struct FX *fx=DC_alloc(sizeof(struct FX));
	fx->l.num=DC_LoadN();
	GENERAL_LOAD(0,4)


var0:
	fx->name=DC_LoadS();
	goto start;
var1:
	fx->color=DC_LoadI();
	if(dc.colorize==true){
		fx->color=dc.startcolor;
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
obj0:
	fx->fxdata=(*track->instrument->LoadFX)(fx,track);
	goto start;


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



