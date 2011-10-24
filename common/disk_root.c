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
#include "disk_song_proc.h"

#include "disk_root_proc.h"



void SaveRoot(struct Root *theroot){
DC_start("ROOT");

//	DC_SSN("def_instrument",theroot->def_instrument->l.num);
	DC_SSN("curr_block",theroot->curr_block);
	DC_SSI("tempo",theroot->tempo);
	DC_SSI("lpb",theroot->lpb);
	DC_SSF("quantitize",theroot->quantitize);
	DC_SSI("keyoct",theroot->keyoct);
	DC_SSI("standardvel",theroot->standardvel);

	SaveSong(theroot->song);

DC_end();
}



/*********** Start Load song *************************/

struct Root *LoadRoot(void){
	static char *objs[1]={
		"SONG"
	};
	static char *vars[7]={
		"def_instrument",
		"curr_block",
		"tempo",
		"lpb",
		"quantitize",
		"keyoct",
		"standardvel"
	};
	struct Root *ret=DC_alloc(sizeof(struct Root));
	ret->scrollplayonoff=true;

	GENERAL_LOAD(1,7)



obj0:
	ret->song=LoadSong();
	goto start;
var0:
	goto start;			// Don't bother with instruments yet.
var1:
	ret->curr_block=DC_LoadN();
	goto start;
var2:
	ret->tempo=DC_LoadI();
	goto start;
var3:
	ret->lpb=DC_LoadI();
	goto start;

var4:
	ret->quantitize=DC_LoadF();
	goto start;

var5:
	ret->keyoct=DC_LoadI();
	goto start;

var6:
	ret->standardvel=DC_LoadI();
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

obj1:
obj2:
obj3:
obj4:
obj5:
obj6:
debug("loadroot, very wrong\n");

error:
debug("loadroot, goto error\n");

end:

	return ret;
}


extern struct Root *root;

void DLoadRoot(struct Root *theroot){
	DLoadSong(theroot,theroot->song);
}


/*********************** End Load Song **************************/


