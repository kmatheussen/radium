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

#include "disk_wtrack_proc.h"



void SaveWTrack(struct WTracks *wtrack){
if(wtrack==NULL) return;
DC_start("WTRACK");


	DC_SaveN(wtrack->l.num);

	DC_SSI("notesonoff",wtrack->notesonoff);
	DC_SSI("notelength",wtrack->notelength);
	DC_SSI("fxwidth",wtrack->fxwidth);
	DC_SSI("num_vel",wtrack->num_vel);


DC_end();
SaveWTrack(NextWTrack(wtrack));
}



struct WTracks *LoadWTrack(void){
	static char **objs=NULL;
	static char *vars[4]={
		"notesonoff",
		"notelength",
		"fxwidth",
		"num_vel"
	};
	struct WTracks *wtrack=DC_alloc(sizeof(struct WTracks));
	wtrack->l.num=DC_LoadN();

	GENERAL_LOAD(0,4);


var0:
	wtrack->notesonoff=DC_LoadI();
	goto start;
var1:
	wtrack->notelength=DC_LoadI();
	goto start;
var2:
	wtrack->fxwidth=DC_LoadI();
	goto start;
var3:
	wtrack->num_vel=DC_LoadI();
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

obj0:
obj1:
obj2:
obj3:
obj4:
obj5:
obj6:

error:
end:
	return wtrack;

}


void DLoadWTracks(
	struct Root *newroot,
	struct Tracker_Windows *window,
	struct WBlocks *wblock,
	struct WTracks *wtrack
){
if(wtrack==NULL) return;

	wtrack->fxonoff=1;
	wtrack->track=(struct Tracks *)ListFindElement1(&wblock->block->tracks->l,wtrack->l.num);


DLoadWTracks(newroot,window,wblock,NextWTrack(wtrack));
}


