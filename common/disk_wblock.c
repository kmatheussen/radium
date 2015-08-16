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
#include "disk_localzooms_proc.h"
#include "disk_wtrack_proc.h"
#include "disk_warea_proc.h"
#include "wblocks_proc.h"

#include "disk_wblock_proc.h"




void SaveWBlock(struct WBlocks *wblock){
if(wblock==NULL) return;
DC_start("WBLOCK");

	DC_SaveL(wblock->l.num);

	DC_SSI("tempotrackonoff",wblock->tempotrackonoff);
	DC_SSI("temponodetrackonoff",wblock->temponodetrackonoff);

        // the "area" values are proabably no point loading/saving...

	//DC_SaveS("zoomlevelarea");SaveWArea(&wblock->zoomlevelarea);
	DC_SaveS("linenumarea");SaveWArea(&wblock->linenumarea);
        //DC_SaveS("zoomlinearea");SaveWArea(&wblock->zoomlevelarea);
	DC_SaveS("lpbTypearea");SaveWArea(&wblock->lpbTypearea);
	DC_SaveS("lpbarea");SaveWArea(&wblock->lpbarea);
	DC_SaveS("tempoTypearea");SaveWArea(&wblock->tempoTypearea);
	DC_SaveS("tempoarea");SaveWArea(&wblock->tempoarea);
	DC_SaveS("temponodearea");SaveWArea(&wblock->temponodearea);

	DC_SSI("maxwtracksize",wblock->maxwtracksize);

	DC_SSI("curr_realline",wblock->curr_realline);

	DC_SSI("num_reallines",wblock->num_reallines);

	DC_SSI("num_expand_lines",wblock->num_expand_lines);

	DC_SSF("reltempomax",wblock->reltempomax);

	DC_SSB("isranged",wblock->isranged);
	DC_SSL("rangex1",wblock->rangex1);
	DC_SSL("rangex2",wblock->rangex2);
	DC_SSL("rangey1",wblock->rangey1);
	DC_SSL("rangey2",wblock->rangey2);

	SaveLocalZooms(wblock->localzooms,wblock->block->num_lines);
	SaveWTrack(wblock->wtracks);

DC_end();
SaveWBlock(NextWBlock(wblock));
}


struct WBlocks *LoadWBlock(void){
	static char *objs[2]={
		"LOCALZOOMS",
		"WTRACK"
	};

	static char *vars[19]={
		"tempotrackonoff",
		"temponodetrackonoff",
		"zoomlevelarea", // not used anymore, kept for being able to load old songs
		"linenumarea",
		"lpbTypearea",
		"lpbarea",
		"tempoTypearea",
		"tempoarea",
		"temponodearea",
		"maxwtracksize",
		"curr_realline",
		"num_reallines",
                "num_expand_lines",
		"reltempomax",
		"isranged",
		"rangex1",
		"rangex2",
		"rangey1",
		"rangey2"
	};
	struct WBlocks *wblock=DC_alloc(sizeof(struct WBlocks));
	wblock->l.num=DC_LoadN();
	wblock->tempocolorarea.width=22;
        wblock->num_expand_lines=1;

        WArea dummy;

	GENERAL_LOAD(2,19)



var0:
	wblock->tempotrackonoff=DC_LoadI();
	goto start;
var1:
	wblock->temponodetrackonoff=DC_LoadI();
	goto start;
var2:
	//LoadWArea(&wblock->zoomlevelarea);
	LoadWArea(&dummy);
	goto start;
var3:
	//LoadWArea(&wblock->linenumarea);
	LoadWArea(&dummy);
	goto start;
var4:
	//LoadWArea(&wblock->lpbTypearea);
	LoadWArea(&dummy);
	goto start;
var5:
	//LoadWArea(&wblock->lpbarea);
	LoadWArea(&dummy);
	goto start;
var6:
	//LoadWArea(&wblock->tempoTypearea);
	LoadWArea(&dummy);
	goto start;
var7:
	//LoadWArea(&wblock->tempoarea);
	LoadWArea(&dummy);
	goto start;
var8:
	LoadWArea(&wblock->temponodearea);
	goto start;
var9:
	wblock->maxwtracksize=DC_LoadI();
	goto start;
var10:
	wblock->curr_realline=DC_LoadI();
	goto start;
var11:
	wblock->num_reallines=DC_LoadI();
	goto start;
var12:
	wblock->num_expand_lines = DC_LoadI();
	goto start;
var13:
	wblock->reltempomax=DC_LoadF();
	goto start;
var14:
	wblock->isranged=DC_LoadB();
	goto start;
var15:
	wblock->rangex1=DC_LoadN();
	goto start;
var16:
	wblock->rangex2=DC_LoadN();
	goto start;
var17:
	wblock->rangey1=DC_LoadN();
	goto start;
var18:
	wblock->rangey2=DC_LoadN();
	goto start;

obj0:
	LoadLocalZooms(&wblock->localzooms);
	goto start;
obj1:
	DC_ListAdd1(&wblock->wtracks,LoadWTrack());
	goto start;

        
var19:
 var20:
obj2:
obj3:
obj4:
obj5:
obj6:

error:
end:
	return wblock;
}

void DLoadWBlocks(
	struct Root *newroot,
	struct Tracker_Windows *window,
	struct WBlocks *wblock
){
if(wblock==NULL) return;

	window->wblock=wblock;

	wblock->title=talloc_atomic(128);

	wblock->block=(struct Blocks *)ListFindElement1(&newroot->song->blocks->l,wblock->l.num);
	wblock->wtrack=(struct WTracks *)ListFindElement1(&wblock->wtracks->l,window->curr_track);

	wblock->left_subtrack=-1;

	DLoadWTracks(newroot,window,wblock,wblock->wtrack);

        UpdateWBlockWidths(window,wblock);


        UpdateWBlockCoordinates(window,wblock);	//Does allso update wtrack	

	DLoadLocalZooms(newroot,window,wblock);

DLoadWBlocks(newroot,window,NextWBlock(wblock));
}








