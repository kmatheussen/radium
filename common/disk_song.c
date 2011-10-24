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
#include "disk_windows_proc.h"
#include "disk_block_proc.h"
#include "disk_playlist_proc.h"
#include "disk_instrument_proc.h"

#include "disk_song_proc.h"



void SaveSong(struct Song *song){
DC_start("SONG");

	DC_SSN("num_blocks",song->num_blocks);
	DC_SSI("length",song->length);
	DC_SSS("songname",song->songname);
	DC_SSN("maxtracks",song->maxtracks);

	SaveWindow(song->tracker_windows);
	SaveBlock(song->blocks);
	SavePlayList(song->playlist,song->length);

	SaveInstrument(song->instruments);

DC_end();
}


struct Song *LoadSong(void){
	static char *objs[4]={
		"TRACKER_WINDOW",
		"BLOCK",
		"PLAYLIST",
		"INSTRUMENT"
	};
	static char *vars[4]={
		"num_blocks",
		"length",
		"songname",
		"maxtracks",
	};
	struct Song *song=DC_alloc(sizeof(struct Song));

	GENERAL_LOAD(4,4)

obj0:
	DC_ListAdd1(&song->tracker_windows,LoadWindow());
	goto start;
obj1:
	DC_ListAdd1(&song->blocks,LoadBlock());
	goto start;
obj2:
	LoadPlayList();
	goto start;
obj3:
	song->instruments=LoadInstrument();
	goto start;

var0:
	song->num_blocks=DC_LoadI();
	goto start;
var1:
	song->length=DC_LoadI();
	goto start;
var2:
	song->songname=DC_LoadS();
	goto start;
var3:
	song->maxtracks=DC_LoadN();
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

obj4:
obj5:
obj6:

error:
end:

	return song;
}

extern struct Instruments *def_instrument;

void DLoadSong(struct Root *newroot,struct Song *song){

	DLoadInstrument(song->instruments);

	newroot->def_instrument=song->instruments;

	DLoadBlocks(newroot,song->blocks);
	DLoadPlayList(newroot,song);

	DLoadWindows(newroot,song->tracker_windows);

}







