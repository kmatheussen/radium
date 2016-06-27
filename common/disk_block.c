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
#include "disk_track_proc.h"
#include "disk_signatures_proc.h"
#include "disk_lpbs_proc.h"
#include "disk_tempos_proc.h"
#include "disk_temponodes_proc.h"
#include "time_proc.h"
#include "Beats_proc.h"
#include "../OpenGL/Widget_proc.h"

#include "disk_block_proc.h"



void SaveBlock(struct Blocks *block){
if(block==NULL) return;
DC_start("BLOCK");

	DC_SaveN(block->l.num);

	DC_SSS("name",block->name);

	DC_SSN("num_tracks",block->num_tracks);
	DC_SSI("num_lines",block->num_lines);
	DC_SSF("reltempo",block->reltempo);

	SaveTrack(block->tracks);
	SaveSignatures(block->signatures);
        SaveLPBs(block->lpbs);
	SaveTempos(block->tempos);
	SaveTempoNodes(block->temponodes);


DC_end();
SaveBlock(NextBlock(block));
}



struct Blocks *LoadBlock(void){
	static char *objs[5]={
		"TRACK",
		"SIGNATURES",
                "LPBs",
		"TEMPOS",
		"RELTEMPO"
	};
	static char *vars[4]={
		"name",
		"num_tracks",
		"num_lines",
		"reltempo"
	};
	struct Blocks *block=DC_alloc(sizeof(struct Blocks));
	block->reltempo=1.0f;

	block->l.num=DC_LoadN();

	GENERAL_LOAD(5,4)

var0:
	block->name=DC_LoadS();
	goto start;
var1:
	block->num_tracks=DC_LoadN();
	goto start;
var2:
	block->num_lines=DC_LoadI();
	goto start;
var3:
	block->reltempo=DC_LoadF();
	goto start;

obj0:
	DC_ListAdd1(&block->tracks,LoadTrack());
	goto start;
obj1:
	LoadSignatures(&block->signatures);
	goto start;
obj2:
	LoadLPBs(&block->lpbs);
	goto start;
obj3:
	LoadTempos(&block->tempos);
	goto start;
obj4:
	LoadTempoNodes(&block->temponodes);
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
var19:
 var20:
obj5:
obj6:

error:
end:
	return block;

}

extern struct Root *root;

void DLoadBlocks(struct Root *newroot,struct Blocks *block){
	struct Root *temp;
if(block==NULL) return;

	block->lasttemponode=(struct TempoNodes *)ListLast3(&block->temponodes->l);

	DLoadTracks(newroot,block->tracks);

        GL_pause_gl_thread_a_short_while();

        PLAYER_lock();{
          //GL_draw_lock();{
          temp=root;
          root=newroot;
        }PLAYER_unlock();
        GL_draw_lock();{
          UpdateSTimes(block);
          UpdateBeats(block);
        }GL_draw_unlock();
        PLAYER_lock();{
          root=temp;
          //}GL_draw_unlock();
        }PLAYER_unlock();
        
DLoadBlocks(newroot,NextBlock(block));
}



