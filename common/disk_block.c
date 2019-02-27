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
#include "disk_swing_proc.h"

#include "time_proc.h"
#include "Beats_proc.h"
#include "visual_proc.h"
#include "../OpenGL/Widget_proc.h"

#include "disk_block_proc.h"



void SaveBlock(struct Blocks *block, bool save_all){
if(block==NULL) return;
DC_start("BLOCK");

	DC_SaveN(block->l.num);

	DC_SSS("name",block->name);

	DC_SSN("num_tracks",block->num_tracks);
	DC_SSI("num_lines",block->num_lines);
	DC_SSF("reltempo",ATOMIC_DOUBLE_GET(block->reltempo));
        DC_SSS("color", GFX_get_colorname_from_color(block->color));
        
	SaveTrack(block->tracks, true);
	SaveSignatures(block->signatures);
        SaveLPBs(block->lpbs);
	SaveTempos(block->tempos);
	SaveTempoNodes(block->temponodes);
        SaveSwings(block->swings);

DC_end();
if(save_all)
  SaveBlock(NextBlock(block), true);
}



struct Blocks *LoadBlock(void){
	static const char *objs[6]={
		"TRACK",
		"SIGNATURES",
                "LPBs",
		"TEMPOS",
		"RELTEMPO",
                "SWING"
	};
	static const char *vars[6]={
		"name",
		"num_tracks",
		"num_lines",
		"reltempo",
                "color",
                "swing_enabled"
	};
	struct Blocks *block=DC_alloc(sizeof(struct Blocks));
	ATOMIC_DOUBLE_SET(block->reltempo, 1.0);

	block->l.num=DC_LoadN();
        block->color = GFX_MakeRandomBlockColor(); //GFX_mix_colors(GFX_MakeRandomColor(), GFX_get_color(HIGH_EDITOR_BACKGROUND_COLOR_NUM), 0.82f);
        block->swing_enabled = true;
        
	GENERAL_LOAD(6,6)

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
	ATOMIC_DOUBLE_SET(block->reltempo, DC_LoadD());
	goto start;
var4:
        block->color = GFX_get_color_from_colorname(DC_LoadS());
        goto start;
var5:
        block->swing_enabled = DC_LoadB();
        goto start;
        
obj0:
	DC_ListAdd1(&block->tracks,LoadTrack());
        g_editor_blocks_generation++;
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
obj5:
        DC_ListAdd3_a(&block->swings, LoadSwing());
        goto start;

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
 var21:
obj6:

error:
end:
	return block;

}

void DLoadBlocks(const struct Root *newroot,struct Blocks *block, bool dload_all){
if(block==NULL) return;

	block->lasttemponode=(struct TempoNodes *)ListLast3(&block->temponodes->l);

	DLoadTracks(newroot,block->tracks, true);

        TIME_everything_in_block_has_changed2(block, newroot, newroot->song);
        
if(dload_all)        
  DLoadBlocks(newroot,NextBlock(block), true);
}



