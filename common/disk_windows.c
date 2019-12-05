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
#include "disk_wblock_proc.h"
#include "disk_slider_proc.h"
#include "windows_proc.h"
#include "visual_proc.h"
#include "sliders_proc.h"
#include "reallines_proc.h"
#include "control_proc.h"
#include "settings_proc.h"

#include "../OpenGL/Widget_proc.h"

#include "../api/api_proc.h"


#include "disk_windows_proc.h"




void SaveWindow(struct Tracker_Windows *window){
if(window==NULL) return;
DC_start("TRACKER_WINDOW");

	DC_SaveN(window->l.num);

	DC_SSI("x",window->x);
	DC_SSI("y",window->y);

	DC_SSI("width",window->width);
	DC_SSI("height",window->height);

	DC_SSS("fontname",window->fontname);
	DC_SSI("fontID",window->fontID);
	DC_SSI("fontTags",window->fontTags);
	DC_SSI("fontwidth",window->fontwidth);
	DC_SSI("fontheight",window->fontheight);
	DC_SSI("org_fontheight",window->org_fontheight);

	DC_SSN("curr_block",window->wblock->l.num); // Save window->wblock->l.num instead of window->curr_block since window->curr_block might not always have a legal value.

	DC_SSI("maxwtracksize",window->maxwtracksize);

	DC_SSB("playalong",window->playalong);

	//DC_SSB("textborder",window->textborder);

	//DC_SSI("minnodesize",window->minnodesize);

        // Compatibility with older songs. (probably not necessary)
        {
          DC_SaveS("leftslider");SaveSlider(&window->leftslider);

          struct Slider bottomslider = {1, window->bottomslider_height, getTrackSliderX1(), getTrackSliderX2(), getTrackSliderX1(), getTrackSliderX2()};
          DC_SaveS("bottomslider");SaveSlider(&bottomslider);
        }
        
        DC_SSB("show_signature_track",window->show_signature_track);
        DC_SSB("show_lpb_track",window->show_lpb_track);
        DC_SSB("show_bpm_track",window->show_bpm_track);
        DC_SSB("show_reltempo_track",window->show_reltempo_track);
        DC_SSB("show_swing_track",window->show_swing_track);

	SaveWBlock(window->wblocks, true);


DC_end();
SaveWindow(NextWindow(window));
}



struct Tracker_Windows *LoadWindow(void){
	static const char *objs[1]={
		"WBLOCK"
	};
	static const char *vars[22]={
		"x",
		"y",
		"width",
		"height",
		"fontname",
		"fontID",
		"fontTags",
		"fontwidth",
		"fontheight",
		"curr_block",
		"maxwtracksize",
		"playalong",
		"leftslider",
		"bottomslider",                
		"textborder",
		"org_fontheight",
		"minnodesize",
                "show_signature_track",
                "show_lpb_track",
                "show_bpm_track",
                "show_reltempo_track",
                "show_swing_track"
	};
        struct Slider dummy;
	struct Tracker_Windows *window=DC_alloc(sizeof(struct Tracker_Windows));
	window->l.num=DC_LoadN();

        window->show_signature_track=true;
        window->show_swing_track=true;
        window->show_lpb_track=true;
        window->show_bpm_track=true;
        window->show_reltempo_track=true;

	GENERAL_LOAD(1,22)

var0:
	//window->x=
        DC_LoadI();
	goto start;
var1:
	//window->y=
        DC_LoadI();
	goto start;
var2:
	//window->width=
        DC_LoadI();
	goto start;
var3:
	//window->height=
        DC_LoadI();
	goto start;
var4:
	//window->fontname=
        DC_LoadS();
	goto start;
var5:
	//window->fontID=
        DC_LoadI();
	goto start;
var6:
	//window->fontTags=
        DC_LoadI();
	goto start;
var7:
	//window->fontwidth=
        DC_LoadI();
	goto start;
var8:
	//window->fontheight=
        DC_LoadI();
	goto start;
var9:
	window->curr_block=DC_LoadN();        
	goto start;
var10:
	//window->maxwtracksize=
        DC_LoadI();
	goto start;
var11:
	window->playalong=DC_LoadB();
	goto start;
var12:
	//LoadSlider(&window->leftslider);
	LoadSlider(&dummy);
	goto start;
var13:
	//LoadSlider(&window->bottomslider);
	LoadSlider(&dummy);
	goto start;
var14:
	//window->textborder=DC_LoadB();
        DC_LoadB();
	goto start;
var15:
	//window->org_fontheight=
        DC_LoadI();
	goto start;
var16:
	//window->minnodesize=DC_LoadI();
        DC_LoadI();
	goto start;

var17:
        window->show_signature_track=DC_LoadB();
	goto start;

var18:
        window->show_lpb_track=DC_LoadB();
	goto start;

var19:
        window->show_bpm_track=DC_LoadB();
	goto start;

var20:
        window->show_reltempo_track=DC_LoadB();
	goto start;

var21:
        window->show_swing_track=DC_LoadB();
	goto start;

obj0:
	DC_ListAdd1(&window->wblocks,LoadWBlock());
	goto start;

obj1:
obj2:
obj3:
obj4:
obj5:
obj6:
error:
end:
	if(window->org_fontheight<window->fontheight+2){
		window->fontheight=window->org_fontheight+2;
	}
	return window;
}


// Sigh. Most of this is a reimplementation of OpenTrackerWindow.

void DLoadWindows(struct Root *newroot,struct Tracker_Windows *window){
if(window==NULL) return;

        //window->textborder=SETTINGS_read_bool("show_text_border",false);
	//window->minnodesize=SETTINGS_read_int("minimum_node_size",20);

	if(GFX_CreateVisual(window)!=0){
		EndProgram();
		exit(2);
	}

	ATOMIC_WRITE(window->curr_track, 0);
	window->curr_track_sub=-1;

        InitSliderValues(window);

	DLoadWBlocks(newroot,window,window->wblocks, true);

	window->wblock=(struct WBlocks *)ListFindElement1(&window->wblocks->l,window->curr_block);
        ATOMIC_SET(g_curr_block, window->wblock->block);
        
	if(window->wblock->isgfxdatahere==false){
		UpdateReallinesDependens(window,window->wblock);
	}

        // Not necessary since current opengl realline is set when rendering.
        //GE_set_curr_realline(window->wblock->curr_realline);

DLoadWindows(newroot,NextWindow(window));
}












