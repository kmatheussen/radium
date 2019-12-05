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
#include "vector_proc.h"
#include "visual_proc.h"
#include "windows_proc.h"
#include "reallines_proc.h"
#include "wblocks_proc.h"
#include "settings_proc.h"

#include "window_config_proc.h"


#if 0
void SelectEditFont(
	struct Tracker_Windows *window
){
	struct WBlocks *wblock=window->wblock;

        char *font = GFX_SelectEditFont(window);
	if(font==NULL)
          return;

	window->leftslider.width=window->fontwidth;

	UpdateAllWBlockWidths(window);

	UpdateReallinesDependens(window,wblock);

        window->blt.clear_do=false;
	window->must_redraw = true;

        printf("font: -%s-\n",font);
        // Saved last in case the font crashes radium
        SETTINGS_write_string("font",font);
}
#endif

static void SelectLeftSliderWidth(
	struct Tracker_Windows *window,
	ReqType reqtype
){
	char temp[100];
	int newwidth;

	sprintf(temp,"New Width (now %d) >",window->leftslider.width);

	newwidth=GFX_GetInteger(
		window,
		reqtype,
		temp,
		4,
		window->wblock->temponodearea.x2,
                true
                                );
	if(newwidth<4) return;

	window->leftslider.width=newwidth;

	UpdateAllWBlockCoordinates(window);

        window->blt.clear_do=false;
	window->must_redraw = true;

        SETTINGS_write_int("left_slider_width",newwidth);
}

extern int g_default_slider_height;

static void SelectBottomSliderHeight(
	struct Tracker_Windows *window,
	ReqType reqtype
){
	char temp[100];
	int newwidth;

	sprintf(temp,"New Height (Now: %d. Use 0 to set automatic value) >",window->bottomslider_height);

	newwidth=GFX_GetInteger(
		window,
		reqtype,
		temp,
		0,
		window->height-(window->wblock->t.y1+(window->fontheight*6)),
                true
	);
	if(newwidth<0) return;

        GFX_set_bottom_widget_height(newwidth < 2 ? g_default_slider_height - 2 : newwidth);
        
	UpdateAllWBlockCoordinates(window);

        window->blt.clear_do=false;
	window->must_redraw = true;

        SETTINGS_write_int("bottom_slider_height",newwidth);
}


int beat_opacity = -1;

static void SetBeatOpacity(
	struct Tracker_Windows *window,
	ReqType reqtype
){
	char temp[1000];
	int new_opacity;

	sprintf(temp,"New Beat Opacity (0-1000) (now %d) >",beat_opacity);

        new_opacity = GFX_GetInteger(
		window,
		reqtype,
		temp,
		0,
		1000,
                true
	);
	if(new_opacity<0 || new_opacity>1000)
          return;

        beat_opacity = new_opacity;

        window->wblock->block->is_dirty = true;

        SETTINGS_write_int("beat_opacity", new_opacity);
}

int first_beat_opacity = -1;

static void SetFirstBeatOpacity(
	struct Tracker_Windows *window,
	ReqType reqtype
){
	char temp[1000];
	int new_opacity;

	sprintf(temp,"New First Beat Opacity (0-1000) (now %d) >",first_beat_opacity);

        new_opacity = GFX_GetInteger(
		window,
		reqtype,
		temp,
		0,
		1000,
                true
	);
	if(new_opacity<0 || new_opacity>1000)
          return;

        first_beat_opacity = new_opacity;

        window->wblock->block->is_dirty = true;

        SETTINGS_write_int("first_beat_opacity", new_opacity);
}

int line_opacity = -1;

static void SetLineOpacity(
	struct Tracker_Windows *window,
	ReqType reqtype
){
	char temp[1000];
	int new_opacity;

	sprintf(temp,"New Line Opacity (0-1000) (now %d) >",line_opacity);

        new_opacity = GFX_GetInteger(
		window,
		reqtype,
		temp,
		0,
		1000,
                true
	);
	if(new_opacity<0 || new_opacity>1000)
          return;

        line_opacity = new_opacity;
        SETTINGS_write_int("line_opacity", new_opacity);

        window->must_redraw = true;
}

#if 0
void SelectMinNodeSize(
	struct Tracker_Windows *window,
	ReqType reqtype
){
	struct WBlocks *wblock=window->wblock;

	char temp[100];
	int newwidth;

	sprintf(temp,"New Size (now %d) >",window->minnodesize);

	newwidth=GFX_GetInteger(
		window,
		reqtype,
		temp,
		1,
		40,
                true
	);
	if(newwidth<1) return;

	window->minnodesize=newwidth;

	UpdateReallinesDependens(window,wblock);

	window->must_redraw = true;

        SETTINGS_write_int("minimum_node_size",newwidth);
}
#endif

void Window_config(
	struct Tracker_Windows *window
){
	ReqType reqtype=GFX_OpenReq(window,30,12,"Select which variable to modify:");

        vector_t v={};
        int leftsliderwidth    = VECTOR_push_back(&v,"Left Slider width");
        int bottomsliderheight = VECTOR_push_back(&v,"Bottom Slider height");
        int firstbeat          = VECTOR_push_back(&v, "First Beat Line color opacity");
        int beatlineopacity    = VECTOR_push_back(&v, "Beat Line color opacity");
        int linecoloropacity   = VECTOR_push_back(&v, "Line separate color opacity");
        //VECTOR_push_back(&v,"Minimum node-size");

	int sel=GFX_Menu(window,reqtype,"",v,true);
        
#if 0
        // case 0:
        SelectEditFont(window);
        break;
#endif
        if (sel==leftsliderwidth)
          SelectLeftSliderWidth(window,reqtype);

        else if (sel==bottomsliderheight)
          SelectBottomSliderHeight(window,reqtype);

        else if (sel==firstbeat)
          SetFirstBeatOpacity(window, reqtype);

        else if (sel==beatlineopacity)
          SetBeatOpacity(window, reqtype);

        else if (sel==linecoloropacity)
          SetLineOpacity(window, reqtype);
        
#if 0
        else if (sel==selminnodesize)
          SelectMinNodeSize(window,reqtype);
#endif


	GFX_CloseReq(window,reqtype);

        if (sel != -1)
          Window_config(window);
}



