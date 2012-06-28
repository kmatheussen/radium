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
#include "visual_proc.h"
#include "trackreallineelements_proc.h"
#include "windows_proc.h"
#include "reallines_proc.h"
#include "wblocks_proc.h"
#include "settings_proc.h"

#include "window_config_proc.h"


void SelectEditFont(
	struct Tracker_Windows *window
){
	struct WBlocks *wblock=window->wblock;

        char *font = GFX_SelectEditFont(window);
	if(font==NULL)
          return;

	while(FreeANotShowedWBlockTREelement());

	window->leftslider.width=window->fontwidth;

	UpdateAllWBlockWidths(window);

	UpdateReallinesDependens(window,wblock);

	GFX_ClearWindow(window);window->blt.clear_do=false;
	DrawUpTrackerWindow(window);

        fprintf(stderr,"font: -%s-\n",font);
        // Saved last in case the font crashes radium
        SETTINGS_write_string("font",font);
}

void SelectLeftSliderWidth(
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
		window->wblock->temponodearea.x2
	);
	if(newwidth<4) return;

	window->leftslider.width=newwidth;

	UpdateAllWBlockCoordinates(window);

	GFX_ClearWindow(window);window->blt.clear_do=false;
	DrawUpTrackerWindow(window);
}

void SelectBottomSliderHeight(
	struct Tracker_Windows *window,
	ReqType reqtype
){
	char temp[100];
	int newwidth;

	sprintf(temp,"New Height (now %d) >",window->bottomslider.width);

	newwidth=GFX_GetInteger(
		window,
		reqtype,
		temp,
		2,
		window->height-(window->wblock->t.y1+(window->fontheight*6))
	);
	if(newwidth<2) return;

	window->bottomslider.width=newwidth;

	UpdateAllWBlockCoordinates(window);

	GFX_ClearWindow(window);window->blt.clear_do=false;
	DrawUpTrackerWindow(window);
}

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
		20
	);
	if(newwidth<1) return;

	window->minnodesize=newwidth;

	while(FreeANotShowedWBlockTREelement());

	UpdateReallinesDependens(window,wblock);

	DrawUpTrackerWindow(window);
}

void TextBorderOn(
	struct Tracker_Windows *window
){
	window->textborder=true;
	DrawUpTrackerWindow(window);
}

void TextBorderOff(
	struct Tracker_Windows *window
){
	window->textborder=false;
	DrawUpTrackerWindow(window);
}

void Window_config(
	struct Tracker_Windows *window
){
	ReqType reqtype;

	char *menutext[6]={
		"Select Edit Font",
		"Text Border on",
		"Text Border off",
		"Left Slider width",
		"Bottom Slider height",
		"Minimum node-size"
	};

	int sel;

	reqtype=GFX_OpenReq(window,30,12,"Window Config");

	sel=GFX_Menu(window,reqtype,"Select operation",6,menutext);

	switch(sel){
		case -1: break;
		case 0:
			SelectEditFont(window);
			break;
		case 1:
			TextBorderOn(window);
			break;
		case 2:
			TextBorderOff(window);
			break;
		case 3:
			SelectLeftSliderWidth(window,reqtype);
			break;
		case 4:
			SelectBottomSliderHeight(window,reqtype);
			break;
		case 5:
			SelectMinNodeSize(window,reqtype);
			break;
	}

	GFX_CloseReq(window,reqtype);
}



