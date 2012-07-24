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


#include "gfx_upperleft_proc.h"

extern struct Root *root;


void GFX_UpdateQuantitize(
	struct Tracker_Windows *window,
	struct WBlocks *wblock
){
	char temp[52];

	if(root->quantitize<=1.0f){
		sprintf(temp,"/%f",1/root->quantitize);
	}else{
		sprintf(temp,"%f",root->quantitize);
	}

        temp[GFX_get_num_characters(window, temp, wblock->linenumarea.x2 - 2)] = 0;

	GFX_Text(
		 window,2,temp,0,0,
                 wblock->linenumarea.x2 - 2,
                 TEXT_CLEAR,
                 PAINT_DIRECTLY
	);

}


void GFX_UpdateKeyOctave(
	struct Tracker_Windows *window,
	struct WBlocks *wblock
){
	char temp[4];

	sprintf(temp,"%X",root->keyoct/12);
	GFX_Text(
		 window,3,temp,0,window->org_fontheight+1,
                 wblock->lpbTypearea.x - 2,
                 TEXT_CLEAR,
                 PAINT_DIRECTLY
	);
}

void GFX_UpdateCurrLine(
	struct Tracker_Windows *window,
	struct WBlocks *wblock
){
	char temp[40];
	int line=wblock->reallines[wblock->curr_realline]->Tline;

	sprintf(temp,"  %d",line);
	GFX_Text(
		 window,1,temp+(line>99?2:line>9?1:0),wblock->linenumarea.x,
                 window->org_fontheight+1,
                 wblock->linenumarea.x2-wblock->linenumarea.x,
                 TEXT_CLEAR,
                 PAINT_DIRECTLY
	);
}


void GFX_UpdateUpLeft(
	struct Tracker_Windows *window,
	struct WBlocks *wblock
){
	GFX_UpdateKeyOctave(window,wblock);
	GFX_UpdateCurrLine(window,wblock);
	GFX_UpdateQuantitize(window,wblock);
}


void GFX_DrawUpLeft(
	struct Tracker_Windows *window,
	struct WBlocks *wblock
){
#if 1
	GFX_FilledBox(
		window,0,
		0,
		wblock->a.y1,
		wblock->lpbTypearea.x-1,
		wblock->t.y1-2,
                PAINT_DIRECTLY
	);
#endif
	GFX_UpdateKeyOctave(window,wblock);
	GFX_UpdateCurrLine(window,wblock);
	GFX_UpdateQuantitize(window,wblock);
}







