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
	float quant;
	bool less=false;
	int precision;

	precision=wblock->lpbTypearea.x/(window->fontwidth+1);

	quant=root->quantitize;
	if(quant<=1.0f){
		less=true;
		quant=1/quant;
		precision--;
	}

	precision-=quant>=10.0?3:2;
	precision=R_MAX(0,precision);

	if(less){
		sprintf(temp,"/%.*f",precision,quant);
	}else{
		sprintf(temp,"%.*f",precision,quant);
	}

	GFX_Text(
		 window,2,temp,0,0,
                 wblock->lpbTypearea.x - 2,
                 TEXT_NOFLAGS
	);

}


void GFX_UpdateKeyOctave(
	struct Tracker_Windows *window,
	struct WBlocks *wblock
){
	char temp[4];

	sprintf(temp,"%X",root->keyoct/12);

	GFX_Text(
		 window,3,temp,0,window->org_fontheight,
                 wblock->lpbTypearea.x - 2,
                 TEXT_NOFLAGS
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
		 window,1,temp+(line>99?2:line>9?1:0),wblock->linenumarea.x,window->org_fontheight,
                 wblock->linenumarea.x2-wblock->linenumarea.x,
                 TEXT_NOFLAGS
	);
}


void GFX_UpdateUpLeft(
	struct Tracker_Windows *window,
	struct WBlocks *wblock
){
	GFX_UpdateQuantitize(window,wblock);
	GFX_UpdateKeyOctave(window,wblock);
	GFX_UpdateCurrLine(window,wblock);
}


void GFX_DrawUpLeft(
	struct Tracker_Windows *window,
	struct WBlocks *wblock
){
	GFX_FilledBox(
		window,0,
		0,
		wblock->a.y1,
		wblock->lpbTypearea.x-1,
		wblock->t.y1-2
	);
	GFX_UpdateQuantitize(window,wblock);
	GFX_UpdateKeyOctave(window,wblock);
	GFX_UpdateCurrLine(window,wblock);
}







