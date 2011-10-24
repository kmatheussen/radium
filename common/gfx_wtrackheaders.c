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
#include "gfx_wtext_proc.h"
#include "visual_proc.h"
#include "list_proc.h"
#include "gfx_wtrackheader_volpan_proc.h"
#include "blts_proc.h"

#include "gfx_wtrackheaders_proc.h"


void DrawTempoHeader(
	struct Tracker_Windows *window,
	struct WBlocks *wblock
){
	
}


void UpdateWTrackHeader(
	struct Tracker_Windows *window,
	struct WBlocks *wblock,
	struct WTracks *wtrack
){
	static char temp[50];

	int maxwidth=(wtrack->notelength*window->fontwidth) + wtrack->fxwidth;
	maxwidth-=window->fontwidth*(wtrack->l.num>=10 ? wtrack->l.num>=100 ? 3 : 2 : 1);
	maxwidth-=2*window->fontwidth;
	maxwidth/=window->fontwidth;

	sprintf(temp,"%d: %.*s",wtrack->l.num,maxwidth,wtrack->track->patch==NULL ? wtrack->track->trackname : wtrack->track->patch->name);
	GFX_P_T_Text(
		window,1,temp,
		wtrack->x+1,
		wtrack->y,
		false
	);
	UpdatePanSlider(window,wblock,wtrack);
	UpdateVolumeSlider(window,wblock,wtrack);

	if(wtrack->track->onoff==0){
		GFX_P_T_Line(window,2,
			wtrack->x+2,wtrack->y+1,
			wtrack->x2-2,wtrack->y+(window->org_fontheight*2)-1
		);
		GFX_P_T_Line(window,2,
			wtrack->x2-2,wtrack->y+1,
			wtrack->x+2,wtrack->y+(window->org_fontheight*2)-1
		);
	}
	GFX_P_T_Line(window,1,wtrack->x,wblock->t.y1-1,wtrack->x2,wblock->t.y1-1);

	Blt_marktrackheader(window,wtrack->l.num,wtrack->l.num);
}




void DrawWTrackHeader(
	struct Tracker_Windows *window,
	struct WBlocks *wblock,
	struct WTracks *wtrack
){

	GFX_P_T_FilledBox(
		window,0,
		wtrack->x-1,wtrack->y,
		wtrack->x2+1,wtrack->y-1+(window->org_fontheight*2)
	);

	UpdateWTrackHeader(window,wblock,wtrack);
}




void UpdateAllWTrackHeaders(
	struct Tracker_Windows *window,
	struct WBlocks *wblock
){
	struct WTracks *wtrack=ListFindElement1(&wblock->wtracks->l,wblock->left_track);

	while(wtrack!=NULL && wtrack->l.num<=wblock->right_track){
	  if(wtrack->x>=wblock->a.x2){
	    break;
	  }
	  UpdateWTrackHeader(window,wblock,wtrack);
	  
	  wtrack=NextWTrack(wtrack);
	}
}

void DrawAllWTrackHeaders(
	struct Tracker_Windows *window,
	struct WBlocks *wblock
){
	struct WTracks *wtrack=ListFindElement1(&wblock->wtracks->l,wblock->left_track);

	while(wtrack!=NULL && wtrack->l.num<=wblock->right_track){
	  if(wtrack->x>=wblock->a.x2){
	    break;
	  }
	  DrawWTrackHeader(window,wblock,wtrack);
	  
	  wtrack=NextWTrack(wtrack);
	}
}


void DrawUpAllWTrackHeaders(
	struct Tracker_Windows *window,
	struct WBlocks *wblock
){
	struct WTracks *wtrack=ListFindElement1(&wblock->wtracks->l,wblock->left_track);

/*
	GFX_P_T_FilledBox(
		window,0,
		wblock->temponodearea.x2+3,wtrack->y,
		wblock->a.x2,wtrack->y-1+(window->org_fontheight*2)
	);
*/

	while(wtrack!=NULL && wtrack->l.num<=wblock->right_track){
	  if(wtrack->x>=wblock->a.x2){
	    break;
	  }
	  UpdateWTrackHeader(window,wblock,wtrack);
	  
	  wtrack=NextWTrack(wtrack);
	}
}
