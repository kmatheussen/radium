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
	static char temp[500];

#if 0
	int maxwidth=(wtrack->notelength*window->fontwidth) + wtrack->fxwidth;
	maxwidth-=window->fontwidth*(wtrack->l.num>=10 ? wtrack->l.num>=100 ? 3 : 2 : 1);
	maxwidth-=2*window->fontwidth;
	maxwidth/=window->fontwidth;
#endif
	sprintf(temp,"%d: %s",wtrack->l.num,wtrack->track->patch==NULL ? wtrack->track->trackname : wtrack->track->patch->name);

#if 0
        int maxwidth = GFX_get_num_characters(window, temp, wtrack->x2 - wtrack->x - 1);
        //if(wtrack->l.num==0)
        //  printf("maxwidth: %d. x/x2: %d/%d\n",maxwidth,wtrack->x,wtrack->x2);
        temp[maxwidth] = 0;
#endif

	sprintf(temp,"%d:",wtrack->l.num);
	GFX_T_Text(
                   window,1,temp,
                   wtrack->x+window->fontwidth/2,
                   wtrack->y,
                   wtrack->x2-wtrack->x-1,
                   TEXT_CLIPRECT|TEXT_BOLD,
                   PAINT_BUFFER
	);

        int name_x = wtrack->x+window->fontwidth/2 + GFX_get_text_width(window,temp) + window->fontwidth;
	QUEUE_GFX_Text(
                   window,8,wtrack->track->patch==NULL ? wtrack->track->trackname : wtrack->track->patch->name,
                     name_x,
                     wtrack->y,
                     (wtrack->x2-window->fontwidth/2) - name_x,
                     TEXT_SCALE,
                     PAINT_BUFFER
                     );
	UpdatePanSlider(window,wblock,wtrack);
	UpdateVolumeSlider(window,wblock,wtrack);

	if(wtrack->track->onoff==0){
		GFX_T_Line(window,2,
                           wtrack->x+2,wtrack->y+1,
                           wtrack->x2-2,wtrack->y+(window->org_fontheight*2)-1,
                           PAINT_BUFFER
		);
		GFX_T_Line(window,2,
                           wtrack->x2-2,wtrack->y+1,
                           wtrack->x+2,wtrack->y+(window->org_fontheight*2)-1,
                           PAINT_BUFFER
		);
	}
	GFX_T_Line(window,1,wtrack->x,wblock->t.y1-1,wtrack->x2,wblock->t.y1-1,PAINT_BUFFER);

	Blt_marktrackheader(window,wtrack->l.num,wtrack->l.num);
}




void DrawWTrackHeader(
	struct Tracker_Windows *window,
	struct WBlocks *wblock,
	struct WTracks *wtrack
){

	GFX_T_FilledBox(
                        window,0,
                        wtrack->x-1,wtrack->y,
                        wtrack->x2+1,wtrack->y-1+(window->org_fontheight*2),
                        PAINT_BUFFER
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
	GFX_T_FilledBox(
		window,0,
		wblock->temponodearea.x2+3,wtrack->y,
		wblock->a.x2,wtrack->y-1+(window->org_fontheight*2),
                PAINT_BUFFER
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
