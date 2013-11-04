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
#include "../audio/SoundPlugin.h"
#include "../Qt/Qt_instruments_proc.h"

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

        bool is_current_track = get_current_instruments_gui_patch()==wtrack->track->patch;

        if(true){
          struct WTracks *next_wtrack=NextWTrack(wtrack);

          int colornum = 15;

          if (wtrack->track->patch!=NULL)
            colornum = wtrack->track->patch->colornum;

          bool is_last_track = next_wtrack==NULL || wtrack->track->patch != next_wtrack->track->patch;

          int x1 = wtrack->x;
          int x2 = wtrack->x2; //is_last_track ? wtrack->x2+2 : next_wtrack->x;
          int y1 = wtrack->y+3;
          int y2 = wblock->t.y1-1;

          if(is_current_track)
            GFX_SetMixColor(window, 7, colornum, 800);
          else
            GFX_SetMixColor(window, 7, colornum, 0);
          GFX_T_FilledBox(window, 7,
                          x1,y1,x2,y2,
                          PAINT_BUFFER);

          // vertical left
          GFX_SetMixColor(window, 1, colornum, 200);
          GFX_T_Line(window, 1,
                     x1,y1,x1,y2,
                     PAINT_BUFFER);

          // horizontal top
          GFX_SetMixColor(window, 1, colornum, 200);
          GFX_T_Line(window, 1,
                     x1,y1,x2,y1,
                     PAINT_BUFFER);

          if(is_last_track){
            // vertical right
            GFX_SetMixColor(window, 1, colornum, 200);
            GFX_Line(window, 1,
                     x2,y1,x2,y2,
                     PAINT_BUFFER);
          }
        }

        GFX_SetClipRect(window,R_MAX(wtrack->x,wblock->temponodearea.x2),0,wtrack->x2,wblock->t.y1,PAINT_BUFFER);
        {
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
        }
        GFX_CancelClipRect(window,PAINT_BUFFER);

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
       int colornum = 0;

       //if (wtrack->track->patch!=NULL)
       //  colornum = wtrack->track->patch->colornum;

	GFX_T_FilledBox(
                        window,colornum,
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
