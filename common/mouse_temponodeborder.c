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
#include "list_proc.h"
#include "windows_proc.h"
#include "temponodes_proc.h"
#include "player_proc.h"
#include "blts_proc.h"
#include "visual_proc.h"
#include "gfx_wblocks_proc.h"
#include "gfx_wtracks_proc.h"
#include "gfx_wtrackheaders_proc.h"
#include "wblocks_proc.h"
#include "sliders_proc.h"
#include "gfx_wblocks_reltempo_proc.h"
#include "gfx_tempotrackheader_proc.h"
#include "gfx_subtrack_proc.h"

#include "mouse_temponodeborder_proc.h"




int MoveTempoNodeBorder_Mouse(
	struct Tracker_Windows *window,
	int x,int y
){
	struct WBlocks *wblock=window->wblock;
	struct WTracks *wtrack2;
	NInt oldrighttrack;

        if(window->prevaction.action==NOACTION)
          window->must_redraw=true; // fix up slightly skewed gradient caused by the direct blitting. (blitting is wrong, but it's faster)

//	PlayStop();

	x-=window->prevaction.eint1;

	int old_width=wblock->temponodearea.width;
	oldrighttrack=wblock->right_track;

        //int max_x2 = R_MAX(wblock->t.x2-window->fontwidth*10, leftmost_track->x2 - 5);
        //printf("max_x2: %d, leftmost_track->x2-5: %d\n",max_x2,leftmost_track->x2-5);

	x=R_BOUNDARIES(wblock->temponodearea.x+3, x, wblock->t.x2-window->fontwidth*10);

        int new_width = x - wblock->temponodearea.x;
        if(old_width == new_width)
          return 0;

#if 1
        struct WTracks *leftmost_track = ListFindElement1(&wblock->wtracks->l,wblock->left_track);
        int change = new_width - old_width;
        //printf("Change: %d, leftmost->x2: %d, wblock->t.x2: %d\n",change,leftmost_track->x2,wblock->t.x2);
        if(leftmost_track->x2 + change >= wblock->t.x2)
          return 0;
#endif

        if(new_width<=0){
          RError("New temponodearea <=0: %d. Old width: %d. Setting to 5.",new_width,wblock->temponodearea.width);
          new_width = 5;
        }

	wblock->temponodearea.width = new_width;

#if 1
	//	UpdateWTempoNodes(window,wblock);

	//	if(window->prevaction.action==NOACTION){
	//	  DrawUpTrackerWindow(window);
	//	}else{
	  //	  UpdateTrackerWindow(window);
	  GFX_BitBlt(window,wblock->temponodearea.width-old_width,0,
		       wblock->temponodearea.x2-2,wblock->a.y1,
		       wblock->a.x2,wblock->t.y2
		       );
	  /*
	  GFX_BitBlt(window,wblock->temponodearea.width-oldwidth,0,
		       wblock->temponodearea.x2-2,wblock->a.y1,
		       wblock->a.x2,wblock->t.y1
		       );
	  */
#endif

	  UpdateWBlockCoordinates(window,wblock);

#if 1
	  DrawUpWTempoNodes(window,wblock);
	  //	  Blt_blt(window);

	  if(old_width>wblock->temponodearea.width){
	    wtrack2=ListFindElement1(&wblock->wtracks->l,oldrighttrack);
	    while(wtrack2!=NULL && wtrack2->l.num<=wblock->right_track){
	      DrawUpWTrack(window,wblock,wtrack2);
	      DrawWTrackHeader(window,wblock,wtrack2);
	      wtrack2=NextWTrack(wtrack2);
	    }
	  }else{
	    while(GetXSubTrack_B2(wblock,window->curr_track,window->curr_track_sub)+3>=wblock->a.x2){
	      window->curr_track_sub--;
	      if(window->curr_track_sub==-2){
		wblock->wtrack=ListFindElement1(&wblock->wtracks->l,window->curr_track-1);
		window->curr_track_sub=wblock->wtrack->num_vel-1;
		window->curr_track--;
	      }
	    }
	  }

	  wtrack2=ListLast1(&wblock->wtracks->l);
	  if(wtrack2->fxarea.x2<wblock->a.x2){
	    GFX_FilledBox(window,0,
                          wtrack2->fxarea.x2+1,wblock->a.y1,
                          wblock->a.x2,wblock->t.y1,
                          PAINT_BUFFER);
	    GFX_FilledBox(window,0,
                          wtrack2->fxarea.x2+3,wblock->a.y1,
                          wblock->a.x2,wblock->t.y2,
                          PAINT_BUFFER);
	    //	    GFX_P_FilledBox(window,0,wtrack2->fxarea.x2+2,0           ,wblock->a.x2,wblock->t.y1);

	  }

	  DrawBlockRelTempo(window,wblock);
	  UpdateTempoTrackHeader_reltempo(window,wblock,2);
	  DrawBottomSlider(window);
	 
	  GFX_FilledBox(
			window,0,
			wblock->reltempo.x2+1,
			window->height - window->bottomslider.width+1,
			window->bottomslider.x-1,
			window->height-1,
                        PAINT_DIRECTLY
                        );
	  
	  
	  Blt_mark(window,wblock->top_realline,wblock->bot_realline,
		   wblock->temponodearea.x2,wblock->a.x2
		   );

	  Blt_marktrackheader(window,wblock->left_track,wblock->right_track);
	  //	}
#endif

          //        window->must_redraw=true;

	return 0;
}


void SetMouseActionTempoNodeBorder(struct Tracker_Windows *window, struct MouseAction *action,int x){
	action->action=TEMPONODEBORDER;
	action->MouseUpFunction= &MoveTempoNodeBorder_Mouse;
	action->eint1=x-window->wblock->temponodearea.x2;
}


