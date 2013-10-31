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
#include "mouse_temponodeborder_proc.h"
#include "list_proc.h"
#include "windows_proc.h"
#include "wblocks_proc.h"
#include "wtracks_proc.h"
#include "gfx_wtracks_proc.h"
#include "gfx_wtrackheaders_proc.h"
#include "trackreallines_proc.h"
#include "fxlines_proc.h"
#include "player_proc.h"
#include "blts_proc.h"
#include "visual_proc.h"
#include "gfx_wblocks_proc.h"
#include "wblocks_proc.h"
#include "sliders_proc.h"
#include "gfx_wblocks_reltempo_proc.h"
#include "gfx_tempotrackheader_proc.h"
#include "gfx_subtrack_proc.h"

#include "mouse_wtrackborder_proc.h"




int MoveWTrackBorder_Mouse(
	struct Tracker_Windows *window,
	int x,int y
){
	int oldfxwidth;
	struct WBlocks *wblock=window->wblock;
	struct WTracks *wtrack=(struct WTracks *)ListFindElement1_r0(&wblock->wtracks->l,(NInt)window->prevaction.eint1);

        if(window->prevaction.action==NOACTION)
          window->must_redraw=true; // fix up slightly skewed gradient caused by the direct blitting. (blitting is wrong, but it's faster)

	if(wtrack==NULL) return 0;

	x=R_BOUNDARIES(wtrack->fxarea.x+3, x, wblock->t.x2-3);

	oldfxwidth=wtrack->fxwidth;
        NInt oldrighttrack=wblock->right_track;

	wtrack->fxwidth=x - wtrack->fxarea.x;
	wtrack->fxwidth=R_MAX(wtrack->fxwidth,2);

	if(oldfxwidth==wtrack->fxwidth && window->prevaction.action!=NOACTION){
	  return 0;
	}

#if 1
	GFX_BitBlt(window,wtrack->fxwidth-oldfxwidth,0,
		     wtrack->fxarea.x2-2,wblock->a.y1,
		     wblock->a.x2,wblock->t.y2
		     );
#endif

	UpdateWBlockCoordinates(window,wblock);

#if 1
	DrawUpWTrack(window,wblock,wtrack);
	DrawWTrackHeader(window,wblock,wtrack);
	
	struct WTracks *wtrack2;

	if(oldfxwidth>wtrack->fxwidth){
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
	  GFX_FilledBox(window,0,wtrack2->fxarea.x2+3,wblock->a.y1,wblock->a.x2,wblock->t.y2,PAINT_BUFFER);
	}

	UpdateBottomSlider(window);
	
	if(wtrack->fxarea.x2<wblock->a.x2){
	  Blt_mark(window,wblock->top_realline,wblock->bot_realline,
		   wtrack->fxarea.x2,wblock->a.x2
		   );
	  Blt_marktrackheader(window,wtrack->l.num,wblock->right_track);
	}
#endif
                   //window->must_redraw=true;
	
	return 0;
}


void SetMouseActionWTrackBorder(
	struct Tracker_Windows *window,
	struct MouseAction *action,
	int x
){
	struct WBlocks *wblock=window->wblock;
	NInt wtracknum=wblock->left_track;
	NInt rwtracknum=wblock->right_track;
	struct WTracks *wtrack=ListFindElement1(&wblock->wtracks->l,wtracknum);
	struct WTracks *next=NextWTrack(wtrack);

	if(x<=wtrack->fxarea.x){
		SetMouseActionTempoNodeBorder(window,action,x);
		return;
	}

	for(;;){
		if(wtrack->l.num==rwtracknum) break;
		if(x>=wtrack->x2 && x<=next->fxarea.x) break;
		wtrack=next;
		next=NextWTrack(wtrack);
	}
	
	if(wtrack->x < wblock->t.x1) return; // Changing trackwidth of a partly shown track doesnt work quite well.

	action->eint1=wtrack->l.num;
	action->action=TRACKBORDER;
	action->MouseUpFunction= &MoveWTrackBorder_Mouse;
}
