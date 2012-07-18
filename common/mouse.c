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
#include "windows_proc.h"
#include "tbox_proc.h"
#include "area_proc.h"
#include "mouse_wtrack_proc.h"
#include "mouse_temponodes_proc.h"
#include "mouse_tempoheader_proc.h"
#include "gfx_tempotrackheader_proc.h"
#include "mouse_quantitize_proc.h"
#include "gfx_statusbar_proc.h"
#include "mouse_reltemposlider_proc.h"
#include "gfx_upperleft_proc.h"
#include "gfx_tempotrackheader_proc.h"

#include "mouse_proc.h"



int lastpointer=NOACTION;

void SetMouseAction(
    struct Tracker_Windows *window,
    struct MouseAction *action,
    int x,int y, int click
){
  struct WBlocks *wblock=window->wblock;
  
  if(
     y>window->height ||
     x>window->width ||
     x<0 ||
     y<0
  ){
    action->action=NOACTION;
    return;
  }

	if(insideTBox(&window->wblock->reltempo,x,y)){
		SetMouseActionRelTempoSlider(window,action,x,y,click);
		return;
	}

	if(y<wblock->t.y2){
		if(x>wblock->t.x1){
			SetMouseActionWTrack(window,action,x,y,click);
			return;
		}else{
			if(y<wblock->t.y1){
				if(x>wblock->linenumarea.x2){
					SetMouseActionTempoHeader(window,action,x,y,click);
					return;
				}else{
					if(y<window->org_fontheight){
						SetMouseActionQuantitize(window,action,x,y,click);
					}
				}
			}
		}
	}

	if(click && insideWArea(&wblock->tempocolorarea,x)){
	  GFX_FilledBox(
			window,0,
			wblock->lpbTypearea.x+1,
			wblock->a.y1,
			wblock->tempoTypearea.x2+3,
			wblock->t.y1-2
			);

		wblock->tempocolorarea.width=0;
		DrawUpTrackerWindow(window);
	}

	if(insideWArea(&wblock->temponodearea,x)){
		SetMouseActionTempoNodes(window,action,x,y,click);
		return;
	}

	action->action=NOACTION;
}

void MouseMove(struct Tracker_Windows *window,int x,int y){
	struct MouseAction *curraction= &window->curraction;
	struct MouseAction *prevaction= &window->prevaction;

	if(prevaction->action!=NOACTION){
		(*prevaction->MouseUpFunction)(window,x,y);
		return;
	}
	SetMouseAction(window,curraction,x,y,0);

	if(prevaction->action!=NOACTION) return;

	switch(curraction->action){
		case NOACTION:
			if(lastpointer!=NOACTION){
				lastpointer=NOACTION;
				SetNormalPointer(window);
			}
			break;
		default:
			break;
	}
}

void LeftMouseDown(struct Tracker_Windows *window,int x,int y){
	struct MouseAction *prevaction= &window->prevaction;

	SetMouseAction(window,prevaction,x,y,1);
}

extern char firstringinstatusbar[32];

int LeftMouseUp(struct Tracker_Windows *window,int x,int y){
	struct MouseAction *prevaction= &window->prevaction;
	int ret=0;

	if(prevaction->action!=NOACTION){
		prevaction->action=NOACTION;
		ret=(*prevaction->MouseUpFunction)(window,x,y);
	}

	if(firstringinstatusbar[0]!=0){
	   firstringinstatusbar[0]=0;
	  GFX_DrawStatusBar(window,window->wblock);
	}
	return ret;

}

