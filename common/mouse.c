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
#include "wtracks_proc.h"
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
#include "gfx_wtracks_proc.h"
#include "gfx_wblocks_proc.h"

#include "mouse_proc.h"



int lastpointer=NOACTION;

void SetMouseAction(
    struct Tracker_Windows *window,
    struct MouseAction *action,
    int x, int y, int click
){

  struct WBlocks *wblock=window->wblock;

#if 0
  struct Notes *old_mouse_note = wblock->mouse_note;

  int old_mouse_track = wblock->mouse_track;

  //wblock->mouse_note = NULL;
  wblock->mouse_track = NOTRACK;
#endif


  if(
     y>window->height ||
     x>window->width ||
     x<0 ||
     y<0
  ){
    action->action=NOACTION;
    goto exit;
  }

	if(insideTBox(&window->wblock->reltempo,x,y)){
		SetMouseActionRelTempoSlider(window,action,x,y,click);
		goto exit;
	}

	if(y<wblock->t.y2){
		if(x>wblock->t.x1){
			SetMouseActionWTrack(window,action,x,y,click);
			goto exit;
		}else{
			if(y<wblock->t.y1){
				if(x>wblock->linenumarea.x2){
					SetMouseActionTempoHeader(window,action,x,y,click);
					goto exit;
				}else{
					if(y<window->org_fontheight){
						SetMouseActionQuantitize(window,action,x,y,click);
					}
				}
			}
		}
	}

	if(click && insideWArea(&wblock->tempocolorarea,x)){
          wblock->tempocolorarea.width=0;
          window->must_redraw = true;
	}

#if !USE_OPENGL
	if(insideWArea(&wblock->temponodearea,x)){
		SetMouseActionTempoNodes(window,action,x,y,click);
		goto exit;
	}
#endif
        
	action->action=NOACTION;

 exit:
        return;

#if 0
        if(old_mouse_track!=wblock->mouse_track || old_mouse_note!=wblock->mouse_note){
          //printf("mouse.c: Drawing up tracks\n");
          UpdateAllWTracksCoordinates(window,wblock);
          //DrawUpAllWTracks(window,wblock,NULL);
#if !USE_OPENGL
          DrawUpTrackerWindow(window);
#endif
        }

#if !USE_OPENGL
        if(old_mouse_track!=wblock->mouse_track && (old_mouse_track==TEMPONODETRACK || wblock->mouse_track==TEMPONODETRACK))
          DrawUpWTempoNodes(window,wblock);
#endif

#endif
}

static float getX(uint32_t keyswitch, struct MouseAction *action, int x){
  if (LeftShift(keyswitch))
    return action->org_x;

  else if (LeftCtrl(keyswitch) || RightCtrl(keyswitch)) {
    int dx = x - action->org_x;
    return action->org_x + dx/10.0f;

  } else
    return x;
}

static float getY(uint32_t keyswitch, struct MouseAction *action, int y){
  if (LeftExtra(keyswitch))
    return action->org_y;

  else if (LeftCtrl(keyswitch) || RightCtrl(keyswitch)) {
    int dy = y - action->org_y;
    return action->org_y + dy/10.0f;

  } else
    return y;
}

void MouseMove(struct Tracker_Windows *window, uint32_t keyswitch, int x,int y){
	struct MouseAction *curraction= &window->curraction;
	struct MouseAction *prevaction= &window->prevaction;

        SetNormalPointer(window);

	if(prevaction->action!=NOACTION){
          (*prevaction->MouseUpFunction)(window, getX(keyswitch,prevaction,x), getY(keyswitch,prevaction,y));
          return;
	}

        curraction->org_x = x;
        curraction->org_y = y;

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

void RightMouseDown(struct Tracker_Windows *window, uint32_t keyswitch, int x,int y){
	struct MouseAction *prevaction= &window->prevaction;

        prevaction->org_x = x;
        prevaction->org_y = y;

	SetMouseAction(window,prevaction,x,y,2);
}

void LeftMouseDown(struct Tracker_Windows *window, uint32_t keyswitch, int x,int y){
	struct MouseAction *prevaction= &window->prevaction;

        prevaction->org_x = x;
        prevaction->org_y = y;

	SetMouseAction(window,prevaction,x,y,1);
}

extern char firstringinstatusbar[32];

int LeftMouseUp(struct Tracker_Windows *window, uint32_t keyswitch, int x,int y){
	struct MouseAction *prevaction= &window->prevaction;
	int ret=0;

	if(prevaction->action!=NOACTION){
		prevaction->action=NOACTION;
		ret=(*prevaction->MouseUpFunction)(window, getX(keyswitch,prevaction,x), getY(keyswitch,prevaction,y));
	}

	if(firstringinstatusbar[0]!=0){
	   firstringinstatusbar[0]=0;
	  GFX_DrawStatusBar(window,window->wblock);
	}
	return ret;
}

int RightMouseUp(struct Tracker_Windows *window, uint32_t keyswitch, int x,int y){
  return LeftMouseUp(window, keyswitch, x,y);
}
