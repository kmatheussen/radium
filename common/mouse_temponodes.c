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
#include "common_proc.h"
#include "area_proc.h"
#include "placement_proc.h"
#include "temponodes_proc.h"
#include "gfx_wblocks_proc.h"
#include "list_proc.h"
#include "time_proc.h"
#include "reltempo_proc.h"
#include "undo_temponodes_proc.h"
#include "gfx_statusbar_proc.h"
#include "player_proc.h"
#include "blts_proc.h"
#include "realline_calc_proc.h"
#include "visual_proc.h"
#include "wblocks_proc.h"
#include "gfx_tempocolor_proc.h"
#include "nodeboxes_proc.h"
#include "trackreallines_proc.h"
#include "gfx_wtracks_proc.h"

#include "mouse_temponodes_proc.h"

extern struct Root *root;

int MoveTempoNode_Mouse(
	struct Tracker_Windows *window,
	float x,float y
){
	struct WBlocks *wblock=window->wblock;
	struct MouseAction *action= &window->prevaction;
	struct TempoNodes *temponode=(struct TempoNodes *)action->pointer1;
	struct TempoNodes *prev;
	Place place,*prev_vel=NULL,*next_vel=NULL;
	int realline;
	int lx,dx;

	int start_realline,end_realline;

	if(isInList1_m(
		     &window->wblocks->l,&wblock->l,
		     &wblock->block->temponodes->l,&temponode->l,
		     root
		     )==false){
	  action->action=NOACTION;
	  return 0;
	}

	PlayStop();

	lx=R_MAX(wblock->temponodearea.x+1,x);
	lx=R_MIN(wblock->temponodearea.x2,lx);

	dx=lx - wblock->temponodearea.x -1;

	if(
		temponode==wblock->block->temponodes ||
		temponode==wblock->block->lasttemponode
	){
		temponode->reltempo=Gfx2RelTempo(wblock,dx);
		GFX_SetChangeFloat(window,wblock,"Reltempo",RelTempo2RealRelTempo(temponode->reltempo));
		if(temponode==wblock->block->temponodes){
		  prev_vel=&temponode->l.p;
		  next_vel=&(NextTempoNode(temponode)->l.p);
		}else{
		  prev_vel=&((struct ListHeader3 *)
			     (ListPrevElement3(&wblock->block->temponodes->l,&wblock->block->lasttemponode->l)
			      ))->p;
		  next_vel=&temponode->l.p;
		}
	}else{
		prev=ListPrevElement3(&wblock->block->temponodes->l,&temponode->l);
		prev_vel=&prev->l.p;
		next_vel=&(NextTempoNode(temponode)->l.p);
		realline=GetReallineAndPlaceFromY(
			window,wblock,y,&place,prev_vel,
			next_vel
		);

		if(
			realline< -(window->fontheight*2) ||
			x>wblock->temponodearea.x2+(window->fontheight*2) ||
			x<wblock->temponodearea.x-(window->fontheight*2)
		){
			ListRemoveElement3(&window->wblock->block->temponodes->l,&temponode->l);
			action->action=NOACTION;
		}else{
			PlaceCopy(&temponode->l.p,&place);
			temponode->reltempo=Gfx2RelTempo(wblock,dx);
			GFX_SetChangeFloat(window,wblock,"Reltempo",RelTempo2RealRelTempo(temponode->reltempo));
		}
	}

	UpdateWTempoNodes(window,wblock);

	start_realline=FindRealLineFor(wblock,0,prev_vel);
	end_realline=FindRealLineFor(wblock,start_realline,next_vel);
	
	WBlock_legalizeStartEndReallines(wblock,&start_realline,&end_realline);

        //	printf("start: %d,end: %d\n",start_realline,end_realline);
	
#if !USE_OPENGL
        EraseLines(window, wblock,
                   wblock->temponodearea.x, wblock->temponodearea.x2,
                   start_realline, end_realline+1
                   );
        /*
	int lokke;

	for(lokke=start_realline;lokke<=end_realline;lokke++){
	  GFX_FilledBox(
                        window,0,
                        wblock->temponodearea.x,
                        GetReallineY1Pos(window,wblock,lokke),
                        wblock->temponodearea.x2,
                        GetReallineY2Pos(window,wblock,lokke),
                        PAINT_BUFFER
                        );
	}
        */

	DrawWTempoNodes(window,wblock,start_realline,end_realline);
#endif

	//	DrawUpWTempoNodes(window,wblock);
	UpdateSTimes(wblock->block);

	GFX_DrawStatusBar(window,wblock);

#if !USE_OPENGL
	WBLOCK_DrawTempoColor(window,wblock,start_realline,end_realline);
#endif

        UpdateAllTrackReallines(window,wblock);

#if !USE_OPENGL
        DrawUpAllPeakWTracks(window,wblock,NULL);
#endif

	return 0;
}

extern char firstringinstatusbar[32];


void SetMouseActionTempoNodes(
	struct Tracker_Windows *window,
	struct MouseAction *action,
	int x,int y, int click
){
	WTempoNodes *wtemponode;
	struct WBlocks *wblock=window->wblock;
	Place place;
	int realline;
	WArea *temponodearea=&wblock->temponodearea;
	int dx;

        TBox within;

        wblock->mouse_track=TEMPONODETRACK;

	realline=GetReallineAndPlaceFromY(window,wblock,y,&place,NULL,NULL);

	within.x1=temponodearea->x;
	within.x2=temponodearea->x2;
	//	within.y1=0;
	//within.y2=window->fontheight;
	within.y1=Common_oldGetReallineY1Pos(window,wblock,realline);
	within.y2=Common_oldGetReallineY2Pos(window,wblock,realline);

	x=R_MAX(temponodearea->x+1,x);
	x=R_MIN(temponodearea->x2-1,x);

	dx=x - temponodearea->x -1;

	if(realline<0) return;

	wtemponode=wblock->wtemponodes[realline];

	while(wtemponode!=NULL){

		if(wtemponode->type==TEMPONODE_NODE){
			if(
			   /*
				insideNArea(
					x,y,
					wblock->temponodearea.x,Common_oldGetReallineY1Pos(window,wblock,realline),
					wtemponode->x1,wtemponode->y1,
					wtemponode->x2,wtemponode->y2
				)
			   */
			   isInsideNodeBox(
					   wtemponode,
					   temponodearea,
					   &within,
					   x,y
					   //-Common_oldGetReallineY1Pos(window,wblock,realline)
					   )
			   //			   y>Common_oldGetReallineY1Pos(window,wblock,realline)+wtemponode->y1
			   // && 
			   // y<Common_oldGetReallineY1Pos(window,wblock,realline)+wtemponode->y1+5
			){

				if(1==click) Undo_TempoNodes_CurrPos(window);
				action->action=TEMPONODE;
				action->MouseUpFunction= &MoveTempoNode_Mouse;
				action->pointer1=wtemponode->pointer;
				GFX_SetChangeFloat(window,wblock,"Reltempo",RelTempo2RealRelTempo(((struct TempoNodes *)wtemponode->pointer)->reltempo));
				GFX_DrawStatusBar(window,wblock);
				return;
			}
		}
		wtemponode=wtemponode->next;
	}

	if(firstringinstatusbar[0]!=0){
	   firstringinstatusbar[0]=0;
	  GFX_DrawStatusBar(window,window->wblock);
	}

	if(1==click){
		PlayStop();
		Undo_TempoNodes_CurrPos(window);
		AddTempoNode(window,wblock,&place,Gfx2RelTempo(wblock,dx));
		GFX_SetChangeFloat(window,wblock,"Reltempo",RelTempo2RealRelTempo(Gfx2RelTempo(wblock,dx)));
		UpdateWTempoNodes(window,wblock);
#if !USE_OPENGL
		DrawUpWTempoNodes(window,wblock);
#endif
		UpdateSTimes(wblock->block);
		SetMouseActionTempoNodes(window,action,x,y,0);
		GFX_DrawStatusBar(window,wblock);
#if !USE_OPENGL
		WBLOCK_DrawTempoColor(window,wblock,0,wblock->num_reallines);
#endif
	}
}




















