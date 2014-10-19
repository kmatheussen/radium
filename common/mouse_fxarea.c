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
#include "mouse_vellinenode_proc.h"
#include "mouse_vellinestart_proc.h"
#include "mouse_vellineend_proc.h"
#include "mouse_fxnode_proc.h"
#include "fxlines_proc.h"
#include "gfx_wtracks_proc.h"
#include "gfx_subtrack_proc.h"
#include "trackreallines_proc.h"
#include "velocities_proc.h"
#include "undo_notes_proc.h"
#include "undo_fxs_proc.h"
#include "gfx_statusbar_proc.h"
#include "player_proc.h"
#include "nodeboxes_proc.h"

#include "mouse_fxarea_proc.h"


extern char firstringinstatusbar[32];

void SetMouseActionVelocities(
	struct Tracker_Windows *window,
	struct MouseAction *action,
	struct WTracks *wtrack,
	int x,int y,
	int realline,
	int click
){
	struct WBlocks *wblock=window->wblock;

	WArea warea;
        TBox within;

	within.y1=Common_oldGetReallineY1Pos(window,wblock,realline);
	within.y2=Common_oldGetReallineY2Pos(window,wblock,realline);

        // Find wblock->mouse_note

        {
          struct TrackReallineElements *element=wtrack->trackreallines[realline].trackreallineelements;

          while(element!=NULL){
            warea.x=GetXSubTrack1(wtrack,element->subtype);
            warea.x2=GetXSubTrack2(wtrack,element->subtype);
            warea.width=warea.x2-warea.x;
            
            within.x1=warea.x;
            within.x2=warea.x2;
            
            if(element->type==TRE_VELLINE){
              int y1=scale(element->y1,0,1,within.y1,within.y2);
              int y2=scale(element->y2,0,1,within.y1,within.y2);
              if(y>=y1 && y<y2 && x>=warea.x && x<warea.x2){
                wblock->mouse_note=element->pointer;
                //printf("Set mouse_note to %d\n",wblock->mouse_note->note);
              }
            }
            
            element=element->next;
          }
        }


	struct TrackReallineElements *element=wtrack->trackreallines[realline].trackreallineelements;

	while(element!=NULL){
	  warea.x=GetXSubTrack1(wtrack,element->subtype);
	  warea.x2=GetXSubTrack2(wtrack,element->subtype);
	  warea.width=warea.x2-warea.x;

	  within.x1=warea.x;
	  within.x2=warea.x2;

		if(element->type==TRE_VELLINENODE || element->type==TRE_VELLINESTART || element->type==TRE_VELLINEEND){
			if(
			   isInsideNodeBox(element,&warea,&within,x,y)
			){
				switch(element->type){
					case TRE_VELLINENODE:
						SetMouseActionVelline(
							window,action,wtrack,
							(struct Velocities *)element->pointer,
							realline,
							element->subtype,
							x,y,click
						);
						GFX_SetChangeInt(window,wblock,"Velocity",((struct Velocities *)element->pointer)->velocity);
						GFX_DrawStatusBar(window,wblock);
						return;
					case TRE_VELLINESTART:
						SetMouseActionVellineStart(
							window,action,wtrack,
							(struct Notes *)element->pointer,
							realline,
							element->subtype,
							x,y,click
						);
						GFX_SetChangeInt(window,wblock,"Velocity Start",((struct Notes *)element->pointer)->velocity);
						GFX_DrawStatusBar(window,wblock);
						return;
					case TRE_VELLINEEND:
						SetMouseActionVellineEnd(
							window,action,wtrack,
							(struct Notes *)element->pointer,
							realline,
							element->subtype,
							x,y,click
						);
						GFX_SetChangeInt(window,wblock,"Velocity End",((struct Notes *)element->pointer)->velocity);
						GFX_DrawStatusBar(window,wblock);
						return;
				}
			}
		}
		element=element->next;
	}
	if(firstringinstatusbar[0]!=0){
	   firstringinstatusbar[0]=0;
	  GFX_DrawStatusBar(window,window->wblock);
	}
}


void SetMouseActionFXNodes(
	struct Tracker_Windows *window,
	struct MouseAction *action,
	struct WTracks *wtrack,
	int x,int y,
	int realline,
	int click
){
	struct WBlocks *wblock=window->wblock;
	WFXNodes *wfxnode=wtrack->wfxnodes[realline];
	WArea warea;
        TBox within;

	warea.x=wtrack->fxarea.x;
	warea.x2=wtrack->fxarea.x2;
	warea.width=warea.x2-warea.x;

	within.x1=warea.x;
	within.x2=warea.x2;
	within.y1=Common_oldGetReallineY1Pos(window,wblock,realline);
	within.y2=Common_oldGetReallineY2Pos(window,wblock,realline);

	while(wfxnode!=NULL){
		if(wfxnode->type==TRE_FXNODE){
                  //printf("Found TRE_FX_NODE at realline %d. Within: %d/%d -> %d/%d. isInside? %d\n",realline,within.x1,within.y1,within.x2,within.y2,isInsideNodeBox(wfxnode,&warea,&within,x,y));

			if(
			   isInsideNodeBox(wfxnode,&warea,&within,x,y)
			   /*
			   insideNArea(
				       x,y,
				       wtrack->fxarea.x,Common_oldGetReallineY1Pos(window,wblock,realline),
				       wfxnode->x1,wfxnode->y1,
				       wfxnode->x2,wfxnode->y2
				       )
			   */
			   ){
				SetMouseActionFXNode(
					window,action,wtrack,
					wfxnode,
					click
				);
				return;
			}
		}
		wfxnode=wfxnode->next;
	}
}

void SetMouseActionFXarea(
	struct Tracker_Windows *window,
	struct MouseAction *action,
	struct WTracks *wtrack,
	int x,int y,
	int click
){
	struct WBlocks *wblock=window->wblock;
	WFXNodes *wfxnode;
	struct TrackReallineElements *element;
	WFXNodes *wfxnodemin=NULL;
	struct FX *fx;
	int subtrack;
	int mintype=0;
	float mindist=-1.0f;
	int realline;
	float rel_y;
	float rel_x;
	Place place;

	action->action=NOACTION;

        if(click==2) {
          AddFXNodeLineCurrPos(window, wblock, wtrack);
          return;
        }

	realline=GetReallineAndPlaceFromY(window,wblock,y,&place,NULL,NULL);
	if(realline<0) return;

	SetMouseActionVelocities(window,action,wtrack,x,y,realline,click);
	if(
		action->action==TRE_VELLINESTART ||
		action->action==TRE_VELLINEEND
	)
          return;

	SetMouseActionFXNodes(window,action,wtrack,x,y,realline,click);

	if(action->action!=NOACTION || 0==click)
          return;

	y-=Common_oldGetReallineY1Pos(window,wblock,realline);
	x-=wtrack->fxarea.x;

	rel_y=((float)y)/window->fontheight;
	rel_x=((float)x)/(wtrack->fxarea.x2-wtrack->fxarea.x);

	wfxnode=wtrack->wfxnodes[realline];

	while(wfxnode!=NULL){
		if(wfxnode->type==TRE_FXLINE){
			if(wfxnode->y1<=rel_y && wfxnode->y2>=rel_y){
				if(wfxnode->y2!=wfxnode->y1){
                                  int nodedist_x = wfxnode->x2-wfxnode->x1;
                                  int nodedist_y = wfxnode->y2-wfxnode->y1;
                                  if(nodedist_y>0){ // shouldnt happen
                                    float tempdist=rel_x-((rel_y-wfxnode->y1)*nodedist_x/nodedist_y+wfxnode->x1);
                                    tempdist=R_ABS(tempdist);
                                    if(tempdist<mindist || mindist==-1.0f){
                                      mindist=tempdist;
                                      wfxnodemin=wfxnode;
                                    }
                                  }
				}
			}
		}
		wfxnode=wfxnode->next;
	}

	element=wtrack->trackreallines[realline].trackreallineelements;
	subtrack=GetSubTrack(wtrack,x);

	if(subtrack>=0){
		while(element!=NULL){
			if(element->type==TRE_VELLINE){
				if(element->subtype==subtrack && element->y1<=rel_y && element->y2>=rel_y){
					if(element->y2!=element->y1){
						float tempdist=rel_x-((rel_y-element->y1)*(element->x2-element->x1)/(element->y2-element->y1)+element->x1);
						tempdist=R_ABS(tempdist);
						if(tempdist<mindist || mindist==-1.0f){
							mindist=tempdist;
							mintype=1;
						}
					}
				}
			}
			element=element->next;
		}
	}

	if(mindist==-1.0f) return;

	if(0==mintype){

          if(wtrack->fxwidth<=1)
            return;

		Undo_FXs(window,wblock->block,wtrack->track,window->wblock->curr_realline);

		fx=((struct FXs *)(wfxnodemin->pointer))->fx;

		PlayStop();

		AddFXNodeLine(
			window,
			wblock,
			wtrack,
			fx,
			((fx->max-fx->min)*x/(wtrack->fxwidth-1))+fx->min,
			&place
		);

		GFX_SetChangeInt(window,wblock,fx->name,((fx->max-fx->min)*x/(wtrack->fxwidth-1))+fx->min);
		GFX_DrawStatusBar(window,wblock);

		UpdateFXNodeLines(window,wblock,wtrack);

#if !USE_OPENGL
		ClearTrack(window,wblock,wtrack,wblock->top_realline,wblock->bot_realline);
		UpdateWTrack(window,wblock,wtrack,wblock->top_realline,wblock->bot_realline);
#endif
		y+=Common_oldGetReallineY1Pos(window,wblock,realline);
		x+=wtrack->fxarea.x;

		SetMouseActionFXNodes(window,action,wtrack,x,y,realline,0);

	}else{
          int subtrackwidth = GetSubTrackWidth(wtrack,subtrack);
          if(subtrackwidth<=0)
            return;

		PlayStop();

		Undo_Notes(window,wblock->block,wtrack->track,window->wblock->curr_realline);

                RError("dead code");
                /*
		AddVelocity(
			window,
			wblock,
			wtrack,
			subtrack,
			MAX_VELOCITY*(x-GetRelXSubTrack1(wtrack,subtrack))/subtrackwidth,
			&place,
			realline
		);
                */
		GFX_SetChangeInt(window,wblock,"Velocity",MAX_VELOCITY*(x-GetRelXSubTrack1(wtrack,subtrack))/subtrackwidth);
		GFX_DrawStatusBar(window,wblock);

		UpdateTrackReallines(window,wblock,wtrack);
#if !USE_OPENGL
		ClearTrack(window,wblock,wtrack,wblock->top_realline,wblock->bot_realline);
		UpdateWTrack(window,wblock,wtrack,wblock->top_realline,wblock->bot_realline);
#endif
		y+=Common_oldGetReallineY1Pos(window,wblock,realline);
		x+=wtrack->fxarea.x;

		SetMouseActionVelocities(window,action,wtrack,x,y,realline,0);

	}

}












