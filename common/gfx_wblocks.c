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


#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "nsmtracker.h"
#include "list_proc.h"
#include "visual_proc.h"
#include "common_proc.h"
#include "wblocks_proc.h"
#include "gfx_wtracks_proc.h"
#include "gfx_wtext_proc.h"
#include "cursor_proc.h"
#include "sliders_proc.h"
#include "gfx_wtrackheaders_proc.h"
#include "gfx_tempotrackheader_proc.h"
#include "gfx_upperleft_proc.h"
#include "gfx_window_title_proc.h"
#include "gfx_wblocks_reltempo_proc.h"
#include "gfx_tempocolor_proc.h"
#include "blts_proc.h"
#include "nodeboxes_proc.h"
#include "nodelines_proc.h"

#include "gfx_wblocks_proc.h"



void DrawUpLineNums(
	struct Tracker_Windows *window,
	struct WBlocks *wblock,
	int start_realline,
	int end_realline
){
	int lokke;
	int color;
	struct LocalZooms *realline;

	if(WBlock_legalizeStartEndReallines(wblock,&start_realline,&end_realline)==false){
	  return;
	}

	for(lokke=start_realline;lokke<=end_realline;lokke++){
		realline= wblock->reallines[lokke];

		if(realline->level>0){
			if(realline->zoomline>0){
				color=R_MIN(7,realline->level+1);
			}else{
				if(realline->level==1){
					color=1;
				}else{
					color=R_MIN(7,realline->level);
				}
			}
		}else{
			color=1;
		}

		if(realline->level>0){
			SetTextNumLine(
				window,
				wblock,
				color,
				realline->level,
				wblock->zoomlevelarea.width/window->fontwidth,
				wblock->zoomlevelarea.x,
				lokke,
				false
			);
		}
		SetTextNumLine(
			window,
			wblock,
			color,
			realline->zoomline,
//			realline->Tline,
			wblock->linenumarea.width/window->fontwidth,
			wblock->linenumarea.x,
			lokke,
			false
		);
	}

	Blt_markSTrack(window,LINENUMBTRACK,start_realline,end_realline);
}

void DrawTempos(
	struct Tracker_Windows *window,
	struct WBlocks *wblock,
	int start_realline,
	int end_realline
){
	int lokke,tempo,type;
	char *typetext=NULL;

	if(WBlock_legalizeStartEndReallines(wblock,&start_realline,&end_realline)==false){
	  return;
	}

	for(lokke=start_realline;lokke<=end_realline;lokke++){
		tempo=wblock->wtempos[lokke].tempo;
		type=wblock->wtempos[lokke].type;
		if(tempo!=0){
			SetTextNumLine(
				window,
				wblock,
				1,
				tempo,
				wblock->tempoarea.width/window->fontwidth,
				wblock->tempoarea.x,
				lokke,
				false
			);
		}
		if(type!=TEMPO_NORMAL){
			switch(type){
				case TEMPO_BELOW:
					typetext="d";
					break;
				case TEMPO_MUL:
					typetext="m";
					break;
			};
			SetTextLine(
				window,
				wblock,
				1,
				typetext,
				wblock->tempoTypearea.x,
				lokke,
				false
			);
		}
	}
	Blt_markSTrack(window,TEMPOTRACK,start_realline,end_realline);
}


void DrawLPBs(
	struct Tracker_Windows *window,
	struct WBlocks *wblock,
	int start_realline,
	int end_realline
){
	int lokke,lpb,type;
	char *typetext=NULL;

	if(WBlock_legalizeStartEndReallines(wblock,&start_realline,&end_realline)==false){
	  return;
	}

	for(lokke=start_realline;lokke<=end_realline;lokke++){
		lpb=wblock->wlpbs[lokke].lpb;
		type=wblock->wlpbs[lokke].type;
		if(lpb!=0){
			SetTextNumLine(
				window,
				wblock,
				1,
				lpb,
				wblock->lpbarea.width/window->fontwidth,
				wblock->lpbarea.x,
				lokke,
				false
			);
		}
		if(type!=LPB_NORMAL){
			switch(type){
				case LPB_BELOW:
					typetext="d";
					break;
				case LPB_MUL:
					typetext="m";
					break;
			};
			SetTextLine(
				window,
				wblock,
				1,
				typetext,
				wblock->lpbTypearea.x,
				lokke,
				false
			);
		}
	}
	Blt_markSTrack(window,LPBTRACK,start_realline,end_realline);
}

void DrawWTempoNodes(
	struct Tracker_Windows *window,
	struct WBlocks *wblock,
	int start_realline,
	int end_realline
){
	int lokke;
	WTempoNodes *wtemponode;
	WArea *temponodearea=&wblock->temponodearea;
	TBox get,within;

	if(WBlock_legalizeStartEndReallines(wblock,&start_realline,&end_realline)==false){
	  return;
	}

	within.x1=temponodearea->x;
	within.x2=temponodearea->x2;

	for(lokke=start_realline;lokke<=end_realline;lokke++){
	  within.y1=GetReallineY1Pos(window,wblock,lokke);
	  within.y2=GetReallineY2Pos(window,wblock,lokke);

		wtemponode=wblock->wtemponodes[lokke];
/*
	GFX_FilledBox(
		window,0,
		wblock->temponodearea.x,
		GetReallineY1Pos(window,wblock,lokke),
		wblock->temponodearea.x2,
		GetReallineY2Pos(window,wblock,lokke)
	);
*/
		while(wtemponode!=NULL){
			switch(wtemponode->type){
				case TEMPONODE_LINE:
				  GetNodeLine(wtemponode,temponodearea,&within,&get);
				  GFX_P_Line(
					     window,4,
					     get.x1,
					     get.y1,
					     get.x2,
					     get.y2
					     );
				  /*
					GFX_P_Line(
						window,4,
						temponodearea->x+(temponodearea->width*wtemponode->x1),
						GetReallineY1Pos(window,wblock,lokke)+wtemponode->y1,
						temponodearea->x+(temponodearea->width*wtemponode->x2),
						GetReallineY1Pos(window,wblock,lokke)+wtemponode->y2
					);
				  */
					break;
				case TEMPONODE_NODE:
				  GetNodeBox(window,wtemponode,temponodearea,&within,&get);
				  GFX_P_FilledBox(
						  window,6,
						  get.x1+1,
						  get.y1+1,
						  get.x2-1,
						  get.y2-1
						  );
				  GFX_P_Box(
					    window,1,
					    get.x1,get.y1,get.x2,get.y2
					    );
				  /*
					GFX_P_FilledBox(
						window,6,
						temponodearea->x+(temponodearea->width*wtemponode->x1)+1,
						GetReallineY1Pos(window,wblock,lokke)+wtemponode->y1+1,
						temponodearea->x+(temponodearea->width*wtemponode->x2)-1,
						GetReallineY1Pos(window,wblock,lokke)+wtemponode->y2-1
					);
					GFX_P_Box(
						window,1,
						temponodearea->x+(temponodearea->width*wtemponode->x1),
						GetReallineY1Pos(window,wblock,lokke)+wtemponode->y1,
						temponodearea->x+(temponodearea->width*wtemponode->x2),
						GetReallineY1Pos(window,wblock,lokke)+wtemponode->y2
					);
				  */
			}
			wtemponode=wtemponode->next;
		}
	}
	Blt_markSTrack(window,TEMPONODETRACK,start_realline,end_realline);
}

void WBLOCK_DrawBorders(
	struct Tracker_Windows *window,
	struct WBlocks *wblock,
	int start_realline,
	int end_realline
){

  int y1,y2;

  if(WBlock_legalizeStartEndReallines(wblock,&start_realline,&end_realline)==false){
    return;
  }

  y1=GetReallineY1Pos(window,wblock,start_realline);
  y2=GetReallineY2Pos(window,wblock,end_realline);

	GFX_P_DrawTrackBorderDouble(
		window,
		wblock->zoomlevelarea.x2+1,
		y1,y2
	);

	GFX_P_DrawTrackBorderDouble(
		window,
		wblock->linenumarea.x2+1,
		y1,y2
	);

	GFX_P_DrawTrackBorderDouble(
		window,
		wblock->lpbarea.x2+1,
		y1,y2
	);

	GFX_P_DrawTrackBorderSingle(
		window,
		wblock->tempoarea.x2+1,
		y1,y2
	);

	GFX_P_DrawTrackBorderDouble(
		window,
		wblock->temponodearea.x2+1,
		y1,y2
	);

}


void DrawUpWTempoNodes(
	struct Tracker_Windows *window,
	struct WBlocks *wblock
){

	GFX_P_FilledBox(
		window,0,
		wblock->temponodearea.x,
		wblock->t.y1,
		wblock->temponodearea.x2,
		wblock->t.y2
	);

	DrawWTempoNodes(window,wblock,wblock->top_realline,wblock->bot_realline);
}

void DrawUpTempos(
	struct Tracker_Windows *window,
	struct WBlocks *wblock
){

	GFX_P_FilledBox(
		window,0,
		wblock->tempoarea.x,
		wblock->t.y1,
		wblock->tempoarea.x2,
		wblock->t.y2
	);

	DrawTempos(window,wblock,wblock->top_realline,wblock->bot_realline);
}

void DrawUpLPBs(
	struct Tracker_Windows *window,
	struct WBlocks *wblock
){

	GFX_P_FilledBox(
		window,0,
		wblock->lpbarea.x,
		wblock->t.y1,
		wblock->lpbarea.x2,
		wblock->t.y2
	);

	DrawLPBs(window,wblock,wblock->top_realline,wblock->bot_realline);
}


void DrawWBlockSpesific(
	struct Tracker_Windows *window,
	struct WBlocks *wblock,
	int start_realline,
	int end_realline
){
	DrawUpLineNums(window,wblock,start_realline,end_realline);
	DrawTempos(window,wblock,start_realline,end_realline);
	DrawLPBs(window,wblock,start_realline,end_realline);
	DrawWTempoNodes(window,wblock,start_realline,end_realline);
	WBLOCK_DrawBorders(window,wblock,start_realline,end_realline);
	WBLOCK_DrawTempoColor(window,wblock,start_realline,end_realline);
}



void DrawWBlock(struct Tracker_Windows *window,struct WBlocks *wblock){

	GFX_DrawWindowTitle(window,wblock);

	DrawTempoTrackHeader(window,wblock);

	GFX_DrawUpLeft(window,wblock);

	GFX_V_DrawTrackBorderDouble(
		window,
		wblock->zoomlevelarea.x2+1,
		wblock->t.y1,
		wblock->a.y2
	);

/*
	GFX_Line(
		window,1,
		0,
		wblock->t.y1-1,
		wblock->linenumarea.x2,
		wblock->t.y1-1
	);
*/

	GFX_V_DrawTrackBorderDouble(
		window,
		wblock->linenumarea.x2+1,
		wblock->a.y1,
		wblock->a.y2
	);

	GFX_V_DrawTrackBorderDouble(
		window,
		wblock->lpbarea.x2+1,
		wblock->a.y1,
		wblock->a.y2
	);

	GFX_V_DrawTrackBorderSingle(
		window,
		wblock->tempoarea.x2+1,
		wblock->a.y1,
		wblock->a.y2
	);

	GFX_V_DrawTrackBorderDouble(
		window,
		wblock->temponodearea.x2+1,
		wblock->a.y1,
		wblock->a.y2
	);

	GFX_Line(
		window,1,
		wblock->linenumarea.x2+3,
		wblock->t.y1-1,
		wblock->temponodearea.x2,
		wblock->t.y1-1
	);


	DrawWBlockSpesific(
		window,
		wblock,
		wblock->top_realline,
		wblock->bot_realline
	);

	
	UpdateAllWTracks(window,wblock,wblock->top_realline,wblock->bot_realline);

	DrawAllWTracksBorders(window,wblock);

	SetCursorPos(window);

	DrawBottomSlider(window);

	GFX_Line(
		window,1,
		wblock->a.x1,
		wblock->t.y2+1,
		window->bottomslider.x-1,
		wblock->t.y2+1
	);

	DrawAllWTrackHeaders(window,wblock);

	DrawBlockRelTempo(window,wblock);
}












