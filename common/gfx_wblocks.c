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
#include "gfx_wtext_proc.h"
#include "cursor_proc.h"
#include "sliders_proc.h"
#include "gfx_wtrackheaders_proc.h"
#include "blts_proc.h"
#include "settings_proc.h"

#include "gfx_wblocks_proc.h"

int first_beat_opacity = -1;
int beat_opacity = -1;
int line_opacity = -1;

#if !USE_OPENGL

void EraseLine(
	struct Tracker_Windows *window,
	struct WBlocks *wblock,
        int x1, int x2,
	int realline
){

  if(lpb_opacity == -1)
    lpb_opacity = SETTINGS_read_int("lpb_opacity", 900);

  if( (wblock->wlpbs[realline].is_beat))
    GFX_SetMixColor(window, HIGH_EDITOR_BACKGROUND_COLOR_NUM, TEXT_COLOR_NUM, lpb_opacity);

  GFX_FilledBox(
                window,
                HIGH_EDITOR_BACKGROUND_COLOR_NUM,
                x1, GetReallineY1Pos(window, wblock, realline),
                x2, GetReallineY2Pos(window, wblock, realline),
                PAINT_BUFFER
                );

  if(line_opacity == -1)
    line_opacity = SETTINGS_read_int("line_opacity", 800);

  if(line_opacity != 1000) {
    GFX_SetMixColor(window, HIGH_EDITOR_BACKGROUND_COLOR_NUM, TEXT_COLOR_NUM, line_opacity);

    GFX_Line(
             window,
             WAVEFORM_COLOR_NUM,
             x1, GetReallineY1Pos(window, wblock, realline),
             x2, GetReallineY1Pos(window, wblock, realline),
             PAINT_BUFFER);
  }
}

void EraseLines(
	struct Tracker_Windows *window,
	struct WBlocks *wblock,
        int x1, int x2,
	int start_realline,
        int end_realline
){
  int realline;
  for(realline = start_realline ; realline<end_realline ; realline++)
    EraseLine(window, wblock, x1, x2, realline);
}

void EraseAllLines(
                   struct Tracker_Windows *window,
                   struct WBlocks *wblock,
                   int x1, int x2
                   )
{
  EraseLines(window, wblock, x1, x2, wblock->top_realline, wblock->bot_realline+1);
}

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

		if(realline->level>0 && realline->zoomline>0){
			SetTextNumLine(
				window,
				wblock,
				color,
				realline->zoomline,
				wblock->zoomlinearea.width/window->fontwidth,
				wblock->zoomlinearea.x,
				lokke,
				false
			);
		}else{
                  SetTextNumLine(
                                 window,
                                 wblock,
                                 color,
                                 //realline->zoomline,
                                 realline->Tline,
                                 (wblock->linenumarea.width)/window->fontwidth,
                                 wblock->linenumarea.x,
                                 lokke,
                                 false
                                 );
                }
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

static void draw_skewed_box(struct Tracker_Windows *window,
                            int color,
                            int x1,int y1,int x2, int y2,
                            int where)
{
  // vertical left
  GFX_SetMixColor(window, color, 2, 100);
  GFX_Line(window,color,
                 x1+1,y1+1,
                 x1+2,y2-1,
                 where);

  // horizontal bottom
  GFX_SetMixColor(window, color, 1, 300);
  GFX_Line(window,color,
                 x1+2,y2-1,
                 x2-1,y2-2,
                 where);

  // vertical right
  GFX_SetMixColor(window, color,1, 400);
  //GFX_SetMixColor(window, 8,9, 700);
  GFX_Line(window,color,
                 x2-1,y2-2,
                 x2-2,y1+2,
                 where);

  // horizontal top
  GFX_SetMixColor(window, color, 2, 300);
  GFX_Line(window,color,
                 x2-2,y1+2,
                 x1+1,y1+1,
                 where);
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

	within.x1=temponodearea->x+1;
	within.x2=temponodearea->x2-1;

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
				  GFX_Line(
                                           window,4,
                                           get.x1,
                                           get.y1,
                                           get.x2,
                                           get.y2,
                                           PAINT_BUFFER
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
				  GetNodeBox(window,wblock,NULL,wtemponode,temponodearea,&within,&get);
                                  if(wblock->mouse_track==TEMPONODETRACK){
                                    GFX_FilledBox(
                                                  window,6,
                                                  get.x1+1,
                                                  get.y1+1,
                                                  get.x2-2,
                                                  get.y2-2,
                                                  PAINT_BUFFER
                                                  );
                                    draw_skewed_box(window,1,get.x1,get.y1,get.x2,get.y2,PAINT_BUFFER);
                                  }
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

	GFX_DrawTrackBorderDouble(
                                  window,
                                  wblock->linenumarea.x2+1,
                                  y1,y2,
                                  PAINT_BUFFER
                                  );

	GFX_DrawTrackBorderDouble(
                                  window,
                                  wblock->zoomlinearea.x2+1,
                                  y1,y2,
                                  PAINT_BUFFER
                                  );

	GFX_DrawTrackBorderDouble(
                                  window,
                                  wblock->lpbarea.x2+1,
                                  y1,y2,
                                  PAINT_BUFFER
                                  );

	GFX_DrawTrackBorderSingle(
                                  window,
                                  wblock->tempoarea.x2+1,
                                  y1,y2,
                                  PAINT_BUFFER
                                  );

	GFX_DrawTrackBorderDouble(
                                  window,
                                  wblock->temponodearea.x2+1,
                                  y1,y2,
                                  PAINT_BUFFER
                                  );

}


void DrawUpWTempoNodes(
	struct Tracker_Windows *window,
	struct WBlocks *wblock
){

  EraseAllLines(window, wblock,
		wblock->temponodearea.x,
		wblock->temponodearea.x2);
  /*
  GFX_FilledBox(
		window,0,
		wblock->temponodearea.x,
		wblock->t.y1,
		wblock->temponodearea.x2,
		wblock->t.y2,
                PAINT_BUFFER
                );
  */

  DrawWTempoNodes(window,wblock,wblock->top_realline,wblock->bot_realline);
}

void DrawUpTempos(
	struct Tracker_Windows *window,
	struct WBlocks *wblock
){

  EraseAllLines(window, wblock,
		wblock->tempoarea.x,
		wblock->tempoarea.x2);
  /*
  GFX_FilledBox(
		window,0,
		wblock->tempoarea.x,
		wblock->t.y1,
		wblock->tempoarea.x2,
		wblock->t.y2,
                PAINT_BUFFER
                );
  */
  DrawTempos(window,wblock,wblock->top_realline,wblock->bot_realline);
}

void DrawUpLPBs(
	struct Tracker_Windows *window,
	struct WBlocks *wblock
){

  EraseAllLines(window, wblock,
		wblock->lpbarea.x,
		wblock->lpbarea.x2);
  /*
  GFX_FilledBox(
		window,0,
		wblock->lpbarea.x,
		wblock->t.y1,
		wblock->lpbarea.x2,
		wblock->t.y2,
                PAINT_BUFFER
                );
  */

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

#endif // USE_OPENGL



void DrawWBlock(struct Tracker_Windows *window,struct WBlocks *wblock){

  if(window->must_redraw==true)
    return;

#if 0
	GFX_DrawStatusBar(window,wblock);

	DrawTempoTrackHeader(window,wblock);

	GFX_DrawUpLeft(window,wblock);

/*
	GFX_Line(
		window,1,
		0,
		wblock->t.y1-1,
		wblock->linenumarea.x2,
		wblock->t.y1-1
	);
*/

	GFX_Line(
		window,1,
		wblock->linenumarea.x2+3,
		wblock->t.y1-1,
		wblock->temponodearea.x2,
		wblock->t.y1-1,
                PAINT_DIRECTLY
	);

#endif

#if !USE_OPENGL
	DrawWBlockSpesific(
		window,
		wblock,
		wblock->top_realline,
		wblock->bot_realline
	);
	
	UpdateAllWTracks(window,wblock,wblock->top_realline,wblock->bot_realline);

	R_SetCursorPos(window);
#endif

	DrawAllWTrackHeaders(window,wblock);
}




