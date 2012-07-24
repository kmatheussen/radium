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
#include "gfx_wblocks_proc.h"
#include "windows_proc.h"
#include "sliders_proc.h"
#include "visual_proc.h"
#include "common_proc.h"
#include "wtracks_proc.h"
#include "gfx_wtracks_proc.h"
#include "gfx_upperleft_proc.h"
#include "wblocks_proc.h"
#include "pixmap_proc.h"
#include "scroll_proc.h"
#include "scroll_play_proc.h"
#include "blts_proc.h"

#include "cursor_updown_proc.h"


void ScrollEditorDown(struct Tracker_Windows *window,int num_lines){
	struct WBlocks *wblock;

	if(num_lines==0) return;

	wblock=window->wblock;

	if(
		wblock->curr_realline<wblock->num_reallines-1 &&
		wblock->curr_realline+num_lines>wblock->num_reallines-1
	){
		num_lines=wblock->num_reallines-1 - wblock->curr_realline;
	}

	if(num_lines==1 || num_lines==-1)
		Scroll_play(wblock,wblock->curr_realline,wblock->curr_realline+num_lines-1);

	if(wblock->curr_realline+num_lines < wblock->num_reallines){
	  Scroll_scroll(window,num_lines);
	}else{
		/* When on the bottom line. */

//		RError("top: %d, num/2: %d\n",wblock->top_realline,wblock->num_visiblelines/2);
		if(wblock->top_realline <= wblock->num_visiblelines/2){
//			RError("jepp\n");
			Scroll_scroll(window,-wblock->curr_realline);

		}else{

			wblock->curr_realline=0;

			SetWBlock_Top_And_Bot_Realline(window,wblock);

			PixMap_reset(window);


			GFX_FilledBox(
				window,
				0,
				wblock->a.x1,wblock->t.y1,
				wblock->t.x2,wblock->t.y2,
                                PAINT_BUFFER
			);

			DrawWBlockSpesific(
				window,
				wblock,
				0,
				wblock->num_visiblelines
			);

			UpdateAllWTracks(
				window,
				wblock,
				0,
				wblock->num_visiblelines
			);
			/*
			GFX_FilledBox(
				      window,
				      0,
				      wblock->a.x1,wblock->t.y1,
				      wblock->t.x2,
				      Common_oldGetReallineY2Pos(window,wblock,wblock->curr_realline-1)
				      );
			*/
		}
	}

	GFX_UpdateCurrLine(window,wblock);

	Blt_scrollMark(window);

	UpdateLeftSlider(window);
}


void ScrollEditorUp(struct Tracker_Windows *window,int num_lines){
	struct WBlocks *wblock;

	if(num_lines==0) return;

	wblock=window->wblock;

	if(wblock->curr_realline>0 && wblock->curr_realline-num_lines<0){
		num_lines=wblock->curr_realline;
	}

        if(num_lines==1 || num_lines==-1)
          Scroll_play(wblock,wblock->curr_realline-num_lines+1,wblock->curr_realline);

	if(wblock->curr_realline-num_lines>=0){

	  Scroll_scroll(window,-num_lines);
	}else{

		if(wblock->bot_realline >= (wblock->num_reallines-(wblock->num_visiblelines/2)-1)){
			Scroll_scroll(window,wblock->num_reallines-1);
		}else{
			wblock->curr_realline=wblock->num_reallines-1;

			SetWBlock_Top_And_Bot_Realline(window,wblock);

			PixMap_reset(window);

			GFX_FilledBox(
				window,
				0,
				wblock->a.x1,wblock->t.y1,
				wblock->t.x2,wblock->t.y2,
                                PAINT_BUFFER
			);
			DrawWBlockSpesific(
				window,
				wblock,
				wblock->top_realline,
				wblock->curr_realline
			);

			UpdateAllWTracks(
				window,
				wblock,
				wblock->top_realline,
				wblock->curr_realline
			);

			/*
			GFX_FilledBox(
				      window,
				      0,
				      wblock->a.x1,Common_oldGetReallineY1Pos(window,wblock,wblock->curr_realline+1),
				      wblock->t.x2,
				      wblock->t.y2
			      );
			*/
		}
	}

	Blt_scrollMark(window);

	GFX_UpdateCurrLine(window,wblock);
	UpdateLeftSlider(window);
}


void ScrollEditorNextNote(struct Tracker_Windows *window){
	struct WBlocks *wblock=window->wblock;
	struct WTracks *wtrack=wblock->wtrack;
	struct TrackRealline *trackreallines=wtrack->trackreallines;
	int curr_realline=wblock->curr_realline;
	int realline=curr_realline+1;

	if(realline>=wblock->num_reallines-1){
		ScrollEditorDown(window,1);
		return;
	}

	while(trackreallines[realline].note==0){
		if(realline==wblock->num_reallines-1) break;
		realline++;
	}

	ScrollEditorDown(window,realline-curr_realline);	
}


void ScrollEditorPrevNote(struct Tracker_Windows *window){
	struct WBlocks *wblock=window->wblock;
	struct WTracks *wtrack=wblock->wtrack;
	struct TrackRealline *trackreallines=wtrack->trackreallines;
	int curr_realline=wblock->curr_realline;
	int realline=curr_realline-1;

	if(realline<=0){
		ScrollEditorUp(window,1);
		return;
	}

	while(trackreallines[realline].note==0){
		if(realline==0) break;
		realline--;
	}

	ScrollEditorUp(window,curr_realline-realline);	
}

void ScrollEditorToRealLine(
	struct Tracker_Windows *window,
	struct WBlocks *wblock,
	int till_curr_realline
){
	int curr_realline=wblock->curr_realline;

	//printf("Going to scroll to line %d. Now: %d \n",till_curr_realline,curr_realline);
/*
	if(till_curr_realline<0){
		till_curr_realline=wblock->num_reallines-1;
	}

	if(till_curr_realline>=wblock->num_reallines){
		till_curr_realline=0;
	}
*/

	if( till_curr_realline < curr_realline ){
		ScrollEditorUp(
			window,
			curr_realline - till_curr_realline
		);
	}else{
		if( till_curr_realline > curr_realline ){
			ScrollEditorDown(
				window,
				till_curr_realline - curr_realline
			);
		}
	}
}

void ScrollEditorToRealLine_CurrPos(
	struct Tracker_Windows *window,
	int till_curr_realline
){
	ScrollEditorToRealLine(window,window->wblock,till_curr_realline);
}

/*
void ScrollEditor(
	struct Tracker_Windows *window,
	int num_reallines
){
	struct WBlocks *wblock=window->wblock;
	int curr_realline=wblock->curr_realline;

	ScrollEditorToRealLine(window,wblock,curr_realline+num_reallines);
}
*/

void ScrollEditorToLine_CurrPos(
	struct Tracker_Windows *window,
	int line
){
	struct WBlocks *wblock=window->wblock;
	struct Blocks *block=wblock->block;

	line=R_MIN(line,block->num_lines-1);
	ScrollEditorToRealLine(window,wblock,wblock->reallines[line]->realline);
}

void ScrollEditorToPercentLine_CurrPos(
	struct Tracker_Windows *window,
	int percent
){
	struct WBlocks *wblock=window->wblock;
	struct Blocks *block=wblock->block;

	int line=block->num_lines*percent/100;

	ScrollEditorToLine_CurrPos(window,line);
}







