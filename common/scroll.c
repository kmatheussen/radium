/* Copyright 2001 Kjetil S. Matheussen

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
#include "pixmap_proc.h"
#include "wblocks_proc.h"
#include "visual_proc.h"
#include "common_proc.h"
#include "gfx_subtrack_proc.h"
#include "../OpenGL/Widget_proc.h"
#include "player_proc.h"
#include "player_pause_proc.h"

#include "scroll_proc.h"



#if 0
static void Scroll_freePixMapLines(
	struct Tracker_Windows *window,
	int startrealline,
	int endrealline
){
  if(WBlock_legalizeStartEndReallines(window->wblock,&startrealline,&endrealline)==false){
    return;
  }

	PixMap_erase(
		window,
		startrealline - window->wblock->top_realline,
		endrealline - window->wblock->top_realline
	);
}
#endif


#if !USE_OPENGL
void Scroll_scrollPixMapDefs(
	struct Tracker_Windows *window,
	int num_toscroll
){
  /*
	struct WBlocks *wblock=window->wblock;

	int startrealline=wblock->top_realline;
	int endrealline=wblock->bot_realline;
  */
	//	WBlock_legalizeStartEndReallines(wblock,&startrealline,&endrealline);

}

void Scroll_makePixMapLines(
	struct Tracker_Windows *window,
	int startrealline,
	int endrealline
){
	struct WBlocks *wblock=window->wblock;

	if(WBlock_legalizeStartEndReallines(wblock,&startrealline,&endrealline)==false){
		return;
	}

	PixMap_makeNewDefs(
		window,
		startrealline-wblock->top_realline,
		endrealline-wblock->top_realline
	);

        {
          struct WTracks *wtrack=ListLast1(&wblock->wtracks->l);
          int x2=wtrack->fxarea.x2;
          EraseLines(
                     window,wblock,
                     wblock->a.x1, x2,
                     startrealline, endrealline+1
                     );
        }

	DrawWBlockSpesific(
		window,
		wblock,
		startrealline,
		endrealline
	);

	UpdateAllWTracks(
		window,
		wblock,
		startrealline,
		endrealline
	);
}
#endif



/* Should only be called from functions in cursor_updown.c */

void Scroll_scroll(
                   struct Tracker_Windows *window,
                   int num_lines
){

  bool dopause = (ATOMIC_GET(root->play_cursor_onoff)==false) && is_playing_current_block();
  
  if (dopause)
    PC_PauseNoMessage();
  
//  int lokke;
	struct WBlocks *wblock=window->wblock;

#if !USE_OPENGL

	int top_realline=wblock->top_realline;
	int bot_realline=wblock->bot_realline;

	/*
	if(num_lines<0){
		Scroll_freePixMapLines(window,bot_realline+num_lines+1,bot_realline);
	}else{
		Scroll_freePixMapLines(window,top_realline,top_realline+num_lines-1);
	}

//	printf("scroll1:\n");
//  for(lokke=0;lokke<window->wblock->num_visiblelines;lokke++)
//    printf("vis: %d, val: %d\n",lokke,window->pixmapdefs[lokke]);

	*/

	//	Scroll_scrollPixMapDefs(window,num_lines);

	PixMap_scrollDefs(
		window,
		num_lines
	);
#endif

//	printf("scroll2:\n");
//  for(lokke=0;lokke<window->wblock->num_visiblelines;lokke++)
//    printf("vis: %d, val: %d\n",lokke,window->pixmapdefs[lokke]);

	wblock->top_realline+=num_lines;
	wblock->curr_realline+=num_lines;
	wblock->bot_realline+=num_lines;

        //printf("scroll: Set realline to %d (%d)\n",wblock->curr_realline,num_lines);
        GE_set_curr_realline(wblock->curr_realline);

#if !USE_OPENGL
	if(num_lines<0){
		Scroll_makePixMapLines(window,top_realline+num_lines,top_realline-1);
	}else{
		Scroll_makePixMapLines(window,bot_realline+1,bot_realline+num_lines);
	}
#endif

        if (dopause)
          PC_StopPauseAtCurrCursorPos(window);

        
//	printf("scroll3: n: %d\n",num_lines);
//  for(lokke=0;lokke<window->wblock->num_visiblelines;lokke++)
//    printf("vis: %d, val: %d\n",lokke,window->pixmapdefs[lokke]);

	/*
	Scroll_drawPixMaps(window,wblock->top_realline,wblock->bot_realline);
	*/
}




