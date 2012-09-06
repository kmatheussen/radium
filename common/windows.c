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
#include "wblocks_proc.h"
#include "wtracks_proc.h"
#include "reallines_proc.h"
#include "gfx_wblocks_proc.h"
#include "common_proc.h"
#include "visual_proc.h"
#include "sliders_proc.h"
#include "trackreallines_proc.h"
#include "gfx_wtracks_proc.h"
#include "pixmap_proc.h"
#include "blts_proc.h"
#include "gfx_op_queue_proc.h"
#include "settings_proc.h"
#include "cursor_proc.h"
#include "gfx_subtrack_proc.h"

#include "windows_proc.h"




extern struct Root *root;

int CloseTrackerWindow(NInt place){
	struct Tracker_Windows *temp;

	temp=(struct Tracker_Windows *)ListFindElement1(&root->song->tracker_windows->l,place);

	if(temp==NULL) return 1;

	GFX_ShutDownVisual(temp);

//	temp->wblock=NULL;

	ListRemoveElement1(&root->song->tracker_windows,&temp->l);

	return 0;
}

void CloseAllTrackerWindows(void){
	if(root==NULL) return;
	if(root->song==NULL) return;
	while(root->song->tracker_windows!=NULL)
		CloseTrackerWindow(root->song->tracker_windows->l.num);
}


/*********************************************************************
  FUNCTION
    Call after window has been made, or fontsize is changed.
*********************************************************************/
void UpdateTrackerWindowCoordinates(struct Tracker_Windows *window){
  window->bottomslider.x2=window->width-1;
}


/*********************************************************************
  FUNCTION
    Is ment to be called instead of clearing all of the window, to avoid flicker.
*********************************************************************/
void ClearUnusedWindowsAreas(struct Tracker_Windows *window){
  const int color = 0;
  struct WBlocks *wblock = window->wblock;

  // Clears the area between the header and the first line, if there is space.
  {
    int top_line_y = Common_oldGetReallineY1Pos(window, wblock, 0);
    //printf("top_line_y: %d. wblock->t.y1: %d\n", top_line_y, wblock->t.y1);

    if(top_line_y>wblock->t.y1)
      GFX_FilledBox(window,color,
                    window->leftslider.width+1, wblock->t.y1,
                    window->width, top_line_y-1,
                    PAINT_DIRECTLY
                    );
  }

  // Clears the area between the last line and the bottom slider, if there is space.
  {
    int bot_line_y2 = Common_oldGetReallineY2Pos(window, wblock, R_MIN(wblock->num_reallines-1,wblock->bot_realline));
    if(bot_line_y2 < wblock->a.y2)
      GFX_FilledBox(window,color,
                    window->leftslider.width+1, bot_line_y2,
                    window->width, wblock->a.y2,
                    PAINT_DIRECTLY
                    );
  }

  // Clear the small area between the temposlider and the bottom slider.
  GFX_FilledBox(
                window,color,
                wblock->reltempo.x2+1,
                window->height - window->bottomslider.width+1,
                window->bottomslider.x-1,
                window->height-1,
                PAINT_DIRECTLY
                );

#if 0
  // Clear the area to the right of the rightmost track.
  {
    struct WTracks *last_wtrack = ListLast1(&wblock->wtracks->l);

    if(last_wtrack->fxarea.x2<wblock->a.x2){
      //GFX_P_FilledBox(window,color,last_wtrack->fxarea.x2+2,wblock->t.y1,wblock->a.x2,wblock->t.y2);
      GFX_FilledBox(window,color,
                    last_wtrack->fxarea.x2+2,  0,
                    wblock->a.x2,              wblock->t.y1,
                    PAINT_DIRECTLY
                    );
    }
  }
#endif

  // Clear the area between the Left slider and the line numbers.
  //printf("sl.x2: %d / %d\n",                window->leftslider.width+1,wblock->zoomlevelarea.x-1);
  //if(window->leftslider.width+1 < wblock->zoomlevelarea.x-1)
  GFX_FilledBox(
                window,color,
                window->leftslider.width+1, wblock->t.y1,
                wblock->zoomlevelarea.x, wblock->t.y2+1,
                PAINT_DIRECTLY
                );

  //printf("%d/%d -> %d/%d\n",window->leftslider.width+1, wblock->t.y1,
  //       wblock->zoomlevelarea.x-1, wblock->t.y2);

  // Clear the one pixel wide area in the left slider.
  GFX_Box(
          window,color,
          1, wblock->t.y1+1,
          window->leftslider.width-1, wblock->t.y2,
          PAINT_DIRECTLY
          );  
}


/**************************************************************************
   The general update function.	The spesification for flags is placed
   in windows_proc.h								   
 *************************************************************************/
/*
void Update(
	    struct Tracker_Windows *window,
	    int starttrack,
	    int endtrack,
	    int startrealline,
	    int endrealline,
	    uint_32 flags
	    ){
  if(flags & WU_WINDOW_COORDINATES){
    UpdateTrackerWindowCoordinates(window);
  }
  if(flags & WU_WBLOCK_COORDINATES){
    UpdateWBlockCoordinates(window,window->wblock);
  }
  if(flags & WU_
}
*/

/**************************************************************************
  FUNCTION
    Draw up the window. Can be called if the window is cleared with color 0.
***************************************************************************/

void UpdateTrackerWindow(struct Tracker_Windows *window){
	root->clearall=1;
	UpdateTrackerWindowCoordinates(window);
	UpdateWBlockCoordinates(window,window->wblock);

	PixMap_reset(window);

	DrawWBlock(window,window->wblock);
	DrawLeftSlider(window);
	window->wblock->isgfxdatahere=true;
	root->clearall=0;
}


/**************************************************************************
  FUNCTION
    Redraw without flickering.
***************************************************************************/
void DrawUpTrackerWindow(struct Tracker_Windows *window){
	GFX_FilledBox(window,0,0,0,window->width-1,window->wblock->t.y1,PAINT_BUFFER);
	GFX_FilledBox(window,0,0,window->wblock->t.y1,window->width-1,window->height-1,PAINT_BUFFER);

        struct WBlocks *wblock = window->wblock;

        while(GetXSubTrack_B2(wblock,window->curr_track,window->curr_track_sub) >= wblock->a.x2){

          if(window->curr_track<wblock->left_track)
            break; // should not be possible

          if(window->curr_track==wblock->left_track && window->curr_track_sub==wblock->left_subtrack)
            break;

          CursorLeft_CurrPos(window);
        }

	root->clearall=1;
	UpdateTrackerWindowCoordinates(window);
	UpdateWBlockCoordinates(window,window->wblock);

	PixMap_reset(window);

	DrawWBlock(window,window->wblock);
	DrawLeftSlider(window);

	window->wblock->isgfxdatahere=true;
	root->clearall=0;

        ClearUnusedWindowsAreas(window);

        Blt_unMarkVisible(window); // Need a better name for this function.
}


/**************************************************************************
  FUNCTION
    When making a new window.
***************************************************************************/

int OpenTrackerWindow(int x, int y, int width,int height){
	struct Tracker_Windows *twindow;

	twindow=talloc(sizeof(struct Tracker_Windows));

	twindow->l.num=ListFindFirstFreePlace1(&root->song->tracker_windows->l);

	twindow->x=x;
	twindow->y=y;
	twindow->width=width;
	twindow->height=height;

	twindow->textborder=SETTINGS_read_bool("show_text_border",true);

#ifdef USE_GFX_OP_QUEUE
        GFX_create_op_queue(twindow);
#endif

	if(GFX_CreateVisual(twindow)!=0)
		return -1;

	twindow->l.num=ListFindFirstFreePlace1(&root->song->tracker_windows->l);

	twindow->playalong=true;

        InitSliderValues(twindow);

	twindow->curr_track_sub= -1;

	twindow->minnodesize=SETTINGS_read_int("minimum_node_size",20);

	UpdateWBlocks(twindow);
	twindow->wblock=twindow->wblocks;

	UpdateTrackerWindow(twindow);

	ListAddElement1(&root->song->tracker_windows,&twindow->l);

	//	PixMap_reset(twindow);

	return twindow->l.num;
}








static void handleDirtyBlock(int blocknum){
  struct Tracker_Windows *window = root->song->tracker_windows;

  while(window!=NULL){
    struct WBlocks *wblock=ListFindElement1(&window->wblocks->l,blocknum);

    UpdateAllWTracksCoordinates(window,wblock);
    UpdateAndClearSomeTrackReallinesAndGfxWTracks(
                                                  window,
                                                  wblock,
                                                  0,
                                                  window->wblock->block->num_tracks-1
                                                  );


    if(wblock->curr_realline>=wblock->num_reallines){
      wblock->curr_realline=wblock->num_reallines-1;
    }

    UpdateReallinesDependens(window,wblock);
    window=NextWindow(window);
  }
}

void checkIfWBlocksAreDirty(void) {
  bool is_dirty = false;
  struct Blocks *block=root->song->blocks;

  while(block!=NULL){
    if (block->is_dirty==true){
      handleDirtyBlock(block->l.num);
      block->is_dirty = false;
      is_dirty = true;
    }
    block = NextBlock(block);
  }

  if(is_dirty==true){
    struct Tracker_Windows *window=root->song->tracker_windows;
    while(window!=NULL){
      DrawUpTrackerWindow(window);
      window=NextWindow(window);
    }
  }
}



