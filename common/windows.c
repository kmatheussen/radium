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
#include "pixmap_proc.h"
#include "blts_proc.h"
#include "gfx_op_queue_proc.h"
#include "settings_proc.h"
#include "cursor_proc.h"
#include "gfx_subtrack_proc.h"
#include "gfx_point_proc.h"

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

#if 0
// Use DrawUpTrackerWindow instead.
void UpdateTrackerWindow(struct Tracker_Windows *window){
	UpdateTrackerWindowCoordinates(window);
	UpdateWBlockCoordinates(window,window->wblock);

	PixMap_reset(window);

	DrawWBlock(window,window->wblock);
	DrawLeftSlider(window);
	window->wblock->isgfxdatahere=true;
}
#endif

// Can not do that since it moves qt widgets. Probably better to do it in the main timer instead before calling update().
#define UPDATECOORDINATES_WHEN_DRAWING 0

/**************************************************************************
  FUNCTION
    Redraw without flickering.
***************************************************************************/
void DrawUpTrackerWindow(struct Tracker_Windows *window){
  //printf("  Draw up tracker 1. width: %d, height: %d\n",window->width,window->height);

  if(window->must_redraw==true)
    return;

#if UPDATECOORDINATES_WHEN_DRAWING
  struct WBlocks *wblock = window->wblock;
#endif
  
#if 0
        while(GetXSubTrack_B2(wblock,window->curr_track,window->curr_track_sub) >= wblock->a.x2){

          if(window->curr_track<wblock->left_track)
            break; // should not be possible

          if(window->curr_track==wblock->left_track && window->curr_track_sub==wblock->left_subtrack)
            break;

          CursorLeft_CurrPos(window);
        }
#endif

#if !USE_OPENGL
        GFX_BouncePoints(window); // To clear point buffer. (TODO: Implement a clear point buffer function.)
#endif

#if UPDATECOORDINATES_WHEN_DRAWING
	UpdateTrackerWindowCoordinates(window);

	UpdateWBlockCoordinates(window,wblock);
#endif


#if !USE_OPENGL
	PixMap_reset(window);
#endif

#if !USE_OPENGL
	struct WTracks *wtrack=ListLast1(&wblock->wtracks->l);
        int x2=wtrack->fxarea.x2;
        EraseAllLines(window, window->wblock, 0, x2);

	GFX_FilledBox(window,0,
                      x2,0,
                      window->width-1,window->height-1,
                      PAINT_BUFFER);

	DrawLeftSlider(window);
#endif

	DrawWBlock(window,window->wblock);
        
	window->wblock->isgfxdatahere=true;

#if !USE_OPENGL
        ClearUnusedWindowsAreas(window);

        Blt_unMarkVisible(window); // Need a better name for this function.
#endif
}


/**************************************************************************
  FUNCTION
    When making a new window.
***************************************************************************/

int OpenTrackerWindow(int x, int y, int width,int height){
	struct Tracker_Windows *twindow;

	twindow=talloc(sizeof(struct Tracker_Windows));

	twindow->l.num = root->song->tracker_windows==NULL ? 0 : ListFindFirstFreePlace1(&root->song->tracker_windows->l);

	twindow->x=x;
	twindow->y=y;
	twindow->width=width;
	twindow->height=height;

#ifdef USE_GFX_OP_QUEUE
        GFX_create_op_queue(twindow);
#endif

	if(GFX_CreateVisual(twindow)!=0)
		return -1;

	twindow->playalong=true;

        InitSliderValues(twindow);

	twindow->curr_track_sub= -1;

        twindow->show_lpb_track=true;
        twindow->show_bpm_track=true;
        twindow->show_reltempo_track=true;
        
	//twindow->minnodesize=SETTINGS_read_int("minimum_node_size",20);

	UpdateWBlocks(twindow);
	twindow->wblock=twindow->wblocks;

	//UpdateTrackerWindow(twindow);
        //DrawUpTrackerWindow(twindow);

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
      //DrawUpTrackerWindow(window);
      window->must_redraw = true;
      window=NextWindow(window);
    }
  }
}


void ValidateCursorPos(struct Tracker_Windows *window){
  struct WBlocks *wblock = window->wblock;
  struct WTracks *wtrack = wblock->wtrack;
  
  int num_subtracks = WTRACK_num_subtracks(wtrack);
  
  if (window->curr_track_sub >= num_subtracks)
    window->curr_track_sub = num_subtracks - 1;
}

