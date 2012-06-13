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
#include "resizewindow_proc.h"
#include "pixmap_proc.h"
#include "blts_proc.h"
#include "trackreallineelements_proc.h"

#include "windows_proc.h"




extern struct Root *root;

int CloseTrackerWindow(NInt place){
	struct Tracker_Windows *temp;

	temp=(struct Tracker_Windows *)ListFindElement1(&root->song->tracker_windows->l,place);

	if(temp==NULL) return 1;

	GFX_ShutDownVisual(temp);

//	temp->wblock=NULL;
	while(FreeANotShowedWBlockTREelement()==true);

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
    Call after window has been made, resized or fontsize is changed.
*********************************************************************/
void UpdateTrackerWindowCoordinates(struct Tracker_Windows *window){

	window->bottomslider.x2=window->width - max(window->bottomslider.width,10);

	window->resizebox.x1=window->bottomslider.x2+1;
	window->resizebox.y1=window->height - window->bottomslider.width;
	window->resizebox.x2=window->width-1;
	window->resizebox.y2=window->height-1;

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
	DrawResizeBox(window);
	window->wblock->isgfxdatahere=true;
	root->clearall=0;

}


/**************************************************************************
  FUNCTION
    First clears the window, then draw up everything.
***************************************************************************/
void DrawUpTrackerWindow(struct Tracker_Windows *window){
	struct WBlocks *wblock;
	struct WTracks *wtrack2;

	GFX_P_ClearWindow(window);

#ifndef _AMIGA
	//	GFX_ClearWindow(window);
#endif

	root->clearall=1;
	UpdateTrackerWindowCoordinates(window);
	UpdateWBlockCoordinates(window,window->wblock);

	PixMap_reset(window);

	DrawWBlock(window,window->wblock);
	DrawLeftSlider(window);
	DrawResizeBox(window);
	window->wblock->isgfxdatahere=true;
	root->clearall=0;

	wblock=window->wblock;

	GFX_FilledBox(
			window,0,
			wblock->reltempo.x2+1,
			window->height - window->bottomslider.width+1,
			window->bottomslider.x-1,
			window->height-1
	);

	wtrack2=ListLast1(&wblock->wtracks->l);

	if(wtrack2->fxarea.x2<wblock->a.x2){
	   GFX_P_FilledBox(window,0,wtrack2->fxarea.x2+2,wblock->t.y1,wblock->a.x2,wblock->t.y2);
	   GFX_FilledBox(window,0,wtrack2->fxarea.x2+2,0,wblock->a.x2,wblock->t.y1);
	}

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

	twindow->textborder=true;

	if(GFX_CreateVisual(twindow)!=0)
		return -1;

	twindow->l.num=ListFindFirstFreePlace1(&root->song->tracker_windows->l);

	twindow->playalong=true;

	twindow->leftslider.show=1;
	twindow->bottomslider.show=1;

//	twindow->bottomslider.width=twindow->fontwidth;
	twindow->bottomslider.width=twindow->fontheight*2/3;
	twindow->leftslider.width=twindow->fontwidth;

	twindow->curr_track_sub= -1;

	twindow->minnodesize=4;

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
  struct Blocks *block=root->song->blocks;

  while(block!=NULL){
    if (block->is_dirty==true){
      handleDirtyBlock(block->l.num);
      block->is_dirty = false;
    }
    block = NextBlock(block);
  }
}



