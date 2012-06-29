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
#include "eventreciever_proc.h"
#include "blts_proc.h"
#include "common_proc.h"
#include "sliders_proc.h"
#include "visual_proc.h"

#include "resizewindow_proc.h"



void DrawResizeBox(struct Tracker_Windows *window){
/*
	GFX_Line(
		window,3,
		window->resizebox.x2-1,window->resizebox.y1+1,
		window->resizebox.x1+1,window->resizebox.y2-1
	);
	GFX_Line(
		window,3,
		window->resizebox.x2-1,window->resizebox.y1+1,
		window->resizebox.x2-1,window->resizebox.y2-1
	);
	GFX_Line(
		window,3,
		window->resizebox.x1+1,window->resizebox.y2,
		window->resizebox.x2-1,window->resizebox.y2
	);
*/
	GFX_Line(
		window,1,
		window->resizebox.x2,window->resizebox.y1,
		window->resizebox.x1,window->resizebox.y2
	);
	GFX_Line(
		window,1,
		window->resizebox.x2,window->resizebox.y1,
		window->resizebox.x2,window->resizebox.y2
	);
	GFX_Line(
		window,1,
		window->resizebox.x1,window->resizebox.y2,
		window->resizebox.x2,window->resizebox.y2
	);

}

int FinishedResizing(struct Tracker_Windows *window){
	UpdateTrackerWindow(window);
	SetNormalPointer(window);
	return TreatAllEvents(window);
}

int WindowResize(struct Tracker_Windows *window,int x,int y){
	int minx,miny;

	miny=(window->bottomslider.show*window->bottomslider.width) +
	  window->fontheight*3 + window->org_fontheight*2 +
	  20;

	minx=window->wblock->wtrack->x+50;
	
	if( y < miny) y=miny;

	if( x < minx) x=minx;

	DontTreatAnyEvents_AndDontBuffer(window);
//	GFX_ClearWindow(window);
	return GFX_ResizeWindow(window,x,y);

}

void Resize_resized(struct Tracker_Windows *window,int width,int height,bool iscleared){
  struct WBlocks *wblock;
  int x2,y2;

  printf("Resize_resized called.\n");

  /* Since resize-events might happen before everything is drawn up, we make some tests. */
  if(window==NULL) return;
  if(window->wblock==NULL) return;

  wblock=window->wblock;

  window->width=width;
  window->height=height;

#undef GFX_FilledBox

  GFX_FilledBox(window,0,
		0,
		window->height-window->bottomslider.width-3,
		window->width,
		window->height
		);

  if(iscleared==true){
    UpdateTrackerWindow(window);
  }else{
    DrawUpTrackerWindow(window);
  }
  Blt_blt(window);

  Blt_unMarkVisible(window);



  if(wblock->top_realline<0){
    y2=Common_oldGetReallineY1Pos(window,wblock,0)-1;
    x2=window->width;
    GFX_FilledBox(window,0,
		  window->leftslider.width,wblock->t.y1,
		  x2,y2
		  );
  }

  if(wblock->bot_realline>=wblock->num_reallines){
    y2=Common_oldGetReallineY2Pos(window,wblock,wblock->num_reallines-1)+1;
    x2=window->width;
    GFX_FilledBox(window,0,
		  window->leftslider.width,y2,
		  x2,
		  window->height - window->bottomslider.width -1
		  );
  }else{
    if(window->height-window->bottomslider.width>Common_oldGetReallineY2Pos(window,wblock,wblock->bot_realline)){
      GFX_FilledBox(window,0,
		    window->leftslider.width,Common_oldGetReallineY2Pos(window,wblock,wblock->bot_realline) +1,
		    window->width,
		    window->height - window->bottomslider.width -1
		    );
    }
  }

  GFX_FilledBox(window,0,
		0,wblock->t.y1,
		wblock->zoomlevelarea.x,
		window->height-window->bottomslider.width -1
		);

  DrawLeftSlider(window);

}

