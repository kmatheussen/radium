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




/*

	Handles various functions for pixmap handle.
	A "pixmap" is an invisible painting area,
	that can be copied into a visible area.

	Each Tracker_Windows struct has two pixmaps.
	One that is equally as big as the visible
	window, and one that contains a cursor-line.

	These two pixmaps should be reachable from
	the OS/hardware-spesific functions via the
	"os_visual" field.

	The main pixmap is never a direct copy of the
	visible window. To avoid having the program
	doing two bitmap-copies for each scroll, the
	"pixmapdefs" pointer field contains
	a pointer to an array of integers that defines
	which line in the pixmap that corresponds
	to the line in the real window.
	Also, the cursor-line in the pixmap is
	a normal line, while in the window, it has
	a different background-color.

	The rest of the pixmap; track-headers, sliders,
	time, current octave, reltempo, etc. is
	a direct copy of the visible window,
	used to avoid flickering. Another reason is because
	first rendering to fast-mem and then bitblt
	it to chip-mem is often twice as fast in the Amiga hardware
	than rendering directly to chip. (chip-memory
	contains the internal gfx-memory on amigas).
	For more modern machines than amiga, having a copy
	really doesn't matter when concerning speed.

	The cursor-pixmap is also a direct copy of the
	cursor-line in the window, and could be made by
	xorblitting the pixmap-line that shows the mid-
	line in the window above graphics that shows
	the cursor. However, that is up to the OS implementation
        how the cursor is made.
*/


#include "nsmtracker.h"
#include "visual_proc.h"
#include "gfx_subtrack_proc.h"
#include "common_proc.h"
#include "gfx_shrink_proc.h"

#include "pixmap_proc.h"



void PixMap_reset(
	     struct Tracker_Windows *window
){
  struct WBlocks *wblock=window->wblock;
  int num_lines=wblock->num_visiblelines;
  int lokke;
  int setto;

  if(window->pixmapdefs==NULL || window->num_pixmapdefs<num_lines){
    window->pixmapdefs=talloc_atomic(sizeof(int)*num_lines);
    window->pixmapdefs_calc=talloc_atomic(sizeof(int)*num_lines);
    window->num_pixmapdefs=num_lines;
  }

  setto=0;
  for(lokke=0;lokke<num_lines;lokke++){
    if(
       lokke+wblock->top_realline>=0
       && lokke+wblock->top_realline<wblock->num_reallines
       )
      {
	window->pixmapdefs[lokke]=setto;
	setto++;
      }else{
	window->pixmapdefs[lokke]=-1;
      }
  }

//  printf("\nreset:\n");
//  for(lokke=0;lokke<window->wblock->num_visiblelines;lokke++)
//    printf("vis: %d, val: %d\n",lokke,window->pixmapdefs[lokke]);

}

int PixMap_getVisibleFromVisual(
	struct Tracker_Windows *window,
	int visualline
){
	int *pixmapdefs=window->pixmapdefs;
	int lokke=0;
	while(pixmapdefs[lokke]!=visualline) lokke++;
	return lokke;
}


int PixMap_getY1(
	struct Tracker_Windows *window,
	int visualline
){
  if(window->pixmapdefs==NULL){
    //    return (visualline*window->fontheight)+window->wblock->t.y1;
    PixMap_reset(window);
  }

  return (
	  (
	   window->pixmapdefs[
			      visualline
	   ]
	   * window->fontheight
	   ) +
	  window->wblock->t.y1
	  );
}


int PixMap_getY2(
	struct Tracker_Windows *window,
	int visualline
){
	return PixMap_getY1(window,visualline) + window->fontheight - 1;
}


void PixMap_markNotInUse(
	struct Tracker_Windows *window,
	int start_visible,
	int end_visible
){
	int lokke;
	for(lokke=start_visible;lokke<=end_visible;lokke++){
		window->pixmapdefs[PixMap_getVisibleFromVisual(window,lokke)]=-1;
	}
}

void PixMap_erase(
	struct Tracker_Windows *window,
	int start_visible,
	int end_visible
){
	int lokke;
	int realvisible;

	for(lokke=start_visible;lokke<=end_visible;lokke++){
		realvisible=PixMap_getVisibleFromVisual(window,lokke);
		if(realvisible!=-1){
			GFX_P_FilledBox(
				window,
				0,
				window->wblock->t.x1,
				PixMap_getY1(window,lokke),
				window->wblock->t.x2,
				PixMap_getY2(window,lokke)
			);
			window->pixmapdefs[realvisible]=-1;
		}
	}
}

void PixMap_makeNewDefs(
			struct Tracker_Windows *window,
			int start_visible,
			int end_visible
			){
  int lokke;
  int curr=start_visible;
  struct WBlocks *wblock=window->wblock;

  for(lokke=0;lokke<wblock->num_visiblelines;lokke++)
    window->pixmapdefs_calc[lokke]=0;
  
  for(lokke=0;lokke<wblock->num_visiblelines;lokke++){
    if(window->pixmapdefs[lokke]>=0){
      window->pixmapdefs_calc[window->pixmapdefs[lokke]]=1;
    }
  }
  
  for(lokke=0;lokke<wblock->num_visiblelines;lokke++){
    if(window->pixmapdefs_calc[lokke]==0){
      
      window->pixmapdefs[curr]=lokke;

      GFX_P_FilledBox(
		      window,
		      0,
		      wblock->a.x1,
		      PixMap_getY1(window,curr),
		      wblock->t.x2,
		      PixMap_getY2(window,curr)
		      );
//      printf("clear: curr: %d, pixdef: %d\n",curr,lokke);

      if(curr==end_visible) return;
      //return;
      curr++;
    }
  }
  
  //  RError("Error in function PixMap_makeNewDefs in file common/pixmap.c\n");
  return;
}


void PixMap_scrollDefs(
	struct Tracker_Windows *window,
	int num_lines
){
  int lokke;
  int *pixmapdefs=window->pixmapdefs;
  struct WBlocks *wblock=window->wblock;

  for(lokke=0;lokke<wblock->num_visiblelines;lokke++){
    window->pixmapdefs_calc[lokke]=pixmapdefs[lokke];
  }

  for(lokke=0;lokke<wblock->num_visiblelines;lokke++){
    if(
       lokke+num_lines>=window->wblock->num_visiblelines
       || lokke+num_lines<0
       ){
      if(pixmapdefs[lokke]!=-1){
	pixmapdefs[lokke]=-1;
      }
    }else{
      pixmapdefs[lokke]=window->pixmapdefs_calc[lokke+num_lines];
    }
  }
}


void PixMap_makeCursor(
		       struct Tracker_Windows *window
){
	struct WBlocks *wblock=window->wblock;
	NInt track=window->curr_track;
	int subtrack=window->curr_track_sub;

	int xb1=GetXSubTrack_B1(wblock,track,subtrack)-3;
	int xb2=GetXSubTrack_B2(wblock,track,subtrack)+3;
	xb2=min(wblock->a.x2,xb2);

	if(xb1<=wblock->a.x1 || xb2<=xb1 || xb2<=wblock->a.x1){
	  RError("Error in function PixMap_makeCursor, %d <= %d < %d <= %d\n",wblock->a.x1,xb1,xb2,wblock->a.x2);
	  return;
	}

	/* window,x1,x2,x3,x4,height, y pixmap */

	GFX_C_DrawCursor(
		window,

		wblock->a.x1,

		xb1,xb2,

		wblock->a.x2,

		window->fontheight,

		PixMap_getY1(window,wblock->curr_realline-wblock->top_realline)
		//		GetCursorY1Pos(window,wblock)
	);
}


void PixMap_private_bltLines(
	struct Tracker_Windows *window,
	int startvisibleline,
	int endvisibleline,
	int x1,
	int x2
){
	int num_visiblelines=window->wblock->num_visiblelines;
	int *pixmapdefs=window->pixmapdefs;

	int lokke1=startvisibleline;
	int lokke2=startvisibleline;

	int pixmappos;

	for(;;){
	  pixmappos=pixmapdefs[lokke2];
	  if(pixmappos!=-1)
	    if(
	       pixmappos==num_visiblelines-1
	       || lokke2==endvisibleline
	       || pixmappos+1 != pixmapdefs[lokke2+1]
	       )
	      {
	      
		GFX_P2V_bitBlt(
			       window,
			       x1,PixMap_getY1(window,lokke1),
			       x1,(window->fontheight*lokke1)+window->wblock->t.y1,
			       
			       x2-x1+1,
			       (window->fontheight*(lokke2-lokke1+1))
			       );

//		printf("1: %d, 2: %d\n",PixMap_getY1(window,lokke1),PixMap_getY1(window,lokke1)+(window->fontheight*(lokke2-lokke1+1)));

		if(lokke2==endvisibleline) return;
		lokke1=lokke2+1;
	      }
	  lokke2++;
	}
}

void PixMap_bltLines(
	struct Tracker_Windows *window,
	int startvisibleline,
	int endvisibleline,
	int x1,
	int x2
){

  struct WBlocks *wblock=window->wblock;
  int currvisibleline=wblock->curr_realline-wblock->top_realline;

  if(currvisibleline>=startvisibleline && currvisibleline<=endvisibleline){
    PixMap_makeCursor(window);
    if(currvisibleline==startvisibleline){

      GFX_C2V_bitBlt(window,x1,x2,GetCursorY1Pos(window,wblock));

      if(endvisibleline>=startvisibleline+1){
	PixMap_private_bltLines(window,startvisibleline+1,endvisibleline,x1,x2);      
      }

    }else{

      PixMap_private_bltLines(window,startvisibleline,currvisibleline-1,x1,x2);

      GFX_C2V_bitBlt(window,x1,x2,GetCursorY1Pos(window,wblock));

      if(currvisibleline<endvisibleline){
	PixMap_private_bltLines(window,currvisibleline+1,endvisibleline,x1,x2);      
      }

    }
  }else{
    PixMap_private_bltLines(window,startvisibleline,endvisibleline,x1,x2);
  }
}
