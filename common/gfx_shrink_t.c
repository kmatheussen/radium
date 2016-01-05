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


/*******************************************************************
  Various functions that is called between the programs GFX_*
  routines and the OS/hardware ones. The difference between
  this and gfx_shrink.c is that this functions cuts the
  x parameters after temponodeare.x2 before calling the GFXS_*
  functions.
********************************************************************/

#include "nsmtracker.h"

#define GFX_DONTSHRINK
#include "visual_proc.h"

#include <string.h>

#define getMinX(a) a->wblock->t.x1


void GFXST_LineType(
	     void (*GFX_OSFunc)(
				struct Tracker_Windows *window,
				enum ColorNums color,
				int x,int y,int x2,int y2,
                                int where
				),
	     struct Tracker_Windows *window,
	     enum ColorNums color,
	     int x,int y,int x2,int y2,
             int where
	     )
{
  int minx=getMinX(window);

  if(x<minx){
    if(x2<minx){
      return;
    }

    y=y2-(((y2-y)*(x2-minx))/(x2-x));
    x=minx;
  }

  if(x2<minx){
    y2=y+(((y2-y)*(minx-x2))/(x-x2));

    x2=minx;
  }


  GFXS_LineType(GFX_OSFunc,window,color,x,y,x2,y2,where);

}


void GFXST_BoxType(
	     void (*GFX_OSFunc)(
				struct Tracker_Windows *window,
				enum ColorNums color,
				int x,int y,int x2,int y2,
                                int where
				),
	     struct Tracker_Windows *window,
	     enum ColorNums color,
	     int x,int y,int x2,int y2,
             int where
	     )
{
  int minx=getMinX(window);

  if(x2<minx) return;
  if(x>x2) return;
  if(y>y2) return;

  GFXS_BoxType(GFX_OSFunc,window,color,R_MAX(minx,x),y,x2,y2,where);
}


void GFXST_TextType(
	     void (*GFX_OSFunc)(
				struct Tracker_Windows *window,
				enum ColorNums color,const char *text,
				int x,int y,
                                int width,
                                int flags,
                                int where
				),
	     struct Tracker_Windows *window,
	     enum ColorNums color,const char *text,
	     int x,int y,
             int width,
             int flags,
             int where
	     )
{
  if(flags & TEXT_NOTEXT){
    int minx=getMinX(window);
    GFXS_TextType(GFX_OSFunc,window,color,text,R_MAX(minx,x),y,width,flags,where);
  }else{
    int minx=getMinX(window);

    if(GFX_OSFunc==QUEUE_GFX_Text){
      if(x<minx){
        x=minx;
        //char temp[600];
        //sprintf(temp,"<--%s",text);
        //GFXS_TextType(GFX_OSFunc,window,color,temp,x,y,width,flags,where);
      }else
        GFXS_TextType(GFX_OSFunc,window,color,text,x,y,width,flags,where);
    }else{
      if(x<minx){
        if((int)((strlen(text)+1)*window->fontwidth) < minx) return;
        text+=(minx-x)/window->fontwidth;
        x=minx;
      }
      GFXS_TextType(GFX_OSFunc,window,color,text,x,y,width,flags,where);
    }
  }
}


void GFXST_BorderType(
		     void (*GFX_P_OSFunc)(
                                          struct Tracker_Windows *window,
                                          int x, int y, int y2,
                                          int where
                                          ),
		     struct Tracker_Windows *window,
		     int x, int y, int y2,
                     int where
		     )
{
  int minx=getMinX(window);
  if(x<minx) return;

  GFXS_BorderType(GFX_P_OSFunc,window,x,y,y2,where);
}

void GFXST_BorderType2(
		     void (*GFX_P_OSFunc)(
                                          struct Tracker_Windows *window,
                                          int x, int y, int y2,
                                          int where
                                          ),
		     struct Tracker_Windows *window,
		     int x, int y, int y2,
                     int where
		     )
{
  int minx=getMinX(window);
  if(x<minx) return;

  GFXS_BorderType2(GFX_P_OSFunc,window,x,y,y2,where);
}


