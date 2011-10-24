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
  routines and the OS/hardware ones.
********************************************************************/

#include "nsmtracker.h"

#define GFX_DONTSHRINK
#include "visual_proc.h"

#include <string.h>


bool GFXS_EnsureBoundaries(
				struct Tracker_Windows *window,
				const char *type,
				int x,int y,int x2,int y2
){
	if(x<0){
		RError("Error. x<0: %d in function %s.\n",x,type);
		return false;
	}
	if(x2<0){
		RError("Error. x2<0: %d in function %s.\n",x2,type);
		return false;
	}
	if(y<0){
		RError("Error. y<0: %d in function %s.\n",y,type);
		return false;
	}
	if(y2<0){
		RError("Error. x<0: %d in function %s.\n",y2,type);
		return false;
	}
	if(y2>=window->height){
		RError("Error. y2>=window->height: %d>=%d in function %s.\n",y2,window->height,type);
		return false;
	}
	if(y>=window->height){
		RError("Error. y>=window->height: %d>=%d in function %s.\n",y,window->height,type);
		return false;
	}
	return true;
}

void GFXS_LineType(
	     void (*GFX_OSFunc)(
				struct Tracker_Windows *window,
				int color,
				int x,int y,int x2,int y2
				),
	     struct Tracker_Windows *window,
	     int color,
	     int x,int y,int x2,int y2
	     )
{
  int maxx=window->wblock->t.x2;

  if(GFXS_EnsureBoundaries(window,"GFXS_LineType",x,y,x2,y2)==false){
    return;
  }

  if(x2>maxx){
    if(x>maxx){
      return;
    }

    y2=(y2-y)*(maxx-x)/(x2-x) + y; // x2-x is never 0 here.

    x2=maxx;
  }

  if(x>maxx){
    y=(y-y2)*(maxx-x2)/(x-x2) + y2; // x2-x is never 0 here.

    x=maxx;
  }


  if(GFXS_EnsureBoundaries(window,"GFXS_LineType2",x,y,x2,y2)==false){
    return;
  }

  (*GFX_OSFunc)(window,color,x,y,x2,y2);


}


void GFXS_BoxType(
	     void (*GFX_OSFunc)(
				struct Tracker_Windows *window,
				int color,
				int x,int y,int x2,int y2
				),
	     struct Tracker_Windows *window,
	     int color,
	     int x,int y,int x2,int y2
	     )
{
  int maxx=window->wblock->t.x2;

  if(x>=maxx) return;

	if(x>x2) return;
	if(y>y2) return;

  if(GFXS_EnsureBoundaries(window,"GFXS_BoxType",x,y,x2,y2)==false){
    return;
  }

  (*GFX_OSFunc)(window,color,x,y,min(x2,maxx),y2);

}


void GFXS_TextType(
	     void (*GFX_OSFunc)(
				struct Tracker_Windows *window,
				int color,char *text,
				int x,int y,
				bool clear
				),
	     struct Tracker_Windows *window,
	     int color,char *text,
	     int x,int y,
	     bool clear
	     )
{
  int maxx=window->wblock->t.x2;
  int len=strlen(text);
  int glen=len*window->fontwidth;
  char temp[100],*to;
  int newlen;

  if(x>=maxx) return;

  if(GFXS_EnsureBoundaries(window,"GFXS_TextType",x,y,x,y)==false){
    return;
  }

  if(x+glen>=maxx){
    newlen=(maxx-x-1)/window->fontwidth;

    if(newlen>99){
      to=talloc(newlen+4);
      sprintf(to,"%s",text);
      text=to;
    }else{
      sprintf(temp,"%s",text);
      text=&temp[0];
    }
    text[newlen]=0;
  }

  if(GFXS_EnsureBoundaries(window,"GFXS_TextType2",x,y,x,y)==false){
    return;
  }

  (*GFX_OSFunc)(window,color,text,x,y,clear);

}


void GFXS_TextType2(
	     void (*GFX_OSFunc)(
				struct Tracker_Windows *window,
				int color,int len,
				int x,int y,
				bool clear
				),
	     struct Tracker_Windows *window,
	     int color,int len,
	     int x,int y,
	     bool clear
	     )
{
  int maxx=window->wblock->t.x2;
  int glen=len*window->fontwidth;

  if(x>=maxx) return;

  if(GFXS_EnsureBoundaries(window,"GFXS_TextType2",x,y,x,y)==false){
    return;
  }

  if(x+glen>=maxx){
    len=(maxx-x-1)/window->fontwidth;
  }

  if(GFXS_EnsureBoundaries(window,"GFXS_TextType22",x,y,x,y)==false){
    return;
  }

  (*GFX_OSFunc)(window,color,len,x,y,clear);

}

void GFXS_BorderType(
		     void (*GFX_P_OSFunc)(
							 struct Tracker_Windows *window,
							 int x, int y, int y2
							 ),
		     struct Tracker_Windows *window,
		     int x, int y, int y2
		     )
{
  int maxx=window->wblock->t.x2;

  if(x>maxx) return;

  if(GFXS_EnsureBoundaries(window,"GFXS_BorderType",x,y,x,y2)==false){
    return;
  }
  (*GFX_P_OSFunc)(window,x,y,y2);
}

void GFXS_BorderType2(
		     void (*GFX_P_OSFunc)(
							 struct Tracker_Windows *window,
							 int x, int y, int y2
							 ),
		     struct Tracker_Windows *window,
		     int x, int y, int y2
		     )
{

  int maxx=window->wblock->t.x2;

  if(x-1>maxx) return;

  if(GFXS_EnsureBoundaries(window,"GFXS_BorderType",x,y,x+1,y2)==false){
    return;
  }
  (*GFX_P_OSFunc)(window,x,y,y2);
}


void GFXS_ScrollType(
		     void (*GFX_P_OSFunc)(
					  struct Tracker_Windows *window,
					  int dx,int dy,
					  int x,int y,
					  int x2,int y2
					  ),
		     struct Tracker_Windows *window,
		     int dx,int dy,
		     int x,int y,
		     int x2,int y2
		     )
{
  struct WBlocks *wblock=window->wblock;

  if(GFXS_EnsureBoundaries(window,"GFXS_ScrollType",x,y,x2,y2)==false){
    return;
  }

  if(x+dx<0){
    x=dx;
  }
  if(x+dx>=wblock->a.x2){
    RError("Error in function GFXS_ScrollType. x+dx>wblock->a.x2: %d,%d,%d\n",x,dx,wblock->a.x2);
    return;
  }

  if(x2+dx<0){
    RError("Error in function GFXS_ScrollType. x2+dx<0: %d,%d\n",x2,dx);
    return;
  }

  if(x2+dx>=wblock->a.x2){
    x2=wblock->a.x2-dx-1;
  }


  if(GFXS_EnsureBoundaries(window,"GFXS_ScrollType2",x,y,x2,y2)==false){
    return;
  }
  
  (*GFX_P_OSFunc)(
		  window,
		  dx,dy,
		  x,y,
		  x2,y2
		  );
}


