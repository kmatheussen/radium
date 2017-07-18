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
          //RError("Error. x<0: %d in function %s.\n",x,type);
		return false;
	}
	if(x2<0){
          //	RError("Error. x2<0: %d in function %s.\n",x2,type);
		return false;
	}
	if(y<0){
          //	RError("Error. y<0: %d in function %s.\n",y,type);
		return false;
	}
	if(y2<0){
          //	RError("Error. x<0: %d in function %s.\n",y2,type);
		return false;
	}
	if(y2>=window->height){
          //RError("Error. y2>=window->height: %d>=%d in function %s.\n",y2,window->height,type);
          fprintf(stderr, "Error. y2>=window->height: %d>=%d in function %s.\n",y2,window->height,type);
		return false;
	}
	if(y>=window->height){
          //	RError("Error. y>=window->height: %d>=%d in function %s.\n",y,window->height,type);
		return false;
	}
	return true;
}

void GFXS_LineType(
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
  int maxx=window->wblock->t.x2;

  if(GFXS_EnsureBoundaries(window,"GFXS_LineType",x,y,x2,y2)==false){
    OS_GFX_CancelMixColor(window);
    return;
  }

  if(x2>maxx){
    if(x>maxx){
      OS_GFX_CancelMixColor(window);
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
    OS_GFX_CancelMixColor(window);
    return;
  }

  (*GFX_OSFunc)(window,color,x,y,x2,y2,where);


}


void GFXS_BoxType(
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
  int maxx=window->wblock->t.x2;


  if(x>=maxx ||  
     x>x2 ||
     y>y2 ||
     GFXS_EnsureBoundaries(window,"GFXS_BoxType",x,y,x2,y2)==false
     )
    {
      OS_GFX_CancelMixColor(window);
      return;
    }

  (*GFX_OSFunc)(window,color,x,y,R_MIN(x2,maxx),y2,where);

}


void GFXS_TextType(
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
  if(text==NULL){
    RWarning("text==NULL\n");
    return;
  }
  if(flags & TEXT_NOTEXT){
    int len=(int)strlen(text);
    int maxx=window->wblock->t.x2;
    int glen=len*window->fontwidth;

    if(x>=maxx){
      OS_GFX_CancelMixColor(window);
      return;
    }
    
    if(GFXS_EnsureBoundaries(window,"GFXS_TextType NOTEXT",x,y,x,y)==false){
      OS_GFX_CancelMixColor(window);
      return;
    }

    if(x+glen>=maxx){
      len=(maxx-x-1)/window->fontwidth;
    }

    if(GFXS_EnsureBoundaries(window,"GFXS_TextType NOTEXT2",x,y,x,y)==false){
      OS_GFX_CancelMixColor(window);
      return;
    }
    
    (*GFX_OSFunc)(window,color,text,x,y,width,flags,where);
    
  }else{

    int maxx=window->wblock->t.x2;
    int len=(int)strlen(text);
    int glen=len*window->fontwidth;
    char *to;
    int newlen;
    
    if(x>=maxx){
      OS_GFX_CancelMixColor(window);
      return;
    }
    
    if(GFXS_EnsureBoundaries(window,"GFXS_TextType",x,y,x,y)==false){
      OS_GFX_CancelMixColor(window);
      return;
    }

    to=talloc_strdup(text);
    
    if(x+glen>=maxx){
      newlen=(maxx-x-1)/window->fontwidth;
      to[newlen]=0;
    }

    if(GFXS_EnsureBoundaries(window,"GFXS_TextType",x,y,x,y)==false){
      OS_GFX_CancelMixColor(window);
      return;
    }

    (*GFX_OSFunc)(window,color,(const char*)to,x,y,width,flags,where);
  }
}


void GFXS_BorderType(
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
  int maxx=window->wblock->t.x2;

  if(x>maxx){
    OS_GFX_CancelMixColor(window);
    return;
  }
  
  if(GFXS_EnsureBoundaries(window,"GFXS_BorderType",x,y,x,y2)==false){
    OS_GFX_CancelMixColor(window);
    return;
  }
  (*GFX_P_OSFunc)(window,x,y,y2,where);
}

void GFXS_BorderType2(
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

  int maxx=window->wblock->t.x2;

  if(x-1>maxx){
    OS_GFX_CancelMixColor(window);
    return;
  }

  if(GFXS_EnsureBoundaries(window,"GFXS_BorderType",x,y,x+1,y2)==false){
    OS_GFX_CancelMixColor(window);
    return;
  }
  (*GFX_P_OSFunc)(window,x,y,y2,where);
}


void GFXS_BitBltType(
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

  if(GFXS_EnsureBoundaries(window,"GFXS_BitBltType",x,y,x2,y2)==false){
    OS_GFX_CancelMixColor(window);
    return;
  }

  if(x+dx<0){
    x=dx;
  }
  if(x+dx>=wblock->a.x2){
    RError("Error in function GFXS_BitBltType. x+dx>wblock->a.x2: %d,%d,%d\n",x,dx,wblock->a.x2);
    return;
  }

  if(x2+dx<0){
    RError("Error in function GFXS_BitBltType. x2+dx<0: %d,%d\n",x2,dx);
    return;
  }

  if(x2+dx>=wblock->a.x2){
    x2=wblock->a.x2-dx-1;
  }


  if(GFXS_EnsureBoundaries(window,"GFXS_BitBltType2",x,y,x2,y2)==false){
    OS_GFX_CancelMixColor(window);
    return;
  }
  
  (*GFX_P_OSFunc)(
		  window,
		  dx,dy,
		  x,y,
		  x2,y2
		  );
}


