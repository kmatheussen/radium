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
#include "placement_proc.h"
#include "realline_calc_proc.h"
#include "pixmap_proc.h"

#include "common_proc.h"


/*************************************************************************
  Common.c

  This file handles common calculation of Graphic Y values. Infact, all
  calculations for Y values should happen here.
*************************************************************************/



/*************************************************************************
  FUNCTION
    Returns the top Y coordinate for the cursor.
*************************************************************************/
int GetCursorY1Pos(const struct Tracker_Windows *window,const struct WBlocks *wblock){
  int curr_visualline=wblock->curr_realline - wblock->top_realline;
  return (curr_visualline*window->fontheight) + wblock->t.y1;
}


/*************************************************************************
  FUNCTION
    Returns the bot Y coordinate for the cursor.
*************************************************************************/
int GetCursorY2Pos(const struct Tracker_Windows *window,const struct WBlocks *wblock){
  return GetCursorY1Pos(window,wblock)+window->fontheight+2;
}


#if !USE_OPENGL

/*************************************************************************
  FUNCTION
    Calculate the top Y value for realline 'realline'.
    Returns -1 if 'realline' is below the visible area.
*************************************************************************/
int GetReallineY1Pos(
	struct Tracker_Windows *window,
	struct WBlocks *wblock,
	int realline
){
	int orgrealline=realline;

	/*
	realline=
		(realline - wblock->curr_realline) * window->fontheight
		+
		GetCursorY1Pos(window,wblock)
	;
	*/
	//	realline=(realline-wblock->top_realline)*window->fontheight + wblock->t.y1;

	realline=PixMap_getY1(window,realline-wblock->top_realline);

	if(realline > 1 + wblock->t.y1+((wblock->num_visiblelines-1)*window->fontheight)){
	  RError("Error in function GetReallineY1Pos in file common.c.\n");
	  RError("realline: %d, retur: %d, wblock->top_realline: %d\n",orgrealline,realline,wblock->top_realline);
		return -1;
	}

	return realline;
}


/*************************************************************************
  FUNCTION
    Calculate the bot Y value for realline 'realline'.
    Returns -1 if 'realline' is below the visible area.
*************************************************************************/
int GetReallineY2Pos(
	struct Tracker_Windows *window,
	struct WBlocks *wblock,
	int realline
){
	realline=GetReallineY1Pos(window,wblock,realline);
	if(realline==-1) return -1;
	return realline+window->fontheight - 1;
}
#endif


// Returns the actual Y1 pos. (GetReallineY1Pos returns the pixmap pos)
int Common_oldGetReallineY1Pos(
			       struct Tracker_Windows *window,
			       struct WBlocks *wblock,
			       int realline
			       )
{
  return ((realline-wblock->top_realline)*window->fontheight)+wblock->t.y1;
}

// Returns the actual Y2 pos. (GetReallineY2Pos returns the pixmap pos)
int Common_oldGetReallineY2Pos(
			       struct Tracker_Windows *window,
			       struct WBlocks *wblock,
			       int realline
			       )
{
  return ((realline-wblock->top_realline)*window->fontheight)+wblock->t.y1+window->fontheight-1;
}


#if !USE_OPENGL
/*************************************************************************
  FUNCTION
    Calculate the top Y value for realline 'realline'.
    If 'realline' is less than 0, or bigger than the lowest
    visible realline, it returns the Y value for realline 0,
    or wblock->num_reallines.
*************************************************************************/
int GetReallineY1SmartPos(
	struct Tracker_Windows *window,
	struct WBlocks *wblock,
	int realline
){
	return GetReallineY1Pos(
		window,
		wblock,
		(realline < 0 ? 0 : 
			(realline >= wblock->num_reallines ? wblock->num_reallines-1 : realline)
		)
	);
}


/*************************************************************************
  FUNCTION
    Calculate the bot Y value for realline 'realline'.
    If 'realline' is less than 0, or bigger than the lowest
    visible realline, it returns the Y value for realline 0,
    or wblock->bot_realline.
*************************************************************************/
int GetReallineY2SmartPos(
	struct Tracker_Windows *window,
	struct WBlocks *wblock,
	int realline
){
	return GetReallineY1SmartPos(window,wblock,realline)+window->fontheight;
}
#endif



/*************************************************************************
  FUNCTION
    Returns the realline an Y coordinate is placed on. If y is not
    placed on a realline, a negative value with number of pixels
    'y' is outside the nearest realline will be returned.
*************************************************************************/
int GetReallineFromY(
	struct Tracker_Windows *window,
	struct WBlocks *wblock,
	int y
){
	int top_realline=wblock->top_realline;
	int bot_realline=wblock->bot_realline;
	int num_reallines=wblock->num_reallines;
	int realtoprealline,realbotrealline;
	int ret;

	ret=(y-wblock->t.y1)/window->fontheight;
	ret+=top_realline;

	if(ret<0 || y<wblock->t.y1){
		realtoprealline=R_MAX(0,top_realline);
		ret=y-Common_oldGetReallineY1Pos(window,wblock,realtoprealline);
	}

	if(ret>bot_realline || ret>=num_reallines || y>wblock->t.y2){
		realbotrealline=R_MIN(num_reallines-1,bot_realline);
		ret=Common_oldGetReallineY2Pos(window,wblock,realbotrealline)-y;
	}

	return ret;

}


/*************************************************************************
  FUNCTION
    Returns nearly the same as GetReallineFromY, but does allso sets
    the values inside 'place' based on 'y'. If the return from
    GetReallineFromY is negative, 'place' gets the nearest
    legal values.

    If minplace is NULL, FirstPlace will be used instead.
    If maxplace is NULL, LastPlace will be used instead.
*************************************************************************/
Place GetPlaceFromY(
                    struct Tracker_Windows *window,
                    struct WBlocks *wblock,
                    float y
                    )
{
	int top_realline     = wblock->top_realline;
	int num_reallines    = wblock->num_reallines;

        float abs_y = (y-(float)wblock->t.y1) + (float)(top_realline*window->fontheight);

        float realline_f = abs_y / (float)window->fontheight;
        int realline = realline_f;

	if(realline<0 || realline_f<0)
          return p_Create(0,0,1);
        
	if(realline>=num_reallines)
          return p_Last_Pos(wblock->block);
        
        float dy = realline_f - realline;
        if (dy<0.0f){
          R_ASSERT_NON_RELEASE(false);          
          dy = 0.0f;
        }
        
        if (dy>=1.0f){
          R_ASSERT_NON_RELEASE(false);
          dy = 0.9999999f;
        }

        //printf("dy: %f\n",dy);
                
        Place x = p_FromFloat(dy);                
        Place x1 = {0,0,1};
        Place x2 = {1,0,1};
                
        Place y1 = wblock->reallines[realline]->l.p;
        Place y2;

        if (realline+1 < wblock->num_reallines)
          y2 = wblock->reallines[realline+1]->l.p;
        else
          y2 = p_Create(wblock->block->num_lines, 0, MAX_UINT32);

        Place ret = p_Scale(x, x1, x2, y1, y2);

        Place minplace = p_Create(0,0,1);
        Place maxplace = p_Last_Pos(wblock->block);
        
	if(p_Less_Than(ret,minplace)){
          R_ASSERT_NON_RELEASE(false);
          ret = minplace;
        }

	if(p_Greater_Than(ret,maxplace)){
          R_ASSERT_NON_RELEASE(false);
          ret = maxplace;
        }

        return ret;
}















