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

#if !USE_OPENGL

#include <string.h>

#include "nsmtracker.h"
#include "visual_proc.h"
#include "common_proc.h"

#include "gfx_wtext_proc.h"



void GWT_P_Text(
	struct Tracker_Windows *window,
	int color,
	char *text,
	int x,
	int y,
	bool t
){
	int x2=x+(strlen(text)*window->fontwidth);
	int y2=y+window->fontheight-1;

	if(t){
	  GFX_T_Text(
		     window,
		     color,
		     text,
		     x,
		     y,
                     x2-x,
                     TEXT_CENTER,
                     PAINT_BUFFER
		     );
	  
	  if(strcmp(text,"---")){
	    //GFX_T_Line(window,9,x,y+1,x,y2+1,PAINT_BUFFER);
	    GFX_T_Line(window,9,x,y+1,x2-1,y+1,PAINT_BUFFER);
            GFX_SetMixColor(window, 11, 1, 800);
            GFX_T_Line(window,1,x2-1,y,x2-1,y2+1,PAINT_BUFFER);
            GFX_SetMixColor(window, 11, 1, 800);
	    GFX_T_Line(window,1,x,y2+1,x2-1,y2+1,PAINT_BUFFER);
	  }
	}else{
	  GFX_Text(
		     window,
		     color,
		     text,
		     x,
		     y,
                     x2-x,
                     TEXT_CENTER,
                     PAINT_BUFFER
		     );
	  
#if 1
	    GFX_Line(window,9,x,y,x,y2,PAINT_BUFFER);
	    GFX_Line(window,9,x,y,x2,y,PAINT_BUFFER);
            GFX_SetMixColor(window, 11, 1, 800);
	    GFX_Line(window,1,x2,y,x2,y2,PAINT_BUFFER);
            GFX_SetMixColor(window, 11, 1, 800);
	    GFX_Line(window,1,x,y2,x2,y2,PAINT_BUFFER);
#else
	    GFX_Line(window,2,x,y,x,y2,PAINT_BUFFER);
	    GFX_Line(window,2,x,y,x2,y,PAINT_BUFFER);
	    GFX_Line(window,1,x2,y,x2,y2,PAINT_BUFFER);
	    GFX_Line(window,1,x,y2,x2,y2,PAINT_BUFFER);
#endif
	}
}

/*************************************************************************
  FUNCTION
    
*************************************************************************/
void SetTextNum(
	struct Tracker_Windows *window,
	int color,
	int num,
	int length,
	int x,
	int y,
	bool t
){
	char temp[50];
	char temp2[50];
	char temp3[60];
	int length2;

	sprintf(temp,"%d",num);
	length2=length-strlen(temp);

	if(length2<0) length2=0;

	if(length2!=0){
		memset(temp2,' ',length2+1);
		temp2[length2]=0;
	}else temp2[0]=0;

	sprintf(temp3,"%s%s",temp2,temp);
	temp3[length+1]=0;

	GWT_P_Text(
		window,
		color,
		temp3,
		x,
		y,
		t
	);
}

/*************************************************************************
  FUNCTION
    
*************************************************************************/
int SetTextNumLine(
	struct Tracker_Windows *window,
	struct WBlocks *wblock,
	int color,
	int num,
	int length,
	int x,
	int realline,
	bool t
){
	SetTextNum(
		window,
		color,
		num,
		length,
		x,
		GetReallineY1Pos(window,wblock,realline),
		t
	);
	return 0;
}


/*************************************************************************
  FUNCTION
    
*************************************************************************/
int SetTextLine(
	struct Tracker_Windows *window,
	struct WBlocks *wblock,
	int color,
	char *text,
	int x,
	int realline,
	bool t
){
	GWT_P_Text(
		window,
		color,
		text,
		x,
		GetReallineY1Pos(window,wblock,realline),
		t
	);
	return 0;
}

int SetInvertTextLine(
	struct Tracker_Windows *window,
	struct WBlocks *wblock,
	int color,
	char *text,
	int x,
	int realline,
	bool t
){
  int x2=x+(strlen(text)*window->fontwidth);

  if(t){
	GFX_T_Text(
		window,
		color,
		text,
		x,
		GetReallineY1Pos(window,wblock,realline),
                x2-x,
                TEXT_CENTER | TEXT_INVERT,
                PAINT_BUFFER
	);
  }else{
	GFX_Text(
		window,
		color,
		text,
		x,
		GetReallineY1Pos(window,wblock,realline),
                x2-x,
                TEXT_CENTER | TEXT_INVERT,
                PAINT_BUFFER
	);
  }
	return 0;
}

int SetInvertTextLineNotext(
	struct Tracker_Windows *window,
	struct WBlocks *wblock,
	int color,
	int len,
	int x,
	int realline,
	bool t
){
  char temp[len+1];
  memset(temp,' ',len);
  temp[len]=0;

  int x2=x+(len*window->fontwidth);

  if(t){
	GFX_T_Text(
		window,
		color,
                temp,
		x,
		GetReallineY1Pos(window,wblock,realline),
                x2-x,
                TEXT_CENTER | TEXT_NOTEXT | TEXT_INVERT,
                PAINT_BUFFER
	);
  }else{
	GFX_Text(
		window,
		color,
                temp,
		x,
		GetReallineY1Pos(window,wblock,realline),
                x2-x,
                TEXT_CENTER | TEXT_NOTEXT | TEXT_INVERT,
                PAINT_BUFFER
	);
  }
	return 0;
}


#endif
