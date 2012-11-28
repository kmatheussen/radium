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
#include "tbox_proc.h"

#include "nodeboxes_proc.h"


#if 0
void MakeBlackBox(
	struct Tracker_Windows *window,
	int u_x,
	int u_y,
	int width,
	struct TrackReallineElements *tre
){
	int size=R_MAX(window->minnodesize,window->fontwidth/4);

	tre->y1=R_MAX(0,u_y-size);
	tre->y2=R_MIN(window->fontheight-1,u_y+size);
	tre->x1=R_MAX(0,u_x-size);
	tre->x2=R_MIN(width,u_x+size);
}
#endif


void GetNodeBox_basic(
		struct TrackReallineElements *tre,
		WArea *warea,
		TBox *within,
		TBox *ret,
		int boxwidth,
		int boxheight
){
	ret->y1 = within->y1 + (tre->y1*(within->y2-within->y1)) - (boxheight/2);
	ret->y2 = within->y1 + (tre->y1*(within->y2-within->y1)) + (boxheight/2);

	ret->x1 = warea->x + (tre->x1*warea->width) - (boxwidth/2);
	ret->x2 = warea->x + (tre->x1*warea->width) + (boxwidth/2);

	TBOX_within(ret,within);
	//	printf("%d,%d - %d,%d\n",ret->x1,ret->y1,ret->x2,ret->y2);
}

void GetNodeBox_customsize(
		struct TrackReallineElements *tre,
		WArea *warea,
		TBox *within,
		TBox *ret,
		int boxwidth,
		int boxheight
){

  GetNodeBox_basic(tre,warea,within,ret,boxwidth,boxheight);
  tre->x2=(float)boxwidth;
  tre->y2=(float)boxheight;
}

int GetNodeSize(struct Tracker_Windows *window, struct WBlocks *wblock, struct WTracks *wtrack){
  return window->fontheight*2;//R_MAX(window->minnodesize*2/3,window->fontwidth*2/3);
}

void GetNodeBox(
		struct Tracker_Windows *window,
                struct WBlocks *wblock,
                struct WTracks *wtrack,
		struct TrackReallineElements *tre,
		WArea *warea,
		TBox *within,
		TBox *ret
){
  int size=GetNodeSize(window,wblock,wtrack);
  GetNodeBox_customsize(tre,warea,within,ret,size,size);
}



bool isInsideNodeBox(
		 struct TrackReallineElements *tre,
		 WArea *warea,
		 TBox *within,
		 int x,int y
		 ){

  TBox get;
  GetNodeBox_basic(tre,warea,within,&get,(int)tre->x2-tre->x1,(int)tre->y2-tre->y1);

  //printf("tbox: %d/%d -> %d/%d\n",get.x1,get.y1,get.x2,get.y2);
  //printf("tre: %f/%f -> %f/%f\n",tre->x1,tre->y1,tre->x2,tre->y2);

  return insideTBox(&get,x,y);
}

