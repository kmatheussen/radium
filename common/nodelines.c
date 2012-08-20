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
#include "placement_proc.h"
#include "realline_calc_proc.h"
#include "nodelines.h"
#include "tbox_proc.h"

#include "nodelines_proc.h"



/*
void FillInLineRealLine(
	struct Tracker_Windows *window,
	struct WBlocks *wblock,
	int realline,

	float x1,float y1,float x2,float y2,

	void *extrainfo,
	void (*ReturnNodeLineData)(
		struct Tracker_Windows *window,
		struct WBlocks *wblock,
		void *extrainfo,
		int firstlast,
		int realline,
		int u_y1,int u_y2,
		float u_x1,float u_x2
	),
	int firstlast
){
	(*ReturnNodeLineData)(
		window,
		wblock,
		extrainfo,
		firstlast,
		realline,
		y1*window->org_fontheight,y2*window->org_fontheight,
		x1,x2
	);
}
*/

#define FillInLineRealLine(a,b,c,d,e,f,g,h,i,j) (*i)(a,b,h,j,c,e,g,(float)(d),(float)(f))


/**********************************************************************
  FUNCTION
    Given the line,counter and dividor for start and end of a nodeline;
    Make nodelines that spans this area.
  INPUTS
    window             - window
    wblock             - wblock
    p1                 - Start placement
    p2                 - End placement
    x1                 - Start x value    (this is not a graphical x, but a vector)
    x2                 - End x value       -------------- "" ---------------------
    minx               - minimum x value   -------------- "" ---------------------
    maxx               - maximum x value   -------------- "" ---------------------
    extrainfo          - Information sent with the ReturnNodeLineData function
    ReturnNodeLineData - When a nodeline has been calculated, this function
                         is called. u_y1,u_y2,u_x1,u_x2 is graphical coordinates.
                         For the parameter 'firstlast', check out nodelines.h.
**********************************************************************/


void MakeNodeLines(
	struct Tracker_Windows *window,
	struct WBlocks *wblock,
	Place *p1,
	Place *p2,
	float x1,float x2,
	float minx,float maxx,
	void *extrainfo,
	void (*ReturnNodeLineData)(
		struct Tracker_Windows *window,
		struct WBlocks *wblock,
		void *extrainfo,
		int firstlast,
		int realline,
		float u_y1,float u_y2,
		float u_x1,float u_x2
	)
){

  float y1=GetFloatFromPlace(p1);
  float y2=GetFloatFromPlace(p2);

  int realline1=FindRealLineFor(wblock,p1->line,p1);
  int realline2=FindRealLineFor(wblock,R_MAX(realline1,p2->line),p2);

  float ry1,ry2;

  /* Get rid of minx. */
  x1-=minx;
  x2-=minx;
  maxx-=minx;

  /* Get rid of maxx. */
  x1/=maxx;
  x2/=maxx;

  if(realline2==realline1){
    ry1=GetFloatFromPlace(&(wblock->reallines[realline1]->l.p));
    if(realline1==wblock->num_reallines-1){
      ry2=wblock->block->num_lines;
    }else{
      ry2=GetFloatFromPlace(&(wblock->reallines[realline1+1]->l.p));
    }

    FillInLineRealLine(
		       window,
		       wblock,
		       realline1,
		       
		       x1,(float)((y1-ry1)/(ry2-ry1)),
		       x2,(float)((y2-ry1)/(ry2-ry1)),

		       extrainfo,
		       ReturnNodeLineData,
		       NODELINE_FIRSTANDLAST
		       );		

  }else{

    int lokke;
    float direction=(x2-x1)/(y2-y1);

    ry1=GetFloatFromPlace(&(wblock->reallines[realline1]->l.p));
    if(realline1==wblock->num_reallines-1){
      ry2=wblock->block->num_lines;
    }else{
      ry2=GetFloatFromPlace(&(wblock->reallines[realline1+1]->l.p));
    }

    FillInLineRealLine(
		       window,
		       wblock,
		       realline1,
		       
		       x1,(float)((y1-ry1)/(ry2-ry1)),
		       (float)((direction*(ry2-y1))+x1),
		       1.0f,
		       
		       extrainfo,
		       ReturnNodeLineData,
		       NODELINE_FIRST
		       );
    
    
    for(lokke=realline1+1;lokke<realline2;lokke++){
      ry1=ry2;
      if(lokke==wblock->num_reallines-1){
	ry2=wblock->block->num_lines;
      }else{
	ry2=GetFloatFromPlace(&(wblock->reallines[lokke+1]->l.p));
      }

      FillInLineRealLine(
			 window,
			 wblock,
			 lokke,
			 
			 (float)((direction*(ry1-y1))+x1),
			 0.0f,
			 (float)((direction*(ry2-y1))+x1),
			 1.0f,
			 
			 extrainfo,
			 ReturnNodeLineData,
			 NODELINE_NOTFIRSTORLAST
			 );
    }


    ry1=ry2;

    if(realline2==wblock->num_reallines-1){
      ry2=wblock->block->num_lines;
    }else{
      ry2=GetFloatFromPlace(&(wblock->reallines[realline2+1]->l.p));
    }

    FillInLineRealLine(
		       window,
		       wblock,
		       realline2,
		       
		       (float)((direction*(ry1-y1))+x1),
		       0.0f,
		       x2,
		       (float)((y2-ry1)/(ry2-ry1)),
		       
		       extrainfo,
		       ReturnNodeLineData,
		       NODELINE_LAST
		       );
  }
}


void GetNodeLine(
		 struct TrackReallineElements *tre,
		 WArea *warea,
		 TBox *within,
		 TBox *ret
		 ){


  ret->y1=within->y1+(tre->y1*(within->y2-within->y1));
  ret->y2=within->y1+(tre->y2*(within->y2-within->y1));

  ret->x1=warea->x+(warea->width*tre->x1);
  ret->x2=warea->x+(warea->width*tre->x2);

  TBOX_within(ret,within);

}


