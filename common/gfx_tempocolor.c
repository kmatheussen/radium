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


#include "nsmtracker.h"
#include <math.h>
#include "time_proc.h"
#include "wblocks_proc.h"
#include "placement_proc.h"
#include "time_proc.h"
#include "visual_proc.h"
#include "common_proc.h"
#include "blts_proc.h"

#include "gfx_tempocolor_proc.h"

void WBLOCK_DrawLineTempoColor(
				struct Tracker_Windows *window,
				int x,int y,int width,
				int color1,int color2,
				float mixfactor
				)
{
	GFX_Point(
                  window,color1,256,
                  x+(int)(mixfactor*width),
                  y,
                  PAINT_BUFFER
	);
}


void WBLOCK_DrawMixedTempoColor(
				struct Tracker_Windows *window,
				int x,int y,int width,
				int color1,int color2,
				float mixfactor
				)
{
  int lokke;
  //  int mf=mixfactor*1000;

  srand(y);

  for(lokke=0;lokke<width;lokke++){
    //       printf("random: %f, max: %d\n",(rand()*1000/RAND_MAX,RAND_MAX);
    GFX_Point(
              window,(float)rand()/(float)RAND_MAX>mixfactor?color2:color1,256,
              x+lokke,
              y,
              PAINT_BUFFER
              );
    
  }
}



void WBLOCK_DrawNotMixedTempoColor(
				struct Tracker_Windows *window,
				int x,int y,int width,
				int color1,int color2,
				float mixfactor
				)

{
  GFX_Line(
	     window,color1,
	     x,
	     y,
	     x+(int)(width*mixfactor),
	     y,
             PAINT_BUFFER
	     );
  GFX_Line(
	     window,color2,
	     x+(int)(width*mixfactor),
	     y,
	     x+width,
	     y,
             PAINT_BUFFER
	     );

}

#if 0
float scale(float x, float x1, float x2, float y1, float y2){
  return y1 + ( ((x-x1)*(y2-y1))
                /
                (x2-x1)
                );
}
#endif

void WBLOCK_DrawTempoColor(
	struct Tracker_Windows *window,
	struct WBlocks *wblock,
	int start_realline,
	int end_realline
){

  //const int numcolors=3;
  //const int colors[3]={1,4,2};
  const int numcolors=6;
  const int colors[6]={1,3,4,5,6,7};
  const int maxtime=6300;
  const int mintime=200;

  int realline,line;
  const int fontheight=window->fontheight;
  static int maxfontheight=0;
  static int *stimes=NULL;
  Place p;
  //,*p1,*p2;
  float fp1=0,fp2=0;
  float colortouse;
  int colortousebase;
  float colortousefloor;

	if(wblock->tempocolorarea.width==0) return;


  if(WBlock_legalizeStartEndReallines(wblock,&start_realline,&end_realline)==false){
    return;
  }

  if(maxfontheight<fontheight){
    stimes=talloc_atomic(sizeof(int)*(fontheight+1));
    maxfontheight=fontheight;
  }

  /*
    realline=69
    num_reallines=70
   */

  for(realline=start_realline;realline<=end_realline;realline++){
    fp1=GetfloatFromPlace(&wblock->reallines[realline]->l.p);
    if(realline<wblock->num_reallines-1){
      fp2=GetfloatFromPlace(&wblock->reallines[realline+1]->l.p);
    }else{
      fp2=(float)wblock->block->num_lines;
    }

    for(line=0;line<fontheight;line++){
	Float2Placement((float)(line*(fp2-fp1)/fontheight+fp1),&p);
	stimes[line]=Place2STime(wblock->block,&p);
    }

    if(realline<wblock->num_reallines-1){
      Float2Placement(fp2,&p);
      stimes[fontheight]=Place2STime(wblock->block,&p);
    }else{
      stimes[fontheight]=getBlockSTimeLength(wblock->block);
      line=fontheight-1;
      colortouse=((numcolors-1)*(stimes[line+1]-stimes[line]-mintime));
      colortouse/=(maxtime-mintime);
    }
    

    for(line=0;line<fontheight;line++){
      int delta = (int) ( (double)(stimes[line+1]-stimes[line]) / wblock->block->reltempo);
      //printf("line: %d, delta: %d (%d - %d)\n",line,delta,mintime, maxtime);
      colortouse=(
                  (numcolors-1)* (delta-mintime)
                  );

      colortouse/=(maxtime-mintime);
      colortousebase=R_BOUNDARIES(0,(int)colortouse,numcolors-1);
      colortousefloor=1.0f-(colortouse-(float)colortousebase);

      //	WBLOCK_DrawLineTempoColor(
      WBLOCK_DrawMixedTempoColor(
      //      WBLOCK_DrawNotMixedTempoColor(
				    window,
				    wblock->tempocolorarea.x,
				    GetReallineY1Pos(window,wblock,realline)+line,
				    wblock->tempocolorarea.width,
				    colors[colortousebase],colors[colortousebase+1],
				    colortousefloor
				    );
    }
  }
  Blt_marktrack(window,TEMPOCOLORTRACK,TEMPOCOLORTRACK,false,start_realline,end_realline);
}








