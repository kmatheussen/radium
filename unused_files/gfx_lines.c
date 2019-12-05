/* Copyright 2012 Kjetil S. Matheussen

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

#include <math.h>

#include "nsmtracker.h"

#include "visual_proc.h"
#include "visual_op_queue_proc.h"
#include "gfx_point_proc.h"


// Xiaolin Wu's line algorithm (antialized lines)
//
// Code copied from http://rosettacode.org/wiki/Xiaolin_Wu's_line_algorithm and slightly modified

static void plot(int x, int y, struct Tracker_Windows *window, enum ColorNums color, float brightness, int where){
  if(brightness>1.0f)
    brightness=1.0f;

  GFX_Point(window, color, brightness*MAX_BRIGHTNESS, x, y, where);
}

#define ipart_(X) ((int)(X)) // Note, for negative numbers, floor must be used instead. (no negative numbers used here though)
#define round_(X) ((int)(((double)(X))+0.5))
#define fpart_(X) (((double)(X))-(double)ipart_(X))
#define rfpart_(X) (1.0-fpart_(X)) 
#define swap_(a, b) do{ __typeof__(a) tmp;  tmp = a; a = b; b = tmp; }while(0)

#define USE_ENDPOINTS 0

static void draw_line_aa(
                         struct Tracker_Windows *window,
                         enum ColorNums color,
                         unsigned int x1, unsigned int y1,
                         unsigned int x2, unsigned int y2,
                         int where
                         )
{
  double dx = (double)x2 - (double)x1;
  double dy = (double)y2 - (double)y1;

  if ( fabs(dx) > fabs(dy) ) {

    if ( x2 < x1 ) {
      swap_(x1, x2);
      swap_(y1, y2);
    }


    double gradient = dy / dx;
    double xend = round_(x1);
    double yend = y1 + gradient*(xend - x1);    
    int xpxl1 = xend;
#if USE_ENDPOINTS
    double xgap = rfpart_(x1 + 0.5);
    int ypxl1 = ipart_(yend);
    plot(xpxl1, ypxl1, window, color, rfpart_(yend)*xgap,where);
    plot(xpxl1, ypxl1+1, window, color, fpart_(yend)*xgap,where);
#endif
    double intery = yend + gradient;
 
    xend = round_(x2);
    yend = y2 + gradient*(xend - x2);
    int xpxl2 = xend;
#if USE_ENDPOINTS
    xgap = fpart_(x2+0.5);
    int ypxl2 = ipart_(yend);
    plot(xpxl2, ypxl2, window, color, rfpart_(yend) * xgap,where);
    plot(xpxl2, ypxl2 + 1, window, color, fpart_(yend) * xgap,where);
 #endif
    int x;
    for(x=xpxl1; x <= (xpxl2); x++) {
      plot(x, ipart_(intery), window, color, rfpart_(intery),where);
      plot(x, ipart_(intery) + 1, window, color, fpart_(intery),where);
      intery += gradient;
    }

  } else {

    if ( y2 < y1 ) {
      swap_(x1, x2);
      swap_(y1, y2);
    }

    double gradient = dx / dy;
    double yend = round_(y1);
    double xend = x1 + gradient*(yend - y1);
    int ypxl1 = yend;
#if USE_ENDPOINTS
    double ygap = rfpart_(y1 + 0.5);
    int xpxl1 = ipart_(xend);
    plot(xpxl1, ypxl1, window, color, rfpart_(xend)*ygap,where);
    plot(xpxl1, ypxl1+1, window, color, fpart_(xend)*ygap,where);
#endif
    double interx = xend + gradient;
 
    yend = round_(y2);
    xend = x2 + gradient*(yend - y2);
    int ypxl2 = yend;
#if USE_ENDPOINTS
    ygap = fpart_(y2+0.5);
    int xpxl2 = ipart_(xend);
    plot(xpxl2, ypxl2, window, color, rfpart_(xend) * ygap,where);
    plot(xpxl2, ypxl2 + 1, window, color, fpart_(xend) * ygap,where);
#endif
    int y;
    for(y=ypxl1; y <= (ypxl2); y++) {
      plot(ipart_(interx), y, window, color, rfpart_(interx),where);
      plot(ipart_(interx) + 1, y, window, color, fpart_(interx),where);
      interx += gradient;
    }
  }
}
#undef swap_
#undef plot_
#undef ipart_
#undef fpart_
#undef round_
#undef rfpart_



void PREOS_GFX_Line(struct Tracker_Windows *window,enum ColorNums color,int x,int y,int x2,int y2,int where){
  if(false && x!=x2 && y!=y2){
    draw_line_aa(window,color,x,y,x2,y2,where);
    //OS_GFX_BouncePoints(window); // doesn't seem necessary
  }else{
    OS_GFX_Line(window,color,x,y,x2,y2,where);
  }
}
