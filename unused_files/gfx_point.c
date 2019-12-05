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

#include "nsmtracker.h"

#define GFX_DONTSHRINK
#include "visual_proc.h"
#include "visual_op_queue_proc.h"

#include "gfx_point_proc.h"


struct Points{
  int pos;
  int size;
  uint16_t *x;
  uint16_t *y;
};

static struct Points *points[16][MAX_BRIGHTNESS+1] = {{0}};

static bool is_dirty=false;

void GFX_Point(
               struct Tracker_Windows *window,
               enum ColorNums color,
               int brightness,
               int x,int y,
               int where
               )
{
  if(where==PAINT_DIRECTLY){
    RError("GFX_Point can not be called with where==PAINT_DIRECTLY");
    return;
  }

  if(color>=16 || color<0){
    RError("Illegal color: %d",color);
    color = R_ABS(color) % 16;
  }

  brightness = R_BOUNDARIES(0, brightness, MAX_BRIGHTNESS);

#if 0
  OS_GFX_Point(window,color,brightness,x,y,where);
  return;
#endif

  struct Points *point=points[color][brightness];
  if(point==NULL)
    points[color][brightness] = point = V_calloc(1,sizeof(struct Points));

  int pos = point->pos;

  if(point->pos==point->size){
    point->size = R_MAX(128,point->size*2);
    point->x=V_realloc(point->x,point->size*sizeof(uint16_t));
    point->y=V_realloc(point->y,point->size*sizeof(uint16_t));
  }

  point->x[pos] = x;
  point->y[pos] = y;
  point->pos = pos+1;

  is_dirty=true;
}

void GFX_BouncePoints(struct Tracker_Windows *window){
  int color;
  int bright;

  if(is_dirty==false)
    return;

  //printf("Bouncing points\n");
  for(color=0;color<16;color++){
    for(bright=0;bright<=MAX_BRIGHTNESS;bright++){
      struct Points *point=points[color][bright];
      if(point!=NULL){
        if(point->pos==1){
          OS_GFX_Point(window,(enum ColorNums)color,bright,point->x[0],point->y[0],PAINT_BUFFER);
          //printf("single point %d/%d, %d/%d\n",color,bright,point->x[0],point->y[0]);
          point->pos=0;
        }else if(point->pos>1){
          OS_GFX_Points(window,(enum ColorNums)color,bright,point->pos,point->x,point->y,PAINT_BUFFER);
          //printf("point %d/%d, %d/%d\n",color,bright,point->x[0],point->y[0]);
          point->pos=0;
        }
      }
    }
  }

  is_dirty=false;
}

