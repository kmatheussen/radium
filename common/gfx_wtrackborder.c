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


#define GFX_DONTSHRINK
#include "nsmtracker.h"
#include "visual_proc.h"


void GFX_DrawTrackBorderDouble(
                               struct Tracker_Windows *window,
                               int x, int y, int y2,
                               int where
                               )
{
  QUEUE_GFX_Line(window,1,x,y,x,y2,where);
  QUEUE_GFX_Line(window,2,x+1,y,x+1,y2,where);
}

void GFX_DrawTrackBorderSingle(
                               struct Tracker_Windows *tvisual,
                               int x, int y, int y2,
                               int where
){
  QUEUE_GFX_Line(tvisual,2,x,y,x,y2,where);
}
