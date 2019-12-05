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

#include <string.h>

#include "nsmtracker.h"

#include "visual_proc.h"
#include "visual_op_queue_proc.h"



void PREOS_GFX_Text(
                    struct Tracker_Windows *tvisual,
                    enum ColorNums color,
                    const char *text,
                    int x,
                    int y,
                    int width,
                    int flags,
                    int where
                    )
{

  // Cliprect on
  if(flags & TEXT_CLIPRECT){
    if(width==TEXT_IGNORE_WIDTH){
      RError("width can not be TEXT_IGNORE_WIDTH when using the TEXT_CLIPRECT flag");
    }else{
      if(width<=0)
        return;
      OS_GFX_SetClipRect(tvisual,x,y,x+width,y+(tvisual->fontheight+20),where);
    }
  }


  // Fill background color
  if(flags & TEXT_INVERT){
    OS_GFX_FilledBox(tvisual,color,x,y,x+(tvisual->fontwidth*(int)strlen(text)),y+tvisual->fontheight,where);
  }else if(flags & TEXT_CLEAR){
    OS_GFX_FilledBox(tvisual,0,x,y,x+(tvisual->fontwidth*(int)strlen(text)),y+tvisual->fontheight,where);
  }

  // Draw text
  {
    int text_color = (flags & TEXT_INVERT) ? 0 : color;
    OS_GFX_Text(tvisual,text_color,text,x,y,width,flags,where);
  }

  // Cliprect off
  if(flags & TEXT_CLIPRECT)
    OS_GFX_CancelClipRect(tvisual,where);

}

