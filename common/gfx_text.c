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



static void draw_text(
                      struct Tracker_Windows *tvisual,
                      int color,
                      char *text,
                      int x,
                      int y,
                      int width,
                      int flags,
                      bool is_pixmap
                      )
{

  // Cliprect on
  if(flags & TEXT_CLIPRECT){
    if(width==TEXT_IGNORE_WIDTH){
      RError("width can not be TEXT_IGNORE_WIDTH when using the TEXT_CLIPRECT flag");
    }else{
      if(width<=0)
        return;
      if(is_pixmap)
        OS_GFX_P_SetClipRect(tvisual,x,y,x+width,y+(tvisual->fontheight+20));
      else
        OS_GFX_SetClipRect(tvisual,x,y,x+width,y+(tvisual->fontheight+20));
    }
  }


  // Fill background color
  if(flags & TEXT_INVERT){
    if(is_pixmap)
      OS_GFX_P_FilledBox(tvisual,color,x,y,x+(tvisual->fontwidth*strlen(text)),y+tvisual->fontheight);
    else
      OS_GFX_FilledBox(tvisual,color,x,y,x+(tvisual->fontwidth*strlen(text)),y+tvisual->fontheight);
  }else if(flags & TEXT_CLEAR){
    if(is_pixmap)
      OS_GFX_P_FilledBox(tvisual,0,x,y,x+(tvisual->fontwidth*strlen(text)),y+tvisual->fontheight);
    else
      OS_GFX_FilledBox(tvisual,0,x,y,x+(tvisual->fontwidth*strlen(text)),y+tvisual->fontheight);
  }

  // Draw text
  {
    int text_color = (flags & TEXT_INVERT) ? 0 : color;
    if(is_pixmap)
      OS_GFX_P_Text(tvisual,text_color,text,x,y,width,flags);
    else
      OS_GFX_Text(tvisual,text_color,text,x,y,width,flags);
  }

  // Cliprect off
  if(flags & TEXT_CLIPRECT){
    if(is_pixmap)
      OS_GFX_P_CancelClipRect(tvisual);
    else
      OS_GFX_CancelClipRect(tvisual);
  }

}

void PREOS_GFX_Text(
                    struct Tracker_Windows *tvisual,
                    int color,
                    char *text,
                    int x,
                    int y,
                    int width,
                    int flags
                    )
{
  draw_text(tvisual,color,text,x,y,width,flags,false);
}

void PREOS_GFX_P_Text(
                      struct Tracker_Windows *tvisual,
                      int color,
                      char *text,
                      int x,
                      int y,
                      int width,
                      int flags
                      )
{
  draw_text(tvisual,color,text,x,y,width,flags,true);
}
