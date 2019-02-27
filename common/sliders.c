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
#include "visual_proc.h"
#include "list_proc.h"
#include "settings_proc.h"
#include "wtracks_proc.h"

#include "sliders_proc.h"


extern int g_default_slider_height;

void InitSliderValues(struct Tracker_Windows *window){
  window->leftslider.show=1;
  window->leftslider.width   = SETTINGS_read_int32("left_slider_width",8);

  window->bottomslider_height = SETTINGS_read_int32("bottom_slider_height", g_default_slider_height - 2); //window->fontheight);//1 + window->fontheight*3/4);
}
