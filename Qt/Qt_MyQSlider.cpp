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


#include "Qt_MyQSlider.h"

#include "Qt_SliderPainter_proc.h"

#include "../common/OS_visual_input.h"


QVector<MyQSlider*> g_all_myqsliders;

static MyQSlider *find_slider(struct Patch *patch, int effect_num){
  for(int i=0;i<g_all_myqsliders.size();i++){
    MyQSlider *slider= g_all_myqsliders[i];
    if(slider->_patch==patch && slider->_effect_num==effect_num)
      return slider;
  }
  fprintf(stderr,"Not found, Qt_MyQSlider.cpp\n"); // could be a checkbox.
  //  abort();
  return NULL;
}

float *OS_SLIDER_obtain_automation_value_pointer(struct Patch *patch,int effect_num){
  MyQSlider *slider= find_slider(patch,effect_num);
  if(slider==NULL)
    return NULL;
  else
    return SLIDERPAINTER_obtain_automation_value_pointer(slider->_painter);
}

int *OS_SLIDER_obtain_automation_color_pointer(struct Patch *patch,int effect_num){
  MyQSlider *slider= find_slider(patch,effect_num);
  if(slider==NULL)
    return NULL;
  else
    return SLIDERPAINTER_obtain_automation_color_pointer(slider->_painter);
}

void OS_SLIDER_release_automation_pointers(struct Patch *patch,int effect_num){
  //RError("Dont think it is a good idea to call OS_SLIDER_release_automation_pointers");
  MyQSlider *slider= find_slider(patch,effect_num);
  if(slider==NULL)
    return;
  else
    SLIDERPAINTER_release_automation_pointers(slider->_painter);
}

