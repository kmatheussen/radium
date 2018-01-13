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



#include <QGraphicsItem>

struct SliderPainter;


SliderPainter *SLIDERPAINTER_create(QAbstractSlider *qslider);
SliderPainter *SLIDERPAINTER_create(QGraphicsItem *graphics_item, int x1, int y1, int x2, int y2);
void SLIDERPAINTER_prepare_for_deletion(SliderPainter *painter);
void SLIDERPAINTER_delete(SliderPainter *painter);

void SLIDERPAINTER_became_visible(SliderPainter *painter);
void SLIDERPAINTER_became_invisible(SliderPainter *painter);

void SLIDERPAINTER_paint(SliderPainter *painter, QPainter *p);

void SLIDERPAINTER_set_num_channels(SliderPainter *painter, int num_channels);

void SLIDERPAINTER_setValue(SliderPainter *painter, int value); // only works with graphics item.

void SLIDERPAINTER_set_peak_value_pointers(SliderPainter *painter, int num_channels, float *pointers, bool single_line_style);
void SLIDERPAINTER_set_automation_value_pointer(SliderPainter *painter, enum ColorNums color_num, float *pointer);
void SLIDERPAINTER_call_regularly(SliderPainter *painter, int num_channels);

void SLIDERPAINTER_set_alternative_color(SliderPainter *painter, bool setit);
void SLIDERPAINTER_set_recording_color(SliderPainter *painter, bool setit);
void SLIDERPAINTER_set_string(SliderPainter *painter,QString string);
QString SLIDERPAINTER_get_string(SliderPainter *painter);
