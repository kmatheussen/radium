/* Copyright 2016 Kjetil S. Matheussen

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

#ifndef _RADIUM_OPENGL_T2_HPP
#define _RADIUM_OPENGL_T2_HPP

//#include <vlCore/VisualizationLibrary.hpp>
//#include <vlVG/VectorGraphics.hpp>

#include "GfxElements.h"

#define T3_TO_T2_QUEUE_SIZE 800

struct T2_data
{
	r::PaintingData *painting_data;

	GE_Rgb background_color;
	
	/*
  vl::ref<vl::VectorGraphics> vg;

  vl::ref<vl::Transform> scroll_transform;
  //vl::ref<vl::Transform> linenumbers_transform;
  vl::ref<vl::Transform> scrollbar_transform;
  vl::ref<vl::Transform> playcursor_transform;
	*/
	
	T2_data(r::PaintingData *painting_data, GE_Rgb background_color);
	~T2_data();
};

T2_data *T3_maybe_get_t2_data(void);
bool T3_use_t2_thread(void);
bool T3_t2_data_picked_up_but_old_data_will_be_sent_back_later();
bool T3_send_back_old_t2_data(T2_data *t2_data);

class QOpenGLContext;
void T1_start_t2_thread(QOpenGLContext *widget_context);

void T1_ensure_t2_is_initialized(void);
void T1_send_data_to_t2(r::PaintingData *painting_data, GE_Rgb background_color);
void T1_wait_until_t3_got_t2_data(void);
void T1_wait_until_t2_got_t1_data(void);
void T1_stop_t2(void);

#endif
