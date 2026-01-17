/* Copyright 2014-2016 Kjetil S. Matheussen

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


#include <sys/types.h>
#include <unistd.h>
#include <errno.h>

#include <bitset>

#include <QWidget>

#if USE_QT5
#include <QWindow>
#include <QScreen>
#endif

#include <math.h>
#include <stdio.h>

#include <QTextEdit>
#include <QMessageBox>
#include <QApplication>
#include <QAbstractButton>
//#include <QGLFormat>
#include <QDebug>
#include <QElapsedTimer>
#include <QOperatingSystemVersion>

#define GE_DRAW_VL

#include "../common/nsmtracker.h"
#include "../common/windows_proc.h"
#include "../common/playerclass.h"
#include "../common/list_proc.h"
#include "../common/realline_calc_proc.h"
#include "../common/time_proc.h"
#include "../common/disk.h"
#include "../common/sequencer_proc.h"
#include "../common/settings_proc.h"
#include "../common/OS_Semaphores.h"
#include "../common/OS_Player_proc.h"
#include "../common/Semaphores.hpp"
#include "../common/Mutex.hpp"
#include "../common/Vector.hpp"
#include "../common/MovingAverage.hpp"
#include "../common/player_proc.h"
#include "../common/visual_proc.h"

#include "../embedded_scheme/scheme_proc.h"

#include "../mixergui/QM_MixerWidget.h"

#include "../Qt/Qt_Bs_edit_proc.h"
#include "../Qt/Timer.hpp"
#include "../Qt/Qt_Fonts_proc.h"
#include "../Qt/Qt_mix_colors.h"

#include "../audio/Juce_plugins_proc.h"

#include "../api/api_gui_proc.h"

#include "GfxElements.h"
//#include "T2.hpp"
#include "Timing.hpp"
#include "Render_proc.h"
//#include "CheckOpenGL_proc.h"

DEFINE_ATOMIC(char *, GE_vendor_string) = strdup("TODO/FIX: vendor-string not set by Radium yet");

static DEFINE_ATOMIC(int, g_curr_realline);

// TS (called from both main thread and opengl thread)
void GE_set_curr_realline(int curr_realline){
  //printf("  ############      Setting g_curr_realline to %d\n", curr_realline);
  ATOMIC_SET(g_curr_realline, curr_realline);
}

#if 0
// OpenGL thread
static float GE_scroll_pos(const SharedVariables *sv, double realline){
  double extra = sv->top_realline - sv->curr_realline;
  return
    (   (realline+extra) * sv->fontheight  );
}
#endif

extern int scrolls_per_second;
extern int default_scrolls_per_second;


// The time_estimator is used to estimate the screen refresh rate according to the soundcard clock.
// I.e. 60.0 hz when using the gfx card clock is unlikely to be 60.0 hz when using the soundcard clock.
//
// However, since we put a high pass filter on the scroll position, it doesn't really matter, so
// we only estimate using QT4. In Qt5, we just use the widget->windowHandle()->screen()->refreshRate() value instead.
//
static TimeEstimator time_estimator;

#if 0
// OpenGL thread
static double get_realline_stime(const SharedVariables *sv, int realline){
  double blocktime;
  if(realline==sv->num_reallines)
    blocktime = sv->block_duration;
  else
    blocktime = Place2STime_from_times2(sv->times, p_getDouble(sv->reallines[realline]->l.p));
  
  return blocktime_to_seqtime_double(sv->seqblock_stretch, blocktime);
}
#endif

#if 0
// OpenGL thread
static bool need_to_reset_timing(const SharedVariables *sv, double stime, int last_used_i_realline, const struct Blocks *last_used_block, double last_used_stime, double blocktime){
  if (stime < 0){
    fprintf(stderr,"Error: stime: %f, pc->blocktime: %f\n",stime,blocktime);
    #if 0
      #if !defined(RELEASE)
        abort();
      #endif
    #endif
    return true;
  }

  if (last_used_block != sv->block)    
    return true;
  
  if(last_used_i_realline>=sv->num_reallines) // First check that i_realline is within the range of the block. (block might have changed number of lines)
    return true;
    
  // TODO: Make the "last_stime < stime"-check configurable.
  if (stime < last_used_stime)
    return true;
  
  if(stime < get_realline_stime(sv, last_used_i_realline)) // Time is now before the line we were at when we left last time. Start searching from 0 again. (Not sure if is correct. It might be last_used_i_realline+1 instead)
    return true;

  return false;
}
#endif

#if 0
// OpenGL thread
static double find_current_realline_while_playing(const SharedVariables *sv, double blocktime){

  double time_in_ms = blocktime * 1000.0 / (double)pc->pfreq; // I'm not entirely sure reading pc->start_time_f instead of pc->start_time is unproblematic.
  double stime      = time_estimator.get(time_in_ms, sv->reltempo * ATOMIC_DOUBLE_GET(g_curr_song_tempo_automation_tempo)) * (double)pc->pfreq / 1000.0; // Could this value be slightly off because we just changed block, and because of that we skipped a few calles to time_estimator.get ? (it shouldn't matter though, timing is resetted when that happens. 'time_in_ms' should always be valid)

  //stime      = time_in_ms* (double)pc->pfreq / 1000.0;

  static double last_stime = stime;
    
    
  //Strictly speaking, we need to atomically get current block + pc->blocktime. But it is uncertain how important this is is.
  // Maybe store blocktime in the block itself?
  
  static int i_realline = 0; // Note that this one is static. The reason is that we are usually placed on the same realline as last time, so we remember last position and start searching from there.
  static const struct Blocks *block = NULL; // Remember block used last time. If we are not on the same block now, we can't use the i_realline value used last time.

  R_ASSERT(i_realline>=0);
  
  //                                    Common situation. We are usually on the same line as the last visit,
  if (i_realline > 0) i_realline--; //  but we need to go one step back to reload prev_line_stime.
  //                                    (storing last used 'stime1' and/or 'stime2' would be an optimization which would make my head hurt and make no difference in cpu usage)

  if (need_to_reset_timing(sv, stime, i_realline, block, last_stime, blocktime)) {
    i_realline = 0;
    block = sv->block;
    time_estimator.set_time(time_in_ms);
    stime = time_in_ms * (double)pc->pfreq / 1000.0; // Convert the current block time into number of frames.
  }

  //  stime -= 24000;
      
  last_stime = stime;
  
  double stime2 = get_realline_stime(sv, i_realline);
  
  while(true){

    double stime1 = stime2;
    for(;;){ // This for loop is here to handle a very special situation where we play so fast that stime1==stime2. In normal songs, this should not happen.
      stime2 = get_realline_stime(sv, i_realline+1);

#if 0
      if (stime1==stime2){ // Could probably happen if playing really fast... Not sure. (yes, it happens if playing really fast)
#if !defined(RELEASE)
        /abort();
#endif
        return i_realline;
      }
#endif
      
      if (i_realline==sv->num_reallines)
        return sv->num_reallines;
      if (equal_doubles(stime1, stime2))
        i_realline++;
      else
        break;
    }
      
    if (stime >= stime1 && stime <= stime2){
      return scale_double(stime,
                          stime1, stime2,
                          i_realline, i_realline+1
                          );
    }

    i_realline++;

    if (i_realline==sv->num_reallines)
      break;
  }

  return sv->num_reallines;
}
#endif


#if 0
// Main thread
static Tracker_Windows *get_window(void){
  return root->song->tracker_windows;
}

// Main thread
static EditorWidget *get_editorwidget(void){
  return (EditorWidget *)get_window()->os_visual.widget;
}
#endif


volatile float g_scroll_pos = 0.0f;

static DEFINE_ATOMIC(double, g_vblank) = 1000 / 60.0;

void GL_update(void)
{	
	if (SCHEME_is_currently_getting_scheme_history()) // Avoid deadlock when assertion reporter is showing.
		return;
}

namespace
{
struct RenderWidget : public QWidget
{
	DEFINE_ATOMIC(bool, _main_window_is_exposed) = false;

	void paintEvent(QPaintEvent *ev) override
	{
		TRACK_PAINT();
		
		QPainter p(this);

		p.fillRect(rect(), get_qcolor(PEAKS_COLOR_NUM));
	}

};
} // anon. namespace


bool g_gl_widget_started = false;
static RenderWidget *g_widget;

static double get_refresh_rate(void){
  
  QWindow *qwindow = g_widget->windowHandle();
  if (qwindow!=NULL){

    double ratio = qwindow->devicePixelRatio();
    //ratio = 0.5;

    if (fabs(ratio-1.0) > 0.01){

      /*
      static bool has_shown_warning = false;

      if (has_shown_warning==false){
        RT_message("Note: High DPI display detected. Radium has not supported high DPI display very long. If you notice strange graphics, please report it.");
        has_shown_warning=true;
      }
      */
      
	    safe_double_write(&g_opengl_scale_ratio, ratio);
      
      // Make sure editor font is scaled.
      if(root!=NULL && root->song!=NULL && root->song->tracker_windows!=NULL){
        setFontValues(root->song->tracker_windows);
      }
      
    }
    
    QScreen *qscreen = qwindow->screen();
    if (qscreen!=NULL)
	{
		//maybe_start_t2_thread();

		return qscreen->refreshRate();
    }

  }

  return -1;
}


static DEFINE_ATOMIC(bool, g_has_found_refresh_rate) = false;

static void setup_screen_changed_callback(void){
	QWindow *qwindow = g_widget->windowHandle();
	R_ASSERT(qwindow != NULL);
	
	if (qwindow==NULL)
		return;
	
	qwindow->connect(qwindow, &QWindow::screenChanged, [](QScreen *screen){
		
		R_ASSERT(screen != NULL);
		
		if (screen==NULL)
			return;
		
		double refresh_rate = screen->refreshRate();
		
		printf("  NEW REFRESH RATE: %f\n", refresh_rate);
		
		if (refresh_rate >= 0.5)
		{
			//widget->set_vblank(1000.0 / refresh_rate);
			ATOMIC_DOUBLE_SET(g_vblank, 1000.0 / refresh_rate);
		}
		
  });
}
  

double GL_get_vblank(void){
  if (ATOMIC_GET(g_has_found_refresh_rate)==false)
    return -1;
  else
    return ATOMIC_DOUBLE_GET(g_vblank);
}

int GL_maybe_notify_that_main_window_is_exposed(int interval){
	static int ret = 0;

	if (ret==0)
	{
		QWindow *window = g_widget->windowHandle();
		if (window != NULL){
			if (window->isExposed()) {
				ATOMIC_SET(g_widget->_main_window_is_exposed, true);
				ret = 1;
			}
		}
	}
	else
	{
    
		ret = 2;
    
		static int downcounter = 0;
		if (downcounter==0) {
#if !defined(RELEASE)
			printf("Checking refresh rate\n");
#endif
			double refresh_rate = get_refresh_rate();
			if (refresh_rate >= 0.5) {
				//g_widget->set_vblank(1000.0 / refresh_rate);
				ATOMIC_DOUBLE_SET(g_vblank, 1000.0 / refresh_rate);
				ATOMIC_SET(g_has_found_refresh_rate, true);
				downcounter = 200 * 1000 / interval; // Check refresh rate every 200 seconds.

				setup_screen_changed_callback();
        
			}else{
				printf("Warning: Unable to find screen refresh rate\n");
				downcounter = 500 / interval; // Check again in 0.5 seconds
			}
		}else
			downcounter--;
	}

	return ret;
}

bool GL_check_compatibility(void)
{
	return true;
}

void GL_set_vsync(bool onoff){
  SETTINGS_write_bool("vsync", onoff);
}

bool GL_get_vsync(void){
  return SETTINGS_read_bool("vsync", true);
}

void GL_set_multisample(int size){
  SETTINGS_write_int("multisample", size);
}

int GL_get_multisample(void){
  return R_BOUNDARIES(1, SETTINGS_read_int32("multisample", 4), 32);
}

void GL_set_safe_mode(bool onoff){
  printf("setting safe mode to %d\n",onoff);
  SETTINGS_write_bool("safe_mode", onoff);
}


static bool g_high_render_thread_priority = true;
bool GL_get_high_render_thread_priority(void){
  static bool s_has_inited = false;
  if (s_has_inited==false){
    g_high_render_thread_priority = SETTINGS_read_bool("high_render_thread_priority", g_high_render_thread_priority);
    s_has_inited = true;
  }
  
  return g_high_render_thread_priority;
}

void GL_set_high_render_thread_priority(bool onoff){
  printf("setting safe mode to %d\n",onoff);
  SETTINGS_write_bool("high_render_thread_priority", onoff);
  g_high_render_thread_priority = onoff;
	/*
  if (g_render_thread != NULL)
    g_render_thread->setPriority(onoff ? QThread::HighestPriority : QThread::NormalPriority);
	*/
}


bool GL_get_safe_mode(void){
  return SETTINGS_read_bool("safe_mode", false);
}

static bool g_pause_rendering_on_off = false;

static void init_g_pause_rendering_on_off(void){
  g_pause_rendering_on_off = SETTINGS_read_bool("pause_rendering", false);
}

void GL_set_pause_rendering_on_off(bool onoff){
  SETTINGS_write_bool("pause_rendering", onoff);
  g_pause_rendering_on_off = onoff;
}

bool GL_get_pause_rendering_on_off(void){
  return g_pause_rendering_on_off;
}


QWidget *GL_create_widget(QWidget *parent)
{
	init_g_pause_rendering_on_off();
	
	g_widget = new RenderWidget();
	g_gl_widget_started = true;
	
	return g_widget;
}


void GL_stop_widget(QWidget *widget)
{
	R_ASSERT(widget == g_widget);
	delete widget;
	g_widget = NULL;
}
