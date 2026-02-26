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


/**
   T1: Main thread
   T2: Drawer thread
   T3: Rendering thread (OpenGL Thread)

   If opengl_draw_in_separate_process is set to false, the T2 tasks are performed by T3 instead.
*/

#include <unistd.h>

#include <QThread>
//#include <QGLContext>

#include "../common/nsmtracker.h"
#include "../common/settings_proc.h"

#include "../common/QueueStack.hpp"


#define GE_DRAW_VL
#include "GfxElements.h"

#include "T2.hpp"


namespace{
struct T1_data
{
	r::PaintingData *painting_data;
	GE_Rgb background_color;
	bool stop_me = false;
};
}


static radium::Queue<T1_data*, 1>  t1_to_t2_queue;
static radium::SyncQueue<T2_data*> t2_to_t3_queue;
static radium::Queue<T2_data*, T3_TO_T2_QUEUE_SIZE>  t3_to_t2_queue; // The queue doesn't really have to be able to hold more than 1, perhaps 2 or 3 elements, ideally, but we set it somewhat higher to lower the risk of growing the buffers indefinitely in case something is wrong.


namespace{
enum Use_T2_Thread{ // doesn't work with clang to use enum class here (enum class doesn't work with atomic operations).
  UNINITIALIZED,
  YES,
  NO
};
}

static DEFINE_ATOMIC(Use_T2_Thread, g_use_t2_thread) = Use_T2_Thread::UNINITIALIZED;


T2_data *T3_maybe_get_t2_data(void){
  if (ATOMIC_GET(g_use_t2_thread)==Use_T2_Thread::YES){
    
    bool got_new_t2_data;
    
    T2_data *t2_data = t2_to_t3_queue.T2_tryGet(got_new_t2_data);
    
    if (!got_new_t2_data)
      return NULL;

    return t2_data;

  } else if (ATOMIC_GET(g_use_t2_thread)==Use_T2_Thread::NO){

    bool got_new_t1_data;
    
    T1_data *t1_data = t1_to_t2_queue.tryGet(got_new_t1_data);

    if (!got_new_t1_data)
      return NULL;

    T2_data *t2_data = new T2_data(t1_data->painting_data, t1_data->background_color);

    GE_draw_vl(t2_data);

    delete t1_data;

    return t2_data;

  } else {

    return NULL;
    
  }
}

/*
void T1_wait_until_t3_got_t2_data(void){
  if (ATOMIC_GET(g_use_t2_thread)==Use_T2_Thread::YES){
    while(t2_to_t3_queue.size() > 0)
      msleep(20);
  }
}
*/

bool T3_use_t2_thread(void){
  enum Use_T2_Thread state = ATOMIC_GET(g_use_t2_thread);
  R_ASSERT(state != Use_T2_Thread::UNINITIALIZED);

  return state==Use_T2_Thread::YES;
}



bool T3_send_back_old_t2_data(T2_data *t2_data){  
  R_ASSERT(ATOMIC_GET(g_use_t2_thread)==Use_T2_Thread::YES);

  // Using tryPut instead of put in case queue is full. If not, we can deadlock. Shouldn't happen, but just in case.
  return t3_to_t2_queue.tryPut(t2_data);
}

bool T3_t2_data_picked_up_but_old_data_will_be_sent_back_later(void){
  R_ASSERT(ATOMIC_GET(g_use_t2_thread)==Use_T2_Thread::YES);

  // Using tryPut instead of put in case queue is full. If not, we can deadlock. Shouldn't happen, but just in case.
  return t3_to_t2_queue.tryPut(NULL); // Wake up t2 thread. See [ref 1] below.
}

T2_data::T2_data(r::PaintingData *painting_data, GE_Rgb background_color)
  : painting_data(painting_data)
  , background_color(background_color)
{
#if 0
	scroll_transform = new vl::Transform;
	//linenumbers_transform = new vl::Transform;
	scrollbar_transform = new vl::Transform;
	playcursor_transform = new vl::Transform;
	
	vg = new vl::VectorGraphics;
#endif
}

T2_data::~T2_data(){
  GE_delete_painting_data(painting_data);
}


#define CREATE_OFFSCREEN_SURFACE 1

#include <QOffscreenSurface>
#include <QOpenGLContext>
#include <QWindow>

#if FOR_WINDOWS
#include <QtPlatformHeaders/QWGLNativeContext>
#endif

#define TEST_TIME 0


#if defined(RELEASE) && TEST_TIME==1
#error "oops"
#endif


static QThread *g_draw_thread = NULL;
static bool g_high_draw_thread_priority = true;

bool GL_get_high_draw_thread_priority(void){
  static bool s_has_inited = false;
  if (s_has_inited==false){
    g_high_draw_thread_priority = SETTINGS_read_bool("high_draw_thread_priority", g_high_draw_thread_priority);
    s_has_inited = true;
  }
  
  return g_high_draw_thread_priority;
}

void GL_set_high_draw_thread_priority(bool onoff){
  printf("setting safe mode to %d\n",onoff);
  SETTINGS_write_bool("high_draw_thread_priority", onoff);
  g_high_draw_thread_priority = onoff;
  if (g_draw_thread != NULL)
    g_draw_thread->setPriority(onoff ? QThread::HighPriority : QThread::NormalPriority);
}


static void T2_thread_func(QOffscreenSurface *offscreen, QOpenGLContext *offscreen_context){
  
  if (offscreen->isValid()==false){
    GFX_Message2(NULL, true, "Invalid offscreen surface. Unable to paint.\n");
    return;
  }

  QSurface *qsurface = offscreen;

  if (offscreen_context->isValid()==false){
    GFX_Message2(NULL, true, "Invalid offscreen OpenGL Context. Unable to paint.\n");
    return;
  }

  g_draw_thread = QThread::currentThread();
  if (GL_get_high_draw_thread_priority()){
    g_draw_thread->setPriority(QThread::HighPriority);
  }

  while(true){

    T1_data *t1_data = t1_to_t2_queue.get();
    if (t1_data->stop_me){
      return;
    }
  
    T2_data *t2_data;

    offscreen_context->makeCurrent(qsurface);{

      while(t3_to_t2_queue.size() > 0){
        T2_data *old_t2_data = t3_to_t2_queue.get();
        delete old_t2_data;
      }
      
      t2_data = new T2_data(t1_data->painting_data, t1_data->background_color);

#if TEST_TIME
      double start = TIME_get_ms();
#endif

      GE_draw_vl(t2_data);
      
#if TEST_TIME
      printf("      GE_draw: %f\n", TIME_get_ms() - start);
#endif
      
    }offscreen_context->doneCurrent();
    
    delete t1_data;

    t2_to_t3_queue.T1_put(t2_data);
    t2_to_t3_queue.T1_wait_for_T2_to_pick_up();    

    T2_data *old_t2_data = t3_to_t2_queue.get(); // [ref 1]

    if (old_t2_data != NULL){
      offscreen_context->makeCurrent(qsurface);{      
        delete old_t2_data;
      }offscreen_context->doneCurrent();
    }
    //printf("gl: %f\n",GL_get_vblank());
    
    //    if (is_playing())
    //  msleep(GL_get_vblank() * 10); // Sleep a little bit to avoid putting too much pressure on the gfx card this frame.
  }
}



namespace{
  struct T2_Thread : public QThread{

    QOffscreenSurface *_offscreen = NULL;
    QOpenGLContext *_offscreen_context = NULL;
    
    T2_Thread(QOpenGLContext *widget_context){

      setObjectName("T2_Thread");
      
      _offscreen_context = new QOpenGLContext;
      
      _offscreen_context->setFormat(widget_context->format());//GL_get_qsurface()->format());
      _offscreen_context->setShareContext(widget_context);
      _offscreen_context->create();
      _offscreen_context->moveToThread(this);
      
      
      _offscreen = new QOffscreenSurface; //(qwindow->screen());
      _offscreen->setFormat(_offscreen_context->format());
      _offscreen->create();
      
      start();
    }

    ~T2_Thread(){
    }
    
    void run() override {
      R_ASSERT_RETURN_IF_FALSE(ATOMIC_GET(g_use_t2_thread)== Use_T2_Thread::YES);
      
      T2_thread_func(_offscreen, _offscreen_context);
      
      delete _offscreen;
      delete _offscreen_context;
    }
  };
}

static T2_Thread *t2_thread = NULL;

void T1_start_t2_thread(QOpenGLContext *widget_context){
	return;
  T1_ensure_t2_is_initialized();
  if (ATOMIC_GET(g_use_t2_thread)== Use_T2_Thread::YES){
    t2_thread = new T2_Thread(widget_context);
  }
}

void T1_ensure_t2_is_initialized(void){
	return;
  if (ATOMIC_GET(g_use_t2_thread)==Use_T2_Thread::UNINITIALIZED){
    if(SETTINGS_read_bool("opengl_draw_in_separate_process",true))
      ATOMIC_SET(g_use_t2_thread, Use_T2_Thread::YES);
    else
      ATOMIC_SET(g_use_t2_thread, Use_T2_Thread::NO);
  }
}


void T1_send_data_to_t2(r::PaintingData *painting_data, GE_Rgb background_color){
	return;
  T1_data *t1_data = new T1_data;

  t1_data->painting_data    = painting_data;
  t1_data->background_color = background_color;

  // Drain queue first. Overflowing the queue is not good for interactivity.  In addition, we don't want a situation where the queue grows faster than t2 is able to handle.
  // And, we also want to avoid waiting for space to be available in the queue. (radium::Queue->put() buzy loops while waiting for space to be availabe in the queue)
  while(t1_to_t2_queue.size() > 0){
    bool gotit;
    T1_data *old = t1_to_t2_queue.tryGet(gotit);
    if(gotit){
      GE_delete_painting_data(old->painting_data);
      delete old;
    }
  }

  t1_to_t2_queue.put(t1_data);
}

void T1_stop_t2(void){
	return;
  if(t2_thread!=NULL){
    T1_data *t1_data = new T1_data;
    t1_data->stop_me = true;
    t1_to_t2_queue.put(t1_data);
    t2_thread->wait();
  }
}

void T1_wait_until_t2_got_t1_data(void){
	return;
#if THREADED_OPENGL
  while(t1_to_t2_queue.size() > 0)
    msleep(20);
#endif
}

