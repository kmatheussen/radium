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
   T3: Rendering thread

   If opengl_draw_in_separate_process is set to false, the T2 tasks are performed by T3 instead.
*/

#include <unistd.h>

#include <vlCore/VisualizationLibrary.hpp>
#include <vlVG/VectorGraphics.hpp>

#include <QThread>
#include <QGLContext>

#include "../common/nsmtracker.h"
#include "../common/settings_proc.h"

#include "../common/Queue.hpp"


#define GE_DRAW_VL
#include "GfxElements.h"

#include "T2.hpp"



struct T1_data{
  PaintingData *painting_data;
  GE_Rgb background_color;
  bool stop_me = false;
};


static radium::Queue<T1_data*, 1>  t1_to_t2_queue;
static radium::SyncQueue<T2_data*> t2_to_t3_queue;
static radium::Queue<T2_data*, 8>  t3_to_t2_queue;


enum Use_T2_Thread{ // doesn't work with clang to use enum class here (enum class doesn't work with atomic operations).
  UNINITIALIZED,
  YES,
  NO
};

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

bool T3_use_t2_thread(void){
  R_ASSERT(ATOMIC_GET(g_use_t2_thread) != Use_T2_Thread::UNINITIALIZED);
  
  return ATOMIC_GET(g_use_t2_thread)==Use_T2_Thread::YES;
}

void T3_send_back_old_t2_data(T2_data *t2_data){  
  R_ASSERT(ATOMIC_GET(g_use_t2_thread)==Use_T2_Thread::YES);
  
  t3_to_t2_queue.put(t2_data);
}

void T3_t2_data_picked_up_but_old_data_will_be_sent_back_later(void){
  R_ASSERT(ATOMIC_GET(g_use_t2_thread)==Use_T2_Thread::YES);
    
  t3_to_t2_queue.put(NULL);
}

T2_data::T2_data(PaintingData *painting_data, GE_Rgb background_color)
  : painting_data(painting_data)
  , background_color(background_color)
{
  scroll_transform = new vl::Transform;
  //linenumbers_transform = new vl::Transform;
  scrollbar_transform = new vl::Transform;
  playcursor_transform = new vl::Transform;

  vg = new vl::VectorGraphics;
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


static void T2_thread_func(){
  QOpenGLContext *offscreen_context = NULL;

  QWindow *editor_qwindow = NULL;
  QGLContext *editor_context = NULL;
  QOffscreenSurface *offscreen = NULL;

  GL_set_t2_thread(QThread::currentThread());
    
  // wait until opengl widget has started
  do{
    usleep(1000*1000);
    editor_qwindow = GL_get_editor_qwindow();
    editor_context = GL_get_context();
    offscreen = GL_get_offscreen_surface();
    offscreen_context = GL_get_offscreen_context();
  }while(editor_qwindow==NULL || editor_context==NULL || offscreen == NULL || offscreen_context == NULL || editor_qwindow->isVisible()==false || editor_context->isValid()==false);

#if !CREATE_OFFSCREEN_SURFACE
  QSurface *qsurface = editor_qwindow;
#else
  //QOffscreenSurface *offscreen = new QOffscreenSurface(editor_qwindow->screen());
  //offscreen->setFormat(editor_context->contextHandle()->format());
  //offscreen->create();
  if (offscreen->isValid()==false){
    GFX_Message(NULL, "Invalid offscreen surface. Unable to paint.\n");
    return;
  }
  QSurface *qsurface = offscreen;
#endif

#if 0
  fprintf(stderr, "    T2: new QOpenGLContext;\n");
  offscreen_context = new QOpenGLContext;

#if 0 //FOR_WINDOWS
  QVariant nativeHandle = editor_context->contextHandle()->nativeHandle();
  if (!nativeHandle.isNull() && nativeHandle.canConvert<QWGLNativeContext>()) {
    //QWGLNativeContext nativeContext = nativeHandle.value<QWGLNativeContext>();
    //    HGLRC hglrc = nativeContext.context();
    offscreen_context->setNativeHandle(nativeHandle);
    offscreen_context->setFormat(editor_context->contextHandle()->format());//GL_get_qsurface()->format());
  } else {
    GFX_Message(NULL, "nativeHandlie is null");
  }
#else
  offscreen_context->setFormat(editor_context->contextHandle()->format());//GL_get_qsurface()->format());
#endif

  fprintf(stderr, "    T2: offscreen_context->setShareContext(editor_context->contextHandle());\n");
  offscreen_context->setShareContext(editor_context->contextHandle());

  fprintf(stderr, "    T2: offscreen_context->create();\n");
  offscreen_context->create();
  fprintf(stderr, "    T2: Gotit. (offscreen_context->create();)\n");
#endif
  
  if (offscreen_context->isValid()==false){
    GFX_Message(NULL, "Invalid offscreen OpenGL Context. Unable to paint.\n");
    return;
  }

  //offscreen_context->moveToThread(QThread::currentThread());
  
  while(true){

    T1_data *t1_data = t1_to_t2_queue.get();
    if (t1_data->stop_me){
      delete offscreen_context;
      delete qsurface;
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

    T2_data *old_t2_data = t3_to_t2_queue.get();

    if (old_t2_data != NULL){
      offscreen_context->makeCurrent(qsurface);{      
        delete old_t2_data;
      }offscreen_context->doneCurrent();
    }
    //printf("gl: %f\n",GL_get_vblank());
    
    //    if (is_playing())
    //  usleep(GL_get_vblank() * 10 * 1000); // Sleep a little bit to avoid putting too much pressure on the gfx card this frame.
  }
}


// Using some qt stuff in this thread, so we use a QThread isntead.
//static std::thread t1(T2_thread_func);

namespace{
  struct T2_Thread : public QThread{
    T2_Thread(){
      start();
    }
    
    void run() override {
      while(ATOMIC_GET(g_use_t2_thread)==Use_T2_Thread::UNINITIALIZED)
        usleep(1000*500);
      
      if (ATOMIC_GET(g_use_t2_thread)==Use_T2_Thread::NO)
        return;

      T2_thread_func();
    }
  };
}

static T2_Thread t2_thread;

void T1_ensure_t2_is_initialized(void){
  if (ATOMIC_GET(g_use_t2_thread)==Use_T2_Thread::UNINITIALIZED){
    if(SETTINGS_read_bool("opengl_draw_in_separate_process",false)) //GL_using_nvidia_card()))
      ATOMIC_SET(g_use_t2_thread, Use_T2_Thread::YES);
    else
      ATOMIC_SET(g_use_t2_thread, Use_T2_Thread::NO);
  }
}


void T1_send_data_to_t2(PaintingData *painting_data, GE_Rgb background_color){
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
  T1_data *t1_data = new T1_data;
  t1_data->stop_me = true;
  t1_to_t2_queue.put(t1_data);
  t2_thread.wait();
}


void T1_wait_until_t2_got_t1_data(void){
  while(t1_to_t2_queue.size() > 0)
    usleep(1000*20);
}

