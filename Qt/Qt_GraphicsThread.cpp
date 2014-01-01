/* Copyright 2013 Kjetil S. Matheussen

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

#include <unistd.h>
#include <jack/ringbuffer.h>

#include <QTime>
#include <QThread>

#include "EditorWidget.h"

#include "../common/gfx_proc.h"
#include "../common/playerclass.h"

#include "Qt_GraphicsThread_proc.h"


extern bool is_starting_up;
extern EditorWidget *g_editor;
extern struct Root *root;
extern PlayerClass *pc;
extern LANGSPEC void P2MUpdateSongPosCallBack(void);



#define IMAGE_RINGBUFFER_SIZE 3
#define RINGBUFFER_SIZE 128

static jack_ringbuffer_t *g_rb_images;
static jack_ringbuffer_t *g_rb_dirty_images;
static jack_ringbuffer_t *g_rb_commandqueue = NULL;


namespace{
struct Command{
  enum Type{
    SCREEN_RESIZE
  } type;
  union{
    int width;
  };
  union{
    int height;
  };
};
}


static QTime dastime;

static int average[8] = {0};
static int avg_n = 0;

static float sum_average(){
  float ret=0.0f;
  for(int i=0;i<8;i++)
    ret+=average[i];
  return ret;
}


static const QImage::Format image_format = QImage::Format_RGB32;


static void generate_image(BackBuffer *image){

  //g_editor->makeCurrent();

  QPainter painter(image);
  //QPainter painter;
  //painter.begin(image);
  //painter.beginNativePainting();
  painter.setRenderHints(QPainter::Antialiasing,true);

#if 0
  static int x=0;
  x++;
  painter.fillRect(0,0,image->width(),image->height(),QColor(x%128,0,0));
  painter.setPen(QColor(128,x%128,128));
  painter.drawLine(x%image->width(),x%image->height(),image->width(),image->height());

#else

  struct Tracker_Windows *window = root->song->tracker_windows;

  g_editor->painter = &painter;

  dastime.restart();
  int time1 = dastime.elapsed();

  if(window->must_redraw==true){
    //printf("** Drawing up everything!\n");
    window->must_redraw=false;
    GFX_clear_op_queue(window);
    DO_GFX(DrawUpTrackerWindow(window));
  }

  int time2 = dastime.elapsed();

  if (pc->isplaying) {
    if (GFX_get_op_queue_size(window)==0)
      Blt_markVisible(window);
    P2MUpdateSongPosCallBack();
    Blt_clearNotUsedVisible(window);
    Blt_blt(window);
  }

  int time3 = dastime.elapsed();

  //printf("paintEvent called. queue size: %d\n",GFX_get_op_queue_size(this->window));
  //printf("paintevent. width: %d, height: %d\n",this->width(),this->height());

  if(GFX_get_op_queue_size(window) > 0){

    {
      GFX_play_op_queue(window);
    }

    //this->painter = NULL;
  }

  //painter.endNativePainting();
  //painter.end();
  //g_editor->doneCurrent();
#endif

  int time4 = dastime.elapsed();

  average[(avg_n++) & 7] = time4;

  if(sum_average() / 8.0f > 14)
    printf("\n\n AVERAGE: %f\n",sum_average() / 8.0f);

  if(time4 > 90)
    printf("time1: %d, time2: %d (%df), time3: %d (%d), time4: %d (%d)\n",time1,time2,time2-time1,time3,time3-time2,time4,time4-time3);
}

static void generate_images(){
  BackBuffer *image;
  int width = g_editor->width();
  int height = g_editor->height();


  while(jack_ringbuffer_read_space(g_rb_dirty_images) >= sizeof(BackBuffer*)){
    jack_ringbuffer_read(g_rb_dirty_images, (char*)&image, sizeof(BackBuffer*));


    if(image->width()!=width || image->height()!=height) {
      delete image;
      //g_editor->makeCurrent();
      image = new BackBuffer(width, height, image_format);
      //g_editor->doneCurrent();
      printf(" GAKK GAKK GAKK GAKK\n");
    }

    generate_image(image);
   
    jack_ringbuffer_write(g_rb_images, (char*)&image, sizeof(BackBuffer*));
  }
}

static void resize(int width, int height){
  struct Tracker_Windows *window = root->song->tracker_windows;

  window->width=width;
  window->height=height;

  g_editor->init_buffers();

  DO_GFX(DrawUpTrackerWindow(window));
}

static void check_command_queue(){
  while(jack_ringbuffer_read_space(g_rb_commandqueue) > 0){
    Command command;
    jack_ringbuffer_read(g_rb_commandqueue, (char*)&command, sizeof(command));
    switch(command.type) {
    case Command::SCREEN_RESIZE:
      resize(command.width, command.height);
      break;
    default:
      RError("check_command_queue: Unknown command: %d\n",command.type);
      break;
    }
  }
}

class MyThread : public QThread{
public:
  void run(){
    // GC_THREAD_INIT();
    dastime.start();

    while(1) {
      usleep(5);
      check_command_queue();
      generate_images();
    }
  }
};


static MyThread mythread;

void GTHREAD_init(){
  int width = g_editor->width();
  int height = g_editor->height();

  g_rb_images = jack_ringbuffer_create(IMAGE_RINGBUFFER_SIZE*sizeof(BackBuffer*));
  g_rb_dirty_images = jack_ringbuffer_create(IMAGE_RINGBUFFER_SIZE*sizeof(BackBuffer*));
  g_rb_commandqueue = jack_ringbuffer_create(RINGBUFFER_SIZE*sizeof(Command));

  while(jack_ringbuffer_write_space(g_rb_dirty_images) >= sizeof(BackBuffer*)){
    //g_editor->makeCurrent();
    BackBuffer *image = new BackBuffer(width, height, image_format);
    //g_editor->doneCurrent();
    jack_ringbuffer_write(g_rb_dirty_images, (char*)&image, sizeof(BackBuffer*));
  }

  mythread.start();
}

void GTHREAD_wakeup(){
}

BackBuffer *GTHREAD_get_image(){
  BackBuffer *image = NULL;
  if (jack_ringbuffer_read_space(g_rb_images)==0)
    return NULL;

  jack_ringbuffer_read(g_rb_images, (char*)&image, sizeof(BackBuffer*));
  return image;
}

void GTHREAD_putback_image(BackBuffer *image){
  jack_ringbuffer_write(g_rb_dirty_images, (char*)&image, sizeof(BackBuffer*));
  GTHREAD_wakeup();
}

static void put(const Command &command){
  jack_ringbuffer_write(g_rb_commandqueue, (char*)&command, sizeof(Command));
  GTHREAD_wakeup();
}

void GTHREAD_resize(int width, int height){

  if(g_rb_commandqueue == NULL)
    return;

  Command command;
  command.type = Command::SCREEN_RESIZE;
  command.width = width;
  command.height = height;
  put(command);
}
