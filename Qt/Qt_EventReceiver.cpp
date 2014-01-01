/* Copyright 2003 Kjetil S. Matheussen

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


#include "EditorWidget.h"
#include "GfxTimer_proc.h"
#include "Qt_GraphicsThread_proc.h"

#include <qpainter.h>

#ifdef USE_QT4
//Added by qt3to4:
#include <QCustomEvent>
#include <QCloseEvent>
#include <QWheelEvent>
#include <QPaintEvent>
#include <QResizeEvent>
#include <QPixmap>
#include <QMouseEvent>
#include <QKeyEvent>
#endif

#include <QTimer>
#include <QTime>
#include <QApplication>
#include <QThread>
#include <QGLFramebufferObject>
#include <QGLPixelBuffer>


#include "../common/blts_proc.h"
#include "../common/eventreciever_proc.h"
#include "../common/PEQ_clock_proc.h"
#include "../common/gfx_proc.h"
#include "../midi/midi_i_input_proc.h"

#include "../common/player_proc.h"
#include "../common/gfx_op_queue_proc.h"
#include "../common/visual_proc.h"

#if USE_GTK_VISUAL
#  ifdef __linux__
#    if USE_QT4
#      include <QX11EmbedContainer>
#      define QtXEmbedContainer QX11EmbedContainer
#    endif
#  else
#    define QtXEmbedContainer QWidget
#  endif
   extern QtXEmbedContainer *g_embed_container;
#  include "../GTK/GTK_visual_proc.h"
#endif

extern LANGSPEC void P2MUpdateSongPosCallBack(void);

#include "../common/playerclass.h"

extern PlayerClass *pc;

extern bool is_starting_up;



#if USE_QT_VISUAL && USE_QT4
#if 0
const QPaintEngine* EditorWidget::paintEngine(){     
  //qDebug()<<"Paint Engine";
  return NULL;
}
#endif
#endif

#if USE_GTK_VISUAL
void EditorWidget::paintEvent( QPaintEvent *e ){
  GFX_play_op_queue(window);
}
#endif

#if USE_QT_VISUAL

// These two variables are used to make sure paintEvent isn't called more often than vertical blank. (any call to update() may cause a call to paintEvent())
static int vblank_counter = 0;
static int paint_counter = 0;

static QTime dastime;
static QTime dastime2;

/*
static int average[8] = {0};
static int avg_n = 0;
static float sum_average(){
  float ret=0.0f;
  for(int i=0;i<8;i++)
    ret+=average[i];
  return ret;
}
*/

extern QApplication *g_qapplication;


namespace{

  class GLPainterThread : public QThread
  {
  public:
    GLPainterThread(QGLWidget* _glw)
      : glw(_glw)
      , fbo(NULL)
    {
      //start();
    }

    void run(){
      int width = 0;
      int height = 0;

      //QImage image2("/home/kjetil/snap.png");

      //QGLPixelBuffer pbuffer(1024,1024);

      glw->makeCurrent();

      QPainter p;
      int last_time = 0;

      //BackBuffer *back_buffer2 = NULL;

      while(1) {
        

        if(dastime2.elapsed()>25)
          fprintf(stderr,"PaintGL. Since last: %d\n",dastime2.elapsed());
        dastime2.restart();

        int time1=0,time2=0,time3=0,time4=0,time5=0,time6=0,time7=0,time8=0,time9=0,time10=0;

        BackBuffer *back_buffer=GTHREAD_get_image();

        {
          if(back_buffer==NULL){
            printf("NO BUFFER FROM GTHREAD (xrun)\n");
          } else {

            //QImage das_image = QGLWidget::convertToGLFormat(*back_buffer);
            //QImage *image = &das_image;
            QImage image = *back_buffer;
            
            if(image.width()!=width || image.height()!=height){
              width = image.width();
              height = image.height();
              glViewport (0, 0, width, height);
              glMatrixMode (GL_PROJECTION);     
              glLoadIdentity();
              glOrtho(0, width,0,height,-1,1);
              glMatrixMode (GL_MODELVIEW);

              delete fbo;
              fbo = new QGLFramebufferObject(width,height,QGLFramebufferObject::CombinedDepthStencil);

              printf("width: %d, height: %d\n",width,height);
              //back_buffer2 = back_buffer;
            }
            //else image = *back_buffer2;

            time1=dastime2.elapsed();

            //QImage gldata = *image; //glw->convertToGLFormat(*image);

            //glDrawPixels(width, height, GL_RGBA, GL_UNSIGNED_BYTE, gldata.bits());
            p.begin(fbo);
            p.beginNativePainting();
            //printf("image.format: %d, image2.format: %d\n",image.format(), image2.format());

            time2=dastime2.elapsed();

            //p.drawImage(QPointF(0,0),image2);
            p.drawImage(QPoint(0,0),image); // commented out
            //p.drawPixmap(QPoint(0,0),*image); // commented out
            /*
            p.fillRect(0,0,width,height,QColor(0,0,0));
            p.setPen(QColor(128,128,128));
            p.drawLine(0,0,width,height);
            */

            time3=dastime2.elapsed();

            p.endNativePainting();
            p.end();

            time4=dastime2.elapsed();

            // start
            glEnable(GL_TEXTURE_2D);
            time5=dastime2.elapsed();            
            glTexEnvi(GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, GL_REPLACE);
            time6=dastime2.elapsed();            
            glBindTexture(GL_TEXTURE_2D, fbo->texture());
            time7=dastime2.elapsed();
            glBegin(GL_QUADS);
            time8=dastime2.elapsed();
            QRect target(0,0,width,height);
            glTexCoord2d(0.0,1.0); glVertex2d(target.left(),      target.top());
            glTexCoord2d(1.0,1.0); glVertex2d(target.right() + 1, target.top());
            glTexCoord2d(1.0,0.0); glVertex2d(target.right() + 1, target.bottom() + 1);
            glTexCoord2d(0.0,0.0); glVertex2d(target.left(),      target.bottom() + 1);
            time9=dastime2.elapsed();
            glEnd();
            // end

            time10=dastime2.elapsed();
            GTHREAD_putback_image(back_buffer);
          }
        }

        if(dastime2.elapsed()>12)
          fprintf(stderr,"Painting time: %d (1: %d, 2: %d, 3: %d (%d), 4: %d, 5: %d, 6: %d, 7: %d, 8: %d, 9: %d, 10: %d)\n",dastime2.elapsed(),time1,time2,time3,last_time,time4,time5,time6,time7,time8,time9,time10);
        last_time = time3-time2;

        QTime time;
        time.start();
        QApplication::processEvents(QEventLoop::AllEvents, 3);
        if(time.elapsed()>4)
          printf("elapsed: %d\n",time.elapsed());

        {
          QTime time2;
          time2.start();
          
          //glw->setUpdatesEnabled(true);
          glw->swapBuffers();
          //glw->doneCurrent();

          if(time2.elapsed()>16)
            printf("swap time: %d\n",time2.elapsed());
        }

      }
    }
    
  private:
    QGLWidget* glw;
    QGLFramebufferObject *fbo;
  };
}

//#include <GL/glut.h>

void EditorWidget::initializeGL(){
}

void EditorWidget::resizeGL(int w, int h)
{

  printf("resizeGL %d %d\n",w,h);
  /*
  if(is_starting_up==true)
    return;


  GTHREAD_resize(width(), height());
  */
}


void EditorWidget::paintEvent( QPaintEvent *e ){

  if (paint_counter==vblank_counter)
    return;
  else
    paint_counter++;
  //printf("hepp2\n");
  if(dastime.elapsed()>25)
    printf("PaintEvent. Since last: %d\n",dastime.elapsed());
  dastime.restart();

  QPainter p(this);
  int time1=dastime.elapsed();
  BackBuffer *image = GTHREAD_get_image();
  int time2=dastime.elapsed();
  int time3=0,time4=0,time5=0;
  if(image==NULL){
    printf("NO BUFFER FROM GTHREAD (xrun)\n");
  }else{
    time3=dastime.elapsed();
    p.drawImage(0,0,*image); // commented out.
    time4=dastime.elapsed();
    GTHREAD_putback_image(image);
    time5=dastime.elapsed();
  }

  if(dastime.elapsed()>3)
    printf("Long time painting: %d (%d,%d,%d,%d,%d)\n",dastime.elapsed(),time1,time2,time3,time4,time5);
}

namespace{
  struct VBlankWidget : public QGLWidget {
    
    QGLFormat desiredFormat(){
      QGLFormat fmt;
      fmt.setSwapInterval(1);
      return fmt;
    }
    
    VBlankWidget()
      : QGLWidget(desiredFormat())
    {
      if(format().swapInterval() == -1)
        // V_blank synchronization not available (tearing likely to happen)
        printf("Swap Buffers at v_blank not available: refresh at approx 60fps.");
    }
  };
}

static QGLWidget *glw;

void gakk();
static void main_loop(EditorWidget *editor){
  //QGLWidget *glw = editor; //new VBlankWidget();
  glw = new VBlankWidget();
  gakk();
  glw->makeCurrent();
  QTime main_time;
  main_time.start();

  while(true){
    //printf("Hepp %d\n",vblank_counter);
    //glw->makeCurrent();


    vblank_counter++;
    editor->repaint();


    if(main_time.elapsed()<=16)
      glw->swapBuffers();
    main_time.restart();

    //glw->doneCurrent();    
    
    QTime time;
    time.start();
    
    //editor->setUpdatesEnabled(true);
    QApplication::processEvents(QEventLoop::AllEvents, 3);
    //editor->setUpdatesEnabled(false);

    if(time.elapsed()>10)
      printf("elapsed: %d\n",time.elapsed());

  }
}

/*
void EditorWidget::paintGL(){

  if (paint_counter==vblank_counter)
    return;
  else
    paint_counter++;

}
*/

/*
static void vertical_blank_callback(void *data){
  EditorWidget *editor = (EditorWidget*)data;

  vblank_counter++;
  if(vblank_counter==1)
    dastime.start();

  dastime.restart();

  editor->repaint();
}
*/

void EditorWidget::start_vertical_blank_callback(){
  #if 0
  // *thread = 
  GLPainterThread *glPainterThread = new GLPainterThread(this);
  //GLPainterThread *glPainterThread = new GLPainterThread(glw);
  //call_function_at_vertical_blank(vertical_blank_callback, this);
  
  glPainterThread->run();
#endif
  main_loop(this);
  
}

#endif

struct TEvent tevent={0};



#if 0 // Old QT keyboard code. Not used, put everything through X11 instead.

const unsigned int Qt2SubId[0x2000]={
  EVENT_A,
  EVENT_B,
  EVENT_C,
  EVENT_D,
  EVENT_E,
  EVENT_F,
  EVENT_G,
  EVENT_H,
  EVENT_I,
  EVENT_J,
  EVENT_K,
  EVENT_L,
  EVENT_M,
  EVENT_N,
  EVENT_O,
  EVENT_P,
  EVENT_Q,
  EVENT_R,
  EVENT_S,
  EVENT_T,
  EVENT_U,
  EVENT_V,
  EVENT_W,
  EVENT_X,
  EVENT_Y,
  EVENT_Z
};

//bool EditorWidget::event(QEvent *event){
//  printf("type: %d\n",event->type());
//  return true;
//}




void EditorWidget::keyPressEvent(QKeyEvent *qkeyevent){
  RWarning("keyPressEvent should not be called.\n");

  if(is_starting_up==true)
    return;

  printf("ascii    : %d\n",qkeyevent->ascii());
  printf("key      : %d\n",qkeyevent->key());
  printf("key press: %d,%d\n",qkeyevent->state(),Qt2SubId[max(0,qkeyevent->key()-0x41)]);
  printf("text     : -%s-\n",(const char *)qkeyevent->text());
  printf("Auto     : %d\n\n",qkeyevent->isAutoRepeat());
  // return;

  tevent.ID=TR_KEYBOARD;

  Qt::ButtonState buttonstate=qkeyevent->state();

  tevent.keyswitch=0;

  if(buttonstate&Qt::ShiftModifier) tevent.keyswitch=EVENT_LEFTSHIFT;
  if(buttonstate&Qt::ControlModifier) tevent.keyswitch|=EVENT_LEFTCTRL;
  if(buttonstate&Qt::AltModifier) tevent.keyswitch|=EVENT_RIGHTEXTRA1;

  //  printf("%d\n",qkeyevent->key());


  if(qkeyevent->key()==4117){
      tevent.SubID=EVENT_DOWNARROW;
      //          for(int lokke=0;lokke<50000;lokke++)
      //EventReciever(&tevent,this->window);

  }else{
    if(qkeyevent->key()==4115){
      tevent.SubID=EVENT_UPARROW;      
    }else{
      if(qkeyevent->key()==4114){ //ventre
	tevent.SubID=EVENT_LEFTARROW;
      }else{
	if(qkeyevent->key()==4116){
	  tevent.SubID=EVENT_RIGHTARROW;
	}else{
	  if(qkeyevent->key()==4103){
	    tevent.SubID=EVENT_DEL;
	  }else{
	    if(qkeyevent->key()>=0x41 && qkeyevent->key()<=0x41+20){
	      tevent.SubID=Qt2SubId[max(0,qkeyevent->key()-0x41)];
	    }else{
	      switch(qkeyevent->key()){
	      case 44:
		tevent.SubID=EVENT_MR1;
		break;
	      case 46:
		tevent.SubID=EVENT_MR2;
		break;
	      case 45:
		tevent.SubID=EVENT_MR3;
		break;
	      }
	    }
	  }
	}
      }
    }
  }

  if(qkeyevent->key()==32){
    tevent.SubID=EVENT_SPACE;
  }

   EventReciever(&tevent,this->window);

   updateEditor();
}

void EditorWidget::keyReleaseEvent(QKeyEvent *qkeyevent){
  RWarning("keyReleaseEvent should not be called.\n");

  //  printf("key release: %d\n",qkeyevent->ascii());
  //  printf("key release: %d\n",qkeyevent->key());
  // printf("Released\n");
  //  this->keyPressEvent(qkeyevent);
}

#endif

#if USE_QT_VISUAL

void EditorWidget::mousePressEvent( QMouseEvent *qmouseevent){
  if(is_starting_up==true)
    return;

  if(qmouseevent->button()==Qt::LeftButton){
    tevent.ID=TR_LEFTMOUSEDOWN;
  }else{
    if(qmouseevent->button()==Qt::RightButton){
      tevent.ID=TR_RIGHTMOUSEDOWN;
      //      exit(2);
    }else{
      tevent.ID=TR_MIDDLEMOUSEDOWN;
    }
  }
  tevent.x=qmouseevent->x();//-XOFFSET;
  tevent.y=qmouseevent->y();//-YOFFSET;

  EventReciever(&tevent,this->window);

  setFocus();
}


void EditorWidget::mouseReleaseEvent( QMouseEvent *qmouseevent){
  if(is_starting_up==true)
    return;

  if(qmouseevent->button()==Qt::LeftButton){
    tevent.ID=TR_LEFTMOUSEUP;
  }else{
    if(qmouseevent->button()==Qt::RightButton){
      tevent.ID=TR_RIGHTMOUSEUP;
    }else{
      tevent.ID=TR_MIDDLEMOUSEUP;
    }
  }
  tevent.x=qmouseevent->x();//-XOFFSET;
  tevent.y=qmouseevent->y();//-YOFFSET;


  EventReciever(&tevent,this->window);
}

void EditorWidget::mouseMoveEvent( QMouseEvent *qmouseevent){
  if(is_starting_up==true)
    return;

  tevent.ID=TR_MOUSEMOVE;
  tevent.x=qmouseevent->x();//-XOFFSET;
  tevent.y=qmouseevent->y();//-YOFFSET;
  EventReciever(&tevent,this->window);
}

#endif // USE_QT_VISUAL


#if USE_GTK_VISUAL
void EditorWidget::resizeEvent( QResizeEvent *qresizeevent){ // Only GTK VISUAL!
  if(is_starting_up==true)
    return;

  printf("got resize event\n");
  this->window->width=this->get_editor_width();
  this->window->height=this->get_editor_height();

  // TODO: Is this really necessary? (Yes, with MINGW it is, at least)
  g_embed_container->resize(this->get_editor_width(),this->get_editor_height());

  GTK_SetPlugSize(this->get_editor_width(),this->get_editor_height());
}
#endif // USE_GTK_VISUAL



#if USE_QT_VISUAL
void EditorWidget::resizeEvent( QResizeEvent *qresizeevent){ // Only QT VISUAL!
  if(is_starting_up==true)
    return;

  GTHREAD_resize(width(), height());
}

#endif // USE_QT_VISUAL


void EditorWidget::closeEvent(QCloseEvent *ce){
  printf("Close event\n");
  //  ce->accept();
}
