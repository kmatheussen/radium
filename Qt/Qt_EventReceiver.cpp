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



#include "MyWidget.h"

#include <qpainter.h>

#ifdef USE_QT4
//Added by qt3to4:
#include <QCustomEvent>
#include <QCloseEvent>
#include <QTimerEvent>
#include <QWheelEvent>
#include <QPaintEvent>
#include <QResizeEvent>
#include <QPixmap>
#include <QMouseEvent>
#include <QKeyEvent>
#endif

#include "../common/resizewindow_proc.h"
#include "../common/blts_proc.h"
#include "../common/eventreciever_proc.h"
#include "../common/PEQ_clock_proc.h"
#include "../common/gfx_proc.h"

#include "../common/player_proc.h"
#include "../common/gfx_op_queue_proc.h"


#if 0
void MyWidget::timerEvent(QTimerEvent *){
  PlayerTask(40);
}
#endif

#if 0
static double get_ms(void){
  struct timespec ts;
  clock_gettime(CLOCK_MONOTONIC, &ts);
  return ts.tv_sec * 1000.0 + ((double)ts.tv_nsec) / 1000000.0;
}
#endif

extern LANGSPEC void P2MUpdateSongPosCallBack(void);



void MyWidget::customEvent(QCustomEvent *e){
  {
    P2MUpdateSongPosCallBack();
    UpdateClock(this->window);
  }

  update();
}


#ifdef USE_QT4
const QPaintEngine* MyWidget::paintEngine(){     
  //qDebug()<<"Paint Engine";
  return NULL;
}
#endif

void MyWidget::paintEvent( QPaintEvent *e ){

//  setBackgroundColor( this->colors[0] );		/* white background */

  //Resize_resized(this->window,this->width()-100,this->height()-30,false);
#if 0
  static int times=0;
  fprintf(stderr,"\n\n*************** paintEVent. Queue size: %d. Erased? %s **************** %d\n\n",GFX_get_op_queue_size(this->window),e->erased()?"TRUE":"FALSE",times++);
#endif

#ifdef USE_QT4
  static int times = 0;
  if(GFX_get_op_queue_size(this->window) ==0) {
    DO_GFX_BLT(DrawUpTrackerWindow(window));
    fprintf(stderr," painting up everything, %d\n",times++);
  }
#endif

#ifdef USE_QT3
  if(e->erased())
    DO_GFX_BLT(DrawUpTrackerWindow(window));
#endif

  if(GFX_get_op_queue_size(this->window) > 0){
    QPainter paint(this);
    QPainter pixmap_paint(this->qpixmap);
    QPainter cursorpixmap_paint(this->cursorpixmap);
    //paint.setRenderHints(QPainter::Antialiasing);
    //pixmap_paint.setRenderHints(QPainter::Antialiasing);
    //this->pixmap_painter->setPen(this->colors[5]);

    paint.translate(XOFFSET,YOFFSET);   // Don't paint on the frame.

    this->painter = &paint;
    this->qpixmap_painter = &pixmap_paint;
    this->cursorpixmap_painter = &cursorpixmap_paint;

    this->qpixmap_painter->setFont(this->font);
    this->painter->setFont(this->font);
    this->setFont(this->font);

    {
      GFX_play_op_queue(this->window);
    }

    this->painter = NULL;
    this->qpixmap_painter = NULL;
    this->cursorpixmap_painter = NULL;
  }


  //    UpdateTrackerWindow(this->window);

    //    QPainter paint(this);

//  QPainter paint( this );
//  paint.drawLine(0,0,50,50);

  QFrame::paintEvent(e);

}

struct TEvent tevent={0};


#if 0

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

//bool MyWidget::event(QEvent *event){
//  printf("type: %d\n",event->type());
//  return true;
//}




void MyWidget::keyPressEvent(QKeyEvent *qkeyevent){
  RWarning("keyPressEvent should not be called.\n");

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

   update();
}

void MyWidget::keyReleaseEvent(QKeyEvent *qkeyevent){
  RWarning("keyReleaseEvent should not be called.\n");

  //  printf("key release: %d\n",qkeyevent->ascii());
  //  printf("key release: %d\n",qkeyevent->key());
  // printf("Released\n");
  //  this->keyPressEvent(qkeyevent);
}

#endif


void MyWidget::wheelEvent(QWheelEvent *qwheelevent){

  tevent.ID=TR_KEYBOARD;

  if(qwheelevent->delta()>0){
    tevent.SubID=EVENT_UPARROW;
  }else{
    tevent.SubID=EVENT_DOWNARROW;
  }

  tevent.x=qwheelevent->x();
  tevent.y=qwheelevent->y();

  for(int lokke=0;lokke<abs(qwheelevent->delta()/120);lokke++){
    EventReciever(&tevent,window);
  }

  update();
}


void MyWidget::mousePressEvent( QMouseEvent *qmouseevent){

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
  tevent.x=qmouseevent->x();
  tevent.y=qmouseevent->y();

  EventReciever(&tevent,this->window);

  update();
}

void MyWidget::mouseReleaseEvent( QMouseEvent *qmouseevent){
  if(qmouseevent->button()==Qt::LeftButton){
    tevent.ID=TR_LEFTMOUSEUP;
  }else{
    if(qmouseevent->button()==Qt::RightButton){
      tevent.ID=TR_RIGHTMOUSEUP;
    }else{
      tevent.ID=TR_MIDDLEMOUSEUP;
    }
  }
  tevent.x=qmouseevent->x();
  tevent.y=qmouseevent->y();


  EventReciever(&tevent,this->window);

  update();
}

void MyWidget::mouseMoveEvent( QMouseEvent *qmouseevent){
  tevent.ID=TR_MOUSEMOVE;
  tevent.x=qmouseevent->x();
  tevent.y=qmouseevent->y();
  EventReciever(&tevent,this->window);

  //fprintf(stderr, "mouse %d / %d\n", tevent.x, tevent.y);

  update();

}

void MyWidget::resizeEvent( QResizeEvent *qresizeevent){
  this->qpixmap->resize(this->width(), this->height());
  this->cursorpixmap->resize(this->width(), this->height());

  int old_width = this->window->width;
  int old_height = this->window->height;

  this->window->width=this->get_editor_width();
  this->window->height=this->get_editor_height();

#if 0
  printf("width: %d/%d, height: %d/%d\n",this->width(),qresizeevent->size().width(),
         this->height(),qresizeevent->size().height());
#endif

  if(this->get_editor_width() > old_width || this->get_editor_height() > old_height){
#if 1
    repaint(); // I don't know why it's not enough just calling DrawUpTrackerWindow.
#else
    // This doesn't really do anything:
    DO_GFX_BLT({
        GFX_FilledBox(window,5,
                      0,old_height-1,
                      this->window->width, this->window->height);
        DrawUpTrackerWindow(window);
                      
      });
    update();
    // Very strange!
#endif
  }else{
    DO_GFX_BLT(DrawUpTrackerWindow(window));
    update();
  }
}

void MyWidget::closeEvent(QCloseEvent *ce){
  //  ce->accept();
}
