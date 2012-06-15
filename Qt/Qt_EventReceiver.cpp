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

#include "../common/resizewindow_proc.h"
#include "../common/blts_proc.h"
#include "../common/eventreciever_proc.h"
#include "../common/PEQ_clock_proc.h"

#include "../common/player_proc.h"

extern QApplication *qapplication;
extern "C" void WBLOCKS_bltBlt(struct Tracker_Windows *window);

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

#include "X11/Xlib.h"


void MyWidget::customEvent(QCustomEvent *e){
  Display *dpy = QPaintDevice::x11AppDisplay();
  XSync(dpy, false);
  {
    P2MUpdateSongPosCallBack();
    UpdateClock(this->window);
  }
  // Flushing and syncing X to avoid lag when playing fast.
  XFlush(dpy);
}

void MyWidget::paintEvent( QPaintEvent *e ){

//  setBackgroundColor( this->colors[0] );		/* white background */

  //Resize_resized(this->window,this->width()-100,this->height()-30,false);
  Resize_resized(this->window,this->width()-100,this->height(),false);
  //    UpdateTrackerWindow(this->window);

    //    QPainter paint(this);

    //    bitBlt(this,0,0,this->qpixmap);
//  QPainter paint( this );
//  paint.drawLine(0,0,50,50);

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
  printf("ascii    : %d\n",qkeyevent->ascii());
  printf("key      : %d\n",qkeyevent->key());
  printf("key press: %d,%d\n",qkeyevent->state(),Qt2SubId[max(0,qkeyevent->key()-0x41)]);
  printf("text     : -%s-\n",(const char *)qkeyevent->text());
  printf("Auto     : %d\n\n",qkeyevent->isAutoRepeat());
  // return;

  tevent.ID=TR_KEYBOARD;

  ButtonState buttonstate=qkeyevent->state();

  tevent.keyswitch=0;

  if(buttonstate&ShiftButton) tevent.keyswitch=EVENT_LEFTSHIFT;
  if(buttonstate&ControlButton) tevent.keyswitch|=EVENT_LEFTCTRL;
  if(buttonstate&AltButton) tevent.keyswitch|=EVENT_RIGHTEXTRA1;

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

  //  WBLOCKS_bltBlt(this->window);

  //this->repaint();
  //  bitBlt(this,0,0,this->qpixmap);
}

void MyWidget::keyReleaseEvent(QKeyEvent *qkeyevent){
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

}


void MyWidget::mousePressEvent( QMouseEvent *qmouseevent){

  if(qmouseevent->button()==LeftButton){
    tevent.ID=TR_LEFTMOUSEDOWN;
  }else{
    if(qmouseevent->button()==RightButton){
      tevent.ID=TR_RIGHTMOUSEDOWN;
      //      exit(2);
    }else{
      tevent.ID=TR_MIDDLEMOUSEDOWN;
    }
  }
  tevent.x=qmouseevent->x();
  tevent.y=qmouseevent->y();

  EventReciever(&tevent,this->window);

  //  WBLOCKS_bltBlt(this->window);
  //bitBlt(this,0,0,this->qpixmap);
}

void MyWidget::mouseReleaseEvent( QMouseEvent *qmouseevent){
  if(qmouseevent->button()==LeftButton){
    tevent.ID=TR_LEFTMOUSEUP;
  }else{
    if(qmouseevent->button()==RightButton){
      tevent.ID=TR_RIGHTMOUSEUP;
    }else{
      tevent.ID=TR_MIDDLEMOUSEUP;
    }
  }
  tevent.x=qmouseevent->x();
  tevent.y=qmouseevent->y();


  EventReciever(&tevent,this->window);

  //  WBLOCKS_bltBlt(this->window);
  //bitBlt(this,0,0,this->qpixmap);
}

void MyWidget::mouseMoveEvent( QMouseEvent *qmouseevent){
  tevent.ID=TR_MOUSEMOVE;
  tevent.x=qmouseevent->x();
  tevent.y=qmouseevent->y();
  EventReciever(&tevent,this->window);

  //  WBLOCKS_bltBlt(this->window);
  //  bitBlt(this,0,0,this->qpixmap);
}

void MyWidget::resizeEvent( QResizeEvent *qresizeevent){
  //  this->window->width=this->width()-100;
  //  this->window->height=this->height();

  //  this->qpixmap=new QPixmap(this->width(),this->height(),-1,QPixmap::BestOptim);
  this->qpixmap=new QPixmap(this->width(),this->height(),-1,QPixmap::BestOptim);
  this->qpixmap->fill( this->colors[0] );		/* grey background */

  this->cursorpixmap=new QPixmap(this->width(),this->height());
  this->cursorpixmap->fill( this->colors[7] );		/* the xored background color for the cursor.*/

  //Resize_resized(this->window,this->width()-100,this->height()-30,false);
  Resize_resized(this->window,this->width()-100,this->height(),false);

  QPainter paint( this );
  paint.setPen(this->colors[6]);
  paint.drawLine(this->width()-99,0,this->width()-99,this->height());

}

void MyWidget::closeEvent(QCloseEvent *ce){
  //  ce->accept();
}
