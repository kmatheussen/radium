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

#include <qpainter.h>
#include <QUrl>

#ifdef USE_QT4
//Added by qt3to4:
#include <QCustomEvent>
#include <QCloseEvent>
#include <QWheelEvent>
#include <QPaintEvent>
#include <QResizeEvent>
#include <QPixmap>
#include <QMouseEvent>
#include <QPointF>
#include <QKeyEvent>
#endif

#include "../common/list_proc.h"
#include "../common/blts_proc.h"
#include "../common/disk_load_proc.h"
#include "../common/OS_string_proc.h"
#include "../common/eventreciever_proc.h"
#include "../common/PEQ_clock_proc.h"
#include "../common/gfx_proc.h"
#include "../midi/midi_i_input_proc.h"

#include "../common/player_proc.h"
#include "../common/gfx_op_queue_proc.h"
#include "../common/visual_proc.h"
#include "../common/wblocks_proc.h"
#include "../common/OS_Bs_edit_proc.h"
#include "../common/cursor_updown_proc.h"

#include "../embedded_scheme/scheme_proc.h"

#include "../OpenGL/Render_proc.h"
#include "../OpenGL/Widget_proc.h"

#include "../api/api_proc.h"


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


DEFINE_ATOMIC(bool, atomic_must_redraw) = false;
DEFINE_ATOMIC(bool, atomic_must_redraw_editor) = false;
DEFINE_ATOMIC(struct Patch*, atomic_must_redraw_instrument) = NULL;
DEFINE_ATOMIC(bool, atomic_must_calculate_coordinates) = false;

static void transfer_atomic_must_redraws(struct Tracker_Windows *window)
{
  bool a_must_redraw = ATOMIC_GET(atomic_must_redraw);
  if (a_must_redraw){
    ATOMIC_SET(atomic_must_redraw, false);
    window->must_redraw = true;
  }
  
  bool a_must_redraw_editor = ATOMIC_GET(atomic_must_redraw_editor);
  if (a_must_redraw_editor){
    ATOMIC_SET(atomic_must_redraw_editor, false);
    window->must_redraw_editor = true;
  }
  
  bool a_must_calculate = ATOMIC_GET(atomic_must_calculate_coordinates);
  if (a_must_calculate){
    ATOMIC_SET(atomic_must_calculate_coordinates, false);
    window->must_calculate_coordinates = true;
  }
}


#if USE_QT_VISUAL
void EditorWidget::paintEvent( QPaintEvent *e ){
  if(ATOMIC_GET(is_starting_up)==true)
    return;

  //printf("    UPDATE called\n");
  
  transfer_atomic_must_redraws(window);
  
  window->redraw_has_been_scheduled=false;

  if(window->must_redraw==true){
    //printf("** Drawing up everything!\n");
    window->must_redraw=false;
    GFX_clear_op_queue(this->window);
    DO_GFX(DrawUpTrackerWindow(this->window));
    window->must_redraw_editor=true;
  }

#if 1
  if (window->must_redraw_editor==true){
    window->must_redraw_editor=false;
    //printf("calling gl_create\n");
    GL_create(window, window->wblock);
  }
#endif
  
  //printf("height: %d, width: %d\n",e->rect().height(),e->rect().width());
  //printf("update editor\n");
  //GL_create(window, window->wblock);


  //printf("paintEvent called. queue size: %d\n",GFX_get_op_queue_size(this->window));
  //printf("paintevent. width: %d, height: %d\n",this->width(),this->height());

  if(GFX_get_op_queue_size(this->window) > 0){
    QPainter paint(this);
    this->painter = &paint;
    //this->painter->setFont(this->font);

    {
      GFX_play_op_queue(this->window);
    }

    this->painter = NULL;
  }
}

//void EditorWidget::showEvent ( QShowEvent * event )
#endif

void EditorWidget::updateEditor(){
  if(ATOMIC_GET(is_starting_up)==true)
    return;

  {
    struct Patch *patch = ATOMIC_GET(atomic_must_redraw_instrument);
    if (patch!=NULL){
      ATOMIC_SET(atomic_must_redraw_instrument, NULL);
      GFX_update_instrument_patch_gui(patch);
    }
  }
  

  transfer_atomic_must_redraws(window);

  //if (this->window->must_redraw) printf(" MUST REDRAW == TRUE (has_beenscheduled: %d)\n",this->window->redraw_has_been_scheduled);

  if (this->window->must_calculate_coordinates==true){
    window->must_calculate_coordinates=false;
    UpdateWBlockCoordinates(this->window, this->window->wblock);
  }

#if 1
  if (this->window->must_redraw_editor==true){
    this->window->must_redraw_editor=false;
    //printf("a3\n");
    GL_create(this->window, this->window->wblock);
  }
#else
  // This version is probably not faster, since update() will call paintEvent immedately (?).
  // It also complicates things since the call to GL_create in paintEvent() should be removed to avoid calling GL_create twice. But it cannot always be removed.
  if (this->window->must_redraw_editor==true || this->window->must_redraw==true){
    this->window->must_redraw_editor=false;
    //printf("a3\n");
    GL_create(this->window, this->window->wblock);
  }
#endif

#if 0
  if (is_playing()){
    if(this->window->must_redraw==true) {
      GL_create(this->window, this->window->wblock);
      this->window->must_redraw=false;
    }
  } else
#endif
  if (this->window->redraw_has_been_scheduled==false) {    
    if(this->window->must_redraw==true || GFX_get_op_queue_size(this->window)>0) {
      this->window->redraw_has_been_scheduled=true;
      update();
    }
  }
}

  /*
void GFX_ScheduleRedraw(void){
  if(is_starting_up==true)
    return;
  
  if(root!=NULL && root->song!=NULL && root->song->tracker_windows!=NULL) {
    struct Tracker_Windows *window=root->song->tracker_windows;

    bool is_main_thread = THREADING_is_main_thread(); 
    
    if (is_main_thread)
      GL_create(window, window->wblock);
    
    window->must_redraw = true;

    if (is_main_thread){
      EditorWidget *editor=(EditorWidget *)window->os_visual.widget;
      editor->update();
    } else {
      window->must_redraw_editor = true;
    }
  }
}

void GFX_ScheduleRedrawEditor(void){
  if(is_starting_up==true)
    return;
  
  if(root!=NULL && root->song!=NULL && root->song->tracker_windows!=NULL) {
    struct Tracker_Windows *window=root->song->tracker_windows;

    bool is_main_thread = THREADING_is_main_thread(); 
    
    if (is_main_thread)
      GL_create(window, window->wblock);
    
    window->must_redraw = true;

    if (is_main_thread){
      EditorWidget *editor=(EditorWidget *)window->os_visual.widget;
      editor->update();
    } else {
      window->must_redraw_editor = true;
    }
  }
}
  */

struct TEvent tevent={}; // c++ way of zero-initialization without getting missing-field-initializers warning.

void EditorWidget::wheelEvent(QWheelEvent *qwheelevent){
  if(ATOMIC_GET(is_starting_up)==true)
      return;

    struct Tracker_Windows *window=static_cast<struct Tracker_Windows*>(root->song->tracker_windows);

    int num_lines = R_ABS(qwheelevent->delta()/120);    
            
    DO_GFX(
           {
             /*
             if(qwheelevent->delta()<0)
               ScrollEditorDown(window,num_lines * getScrollMultiplication());
             else
               ScrollEditorUp(window,num_lines * getScrollMultiplication());
             */

             if (qwheelevent->modifiers() & Qt::ControlModifier) {
               if (qwheelevent->delta() > 0)
                 zoom(1,window->l.num);
               else
                 zoom(-1,window->l.num);

             } else if (qwheelevent->modifiers() & Qt::ShiftModifier) {

               tevent.ID=TR_KEYBOARD;
               if(qwheelevent->delta()<0)
                 tevent.SubID=EVENT_LEFTARROW;
               else
                 tevent.SubID=EVENT_RIGHTARROW;
               
               for(int i=0;i<num_lines;i++)
                 EventReciever(&tevent,window);

             } else {

               tevent.ID=TR_KEYBOARD;
               if(qwheelevent->delta()<0)
                 tevent.SubID=EVENT_DOWNARROW;
               else
                 tevent.SubID=EVENT_UPARROW;
               
               for(int i=0;i<num_lines;i++)
                 EventReciever(&tevent,window);
             }
             
           });

#if USE_QT_VISUAL
    updateEditor();
#endif
}


#if 0

void EditorWidget::keyPressEvent(QKeyEvent *qkeyevent){
  if(ATOMIC_GET(is_starting_up)==true)
    return;

  //printf("ascii    : %d\n",qkeyevent->toUtf8().constData());
  printf("key      : %d\n",qkeyevent->key());
  //printf("key press: %d,%d\n",qkeyevent->state(),Qt2SubId[max(0,qkeyevent->key()-0x41)]);
  printf("text     : -%s-\n",(const char *)qkeyevent->text().toUtf8().constData());
  printf("Auto     : %d\n\n",qkeyevent->isAutoRepeat());
  // return;
}

void EditorWidget::keyReleaseEvent(QKeyEvent *qkeyevent){
  printf("keyreleaseEvent\n");
}
#endif


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

  if(ATOMIC_GET(is_starting_up)==true)
    return;

  printf("ascii    : %d\n",qkeyevent->toUtf8().constData());
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

  //  printf("key release: %d\n",qkeyevent->toUtf8().constData());
  //  printf("key release: %d\n",qkeyevent->key());
  // printf("Released\n");
  //  this->keyPressEvent(qkeyevent);
}

#endif

#if USE_QT_VISUAL

static int currentButton = 0;

static int getMouseButtonEventID( QMouseEvent *qmouseevent){
  if(qmouseevent->button()==Qt::LeftButton)
    return TR_LEFTMOUSEDOWN;
  else if(qmouseevent->button()==Qt::RightButton)
    return TR_RIGHTMOUSEDOWN;
  else if(qmouseevent->button()==Qt::MiddleButton)
    return TR_MIDDLEMOUSEDOWN;
  else
    return 0;
}

void EditorWidget::mousePressEvent( QMouseEvent *qmouseevent){
  if(ATOMIC_GET(is_starting_up)==true)
    return;

  tevent.ID = getMouseButtonEventID(qmouseevent);
  tevent.x  = qmouseevent->posF().x();//-XOFFSET;
  tevent.y  = qmouseevent->posF().y();//-YOFFSET;

  currentButton = tevent.ID;

  //printf("> Got mouse press %d %d\n",tevent.x,tevent.y);

  if (SCHEME_mousepress(currentButton, qmouseevent->posF().x(), qmouseevent->posF().y())==false) {

    EventReciever(&tevent,this->window);

  }

  release_keyboard_focus();
  
  // GL_lock is needed when using intel gfx driver to avoid crash caused by opening two opengl contexts simultaneously from two threads.
  GL_lock();{
    setFocus();
  }GL_unlock();

  updateEditor();
}


void EditorWidget::mouseMoveEvent( QMouseEvent *qmouseevent){
  if(ATOMIC_GET(is_starting_up)==true)
    return;

  tevent.ID=TR_MOUSEMOVE;
  tevent.x=qmouseevent->posF().x();//-XOFFSET;
  tevent.y=qmouseevent->posF().y();//-YOFFSET;

  //Qt::ButtonState buttonstate=qmouseevent->state();
  //printf("************** buttonstate: %d, %d, %d\n",getMouseButtonEventID(qmouseevent),buttonstate,tevent.keyswitch);

  if (SCHEME_mousemove(currentButton, qmouseevent->posF().x(), qmouseevent->posF().y())==false)
    EventReciever(&tevent,this->window);

  //fprintf(stderr, "mouse %d / %d\n", tevent.x, tevent.y);
//  printf("----Got mouse move %d %d %f %f\n",tevent.x,tevent.y,qmouseevent->posF().x(),qmouseevent->posF().y());

  updateEditor();
}


void EditorWidget::mouseReleaseEvent( QMouseEvent *qmouseevent){
  if(ATOMIC_GET(is_starting_up)==true)
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
  tevent.x=qmouseevent->posF().x();//-XOFFSET;
  tevent.y=qmouseevent->posF().y();//-YOFFSET;

  //printf("< Got mouse release %d %d\n",tevent.x,tevent.y);
  if (SCHEME_mouserelease(currentButton, qmouseevent->posF().x(), qmouseevent->posF().y())==false)
    EventReciever(&tevent,this->window);

  currentButton = 0;

  updateEditor();
}

#endif // USE_QT_VISUAL


#if USE_GTK_VISUAL
void EditorWidget::resizeEvent( QResizeEvent *qresizeevent){ // Only GTK VISUAL!
  if(ATOMIC_GET(is_starting_up)==true)
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
  this->init_buffers();

  this->window->width=qresizeevent->size().width(); //this->get_editor_width();
  this->window->height=qresizeevent->size().height(); //this->get_editor_height();

  if(ATOMIC_GET(is_starting_up)==true)
    return;

  UpdateWBlockCoordinates(window, window->wblock);

#if 0
  printf("width: %d/%d, height: %d/%d\n",this->width(),qresizeevent->size().width(),
         this->height(),qresizeevent->size().height());
#endif

#if 1
  DO_GFX(DrawUpTrackerWindow(window));
  updateEditor();
#else
  update();
#endif

  UpdateWBlockCoordinates(window, window->wblock);

#if USE_OPENGL
  printf("********* height: %d\n",qresizeevent->size().height());
  position_gl_widget(window);
#endif
}
#endif // USE_QT_VISUAL


void EditorWidget::closeEvent(QCloseEvent *ce){
  printf("Close event\n");
  //  ce->accept();
}
