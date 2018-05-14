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
//#include <QCustomEvent>
#include <QCloseEvent>
#include <QWheelEvent>
#include <QPaintEvent>
#include <QResizeEvent>
#include <QPixmap>
#include <QMouseEvent>
#include <QPointF>
#include <QKeyEvent>
#endif

#include "Qt_instruments_proc.h"

#include "../common/list_proc.h"
#include "../common/blts_proc.h"
#include "../common/disk_load_proc.h"
#include "../common/OS_string_proc.h"
#include "../common/eventreciever_proc.h"
//#include "../common/PEQ_clock_proc.h"
#include "../common/gfx_proc.h"
#include "../common/gfx_wtrackheaders_proc.h"
#include "../midi/midi_i_input_proc.h"

#include "../common/player_proc.h"
#include "../common/gfx_op_queue_proc.h"
#include "../common/visual_proc.h"
#include "../common/wblocks_proc.h"
#include "../common/OS_Bs_edit_proc.h"
#include "../common/cursor_updown_proc.h"
#include "../common/player_pause_proc.h"
#include "../common/seqtrack_proc.h"

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
  bool a_must_redraw = ATOMIC_COMPARE_AND_SET_BOOL(atomic_must_redraw, true, false);
  if (a_must_redraw)
    window->must_redraw = true;
  
  bool a_must_redraw_editor = ATOMIC_COMPARE_AND_SET_BOOL(atomic_must_redraw_editor, true, false);
  if (a_must_redraw_editor)
    window->must_redraw_editor = true;
  
  bool a_must_calculate = ATOMIC_COMPARE_AND_SET_BOOL(atomic_must_calculate_coordinates, true, false);
  if (a_must_calculate)
    window->must_calculate_coordinates = true;
}


bool g_allowed_to_grow_queue = false;

#if USE_QT_VISUAL
void EditorWidget::paintEvent( QPaintEvent *e ){
  TRACK_PAINT();

  RETURN_IF_DATA_IS_INACCESSIBLE();

  if(g_is_starting_up==true)
    return;
  
  //static int n=0;  printf("** Drawing up everything! %d\n",n++);
  
  GFX_clear_op_queue(this->window);

  g_allowed_to_grow_queue = true;
  DO_GFX(DrawUpTrackerWindow(this->window));
  g_allowed_to_grow_queue = false;

  if(GFX_get_op_queue_size(this->window) > 0){
    QPainter paint(this);

    this->painter = &paint;
    //this->painter->setFont(this->font);
    
    {
      GFX_play_op_queue(this->window);
      //GFX_clear_op_queue(this->window);
    }
    
    this->painter = NULL;
  }
}

//void EditorWidget::showEvent ( QShowEvent * event )
#endif

void EditorWidget::updateEditor(){
  if(g_is_starting_up==true)
    return;

  {
    struct Patch *patch = ATOMIC_GET(atomic_must_redraw_instrument);
    if (patch!=NULL){
      ATOMIC_SET(atomic_must_redraw_instrument, NULL);
      GFX_update_instrument_widget(patch);//GFX_update_instrument_patch_gui(patch);
    }
  }
  

  transfer_atomic_must_redraws(window);
#if 0 //!defined(RELEASE)
  {
    int queue_size = GFX_get_op_queue_size(this->window);
    if (queue_size > 0 || this->window->must_calculate_coordinates==true || this->window->must_redraw==true || this->window->must_redraw_editor)
      printf("..Updating. Queue: %d. Update coordinates: %d. Redraw editor: %d. Redraw: %d\n",
             queue_size, this->window->must_calculate_coordinates, this->window->must_redraw_editor, this->window->must_redraw
             );
  }
#endif
  
  if (GFX_get_op_queue_size(this->window)>0)
    this->window->must_redraw = true;
    
  if (this->window->must_calculate_coordinates==true){
    this->window->must_redraw = true;
    this->window->must_calculate_coordinates=false;
  }

#if 0
  if (this->window->must_redraw || this->window->must_redraw_editor)
    printf("   Must_redraw: %d. Must redraw editor: %d\n", this->window->must_redraw, this->window->must_redraw_editor);
#endif
  
  if (this->window->must_redraw) {
    UpdateTrackerWindowCoordinates(window);
    UpdateWBlockCoordinates(this->window, this->window->wblock);
    GFX_UpdateUpperLeft(window, window->wblock);
    UpdateAllPianoRollHeaders(window, window->wblock);
    SEQUENCER_update(SEQUPDATE_TIME);
    
    update();

    this->window->must_redraw_editor=true;
    this->window->must_redraw=false;
  }

  if (this->window->must_redraw_editor==true){
    GL_create(this->window);
    if (!is_playing())
      SEQUENCER_update(SEQUPDATE_TIME);
    this->window->must_redraw_editor=false;
  }
}

  /*
void GFX_ScheduleRedraw(void){
  if(g_is_starting_up==true)
    return;
  
  if(root!=NULL && root->song!=NULL && root->song->tracker_windows!=NULL) {
    struct Tracker_Windows *window=root->song->tracker_windows;

    bool is_main_thread = THREADING_is_main_thread(); 
    
    if (is_main_thread)
      GL_create(window);
    
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
  if(g_is_starting_up==true)
    return;
  
  if(root!=NULL && root->song!=NULL && root->song->tracker_windows!=NULL) {
    struct Tracker_Windows *window=root->song->tracker_windows;

    bool is_main_thread = THREADING_is_main_thread(); 
    
    if (is_main_thread)
      GL_create(window);
    
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
    if(g_is_starting_up==true)
      return;
 
    if (MIXER_is_saving())
      return;

       
    //struct Tracker_Windows *window=static_cast<struct Tracker_Windows*>(root->song->tracker_windows);

    int num_lines = R_ABS(qwheelevent->delta()/120);

    //printf("   Got wheel event %d\n",qwheelevent->delta()/120);
    
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
#if 0
             } else if (qwheelevent->modifiers() & Qt::ShiftModifier) {

               tevent.ID=TR_KEYBOARD;
               if(qwheelevent->delta()<0)
                 tevent.SubID=EVENT_LEFTARROW;
               else
                 tevent.SubID=EVENT_RIGHTARROW;
               
               for(int i=0;i<num_lines;i++)
                 EventReciever(&tevent,window);
#endif
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

    qwheelevent->accept();
}


#if 0

void EditorWidget::keyPressEvent(QKeyEvent *qkeyevent){
  if(g_is_starting_up==true)
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

  if(g_is_starting_up==true)
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

static int g_currentButton = 0;

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

void EditorWidget::mousePressEvent( QMouseEvent *qmouseevent) {
  if(g_is_starting_up==true)
    return;

  tevent.ID = getMouseButtonEventID(qmouseevent);
#if USE_QT5
  tevent.x  = qmouseevent->localPos().x();
  tevent.y  = qmouseevent->localPos().y();
#else
  tevent.x  = qmouseevent->posF().x();
  tevent.y  = qmouseevent->posF().y();
#endif
  g_currentButton = tevent.ID;

  //printf("> Got mouse press %d %d\n",tevent.x,tevent.y);

  if (SCHEME_mousepress(g_currentButton, tevent.x, tevent.y)==false) {

    EventReciever(&tevent,this->window);

  }

  R_ASSERT(g_pausing_level==0);

  release_keyboard_focus();
  
  // GL_lock is needed when using intel gfx driver to avoid crash caused by opening two opengl contexts simultaneously from two threads.
  GL_lock();{
    setFocus();
  }GL_unlock();

  updateEditor();

  qmouseevent->accept();
}


void EditorWidget::mouseMoveEvent( QMouseEvent *qmouseevent) {
  if(g_is_starting_up==true)
    return;

  tevent.ID=TR_MOUSEMOVE;
#if USE_QT5
  tevent.x  = qmouseevent->localPos().x();
  tevent.y  = qmouseevent->localPos().y();
#else
  tevent.x  = qmouseevent->posF().x();
  tevent.y  = qmouseevent->posF().y();
#endif

  //Qt::ButtonState buttonstate=qmouseevent->state();
  //printf("************** buttonstate: %d, %d, %d\n",getMouseButtonEventID(qmouseevent),buttonstate,tevent.keyswitch);

  if (SCHEME_mousemove(g_currentButton, tevent.x, tevent.y)==false)
    EventReciever(&tevent,this->window);

  R_ASSERT(g_pausing_level==0);
  
  //fprintf(stderr, "mouse %d / %d\n", tevent.x, tevent.y);
//  printf("----Got mouse move %d %d %f %f\n",tevent.x,tevent.y,qmouseevent->posF().x(),qmouseevent->posF().y());

  updateEditor();

  qmouseevent->accept();
}


void EditorWidget::mouseReleaseEvent( QMouseEvent *qmouseevent) {
  if(g_is_starting_up==true)
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
#if USE_QT5
  tevent.x  = qmouseevent->localPos().x();
  tevent.y  = qmouseevent->localPos().y();
#else
  tevent.x  = qmouseevent->posF().x();
  tevent.y  = qmouseevent->posF().y();
#endif

  //printf("< Got mouse release %d %d\n",tevent.x,tevent.y);
  if (SCHEME_mouserelease(g_currentButton, tevent.x, tevent.y)==false)
    EventReciever(&tevent,this->window);

  R_ASSERT(g_pausing_level==0);
  
  g_currentButton = 0;

  updateEditor();

  qmouseevent->accept();
}

#endif // USE_QT_VISUAL


#if USE_GTK_VISUAL
void EditorWidget::resizeEvent( QResizeEvent *qresizeevent){ // Only GTK VISUAL!
  if(g_is_starting_up==true)
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
#if !USE_OPENGL
  this->init_buffers();
#endif

  if (this->window != NULL){
    this->window->width=qresizeevent->size().width(); //this->get_editor_width();
    this->window->height=qresizeevent->size().height(); //this->get_editor_height();
  }
  
  if(g_is_starting_up==true)
    return;

  //UpdateWBlockCoordinates(window, window->wblock);

#if 0
  printf("width: %d/%d, height: %d/%d\n",this->width(),qresizeevent->size().width(),
         this->height(),qresizeevent->size().height());
#endif

  window->must_redraw = true;
  updateEditor();

  //UpdateWBlockCoordinates(window, window->wblock);

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
