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

#include "../common/nsmtracker.h"

#include "EditorWidget.h"
#include "Qt_instruments_proc.h"

#include "../common/list_proc.h"
#include "../common/disk_load_proc.h"
#include "../common/OS_string_proc.h"
#include "../common/eventreciever_proc.h"
//#include "../common/PEQ_clock_proc.h"
#include "../midi/midi_i_input_proc.h"

#include "../common/player_proc.h"
#include "../common/visual_proc.h"
#include "../common/wblocks_proc.h"
#include "../common/OS_Bs_edit_proc.h"
#include "../common/cursor_updown_proc.h"
#include "../common/player_pause_proc.h"
#include "../common/sequencer_proc.h"

#include "../embedded_scheme/scheme_proc.h"
#include "../embedded_scheme/s7extra_proc.h"

#include "../OpenGL/Render_proc.h"
#include "../OpenGL/Widget_proc.h"

#include "../api/api_proc.h"
#include "../api/api_gui_proc.h"


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

bool g_rt_do_rerendering = true;

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


//void EditorWidget::showEvent ( QShowEvent * event )
#endif

static void update_seqtracks_with_current_editor_block(void){
  if(root==NULL || root->song==NULL || root->song->tracker_windows==NULL || root->song->tracker_windows->wblock==NULL || root->song->tracker_windows->wblock->block==NULL){
    R_ASSERT_NON_RELEASE(false);
    return;
  }

  SEQUENCER_update_seqblocks_holding_editor_block(root->song->tracker_windows->wblock->block);
}

void EditorWidget::updateEditor(void) const {
  if(g_is_starting_up==true)
    return;

  {
    struct Patch *patch = ATOMIC_GET(atomic_must_redraw_instrument);
    if (patch!=NULL && patch == PATCH_get_current()){
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
  
  if (this->window->must_calculate_coordinates==true){
    this->window->must_redraw = true;
    this->window->must_calculate_coordinates=false;
  }

#if 0
  if (this->window->must_redraw || this->window->must_redraw_editor)
    printf("   Must_redraw: %d. Must redraw editor: %d\n", this->window->must_redraw, this->window->must_redraw_editor);
#endif
  
  if (this->window->must_redraw) {
    /*
    int x2_before = getReltempoSliderX2();
    int skew_before = this->window->wblock->skew_x;
    */

    UpdateWBlockCoordinates(this->window, this->window->wblock);
    GFX_UpdateUpperLeft(window, window->wblock);
    //UpdateAllPianoRollHeaders(window, window->wblock);

    /*
    if (x2_before != getReltempoSliderX2() || skew_before != this->window->wblock->skew_x)
    S7CALL2(void_void,"FROM_C-reconfigure-editor-lower-part-gui!");
    else
      bottom_widget->update();
    */

    S7CALL2(void_void,"FROM_C-reconfigure-editor-track-headers-gui!");
    S7CALL2(void_void,"FROM_C-reconfigure-editor-lower-part-gui!");
    
    update_seqtracks_with_current_editor_block();

    //update();
    
    this->window->must_redraw_editor=true;
    this->window->must_redraw=false;
  }

  if (this->window->must_redraw_editor==true){
    GL_create(this->window);
    if (!is_playing())
      update_seqtracks_with_current_editor_block();
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
        
      } else {

        tevent.keyswitch &= ~EVENT_MOUSE_SEQUENCER2;
        tevent.keyswitch &= ~EVENT_MOUSE_MIXER2;
        tevent.keyswitch &= ~EVENT_MOUSE_MIXERSTRIPS2;
        tevent.keyswitch &= ~EVENT_FOCUS_SEQUENCER2;
        tevent.keyswitch &= ~EVENT_FOCUS_MIXER2;
        tevent.keyswitch &= ~EVENT_FOCUS_MIXERSTRIPS2;
        
        tevent.keyswitch |= EVENT_MOUSE_EDITOR2 | EVENT_FOCUS_EDITOR2;        
        
        tevent.ID=TR_KEYBOARD;
        if(qwheelevent->delta()<0)
          tevent.SubID=EVENT_DOWNARROW;
        else
          tevent.SubID=EVENT_UPARROW;
        
        for(int i=0;i<num_lines;i++)
          EventReciever(&tevent,window);
        
      }
      
    }

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

static int getMouseButtonEventID(Qt::MouseButton button){
  if(button==Qt::LeftButton)
    return TR_LEFTMOUSEDOWN;
  else if(button==Qt::RightButton)
    return TR_RIGHTMOUSEDOWN;
  else if(button==Qt::MiddleButton)
    return TR_MIDDLEMOUSEDOWN;
  else
    return 0;
}

/*
static int getMouseButtonEventID( QMouseEvent *qmouseevent){
  return getMouseButtonEventID(qmouseevent->button());
}
*/

static bool g_is_mousing_editor = false;

#if 1
void EditorWidget::handle_mouse_press(Qt::MouseButton button, float x, float y) const{
  g_is_mousing_editor = true;
  
  if(g_is_starting_up==true)
    return;

  //FOCUSFRAMES_set_focus(radium::KeyboardFocusFrameType::EDITOR, true);
  
  tevent.ID = getMouseButtonEventID(button);
  tevent.x = x;
  tevent.y = y;
  
  g_currentButton = tevent.ID;

  //printf("> Got mouse press %d %d\n",tevent.x,tevent.y);

  if (SCHEME_mousepress(g_currentButton, tevent.x, tevent.y)==false) {

    EventReciever(&tevent,this->window);

  }

  R_ASSERT(g_pausing_level==0);

  release_keyboard_focus();
  
  // GL_lock is needed when using intel gfx driver to avoid crash caused by opening two opengl contexts simultaneously from two threads.
  /*
  GL_lock();{
    setFocus();
  }GL_unlock();
  */
  
  updateEditor();
}
#endif

#if FOR_WINDOWS
void MouseMoveRelative(float x, float y, float dx, float dy) {
#if 1
  
  g_mouse_dx += dx;
  g_mouse_dy += dy;
  return;

#else
  
  if (g_is_mousing_editor==false)
    return;
  
  if(g_is_starting_up==true)
    return;

  if (g_wants_delta_movements==false)
    return;
    
  
  tevent.ID=TR_MOUSEMOVE;
  tevent.x  = x;
  tevent.y  = y;


  //Qt::ButtonState buttonstate=qmouseevent->state();
  //printf("************** buttonstate: %d, %d, %d\n",getMouseButtonEventID(qmouseevent),buttonstate,tevent.keyswitch);

  if (SCHEME_mousemove(g_currentButton, tevent.x, tevent.y)==false)
    EventReciever(&tevent,root->song->tracker_windows);

  R_ASSERT(g_pausing_level==0);
  
  //fprintf(stderr, "mouse %d / %d\n", tevent.x, tevent.y);
//  printf("----Got mouse move %d %d %f %f\n",tevent.x,tevent.y,qmouseevent->posF().x(),qmouseevent->posF().y());

  g_editor->updateEditor();
#endif
}
#endif


//void EditorWidget::fix_mouseMoveEvent( QMouseEvent *qmouseevent) {
void EditorWidget::handle_mouse_move(Qt::MouseButton button, float x, float y) const {
  if(g_is_starting_up==true)
    return;

  tevent.ID=TR_MOUSEMOVE;
  tevent.x  = x;
  tevent.y  = y;

  if (true || g_wants_delta_movements==false)
    if (SCHEME_mousemove(g_currentButton, tevent.x, tevent.y)==false)
      EventReciever(&tevent,root->song->tracker_windows);

  R_ASSERT(g_pausing_level==0);

  updateEditor();
}


void EditorWidget::handle_mouse_release(Qt::MouseButton button, float x, float y) const {
  g_is_mousing_editor = false;

  if(g_is_starting_up==true)
    return;

  if(button==Qt::LeftButton){
    tevent.ID=TR_LEFTMOUSEUP;
  }else{
    if(button==Qt::RightButton){
      tevent.ID=TR_RIGHTMOUSEUP;
    }else{
      tevent.ID=TR_MIDDLEMOUSEUP;
    }
  }

  tevent.x = x;
  tevent.y = y;

  //printf("< Got mouse release %d %d\n",tevent.x,tevent.y);
  if (SCHEME_mouserelease(g_currentButton, tevent.x, tevent.y)==false)
    EventReciever(&tevent,this->window);

  R_ASSERT(g_pausing_level==0);
  
  g_currentButton = 0;

  updateEditor();
}

#endif // USE_QT_VISUAL


#if USE_GTK_VISUAL
void EditorWidget::resizeEvent( QResizeEvent *qresizeevent){ // Only GTK VISUAL!
  radium::ScopedResizeEventTracker resize_event_tracker;
  
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
#if 0
void EditorWidget::resizeEvent( QResizeEvent *qresizeevent){
  radium::ScopedResizeEventTracker resize_event_tracker;
  
  // If we don't do this, there could be graphical garbage in the tracker headers while resizing.
  window->must_redraw = true;
  updateEditor();
}
#endif

#endif // USE_QT_VISUAL


#if 0
void EditorWidget::closeEvent(QCloseEvent *ce){
  printf("Close event\n");
  //  ce->accept();
}
#endif
