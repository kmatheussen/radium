

#include "../common/nsmtracker.h"
#include "../api/api_gui_proc.h"

#include "Qt_Bs_edit_proc.h"


#include "KeyboardFocusFrame.hpp"


radium::KeyboardFocusFrame* g_keyboard_focus_frames[(int)radium::KeyboardFocusFrameType::NUM_TYPES] = {};

namespace{
  
  struct Obj : public QObject {

    QWidget *_edit_gui = NULL;
    
    void handle_mouse_pressed_widget(QWidget *w){
      if (w==NULL)
        return;

      if (_edit_gui==NULL)
        _edit_gui = API_get_editGui();
      
      if (w==g_keyboard_focus_frames[(int)radium::KeyboardFocusFrameType::EDITOR] || w==_edit_gui || w==BS_get())
        FOCUSFRAMES_set_focus(radium::KeyboardFocusFrameType::EDITOR, true);
      
      else if (w==g_keyboard_focus_frames[(int)radium::KeyboardFocusFrameType::MIXER])
        FOCUSFRAMES_set_focus(radium::KeyboardFocusFrameType::MIXER, true);
      
      else if (w==g_keyboard_focus_frames[(int)radium::KeyboardFocusFrameType::SEQUENCER])
        FOCUSFRAMES_set_focus(radium::KeyboardFocusFrameType::SEQUENCER, true);

      
      else
        handle_mouse_pressed_widget(w->parentWidget());
    }

    bool eventFilter(QObject *obj, QEvent *event) override {
      if (event->type()==QEvent::GraphicsSceneMousePress || event->type()==QEvent::MouseButtonPress)
        handle_mouse_pressed_widget(dynamic_cast<QWidget*>(obj));
      
      return false;
    }
  };
  
  static Obj g_obj;
}


void FOCUSFRAMES_init(void){
  static bool has_installed_event_filter=false;
  
  if (has_installed_event_filter==false){
    has_installed_event_filter=true;
    qApp->installEventFilter(&g_obj);
  }
}


radium::KeyboardFocusFrameType g_curr_focus_type = radium::KeyboardFocusFrameType::EDITOR;
radium::KeyboardFocusFrameType g_prev_focus_type = radium::KeyboardFocusFrameType::SEQUENCER;
  
void FOCUSFRAMES_set_focus(radium::KeyboardFocusFrameType type, bool has_focus){
  R_ASSERT_RETURN_IF_FALSE(g_keyboard_focus_frames[(int)type] != NULL);
  
  g_keyboard_focus_frames[(int)type]->set_focus(has_focus);
}

static bool set_if_visible(radium::KeyboardFocusFrameType type){
  auto *frame = g_keyboard_focus_frames[(int)type];
  if (frame==NULL)
    return false;
  
  if (frame->isVisible()){
    FOCUSFRAMES_set_focus(type, true);
    return true;
  }

  return false;
}

void FOCUSFRAMES_set_focus_best_guess(void){
  if (set_if_visible(g_curr_focus_type))
    return;

  if (set_if_visible(g_curr_focus_type))
    return;

  for(int type = 0 ; type < (int)radium::KeyboardFocusFrameType::NUM_TYPES ; type++){
    if (set_if_visible((radium::KeyboardFocusFrameType)type))
      return;
  }
}
  
bool FOCUSFRAMES_has_focus(radium::KeyboardFocusFrameType type){
  auto *frame = g_keyboard_focus_frames[(int)type];
  if (frame==NULL)
    return false;
  
  return frame->has_focus();
}

