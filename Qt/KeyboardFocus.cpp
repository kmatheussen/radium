


#include "../common/nsmtracker.h"

#include "KeyboardFocusFrame.hpp"


radium::KeyboardFocusFrame* g_keyboard_focus_frames[(int)radium::KeyboardFocusFrameType::NUM_TYPES] = {};

void FOCUSFRAMES_set_focus(radium::KeyboardFocusFrameType type, bool has_focus){
  R_ASSERT_RETURN_IF_FALSE(g_keyboard_focus_frames[(int)type] != NULL);
  
  g_keyboard_focus_frames[(int)type]->set_focus(has_focus);
}

bool FOCUSFRAMES_has_focus(radium::KeyboardFocusFrameType type){
  if (g_keyboard_focus_frames[(int)type]==NULL)
    return false;
  
  return g_keyboard_focus_frames[(int)type]->has_focus();
}

