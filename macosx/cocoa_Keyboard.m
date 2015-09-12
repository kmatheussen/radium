/* Copyright 2012 Kjetil S. Matheussen

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


#ifdef FOR_MACOSX

#import <Cocoa/Cocoa.h>
#import <Carbon/Carbon.h>


#undef EVENT_H

#include "../common/nsmtracker.h"
#include "../common/eventreciever_proc.h"

#include "../common/OS_system_proc.h"


extern struct TEvent tevent;
extern struct Root *root;

  
int OS_SYSTEM_get_event_type(void *void_event){
  NSEvent *event = (NSEvent *)void_event;
  NSEventType type = [event type];
  if(type==NSKeyDown)
    return TR_KEYBOARD;
  else if (type==NSKeyUp)
    return TR_KEYBOARDUP;
  else
    return -1;
}
                             
static enum SubIds cocoa_get_modifier_event(void *void_event){
  NSEvent *event = (NSEvent *)void_event;
  
  switch ([event keyCode]) {
  case 54: // Right Command
    return EVENT_EXTRA_R;
  case 55: // Left Command
    return EVENT_EXTRA_L;
  //case 57: // Capslock
  case 56: // Left Shift
    return EVENT_SHIFT_L;
  case 60: // Right Shift
    return EVENT_SHIFT_R;
  case 58: // Left Alt
    return EVENT_ALT_L;
  case 61: // Right Alt
    return EVENT_ALT_R;
  case 59: // Left Ctrl
    return EVENT_CTRL_L;
  case 62: // Right Ctrl
    return EVENT_CTRL_R;
    //case 63: // Function
  default:
    return EVENT_NO;
  }
}


static bool isModifierKey(NSEvent *event){
  return cocoa_get_modifier_event(event) != EVENT_NO;
}

// Copied from http://opensource.apple.com/source/WebCore/WebCore-955.66/platform/mac/KeyEventMac.mm?txt
static inline bool isKeyUpEvent(NSEvent *event)
{
    if ([event type] != NSFlagsChanged)
        return [event type] == NSKeyUp;
    // FIXME: This logic fails if the user presses both Shift keys at once, for example:
    // we treat releasing one of them as keyDown.
    switch ([event keyCode]) {
        case 54: // Right Command
        case 55: // Left Command
            return ([event modifierFlags] & NSCommandKeyMask) == 0;

        case 57: // Capslock
            return ([event modifierFlags] & NSAlphaShiftKeyMask) == 0;

        case 56: // Left Shift
        case 60: // Right Shift
            return ([event modifierFlags] & NSShiftKeyMask) == 0;

        case 58: // Left Alt
        case 61: // Right Alt
            return ([event modifierFlags] & NSAlternateKeyMask) == 0;

        case 59: // Left Ctrl
        case 62: // Right Ctrl
            return ([event modifierFlags] & NSControlKeyMask) == 0;

        case 63: // Function
            return ([event modifierFlags] & NSFunctionKeyMask) == 0;
    }
    return false;
}

static bool modifiers[64];

static bool set_modifier(NSEvent *event){
  if([event type]==NSFlagsChanged){
    if(isModifierKey(event)){
      int keyCode = [event keyCode];
      if(isKeyUpEvent(event))
        modifiers[keyCode] = false;
      else
        modifiers[keyCode] = true;

      return true;
    }
  }
  return false;
}

static void clear_modifiers(void){
  int i;
  for(i=0;i<64;i++)
    modifiers[i]=false;
}

void OS_SYSTEM_ResetKeysUpDowns(void){
  clear_modifiers();
}

static uint32_t get_keyswitch(void){
  uint32_t keyswitch=0;

  if(modifiers[62])
    keyswitch |= EVENT_RIGHTCTRL;

  if(modifiers[59])
    keyswitch |= EVENT_LEFTCTRL;

  if(modifiers[56])
    keyswitch |= EVENT_LEFTSHIFT;

  if(modifiers[60])
    keyswitch |= EVENT_RIGHTSHIFT;

  if(modifiers[58])
    keyswitch |= EVENT_LEFTALT;

  if(modifiers[61])
    keyswitch |= EVENT_RIGHTALT;

  if(modifiers[55])
    keyswitch |= EVENT_LEFTEXTRA1;

  if(modifiers[54])
    keyswitch |= EVENT_RIGHTEXTRA1;

  return keyswitch;
}

static int keymap[0x100] = {EVENT_NO};

// This only works for qwerty-keyboards (i.e. what scancode is used for in windows and linux). To get EVENT_A (for instance): http://stackoverflow.com/questions/8263618/convert-virtual-key-code-to-unicode-string
// Mac vk overview: http://stackoverflow.com/questions/3202629/where-can-i-find-a-list-of-mac-virtual-key-codes
static void init_keymap(void){
  // alpha
  keymap[kVK_ANSI_A] = EVENT_A; // TODO: This is actually an EVENT_QWERTY_A event, not an EVENT_A event.
  keymap[kVK_ANSI_B] = EVENT_B;
  keymap[kVK_ANSI_C] = EVENT_C;
  keymap[kVK_ANSI_D] = EVENT_D;
  keymap[kVK_ANSI_E] = EVENT_E;
  keymap[kVK_ANSI_F] = EVENT_F;
  keymap[kVK_ANSI_G] = EVENT_G;
  keymap[kVK_ANSI_H] = EVENT_H;
  keymap[kVK_ANSI_I] = EVENT_I;
  keymap[kVK_ANSI_J] = EVENT_J;
  keymap[kVK_ANSI_K] = EVENT_K;
  keymap[kVK_ANSI_L] = EVENT_L;
  keymap[kVK_ANSI_M] = EVENT_M;
  keymap[kVK_ANSI_N] = EVENT_N;
  keymap[kVK_ANSI_O] = EVENT_O;
  keymap[kVK_ANSI_P] = EVENT_P;
  keymap[kVK_ANSI_Q] = EVENT_Q;
  keymap[kVK_ANSI_R] = EVENT_R;
  keymap[kVK_ANSI_S] = EVENT_S;
  keymap[kVK_ANSI_T] = EVENT_T;
  keymap[kVK_ANSI_U] = EVENT_U;
  keymap[kVK_ANSI_V] = EVENT_V;
  keymap[kVK_ANSI_W] = EVENT_W;
  keymap[kVK_ANSI_X] = EVENT_X;
  keymap[kVK_ANSI_Y] = EVENT_Y;
  keymap[kVK_ANSI_Z] = EVENT_Z;

  // num
  keymap[kVK_ANSI_0] = EVENT_0;
  keymap[kVK_ANSI_1] = EVENT_1;
  keymap[kVK_ANSI_2] = EVENT_2;
  keymap[kVK_ANSI_3] = EVENT_3;
  keymap[kVK_ANSI_4] = EVENT_4;
  keymap[kVK_ANSI_5] = EVENT_5;
  keymap[kVK_ANSI_6] = EVENT_6;
  keymap[kVK_ANSI_7] = EVENT_7;
  keymap[kVK_ANSI_8] = EVENT_8;
  keymap[kVK_ANSI_9] = EVENT_9;

  // row 1
  keymap[kVK_Escape] = EVENT_ESC;
  keymap[kVK_F1] = EVENT_F1;
  keymap[kVK_F2] = EVENT_F2;
  keymap[kVK_F3] = EVENT_F3;
  keymap[kVK_F4] = EVENT_F4;
  keymap[kVK_F5] = EVENT_F5;
  keymap[kVK_F6] = EVENT_F6;
  keymap[kVK_F7] = EVENT_F7;
  keymap[kVK_F8] = EVENT_F8;
  keymap[kVK_F9] = EVENT_F9;
  keymap[kVK_F10] = EVENT_F10;
  keymap[kVK_F11] = EVENT_F11;
  keymap[kVK_F12] = EVENT_F12;

  // row 2
  keymap[kVK_ISO_Section] = EVENT_1L1;
  keymap[kVK_ANSI_Minus] = EVENT_0R1;
  keymap[kVK_ANSI_Equal] = EVENT_0R2;
  keymap[kVK_Delete] = EVENT_BACKSPACE;

  // row 3
  keymap[kVK_Tab] = EVENT_TAB;
  keymap[kVK_ANSI_LeftBracket] = EVENT_PR1;
  keymap[kVK_ANSI_RightBracket] = EVENT_PR2;
  keymap[kVK_Return] = EVENT_RETURN;

  // row 4
  keymap[kVK_ANSI_Semicolon] = EVENT_LR1;
  keymap[kVK_ANSI_Quote] = EVENT_LR2;
  keymap[kVK_ANSI_Backslash] = EVENT_LR3;

  // row 5
  keymap[kVK_ANSI_Grave]    = EVENT_ZL1;
  keymap[kVK_ANSI_Comma]  = EVENT_MR1;
  keymap[kVK_ANSI_Period] = EVENT_MR2;
  keymap[kVK_ANSI_Slash]      = EVENT_MR3;

  // row 6
  keymap[kVK_Space] = EVENT_SPACE;

  // insert/del/etc.
  keymap[kVK_Help] = EVENT_INSERT;
  keymap[kVK_Home] = EVENT_HOME;
  keymap[kVK_PageUp] = EVENT_PAGE_UP;
  keymap[kVK_PageDown] = EVENT_PAGE_DOWN;
  keymap[kVK_ForwardDelete] = EVENT_DEL;
  keymap[kVK_End] = EVENT_END;

  // arrows
  keymap[kVK_LeftArrow]  = EVENT_LEFTARROW;
  keymap[kVK_UpArrow]    = EVENT_UPARROW;
  keymap[kVK_RightArrow] = EVENT_RIGHTARROW;
  keymap[kVK_DownArrow]  = EVENT_DOWNARROW;

  // keypad
  keymap[kVK_ANSI_Keypad0] = EVENT_KP_0;
  keymap[kVK_ANSI_Keypad1] = EVENT_KP_1;
  keymap[kVK_ANSI_Keypad2] = EVENT_KP_2;
  keymap[kVK_ANSI_Keypad3] = EVENT_KP_3;
  keymap[kVK_ANSI_Keypad4] = EVENT_KP_4;
  keymap[kVK_ANSI_Keypad5] = EVENT_KP_5;
  keymap[kVK_ANSI_Keypad6] = EVENT_KP_6;
  keymap[kVK_ANSI_Keypad7] = EVENT_KP_7;
  keymap[kVK_ANSI_Keypad8] = EVENT_KP_8;
  keymap[kVK_ANSI_Keypad9] = EVENT_KP_9;
  keymap[kVK_ANSI_KeypadDecimal] = EVENT_KP_DOT;
  keymap[kVK_ANSI_KeypadEnter] = EVENT_KP_ENTER;
  keymap[kVK_ANSI_KeypadPlus] = EVENT_KP_ADD;
  keymap[kVK_ANSI_KeypadMinus] = EVENT_KP_SUB;
  keymap[kVK_ANSI_KeypadMultiply] = EVENT_KP_MUL;
  keymap[kVK_ANSI_KeypadDivide] = EVENT_KP_DIV;
}

static int get_keyboard_subID(int keycode){
  if(keycode >= 0x100)
    return EVENT_NO;
  else
    return keymap[keycode];
}

int OS_SYSTEM_get_keynum(void *focused_widget, void *void_event){
  NSEvent *event = (NSEvent *)void_event;

  int keycode = [event keyCode];
  int keynum = get_keyboard_subID(keycode);

  if (keynum==-1)
    return cocoa_get_modifier_event(event);
  else
    return keynum;
}

void OS_SYSTEM_init_keyboard(void){
  init_keymap();
  clear_modifiers();
}


void OS_SYSTEM_EventPreHandler(void *void_event){
  NSEvent *event = (NSEvent *)void_event;
  NSEventType type = [event type];

  OS_SYSTEM_init_keyboard();

  static void *oldHotKeyMode = NULL;
  if(type==NSAppKitDefined || type==NSSystemDefined || type==NSApplicationDefined){ // These three events are received when losing focus. Haven't found a better time to clear modifiers.
    clear_modifiers();
    if(oldHotKeyMode!=NULL){
      PushSymbolicHotKeyMode(kHIHotKeyModeAllEnabled);
      oldHotKeyMode = NULL;
    }
    return;
  }else{
    if(oldHotKeyMode==NULL)
      oldHotKeyMode = PushSymbolicHotKeyMode(kHIHotKeyModeAllDisabled); 
  }

  set_modifier(event);
}

extern int num_users_of_keyboard;

// Seems like NSEvent has a flag called 'isARepeat' which can be used to detect whehter the event was generated by autorepeat.
// https://developer.apple.com/library/mac/documentation/Cocoa/Reference/ApplicationKit/Classes/NSEvent_Class/#//apple_ref/occ/instp/NSEvent/ARepeat

bool OS_SYSTEM_KeyboardFilter(void *focused_widget, void *void_event){
  NSEvent *event = (NSEvent *)void_event;
  NSEventType type = [event type];

#if 0
  {
    if(type==10||type==11||type==12){
      int keycode = [event keyCode];
      uint32_t keyswitch=get_keyswitch();
      //printf("keycode: %x, switch: %x\n",keycode,(int)keyswitch);
    }
  }
#endif



  if(type!=NSKeyUp && type!=NSKeyDown)
    return false;

  if(root==NULL || root->song==NULL || root->song->tracker_windows==NULL)
    return false;

  if(num_users_of_keyboard>0)
    return false;

  {
    struct Tracker_Windows *window=root->song->tracker_windows;

    int keycode = [event keyCode];

    if(type==NSKeyDown)
      tevent.ID=TR_KEYBOARD;
    else
      tevent.ID=TR_KEYBOARDUP;

    tevent.SubID=get_keyboard_subID(keycode);
    tevent.keyswitch=get_keyswitch();

    EventReciever(&tevent,window);
  }

  return true;
}

#endif
