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

#ifdef FOR_WINDOWS

#include <windows.h>

#include "../common/nsmtracker.h"
#include "../common/eventreciever_proc.h"

#include "W_Keyboard_proc.h"

extern struct Root *root;

static bool left_windows_down = false;
static bool right_windows_down = false;

static uint32_t get_keyswitch(void){
  uint32_t keyswitch=0;
  if(GetKeyState(VK_RCONTROL)&0x8000)
    keyswitch |= EVENT_RIGHTCTRL;
  if(GetKeyState(VK_LCONTROL)&0x8000)
    keyswitch |= EVENT_LEFTCTRL;

  if(GetKeyState(VK_LSHIFT)&0x8000)
    keyswitch |= EVENT_LEFTSHIFT;
  if(GetKeyState(VK_RSHIFT)&0x8000)
    keyswitch |= EVENT_RIGHTSHIFT;

  if(GetKeyState(VK_LMENU)&0x8000)
    keyswitch |= EVENT_LEFTALT;
  if(GetKeyState(VK_RMENU)&0x8000)
    keyswitch |= EVENT_RIGHTALT;

  if(left_windows_down)
    keyswitch |= EVENT_LEFTEXTRA1;
  if(right_windows_down)
    keyswitch |= EVENT_RIGHTEXTRA1;

  return keyswitch;
}

static bool keymap_initialized=false;
static int keymap[0x100] = {EVENT_NO};

static void init_keymap(void){
  // alpha
  keymap[0x41] = EVENT_A;
  keymap[0x42] = EVENT_B;
  keymap[0x43] = EVENT_C;
  keymap[0x44] = EVENT_D;
  keymap[0x45] = EVENT_E;
  keymap[0x46] = EVENT_F;
  keymap[0x47] = EVENT_G;
  keymap[0x48] = EVENT_H;
  keymap[0x49] = EVENT_I;
  keymap[0x4a] = EVENT_J;
  keymap[0x4b] = EVENT_K;
  keymap[0x4c] = EVENT_L;
  keymap[0x4d] = EVENT_M;
  keymap[0x4e] = EVENT_N;
  keymap[0x4f] = EVENT_O;
  keymap[0x50] = EVENT_P;
  keymap[0x51] = EVENT_Q;
  keymap[0x52] = EVENT_R;
  keymap[0x53] = EVENT_S;
  keymap[0x54] = EVENT_T;
  keymap[0x55] = EVENT_U;
  keymap[0x56] = EVENT_V;
  keymap[0x57] = EVENT_W;
  keymap[0x58] = EVENT_X;
  keymap[0x59] = EVENT_Y;
  keymap[0x5a] = EVENT_Z;

  // row 1
  keymap[VK_ESCAPE] = EVENT_ESC;
  keymap[VK_F1] = EVENT_F1;
  keymap[VK_F2] = EVENT_F2;
  keymap[VK_F3] = EVENT_F3;
  keymap[VK_F4] = EVENT_F4;
  keymap[VK_F5] = EVENT_F5;
  keymap[VK_F6] = EVENT_F6;
  keymap[VK_F7] = EVENT_F7;
  keymap[VK_F8] = EVENT_F8;
  keymap[VK_F9] = EVENT_F9;
  keymap[VK_F10] = EVENT_F10;
  keymap[VK_F11] = EVENT_F11;
  keymap[VK_F12] = EVENT_F12;

  // row 2
  keymap[VK_OEM_3] = EVENT_1L1;
  keymap[0x31] = EVENT_1;
  keymap[0x32] = EVENT_2;
  keymap[0x33] = EVENT_3;
  keymap[0x34] = EVENT_4;
  keymap[0x35] = EVENT_5;
  keymap[0x36] = EVENT_6;
  keymap[0x37] = EVENT_7;
  keymap[0x38] = EVENT_8;
  keymap[0x39] = EVENT_9;
  keymap[0x30] = EVENT_0;
  keymap[VK_OEM_MINUS] = EVENT_0R1;
  keymap[VK_OEM_PLUS]  = EVENT_0R2;
  keymap[VK_BACK]      = EVENT_BACKSPACE;

  // row 3
  keymap[VK_TAB]    = EVENT_TAB;
  keymap[VK_OEM_4]  = EVENT_PR1;
  keymap[VK_OEM_6]  = EVENT_PR2;
  keymap[VK_RETURN] = EVENT_RETURN;

  // row 4
  keymap[VK_OEM_1] = EVENT_LR1;
  keymap[VK_OEM_7] = EVENT_LR2;
  keymap[VK_OEM_5] = EVENT_LR3;

  // row 5
  keymap[VK_OEM_102]    = EVENT_ZL1;
  keymap[VK_OEM_COMMA]  = EVENT_MR1;
  keymap[VK_OEM_PERIOD] = EVENT_MR2;
  keymap[VK_OEM_2]      = EVENT_MR3;

  // row 6
  keymap[VK_SPACE] = EVENT_SPACE;

  // insert/del/etc.
  keymap[VK_INSERT] = EVENT_INSERT;
  keymap[VK_HOME] = EVENT_HOME;
  keymap[VK_PRIOR] = EVENT_PAGE_UP;
  keymap[VK_NEXT] = EVENT_PAGE_DOWN;
  keymap[VK_DELETE] = EVENT_DEL;
  keymap[VK_END] = EVENT_END;

  // arrows
  keymap[VK_LEFT]  = EVENT_LEFTARROW;
  keymap[VK_UP]    = EVENT_UPARROW;
  keymap[VK_RIGHT] = EVENT_RIGHTARROW;
  keymap[VK_DOWN]  = EVENT_DOWNARROW;

  // keypad
  // ... is handled in the function get_keypad_subID

  keymap_initialized=true;
}

static int get_keypad_subID(MSG *msg){
  char temp[500];
  GetKeyNameText(msg->lParam,temp,500);

  if(strlen(temp)<4 || temp[0]!='K' || temp[1]!='P' || temp[2]!='_')
    return EVENT_NO;

  if(!strcmp(temp,"KP_Insert"))
    return EVENT_KP_0;
  if(!strcmp(temp,"KP_Delete"))
    return EVENT_KP_DOT;
  if(!strcmp(temp,"KP_Enter"))
    return EVENT_KP_ENTER;

  if(!strcmp(temp,"KP_End"))
    return EVENT_KP_1;
  if(!strcmp(temp,"KP_Down"))
    return EVENT_KP_2;
  if(!strcmp(temp,"KP_Next"))
    return EVENT_KP_3;

  if(!strcmp(temp,"KP_Left"))
    return EVENT_KP_4;
  if(!strcmp(temp,"KP_Begin"))
    return EVENT_KP_5;
  if(!strcmp(temp,"KP_Right"))
    return EVENT_KP_6;

  if(!strcmp(temp,"KP_Home"))
    return EVENT_KP_7;
  if(!strcmp(temp,"KP_Up"))
    return EVENT_KP_8;
  if(!strcmp(temp,"KP_Prior"))
    return EVENT_KP_9;

  if(!strcmp(temp,"KP_Add"))
    return EVENT_KP_ADD;
  if(!strcmp(temp,"KP_Subtract"))
    return EVENT_KP_SUB;
  if(!strcmp(temp,"KP_Multiply"))
    return EVENT_KP_MUL;
  if(!strcmp(temp,"KP_Divide"))
    return EVENT_KP_DIV;

  return EVENT_NO;
}

static int get_keyboard_subID(MSG *msg){
  if(msg->wParam >= 0x100)
    return EVENT_NO;

  int subID = get_keypad_subID(msg);
  if(subID!=EVENT_NO)
    return subID;

  return keymap[msg->wParam];
}


bool W_KeyboardFilter(MSG *msg){
  if(keymap_initialized==false)
    init_keymap();

  if(root==NULL || root->song==NULL || root->song->tracker_windows==NULL)
    return false;

  struct TEvent tevent;
  struct Tracker_Windows *window=root->song->tracker_windows;
  static int num=0;
  //char *temp="";
  //printf("Got something. Message: 0x%x. wParam: 0x%x. lParam: 0x%x. Num: %d. Name: \"%s\", Left shift? 0x%x\n",(int)msg->message,(int)msg->wParam,(int)msg->lParam,num++,temp,(int)GetKeyState(VK_LSHIFT));
  //fflush(stdout);
  switch(msg->message){
    case WM_KILLFOCUS:
    case WM_SETFOCUS:
    case WM_ACTIVATE:
      right_windows_down = false;
      left_windows_down = false;
      break;

    case WM_KEYDOWN:
    case WM_SYSKEYDOWN:
      if(msg->wParam==VK_RWIN)
        right_windows_down = true;
      if(msg->wParam==VK_LWIN)
        left_windows_down = true;
      
      tevent.ID=TR_KEYBOARD;
      tevent.SubID=get_keyboard_subID(msg);
      tevent.keyswitch=get_keyswitch();
      EventReciever(&tevent,window);

#if 0
      char temp[500];
      GetKeyNameText(msg->lParam,temp,500);
      printf("Got something. Message: 0x%x. wParam: 0x%x. lParam: 0x%x. Num: %d. Name: \"%s\", Left shift? 0x%x. not keypad? 0x%x\n",(int)msg->message,(int)msg->wParam,(int)msg->lParam,num++,temp,(int)(GetKeyState(VK_RCONTROL)&0x8000),(int)(msg->lParam&0x1000000));
      //printf("Got something down.... %d/0x%x. temp: \"%s\" %d\n",(int)msg->wParam,(int)msg->wParam,temp,num++);
      fflush(stdout);
#endif
      return true;

    case WM_KEYUP: 
    case WM_SYSKEYUP:
      if(msg->wParam==VK_RWIN)
        right_windows_down = false;
      if(msg->wParam==VK_LWIN)
        left_windows_down = false;

      tevent.ID=TR_KEYBOARDUP;
      tevent.SubID=get_keyboard_subID(msg);
      tevent.keyswitch=get_keyswitch();
      EventReciever(&tevent,window);
      return true;

    default:
      break;
  }

  return false;
}

#endif // FOR_WINDOWS

