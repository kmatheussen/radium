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
#include "../common/playerclass.h"
#include "../common/eventreciever_proc.h"
#include "../common/player_proc.h"
#include "../audio/Mixer_proc.h"

#include "W_Keyboard_proc.h"

extern struct Root *root;
extern PlayerClass *pc;

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

  keymap[VK_APPS] = EVENT_MENU;
  keymap[VK_VOLUME_MUTE] = EVENT_MUTE;
  keymap[VK_VOLUME_DOWN] = EVENT_VOLUME_DOWN;
  keymap[VK_VOLUME_UP] = EVENT_VOLUME_UP;
  keymap[VK_MEDIA_STOP] = EVENT_STOP;
  keymap[VK_MEDIA_PLAY_PAUSE] = EVENT_PLAY;


  // arrows
  keymap[VK_LEFT]  = EVENT_LEFTARROW;
  keymap[VK_UP]    = EVENT_UPARROW;
  keymap[VK_RIGHT] = EVENT_RIGHTARROW;
  keymap[VK_DOWN]  = EVENT_DOWNARROW;

  // keypad
  // ... is handled in the function get_keypad_subID
}

#if 0
// Didn't work. GetKeyNameText returned something else on XP and windows 7
static int get_keypad_subID(MSG *msg){
  char temp[500];
  GetKeyNameText(msg->lParam,temp,500);

  printf("Keyname: \"%s\"\n",temp);
  fflush(stdout);

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
#endif

static int get_keypad_subID(MSG *msg){
  if(0x1000000 & msg->lParam){ // i.e. insert/home/pageup/delete/end/pagedown, an arrow key, KP_DIV or KP_ENTER. (strange flag)
    if(msg->wParam==VK_RETURN)
      return EVENT_KP_ENTER;
    if(msg->wParam==VK_DIVIDE)
      return EVENT_KP_DIV;
    return EVENT_NO;
  }

  switch(msg->wParam){
  case VK_MULTIPLY:
    return EVENT_KP_MUL;
  case 0x6d:
    return EVENT_KP_SUB;
  case 0x24:
    return EVENT_KP_7;
  case 0x26:
    return EVENT_KP_8;
  case 0x21:
    return EVENT_KP_9;
  case 0x25:
    return EVENT_KP_4;
  case 0x0c:
    return EVENT_KP_5;
  case 0x27:
    return EVENT_KP_6;
  case 0x23:
    return EVENT_KP_1;
  case 0x28:
    return EVENT_KP_2;
  case 0x22:
    return EVENT_KP_3;
  case 0x2d:
    return EVENT_KP_0;
  case 0x2e:
    return EVENT_KP_DOT;
  case 0x6b:
    return EVENT_KP_ADD;
  }
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

static bool g_bWindowActive = true;
static HHOOK g_hKeyboardHook = NULL;

// http://msdn.microsoft.com/en-us/library/windows/desktop/ee416808(v=vs.85).aspx
LRESULT CALLBACK LowLevelKeyboardProc( int nCode, WPARAM wParam, LPARAM lParam )
{
    if (nCode < 0 || nCode != HC_ACTION )  // do not process message 
        return CallNextHookEx( g_hKeyboardHook, nCode, wParam, lParam); 
 
    KBDLLHOOKSTRUCT* p = (KBDLLHOOKSTRUCT*)lParam;

    if(wParam==WM_KEYDOWN || wParam==WM_KEYUP){
      if(p->vkCode==VK_LWIN || p->vkCode==VK_RWIN){

        if(p->vkCode==VK_LWIN)
          left_windows_down = wParam==WM_KEYDOWN?true:false;
        else
          right_windows_down = wParam==WM_KEYDOWN?true:false;

        if(g_bWindowActive)
          return 1;
      }
    }
 
    return CallNextHookEx( g_hKeyboardHook, nCode, wParam, lParam );
}

void W_KeyboardHandlerShutDown(void){
  if(g_hKeyboardHook!=NULL)
    UnhookWindowsHookEx(g_hKeyboardHook);
}

extern int num_users_of_keyboard;

bool W_KeyboardFilter(MSG *msg){
  static bool initialized=false;
  static int last_pressed_key = -1;
  static int64_t last_pressed_key_time = -1;
  static int last_pressed_keyswitch = -1;

  if(initialized==false){
    init_keymap();
    
    g_hKeyboardHook = SetWindowsHookEx( WH_KEYBOARD_LL,  LowLevelKeyboardProc, GetModuleHandle(NULL), 0 );

    initialized=true;
  }

  if(root==NULL || root->song==NULL || root->song->tracker_windows==NULL)
    return false;

  struct TEvent tevent;
  struct Tracker_Windows *window=root->song->tracker_windows;
#if 0
  static int num=0;
  if(msg->message!=WM_TIMER && msg->message!=0x84 && msg->message!=WM_MOUSEFIRST && msg->message!=WM_MOUSEMOVE && msg->message!=WM_SETCURSOR){
    char *temp="";
    printf("Got something. Message: 0x%x. wParam: 0x%x. lParam: 0x%x. Num: %d. Name: \"%s\", Left shift? 0x%x\n",(int)msg->message,(int)msg->wParam,(int)msg->lParam,num++,temp,(int)GetKeyState(VK_LSHIFT));
    fflush(stdout);
  }
#endif
  switch(msg->message){
    case WM_HOTKEY:
      //printf("Got HotKey\n");
      //fflush(stdout);
      return true;
    case WM_ACTIVATEAPP:
      g_bWindowActive = msg->wParam ? true : false;
      //printf("Got Activate app. wParam: %d\n",(int)msg->wParam);
      //fflush(stdout);
      break;
    case WM_KEYDOWN:
    case WM_SYSKEYDOWN:
      if(num_users_of_keyboard>0)
        return false;

      tevent.ID=TR_KEYBOARD;

      tevent.SubID=get_keyboard_subID(msg);
      last_pressed_key = tevent.SubID;

      if(last_pressed_key_time==-1)
        last_pressed_key_time = MIXER_get_time();

      tevent.keyswitch=get_keyswitch();
      last_pressed_keyswitch = tevent.keyswitch;

      EventReciever(&tevent,window);

      return true;

    case WM_KEYUP: 
    case WM_SYSKEYUP:
      if(msg->wParam==VK_RWIN){
        right_windows_down = false;
        return false;
      }
      if(msg->wParam==VK_LWIN){
        left_windows_down = false;
        return false;
      }

      if(num_users_of_keyboard>0)
        return false;

      int keynum = get_keyboard_subID(msg);

      tevent.ID=TR_KEYBOARDUP;
      tevent.SubID=keynum;
      tevent.keyswitch=get_keyswitch();

      int64_t time_now = MIXER_get_time();

      //printf("keynum: %d, last_pressed: %d, ALT_R: %d, time_now: %d, last_time: %d, diff: %d\n",keynum,last_pressed_key,EVENT_ALT_R,(int)time_now,(int)last_pressed_key_time,(int)(time_now-last_pressed_key_time));
      //fflush(stdout);

      if( (time_now-last_pressed_key_time) < pc->pfreq/4){ // i.e. only play if holding the key less than 0.25 seconds.
        if(keynum==last_pressed_key && keynum==0 && tevent.keyswitch==0 && last_pressed_keyswitch==EVENT_RIGHTALT)
          PlayBlockFromStart(window,true); // true == do_loop
        
        if(keynum==last_pressed_key && keynum==0 && tevent.keyswitch==0 && last_pressed_keyswitch==EVENT_RIGHTSHIFT)
          PlayBlockFromStart(window,true); // true == do_loop
      }

      last_pressed_key_time=-1;

      EventReciever(&tevent,window);
      return true;

    default:
      break;
  }

  return false;
}

#endif // FOR_WINDOWS

