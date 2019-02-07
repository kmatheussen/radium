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
#include <windowsx.h>

#ifndef HID_USAGE_PAGE_GENERIC
#define HID_USAGE_PAGE_GENERIC         ((USHORT) 0x01)
#endif
#ifndef HID_USAGE_GENERIC_MOUSE
#define HID_USAGE_GENERIC_MOUSE        ((USHORT) 0x02)
#endif

#include <math.h>

#include "../common/nsmtracker.h"
#include "../common/playerclass.h"
#include "../common/eventreciever_proc.h"
#include "../common/player_proc.h"
#include "../common/scancodes_proc.h"
#include "../common/OS_visual_input.h"
#include "../audio/Mixer_proc.h"

#include "../common/OS_system_proc.h"
#include "W_Keyboard_proc.h"


#define CHECK_BIT(var,pos) ((var) & (1<<(pos)))


extern struct TEvent tevent;

static DEFINE_ATOMIC(bool, left_windows_down) = false;
static DEFINE_ATOMIC(bool, right_windows_down) = false;

static unsigned int g_last_keyswitch;

#ifndef RUN_TEST

void OS_WINDOWS_set_window_on_top_of(void *parent_handle, void *child_handle){
  SetWindowLongPtr(child_handle, -8, (LONG_PTR)parent_handle);
}
  
void OS_WINDOWS_set_always_on_top(void *child_handle){
#if 0
  //SetWindowPos(main_handle, hwnd, 100, 100, 1000, 1000, SWP_SHOWWINDOW);
  SetWindowPos(hwnd, HWND_TOP, 100, 100, 1000, 1000, SWP_SHOWWINDOW);
#else
  HWND child_hwnd = (HWND)child_handle;
#if 0
  // Set it on top of the main window.
  HWND parent_hwnd = (HWND)OS_GFX_get_native_main_window();
  SetWindowLongPtr(child_hwnd, -8, (LONG_PTR)parent_hwnd);
#elif 1
  // Set it on top of all open toplevel windows.
  int num_toplevelwindows = OS_GFX_get_num_toplevel_windows();
  printf("   NUM_TOP: %d\n", num_toplevelwindows);
  for(int i=0;i<num_toplevelwindows;i++){
    HWND parent_hwnd = (HWND)OS_GFX_get_native_toplevel_window(i);
    printf("parent_hwnd %d: %p\n", i, parent_hwnd);
    if (parent_hwnd != NULL)
      SetWindowLongPtr(child_hwnd, -8, (LONG_PTR)parent_hwnd);
  }
#else
  // Set it on top of the mixer window, if visible. If not, on top of the main window.
  HWND parent_hwnd = (HWND)OS_GFX_get_mixer_toplevel_window_if_visible();
  printf("Mixer hwnd: %p\n", parent_hwnd);
  if (parent_hwnd==NULL)
    parent_hwnd = (HWND)OS_GFX_get_native_main_window();
  SetWindowLongPtr(child_hwnd, -8, (LONG_PTR)parent_hwnd);
#endif
#endif
}

#endif

#if 0
void OS_WINDOWS_set_on_top_of_everything(void *child_handle){
  HWND wnd=(HWND)child_handle;
  SetFocus(wnd);
  SetWindowPos(wnd, HWND_TOPMOST, 0, 0, 0, 0, SWP_NOMOVE|SWP_NOSIZE);
}
#endif


static uint32_t get_keyswitch(void){
  uint32_t keyswitch=0;

  bool is_left_ctrl = GetKeyState(VK_LCONTROL) & 0x8000;
  bool is_right_ctrl = GetKeyState(VK_RCONTROL) & 0x8000;
    
  if(is_right_ctrl)
    keyswitch |= EVENT_RIGHTCTRL;
  if(is_left_ctrl)
    keyswitch |= EVENT_LEFTCTRL;

  if(GetKeyState(VK_LSHIFT)&0x8000)
    keyswitch |= EVENT_LEFTSHIFT;
  if(GetKeyState(VK_RSHIFT)&0x8000)
    keyswitch |= EVENT_RIGHTSHIFT;

  bool is_left_alt = GetKeyState(VK_LMENU) & 0x8000;

  if (is_left_alt)
    keyswitch |= EVENT_LEFTALT;

  if(GetKeyState(VK_RMENU)&0x8000)
    keyswitch |= EVENT_RIGHTALT;

  if(ATOMIC_GET(left_windows_down))
    keyswitch |= EVENT_LEFTEXTRA1;
  if(ATOMIC_GET(right_windows_down))
    keyswitch |= EVENT_RIGHTEXTRA1;

  bool is_right_alt =    
    (GetKeyState(VK_RMENU) & 0x8000) ||
    (is_left_alt && is_left_ctrl) || // alt+ctrl is the same as altgr on windows
    (is_left_alt && is_right_ctrl) ||  // alt+ctrl is the same as altgr on windows
    keyswitch == 0xc2 // Don't quite know why. It's just the number that pops up on my windows 8.1 machine.
    ;

  // Some quick hacking. Windows detects right alt as ctrl+left alt. This should probably be programmed properly in the future though.
  if(is_right_alt) {
    keyswitch &= ~EVENT_LEFTALT;
    keyswitch &= ~EVENT_RIGHTCTRL;
    keyswitch &= ~EVENT_LEFTCTRL;
    keyswitch |= EVENT_RIGHTALT;
  }

  keyswitch = OS_SYSTEM_add_mouse_keyswitches(keyswitch);
  
  return keyswitch;
}

static int keymap[0x100] = {EVENT_NO};

// https://msdn.microsoft.com/en-us/library/windows/desktop/dd375731%28v=vs.85%29.aspx
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
  //keymap[VK_OEM_3] = EVENT_1L1;
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
  //keymap[VK_OEM_MINUS] = EVENT_0R1;
  //keymap[VK_OEM_PLUS]  = EVENT_0R2;
  keymap[VK_BACK]      = EVENT_BACKSPACE;

  // row 3
  keymap[VK_TAB]    = EVENT_TAB;
  //keymap[VK_OEM_4]  = EVENT_PR1;
  //keymap[VK_OEM_6]  = EVENT_PR2;
  keymap[VK_RETURN] = EVENT_RETURN;

  // row 4
  //keymap[VK_OEM_1] = EVENT_LR1;
  //keymap[VK_OEM_7] = EVENT_LR2;
  //keymap[VK_OEM_5] = EVENT_LR3;

  // row 5
  //keymap[VK_OEM_102]    = EVENT_ZL1;
  //keymap[VK_OEM_COMMA]  = EVENT_MR1;
  //keymap[VK_OEM_PERIOD] = EVENT_MR2;
  //keymap[VK_OEM_2]      = EVENT_MR3;

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
  /*
  keymap[VK_VOLUME_MUTE] = EVENT_MUTE;
  keymap[VK_VOLUME_DOWN] = EVENT_VOLUME_DOWN;
  keymap[VK_VOLUME_UP] = EVENT_VOLUME_UP;
  keymap[VK_MEDIA_STOP] = EVENT_STOP;
  keymap[VK_MEDIA_PLAY_PAUSE] = EVENT_PLAY;
  */

  // These are handled by WM_APPCOMMAND events instead. However, we need to eat them so Radium doesn't try to look at the scancode, which doesn't seem to be valid for these keys.
  keymap[VK_VOLUME_MUTE] = EVENT_EAT_BUT_DO_NOTHING;
  keymap[VK_VOLUME_DOWN] = EVENT_EAT_BUT_DO_NOTHING;
  keymap[VK_VOLUME_UP] = EVENT_EAT_BUT_DO_NOTHING;
  keymap[VK_MEDIA_STOP] = EVENT_EAT_BUT_DO_NOTHING;
  keymap[VK_MEDIA_PLAY_PAUSE] = EVENT_EAT_BUT_DO_NOTHING;
  keymap[VK_MEDIA_NEXT_TRACK] = EVENT_EAT_BUT_DO_NOTHING;
  keymap[VK_MEDIA_PREV_TRACK] = EVENT_EAT_BUT_DO_NOTHING;
  
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

static int get_keypad_subID2(uint32_t wParam){
  if(wParam==VK_RETURN)
    return EVENT_KP_ENTER;
  if(wParam==VK_DIVIDE)
    return EVENT_KP_DIV;

  switch(wParam){
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

int OS_SYSTEM_get_keynum2(uint32_t wParam, bool keypad_pressed){
  if(wParam >= 0x100)
    return EVENT_NO;

  if (keypad_pressed){
    int subID = get_keypad_subID2(wParam);
    if(subID!=EVENT_NO)
      return subID;
  }

  return keymap[wParam];
}

static int get_event_from_appcommand(MSG *msg){
  switch(GET_APPCOMMAND_LPARAM(msg->lParam)){
    case APPCOMMAND_MEDIA_STOP: return EVENT_STOP;
    case APPCOMMAND_MEDIA_PLAY_PAUSE: return EVENT_PLAY;
    case APPCOMMAND_MEDIA_PLAY: return EVENT_PLAY;
    case APPCOMMAND_VOLUME_UP: return EVENT_VOLUME_UP;
    case APPCOMMAND_VOLUME_DOWN: return EVENT_VOLUME_DOWN;
    case APPCOMMAND_VOLUME_MUTE: return EVENT_MUTE;
      
    default:
      return EVENT_NO;            
  }
}

int OS_SYSTEM_get_keynum(void *void_event){
  MSG *msg = (MSG*)void_event;
  
  if (msg->message==WM_APPCOMMAND){
    
    // Although these keys are registered in the normal keyboard handler as well, we need to return true now. If not the events will also be sent to other programs.
    
    return get_event_from_appcommand(msg);
  }

  if(msg->wParam >= 0x100)
    return EVENT_NO;

  int subID = get_keypad_subID(msg);
  if(subID!=EVENT_NO)
    return subID;

  return keymap[msg->wParam];
}

int OS_SYSTEM_get_qwerty_keynum2(uint32_t scancode){
  if (scancode!=0){
    int subID = get_subID_from_scancode(scancode);
    if (subID!=EVENT_NO)
      return subID;
  }

  return EVENT_NO;
}

int OS_SYSTEM_get_qwerty_keynum(void *void_event) {
  MSG *msg = (MSG*)void_event;
  
  int scancode = MapVirtualKey(msg->wParam, MAPVK_VK_TO_VSC);
  if (scancode!=0){
    int subID = get_subID_from_scancode(scancode);
    if (subID!=EVENT_NO)
      return subID;
  }

  return EVENT_NO;
}

int OS_SYSTEM_get_scancode(void *void_event) {
  MSG *msg = (MSG*)void_event;
  
  int scancode = MapVirtualKey(msg->wParam, MAPVK_VK_TO_VSC);
  return scancode;
}

static DEFINE_ATOMIC(bool, g_bWindowActive) = true;




/*************  START / Code to automatically figure out Mouse DPI (approximately at least) ******************/

static double total_distance_abs = 0;
static double total_distance_rel = 0;
static double mouse_ratio = 0.5;

typedef struct{
  double start_x, inc_dx;
} mouse_thing_t;

static void reset_mouse_thing(mouse_thing_t *mouse_thing, double x){
  mouse_thing->start_x = x;
  mouse_thing->inc_dx = 0;
}

static mouse_thing_t mouse_x = {0};
static mouse_thing_t mouse_y = {0};

static bool is_measuring = false;
static bool finished_measuring = false;

static void maybe_improve_ratio(double x, mouse_thing_t *mouse_thing){ 

  double distance_rel = fabs(mouse_thing->inc_dx);
  if (distance_rel < 1)
    return;

  double distance_abs = fabs(x-mouse_thing->start_x);

  if (distance_abs < 50) { // If moving more than 50 pixels, the mouse most likely has moved faster than it would do when editing data.
    
    total_distance_abs += distance_abs;
    total_distance_rel += distance_rel;

    mouse_ratio = total_distance_abs / total_distance_rel;

    //    printf("    New Best Ratio: %f. distance_abs: %f. distance_rel: %f. inc_dx: %f. Total_abs/rel: %f / %f\n", mouse_ratio, distance_abs, distance_rel, mouse_thing->inc_dx, total_distance_abs, total_distance_rel);

  }

  mouse_thing->inc_dx = 0;
  mouse_thing->start_x = x;

  if (total_distance_abs > 5000){
    printf("\n\n\n    ======Finished figuring out average mouse DPI. Ratio: %f. distance_abs: %f. distance_rel: %f.=============\n\n\n", mouse_ratio, total_distance_abs, total_distance_rel);
    finished_measuring = true;
  }
}

static void add_ratio_absolute_data(float x, float y){

  // When mouse pointer is set by the program, positions are probably screwed up. Wait at least 1 second before trying to measure again.
  if (g_last_time_mouse_pointer_was_moved_by_the_program>0 && (TIME_get_ms() - g_last_time_mouse_pointer_was_moved_by_the_program) < 1000) {
    //printf("   Don't want to measure right now\n");
    is_measuring = false;
    return;
  }
  
  if (x < 100 || y < 100 || x > (OS_get_main_window_width()-100) || y > (OS_get_main_window_height()-100)){
    is_measuring = false;
    return;
  }

  if (is_measuring==false){
    is_measuring = true;
    reset_mouse_thing(&mouse_x, x);
    reset_mouse_thing(&mouse_y, y);
    return;
  }

  maybe_improve_ratio(x, &mouse_x);
  maybe_improve_ratio(y, &mouse_y);

  /*
  if (x < start_x){
    printf(" ... x < start_x: %f (%f)\n", x, start_x);
    start_x = x;
    inc_dx = 0;
  }
  
  if (y < start_y){
    printf(" ... y < start_y: %f (%f)\n", y, start_y);
    start_y = y;
    inc_dy = 0;
  }
  */
}

static void add_ratio_relative_data(float dx, float dy){

  mouse_x.inc_dx += dx;
  mouse_y.inc_dx += dy;

  //printf("              dx: %f / %f. dy: %f / %f\n", dx, inc_dx, dy, inc_dy);
}


static bool g_has_delta_mouse = false;
static bool g_can_move_pointer = false;


static void wminputhandler(MSG *msg){
    UINT dwSize;

    // get size
    GetRawInputData((HRAWINPUT)msg->lParam, RID_INPUT, NULL, &dwSize, 
                    sizeof(RAWINPUTHEADER));
    
    BYTE lpb[dwSize];
    memset(lpb, 0, sizeof(BYTE)*dwSize);

    // get data
    if (GetRawInputData((HRAWINPUT)msg->lParam, RID_INPUT, lpb, &dwSize, 
                        sizeof(RAWINPUTHEADER)) != dwSize )
      OutputDebugString (TEXT("GetRawInputData does not return correct size !\n")); 
    
    RAWINPUT* raw = (RAWINPUT*)lpb;

    if (raw->header.dwType == RIM_TYPEMOUSE) 
    {
      //HRESULT hResult = StringCchPrintf(szTempOutput, STRSAFE_MAX_CCH, TEXT("Mouse: usFlags=%04x ulButtons=%04x usButtonFlags=%04x usButtonData=%04x ulRawButtons=%04x lLastX=%04x lLastY=%04x ulExtraInformation=%04x\r\n"),

      // https://msdn.microsoft.com/en-us/library/windows/desktop/ms645578(v=vs.85).aspx

      RAWMOUSE mouse = raw->data.mouse;

      static LONG last_x=0, last_y=0;
      LONG dx,dy;
      
      if (mouse.usFlags & MOUSE_MOVE_ABSOLUTE){
        dx = last_x - mouse.lLastX;
        dy = last_y - mouse.lLastY;
        last_x = mouse.lLastX;
        last_y = mouse.lLastY;
        /* 
           Commented out since measuring ratio after using the mouse for a while in absolute mode doesn't seem to work.
        g_has_delta_mouse = false;
        g_can_move_pointer = false;
        */
      } else {
        dx = mouse.lLastX;
        dy = mouse.lLastY;
        last_x += dx;
        last_y += dy;
        g_has_delta_mouse = true;
        g_can_move_pointer = true;
      }

#if 0
      char temp[1000];
      
      sprintf(temp,"Mouse: usFlags=%04x ulButtons=%04x usButtonFlags=%04x usButtonData=%04x ulRawButtons=%04x lLastX=%d lLastY=%d ulExtraInformation=%04x",
              (unsigned int)raw->data.mouse.usFlags, 
              (unsigned int)raw->data.mouse.ulButtons, 
              (unsigned int)raw->data.mouse.usButtonFlags, 
              (unsigned int)raw->data.mouse.usButtonData, 
              (unsigned int)raw->data.mouse.ulRawButtons, 
              (int)raw->data.mouse.lLastX, 
              (int)raw->data.mouse.lLastY, 
              (unsigned int)raw->data.mouse.ulExtraInformation
              );

      GFX_SetStatusBar(temp);
#endif
      
      add_ratio_relative_data(dx, dy);
      
      MouseMoveRelative(last_x, last_y, dx * mouse_ratio, dy * mouse_ratio);
      
    } 
}

bool W_HasDeltaMouse(void){
  return g_has_delta_mouse;
}

bool W_CanMovePointer(void){
  return g_can_move_pointer;
}

void W_RegisterRawInputHandler(void *hwnd){
   RAWINPUTDEVICE Rid[1];
   Rid[0].usUsagePage = HID_USAGE_PAGE_GENERIC;
   Rid[0].usUsage = HID_USAGE_GENERIC_MOUSE;
   Rid[0].dwFlags = RIDEV_INPUTSINK;
   Rid[0].hwndTarget = (HWND)hwnd;//w.effectiveWinId();
   if(!RegisterRawInputDevices(Rid, 1, sizeof(Rid[0]))){
#if !defined(RELEASE)
     R_ASSERT(false);
#endif
   }
}


/*************  END / Code to figure out Mouse DPI ******************/




void OS_SYSTEM_EventPreHandler(void *void_event){
  MSG *msg = (MSG*)void_event;

  // https://msdn.microsoft.com/en-us/library/windows/desktop/ff468922(v=vs.85).aspx
  switch(msg->message){

    case WM_INPUT:
      wminputhandler(msg);
      break;

    case WM_MOUSEMOVE:
      if (finished_measuring==false){
        float xPos = GET_X_LPARAM(msg->lParam); 
        float yPos = GET_Y_LPARAM(msg->lParam);
        //printf("    MOUSEMOVE. %f %f\n", xPos, yPos);
        add_ratio_absolute_data(xPos, yPos);
      }
      break;
 
    case WM_NCACTIVATE:
      ATOMIC_SET(g_bWindowActive, msg->wParam ? true : false);
      //printf("1. Got NC Activate. wParam: %d. Active: %p\n",(int)msg->wParam,GetForegroundWindow());
      //fflush(stdout);
      //if (msg->wParam==0)
      //call_me_if_another_window_may_have_taken_focus_but_still_need_our_key_events();
      break;
      
    case WM_ACTIVATE:
      ATOMIC_SET(g_bWindowActive, msg->wParam ? true : false);
      //printf("2. Got Activate. wParam: %d. Active: %p\n",(int)msg->wParam,GetForegroundWindow());
      //fflush(stdout);
      //if (msg->wParam==0)
      call_me_if_another_window_may_have_taken_focus_but_still_need_our_key_events(); // Need to call this one since EVENT_SYSTEM_FOREGROUND is not called when radium is sent to the foreground if the program was minimized by pressing the icon in the windows bar instead of the "_" icon in the window bar.
      break;
      
    case WM_ACTIVATEAPP:
      ATOMIC_SET(g_bWindowActive, msg->wParam ? true : false);
      //printf("3. Got Activate app. wParam: %d. Active: %p\n",(int)msg->wParam,GetForegroundWindow());
      //fflush(stdout);
      //if (msg->wParam==0)
      //call_me_if_another_window_may_have_taken_focus_but_still_need_our_key_events();
      break;

    case WM_CHILDACTIVATE:
      //printf("4. Child activate Active: %p\n",GetForegroundWindow());
      break;
    case WM_ENTERSIZEMOVE:
      //printf("5. Enter/size/move Active: %p\n",GetForegroundWindow());
      break;
      
      //default:
      //printf("5. Unknown message.                      Active: %p\n",GetForegroundWindow());
  }

  int type = OS_SYSTEM_get_event_type(void_event, true);
  if (type==TR_KEYBOARD || type==TR_KEYBOARDUP){
    g_last_keyswitch = tevent.keyswitch;
    tevent.keyswitch = get_keyswitch();
  }
}

static bool event_is_arrow(WPARAM w){
  return w==VK_DOWN || w==VK_UP || w==VK_LEFT || w==VK_RIGHT || w==VK_PRIOR || w==VK_NEXT;
}

int OS_SYSTEM_get_event_type(void *void_event, bool ignore_autorepeat){
  MSG *msg = (MSG*)void_event;
  switch(msg->message){
  case WM_APPCOMMAND:
    //printf("   EVENT_TYPE. GOT HANDLE appcommand\n");
    if (get_event_from_appcommand(msg) != EVENT_NO)
      return TR_KEYBOARD;
    else
      return -1; // Only return TR_KEYBOARD for those keys we are able to handle. Shouldn"t make a difference, but things are messy here, so we return -1 to ensure the system-event handler returns as soon as possible.
  case WM_KEYDOWN:
  case WM_SYSKEYDOWN:
    {
      if (ignore_autorepeat && !event_is_arrow(msg->wParam)){
        if (CHECK_BIT(msg->lParam, 30)) { // autorepeat. https://msdn.microsoft.com/en-us/library/windows/desktop/ms646280(v=vs.85).aspx
          return TR_AUTOREPEAT;
        }
      }
      return TR_KEYBOARD;
    }
  case WM_KEYUP: 
  case WM_SYSKEYUP:
    return TR_KEYBOARDUP;
  default:
    return -1;
  }
}


// Note that OS_SYSTEM_get_modifier is unable to return an EVENT_EXTRA_L event. Several other problems too.
int OS_SYSTEM_get_modifier(void *void_msg){
  MSG *msg = (MSG*)void_msg;

  if (msg->message==WM_APPCOMMAND){
    //printf("   OS_SYSTEM_get_modifier. GOT HANDLE appcommand\n");
    return EVENT_NO;
  }
      
  int type = OS_SYSTEM_get_event_type(void_msg, true);
  
  unsigned int keyswitch;
  if (type==TR_KEYBOARD)
    keyswitch = tevent.keyswitch;
  else if (type==TR_KEYBOARDUP)
    keyswitch = g_last_keyswitch;
  else
    return EVENT_NO;
  
  switch(msg->wParam){
  case VK_SHIFT:
    {
      if (keyswitch & EVENT_RIGHTSHIFT)
        return EVENT_SHIFT_R;
      if (keyswitch & EVENT_LEFTSHIFT)
        return EVENT_SHIFT_L;
    }
    break;
  case VK_CONTROL:
    {
      if (keyswitch & EVENT_RIGHTCTRL)
        return EVENT_CTRL_R;
      if (keyswitch & EVENT_LEFTCTRL)
        return EVENT_CTRL_L;
    }
    break;
  case VK_MENU:
    {
      if (keyswitch & EVENT_RIGHTALT)
        return EVENT_ALT_R;
      if (keyswitch & EVENT_LEFTALT)
        return EVENT_ALT_L;
    }
    break;
    //  case VK_APPS:
    //    return EVENT_EXTRA_R;
  }
  
  return EVENT_NO;
}

static HWINEVENTHOOK g_system_foreground_hook = NULL;
static HHOOK g_hKeyboardHook = NULL;

// http://msdn.microsoft.com/en-us/library/windows/desktop/ee416808(v=vs.85).aspx
static LRESULT CALLBACK LowLevelKeyboardProc( int nCode, WPARAM wParam, LPARAM lParam )
{
          
    if (nCode < 0 || nCode != HC_ACTION )  // do not process message 
        return CallNextHookEx( g_hKeyboardHook, nCode, wParam, lParam); 
 
    KBDLLHOOKSTRUCT* p = (KBDLLHOOKSTRUCT*)lParam;

    if(wParam==WM_KEYDOWN || wParam==WM_KEYUP){
      //printf("^^^^^^^^^^^^^^^^^^ p: %p\n",p);
      //printf("^^^^^^^^^^^^^^^^^^^^^ vkCode: %x\n",(int)p->vkCode);

#if 0
      printf("  %s: vkCode: %x\n",wParam==WM_KEYDOWN ? "Down":"Up", (int)p->vkCode);
#endif
      
      if(p->vkCode==VK_LWIN || p->vkCode==VK_RWIN){// || p->vkCode==VK_RMENU){

        if(p->vkCode==VK_LWIN)
          ATOMIC_SET(left_windows_down, wParam==WM_KEYDOWN);
        else
          ATOMIC_SET(right_windows_down, wParam==WM_KEYDOWN);

#if 0
        if(p->vkCode==VK_LWIN && wParam==WM_KEYDOWN)
          printf("   1. Left down\n");
        else if (p->vkCode==VK_LWIN && wParam==WM_KEYUP)
          printf("   1. Left up\n");
        else if (p->vkCode==VK_RMENU && wParam==WM_KEYDOWN)
          printf("   2. Right down\n");
        else if (p->vkCode==VK_RMENU)
          printf("   2. Right up\n");
#endif
        
        #if 0
        // Don't think this is any point.
        if (left_windows_down)
          tevent.keyswitch |= EVENT_LEFTEXTRA1;
        else
          tevent.keyswitch &= (~EVENT_LEFTEXTRA1);
        #endif
        
        //printf("active: %d, left: %s, right: %s. switch: %x\n",g_bWindowActive, left_windows_down?"down":"up", right_windows_down?"down":"up", );
        if(ATOMIC_GET(g_bWindowActive)==true)
          return 1; // To avoid having the windows menu pop up when the radium window is active and pressing left windows key.
      }
    }
 
    return CallNextHookEx( g_hKeyboardHook, nCode, wParam, lParam );
}

// Code copied from here: https://social.msdn.microsoft.com/Forums/windowsdesktop/en-US/f6032ca1-31b8-4ad5-be39-f78dd29952da/hooking-problem-in-windows-7?forum=windowscompatibility
static DWORD WINAPI hookThreadProc(LPVOID lParam)
{
    MSG msg;

    g_hKeyboardHook = SetWindowsHookEx( WH_KEYBOARD_LL,  LowLevelKeyboardProc, GetModuleHandle(NULL), 0 );
    
    while(GetMessage(&msg, NULL, 0, 0) != FALSE)
    {
        TranslateMessage(&msg);
        DispatchMessage(&msg);
    }

    return 0;
}

void OS_WINDOWS_set_key_window(void *win){
  SetForegroundWindow(win);
}
  
bool OS_WINDOWS_is_key_window(void *maybewin){
  //printf("a: %p, b: %p. %d\n",maybewin, GetForegroundWindow(), maybewin == GetForegroundWindow());
  HWND f = GetForegroundWindow();
  return f==NULL || maybewin == f;
}

static VOID CALLBACK WinEventProcCallback ( HWINEVENTHOOK hWinEventHook, DWORD dwEvent, HWND hwnd, LONG idObject, LONG idChild, DWORD dwEventThread, DWORD dwmsEventTime)
{
  //fprintf(stderr, "\n\n  =======  WinEventHook.  Foreground window : %p     ========\n\n",GetForegroundWindow());
  //fflush(stderr);
  call_me_if_another_window_may_have_taken_focus_but_still_need_our_key_events();
}
 

void OS_SYSTEM_init_keyboard(void) {
  init_keymap();  

  g_system_foreground_hook = SetWinEventHook(EVENT_SYSTEM_FOREGROUND , 
                                             EVENT_SYSTEM_FOREGROUND ,
                                             NULL, 
                                             WinEventProcCallback, 0, 0, 
                                             WINEVENT_OUTOFCONTEXT  //| WINEVENT_SKIPOWNPROCESS);
                                             );


  CreateThread( 
               NULL,                   // default security attributes
               0,                      // use default stack size  
               hookThreadProc,       // thread function name
               NULL,          // argument to thread function 
               0,                      // use default creation flags 
               NULL   // returns the thread identifier 
                );
    
  //g_hKeyboardHook = SetWindowsHookEx( WH_KEYBOARD_LL,  LowLevelKeyboardProc, GetModuleHandle(NULL), 0 );
}

void W_KeyboardHandlerShutDown(void){
  //if(g_hKeyboardHook!=NULL)
  //  UnhookWindowsHookEx(g_hKeyboardHook);

  if(g_system_foreground_hook!=NULL)
    UnhookWinEvent(g_system_foreground_hook);
}

#ifdef RUN_TEST
struct Root *root = NULL;
struct TEvent tevent = {0};
PlayerClass *pc = NULL;
int num_users_of_keyboard = 0;
int64_t MIXER_get_time(void){
  return 0;
}
bool EventReciever(struct TEvent *tevent,struct Tracker_Windows *window){
  return true;
}
void PlayBlockFromStart(struct Tracker_Windows *window,bool do_loop){
}
void CRASHREPORTER_send_assert_message(enum Crash_Type crash_type, const char *fmt,...){
  fprintf(stderr," CRASH: %s\n", fmt);
  abort();
}
void call_me_if_another_window_may_have_taken_focus_but_still_need_our_key_events(void){
}

// x86_64-w64-mingw32-g++ -Wall W_Keyboard.c -mconsole -DRUN_TEST -DFOR_WINDOWS -I../Qt/ -DUSE_QT_REQTYPE=1 ../common/scancodes.c `mingw64-pkg-config --libs --cflags QtGui` && wine64 a.exe 

#include <QApplication>
#include <QPushButton>

class MyApplication : public QApplication{
public:

  MyApplication(int &argc,char **argv)
    : QApplication(argc,argv)
    {
      //setStyleSheet("QStatusBar::item { border: 0px solid black }; ");
    }

protected:

  bool last_key_was_lalt;

  
  bool SystemEventFilter(void *event){
    OS_SYSTEM_EventPreHandler(event);
    
    MSG *msg = (MSG*)event;
    int type = OS_SYSTEM_get_event_type(event, true);
    if (type!=-1){
      int keynum = OS_SYSTEM_get_keynum(event);
      int qwerty = OS_SYSTEM_get_qwerty_keynum(event);
      printf("Got %s event: %d / %d. swiktch: %x. wparam: 0x%lx. Modifier: %d. Autorepeat: %s\n",
             type==TR_KEYBOARD?"press":"release",keynum,qwerty,get_keyswitch(),(long unsigned int)msg->wParam,OS_SYSTEM_get_modifier(event),type==TR_AUTOREPEAT?"true":"false"
             );
    }
    return false;
  }

  bool 	winEventFilter ( MSG * msg, long * result ){
    return SystemEventFilter(msg);
  }

};

int main(int argc, char **argv)
{
  OS_SYSTEM_init_keyboard();

  MyApplication app (argc, argv);
  
  QPushButton button ("Hello world !");
  button.show();
  
  app.exec();
  
  W_KeyboardHandlerShutDown();
  return 0;
}

#endif

#endif // FOR_WINDOWS

