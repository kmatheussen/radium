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



#include "X11.h"

#include "../common/nsmtracker.h"
#include "../common/eventreciever_proc.h"


#include "X11_keyboard_proc.h"


/*
  X11 key array -> Radium key array:

  9: ESC
  10-22: 1-0,0R1,0R2,BACKSPACE
  23-35: TAB-PR2
  36: RETURN
  38-48: A-L,LR1,LR2
  49: 1L1
  51: LR3
  52-61: Z-MR3
  63: KP_MUL
  65: SPACE

  67-76: F1-F10
  79-81: KP_7-KP_9
  82: KP_SUB
  83-85: KP_4-KP_6
  86: KP_ADD
  87-89: KP_1-KØ_3
  90: KP_0
  91: KP_DOT
  94: ZL1
  95-96: F11,F12
  97: MIDDLE2
  98: UPARROW
  99: MIDDLE3
  100: LEFTARROW
  102: RIGHTARROW
  103: HELP
  104: DOWNARROW
  105: MIDDLE6
  106: MIDDLE1
  107: DEL
  108: KP_ENTER
  111: F13
  112: KP_DIV
  117: HELP (nede)

  Switch:
  66: CAPS

 */


static int X11_keytable_to_radium_keytable[]={
  // 0
  EVENT_NO,EVENT_NO,EVENT_NO,EVENT_NO,EVENT_NO,EVENT_NO,EVENT_NO,EVENT_NO,EVENT_NO,
  // 9
  EVENT_ESC,
  // 10
  EVENT_1,EVENT_2,EVENT_3,EVENT_4,EVENT_5,EVENT_6,EVENT_7,EVENT_8,EVENT_9,EVENT_0,EVENT_0R1,EVENT_0R2,EVENT_BACKSPACE,
  // 23
  EVENT_TAB,EVENT_Q,EVENT_W,EVENT_E,EVENT_R,EVENT_T,EVENT_Y,EVENT_U,EVENT_I,EVENT_O,EVENT_P,EVENT_PR1,EVENT_PR2,
  // 36
  EVENT_RETURN,
  // 37
  EVENT_LEFTCTRL_I,
  //EVENT_NO,
  // 38
  EVENT_A,EVENT_S,EVENT_D,EVENT_F,EVENT_G,EVENT_H,EVENT_J,EVENT_K,EVENT_L,EVENT_LR1,EVENT_LR2,
  // 49
  EVENT_1L1,
  // 50
  EVENT_LEFTSHIFT_I,
  //EVENT_NO,
  // 51
  EVENT_LR3,
  // 52
  EVENT_Z,EVENT_X,EVENT_C,EVENT_V,EVENT_B,EVENT_N,EVENT_M,EVENT_MR1,EVENT_MR2,EVENT_MR3,
  // 62
  EVENT_RIGHTSHIFT_I,
  // 63
  EVENT_KP_MUL,
  // 64
  EVENT_LEFTALT_I,
  // 65
  EVENT_SPACE,
  // 66
  EVENT_CAPSLOCK_I,
  //  EVENT_NO,
  // 67
  EVENT_F1,EVENT_F2,EVENT_F3,EVENT_F4,EVENT_F5,EVENT_F6,EVENT_F7,EVENT_F8,EVENT_F9,EVENT_F10,
  // 77
  EVENT_NO,EVENT_NO,
  // 79
  EVENT_KP_7,EVENT_KP_8,EVENT_KP_9,
  // 82
  EVENT_KP_SUB,
  // 83
  EVENT_KP_4,EVENT_KP_5,EVENT_KP_6,
  // 86
  EVENT_KP_ADD,
  // 87
  EVENT_KP_1,EVENT_KP_2,EVENT_KP_3,
  //90
  EVENT_KP_0,
  //91
  EVENT_KP_DOT,
  //92
  EVENT_NO,EVENT_NO,
  //94
  EVENT_ZL1,
  //95
  EVENT_F11,EVENT_F12,
  //97
  EVENT_MIDDLE2,
  //98
  EVENT_UPARROW,
  //99
  EVENT_MIDDLE3,
  //100
  EVENT_LEFTARROW,
  //101
  EVENT_NO,
  //102
  EVENT_RIGHTARROW,
  //103
  EVENT_HELP,
  EVENT_DOWNARROW,
  EVENT_MIDDLE6,
  EVENT_MIDDLE1,
  EVENT_DEL,
  EVENT_KP_ENTER,
  //109
  EVENT_RIGHTCTRL_I,
  EVENT_NO,
  //111
  EVENT_F13,
  EVENT_KP_DIV,
  //113
  EVENT_RIGHTALT_I,
  //114
  EVENT_NO,
  //115
  EVENT_LEFTEXTRA1_I,
  EVENT_RIGHTEXTRA1_I,
  //117
  EVENT_HELP
};

static int keyupdowns[EVENT_MAX_I+1]={0};


extern struct TEvent tevent;


/*
37: EVENT_LEFTCTRL
50: EVENT_LEFTSHIFT
66: EVENT_CAPSLOCK
115: EVENT_LEFTEXTRA
64: EVENT_LEFTALT

113: EVENT_RIGHTALT
116: EVENT_RIGHTEXTRA
109: EVENT_RIGHTCTRL
62: EVENT_RIGHTSHIFT

 */

static void setKeySwitch(unsigned int state){
  int lokke;
  const int numswitches=9;
#if 0
  static int x11switch[]   ={37,50,66,
			     115,64,113,
			     116,109,62};
#endif

  static int x11switch[]={EVENT_LEFTCTRL_I,EVENT_LEFTSHIFT_I,EVENT_CAPSLOCK_I,
			     EVENT_LEFTEXTRA1_I,EVENT_LEFTALT_I,EVENT_RIGHTALT_I,
			     EVENT_RIGHTEXTRA1_I,EVENT_RIGHTCTRL_I,EVENT_RIGHTSHIFT_I};

  // Note! EVENT_RIGHTEXTRA1 and EVENT_RIGHTCTRL is switched because EVENT_RIGHTEXTRA1 is autorepeating.
  // EVENT_RIGHTCTRL isn't used anyway.
  // This behaviour might change in the future.
  static int radiumswitch[]={EVENT_LEFTCTRL,EVENT_LEFTSHIFT,EVENT_CAPSLOCK,
			     EVENT_LEFTEXTRA1,EVENT_LEFTALT,EVENT_RIGHTALT,
			     EVENT_RIGHTEXTRA1,EVENT_RIGHTEXTRA1,EVENT_RIGHTSHIFT};

  tevent.keyswitch=0;
  for(lokke=0;lokke<numswitches;lokke++){
    if(keyupdowns[x11switch[lokke]]==1){
      tevent.keyswitch|=radiumswitch[lokke];
    }
  }

  printf("keyswitch: %x / %x\n",(unsigned int)tevent.keyswitch,state);
}



// This function fixes situations where keyswitches is registered by radium to be pressed, but in fact
// are not since they were unreleased in another window. Happens quite often.
/*
static void fixBrokenKeySwitch(unsigned int state){
  if(state&ShiftMask) printf("shift\n");
  if(state&ControlMask) printf("control\n");
  if(state&Mod1Mask) printf("mod1\n");
  if(state&Mod2Mask) printf("mod2\n");
  if(state&Mod3Mask) printf("mod3\n");
  if(state&Mod4Mask) printf("mod4\n");
  if(state&Mod5Mask) printf("mod5\n");
}
*/

int X11Event_KeyPress(int keynum,int keystate,struct Tracker_Windows *window){
  //switch(((XKeyEvent *)&event)->keycode){
  printf("Pressing %d,\n",keynum);
  keyupdowns[keynum]=1;
  setKeySwitch(keystate);
  tevent.ID=TR_KEYBOARD;
  tevent.SubID=keynum;
  if(tevent.SubID>EVENT_MAX) tevent.SubID=EVENT_NO;

  //fixBrokenKeySwitch(keystate);

  return EventReciever(&tevent,window);
}

void X11_ResetKeysUpDowns(void){
  memset(keyupdowns,0,sizeof(int)*EVENT_MAX_I+1);
}

int X11_KeyPress(XKeyEvent *event,struct Tracker_Windows *window){
  //printf("State: %x\n",(unsigned int)event->state);
  //  return 0;
  return X11Event_KeyPress(X11_keytable_to_radium_keytable[event->keycode],event->state,window);
}


int X11Event_KeyRelease(int keynum,int keystate,struct Tracker_Windows *window){
  //printf("Releasing %d,\n",keynum);
  keyupdowns[keynum]=0;
  setKeySwitch(keystate);
  tevent.ID=TR_KEYBOARDUP;
  tevent.SubID=keynum;
  if(tevent.SubID>EVENT_MAX) tevent.SubID=EVENT_NO;
  EventReciever(&tevent,window);
  return 0;
}

int X11_KeyRelease(XKeyEvent *event,struct Tracker_Windows *window){
  return X11Event_KeyRelease(X11_keytable_to_radium_keytable[event->keycode],event->state,window);
}

