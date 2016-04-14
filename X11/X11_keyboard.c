
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


#ifdef __linux__

#include "Python.h"


#include "X11.h"
#include <X11/Xlib.h>

#include "../common/nsmtracker.h"
#include "../common/playerclass.h"
#include "../common/eventreciever_proc.h"
#include "../common/player_proc.h"
#include "../common/OS_Player_proc.h"
#include "../common/hashmap_proc.h"
#include "../audio/Mixer_proc.h"
#include "../common/scancodes_proc.h"

#include "../common/OS_system_proc.h"


static int keycode_to_keynum[256]; // "A KeyCode represents a physical (or logical) key. KeyCodes lie in the inclusive range [8,255]" (keyboard-encoding.html)


void OS_SYSTEM_init_keyboard(void) {
  OS_SYSTEM_ResetKeysUpDowns();
}


static int keysym_to_keynum(KeySym keysym) {

  // Handle some special keys first.
  switch(keysym){
  case XK_Menu:
    return EVENT_MENU;
  case XF86XK_AudioLowerVolume:
    return EVENT_VOLUME_DOWN;
  case XF86XK_AudioRaiseVolume:
    return EVENT_VOLUME_UP;
  case XF86XK_AudioMute:
    return EVENT_MUTE;
  case XF86XK_AudioPlay:
    return EVENT_PLAY;
  case XF86XK_AudioStop:
    return EVENT_STOP;
  case XF86XK_Calculator:
    return EVENT_CALCULATOR;
  case XF86XK_Mail:
    return EVENT_MAIL;
  case XF86XK_HomePage:
    return EVENT_HOMEPAGE;
  }


# define S(X11_VAL, EVENT_VAL) if(keysym==XK_##X11_VAL) return EVENT_##EVENT_VAL;
# define T(VAL) if(keysym==XK_##VAL) return EVENT_##VAL;


  // row 1
  /////////////////////
  S(Escape,ESC);
  T(F1);T(F2);T(F3);T(F4);T(F5);T(F6);T(F7);T(F8);T(F9);T(F10);T(F11);T(F12);



  // row 2
  /////////////////////

  //S(grave, 1L1);
  T(1);T(2);T(3);T(4);T(5);T(6);T(7);T(8);T(9);T(0);
  //S(minus, 0R1);S(equal, 0R2); // Missing 0R3!
  S(BackSpace, BACKSPACE);



  // row 3
  /////////////////////

  S(Tab, TAB);
  S(q,Q);S(w,W);S(e,E);S(r,R);S(t,T);S(y,Y);S(u,U);S(i,I);S(o,O);S(p,P);
  //S(bracketleft,PR1);S(bracketright,PR2);
  S(Return, RETURN);



  // row 4
  /////////////////////
  S(ISO_Next_Group, CAPS);
  S(a,A);S(s,S);S(d,D);S(f,F);S(g,G);S(h,H);S(j,J);S(k,K);S(l,L);
  //S(semicolon,LR1);S(apostrophe,LR2);S(backslash,LR3);


  // row 5
  /////////////////////

  // ZL1
  //S(less,ZL1);
  S(z,Z);S(x,X);S(c,C);S(v,V);S(b,B);S(n,N);S(m,M);
  //S(comma,MR1);S(period,MR2);S(slash,MR3);

  

  // Row 6
  /////////////////////


  S(space, SPACE);

  S(Control_L, CTRL_L);
  S(Control_R, CTRL_R);
  S(Shift_L, SHIFT_L);
  S(Shift_R, SHIFT_R);
  S(Alt_L, ALT_L);
  S(Alt_R, ALT_R); S(ISO_Level3_Shift, ALT_R);
  S(Super_L, EXTRA_L);
  S(Super_R, EXTRA_R);



  // Between keyboard and keypad
  ///////////////////////////////

  S(Insert, INSERT);
  S(Home, HOME);
  S(Page_Up, PAGE_UP);
  S(Page_Down, PAGE_DOWN);
  S(Delete, DEL);
  S(Home, HOME);
  S(End, END);

  S(Down, DOWNARROW);
  S(Up, UPARROW);
  S(Right, RIGHTARROW);
  S(Left, LEFTARROW);



  // Keypad
  /////////////////////


  // KP_E1, KP_E2 (??)
  S(KP_Divide, KP_DIV);
  S(KP_Multiply, KP_MUL);
  S(KP_Subtract, KP_SUB);
  S(KP_Add, KP_ADD);
  S(KP_Insert, KP_0);
  S(KP_Delete, KP_DOT);
  S(KP_Enter, KP_ENTER);
  S(KP_End, KP_1);
  S(KP_Down, KP_2);
  S(KP_Next, KP_3);
  S(KP_Left, KP_4);
  S(KP_Begin, KP_5);
  S(KP_Right, KP_6);
  S(KP_Home, KP_7);
  S(KP_Up, KP_8);
  S(KP_Prior, KP_9);

  return EVENT_NO;

# undef T
# undef S
}


static void init_keynums(XEvent *event){
  static bool inited_keynums = false;

  if(event->type==KeyPress || event->type==KeyRelease){
    
    if(inited_keynums==false){
      XAnyEvent *any_event = (XAnyEvent *)event;
    
      int i;
      for(i=0;i<256;i++)
        keycode_to_keynum[i] = keysym_to_keynum(XkbKeycodeToKeysym(any_event->display, i, 0, 0));
      inited_keynums = true;
    }
  }
}


static int get_modifier(KeySym keysym){
# define S(X11_VAL, EVENT_VAL) case XK_##X11_VAL: return EVENT_##EVENT_VAL;

  //printf("keysum: %d, caps_lock: %d\n",(int)keysym,XK_Caps_Lock);
  switch(keysym){
    S(Control_L, CTRL_L);
    S(Control_R, CTRL_R);
    S(Caps_Lock, CAPS);
    S(Shift_L, SHIFT_L);
    S(Shift_R, SHIFT_R);
    S(Alt_L, ALT_L);
    S(Alt_R, ALT_R); S(ISO_Level3_Shift, ALT_R);
    S(Super_L, EXTRA_L);
    S(Super_R, EXTRA_R);
  }

#undef S
  return EVENT_NO;
}

int OS_SYSTEM_get_modifier(void *void_event){
  XKeyEvent *event = void_event;

  KeySym keysym = XkbKeycodeToKeysym(event->display, event->keycode, 0, 0);
    
  //KeySym keysym = (KeySym)virtualkey;

  int ret = get_modifier(keysym);

  if (ret==EVENT_NO && get_subID_from_scancode(OS_SYSTEM_get_keycode(void_event))==EVENT_CAPS) // caps lock key doesn't map to XK_Caps_Lock on my keyboard.
    return EVENT_CAPS;

  return ret;
}

int OS_SYSTEM_get_keynum(void *event){
  XKeyEvent *key_event = event;
  init_keynums((XEvent*)key_event);
  return keycode_to_keynum[key_event->keycode];
}

int OS_SYSTEM_get_qwerty_keynum(void *event){
  XKeyEvent *key_event = event;
  
  return get_subID_from_scancode(key_event->keycode-8);
}

int OS_SYSTEM_get_keycode(void *event){
  XKeyEvent *key_event = event;
  
  return key_event->keycode-8;
}





void OS_SYSTEM_EventPreHandler(void *void_event){
  XEvent *event = void_event;
  
  //init_keynums(NULL, event);

  switch(event->type){
  case EnterNotify:
    {
      XCrossingEvent *e = (XCrossingEvent*) event;
      //printf("got enter notify. mode: %d, same_screen: %d, focus: %d\n",(int)e->mode,(int)e->same_screen,(int)e->focus);
      if(e->focus==False)
        OS_SYSTEM_ResetKeysUpDowns();
    }
    break;
  case LeaveNotify:
    {
      XCrossingEvent *e = (XCrossingEvent*) event;
      //printf("got leave notify. mode: %d, same_screen: %d, focus: %d\n",(int)e->mode,(int)e->same_screen,(int)e->focus);
      if(e->focus==False)
        OS_SYSTEM_ResetKeysUpDowns();
    }
    break;
  }
}

static bool event_is_arrow(XKeyEvent *event){
  KeySym keysym = XkbKeycodeToKeysym(event->display, event->keycode, 0, 0);

  return keysym==XK_Down || keysym==XK_Up || keysym==XK_Right || keysym==XK_Left || keysym==XK_Page_Up || keysym==XK_Page_Down;
}

int OS_SYSTEM_get_event_type(void *void_event, bool ignore_autorepeat){
  XEvent *event = void_event;
  
  if(event->type==KeyPress){
    //printf(">>> Keypress\n");
    return TR_KEYBOARD;
  }
  
  else if (event->type==KeyRelease){

    if (ignore_autorepeat && !event_is_arrow(&event->xkey)){

      // logic picked up from http://stackoverflow.com/questions/2100654/ignore-auto-repeat-in-x11-applications
      
      Display *dis = event->xkey.display;
    
      if (XEventsQueued(dis, QueuedAfterReading)) {
        XEvent nev;
        XPeekEvent(dis, &nev);
        
        if (nev.type == KeyPress &&
            nev.xkey.time == event->xkey.time &&
            nev.xkey.keycode == event->xkey.keycode
            )
          {
            //fprintf (stdout, "key #%ld was retriggered.\n",
            //         (long) XLookupKeysym (&nev.xkey, 0));
            
            // delete retriggered KeyPress event
            XNextEvent (dis, event);
            return TR_AUTOREPEAT;
          }
      }
    }

    //printf("<<< KeyRelease\n");
    
    //fprintf (stdout, "key #%ld was released.\n",
    //         (long) XLookupKeysym (&event->xkey, 0));

     
    return TR_KEYBOARDUP;
  }
  
  else
    return -1;
}


#endif // __linux__
