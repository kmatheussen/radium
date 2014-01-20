
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

#include "X11.h"

#include "../common/nsmtracker.h"
#include "../common/playerclass.h"
#include "../common/eventreciever_proc.h"
#include "../common/player_proc.h"
#include "../common/OS_Player_proc.h"
#include "../common/hashmap_proc.h"
#include "../audio/Mixer_proc.h"

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

extern bool doquit;
extern struct Root *root;

extern PlayerClass *pc;


static int keytable_size = 0;
static int *keytable = NULL;
static hash_t *keyupdowns = NULL;


static void add_key(int X11_val, int EVENT_val){
  if (X11_val >= keytable_size) {
    int new_size = 128;
    while (X11_val >= new_size)
      new_size *= 2;
    int *new_keytable = talloc_atomic_uncollectable(sizeof(int)*new_size);
    {
      int i=0;
      for(i=0 ; i<new_size ; i++)
        if (i<keytable_size)
          new_keytable[i] = keytable[i];
        else
          new_keytable[i] = EVENT_NO;
    }
    if(keytable!=NULL)
      tfree(keytable);
    keytable = new_keytable;
    keytable_size = new_size;
  }
  keytable[X11_val] = EVENT_val;
}

static void init_keytable(void) {
# define S(X11_VAL, EVENT_VAL) add_key(XK_##X11_VAL, EVENT_##EVENT_VAL)
# define T(VAL) add_key(XK_##VAL, EVENT_##VAL)

  // row 1
  /////////////////////
  S(Escape,ESC);
  T(F1);T(F2);T(F3);T(F4);T(F5);T(F6);T(F7);T(F8);T(F9);T(F10);T(F11);T(F12);



  // row 2
  /////////////////////

  S(grave, 1L1);
  T(1);T(2);T(3);T(4);T(5);T(6);T(7);T(8);T(9);T(0);
  S(minus, 0R1);S(equal, 0R2); // Missing 0R3!
  S(BackSpace, BACKSPACE);



  // row 3
  /////////////////////

  S(Tab, TAB);
  S(q,Q);S(w,W);S(e,E);S(r,R);S(t,T);S(y,Y);S(u,U);S(i,I);S(o,O);S(p,P);
  S(bracketleft,PR1);S(bracketright,PR2);
  S(Return, RETURN);



  // row 4
  /////////////////////
  S(ISO_Next_Group, CAPS);
  S(a,A);S(s,S);S(d,D);S(f,F);S(g,G);S(h,H);S(j,J);S(k,K);S(l,L);
  S(semicolon,LR1);S(apostrophe,LR2);S(backslash,LR3);


  // row 5
  /////////////////////

  // ZL1
  S(less,ZL1);
  S(z,Z);S(x,X);S(c,C);S(v,V);S(b,B);S(n,N);S(m,M);
  S(comma,MR1);S(period,MR2);S(slash,MR3);

  

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



# undef T
# undef S
}

void X11_ResetKeysUpDowns(void){
  keyupdowns = HASH_create(keytable_size+100);
}

static const char *get_keyupdown_key(int keynum){
  char raw_key[128];
  sprintf(raw_key,"%d",keynum);
  const char *key = HASH_get_key(keyupdowns, raw_key);

  if(key!=NULL)
    return key;
  else
    return (const char*)talloc_format("%d",keynum);
}

static void set_keydown(int keynum){
  const char *key = get_keyupdown_key(keynum);
  HASH_put_int(keyupdowns,key,1);
}

static void set_keyup(int keynum){
  const char *key = get_keyupdown_key(keynum);
  HASH_put_int(keyupdowns,key,0);
}

static bool get_keyupdown(keynum){
  char key[128];
  sprintf(key,"%d",keynum);
  return HASH_has_key(keyupdowns,key) && HASH_get_int(keyupdowns,key)==1;
}

void X11_init_keyboard(void) {
  init_keytable();
  X11_ResetKeysUpDowns();
}

static int get_keynum(XEvent *event){
  XKeyEvent *key_event = (XKeyEvent *)event;
  KeySym sym = XkbKeycodeToKeysym(key_event->display, key_event->keycode, 0, 0);

  // Some of the sym values are very large. Can not add them since the keytable would be very large.
  switch(sym){
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

  if(sym > keytable_size) {
    return -1;
  } else
    return keytable[sym];
}



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

  static int x11switch[]={EVENT_CTRL_L, EVENT_SHIFT_L,EVENT_CAPS,
                          EVENT_EXTRA_L,EVENT_ALT_L,EVENT_ALT_R,
                          EVENT_EXTRA_R, EVENT_CTRL_R, EVENT_SHIFT_R
                         };

  static int radiumswitch[]={EVENT_LEFTCTRL,EVENT_LEFTSHIFT,EVENT_CAPSLOCK,
			     EVENT_LEFTEXTRA1,EVENT_LEFTALT,EVENT_RIGHTALT,
			     EVENT_RIGHTEXTRA1,EVENT_RIGHTCTRL,EVENT_RIGHTSHIFT
                            };

  int numswitches = sizeof(x11switch)/sizeof(int);

  tevent.keyswitch=0;
  for(lokke=0;lokke<numswitches;lokke++){
    int keynum = x11switch[lokke];
    if(get_keyupdown(keynum)==true){
      tevent.keyswitch |= radiumswitch[lokke];
    }
  }

  //printf("keyswitch: %x / %x. Leftshift: %s. Rightshift: %s\n",(unsigned int)tevent.keyswitch, state, LeftShift(tevent.keyswitch)?"on":"off", RightShift(tevent.keyswitch)?"on":"off");
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

static void setKeyUpDowns(XEvent *event){
  if(event->type!=KeyPress && event->type!=KeyRelease)
    return;

  int keynum = get_keynum(event);
  if(keynum==-1)
    return;

  if(event->type==KeyPress)
    set_keydown(keynum);
  else
    set_keyup(keynum);
}

static int g_last_pressed_key = -1;
static int64_t g_last_pressed_key_time = 0;

int X11Event_KeyPress(int keynum,int keystate,struct Tracker_Windows *window){
  setKeySwitch(keystate);
  tevent.ID=TR_KEYBOARD;
  tevent.SubID=keynum;

  if(keynum==EVENT_ALT_R)
    printf("> Pressing alt_gr\n");

  g_last_pressed_key = keynum;
  g_last_pressed_key_time = MIXER_get_time();

  if(tevent.SubID<EVENT_FIRST_NON_QUALIFIER)
    tevent.SubID=EVENT_NO;

  //fixBrokenKeySwitch(keystate);

  return EventReciever(&tevent,window);
}

int X11_MyKeyPress(XEvent *event,struct Tracker_Windows *window){
  //printf("keynum: %x. keycode: %d. Audio: %x/%d\n",(unsigned int)sym,event->keycode,0x1008FF1,0x1008FF1);

  int keynum = get_keynum(event);

  if (keynum==-1)
    return 0;
  else
    return X11Event_KeyPress(keynum,((XKeyEvent*)event)->state,window);
}


int X11Event_KeyRelease(int keynum,int keystate,struct Tracker_Windows *window){
  setKeySwitch(keystate);
  tevent.ID=TR_KEYBOARDUP;
  tevent.SubID=keynum;

  if(keynum==EVENT_ALT_R)
    printf("< Releasing alt_gr\n");

  int64_t time_now = MIXER_get_time();

  if( (time_now-g_last_pressed_key_time) < pc->pfreq/4){ // i.e. only play if holding the key less than 0.25 seconds.
    if(keynum==g_last_pressed_key && keynum==EVENT_ALT_R)
      PlayBlockFromStart(window,true); // true == do_loop
    
    if(keynum==g_last_pressed_key && keynum==EVENT_SHIFT_R)
      PlayBlockFromStart(window,true); // true == do_loop
  }

  if(tevent.SubID<EVENT_FIRST_NON_QUALIFIER)
    tevent.SubID=EVENT_NO;

  EventReciever(&tevent,window);
  return 0;
}

int X11_MyKeyRelease(XEvent *event,struct Tracker_Windows *window){
  int keynum = get_keynum(event);

  if (keynum==-1)
    return 0;
  else

  return X11Event_KeyRelease(keynum,((XKeyEvent*)event)->state,window);
}


extern int num_users_of_keyboard;

bool X11_KeyboardFilter(XEvent *event){
  setKeyUpDowns(event);

  //static int num=0;
  //printf("Got event %d\n",num++);
  switch(event->type){
  case KeyPress:
    if(num_users_of_keyboard>0)
      return false;

    if(X11_MyKeyPress(event,root->song->tracker_windows)==1){
      //this->quit();
      //doquit = true;
    }
    return true;
  case KeyRelease:
    if(num_users_of_keyboard>0)
      return false;

    X11_MyKeyRelease(event,root->song->tracker_windows);
    return true;
  case EnterNotify:
    {
      XCrossingEvent *e = (XCrossingEvent*) event;
      //printf("got enter notify. mode: %d, same_screen: %d, focus: %d\n",(int)e->mode,(int)e->same_screen,(int)e->focus);
      if(e->focus==False)
        X11_ResetKeysUpDowns();
    }
    break;
  case LeaveNotify:
    {
      XCrossingEvent *e = (XCrossingEvent*) event;
      //printf("got leave notify. mode: %d, same_screen: %d, focus: %d\n",(int)e->mode,(int)e->same_screen,(int)e->focus);
      if(e->focus==False)
        X11_ResetKeysUpDowns();
    }
    break;
  case ClientMessage:
#if 0
    if(X11Event_ClientMessage((XClientMessageEvent *)&event,root->song->tracker_windows)==false){
      this->quit();
    }
#endif
    break;
  default:
    //printf("Got unknwon event %d. %d %d\n",num++,instrumentWidgetUsesKeyboard(),event->type);

    //fprintf(stderr, "got Unknown x11 event\n");
    break;
  }

  return false;
}

#endif // __linux__
