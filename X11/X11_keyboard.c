
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

#include "../common/nsmtracker.h"
#include "../common/playerclass.h"
#include "../common/eventreciever_proc.h"
#include "../common/player_proc.h"
#include "../common/OS_Player_proc.h"
#include "../common/hashmap_proc.h"
#include "../audio/Mixer_proc.h"

#include "X11_keyboard_proc.h"


extern bool doquit;
extern struct Root *root;

extern PlayerClass *pc;

static hash_t *keyupdowns = NULL;

static int keycode_to_keynum[256];


void X11_ResetKeysUpDowns(void){
  keyupdowns = HASH_create(EVENT_DASMAX);
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
  X11_ResetKeysUpDowns();
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

  return -1;

# undef T
# undef S
}


struct displays_t{
  struct displays_t *next;
  Display *display;
};

static void init_keynums(XEvent *event){
  static struct displays_t *displays = NULL;
  static bool inited_keynums = false;


  if(event->type==KeyPress || event->type==KeyRelease) {

    XAnyEvent *any_event = (XAnyEvent *)event;

    if(inited_keynums==false){
      
      PyRun_SimpleString("import X11_xkb ; X11_xkb.save_xkb(os.path.join(sys.g_program_path,\"packages/setxkbmap/setxkbmap\"))");
      {
        PyRun_SimpleString("import X11_xkb ; X11_xkb.set_xkb(os.path.join(sys.g_program_path,\"packages/setxkbmap/setxkbmap\"), \"us\")");
        
        int i;
        for(i=0;i<256;i++)
          keycode_to_keynum[i] = keysym_to_keynum(XkbKeycodeToKeysym(any_event->display, i, 0, 0));
      }
      //sleep(1);
      //PyRun_SimpleString("import X11_xkb ; X11_xkb.restore_xkb(os.path.join(sys.g_program_path,\"packages/setxkbmap/setxkbmap\"))");
      
      inited_keynums = true;
    }
    
    struct displays_t *display = displays;
    
    while(display!=NULL && display->display!=any_event->display)
      display=display->next;
    
    if(display==NULL){
      printf("\n\nSetting back keyboards for display %p\n\n",any_event->display);
      PyRun_SimpleString("import X11_xkb ; X11_xkb.restore_xkb(os.path.join(sys.g_program_path,\"packages/setxkbmap/setxkbmap\"))");
      display = calloc(1,sizeof(struct displays_t));
      display->display = any_event->display;
      display->next = displays;
      displays = display;
    }
  }
}


int X11_get_keynum(XKeyEvent *key_event){
  init_keynums((XEvent*)key_event);
  return keycode_to_keynum[key_event->keycode];
}



extern struct TEvent tevent;


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



static void setKeyUpDowns(XKeyEvent *key_event){
  int keynum = X11_get_keynum(key_event);
  if(keynum==-1)
    return;

  if(key_event->type==KeyPress)
    set_keydown(keynum);
  else
    set_keyup(keynum);
}

static int g_last_pressed_key = -1;
static int64_t g_last_pressed_key_time = 0;

static int X11Event_KeyPress(int keynum,int keystate,struct Tracker_Windows *window){
  setKeySwitch(keystate);
  tevent.ID=TR_KEYBOARD;
  tevent.SubID=keynum;

  if(keynum==EVENT_ALT_R)
    printf("> Pressing alt_gr\n");

  g_last_pressed_key = keynum;
  g_last_pressed_key_time = MIXER_get_time();

  if(tevent.SubID<EVENT_FIRST_NON_QUALIFIER)
    tevent.SubID=EVENT_NO;

  return EventReciever(&tevent,window);
}

static int X11_MyKeyPress(XKeyEvent *key_event,struct Tracker_Windows *window){
  //printf("keynum: %x. keycode: %d. Audio: %x/%d\n",(unsigned int)sym,event->keycode,0x1008FF1,0x1008FF1);

  int keynum = X11_get_keynum(key_event);

  if (keynum==-1)
    return 0;
  else
    return X11Event_KeyPress(keynum,key_event->state,window);
}


static int X11Event_KeyRelease(int keynum,int keystate,struct Tracker_Windows *window){
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

static int X11_MyKeyRelease(XKeyEvent *key_event,struct Tracker_Windows *window){
  int keynum = X11_get_keynum(key_event);

  if (keynum==-1)
    return 0;
  else

  return X11Event_KeyRelease(keynum,key_event->state,window);
}


extern int num_users_of_keyboard;

bool X11_KeyboardFilter(XEvent *event){

  init_keynums(event);

  if(event->type==KeyPress || event->type==KeyRelease) {
    XKeyEvent *key_event = (XKeyEvent *)event;
    setKeyUpDowns(key_event);
  }

  //static int num=0;
  //printf("Got event %d\n",num++);
  switch(event->type){
  case KeyPress:
    if(num_users_of_keyboard>0)
      return false;

    if(X11_MyKeyPress((XKeyEvent *)event,root->song->tracker_windows)==1){
      //this->quit();
      //doquit = true;
    }
    return true;
  case KeyRelease:
    if(num_users_of_keyboard>0)
      return false;

    X11_MyKeyRelease((XKeyEvent *)event,root->song->tracker_windows);
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
