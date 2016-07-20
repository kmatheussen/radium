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

#include "../common/includepython.h"

#include <signal.h>
#include <unistd.h>

#include <gc.h>

#if USE_QT5
#include <QPluginLoader>
#endif


#include <qapplication.h>
#include <qsplashscreen.h>
#include <qmainwindow.h>
#include <qsplitter.h>
#include <qpalette.h>
#include <qtabwidget.h>
#include <qfontdatabase.h>
#include <QEvent>
#include <Qt>
#include <QDir>
#include <QTextEdit>
#include <QLayout>
#include <QDesktopServices>
#include <QTextCodec>

#include <QStyleFactory>


#ifdef __linux__
#ifndef USE_QT5
#  include <QX11Info>
#endif
#endif

#ifdef USE_QT4
#include <QMainWindow>

//Added by qt3to4:
#include <QEvent>
#ifndef USE_QT5
#  include <QCustomEvent>
#endif
#endif

#include "../common/nsmtracker.h"
#include "../common/threading.h"
#include "../common/disk_load_proc.h"
#include "../common/patch_proc.h"
#include "../common/undo.h"
#include "../common/nag.h"
#include "../common/OS_settings_proc.h"
#include "../common/OS_visual_input.h"
#include "../common/scancodes_proc.h"
#include "../common/player_proc.h"
#include "../common/gfx_wtrackheaders_proc.h"
#include "../common/data_as_text_proc.h"

#include "../api/api_proc.h"

#include "../mixergui/QM_MixerWidget.h"

#include "EditorWidget.h"
#include "Qt_colors_proc.h"
#include "Qt_AutoBackups_proc.h"

#include "../common/eventreciever_proc.h"
#include "../common/control_proc.h"
#include "../common/settings_proc.h"

#include "../common/OS_settings_proc.h"
#include "../common/OS_Player_proc.h"
#include "../common/OS_system_proc.h"

#include "../crashreporter/crashreporter_proc.h"

#include "../audio/Juce_plugins_proc.h"
#include "../audio/Mixer_proc.h"
#include "../audio/MultiCore_proc.h"
#include "../audio/Faust_plugins_proc.h"


#ifdef __linux__
#include <X11/Xlib.h>
//#include "../X11/X11_Bs_edit_proc.h"
//#include "../X11/X11_MidiProperties_proc.h"
//#include "../X11/X11_ClientMessages_proc.h"
#include "../X11/X11_Qtstuff_proc.h"
#endif

#include "../common/OS_Bs_edit_proc.h"
//#include "../X11/X11_Ptask2Mtask_proc.h"
#include "../posix/posix_Player_proc.h"
#include "../common/OS_Ptask2Mtask_proc.h"

#include "Qt_Bs_edit_proc.h"
#include "Qt_instruments_proc.h"
#include "Qt_MainWindow_proc.h"
#include "Qt_Menues_proc.h"

#include "../GTK/GTK_visual_proc.h"

#if 0
#ifdef FOR_WINDOWS
#  include <windows.h>
#  include "../windows/W_Keyboard_proc.h"
#endif
#endif

#include "../OpenGL/Render_proc.h"
#include "../OpenGL/Widget_proc.h"

#include "../embedded_scheme/scheme_proc.h"


#include "Qt_Main_proc.h"


extern bool doquit;
extern struct Root *root;

extern bool g_show_key_codes;

static bool editor_has_keyboard = true;
bool radium_runs_custom_exec = false;

bool g_gc_is_incremental = false;

void obtain_keyboard_focus(void){
  editor_has_keyboard = false;
}

void release_keyboard_focus(void){
  editor_has_keyboard = true;
}

bool editor_has_keyboard_focus(void){
  return editor_has_keyboard;
}


static bool another_window_has_focus = false;

// OSX needs to call this function since sub windows (created by for instance VST plugins) use our key events, and then we can not eat them.
void call_me_if_another_window_may_have_taken_focus_but_still_need_our_key_events(void){
  if (main_window_has_focus())
    another_window_has_focus = false;
  else
    another_window_has_focus = true;

  //printf("main_window_has_focus(): %d\n", main_window_has_focus());
}


DEFINE_ATOMIC(bool, is_starting_up) = true;
bool g_qt_is_running = false;
//void gakk();

extern void set_editor_focus(void);


extern struct TEvent tevent;

static bool g_up_downs[EVENT_DASMAX];

#ifdef FOR_WINDOWS

// W_Keyboars.c updates tevent.keyswitch in it's own way. Has to be that way since we don't register the left windows key the normal way, since that causes the windows menu to appear.
static void set_keyswitch(void){
}

#else
static void set_keyswitch(void){
  static int keynumswitch[]={
    EVENT_CTRL_L, EVENT_SHIFT_L, EVENT_CAPS,
    EVENT_EXTRA_L,EVENT_ALT_L,EVENT_ALT_R,
    EVENT_EXTRA_R, EVENT_CTRL_R, EVENT_SHIFT_R
  };
  
  static int radiumswitch[]={
    EVENT_LEFTCTRL,EVENT_LEFTSHIFT,EVENT_CAPSLOCK,
    EVENT_LEFTEXTRA1,EVENT_LEFTALT,EVENT_RIGHTALT,
    EVENT_RIGHTEXTRA1,EVENT_RIGHTCTRL,EVENT_RIGHTSHIFT
  };
  
  int numswitches = sizeof(keynumswitch)/sizeof(int);
  
  tevent.keyswitch=0;
  
  for(int lokke=0;lokke<numswitches;lokke++){
    int keynum = keynumswitch[lokke];
    //printf("keynum: %d. is_down: %d\n",keynum, g_up_downs[keynum]);
    if(g_up_downs[keynum]){
      tevent.keyswitch |= radiumswitch[lokke];
    }
  }
  
  //printf("keyswtich: %x\n",tevent.keyswitch);
}
#endif

void OS_SYSTEM_ResetKeysUpDowns(void){
  for(int i=0;i<EVENT_DASMAX;i++)
    g_up_downs[i]=false;
  set_keyswitch();
}



#if 0
static bool handle_qt_keyboard(QKeyEvent *event, bool is_key_down){
  int keynum = EVENT_NO;

#define S(QT_VAL, EVENT_VAL) case Qt::Key_##QT_VAL: keynum=EVENT_##EVENT_VAL;break;  
  switch(event->key()){
    S(Escape, ESC);
    S(Tab, TAB);
    S(Backspace, BACKSPACE);
    S(Return, RETURN);
    S(Enter, KP_ENTER);
    S(Insert, INSERT);
    S(Delete, DEL);

    S(Home, HOME);
    S(End, END);
    S(Left, LEFTARROW);
    S(Right, RIGHTARROW);
    S(Down, DOWNARROW);
    S(Up, UPARROW);
    S(PageUp, PAGE_UP);
    S(PageDown, PAGE_DOWN);

    S(F1, F1);
    S(F2, F2);
    S(F3, F3);
    S(F4, F4);
    S(F5, F5);
    S(F6, F6);
    S(F7, F7);
    S(F8, F8);
    S(F9, F9);
    S(F10, F10);
    S(F11, F11);
    S(F12, F12);

    S(0,0);
    S(1,1);
    S(2,2);
    S(3,3);
    S(4,4);
    S(5,5);
    S(6,6);
    S(7,7);
    S(8,8);
    S(9,9);

    S(A,A);
    S(B,B);
    S(C,C);
    S(D,D);
    S(E,E);
    S(F,F);
    S(G,G);
    S(H,H);
    S(I,I);
    S(J,J);
    S(K,K);
    S(L,L);
    S(M,M);
    S(N,N);
    S(O,O);
    S(P,P);
    S(Q,Q);
    S(R,R);
    S(S,S);
    S(T,T);
    S(V,U);
    S(U,V);
    S(W,W);
    S(X,X);
    S(Y,Y);
    S(Z,Z);

    S(Space, SPACE);
    
    S(Menu, MENU);

    S(MediaPlay, PLAY);
    S(MediaStop, STOP);
    S(VolumeDown, VOLUME_DOWN);
    S(VolumeUp, VOLUME_UP);
    S(VolumeMute, MUTE);

    S(Calculator, CALCULATOR);
    S(LaunchMail, MAIL);
    S(HomePage, HOMEPAGE);    
  }
#undef S

#if FOR_LINUX
  const int sub=8;
#else
  const int sub=0;
#endif

  return handle_keyboard(keynum, event->nativeScanCode()-sub, event->nativeVirtualKey(), is_key_down);
}
#endif

extern EditorWidget *g_editor;

#if USE_QT5
#include <QAbstractNativeEventFilter>
#endif

class MyApplication
  : public QApplication
#if USE_QT5
  , public QAbstractNativeEventFilter
#endif
{
public:

  MyApplication(int &argc,char **argv);

protected:

  bool last_key_was_lalt;
  
  bool SystemEventFilter(void *event){

    if(ATOMIC_GET(is_starting_up)==true)
      return false;

    OS_SYSTEM_EventPreHandler(event);

    /*
    QMainWindow *main_window = static_cast<QMainWindow*>(root->song->tracker_windows->os_visual.main_window);

    printf("   focus: %d,   active: %d.  key: %d\n",
           QApplication::focusWidget() != NULL,
           QApplication::activeWindow() != NULL,
           OS_OSX_is_key_window((void*)main_window->winId())
           );
    */
    
    if (another_window_has_focus)
      return false;
    
    struct Tracker_Windows *window = root->song->tracker_windows;

    bool ignore_autorepeat = !doAutoRepeat() && editor_has_keyboard == true;
    
    int type = OS_SYSTEM_get_event_type(event, ignore_autorepeat);

    if (type==TR_AUTOREPEAT)
      return true;
    
    if (type!=TR_KEYBOARD && type!=TR_KEYBOARDUP)
      return false;
    
#if 0 //FOR_LINUX
    return true;
#endif
    
    bool is_key_press = type==TR_KEYBOARD;
    
    int modifier = OS_SYSTEM_get_modifier(event); // Note that OS_SYSTEM_get_modifier is unable to return an EVENT_EXTRA_L event on windows. Not too sure about EVENT_EXTRA_R either (VK_APPS key) (doesn't matter, EVENT_EXTRA_R is abandoned, and the key is just used to configure block). In addition, the release value order might be wrong if pressing several modifier keys, still windows only.

    //printf("modifier: %d\n",modifier);
    if (g_show_key_codes){
      char *message = talloc_format("%d - %d - %d", is_key_press ? 1 : 0, modifier, OS_SYSTEM_get_scancode(event));
      printf("  Got key: %s\n",message);
      window->message=message;
      
      GL_create(window,window->wblock);
    }
              
    static int last_pressed_key = EVENT_NO;

    if (modifier != EVENT_NO) {

      bool must_return_true = false;
      
      if (modifier==EVENT_ALT_L){
        if (is_key_press){
          last_key_was_lalt = true;
        }else {

          // release
          
          must_return_true = true;
          
          if(last_key_was_lalt==true){
            
            if (GFX_MenuVisible(window) && GFX_MenuActive()==true) {
              GFX_HideMenu(window);
              set_editor_focus();
            } else if (!GFX_MenuVisible(window)) {
              GFX_ShowMenu(window);
            }
              
            must_return_true = false; // pass the EVENT_ALT_L event to qt so that we can navigate the menues.
            
            last_key_was_lalt = false;                      
          }

        }
      }else
        last_key_was_lalt = false;

      static double last_pressed_key_time = 0;

      double time_now = TIME_get_ms();

      if (is_key_press) {
        
        last_pressed_key_time = time_now;
        last_pressed_key = modifier;
               
      } else {
        
        // key release:

        if (editor_has_keyboard==true) {
          if( (time_now - last_pressed_key_time) < 1000/4){ // i.e. only play if holding the key less than 0.25 seconds.
            if(modifier==last_pressed_key && modifier==EVENT_ALT_R) {
              PlayBlockFromStart(window,true); // true == do_loop
            }
            
            if(modifier==last_pressed_key && modifier==EVENT_SHIFT_R) {
              PlayBlockFromStart(window,true); // true == do_loop
            }
          }
        }
      }

      g_up_downs[modifier] = is_key_press;
      set_keyswitch();
      //printf("__________________________ Got modifier %s. Returning false\n",is_key_press ? "down" : "up");

      if (modifier==EVENT_ALT_R || must_return_true)
        return true; // If not, Qt starts to navigate the menues.

      return false;
    }

    last_key_was_lalt = false;

#if 0
    printf("is_key_press: %d, keynum: %d, EVENT_MENU: %d\n",is_key_press,keynum,EVENT_MENU);

    if (is_key_press==false && keynum==EVENT_MENU)
      return true; // swallow the general qt menu popup menu. Sometimes it pops up when configuring block. If you need it, just press right mouse button.
#endif
    
    if (editor_has_keyboard==false)
      return false;

    int keynum = OS_SYSTEM_get_keynum(event);

    last_pressed_key = keynum;
            
    //printf("keynum1: %d. switch: %d\n",keynum,tevent.keyswitch);
    
    switch(keynum){
    case EVENT_ESC:
    case EVENT_UPARROW:
    case EVENT_DOWNARROW:
    case EVENT_LEFTARROW:
    case EVENT_RIGHTARROW:
    case EVENT_RETURN:
    case EVENT_KP_ENTER: {
      if(GFX_MenuActive()==true)
        return false;
      break;
    }
    }

    window->must_redraw = true;

    if (is_key_press)
      tevent.ID=TR_KEYBOARD;
    else
      tevent.ID=TR_KEYBOARDUP;
    
    tevent.SubID=keynum;  
        
    
    bool ret;

    bool dat_used_key = DAT_keypress(window, tevent.SubID, is_key_press);

    if (dat_used_key) {
      
      ret = true;

    } else {
      
      if (keynum==EVENT_NO)
        ret = false;
      else
        ret = EventReciever(&tevent,window);
      
      if (ret==false) {
        keynum = OS_SYSTEM_get_qwerty_keynum(event); // e.g. using scancode.
        
        //printf("keynum2: %d. switch: %d\n",keynum,tevent.keyswitch);
        
        if (keynum==EVENT_NO){
          //printf("Unknown key for n%p\n",event);//virtual_key);
          return false;
        }
        
        tevent.SubID=keynum;
        
        ret = EventReciever(&tevent,window);
      }
    }

    //printf("ret2: %d\n",ret);
    
    if(ret==true)
      static_cast<EditorWidget*>(window->os_visual.widget)->updateEditor();
    
    return true;
  }

#ifdef USE_QT5

  virtual bool nativeEventFilter(const QByteArray &eventType, void *message, long *) Q_DECL_OVERRIDE
  {
    //printf("NAtive event filter!\n");
    return SystemEventFilter(message);
  }

#else // USE_QT5
  
#ifdef __linux__
  bool x11EventFilter(XEvent *event) override {
    bool ret = SystemEventFilter(event);
    //printf("         eventfilter ret: %d\n",ret);
    return ret;
  }
#endif

#ifdef FOR_WINDOWS
  bool 	winEventFilter ( MSG * msg, long * result ) override {
    return SystemEventFilter(msg);
  }
#endif

#ifdef FOR_MACOSX
  bool macEventFilter ( EventHandlerCallRef caller, EventRef event ) override {
    return SystemEventFilter(event);
  }
#endif

#endif // !USE_QT5

  /*
  bool event(QEvent *event){
    return QApplication::event(event);
  }
  */
};

MyApplication::MyApplication(int &argc,char **argv)
  : QApplication(argc,argv)
  , last_key_was_lalt(false)
{
  //setStyleSheet("QStatusBar::item { border: 0px solid black }; ");
#if USE_QT5
  installNativeEventFilter(this);
#endif
}


  //QApplication *qapplication;
MyApplication *qapplication = NULL;
QApplication *g_qapplication = NULL;
static QSplashScreen *g_splashscreen;


bool main_window_has_focus(void){
#if FOR_MACOSX
  QMainWindow *main_window = static_cast<QMainWindow*>(root->song->tracker_windows->os_visual.main_window);
  return OS_OSX_is_key_window((void*)main_window->winId());
#else
  //return g_qapplication->focusWidget() != NULL;  
  return g_qapplication->activeWindow() != NULL;
#endif
}


extern "C" void run_main_loop(void);
void run_main_loop(void){
  g_qapplication->exec();
}

#if 1 //USE_QT_VISUAL

#if 0
#if __linux__
static double get_ms(void){
  struct timespec ts;
  clock_gettime(CLOCK_MONOTONIC, &ts);
  return ts.tv_sec * 1000.0 + ((double)ts.tv_nsec) / 1000000.0;
}
#endif
#endif

#if 0
// Should ideally be atomic, but read and write are usually atomic anyway.
static volatile int using_the_event;

class MyQCustomEvent : public QCustomEvent{
public:
  MyQCustomEvent()
    : QCustomEvent(QEvent::User+1)
  {}

  // We don't want to malloc() from the player thread, since it's running SCHED_FIFO.
  void* operator new(size_t size){
    static MyQCustomEvent event;
    // new is always called from the player thread
    using_the_event = 1;
    return &event;
  }
  void operator delete(void *p){
    // delete is always called from the GUI thread
    using_the_event = 0;
  }
};
#endif

// This is now a dummy function
void Ptask2Mtask(void){
#if 0 // Polling instead.
  if(using_the_event==1)
    return;

  // Check that we're not overflowing the Qt Event system.
#if __linux__
  {
    static double last_time = 0.0;
    double time = get_ms();
    //if(time-last_time < 150) // this looks much better! (but it needs to be configurable)
    if(time-last_time < 40)
      return;
    last_time = time;
  }
#endif

  if(1){
    QObject *qobject=(QObject *)root->song->tracker_windows->os_visual.widget;
    MyQCustomEvent *qce = new MyQCustomEvent();

    qapplication->postEvent(qobject,qce);
  }
#endif
}

#endif // USE_QT_VISUAL

#if !GTK_IS_USED

#include <QTimer>
#include <QMessageBox>
#include "../midi/midi_i_input_proc.h"
#include "../common/gfx_proc.h"
#include "../common/gfx_op_queue_proc.h"
#include "../common/player_proc.h"

//#define TEST_GC

#ifdef TEST_GC
#  include "gc.h"
#endif

extern LANGSPEC void P2MUpdateSongPosCallBack(void);

enum RT_MESSAGE_STATUS {
  RT_MESSAGE_READY,
  RT_MESSAGE_FILLING_UP,
  RT_MESSAGE_READY_FOR_SHOWING,
  RT_MESSAGE_SHOWING
};

static DEFINE_ATOMIC(RT_MESSAGE_STATUS, rt_message_status) = RT_MESSAGE_READY;
static const int rt_message_length = 1024;
static char rt_message[rt_message_length];
static DEFINE_ATOMIC(bool, request_to_stop_playing) = false;

class CalledPeriodically : public QTimer {

  QMessageBox msgBox;
  QAbstractButton *msgBox_ok;
  QAbstractButton *msgBox_stop_playing;
  QAbstractButton *msgBox_dontshowagain;
  QSet<QString> dontshow;

  const int interval;
  int64_t num_calls;
  
public:
  CalledPeriodically()
    : interval(20)
    , num_calls(0)
  {
    msgBox.setModal(false);
    msgBox_dontshowagain = (QAbstractButton*)msgBox.addButton("Dont show this message again",QMessageBox::ApplyRole);
    msgBox_stop_playing = (QAbstractButton*)msgBox.addButton("Stop playing!",QMessageBox::ApplyRole);
    msgBox_ok = (QAbstractButton*)msgBox.addButton("Ok",QMessageBox::AcceptRole);
    msgBox.open();
    msgBox.hide();
    
    setInterval(interval);
    start();
  }
protected:

  void 	timerEvent ( QTimerEvent * e ){
#ifdef TEST_GC
    printf("triggering full collect\n");
    GC_gcollect();
#endif
    
    //static int hepp=0; printf("hepp %d\n",hepp++);
    
    if (ATOMIC_GET(rt_message_status) == RT_MESSAGE_READY_FOR_SHOWING) {

      QString message(rt_message);

      if (dontshow.contains(message)==false){
        msgBox.setText(message);
        safeShow(&msgBox);
      }

      ATOMIC_SET(rt_message_status, RT_MESSAGE_SHOWING);
      
    } else if (ATOMIC_GET(rt_message_status) == RT_MESSAGE_SHOWING && msgBox.isHidden()) {

      if (msgBox.clickedButton() == msgBox_dontshowagain){
        //printf("Dontshowagain\n");
        dontshow.insert(rt_message);
      } else if (msgBox.clickedButton() == msgBox_stop_playing){
        PlayStop();
      }
      
      ATOMIC_SET(rt_message_status, RT_MESSAGE_READY);
    }

    num_calls++;

    struct Tracker_Windows *window=root->song->tracker_windows;
    
    if(num_calls<1000/interval){ // Update the screen constantly during the first second. It's a hack to make sure graphics is properly drawn after startup. (dont know what goes wrong)
      window->must_redraw = true;
    }
      
    {
      DO_GFX({
          MIDI_HandleInputMessage();
#if !USE_OPENGL
          TRACKREALLINES_call_very_often(window);
#endif
        });
    }

    //if (qapplication->activeWindow() != NULL)
    //  printf("   active window\n");
    
    // Check if player has shut down
    if (PLAYER_is_running()==false)
      PlayStop();

    if(ATOMIC_GET(request_to_stop_playing) == true) {
      PlayStop();
      ATOMIC_SET(request_to_stop_playing, false);
    }
    
    if(ATOMIC_GET(pc->player_state)==PLAYER_STATE_PLAYING){
      P2MUpdateSongPosCallBack();
      PlayCallVeryOften();
    }

    static_cast<EditorWidget*>(window->os_visual.widget)->updateEditor(); // Calls EditorWidget::updateEditor(), which is a light function    

    if(doquit==true) {
      QApplication::quit();
    }

    PATCH_call_very_often();
    BACKUP_call_very_often();

    if (window->message_duration_left > 0){
      //printf("message dur: %d\n",window->message_duration_left);
      window->message_duration_left -= interval;
      if (window->message_duration_left <= 0){
        window->message_duration_left = 0;
        window->message = NULL;
        GL_create(window, window->wblock);
      }
    }

    const char *gc_warning_message = "Warning: Garbage collector is turned off";

    if (window->message==NULL && GC_is_disabled()) {
      
      window->message = gc_warning_message;
      GL_create(window, window->wblock);
      
    } else if (window->message==gc_warning_message && GC_is_disabled()==false){
      
      window->message=NULL;
      GL_create(window, window->wblock);
      
    }
    
    if ( (num_calls % (5*1000/interval)) == 0) { // Ask for gl.make_current each 5 seconds.
      GL_lock();{
        GL_EnsureMakeCurrentIsCalled();
      }GL_unlock();
    }

    // Ensure editor is redrawn after resizing. For some reason, it doesn't always work to set must_redraw=true in the resize event virtual method.
    {
      static int last_height = -1;
      QMainWindow *main_window = (QMainWindow *)window->os_visual.main_window;
      int new_height = main_window->height();
      if (main_window!=NULL && new_height != last_height){
        window->must_redraw = true;
        last_height = new_height;
      }
    }

    if (GL_maybe_notify_that_main_window_is_exposed(interval)==true)
      GL_maybe_estimate_vblank(static_cast<EditorWidget*>(window->os_visual.widget)->gl_widget);
    
    #if 0
    static bool main_window_is_exposed = false;
    if (main_window_is_exposed==false){
      QMainWindow *main_window = (QMainWindow *)window->os_visual.main_window;
      if (main_window != NULL){
        if (main_window->isExposed()) {

          main_window_is_exposed = true;
        }
      }
    }
    #endif

    
        
    //MIXER_called_regularly_by_main_thread();
    
#if 0
    // Update graphics when playing
    {
      struct Tracker_Windows *window=root->song->tracker_windows;
      static_cast<EditorWidget*>(window->os_visual.widget)->callCustomEvent();
    }
#endif
  }
};

void RT_message(const char *fmt,...){
  va_list argp;

  if (!atomic_compare_and_set_int((int*)&ATOMIC_NAME(rt_message_status), RT_MESSAGE_READY, RT_MESSAGE_FILLING_UP))
    return;
  
  va_start(argp,fmt);
  /*	vfprintf(stderr,fmt,argp); */
  vsnprintf(rt_message,rt_message_length-1,fmt,argp);
  va_end(argp);

  ATOMIC_SET(rt_message_status, RT_MESSAGE_READY_FOR_SHOWING);
}

void RT_request_to_stop_playing(void){
  ATOMIC_SET(request_to_stop_playing, true);
}

#endif


int GFX_ResizeWindow(struct Tracker_Windows *tvisual,int x,int y){return 0;}

bool CtrlPressed(void){
  return QApplication::keyboardModifiers() & Qt::ControlModifier;
}

bool ShiftPressed(void){
  return QApplication::keyboardModifiers() & Qt::ShiftModifier;
}

bool AltPressed(void){
  return QApplication::keyboardModifiers() & Qt::AltModifier;
}

void SetNormalPointer(struct Tracker_Windows *tvisual){
  QMainWindow *main_window = (QMainWindow *)tvisual->os_visual.main_window;
  main_window->setCursor(Qt::ArrowCursor);
}
void SetPointingPointer(struct Tracker_Windows *tvisual){
  QMainWindow *main_window = (QMainWindow *)tvisual->os_visual.main_window;
  main_window->setCursor(Qt::PointingHandCursor);
}
void SetBlankPointer(struct Tracker_Windows *tvisual){
  QMainWindow *main_window = (QMainWindow *)tvisual->os_visual.main_window;
  main_window->setCursor(Qt::BlankCursor);
}
void SetDiagResizePointer(struct Tracker_Windows *tvisual){
  QMainWindow *main_window = (QMainWindow *)tvisual->os_visual.main_window;
  main_window->setCursor(Qt::SizeFDiagCursor);
}
void SetHorizResizePointer(struct Tracker_Windows *tvisual){
  QMainWindow *main_window = (QMainWindow *)tvisual->os_visual.main_window;
  main_window->setCursor(Qt::SizeHorCursor);
}
void SetVerticalResizePointer(struct Tracker_Windows *tvisual){
  QMainWindow *main_window = (QMainWindow *)tvisual->os_visual.main_window;
  main_window->setCursor(Qt::SizeVerCursor);
}
void MovePointer(struct Tracker_Windows *tvisual, float x, float y){
  EditorWidget *editor=(EditorWidget *)tvisual->os_visual.widget;
  QCursor::setPos(editor->mapToGlobal(QPoint(x,y)));
}

WPoint GetPointerPos(struct Tracker_Windows *tvisual){
  WPoint ret;
  EditorWidget *editor=(EditorWidget *)tvisual->os_visual.widget;
  QPoint pos = editor->mapFromGlobal(QCursor::pos());
  ret.x = pos.x();
  ret.y = pos.y();
  return ret;
}

void GFX_toggleFullScreen(struct Tracker_Windows *tvisual){
#if defined(FOR_MACOSX) && !defined(USE_QT5)
  GFX_Message(NULL, "Full screen not supported in OSX");
#else
  QMainWindow *main_window = (QMainWindow *)tvisual->os_visual.main_window;

  if(main_window->isFullScreen()){
    main_window->showNormal();
  }else{
    main_window->showFullScreen();
  }
#endif
}



void GFX_EditorWindowToFront(struct Tracker_Windows *tvisual){
  QMainWindow *main_window=static_cast<QMainWindow*>(tvisual->os_visual.main_window);

  //GFX_PlayListWindowToBack();
  main_window->raise();

#ifdef __linux__
  #ifndef USE_QT5
    XSetInputFocus(QX11Info::display(),(Window)QX11Info::appRootWindow(),RevertToNone,CurrentTime);
  #endif
#endif

  OS_SYSTEM_ResetKeysUpDowns();
}


void assertRadiumInHomeDirectory(void){
  QString program_path = QCoreApplication::applicationDirPath();

#if 0
  QString home_path = QDesktopServices::storageLocation(QDesktopServices::HomeLocation);

  if (!program_path.startsWith(home_path)){
    GFX_Message(NULL,
                QString("Warning!\n\n") +
                "Radium is not installed in your home directory. Unless you have write access to the directory \"" + program_path + "\", undefined behaviors are likely to happen"
                );
  }
#else
  QFile file(program_path + "/checking_write_permission.txt");

  bool success = file.open(QIODevice::WriteOnly);

  if (success)
    success=file.isOpen();

  if (success)
    success = file.write("hello",2)>0;

  if (file.isOpen())
    file.close();
  
  //  QFileInfo info(program_path + QDir::separator() + "eventreceiverparser_generated.py");
  //if (!info.isWritable())

  if (!success)
    GFX_Message(NULL,
                (
                 QString("Warning!\n\n") +
                 "Radium is installed in a directory without write access. (" + program_path + ")\n"
                 "Undefined behaviors may happen"
                 ).toUtf8().constData()
                );
#endif
}



//#include <qwindowsstyle.h>
//#include <qmacstyle_mac.h>
#if 0
#include <qplatinumstyle.h>
#include <qcdestyle.h>
#include <qmotifplusstyle.h>
#include <qsgistyle.h>
#include <gtkstyle.h>
#endif

#if USE_QT4
//#include <QCleanlooksStyle>
//#include <QOxygenStyle>
#ifndef USE_QT5
#  include <QPlastiqueStyle>
#endif
#endif

#ifdef USE_QT3
#include "qwidget.h"
#include "qlistbox.h"
#endif

//#include "google/profiler.h"

//extern LANGSPEC int dasmain(int argc,char **argv);
extern LANGSPEC int radium_main(char *arg);
//extern LANGSPEC int GC_dont_gc;
//int radium_main(int argc,char **argv){

// Called from gtk main loop
void Qt_EventHandler(void){
#if 1 // At least on windows, qapplication->hasPendingEvents() never returns false.
  qapplication->processEvents();
#else
  while(qapplication->hasPendingEvents() && doquit==false)
    qapplication->processEvents();
#endif
#if 0
  QMainWindow *main_window = static_cast<QMainWindow*>(root->song->tracker_windows->os_visual.main_window);
  if(main_window->isVisible()==false)
    doquit=true;
#endif
}


//extern void updateAllFonts(QWidget *widget);

static bool load_new_song=true;

extern void TIME_init(void);
extern void UPDATECHECKER_doit(void);

int radium_main(char *arg){

  TIME_init();

#if 0
  QApplication::setStyle( new QPlatinumStyle() );
  QApplication::setStyle( new QCDEStyle() );
  QApplication::setStyle( new QMotifPlusStyle() );
  QApplication::setStyle( new QSGIStyle() );
#endif
  {
    bool override_default_qt_style = SETTINGS_read_bool((char*)"override_default_qt_style",true);
    SETTINGS_write_bool((char*)"override_default_qt_style",override_default_qt_style);

#if 1
    if(override_default_qt_style){
      //QApplication::setStyle( new QOxygenStyle());

#ifdef USE_QT5

#if 0
        QString styles_path = QCoreApplication::applicationDirPath() + QDir::separator()
          + "packages" + QDir::separator()
          + "qtstyleplugins-src-5.0.0" + QDir::separator()
          + "plugins" + QDir::separator()
          + "styles";

        // Did not work. (???) Created a symbolic link to the plugin file instead.
        QCoreApplication::addLibraryPath(styles_path);

        // Did not work either. (???) Created a symbolic link to the plugin file instead.   
        QPluginLoader *gakk = new QPluginLoader(styles_path + QDir::separator()
#ifdef FOR_WINDOWS
                                                + "qplastiquestyle.dll"
#else
                                                + "libqplastiquestyle"
#endif
                                                );

        // This call succeeds, but the call to QStyleFactory::create("plastique") fails no matter what I do, except putting libqplastiquestyle.so directly into bin/styles/
        if (gakk->load()==false)
          GFX_Message(NULL, "Unable to load style library. Ensure Radium is installed properly.");
#endif // 0

        //QStyle *style = QStyleFactory::create("plastique");
        QStyle *style = QStyleFactory::create("fusion");
        if (style==NULL)
          GFX_Message(NULL, "Unable to load fusion style");
        else
          QApplication::setStyle(style);
        
#else // USE_QT5
        
        QApplication::setStyle( new QPlastiqueStyle());
        
#endif
      //QApplication::setStyle( new QMacStyle());
    
    //QApplication::setStyle( new QCleanlooksStyle() );
    //QApplication::setStyle( new QWindowsStyle() );
    }
#endif
  }

  QApplication::setEffectEnabled(Qt::UI_AnimateMenu,true);
  QApplication::setEffectEnabled(Qt::UI_AnimateCombo,true);

  //QApplication::setGraphicsSystem("native");
  //QApplication::setGraphicsSystem("raster");


  setApplicationColors(qapplication);

  g_qt_is_running = true;

#if 0
    vector_t v = {0};
  VECTOR_push_back(&v,"hepp1");
  VECTOR_push_back(&v,"hepp2");
  VECTOR_push_back(&v,"hepp3");
  GFX_Message(&v, "hepp hepp");
#endif


  OS_SYSTEM_init_keyboard();

  SetupMainWindow();


  //X11_StartBlockSelector();
  //X11_StartMidiProperties();

  //StartGuiThread();

  // ProfilerStart("hepps");

  SCHEME_start();

  printf("starting\n");
  if(InitProgram()==false)
    return 0;
  printf("ending\n");

  //ProfilerStop();

  posix_InitPlayer();

#ifdef __linux__
  X11_StartQtStuff();
#endif

  QWidget *block_selector = create_blockselector();

  BS_UpdateBlockList();
  BS_UpdatePlayList();
  BS_SelectBlock(root->song->blocks);
  BS_SelectPlaylistPos(0);

  struct Tracker_Windows *window = root->song->tracker_windows;

  QMainWindow *main_window = static_cast<QMainWindow*>(window->os_visual.main_window);

  {
    EditorWidget *editor = static_cast<EditorWidget*>(window->os_visual.widget);

    {
      QSplitter *xsplitter = new QSplitter(Qt::Horizontal);//, main_window);
      editor->xsplitter = xsplitter;

      xsplitter->setOpaqueResize(true);

#if USE_QT3
      // Fix. Why does this crash QT4?
      editor->reparent(xsplitter, QPoint(0,0), true);
#endif
      //xsplitter->show();
      editor->setParent(xsplitter); //, QPoint(0,0), false);

      block_selector->setParent(xsplitter);//, QPoint(main_window->width()-100,0), true);
      block_selector->move(main_window->width()-100,0);

      block_selector->resize(100,block_selector->height());

      if(1){
        QSplitter *ysplitter = new QSplitter(Qt::Vertical, main_window);
        editor->ysplitter = ysplitter;
        ysplitter->setOpaqueResize(true);
        

        QWidget *instruments = createInstrumentsWidget();

        xsplitter->setParent(ysplitter); //, QPoint(0,0), true);
        instruments->setParent(ysplitter); //, QPoint(0, main_window->height()-220), true);
        instruments->move(0, main_window->height()-220);
    
        main_window->setCentralWidget(ysplitter);

        ysplitter->setStretchFactor(0,100000);
        ysplitter->setStretchFactor(1,0);
        ysplitter->handle(1)->setEnabled(false);

        qApp->setStyleSheet("QSplitter::handle{background-color: " + get_qcolor(HIGH_BACKGROUND_COLOR_NUM).dark(110).name() + ";}"); 

      } else {
        QWidget *w = new QWidget(main_window);

        QVBoxLayout *layout = new QVBoxLayout(0);
        w->setLayout(layout);

        QWidget *instruments = createInstrumentsWidget();
        instruments->layout()->setSpacing(0);
        //xsplitter->layout()->setSpacing(0);

        instruments->setMinimumHeight(instruments->height() + 10);

        layout->addWidget(xsplitter);
        layout->addWidget(instruments);
        

        layout->setStretch(0,10000);
        layout->setStretch(1,0);


        QLabel *label = new QLabel("hello",main_window);
        layout->addWidget(label);
        layout->setStretch(0,10000);
        layout->setStretch(1,0);
        layout->setStretch(2,0);

        //xsplitter->reparent(w, QPoint(0,0), true);
        //instruments->reparent(w, QPoint(0, main_window->height()-100), true);

        main_window->setCentralWidget(w);

      }

      //MixerWidget *mixer_widget = 
      new MixerWidget(xsplitter);
      //mixer_widget->resize(300,mixer_widget->height());

    }

    //qapplication->setMainWidget(main_window);
    //GFX_PlayListWindowToFront(); // To provoce setting width to 'blocklist_width'
    main_window->show();
  }

  PyRun_SimpleString("import menues");

  //QFontDatabase::addApplicationFont("/gammelhd/usr/share/fonts/liberation/LiberationMono-Regular.ttf");

  ResetUndo();
  if(load_new_song==true)
    NewSong_CurrPos(window);

  //updateAllFonts(QApplication::mainWidget());

  main_window->repaint();
  //DrawUpTrackerWindow(window);
  

  EditorWidget *editor = static_cast<EditorWidget*>(window->os_visual.widget);

#if USE_OPENGL
  editor->gl_widget = GL_create_widget(editor);
  //editor->gl_widget->setAttribute(Qt::WA_PaintOnScreen);
  editor->position_gl_widget(window);
#endif


  //QApplication::quit();

#if !GTK_IS_USED
  CalledPeriodically periodic_timer;
#endif

  window->must_redraw = true;

  //printf("col: -%s-, font: -%s-\n",SETTINGS_read_string("last_color_version","0.0"),SETTINGS_read_string("last_system_font_version","0.0"));

  
  if(strcmp(SETTINGS_read_string("last_color_version","0.0"),"3.7.2")){
    GFX_Message(NULL,
                "Note!\n\n"
                "The default colors have changed. In case you have run Radium before, you might want to go to the Edit menu and select \"Set Default Colors\".\n"
                "\n"
                "You will only see this message once.");
    SETTINGS_write_string("last_color_version","3.7.2");
  }

  if(strcmp(SETTINGS_read_string("last_system_font_version","0.0"),"1.9.21")){
    GFX_Message(NULL,
                "Note!\n\n"
                "The default system font has changed. In case you have run Radium before, you might want to go to the Edit menu and select \"Set Default System Font\".\n\n"
                "You will only see this message once.");
    SETTINGS_write_string("last_system_font_version","1.9.21");
  }

  if(strcmp(SETTINGS_read_string("last_editor_font_version","0.0"),"3.0.b2.3")){
    GFX_Message(NULL,
                "Note!\n\n"
                "The default editor font has changed. In case you have run Radium before, you might want to go to the Edit menu and select \"Set Default Editor Font\".\n\n"
                "You will only see this message once.");
    SETTINGS_write_string("last_editor_font_version","3.0.b2.3");
  }


  INIT_Pianoroll_headers();

  ATOMIC_SET(is_starting_up, false);

  window->must_redraw = true;
  editor->update();
  editor->resize(editor->width(),editor->height());

#if USE_OPENGL
  GL_create(window, window->wblock);
#endif

  // Hack to make Qt text input widgets not crash the program when using intel gfx driver and running opengl in separate thread (crash caused by opening two opengl contexts simultaneously from two threads). (strange stuff)
  GL_lock();
  //GL_draw_lock(); // <-- This prevents some crashes in buggy gfx drivers, but it could also cause a deadlock (not sure).
  {
    QTextEdit e;
    e.show();
    e.setFocus();
    qApp->processEvents();
  }
  //GL_draw_unlock();
  GL_unlock();

#if 0
  while(1){
    qApp->processEvents();
    usleep(500000);
  }
#endif

#if 0
  vector_t v = {0};
  VECTOR_push_back(&v,"hepp1");
  VECTOR_push_back(&v,"hepp2");
  VECTOR_push_back(&v,"hepp3");
  GFX_Message(&v, "hepp hepp");
#endif

  //abort();

  //RWarning("warning!");
  //g_splashscreen->finish(main_window);
  delete g_splashscreen;

  
  show_nag_window("");


  assertRadiumInHomeDirectory();


  UPDATECHECKER_doit();
  
  //QApplication::processEvents(); // Windows spends some time to initialize proxy, or something like that (there are numerous QTBUG entries on this). We trigger that work here while the splash screen is still open. If not it seems like the program have hanged right after startup. (No, it didnt make a difference. Qt has screwed up network initialization on windows since it blocks the main thread for a few seconds when doing the first request. Qt should have done this in a separate thread. Seems like the simplest solution is to use libcurl.)
    
  
       
#if USE_QT_VISUAL
  qapplication->exec();
#else
  GTK_MainLoop();
#endif
      
  // We don't want the crashreporter to pop up if there is something wrong when program exits. Not so important, and it looks unprofessional.
  CRASHREPORTER_dont_report_more();

  DISK_cleanup();
      
  fprintf(stderr,"          ENDING 1\n");
  
  g_qt_is_running = false;

  if (editor->gl_widget != NULL)
    GL_stop_widget(editor->gl_widget);

  fprintf(stderr,"          ENDING 2\n");
  
#if 0
  while(doquit==false){
    while(GTK_HasPendingEvents() || qapplication->hasPendingEvents()){
      GTK_HandleEvents();
      qapplication->processEvents();
    }
    usleep(1000);
  }
#endif

  Undo_start_ignoring_undo_operations();{
    MW_cleanup(false); // Stop all sound properly. Don't want clicks when exiting.
  }Undo_stop_ignoring_undo_operations();

  fprintf(stderr,"          ENDING 3\n");
    
  ATOMIC_SET(is_starting_up, true); // Tell the mixer that program is not running
  usleep(3000); // wait a little bit so the player gets back to the main loop
  
  EndProgram(); // shut down most of the program, except audio

  fprintf(stderr,"          ENDING 4\n");
    
  posix_EndPlayer();
  //EndGuiThread();

  fprintf(stderr,"          ENDING 5\n");
  
  MIXER_stop();

  fprintf(stderr,"          ENDING 6\n");
    
  MULTICORE_shut_down();

  fprintf(stderr,"          ENDING 7\n");
  
#ifdef WITH_FAUST_DEV
  FFF_shut_down();
#endif

  fprintf(stderr,"          ENDING 8\n");

  //V_shutdown();
  
  //CRASHREPORTER_close();

  // Give various stuff some time to exit
  OS_WaitForAShortTime(100);

  fprintf(stderr,"          ENDING 9\n");
  
  return 0;

}

extern "C" {
  static void finish(int sig){
    fprintf(stderr,"FINISH!\n");
    QApplication::quit();
  }
#if 0
  static void crash(int sig){
    fprintf(stderr,"CRASH!!! %d\n",sig);
    abort();
  }
#endif
  extern void initradium(void);
}


// based on qglobal::qunsetenv from the qt 5 source
#ifndef USE_QT5

static void qunsetenv(const char *varName)
{
#if defined(_MSC_VER) && _MSC_VER >= 1400
    _putenv_s(varName, "") == 0;
#else

#if (defined(_POSIX_VERSION) && (_POSIX_VERSION-0) >= 200112L) || defined(Q_OS_BSD4) || defined(Q_OS_HAIKU)
    // POSIX.1-2001 and BSD have unsetenv
    unsetenv(varName);
#endif
    
#if FOR_WINDOWS
    {
      // On mingw, putenv("var=") removes "var" from the environment
      QByteArray buffer(varName);
      buffer += '=';
      putenv(buffer.constData());
    }
#endif
    
    {
      // Fallback to putenv("var=") which will insert an empty var into the
      // environment and leak it
      QByteArray buffer(varName);
      buffer += '=';
      char *envVar = qstrdup(buffer.constData());
      putenv(envVar);
    }
#endif
}

#endif

#ifdef USE_QT5
void myMessageOutput(QtMsgType type, const QMessageLogContext &context, const QString &msg)
#else
  void myMessageOutput(QtMsgType type, const char *localMsg)
#endif

{

#ifdef USE_QT5
  QByteArray localMsg = msg.toLocal8Bit();
#endif
  
  g_qt_is_running=false;
    
  switch (type) {
    case QtDebugMsg:
      fprintf(stderr, "Debug: %s (%s:%u, %s)\n", localMsg.constData(), context.file, context.line, context.function);
      break;
#if QT_VERSION >= 0x050500
    case QtInfoMsg:
      fprintf(stderr, "Info: %s (%s:%u, %s)\n", localMsg.constData(), context.file, context.line, context.function);
      break;
#endif
    case QtWarningMsg:
      fprintf(stderr, "Warning: %s (%s:%u, %s)\n", localMsg.constData(), context.file, context.line, context.function);
#ifndef RELEASE
      GFX_Message(NULL, "Warning: %s (%s:%u, %s)\n", localMsg.constData(), context.file, context.line, context.function);
#endif
      break;
    case QtCriticalMsg:
      fprintf(stderr, "Critical: %s (%s:%u, %s)\n", localMsg.constData(), context.file, context.line, context.function);
#ifndef RELEASE
      GFX_Message(NULL, "Critical: %s (%s:%u, %s)\n", localMsg.constData(), context.file, context.line, context.function);
#endif
      break;
    case QtFatalMsg:
      fprintf(stderr, "Fatal: %s (%s:%u, %s)\n", localMsg.constData(), context.file, context.line, context.function);
#ifndef RELEASE
      GFX_Message(NULL, "Fatal: %s (%s:%u, %s)\n", localMsg.constData(), context.file, context.line, context.function);
#endif        
      break;
      //abort();
    default:
      fprintf(stderr, "Unkwon qt: %s (%s:%u, %s)\n", localMsg.constData(), context.file, context.line, context.function);
  }

  g_qt_is_running=true;
}
 
void MONOTONIC_TIMER_init(void);


#if FOR_MACOSX
#include <mach-o/dyld.h> 
static char *get_macosx_executable_path(void){
  uint32_t size = 1024;
  
  char *ret = (char*)malloc(size);
  
  if (_NSGetExecutablePath(ret, &size) == -1) {
    size++;
    free(ret);
    ret = (char*)malloc(size);
    R_ASSERT(_NSGetExecutablePath(ret, &size) != -1);
  }
  
  return ret;
}
#endif


static char g_static_char;
static char g_static_char2 = 5;

char g_char;
char g_char2 = 6;

static bool char_inside(char *a, char *b, char*c){
  if (b >= a && b < c)
    return true;
  else
    return false;
}


#if defined(FOR_WINDOWS)

#include <dbghelp.h>

// https://msdn.microsoft.com/en-us/library/windows/desktop/ms680341(v=vs.85).aspx
// http://www.csn.ul.ie/~caolan/pub/winresdump/winresdump/doc/pefile2.html
static void add_windows_gc_roots(void){

  static char l_static_char;
  static char l_static_char2 = 7;
  
  HMODULE module_handle = GetModuleHandle(NULL);
  
  char *image_base = (char*)module_handle;
  
  IMAGE_NT_HEADERS *image_nt_header = ImageNtHeader(module_handle);
  
  IMAGE_SECTION_HEADER *image_section_headers = (IMAGE_SECTION_HEADER *) (image_nt_header + 1);
  
  int total_bytes = 0;
  
  for ( int i = 0 ; i < image_nt_header->FileHeader.NumberOfSections ; i++) {

    char name[16] = {};
    strncpy(name, (char*)image_section_headers[i].Name, 8);
    
    char *start = image_base + image_section_headers[i].VirtualAddress;
    int size = image_section_headers[i].Misc.VirtualSize;
    char *end = start + size;

    bool in1 = char_inside(start,&g_static_char,end);
    bool in2 = char_inside(start,&g_static_char2,end);
    bool in3 = char_inside(start,&g_char,end);
    bool in4 = char_inside(start,&g_char2,end);
    bool in5 = char_inside(start,&l_static_char,end);
    bool in6 = char_inside(start,&l_static_char2,end);
    bool is_inside = in1 || in2 || in3 || in4 || in5 || in6;

    bool writable = image_section_headers[i].Characteristics & IMAGE_SCN_MEM_WRITE;
    
    if (writable || is_inside) {
      GC_add_roots(start, end);
      total_bytes += size;
    }
#if !defined(RELEASE)
    if (is_inside)
      R_ASSERT(writable);
#endif
    
    printf("\"%s\". Writable: %d. Size: %d, start: %p, inside: %d %d %d %d %d %d\n", name, writable, size, start, in1,in2,in3,in4,in5,in6);
  }
    
  printf("finished. Total roots added: %fMb\n", (double)total_bytes / (1024*1024.0));
  //getchar();
}

#endif // defined(FOR_WINDOWS)


#if defined(FOR_MACOSX) || defined(FOR_LINUX) // Doesn't seem like the GC_has_static_roots callback is used in windows.
static int gc_has_static_roots_func(
                                     const char * dlpi_name,
                                     void * p,
                                     size_t size
                                     )
{
  static char l_static_char;
  static char l_static_char2 = 7;
  
  char *start = (char*)p;
  char *end = start + size;

#if FOR_MACOSX
  static char *executable_path = get_macosx_executable_path();
#endif

  bool in1 = char_inside(start,&g_static_char,end);
  bool in2 = char_inside(start,&g_static_char2,end);
  bool in3 = char_inside(start,&g_char,end);
  bool in4 = char_inside(start,&g_char2,end);
  bool in5 = char_inside(start,&l_static_char,end);
  bool in6 = char_inside(start,&l_static_char2,end);
  bool is_inside = in1 || in2 || in3 || in4 || in5 || in6;
  
  bool is_main_root;

  if (is_inside) // Normally, this should(?) be good enough. But we have extra checks below as well.
    is_main_root = true;
#if defined(FOR_LINUX)
  else if (!strcmp("PT_GNU_RELRO", dlpi_name))
    is_main_root = false;
  else if (!strcmp("", dlpi_name)) // This is a bit flaky. Haven't found any way any other way to identify the main name on linux. Note that there are two assertion in !RELEASE mode for this check below.
    is_main_root = true;
  else if (dlpi_name[0] != '/') // Add this check as well, might be a useless check.
    is_main_root = true;
#elif defined(FOR_MACOSX)
  else if (!strcmp(executable_path, dlpi_name)) // This should not be flaky
    is_main_root = true;
#endif
  else
    is_main_root = false;
  
#if !defined(RELEASE)
  static int total = 0;

  #if FOR_LINUX
    if (is_inside && strcmp("", dlpi_name)){
      fprintf(stderr, "1. start: %p, static: %p, end: %p, size: %d\n",start,&g_static_char,end,(int)size);
      abort();
    }
    if (!is_inside && !strcmp("", dlpi_name)){
      fprintf(stderr, "2. start: %p, static: %p, end: %p, size: %d\n",start,&g_static_char,end,(int)size);
      abort();
    }
  #endif
  
  if (is_main_root)
    total = size;
  else if (!strcmp("PT_GNU_RELRO", dlpi_name))
    total -= size;
  else
    total += size;

  
  

  #if !defined(FOR_MACOSX)
    const char *executable_path = "";
  #endif
  
  printf("   ===== has_static_roots: -%s-, %fMB (%f). is_main: %d.  (%p). argv0: -%s-\n", dlpi_name, (double)total / (1024*1024.0), (double)size / (1024*1024.0), is_main_root, p, executable_path);
  //getchar();
  //abort();
  
#endif // !defined(RELEASE)
  
  if (is_main_root)
    return 1;
  else
    return 0;
}
#endif // defined(FOR_MACOSX) || defined(FOR_LINUX)

void processEventsALittleBit(void){
  QCoreApplication::processEvents(QEventLoop::ExcludeUserInputEvents, 1);
}

int main(int argc, char **argv){  

#if defined(FOR_WINDOWS)
  GC_set_no_dls(1);
#endif
  
#if defined(FOR_MACOSX) || defined(FOR_LINUX)
  GC_register_has_static_roots_callback(gc_has_static_roots_func);
#endif
  
  GC_INIT();

#if defined(FOR_WINDOWS)
  add_windows_gc_roots();
#endif
  

#ifdef FOR_MACOSX
  OS_OSX_show_icon_in_dock();
#endif
  
  THREADING_init_main_thread_type();

#ifdef USE_QT5
  qInstallMessageHandler(myMessageOutput);
#else
  qInstallMsgHandler(myMessageOutput);
#endif
  
  QCoreApplication::setLibraryPaths(QStringList());  

  QCoreApplication::setAttribute(Qt::AA_X11InitThreads);

#ifndef USE_QT5
  QTextCodec::setCodecForCStrings(QTextCodec::codecForName("UTF-8"));
#endif
  
  MONOTONIC_TIMER_init();
  
  PLUGINHOST_init();
  
  //signal(SIGSEGV,crash);
  //signal(SIGFPE,crash);

  // http://stackoverflow.com/questions/27982443/qnetworkaccessmanager-crash-related-to-ssl
  qunsetenv("OPENSSL_CONF");

#if !defined(FOR_WINDOWS)
  setenv("LC_NUMERIC", "C", 1); // Qt insists on doing strange things with locale settings, causing commans to appear instead of punctation. In an ideal world, LC_NUMERIC/LANG should never be set to anything else than "C", but unfortunately, many computers runs with uncommon language settings such as french or swedish. By default, programs seems to respect the sane behaviour (in the programming world), namely to never use commas when converting between strings and floats, but Qt does something strange with the world inside the QApplication contructor, and causes commas to be used everywhere if there is an uncommon LC_NUMERIC settings (or uncommon LANG setting). This setenv call is the only way I was able to make Pd work, without modifying Pd itself. (I modified Pd too though, but kept this line to prevent similar errors to appear in other libraries.) This behaviour should be changed in Qt.)
#endif

  // for mingw
  putenv(V_strdup("LC_NUMERIC=C"));
  
  //QLocale::setDefault(QLocale::C);
  QLocale::setDefault(QLocale::c());

#if 0
  printf("argv0: \"%s\"\n",argv[0]);
  return 0;
#endif

  QApplication::setDesktopSettingsAware(false);

  QLocale::setDefault(QLocale::C);

  // Create application here in order to get default style. (not recommended, but can't find another way)
  qapplication=new MyApplication(argc,argv);
  qapplication->setAttribute(Qt::AA_MacDontSwapCtrlAndMeta, true);

#if 0
 #if defined(IS_LINUX_BINARY) || defined(FOR_WINDOWS) || defined(FOR_MACOSX)
    QApplication::addLibraryPath(QCoreApplication::applicationDirPath() + QDir::separator() + "qt5_plugins");
  #endif
#endif
  
  g_qapplication = qapplication;

  OS_set_argv0(argv[0]);

  R_ASSERT(THREADING_is_main_thread());

  CRASHREPORTER_init();

  bool try_incremental_gc = SETTINGS_read_bool("try_incremental_gc",false);
  if (try_incremental_gc || SETTINGS_read_bool("incremental_gc",false)) {
    if (try_incremental_gc)
      SETTINGS_write_bool("try_incremental_gc",false); // Set back before calling 'GC_enable_incremental' in case 'GC_enable_incremental' crashes.
    
#if defined(RELEASE) // incremental crashes under gdb
    GC_enable_incremental();
#endif
    
    g_gc_is_incremental = true;
  }
  
  
  //GC_disable();
  
  QPixmap pixmap(OS_get_full_program_file_path("radium_256x256x32.png"));
  g_splashscreen = new QSplashScreen(pixmap);
#if 1 //def RELEASE
  g_splashscreen->show();
  g_splashscreen->raise();
  g_splashscreen->showMessage("Starting up");
  QApplication::processEvents();
#endif
  
  printf("1: argv[0]: \"%s\"\n",argv[0]);

#if 0
  {
    int i=0;
    int k=5;
    printf("%d\n",k/i);
    sscanf("12345", "%i", (int *) (k=i));
  }
#endif

  if(argc>1 && !strcmp(argv[1],"--dont-load-new-song"))
    load_new_song=false;

#if defined(IS_LINUX_BINARY)
#if 0  
  setenv("PYTHONHOME","temp/dist",1);
  setenv("PYTHONPATH","temp/dist",1);
#else
  QString pythonlibpath = OS_get_full_program_file_path(QString("python2.7/lib"));
  setenv("PYTHONHOME",V_strdup(pythonlibpath.toUtf8().constData()),1);
  setenv("PYTHONPATH",V_strdup(pythonlibpath.toUtf8().constData()),1);
#endif
#endif

#if defined(FOR_MACOSX)
  QString pythonlibpath = OS_get_full_program_file_path(QString("python2.7/lib"));
  setenv("PYTHONHOME",V_strdup(pythonlibpath.toUtf8().constData()),1);
  setenv("PYTHONPATH",V_strdup(pythonlibpath.toUtf8().constData()),1);
#endif
  
#if defined(FOR_WINDOWS)
#if __WIN64
  //QString pythonlibpath = QCoreApplication::applicationDirPath() + QDir::separator() + "python2.7" + QDir::separator() + "lib"; // + QDir::separator() + "lib" + QDir::separator() + "python2.7";
  QString pythonlibpath = OS_get_full_program_file_path("python2.7"); // + QDir::separator() + "lib" + QDir::separator() + "python2.7";
  //putenv(strdup(QString("PYTHONHOME="+pythonlibpath).toUtf8().constData()));
  //putenv(strdup(QString("PYTHONPATH="+pythonlibpath).toUtf8().constData()));
  printf("pythonlibpath: -%s-\n",pythonlibpath.toUtf8().constData());
  Py_SetPythonHome(V_strdup(pythonlibpath.toUtf8().constData()));
#endif
#endif
  //Py_SetProgramName(QString(python

  Py_Initialize();
  
  {
    char temp[500];

    // Set loading path to argv[0]
    PyRun_SimpleString("import sys");

    PyRun_SimpleString("import os");
        
#if 1
    //#if defined(FOR_WINDOWS)
    sprintf(temp,"sys.g_program_path = \"\"");
#else
    // This doesn't work on mingw. Could be a wine problem only.
    sprintf(temp,"sys.g_program_path = os.path.abspath(os.path.dirname(\"%s\"))",argv[0]);
#endif
    PyRun_SimpleString(temp);

    PyRun_SimpleString("print \"hepp:\",sys.g_program_path,23");
    
    PyRun_SimpleString("sys.path = [sys.g_program_path] + sys.path");
    //PyRun_SimpleString("sys.path = [sys.g_program_path]");
    
    // Set sys.argv[0]
    sprintf(temp,"sys.argv=[\"%s\",\"%s\", \"%s\"]",
            argv[0],
            OS_get_keybindings_conf_filename().replace("\\","\\\\").toUtf8().constData(),
            OS_get_custom_keybindings_conf_filename().replace("\\","\\\\").toUtf8().constData()
            );
    PyRun_SimpleString(temp);
    
    printf("argv[0]: %s\n",argv[0]);
    PyRun_SimpleString("print \"path:\",sys.g_program_path,239");
    
    //exit(0);
  }

  qapplication->setWindowIcon(QIcon(OS_get_full_program_file_path("radium_256x256x32.png")));

  {
    // Add fonts in the "fonts" directory
    {
      QDir dir(OS_get_full_program_file_path("fonts"));
      QFileInfoList list = dir.entryInfoList(QDir::AllEntries|QDir::NoDotAndDotDot);
      for (int i=0;i<list.count();i++){
        QFileInfo file_info = list[i];
        
        QString file_path = file_info.filePath();
        if(file_info.suffix()=="ttf"){
          //printf("file_path: %s\n",file_path.toUtf8().constData());
          QFontDatabase::addApplicationFont(file_path);
        }
      }
    }

    // set system font

    bool custom_config_set = false;
    QString fontstring = SETTINGS_read_qstring("system_font","");

    if(fontstring=="") {
      SETTINGS_set_custom_configfile(OS_get_full_program_file_path("config"));
      fontstring = SETTINGS_read_qstring("system_font","");
      R_ASSERT(fontstring != "");
      custom_config_set = true;
    }

    {
      QFont font;
      font.fromString(fontstring);

#if 0 //FOR_MACOSX
      if(custom_config_set)
        font.setPointSizeF(font.pointSizeF()*96.0/72.0); // macs have dpi of 72, while linux and windows have 96.
#endif
      
      if(SETTINGS_read_qstring("system_font_style","")!="")
        font.setStyleName(SETTINGS_read_qstring("system_font_style",""));
      qapplication->setFont(font);
      QApplication::setFont(font);
    }

    if (custom_config_set==true){
      SETTINGS_unset_custom_configfile();
    }

  }


  //signal(SIGSEGV,finish);
  //signal(SIGFPE,finish);
  //signal(SIGTERM,finish);

  signal(SIGINT,finish);

#if GTK_IS_USED
  GTK_Init(argc,argv);
#endif

  printf("2\n");

  initradium();


  OS_get_full_program_file_path("start.py"); // ensure file is there
  
  PyRun_SimpleString("execfile(os.path.join(sys.g_program_path,\"start.py\"))");

  fprintf(stderr,"          ENDING B 1\n");
    
  Py_Finalize();

  fprintf(stderr,"          ENDING B 2\n");

  //abort();
  fprintf(stderr, "My pid: %d\n",(int)getpid());

  // Make it quit faster. Qt 5.7.0 crashes during shut down on linux.
#if defined(FOR_LINUX) && defined(RELEASE) && QT_VERSION==0x050700
  kill(getpid(), SIGKILL);
#endif
  
  //RError("hepp");
  return 0;
}

