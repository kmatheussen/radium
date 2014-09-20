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

#include "Python.h"

#include <signal.h>

#include <qapplication.h>
#include <qmainwindow.h>
#include <qsplitter.h>
#include <qpalette.h>
#include <qtabwidget.h>
#include <qfontdatabase.h>
#include <QEvent>
#include <QKeyEvent>
#include <Qt>
#include <QDir>
#include <QTextEdit>
#include <QLayout>


#ifdef USE_QT4
#include <QMainWindow>

//Added by qt3to4:
#include <QEvent>
#include <QCustomEvent>
#endif

#include "../common/nsmtracker.h"
#include "../common/disk_load_proc.h"
#include "../common/undo.h"
#include "../common/nag.h"
#include "../common/OS_settings_proc.h"
#include "../common/OS_visual_input.h"

#include "../mixergui/QM_MixerWidget.h"

#include "EditorWidget.h"
#include "Qt_colors_proc.h"

#include "../common/eventreciever_proc.h"
#include "../common/control_proc.h"
#include "../common/settings_proc.h"
#include "../common/trackreallines_proc.h"
#include "../common/OS_settings_proc.h"

#include "../crashreporter/crashreporter_proc.h"

#ifdef __linux__
#include <X11/Xlib.h>
//#include "../X11/X11_Bs_edit_proc.h"
//#include "../X11/X11_MidiProperties_proc.h"
#include "../X11/X11_keyboard_proc.h"
#include "../X11/X11_ClientMessages_proc.h"
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

#ifdef FOR_WINDOWS
#  include <windows.h>
#  include "../windows/W_Keyboard_proc.h"
#endif

#ifdef FOR_MACOSX
#  include "../macosx/cocoa_Keyboard_proc.h"
#endif

#include "../OpenGL/Render_proc.h"
#include "../OpenGL/Widget_proc.h"

#include "../embedded_scheme/scheme_proc.h"


#include "Qt_Main_proc.h"


extern bool doquit;
extern struct Root *root;


int num_users_of_keyboard = 0;

bool is_starting_up = true;
//void gakk();

class MyApplication : public QApplication{
public:

  MyApplication(int &argc,char **argv);

protected:

  bool last_key_was_lalt;

#ifdef __linux__
  bool x11EventFilter(XEvent *event){

    if(is_starting_up==true)// || return_false_now)
      return false;

    bool must_return_false = false;

    if(event->type==KeyPress || event->type==KeyRelease){
      XKeyEvent *key_event = (XKeyEvent*)event;

      int keynum = X11_get_keynum(QApplication::focusWidget(), key_event);


      if (keynum==EVENT_ALT_L){
        if (event->type==KeyPress){
          last_key_was_lalt = true;
          must_return_false = true;
        }else if (event->type==KeyRelease) {
          if(last_key_was_lalt==true){
            must_return_false = true;
            last_key_was_lalt = false;
          }
        }
      }else
        last_key_was_lalt = false;


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
    }

    bool ret = X11_KeyboardFilter(QApplication::focusWidget(), event);

    if(ret==true) {
      static_cast<EditorWidget*>(root->song->tracker_windows->os_visual.widget)->updateEditor();
      if(event->type==KeyPress)
        GL_create(root->song->tracker_windows, root->song->tracker_windows->wblock);
    }

    if(doquit==true)
      QApplication::quit();

    if (must_return_false==true)
      return false;
    else
      return ret;
  }
#endif

#ifdef FOR_WINDOWS
  bool 	winEventFilter ( MSG * msg, long * result ){
    if(is_starting_up==true)
      return false;

    bool ret = W_KeyboardFilter(msg);

    if(ret==true) {
      static_cast<EditorWidget*>(root->song->tracker_windows->os_visual.widget)->updateEditor();
      //if(event->type==KeyPress) TODO!
        GL_create(root->song->tracker_windows, root->song->tracker_windows->wblock);
    }
    return ret;
  }
#endif

#ifdef FOR_MACOSX
  bool macEventFilter ( EventHandlerCallRef caller, EventRef event ){
    if(is_starting_up==true)
      return false;

    bool ret = cocoa_KeyboardFilter(event);

    if(ret==true) {
      static_cast<EditorWidget*>(root->song->tracker_windows->os_visual.widget)->updateEditor();
      if(event->type==KeyPress)
        GL_create(window, window->wblock);
    }

    return ret;
  }
#endif
};

MyApplication::MyApplication(int &argc,char **argv)
  : QApplication(argc,argv)
  , last_key_was_lalt(false)
{
  //setStyleSheet("QStatusBar::item { border: 0px solid black }; ");
}



  //QApplication *qapplication;
MyApplication *qapplication;
QApplication *g_qapplication;


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

enum RT_MESSAGE_STATUS {
  RT_MESSAGE_READY,
  RT_MESSAGE_READY_FOR_SHOWING,
  RT_MESSAGE_SHOWING
};

volatile RT_MESSAGE_STATUS rt_message_status = RT_MESSAGE_READY;
static const int rt_message_length = 1024;
static char rt_message[rt_message_length];

class CalledPeriodically : public QTimer {

  QMessageBox msgBox;
  QAbstractButton *msgBox_ok;
  QAbstractButton *msgBox_dontshowagain;
  QSet<QString> dontshow;

public:
  CalledPeriodically(){
    setInterval(20);
    start();
    msgBox.setModal(false);
    msgBox_dontshowagain = (QAbstractButton*)msgBox.addButton("Dont show this message again",QMessageBox::ApplyRole);
    msgBox_ok = (QAbstractButton*)msgBox.addButton("Ok",QMessageBox::AcceptRole);
    msgBox.open();
    msgBox.hide();
  }
protected:

  void 	timerEvent ( QTimerEvent * e ){
#ifdef TEST_GC
    printf("triggering full collect\n");
    GC_gcollect();
#endif


    if (rt_message_status == RT_MESSAGE_READY_FOR_SHOWING) {

      QString message(rt_message);

      if (dontshow.contains(message)==false){
        msgBox.setText(message);
        msgBox.show();
      }

      rt_message_status = RT_MESSAGE_SHOWING;
    } else if (rt_message_status == RT_MESSAGE_SHOWING && msgBox.isHidden()) {

      if (msgBox.clickedButton() == msgBox_dontshowagain){
        //printf("Dontshowagain\n");
        dontshow.insert(rt_message);
      }
          
      rt_message_status = RT_MESSAGE_READY;
    }


    if(num_users_of_keyboard==0){
      {
        static int num_calls = 0;
        if(num_calls<1000/20){ // Update the screen constantly during the first second. It's a hack to make sure graphics is properly drawn after startup. (dont know what goes wrong)
          root->song->tracker_windows->must_redraw = true;
          num_calls++;
        }
      }
      
      {
        struct Tracker_Windows *window=root->song->tracker_windows;
        DO_GFX({
            MIDI_HandleInputMessage();
            TRACKREALLINES_call_very_often(window);
          });
        static_cast<EditorWidget*>(window->os_visual.widget)->updateEditor();
        
        if(doquit==true)
          QApplication::quit();
      }
    } // num_users_of_keyboard==0

    // Update graphics when playing
    {
      struct Tracker_Windows *window=root->song->tracker_windows;
      static_cast<EditorWidget*>(window->os_visual.widget)->callCustomEvent();
    }
  }
};

void RT_message(const char *fmt,...){
  va_list argp;

  if(rt_message_status != RT_MESSAGE_READY)
    return;
  
  va_start(argp,fmt);
  /*	vfprintf(stderr,fmt,argp); */
  vsnprintf(rt_message,rt_message_length,fmt,argp);
  va_end(argp);

  rt_message_status = RT_MESSAGE_READY_FOR_SHOWING;
}


#endif


int GFX_ResizeWindow(struct Tracker_Windows *tvisual,int x,int y){return 0;}

void SetNormalPointer(struct Tracker_Windows *tvisual){
  QMainWindow *main_window = (QMainWindow *)tvisual->os_visual.main_window;
  main_window->setCursor(Qt::ArrowCursor);
}
void SetDiagResizePointer(struct Tracker_Windows *tvisual){
  QMainWindow *main_window = (QMainWindow *)tvisual->os_visual.main_window;
  main_window->setCursor(Qt::SizeFDiagCursor);
}
void SetHorizResizePointer(struct Tracker_Windows *tvisual){
  QMainWindow *main_window = (QMainWindow *)tvisual->os_visual.main_window;
  main_window->setCursor(Qt::SizeHorCursor);
}


void GFX_toggleFullScreen(struct Tracker_Windows *tvisual){
  QMainWindow *main_window = (QMainWindow *)tvisual->os_visual.main_window;

  if(main_window->isFullScreen()){
    main_window->showNormal();
  }else{
    main_window->showFullScreen();
  }
}



void GFX_EditorWindowToFront(struct Tracker_Windows *tvisual){
  QMainWindow *main_window=static_cast<QMainWindow*>(tvisual->os_visual.main_window);

  //GFX_PlayListWindowToBack();
  main_window->raise();

#ifdef __linux__
  XSetInputFocus(main_window->x11Display(),(Window)main_window->x11AppRootWindow(),RevertToNone,CurrentTime);
  X11_ResetKeysUpDowns();
#endif
}




#include <qwindowsstyle.h>
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
#include <QPlastiqueStyle>
#endif

#ifdef USE_QT3
#include "qwidget.h"
#include "qlistbox.h"
#endif

#include <gc.h>

//#include "google/profiler.h"

void start_blockselector();

//extern LANGSPEC int dasmain(int argc,char **argv);
extern LANGSPEC int radium_main(char *arg);
extern LANGSPEC int GC_dont_gc;
//int radium_main(int argc,char **argv){

QString default_style_name;

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

int radium_main(char *arg){

  default_style_name = QApplication::style()->objectName();

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
    QApplication::setStyle( new QPlastiqueStyle());
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




#ifdef __linux__
  X11_init_keyboard();
#endif

  SetupMainWindow();


  //X11_StartBlockSelector();
  //X11_StartMidiProperties();

  //StartGuiThread();

  // ProfilerStart("hepps");

  printf("starting\n");
  if(InitProgram()==false)
    return 0;
  printf("ending\n");


  SCHEME_start();


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
      editor->reparent(xsplitter, QPoint(0,0), false);

      block_selector->reparent(xsplitter, QPoint(main_window->width()-100,0), true);

      block_selector->resize(100,block_selector->height());

      {
        QSplitter *ysplitter = new QSplitter(Qt::Vertical, main_window);
        editor->ysplitter = ysplitter;
        ysplitter->setOpaqueResize(true);
        

        QWidget *instruments = createInstrumentsWidget();

        xsplitter->reparent(ysplitter, QPoint(0,0), true);
        instruments->reparent(ysplitter, QPoint(0, main_window->height()-100), true);

        main_window->setCentralWidget(ysplitter);

        ysplitter->setStretchFactor(0,1);
        ysplitter->setStretchFactor(1,0);
      }

      //MixerWidget *mixer_widget = 
      new MixerWidget(xsplitter);
      //mixer_widget->resize(300,mixer_widget->height());

    }

    qapplication->setMainWidget(main_window);
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
  DrawUpTrackerWindow(window);
  

  EditorWidget *editor = static_cast<EditorWidget*>(window->os_visual.widget);

#if USE_OPENGL
  editor->gl_widget = GL_create_widget(editor);
  editor->position_gl_widget(window);
#endif

  show_nag_window("");

  //QApplication::quit();

#if !GTK_IS_USED
  CalledPeriodically periodic_timer;
#endif

  window->must_redraw = true;

  if(strcmp(SETTINGS_read_string("last_color_version","0.0"),"1.9.13")){
    GFX_Message(NULL,
                "Note!\n\n"
                "The default colors have changed. In case you have run Radium before, you might want to go to the Edit menu and select \"Set Default Colors\".\n\n"
                "You will only see this message once.");
    SETTINGS_write_string("last_color_version","1.9.13");
  }

  if(strcmp(SETTINGS_read_string("last_system_font_version","0.0"),"1.9.21")){
    GFX_Message(NULL,
                "Note!\n\n"
                "The default system font has changed. In case you have run Radium before, you might want to go to the Edit menu and select \"Set Default System Font\".\n\n"
                "You will only see this message once.");
    SETTINGS_write_string("last_system_font_version","1.9.21");
  }


  is_starting_up=false;

  window->must_redraw = true;
  editor->update();
  editor->resize(editor->width(),editor->height());

#if USE_OPENGL
  GL_create(window, window->wblock);
#endif

  // Hack to make Qt text input widgets not crash the program when using intel gfx driver and running opengl in separate thread (crash caused by opening two opengl contexts simultaneously from two threads). (strange stuff)
  GL_lock();
  {
    QTextEdit e;
    e.show();
    e.setFocus();
    qApp->processEvents();
  }
  GL_unlock();

#if USE_QT_VISUAL
  qapplication->exec();
#else
  GTK_MainLoop();
#endif

  GL_stop_widget(editor->gl_widget);

#if 0
  while(doquit==false){
    while(GTK_HasPendingEvents() || qapplication->hasPendingEvents()){
      GTK_HandleEvents();
      qapplication->processEvents();
    }
    usleep(1000);
  }
#endif

  MW_cleanup(); // Stop all sound properly. Don't want clicks when exiting.

  EndProgram();
  posix_EndPlayer();
  //EndGuiThread();

  CRASHREPORTER_close();

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



int main(int argc, char **argv){

  QCoreApplication::setAttribute(Qt::AA_X11InitThreads);


  //signal(SIGSEGV,crash);
  //signal(SIGFPE,crash);

#if !defined(FOR_WINDOWS)
  setenv("LC_NUMERIC", "C", 1); // Qt insists on doing strange things with locale settings, causing commans to appear instead of punctation. In an ideal world, LC_NUMERIC/LANG should never be set to anything else than "C", but unfortunately, many computers runs with uncommon language settings such as french or swedish. By default, programs seems to respect the sane behaviour (in the programming world), namely to never use commas when converting between strings and floats, but Qt does something strange with the world inside the QApplication contructor, and causes commas to be used everywhere if there is an uncommon LC_NUMERIC settings (or uncommon LANG setting). This setenv call is the only way I was able to make Pd work, without modifying Pd itself. (I modified Pd too though, but kept this line to prevent similar errors to appear in other libraries.) This behaviour should be changed in Qt.)
#endif

  // for mingw
  putenv(strdup("LC_NUMERIC=C"));

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

  g_qapplication = qapplication;

  OS_set_argv0(argv[0]);
  

  CRASHREPORTER_init();

  GC_INIT(); // mingw/wine crashes immediately if not doing this when compiling without --enable-threads=no. (wine doesn't work very well with libgc. Should perhaps file a report.)

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

#ifdef IS_LINUX_BINARY
#if 0
  setenv("PYTHONHOME","temp/dist",1);
  setenv("PYTHONPATH","temp/dist",1);
#else
  setenv("PYTHONHOME","python2.7/lib",1);
  setenv("PYTHONPATH","python2.7/lib",1);
#endif
#endif

  Py_Initialize();

  {
    char temp[500];

    // Set loading path to argv[0]
    PyRun_SimpleString("import sys,os");

#if defined(FOR_WINDOWS)
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
    sprintf(temp,"sys.argv=[\"%s\",\"%s\"]", argv[0], OS_get_keybindings_conf_filename());
    PyRun_SimpleString(temp);
    
    printf("argv[0]: %s\n",argv[0]);
    PyRun_SimpleString("print \"path:\",sys.g_program_path,239");
    
    //exit(0);
  }

  qapplication->setWindowIcon(QIcon(QString(OS_get_program_path()) + OS_get_directory_separator() + "radium_256x256x32.png"));

  {
    QFontDatabase::addApplicationFont(QString(OS_get_program_path()) + OS_get_directory_separator() + "fonts/LiberationMono-Bold.ttf");
    QFontDatabase::addApplicationFont(QString(OS_get_program_path()) + OS_get_directory_separator() + "fonts/VeraMono.ttf");
    QFontDatabase::addApplicationFont(QString(OS_get_program_path()) + OS_get_directory_separator() + "fonts/VeraMoBd.ttf");
    QFontDatabase::addApplicationFont(QString(OS_get_program_path()) + OS_get_directory_separator() + "fonts/NimbusSansL.ttf");
    QFontDatabase::addApplicationFont(QString(OS_get_program_path()) + OS_get_directory_separator() + "fonts/DejaVuSansCondensed-Bold.ttf");
    QFontDatabase::addApplicationFont(QString(OS_get_program_path()) + OS_get_directory_separator() + "fonts/DejaVuSansMono-Bold.ttf");
    QFontDatabase::addApplicationFont(QString(OS_get_program_path()) + OS_get_directory_separator() + "fonts/Lato-Bla.ttf");

    //QApplication::setFont(QFont("Lohit-Tamil",8));
    //QApplication::setFont(QFont("Nimbus Sans L",8));
    //QApplication::setFont(QFont("Liberation Sans L",8));

    //printf("System font name: \"%s\". Size: %d\n",QApplication::font().family().ascii(),QApplication::font().pointSize());


    const char *fontstring = SETTINGS_read_string("system_font",NULL);
    if(fontstring!=NULL){
      QFont font;
      font.fromString(fontstring);
      if(SETTINGS_read_string("system_font_style",NULL)!=NULL)
        font.setStyleName(SETTINGS_read_string("system_font_style",NULL));
      qapplication->setFont(font);
      QApplication::setFont(font);
    }


#if 0

    int system_font_size = SETTINGS_read_int((char*)"system_font_size",-1);
    if(system_font_size>=0){
#if 0 //defined(FOR_MACOSX)
      QFont font=QFont(QApplication::font().family(),system_font_size);
#else
      QFont font=QFont("Nimbus Sans L",system_font_size);
#endif
      //QFont font=QFont("Nimbus Sans L",10);
      //font.setPointSize(system_font_size);
      //QFont font=QFont("Bitstream Vera Sans Mono",system_font_size);
      QApplication::setFont(font);
    }
#endif
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

  PyRun_SimpleString("execfile(os.path.join(sys.g_program_path,\"start.py\"))"); // keybindings.conf start.sh\")");

  Py_Finalize();
}

