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

#include "../mixergui/QM_MixerWidget.h"


#include "EditorWidget.h"
#include "Qt_colors_proc.h"

#include "../common/eventreciever_proc.h"
#include "../common/control_proc.h"
#include "../common/settings_proc.h"

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

#include "../GTK/GTK_visual_proc.h"

#ifdef FOR_WINDOWS
#  include <windows.h>
#  include "../windows/W_Keyboard_proc.h"
#endif

#ifdef FOR_MACOSX
#  include "../macosx/cocoa_Keyboard_proc.h"
#endif

#include "Qt_Main_proc.h"


extern bool doquit;
extern struct Root *root;


int num_users_of_keyboard = 0;


class MyApplication : public QApplication{
public:
  MyApplication(int &argc,char **argv);

protected:

#ifdef __linux__
  bool x11EventFilter(XEvent *event){
    bool ret = X11_KeyboardFilter(event);

    if(ret==true)
      static_cast<EditorWidget*>(root->song->tracker_windows->os_visual.widget)->updateEditor();

    if(doquit==true)
      QApplication::quit();

    return ret;
  }
#endif

#ifdef FOR_WINDOWS
  bool 	winEventFilter ( MSG * msg, long * result ){
    bool ret = W_KeyboardFilter(msg);

    if(ret==true)
      static_cast<EditorWidget*>(root->song->tracker_windows->os_visual.widget)->updateEditor();

    return ret;
  }
#endif

#ifdef FOR_MACOSX
  bool macEventFilter ( EventHandlerCallRef caller, EventRef event ){
    bool ret = cocoa_KeyboardFilter(event);

    if(ret==true)
      static_cast<EditorWidget*>(root->song->tracker_windows->os_visual.widget)->updateEditor();

    return ret;
  }
#endif
};

MyApplication::MyApplication(int &argc,char **argv)
  : QApplication(argc,argv)
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
#include "../midi/midi_i_input_proc.h"
#include "../common/gfx_proc.h"
#include "../common/gfx_op_queue_proc.h"

//#define TEST_GC

#ifdef TEST_GC
#  include "gc.h"
#endif

class CalledPeriodically : public QTimer{
public:
  CalledPeriodically(){
    setInterval(20);
    start();
  }
protected:
  void 	timerEvent ( QTimerEvent * e ){
#ifdef TEST_GC
    printf("triggering full collect\n");
    GC_gcollect();
#endif

    if(1){
      struct Tracker_Windows *window=root->song->tracker_windows;
      static_cast<EditorWidget*>(window->os_visual.widget)->callCustomEvent();
    }

    if(num_users_of_keyboard>0)
      return;
    {
      struct Tracker_Windows *window=root->song->tracker_windows;
      DO_GFX_BLT(MIDI_HandleInputMessage());
      if(GFX_get_op_queue_size(window) > 0)
        static_cast<EditorWidget*>(window->os_visual.widget)->updateEditor();
      
      if(doquit==true)
        QApplication::quit();
    }
  }
};
#endif

int GFX_ResizeWindow(struct Tracker_Windows *tvisual,int x,int y){return 0;}

void SetNormalPointer(struct Tracker_Windows *tvisual){return ;}
void SetResizePointer(struct Tracker_Windows *tvisual){return ;}


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

  {
    QMainWindow *main_window = static_cast<QMainWindow*>(root->song->tracker_windows->os_visual.main_window);
    EditorWidget *editor = static_cast<EditorWidget*>(root->song->tracker_windows->os_visual.widget);

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

#if !GTK_IS_USED
  CalledPeriodically periodic_timer;
#endif

  //QFontDatabase::addApplicationFont("/gammelhd/usr/share/fonts/liberation/LiberationMono-Regular.ttf");

  ResetUndo();
  if(load_new_song==true)
    NewSong_CurrPos(root->song->tracker_windows);

  //updateAllFonts(QApplication::mainWidget());

  show_nag_window("");

#if USE_QT_VISUAL
  qapplication->exec();
#else
  GTK_MainLoop();
#endif

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

  return 0;

}

extern "C" {
  static void finish(int sig){
    QApplication::quit();
  }
  extern void initradium(void);
}

int main(int argc, char **argv){
  GC_INIT(); // mingw/wine crashes immediately if not doing this when compiling without --enable-threads=no. (wine doesn't work very well with libgc. Should perhaps file a report.)

  printf("1: argv[0]: \"%s\"\n",argv[0]);

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
    sprintf(temp,"sys.argv=[\"%s\",os.path.join(sys.g_program_path,\"keybindings.conf\")]",argv[0]);
    PyRun_SimpleString(temp);
    
    printf("argv[0]: %s\n",argv[0]);
    PyRun_SimpleString("print \"path:\",sys.g_program_path,239");
    
    //exit(0);
  }

  QApplication::setDesktopSettingsAware(false);

  // Create application here in order to get default style. (not recommended, but can't find another way)
  qapplication=new MyApplication(argc,argv);
  g_qapplication = qapplication;
  
  qapplication->setWindowIcon(QIcon("radium_256x256x32.png"));

  {
#if 0
    fprintf(stderr,"load1\n");
    fflush(stderr);
    if(-1==QFontDatabase::addApplicationFont("fonts/Lohit-Tamil.ttf"))
      abort();
    fprintf(stderr,"load2\n");
    fflush(stderr);
    if(-1==QFontDatabase::addApplicationFont("fonts/LiberationMono-Bold.ttf"))
      abort();
    fprintf(stderr,"load3\n");
    fflush(stderr);
    if(-1==QFontDatabase::addApplicationFont("fonts/NimbusSansL.ttf"))
      abort();
    fprintf(stderr,"load4\n");
    fflush(stderr);
    if(-1==QFontDatabase::addApplicationFont("fonts/VeraMono.ttf"))
      abort();
    fprintf(stderr,"load5\n");
    fflush(stderr);
#endif

    QFontDatabase::addApplicationFont("fonts/LiberationMono-Bold.ttf");
    QFontDatabase::addApplicationFont("fonts/VeraMono.ttf");
    QFontDatabase::addApplicationFont("fonts/VeraMoBd.ttf");
    QFontDatabase::addApplicationFont("fonts/NimbusSansL.ttf");
    QFontDatabase::addApplicationFont("fonts/DejaVuSansCondensed-Bold.ttf");
    QFontDatabase::addApplicationFont("fonts/DejaVuSansMono-Bold.ttf");


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


  signal(SIGINT,finish);

#if GTK_IS_USED
  GTK_Init(argc,argv);
#endif

  printf("2\n");

  initradium();

  PyRun_SimpleString("execfile(os.path.join(sys.g_program_path,\"start.py\"))"); // keybindings.conf start.sh\")");

  Py_Finalize();
}

