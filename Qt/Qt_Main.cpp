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

#include <qapplication.h>
#include <qmainwindow.h>
#include <qsplitter.h>
#include <qpalette.h>
#include <qtabwidget.h>

#ifdef USE_QT4
#include <QMainWindow>

//Added by qt3to4:
#include <QEvent>
#include <QCustomEvent>
#endif

#include "EditorWidget.h"
#include "Qt_colors_proc.h"

#include "../common/nsmtracker.h"
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

#if USE_GTK_VISUAL
#  include "GTK_visual_proc.h"
#endif

#include "Qt_Main_proc.h"


extern bool doquit;
extern struct Root *root;


class MyApplication : public QApplication{
public:
  MyApplication(int argc,char **argv);
protected:
#ifdef __linux__
  bool x11EventFilter(XEvent*);
  //int x11ProcessEvent(XEvent*);
#endif
};

MyApplication::MyApplication(int argc,char **argv)
  : QApplication(argc,argv)
{
}

#ifdef __linux__

#if 0
int MyApplication::x11ProcessEvent(XEvent *event){
  if(event->type==ClientMessage)
    fprintf(stderr,"GOT IT\n");
  return QApplication::x11ProcessEvent(event);
}
#endif

bool MyApplication::x11EventFilter(XEvent *event){

  switch(event->type){
  case KeyPress:
    if(instrumentWidgetUsesKeyboard())
      return false;
  case KeyRelease:
    if(instrumentWidgetUsesKeyboard())
      return false;
  }

  {
    bool ret = X11_KeyboardFilter(event);

    if(ret==true)
      static_cast<EditorWidget*>(root->song->tracker_windows->os_visual.widget)->update();

    return ret;
  }
}
#endif // __linux__


  //QApplication *qapplication;
MyApplication *qapplication;


#if USE_QT_VISUAL

static double get_ms(void){
  struct timespec ts;
  clock_gettime(CLOCK_MONOTONIC, &ts);
  return ts.tv_sec * 1000.0 + ((double)ts.tv_nsec) / 1000000.0;
}


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
  if(using_the_event==1)
    return;

  // Check that we're not overflowing the Qt Event system.
  {
    static double last_time = 0.0;
    double time = get_ms();
    //if(time-last_time < 150) // this looks much better! (but it needs to be configurable)
    if(time-last_time < 5)
      return;
    last_time = time;
  }

  {
    QObject *qobject=(QObject *)root->song->tracker_windows->os_visual.widget;
    MyQCustomEvent *qce = new MyQCustomEvent();

    qapplication->postEvent(qobject,qce);
  }
}

#endif // USE_QT_VISUAL


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


// Called from gtk main loop
void Qt_EventHandler(void){
  while(qapplication->hasPendingEvents() && doquit==false)
    qapplication->processEvents();
#if 0
  QMainWindow *main_window = static_cast<QMainWindow*>(root->song->tracker_windows->os_visual.main_window);
  if(main_window->isVisible()==false)
    doquit=true;
#endif
}

int radium_main(char *arg){
  int argc=1;
  char *argv[2];

  argv[0] = strdup("radium");
  argv[1] = NULL;

#if USE_GTK_VISUAL
  GTK_Init(argc,argv);
#endif

  //GC_set_all_interior_pointers(0); // crash... (???)
  //GC_enable_incremental(); // crash.
  //GC_dont_gc = 1; // testing

  {
    int system_font_size = SETTINGS_read_int((char*)"system_font_size",-1);
    if(system_font_size>=0){
      QFont font=QFont(QApplication::font().family(),system_font_size);
      QApplication::setFont(font);
    }
  }

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
    if(override_default_qt_style)
      //QApplication::setStyle( new QOxygenStyle());
      QApplication::setStyle( new QPlastiqueStyle());
    //QApplication::setStyle( new QCleanlooksStyle() );
    //QApplication::setStyle( new QWindowsStyle() );
#endif
  }



#ifdef USE_QT4
  //QApplication::setGraphicsSystem("native");
  //QApplication::setGraphicsSystem("raster");
#endif

  qapplication=new MyApplication(argc,argv);

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
      xsplitter->show();
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
    }

    qapplication->setMainWidget(main_window);
    GFX_PlayListWindowToFront(); // To provoce setting width to 'blocklist_width'
    main_window->show();
  }

  PyRun_SimpleString("import menues");

#if USE_QT_VISUAL
  qapplication->exec();
#else
  GTK_MainLoop();
#if 0
  while(doquit==false){
    while(GTK_HasPendingEvents() || qapplication->hasPendingEvents()){
      GTK_HandleEvents();
      qapplication->processEvents();
    }
    usleep(1000);
  }
#endif
#endif

  posix_EndPlayer();
  //EndGuiThread();

  return 0;

}
extern "C" void initradium(void);
int main(int argc, char **argv){
  Py_Initialize();
  char temp[500];
  sprintf(temp,"import sys;sys.argv=[\"%s\",\"keybindings.conf\"]",argv[0]);
  PyRun_SimpleString(temp);
  initradium();
  PyRun_SimpleString("execfile(\"start.py\")"); // keybindings.conf start.sh\")");
  Py_Finalize();
}
