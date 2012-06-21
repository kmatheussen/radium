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

#include "MyWidget.h"

#include <X11/Xlib.h>

#include "../common/nsmtracker.h"
#include "../common/eventreciever_proc.h"
#include "../common/control_proc.h"

//#include "../X11/X11_Bs_edit_proc.h"
//#include "../X11/X11_MidiProperties_proc.h"
#include "../X11/X11_keyboard_proc.h"
#include "../X11/X11_ClientMessages_proc.h"
#include "../X11/X11_Qtstuff_proc.h"
#include "../common/OS_Bs_edit_proc.h"
#include "../X11/X11_Ptask2Mtask_proc.h"
#include "../X11/X11_Player_proc.h"

#include "Qt_Bs_edit_proc.h"
#include "Qt_instruments_proc.h"


extern struct Root *root;


class MyApplication : public QApplication{
public:
  MyApplication(int argc,char **argv);
protected:
  bool x11EventFilter(XEvent*);
  //int x11ProcessEvent(XEvent*);
};

MyApplication::MyApplication(int argc,char **argv)
  : QApplication(argc,argv)
{
}


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
    if(instrumentWindowUsesKeyboard())
      return false;
    if(X11_KeyPress((XKeyEvent *)event,root->song->tracker_windows)==1){
      this->quit();
    }
    static_cast<MyWidget*>(root->song->tracker_windows->os_visual.widget)->update();
    return TRUE;
  case KeyRelease:
    if(instrumentWindowUsesKeyboard())
      return false;
    X11_KeyRelease((XKeyEvent *)event,root->song->tracker_windows);
    static_cast<MyWidget*>(root->song->tracker_windows->os_visual.widget)->update();
    return TRUE;
  case EnterNotify:
    //printf("got enter notify\n");
    X11_ResetKeysUpDowns();
    break;
  case LeaveNotify:
    //printf("got leave notify\n");
    X11_ResetKeysUpDowns();
    break;
  case ClientMessage:
#if 0
    if(X11Event_ClientMessage((XClientMessageEvent *)&event,root->song->tracker_windows)==false){
      this->quit();
    }
#endif
    break;
  default:
    //fprintf(stderr, "got Unknown x11 event\n");
    break;
  }

  return FALSE;
}



  //QApplication *qapplication;
MyApplication *qapplication;


extern LANGSPEC void Qt_Ptask2Mtask(void);

extern LANGSPEC void Qt_Ptask2Mtask(void){
  QObject *qobject=(QObject *)root->song->tracker_windows->os_visual.widget;

#warning "FIXME: Player thread shall not allocate"
  QCustomEvent *qce = new QCustomEvent(QEvent::User+1);

  //static int gakk;
  //qapplication->sendEvent(qobject,&qce);
  //fprintf(stderr,"Posting custom event %d\n", gakk++);
  qapplication->postEvent(qobject,qce);
}


#include <qwindowsstyle.h>
//#include <qmacstyle_mac.h>
#if 0
#include <qplatinumstyle.h>
#include <qcdestyle.h>
#include <qmotifplusstyle.h>
#include <qsgistyle.h>
#endif



#include "qwidget.h"
#include "qlistbox.h"

//#include "google/profiler.h"

void start_blockselector();

//extern LANGSPEC int dasmain(int argc,char **argv);
extern LANGSPEC int radium_main(char *arg);
extern LANGSPEC int GC_dont_gc;
//int radium_main(int argc,char **argv){
int radium_main(char *arg){
  int argc=1;
  char *argv[2];

  argv[0] = strdup("Radium");
  argv[1] = NULL;

  //GC_dont_gc = 1;

#if 0
  QApplication::setStyle( new QPlatinumStyle() );
  QApplication::setStyle( new QCDEStyle() );
  QApplication::setStyle( new QMotifPlusStyle() );
  QApplication::setStyle( new QSGIStyle() );
#endif
  QApplication::setStyle( new QWindowsStyle() );

#ifdef USE_QT4
  //QApplication::setGraphicsSystem("native");
  //QApplication::setGraphicsSystem("raster");
#endif

  qapplication=new MyApplication(argc,argv);


  //QColor c(0xe5, 0xe5, 0xe5);
  QColor c(0xd2, 0xd0, 0xd5);
  QPalette pal = QPalette(qapplication->palette());
  pal.setColor( QPalette::Active, QColorGroup::Background, c);
  pal.setColor( QPalette::Active, QColorGroup::Button, c);
  pal.setColor( QPalette::Inactive, QColorGroup::Background, c);
  pal.setColor( QPalette::Inactive, QColorGroup::Button, c);
  qapplication->setPalette(pal);

  X11_init_keyboard();

  //X11_StartBlockSelector();
  //X11_StartMidiProperties();

  StartGuiThread();

  // ProfilerStart("hepps");

  printf("starting\n");
  InitProgram();
  printf("ending\n");

  //ProfilerStop();

  X11_InitPlayer();

  X11_StartQtStuff();

  QWidget *block_selector = create_blockselector();

  BS_UpdateBlockList();
  BS_UpdatePlayList();
  BS_SelectBlock(root->song->blocks);
  BS_SelectPlaylistPos(0);

  {
    QMainWindow *main_window = static_cast<QMainWindow*>(root->song->tracker_windows->os_visual.main_window);
    MyWidget *my_widget = static_cast<MyWidget*>(root->song->tracker_windows->os_visual.widget);

    {
      QSplitter *xsplitter = new QSplitter(Qt::Horizontal);//, main_window);
      xsplitter->setOpaqueResize(true);

      my_widget->reparent(xsplitter, QPoint(0,0), true);
      block_selector->reparent(xsplitter, QPoint(main_window->width()-100,0), true);

      block_selector->resize(100,block_selector->height());

      {
        QSplitter *ysplitter = new QSplitter(Qt::Vertical, main_window);
        ysplitter->setOpaqueResize(true);

        QWidget *instruments = createInstrumentsWidget();
        //instruments->show();

        xsplitter->reparent(ysplitter, QPoint(0,0), true);
        instruments->reparent(ysplitter, QPoint(0, main_window->height()-100), true);

        //ysplitter->show();

        main_window->setCentralWidget(ysplitter);
      }
    }

    qapplication->setMainWidget(main_window);
    main_window->show();
  }

  PyRun_SimpleString("import menues");
  
  qapplication->exec();
  
  X11_EndPlayer();
  EndGuiThread();

  return 0;

}

