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


#include <qapplication.h>
#include <qmainwindow.h>

#include "MyWidget.h"

#include <X11/Xlib.h>

#include "../common/nsmtracker.h"
#include "../common/eventreciever_proc.h"
#include "../common/control_proc.h"

#include "../X11/X11_Bs_edit_proc.h"
#include "../X11/X11_MidiProperties_proc.h"
#include "../X11/X11_keyboard_proc.h"
#include "../X11/X11_ClientMessages_proc.h"
#include "../X11/X11_Qtstuff_proc.h"
#include "../common/OS_Bs_edit_proc.h"
#include "../X11/X11_Ptask2Mtask_proc.h"
#include "../X11/X11_Player_proc.h"


extern struct Root *root;


class MyApplication : public QApplication{
public:
  MyApplication(int argc,char **argv);
protected:
  bool x11EventFilter ( XEvent *);
};

MyApplication::MyApplication(int argc,char **argv)
  : QApplication(argc,argv)
{
}



bool MyApplication::x11EventFilter(XEvent *event){

  switch(event->type){
  case KeyPress:
    if(X11_KeyPress((XKeyEvent *)event,root->song->tracker_windows)==1){
      this->quit();
    }
    return TRUE;
  case KeyRelease:
    X11_KeyRelease((XKeyEvent *)event,root->song->tracker_windows);
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
    if(X11Event_ClientMessage((XClientMessageEvent *)&event,root->song->tracker_windows)==false){
      this->quit();
    }
    break;
  }

  return FALSE;
}



  //QApplication *qapplication;
MyApplication *qapplication;


extern LANGSPEC void Qt_Ptask2Mtask(void);

extern LANGSPEC void Qt_Ptask2Mtask(void){
  QObject *qobject=(QObject *)root->song->tracker_windows->os_visual->widget;

#warning "FIXME: Player thread shall not allocate"
  QCustomEvent *qce = new QCustomEvent(QEvent::User+1);

  //static int gakk;
  //qapplication->sendEvent(qobject,&qce);
  //fprintf(stderr,"Posting custom event %d\n", gakk++);
  qapplication->postEvent(qobject,qce);
}


//#include "google/profiler.h"

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

  qapplication=new MyApplication(argc,argv);

  X11_init_keyboard();

  X11_StartBlockSelector();
  X11_StartMidiProperties();

  StartGuiThread();

  // ProfilerStart("hepps");

  printf("starting\n");
  InitProgram();
  printf("ending\n");

  //ProfilerStop();

  X11_InitPlayer();

  X11_StartQtStuff();

  BS_UpdateBlockList();
  BS_UpdatePlayList();
  BS_SelectBlock(root->song->blocks);
  BS_SelectPlaylistPos(0);

  QMainWindow *main_window = static_cast<QMainWindow*>(root->song->tracker_windows->os_visual->main_window);
  main_window->show();
  qapplication->setMainWidget(main_window);
  qapplication->exec();
  
  X11_EndPlayer();
  EndGuiThread();

  return 0;

}

