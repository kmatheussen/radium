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
    //printf("%d\n",((XKeyEvent *)event)->keycode);
    if(X11_KeyPress((XKeyEvent *)event,root->song->tracker_windows)==1){
      this->quit();
    }
    return TRUE;
  case KeyRelease:
    X11_KeyRelease((XKeyEvent *)event,root->song->tracker_windows);
    return TRUE;
  case EnterNotify:
  case LeaveNotify:
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
  QCustomEvent qce(QEvent::User+1);

  qapplication->sendEvent(qobject,&qce);
}



//extern LANGSPEC int dasmain(int argc,char **argv);
extern LANGSPEC int radium_main(char *arg);

//int radium_main(int argc,char **argv){
int radium_main(char *arg){
  int argc=1;
  char *argv[2];

  argv[0] = strdup("Radium");
  argv[1] = NULL;

  qapplication=new MyApplication(argc,argv);

  X11_StartBlockSelector();
  X11_StartMidiProperties();

  StartGuiThread();
  X11_InitPlayer();

  printf("starting\n");
  InitProgram();
  printf("ending\n");

  X11_StartQtStuff();

  BS_UpdateBlockList();
  BS_UpdatePlayList();
  BS_SelectBlock(root->song->blocks);
  BS_SelectPlaylistPos(0);

  qapplication->setMainWidget((QWidget *)root->song->tracker_windows->os_visual->widget);
  qapplication->exec();

  X11_EndPlayer();
  EndGuiThread();

  return 0;

}

