/* Copyright 2012 Kjetil S. Matheussen

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

#if USE_QT_REQTYPE


#include <unistd.h>

#include <QApplication>
#include <QWidget>
#include <QLabel>
#include <QLineEdit>
#include <QKeyEvent>
#include <QSplitter>

#include "../common/nsmtracker.h"
#include "../common/visual_proc.h"
#include "../common/gfx_op_queue_proc.h"
#include "../common/gfx_proc.h"

#include "EditorWidget.h"

#include "Qt_MainWindow_proc.h"
#include "../GTK/GTK_visual_proc.h"

#include "../common/OS_system_proc.h"

#include "../common/OS_visual_input.h"
#include "../OpenGL/Widget_proc.h"


#ifdef TEST_MAIN

int num_users_of_keyboard = 0;
void X11_ResetKeysUpDowns(void){}
void *talloc_atomic(size_t size){return malloc(size);}
void *talloc(size_t size){return calloc(1,size);}
QApplication *qapplication;

#else //  TEST_MAIN

extern struct Root *root;
extern int num_users_of_keyboard;
//extern MyApplication *qapplication;

#endif //  TEST_MAIN

static const int x_margin = 5;
static const int y_margin = 5;

struct MyReqType{
  QFrame *frame;
  QString label_text;
  QString default_value;
  int y;
  bool widgets_disabled;
};

extern EditorWidget *g_editor;

// tvisual might be NULL
ReqType GFX_OpenReq(struct Tracker_Windows *tvisual,int width,int height,const char *title){
  num_users_of_keyboard++; // disable X11 keyboard sniffer

  MyReqType *reqtype = new MyReqType();

  if(tvisual==NULL){
    
    reqtype->frame = new QFrame();
    reqtype->frame->show();
    
  }else {
    
    EditorWidget *editor = g_editor;
    QSplitter *ysplitter = editor->ysplitter;
  
    reqtype->frame = new QFrame(ysplitter);

    ysplitter->insertWidget(0,reqtype->frame);
  }

  reqtype->frame->resize(5,10);
  reqtype->frame->show();
  reqtype->y = y_margin;
  reqtype->widgets_disabled = false;

  return reqtype;
}

// tvisual might be NULL
void GFX_CloseReq(struct Tracker_Windows *tvisual,ReqType das_reqtype){
  //EditorWidget *editor = static_cast<EditorWidget*>(tvisual->os_visual.widget);
  MyReqType *reqtype = static_cast<MyReqType*>(das_reqtype);

  delete reqtype->frame;

  num_users_of_keyboard--;

  OS_SYSTEM_ResetKeysUpDowns(); // Since we disabled X11 events, the X11 event sniffer didn't notice that we changed focus.

  if(reqtype->widgets_disabled==true){
    Qt_EnableAllWidgets();
    GFX_enable_mouse_keyboard();
  }

#if USE_GTK_VISUAL
  GTK_SetFocus();
#endif

  delete reqtype;
}

void GFX_WriteString(ReqType das_reqtype,const char *text){
  MyReqType *reqtype = static_cast<MyReqType*>(das_reqtype);

  reqtype->label_text += text;
}

void GFX_SetString(ReqType das_reqtype,const char *text){
  MyReqType *reqtype = static_cast<MyReqType*>(das_reqtype);

  reqtype->default_value = text;
}

namespace{
  class MyQLineEdit : public QLineEdit {
  public:
    MyQLineEdit(QFrame *parent)
      : QLineEdit(parent)
      , gotit(false)
    {}
    void keyPressEvent ( QKeyEvent * event ){
      printf("oh yeah baby %d\n",event->key());
      //event->ignore();
      if(event->key()>0){
        QLineEdit::keyPressEvent(event);
      
        if(event->key()==Qt::Key_Return || event->key()==Qt::Key_Escape)
          gotit = true;

#if USE_GTK_VISUAL
        if(event->key()==Qt::Key_Return)
          GTK_MainQuit();
#endif
      }
    }
    bool gotit;
  };
}

MyQLineEdit *g_edit;
void gotchar(char c){
  if(c=='\n')
    g_edit->gotit=true;
  else
    g_edit->insert(QString(c));
}

void GFX_ReadString(ReqType das_reqtype,char *buffer,int bufferlength){

  MyReqType *reqtype = static_cast<MyReqType*>(das_reqtype);

  int x = x_margin;

  int last_height = 0;
  QStringList lines = reqtype->label_text.split('\n');
  Q_FOREACH (QString line, lines) {
    reqtype->y += last_height;
    QLabel *label = new QLabel(line,reqtype->frame);
    label->move(x_margin,reqtype->y + 3);
    label->show();
    x = x_margin + label->width();
    last_height = label->height();
  }

  MyQLineEdit *edit = new MyQLineEdit(reqtype->frame);
  edit->insert(reqtype->default_value);
  edit->move(x + 5, reqtype->y);
  edit->show();
  
  reqtype->frame->adjustSize();
  reqtype->frame->setMinimumHeight(reqtype->y+R_MAX(20,edit->height()+10));

  // GL_lock is needed when using intel gfx driver to avoid crash caused by opening two opengl contexts simultaneously from two threads.
  GL_lock();{
    edit->setFocus();
  }GL_unlock();

  if(reqtype->widgets_disabled==false){
    Qt_DisableAllWidgets(reqtype->frame);
    GFX_disable_mouse_keyboard();
    reqtype->widgets_disabled=true;
  }

#if USE_GTK_VISUAL
  GTK_MainLoop();
#endif

#if USE_QT_VISUAL
  QString text = edit->text();

  g_edit = edit;
  //gotchar('a');
  //gotchar('b');

  while(edit->gotit==false){
    // GL_lock is needed when using intel gfx driver to avoid crash caused by opening two opengl contexts simultaneously from two threads.
    GL_lock();{
      QCoreApplication::processEvents();
    }GL_unlock();
    
    //GTK_HandleEvents();
    if(text!=edit->text()){
      text = edit->text();
      printf("text: \"%s\"\n",text.toAscii().constData());
    }
    usleep(100);
  }
#endif

  edit->setEnabled(false);

  reqtype->label_text = "";
  reqtype->y = reqtype->y + edit->height() + 10;

  snprintf(buffer,bufferlength-1,"%s",edit->text().toAscii().constData());
  printf("Got: \"%s\"\n",buffer);
}


#ifdef TEST_MAIN
/*
  g++ -g -DTEST_MAIN -DUSE_QT_VISUAL -DDEBUG -I../Qt Qt_ReqType.cpp -Wall `pkg-config --libs --cflags QtGui` && ./a.out
  gdb ./a.out
*/

int main(int argc, char **argv){
  qapplication=new QApplication(argc,argv);

  ReqType reqtype = GFX_OpenReq(NULL,30,5,"gakkgakk");

  {
    GFX_WriteString(reqtype,"hello1? ");

    char temp[500];
    GFX_ReadString(reqtype,temp,500);

    printf("Got \"%s\"\n",temp);
  }

  {
    GFX_WriteString(reqtype,"line above");
    GFX_WriteString(reqtype,"\n");
    GFX_WriteString(reqtype,"> ");
    //GFX_WriteString(reqtype,"hello2?\n Answer: ");

    char temp[500];
    GFX_ReadString(reqtype,temp,500);

    printf("Got \"%s\"\n",temp);
  }

  GFX_CloseReq(NULL,reqtype);


  return 0;
}
#endif

#endif // USE_QT_REQTYPE
