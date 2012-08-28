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


#include <qstatusbar.h>
#include <qmainwindow.h>
#include <qfiledialog.h>

#include "EditorWidget.h"

#include "Qt_colors_proc.h"
#include "Qt_Menues_proc.h"

#include <QtCore>
#include <QtGui>


#if USE_GTK_VISUAL
#  ifdef __linux__
#    if USE_QT3
#      include "qtxembed-1.3-free/src/qtxembed.h"
#    endif
#    if USE_QT4
#      include <QX11EmbedContainer>
#      define QtXEmbedContainer QX11EmbedContainer
#    endif
#  else
#    if FOR_MACOSX
//#      include  <QMacNativeWidget>
//#      define QtXEmbedContainer QMacNativeWidget 
#      define QtXEmbedContainer QWidget
#    else
#      define QtXEmbedContainer QWidget
#    endif
#  endif
   QtXEmbedContainer *g_embed_container;
#  include "../GTK/GTK_visual_proc.h"
#endif


#if USE_QT_VISUAL
#  include "Qt_Fonts_proc.h"
#endif

#include "../common/settings_proc.h"
#include "../common/eventreciever_proc.h"

#include "Qt_MainWindow_proc.h"

#ifdef FOR_WINDOWS
#  include <windows.h>
static HWND gtk_hwnd = NULL;
#endif


#if USE_GTK_VISUAL

#if FOR_WINDOWS
static bool sat=false;
#endif

class MyQtXEmbedContainer : public QtXEmbedContainer{
public:
  MyQtXEmbedContainer(QWidget *widget)
    : QtXEmbedContainer(widget)
  {
#if USE_QT3
    setWFlags(Qt::WStaticContents | Qt::WResizeNoErase | Qt::WRepaintNoErase | Qt::WNoAutoErase);
#endif
  }
  void paintEvent( QPaintEvent *e ){
    //printf("got emb paint event %p\n",gtk_hwnd);
    // Shouldn't we call the super method here?
    QtXEmbedContainer::paintEvent(e);

#if 1
#if FOR_WINDOWS
    if(gtk_hwnd!=NULL && sat==false){
      //SetParent((HWND)gtk_hwnd,(HWND)g_embed_container->nativeParentWidget());
      SetParent((HWND)gtk_hwnd,g_embed_container->winId());
      // printf("Trying: %p\n",SetParent((HWND)gtk_hwnd,g_embed_container->effectiveWinId()));
      sat=true;
    }
    //MoveWindow(gtk_hwnd, 0, 0, 600,600,true);
#endif
#endif

  }
};
#endif

EditorWidget::EditorWidget(QWidget *parent, const char *name )
  //: QFrame( parent, name, Qt::WStaticContents | Qt::WResizeNoErase | Qt::WRepaintNoErase | Qt::WNoAutoErase )
  : QWidget( parent, name, Qt::WStaticContents | Qt::WResizeNoErase | Qt::WRepaintNoErase | Qt::WNoAutoErase )
    //: QtXEmbedContainer( parent, name) //, Qt::WStaticContents | Qt::WResizeNoErase | Qt::WRepaintNoErase | Qt::WNoAutoErase )
  , qpa(256)
{
#if USE_QT_VISUAL
  this->qpixmap=NULL;
#endif

#if USE_GTK_VISUAL
  if(sizeof(int64_t) < sizeof(WId))
    abort();

  if(g_embed_container==NULL){
    //g_embed_container = this;
    g_embed_container = new MyQtXEmbedContainer(this);
    //g_embed_container = new QtXEmbedContainer(this);
    g_embed_container->setBackgroundMode(Qt::NoBackground);
    //g_embed_container->show();

    //g_embed_container->setSizePolicy(QSizePolicy::Maximum,QSizePolicy::Maximum);

    //usleep(1*1000*1000);

    //GTK_CreateVisual(g_embed_container->winId());

    // g_embed_container is not visible now (it's complicated), but by calling embed(), the gdk widget still shows up, for some reason.
    // Don't know if this is an okay workaround (or why it even is a workaround). Going to clean up this mess later, hopefully.
#if USE_QT3
    g_embed_container->embed(GTK_CreateVisual(g_embed_container->winId()),true);
#endif

#if __linux__
#if USE_QT4
    g_embed_container->show();
    if(0) //getenv("KDE_FULL_SESSION")!=NULL)
      g_embed_container->embedClient(GTK_CreateVisual(g_embed_container->winId()));
    else
      GTK_CreateVisual(g_embed_container->winId());
#endif
#endif

#if FOR_WINDOWS
    gtk_hwnd = (HWND)GTK_CreateVisual(g_embed_container->winId());
    //gtk_hwnd = (HWND)GTK_CreateVisual(g_embed_container->nativeParentWidget());
    //gtk_hwnd = (HWND)GTK_CreateVisual(g_embed_container->effectiveWinId());
    g_embed_container->show();
#endif

#if FOR_MACOSX
    //g_embed_container->show();
    //g_embed_container->show();
    GTK_CreateVisual((void*)g_embed_container->winId());
    //gtk_hwnd = (HWND)GTK_CreateVisual(g_embed_container->nativeParentWidget());
    //gtk_hwnd = (HWND)GTK_CreateVisual(g_embed_container->effectiveWinId());
#endif

    //g_embed_container->grabKeyboard();

    //g_embed_container->moveInputToProxy();

    //g_embed_container->show();

    //usleep(1*1000*1000);
#if 1
    int width=600;
    int height=600;
    g_embed_container->resize(width,height);
    GTK_SetSize(width,height);
#endif

#if 1
#if FOR_WINDOWS
    g_embed_container->show();
    //SetParent((HWND)gtk_hwnd,g_embed_container->winId());
    //SetParent((HWND)gtk_hwnd,g_embed_container->effectiveWinId());
    //SetParent(g_embed_container->winId(),(HWND)gtk_hwnd);
#endif
#endif

  }
#endif

  setEditorColors(this);

#if USE_QT_VISUAL
  //this->setMouseTracking(true);
  //g_embed_container->setMouseTracking(true);
#endif

#if 0
  //this->setFrameStyle(QFrame::Raised );
  this->setFrameStyle(QFrame::Sunken );
  this->setFrameShape(QFrame::Panel);
  this->setLineWidth(2);
#endif
}

EditorWidget::~EditorWidget()
{
}


extern bool doquit;
extern struct Root *root;

EditorWidget *g_editor = NULL;

#if 1
// dangerous stuff
extern "C" void grabKeyboard(void);
void grabKeyboard(void){
  //g_embed_container->grabKeyboard();
  //g_editor->main_window->grabKeyboard();
  //abort(); // This function should not be used.
  g_editor->setFocus();
}

void Qt_DisableAllWidgets(void){
  //g_embed_container->grabKeyboard();
  g_editor->main_window->setEnabled(false);
}
void Qt_EnableAllWidgets(void){
  //g_embed_container->grabKeyboard();
  g_editor->main_window->setEnabled(true);
}
#endif

class MyQMainWindow : public QMainWindow{
public:
  MyQMainWindow() : QMainWindow(NULL,"Radium") {}
  void closeEvent(QCloseEvent *ce){
    struct Tracker_Windows *window=static_cast<struct Tracker_Windows*>(root->song->tracker_windows);
    doquit = Quit(window);
  }

  // Want the wheel to work from everywhere.
  void wheelEvent(QWheelEvent *qwheelevent){
    struct Tracker_Windows *window=static_cast<struct Tracker_Windows*>(root->song->tracker_windows);
    struct TEvent tevent={0};
    tevent.ID=TR_KEYBOARD;
    tevent.keyswitch=0;

    if(qwheelevent->delta()>0){
      tevent.SubID=EVENT_UPARROW;
    }else{
      tevent.SubID=EVENT_DOWNARROW;
    }

    tevent.x=qwheelevent->x()-XOFFSET;
    tevent.y=qwheelevent->y()-YOFFSET;

    for(int lokke=0;lokke<R_ABS(qwheelevent->delta()/120);lokke++){
      EventReciever(&tevent,window);
    }

    update();
  }
};

void SetupMainWindow(void){

  //QMainWindow *main_window = new QMainWindow(NULL, "Radium", Qt::WStyle_Customize | Qt::WStyle_NoBorder);// | Qt::WStyle_Dialog);
  QMainWindow *main_window = new MyQMainWindow();//NULL, "Radium");

#ifdef USE_QT4
  //main_window->setAttribute(Qt::WA_PaintOnScreen);
  //main_window->setAttribute(Qt::WA_OpaquePaintEvent);
  //main_window->setAttribute(Qt::WA_NoSystemBackground);
  //main_window->setBackgroundMode(Qt::NoBackground);
#endif

#ifdef USE_QT3
  main_window->setBackgroundMode(Qt::NoBackground);
#endif

  //QPalette pal = QPalette(main_window->palette());
  //pal.setColor( QPalette::Active, QColorGroup::Background, Qt::green);
  //pal.setColor(main_window->backgroundRole(), Qt::blue);
  //main_window->setPalette(pal);
  //main_window->menuBar()->setPalette(pal);

  EditorWidget *editor=new EditorWidget(main_window,"name");
#if USE_QT_VISUAL
  editor->setFocus(); // Lots of trouble with focus with the qt_visual backend.
#endif

#ifdef USE_QT4
  editor->setAttribute(Qt::WA_PaintOnScreen);
  editor->setAttribute(Qt::WA_OpaquePaintEvent);
  editor->setAttribute(Qt::WA_NoSystemBackground);
#endif
  editor->main_window = main_window;

#ifdef USE_QT3
  main_window->setBackgroundMode(Qt::NoBackground);
#endif

  main_window->resize(800,400);
  editor->setMinimumWidth(400);
  editor->setMinimumHeight(200);

  main_window->setCaption("Radium editor window");
  main_window->statusBar()->message( "Ready", 2000 );

  {
    QStatusBar *status_bar = main_window->statusBar();
    editor->status_label = new QLabel(status_bar);//"");
    editor->status_label->setFrameStyle(QFrame::Sunken);
    //editor->status_frame->
    status_bar->addWidget(editor->status_label, 1, true);
  }


#if FOR_MACOSX
  //qt_mac_set_native_menubar(false);
  QApplication::instance()->setAttribute(Qt::AA_DontUseNativeMenuBar);
  QMenuBar *menubar = new QMenuBar(0);
  initMenues(menubar);
  //menubar->show();
  main_window->setMenuBar(menubar);
#else
  initMenues(main_window->menuBar());
#endif

#if USE_QT_VISUAL

  {
    QFont font = QFont("Monospace");

    const char *fontstring = SETTINGS_read_string("font",NULL);
    if(fontstring!=NULL)
      font.fromString(fontstring);

    editor->font = font;

    //editor->font->setStyleHint(QFont::TypeWriter);
    //editor->font->setFixedPitch(false);
  }


  editor->qpixmap=new QPixmap(editor->width(),editor->height());
#ifdef USE_QT3
  editor->qpixmap->setOptimization(QPixmap::BestOptim);
#endif

  editor->cursorpixmap=new QPixmap(editor->width(),editor->height());
#ifdef USE_QT3
  editor->cursorpixmap->setOptimization(QPixmap::BestOptim);
#endif

#endif // USE_QT_VISUAL


  g_editor = editor;
}

void GFX_SetMinimumWindowWidth(struct Tracker_Windows *tvisual, int width){
  EditorWidget *editor=(EditorWidget *)tvisual->os_visual.widget;
  editor->setMinimumWidth(width);
}


void GFX_SetWindowTitle(struct Tracker_Windows *tvisual,const char *title){
  QMainWindow *main_window = (QMainWindow *)tvisual->os_visual.main_window;
  main_window->setCaption(title);
}

void GFX_SetStatusBar(struct Tracker_Windows *tvisual,const char *title){
  //QMainWindow *main_window = (QMainWindow *)tvisual->os_visual.main_window;
  //main_window->statusBar()->message(title);

  EditorWidget *editor=(EditorWidget *)tvisual->os_visual.widget;
  editor->status_label->setText(title);
}

extern "C" int num_users_of_keyboard;

const char *GFX_GetLoadFileName(
	struct Tracker_Windows *tvisual,
	ReqType reqtype,
	char *seltext,
	char *dir
){
  const char *ret;

  num_users_of_keyboard++;

  QString filename = QFileDialog::getOpenFileName();

  if(filename == ""){
    ret=NULL;
    goto exit;
  }

  ret = talloc_strdup(filename.ascii());

 exit:
  num_users_of_keyboard--;
  return ret;
}

const char *GFX_GetSaveFileName(
	struct Tracker_Windows *tvisual,
	ReqType reqtype,
	char *seltext,
	char *dir
){
  num_users_of_keyboard++;
  const char *ret = talloc_strdup((char*)QFileDialog::getSaveFileName().ascii());
  num_users_of_keyboard--;
  return ret==NULL || strlen(ret)==0 
    ? NULL 
    : ret;
}


