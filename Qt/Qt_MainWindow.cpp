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

extern "C"{
#include <Python.h>
#include "../api/radium_proc.h"
}

#include <qapplication.h>
#include <qmainwindow.h>
#include <qsplitter.h>
#include <QCloseEvent>
#include <QStatusBar>
#include <QMenuBar>
#include <QMessageBox>

#include <qfiledialog.h>

extern bool is_starting_up;


#if USE_GTK_VISUAL
#  ifdef __linux__
#    if USE_QT3
#      include "qtxembed-1.3-free/src/qtxembed.h"
#    endif
#    if USE_QT4
#      include <QX11EmbedContainer>
#      define EditorWidgetParent QX11EmbedContainer
#    endif
#  else
#    if FOR_MACOSX
//#      include  <QMacNativeWidget>
//#      define EditorWidgetParent QMacNativeWidget 
#      define EditorWidgetParent QWidget
#    else
#      define EditorWidgetParent QWidget
#    endif
#  endif
   EditorWidgetParent *g_embed_container;
#endif // USE_GTK_VISUAL

#if defined(FOR_WINDOWS) && USE_GTK_VISUAL
#  include <windows.h>
static HWND gtk_hwnd = NULL;
#endif

#include "EditorWidget.h"

#if USE_GTK_VISUAL
#  include "../GTK/GTK_visual_proc.h"
#endif

#include "../common/gfx_proc.h"
#include "../common/cursor_updown_proc.h"

#include "Qt_colors_proc.h"
#include "Qt_Menues_proc.h"


#if USE_QT_VISUAL
#  include "Qt_Fonts_proc.h"
#endif

#include "../common/settings_proc.h"
#include "../common/eventreciever_proc.h"

#include "Qt_MyQSlider.h"
#include "Qt_MyQCheckBox.h"
#include "mQt_bottom_bar_widget_callbacks.h"

#include "Qt_MainWindow_proc.h"


#if USE_GTK_VISUAL

#if FOR_WINDOWS
static bool sat=false;
#endif


class MyEditorWidgetParent : public EditorWidgetParent{
  //Q_OBJECT;

public:
  MyEditorWidgetParent(QWidget *widget)
    : EditorWidgetParent(widget)
  {
#if USE_QT3
    setWFlags(Qt::WStaticContents | Qt::WResizeNoErase | Qt::WRepaintNoErase | Qt::WNoAutoErase);
#endif
  }
  void paintEvent( QPaintEvent *e ){
    //printf("got emb paint event %p\n",gtk_hwnd);
    // Shouldn't we call the super method here?
    EditorWidgetParent::paintEvent(e);

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
    //: QWidget( parent, name) //, Qt::WStaticContents | Qt::WResizeNoErase | Qt::WRepaintNoErase | Qt::WNoAutoErase )
    //: EditorWidgetParent( parent, name) //, Qt::WStaticContents | Qt::WResizeNoErase | Qt::WRepaintNoErase | Qt::WNoAutoErase )
#if USE_QT_VISUAL
  , paintbuffer(NULL)
  , cursorbuffer(NULL)
  , linesbuffer(NULL)
  , paintbuffer_painter(NULL)
  , cursorbuffer_painter(NULL)
  , linesbuffer_painter(NULL)
#endif
  , qpa(256)
{

#if USE_GTK_VISUAL
  if(sizeof(int64_t) < sizeof(WId))
    abort();

  if(g_embed_container==NULL){
    //g_embed_container = this;
    g_embed_container = new MyEditorWidgetParent(this);
    //g_embed_container = new EditorWidgetParent(this);
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
    int width=800;
    int height=800;
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
#endif // USE_GTK_VISUAL

  setEditorColors(this);

#if USE_QT_VISUAL
  this->setMouseTracking(true);
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

static Bottom_bar_widget *g_bottom_bar = NULL;


#if 1
// dangerous stuff
extern "C" void grabKeyboard(void);
void grabKeyboard(void){
  //g_embed_container->grabKeyboard();
  //g_editor->main_window->grabKeyboard();
  //abort(); // This function should not be used.
  g_editor->setFocus();
}

static bool widgets_are_disabled = false;

void Qt_DisableAllWidgets(QWidget *not_me){
  widgets_are_disabled = true;
#if USE_GTK_REQTYPE
  g_editor->main_window->setEnabled(false);
#endif
#if USE_QT_REQTYPE
  g_editor->main_window->menuWidget()->setEnabled(false);
  for(int i=0;i<g_editor->ysplitter->count();i++)
    if(g_editor->ysplitter->widget(i) != g_editor && g_editor->ysplitter->widget(i)!=not_me)
      g_editor->ysplitter->widget(i)->setEnabled(false);
#endif
}
void Qt_EnableAllWidgets(void){
  //g_embed_container->grabKeyboard();
  g_editor->main_window->setEnabled(true);
#if USE_QT_REQTYPE
  g_editor->main_window->menuWidget()->setEnabled(true);
  for(int i=0;i<g_editor->ysplitter->count();i++)
    g_editor->ysplitter->widget(i)->setEnabled(true);
#endif

  widgets_are_disabled = false;
}
#endif

bool GFX_EditorIsVisible(void){
  return !g_editor->isHidden();
}

void GFX_ShowEditor(void){
  g_editor->show();
}

void GFX_HideEditor(void){
  g_editor->hide();
}

void GFX_showHideEditor(void){
  if(g_editor->isHidden())
    g_editor->show();
  else
    g_editor->hide();
}



class MyQMainWindow : public QMainWindow{
  //Q_OBJECT;

public:
  MyQMainWindow() : QMainWindow(NULL,"Radium") {}

  void closeEvent(QCloseEvent *ce){
    ce->ignore();
    if(widgets_are_disabled==false){
      struct Tracker_Windows *window=static_cast<struct Tracker_Windows*>(root->song->tracker_windows);
      doquit = Quit(window);
#if USE_QT_VISUAL
      if(doquit==true)
        QApplication::quit();
#endif
    }
  }

  // Want the wheel to work from everywhere.
  void wheelEvent(QWheelEvent *qwheelevent){
    if(is_starting_up==true)
      return;

    struct Tracker_Windows *window=static_cast<struct Tracker_Windows*>(root->song->tracker_windows);

    int num_lines = R_ABS(qwheelevent->delta()/120);    

    DO_GFX(
           {
             if(qwheelevent->delta()<0)
               ScrollEditorDown(window,num_lines);
             else
               ScrollEditorUp(window,num_lines);
           });
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
#if USE_GTK_VISUAL
  editor->setAttribute(Qt::WA_PaintOnScreen);
#endif
#if USE_QT_VISUAL && !defined(__linux__)  // double buffer actually improves performance on linux. Still not as good as gtk though.
  editor->setAttribute(Qt::WA_PaintOnScreen);
#endif
  //editor->setAttribute(Qt::WA_PaintOnScreen);

  editor->setAttribute(Qt::WA_OpaquePaintEvent);
  editor->setAttribute(Qt::WA_NoSystemBackground);
#endif
  editor->main_window = main_window;

#ifdef USE_QT3
  main_window->setBackgroundMode(Qt::NoBackground);
#endif

  main_window->resize(1024,600);
  editor->setMinimumWidth(600);
  editor->setMinimumHeight(400);

  main_window->setCaption("Radium editor window");
  main_window->statusBar()->message( "Ready", 2000 );

#if 1
  {
    QStatusBar *status_bar = main_window->statusBar();
    g_bottom_bar = new Bottom_bar_widget(main_window);
    status_bar->addWidget(g_bottom_bar, 1, true);
    //main_window->statusBar()->addWidget(g_bottom_bar, 1, true);
    editor->status_label = g_bottom_bar->status_label;
    //main_window->statusBar()->setFrameStyle(QFrame::NoFrame);
    {
      QColor system_color(SETTINGS_read_string("system_color","#d2d0d5"));
      QPalette pal(status_bar->palette());
      pal.setColor( QPalette::Active, QColorGroup::Dark, system_color);
      pal.setColor( QPalette::Active, QColorGroup::Light, system_color);
      pal.setColor( QPalette::Inactive, QColorGroup::Dark, system_color);
      pal.setColor( QPalette::Inactive, QColorGroup::Light, system_color);
      pal.setColor( QPalette::Disabled, QColorGroup::Dark, system_color);
      pal.setColor( QPalette::Disabled, QColorGroup::Light, system_color);
      status_bar->setPalette(pal);
    }
    {
      QColor system_color(SETTINGS_read_string("system_color","#d2d0d5"));
      QPalette pal(g_bottom_bar->palette());
      pal.setColor( QPalette::Active, QColorGroup::Dark, system_color);
      pal.setColor( QPalette::Active, QColorGroup::Light, system_color);
      pal.setColor( QPalette::Inactive, QColorGroup::Dark, system_color);
      pal.setColor( QPalette::Inactive, QColorGroup::Light, system_color);
      pal.setColor( QPalette::Disabled, QColorGroup::Dark, system_color);
      pal.setColor( QPalette::Disabled, QColorGroup::Light, system_color);
      g_bottom_bar->setPalette(pal);
    }
    main_window->setStyleSheet("QStatusBar::item { border: 0px solid black }; ");
    status_bar->setSizeGripEnabled(false);
  }
#else
  {
    QStatusBar *status_bar = main_window->statusBar();
    editor->status_label = new QLabel(status_bar);//"");
    editor->status_label->setFrameStyle(QFrame::Sunken);
    //editor->status_frame->
    status_bar->addWidget(editor->status_label, 1, true);
  }
#endif


  QApplication::instance()->setAttribute(Qt::AA_DontUseNativeMenuBar);

#if FOR_MACOSX
  //qt_mac_set_native_menubar(false);
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

    //const char *fontstyle = SETTINGS_read_string("font_style",NULL);  //(arrgh, there's a billion bugs in qt when it comes to font styles)
    //if(fontstyle!=NULL)
    //  font.setStyleName(fontstyle);

    editor->font = font;

    //editor->font->setStyleHint(QFont::TypeWriter);
    //editor->font->setFixedPitch(false);
  }


  editor->init_buffers();

#endif // USE_QT_VISUAL

  g_editor = editor;
}

void GFX_SetMinimumWindowWidth(struct Tracker_Windows *tvisual, int width){
#if 0
  EditorWidget *editor=(EditorWidget *)tvisual->os_visual.widget;
  editor->setMinimumWidth(width);
#endif
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

const char *GFX_GetLoadFileName(
	struct Tracker_Windows *tvisual,
	ReqType reqtype,
	char *seltext,
	char *dir
){
  EditorWidget *editor=(EditorWidget *)tvisual->os_visual.widget;
  const char *ret;

  num_users_of_keyboard++;

  QString filename = QFileDialog::getOpenFileName(editor,seltext);

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
  EditorWidget *editor=(EditorWidget *)tvisual->os_visual.widget;

  num_users_of_keyboard++;
  const char *ret = talloc_strdup((char*)QFileDialog::getSaveFileName(editor,seltext).ascii());
  num_users_of_keyboard--;
  return ret==NULL || strlen(ret)==0 
    ? NULL 
    : ret;
}

static int show_message(vector_t *buttons, const char *message){
        

  if(buttons==NULL){

    QMessageBox msgBox;
    msgBox.setText(QString(message));
    msgBox.setStandardButtons(QMessageBox::Ok);

    num_users_of_keyboard++;
    {
      msgBox.exec();
    }
    num_users_of_keyboard--;

    return 0;
  }

  RWarning("what?");

  return 0;

#if 0
  VECTOR_FOR_EACH(const char *,button_text,buttons){
    msgBox.addButton(button_text);
  }END_VECTOR_FOR_EACH;
#endif
}


int GFX_Message(vector_t *buttons, const char *fmt,...){
  char message[1000];
  va_list argp;
  
  va_start(argp,fmt);
  /*	vfprintf(stderr,fmt,argp); */
  vsprintf(message,fmt,argp);
  va_end(argp);
  
  return show_message(buttons,message);
}

//#include "mgakk.cpp"

