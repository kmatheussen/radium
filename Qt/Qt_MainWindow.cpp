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

#include "EditorWidget.h"

#include "Qt_colors_proc.h"
#include "Qt_Menues_proc.h"


#if USE_GTK_VISUAL
#  include "qtxembed-1.3-free/src/qtxembed.h"
#  include "GTK_visual_proc.h"
   QtXEmbedContainer *g_embed_container;
#endif  // USE_GTK_VISUAL

#if USE_QT_VISUAL
#  include "Qt_Fonts_proc.h"
#endif

#include "../common/settings_proc.h"
#include "../common/eventreciever_proc.h"

#include "Qt_MainWindow_proc.h"

#if 1
class MyQtXEmbedContainer : public QtXEmbedContainer{
public:
  MyQtXEmbedContainer(QWidget *widget)
    : QtXEmbedContainer(widget)
  {
    setWFlags(Qt::WStaticContents | Qt::WResizeNoErase | Qt::WRepaintNoErase | Qt::WNoAutoErase);
  }
  void paintEvent( QPaintEvent *e ){
    printf("got emb paint event\n");
  }
};
#endif


EditorWidget::EditorWidget(QWidget *parent, const char *name )
  //: QFrame( parent, name, Qt::WStaticContents | Qt::WResizeNoErase | Qt::WRepaintNoErase | Qt::WNoAutoErase )
  : QWidget( parent, name, Qt::WStaticContents | Qt::WResizeNoErase | Qt::WRepaintNoErase | Qt::WNoAutoErase )
    //: QtXEmbedContainer( parent, name) //, Qt::WStaticContents | Qt::WResizeNoErase | Qt::WRepaintNoErase | Qt::WNoAutoErase )
  , qpa(256)
{
  this->qpixmap=NULL;

#if USE_GTK_VISUAL
  if(sizeof(int64_t) < sizeof(WId))
    abort();

  if(g_embed_container==NULL){
    //g_embed_container = this;
    g_embed_container = new MyQtXEmbedContainer(this);
    //g_embed_container = new QtXEmbedContainer(this);
    g_embed_container->setBackgroundMode(Qt::NoBackground);
    g_embed_container->show();

    //g_embed_container->setSizePolicy(QSizePolicy::Maximum,QSizePolicy::Maximum);

    //usleep(1*1000*1000);

    //GTK_CreateVisual(g_embed_container->winId());

    // g_embed_container is not visible now (it's complicated), but by calling embed(), the gdk widget still shows up, for some reason.
    // Don't know if this is an okay workaround (or why it even is a workaround). Going to clean up this mess later, hopefully.
    g_embed_container->embed(GTK_CreateVisual(g_embed_container->winId()),true);

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
  }
#endif

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

class MyQMainWindow : public QMainWindow{
public:
  MyQMainWindow() : QMainWindow(NULL,"Radium") {}
  void closeEvent(QCloseEvent *ce){
    struct Tracker_Windows *window=static_cast<struct Tracker_Windows*>(root->song->tracker_windows);
    doquit = Quit(window);
  }
};

void SetupMainWindow(void){

  //QMainWindow *main_window = new QMainWindow(NULL, "Radium", Qt::WStyle_Customize | Qt::WStyle_NoBorder);// | Qt::WStyle_Dialog);
  QMainWindow *main_window = new MyQMainWindow();//NULL, "Radium");

#ifdef USE_QT4
  main_window->setAttribute(Qt::WA_PaintOnScreen);
  main_window->setAttribute(Qt::WA_OpaquePaintEvent);
  main_window->setAttribute(Qt::WA_NoSystemBackground);
#endif
  main_window->setBackgroundMode(Qt::NoBackground);

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

  main_window->setBackgroundMode(Qt::NoBackground);

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

  initMenues(main_window->menuBar());

#if USE_QT_VISUAL

  {
    QFont font = QFont("Monospace");

    char *fontstring = SETTINGS_read_string((char*)"font",NULL);
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


void GFX_SetWindowTitle(struct Tracker_Windows *tvisual,char *title){
  QMainWindow *main_window = (QMainWindow *)tvisual->os_visual.main_window;
  main_window->setCaption(title);
}

void GFX_SetStatusBar(struct Tracker_Windows *tvisual,char *title){
  //QMainWindow *main_window = (QMainWindow *)tvisual->os_visual.main_window;
  //main_window->statusBar()->message(title);

  EditorWidget *editor=(EditorWidget *)tvisual->os_visual.widget;
  editor->status_label->setText(title);
}


