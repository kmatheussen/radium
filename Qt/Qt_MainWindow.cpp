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

#include "../common/includepython.h"
#include "../api/api_proc.h"

#include <unistd.h>

#include <QApplication>
#include <QMainWindow>
#include <QSplitter>
#include <QCloseEvent>
#include <QStatusBar>
#include <QMenuBar>
#include <QUrl>
#include <QMimeData>
#include <QFileDialog>

extern bool g_qt_is_running;

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
#include "../common/threading.h"

#if USE_GTK_VISUAL
#  include "../GTK/GTK_visual_proc.h"
#endif

#include "../common/gfx_proc.h"
#include "../common/cursor_updown_proc.h"
#include "../common/OS_string_proc.h"
#include "../common/disk_load_proc.h"

#include "Qt_colors_proc.h"
#include "Qt_Menues_proc.h"


#if USE_QT_VISUAL
#  include "Qt_Fonts_proc.h"
#endif

#include "../common/settings_proc.h"
#include "../common/eventreciever_proc.h"
#include "../common/OS_settings_proc.h"

#include "../audio/SoundPlugin.h"
#include "../audio/SoundPluginRegistry_proc.h"
#include "../audio/Sampler_plugin_proc.h"

#include "Qt_MyQSlider.h"
#include "Qt_MyQCheckBox.h"


class Bottom_bar_widget;
static Bottom_bar_widget *g_bottom_bar = NULL; // need to be defined here since it's used by the upperleft widget.

#include "mQt_bottom_bar_widget_callbacks.h"
#include "mQt_upperleft_widget_callbacks.h"
#include "../OpenGL/Widget_proc.h"

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


void GFX_PositionUpperLeftArea(struct Tracker_Windows *tvisual, struct WBlocks *wblock){
  EditorWidget *editor=(EditorWidget *)tvisual->os_visual.widget;
  editor->upperleft_widget->position(tvisual, wblock);
}

void GFX_UpdateUpperLeft(struct Tracker_Windows *tvisual, struct WBlocks *wblock){
  EditorWidget *editor=(EditorWidget *)tvisual->os_visual.widget;
  editor->upperleft_widget->updateWidgets(wblock);
}

EditorWidget::EditorWidget(QWidget *parent, const char *name )
  //: QFrame( parent, name, Qt::WStaticContents | Qt::WResizeNoErase | Qt::WRepaintNoErase | Qt::WNoAutoErase )
  : QWidget( parent)
    //: QWidget( parent, name) //, Qt::WStaticContents | Qt::WResizeNoErase | Qt::WRepaintNoErase | Qt::WNoAutoErase )
    //: EditorWidgetParent( parent, name) //, Qt::WStaticContents | Qt::WResizeNoErase | Qt::WRepaintNoErase | Qt::WNoAutoErase )
#if USE_QT_VISUAL && !USE_OPENGL
  , paintbuffer(NULL)
  , cursorbuffer(NULL)
  , paintbuffer_painter(NULL)
  , cursorbuffer_painter(NULL)
#endif

#if USE_OPENGL
  , gl_widget(NULL)
#else    
  , qpa(256)
#endif
{
#if USE_QT_VISUAL && !USE_OPENGL
  this->paintbuffer=NULL;
#endif

  setAttribute(Qt::WA_StaticContents, true);
  
  upperleft_widget = new Upperleft_widget(this);
  upperleft_widget->move(0,0);

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

  //setEditorColors(this);

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

#if 1
// dangerous stuff
extern "C" void grabKeyboard(void);
void grabKeyboard(void){
  //g_embed_container->grabKeyboard();
  //g_editor->main_window->grabKeyboard();
  //abort(); // This function should not be used.
  // GL_lock is needed when using intel gfx driver to avoid crash caused by opening two opengl contexts simultaneously from two threads.
  GL_lock();{
    g_editor->setFocus();
  }GL_unlock();
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
  GL_lock();{
    g_editor->show();
  }GL_unlock();
}

void GFX_HideEditor(void){
  g_editor->hide();
}

void GFX_showHideEditor(void){
  GL_lock();{
    if(g_editor->isHidden())
      g_editor->show();
    else
      g_editor->hide();
  }GL_unlock();
}

static int get_track_from_x(float x){
  if (x < 0 || x < getEditorX1(-1) || x>=getEditorX2(-1))
    return -1;

  int num_tracks = getNumTracks(-1);
  for(int i=0;i<num_tracks;i++){
    if( x >= getTrackX1(i,-1,-1) && x < getTrackX2(i,-1,-1))
      return i;
  }

  return -1;
}

void handleDropEvent(QString filename, float x){
  struct Tracker_Windows *window=static_cast<struct Tracker_Windows*>(root->song->tracker_windows);

  int tracknum = get_track_from_x(x);
  int64_t instrument_id = -1;

  Undo_Open();{
    
    if (filename.endsWith(".rad"))
      LoadSong_CurrPos(window, STRING_create(filename));
    
    else if (filename.endsWith(".rec"))
      instrument_id = createAudioInstrumentFromPreset(filename.toUtf8().constData(), NULL);
    
    else if (file_could_be_a_sample(filename) || filename.endsWith(".sf2")){
      struct Patch *patch = PATCH_create_audio("Sample Player", "Sample Player", NULL, NULL);
      instrument_id = patch->id;
      SoundPlugin *plugin = (SoundPlugin*)patch->patchdata;
      SAMPLER_set_new_sample(plugin, STRING_create(filename), 0);
    }
    
    if (instrument_id != -1) {
      connectAudioInstrumentToMainPipe(instrument_id);
      
      if (tracknum != -1)
        setInstrumentForTrack(instrument_id, tracknum, -1, -1);

      GFX_update_instrument_patch_gui(PATCH_get_from_id(instrument_id));
    }

  }Undo_Close();
}


namespace{
class MyQMainWindow : public QMainWindow{
  //Q_OBJECT;

public:
  MyQMainWindow() : QMainWindow(NULL) {
    setAcceptDrops(true);
  }

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

#if 0
  // Want the wheel to work from everywhere. (actually we don't want that)
  void wheelEvent(QWheelEvent *qwheelevent) {
    if(ATOMIC_GET(is_starting_up)==true)
      return;
    printf("Got wheel event\n");
    g_editor->wheelEvent(qwheelevent);
  }
#endif

  void dragEnterEvent(QDragEnterEvent *e){
    e->acceptProposedAction();
  }

  void dropEvent(QDropEvent *event){
      printf("Got drop event\n");
  if (event->mimeData()->hasUrls())
    {
      foreach (QUrl url, event->mimeData()->urls())
        {
          printf(" Filepath: -%s-\n",url.toLocalFile().toUtf8().constData());          
          QString filename = url.toLocalFile();
          handleDropEvent(filename, event->pos().x());
        }
    }
  }

};
}


QMainWindow *g_main_window = NULL;

void SetupMainWindow(void){

  //QMainWindow *main_window = new QMainWindow(NULL, "Radium", Qt::WStyle_Customize | Qt::WStyle_NoBorder);// | Qt::WStyle_Dialog);
  QMainWindow *main_window = new MyQMainWindow();//NULL, "Radium");
  g_main_window = main_window;
  
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
  // GL_lock is needed when using intel gfx driver to avoid crash caused by opening two opengl contexts simultaneously from two threads.
  GL_lock();{
    editor->setFocus(); // Lots of trouble with focus with the qt_visual backend.
  }GL_unlock();
#endif

#ifdef USE_QT4
#if USE_GTK_VISUAL
  editor->setAttribute(Qt::WA_PaintOnScreen);
#endif
#if USE_QT_VISUAL //&& !defined(__linux__)  // double buffer actually improves performance on linux. Still not as good as gtk though.
  //editor->setAttribute(Qt::WA_PaintOnScreen);
#endif

  editor->setAcceptDrops(true);
                 
  //#if USE_OPENGL
    // editor->setAttribute(Qt::WA_PaintOnScreen);
  //#endif

  //editor->setAttribute(Qt::WA_PaintOnScreen);

  editor->setAttribute(Qt::WA_OpaquePaintEvent);
  editor->setAttribute(Qt::WA_NoSystemBackground);
#endif
  editor->main_window = main_window;

#ifdef USE_QT3
  main_window->setBackgroundMode(Qt::NoBackground);
#endif

  main_window->resize(1024,550);
  editor->setMinimumWidth(550);
  editor->setMinimumHeight(400);

  main_window->setWindowTitle("Radium editor window");
  main_window->statusBar()->showMessage( "Ready", 2000 );

#if 1
  {
    QStatusBar *status_bar = main_window->statusBar();
    g_bottom_bar = new Bottom_bar_widget(main_window);
    status_bar->addWidget(g_bottom_bar, 1);//, true);
    g_bottom_bar->show();
    //main_window->statusBar()->addWidget(g_bottom_bar, 1, true);
    editor->status_label = g_bottom_bar->status_label;
    //main_window->statusBar()->setFrameStyle(QFrame::NoFrame);
    {
      QColor system_color(SETTINGS_read_string("system_color","#d2d0d5"));
      QPalette pal(status_bar->palette());
      pal.setColor( QPalette::Active, QPalette::Dark, system_color);
      pal.setColor( QPalette::Active, QPalette::Light, system_color);
      pal.setColor( QPalette::Inactive, QPalette::Dark, system_color);
      pal.setColor( QPalette::Inactive, QPalette::Light, system_color);
      pal.setColor( QPalette::Disabled, QPalette::Dark, system_color);
      pal.setColor( QPalette::Disabled, QPalette::Light, system_color);
      status_bar->setPalette(pal);
    }
    {
      QColor system_color(SETTINGS_read_string("system_color","#d2d0d5"));
      QPalette pal(g_bottom_bar->palette());
      pal.setColor( QPalette::Active, QPalette::Dark, system_color);
      pal.setColor( QPalette::Active, QPalette::Light, system_color);
      pal.setColor( QPalette::Inactive, QPalette::Dark, system_color);
      pal.setColor( QPalette::Inactive, QPalette::Light, system_color);
      pal.setColor( QPalette::Disabled, QPalette::Dark, system_color);
      pal.setColor( QPalette::Disabled, QPalette::Light, system_color);
      g_bottom_bar->setPalette(pal);
    }
    main_window->setStyleSheet("QStatusBar::item { border: 0px solid black }; ");
    status_bar->setSizeGripEnabled(false);

    {
      QColor system_color(SETTINGS_read_string("system_color","#d2d0d5"));
      status_bar->setStyleSheet("#frame { border: 1px solid " + system_color.darker(150).name() + "; }");
    }
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
  main_window->menuBar()->show();
  main_window->menuBar()->setNativeMenuBar(false);
#endif

#if USE_QT_VISUAL

  {
    bool custom_config_set = false;
    QFont font = QFont("Monospace");

    const char *fontstring = SETTINGS_read_string("font",NULL);
    if(fontstring==NULL) {
      SETTINGS_set_custom_configfile(OS_get_full_program_file_path("config"));
      fontstring = SETTINGS_read_string("font",NULL);
      R_ASSERT(fontstring != NULL);
      custom_config_set = true;
    }

    font.fromString(fontstring);

#if 0 //FOR_MACOSX
    if (custom_config_set==true)
      font.setPointSize(font.pointSize()*96.0/72.0); // macs have dpi of 72, while linux and windows have 96.
#endif

    if(SETTINGS_read_string("font_style",NULL)!=NULL)
      font.setStyleName(SETTINGS_read_string("font_style",NULL));

    editor->font = font;

    if (custom_config_set==true){
      SETTINGS_unset_custom_configfile();
    }

    //editor->font->setStyleHint(QFont::TypeWriter);
    //editor->font->setFixedPitch(false);
  }


#if !USE_OPENGL
  editor->init_buffers();
#endif
  
#endif // USE_QT_VISUAL

  g_editor = editor;
}

void GFX_SetMinimumWindowWidth(struct Tracker_Windows *tvisual, int width){
#if 0
  EditorWidget *editor=(EditorWidget *)tvisual->os_visual.widget;
  editor->setMinimumWidth(width);
#endif
}


void GFX_SetWindowTitle(struct Tracker_Windows *tvisual,const wchar_t *title){
  QMainWindow *main_window = (QMainWindow *)tvisual->os_visual.main_window;
  main_window->setWindowTitle(STRING_get_qstring(title));
}

void GFX_SetStatusBar(struct Tracker_Windows *tvisual,const char *title){
  //QMainWindow *main_window = (QMainWindow *)tvisual->os_visual.main_window;
  //main_window->statusBar()->message(title);

  EditorWidget *editor=(EditorWidget *)tvisual->os_visual.widget;
  editor->status_label->setText(title);
}

static QString get_postfixes_filter(const char *postfixes){
  QString postfixes2 = postfixes==NULL ? "*.rad *.mmd *.mmd2 *.mmd3 *.MMD *.MMD2 *.MMD3" : QString(postfixes);
  
#if FOR_WINDOWS
  return postfixes2 + " ;; All files (*)";
#else
  return QString("Song files (") + postfixes2 + ") ;; All files (*)";
#endif
}

const wchar_t *GFX_GetLoadFileName(
                                   struct Tracker_Windows *tvisual,
                                   ReqType reqtype,
                                   const char *seltext,
                                   wchar_t *wdir,
                                   const char *postfixes
){
  EditorWidget *editor=(EditorWidget *)tvisual->os_visual.widget;

  obtain_keyboard_focus();

  QString filename;
  
  GL_lock();{ // GL_lock is needed when using intel gfx driver to avoid crash caused by opening two opengl contexts simultaneously from two threads.
  
    QString dir = wdir==NULL ? "" : QString::fromWCharArray(wdir);
    filename = QFileDialog::getOpenFileName(editor,
                                            seltext,
                                            dir,
                                            get_postfixes_filter(postfixes),
                                            0,
                                            useNativeFileRequesters() ? (QFileDialog::Option)0 : QFileDialog::DontUseNativeDialog
                                            );
    
  }GL_unlock();

  release_keyboard_focus();
    
  if(filename == "")
    return NULL;
  else
    return STRING_create(filename);  
}

const wchar_t *GFX_GetSaveFileName(
                                   struct Tracker_Windows *tvisual,
                                   ReqType reqtype,
                                   const char *seltext,
                                   wchar_t *wdir,
                                   const char *postfixes
                                   ){
  EditorWidget *editor=(EditorWidget *)tvisual->os_visual.widget;

  obtain_keyboard_focus();

  QString filename;
  
  GL_lock();{ // GL_lock is needed when using intel gfx driver to avoid crash caused by opening two opengl contexts simultaneously from two threads.

    QString dir = wdir==NULL ? "" : QString::fromWCharArray(wdir);
    filename = QFileDialog::getSaveFileName(editor,
                                            seltext,
                                            "",
                                            get_postfixes_filter(postfixes),
                                            0,
                                            useNativeFileRequesters() ? (QFileDialog::Option)0 : QFileDialog::DontUseNativeDialog
                                            );
  }GL_unlock();

  release_keyboard_focus();

  if (filename == "")
    return NULL;

  return STRING_create(filename);
}


static int GFX_Message(vector_t *buttons, QString message){
  R_ASSERT(THREADING_is_main_thread());

  MyQMessageBox msgBox(g_editor);
  
  msgBox.setText(message);

  if(buttons==NULL){

    msgBox.setStandardButtons(QMessageBox::Ok);

  } else {

    VECTOR_FOR_EACH(const char *,button_text,buttons){
      msgBox.addButton(button_text, QMessageBox::AcceptRole);
    }END_VECTOR_FOR_EACH;

  }

  RememberGeometryQDialog::num_open_dialogs++;
  safeExec(msgBox);
  RememberGeometryQDialog::num_open_dialogs--;
  
  QAbstractButton *clicked_button = msgBox.clickedButton();

  if (buttons != NULL) {
    for(int i=0;i<buttons->num_elements;i++)
      if(QString((char*)buttons->elements[i])==clicked_button->text())
        return i;

    fprintf(stderr,"******************** \n\n\n\n  ******************* \n\n\n ******** Somethings not working in GFX_Message *************** \n\n\n");
  }
  
  return 0;
}

int GFX_Message(vector_t *buttons, const char *fmt,...){
  if (buttons!=NULL)
    R_ASSERT(THREADING_is_main_thread());

  char message[1000];
  va_list argp;
  
  va_start(argp,fmt);
  /*	vfprintf(stderr,fmt,argp); */
  vsprintf(message,fmt,argp);
  va_end(argp);

  if (g_qt_is_running==false || !THREADING_is_main_thread()) {

    SYSTEM_show_message(message);
    return -1;

  } else {
 
    return GFX_Message(buttons,QString(message));

  }
}

/*
QXcbConnection: XCB error: 3 (BadWindow), sequence: 8449, resource id: 8913535, major code: 40 (TranslateCoords), minor code: 0

Program received signal SIGABRT, Aborted.
0x0000003b49435935 in raise () from /lib64/libc.so.6
Missing separate debuginfos, use: debuginfo-install cyrus-sasl-lib-2.1.23-31.fc17.x86_64 fftw-libs-single-3.3.3-5.fc17.x86_64 flac-1.2.1-9.fc17.x86_64 glibc-2.15-59.fc17.x86_64 gsm-1.0.13-6.fc17.x86_64 gstreamer-0.10.36-1.fc17.x86_64 libcurl-7.24.0-10.fc17.x86_64 libidn-1.24-1.fc17.x86_64 liblrdf-0.5.0-3.fc17.x86_64 libssh2-1.4.1-2.fc17.x86_64 libudev-182-3.fc17.x86_64 libvorbis-1.3.3-1.fc17.x86_64 libxml2-2.7.8-9.fc17.x86_64 libxslt-1.1.26-10.fc17.x86_64 nspr-4.10.0-3.fc17.x86_64 nss-3.14.3-2.fc17.x86_64 nss-mdns-0.10-10.fc17.x86_64 nss-util-3.14.3-1.fc17.x86_64 openldap-2.4.33-3.fc17.x86_64 orc-0.4.17-2.fc17.x86_64 raptor2-2.0.7-1.fc17.x86_64 tslib-1.0-4.fc17.x86_64 xcb-util-0.3.8-2.fc17.x86_64 xcb-util-image-0.3.8-3.fc17.x86_64 xcb-util-keysyms-0.3.8-3.fc17.x86_64 xcb-util-renderutil-0.3.8-1.fc17.x86_64 xcb-util-wm-0.3.8-2.fc17.x86_64 yajl-2.0.4-1.fc17.x86_64
(gdb) bt
#0  0x0000003b49435935 in raise () from /lib64/libc.so.6
#1  0x0000003b494370e8 in abort () from /lib64/libc.so.6
#2  0x00007ffff46dceda in qt_message_fatal (context=..., message=...) at global/qlogging.cpp:1680
#3  QMessageLogger::warning (this=<optimized out>, msg=<optimized out>) at global/qlogging.cpp:560
#4  0x00007ffff26dad1e in QXcbConnection::handleXcbError (this=0x5b31970, error=0xcc941b0) at qxcbconnection.cpp:1000
#5  0x00007ffff26e0c6b in QXcbConnection::processXcbEvents (this=0x5b31970) at qxcbconnection.cpp:1712
#6  0x00007ffff48e7c21 in QObject::event (this=0x5b31970, e=<optimized out>) at kernel/qobject.cpp:1263
#7  0x00007ffff782689c in QApplicationPrivate::notify_helper (this=<optimized out>, receiver=0x5b31970, e=0x7fffe0005e30) at kernel/qapplication.cpp:3799
#8  0x00007ffff782dcb6 in QApplication::notify (this=0x5b21fb0, receiver=0x5b31970, e=0x7fffe0005e30) at kernel/qapplication.cpp:3556
#9  0x00007ffff48bdc98 in QCoreApplication::notifyInternal2 (receiver=0x5b31970, event=event@entry=0x7fffe0005e30) at kernel/qcoreapplication.cpp:988
#10 0x00007ffff48c028b in sendEvent (event=0x7fffe0005e30, receiver=<optimized out>) at ../../include/QtCore/../../src/corelib/kernel/qcoreapplication.h:231
#11 QCoreApplicationPrivate::sendPostedEvents (receiver=0x0, event_type=0, data=0x5b0d5c0) at kernel/qcoreapplication.cpp:1649
#12 0x00007ffff490e183 in postEventSourceDispatch (s=s@entry=0x5b762a0) at kernel/qeventdispatcher_glib.cpp:276
#13 0x0000003b4b047825 in g_main_dispatch (context=0x7fffe0002e00) at gmain.c:2539
#14 g_main_context_dispatch (context=context@entry=0x7fffe0002e00) at gmain.c:3075
#15 0x0000003b4b047b58 in g_main_context_iterate (context=context@entry=0x7fffe0002e00, block=block@entry=0, dispatch=dispatch@entry=1, self=<optimized out>) at gmain.c:3146
#16 0x0000003b4b047c14 in g_main_context_iteration (context=0x7fffe0002e00, may_block=may_block@entry=0) at gmain.c:3207
#17 0x00007ffff490e58f in QEventDispatcherGlib::processEvents (this=0x5b676a0, flags=...) at kernel/qeventdispatcher_glib.cpp:423
#18 0x00000000006c63f5 in GFX_OpenProgress (message=<optimized out>, message@entry=0xb523d30 "Please wait, loading /home/kjetil/Downloads/estrayk_-_horizon.mod") at Qt/Qt_MainWindow.cpp:731
#19 0x00000000007086cd in openProgressWindow (message=message@entry=0xb523d30 "Please wait, loading /home/kjetil/Downloads/estrayk_-_horizon.mod") at api/api_requesters.c:180
#20 0x00000000005f5b46 in radium_s7_openProgressWindow (radiums7_sc=0x5fe9770, radiums7_args=<optimized out>) at api/radium_s7_wrap.c:11671
 */

/*
static QMessageBox *progressBox = NULL;

void GFX_OpenProgress(const char *message){
  delete progressBox;

  progressBox = new QMessageBox(g_editor);
  progressBox->setStandardButtons(0);
  progressBox->setText(QString(message) + "             \n            \n              \n                \n               \n");
  safeShow(progressBox);
  for(int i=0; i < 1000 ; i++){
    progressBox->repaint();
    QCoreApplication::processEvents(QEventLoop::ExcludeUserInputEvents);
    usleep(10);
  }
}

void GFX_ShowProgressMessage(const char *message){
  if (progressBox == NULL)
    GFX_OpenProgress("...");

  progressBox->setInformativeText(message);
  progressBox->repaint();
  QCoreApplication::processEvents(QEventLoop::ExcludeUserInputEvents);
}

void GFX_CloseProgress(void){
  delete progressBox;
  progressBox = NULL;
}
*/

const char *GFX_qVersion(void){
  return qVersion();
}


  
void GFX_showChanceHelpWidget(void){
    static MyQMessageBox *msgBox = new MyQMessageBox;
        
    msgBox->setText("Chance text");
    msgBox->setInformativeText(
                               QString(talloc_format("<pre><span class=\"inner-pre\" style=\"font-size: %dpx\">", QApplication::font().pointSize()*2)) +
                               "Chance text lets you set the probability for this note or pitch to play.\n"
                               "\n"
                               "Chance text format: \"xx\", where\n"
                               "\n"
                               "  0 = lowest chance, ff = highest chance. (hex format)\n"
                               "\n"
                               "Examples:\n"
                               "\n"
                               "  xx = 80: 50% chance of this note playing\n"
                               "  xx =  0: 0% chance of this note playing\n"
                               "  xx = ff: 255/256 chance of this note playing\n"
                               "\n"
                               "To remove a chance text (to make sure the note is always played), press DEL.\n"
                               "\n"
                               "To control the chance of individual voices in the note duplicator,\n"
                               "you can automate the \"System Chance 1\", \"System Chance 2\", etc. effects instead.\n"
                               "(Note that the value \"ff\" is interpreted as 256 when automating the chance\n"
                               "of individual voices in the note duplicator so you don't have to worry about\n"
                               "the voices not always playing.)\n"
                               "\n"                               
                               "</scan></pre>"                               
                               );
    msgBox->setStandardButtons(QMessageBox::Ok);

    msgBox->setModal(false);
     
    safeShowOrExec(msgBox);
}

  

void GFX_showVelocityHelpWidget(void){
    static MyQMessageBox *msgBox = new MyQMessageBox;
        
    msgBox->setText("Velocity text");
    msgBox->setInformativeText(
                               QString(talloc_format("<pre><span class=\"inner-pre\" style=\"font-size: %dpx\">", QApplication::font().pointSize()*2)) +
                               "Velocity text is an alternative way to edit the velocities of a note.\n"
                               "Editing text is often quicker than editing velocities graphically.\n"
                               "\n"
                               "Key bindings:\n"
                               "\n"
                               "  Left Alt  + Y: Turn on/off velocity text for track\n"
                               "  Left Ctrl + Y: Turn on/off velocity text for all tracks in block\n"
                               "\n"
                               "Velocity text format: \"xxt\"\n"
                               "\n"
                               "  xx = Velocity value. 0 = lowest velocity, ff = highest velocity. (hex format)\n"
                               "   t = Whether to glide to the next velocity or not.\n"
                               "\n"
                               "  Tips: -To quickly add an \"ff\" velocity, press 'G'.\n"
                               "        -To switch glide mode, press 'T'.\n"
                               "\n"                               
                               "</scan></pre>"                               
                               );
    msgBox->setStandardButtons(QMessageBox::Ok);

    msgBox->setModal(false);
     
    safeShowOrExec(msgBox);
}

  

void GFX_showFXHelpWidget(void){
    static MyQMessageBox *msgBox = new MyQMessageBox;
        
    msgBox->setText("FX text");
    msgBox->setInformativeText(
                               QString(talloc_format("<pre><span class=\"inner-pre\" style=\"font-size: %dpx\">", QApplication::font().pointSize()*2)) +
                               "FX text is an alternative way to edit effects.\n"
                               "Editing text is often quicker than editing effects graphically.\n"
                               "\n"
                               "FX text format: \"xxt\"\n"
                               "\n"
                               "  xx = effect value. 0 = lowest value, ff = highest value. (hex format)\n"
                               "   t = Whether to glide to the next effect or not (denoted by '|').\n"
                               "\n"
                               "A special situation:\n"
                               "  Let's say you have an fx with two nodes.\n"
                               "  The first node has the value '6e|', and the second node has the value '28 '.\n"
                               "  Q: Will the value 28 be sent to the instrument?\n"
                               "  A: Yes.\n"
                               "\n"
                               "\n"
                               "Tips:\n"
                               "  -To quickly add an \"ff\" velocity, press 'G'.\n"
                               "  -To switch glide mode, press 'T'.\n"
                               "\n"                               
                               "</scan></pre>"                               
                               );
    msgBox->setStandardButtons(QMessageBox::Ok);

    msgBox->setModal(false);
     
    safeShowOrExec(msgBox);
}

void GFX_showMixerHelpWindow(void){
    static MyQMessageBox *msgBox = new MyQMessageBox;
        
    msgBox->setText("Mixer Interface");
    msgBox->setInformativeText(
                               QString(talloc_format("<pre><span class=\"inner-pre\" style=\"font-size: %dpx\">", QApplication::font().pointSize()*2)) +
                              "* Move objects with right mouse button.\n"
                              "\n"
                              "* Double-click the name of an object to open GUI.\n"
                              "\n"
                              "* Delete objects or connections by pressing SHIFT and click left (or right).\n"
                              "  - Alternatively, click with middle mouse button.\n"
                              "\n"
                              "* Select more than one object by holding CTRL when clicking.\n"
                              "  - Alternatively, mark an area of objects with left mouse button.\n"
                              "\n"
                              "* To autoconnect a new object to an existing object, right-click at \n"
                              "   the input or output of an existing object.\n"
                              "\n"
                              "* To move and connect an object in one operation, place one object on \n"
                              "   top of another object.\n"
                              "\n"
                              "* Zoom in and out by pressing CTRL and using the scroll wheel.\n"
                               "</scan></pre>"                               
                              );
    msgBox->setStandardButtons(QMessageBox::Ok);
    msgBox->setModal(false);
    
    safeShowOrExec(msgBox);
}


