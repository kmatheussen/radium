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

#define __STDC_FORMAT_MACROS 1

#include "../common/includepython.h"

#include <inttypes.h>

#include "../api/api_proc.h"

#include <unistd.h> // Must be placed after includepyton/api_proc to avoid compilation error.

#include <QApplication>
//#include <QSplitter>
#include <QCloseEvent>
#include <QStatusBar>
#include <QMenuBar>
#include <QUrl>
#include <QMimeData>
#include <QFileDialog>

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
static QVector<Bottom_bar_widget*> g_bottom_bars; // need to be defined here since it's used by the upperleft widget.

#include "mQt_bottom_bar_widget_callbacks.h"
#include "mQt_upperleft_widget_callbacks.h"
#include "../OpenGL/Widget_proc.h"

#include "Qt_MainWindow_proc.h"


QVector<QWidget*> g_static_toplevel_widgets;

/*
struct MyQMenuBar : QMenuBar {
  void hideEvent (QHideEvent * event_){
    QMenuBar::hideEvent(event_);
    set_editor_focus();
  }                                    
};
*/

bool g_user_interaction_enabled = true;


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
  void paintEvent( QPaintEvent *e ) override {
    TRACK_PAINT();
    
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

    //msleep(1*1000);

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

    //msleep(1*1000);
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


/*
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
*/

#endif


bool GFX_EditorIsVisible(void){
  return !g_editor->editor_layout_widget->isHidden();
}

void GFX_ShowEditor(void){
  GL_lock();{
    g_editor->editor_layout_widget->show();
  }GL_unlock();

  set_editor_focus();
}

void GFX_HideEditor(void){
  g_editor->editor_layout_widget->hide();

  set_editor_focus();
}

void GFX_showHideEditor(void){
  GL_lock();{
    if(g_editor->editor_layout_widget->isHidden())
      g_editor->editor_layout_widget->show();
    else
      g_editor->editor_layout_widget->hide();
  }GL_unlock();
  
  set_editor_focus();
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

  RETURN_IF_DATA_IS_INACCESSIBLE();

  int tracknum = get_track_from_x(x);
  int64_t instrument_id = -1;

  UNDO_OPEN();{
    
    if (filename.endsWith(".rad"))
      LoadSong_CurrPos(window, STRING_create(filename));
    
    else if (filename.endsWith(".rec"))
      instrument_id = createAudioInstrumentFromPreset(filename.toUtf8().constData(), NULL, 0, 0, true);
    
    else if (filename.endsWith(".mrec"))
      instrument_id = createAudioInstrumentFromPreset(filename.toUtf8().constData(), NULL, 0, 0, true);
    
    else if (file_could_be_a_sample(filename) || filename.endsWith(".sf2")){
      struct Patch *patch = PATCH_create_audio("Sample Player", "Sample Player", NULL, NULL, true, 0, 0);
      instrument_id = patch->id;
      SoundPlugin *plugin = (SoundPlugin*)patch->patchdata;
      SAMPLER_set_new_sample(plugin, STRING_create(filename), 0);
      Sample_requester_widget *w = AUDIOWIDGET_get_sample_requester_widget(patch);
      if (w==NULL)
        R_ASSERT(false);
      else
        SAMPLEREQUESTER_set_path(w, QFileInfo(filename).absoluteDir().absolutePath());
    }
    
    if (instrument_id != -1) {
      autopositionInstrument(instrument_id);
      connectAudioInstrumentToMainPipe(instrument_id);
      
      if (tracknum != -1)
        setInstrumentForTrack(instrument_id, tracknum, -1, -1);

      GFX_update_instrument_patch_gui(PATCH_get_from_id(instrument_id));
    }

  }UNDO_CLOSE();
}



namespace{
class MyQMainWindow : public QWidget{
  //Q_OBJECT;

public:
  MyQMainWindow(){
    setAcceptDrops(true);
#if FOR_MACOSX
    setWindowFlags(windowFlags () | Qt::WindowFullscreenButtonHint); // F11 doesn't work in sierra. Setting this flag probably doesn't work though.
#endif
    raise();
    activateWindow();
  }

  void closeEvent(QCloseEvent *ce) override{
    CancelMaybeNavigateMenus();
    ce->ignore();
    struct Tracker_Windows *window=static_cast<struct Tracker_Windows*>(root->song->tracker_windows);
    doquit = Quit(window);
#if USE_QT_VISUAL
    if(doquit==true)
      QApplication::quit();
#endif
  }

#if 0
  // Want the wheel to work from everywhere. (actually we don't want that)
  void wheelEvent(QWheelEvent *qwheelevent) override {
    if(g_is_starting_up==true)
      return;
    printf("Got wheel event\n");
    g_editor->wheelEvent(qwheelevent);
  }
#endif

  void dragEnterEvent(QDragEnterEvent *e) override {
    CancelMaybeNavigateMenus();
    e->acceptProposedAction();
  }

  /*
  void keyPressEvent(QKeyEvent *event) override{
    printf("Press. Auto: %d. %d %d\n", event->isAutoRepeat(),event->nativeScanCode(), event->nativeVirtualKey() );
  }
  
  void keyReleaseEvent(QKeyEvent *event) override{
    printf("Release. Auto: %d. %d %d\n", event->isAutoRepeat(),event->nativeScanCode(), event->nativeVirtualKey() );
  }
  */

  void moveEvent(QMoveEvent *event) override {
    CancelMaybeNavigateMenus();
  }

  void resizeEvent(QResizeEvent *event) override {
    CancelMaybeNavigateMenus();
  }

  
  void dropEvent(QDropEvent *event) override {
    printf("Got drop event\n");
    CancelMaybeNavigateMenus();
    if (event->mimeData()->hasUrls()){
      foreach (QUrl url, event->mimeData()->urls()){
        printf(" Filepath: -%s-\n",url.toLocalFile().toUtf8().constData());          
        QString filename = url.toLocalFile();
        handleDropEvent(filename, event->pos().x());
      }
    }
  }

};
}


QWidget *g_main_window = NULL;

void SetupMainWindow(void){

  //QMainWindow *main_window = new QMainWindow(NULL, "Radium", Qt::WStyle_Customize | Qt::WStyle_NoBorder);// | Qt::WStyle_Dialog);
  QWidget *main_window = new MyQMainWindow();//NULL, "Radium");
  g_main_window = main_window;
  g_static_toplevel_widgets.push_back(main_window);


  //QWidget *central_widget = new QWidget(main_window);
  QVBoxLayout *mainLayout = new QVBoxLayout;
  mainLayout->setSpacing(0);
  mainLayout->setContentsMargins(0,0,0,0);
  main_window->setLayout(mainLayout);
  
  //main_window->installEventFilter(main_window);
  
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

  // These two doesn't provide any optimization any more since most things are drawn using OpenGL.
  // But the reason the two lines are commented away is because glClear()/glClearColor doesn't quite work on OSX unless the background is actually clear.
  // We might experience less or more flicker though.
  //

  set_widget_takes_care_of_painting_everything(editor);

  
#if !defined(FOR_MACOSX) // Yes, we definitely experienced more flicker. Turn ON unless running OSX.
  //editor->setAttribute(Qt::WA_OpaquePaintEvent);
  //  editor->setAttribute(Qt::WA_NoSystemBackground); // This one doesn't seem to make a difference though.
#else
  //  #error "Check if colors work if enabling \"editor->setAttribute(Qt::WA_OpaquePaintEvent);\" above."
#endif
  
#endif
  editor->main_window = main_window;

#ifdef USE_QT3
  main_window->setBackgroundMode(Qt::NoBackground);
#endif

  main_window->resize(1024,550);
  editor->setMinimumWidth(550);
  //editor->setMinimumHeight(100);

  main_window->setWindowTitle("Radium editor window");


  //main_window->statusBar()->showMessage( "Ready", 2000 );

#if 1
  {
    //QStatusBar *status_bar = main_window->statusBar();
    auto *bottom_bar = BottomBar_create(main_window, true, true);
    mainLayout->addWidget(bottom_bar, 0); // stretch = 1;
    //bottom_bar->show();
    //main_window->statusBar()->addWidget(bottom_bar, 1, true);
    //editor->status_labels.push_back(bottom_bar->status_label);
    //main_window->statusBar()->setFrameStyle(QFrame::NoFrame);

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

  QMenuBar *menubar = new QMenuBar(0);
  initMenues(menubar);
  menubar->setNativeMenuBar(false);
  
  mainLayout->insertWidget(0, menubar, 0); // position 0, stretch 1.

  
#if 0
#if FOR_MACOSX
  //qt_mac_set_native_menubar(false);
  //menubar->show();
  main_window->setMenuBar(menubar);
  
#else
  //QMenuBar *menubar = new MyQMenuBar();
  initMenues(main_window->menuBar());
  //main_window->setMenuBar(menubar);
  main_window->menuBar()->show();
  main_window->menuBar()->setNativeMenuBar(false);
#endif
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
  QWidget *main_window = (QWidget *)tvisual->os_visual.main_window;
  main_window->setWindowTitle(STRING_get_qstring(title));
}

#include <QToolTip>

static int64_t g_status_bar_id=0;

static void set_status_bar_text(const char *title){
  static int s_len_last_title = 10;
  static char *s_last_title=(char*)calloc(1, s_len_last_title);

  if (strcmp(s_last_title, title)){
    
    //printf("SETTIKNG text -%s-. Before: -%s-\n", title, last_title);
    
    //EditorWidget *editor=(EditorWidget *)tvisual->os_visual.widget;
    for(auto *bottom_bar : g_bottom_bars){      
      bottom_bar->status_label->setText(title);
    }

    int len = strlen(title);
    
    if (len > s_len_last_title){
      s_last_title = (char*)malloc(len+2);
      s_len_last_title = len;
    }
    
    strcpy(s_last_title, title);
  }
}

int64_t GFX_SetStatusBar(const char *title){
  //QMainWindow *main_window = (QMainWindow *)tvisual->os_visual.main_window;
  //main_window->statusBar()->message(title);

  set_status_bar_text(title);
  
  //QToolTip::showText(QCursor::pos(),title,NULL,QRect());

  return ++g_status_bar_id;
}

void GFX_RemoveStatusbarText(int64_t id){
  if (id==g_status_bar_id)
    set_status_bar_text("");
}

void GFX_DisablePainting(void){
  g_main_window->setUpdatesEnabled(false);
}

void GFX_EnablePainting(void){
  g_main_window->setUpdatesEnabled(true);
}



static QString get_postfixes_filter(const char *type, const char *postfixes){
  QString postfixes2 = postfixes==NULL ? "*.rad *.mmd *.mmd2 *.mmd3 *.MMD *.MMD2 *.MMD3" : QString(postfixes);
  
#if FOR_WINDOWS
  return postfixes2 + " ;; All files (*)";
#else
  type = type==NULL ? "Song files" : type;
  return QString(type) + " (" + postfixes2 + ") ;; All files (*)";
#endif
}

const wchar_t *GFX_GetLoadFileName(
                                   struct Tracker_Windows *tvisual,
                                   ReqType reqtype,
                                   const char *seltext,
                                   wchar_t *wdir,
                                   const char *postfixes,
                                   const char *type,
                                   bool program_state_is_valid
){
  EditorWidget *editor=(EditorWidget *)tvisual->os_visual.widget;

  R_ASSERT_RETURN_IF_FALSE2(g_radium_runs_custom_exec==false, NULL);

  radium::ScopedExec scopedExec(program_state_is_valid);

  QString dir = wdir==NULL ? "" : QString::fromWCharArray(wdir);
  QString filename = QFileDialog::getOpenFileName(editor,
                                                  seltext,
                                                  dir,
                                                  get_postfixes_filter(type, postfixes),
                                                  0,
                                                  QFileDialog::DontUseCustomDirectoryIcons | (useNativeFileRequesters() ? (QFileDialog::Option)0 : QFileDialog::DontUseNativeDialog)
                                                  );
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
                                   const char *postfixes,
                                   const char *type,
                                   bool program_state_is_valid
                                   ){
  EditorWidget *editor=(EditorWidget *)tvisual->os_visual.widget;

  R_ASSERT_RETURN_IF_FALSE2(g_radium_runs_custom_exec==false, NULL);

  radium::ScopedExec scopedExec(program_state_is_valid);

  QString dir = wdir==NULL ? "" : QString::fromWCharArray(wdir);
  QString filename = QFileDialog::getSaveFileName(editor,
                                                  seltext,
                                                  "",
                                                  get_postfixes_filter(type, postfixes),
                                                  0,
                                                  QFileDialog::DontUseCustomDirectoryIcons | (useNativeFileRequesters() ? (QFileDialog::Option)0 : QFileDialog::DontUseNativeDialog)
                                                  );
  if (filename == "")
    return NULL;
  else
    return STRING_create(filename);
}

static QVector<QPointer<QWidget>> g_modal_widgets;

static void remove_null_and_nonmodal_widgets(void){
  QVector<QPointer<QWidget>> new_modal_widgets;

  for(QPointer<QWidget> &w : g_modal_widgets){
    QWidget *widget = w.data();
    if (widget != NULL){
      if (widget->isModal()==true){
        new_modal_widgets.push_back(w);
      }
    }
  }

  g_modal_widgets = new_modal_widgets;
}

void register_modal_qwidget(QWidget *widget){
#if !defined(RELEASE)
  R_ASSERT_RETURN_IF_FALSE(widget->isModal());
#endif

  //R_ASSERT_NON_RELEASE(!a_modal_widget_is_open());

  remove_null_and_nonmodal_widgets();

  for(auto &w : g_modal_widgets){
    if (w.data()==widget){
      R_ASSERT_NON_RELEASE(false);
      return;
    }
  }

  g_modal_widgets.push_back(QPointer<QWidget>(widget));
}

bool a_modal_widget_is_open(void){
  remove_null_and_nonmodal_widgets();

  for(auto &w : g_modal_widgets){
    auto *w2 = w.data();
    if (w2==NULL)
      R_ASSERT(false);
    else
      if (w2->isVisible())
        return true;
  }

  return false;
}


static double g_last_time = -1;
static double g_ignore_until = -1;

bool GFX_Message_ignore_questionmark(void){
  return TIME_get_ms() <= g_ignore_until;
}

bool GFX_Message_ask_ignore_question_questionmark(void){
  return (TIME_get_ms() - g_last_time) < 2000;
}

void GFX_Message_call_after_showing(bool clicked_ignore){
  g_last_time = TIME_get_ms();
  
  if (clicked_ignore)
    g_ignore_until = g_last_time + 2000;
}

static int show_gfx_message(vector_t *buttons, bool program_state_is_valid, QString message){
  R_ASSERT(THREADING_is_main_thread());

  if (buttons==NULL && GFX_Message_ignore_questionmark())
    return 0;

  ScopedQPointer<MyQMessageBox> msgBox(MyQMessageBox::create(true));
  
  msgBox->setText(message);

  QString wait_message = "Ignore messages for two seconds";

  
  if(buttons==NULL){

    msgBox->setStandardButtons(QMessageBox::Ok);

    if (GFX_Message_ask_ignore_question_questionmark())
      msgBox->addButton(wait_message, QMessageBox::AcceptRole);  

  } else {

    QPushButton *last_button = NULL;

    VECTOR_FOR_EACH(const char *,button_text,buttons){
      last_button = msgBox->addButton(button_text, QMessageBox::AcceptRole);
    }END_VECTOR_FOR_EACH;

    R_ASSERT(last_button != NULL);
    msgBox->setDefaultButton(last_button);
  }

  
  //msgBox->move(QCursor::pos());
#if PUT_ON_TOP
  RememberGeometryQDialog::num_open_dialogs++;
#endif

  adjustSizeAndMoveWindowToCentre(msgBox);
  
  {
    safeExec(msgBox, program_state_is_valid);
  }
  
#if PUT_ON_TOP
  RememberGeometryQDialog::num_open_dialogs--;
#endif
  
  if (msgBox==NULL) // Theoretically, msgBox could have been deleted while calling exec(). (It is deleted if the parent is deleted, and the parent could basically be any widget)
    return -1;
  
  QAbstractButton *clicked_button = msgBox->clickedButton();

  if (buttons != NULL) {
    for(int i=0;i<buttons->num_elements;i++)
      if(QString((char*)buttons->elements[i])==clicked_button->text()) // This will probably fail if that buggy KDE library is loaded.
        return i;

    for(int i=0;i<buttons->num_elements;i++)
      if(QString((char*)buttons->elements[i]).contains(clicked_button->text())) // Workaround for buggy KDE library. Not a good workaround though.
        return i;

    fprintf(stderr,"******************** \n\n\n\n  ******************* \n\n\n ******** Somethings not working in GFX_Message *************** \n\n\n");

  } else {

    bool clicked_ignore = clicked_button != NULL && clicked_button->text().contains(wait_message);
    GFX_Message_call_after_showing(clicked_ignore);
  
  }

  return 0;
}

int GFX_Message2_internal(vector_t *buttons, bool program_state_is_valid, const char *fmt,...){
  bool is_main_thread = THREADING_is_main_thread();
  bool has_player_lock = PLAYER_current_thread_has_lock();
  
  if (buttons!=NULL){
    R_ASSERT(is_main_thread);
    R_ASSERT(!has_player_lock);
  }
  
  char message[1000];
  va_list argp;
  
  va_start(argp,fmt);
  /*	vfprintf(stderr,fmt,argp); */
  vsnprintf(message,998,fmt,argp);
  va_end(argp);

  R_ASSERT_NON_RELEASE(g_radium_runs_custom_exec==false);

#if !defined(RELEASE)
  if (g_user_interaction_enabled==false){
    if (is_main_thread){
      const char *message2 = talloc_format("  TESTING. show_gfx_message: %s. Num buttons: %d. Autoreturning false.\n", message, buttons==NULL ? 0 : buttons->num_elements);
      addMessage(message2);
      puts(message2);
      return 0;
    }
  }
#endif

    
  if (is_main_thread && has_player_lock==false)
    EVENTLOG_add_event(talloc_strdup(message));

  if (safe_to_run_exec())
    return show_gfx_message(buttons, program_state_is_valid, QString(message));
  else
    return SYSTEM_show_message_menu(buttons, message);
}

void GFX_addMessage_internal(const char *message){
  addMessage(message);
}


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
    msleep(1);
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
  return talloc_format("%s: %s", qVersion(), QSysInfo::buildAbi().toUtf8().constData());
}


#if 0  
void GFX_showChanceHelpWindow(void){
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
#endif

