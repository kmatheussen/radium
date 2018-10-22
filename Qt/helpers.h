#ifndef RADIUM_QT_HELPERS
#define RADIUM_QT_HELPERS

#include <QMessageBox>
#include <QMenu>
#include <QTimer>
#include <QTime>
#include <QMainWindow>
#include <QSplashScreen>
#include <QApplication>
#include <QScrollArea>
#include <QVBoxLayout>
#include <QGuiApplication>
#include <QPointer>
#include <QDesktopWidget>

#include "../OpenGL/Widget_proc.h"
#include "../common/keyboard_focus_proc.h"
#include "../common/visual_proc.h"
#include "../common/OS_system_proc.h"
#include "getqapplicationconstructorargs.hpp"

#include "../api/api_proc.h"


#define PUT_ON_TOP 0

extern bool g_radium_runs_custom_exec;
extern bool g_and_its_not_safe_to_paint;
static inline bool can_internal_data_be_accessed_questionmark(void){
  if(g_radium_runs_custom_exec && g_and_its_not_safe_to_paint) return false;
  if(g_is_loading) return false;
  return true;
}
static inline bool can_internal_data_be_accessed_questionmark_safer(void){
  if(g_radium_runs_custom_exec) return false;
  if(g_is_loading) return false;
  return true;
}
#define RETURN_IF_DATA_IS_INACCESSIBLE(...) if(can_internal_data_be_accessed_questionmark()==false) return __VA_ARGS__;
#define RETURN_IF_DATA_IS_INACCESSIBLE_SAFE2(...) if(can_internal_data_be_accessed_questionmark_safer()==false) return __VA_ARGS__;


extern void register_modal_qwidget(QWidget *widget);
extern bool a_modal_widget_is_open(void);

extern void set_editor_focus(void);

extern QVector<QWidget*> g_static_toplevel_widgets;

extern QMainWindow *g_main_window;
extern QSplashScreen *g_splashscreen;
extern QPointer<QWidget> g_current_parent_before_qmenu_opened; // Only valid if !g_curr_popup_qmenu.isNull()
extern QPointer<QMenu> g_curr_popup_qmenu;

extern int g_num_running_resize_events;
extern bool g_qt_is_painting;
extern bool g_qtgui_has_started,g_qtgui_has_stopped;
extern bool g_qtgui_has_stopped;

typedef QPointer<QObject> IsAlive;

extern QPoint mapFromEditor(QWidget *widget, QPoint point); // Defined in Qt_sequencer.cpp
extern QPoint mapToEditor(QWidget *widget, QPoint point); // Defined in Qt_sequencer.cpp

static inline bool safe_to_run_exec(void){

  /*
  printf("reason: %d, %d, %d, %d, %d, %d, %d, %d, %d, %d, %d, %d\n",
         !is_main_thread , has_player_lock , g_num_running_resize_events > 0 , g_qt_is_painting , g_is_loading , g_qtgui_has_started==false , g_qtgui_has_stopped==true , g_radium_runs_custom_exec , QApplication::activeModalWidget()!=NULL , !g_curr_popup_qmenu.isNull() , QApplication::activePopupWidget()!=NULL , a_modal_widget_is_open());
  */

  bool not_safe = false
    || !THREADING_is_main_thread()
    || PLAYER_current_thread_has_lock()
    || g_num_running_resize_events > 0
    || g_qt_is_painting
    || g_is_loading
    || g_qtgui_has_started==false
    || g_qtgui_has_stopped==true
    || g_radium_runs_custom_exec
    || QApplication::activeModalWidget()!=NULL
    || !g_curr_popup_qmenu.isNull()
    || QApplication::activePopupWidget()!=NULL
    || a_modal_widget_is_open()
    ;
  
  return !not_safe;  
}
  
// QRegion::contains doesn't work.
static inline bool workingQRegionContains(const QRegion &region, const QRect &rect2){
  for(const QRect &rect : region){
    if (rect.contains(rect2))
      return true;
  }
  return false;
}

static inline bool workingQRegionContains(const QRegion &region, const QRegion &region2){
  for(const QRect &rect2 : region2)
    if(workingQRegionContains(region, rect2)==false)
      return false;

  return true;
}


static QPoint getCentrePosition(QWidget *parent, int width, int height, QRect parentRect = QRect()){

  if (parentRect.isNull() || parentRect.isEmpty() || !parentRect.isValid()) {
    
    if (parent==NULL || parent->isVisible()==false)
      // Move to middle of screen instead.
      parentRect = QApplication::desktop()->availableGeometry();
    else
      parentRect = parent->window()->geometry();
    
  }
  
  int x = parentRect.x()+parentRect.width()/2-width/2;
  int y = parentRect.y()+parentRect.height()/2-height/2;
  //printf("w: %d, h: %d\n",width,height);

  return QPoint(R_MAX(20, x), R_MAX(20, y));
}

static inline void moveWindowToCentre(QWidget *widget, QRect parentRect = QRect()){
  int width = R_MAX(widget->width(), 100);
  int height = R_MAX(widget->height(), 50);
  QPoint point = getCentrePosition(widget->parentWidget(), width, height, parentRect);

  widget->move(point);
}

static inline void adjustSizeAndMoveWindowToCentre(QWidget *widget, QRect parentRect = QRect()){
  widget->adjustSize();
  widget->updateGeometry();
  moveWindowToCentre(widget, parentRect);
}

#if !defined(RELEASE)
  #define D(A) A
#else
  #define D(A)
#endif


static bool can_widget_be_parent_questionmark(QWidget *w, bool is_going_to_run_custom_exec){
  if (w==NULL)
    return false;
  /*
  if (w==g_curr_popup_qmenu)
    return false;
  if (w->windowFlags() & Qt::Popup)
    return false;
  if (w->windowFlags() & Qt::ToolTip)
    return false;
  */

  if (w->isVisible()==false)
    return false;
  
  if (w==g_curr_popup_qmenu){
    D(printf("iscurrpopup\n"));
    return false;
  }

  if (w->windowFlags() & Qt::ToolTip){
    D(printf("istooltip\n"));
    return false;
  }
  
  if (dynamic_cast<QMenu*>(w) != NULL){
    D(printf("ismenu\n"));
    return false;
  }

  // Should be safe. When running custom exec, a widget can not be deleted until exec() is finished. (don't think that's correct. exec() can do anything)
  //if(is_going_to_run_custom_exec==true)
  //  return true;

  if (w->windowFlags() & Qt::Popup){
    D(printf("ispopup\n"));
    return false;
  }
  
  return true;
}

/*
// Didn't work very well.
static inline QWidget *find_modal_childwindow_to_be_parent(QWidget *widget, bool is_going_to_run_custom_exec){
  const QList<QObject*> list = widget->children();
  for (auto *o : list){
    QWidget *widget = dynamic_cast<QWidget*>(o);
    if(widget!=NULL && widget->isWindow() && widget->isModal() && widget->isVisible()) // && can_widget_be_parent_questionmark(widget, is_going_to_run_custom_exec))
      return widget;
  }

  return NULL;
}
*/

// Can only return a widget that is a member of g_static_toplevel_widgets.
static inline QWidget *get_current_parent(QWidget *child, bool is_going_to_run_custom_exec, bool may_return_current_parent_before_qmenu_opened = true){
  
  if (child != NULL){
    if (g_static_toplevel_widgets.contains(child)){
#if !defined(RELEASE)
      abort();
#endif
      if (child==g_main_window){
        int *ai=NULL;
        ai[0] = 51; // Start the crash reporter. We can't use R_ASSERT in this function since get_current_parent is used by some of the bin/radium_* programs.
      }
      return g_main_window;
    }
  }

  if (may_return_current_parent_before_qmenu_opened && !g_curr_popup_qmenu.isNull() && !g_current_parent_before_qmenu_opened.isNull()){
    D(printf("1111 %p\n", g_current_parent_before_qmenu_opened.data()));
    return g_current_parent_before_qmenu_opened;
  }

  QWidget *ret = QApplication::activeModalWidget(); // TODO: Fix. This one returns NULL if the widget is not active. But the active window could be the parent (seems like), and we don't want to return the parent. At least we don't want to return g_main_window when it has a modal window as child. (tried that, but qt behaved strange)
  D(printf("2222 %p\n", ret));
  if (can_widget_be_parent_questionmark(ret, is_going_to_run_custom_exec)){
    return ret;
  }

  ret = QApplication::focusWidget();
  D(printf("333 %p\n", ret));
  if (can_widget_be_parent_questionmark(ret, is_going_to_run_custom_exec)){
    return ret->window();
  }

  /*
  ret = QApplication::activePopupWidget();
  D(printf("333555 %p\n", ret));
  if (can_widget_be_parent_questionmark(ret, is_going_to_run_custom_exec)){
    return ret->window();
  }
  */
  
  ret = QApplication::activeWindow();
  D(printf("444 %p\n", ret));
  if (can_widget_be_parent_questionmark(ret, is_going_to_run_custom_exec)){
    return ret;
  }

  /*
  QWidget *mixer_strips_widget = MIXERSTRIPS_get_curr_widget();
  printf("555 %p\n", ret);
  if (mixer_strips_widget!=NULL){
    return mixer_strips_widget;
  }
  */

  /*
    // Don't think we usually want this. Not sure.
  ret = QApplication::widgetAt(QCursor::pos());
  D(printf("666 %p\n", ret));
  if (g_static_toplevel_widgets.contains(ret)){
    return ret->window();;
  }
  */

  /*
  ret = find_modal_childwindow_to_be_parent(g_main_window, is_going_to_run_custom_exec);
  D(printf("777 %p\n", ret));
  if (ret != NULL)
    return ret;
  */
  
  D(printf("888\n"));
  
  return g_main_window;
    
    /*
    if (curr_under != NULL)
      return curr_under;
    else if (QApplication::activeWindow()!=NULL)
      return QApplication::activeWindow();
    else
      return g_main_window;
    */
}

#undef D

//   We normally don't want to set Qt::WindowStaysOnTopHint since VST plugin GUIs will appear behind.
#define DEFAULT_WINDOW_FLAGS (Qt::CustomizeWindowHint | Qt::WindowFullscreenButtonHint | Qt::WindowCloseButtonHint | Qt::WindowMinMaxButtonsHint) // | Qt::WindowStaysOnTopHint)

namespace radium{
  enum Modality{
    IS_MODAL,
    MAY_BE_MODAL, // i.e. becomes modal if it is a decendant of a child of a toplevel window. On OSX (and probably some linux window managers), modality fixes always-on-top for crandchildren of toplevel windows.
    NOT_MODAL // But if modality has this value, we wont use that trick to fix always-on-top.
  };
}

// Returns true if modality is turned on when 'is_modal'==false.
static inline bool set_window_parent_andor_flags(QWidget *window, QWidget *parent, radium::Modality modality, bool only_set_flags, bool is_converting_widget_to_window = false){ //bool is_modal, bool only_set_flags){

//  #if defined(FOR_MACOSX)
#if 1
  // Although these hacks are only needed on OSX, it's probably best to let the program behave similarly on all platforms. (this behavior is not that bad)
  // Besides, it may be that setParent isn't always working properly on windows and linux either, for all I know.
  const bool set_parent_working_properly = false;
#else
  const bool set_parent_working_properly = true;
#endif

  // sanity checks

  if (is_converting_widget_to_window){

#if !defined(RELEASE)
    if(window->isWindow())
      abort();
#endif
    
  } else {

#if !defined(RELEASE)
    if(!window->isWindow())
      abort();
#endif
    
    QWidget *child_window = window->window();
    QWidget *parent_window = parent==NULL ? NULL : parent->window();

#if !defined(RELEASE)
    if(parent!=NULL && parent_window==NULL)
      abort();
#endif    
    if (child_window!=NULL && parent_window!=NULL){
      if (child_window==parent_window){
#if !defined(RELEASE)
        abort();
#endif
        return false;
      }
    }
  }
  
        
  Qt::WindowFlags f = Qt::Window | DEFAULT_WINDOW_FLAGS;
  bool force_modal = false;

  if (!set_parent_working_properly){
    if (parent==g_main_window) {

      // On OSX, you can't create an "on-top-of hierarchy" by setting the windows parent. But for level 1, we can work around this by setting the Qt::Tool flag.
#if defined(FOR_WINDOWS)
      f = Qt::Window | DEFAULT_WINDOW_FLAGS; // Remove Qt::Tool since it's not needed. Qt::Tool is just a workaround on OSX, and possibly Linux.
#else
      f = Qt::Window | Qt::Tool | DEFAULT_WINDOW_FLAGS;
#endif
      
      // Qt::Tool windows dissapear on OSX if the application is not active. (At least according to Qt documentation. I haven't tested it.)
      //window->setAttribute(Qt::WA_MacAlwaysShowToolWindow, true); // Warning: Causes program to crash.
      
    } else if (modality != radium::NOT_MODAL)
      force_modal=true; // Qt::Tool doesn't work for levels larger than 1 (it doesn't work if the parent is a Qt::Tool window), so we work around it by using modal windows. Modal windows seems to always be on top of parent window.
  }


#if !defined(RELEASE)
  if (parent==window)
    abort();
#endif
  

  if (parent==window || only_set_flags)
    window->setWindowFlags(f);
  else
    window->setParent(parent, f);
 
  if (true &&
      //(parent==NULL || !parent->isModal()) && // Uncomment this line to prevent a modal window to be parent of another modal window. Should be fine though. It's modal siblings that can lock up the program (most likely a qt bug)
      (force_modal || modality==radium::IS_MODAL)
      )
  {

    if (window->windowModality()!=Qt::ApplicationModal){
      window->setWindowModality(Qt::ApplicationModal);
      register_modal_qwidget(window);
    }

    return !(modality==radium::IS_MODAL);
    
  } else {
    
    if (modality!=radium::IS_MODAL && window->windowModality()!=Qt::NonModal){ // We may have forcefully turned on modality in a previous call. Turn it off now.
      window->setWindowModality(Qt::NonModal);
    }
    
    return false;
    
  }
}

// Returns true if modality is turned on when 'is_modal'==false.
static inline bool set_window_parent(QWidget *window, QWidget *parent, radium::Modality modality){
  return set_window_parent_andor_flags(window, parent, modality, false);
}

// Returns true if modality is turned on when 'is_modal'==false.
static inline bool convert_widget_to_window(QWidget *widget, QWidget *parent, radium::Modality modality){
  return set_window_parent_andor_flags(widget, parent, modality, false, true);
}

// Returns true if modality is turned on when 'is_modal'==false.
static inline bool set_window_flags(QWidget *window, radium::Modality modality){
  return set_window_parent_andor_flags(window, window->parentWidget(), modality, true);
}

static inline void set_widget_takes_care_of_painting_everything(QWidget *widget){
  widget->setAttribute(Qt::WA_NoSystemBackground);
  
#if !defined(FOR_MACOSX)
  widget->setAttribute(Qt::WA_OpaquePaintEvent); // This seems broken on OSX. (is this the fix for unwanted transparancy?)
#endif
  
}


namespace radium{
  struct ScopedResizeEventTracker{
    ScopedResizeEventTracker(){
      g_num_running_resize_events++;
    }
    ~ScopedResizeEventTracker(){
      --g_num_running_resize_events;
    }
  };
}


namespace radium{
  struct ASMTimer : public QTimer{
    QTime time;
    bool left_mouse_is_down = false;
    
    ASMTimer(QWidget *parent_)
      :QTimer(parent_)
    {
      time.start();
      setInterval(10);
    }

    void timerEvent ( QTimerEvent * e ) override {
      if (QGuiApplication::mouseButtons()==Qt::LeftButton)
        left_mouse_is_down = true;
      else if (left_mouse_is_down){
        left_mouse_is_down = false;
        time.restart();
      }
        
      //printf("  MOUSE DOWN: %d\n", QGuiApplication::mouseButtons()==Qt::LeftButton);
    }

    bool mouseWasDown(void){
      if(left_mouse_is_down==true)
        return true;
      if (time.elapsed() < 500)
        return true;
      return false;
    }
  };

  class VerticalScroll : public QScrollArea {
    //    Q_OBJECT;
    
  public:

    QVBoxLayout *layout;    
    
    VerticalScroll(QWidget *parent_)
      :QScrollArea(parent_)
    {
      setVerticalScrollBarPolicy(Qt::ScrollBarAlwaysOn);
      setWidgetResizable(true);
      
      QWidget *contents = new QWidget(this);
      
      layout = new QVBoxLayout(contents);
      layout->setSpacing(1);

      contents->setLayout(layout);
      
      setWidget(contents);    
    }
    
    void addWidget(QWidget *widget_){
      layout->addWidget(widget_);
    }
    
    void removeWidget(QWidget *widget_){
      layout->removeWidget(widget_);
    }
  };


}

extern const char *g_qt_is_painting_where;

namespace radium{
  struct PaintEventTracker{
    PaintEventTracker(const char *where){
      R_ASSERT_NON_RELEASE(THREADING_is_main_thread());
      R_ASSERT_NON_RELEASE(g_qt_is_painting==false);
      g_qt_is_painting_where = where;
      g_qt_is_painting=true;
    }
    ~PaintEventTracker(){
      R_ASSERT_NON_RELEASE(g_qt_is_painting==true);
      g_qt_is_painting=false;
    }
  };
}

#define TRACK_PAINT() radium::PaintEventTracker radium_internal_pet(CR_FORMATEVENT(""))


// Why doesn't Qt provide this one?
namespace{
template <typename T>
struct ScopedQPointer : public QPointer<T>{
  T *_widget;
  ScopedQPointer(T *widget)
    : QPointer<T>(widget)
    , _widget(widget)
  {}
  ~ScopedQPointer(){
    printf("Deleting scoped pointer widget %p\n",_widget);
    delete _widget;
  }
};
/*
template <typename T>
struct ScopedQPointer {

  QPointer<T> _box;
  
  MyQPointer(T *t){
    _box = t;
    //printf("MyQPointer created %p for %p %p %p\n", this, _box, _box.data(), t);
  }

  ~MyQPointer(){    
    //printf("MyQPointer %p deleted (_box: %p %p)\n", this, _box, _box.data());
    delete data();
  }

  inline T* data() const
  {
    return _box.data();
  }
  inline T* operator->() const
  { return data(); }
  inline T& operator*() const
  { return *data(); }
  inline operator T*() const
  { return data(); }
};
*/
}


struct MyQMessageBox : public QMessageBox {
  bool _splashscreen_visible;

  // Prevent stack allocation. Not sure, but I think get_current_parent() could return something that could be deleted while calling exec.
  MyQMessageBox(const MyQMessageBox &) = delete;
  MyQMessageBox & operator=(const MyQMessageBox &) = delete;
  MyQMessageBox(MyQMessageBox &&) = delete;
  MyQMessageBox & operator=(MyQMessageBox &&) = delete;

  //
  // !!! parent must/should be g_main_window unless we use ScopedQPointer !!!
  //
  static MyQMessageBox *create(bool is_going_to_run_custom_exec, QWidget *parent = NULL){
    return new MyQMessageBox(is_going_to_run_custom_exec, parent);
  }
  
 private:  
    
  MyQMessageBox(bool is_going_to_run_custom_exec, QWidget *parent_ = NULL)
    : QMessageBox(parent_!=NULL ? parent_ : get_current_parent(NULL, is_going_to_run_custom_exec))
  {
    register_modal_qwidget(this);

    setWindowTitle("Radium message");
    //printf("            PAERENT: %p. visible: %d\n",parent(),dynamic_cast<QWidget*>(parent())==NULL ? 0 : dynamic_cast<QWidget*>(parent())->isVisible());
    if(dynamic_cast<QWidget*>(parent())==NULL || dynamic_cast<QWidget*>(parent())->isVisible()==false){
      //setWindowModality(Qt::ApplicationModal);
      //setWindowFlags(Qt::Popup | Qt::WindowStaysOnTopHint);
      //setWindowFlags(windowFlags() | Qt::FramelessWindowHint | Qt::MSWindowsFixedSizeDialogHint);
      setWindowFlags(Qt::Window | Qt::WindowStaysOnTopHint | Qt::MSWindowsFixedSizeDialogHint);
      raise();
      activateWindow();
    } else {
      set_window_flags(this, radium::IS_MODAL);
    }
    adjustSizeAndMoveWindowToCentre(this);
  }

 public:
  
  ~MyQMessageBox(){
    printf("MyQMessageBox %p deleted\n", this);
  }
  
  void showEvent(QShowEvent *event_) override {
    _splashscreen_visible = g_splashscreen!=NULL && g_splashscreen->isVisible();

    if (_splashscreen_visible)
      g_splashscreen->hide();

    QMessageBox::showEvent(event_);
  }
  
  void hideEvent(QHideEvent *event_) override {
    if (_splashscreen_visible && g_splashscreen!=NULL)
      g_splashscreen->show();

    QMessageBox::hideEvent(event_);
  }
  
};


static inline void setUpdatesEnabledRecursively(QWidget *widget, bool doit){
  if (widget != NULL){
    widget->setUpdatesEnabled(doit);
    
    for(auto *c : widget->children()){
      QWidget *w = dynamic_cast<QWidget*>(c);      
      if (w && w->isWindow()==false)
        setUpdatesEnabledRecursively(w, doit);
    }
  }
}

namespace{
  struct PauseUpdatesTimer : public QTimer{
    QWidget *_w;
    QTime _time;
    int _ms;
    bool _has_done_step1 = false;
    
    PauseUpdatesTimer(QWidget *parent, int ms)
      : _w(parent)
      , _ms(ms)
    {
      //setSingleShot(true);
      _time.start();
      setInterval(15);
      start();
    }

    void timerEvent(QTimerEvent *e) override{
      if (_time.elapsed() >= _ms){ // singleshot is messy since we might get deleted at any time.
        //printf("TEIMERERINE EVENT\n");
        if (_has_done_step1==false && _w->isVisible()==false){
          setUpdatesEnabledRecursively(_w, true);
          _ms *= 2;
          _has_done_step1=true;
        } else {
          setUpdatesEnabledRecursively(_w, true);
          _w->show();
          stop();
          delete this;
        }
      }
    }
  };
}

static inline void pauseUpdates(QWidget *w, int ms = 50){
  setUpdatesEnabledRecursively(w, false);
  new PauseUpdatesTimer(w, ms);
}

static inline void updateWidgetRecursively(QObject *object){
  if (object != NULL){

    QWidget *w = dynamic_cast<QWidget*>(object);
    if (w!=NULL){
      //printf("Updating %p. (%s)\n", w, w->metaObject()->className());
      w->update();
    }
    
    for(auto *c : object->children())
      updateWidgetRecursively(c);
  }
}

// uncomment if needed.
//extern QByteArray g_filedialog_geometry;

namespace{

  /*
    Qt makes it _almost_ impossible to remember geometry of windows (both qdialog and qwidget) without adding a timer that monitors what's happening and tries to do the right thing.
    (and we most certainly don't want to that)

    The problem is that Qt always opens the windows at the original position when calling setVisible(true) or show(). There's no way to override that. It would be the most
    natural thing in the world to override, but there is no way. The only way to open at the original position is to remember geometry when hiding, and restore when showing.
    Unfortunatly things becomes very complicated since it's unclear (probably also for those who develops Qt) when the widgets are actually shown and hidden.

    However, I have found that the following seems to work (at least for Qt 5.5.1 on Linux with FVWM2):

    To use it, override setVisible and hideEvent like this:

    void setVisible(bool visible) override {
      remember_geometry.setVisible_override<superclass>(this, visible);
    }

    void hideEvent(QHideEvent *event_) override {
      remember_geometry.hideEvent_override(this);
    }

   */

  struct RememberGeometry{
    QByteArray geometry;
    bool has_stored_geometry = false;
    
    bool move_window_to_centre_first_time_its_opened = true; // may be set to false by owner.
    
    void save(QWidget *widget){
      geometry = widget->saveGeometry();
      has_stored_geometry = true;
      //printf("   SAVING geometry\n");
    }

    void restore(QWidget *widget){
      if (has_stored_geometry){

        //printf("   RESTORING geometry\n");
        widget->restoreGeometry(geometry);
        
      }else{

        //printf("   88888888888888888888888888888888888888888888 NO geometry stored\n");

        if (move_window_to_centre_first_time_its_opened)
          moveWindowToCentre(widget);
      }
    }

    ///////////////////
    // I've tried lots of things, and the only thing that seems to work is overriding setVisible and hideEvent exactly like below.
    // Other combinations will fail in more or less subtle ways.
    ///////////////////

    template <class SuperWidget>
    void setVisible_override(SuperWidget *widget, bool visible) {
      if (visible==false && widget->isVisible()==false){
        widget->SuperWidget::setVisible(visible);
        return;
      }
      if (visible==true && widget->isVisible()==true){
        widget->SuperWidget::setVisible(visible);
        return;
      }

      if (visible && widget->window()==widget)
        restore(widget);  // Not necessary for correct operation, but it might seem like it removes some flickering.
     
      widget->SuperWidget::setVisible(visible);
      
      if (visible && widget->window()==widget)
        restore(widget);
    }

    void hideEvent_override(QWidget *widget) {
      if (widget->window()==widget)
        save(widget);
    }

  };


struct RememberGeometryQDialog : public QDialog {

  RememberGeometry remember_geometry;

#if PUT_ON_TOP
  
  static int num_open_dialogs;

  struct Timer : public QTimer {
    bool was_visible = false;
    
    QWidget *parent;
    Timer(QWidget *parent)
      :parent(parent)
    {
      setInterval(200);
      start();      
    }

    ~Timer(){
      if (was_visible)
        RememberGeometryQDialog::num_open_dialogs--;
    }
      

    void timerEvent ( QTimerEvent * e ){
      //printf("Raising parent\n");
      bool is_visible = parent->isVisible();
      
      if(is_visible && !was_visible) {
        was_visible = true;
        RememberGeometryQDialog::num_open_dialogs++;
      }
          
      if(!is_visible && was_visible) {
        was_visible = false;
        RememberGeometryQDialog::num_open_dialogs--;
      }
          
      if (is_visible && RememberGeometryQDialog::num_open_dialogs==1)
        parent->raise(); // This is probably a bad idea.
    }
  };
  

  Timer timer;
#endif
  
public:
   RememberGeometryQDialog(QWidget *parent_, radium::Modality modality)
    : QDialog(parent_!=NULL ? parent_ : g_main_window, Qt::Window) // | Qt::Tool)
#if PUT_ON_TOP
    , timer(this)
#endif
  {
    set_window_flags(this, modality);
    //QDialog::setWindowModality(Qt::ApplicationModal);
    //setWindowFlags(windowFlags() | Qt::WindowStaysOnTopHint);
  }
  
  virtual void setVisible(bool visible) override {
    remember_geometry.setVisible_override<QDialog>(this, visible);
  }
  
  // See comment in helpers.h for the radium::RememberGeometry class.
  virtual void hideEvent(QHideEvent *event_) override {
    remember_geometry.hideEvent_override(this);
  }

};

}

struct GL_PauseCaller{
  GL_PauseCaller(){
    bool locked = GL_maybeLock();
    GL_pause_gl_thread_a_short_while();
    if (locked)
      GL_unlock();      
  }
};


namespace radium{
struct ScopedExec{
  bool _do_tell_program_state_is_valid;
  bool _has_obtained_custom_exec;
  bool _has_obtained_safe_to_paint;
  
  ScopedExec(bool program_state_is_valid, bool assert_running_custom_exec = true)
    : _do_tell_program_state_is_valid(program_state_is_valid)
  {
    obtain_keyboard_focus();

    if (assert_running_custom_exec){
      R_ASSERT_NON_RELEASE(g_radium_runs_custom_exec==false);
    }
    
    if (_do_tell_program_state_is_valid){
      R_ASSERT_NON_RELEASE(g_and_its_not_safe_to_paint==true);
      if (g_and_its_not_safe_to_paint==true){
        g_and_its_not_safe_to_paint = false;
        _has_obtained_safe_to_paint = true;
      } else
        _has_obtained_safe_to_paint = false;
    }else{
      _has_obtained_safe_to_paint = false;
    }

    if (g_radium_runs_custom_exec) {
      _has_obtained_custom_exec = false;
    } else {
      g_radium_runs_custom_exec = true;
      _has_obtained_custom_exec = true;
    }
    
    GFX_HideProgress();
  }
  
  ~ScopedExec(){
    GFX_ShowProgress();

    R_ASSERT_NON_RELEASE(g_radium_runs_custom_exec==true);

    if (_has_obtained_custom_exec)
      g_radium_runs_custom_exec = false;
    
    if (_do_tell_program_state_is_valid){
      R_ASSERT_NON_RELEASE(g_and_its_not_safe_to_paint==false);
      if (_has_obtained_safe_to_paint)
        g_and_its_not_safe_to_paint = true;
    }

    for(auto *window : QApplication::topLevelWidgets())
      updateWidgetRecursively(window);

    release_keyboard_focus();
  }
};
}


// Happens sometimes that there are more than two menues visible at the same time. (probably got something to do with non-async menues)
static inline void closePopup(void){
  R_ASSERT_RETURN_IF_FALSE(THREADING_is_main_thread());

  if (!g_curr_popup_qmenu.isNull()){
    g_curr_popup_qmenu->hide(); // safer.
  //g_curr_popup_qmenu->deleteLater(); // We might be called from the "triggered" callback of the menu.
  }
}

static inline void set_those_menu_variables_when_starting_a_popup_menu(QMenu *menu_to_be_started){
  if (!g_curr_popup_qmenu.isNull()) // Don't set it if there's a menu open already.
    g_current_parent_before_qmenu_opened = get_current_parent(menu_to_be_started, false, false);

  g_curr_popup_qmenu = menu_to_be_started;
}

static inline int safeExec(QMessageBox *widget, bool program_state_is_valid){
  R_ASSERT_RETURN_IF_FALSE2(g_qt_is_painting==false, -1);
    
  closePopup();

#if defined(RELEASE)
  // QMessageBox is usually used to show error and warning messages.
  R_ASSERT(g_radium_runs_custom_exec==false);
#endif

  if (g_radium_runs_custom_exec==true){
    GFX_HideProgress();
    GL_lock();
    int ret = widget->exec();
    GFX_ShowProgress();
    GL_unlock();
    return ret;
  }

  radium::ScopedExec scopedExec(program_state_is_valid);

  //QPointer<QWidget> prev_widget = QApplication::focusWidget();
  
  int ret = widget->exec();

  /*
  if (prev_widget != NULL)
    prev_widget->setFocus();
  */
  
  return ret;
  
}

/*
static inline int safeExec(QMessageBox &widget, bool program_state_is_valid){
  closePopup();

  R_ASSERT_RETURN_IF_FALSE2(g_radium_runs_custom_exec==false, 0);

  radium::ScopedExec scopedExec(program_state_is_valid);

  return widget.exec();
}
*/

static inline int safeExec(QDialog *widget, bool program_state_is_valid){
  R_ASSERT_RETURN_IF_FALSE2(g_qt_is_painting==false, -1);
  
  closePopup();

  R_ASSERT_RETURN_IF_FALSE2(g_radium_runs_custom_exec==false, 0);

  radium::ScopedExec scopedExec(program_state_is_valid);

  return widget->exec();
}

static inline QAction *safeMenuExec(QMenu *widget, bool program_state_is_valid){
  R_ASSERT(g_qt_is_painting==false);
  
  // safeExec might be called recursively if you double click right mouse button to open a pop up menu. Seems like a bug or design flaw in Qt.
#if !defined(RELEASE)
  if (g_radium_runs_custom_exec==true)
    GFX_Message(NULL, "Already runs custom exec");
#endif

  closePopup();
  set_those_menu_variables_when_starting_a_popup_menu(widget);
      
  if (doModalWindows()) {
    radium::ScopedExec scopedExec(program_state_is_valid);
    return widget->exec(QCursor::pos());
  }else{
    radium::ScopedExec scopedExec(program_state_is_valid);
    GL_lock();{
      GL_pause_gl_thread_a_short_while();
    }GL_unlock();    
    return widget->exec(QCursor::pos());
  }
}

static inline void safeMenuPopup(QMenu *menu){
  R_ASSERT_RETURN_IF_FALSE(g_qt_is_painting==false);
  
  closePopup();
  set_those_menu_variables_when_starting_a_popup_menu(menu);

  menu->popup(QCursor::pos());
}

static inline void safeShowPopup(QWidget *popup){
  R_ASSERT_RETURN_IF_FALSE(g_qt_is_painting==false);
  
  closePopup();
  popup->show();
}

static inline void safeShow(QWidget *widget){
  R_ASSERT_RETURN_IF_FALSE(g_qt_is_painting==false);
  closePopup();

  GL_lock(); {
    GL_pause_gl_thread_a_short_while();
    widget->show();
    widget->raise();
    widget->activateWindow();
#if 0 //def FOR_WINDOWS
    HWND wnd=(HWND)w->winId();
    SetFocus(wnd);
    SetWindowPos(wnd, HWND_TOPMOST, 0, 0, 0, 0, SWP_NOMOVE|SWP_NOSIZE);
#endif
  }GL_unlock();
}

static inline void safeShowOrExec(QDialog *widget, bool program_state_is_valid){
  if (doModalWindows())
    safeExec(widget,program_state_is_valid);
  else
    safeShow(widget);
}

#endif // RADIUM_QT_HELPERS
