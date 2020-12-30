#ifndef RADIUM_QT_HELPERS
#define RADIUM_QT_HELPERS

#include <QMessageBox>
#include <QMenu>
#include <QTimer>
#include <QTime>
#include <QApplication>
#include <QScrollArea>
#include <QVBoxLayout>
#include <QGuiApplication>
#include <QScreen>
#include <QPointer>
#include <QDesktopWidget>
#include <QMouseEvent>
#include <QKeyEvent>
#include <QPainter>
#include <QGraphicsSceneMouseEvent>
#include <QElapsedTimer>

#include "../OpenGL/Widget_proc.h"
#include "../common/keyboard_focus_proc.h"
#include "../common/visual_proc.h"
#include "../common/OS_system_proc.h"
#include "getqapplicationconstructorargs.hpp"

#include "../api/api_proc.h"

extern "C"{
  void CRASHREPORTER_send_assert_message(Crash_Type tye, const char *message, ...);
}
  
#define PUT_ON_TOP 0

extern bool g_radium_runs_custom_exec;
extern bool g_and_its_not_safe_to_paint;
extern bool g_qtgui_has_stopped;

static inline bool can_internal_data_be_accessed_questionmark(void){
  if(g_radium_runs_custom_exec && g_and_its_not_safe_to_paint) return false;
  if(g_is_loading) return false;
  if (g_qtgui_has_stopped) return false;
  return true;
}
static inline bool can_internal_data_be_accessed_questionmark_safer(void){
  if(g_radium_runs_custom_exec) return false;
  if(g_is_loading) return false;
  if (g_qtgui_has_stopped) return false;
  return true;
}
#define RETURN_IF_DATA_IS_INACCESSIBLE(...) if(can_internal_data_be_accessed_questionmark()==false) return __VA_ARGS__;
#define RETURN_IF_DATA_IS_INACCESSIBLE_SAFE2(...) if(can_internal_data_be_accessed_questionmark_safer()==false) return __VA_ARGS__;


extern void register_modal_qwidget(QWidget *widget);
extern bool a_modal_widget_is_open(void);

extern void set_editor_focus(void);

extern QVector<QWidget*> g_static_toplevel_widgets;

extern QWidget *g_main_window;
//extern QSplashScreen *g_splashscreen;
extern QPointer<QWidget> g_current_parent_before_qmenu_opened; // Only valid if !g_curr_popup_qmenu.isNull()
extern QPointer<QMenu> g_curr_popup_qmenu;

extern int g_num_running_resize_events;
extern bool g_qt_is_painting;
extern bool g_qtgui_has_started;
extern DEFINE_ATOMIC(bool, g_qtgui_has_started_step2);

typedef QPointer<QObject> IsAlive;

extern QPoint mapFromEditor(QWidget *widget, QPoint point); // Defined in Qt_sequencer.cpp
extern QPoint mapToEditor(QWidget *widget, QPoint point); // Defined in Qt_sequencer.cpp

static inline Qt::KeyboardModifier get_modifier_may_causing_rightclick(void){
  if (swapCtrlAndCmd())
    return Qt::MetaModifier;
  else
    return Qt::ControlModifier;
}

static inline void send_key_up(QObject *where, int how_many){
  if (where==NULL)
    return;
  for(int i=0;i<how_many;i++){
    QKeyEvent *eve1 = new QKeyEvent((enum QEvent::Type)6, Qt::Key_Up, Qt::NoModifier);
    qApp->postEvent(where,eve1);
  }
}

static inline void send_key_down(QObject *where, int how_many){
  if (where==NULL)
    return;
  for(int i=0;i<how_many;i++){
    QKeyEvent *eve1 = new QKeyEvent((enum QEvent::Type)6, Qt::Key_Down, Qt::NoModifier);
    qApp->postEvent(where,eve1);
  }
}

extern bool g_force_regular_gfx_message;

static inline bool safe_to_run_exec(void){

#if 0
  printf("reason: %d, %d, %d, %d, %d, %d, %d, %d, %d, %d, %d, %d\n",
         !THREADING_is_main_thread(), PLAYER_current_thread_has_lock() , g_num_running_resize_events > 0 , g_qt_is_painting , g_is_loading , g_qtgui_has_started==false , g_qtgui_has_stopped==true , g_radium_runs_custom_exec , QApplication::activeModalWidget()!=NULL , !g_curr_popup_qmenu.isNull() , QApplication::activePopupWidget()!=NULL , a_modal_widget_is_open());
#endif

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
    //|| g_scheme_nested_level > 0 // Probably a good idea...
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


// Haven't tested, but I don't trust QRegion::intersects to work either
static inline bool workingQRegionIntersects(const QRegion &region, const QRect &rect2){
  for(const QRect &rect : region){
    if (rect.intersects(rect2))
      return true;
  }
  return false;
}

static inline bool workingQRegionIntersects(const QRegion &region, const QRectF &rect2){
  return workingQRegionIntersects(region, rect2.toAlignedRect());
}


static inline QPoint getCentrePosition(QWidget *parent, int width, int height, QRect parentRect = QRect()){

  if (parentRect.isNull() || parentRect.isEmpty() || !parentRect.isValid()) {
    
    if (parent==NULL || parent->isVisible()==false) {
      
      if (g_main_window != NULL) {
        
        // Move to middle of main window.
        parentRect = g_main_window->window()->frameGeometry();
        
      } else {
        
        // Move to middle of screen instead.
        QScreen *screen = QApplication::screens().first();
        
        if(screen != NULL) {
          
          parentRect = screen->availableGeometry();
          
        } else {
          
          fprintf(stderr, "Warning: No screens found\n");
#if !defined(RELEASE)
          abort();
#endif
          return QPoint(100,100);
          
        }
        
      }
      
    } else {
      
      parentRect = parent->window()->frameGeometry();

    }
  }
  
  int x = parentRect.x() + (parentRect.width() - width)/2;
  int y = parentRect.y() + (parentRect.height() - height)/2;
  
  //printf("w: %d, h: %d\n",width,height);

  return QPoint(R_MAX(20, x), R_MAX(20, y));
}

static inline void moveWindowToCentre(QWidget *widget, QRect parentRect = QRect()){
#ifndef CRASHREPORTER_BIN
#ifndef COMPILE_EXECUTABLE
  R_ASSERT_NON_RELEASE(widget->width()>0);
  R_ASSERT_NON_RELEASE(widget->height()>0);
#endif
#endif

  if (widget->window() != widget){
    
    R_ASSERT_NON_RELEASE(false);
    
  } else {

    int width = R_MAX(widget->width(), 100);
    int height = R_MAX(widget->height(), 50);
    QPoint point = getCentrePosition(widget->parentWidget(), width, height, parentRect);
    
    widget->move(point);
  }
}

static inline void adjustSizeAndMoveWindowToCentre(QWidget *widget, QRect parentRect = QRect()){
  if (widget->window() != widget){
    
    R_ASSERT_NON_RELEASE(false);
    
  } else {

    widget->adjustSize();
    widget->updateGeometry();
    moveWindowToCentre(widget, parentRect);

  }
}

#ifndef D_IS_DEFINED
  #if !defined(RELEASE)
    #define D(A) A
  #else
    #define D(A)
  #endif
  #define D_IS_DEFINED 1
#endif


static inline QObject *get_oldest_parent(QObject *w){
  if (w==NULL)
    return NULL;

  int safety = 0;

  while(w->parent() != NULL){
    w = w->parent();
    safety++;
    if (w!=NULL && safety >= 1000){
#ifndef COMPILE_EXECUTABLE
#ifndef CRASHREPORTER_BIN
      RError("Error! Recursive widget parent for %p. Setting parent to NULL.", w);
#endif
#endif
      w->setParent(NULL);
      return NULL;
    }
  }


  //printf("...........Safety: %d\n", safety);
  return w;
}


static inline bool can_widget_be_parent_questionmark(QWidget *child, QWidget *parent, bool is_going_to_run_custom_exec){
  if (parent==NULL)
    return false;

  {
    auto *a = get_oldest_parent(child);
    auto *b = get_oldest_parent(parent);

    if (a==b)
      return false;
  }

  /*
  if (parent==g_curr_popup_qmenu)
    return false;
  if (parent->windowFlags() & Qt::Popup)
    return false;
  if (parent->windowFlags() & Qt::ToolTip)
    return false;
  */

  if (parent->isVisible()==false)
    return false;
  
  if (parent==g_curr_popup_qmenu){
    D(printf("iscurrpopup\n"));
    return false;
  }

  if (parent->windowFlags() & Qt::ToolTip){
    D(printf("istooltip\n"));
    return false;
  }
  
  if (dynamic_cast<QMenu*>(parent) != NULL){
    D(printf("ismenu\n"));
    return false;
  }

  // Should be safe. When running custom exec, a widget can not be deleted until exec() is finished. (don't think that's correct. exec() can do anything)
  //if(is_going_to_run_custom_exec==true)
  //  return true;

  if (parent->windowFlags() & Qt::Popup){
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
  if (can_widget_be_parent_questionmark(child, ret, is_going_to_run_custom_exec)){
    return ret;
  }

  ret = QApplication::focusWidget();
  D(printf("333 %p\n", ret));
  if (can_widget_be_parent_questionmark(child, ret, is_going_to_run_custom_exec)){
    return ret->window();
  }

  /*
  ret = QApplication::activePopupWidget();
  D(printf("333555 %p\n", ret));
  if (can_widget_be_parent_questionmark(child, ret, is_going_to_run_custom_exec)){
    return ret->window();
  }
  */
  
  ret = QApplication::activeWindow();
  D(printf("444 %p\n", ret));
  if (can_widget_be_parent_questionmark(child, ret, is_going_to_run_custom_exec)){
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


static inline bool a_is_a_parent_of_b(QObject *a, QObject *b){
  
  for(int safety=0 ; safety < 1000 ; safety++){
    if (a==b)
      return true;
    
    if (b==NULL)
      return false;

    b = b->parent();
  }

  if (b != NULL){
#ifndef CRASHREPORTER_BIN
#ifndef COMPILE_EXECUTABLE    
    RError("Error! Recursive widget parent for %p. Setting parent to NULL.", b);
#endif
#endif
    b->setParent(NULL);
  }
  
  return false;
}
  

static inline void safe_set_parent(QWidget *w, QWidget *parent, Qt::WindowFlags f, bool set_window_flags, enum ShowAssertionOrThrowAPIException error_type){
  //printf("w: %p. parent: %p. parent==w->parentWidget(): %d. main window: %p\n", w, parent, (w==NULL ? NULL : w->parentWidget()) == parent, g_main_window);

  if (a_is_a_parent_of_b(w, parent)){
#ifndef CRASHREPORTER_BIN
#ifndef COMPILE_EXECUTABLE
    RError("Error in safe_set_parent(w, parent): \"w\" is a parent of parent. Without this check, Qt would have gone into an infinite recursive loop");
#endif
#endif
    return;
  }

  auto *a = get_oldest_parent(w);
  auto *b = get_oldest_parent(parent);
  
  if (a==b){
#if !defined(RELEASE)
#ifndef CRASHREPORTER_BIN
#ifndef COMPILE_EXECUTABLE
    R_ASSERT_RETURN_IF_FALSE4(a!=b, error_type, "widget::setParent: widget and parent have common ancestor: %p, %p", a, b);
#endif
#endif
#endif
    //return; // it's probably okay to set parent below. I think the "a_is_a_parent_of_b" check above covers all situations that causes crash.
  }

  if (set_window_flags)
    w->setParent(parent, f);
  else
    w->setParent(parent);
}

static inline void safe_set_parent(QWidget *w, QWidget *parent, Qt::WindowFlags f, enum ShowAssertionOrThrowAPIException error_type){
  safe_set_parent(w, parent, f, true, error_type);
}

static inline void safe_set_parent(QWidget *w, QWidget *parent, enum ShowAssertionOrThrowAPIException error_type){
  safe_set_parent(w, parent, Qt::Widget, false, error_type);
}


// Returns true if modality is turned on when 'is_modal'==false.
static inline bool set_window_parent_andor_flags(QWidget *window, QWidget *parent, radium::Modality modality, bool only_set_flags, bool is_converting_widget_to_window, enum ShowAssertionOrThrowAPIException error_type){ //bool is_modal, bool only_set_flags){

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

    // Not sure if this is any point. But it seems kind of right.
    if (window->parent() != NULL)
      safe_set_parent(window, NULL, error_type);
    
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
  
  else {
    
    safe_set_parent(window, parent, f, error_type);
  }
  
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
static inline bool set_window_parent(QWidget *window, QWidget *parent, radium::Modality modality, enum ShowAssertionOrThrowAPIException error_type){
  return set_window_parent_andor_flags(window, parent, modality, false, false, error_type);
}

// Returns true if modality is turned on when 'is_modal'==false.
static inline bool convert_widget_to_window(QWidget *widget, QWidget *parent, radium::Modality modality, enum ShowAssertionOrThrowAPIException error_type){
  return set_window_parent_andor_flags(widget, parent, modality, false, true, error_type);
}

// Returns true if modality is turned on when 'is_modal'==false.
static inline bool set_window_flags(QWidget *window, radium::Modality modality){
  return set_window_parent_andor_flags(window, window->parentWidget(), modality, true, false, ShowAssertionOrThrowAPIException::SHOW_ASSERTION);
}

static inline void set_widget_takes_care_of_painting_everything(QWidget *widget){
  widget->setAttribute(Qt::WA_NoSystemBackground);
  widget->setAttribute(Qt::WA_OpaquePaintEvent);
}


// Call these these three functions in the top of mousePressEvent/mouseMoveEvent/mouseReleaseEvent to ensure mouseReleaseEvent is always called. (workaround for qt design flaw)
bool MOUSE_CYCLE_register(QObject *widget, QEvent *event);
bool MOUSE_CYCLE_move(QObject *widget, QEvent *event);
bool MOUSE_CYCLE_unregister(QObject *widget);
void MOUSE_CYCLE_schedule_unregister_all(void); // Can safely be called at any moment when we know that the current mouse cycle should not run (schedules a call to mouseReleaseEvent if there is a mouse event).
Qt::MouseButtons MOUSE_CYCLE_get_mouse_buttons(void); // Use this one instead of QApplication::mouseButtons() to avoid getting false positives. (It will always return Qt::NoButton for non-mousecycle widgets though)
bool MOUSE_CYCLE_has_moved(void); // returns true if MOUSE_CYCLE_move has been called after MOUSE_CYCLE_register

static inline QPointF get_localpos_from_qevent(QEvent *event){
  auto *mouse_event = dynamic_cast<QMouseEvent*>(event);
  //auto scene_mouse_event = dynamic_cast<QGraphicsSceneMouseEvent*>(event);
  
  if (mouse_event!=NULL)
    return mouse_event->localPos();
  /*
  else if (scene_mouse_event!=NULL)
    return scene_mouse_event->scenePos();
  */
  else
    return QPoint();
}

static inline QPoint get_pos_from_qevent(QEvent *event){
  auto *mouse_event = dynamic_cast<QMouseEvent*>(event);
  auto scene_mouse_event = dynamic_cast<QGraphicsSceneMouseEvent*>(event);
  
  if (mouse_event!=NULL)
    return mouse_event->pos();
  else if (scene_mouse_event!=NULL)
    return scene_mouse_event->scenePos().toPoint();
  else
    return QPoint();
}

static inline QPoint get_globalpos_from_qevent(QEvent *event){
  auto *mouse_event = dynamic_cast<QMouseEvent*>(event);
  auto scene_mouse_event = dynamic_cast<QGraphicsSceneMouseEvent*>(event);
  
  if (mouse_event!=NULL)
    return mouse_event->globalPos();
  else if (scene_mouse_event!=NULL)
    return scene_mouse_event->screenPos(); // good enough. globalpos is only used to avoid double moveevents here.
  else
    return QPoint();
}

static inline Qt::MouseButton get_button_from_qevent(QEvent *event){
  auto *mouse_event = dynamic_cast<QMouseEvent*>(event);
  auto scene_mouse_event = dynamic_cast<QGraphicsSceneMouseEvent*>(event);
  
  if (mouse_event!=NULL)
    return mouse_event->button();
  else if (scene_mouse_event!=NULL)
    return scene_mouse_event->button();
  else
    return Qt::NoButton;
}

static inline Qt::MouseButtons get_buttons_from_qevent(QEvent *event){
  auto *mouse_event = dynamic_cast<QMouseEvent*>(event);
  auto scene_mouse_event = dynamic_cast<QGraphicsSceneMouseEvent*>(event);
  
  if (mouse_event!=NULL)
    return mouse_event->buttons();
  else if (scene_mouse_event!=NULL)
    return scene_mouse_event->buttons();
  else
    return Qt::NoButton;
}

static inline Qt::KeyboardModifiers get_modifiers_from_qevent(QEvent *event){
  auto *mouse_event = dynamic_cast<QMouseEvent*>(event);
  auto scene_mouse_event = dynamic_cast<QGraphicsSceneMouseEvent*>(event);
  
  if (mouse_event!=NULL)
    return mouse_event->modifiers();
  else if (scene_mouse_event!=NULL)
    return scene_mouse_event->modifiers();
  else
    return Qt::NoModifier;
}

namespace radium{

struct MouseCycleEvent{
private:

  friend struct MouseCycleFix;

  QEvent *_event;
  QMouseEvent *_mouse_event;
  QGraphicsSceneMouseEvent *_scene_mouse_event;
  bool _is_real_event;

  bool _is_ctrl_clicking;

  QPoint _pos;
  
public:

  MouseCycleEvent(QEvent *event, bool is_real_event, bool is_ctrl_clicking = false)
  : _event(event)
  , _is_real_event(is_real_event)
  {
    _mouse_event = dynamic_cast<QMouseEvent*>(event);
    _scene_mouse_event = dynamic_cast<QGraphicsSceneMouseEvent*>(event);

    if(_mouse_event)
      R_ASSERT(_scene_mouse_event==NULL);
    else
      R_ASSERT(_scene_mouse_event!=NULL);

    _is_ctrl_clicking = is_ctrl_clicking || (get_button_from_qevent(event)==Qt::RightButton && (get_modifiers_from_qevent(event) & get_modifier_may_causing_rightclick()));

    //printf("...._is_ctrl_clicking: %d. modifiers: %x. modifier may causing: %x\n", _is_ctrl_clicking, (unsigned int)get_modifiers_from_qevent(event), (unsigned int)get_modifier_may_causing_rightclick());

    _pos = get_pos_from_qevent(event);
  }

  QPoint pos(void) const {
    R_ASSERT(_mouse_event!=NULL);
    return _pos;
  }

  QPointF scenePos(void) const {
    if(_scene_mouse_event!=NULL)
      return _scene_mouse_event->scenePos();
    else
      return QPointF(0,0);
  }

  QPointF localPos(void) const {
    return get_localpos_from_qevent(_event);
  }

  int x(void) const {
    if(_mouse_event)
      return _mouse_event->x();
    else
      return 0.0;
  }
  
  int y(void) const {
    if(_mouse_event)
      return _mouse_event->y();
    else
      return 0.0;
  }

  Qt::MouseButton button(void) const {
    if (_is_ctrl_clicking)
      return Qt::LeftButton;

    return get_button_from_qevent(_event);
  }

  Qt::KeyboardModifiers modifiers(void) const {
    return get_modifiers_from_qevent(_event);
  }
  
  void accept(void){
    if(_is_real_event){
      if(_mouse_event)
        return _mouse_event->accept();
      else
        return _scene_mouse_event->accept();
    }
  }

  bool is_real_event(void) const {
    return _is_real_event;
  }
  
  QMouseEvent *get_qtevent(void) const{
    if (_is_real_event)
      return _mouse_event;
    else
      return NULL;
  }
  
  QGraphicsSceneMouseEvent *get_qtscene_event(void) const{
    if (_is_real_event)
      return _scene_mouse_event;
    else
      return NULL;
  }
};

#ifdef TRACKER_EVENTS_DEFINE
static inline int getMouseButtonEventID(radium::MouseCycleEvent &qmouseevent){
  return getMouseButtonEventID2((Qt::MouseButton)qmouseevent.button(), (Qt::KeyboardModifiers)qmouseevent.modifiers());
}
#endif

struct MouseCycleFix{
  int _last_mouse_cycle_x = -10000;
  int _last_mouse_cycle_y = -10000;

  QPointer<QObject> _widget;
  bool _is_ctrl_clicking = false;
  
  virtual void fix_mousePressEvent(radium::MouseCycleEvent &event) = 0;
  virtual void fix_mouseMoveEvent(radium::MouseCycleEvent &event) = 0;
  virtual void fix_mouseReleaseEvent(radium::MouseCycleEvent &event) = 0;

  void cycle_mouse_press_event(QObject *w, QEvent *event, bool is_real_qevent){
    _widget = w;
    
    _last_mouse_cycle_x = -10000;
    _last_mouse_cycle_y = -10000;
    _is_ctrl_clicking = false;
    
    if(MOUSE_CYCLE_register(w, event)){
      radium::MouseCycleEvent event2(event, is_real_qevent);
      
      _is_ctrl_clicking = event2._is_ctrl_clicking;
      
      fix_mousePressEvent(event2);
    }
  }

  void cycle_mouse_move_event(QObject *w, QEvent *event, bool is_real_qevent){
    if(MOUSE_CYCLE_move(w, event)){

      QPoint pos;

      QWidget *widget = dynamic_cast<QWidget*>(_widget.data());
      
      if (widget != NULL)
        pos = widget->mapFromGlobal(get_globalpos_from_qevent(event));
      else
        pos = get_pos_from_qevent(event);
        
      int x = pos.x();
      int y = pos.y();
      
      if (_last_mouse_cycle_x != x || _last_mouse_cycle_y != y) {
        _last_mouse_cycle_x = x;
        _last_mouse_cycle_y = y;
        
        radium::MouseCycleEvent event2(event, is_real_qevent, _is_ctrl_clicking);
        event2._pos = pos;
                
        fix_mouseMoveEvent(event2);
      }else{
#if 0 //!defined(RELEASE)
        printf("Note: mouse-move called with same x and y values. Ignored.\n");
#endif
      }
    }
  }
  void cycle_mouse_release_event(QObject *w, QEvent *event, bool is_real_qevent){
    _last_mouse_cycle_x = -10000;
    _last_mouse_cycle_y = -10000;
    
    if(MOUSE_CYCLE_unregister(w)){
      radium::MouseCycleEvent event2(event, is_real_qevent, _is_ctrl_clicking);

      QWidget *widget = dynamic_cast<QWidget*>(_widget.data());
      if (widget)
        event2._pos = widget->mapFromGlobal(get_globalpos_from_qevent(event));
      
      fix_mouseReleaseEvent(event2);
    }

    _is_ctrl_clicking = false;
  }
};
}

#define MOUSE_CYCLE_CALLBACKS_FOR_QT                                    \
  void	mousePressEvent(QMouseEvent *event) override{cycle_mouse_press_event(this, event, true);} \
  void	mouseMoveEvent(QMouseEvent *event) override{cycle_mouse_move_event(this, event, true);} \
  void	mouseReleaseEvent(QMouseEvent *event) override{cycle_mouse_release_event(this, event, true);}




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
    QElapsedTimer time;
    bool left_mouse_is_down = false;
    
    ASMTimer(QWidget *parent_)
      : QTimer(parent_)
    {
      time.start();
      setInterval(10);
    }

    void timerEvent ( QTimerEvent * e ) override {
      if (MOUSE_CYCLE_get_mouse_buttons()==Qt::LeftButton)
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
  //bool _splashscreen_visible;

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
    /*
    _splashscreen_visible = g_splashscreen!=NULL && g_splashscreen->isVisible();

    if (_splashscreen_visible)
      g_splashscreen->hide();
    */
    QMessageBox::showEvent(event_);
  }
  
  void hideEvent(QHideEvent *event_) override {
    /*
    if (_splashscreen_visible && g_splashscreen!=NULL)
      g_splashscreen->show();
    */
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
    QElapsedTimer _time;
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

static inline void updateWidgetRecursively(QObject *object, bool is_child = false){
  if (object != NULL){

    QWidget *w = dynamic_cast<QWidget*>(object);

    if (w != NULL && w->isVisible()) {

      if (is_child && w->isWindow())
        return;
      
      w->update();
        
      for(auto *c : object->children())
        updateWidgetRecursively(c, true);
      
    }
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
      if (widget->window() != widget){
        R_ASSERT_NON_RELEASE(false);
      } else {
        geometry = widget->saveGeometry();
        has_stored_geometry = true;
        //printf("   SAVING geometry\n");
      }
    }

    void restore(QWidget *widget){
      if (widget->window() != widget){
        R_ASSERT_NON_RELEASE(false);
      } else {
        if (has_stored_geometry){

          //printf("   RESTORING geometry\n");
          widget->restoreGeometry(geometry);
          
        }else{
          
          //printf("   88888888888888888888888888888888888888888888 NO geometry stored\n");
          
          if (move_window_to_centre_first_time_its_opened)
            moveWindowToCentre(widget);
        }
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
     : QDialog(parent_!=NULL ? parent_->window() : g_main_window, Qt::Window) // | Qt::Tool)
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

static inline void myFillRectHorizontalGradient(QPainter &p, QRectF rect, const QColor &color, bool do_gradient = true, int how_much_gradient = 15){
  QPen pen = p.pen();
  p.setPen(Qt::NoPen);
  
  
  if (do_gradient){
    int lighter = 100 + how_much_gradient;
    int darker = 100 + how_much_gradient;
    
    QLinearGradient gradient(rect.topLeft(), rect.topRight());
    gradient.setColorAt(0, color.lighter(lighter));
    gradient.setColorAt(1, color.darker(darker));
    p.setBrush(gradient);
  } else {
    p.setBrush(color);
  }
  p.drawRect(rect);
  p.setBrush(Qt::NoBrush);
  p.setPen(pen);
}

static inline void myFillRect(QPainter &p, QRectF rect, const QColor &color, bool do_gradient = true, int how_much_gradient = 10){
  QPen pen = p.pen();
  p.setPen(Qt::NoPen);
  
  if (do_gradient){

    int lighter = 100 + how_much_gradient;
    int darker = 100 + how_much_gradient;

    QLinearGradient gradient(rect.topLeft(), rect.bottomLeft());
    gradient.setColorAt(0, color.lighter(lighter));
    gradient.setColorAt(1, color.darker(darker));
    p.setBrush(gradient);
  } else {
    p.setBrush(color);
  }
  p.drawRect(rect);
  p.setBrush(Qt::NoBrush);
  p.setPen(pen);
}
static inline void myFillRoundedRect(QPainter &p, QRectF rect, const QColor &color, float rounding, bool do_gradient = true){
  QPen pen = p.pen();
  p.setPen(Qt::NoPen);
  if (do_gradient){
    QLinearGradient gradient(rect.topLeft(), rect.bottomLeft());
    gradient.setColorAt(0, color.lighter(125));
    gradient.setColorAt(1, color.darker(125));
    p.setBrush(gradient);
  } else {
    p.setBrush(color);
  }
  p.drawRoundedRect(rect, rounding, rounding);
  p.setBrush(Qt::NoBrush);
  p.setPen(pen);
}

// returns true if all text was painted (in Qt_Sequencer.cpp)
bool myDrawText(QPainter &p, QRectF rect, QString text, int flags = Qt::AlignLeft | Qt::AlignTop, bool wrap_lines = false, int rotate = 0, bool scale_font_size = false, bool cut_text_to_fit = false);



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
  bool _has_hidden_progress = false;
  
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

    if (true || ATOMIC_GET(g_qtgui_has_started_step2)==true){
      GFX_HideProgress();
      _has_hidden_progress = true;
    }
  }
  
  ~ScopedExec(){

    if (_has_hidden_progress)
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

  MOUSE_CYCLE_schedule_unregister_all();
  
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
    bool hide_progress = ATOMIC_GET_RELAXED(g_qtgui_has_started_step2)==true;
    
    if (true || hide_progress)
      GFX_HideProgress();
    
    GL_lock();
    int ret = widget->exec();
    GL_unlock();

    if (true || hide_progress)
      GFX_ShowProgress();
    
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

namespace radium{
  class ScopedQClipRect{
    QPainter &_p;

    const bool _was_clipping;
    const QRegion _prev_clip_region;

    bool _doit;
    
  public:
    
    ScopedQClipRect(QPainter &p, const QRectF &rect, bool doit=true)
      : _p(p)
      , _was_clipping(p.hasClipping())
      , _prev_clip_region(_was_clipping ? p.clipRegion() : QRegion())
      , _doit(doit)
    {
      if (!_doit)
        return;
   
      if (_was_clipping)
        p.setClipRect(rect, Qt::IntersectClip);
      else{
        p.setClipRect(rect, Qt::ReplaceClip);      
        p.setClipping(true);
      }
    }

    ScopedQClipRect(QPainter &p, qreal x1, qreal y1, qreal x2, qreal y2)
      : ScopedQClipRect(p, QRectF(x1, y1, x2-x1, y2-y1))
    {}
    
    ~ScopedQClipRect(){
      if (!_doit)
        return;
      
      if(_was_clipping)
        _p.setClipRegion(_prev_clip_region);
      else      
        _p.setClipping(false);
    }
  };
}

#endif // RADIUM_QT_HELPERS
