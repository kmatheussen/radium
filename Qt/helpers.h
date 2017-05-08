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

#include "../OpenGL/Widget_proc.h"
#include "../common/keyboard_focus_proc.h"
#include "../common/visual_proc.h"

#include "../api/api_gui_proc.h"
#include "../api/api_proc.h"

#define PUT_ON_TOP 0

extern bool g_radium_runs_custom_exec;

extern void set_editor_focus(void);

extern QVector<QWidget*> g_static_toplevel_widgets;

extern QMainWindow *g_main_window;
extern QSplashScreen *g_splashscreen;
extern QPointer<QWidget> g_current_parent_before_qmenu_opened; // Only valid if !g_curr_popup_qmenu.isNull()
extern QPointer<QMenu> g_curr_popup_qmenu;

typedef QPointer<QObject> IsAlive;

static bool can_widget_be_parent_questionmark(QWidget *w){
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
  if (!g_static_toplevel_widgets.contains(w))
    return false;
  
  return true;
}

// Can only return a widget that is a member of g_static_toplevel_widgets.
static inline QWidget *get_current_parent(bool may_return_current_parent_before_qmenu_opened = true){

  if (may_return_current_parent_before_qmenu_opened && !g_curr_popup_qmenu.isNull() && !g_current_parent_before_qmenu_opened.isNull()){
    //printf("1111 %p\n", g_current_parent_before_qmenu_opened.data());
    return g_current_parent_before_qmenu_opened;
  }

  QWidget *ret = QApplication::activeModalWidget();
  //printf("2222 %p\n", ret);
  if (can_widget_be_parent_questionmark(ret)){
    return ret;
  }

  ret = QApplication::focusWidget();
  //printf("333 %p\n", ret);
  if (can_widget_be_parent_questionmark(ret)){
    return ret;
  }

  ret = QApplication::activePopupWidget();
  //printf("333555 %p\n", ret);
  if (can_widget_be_parent_questionmark(ret)){
    return ret;
  }

  ret = QApplication::activeWindow();
  //printf("444 %p\n", ret);
  if (can_widget_be_parent_questionmark(ret)){
    return ret;
  }

  /*
  QWidget *mixer_strips_widget = MIXERSTRIPS_get_curr_widget();
  printf("555 %p\n", ret);
  if (mixer_strips_widget!=NULL){
    return mixer_strips_widget;
  }
  */
  
  ret = QApplication::widgetAt(QCursor::pos());
  //printf("666 %p\n", ret);
  if (g_static_toplevel_widgets.contains(ret)){
    return ret;
  }

  //printf("777\n");
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

// Returns true if modality is turned on when 'is_modal'==false.
static inline bool set_window_parent_andor_flags(QWidget *window, QWidget *parent, bool is_modal, bool only_set_flags){

//  #if defined(FOR_MACOSX)
#if 1
  // Although these hacks are only needed on OSX, it's probably best to let the program behave similarly on all platforms. (this behavior is not that bad)
  // Besides, it may be that setParent isn't always working properly on windows and linux either, for all I know.
  const bool set_parent_working_properly = false;
#else
  const bool set_parent_working_properly = true;
#endif
  
  Qt::WindowFlags f = Qt::Window;
  bool force_modal = false;

  if (!set_parent_working_properly){
    if (parent==g_main_window) {
      f = Qt::Window | Qt::Tool; // On OSX, you can't create an "on-top-of hierarchy" by setting the windows parent. But for level 1, we can work around this by setting the Qt::Tool flag.
      window->setAttribute(Qt::WA_MacAlwaysShowToolWindow, true); // Qt::Tool windows dissapear on OSX if the application is not active. (At least according to Qt documentation. I haven't tested it.)
    } else
      force_modal=true; // Qt::Tool doesn't work for levels larger than 1 (it doesn't work if the parent is a Qt::Tool window), so we work around it by using modal windows. Modal windows seems to always be on top of parent window.
  }
  
  if (only_set_flags)
    window->setWindowFlags(f);
  else
    window->setParent(parent, f);
 
  if (force_modal || is_modal) {

    if (window->windowModality()!=Qt::ApplicationModal)
      window->setWindowModality(Qt::ApplicationModal);

    return !is_modal;

  } else {

    if (!is_modal && window->windowModality()!=Qt::NonModal) // We may have forcefully turned on modality in a previous call. Turn it off now.
      window->setWindowModality(Qt::NonModal);

    return false;
    
  }
}

// Returns true if modality is turned on when 'is_modal'==false.
static inline bool set_window_parent(QWidget *window, QWidget *parent, bool is_modal){
  return set_window_parent_andor_flags(window, parent, is_modal, false);
}

// Returns true if modality is turned on when 'is_modal'==false.
static inline bool set_window_flags(QWidget *window, bool is_modal){
  return set_window_parent_andor_flags(window, window->parentWidget(), is_modal, true);
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

    void timerEvent ( QTimerEvent * e ){
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
  static MyQMessageBox *create(QWidget *parent = NULL){
    return new MyQMessageBox(parent);
  }
  
 private:  
    
  MyQMessageBox(QWidget *parent_ = NULL)
    : QMessageBox(parent_!=NULL ? parent_ : get_current_parent())
  {
    set_window_flags(this, true);
  }

 public:
  
  ~MyQMessageBox(){
    printf("MyQMessageBox %p deleted\n", this);
  }
  
  void showEvent(QShowEvent *event_){
    _splashscreen_visible = g_splashscreen!=NULL && g_splashscreen->isVisible();

    if (_splashscreen_visible)
      g_splashscreen->hide();

    QMessageBox::showEvent(event_);
  }
  
  void hideEvent(QHideEvent *event_) {
    if (_splashscreen_visible && g_splashscreen!=NULL)
      g_splashscreen->show();

    QMessageBox::hideEvent(event_);
  }
  
};

namespace radium{

  /*
    Qt makes it _almost_ impossible to remember geometry of windows (both qdialog and qwidget) without adding a timer that monitors what's happening and tries to do the right thing.
    (and we most certainly don't want to that)

    The problem is that Qt always opens the windows at the original position when calling setVisible(true) or show(). There's no way to override that. It would be the most
    natural thing in the world to override, but there is no way. The only way to open at the original position is to remember geometry when hiding, and restore when showing.
    Unfortunatly things becomes very complicated since it's unclear (probably also for those who develops Qt) when the widgets are actually shown and hidden.

    However, I have found that the following seems to work (at least for Qt 5.5.1 on Linux with FVWM2):

    1. Override setVisible like this:

    void setVisible(bool visible) override {
      if (visible==false && isVisible()==false)                           
        return;                                                           
      if (visible==true && isVisible()==true)                             
        return;                                                           
      super::setVisible(visible);
      if (visible && window()==this)
        remember_geometry.restore(this);
    }

    2. Override hideEvent like this:

    void hideEvent(QHideEvent *event) override {
      if (window()==this)
        remember_geometry.save(this);
    }

    ANY other combination will fail in more or less subtle ways. Sigh.

    However, the Preferences dialog seems to remember width and height, but not position. Sigh. It's just impossible.

    Perhaps it would take less time to fork Qt and fix all these weird things than to add all these ad-hoc hacks.
   */
  
  struct RememberGeometry{
    QByteArray geometry;
    bool has_stored_geometry = false;

    void save(QWidget *widget){
      geometry = widget->saveGeometry();
      has_stored_geometry = true;
    }

    void restore(QWidget *widget){
      if (has_stored_geometry)
        widget->restoreGeometry(geometry);
    }

    void remember_geometry_setVisible_override_func(QWidget *widget, bool visible) {
      //printf("   AUIAUAUAU Set visible %d\n",visible);
      
      if (!visible){

        save(widget);
        
      } else {
        
        restore(widget);        
      }
      
      //QDialog::setVisible(visible);    
    }
  };
}

  
struct RememberGeometryQDialog : public QDialog {

  radium::RememberGeometry remember_geometry;

  static int num_open_dialogs;

#if PUT_ON_TOP

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
   RememberGeometryQDialog(QWidget *parent_, bool is_modal)
    : QDialog(parent_!=NULL ? parent_ : g_main_window, Qt::Window) // | Qt::Tool)
#if PUT_ON_TOP
    , timer(this)
#endif
  {
    set_window_flags(this, is_modal);
    //QDialog::setWindowModality(Qt::ApplicationModal);
    //setWindowFlags(windowFlags() | Qt::WindowStaysOnTopHint);
  }
  
  // See comment in helpers.h for the radium::RememberGeometry class.
  virtual void setVisible(bool visible) override {
    if (visible==false && isVisible()==false)                           
      return;                                                           
    if (visible==true && isVisible()==true)                             
      return;                                                           
    QWidget::setVisible(visible);    
    if (visible && window()==this)
      remember_geometry.restore(this);
  }
  
  // See comment in helpers.h for the radium::RememberGeometry class.
  virtual void hideEvent(QHideEvent *event_) override {
    //printf("        HIDEVENT2\n");
    if (window()==this)
      remember_geometry.save(this);
  }

};

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
  bool _lock;
  
  ScopedExec(bool lock=true)
    : _lock(lock)
  {      
    obtain_keyboard_focus();
    
    g_radium_runs_custom_exec = true;
    
    GFX_HideProgress();
    
    if (_lock)  // GL_lock <strike>is</strike> was needed when using intel gfx driver to avoid crash caused by opening two opengl contexts simultaneously from two threads.
      GL_lock();
  }
  
  ~ScopedExec(){
    if (_lock)
      GL_unlock();
    
    GFX_ShowProgress();
    
    g_radium_runs_custom_exec = false;
    
    release_keyboard_focus();
  }
};
}


// Happens sometimes that there are more than two menues visible at the same time. (probably got something to do with non-async menues)
static inline void closePopup(void){
  if (!g_curr_popup_qmenu.isNull()){
    g_curr_popup_qmenu->hide(); // safer.
  //g_curr_popup_qmenu->deleteLater(); // We might be called from the "triggered" callback of the menu.
  }
}

static inline void set_those_menu_variables_when_starting_a_popup_menu(QMenu *menu_to_be_started){
  if (!g_curr_popup_qmenu.isNull()) // Don't set it if there's a menu open already.
    g_current_parent_before_qmenu_opened = get_current_parent(false);

  g_curr_popup_qmenu = menu_to_be_started;
}

static inline int safeExec(QMessageBox *widget){
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

  radium::ScopedExec scopedExec;

  return widget->exec();
}

/*
static inline int safeExec(QMessageBox &widget){
  closePopup();

  R_ASSERT_RETURN_IF_FALSE2(g_radium_runs_custom_exec==false, 0);

  radium::ScopedExec scopedExec;

  return widget.exec();
}
*/

static inline int safeExec(QDialog *widget){
  closePopup();

  R_ASSERT_RETURN_IF_FALSE2(g_radium_runs_custom_exec==false, 0);

  radium::ScopedExec scopedExec;

  return widget->exec();
}

static inline QAction *safeMenuExec(QMenu *widget){

  // safeExec might be called recursively if you double click right mouse button to open a pop up menu. Seems like a bug or design flaw in Qt.
#if !defined(RELEASE)
  if (g_radium_runs_custom_exec==true)
    GFX_Message(NULL, "Already runs custom exec");
#endif

  closePopup();
  set_those_menu_variables_when_starting_a_popup_menu(widget);
      
  if (doModalWindows()) {
    radium::ScopedExec scopedExec;
    return widget->exec(QCursor::pos());
  }else{
    radium::ScopedExec scopedExec(false);
    GL_lock();{
      GL_pause_gl_thread_a_short_while();
    }GL_unlock();    
    return widget->exec(QCursor::pos());
  }
}

static inline void safeMenuPopup(QMenu *menu){
  closePopup();
  set_those_menu_variables_when_starting_a_popup_menu(menu);

  menu->popup(QCursor::pos());
}

static inline void safeShow(QWidget *widget){  
  closePopup();

  GL_lock(); {
    GL_pause_gl_thread_a_short_while();
    widget->show();
    widget->raise();
    widget->activateWindow();
  }GL_unlock();
}

static inline void safeShowOrExec(QDialog *widget){
  if (doModalWindows())
    safeExec(widget);
  else
    safeShow(widget);
}

#endif // RADIUM_QT_HELPERS
