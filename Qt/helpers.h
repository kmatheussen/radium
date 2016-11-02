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

#include "../OpenGL/Widget_proc.h"
#include "../common/keyboard_focus_proc.h"

#include "../api/api_proc.h"

#define PUT_ON_TOP 0

extern bool g_radium_runs_custom_exec;

extern void set_editor_focus(void);

extern QMainWindow *g_main_window;
extern QSplashScreen *g_splashscreen;

namespace radium{
  struct ASMTimer : public QTimer{
    QTime time;
    bool left_mouse_is_down = false;
    
    ASMTimer(QWidget *parent)
      :QTimer(parent)
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
    
    VerticalScroll(QWidget *parent)
      :QScrollArea(parent)
    {
      setVerticalScrollBarPolicy(Qt::ScrollBarAlwaysOn);
      setWidgetResizable(true);
      
      QWidget *contents = new QWidget(this);
      
      layout = new QVBoxLayout(contents);
      layout->setSpacing(1);
      
      contents->setLayout(layout);
      
      setWidget(contents);    
    }
    
    void addWidget(QWidget *widget){
      layout->addWidget(widget);
    }
    
    void removeWidget(QWidget *widget){
      layout->removeWidget(widget);
    }
  };


}


struct MyQMessageBox : public QMessageBox {
  bool _splashscreen_visible;
  
  MyQMessageBox(QWidget *parent = NULL)
    : QMessageBox(parent!=NULL ? parent : g_main_window)
  {
    setWindowModality(Qt::ApplicationModal);
    setWindowFlags(Qt::Window | Qt::Tool);
  }
  
  ~MyQMessageBox(){
  }
  
  void showEvent(QShowEvent *event){
    _splashscreen_visible = g_splashscreen!=NULL && g_splashscreen->isVisible();

    if (_splashscreen_visible)
      g_splashscreen->hide();

    QMessageBox::showEvent(event);
  }
  
  void hideEvent(QHideEvent *event) {
    if (_splashscreen_visible && g_splashscreen!=NULL)
      g_splashscreen->show();

    QMessageBox::hideEvent(event);
  }
  
};
  
struct RememberGeometryQDialog : public QDialog {

  QByteArray geometry;
  bool has_stored_geometry;

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
  RememberGeometryQDialog(QWidget *parent)
    : QDialog(parent!=NULL ? parent : g_main_window, Qt::Window | Qt::Tool)
    , has_stored_geometry(false)
#if PUT_ON_TOP
    , timer(this)
#endif
  {    
    //QDialog::setWindowModality(Qt::ApplicationModal);
    //setWindowFlags(windowFlags() | Qt::WindowStaysOnTopHint);
  }
  void setVisible(bool visible) override {      
    //printf("   Set visible %d\n",visible);

    if (!visible){
      
      geometry = saveGeometry();
      has_stored_geometry = true;
      
    } else {
      
      if (has_stored_geometry)
        restoreGeometry(geometry);

    }

    QDialog::setVisible(visible);    
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



static inline int safeExec(QMessageBox *widget){
  int ret;

  R_ASSERT_RETURN_IF_FALSE2(g_radium_runs_custom_exec==false, 0);

  g_radium_runs_custom_exec = true;
  
  obtain_keyboard_focus();
  
  GL_lock();{
    ret = widget->exec();
  }GL_unlock();

  release_keyboard_focus();

  g_radium_runs_custom_exec = false;
  
  return ret;
}

static inline int safeExec(QMessageBox &widget){
  int ret;

  R_ASSERT_RETURN_IF_FALSE2(g_radium_runs_custom_exec==false, 0);

  g_radium_runs_custom_exec = true;
    
  obtain_keyboard_focus();

  GL_lock();{
    ret = widget.exec();
  }GL_unlock();

  release_keyboard_focus();

  g_radium_runs_custom_exec = false;
  
  return ret;
}

static inline int safeExec(QDialog *widget){
  int ret;

  R_ASSERT_RETURN_IF_FALSE2(g_radium_runs_custom_exec==false, 0);

  g_radium_runs_custom_exec = true;
  
  obtain_keyboard_focus();

  GL_lock();{
    ret = widget->exec();
  }GL_unlock();

  release_keyboard_focus();

  g_radium_runs_custom_exec = false;
  
  return ret;
}

static inline QAction *safeExec(QMenu *widget){
  QAction *ret;

  R_ASSERT_RETURN_IF_FALSE2(g_radium_runs_custom_exec==false, 0);

  g_radium_runs_custom_exec = true;
    
  obtain_keyboard_focus();

  if (doModalWindows()) {
    GL_lock();{
      ret = widget->exec(QCursor::pos());
    }GL_unlock();
  }else{
    GL_lock();{
      GL_pause_gl_thread_a_short_while();
    }GL_unlock();    
    ret = widget->exec(QCursor::pos());
  }
  
  release_keyboard_focus();

  g_radium_runs_custom_exec = false;
  
  return ret;
}

static inline void safeShow(QWidget *widget){  
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
