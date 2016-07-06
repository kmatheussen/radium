#ifndef RADIUM_QT_HELPERS
#define RADIUM_QT_HELPERS

#include <QMessageBox>
#include <QMenu>
#include <QTimer>

#include "../OpenGL/Widget_proc.h"
#include "../common/keyboard_focus_proc.h"

#include "../api/api_proc.h"

extern bool radium_runs_custom_exec;

class RememberGeometryQDialog : public QDialog {

  QByteArray geometry;
  bool has_stored_geometry;

  
  struct Timer : public QTimer {
    QWidget *parent;
    Timer(QWidget *parent)
      :parent(parent)
    {
      setInterval(200);
      start();      
    }
    void timerEvent ( QTimerEvent * e ){
      //printf("Raising parent\n");
      if (parent->isVisible())
        parent->raise();
    }
  };
  

  Timer timer;
  
public:
  RememberGeometryQDialog(QWidget *parent)
    : QDialog(parent)
    , has_stored_geometry(false)
    , timer(this)
  {
    setWindowFlags(Qt::WindowStaysOnTopHint);
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

  radium_runs_custom_exec = true;
  
  obtain_keyboard_focus();
  
  GL_lock();{
    ret = widget->exec();
  }GL_unlock();

  release_keyboard_focus();

  radium_runs_custom_exec = false;
  
  return ret;
}

static inline int safeExec(QMessageBox &widget){
  int ret;

  radium_runs_custom_exec = true;
    
  obtain_keyboard_focus();

  GL_lock();{
    ret = widget.exec();
  }GL_unlock();

  release_keyboard_focus();

  radium_runs_custom_exec = false;
  
  return ret;
}

static inline int safeExec(QDialog *widget){
  int ret;

  radium_runs_custom_exec = true;
  
  obtain_keyboard_focus();

  GL_lock();{
    ret = widget->exec();
  }GL_unlock();

  release_keyboard_focus();

  radium_runs_custom_exec = false;
  
  return ret;
}

static inline QAction *safeExec(QMenu *widget){
  QAction *ret;

  radium_runs_custom_exec = true;
    
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

  radium_runs_custom_exec = false;
  
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
