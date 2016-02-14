#ifndef RADIUM_QT_HELPERS
#define RADIUM_QT_HELPERS

#include <QMessageBox>

#include "../OpenGL/Widget_proc.h"
#include "../common/keyboard_focus_proc.h"

#include "../api/api_proc.h"



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

  obtain_keyboard_focus();
  
  GL_lock();{
    ret = widget->exec();
  }GL_unlock();

  release_keyboard_focus();
  
  return ret;
}

static inline int safeExec(QMessageBox &widget){
  int ret;

  obtain_keyboard_focus();

  GL_lock();{
    ret = widget.exec();
  }GL_unlock();

  release_keyboard_focus();
  
  return ret;
}

static inline int safeExec(QDialog *widget){
  int ret;

  obtain_keyboard_focus();

  GL_lock();{
    ret = widget->exec();
  }GL_unlock();

  release_keyboard_focus();
  
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
