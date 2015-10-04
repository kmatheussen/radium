#ifndef RADIUM_QT_HELPERS
#define RADIUM_QT_HELPERS

#include <QMessageBox>

#include "../OpenGL/Widget_proc.h"

#include "../api/api_proc.h"


extern int num_users_of_keyboard;


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
  
  num_users_of_keyboard++;
  GL_lock();{
    ret = widget->exec();
  }GL_unlock();
  num_users_of_keyboard--;
  
  return ret;
}

static inline int safeExec(QMessageBox &widget){
  int ret;
  
  num_users_of_keyboard++;
  GL_lock();{
    ret = widget.exec();
  }GL_unlock();
  num_users_of_keyboard--;
  
  return ret;
}

static inline int safeExec(QDialog *widget){
  int ret;

  num_users_of_keyboard++;
  GL_lock();{
    ret = widget->exec();
  }GL_unlock();
  num_users_of_keyboard--;
  
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
