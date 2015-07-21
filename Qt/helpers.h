#ifndef RADIUM_QT_HELPERS
#define RADIUM_QT_HELPERS

#include <QMessageBox>

#include "../OpenGL/Widget_proc.h"

#include "../api/api_proc.h"


extern int num_users_of_keyboard;


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
