#ifndef RADIUM_QT_HELPERS
#define RADIUM_QT_HELPERS

#include <QMessageBox>

#include "../OpenGL/Widget_proc.h"


static inline int safeExec(QMessageBox *widget){
  int ret;
  GL_lock();{
    ret = widget->exec();
  }GL_unlock();
  return ret;
}

static inline int safeExec(QMessageBox &widget){
  int ret;
  GL_lock();{
    ret = widget.exec();
  }GL_unlock();
  return ret;
}

static inline int safeExec(QDialog *widget){
  int ret;
  GL_lock();{
    ret = widget->exec();
  }GL_unlock();
  return ret;
}


#endif // RADIUM_QT_HELPERS
