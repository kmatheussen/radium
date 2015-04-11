#ifndef RADIUM_QT_HELPERS
#define RADIUM_QT_HELPERS

#include <QMessageBox>

#include "../OpenGL/Widget_proc.h"


static inline int safeExec(QMessageBox *widget){
  GL_ScopedLock lock;
  return widget->exec();
}

static inline int safeExec(QMessageBox &widget){
  GL_ScopedLock lock;
  return widget.exec();
}

static inline int safeExec(QDialog *widget){
  GL_ScopedLock lock;
  return widget->exec();
}



#endif // RADIUM_QT_HELPERS
