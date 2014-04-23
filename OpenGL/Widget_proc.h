#ifndef OPENGL_WIDGET_PROC_H
#define OPENGL_WIDGET_PROC_H

#include <QWidget>

QWidget *GL_create_widget(QWidget *parent);
void GL_stop_widget(QWidget *widget);

void GL_lock(void);
void GL_unlock(void);

#endif
