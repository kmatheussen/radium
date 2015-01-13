#ifndef OPENGL_WIDGET_PROC_H
#define OPENGL_WIDGET_PROC_H

#ifdef __cplusplus
#include <QWidget>

QWidget *GL_create_widget(QWidget *parent);
void GL_stop_widget(QWidget *widget);
#endif

#ifdef __cplusplus
extern "C" {
#endif

extern void GL_erase_estimated_vblank(void);

extern void GL_lock(void);
extern void GL_unlock(void);

extern void GE_set_curr_realline(int curr_realline);

#ifdef __cplusplus
}
#endif

#endif
