#ifndef OPENGL_WIDGET_PROC_H
#define OPENGL_WIDGET_PROC_H

#include <stdint.h>

#ifdef __cplusplus
class QWidget;

  extern QWidget *GL_create_widget(QWidget *parent);
  extern void GL_stop_widget(QWidget *widget);

  extern "C" {
#endif

extern bool GL_should_do_modal_windows(void);

extern bool GL_notify_that_main_window_is_exposed(void);
    
extern void GL_pause_gl_thread_a_short_while(void);
extern void GL_EnsureMakeCurrentIsCalled(void);
  
extern void GL_erase_estimated_vblank(void);
extern double GL_get_estimated_vblank(void);

extern void GL_set_vsync(bool onoff);
extern bool GL_get_vsync(void);

extern void GL_set_multisample(int size);
extern int GL_get_multisample(void);

extern void GL_set_safe_mode(bool onoff);
extern bool GL_get_safe_mode(void);

extern void GL_set_pause_rendering_on_off(bool onoff);
extern bool GL_get_pause_rendering_on_off(void);

  
extern void GL_lock(void);
extern void GL_unlock(void);
extern bool GL_maybeLock(void);

extern void GL_draw_lock(void);
extern void GL_draw_unlock(void);
    
#include "../common/atomic.h"
    
extern DEFINE_ATOMIC(char *, GE_vendor_string);
extern DEFINE_ATOMIC(char *, GE_renderer_string);
extern DEFINE_ATOMIC(char *, GE_version_string);
extern DEFINE_ATOMIC(uint32_t, GE_opengl_version_flags);


extern void GE_set_curr_realline(int curr_realline);

#ifdef __cplusplus
  }
#endif

#endif // OPENGL_WIDGET_PROC_H
