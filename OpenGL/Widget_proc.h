#ifndef OPENGL_WIDGET_PROC_H
#define OPENGL_WIDGET_PROC_H

#include <stdint.h>

extern bool g_gl_widget_started;


// Should be called as early as possible. Might take some time though, so progress window should open first.
bool GL_check_compatibility(void);


#ifdef __cplusplus
class QWidget;

  extern QWidget *GL_create_widget(QWidget *parent);
  extern void GL_stop_widget(QWidget *widget);
//extern void GL_maybe_estimate_vblank(QWidget *widget);

  extern "C" {
#endif

#if THREADED_OPENGL
	static inline void GL_update(void){}
#else
 	extern void GL_update(void);
#endif

extern double GL_get_vblank(void);

//extern bool GL_should_do_modal_windows(void);

    // returns 0 if gl not started
    // returns 1 if gl just started (only returned once)
    // returns 2 if gl had been started earlier
extern int GL_maybe_notify_that_main_window_is_exposed(int interval);
    
static inline void GL_pause_gl_thread_a_short_while(void){}
static inline void GL_EnsureMakeCurrentIsCalled(void){}
  
extern void GL_erase_estimated_vblank(void);
extern double GL_get_estimated_vblank(void);

extern void GL_set_vsync(bool onoff);
extern bool GL_get_vsync(void);

extern void GL_set_multisample(int size);
extern int GL_get_multisample(void);

extern void GL_set_safe_mode(bool onoff);
extern bool GL_get_safe_mode(void);

extern bool GL_get_high_render_thread_priority(void);
extern void GL_set_high_render_thread_priority(bool onoff);
    
extern bool GL_get_high_draw_thread_priority(void);
extern void GL_set_high_draw_thread_priority(bool onoff);

extern void GL_set_pause_rendering_on_off(bool onoff);
extern bool GL_get_pause_rendering_on_off(void);

#if 0  
extern void GL_lock(void);
extern void GL_unlock(void);
extern bool GL_maybeLock(void);

extern void GL_draw_lock(void);
extern void GL_draw_unlock(void);
#else
static inline void GL_lock(void){}
static inline void GL_unlock(void){}
static inline bool GL_maybeLock(void){return true;}

static inline void GL_draw_lock(void){}
static inline void GL_draw_unlock(void){}
#endif
	  
#include "../common/atomic.h"
    
extern DEFINE_ATOMIC(char *, GE_vendor_string);
//extern DEFINE_ATOMIC(char *, GE_renderer_string);
//extern DEFINE_ATOMIC(char *, GE_version_string);
//extern DEFINE_ATOMIC(uint32_t, GE_opengl_version_flags);

extern bool GL_using_nvidia_card(void);

extern void GE_set_curr_realline(int curr_realline);

#ifdef __cplusplus
  }
#endif

#endif // OPENGL_WIDGET_PROC_H
