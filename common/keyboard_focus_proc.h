#ifndef RADIUM_COMMON_KEYBOARD_FOCUS_PROC_H
#define RADIUM_COMMON_KEYBOARD_FOCUS_PROC_H

#  ifdef __cplusplus
extern "C"{
#endif

extern bool g_do_grey_editor;

extern bool editor_has_keyboard_focus(void);

extern void obtain_keyboard_focus_without_greying(void);
extern void obtain_keyboard_focus(void);
extern void release_keyboard_focus(void);
extern void obtain_keyboard_focus_counting(void); // careful...
extern void release_keyboard_focus_counting(void);
  
extern void call_me_if_another_window_may_have_taken_focus_but_still_need_our_key_events(void);
extern bool main_window_has_focus(void);
  
#  ifdef __cplusplus
}
#endif
  

#endif

