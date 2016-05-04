#ifndef RADIUM_COMMON_KEYBOARD_FOCUS_PROC_H
#define RADIUM_COMMON_KEYBOARD_FOCUS_PROC_H

#  ifdef __cplusplus
extern "C"{
#endif
  
extern void obtain_keyboard_focus(void);
extern void release_keyboard_focus(void);
extern bool editor_has_keyboard_focus(void);
  
#  ifdef __cplusplus
}
#endif
  

#endif

