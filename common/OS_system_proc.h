// operating system system

extern LANGSPEC void OS_OSX_show_icon_in_dock(void);

extern LANGSPEC void OS_SYSTEM_init_keyboard(void);
extern LANGSPEC void OS_SYSTEM_EventPreHandler(void *event);
extern LANGSPEC int OS_SYSTEM_get_event_type(void *event, bool ignore_autorepeat);

extern LANGSPEC int OS_SYSTEM_get_scancode(void *void_event);

extern LANGSPEC void OS_OSX_set_cursorpos(int x, int y);
extern LANGSPEC void OS_OSX_clear_modifiers(void);

extern LANGSPEC int OS_SYSTEM_get_modifier(void *void_event);
extern LANGSPEC int OS_SYSTEM_get_keynum(void *void_event);
extern LANGSPEC int OS_SYSTEM_get_qwerty_keynum(void *void_event); // e.g. using scancode.

extern LANGSPEC int OS_SYSTEM_get_keynum2(uint32_t ulong_keysum, bool keypad_pressed);
extern LANGSPEC int OS_SYSTEM_get_qwerty_keynum2(uint32_t scancode);

//extern LANGSPEC bool OS_SYSTEM_window_is_actually_visible(void *win); // Returns true if actually possibly to view by a human when looking at a screen.

extern LANGSPEC void OS_WINDOWS_set_key_window(void *win);

extern LANGSPEC bool OS_OSX_is_key_window(void *void_nsview);
extern LANGSPEC bool OS_WINDOWS_is_key_window(void *maybewin);

extern LANGSPEC void OS_WINDOWS_set_always_on_top(void *child_handle);
//extern LANGSPEC void OS_WINDOWS_set_on_top_of_everything(void *child_handle);

#ifdef __cplusplus
void OS_GFX_close_embedded_native_window(void *daswidget);
void *OS_GFX_create_embedded_native_window(void *child_handle, int x, int y, int width, int height, std::function<void(void*)> delete_child_handle_func);
#endif

extern LANGSPEC int OS_GFX_get_num_toplevel_windows(void);
extern LANGSPEC void *OS_GFX_get_native_toplevel_window(int toplevelwindow_num);
extern LANGSPEC void *OS_GFX_get_mixer_toplevel_window_if_visible(void);

extern LANGSPEC uint32_t OS_SYSTEM_add_mouse_keyswitches(uint32_t keyswitch);
  
//extern LANGSPEC bool OS_SYSTEM_KeyboardFilter(void *focused_widget, void *event);
extern LANGSPEC void OS_SYSTEM_ResetKeysUpDowns(void);
