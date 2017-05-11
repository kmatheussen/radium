// operating system system

extern LANGSPEC void OS_OSX_show_icon_in_dock(void);

extern LANGSPEC void OS_SYSTEM_init_keyboard(void);
extern LANGSPEC void OS_SYSTEM_EventPreHandler(void *event);
extern LANGSPEC int OS_SYSTEM_get_event_type(void *event, bool ignore_autorepeat);

extern LANGSPEC int OS_SYSTEM_get_scancode(void *void_event);

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


//extern LANGSPEC bool OS_SYSTEM_KeyboardFilter(void *focused_widget, void *event);
extern LANGSPEC void OS_SYSTEM_ResetKeysUpDowns(void);
