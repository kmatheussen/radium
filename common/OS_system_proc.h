// operating system system

extern LANGSPEC void OS_SYSTEM_init_keyboard(void);
extern LANGSPEC void OS_SYSTEM_EventPreHandler(void *event);
extern LANGSPEC int OS_SYSTEM_get_event_type(void *event, bool ignore_autorepeat);

extern LANGSPEC int OS_SYSTEM_get_scancode(void *void_event);

extern LANGSPEC int OS_SYSTEM_get_modifier(void *void_event);
extern LANGSPEC int OS_SYSTEM_get_keynum(void *void_event);
extern LANGSPEC int OS_SYSTEM_get_qwerty_keynum(void *void_event); // e.g. using scancode.

//extern LANGSPEC bool OS_SYSTEM_KeyboardFilter(void *focused_widget, void *event);
extern LANGSPEC void OS_SYSTEM_ResetKeysUpDowns(void);
