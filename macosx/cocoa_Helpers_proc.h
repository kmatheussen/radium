#pragma once

extern LANGSPEC void OS_OSX_show_icon_in_dock(void);

extern LANGSPEC void OS_OSX_set_cursorpos(int x, int y);

extern LANGSPEC bool OS_OSX_is_key_window(void *void_nsview);
//extern LANGSPEC void OS_WINDOWS_set_key_window(void *win);


extern LANGSPEC void OS_OSX_show_very_fullscreen(void *void_nsview);
extern LANGSPEC void OS_OSX_unshow_very_fullscreen(void *void_nsview);
extern LANGSPEC bool OS_OSX_window_is_fullscreen(void *void_nsview);

// Reroute the native macOS fullscreen operation (green button) to the
// home-made fullscreen used by F11.
extern LANGSPEC void OS_OSX_install_fullscreen_button_redirect(void *void_nsview);
extern LANGSPEC void OS_OSX_toggle_main_window_fullscreen(void);

bool OS_OSX_has_notch(void *void_nsview);
#ifdef __cplusplus
// Returns false if there is no notch.
bool OS_OSX_get_notch_rect(void *void_nsview, int &x1, int &y1, int &x2, int &y2);
#endif
