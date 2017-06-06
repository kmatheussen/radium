
extern void GL_set_colored_tracks(bool onoff);
extern bool GL_get_colored_tracks(void);

extern DEFINE_ATOMIC(bool, g_is_creating_all_GL_blocks);

extern LANGSPEC void GL_create(const struct Tracker_Windows *window, struct WBlocks *wblock);
extern LANGSPEC void GL_create_all(const struct Tracker_Windows *window); // g_is_creating_all_GL_blocks is true while this function is working

