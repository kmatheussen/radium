
extern const struct ListHeader3 *g_current_node;
extern const struct ListHeader3 *g_indicator_node;
extern int g_indicator_velocity_num;
extern int g_indicator_pitch_num;

extern int64_t g_current_node_id;
extern int64_t g_indicator_node_id;

extern int g_current_bar_num;
extern int g_current_beat_num;
extern int g_current_barbeat_block_num;

extern void GL_set_colored_tracks(bool onoff);
extern bool GL_get_colored_tracks(void);

extern DEFINE_ATOMIC(bool, g_is_creating_all_GL_blocks);

extern LANGSPEC void GL_create(const struct Tracker_Windows *window);
extern LANGSPEC void GL_create_all(const struct Tracker_Windows *window); // g_is_creating_all_GL_blocks is true while this function is working

