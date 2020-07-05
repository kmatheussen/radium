
#ifndef _RADIUM_API_API_MOUSE_PROC_H
#define _RADIUM_API_API_MOUSE_PROC_H


extern const struct ListHeader3 *g_current_node;
extern const struct ListHeader3 *g_indicator_node;
extern int g_indicator_velocity_num;
extern int g_indicator_pitch_num;


extern LANGSPEC void API_setCurrentNode(struct ListHeader3 *new_current_node);
extern LANGSPEC void API_setIndicatorNode(const struct ListHeader3 *new_indicator_node);

extern LANGSPEC float get_scroll_pos(void);

#endif
