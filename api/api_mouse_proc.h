
#ifndef _RADIUM_API_API_MOUSE_PROC_H
#define _RADIUM_API_API_MOUSE_PROC_H


extern const struct ListHeader3 *current_node;
extern const struct ListHeader3 *indicator_node;
extern int indicator_velocity_num;
extern int indicator_pitch_num;


extern LANGSPEC void setCurrentNode(struct ListHeader3 *new_current_node);
extern LANGSPEC void setIndicatorNode(const struct ListHeader3 *new_indicator_node);
extern LANGSPEC void cancelIndicatorNode(void);

extern LANGSPEC float get_scroll_pos(void);

#endif
