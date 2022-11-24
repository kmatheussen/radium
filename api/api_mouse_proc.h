
#pragma once


extern LANGSPEC void API_setCurrentNode(struct ListHeader3 *new_current_node);
extern LANGSPEC void API_setCurrentNode2(int64_t id);

extern LANGSPEC void API_setIndicatorNode(const struct ListHeader3 *new_indicator_node);
extern LANGSPEC void API_setIndicatorNode2(int64_t id);

extern LANGSPEC float get_scroll_pos(void);
