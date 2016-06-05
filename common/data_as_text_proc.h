extern LANGSPEC data_as_text_t DAT_get_newvalue(int subtrack, int key, int default_value, int min_value, int max_value, bool highest_value_is_one_more);
extern LANGSPEC data_as_text_t DAT_get_overwrite(int old_value, int old_logtype, int subtrack, int key, int min_value, int max_value, bool is_hex, bool highest_value_is_one_more);

extern LANGSPEC bool DAT_keypress(struct Tracker_Windows *window, int key, bool is_keydown);
