extern LANGSPEC data_as_text_t DAT_get_newvalue(int subsubtrack,
                                                int key,
                                                int default_value,
                                                int default_logtype,
                                                int min_value, int max_value,
                                                int min_return_value, int max_return_value,
                                                bool is_hex, bool has_logtype, bool highest_value_is_one_more);
extern LANGSPEC data_as_text_t DAT_get_overwrite(int old_value, int logtype, int subsubtrack, int key, int min_value, int max_value, int min_return_value, int max_return_value, bool is_hex, bool highest_value_is_one_more);
//extern LANGSPEC data_as_text_t DAT_get_newvalue(int subtrack, int key, int default_value, int min_value, int max_value, bool is_hex, bool has_logtype, bool highest_value_is_one_more);
//extern LANGSPEC data_as_text_t DAT_get_overwrite(int old_value, int old_logtype, int subtrack, int key, int min_value, int max_value, bool is_hex, bool highest_value_is_one_more);

extern LANGSPEC bool DAT_keypress(struct Tracker_Windows *window, const int key, const bool is_keydown);
