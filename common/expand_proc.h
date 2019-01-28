extern LANGSPEC void EXPAND_Block(struct Tracker_Windows *window, struct WBlocks *wblock, const Place start, const Place end, const Place new_end);
extern LANGSPEC void EXPAND_Block2(struct Tracker_Windows *window, struct WBlocks *wblock, int new_num_lines);
extern LANGSPEC void EXPAND_Block_full_control_CurrPos(struct Tracker_Windows *window, struct WBlocks *wblock, const Place start, const Place end, const Place new_end);
extern LANGSPEC void EXPAND_Block_CurrPos(struct Tracker_Windows *window, struct WBlocks *wblock, int num_lines);
extern LANGSPEC void EXPAND_Block_from_range_CurrPos(struct Tracker_Windows *window, struct WBlocks *wblock, const Place range_duration_after);
