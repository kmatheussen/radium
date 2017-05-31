
#ifndef _RADIUM_COMMON_SWINGTEXT_PROC_H
#define _RADIUM_COMMON_SWINGTEXT_PROC_H

extern LANGSPEC int SWINGTEXT_subsubtrack(const struct Tracker_Windows *window, const struct WTracks *wtrack);
extern LANGSPEC bool SWINGTEXT_keypress(struct Tracker_Windows *window, struct WBlocks *wblock, struct WTracks *wtrack, int realline, Place *place, int key); // wtrack must be NULL if window->curr_track < 0

#endif
