
#ifndef _RADIUM_COMMON_SWINGTEXT_PROC_H
#define _RADIUM_COMMON_SWINGTEXT_PROC_H

extern LANGSPEC int SWINGTEXT_subsubtrack(struct Tracker_Windows *window, struct WTracks *wtrack);
extern LANGSPEC bool SWINGTEXT_keypress(struct Tracker_Windows *window, struct WBlocks *wblock, struct WTracks *wtrack, int realline, Place *place, int key); // wtrack must be NULL if window->curr_track < 0

#endif
