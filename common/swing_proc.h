
#ifndef _RADIUM_COMMON_SWING_PROC_H
#define _RADIUM_COMMON_SWING_PROC_H

#ifdef USE_QT4
QVector<Swing*> Swings_get(const struct WBlocks *wblock, const struct Tracks *track, int realline);
#endif

extern LANGSPEC void AddSwing(struct Blocks *block, struct Tracks *track,const Place place, int weight, int logtype);

extern LANGSPEC void RemoveSwing(struct Blocks *block,struct Tracks *track,struct Swing *swing);

extern LANGSPEC void RemoveSwingCurrPos(struct Tracker_Windows *window);

#endif
