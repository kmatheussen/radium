
#ifndef _RADIUM_COMMON_TRACK_INSERT_PROC_H
#define _RADIUM_COMMON_TRACK_INSERT_PROC_H

extern LANGSPEC void DeleteTracks(
                  struct Tracker_Windows *window,
                  struct WBlocks *wblock,
                  NInt tracknum,
                  NInt todelete
                  );

extern LANGSPEC void InsertTracks(
                         struct Tracker_Windows *window,
                         struct WBlocks *wblock,
                         NInt tracknum,
                         NInt toinsert
                         );

extern LANGSPEC void InsertTracks_CurrPos(
	struct Tracker_Windows *window,
	NInt toinsert
);

#endif
