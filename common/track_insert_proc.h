
void DeleteTracks(
                  struct Tracker_Windows *window,
                  struct WBlocks *wblock,
                  NInt tracknum,
                  NInt todelete
                  );

extern void InsertTracks(
                         struct Tracker_Windows *window,
                         struct WBlocks *wblock,
                         NInt tracknum,
                         NInt toinsert
                         );

extern void InsertTracks_CurrPos(
	struct Tracker_Windows *window,
	NInt toinsert
);
