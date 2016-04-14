
extern LANGSPEC vector_t *VELTEXTS_get(const struct WBlocks *wblock, const struct WTracks *wtrack);

extern LANGSPEC int VELTEXT_subsubtrack(struct Tracker_Windows *window, struct WTracks *wtrack);
extern LANGSPEC bool VELTEXT_keypress(struct Tracker_Windows *window, struct WBlocks *wblock, struct WTracks *wtrack, int realline, Place *place, int key);
  
