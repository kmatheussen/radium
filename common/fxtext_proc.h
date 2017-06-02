
// Returns a pointer to AN ARRAY of vectors (one vector for each realline), not a pointer to a vector (as one would think).
//extern LANGSPEC vector_t *FXTEXTS_get(const struct WBlocks *wblock, const struct WTracks *wtrack, const struct FXs *fxs);

#if USE_QT4
const FXText_trss FXTEXTS_get(const struct WBlocks *wblock, const struct WTracks *wtrack, const struct FXs *fxs);
#endif

extern LANGSPEC int FXTEXT_subsubtrack(const struct Tracker_Windows *window, const struct WTracks *wtrack, struct FXs **to_fxs);
extern LANGSPEC bool FXTEXT_keypress(struct Tracker_Windows *window, struct WBlocks *wblock, struct WTracks *wtrack, int realline, const Place *place, int key);

