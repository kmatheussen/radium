
#ifndef _RADIUM_API_API_TIMING_PROC_H
#define _RADIUM_API_API_TIMING_PROC_H

extern LANGSPEC dyn_t API_getAllBPM(const struct Blocks *block);
extern LANGSPEC dyn_t API_getAllLPB(const struct Blocks *block);
extern LANGSPEC dyn_t API_getAllTemponodes(const struct Blocks *blocks);
extern LANGSPEC dyn_t API_getAllBeats(const struct Beats *beats);
extern LANGSPEC dyn_t API_getAllBlockSwings(const struct Blocks *block);

#endif
