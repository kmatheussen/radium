
#ifndef _RADIUM_COMMON_BLOCK_INSERT_PROC_H
#define _RADIUM_COMMON_BLOCK_INSERT_PROC_H

extern LANGSPEC void InsertBlock_IncBlockNums(
	NInt blockpos
);

extern LANGSPEC struct Blocks *InsertBlock(
                           NInt blockpos,
                           NInt num_tracks,
                           int num_lines,
                           const char *name
                           );

extern LANGSPEC struct Blocks *InsertBlock_CurrPos(
                                          struct Tracker_Windows *window
                                          );


#endif
