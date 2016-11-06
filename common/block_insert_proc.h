
#ifndef _RADIUM_COMMON_BLOCK_INSERT_PROC_H
#define _RADIUM_COMMON_BLOCK_INSERT_PROC_H

extern void InsertBlock_IncBlockNums(
	NInt blockpos
);

extern struct Blocks *InsertBlock(
                           NInt blockpos,
                           NInt num_tracks,
                           int num_lines,
                           char *name
                           );

extern struct Blocks *InsertBlock_CurrPos(
                                          struct Tracker_Windows *window
                                          );


#endif
