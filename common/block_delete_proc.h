
#ifndef _RADIUM_COMMON_BLOCK_DELETE_PROC_H
#define _RADIUM_COMMON_BLOCK_DELETE_PROC_H

extern LANGSPEC void DeleteBlock(
	NInt blockpos
);

extern LANGSPEC void DeleteBlock_CurrPos(
                                         struct Tracker_Windows *window,
                                         int blockpos
);

#endif


