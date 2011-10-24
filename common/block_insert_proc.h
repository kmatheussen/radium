
#ifndef TRACKER_INCLUDE

extern void InsertBlock_IncBlockNums(
	NInt blockpos
);

void InsertBlock(
	NInt blockpos,
	NInt num_tracks,
	int num_lines,
	char *name
);

extern void InsertBlock_CurrPos(
	struct Tracker_Windows *window
);

#endif


