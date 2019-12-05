/* Copyright 2000 Kjetil S. Matheussen

This program is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public License
as published by the Free Software Foundation; either version 2
of the License, or (at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA. */


#ifndef TRACKER_INCLUDE
#include "nsmtracker.h"

extern void SetTextNum(
	struct Tracker_Windows *window,
	int color,
	int num,
	int length,
	int x,
	int y,
	bool t
);

extern int SetTextNumLine(
	struct Tracker_Windows *window,
	struct WBlocks *wblocks,
	int color,
	int num,
	int length,
	int x,
	int realline,
	bool t
);

extern int SetTextLine(
	struct Tracker_Windows *window,
	struct WBlocks *wblock,
	int color,
	char *text,
	int x,
	int realline,
	bool t
);

extern int SetInvertTextLine(
	struct Tracker_Windows *window,
	struct WBlocks *wblock,
	int color,
	char *text,
	int x,
	int realline,
	bool t
);

int SetInvertTextLineNotext(
	struct Tracker_Windows *window,
	struct WBlocks *wblock,
	int color,
	int len,
	int x,
	int realline,
	bool t
	);


#endif
