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


extern void EraseLine(
                      struct Tracker_Windows *window,
                      struct WBlocks *wblock,
                      int x1, int x2,
                      int realline
                      );

extern void EraseLines(
                       struct Tracker_Windows *window,
                       struct WBlocks *wblock,
                       int x1, int x2,
                       int start_realline,
                       int end_realline
                       );

extern void EraseAllLines(
                          struct Tracker_Windows *window,
                          struct WBlocks *wblock,
                          int x1, int x2
                          );

extern void DrawWBlockSpesific(
	struct Tracker_Windows *window,
	struct WBlocks *wblock,
	int start_realline,
	int end_realline
);


#ifndef TRACKER_INCLUDE

extern void DrawUpLineNums(
	struct Tracker_Windows *window,
	struct WBlocks *wblock,
	int start_realline,
	int end_realline
);
extern void DrawTempos(
	struct Tracker_Windows *window,
	struct WBlocks *wblock,
	int start_realline,
	int end_realline
);
extern void DrawLPBs(
	struct Tracker_Windows *window,
	struct WBlocks *wblock,
	int start_realline,
	int end_realline
);

extern void DrawWTempoNodes(
	struct Tracker_Windows *window,
	struct WBlocks *wblock,
	int start_realline,
	int end_realline
);

extern void DrawUpWTempoNodes(
	struct Tracker_Windows *window,
	struct WBlocks *wblock
);

extern void DrawUpTempos(
	struct Tracker_Windows *window,
	struct WBlocks *wblock
);

extern void DrawUpLPBs(
	struct Tracker_Windows *window,
	struct WBlocks *wblock
);

extern void DrawWBlock(struct Tracker_Windows *window,struct WBlocks *wblock);

#endif

