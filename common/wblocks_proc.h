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

extern void CloseWBlock(struct Tracker_Windows *window, NInt blocknum);
extern void CloseAllWBlocks(struct Tracker_Windows *window);

extern bool WBlock_legalizeStartEndReallines(struct WBlocks *wblock,int *start_realline,int *end_realline);

extern void UpdateWBlockCoordinates(
	struct Tracker_Windows *window,
	struct WBlocks *wblock
);

extern void SetWBlock_Top_And_Bot_Realline(
	struct Tracker_Windows *window,
	struct WBlocks *wblock
);

extern void UpdateAllWBlockCoordinates(
	struct Tracker_Windows *window
);

extern void UpdateAllWBlockWidths(struct Tracker_Windows *window);

extern void SelectWBlock(struct Tracker_Windows *window,struct WBlocks *wblock);

extern void SelectPrevWBlock(struct Tracker_Windows *window);

extern void SelectNextWBlock(struct Tracker_Windows *window);

extern void SelectPrevPlaylistWBlock(struct Tracker_Windows *window);
extern void SelectNextPlaylistWBlock(struct Tracker_Windows *window);

extern void NewWBlock(
	struct Tracker_Windows *window,
	struct WBlocks *wblock,
	struct Blocks *block
);

extern void UpdateWBlocks(struct Tracker_Windows *window);

extern void AppendWBlock(struct Tracker_Windows *window);

extern void AppendWBlock_spes(struct Tracker_Windows *window,int num_lines,NInt num_tracks);

#endif
