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


extern LANGSPEC void CloseWBlock(struct Tracker_Windows *window, NInt blocknum);
extern LANGSPEC void CloseAllWBlocks(struct Tracker_Windows *window);

extern LANGSPEC bool WBlock_legalizeStartEndReallines(const struct WBlocks *wblock,int *start_realline,int *end_realline);

extern LANGSPEC void UpdateWBlockCoordinates(
	struct Tracker_Windows *window,
	struct WBlocks *wblock
);

extern LANGSPEC void SetWBlock_Top_And_Bot_Realline(
	struct Tracker_Windows *window,
	struct WBlocks *wblock
);

extern LANGSPEC void UpdateAllWBlockCoordinates(
	struct Tracker_Windows *window
);

extern LANGSPEC void UpdateWBlockWidths(struct Tracker_Windows *window,struct WBlocks *wblock);
extern LANGSPEC void UpdateAllWBlockWidths(struct Tracker_Windows *window);

extern LANGSPEC void SelectWBlock(struct Tracker_Windows *window,struct WBlocks *wblock);

extern LANGSPEC void SelectPrevWBlock(struct Tracker_Windows *window);

extern LANGSPEC void SelectNextWBlock(struct Tracker_Windows *window);

extern LANGSPEC void SelectPrevPlaylistWBlock(struct Tracker_Windows *window);
extern LANGSPEC void SelectNextPlaylistWBlock(struct Tracker_Windows *window);

extern LANGSPEC void NewWBlock(
	struct Tracker_Windows *window,
	struct WBlocks *wblock,
	struct Blocks *block
);

extern LANGSPEC void UpdateWBlocks(struct Tracker_Windows *window);

extern LANGSPEC void AppendWBlock(struct Tracker_Windows *window);

extern LANGSPEC void AppendWBlock_spes(struct Tracker_Windows *window,int num_lines,NInt num_tracks);
