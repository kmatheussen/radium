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


extern LANGSPEC void UpdateReallinesDependens(
	struct Tracker_Windows *window,
	struct WBlocks *wblock
);
extern LANGSPEC void UpdateRealLines(const struct Tracker_Windows *window,struct WBlocks *wblock);
//extern int FindHighestLocalzoomLevel(struct WBlocks *wblock);
extern LANGSPEC void SetZoomLevelAreaWidth(const struct Tracker_Windows *window,
                                           struct WBlocks *wblock);
/*
extern LANGSPEC void ExpandLine(
	struct Tracker_Windows *window,
	struct WBlocks *wblock,
	int realline,
	int num_newreallines
);
*/

extern LANGSPEC void ExpandLineCurrPos(
	struct Tracker_Windows *window,
	int num_newreallines
);
extern LANGSPEC void Unexpand(struct Tracker_Windows *window,struct WBlocks *wblock,int realline);
extern LANGSPEC void UnexpandCurrPos(struct Tracker_Windows *window);

extern LANGSPEC void Zoom(struct Tracker_Windows *window,struct WBlocks *wblock,int numtozoom);

extern LANGSPEC void LineZoomBlock(struct Tracker_Windows *window, struct WBlocks *wblock, int num_lines);
extern LANGSPEC void LineZoomBlockInc(struct Tracker_Windows *window, struct WBlocks *wblock, int inc_num_lines);
