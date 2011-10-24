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

extern void ScrollEditorDown(struct Tracker_Windows *window,int num_lines);

extern void ScrollEditorUp(struct Tracker_Windows *window,int num_lines);

extern void ScrollEditorNextNote(struct Tracker_Windows *window);
extern void ScrollEditorPrevNote(struct Tracker_Windows *window);

void ScrollEditorToRealLine(
	struct Tracker_Windows *window,
	struct WBlocks *wblock,
	int till_curr_realline
);

void ScrollEditorToRealLine_CurrPos(
	struct Tracker_Windows *window,
	int till_curr_realline
);

void ScrollEditor(
	struct Tracker_Windows *window,
	int num_reallines
);

void ScrollEditorToLine_CurrPos(
	struct Tracker_Windows *window,
	int line
);

void ScrollEditorToPercentLine_CurrPos(
	struct Tracker_Windows *window,
	int percent
);

#endif

