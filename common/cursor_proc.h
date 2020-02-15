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


extern LANGSPEC bool EDITOR_is_legal_track(const struct Tracker_Windows *window, int tracknum);

extern LANGSPEC int CursorRight(struct Tracker_Windows *window,struct WBlocks *wblock);
extern LANGSPEC int CursorLeft(struct Tracker_Windows *window,struct WBlocks *wblock);

extern LANGSPEC void R_SetCursorPos(struct Tracker_Windows *window);
extern LANGSPEC void CursorRight_CurrPos(struct Tracker_Windows *window);
extern LANGSPEC void CursorLeft_CurrPos(struct Tracker_Windows *window);

extern LANGSPEC void CursorNextTrack_CurrPos(struct Tracker_Windows *window);
extern LANGSPEC void CursorPrevTrack_CurrPos(struct Tracker_Windows *window);

extern LANGSPEC bool SetCursorPosConcrete(
	struct Tracker_Windows *window,
	struct WBlocks *wblock,
	NInt tracknum,
	int subtrack
);
extern LANGSPEC void SetCursorPosConcrete_CurrPos(
	struct Tracker_Windows *window,
	NInt tracknum
);
