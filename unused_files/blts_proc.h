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

extern LANGSPEC void Blt_blt(struct Tracker_Windows *window);

extern LANGSPEC void Blt_mark(
	      struct Tracker_Windows *window,
	      int startrealline,int endrealline,
	      int x1,int x2
	      );

extern LANGSPEC void Blt_marktrack(
	struct Tracker_Windows *window,
	NInt starttrack,
	NInt endtrack,
	bool starttrack_onlyfxarea,
	int startrealline,
	int endrealline
);

extern LANGSPEC void  Blt_markTrack(
			struct Tracker_Windows *window,
			NInt tracknum
			);

extern LANGSPEC void Blt_marktrackheader(
	struct Tracker_Windows *window,
	NInt starttrack,
	NInt endtrack
	);

extern LANGSPEC void  Blt_scrollMark(struct Tracker_Windows *window);

extern LANGSPEC void Blt_markAll(
		struct Tracker_Windows *window,
		NInt starttrack,
		NInt endtrack,
		int startrealline,
		int endrealline
		);

extern LANGSPEC void  Blt_markSTrack(
			struct Tracker_Windows *window,
			NInt tracknum,
			int startrealline,
			int endrealline
			);

extern LANGSPEC void  Blt_markFxTrack(
			struct Tracker_Windows *window,
			NInt tracknum
			);

extern LANGSPEC void  Blt_markCurrTrack(
		       struct Tracker_Windows *window,
		       int startrealline,
		       int endrealline
		       );

extern LANGSPEC void Blt_markCurrFxTrack(
		       struct Tracker_Windows *window,
		       int startrealline,
		       int endrealline
		       );

extern LANGSPEC void Blt_clearNotUsedVisible(struct Tracker_Windows *window);

extern LANGSPEC void Blt_markVisible(struct Tracker_Windows *window);

extern LANGSPEC void Blt_unMarkVisible(struct Tracker_Windows *window);
