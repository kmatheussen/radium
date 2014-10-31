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

extern LANGSPEC void MakeNodeLines(
	struct Tracker_Windows *window,
	struct WBlocks *wblock,
        struct WTracks *wtrack,
	Place *p1,
	Place *p2,
	float x1,float x2,
	float minx,float maxx,
	void *extrainfo,
	void (*ReturnNodeLineData)(
		struct Tracker_Windows *window,
		struct WBlocks *wblock,
                struct WTracks *wtrack,
		void *extrainfo,
		int firstlast,
		int realline,
		float u_y1,float u_y2,
		float u_x1,float u_x2
		)
	);

extern LANGSPEC void GetNodeLine(
		 struct TrackReallineElements *tre,
		 WArea *warea,
		 TBox *within,
		 TBox *ret
		 );
