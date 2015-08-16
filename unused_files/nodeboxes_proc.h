/* Copyright 2001 Kjetil S. Matheussen

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


#if 0
extern void MakeBlackBox(
	struct Tracker_Windows *window,
	int u_x,
	int u_y,
	int width,
	struct TrackReallineElements *tre
);
#endif

int GetNodeSize(struct Tracker_Windows *window, struct WBlocks *wblock, struct WTracks *wtrack);

void GetNodeBox_customsize(
		struct TrackReallineElements *tre,
		WArea *warea,
		TBox *within,
		TBox *ret,
		int boxwidth,
		int boxheight
		);

void GetNodeBox(
		struct Tracker_Windows *window,
                struct WBlocks *wblock,
                struct WTracks *wtrack,
		struct TrackReallineElements *tre,
		WArea *warea,
		TBox *within,
		TBox *ret
		);

bool isInsideNodeBox(
		 struct TrackReallineElements *tre,
		 WArea *warea,
		 TBox *within,
		 int x,int y
		 );

