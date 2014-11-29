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


extern LANGSPEC float FindReallineForF(
                                       const struct WBlocks *wblock,
                                       float reallineF, // start search from here. Use 0 to search all.
                                       const Place *place
                                       );

extern int FindRealLineFor(
	struct WBlocks *wblock,
	int realline,
	const Place *place
);

extern int FindRealLineForNote(
	struct WBlocks *wblock,
	int realline,
	struct Notes *note
);

extern int FindRealLineForEndNote(
	struct WBlocks *wblock,
	int realline,
	struct Notes *note
);

extern int FindSubRealLine(
	struct Tracker_Windows *window,
	struct WBlocks *wblock,
	int realline,
	Place *p
);

static inline int get_realline_y1(const struct Tracker_Windows *window, int realline){
  return window->fontheight*realline;
}

static inline int get_realline_y2(const struct Tracker_Windows *window, int realline){
  return window->fontheight*(realline+1);
}

static inline float get_realline_y(const struct Tracker_Windows *window, float reallineF){
  return window->fontheight*reallineF;
}

