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




/*

	Handles various functions for pixmap handle.
	A "pixmap" is an invisible painting area,
	that can be copied into a visible area.

	Each Tracker_Windows struct has two pixmaps.
	One that is equally as big as the visible
	window, and one that contains a cursor-line.

	These two pixmaps should be reachable from
	the OS/hardware-spesific functions via the
	"os_visual" field.

	The main pixmap is never a direct copy of the
	visible window. To avoid having the program
	doing two bitmap-copies for each scroll, the
	"pixmapdefs" pointer field contains
	a pointer to an array of integers that defines
	which line in the tracker the respective
	integers maps to into the real window.
	Also, the cursor-line in the pixmap is
	a normal line, while in the window, it has
	a different background-color.

	The rest of the pixmap; track-headers, sliders,
	time, current octave, reltempo, etc. is
	a direct copy of the visible window,
	used to avoid flickering. Another reason is because
	first rendering to fast-mem and then bitblt
	it to chip-mem is often twice as fast in the Amiga hardware
	than rendering directly to chip. (chip-memory
	contains the internal gfx-memory on amigas).
	The cursor-pixmap is a direct copy of the
	cursor-line in the window, for the same reason.

*/



void PixMap_numVisibleLinesChange(
	struct Tracker_Windows *window,
	int num_lines
	);


void PixMap_isCleared(struct Tracker_Windows *window);


void PixMap_reset(struct Tracker_Windows *window);

int PixMap_getVisibleFromVisual(
	struct Tracker_Windows *window,
	int visualline
	);

int PixMap_getY1(
	struct Tracker_Windows *window,
	int visualline
	);

int PixMap_getY2(
	struct Tracker_Windows *window,
	int visualline
	);

void PixMap_markNotInUse(
	struct Tracker_Windows *window,
	int start_visible,
	int end_visible
	);

void PixMap_erase(
	struct Tracker_Windows *window,
	int start_visible,
	int end_visible
	);


void PixMap_makeNewDefs(
	struct Tracker_Windows *window,
	int start_visible,
	int end_visible
	);

void PixMap_scrollDefs(
	struct Tracker_Windows *window,
	int num_lines
	);

void PixMap_bltLines(
	struct Tracker_Windows *window,
	int startvisibleline,
	int endvisibleline,
	int x1,
	int x2
	);


