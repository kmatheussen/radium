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




#include <X11/Xlib.h>

#define NUM_COLORS 8
#define NUM_XPOINTS 4096

struct OS_visual{
  Window window;
  GC *gcs; // Pointer to array.

  Pixmap pixmap;
  Pixmap cursorpixmap;

  XFontStruct *xfontstruct;

  int num_points[NUM_COLORS];
  XPoint *xpoints[NUM_COLORS];
};


typedef int* ReqType;
