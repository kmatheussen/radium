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


//#include <intuition/intuition.h>
//Following code is to speed up the compilation-time:
#ifndef INTUITION_INTUITION_H
struct Screen;
struct Window;
#endif

#define DWIDTH 6
#define DHEIGHT 4

struct OS_visual{
	struct Screen *screen;
	struct Window *window;
	int xpluss,ypluss;
	int dowaitfor_IDCMP_NEWSIZE;

	struct RastPort *RPort;

	struct RastPort *CRPort;
};


// #include <dos/dos.h>
// Following ripeed from dos/dos.h : (to speed up compilation time)

#ifndef DOS_DOS_H
typedef long  BPTR;		    /* Long word pointer */
#endif

typedef BPTR ReqType;
