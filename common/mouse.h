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


#ifndef TRACKER_MOUSE_H
#define TRACKER_MOUSE_H

#include "nsmtracker.h"


struct MouseAction{
	int action;
	int eint1;
	int eint2;
	int eint3;
	void *pointer1;
	void *pointer2;
	void *pointer3;
	void *pointer4;
	int (*MouseUpFunction)(
		struct Tracker_Windows *window,
		int x,int y
	);
};


// MouseAction actions:

#define NOACTION 0

#define WINDOWRESIZE 1

#define LEFTSLIDER 2
#define BOTTOMSLIDER 3

#define ZOOM 4

#define TRACKNAME 5
#define INSTRUMENTNAME 6

#define TEMPOLINE 7
#define TEMPONODE 8

#define NOTESTART 9
#define NOTEEND 10
#define VELLINE 11
#define VELNODE 12

#define FXNODE 13
#define FXLINE 14

#define NOTE 15

#define LEVELBORDER 16
#define LINEBORDER 17
#define LPBBORDER 18
#define TEMPOBORDER 19
#define TEMPONODEBORDER 20
#define FXBORDER 21
#define TRACKBORDER 22

#define VOLUMESLIDER 23
#define PANSLIDER 24

#define MAINBPM 25
#define MAINLPB 26
#define RELTEMPO 27

#define RELTEMPOSLIDER 28

#endif /* TRACKER_MOUSE_H */





