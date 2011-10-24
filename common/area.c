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


#include "nsmtracker.h"

#include "area_proc.h"


bool insideArea(Area *area,int x){
	if(
		x<area->x ||
		x>area->x2
	)return false;

	return true;
}

bool insideYArea(YArea *yarea,int y){
	if(
		y<yarea->y ||
		y>yarea->y2
	)return false;

	return true;

}

bool insideWArea(WArea *warea,int x){
	if(
		x<warea->x ||
		x>warea->x2
	)return false;

	return true;
}

bool insideNArea(
	int x,int y,
	int sx,int sy,
	int bx1,int by1,
	int bx2,int by2
){
	bx1+=sx;bx2+=sx;
	by1+=sy;by2+=sy;
	if(x<bx1) return false;
	if(x>bx2) return false;
	if(y<by1) return false;
	if(y>by2) return false;

	return true;
}

