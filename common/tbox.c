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

#include "tbox_proc.h"



bool insideTBox(TBox *tbox,int x,int y){
	if(
		x < tbox->x1 ||
		x > tbox->x2 ||
		y < tbox->y1 ||
		y > tbox->y2
	)return false;

	return true;
}

/*
static bool TboxInsideTBox(TBox *tbox1,TBox *tbox2){
  if(
     tbox1->x1 >= tbox2->x1 &&
     tbox1->x2 <= tbox2->x2 &&
     tbox1->y1 >= tbox2->y1 &&
     tbox1->y2 <= tbox2->y2
     ) return true;

  return false;
}
*/

void TBOX_within(TBox *in,TBox *within){
  in->x1=R_BOUNDARIES(within->x1,in->x1,within->x2);
  in->x2=R_BOUNDARIES(within->x1,in->x2,within->x2);
  in->y1=R_BOUNDARIES(within->y1,in->y1,within->y2);
  in->y2=R_BOUNDARIES(within->y1,in->y2,within->y2);
}

#if 0
static bool TboxOverlapTBox(TBox *tbox1,TBox *tbox2){
  if(
     tbox1->x1 >= tbox2->x1 &&
     tbox1->x1 <= tbox2->x2
     ) return true;
  
  if(
     tbox1->y1 >= tbox2->y1 &&
     tbox1->y1 <= tbox2->y2
     ) return true;
  
  if(
     tbox1->x2 >= tbox2->x1 &&
     tbox1->x2 <= tbox2->x2
     ) return true;
  
  if(
     tbox1->y2 >= tbox2->y1 &&
     tbox1->y2 <= tbox2->y2
     ) return true;
  
  return false;
}
		     
static bool TBoxOnlyOverLapTBox_Y(TBox *tbox1,TBox *tbox2){
  if(
     tbox1->x1==tbox2->x1 &&
     tbox1->x2==tbox2->x2
     ){
    return true;
  }
  return false;
}

static bool TBoxOnlyOverLapTBox_X(TBox *tbox1,TBox *tbox2){
  if(
     tbox1->y1==tbox2->y1 &&
     tbox1->y2==tbox2->y2
     ){
    return true;
  }
  return false;
}

static void TBoxSquize_Y(TBox *from,TBox *to){
  to->y1=R_MIN(to->y1,from->y1);
  from->y2=R_MAX(to->y2,from->y2);
}

static void TBoxSquize_X(TBox *from,TBox *to){
  to->x1=R_MIN(to->x1,from->x1);
  from->x2=R_MAX(to->x2,from->x2);
}



static bool TBoxSquize(TBox *from,TBox *to){
  return false;
}



/* "from" and "overlap" overlaps, so we change "overlap" and
   make "to", to make 3 TBoxes that doesn't overlap.
*/

static void TBoxMake2Into3(TBox *from,TBox *overlap,TBox *to){
  
}

#endif
