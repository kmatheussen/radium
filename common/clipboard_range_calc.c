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
#include "placement_proc.h"

#include "clipboard_range_calc_proc.h"




/********************************************************
  FUNCTION
    Returns the place where the range starts.
********************************************************/
__inline Place *getRangeStartPlace(
	struct WBlocks *wblock
){
	struct LocalZooms **reallines=wblock->reallines;
	if(
		wblock->rangey1>=wblock->num_reallines ||
		wblock->rangey1<0
	){
		RError(
			"Error. First rangeline is not legal.\n"
			"wblock->raggey1: %d, wblock->num_reallines: %d\n",
			wblock->rangey1,wblock->num_reallines
		);
		wblock->rangey1=0;
	}
	return &reallines[wblock->rangey1]->l.p;
}

/********************************************************
  FUNCTION
    Returns the place where the range ends.
********************************************************/
__inline Place *getRangeEndPlace(
	struct WBlocks *wblock
){
	Place *place;
	struct LocalZooms **reallines=wblock->reallines;
	if(
		wblock->rangey2>wblock->num_reallines ||
		wblock->rangey2<0
	){
		RError(
			"Error. Last rangeline is not legal.\n"
			"wblock->rangey2: %d, block->num_reallines: %d\n",
			wblock->rangey2,wblock->num_reallines
		);
		wblock->rangey2=wblock->num_reallines;
	}
	if(wblock->rangey2==wblock->num_reallines){
		place=talloc_atomic(sizeof(Place));
		PlaceSetLastPos(wblock->block,place);
		return place;
	}
	return &reallines[wblock->rangey2]->l.p;
}

/********************************************************
  FUNCTION
    Returns true if the place is within the range of
    the wblock. (Does not check track)
********************************************************/
bool isPlaceRanged(
	struct WBlocks *wblock,
	Place *p
){
	Place *p1= getRangeStartPlace(wblock);
	Place *p2= getRangeEndPlace(wblock);

	if(PlaceLessThan(p,p1)) return false;
	if(PlaceGreaterOrEqual(p,p2)) return false;

	return true;

}


/********************************************************
  FUNCTION
    Returns the length of the range represented as Place,
    into the place
********************************************************/
void getRangePlaceLength(
	Place *place,
	struct WBlocks *wblock
){
	Place *p1= getRangeStartPlace(wblock);
	Place *p2= getRangeEndPlace(wblock);

	PlaceCopy(place,p2);
	PlaceSub(place,p1);
}



