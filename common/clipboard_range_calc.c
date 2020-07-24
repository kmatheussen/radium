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
    Returns true if the place is within the range of
    the wblock. (Does not check track)
********************************************************/
bool IsPlaceRanged(
                   const struct WBlocks *wblock,
                   const Place *p
){
  if(PlaceLessThan(p,&wblock->range.y1)) return false;
  if(PlaceGreaterOrEqual(p,&wblock->range.y2)) return false;
  
  return true;

}


/********************************************************
  FUNCTION
    Returns the length of the range represented as Place,
    into the place
********************************************************/
void GetRangePlaceLength(
                         Place *place,
                         const struct WBlocks *wblock
){
  *place = p_Sub(wblock->range.y2, wblock->range.y1);
}
