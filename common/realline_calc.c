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

#include "realline_calc_proc.h"




/*******************************************************************
  FUNCTION
    Returns the realline the placement is placed on. The function start
    searching at realline 'realline', and the placement must be below
    or on that realline. The placement can not be below last realline
    in the wblock.
*******************************************************************/
int FindRealLineFor(
	struct WBlocks *wblock,
	int realline,
	Place *place
){
	struct LocalZooms **reallines=wblock->reallines;

	if(realline>=wblock->num_reallines){
		RError("\n\nError In the function \"FindReallineFor\" in the file \"realline_calc.c\"\n");
		RError("Input parameter 'realline' is below last realline.\n\n");
		return wblock->num_reallines-1;
	}

	for(realline=max(realline,place->line);;realline++){
		if(realline>=wblock->num_reallines) break;
		if(
			PlaceLessThan(place,&reallines[realline]->l.p) &&
			PlaceGreaterOrEqual(place,&reallines[realline-1]->l.p)
		) break;
	}

	return realline-1;
}


int FindRealLineForNote(
	struct WBlocks *wblock,
	int realline,
	struct Notes *note
){
	return FindRealLineFor(wblock,realline,&note->l.p);
}

int FindRealLineForEndNote(
	struct WBlocks *wblock,
	int realline,
	struct Notes *note
){
	return FindRealLineFor(wblock,max(note->end.line,realline),&note->end);
}


/************************************************************************
  FUNCTION
    Returns the realline that the placent belongs too. The difference
    between this one and 'FindRealLineFor' is that this one searches backwards.
************************************************************************/
/*
int FindRealLineForB(
	struct WBlocks *wblock,
	int realline,
	Place *place
){
	int realline=
}
*/


/***********************************************************************
  FUNCTION
    Return the relative y position where counter and dividor is placed,
    for the realline.
  HOW IT WORKS
    The input parameter 'realline' is the realline for the placement.
    The variable 'nextplacement' gets the value of the next realline
    ('realline'+1). So, by dividing (placement-realline) by
    (realline+1-realline), and multiply that value with the height of
    the windows font, we get the correct answer.
***********************************************************************/
int FindSubRealLine(
	struct Tracker_Windows *window,
	struct WBlocks *wblock,
	int realline,
	Place *p
){
	int line=p->line;
	uint_32 counter=p->counter;
	uint_32 dividor=p->dividor;

	struct LocalZooms *firstlz=wblock->reallines[realline];
	struct LocalZooms *nextlz;
	uint_32 next_counter;
	uint_32 next_dividor;
	float placement=GetfloatFromCounterDividor(counter,dividor);
	float thisplacement=GetfloatFromCounterDividor(firstlz->Tcounter,firstlz->Tdividor);
	float nextplacement;
	int ret;

	if(realline+1>=wblock->num_reallines){
		nextplacement=1.0;
	}else{
		nextlz=wblock->reallines[realline+1];
		if(nextlz->Tline>line){
			nextplacement=1.0;
		}else{
			next_counter=nextlz->Tcounter;
			next_dividor=nextlz->Tdividor;
			nextplacement=GetfloatFromCounterDividor(next_counter,next_dividor);
		}
	}

	ret= (int) (
		((float)(window->fontheight) * 
		(placement-thisplacement))/(nextplacement-thisplacement)
	);
	return ret;
}


//					next_counter=(2*firstlz->Tcounter) - wblock->reallines[realline-1]->Tcounter;
//					nextplacement=GetfloatFromCounterDividor(next_counter,firstlz->Tdividor);









