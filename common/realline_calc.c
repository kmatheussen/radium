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



float FindReallineForF(
                       const struct WBlocks *wblock,
                       float reallineF, // start search from here. Use 0 to search all.
                       const Place *place
                       )
{
  if (wblock->num_expand_lines<0)
    reallineF=0;

        int realline = reallineF;

        if (place->line >= wblock->block->num_lines || realline >= wblock->num_reallines)
          return wblock->num_reallines;

	const struct LocalZooms **reallines=wblock->reallines;

        if (wblock->num_expand_lines>0)
          if(place->line > realline)
            realline = place->line;

        // find next realline higher than place.
        {
          realline++;
        
          for(;;){
            if(realline>=wblock->num_reallines)
              break;
            if(PlaceGreaterOrEqual(&reallines[realline]->l.p, place))
              break;
            realline++;
          }
        }

        if (realline>wblock->num_reallines){
          RError("realline>wblock->num_reallines: %d > %d. place: %s",realline,wblock->num_reallines,PlaceToString(place));
          return wblock->num_reallines;
        }
        
        const Place *p1 = &reallines[realline-1]->l.p;
        const Place *p2 = realline==wblock->num_reallines ? NULL  : &reallines[realline]->l.p;

        float y=GetFloatFromPlace(place);
        float y1=GetFloatFromPlace(p1);
        float y2=p2==NULL ? wblock->block->num_lines : GetFloatFromPlace(p2);

	return scale(y,y1,y2,realline-1,realline);
}


/*******************************************************************
  FUNCTION
    Returns the realline the placement is placed on. The function start
    searching at realline 'realline', and the placement must be below
    or on that realline. The placement can not be below last realline
    in the wblock.
*******************************************************************/
int FindRealLineFor(
                    const struct WBlocks *wblock,
                    int realline,
                    const Place *place
){
  if (wblock->num_expand_lines<0)
    realline=0;

  const struct LocalZooms **reallines=wblock->reallines;

	if(realline>=wblock->num_reallines){
		RError("\n\nError In the function \"FindReallineFor\" in the file \"realline_calc.c\"\n"
                       "Input parameter 'realline' is below last realline.\n\n");
		return wblock->num_reallines-1;
	}

        if (wblock->num_expand_lines>0)
          if(place->line > realline)
            realline = place->line;

	for(;;realline++){
		if(realline>=wblock->num_reallines) break;
		if(
			PlaceLessThan(place,&reallines[realline]->l.p) &&
			PlaceGreaterOrEqual(place,&reallines[realline-1]->l.p)
		) break;
	}

        realline--;

        if (realline >= wblock->num_reallines){
          R_ASSERT(false);
          return wblock->num_reallines-1;
        }

	return realline;
}


int FindRealLineForNote(
                        const struct WBlocks *wblock,
                        int realline,
                        const struct Notes *note
){
	return FindRealLineFor(wblock,realline,&note->l.p);
}

int FindRealLineForEndNote(
                           const struct WBlocks *wblock,
                           int realline,
                           const struct Notes *note
){
	return FindRealLineFor(wblock,R_MAX(note->end.line,realline),&note->end);
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
                    const struct Tracker_Windows *window,
                    const struct WBlocks *wblock,
                    int realline,
                    const Place *p
){
	int line=p->line;
	uint_32 counter=p->counter;
	uint_32 dividor=p->dividor;

	const struct LocalZooms *firstlz=wblock->reallines[realline];
	const struct LocalZooms *nextlz;
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









