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



/*******************************************************

  The Placement type is the type that defines a unique
  place for a note, velocity-node, tempo, lpb, fxs, etc.

  This file has some functions to make the use of it
  easier.

  Note: placement_proc.h contains some useful macros
        that are very much used in the program. But
        beware that some of them has to be treated
        carefully.

  Note2:
  to use either a float or an int instead
  of line,counter and dividor will not work as notes
  will be put before or after notes that they was not
  ment to be put before or after, and such.

*******************************************************/

#define RADIUM_PLACEMENTISCALLINGNOW

#include "nsmtracker.h"
#include <math.h>

#include "placement_proc.h"


Place *PlaceCreate(int line, int counter, int dividor) {
  Place *place=talloc(sizeof(Place));
  place->line = line;
  place->counter = counter;
  place->dividor = dividor;
  return place;
}

/*
Place *PlaceCopy(Place *p){
  Place *place=talloc(sizeof(Place));
  place->line = p->line;
  place->counter = p->counter;
  place->dividor = p->dividor;
  return place;
}
*/


/*************************************************************
  FUNCTION
    Returns 0 if placement1 is the same as placement2,
    -1 if placement1 is before placement2, and 1 if after.
  NOTE
    See also placement_proc.h for macros that makes use of this
    function.
*************************************************************/
int PlaceCmp(  Place *p1,  Place *p2){

	if(p1->line < p2->line) return -1;
	if(p1->line > p2->line) return 1;

	if(p1->counter*p2->dividor < p2->counter*p1->dividor) return -1;
	if(p1->counter*p2->dividor > p2->counter*p1->dividor) return 1;

	return 0;
}

/*************************************************************
  FUNCTION
    Returns the latest placement. If both placements are NULL,
    NULL is allso returned. (don't bother optimizing the
    obvious things, unless you really really have to, you
    think. :)
*************************************************************/
Place *PlaceMax(  Place *p1,  Place *p2){
	if(p1!=NULL){
		if(p2!=NULL){
			if(PlaceCmp(p1,p2) > 0){
				return p1;
			}else{
				return p2;
			}
		}else{
			return p1;
		}
	}else{
		if(p2!=NULL){
			return p2;
		}else{
			return NULL;
		}
	}

}


/*************************************************************
  FUNCTION
    Returns the first placement. If both placements are NULL,
    NULL is also returned. If only one placement is NULL,
    the other placement is returned. (don't bother optimizing the
    obvious things, unless you really really have to, you
    think. :)
*************************************************************/
Place *PlaceMin(  Place *p1,  Place *p2){

	if(p1!=NULL){
		if(p2!=NULL){
			if(PlaceCmp(p1,p2) > 0){
				return p2;
			}else{
				return p1;
			}
		}else{
			return p1;
		}
	}else{
		if(p2!=NULL){
			return p2;
		}else{
			return NULL;
		}
	}

}

/*************************************************************
  FUNCTION
    Sets the attributes of p1 to the ones in p2.
*************************************************************/
/*
void PlaceCopy(Place *p1,  Place *p2){
	p1->line=p2->line;
	p1->counter=p2->counter;
	p1->dividor=p2->dividor;
}
*/
/************************************************************
  FUNCTION
    Make shure that countor is not bigger or equal to dividor
    and that dividor is not bigger than MAX_UINT32.
************************************************************/
void PlaceHandleOverflow(Place *p){

	while(p->counter>=p->dividor){
		p->line++;
		p->counter-=p->dividor;
	}

	if(p->dividor>=MAX_UINT32){
		if(p->counter>=MAX_UINT32){
			p->counter=
				(uint_32)(
					(float)(
						(float)(p->counter)*
						(float)(MAX_UINT32)/
						(float)(p->dividor)
					)
					);
		}else{
			p->counter=(p->counter*MAX_UINT32)/p->dividor;
		}
		p->dividor=MAX_UINT32;
		if(p->counter>=p->dividor){
			fprintf(stderr,"Error in function PlaceHandleOverflow in file placement.c\n");
		}
	}

}


/*************************************************************
  FUNCTION
    Calculating stuff.
  NOTE
    If you want to handle placements that could be negative,
    these functions will not work. The solution is to convert
    the placement into floats, do the calculation, and
    convert the result back to a Place. (the result can not
    be negative either).
*************************************************************/
void PlaceAdd(Place *p1,  Place *p2){

	p1->line+=p2->line;
	p1->counter*=p2->dividor;
	p1->counter+=p2->counter*p1->dividor;
	p1->dividor*=p2->dividor;
	PlaceHandleOverflow(p1);
}
/*

1 + 1/5
4 + 2/3

5 + (2*5)+(1*3)/(3*5)
5 + 13/15

--
4 + 1/5
-1 - 2/3

3 + (
*/


/* NOTE, p1 must be bigger than p2 */

// p1: 22,1,8
// p2: 19,2,8
/*
void PlaceSub(Place *p1,  Place *p2){
	p1->line-=p2->line;								// p1->line=3
	p1->counter*=p2->dividor;						// p1->counter=8
	p1->counter-=p2->counter*p1->dividor;		// p1->counter=-8
	p1->dividor*=p2->dividor;						// p1->dividor=64
	if(p1->counter<0){
		p1->line--;										// p1->line=2
		p1->counter+=p1->dividor;					// p1->counter=A very big number
	}
	PlaceHandleOverflow(p1);
}
*/


void PlaceSub(Place *p1,  Place *p2){

	uint_32 temp=p2->counter*p1->dividor;		// temp=16

	p1->line-=p2->line;								// p1->line=3

	p1->counter*=p2->dividor;						// p1->counter=8
	p1->dividor*=p2->dividor;						// p1->dividor=64

	if(temp > p1->counter){
		p1->line--;										// p1->line=2
		p1->counter+=p1->dividor;					// p1->counter=72
	}

	p1->counter-=temp;								// p1->counter=56

	PlaceHandleOverflow(p1);
}

/*

4 + 2/3
1 + 1/5

3 + (2*5)-(1*3)/(3*5)
3 + 7/15

*/



/* I'm not too shure if the next two routines ever should be used.
   Handling overflow in these cases are maybe impossible. 
   They have never been tested either, and they have never been
   used anywhere. I guess the best solution for this
   is to convert the places to floats and do the calculating by
   floats instead. But there will still be overflow problems.
*/

void PlaceMul(Place *p1,  Place *p2){
	Place temp1,temp2;
	temp1.line=0;
	temp1.counter=p1->line*p2->line;
	temp1.dividor=1;

	temp2.line=0;

	temp2.counter=p1->line*p2->counter;
	temp2.dividor=p2->dividor;
	PlaceAdd(&temp1,&temp2);

	temp2.counter=p1->counter*p2->line;
	temp2.dividor=p1->dividor;
	PlaceAdd(&temp1,&temp2);

	temp2.counter=p1->counter*p2->counter;
	temp2.dividor=p1->dividor*p2->dividor;
	PlaceHandleOverflow(&temp2);
	PlaceAdd(&temp1,&temp2);

	PlaceCopy(p1,&temp1);
}

/*

4 + 2/3
1 + 1/5

14/3
6/5

6*14/15

p1->counter+=p1->line*p1->dividor
p1->line=0
p1->counter*=p2->line*p2->dividor
p1->dividor*=p2->dividor
*/

void PlaceDiv(Place *p1,  Place *p2){
	Place temp;
	temp.line=0;
	temp.counter=p2->dividor;
	temp.dividor=p1->counter+(p2->line*p2->dividor);
	PlaceHandleOverflow(&temp);

	PlaceMul(p1,&temp);
}

// most of the time, counter is 0. This is allso the procedure that is called most often.
/*
float GetfloatFromCounterDividor(uint_32 counter,uint_32 dividor){
	return counter==0 ? 0.0f :(float)((float)counter/(float)dividor);
}

float GetfloatFromLineCounterDividor(Place *placement){
	return (float)((float)(placement->line)+GetfloatFromCounterDividor(placement->counter,placement->dividor));
}

float GetfloatFromPlacement(Place *placement){
	return (float)((float)(placement->line)+GetfloatFromCounterDividor(placement->counter,placement->dividor));
}

float GetfloatFromPlace(Place *placement){
	return (float)((float)(placement->line)+GetfloatFromCounterDividor(placement->counter,placement->dividor));
}
*/

/**********************************************************
  FUNCTION
    Convert a float into a placement.
**********************************************************/
void Float2Placement(float f,Place *p){

	p->line=(int)f;

	f=f-(float)p->line;

	p->counter=f*MAX_UINT32;
	p->dividor=MAX_UINT32;

	PlaceHandleOverflow(p);		//Probably not necesarry
}

void PlaceAddfloat(Place *p,float f){
	float temp;
	temp=GetfloatFromPlacement(p);
	temp+=f;
	Float2Placement(temp,p);
}

void PlaceSubfloat(Place *p,float f){
	float temp;
	temp=GetfloatFromPlacement(p);
	temp-=f;
	Float2Placement(temp,p);
}


/**********************************************************
  FUNCTION
    Puts 'p' on the last legal position in a block.
**********************************************************/
/*
void PlaceSetLastPos(struct Blocks *block,Place *p){
	p->line=block->num_lines-1;
	p->counter=MAX_UINT32-1;
	p->dividor=MAX_UINT32;
}
*/

/**********************************************************
  FUNCTION
    Returns false if 'p' is below the last legal position
    in the block.
**********************************************************/
bool PlaceLegal(struct Blocks *block,  Place *p){
	Place temp;
	PlaceSetLastPos(block,&temp);
	if(PlaceGreaterThan(p,&temp)) return false;
	return true;
}

/**********************************************************
  FUNCTION
    Puts 'p' on the first legal position in a block.
**********************************************************/
/*
void PlaceSetFirstPos(Place *p){
	p->line=0;
	p->counter=0;
	p->dividor=1;
}
*/

/**********************************************************
  FUNCTION
    The same, only that it returns a global place allways
    set to first position, instead.
**********************************************************/
#ifdef SYSBASEDEBUG
Place PlaceFirstPos={0,0,0,1};
#else
Place PlaceFirstPos={0,0,1};
#endif
/*
Place *PlaceGetFirstPos(void){
	return &PlaceFirstPos;
}
*/

/**********************************************************
  FUNCTION
    Sets 'p' to the realline-placement. If realline
    is on the realline that is after the last, p will actually get
    an illegal placement-value. But it can still be
    used for calculation.
**********************************************************/
void PlaceSetReallinePlace(
	  struct WBlocks *wblock,
	int realline,
	Place *p
){
	if(realline>=wblock->num_reallines){
		if(realline>wblock->num_reallines){
			fprintf(stderr,"Error in function 'PlaceSetNextRealline' in file 'placement.c'\n");
		}
		p->line=wblock->num_reallines;
		p->counter=0;
		p->dividor=1;
	}else{
		PlaceCopy(p,&wblock->reallines[realline]->l.p);
	}
}


/**********************************************************
  FUNCTION
    Puts 'p' as near as possible 'tp' such that p<tp.
**********************************************************/
void PlaceTilLimit(Place *p, Place *tp){
	Place temp;

	PlaceCopy(&temp,tp);

	if(0==temp.counter){
		p->line=temp.line-1;
		p->counter=MAX_UINT32-1;
		p->dividor=MAX_UINT32;
	}else{
		p->line=temp.line;
		temp.counter*=MAX_UINT32;
		temp.dividor*=MAX_UINT32;
		PlaceHandleOverflow(&temp);
		p->counter=temp.counter-1;
		p->dividor=temp.dividor;
		if(0==PlaceCmp(p,&temp)){
			p->counter--;
		}
	}
}


/**********************************************************
  FUNCTION
    Puts 'p' as near as possible 'tp' such that p>tp.
**********************************************************/
void PlaceFromLimit(Place *p, Place *tp){
	Place temp;

	PlaceCopy(&temp,tp);

	p->line=temp.line;
	temp.counter*=MAX_UINT32;
	temp.dividor*=MAX_UINT32;
//	printf("temp->c: %u, temp->di: %u\n",temp.counter,temp.dividor);
	PlaceHandleOverflow(&temp);

	p->counter=temp.counter+1;
	p->dividor=temp.dividor;
//	printf("tp->c: %u, tp->di: %u\n",tp->counter,tp->dividor);
//	printf("c: %u, di: %u\n\n",p->counter,p->dividor);

	if(0==PlaceCmp(p,&temp)){
		p->line=tp->line+1;
		p->counter=0;
		p->dividor=1;
	};
}
















