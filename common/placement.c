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

#include <math.h>

#include "nsmtracker.h"
#include "OS_string_proc.h"
#include "ratio_funcs.h"

#include "placement_proc.h"



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
    Make sure that countor is not bigger or equal to dividor
    and that dividor is not bigger than MAX_UINT32.
************************************************************/
void PlaceHandleOverflow(Place *p){

  *p = place_from_64(p->line, p->counter, p->dividor);
  
}


#if 0
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
void PlaceAdd(Place *p1,  const Place *p2){

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


void PlaceSub(Place *p1,  const Place *p2){

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



/* I'm not too sure if the next two routines ever should be used.
   Handling overflow in these cases are maybe impossible. 
   They have never been tested either, and they have never been
   used anywhere. I guess the best solution for this
   is to convert the places to floats and do the calculating by
   floats instead. But there will still be overflow problems.
*/

void PlaceMul(Place *p1,  const Place *p2){
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


void PlaceDiv(Place *p1,  const Place *p2){
	Place temp;
	temp.line=0;
	temp.counter=p2->dividor;
	temp.dividor=p1->counter+(p2->line*p2->dividor);
	PlaceHandleOverflow(&temp);

	PlaceMul(p1,&temp);
}
#endif


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
bool PlaceLegal(const struct Blocks *block,  const Place *p){
  R_ASSERT_RETURN_IF_FALSE2(p!=NULL, false);
  R_ASSERT_RETURN_IF_FALSE2(block!=NULL, false);

  if (p->line < 0 || p->counter >= p->dividor || p->dividor > MAX_UINT32)
    return false;
  
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
const Place PlaceFirstPos={0,0,0,1};
#else
const Place PlaceFirstPos={0,0,1};
#endif
/*
Place *PlaceGetFirstPos(void){
	return &PlaceFirstPos;
}
*/

const Place g_same_place = {-999999,999999,999999};

/**********************************************************
  FUNCTION
    Sets 'p' to the realline-placement. If realline
    is on the realline that is after the last, p will actually get
    an illegal placement-value. But it can still be
    used for calculation.
**********************************************************/
void PlaceSetReallinePlace(
                           const struct WBlocks *wblock,
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
void PlaceTilLimit(Place *p, const Place *tp){

  if(0==tp->counter){
    p->line=tp->line-1;
    p->counter=MAX_UINT32-1;
    p->dividor=MAX_UINT32;

    R_ASSERT(p->line>=0);

  }else{

    p->line=tp->line;
    
    uint32_t new_counter = scale_double(tp->counter, // first scale it up as much as possible
                                        0,tp->dividor,
                                        0,MAX_UINT32);
    new_counter--; // then subtract one.
    
    p->counter=new_counter;
    p->dividor=MAX_UINT32;
  }
}


/**********************************************************
  FUNCTION
    Puts 'p' as near as possible 'tp' such that p>tp.
**********************************************************/
void PlaceFromLimit(Place *p, const Place *tp){

  uint32_t new_counter = scale_double(tp->counter, // first scale it up as much as possible
                                      0,tp->dividor,
                                      0,MAX_UINT32);
  new_counter++; // then add one.

  if (new_counter==MAX_UINT32){
    p->line=tp->line+1;
    p->counter=0;
    p->dividor=1;
  } else {
    p->line=tp->line;
    p->counter=new_counter;
    p->dividor = MAX_UINT32;
  }
}

const char* PlaceToString(const Place *a){
  return talloc_format("%d + %d/%d",(a)->line,(a)->counter,(a)->dividor);
}

const char* p_ToString(const Place a){
  return PlaceToString(&a);
}

#ifndef TEST_PLACEMENT

Place p_FromString(const char* s){
  const wchar_t *w = STRING_create(s);


  const wchar_t *w_line = NULL;
  const wchar_t *w_ratio = NULL;
  const wchar_t *w_numerator = NULL;
  const wchar_t *w_denominator = NULL;
  
  int plus_pos = STRING_find_pos(w, 0, "+");

  if (plus_pos > 0){
    
    w_line = STRING_remove_end(w, plus_pos);

    if (plus_pos+1 <= STRING_length(w))
      w_ratio = STRING_remove_start(w, plus_pos+1);
      
  } else {

    w_ratio = w;    

  }
      
  if (w_ratio != NULL) {
    
    int divide_pos = STRING_find_pos(w_ratio, 0, "/");

    if (divide_pos > 0){

      w_numerator = STRING_remove_end(w_ratio, divide_pos);

      if (divide_pos+1 <= STRING_length(w_ratio))
        w_denominator = STRING_remove_start(w_ratio, divide_pos+1);
        
    } else {

      w_denominator = w_ratio;
      
    }
  }
    

  {
    int64_t line = 0;
    int64_t numerator = 0;
    int64_t denominator = 1;
    
    if (w_line != NULL)
      line = STRING_get_int64(w_line);

    if (w_numerator != NULL)
      numerator = STRING_get_int64(w_numerator);

    if (w_denominator != NULL)
      denominator = STRING_get_int64(w_denominator);

    if (denominator==0){
#if !defined(RELEASE)
      printf("-------- Div by zero. s: \"%s\"\n", s);
      getchar();
#endif
      return p_Create(0,0,1);
    }
    
    Ratio ratio = make_ratio(numerator + line*denominator, denominator);

    Place place = make_place_from_ratio(ratio);

    return place;
  }
}

#endif


#ifdef TEST_PLACEMENT

#if 0
gcc -Wall -Werror -DTEST_PLACEMENT -DDEBUG -DUSE_QT_REQTYPE=1 common/placement.c common/memory.c -IQt bin/packages/gc-7.2/.libs/libgc.a -lpthread && ./a.out
#endif
  
  double g_double_epsilon = 0.0000000001;
  
#include <stdarg.h>
#include <assert.h>

void EndProgram(void){
  printf("ENDPROGRAM called\n");
}

void CRASHREPORTER_send_assert_message(enum Crash_Type crash_type, const char *fmt,...){
  abort();
}

void RWarning_internal(const char *fmt,...){
  abort();
}

bool THREADING_is_main_thread(void){
  return true;
}

bool PLAYER_current_thread_has_lock(void){
  return false;
}

bool g_is_starting_up = false;

void RError_internal(const char *fmt,...){
  char message[1000];
  va_list argp;
  
  va_start(argp,fmt);
  /*	vfprintf(stderr,fmt,argp); */
  vsnprintf(message,998,fmt,argp);
  va_end(argp);

  fprintf(stderr,"error: %s\n",message);
}

#define VALIDATE(_p, _line, _counter, _dividor)  do{ \
    assert(_p->line==_line);                       \
    assert(_p->counter==_counter);                 \
    assert(_p->dividor==_dividor);                 \
  }while(0)

int main(void){

  Place *p = PlaceCreate(0,0,1);
  const Place *p1 = PlaceCreate(5,0,50);
  const Place *p2 = PlaceCreate(5,1,MAX_UINT32);
  const Place *p3 = PlaceCreate(5,MAX_UINT32-1,MAX_UINT32);

  Place *p1b = (Place*)p1;
  Place *p2b = (Place*)p2;
  Place *p3b = (Place*)p3;

  // Float2Placement
  //assert(Float2Placement(

  // compare and min/max
  assert(PlaceCmp(p1,p2)==-1);
  assert(PlaceCmp(p2,p1)==1);
  assert(PlaceCmp(p1,p1)==0);

  assert(p1==PlaceMin(p1b,p2b));
  assert(p2==PlaceMax(p1b,p2b));

  assert(p2==PlaceBetween(p1b,p2b,p3b));
  assert(p1==PlaceBetween(p1b,p1b,p3b));
  assert(p3==PlaceBetween(p1b,p3b,p3b));


  // PlaceTilLimit

  PlaceTilLimit(p, p1);
  VALIDATE(p,4,MAX_UINT32-1,MAX_UINT32);

  PlaceTilLimit(p, p2); 
  VALIDATE(p,5,0,MAX_UINT32);

  PlaceTilLimit(p, p3);
  VALIDATE(p,5,MAX_UINT32-2,MAX_UINT32);

  // PlaceFromLimit

  PlaceFromLimit(p, p1);
  VALIDATE(p,5,1,MAX_UINT32);

  PlaceFromLimit(p, p2); 
  VALIDATE(p,5,2,MAX_UINT32);

  PlaceFromLimit(p, p3); 
  assert(p->line==6);
  assert(p->counter==0);
  assert(p->dividor>0);


  // Check that the test placements haven't been modified during testing.
  assert(PlaceCmp(p1,p1b)==0);
  assert(PlaceCmp(p2,p2b)==0);
  assert(PlaceCmp(p3,p3b)==0);
  VALIDATE(p1b, 5,0,50);
  VALIDATE(p2b, 5,1,MAX_UINT32);
  VALIDATE(p3b, 5,MAX_UINT32-1,MAX_UINT32);

  printf("Success, no errors\n");
  return 0;
}

#endif // TEST_PLACEMENT
