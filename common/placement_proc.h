/* Copyright 2000 Kjetil S. Matheussen

This program is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public License
as published by the Free Software Foundation; either version 2
of the License, or (at your option) any later version.
-
This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA. */

#ifndef COMMON_PLACEMENT_PROC_H
#define COMMON_PLACEMENT_PROC_H

#include "overflow_funcs.h"

static inline Place p_Create(int line, int counter, int dividor) {
  // Uncomment here. p_Create() is used more for calculation

  R_ASSERT_NON_RELEASE(line >= 0);
  R_ASSERT_NON_RELEASE(counter >= 0);
  //R_ASSERT_NON_RELEASE(counter < dividor); (we use place for signatures as well.)
  R_ASSERT_NON_RELEASE(dividor > 0);
  R_ASSERT_NON_RELEASE(dividor <= MAX_UINT32);

  if(dividor==0){
    R_ASSERT(false);
    dividor=1;
  }

  Place place;
  place.line = line;
  place.counter = (uint32_t)counter;
  place.dividor = (uint32_t)dividor;
  return place;
}


extern const Place g_same_place;

static inline bool p_is_same_place(Place place){
  return
    place.line==g_same_place.line &&
    place.counter==g_same_place.counter &&
    place.dividor==g_same_place.dividor;
}

// todo: Delete this function. 'place' is usually used as a variable name.
/*
static inline Place p_Create(int line, int counter, int dividor) {
  return p_Create(line, counter, dividor);
}
*/

static inline void ValidatePlace(const Place *place){
  if (place==NULL)
    return;

  R_ASSERT(place->line >= 0);
  //R_ASSERT(place->counter >= 0); TODO! Put back this test when counter is signed.
  R_ASSERT(place->counter < place->dividor);
  //R_ASSERT(place->dividor > 0); TODO! Put back this test when dividor is signed.
  R_ASSERT(place->dividor != 0);
  R_ASSERT(place->dividor <= MAX_UINT32);
}

static inline Place place_from_64(int64_t line, int64_t num, int64_t den){

  R_ASSERT(den > 0);

  while(num > den) {
    line++;
    num -= den;
  }

  while(num < 0){
    line--;
    num += den;
  }

  R_ASSERT(line >= 0);
  R_ASSERT(line < INT_MAX);

  Place ret;
  
  if (den > MAX_UINT32) {
    int counter = (int)scale_double((double)num, 0, (double)den, 0, MAX_UINT32);
    int dividor = MAX_UINT32;

    if (counter >= dividor)
      ret = p_Create((int)line+1, 0, 1);
    else
      ret = p_Create((int)line, counter, dividor);
    
  } else {

    R_ASSERT(num < INT_MAX);
      
    ret = p_Create((int)line, (int)num, (int)den);
    
  }

  ValidatePlace(&ret);
  
  return ret;
}

static inline Place place_from_64b(int64_t num, int64_t den){
  int64_t lines = num/den;

  return place_from_64(lines, num - (lines*den), den);
}

static inline Place DYN_get_place(const dyn_t dyn){
  Ratio ratio = DYN_get_ratio(dyn);
  return place_from_64b(ratio.num, ratio.den);
}


#include "overflow_funcs.h"


static inline Ratio ratio_from_place(const Place p){
  R_ASSERT_NON_RELEASE(p.dividor > 0);

  int64_t num;
  
  if (ov_mul(p.line, p.dividor, &num)){
    R_ASSERT_NON_RELEASE(false); // should be impossible
  }
  
  if (ov_add(num, p.counter, &num)){
    R_ASSERT_NON_RELEASE(false); // should be impossible
  }
  
  return make_ratio(num, p.dividor);
}

static inline Ratio place2ratio(const Place p){
  return ratio_from_place(p);
}

static inline Place p_FromDouble(double d);

static inline Place ratio2place(const Ratio ratio){
  R_ASSERT_NON_RELEASE(ratio.den > 0);
  Place place;
  
  place.line = ratio.num / ratio.den;

  if (place.line < 0){
#if !defined(RELEASE)
    abort();
#endif
    place.line = 0;
    place.counter = 0;
    place.dividor = 1;
    return place;
  }
  
  int64_t num;
  if (ov_mul(place.line, ratio.den, &num))
    goto overflow_handler;
  
  if (ov_sub(ratio.num, num, &num))
    goto overflow_handler;

  {
    Ratio r2 = RATIO_minimize(make_ratio(num, ratio.den));
    
    if (r2.den <= MAX_UINT32) {
      
      place.counter = r2.num;
      place.dividor = r2.den;
      
    } else {
      
      place.counter = scale_int64(MAX_UINT32,
                                  0, r2.den,
                                  0, r2.num);
      
      place.dividor = MAX_UINT32;    
    }

    return place;
  }
  
 overflow_handler:
  {
#if !defined(RELEASE)
    abort();
#endif
    double r = (double)ratio.num / (double)ratio.den;
    return p_FromDouble(r);
  }
}

static inline Place *PlaceCreate(int line, int counter, int dividor) {
  R_ASSERT_NON_RELEASE(line >= 0);
  R_ASSERT_NON_RELEASE(counter >= 0);
  R_ASSERT_NON_RELEASE(counter < dividor);
  R_ASSERT_NON_RELEASE(dividor > 0);
  R_ASSERT_NON_RELEASE(dividor <= MAX_UINT32);
  if (dividor==0){
    R_ASSERT(false);
    dividor=1;
  }
  Place *place=(Place*)talloc(sizeof(Place));  
  place->line = line;
  place->counter = (uint32_t)counter;
  place->dividor = (uint32_t)dividor;
  return place;
}

/**********************************************************
  FUNCTION
    Convert a float into a placement.
**********************************************************/
static inline void Double2Placement(double f,Place *p){
  if (f<0.0){
    RError("Double2Placement: Position can not start before block starts: %f",f);
    p->line=0;
    p->counter=0;
    p->dividor=1;
  }
  
  p->line = (int)f;
  f -= p->line;

  p->counter = (uint32_t)(f * MAX_UINT32);
  p->dividor = MAX_UINT32;
}

static inline void Float2Placement(float f,Place *p){
  Double2Placement((double)f,p);
}

static inline Place *PlaceCreate2(float f){
  Place *place=(Place*)talloc(sizeof(Place));
  Float2Placement(f, place);
  return place;
}

static inline Place p_FromFloat(float f){
  Place place;
  Float2Placement(f, &place);
  return place;
}

static inline Place p_FromDouble(double d){
  Place place;
  Double2Placement(d, &place);
  return place;
}

static inline double p_getDouble(const Place p){
  return (double)p.line + (double)p.counter/(double)p.dividor;
}

/*************************************************************
  FUNCTION
    Returns 0 if placement1 is the same as placement2,
    -1 if placement1 is before placement2, and 1 if after.
  NOTE
    See also placement_proc.h for macros that makes use of this
    function.
*************************************************************/
static inline int PlaceCmp(  const Place *p1,  const Place *p2){

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
static inline const Place *PlaceMax(  const Place *p1,  const Place *p2){
  if (p1==NULL)
    return p2;
  if (p2==NULL)
    return p1;

  if(PlaceCmp(p1,p2) > 0){
    return p1;
  }else{
    return p2;
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
static inline const Place *PlaceMin(  const Place *p1,  const Place *p2){
  if (p1==NULL)
    return p2;

  if (p2==NULL)
    return p1;

  if(PlaceCmp(p1,p2) > 0){
    return p2;
  }else{
    return p1;
  }
}


#define PlaceBetween(p1, p, p2) PlaceMin(PlaceMax(p1, p), p2)

extern LANGSPEC void PlaceHandleOverflow(Place *p);

// These functions are implmentented in embedded_scheme/scheme.cpp (rationals are much simpler to programme in scheme (it's just like any other number))
extern LANGSPEC Place p_Scale(const Place x, const Place x1, const Place x2, const Place y1, const Place y2);

extern LANGSPEC void PlaceAdd(Place *p1,  const Place *p2);
extern LANGSPEC void PlaceSub(Place *p1,  const Place *p2);
extern LANGSPEC void PlaceMul(Place *p1,  const Place *p2);
extern LANGSPEC void PlaceDiv(Place *p1,  const Place *p2);

extern LANGSPEC Place p_Add(const Place p1, const Place p2);
extern LANGSPEC Place p_Sub(const Place p1, const Place p2);
extern LANGSPEC Place p_Mul(const Place p1, const Place p2);
extern LANGSPEC Place p_Div(const Place p1, const Place p2);

/******************************************************************
  FUNCTION
    Quantitize 'toquant' spesified by 'quant'. 'Block' is used
    to know the last zposition in the block, so that 'toquant' isn't
    placed after that.

  EXAMPLES
    toquant=1,2,3 quant=0.5 -> toquant=1,1,2
    toquant=1,6,7 quant=0.5 -> toquant=2,0,1
    toquant=5,0,1 quant=2.0 -> toquant=6,0,1
******************************************************************/
extern LANGSPEC Place p_Quantitize(const Place p, const Place q);

static inline float p_float(const Place p){
  return (float)p.line + (float)p.counter/(float)p.dividor;
}

extern float GetfloatFromCounterDividor(uint_32 counter,uint_32 dividor);
extern float GetfloatFromLineCounterDividor(const Place *placement);
extern float GetfloatFromPlacement(const Place *placement);
extern float GetfloatFromPlace(const Place *placement);

extern void PlaceAddfloat(Place *p,float f);
extern void PlaceSubfloat(Place *p,float f);

//extern void PlaceSetLastPos(struct Blocks *wblock,Place *p);
extern LANGSPEC bool PlaceLegal(const struct Blocks *block,  const Place *p);
extern LANGSPEC void PlaceSetFirstPos(Place *p);
extern LANGSPEC Place *PlaceGetFirstPos(void);

extern LANGSPEC void PlaceSetReallinePlace(
                                           const struct WBlocks *wblock,
                                           int realline,
                                           Place *p
);

// Puts 'p' as near as possible 'tp' so that p<tp.
extern LANGSPEC void PlaceTilLimit(Place *p,  const Place *tp);

// Puts 'p' as near as possible 'tp' so that p>tp.
extern LANGSPEC void PlaceFromLimit(Place *p,  const Place *tp);



//#define PlaceEqual(a,b) (PlaceCmp((a),(b))==0)
//#define PlaceNotEqual(a,b) (PlaceCmp((a),(b))!=0)
//#define PlaceGreaterThan(a,b) (PlaceCmp((a),(b))>0)
//#define PlaceLessThan(a,b) (PlaceCmp((a),(b))<0)
//#define PlaceGreaterOrEqual(a,b) (PlaceCmp((a),(b))>=0)
//#define PlaceLessOrEqual(a,b) (PlaceCmp((a),(b))<=0)


#define PlaceEqual(a,b) (((a)->line==(b)->line) && ((a)->counter*(b)->dividor==(b)->counter*(a)->dividor))
#define PlaceNotEqual(a,b) (((a)->line!=(b)->line) || ((a)->counter*(b)->dividor!=(b)->counter*(a)->dividor))

#define PlaceGreaterThan(a,b) (							\
((a)->line > (b)->line)									\
|| 											\
(((a)->line == (b)->line) && ((a)->counter*(b)->dividor > (b)->counter*(a)->dividor))	\
)
#define PlaceLessThan(a,b) (							\
((a)->line<(b)->line)									\
|| 											\
(((a)->line == (b)->line) && ((a)->counter*(b)->dividor < (b)->counter*(a)->dividor))	\
)

#define PlaceGreaterOrEqual(a,b) (							\
((a)->line>(b)->line)									\
|| 											\
(((a)->line == (b)->line) && ((a)->counter*(b)->dividor >= (b)->counter*(a)->dividor))	\
)
#define PlaceLessOrEqual(a,b) (							\
((a)->line<(b)->line)									\
|| 											\
(((a)->line == (b)->line) && ((a)->counter*(b)->dividor <= (b)->counter*(a)->dividor))	\
)


#define PlaceIsBetween(a,b,c) (PlaceGreaterOrEqual((a),(b)) && PlaceLessOrEqual((a),(c)))
#define PlaceIsBetween2(a,b,c) (PlaceGreaterOrEqual((a),(b)) && PlaceLessThan((a),(c)))
#define PlaceIsBetween3(a,b,c) (PlaceGreaterThan((a),(b)) && PlaceLessThan((a),(c)))


#define p_Equal(a,b) (((a).line==(b).line) && ((a).counter*(b).dividor==(b).counter*(a).dividor))
#define p_NOT_Equal(a,b) (((a).line!=(b).line) || ((a).counter*(b).dividor!=(b).counter*(a).dividor))

#define p_Greater_Than(a,b) (							\
((a).line>(b).line)									\
|| 											\
(((a).line == (b).line) && ((a).counter*(b).dividor > (b).counter*(a).dividor)) \
)
#define p_Less_Than(a,b) (							\
((a).line<(b).line)									\
|| 											\
(((a).line == (b).line) && ((a).counter*(b).dividor < (b).counter*(a).dividor))	\
)

#define p_Greater_Or_Equal(a,b) (							\
((a).line>(b).line)									\
|| 											\
(((a).line == (b).line) && ((a).counter*(b).dividor >= (b).counter*(a).dividor))	\
)
#define p_Less_Or_Equal(a,b) (							\
((a).line<(b).line)									\
|| 											\
(((a).line == (b).line) && ((a).counter*(b).dividor <= (b).counter*(a).dividor))	\
)



// I don't trust the implementation of PlaceSub (warning: this one hasn't been tested or used and shouldn't be trusted)
static inline void TrustedPlaceSub(const Place *p1,  const Place *p2, Place *result){
  R_ASSERT(PlaceGreaterOrEqual(p1, p2));
           
  result->line = p1->line - p2->line;
  
  result->dividor = p1->dividor * p2->dividor;
  
  int64_t counter1 = p1->counter * p2->dividor;
  int64_t counter2 = p2->counter * p1->dividor;

  int64_t counter = counter1-counter2;
  R_ASSERT(counter < INT_MAX);
    
  result->counter = (uint32_t)counter;

  // todo: 
}


#ifndef RADIUM_PLACEMENTISCALLINGNOW
extern const Place PlaceFirstPos;
#endif

#define PlaceGetFirstPos() &PlaceFirstPos

#define PlaceSetFirstPos(a) do { (a)->line=0;(a)->counter=0;(a)->dividor=1; }while(0)

#define PlaceIsFirstPos(a) ((a)->line==0 && (a)->counter==0)

static inline void PlaceCopy(Place *to, const Place *from){
  to->line = from->line;
  to->counter = from->counter;
  to->dividor = from->dividor;
}

#define GetfloatFromPlace(a) ((float) ( \
		(float)((a)->line) + ( \
			(float)((a)->counter)/ \
			(float)((a)->dividor) \
		) \
	))

#define GetDoubleFromPlace(a) ((double) ( \
		(double)((a)->line) + ( \
			(double)((a)->counter)/ \
			(double)((a)->dividor) \
		) \
	))

#define GetfloatFromLineCounterDividor(a) GetfloatFromPlace(a)
#define GetfloatFromPlacement(a) GetfloatFromPlace(a)
#define GetFloatFromPlace(a) GetfloatFromPlace(a)

#define GetfloatFromCounterDividor(a,b) ((a)==0 ? 0.0f : (float)((float)(a)/(float)(b)))

#define PlaceSetLastPos(a,b) do{   \
    (b)->line = (a)->num_lines-1;  \
    (b)->counter = MAX_UINT32-1;   \
    (b)->dividor = MAX_UINT32;     \
  }while(0)

static inline Place *PlaceGetLastPos(const struct Blocks *block){
  return PlaceCreate(block->num_lines-1, MAX_UINT32-1, MAX_UINT32);
}

static inline Place p_Last_Pos(const struct Blocks *block){
  return p_Create(block->num_lines-1, MAX_UINT32-1, MAX_UINT32);
}

#define SetAbsoluteLastPlace(place, block) do{        \
    Place *p = place;                                 \
    p->line = (block)->num_lines;                     \
    p->counter = 0;                                   \
    p->dividor = 1;                                   \
  }while(0)

static inline Place p_Absolute_Last_Pos(const struct Blocks *block){
  return p_Create(block->num_lines, 0, 1);
}

extern LANGSPEC const char* PlaceToString(const Place *a);


#define PrintPlace(title,a) printf(title ": %d + %d/%d\n",(a)->line,(a)->counter,(a)->dividor);


#endif // COMMON_PLACEMENT_PROC_H

