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


static inline Place PlaceCreate3(int line, int counter, int dividor) {
  Place place;
  place.line = line;
  place.counter = counter;
  place.dividor = dividor;
  return place;
}

static inline Place *PlaceCreate(int line, int counter, int dividor) {
  Place *place=(Place*)talloc(sizeof(Place));
  place->line = line;
  place->counter = counter;
  place->dividor = dividor;
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

  p->counter = f * MAX_UINT32;
  p->dividor = MAX_UINT32;
}

static inline void Float2Placement(float f,Place *p){
  Double2Placement(f,p);
}

static inline Place *PlaceCreate2(float f){
  Place *place=(Place*)talloc(sizeof(Place));
  Float2Placement(f, place);
  return place;
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
static inline Place *PlaceMax(  Place *p1,  Place *p2){
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
static inline Place *PlaceMin(  Place *p1,  Place *p2){
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
extern LANGSPEC Place *PlaceScale(const Place *x, const Place *x1, const Place *x2, const Place *y1, const Place *y2);
extern LANGSPEC Place PlaceQuantitize(Place p, Place q);
extern LANGSPEC void PlaceAdd(Place *p1,  const Place *p2);
extern LANGSPEC void PlaceSub(Place *p1,  const Place *p2);
extern LANGSPEC void PlaceMul(Place *p1,  const Place *p2);
extern LANGSPEC void PlaceDiv(Place *p1,  const Place *p2);


extern float GetfloatFromCounterDividor(uint_32 counter,uint_32 dividor);
extern float GetfloatFromLineCounterDividor(const Place *placement);
extern float GetfloatFromPlacement(const Place *placement);
extern float GetfloatFromPlace(const Place *placement);

extern void PlaceAddfloat(Place *p,float f);
extern void PlaceSubfloat(Place *p,float f);

//extern void PlaceSetLastPos(struct Blocks *wblock,Place *p);
extern bool PlaceLegal(const struct Blocks *block,  const Place *p);
extern void PlaceSetFirstPos(Place *p);
extern Place *PlaceGetFirstPos(void);

extern LANGSPEC void PlaceSetReallinePlace(
                                           const struct WBlocks *wblock,
                                           int realline,
                                           Place *p
);

// Puts 'p' as near as possible 'tp' so that p<tp.
extern void PlaceTilLimit(Place *p,  const Place *tp);

// Puts 'p' as near as possible 'tp' so that p>tp.
extern void PlaceFromLimit(Place *p,  const Place *tp);



//#define PlaceEqual(a,b) (PlaceCmp((a),(b))==0)
//#define PlaceNotEqual(a,b) (PlaceCmp((a),(b))!=0)
//#define PlaceGreaterThan(a,b) (PlaceCmp((a),(b))>0)
//#define PlaceLessThan(a,b) (PlaceCmp((a),(b))<0)
//#define PlaceGreaterOrEqual(a,b) (PlaceCmp((a),(b))>=0)
//#define PlaceLessOrEqual(a,b) (PlaceCmp((a),(b))<=0)


#define PlaceEqual(a,b) (((a)->line==(b)->line) && ((a)->counter*(b)->dividor==(b)->counter*(a)->dividor))
#define PlaceNotEqual(a,b) (((a)->line!=(b)->line) || ((a)->counter*(b)->dividor!=(b)->counter*(a)->dividor))

#define PlaceGreaterThan(a,b) (							\
((a)->line>(b)->line)									\
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


// I don't trust the implementation of PlaceSub (warning: this one hasn't been tested or used and shouldn't be trusted)
static inline void TrustedPlaceSub(const Place *p1,  const Place *p2, Place *result){
  R_ASSERT(PlaceGreaterOrEqual(p1, p2));
           
  result->line = p1->line - p2->line;
  
  result->dividor = p1->dividor * p2->dividor;
  
  int64_t counter1 = p1->counter * p2->dividor;
  int64_t counter2 = p2->counter * p1->dividor;

  result->counter = counter1-counter2;

  // todo: 
}


#ifndef RADIUM_PLACEMENTISCALLINGNOW
extern Place PlaceFirstPos;
#endif

#define PlaceGetFirstPos() &PlaceFirstPos

#define PlaceSetFirstPos(a) do { (a)->line=0;(a)->counter=0;(a)->dividor=1; }while(0)

#define PlaceIsFirstPos(a) ((a)->line==0 && (a)->counter==0)

#define PlaceCopy(a,b) do {                              \
    Place *das_a = (Place *)a;                           \
    Place *das_b = (Place *)b;                                    \
    das_a->line=das_b->line;                                      \
    das_a->counter=das_b->counter;                                \
    das_a->dividor=das_b->dividor;                                \
  } while(0)

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

static inline Place *PlaceGetLastPos(struct Blocks *block){
  return PlaceCreate(block->num_lines-1, MAX_UINT32-1, MAX_UINT32);
}

#define PrintPlace(title,a) printf(title ": %d + %d/%d\n",(a)->line,(a)->counter,(a)->dividor);


#endif // COMMON_PLACEMENT_PROC_H

