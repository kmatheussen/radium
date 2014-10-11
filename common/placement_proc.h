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

#ifndef TRACKER_PLACEMENT
#define TRACKER_PLACEMENT


extern Place *PlaceCreate(int line, int counter, int dividor);

extern int PlaceCmp(  Place *p1,  Place *p2);
extern Place *PlaceMax(  Place *p1,  Place *p2);
extern Place *PlaceMin(  Place *p1,  Place *p2);

#define PlaceBetween(p1, p, p2) PlaceMin(PlaceMax(p1, p), p2)

extern void PlaceHandleOverflow(Place *p);

extern void PlaceAdd(Place *p1,  Place *p2);
extern void PlaceSub(Place *p1,  Place *p2);
extern void PlaceMul(Place *p1,  Place *p2);
extern void PlaceDiv(Place *p1,  Place *p2);

extern float GetfloatFromCounterDividor(uint_32 counter,uint_32 dividor);
extern float GetfloatFromLineCounterDividor(Place *placement);
extern float GetfloatFromPlacement(Place *placement);
extern float GetfloatFromPlace(Place *placement);

extern LANGSPEC void Float2Placement(float f,Place *p);
extern void PlaceAddfloat(Place *p,float f);
extern void PlaceSubfloat(Place *p,float f);

//extern void PlaceSetLastPos(struct Blocks *wblock,Place *p);
extern bool PlaceLegal(struct Blocks *block,  Place *p);
extern void PlaceSetFirstPos(Place *p);
extern Place *PlaceGetFirstPos(void);

extern void PlaceSetReallinePlace(
	  struct WBlocks *wblock,
	int realline,
	Place *p
);

extern void PlaceTilLimit(Place *p,  Place *tp);
extern void PlaceFromLimit(Place *p,  Place *tp);



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



#ifndef RADIUM_PLACEMENTISCALLINGNOW
extern Place PlaceFirstPos;
#endif

#define PlaceGetFirstPos() &PlaceFirstPos

#define PlaceSetFirstPos(a) do { (a)->line=0;(a)->counter=0;(a)->dividor=1; }while(0)

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

#define GetfloatFromLineCounterDividor(a) GetfloatFromPlace(a)
#define GetfloatFromPlacement(a) GetfloatFromPlace(a)
#define GetFloatFromPlace(a) GetfloatFromPlace(a)

#define GetfloatFromCounterDividor(a,b) ((a)==0 ? 0.0f : (float)((float)(a)/(float)(b)))

#define PlaceSetLastPos(a,b) (b)->line=(a)->num_lines-1;(b)->counter=MAX_UINT32-1;(b)->dividor=MAX_UINT32

#endif

