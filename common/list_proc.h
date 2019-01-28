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


#ifndef RADIUM_COMMON_LIST_PROC_H
#define RADIUM_COMMON_LIST_PROC_H

extern LANGSPEC void ListAddElement3(
	void *listroot,
	struct ListHeader3 *element
);

extern LANGSPEC void ListAddElement3_a(
	void *voidlistroot,
	struct ListHeader3 *element
);

extern LANGSPEC void ListAddElement1(
	void *listroot,
	struct ListHeader1 *element
);

extern LANGSPEC void ListAddElementP(
	void *listroot,
	struct ListHeaderP *element
);

extern LANGSPEC void ListAddElement1_a(
	void *voidlistroot,
	struct ListHeader1 *element
);

extern LANGSPEC void ListAddElementP_a(
	void *voidlistroot,
	struct ListHeaderP *element
);


extern LANGSPEC void ListRemoveElement3_fromNum(
                                       void *voidlistroot,
                                       int num
                                       );

extern LANGSPEC void ListRemoveElement1_fromNum(
                                       void *voidlistroot,
                                       int num
                                       );

extern LANGSPEC void ListRemoveElement3(
	void *listroot,
	const struct ListHeader3 *element
);

extern LANGSPEC void ListRemoveElement1(
	void *listroot,
	const struct ListHeader1 *element
);

extern LANGSPEC void ListRemoveElements3(
                                         void *voidlistroot,
                                         const Place *p1,
                                         const Place *p2
);

extern LANGSPEC void ListMoveElement3_ns(
                                const void *voidlistroot,
                                struct ListHeader3 *element,
                                const Place *newplace,
                                const Place *firstlegalpos,
                                const Place *lastlegalpos
                                );

extern LANGSPEC struct ListHeader3 *ListMoveElement3_FromNum_ns(
                                                                const void *voidlistroot,
                                                                int num,
                                                                const Place *newplace,
                                                                const Place *firstlegalpos,
                                                                const Place *lastlegalpos
                                                                );

extern LANGSPEC int ListAddElement3_ns(
	void *listroot,
	struct ListHeader3 *element
);

extern LANGSPEC NInt ListFindFirstFreePlace1(struct ListHeader1 *list);

extern LANGSPEC void ListAddElement1_ff(
	void *voidlistroot,
	struct ListHeader1 *element
);

extern LANGSPEC void *ListFindElement1(const struct ListHeader1 *list,NInt num);
extern LANGSPEC void *ListFindElementP(struct ListHeaderP *list,NInt num);
extern LANGSPEC void *ListFindElement1_r0(const struct ListHeader1 *list,NInt num);
extern LANGSPEC void *ListFindElement1_num_r0(const struct ListHeader1 *list,NInt num);

extern LANGSPEC void *ListFindElement3(
	const struct ListHeader3 *element,
	const Place *placement
);

extern LANGSPEC void *ListFindElement3_num(
	struct ListHeader3 *element,
	NInt num
);

extern LANGSPEC void *ListFindElement3_num_r0(
                              struct ListHeader3 *element,
                              NInt num
                              );


#define ListFindElement1_num(a,b) ListFindElement3_num((struct ListHeader3 *)(a),b)

extern LANGSPEC bool isInList3(
	struct ListHeader3 *liststart,
	struct ListHeader3 *element
);

extern LANGSPEC bool isInList1(
	struct ListHeader1 *liststart,
	struct ListHeader1 *element
);

extern LANGSPEC bool isInList0(
	struct ListHeader0 *liststart,
	struct ListHeader0 *element
);

extern LANGSPEC bool isInList3_m(struct ListHeader3 *liststart,struct ListHeader3 *element,...);
extern LANGSPEC bool isInList1_m(struct ListHeader1 *liststart,struct ListHeader1 *element,...);

extern LANGSPEC void *ListPrevElement1(
	struct ListHeader1 *list,
	struct ListHeader1 *element
);

extern LANGSPEC void *ListPrevElement3(
	struct ListHeader3 *list,
	struct ListHeader3 *element
);

extern LANGSPEC void CutListAt(void *listroot,const Place *place);
extern LANGSPEC void CutListAt_a(void *listroot,const Place *place);
extern LANGSPEC void CutListAt1(void *listroot,NInt num);

extern LANGSPEC int ListPosition3(struct ListHeader3 *list,
                                  struct ListHeader3 *element
                                  );

extern LANGSPEC int ForAllListElements1(
	void *voidlistroot,
	int (*function)(struct ListHeader1 *list)
);

extern LANGSPEC int ForAllListElements3(
	void *voidlistroot,
	int (*function)(struct ListHeader3 *list)
);

extern LANGSPEC NInt ListFindNumElements0(
                                          struct ListHeader0 *list
                                          );

extern LANGSPEC NInt ListFindNumElements1(
	struct ListHeader1 *list
);

extern LANGSPEC NInt ListFindNumElements3(
	struct ListHeader3 *list
);

extern LANGSPEC NInt ListFindElementPos1(
                                struct ListHeader1 *list,
                                struct ListHeader1 *element
                                );

extern LANGSPEC NInt ListFindElementPos3(
                                struct ListHeader3 *list,
                                struct ListHeader3 *element
                                );

extern LANGSPEC Place *ListLastPlace3(struct ListHeader3 *list);
extern LANGSPEC void *ListLast3(struct ListHeader3 *list);
extern LANGSPEC void *ListSecondLast3(struct ListHeader3 *list);
extern LANGSPEC void *ListLast1(struct ListHeader1 *list);

extern LANGSPEC void List_InsertLines3(
	void *to,
	struct ListHeader3 *l,
	int line,
	int toinsert,
	void (*Insert_Lines_extra)(void *to,struct ListHeader3 *l,int line,int toinsert)
);

extern LANGSPEC void List_InsertRatio3(
                                       void *to,
                                       struct ListHeader3 *l,
                                       Place place,
                                       const Ratio toinsert,
                                       void (*Insert_Ratio_extra)(void *to,struct ListHeader3 *l, const Place place, const Ratio toinsert)
                                       );

extern LANGSPEC void List_InsertPlaceLen3(
	struct Blocks *block,
	void *to,
	struct ListHeader3 *l,
	float place,
	float toplace,
	void (*Insert_PlaceLen_extra)(struct Blocks *block,void *to,struct ListHeader3 *l,float place,float toplace)
);

#ifndef TRACKER_LIST_PROC_MACROS
#define TRACKER_LIST_PROC_MACROS

#define ListRemoveElementP(a,b) ListRemoveElement1((a),(struct ListHeader1 *)(b));
#define isInListP(a,b) isInList1((struct ListHeaderP *)(a),(struct ListHeader1 *)(b));
#define ListPrevElementP(a,b) ListPrevElement1((struct ListHeader1 *)(a),(struct ListHeaderP *)(b));
#define ListFindNumElementsP(a) ListFindNumElements1((struct ListHeader1 *)(a));
#define ListLastP(a) ListLast1((struct ListHeader1 *)(a));


#endif

#endif
