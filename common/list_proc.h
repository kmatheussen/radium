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


#ifndef TRACKER_INCLUDE

extern void ListAddElement3(
	void *listroot,
	struct ListHeader3 *element
);

extern void ListAddElement3_a(
	void *voidlistroot,
	struct ListHeader3 *element
);

extern void ListAddElement1(
	void *listroot,
	struct ListHeader1 *element
);

extern void ListAddElementP(
	void *listroot,
	struct ListHeaderP *element
);

extern void ListAddElement1_a(
	void *voidlistroot,
	struct ListHeader1 *element
);

extern void ListAddElementP_a(
	void *voidlistroot,
	struct ListHeaderP *element
);


extern void ListRemoveElement3_fromNum(
                                       void *voidlistroot,
                                       int num
                                       );

extern void ListRemoveElement1_fromNum(
                                       void *voidlistroot,
                                       int num
                                       );

extern void ListRemoveElement3(
	void *listroot,
	struct ListHeader3 *element
);

extern void ListRemoveElement1(
	void *listroot,
	struct ListHeader1 *element
);

extern void ListRemoveElements3(
	void *voidlistroot,
	Place *p1,
	Place *p2
);

extern struct ListHeader3 *ListAddElement3_ns(
	void *listroot,
	struct ListHeader3 *element
);

extern NInt ListFindFirstFreePlace1(struct ListHeader1 *list);

extern void ListAddElement1_ff(
	void *voidlistroot,
	struct ListHeader1 *element
);

extern LANGSPEC void *ListFindElement1(const struct ListHeader1 *list,NInt num);
extern void *ListFindElementP(struct ListHeaderP *list,NInt num);
extern void *ListFindElement1_r0(const struct ListHeader1 *list,NInt num);

extern void *ListFindElement3(
	struct ListHeader3 *element,
	Place *placement
);
void *ListFindElement3_num(
	struct ListHeader3 *element,
	NInt num
);

#define ListFindElement1_num(a,b) ListFindElement3_num((struct ListHeader3 *)(a),b)

extern bool isInList3(
	struct ListHeader3 *liststart,
	struct ListHeader3 *element
);

extern bool isInList1(
	struct ListHeader1 *liststart,
	struct ListHeader1 *element
);

extern bool isInList3_m(struct ListHeader3 *liststart,struct ListHeader3 *element,...);
extern bool isInList1_m(struct ListHeader1 *liststart,struct ListHeader1 *element,...);

extern void *ListPrevElement1(
	struct ListHeader1 *list,
	struct ListHeader1 *element
);

extern void *ListPrevElement3(
	struct ListHeader3 *list,
	struct ListHeader3 *element
);

extern void CutListAt(void *listroot,Place *place);
extern void CutListAt_a(void *listroot,Place *place);
extern void CutListAt1(void *listroot,NInt num);

extern int ForAllListElements1(
	void *voidlistroot,
	int (*function)(struct ListHeader1 *list)
);

extern int ForAllListElements3(
	void *voidlistroot,
	int (*function)(struct ListHeader3 *list)
);

extern NInt ListFindNumElements1(
	struct ListHeader1 *list
);

extern NInt ListFindNumElements3(
	struct ListHeader3 *list
);

extern NInt ListFindElementPos1(
                                struct ListHeader1 *list,
                                struct ListHeader1 *element
                                );

extern NInt ListFindElementPos3(
                                struct ListHeader3 *list,
                                struct ListHeader3 *element
                                );

extern Place *ListLastPlace3(struct ListHeader3 *list);
extern void *ListLast3(struct ListHeader3 *list);
extern LANGSPEC void *ListLast1(struct ListHeader1 *list);

extern void List_InsertLines3(
	void *to,
	struct ListHeader3 *l,
	int line,
	int toinsert,
	void (*Insert_Lines_extra)(void *to,struct ListHeader3 *l,int line,int toinsert)
);

extern void List_InsertPlaceLen3(
	struct Blocks *block,
	void *to,
	struct ListHeader3 *l,
	float place,
	float toplace,
	void (*Insert_PlaceLen_extra)(struct Blocks *block,void *to,struct ListHeader3 *l,float place,float toplace)
);

#endif

#ifndef TRACKER_LIST_PROC_MACROS
#define TRACKER_LIST_PROC_MACROS

#define ListRemoveElementP(a,b) ListRemoveElement1((a),(struct ListHeader1 *)(b));
#define isInListP(a,b) isInList1((struct ListHeaderP *)(a),(struct ListHeader1 *)(b));
#define ListPrevElementP(a,b) ListPrevElement1((struct ListHeader1 *)(a),(struct ListHeaderP *)(b));
#define ListFindNumElementsP(a) ListFindNumElements1((struct ListHeader1 *)(a));
#define ListLastP(a) ListLast1((struct ListHeader1 *)(a));


#endif
