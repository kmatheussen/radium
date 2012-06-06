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



/******************************************************************************
   LIST.C

     Various List functions.

******************************************************************************/

#include "nsmtracker.h"
#include "placement_proc.h"
#include <stdarg.h>

#include "list_proc.h"



/* Next two structs.. It only looks a bit nicer using them I think. */

struct ListHeaderPointer1{
	struct ListHeader1 *root;
};

struct ListHeaderPointer3{
	struct ListHeader3 *root;
};



/******************************************************************************
    SHORT
        Add an Element to a list.

    NAME
      ListAddElement

    SYNOPSIS
        void ListAddElement(
          struct ListHeaderPointer *listroot,
          struct ListHeader *element
        )

    FUNCTION
		  Reads element->place, element->counter and element->dividor
        and puts the element before before that in the list.
******************************************************************************/
void ListAddElement3(
	void *voidlistroot,
	struct ListHeader3 *element
){
	struct ListHeaderPointer3 *listroot=voidlistroot;
	struct ListHeader3 *prev=NULL;
	struct ListHeader3 *temp=listroot->root;

	if(element==NULL) return;

	/* According to profiling, this function used quite a bit of time,
	   and by adding the next four lines, it seems to be a bit better. */
	while(temp!=NULL && temp->p.line < element->p.line){
		prev=temp;
		temp=temp->next;
	}

	while(temp!=NULL){
		if(PlaceGreaterOrEqual(&temp->p,&element->p)) break;
		prev=temp;
		temp=temp->next;
	}

	if(prev==NULL){
		element->next=listroot->root;
		listroot->root=element;
	}else{
		element->next=prev->next;
		prev->next=element;
	}
}



void ListAddElement3_a(
	void *voidlistroot,
	struct ListHeader3 *element
){
	struct ListHeaderPointer3 *listroot=voidlistroot;
	struct ListHeader3 *prev=NULL;
	struct ListHeader3 *temp=listroot->root;

	if(element==NULL) return;

	while(temp!=NULL){
		if(PlaceGreaterThan(&temp->p,&element->p)) break;
		prev=temp;
		temp=temp->next;
	}

	if(prev==NULL){
		element->next=listroot->root;
		listroot->root=element;
	}else{
		element->next=prev->next;
		prev->next=element;
	}
}



/******************************************************************************

    SHORT
        Add an Element to a list.

    NAME
      ListAddElement

    SYNOPSIS
        void ListAddElement(
          struct ListHeaderPointer *listroot,
          struct ListHeader *element
        )

    FUNCTION
		  Reads element->l.num and puts the element
        before before that in the list.

******************************************************************************/

void ListAddElement1(
	void *voidlistroot,
	struct ListHeader1 *element
){
	struct ListHeaderPointer1 *listroot=voidlistroot;
	struct ListHeader1 *temp=listroot->root;
	struct ListHeader1 *prev=NULL;

	if(element==NULL) return;

	while(temp!=NULL){
		if(temp->num>=element->num) break;
		prev=temp;
		temp=temp->next;
	}

	if(prev==NULL){
		element->next=listroot->root;
		listroot->root=element;
	}else{
		element->next=prev->next;
		prev->next=element;
	}
}



/******************************************************************************
  FUNCTION
    Same as ListAddElement1, except that it puts the element after all
    elements that has equal l.num. ListAddElement1 puts it before.
    (this is important for the player-routine, to ensure that when
     a note is started at the same time as another one is ended, that it doesnt
     start the new note before the previous is ended.)
******************************************************************************/
void ListAddElement1_a(
	void *voidlistroot,
	struct ListHeader1 *element
){
	struct ListHeaderPointer1 *listroot=voidlistroot;
	struct ListHeader1 *temp=listroot->root;
	struct ListHeader1 *prev=NULL;

	if(element==NULL) return;

	while(temp!=NULL){
		if(temp->num>element->num) break;
		prev=temp;
		temp=temp->next;
	}

	if(prev==NULL){
		element->next=listroot->root;
		listroot->root=element;
	}else{
		element->next=prev->next;
		prev->next=element;
	}
}

void ListRemoveElement3(
	void *voidlistroot,
	struct ListHeader3 *element
){
	struct ListHeaderPointer3 *listroot=voidlistroot;
	struct ListHeader3 *temp=listroot->root;
	struct ListHeader3 *prev=NULL;

	if(temp==element){
		listroot->root=element->next;
	}else{
		while(temp!=element){
			prev=temp;
			temp=temp->next;
		}
		prev->next=element->next;
	}
}

void ListRemoveElement1(
	void *voidlistroot,
	struct ListHeader1 *element
){
	ListRemoveElement3(voidlistroot,(struct ListHeader3 *)element);
}

/*****************************************************************************
  FUNCTION
    Removes all elements in a list that is greater or equal to p1, and
    less than p2.
******************************************************************************/
void ListRemoveElements3(
	void *voidlistroot,
	Place *p1,
	Place *p2
){
	struct ListHeaderPointer3 *listroot=voidlistroot;
	struct ListHeader3 *l=listroot->root;
	struct ListHeader3 *temp;

	while(l!=NULL){
		temp=l->next;
		if(PlaceIsBetween2(&l->p,p1,p2)){
			ListRemoveElement3(voidlistroot,l);
		}
		l=temp;
	}
}


/*****************************************************************************
  FUNCTION
    Adds an element only if the list doesn't allready contain an element
    with the same placement attributes.
******************************************************************************/
struct ListHeader3 *ListAddElement3_ns(
	void *listroot,
	struct ListHeader3 *element
){
	if(element==NULL) return NULL;

	ListAddElement3(listroot,element);
	if(element->next!=NULL)
		if(PlaceEqual(&element->p,&element->next->p)){
			ListRemoveElement3(listroot,element);
			return NULL;
		}
	return element;
}

NInt ListFindFirstFreePlace1(struct ListHeader1 *list){
	NInt num= -1;
	while(list!=NULL){
		if(list->num > num+1){
			return num+1;
		}
		num++;
		list=list->next;
	}
	return num+1;
}

/*****************************************************************************
  FUNCTION
    Sets the num attribute to the first free elementnum and inserts the
    element.
*****************************************************************************/
void ListAddElement1_ff(
	void *voidlistroot,
	struct ListHeader1 *element
){
	struct ListHeaderPointer1 *listroot=voidlistroot;
	element->num=ListFindFirstFreePlace1(listroot->root);
	ListAddElement1(listroot,element);
}


void *ListFindElement1(struct ListHeader1 *list,NInt num){
	NInt lastnum;

	while(list!=NULL && list->num<=num){
		lastnum=list->num;
		if(list->num==num) return list;
		list=list->next;
		if(list!=NULL && lastnum+1!=list->num){
			RError("Warning. In function 'ListFindElement1' in file 'list.c'. Strange list. Last: %d, now: %d\n",lastnum,list->num);
		}
	}

	RError("Warning. In function 'ListFindElement1' in file 'list.c'. Could not find element %d.\n",num);
	return NULL;
}

void *ListFindElementP(struct ListHeaderP *list,NInt num){

	while(list!=NULL && list->time<=num){
		if(list->time==num) return list;
		list=list->next;
	}

	RError("Warning. In function 'ListFindElementP' in file 'list.c'. Could not find element %d.\n",num);
	return NULL;
}



/****************************************************************
  FUNCTION
    Same as ListFindElement1, except that it is supposed to
    return NULL if the element is not found, so it doesn't write
    out an error-message in that case. The list may allso
    contain not-increase-by-one sequential elements, so it doesn't
    write out an error-message in that case either.
****************************************************************/

void *ListFindElement1_r0(struct ListHeader1 *list,NInt num){

	while(list!=NULL && list->num<=num){
		if(list->num==num) return list;
		list=list->next;
	}

	return NULL;
}


/**********************************************************************
  FUNCTION
    Finds the first element in the list at or after place 'place' and
    counter 'counter' for dividor 'dividor'. If no element is found
    after 'place' and 'counter/dividor', NULL is returned.
**********************************************************************/
void *ListFindElement3(
	struct ListHeader3 *element,
	Place *placement
){
	for(;element!=NULL;element=element->next)
		if(PlaceGreaterOrEqual(&element->p,placement)) return element;

	return NULL;
}

void *ListFindElement3_num(
	struct ListHeader3 *element,
	NInt num
){
	int lokke;

	for(lokke=0;lokke<num && element!=NULL;lokke++)
		element=element->next;

	return element;
}

/**********************************************************************
  FUNCTION
    Returns true if the element is in a list.
**********************************************************************/
bool isInList3(
	struct ListHeader3 *liststart,
	struct ListHeader3 *element
){
	while(liststart!=NULL){
		if(liststart==element) return true;
		liststart=liststart->next;
	}
	return false;
}

bool isInList1(
	struct ListHeader1 *liststart,
	struct ListHeader1 *element
){
	return isInList3((struct ListHeader3 *)liststart,(struct ListHeader3 *)element);
}

extern struct Root *root;

/* Do a check on an array of liststart/elements, ended with the root-pointer. */
bool isInList3_m(struct ListHeader3 *liststart,struct ListHeader3 *element,...){
  va_list ap;

  struct ListHeader3 *i1;
  struct ListHeader3 *i2;

  if(isInList3(liststart,element)==false) return false;

  va_start(ap,element);

  for(;;){
    i1=va_arg(ap,struct ListHeader3 *);
    if((void *)i1==(void *)root) break;
    i2=va_arg(ap,struct ListHeader3 *);
    if(isInList3(i1,i2)==false){
      va_end(ap);
      return false;
    }
  }

  va_end(ap);
  return true;
}

bool isInList1_m(struct ListHeader1 *liststart,struct ListHeader1 *element,...){
  va_list ap;

  struct ListHeader1 *i1;
  struct ListHeader1 *i2;

  if(isInList1(liststart,element)==false) return false;

  va_start(ap,element);

  for(;;){
    i1=va_arg(ap,struct ListHeader1 *);
    if((void *)i1==(void *)root) break;
    i2=va_arg(ap,struct ListHeader1 *);
    if(isInList1(i1,i2)==false){
      va_end(ap);
      return false;
    }
  }

  va_end(ap);
  return true;
}


/******************************************************************************
  FUNCTION
    Returns the previos element in a list. (slow).
******************************************************************************/
void *ListPrevElement1(
	struct ListHeader1 *list,
	struct ListHeader1 *element
){
	struct ListHeader1 *prev=NULL;
	while(list!=element){prev=list;list=list->next;}

	return prev;

}

void *ListPrevElement3(
	struct ListHeader3 *list,
	struct ListHeader3 *element
){
	return ListPrevElement1((struct ListHeader1 *)list,(struct ListHeader1 *)element);
}




/******************************************************************************
  FUNCTION
    Remove all elements that is placed after or has the same position as
    'placement'.
******************************************************************************/
void CutListAt(void *listroot,Place *place){
	struct ListHeaderPointer3 *root=(struct ListHeaderPointer3 *)listroot;
	struct ListHeader3 *element=root->root;

	if(element==NULL) return;

	if(PlaceGreaterOrEqual(&element->p,place)){
		root->root=NULL;
		return;
	}

	for(;element!=NULL;element=element->next){
		if(
			element->next!=NULL &&
			PlaceGreaterOrEqual(&element->next->p,place)
		){
			element->next=NULL;
			break;
		}
	}
	
}


/* Nearly the same, except that it cuts first one that is placed _after_ 'place' */
void CutListAt_a(void *listroot,Place *place){
	struct ListHeaderPointer3 *root=(struct ListHeaderPointer3 *)listroot;
	struct ListHeader3 *element=root->root;

	if(element==NULL) return;

	if(PlaceGreaterThan(&element->p,place)){
		root->root=NULL;
		return;
	}

	for(;element!=NULL;element=element->next){
		if(
			element->next!=NULL &&
			PlaceGreaterThan(&element->next->p,place)
		){
			element->next=NULL;
			break;
		}
	}
	
}

void CutListAt1(void *listroot,NInt num){
	struct ListHeaderPointer1 *root=(struct ListHeaderPointer1 *)listroot;
	struct ListHeader1 *element=root->root;

	if(element==NULL) return;

	if(element->num>num){
		root->root=NULL;
		return;
	}

	for(;element!=NULL;element=element->next){
		if(
			element->next!=NULL &&
			element->next->num>=num
		){
			element->next=NULL;
			break;
		}
	}
	
}

/******************************************************************************
  FUNCTION
    Calls 'function' for all list elements with each list element as argument.
    Starts at the first element and so on. If 'funtion' returns anything else
    than zero, ForAllListElements1 will stop and return that number. You are
    allowed to change the next attribute inside function, and you can free
    the element too if you want, but then you'll have to set the previous
    next pointer to an appropriate value. Returns -1 if there was no elements,
    0 if all went okey.
******************************************************************************/
int ForAllListElements1(
	void *voidlistroot,
	int (*function)(struct ListHeader1 *list)
){
	NInt ret;
	struct ListHeaderPointer1 *listroot=voidlistroot;
	struct ListHeader1 *temp;
	struct ListHeader1 *element=listroot->root;

	if(element==NULL) return -1;

	while(element!=NULL){
		temp=element->next;
		ret=(*function)(element);
		if(ret!=0) return ret;
		element=temp;
	}

	return 0;
}

int ForAllListElements3(
	void *voidlistroot,
	int (*function)(struct ListHeader3 *list)
){
	NInt ret;
	struct ListHeaderPointer3 *listroot=voidlistroot;
	struct ListHeader3 *temp;
	struct ListHeader3 *element=listroot->root;

	if(element==NULL) return -1;

	while(element!=NULL){
		temp=element->next;
		ret=(*function)(element);
		if(ret!=0) return ret;
		element=temp;
	}

	return 0;
}


/******************************************************************************
  FUNCTION
    Returns the number of elements in a list.
******************************************************************************/

NInt ListFindNumElements1(
	struct ListHeader1 *list
){
	NInt ret=0;
	while(list!=NULL){
		list=list->next;
		ret++;
	}
	return ret;
}

NInt ListFindNumElements3(
	struct ListHeader3 *list
){
	return ListFindNumElements1((struct ListHeader1 *)list);
}


/******************************************************************************
  FUNCTION
    Returns the position of an element in a list.
******************************************************************************/

NInt ListFindElementPos1(
                         struct ListHeader1 *list,
                         struct ListHeader1 *element
){
	NInt pos=0;
	while(list!=element){
		list=list->next;
		pos++;
                if (list==NULL) {
                  RError("element not in list");
                  return pos-1;
                }
	}
	return pos;
}

NInt ListFindElementPos3(
                         struct ListHeader3 *list,
                         struct ListHeader3 *element
){
  return ListFindElementPos1((struct ListHeader1 *)list,(struct ListHeader1 *)element);
}


/******************************************************************************
  FUNCTION
    Finds the last element in a list. 'list' may be NULL.
******************************************************************************/

void *ListLast3(struct ListHeader3 *list){
	while(list!=NULL){
		if(list->next==NULL) break;
		list=list->next;
	}
	return list;
}

void *ListLast1(struct ListHeader1 *list){
	while(list!=NULL){
		if(list->next==NULL) break;
		list=list->next;
	}
	return list;
}




/************************************************************
  FUNCTION
    Increase 'l'->p.line by 'toinsert' in every element in
    the 'to' list, starting from 'l'. 'toinsert' may be
    negative. Calls 'Insert_Lines_extra' for each element
    unless it is NULL. Removes the appropriate elements if
    'toinsert' is negative. Called from lines.c.
************************************************************/

void List_InsertLines3(
	void *to,
	struct ListHeader3 *l,
	int line,
	int toinsert,
	void (*Insert_Lines_extra)(void *to,struct ListHeader3 *l,int line,int toinsert)
){
	struct ListHeader3 *temp;

	while(l!=NULL){
		if(Insert_Lines_extra!=NULL){
			(*Insert_Lines_extra)(to,l,line,toinsert);
		}
		if(l->p.line>=line){
			if(l->p.line<line-toinsert){
				temp=l->next;
				ListRemoveElement3(to,l);
				l=temp;
				continue;
			}
			l->p.line+=toinsert;
		}

		l=l->next;
	}

}


void List_InsertPlaceLen3(
	struct Blocks *block,
	void *to,
	struct ListHeader3 *l,
	float place,
	float toplace,
	void (*Insert_PlaceLen_extra)(struct Blocks *block,void *to,struct ListHeader3 *l,float place,float toplace)
){
	struct ListHeader3 *temp;

	while(l!=NULL){
		temp=l->next;
		if(Insert_PlaceLen_extra!=NULL){
			(*Insert_PlaceLen_extra)(block,to,l,place,toplace);
		}
		if(GetfloatFromPlacement(&l->p)>=place){
			if(GetfloatFromPlacement(&l->p)<place-toplace){
				ListRemoveElement3(to,l);
			}else{
				PlaceAddfloat(&l->p,toplace);
				if( ! PlaceLegal(block,&l->p)){
					ListRemoveElement3(to,l);
				}
			}
		}
		l=temp;
	}

}

//void *ListCopy3(












