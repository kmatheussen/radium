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
#include "ratio_funcs.h"
#include <stdarg.h>

#include "list_proc.h"



/* Next two structs.. It only looks a bit nicer using them I think. */

struct ListHeaderPointer0{
	struct ListHeader0 *root;
};

struct ListHeaderPointer1{
	struct ListHeader1 *root;
};

struct ListHeaderPointerP{
	struct ListHeaderP *root;
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

        ValidatePlace(&element->p);
                
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
                R_ASSERT_RETURN_IF_FALSE(element!=NULL);
		element->next=listroot->root;
		listroot->root=element;
	}else{
                R_ASSERT_RETURN_IF_FALSE(element!=NULL);
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

        ValidatePlace(&element->p);
        
	if(element==NULL) return;

	while(temp!=NULL){
		if(PlaceGreaterThan(&temp->p,&element->p)) break;
		prev=temp;
		temp=temp->next;
	}

	if(prev==NULL){
                R_ASSERT_RETURN_IF_FALSE(element!=NULL);
		element->next=listroot->root;
		listroot->root=element;
	}else{
                R_ASSERT_RETURN_IF_FALSE(element!=NULL);
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
/*
void ListAddElement0(
                     void *voidlistroot,
                     struct ListHeader0 *element
){
  R_ASSERT_RETURN_IF_FALSE(voidlistroot!=NULL);
  R_ASSERT_RETURN_IF_FALSE(element!=NULL);
  
  struct ListHeaderPointer0 *listroot=voidlistroot;
  struct ListHeader0 *old_first=listroot->root;

  listroot->root = element;
  element->next = old_first;
}
*/

void ListAddElement1(
	void *voidlistroot,
	struct ListHeader1 *element
){
	struct ListHeaderPointer1 *listroot=voidlistroot;
	struct ListHeader1 *temp=listroot->root;
	struct ListHeader1 *prev=NULL;

	if(element==NULL) return;

	while(temp!=NULL){
		if(temp->num >= element->num) break;
		prev=temp;
		temp=temp->next;
	}

	if(prev==NULL){
                R_ASSERT_RETURN_IF_FALSE(element!=NULL);
		element->next=listroot->root;
		listroot->root=element;
	}else{
                R_ASSERT_RETURN_IF_FALSE(element!=NULL);
		element->next=prev->next;
		prev->next=element;
	}
}

void ListAddElementP(
	void *voidlistroot,
	struct ListHeaderP *element
){
	struct ListHeaderPointerP *listroot=voidlistroot;
	struct ListHeaderP *temp=listroot->root;
	struct ListHeaderP *prev=NULL;

	if(element==NULL) return;

	while(temp!=NULL){
		if(temp->time >= element->time) break;
		prev=temp;
		temp=temp->next;
	}

	if(prev==NULL){
                R_ASSERT_RETURN_IF_FALSE(element!=NULL);
		element->next=listroot->root;
		listroot->root=element;
	}else{
                R_ASSERT_RETURN_IF_FALSE(prev!=NULL);
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
		if(temp->num > element->num) break;
		prev=temp;                
		temp=temp->next;
	}

	if(prev==NULL){
                R_ASSERT_RETURN_IF_FALSE(element!=NULL);
		element->next=listroot->root;
		listroot->root=element;
	}else{
                R_ASSERT_RETURN_IF_FALSE(element!=NULL);
		element->next=prev->next;
		prev->next=element;
	}
}

void ListAddElementP_a(
	void *voidlistroot,
	struct ListHeaderP *element
){
	struct ListHeaderPointerP *listroot=voidlistroot;
	struct ListHeaderP *temp=listroot->root;
	struct ListHeaderP *prev=NULL;

	if(element==NULL) return;

	while(temp!=NULL){
		if(temp->time > element->time) break;
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

void ListRemoveElement3_fromNum(
                                void *voidlistroot,
                                int num
                                )
{
	struct ListHeaderPointer3 *listroot=voidlistroot;
	struct ListHeader3 *temp=listroot->root;
	struct ListHeader3 *prev=NULL;
        int n = 0;
        
	if(num==0){
          listroot->root=temp->next;
	}else{
          while(n!=num){
            prev=temp;
            R_ASSERT_RETURN_IF_FALSE(temp!=NULL);
            temp=temp->next;
            n++;
          }
          R_ASSERT_RETURN_IF_FALSE(prev!=NULL);
          R_ASSERT_RETURN_IF_FALSE(temp!=NULL);
          prev->next=temp->next;
	}
}

void ListRemoveElement1_fromNum(
	void *voidlistroot,
        int num
){
  ListRemoveElement3_fromNum(voidlistroot,num);
}

void ListRemoveElement1(
	void *voidlistroot,
	const struct ListHeader1 *element
){
	struct ListHeaderPointer1 *listroot=voidlistroot;
	struct ListHeader1 *temp=listroot->root;
	struct ListHeader1 *prev=NULL;

	if(temp==element){
                R_ASSERT_RETURN_IF_FALSE(element!=NULL);
		listroot->root=element->next;
	}else{
		while(temp!=element){
                        R_ASSERT_RETURN_IF_FALSE(temp!=NULL);
			prev=temp;
			temp=temp->next;

		}
                R_ASSERT_RETURN_IF_FALSE(prev!=NULL);
                R_ASSERT_RETURN_IF_FALSE(element!=NULL);
		prev->next=element->next;
	}
}

void ListRemoveElement3(
	void *voidlistroot,
	const struct ListHeader3 *element
){
        ValidatePlace(&element->p);
        
	ListRemoveElement1(voidlistroot,(struct ListHeader1 *)element);
}

/*****************************************************************************
  FUNCTION
    Removes all elements in a list that is greater or equal to p1, and
    less than p2.
******************************************************************************/
void ListRemoveElements3(
	void *voidlistroot,
	const Place *p1,
	const Place *p2
){
	struct ListHeaderPointer3 *listroot=voidlistroot;
	struct ListHeader3 *l=listroot->root;
	struct ListHeader3 *temp;

        ValidatePlace(p1);
        ValidatePlace(p2);
        
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
    Moves the element to new place 'p', or closest legal position.
    A "legal position" means that it can not be positioned before the
    previous element, or after the next element. In addition, it can not be
    position at the same position as the previous element or next element
    ("ns" means "not same").

    The value for 'firstlegalpos' is only used if prev element is NULL.
    The value for 'lastlegalpos' is only used if next element is NULL.
******************************************************************************/
void ListMoveElement3_ns(
                         const void *voidlistroot,
                         struct ListHeader3 *element,
                         const Place *newplace,
                         const Place *firstlegalpos,
                         const Place *lastlegalpos
){
  const struct ListHeaderPointer3 *listroot=voidlistroot;
  struct ListHeader3 *prev=NULL;
  struct ListHeader3 *next = element->next;
  struct ListHeader3 *l=listroot->root;

  ValidatePlace(&element->p);
  ValidatePlace(newplace);
  ValidatePlace(firstlegalpos);
  ValidatePlace(lastlegalpos);
  
  while(l!=element){    
    prev = l;
    R_ASSERT_RETURN_IF_FALSE(l!=NULL);
    l=l->next;
  }

  Place firstlegalpos2;
  if (prev!=NULL)
    PlaceFromLimit(&firstlegalpos2, &prev->p);
  else
    PlaceCopy(&firstlegalpos2, firstlegalpos);

  Place lastlegalpos2;
  if (next!=NULL)
    PlaceTilLimit(&lastlegalpos2, &next->p);
  else
    PlaceCopy(&lastlegalpos2, lastlegalpos);

  element->p = *PlaceBetween(&firstlegalpos2, newplace, &lastlegalpos2);
}

struct ListHeader3 *ListMoveElement3_FromNum_ns(
                                                const void *voidlistroot,
                                                int num,
                                                const Place *newplace,
                                                const Place *firstlegalpos,
                                                const Place *lastlegalpos
){
  const struct ListHeaderPointer3 *listroot=voidlistroot;

  ValidatePlace(newplace);
  ValidatePlace(firstlegalpos);
  ValidatePlace(lastlegalpos);

  struct ListHeader3 *element = ListFindElement3_num(listroot->root, num);
  if (element!=NULL)
    ListMoveElement3_ns(voidlistroot, element, newplace, firstlegalpos, lastlegalpos);
  
  return element;
}


/*****************************************************************************
  FUNCTION
    Adds an element only if the list doesn't already contain an element
    with the same placement attributes.
******************************************************************************/
int ListAddElement3_ns(
	void *voidlistroot,
	struct ListHeader3 *element
){
	if(element==NULL) return -1;

        ValidatePlace(&element->p);
          
        struct ListHeaderPointer3 *listroot=voidlistroot;

	ListAddElement3(listroot,element);
	if(element->next!=NULL)
		if(PlaceEqual(&element->p,&element->next->p)){
			ListRemoveElement3(listroot,element);
			return -1;
		}

        struct ListHeader3 *list=listroot->root;
	return ListPosition3(list,element);
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


void *ListFindElement1(const struct ListHeader1 *list,NInt num){
	NInt lastnum;

	while(list!=NULL && list->num<=num){
		lastnum=list->num;
		if(list->num==num) return (void*)list;
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

void *ListFindElement1_r0(const struct ListHeader1 *list,NInt num){

	while(list!=NULL && list->num<=num){
                if(list->num==num) return (void*)list;
		list=list->next;
	}

	return NULL;
}

void *ListFindElement1_num_r0(
	const struct ListHeader1 *element,
	NInt num
){
  if (num < 0)
    return NULL;
  
	int lokke;

	for(lokke=0;lokke<num && element!=NULL;lokke++)
          if (element==NULL)
            return NULL;
          else
            element=element->next;

	return (void*)element;
}

/**********************************************************************
  FUNCTION
    Finds the first element in the list at or after place 'place' and
    counter 'counter' for dividor 'dividor'. If no element is found
    after 'place' and 'counter/dividor', NULL is returned.
**********************************************************************/
void *ListFindElement3(
	const struct ListHeader3 *element,
	const Place *placement
){
  ValidatePlace(placement);
  
	for(;element!=NULL;element=element->next)
          if(p_Greater_Or_Equal(element->p,*placement))
            return (void*)element;

	return NULL;
}

void *ListFindElement3_num(
	struct ListHeader3 *element,
	NInt num
){
  if (num < 0)
    return NULL;
  
	int lokke;

	for(lokke=0;lokke<num && element!=NULL;lokke++)
		element=element->next;

	return element;
}

void *ListFindElement3_num_r0(
	struct ListHeader3 *element,
	NInt num
){
  if (num < 0)
    return NULL;
  
	int lokke;

	for(lokke=0;lokke<num && element!=NULL;lokke++)
          if (element==NULL)
            return NULL;
          else
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

bool isInList0(
	struct ListHeader0 *liststart,
	struct ListHeader0 *element
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
	while(list!=element){
          R_ASSERT_RETURN_IF_FALSE2(list!=NULL, element);
          prev=list;
          list=list->next;
        }

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
void CutListAt(void *listroot,const Place *place){
	struct ListHeaderPointer3 *root=(struct ListHeaderPointer3 *)listroot;
	struct ListHeader3 *element=root->root;

        ValidatePlace(place);
          
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
void CutListAt_a(void *listroot,const Place *place){
  ValidatePlace(place);
          
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

int ListPosition3(struct ListHeader3 *list,
                  struct ListHeader3 *element
                  )
{
  ValidatePlace(&element->p);
          
  int ret = 0;
  while(list!=element) {
    ret++;
    R_ASSERT_RETURN_IF_FALSE2(list!=NULL, 0);
    list = list->next;
  }
  return ret;
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

NInt ListFindNumElements0(
	struct ListHeader0 *list
){
	NInt ret=0;
	while(list!=NULL){
		list=list->next;
		ret++;
	}
	return ret;
}

NInt ListFindNumElements1(
	struct ListHeader1 *list
){
  	return ListFindNumElements0((struct ListHeader0 *)list);
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
                R_ASSERT_RETURN_IF_FALSE2(list!=NULL, 0);
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

Place *ListLastPlace3(struct ListHeader3 *list){
	while(list!=NULL){
		if(list->next==NULL) break;
		list=list->next;
	}
        if (list==NULL)
          return NULL;
        else
          return &list->p;
}

void *ListLast3(struct ListHeader3 *list){
	while(list!=NULL){
		if(list->next==NULL) break;
		list=list->next;
	}
	return list;
}

void *ListSecondLast3(struct ListHeader3 *list){
  if(list==NULL)
    return NULL;
  
  if(list->next==NULL)
    return NULL;
          
  while(list->next!=NULL){
    
    if (list->next->next==NULL)
      return list;
    
    list=list->next;
  }

  R_ASSERT(false);
  return NULL;
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
                                R_ASSERT_RETURN_IF_FALSE(l!=NULL);
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


void List_InsertRatio3(
	void *to,
	struct ListHeader3 *l,
	const Place place,
        const Ratio toinsert,
	void (*Insert_Ratio_extra)(void *to,struct ListHeader3 *l, const Place place, const Ratio toinsert)
){

        const Ratio start_place = make_ratio_from_place(place);
        const Ratio new_place = RATIO_add(start_place, toinsert);
          
	while(l!=NULL){

          if(Insert_Ratio_extra!=NULL){
            (*Insert_Ratio_extra)(to, l, place, toinsert);
          }

          Ratio old_place = make_ratio_from_place(l->p);
            
          if(RATIO_greater_or_equal_than(old_place, start_place)) {
            
            if(RATIO_less_than(old_place, new_place)){
              
              R_ASSERT_RETURN_IF_FALSE(l!=NULL);
              
              struct ListHeader3 *temp=l->next;
              
              ListRemoveElement3(to,l);
              
              l=temp;
              
              continue;
              
            }

            l->p = make_place_from_ratio(new_place);
            
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

	while(l!=NULL){
                struct ListHeader3 *next = l->next;

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
                
		l = next;
	}

}

