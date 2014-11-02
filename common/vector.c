/* Copyright 2012 Kjetil S. Matheussen

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

#include <string.h>

#include "nsmtracker.h"

#include "vector_proc.h"

void VECTOR_reverse(vector_t *v){
  int size=v->num_elements;
  int i;
  for(i=0;i<size/2;i++){
    void *temp = v->elements[size-i-1];
    v->elements[size-i-1] = v->elements[i];
    v->elements[i] = temp;
  }
}

vector_t *VECTOR_copy(vector_t *from){
  vector_t *to=talloc(sizeof(vector_t));

  to->num_elements_allocated = from->num_elements_allocated;
  to->num_elements = from->num_elements;

  to->elements=talloc(from->num_elements_allocated*sizeof(void*));
  memcpy(to->elements,from->elements,from->num_elements*sizeof(void*));

  return to;
}

void VECTOR_clean(vector_t *v){
  v->num_elements = 0;
  memset(v->elements,0,v->num_elements*sizeof(void*));
}

vector_t *VECTOR_append(vector_t *v1, vector_t *v2){
  int i;
  for(i=0;i<v2->num_elements;i++)
    VECTOR_push_back(v1,v2->elements[i]);
  return v1;
}

void VECTOR_delete(vector_t *v, int pos){
  int i;
  v->num_elements--;

  for(i=pos;i<v->num_elements;i++)
    v->elements[i]=v->elements[i+1];

  v->elements[v->num_elements]=NULL;
}

int VECTOR_find_pos(vector_t *v, const void *element){
  int i;
  for(i=0;i<v->num_elements;i++)
    if(v->elements[i]==element)
      return i;
  return -1;
}

bool VECTOR_is_in_vector(vector_t *v, const void *element){
  return VECTOR_find_pos(v,element)>=0;
}

void VECTOR_remove(vector_t *v, const void *element){
  int pos=VECTOR_find_pos(v,element);
  if(pos==-1){
    RError("Element %p not found in vector %p\n",element,v);
    return;
  }
  VECTOR_delete(v,pos);
}

vector_t *VECTOR_list1_to_vector(struct ListHeader1 *list){
  vector_t *v = talloc(sizeof(vector_t));
  while(list!=NULL){
    VECTOR_push_back(v,list);
    list=list->next;
  }
  return v;
}

vector_t *VECTOR_list3_to_vector(struct ListHeader3 *list){
  return VECTOR_list1_to_vector((struct ListHeader1*)list);
}
