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
#include "placement_proc.h"

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

vector_t *VECTOR_list1_to_vector(const struct ListHeader1 *list){
  vector_t *v = talloc(sizeof(vector_t));
  while(list!=NULL){
    VECTOR_push_back(v,list);
    list=list->next;
  }
  return v;
}

vector_t *VECTOR_list3_to_vector(const struct ListHeader3 *list){
  return VECTOR_list1_to_vector((const struct ListHeader1*)list);
}

void VECTOR_insert_list3(vector_t *v, const struct ListHeader3 *element){
  const Place *p = &element->p;
  int i;

  for(i=0 ; i<v->num_elements ; i++){
    struct ListHeader3 *l3 = (struct ListHeader3*)v->elements[i];
    if (PlaceLessThan(p, &l3->p)) {
      VECTOR_insert(v, element, i);
      return;
    }
  }

  VECTOR_push_back(v, element);
}


#ifdef TEST_VECTOR

#include <stdarg.h>
#include <assert.h>

void EndProgram(void){
  printf("ENDPROGRAM called\n");
}

void RError(const char *fmt,...){
  char message[1000];
  va_list argp;
  
  va_start(argp,fmt);
  /*	vfprintf(stderr,fmt,argp); */
  vsprintf(message,fmt,argp);
  va_end(argp);

  fprintf(stderr,"error: %s\n",message);
}

#define TESTCODE(MAKE,GET) {                    \
  vector_t v = {0};                             \
                                                \
  VECTOR_insert(&v, MAKE(0), 0);                \
  printf("s1: %d %d\n",v.num_elements,GET(0));  \
                                                \
  VECTOR_insert(&v, MAKE(1), 1);                \
  printf("s2: %d %d\n",v.num_elements,GET(1));  \
                                                \
  VECTOR_insert(&v, MAKE(3), 2);                \
  printf("s3: %d %d\n",v.num_elements,GET(2));  \
                                                \
  VECTOR_insert(&v, MAKE(2), 2);                        \
  printf("s4: %d %p\n",v.num_elements,v.elements[3]);           \
  printf("s4: %d %d,%d\n",v.num_elements, GET(2), GET(3));      \
                                                                \
  VECTOR_insert(&v, MAKE(-1), 0);                               \
  printf("s5: %d %d\n",v.num_elements, GET(0));                 \
                                                                \
  assert(v.num_elements==5);                                    \
                                                                \
  assert(GET(0)==-1);                                           \
  assert(GET(1)==0);                                            \
  printf("2: %d\n",GET(2));                                     \
  printf("3p: %p\n",v.elements[3]);                             \
  printf("3: %d\n",GET(3));                                     \
  assert(GET(2)==1);                                            \
  assert(GET(3)==2);                                            \
  assert(GET(4)==3);                                            \
  }                                                             



typedef struct {
  int i;
} I;

static I *i(int i){
  I *ret = talloc(sizeof(I));
  ret->i=i;
  return ret;
}


#define MAKE_I(n) i(n)
#define GET_I(n) ((I*)v.elements[n])->i

static void test_insert(void) TESTCODE(MAKE_I, GET_I)


struct ListHeader3 *l3(int i){
  struct ListHeader3 *ret = talloc(sizeof(struct ListHeader3));
  ret->p.line = i;
  return ret;
}

#define MAKE_L3(n) l3(n)
#define GET_L3(n) ((struct ListHeader3*)v.elements[n])->p.line

static void test_insert_list3(void) TESTCODE(MAKE_L3, GET_L3)


int main(void){

  test_insert();
  test_insert_list3();

  printf("Success, no errors\n");

  return 0;
}

#endif // TEST_VECTOR
