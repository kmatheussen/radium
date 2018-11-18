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


vector_t g_global_roots = {0};


void VECTOR_reverse(vector_t *v){
  int size=v->num_elements;
  int i;
  for(i=0;i<size/2;i++){
    void *temp = v->elements[size-i-1];
    v->elements[size-i-1] = v->elements[i];
    v->elements[i] = temp;
  }
}

void VECTOR_clean(vector_t *v){
  if (v->num_elements_allocated > 0 && v->num_elements>0)
    memset(v->elements,0,v->num_elements*sizeof(void*)); // Not necessary, but since we use a GC we might avoid memory leaks this way.
  
  v->num_elements = 0;
}

vector_t *VECTOR_move(vector_t *from){
  vector_t *to = talloc(sizeof(vector_t));

  to->elements               = from->elements;
  to->num_elements_allocated = from->num_elements_allocated;
  to->num_elements           = from->num_elements;

  from->elements               = NULL;
  from->num_elements_allocated = 0;
  from->num_elements           = 0;
  
  return to;
}

vector_t *VECTOR_copy(const vector_t *from){
  vector_t *to=talloc(sizeof(vector_t));

  to->num_elements_allocated = from->num_elements_allocated;
  to->num_elements = from->num_elements;

  if (from->num_elements_allocated > 0){    
    to->elements=talloc(from->num_elements_allocated*sizeof(void*));
    memcpy(to->elements,from->elements,from->num_elements*sizeof(void*));
  }
  
  return to;
}

void VECTOR_copy_elements(const vector_t *from, int from_pos, int num_elements_to_copy, vector_t *to){
  R_ASSERT(to->num_elements==0); // A more advanced VECTOR_copy_elements function is not needed yet.
  
  R_ASSERT(from_pos + num_elements_to_copy <= from->num_elements);

  if (to->num_elements_allocated < num_elements_to_copy) {
    to->elements = talloc(num_elements_to_copy * sizeof(void*));
    to->num_elements_allocated = num_elements_to_copy;
  }

  to->num_elements = num_elements_to_copy;

  memcpy(to->elements, &from->elements[from_pos], sizeof(void*)*num_elements_to_copy);
}

vector_t *VECTOR_append(vector_t *v1, const vector_t *v2){
  int i;
  for(i=0;i<v2->num_elements;i++)
    VECTOR_push_back(v1,v2->elements[i]);
  return v1;
}

// must keep order
void VECTOR_delete(vector_t *v, int pos){
  int i;
  v->num_elements--;

  for(i=pos;i<v->num_elements;i++)
    v->elements[i]=v->elements[i+1];

  v->elements[v->num_elements]=NULL;
}

#if 1
// I think element is usually in the end of the vector, not the beginning.
int VECTOR_find_pos(const vector_t *v, const void *element){
  for(int i=v->num_elements-1 ; i>=0 ; i--)
    if(v->elements[i]==element)
      return i;
  return -1;
}
#else
int VECTOR_find_pos(const vector_t *v, const void *element){
  int i;
  for(i=0;i<v->num_elements;i++)
    if(v->elements[i]==element)
      return i;
  return -1;
}
#endif

bool VECTOR_is_in_vector(const vector_t *v, const void *element){
  return VECTOR_find_pos(v,element)>=0;
}

// must keep order
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

  for(i=0 ; i<v->num_elements ; i++){ // could be optimized by using binary search, but binary search is hard to get correct. That speedup is not needed for now anyway.
    struct ListHeader3 *l3 = (struct ListHeader3*)v->elements[i];
    if (PlaceLessThan(p, &l3->p)) {
      VECTOR_insert(v, element, i);
      return;
    }
  }

  VECTOR_push_back(v, element);
}

void VECTOR_insert_place(vector_t *v, const Place *p){
  int i;

  for(i=0 ; i<v->num_elements ; i++){ // could be optimized by using binary search, but binary search is hard to get correct. That speedup is not needed for now anyway.
    Place *element = v->elements[i];
    if (PlaceLessThan(p, element)) {
      VECTOR_insert(v, p, i);
      return;
    }
  }

  VECTOR_push_back(v, p);
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
  vsnprintf(message,998,fmt,argp);
  va_end(argp);

  fprintf(stderr,"error: %s\n",message);
}

void RWarning(const char *fmt,...){
  abort();
}

bool PLAYER_current_thread_has_lock(void){
  return false;
}

bool THREADING_is_main_thread(void){
  return true;
}

void CRASHREPORTER_send_assert_message(enum Crash_Type crash_type, const char *fmt,...){
  abort();
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
  assert(v.elements[5]==NULL);                                  \
                                                                \
  VECTOR_delete(&v, 0);                                         \
  assert(GET(0)==0);                                            \
  assert(GET(1)==1);                                            \
  assert(GET(2)==2);                                            \
  assert(GET(3)==3);                                            \
  assert(v.elements[4]==NULL);                                  \
                                                                \
  VECTOR_delete(&v, 3);                                         \
  assert(GET(0)==0);                                            \
  assert(GET(1)==1);                                            \
  assert(GET(2)==2);                                            \
  assert(v.elements[3]==NULL);                                  \
                                                                \
  VECTOR_remove(&v, v.elements[1]);                             \
  assert(GET(0)==0);                                            \
  assert(GET(1)==2);                                            \
  assert(v.elements[2]==NULL);                                  \
                                                                \
  VECTOR_remove(&v, v.elements[0]);                             \
  VECTOR_remove(&v, v.elements[0]);                             \
  assert(v.elements[0]==NULL);                                  \
  assert(v.elements[1]==NULL);                                  \
  assert(v.elements[2]==NULL);                                  \
                                                                \
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
