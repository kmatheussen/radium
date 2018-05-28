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

#ifndef VECTOR_PROC_H
#define VECTOR_PROC_H

#include <string.h>

static inline void VECTOR_ensure_space_for_one_more_element(vector_t *v){
  const int num_elements = v->num_elements;

  if(num_elements==v->num_elements_allocated){
    if(num_elements==0){
#ifdef TEST_VECTOR
      const int num_init = 2;
#else
      const int num_init = 8;
#endif
      v->num_elements_allocated = num_init;
      v->elements = (void**)talloc(num_init*(int)sizeof(void*));
    }else{
      const int num_elements_allocated = num_elements * 2;
      v->num_elements_allocated = num_elements_allocated;
      v->elements = (void**)talloc_realloc(v->elements,num_elements_allocated*(int)sizeof(void*));
    }
  }
  
}

// Might save some cpu cycles if creating a large vector with known size (firstmost in the memory allocator). It probably makes no practicaly difference though.
static inline vector_t VECTOR_create(int size){
  vector_t v = {0};
  v.num_elements = v.num_elements_allocated = size;
  v.elements = (void**)talloc(size*(int)sizeof(void*));
  return v;
}


static inline int VECTOR_push_back(vector_t *v, const void *element){
#if 0 //ifndef RELEASE
  R_ASSERT(element!=NULL);
#endif

  VECTOR_ensure_space_for_one_more_element(v);
  
  const int num_elements = v->num_elements;

  v->elements[num_elements] = (void*)element;
  v->num_elements = num_elements+1;
  
  return num_elements;
}

static inline void VECTOR_insert(vector_t *v, const void *element, int pos){
  int num_elements = v->num_elements;
  
  R_ASSERT(pos>=0 && pos<=num_elements);

  if (pos==num_elements) {
    
    VECTOR_push_back(v, element);

  } else if (v->num_elements_allocated == v->num_elements){
    
    const int num_elements_allocated = num_elements * 2;
    v->num_elements_allocated = num_elements_allocated;
    void **old_elements = v->elements;
    v->elements = (void**)talloc(num_elements_allocated*(int)sizeof(void*));
    
    if(pos>0)
      memcpy(v->elements, old_elements, pos*(int)sizeof(void*));
    
    v->elements[pos] = (void*)element;
    
    memcpy(&v->elements[pos+1], &old_elements[pos], sizeof(void*)*(num_elements - pos));

    v->num_elements = num_elements+1;
    
  } else {

    memmove(&v->elements[pos+1], &v->elements[pos], sizeof(void*)*(num_elements - pos));

    v->elements[pos] = (void*)element;

    v->num_elements = num_elements+1;

  }
}

static inline void VECTOR_push_front(vector_t *v, const void *element){
  VECTOR_insert(v, element, 0);
}

static inline void *VECTOR_get_r0(const vector_t *v, int num, const char *type){
  if (num < 0){
    RError("Can not use negative index for VECTOR_get. name: %s index: %d (size: %d)",type,num,v->num_elements);
    return NULL;
  }
  if (num>=v->num_elements)
    return NULL;
  
  return v->elements[num];
}
                         
static inline void *VECTOR_get(const vector_t *v, int num, const char *type){
  void *ret = VECTOR_get_r0(v, num, type);

  if (ret==NULL && num>=0){
    RError("There is no %s %d (size: %d)",type,num,v->num_elements);
    return NULL;
  }
  
  return v->elements[num];
}

extern LANGSPEC void VECTOR_reverse(vector_t *v);
extern LANGSPEC vector_t *VECTOR_move(vector_t *from);
extern LANGSPEC vector_t *VECTOR_copy(vector_t *from);
extern LANGSPEC void VECTOR_copy_elements(vector_t *from, int from_pos, int num_elements_to_copy, vector_t *to);
extern LANGSPEC void VECTOR_clean(vector_t *v);
extern LANGSPEC vector_t *VECTOR_append(vector_t *v1, const vector_t *v2);
extern LANGSPEC void VECTOR_delete(vector_t *v, int pos); //keeps order
extern LANGSPEC int VECTOR_find_pos(const vector_t *v, const void *element);
extern LANGSPEC bool VECTOR_is_in_vector(const vector_t *v, const void *element);
static inline bool VECTOR_contains(const vector_t *v, const void *element){
  return VECTOR_is_in_vector(v, element);
}
extern LANGSPEC void VECTOR_remove(vector_t *v, const void *element); //keeps order
extern LANGSPEC vector_t *VECTOR_list1_to_vector(const struct ListHeader1 *list);
extern LANGSPEC vector_t *VECTOR_list3_to_vector(const struct ListHeader3 *list);
extern LANGSPEC void VECTOR_insert_list3(vector_t *v, const struct ListHeader3 *element);
extern LANGSPEC void VECTOR_insert_place(vector_t *v, const Place *p);

static inline void *VECTOR_last(vector_t *v){
  if (v->num_elements==0)
    return NULL;
  else
    return v->elements[v->num_elements-1];
}

static inline void VECTOR_set(vector_t *v, int pos, void *element){
  R_ASSERT_RETURN_IF_FALSE(pos < v->num_elements);
  v->elements[pos] = element;
}

static inline void VECTOR_delete_last(vector_t *v){
  R_ASSERT_RETURN_IF_FALSE(v->num_elements > 0);
  v->num_elements--;
  v->elements[v->num_elements]=NULL;
}

//#ifdef _GNU_SOURCE
typedef  int (*vector_sort_callback_t) (const void *a, const void *b);

static void *g_sort_arg;

static inline int VECTOR_sort_comp(const void *a, const void *b){
  void *arg = g_sort_arg;
  const struct SeqBlock *s1 = ((const struct SeqBlock **)a)[0];
  const struct SeqBlock *s2 = ((const struct SeqBlock **)b)[0];
  vector_sort_callback_t callback = (vector_sort_callback_t)arg;
  return callback(s1, s2);
}

static inline void VECTOR_sort(vector_t *v, vector_sort_callback_t comp){
  R_ASSERT(THREADING_is_main_thread());
  
  if(v->num_elements==0) // v->elements can be NULL when this happens, and qsort expects first argument to be !=NULL.
    return;

  g_sort_arg = (void*)comp;
  
  qsort(v->elements, v->num_elements, sizeof(void*), VECTOR_sort_comp);
}

// Might be more efficient if there's a good chance v is sorted already, depending on the implementation of qsort.
static inline void VECTOR_sort2(vector_t *v, vector_sort_callback_t comp){
  for(int i=0;i<v->num_elements-1;i++)
    if (comp(v->elements[i], v->elements[i+1]) > 0){
      //printf("   NEEDS SORTING\n");
      VECTOR_sort(v, comp);
      return;
    }
}
//#endif


#ifdef __cplusplus
#define VECTOR_FOR_EACH(type,varname,vector) {                          \
  int iterator666;                                                      \
  const vector_t *vector666 = vector;                                         \
  for(iterator666=0;iterator666<(vector666)->num_elements;iterator666++){ \
  type varname = (type)((vector666)->elements[iterator666]);
#else
#define VECTOR_FOR_EACH(var,vector) {                                   \
  int iterator666;                                                      \
  const vector_t *vector666 = vector;                                         \
  for(iterator666=0;iterator666<(vector666)->num_elements;iterator666++){  \
  var = (vector666)->elements[iterator666];
#endif
#define END_VECTOR_FOR_EACH }}


#define ALL_SEQTRACKS_FOR_EACH()                                        \
  for(int seqiterator666=-1 ; seqiterator666<root->song->seqtracks.num_elements ; seqiterator666++){ \
    struct SeqTrack *seqtrack = seqiterator666==-1 ? root->song->block_seqtrack : (struct SeqTrack*)root->song->seqtracks.elements[seqiterator666];

#define END_ALL_SEQTRACKS_FOR_EACH }


#ifdef __cplusplus

template<typename T>
static inline QVector<T> VECTOR_get_qvector(const vector_t *v){
  QVector<T> ret;
  VECTOR_FOR_EACH(T, el, v){
    ret.push_back(el);
  }END_VECTOR_FOR_EACH;
  return ret;
}

#endif



#ifdef __cplusplus

#include "LockAsserter.hpp"

namespace radium{
  template <typename T> 
  class Vector_t {

    vector_t *v;
    
    LockAsserter lockAsserter;

  public:
    
    Vector_t(vector_t *v)
      : v(v)
    {
      LOCKASSERTER_EXCLUSIVE(&lockAsserter);
    }

    T *operator[](int i) const {
      return at(i);
    }
    
    T *at(int i) const {
      LOCKASSERTER_SHARED(&lockAsserter);
      
      return at_internal(i);
    }
    
private:
  
    T *at_internal(int i) const {
      R_ASSERT_RETURN_IF_FALSE2(i>=0, v->elements[0]);
      R_ASSERT_RETURN_IF_FALSE2(i<v->num_elements, v->elements[0]);
      
      return (T*)v->elements[i];
    }

public:

    const T* begin() const {
      LOCKASSERTER_SHARED(&lockAsserter);
      
      return (T*)&v->elements[0];
    }
    
    // This function can be called in parallell with the other const functions (i.e. the non-mutating ones).
    const T* end() const {
      LOCKASSERTER_SHARED(&lockAsserter);
      
      return (T*)&v->elements[v->num_elements];
    }
    
    int size(void) const {
      LOCKASSERTER_SHARED(&lockAsserter);
      
      return v->num_elements;
    }

    void push_back(T *t){
      LOCKASSERTER_EXCLUSIVE(&lockAsserter);
      VECTOR_push_back(t);
    }

  };

//#define V_t Vector_t
}

#endif


#endif
