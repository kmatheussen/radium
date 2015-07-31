
#ifndef RADIUM_COMMON_VECTOR_HPP
#define RADIUM_COMMON_VECTOR_HPP

#include "LockAsserter.hpp"


namespace radium{


template <typename T> struct Vector{
  
private:
  int num_elements_max;
  int num_elements;
  
  T *next_elements;
  int next_num_elements_max;
  
  LockAsserter lockAsserter;
  
  
public:
  
  T *elements;
  
  Vector()
    : num_elements_max(4)
    , num_elements(0)
    , next_elements(NULL)
    , next_num_elements_max(0)
  {
    LOCKASSERTER_EXCLUSIVE(&lockAsserter);

    R_ASSERT(num_elements_max > 0);
    
    elements = (T*)calloc(num_elements_max, sizeof(T));
  }

  ~Vector(){
    LOCKASSERTER_EXCLUSIVE(&lockAsserter);
    
    free(elements);
    elements = NULL; // For debugging
  }

  // This function can be called in parallell with the other const functions (i.e. the non-mutating ones).
  T operator[](int i) const {
    LOCKASSERTER_SHARED(&lockAsserter);

    R_ASSERT(i>=0);
    R_ASSERT(i<num_elements);
    
    return elements[i];
  }

  // This function can be called in parallell with the other const functions (i.e. the non-mutating ones).
  const T* begin() const {
    LOCKASSERTER_SHARED(&lockAsserter);
    
    return &elements[0];
  }

  // This function can be called in parallell with the other const functions (i.e. the non-mutating ones).
  const T* end() const {
    LOCKASSERTER_SHARED(&lockAsserter);
    
    return &elements[num_elements];
  }

  // This function can be called in parallell with the other const functions (i.e. the non-mutating ones).
  bool is_empty(void) const {
    LOCKASSERTER_SHARED(&lockAsserter);
    
    return num_elements == 0;
  }

  // This function can be called in parallell with the other const functions (i.e. the non-mutating ones).
  int size(void) const {
    LOCKASSERTER_SHARED(&lockAsserter);
    
    return num_elements;
  }

  // NOT RT safe
  //
  // This function must _always or never_ be called before calling add. No mixing.
  //
  // This function CAN be called in parallell with the const functions (i.e. the non-mutating ones),
  // but it can not be called in parallel with itself or any other non-const/mutating function.
  // (it is not asserted that this function is not called in parallell with itself)
  void ensure_there_is_room_for_one_more_without_having_to_allocate_memory(void){
    LOCKASSERTER_SHARED(&lockAsserter);
    
    R_ASSERT(next_elements == NULL);
    
    int new_num_elements = num_elements+1;

    R_ASSERT(num_elements_max > 0);
      
    if (new_num_elements > num_elements_max) {

      next_num_elements_max = num_elements_max;
      
      while (new_num_elements > next_num_elements_max)
        next_num_elements_max *= 2;

      next_elements = (T*) calloc(sizeof(T), next_num_elements_max);
      memcpy(next_elements, elements, sizeof(T)*num_elements);      
    }
  }

private:
    
    void basic_add(T t){
      num_elements++;

      R_ASSERT(num_elements_max > 0);

      if (num_elements > num_elements_max) {        
        while (num_elements > num_elements_max)
          num_elements_max *= 2;

        elements = (T*) realloc(elements, sizeof(T) * num_elements_max);
      }
      
      elements[num_elements-1] = t;
    }

public:
  
  // Only RT safe if ensure_there_is_room_for_one_more_without_having_to_allocate_memory is called first.
  //
  // This function can NOT be called in parallell with other functions
  void add(T t){
    LOCKASSERTER_EXCLUSIVE(&lockAsserter);
    
    if (next_elements == NULL) {

      basic_add(t);
      
    } else {

      num_elements++;

      R_ASSERT(num_elements <= next_num_elements_max);
      
      free(elements);
      
      elements = next_elements;
      num_elements_max = next_num_elements_max;
      
      next_elements = NULL;
      next_num_elements_max = 0;

      elements[num_elements-1] = t;
    }
  }

  // NOT RT safe
  //
  // This function can NOT be called in parallell with other functions
  void append(Vector<T> *ts){
    LOCKASSERTER_EXCLUSIVE(&lockAsserter);
    
    R_ASSERT(next_elements == NULL);
    
    for (T t : *ts)
      basic_add(t);
  }

  // NOT RT safe
  //
  // This function can NOT be called in parallell with other functions
  void append(Vector<T> &ts){
    LOCKASSERTER_EXCLUSIVE(&lockAsserter);
    
    R_ASSERT(next_elements == NULL);
    
    for (T t : ts)
      basic_add(t);
  }
  
  // RT safe (except for the O(n) performance)
  //
  // This function can NOT be called in parallell with other functions
  void remove(T t){
    LOCKASSERTER_EXCLUSIVE(&lockAsserter);
    
    R_ASSERT(next_elements == NULL);
    
    int pos;
    
    for(pos=0 ; pos<num_elements ; pos++)
      if (elements[pos]==t)
        break;
    
    R_ASSERT(pos < num_elements);

    if (num_elements==1){
      R_ASSERT(pos==0);
      elements[pos] = (T)NULL;
    } else {
      elements[pos] = elements[num_elements-1];
    }

    num_elements--;
  }

  // RT safe
  //
  // This function can NOT be called in parallell with other functions
  void clear(void) {
    LOCKASSERTER_EXCLUSIVE(&lockAsserter);
    
    R_ASSERT(next_elements == NULL);
    num_elements = 0;
  }


};
}


#endif
