
#ifndef RADIUM_COMMON_ATOMIC_H
#define RADIUM_COMMON_ATOMIC_H

#include <stdbool.h>
#include <stdint.h>


#define ATOMIC_NAME(name) \
  name##_atomic

#define DEFINE_ATOMIC(type, name) \
  type ATOMIC_NAME(name)

#define ATOMIC_SET(name, val) \
  __atomic_store_n (&(ATOMIC_NAME(name)), (val), __ATOMIC_SEQ_CST)
                   
#define ATOMIC_SET_RELAXED(name, val) \
  __atomic_store_n (&(ATOMIC_NAME(name)), (val), __ATOMIC_RELAXED)
                   
#define ATOMIC_GET(name) \
  __atomic_load_n (&(ATOMIC_NAME(name)), __ATOMIC_SEQ_CST)

/*
#define ATOMIC_GET2(name) \
  __atomic_load_n (&(name), __ATOMIC_SEQ_CST)
*/

#define ATOMIC_GET_ARRAY(name,pos)                         \
  __atomic_load_n (&(ATOMIC_NAME(name)[pos]), __ATOMIC_SEQ_CST)

#define ATOMIC_GET_RELAXED(name) \
  __atomic_load_n (&(ATOMIC_NAME(name)), __ATOMIC_RELAXED)


#define ATOMIC_SET_ARRAY(name, pos, val)                            \
  __atomic_store_n (&(ATOMIC_NAME(name)[pos]), (val), __ATOMIC_SEQ_CST)


/*
  __atomic_compare_exchange_n(type *ptr,
                              type *expected,
                              type desired,
                              bool weak,
                              int success_memorder,
                              int failure_memorder
                              );
  works like this:

  if (ptr==expected) {
     ptr = desired;
     return true;
  } else {
     expected = ptr;
     return false
  }
*/

/*
   atomic_compare_and_set_bool(bool *variable,
                               bool old_value,
                               bool new_value
                               );

   works like this:

   if (variable==old_value) {
      variable = new_value;
      return true;
   } else {
      return false;
   }
 */

static inline bool atomic_compare_and_set_bool(bool *variable, bool old_value, bool new_value){
  return __atomic_compare_exchange_n (variable, &old_value, new_value, true, __ATOMIC_SEQ_CST, __ATOMIC_SEQ_CST);
}
                            
static inline bool atomic_compare_and_set_int(int *variable, int old_value, int new_value){
  return __atomic_compare_exchange_n (variable, &old_value, new_value, true, __ATOMIC_SEQ_CST, __ATOMIC_SEQ_CST);
}

static inline bool atomic_compare_and_set_uint32(uint32_t *variable, uint32_t old_value, uint32_t new_value){
  return __atomic_compare_exchange_n (variable, &old_value, new_value, true, __ATOMIC_SEQ_CST, __ATOMIC_SEQ_CST);
}

#define ATOMIC_COMPARE_AND_SET_BOOL(name, old_value, new_value) \
  atomic_compare_and_set_bool(&ATOMIC_NAME(name), old_value, new_value)

#define ATOMIC_COMPARE_AND_SET_INT(name, old_value, new_value) \
  atomic_compare_and_set_int(&ATOMIC_NAME(name), old_value, new_value)

#define ATOMIC_COMPARE_AND_SET_UINT32(name, old_value, new_value) \
  atomic_compare_and_set_uint32(&ATOMIC_NAME(name), old_value, new_value)

#define ATOMIC_SET_RETURN_OLD(name, val) \
  __atomic_exchange_n (&ATOMIC_NAME(name), val, __ATOMIC_SEQ_CST)

#define ATOMIC_ADD_RETURN_OLD(name, how_much)                           \
  __atomic_fetch_add (&ATOMIC_NAME(name), how_much, __ATOMIC_SEQ_CST)

#define ATOMIC_ADD(name, how_much) ATOMIC_ADD_RETURN_OLD(name, how_much)

/*
#define ATOMIC_ADD_RETURN_OLD2(name, how_much)                           \
  __atomic_fetch_add (&(name), how_much, __ATOMIC_SEQ_CST)

#define ATOMIC_ADD2(name, how_much) ATOMIC_ADD_RETURN_OLD2(name, how_much)
*/

// doesn't work with bool!
#define ATOMIC_ADD_RETURN_NEW(name, how_much)                           \
  (__atomic_fetch_add (&ATOMIC_NAME(name), how_much, __ATOMIC_SEQ_CST) + how_much)

#define DEFINE_SPINLOCK_NOINIT(name) \
  DEFINE_ATOMIC(bool, name)

#define INIT_SPINLOCK(name) \
  ATOMIC_SET(name, false)

#define DEFINE_SPINLOCK(name) \
  DEFINE_SPINLOCK_NOINIT(name) = false

#define SPINLOCK_OBTAIN(name)                                           \
  while(atomic_compare_and_set_bool(&ATOMIC_NAME(name), false, true)==false)

#define SPINLOCK_RELEASE(name) \
  ATOMIC_SET(name, false)


#define SPINLOCK_IS_OBTAINED(name) \
  ATOMIC_GET(spinlock)==true


/************** float ******************/
  
// These functions are suppressed from tsan
static inline void safe_float_write(float *pos, float value){
  *pos = value;
}

static inline void safe_volatile_float_write(volatile float *pos, float value){
  *pos = value;
}

static inline float safe_volatile_float_read(volatile float *pos){
  return *pos;
}

static inline float safe_float_read(float *pos){
  return *pos;
}

static inline void safe_int_write(int *pos, int value){
  *pos = value;
}

static inline int safe_int_read(int *pos){
  return *pos;
}

//#define ATOMIC_RELAXED_WRITE(var, value) __atomic_store_n (&var,value,  __ATOMIC_RELAXED) // careful. Probably never any point using.
//#define ATOMIC_RELAXED_READ(var) __atomic_load_n (&var, __ATOMIC_RELAXED) // careful. Probably never any point using.

#define ATOMIC_WRITE(var, value) __atomic_store_n (&var,value,  __ATOMIC_SEQ_CST)
#define ATOMIC_INC(var, how_much) __atomic_fetch_add (&var, how_much, __ATOMIC_SEQ_CST)
#define ATOMIC_READ(var) __atomic_load_n (&var,  __ATOMIC_SEQ_CST)


/************** pointers ******************/

#if 0
static inline void *safe_pointer_read(void **p){
  return __atomic_load_n(p, __ATOMIC_RELAXED);
}
#endif

static inline void *atomic_pointer_read(void **p){
  return __atomic_load_n(p, __ATOMIC_SEQ_CST);
}

static inline void atomic_pointer_write(void **p, void *v){
  __atomic_store_n(p, v, __ATOMIC_SEQ_CST);
}



/************** doubles ******************/

typedef double atomic_double_t;

#define ATOMIC_DOUBLE_GET(name) ({                                      \
      double result;                                                    \
      __atomic_load (&ATOMIC_NAME(name), &result, __ATOMIC_SEQ_CST);      \
      result;                                                           \
    })

#define ATOMIC_DOUBLE_SET(name,new_value) ({                            \
      double new_value_variable = new_value;                            \
      __atomic_store (&ATOMIC_NAME(name), &new_value_variable, __ATOMIC_SEQ_CST); \
    })


/*
// redhat gcc 5.3.1: "warning: parameter ‘atomic_double’ set but not used [-Wunused-but-set-parameter]"
static inline double atomic_double_read(const atomic_double_t *atomic_double){
  double result;
  __atomic_load(atomic_double, &result, __ATOMIC_SEQ_CST);
  return result;
}

// redhat gcc 5.3.1: "warning: parameter ‘atomic_double’ set but not used [-Wunused-but-set-parameter]"
static inline void atomic_double_write(atomic_double_t *atomic_double, double new_value){
  __atomic_store(atomic_double, &new_value, __ATOMIC_SEQ_CST);
}
*/



#ifdef __cplusplus

// Can be used if one thread set a set of variables, while another thread read the set of variables
// The writing thread will not block, while the reading thread might block.
// Note: I'm not 100% sure the code is correct, but it probably protects more than if it had not been used.
// Class should not be used if it is extremely important that it works correctly.
//
class SetSeveralAtomicVariables{
  DEFINE_ATOMIC(int, generation);
  DEFINE_ATOMIC(bool, is_writing);
                  
 public:
  
  SetSeveralAtomicVariables(){
    ATOMIC_SET(generation, 0);
    ATOMIC_SET(is_writing, false);
  }

  void write_start(void){
    ATOMIC_ADD(generation, 1);
    ATOMIC_SET(is_writing, true);
    ATOMIC_ADD(generation, 1);
  }

  void write_end(void){
    ATOMIC_ADD(generation, 1);
    ATOMIC_SET(is_writing, false);
    ATOMIC_ADD(generation, 1);
  }

  int read_start(void){
    while(ATOMIC_GET(is_writing)==true);
    
    return ATOMIC_GET(generation);
  }

  bool read_end(int read_start_generation){
    while(ATOMIC_GET(is_writing)==true);
    
    if (ATOMIC_GET(generation) == read_start_generation)
      return true;
    else
      return false;
  }
};

#endif


#endif
