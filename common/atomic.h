
#ifndef RADIUM_COMMON_ATOMIC_H
#define RADIUM_COMMON_ATOMIC_H

#define DEFINE_ATOMIC(type, name) \
  type name##_atomic

#define ATOMIC_SET(name, val) \
  __atomic_store_n (&(name##_atomic), (val), __ATOMIC_SEQ_CST)
                   
#define ATOMIC_SET_RELAXED(name, val) \
  __atomic_store_n (&(name##_atomic), (val), __ATOMIC_RELAXED)
                   
#define ATOMIC_GET(name) \
  __atomic_load_n (&(name##_atomic), __ATOMIC_SEQ_CST)

#define ATOMIC_GET_ARRAY(name,pos)                         \
  __atomic_load_n (&(name##_atomic[pos]), __ATOMIC_SEQ_CST)

#define ATOMIC_GET_RELAXED(name) \
  __atomic_load_n (&(name##_atomic), __ATOMIC_RELAXED)


#define ATOMIC_SET_ARRAY(name, pos, val)                            \
  __atomic_store_n (&(name##_atomic[pos]), (val), __ATOMIC_SEQ_CST)


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

#define ATOMIC_COMPARE_AND_SET_INT(name, old_value, new_value) \
  atomic_compare_and_set_int(&(name##_atomic), old_value, new_value)

#define ATOMIC_COMPARE_AND_SET_UINT32(name, old_value, new_value) \
  atomic_compare_and_set_uint32(&(name##_atomic), old_value, new_value)

#define ATOMIC_SET_RETURN_OLD(name, val) \
  __atomic_exchange_n (&(name##_atomic), val, __ATOMIC_SEQ_CST)

#define ATOMIC_ADD_RETURN_OLD(name, how_much)                           \
  __atomic_fetch_add (&(name##_atomic), how_much, __ATOMIC_SEQ_CST)

#define ATOMIC_ADD(name, how_much) ATOMIC_ADD_RETURN_OLD(name, how_much)

// doesn't work with bool!
#define ATOMIC_ADD_RETURN_NEW(name, how_much)                           \
  (__atomic_fetch_add (&(name##_atomic), how_much, __ATOMIC_SEQ_CST) + how_much)

#define DEFINE_SPINLOCK(name) \
  DEFINE_ATOMIC(bool, name) = false;

#define SPINLOCK_OBTAIN(name)                                           \
  while(atomic_compare_and_set_bool(&(name##_atomic), false, true)==false)

#define SPINLOCK_RELEASE(name) \
  ATOMIC_SET(name, false)


#define SPINLOCK_IS_OBTAINED(name) \
  ATOMIC_GET(spinlock)==true

// This function is suppressed from tsan
static inline void safe_float_write(float *pos, float value){
  *pos = value;
}

static inline void safe_volatile_float_write(volatile float *pos, float value){
  *pos = value;
}

static inline float safe_volatile_float_read(volatile float *pos){
  return *pos;
}

// This function is suppressed from tsan
static inline float safe_float_read(float *pos){
  return *pos;
}

static inline void safe_double_write(volatile double *pos, double value){
  *pos = value; // is this really atomic?
}

// This function is suppressed from tsan
static inline double safe_double_read(volatile double *pos){
  return *pos; // is this really atomic?
}

static inline void *safe_pointer_read(void **p){
  return __atomic_load_n(p, __ATOMIC_RELAXED);
}


#endif

