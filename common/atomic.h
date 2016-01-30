

#define DEFINE_ATOMIC(type, name) \
  type name##_atomic

#define ATOMIC_SET(name, val) \
  __atomic_store_n (&(name##_atomic), (val), __ATOMIC_SEQ_CST)
                   
#define ATOMIC_GET(name) \
  __atomic_load_n (&(name##_atomic), __ATOMIC_SEQ_CST)

#define ATOMIC_COMPARE_EXCHANGE(name, expected_p, new_value) \
  __atomic_compare_exchange_n (&(name##_atomic), expected_p, new_value, true, __ATOMIC_SEQ_CST, __ATOMIC_SEQ_CST)

// Writes 'new' to 'name' if 'name' has the value 'old'. Returns true if successful.
#define ATOMIC_COMPARE_AND_SWAP(name, old, new)                             \
  __atomic_compare_exchange_n (&(name##_atomic), expected_p, new_value, true, __ATOMIC_SEQ_CST, __ATOMIC_SEQ_CST)


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
   atomic_compare_and_swap_bool(bool *variable,
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

static inline bool atomic_compare_and_swap_bool(bool *variable, bool old_value, bool new_value){
  return __atomic_compare_exchange_n (variable, &old_value, new_value, true, __ATOMIC_SEQ_CST, __ATOMIC_SEQ_CST);
}
                            
#define ATOMIC_SET_RETURN_OLD(name, val) \
  __atomic_exchange_n (&(name##_atomic), val, __ATOMIC_SEQ_CST)

#define ATOMIC_ADD_RETURN_OLD(name, how_much)                           \
  __atomic_fetch_add (&(name##_atomic), how_much, __ATOMIC_SEQ_CST)


#define DEFINE_SPINLOCK(name) \
  DEFINE_ATOMIC(bool, name) = false;

#define SPINLOCK_OBTAIN(name)                                           \
  while(atomic_compare_and_swap_bool(&(name##_atomic), false, true)==false)

#if 0
for(;;){                                                                \
    bool expected = true;                                               \
    __atomic_compare_exchange_n (&(name##_atomic), &expected, true, true, __ATOMIC_SEQ_CST, __ATOMIC_SEQ_CST); \
    if(expected==true)                                                  \
      break;                                                            \
  };
#endif
  
#define SPINLOCK_RELEASE(name) \
  ATOMIC_SET(name, false)


#define SPINLOCK_IS_OBTAINED(name) \
  ATOMIC_GET(spinlock)==true

  
