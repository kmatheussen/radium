
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

#define ATOMIC_GET_RELAXED(name) \
  __atomic_load_n (&(ATOMIC_NAME(name)), __ATOMIC_RELAXED)


/*
#define ATOMIC_GET2(name) \
  __atomic_load_n (&(name), __ATOMIC_SEQ_CST)
*/

#define ATOMIC_GET_ARRAY(name,pos)                         \
  __atomic_load_n (&(ATOMIC_NAME(name)[pos]), __ATOMIC_SEQ_CST)

#define ATOMIC_GET_ARRAY_RELAXED(name,pos)                         \
  __atomic_load_n (&(ATOMIC_NAME(name)[pos]), __ATOMIC_RELAXED)


#define ATOMIC_SET_ARRAY(name, pos, val)                            \
  __atomic_store_n (&(ATOMIC_NAME(name)[pos]), (val), __ATOMIC_SEQ_CST)

#define ATOMIC_SET_RETURN_OLD_ARRAY(name, pos, val)                         \
  __atomic_exchange_n (&ATOMIC_NAME(name)[pos], val, __ATOMIC_SEQ_CST)


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
                            
static inline bool atomic_compare_and_set_bool_relaxed(bool *variable, bool old_value, bool new_value){
  return __atomic_compare_exchange_n (variable, &old_value, new_value, true, __ATOMIC_RELAXED, __ATOMIC_RELAXED);
}
                            
static inline bool atomic_compare_and_set_int(int *variable, int old_value, int new_value){
  return __atomic_compare_exchange_n (variable, &old_value, new_value, true, __ATOMIC_SEQ_CST, __ATOMIC_SEQ_CST);
}

static inline bool atomic_compare_and_set_uint32(uint32_t *variable, uint32_t old_value, uint32_t new_value){
  return __atomic_compare_exchange_n (variable, &old_value, new_value, true, __ATOMIC_SEQ_CST, __ATOMIC_SEQ_CST);
}

static inline bool atomic_compare_and_set_pointer(void **variable, void *old_value, void *new_value){
  return __atomic_compare_exchange_n (variable, &old_value, new_value, true, __ATOMIC_SEQ_CST, __ATOMIC_SEQ_CST);
}

#ifdef __cplusplus
// Were doing type punning of float below.
static_assert (sizeof(float) == 4, "Size of float is not correct");

static inline void atomic_set_float(float *variable, float new_value){
  union{
    float new_float;
    uint32_t new_uint32;
  };
  
  new_float = new_value;
  
  __atomic_store_n ((uint32_t*)variable, new_uint32, __ATOMIC_SEQ_CST);
}
                                             
static inline void atomic_set_float_relaxed(float *variable, float new_value){
  union{
    float new_float;
    uint32_t new_uint32;
  };
  
  new_float = new_value;
  
  __atomic_store_n ((uint32_t*)variable, new_uint32, __ATOMIC_RELAXED);
}
                                             
static inline float atomic_get_float(float *variable){
  union{
    float new_float;
    uint32_t new_uint32;
  };
  
  new_uint32 = __atomic_load_n ((uint32_t*)variable, __ATOMIC_SEQ_CST);

  return new_float;
}
                                             
static inline float atomic_get_float_relaxed(float *variable){
  union{
    float new_float;
    uint32_t new_uint32;
  };
  
  new_uint32 = __atomic_load_n ((uint32_t*)variable, __ATOMIC_RELAXED);

  return new_float;
}
                                             
// Why doesn't gcc allow float when using __atomic_compare_exchange_n ?
static inline bool atomic_compare_and_set_float(float *variable, float old_value, float new_value){
  union{
    float old_float;
    uint32_t old_uint32;
  };
  
  old_float = old_value;

  union{
    float new_float;
    uint32_t new_uint32;
  };
  
  new_float = new_value;

  return atomic_compare_and_set_uint32((uint32_t*)variable, old_uint32, new_uint32);
}
#endif

                          
#define ATOMIC_COMPARE_AND_SET_BOOL(name, old_value, new_value) \
  atomic_compare_and_set_bool(&ATOMIC_NAME(name), old_value, new_value)

#define ATOMIC_COMPARE_AND_SET_BOOL_RELAXED(name, old_value, new_value) \
  atomic_compare_and_set_bool_relaxed(&ATOMIC_NAME(name), old_value, new_value)

#define ATOMIC_COMPARE_AND_SET_INT(name, old_value, new_value) \
  atomic_compare_and_set_int(&ATOMIC_NAME(name), old_value, new_value)

#define ATOMIC_COMPARE_AND_SET_UINT32(name, old_value, new_value) \
  atomic_compare_and_set_uint32(&ATOMIC_NAME(name), old_value, new_value)

#define ATOMIC_COMPARE_AND_SET_POINTER(name, old_value, new_value) \
  atomic_compare_and_set_pointer(&ATOMIC_NAME(name), old_value, new_value)

#define ATOMIC_COMPARE_AND_SET_POINTER_ARRAY(name, pos, old_value, new_value) \
  atomic_compare_and_set_pointer(&ATOMIC_NAME(name)[pos], old_value, new_value)

#define ATOMIC_COMPARE_AND_SET_BOOL_ARRAY(name, pos, old_value, new_value) \
  atomic_compare_and_set_bool(&ATOMIC_NAME(name)[pos], old_value, new_value)

#define ATOMIC_SET_RETURN_OLD(name, val) \
  __atomic_exchange_n (&ATOMIC_NAME(name), val, __ATOMIC_SEQ_CST)

#define ATOMIC_SET_RETURN_OLD_RELAXED(name, val) \
  __atomic_exchange_n (&ATOMIC_NAME(name), val, __ATOMIC_RELAXED)

#define ATOMIC_ADD_RETURN_OLD(name, how_much)                           \
  __atomic_fetch_add (&ATOMIC_NAME(name), how_much, __ATOMIC_SEQ_CST)

#define ATOMIC_ADD_RETURN_OLD_RELAXED(name, how_much)                           \
  __atomic_fetch_add (&ATOMIC_NAME(name), how_much, __ATOMIC_RELAXED)

#define ATOMIC_OR_RETURN_OLD_RELAXED(name, how_much)                           \
  __atomic_fetch_or (&ATOMIC_NAME(name), how_much, __ATOMIC_RELAXED)

#define ATOMIC_ADD_ARRAY_RETURN_OLD(name, pos, how_much)                 \
  __atomic_fetch_add (&ATOMIC_NAME(name)[pos], how_much, __ATOMIC_SEQ_CST)

#define ATOMIC_ADD(name, how_much) ATOMIC_ADD_RETURN_OLD(name, how_much)

#define ATOMIC_ADD_ARRAY(name, pos, how_much) ATOMIC_ADD_ARRAY_RETURN_OLD(name, poshow_much)


/*
#define ATOMIC_ADD_RETURN_OLD2(name, how_much)                           \
  __atomic_fetch_add (&(name), how_much, __ATOMIC_SEQ_CST)

#define ATOMIC_ADD2(name, how_much) ATOMIC_ADD_RETURN_OLD2(name, how_much)
*/

// doesn't work with bool!
#define ATOMIC_ADD_RETURN_NEW(name, how_much)                           \
  (__atomic_fetch_add (&ATOMIC_NAME(name), how_much, __ATOMIC_SEQ_CST) + how_much)

#define ATOMIC_OR_RETURN_NEW_RELAXED(name, how_much)                           \
  (__atomic_fetch_or (&ATOMIC_NAME(name), how_much, __ATOMIC_RELAXED) | how_much)



/************** float ******************/
  
// These functions are suppressed from tsan
static inline void safe_float_write(float *pos, float value){
  *pos = value;
}

static inline void safe_double_write(double *pos, double value){
  *pos = value;
}

static inline void safe_volatile_float_write(volatile float *pos, float value){
  *pos = value;
}

static inline float safe_volatile_float_read(volatile const float *pos){
  return *pos;
}

static inline float safe_float_read(const float *pos){
  return *pos;
}

static inline double safe_double_read(const double *pos){
  return *pos;
}

static inline void safe_int_write(int *pos, int value){
  *pos = value;
}

static inline int safe_int_read(const int *pos){
  return *pos;
}

static inline void safe_bool_write(bool *pos, bool value){
  *pos = value;
}

static inline bool safe_bool_read(const bool *pos){
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

static inline void *atomic_pointer_read_relaxed(void **p){
  return __atomic_load_n(p, __ATOMIC_RELAXED);
}

static inline void atomic_pointer_write(void **p, void *v){
  __atomic_store_n(p, v, __ATOMIC_SEQ_CST);
}

static inline void atomic_pointer_write_relaxed(void **p, void *v){
  __atomic_store_n(p, v, __ATOMIC_RELAXED);
}



/************** doubles ******************/

// Is this really working? (Think I spent a long time investigating it, and found out that it was. Double is just 8 bytes, so it shouldn't be different to int64_t)

/*
double dasdouble;

void set_das_double(void){
  double new_value_variable = 5.0;
  __atomic_store(&dasdouble, &new_value_variable, __ATOMIC_SEQ_CST);

}

=>

64 bit
======
set_das_double:
.LFB0:
	.cfi_startproc
	movabsq	$4617315517961601024, %rax
	movq	%rax, dasdouble(%rip)
	mfence
	ret
	.cfi_endproc

32 bit
======
set_das_double:
.LFB0:
	.cfi_startproc
	subl	$12, %esp
	.cfi_def_cfa_offset 16
	xorl	%eax, %eax
	movl	$1075052544, %edx
	movl	%eax, (%esp)
	movl	%edx, 4(%esp)
	movq	(%esp), %xmm0
	movq	%xmm0, dasdouble
	mfence
	addl	$12, %esp
	.cfi_def_cfa_offset 4
	ret
	.cfi_endproc

----------------------------------------------------------

double get_das_double(void){
  double result;                                          
  __atomic_load (&dasdouble, &result, __ATOMIC_SEQ_CST);
  return result;
}

=>

64 bit
======
get_das_double:
.LFB1:
	.cfi_startproc
	movq	dasdouble(%rip), %rax
	movq	%rax, -8(%rsp)
	movsd	-8(%rsp), %xmm0
	ret
	.cfi_endproc

32 bit
======
get_das_double:
.LFB1:
	.cfi_startproc
	subl	$20, %esp
	.cfi_def_cfa_offset 24
	movq	dasdouble, %xmm0
	movsd	%xmm0, (%esp)
	fldl	(%esp)
	addl	$20, %esp
	.cfi_def_cfa_offset 4
	ret
	.cfi_endproc


 */

#ifdef __cplusplus
static_assert (sizeof(double) == sizeof(int64_t), "Size of double is not correct");
#endif

#define ATOMIC_DOUBLE_GET(name) ({                                      \
      double result;                                                    \
      __atomic_load (&ATOMIC_NAME(name), &result, __ATOMIC_SEQ_CST);      \
      result;                                                           \
    })

#define ATOMIC_DOUBLE_GET_RELAXED(name) ({                              \
      double result;                                                    \
      __atomic_load (&ATOMIC_NAME(name), &result, __ATOMIC_RELAXED);    \
      result;                                                           \
    })

#define ATOMIC_DOUBLE_SET(name,new_value) ({                            \
      double new_value_variable = new_value;                            \
      __atomic_store (&(ATOMIC_NAME(name)), &new_value_variable, __ATOMIC_SEQ_CST); \
    })

#define ATOMIC_DOUBLE_SET_RETURN_OLD(name,new_value) ({                            \
      double result;                                                    \
      double new_value_variable = new_value;                            \
      __atomic_exchange (&(ATOMIC_NAME(name)), &new_value_variable, &result, __ATOMIC_SEQ_CST); \
      result;                                                           \
    })

#define ATOMIC_DOUBLE_SET_RELAXED(name,new_value) ({                            \
      double new_value_variable = new_value;                            \
      __atomic_store (&(ATOMIC_NAME(name)), &new_value_variable, __ATOMIC_RELAXED); \
    })


/*

typedef double atomic_double_t;

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

namespace radium{

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

  // Returns the argument for read_end.
  int read_start(void){
    while(ATOMIC_GET(is_writing)==true);
    
    return ATOMIC_GET(generation);
  }

  // Returns true if reading was successful.
  bool read_end(int read_start_generation){
    while(ATOMIC_GET(is_writing)==true);
    
    if (ATOMIC_GET(generation) == read_start_generation)
      return true;
    else
      return false;
  }
};


// Class to store a pointer.
// The main thread can set, replace and free the pointer at any time. (doesn't look like it can free... that seems to be taken care of automatically)
// A realtime thread can access the pointer at any time by using the ScopedUsage class.
//
template <typename T>
class AtomicPointerStorage{

  template <typename T2> friend class RT_AtomicPointerStorage_ScopedUsage;

#if !defined(RELEASE)
  bool _is_used = false;
#endif

  AtomicPointerStorage(const AtomicPointerStorage&) = delete;
  AtomicPointerStorage& operator=(const AtomicPointerStorage&) = delete;

private:
  
  DEFINE_ATOMIC(T *, _pointer) = NULL;
  DEFINE_ATOMIC(T *, _old_pointer_to_be_freed) = NULL;

  void (*_free_pointer_function)(T *);

  void maybe_free_something(T *a, T *b){
    if (_free_pointer_function != NULL){
      if (a!=NULL)
        _free_pointer_function(a);
      if (b!=NULL)
        _free_pointer_function(b);
    }
  }

public:

  AtomicPointerStorage(void (*free_pointer_function)(T *))
    : _free_pointer_function(free_pointer_function)
  {
  }

  ~AtomicPointerStorage(){
    maybe_free_something(ATOMIC_GET(_pointer), ATOMIC_GET(_old_pointer_to_be_freed));
  }

  // May be called at any time. 'free_pointer_function' may be called 0, 1, or 2 times. (usually 1 time)
  void set_new_pointer(T *new_pointer){
    T *old_pointer_to_be_freed = ATOMIC_SET_RETURN_OLD(_old_pointer_to_be_freed, NULL);

    T *old = ATOMIC_SET_RETURN_OLD(_pointer, new_pointer);
    //printf("Has set. new: %p, old: %p, curr: %p\n", new_pointer, old, ATOMIC_GET(_pointer));

    maybe_free_something(old, old_pointer_to_be_freed);
  }
};

// Create an instance of this class to access pointer from a realtime thread.
// I don't think it works to create more than one instance of this at the same time.
template <typename T>
class RT_AtomicPointerStorage_ScopedUsage{

  AtomicPointerStorage<T> *_storage;
  T *_pointer;

  RT_AtomicPointerStorage_ScopedUsage(const RT_AtomicPointerStorage_ScopedUsage&) = delete;
  RT_AtomicPointerStorage_ScopedUsage& operator=(const RT_AtomicPointerStorage_ScopedUsage&) = delete;

public:

  T *get_pointer(void) const {
    return _pointer;
  }
  
  RT_AtomicPointerStorage_ScopedUsage(AtomicPointerStorage<T> *storage)
    :_storage(storage)
  {
#if !defined(RELEASE)
    if(storage->_is_used==true)
      abort();
    storage->_is_used=true;
#endif
    _pointer = ATOMIC_SET_RETURN_OLD(storage->_pointer, NULL);
  }
    
  ~RT_AtomicPointerStorage_ScopedUsage(){
#if !defined(RELEASE)
    if(_storage->_is_used==false)
      abort();
    _storage->_is_used = false;
#endif

    if(atomic_compare_and_set_pointer(reinterpret_cast<void**>(&ATOMIC_NAME(_storage->_pointer)), NULL, _pointer)){ // The void-cast is a workaround for compiler error. Strange.
      return;
    } else {
#if !defined(RELEASE)
      T *old_pointer = ATOMIC_GET(_storage->_old_pointer_to_be_freed);
      if (old_pointer != NULL && old_pointer!=_pointer)
        abort();
#endif
      ATOMIC_SET(_storage->_old_pointer_to_be_freed, _pointer);
    }
  }
};

} // namespace radium

#endif // __cplusplus


#endif
