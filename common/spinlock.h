
#ifndef RADIUM_COMMON_SPINLOCK_H
#define RADIUM_COMMON_SPINLOCK_H

#include "atomic.h"



#if 1 // defined(FOR_WINDOWS) || defined(FOR_MACOSX) // Fallback spinlock implementation

// The arguments for "weak" and the various memorders are taken from https://github.com/seL4/seL4_libs/blob/master/libsel4sync/include/sync/spinlock.h

#define SPINLOCK_TYPE int // Integers are faster than booleans
#define SPINLOCK_FALSE 0
#define SPINLOCK_TRUE 1

//#define DEFINE_SPINLOCK_NOINIT(name) SPINLOCK_TYPE name

#define SPINLOCK_INIT(name) \
  (name = SPINLOCK_FALSE)

//#define DEFINE_SPINLOCK(name)                 
//  DEFINE_SPINLOCK_NOINIT(name) = false

#define SPINLOCK_TRYLOCK(name)                                          \
  ({                                                                    \
    SPINLOCK_TYPE old_value = SPINLOCK_FALSE;                           \
    __atomic_compare_exchange_n(&(name), &old_value, SPINLOCK_TRUE, false,  __ATOMIC_ACQUIRE, __ATOMIC_RELAXED); \
  })
  

#define SPINLOCK_OBTAIN(name)                                           \
  do{                                                                   \
    SPINLOCK_TYPE old_value;                                            \
    do{                                                                 \
      old_value = SPINLOCK_FALSE;                                       \
    }while(__atomic_compare_exchange_n(&(name), &old_value, SPINLOCK_TRUE, true,  __ATOMIC_ACQUIRE, __ATOMIC_RELAXED)==false); \
  }while(0);

#define SPINLOCK_RELEASE(name) \
  __atomic_store_n (&(name), SPINLOCK_FALSE, __ATOMIC_RELEASE);


#define SPINLOCK_DESTROY(name)

//#define SPINLOCK_IS_OBTAINED(name)            
//  ATOMIC_GET(spinlock)==true




#else // if 0 -> !0


#ifdef __cplusplus
#  define DEFINE_SPINLOCK_NOINIT(name) \
  pthread_spinlock_t name = {}
#else
#  define DEFINE_SPINLOCK_NOINIT(name) \
  pthread_spinlock_t name = {0}
#endif

#define SPINLOCK_INIT(name) \
  R_ASSERT(pthread_spin_init(&(name), PTHREAD_PROCESS_PRIVATE) == 0)

//#define DEFINE_SPINLOCK(name)                 
//  DEFINE_SPINLOCK_NOINIT(name) = false

#if !defined(RELEASE)
#  define SPINLOCK_TRYLOCK(name)                                        \
  ({int __radium_res2 = pthread_spin_trylock(&(name)); R_ASSERT(__radium_res2==0 || __radium_res2==EBUSY) ; __radium_res2==0;})
#else
#  define SPINLOCK_TRYLOCK(name)                \
  (pthread_spin_trylock(&(name))==0)
#endif

#if !defined(RELEASE)
#  define SPINLOCK_OBTAIN(name)                   \
  R_ASSERT(pthread_spin_lock(&(name))==0)
#else
#  define SPINLOCK_OBTAIN(name)                 \
  pthread_spin_lock(&(name))
#endif


#if !defined(RELEASE)
#  define SPINLOCK_RELEASE(name)                \
  R_ASSERT(pthread_spin_unlock(&(name))==0)
#else
#  define SPINLOCK_RELEASE(name)                 \
  pthread_spin_unlock(&(name))
#endif

#if !defined(RELEASE)
#  define SPINLOCK_DESTROY(name)                \
  R_ASSERT(pthread_spin_destroy(&(name))==0)
#else
#  define SPINLOCK_DESTROY(name)                 \
  pthread_spin_unlock(&(name))
#endif



//#define SPINLOCK_IS_OBTAINED(name)            
//  ATOMIC_GET(spinlock)==true


#endif // !0



#ifdef __cplusplus

namespace radium{


class Spinlock {
  
  SPINLOCK_TYPE _lock;

public:

# if !defined(RELEASE)
  bool _holds_lock = false;
# endif

  Spinlock(){
    SPINLOCK_INIT(_lock);
  }

  ~Spinlock(){
#   if !defined(RELEASE)
    R_ASSERT(_holds_lock==false);
#   endif
    SPINLOCK_DESTROY(_lock);
  }

  bool trylock(void){
#if defined(RELEASE)
    return SPINLOCK_TRYLOCK(_lock);
#else
    bool ret = SPINLOCK_TRYLOCK(_lock);
    if (ret){
      R_ASSERT(_holds_lock==false);
      _holds_lock = true;
    }
    return ret;
#endif
  }

  void lock(void){
    SPINLOCK_OBTAIN(_lock);

#   if !defined(RELEASE)
    R_ASSERT(_holds_lock==false);
    _holds_lock = true;
#   endif
  }

  void unlock(){
#   if !defined(RELEASE)
    R_ASSERT(_holds_lock==true);
    _holds_lock = false;
#   endif

    SPINLOCK_RELEASE(_lock);
  }
};


class ScopedSpinlock {

  Spinlock &_lock;
  bool _needs_lock;
  
 public:

  ScopedSpinlock(Spinlock &lock, bool needs_lock = true)
    : _lock(lock)
    , _needs_lock(needs_lock)
  {
    if(_needs_lock)
      lock.lock();
  }
  ~ScopedSpinlock(){
    if(_needs_lock)
      _lock.unlock();
  }

};

class ScopedTrySpinlock {

  Spinlock &_lock;
  bool _gotit;

 public:

  ScopedTrySpinlock(Spinlock &lock)
    :_lock(lock)
  {
    _gotit = lock.trylock();
  }

  ~ScopedTrySpinlock(){
    if (_gotit)
      _lock.unlock();
  }

  bool gotit(void) const {
    return _gotit;
  }
};

} // namespace radium


#endif // __cplusplus


#endif // RADIUM_COMMON_SPINLOCK_H

