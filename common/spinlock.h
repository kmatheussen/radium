
#ifndef RADIUM_COMMON_SPINLOCK_H
#define RADIUM_COMMON_SPINLOCK_H



#if 0 // Fallback spinlock implementation

#define DEFINE_SPINLOCK_NOINIT(name) \
  DEFINE_ATOMIC(bool, name)

#define SPINLOCK_INIT(name) \
  ATOMIC_SET(name, false)

//#define DEFINE_SPINLOCK(name)                 
//  DEFINE_SPINLOCK_NOINIT(name) = false

#define SPINLOCK_TRYLOCK(name)                                          \
  (                                                                     \
   bool old_value = false,                                              \
   __atomic_compare_exchange_n(&ATOMIC_NAME(name), &old_value, true, true,  __ATOMIC_ACQUIRE, __ATOMIC_ACQUIRE) \
  )
  

#define SPINLOCK_OBTAIN(name)                                           \
  do{                                                                   \
    bool old_value;                                                     \
    do{                                                                 \
      old_value = false;                                                \
    }while(__atomic_compare_exchange_n(&ATOMIC_NAME(name), &old_value, true, true,  __ATOMIC_ACQUIRE, __ATOMIC_ACQUIRE)==false); \
  }while(0);

#define SPINLOCK_RELEASE(name) \
  __atomic_store_n (&(ATOMIC_NAME(name)), false, __ATOMIC_RELEASE); // Not entirely sure __ATOMIC_RELEASE is correct here...


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
  
  DEFINE_SPINLOCK_NOINIT(_lock);

# if !defined(RELEASE)
  bool _holds_lock = false;
# endif

public:

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

 public:

  ScopedSpinlock(Spinlock &lock)
    :_lock(lock)
  {
    lock.lock();
  }
  ~ScopedSpinlock(){
    _lock.unlock();
  }

};

} // namespace radium


#endif // __cplusplus


#endif // RADIUM_COMMON_SPINLOCK_H

