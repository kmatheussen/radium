#ifndef RADIUM_COMMON_THREADING_LOWLEVEL_H
#define RADIUM_COMMON_THREADING_LOWLEVEL_H



// It's likely that we lower the chance of xruns by setting
// USE_SPINLOCK_FOR_PLAYER_LOCK to 1, but it also becomes
// more complicated to reason about what is happening,
// probably for no significant gain.
//
// A hybrid solution could be the best solution: Let
// the jack thread spin, but let other threads wait on
// a semaphore if they can't obtain the lock right away.
#define USE_SPINLOCK_FOR_PLAYER_LOCK 0



#if USE_SPINLOCK_FOR_PLAYER_LOCK

  #include "spinlock.h"

  #define LockType SPINLOCK_TYPE
  #define LOCK_INITIALIZE(A) SPINLOCK_INIT(A)
  #define LOCK_DESTROY(A)
  #define LOCK_LOCK(A)       SPINLOCK_OBTAIN(A)
  #define LOCK_UNLOCK(A)     SPINLOCK_RELEASE(A)

#endif





#if defined(FOR_WINDOWS)


  #include <windows.h>
  #include <process.h>
  #include <comutil.h>

  #if !defined(LockType)
    #define LockType CRITICAL_SECTION
    #define LOCK_INITIALIZE(A) InitializeCriticalSection(&A)
    #define LOCK_DESTROY(A)    DeleteCriticalSection(&A)
    #define LOCK_LOCK(A)       EnterCriticalSection(&A)
    #define LOCK_UNLOCK(A)     LeaveCriticalSection(&A)
  #endif

  #define GET_CURRENT_THREAD() GetCurrentThread()

typedef struct{
  int priority;
} priority_t;



#else // WINDOWS -> LINUX/OSX


#include <pthread.h>

  // pthread API
  #if !defined(LockType)
    #define LockType pthread_mutex_t
    #define LOCK_INITIALIZE(A) pthread_mutex_init(&A, NULL)
    #define LOCK_DESTROY(A)    pthread_mutex_destroy(&A)
    #define LOCK_LOCK(A)       pthread_mutex_lock(&A)
    #define LOCK_UNLOCK(A)     pthread_mutex_unlock(&A)
  #endif

  #define GET_CURRENT_THREAD() pthread_self()

typedef struct{
  int policy;  
  struct sched_param param;
} priority_t;



#endif // linux||osx


#if !defined(RELEASE)
enum R_thread_is_RT{
                    R_UNINITIALIZED,
                    R_IS_NOT_RT,
                    R_IS_RT
};
             
extern __thread enum R_thread_is_RT g_t_current_thread_is_RT; // Note: This value is the intended priority. I.e. the value is R_IS_RT even if trying to set it to RT failed.
#endif

extern LANGSPEC priority_t THREADING_get_priority(void);
extern LANGSPEC void THREADING_set_priority(priority_t priority);


#if defined(FOR_WINDOWS)
typedef HANDLE radium_thread_t;
#else
typedef pthread_t radium_thread_t;
#endif

#if !defined(RELEASE)
#if !defined(FOR_MACOSX)
extern LANGSPEC bool THREADING_has_player_thread_priority(void);  // Implemented in common/threading.cpp
#endif
#endif

extern LANGSPEC void THREADING_acquire_player_thread_priority2(radium_thread_t thread);  // Implemented in audio/Mixer.cpp
extern LANGSPEC void THREADING_acquire_player_thread_priority(void); // Implemented in audio/Mixer.cpp
extern LANGSPEC void THREADING_drop_player_thread_priority2(radium_thread_t thread); // Implemented in audio/Mixer.cpp
extern LANGSPEC void THREADING_drop_player_thread_priority(void); // Implemented in audio/Mixer.cpp



#ifdef __cplusplus

// To prevent priority inversion.
namespace radium{
class ScopedPlayerThreadPriority {
  const bool _doit;
  priority_t _priority_before;
 public:
  ScopedPlayerThreadPriority(bool doit = true)
    : _doit(doit)
  {
    if (doit){
      _priority_before = THREADING_get_priority();
      THREADING_acquire_player_thread_priority();
    }
  }
  ~ScopedPlayerThreadPriority(){
    if (_doit)
      THREADING_set_priority(_priority_before);
  }
};    
}
#endif


#endif
