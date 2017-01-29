#ifndef RADIUM_COMMON_THREADING_LOWLEVEL_H
#define RADIUM_COMMON_THREADING_LOWLEVEL_H

#if defined(FOR_WINDOWS)



  #include <windows.h>
  #include <process.h>
  #include <comutil.h>

  #define LockType CRITICAL_SECTION
  #define LOCK_INITIALIZE(A) InitializeCriticalSection(&A)
  #define LOCK_DESTROY(A)    DeleteCriticalSection(&A)
  #define LOCK_LOCK(A)       EnterCriticalSection(&A)
  #define LOCK_UNLOCK(A)     LeaveCriticalSection(&A)

  #define GET_CURRENT_THREAD() GetCurrentThread()


typedef struct{
  int priority;
} priority_t;


#else

#include <pthread.h>

  // pthread API
  #define LockType pthread_mutex_t
  #define LOCK_INITIALIZE(A) pthread_mutex_init(&A, NULL)
  #define LOCK_DESTROY(A)    pthread_mutex_destroy(&A)
  #define LOCK_LOCK(A)       pthread_mutex_lock(&A)
  #define LOCK_UNLOCK(A)     pthread_mutex_unlock(&A)

  #define GET_CURRENT_THREAD() pthread_self()


#if defined(FOR_MACOSX)

// osx
typedef struct{
  int policy;  
  struct sched_param param;
} priority_t;

#else

// linux
typedef struct{
  int policy;  
  struct sched_param param;
} priority_t;

#endif


#endif // linux||osx


extern LANGSPEC priority_t THREADING_get_priority(void);
extern LANGSPEC void THREADING_set_priority(priority_t priority);



#endif
