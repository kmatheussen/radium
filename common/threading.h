// Mutex and sleeping (copied from RtAudio.h, RtAudio.cpp and tests/qmidiin.cpp)

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



#elif defined(__linux__) || defined(FOR_MACOSX)



  #include <pthread.h>
  #include <unistd.h>
  #include <errno.h>

  // pthread API
  #define LockType pthread_mutex_t
  #define LOCK_INITIALIZE(A) pthread_mutex_init(&A, NULL)
  #define LOCK_DESTROY(A)    pthread_mutex_destroy(&A)
  #define LOCK_LOCK(A)       pthread_mutex_lock(&A)
  #define LOCK_UNLOCK(A)     pthread_mutex_unlock(&A)

  #define GET_CURRENT_THREAD() pthread_self()

typedef struct{
  int policy;  
  struct sched_param param;
} priority_t;

#else



  #error "unkwnonw architantaiehnr"



#endif



#ifndef DOESNT_HAVE_SSE
#  include <xmmintrin.h>
#endif


#include <float.h>

//#pragma fenv_access (on)

// On Intel set FZ (Flush to Zero) and DAZ (Denormals Are Zero)
// flags to avoid costly denormals
// (Copied from faust)
#ifdef __SSE__
    #ifdef __SSE2__
        #define AVOIDDENORMALS _mm_setcsr(_mm_getcsr() | 0x8040)
    #else
        #define AVOIDDENORMALS _mm_setcsr(_mm_getcsr() | 0x8000)
    #endif
#else
#   error "AVOIDDENORMALS is not defined"
    #define AVOIDDENORMALS 
#endif




extern LANGSPEC void THREADING_init_main_thread_type(void);
extern LANGSPEC void THREADING_init_player_thread_type(void);
extern LANGSPEC bool THREADING_is_main_thread(void);
extern LANGSPEC bool THREADING_is_player_thread(void);

extern LANGSPEC void THREADING_acquire_player_thread_priority(void); // Implemented in audio/Mixer.cpp
extern LANGSPEC void THREADING_drop_player_thread_priority(void); // Implemented in audio/Mixer.cpp

extern LANGSPEC priority_t THREADING_get_priority(void);
extern LANGSPEC void THREADING_set_priority(priority_t priority);

