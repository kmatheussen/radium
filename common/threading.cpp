
#ifndef TEST_THREADING
  #include "nsmtracker.h"
  #include "threading.h"
#endif


enum ThreadType{
  OTHER_THREAD,
  MAIN_THREAD,
  PLAYER_THREAD
};

static __thread ThreadType thread_type = OTHER_THREAD;



void THREADING_init_main_thread_type(void) {
  thread_type = MAIN_THREAD;
}

void THREADING_init_player_thread_type(void) {
  thread_type = PLAYER_THREAD;
}

bool THREADING_is_main_thread(void){
  return thread_type==MAIN_THREAD;
}

bool THREADING_is_player_thread(void){
  return thread_type==PLAYER_THREAD;
}

priority_t THREADING_get_priority(void){
  priority_t priority;

#if defined(FOR_WINDOWS)

  priority.priority = GetThreadPriority(GetCurrentThread());
  
  if (priority.priority==THREAD_PRIORITY_ERROR_RETURN){
    RError("GetThreadPriority failed: %d",GetLastError());
    priority.priority = THREAD_PRIORITY_NORMAL;
  }
  
#elif defined(__linux__) || defined(FOR_MACOSX)
    
  int success = pthread_getschedparam(pthread_self(), &priority.policy, &priority.param);
  if (success!=0) {
    RError("pthread_getschedparam returned %d (really strange))",success);
  }

#else
  #error "unkwnonw architantaiehnr"
#endif

  return priority;
}

void THREADING_set_priority(priority_t priority){
#if defined(FOR_WINDOWS)

  int success = SetThreadPriority(GetCurrentThread(), priority.priority);

  if (success!=0) {
    RError("SetThreadPriority failed: %d", success);
  }
  
#elif defined(__linux__) || defined(FOR_MACOSX)
    
  int success = pthread_setschedparam(pthread_self(), priority.policy, &priority.param);

  if (success!=0) {
    RError("pthread_getschedparam returned %d (%s)",
           success,
           success==EINVAL ? "policy is not a recognized policy, or param does not make sense for the policy."
           : success==EPERM ? "The caller does not have appropriate privileges to set the specified scheduling policy and parameters."
           : success==ENOTSUP ? "attempt was made to set the policy or scheduling parameters to an unsupported value"
           : "Unknown error type... (really strange)"
           );
  }
  
#else
  #error "unkwnonw architantaiehnr"
#endif
}

#ifdef TEST_THREADING

#if 0
g++ threading.cpp -Wall -lpthread
#endif


#include <stdio.h>
#include <pthread.h>
#include <unistd.h>
#include <assert.h>

static pthread_t thread;

static void *player_thread_func(void* arg){

  THREADING_init_player_thread_type();

  assert(!THREADING_is_main_thread());
  assert(THREADING_is_player_thread());

  sleep(2);

  return &thread;
}

int main(){

  THREADING_init_main_thread_type();
  
  assert(THREADING_is_main_thread());
  assert(!THREADING_is_player_thread());

  pthread_create(&thread, NULL, player_thread_func, NULL);

  sleep(1);

  assert(THREADING_is_main_thread());
  assert(!THREADING_is_player_thread());

  void *retval;
  pthread_join(thread, &retval);
  assert(retval==&thread);
  sleep(3);

  assert(THREADING_is_main_thread());
  assert(!THREADING_is_player_thread());

  printf("\n\n\n == Threading test success == \n");
  return 0;
}

#endif
