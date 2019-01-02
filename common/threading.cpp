
#include <unistd.h>
#include <errno.h>
#include <pthread.h>

#include <functional>

#include <QHash>

#ifndef TEST_THREADING

#if defined(FOR_MACOSX)
#include "../weakjack/weak_libjack.h"
#endif

#include "nsmtracker.h"
#include "visual_proc.h"
#include "Semaphores.hpp"
#include "Mutex.hpp"

#include "threading.h"
#include "threading_lowlevel.h"

#include "OS_Player_proc.h"



void OS_WaitForAShortTime(int milliseconds){
#ifdef FOR_WINDOWS
  Sleep(milliseconds);
#else
  msleep(milliseconds);
#endif
}


// Use this function if it is important that we don't return too early.
void OS_WaitAtLeast(int milliseconds){
  double start_time = TIME_get_ms();

  OS_WaitForAShortTime(milliseconds);

  while( (TIME_get_ms()-start_time) < milliseconds)
    OS_WaitForAShortTime(10);
}

#endif // !TEST_THREADING




enum ThreadType{
  OTHER_THREAD,
  MAIN_THREAD,
  PLAYER_THREAD,
  JUCE_THREAD,
  RUNNER_THREAD
};

static __thread ThreadType thread_type = OTHER_THREAD;


void THREADING_init_main_thread_type(void) {
  R_ASSERT_NON_RELEASE(thread_type==OTHER_THREAD);
  thread_type = MAIN_THREAD;
}

void THREADING_init_player_thread_type(void) {
  R_ASSERT_NON_RELEASE(thread_type==OTHER_THREAD);
  thread_type = PLAYER_THREAD;
}

void THREADING_init_runner_thread_type(void) {
  R_ASSERT_NON_RELEASE(thread_type==OTHER_THREAD);
  thread_type = RUNNER_THREAD;
}

void THREADING_init_juce_thread_type(void) {
  R_ASSERT_NON_RELEASE(thread_type==OTHER_THREAD);
  thread_type = JUCE_THREAD;
}

bool THREADING_is_main_thread(void){
  return thread_type==MAIN_THREAD;
}

bool THREADING_is_player_thread(void){
  return thread_type==PLAYER_THREAD;
}

bool THREADING_is_runner_thread(void){
  return thread_type==RUNNER_THREAD;
}

bool THREADING_is_player_or_runner_thread(void){
  return THREADING_is_player_thread() || THREADING_is_runner_thread();
}


bool THREADING_is_juce_thread(void){
  return thread_type==JUCE_THREAD;
}

static radium::Mutex g_on_main_thread_lock;

namespace{
  struct OnMainThread{
    radium::Semaphore *semaphore = NULL;
    std::function<void(void)> callback;

    OnMainThread(std::function<void(void)> callback)
      : callback(callback)
    {}
  };
}
  
static int64_t g_on_main_thread_id = 0;

static QMap<int64_t, OnMainThread*> g_on_main_threads; // Using QMap instead of QHash to run in same order.

void THREADING_call_very_often(void){

  while(true) {
    
    int64_t id;
    OnMainThread *on_main_thread;
    
    {
      radium::ScopedMutex lock(g_on_main_thread_lock);
      if (g_on_main_threads.isEmpty())
        return;

      id = g_on_main_threads.keys().first();
      on_main_thread = g_on_main_threads[id];
    }

    on_main_thread->callback();
    
    {
      radium::ScopedMutex lock(g_on_main_thread_lock);

      g_on_main_threads.remove(id);
      
      if (on_main_thread->semaphore != NULL)
        on_main_thread->semaphore->signal();
    }

    delete on_main_thread;
  }
}

bool THREADING_async_function_has_run(int64_t id){
  R_ASSERT_NON_RELEASE(!THREADING_is_main_thread());
  
  radium::ScopedMutex lock(g_on_main_thread_lock);
  return g_on_main_threads.contains(id)==false;
}

void THREADING_wait_for_async_function(int64_t id){
  
  if (THREADING_is_main_thread()){
    
    R_ASSERT_NON_RELEASE(false);

    {
      bool call_call_very_often = false;

      {
        radium::ScopedMutex lock(g_on_main_thread_lock);
        call_call_very_often = g_on_main_threads.contains(id);
      }

      if (call_call_very_often)
        THREADING_call_very_often();
    }
    
  } else {

    radium::Semaphore semaphore;
    
    {
      radium::ScopedMutex lock(g_on_main_thread_lock);
      
      if(g_on_main_threads.contains(id)==false)
        return;
      
      OnMainThread *on_main_thread = g_on_main_threads[id];

      if(on_main_thread->semaphore != NULL)
        R_ASSERT(false);
      else
        on_main_thread->semaphore = &semaphore;
    }
    
    semaphore.wait();
  }
}

int64_t THREADING_run_on_main_thread_async(std::function<void(void)> callback, bool called_from_main_thread){
  R_ASSERT(!PLAYER_current_thread_has_lock());

  if (called_from_main_thread){
    R_ASSERT_NON_RELEASE(THREADING_is_main_thread());
  }else{
    R_ASSERT_NON_RELEASE(!THREADING_is_main_thread());
  }

  {
    radium::ScopedMutex lock(g_on_main_thread_lock);
    
    int64_t id = g_on_main_thread_id++;
    
    g_on_main_threads[id] = new OnMainThread(callback);
    
    return id;
  }
}

void THREADING_run_on_main_thread_and_wait(std::function<void(void)> callback){
  if (THREADING_is_main_thread()){
    R_ASSERT(!PLAYER_current_thread_has_lock());
    R_ASSERT_NON_RELEASE(false);
    callback();
    return;    
  }
  
  int64_t id = THREADING_run_on_main_thread_async(callback);
  THREADING_wait_for_async_function(id);
}
    
priority_t THREADING_get_priority(void){
  priority_t priority;

#if defined(FOR_WINDOWS)

  priority.priority = GetThreadPriority(GetCurrentThread());
  
  if (priority.priority==THREAD_PRIORITY_ERROR_RETURN){
    GFX_Message(NULL, "GetThreadPriority failed: %u", (unsigned int)GetLastError());
    priority.priority = THREAD_PRIORITY_NORMAL;
  }
  
#elif defined(__linux__) || defined(FOR_MACOSX)

  int success = pthread_getschedparam(pthread_self(), &priority.policy, &priority.param);
  if (success!=0) {
    GFX_Message(NULL, "pthread_getschedparam returned %d (really strange))",success);
  }

#else
  #error "unkwnonw architantaiehnr"
#endif

  return priority;
}

void THREADING_set_priority(priority_t priority){
#if defined(FOR_WINDOWS)
  
  int success = SetThreadPriority(GetCurrentThread(), priority.priority);

  if (success==0) {
    GFX_Message(NULL, "SetThreadPriority failed: %d", success);
  }
  
#elif defined(__linux__) || defined(FOR_MACOSX)

  // Maybe try to use native thread api on osx. This actually fails (!) on osx 10.12 .
  // Example: https://github.com/SchwartzNU/DataAcquisition/blob/47d728c34bd9db1787bbb3f7805aff60484104d0/Stage/Externals/matlab-priority/setNormalPriority.c

#if defined(FOR_MACOSX)
  // Workaround for 10.12
  if (priority.policy==SCHED_OTHER){
    int success = jack_drop_real_time_scheduling(GET_CURRENT_THREAD());
    if (success!=0) {
      GFX_Message(NULL, "jack_drop_real_time_scheduling(GET_CURRENT_THREAD()) returned %d (policy: %d, priority: %d, message: \"%s\")",
                  success,
                  priority.policy,
                  priority.param.sched_priority,
                  success==EINVAL ? "policy is not a recognized policy, or param does not make sense for the policy."
                  : success==EPERM ? "The caller does not have appropriate privileges to set the specified scheduling policy and parameters."
                  : success==ENOTSUP ? "attempt was made to set the policy or scheduling parameters to an unsupported value"
                  : "Unknown error type"
                  );
    }
    return;
  }
#endif
  
  int success = pthread_setschedparam(pthread_self(), priority.policy, &priority.param);

  if (success!=0) {
    GFX_Message(NULL, "pthread_setschedparam(,%d,%d) returned %d (%s)",
                priority.policy,
                priority.param.sched_priority,
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
