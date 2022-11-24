
#include <unistd.h>
#include <errno.h>
#include <pthread.h>

#include <functional>

#include "include_boost.h"


#include <QHash>

#if defined(FOR_MACOSX)
#include "../weakjack/weak_libjack.h"
#endif

#include "nsmtracker.h"
#include "visual_proc.h"
#include "Semaphores.hpp"
#include "Mutex.hpp"

#include "threading.h"
#include "threading_lowlevel.h"

#include "../audio/Mixer_proc.h"
#include "../audio/Juce_plugins_proc.h"

#if defined(FOR_MACOSX)
#include "../macosx/machthreads_proc.h"
#endif

#include "OS_Player_proc.h"



#ifndef TEST_THREADING

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
#endif



enum ThreadType{
  OTHER_THREAD,
  MAIN_THREAD,
  PLAYER_THREAD,
  JUCE_THREAD,
  RUNNER_THREAD
};

static __thread ThreadType g_thread_type = OTHER_THREAD;


void THREADING_init_main_thread_type(void) {
  R_ASSERT_NON_RELEASE(g_thread_type==OTHER_THREAD);
  g_thread_type = MAIN_THREAD;
}

// Note, called very often.
bool THREADING_init_player_thread_type(void) {
  if (g_thread_type == PLAYER_THREAD)
    return false;
  
  g_thread_type = PLAYER_THREAD;
  return true;
}

void THREADING_init_runner_thread_type(void) {
  R_ASSERT_NON_RELEASE(g_thread_type==OTHER_THREAD);
  g_thread_type = RUNNER_THREAD;
}

void THREADING_init_juce_thread_type(void) {
  R_ASSERT_NON_RELEASE(g_thread_type==OTHER_THREAD);
  g_thread_type = JUCE_THREAD;
}

bool THREADING_is_main_thread(void){
  return g_thread_type==MAIN_THREAD;
}

bool THREADING_is_player_thread(void){
  return g_thread_type==PLAYER_THREAD;
}

bool THREADING_is_runner_thread(void){
  return g_thread_type==RUNNER_THREAD;
}

bool THREADING_is_player_or_runner_thread(void){
  return THREADING_is_player_thread() || THREADING_is_runner_thread();
}


bool THREADING_is_juce_thread(void){
  return g_thread_type==JUCE_THREAD;
}


#ifndef TEST_THREADING

/******************************************/
/***** Run functions on main thread   ****/
/****************************************/


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

#if !defined(RELEASE)
    R_ASSERT(!JUCE_current_thread_is_message_thread()); // Big chance of deadlock if waiting for the main thread on the juce message thread. The juce message thread often waits for the main thread.
#endif

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

int64_t THREADING_run_on_main_thread_async(std::function<void(void)> callback, bool called_from_main_thread, bool run_now_if_called_from_main_thread){
  R_ASSERT(!PLAYER_current_thread_has_lock());

  if (called_from_main_thread){
    
    R_ASSERT_NON_RELEASE(THREADING_is_main_thread());

    if (run_now_if_called_from_main_thread){
      callback();
      return -1;
    }

  }else{

    // Happens on JucePlayer::changeListenerCallback when starting up, on windows only.
    // Shouldn't be a problem if we are running on the main thread here though.
    //R_ASSERT_NON_RELEASE(!THREADING_is_main_thread());
    
  }

  {
    radium::ScopedMutex lock(g_on_main_thread_lock);
    
    int64_t id = g_on_main_thread_id++;
    
    g_on_main_threads[id] = new OnMainThread(callback);
    
    return id;
  }
}
/*
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
*/
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




/******************************************/
/***** Run functions on player thread ****/
/****************************************/

namespace radium{
  class Ready_To_Run_Scheduled_RT_functions{

    DEFINE_ATOMIC(bool, _finished_running) = false;

    int _i = 0;
    int _num_functions;
    std::function<void(void)> **_functions;

    bool has_spent_too_much_time(void) const {
      //return false;
      //return true;
      return MIXER_get_curr_audio_block_cycle_fraction() >= 0.9;
    }
    
  public:
    
    Ready_To_Run_Scheduled_RT_functions(Scheduled_RT_functions *rt_functions)
      : _num_functions(rt_functions->_functions.size())
      , _functions((std::function<void(void)> **)V_malloc(sizeof(std::function<void(void)>*) * _num_functions))
    {      
      int i=0;
      for(auto func : rt_functions->_functions)
        _functions[i++] = new std::function<void(void)>(func);
      
      rt_functions->_functions.clear();
    }

    ~Ready_To_Run_Scheduled_RT_functions(){
      for(int i=0;i<_num_functions;i++)
        delete _functions[i];
      
      V_free(_functions);
    }
    
    bool finished_running(void) const {
      return ATOMIC_GET(_finished_running);
    }
    
    bool RT_run(void){
      R_ASSERT_NON_RELEASE(finished_running()==false);

      bool has_run_a_func = false;
#if 0 //!defined(RELEASE)
      static int num = 0;
      printf("-------------------RT. START running RT funcs. Size: %d. Num: %d------------------\n", _num_functions, num++);
#endif
      for(; _i < _num_functions ; _i++){
        if (has_run_a_func && has_spent_too_much_time()) // always run at least one func.
          return false;
        
        auto *func = _functions[_i];
        (*func)();
        
        has_run_a_func = true;        
      }
#if 0 //!defined(RELEASE)
      printf("-------------------RT. STOP running RT funcs------------------\n");
#endif
      
      ATOMIC_SET(_finished_running, true);

      return true;
    }

  };
}

static QVector<radium::Ready_To_Run_Scheduled_RT_functions*> g_alive_functions_to_run_on_player_thread;

static boost::lockfree::queue< radium::Ready_To_Run_Scheduled_RT_functions* , boost::lockfree::capacity<256> > g_functions_to_run_on_player_thread_queue;


void RT_call_functions_scheduled_to_run_on_player_thread(void){

  bool have_run_something = false;
  
  static radium::Ready_To_Run_Scheduled_RT_functions *curr_rt_functions = NULL;
    
  while(true){

    if (curr_rt_functions==NULL)
      if (!g_functions_to_run_on_player_thread_queue.pop(curr_rt_functions)){
        if (have_run_something)
          GFX_ScheduleCurrentInstrumentRedraw();
        break;
      }
    
    if (curr_rt_functions->RT_run()){
      have_run_something = true;
      curr_rt_functions = NULL;
    }else
      break;
  }

}

radium::Ready_To_Run_Scheduled_RT_functions *radium::Scheduled_RT_functions::schedule_it(void){
  R_ASSERT_NON_RELEASE(!PLAYER_current_thread_has_lock());

  if (_functions.size()==0)
    return NULL;
  
  int safety = 0;

  radium::Ready_To_Run_Scheduled_RT_functions *ready_to_run = new radium::Ready_To_Run_Scheduled_RT_functions(this);
  g_alive_functions_to_run_on_player_thread.push_back(ready_to_run);
  
  while(!g_functions_to_run_on_player_thread_queue.bounded_push(ready_to_run)){
    printf("THREADING_schedule_on_player_thread: Thread full. Waiting...\n");
    msleep(100);
    safety++;
    if (safety == 100){
      printf("THREADING_schedule_on_player_thread: Giving up.\n");
      break;
    }
  }

  return ready_to_run;
}

void radium::Scheduled_RT_functions::wait_until_ready_to_run_is_finished(radium::Ready_To_Run_Scheduled_RT_functions *ready_to_run){
  if (ready_to_run==NULL)
    return;
  
  while(true){
        
    if (ready_to_run->finished_running())
      break;
    
    msleep(15);
  }
}

void THREADING_schedule_on_player_thread_call_very_often(void){
  QVector<radium::Ready_To_Run_Scheduled_RT_functions*> finished;

  for(auto *func : g_alive_functions_to_run_on_player_thread)
    if (func->finished_running())
      finished.push_back(func);

  for(auto *func : finished){
    R_ASSERT(g_alive_functions_to_run_on_player_thread.removeAll(func)==1);
    delete func;
  }
}
  

#endif // TEST_THREADING



/******************************************/
/***** Priority                       ****/
/****************************************/

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

#if !defined(RELEASE)
enum R_thread_is_RT __thread g_t_current_thread_is_RT = R_UNINITIALIZED;
#endif


void THREADING_set_priority(priority_t priority){

  // NOTE! This function is always used to set non-realtime priority. This is asserted in debug mode on linux.

#if !defined(RELEASE)
  if (priority.policy==SCHED_OTHER)
    g_t_current_thread_is_RT = R_IS_NOT_RT;
  else
    g_t_current_thread_is_RT = R_IS_RT;
#endif

#if !defined(RELEASE)
#if defined(FOR_LINUX)
  R_ASSERT(priority.policy==SCHED_OTHER);
#endif
#endif
  
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
  if (true || priority.policy==SCHED_OTHER){
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

#if !defined(RELEASE)
#if !defined(FOR_MACOSX)
bool THREADING_has_player_thread_priority(void){
  priority_t priority = THREADING_get_priority();
#if defined(FOR_WINDOWS)
  return priority.priority==THREAD_PRIORITY_TIME_CRITICAL;
#else
  //printf("----policy: %d. RR: %d. FIFO: %d. priority: %d\n", priority.policy, SCHED_RR, SCHED_FIFO, priority.param.sched_priority);
  return (priority.policy==SCHED_RR || priority.policy==SCHED_FIFO);
#endif
}
#endif
#endif

#ifdef TEST_THREADING


#include <stdio.h>
#include <pthread.h>
#include <unistd.h>
#include <assert.h>

void CRASHREPORTER_send_assert_message(enum Crash_Type crash_type, const char *fmt,...){
  abort();
}
bool PLAYER_current_thread_has_lock(void){
  return false;
}

int GFX_Message2_internal(vector_t *buttons, bool program_state_is_valid, const char *fmt,...){
  abort();
  return -1;
}

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
