#ifndef RADIUM_COMMON_THREADING_H
#define RADIUM_COMMON_THREADING_H




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
        #define RADIUM_AVOIDDENORMALS _mm_setcsr(_mm_getcsr() | 0x8040)
    #else
        #define RADIUM_AVOIDDENORMALS _mm_setcsr(_mm_getcsr() | 0x8000)
    #endif
#else
#   error "RADIUM_AVOIDDENORMALS is not defined"
    #define RADIUM_AVOIDDENORMALS 
#endif

extern LANGSPEC void THREADING_init_main_thread_type(void);
extern LANGSPEC void THREADING_init_player_locks(void);
extern LANGSPEC bool THREADING_init_player_thread_type(void); // returns true if thread is changed.
extern LANGSPEC void THREADING_init_runner_thread_type(void);
extern LANGSPEC void THREADING_init_juce_thread_type(void);
extern LANGSPEC bool THREADING_is_main_thread(void); // This function is called very often (every time we gc-alloc, for instance). The reason it is not inlined is because static thread local variables (which are used to identify which thread is currently running) may be a lot faster to access than non-static thread-local variabes: http://david-grs.github.io/tls_performance_overhead_cost_linux/ (I have just very superficially skimmed the article though, so I could very well have misunderstood this.).
extern LANGSPEC bool THREADING_is_player_thread(void);
extern LANGSPEC bool THREADING_is_runner_thread(void);
extern LANGSPEC bool THREADING_is_player_or_runner_thread(void);
extern LANGSPEC bool THREADING_is_juce_thread(void);
extern LANGSPEC bool THREADING_is_RT(void); // Returns true if current thread runs with realtime priority or holds the player lock. (same as (THREADING_is_player_or_runner_thread || PLAYER_current_thread_has_lock(), but faster).

extern LANGSPEC void THREADING_inc_RT(void); // called before obtaining player lock, or before setting realtime priority
extern LANGSPEC void THREADING_dec_RT(void); // called afterwards.


#ifdef __cplusplus
#include <functional>
#include <vector>
void THREADING_call_very_often(void);

/*
 * Returns id.
 * Callbacks are run in order
 * If called_from_main_thread==true, we must be called from the main thread.
 * If called_from_main_thread==true && run_now_if_called_from_main_thread==true, return value is -1. (it's probably cleaner not to use THREADING_run_on_main_thread_async in this case though)
 */
int64_t THREADING_run_on_main_thread_async(std::function<void(void)> callback, bool called_from_main_thread = false, bool run_now_if_called_from_main_thread = false);


bool THREADING_async_function_has_run(int64_t id);
void THREADING_wait_for_async_function(int64_t id);
void THREADING_run_on_main_thread_and_wait(std::function<void(void)> callback); // (callbacks are run in order)

namespace radium{
  class Scheduled_RT_functions;
}

namespace radium{

  class Ready_To_Run_Scheduled_RT_functions;
  
  class Scheduled_RT_functions{

    friend class radium::Ready_To_Run_Scheduled_RT_functions;
    
    std::vector<std::function<void(void)>> _functions;

    void wait_until_ready_to_run_is_finished(radium::Ready_To_Run_Scheduled_RT_functions *ready_to_run);
    
    radium::Ready_To_Run_Scheduled_RT_functions *schedule_it(void);
      
  public:

    bool force_next_scheduling_to_wait = false;

    // Prevent heap allocation
    void *operator new (size_t) = delete;
    void *operator new[] (size_t) = delete;
    void  operator delete (void *) = delete;
    void  operator delete[] (void*) = delete;

    Scheduled_RT_functions(const Scheduled_RT_functions&) = delete;
    Scheduled_RT_functions& operator=(const Scheduled_RT_functions&) = delete;
    
    Scheduled_RT_functions(){
    }
    
    ~Scheduled_RT_functions(){
      schedule_to_run_on_player_thread();
    }

    // returns immediately
    void schedule_to_run_on_player_thread(void){
      if (force_next_scheduling_to_wait)
        schedule_to_run_on_player_thread_and_wait();
      else
        schedule_it();
    }

    // returns when all funcs in rt_functions has run.
    void schedule_to_run_on_player_thread_and_wait(void){
      radium::Ready_To_Run_Scheduled_RT_functions *ready_to_run = schedule_it();

      //printf("==========================================   ...Scheduled and wating for %p\n", ready_to_run);

      force_next_scheduling_to_wait = false;
      wait_until_ready_to_run_is_finished(ready_to_run);
    }

    
    void add(std::function<void(void)> func){
      _functions.push_back(func);
    }
  };
}

#if 0
static inline radium::Scheduled_RT_functions create_scheduled_rt_functions(void){
  radium::Scheduled_RT_functions ret;
  return ret;
}
#endif

void RT_call_functions_scheduled_to_run_on_player_thread(void);
void THREADING_schedule_on_player_thread_call_very_often(void);
#endif



#endif
