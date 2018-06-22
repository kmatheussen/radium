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
extern LANGSPEC void THREADING_init_juce_thread_type(void);
extern LANGSPEC bool THREADING_is_main_thread(void); // This function is called very often (every time we gc-alloc, for instance). The reason it is not inlined is because static thread local variables (which are used to identify which thread is currently running) may be a lot faster to access than non-static thread-local variabes: http://david-grs.github.io/tls_performance_overhead_cost_linux/ (I have just very superficially skimmed the article though, so I could very well have misunderstood this.).
extern LANGSPEC bool THREADING_is_player_thread(void);
extern LANGSPEC bool THREADING_is_juce_thread(void);

#ifdef __cplusplus
#include <functional>
void THREADING_call_very_often(void);
int64_t THREADING_run_on_main_thread_async(std::function<void(void)> callback); // returns id
bool THREADING_async_function_has_run(int64_t id);
void THREADING_wait_for_async_function(int64_t id);
void THREADING_run_on_main_thread_and_wait(std::function<void(void)> callback);
#endif

extern LANGSPEC void THREADING_acquire_player_thread_priority(void); // Implemented in audio/Mixer.cpp
extern LANGSPEC void THREADING_drop_player_thread_priority(void); // Implemented in audio/Mixer.cpp

#endif
