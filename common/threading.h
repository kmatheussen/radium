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
extern LANGSPEC bool THREADING_is_main_thread(void);
extern LANGSPEC bool THREADING_is_player_thread(void);

extern LANGSPEC void THREADING_acquire_player_thread_priority(void); // Implemented in audio/Mixer.cpp
extern LANGSPEC void THREADING_drop_player_thread_priority(void); // Implemented in audio/Mixer.cpp

#endif
