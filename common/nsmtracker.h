/* Copyright 2000 Kjetil S. Matheussen

This program is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public License
as published by the Free Software Foundation; either version 2
of the License, or (at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA. */


#ifndef RADIUM_COMMON_NSMTRACKER_H
#define RADIUM_COMMON_NSMTRACKER_H 1

#ifndef __STDC_FORMAT_MACROS
#error "what?"
#define __STDC_FORMAT_MACROS 1
#endif

#ifdef LANGSPEC
#error "LANGSPEC already defined"
#endif

#  ifdef __cplusplus
#    define LANGSPEC "C"
#  else
#    define LANGSPEC
#  endif


#if  ((defined(__clang__) && ((__clang_major__ > 3) || (__clang_major__ == 3 && __clang_minor__ >= 4))) || (defined(__GNUC__) && __GNUC__ >= 5))
#else
#  error "Need at least clang 3.4 or gcc 5 (you can uncomment this line to make the program run, but swing, and perhaps other things as well, might be unstable.)"
#endif

  
#ifndef __SSE2__
#error "SSE2 is missing (i.e. -msse2 is lacking)"
#endif

#ifndef __SSE2_MATH__
#error "SSE2 math is missing (i.e. -fpmath=sse is lacking)"
#endif

#if defined(__tune_corei7__) && __tune_corei7__
#  if !defined(MARCH_NATIVE_ALLOWED)
#    error "Compiled with -mtune=native or -mtune=corei7"
#  endif
#endif

#if !defined(RELEASE) && !defined(FOR_WINDOWS)
#  include <sanitizer/common_interface_defs.h>
#  if defined(RADIUM_USES_ASAN)
#    if !__has_feature(address_sanitizer) && !defined(__SANITIZE_ADDRESS__)
#      error "Error. Check usage of ASAN_POISON_MEMORY_REGION / ASAN_UNPOISON_MEMORY_REGION macros"
#    endif
#  endif
#  if __has_feature(address_sanitizer) || defined(__SANITIZE_ADDRESS__)
#    if !defined(RADIUM_USES_ASAN)
#      error "Error. RADIUM_USES_ASAN macro not defined"
#    endif
#  endif
#endif

#ifdef RELEASE
  #ifndef __OPTIMIZE__
    #error "Missing -O2 or -O3 compiler option"
  #endif
  #ifdef DISABLE_BDWGC
    #error "DISABLE_BDWGC can not be defined in release mode. (runs out of memory pretty quickly then)"
  #endif
  #ifdef MEMORY_DEBUG
    #error "MEMORY_DEBUG can not be defined in release mode. (plus that it makes more sense to use DISABLE_BDWGC together with fsanitizer=address instead of MEMORY_DEBUG)"
  #endif
#endif

#ifndef DEBUG_ALLOWED
#ifdef DEBUG
#  error "DEBUG option can not be specified"
#endif
#endif

#ifdef FOR_WINDOWS
#ifdef FOR_WINDOWS
#else
#error "oops"
#endif
#endif

#ifdef FOR_LINUX
#ifdef FOR_LINUX
#else
#error "oops"
#endif
#endif

#ifdef FOR_MACOSX
#ifdef FOR_MACOSX
#else
#error "oops"
#endif
#endif

#if USE_GTK_VISUAL
#error "oops"
#endif


#if defined(FOR_LINUX)
#elif defined(FOR_MACOSX)
#elif defined(FOR_WINDOWS)
#else
#error "unknown architecture"
#endif


#ifdef __cplusplus
static_assert (sizeof(long long int) >= 8, "sizof(long long int) must be 8 or higher (the assertion is here because the function llabs is used on int64_t)");
#endif

/******************************************************************
  Main header file for the tracker. Each struct often has a source-
  file with the same, or nearly the same, name.

  Note, the file OS_Visual.h is OS spesific
  and must be put into the OS directory it belongs te. The other
  OS_*_proc.h files are not.
******************************************************************/

#if !USE_GTK_VISUAL && !USE_GTK_REQTYPE && !USE_GTK_MENU
#  define GTK_IS_USED 0
#else
#  define GTK_IS_USED 1
#endif

#include <inttypes.h>
#include <stdio.h>
#include <stdlib.h>
#include <limits.h>
#include <stdint.h>
#include <stdbool.h>
#include <math.h>

#ifdef __cplusplus
#include <cmath>
#include <limits>
#include <atomic>
#endif

#ifdef USE_QT4
#if !defined(__clang__)
#  pragma GCC diagnostic push
#  pragma GCC diagnostic ignored "-Wsuggest-attribute=const"
#endif

#pragma clang diagnostic push
#pragma clang diagnostic ignored "-Wshorten-64-to-32"

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wnull-dereference"

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wstrict-overflow"

#include <QList>
#include <QMap>

#pragma GCC diagnostic pop

#pragma GCC diagnostic pop

#pragma clang diagnostic pop

#if !defined(__clang__)
#  pragma GCC diagnostic pop
#endif
#endif

#if defined(__clang__)
  #define FORMAT_ATTRIBUTE(A,B)
#else
  #define FORMAT_ATTRIBUTE(A,B) __attribute__((format(gnu_printf, A, B)))
#endif

#if defined(__cplusplus)
//#include "RT_memory_allocator_proc.h"
#include "RT_memory_freer_proc.h"
#endif


/* Unfortunately, AmigaOS has one absolute address that is legal to
   read from; 4, which often makes MuForce not to report reading
   from the wrong address (radiums fault, not MuFurces). By 
   inserting SDB at all 4 offsets, we never read from 4 if a pointer is NULL.
*/
#ifdef SYSBASEDEBUG
#  define SDB int sysbasedebug;
#else
#  define SDB
#endif

//typedef int32_t STime;		/* Time can be negative. */
typedef int64_t STime;		/* Time can be negative. */
//typedef STime NInt;
typedef int32_t NInt;
//#define PFREQ (48000*8)			/* Subseconds for STime */ /* Replaced by samplerate */

//#define LATENCY (PFREQ/200)
#define LATENCY 0 // Dont need this when the player is called from the jack thread.


extern int g_audio_block_size;

// Must be a multiply of 64 because of pd, which uses a block size of 64. 64 seems to work fine.
//#define RADIUM_BLOCKSIZE 64
//#define RADIUM_BLOCKSIZE 1024
#define RADIUM_BLOCKSIZE g_audio_block_size

#define MAX_NUM_CPUS 128

#define MAIN_TIMER_INTERVAL 15 // in milliseconds. Can be set to 1, 5, 10, 25, or 50. (must probably be set to 5)
extern int64_t g_main_timer_num_calls;

#define MINBLOCKRELTIME 0.001f
#define MAXBLOCKRELTIME 6.0f

// Higher than 255 is no point.
#define MAX_BRIGHTNESS 63

#if !USE_OPENGL
enum{
  PAINT_DIRECTLY = 0,
  PAINT_BUFFER = 1
};
#else
enum{
  PAINT_DIRECTLY = 0,
  PAINT_BUFFER = PAINT_DIRECTLY
};
#endif

extern bool g_is_loading;
extern bool g_is_saving;

extern bool g_initing_starting_to_play_song;

extern bool g_user_interaction_enabled; // Used for testing. If this one is false, all user interaction will be done automatically, randomly.

extern float g_mouse_dx, g_mouse_dy; // Only have valid values in windows.
extern bool g_wants_delta_movements; // Only useful in windows.
extern double g_last_time_mouse_pointer_was_moved_by_the_program; // Only used in windows.

extern float g_dpi;
extern float g_gfx_scale;
extern bool g_has_gfx_scale;


#include "../crashreporter/crashreporter_proc.h"


#include <OS_Visual.h>


#if 0
#define R_MAX(a,b) (((a)>(b))?(a):(b))
#define R_MIN(a,b) (((a)<(b))?(a):(b))
#define R_ABS(a) ((a)<0?(-(a)):(a))
#else
#define R_MAX(a,b) ({ typeof(a) aTEMP = (a); typeof(b) bTEMP = (b); aTEMP > bTEMP ? aTEMP : bTEMP; })
#define R_MIN(a,b) ({ typeof(a) aTEMP = (a); typeof(b) bTEMP = (b); aTEMP < bTEMP ? aTEMP : bTEMP; })
#define R_ABS(a) ({ typeof(a) aTEMP = (a) ; aTEMP<0?-aTEMP:aTEMP;})
#endif

#if defined(RELEASE)
#  define R_BOUNDARIES(min,b,max) ({ typeof(min) minTEMP = (min); typeof(b) bTEMP = (b); typeof(max) maxTEMP = (max); bTEMP < minTEMP ? minTEMP : bTEMP > maxTEMP ? maxTEMP : bTEMP;})
#else
#  define R_BOUNDARIES(min,b,max) ({ typeof(min) minTEMP = (min); typeof(b) bTEMP = (b); typeof(max) maxTEMP = (max); maxTEMP < minTEMP ? (abort(),minTEMP) : bTEMP < minTEMP ? minTEMP : bTEMP > maxTEMP ? maxTEMP : bTEMP;})
#endif

static inline bool sane_isnormal_FLOAT(float x) {
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wfloat-equal"
  
#ifdef __cplusplus
  return x==0.0f || std::isnormal(x);
#else
  return x==0.0f || isnormal(x);
#endif

#pragma GCC diagnostic pop
}

static inline bool sane_isnormal_DOUBLE(double x) {
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wfloat-equal"
#ifdef __cplusplus
  return x==0.0 || std::isnormal(x);
#else
  return x==0.0 || isnormal(x);
#endif
#pragma GCC diagnostic pop
}

#ifdef __cplusplus
template <typename T>
static inline bool sane_isnormal(T x){
#pragma clang diagnostic push
#pragma clang diagnostic ignored "-Wdouble-promotion"

  if (sizeof (x) == sizeof (float))
    return sane_isnormal_FLOAT(x);
  else
    return sane_isnormal_DOUBLE(x);
#pragma clang diagnostic pop
}
#else
#define sane_isnormal(x) (sizeof (x) == sizeof (float) ? sane_isnormal_FLOAT(x) : sane_isnormal_DOUBLE(x))
#endif

#ifdef __cplusplus
static inline bool equal_floats(float x, float y) {
#if !defined(RELEASE)
#pragma clang diagnostic push
#pragma clang diagnostic ignored "-Wdouble-promotion"

  if(!sane_isnormal(x) || !sane_isnormal(y)){
    fprintf(stderr, "equal_floats(): Calling abort(). x: %f. y: %f. sane x: %d. sane y: %d\n", x, y, sane_isnormal(x), sane_isnormal(y));
    abort();
  }
#pragma clang diagnostic pop

#endif
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wfloat-equal"
  if(x==y)
    return true;
#pragma GCC diagnostic pop
  return R_ABS(x - y) < std::numeric_limits<float>::epsilon();
}
static inline bool equal_doubles(double x, double y) {
#if !defined(RELEASE)
  if(!sane_isnormal(x) || !sane_isnormal(y))
    abort();
#endif
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wfloat-equal"
  if(x==y)
    return true;
#pragma GCC diagnostic pop
  return R_ABS(x - y) < std::numeric_limits<double>::epsilon();
}
#else
extern float g_float_epsilon;
static inline bool equal_floats(float x, float y) {
#if !defined(RELEASE)
#pragma clang diagnostic push
#pragma clang diagnostic ignored "-Wdouble-promotion"

  if(!sane_isnormal(x) || !sane_isnormal(y))
    abort();
#pragma clang diagnostic pop
  
#endif
#pragma GCC diagnostic ignored "-Wfloat-equal"
  if(x==y)
    return true;
#pragma GCC diagnostic pop
  return R_ABS(x - y) < g_float_epsilon;
}
extern double g_double_epsilon;
static inline bool equal_doubles(double x, double y) {
#if !defined(RELEASE)
  if(!sane_isnormal(x) || !sane_isnormal(y))
    abort();
#endif
#pragma GCC diagnostic ignored "-Wfloat-equal"
  if(x==y)
    return true;
#pragma GCC diagnostic pop
  return R_ABS(x - y) < g_double_epsilon;
}
#endif


typedef struct vector_t_ vector_t;

// NOTE: Might return -1
extern LANGSPEC int GFX_Message2_internal(vector_t *buttons, bool program_state_is_valid, const char *fmt,...) FORMAT_ATTRIBUTE(3,4);
#define GFX_Message2(Buttons, PSIV, ...) ((void)donothing(0 && printf(__VA_ARGS__)), GFX_Message2_internal(Buttons, PSIV, __VA_ARGS__)) // Add a "printf" call to make the C compiler show warning/error if using wrong arguments for FMT.

// NOTE: Might return -1
#define GFX_Message(buttons, ...) GFX_Message2(buttons, false,  __VA_ARGS__)

// Note: Can be called from any thread
extern LANGSPEC bool GFX_is_showing_message(void);



#define R_ASSERT_MESSAGE(a)                                             \
  do{                                                                   \
    if(!(a))                                                            \
      GFX_Message(NULL, "Warning: \"" # a "\" is not true");            \
  }while(0)

#define R_ASSERT_INNER(a)                                               \
  if(!(a))                                                              \
    CRASHREPORTER_send_assert_message(CT_ERROR, "Assert failed: \"" # a "\". %s: " __FILE__":%d", __FUNCTION__, __LINE__)

#define R_ASSERT(a)                                                     \
  do{                                                                   \
    R_ASSERT_INNER(a);                                                  \
  }while(0)

#define R_ASSERT_RETURN_IF_FALSE2(a,b)                                  \
  do{                                                                   \
    if(!(a)) {                                                          \
      CRASHREPORTER_send_assert_message(CT_ERROR, "Assert failed: \"" # a "\". %s: " __FILE__":%d", __FUNCTION__, __LINE__); \
      return b;                                                         \
    }                                                                   \
  }while(0)

#define R_ASSERT_RETURN_IF_FALSE(a) R_ASSERT_RETURN_IF_FALSE2(a,)

#if defined(RELEASE)
  #define R_ASSERT_NON_RELEASE(a) do{}while(0)
  #define R_ASSERT_NON_RELEASE2(a, returnvalue) do{}while(0)
#else
  #define R_ASSERT_NON_RELEASE(a) do{if(!(a) && !CRASHREPORTER_is_currently_sending())abort();}while(0)
  #define R_ASSERT_NON_RELEASE2(a, returnvalue) R_ASSERT_RETURN_IF_FALSE2(a, returnvalue)
#endif

enum ShowAssertionOrThrowAPIException{
  SHOW_ASSERTION,
  THROW_API_EXCEPTION
};

#define R_ASSERT_RETURN_IF_FALSE3(Test, Handle_Error_Type, Return, Fmt, ...) \
  do{                                                                   \
    if(!(Test)){                                                        \
      if(Handle_Error_Type==SHOW_ASSERTION)                             \
        RError(Fmt, __VA_ARGS__);                                       \
      else                                                              \
        handleError(Fmt, __VA_ARGS__);                                  \
      return Return;                                                    \
    }                                                                   \
  }while(0)
    

#define R_ASSERT_RETURN_IF_FALSE4(Test, Handle_Error_Type, Fmt, ...)    \
  do{                                                                   \
    if(!(Test)){                                                        \
      if(Handle_Error_Type==SHOW_ASSERTION)                             \
        RError(Fmt, __VA_ARGS__);                                       \
      else                                                              \
        handleError(Fmt, __VA_ARGS__);                                  \
      return;                                                           \
    }                                                                   \
  }while(0)
    



#if 1 //defined(USE_CUSTOM_NUM_FRAMES)
#  define R_NUM_FRAMES_DECL int radium_num_frames___,
#  define R_NUM_FRAMES radium_num_frames___
#  define R_NUM_FRAMES_ARG radium_num_frames___,
#else
#  define R_NUM_FRAMES_DECL
#  define R_NUM_FRAMES radium_num_frames___ RADIUM_BLOCKSIZE
#  define R_NUM_FRAMES_ARG
#endif


extern LANGSPEC bool RT_message_will_be_sent(void);
extern LANGSPEC void RT_message_internal(const char *fmt,...) FORMAT_ATTRIBUTE(1,2);
#define RT_message(...) do{donothing(0 && printf(__VA_ARGS__)); RT_message_internal(__VA_ARGS__);}while(0) // Add a "printf" call to make the C compiler show warning/error if using wrong arguments for FMT.
extern LANGSPEC bool is_showing_RT_message(void); // Can be called from any thread.

static inline bool is_playing(void);

struct vector_t_;

static inline int donothing(int input){
  return input;
}

extern int64_t g_num_calls_to_handleError;

extern LANGSPEC void handleError_internal(const char *fmt,...) FORMAT_ATTRIBUTE(1,2);
#define handleError(...) ((void)donothing(0 && printf(__VA_ARGS__)), handleError_internal(__VA_ARGS__)) // Add a "printf" call to make the C compiler show warning/error if using wrong arguments for FMT.


extern LANGSPEC void msleep(int ms);


#include "atomic.h"


// These two variables contains the same value, but g_is_starting_up can only be accessed from the main thread.
extern DEFINE_ATOMIC(bool, is_starting_up);
extern bool g_is_starting_up;



#include "debug_proc.h"
#include "threading.h"
#include "OS_Player_proc.h"
#include "memory_proc.h"
#include "nsmtracker_events.h"

#include "OS_error_proc.h"
#include "OS_Semaphores.h"
#include "keyboard_focus_proc.h"

#include "validatemem_proc.h"



static inline int bool_to_int(bool val){
  return val==true ? 1 : 0;
}

static inline bool int_to_bool(int value){
  if (value == 0)
    return false;

  R_ASSERT(value==1);
  return true;
}

#define R_SCALE(x__,x1__,x2__,y1__,y2__) \
  (                                      \
   y1__ + ( ((x__-x1__)*(y2__-y1__))     \
            /                            \
            (x2__-x1__)                  \
            )                            \
  )   

static inline int64_t scale_int64(const int64_t x, const int64_t x1, const int64_t x2, const int64_t y1, const int64_t y2){

  int64_t diff = x2-x1;
  
#if !defined(RELEASE)
  R_ASSERT(diff!=0);
#endif
  
  if (diff==0) // this is never supposed to happen, but to avoid integer divide-by-zero, we sacrifice some cycles here.
    return (y1+y2)/2;
  else
    return y1 + ( ((x-x1)*(y2-y1))
                  /
                  diff
                  );
}

static inline double scale_double(const double x, const double x1, const double x2, const double y1, const double y2){
  double diff = x2-x1;
  
#if !defined(RELEASE)
  R_ASSERT(!equal_doubles(diff,0.0));
  if(!sane_isnormal(x) || !sane_isnormal(x2) || !sane_isnormal(x2) || !sane_isnormal(y1) || !sane_isnormal(y2))
    abort();
  
  if (equal_doubles(diff, 0.0))
    return (y1+y2)/2.0;
  else
#endif
    return y1 + ( ((x-x1)*(y2-y1))
                  /
                  diff
                  );
}

static inline float scale(const float x, const float x1, const float x2, const float y1, const float y2){
  float diff = x2-x1;
  
#if !defined(RELEASE)
  if(!sane_isnormal_FLOAT(x) || !sane_isnormal_FLOAT(x1) || !sane_isnormal_FLOAT(x2) || !sane_isnormal_FLOAT(y1) || !sane_isnormal_FLOAT(y2) || !sane_isnormal_FLOAT(diff)){

    fprintf(stderr, "scale(): Calling abort(). x/x1/x2/y1/y2: %f %f %f %f %f ---- %d %d %d %d %d %d\n",
            (double)x,(double)x1,(double)x2,(double)y1,(double)y2,
            (int)sane_isnormal_FLOAT(x), (int)sane_isnormal_FLOAT(12), (int)sane_isnormal_FLOAT(x2), (int)sane_isnormal_FLOAT(y1), (int)sane_isnormal_FLOAT(y2), (int)sane_isnormal_FLOAT(diff));

    if (isfinite(x) && isfinite(x1) && isfinite(x2) && isfinite(y1) && isfinite(y2) && isfinite(diff))
      fprintf(stderr, "   ....Aborting call to abort. None of the numbers where inf or nan.\n");
    else
      abort();
  }
  
  R_ASSERT(!equal_floats(diff,0.0));
  
  if (equal_floats(diff, 0.0))
    return (y1+y2)/2.0f;
  else
#endif
    return y1 + ( ((x-x1)*(y2-y1))
                  /
                  diff
                  );
}

// When exp_value < 1, then scale_exp(0.5,0,1,0,1,..) < 0.5
// When exp_value > 1, then scale_exp(0.5,0,1,0,1,..) > 0.5
static inline float scale_exp(float x, float x1, float x2, float y1, float y2, float exp_value){
  float normalized_x = scale(x, x1, x2, 0, 1);
  float normalized_y = powf(normalized_x, exp_value);
  return scale(normalized_y, 0, 1, y1, y2);
}

#if defined(_MATH_H) || defined(__MATH_H__) || defined(_MATH_H_)
//          ^ linux             ^ osx                  ^ windows
static inline double midi_to_hz(double midi){
  if(midi<=0)
    return 0;
  else
    //  return 1;
  return 8.17579891564*(exp(.0577622650*midi));
}
#endif

typedef struct{
  const char *filename;
  const char *function_name;
  const int linenum;
  const char *extra_info;
} source_pos_t;


#define LOC() ({                                          \
      source_pos_t sp = {__FILE__,  __FUNCTION__, __LINE__,NULL};   \
      sp;                                                       \
    })
#define LOC2(a) ({                                          \
      source_pos_t sp = {__FILE__,  __FUNCTION__, __LINE__,a};   \
      sp;                                                       \
    })


enum WhetherToDeleteUnusedRecordingTakes{
  URTT_NEVER=0,
  URTT_ASK=1,
  URTT_ALWAYS=2
};

#ifdef __cplusplus
namespace radium{
  class ScopedBoolean{
    bool &_abool;
    const bool _doit;
  public:
    ScopedBoolean(bool &abool, const bool doit = true)
      : _abool(abool)
      , _doit(doit)
    {
      R_ASSERT(_abool==false);
      if(_doit)
        _abool = true;
    }

    ~ScopedBoolean(){
      if(_doit){
        R_ASSERT(_abool==true);
        _abool = false;
      } else {
        R_ASSERT_NON_RELEASE(_abool==false);
      }
    }
  };    
  class ScopedGeneration{
    int &_anint;
    const bool _doit;
  public:
    ScopedGeneration(int &anint, const bool doit = true)
      : _anint(anint)
      , _doit(doit)
    {
      R_ASSERT_NON_RELEASE(_anint >= 0);
      
      if(_doit)
        _anint++;
    }

    ~ScopedGeneration(){
      R_ASSERT_NON_RELEASE(_anint >= 1);
      
      if(_doit)
        _anint--;
    }
  };
  class ScopedIniting;
  class Initing{
    friend ScopedIniting;
    int _counter = 0;
  public :
    bool can_access(void) const {
      return _counter==0;
    }
  };
  class ScopedIniting{
    ScopedGeneration _scoped_generation;
  public:
    ScopedIniting(Initing &initing)
      : _scoped_generation(initing._counter)
    {}
  };
};
#endif


/*********************************************************************
	time.h
*********************************************************************/

#include "nsmtracker_time.h"

#ifdef USE_QT4
#include <QString>

namespace radium{

  // Note: "static inline QString get_time_string(int64_t frames, bool include_centiseconds = true)" is available by including audio/Mixer_proc.h
  //
  static inline QString get_time_string(double seconds, bool include_centiseconds = true){
    bool is_negative = false;

    if (seconds < 0){
      is_negative = true;
      seconds = -seconds;
    }

    int i_seconds = seconds;
    int minutes = i_seconds / 60;
    int hours = minutes / 60;
    
    int centiseconds = seconds*100 - i_seconds*100;

    i_seconds = i_seconds % 60;
    minutes = minutes % 60;
    
    
    QString base((is_negative ? QString("-") : QString()) +
                 (minutes < 10 ? "0" : "") +
                 QString::number(minutes) +
                 ":" +
                 (i_seconds < 10 ? "0" : "") +
                 QString::number(i_seconds));
    
    QString ret = base + ":" + (centiseconds < 10 ? "0" : "") + QString::number(centiseconds);
    
    if (hours > 0)
      return QString(hours < 10 ? " " : "" + QString::number(hours)) + ret;
    else
      return ret;
  }

}
#endif

static inline bool is_called_every_ms(int ms){
#if !defined(RELEASE)
  R_ASSERT( (ms % MAIN_TIMER_INTERVAL) == 0);
#endif
  return g_main_timer_num_calls % (ms/MAIN_TIMER_INTERVAL) == 0;
}


/*********************************************************************
	colors.h
*********************************************************************/



enum ColorNums {
  
  START_CONFIG_COLOR_NUM = 0,

  ILLEGAL_COLOR_NUM = -1,

  GUI_SEPARATOR, // Separator

  LOW_BACKGROUND_COLOR_NUM,
  HIGH_BACKGROUND_COLOR_NUM,

  LABEL_COLOR_NUM,
  
  MENU_TEXT_COLOR_NUM,
  MENU_KEYBINDING_TEXT_COLOR_NUM,
  KEYBOARD_FOCUS_BORDER_COLOR_NUM,
  ALTERNATIVE_LABEL_COLOR_NUM,

  BUTTONS_SEPARATOR, // Separator
  
  BUTTONS_COLOR_NUM,
  BUTTONS_PRESSED_COLOR_NUM,
  CHECK_BOX_SELECTED_COLOR_NUM,
  CHECK_BOX_UNSELECTED_COLOR_NUM,
  BUTTONS_TEXT_COLOR_NUM,  
  
  SCROLLBAR_SEPARATOR, // Separator

  SCROLLBAR_COLOR_NUM,
  SCROLLBAR_BACKGROUND_COLOR_NUM,

  SLIDER_SEPARATOR, // Separator
  
  SLIDER1_COLOR_NUM,
  SLIDER2_COLOR_NUM,
  SLIDER_BACKGROUND_COLOR_NUM,
  SLIDER_DISABLED_COLOR_NUM,
  SLIDER_TEXT_COLOR_NUM,
  SLIDER_POINTER_COLOR_NUM,
  SLIDER_RECORDING_COLOR_NUM,


  TABS_SEPARATOR, // Separator

  TAB_SELECTED_COLOR_NUM,
  TAB_UNSELECTED_COLOR_NUM,

  AUDIO_SEPARATOR, // Separator
  
  PEAKS_COLOR_NUM,
  PEAKS_0DB_COLOR_NUM,
  PEAKS_4DB_COLOR_NUM,

  WAVEFORM_COLOR_NUM,

  EDITOR_SEPARATOR, // Separator
  
  LOW_EDITOR_BACKGROUND_COLOR_NUM,
  HIGH_EDITOR_BACKGROUND_COLOR_NUM,
  EDITOR_GRAYED_OUT_COLOR_NUM,
  
  TEXT_COLOR_NUM,
  BAR_TEXT_COLOR_NUM,
  INSTRUMENT_NAME_COLOR_NUM,
  PORTAMENTO_NOTE_TEXT_COLOR_NUM,
  PORTAMENTO_END_NOTE_TEXT_COLOR_NUM,
  VELOCITY_TEXT_COLOR_NUM,
  MIDDLE_VELOCITY_TEXT_COLOR_NUM,
  LAST_VELOCITY_TEXT_COLOR_NUM, 
  
  EDITOR_SLIDERS_COLOR_NUM,
  TRACK_SLIDER_COLOR_NUM,
  LINE_SLIDER_COLOR_NUM,
  
  VELOCITY1_COLOR_NUM,
  VELOCITY2_COLOR_NUM,
  TRACK_SEPARATOR1_COLOR_NUM,
  TRACK_SEPARATOR2A_COLOR_NUM,
  TRACK_SEPARATOR2B_COLOR_NUM,
  RANGE_COLOR_NUM,  
  PITCH_LINE_COLOR_NUM,
  
  CURSOR_EDIT_ON_COLOR_NUM,                       // 7
  CURSOR_EDIT_OFF_COLOR_NUM,
  CURSOR_BORDER_COLOR_NUM,
  CURSOR_CURR_COLUMN_BORDER_COLOR_NUM,
  PLAY_CURSOR_COLOR_NUM,

  CURRENT_BEAT_TEXT_COLOR_NUM,
  CURRENT_BEAT_RANGE_COLOR_NUM,
  
  PIANONOTE_COLOR_NUM,
  PIANONOTE_SELECTED_COLOR_NUM,
  PIANOROLL_OCTAVE_COLOR_NUM,
  PIANOROLL_NOTE_NAME_COLOR_NUM,
  PIANOROLL_NOTE_BORDER_COLOR_NUM,

  ZOOMLINE_TEXT_COLOR_NUM1,
  ZOOMLINE_TEXT_COLOR_NUM2,
  ZOOMLINE_TEXT_COLOR_NUM3,
  ZOOMLINE_TEXT_COLOR_NUM4,
  ZOOMLINE_TEXT_COLOR_NUM5,
  ZOOMLINE_TEXT_COLOR_NUM6,
  ZOOMLINE_TEXT_COLOR_NUM7,
  TEMPOGRAPH_COLOR_NUM,

  TEMPO_MULTIPLIER_SLIDER_COLOR_NUM,
  TEMPO_MULTIPLIER_SLIDER_TEXT_COLOR_NUM,

  
  AUTOMATION_SEPARATOR, // Separator
  
  //AUTOMATION_INDICATOR_COLOR_NUM,
  AUTOMATION1_COLOR_NUM,
  AUTOMATION2_COLOR_NUM,
  AUTOMATION3_COLOR_NUM,
  AUTOMATION4_COLOR_NUM,
  AUTOMATION5_COLOR_NUM,
  AUTOMATION6_COLOR_NUM,
  AUTOMATION7_COLOR_NUM,
  AUTOMATION8_COLOR_NUM,


  
  INSTRUMENT_SEPARATOR, // Separator
 
  SOUNDFONT_COLOR_NUM,
  SOUNDFILE_COLOR_NUM,
  CURRENT_SOUNDFILE_COLOR_NUM,

  

  MIXER_SEPARATOR, // Separator

  MIXER_BACKGROUND_COLOR_NUM,
  MIXER_TEXT_COLOR_NUM,
  MIXER_BORDER_COLOR_NUM,
  MIXER_EVENT_CONNECTION_COLOR_NUM,
  MIXER_AUDIO_CONNECTION_COLOR_NUM,
  //  MIXER_AUDIO_CONNECTION_MUTED_COLOR_NUM,
  MIXER_AUDIO_PORT_COLOR_NUM,
  MIXER_SLOT_INDICATOR_COLOR_NUM,
  MIXER_AUTOSUSPENSION_COLOR_NUM,
  MIXER_CURRENT_OBJECT_BORDER_COLOR_NUM,
  MIXER_SELECTED_OBJECT_BORDER_COLOR_NUM,
  MIXERSTRIPS_CURRENT_INSTRUMENT_BORDER_COLOR_NUM,

  NOTE_EVENT_INDICATOR_COLOR_NUM,
  NOTE_EVENT_INDICATOR_BORDER_COLOR_NUM,


  
  SEQUENCER_SEPARATOR, // Separator

  PLAYLIST_CURRENT_ENTRIES_BORDER_COLOR_NUM,
  
  SEQUENCER_EDITOR_SEQTRACKS_BACKGROUND_COLOR_NUM,
  
  INSTRUMENT_BUS_DEFAULT_COLOR_NUM,
  SEQTRACK_INSTRUMENT_DEFAULT_COLOR_NUM,

  SEQUENCER_CURRTRACK_BORDER_COLOR_NUM,
  SEQUENCER_CURR_SEQBLOCK_BORDER_COLOR_NUM,
  SEQUENCER_LANES_BACKGROUND_COLOR_NUM,
  SEQTRACKS_BACKGROUND_COLOR_NUM,
  SEQUENCER_BORDER_COLOR_NUM,
  SEQUENCER_TEXT_COLOR_NUM,
  SEQUENCER_TEXT_CURRENT_BLOCK_COLOR_NUM,
  SEQUENCER_NOTE_COLOR_NUM,
  SEQUENCER_NOTE_START_COLOR_NUM,

  SEQUENCER_CURR_ENTRY_BORDER_COLOR_NUM,
  
  SEQUENCER_BLOCK_BACKGROUND_COLOR_NUM,
  SEQUENCER_BLOCK_BORDER_COLOR_NUM,
  SEQUENCER_BLOCK_BAR_COLOR_NUM,
  SEQUENCER_BLOCK_BEAT_COLOR_NUM,
  SEQUENCER_BLOCK_SELECTED_COLOR_NUM,
  SEQUENCER_BLOCK_MULTISELECT_BACKGROUND_COLOR_NUM,
  SEQUENCER_BLOCK_AUDIO_FILE_BACKGROUND_COLOR_NUM,
  
  SEQUENCER_BLOCK_FADE_BOX_COLOR_NUM,
  SEQUENCER_BLOCK_INTERIOR_BOX_COLOR_NUM,
  SEQUENCER_BLOCK_SPEED_BOX_COLOR_NUM,
  SEQUENCER_BLOCK_STRETCH_BOX_COLOR_NUM,
  
  SEQUENCER_WAVEFORM_COLOR_NUM,
  SEQUENCER_WAVEFORM_BORDER_COLOR_NUM,
  
  SEQUENCER_GRID_COLOR_NUM,
  SEQUENCER_TRACK_BORDER1_COLOR_NUM,
  SEQUENCER_TRACK_BORDER2_COLOR_NUM,
  SEQUENCER_CURSOR_COLOR_NUM,
  SEQUENCER_TIMELINE_BACKGROUND_COLOR_NUM,
  SEQUENCER_TIMELINE_ARROW_COLOR_NUM,
  SEQUENCER_NAVIGATOR_HANDLER_COLOR_NUM,
  SEQUENCER_NAVIGATOR_GRAYOUT_COLOR_NUM,
  SEQUENCER_TEMPO_AUTOMATION_COLOR_NUM,
  SEQUENCER_MARKER_COLOR_NUM,

  END_CONFIG_COLOR_NUM,

  BLACK_COLOR_NUM = 500,
  WHITE_COLOR_NUM,
  RED_COLOR_NUM,

  END_ALL_COLOR_NUMS,
};


/*********************************************************************
	placement.h
*********************************************************************/



#include "placement_type.h" // <--- TODO: place.counter should definitely not be unsigned.



/*********************************************************************
	list.h
*********************************************************************/

struct ListHeader0{
	struct ListHeader0 *next;
};

struct ListHeader1{
	struct ListHeader1 *next;
   SDB
	NInt num;
};

struct ListHeader3{
	struct ListHeader3 *next;
   SDB
	Place p;
};

#define LCAST(a) ((a)==NULL ? NULL : &(a)->l)

#define Tline l.p.line
#define Tcounter l.p.counter
#define Tdividor l.p.dividor


struct ListHeaderP{
	struct ListHeaderP *next;
   SDB
	STime time;
};



/*********************************************************************
        playerclass
*********************************************************************/

#include "playerclass.h"

static inline double ms_to_s(double ms){
  return ms / 1000.0;
}

static inline double s_to_ms(double s){
  return s * 1000.0;
}

static inline double frames_to_s(double frames){
  return frames / (double) pc->pfreq;
}

static inline double s_to_frames(double s){
  return s * (double)pc->pfreq;
}

static inline double ms_to_frames(double ms){
  return s_to_frames(ms_to_s(ms));
}

static inline double frames_to_ms(double frames){
  return s_to_ms(frames_to_s(frames));
}




/*********************************************************************
	vector.h
*********************************************************************/

struct vector_t_{
  int num_elements;
  int num_elements_allocated; // Private. Holds allocated size of 'elements'
  void **elements;
};

static inline vector_t create_static_vector_t(int num_elements, void **elements){
  const vector_t ret = {
    .num_elements = num_elements,
    .num_elements_allocated= num_elements,
    .elements = elements
  };
  return ret;
}

#include "vector_proc.h"



/*********************************************************************
	global roots
*********************************************************************/

extern vector_t g_global_roots;


#ifdef __cplusplus

template<typename T> 
static inline T add_gc_root(T root){
  if(root != NULL)
    VECTOR_push_back(&g_global_roots, root);
#if !defined(RELEASE)
  if(g_global_roots.num_elements > 10000)
    printf("\n\n\n  ==================   GC_ROOT(1) size: %d =================== \n\n\n\n\n", g_global_roots.num_elements);
#endif
  //printf("  Add. gc_root size: %d\n", g_global_roots.num_elements);
  return root;
}

static inline void remove_gc_root(const void *root){
  if(root != NULL){
    VECTOR_remove(&g_global_roots, root);
    //printf("  Remove. gc_root size: %d\n", g_global_roots.num_elements);
  }
}

template<typename T>
static inline T replace_gc_root(T old_root, T new_root){
  if (old_root!=new_root){
    if(old_root != NULL)
      remove_gc_root(old_root);
    if(new_root!=NULL)
      add_gc_root(new_root);
  }
  return new_root;
}

namespace radium{
  template <typename T>
  struct GcHolder{

  private:
    T *t;

    GcHolder(const GcHolder&) = delete;
    GcHolder& operator=(const GcHolder&) = delete;

  public:
    GcHolder(T *t = NULL)
      : t(t)
    {  
      if(t!=NULL)
        add_gc_root(t);
    }
    ~GcHolder(){
      if(t!=NULL)
        remove_gc_root(t);
    }
    T *operator->() const {
      return t;
    }
    void set(T *new_t){
      if(t != new_t){
        replace_gc_root(t, new_t);
        t = new_t;
      }
    }
    T *data(void) const {
      return t;
    }
  };

  template <typename T>
  struct GcVector_t : public Vector_t<T>{
    GcHolder<T> _holder;
    GcVector_t(const vector_t &v)
      : Vector_t<T>(v)
      , _holder(v.elements)
    {}
  };
}

#else

static inline void *add_gc_root(void *root){
  if(root!=NULL)
    VECTOR_push_back(&g_global_roots, root);
#if !defined(RELEASE)
  if(g_global_roots.num_elements > 10000)
    printf("\n\n\n  ==================   GC_ROOT(2) size: %d =================== \n\n\n\n\n", g_global_roots.num_elements);
#endif
  //printf("  Add. gc_root size: %d\n", g_global_roots.num_elements);
  return root;
}

static inline void remove_gc_root(const void *root){
  if(root!=NULL){
    VECTOR_remove(&g_global_roots, root);
    printf("  Remove. gc_root size: %d\n", g_global_roots.num_elements);
  }
}

static inline void *replace_gc_root(const void *old_root, void *new_root){
  if (old_root!=new_root){
    if(old_root!=NULL)
      remove_gc_root(old_root);
    if(new_root!=NULL)
      add_gc_root(new_root);
  }
  return new_root;
}

#endif


/*********************************************************************
	ratio.h
*********************************************************************/

#include "ratio_type.h"

static inline char *static_ratio_to_string(const StaticRatio ratio){
  return talloc_format("%d/%d", (int)ratio.numerator, (int)ratio.denominator);
}



typedef struct _radium_os_disk disk_t;



#include "dyn_type.h"

extern filepath_t g_illegal_filepath;

static inline filepath_t copy_filepath(filepath_t filepath){
  return make_filepath(talloc_wcsdup(filepath.id));
}

#ifdef __cplusplus
namespace radium{
  class FilePath{

    filepath_t _filepath;

  public:
    FilePath(const wchar_t *string)
      : _filepath(make_filepath(string==NULL ? g_illegal_filepath.id : wcsdup(string)))
    {
      R_ASSERT(string!=NULL);
    }
    FilePath(filepath_t filepath)
      : FilePath(filepath.id)
    {}
    FilePath()
      : FilePath(g_illegal_filepath)
    {}
    ~FilePath(){
      free((void*)_filepath.id);
    }

    FilePath(const FilePath &filepath)
      : FilePath(filepath.get())
    {}
    
    FilePath& operator=(const FilePath &filepath)
    {
      free((void*)_filepath.id);
      _filepath.id = wcsdup(filepath.getString());
      return *this;
    }

    const wchar_t *getString(void) const {
      return _filepath.id;
    }
    filepath_t get(void) const {
      return _filepath;
    }
    filepath_t getGC(void) const {
      return make_filepath(talloc_wcsdup(_filepath.id));
    }
    bool isLegal(void) const {
      return wcscmp(_filepath.id, g_illegal_filepath.id);
    }
    bool isEmpty(void) const {
      return !isLegal() || wcslen(getString())==0;
    }
  };
}
#endif


/*********************************************************************
	hashmap.h
*********************************************************************/

// (There is also a DYN_get_place function in placement_proc.h, and a DYN_get_liberal_ratio function further down in this file)
static inline Ratio DYN_get_ratio(const dyn_t dyn){
  if (dyn.type==INT_TYPE)
    return make_ratio(dyn.int_number, 1);

  if (dyn.type==RATIO_TYPE)
    return *dyn.ratio;

  RError("DYN_Get_ratio: dyn (type: %d) can not be converted to a ratio", dyn.type);

  return make_ratio(0,1);      
}


#include "hashmap_proc.h"




/*********************************************************************
	dyn.h
*********************************************************************/

#include "Dynvec_proc.h"

static inline const char *DYN_type_name(enum DynType type){
  switch(type){
    case UNINITIALIZED_TYPE:
      return "UNINITIALIZED_TYPE";
    case STRING_TYPE:
      return "STRING_TYPE";
    case SYMBOL_TYPE:
      return "SYMBOL_TYPE";
    case INT_TYPE:
      return "INT_TYPE";
    case FLOAT_TYPE:
      return "FLOAT_TYPE";
    case HASH_TYPE:
      return "HASH_TYPE";
    case ARRAY_TYPE:
      return "ARRAY_TYPE";
    case RATIO_TYPE:
      return "RATIO_TYPE";
    case FUNC_TYPE:
      return "FUNC_TYPE";
    case INSTRUMENT_TYPE:
      return "INSTRUMENT_TYPE";
    case FILEPATH_TYPE:
      return "FILEPATH_TYPE";
    case BLUB_TYPE:
      return "BLUB_TYPE";
    case BOOL_TYPE:
      return "BOOL_TYPE";
  }
  RError("Unknown dyn type: %d", type);
  return "";
}

#ifdef __cplusplus
static inline const char *DYN_type_name(const dyn_t &type){
  return DYN_type_name(type.type);
}
#endif

static inline enum DynType DYN_get_type_from_name(const char* type_name){
  for(int i=0;i<NUM_DYNTYPE_TYPES;i++)
    if(!strcmp(type_name, DYN_type_name((enum DynType)i)))
      return (enum DynType)i;

  RError("Unknown dyn type name: \"%s\"",type_name);
  return INT_TYPE;
}

// code copied from s7.c
static inline bool doubles_are_equal(double x, double y){

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wfloat-equal"
  if (x == y)
    return(true);
#pragma GCC diagnostic pop

  const double equivalent_float_epsilon = 1.0e-15; // Almost std::numeric_limits<double>::epsilon();
  
  const double eps = equivalent_float_epsilon;
  const double diff = fabs(x - y);

  if (diff <= eps)
    return(true);
#ifdef __cplusplus
  if ((std::isnan(x)) || (std::isnan(y)))
    return((std::isnan(x)) && (std::isnan(y)));
#else
  if ((isnan(x)) || (isnan(y)))
    return((isnan(x)) && (isnan(y)));
#endif
  // I don't understand what's happening below here, but I trust Bill.
  
  if (x < 0.0)
    x = -x;
  
  return((x > 1.0) &&
	 (diff < (x * eps)));
}


static inline bool DYN_equal(const dyn_t a1, const dyn_t a2){
  R_ASSERT_NON_RELEASE(g_dyn_true.bool_number==true);
  R_ASSERT_NON_RELEASE(g_dyn_false.bool_number==false);

  if (a1.type!=a2.type)
    return false;
  
  switch(a1.type){
    case UNINITIALIZED_TYPE:
      R_ASSERT(false);
      return false;
    case STRING_TYPE:
      if (a1.string==a2.string)
        return true;
      else if (a1.string==NULL || a2.string==NULL)
        return false;
      else
        return !wcscmp(a1.string, a2.string);
    case SYMBOL_TYPE:
      if (a1.symbol==a2.symbol)
        return true;
      else if (a1.symbol==NULL || a2.symbol==NULL){
        R_ASSERT(false);
        return false;
      }else
        return !strcmp(a1.symbol, a2.symbol);
    case INT_TYPE:
      return a1.int_number==a2.int_number;
    case FLOAT_TYPE:
      return doubles_are_equal(a1.float_number, a2.float_number);
    case HASH_TYPE:
      return HASH_equal(a1.hash, a2.hash);
    case ARRAY_TYPE:
      return DYNVEC_equal(a1.array, a2.array);
    case RATIO_TYPE:
      return RATIO_equal(*a1.ratio, *a2.ratio);
    case FUNC_TYPE:
      return a1.func==a2.func;
    case INSTRUMENT_TYPE:
      return a1.instrument.id==a2.instrument.id;
    case FILEPATH_TYPE:
      if (a1.filepath.id==a2.filepath.id)
        return true;
      else if (a1.filepath.id==NULL || a2.filepath.id==NULL){
        R_ASSERT_NON_RELEASE(false);
        return false;
      }else
        return !wcscmp(a1.filepath.id, a2.filepath.id);
    case BLUB_TYPE:
      if (a1.blub->size!=a2.blub->size)
        return false;
      if (a1.blub->data==a2.blub->data)
        return true;
      else if (a1.blub->data==NULL || a2.blub->data==NULL){
        R_ASSERT_NON_RELEASE(false); // Not sure.
        return false;
      }else
        return 0==memcmp(a1.blub->data, a2.blub->data, (size_t)a1.blub->size);
    case BOOL_TYPE:
      return a1.bool_number==a2.bool_number;
  }

  R_ASSERT(false);
  return false;
}

static inline dyn_t DYN_create_string_dont_copy(const wchar_t *string){
  dyn_t a;
  a.type = STRING_TYPE;
  if(string==NULL){
    R_ASSERT(false);
    string = L"";
  }
  a.string = string;
  return a;
}

static inline dyn_t DYN_create_string(const wchar_t *string){
  return DYN_create_string_dont_copy(STRING_copy(string));
}

#ifdef USE_QT4
static inline dyn_t DYN_create_string(const QString &string){
  return DYN_create_string_dont_copy(STRING_create(string));
}
#endif

static inline dyn_t DYN_create_string_from_chars(const char *chars){
  return DYN_create_string_dont_copy(STRING_create(chars));
}

static inline dyn_t DYN_create_symbol_dont_copy(const char *symbol){
  dyn_t a;
  a.type = SYMBOL_TYPE;
  if(symbol==NULL){
    R_ASSERT(false);
    symbol = "";
  }
  a.symbol = symbol;
  return a;
}

static inline dyn_t DYN_create_symbol(const char *symbol){
  return DYN_create_symbol_dont_copy(talloc_strdup(symbol));
}

#ifdef USE_QT4
static inline dyn_t DYN_create_symbol(QString symbol){
  return DYN_create_symbol(symbol.toUtf8().constData());
}
#endif

static inline dyn_t DYN_create_func(func_t *func){
  dyn_t a;
  a.type = FUNC_TYPE;
  a.func = func;
  return a;  
}

static inline bool DYN_check_type(const dyn_t dyn, enum DynType type, const char *error){
  if (dyn.type != type){
    handleError("%sExpected %s, found %s", error, DYN_type_name(type), DYN_type_name(dyn.type));
    return false;
  }
  
  return true;
}

static inline bool HASH_check_type(const hash_t *hash, const char *key, enum DynType type, const char *error){
  if (!HASH_has_key(hash, key)){
    handleError("%sCould not find \"%s\" in hash table", error, key);
    return false;
  }    

  return DYN_check_type(HASH_get_dyn(hash, key), type, error);
}


static inline dyn_t DYN_create_int(int64_t int_number){
  dyn_t a;
  a.type = INT_TYPE;
  a.int_number = int_number;
  return a;
}

static inline dyn_t DYN_create_instrument(instrument_t instrument){
  dyn_t a;
  a.type = INSTRUMENT_TYPE;
  a.instrument = instrument;
  return a;
}

static inline dyn_t DYN_create_filepath(filepath_t filepath){
  dyn_t a;
  a.type = FILEPATH_TYPE;
  a.filepath = make_filepath(STRING_copy(filepath.id));
  return a;
}

static inline dyn_t DYN_create_blub(int size, void *data){
  dyn_t a;
  a.type = BLUB_TYPE;
  a.blub = (blub_t*)talloc(sizeof(blub_t));
  a.blub->size = size;
  a.blub->data = data;
  return a;
}

static inline dyn_t DYN_create_blub_with_data(int size){
  dyn_t a;
  a.type = BLUB_TYPE;
  a.blub = (blub_t*)talloc(sizeof(blub_t));
  a.blub->size = size;
  a.blub->data = talloc_atomic(size);
  return a;
}

static inline dyn_t DYN_create_blub_and_copy_data(int size, void *data){
  return DYN_create_blub(size, tcopy2_atomic(data, (size_t)size));
}

/*
static inline dyn_t DYN_create_bool(bool bool_number){
  dyn_t a;
  a.type = BOOL_TYPE;
  a.bool_number = bool_number;
  return a;
}
*/

#define DYN_create_bool(B) ((B) ? g_dyn_true : g_dyn_false)


static inline dyn_t DYN_create_float(double float_number){
#if !defined(RELEASE)
  if(!sane_isnormal(float_number))
    abort();
#endif
  
  dyn_t a;
  a.type = FLOAT_TYPE;
  a.float_number = float_number;
  return a;
}

static inline bool DYN_is_float(const dyn_t a){
  return a.type==FLOAT_TYPE;
}

static inline bool DYN_is_number(const dyn_t a){
  if (a.type==INT_TYPE)
    return true;
  if (a.type==FLOAT_TYPE)
    return true;
  if (a.type==RATIO_TYPE)
    return true;
  return false;
}

static inline double DYN_get_double_from_number(const dyn_t a){
  R_ASSERT_NON_RELEASE(DYN_is_number(a));
  if (a.type==INT_TYPE)
    return (double)a.int_number;
  if (a.type==FLOAT_TYPE)
    return a.float_number;
  if (a.type==RATIO_TYPE)
    return (double)a.ratio->num / (double)a.ratio->den;

  RError("DYN_get_double_from_number: 'a' is not a number, but a %s", DYN_type_name(a.type));
  return 0.0;
}

static inline int64_t DYN_get_int64_from_number(const dyn_t a){
  R_ASSERT_NON_RELEASE(DYN_is_number(a));
  if (a.type==INT_TYPE)
    return a.int_number;
  if (a.type==FLOAT_TYPE)
    return (int64_t)a.float_number;
  if (a.type==RATIO_TYPE)
    return (int64_t)((double)a.ratio->num / (double)a.ratio->den);

  RError("DYN_get_double_from_number: 'a' is not a number, but a %s", DYN_type_name(a.type));
  return 0;
}

static inline dyn_t DYN_create_ratio(const Ratio ratio){
  dyn_t a;
  a.type = RATIO_TYPE;
  a.ratio = (Ratio*)tcopy(&ratio);
  return a;
}

static inline dyn_t DYN_create_place(const Place place){
  return DYN_create_ratio(RATIO_minimize(make_ratio((int)place.counter + place.line*(int)place.dividor, (int)place.dividor)));
}

static inline bool DYN_is_ratio(const dyn_t a){
  if (a.type==INT_TYPE)
    return true;
  if (a.type==RATIO_TYPE)
    return true;
  
  return false;
}

// Also allows strings and floats.
static inline StaticRatio DYN_get_static_ratio(const dyn_t dyn){
  if (dyn.type==INT_TYPE)
    return make_static_ratio(dyn.int_number, 1);

  if (dyn.type==RATIO_TYPE)
    return make_static_ratio_from_ratio(*dyn.ratio);

  else if (dyn.type==FLOAT_TYPE)
    return make_static_ratio((int)(dyn.float_number*1000), 1000);

  else if (dyn.type==STRING_TYPE)
    return STATIC_RATIO_from_string(dyn.string);
    
  RError("DYN_Get_ratio: dyn (type: %d) can not be converted to a ratio", dyn.type);

  return make_static_ratio(0,1);      
}

static inline bool DYN_is_liberal_ratio(const dyn_t a){
  return a.type==INT_TYPE || a.type==RATIO_TYPE || a.type==FLOAT_TYPE || a.type==STRING_TYPE;
}




/*
Must include placement_proc.h to get this function.
static inline Place DYN_get_place(const dyn_t dyn){
  Ratio ratio = DYN_get_ratio(dyn);
  return place_from_64b(ratio.num, ratio.den);
}
*/

static inline dyn_t DYN_create_hash(hash_t *hash){
  dyn_t a;
  a.type = HASH_TYPE;
  a.hash = hash;
  return a;
}

static inline dyn_t DYN_create_array(const dynvec_t dynvec){
  dyn_t a;
  a.type = ARRAY_TYPE;
  a.array = (dynvec_t*)tcopy(&dynvec);
  return a;
}

static inline dyn_t DYN_copy(const dyn_t a){
  if (a.type==HASH_TYPE)
    return DYN_create_hash(HASH_copy(a.hash));
  else if (a.type==ARRAY_TYPE)
    return DYN_create_array(DYNVEC_copy(a.array));
  else if (a.type==RATIO_TYPE)
    return DYN_create_ratio(*a.ratio);
  else if (a.type==BLUB_TYPE)
    return DYN_create_blub(a.blub->size, a.blub->data);
  else
    return a;
}



/*********************************************************************
	logtype.h
*********************************************************************/

// Note that logtype values have been saved to disk for some time. These enum values can not change values without doing conversion when loading old songs.
enum{
  LOGTYPE_IRRELEVANT = -2,    // Logtype value of a last node.
  LOGTYPE_IMMEDIATELY = -1,   // Like this: f(x) = f(1),  [ 0 <= x <= 1 ]
  LOGTYPE_LINEAR = 0,         // Like this: f(x) = x
  LOGTYPE_MIN = 1,            // Probably something like this: f(x)=x^0.1
  LOGTYPE_ALSO_LINEAR = 100,  // Something like this: f(x) = x
  LOGTYPE_MAX = 200,          // Probably something like this: f(x)=x^10
  LOGTYPE_HOLD = 201          // Like this: f(x) = f(0), [ 0 <= x <= 1 ]. Currently the only understood value, together with LOGTYPE_LINEAR.
};

typedef enum {
  FX_start = 0,   // Used by automation and envelope controller
  FX_middle = 1,  // Used by automation and envelope controller
  FX_end = 2,     // Used by automation and envelope controller
  FX_single = 3,
  //FX_no_fx = 4; // No effect must be sent out. Used by seqtrack automation.
} FX_when;

static inline const char *get_FX_when_name(FX_when when){
  switch(when){
    case FX_start: return "FX_start";
    case FX_middle: return "FX_middle";
    case FX_end: return "FX_end";
    case FX_single: return "FX_single";
    default:
      R_ASSERT_NON_RELEASE(false);
      return "Error";
  }  
}




#ifdef __cplusplus

namespace r{

template <typename ValType>
class RT_TimeData_Player_Cache{

public:
  int _curr_pos = 0; // vector pos.

  template <class SeqBlockT>
  friend struct RT_TimeData_Cache_Handler;
  
#if defined(__GNUC__) && __GNUC__ < 8
public: // "friend struct RT_TimeData_Cache_Handler" above doesn't work for gcc 7.
#else
private:
#endif
  
  ValType _last_value = 0; // last value returned from TimeData::get_value();
  int _last_play_id = -1; // Value of pc->play_id when last_value was returned.
};


struct DefaultWriterFinalizerArguments {
};

template <class T, class SeqBlockT, typename WriterFinalizerArguments> class TimeData;
}

#endif


/*********************************************************************
	sndfile.h
*********************************************************************/

#if defined(INCLUDE_SNDFILE_OPEN_FUNCTIONS)

#if defined(FOR_WINDOWS)
#include <windows.h>
#define ENABLE_SNDFILE_WINDOWS_PROTOTYPES 1
#endif

#include <sndfile.h>

extern LANGSPEC SNDFILE *radium_sf_open(filepath_t filename, int mode, SF_INFO *sfinfo);
#ifdef USE_QT4
extern SNDFILE *radium_sf_open(QString filename, int mode, SF_INFO *sfinfo);
#endif

#endif


/*********************************************************************
	symbol.h
*********************************************************************/

extern vector_t g_symbols;

typedef struct{
  const char *name;
} symbol_t;

static inline const symbol_t *get_symbol(const char *name){
  int i;
  for(i=0;i<g_symbols.num_elements;i++){
    symbol_t *symbol = (symbol_t*)g_symbols.elements[i];
    if (!strcmp(name, symbol->name))
      return symbol;
  }

  symbol_t *symbol = (symbol_t*)malloc(sizeof(symbol_t));
  symbol->name = strdup(name);
  VECTOR_push_back(&g_symbols, symbol);

  return (const symbol_t*)symbol;
}

static inline void set_symbol_name(const symbol_t *symbol, const char *new_name){
  R_ASSERT_RETURN_IF_FALSE(new_name != NULL);
  free((void*)symbol->name);
#ifdef __cplusplus
  const_cast<symbol_t*>(symbol)->name = strdup(new_name);
#else
  ((symbol_t*)symbol)->name = strdup(new_name);
#endif
}
                                   

/*********************************************************************
	quantitize.h
*********************************************************************/

typedef struct{
  StaticRatio quant;
  /*
    // In scheme, we read these three variables directly from the gui now.
  bool quantitize_start;
  bool quantitize_end;
  bool keep_note_length;
  */
  int type;
} quantitize_options_t;




// Fade types take from ardour. The numbers must start at 0, and increase by 1 so that they can easily be iterated.
enum FadeShape{
  FADE_CUSTOM = 0,
  FADE_LINEAR,
  FADE_FAST,
  FADE_SLOW,
  FADE_CONSTANT_POWER,
  FADE_SYMMETRIC // If not last anymore, update NUM_FADE_SHAPES definition below.
};
#define NUM_FADE_SHAPES (FADE_SYMMETRIC+1)


static inline const char *fade_shape_to_string(enum FadeShape fade_shape){
  switch(fade_shape){
    case FADE_CUSTOM: return "Custom";
    case FADE_LINEAR: return "Linear";
    case FADE_FAST: return "Fast";
    case FADE_SLOW: return "Slow";
    case FADE_CONSTANT_POWER: return "Constant Power";
    case FADE_SYMMETRIC: return "Symmetric";
  }

  R_ASSERT(false);
  return fade_shape_to_string(FADE_LINEAR); // Fewer assertions than FADE_CUSTOM down the lane
}


static inline bool string_to_fade_shape2(const char *string, enum FadeShape *ret){
  for(int i = 0 ; i<NUM_FADE_SHAPES ; i++){
    enum FadeShape shape = (enum FadeShape)i;
    if (!strcmp(string, fade_shape_to_string(shape))){
      *ret = shape;
      return true;
    }
  }

  return false;
}

static inline enum FadeShape string_to_fade_shape(const char *string){
  enum FadeShape ret;
  if (string_to_fade_shape2(string, &ret))
    return ret;
  
  R_ASSERT(false);
  return FADE_LINEAR;  // Fewer assertions than FADE_CUSTOM down the lane
}


/*********************************************************************
	velocities.h
*********************************************************************/

#define MAX_VELOCITY (1<<16)

/*
struct Velocities{
	struct ListHeader3 l;

	int velocity;
	int logtype;
};
#define NextVelocity(a) ((struct Velocities *)((a)->l.next))
*/

struct Tracks;


static inline double VELOCITY_get(int velocity){
  return scale_double(velocity,0,MAX_VELOCITY,0,1);
}

#ifdef __cplusplus
namespace r{

  struct TimeDataDataTypeNoVal {
    using TType = int;
    
    Ratio _time;
    
    int get_logtype(void) const {
      R_ASSERT_NON_RELEASE(false);
      return 0;
    }
    
    int get_val(void) const {
      R_ASSERT_NON_RELEASE(false);
      return 0;
    }
    
    const Ratio &get_time(void) const {
      return _time;
    }

    void set_time(const Ratio &time) {
      _time = time;
    }
      
    
    TimeDataDataTypeNoVal(){
    }
    
    TimeDataDataTypeNoVal(Ratio time)
      : _time(time)
    {
    }  
     
  };

  template <typename ValType>
  struct TimeDataDataType {
    using TType = ValType;
    
    Ratio _time;
    ValType _val;
    int _logtype;
      
    ValType get_val(void) const {
      return _val;
    }
    
    int get_logtype(void) const {
      return _logtype;
    }
    
    const Ratio &get_time(void) const {
      return _time;
    }

    void set_time(const Ratio &time) {
      _time = time;
    }
          
    TimeDataDataType(){
    }
    
    TimeDataDataType(Ratio time, ValType val, int logtype)
      : _time(time)
      , _val(val)
      , _logtype(logtype)
    {
    }  
     
  };

  template <typename ValType>
  struct TimeDataDataTypeRef : TimeDataDataType<ValType>, radium::Deletable {
    std::atomic<int> _num_references;

    TimeDataDataTypeRef(const Ratio &ratio, ValType val, int logtype = LOGTYPE_LINEAR)
      : TimeDataDataType<ValType>(ratio, val, logtype)
      , _num_references(0)
    {}
  };
    
  // Like shared_ptr, but can be used as datatype for TimeData.
  //
  template <class T>
  struct TimeData_shared_ptr
  {
    using TType = T;
    using ValType = typename T::TType;
  
    static_assert(std::is_base_of<TimeDataDataTypeRef<ValType>, T>::value, "T must be a subclass of r::TimeDataDataTypeRef");
  
    T *_t;

    int get_logtype(void) const {
      return _t->get_logtype();
    }
  
    ValType get_val(void) const {
      return _t->get_val();
    }
  
    const Ratio &get_time(void) const {
      return _t->get_time();
    }

    void set_time(const Ratio &time) {
      _t->set_time(time);
    }

    TimeData_shared_ptr()
      : _t(nullptr)
    {
    }

    explicit TimeData_shared_ptr(T *t)  __attribute__((__nonnull__))
      : _t(t)
    {
      if (_t)
        _t->_num_references++;
    
      //printf("    Constr: %d - %p (%p)\n", _t->_num_references.load(), _t, this);
    }

    // copy constructor
    TimeData_shared_ptr(const TimeData_shared_ptr &other)
      : _t(other._t)
    {
      if (_t)
        _t->_num_references++;
    
      //printf("    Copy-constr: %d - %p. (%p -> %p / %p -> %p)\n", _t->_num_references.load(), _t, &other, this, other._t, _t);
    }

    // copy assignment
    TimeData_shared_ptr& operator=(const TimeData_shared_ptr &other){
      
      this->_t = other._t;

      if (_t)
        _t->_num_references++;

      //printf("    Copy-assign: %d - %p. (%p -> %p)\n", _t->_num_references.load(), _t, &other, this);
    
      return *this;
    }
  
    // move constructor
    TimeData_shared_ptr(TimeData_shared_ptr&& other)
    {
      this->_t = other._t;

      //printf("    Move-constr: %d - %p (%p -> %p)\n", this->_t->_num_references.load(), _t, &other, this);
    
      other._t = NULL;
    }

    // Move assignment
    TimeData_shared_ptr& operator=(TimeData_shared_ptr&& a)
    {
      if (&a == this){
        R_ASSERT_NON_RELEASE(false); // Interested in knowing when this happens.
        return *this;
      }
    
      if (_t != NULL){
        //R_ASSERT_NON_RELEASE(false); // Interested in knowing when this happens.
        //printf("                      Deleting %p in move assigment\n", _t);
        fprintf(stderr, "           c1\n");
        cleanup();
      }
    
      _t = a._t;
      a._t = nullptr;

      //printf("    Move-assign: %d - %p (%p -> %p)\n", this->_t->_num_references.load(), _t, &a, this);
    
      return *this;
    }

  
    ~TimeData_shared_ptr()
    {
      //fprintf(stderr, "           c2\n");
      cleanup();
    }

  
  private:
  
    void cleanup(void){
      if (_t==NULL){
        //      abort();
        return; // (Happens after using move constructor)
      }
    
      R_ASSERT(_t->_num_references > 0);
      
      //printf("    ~TimeData_shared_ptr: %d - %p (%p)\n", _t->_num_references.load()-1, _t, this);
    
      if ((--_t->_num_references)==0){

#if defined(TEST_TIMEDATA_MAIN) || defined(TEST_RADIUM_VECTOR_MAIN)
        g_num_freed++;
#endif

        if (THREADING_is_main_thread()) {
          
          delete _t;
          
        } else {
          
          R_ASSERT_NON_RELEASE(THREADING_is_RT()); // If not, something unusual has happened.
          
          RT_free(_t);
          
        }
        
      }
    }

  
  public:
    explicit operator bool() const {
      return _t != NULL;
    }
  
    T *operator->() const {
      return _t;
    }
  
    const T *get(void) const {
      return _t;
    }

    T *get_mutable(void) const {
      return _t;
    }

    void reset(T *new_value) {
      fprintf(stderr, "           c3\n");
      cleanup();

      if (new_value)
        new_value->_num_references++;

      _t = new_value;
    }
    
    bool operator==(const TimeData_shared_ptr &other) const{
      return _t==other._t;
    }
  
    bool operator!=(const TimeData_shared_ptr &other) const{
      return _t!=other._t;
    }
  };

  extern int64_t g_node_id;
  
  struct NodeId {
    int64_t _id;
    NodeId()
      : _id(g_node_id++)
    {
      R_ASSERT_NON_RELEASE(THREADING_is_main_thread()); // because of g_node_id. If needed, we can make g_node_id atomic.
    }

    // We could have changed the code so that it wouldn't be necessary to call 'gen_new_id' (for instance by almost always
    // setting a unique id for the node in TimeData::Writer::add. However, we don't need to call this function very often, it's just
    // for the clipboard and perhaps a few other places, so it's probably best to do it manually.
    NodeId &gen_new_id(void)
    {
      R_ASSERT_NON_RELEASE(THREADING_is_main_thread()); // because of g_node_id. If needed, we can make g_node_id atomic.
      _id = g_node_id++;

      return *this;
    }
    
    /*
    void operator delete(void * p) {
      NodeId *node = static_cast<NodeId*>(p);
      
      if (node==NULL)
        return;
      
      ::operator delete(p);
    }
    */
  };
  
  struct Velocity : NodeId, TimeDataDataType<int> {
    Velocity(Ratio time, int val, int logtype = LOGTYPE_LINEAR)
      : TimeDataDataType<int>(time, R_BOUNDARIES(0, val, MAX_VELOCITY), logtype)
    {}    
  };

  struct VelocitySeqBlock : public RT_TimeData_Player_Cache<typeof(Velocity::_val)> {
  };
  
  using VelocityTimeData = TimeData<Velocity, VelocitySeqBlock, DefaultWriterFinalizerArguments>;
}

#include "TimeData.hpp"

#endif


#define MIN_PATCHVOICE_CHANCE 0
#define MAX_PATCHVOICE_CHANCE (0x100)


/*********************************************************************
	pitches.h
*********************************************************************/

/*
struct Pitches{
	struct ListHeader3 l;
  
	float note;
	int logtype;

        int chance;
};
#define NextPitch(a) ((struct Pitches *)((a)->l.next))
*/

#ifdef __cplusplus

namespace r{
struct NoteTimeData;
}

extern r::NoteTimeData *g_dummy_notes;

namespace r{

struct Pitch : NodeId, TimeDataDataType<float> {
  int _chance;

  Pitch(Ratio time, float pitch, int logtype = LOGTYPE_LINEAR, int chance = MAX_PATCHVOICE_CHANCE)
    : TimeDataDataType<float>(time, pitch, logtype)
    , _chance(chance)
  {}
};

struct PitchSeqBlock : RT_TimeData_Player_Cache<typeof(Pitch::_val)> {
  bool _enabled = true; // Can be false if pitch._chance < MAX_PATCHVOICE_CHANCE.
};

struct PitchWriterFinalizerArguments {
  NoteTimeData *_notes;

  PitchWriterFinalizerArguments()
  {
    R_ASSERT_NON_RELEASE(false); // TIP: If it's not necessary to find pitchmin/pitchmax in a track after changing the pitch, use 'g_dummy_notes' as writer finalizer argument.
  }
  
  PitchWriterFinalizerArguments(NoteTimeData *notes)
    : _notes(notes)
  {
  }
};

struct PitchTimeData : public TimeData<Pitch, PitchSeqBlock, PitchWriterFinalizerArguments>{
  
  // Sets _min_pitch, and _max_pitch;
  void writer_finalizer_after(const Writer &writer, PitchWriterFinalizerArguments &args) override;
};

} // namespace r

#endif


/*********************************************************************
	notes.h
*********************************************************************/

#define NOTE_END_NORMAL 128
enum{
  NOTE_MUL=NOTE_END_NORMAL,
  NOTE_STP,
  NOTE_MUR
};


struct Notes{
	struct ListHeader3 l;

	float note;
	int pitch_first_logtype;
  
	int velocity;
	int velocity_first_logtype;
  
	Ratio end;

        int chance;
  
        //struct Velocities *velocities;
	int velocity_end;

#ifdef __cplusplus
        r::VelocityTimeData *_velocities;
#else
        void *_velocities;
#endif

        //struct Pitches *pitches;
        float pitch_end; // If pitch_end==0 and pitches==NULL, there's no pitch changes for this note.

#ifdef __cplusplus
       r::PitchTimeData *_pitches;
       //r::TimeData<r::Pitch> *_pitches;
#else
        void *_pitches;
#endif
  
  /*
	struct Velocities first_velocity; // used by nodelines
	struct Velocities last_velocity; // used by nodelines
  */
  
        int polyphony_num; //subtrack;

	int noend;

        bool has_sent_seqblock_volume_automation_this_block;
#if !defined(RELEASE)
        bool has_automatically_sent_seqblock_volume_automation_this_block;
#endif
        bool scheduler_may_send_velocity_next_block;
        bool scheduler_must_send_velocity_next_block; // Can only be set to true if sheduler_may_send_velocity_next_block==true.
        float curr_velocity; // Don't think this variable needs to be stored in seqblock->playing_notes. It seems to only contain note->velocity*track->volume*track->mute
        int64_t curr_velocity_time;
  
        bool scheduler_may_send_pitch_next_block;
        bool scheduler_must_send_pitch_next_block; // Can only be set to true if sheduler_may_send_pitch_next_block==true.
        float curr_pitch;
        int64_t curr_pitch_time;

        bool pianonote_is_selected;
  
        int64_t id;
};
#define NextNote(a) ((struct Notes *)((a)->l.next))

#ifdef __cplusplus

extern int64_t new_note_id(void);

namespace r{

struct NoteData{
  int _pitch_first_logtype;
  
  int _velocity;
  int _velocity_first_logtype;
  
  Ratio _end;

  int _velocity_end;

  int _chance;
  
  float _pitch_end; // If pitch_end==0 and pitches==NULL, there's no pitch changes for this note.

  int _polyphony_num; //subtrack;

  bool _noend;

  bool _has_sent_seqblock_volume_automation_this_block;
#if !defined(RELEASE)
  bool _has_automatically_sent_seqblock_volume_automation_this_block;
#endif
  bool _scheduler_may_send_velocity_next_block;
  bool _scheduler_must_send_velocity_next_block; // Can only be set to true if sheduler_may_send_velocity_next_block==true.
  float _curr_velocity; // Don't think this variable needs to be stored in seqblock->playing_notes. It seems to only contain note->velocity*track->volume*track->mute
  int64_t _curr_velocity_time;
  
  bool _scheduler_may_send_pitch_next_block;
  bool _scheduler_must_send_pitch_next_block; // Can only be set to true if sheduler_may_send_pitch_next_block==true.
  float _curr_pitch;
  int64_t _curr_pitch_time;

  bool _pianonote_is_selected;
};

#if !defined(RELEASE) || defined(TEST_TIMEDATA_MAIN)
  struct Note;
  extern int g_num_allocated_notes;
  void debug_note_added(const r::Note *note, const char *where);
  void debug_note_removed(const r::Note *note);
#endif

struct Note : NodeId, public r::TimeDataDataTypeRef<float> {

  /*
  Note(){
        debug_note_added(this, "Empty");
  }
  */
  
  Note(Ratio time, float note, int velocity)
    : TimeDataDataTypeRef<float>(time, note)
    , _id(new_note_id())
  {
    debug_note_added(this, "A");
    
    memset(&d, 0, sizeof(NoteData));
    
    d._velocity = velocity;
    d._velocity_end = velocity;
    d._chance = MAX_PATCHVOICE_CHANCE;
  }

  Note(const Note *other, bool copy_velocities_and_pitches = true)
    : TimeDataDataTypeRef<float>(other->get_time(), other->_val)
    , _id(new_note_id())
  {
    debug_note_added(this, "B");
    
    d = other->d;

    // TOPIC: Should we use shared_ptr for _velocities and _pitches instead of copying the content of them?
    // (I guess copying doesn't take up that much time, so it's probably not worth the complication)
    if (copy_velocities_and_pitches){
      _velocities.replace_with(&other->_velocities);

      _pitches.replace_with(&other->_pitches, g_dummy_notes);
    }
  }
      
  Note(const Note &other)
    : Note(&other)
  {
    debug_note_added(this, "C");
  }
  
  Note& operator=(const Note &other){
    debug_note_added(this, "D");
    
    set_time(other.get_time());
    _val = other.get_val();
      
    d = other.d;
    
    _id = new_note_id();
    
    return *this;
  }

#if !defined(RELEASE)
  ~Note()
  {
    debug_note_removed(this);
  }
#endif

  int64_t get_node_id(void) const {
    return NodeId::_id;
  }
  
  VelocityTimeData _velocities;
  PitchTimeData _pitches;
  
  NoteData d;

#pragma clang diagnostic push
#pragma clang diagnostic ignored "-Wshadow-field"

  int64_t _id;

#pragma clang diagnostic pop
};

struct NoteSeqBlock : public RT_TimeData_Player_Cache<typeof(Note::_val)> {
};


using NotePtr = TimeData_shared_ptr<Note>;

using LineNotes = radium::Vector<r::Note*, radium::AllocatorType::STD, 1>;
  
struct NoteTimeData : public TimeData<NotePtr, NoteSeqBlock>{
  int _polyphony = 1;

  float _min_pitch = 0;
  float _max_pitch = 128;
  
  float _min_display_pitch = 0;
  float _max_display_pitch = 128;

  // Vector of vector of r::Note pointers.
  // Line number is used for indexing the elements of the outer vector.
  // The inner vectors contains pointer to all notes playing at that line.
  // (Note that the size of the vector is equal to the last line (plus 1) that a note is playing at,
  //  and NOT the number of lines in the block.)
  //
  // Used by the player and gfx renderer.
  //
  // Updated in the writer_finalizer method.
  //
  // (TODO/FIX: Are the indinvidual elements in the vector (the LineNotes*s) freed?)
  //
  radium::Vector<LineNotes*> _line_notes;

  // Number of _valid_ elements in _line_notes. (size of the vector might be larger, but those elements are not valid)
  int _num_valid_elements_in_line_notes;

  template <class Iterator>
  void set_note_min_max_pitch(const Iterator &iterator){
      float min_pitch = 10000.0f;
      float max_pitch = -1.0f;

      int num_pitches = 0;
  
      // find min_pitch and max_pitch
      for(const r::NotePtr &note : iterator){
        min_pitch = R_MIN(note->get_val(), min_pitch);
        max_pitch = R_MAX(note->get_val(), max_pitch);
        num_pitches ++;
        if (note->d._pitch_end > 0){
          min_pitch = R_MIN(note->d._pitch_end, min_pitch);
          max_pitch = R_MAX(note->d._pitch_end, max_pitch);
          num_pitches ++;
        }

        r::PitchTimeData::Reader reader(&note->_pitches);
        for(const r::Pitch &pitch : reader){
          min_pitch = R_MIN(pitch._val, min_pitch);
          max_pitch = R_MAX(pitch._val, max_pitch);
          num_pitches ++;
        }
      }

      _min_pitch = min_pitch;
      _max_pitch = max_pitch;

      if (num_pitches <= 3) {
    
        _min_display_pitch = 0;
        _max_display_pitch = 127;
    
      }else{
    
        float pitch_range = max_pitch - min_pitch;
    
        min_pitch = min_pitch - pitch_range/8.0f;
        if(min_pitch < 0)
          min_pitch = 0;
    
        max_pitch = max_pitch + pitch_range/8.0f;
        if(max_pitch > 127)
          max_pitch = 127;
    
        _min_display_pitch = min_pitch;
        _max_display_pitch = max_pitch;
      }
  }
    
  // Sets _polyphony, _min_pitch, _max_pitch, and _line_notes.
  void writer_finalizer_before(Writer &writer, DefaultWriterFinalizerArguments &args) override;
  
  void sortit(TimeDataVector *vector) override;

  bool insert_ratio(Writer &writer, const Ratio &where_to_start, const Ratio &how_much);
};

  /*
    ModifyNote is used to make it easier to modify a current note in a track.

    The reason for using this class is that we can NOT modify a note in a track directly
    since the note might be used by the player (or something else) in a different thread at the same time.
    Therefore we first have to make a copy, and afterwards replace the old note with the new one in the Writer.

    This is magically taken care of here.

    Also note that the id is reused in the new note (for practical reasons), so the old raw note (NOT the noteptr) must always be discarded afterwards.
   */
class ModifyNote {
  NotePtr &_noteptr;

  Note *_note;

  Ratio _time;
  
public:

  enum class Type{
                  CAN_MODIFY_TIME,
                  CAN_NOT_MODIFY_TIME
  };
  
  const Type _type;

public:
  
  // Note: This constructor can only be used if 'noteptr' is already a reference to an element in a Writer.
  ModifyNote(NotePtr &noteptr, Type type = Type::CAN_NOT_MODIFY_TIME)
    : _noteptr(noteptr)
    , _note(new Note(noteptr.get()))
    , _time(noteptr->get_time())
    , _type(type)
  {
    _note->_id = noteptr->_id; // Very ugly, but it's probably fine. Fishing out the id of a modified note is very cumbersome.
    
    R_ASSERT(_noteptr.get() == noteptr.get());
  }

  ModifyNote(const r::NoteTimeData::Writer &writer, const NotePtr &noteptr, Type type = Type::CAN_NOT_MODIFY_TIME)
    : ModifyNote(writer.find([&noteptr](const r::NotePtr &maybe)
                   {
                     return maybe.get()==noteptr.get();
                   }),
                 type)
  {
  }

  ~ModifyNote(){
    R_ASSERT(_type==Type::CAN_MODIFY_TIME || _note->get_time() == _time);
             
    _noteptr.reset(_note);
  }

  Note *operator->() const {
    return _note;
  }

  Note *get(void) const {
    return _note;
  }

  NotePtr &get_noteptr(void) const {
    return _noteptr;
  }
};
}

#endif



/*********************************************************************
	patch.h
*********************************************************************/

struct Instruments;

union SuperType{
  void *pointer;
  const void *const_pointer;
  int64_t int_num;
  int32_t int32_num;
  uint32_t uint32_num; // Only used for midi messages
  double float_num;
  bool bool_num;
};

enum TimeFormat{
  TIME_IN_BEATS = 0,
  TIME_IN_MS = 1,
  TIME_IN_S = 2
};

struct PatchVoice{
  bool is_on;
  float transpose;
  float volume;
  float start;
  float length;
  float pan;
  enum TimeFormat time_format;
  float chance;

  bool only_set_new_transpose_when_note_on; // protect write by player lock
  bool only_set_new_volume_when_note_on; // protect write by player lock
  bool only_set_new_pan_when_note_on; // protect write by player lock
};

#define NUM_PATCH_VOICES 7
#define MAX_NUM_EVENT_RECEIVERS 64
#define MAX_NOTE_INTENCITY 20

#define MIN_PATCHVOICE_PAN -90
#define MAX_PATCHVOICE_PAN 90

#define MIN_PATCHVOICE_VOLUME -35
#define MAX_PATCHVOICE_VOLUME 35
//#define MAX_PATCHVOICE_VOLUME 75

// These two have been moved up.
//#define MIN_PATCHVOICE_CHANCE 0
//#define MAX_PATCHVOICE_CHANCE (0x100)


/*
typedef struct{
  float note_num;
  int64_t note_id;
  float pan;
} PatchPlayingNote;

static inline PatchPlayingNote NewPatchPlayingNote(float note_num, int64_t note_id,float pan){
  PatchPlayingNote ret;
  ret.note_num=note_num;
  ret.note_id=note_id;
  ret.pan=pan;
  return ret;
}
*/

// Used by the player when playing/changing/stopping note
typedef struct {
  int64_t id;
  const struct SeqBlock *seqblock;
  float pitch;
  float velocity;
  float pan;
  int64_t sample_pos; // for sample seek
  char midi_channel;
  char voicenum;
} note_t;

static inline bool is_note(note_t note, int64_t id, const struct SeqBlock *seqblock){
  return note.id==id && note.seqblock==seqblock;
}

#define NOTE_ID_RESOLUTION 256 // i.e. 256 id's per note.
static inline int64_t NotenumId(double notenum){
  int64_t n = (int64_t)(notenum * (double)(NOTE_ID_RESOLUTION));
  return n*NUM_PATCH_VOICES;
}


typedef struct _linked_note_t{
  struct _linked_note_t *next;
  struct _linked_note_t *prev;
  note_t note;  
  struct Notes *editor_note;
  struct SeqTrack *seqtrack;
  int play_id;
} linked_note_t;


enum PatchWidgetSizeType{
  SIZETYPE_NORMAL = 0, // Don't change. Saved to disk
  SIZETYPE_HALF = 1, // Don't change. Saved to disk
  SIZETYPE_FULL = 2 // Don't change. Saved to disk
};


// #define CAST_API_PATCH_ID(a) ((int)a) // When API is converted to 64 bit, this definition should be changed to just 'a'.

// Note that Patch objects are stored directly in undo/redo (not copied), so it must not be freed, reused for other purposes, or othervice manipulated when not available.
struct Patch{
  instrument_t id;
  const char *uuid; // Not currently used for anything.
  
  bool is_usable; // If pasting a track with this patch, this flag tells whether the patch can be used on the new track.
  hash_t *state; // If is_usable==false, this field contains the plugin state.

  bool has_been_assigned_to_editor_track;
  
  bool name_is_edited;
  const char *name;
  const symbol_t *midi_learn_port_name;
  
  const char *comment;

  unsigned int color;
  //int colornum;

  void (*playnote)(struct SeqTrack *seqtrack, struct Patch *patch,note_t note,STime time);
  void (*changevelocity)(struct SeqTrack *seqtrack, struct Patch *patch,note_t note,STime time);
  bool (*changepitch)(struct SeqTrack *seqtrack, struct Patch *patch,note_t note,STime time); // Returns false if instrument wasn't able to change pitch.
  void (*changepan)(struct SeqTrack *seqtrack, struct Patch *patch,note_t note,STime time);
  void (*sendrawmidimessage)(struct SeqTrack *seqtrack, struct Patch *patch,uint32_t msg,STime time,double block_reltempo); // note on, note off, and polyphonic aftertouch are/should not be sent using sendmidimessage. sysex is not supported either.
  void (*stopnote)(struct SeqTrack *seqtrack, struct Patch *patch,note_t note,STime time);
  void (*closePatch)(struct Patch *patch);
  
  struct Instruments *instrument;

  int permanent_id;             // Free use by the instrument plug-in.
  void *patchdata;		// Free use by the instrument plug-in. May be NULL. Player is locked when value is set.
  bool wide_mixer_strip;        // Only used by the audio instrument.

  void (*changeTrackPan)(int newpan,const struct Tracks *track);

  struct PatchVoice *voices; // num=NUM_PATCH_VOICES

  linked_note_t *playing_voices; /* To keep track of how many times a voice has to be turned off. */
  linked_note_t *playing_notes;  /* To keep track of which notes are playing. (Useful to avoid hanging notes when turning on and off voices) */

  bool peaks_are_dirty; /* Can be set to true by any thread. */

  bool forward_events; /* If true, all events that comes in, are also sent out to the receivers. True by default. */
  int num_event_receivers;
  struct Patch *event_receivers[MAX_NUM_EVENT_RECEIVERS];

  bool last_chance_decision_value; // Used when chance==0. Only accessed by the main player thread.

  enum PatchWidgetSizeType widget_height_type;
  
  //DEFINE_ATOMIC(int, visual_note_pitch);
  DEFINE_ATOMIC(int, visual_note_intencity); // Used by the mixer to keep track of how bright the note indicator should light up.

  DEFINE_ATOMIC(bool, widget_needs_to_be_updated);

  DEFINE_ATOMIC(bool, is_recording);

  DEFINE_ATOMIC(bool, always_receive_midi_input);

  bool is_visible;
};
#define PATCH_FAILED 0
#define PATCH_SUCCESS 1
#define NextPatch(a) ((struct Patch *)((a)->l.next))


#if 0
// Not necessary. Just create state from 'source' and apply it to 'dest'.
static inline void Patch_copyAttributesFromAnotherPatch(struct Patch *dest, struct Patch *source){
  dest->name_is_edited = source->name_is_edited;
  dest->name = source->name;
  dest->forward_events = source->forward_events;
  int i;
  for (i=0;i<NUM_PATCH_VOICES; i++)
    dest->voices[i] = source->voices[i];
}
#endif



/*********************************************************************
	instruments.h
*********************************************************************/

extern DEFINE_ATOMIC(bool, g_enable_autobypass);
extern DEFINE_ATOMIC(int, g_autobypass_delay);

// These numbers are saved to disk, so they can not be changed later. Currently, the quality must also be increasing (see SOUNDFILESAVER_save).
enum ResamplerType {
  RESAMPLER_NON=0,
  RESAMPLER_LINEAR=1,
  RESAMPLER_CUBIC=2,
  RESAMPLER_SINC1=3,
  RESAMPLER_SINC2=4
};

// These constants are not only used internally, but they are also saved to disk.
enum{
  NO_INSTRUMENT_TYPE = 0,
  MIDI_INSTRUMENT_TYPE,
  AUDIO_INSTRUMENT_TYPE
};


#define NOTUSED_EFFECT_NAME "NOTUSED"


struct Tracker_Windows;
struct Blocks;

struct Instruments{ 
	struct ListHeader1 l;

	const char *instrumentname;

        vector_t patches; // Not safe to traverse from player thread.

        //int (*getMaxVelocity)(const struct Patch *patch);

        int (*getNumFxs)(const struct Patch *patch);
        const char *(*getFxName)(const struct Patch *patch, int fxnum);
        struct FX *(*createFX)(const struct Tracks *track, struct Patch *patch, int effect_num);

#ifdef __cplusplus
        void (*getFX)(struct Tracker_Windows *window,const struct Tracks *track, std::function<void(struct FX*)> callback);
#else
        void *getFX;
#endif
        //int (*getPatch)(struct Tracker_Windows *window,ReqType reqtype,const struct Tracks *track,struct Patch *patch);
	//void (*treatSpecialCommand)(char *command,struct Tracks *track);
	void (*CloseInstrument)(struct Instruments *instrument);
	void (*StopPlaying)(struct Instruments *instrument);
        void (*RT_StopPlaying)(struct Instruments *instrument); // Called from the player thread. StopPlaying is called from the main thread, and only if it apparently wasn't playing before. This function is always called right after the player has set player_state to PLAYER_STATE_STOPPED
        void (*PP_Update)(struct Instruments *instrument,struct Patch *patch, bool is_loading);
	void *(*CopyInstrumentData)(const struct Tracks *track);		//Necesarry for undo.

        void (*PlaySongHook)(struct Instruments *instrument, int64_t abstime);

	void *(*LoadFX)(struct FX *fx,const struct Tracks *track);

        // 'has_pause' will be set to true if we needed to pause the player, but we will only pause the player if the value was already false.
        // If only_check_this_block/only_check_this_track is defined, we don't add undo.
        void (*handle_fx_when_a_patch_has_been_replaced)(const struct Patch *old_patch, struct Patch *new_patch, struct Blocks *only_check_this_block, struct Tracks *only_check_this_track, bool *has_paused);
  
        void (*remove_patchdata)(struct Patch *patch);

	void (*setPatchData)(struct Patch *patch, const char *key, const char *value, bool program_state_is_valid);
	const char *(*getPatchData)(struct Patch *patch, const char *key);
};
#define INSTRUMENT_FAILED 0
#define INSTRUMENT_SUCCESS 1
#define NextInstrument(a) ((struct Instruments *)((a)->l.next))



/*********************************************************************
	stops.h
*********************************************************************/

#ifdef __cplusplus
namespace r{

struct Stop : public TimeDataDataTypeNoVal {
  Stop(Ratio time)
    : TimeDataDataTypeNoVal(time)
  {}
};

  struct StopSeqBlock : RT_TimeData_Player_Cache<int> {
  };
  
  using StopTimeData = TimeData<Stop, StopSeqBlock>;

}
#endif
/*
struct Stops{
	struct ListHeader3 l;
};
#define NextStop(a) ((struct Stops *)((a)->l.next))
*/


/*********************************************************************
	fxnodelines.h
*********************************************************************/

/*
struct FXNodeLines{
	struct ListHeader3 l;
	int val;
    	int logtype;
};
#define NextFXNodeLine(a) ((struct FXNodeLines *)((a)->l.next))
*/




/*********************************************************************
	swing.h
*********************************************************************/

struct Swing {
  struct ListHeader3 l;
  int weight;
  int logtype;
};
#define NextSwing(a) ((struct Swing *)((a)->l.next))


enum WSwingType {
  WSWING_NORMAL,
  WSWING_BELOW,
  WSWING_MUL,
};
  



/*********************************************************************
	tracks.h
*********************************************************************/

struct Tracks{
	struct ListHeader1 l;

#ifdef __cplusplus
	r::NoteTimeData *_notes2;
	r::NoteTimeData *_gfx_notes2;
#else
	void *_notes2;
	void *_gfx_notes2;
#endif
  
	struct Notes *notes;

#ifdef __cplusplus
        r::StopTimeData *stops2;
#else
        void *stops2;
#endif
  //	struct Stops *stops;
        struct Notes *gfx_notes; // Used when recording MIDI notes in sequencer mode.

        const char *trackname; // Contains the value "(click me)" when patch==NULL
	struct Patch *patch;
        vector_t fxs; // Contains struct FXs* elements

        struct Swing *swings;
        dyn_t  filledout_swings; // Used both to calculate timing, and for rendering. Calculated from block->beats, block->swings, and swings.
        const struct STimes *times;			/* Pointer to array. Last element (times[num_lines]) is the playtime of the block. Calculated from block->lpbs/block->tempos/block->temponodes/root->lpb/root->bpm/filledout_swings. */
  
        void *midi_instrumentdata;			/* Used by the midi instrument. */

        DEFINE_ATOMIC(bool, is_recording);


  /**************************************************************
   * Note! All data below here can be memcpied to a new track. *
   *************************************************************/
  
        union{
          int start_copyable_data;
          int onoff;
        };

  // Use _notes2._polyphony instead.
  //int polyphony;
  
	int pan;
	int volume;

	bool panonoff;
        bool volumeonoff;                      /* The volume-button on/off, not track on/off. (i.e. if off, volume=1.0, not 0.0) */

        DEFINE_ATOMIC(int, midi_channel);
};
#define NextTrack(a) ((struct Tracks *)((a)->l.next))

#define MAXTRACKVOL 1000
#define MAXTRACKPAN 1000

static inline float TRACK_get_pan(const struct Tracks *track){
  if(track->panonoff)
    return scale((float)track->pan, -MAXTRACKPAN, MAXTRACKPAN, -1.0f, 1.0f);
  else
    return 0.0f;
}

static inline float TRACK_get_volume(const struct Tracks *track){
  if(track->volumeonoff)
    return scale((float)track->volume, 0, MAXTRACKVOL, 0.0f, 1.0f);
  else
    return 1.0f;
}

static inline double TRACK_get_velocity(const struct Tracks *track, int velocity){
  return (double)TRACK_get_volume(track) * VELOCITY_get(velocity);
}

#define FOR_EACH_TRACK()                                                \
  struct Blocks *block = root->song->blocks;                            \
  while(block != NULL){                                                 \
    struct Tracks *track = block->tracks;                               \
    while(track != NULL){                                               

#define END_FOR_EACH_TRACK                                              \
      track = NextTrack(track);                                            \
    }                                                                     \
    block = NextBlock(block);                                           \
  }                                                                   

#define FOR_EACH_WTRACK(window)                                         \
  struct WBlocks *wblock = window->wblocks;                             \
  while(wblock != NULL){                                                \
    struct WTracks *wtrack = wblock->wtracks;                           \
    while(wtrack != NULL){                                              \
      struct Tracks *track = wtrack->track;
      
#define END_FOR_EACH_WTRACK                                             \
      wtrack = NextWTrack(wtrack);                                      \
    }                                                                   \
    wblock = NextWBlock(wblock);                                        \
  }                                                                     
      


/*********************************************************************
	time_position.h
*********************************************************************/

typedef struct{
  int16_t blocknum;
  int16_t tracknum;
  STime blocktime;
} time_position_t;



/*********************************************************************
	area.h
*********************************************************************/



typedef struct{
	int x,x2;
}Area;

typedef struct{
	int y,y2;
}YArea;

typedef struct{
	int width;
	int x,x2;
}WArea;

typedef struct{
  float x,y;
}WPoint;




/*********************************************************************
	peaks.h
*********************************************************************/

#define NUM_PEAKS_PER_LINE 8
typedef struct{
  float x,y;
} APoint;



/*********************************************************************
	data_as_text.h
*********************************************************************/

typedef struct{
  int value;
  int logtype;
  bool is_valid;
} data_as_text_t;


/*********************************************************************
	trackreallines2.h
*********************************************************************/

enum TR2_Type{
  TR2_NOTE_START,
  TR2_NOTE_END,
  TR2_PITCH,
  TR2_STOP
};

#  ifdef __cplusplus
typedef struct{
  Place p;
  enum TR2_Type type;

#if 0
  struct Notes *note;
#else
  r::NotePtr note;
#endif
  
  int64_t node_id;
  float pitch;
  int chance;

  int pitchnum;
} TrackRealline2;
#endif

#ifdef __cplusplus
typedef struct{
  Place p;
  struct Notes *note;
  //r::Velocity velocity(make_ratio(0,1),0);
  int velocity_value;
  int value;
  int logtype;
  bool is_first_velocity;
  bool is_last_velocity;
  int velnum;
} VelText;
#endif


#ifdef USE_QT4
typedef QList<TrackRealline2> Trs;
typedef QMap<int, Trs> Trss;

//typedef QList<struct Notes*> Waveform_trs;
typedef QMap<int, bool> Waveform_trss;

typedef QList<VelText> VelText_trs;
typedef QMap<int, VelText_trs> VelText_trss;

#define TRS_INSERT_PLACE(trs, tr)                                       \
  do{                                                                   \
    bool inserted=false;                                                \
    for(int i=0 ; i<trs.size() ; i++){                                  \
      if (PlaceLessThan(&tr.p, &trs.at(i).p)) {                         \
        trs.insert(i, tr);                                              \
        break;                                                          \
      }                                                                 \
    }                                                                   \
    if (inserted==false)                                                \
      trs.push_back(tr);                                                \
  }while(0)

#endif



/*********************************************************************
	nodelinens.h
*********************************************************************/

enum NodeNodeType{
  NODETYPE_FIRST = -1,
  NODETYPE_LAST = -2,
  NODETYPE_NO_NODE = -3  
};
  
struct NodeLine{
  struct NodeLine *next;

  float x1,y1;
  float x2,y2;

  const struct ListHeader3 *element1;
  const struct ListHeader3 *element2;
  
  int logtype;
  
  bool is_node;
};

struct NodeLine2{
  struct NodeLine2 *next;

  float x1,y1;
  float x2,y2;

  //int n1,n2;
  Ratio time1, time2;
  int64_t id1,id2;
  
  int logtype;
  
  bool is_node;
};

static inline const struct NodeLine *Nodeline_n(const struct NodeLine *nodeline, int n){
  while(n>0 && nodeline!=NULL) {
    nodeline = nodeline->next;
    n--;
  }
  return nodeline;
}

struct Node{
  float x, y;
  const struct ListHeader3 *element;
};

struct Node2{
  float x, y;
  //int n;
  int64_t id;
};

struct MinMax{
  float min;
  float max;
};

typedef struct {
  float x1,y1;
  float x2,y2;
} NodelineBox;




/*********************************************************************
	tbox.h
*********************************************************************/
struct TBoxstruct{
  int x1,y1;
  int x2,y2;
};
typedef struct TBoxstruct TBox;


/*********************************************************************
	wtracks.h
*********************************************************************/

#define WTRACKS_SPACE 2


struct WTracks{
	struct ListHeader1 l;
//	l.num=wtracknum;

  	struct Tracks *track;			/* Only referenced. wtracknum=track->tracknum */

	int x,y,x2,y2;						/* GFX area. */

  /**************************************************************
   * Note! All data below here can be memcpied to a new wtrack. *
   *************************************************************/  
        union{
          int start_copyable_data;
          int notesonoff;					/* notearea and placementarea on/off. */
        };
	int notelength;					/* Number of characters the notes is. Usually 2 or 3. */
        int notewidth;
	Area notearea;						/* These are all parts of the GFX area. */

        bool swingtext_on;
        Area swingtextarea;
        bool swingtext_fits_reallines;

        bool centtext_on;
        Area centtextarea;
  
        bool chancetext_on;
        Area chancetextarea;
  
        bool veltext_on;
        Area veltextarea;

        bool fxtext_on;
        Area fxtextarea;
  
	int fxonoff;						/* FX area on/off */
	int fxwidth;						/* is fxarea.x2-fxarea.x */
	Area fxarea;

        int non_wide_fx_width;
        bool is_wide;

        bool pianoroll_on;
        int pianoroll_lowkey;
        int pianoroll_highkey;

        int pianoroll_width; // not necessary
        Area pianoroll_area;
  
        //int num_vel;						/* Max number of velocity lines showed simultaniously. (I.e the number of subtracks)*/

  /*
        TBox name;

	TBox pan;
	TBox volume;

	TBox panonoff;
	TBox volumeonoff;

        TBox meter;
  */
        int noteshowtype;
};
#define NextWTrack(a) ((struct WTracks *)((a)->l.next))

#define TEXTTYPE 0
#define GFXTYPE1 1
#define MAXTYPE 1
 
struct CurrentPianoNote{
  int tracknum;
  int64_t noteid;
  int pianonotenum;
};

extern struct CurrentPianoNote g_current_piano_note;

struct CurrentPianoGhostNote{
  int tracknum;
  Place start;
  Place end;
  float value;
};

extern struct CurrentPianoGhostNote g_current_piano_ghost_note;

/*
static inline NodelineBox GetPianoNoteBox(const struct WTracks *wtrack, const struct NodeLine *nodeline){
  const float gfx_width  = wtrack->pianoroll_area.x2 - wtrack->pianoroll_area.x;
  const float notespan   = wtrack->pianoroll_highkey - wtrack->pianoroll_lowkey;
  const float note_width = gfx_width / notespan;

  float x_min = R_MIN(nodeline->x1, nodeline->x2);
  float x_max = R_MAX(nodeline->x1, nodeline->x2);

  float y_min = R_MIN(nodeline->y1, nodeline->y2);
  float y_max = R_MAX(nodeline->y1, nodeline->y2);

  const struct NodeLine *next = nodeline->next;
  
  while(next!=NULL){
    if (next->is_node)
      break;

    x_min = R_MIN(x_min, R_MIN(next->x1, next->x2));
    x_max = R_MAX(x_max, R_MAX(next->x1, next->x2));

    y_min = R_MIN(y_min, R_MIN(next->y1, next->y2));
    y_max = R_MAX(y_max, R_MAX(next->y1, next->y2));

    next = next->next;
  }

  NodelineBox nodelineBox;

  nodelineBox.x1 = x_min-note_width/2.0f;
  nodelineBox.y1 = y_min;
  nodelineBox.x2 = x_max+note_width/2.0f;
  nodelineBox.y2 = y_max;

  return nodelineBox;
}
*/

static inline NodelineBox GetPianoNoteBox2(const struct WTracks *wtrack, const struct NodeLine2 *nodeline){
  const int gfx_width  = wtrack->pianoroll_area.x2 - wtrack->pianoroll_area.x;
  const int notespan   = wtrack->pianoroll_highkey - wtrack->pianoroll_lowkey;
  const float note_width = (float)gfx_width / (float)notespan;

  float x_min = R_MIN(nodeline->x1, nodeline->x2);
  float x_max = R_MAX(nodeline->x1, nodeline->x2);

  float y_min = R_MIN(nodeline->y1, nodeline->y2);
  float y_max = R_MAX(nodeline->y1, nodeline->y2);

  const struct NodeLine2 *next = nodeline->next;
  
  while(next!=NULL){
    if (next->is_node)
      break;
    
#pragma clang diagnostic push
#pragma clang diagnostic ignored "-Wshadow"

    x_min = R_MIN(x_min, R_MIN(next->x1, next->x2));
    x_max = R_MAX(x_max, R_MAX(next->x1, next->x2));

    y_min = R_MIN(y_min, R_MIN(next->y1, next->y2));
    y_max = R_MAX(y_max, R_MAX(next->y1, next->y2));

#pragma clang diagnostic pop
    
    next = next->next;
  }

  NodelineBox nodelineBox;

  nodelineBox.x1 = x_min-note_width/2.0f;
  nodelineBox.y1 = y_min;
  nodelineBox.x2 = x_max+note_width/2.0f;
  nodelineBox.y2 = y_max;

  return nodelineBox;
}

struct PianorollRectangle{
  float pitch1;
  float pitch2;
  Place place1;
  Place place2;
  int tracknum;
  int blocknum;
};

extern struct PianorollRectangle g_current_pianobar_rubber;
extern struct PianorollRectangle g_current_pianobar_selection_rectangle;


/*********************************************************************
	Signature.h
*********************************************************************/

struct Signatures{
  struct ListHeader3 l;
  StaticRatio signature;
};
#define NextSignature(a) (struct Signatures *)((a)->l.next)
#define NextConstSignature(a) (const struct Signatures *)((a)->l.next)

#ifdef USE_QT4

#pragma clang diagnostic push
#pragma clang diagnostic ignored "-Wshorten-64-to-32"
#include <QVector>
#pragma clang diagnostic pop

#define SIGNATURE_NORMAL 0
#define SIGNATURE_BELOW 1
#define SIGNATURE_MUL 2

struct WSignature{
  StaticRatio signature;
  int bar_num;
  int beat_num;   // In a 4/4 measure, this value is either 0, 1, 2 or 3, or 4. (0 means that there is no beat placed on this realline)
  int type;	  /* 0=normal, 1=below positioned, 2=mul. */
  QVector<float> how_much_below;  /* If type is 1 or 2, these values contains how much below (between 0 and 1) */
  //vector_t how_much_below;

  WSignature()
    : signature(make_empty_static_ratio())
    , bar_num(0)
    , beat_num(0)
    , type(SIGNATURE_NORMAL)
  {}
};

typedef QVector<WSignature> WSignature_trss;

static inline bool WSIGNATURE_is_measure_change(const WSignature &signature){
  return signature.signature.numerator != 0 && signature.beat_num==1;
}

static inline bool WSIGNATURE_is_first_beat(const WSignature &signature){
  return signature.beat_num==1;
}

#endif




/*********************************************************************
	Beats.h
*********************************************************************/

struct Beats{
  struct ListHeader3 l;
  StaticRatio signature; // Current signature for this beat.
  int bar_num;   // Starts counting from 1.
  int beat_num;  // For instance, in a 4/4 measure, this value is either 1, 2 or 3, or 4. Starts counting from 1.

  StaticRatio valid_signature; // signature is 0/0 if there isn't a signature change at this beat.
};
#define NextBeat(a) (struct Beats *)((a)->l.next)
  

/*********************************************************************
	lpb.h
*********************************************************************/


struct LPBs{
	struct ListHeader3 l;
	int lpb;
};
#define NextLPB(a) (struct LPBs *)((a)->l.next)
#define NextConstLPB(a) (const struct LPBs *)((a)->l.next)

struct WLPBs{
	int lpb;
	int type;					/* 0=normal, 1=below positioned, 2=mul. */
};
#define LPB_NORMAL 0
#define LPB_BELOW 1
#define LPB_MUL 2



/*********************************************************************
	tempos.h (i.e. BPM)
*********************************************************************/


struct Tempos{
	struct ListHeader3 l;
	int tempo;
        int logtype;
};
#define NextTempo(a) (struct Tempos *)((a)->l.next)
#define NextBPM(a) NextTempo(a)
#define NextConstTempo(a) (const struct Tempos *)((a)->l.next)
#define NextConstBPM(a) NextConstTempo(a)

struct WTempos{
	int tempo;
	int type;							/* 0=normal, 1=below positioned, 2=mul. */
        int logtype;
};
/* Types */
#define TEMPO_NORMAL 0
#define TEMPO_BELOW 1
#define TEMPO_MUL 2

// Todo: Rename all Tempos to BPMs. First step:
#define BPMs Tempos
#define WBPMs WTempos


/*********************************************************************
	temponodes.h
*********************************************************************/



struct TempoNodes{
	struct ListHeader3 l;
	double reltempo;
};
#define NextTempoNode(a) ((struct TempoNodes *)((a)->l.next))
#define NextConstTempoNode(a) ((const struct TempoNodes *)((a)->l.next))




/*********************************************************************
	time.h
*********************************************************************/

#define USE_NEW_TIMING 1


#if !USE_NEW_TIMING
struct STimeChanges{
	struct ListHeader3 l;
	STime time;

	double tempo1;			// tempo (tempo*lpb) at this->l.p
	double rel;				// reltempo for this->l.p
	double deltarel;		// rel+deltarel is reltempo for this->l.next->l.p
};
#define NextSTimeChange(a) (struct STimeChanges *)((a)->l.next)

#else

struct STimeChange{
  double y1,x1,t1; // y=line (place as double), x = tempo at y (BPM*LPB), t = time at y (STime)
  double y2,x2,t2; //

  double logt1;   // Precalculated log(x1)     [ !!!!! NOT log(t1)    !!!!! ]
  double logt2t1; // Precalculated log(x2/x1)  [ !!!!! NOT log(t2/t2) !!!!! ]

  double glidingswing_scale_value; // hack to fix gliding swing values. Not used if it has value 0;
};

#endif


struct STimes{									/* One element for each line. */
	STime time;							/* Start-time for the line. */
#if !USE_NEW_TIMING
	const struct STimeChanges *timechanges;
#else
        const struct STimeChange *tchanges;
#endif
};

/*********************************************************************
	blocks.h
*********************************************************************/

extern int64_t g_editor_blocks_generation; //This number increases every time a block is added or removed, tracks are added or removed, block is renamed, block changes color, or block duration changes.

#ifdef __cplusplus
namespace r{
  class CacheNumHolder;
}
#endif

struct Blocks{
	struct ListHeader1 l;

	const char *name;

	NInt num_tracks;
	int num_lines;

	struct Tracks *tracks;

        const struct Beats        *beats; // Calculated from signatures and lpbs. Set in time.c.
        dyn_t  filledout_swings; // Used both to calculate timing, and for rendering. Calculated from beats and swings.
  
	struct Signatures   *signatures;  // Used by player. Player must be stopped when modifying.
        struct LPBs   *lpbs; // Used by player. Player must be stopped when modifying.
	struct Tempos *tempos;
	struct TempoNodes *temponodes;
	struct TempoNodes *lasttemponode;
        struct Swing *swings;
        bool swing_enabled;
  
        int num_time_lines; // Contains number of lines in 'times' minus one (same as num_lines, normally). Can be read from any thread.
        const struct STimes *times_with_global_swings; // Pointer to array. Last element (times[num_lines]) is the playtime of the block. Calculated from lpbs/tempos/temponodes/global lpb/global bpm/filledout_swings.
        const struct STimes *times_without_global_swings;
  //const struct STimes *__gakkgakk_remove__gakkkakk__times;  //  Either points to 'times' or 'times_with_global_swings', depending on whether plugins should receive swing tempo or not.

        STime length; // Same as block->times[block->num_time_lines].time, but can be accessed from any thread. Must obtain player lock when writing to this variable.
  
        DEFINE_ATOMIC(double, reltempo);					/* factor that the tempo is multiplied with when playing this block. */

        DEFINE_ATOMIC(double, player_time);	/* = pc->end_time - RT_curr_seqblock()->time */

  unsigned int color;
  
  // This variable is checked after each keyboard or menu event. If true, trackreallines, wtracks, etc. will be updated.
  bool is_dirty;

#ifdef __cplusplus
  r::CacheNumHolder *cache_num_holder; // Sets seqblock->cache_num in the player.
#else
  void *cache_num_holder;
#endif
};


#define NextBlock(a) (struct Blocks *)((a)->l.next)


extern DEFINE_ATOMIC(bool, g_use_track_channel_for_midi_input);
extern DEFINE_ATOMIC(bool, g_send_midi_input_to_current_instrument);
extern DEFINE_ATOMIC(int, g_curr_midi_channel);
extern DEFINE_ATOMIC(struct Blocks *, g_curr_block);



/*********************************************************************
	localzooms.h
*********************************************************************/


struct LocalZooms{
	struct ListHeader3 l;
//	struct LocalZooms *next;		/* Next on the same level. */
//	int line;							/* The same type of line that note has. */
//	uint_32 counter;					/* Same type of counter that line has. */
//	uint_32 dividor;					/* Same type of dividor that line has. */

	int zoomline;						/* The linetype that is showed in the editor. */
	int level;
	int realline;

	struct LocalZooms *uplevel;	/* Contains 'num_newlines' # of elements. */

  bool autogenerated;
};
#define NextLocalZoom(a) ((struct LocalZooms *)((a)->l.next))



/*********************************************************************
	wblocks.h
*********************************************************************/
typedef struct {
  bool enabled;
  int x1;
  int x2;
  Place y1;
  Place y2;
} range_t;

static inline range_t make_range(bool enabled, int x1, int x2, Place y1, Place y2){
  R_ASSERT_NON_RELEASE(x2>=x1);
  range_t range = {enabled, x1, x2-1, y1, y2};
  return range;
}

struct WBlocks{
	struct ListHeader1 l;

	int tempotrackonoff;				/* 1=on, 0=off */
	int temponodetrackonoff;		/* 1=on, 0=off */

	TBox a; // everything
//	int x,y,x2,y2;						/* GFX area. */

        int skew_x; // All x values in the tracks has this value added to them. It is always 0 or negative. Set when moving the track scrollbar.

	TBox t;
//	int tx,ty,tx2,ty2;				/* lines, nodes, etc. GFX area. */

        TBox bottombar;                                /* midi record button, reltempo slider and track slider */

	//WArea zoomlevelarea;
        WArea linenumarea;
        int linenumbers_max_num_characters;
        int bars_max_num_characters;
        int beats_x; int beats_max_num_characters;
        int zoomlines_x; int zoomlines_max_num_characters;

  //        WArea 
  //WArea zoomlinearea;
        WArea tempocolorarea;
  //WArea signatureTypearea;
	WArea signaturearea;
	WArea lpbTypearea;
	WArea lpbarea;
	WArea tempoTypearea; // When one character signals whether the tempo is down "d", or multi "m"
	WArea tempoarea;
        WArea swingTypearea;
        WArea swingarea;
        bool swingtext_fits_reallines;
	WArea temponodearea;

	YArea linearea;

	int maxwtracksize;					/* The size of the widest wtrack. */

	int num_visiblelines;

        DEFINE_ATOMIC(int, till_curr_realline);       // Set by the player thread. Read by the main thread.
  
        int top_realline; // only access from main thread.
        int curr_realline; // only access from main thread.
        int bot_realline; // only access from main thread.

        int mouse_track; // The track the mouse is currently above. -1 if not on a track.

        int64_t mouse_note_id; // id of the note the mouse is currently above. Is 0 is not a note.
  
        struct FXs *mouse_fxs; // The fxs the mouse is currently above. NULL if mouse is not above an fx.
  
	struct Blocks *block;			/* Only referenced. wblocknum=block->blocknum */

	struct LocalZooms *localzooms;    /* These two variables (localzooms and reallines) contain the same elements, but 'localzooms' is organized as a tree, while 'reallines' is organized as an array. (roughly)*/
        const struct LocalZooms **reallines;   // Used by the player. Must be protected by PLAYER_lock. Also Used by the OpenGL thread. The content must not be modified after creation, but it can be replaced. It's also fine to modify the content of individual elements. The OpenGL thread only uses the ->l.p values.

	int num_reallines;               // Same here. Must be protected by PLAYER_lock.

        int num_expand_lines;

	struct WTracks *wtracks;
	struct WTracks *wtrack;			/* Current track. Only referenced. Must use ATOMIC_WRITE when setting. Is never (and must never be) NULL. In other threads than the main thread, ATOMIC_READ must be used to read this variable. */

  /*
	NInt left_track;				
	int left_subtrack;
	NInt right_track;				
	int right_subtrack;
*/

        //struct WTempos *wtempos;
	double reltempomax;

        range_t range;
  
	bool isgfxdatahere;

	TBox reltempo; // API: (x1 y1 x2 y2) getWBlockFromNum(-1,-1)
};
#define NextWBlock(a) (struct WBlocks *)((a)->l.next)



/*********************************************************************
	slider.h
*********************************************************************/


struct Slider{
	int show;
	int width;
	int x,x2;
	int lx,lx2;
};


/*********************************************************************
       blts.h
********************************************************************/

typedef struct{

  /* Used by Blt_blt Blt_mark */
  bool blt_do;
  int x1;int x2;
  int startrealline;
  int endrealline;

  /* Used by Blt_blt and Blt_marktrackheader */
  bool blt_doheader;
  NInt starttrack;
  NInt endtrack;

  /* Used by Blt_clearNotUsedVisible and Blt_markVisible */
  bool clear_do;
  int v_x1;int v_x2;
  int v_y1;int v_y2;
}Blt;


/*********************************************************************
       Mixer.hpp
********************************************************************/

struct SoundProducer;

typedef struct {
  struct SoundProducer *bus1;
  struct SoundProducer *bus2;
  struct SoundProducer *bus3;
  struct SoundProducer *bus4;
  struct SoundProducer *bus5;
} Buses;


/*********************************************************************
	windows.h
*********************************************************************/

struct Tracker_Windows{
	struct ListHeader1 l;

	struct OS_visual os_visual;
	int x,y;								/* Where window is placed. (for amiga: screen-pos)*/
	int width,height;					/* Size of area to use. */
	char *fontname;
	int fontID;							/* System spesific. For amiga: fontsize. */
	int fontTags;						/* System spesific. For amiga: nothing. */
	int fontwidth,fontheight;		/* Proportional fonts not so very allowed. */
	int systemfontheight;
  
	NInt curr_track;
	int curr_track_sub;				/* -1=note, 0,1,2,...,n=vel (note that the swing subtracks are 0,1,2 even though swing is to the left of the note. note is always -1.)*/
        NInt curr_block; // Used by P2MUpdateSongPosCallBack to keep track of whether to switch current block. The value -1 means that current block is not visible. (happens when playing song)
        int curr_othertrack_sub; // subtrack for bpm track, and so forth.
        
	int maxwtracksize;					/* The size of the widest wtrack for all wblocks. */

        //struct Slider bottomslider;
        int bottomslider_height;
  
	struct Slider leftslider;

        bool track_slider_is_moving; // If true, draw bottomslider in a different color.
        bool scrollbar_is_moving; // If true, draw leftslider in a different color.
  
	bool playalong;					/* If true, this window allso shows whats being played
											   if any other window is playing. default=true. */
  
        int message_duration_left;             // in ms. If not set, message is not automatically removed. (note that if program is buzy, the message may stay longer)
        const char *message;
  
	struct WBlocks *wblock;			/* Current wblock. Only referenced. */
	struct WBlocks *wblocks;

  //	struct TEventFIFO *TELroot;
  //	struct TEventFIFO *TELlast;
  //	uint32_t event_treat;		/* Chooses which event type(s) to treat. (0=all)*/
  //	int dontbuffer;

  //	struct MouseAction curraction;
  //	struct MouseAction prevaction;

	int org_fontheight;
#ifdef _AMIGA
	char *h_fontname;
	int h_fontID;							/* System spesific. For amiga: fontsize. */
	int h_fontTags;						/* System spesific. For amiga: nothing. */
	int h_fontwidth;
#endif

  bool show_signature_track;
  bool show_swing_track;
  bool show_lpb_track;
  bool show_bpm_track;
  bool show_reltempo_track;

  int num_pixmapdefs;
  int *pixmapdefs;
  int *pixmapdefs_calc;

  Blt blt;

#ifdef USE_GFX_OP_QUEUE
  void *op_queue;
#endif

  bool must_calculate_coordinates;
  bool must_redraw;
  bool must_redraw_editor; // Same as must_redraw, but only redraws the editor.
  
  bool redraw_has_been_scheduled;
};
#define NextWindow(a) (struct Tracker_Windows *)((a)->l.next)

/* curr_track types. Order must be correct. */
#define TEMPOCOLORTRACK -7
#define TEMPOTRACK -6
#define LPBTRACK -5
#define SIGNATURETRACK -4
#define LINENUMBTRACK -3
#define SWINGTRACK -2
#define TEMPONODETRACK -1
#define LEFTMOSTTRACK TEMPOCOLORTRACK
#define NOTRACK -10000

static inline const char *get_track_name(int tracknum){
  if (tracknum >= 0)
    return "Normal";
  
  switch(tracknum){
  case TEMPOTRACK:
    return "BPM";
  case LPBTRACK:
    return "LPB";
  case SIGNATURETRACK:
    return "Time signature";
  case SWINGTRACK:
    return "Swing";
  case TEMPONODETRACK:
    return "Tempo automation";
  default:
    RError("Unkown track num %d", tracknum);
    return "(unknown)";
  }
}


/*********************************************************************
	song.h
*********************************************************************/

struct LPBs;

typedef struct {
  const struct LPBs *next_lpb;

  double num_beats_played_so_far;

  Place place1;
  Place place2;
  
  double place1_f;
  double place2_f;
  
  int lpb_value;
  double num_beats_between_place1_and_place2;

  // Beats / BPM (the BPM value used in timing is calculated from LPB, not calculated from the BPM track)
  double curr_num_beats;
  double next_num_beats;
  double curr_bpm;

  bool has_next_num_beats;
  
} LPB_Iterator;


#if 0
struct Tempos;

typedef struct {
  const struct tempos *next_tempo;

  Place place1;
  Place place2;
  
  double place1_f;
  double place2_f;
  
  int lpb_value;
} BPM_Iterator;
#endif


typedef struct {
  const struct Beats *next_beat;

  double beat_position_of_last_bar_start; // = 0.0;

  StaticRatio last_valid_signature; // = {4,4};

  bool new_beat_bar_set; // = false;

  int play_id; // for sanity check

  bool is_active; // sanity check
  
} Beat_Iterator;


typedef struct {
  const struct Signatures *next_signature;
  StaticRatio signature_value; // = {4,4};
} Signature_Iterator;

#define MAX_SEQBLOCK_VOLUME_ENVELOPE_DB 6
#define MAX_DISABLED_SEQBLOCK_TRACKS 512

struct SeqBlockTimings{
  int64_t time;
  
  // End seqtime.
  // When seqblock is NOT stretched, end_time = time + getBlockSTimeLength(seqblock->block).
  int64_t time2;

  
  int64_t default_duration; // Has value Place2Stime(end_place)-Place2Stime(start_place), or num_samples/resample_ratio.
  int64_t num_samples; // Only used if seqblock->block==NULL.
  
  Place start_place; // usually {0,0,1} (not used yet). Only used if block!=NULL
  Place end_place;   // usually {num_lines,0,1} (not used yet)

  bool is_looping;
  int64_t interior_start; // seqtime version of start_place. Non-stretched value. Divide by resampling ratio to get sample pos.
  int64_t interior_end;   // seqtime version of end_place. Non-stretched value. Divide by resampling ratio to get sample pos.

  //int64_t noninterior_start; // The seqtime start if interior_start==0
  //int64_t noninterior_end; // The seqtime end if interior_end==0

  double stretch_without_tempo_multiplier;
  
  // stretch = (end_time-time) / getBlockSTimeLength(seqblock->block)
  // 1.0 = no stretch. 0.5 = double tempo. 2.0 = half tempo.
  // Only used for converting stime -> seqtime a little bit faster. Updated when the result of end_time-time or getBlockSTimeLength(seqblock->block) changes.
  // Must not be used to find seqblock duration (i.e end_time-time).
  double stretch;
  
  // Same format as stretch. E.g. 0.5 = double tempo, 2.0 = half tempo.
  // Contrary to stretch, this value is stored in state, and is not the same as expected duration divided by actual duration.
  double speed;
};

struct SeqblockAutomation;

// If changing this one, also change 'get-selected-box-num' in sequencer.scm
enum SeqblockBoxSelected{
  SB_NO_SELECTED,
  SB_FADE_LEFT,
  SB_FADE_RIGHT,
  SB_INTERIOR_LEFT,
  SB_INTERIOR_RIGHT,
  SB_SPEED_LEFT,
  SB_SPEED_RIGHT,
  SB_STRETCH_LEFT,
  SB_STRETCH_RIGHT,
};

static inline int get_system_fontheight(void);

#ifdef __cplusplus
enum class Seqblock_Type{
  REGULAR,
  GFX, // When moving or changing seqblocks.
  GFX_GFX, // When moving a copy of several seqblocks (orange transparent color)
  RECORDING
};
#endif

// Note: If changing order here, seqblock_audio.scm must be updated.
enum Seqblock_Automation_Type{
  SAT_VOLUME = 0,
  SAT_GRAIN_OVERLAP,
  SAT_GRAIN_LENGTH,
  SAT_GRAIN_JITTER,
  SAT_GRAIN_RAMP,
  SAT_STRETCH,
  SAT_SPEED,
  NUM_SATS
};

#define NUM_EDITOR_BLOCK_SATS (SAT_VOLUME+1)

static inline const char *sat_to_string(enum Seqblock_Automation_Type sat){
  switch(sat){
    case SAT_VOLUME:
      return "Volume";
    case SAT_GRAIN_OVERLAP:
      return "Grain Overlap";
    case SAT_GRAIN_LENGTH:
      return "Grain Length";
    case SAT_GRAIN_JITTER:
      return "Grain Jitter";
    case SAT_GRAIN_RAMP:
      return "Grain Ramp";
    case SAT_STRETCH:
      return "Stretch";
    case SAT_SPEED:
      return "Speed";
    case NUM_SATS:
    default:      
      R_ASSERT(false);
      return "Unknown";
  }
}

struct StretchspeedTimeConversionTable{
  double stretch_automation_compensation;
  double speed_automation_compensation;
  double stretchspeed_automation_compensation;
  
  int num_elements;
  int64_t *array; // Array that maps seqtime -> sample position. (necessary when automating stretch or speed)    
};

#ifdef __cplusplus
class QPainter;
namespace radium{
struct AutomationPainter{
  enum What{
    FILL = 2 << 0,
    LINES = 2 << 1,
    NODES = 2 << 2,
  };
  virtual ~AutomationPainter() = default; // Crazy c++ stuff. https://www.securecoding.cert.org/confluence/display/cplusplus/OOP52-CPP.+Do+not+delete+a+polymorphic+object+without+a+virtual+destructor
  virtual void paint_fill(QPainter *p) const = 0;
  virtual void paint_lines(QPainter *p) const = 0;
  virtual void paint_nodes(QPainter *p) const = 0;
  void paint(QPainter *p, unsigned int what) const {
    if (what & What::FILL)
      paint_fill(p);
    if (what & What::LINES)
      paint_lines(p);
    if (what & What::NODES)
      paint_nodes(p);
  }
};
}
#endif

#ifdef __cplusplus
namespace radium{
  class Envelope;
}

/*
namespace radium{
}
*/
#endif

#ifdef SEQBLOCK_USING_VECTOR
#include "Vector.hpp"
namespace radium{
  using RT_NoteVector = radium::Vector<struct Notes*, radium::AllocatorType::RT>;

  struct HangingNote{
    r::NotePtr note;
    Patch *patch;
    const struct SeqBlock *seqblock;
    int midi_channel;
  };
  using RT_HangingNoteVector = radium::Vector<HangingNote, radium::AllocatorType::RT>;
}
#endif

struct SeqBlock{
  int64_t id;

  struct SeqBlockTimings t; // Note, player lock must be obtained when changing values in t, even when not playing song. There's a race condition in audio/Seqtrack_plugin.c that's quite tricky to avoid, so it's probably better to always lock player.

  // TODO: Write a function to recalculate this number for all editor-seqblocks in the sequencer whenever a seqblock is moved, added, or deleted.
  DEFINE_ATOMIC(int, timedata_player_index); // Used by the player for caching vector index. (editor-seqblocks only)

  
  struct Blocks *block; // If NULL, then the seqblock holds a sample.

  int64_t sample_id; // Has valid value if block==NULL.
  filepath_t sample_filename;
  filepath_t sample_filename_without_path;
  const wchar_t *name; // Only used when block==NULL. If null, sample_filename is displayed.
  
  bool *track_is_disabled; // Is NULL in the seqblock used when playing block.
    
  bool is_selected;

  enum SeqblockBoxSelected selected_box;

  double fadein; // value between 0 and 1
  double fadeout; // value between 0 and 1

#ifdef __cplusplus
  radium::Envelope *fade_in_envelope;
  radium::Envelope *fade_out_envelope;
#else
  void *fade_in_envelope; // radium::Envelope instance
  void *fade_out_envelope; // radium::Envelope instance
#endif

  float gain;

  float curr_gain; // gain for the current audio block. Calculated from envelope+gain+fadein+fadeout.
  bool curr_gain_changed_this_block; // set to true or false each block.

  float envelope_db;     // db version of envelope_volume

#if !defined(RELEASE)
  int64_t gcfinalizerdebuggingvariable;
#endif
  
  struct SeqblockAutomation *automations[NUM_SATS];

  struct StretchspeedTimeConversionTable conversion_table;

  int64_t curr_scheduled_realline_counter; // used by the scheduler. Only accessed from the main player thread.

  // These two are used by the scheduler to find cache_num in various TimeData structures.
#ifdef __cplusplus
  mutable int last_play_id;
  mutable int cache_num;
#else
  int last_play_id;
  int cache_num;
#endif

#ifdef SEQBLOCK_USING_VECTOR
  radium::Vector< radium::RT_NoteVector*, radium::AllocatorType::RT > *playing_notes;
  //radium::Vector< radium::RT_NoteVector2*, radium::AllocatorType::RT > *playing_notes2;
#else
  void *playing_notes;
  //void *playing_notes2;
#endif
  
  /*
  double stretch_automation_compensation;
  double speed_automation_compensation;
  double stretchspeed_automation_compensation;

  int num_time_conversion_table_elements;
  int64_t *time_conversion_table; // Maps sample position -> sample position. Used when automating stretch or speed.
  */
};


//extern struct SeqBlock *g_curr_seqblock;
extern int g_curr_seqtrack_under_mouse;
extern int64_t g_curr_seqblock_id_under_mouse;
extern int64_t g_curr_seqblock_id;

static inline bool is_current_seqblock(const struct SeqBlock *seqblock){
  return g_curr_seqblock_id==seqblock->id;
}

static inline int SEQBLOCK_num_automations(const struct SeqBlock *seqblock){
  return seqblock->block!=NULL ? NUM_EDITOR_BLOCK_SATS : NUM_SATS;
}

static inline double get_note_reltempo(note_t note){
  if (note.seqblock==NULL)
    return 1.0;
  else
    return ATOMIC_DOUBLE_GET(note.seqblock->block->reltempo);
}

#define NUM_CHANNELS_RECORDING_MATRIX 8

struct SeqtrackRecordingConfig{
  bool record_from_system_input;
  bool compensate_latency;
  bool matrix[NUM_CHANNELS_RECORDING_MATRIX][NUM_CHANNELS_RECORDING_MATRIX];
};


struct _scheduler_t;
typedef struct _scheduler_t scheduler_t;

struct SeqtrackAutomation;

// If changing this one, also update documentation in protos.conf (should probably don't change order since that would change the API), and update get-seqtrack-height-type in sequencer.scm.
enum SeqtrackHeightType{
  SHT_CUSTOM = 0,
  SHT_1ROW = 1,
  SHT_2ROWS = 2,
  SHT_3ROWS = 3,
  SHT_UNLIMITED = 4,
  NUM_SHTs
};

static inline enum SeqtrackHeightType get_seqtrack_height_type_from_string(const char *s){
  if(!strcmp(s,"custom"))
    return SHT_CUSTOM;
  else if(!strcmp(s,"1 row"))
    return SHT_1ROW;
  else if(!strcmp(s,"2 rows"))
    return SHT_2ROWS;
  else if(!strcmp(s,"3 rows"))
    return SHT_3ROWS;
  else if(!strcmp(s,"unlimited"))
    return SHT_UNLIMITED;
  else{
    R_ASSERT(false);
    return SHT_1ROW;
  }
}

static inline const char *get_string_from_seqtrack_height_type(enum SeqtrackHeightType type){
  switch(type){
  case SHT_CUSTOM: return "custom";
  case SHT_1ROW: return "1 row";
  case SHT_2ROWS: return "2 rows";
  case SHT_3ROWS: return "3 rows";
  case SHT_UNLIMITED: return "unlimited";
  case NUM_SHTs:
  default:{
    R_ASSERT(false);
    return "1 row";
  }
  }
}

struct SeqTrack{
  scheduler_t *scheduler;

  const char *uuid; // Used for seqtracks config.
  
  bool for_audiofiles;
  bool is_bus;
  
  vector_t seqblocks; // Player must be stopped when modifying this variable. Also used for displaying if gfx_seqblocks != NULL.
  vector_t *gfx_seqblocks; // Used for displaying. Might have the same content as this->seqblocks (pointing to &this->seqblocks). Changing the content should happen inside a Scoped_Update_RT_GFX_variables instance.
  vector_t gfx_gfx_seqblocks; // When moving several seqblocks. Just for graphics. Player does not have to be stopped when modifying this variable. Changing the content should happen inside a Scoped_Update_RT_GFX_variables instance.
  vector_t recording_seqblocks;

  bool RT_has_gfx_seqblocks; // Automatically updated in a Scoped_Update_RT_GFX_variables instance.
  bool RT_has_gfx_gfx_seqblocks; // Automatically updated in a Scoped_Update_RT_GFX_variables instance.

  dynvec_t seqblocks_z_order; // Used when painting seqblocks. Contains an array of indexes to gfx_seqblocks. Might not be constantly updated, so must check validity when using.
  
  struct SeqBlock *curr_seqblock; // curr_seqblock->block and curr_seqblock->time contains the same values as pc->block and pc->seqtime did before introducing seqtrack/seqblock.
  struct SeqBlock *curr_sample_seqblock; // Currently only used for displaying audiofile name in the editor. Note that curr_sample_seqblock->sample_id might not always be valid.

  double start_time; // Current seqtime. Can only be accessed from the player thread.
  double end_time;   // Same here. (should be the same as start_time + RADIUM_BLOCKSIZE)

  int64_t last_curr_seqblock_id;
  
  // These two variables are here only for convenience (and maybe a little bit of efficency) so that we don't have to do atomic operations on start_time and end_time in the player thread.
  // They contain the same values as 'start_time" and 'end_time' above.
  DEFINE_ATOMIC(double, start_time_nonrealtime);
  //DEFINE_ATOMIC(double, end_time_nonrealtime);

  //Place p; // Used by the scheduler when starting to play in the middle of a block

  LPB_Iterator lpb_iterator; // Used by scheduler_LPB.c to keep track of timing (PPQ and BPM)
  Beat_Iterator beat_iterator;
  Signature_Iterator signature_iterator;

  struct SeqtrackAutomation *seqtrackautomation;

  float note_gain; // Only used when for_audiofiles==false; Change by calling setSeqtrackNoteGain.
  float note_gain_muted; // if an editor seqtrack is muted, this field has the value 0.0.
  bool note_gain_soloed; // not used yet.
  bool note_gain_has_changed_this_block; // set to false after each audio block. Only used when for_audiofiles==false.
  
  const char *name; // Not used when for_audiofiles==true. (then we use patch->name instead)
  bool is_visible;
  
  enum SeqtrackHeightType min_height_type;
  enum SeqtrackHeightType max_height_type;

  double custom_min_height; // divided by system font height
  double custom_max_height; // divided by system font height

  bool has_calculated_coordinates;
  double y1, y2; // in the sequencer. Only valid if has_calculated_coordinates is true.
  
  // All variables below are only used when for_audiofiles==true.
  struct Patch *patch; // A "Sequencer audio file recorder/player" audio plugin.
  bool is_recording;
  bool use_custom_recording_config;
  struct SeqtrackRecordingConfig custom_recording_config;
  int recording_generation; // Used in audio/Seqtrack_plugin.cpp

#ifdef SEQBLOCK_USING_VECTOR
  radium::Vector< radium::RT_HangingNoteVector*, radium::AllocatorType::RT > *hanging_notes;
#else
  void *hanging_notes;
#endif

};


static inline double get_seqtrack_reltempo(struct SeqTrack *seqtrack){
  if (seqtrack==NULL)
    return 0.0;
  
  struct SeqBlock *seqblock = seqtrack->curr_seqblock;
  if (seqblock==NULL)
    return 1.0; // <--- NOTE: Changed this value from 0.0f to 1.0f. Seems wrong that it should be 0.0f.

  if (seqblock->block==NULL)
    return 1.0;
  
  return ATOMIC_DOUBLE_GET(seqblock->block->reltempo);
}


#ifndef __cplusplus
static inline struct SeqBlock *get_next_seqblock_block(const struct SeqTrack *seqtrack, int64_t start_time){
  
  VECTOR_FOR_EACH(struct SeqBlock *seqblock, &seqtrack->seqblocks){

    if (seqblock->block!=NULL && seqblock->t.time > start_time)
      return seqblock;
     
  }END_VECTOR_FOR_EACH;

  return NULL;
}
#endif

/*
struct SeqPlaylist{
  struct SeqTrack **seqtracks;
};
*/

struct LoopingOrPunching{
  DEFINE_ATOMIC(bool, enabled);
  DEFINE_ATOMIC(int64_t, start);
  DEFINE_ATOMIC(int64_t, end);
};

// These numbers are saved to disk, so they can not be changed.
enum GlissandBehavior{
  NEVER_DO_GLISSANDO = 0,
  DO_GLISSANDO_WHEN_INSTRUMENT_DOESNT_SUPPORT_CHANGING_PITCH = 1,
  ALWAYS_DO_GLISSANDO = 2
};

struct Song{
	struct Tracker_Windows *tracker_windows;
	struct Blocks *blocks;

        struct SeqTrack *block_seqtrack; // Used when playing block.

        struct LoopingOrPunching looping;
        struct LoopingOrPunching punching;
        DEFINE_ATOMIC(int, curr_seqtracknum);
        int topmost_visible_seqtrack;

        int curr_seqtrack_config_num;
        vector_t seqtracks; // New playlist. Player must both be stopped and locked when modifying this variable, or any of the contents.

        bool use_sequencer_tempos_and_signatures;

        bool show_bars_and_beats_sequencer_lane;
        bool show_time_sequencer_lane;
        bool show_tempos_sequencer_lane;
        bool show_signatures_sequencer_lane;
        bool show_markers_sequencer_lane;

	NInt num_blocks;
	const char *songname;

        bool linear_accelerando; // player must be stopped when writing to, or hold the player lock.
        bool linear_ritardando; // player must be stopped when writing to, or hold the player lock.
        bool plugins_should_receive_swing_tempo;
        bool use_swinging_beats_in_sequencer;
        bool display_swinging_beats_in_seqblocks_in_sequencer;
        bool editor_should_swing_along;

        int glissando_behavior; // Must hold player lock when writing.
  
        bool mixer_comments_visible;
        bool include_pan_and_dry_in_wet_signal; // Must hold player lock when writing.
        bool mute_system_buses_when_bypassed; // Must hold player lock when writing.
        bool mute_editor_automation_when_track_is_muted;
        int default_num_bus_channels; // Must be 1 or higher. User interface allows values up to 99 channels.

        struct SeqtrackRecordingConfig default_recording_config;

        bool RT_mute_plugin_MIDI_when_muted; // must hold player lock when writing.
        bool RT_send_plugin_MIDI_through_when_bypassed; // must hold player lock when writing.
        bool RT_implicitly_mute_plugin_MIDI; // must hold player lock when writing.

        bool includeAudioConnectionsInMixerConfig;
        bool includeEventConnectionsInMixerConfig;
        bool includeVolumeInMixerConfig;
        bool includePanningInMixerConfig;
        bool includeMuteSoloBypassInMixerConfig;
        bool includeSystemEffectsInMixerConfig;
        bool includeInstrumentEffectsInMixerConfig;
        bool includeInstrumentStatesInMixerConfig;
        bool includeMixerStripsConfigurationInMixerConfig;
        bool includeRememberCurrentInstrumentInMixerConfig;
        bool includeModulatorConnectionsInMixerConfig;
        bool includeSystemVolumeInMixerConfig;
  
	hash_t *mixerwidget_state; // Only used during loading.
	hash_t *instrument_widget_order_state; // Only used during loading.
};

extern LANGSPEC void SONGPROPERTIES_update(struct Song *song);



/*********************************************************************
	root.h
*********************************************************************/

struct Root{
	struct Song *song;

       //DEFINE_ATOMIC(NInt, curr_blocknum); // Currently playing blocknum

	int tempo;			/* Standard tempo. Player must be stopped when modifying. */
	int lpb;			/* Standard lpb. Player must be stopped when modifying. */
	StaticRatio signature;		/* Standard signature. Player must be stopped when modifying. */

        quantitize_options_t quantitize_options;
  
        int grid_numerator;
        int grid_denominator;

	int keyoct;
        int min_standardvel;
        int standardvel;

        DEFINE_ATOMIC(bool, editonoff);
        DEFINE_ATOMIC(bool, play_cursor_onoff);
        DEFINE_ATOMIC(bool, editor_follows_play_cursor_onoff);
        DEFINE_ATOMIC(bool, clickonoff);

        DEFINE_ATOMIC(bool, song_state_is_locked); // 'song state' means song->blocks, block->times, and song->playlist. The player is always stopped when changing any of these variables.
};

extern struct Root *root;


static inline int get_system_fontheight(void){
  if (root!=NULL && root->song!=NULL && root->song->tracker_windows!=NULL)
    return root->song->tracker_windows->systemfontheight;
  else
    return 20;
}

// Both width and height of automation nodes are get_min_node_size()*2
static inline double get_min_node_size(void) {
  return root->song->tracker_windows->fontheight / 1.5;
}


extern bool g_is_replacing_main_pipe;

extern bool g_mouse_is_pressed; // Should work for all widgets. If true, we can assume that we are inside a mouse cycle.

extern bool g_embed_samples;
extern bool g_curr_song_contains_embedded_samples;

static inline struct SeqTrack *SEQUENCER_get_curr_seqtrack(void){
  int curr_seqtracknum = ATOMIC_GET(root->song->curr_seqtracknum);

  R_ASSERT_NON_RELEASE(curr_seqtracknum >= 0);
  R_ASSERT_NON_RELEASE(curr_seqtracknum < root->song->seqtracks.num_elements);

  vector_t *seqtracks = &root->song->seqtracks;
  
  R_ASSERT(seqtracks->num_elements > 0);

  if (curr_seqtracknum >= seqtracks->num_elements || curr_seqtracknum<0){
    R_ASSERT_NON_RELEASE(false);
    if (curr_seqtracknum >= seqtracks->num_elements){
      curr_seqtracknum = seqtracks->num_elements-1;
    } else
      curr_seqtracknum = 0;
  }
  
  return (struct SeqTrack*)seqtracks->elements[curr_seqtracknum];
}

static inline struct SeqTrack *RT_get_curr_seqtrack(void){
  if (pc->playtype==PLAYSONG){// && is_playing()) {
    return SEQUENCER_get_curr_seqtrack();
  } else {
    return root->song->block_seqtrack;
  }
}

static inline struct SeqTrack *RT_get_aux_seqtrack(void){
  return root->song->block_seqtrack;
}

static inline struct SeqBlock *RT_get_curr_seqblock2(struct SeqTrack *curr_seqtrack){
  if (curr_seqtrack==NULL)
    return NULL;
  else
    return (struct SeqBlock*)atomic_pointer_read_relaxed((void**)&curr_seqtrack->curr_seqblock);
}

static inline struct SeqBlock *RT_get_curr_seqblock(void){
  return RT_get_curr_seqblock2(RT_get_curr_seqtrack());
}

static inline struct SeqBlock *RT_get_curr_sample_seqblock2(struct SeqTrack *curr_seqtrack){
  if (curr_seqtrack==NULL)
    return NULL;
  else
    return (struct SeqBlock*)atomic_pointer_read_relaxed((void**)&curr_seqtrack->curr_sample_seqblock);
}

static inline struct SeqBlock *RT_get_curr_sample_seqblock(void){
  return RT_get_curr_seqblock2(RT_get_curr_seqtrack());
}

static inline struct Blocks *RT_get_curr_playing_block(void){
  struct SeqBlock *seqblock = RT_get_curr_seqblock();
  if (seqblock != NULL)
    return seqblock->block;
  else
    return NULL;
  /*
  if (root->song->tracker_windows != NULL)
    return root->song->tracker_windows->wblock->block;
  else
    return root->song->blocks;
  */
}

static inline bool is_playing_current_block(void){
  if (pc==NULL)
    return false;
  
  if (RT_get_curr_playing_block() != root->song->tracker_windows->wblock->block)
    return false;

  Player_State state = ATOMIC_GET(pc->player_state);
  return state==PLAYER_STATE_STARTING_TO_PLAY || state==PLAYER_STATE_PLAYING;
}


static inline note_t create_note_t_plain(const struct SeqBlock *seqblock,
                                         int64_t note_id,
                                         float pitch,
                                         float velocity,
                                         float pan,
                                         int midi_channel,
                                         int voicenum,
                                         int64_t sample_pos
                                         )
{
#if !defined(RELEASE)
  R_ASSERT(midi_channel>=0 && midi_channel <= 15);
  R_ASSERT(note_id >= -1);
  
  //R_ASSERT(pitch < 150); // approx. This assert might give false positives.
  
  R_ASSERT(pitch >= 0);
  //R_ASSERT(pan >= -1); // Pans have different range in midi
  //R_ASSERT(pan <= 1);
#endif
  
  midi_channel = R_BOUNDARIES(0, midi_channel, 15);
  
  if(note_id<=-1)
    note_id = NotenumId((double)pitch);

  note_t note = {
    .id = note_id,
    .seqblock = seqblock,
    .pitch = pitch,
    .velocity =velocity,
    .pan = pan,
    .sample_pos = sample_pos,
    .midi_channel = (char)midi_channel,
    .voicenum = (char)voicenum
  };
  
  return note;  
}

static inline note_t create_note_t(const struct SeqBlock *seqblock,
                                   int64_t note_id,
                                   float pitch,
                                   float velocity,
                                   float pan,
                                   int midi_channel,
                                   int voicenum,
                                   int64_t sample_pos
                                   )
{
  return create_note_t_plain(seqblock, note_id, pitch, velocity, pan, midi_channel, voicenum, sample_pos);
}

static inline note_t create_note_t2(const struct SeqBlock *seqblock,
                                    int64_t note_id,
                                    float pitch
                                    )
{
  return create_note_t(seqblock, note_id, pitch, 0, 0, 0, 0, 0);
}

static inline note_t create_note_t3(const struct SeqBlock *seqblock,
                                    int64_t note_id,
                                    float pitch,
                                    int midi_channel
                                    )
{
  return create_note_t(seqblock, note_id, pitch, 0, 0, midi_channel, 0, 0);
}



/*********************************************************************
	undo.h
*********************************************************************/

#include "undo.h"




/*************************************************
 Structures for the advanced functions.
 (especially made for extension language support.)
 *************************************************/

struct NoteAdds_track{
	float place;					// A placement type represented as float
	int notenum;
	float volume;					// 0.0 is off, 1.0 is max, -0.0 - -2.0 is default
	float endplace;				// The end place. A value -0.0 - -2.0 means no spesified end-place (end-place is set to the same as the start-place of the next note in the array).
};

struct NoteAdds_track_do{
	NInt tracknum;
	int num_nats;
	struct NoteAdds_track *nats;
	float startplace;
	int sort;
};

struct NoteAdds_block{
	NInt blocknum;
	int num_nats_do;
	struct NoteAdds_track_do **nats_do;
};



/********* Various declarations ********************/

extern LANGSPEC void SONGPROPERTIES_open(void);
extern LANGSPEC void COMMENTDIALOG_open(void);
extern LANGSPEC void UPDATECHECKER_doit(void);
extern LANGSPEC void processEventsALittleBit(void);
extern LANGSPEC void MONOTONIC_TIMER_init(void);
#endif
