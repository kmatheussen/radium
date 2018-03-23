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

#if __tune_corei7__
#error "Compiled with -mtune=native or -mtune=corei7"
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
#if FOR_WINDOWS
#else
#error "oops"
#endif
#endif

#ifdef FOR_LINUX
#if FOR_LINUX
#else
#error "oops"
#endif
#endif

#ifdef FOR_MACOSX
#if FOR_MACOSX
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

#include <stdio.h>
#include <stdlib.h>
#include <limits.h>
#include <stdint.h>
#include <stdbool.h>

#if USE_QT4
#if !defined(__clang__)
#  pragma GCC diagnostic push
#  pragma GCC diagnostic ignored "-Wsuggest-attribute=const"
#endif

#pragma clang diagnostic push
#pragma clang diagnostic ignored "-Wshorten-64-to-32"

#include <QList>
#include <QMap>

#pragma clang diagnostic pop

#if !defined(__clang__)
#  pragma GCC diagnostic pop
#endif
#endif

#if defined(__clang__)
  #define FORMAT_ATTRIBUTE(A,B)
#else
  #define FORMAT_ATTRIBUTE(A,B) __attribute__((format(gnu_printf, A, B)));;
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


// Must be a multiply of 64 because of pd, which uses a block size of 64. 64 seems to work fine.
#define RADIUM_BLOCKSIZE 64

#define MAIN_TIMER_INTERVAL 5 // in milliseconds. Can be set to 1, 2, 5, 10, 25, or 50.
extern int g_main_timer_num_calls;

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
extern bool g_initing_starting_to_play_song;

extern bool g_user_interaction_enabled; // Used for testing. If this one is false, all user interaction will be done automatically, randomly.

#include "atomic.h"

#include "../crashreporter/crashreporter_proc.h"


#include <OS_Visual.h>


#define R_MAX(a,b) (((a)>(b))?(a):(b))
#define R_MIN(a,b) (((a)<(b))?(a):(b))
#define R_ABS(a) ((a)<0?(-(a)):(a))
#define R_BOUNDARIES(min,b,max) (R_MIN(R_MAX((min),(b)),(max)))

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
  #define R_ASSERT_NON_RELEASE(a) R_ASSERT(a)
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
    



#if 1 //defined(USE_CUSTOM_NUM_FRAMES)
#  define R_NUM_FRAMES_DECL int radium_num_frames___,
#  define R_NUM_FRAMES radium_num_frames___
#  define R_NUM_FRAMES_ARG radium_num_frames___,
#else
#  define R_NUM_FRAMES_DECL
#  define R_NUM_FRAMES radium_num_frames___ RADIUM_BLOCKSIZE
#  define R_NUM_FRAMES_ARG
#endif

static inline bool is_playing(void);

struct vector_t_;

#include "debug_proc.h"
#include "threading.h"
#include "OS_Player_proc.h"
#include "memory_proc.h"
#include "nsmtracker_events.h"

#include "OS_error_proc.h"
#include "OS_Semaphores.h"
#include "keyboard_focus_proc.h"

static inline int donothing(int input){
  return input;
}


extern LANGSPEC void handleError_internal(const char *fmt,...) FORMAT_ATTRIBUTE(1,2);
#define handleError(...) ((void)donothing(0 && printf(__VA_ARGS__)), handleError_internal(__VA_ARGS__)) // Add a "printf" call to make the C compiler show warning/error if using wrong arguments for FMT.


extern LANGSPEC void msleep(int ms);

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

static inline int64_t scale_int64(int64_t x, int64_t x1, int64_t x2, int64_t y1, int64_t y2){
  int64_t diff = x2-x1;
  
#if !defined(RELEASE)
  R_ASSERT(diff!=0);
#endif
  
  if (diff==0) // this is never supposed to happen, but to avoid integer divide-by-zero, we sacrifice some cycles here.
    return y1;
  else
    return y1 + ( ((x-x1)*(y2-y1))
                  /
                  diff
                  );
}

static inline double scale_double(double x, double x1, double x2, double y1, double y2){
#if !defined(RELEASE)
  R_ASSERT_RETURN_IF_FALSE2(x2!=x1,(y1+y2)/2);
#endif
  return y1 + ( ((x-x1)*(y2-y1))
                /
                (x2-x1)
                );
}

static inline float scale(float x, float x1, float x2, float y1, float y2){
#if !defined(RELEASE)
  R_ASSERT_RETURN_IF_FALSE2(x2!=x1,(y1+y2)/2);
#endif
  return y1 + ( ((x-x1)*(y2-y1))
                /
                (x2-x1)
                );
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




/*********************************************************************
	time.h
*********************************************************************/

#include "nsmtracker_time.h"

#if USE_QT4
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
	sndfile.h
*********************************************************************/

#if defined(INCLUDE_SNDFILE_OPEN_FUNCTIONS)

#if defined(FOR_WINDOWS)
#include <windows.h>
#define ENABLE_SNDFILE_WINDOWS_PROTOTYPES 1
#endif

#include <sndfile.h>

extern LANGSPEC SNDFILE *radium_sf_open(const wchar_t *filename, int mode, SF_INFO *sfinfo);
#if USE_QT4
extern SNDFILE *radium_sf_open(QString filename, int mode, SF_INFO *sfinfo);
#endif

#endif


/*********************************************************************
	colors.h
*********************************************************************/



enum ColorNums {
  ILLEGAL_COLOR_NUM = -1,
  
  START_CONFIG_COLOR_NUM = 0,

  LOW_EDITOR_BACKGROUND_COLOR_NUM = START_CONFIG_COLOR_NUM, // 0
  HIGH_EDITOR_BACKGROUND_COLOR_NUM,         // 15

  TEXT_COLOR_NUM,
  BAR_TEXT_COLOR_NUM,
  INSTRUMENT_NAME_COLOR_NUM,
  PORTAMENTO_NOTE_TEXT_COLOR_NUM,
  PORTAMENTO_END_NOTE_TEXT_COLOR_NUM,
  VELOCITY_TEXT_COLOR_NUM, 
  
  LOW_BACKGROUND_COLOR_NUM,
  HIGH_BACKGROUND_COLOR_NUM,                 // 11
  EDITOR_SLIDERS_COLOR_NUM,
  TRACK_SLIDER_COLOR_NUM,
  LINE_SLIDER_COLOR_NUM,
  BUTTONS_COLOR_NUM,                          // 13
  BUTTONS_ON_OFF_COLOR_NUM,
  BUTTONS_TEXT_COLOR_NUM,
  PEAKS_COLOR_NUM,
  PEAKS_0DB_COLOR_NUM,
  PEAKS_4DB_COLOR_NUM,
  SLIDER1_COLOR_NUM,
  SLIDER2_COLOR_NUM,
  SLIDER_DISABLED_COLOR_NUM,
  SLIDER_TEXT_COLOR_NUM,
  SLIDER_RECORDING_COLOR_NUM,
  
  WAVEFORM_COLOR_NUM,
  VELOCITY1_COLOR_NUM,
  VELOCITY2_COLOR_NUM,
  TRACK_SEPARATOR1_COLOR_NUM,
  TRACK_SEPARATOR2A_COLOR_NUM,
  TRACK_SEPARATOR2B_COLOR_NUM,
  RANGE_COLOR_NUM,  
  PITCH_LINE_COLOR_NUM,

  CURSOR_EDIT_ON_COLOR_NUM,                       // 7
  CURSOR_EDIT_OFF_COLOR_NUM,                       // 7
  PLAY_CURSOR_COLOR_NUM,
  
  AUTOMATION_INDICATOR_COLOR_NUM,
  AUTOMATION1_COLOR_NUM,
  AUTOMATION2_COLOR_NUM,
  AUTOMATION3_COLOR_NUM,
  AUTOMATION4_COLOR_NUM,
  AUTOMATION5_COLOR_NUM,
  AUTOMATION6_COLOR_NUM,
  AUTOMATION7_COLOR_NUM,
  AUTOMATION8_COLOR_NUM,
  TEMPOGRAPH_COLOR_NUM,

  PIANONOTE_COLOR_NUM,
  PIANOROLL_OCTAVE_COLOR_NUM,
  PIANOROLL_NOTE_NAME_COLOR_NUM,
  PIANOROLL_NOTE_BORDER_COLOR_NUM,
  
  SOUNDFONT_COLOR_NUM,
  SOUNDFILE_COLOR_NUM,
  CURRENT_SOUNDFILE_COLOR_NUM,

  ZOOMLINE_TEXT_COLOR_NUM1,
  ZOOMLINE_TEXT_COLOR_NUM2,
  ZOOMLINE_TEXT_COLOR_NUM3,
  ZOOMLINE_TEXT_COLOR_NUM4,
  ZOOMLINE_TEXT_COLOR_NUM5,
  ZOOMLINE_TEXT_COLOR_NUM6,
  ZOOMLINE_TEXT_COLOR_NUM7,

  MIXER_TEXT_COLOR_NUM,
  MIXER_BORDER_COLOR_NUM,
  NOTE_EVENT_INDICATOR_COLOR_NUM,
  NOTE_EVENT_INDICATOR_BORDER_COLOR_NUM,
  MIXER_EVENT_CONNECTION_COLOR_NUM,
  MIXER_AUDIO_CONNECTION_COLOR_NUM,
  MIXER_AUTOSUSPENSION_COLOR_NUM,
  MIXER_SELECTED_OBJECT_COLOR_NUM,
  MIXER_SELECTED_OBJECT_BORDER_COLOR_NUM,
  MIXERSTRIPS_CURRENT_INSTRUMENT_BORDER_COLOR_NUM,
   
  SEQUENCER_CURRTRACK_BORDER_COLOR_NUM,
  SEQUENCER_BACKGROUND_COLOR_NUM,
  SEQUENCER_BORDER_COLOR_NUM,
  SEQUENCER_TEXT_COLOR_NUM,
  SEQUENCER_NOTE_COLOR_NUM,
  SEQUENCER_NOTE_START_COLOR_NUM,

  SEQUENCER_BLOCK_HEADER_BACKGROUND_COLOR_NUM,
  SEQUENCER_BLOCK_BACKGROUND_COLOR_NUM,
  SEQUENCER_BLOCK_BORDER_COLOR_NUM,
  SEQUENCER_BLOCK_BAR_COLOR_NUM,
  SEQUENCER_BLOCK_BEAT_COLOR_NUM,
  SEQUENCER_BLOCK_SELECTED_COLOR_NUM,
  SEQUENCER_BLOCK_MULTISELECT_BACKGROUND_COLOR_NUM,
  SEQUENCER_BLOCK_INTERFACE_COLOR_NUM,
  SEQUENCER_BLOCK_AUDIO_FILE_BACKGROUND_COLOR_NUM,

  SEQUENCER_GRID_COLOR_NUM,
  SEQUENCER_TRACK_BORDER1_COLOR_NUM,
  SEQUENCER_TRACK_BORDER2_COLOR_NUM,
  SEQUENCER_CURSOR_COLOR_NUM,
  SEQUENCER_TIMELINE_BACKGROUND_COLOR_NUM,
  SEQUENCER_TIMELINE_ARROW_COLOR_NUM,
  SEQUENCER_NAVIGATOR_HANDLER_COLOR_NUM,
  SEQUENCER_NAVIGATOR_GRAYOUT_COLOR_NUM,
  SEQUENCER_TEMPO_AUTOMATION_COLOR_NUM,
  
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



/*********************************************************************
	vector.h
*********************************************************************/

struct vector_t_{
  int num_elements;
  int num_elements_allocated; // Private. Holds allocated size of 'elements'
  void **elements;
};

typedef struct vector_t_ vector_t;

#include "vector_proc.h"





/*********************************************************************
	global roots
*********************************************************************/

extern vector_t g_global_roots;


#ifdef __cplusplus
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
      replace_gc_root(t, new_t);
      t = new_t;
    }
    T *data(void) const {
      return t;
    }
  };
}

template<typename T> 
static inline T add_gc_root(T root){
  if(root != NULL)
    VECTOR_push_back(&g_global_roots, root);
  return root;
}

static inline void remove_gc_root(void *root){
  if(root != NULL)
    VECTOR_remove(&g_global_roots, root);
}

template<typename T>
static inline T replace_gc_root(T old_root, T new_root){
  if(old_root != NULL)
    remove_gc_root(old_root);
  if(new_root!=NULL)
    add_gc_root(new_root);
  return new_root;
}

#else

static inline void *add_gc_root(void *root){
  if(root!=NULL)
    VECTOR_push_back(&g_global_roots, root);
  return root;
}

static inline void remove_gc_root(void *root){
  if(root!=NULL)
    VECTOR_remove(&g_global_roots, root);
}

static inline void *replace_gc_root(void *old_root, void *new_root){
  if(old_root!=NULL)
    remove_gc_root(old_root);
  if(new_root!=NULL)
    add_gc_root(new_root);
  return new_root;
}

#endif


/*********************************************************************
	ratio.h
*********************************************************************/

#include "ratio_type.h"

static inline char *ratio_to_string(const Ratio ratio){
  return talloc_format("%d/%d", (int)ratio.numerator, (int)ratio.denominator);
}



typedef struct _radium_os_disk disk_t;



#include "dyn_type.h"


/*********************************************************************
	hashmap.h
*********************************************************************/

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


static inline bool DYN_equal(const dyn_t a1, const dyn_t a2){
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
    case INT_TYPE:
      return a1.int_number==a2.int_number;
    case FLOAT_TYPE:
      return a1.float_number==a2.float_number;
    case HASH_TYPE:
      return HASH_equal(a1.hash, a2.hash);
    case ARRAY_TYPE:
      return DYNVEC_equal(a1.array, a2.array);
    case RATIO_TYPE:
      return RATIO_equal(*a1.ratio, *a2.ratio);
    case BOOL_TYPE:
      return a1.bool_number==a2.bool_number;
  }

  R_ASSERT(false);
  return false;
}

static inline dyn_t DYN_create_string_dont_copy(const wchar_t *string){
  dyn_t a;
  a.type = STRING_TYPE;
  a.string = string;
  return a;
}

static inline dyn_t DYN_create_string(const wchar_t *string){
  return DYN_create_string_dont_copy(STRING_copy(string));
}

#if USE_QT4
static inline dyn_t DYN_create_string(QString string){
  return DYN_create_string_dont_copy(STRING_create(string));
}
#endif

static inline dyn_t DYN_create_string_from_chars(const char *chars){
  return DYN_create_string_dont_copy(STRING_create(chars));
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
    return a.int_number;
  if (a.type==FLOAT_TYPE)
    return a.float_number;
  if (a.type==RATIO_TYPE)
    return (double)a.ratio->numerator / (double)a.ratio->denominator;

  RError("DYN_get_double_from_number: 'a' is not a number, but a %s", DYN_type_name(a.type));
  return 0;
}

static inline int64_t DYN_get_int64_from_number(const dyn_t a){
  R_ASSERT_NON_RELEASE(DYN_is_number(a));
  if (a.type==INT_TYPE)
    return a.int_number;
  if (a.type==FLOAT_TYPE)
    return a.float_number;
  if (a.type==RATIO_TYPE)
    return (double)a.ratio->numerator / (double)a.ratio->denominator;

  RError("DYN_get_double_from_number: 'a' is not a number, but a %s", DYN_type_name(a.type));
  return 0;
}

static inline dyn_t DYN_create_ratio(const Ratio ratio){
  dyn_t a;
  a.type = RATIO_TYPE;
  a.ratio = (Ratio*)tcopy(&ratio, sizeof(Ratio));
  return a;
}

static inline dyn_t DYN_create_place(const Place place){
  return DYN_create_ratio(RATIO_minimize(make_ratio(place.counter + place.line*place.dividor, place.dividor)));
}

// (There is also a DYN_get_place function in placement_proc.h, and a DYN_get_liberal_ratio function further down in this file)
static inline Ratio DYN_get_ratio(const dyn_t dyn){
  if (dyn.type==INT_TYPE)
    return make_ratio(dyn.int_number, 1);

  if (dyn.type==RATIO_TYPE)
    return *dyn.ratio;

  RError("DYN_Get_ratio: dyn (type: %d) can not be converted to a ratio", dyn.type);

  return make_ratio(0,1);      
}

static inline bool DYN_is_ratio(const dyn_t a){
  if (a.type==INT_TYPE)
    return true;
  if (a.type==RATIO_TYPE)
    return true;
  
  return false;
}

// Also allows strings and floats.
static inline Ratio DYN_get_liberal_ratio(const dyn_t dyn){
  if (dyn.type==INT_TYPE)
    return make_ratio(dyn.int_number, 1);

  if (dyn.type==RATIO_TYPE)
    return *dyn.ratio;

  else if (dyn.type==FLOAT_TYPE)
    return make_ratio(dyn.float_number*1000, 1000);

  else if (dyn.type==STRING_TYPE)
    return RATIO_from_string(dyn.string);
    
  RError("DYN_Get_ratio: dyn (type: %d) can not be converted to a ratio", dyn.type);

  return make_ratio(0,1);      
}

static inline bool DYN_is_liberal_ratio(const dyn_t a){
  return a.type==INT_TYPE || a.type==RATIO_TYPE || a.type==FLOAT_TYPE || a.type==STRING_TYPE;
}




/*
Must include placement_proc.h to get this function.
static inline Place DYN_get_place(const dyn_t dyn){
  Ratio ratio = DYN_get_ratio(dyn);
  return place_from_64b(ratio.numerator, ratio.denominator);
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
  a.array = (dynvec_t*)tcopy(&dynvec, sizeof(dynvec_t));
  return a;
}



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
  ((symbol_t*)symbol)->name = strdup(new_name);
}
                                   

/*********************************************************************
	quantitize.h
*********************************************************************/

typedef struct{
  Ratio quant;
  /*
    // In scheme, we read these three variables directly from the gui now.
  bool quantitize_start;
  bool quantitize_end;
  bool keep_note_length;
  */
  int type;
} quantitize_options_t;



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




/*********************************************************************
	velocities.h
*********************************************************************/

#define MAX_VELOCITY (1<<16)

struct Velocities{
	struct ListHeader3 l;

	int velocity;
	int logtype;
};
#define NextVelocity(a) ((struct Velocities *)((a)->l.next))

static inline float VELOCITY_get(int velocity){
  return scale_double(velocity,0,MAX_VELOCITY,0,1);
}

/*********************************************************************
	pitches.h
*********************************************************************/

struct Pitches{
	struct ListHeader3 l;
  
	float note;
	int logtype;

        int chance;
};
#define NextPitch(a) ((struct Pitches *)((a)->l.next))



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
  
	Place end;

        int chance;
  
	struct Velocities *velocities;
	int velocity_end;
  
	struct Pitches *pitches;
        float pitch_end; // If pitch_end==0 and pitches==NULL, there's no pitch changes for this note.
  
	struct Velocities first_velocity; // used by nodelines
	struct Velocities last_velocity; // used by nodelines

        int polyphony_num; //subtrack;

	int noend;

        bool has_sent_seqblock_volume_automation_this_block;
#if !defined(RELEASE)
        bool has_automatically_sent_seqblock_volume_automation_this_block;
#endif
        int64_t id;
};
#define NextNote(a) ((struct Notes *)((a)->l.next))




/*********************************************************************
	patch.h
*********************************************************************/

struct Tracks;

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
  enum TimeFormat time_format;
  float chance;
};

#define NUM_PATCH_VOICES 7
#define MAX_NUM_EVENT_RECEIVERS 64
#define MAX_NOTE_INTENCITY 20

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
  char midi_channel;
  char voicenum;
  int64_t sample_pos; // for sample seek
} note_t;

static inline bool is_note(note_t note, int64_t id, const struct SeqBlock *seqblock){
  return note.id==id && note.seqblock==seqblock;
}

#define NOTE_ID_RESOLUTION 256 // i.e. 256 id's per note.
static inline int64_t NotenumId(float notenum){
  int64_t n = notenum*NOTE_ID_RESOLUTION;
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


// #define CAST_API_PATCH_ID(a) ((int)a) // When API is converted to 64 bit, this definition should be changed to just 'a'.

// Note that Patch objects are stored directly in undo/redo (not copied), so it must not be freed, reused for other purposes, or othervice manipulated when not available.
struct Patch{
  int64_t id;
  const char *uuid; // Not currently used for anything.
  
  bool is_usable; // If pasting a track with this patch, this flag tells whether the patch can be used on the new track.
  hash_t *state; // If is_usable==false, this field contains the plugin state.
  
  bool name_is_edited;
  const char *name;
  const symbol_t *midi_learn_port_name;
  
  const char *comment;

  unsigned int color;
  //int colornum;

  void (*playnote)(struct SeqTrack *seqtrack, struct Patch *patch,note_t note,STime time);
  void (*changevelocity)(struct SeqTrack *seqtrack, struct Patch *patch,note_t note,STime time);
  void (*changepitch)(struct SeqTrack *seqtrack, struct Patch *patch,note_t note,STime time);
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

  DEFINE_ATOMIC(int, visual_note_intencity); // Used by the mixer to keep track of how bright the note indicator should light up.

  DEFINE_ATOMIC(bool, widget_needs_to_be_updated);

  DEFINE_ATOMIC(bool, is_recording);
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
	fx.h
*********************************************************************/

typedef enum {
  FX_start = 0,   // Used by automation and envelope controller
  FX_middle = 1,  // Used by automation and envelope controller
  FX_end = 2,     // Used by automation and envelope controller
  FX_single = 3,
  //FX_no_fx = 4; // No effect must be sent out. Used by seqtrack automation.
} FX_when;

static inline bool FX_when_is_automation(FX_when when){
  return (when != FX_single);
}

struct FX{
	const char *name;
	enum ColorNums color;
	void (*configureFX)(struct FX *fx,struct Tracks *track);
        int min; // can not be equal to INT32_MIN (automation can fail if that is the case)
        int max; // same here

        struct Patch *patch;
  
  	int effect_num; // Set by the instrument plugin. For audio, this is the effect num, while for midi, this is cc (plus some special rules for 14 bit cc and pitch change).

  /*
	// Having pointers to variables in sliders is a bit strange, but letting sliders reference FX instead would cause bookkeeping of live and not alive FX objects.
	// Not getting that bookkeeping correct would mean crashes that could be difficult to track.
	// This, on the other hand, is safe, since sliders are always alive as long as the Patch is alive, and the patch always outlives an FX object.
	// (The refactor to let Patch own FX hasn't been done yet. It didn't make sense when there were only MIDI instrument, but now it's too complicated to let FX live independently.
        //  However, when an instrument is deleted, all tracks are scanned, and FX are deleted when a patch is deleted. Same when changing patch for a track.)
        DEFINE_ATOMIC(float *, slider_automation_value); // Pointer to the float value showing automation in slider. Value is scaled between 0-1. May be NULL.
        DEFINE_ATOMIC(enum ColorNums   *, slider_automation_color); // Pointer to the integer holding color number for showing automation in slider. May be NULL.
  */
  
        void (*treatFX)(struct SeqTrack *seqtrack, struct FX *fx,int val,STime time,int skip, FX_when when, double block_reltempo);

        void (*call_me_before_starting_to_play_song_MIDDLE)(struct FX *fx, int val, int64_t abstime, FX_when when);

	void (*closeFX)(struct FX *fx,const struct Tracks *track);
	void *fxdata;	//Free use for the instrument plug-in.
	void (*SaveFX)(struct FX *fx,const struct Tracks *track);
        int (*defaultFXValue)(const struct FX *fx);

  //void (*setFXstring)(struct FX *fx,struct Tracks *track, char *string);
};
#define FX_FAILED 0
#define FX_SUCCESS 1


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

struct Tracker_Windows;
struct Blocks;
struct Tracks;
struct Instruments{
	struct ListHeader1 l;

	const char *instrumentname;

        vector_t patches; // Not safe to traverse from player thread.

        //int (*getMaxVelocity)(const struct Patch *patch);

        vector_t *(*getFxNames)(const struct Patch *patch);
        struct FX *(*createFX)(const struct Tracks *track, struct Patch *patch, int effect_num);
  
	int (*getFX)(struct Tracker_Windows *window,const struct Tracks *track,struct FX *fx);
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
	char *(*getPatchData)(struct Patch *patch, const char *key);
};
#define INSTRUMENT_FAILED 0
#define INSTRUMENT_SUCCESS 1
#define NextInstrument(a) ((struct Instruments *)((a)->l.next))



/*********************************************************************
	stops.h
*********************************************************************/


struct Stops{
	struct ListHeader3 l;
};
#define NextStop(a) ((struct Stops *)((a)->l.next))



/*********************************************************************
	fxnodelines.h
*********************************************************************/


struct FXNodeLines{
	struct ListHeader3 l;
	int val;
    	int logtype;
};
#define NextFXNodeLine(a) ((struct FXNodeLines *)((a)->l.next))


struct FXs{
	struct FX *fx;
	struct FXNodeLines *fxnodelines;
};



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

	struct Notes *notes;
	struct Stops *stops;
	int onoff;

        int polyphony;
  
        const char *trackname; // Contains the value "(click me)" when patch==NULL
	struct Patch *patch;
        vector_t fxs; // Contains struct FXs* elements

        struct Swing *swings;
        dyn_t  filledout_swings; // Used both to calculate timing, and for rendering. Calculated from block->beats, block->swings, and swings.
        const struct STimes *times;			/* Pointer to array. Last element (times[num_lines]) is the playtime of the block. Calculated from block->lpbs/block->tempos/block->temponodes/root->lpb/root->bpm/filledout_swings. */
  
        void *midi_instrumentdata;			/* Used by the midi instrument. */

	int pan;
	int volume;

	bool panonoff;
        bool volumeonoff;                      /* The volume-button on/off, not track on/off. (i.e. if off, volume=1.0, not 0.0) */

        DEFINE_ATOMIC(int, midi_channel);
  
        DEFINE_ATOMIC(bool, is_recording);
};
#define NextTrack(a) ((struct Tracks *)((a)->l.next))

#define MAXTRACKVOL 1000
#define MAXTRACKPAN 1000

static inline float TRACK_get_pan(const struct Tracks *track){
  if(track->panonoff)
    return scale(track->pan, -MAXTRACKPAN, MAXTRACKPAN, -1.0, 1.0);
  else
    return 0.0f;
}

static inline float TRACK_get_volume(const struct Tracks *track){
  if(track->volumeonoff)
    return scale(track->volume, 0, MAXTRACKVOL, 0, 1);
  else
    return 1.0f;
}

static inline float TRACK_get_velocity(const struct Tracks *track, int velocity){
  return TRACK_get_volume(track) * VELOCITY_get(velocity);
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

typedef struct{
  Place p;
  struct Notes *note;
  struct Pitches *pitch;
  struct Stops *stop;
  bool is_end_pitch;
} TrackRealline2;

typedef struct{
  Place p;
  struct Notes *note;
  struct Velocities *velocity;
  int value;
  int logtype;
  bool is_first_velocity;
  bool is_last_velocity;
} VelText;

typedef struct{
  Place p;
  const struct FX *fx;
  struct FXNodeLines *fxnodeline;

  int value;
  int logtype;
} FXText;



#if USE_QT4
typedef QList<TrackRealline2> Trs;
typedef QMap<int, Trs> Trss;

//typedef QList<struct Notes*> Waveform_trs;
typedef QMap<int, bool> Waveform_trss;

typedef QList<VelText> VelText_trs;
typedef QMap<int, VelText_trs> VelText_trss;

typedef QList<FXText> FXText_trs;
typedef QMap<int, FXText_trs> FXText_trss;

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

struct NodeLine{
  struct NodeLine *next;

  float x1,y1;
  float x2,y2;

  const struct ListHeader3 *element1;
  const struct ListHeader3 *element2;

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

	int x,y,x2,y2;						/* GFX area. */

	int notesonoff;					/* notearea and placementarea on/off. */
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

	struct Tracks *track;			/* Only referenced. wtracknum=track->tracknum */

	TBox pan;
	TBox volume;

	TBox panonoff;
	TBox volumeonoff;

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

extern struct CurrentPianoNote current_piano_note;

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

  nodelineBox.x1 = x_min-note_width/2.0;
  nodelineBox.y1 = y_min;
  nodelineBox.x2 = x_max+note_width/2.0;
  nodelineBox.y2 = y_max;

  return nodelineBox;
}

struct PianorollRubber{
  float pitch1;
  float pitch2;
  Place place1;
  Place place2;
  int tracknum;
  int blocknum;
};

extern struct PianorollRubber g_current_pianobar_rubber;


/*********************************************************************
	Signature.h
*********************************************************************/

struct Signatures{
  struct ListHeader3 l;
  Ratio signature;
};
#define NextSignature(a) (struct Signatures *)((a)->l.next)

#if USE_QT4

#pragma clang diagnostic push
#pragma clang diagnostic ignored "-Wshorten-64-to-32"
#include <QVector>
#pragma clang diagnostic pop

#define SIGNATURE_NORMAL 0
#define SIGNATURE_BELOW 1
#define SIGNATURE_MUL 2

struct WSignature{
  Ratio signature;
  int bar_num;
  int beat_num;   // In a 4/4 measure, this value is either 0, 1, 2 or 3, or 4. (0 means that there is no beat placed on this realline)
  int type;	  /* 0=normal, 1=below positioned, 2=mul. */
  QVector<float> how_much_below;  /* If type is 1 or 2, these values contains how much below (between 0 and 1) */
  //vector_t how_much_below;

  WSignature()
    : signature(make_ratio(0,0))
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
  Ratio signature; // Current signature for this beat.
  int bar_num;   // Starts counting from 1.
  int beat_num;  // For instance, in a 4/4 measure, this value is either 1, 2 or 3, or 4. Starts counting from 1.

  Ratio valid_signature; // signature is 0/0 if there isn't a signature change at this beat.
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

struct STimeChange;

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
  
        int num_time_lines; // Contains number of lines in 'times' minus one (same as num_lines, normally). Only for validation.
        const struct STimes *times_with_global_swings; // Pointer to array. Last element (times[num_lines]) is the playtime of the block. Calculated from lpbs/tempos/temponodes/global lpb/global bpm/filledout_swings.
        const struct STimes *times_without_global_swings;
        const struct STimes *times;  //  Either points to 'times' or 'times_with_global_swings', depending on whether plugins should receive swing tempo or not.
        
        DEFINE_ATOMIC(double, reltempo);					/* factor that the tempo is multiplied with when playing this block. */

        DEFINE_ATOMIC(double, player_time);	/* = pc->end_time - RT_curr_seqblock()->time */

  unsigned int color;
  
  // This variable is checked after each keyboard or menu event. If true, trackreallines, wtracks, etc. will be updated.
  bool is_dirty; 
};
#define NextBlock(a) (struct Blocks *)((a)->l.next)


extern DEFINE_ATOMIC(bool, g_use_track_channel_for_midi_input);
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


struct WBlocks{
	struct ListHeader1 l;

	int tempotrackonoff;				/* 1=on, 0=off */
	int temponodetrackonoff;		/* 1=on, 0=off */

	TBox a; // everything
//	int x,y,x2,y2;						/* GFX area. */

        int skew_x; // All x values in the tracks has this value added to them. It is always 0 or negative. Set when moving the track scrollbar.

	TBox t;
//	int tx,ty,tx2,ty2;				/* lines, nodes, etc. GFX area. */

        TBox bottombar;                                /* reltempo slider and track slider */

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
        struct Notes *mouse_note; // The note the mouse is currently above. NULL if mouse is not above a note.

        struct FXs *mouse_fxs; // The fxs the mouse is currently above. NULL if mouse is not above an fx.
  
	struct Blocks *block;			/* Only referenced. wblocknum=block->blocknum */

	struct LocalZooms *localzooms;    /* These two variables (localzooms and reallines) contain the same elements, but 'localzooms' is organized as a tree, while 'reallines' is organized as an array. (roughly)*/
        const struct LocalZooms **reallines;   // Used by the player. Must be protected by PLAYER_lock. Also Used by the OpenGL thread. The content must not be modified after creation, but it can be replaced. It's also fine to modify the content of individual elements. The OpenGL thread only uses the ->l.p values.

	int num_reallines;               // Same here. Must be protected by PLAYER_lock.

        int num_expand_lines;

	struct WTracks *wtracks;
	struct WTracks *wtrack;			/* Current track. Only referenced. */

  /*
	NInt left_track;				
	int left_subtrack;
	NInt right_track;				
	int right_subtrack;
*/

        //struct WTempos *wtempos;
	double reltempomax;

	bool isranged;
	NInt rangex1;
	NInt rangex2;
	Place rangey1;
	Place rangey2;

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
	int curr_track_sub;				/* -1=note, 0,1,2,...,n=vel */
        NInt curr_block; // Used by P2MUpdateSongPosCallBack to keep track of whether to switch current block. The value -1 means that current block is not visible. (happens when playing song)
        int curr_othertrack_sub; // subtrack for bpm track, and so forth.
        
	int maxwtracksize;					/* The size of the widest wtrack for all wblocks. */

	struct Slider bottomslider;
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

  Ratio last_valid_signature; // = {4,4};

  int last_played_metronome_note_num; // = -1;

  bool new_beat_bar_set; // = false;
} Beat_Iterator;


typedef struct {
  const struct Signatures *next_signature;
  Ratio signature_value; // = {4,4};
} Signature_Iterator;

#define MAX_SEQBLOCK_VOLUME_ENVELOPE_DB 6
#define MAX_DISABLED_SEQBLOCK_TRACKS 512

struct SeqBlockTimings{
  int64_t time;
  
  // End seqtime.
  // When seqblock is NOT stretched, end_time = time + getBlockSTimeLength(seqblock->block).
  int64_t time2;

  
  int64_t default_duration; // Usually has value Place2Stime(end_place)-Place2Stime(start_place). Also used to set the stretch value.

  
  Place start_place; // usually {0,0,1} (not used yet). Only used if block!=NULL

  Place end_place;   // usually {num_lines,0,1} (not used yet)

  bool is_looping;
  int64_t interior_start; // seqtime version of start_place (non-stretched value)
  int64_t interior_end;   // seqtime version of end_place (non-stretched value)

  //int64_t noninterior_start; // The seqtime start if interior_start==0
  //int64_t noninterior_end; // The seqtime end if interior_end==0

  double stretch_without_tempo_multiplier;
  
  // stretch = (end_time-time) / getBlockSTimeLength(seqblock->block)
  // 1.0 = no stretch. 0.5 = double tempo. 2.0 = half tempo.
  // Only used for converting stime -> seqtime a little bit faster. Updated when the result of end_time-time or getBlockSTimeLength(seqblock->block) changes.
  // Must not be used to find seqblock duration (i.e end_time-time).
  double stretch;
};

struct SeqblockEnvelope;

// Note: The values 0,1,2,3,4,5,6 are used directly in bin/scheme/mouse.scm. Probably simplest not to change any of the types, and just append new types to the end.
enum SeqblockBoxSelected{
  SB_NO_SELECTED,
  SB_FADE_LEFT,
  SB_FADE_RIGHT,
  SB_STRETCH_LEFT,
  SB_STRETCH_RIGHT,
  SB_INTERIOR_LEFT,
  SB_INTERIOR_RIGHT
};

struct SeqBlock{
  int seqblocknum; // Must be unique. Can change value when player is stopped.

  // FIX: Don't need two of these anymore. Pretty sure they never differ now.
  struct SeqBlockTimings t;
  struct SeqBlockTimings gfx;

  /*
  // Start seqtime.
  // Player must be stopped when modifying this variable.
  // Note that because of tempo multipliers (block->reltempo), the 'start_time' and 'end_time' fields does not correspond linearly to this value.
  // Written to by the main thread, read by the main thread and the player thread(s).
  int64_t time;
  int64_t gfx_time;  // Usually contains the same value as 'time', but when moving seqblocks with the mouse, it contains the currently visible time instead. When releasing the mouse button, 'time' will get the same value as 'gfx_time'.
  
  // End seqtime.
  // When seqblock is NOT stretched, end_time = time + getBlockSTimeLength(seqblock->block).
  int64_t time2;
  int64_t gfx_time2; // (see gfx_time)

  
  int64_t default_duration; // Usually has value Place2Stime(end_place)-Place2Stime(start_place). Also used to set the stretch value.
  int64_t gfx_default_duration;


  Place start_place; // usually {0,0,1} (not used yet). Only used if block!=NULL
  Place gfx_start_place; // (not used yet). Only used if block!=NULL

  Place end_place;   // usually {num_lines,0,1} (not used yet)
  Place gfx_end_place; // (not used yet)

  bool is_looping;
  int64_t interior_start; // seqtime version of start_place
  int64_t interior_end;   // seqtime version of end_place
  
  // stretch = (end_time-time) / getBlockSTimeLength(seqblock->block)
  // 1.0 = no stretch. 0.5 = double tempo. 2.0 = half tempo.
  // Only used for converting stime -> seqtime a little bit faster. Updated when the result of end_time-time or getBlockSTimeLength(seqblock->block) changes.
  // Must not be used to find seqblock duration (i.e end_time-time).
  double stretch;
  double gfx_stretch; // (see gfx_time)
  */
  
  struct Blocks *block; // If NULL, then the seqblock holds a sample.

  int64_t sample_id; // Has valid value if block==NULL.
  const wchar_t *sample_filename;
  const wchar_t *sample_filename_without_path;

  bool *track_is_disabled; // Is NULL in the seqblock used when playing block.
  
  
  bool is_selected;

  enum SeqblockBoxSelected selected_box;

  double fadein; // value between 0 and 1
  double fadeout; // value between 0 and 1

  bool envelope_enabled;
  float envelope_volume; // gain (current envelope_volume calculated from envelope+fadein+fadeout, updated each block)
  float envelope_db;     // db version of envelope_volume
  struct SeqblockEnvelope *envelope;
  bool envelope_volume_changed_this_block;
  
  // 'start_time' and 'end_time' are absolute times.
  // They are only used the main thread.
  //
  // Note that these two values are NOT updated when seqblock->time or seqblock->block is changed.
  // Instead, the function SEQTRACK_update_seqblock_start_and_end_times or the function SEQTRACK_update_seqblock_gfx_start_and_end_times
  // must be called right before reading any of these two variables.
  // (using these two variables is functional programming in disguise. We could have had a function returning
  //  a vector with all these values instead, but that would have been more inconvenient and slower (although speed probably doesn't matter here).)
  double start_time; // in seconds. TODO: Change to frames.
  double end_time; // in seconds. TODO: Change to frames.
};


static inline double get_note_reltempo(note_t note){
  if (note.seqblock==NULL)
    return 1.0f;
  else
    return ATOMIC_DOUBLE_GET(note.seqblock->block->reltempo);
}



struct _scheduler_t;
typedef struct _scheduler_t scheduler_t;

struct SeqtrackAutomation;

struct SeqTrack{
  vector_t seqblocks; // Player must be stopped when modifying this variable. Also used for displaying if gfx_seqblocks != NULL.
  vector_t *gfx_seqblocks; // Used for displaying. Might have the same content as this->seqblocks (points to).
  vector_t gfx_gfx_seqblocks; // Just for graphics. Player does not have to be stopped when modifying this variable

  struct SeqBlock *curr_seqblock; // curr_seqblock->block and curr_seqblock->time contains the same values as pc->block and pc->seqtime did before introducing seqtrack/seqblock.
  struct SeqBlock *curr_sample_seqblock; // Currently only used for displaying audiofile name in the editor. Note that curr_sample_seqblock->sample_id might not always be valid.
  
  double start_time; // Current seqtime. Can only be accessed from the player thread.
  double end_time;   // Same here. (should be the same as start_time + RADIUM_BLOCKSIZE)

  // These two variables are here only for convenience (and maybe a little bit of efficency) so that we don't have to do atomic operations on start_time and end_time in the player thread.
  // They contain the same values as 'start_time" and 'end_time' above.
  DEFINE_ATOMIC(double, start_time_nonrealtime);
  DEFINE_ATOMIC(double, end_time_nonrealtime);

  //Place p; // Used by the scheduler when starting to play in the middle of a block

  LPB_Iterator lpb_iterator; // Used by scheduler_LPB.c to keep track of timing (PPQ and BPM)
  Beat_Iterator beat_iterator;
  Signature_Iterator signature_iterator;

  struct SeqtrackAutomation *seqtrackautomation;

  struct Patch *patch; // A "Sequencer audio file recorder/player" audio plugin.
  
  scheduler_t *scheduler;
};


static inline double get_seqtrack_reltempo(struct SeqTrack *seqtrack){
  if (seqtrack==NULL)
    return 0.0;
  
  struct SeqBlock *seqblock = seqtrack->curr_seqblock;
  if (seqblock==NULL)
    return 1.0f; // <--- NOTE: Changed this value from 0.0f to 1.0f. Seems wrong that it should be 0.0f.

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

struct Looping{
  DEFINE_ATOMIC(bool, enabled);
  int64_t start; // abstime
  DEFINE_ATOMIC(int64_t, end);   // abstime
};


struct Song{
	struct Tracker_Windows *tracker_windows;
	struct Blocks *blocks;

        struct SeqTrack *block_seqtrack; // Used when playing block.

        struct Looping looping;
        DEFINE_ATOMIC(int, curr_seqtracknum);
        vector_t seqtracks; // New playlist. Player must both be stopped and locked when modifying this variable, or any of the contents.

	NInt num_blocks;
	char *songname;

        bool linear_accelerando; // player must be stopped when writing to, or hold the player lock.
        bool linear_ritardando; // player must be stopped when writing to, or hold the player lock.
        bool plugins_should_receive_swing_tempo;
        bool editor_should_swing_along;

        bool mixer_comments_visible;
        bool mute_editor_automation_when_track_is_muted;
        int num_channels_in_main_pipe;

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
	Ratio signature;		/* Standard signature. Player must be stopped when modifying. */

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

// these two contains the same value, but g_is_starting_up can only be accessed from the main thread.
extern DEFINE_ATOMIC(bool, is_starting_up);
extern bool g_is_starting_up;
extern bool g_is_replacing_main_pipe;

extern bool g_embed_samples;
extern bool g_curr_song_contains_embedded_samples;

static inline struct SeqTrack *SEQUENCER_get_curr_seqtrack(void){
  int curr_seqtracknum = ATOMIC_GET(root->song->curr_seqtracknum);

  R_ASSERT_NON_RELEASE(curr_seqtracknum >= 0);
  R_ASSERT_NON_RELEASE(curr_seqtracknum < root->song->seqtracks.num_elements);
  
  vector_t *seqtracks = &root->song->seqtracks;
  if (curr_seqtracknum >= seqtracks->num_elements)
    return NULL;
  else
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

static inline struct Blocks *RT_get_curr_visible_block(void){
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

static inline note_t create_note_t_plain(const struct SeqBlock *seqblock,
                                         int64_t note_id,
                                         float pitch,
                                         float velocity,
                                         float pan,
                                         char midi_channel,
                                         char voicenum,
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
  
  midi_channel = R_BOUNDARIES(0,midi_channel, 15);
  
  if(note_id<=-1)
    note_id = NotenumId(pitch);

  note_t note = {
    .id = note_id,
    .seqblock = seqblock,
    .pitch = pitch,
    .velocity =velocity,
    .pan = pan,
    .midi_channel = midi_channel,
    .voicenum = voicenum,
    .sample_pos = sample_pos
  };
  
  return note;  
}

static inline note_t create_note_t(const struct SeqBlock *seqblock,
                                   int64_t note_id,
                                   float pitch,
                                   float velocity,
                                   float pan,
                                   char midi_channel,
                                   char voicenum,
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
                                    char midi_channel
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


#endif

