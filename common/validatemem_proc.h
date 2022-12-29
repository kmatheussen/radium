

#ifndef RADIUM_COMMON_VALIDATE_MEM_PROC_H
#define RADIUM_COMMON_VALIDATE_MEM_PROC_H

#include "threading_lowlevel.h"


#if 0

#if !defined(RELEASE)
  // Probably want to comment out the line below if compiling with -fsanitize=thread
  #if !defined DISABLE_BDWGC
    #if FOR_LINUX
      #define VALIDATE_MEM 1 // VALIDATE_MEM validates mem allocated by bdwgc that fsanitize=address doesn't. However, for best memory validation, DISABLE_BDWGC should be defined instead.
    #endif
  #endif
#endif

#endif

#if defined(RELEASE)
  #if defined(VALIDATE_MEM)
    #error "oops"
  #endif
#endif



typedef void *(*MemoryAllocator)(size_t size);
typedef void (*MemoryFreeer)(void* mem);

#ifdef __cplusplus
extern "C" {
#endif
  void V_validate(void *mem);
  void V_validate_all(void);
  void V_run_validation_thread(void); // Is run automatically unless DONT_RUN_VALIDATION_THREAD is defined
  
  void *V_alloc(MemoryAllocator allocator, int size, const char *filename, int linenumber);

  void V_free_actual_mem_real_start(MemoryFreeer freeer, void *actual_mem_real_start);
  void V_free_it(MemoryFreeer freeer, void *allocated_mem);

  // too complicated:
  //void *V_realloc_it(MemoryFreeer freeer, void *ptr, size_t size, const char *filename, int linenumber);

  // Use these ones instead of V_realloc_it, and realloc manually.
  MemoryAllocator V_get_MemoryAllocator(void *mem);
  int V_get_size(void *mem);
  
  void *V_allocated_mem_real_start(void *allocated_mem);
  
  void *V_malloc__(size_t size, const char *filename, int linenumber);
  char *V_strdup__(const char *s, const char *filename, int linenumber);
  char *V_wcsdup__(const wchar_t *s, const char *filename, int linenumber);
  void *V_calloc__(size_t n, size_t size, const char *filename, int linenumber);
  void V_free__(void *ptr);
  void *V_realloc__(void *ptr, size_t size, const char *filename, int linenumber);
#ifdef __cplusplus
}
#endif




#if !defined(VALIDATE_MEM)


static inline void* my_calloc(size_t size1,size_t size2) __attribute__((returns_nonnull));

static inline void* my_calloc(size_t size1,size_t size2) {
  size_t size = size1*size2;

  // 1. allocate
  //

  char*  ret  = (char*)malloc(size);
  if (ret==NULL){
    R_ASSERT(false);
#if !defined(TEST_TIMEDATA_MAIN)
    msleep(10000);
#endif
    return calloc(1,size); // return calloc(1,size) instead of NULL to avoid null-reference warning.
  }
  
  // 2. Ensure the memory is physically available.
  //

  int64_t *ret64 = (int64_t*)ret;
  size_t s2=size/sizeof(int64_t);

  for(size_t i=0;i<s2;i++)
    ret64[i]=0;

  for(size_t i=s2*sizeof(int64_t);i<size;i++)
    ret[i] = 0;
  
  return ret;
}

#  if defined(RELEASE)
#    define V_malloc(size) malloc(size)
#    define V_strdup(s) strdup(s)
#    define V_wcsdup(s) wcsdup(s)
#    define V_calloc(n, size) my_calloc(n, size)
#    define V_free(ptr) free((void*)ptr)
#    define V_free_function free
#    define V_realloc(ptr, size) realloc(ptr, size);
#  else

#    define V_free_function V_free

static inline void assert_allowed_to_alloc(void){
#if 0
  
  if (PLAYER_current_thread_has_lock())
    abort();

  if (THREADING_is_runner_thread())
    abort();

#if !defined(FOR_MACOSX)
  if (THREADING_has_player_thread_priority())
    abort();
#endif
  
#else
  
  R_ASSERT(!PLAYER_current_thread_has_lock());
  R_ASSERT(!THREADING_is_runner_thread());

  #if 0
    #if !defined(FOR_MACOSX)
      #if defined(COMPILING_RADIUM)
  if (!ATOMIC_GET(is_starting_up)) // turn off check for now. @#$@#$ juce.
      #endif
    R_ASSERT(!THREADING_has_player_thread_priority());
    #endif
  #endif
#endif
}

static inline void *V_malloc(size_t size){
  assert_allowed_to_alloc();
  return malloc(size);
}
static inline char *V_strdup(const char *s){
  R_ASSERT(MIXER_is_saving() || !PLAYER_current_thread_has_lock());
  return strdup(s);
}
static inline wchar_t *V_wcsdup(const wchar_t *s){
  R_ASSERT(MIXER_is_saving() || !PLAYER_current_thread_has_lock());
  return wcsdup(s);
}


static inline void *V_calloc(size_t n, size_t size)  __attribute__((returns_nonnull));

static inline void *V_calloc(size_t n, size_t size){
  assert_allowed_to_alloc();
  return my_calloc(n,size);
}


#ifndef __clang__
#include <malloc.h>
#endif

static inline void V_free(void *ptr){
  assert_allowed_to_alloc();
  
#ifndef __clang__
#if defined(FOR_LINUX)
#if !defined(RELEASE)
  if (ptr != NULL)
    memset(ptr, -1, malloc_usable_size(ptr)); // for debugging. Crashes faster if something is wrong.
#endif
#endif
#endif
  free(ptr);
}
static inline void *V_realloc(void *ptr, size_t size){
  assert_allowed_to_alloc();
  return realloc(ptr, size);
}

#  endif

static inline void V_shutdown(void){
}
  
#else

#  define V_malloc(size) V_malloc__(size, __FILE__, __LINE__)
#  define V_strdup(s) V_strdup__(s, __FILE__, __LINE__)
#  define V_wcsdup(s) V_wcsdup__(s, __FILE__, __LINE__)
#  define V_calloc(n, size) V_calloc__(n, size, __FILE__, __LINE__)
#  define V_free(ptr) V_free__((void*)(ptr))
#  define V_realloc(ptr, size) V_realloc__((void*)ptr, size, __FILE__, __LINE__)

#  ifdef __cplusplus
extern "C" {
#  endif
  void V_shutdown(void);
#  ifdef __cplusplus
}
#  endif

#endif // !RELEASE



#if 0

  // This can be added to various files

  void *V_malloc__(size_t size, const char *filename, int linenumber);
  char *V_strdup__(const char *s, const char *filename, int linenumber);
  void *V_calloc__(size_t n, size_t size, const char *filename, int linenumber);
  void V_free__(void *ptr);
  void *V_realloc__(void *ptr, size_t size, const char *filename, int linenumber);

  #define V_malloc(size) V_malloc__(size, __FILE__, __LINE__)
  #define V_strdup(s) V_strdup__(s, __FILE__, __LINE__)
  #define V_calloc(n, size) V_calloc__(n, size, __FILE__, __LINE__)
  #define V_free(ptr) V_free__((void*)(ptr))
  #define V_realloc(ptr, size) V_realloc__((void*)ptr, size, __FILE__, __LINE__)

  #define malloc(a) V_malloc(a)
  #define strdup(s) V_strdup(s)
  #define calloc(a,b) V_calloc(a,b)
  #define realloc(a,b) V_realloc(a,b)
  #define free(a) V_free(a)

#endif

  
#endif
