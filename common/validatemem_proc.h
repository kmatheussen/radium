

#ifndef RADIUM_COMMON_VALIDATE_MEM_PROC_H
#define RADIUM_COMMON_VALIDATE_MEM_PROC_H

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
  void *V_calloc__(size_t n, size_t size, const char *filename, int linenumber);
  void V_free__(void *ptr);
  void *V_realloc__(void *ptr, size_t size, const char *filename, int linenumber);
#ifdef __cplusplus
}
#endif

#if defined(RELEASE)

#define V_malloc(size) malloc(size)
#define V_strdup(s) strdup(s)
#define V_calloc(n, size) calloc(n, size)
#define V_free(ptr) free((void*)ptr)
#define V_realloc(ptr, size) realloc(ptr, size);

#else // RELEASE -> !RELEASE

#define V_malloc(size) V_malloc__(size, __FILE__, __LINE__)
#define V_strdup(s) V_strdup__(s, __FILE__, __LINE__)
#define V_calloc(n, size) V_calloc__(n, size, __FILE__, __LINE__)
#define V_free(ptr) V_free__((void*)(ptr))
#define V_realloc(ptr, size) V_realloc__((void*)ptr, size, __FILE__, __LINE__)

#endif // !RELEASE

  
#endif
