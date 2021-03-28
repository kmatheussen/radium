#pragma once

extern void RT_mempool_init(void);
extern void RT_inc_ref_raw(void *mem); // Reference counting. (Use RT_free() to decrement). Note that this function is quite efficient.
extern void RT_free_raw(void *mem, const char *who);
extern void *RT_alloc_raw(int size, const char *who);

template<typename T> struct RT_Mem;

template<typename T> 
static inline void RT_inc_ref(RT_Mem<T> *mem){
  return RT_inc_ref_raw(mem);
}

template<typename T> 
static inline void RT_free(RT_Mem<T> *mem, const char *who){
  RT_free_raw(mem, who);
}

template<typename T> 
static inline RT_Mem<T> *RT_alloc(int size, const char *who){
  return static_cast<RT_Mem<T>*>(RT_alloc_raw(size, who));
}
/*
template<typename T> 
static inline RT_Mem<T> *RT_alloc(size_t size, const char *who){
  return RT_alloc((int)size, who);
}
*/
template<typename T> 
static inline T *RT_data(RT_Mem<T> *mem){
  return (T*)(mem);
}
