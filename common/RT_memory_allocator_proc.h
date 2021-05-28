#pragma once

extern void RT_mempool_init(void);
extern void RT_inc_ref_raw(void *mem); // Reference counting. (Use RT_free() to decrement). Note that this function is quite efficient.
extern void RT_free_raw(void *mem, const char *who);
extern void *RT_alloc_raw(int size, const char *who);

extern void *RT_alloc_raw_internal(const int minimum_num_elements, const int element_size, int &actual_num_elements, const char *who); // Use RT_alloc_raw2 or RT_alloc_raw instead.

// Same as calling RT_alloc_raw(minimum_size*sizeof(T), who), but also sets the actual number of elements available for the returned mem into actual_num_elements.
template<typename T>
static inline T *RT_alloc_raw2(const int minimum_num_elements, int &actual_num_elements, const char *who){
  return (T*)RT_alloc_raw_internal(minimum_num_elements, sizeof(T), actual_num_elements, who);
}

static inline void *RT_alloc_clean_raw(const int size, const char *who){
  void *ret = RT_alloc_raw(size, who);
  memset(ret, 0, size);
  return ret;
}

template<typename T>
static inline T *RT_alloc_clean_raw2(const int minimum_num_elements, int &actual_num_elements, const char *who){
  T *ret = RT_alloc_raw2<T>(minimum_num_elements, actual_num_elements, who);
  memset((void*)ret, 0, actual_num_elements*sizeof(T));
  return ret;
}

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
static inline RT_Mem<T> *RT_alloc(int num_elements, const char *who){
  return static_cast<RT_Mem<T>*>(RT_alloc_raw(sizeof(T)*num_elements, who));
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
