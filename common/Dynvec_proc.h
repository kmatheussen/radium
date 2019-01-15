
#if !defined(_RADIUM_COMMON_DYNVEC_PROC_H)
#define _RADIUM_COMMON_DYNVEC_PROC_H

extern void DYN_save(disk_t *file, const dyn_t dyn);
extern dyn_t DYN_load(disk_t *file, bool *success);

extern LANGSPEC bool DYNVEC_equal(dynvec_t *v1, dynvec_t *v2);

extern LANGSPEC void DYNVEC_save(disk_t *file, const dynvec_t dynvec);
extern LANGSPEC dynvec_t DYNVEC_load(disk_t *file, bool *success);

static inline void DYNVEC_ensure_space_for_one_more_element(dynvec_t *v){
  const int num_elements = v->num_elements;
  
  if(num_elements==v->num_elements_allocated){
    if(num_elements==0){
#ifdef TEST_VECTOR
      const int num_init = 2;
#else
      const int num_init = 8;
#endif
      v->num_elements_allocated = num_init;
      v->elements = (dyn_t*)talloc(num_init*(int)sizeof(dyn_t));
    }else{
      const int num_elements_allocated = num_elements * 2;
      v->num_elements_allocated = num_elements_allocated;
      v->elements = (dyn_t*)talloc_realloc(v->elements,num_elements_allocated*(int)sizeof(dyn_t));
    }
  }
  
}

// 'v' is a pointer since a) it's called from C, and b) changing v.num_elements wouldn't have any effect.
static inline int DYNVEC_push_back(dynvec_t *v, const dyn_t element){
#if 0 //ifndef RELEASE
  R_ASSERT(element!=NULL);
#endif

  DYNVEC_ensure_space_for_one_more_element(v);
  
  const int num_elements = v->num_elements;

  v->elements[num_elements] = element;
  v->num_elements = num_elements+1;
  
  return num_elements;
}

#ifdef __cplusplus

static inline void DYNVEC_light_clean(dynvec_t &v){
  v.num_elements = 0;
}

static inline int DYNVEC_push_back(dynvec_t &v, const dyn_t element){
  return DYNVEC_push_back(&v, element);
}

static inline const dyn_t* begin(const dynvec_t &v) {
  return &v.elements[0];
}

static inline const dyn_t* end(const dynvec_t &v) {
  return &v.elements[v.num_elements];
}

static inline const dyn_t* begin(const dynvec_t *v) {
  return &v->elements[0];
}

static inline const dyn_t* end(const dynvec_t *v) {
  return &v->elements[v->num_elements];
}


#endif


#endif
