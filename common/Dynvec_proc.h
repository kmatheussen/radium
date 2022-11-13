
#if !defined(_RADIUM_COMMON_DYNVEC_PROC_H)
#define _RADIUM_COMMON_DYNVEC_PROC_H


#ifdef __cplusplus
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


extern LANGSPEC void DYN_save(disk_t *file, const dyn_t dyn);
extern LANGSPEC dyn_t DYN_load(disk_t *file, bool *success);

extern LANGSPEC const wchar_t *DYN_to_string(const dyn_t dyn);
                               
static inline dyn_t DYN_copy(const dyn_t a);

static inline dynvec_t DYNVEC_create(int num_elements){
  dynvec_t ret;
  
  ret.num_elements_allocated = num_elements;
  ret.num_elements = num_elements;
  
  ret.elements = num_elements==0 ? NULL : (dyn_t*)talloc(ret.num_elements_allocated*(int)sizeof(dyn_t));
  
  return ret;
}

static inline dynvec_t DYNVEC_copy(const dynvec_t *v){
  dynvec_t ret = DYNVEC_create(v->num_elements_allocated);
  
  for(int i=0;i<ret.num_elements;i++)
    ret.elements[i] = DYN_copy(v->elements[i]);

  return ret;
}

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

static inline void DYNVEC_light_clean(dynvec_t *v){
  v->num_elements = 0;
}

#ifdef __cplusplus

static inline void DYNVEC_light_clean(dynvec_t &v){
  v.num_elements = 0;
}

static inline void DYNVEC_remove_at_and_keep_order(dynvec_t &v, int pos){
  R_ASSERT_RETURN_IF_FALSE(pos < v.num_elements);
  R_ASSERT_RETURN_IF_FALSE(pos >= 0);
  
  v.num_elements--;

  for(int i=pos ; i < v.num_elements ; i++)    
    v.elements[i] = v.elements[i+1];
  
  v.elements[v.num_elements] = g_uninitialized_dyn; // help gc
}

static inline void DYNVEC_remove_at(dynvec_t &v, int pos){
  R_ASSERT_RETURN_IF_FALSE(pos < v.num_elements);
  R_ASSERT_RETURN_IF_FALSE(pos >= 0);
  
  v.num_elements--;

  if (pos < v.num_elements)
    v.elements[pos] = v.elements[v.num_elements];
  
  v.elements[v.num_elements] = g_uninitialized_dyn; // help gc
}

static inline bool DYN_equal(const dyn_t a1, const dyn_t a2);
static inline int DYNVEC_find_pos(dynvec_t &v, const dyn_t &element){
  for(int i=0;i<v.num_elements;i++)
    if(DYN_equal(v.elements[i], element))
      return i;

  return -1;
}

static inline bool DYNVEC_has_element(dynvec_t &v, const dyn_t &element){
  return DYNVEC_find_pos(v, element) >= 0;
}

static inline bool DYNVEC_remove_element_and_keep_order(dynvec_t &v, const dyn_t &element){
  int pos = DYNVEC_find_pos(v, element);
  if (pos < 0)
    return false;
  
  DYNVEC_remove_at_and_keep_order(v, pos);
  return true;
}

static inline bool DYNVEC_remove_element(dynvec_t &v, const dyn_t &element){
  int pos = DYNVEC_find_pos(v, element);
  if (pos < 0)
    return false;
  
  DYNVEC_remove_at(v, pos);
  return true;
}

static inline int DYNVEC_push_back(dynvec_t &v, const dyn_t element){
  return DYNVEC_push_back(&v, element);
}

static inline void DYNVEC_set(dynvec_t &v, int pos, const dyn_t element){
  R_ASSERT_RETURN_IF_FALSE(pos>=0 && pos < v.num_elements);

  v.elements[pos] = element;
}


#endif


#endif
