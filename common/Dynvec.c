

#include "nsmtracker.h"

#include "Dynvec_proc.h"

static const dynvec_t g_empty_dynvec_dynvec = {0};
const dyn_t g_empty_dynvec = {
 .type = ARRAY_TYPE,
 .array = (dynvec_t*)&g_empty_dynvec_dynvec
};

const dyn_t g_uninitialized_dyn = {0};

bool DYNVEC_equal(dynvec_t *v1, dynvec_t *v2){

  if (v1->num_elements != v2->num_elements)
    return false;

  for(int i = 0 ; i<v1->num_elements ; i++)
    if (DYN_equal(v1->elements[i], v2->elements[i])==false)
      return false;

  return true;
}
