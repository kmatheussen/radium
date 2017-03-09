

#include "nsmtracker.h"

#include "Dynvec_proc.h"



bool DYNVEC_equal(dynvec_t *v1, dynvec_t *v2){

  if (v1->num_elements != v2->num_elements)
    return false;

  for(int i = 0 ; i<v1->num_elements ; i++)
    if (DYN_equal(v1->elements[i], v2->elements[i])==false)
      return false;

  return true;
}
