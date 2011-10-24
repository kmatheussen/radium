

#include "../common/nsmtracker.h"

void  OS_tfree(void *mem){
  free(mem);
}

void *OS_getmem(size_t size){
  return calloc(1,size);
}
