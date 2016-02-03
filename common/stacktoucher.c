
#include <stdlib.h>
#include <stdio.h>

// Should/must be compiled with -O0

#ifdef __OPTIMIZE__
#error "This file should/must not be compiled with optimization"
#endif

#define LANGSPEC

#include "stacktoucher_proc.h"


void touch_stack(void){
  int stack_size = 1024*64;
  
  char hepp[stack_size];

  int i;
  
  for(i=0;i<stack_size;i++) {
    hepp[i] = rand() % 128;
    //usleep(2);
  }
  
  int ret = 0;
  
  for(i=0;i<stack_size;i++){
    ret += hepp[i];
    //usleep(2);
  }

  printf("stack is hopefully touched enough: %d\n", ret);
}

