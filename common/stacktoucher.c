
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
  
  for(i=stack_size-1;i>=0;i--) {
    hepp[i] = rand() % 128;
    //usleep(2);
  }

  // TODO: I guess some compilers still might optimize away touching the stack. To be safe, we should have another source file with a function that takes an array as argument. Then this function can't ignore 'hepp'.
  
  int ret = 0;

  for(i=stack_size-1;i>=0;i--) {
    ret += hepp[i];
    //usleep(2);
  }

  printf("stack is hopefully touched enough: %d\n", ret);
}

