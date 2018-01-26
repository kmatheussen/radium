
#include <stdlib.h>
#include <stdio.h>

// Should/must be compiled with -O0

#ifdef __OPTIMIZE__
#error "This file should/must not be compiled with optimization"
#endif

#define LANGSPEC

#include "stacktoucher_proc.h"

static void __attribute__ ((noinline)) fill(int size, char *hepp) {
  
  for(int i=size-1;i>=0;i--) {
    hepp[i] = rand() % 128;
    __asm__("");
    //usleep(2);
  }
}

void touch_stack(void){
  int stack_size = 1024*64;
  
  char hepp[stack_size];

  fill(stack_size, hepp);

  int ret = 0;

  for(int i=stack_size-1;i>=0;i--) {
    ret += hepp[i];
    __asm__("");
    //usleep(2);
  }

  printf("stack is hopefully touched enough: %d\n", ret);
}

