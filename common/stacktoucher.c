
#include <stdlib.h>
#include <stdio.h>

// Should/must be compiled with -O0

#define LANGSPEC

#include "stacktoucher_proc.h"


void touch_stack(void){
  int stack_size = 1024*64;
  
  char hepp[stack_size];

  int i;
  
  for(i=0;i<stack_size;i++)
    hepp[i] = rand() % 128;
  
  int ret = 0;
  
  for(i=0;i<stack_size;i++)
    ret += hepp[i];

  printf("stack is hopefully touched enough: %d\n", ret);
}

