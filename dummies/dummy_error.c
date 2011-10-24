
#include "../common/nsmtracker.h"
#include "../common/OS_error_proc.h"

#include <stdio.h>
#include <stdarg.h>


bool Error_init(void){return true;}

void RError(const char *fmt,...){
  va_list argp;
  
  va_start(argp,fmt);
  /*	vfprintf(stderr,fmt,argp); */
  vfprintf(stderr,fmt,argp);
  va_end(argp);
}

void Error_uninit(void){}
