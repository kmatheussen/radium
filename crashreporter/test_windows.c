#include <stdio.h>

#include <windows.h>

#include "crashreporter_proc.h"

// i686-w64-mingw32.static-gcc -O2 -Wall test2.c -lbfd -liberty -limagehlp -lz -g

double TIME_get_ms(void){
  return 0;
}

void CRASHREPORTER_send_message(const char *additional_information, const char **messages, int num_messages, enum Crash_Type crash_type, double time){
  printf("CR_send_message_called with %d lines of info about -%s-\n",num_messages,additional_information);
  int i;
  for(i=0;i<num_messages;i++)
    printf("%d: %s\n",i,messages[i]);
}


#define FOR_WINDOWS 1
#define PACKAGE
#include "crashreporter_windows.c"


static void
foo()
{
  int *f=NULL;
  *f = 0;
}

static void
bar()
{
  foo();
}

int
main()
{
#if 1
  printf("STASRTUP PU\n");
  //LoadLibraryA("backtrace.dll");
  CRASHREPORTER_windows_init();
  
  bar();
  
  CRASHREPORTER_windows_close();
#else
  printf("gakkgakk\n");
#endif
  return 0;
}

