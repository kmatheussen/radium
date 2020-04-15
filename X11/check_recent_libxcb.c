#include <stdlib.h>
#include <stdio.h>
#include <dlfcn.h>

int main(void){
  
  void *handle = dlopen("libxcb.so.1", RTLD_NOW|RTLD_LOCAL);
  
  if (handle==NULL){
    printf("Could not find libxcb\n");
    return -1;
  }

  if (dlsym(handle, "xcb_discard_reply64")==NULL){
    printf("No xcb_discard_reply64 in libxcb\n");
    return -2;
  }

  printf("libxcb is recent enough. We can use it\n");

  dlclose(handle);
  
  return 0;
}
