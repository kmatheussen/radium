

#include <stdlib.h>
#include <pthread.h>

pthread_t athread={0};

void *athreadcode(void *arg){
  usleep(2000000);
  return NULL;
}

int dasmain(void){
  pthread_create(&athread,NULL,athreadcode,NULL);
  system("/usr/bin/givertcap");
  pthread_join(athread,NULL);
  return 0;
}

