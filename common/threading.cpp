
#ifndef TEST_THREADING
  #include "nsmtracker.h"
  #include "threading.h"
#endif


enum ThreadType{
  OTHER_THREAD,
  MAIN_THREAD,
  PLAYER_THREAD
};

__thread ThreadType thread_type;



void THREADING_init_main_thread_type(void) {
  thread_type = MAIN_THREAD;
}

void THREADING_init_player_thread_type(void) {
  thread_type = PLAYER_THREAD;
}

bool THREADING_is_main_thread(void){
  return thread_type==MAIN_THREAD;
}

bool THREADING_is_player_thread(void){
  return thread_type==PLAYER_THREAD;
}


#ifdef TEST_THREADING

#if 0
g++ threading.cpp -Wall -lpthread
#endif


#include <stdio.h>
#include <pthread.h>
#include <unistd.h>
#include <assert.h>

static pthread_t thread;

static void *player_thread_func(void* arg){

  THREADING_init_player_thread_type();

  assert(!THREADING_is_main_thread());
  assert(THREADING_is_player_thread());

  sleep(2);

  return &thread;
}

int main(){

  THREADING_init_main_thread_type();
  
  assert(THREADING_is_main_thread());
  assert(!THREADING_is_player_thread());

  pthread_create(&thread, NULL, player_thread_func, NULL);

  sleep(1);

  assert(THREADING_is_main_thread());
  assert(!THREADING_is_player_thread());

  void *retval;
  pthread_join(thread, &retval);
  assert(retval==&thread);
  sleep(3);

  assert(THREADING_is_main_thread());
  assert(!THREADING_is_player_thread());

  printf("\n\n\n == Threading test success == \n");
  return 0;
}

#endif
