
#include <thread>

#include "nsmtracker.h"

#include "QueueStack.hpp"



static radium::Queue<void*, 8000> g_queue;


void RT_memory_freer_init(void){

  std::thread([](){
    
    while(true){
      void *mem = g_queue.get();

      printf("RT_FREED %p\n", mem);
      
      V_free(mem);
    }
      
  }).detach();
}

bool RT_free(void *mem){
  bool ret = g_queue.tryPut(mem);

  R_ASSERT_NON_RELEASE(ret);

  return ret;
}
