
#include <thread>

#include "nsmtracker.h"

#include "QueueStack.hpp"

namespace{
  struct Element {
    bool is_deletable;
    void *mem;
  };
}

static radium::Queue<Element, 8000> *g_queue;

void RT_memory_freer_init(void){

  g_queue = new radium::Queue<Element, 8000>;
  
  std::thread([](){

    THREADING_init_deleter_thread_type();
    
    while(true){
      Element element = g_queue->get();

      if (element.is_deletable) {

        radium::Deletable *deletable = static_cast<radium::Deletable*>(element.mem);
        
        THREADING_run_on_main_thread_async([deletable](void) {
          printf("RT_DELETE %p\n", deletable);
          delete deletable;
        });
        
      } else {
      
        printf("RT_FREED %p\n", element.mem);
      
        V_free(element.mem);
      }
    }
      
  }).detach();
}

bool RT_free(void *mem){
  Element element{false,mem};
  
  bool ret = g_queue->tryPut(element);

  R_ASSERT_NON_RELEASE(ret);

  return ret;
}

bool RT_free(radium::Deletable *mem){
  Element element{true,(void*)mem};

  printf("Queuing Deltable %p\n", mem);
  
  bool ret = g_queue->tryPut(element);

  R_ASSERT_NON_RELEASE(ret);

  return ret;
}
