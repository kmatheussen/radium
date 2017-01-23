
namespace radium {

template <typename Event> struct PriorityQueue {

private:

  int _queue_size = 0;
  int _max_queue_size;

  Event _event0 = {};

  Event **_queue;

public:

  PriorityQueue(int max_queue_size, double a_lower_priority_than_we_will_ever_add = -10000)
    : _max_queue_size(max_queue_size + 2)
  {
    _queue = (Event**)V_calloc(sizeof(Event*), _max_queue_size);
    _event0.priority = a_lower_priority_than_we_will_ever_add;
    _queue[0] = &_event0;
  }

  ~PriorityQueue(){
    V_free(_queue);
  }

  bool add(Event *event){

    if(_queue_size > _max_queue_size-2)
      return false;
    
    _queue_size++;
    
    Event **queue = _queue;
    
    int i = _queue_size;
    int new_i = i >> 1;

    auto priority = event->priority;

    while(priority < queue[new_i]->priority){
      queue[i] = queue[new_i];
      i = new_i;
      new_i = new_i >> 1;
    }
  
    queue[i] = event;

    return true;
  }

  void remove_first_event(void){
    R_ASSERT_RETURN_IF_FALSE(_queue_size > 0);

    Event **queue = _queue;
    
    Event *last = queue[_queue_size];
    auto last_priority = last->priority;
    
    int i = 1;
    int child = 2;
    
    _queue_size--;
    
    int queue_size = _queue_size;
    
    while(child <= queue_size){
      if(child != queue_size
         && queue[child+1]->priority < queue[child]->priority)
        child++;
      
      if(last_priority <= queue[child]->priority)
        break;
      
      queue[i] = queue[child];
      i        = child;
      child    = child * 2;
    }
    queue[i] = last;
    
    queue[queue_size+1] = NULL;
  }

  Event *get_first_event(void){
    if (_queue_size==0)
      return NULL;

    return _queue[1];
  }

  int size(void) const {
    return _queue_size;
  }
};

}

