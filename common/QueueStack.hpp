/* Copyright 2015-2016 Kjetil S. Matheussen

This program is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public License
as published by the Free Software Foundation; either version 2
of the License, or (at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA. */


#ifndef _RADIUM_COMMON_QUEUES_HPP
#define _RADIUM_COMMON_QUEUES_HPP


#include <boost/version.hpp>
#if (BOOST_VERSION < 100000) || ((BOOST_VERSION / 100 % 1000) < 58)
  #error "Boost too old. Need at least 1.58.\n Quick fix: cd $HOME ; wget http://downloads.sourceforge.net/project/boost/boost/1.63.0/boost_1_63_0.tar.bz2 ; tar xvjf boost_1_63_0.tar.bz2 (that's it!)"
#endif
#include <boost/lockfree/queue.hpp>

#include "Semaphores.hpp"
#include "spinlock.h"



// Lockless multithread read / multithread write queue

namespace radium {

  /*
template <typename T> struct QueueStorage{
};
  */
template <typename T> struct BaseQueueStack{

private:

  DEFINE_ATOMIC(bool, buzy_get_is_enabled) = false;

  Semaphore buzyget_ready;

  Semaphore ready;
  
  //boost::lockfree::queue< T > queue2;


  virtual bool pop(T &ret) = 0;
  virtual bool push(T &t) = 0;
  
public:

  // sets success to false if failed, true if succeeded. Return value is undefined if "success" is false.
  T tryGet(bool &success){
    T ret = 0;
    
    if (ready.tryWait()) {
    
      R_ASSERT(pop(ret));

      success = true;

    } else {

      success = false;

    }
    
    return ret;
  }

  
private:  // Rather not expose this messy (and unsafe) API unless it's needed.
 
  // MUST call either cancelGet() or get_withoutWaiting() afterwards.
  void wait(void){
    ready.wait();
  }

  // Must NOT be called without first calling wait()
  T get_withoutWaiting(){
    T ret;
    memset(&ret, 0, sizeof(T)); // why?
    
    R_ASSERT(pop(ret));

    return ret;
  }

  // Must NOT be called without first calling wait().
  void cancelGet(void){
    ready.signal();
  }

public:
  
  // Waits until available. Never returns NULL. Same as calling wait() and get_withoutWaiting() in a row
  T get(void){
    wait();
    return get_withoutWaiting();
  }
  
  int getNumBuzyGetWaiters(void){
    return buzyget_ready.numWaiters();
  }
  
  void waitUntilBuzyGetIsEnabled(void){
    buzyget_ready.wait();
  }

  bool buzyGetEnabled(void){
    return ATOMIC_GET(buzy_get_is_enabled);
  }

  void enableBuzyGet(void){
    R_ASSERT_NON_RELEASE(!buzyGetEnabled());
    ATOMIC_SET(buzy_get_is_enabled, true);
    buzyget_ready.signalAll();
  }

  // Can be called while other threads are buzy-getting. If that happens, the other threads will call get() instead.
  void disableBuzyGet(void){
    R_ASSERT_NON_RELEASE(buzyGetEnabled());
    ATOMIC_SET(buzy_get_is_enabled, false);
  }

  // Same as get, but instead of calling semaphore.wait, we call seamphore.tryWait again and again until it's there. (i.e. we are spinning)
  // Note that enableBuzyGet() must be called first. buzyGet is disabled initially.
  T buzyGetIfEnabled(void){
    for(;;){
      bool gotit;
      T ret = tryGet(gotit);
      if (gotit)
        return ret;
      else if (!buzyGetEnabled())
        return get();
    }
  }
                 
  void putWithoutSignal(T t) {
    while (!push(t));
  }

  void signal(int num){
    ready.signal(num);
  }
  
  // returns false if queue was full
  bool tryPut(T t){
    if (push(t)) {
      ready.signal();
      return true;
    }

    return false;
  }

  // If queue is full, buzy-waits until space is available. Same as calling while(!tryPut(t));
  // (it's simple to create a put which waits on a semaphore instead of buzy-looping, but that functionality hasn't been needed in Radium so far)
  void put(T t){
    while(!tryPut(t));
  }

  int size(void){
    return ready.numSignallers();
  }
};

// RT safe.
template <typename T, int SIZE> class Queue : public BaseQueueStack<T>{

  using queuetype = boost::lockfree::queue< T , boost::lockfree::capacity<SIZE> >;
  queuetype queue;

  bool pop(T &ret) override{
    return queue.pop(ret);
  }

  bool push(T &t) override{
    return queue.push(t);
  }

};

// RT safe, but all threads must run with the same priority to avoid priority inversion
// 'T' must be a pointer and have a "T *next" field.
template <typename T> class LinkedListStack : public BaseQueueStack<T>{

  radium::Spinlock _lock;

  T _root = NULL;

  //int size=0;

  bool pop(T &ret) override{
    radium::ScopedSpinlock lock(_lock);
    if (_root==NULL)
      return false;

    ret = _root;
    _root = _root->next;

    //size--;   printf("        << Pop %p. Size: %d\n", ret, size);

    return true;
  }

  bool push(T &t) override{
    radium::ScopedSpinlock lock(_lock);

    //size++;   printf("        >> Push %p. Size: %d\n", t, size);

    t->next = _root;
    _root = t;

    return true;
  }

};

// RT safe, but all threads must run with the same priority to avoid priority inversion.
template <typename T, int SIZE> class VectorStack : public BaseQueueStack<T>{

  radium::Spinlock _lock;
  int _pos = 0;

  T _elements[SIZE];

  //int size=0;

  bool pop(T &ret) override{
    radium::ScopedSpinlock lock(_lock);

    if (_pos==0)
      return false;

    _pos--;

    ret = _elements[_pos];

    //size--;   printf("        << Pop %p. Size: %d\n", ret, size);

    return true;
  }

  bool push(T &t) override{
    radium::ScopedSpinlock lock(_lock);

    //size++;   printf("        >> Push %p. Size: %d\n", t, size);

    if (_pos==SIZE)
      return false;

    _elements[_pos] = t;

    _pos++;

    return true;
  }

};

static inline int get_curr_cpu(void){
#if defined(FOR_WINDOWS)
  GetCurrentProcessorNumber();
#elif defined(FOR_LINUX)
  return sched_getcpu();
#elif defined(FOR_MACOSX)
  return 0; // Can't find equivalent function on osx.
#endif
}

// 'pop' returns the first element that was runing on the same cpunum as the current thread when being pushed.
// RT safe, but all access must have realtime priority to avoid priority inversion
// 'T' must be a pointer and have a "T *next" field.
// Implementation is not finished.
template <typename T> class SameCpuQueue : public BaseQueueStack<T>{

  // hwloc: https://www.open-mpi.org/projects/hwloc/ (probably faster to implement a library from scratch than it would take to learn that api though)

  #define num_cpus 4 // Hardcoded to match my laptop. Definitely not always correct.
  T _roots[num_cpus] = {}; //NULL;

  // Hardcoded to match my laptop. Use hwloc to generate table.
  const int cpumap[4][3] = {{1,2,3},
                            {0,3,2},
                            {3,0,1},
                            {2,1,0}};
  /*
  const int cpumap[4][3] = {{1,2,3},
                            {0,2,3},
                            {0,1,3},
                            {0,1,2}};
  */
  //radium::Spinlock _locks[num_cpus];
  radium::Spinlock _lock;

  //int size=0;

  bool pop(T &ret, int cpunum, int current_cpunum){
    //radium::ScopedSpinlock locks(_lock[cpunum]);

    if (_roots[cpunum]==NULL)
      return false;

    ret = _roots[cpunum];
    _roots[cpunum] = _roots[cpunum]->next;

    ret->cpunum = current_cpunum;

    return true;
  }

  bool pop(T &ret) override{

    //size--;   printf("        << Pop %p. Size: %d\n", ret, size);

    int current_cpunum = get_curr_cpu();

    if(current_cpunum < 0 || current_cpunum>3){
      printf("========================Illegal cpu num: %d\n", current_cpunum);
      abort();
    }

    radium::ScopedSpinlock lock(_lock);

    if (pop(ret, current_cpunum, current_cpunum))
      return true;
      
    for(int i=0;i<3;i++){
      int old_cpunum = cpumap[current_cpunum][i];
      if (pop(ret, old_cpunum, current_cpunum)){
        //if (i > 0) printf("....%s switched CPU %d -> %d\n", ret->_plugin->patch->name, old_cpunum, current_cpunum);
        return true;
      }
    }

    printf("========================No ret\n");
    abort();

    return false;
  }

  bool push(T &t) override{

    //size++;   printf("        >> Push %p. Size: %d\n", t, size);

    int cpunum = t->cpunum;

    //radium::ScopedSpinlock lock(_locks[cpunum]);
    radium::ScopedSpinlock lock(_lock);

    t->next = _roots[cpunum];
    _roots[cpunum] = t;

    return true;
  }

  #undef num_cpus
};

 
#if 0
// This buffer does not keep order.
template <typename T> struct ExpandableBuffer{
  const int MAX_QUEUE_SIZE = (65536-2);       // boost::lockfree::queue limit
  const int max_queues = 1024;
  
  using queuetype = boost::lockfree::queue< T , boost::lockfree::capacity<MAX_QUEUE_SIZE> >;

  
  DEFINE_ATOMIC(int, num_queues) = 0;
  DEFINE_ATOMIC(queuetype*, queues);

  ExpandableBuffer(){
    queues = calloc(max_queues, sizeof(queuetype*));
    add_queue();
  }

  ~ExpandableBuffer(){
    for(int i = 0 ; i < max_queues ; i++)
      free(ATOMIC_ARRAY_GET(queues, i));
    
    free(queues);
  }
  
  bool push(T &t){
    for(int i = 0 ; i < max_queues ; i++){
      queuetype *queue = ATOMIC_ARRAY_GET(queues, i);
      if (queue==NULL)
        return false;

      if (queue.bounded_push(t))
        return true;
    }

    return false;
  }

private:
  
  void add_queue(void){
    queuetype *queue = new queuetype;
    
    int queue_num = ATOMIC_GET(num_queues); //ATOMIC_INC_RETURN_OLD(num_queues);

    for(int i=0;i<max_queues;i++){
      bool did_set = ATOMIC_COMPARE_AND_SET_POINTER_ARRAY(queues, i, NULL, queue);
      if (did_set)
        break;
    }
  }
  
};
#endif
  
  
template <typename T>
struct DispatchQueueElement{
  int type;
  T *data;
};

template <typename T, int SIZE>
struct DispatchQueue : public Queue<DispatchQueueElement<T>, SIZE> {
  
};


template <typename T>
struct SyncQueue{

private:
  Semaphore T1_ready;
  Semaphore T2_ready;
  bool has_data;
  T data;

  void T2_wait(void){
    T2_ready.wait();
  }

  void T2_has_picked_up_data(void){
    data = 0;
    has_data = false;
    T1_ready.signal();
  }
  
  T T2_get_withoutWaiting(){
    R_ASSERT(has_data);
    
    T ret = data;
    T2_has_picked_up_data();
    
    return ret;
  }


public:

  SyncQueue()
    :has_data(false)
  {
  }
  
  T T2_tryGet(bool &gotit){
    if (T2_ready.tryWait()) {
      gotit = true;
      
      return T2_get_withoutWaiting();
            
    } else {

      gotit = false;

      return 0;
    }
  }
  
  void T2_get(T t){
    T2_wait();
    return T2_get_withoutWaiting();
  }

  void T1_put(T t){
    R_ASSERT(!has_data);

    data = t;
    has_data = true;

    T2_ready.signal();
  }

  void T1_wait_for_T2_to_pick_up(void){
    T1_ready.wait();
    R_ASSERT(!has_data);
  }
  
};

  
}

  
#endif
