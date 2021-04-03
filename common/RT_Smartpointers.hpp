#pragma once

#include "RT_memory_allocator_proc.h"

/*
  Realtime safe and multithread safe scoped_ptr and shared_ptr.
  See test_RT_Smartpointers for example of usage.
 */

namespace radium{

// The template type T used by RT_scoped_ptr and RT_unique_ptr must be a subclass of RT_ref (this is also asserted during runtime in debug mode).
struct RT_Class{

  void *operator new(size_t size){
    return RT_alloc_raw(size, "RT_Class::operator new");
  }
  
  void operator delete(void *p){
    RT_free_raw(p, "RT_Class::operator delete");
  }
};


// Note: RT_scoped_ptr is not more efficient than RT_shared_ptr. The only two reasons for using RT_scoped_ptr instead of RT_shared_ptr are:
//   1. To make code clearer
//   2. To prevent accidentally sharing pointers that are not supposed to be shared, at compile time. (The copy constructors are explicitly deleted in RT_scoped_ptr.)
template<typename T> class RT_scoped_ptr{

  T *_data;

public:
  
  RT_scoped_ptr(const RT_scoped_ptr &other) = delete;

  RT_scoped_ptr& operator=(const RT_scoped_ptr &other) = delete;

  // Note: RT_scoped_ptr takes ownership of data here.
  RT_scoped_ptr(T *data)
    : _data(data)
  {
    R_ASSERT_NON_RELEASE(data != NULL);
    R_ASSERT_NON_RELEASE(dynamic_cast<RT_Class*>(data)==data);
  }
  
  ~RT_scoped_ptr(){
    delete _data;
  }

  T *operator->() const {
    return _data;
  }

  T *get(void) const {
    return _data;
  }
};

  
template<typename T> class RT_shared_ptr{

  T *_data;

public:
  
  RT_shared_ptr(const RT_shared_ptr &other)
    : _data(other._data)
  {
    RT_inc_ref_raw(_data);
  }

  RT_shared_ptr& operator=(const RT_shared_ptr &other){
    this._data = other._data;
    RT_inc_ref(_data);
  }

  // Note: RT_shared_ptr takes ownership of data here.
  RT_shared_ptr(T *data)
    : _data(data)
  {
    R_ASSERT_NON_RELEASE(data != NULL);
    R_ASSERT_NON_RELEASE(dynamic_cast<RT_Class*>(data)==data);
  }
  
  ~RT_shared_ptr(){
    delete _data;
  }

  T *operator->() const {
    return _data;
  }

  T *get(void) const {
    return _data;
  }
};
  
}

