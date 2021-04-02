#pragma once

#include "RT_memory_allocator_proc.h"

/*
  Both RT safe allocation/deallocation and multithreaded read and write access.
  RT_Alloc / RT_free does create a lot of fragmentation though, but for minor usage this should be excellent. Reference counting should be as quick as shared_ptr.
 */

namespace radium{

template<typename T> struct RT_Array{
  RT_Mem<T> *_data;
  int _size;
  
  RT_Array(const RT_Array &other)
    : _data(other._data)
    , _size(other._size)
  {
    RT_inc_ref(_data);
  }

  RT_Array& operator=(const RT_Array &other) = delete;
  
  /*
    Doesn't compile with clang:
  RT_Array& operator=(const RT_Array &other){
    this._data = other._data;
    this._size = other._size;
    RT_inc_ref(_data);
  }
  */
  
  RT_Array(int num_elements, const char *who)
    : _data(RT_alloc<T>(std::max(1, num_elements), who)) // Allocate at least one element to prevent crash if [] is used the wrong way.
    , _size(num_elements)
  {}

  ~RT_Array(){
    RT_free(_data, "~RT_Array()");
  }
  
  inline T &operator[](int i) const {
    if (i < 0) {
      R_ASSERT(false);
      return RT_data(_data)[0];
    } else if (i >= _size) {
      R_ASSERT(false);
      return RT_data(_data)[_size-1];
    } else {
      return RT_data(_data)[i];
    }
  }

  void inc_ref(void) {
    RT_inc_ref(_data);
  }
};

}
