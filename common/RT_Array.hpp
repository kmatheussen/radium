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
      return RT_data(_data)[0];
    } else {
      return RT_data(_data)[i];
    }
  }

  void inc_ref(void) {
    RT_inc_ref(_data);
  }
};


#if 0 // Makes little sense. Use the RT_ALLOC_STACK macro instead.

// Dynamic RT-array. Convenient when the compiler complains too much about variable length arrays.
//
template <typename T, int PREALLOCATED_SIZE = sizeof(float)*64*4> // (PREALLOCATED_SIZE is in bytes, not in number of elements)
struct RT_Array_maybe_stack final
{
	static_assert(std::is_trivial<T>::value, "Only for trivial types, but non-trivial types might be trivial to support when/if needed.");
	
private:

	T *_elements;
	
	char  _pre_allocated_memory[find_vector_preallocate_size<T>(PREALLOCATED_SIZE)] __attribute__((aligned(std::alignment_of<T>::value)));

#if !defined(RELEASE)
	const int _num_elements;
#endif
	
public:
  
	RT_Array_maybe_stack(){
	}

	RT_Array_maybe_stack(int num_elements)
#if !defined(RELEASE)
		: _num_elements(num_elements)
#endif
	{
		R_ASSERT_NON_RELEASE(num_elements < 1024); // increase if necessary.

		constexpr int num_preallocated_elements = sizeof(_pre_allocated_memory) / sizeof(T);

		static_assert(num_preallocated_elements==std::max(1, int(PREALLOCATED_SIZE / sizeof(T))), "?");

		if (num_preallocated_elements >= num_elements) [[likely]]
			_elements = (T*)_pre_allocated_memory;
		else
		{
			R_ASSERT_NON_RELEASE(false); // Don't want this to happen...
			_elements = (T*)RT_alloc_raw(sizeof(T)*num_elements, "RT_Array_maybe_stack");
		}
	}
  
	~RT_Array_maybe_stack()
	{
		if (_elements != (T*)_pre_allocated_memory) [[unlikely]]
			RT_free_raw(_elements, "~RT_Array_maybe_stack");
	}

	T &operator[](int i) const
	{
		R_ASSERT_NON_RELEASE(i>=0 && i<_num_elements);
		return _elements[i];
	}

	void set(int i, T value)
	{
		R_ASSERT_NON_RELEASE(i>=0 && i<_num_elements);
		_elements[i] = value;
	}

	/*
	int size(void)
	{
		return _num_elements;
	}
	*/
	
	T *data(void){
		return _elements;
	}

	/*
	const T* begin() const {
		return &_elements[0];
	}

	const T* end() const {
		return &_elements[_num_elements];
	}
	*/

};
#endif // 0

}
