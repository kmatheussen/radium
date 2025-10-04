#ifndef _RADIUM_COMMON_ARRAY_HPP
#define _RADIUM_COMMON_ARRAY_HPP

#include "Vector.hpp"
#include "RT_memory_allocator_proc.h"

namespace radium{

#if 0 // never used

template <typename T>
struct Array{

private:
  
  T *_elements = NULL;  
  int _num_elements = 0;

public:
  
  Array(){
  }

  Array(int num_elements)
  {
    init(num_elements);
  }

  ~Array(){
    V_free(_elements);
  }

  void init(int num_elements){
    //R_ASSERT(_elements==NULL);
    R_ASSERT_RETURN_IF_FALSE(num_elements < 9999999);
    
    V_free(_elements);

    _num_elements = num_elements;

    _elements = (T*)V_calloc(num_elements, sizeof(T));
  }

  T operator[](int i) const {
    R_ASSERT_NON_RELEASE(i>=0 && i<_num_elements);
    if(i>=0 && i<_num_elements)
      return _elements[i];
    else
      return _elements[0];
  }

  void set(int i, T value){
    R_ASSERT_NON_RELEASE(i>=0 && i<_num_elements);
    if(i>=0 && i<_num_elements)
      _elements[i] = value;
  }

  int size(void) const {
    return _num_elements;
  }
  
  T *get_array(void) const {
    return _elements;
  }
  
  const T* begin() const {
    return &_elements[0];
  }

  // This function can be called in parallell with the other const functions (i.e. the non-mutating ones).
  const T* end() const {
    return &_elements[_num_elements];
  }

};

template <typename T>
struct ArrayAccessor{

private:
  
  const T *_elements = NULL;  
  int _num_elements = 0;

public:
  
  ArrayAccessor(const T *elements, int num_elements)
    : _elements(elements)
    , _num_elements(num_elements)
  {
  }

  ArrayAccessor(const radium::Array<T> &array)
    : ArrayAccessor(array.get_array(), array.size())
  {
  }

  T operator[](int i) const {
    R_ASSERT_NON_RELEASE(i>=0 && i<_num_elements);
    if(i>=0 && i<_num_elements)
      return _elements[i];
    else
      return _elements[0];
  }

  int size(void) const {
    return _num_elements;
  }
  
  const T *get_array(void) const {
    return _elements;
  }
  
  const T* begin() const {
    return &_elements[0];
  }

  // This function can be called in parallell with the other const functions (i.e. the non-mutating ones).
  const T* end() const {
    return &_elements[_num_elements];
  }
  
};

#endif // 0


// Dynamic 2D-array. Convenient when the compiler complains too much about variable length arrays.
//
template <typename T>
struct Array2D{

	static_assert(std::is_trivial<T>::value, "Currently only for trivial types, but non-trivial types should be trivial to support when/if needed.");
	
private:
  
	T **_elements;
	const int _num_elements1;
	const int _num_elements2;

public:
  
	Array2D(){
	}

	Array2D(int num_elements1, int num_elements2)
		: _num_elements1(num_elements1)
		, _num_elements2(num_elements2)
	{
		R_ASSERT_RETURN_IF_FALSE(_num_elements1 < 9999999);
		R_ASSERT_RETURN_IF_FALSE(_num_elements2 < 9999999);
		
		_elements = (T**)V_calloc(_num_elements1, sizeof(T*));

		for(int i = 0 ; i < _num_elements1 ; i++)
			_elements[i] = (T*)V_calloc(_num_elements2, sizeof(T));
	}
  
	~Array2D()
	{
		for(int i = 0 ; i < _num_elements1 ; i++)
			V_free(_elements[i]);
    
		V_free(_elements);
	}

	T *operator[](int i) const
	{
		R_ASSERT_NON_RELEASE(i>=0 && i<_num_elements1);
		return _elements[i];
	}

	void set(int i, T value)
	{
		R_ASSERT_NON_RELEASE(i>=0 && i<_num_elements1);
		_elements[i] = value;
	}

	int size(void)
	{
		return _num_elements1;
	}
  
	T **get_array(void){
		return _elements;
	}
  
	const T* begin() const {
		return &_elements[0];
	}

	const T* end() const {
		return &_elements[_num_elements1];
	}

};


  
} // namespace Radium

#endif
