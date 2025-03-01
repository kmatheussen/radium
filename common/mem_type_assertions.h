#pragma once

#if !defined(RELEASE) && defined(__cplusplus)

#include <type_traits>
#include <typeinfo>

#include <string.h>

#include <functional>


// Copied from https://stackoverflow.com/questions/17946436/compile-time-warning-about-memset-on-non-plain-old-data
template <typename T>
static inline void* safe_memset(T* ptr, int value, size_t num) {
	static_assert(
#if defined(QACCESSIBLE_H)
		std::is_same<T,QAccessible::State>::value ||
#endif
		std::is_same<T,void>::value ||
	        std::is_trivial<T>::value,
		"memset on non-trivial type");
	return memset(ptr, value, num);
}


#define memset(Ptr, Value, Num) safe_memset(Ptr, Value, Num)


template <typename T>
static inline void safe_free(T* ptr) {
	static_assert(std::is_same<T,void>::value || std::is_trivial<T>::value, "free on non-trivial type");
	free(ptr);
}

#define free(Ptr) safe_free(Ptr)


template <typename T1, typename T2>
static inline void *safe_memcpy(T1* dest, T2 *src, size_t n)
{
	static_assert(std::is_same<T1,void>::value || std::is_trivial<T1>::value, "memcpy on non-trivial type");
	static_assert(std::is_same<typename std::remove_const<T2>::type,void>::value || std::is_trivial<T2>::value, "memcpy on non-trivial type");
	return memcpy(dest, src, n);
}

#define memcpy(Dest, Src, N) safe_memcpy(Dest, Src, N)


template <typename T1, typename T2>
static inline void *safe_memmove(T1* dest, T2 *src, size_t n)
{
	static_assert(std::is_same<T1,void>::value || std::is_trivial<T1>::value, "memmove on non-trivial type");
	static_assert(std::is_same<T2,void>::value || std::is_trivial<T2>::value, "memmove on non-trivial type");
	return memmove(dest, src, n);
}

#define memmove(Dest, Src, N) safe_memmove(Dest, Src, N)


namespace std
{
template <typename T>
static inline void *safe_memset(T* ptr, int value, size_t num)
{
	return ::safe_memset(ptr, value, num);
}
template <typename T>
static inline void safe_free(T* ptr)
{
	::safe_free(ptr);
}
template <typename T1, typename T2>
static inline void* safe_memcpy(T1* dest, T2 *src, size_t n)
{
	return ::safe_memcpy(dest, src, n);
}
template <typename T1, typename T2>
static inline void* safe_memmove(T1* dest, T2 *src, size_t n)
{
	return ::safe_memmove(dest, src, n);
}
} // namespace std.



namespace
{
template <typename A1>
class AllocTypeCheckerHelperHack1
{
private:
	std::function<void*(A1)> _alloc_func;
	A1 _a1;

public:
	// Found this trick here: https://stackoverflow.com/questions/26081814/can-c-templates-infer-return-type
	template< class Return_type >
	operator Return_type () const
	{
		using NonPointerReturnType = typename std::remove_pointer<Return_type>::type;
		static_assert(std::is_same<NonPointerReturnType, void>::value || std::is_trivial<NonPointerReturnType>::value, "malloc/etc. used to create object with non-trivial type");
		return (Return_type)_alloc_func(_a1);
	}
	
	AllocTypeCheckerHelperHack1(std::function<void*(A1)> alloc_func, A1 a1)
		: _alloc_func(alloc_func)
		, _a1(a1)
	{
	}
};

template <typename A1, typename A2>
class AllocTypeCheckerHelperHack2
{
private:
	std::function<void*(A1,A2)> _alloc_func;
	A1 _a1;
	A2 _a2;

public:
	// Found this trick here: https://stackoverflow.com/questions/26081814/can-c-templates-infer-return-type
	template< class Return_type >
	operator Return_type () const
	{
		using NonPointerReturnType = typename std::remove_pointer<Return_type>::type;
		static_assert(std::is_same<NonPointerReturnType, void>::value || std::is_trivial<NonPointerReturnType>::value, "malloc/etc. used to create object with non-trivial type");
		return (Return_type)_alloc_func(_a1, _a2);
	}
	
	AllocTypeCheckerHelperHack2(std::function<void*(A1,A2)> alloc_func, A1 a1, A2 a2)
		: _alloc_func(alloc_func)
		, _a1(a1)
		, _a2(a2)
	{
	}
};

template <typename A1, typename A2, typename A3>
class AllocTypeCheckerHelperHack3
{
private:
	std::function<void*(A1,A2,A3)> _alloc_func;
	A1 _a1;
	A2 _a2;
	A3 _a3;

public:
	// Found this trick here: https://stackoverflow.com/questions/26081814/can-c-templates-infer-return-type
	template< class Return_type >
	operator Return_type () const
	{
		using NonPointerReturnType = typename std::remove_pointer<Return_type>::type;
		static_assert(std::is_same<NonPointerReturnType, void>::value || std::is_trivial<NonPointerReturnType>::value, "malloc/etc. used to create object with non-trivial type");
		return (Return_type)_alloc_func(_a1, _a2, _a3);
	}
	
	AllocTypeCheckerHelperHack3(std::function<void*(A1,A2,A3)> alloc_func, A1 a1, A2 a2, A3 a3)
		: _alloc_func(alloc_func)
		, _a1(a1)
		, _a2(a2)
		, _a3(a3)
	{
	}
};

template <typename A1, typename A2, typename A3, typename A4>
class AllocTypeCheckerHelperHack4
{
private:
	std::function<void*(A1,A2,A3,A4)> _alloc_func;
	A1 _a1;
	A2 _a2;
	A3 _a3;
	A4 _a4;

public:
	// Found this trick here: https://stackoverflow.com/questions/26081814/can-c-templates-infer-return-type
	template< class Return_type >
	operator Return_type () const
	{
		using NonPointerReturnType = typename std::remove_pointer<Return_type>::type;
		static_assert(std::is_same<NonPointerReturnType, void>::value || std::is_trivial<NonPointerReturnType>::value, "malloc/etc. used to create object with non-trivial type");
		return (Return_type)_alloc_func(_a1, _a2, _a3, _a4);
	}
	
	AllocTypeCheckerHelperHack4(std::function<void*(A1,A2,A3,A4)> alloc_func, A1 a1, A2 a2, A3 a3, A4 a4)
		: _alloc_func(alloc_func)
		, _a1(a1)
		, _a2(a2)
		, _a3(a3)
		, _a4(a4)
	{
	}
};
}

#define malloc(Size) AllocTypeCheckerHelperHack1<size_t>(malloc, Size)

#define calloc(A,B) AllocTypeCheckerHelperHack2<size_t, size_t>(calloc, A, B)

/*
template <typename T>
static inline void safe_fwMem_Free(T* ptr) {
	static_assert(std::is_same<T,void>::value || std::is_trivial<T>::value, "fwMem_Free on non-trivial type");
	fwMem_Free(ptr);
}

#define fwMem_Free(Ptr) safe_fwMem_Free(Ptr)
*/


#endif // !defined(RELEASE) && defined(__cplusplus)
