
// Must always be included first.

#ifdef _POSIX_C_SOURCE
#  undef _POSIX_C_SOURCE
#endif

#ifdef FOR_WINDOWS
#ifdef _WIN64
#  ifndef MS_WIN64
#    define MS_WIN64 1
#  endif
#endif
#endif

#ifdef __cplusplus
extern "C" {
#endif

#ifdef __cplusplus
#  pragma GCC diagnostic push
#  pragma GCC diagnostic ignored "-Wregister"
#endif // __cplusplus
	
#include <Python.h>

#ifdef __cplusplus
#  pragma GCC pop
#endif
	
#define RADIUM_PYTHON_IS_DEFINED 1
  
#ifdef __cplusplus
}
#endif

#if !defined(PY_MAJOR_VERSION)
#  error "PY_MAJOR_VERSION not defined..."
#endif

#if PY_MAJOR_VERSION != 2 || PY_MINOR_VERSION != 7
#  define STRINGIFY_HELPER(x) #x
#  define STRINGIFY(x) STRINGIFY_HELPER(x)
#  pragma message("Wrong python version. Expected 2.7 got " STRINGIFY(PY_MAJOR_VERSION) "." STRINGIFY(PY_MINOR_VERSION))
#  error "Wrong python version."
#endif

#ifdef FOR_WINDOWS
#  ifndef _POSIX_C_SOURCE
#    define _POSIX_C_SOURCE 1
#  endif
#endif
