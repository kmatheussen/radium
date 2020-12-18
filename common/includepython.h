
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

#include <Python.h>

#define RADIUM_PYTHON_IS_DEFINED 1
  
#ifdef __cplusplus
}
#endif

  
#ifdef FOR_WINDOWS
#  ifndef _POSIX_C_SOURCE
#    define _POSIX_C_SOURCE 1
#  endif
#endif
