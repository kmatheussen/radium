
// Must always be included first.

#pragma clang diagnostic push
#pragma clang diagnostic ignored "-Wreserved-id-macro"
#pragma clang diagnostic ignored "-Wpadded"
#pragma clang diagnostic ignored "-Wc++98-compat-pedantic"


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
  
#pragma clang diagnostic pop
  
#define RADIUM_PYTHON_IS_DEFINED 1

#ifdef __cplusplus
}
#endif

  
#ifdef FOR_WINDOWS
#  ifndef _POSIX_C_SOURCE
#    define _POSIX_C_SOURCE 1
#  endif
#endif
