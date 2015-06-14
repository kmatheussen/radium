#ifdef FOR_WINDOWS
#  ifdef _POSIX_C_SOURCE
#    undef _POSIX_C_SOURCE
#  endif
#ifdef _WIN64
#  ifndef MS_WIN64
#    define MS_WIN64 1
#  endif
#endif
#endif

#include <Python.h>

#ifdef FOR_WINDOWS
#  ifndef _POSIX_C_SOURCE
#    define _POSIX_C_SOURCE 1
#  endif
#endif
