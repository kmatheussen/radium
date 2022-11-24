
#ifndef RADIUM_COMMON_API_PROC_H
#define RADIUM_COMMON_API_PROC_H

#ifdef __cplusplus
extern "C" {
#endif

#ifndef RADIUM_PYTHON_IS_DEFINED
#ifdef __cplusplus
  typedef struct _object PyObject;
  //struct PyObject;
#else
  typedef struct PyObject_ PyObject;
#endif
#endif

#ifdef __cplusplus
}
#endif


#include "radium_proc.h"


#endif
