#ifndef _RADIUM_COMMON_DYN_TYPE_H
#define _RADIUM_COMMON_DYN_TYPE_H

#include <wchar.h>

struct _hash_t;
typedef struct _hash_t hash_t;

enum DynType{
  STRING_TYPE,
  INT_TYPE,
  FLOAT_TYPE,
  HASH_TYPE,
  BOOL_TYPE // must be placed last because of the NUM_DYNTYPE_TYPES definition below.
};

#define NUM_DYNTYPE_TYPES (1+BOOL_TYPE)

typedef struct{
  enum DynType type;
  union{
    const wchar_t *string;
    int64_t int_number;
    double float_number;
    hash_t *hash;
    bool bool_number;
  };
} dyn_t;

#endif
