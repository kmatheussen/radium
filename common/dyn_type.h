#pragma once

#include <wchar.h>

#include "ratio_type.h"
#include "../api/s7_types.h"

struct _hash_t;
typedef struct _hash_t hash_t;

struct _dynvec_t;
typedef struct _dynvec_t dynvec_t;

struct _blub_t;
typedef struct _blub_t blub_t;

enum DynType{
  UNINITIALIZED_TYPE = 0, // Must be 0
  STRING_TYPE,
  SYMBOL_TYPE,
  INT_TYPE,
  FLOAT_TYPE,
  HASH_TYPE,
  ARRAY_TYPE,
  RATIO_TYPE,
  FUNC_TYPE,
  INSTRUMENT_TYPE,
  FILEPATH_TYPE,
  BLUB_TYPE,
  BOOL_TYPE // must be placed last (see below)
};

#define NUM_DYNTYPE_TYPES (1+BOOL_TYPE) // If placed inside enum, compiler will complain about missing handler in switch{}.

typedef struct{
  enum DynType type;
  union{
    const wchar_t *string;
    const char *symbol;
    int64_t int_number;
    instrument_t instrument;
    filepath_t filepath;
    double float_number;
    hash_t *hash;
    dynvec_t *array;
    Ratio *ratio;
    func_t *func;
    blub_t *blub;
    bool bool_number;
  };
} dyn_t;

struct _dynvec_t{
  int num_elements;
  int num_elements_allocated;
  dyn_t *elements;
};

struct _blub_t{
  int size;
  void *data;
  
  dyn_t (*create_savable_dyn)(const blub_t*);
  dyn_t funcdata;
};

extern const dynvec_t g_empty_dynvec;
extern const dyn_t g_empty_dynvec_dyn;
extern const dyn_t g_uninitialized_dyn;

extern const dyn_t g_dyn_false;
extern const dyn_t g_dyn_true;

extern const dyn_t g_dyn_minus_one;
