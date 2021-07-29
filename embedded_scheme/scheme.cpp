/* Copyright 2014 Kjetil S. Matheussen

This program is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public License
as published by the Free Software Foundation; either version 2
of the License, or (at your option) any later version.
-
This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA. */



#include <unistd.h>

#include "s7.h"
#include "s7webserver/s7webserver.h"

#include <QCoreApplication>
#include <QRegExp>
#include <QStringList>
#include <QDebug>

#include <qhttpserver.h>
#include <qhttprequest.h>
#include <qhttpresponse.h>

#include "../common/nsmtracker.h"
#include "../common/OS_settings_proc.h"
#include "../common/placement_proc.h"
#include "../common/visual_proc.h"
#include "../common/ratio_funcs.h"
#include "../common/notes_proc.h"
#include "../common/Vector.hpp"

#include "../Qt/Qt_PopupMenu_proc.h"

#include "../api/api_common_proc.h"
#include "../api/api_proc.h"


#include "scheme_proc.h"
#include "s7extra_proc.h"
#include "s7_radium_proc.h"


static s7_pointer s7_list2_(s7_scheme *sc, s7_int num_values, s7_pointer args[])
{
  s7_pointer ret = s7_make_list(sc, num_values, s7_nil(sc));
  s7_pointer p = ret;

  for (int i = 0 ; i < num_values ; i++){
    if (args[i]==NULL){      
      R_ASSERT_NON_RELEASE(false);
      break;
    }
    s7_set_car(p, args[i]);
    p = s7_cdr(p);
  }

  return ret;
}

#define S7_LIST(S7, Args...) ({                                      \
      s7_pointer _args[] = {Args};                                    \
      s7_list2_(S7,sizeof(_args)/sizeof(s7_pointer), _args);           \
    })


extern struct TEvent tevent;

static s7_scheme *s7 = NULL;
static s7webserver_t *s7webserver;

static s7_pointer g_eat_errors_failed;
bool g_scheme_failed = false;
int g_s7_history_disabled = 0;
  

bool g_scheme_has_inited1 = false;
bool g_scheme_has_inited2 = false;


QHash<const char*, int> g_num_s7_protected;


namespace{

  typedef radium::ProtectedS7Extra<s7_pointer> Protect;

  static int g_evals = 0;

  struct ScopedProtect{
    radium::Vector<int> _positions;
    radium::Vector<s7_pointer> _pointers;

    virtual ~ScopedProtect(){
      for(int i = 0 ; i < (int)_positions.size() ; i++)
        s7extra_unprotect(_pointers.at(i), _positions.at(i));
    }

    s7_pointer p(s7_pointer pointer){
      int pos = s7extra_protect(pointer);
      _positions.push_back(pos);
      _pointers.push_back(pointer);
      return pointer;
    }
  };
    
  struct ScopedEvalTracker : public ScopedProtect{
    ScopedEvalTracker(){
      R_ASSERT(THREADING_is_main_thread());
      g_evals++;
    }
    ~ScopedEvalTracker(){
      g_evals--;
    }
  };

  static int g_gc_is_off = 0;
  
  struct ScopedGcDisabler{
    ScopedGcDisabler(){
      g_gc_is_off++;
      if (g_gc_is_off==1)
        s7_gc_on(s7, false);
    }

    ~ScopedGcDisabler(){
      R_ASSERT_RETURN_IF_FALSE(g_gc_is_off >= 0);
      g_gc_is_off--;
      if (g_gc_is_off==0)
        s7_gc_on(s7, true);
    }
  };

}

static s7_pointer find_scheme_value(s7_scheme *s7, const char *funcname){
  
#if !defined(RELEASE)
  R_ASSERT(s7_is_defined(s7, funcname));
  {
    static s7_pointer s_func = s7_undefined(s7);
    if (!s7_is_procedure(s_func)){
      s7_pointer symbol = s7_make_symbol(s7, "FROM-C-assert-that-function-can-be-called-from-C");
      s_func = s7_symbol_local_value(s7, symbol, s7_rootlet(s7));
      if (!s7_is_procedure(s_func))
        abort();
      s7_gc_protect(s7, s_func);
    }
    
    s7_pointer args = S7_LIST(s7,
                               s7_make_string(s7, funcname)
                               );
    
    s7_call(s7, s_func, args);
  }
#endif
  
  s7_pointer symbol = s7_make_symbol(s7, funcname);
  s7_pointer value = s7_symbol_local_value(s7, symbol, s7_rootlet(s7));
     
  return value;  
}

static s7_pointer find_and_protect_scheme_value(const char *funcname){
  s7_pointer scheme_func = find_scheme_value(s7, funcname);
  
  s7_gc_protect(s7, scheme_func);
  return scheme_func;
}

static s7_pointer place_to_ratio(const Place *p){
  
  s7_Int temp = p->line*p->dividor; // use temp variable (of type s7_Int, which is at least 64 bit) to make sure it doesn't overflow.
  s7_Int a = temp + p->counter;
  s7_Int b = p->dividor;
  if (b==0){
    R_ASSERT(false);
    b = 1;
  }
  s7_pointer ratio = s7_make_ratio(s7, a, b);
  
  //fprintf(stderr,"\n\n          a: %d, b: %d. is_number: %d, is_integer: %d, is_ratio: %d, is_real: %d\n\n\n", (int)a, (int)b, s7_is_number(ratio),s7_is_integer(ratio),s7_is_ratio(ratio),s7_is_real(ratio));  
  return ratio;
}

static s7_pointer place_to_ratio(const Place &p){
  return place_to_ratio(&p);
}

static Place *ratio_to_place(s7_pointer ratio){
  s7_Int num = s7_numerator(ratio);
  s7_Int den = s7_denominator(ratio);

  Place ret = place_from_64b(num, den);

  return PlaceCreate(ret.line, ret.counter, ret.dividor);
}

static Place p_ratio_to_place(s7_pointer ratio){
  s7_Int num = s7_numerator(ratio);
  s7_Int den = s7_denominator(ratio);

  return place_from_64b(num, den);
}

static Place number_to_place(s7_scheme *s7, s7_pointer number, const char **error){

  if (s7_is_ratio(number)) {
    s7_Int num = s7_numerator(number);
    s7_Int den = s7_denominator(number);

    if (num<0 && den<0){
      num = -num;
      den = -den;
    }

    if (num<0 || den<0){
      if (error)
        *error = "place>=0";
      else
        R_ASSERT_NON_RELEASE(false);
      return p_Create(0,0,1);
    }
    
    return place_from_64b(num, den);
  }

  if (s7_is_integer(number)){
    int line = (int)s7_integer(number);
    if
#if defined(RELEASE)
      (line < -1)  // Earlier we allowed -1 as a no-op place, but now 'same-place must be used instead.
#else
      (line < 0)
#endif
      {
      if (error)
        *error = "place>=0";
      else
        R_ASSERT_NON_RELEASE(false);
      return p_Create(0,0,1);
    }
    return p_Create((int)s7_integer(number), 0, 1);
  }
  
  if (s7_is_real(number)) {
    double real = s7_real(number);
    if (real < 0){
      if (error)
        *error = "place>=0";
      else
        R_ASSERT_NON_RELEASE(false);
      return p_Create(0,0,1);
    }
    
    Place place;
    Double2Placement(real, &place);
    
#if !defined(RELEASE)
    RError("scheme.cpp/number_to_place: input parameter was a real. is_number: %d, is_integer: %d, is_ratio: %d, is_real: %d, value: %f, is_complex: %d\n\n\n",s7_is_number(number),s7_is_integer(number),s7_is_ratio(number),s7_is_real(number),s7_number_to_real(s7,number),s7_is_complex(number));
#endif

    return place;
  }

  
  RError("scheme.cpp/number_to_place: input parameter was not a number. returning 0. is_number: %d, is_integer: %d, is_ratio: %d, is_real: %d, value: %f, is_complex: %d\n\n\n",s7_is_number(number),s7_is_integer(number),s7_is_ratio(number),s7_is_real(number),s7_number_to_real(s7,number),s7_is_complex(number));
    
  return p_Create(0,0,1);
}


static s7_pointer s7extra_make_real(s7_scheme *radiums7_sc, double x){
#if !defined(RELEASE)
  if(!sane_isnormal(x))
    abort();
#endif

  return s7_make_real(radiums7_sc, x);
}

/**
 *
 * s7pointer -> dyn_t (or subtypes of dyn_t)
 *
 **/


bool s7extra_is_place(s7_pointer place){
  if (s7_is_symbol(place) && !strcmp(s7_symbol_name(place), "same-place"))
    return true;
  else
    return s7_is_real(place) || s7_is_rational(place);
}

Place s7extra_place(s7_scheme *s7, s7_pointer place){
  return number_to_place(s7, place, NULL); // Should we allow floating numbers? (i.e. not give error message for it) Yes, definitely.
}

s7_pointer s7extra_make_place(s7_scheme *radiums7_sc, const Place place){
  return place_to_ratio(&place);
}

bool s7extra_is_dyn(s7_pointer dyn){
  return s7_is_number(dyn) || s7_is_string(dyn) || s7_is_boolean(dyn) || s7_is_hash_table(dyn) || s7_is_vector(dyn) || s7_is_pair(dyn);
}

// 's_hash' has been gc-protected before calling
static hash_t *s7extra_hash(s7_scheme *s7, s7_pointer s_hash){
  int hash_size = 16;

  hash_t *r_hash = HASH_create(hash_size); // would be nice to know size of s_hash so we didn't have to rehash

  R_ASSERT_RETURN_IF_FALSE2(s7_is_hash_table(s_hash), r_hash);

  Protect protect(s_hash); // Not sure if this is necessary. (doesn't the iterator below hold a pointer to the vector?)
    
  Protect iterator(s7_make_iterator(s7, s_hash));

  int num_elements = 0;
  while(true){
    s7_pointer val = s7_iterate(s7, iterator.v);

    if (s7_iterator_is_at_end(s7, iterator.v))
      break;

    s7_pointer key = s7_car(val);

    const dyn_t value = s7extra_dyn(s7, s7_cdr(val));

    if (value.type != BOOL_TYPE || value.bool_number != false){ // Earlier, elements containing "#f" wasn't included in s7 hash tables.

      const char *key_name;

      if (s7_is_symbol(key))
        key_name = s7_symbol_name(key);
      else{
        handleError("Only symbols are supported as hash table keys");
        key_name = "___radium__illegal_key type";
      }
      
      HASH_put_dyn(r_hash,
                   key_name,
                   value);
      
      num_elements++;
      if (num_elements > hash_size*2){
        HASH_REHASH(r_hash);
        hash_size = num_elements;
      }
      
    }
  }

  return r_hash;
}

static dynvec_t s7extra_array(s7_scheme *s7, s7_pointer vector){
  Protect protect(vector); // Not sure if this is necessary. (doesn't the iterator below hold a pointer to the vector?)
  
  dynvec_t dynvec = {};

  Protect iterator(s7_make_iterator(s7, vector));

  while(true){
    s7_pointer val = s7_iterate(s7, iterator.v);

    if (s7_iterator_is_at_end(s7, iterator.v))
      break;

    DYNVEC_push_back(&dynvec, s7extra_dyn(s7, val));
  }

  return dynvec;
}


static bool is_instrument(s7_pointer s){
  return s7_is_c_object(s) && (s7_c_object_type(s) == g_instrument_type_tag);
}

static bool is_file(s7_pointer s){
  return s7_is_c_object(s) && (s7_c_object_type(s) == g_file_type_tag);
}

static bool is_filepath(s7_pointer s){
  return s7_is_c_object(s) && (s7_c_object_type(s) == g_filepath_type_tag);
}

static instrument_t get_instrument(s7_pointer s){
  R_ASSERT_NON_RELEASE(is_instrument(s));
                                        
  if (sizeof(instrument_t) <= sizeof(void*))
    return make_instrument((int64_t)s7_c_object_value(s));
      
  int64_t *i = (int64_t*)s7_c_object_value(s);

  return make_instrument(*i);
}

static file_t get_file(s7_pointer s){
  R_ASSERT_NON_RELEASE(is_file(s));
                                        
  if (sizeof(file_t) <= sizeof(void*))
    return make_file((int64_t)s7_c_object_value(s));
      
  int64_t *i = (int64_t*)s7_c_object_value(s);

  return make_file(*i);
}

static filepath_t get_filepath(s7_pointer s, bool make_copy){
  R_ASSERT_NON_RELEASE(is_filepath(s));

  const wchar_t *w = (const wchar_t*)s7_c_object_value(s);
  const wchar_t *i = make_copy ? talloc_wcsdup(w) : w;

  return make_filepath(i);
}

static dyn_t create_dyn_from_s7(s7_scheme *s7, s7_pointer s, bool undefinedIsError){
  
  if (s7_is_integer(s))
    return DYN_create_int(s7_integer(s));

  if (s7_is_ratio(s))
    return DYN_create_ratio(make_ratio(s7_numerator(s), s7_denominator(s)));

  if (s7_is_number(s))
    return DYN_create_float(s7_number_to_real(s7, s));

  if (s7_is_string(s))
    return DYN_create_string_from_chars(s7_string(s));

  if (s7_is_symbol(s))
    return DYN_create_symbol(s7_symbol_name(s));

  if (s7_is_boolean(s))
    return DYN_create_bool(s7_boolean(s7, s));

  if (s7_is_hash_table(s))
    return DYN_create_hash(s7extra_hash(s7, s));

  if (s7_is_vector(s) || s7_is_pair(s))
    return DYN_create_array(s7extra_array(s7, s));

  if (s7_is_null(s7, s)){
    dynvec_t vec = {};
    return DYN_create_array(vec);
  }    

  if (s7_is_procedure(s))
    return DYN_create_func((func_t*)s);

  if (is_instrument(s))
    return DYN_create_instrument(get_instrument(s));
      
  if (is_filepath(s))
    return DYN_create_filepath(get_filepath(s, false));
      
  if(undefinedIsError){
    //if (g_user_interaction_enabled==false)
    //  abort(); // Something is wrong with the script. Call abort here to get backtrace.
    handleError("s7extra_dyn: Unsupported s7 type");
  }
  
  return g_uninitialized_dyn;
}

dyn_t s7extra_dyn(s7_scheme *s7, s7_pointer s){
  return create_dyn_from_s7(s7, s, true);
}


bool s7extra_is_dynvec(s7_pointer dynvec){
  return s7_is_vector(dynvec) || s7_is_list(s7, dynvec);
}

dynvec_t s7extra_dynvec(s7_scheme *s7, s7_pointer s){
  if (s7_is_vector(s) || s7_is_list(s7, s))
    return s7extra_array(s7, s);

  handleError("s7extra_dynvec: Unsupported s7 type");

  dynvec_t ret = {};
  return ret;
}

static s7_pointer dynvec_to_s7(s7_scheme *sc, const dynvec_t &dynvec);

s7_pointer s7extra_make_dynvec(s7_scheme *radiums7_sc, const dynvec_t dynvec){
  return dynvec_to_s7(radiums7_sc, dynvec);
}


/**
 *
 * Various s7extra_get_* functions. Used by api/radium_s7_wrap.c (a file generated by bin/protoconfparser.py).
 *
 **/
int64_t s7extra_get_integer(s7_scheme *s7, s7_pointer s, const char **error){
  if (!s7_is_integer(s)){
    *error = "integer";
    return -1;
  }

  return s7_integer(s);
}

float s7extra_get_float(s7_scheme *s7, s7_pointer s, const char **error){
  if (!s7_is_number(s)){
    *error = "float";
    return -1;
  }

  return s7_number_to_real(s7, s);
}

double s7extra_get_double(s7_scheme *s7, s7_pointer s, const char **error){
  if (!s7_is_number(s)){
    *error = "double";
    return -1;
  }

  return s7_number_to_real(s7, s);
}

const char *s7extra_get_string(s7_scheme *s7, s7_pointer s, const char **error){
  if (!s7_is_string(s)){
    *error = "string";
    return NULL;
  }

  return s7_string(s);
}

bool s7extra_get_boolean(s7_scheme *s7, s7_pointer s, const char **error){
  if (!s7_is_boolean(s)){
    *error = "bool";
    return false;
  }

  return s7_boolean(s7, s);
}

Place s7extra_get_place(s7_scheme *s7, s7_pointer s, const char **error){

  if (s7_is_symbol(s) && !strcmp(s7_symbol_name(s), "same-place"))
    return g_same_place;

  bool legal = s7_is_rational(s);

#if !defined(RELEASE)
  if (!legal)
    R_ASSERT(!s7_is_real(s));  // 3rd party code is allowed to supply a real as a place, but radium itself doesn't do this.
#endif

  legal |= s7_is_real(s);
  
  if (!legal){
    *error = "place (i.e. any type of number or 'same-place)";
    return p_Create(0,0,1);
  }

  return number_to_place(s7, s, error);
}

func_t *s7extra_get_func(s7_scheme *s7, s7_pointer func, const char **error){
  if (!s7_is_procedure(func)){
    *error = "function";
    return NULL;
  }
  
  return (func_t*)func;
}

dynvec_t s7extra_get_dynvec(s7_scheme *s7, s7_pointer vec, const char **error){
  if (!s7extra_is_dynvec(vec)){
    *error = "vector or list";
    dynvec_t ret = {};
    return ret;
  }
  
  return s7extra_dynvec(s7, vec);
}

dyn_t s7extra_get_dyn(s7_scheme *s7, s7_pointer s, const char **error){
  dyn_t ret = create_dyn_from_s7(s7, s, false);
  if (ret.type==UNINITIALIZED_TYPE)
    *error = "known type";
  
  return ret;
}

instrument_t s7extra_get_instrument(s7_scheme *s7, s7_pointer s, const char **error){
  
  if (!is_instrument(s)){
    *error = "instrument";
    return make_instrument(-1);
  }

  return get_instrument(s);
}

file_t s7extra_get_file(s7_scheme *s7, s7_pointer s, const char **error){
  
  if (!is_file(s)){
    *error = "file";
    return createIllegalFile();
  }

  return get_file(s);
}

filepath_t s7extra_get_filepath(s7_scheme *s7, s7_pointer s, const char **error){
  
  if (!is_filepath(s)){
    *error = "filepath";
    return createIllegalFilepath();
  }

  return get_filepath(s, true); // Probably not necessary to make a copy, but we never know.
}



/**
 *
 * dyn_t -> s7pointer
 *
 **/
  
static s7_pointer hash_to_s7(s7_scheme *sc, const hash_t *r_hash){

  Protect s_hash(s7_make_hash_table(sc, HASH_get_num_elements(r_hash)));

  dynvec_t dynvec = HASH_get_values(r_hash);
  vector_t keys = HASH_get_keys(r_hash);

  for(int i = 0 ; i < dynvec.num_elements ; i++){
    s7_hash_table_set(sc,
                      s_hash.v,
                      Protect(s7_make_symbol(sc, (char*)keys.elements[i])).v,
                      Protect(s7extra_make_dyn(sc, dynvec.elements[i])).v
                      );
  }

  return s_hash.v;
}

static s7_pointer dynvec_to_s7(s7_scheme *sc, const dynvec_t &dynvec){
  
  Protect s_vec(s7_make_vector(sc, dynvec.num_elements));

  for(int i = 0 ; i < dynvec.num_elements ; i++){
    s7_vector_set(sc,
                  s_vec.v,
                  i,
                  Protect(s7extra_make_dyn(sc, dynvec.elements[i])).v
                  );
  }

  return s_vec.v;
}

s7_pointer s7extra_make_dyn(s7_scheme *radiums7_sc, const dyn_t dyn){
  
  switch(dyn.type){
    case UNINITIALIZED_TYPE:
      return s7_unspecified(radiums7_sc);
    case STRING_TYPE:
      return s7_make_string(radiums7_sc, STRING_get_chars(dyn.string));
    case SYMBOL_TYPE:
      return s7_make_symbol(radiums7_sc, dyn.symbol);
    case INT_TYPE:
      return s7_make_integer(radiums7_sc, dyn.int_number);
    case FLOAT_TYPE:
      return s7extra_make_real(radiums7_sc, dyn.float_number);
    case HASH_TYPE:
      return hash_to_s7(radiums7_sc, dyn.hash);
    case ARRAY_TYPE:
      return dynvec_to_s7(radiums7_sc, *dyn.array);
    case RATIO_TYPE:
      return s7_make_ratio(radiums7_sc, dyn.ratio->num, dyn.ratio->den);
    case FUNC_TYPE:
      return (s7_pointer)dyn.func;
    case INSTRUMENT_TYPE:
      return s7extra_make_instrument(s7, dyn.instrument);
    case FILEPATH_TYPE:
      return s7extra_make_filepath(s7, dyn.filepath);
    case BOOL_TYPE:
      return s7_make_boolean(radiums7_sc, dyn.bool_number);
    case BLUB_TYPE:
      R_ASSERT(false);
      break;
  }

  return s7_make_boolean(radiums7_sc, false);
}

func_t *s7extra_func(s7_scheme *s7, s7_pointer func){
  return (func_t*)func;
}

func_t *s7extra_get_func_from_funcname_for_storing(const char *funcname){
  return (func_t*)find_and_protect_scheme_value(funcname);
}

func_t *s7extra_get_func_from_funcname(const char *funcname){
  return (func_t*)find_scheme_value(s7, funcname);
}

int g_scheme_nested_level = 0;

static s7_pointer catch_call(s7_scheme *sc, const s7_pointer args){
  static s7_pointer s_catchallerrors_func = s7_undefined(sc);
  
#if defined(RELEASE)
  if (!s7_is_procedure(s_catchallerrors_func))
    s_catchallerrors_func = find_and_protect_scheme_value("FROM-C-catch-all-errors-and-display-backtrace-automatically");
#else
  s_catchallerrors_func = find_scheme_value(s7, "FROM-C-catch-all-errors-and-display-backtrace-automatically");
#endif

  if (!s7_is_procedure(s_catchallerrors_func)){
    if (s_catchallerrors_func != s7_undefined(sc)){
      fprintf(stderr, "NOTE: FROM-C-catch-all-errors-and-display-backtrace-automatically is not a procedure and not undefined. (?)\n");
#if !defined(RELEASE)
      abort();
#endif
    }
    fprintf(stderr, "NOTE: FROM-C-catch-all-errors-and-display-backtrace-automatically not defined yet. Won't eval scheme now.\n");
    return s7_make_string(sc, "NOTE: FROM-C-catch-all-errors-and-display-backtrace-automatically not defined yet. Won't eval scheme now.\n");
  }
             

  g_scheme_failed=false;

  ASSERT_IS_NONRT_MAIN_THREAD();

  bool old_is_going_to_call_throwExceptionIfError = g_is_going_to_call_throwExceptionIfError; // save old value. Necesarry if a C function called from C evaluates some other scheme code.
  if (g_scheme_nested_level==0){ R_ASSERT_NON_RELEASE(old_is_going_to_call_throwExceptionIfError==false); }
  g_is_going_to_call_throwExceptionIfError = false;
    
  g_scheme_nested_level++;

#if !defined(RELEASE)
  int curr_level = g_scheme_nested_level;
#endif

  s7_pointer ret = s7_call(sc, s_catchallerrors_func, args);

  R_ASSERT_NON_RELEASE(curr_level==g_scheme_nested_level); // Assert that g_catchallerros_func is working. (we don't want any longjmp in the C part)
  
  g_scheme_nested_level--;

  g_is_going_to_call_throwExceptionIfError = old_is_going_to_call_throwExceptionIfError; // put back old value
  
  if (s7_is_symbol(ret) && s7_symbol_name(ret)==s7_symbol_name(g_eat_errors_failed)){
    g_scheme_failed = true;
    R_ASSERT(g_is_starting_up==false);
  }
  
  return ret;
}

void s7extra_callFunc_void_void(const func_t *func){
  ScopedEvalTracker eval_tracker;

  catch_call(s7,
             S7_LIST(s7, (s7_pointer)func)
             );
}

void s7extra_callFunc2_void_void(const char *funcname){
  s7extra_callFunc_void_void((const func_t*)find_scheme_value(s7, funcname));
}

void s7extra_applyFunc_void(const func_t *func, dynvec_t args){
  ScopedEvalTracker eval_tracker;

  s7_pointer list = s7_nil(s7);
  
  for(int i=args.num_elements-1;i>=0;i--){
    list = eval_tracker.p(s7_cons(s7,
                                  eval_tracker.p(s7extra_make_dyn(s7, args.elements[i])),
                                  list)
                          );
  }

  list = eval_tracker.p(s7_cons(s7,
                                (s7_pointer)func,
                                list)
                        );
  
  catch_call(s7, list);
}

void s7extra_applyFunc2_void(const char *funcname, dynvec_t args){
  s7extra_applyFunc_void((const func_t*)find_scheme_value(s7, funcname), args);
}

void s7extra_applyFunc_void_varargs(const func_t *func, ...){
  ScopedEvalTracker eval_tracker;

  s7_pointer list = s7_nil(s7);

  {
    va_list vl;
    va_start(vl,func);
    
    for(;;){
      dyn_t arg = va_arg(vl, dyn_t);
      
      if (arg.type==UNINITIALIZED_TYPE)
        break;
      
      list = eval_tracker.p(s7_cons(s7,
                                    eval_tracker.p(s7extra_make_dyn(s7, arg)),
                                    list)
                            );
    }
    
    va_end(vl);
  }
  
  
  list = eval_tracker.p(s7_reverse(s7, list));
  
  list = eval_tracker.p(s7_cons(s7,
                                (s7_pointer)func,
                                list)
                        );
  
  catch_call(s7, list);
}

dyn_t s7extra_applyFunc_dyn(const func_t *func, dynvec_t args){
  ScopedEvalTracker eval_tracker;

  s7_pointer list = eval_tracker.p(s7_nil(s7));
  
  for(int i=args.num_elements-1;i>=0;i--){
    list = eval_tracker.p(s7_cons(s7,
                                  eval_tracker.p(s7extra_make_dyn(s7, args.elements[i])),
                                  list)
                          );
  }

  list = eval_tracker.p(s7_cons(s7,
                                (s7_pointer)func,
                                list)
                        );
  
  return s7extra_dyn(s7, catch_call(s7, list));
}

dyn_t s7extra_applyFunc2_dyn(const char *funcname, dynvec_t args){
  return s7extra_applyFunc_dyn((const func_t*)find_scheme_value(s7, funcname), args);
}

double s7extra_callFunc_double_void(const func_t *func){
  ScopedEvalTracker eval_tracker;
  
  s7_pointer ret = catch_call(s7,
                              S7_LIST(s7, (s7_pointer)func)
                              );

  if (!s7_is_number(ret)){
    handleError("Callback did not return a double");
    return -1.0;
  }else{
    return s7_number_to_real(s7, ret);
  }
}

double s7extra_callFunc2_double_void(const char *funcname){
  return s7extra_callFunc_double_void((const func_t*)find_scheme_value(s7, funcname));
}


bool s7extra_callFunc_bool_void(const func_t *func){
  ScopedEvalTracker eval_tracker;
  
  s7_pointer ret = catch_call(s7,
                              S7_LIST(s7, (s7_pointer)func)
                              );

  if (!s7_is_boolean(ret)){
    handleError("Callback did not return a boolean");
    return false;
  }else{
    return s7_boolean(s7, ret);
  }
}

bool s7extra_callFunc2_bool_void(const char *funcname){
  return s7extra_callFunc_bool_void((const func_t*)find_scheme_value(s7, funcname));
}


void s7extra_add_history(const char *funcname, const char *info){
  //printf("%s - %s", funcname, info);

  ASSERT_IS_NONRT_MAIN_THREAD_NON_RELEASE();

  s7_add_to_history(s7, s7_make_string(s7, info));
  s7_add_to_history(s7, s7_make_string(s7, funcname));
}

  
dyn_t s7extra_callFunc_dyn_void(const func_t *func){
  ScopedEvalTracker eval_tracker;

  //s7_add_to_history(s7, s7_make_string(s7, "----------------------s7extra_callFunc_dyn_void---------------------"));
  //s7_add_to_history(s7, s7_cons(s7, (s7_pointer)func, s7_cons(s7, s7_make_string(s7, "\n\n----------------------s7extra_callFunc_dyn_void222---------------------\n\n"), s7_list(s7, 0))));
    
  s7_pointer ret = catch_call(s7,
                              S7_LIST(s7, (s7_pointer)func)
                              );
  
  return s7extra_dyn(s7, ret);
}

dyn_t s7extra_callFunc2_dyn_void(const char *funcname){
  return s7extra_callFunc_dyn_void((const func_t*)find_scheme_value(s7, funcname));
}

dyn_t s7extra_callFunc_dyn_int(const func_t *func, int64_t arg1){
  ScopedEvalTracker eval_tracker;
  
  s7_pointer ret = catch_call(s7,
                              S7_LIST(s7,
                                       (s7_pointer)func,
                                       s7_make_integer(s7, arg1)
                                       )
                              );
  

  return s7extra_dyn(s7, ret);
}

dyn_t s7extra_callFunc2_dyn_int(const char *funcname, int64_t arg1){
  return s7extra_callFunc_dyn_int((const func_t*)find_scheme_value(s7, funcname), arg1);
}


dyn_t s7extra_callFunc_dyn_int_int_int(const func_t *func, int64_t arg1, int64_t arg2, int64_t arg3){
  ScopedEvalTracker eval_tracker;
  
  s7_pointer ret = catch_call(s7,
                              S7_LIST(s7,
                                       (s7_pointer)func,
                                       s7_make_integer(s7, arg1),
                                       s7_make_integer(s7, arg2),
                                       s7_make_integer(s7, arg3)
                                      )
                              );
  

  return s7extra_dyn(s7, ret);
}

dyn_t s7extra_callFunc2_dyn_int_int_int(const char *funcname, int64_t arg1, int64_t arg2, int64_t arg3){
  return s7extra_callFunc_dyn_int_int_int((const func_t*)find_scheme_value(s7, funcname), arg1, arg2, arg3);
}


dyn_t s7extra_callFunc_dyn_int_int_int_dyn_dyn_dyn(const func_t *func, int64_t arg1, int64_t arg2, int64_t arg3, const dyn_t arg4, const dyn_t arg5, const dyn_t arg6){
  ScopedEvalTracker eval_tracker;
  
  s7_pointer ret = catch_call(s7,
                              S7_LIST(s7,
                                       (s7_pointer)func,
                                       eval_tracker.p(s7_make_integer(s7, arg1)), // Need to protect everything, even integers, since s7extra_make_dyn may allocate over 256 objects.
                                       eval_tracker.p(s7_make_integer(s7, arg2)),
                                       eval_tracker.p(s7_make_integer(s7, arg3)),
                                       eval_tracker.p(s7extra_make_dyn(s7, arg4)),
                                       eval_tracker.p(s7extra_make_dyn(s7, arg5)),
                                       eval_tracker.p(s7extra_make_dyn(s7, arg6))
                                       )
                              );
  

  return s7extra_dyn(s7, ret);
}

dyn_t s7extra_callFunc2_dyn_int_int_int_dyn_dyn_dyn(const char *funcname, int64_t arg1, int64_t arg2, int64_t arg3, const dyn_t arg4, const dyn_t arg5, const dyn_t arg6){
  return s7extra_callFunc_dyn_int_int_int_dyn_dyn_dyn((const func_t*)find_scheme_value(s7, funcname), arg1, arg2, arg3, arg4, arg5, arg6);
}

dyn_t s7extra_callFunc_dyn_int_int_int_dyn_dyn_dyn_dyn_dyn(const func_t *func, int64_t arg1, int64_t arg2, int64_t arg3, const dyn_t arg4, const dyn_t arg5, const dyn_t arg6, const dyn_t arg7, const dyn_t arg8){
  ScopedEvalTracker eval_tracker;
  
  s7_pointer ret = catch_call(s7,
                              S7_LIST(s7,
                                       (s7_pointer)func,
                                       eval_tracker.p(s7_make_integer(s7, arg1)),
                                       eval_tracker.p(s7_make_integer(s7, arg2)),
                                       eval_tracker.p(s7_make_integer(s7, arg3)),
                                       eval_tracker.p(s7extra_make_dyn(s7, arg4)),
                                       eval_tracker.p(s7extra_make_dyn(s7, arg5)),
                                       eval_tracker.p(s7extra_make_dyn(s7, arg6)),
                                       eval_tracker.p(s7extra_make_dyn(s7, arg7)),
                                       eval_tracker.p(s7extra_make_dyn(s7, arg8))
                                       )
                              );
  
  return s7extra_dyn(s7, ret);
}

dyn_t s7extra_callFunc2_dyn_int_int_int_dyn_dyn_dyn_dyn_dyn(const char *funcname, int64_t arg1, int64_t arg2, int64_t arg3, const dyn_t arg4, const dyn_t arg5, const dyn_t arg6, const dyn_t arg7, const dyn_t arg8){
  return s7extra_callFunc_dyn_int_int_int_dyn_dyn_dyn_dyn_dyn((const func_t*)find_scheme_value(s7, funcname), arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8);
}


dyn_t s7extra_callFunc_dyn_dyn_dyn_dyn_int(const func_t *func, const dyn_t arg1, const dyn_t arg2, const dyn_t arg3, int64_t arg4){
  ScopedEvalTracker eval_tracker;
  
  s7_pointer ret = catch_call(s7,
                              S7_LIST(s7,
                                       (s7_pointer)func,
                                       eval_tracker.p(s7extra_make_dyn(s7, arg1)),
                                       eval_tracker.p(s7extra_make_dyn(s7, arg2)),
                                       eval_tracker.p(s7extra_make_dyn(s7, arg3)),
                                       eval_tracker.p(s7_make_integer(s7, arg4))
                                       )
                              );
  

  return s7extra_dyn(s7, ret);
}

dyn_t s7extra_callFunc2_dyn_dyn_dyn_dyn_int(const char *funcname, const dyn_t arg1, const dyn_t arg2, const dyn_t arg3, int64_t arg4){
  return s7extra_callFunc_dyn_dyn_dyn_dyn_int((const func_t*)find_scheme_value(s7, funcname), arg1, arg2, arg3, arg4);
}


dyn_t s7extra_callFunc_dyn_dyn_int(const func_t *func, const dyn_t arg1, int64_t arg2){
  ScopedEvalTracker eval_tracker;
  
  s7_pointer ret = catch_call(s7,
                              S7_LIST(s7,
                                       (s7_pointer)func,
                                       eval_tracker.p(s7extra_make_dyn(s7, arg1)),
                                       eval_tracker.p(s7_make_integer(s7, arg2))
                                       )
                              );
  

  return s7extra_dyn(s7, ret);
}

dyn_t s7extra_callFunc2_dyn_dyn_int(const char *funcname, const dyn_t arg1, int64_t arg2){
  return s7extra_callFunc_dyn_dyn_int((const func_t*)find_scheme_value(s7, funcname), arg1, arg2);
}


dyn_t s7extra_callFunc_dyn_int_charpointer(const func_t *func, int64_t arg1, const char *arg2){
  ScopedEvalTracker eval_tracker;
  
  s7_pointer ret = catch_call(s7,
                              S7_LIST(s7,
                                       (s7_pointer)func,
                                       s7_make_integer(s7, arg1),
                                       s7_make_string(s7, arg2)
                                       )
                              );
  

  return s7extra_dyn(s7, ret);
}

dyn_t s7extra_callFunc2_dyn_int_charpointer(const char *funcname, int64_t arg1, const char *arg2){
  return s7extra_callFunc_dyn_int_charpointer((const func_t*)find_scheme_value(s7, funcname), arg1, arg2);
}

dyn_t s7extra_callFunc_dyn_dyn_charpointer(const func_t *func, dyn_t arg1, const char *arg2){
  ScopedEvalTracker eval_tracker;
  
  s7_pointer ret = catch_call(s7,
                              S7_LIST(s7,
                                       (s7_pointer)func,
                                       eval_tracker.p(s7extra_make_dyn(s7, arg1)),
                                       eval_tracker.p(s7_make_string(s7, arg2))
                                       )
                              );
  

  return s7extra_dyn(s7, ret);
}

dyn_t s7extra_callFunc2_dyn_dyn_charpointer(const char *funcname, dyn_t arg1, const char *arg2){
  return s7extra_callFunc_dyn_dyn_charpointer((const func_t*)find_scheme_value(s7, funcname), arg1, arg2);
}


dyn_t s7extra_callFunc_dyn_charpointer(const func_t *func, const char *arg1){
  ScopedEvalTracker eval_tracker;
  
  s7_pointer ret = catch_call(s7,
                              S7_LIST(s7,
                                       (s7_pointer)func,
                                       s7_make_string(s7, arg1)
                                       )
                              );
  

  return s7extra_dyn(s7, ret);
}

dyn_t s7extra_callFunc2_dyn_charpointer(const char *funcname, const char *arg1){
  return s7extra_callFunc_dyn_charpointer((const func_t*)find_scheme_value(s7, funcname), arg1);
}


void s7extra_callFunc_void_int_charpointer_dyn(const func_t *func, int64_t arg1, const char* arg2, const dyn_t arg3){
  ScopedEvalTracker eval_tracker;
    
  catch_call(s7,
             S7_LIST(s7,
                      (s7_pointer)func,
                      eval_tracker.p(s7_make_integer(s7, arg1)),
                      eval_tracker.p(s7_make_string(s7, arg2)),
                      eval_tracker.p(s7extra_make_dyn(s7, arg3))
                      )
             );
}

void s7extra_callFunc2_void_int_charpointer_dyn(const char *funcname, int64_t arg1, const char* arg2, const dyn_t arg3){
  s7extra_callFunc_void_int_charpointer_dyn((const func_t*)find_scheme_value(s7, funcname), arg1, arg2, arg3);
}

void s7extra_callFunc_void_instrument_charpointer_dyn(const func_t *func, instrument_t arg1, const char* arg2, const dyn_t arg3){
  ScopedEvalTracker eval_tracker;
    
  catch_call(s7,
             S7_LIST(s7,
                      (s7_pointer)func,
                      eval_tracker.p(s7extra_make_instrument(s7, arg1)),
                      eval_tracker.p(s7_make_string(s7, arg2)),
                      eval_tracker.p(s7extra_make_dyn(s7, arg3))
                      )
             );
}

void s7extra_callFunc2_void_instrument_charpointer_dyn(const char *funcname, instrument_t arg1, const char* arg2, const dyn_t arg3){
  s7extra_callFunc_void_instrument_charpointer_dyn((const func_t*)find_scheme_value(s7, funcname), arg1, arg2, arg3);
}

void s7extra_callFunc_void_int_charpointer_int(const func_t *func, int64_t arg1, const char* arg2, int64_t arg3){
  ScopedEvalTracker eval_tracker;
    
  catch_call(s7,
             S7_LIST(s7,
                      (s7_pointer)func,
                      s7_make_integer(s7, arg1),
                      s7_make_string(s7, arg2),
                      s7_make_integer(s7, arg3)
                      )
             );
}

void s7extra_callFunc2_void_int_charpointer_int(const char *funcname, int64_t arg1, const char* arg2, int64_t arg3){
  s7extra_callFunc_void_int_charpointer_int((const func_t*)find_scheme_value(s7, funcname), arg1, arg2, arg3);
}

void s7extra_callFunc_void_instrument_charpointer_int(const func_t *func, instrument_t arg1, const char* arg2, int64_t arg3){
  ScopedEvalTracker eval_tracker;
    
  catch_call(s7,
             S7_LIST(s7,
                      (s7_pointer)func,
                      eval_tracker.p(s7extra_make_instrument(s7, arg1)),
                      eval_tracker.p(s7_make_string(s7, arg2)),
                      eval_tracker.p(s7_make_integer(s7, arg3))
                      )
             );
}

void s7extra_callFunc2_void_instrument_charpointer_int(const char *funcname, instrument_t arg1, const char* arg2, int64_t arg3){
  s7extra_callFunc_void_instrument_charpointer_int((const func_t*)find_scheme_value(s7, funcname), arg1, arg2, arg3);
}

void s7extra_callFunc_void_int_bool(const func_t *func, int64_t arg1, bool arg2){
  ScopedEvalTracker eval_tracker;
    
  catch_call(s7,
             S7_LIST(s7,
                      (s7_pointer)func,
                      s7_make_integer(s7, arg1),
                      s7_make_boolean(s7, arg2)
                      )
             );
}

void s7extra_callFunc2_void_int_bool(const char *funcname, int64_t arg1, bool arg2){
  s7extra_callFunc_void_int_bool((const func_t*)find_scheme_value(s7, funcname), arg1, arg2);
}

void s7extra_callFunc_void_int_int_bool(const func_t *func, int64_t arg1, int64_t arg2, bool arg3){
  ScopedEvalTracker eval_tracker;
    
  catch_call(s7,
             S7_LIST(s7,
                      (s7_pointer)func,
                      s7_make_integer(s7, arg1),
                      s7_make_integer(s7, arg2),
                      s7_make_boolean(s7, arg3)
                      )
             );
}

void s7extra_callFunc2_void_int_int_bool(const char *funcname, int64_t arg1, int64_t arg2, bool arg3){
  s7extra_callFunc_void_int_int_bool((const func_t*)find_scheme_value(s7, funcname), arg1, arg2, arg3);
}

void s7extra_callFunc_void_int(const func_t *func, int64_t arg1){
  ScopedEvalTracker eval_tracker;
  
  catch_call(s7,
             S7_LIST(s7,
                      (s7_pointer)func,
                      s7_make_integer(s7, arg1)
                      )
             
             );
}

void s7extra_callFunc2_void_int(const char *funcname, int64_t arg1){
  s7extra_callFunc_void_int((const func_t*)find_scheme_value(s7, funcname), arg1);
}

void s7extra_callFunc_void_double(const func_t *func, double arg1){
  ScopedEvalTracker eval_tracker;
  
  catch_call(s7,
             S7_LIST(s7,
                      (s7_pointer)func,
                      s7extra_make_real(s7, arg1)
                      )
             );
}

void s7extra_callFunc2_void_double(const char *funcname, double arg1){
  s7extra_callFunc_void_double((const func_t*)find_scheme_value(s7, funcname), arg1);
}

void s7extra_callFunc_void_bool(const func_t *func, bool arg1){
  ScopedEvalTracker eval_tracker;
  
  catch_call(s7,
             S7_LIST(s7,
                      (s7_pointer)func,
                      s7_make_boolean(s7, arg1)
                      )
             );
}

void s7extra_callFunc2_void_bool(const char *funcname, bool arg1){
  s7extra_callFunc_void_bool((const func_t*)find_scheme_value(s7, funcname), arg1);
}

void s7extra_callFunc_void_bool_bool(const func_t *func, bool arg1, bool arg2){
  ScopedEvalTracker eval_tracker;
  
  catch_call(s7,
             S7_LIST(s7,
                      (s7_pointer)func,
                      s7_make_boolean(s7, arg1),
                      s7_make_boolean(s7, arg2)
                      )
             );
}

void s7extra_callFunc2_void_bool_bool(const char *funcname, bool arg1, bool arg2){
  s7extra_callFunc_void_bool_bool((const func_t*)find_scheme_value(s7, funcname), arg1, arg2);
}

void s7extra_callFunc_void_float(const func_t *func, double arg1){
  ScopedEvalTracker eval_tracker;
  
  catch_call(s7,
             S7_LIST(s7,
                      (s7_pointer)func,
                      s7extra_make_real(s7, arg1)
                      )
             );
}

void s7extra_callFunc2_void_float(const char *funcname, double arg1){
  s7extra_callFunc_void_float((const func_t*)find_scheme_value(s7, funcname), arg1);
}

void s7extra_callFunc_void_float_float(const func_t *func, double arg1, double arg2){
  ScopedEvalTracker eval_tracker;
  
  catch_call(s7,
             S7_LIST(s7,
                      (s7_pointer)func,
                      s7extra_make_real(s7, arg1),
                      s7extra_make_real(s7, arg2)
                      )
             );
}

void s7extra_callFunc2_void_float_float(const char *funcname, double arg1, double arg2){
  s7extra_callFunc_void_float_float((const func_t*)find_scheme_value(s7, funcname), arg1, arg2);
}

void s7extra_callFunc_void_dyn(const func_t *func, const dyn_t arg1){
  ScopedEvalTracker eval_tracker;
  
  catch_call(s7,
             S7_LIST(s7,
                      (s7_pointer)func,
                      eval_tracker.p(s7extra_make_dyn(s7, arg1))
                      )
             );
}

void s7extra_callFunc2_void_dyn(const char *funcname, const dyn_t arg1){
  s7extra_callFunc_void_dyn((const func_t*)find_scheme_value(s7, funcname), arg1);
}

void s7extra_callFunc_void_dyn_dyn(const func_t *func, const dyn_t arg1, const dyn_t arg2){
  ScopedEvalTracker eval_tracker;
  
  catch_call(s7,
             S7_LIST(s7,
                      (s7_pointer)func,
                      eval_tracker.p(s7extra_make_dyn(s7, arg1)),
                      eval_tracker.p(s7extra_make_dyn(s7, arg2))
                      )
             );
}

void s7extra_callFunc2_void_dyn_dyn(const char *funcname, const dyn_t arg1, const dyn_t arg2){
  s7extra_callFunc_void_dyn_dyn((const func_t*)find_scheme_value(s7, funcname), arg1, arg2);
}

void s7extra_callFunc_void_dyn_bool(const func_t *func, const dyn_t arg1, bool arg2){
  ScopedEvalTracker eval_tracker;
  
  catch_call(s7,
             S7_LIST(s7,
                      (s7_pointer)func,
                      eval_tracker.p(s7extra_make_dyn(s7, arg1)),
                      s7_make_boolean(s7, arg2)
                      )
             );
}

void s7extra_callFunc2_void_dyn_bool(const char *funcname, const dyn_t arg1, bool arg2){
  s7extra_callFunc_void_dyn_bool((const func_t*)find_scheme_value(s7, funcname), arg1, arg2);
}

void s7extra_callFunc_void_dynvec_bool(const func_t *func, const dynvec_t arg1, bool arg2){
  ScopedEvalTracker eval_tracker;
  
  catch_call(s7,
             S7_LIST(s7,
                      (s7_pointer)func,
                      eval_tracker.p(s7extra_make_dynvec(s7, arg1)),
                      s7_make_boolean(s7, arg2)
                      )
             );
}

void s7extra_callFunc2_void_dynvec_bool(const char *funcname, const dynvec_t arg1, bool arg2){
  s7extra_callFunc_void_dynvec_bool((const func_t*)find_scheme_value(s7, funcname), arg1, arg2);
}

void s7extra_callFunc_void_charpointer(const func_t *func, const char* arg1){
  ScopedEvalTracker eval_tracker;
  
  catch_call(s7,
             S7_LIST(s7,
                      (s7_pointer)func,
                      s7_make_string(s7, arg1)
                      )
             );
}

void s7extra_callFunc2_void_charpointer(const char *funcname, const char* arg1){
  s7extra_callFunc_void_charpointer((const func_t*)find_scheme_value(s7, funcname), arg1);
}


void s7extra_callFunc_void_int_charpointer(const func_t *func, int64_t arg1, const char* arg2){
  ScopedEvalTracker eval_tracker;
  
  catch_call(s7,
             S7_LIST(s7,
                      (s7_pointer)func,
                      s7_make_integer(s7, arg1),
                      s7_make_string(s7, arg2)
                      )
             );
}

void s7extra_callFunc2_void_int_charpointer(const char *funcname, int64_t arg1, const char* arg2){
  s7extra_callFunc_void_int_charpointer((const func_t*)find_scheme_value(s7, funcname), arg1, arg2);
}

bool s7extra_callFunc_bool_int_charpointer(const func_t *func, int64_t arg1, const char* arg2){
  ScopedEvalTracker eval_tracker;
  
  s7_pointer ret = catch_call(s7,
                              S7_LIST(s7,
                                       (s7_pointer)func,
                                       s7_make_integer(s7, arg1),
                                       s7_make_string(s7, arg2)
                                       )
                              );
  
  if(!s7_is_boolean(ret)){
    handleError("Callback did not return a boolean");
    return false;
  }else{
    return s7_boolean(s7, ret);
  }
}

bool s7extra_callFunc2_bool_int_charpointer(const char *funcname, int64_t arg1, const char* arg2){
  return s7extra_callFunc_bool_int_charpointer((const func_t*)find_scheme_value(s7, funcname), arg1, arg2);
}

bool s7extra_callFunc_bool_int(const func_t *func, int64_t arg1){
  ScopedEvalTracker eval_tracker;
  
  s7_pointer ret = catch_call(s7,
                              S7_LIST(s7,
                                       (s7_pointer)func,
                                       s7_make_integer(s7, arg1)
                                       )
                              );
  
  if(!s7_is_boolean(ret)){
    handleError("Callback did not return a boolean");
    return false;
  }else{
    return s7_boolean(s7, ret);
  }
}

bool s7extra_callFunc2_bool_int(const char *funcname, int64_t arg1){
  return s7extra_callFunc_bool_int((const func_t*)find_scheme_value(s7, funcname), arg1);
}

void s7extra_callFunc_void_int_charpointer_bool_bool(const func_t *func, int64_t arg1, const char* arg2, bool arg3, bool arg4){
  ScopedEvalTracker eval_tracker;
  
  catch_call(s7,
             S7_LIST(s7,
                      (s7_pointer)func,
                      s7_make_integer(s7, arg1),
                      s7_make_string(s7, arg2),
                      s7_make_boolean(s7, arg3),
                      s7_make_boolean(s7, arg4)
                      )
             );
}

void s7extra_callFunc2_void_int_charpointer_bool_bool(const char *funcname, int64_t arg1, const char* arg2, bool arg3, bool arg4){
  s7extra_callFunc_void_int_charpointer_bool_bool((const func_t*)find_scheme_value(s7, funcname), arg1, arg2, arg3, arg4);
}

void s7extra_callFunc_void_int_int(const func_t *func, int64_t arg1, int64_t arg2){
  ScopedEvalTracker eval_tracker;
  
  catch_call(s7,
             S7_LIST(s7,
                        (s7_pointer)func,
                        s7_make_integer(s7, arg1),
                        s7_make_integer(s7, arg2)
                      )
             );
}

void s7extra_callFunc2_void_int_int(const char *funcname, int64_t arg1, int64_t arg2){
  s7extra_callFunc_void_int_int((const func_t*)find_scheme_value(s7, funcname), arg1, arg2);
}

void s7extra_callFunc_void_int_int_int(const func_t *func, int64_t arg1, int64_t arg2, int64_t arg3){
  ScopedEvalTracker eval_tracker;
  
  catch_call(s7,
             S7_LIST(s7,
                        (s7_pointer)func,
                        s7_make_integer(s7, arg1),
                        s7_make_integer(s7, arg2),
                        s7_make_integer(s7, arg3)
                        )
             );
}

void s7extra_callFunc2_void_int_int_int(const char *funcname, int64_t arg1, int64_t arg2, int64_t arg3){
  s7extra_callFunc_void_int_int_int((const func_t*)find_scheme_value(s7, funcname), arg1, arg2, arg3);
}

void s7extra_callFunc_void_int_int_int_int(const func_t *func, int64_t arg1, int64_t arg2, int64_t arg3, int64_t arg4){
  ScopedEvalTracker eval_tracker;
  
  catch_call(s7,
             S7_LIST(s7,
                        (s7_pointer)func,
                        s7_make_integer(s7, arg1),
                        s7_make_integer(s7, arg2),
                        s7_make_integer(s7, arg3),
                        s7_make_integer(s7, arg4)
                        )
             );
}

void s7extra_callFunc2_void_int_int_int_int(const char *funcname, int64_t arg1, int64_t arg2, int64_t arg3, int64_t arg4){
  s7extra_callFunc_void_int_int_int_int((const func_t*)find_scheme_value(s7, funcname), arg1, arg2, arg3, arg4);
}

void s7extra_callFunc_void_int_dyn(const func_t *func, int64_t arg1, const dyn_t arg2){
  ScopedEvalTracker eval_tracker;
  
  catch_call(s7,
             S7_LIST(s7,
                        (s7_pointer)func,
                        eval_tracker.p(s7_make_integer(s7, arg1)),
                        eval_tracker.p(s7extra_make_dyn(s7, arg2))
                        )
             );
}

void s7extra_callFunc2_void_int_dyn(const char *funcname, int64_t arg1, const dyn_t arg2){
  s7extra_callFunc_void_int_dyn((const func_t*)find_scheme_value(s7, funcname), arg1, arg2);
}

void s7extra_callFunc_void_int_float_float(const func_t *func, int64_t arg1, double arg2, double arg3){
  ScopedEvalTracker eval_tracker;
  
  catch_call(s7,
             S7_LIST(s7,
                        (s7_pointer)func,
                        s7_make_integer(s7, arg1),
                        s7extra_make_real(s7, arg2),
                        s7extra_make_real(s7, arg3)
                        )
             );
}

void s7extra_callFunc2_void_int_float_float(const char *funcname, int64_t arg1, double arg2, double arg3){
  s7extra_callFunc_void_int_float_float((const func_t*)find_scheme_value(s7, funcname), arg1, arg2, arg3);
}

void s7extra_callFunc_void_int_float_float_float_float(const func_t *func, int64_t arg1, double arg2, double arg3, double arg4, double arg5){
  ScopedEvalTracker eval_tracker;
  
  catch_call(s7,
             S7_LIST(s7,
                        (s7_pointer)func,
                        s7_make_integer(s7, arg1),
                        s7extra_make_real(s7, arg2),
                        s7extra_make_real(s7, arg3),
                        s7extra_make_real(s7, arg4),
                        s7extra_make_real(s7, arg5)
                        )
             );
}

void s7extra_callFunc2_void_int_float_float_float_float(const char *funcname, int64_t arg1, double arg2, double arg3, double arg4, double arg5){
  s7extra_callFunc_void_int_float_float_float_float((const func_t*)find_scheme_value(s7, funcname), arg1, arg2, arg3, arg4, arg5);
}

void s7extra_callFunc_void_int_int_float_float(const func_t *func, int64_t arg1, int64_t arg2, double arg3, double arg4){
  ScopedEvalTracker eval_tracker;
  
  catch_call(s7,
             S7_LIST(s7,
                        (s7_pointer)func,
                        s7_make_integer(s7, arg1),
                        s7_make_integer(s7, arg2),
                        s7extra_make_real(s7, arg3),
                        s7extra_make_real(s7, arg4)
                        )
             );
}

void s7extra_callFunc2_void_int_int_float_float(const char *funcname, int64_t arg1, int64_t arg2, double arg3, double arg4){
  s7extra_callFunc_void_int_int_float_float((const func_t*)find_scheme_value(s7, funcname), arg1, arg2, arg3, arg4);
}

bool s7extra_callFunc_bool_int_int_float_float(const func_t *func, int64_t arg1, int64_t arg2, double arg3, double arg4){
  ScopedEvalTracker eval_tracker;
  
  s7_pointer ret = catch_call(s7,
                              S7_LIST(s7,
                                         (s7_pointer)func,
                                         s7_make_integer(s7, arg1),
                                         s7_make_integer(s7, arg2),
                                         s7extra_make_real(s7, arg3),
                                         s7extra_make_real(s7, arg4)
                                         )
                              );
  if(!s7_is_boolean(ret)){
    //SCHEME_eval("(gakkagkakags)");
    handleError("Callback did not return a boolean");
    return false;
  }else{
    return s7_boolean(s7, ret);
  }
}

bool s7extra_callFunc2_bool_int_int_float_float(const char *funcname, int64_t arg1, int64_t arg2, double arg3, double arg4){
  return s7extra_callFunc_bool_int_int_float_float((const func_t*)find_scheme_value(s7, funcname), arg1, arg2, arg3, arg4);
}

bool s7extra_callFunc_bool_int_int_int_int(const func_t *func, int64_t arg1, int64_t arg2, int arg3, int arg4){
  ScopedEvalTracker eval_tracker;
  
  s7_pointer ret = catch_call(s7,
                              S7_LIST(s7,
                                         (s7_pointer)func,
                                         s7_make_integer(s7, arg1),
                                         s7_make_integer(s7, arg2),
                                         s7_make_integer(s7, arg3),
                                         s7_make_integer(s7, arg4)
                                         )
                              );
  if(!s7_is_boolean(ret)){
    //SCHEME_eval("(gakkagkakags)");
    handleError("Callback did not return a boolean");
    return false;
  }else{
    return s7_boolean(s7, ret);
  }
}

bool s7extra_callFunc2_bool_int_int_int_int(const char *funcname, int64_t arg1, int64_t arg2, int arg3, int arg4){
  return s7extra_callFunc_bool_int_int_int_int((const func_t*)find_scheme_value(s7, funcname), arg1, arg2, arg3, arg4);
}

bool s7extra_callFunc_bool_int_int_int(const func_t *func, int64_t arg1, int64_t arg2, int arg3){
  ScopedEvalTracker eval_tracker;
  
  s7_pointer ret = catch_call(s7,
                              S7_LIST(s7,
                                         (s7_pointer)func,
                                         s7_make_integer(s7, arg1),
                                         s7_make_integer(s7, arg2),
                                         s7_make_integer(s7, arg3)
                                         )
                              );
  if(!s7_is_boolean(ret)){
    //SCHEME_eval("(gakkagkakags)");
    handleError("Callback did not return a boolean");
    return false;
  }else{
    return s7_boolean(s7, ret);
  }
}

bool s7extra_callFunc2_bool_int_int_int(const char *funcname, int64_t arg1, int64_t arg2, int arg3){
  return s7extra_callFunc_bool_int_int_int((const func_t*)find_scheme_value(s7, funcname), arg1, arg2, arg3);
}

bool s7extra_callFunc_bool_int_float_float(const func_t *func, int64_t arg1, double arg2, double arg3){
  ScopedEvalTracker eval_tracker;
  
  s7_pointer ret = catch_call(s7,
                              S7_LIST(s7,
                                         (s7_pointer)func,
                                         s7_make_integer(s7, arg1),
                                         s7extra_make_real(s7, arg2),
                                         s7extra_make_real(s7, arg3)
                                         )
                              );
  if(!s7_is_boolean(ret)){
    handleError("Callback did not return a boolean");
    return false;
  }else{
    return s7_boolean(s7, ret);
  }
}

bool s7extra_callFunc2_bool_int_float_float(const char *funcname, int64_t arg1, double arg2, double arg3){
  return s7extra_callFunc_bool_int_float_float((const func_t*)find_scheme_value(s7, funcname), arg1, arg2, arg3);
}

bool s7extra_callFunc_bool_bool(const func_t *func, bool arg1){
  ScopedEvalTracker eval_tracker;
  
  s7_pointer ret = catch_call(s7,
                              S7_LIST(s7,
                                         (s7_pointer)func,
                                         s7_make_boolean(s7, arg1)
                                         )
                              );
  if(!s7_is_boolean(ret)){
    handleError("Callback did not return a boolean");
    return -1;
  }else{
    return s7_boolean(s7, ret);
  }
}

bool s7extra_callFunc2_bool_bool(const char *funcname, bool arg1){
  return s7extra_callFunc_bool_bool((const func_t*)find_scheme_value(s7, funcname), arg1);
}

bool s7extra_callFunc_bool_bool_float_float(const func_t *func, bool arg1, double arg2, double arg3){
  ScopedEvalTracker eval_tracker;
  
  s7_pointer ret = catch_call(s7,
                              S7_LIST(s7,
                                         (s7_pointer)func,
                                         s7_make_boolean(s7, arg1),
                                         s7extra_make_real(s7, arg2),
                                         s7extra_make_real(s7, arg3)
                                         )
                              );
  if(!s7_is_boolean(ret)){
    handleError("Callback did not return a boolean");
    return false;
  }else{
    return s7_boolean(s7, ret);
  }
}

bool s7extra_callFunc2_bool_bool_float_float(const char *funcname, bool arg1, double arg2, double arg3){
  return s7extra_callFunc_bool_bool_float_float((const func_t*)find_scheme_value(s7, funcname), arg1, arg2, arg3);
}

bool s7extra_callFunc_bool_bool_dyn(const func_t *func, bool arg1, dyn_t arg2){
  ScopedEvalTracker eval_tracker;
  
  s7_pointer ret = catch_call(s7,
                              S7_LIST(s7,
                                         (s7_pointer)func,
                                         eval_tracker.p(s7_make_boolean(s7, arg1)),
                                         eval_tracker.p(s7extra_make_dyn(s7, arg2))
                                         )
                              );
  if(!s7_is_boolean(ret)){
    handleError("Callback did not return a boolean");
    return false;
  }else{
    return s7_boolean(s7, ret);
  }
}

bool s7extra_callFunc2_bool_bool_dyn(const char *funcname, bool arg1, dyn_t arg2){
  return s7extra_callFunc_bool_bool_dyn((const func_t*)find_scheme_value(s7, funcname), arg1, arg2);
}

bool s7extra_callFunc_bool_dyn(const func_t *func, dyn_t arg1){
  ScopedEvalTracker eval_tracker;
  
  s7_pointer ret = catch_call(s7,
                              S7_LIST(s7,
                                         (s7_pointer)func,
                                         eval_tracker.p(s7extra_make_dyn(s7, arg1))
                                         )
                              );
  if(!s7_is_boolean(ret)){
    handleError("Callback did not return a boolean");
    return false;
  }else{
    return s7_boolean(s7, ret);
  }
}

bool s7extra_callFunc2_bool_dyn(const char *funcname, dyn_t arg1){
  return s7extra_callFunc_bool_dyn((const func_t*)find_scheme_value(s7, funcname), arg1);
}


bool s7extra_callFunc_bool_bool_charpointer_charpointer_dyn(const func_t *func, bool arg1, const char* arg2, const char* arg3, dyn_t arg4){
  ScopedEvalTracker eval_tracker;
  
  s7_pointer ret = catch_call(s7,
                              S7_LIST(s7,
                                         (s7_pointer)func,
                                         eval_tracker.p(s7_make_boolean(s7, arg1)),
                                         eval_tracker.p(s7_make_string(s7, arg2)),
                                         eval_tracker.p(s7_make_string(s7, arg3)),
                                         eval_tracker.p(s7extra_make_dyn(s7, arg4))
                                         )
                              );
  if(!s7_is_boolean(ret)){
    handleError("Callback did not return a boolean");
    return false;
  }else{
    return s7_boolean(s7, ret);
  }
}

bool s7extra_callFunc2_bool_bool_charpointer_charpointer_dyn(const char *funcname, bool arg1, const char* arg2, const char* arg3, dyn_t arg4){
  return s7extra_callFunc_bool_bool_charpointer_charpointer_dyn((const func_t*)find_scheme_value(s7, funcname), arg1, arg2, arg3, arg4);
}

int64_t s7extra_callFunc_int_void(const func_t *func){
  ScopedEvalTracker eval_tracker;
  
  s7_pointer ret = catch_call(s7,
                              S7_LIST(s7,
                                         (s7_pointer)func
                                       )
                              );
  if(!s7_is_integer(ret)){
    handleError("Callback did not return an integer");
    return -1;
  }else{
    return s7_integer(ret);
  }
}

int64_t s7extra_callFunc2_int_void(const char *funcname){
  return s7extra_callFunc_int_void((const func_t*)find_scheme_value(s7, funcname));
}

int64_t s7extra_callFunc_int_int(const func_t *func, int64_t arg1){
  ScopedEvalTracker eval_tracker;
  
  s7_pointer ret = catch_call(s7,
                              S7_LIST(s7,
                                       (s7_pointer)func,
                                       s7_make_integer(s7, arg1)
                                       )
                              );
  if(!s7_is_integer(ret)){
    handleError("Callback did not return an integer");
    return -1;
  }else{
    return s7_integer(ret);
  }
}

int64_t s7extra_callFunc2_int_int(const char *funcname, int64_t arg1){
  return s7extra_callFunc_int_int((const func_t*)find_scheme_value(s7, funcname), arg1);
}

int64_t s7extra_callFunc_int_instrument(const func_t *func, instrument_t arg1){
  ScopedEvalTracker eval_tracker;
  
  s7_pointer ret = catch_call(s7,
                              S7_LIST(s7,
                                         (s7_pointer)func,
                                         eval_tracker.p(s7extra_make_instrument(s7, arg1))
                                         )
                              );
  if(!s7_is_integer(ret)){
    handleError("Callback did not return an integer");
    return -1;
  }else{
    return s7_integer(ret);
  }
}

int64_t s7extra_callFunc2_int_instrument(const char *funcname, instrument_t arg1){
  return s7extra_callFunc_int_instrument((const func_t*)find_scheme_value(s7, funcname), arg1);
}

void s7extra_callFunc_void_instrument(const func_t *func, instrument_t arg1){
  ScopedEvalTracker eval_tracker;
  
  catch_call(s7,
             S7_LIST(s7,
                        (s7_pointer)func,
                        eval_tracker.p(s7extra_make_instrument(s7, arg1))
                        )
             );
}

void s7extra_callFunc2_void_instrument(const char *funcname, instrument_t arg1){
  s7extra_callFunc_void_instrument((const func_t*)find_scheme_value(s7, funcname), arg1);
}

void s7extra_callFunc_void_instrument_int(const func_t *func, instrument_t arg1, int64_t arg2){
  ScopedEvalTracker eval_tracker;
    
  catch_call(s7,
             S7_LIST(s7,
                      (s7_pointer)func,
                      eval_tracker.p(s7extra_make_instrument(s7, arg1)),
                      eval_tracker.p(s7_make_integer(s7, arg2))
                      )
             );
}

void s7extra_callFunc2_void_instrument_int(const char *funcname, instrument_t arg1, int64_t arg2){
  s7extra_callFunc_void_instrument_int((const func_t*)find_scheme_value(s7, funcname), arg1, arg2);
}

int64_t s7extra_callFunc_int_int_int_int(const func_t *func, int64_t arg1, int64_t arg2, int64_t arg3){
  ScopedEvalTracker eval_tracker;
  
  s7_pointer ret = catch_call(s7,
                              S7_LIST(s7,
                                         (s7_pointer)func,
                                         s7_make_integer(s7, arg1),
                                         s7_make_integer(s7, arg2),
                                         s7_make_integer(s7, arg3)
                                         )
                              );
  if(!s7_is_integer(ret)){
    handleError("Callback did not return an integer");
    return -1;
  }else{
    return s7_integer(ret);
  }
}

int64_t s7extra_callFunc2_int_int_int_int(const char *funcname, int64_t arg1, int64_t arg2, int64_t arg3){
  return s7extra_callFunc_int_int_int_int((const func_t*)find_scheme_value(s7, funcname), arg1, arg2, arg3);
}

int64_t s7extra_callFunc_int_dyn_int_int(const func_t *func, dyn_t arg1, int64_t arg2, int64_t arg3){
  ScopedEvalTracker eval_tracker;
  
  s7_pointer ret = catch_call(s7,
                              S7_LIST(s7,
                                         (s7_pointer)func,
                                         eval_tracker.p(s7extra_make_dyn(s7, arg1)),
                                         eval_tracker.p(s7_make_integer(s7, arg2)),
                                         eval_tracker.p(s7_make_integer(s7, arg3))
                                         )
                              );
  if(!s7_is_integer(ret)){
    handleError("Callback did not return an integer");
    return -1;
  }else{
    return s7_integer(ret);
  }
}

int64_t s7extra_callFunc2_int_dyn_int_int(const char *funcname, dyn_t arg1, int64_t arg2, int64_t arg3){
  return s7extra_callFunc_int_dyn_int_int((const func_t*)find_scheme_value(s7, funcname), arg1, arg2, arg3);
}

int64_t s7extra_callFunc_int_instrument_int_int(const func_t *func, instrument_t arg1, int64_t arg2, int64_t arg3){
  ScopedEvalTracker eval_tracker;
  
  s7_pointer ret = catch_call(s7,
                              S7_LIST(s7,
                                         (s7_pointer)func,
                                         eval_tracker.p(s7extra_make_instrument(s7, arg1)),
                                         eval_tracker.p(s7_make_integer(s7, arg2)),
                                         eval_tracker.p(s7_make_integer(s7, arg3))
                                         )
                              );
  if(!s7_is_integer(ret)){
    handleError("Callback did not return an integer");
    return -1;
  }else{
    return s7_integer(ret);
  }
}

int64_t s7extra_callFunc2_int_instrument_int_int(const char *funcname, instrument_t arg1, int64_t arg2, int64_t arg3){
  return s7extra_callFunc_int_instrument_int_int((const func_t*)find_scheme_value(s7, funcname), arg1, arg2, arg3);
}

int64_t s7extra_callFunc_int_int_int_int_bool(const func_t *func, int64_t arg1, int64_t arg2, int64_t arg3, bool arg4){
  ScopedEvalTracker eval_tracker;
  
  s7_pointer ret = catch_call(s7,
                              S7_LIST(s7,
                                         (s7_pointer)func,
                                         s7_make_integer(s7, arg1),
                                         s7_make_integer(s7, arg2),
                                         s7_make_integer(s7, arg3),
                                         s7_make_boolean(s7, arg4)
                                   )
                              );
  if(!s7_is_integer(ret)){
    handleError("Callback did not return an integer");
    return -1;
  }else{
    return s7_integer(ret);
  }
}

int64_t s7extra_callFunc2_int_int_int_int_bool(const char *funcname, int64_t arg1, int64_t arg2, int64_t arg3, bool arg4){
  return s7extra_callFunc_int_int_int_int_bool((const func_t*)find_scheme_value(s7, funcname), arg1, arg2, arg3, arg4);
}

int64_t s7extra_callFunc_int_int_dyn(const func_t *func, int64_t arg1, const dyn_t arg2){
  ScopedEvalTracker eval_tracker;
  
  s7_pointer ret = catch_call(s7,
                              S7_LIST(s7,
                                         (s7_pointer)func,
                                         eval_tracker.p(s7_make_integer(s7, arg1)),
                                         eval_tracker.p(s7extra_make_dyn(s7, arg2))
                                         )
                              );
  if(!s7_is_integer(ret)){
    handleError("Callback did not return an integer");
    return -1;
  }else{
    return s7_integer(ret);
  }
}

int64_t s7extra_callFunc2_int_int_dyn(const char *funcname, int64_t arg1, const dyn_t arg2){
  return s7extra_callFunc_int_int_dyn((const func_t*)find_scheme_value(s7, funcname), arg1, arg2);
}

int64_t s7extra_callFunc_int_int_int_dyn(const func_t *func, int64_t arg1, int64_t arg2, const dyn_t arg3){
  ScopedEvalTracker eval_tracker;
  
  s7_pointer ret = catch_call(s7,
                              S7_LIST(s7,
                                         (s7_pointer)func,
                                         eval_tracker.p(s7_make_integer(s7, arg1)),
                                         eval_tracker.p(s7_make_integer(s7, arg2)),
                                         eval_tracker.p(s7extra_make_dyn(s7, arg3))
                                         )
                              );
  if(!s7_is_integer(ret)){
    handleError("Callback did not return an integer");
    return -1;
  }else{
    return s7_integer(ret);
  }
}

int64_t s7extra_callFunc2_int_int_int_dyn(const char *funcname, int64_t arg1, int64_t arg2, const dyn_t arg3){
  return s7extra_callFunc_int_int_int_dyn((const func_t*)find_scheme_value(s7, funcname), arg1, arg2, arg3);
}

int64_t s7extra_callFunc_int_int_dyn_dyn(const func_t *func, int64_t arg1, const dyn_t arg2, const dyn_t arg3){
  ScopedEvalTracker eval_tracker;
  
  s7_pointer ret = catch_call(s7,
                              S7_LIST(s7,
                                         (s7_pointer)func,
                                         eval_tracker.p(s7_make_integer(s7, arg1)),
                                         eval_tracker.p(s7extra_make_dyn(s7, arg2)),
                                         eval_tracker.p(s7extra_make_dyn(s7, arg3))
                                         )
                              );
  if(!s7_is_integer(ret)){
    handleError("Callback did not return an integer");
    return -1;
  }else{
    return s7_integer(ret);
  }
}

int64_t s7extra_callFunc2_int_int_dyn_dyn(const char *funcname, int64_t arg1, const dyn_t arg2, const dyn_t arg3){
  return s7extra_callFunc_int_int_dyn_dyn((const func_t*)find_scheme_value(s7, funcname), arg1, arg2, arg3);
}

int64_t s7extra_callFunc_int_charpointer_charpointer(const func_t *func, const char *arg1, const char *arg2){
  ScopedEvalTracker eval_tracker;
  
  s7_pointer ret = catch_call(s7,
                              S7_LIST(s7,
                                         (s7_pointer)func,
                                         s7_make_string(s7, arg1),
                                         s7_make_string(s7, arg2)
                                         )
                              );
  if(!s7_is_integer(ret)){
    handleError("Callback did not return an integer");
    return -1;
  }else{
    return s7_integer(ret);
  }
}

int64_t s7extra_callFunc2_int_charpointer_charpointer(const char *funcname, const char *arg1, const char *arg2){
  return s7extra_callFunc_int_charpointer_charpointer((const func_t*)find_scheme_value(s7, funcname), arg1, arg2);
}

int64_t s7extra_callFunc_int_int_charpointer_charpointer_int_bool_bool_bool_bool_bool(const func_t *func, int64_t arg0, const char *arg1, const char *arg2, int arg3, bool arg4, bool arg5, bool arg6, bool arg7, bool arg8){
  ScopedEvalTracker eval_tracker;
  
  s7_pointer ret = catch_call(s7,
                              S7_LIST(s7,
                                         (s7_pointer)func,
                                         s7_make_integer(s7, arg0),
                                         s7_make_string(s7, arg1),
                                         s7_make_string(s7, arg2),
                                         s7_make_integer(s7, arg3),
                                         s7_make_boolean(s7, arg4),
                                         s7_make_boolean(s7, arg5),
                                         s7_make_boolean(s7, arg6),
                                         s7_make_boolean(s7, arg7),
                                         s7_make_boolean(s7, arg8)
                                         )
                              );
  if(!s7_is_integer(ret)){
    handleError("Callback did not return an integer");
    return -1;
  }else{
    return s7_integer(ret);
  }
}

int64_t s7extra_callFunc2_int_int_charpointer_charpointer_int_bool_bool_bool_bool_bool(const char *funcname, int64_t arg0, const char *arg1, const char *arg2, int arg3, bool arg4, bool arg5, bool arg6, bool arg7, bool arg8){
  return s7extra_callFunc_int_int_charpointer_charpointer_int_bool_bool_bool_bool_bool((const func_t*)find_scheme_value(s7, funcname), arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8);
}

const char *s7extra_callFunc_charpointer_dyn(const func_t *func, const dyn_t arg1){
  ScopedEvalTracker eval_tracker;
  
  s7_pointer ret = catch_call(s7,
                              S7_LIST(s7,
                                       (s7_pointer)func,
                                       eval_tracker.p(s7extra_make_dyn(s7, arg1))
                                       )
                              );
  if(!s7_is_string(ret)){
    handleError("Callback did not return a string");
    return "";
  }else{
    return talloc_strdup(s7_string(ret));
  }
}

const char *s7extra_callFunc2_charpointer_dyn(const char *funcname, const dyn_t arg1){
  return s7extra_callFunc_charpointer_dyn((const func_t*)find_scheme_value(s7, funcname), arg1);
}

const char *s7extra_callFunc_charpointer_charpointer_dyn(const func_t *func, const char* arg1, const dyn_t arg2){
  ScopedEvalTracker eval_tracker;
  
  s7_pointer ret = catch_call(s7,
                              S7_LIST(s7,
                                         (s7_pointer)func,
                                         eval_tracker.p(s7_make_string(s7, arg1)),
                                         eval_tracker.p(s7extra_make_dyn(s7, arg2))
                                         )
                              );
  if(!s7_is_string(ret)){
    handleError("Callback did not return a string");
    return "";
  }else{
    return talloc_strdup(s7_string(ret));
  }
}

const char *s7extra_callFunc2_charpointer_charpointer_dyn(const char *funcname, const char* arg1, const dyn_t arg2){
  return s7extra_callFunc_charpointer_charpointer_dyn((const func_t*)find_scheme_value(s7, funcname), arg1, arg2);
}

//static int g_num_protected = 0;

#if !defined(RELEASE)
static QHash<int64_t, void*> g_used_pos;
#endif

int64_t s7extra_protect(void *v){
  int64_t ret = s7_gc_protect(s7, (s7_pointer)v);

#if !defined(RELEASE)
  R_ASSERT(g_used_pos.contains(ret)==false);
  g_used_pos[ret] = v;
#endif
  
  //printf("           s7extra_protect %p. pos: %d\n", v, (int)ret);
  //printf("            Protect: %d\n", g_num_protected++);
  return ret;
}

/*
It's dangerous to mix s7_gc_unprotect and s7_g_unprotect_at
void s7extra_unprotect(void *v){
  printf("             1. s7extra unprotecting %p\n", v);
  s7_gc_unprotect(s7, (s7_pointer)v);  
}
*/

extern bool g_program_has_ended;

void s7extra_unprotect(void *v, int64_t pos){
#if !defined(RELEASE)
  //printf("             2. s7extra unprotecting2 %p. pos: %d\n", v, (int)pos);
  if (g_program_has_ended==false){ // Can't access Qt stuff after program has ended since various destructors could have run then.
    R_ASSERT(g_used_pos.contains(pos)==true);
    if (g_used_pos[pos] !=v ){
      printf("   POS: %d. Actual: %p. Supposed: %p\n", (int)pos, g_used_pos[pos], v);
      abort();
    }
    g_used_pos.remove(pos);
  }
#endif
  
  R_ASSERT_RETURN_IF_FALSE(s7_gc_protected_at(s7, pos)==v);
  s7_gc_unprotect_at(s7, pos);
  //s7_gc_safe_unprotect_at(s7, (s7_pointer)v, pos);

  //printf("            Unprotect: %d\n", --g_num_protected);
}

void s7extra_disable_history(void){
  //printf("\n\n\n\n =================================== >>>>>>>> DISABLE: %d (enabled: %d) ===============================\n\n\n", g_s7_history_disabled, s7_history_enabled(s7));
  g_s7_history_disabled++;
  
  if(g_s7_history_disabled==1){
    R_ASSERT_NON_RELEASE(s7_history_enabled(s7)==true);
    //printf("\n\n\n\n ========================================================= Disabling history\n");
    s7_set_history_enabled(s7, false);
  }else{
    R_ASSERT_NON_RELEASE(s7_history_enabled(s7)==false);
  }
}
  
void s7extra_enable_history(void){
  //printf("\n\n\n\n =================================== <<< ENABLE: %d (enabled: %d) ===============================\n\n\n", g_s7_history_disabled, s7_history_enabled(s7));
  
  R_ASSERT_RETURN_IF_FALSE(g_s7_history_disabled > 0);
  g_s7_history_disabled--;

  R_ASSERT_NON_RELEASE(s7_history_enabled(s7)==false);

  if(g_s7_history_disabled == 0){
    //printf("\n\n\n\n ==================================================== Enabling history\n");
    s7_set_history_enabled(s7, true);
  }
}

bool s7extra_is_defined(const char* funcname){
  return s7_is_defined(s7, funcname);
}

int placetest(Place dasplacevar,int windownum){
  return dasplacevar.line;
}

Place placetest2(int a, int b, int c){
  Place p = {a,(uint_32)b,(uint_32)c};
  return p;
}


// Warning: No protection against scheme throwing an error. The function MUST not throw an error.
Place p_Scale(const Place x, const Place x1, const Place x2, const Place y1, const Place y2) {
  ScopedEvalTracker eval_tracker;
  
  static s7_pointer scheme_func = find_and_protect_scheme_value("safe-scale");

  s7_pointer result = s7_call(s7,
                              scheme_func,
                              S7_LIST(s7,
                                         place_to_ratio(x),
                                         place_to_ratio(x1),
                                         place_to_ratio(x2),
                                         place_to_ratio(y1),
                                         place_to_ratio(y2)
                                         )
                              );

  if (s7_is_ratio(result))
    return p_ratio_to_place(result);
  else if (s7_is_integer(result))
    return p_Create((int)s7_integer(result), 0, 1);
  else if (s7_is_real(result)) {
    RError("p_Scale: result was a real (strange): %f (%s %s %s %s %s)",s7_real(result),p_ToString(x),p_ToString(x1),p_ToString(x2),p_ToString(y1),p_ToString(y2));
    return p_FromDouble(s7_real(result));
  } else {
    RError("result was not ratio or integer. Returning 0 (%s %s %s %s %s)",p_ToString(x),p_ToString(x1),p_ToString(x2),p_ToString(y1),p_ToString(y2));
    return p_Create(0,0,1);
  }
}

bool quantitize_note(const struct Blocks *block, struct Notes *note, bool *was_changed) {
  ScopedEvalTracker eval_tracker;

  *was_changed = false;

  s7_pointer scheme_func = find_scheme_value(s7, "quantitize-note");
  
  Place last_place = p_Last_Pos(block);

  s7extra_add_history(__func__, CR_FORMATEVENT("========== s7callling quantitize-note"));

  Ratio quant =
    make_ratio_from_static_ratio(root->quantitize_options.quant)
    /
    DYN_get_ratio(getLineZoomBlockRatio(block->l.num, -1));

  s7_pointer result = catch_call(s7,
                                 S7_LIST(s7,
                                            scheme_func,
                                            place_to_ratio(&note->l.p),
                                            s7_make_ratio(s7, note->end.num, note->end.den),
                                            s7_make_ratio(s7, quant.num, quant.den),
                                            place_to_ratio(&last_place),
                                            s7_make_integer(s7, root->quantitize_options.type)
                                            )
                                 );
  
  if (s7_is_boolean(result))
    return false;

  if (!s7_is_pair(result)) {
    RError("Unknown result after call to quantitize-note");
    return false;
  }

  Place new_start = number_to_place(s7, s7_car(result), NULL);
  Ratio new_end = place2ratio(number_to_place(s7, s7_cdr(result), NULL));

  if (!p_Equal(new_start, note->l.p)){
    note->l.p = new_start;
    *was_changed = true;
  }

  if (note->end != new_end) {
    note->end = new_end;
    *was_changed = true;
  }

  return true;
}


// Warning: No protection against scheme throwing an error. The function MUST not throw an error.
static void place_operation_void_p1_p2(s7_pointer scheme_func, Place *p1,  const Place *p2){
  ScopedEvalTracker eval_tracker;
  
  R_ASSERT_RETURN_IF_FALSE(p1->dividor > 0);
  R_ASSERT_RETURN_IF_FALSE(p2->dividor > 0);
  
  s7_pointer result = s7_call(s7,
                              scheme_func,
                              S7_LIST(s7,
                                         place_to_ratio(p1),
                                         place_to_ratio(p2)
                                         )
                              );
  
  if (s7_is_ratio(result))
    PlaceCopy(p1, ratio_to_place(result));
  else if (s7_is_integer(result))
    PlaceCopy(p1, PlaceCreate((int)s7_integer(result), 0, 1));
  else if (s7_is_real(result)) {
    RError("result was a real (strange): %f (%s %s)",s7_real(result),PlaceToString(p1),PlaceToString(p2));
    Double2Placement(s7_real(result), p1);
  }else {
    RError("result was not ratio or integer. Returning 0 (%s %s)",PlaceToString(p1),PlaceToString(p2));
    PlaceCopy(p1, PlaceCreate(0,0,1));
  }
}
                            
void PlaceAdd(Place *p1,  const Place *p2){
  static s7_pointer scheme_func = find_and_protect_scheme_value("+");
  place_operation_void_p1_p2(scheme_func, p1,p2);
}

void PlaceSub(Place *p1,  const Place *p2){
  static s7_pointer scheme_func = find_and_protect_scheme_value("-");
  place_operation_void_p1_p2(scheme_func, p1,p2);
}

void PlaceMul(Place *p1,  const Place *p2){
  static s7_pointer scheme_func = find_and_protect_scheme_value("*");
  place_operation_void_p1_p2(scheme_func, p1,p2);
}

void PlaceDiv(Place *p1,  const Place *p2){
  R_ASSERT_RETURN_IF_FALSE(p2->line!=0 || p2->counter!=0); // There is no protection if scheme fails here, so we must make sure the arguments can't cause scheme to throw an error.
  static s7_pointer scheme_func = find_and_protect_scheme_value("/");
  place_operation_void_p1_p2(scheme_func, p1,p2);
}


// Warning: No protection against scheme throwing an error. The function MUST not throw an error.
static Place place_operation_place_p1_p2(s7_pointer scheme_func, const Place p1,  const Place p2){
  ScopedEvalTracker eval_tracker;
  
  s7_pointer result = s7_call(s7,
                              scheme_func,
                              S7_LIST(s7,
                                         place_to_ratio(&p1),
                                         place_to_ratio(&p2)
                                         )
                              );

  return number_to_place(s7, result, NULL);
}

Place p_Add(const Place p1, const Place p2){
  //PrintPlace("p1: ",&p1);
  //PrintPlace("p2: ",&p2);
  static s7_pointer scheme_func = find_and_protect_scheme_value("+");
  return place_operation_place_p1_p2(scheme_func, p1,p2);
}

Place p_Sub(const Place p1, const Place p2){
  static s7_pointer scheme_func = find_and_protect_scheme_value("-");
  return place_operation_place_p1_p2(scheme_func, p1,p2);
}

Place p_Mul(const Place p1, const Place p2){
  static s7_pointer scheme_func = find_and_protect_scheme_value("*");
  return place_operation_place_p1_p2(scheme_func, p1,p2);
}

Place p_Div(const Place p1, const Place p2){
  R_ASSERT_RETURN_IF_FALSE2(p2.line!=0 || p2.counter!=0, p_Create(0,0,1)); // There is no protection if scheme fails here, so we must make sure the arguments can't cause scheme to throw an error.
  
  static s7_pointer scheme_func = find_and_protect_scheme_value("/");
  return place_operation_place_p1_p2(scheme_func, p1,p2);
}

Place p_Quantitize(const Place p, const Place q){
  R_ASSERT_RETURN_IF_FALSE2(q.line!=0 || q.counter!=0, p); // There is no protection if scheme fails here, so we must make sure the arguments can't cause scheme to throw an error.
    
  /*
    We can probably assume that 'quantitize' will never fail:

  (define (roundup A)
  (floor (+ A 0.5)))

  (define (quantitize Place Q)
    (* (roundup (/ Place Q))
       Q))

  I.e. no need to put it inside catch, add history, etc.
  */
  
  static s7_pointer scheme_func = find_and_protect_scheme_value("quantitize");
  return place_operation_place_p1_p2(scheme_func, p, q);
}


void SCHEME_throw_catch(const char *symbol, const char *message){
  ScopedProtect scoped_protect;
  //SCHEME_eval("(safe-display-history-ow!)");
  //return;
    
  static s7_pointer scheme_func = find_and_protect_scheme_value("error");

  s7extra_disable_history();
  
  catch_call(s7,
             S7_LIST(s7,
                        scheme_func,
                        s7_make_symbol(s7, symbol),
                        S7_LIST(s7,
                                scoped_protect.p(s7_make_string(s7, message))
                                )
                      )
             );

  s7extra_enable_history();
}

void SCHEME_throw(const char *symbol, const char *message){
  ScopedProtect scoped_protect;
  
  if (g_scheme_nested_level<=0){
    printf("   ERROR: g_scheme_nested_level<=0: %d\n", g_scheme_nested_level);
    R_ASSERT(g_scheme_nested_level==0);
    R_ASSERT_NON_RELEASE(false);
    return;
  }
                       
  //printf("SCHEME_THROW. Message: \"%s\"\n", message);
  //if(g_evals>0){
  s7_error(s7,
           s7_make_symbol(s7, symbol),
           S7_LIST(s7,
                   scoped_protect.p(s7_make_string(s7, message))
                   )
           );
    //}
}


const char *SCHEME_get_history(void){
  ScopedEvalTracker eval_tracker;
  
  //SCHEME_eval("(throw \'get-backtrace)"); // Fill in error-lines and so forth into s7. (no, then we risk longjmp a place we don't want to longjmp. We don't want to longjmp at all actually.)

  if (s7 == NULL)
    return strdup("");
  
  const char *funcname = "safe-history-ow!";
  
  if (!s7_is_defined(s7, funcname))
    return strdup("");

  s7extra_disable_history();

  s7_pointer s7s = catch_call(s7,
                              S7_LIST(s7,
                                       find_scheme_value(s7, funcname)
                                       )
                              );

  s7extra_enable_history();

  if (!s7_is_string(s7s))
    return strdup("...Unable to run 'safe-history-ow!'...");
  
  return strdup(s7_string(s7s));
}


bool SCHEME_mousepress(int button, float x, float y){
  ScopedEvalTracker eval_tracker;
  
  tevent.x  = x;
  tevent.y  = y;

  if (g_scheme_nested_level > 0){
#if !defined(RELEASE)
    printf("cancel call to \"radium-mouse-press\" since other scheme code is running\n");
#endif
    return false;
  }
  
  return S7CALL2(bool_int_float_float,"radium-mouse-press", // [1]
                 button,x,y);
  /*
  return s7_boolean(s7,
                    s7_call(s7, 
                            find_scheme_value(s7, "radium-mouse-press"), // [1]
                            s7_list(s7,
                                    3,
                                    s7_make_integer(s7, button),
                                    s7extra_make_real(s7, x),
                                    s7extra_make_real(s7, y)
                                    )
                            )
                    );
  */
  // [1] Not storing/reusing this value since 'find_scheme_value' is probably ligthing fast anyway, plus that it'll be possible to redefine radium-mouse-press from scheme this way.
}

bool SCHEME_mousemove(int button, float x, float y){
  ScopedEvalTracker eval_tracker;
  
  tevent.x  = x;
  tevent.y  = y;
 
  if (g_scheme_nested_level > 0){
#if !defined(RELEASE)
    printf("cancel call to \"radium-mouse-move\" since other scheme code is running\n");
#endif
    return false;
  }
  
 return S7CALL2(bool_int_float_float,"radium-mouse-move", // [1]
                 button,x,y);
  
  // [1] Not storing/reusing this value since 'find_scheme_value' is probably ligthing fast anyway, plus that it'll be possible to redefine radium-mouse-press from scheme this way.
}

bool SCHEME_mouserelease(int button, float x, float y){
  ScopedEvalTracker eval_tracker;
  
  tevent.x  = x;
  tevent.y  = y;

  if (g_scheme_nested_level > 0){
#if !defined(RELEASE)
    printf("cancel call to \"radium-mouse-release\" since other scheme code is running\n");
#endif
    return false;
  }

  return S7CALL2(bool_int_float_float,"radium-mouse-release", // [1]
                 button,x,y);
  
  // [1] Not storing/reusing this value since 'find_scheme_value' is probably ligthing fast anyway, plus that it'll be possible to redefine radium-mouse-press from scheme this way.
}

dyn_t SCHEME_eval_withreturn(const char *code){
  ScopedEvalTracker eval_tracker;

  if(errorCheckEvalScheme())
    return S7CALL2(dyn_charpointer,"mylint-and-eval-string",code);
  else
    return S7CALL2(dyn_charpointer,"eval-string",code);
}

// called from s7webserver.
s7_pointer RADIUM_SCHEME_eval2(const char *code){
  ScopedProtect scoped_protect;
  
  s7extra_disable_history();{
    SCHEME_eval("(if *message-gui* (<gui> :hide *message-gui*))");   // For developing.
  }s7extra_enable_history();
  
  s7extra_add_history(__func__, CR_FORMATEVENT("========== RADIUM_SCHEME_eval2 (Code from s7webserver.)\n\n"));
  
  s7_pointer ret = catch_call(s7,
                              S7_LIST(s7,
                                      scoped_protect.p(find_scheme_value(s7, errorCheckEvalScheme() ? "mylint-and-eval-string" : "eval-string")),
                                      scoped_protect.p(s7_make_string(s7, code)),
                                      s7_rootlet(s7) // Eval in global environment
                                      )
                              );
  
#if !defined(RELEASE)
  GFX_clear_menu_cache();
#endif
  
  return ret;
}

void SCHEME_eval(const char *code){
  ScopedEvalTracker eval_tracker;

  if(errorCheckEvalScheme())
    S7CALL2(void_charpointer,"mylint-and-eval-string",code);
  else
    S7CALL2(void_charpointer,"eval-string",code);
}

int SCHEME_get_webserver_port(void){
  return s7webserver_get_portnumber(s7webserver);
}

static s7_pointer getHistory(s7_scheme *radiums7_sc, s7_pointer radiums7_args){
  return s7_history(radiums7_sc);
}

void SCHEME_init1(void){
  ScopedEvalTracker eval_tracker;
  
  s7 = s7_init();
  if (s7==NULL) {
    RError("Can't start s7 scheme");
    exit(-1);
    return;
  }
  
  std::string os_path = ""; //OS_get_program_path() + OS_get_directory_separator();
  //printf("%s\n",os_path);

  s7_add_to_load_path(s7,(os_path+"packages"+OS_get_directory_separator()+"s7").c_str()); // bin/packages/s7 . No solution to utf-8 here. s7_add_to_load_path takes char* only.
  s7_add_to_load_path(s7,(os_path+"scheme").c_str()); // bin/scheme

  s7_define_function(s7, "s7:get-history", getHistory, 0, 0, false, "(s7:get-history)");

  init_radium_s7(s7);

  g_scheme_nested_level++;
  s7_load(s7,"init.scm");
  //s7_eval_c_string(s7, "(catch #t (lambda () (load \"init.scm\")) (lambda args (error 'ERROR-in-radium-init)))");
  g_scheme_nested_level--;
  R_ASSERT(g_scheme_nested_level==0);
  
  g_eat_errors_failed = find_and_protect_scheme_value("*eat-errors-failed-return-value*");

  s7webserver = s7webserver_create(s7, 5080, true);  
  s7webserver_set_verbose(s7webserver, true);
#if !defined(RELEASE)
  s7webserver_set_very_verbose(s7webserver, true);
#endif

  g_scheme_has_inited1 = true;
}

void SCHEME_init2(void){
  SCHEME_eval("(init-step-2)");
  g_scheme_has_inited2 = true;
}

