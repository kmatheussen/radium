/* Copyright 2012 Kjetil S. Matheussen

This program is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public License
as published by the Free Software Foundation; either version 2
of the License, or (at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA. */


#ifndef HASHMAP_PROC_H
#define HASHMAP_PROC_H

#include "OS_disk_proc.h"


// Warning: Writing two elements with the same key to the same hash table just adds two elements with the same key into the table.
// Nothing is ever deleted from the tables.

// A key can not contain lineshift since lineshifts are used as delimiter character when loading/saving.

extern LANGSPEC void HASH_clear(hash_t *hash);

extern LANGSPEC hash_t *HASH_create(int approx_size);

extern LANGSPEC bool HASH_equal(const hash_t *h1, const hash_t *h2);

// Can be used to make a copy of a hash which later could be HASH_clear-ed.
extern LANGSPEC hash_t *HASH_shallow_copy(const hash_t *hash);

// Can also be used to rehash
extern LANGSPEC hash_t *HASH_copy(const hash_t *hash);
#define HASH_REHASH(hash) do{hash = HASH_copy(hash);}while(0)

extern LANGSPEC const char *HASH_get_key(const hash_t *hash, const char *key); // Returns the stored pointed, if it needs to be reused.
extern LANGSPEC bool HASH_has_key(const hash_t *hash, const char *key);
extern LANGSPEC bool HASH_has_key_at(const hash_t *hash, const char *key, int i);

extern LANGSPEC int HASH_get_num_elements(const hash_t *hash);

extern LANGSPEC hash_t *HASH_get_keys_in_hash(const hash_t *hash); // Returns all keys in a new hash array.
extern LANGSPEC vector_t HASH_get_keys(const hash_t *hash);
extern LANGSPEC dynvec_t HASH_get_values(const hash_t *hash);

// HASH_put_*: char *key is not copied. I.e. the key is used directly, not a copy of it.
// HASH_put_string: A copy of char *val is used, not val itself.

extern LANGSPEC void HASH_put_dyn(hash_t *hash, const char *key, const dyn_t val);
extern LANGSPEC void HASH_put_string(hash_t *hash, const char *key, const wchar_t *val);
extern LANGSPEC void HASH_put_chars(hash_t *hash, const char *key, const char *val);
extern LANGSPEC void HASH_put_int(hash_t *hash, const char *key, int64_t val);
extern LANGSPEC void HASH_put_instrument(hash_t *hash, const char *key, instrument_t val);
extern LANGSPEC void HASH_put_filepath(hash_t *hash, const char *key, filepath_t val);
extern LANGSPEC void HASH_put_bool(hash_t *hash, const char *key, bool val);
extern LANGSPEC void HASH_put_float(hash_t *hash, const char *key, double val);
extern LANGSPEC void HASH_put_ratio(hash_t *hash, const char *key, const Ratio val);
extern LANGSPEC void HASH_put_hash(hash_t *hash, const char *key, hash_t *val);
extern LANGSPEC void HASH_put_array(hash_t *hash, const char *key, const dynvec_t dynvec);

#if USE_QT4
static inline void HASH_put_string(hash_t *hash, const char *key, const QString &val){
  HASH_put_string(hash, key, STRING_create(val));
}
#endif

extern LANGSPEC enum DynType HASH_get_type_at(const hash_t *hash, const char *raw_key, int i);
extern LANGSPEC enum DynType HASH_get_type(const hash_t *hash, const char *raw_key);

extern LANGSPEC dyn_t HASH_get_dyn(const hash_t *hash, const char *key);
extern LANGSPEC const wchar_t *HASH_get_string(const hash_t *hash, const char *key);
extern LANGSPEC const char *HASH_get_chars(const hash_t *hash, const char *key);
extern LANGSPEC int64_t HASH_get_int(const hash_t *hash, const char *key);
extern LANGSPEC instrument_t HASH_get_instrument(const hash_t *hash, const char *key);
extern LANGSPEC filepath_t HASH_get_filepath(const hash_t *hash, const char *key);
extern LANGSPEC bool HASH_get_bool(const hash_t *hash, const char *key);
extern LANGSPEC double HASH_get_float(const hash_t *hash, const char *key);
extern LANGSPEC hash_t *HASH_get_hash(const hash_t *hash, const char *key);
extern LANGSPEC double HASH_get_number(const hash_t *hash, const char *key); // Same as HASH_get_float, but if element is an integer, convert integer to double and return value.
extern LANGSPEC dynvec_t HASH_get_array(const hash_t *hash, const char *key);
static inline int HASH_get_int32(const hash_t *hash, const char *key){
  return (int)HASH_get_int(hash, key);
}
#if USE_QT4
static inline QString HASH_get_qstring(const hash_t *hash, const char *key){
  return STRING_get_qstring(HASH_get_string(hash, key));
}
static inline void HASH_put_qstring(hash_t *hash, const char *key, const QString string){
  return HASH_put_string(hash, key, STRING_create(string));
}
#endif


// Array interface

extern LANGSPEC int HASH_get_array_size(const hash_t *hash, const char *key);

extern LANGSPEC bool HASH_remove_at(hash_t *hash, const char *raw_key, int i);
extern LANGSPEC bool HASH_remove(hash_t *hash, const char *raw_key);

extern LANGSPEC void HASH_put_dyn_at(hash_t *hash, const char *key, int i, const dyn_t val);
extern LANGSPEC void HASH_put_string_at(hash_t *hash, const char *key, int i, const wchar_t *val);
extern LANGSPEC void HASH_put_chars_at(hash_t *hash, const char *key, int i, const char *val);
extern LANGSPEC void HASH_put_int_at(hash_t *hash, const char *key, int i, int64_t val);
extern LANGSPEC void HASH_put_instrument_at(hash_t *hash, const char *key, int i, instrument_t val);
extern LANGSPEC void HASH_put_filepath_at(hash_t *hash, const char *key, int i, filepath_t val);
extern LANGSPEC void HASH_put_bool_at(hash_t *hash, const char *key, int i, bool val);
extern LANGSPEC void HASH_put_float_at(hash_t *hash, const char *key, int i, double val);
extern LANGSPEC void HASH_put_ratio_at(hash_t *hash, const char *key, int i, const Ratio val);
extern LANGSPEC void HASH_put_hash_at(hash_t *hash, const char *key, int i, hash_t *val);
extern LANGSPEC void HASH_put_array_at(hash_t *hash, const char *key, int i, const dynvec_t val);

extern LANGSPEC dyn_t HASH_get_dyn_at(const hash_t *hash, const char *key, int i);
extern LANGSPEC const wchar_t *HASH_get_string_at(const hash_t *hash, const char *key, int i);
extern LANGSPEC const char *HASH_get_chars_at(const hash_t *hash, const char *key, int i);
extern LANGSPEC int64_t HASH_get_int_at(const hash_t *hash, const char *key, int i);
extern LANGSPEC instrument_t HASH_get_instrument_at(const hash_t *hash, const char *key, int i);
extern LANGSPEC filepath_t HASH_get_filepath_at(const hash_t *hash, const char *key, int i);
static inline int HASH_get_int32_at(const hash_t *hash, const char *key, int i){
  return (int)HASH_get_int_at(hash, key, i);
}

extern LANGSPEC bool HASH_get_bool_at(const hash_t *hash, const char *key, int i);
extern LANGSPEC double HASH_get_float_at(const hash_t *hash, const char *key, int i);
extern LANGSPEC double HASH_get_number_at(const hash_t *hash, const char *key, int i); // same as HASH_get_float_at, but allows element to be stored as integer.
extern LANGSPEC hash_t *HASH_get_hash_at(const hash_t *hash, const char *key, int i);


// Loading and saving (serializing and deserializing to string. (Tip: The 'file' argument can be stdout or stdout (useful for debugging))

extern LANGSPEC void HASH_save(const hash_t *hash, disk_t *file);
extern LANGSPEC hash_t *HASH_load(disk_t *file);

// This function was used before the array interface was introduced.
// extern LANGSPEC const char *HASH_get_int_hash(int i);

#endif // HASHMAP_PROC_H
