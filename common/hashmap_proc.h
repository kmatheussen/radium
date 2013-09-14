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


// Warning: Writing two elements with the same key to the same hash table just adds two elements with the same key into the table.
// Nothing is ever deleted from the tables.

// A key can not contain lineshift since lineshifts are used as delimiter character when loading/saving.

extern LANGSPEC hash_t *HASH_create(int approx_size);

extern LANGSPEC bool HASH_has_key(hash_t *hash, const char *key);
extern LANGSPEC bool HASH_has_key_at(hash_t *hash, const char *key, int i);

extern LANGSPEC int HASH_get_num_elements(hash_t *hash);

extern LANGSPEC hash_t *HASH_get_keys(hash_t *hash); // Returns all keys in a new hash array.
extern LANGSPEC vector_t *HASH_get_values(hash_t *hash);

// HASH_put_*: char *key is not copied. I.e. the key is used directly, not a copy of it.
// HASH_put_string: A copy of char *val is used, not val itself.

extern LANGSPEC void HASH_put_string(hash_t *hash, const char *key, const char *val);
extern LANGSPEC void HASH_put_int(hash_t *hash, const char *key, int64_t val);
extern LANGSPEC void HASH_put_float(hash_t *hash, const char *key, double val);
extern LANGSPEC void HASH_put_hash(hash_t *hash, const char *key, hash_t *val);

extern LANGSPEC const char *HASH_get_string(hash_t *hash, const char *key);
extern LANGSPEC int64_t HASH_get_int(hash_t *hash, const char *key);
extern LANGSPEC double HASH_get_float(hash_t *hash, const char *key);
extern LANGSPEC hash_t *HASH_get_hash(hash_t *hash, const char *key);


// Array interface

extern LANGSPEC int HASH_get_array_size(hash_t *hash);

extern LANGSPEC void HASH_put_string_at(hash_t *hash, const char *key, int i, const char *val);
extern LANGSPEC void HASH_put_int_at(hash_t *hash, const char *key, int i, int64_t val);
extern LANGSPEC void HASH_put_float_at(hash_t *hash, const char *key, int i, double val);
extern LANGSPEC void HASH_put_hash_at(hash_t *hash, const char *key, int i, hash_t *val);

extern LANGSPEC const char *HASH_get_string_at(hash_t *hash, const char *key, int i);
extern LANGSPEC int64_t HASH_get_int_at(hash_t *hash, const char *key, int i);
extern LANGSPEC double HASH_get_float_at(hash_t *hash, const char *key, int i);
extern LANGSPEC hash_t *HASH_get_hash_at(hash_t *hash, const char *key, int i);


// Loading and saving (serializing and deserializing to string. (The 'file' argument can be stdout for debugging))

extern LANGSPEC void HASH_save(hash_t *hash, FILE *file);
extern LANGSPEC hash_t *HASH_load(FILE *file);

// This function was used before the array interface was introduced.
// extern LANGSPEC const char *HASH_get_int_hash(int i);

#endif // HASHMAP_PROC_H
