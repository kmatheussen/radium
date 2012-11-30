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

#include <inttypes.h>
#include <string.h>

#include "nsmtracker.h"
#include "OS_settings_proc.h"

#include "hashmap_proc.h"

// table is copied from wikipedia
static const int prime_numbers[] = {2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47, 53, 59, 61, 67, 71, 73, 79, 83, 89, 97, 101, 103, 107, 109, 113, 127, 131, 137, 139, 149, 151, 157, 163, 167, 173, 179, 181, 191, 193, 197, 199, 211, 223, 227, 229, 233, 239, 241, 251, 257, 263, 269, 271, 277, 281, 283, 293, 307, 311, 313, 317, 331, 337, 347, 349, 353, 359, 367, 373, 379, 383, 389, 397, 401, 409, 419, 421, 431, 433, 439, 443, 449, 457, 461, 463, 467, 479, 487, 491, 499, 503, 509, 521, 523, 541, 547, 557, 563, 569, 571, 577, 587, 593, 599, 601, 607, 613, 617, 619, 631, 641, 643, 647, 653, 659, 661, 673, 677, 683, 691, 701, 709, 719, 727, 733, 739, 743, 751, 757, 761, 769, 773, 787, 797, 809, 811, 821, 823, 827, 829, 839, 853, 857, 859, 863, 877, 881, 883, 887, 907, 911, 919, 929, 937, 941, 947, 953, 967, 971, 977, 983, 991, 997};

static int find_next_prime_number(int number){
  if(number>997)
    return number * 3 / 2;

  int i;
  for(i=0;i<168;i++)
    if(prime_numbers[i]>number)
      return prime_numbers[i];

  RError("Somethings wrong");
  return 997;
}


// Bob Jenkins's One-at-a-Time hash
// copied from http://eternallyconfuzzled.com/tuts/algorithms/jsw_tut_hashing.aspx
unsigned int oat_hash(const void *key)
{
  const unsigned char *p = key;
  unsigned h = 0;
  int i;
  int len = strlen(key);

  for ( i = 0; i < len; i++ ) {
    h += p[i];
    h += ( h << 10 );
    h ^= ( h >> 6 );
  }
  
  h += ( h << 3 );
  h ^= ( h >> 11 );
  h += ( h << 15 );
  
  return h;
}

enum{
  STRING_TYPE,
  INT_TYPE,
  FLOAT_TYPE,
  HASH_TYPE
};

static const char *type_names[4]={"STRING_TYPE","INT_TYPE","FLOAT_TYPE","HASH_TYPE"};

static const char *type_to_typename(int type){
  return type_names[type];
}

static int typename_to_type(const char *type_name){
  int i;
  for(i=0;i<4;i++)
    if(!strcmp(type_name,type_names[i]))
      return i;
  RError("Unknown type_name: \"\%s\"",type_name);
  return 1;
}

typedef struct _hash_element_t{
  struct _hash_element_t *next;
  const char *key;
  int type;
  union{
    const char *string;
    int64_t int_number;
    double float_number;
    hash_t *hash;
  };
} hash_element_t;
  
struct _hash_t{
  int num_array_elements;
  int num_elements;

  int elements_size;
  hash_element_t *elements[];
}; // hash_t;

hash_t *HASH_create(int approx_size){
  int elements_size = find_next_prime_number(approx_size*3/2);

  hash_t *hash=talloc(sizeof(hash_t) + sizeof(hash_element_t*)*elements_size);

  hash->elements_size = elements_size;

  return hash;
}

int HASH_get_array_size(hash_t *hash){
  return hash->num_array_elements;
}

int HASH_get_num_elements(hash_t *hash){
  return hash->num_elements;
}

hash_t *HASH_get_keys(hash_t *hash){
  hash_t *keys = HASH_create(hash->num_elements);
  int pos=0;
  int i;
  for(i=0;i<hash->elements_size;i++){
    hash_element_t *element = hash->elements[i];
    while(element!=NULL){
      HASH_put_string_at(keys,pos++,element->key);
      element=element->next;
    }
  }
  return keys;
}

static const char *HASH_get_int_hash(int i){
  const int table_size = 1024*16;
  if(i<table_size && i>=0){
    static const char **h=NULL;
    if(h==NULL)
      h=calloc(sizeof(char*),table_size); // allocate at runtime because of GC. (less scanning)
    if(h[i]==NULL){
      char num[64];
      sprintf(num,"<int hash> %d",i);
      h[i] = strdup(num);
    }
    return h[i];

  }else
    return talloc_numberstring(i);
}

static void HASH_put(hash_t *hash, const char *key, hash_element_t *element){
  unsigned int index = oat_hash(key) % hash->elements_size;

  hash->num_elements++;

  element->next = hash->elements[index];
  hash->elements[index] = element;
}

void HASH_put_string(hash_t *hash, const char *key, const char *val){
  hash_element_t *element = talloc(sizeof(hash_element_t));
  element->key=key;
  element->type=STRING_TYPE;
  element->string=talloc_strdup(val);

  HASH_put(hash,key,element);
}

void HASH_put_int(hash_t *hash, const char *key, int64_t val){
  hash_element_t *element = talloc(sizeof(hash_element_t));
  element->key=key;
  element->type=INT_TYPE;
  element->int_number=val;

  HASH_put(hash,key,element);
}

void HASH_put_float(hash_t *hash, const char *key, double val){
  hash_element_t *element = talloc(sizeof(hash_element_t));
  element->key=key;
  element->type=FLOAT_TYPE;
  element->float_number=val;

  HASH_put(hash,key,element);
}

void HASH_put_hash(hash_t *hash, const char *key, hash_t *val){
  hash_element_t *element = talloc(sizeof(hash_element_t));
  element->key=key;
  element->type=HASH_TYPE;
  element->hash=val;

  HASH_put(hash,key,element);
}

void HASH_put_string_at(hash_t *hash, int i, const char *val){
  HASH_put_string(hash, HASH_get_int_hash(i), val);
  hash->num_array_elements++;
}
void HASH_put_int_at(hash_t *hash, int i, int64_t val){
  HASH_put_int(hash, HASH_get_int_hash(i), val);
  hash->num_array_elements++;
}
void HASH_put_float_at(hash_t *hash, int i, double val){
  HASH_put_float(hash, HASH_get_int_hash(i), val);
  hash->num_array_elements++;
}
void HASH_put_hash_at(hash_t *hash, int i, hash_t *val){
  HASH_put_hash(hash, HASH_get_int_hash(i), val);
  hash->num_array_elements++;
}

static hash_element_t *HASH_get_no_complaining(hash_t *hash, const char *key){
  unsigned int index = oat_hash(key) % hash->elements_size;
  hash_element_t *element=hash->elements[index];

  while(element!=NULL && strcmp(key,element->key))
    element=element->next;

  return element;
}

bool HASH_has_key(hash_t *hash, const char *key){
  return HASH_get_no_complaining(hash,key) != NULL;
}

static hash_element_t *HASH_get(hash_t *hash, const char *key, int type){
  hash_element_t *element=HASH_get_no_complaining(hash, key);

  if(element==NULL)
    RWarning("HASH_get. Element not found. key: \"%s\"",key);

  if(element->type!=type){
    RWarning("HASH_get. Element \"%s\" is found, but is wrong type. Requested %d, found %d.",key,type,element->type);
    element = NULL;
  }

  return element;
}

const char *HASH_get_string(hash_t *hash, const char *key){
  hash_element_t *element = HASH_get(hash,key,STRING_TYPE);
  if(element==NULL)
    return NULL;

  return element->string;
}

int64_t HASH_get_int(hash_t *hash, const char *key){
  hash_element_t *element = HASH_get(hash,key,INT_TYPE);
  if(element==NULL)
    return 0;

  return element->int_number;
}

double HASH_get_float(hash_t *hash, const char *key){
  hash_element_t *element = HASH_get(hash,key,FLOAT_TYPE);
  if(element==NULL)
    return 0.0;

  return element->float_number;
}


hash_t *HASH_get_hash(hash_t *hash, const char *key){
  hash_element_t *element = HASH_get(hash,key,HASH_TYPE);
  if(element==NULL)
    return NULL;

  return element->hash;
}

const char *HASH_get_string_at(hash_t *hash, int i){
  return HASH_get_string(hash, HASH_get_int_hash(i));
}

int64_t HASH_get_int_at(hash_t *hash, int i){
  return HASH_get_int(hash, HASH_get_int_hash(i));
}

double HASH_get_float_at(hash_t *hash, int i){
  return HASH_get_float(hash, HASH_get_int_hash(i));
}

hash_t *HASH_get_hash_at(hash_t *hash, int i){
  return HASH_get_hash(hash, HASH_get_int_hash(i));
}


void HASH_save(hash_t *hash, FILE *file){
  fprintf(file, ">> HASH MAP BEGIN\n");

  fprintf(file, "%d\n", hash->elements_size);

  int i;
  for(i=0;i<hash->elements_size;i++){
    hash_element_t *element=hash->elements[i];
    while(element!=NULL){
      fprintf(file,"%s\n",element->key);
      fprintf(file,"%s\n",type_to_typename(element->type));
      switch(element->type){
      case STRING_TYPE:
        fprintf(file,"%s\n",element->string);
        break;
      case INT_TYPE:
        fprintf(file,"%" PRId64 "\n",element->int_number);
        break;
      case FLOAT_TYPE:
        fprintf(file,"%f\n",element->float_number);
        break;
      case HASH_TYPE:
        HASH_save(element->hash, file);
        break;
      }
      element = element->next;
    }
  }
  fprintf(file,"<< HASH MAP END\n");
}

static bool read_line(char *line,int len,FILE *file){
  if(fgets(line, len, file)==NULL){
    RError("End of file before finished reading hash map");
    return false;
  }

  if(line[strlen(line)-1]=='\n')
    line[strlen(line)-1] = 0;

  return true;
}

hash_t *HASH_load(FILE *file){
  char line[8194];

  read_line(line,8193,file);
  if(strcmp(line,">> HASH MAP BEGIN")){
    RError("Trying to load something which is not a hash map. First line: \"%s\"",line);
    return NULL;
  }

  read_line(line, 8193, file);
  int elements_size = atoi(line);

  hash_t *hash=HASH_create(elements_size);

  read_line(line, 8193, file);

  while(strcmp(line,"<< HASH MAP END")){

    const char *key = talloc_strdup(line);

    if(!strncmp(key,"<int hash>",strlen("<int hash>")))
      hash->num_array_elements++;

    read_line(line, 8193, file);
    int type = typename_to_type(line);

    switch(type){

    case STRING_TYPE:
      read_line(line, 8193, file);
      HASH_put_string(hash, key, line);
      break;
    case INT_TYPE:
      read_line(line, 8193, file);
      HASH_put_int(hash, key, atoll(line));
      break;
    case FLOAT_TYPE:
      read_line(line, 8193, file);
      HASH_put_float(hash, key, OS_get_double_from_string(line));
      break;
    case HASH_TYPE:
      HASH_put_hash(hash, key, HASH_load(file));
      break;
    }

    if(read_line(line, 8193, file)==false)
      return NULL;
  }

  return hash;  
}

