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
#include "visual_proc.h"
#include "vector_proc.h"
#include "OS_settings_proc.h"
#include "OS_disk_proc.h"

#include "hashmap_proc.h"


// table is copied from wikipedia
static const int prime_numbers[] = {2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47, 53, 59, 61, 67, 71, 73, 79, 83, 89, 97, 101, 103, 107, 109, 113, 127, 131, 137, 139, 149, 151, 157, 163, 167, 173, 179, 181, 191, 193, 197, 199, 211, 223, 227, 229, 233, 239, 241, 251, 257, 263, 269, 271, 277, 281, 283, 293, 307, 311, 313, 317, 331, 337, 347, 349, 353, 359, 367, 373, 379, 383, 389, 397, 401, 409, 419, 421, 431, 433, 439, 443, 449, 457, 461, 463, 467, 479, 487, 491, 499, 503, 509, 521, 523, 541, 547, 557, 563, 569, 571, 577, 587, 593, 599, 601, 607, 613, 617, 619, 631, 641, 643, 647, 653, 659, 661, 673, 677, 683, 691, 701, 709, 719, 727, 733, 739, 743, 751, 757, 761, 769, 773, 787, 797, 809, 811, 821, 823, 827, 829, 839, 853, 857, 859, 863, 877, 881, 883, 887, 907, 911, 919, 929, 937, 941, 947, 953, 967, 971, 977, 983, 991, 997};

static int find_next_prime_number(int number){
  if(number>997)
    return (number * 3 / 2) + 1;

  int i;
  for(i=0;i<168;i++)
    if(prime_numbers[i]>number)
      return prime_numbers[i];

  R_ASSERT(false);
  return 997;
}


// Bob Jenkins's One-at-a-Time hash
// copied from http://eternallyconfuzzled.com/tuts/algorithms/jsw_tut_hashing.aspx
unsigned int oat_hash(const char *key, int i_i)
{
  unsigned h = 0;
  int len = (int)strlen(key);
  int i;

  const uint8_t *p = (const unsigned char*) key;

  for ( i = 0; i < len; i++ ) {
    h += p[i];
    h += ( h << 10 );
    h ^= ( h >> 6 );
  }

  h += i_i;
  h += ( h << 10 );
  h ^= ( h >> 6 );
  
  h += ( h << 3 );
  h ^= ( h >> 11 );
  h += ( h << 15 );

  return h;
}

static const char *type_to_typename(enum DynType type){
  return DYN_type_name(type);
}

static int typename_to_type(const wchar_t *wtype_name){
  return DYN_get_type_from_name(STRING_get_chars(wtype_name));
}

typedef struct _hash_element_t{
  struct _hash_element_t *next;
  const char *key;
  int i;
  dyn_t a;
} hash_element_t;

static hash_element_t *copy_element(hash_element_t *element){
  hash_element_t *element_copy = tcopy(element, sizeof(hash_element_t));
  //element_copy->key = talloc_strdup(element->key); // is this necessary? The key is already allocated by bdwgc, and it is never changed as far as I know.
  
  if (element->a.type==HASH_TYPE)
    element_copy->a.hash = HASH_copy(element->a.hash);
  
  //else if (element->a.type==STRING_TYPE)
  //  element_copy->string = STRING_copy(element->string); // This is probably not necessary since strings are copied before inserted into the hash table.

  return element_copy;
}

static bool elements_are_equal(const hash_element_t *el1, const hash_element_t *el2){
  R_ASSERT_NON_RELEASE(el1->i==el2->i);
  R_ASSERT_NON_RELEASE(!strcmp(el1->key,el2->key));

  return DYN_equal(el1->a, el2->a);
}

struct _hash_t{
  int num_array_elements;
  int num_elements;

  int elements_size;
  hash_element_t *elements[];
}; // hash_t;


void HASH_clear(hash_t *hash){
  hash->num_elements=0;
  hash->num_array_elements=0;
  memset(hash->elements, 0, hash->elements_size*sizeof(hash_element_t*));
}
  
static hash_element_t *HASH_get_no_complaining(const hash_t *hash, const char *raw_key, int i);

bool HASH_equal(const hash_t *h1, const hash_t *h2){
  if (h1->num_elements != h2->num_elements)
    return false;
  
  for(int i=0;i<h1->elements_size;i++){
    const hash_element_t *el1 = h1->elements[i];
    
    while (el1 != NULL){
      const hash_element_t *el2 = HASH_get_no_complaining(h2, el1->key, el1->i);
    
      if(el2==NULL)
        return false;
      
      if (elements_are_equal(el1, el2)==false)
        return false;

      el1=el1->next;
    }
  }

  return true;
}

hash_t *HASH_create(int approx_size){
  int elements_size = find_next_prime_number(approx_size*3/2);

  hash_t *hash=talloc(sizeof(hash_t) + sizeof(hash_element_t*)*elements_size);

  hash->elements_size = elements_size;

  return hash;
}

static void put2(hash_t *hash, const char *key, int i, hash_element_t *element);

hash_t *HASH_copy(const hash_t *hash){
  hash_t *ret = HASH_create(hash->elements_size / 2);

  int i;
  for(i=0;i<hash->elements_size;i++){
    hash_element_t *element = hash->elements[i];
    while(element!=NULL){
      hash_element_t *element_copy = copy_element(element);

      put2(ret, element->key, element->i, element_copy);
        
      element=element->next;
    }
  }

  return ret;
}

int HASH_get_array_size(const hash_t *hash){
  return hash->num_array_elements;
}

int HASH_get_num_elements(const hash_t *hash){
  return hash->num_elements;
}

hash_t *HASH_get_keys(const hash_t *hash){
  hash_t *keys = HASH_create(hash->num_elements);
  int pos=0;
  int i;
  for(i=0;i<hash->elements_size;i++){
    hash_element_t *element = hash->elements[i];
    while(element!=NULL){
      HASH_put_chars_at(keys,"key",pos++,element->key);
      element=element->next;
    }
  }
  return keys;
}

vector_t *HASH_get_values(const hash_t *hash){
  vector_t *vector = talloc(sizeof(vector_t));
  int i;
  for(i=0;i<hash->elements_size;i++){
    hash_element_t *element = hash->elements[i];
    while(element!=NULL){
      R_ASSERT_RETURN_IF_FALSE2(element->a.type==HASH_TYPE, vector);
      VECTOR_push_back(vector, element->a.hash);
      element=element->next;
    }
  }
  return vector; 
}


bool HASH_remove_at(hash_t *hash, const char *raw_key, int i){
  const char *key = STRING_get_utf8_chars(raw_key);
  unsigned int index = oat_hash(key,i) % hash->elements_size;
  hash_element_t *element=hash->elements[index];

  hash_element_t *prev = NULL;
  
  while(element!=NULL && (element->i!=i || strcmp(key,element->key))) {
    prev=element;
    element=element->next;
  }

  if (element==NULL)
    return false;

  hash->num_elements--;
  
  if (prev==NULL)
    hash->elements[index] = element->next;
  else
    prev->next = element->next;

  return true;
}

bool HASH_remove(hash_t *hash, const char *raw_key){
  return HASH_remove_at(hash, raw_key, 0);
}

static void put2(hash_t *hash, const char *key, int i, hash_element_t *element){
#if !defined(RELEASE)
  if (strcmp("NOTUSED", key)) // <- The pd and faust instruments create effect names called "NOTUSED" for unused effects.
    R_ASSERT(!HASH_has_key_at(hash, key, i)); // NOTE. Hitting this one is not necessarily a bug. But since it's so seldom that we overwrite hash table elements, it seems like a good idea to have this line here.
#endif
  
  unsigned int index = oat_hash(key,i) % hash->elements_size;
  //fprintf(stderr,"put %p. index: %u\n",hash,index);

  element->key=key;
  element->i=i;

  hash->num_elements++;

  element->next = hash->elements[index];
  hash->elements[index] = element;
}

static void put(hash_t *hash, const char *raw_key, int i, hash_element_t *element){
  const char *key = STRING_get_utf8_chars(raw_key);

  put2(hash, key, i, element);
}

static void put_string(hash_t *hash, const char *key, int i, const wchar_t *val){
  hash_element_t *element = talloc(sizeof(hash_element_t));
  element->a = DYN_create_string(val);

  put(hash,key,i,element);
}

static void put_chars(hash_t *hash, const char *key, int i, const char *val){
  hash_element_t *element = talloc(sizeof(hash_element_t));
  element->a = DYN_create_string_from_chars(val);

  put(hash,key,i,element);
}

static void put_int(hash_t *hash, const char *key, int i, int64_t val){
  hash_element_t *element = talloc(sizeof(hash_element_t));
  element->a = DYN_create_int(val);

  put(hash,key,i,element);
}

static void put_float(hash_t *hash, const char *key, int i, double val){
  hash_element_t *element = talloc(sizeof(hash_element_t));
  element->a = DYN_create_float(val);

  put(hash,key,i,element);
}

static void put_hash(hash_t *hash, const char *key, int i, hash_t *val){
  if (val==NULL){
    RError("put_hash: val==NULL. key: %d, i: %d\n", key, i);
    return;
  }
  
  hash_element_t *element = talloc(sizeof(hash_element_t));
  element->a = DYN_create_hash(val);

  put(hash,key,i,element);
}

void HASH_put_string(hash_t *hash, const char *key, const wchar_t *val){
  put_string(hash, key, 0, val);
}

void HASH_put_chars(hash_t *hash, const char *key, const char *val){
  put_chars(hash, key, 0, val);
}

void HASH_put_int(hash_t *hash, const char *key, int64_t val){
  put_int(hash, key, 0, val);
}

void HASH_put_float(hash_t *hash, const char *key, double val){
  put_float(hash, key, 0, val);
}

void HASH_put_hash(hash_t *hash, const char *key, hash_t *val){
  put_hash(hash, key, 0, val);
}

void HASH_put_string_at(hash_t *hash, const char *key, int i, const wchar_t *val){
  put_string(hash, key, i, val);
  int new_size = i+1;
  if(new_size>hash->num_array_elements)
    hash->num_array_elements = new_size;
}

void HASH_put_chars_at(hash_t *hash, const char *key, int i, const char *val){
  put_chars(hash, key, i, val);
  int new_size = i+1;
  if(new_size>hash->num_array_elements)
    hash->num_array_elements = new_size;
}

void HASH_put_int_at(hash_t *hash, const char *key, int i, int64_t val){
  put_int(hash, key, i, val);
  int new_size = i+1;
  if(new_size>hash->num_array_elements)
    hash->num_array_elements = new_size;
}
void HASH_put_float_at(hash_t *hash, const char *key, int i, double val){
  put_float(hash, key, i, val);
  int new_size = i+1;
  if(new_size>hash->num_array_elements)
    hash->num_array_elements = new_size;
}
void HASH_put_hash_at(hash_t *hash, const char *key, int i, hash_t *val){
  put_hash(hash, key, i, val);
  int new_size = i+1;
  if(new_size>hash->num_array_elements)
    hash->num_array_elements = new_size;
}

static hash_element_t *HASH_get_no_complaining(const hash_t *hash, const char *raw_key, int i){
  const char *key = STRING_get_utf8_chars(raw_key);
    
  unsigned int index = oat_hash(key,i) % hash->elements_size;
  hash_element_t *element=hash->elements[index];

  //fprintf(stderr,"start searching\n");
  while(element!=NULL && (element->i!=i || strcmp(key,element->key))) {
    //fprintf(stderr,"key: -%s- (-%s-), i: %d (%d)\n",element->key,key,element->i,i);
    element=element->next;
  }
  //fprintf(stderr,"end searching\n");

  return element;
}

// Returns the stored pointed, if it needs to be reused.
const char *HASH_get_key(const hash_t *hash, const char *key){
  hash_element_t *element = HASH_get_no_complaining(hash,key,0);
  if(element==NULL)
    return NULL;
  else
    return element->key;
}

bool HASH_has_key_at(const hash_t *hash, const char *key, int i){
  return HASH_get_no_complaining(hash,key,i) != NULL;
}

bool HASH_has_key(const hash_t *hash, const char *key){
  return HASH_has_key_at(hash, key, 0);
}

static hash_element_t *HASH_get(const hash_t *hash, const char *key, int i, enum DynType type){
  hash_element_t *element=HASH_get_no_complaining(hash, key, i);

  if(element==NULL){
    RWarning("HASH_get. Element not found. key: \"%s\"/%d. hash: %p",key,i,hash);
    return NULL;
  }
  
  if(element->a.type!=type){
    RWarning("HASH_get. Element \"%s\"/%d is found, but is wrong type. Requested %d, found %d.",key,i,type,element->a.type);
    return NULL;
  }

  return element;
}

static const wchar_t *get_string(const hash_t *hash, const char *key, int i){
  hash_element_t *element = HASH_get(hash,key,i,STRING_TYPE);
  if(element==NULL)
    return NULL;

  return element->a.string;
}

static const char *get_chars(const hash_t *hash, const char *key, int i){
  hash_element_t *element = HASH_get(hash,key,i,STRING_TYPE);
  if(element==NULL)
    return NULL;

  return STRING_get_chars(element->a.string);
}

static int64_t get_int(const hash_t *hash, const char *key, int i){
  hash_element_t *element = HASH_get(hash,key,i,INT_TYPE);
  if(element==NULL)
    return 0;

  return element->a.int_number;
}

static double get_float(const hash_t *hash, const char *key, int i){
  hash_element_t *element = HASH_get(hash,key,i,FLOAT_TYPE);
  if(element==NULL)
    return 0.0;

  return element->a.float_number;
}


static hash_t *get_hash(const hash_t *hash, const char *key, int i){
  hash_element_t *element = HASH_get(hash,key,i,HASH_TYPE);
  if(element==NULL)
    return NULL;

  if (element->a.hash==NULL){
    RError("element->hash==NULL. key: %s, i: %d\n", key, i);
    return HASH_create(1);
  }
  
  return element->a.hash;
}

const wchar_t *HASH_get_string(const hash_t *hash, const char *key){
  return get_string(hash, key, 0);
}

const char *HASH_get_chars(const hash_t *hash, const char *key){
  return get_chars(hash, key, 0);
}

int64_t HASH_get_int(const hash_t *hash, const char *key){
  return get_int(hash, key, 0);
}

double HASH_get_float(const hash_t *hash, const char *key){
  return get_float(hash, key, 0);
}

hash_t *HASH_get_hash(const hash_t *hash, const char *key){
  return get_hash(hash, key, 0);
}

const wchar_t *HASH_get_string_at(const hash_t *hash, const char *key, int i){
  return get_string(hash, key, i);
}

const char *HASH_get_chars_at(const hash_t *hash, const char *key, int i){
  return get_chars(hash, key, i);
}

int64_t HASH_get_int_at(const hash_t *hash, const char *key, int i){
  return get_int(hash, key, i);
}

double HASH_get_float_at(const hash_t *hash, const char *key, int i){
  return get_float(hash, key, i);
}

hash_t *HASH_get_hash_at(const hash_t *hash, const char *key, int i){
  return get_hash(hash, key, i);
}


static vector_t get_elements(const hash_t *hash){
  vector_t vector = {0};
  int i;
  for(i=0;i<hash->elements_size;i++){
    hash_element_t *element = hash->elements[i];
    while(element!=NULL){
      VECTOR_push_back(&vector, element);
      element=element->next;
    }
  }
  return vector; 
}

static int compare_hash_elements(const void *a2, const void *b2){
  hash_element_t *a = * (hash_element_t**)a2;
  hash_element_t *b = * (hash_element_t**)b2;

  int string_cmp = strcmp(a->key, b->key);

  if(string_cmp !=0 )
    return string_cmp;

  if(a->a.type != b->a.type)
    return a->a.type - b->a.type;

  return a->i - b->i;
}

static vector_t get_sorted_elements(hash_t *hash){
  vector_t elements = get_elements(hash);
  
  if(elements.num_elements>1) // fsanitize=undefined aborts the program if sending null pointer to qsort.
    qsort(elements.elements, elements.num_elements, sizeof(void*), compare_hash_elements);
  
  return elements;
}

#if 0
wchar_t *HASH_to_string(hash_t *hash){
  GFX_Message(NULL, "Warning, never tested");
  disk_t *disk = DISK_open_temp_for_writing();
  HASH_save(hash, file);
  return DISK_close_temp_for_writing(disk);
}
#endif

void HASH_save(hash_t *hash, disk_t *file){
  DISK_write(file, ">> HASH MAP V2 BEGIN\n");

  R_ASSERT(hash != NULL);
  
  if (hash != NULL) {
    vector_t elements = get_sorted_elements(hash);

    DISK_printf(file, "%d\n", elements.num_elements);
    
    int i;
    for(i=0;i<elements.num_elements;i++){
      hash_element_t *element=elements.elements[i];
      //DISK_write(file,element->key);DISK_write(file,"\n");
      DISK_printf(file,"%s\n",element->key);
      DISK_printf(file,"%d\n",element->i);
      DISK_printf(file,"%s\n",type_to_typename(element->a.type));
      switch(element->a.type){
        case STRING_TYPE:
          DISK_write_wchar(file, element->a.string);
          DISK_write(file, "\n");
          break;
        case INT_TYPE:
          DISK_printf(file,"%" PRId64 "\n",element->a.int_number);
          break;
        case FLOAT_TYPE:
          DISK_printf(file,"%s\n",OS_get_string_from_double(element->a.float_number));
          break;
        case HASH_TYPE:
          if (element->a.hash==NULL)
            RError("element->hash==NULL. i: %d, num_elements: %d, element->key: %s. element->i: %d, typename: %s", i, elements.num_elements, element->key, element->i, type_to_typename(element->a.type));
          else
            HASH_save(element->a.hash, file);
          break;
        case BOOL_TYPE:
          RError("Not using bool type in hash");
      }
    }
  }
  
  DISK_write(file,"<< HASH MAP V2 END\n");
}

extern int curr_disk_line;

static wchar_t *read_line(disk_t *file){

  curr_disk_line++;

  wchar_t *line = DISK_read_wchar_line(file);

  //printf("%d: -%s-\n", curr_disk_line, STRING_get_chars(line));
  
  if(line==NULL){
    GFX_Message(NULL, "End of file before finished reading hash map");
    return NULL;
  }

  return line;
}

hash_t *HASH_load(disk_t *file){
  bool new_format = false;

  wchar_t *line = read_line(file);
  if (line==NULL) return NULL;
  
  if(!STRING_equals(line,">> HASH MAP BEGIN")){
    if(!STRING_equals(line,">> HASH MAP V2 BEGIN")){
      GFX_Message(NULL, "Trying to load something which is not a hash map. First line: \"%s\"",line);
      return NULL;
    }else {
      new_format = true;
    }
  }

  line = read_line(file);
  int elements_size = STRING_get_int(line);

  hash_t *hash=HASH_create(elements_size);

  line = read_line(file);
  if (line==NULL)
    return NULL;
  
  while(!STRING_equals(line,"<< HASH MAP END") && !STRING_equals(line,"<< HASH MAP V2 END")){
    const char *key = STRING_get_chars(line);
    int i = 0;

    if(new_format==true){
      line = read_line(file);
      if (line==NULL) return NULL;
      
      i = STRING_get_int(line);
      int new_size = i+1;
      if(new_size>hash->num_array_elements)
        hash->num_array_elements = new_size;
    } else if(!strncmp(key,"<int hash>",strlen("<int hash>"))) {
      sscanf(key, "<int hash> %d", &i);
      key = "";
      hash->num_array_elements++;
    }

    line = read_line(file);
    if (line==NULL) return NULL;
    
    int type = typename_to_type(line);

    //printf("           Putting %d / %s\n",i, key);
    switch(type){

    case STRING_TYPE:
      line = read_line(file);
      put_string(hash, key, i, line);
      break;
    case INT_TYPE:
      line = read_line(file);
      put_int(hash, key, i, STRING_get_int64(line));
      break;
    case FLOAT_TYPE:
      line = read_line(file);
      put_float(hash, key, i, STRING_get_double(line));
      break;
    case HASH_TYPE:
      put_hash(hash, key, i, HASH_load(file));
      break;
    }

    line = read_line(file);
    if(line==NULL)
      return NULL;
  }

  return hash;  
}

