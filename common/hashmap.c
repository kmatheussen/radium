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
#include "Dynvec_proc.h"
#include "OS_settings_proc.h"
#include "OS_disk_proc.h"
#include "ratio_funcs.h"

#include "../api/api_proc.h"

#include "hashmap_proc.h"


static const int prime_numbers[] = {

// These numbers are copied from https://primes.utm.edu/lists/small/10000.txt
     2,     3,     5,     7,    11,    13,    17,    19,    23,    29,
    31,    37,    41,    43,    47,    53,    59,    61,    67,    71,
    73,    79,    83,    89,    97,   101,   103,   107,   109,   113,
   127,   131,   137,   139,   149,   151,   157,   163,   167,   173,
   179,   181,   191,   193,   197,   199,   211,   223,   227,   229,
   233,   239,   241,   251,   257,   263,   269,   271,   277,   281,
   283,   293,   307,   311,   313,   317,   331,   337,   347,   349,
   353,   359,   367,   373,   379,   383,   389,   397,   401,   409,
   419,   421,   431,   433,   439,   443,   449,   457,   461,   463,
   467,   479,   487,   491,   499,   503,   509,   521,   523,   541,
   547,   557,   563,   569,   571,   577,   587,   593,   599,   601,
   607,   613,   617,   619,   631,   641,   643,   647,   653,   659,
   661,   673,   677,   683,   691,   701,   709,   719,   727,   733,
   739,   743,   751,   757,   761,   769,   773,   787,   797,   809,
   811,   821,   823,   827,   829,   839,   853,   857,   859,   863,
   877,   881,   883,   887,   907,   911,   919,   929,   937,   941,

// These numbers are also copied from https://primes.utm.edu/lists/small/10000.txt, but a lot of the numbers have been manually removed to make the table smaller.
   947,   967,   977,   991,   997,
  1019,  1021,  1033,  1049,  1061,
  1087,  1091,  1097,  1109,  1123,
  1153,  1163,  1181,  1193,  1213,
  1229,  1231,  1249,  1277,  1283,
  1297,  1307,  1327,
  1381,  1423,  1433,
  1453,  1481,  1489,
  1523,  1549,  1567,
  1597,  1609,  1621,
  1663,  1693,  1709,
  1741,  1783,
  1823,  1871,
  1901,  1949,
  1993,  2017,
  2063,  2089,
  2131,  2161,
  2221,  2267,
  2293,  2339,
  2371,  2393,
  2437,  2473,
  2539,  2579,
  2621,  2663,
  2689,  2713,
  2749,  2791,
  2833,  2861,
  2909,  2957,     
3001,
3083,
3187,
3259,
3343,
3433,
3517,
3581,
3659,
3733,
3823,
3911,
4001,
4073,
4153,
4241,
4327,
4421,
4507,
4591,
4663,
4759,
4861,
4943,
5009,
5099,
5189,
5281,
5393,
5449,
5527,
5641,
5701,
5801,
5861,
5953,
6067,
6143,
6229,
6311,
6373,
6481,
6577,
6679,
6763,
6841,
6947,
7001,
7109,
7211,
7307,
7417,
7507,
7573,
7649,
7727,
7841,
7927,
8039,
8117,
8221,
8293,
8389,
8513,
8599,
8681,
8747,
8837,
8933,
9013,
9127,
9203,
9293,
9391,
9461,
9539,
9643,
9739,
9817,
9901,
10009,
10103,
10181,
10273,
10357,
10463,
10589,
10663,
10753,
10861,
10957,
11069,
11159,
11257,
11351,
11447,
11549,
11677,
11779,
11839,
11939,
12037,
12113,
12227,
12301,
12409,
12491,
12569,
12647,
12743,
12841,
12941,
13009,
13121,
13217,
13313,
13417,
13513,
13627,
13709,
13789,
13883,
13997,
14083,
14207,
14327,
14423,
14533,
14621,
14713,
14771,
14867,
14951,
15077,
15161,
15263,
15329,
15413,
15511,
15619,
15683,
15787,
15887,
15973,
16073,
16187,
16273,
16411,
16487,
16607,
16693,
16823,
16921,
17011,
17099,
17203,
17321,
17393,
17483,
17579,
17681,
17789,
17903,
17977,
18061,
18149,
18251,
18329,
18433,
18521,
18661,
18757,
18911,
19013,
19139,
19231,
19333,
19427,
19483,
19577,
19709,
19801,
19913,
19993,
20089,
20161,
20269,
20359,
20477,
20563,
20707,
20773,
20899,
21001,
21089,
21179,
21283,
21391,
21493,
21569,
21649,
21757,
21851,
21961,
22051,
22129,
22247,
22343,
22447,
22549,
22651,
22739,
22853,
22961,
23039,
23117,
23209,
23327,
23459,
23563,
23633,
23747,
23831,
23911,
24019,
24097,
24179,
24317,
24419,
24527,
24671,
24781,
24889,
24979,
25111,
25189,
25307,
25409,
25523,
25609,
25703,
25801,
25919,
26003,
26113,
26209,
26309,
26407,
26501,
26641,
26713,
26813,
26893,
26993,
27091,
27239,
27457,
27581,
27697,
27773,
27847,
27953,
28057,
28163,
28289,
28409,
28513,
28591,
28657,
28751,
28843,
28949,
29063,
29173,
29269,
29383,
29453,
29581,
30071,
31013,
33029,
36067,
40093,
45191,
51131,
58067,
66509,
75539,
100129,

// These remaining numbers are gathered here: http://compoasso.free.fr/primelistweb/page/prime/liste_online_en.php
200003, 400009, 800011, 1600033, 3200003, 6400013, 12800009, 25600013
};

static const int num_prime_numbers = sizeof(prime_numbers) / sizeof(int);
static const int last_prime_number = 25600013; // insert 25600013 manually since gcc 8.1.0 didn't compile "prime_numbers[num_prime_numbers-1]". [NO_STATIC_ARRAY_WARNING]

/*
(define (binsearch vector number low high)
  (if (< high low)
      low
      (let ((mid (floor (/ (+ low high) 2))))
        (if (>= (vector mid) number)
            (binsearch vector number low (1- mid))
            (binsearch vector number (1+ mid) high)))))
(assert (= (binsearch (vector 1 3 8 12 22)
                      0
                      0 4)
           0))
(assert (= (binsearch (vector 1 3 8 12 22)
                      1
                      0 4)
           0))
(assert (= (binsearch (vector 1 3 8 12 22)
                      2
                      0 4)
           1))
(assert (= (binsearch (vector 1 3 8 12 22)
                      3
                      0 4)
           2))
(assert (= (binsearch (vector 1 3 8 12 22)
                      12
                      0 4)
           3))
(assert (= (binsearch (vector 1 3 8 12 22)
                      21
                      0 4)
           4))
(assert (= (binsearch (vector 1 3 8 12 22)
                      22
                      0 4)
           4))
(assert (= (binsearch (vector 1 3 8 12 22)
                      23
                      0 4)
           5))
*/

// Manual Scheme->C translation of "binsearch" above.
static int binary_search_find_next_prime_number(int number, int low, int high) {   // initially called with low = 0, high = num_prime_numbers-1
  if (high < low)
    return low;
  
  int mid = (low + high) / 2;
  
  if (prime_numbers[mid] >= number)
    return binary_search_find_next_prime_number(number, low, mid-1);
  else
    return binary_search_find_next_prime_number(number, mid+1, high);
}

static int find_next_prime_number(int number){
  if(number <= last_prime_number){

    int pos = binary_search_find_next_prime_number(number, 0, num_prime_numbers-1);
    if (pos >= num_prime_numbers){
      R_ASSERT(false);
      return number;
    }

    return prime_numbers[pos];
    
  } else {
  
    if (number % 2)
      return number;
    else
      return number + 1;

  }
}


// Bob Jenkins's One-at-a-Time hash
// copied from http://eternallyconfuzzled.com/tuts/algorithms/jsw_tut_hashing.aspx
static unsigned int oat_hash(const char *key, int i_i)
{
  unsigned h = 0;

  const uint8_t *p = (const unsigned char*) key;

  for (int i = 0; ; i++ ) {
    uint8_t pi = p[i];
    if(pi==0)
      break;
    h += pi;
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


typedef struct _hash_element_t{
  const char *key;
  int i;
  dyn_t a;
} hash_element_t;


static bool elements_are_equal(const hash_element_t *el1, const hash_element_t *el2){
  R_ASSERT_NON_RELEASE(el1->i==el2->i);
  R_ASSERT_NON_RELEASE(!strcmp(el1->key,el2->key));

  return DYN_equal(el1->a, el2->a);
}

typedef struct{
  vector_t v;
  void *element; // first element.
} hash_vector_t;
  
struct _hash_t{
  int num_array_elements;
  int num_elements;

  int version;

  int elements_size;
  
  //hash_element_t *elements[];
  hash_vector_t elements[];
}; // hash_t;


void HASH_clear(hash_t *hash){
  hash->num_elements=0;
  hash->num_array_elements=0;
  memset(hash->elements, 0, hash->elements_size*sizeof(hash_vector_t)); //hash_element_t*));
}

int HASH_get_version(const hash_t *hash){
  return hash->version;
}

static hash_element_t *HASH_get_no_complaining(const hash_t *hash, const char *raw_key, int i);

bool HASH_equal(const hash_t *h1, const hash_t *h2){
  if (h1->num_elements != h2->num_elements)
    return false;
  
  for(int i=0;i<h1->elements_size;i++){
    
    VECTOR_FOR_EACH(const hash_element_t *el1, &h1->elements[i].v){
      const hash_element_t *el2 = HASH_get_no_complaining(h2, el1->key, el1->i);
    
      if(el2==NULL)
        return false;
      
      if (elements_are_equal(el1, el2)==false)
        return false;
      
    }END_VECTOR_FOR_EACH;
    
  }

  return true;
}

hash_t *HASH_create2(int approx_size, int version){
  int elements_size = find_next_prime_number(approx_size*3/2);

  hash_t *hash=talloc(sizeof(hash_t) + sizeof(hash_vector_t)*elements_size);

  hash->elements_size = elements_size;

  R_ASSERT(version>=3);
  
  hash->version = version;

  return hash;
}

hash_t *HASH_create(int approx_size){
  return HASH_create2(approx_size, 5);
}

static void put2(hash_t *hash, const char *key, int i, const dyn_t dyn);

hash_t *HASH_shallow_copy(const hash_t *hash){
  hash_t *copy = tcopy2(hash, sizeof(hash_t) + (hash->elements_size*sizeof(hash_vector_t)));
  return copy;
}
  
// Can also be used to rehash
hash_t *HASH_copy(const hash_t *hash){
  hash_t *ret = HASH_create(hash->num_elements);
  ret->version = hash->version;

  for(int i=0;i<hash->elements_size;i++){

    VECTOR_FOR_EACH(const hash_element_t *element, &hash->elements[i].v){
      
      put2(ret,
           element->key,
           element->i,
           DYN_copy(element->a)
           );
      
    }END_VECTOR_FOR_EACH;
    
  }

  return ret;
}

int HASH_get_array_size(const hash_t *hash, const char *key){
  int size = hash->num_array_elements;
  while(size > 0){
    if (HASH_has_key_at(hash, key, size-1))
      return size;
    else
      size--;
  }
  return 0;
}

int HASH_get_num_elements(const hash_t *hash){
  return hash->num_elements;
}

hash_t *HASH_get_keys_in_hash(const hash_t *hash){
  hash_t *keys = HASH_create(hash->num_elements);
  int pos=0;
  int i;
  for(i=0;i<hash->elements_size;i++){
    VECTOR_FOR_EACH(const hash_element_t *element, &hash->elements[i].v){
      HASH_put_chars_at(keys,"key",pos++,element->key);
    }END_VECTOR_FOR_EACH;
  }
  return keys;
}

vector_t HASH_get_keys(const hash_t *hash){
  vector_t vector = {0};
  int i;
  for(i=0;i<hash->elements_size;i++){
    VECTOR_FOR_EACH(const hash_element_t *element, &hash->elements[i].v){
      VECTOR_push_back(&vector, element->key);
    }END_VECTOR_FOR_EACH;
  }
  return vector; 
}

dynvec_t HASH_get_values(const hash_t *hash){
  dynvec_t vector = {0};
  int i;
  for(i=0;i<hash->elements_size;i++){
    VECTOR_FOR_EACH(const hash_element_t *element, &hash->elements[i].v){
      //R_ASSERT_RETURN_IF_FALSE2(element->a.type==HASH_TYPE, vector);
      DYNVEC_push_back(&vector, element->a);
    }END_VECTOR_FOR_EACH;
  }
  return vector; 
}

bool HASH_remove_at(hash_t *hash, const char *raw_key, int i){
  const char *key = STRING_get_utf8_chars(raw_key);
  unsigned int index = oat_hash(key,i) % hash->elements_size;

  VECTOR_FOR_EACH(const hash_element_t *element, &hash->elements[index].v){
    if (element->i==i && !strcmp(key, element->key)){

      VECTOR_delete_ignore_order(&hash->elements[index].v, iterator666);
  
      hash->num_elements--;
  
      return true;
    }
  }END_VECTOR_FOR_EACH;

  return false;
}

bool HASH_remove(hash_t *hash, const char *raw_key){
  return HASH_remove_at(hash, raw_key, 0);
}

static void put2(hash_t *hash, const char *key, int i, const dyn_t dyn){
  /*
  if (hash->version < 4 && dyn.type==INSTRUMENT_TYPE)
    abort();
  */
  
#if !defined(RELEASE)
  if (strcmp(NOTUSED_EFFECT_NAME, key)) // <- The pd and faust instruments create effect names called "NOTUSED" for unused effects.
    R_ASSERT(!HASH_has_key_at(hash, key, i)); // NOTE. Hitting this one is not necessarily a bug. But since it's so seldom that we overwrite hash table elements, it seems like a good idea to have this line here.
#endif
  
  unsigned int index = oat_hash(key,i) % hash->elements_size;
  //fprintf(stderr,"put %p. index: %u\n",hash,index);

  // First check if we should replace.
  VECTOR_FOR_EACH(hash_element_t *element, &hash->elements[index].v){
    if (element->i==i && !strcmp(key, element->key)){
#if !defined(RELEASE)
      if (strcmp(NOTUSED_EFFECT_NAME, key)) // <- The pd and faust instruments create effect names called "NOTUSED" for unused effects.
        R_ASSERT(false); // NOTE. Hitting this one is not necessarily a bug. But since it's so seldom that we overwrite hash table elements, it seems like a good idea to have this line here.
#endif
      element->a = dyn;
      return;
    }
  }END_VECTOR_FOR_EACH;

  {
    hash_element_t *element = talloc(sizeof(hash_element_t));
    element->a = dyn;
    
    element->key=key;
    element->i=i;

    if (hash->elements[index].v.num_elements_allocated==0 && hash->elements[index].v.num_elements==0){
      
      hash->elements[index].v.num_elements = 1;
      
      hash->elements[index].v.elements = &hash->elements[index].element;
      hash->elements[index].element = element;
      
    } else {

      VECTOR_push_back(&hash->elements[index].v, element);

#if !defined(RELEASE)
      R_ASSERT(hash->elements[index].v.num_elements >= 2);
      if(hash->elements[index].element != NULL){
        R_ASSERT(hash->elements[index].v.num_elements==2);
        R_ASSERT(hash->elements[index].element !=NULL);
        R_ASSERT(hash->elements[index].v.elements[0]==hash->elements[index].element);
      }
#endif
      
      hash->elements[index].element = NULL;      
      
    }
    
    hash->num_elements++;
  }
}

static void put(hash_t *hash, const char *raw_key, int i, const dyn_t dyn){
  const char *key = STRING_get_utf8_chars(raw_key);

  put2(hash, key, i, dyn);
}

static void put_dyn(hash_t *hash, const char *key, int i, const dyn_t dyn){
  put(hash,key,i,dyn);
}

static void put_string(hash_t *hash, const char *key, int i, const wchar_t *val){
  if(val==NULL){
    R_ASSERT(false);
    val = L"";
  }
#if 0
  static int64_t size=0,num=0;
  printf("    %d: %f\n", (int)(++num), (double)wcslen(val) / (1024.0*1024.0));
  size += wcslen(val);
#endif
  put_dyn(hash, key, i, DYN_create_string(val));
}

static void put_chars(hash_t *hash, const char *key, int i, const char *val){
  if(val==NULL){
    R_ASSERT(false);
    val = "";
  }
  put_dyn(hash, key, i, DYN_create_string_from_chars(val));
}

static void put_int(hash_t *hash, const char *key, int i, int64_t val){
  put_dyn(hash, key, i, DYN_create_int(val));
}

static void put_instrument(hash_t *hash, const char *key, int i, instrument_t val){
  if(hash->version < 4)
    put_dyn(hash, key, i, DYN_create_int(val.id));
  else
    put_dyn(hash, key, i, DYN_create_instrument(val));
}

static void put_filepath(hash_t *hash, const char *key, int i, filepath_t val){
  if(hash->version < 5)
    put_dyn(hash, key, i, DYN_create_string(val.id));
  else{
    R_ASSERT_NON_RELEASE(isLegalFilepath(val)); // we don't want this in case the illegal file path string is changed.
    put_dyn(hash, key, i, DYN_create_filepath(val));
  }
}

static void put_bool(hash_t *hash, const char *key, int i, bool val){
  put_dyn(hash, key, i, DYN_create_bool(val));
}

static void put_float(hash_t *hash, const char *key, int i, double val){
#if !defined(RELEASE)
  if(!sane_isnormal(val))
    abort();
#endif

  put_dyn(hash, key, i, DYN_create_float(val));
}

static void put_ratio(hash_t *hash, const char *key, int i, Ratio val){
  put_dyn(hash, key, i, DYN_create_ratio(val));
}

static void put_hash(hash_t *hash, const char *key, int i, hash_t *val){
  if (val==NULL){
    RError("put_hash: val==NULL. key: %s, i: %d\n", key, i);
    return;
  }

  put_dyn(hash, key, i, DYN_create_hash(val));
}

static void put_array(hash_t *hash, const char *key, int i, const dynvec_t val){
  put_dyn(hash, key, i, DYN_create_array(val));
}

void HASH_put_dyn(hash_t *hash, const char *key, const dyn_t val){
  put_dyn(hash, key, 0, val);
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

void HASH_put_instrument(hash_t *hash, const char *key, instrument_t val){
  put_instrument(hash, key, 0, val);
}

void HASH_put_filepath(hash_t *hash, const char *key, filepath_t val){
  put_filepath(hash, key, 0, val);
}

void HASH_put_bool(hash_t *hash, const char *key, bool val){
  put_bool(hash, key, 0, val);
}

void HASH_put_float(hash_t *hash, const char *key, double val){
  put_float(hash, key, 0, val);
}

void HASH_put_ratio(hash_t *hash, const char *key, const Ratio val){
  put_ratio(hash, key, 0, val);
}

void HASH_put_place(hash_t *hash, const char *key, const Place place){
  HASH_put_ratio(hash, key, make_ratio_from_place(place));
}


void HASH_put_hash(hash_t *hash, const char *key, hash_t *val){
  put_hash(hash, key, 0, val);
}

void HASH_put_array(hash_t *hash, const char *key, const dynvec_t dynvec){
  put_array(hash, key, 0, dynvec);
}

void HASH_put_dyn_at(hash_t *hash, const char *key, int i, const dyn_t val){
  put_dyn(hash, key, i, val);
  int new_size = i+1;
  if(new_size>hash->num_array_elements)
    hash->num_array_elements = new_size;
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

void HASH_put_instrument_at(hash_t *hash, const char *key, int i, instrument_t val){
  put_instrument(hash, key, i, val);
  int new_size = i+1;
  if(new_size>hash->num_array_elements)
    hash->num_array_elements = new_size;
}

void HASH_put_filepath_at(hash_t *hash, const char *key, int i, filepath_t val){
  put_filepath(hash, key, i, val);
  int new_size = i+1;
  if(new_size>hash->num_array_elements)
    hash->num_array_elements = new_size;
}

void HASH_put_bool_at(hash_t *hash, const char *key, int i, bool val){
  put_bool(hash, key, i, val);
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
void HASH_put_ratio_at(hash_t *hash, const char *key, int i, const Ratio val){
  put_ratio(hash, key, i, val);
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
void HASH_put_array_at(hash_t *hash, const char *key, int i, const dynvec_t val){
  put_array(hash, key, i, val);
  int new_size = i+1;
  if(new_size>hash->num_array_elements)
    hash->num_array_elements = new_size;
}

static hash_element_t *HASH_get_no_complaining(const hash_t *hash, const char *raw_key, int i){
  const char *key = STRING_get_utf8_chars(raw_key);
    
  unsigned int index = oat_hash(key,i) % hash->elements_size;

  //fprintf(stderr,"start searching\n");
  VECTOR_FOR_EACH(hash_element_t *element, &hash->elements[index].v){
    if (element->i==i && !strcmp(key, element->key))
      return element;
  }END_VECTOR_FOR_EACH;

  return NULL;
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

static hash_element_t *HASH_get_any_type(const hash_t *hash, const char *key, int i){
  hash_element_t *element=HASH_get_no_complaining(hash, key, i);

  if(element==NULL){
    RWarning("HASH_get. Element not found. key: \"%s\"/%d. hash: %p",key,i,hash);
    return NULL;
  }
  
  return element;
}

static hash_element_t *HASH_get(const hash_t *hash, const char *key, int i, enum DynType type){
  hash_element_t *element=HASH_get_any_type(hash, key, i);

  if(element==NULL)
    return NULL;
  
  if(element->a.type!=type){
    RWarning("HASH_get. Element \"%s\"/%d is found, but is wrong type. Requested %s, found %s.",key,i,DYN_type_name(type),DYN_type_name(element->a.type));
    return NULL;
  }

  return element;
}

static dyn_t get_dyn(const hash_t *hash, const char *key, int i){
  hash_element_t *element=HASH_get_no_complaining(hash, key, i);

  if(element==NULL){
    RWarning("HASH_get_dyn. Element not found. key: \"%s\"/%d. hash: %p",key,i,hash);
    return DYN_create_bool(false);
  }

  return element->a;
}

enum DynType HASH_get_type_at(const hash_t *hash, const char *raw_key, int i){
  return get_dyn(hash, raw_key, i).type;
}

enum DynType HASH_get_type(const hash_t *hash, const char *raw_key){
  return HASH_get_type_at(hash, raw_key, 0);
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

static instrument_t get_instrument(const hash_t *hash, const char *key, int i){
  if(hash->version < 4){

    // Need to check both INT_TYPE and INSTRUMENT_TYPE. Reason is that :instrument-id is stored as instrument type in HASH_load (even when version==3) to fix mixer strips.
    
    hash_element_t *element=HASH_get_any_type(hash, key, i);
    if (element==NULL)
      return make_instrument(0);

    if (element->a.type==INT_TYPE)
      return make_instrument(element->a.int_number);
    else if (element->a.type==INSTRUMENT_TYPE)
      return element->a.instrument;

    RWarning("HASH_get. Element \"%s\"/%d is found, but is wrong type. Expected INT_TYPE or INSTRUMENT_TYPE, found %s",key,i,DYN_type_name(element->a.type));
    
    return make_instrument(0);
    
  } else {
  
    hash_element_t *element = HASH_get(hash,key,i,INSTRUMENT_TYPE);
    if(element==NULL)
      return make_instrument(0);

    return element->a.instrument;
  }
}

static filepath_t get_filepath(const hash_t *hash, const char *key, int i){
  if(hash->version < 5){

    // Need to check both INT_TYPE and FILEPATH_TYPE. Reason is that :filepath-id is stored as filepath type in HASH_load (even when version==3) to fix mixer strips.
    
    hash_element_t *element=HASH_get_any_type(hash, key, i);
    if (element==NULL)
      return createIllegalFilepath();

    if (element->a.type==STRING_TYPE)
      return make_filepath(element->a.string);
    else if (element->a.type==FILEPATH_TYPE)
      return element->a.filepath;

    RWarning("HASH_get. Element \"%s\"/%d is found, but is wrong type. Expected STRING_TYPE or FILEPATH_TYPE, found %s",key,i,DYN_type_name(element->a.type));
    
    return createIllegalFilepath();
    
  } else {
  
    hash_element_t *element = HASH_get(hash,key,i,FILEPATH_TYPE);
    if(element==NULL)
      return createIllegalFilepath();

    return element->a.filepath;
  }
}

static double get_number(const hash_t *hash, const char *key, int i){
  hash_element_t *element=HASH_get_no_complaining(hash, key, i);

  if(element==NULL){
    RWarning("HASH_get_number. Element not found. key: \"%s\"/%d. hash: %p",key,i,hash);
    return 0.0;
  }

  if(element->a.type==INT_TYPE)
    return element->a.int_number;
  else if (element->a.type==FLOAT_TYPE)
    return element->a.float_number;


  RWarning("HASH_get. Element \"%s\"/%d is found, but is wrong type. Requested a number, found %s.",key,i,DYN_type_name(element->a.type));
  return 0.0;
}

static bool get_bool(const hash_t *hash, const char *key, int i){
  hash_element_t *element=HASH_get_no_complaining(hash, key, i);

  if(element==NULL){
    RWarning("HASH_get_bool. Element not found. key: \"%s\"/%d. hash: %p",key,i,hash);
    return false;
  }
  
  if(element->a.type==BOOL_TYPE)
    return element->a.bool_number;

  // Before v3, BOOL_TYPE was saved to disk as integers. Unfortunatly, we can't check the value of hash->version since we don't save old hash types, meaning that this info is lost when loading old song, saving it, and loading it again.
  if(element->a.type==INT_TYPE){
    if (element->a.int_number==0)
      return false;
    if (element->a.int_number==1)
      return true;
  }

  RWarning("HASH_get. Element \"%s\"/%d is found, but is wrong type. Requested BOOL_TYPE, found %s.",key,i,DYN_type_name(element->a.type));

  return false;
}

static double get_float(const hash_t *hash, const char *key, int i){
  hash_element_t *element = HASH_get(hash,key,i,FLOAT_TYPE);
  if(element==NULL)
    return 0.0;

  return element->a.float_number;
}

static Ratio get_ratio(hash_t *hash, const char *key, int i){
  hash_element_t *element = HASH_get(hash,key,i, RATIO_TYPE);
  if(element==NULL)
    return make_ratio(0,1);
  
  return *element->a.ratio;
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

static dynvec_t get_array(const hash_t *hash, const char *key, int i){
  hash_element_t *element = HASH_get(hash,key,i,ARRAY_TYPE);
  if(element==NULL){
    dynvec_t ret = {0};
    return ret;
  }

  if (element->a.array==NULL){
    RError("element->array==NULL. key: %s, i: %d\n", key, i);
    dynvec_t ret = {0};
    return ret;
  }
  
  return *element->a.array;
}

dyn_t HASH_get_dyn(const hash_t *hash, const char *key){
  return get_dyn(hash, key, 0);
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

instrument_t HASH_get_instrument(const hash_t *hash, const char *key){
  return get_instrument(hash, key, 0);
}

filepath_t HASH_get_filepath(const hash_t *hash, const char *key){
  return get_filepath(hash, key, 0);
}

bool HASH_get_bool(const hash_t *hash, const char *key){
  return get_bool(hash, key, 0);
}

double HASH_get_float(const hash_t *hash, const char *key){
  return get_float(hash, key, 0);
}

Ratio HASH_get_ratio(hash_t *hash, const char *key){
  return get_ratio(hash, key, 0);
}

Place HASH_get_place(hash_t *hash, const char *key){
  return make_place_from_ratio(HASH_get_ratio(hash, key));
}

double HASH_get_number(const hash_t *hash, const char *key){
  return get_number(hash, key, 0);
}

hash_t *HASH_get_hash(const hash_t *hash, const char *key){
  return get_hash(hash, key, 0);
}

dynvec_t HASH_get_array(const hash_t *hash, const char *key){
  return get_array(hash, key, 0);
}

dyn_t HASH_get_dyn_at(const hash_t *hash, const char *key, int i){
  return get_dyn(hash, key, i);
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

instrument_t HASH_get_instrument_at(const hash_t *hash, const char *key, int i){
  return get_instrument(hash, key, i);
}

filepath_t HASH_get_filepath_at(const hash_t *hash, const char *key, int i){
  return get_filepath(hash, key, i);
}

bool HASH_get_bool_at(const hash_t *hash, const char *key, int i){
  return get_bool(hash, key, i);
}

double HASH_get_float_at(const hash_t *hash, const char *key, int i){
  return get_float(hash, key, i);
}

double HASH_get_number_at(const hash_t *hash, const char *key, int i){
  return get_number(hash, key, i);
}

hash_t *HASH_get_hash_at(const hash_t *hash, const char *key, int i){
  return get_hash(hash, key, i);
}

static vector_t get_elements(const hash_t *hash){
  vector_t vector = {0};
  
  VECTOR_reserve(&vector, hash->num_elements);
  
  for(int i=0;i<hash->elements_size;i++){
    VECTOR_append(&vector, &hash->elements[i].v);
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

static vector_t get_sorted_elements(const hash_t *hash){
  vector_t elements = get_elements(hash);
  
  if(elements.num_elements>1) // fsanitize=undefined aborts the program if sending null pointer to qsort.
    qsort(elements.elements, elements.num_elements, sizeof(void*), compare_hash_elements);
  
  return elements;
}

#if 0
wchar_t *HASH_to_string(const hash_t *hash){
  GFX_Message(NULL, "Warning, never tested");
  disk_t *disk = DISK_open_temp_for_writing();
  HASH_save(hash, file);
  return DISK_close_temp_for_writing(disk);
}
#endif

bool HASH_save(const hash_t *hash, disk_t *file){  
  DISK_write(file, talloc_format(">> HASH MAP V%d BEGIN\n", hash->version));

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
      DYN_save(file, element->a);
    }
  }
  
  DISK_write(file,talloc_format("<< HASH MAP V%d END\n", hash->version));

  return DISK_get_error(file)==NULL;
}

static wchar_t *read_line(disk_t *file){

  wchar_t *line = DISK_read_wchar_line(file);
    
  //printf("%d: -%S-\n", g_curr_disk_line, line);
  
  if(line==NULL){
    GFX_Message(NULL, "End of file before finished reading hash map");
    return NULL;
  }

  return line;
}

#define READ_LINE(file) read_line(file); if (line==NULL) return NULL;

hash_t *HASH_load2(disk_t *file, bool return_null_for_unsupported_hasmap_versions){

  wchar_t *line = L"";
  while(STRING_starts_with(line, "#") || STRING_equals2(STRING_trim(line), L"")){
    line = READ_LINE(file);
    if (line==NULL)
      return NULL;
  }
  
  int version;
  if(STRING_equals(line,">> HASH MAP BEGIN")){
    version = 1;
  } else if (STRING_equals(line,">> HASH MAP V2 BEGIN")){
    version = 2;
  } else if (STRING_equals(line,">> HASH MAP V3 BEGIN")){
    version = 3;
  } else if (STRING_equals(line,">> HASH MAP V4 BEGIN")){
    version = 4;
  } else if (STRING_equals(line,">> HASH MAP V5 BEGIN")){
    version = 5;
  } else  if (STRING_starts_with(line, ">> HASH MAP V")){

    if (return_null_for_unsupported_hasmap_versions)
      return NULL;

    version = 3;
    vector_t v = {0};
    int try_anyway = VECTOR_push_back(&v, "Try anyway (program might crash and/or behave unstable)");
    int ok = VECTOR_push_back(&v, "Ok");

    int res = GFX_Message(&v, "Need a newer version of Radium to load this file");

    if (res!=try_anyway)
      return NULL;
    (void)ok;

  } else {
    GFX_Message(NULL, "Trying to load something which is not a hash map. First line: \"%S\"", line);
    return NULL;
  }

  line = READ_LINE(file);
  
  int elements_size = STRING_get_int(line);

  hash_t *hash=HASH_create(elements_size);
  hash->version = version;

  line = READ_LINE(file);
  
  while(!STRING_equals(line,"<< HASH MAP END")
        && !STRING_equals(line,"<< HASH MAP V2 END")
        && !STRING_equals(line,"<< HASH MAP V3 END")
        && !STRING_equals(line,"<< HASH MAP V4 END")
        && !STRING_equals(line,"<< HASH MAP V5 END")
        )
    {
      const char *key = STRING_get_chars(line);
      int i = 0;
      
      if(version > 1){
        
        line = READ_LINE(file);
        
        i = STRING_get_int(line);
        int new_size = i+1;
        if(new_size > hash->num_array_elements)
          hash->num_array_elements = new_size;
        
      } else if(!strncmp(key,"<int hash>",strlen("<int hash>"))) {
        
        sscanf(key, "<int hash> %d", &i);
        key = "";
        hash->num_array_elements++;
        
      }
      
      bool success;
      dyn_t dyn = DYN_load(file, &success);
      if (!success)
        return NULL;

      if (hash->version < 4 && !strcmp(key, ":instrument-id") && dyn.type==INT_TYPE)
        dyn = DYN_create_instrument(make_instrument(dyn.int_number));
      
      put_dyn(hash, key, i, dyn);
      
      line = READ_LINE(file);
    }
  
  return hash;  
}

hash_t *HASH_load(disk_t *file){
  return HASH_load2(file, false);
}
 
