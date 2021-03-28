


/* Realtime threadsafe memory allocator. Very simple, not very smart. Huge fragmentation and waste.
   Should be good enough to prevent calls to system malloc in juce::MidiMessage and similar usages.

   The allocator resorts to malloc in the unlikely event that it fails. An RT warning will be shown if we resort to using malloc.
   Also note that all memory allocated by malloc is added to the pool when released (i.e. "free" is never called).
*/


#include <boost/lockfree/stack.hpp>

#if TEST_MAIN
#include <unistd.h>
#include <sys/time.h>
#include <stdarg.h>
#include "nsmtracker.h"

void CRASHREPORTER_send_assert_message(Crash_Type tye, const char *message, ...){
  abort();
}
void RError_internal(const char *fmt,...){
  abort();
}
void RT_message_internal(const char *fmt,...){
  va_list argp;
  va_start(argp,fmt);
  vfprintf(stderr,fmt,argp);
  fprintf(stderr, "\n");
  //vsnprintf(rt_message,rt_message_length-1,fmt,argp);
  va_end(argp);
}
void msleep(int ms){
  usleep(1000*ms);
}

double TIME_get_ms(void){
  struct timeval now;

  int err = gettimeofday(&now, NULL);
  if (err != 0)
    abort();

  return (double)now.tv_sec*1000.0 + (double)now.tv_usec/1000.0;
}
#endif

#include "nsmtracker.h"
#include "Vector.hpp"

#include "RT_memory_allocator_proc.h"
#include "RT_Array.hpp"


#if TEST_MAIN
#  define TOTAL_MEM_SIZE (1024) // Use very low value to provoke calls to malloc.
#else
#  define TOTAL_MEM_SIZE (1024*1024)
#endif

#define NUM_POOLS 16

#if TEST_MAIN
#define MAX_POOL_SIZE 10 // Very low value to provoke RT_free to fail now and then (to check that it doesn't crash if rt_free fails)
#else
#define MAX_POOL_SIZE 512
#endif

static constexpr int g_max_mem_size = 4 << (NUM_POOLS-1);

static char *g_mem;

static DEFINE_ATOMIC(int, g_curr_mem_size) = 0;

struct RT_Mem_internal{
  int _something;
};

struct RT_mempool_data{
  int16_t _pool_num;
  DEFINE_ATOMIC(int16_t, _num_users);
  struct RT_Mem_internal _mem;
};

static constexpr int g_offset_of_data = offsetof(RT_mempool_data, _mem);


#define POOL boost::lockfree::stack<RT_mempool_data*>

static POOL *g_pools[NUM_POOLS]; // First pool has 4, second has 8, third has 16, and so forth. [NO_STATIC_ARRAY_WARNING]


void RT_mempool_init(void){
  g_mem = (char*)calloc(1, TOTAL_MEM_SIZE);
  
  for(int i=0;i<NUM_POOLS;i++)
    g_pools[i] = new POOL(MAX_POOL_SIZE);
}

// Note: Also sets size;
static POOL *get_pool(int &size, int &pool_num){
  int size2 = 4;
  
  for(pool_num = 0 ; pool_num<NUM_POOLS ; pool_num++) {
    if (size2 >= size) {
        size = size2;
        R_ASSERT(size==4<<pool_num);
        return g_pools[pool_num];
    }
    
    size2 *= 2;
  }

  R_ASSERT(false);
  
  return NULL;
}

static RT_Mem_internal *RT_alloc_from_pool(POOL *pool){
  RT_mempool_data *data;
  
  if (pool->pop(data)){
    R_ASSERT_NON_RELEASE(ATOMIC_GET(data->_num_users)==0);
    ATOMIC_SET(data->_num_users, 1);
    return &data->_mem;
  }

  return NULL;
}

static RT_Mem_internal *RT_alloc_from_global_mem(int size, int pool_num){
  size += g_offset_of_data;
                 
  int pos = ATOMIC_ADD_RETURN_OLD(g_curr_mem_size, size);
  
  if (pos + size > TOTAL_MEM_SIZE){
    ATOMIC_ADD_RETURN_OLD(g_curr_mem_size, -size);
    return NULL;
  }

  RT_mempool_data *ret = (RT_mempool_data*)(g_mem + pos);

  ret->_pool_num = pool_num;
  ATOMIC_NAME(ret->_num_users) = 1;
  
  return &ret->_mem;
}

#if TEST_MAIN
static DEFINE_ATOMIC(int, g_total_malloc) = 0;
#endif

static RT_Mem_internal *RT_alloc_using_malloc(int size, int pool_num, const char *who, int where){
  
#if TEST_MAIN
  //printf("...RT_alloc failed. Who: \"%s\". Size: %d. Where: %d\n", who, size, where);
  ATOMIC_ADD(g_total_malloc, size);
#else
  RT_message("RT_alloc failed. Who: \"%s\". Size: %d. Where: %d", who, size, where);
#endif
  
  RT_mempool_data *data = (RT_mempool_data *)malloc(g_offset_of_data + size);
  data->_pool_num = pool_num;
  ATOMIC_NAME(data->_num_users) = 1;
  
  return &data->_mem;
}

#if TEST_MAIN
static DEFINE_ATOMIC(int, g_used_mem) = 0;
#endif

void *RT_alloc_raw(int size, const char *who){

  R_ASSERT_NON_RELEASE(g_mem != NULL);

  if (size > g_max_mem_size)
    return RT_alloc_using_malloc(size, -1, who, 1);
  
  int pool_num;
  POOL *pool = get_pool(size, pool_num); // <- Note: Adjusts size to nearest power of two.

#if TEST_MAIN
  ATOMIC_ADD(g_used_mem, size);
#endif
  
  RT_Mem_internal *ret = RT_alloc_from_pool(pool);
  if (ret != NULL)
    return ret;
  
  ret = RT_alloc_from_global_mem(size, pool_num);
  if (ret != NULL)
    return ret;
  
  return RT_alloc_using_malloc(size, pool_num, who, 2);
}


void RT_inc_ref_raw(void *mem){
  if (mem==NULL){
    R_ASSERT(false);
    return;
  }
    
  char *cmem = (char*)mem;
  
  RT_mempool_data *data = (RT_mempool_data *)(cmem - g_offset_of_data);

  R_ASSERT_NON_RELEASE(ATOMIC_GET(data->_num_users) > 0);
  
  ATOMIC_ADD(data->_num_users, 1);
}

void RT_free_raw(void *mem, const char *who){
  if (mem==NULL)
    return;
  
  char *cmem = (char*)mem;
  
  RT_mempool_data *data = (RT_mempool_data *)(cmem - g_offset_of_data);
  
  //fprintf(stderr, "RT_Free: num users: %d. pool num: %d\n", ATOMIC_GET(data->_num_users), data->_pool_num);
  
  R_ASSERT_RETURN_IF_FALSE(data->_pool_num < NUM_POOLS);
  
  R_ASSERT_NON_RELEASE(ATOMIC_GET(data->_num_users) > 0);
  
  if (ATOMIC_ADD_RETURN_NEW(data->_num_users, -1) > 0)
    return;
     
  if (data->_pool_num < 0){
    R_ASSERT(data->_pool_num==-1); // pool_num is -1 if size of memory block is larger than g_max_mem_size.
    free(mem);
    return;
  }

  POOL *pool = g_pools[data->_pool_num];
  
  if (!pool->bounded_push(data))
    RT_message("RT_free failed. Who: \"%s\". pool_num: %d. Size: %d", who, data->_pool_num, 4 << data->_pool_num);

#if TEST_MAIN
  ATOMIC_ADD(g_used_mem, -(4<<data->_pool_num));
#endif
}


#ifdef TEST_MAIN

#include <thread>

static __thread int g_thread_type = -1;

bool PLAYER_current_thread_has_lock(void){
  return g_thread_type==1;
}


DEFINE_ATOMIC(int, g_allocated_middle) = 0;
DEFINE_ATOMIC(int, g_allocated_total) = 0;

int main(){

  int seed = time(NULL);
  printf("Seed: %d\n", (int)seed);

  srand(seed);
    
  RT_mempool_init();
  
  const int num_main_iterations = 40 + rand()%25;

  for(int i=0;i<num_main_iterations;i++){
    
    const int num_threads = 20 + rand()%50;
    const int num_reader_iterations = 2 + rand()%20;

    if (ATOMIC_GET(g_used_mem) != 0)
      abort();
    
    printf("  I: %d/%d (%d, %d).  Used global mem: %d / %d. Max used mem: %d. Total allocation: %d. Total malloc allocations: %d\n",
           i,
           num_main_iterations,
           num_threads,
           num_reader_iterations,
           ATOMIC_GET(g_curr_mem_size),
           TOTAL_MEM_SIZE,
           ATOMIC_GET(g_allocated_middle),
           ATOMIC_GET(g_allocated_total),
           ATOMIC_GET(g_total_malloc)
           );

    ATOMIC_SET(g_allocated_middle, 0);
    ATOMIC_SET(g_allocated_total, 0);
    ATOMIC_SET(g_total_malloc, 0);
    
    std::thread t[num_threads];
    
    for(int i=0;i<num_threads;i++){

      t[i] = std::thread([i, num_threads, num_reader_iterations](){
          
          g_thread_type = 1;
          
          msleep(rand() % 2);
          
          double ms = TIME_get_ms();
          double wait = rand() % 50;

          do{
            for(int i2=0;i2<num_reader_iterations;i2++){

              int size = 2 + rand() % 512;

              radium::RT_Array<char> data(size, "test");

              ATOMIC_ADD(g_allocated_total, size);
              
              if (ATOMIC_GET(g_used_mem) > ATOMIC_GET(g_allocated_middle))
                ATOMIC_SET(g_allocated_middle, ATOMIC_GET(g_used_mem));
              
              int val = rand() % 127;
              
              for(int i3=0; i3 < size ; i3 ++){
                data[i3] = val;
              }

              if (rand() % 1)
                msleep(2);

              for(int i3=0; i3 < size ; i3 ++){
                if (data[i3] != val){
                  fprintf(stderr, "i3: %d. data[i3]: %d. val: %d\n", i3, data[i3], val);
                  abort();
                }
              }
            }
            
            if (rand() % 5)
              msleep(1);
            
          }while(TIME_get_ms() < ms+wait);
          
        });
      
    }

    for(int i=0;i<num_threads;i++)
      t[i].join();
  }
  
  return 0;
}

#endif

