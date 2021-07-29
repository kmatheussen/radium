


/* Realtime threadsafe memory allocator. Very simple, not very smart. Huge fragmentation and waste.
   Should be good enough to prevent calls to system malloc in juce::MidiMessage and similar usages.

   The allocator resorts to malloc in the unlikely event that it fails. An RT warning will be shown if we resort to using malloc.
   Also note that all memory allocated by malloc is added to the pool when released (i.e. "free" is never called).
*/


#if defined(RADIUM_USES_ASAN)
#  include "sanitizer/asan_interface.h"
#else
#  define ASAN_UNPOISON_MEMORY_REGION(a,b)
#  define ASAN_POISON_MEMORY_REGION(a,b)
#endif

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

bool THREADING_has_player_thread_priority(void){
  return false;
}

double TIME_get_ms(void){
  struct timeval now;

  int err = gettimeofday(&now, NULL);
  if (err != 0){
    fprintf(stderr, "============ err: %d ==========\n", err);
    //abort();
  }

  return (double)now.tv_sec*1000.0 + (double)now.tv_usec/1000.0;
}

#  define ASSERT_MEMORY 1

#if defined(RELEASE)
#  error "RELEASE should not be defined"
#endif

#else // TEST_MAIN -> ! TEST_MAIN

#  if !defined(RELEASE)
#    define ASSERT_MEMORY 1
#  endif

#endif

#if defined(RELEASE)
#  if ASSERT_MEMORY
#    error "error"
#  endif
#endif

#include "nsmtracker.h"
#include "Vector.hpp"

#include "RT_memory_allocator_proc.h"
#include "RT_Array.hpp"


namespace{
  struct Scoped_asan_unpoison{
    const volatile void *_addr;
    const size_t _size;
    Scoped_asan_unpoison(const volatile void *addr, const size_t size)
      : _addr(addr)
      , _size(size)
    {
#if defined(RADIUM_USES_ASAN)
      R_ASSERT(__asan_address_is_poisoned(addr) == 1); // This might not always be true though due to alignment restrictions (unpoisoning can unpoison more than it was asked, and poison might poison less than it was asked). If that happens it might help to uncomment the line with "[529834]" below.
#endif
      ASAN_UNPOISON_MEMORY_REGION(addr, size);
    }

    ~Scoped_asan_unpoison(){
#if defined(RADIUM_USES_ASAN)
      R_ASSERT(__asan_address_is_poisoned(_addr) == 0);
#endif
      ASAN_POISON_MEMORY_REGION(_addr, _size);
    }
  };
}



#if TEST_MAIN
#  define TOTAL_MEM_SIZE (1024) // Use very low value to provoke calls to malloc.
#else
#  define TOTAL_MEM_SIZE (1024*1024)
#endif

#define NUM_POOLS 16

#if TEST_MAIN
#  define MAX_POOL_SIZE 8 // Very low value to provoke RT_free to fail now and then (to check that it doesn't crash if rt_free fails)
#else
#  define MAX_POOL_SIZE 4096
#endif

#define ALIGN_MAX ((int)(std::alignment_of<std::max_align_t>::value))

#define IS_ALIGNED(POINTER) (((uintptr_t)(const void *)(POINTER)) % (ALIGN_MAX) == 0)

#define ALIGN_UP(value,alignment) (((uintptr_t)value + alignment - 1) & -alignment)


#define MIN_MEMPOOL_SIZE ALIGN_MAX // The way things work now, this is necessary to ensure memory blocks are properly aligned.

#define POOL_MEM_SIZE(Pool_num) (MIN_MEMPOOL_SIZE<<(Pool_num))


static constexpr int g_max_mem_size = POOL_MEM_SIZE(NUM_POOLS-1);

static char *g_mem;

static DEFINE_ATOMIC(int, g_curr_mem_size) = 0;

namespace{
  struct RT_Mem_internal{
    int _something;
  };
  
  struct RT_mempool_data{
    int32_t _pool_num;
    DEFINE_ATOMIC(int32_t, _num_users);

    // This makes RT_mempool_data blocks at least 32 bytes long on x86-64 CPUs since both max alignment and MIN_MEMPOOL_SIZE are 16 bytes each;
    // There are probably ways to lower the overhead, but it might not be worth it.
    struct RT_Mem_internal _mem __attribute__((aligned(std::alignment_of<std::max_align_t>::value)));
  };
}

static constexpr int g_offset_of_data = offsetof(RT_mempool_data, _mem);


static_assert(g_offset_of_data==ALIGN_MAX, "RT-allocated memory will not be aligned at max");
static_assert(g_offset_of_data==MIN_MEMPOOL_SIZE, "RT-allocated memory alignment must be equal to minimum pool size. If not things could end up not being properly aligned the way things work now.");


#define POOL boost::lockfree::stack<RT_mempool_data*>

static POOL *g_pools[NUM_POOLS]; // First pool has 8, second has 16, third has 32, and so forth. [NO_STATIC_ARRAY_WARNING]


void RT_mempool_init(void){
  g_mem = (char*)calloc(1, TOTAL_MEM_SIZE);

#if !defined(RELEASE)
  if (!IS_ALIGNED(g_mem)) {
    printf(" Memory is not aligned. Mem: %p. Size: %d\n", g_mem, TOTAL_MEM_SIZE);
    abort();
  }
#endif
  
#if ASSERT_MEMORY
  {
    unsigned char *mem = (unsigned char*)g_mem;
    for(int i=0 ; i < TOTAL_MEM_SIZE ; i++)
      mem[i] = 0x3e;
  }
#endif

  ASAN_POISON_MEMORY_REGION(g_mem, TOTAL_MEM_SIZE);  

  for(int i=0;i<NUM_POOLS;i++)
    g_pools[i] = new POOL(MAX_POOL_SIZE);
}

// Note: Also sets size and pool_num
static POOL *get_pool(int &size, int &pool_num){
  int size2 = MIN_MEMPOOL_SIZE;
  
  for(pool_num = 0 ; pool_num<NUM_POOLS ; pool_num++) {
    if (size2 >= size) {
        size = size2;
        R_ASSERT_NON_RELEASE(size==POOL_MEM_SIZE(pool_num));
        return g_pools[pool_num];
    }
    
    size2 *= 2;
  }

  R_ASSERT(false);
  
  return NULL;
}

static RT_Mem_internal *RT_alloc_from_pool(POOL *pool, int pool_num){
  RT_mempool_data *data;
  
  if (pool->pop(data)){
    Scoped_asan_unpoison unpoison(data, g_offset_of_data);
    
    R_ASSERT_NON_RELEASE(data->_pool_num >= pool_num);
    R_ASSERT_NON_RELEASE(ATOMIC_GET(data->_num_users)==0);
    ATOMIC_SET(data->_num_users, 1);

#if defined(RADIUM_USES_ASAN)
    R_ASSERT(__asan_address_is_poisoned(&data->_mem) == 1);
#endif

    //printf("Got memory from pool %d\n", pool_num);
    
    return &data->_mem;
  }

  return NULL;
}

static RT_Mem_internal *RT_alloc_from_global_mem(int size, int pool_num){
  size += g_offset_of_data;

  size = ALIGN_UP(size, ALIGN_MAX); // We need to do this to ensure g_curr_mem_size continues to be properly aligned also for the next call to this function.
    
  int pos = ATOMIC_ADD_RETURN_OLD(g_curr_mem_size, size);

  //printf("---Allocating global. pos: %d. size: %d\n", size, pos);
  
  if (pos + size > TOTAL_MEM_SIZE){
    ATOMIC_ADD_RETURN_OLD(g_curr_mem_size, -size);
    return NULL;
  }

  RT_mempool_data *ret = (RT_mempool_data*)(g_mem + pos);

  {
    Scoped_asan_unpoison unpoison(ret, g_offset_of_data);
    ret->_pool_num = pool_num;
    ATOMIC_NAME(ret->_num_users) = 1;
  }
  
#if !defined(RELEASE)
  if (!IS_ALIGNED(ret)) {
    printf(" Memory is not aligned. Mem: %p. Size: %d\n", ret, size);
    abort();
  }
  if (!IS_ALIGNED(&ret->_mem)) {
    printf(" Memory is not aligned. Mem: %p. Size: %d\n", &ret->_mem, size);
    abort();
  }
#endif
  
  return &ret->_mem;
}

static bool data_is_in_global_mem(const RT_mempool_data *data){
  const char *cdata = (const char*) data;

  if (cdata < g_mem)
    return false;

  if (cdata >= &g_mem[TOTAL_MEM_SIZE])
    return false;
  
  return true;
}
                      
#define FREE_FROM_GLOBAL_MEM 0 // Not much point, doesn't succeed that often, and there is also a bug causing tsan to fail some times.

static bool RT_maybe_free_to_global_mem(RT_mempool_data *data){
#if !FREE_FROM_GLOBAL_MEM
  return false;
#else
  
  if ( ((char*)data) < g_mem){
    //fprintf(stderr,"a\n");
    return false;
  }
  
  int size = g_offset_of_data + POOL_MEM_SIZE(data->_pool_num);

  int pos = ((char*)data) - ((char*)g_mem);

  if (pos+size > g_max_mem_size){
    //fprintf(stderr,"b\n");
    return false;
  }
  
  //printf("pos: %d. g_curr_mem_size: %d\n", pos+size, ATOMIC_GET(g_curr_mem_size));
  return ATOMIC_COMPARE_AND_SET_INT(g_curr_mem_size, pos+size, pos);
#endif
}

#if TEST_MAIN
static DEFINE_ATOMIC(int, g_total_malloc) = 0;
#endif

static RT_Mem_internal *RT_alloc_using_malloc(int size, int pool_num, const char *who, int where, bool show_warning, bool we_know_for_sure_we_are_not_RT){
  
#if TEST_MAIN
  //printf("...RT_alloc failed. Who: \"%s\". Size: %d. Where: %d\n", who, size, where);
  ATOMIC_ADD(g_total_malloc, size);
#else
  if (show_warning) {
#if !defined(RELEASE)
    fprintf(stderr, "-----------RT_alloc failed. Who: \"%s\". Size: %d. Where: %d", who, size, where);
    getchar();
#endif
    RT_message("RT_alloc failed. Who: \"%s\". Size: %d. Where: %d", who, size, where);
  }
#endif

  RT_mempool_data *data;
  if (we_know_for_sure_we_are_not_RT)
    data = (RT_mempool_data *)V_calloc(1, g_offset_of_data + size); // V_calloc makes sure data is in cache.
  else
    data = (RT_mempool_data *)malloc(g_offset_of_data + size);

  data->_pool_num = pool_num;
  ATOMIC_NAME(data->_num_users) = 1;

#if !defined(RELEASE)
  if (!IS_ALIGNED(data)) {
    printf(" Memory is not aligned. Mem: %p. Size: %d\n", data, g_offset_of_data + size);
    abort();
  }
  if (!IS_ALIGNED(&data->_mem)) {
    printf(" Memory is not aligned. Mem: %p. Size: %d\n", data, g_offset_of_data + size);
    abort();
  }
#endif
  
#if ASSERT_MEMORY
  {
    unsigned char *mem = (unsigned char*)&data->_mem;
    for(int i=0 ; i < size ; i++)
      mem[i] = 0x3e;
  }
#endif

  ASAN_POISON_MEMORY_REGION(data, g_offset_of_data + size);

  return &data->_mem;
}

#if !defined(RELEASE)
static DEFINE_ATOMIC(int, g_used_mem) = 0;
#endif

void *RT_alloc_raw_internal(const int minimum_num_elements, const int element_size, int &actual_num_elements, const char *who){

  R_ASSERT_NON_RELEASE(g_mem != NULL);

  int size = minimum_num_elements * element_size;
  
  void *ret;
  
  if (size > g_max_mem_size) {

    actual_num_elements = minimum_num_elements;
    ret = RT_alloc_using_malloc(size, -1, who, 1, true, false);

  } else {
    
    int pool_num;
    POOL *pool = get_pool(size, pool_num); // <- Note: Adjusts size to nearest power of two.
    
#if !defined(RELEASE)
    ATOMIC_ADD(g_used_mem, size);
    //printf("    ALLOCED MEM: %d\n", ATOMIC_GET(g_used_mem));
#endif

    ret = RT_alloc_from_pool(pool, pool_num);

    if (ret != NULL) {

      actual_num_elements = size / element_size;

    } else {

      actual_num_elements = minimum_num_elements;

      if (!THREADING_is_runner_thread() && !PLAYER_current_thread_has_lock())
      {

        ret = RT_alloc_using_malloc(size, pool_num, who, 2, false, true);
        
      } else {

        ret = RT_alloc_from_global_mem(size, pool_num);

        if (ret == NULL)
          ret = RT_alloc_using_malloc(size, pool_num, who, 2, true, false);

      }

    }

  }

  ASAN_UNPOISON_MEMORY_REGION(ret, size);

#if defined(RADIUM_USES_ASAN)
  {
    // These tests might not always be true though due to alignment restrictions (unpoisoning can unpoison more than it was asked, and poison might poison less than it was asked).
    // If this fails it might help to uncomment the line with "[529834]" above.
    RT_mempool_data *data = (RT_mempool_data *)(((char*)ret) - g_offset_of_data);
    R_ASSERT(__asan_address_is_poisoned(&data->_pool_num) == 1);
    R_ASSERT(__asan_address_is_poisoned(&ATOMIC_NAME(data->_num_users)) == 1);
    R_ASSERT(__asan_address_is_poisoned(ret) == 0);
  }
#endif

#if ASSERT_MEMORY
  {
    const char *data = (const char*)ret;
    
    for(int i3=0 ; i3 < size ; i3 ++){
      if (data[i3] != 0x3e){
        fprintf(stderr, "i3: %d. data[i3]: %d. size: %d\n", i3, data[i3], size);
        abort();
      }
    }
  }

#endif
  
#if !defined(RELEASE)
  if (!IS_ALIGNED(ret)) {
    fprintf(stderr, " Memory is not aligned. Mem: %p. Size: %d\n", g_mem, TOTAL_MEM_SIZE);
    abort();
  }
#endif
  
  return ret;
}


void *RT_alloc_raw(const int size, const char *who){
  int actual_size;
  
  void *ret = RT_alloc_raw_internal(1, size, actual_size, who);
  
  R_ASSERT_NON_RELEASE(actual_size > 0);

#if !defined(RELEASE)
  if (!IS_ALIGNED(ret)) {
    fprintf(stderr, " Memory is not aligned. Mem: %p. Size: %d\n", g_mem, TOTAL_MEM_SIZE);
    abort();
  }
#endif
  
  return ret;
}

void RT_inc_ref_raw(void *mem){
  if (mem==NULL){
    R_ASSERT_NON_RELEASE(false);
    return;
  }
    
  char *cmem = (char*)mem;
  
  RT_mempool_data *data = (RT_mempool_data *)(cmem - g_offset_of_data);

  {
    Scoped_asan_unpoison unpoison(data, g_offset_of_data);
    
    R_ASSERT_NON_RELEASE(ATOMIC_GET(data->_num_users) > 0);
    
    ATOMIC_ADD(data->_num_users, 1);
  }

}

void RT_free_raw(void *mem, const char *who){
  if (mem==NULL)
    return;
  
  char *cmem = (char*)mem;

  RT_mempool_data *data = (RT_mempool_data *)(cmem - g_offset_of_data);


  //fprintf(stderr, "RT_Free: num users: %d. pool num: %d\n", ATOMIC_GET(data->_num_users), data->_pool_num);

  int pool_num;
  {
    Scoped_asan_unpoison unpoison(data, g_offset_of_data);
    pool_num = data->_pool_num;

    R_ASSERT_RETURN_IF_FALSE(pool_num < NUM_POOLS);  
    R_ASSERT_NON_RELEASE(pool_num >= 0);

    R_ASSERT_NON_RELEASE(ATOMIC_GET(data->_num_users) > 0);
    if (ATOMIC_ADD_RETURN_NEW(data->_num_users, -1) > 0)
      return;
  }

#if ASSERT_MEMORY
  {
    char *m = (char*)&data->_mem;
    for(int i3=0 ; i3 < POOL_MEM_SIZE(pool_num) ; i3 ++)
      m[i3] = 0x3e;
  }
#endif

  if (pool_num < 0){
    
    R_ASSERT(pool_num==-1); // pool_num is -1 if size of memory block is larger than g_max_mem_size.

    ASAN_UNPOISON_MEMORY_REGION(mem, g_offset_of_data);
    
    free(mem);
    
  } else {

#if !defined(RELEASE)
    ATOMIC_ADD(g_used_mem, -POOL_MEM_SIZE(pool_num));
#endif
  

    if (RT_maybe_free_to_global_mem(data)) { // Note: RT_maybe_free_to_global_mem is currently disabled. It's not much point.
    
      //    printf("GOTIT\n");
    
    } else {

      ASAN_POISON_MEMORY_REGION(&data->_mem, POOL_MEM_SIZE(pool_num));
      
      if (g_pools[pool_num]->bounded_push(data))
        return;

      const bool can_free = !data_is_in_global_mem(data) && !THREADING_is_runner_thread() && !PLAYER_current_thread_has_lock();

      if (can_free) {
        ASAN_UNPOISON_MEMORY_REGION(data, g_offset_of_data + POOL_MEM_SIZE(pool_num));
        V_free(data);
        return;
      }
      
      for(int i = pool_num-1 ; i >= 0 ; i--)
        if (g_pools[i]->bounded_push(data))
          return;

#if !TEST_MAIN
      RT_message("RT_free failed. Who: \"%s\". pool_num: %d. Size: %d.", who, pool_num, POOL_MEM_SIZE(pool_num));
#endif

    }
    
  }
  
}


#ifdef TEST_MAIN

#include <thread>

#include "RT_Smartpointers.hpp"


struct RT_usage : public radium::RT_Class {
  int something = 90;
};

struct RT_usage2 : public radium::RT_Class {
  int something2[50] = {91};
};

static void test_RT_Smartpointers(void){
  //auto usage = radium::make_RT_shared_ptr<RT_usage>();
  radium::RT_shared_ptr<RT_usage> usage(new RT_usage);
  
  printf("A: %d (%d)\n", ATOMIC_GET(g_used_mem), usage.get()->something);
  
  radium::RT_shared_ptr usage2 = usage;
  
  radium::RT_shared_ptr usage3(new RT_usage2);
  
  radium::RT_shared_ptr usage4(usage3);

  radium::RT_scoped_ptr usage5(new RT_usage2);
  
  printf("B: %d. %d / %d\n", ATOMIC_GET(g_used_mem), usage2->something, usage4->something2[0]);
}

static void test_memory_alignment(void){
  std::vector<void*> allocated;
  
  for(int i = 1 ; i < g_max_mem_size  ; i++) {

    int size = 1 + rand()%4;
    
    void *mem = RT_alloc_raw(size, "test_memory_alignment");
    //printf("Mem: %p. Pos: %d. Max mem: %d\n", mem, int(((char*)mem)-g_mem), g_max_mem_size);
    
    if (!IS_ALIGNED(mem)) {
      printf(" Memory is not aligned. Mem: %p. Size: %d\n", mem, size);
      abort();
    }

    if (rand()%8)
      RT_free_raw(mem, "test_memory_alignment");
    else
      allocated.push_back(mem);
  }

  for(int i=0;i<(int)allocated.size();i++)
    if (allocated.at(i) != NULL)
      RT_free_raw(allocated.at(i), "test_memory_alignment2");
}


#define CURRENT_THREAD_HAS_LOCK 1
#define CURRENT_THREAD_DOES_NOT_HAVE_LOCK 0

static __thread int g_thread_type = -1;

bool PLAYER_current_thread_has_lock(void){
  return g_thread_type==CURRENT_THREAD_HAS_LOCK;
}

bool THREADING_is_runner_thread(void){
  return false;
}

static DEFINE_ATOMIC(int, g_highest_allocated) = 0;
static DEFINE_ATOMIC(int, g_allocated_total) = 0;

int main(){

  int seed = time(NULL);
  printf("Seed: %d\n", (int)seed);

  srand(seed);

  RT_mempool_init();

  g_thread_type = CURRENT_THREAD_HAS_LOCK; // If not we don't test the custom allocator.
  
  if (rand()%2==0) {
    test_memory_alignment();
    if (ATOMIC_GET(g_used_mem) != 0)
      abort();
  }
  
  if (rand()%2==0){
    test_RT_Smartpointers();
    if (ATOMIC_GET(g_used_mem) != 0)
      abort();
    //return 0;
  }

  const int num_main_iterations = 40 + rand()%25;

  int *highest_allocated = (int*)calloc(sizeof(int), num_main_iterations);
  
  for(int i=0;i<num_main_iterations;i++){
    
    const int num_threads = 2 + rand()%5;
    const int num_reader_iterations = 2 + rand()%20;

    if (ATOMIC_GET(g_used_mem) != 0) {
      fprintf(stderr, "Unused mem: %d\n", ATOMIC_GET(g_used_mem));
      abort(); // memleak
    }
    
    printf("  I: %d/%d (%d, %d).  Used global mem: %d / %d. Max used mem: %d. Total allocation: %d. Total malloc allocations: %d\n",
           i,
           num_main_iterations,
           num_threads,
           num_reader_iterations,
           ATOMIC_GET(g_curr_mem_size),
           TOTAL_MEM_SIZE,
           ATOMIC_GET(g_highest_allocated),
           ATOMIC_GET(g_allocated_total),
           ATOMIC_GET(g_total_malloc)
           );

    ATOMIC_SET(g_highest_allocated, 0);
    ATOMIC_SET(g_allocated_total, 0);
    ATOMIC_SET(g_total_malloc, 0);
    
    std::thread t[num_threads];
    
    for(int i=0;i<num_threads;i++){

      t[i] = std::thread([i, num_reader_iterations](){

          g_thread_type = i==0 ? CURRENT_THREAD_DOES_NOT_HAVE_LOCK : CURRENT_THREAD_HAS_LOCK; // Let first thread act as main thread.
          
          msleep(rand() % 2);
          
          double ms = TIME_get_ms();
          double wait = rand() % 50;

          do{
            for(int i2=0;i2<num_reader_iterations;i2++){

              int size = 2 + rand() % 512;

              radium::RT_Array<char> data(size, "test");

              ATOMIC_ADD(g_allocated_total, size);
              
              if (ATOMIC_GET(g_used_mem) > ATOMIC_GET(g_highest_allocated))
                ATOMIC_SET(g_highest_allocated, ATOMIC_GET(g_used_mem));
              
              int val = rand() % 127;
              
              for(int i3=0; i3 < size ; i3 ++)
                data[i3] = val;

              if (rand() % 1)
                msleep(2);

              for(int i3=0; i3 < size ; i3 ++)
                if (data[i3] != val){
                  fprintf(stderr, "i3: %d. data[i3]: %d. val: %d\n", i3, data[i3], val);
                  abort();
                }
            }
            
            if (rand() % 5)
              msleep(1);
            
          }while(TIME_get_ms() < ms+wait);
          
        });
      
    }

    for(int i=0;i<num_threads;i++)
      t[i].join();

    highest_allocated[i] = ATOMIC_GET(g_highest_allocated);
  }

  int64_t sum = 0;
  for(int i=0;i<num_main_iterations;i++)
    sum += highest_allocated[i];

  printf("Average highest: %f\n", (double)sum / (double)num_main_iterations);
  
  return 0;
}

#endif

