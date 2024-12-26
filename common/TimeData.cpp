
#if defined(TEST_TIMEDATA_MAIN)

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <math.h>
#include <unistd.h>
#include <sys/time.h>
#include <atomic>
#include <assert.h>

#include "nsmtracker.h"

/*
__attribute__((no_sanitize("undefined"))) extern "C" __int128_t
__muloti4(__int128_t a, __int128_t b, int* overflow) {
  const int N = (int)(sizeof(__int128_t) * CHAR_BIT);
  const __int128_t MIN = (__int128_t)1 << (N - 1);
  const __int128_t MAX = ~MIN;
  *overflow = 0;
  __int128_t result = a * b;
  if (a == MIN) {
    if (b != 0 && b != 1)
      *overflow = 1;
    return result;
  }
  if (b == MIN) {
    if (a != 0 && a != 1)
      *overflow = 1;
    return result;
  }
  __int128_t sa = a >> (N - 1);
  __int128_t abs_a = (a ^ sa) - sa;
  __int128_t sb = b >> (N - 1);
  __int128_t abs_b = (b ^ sb) - sb;
  if (abs_a < 2 || abs_b < 2)
    return result;
  if (sa == sb) {
    if (abs_a > MAX / abs_b)
      *overflow = 1;
  } else {
    if (abs_a > MIN / -abs_b)
      *overflow = 1;
  }
  return result;
}
*/

#include "../test/test_dummies.c"

#include "atomic.h"
# include "ratio_type.h"


#else

#include "nsmtracker.h"

#include "Mutex.hpp"

#endif


#include "TimeData.hpp"

#if !defined(RELEASE) || defined(TEST_TIMEDATA_MAIN)
int g_num_timedata_vectors = 0;
#endif

#if !defined(TEST_TIMEDATA_MAIN)

#if 0 // don't use it now, but it might be used in the future.

// FreeableList-stuff below here.
//

static radium::Mutex g_RT_free_list_lock;
static DEFINE_ATOMIC(const radium::FreeableList*, g_RT_free_list);

/*
void RT_FREEABLELIST_add(const radium::FreeableList *something){
  R_ASSERT_NON_RELEASE(PLAYER_current_thread_has_lock());
  
  // Only one thread can write to _RT_free_list at the time. Here we might be called from a runner.
  //
  // (ATOMIC_SET_RETURN_OLD isn't enough to make it thread safe. There are atomic lists though, but it's heavy, and we don't need that here.
  // It's much more efficient just holding the player lock a few ns in the main thread (in FREEABLELIST_free_all) those few times
  // the list is not empty.)
  radium::ScopedMutex lock(g_RT_free_list_lock);
  
  something->_next = ATOMIC_SET_RETURN_OLD(g_RT_free_list, something);
}
*/

// Beware that this function should be called very seldomly. Probably no need to optimize it.
void RT_FREEABLELIST_add_unique(const radium::FreeableList *something){
  R_ASSERT_NON_RELEASE(PLAYER_current_thread_has_lock());
  
  // Only one thread can write to _RT_free_list at the time. Here we might be called from a runner.
  //
  // (ATOMIC_SET_RETURN_OLD isn't enough to make it thread safe. There are atomic lists though, but it's heavy, and we don't need that here.
  // It's much more efficient just holding the player lock a few ns in the main thread (in FREEABLELIST_free_all) those few times
  // the list is not empty.)
  radium::ScopedMutex lock(g_RT_free_list_lock);

  const radium::FreeableList *l = ATOMIC_GET(g_RT_free_list);
  while(l != NULL)
    if (l==something)
      return;
  
  something->_next = ATOMIC_SET_RETURN_OLD(g_RT_free_list, something);
}

/*
void FREEABLELIST_free_all(void){
  R_ASSERT_NON_RELEASE(THREADING_is_main_thread());
  
  const radium::FreeableList *free = ATOMIC_GET(g_RT_free_list);
  if (free == NULL)
    return;
  
  {
    // Must have lock. The only reason _RT_free_list is atomic is because of the quick-test above (which makes sense because _RT_free_list is usually NULL).
    //radium::ScopedRealtimePriority prio; // FIX
    radium::ScopedMutex lock(g_RT_free_list_lock);
    
    free = ATOMIC_SET_RETURN_OLD(g_RT_free_list, NULL);
  }
  
  if (free == NULL){
    R_ASSERT_NON_RELEASE(false); // not sure
    return;
  }
  
  // We're probably not here very often.
  printf("\n\n\n==================Note: free_free_list not empty!===========\n\n\n\n");
  
  do{
    const radium::FreeableList *next = free->_next;
    delete free;
    free = next;
  } while(free != NULL);
}
*/

const radium::FreeableList *FREEABLELIST_transfer_all(void){
  const radium::FreeableList *free = ATOMIC_GET(g_RT_free_list);
  if (free == NULL)
    return NULL;

  // Note: We should be here very seldomly. Should be no need to optimize.
  
  {
    // Must have lock. The only reason _RT_free_list is atomic is because of the quick-test above (which makes sense because _RT_free_list is usually NULL).
    //radium::ScopedRealtimePriority prio; // FIX
    radium::ScopedMutex lock(g_RT_free_list_lock);
    
    return ATOMIC_SET_RETURN_OLD(g_RT_free_list, NULL);
  }
}
#endif
#endif



#ifdef TEST_TIMEDATA_MAIN

#include <thread>


struct Gakk1 : public r::TimeDataDataType<int>{
  Gakk1(int64_t a, int64_t b, int val = 1, int logtype = LOGTYPE_LINEAR)
    : TimeDataDataType(make_ratio(a,b), val, logtype)
  {
  }
};

int g_num_freed = 0;
int g_num_allocated = 0;

struct Gakk2 : public r::TimeDataDataTypeRef<int>{
  Gakk2(int64_t a, int64_t b, int val = 1, int logtype = LOGTYPE_LINEAR)
    : TimeDataDataTypeRef<int>(make_ratio(a,b), val, logtype)
  {
    g_num_allocated++;
  }
};

using GakkTimeData1 = r::TimeData<Gakk1, r::RT_TimeData_Player_Cache<typeof(Gakk1::_val)>>;

using GakkTimeData2 = r::TimeData<r::TimeData_shared_ptr<Gakk2>, r::RT_TimeData_Player_Cache<typeof(Gakk2::_val)>>;

#if TEST_SHARED_PTR

using Gakk = Gakk2;
using GakkTimeData = GakkTimeData2;

static r::TimeData_shared_ptr<Gakk> make_gakk(int64_t a, int64_t b, int val = 1, int logtype = LOGTYPE_LINEAR){
  r::TimeData_shared_ptr<Gakk> gakk(new Gakk(a,b,val,logtype));
  if (chance(0.5))
    return r::TimeData_shared_ptr<Gakk>(new Gakk(a,b,val,logtype));
  else
    return gakk;
}

static Ratio get_time(const r::TimeData_shared_ptr<Gakk> &gakk){
  return gakk->get_time();
}

#else

using Gakk = Gakk1;
using GakkTimeData = GakkTimeData1;

static Gakk make_gakk(int64_t a, int64_t b, int val = 1, int logtype = LOGTYPE_LINEAR){
  return Gakk1(a,b,val,logtype);
}

static Ratio get_time(const Gakk &gakk){
  return gakk.get_time();
}

#endif

//sjekk assigning av ny verdi til tidligere (altså sletta slot) i vector.

//#include <QVector>

static void test_refcount(void){
  printf("  T1\n");
  GakkTimeData2 refcountgakk;

  printf("  T2\n");
  
  {
    GakkTimeData2::Writer writer(&refcountgakk);
    printf("  T3a\n");

#if 1
    //RefCountGakk<Gakk2> val(new Gakk2(2,6,979));
    writer.add(new Gakk2(2,6,979));
    //std::move(val));
    
#elif 1

    radium::Vector<RefCountGakk<Gakk2>> ai;
    RefCountGakk<Gakk2> val(new Gakk2(2,6,979));
    ai.push_back2(std::move(val));

#elif 0
    std::vector<RefCountGakk<Gakk2>> ai;
    RefCountGakk<Gakk2> val(new Gakk2(2,6,979));
    ai.push_back(std::move(val));
#elif 0
    QVector<RefCountGakk<Gakk2>> ai;
    RefCountGakk<Gakk2> val(new Gakk2(2,6,979));
    ai.push_back(std::move(val));
#endif
    
    printf("  T3b\n");
    //-writer.add2(RefCountGakk(new Gakk2(3,6,980)));
    writer.add2(new Gakk2(4,6,981));
    writer.add2(new Gakk2(4,6,981));
    writer.add2(new Gakk2(4,6,981));
    printf("\n          Size1: %d\n", writer.size());
    writer.remove_at_pos(0);
    printf("          Size2: %d\n\n", writer.size());
    writer.clear();
    //writer.add2(RefCountGakk(new Gakk2(5,6,982)));
  }


  printf("  T4-1\n");
  
#if 1
  {
    GakkTimeData2::Writer writer(&refcountgakk);
    printf("  T4-2\n");
    //writer.add2(RefCountGakk(new Gakk2(5,6,982)));
    writer.remove_at_pos(0);
    printf("  T4-3\n");

    writer.clear();
    printf("  T4-4\n");
    writer.add2(new Gakk2(4,6,1981));
    printf("  T4-5\n");
  }
#endif

#if 1
    {
      GakkTimeData2::Writer writer(&refcountgakk);
      printf("  T4-2\n");
      writer.remove_at_pos(0);
      printf("  T4-3\n");
      writer.clear();
      writer.add2(new Gakk2(4,6,1982));
    }
    
  printf("  T4-6\n");
  
  {
    GakkTimeData2::Reader reader(&refcountgakk);
    printf("  T5\n");
    if (reader.size() > 0)
      printf("     Reader val: %d\n", reader.at_ref(0)->_val);
  }
#endif
  
  printf("  T6\n");
}



/*
static Gakk make_gakk(int64_t a, int64_t b, int val = 1, int logtype = LOGTYPE_LINEAR){
  Gakk gakk(a,b,val,logtype);
  return gakk;
}
*/

#define TEST(a, b)                  do{         \
  if (a!=b)                                     \
    abort();                                    \
  }while(0)


static void test_get_value(void){
  GakkTimeData gakk;
  
  {
    GakkTimeData::Writer writer(&gakk);
    writer.add(make_gakk(10,1,10)); // 0
    writer.add(make_gakk(20,1,20, LOGTYPE_HOLD)); // 1
    writer.add(make_gakk(30,1,30)); // 2
    writer.add(make_gakk(40,1,40)); // 3
    writer.add(make_gakk(40,1,50)); // 4
    writer.add(make_gakk(40,1,60)); // 5
    writer.add(make_gakk(40,1,70)); // 6
    writer.add(make_gakk(40,1,80)); // 7
    writer.add(make_gakk(40,1,90)); // 8
    writer.add(make_gakk(40,1,100)); // 9
    writer.add(make_gakk(40,1,110)); // 10
    writer.add(make_gakk(50,1,120)); // 11
    writer.add(make_gakk(60,1,130, LOGTYPE_HOLD)); // 12
  }

  GakkTimeData::Reader reader(&gakk);

  auto test_binsearch = [&reader](int time, int expect){
    int ret = reader.BinarySearch_Rightmost(make_ratio(time, 1), 1, reader.size()-1);
    printf("test_binsearch. time: %d. expect: %d. ret: %d.\n", time, expect, ret);
    TEST(ret, expect);
  };
  
  test_binsearch(10,1);
  test_binsearch(11,1);
  test_binsearch(19,1);
  test_binsearch(20,2);
  test_binsearch(30,3);
  test_binsearch(39,3);
  test_binsearch(40,11);
  test_binsearch(49,11);
  test_binsearch(50,12);
  test_binsearch(60,12);
  
  auto test_pos = [&reader](int time, int expect){
    int ret = reader.find_pos_for_get_value(make_ratio(time, 1));
    printf("test_pos. time: %d. expect: %d. ret: %d.\n", time, expect, ret);
    TEST(ret, expect);
  };

  test_pos(10,1);
  test_pos(11,1);
  test_pos(19,1);
  test_pos(20,2);
  test_pos(30,3);
  test_pos(39,3);
  test_pos(40,11);
  test_pos(49,11);
  test_pos(50,12);
  test_pos(59,12);

  bool random_cache;
  
  auto test_value = [&reader, &random_cache](int time, int expect, int do_binary_search){
    if (random_cache)
      reader._curr_pos = 1 + rand() % reader.size();

    int num_binarysearches = reader._num_calls_to_binarysearch;

    int prev_curr_pos = reader._curr_pos;
    
    FX_when when;
    int ret;
    bool legal = reader.get_value(0, make_ratio(time, 1), ret, when);
    if (!legal)
      ret = -1;

    if (!random_cache) {

      printf("test_value. time: %d. expect: %d. ret: %d. Prev curr_pos: %d\n", time, expect, ret, prev_curr_pos);
      
      if (do_binary_search==1)
        TEST(num_binarysearches + 1, reader._num_calls_to_binarysearch);
      else if (do_binary_search==0)
        TEST(num_binarysearches, reader._num_calls_to_binarysearch);
      
    }
    
    TEST(ret, expect);
  };
  
  auto testit = [&test_value](void){
    test_value(9,-1, -1);
    
    test_value(10,10, -1);
    test_value(11,11, 0);
    test_value(15,15, 0);
    test_value(19,19, 0);
    
    test_value(20,20, 0);
    test_value(21,20, 0);
    test_value(25,20, 0);
    test_value(29,20, 0);

    test_value(30,30, 0);
    test_value(39,39, 0);
    
    test_value(40,110, 1);
    
    test_value(41,111, 0);
    test_value(50,120, 0);
    test_value(59,129, 0);
    test_value(60,130, 0);
    
    test_value(61,-1, 0);

    test_value(10,10, 1);
  };

  random_cache = false;
  testit();

  random_cache = true;
  for(int i=0;i<1000;i++)
    testit();
}

int main(void){
  double start_time = get_ms();
  
  int seed = time(NULL);
  printf("Seed: %d\n", (int)seed);
  
  srand(seed);
  
  const int num_main_iterations = 75 + rand()%50;
  int total_num_elements = 0;
  
  test_refcount();
#if defined(TEST_REFCOUNTTIMEDATA)
  exit(0);
#endif
  
  test_get_value();
  
  GakkTimeData *gakk = new GakkTimeData;
  
  {
    GakkTimeData::Writer writer(gakk);
  }

  for(int i=0;i<num_main_iterations;i++){
    const int num_threads = 20 + rand()%50;
    const int num_reader_iterations = 2 + rand()%20;
    const int num_writer_iterations = 5 + rand()%40;
    printf("  I: %d/%d (%d, %d, %d). Num elements now: %d\n", i, num_main_iterations, num_threads, num_reader_iterations, num_writer_iterations, total_num_elements);

    std::thread t[num_threads];
    
    for(int i=0;i<num_threads;i++){
      
      t[i] = std::thread([gakk, num_reader_iterations](){

          g_thread_type = 1;

          msleep(rand() % 4);

          double ms = TIME_get_ms();
          double wait = rand() % 50;

          do{
            for(int i2=0;i2<num_reader_iterations;i2++){

              GakkTimeData::Reader reader(gakk);
              
              Ratio prev = make_ratio(0,1);
              
              for(auto gakk : reader){
                
                if (get_time(gakk) < prev)
                  abort();
                
                prev = get_time(gakk);
              }
            }
            
            if (rand() % 1)
              msleep(1);
            
          }while(TIME_get_ms() < ms+wait);
        });
      
    }

    for(int i2=0;i2<num_writer_iterations;i2++){
      GakkTimeData::Writer writer(gakk);

      if (total_num_elements < 0)
        abort();
      
      if (total_num_elements==0 || rand()%10 > 3) {
        
        writer.add(make_gakk(i2, i+1));

        total_num_elements++;
        
      } else {

        int pos = rand()%total_num_elements;
        Ratio ratio = get_time(writer.at(pos));
        if (!writer.remove_at_time(ratio))
          abort();
        
        total_num_elements--;
      }
      
      msleep(rand() % std::min(i2+1, 10));
    }

    {
      GakkTimeData::Reader reader(gakk);
      GakkTimeData::Reader reader2(gakk);
      GakkTimeData::Reader reader3(gakk);
      GakkTimeData::Reader reader4(gakk);
      GakkTimeData::Reader reader5(gakk);
      GakkTimeData::Reader reader6(gakk);
      GakkTimeData::Reader reader7(gakk);
    }

    for(int i=0;i<num_threads;i++)
      t[i].join();
  }

  {
    GakkTimeData::Reader reader(gakk);
    reader.get_vector()->print_all_times();
    if (reader.size() != total_num_elements)
      abort();
  }

  delete gakk;

  if (g_num_timedata_vectors != 0){
    printf("Num unfreed TimeDataVectors: %d\n", g_num_timedata_vectors); // Extremely rarely, this actually fails. Might be a bug in the program, might be something else. However, since it's so extremely seldom, and it's just a minor memory leak, it doesn't seem important to fix.
    abort();
  }

  assert(g_num_allocated==g_num_freed);
  assert(g_num_timedata_vectors==0);

  printf("Success. Duration: %f ms\n", (get_ms()-start_time) / 1000.0);
  
  return 0;
}
#endif
