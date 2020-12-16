
#if defined(TEST_TIMEDATA_MAIN)

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <math.h>
#include <unistd.h>
#include <sys/time.h>


bool g_qtgui_has_stopped = false;

#define R_ASSERT(a) do{if(!(a))abort();}while(0)
#define R_ASSERT_RETURN_IF_FALSE(a) R_ASSERT(a)
#define R_ASSERT_RETURN_IF_FALSE2(a,b) R_ASSERT(a)
#define R_ASSERT_NON_RELEASE(a) R_ASSERT(a)
#define RError(...) abort()
#define CR_FORMATEVENT(a) "formt"

#if !defined(RELEASE)
static bool MIXER_is_saving(){
  return false;
}
#endif

static void PLAYER_lock(){
  abort();
}
  
static void PLAYER_unlock(){
  abort();
}

static __thread int g_thread_type = -1;

static bool PLAYER_current_thread_has_lock(void){
  return g_thread_type==1;
}
static bool THREADING_is_main_thread(void){
  return g_thread_type==0;
}

static bool RT_message(const char *a){
  abort();
}

static void msleep(int ms){
  usleep(1000*ms);
}

static double TIME_get_ms(void){
  struct timeval now;

  int err = gettimeofday(&now, NULL);
  if (err != 0)
    abort();

  return (double)now.tv_sec*1000.0 + (double)now.tv_usec/1000.0;
}

#define ASSERT_NON_RT_NON_RELEASE() R_ASSERT(THREADING_is_main_thread())

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

struct Gakk{
  Ratio _time;

  Gakk(int64_t a, int64_t b)
  {
    _time = make_ratio(a,b);
  }
};

int main(void){
  g_thread_type = 0;
  
  srand(time(NULL));
  
  const int num_main_iterations = 75 + rand()%50;
  int total_num_elements = 0;
  
  r::TimeData<Gakk> *gakk = new r::TimeData<Gakk>;
  
  {
    r::TimeData<Gakk>::Writer writer(gakk);
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

              r::TimeData<Gakk>::Reader reader(gakk);
              
              Ratio prev = make_ratio(0,1);
              
              for(auto gakk : reader){
                
                if (gakk._time < prev)
                  abort();
                
                prev = gakk._time;
              }
            }
            
            if (rand() % 1)
              msleep(1);
            
          }while(TIME_get_ms() < ms+wait);
        });
      
    }

    for(int i2=0;i2<num_writer_iterations;i2++){
      r::TimeData<Gakk>::Writer writer(gakk);

      if (total_num_elements < 0)
        abort();
      
      if (total_num_elements==0 || rand()%10 > 3) {
        
        Gakk gakk2(i2,i+1);
        writer.add(gakk2);

        total_num_elements++;
        
      } else {

        int pos = rand()%total_num_elements;
        Ratio ratio = writer.at(pos)._time;
        if (!writer.remove_at_time(ratio))
          abort();
        
        total_num_elements--;
      }
      
      msleep(rand() % std::min(i2+1, 10));
    }

    {
      r::TimeData<Gakk>::Reader reader(gakk);
      r::TimeData<Gakk>::Reader reader2(gakk);
      r::TimeData<Gakk>::Reader reader3(gakk);
      r::TimeData<Gakk>::Reader reader4(gakk);
      r::TimeData<Gakk>::Reader reader5(gakk);
      r::TimeData<Gakk>::Reader reader6(gakk);
      r::TimeData<Gakk>::Reader reader7(gakk);
    }

    for(int i=0;i<num_threads;i++)
      t[i].join();
  }

  {
    r::TimeData<Gakk>::Reader reader(gakk);
    reader.get_vector()->print_all_times();
    if (reader.size() != total_num_elements)
      abort();
  }

  delete gakk;

  if (g_num_timedata_vectors != 0){
    printf("Num unfreed TimeDataVectors: %d\n", g_num_timedata_vectors);
    abort();
  }
  
  printf("Success.\n");
  
  return 0;
}
#endif
