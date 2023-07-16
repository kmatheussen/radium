/* Copyright 2020 Kjetil S. Matheussen

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



/*
  DESIGN
  ======
  * Several readers (up to 128 simultaneous threads can read at the same time (see "MAX_NUM_READERS" in AtomicPointerStorage.hpp.))
  * One writer
  * The readers don't have to be synchronized.
  * Any reader can be a realtime thread, but it doesn't have to be.
  * Writer thread can not be a realtime thread.
  * DataType must provide the two methods 'get_time' and 'set_time' (and sometimes 'get_val'),
    which is usually done by subclassing r::TimeDataDataType.
    'get_time' and 'set_time' operates on 'Ratio', but other types can probably be supported by
    using a template, if necessary, later.
  * The underlying vector is sorted by the result of calling get_time().
  * DataType should ideally be a trivial datatype (i.e. a POD). If that is not possible,
    it's probably best to use r::TimeData_shared_ptr instead.
    "r::TimeData_shared_ptr" is a special type of shared-pointer. The important differences, compared to std::shared_ptr, are:
    1. r::TimeData_shared_ptr doesn't allocate or delete memory in a RT thread.
    2. r::TimeData_shared_ptr implements 'get_time', 'set_time', and 'get_val'.
    3. r::TimeData_shared_ptr requires the template type to include the field 'std::atomic<int> _num_references'.
       (This field is used to avoid having to allocate an extra memory block for the reference counter (which complicates code).)
  * Accessing a random element (given time) is O(Log(n)). However, when accessing a sequential element,
    which we usually do, lookup is usually O(1), and it's also sped up more if cache_num >= 0.


  HOW IT WORKS
  ============
  * We never modify the underlying vector.
    We only modify a COPY of the underlying vector, and when finished, the underlying vector is atomically replaced with the modified copy.
  * To make sure making a copy of the underlying vector doesn't take too much time, the element type ("DataType") should not be too big.
    If DataType is very big, use r::RefCountTimeDate instead to store a shared-pointer to the actual data.
  * A Reader object is optimized to get data in a linear increasing fashion,
    i.e. starting with a low time and ending on a high time, always increasing time from call to call. (This is not a requirement though, but when
    time is not increasing, it's necessary to do a O(log N) binary search to find the new index. This can be important to avoid in RT code.)
    If cache_num >= 0, the index is also remembered in the TimeData object itself,
    further decreasing the need to do a binary search in a realtime thread.



  TESTS
  =====
  TimeData is tested by running "make test_refcounttimedata", "make test_radium_vector", and "make test_timedata".


  NOTES
  =====
  * Writing is a heavy operation since it recreates the underlying vector. Therefore, nothing is actually written until the Writer
    object is deleted, and it makes sense to create as few writer objects as possible.
  * TimeData is very similar to SeqAutomation. Both datatypes support multithreaded reading and writing. Differences:
    1. TimeData supports 128 simultaneous readers, all of them can be RT. SeqAutomation only supports 1 RT reader
       and 1 main thread reader (i.e. the writer can also be used as a reader).
    2. TimeData uses Ratio as time type. SeqAutomation uses double.
    3. TimeData provides a wrapper around radium::Vector for writing, while SeqAutomation provides a wrapper around QVector.
    4. TimeData uses radium::Vector also for reading (in fact the same vector used for writing), while SeqAutomation only uses a plain array for reading.
 */

#pragma once

#define RADIUM_COMMON_TIMEDATA_HPP 1

#include <memory>

#include "Vector.hpp"
#include "ratio_funcs.h"
//#include "sequencer_proc.h"
#include "AtomicPointerStorage.hpp"

#define MAX_NUM_PARALLEL_TIMEDATAS 64 // If using more than this, caching is disabled for the extra timedatas, and performance will be slightly worse for those.

/*
// "FreeableList" is it's own thing, but I don't bother making a new file for it. Especially since it's likely to be only used by TimeData.
//

// Was used in an early version of TimeData. Don't use it now, but it might be used in the future.

namespace radium{

struct FreeableList{
  mutable const FreeableList *_next;
  virtual ~FreeableList(){ // Must be virtual to make sure the destructor of the child class is called. (It's insane that c++ doesn't do this automatically)
  }
};
}

extern void RT_FREEABLELIST_add(const radium::FreeableList *something);
extern void RT_FREEABLELIST_add_unique(const radium::FreeableList *something);
extern void FREEABLELIST_free_all(void);
extern const radium::FreeableList *FREEABLELIST_transfer_all(void);
*/

#if defined(TEST_TIMEDATA_MAIN) || defined(TEST_RADIUM_VECTOR_MAIN)
extern int g_num_timedata_vectors;
extern int g_num_allocated;
extern int g_num_freed;
#endif

namespace r{

class CacheNumHolder{
  
  radium::Vector<int> _used;
  radium::Vector<int> _free;

  int _last_play_id = -1;
  
public:

#define ASSERT_SIZE() R_ASSERT_NON_RELEASE(_free.size() + _used.size() == MAX_NUM_PARALLEL_TIMEDATAS);

  CacheNumHolder(){
    for(int i = MAX_NUM_PARALLEL_TIMEDATAS-1 ; i >= 0 ; i--)
      _free.push_back(i);
    
    _used.reserve(MAX_NUM_PARALLEL_TIMEDATAS);

    R_ASSERT(_free.size()==MAX_NUM_PARALLEL_TIMEDATAS);
    R_ASSERT(_used.size()==0);

    ASSERT_SIZE();
  }

  /*
  int RT_get_play_id(void) const {
    return _last_play_id;
  }
  */
  
  int RT_obtain(int play_id){
    ASSERT_SIZE();
    
    R_ASSERT_NON_RELEASE(THREADING_is_player_thread());
    
    if (play_id != _last_play_id) {

      _last_play_id = play_id;

      if (!_used.is_empty()) {

        int ret = _used.at(0); // Just reuse the first one. All of them were used so it doesn't matter CPU cache-vice which one to pick.
        
        for(int pos = 1 ; pos < _used.size() ; pos++) // Note: Usually _used.size() should not be large. Probably no need to optimize this.
          _free.push_back(_used.at(pos));

        _used.set_num_elements(1);

        ASSERT_SIZE();
        
        return ret;

      }
    }
    
    if (_free.is_empty()){
      R_ASSERT_NON_RELEASE(false); // Note: Only an error if a block is played more than MAX_NUM_PARALLEL_TIMEDATAS times simultaneously.
      return -1;
    }

    int ret = _free.pop_back();
    _used.push_back(ret);

    ASSERT_SIZE();
    
    return ret;
  }

  void RT_release(int num){
    ASSERT_SIZE();

    R_ASSERT_NON_RELEASE(THREADING_is_player_thread());
    
    if (num==-1){
      R_ASSERT_NON_RELEASE(false); // Note: Only an error if a block is played more than MAX_NUM_PARALLEL_TIMEDATAS times simultaneously.
      return;
    }

#if !defined(RELEASE)
    if (!_used.contains(num))
      abort();
    if (_free.contains(num))
      abort();
#endif

    _used.remove(num, false);
    _free.push_back(num);
    ASSERT_SIZE();
  }

#undef ASSERT_SIZE
};


  /*
enum class DataTypeReturnType{
  VALUE_OK,
  NO_VALUES_YET,
  NO_VALUES,
  LAST_VALUE,
};
  */



template <class SeqBlockT>
struct RT_TimeData_Cache_Handler{

  SeqBlockT *_cache;
  int64_t _play_id;

  using ValType = decltype(SeqBlockT::_last_value); //typename SeqBlockT::RT_TimeData_Player_Cache::ValType;
  
  RT_TimeData_Cache_Handler(SeqBlockT *cache, int64_t play_id)
    : _cache(cache)
    , _play_id(play_id)
  {
  }

  ValType get_value(void) const {
    if (_cache==NULL){
      R_ASSERT_NON_RELEASE(false);
      return -1;
    }else
      return _cache->_last_value;
  }
  
  bool has_value(void) const {
    if (_cache==NULL)
      return false;
    if (_play_id != _cache->_last_play_id)
      return false;

    return true;
  }
  
  static bool is_same_value(ValType value1, ValType value2) {    
#if defined(__GNUC__) && __GNUC__ < 9
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wfloat-equal"
#endif
    
    return (std::is_same<ValType, int>::value
      ? value1==value2
      : (std::is_same<ValType, float>::value
         ? equal_floats(value1, value2)
         : equal_doubles(value1, value2)
         ));

#if defined(__GNUC__) && __GNUC__ < 9
#pragma GCC diagnostic pop
#endif
  }
  
  bool is_same_value(ValType value) const {
    return has_value() && is_same_value(value, _cache->_last_value);
  }

  void update_value(ValType value) {
    if (_cache==NULL)
      return;

    _cache->_last_value = value;
    _cache->_last_play_id = _play_id;
  }

};
  

template <typename ValType>
class IterateCallback {
public:

  virtual ~IterateCallback(){
  }
  
  virtual void callback(struct SeqTrack *seqtrack,
                        const struct SeqBlock *seqblock,
                        const struct Tracks *track,
                        int index,
                        ValType val,
                        int64_t time,
                        FX_when when,
                        bool is_new_node) const = 0;
};

template <typename ValType>
struct TimeDataSimpleNode : public TimeDataDataType<ValType> {
  TimeDataSimpleNode(Ratio time, ValType val, int logtype = LOGTYPE_LINEAR)
    : TimeDataDataType<ValType>(time, val, logtype)
  {}
};

enum class KeepOldData {
  KEEP_OLD_DATA,
  USE_CLEAN_DATA
};
    


/*
  SeqBlockT must be a subclass of RT_TimeData_Player_Cache.

  The SeqBlockT class must contain both any seqblock-specific data needed when playing, and the cache data in RT_TimeData_Player_Cache.
  It doesn't make sense to unlink these two types since the cache data is always seqblock-specific.
  Cache data must be seqblock-specific in order to correctly play the same block at the same time in more than one seqtrack.
  Maybe SeqBlockT should have been named SeqblockT_And_RT_Cache though, which would have been a clearer, but less correct, name.
*/

template <class T, class SeqBlockT>
class TimeData {

  static_assert(std::is_base_of<TimeData_shared_ptr<typename T::TType>, T>::value
                ||
                (  std::is_trivially_destructible<T>::value
                   &&
                   std::is_trivially_copyable<T>::value
                   &&
                   (  std::is_base_of<TimeDataDataType<typename T::TType>, T>::value
                      ||
                      std::is_base_of<TimeDataDataTypeNoVal, T>::value
                   ) 
                ),
                "T must be either 1: r::TimeData_shared_ptr<T>, or 2: pretty trival and subclass of either r::TimeDataDataType or r::TimeDataDataTypeNoVal)");

#if !defined(RELEASE)
  mutable int _binsearch=0;  
#endif

public:
  
  using RT_CacheHandler = RT_TimeData_Cache_Handler<SeqBlockT>;
  
  struct TimeDataVector : public radium::Vector<T> {

    TimeDataVector(TimeDataVector *vector)
      : radium::Vector<T>(vector)
    {
      R_ASSERT_NON_RELEASE(THREADING_is_main_thread());
#if defined(TEST_TIMEDATA_MAIN) || defined(TEST_RADIUM_VECTOR_MAIN)
      g_num_timedata_vectors++;
#endif
    }

    TimeDataVector()
      : TimeDataVector(NULL)
    {
    }
    
    ~TimeDataVector() {
      R_ASSERT_NON_RELEASE(THREADING_is_main_thread());
#if defined(TEST_TIMEDATA_MAIN) || defined(TEST_RADIUM_VECTOR_MAIN)
      g_num_timedata_vectors--;
      //printf("                 FREEEING TimeDataVector %d\n", g_num_timedata_vectors);
      //getchar();
#endif
    }

    void assert_sorted(void) const {
#if !defined(RELEASE)
      if (this->size() > 0){
        Ratio prev = this->at_ref(0).get_time();
        for(int i=1;i<this->size();i++){
          if (this->at_ref(i).get_time() < prev)
            abort();
          prev = this->at_ref(i).get_time();
        }
      }
#endif
    }

    void print_all_times(void) const {
      printf("--------TimeData start. Size: %d\n", this->size());
      for(int i=0;i<this->size();i++)
        printf("   %d: %f  (%d / %d)\n", i, make_double_from_ratio(this->at(i).get_time()), (int)this->at(i).get_time().num, (int)this->at(i).get_time().den);
      printf("--------TimeData end.\n\n");
    }
    
    void sortit(void) {
      //printf("  Sorting. is_sorted: %d\n", _is_sorted);
      //printf("Before:\n");
      //print_all_times();
      //printf("  >>> ==== Sorting start >>>\n");
      this->sort([](const T &a, const T &b){
        //printf("  COMP called %p %p\n", &a, &b);
        return a.get_time() < b.get_time();
      });
      //printf("  <<< ===== Sorting end <<<\n");
      
      //printf("After:\n");
      //print_all_times();
      assert_sorted();
    }
  
  };


private:

  TimeDataVector* _vector;

  mutable radium::AtomicPointerStorageMultipleReaders<const TimeDataVector> _atomic_pointer_storage;

  mutable SeqBlockT _player_caches[MAX_NUM_PARALLEL_TIMEDATAS];

  
public:
  
  TimeData(void)
    : _vector(new TimeDataVector())
  {
    R_ASSERT_NON_RELEASE(THREADING_is_main_thread());
    _atomic_pointer_storage.set_new_pointer(_vector);
  }

  
  virtual ~TimeData(){
    R_ASSERT_NON_RELEASE(THREADING_is_main_thread());
  }

  // Overridden at least by Notes.
  virtual void sortit(TimeDataVector *vector){
    vector->sortit();
  }
  
private:

  // When finished, we must always call 'replace_vector' or delete, on the return value.
  TimeDataVector *get_write_vector(KeepOldData keep){
    R_ASSERT_NON_RELEASE(THREADING_is_main_thread());
    return new TimeDataVector(keep==KeepOldData::USE_CLEAN_DATA ? NULL : _vector);
  }

  // Called by the writer.
  void replace_vector(TimeDataVector *vector){
    R_ASSERT_NON_RELEASE(THREADING_is_main_thread());
    R_ASSERT(!PLAYER_current_thread_has_lock());
    
    R_ASSERT_NON_RELEASE(vector != _vector);

    _vector = vector;
    _atomic_pointer_storage.set_new_pointer(_vector);
  }

  
  template <class TimeData, class TimeDataVector>
  class ReaderWriter{

    ReaderWriter(const ReaderWriter&) = delete;
    ReaderWriter& operator=(const ReaderWriter&) = delete;

  protected:

    TimeData *_time_data;

    TimeDataVector *_vector;
    
    SeqBlockT *_player_cache;

#if defined(TEST_TIMEDATA_MAIN)
  public:
#endif
    
    const int _cache_num; // is -1 if not a player.
    int &_curr_pos; // caching read position
    mutable int _non_player_curr_pos = 0;
    
  private:
    
    int BinarySearch_Left_exact(const T *array, const Ratio ratio, const int low, const int high, bool &found_exact) const {   // initially called with low = 0, high = N - 1

#if defined(TEST_TIMEDATA_MAIN)
      _num_calls_to_binarysearch++;
#endif
      
      // invariants: ratio  > A[i] for all i < low
      //             ratio <= A[i] for all i > high
      if (high < low){ // Pretty sure we can use <= instead of < here...
        found_exact = false;
        return low;
      }
      
      const int mid = (low + high) / 2;
      const Ratio time = array[mid].get_time();
      
      if (time == ratio){
        found_exact = true;
        return mid;
      }else if (time > ratio)
        return BinarySearch_Left_exact(array, ratio, low, mid-1, found_exact);
      else
        return BinarySearch_Left_exact(array, ratio, mid+1, high, found_exact);
    }

    int BinarySearch_Left_exact(const Ratio ratio, bool &found_exact) const {
      return BinarySearch_Left_exact(_vector->get_array(), ratio, 0, size()-1, found_exact);
    }

    // Made by looking at https://en.wikipedia.org/wiki/Binary_search_algorithm#Duplicate_elements
    int BinarySearch_Rightmost(const T *array, const Ratio ratio, int low, int high) const {
      R_ASSERT_NON_RELEASE(this->size() >= 2);
      R_ASSERT_NON_RELEASE(low >= 1);
      R_ASSERT_NON_RELEASE(high < this->size());

#if defined(TEST_TIMEDATA_MAIN)
      _num_calls_to_binarysearch++;
#endif

#if !defined(RELEASE)
      printf("   DOING BINARYSEARCH %d. Low: %d. High: %d. Cache #%d\n", _time_data->_binsearch++, low, high, _cache_num);
#endif
      
      while(low<high){
        const int mid = (low+high)/2;
        const Ratio mid_time = array[mid].get_time();
        if (mid_time > ratio)
          high = mid;
        else
          low = mid+1;
      }
      return high; // (normally this algorithm returns high-1 here, but we just want the high valule)
    }
    
#if defined(TEST_TIMEDATA_MAIN)
  public:
#endif
    
    int BinarySearch_Rightmost(const Ratio ratio, const int low, const int high) const {
      R_ASSERT_NON_RELEASE(this->size() >= 2);
      R_ASSERT_NON_RELEASE(low >= 1);
      R_ASSERT_NON_RELEASE(high < this->size());
      
      return BinarySearch_Rightmost(_vector->get_array(), ratio, low, high);
    }
    
  protected:

    // Returns index at, or before, there is an element at 'ratio'
    // If there isn't any elements before 'ratio', the function will return 0.
    //
    // Note: Due to the nature of binary search, we should not trust the returned value
    // to be optimal (because we don't want a headache thinking through whether the functions
    // are behaving 100% correct all the time). Only trust that the returned value
    // is lower or equal to the index of the first element that has a ratio of at least 'ratio'.
    // (in other words, only use this function as an optimization, but don't expect the result to be optimal.)
    int find_pos_left(const Ratio &ratio) const {
      if (is_empty())
        return 0;
      
      bool found_exact;
      int ret = BinarySearch_Left_exact(ratio, found_exact);

      if (ret == 0)
        return 0;
      
      ret--;
      
      if (ret >= size()) {
        R_ASSERT(false);
        return size()-1;
      }

      R_ASSERT_NON_RELEASE(at(ret).get_time() <= ratio);
      R_ASSERT_NON_RELEASE(ret==0 || at(ret-1).get_time() < ratio);
      
      return ret;
    }
    
      
    // returns -1 if not found.
    int find_pos_exact(const Ratio &ratio) const {
      R_ASSERT(_cache_num==-1);
      
      bool found_exact;

      _curr_pos = BinarySearch_Left_exact(ratio, found_exact);

      if (found_exact)
        return _curr_pos;
      else
        return -1;
    }

#if defined(TEST_TIMEDATA_MAIN)
  public:
    mutable int _num_calls_to_binarysearch = 0;
#endif
    
    // Only used from get_value, used by the player. Should be as fast as possible.
    // Note that ratio is always legal.
    // TODO: Let this function take "T &t1, T &t2" as arguments as well.
    int find_pos_for_get_value(const Ratio &ratio) const {

      int das_size = size();
      
      R_ASSERT_NON_RELEASE(das_size >= 2);
      R_ASSERT_NON_RELEASE(ratio >= _vector->at_ref(0).get_time());
      R_ASSERT_NON_RELEASE(ratio < _vector->at_ref(das_size-1).get_time());
      
      int curr_pos = R_MAX(_curr_pos, 1);
        
      if (curr_pos < das_size) {
        
        R_ASSERT_NON_RELEASE(curr_pos>=0);

        //printf(" F1. curr_pos: %d. ratio: %f. [curr_pos].get_time(): %f\n", curr_pos, make_double_from_ratio(ratio), make_double_from_ratio(_vector->at_ref(curr_pos).get_time()));
        
        if (ratio >= _vector->at_ref(curr_pos-1).get_time()) {

          //printf(" F2\n");
          // Hopefully the compiler is able to hyper-optimize this block.

          //curr_pos--;
          
          for(int i=0 ; i < 4; i++, curr_pos++) {
            R_ASSERT_NON_RELEASE(curr_pos < das_size);
            
            const T &next = _vector->at_ref(curr_pos);

            //printf(" F3 %d\n", i);
            
            if (ratio < next.get_time()){
              _curr_pos = curr_pos;
              //printf(" F4 %d\n", _curr_pos);
              return _curr_pos;
            }
          }
          
          _curr_pos = BinarySearch_Rightmost(ratio, curr_pos, das_size-1);
          //printf("   F5 %d\n", _curr_pos);
          return _curr_pos;
          
        } else {

          _curr_pos = BinarySearch_Rightmost(ratio, 1, curr_pos);
          //printf("   F6 %d\n", _curr_pos);
          return _curr_pos;          
          
        }
        
      } else {

        _curr_pos = BinarySearch_Rightmost(ratio, 1, das_size-1);
        //printf("   F7 %d\n", _curr_pos);
        return _curr_pos;

      }
      
    }

    /*
    // not used.
    void update_curr_pos(const Ratio &ratio) const {
      find_pos_for_get_value(ratio);
    }
    */
    
  public:
    
    ReaderWriter(TimeData *time_data, TimeDataVector *vector, const int cache_num)
      : _time_data(time_data)
      , _vector(vector)
      , _cache_num(cache_num)
      , _curr_pos(cache_num >= 0 ? time_data->_player_caches[cache_num]._curr_pos : _non_player_curr_pos)
    {
      R_ASSERT_NON_RELEASE(cache_num < MAX_NUM_PARALLEL_TIMEDATAS);
      R_ASSERT_NON_RELEASE(cache_num==-1 || THREADING_is_player_thread());

    }

    const T* begin() const {
      return this->_vector->begin();
    }
    
    const T* end() const {
      return this->_vector->end();
    }

    struct Iterator{

      T *_begin;
      T *_end;
      
      Iterator(T *begin, T *end)
        : _begin(begin)
        , _end(end)
      {
      }
      
      T* begin() const {
        return _begin;
      }

      T* end() const {
        return _end;
      }
    };

    // (The comment for 'find_pos_left' applies to this function as well)
    Iterator get_iterator_left(const Ratio &ratio) const {
      T *end = const_cast<T*>(this->_vector->end());
      
      if (is_empty())
        return Iterator(end, end);
      
      int pos = find_pos_left(ratio);
      return Iterator(&this->_vector->at_ref(pos), end);
    }

    /*
    ~ReaderWriter(){
      if (_is_player)
        _time_data->_RT_player_curr_pos = _curr_pos;
    }
    */
    const T &at_ref(int i) const {
      return _vector->at_ref(i);
    }
    
    const T at(int i) const {
      return _vector->at(i);
    }
    
    const T operator[](int i) const {
      return at(i);
    }

    int size(void) const {
      return _vector->size();
    }

    bool is_empty(void) const {
      return _vector->is_empty();
    }

    bool contains(const T &element) {
      for(int i=0;i<this->size();i++)
        if (at_ref(i) == element)
          return true;

      return false;
    }

    int find_element_at_ratio(const Ratio &ratio) const {
      return find_pos_exact(ratio);
    }
    
    bool has_element_at_ratio(const Ratio &ratio) const {
      return find_pos_exact(ratio) >= 0;
    }
    
    bool has_element_between(const Ratio &begin, const Ratio &end) const {
      for(const T &t : *_vector)
        if (t.get_time() >= begin && t.get_time() < end)
          return true;
      return false;
    }
    
    const TimeDataVector *get_vector(void) const {
      return _vector;
    }

      //    TODO: Reuse tests from SeqAutomation.
    

  private:
    
    double get_value_raw(const Ratio &ratio, const int das_size) const {
      //printf("Get value raw, start 1.\n");
      //int old = _curr_pos;
      int index = find_pos_for_get_value(ratio);
      //printf("...Result: %d -> %d, %d\n\n", old, index, _curr_pos);
      
      R_ASSERT_NON_RELEASE(index > 0);
      R_ASSERT_NON_RELEASE(index < das_size);
      
      const T &t1 = at_ref(index-1);
      const T &t2 = at_ref(index);
      
#if !defined(RELEASE)
      if (ratio < t1.get_time())
        abort();
      
      if (ratio > t2.get_time())
        abort();
#endif
        
      if (t1.get_logtype()==LOGTYPE_HOLD){

        return t1.get_val();
        
      } else {

        /*
        printf("Value RAW. index: %d. Ratio: %f. times: %f / %f. Values: %d / %d\n", index, make_double_from_ratio(ratio),
               make_double_from_ratio(t1.get_time()), make_double_from_ratio(t2.get_time()),
               t1.get_val(), t2.get_val());
        */
        
        if (t1.get_time()==t2.get_time()){  // Might happen at last node, not sure.
          R_ASSERT_NON_RELEASE(false);
          return t2.get_val();
        }else
          return scale_double(make_double_from_ratio(ratio),
                              make_double_from_ratio(t1.get_time()), make_double_from_ratio(t2.get_time()),
                              t1.get_val(), t2.get_val());
      }
    }

  public:
    
    bool period_is_inside(const r::RatioPeriod &period) const {
      const int das_size = size();
      if (das_size<2)
        return false;
      
      const T &first_t = _vector->at_ref(0);
      
      if (period._end < first_t.get_time())
        return false;
      
      const T &last_t = _vector->at_ref(das_size-1);
      
      if (period._start >= last_t.get_time())
        return false;

      return true;
    }

    SeqBlockT *get_player_cache(void) const {
      return _cache_num < 0 ? NULL : &_time_data->_player_caches[_cache_num];
    }

    // Same as calling get_value, sort of, but also makes sure all values positioned at nodes inside 'period' are sent out.
    template <typename ValType>
    void iterate(struct SeqTrack *seqtrack,
                 const struct SeqBlock *seqblock,
                 const struct Tracks *track,
                 int play_id,
                 const int64_t seqtime_start,
                 const r::RatioPeriod &period,
                 const IterateCallback<ValType> &callback) const
    {
      R_ASSERT_NON_RELEASE(period._end >= period._start);
  
      if (!period_is_inside(period))
        return;

      //int prev_curr_pos = _curr_pos;
        
      const int das_size = size();
      const T &first_t = at_ref(0);
      const T &last_t = at_ref(das_size-1);

      RT_CacheHandler cache(get_player_cache(), play_id);

      int old_pos = _curr_pos;
      
      bool has_prev_value;
      double prev_value;

      // Find previous value
      {
        if (period._start.num==0 || period._start < first_t.get_time()) { // Note: period._start==0 when it's the first call to block.

          prev_value = 0.0; // Not necessary. Only to silence compiler error. (Usually I have the opposite problem, that it won't give error when using uninitialized value. Sigh. Why don't the gcc and clang people prioritize to get this right? It seems far more important than minor optimizations for instance.)
          has_prev_value = false;
    
        } else {
      
          if (cache.has_value()) {
        
            prev_value = cache.get_value();
        
          } else {

            // Approximately. (This is a corner case,
            // even if this value is totally wrong, no one would probably notice, and if they did it would be extremely seldom.)
            Ratio ratio_prev = period._start - (period._end-period._start);
            
            if (ratio_prev < first_t.get_time()) {
              prev_value = first_t.get_val();
            } else {
              prev_value = get_value_raw(ratio_prev, das_size);
            }
        
          }
      
          has_prev_value = true;
        }
      }

      int64_t value_time;
      ValType value;
      FX_when when;

      // Find value at period._start
      {
        if (period._end >= last_t.get_time()){

          value_time = get_seqblock_ratio_time2(seqblock, track, last_t.get_time());
          value = last_t.get_val();
          when = FX_end;

        } else if (period._start.num==0 || period._start < first_t.get_time()) { // Note: period._start==0 when it's the first call to block.

          value_time = get_seqblock_ratio_time2(seqblock, track, first_t.get_time());
          value = first_t.get_val();
          when = FX_start;
          _curr_pos = 1;
      
        } else {

          value_time = seqtime_start;
          value = get_value_raw(period._start, das_size); // get_value_raw updates _curr_pos.
          when = FX_middle;

#if !defined(RELEASE)
          int curr_pos = _curr_pos;      
          R_ASSERT_NON_RELEASE(curr_pos == find_pos_for_get_value(period._start));
#endif
        }
      }

      const bool same_value_as_last_time = has_prev_value && cache.is_same_value(prev_value, value);

      if (when==FX_start || when==FX_end || !same_value_as_last_time){
        /*
          if (0) {
          printf("....1. %d: %f. When: %d. _curr_pos: %d\n", (int)value_time, (double)value / (double)fx->max, (int) when, _curr_pos);
          if (when==FX_middle){
          auto node = _vector->at_ref(_curr_pos-1);
          auto value_time = get_seqblock_ratio_time2(seqblock, track, node.get_time());
          printf("........ time last node: %d. Value last node: %f\n", (int)value_time, (double)node.get_val() / (double)fx->max);
          }
          }
        */
        //printf("Callback 1. pos: %d -> %d\n", old_pos, _curr_pos);
        callback.callback(seqtrack, seqblock, track, when==FX_start ? 0 : when==FX_end ? das_size-1 : _curr_pos-1, value, value_time, when, when==FX_start || when==FX_end || old_pos != _curr_pos);
      }

      // Send out all node values between period._start and period._end
      if (when != FX_end) {
    
        for( ; _curr_pos < das_size ; _curr_pos++) {

          const T &node = at_ref(_curr_pos);

#if 0
          {
            auto node_time = get_seqblock_ratio_time2(seqblock, track, node.get_time());
            auto end_time = get_seqblock_ratio_time2(seqblock, track, node.get_time());
            printf("............(2) _curr_pos: %d. node(_curr_pos) time: %d. end_time: %d. node ratio: %d / %d. end ratio: %d / %d\n", _curr_pos, (int)node_time, (int)end_time,
                   (int)node.get_time().num, (int)node.get_time().den, 
                   (int)period._end.num, (int)period._end.den);
          }
#endif
          
          // (maybe) FIX: The correct test here is actually node.get_time() >= period._end, and not node.get_time() > period._end.
          // However, because of rounding errors, notes can be sent out in the block before an fx node at the same position.
          // And it's quite important that fx are sent out before note start, for instance if setting start position of a sample (common in MOD files).
          // Afters notes have been converted to TimeData, this test should probably be corrected.
          if (node.get_time() > period._end)
            break;
      
          value = node.get_val();
      
          when = (_curr_pos == das_size-1) ? FX_end : FX_middle;

          int64_t time = get_seqblock_ratio_time2(seqblock, track, node.get_time());
          //printf("....2. %d: %f. When: %d. _curr_pos: %d\n", (int)value, (double)value, (int) when, _curr_pos);
          callback.callback(seqtrack, seqblock, track, _curr_pos, value, time, when, true);

        }
      }

      cache.update_value(value);
    }

    // Same as iterate, but also handles one external node placed before first node, and one external node placed after last node.
    // Used for handling note velocities and note pitches.
    // Implemented in notes.cpp
    template <typename ValType>
    void iterate_extended(struct SeqTrack *seqtrack,
                          const struct SeqBlock *seqblock,
                          const struct Tracks *track,
                          int play_id,
                          
                          const int64_t seqtime_start,
                          const r::RatioPeriod &period,
                          
                          const r::IterateCallback<ValType> &callback,
                          
                          const r::TimeDataSimpleNode<ValType> &node_start,
                          const r::TimeDataSimpleNode<ValType> &node_end
                          ) const;

    
    template <typename ValType>
    bool get_value(int play_id, const Ratio &ratio, ValType &value, FX_when &when) const {
      static_assert(std::is_same<ValType, int>::value || std::is_same<ValType, float>::value || std::is_same<ValType, double>::value, "ValType should be int, float, or double");

      const int das_size = size();
      const T &first_t = _vector->at_ref(0);
      const T &last_t = _vector->at_ref(das_size-1);

      double curr_value;
      
      if (ratio > last_t.get_time()) {
        
        return false;

      } else if (ratio < first_t.get_time()) {
        
        return false;
      
      } else if (ratio == last_t.get_time()){
        
        curr_value = last_t.get_val();
        when = FX_end;
        
      } else {

        if (ratio == first_t.get_time())
          when = FX_start;
        else
          when = FX_middle;

        curr_value = get_value_raw(ratio, das_size);
        
      }

      RT_CacheHandler cache(get_player_cache(), play_id);

      cache.update_value(curr_value);

      value = curr_value;
      
      return true;
    }

};
  
  
public:

  // Optimized reader if reading the vector in a timely linear fashion (cache_num should be supplied in RT code).
  class Reader
    : private radium::RT_AtomicPointerStorageMultipleReaders_ScopedUsage<const TimeDataVector>
    , public ReaderWriter<const TimeData, const TimeDataVector>
  {
    
    // We can probably make this work, but if trying to copy a Reader, it's most likely an error.
    Reader(const Reader&) = delete;
    Reader& operator=(const Reader&) = delete;

   public:
    
    Reader(const TimeData *time_data, const int cache_num = -1)
      : radium::RT_AtomicPointerStorageMultipleReaders_ScopedUsage<const TimeDataVector>(time_data->_atomic_pointer_storage)
      , ReaderWriter<const TimeData, const TimeDataVector>(time_data,
                                                           radium::RT_AtomicPointerStorageMultipleReaders_ScopedUsage<const TimeDataVector>::get_pointer(),
                                                           cache_num)
    {
    }
    
    ~Reader(){
    }

    const T &at_first(void) const {
      return this->_vector->at_first();
    }

    const T &at_last(void) const {
      return this->_vector->at_last();
    }

    const T &at_ref(int i) const {
      return this->_vector->at_ref(i);
    }

    const T &find(std::function<bool(const T&)> func) const {
      for(const T &t : *this)
        if (func(t))
          return t;

      R_ASSERT_NON_RELEASE(false);
      
      return at_first();
    }
  };


  class Writer : public ReaderWriter<TimeData, TimeDataVector>{

    // We can probably make this work, but if trying to copy a Writer, it's most likely (over 99% sure) an error.
    Writer(const Writer&) = delete;
    Writer& operator=(const Writer&) = delete;

    bool _has_cancelled = false;
    
  public:
    
    Writer(TimeData *time_data, KeepOldData keep = KeepOldData::KEEP_OLD_DATA)
      : ReaderWriter<TimeData, TimeDataVector>(time_data, time_data->get_write_vector(keep), -1)
    {
      R_ASSERT_NON_RELEASE(THREADING_is_main_thread());
      R_ASSERT_NON_RELEASE(!PLAYER_current_thread_has_lock());

      //printf("New writer\n");
    }
    
    ~Writer(){
      if (!_has_cancelled)
        this->_time_data->writer_finalizer(*this);

      if (_has_cancelled)
        delete this->_vector;
      else
        this->_time_data->replace_vector(this->_vector);

      //printf("...New writer created: %d\n", !_has_cancelled);
    }

    T &at_first(void) const {
      return this->_vector->at_first();
    }
    
    T &at_last(void) const {
      return this->_vector->at_last();
    }
    
    T &at_ref(int i) const {
      return this->_vector->at_ref(i);
    }
    
    T* begin() const {
      return const_cast<T*>(this->_vector->begin());
    }
    
    T* end() const {
      return const_cast<T*>(this->_vector->end());
    }
  
    T &find(std::function<bool(T&)> func) const {
      for(T &t : *this)
        if (func(t))
          return t;

      R_ASSERT_NON_RELEASE(false);
      
      return at_first();
    }
    
    void sortit(void){
      R_ASSERT_NON_RELEASE(_has_cancelled==false);
      this->_time_data->sortit(this->_vector);
    }
    
    void assert_sorted(void){
#if !defined(RELEASE)
      this->_vector->assert_sorted();
#endif
    }
    
    void add(T &data){
      /*
      int pos = BinarySearch_Left(data.time);
      printf("POS: %d\n", pos);
      */
      this->_vector->push_back(data);
      sortit();
    }

    void add(T &&data){
      this->_vector->push_back(std::move(data));
      sortit();
    }

    void add2(T data){
      this->_vector->push_back(std::move(data));
      sortit();
    }

    template<typename ... Args> 
    void add(Args ... args){
      this->_vector->push_back(std::move(T(args...)));
      sortit();
    }

    // Moves node within MAX(0, previous node) and MIN(next node, max_pos)
    bool constraint_move(int num, Ratio new_pos, Ratio max_pos){
      R_ASSERT_NON_RELEASE(_has_cancelled==false);

      Ratio min_pos;

      if (num < 0){
        R_ASSERT(false);
        return false;
      }

      int size = this->size();
      
      if (num >= size){
        R_ASSERT(false);
        return false;
      }
      
      if (num==0)
        min_pos = make_ratio(0,1);
      else
        min_pos = at_ref(num-1).get_time();

      if (num < size-1)
        max_pos = at_ref(num+1).get_time();

      if (new_pos < min_pos)
        new_pos = min_pos;
      else if (new_pos > max_pos)
        new_pos = max_pos;

      at_ref(num).set_time(new_pos);
      return true;
    }

    bool constraint_move(int num, Ratio new_pos, int max_pos){
      return constraint_move(num, new_pos, make_ratio(max_pos, 1));
    }
    
    void clear(void){
      R_ASSERT_NON_RELEASE(_has_cancelled==false);
      
      this->_vector->clear();
    }

    bool remove_at_pos(int pos){
      R_ASSERT_NON_RELEASE(_has_cancelled==false);
      
      int dassize = this->size();
      
      if (pos < 0 || pos >= dassize){
        return false;
      }

#if 1
      this->_vector->remove_pos(pos, true);
#else
      // Less efficient. Code is here for testing purposes.
      this->_vector->remove_pos(pos, false);
      sortit();
  #if defined(RELEASE)
    #error "error"
  #endif
#endif

      assert_sorted();

      return true;
    }

    // Note: return value isn't always correct. It might return true even if nothing was removed.
    bool remove_at_positions(const std::vector<int> &positions){
      R_ASSERT_NON_RELEASE(_has_cancelled==false);

      if (positions.size()==0)
        return false;
      
#if 1
      TimeDataVector *_new_vector = new TimeDataVector();

      int i = -1;
      for(const auto &element : *this->_vector){
        i++;
        bool include = true;
        for(int pos : positions)
          if (pos==i){
            include = false;
            break;
          }
        if (include)
          _new_vector->push_back(element);
      }

      delete this->_vector;
      this->_vector = _new_vector;

      return true;
#else
      
      // Make sure we iterate from last to first position.
      // If not, we might remove wrong elements since 'remove_at_pos' just swap 'pos' with last element and decrements size.
      std::sort(positions.begin(), positions.end(), std::greater<int>());

      int dassize = this->size();
      
      int last = -1;
      for(int pos : positions){

        R_ASSERT_RETURN_IF_FALSE(pos >= 0 && pos < dassize);
        
        if (pos >= last){
          R_ASSERT_NON_RELEASE(false);
        } else {
          this->_vector->remove_pos(pos);
          last = pos;
        }
      }

      sortit();
#endif
    }

    bool remove(std::function<bool(int, T&)> remove_questionmark){
      std::vector<int> to_remove;
      
      for(int i=0;i<this->size();i++)
        if (remove_questionmark(i, at_ref(i)))
          to_remove.push_back(i);
      
      return remove_at_positions(to_remove);      
    }
    
    bool removeElement(const T &element) {
      for(int i=0;i<this->size();i++){
        if (at_ref(i) == element)
          return remove_at_pos(i);
      }
      return false;
    }
    
    bool remove_at_time(Ratio ratio){
      int pos = this->find_pos_exact(ratio);
      if (pos < 0)
        return false;
      return remove_at_pos(pos);
    }

    void remove_everything_after(Ratio time, bool include_at = true){
      
      for(int i=this->size()-1 ; i >= 0 ; i--){
        
        T &t = this->at_ref(i);

        if (include_at) {

          if (t.get_time() >= time)
            remove_at_pos(i);
          
        } else {

          if (t.get_time() > time)
            remove_at_pos(i);

        }
        
      }
    }

    // 'end' is not including.
    void remove_between(const Ratio start, const Ratio end) {      
      for(int i = this->size()-1 ; i >= 0 ; i--){
        
        T &t = this->at_ref(i);

        const Ratio time = t.get_time();

        if (time < start)
          return;
        
        if (time < end)
          remove_at_pos(i);
      }
    }
  
    // equiv. to List_InsertRatioLen3.
    bool insert_ratio(const Ratio &where_to_start, const Ratio &how_much, const Ratio last_legal_place = make_ratio(-1,1), std::function<void(T &t)> modify_func = nullptr){

      std::vector<int> to_remove;
      
      bool ret = false;
      
      for(int i=this->find_pos_left(where_to_start);i<this->size();i++){
        
        T &t = this->at_ref(i);

        if (t.get_time() >= where_to_start) {
          
          ret = true;

          if (modify_func)
            modify_func(t);
            
          if (t.get_time() < (where_to_start - how_much)) {

            to_remove.push_back(i);
            
          } else {

            t.set_time(t.get_time() + how_much);

            if (t.get_time().num < 0 || (last_legal_place.num>=0 && t.get_time() > last_legal_place)){

              to_remove.push_back(i);
              
            }
          }

        }

      }

      remove_at_positions(to_remove);

      return ret;
    }

    // equiv. to List_InsertLines3.
    void insert_lines(const Ratio &where_to_start, const Ratio &how_much){
      insert_ratio(where_to_start, how_much);
    }

    // equiv. to expand_list3
    void expand(const Ratio &start,
                const Ratio &end,
                const Ratio &new_end,
                const Ratio &last_legal_place,
                std::function<void(T &t)> modify_func = nullptr){
      
      R_ASSERT_NON_RELEASE(_has_cancelled==false);
      
      for(int i=this->find_pos_left(start);i<this->size();i++){
        
        T &t = this->at_ref(i);

        if (t.get_time() > start) {

          if (modify_func)
            modify_func(t);
          
          Ratio new_time;
        
          if (t.get_time() >= end) {

            if (new_end < end)
              new_time = t.get_time() - (end-new_end);
            else
              new_time = t.get_time() + (new_end-end);
            
          } else {

            if (start==end)
              new_time = (start+new_end)/2.0;
            else
              new_time = scale_ratio(t.get_time(), start, end, start, new_end);
            
          }
           
          if (new_time > last_legal_place) {
            R_ASSERT_NON_RELEASE(false);
            new_time = last_legal_place;
          }
          
          t.set_time(new_time);
        }

      }

      sortit();
    }
    
    void replace_with(const TimeData<T,SeqBlockT> *from){
      R_ASSERT(this->is_empty());
      
      Reader from_reader(from);
      
      for(const T &t : from_reader)
        add(t);
    }

    void replace_with_and_clear_source(TimeData<T,SeqBlockT> *from){
      R_ASSERT(this->is_empty());
      
      Writer from_writer(from);
      
      for(const T &t : from_writer)
        add(t);

      from_writer.clear();
    }
    
    void cancel(void){
      R_ASSERT_NON_RELEASE(_has_cancelled==false);
      
      _has_cancelled = true;
    }
  };

  // Called by ~Writer right before replacing the vector.
  virtual void writer_finalizer(TimeData<T,SeqBlockT>::Writer &writer) {
  }

  void replace_with(const TimeData<T,SeqBlockT> *from){
    Writer to_writer(this, KeepOldData::USE_CLEAN_DATA);

    to_writer.replace_with(from);
  }

  void replace_with_and_clear_source(TimeData<T,SeqBlockT> *from){
    Writer to_writer(this, KeepOldData::USE_CLEAN_DATA);

    to_writer.replace_with_and_clear_source(from);
  }

  // This one can be optimized, but not sure if it's any point.
  // Probably have to copy at least 10ns of thousands of non-trivial elements
  // to notice any difference.
  //
  void swap(TimeData<T,SeqBlockT> *other){    
    TimeData<T,SeqBlockT> temp;

    // 1. this -> temp
    temp.replace_with(this);

    // 2. other -> this
    replace_with(other);

    // 3. temp -> other;
    other->replace_with(&temp);
  }
};

  

#if 0
struct Gakk{
  int a = 20;
  ~Gakk(){
    printf(" GAKK freed!\n");
  }
};
  
struct Velocity{
  Place time;
  int logtype;
  int velocity;
  Gakk gakk;
};
  
struct Velocities : public TimeData<Place, Velocity>{
};

static Velocities *g_velocities = new Velocities;
#endif
 
}

