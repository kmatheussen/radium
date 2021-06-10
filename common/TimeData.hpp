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
  * Several readers (up to 128 simultaneous threads can read at the same time)
  * One writer
  * The readers don't have to be synchronized.
  * A reader can be a realtime thread.
  * Writer thread is never a realtime thread.
  * DataType must have this variable:
    "Ratio _time;"
    (TimeData can probably easily be extended to support any _time type, not just Ratio, by making Ratio a template type, if necessary)
  * The underlying vector is sorted by _time.
  * Accessing a random element (given time) is O(Log(n)) while accessing a sequential element is usually O(1) if cache_num >= 0 (i.e. when time is only a little bit higher than last call).


  HOW IT WORKS
  ============
  * We never modify the underlying vector.
    We only modify a COPY of the underlying vector, and when finished, the underlying vector is atomically replaced with the modified copy.
  * To make sure making a copy of the underlying vector doesn't take too much time, the element type ("DataType") must not be too complicated.
    If DataType is big, use pointers to instances instead of the instance itself.
  * A Reader object is optimized to get data in a linear increasing fashion,
    i.e. starting with a low time and ending on a high time, always increasing time from call to call. (This is not a requirement, but when
    time is not increasing, it's necessary to do a O(log N) binary search to find the new index.)
    If cache_num >= 0, the index is also remembered in the TimeData object itself, further decreasing the need to do a binary search in the audio realtime thread.


  NOTES
  =====
  * Writing is a heavy operation since it recreates the underlying vector. Therefore, nothing is actually written until the Writer
    object is deleted, and it makes sense to create as few writer objects as possible.
  * TimeData is very similar to SeqAutomation. The main difference is that TimeData supports more than one simultaneous reader, but
    it also uses Ratio as time type instead of double, and it provides a wrapper around radium::Vector instead of using QVector for
    writing and a plain array for reading.
 */

#pragma once


#include <memory>

#include "Vector.hpp"
#include "ratio_funcs.h"
#include "sequencer_proc.h"
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

#if !defined(RELEASE) || defined(TEST_TIMEDATA_MAIN)
extern int g_num_timedata_vectors;
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

  int RT_get_play_id(void) const {
    return _last_play_id;
  }

  int RT_obtain(int play_id){
    ASSERT_SIZE();
    
    if (play_id != _last_play_id) {

      _last_play_id = play_id;

      if (!_used.is_empty()) {

        int ret = _used.at(0);
        
        for(int pos = 1 ; pos < _used.size() ; pos++)
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
  
class RT_TimeData_Player_Cache{

public:
    int _curr_pos = 0; // vector pos.

private:
  
  template <typename ValType>
  friend class RT_TimeData_Cache_Handler;
    
  double _last_value = 0; // last value returned from TimeData::get_value();
  int _last_play_id = -1; // Value of pc->play_id when last_value was returned.
};


template <typename ValType>
struct RT_TimeData_Cache_Handler{

  RT_TimeData_Player_Cache *_cache;
  int64_t _play_id;
  
  RT_TimeData_Cache_Handler(RT_TimeData_Player_Cache *cache, int64_t play_id)
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
  
  bool is_same_value(ValType value1, ValType value2) const {
    return (std::is_same<ValType, int>::value
            ? value1==value2
            : (std::is_same<ValType, float>::value
               ? equal_floats(value1, value2)
               : equal_doubles(value1, value2)
               )
            );
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
  
  virtual void callback(struct SeqTrack *seqtrack,
                        const struct SeqBlock *seqblock,
                        const struct Tracks *track,
                        ValType val,
                        int64_t time,
                        FX_when when) const = 0;
};

template <typename ValType>
struct TimeDataSimpleNode{
  Ratio _time;
  ValType _val;
  int _logtype;
  
  TimeDataSimpleNode(Ratio time, ValType val, int logtype = LOGTYPE_LINEAR)
    : _time(time)
    , _val(val)
    , _logtype(logtype)
  {}
};


template <typename T>
class TimeData {

  //static_assert(sizeof(T) < sizeof(void*)*8, "T should be a pointer if too big. This to lower the time it takes to copy the underlying vector.");
  static_assert(std::is_trivially_copyable<T>::value, "T should be a pointer if not trivially copyable. (don't want to copy all data every time we add a block for instance)");

#if !defined(RELEASE)
  mutable int _binsearch=0;  
#endif
  
private:

  struct TimeDataVector : public radium::Vector<T> {

    TimeDataVector(TimeDataVector *vector)
      : radium::Vector<T>(vector)
    {
      R_ASSERT_NON_RELEASE(THREADING_is_main_thread());
#if !defined(RELEASE) || defined(TEST_TIMEDATA_MAIN)
      g_num_timedata_vectors++;
#endif
    }

    TimeDataVector()
      : TimeDataVector(NULL)
    {
    }
    
    ~TimeDataVector() {
      R_ASSERT_NON_RELEASE(THREADING_is_main_thread());
#if !defined(RELEASE) || defined(TEST_TIMEDATA_MAIN)
      g_num_timedata_vectors--;
      //printf("                 FREEEING TimeDataVector %d\n", g_num_timedata_vectors);
      //getchar();
#endif
    }

    void print_all_times(void) const {
      printf("--------TimeData start. Size: %d\n", this->size());
      for(int i=0;i<this->size();i++)
        printf("   %d: %f  (%d / %d)\n", i, make_double_from_ratio(this->at(i)._time), (int)this->at(i)._time.num, (int)this->at(i)._time.den);
      printf("--------TimeData end.\n\n");
    }
    
    void sortit(void) {
      //printf("  Sorting. is_sorted: %d\n", _is_sorted);
      //printf("Before:\n");
      //print_all_times();
      
      this->sort([](const T &a, const T &b){
          return a._time < b._time;
        });
      
      //printf("After:\n");
      //print_all_times();
#if !defined(RELEASE)
      if (this->size() > 0){
        Ratio prev = this->at(0)._time;
        for(int i=1;i<this->size();i++){
          if (this->at(i)._time < prev)
            abort();
          prev = this->at(i)._time;
        }
      }
#endif
    }
  
  };

  TimeDataVector* _vector;

  mutable radium::AtomicPointerStorageMultipleReaders<TimeDataVector> _atomic_pointer_storage;

  mutable RT_TimeData_Player_Cache _player_caches[MAX_NUM_PARALLEL_TIMEDATAS];

  
public:
  
  TimeData(void)
    : _vector(new TimeDataVector())
  {
    R_ASSERT_NON_RELEASE(THREADING_is_main_thread());
    _atomic_pointer_storage.set_new_pointer(_vector);
  }

  
  ~TimeData(){
    R_ASSERT_NON_RELEASE(THREADING_is_main_thread());
  }
  
private:

  
  // When finished, we must always call 'replace_vector' or delete, on the return value.
  TimeDataVector *get_write_vector(bool get_clean){
    R_ASSERT_NON_RELEASE(THREADING_is_main_thread());
    return new TimeDataVector(get_clean ? NULL : _vector);
  }

  // Called by the writer.
  void replace_vector(TimeDataVector *vector){
    R_ASSERT_NON_RELEASE(THREADING_is_main_thread());
    R_ASSERT(!PLAYER_current_thread_has_lock());
    
    R_ASSERT_NON_RELEASE(vector != _vector);

    _vector = vector;
    _atomic_pointer_storage.set_new_pointer(_vector);
  }

  template <typename TimeData, typename TimeDataVector>
  class ReaderWriter{

    ReaderWriter(const ReaderWriter&) = delete;
    ReaderWriter& operator=(const ReaderWriter&) = delete;

  protected:

    TimeData *_time_data;

    TimeDataVector *_vector;
    
    RT_TimeData_Player_Cache *_player_cache;

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
      const Ratio time = array[mid]._time;
      
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
        const Ratio mid_time = array[mid]._time;
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
      R_ASSERT_NON_RELEASE(ratio >= _vector->at_ref(0)._time);
      R_ASSERT_NON_RELEASE(ratio < _vector->at_ref(das_size-1)._time);
      
      int curr_pos = R_MAX(_curr_pos, 1);
        
      if (curr_pos < das_size) {
        
        R_ASSERT_NON_RELEASE(curr_pos>=0);

        //printf(" F1. curr_pos: %d. ratio: %f. [curr_pos]._time: %f\n", curr_pos, make_double_from_ratio(ratio), make_double_from_ratio(_vector->at_ref(curr_pos)._time));
        
        if (ratio >= _vector->at_ref(curr_pos-1)._time) {

          //printf(" F2\n");
          // Hopefully the compiler is able to hyper-optimize this block.

          //curr_pos--;
          
          for(int i=0 ; i < 4; i++, curr_pos++) {
            R_ASSERT_NON_RELEASE(curr_pos < das_size);
            
            const T &next = _vector->at_ref(curr_pos);

            //printf(" F3 %d\n", i);
            
            if (ratio < next._time){
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

    int find_element_at_ratio(const Ratio &ratio) const {
      return find_pos_exact(ratio);
    }
    
    bool has_element_at_ratio(const Ratio &ratio) const {
      return find_pos_exact(ratio) >= 0;
    }
    
    bool has_element_between(const Ratio &begin, const Ratio &end) const {
      for(const T &t : *_vector)
        if (t._time >= begin && t._time < end)
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
      if (ratio < t1._time)
        abort();
      
      if (ratio > t2._time)
        abort();
#endif
        
      if (t1._logtype==LOGTYPE_HOLD){

        return t1._val;
        
      } else {

        /*
        printf("Value RAW. index: %d. Ratio: %f. times: %f / %f. Values: %d / %d\n", index, make_double_from_ratio(ratio),
               make_double_from_ratio(t1._time), make_double_from_ratio(t2._time),
               t1._val, t2._val);
        */
        
        if (t1._time==t2._time){  // Might happen at last node, not sure.
          R_ASSERT_NON_RELEASE(false);
          return t2._val;
        }else
          return scale_double(make_double_from_ratio(ratio),
                              make_double_from_ratio(t1._time), make_double_from_ratio(t2._time),
                              t1._val, t2._val);
      }
    }

  public:
    
    bool period_is_inside(const r::RatioPeriod &period) const {
      const int das_size = size();
      if (das_size<2)
        return false;
      
      const T &first_t = _vector->at_ref(0);
      
      if (period._end < first_t._time)
        return false;
      
      const T &last_t = _vector->at_ref(das_size-1);
      
      if (period._start >= last_t._time)
        return false;

      return true;
    }

    RT_TimeData_Player_Cache *get_player_cache(void) const {
      return _cache_num < 0 ? NULL : &_time_data->_player_caches[_cache_num];
    };

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

      const int das_size = size();
      const T &first_t = at_ref(0);
      const T &last_t = at_ref(das_size-1);

      RT_TimeData_Cache_Handler<ValType> cache(get_player_cache(), play_id);

      bool has_prev_value;
      double prev_value;

      // Find previous value
      {
        if (period._start.num==0 || period._start < first_t._time) { // Note: period._start==0 when it's the first call to block.

          prev_value = 0.0; // Not necessary. Only to silence compiler error. (Usually I have the opposite problem, that it won't give error when using uninitialized value. Sigh. Why don't the gcc and clang people prioritize to get this right? It seems far more important than minor optimizations for instance.)
          has_prev_value = false;
    
        } else {
      
          if (cache.has_value()) {
        
            prev_value = cache.get_value();
        
          } else {

            // Approximately. (This is a corner case,
            // even if this value is totally wrong, no one would probably notice, and if they did it would be extremely seldom.)
            Ratio ratio_prev = period._start - (period._end-period._start);
            
            if (ratio_prev < first_t._time) {
              prev_value = first_t._val;
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
        if (period._end >= last_t._time){

          value_time = get_seqblock_place_time3(seqblock, track, last_t._time);
          value = last_t._val;
          when = FX_end;

        } else if (period._start.num==0 || period._start < first_t._time) { // Note: period._start==0 when it's the first call to block.

          value_time = get_seqblock_place_time3(seqblock, track, first_t._time);
          value = first_t._val;
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
          auto value_time = get_seqblock_place_time3(seqblock, track, node._time);
          printf("........ time last node: %d. Value last node: %f\n", (int)value_time, (double)node._val / (double)fx->max);
          }
          }
        */
        callback.callback(seqtrack, seqblock, track, value, value_time, when);
      }

      // Send out all node values between period._start and period._end
      if (when != FX_end) {
    
        for( ; _curr_pos < das_size ; _curr_pos++) {

          const T &node = at_ref(_curr_pos);

          if (0){
            auto node_time = get_seqblock_place_time3(seqblock, track, node._time);
            auto end_time = get_seqblock_place_time3(seqblock, track, node._time);
            printf("............(2) _curr_pos: %d. node(_curr_pos) time: %d. end_time: %d. node ratio: %d / %d. end ratio: %d / %d\n", _curr_pos, (int)node_time, (int)end_time,
                   (int)node._time.num, (int)node._time.den, 
                   (int)period._end.num, (int)period._end.den);
          }
      
          // (maybe) FIX: The correct test here is actually node._time >= period._end, and not node._time > period._end.
          // However, because of rounding errors, notes can be sent out in the block before an fx node at the same position.
          // And it's quite important that fx are sent out before note start, for instance if setting start position of a sample (common in MOD files).
          // Afters notes have been converted to TimeData, this test should probably be corrected.
          if (node._time > period._end)
            break;
      
          value = node._val;
      
          FX_when when = _curr_pos == das_size-1 ? FX_end : FX_middle;

          int64_t time = get_seqblock_place_time3(seqblock, track, node._time);
          //printf("....2. %d: %f. When: %d. _curr_pos: %d\n", (int)value, (double)value / (double)fx->max, (int) when, _curr_pos);
          callback.callback(seqtrack, seqblock, track, value, time, when);

        }
      }

      cache.update_value(value);
    }

    // Same as iterate, but also handles one external node placed before first node, and one external node placed after last node.
    // Used for handling note velocities and note pitches.
    // Implemented in velocities.cpp
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
      
      if (ratio > last_t._time)
        return false;

      if (ratio < first_t._time)
        return false;
      
      if (ratio == last_t._time){
        
        curr_value = last_t._val;
        when = FX_end;
        
      } else {

        if (ratio == first_t._time)
          when = FX_start;
        else
          when = FX_middle;

        curr_value = get_value_raw(ratio, das_size);
        
      }

      RT_TimeData_Cache_Handler<ValType> cache(get_player_cache(), play_id);

      cache.update_value(curr_value);

      value = curr_value;
      
      return true;
    }

};
  
  
public:

  // Optimized reader if reading the vector in a timely linear fashion (cache_num should be supplied).
  class Reader : radium::RT_AtomicPointerStorageMultipleReaders_ScopedUsage<TimeDataVector>, public ReaderWriter<const TimeData, const TimeDataVector>{

    // We can probably make this work, but if trying to copy a Reader, it's most likely (over 99% sure) an error.
    Reader(const Reader&) = delete;
    Reader& operator=(const Reader&) = delete;

   public:
    
    Reader(const TimeData *time_data, const int cache_num = -1)
      : radium::RT_AtomicPointerStorageMultipleReaders_ScopedUsage<TimeDataVector>(time_data->_atomic_pointer_storage)
      , ReaderWriter<const TimeData, const TimeDataVector>(time_data, radium::RT_AtomicPointerStorageMultipleReaders_ScopedUsage<TimeDataVector>::get_pointer(), cache_num)
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

    const T* begin() const {
      return this->_vector->begin();
    }
    
    const T* end() const {
      return this->_vector->end();
    }

  };


  class Writer : public ReaderWriter<TimeData, TimeDataVector>{

    // We can probably make this work, but if trying to copy a Writer, it's most likely (over 99% sure) an error.
    Writer(const Writer&) = delete;
    Writer& operator=(const Writer&) = delete;

    bool _has_cancelled = false;
    
  public:
    
    Writer(TimeData *time_data, bool get_clean = false)
      : ReaderWriter<TimeData, TimeDataVector>(time_data, time_data->get_write_vector(get_clean), -1)
    {
      R_ASSERT_NON_RELEASE(THREADING_is_main_thread());
      R_ASSERT_NON_RELEASE(!PLAYER_current_thread_has_lock());

      //printf("New writer\n");
    }
    
    ~Writer(){
      if (_has_cancelled){
        delete this->_vector;
      }else
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

    void sortit(void){
      this->_vector->sortit();
    }
    
    void add(T &data){
      /*
      int pos = BinarySearch_Left(data.time);
      printf("POS: %d\n", pos);
      */
      this->_vector->push_back(data);
      sortit();
    }

    void add2(T data){
      add(data);
    }

    // Moves node within MAX(0, previous node) and MIN(next node, max_pos)
    bool constraint_move(int num, Ratio new_pos, Ratio max_pos){
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
        min_pos = at_ref(num-1)._time;

      if (num < size-1)
        max_pos = at_ref(num+1)._time;

      if (new_pos < min_pos)
        new_pos = min_pos;
      else if (new_pos > max_pos)
        new_pos = max_pos;

      at_ref(num)._time = new_pos;
      return true;
    }

    bool constraint_move(int num, Ratio new_pos, int max_pos){
      return constraint_move(num, new_pos, make_ratio(max_pos, 1));
    }
    
    void clear(void){
      this->_vector->clear();
    }

    bool remove_at_pos(int pos){
      int dassize = this->size();
      
      if (pos < 0 || pos >= dassize){
        return false;
      }
      
      this->_vector->remove_pos(pos);

      if (pos < dassize-1)
        sortit();
      
      return true;
    }

    void remove_at_positions(const std::vector<int> &positions){

      if (positions.size()==0)
        return;
      
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

    void remove(std::function<bool(int, T&)> remove_questionmark){
      std::vector<int> to_remove;
      
      for(int i=0;i<this->size();i++)
        if (remove_questionmark(i, at_ref(i)))
          to_remove.push_back(i);
      
      remove_at_positions(to_remove);      
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

          if (t._time >= time)
            remove_at_pos(i);
          
        } else {

          if (t._time > time)
            remove_at_pos(i);

        }
        
      }
    }

    // equiv. to List_InsertRatioLen3.
    bool insert_ratio(const Ratio &where_to_start, const Ratio &how_much, const Ratio last_legal_place = make_ratio(-1,1)){
      
      std::vector<int> to_remove;
      
      bool ret = false;
      
      for(int i=0;i<this->size();i++){
        
        T &t = this->at_ref(i);

        if (t._time >= where_to_start) {
          ret = true;

          if (t._time < (where_to_start - how_much)) {

            to_remove.push_back(i);
            
          } else {

            t._time += how_much;

            if (t._time.num < 0 || (last_legal_place.num>=0 && t._time > last_legal_place)){

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
    void expand(const Ratio &start, const Ratio &end, const Ratio &new_end, const Ratio &last_legal_place){
      
      for(int i=0;i<this->size();i++){
        
        T &t = this->at_ref(i);

        if (t._time > start) {

          Ratio new_time;
        
          if (t._time >= end) {

            if (new_end < end)
              new_time = t._time - (end-new_end);
            else
              new_time = t._time + (new_end-end);
            
          } else {

            if (start==end)
              new_time = (start+new_end)/2.0;
            else
              new_time = scale_ratio(t._time, start, end, start, new_end);
            
          }
           
          if (new_time > last_legal_place) {
            R_ASSERT_NON_RELEASE(false);
            new_time = last_legal_place;
          }
          
          t._time = new_time;
        }

      }
    }
    
    void cancel(void){
      R_ASSERT_NON_RELEASE(_has_cancelled==false);
      _has_cancelled = true;
    }
  };

  void move_from(TimeData<T> *from){
    Writer to_writer(this, true);
    Writer from_writer(from);

    for(T t : from_writer)
      to_writer.add(t);

    from_writer.clear();
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

