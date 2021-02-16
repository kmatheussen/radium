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
  * Accessing a random element (from time) is O(Log(n)) while accessing a later element (from time) is usually O(1) if player_num >= 0.


  HOW IT WORKS
  ============
  * We never modify the underlying vector.
    We only modify a COPY of the underlying vector, and when finished, the underlying vector is atomically replaced with the copy.
  * To make sure making a copy of the underlying vector doesn't take too much time, the element type ("DataType") must not be too complicated.
    If DataType is big, use pointers to instances instead of the instance itself.
  * A Reader object is optimized to get data in a linear increasing fashion,
    i.e. starting with a low time and ending on a high time, always increasing time from call to call. (This is not a requirement, but when
    time is not increasing, it's necessary to do a O(log N) binary search to find the new index.)
    If player_num >= 0, the index is also remembered in the TimeData object itself, further decreasing the need to do a binary search in the audio realtime thread.


  NOTES
  =====
  * Writing is a heavy operation since it recreates the underlying vector. Therefore, nothing is actually written until the Writer is deleted, and it makes sense to create as few
    writer objects as possible.
  * TimeData is very similar to SeqAutomation, but TimeData supports more than one simultaneous reader, it uses Ratio as time type instead of double,
    and it provides a wrapper around radium::Vector instead of QVector (writing) and plain array (reader).
 */

#pragma once


#include <memory>

#include "Vector.hpp"
#include "ratio_funcs.h"
#include "AtomicPointerStorage.hpp"


#define MAX_NUM_PARALLEL_TIMEDATAS 16

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
  struct RT_TimeData_Player_Cache_Holder;
}

extern void TIMEDATA_PLAYERCACHE_add_and_initialize(r::RT_TimeData_Player_Cache_Holder *holder);
extern void TIMEDATA_PLAYERCACHE_remove(r::RT_TimeData_Player_Cache_Holder *holder);
extern void TIMEDATA_PLAYERCACHE_reconfigure(void);

namespace r{

  /*
enum class DataTypeReturnType{
  VALUE_OK,
  NO_VALUES_YET,
  NO_VALUES,
  LAST_VALUE,
};
  */
  
struct RT_TimeData_Player_Cache{
  int _curr_pos = 0; // vector pos.
  double _last_value = 0; // last value returned from TimeData::get_value();
  int _last_play_id = -1; // Value of pc->play_id when last_value was returned.
};

struct RT_TimeData_Player_Cache_Holder{
#if !defined(RELEASE)
  int _num_caches;
#endif
  RT_TimeData_Player_Cache *caches; // array;

  RT_TimeData_Player_Cache_Holder()
  {
    TIMEDATA_PLAYERCACHE_add_and_initialize(this);
  }

  ~RT_TimeData_Player_Cache_Holder(){
    TIMEDATA_PLAYERCACHE_remove(this);
  }
};
  
template <typename T>
class TimeData {

  static_assert(sizeof(T) < sizeof(void*)*8, "T should be a pointer if too big. This to lower the time it takes to copy the underlying vector.");
  static_assert(std::is_trivially_copyable<T>::value, "T should be a pointer if not trivially copyable. (don't want to copy all data every time we add a block for instance)");
  
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

#if 1

  RT_TimeData_Player_Cache_Holder _player_cache_holder;

#else
  
  mutable int _RT_player_curr_pos[MAX_NUM_PARALLEL_TIMEDATAS] = {}; // Used by the player. Is mutable because it's used by the Reader for caching read position.
  mutable double _RT_player_last_value[MAX_NUM_PARALLEL_TIMEDATAS] = {}; // Used by the player. Is mutable because it's used by the Reader for caching read position.
  mutable int _RT_player_last_play_id[MAX_NUM_PARALLEL_TIMEDATAS] = {}; // Used by the player. Is mutable because it's used by the Reader for caching read position.
#endif

  
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
    
    const int _player_num; // is -1 if not a player.
    int &_curr_pos; // caching read position
    mutable int _non_player_curr_pos = 0;
    
  private:
    
    int BinarySearch_Left_exact(const T *array, const Ratio ratio, const int low, const int high, bool &found_exact) const {   // initially called with low = 0, high = N - 1
      
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

#if 0 // not used
    
    int BinarySearch_Left(const T *array, const Ratio ratio, const int low, const int high) const {
      printf("BinarySearch. Ratio: %f. low: %d (%f). High: %d (%f)\n", (double)ratio.num/(double)ratio.den, low,
             make_double_from_ratio(array[low]._time),  high, make_double_from_ratio(array[high]._time)
             );
      
      // invariants: ratio  > A[i] for all i < low
      //             ratio <= A[i] for all i > high
      if (high <= low) // Hmm. We use "<=" here, which has to be correct, but "<" everywhere else. ("<" is probably correct too though, but probably slower). Binarysearch is hard.
        return low;
      
      const int mid = (low + high) / 2;
      const Ratio mid_time = array[mid]._time;

      printf("    .... mid: %d. mid-time: %f\n", mid, make_double_from_ratio(mid_time));
      
      if (mid_time >= ratio)
        return BinarySearch_Left(array, ratio, low, mid-1);
      else
        return BinarySearch_Left(array, ratio, mid+1, high);
    }

    int BinarySearch_Left(const Ratio ratio, const int low, const int high) const {
      R_ASSERT_NON_RELEASE(this->size() >= 2);
      R_ASSERT_NON_RELEASE(low >= 1);
      R_ASSERT_NON_RELEASE(high < this->size());
      return BinarySearch_Left(_vector->get_array(), ratio, low, high);
    }
#endif
    
    // Made by looking at https://en.wikipedia.org/wiki/Binary_search_algorithm#Duplicate_elements
    int BinarySearch_Rightmost(const T *array, const Ratio ratio, int low, int high) const {
      R_ASSERT_NON_RELEASE(this->size() >= 2);
      R_ASSERT_NON_RELEASE(low >= 1);
      R_ASSERT_NON_RELEASE(high < this->size());
      
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
      R_ASSERT(_player_num==-1);
      
      bool found_exact;

      _curr_pos = BinarySearch_Left_exact(ratio, found_exact);

      if (found_exact)
        return _curr_pos;
      else
        return -1;
    }

#if defined(TEST_TIMEDATA_MAIN)
  public:
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
        
      if (curr_pos < das_size - 1) {
        
        R_ASSERT_NON_RELEASE(curr_pos>=0);

        //printf(" F1. curr_pos: %d. ratio: %f. [curr_pos]._time: %f\n", curr_pos, make_double_from_ratio(ratio), make_double_from_ratio(_vector->at_ref(curr_pos)._time));
        
        if (ratio >= _vector->at_ref(curr_pos)._time) {

          //printf(" F2\n");
          // Hopefully the compiler is able to hyper-optimize this block.

          for(int i=0 ; i < 4; i++, curr_pos++) {
            R_ASSERT_NON_RELEASE(curr_pos < das_size-1);
            
            const T &next = _vector->at_ref(curr_pos + 1);

            //printf(" F3 %d\n", i);
            
            if (ratio < next._time){
              _curr_pos = curr_pos+1;
              //printf(" F4 %d\n", _curr_pos);
              return _curr_pos;
            }
          }
          
          _curr_pos = BinarySearch_Rightmost(ratio, curr_pos, das_size-1);
          //printf(" F5 %d\n", _curr_pos);
          return _curr_pos;
          
        } else {

          _curr_pos = BinarySearch_Rightmost(ratio, 1, curr_pos);
          //printf(" F6 %d\n", _curr_pos);
          return _curr_pos;          
          
        }
        
      } else {

        _curr_pos = BinarySearch_Rightmost(ratio, 1, das_size-1);
        //printf(" F7 %d\n", _curr_pos);
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
    
    ReaderWriter(TimeData *time_data, TimeDataVector *vector, const int player_num)
      : _time_data(time_data)
      , _vector(vector)
      , _player_num(player_num)
      , _curr_pos(player_num >= 0 ? time_data->_player_cache_holder.caches[player_num]._curr_pos : _non_player_curr_pos)
    {
      R_ASSERT_NON_RELEASE(player_num < MAX_NUM_PARALLEL_TIMEDATAS);
      R_ASSERT_NON_RELEASE(player_num==-1 || THREADING_is_player_thread());      
      R_ASSERT_NON_RELEASE(player_num>=-1 || player_num < time_data->_player_cache_holder._num_caches);
    }

    /*
    ~ReaderWriter(){
      if (_is_player)
        _time_data->_RT_player_curr_pos = _curr_pos;
    }
    */
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
      int index = find_pos_for_get_value(ratio);
      R_ASSERT_NON_RELEASE(index > 0);
      R_ASSERT_NON_RELEASE(index < das_size);
      
      const T &t1 = _vector->at_ref(index-1);
      const T &t2 = _vector->at_ref(index);
      
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
    
#if 0
    VI må bruke ratio_start, ikke ratio_end. Vil tro at massakre høres riktig ut da (den har start-pos automasjon med flere node).
    Men da MÅ nok denne også kunne returnere to verdier når last()._time != ratio_start.
      Er kanskje ikke så viktig at massakre osv. høres helt riktig ut da. Det vil uansett høres riktig ut ca. 99% av tida. Men det kan være at noen trenger at det er helt nøyaktig.
      Men det virker fortsatt riktig å returnere fra ratio_start. Effekten skal jo representere det som spilles av i tidsrommet.

      Løsning: Lag en egen process_fx_values HER. (eller, selve implementasjonen av TimeData::Reader::process_fx_values kan jo ligge i fxlines.cpp da)
#endif

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
      return _player_num < 0 ? NULL : &_time_data->_player_cache_holder.caches[_player_num];
    };
    
    // Implemented in fxlines.cpp (the method is very general, it can probably easily be converted into a general iterate function)
    template <typename ValType>
    void iterate_fx(struct SeqTrack *seqtrack, const struct SeqBlock *seqblock, const struct Tracks *track, struct FX *fx, int play_id, const int64_t seqtime_start, const r::RatioPeriod &period) const;


    template <typename ValType>
    bool get_value(int play_id, const Ratio &ratio, ValType &value, FX_when &when) const {
      static_assert(std::is_same<ValType, int>::value || std::is_same<ValType, float>::value || std::is_same<ValType, double>::value);
        
      RT_TimeData_Player_Cache *cache = get_player_cache();

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
      
      if (cache != NULL) {

        cache->_last_value = curr_value;
        cache->_last_play_id = play_id;
        
      }

      value = curr_value;
      
      return true;
    }

#if 0
    // ratio_start is used if _player_num == -1.
    template <typename ValType>
    bool get_value_old(int play_id, const RatioPeriod &period, ValType &value, FX_when &when) const {

      static_assert(std::is_same<ValType, int>::value || std::is_same<ValType, float>::value || std::is_same<ValType, double>::value);
        
      if (!period_is_inside(period))
        return false;

      RT_TimeData_Player_Cache *cache = get_player_cache();

      const int das_size = size();
      const T &first_t = _vector->at_ref(0);
      const T &last_t = _vector->at_ref(das_size-1);
      
      bool has_prev_value;
      double prev_value;

      if (period._start.num==0 || period._start < first_t._time) // Note: ratio_start==0 when it's the first call to block.
        
        has_prev_value = false;
      
      else {
        
        if (cache!=NULL && cache->_last_play_id == play_id)
          prev_value = cache->_last_value;
        else
          prev_value = get_value_raw(period._start, das_size); // Faster to call get_value_raw(period._start,...) before get_value_raw(ratio,...) than the other way.
        
        has_prev_value = true;
      }
      
        
      double curr_value;
      
      if (period._end >= last_t._time){
        
        curr_value = last_t._val;
        when = FX_end;
        
      } else {

        if (period._start.num==0 || period._start < first_t._time)
          when = FX_start;
        else
          when = FX_middle;

        //printf("HERE: Ratio: %d / %d\n", (int)ratio.num, (int)ratio.den);
        
        curr_value = get_value_raw(period._end, das_size);
        
      }
      
      value = curr_value; // Note: typeof(value)==ValType. typeof(curr_value)==double

      /*
      printf("has_prev: %d. prev: %f. curr: %f. First_time: %f. Prev time: %f. Now Time: %f\n", has_prev_value, ( (!has_prev_value) ? -1234.0 : prev_value), curr_value,
             make_double_from_ratio(first_t._time),
             make_double_from_ratio(period._start),
             make_double_from_ratio(period._end));
      */
      
      bool same_value_as_last_time =
        has_prev_value
        && (std::is_same<ValType, int>::value
            ? int(prev_value)==value
            : (std::is_same<ValType, float>::value
               ? equal_floats(float(prev_value), value)
               : equal_doubles(prev_value, value)
               )
            );

      if (cache != NULL) {

        cache->_last_value = curr_value;
        cache->_last_play_id = play_id;
        
      }

      return when==FX_end || when==FX_start || !same_value_as_last_time;
    }
#endif
};
  
  
public:
  
  // Optimized reader if reading the vector in a timely linear fashion.
  class Reader : radium::RT_AtomicPointerStorageMultipleReaders_ScopedUsage<TimeDataVector>, public ReaderWriter<const TimeData, const TimeDataVector>{

   public:
    
    Reader(const TimeData *time_data, const int player_num = -1)
      : radium::RT_AtomicPointerStorageMultipleReaders_ScopedUsage<TimeDataVector>(time_data->_atomic_pointer_storage)
      , ReaderWriter<const TimeData, const TimeDataVector>(time_data, radium::RT_AtomicPointerStorageMultipleReaders_ScopedUsage<TimeDataVector>::get_pointer(), player_num)
    {
    }
    
    ~Reader(){
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

    bool _has_cancelled = false;
    
  public:
    
    Writer(TimeData *time_data, bool get_clean = false)
      : ReaderWriter<TimeData, TimeDataVector>(time_data, time_data->get_write_vector(get_clean), -1)
    {
      R_ASSERT_NON_RELEASE(THREADING_is_main_thread());
    }
    
    ~Writer(){
      if (_has_cancelled){
        delete this->_vector;
      }else
        this->_time_data->replace_vector(this->_vector);
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
    
    bool remove_at_time(Ratio ratio){
      int pos = this->find_pos_exact(ratio);
      if (pos < 0)
        return false;
      return remove_at_pos(pos);
    }

    void remove_everything_after(Ratio time, bool include_at = true){
      
      for(int i=this->size()-1 ; i >= 0 ; i--){
        
        T &t = this->at_ref(i);
        
        if (t._time >= time){
          
          if (t._time==time){
            if (include_at)
              remove_at_pos(i);
          }else
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

