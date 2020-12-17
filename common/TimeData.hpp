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
    (TimeData can be extended later to support any _time type not just Ratio)
  * The underlying vector is sorted by _time.
  * Accessing a random element is O(Log(n)) while accessing the next element is O(1).


  HOW IT WORKS
  ============
  * We never modify the underlying vector.
    We only modify a COPY of the underlying vector, and when finished, the underlying vector is atomically replaced with the copy.
  * To make sure making a copy of the underlying vector doesn't take too much time, the element type ("DataType") must not be too complicated.
    If DataType is big, use pointers to instances instead of the instance itself.


  TIPS
  ====
  * A Reader object is optimized to get data in a linear increasing fashion, i.e. starting with a low time and ending on a high time, always increasing time from call to call.
    If a reader is not accessed this way, it might be a good idea to use more than one Reader to access the data.
    It's also less realtime friendly to read in a non-linear fashion since accessing a random element is O(Log(n)) while accessing the next element is O(1).


  NOTES
  =====
  * Writing is a heavy operation since it recreates the underlying vector. Therefore, nothing is actually written until the Writer is deleted, and it makes sense to create as few
    writer objects as possible.
 */

#pragma once


#include <memory>

#include "Vector.hpp"
#include "ratio_funcs.h"
#include "AtomicPointerStorage.hpp"


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

template <typename T>
class TimeData{

  static_assert(sizeof(T) < sizeof(void*)*8, "T should be a pointer if too big. This to lower the time it takes to copy the underlying vector.");
  static_assert(std::is_trivially_copyable<T>::value, "T should be a pointer if not trivially copyable. (don't want to copy all data every time we add a block for instance)");
  
 public:

  struct BetweenTwoTs{
    const T *data1;
    const T *data2;
  };
  
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
    

    // Can only be set from Writer.
    bool _is_sorted = true;
    
    void sortit_if_necessary(void){
      //printf("  Sorting. is_sorted: %d\n", _is_sorted);
      if (!_is_sorted){
        
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
        
        _is_sorted = true;
      }
    }
  
  };

  TimeDataVector* _vector;

  mutable radium::AtomicPointerStorageMultipleReaders<TimeDataVector> _atomic_pointer_storage;
  
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
    
    vector->sortit_if_necessary();

    _vector = vector;
    _atomic_pointer_storage.set_new_pointer(_vector);
  }

  template <typename TimeData, typename TimeDataVector>
  class ReaderWriter{

    ReaderWriter(const ReaderWriter&) = delete;
    ReaderWriter& operator=(const ReaderWriter&) = delete;

  protected:
    
    TimeDataVector *_vector;

  private:
    
    int BinarySearch_Left(Ratio value, int low, int high, bool &found_exact) const {   // initially called with low = 0, high = N - 1
      // invariants: value  > A[i] for all i < low
      //             value <= A[i] for all i > high
      if (high < low){
        found_exact = false;
        return low;
      }
      
      int mid = (low + high) / 2;
      Ratio time = _vector->at(mid)._time;
      
      if (time == value){
        found_exact = true;
        return mid;
      }else if (time > value)
        return BinarySearch_Left(value, low, mid-1, found_exact);
      else
        return BinarySearch_Left(value, mid+1, high, found_exact);
    }

  protected:
    
    int BinarySearch_Left(Ratio value, bool &found_exact) const {
      _vector->sortit_if_necessary();
      return BinarySearch_Left(value, 0, size()-1, found_exact);
    }

    int find_pos(Ratio value) const {
      bool found_exact;
      int pos = BinarySearch_Left(value, found_exact);
      if (found_exact)
        return pos;
      else
        return -1;
    }
    
  public:
    
    ReaderWriter(TimeDataVector *vector)
      : _vector(vector)
    {
    }
    
    T &at_ref(int i) const {
      return _vector->at_ref(i);
    }
    
    T at(int i) const {
      return _vector->at(i);
    }
    
    T operator[](int i) const {
      return at(i);
    }

    int size(void) const {
      return _vector->size();
    }

    const T* begin() const {
      return _vector->begin();
    }
    
    const T* end() const {
      return _vector->end();
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
  };
  
  
public:
  
  // Optimized reader if reading the vector in a timely linear fashion.
  class Reader : radium::RT_AtomicPointerStorageMultipleReaders_ScopedUsage<TimeDataVector>, public ReaderWriter<TimeData, TimeDataVector>{

    int _last_n1 = -1;
    /*
    T _last_data1 = NULL; // same as _vector->_data[_last_n1]
    T _last_data2 = NULL; // same as _vector->_data[_last_n1+1]
    */

   public:
    
    Reader(const TimeData *time_data)
      : radium::RT_AtomicPointerStorageMultipleReaders_ScopedUsage<TimeDataVector>(time_data->_atomic_pointer_storage)
      , ReaderWriter<TimeData, TimeDataVector>(radium::RT_AtomicPointerStorageMultipleReaders_ScopedUsage<TimeDataVector>::get_pointer())
    {
    }
    /*
    Reader(const TimeData *time_data)
      : Reader(const_cast<TimeData*>(time_data))
    {}
    */
    
    ~Reader(){
    }

    BetweenTwoTs get_at_time(Ratio when) const{
    }
  };


  class Writer : public ReaderWriter<TimeData, TimeDataVector>{

    TimeData *_time_data;
    bool _has_cancelled = false;
    
  public:
    
    Writer(TimeData *time_data, bool get_clean = false)
      : ReaderWriter<TimeData, TimeDataVector>(time_data->get_write_vector(get_clean))
      , _time_data(time_data)
    {
      R_ASSERT_NON_RELEASE(THREADING_is_main_thread());
    }
    
    ~Writer(){
      if (_has_cancelled){
        delete this->_vector;
      }else
        _time_data->replace_vector(this->_vector);
    }

    void add(T &data){
      /*
      int pos = BinarySearch_Left(data.time);
      printf("POS: %d\n", pos);
      */
      this->_vector->push_back(data);
      this->_vector->_is_sorted = false;
    }

    void clear(void){
      this->_vector->clear();
      this->_vector->_is_sorted = true;
    }

    bool remove_at_pos(int pos){
      if (pos < 0 || pos >= this->size()){
        return false;
      }
      
      this->_vector->remove_pos(pos);
      this->_vector->_is_sorted = false; // (_vector->remove_pos doesn't keep order)
      return true;
    }

    void remove_at_positions(std::vector<int> positions){
      // Make sure we iterate from last to first position.
      // If not, we might remove wrong elements since 'remove_at_pos' just swap 'pos' with last element and decrements size.
      std::sort(positions.begin(), positions.end(), std::greater<int>());

      int last = -1;
      for(int pos : positions){
        if (last==pos){
          R_ASSERT_NON_RELEASE(false);
        } else {
          remove_at_pos(pos);
          last = pos;
        }
      }
    }
    
    bool remove_at_time(Ratio ratio){
      int pos = this->find_pos(ratio);
      if (pos < 0)
        return false;
      return remove_at_pos(pos);
    }

    void remove_everything_after(Ratio time, bool include_at = true){
      sortit_if_necessary();
      
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
      sortit_if_necessary();
      
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

      remove_at_pos(to_remove);

      return ret;
    }

    // equiv. to List_InsertLines3.
    void insert_lines(const Ratio &where_to_start, const Ratio &how_much){
      insert_ratio(where_to_start, how_much);
    }

    void expand(const Ratio &start, const Ratio &end, const Ratio &new_end, const Ratio &last_legal_place){
      sortit_if_necessary();
      
      for(int i=0;i<this->size();i++){
        
        T &t = this->at_ref(i);

        Ratio new_time;
        
        if (t._time > start) {

          if (t._time >= end) {

            if (new_end < end)
              new_time -= (end-new_end);
            else
              new_time += (new_end-end);
            
          } else {

            new_time = scale_ratio(t._time, start, end, start, new_end);
          }
          
        }

        if (new_time > last_legal_place) {
          R_ASSERT_NON_RELEASE(false);
          new_time = last_legal_place;
        }

        t._time = new_time;
      }
    }
    
    void cancel(void){
      R_ASSERT_NON_RELEASE(_has_cancelled==false);
      _has_cancelled = true;
    }

    // Must be called after modifying if you expect data to be sorted. Does nothing if the vector is already sorted.
    // Not necessary to call before any of the methods in the Writer class, but may be needed if calling any of the methods in ReaderWriter.
    void sortit_if_necesarry(void){
      this->_vector->sortit_if_necessary();
    }

    // If changing a "at_ref()._time" value, call this one afterwards.
    void mark_not_sorted(void){
      this->_vector->_is_sorted = false;
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

