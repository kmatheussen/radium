
#ifndef RADIUM_COMMON_VECTOR_HPP
#define RADIUM_COMMON_VECTOR_HPP

#include "validatemem_proc.h"

#include "LockAsserter.hpp"

/**
 *
 * radium::Vector validates multithreaded access and have special support for adding elements when called from a realtime thread.
 *
 **/


extern bool g_qtgui_has_stopped;


// NOTE: Can not use radium::Vector if any of the fields in T uses a custom copy constructor (is there any way to detect that before getting a random crash?)


namespace radium{

template <typename T> struct Vector{
  
private:
  int num_elements_max;

  class NumElements{
    int num_elements = 0;
    DEFINE_ATOMIC(int, num_elements_relaxed) = 0;

  public:
    int get(void) const{
      return num_elements;
    }

    int get_relaxed(void) const{
      return ATOMIC_GET_RELAXED(num_elements_relaxed);
    }

    void set(int new_num_elements){
      num_elements = new_num_elements;
      ATOMIC_SET_RELAXED(num_elements_relaxed, new_num_elements);
    }

    void inc(void){
      set(num_elements+1);
    }
    void dec(void){
      set(num_elements-1);
    }
  };
  
  NumElements num_elements;

  T *next_elements;
  
  T *elements_ready_for_freeing;
  
  int next_num_elements_max;
  
  LockAsserter lockAsserter;

  
  // Normally it would be a typo or an error if trying to copy a radium::Vector.
  Vector(const Vector&) = delete;
  Vector& operator=(const Vector&) = delete;

  
public:
  
  T *elements;
  
  Vector()
    : num_elements_max(4)
    , next_elements(NULL)
    , elements_ready_for_freeing(NULL)
    , next_num_elements_max(0)
  {
    LOCKASSERTER_EXCLUSIVE(&lockAsserter);

    R_ASSERT(num_elements_max > 0);
    
    elements = (T*)V_calloc(num_elements_max, sizeof(T));
  }

  ~Vector(){
    LOCKASSERTER_EXCLUSIVE(&lockAsserter);

    // Don't want to free static global memory during shut down since it may be used by threads which are not shut down. (hmm, this will probably cover bugs)
    if (g_qtgui_has_stopped==false){
      V_free(elements);      
      elements = NULL; // For debugging
    }
  }

  // This function can be called in parallell with the other const functions (i.e. the non-mutating ones).
  T operator[](int i) const {
    return at(i);
  }

  T at(int i) const {
    LOCKASSERTER_SHARED(&lockAsserter);

    return at_internal(i);
  }

private:
  
  T at_internal(int i) const {
    R_ASSERT_RETURN_IF_FALSE2(i>=0, elements[0]);
    R_ASSERT_RETURN_IF_FALSE2(i<num_elements.get(), elements[0]);
    
    return elements[i];
  }
  
public:
  
  T* ref(int i) const {
    LOCKASSERTER_SHARED(&lockAsserter);
    
    R_ASSERT(i>=0);
    R_ASSERT(i<num_elements.get());
    
    return &elements[i];
  }

  // This function can be called in parallell with the other const functions (i.e. the non-mutating ones).
  const T* begin() const {
    LOCKASSERTER_SHARED(&lockAsserter);
    
    return &elements[0];
  }

  // This function can be called in parallell with the other const functions (i.e. the non-mutating ones).
  const T* end() const {
    LOCKASSERTER_SHARED(&lockAsserter);
    
    return &elements[num_elements.get()];
  }

  // This function can be called in parallell with the other const functions (i.e. the non-mutating ones).
  bool is_empty(void) const {
    LOCKASSERTER_SHARED(&lockAsserter);
    
    return num_elements.get() == 0;
  }

  // This function can be called in parallell with the other const functions (i.e. the non-mutating ones).
  int size(void) const {
    LOCKASSERTER_SHARED(&lockAsserter);
    
    return num_elements.get();
  }

  // Thread safe function to get size. Note that the result value can not be trusted 100% if called from another thread.
  // Function can be used to find out if size have changed since last time, and it's good enough if the result is correct 99.99% of the time.
  int size_relaxed(void) const {
    return num_elements.get_relaxed();
  }

  int free_space(void) const {
    LOCKASSERTER_SHARED(&lockAsserter);

    return num_elements_max - num_elements.get();
  }

  // NOT RT safe
  //
  // This function must _always or never_ be called before calling add. No mixing.
  //
  // This function CAN be called in parallell with the const functions (i.e. the non-mutating ones),
  // but it can not be called in parallel with itself or any other non-const/mutating function.
  // (it is not asserted that this function is not called in parallell with itself)
  //
  // post_add MUST be called after calling add after calling ensure_there_is_room_for_more_without_having_to_allocate_memory.
  void ensure_there_is_room_for_more_without_having_to_allocate_memory(int how_many = 1){
    LOCKASSERTER_SHARED(&lockAsserter);

    R_ASSERT(elements_ready_for_freeing == NULL);
    R_ASSERT(next_elements == NULL);

    int new_num_elements = num_elements.get() + how_many;

    R_ASSERT(num_elements_max > 0);
      
    if (new_num_elements > num_elements_max) {

      next_num_elements_max = find_next_num_elements_max(new_num_elements);
        
      next_elements = create_new_elements(next_num_elements_max);
    }
  }

  void ensure2(int n){
    ensure_there_is_room_for_more_without_having_to_allocate_memory(n - num_elements.get());
  }
    
  // Must be called after calling 'add' if 'ensure_there_is_room_for_one_more_without_having_to_allocate_memory' was called before 'add'.
  void post_add(void){
    LOCKASSERTER_SHARED(&lockAsserter);
    
    if (elements_ready_for_freeing != NULL){
      V_free(elements_ready_for_freeing);
      elements_ready_for_freeing = NULL;
    }
  }
  
private:

  int find_next_num_elements_max(int minimum){
    int new_num_elements_max = num_elements_max;
    
    while(new_num_elements_max < minimum)
      new_num_elements_max *= 2;
    
    return new_num_elements_max;
  }
  
  T *create_new_elements(int new_num_elements_max) const {
    T *new_elements = (T*) V_calloc(sizeof(T), new_num_elements_max);
    memcpy(new_elements, elements, sizeof(T)*num_elements.get());
    return new_elements;
  }
  
  void reserve_internal(int new_num_elements_max, bool do_lock_player){
    ASSERT_NON_RT_NON_RELEASE(); // V_realloc should already have an assertion for this, but the memory functions are a bit chaotic.
    
    R_ASSERT_NON_RELEASE(new_num_elements_max > num_elements_max);

    // Scale up to next multiple by 2.
    new_num_elements_max = find_next_num_elements_max(new_num_elements_max);
    
    T* old_elements = elements;
    T* new_elements = create_new_elements(new_num_elements_max);

    if (do_lock_player){
      radium::PlayerLock lock;
      
      LOCKASSERTER_EXCLUSIVE(&lockAsserter);

      elements = new_elements;
      num_elements_max = new_num_elements_max;

    } else {
      
      elements = new_elements;
      num_elements_max = new_num_elements_max;
      
    } 

    V_free(old_elements);
  }
  
  void basic_push_back(T t){    
    R_ASSERT(num_elements_max > 0);

    int new_num_elements = num_elements.get() + 1;
    
    if (new_num_elements > num_elements_max) 
      reserve_internal(new_num_elements, false);

    elements[num_elements.get()] = t;
    
    num_elements.set(new_num_elements);
  }

  void remove_pos_internal(int pos, bool keep_order){
    R_ASSERT_RETURN_IF_FALSE(pos < num_elements.get());
    R_ASSERT_RETURN_IF_FALSE(pos >= 0);

    if (keep_order) {
      
      int i;
      num_elements.dec();

      for(i=pos;i<this->num_elements.get();i++)
        this->elements[i]=this->elements[i+1];
      
    } else {

      if (num_elements.get()==1){
        R_ASSERT(pos==0);
      } else {
        elements[pos] = elements[num_elements.get()-1];
      }
      
      num_elements.dec();
    }

    memset(&elements[num_elements.get()], 0, sizeof(T)); // for debugging
  }

  int find_pos_internal(const T t) const {
    int pos;
    
    for(pos=0 ; pos<num_elements.get() ; pos++)
      if (elements[pos]==t)
        break;

    if (pos<num_elements.get())
      return pos;
    else
      return -1;
  }

public:

  // Only RT safe if ensure_there_is_room_for_one_more_without_having_to_allocate_memory is called first AND post_add is called afterwards.
  //
  // This function can NOT be called in parallell with other functions
  //
  void push_back(T t){
    LOCKASSERTER_EXCLUSIVE(&lockAsserter);

    //R_ASSERT(elements_ready_for_freeing == NULL); <-- Fails when we have preallocated for more than one element.

    if (next_elements == NULL) {

      basic_push_back(t);
      
    } else {

      num_elements.inc();

      R_ASSERT(num_elements.get() <= next_num_elements_max);

      elements_ready_for_freeing = elements;
      
      elements = next_elements;
      num_elements_max = next_num_elements_max;
      
      next_elements = NULL;
      next_num_elements_max = 0;

      elements[num_elements.get()-1] = t;
    }
  }

  // Obtains the player lock while modifying state.
  //
  // Convenience function. Must not be called from a realtime thread.
  //
  // Can not be mixed with parallel use of 'ensure_there_is_room_for_more_without_having_to_allocate_memory' or 'reserve_in_realtime_safe_manner'.
  void push_back_in_realtime_safe_manner(T t){
    R_ASSERT(PLAYER_current_thread_has_lock()==false);

    ensure_there_is_room_for_more_without_having_to_allocate_memory(1);

    PLAYER_lock();{
      push_back(t);
    }PLAYER_unlock();

    post_add();
  }


  // Ensures at least new_num_elements_max is reserved so that add/insert/push_back can be callled without any memory being allocated.
  //
  // Scales 'new_num_elements_max' up to next n^2.
  // Returns immediately if there is already room for new_num_elements_max elements.
  //
  // NOT RT safe
  //
  void reserve(int new_num_elements_max) {
    LOCKASSERTER_EXCLUSIVE(&lockAsserter);

    R_ASSERT(next_elements == NULL);
    
    if (new_num_elements_max <= num_elements_max)
      return;
    
    reserve_internal(new_num_elements_max, false);
  }

  // Obtains the player lock while modifying state.
  //
  // Convenience function. Must not be called from a realtime thread.
  //
  // Can not be mixed with parallel use of 'ensure_there_is_room_for_more_without_having_to_allocate_memory' or 'push_back_in_realtime_safe_manner'.
  //
  void reserve_in_realtime_safe_manner(int new_num_elements_max, bool do_lock = true) {
    R_ASSERT(PLAYER_current_thread_has_lock()==false);

    R_ASSERT(next_elements == NULL);
    
    {
      LOCKASSERTER_SHARED(&lockAsserter);
      if (new_num_elements_max <= num_elements_max)
        return;
    }
    
    reserve_internal(new_num_elements_max, do_lock);
  }
  
  // NOT RT safe
  //
  // This function can NOT be called in parallell with other functions
  void append(Vector<T> *ts){
    LOCKASSERTER_EXCLUSIVE(&lockAsserter);
    
    R_ASSERT(next_elements == NULL);

    for (T t : *ts)
      basic_push_back(t);
  }

  // NOT RT safe
  //
  // This function can NOT be called in parallell with other functions
  void append(Vector<T> &ts){
    LOCKASSERTER_EXCLUSIVE(&lockAsserter);
    
    R_ASSERT(next_elements == NULL);
    
    for (T t : ts)
      basic_push_back(t);
  }

  Vector<T> intersection(const Vector<T> &ts) const {
    Vector<T> ret;
    
    for (const T &t : ts)
      if (contains(t))
        ret.push_back(ret);

    return ret;
  }
  
  bool intersects(const Vector<T> &ts) const {
    for (const T &t : ts)
      if (contains(t))
        return true;

    return false;
  }

  bool intersects(const Vector<T> &ts, bool (*equal)(const T, const T)) const {
    for (const T &t1 : ts)
      for(int i = 0 ; i < num_elements.get() ; i++)
        if (equal(elements[i], t1))
          return true;

    return false;
  }

  bool only_unique_elements(bool (*equal)(const T, const T)) const {
    for(int i1 = 0 ; i1 < num_elements.get()-1 ; i1++)
      for(int i2 = i1+1 ; i2 < num_elements.get() ; i2++){
        //printf("i1: %d, i2: %d\n", i1, i2);
        if (equal(elements[i1], elements[i2]))
          return false;
      }

    return true;
  }
  
  // This function can be called in parallell with the other const functions (i.e. the non-mutating ones).
  int find_pos(const T t) const {
    LOCKASSERTER_SHARED(&lockAsserter);

    return find_pos_internal(t);
  }

  bool contains(const T t) const {
    return find_pos(t) >= 0;
  }
    
  // RT safe (except for the O(n) performance)
  //
  // This function can NOT be called in parallell with other functions
  void remove(const T t, bool keep_order = false){
    LOCKASSERTER_EXCLUSIVE(&lockAsserter);
    
    R_ASSERT(next_elements == NULL);
    R_ASSERT(elements_ready_for_freeing == NULL);

    int pos = find_pos_internal(t);
    R_ASSERT_RETURN_IF_FALSE(pos>=0);
    
    remove_pos_internal(pos, keep_order);
  }

  // RT safe (except for the O(n) performance)
  //
  // This function can NOT be called in parallell with other functions
  void remove_pos(int pos, bool keep_order = false){
    LOCKASSERTER_EXCLUSIVE(&lockAsserter);

    remove_pos_internal(pos, keep_order);
  }

  T pop(int pos, bool keep_order = false){
    LOCKASSERTER_EXCLUSIVE(&lockAsserter);
    
    T ret = at_internal(pos);
    
    remove_pos_internal(pos, keep_order);
    
    return ret;
  }

  T pop_back(void){
    return pop(num_elements.get()-1, false);
  }
  
  template <class S>
  void sort(S comp){
    LOCKASSERTER_EXCLUSIVE(&lockAsserter);
    
    std::sort(&elements[0], &elements[num_elements.get()], comp);
  }
    
  std::vector<T> to_std_vector(void) const{
    LOCKASSERTER_SHARED(&lockAsserter);

    std::vector<T> ret;

    ret.reserve(num_elements.get());

    for(int pos=0 ; pos<num_elements.get() ; pos++)
      ret.push_back(elements[pos]);

    return ret;
  }

  // RT safe
  //
  // This function can NOT be called in parallell with other functions
  void clear(void) {
    LOCKASSERTER_EXCLUSIVE(&lockAsserter);

    R_ASSERT(next_elements == NULL);
    num_elements.set(0);
  }

};



  // A fixed-size multithread-accessible array. I.e. an array that ensures that only one thread is using an element at the same time.
  // It also ensures that we use as few buffers as possible and that we always use the lowest possible indexed free element.
  // E.g. if we only use max 5 number of simultaneous buffers out of 5000 available buffers, the last 4995 buffers will never be used.
  //
  // All elements in the array are delete-ed in the deconstructor. The "content" argument itself for the constructor is not stored, only the buffers in it.
  //
  // Use ScopedMultiThreadAccessArrayElement to conveniently access a currently unused buffer.
  //
  template <typename T> class MultiThreadAccessArray{

    int _size;
    DEFINE_ATOMIC(bool*, _in_use) = {};
    
    T **_buffers;

    MultiThreadAccessArray(const MultiThreadAccessArray&) = delete;
    MultiThreadAccessArray& operator=(const MultiThreadAccessArray&) = delete;

  public:

    MultiThreadAccessArray(const std::vector<T*> &content)
      : _size(content.size())
    {
      // Use V_calloc/V_free instead of new[]/delete[] since V_calloc ensures the memory is actually allocated. (should probably override global new*)
      _buffers = (T**)V_calloc(sizeof(T*),  _size);
      ATOMIC_NAME(_in_use) = (bool*)V_calloc(sizeof(bool), _size);
      
      for(int i=0;i<_size;i++){
        _buffers[i] = content.at(i);
      }
    }

    ~MultiThreadAccessArray(){
      for(auto *t : *this)
        delete t;

      V_free(_buffers);
      V_free(ATOMIC_NAME(_in_use));
    }
      
    T* at(int pos) const {
      R_ASSERT_NON_RELEASE(pos>=0 && pos<_size);
      return _buffers[pos];
    }
    
    T* const * begin() const {
      return &_buffers[0];
    }

    T* const * end() const {
      return &_buffers[_size];
    }
    
    void RT_release(int pos){
      R_ASSERT_NON_RELEASE(ATOMIC_GET_ARRAY(_in_use, pos)==true);
      ATOMIC_SET_ARRAY(_in_use, pos, false);
    }
    
    int RT_obtain(void){
      for(int i=0;i<_size;i++)
        if(ATOMIC_COMPARE_AND_SET_BOOL_ARRAY(_in_use, i, false, true))
          return i;

      R_ASSERT(false);
      return 0;
    }
  };


  template <typename T> class ScopedMultiThreadAccessArrayElement{
    
    MultiThreadAccessArray<T> &_buffers;
    int _pos;

  public:
    
    ScopedMultiThreadAccessArrayElement(MultiThreadAccessArray<T> &buffers)
      : _buffers(buffers)
      , _pos(buffers.RT_obtain())
    {
    }
    
    ~ScopedMultiThreadAccessArrayElement(){
      _buffers.RT_release(_pos);
    }

    T *RT_get(void){
      return _buffers.at(_pos);
    }
  };



}


#endif
