
#ifndef RADIUM_COMMON_VECTOR_HPP
#define RADIUM_COMMON_VECTOR_HPP

#include <vector>
#include <algorithm>

#include "validatemem_proc.h"
#include "RT_memory_allocator_proc.h"

#include "LockAsserter.hpp"

/**
 *
 * radium::Vector validates multithreaded access and have special support for realtime usage including optional RT-safe allocation and deallocation.
 *
 **/


extern bool g_qtgui_has_stopped;


namespace radium{

enum class AllocatorType{
  RT,
  STD
};

template <typename T>
static inline constexpr int find_vector_preallocate_size(const int max_size){
  
  int ret = (int)sizeof(T);
  
  for(;;){
    int maybe = ret + (int)sizeof(T);
    if (maybe > max_size)
      return ret;
    else
      ret = maybe;
  }
}
  
// Note:  PREALLOCATED_SIZE is number of bytes, not number of elements. Number of elements is rounded down, but there will always be room for at least one element.
template <typename T, AllocatorType ALLOCATOR_TYPE = AllocatorType::STD, int PREALLOCATED_SIZE = 256>
struct Vector{

  /*
  static_assert(
                ALLOCATOR_TYPE == AllocatorType::STD || std::is_trivially_copyable<T>::value,
                "Doesn't have to be a problem. The assertion is just added because it seems likely to be an error in the code if this assertion fails."
                );
  */
  
private:
  int _num_elements_max;

  class NumElements{
    int _num_elements = 0; // Only accessed from the thread writing to the vector.
    DEFINE_ATOMIC(int, _num_elements_relaxed) = 0; // Can be accessed from any thread.

  public:
    int get(void) const {
      return _num_elements;
    }

    int get_relaxed(void) const {
      return ATOMIC_GET_RELAXED(_num_elements_relaxed);
    }

    void set(int new_num_elements){
      _num_elements = new_num_elements;
      ATOMIC_SET_RELAXED(_num_elements_relaxed, new_num_elements);
    }

    void inc(void){
      set(_num_elements+1);
    }
    void dec(void){
      set(_num_elements-1);
    }
  };
  
  NumElements _num_elements;

  T *_next_elements = NULL;

  int _num_elements_ready_for_freeing = 0;
  T *_elements_ready_for_freeing = NULL;
  
  int _next_num_elements_max;

#if !defined(RELEASE)
  LockAsserter _lockAsserter;
#endif
  
  // Normally it would be a typo or an error if trying to copy a radium::Vector.
  Vector(const Vector&) = delete;
  Vector& operator=(const Vector&) = delete;


public:
  
  T *_elements;

private:

  char  _pre_allocated_memory[find_vector_preallocate_size<T>(PREALLOCATED_SIZE)] __attribute__((aligned(std::alignment_of<T>::value)));

public:
  
  Vector() {
    LOCKASSERTER_EXCLUSIVE_NON_RELEASE(&_lockAsserter);

    constexpr int num_preallocated_elements = (int)sizeof(_pre_allocated_memory) / (int)sizeof(T);
    static_assert(num_preallocated_elements==std::max(1, int(PREALLOCATED_SIZE / (int)sizeof(T))), "?");

    _num_elements_max = num_preallocated_elements;

    _elements = (T*)_pre_allocated_memory;
 
    R_ASSERT_NON_RELEASE(_num_elements_max > 0);
    R_ASSERT_NON_RELEASE(_elements!=NULL);
  }

  Vector(const Vector *vector)
    : Vector()
  {
    if (vector != NULL && vector->size()>0) {

      int size = vector->size();

      constexpr int num_preallocated_elements = (int)sizeof(_pre_allocated_memory) / (int)sizeof(T);
      static_assert(num_preallocated_elements==std::max(1, int(PREALLOCATED_SIZE / (int)sizeof(T))), "?");

      if (size > num_preallocated_elements) {
        
        if (ALLOCATOR_TYPE == AllocatorType::RT) {

          _num_elements_max = vector->_num_elements_max;
          _elements = RT_alloc_raw2<T>(_num_elements_max, _num_elements_max, "Vector.hpp");
          
        } else {
          
          _num_elements_max = vector->_num_elements_max;
          _elements = (T*)V_calloc((size_t)_num_elements_max, sizeof(T));
          
        }
      }
      
      std::copy(&vector->_elements[0], &vector->_elements[size], _elements);
      
      _num_elements.set(size);
    }
 
    R_ASSERT_NON_RELEASE(_num_elements_max > 0);
    R_ASSERT_NON_RELEASE(_elements!=NULL);
  }

  ~Vector(){
    LOCKASSERTER_EXCLUSIVE_NON_RELEASE(&_lockAsserter);

    // Don't want to free static global memory during shut down since it may be used by threads which are not shut down. (hmm, this will probably cover bugs)
    if (g_qtgui_has_stopped==false){
      free_internal(_elements, _num_elements.get());
      _elements = NULL; // For debugging
    }
  }

  // This function can be called in parallell with the other const functions (i.e. the non-mutating ones).
  T operator[](int i) const {
    return at(i);
  }

  T at(int i) const {
    LOCKASSERTER_SHARED_NON_RELEASE(&_lockAsserter);

    return at_internal(i);
  }

  T &at_first(void) const {
    //fprintf(stderr, "\nat_ref. i: %d. _num_elements: %d.\n", i, _num_elements.get());
    LOCKASSERTER_SHARED_NON_RELEASE(&_lockAsserter);

    int size = _num_elements.get();
    R_ASSERT_RETURN_IF_FALSE2(size > 0, _elements[0]);

    return _elements[0];
  }

  T &at_last(void) const {
    //fprintf(stderr, "\nat_ref. i: %d. _num_elements: %d.\n", i, _num_elements.get());
    LOCKASSERTER_SHARED_NON_RELEASE(&_lockAsserter);

    int size = _num_elements.get();
    R_ASSERT_RETURN_IF_FALSE2(size > 0, _elements[0]);
    
    return _elements[size-1];
  }

  T &at_ref(int i) const {
    //fprintf(stderr, "\nat_ref. i: %d. _num_elements: %d.\n", i, _num_elements.get());
    LOCKASSERTER_SHARED_NON_RELEASE(&_lockAsserter);

    return at_internal(i);
  }

  bool is_preallocated(T *elements) const {
    return (char*)elements==&_pre_allocated_memory[0];
  }
  
private:

  void free_internal(T *elements, int size) const {
    if (!std::is_trivially_destructible<T>::value){
      //      R_ASSERT_NON_RELEASE(false); // never tested
      for(int i=0;i<size;i++){
        //printf("   radium::Vector::free_internal %d\n", i);
        elements[i].~T();
      }
    }

    if (!is_preallocated(elements)) {
      if (ALLOCATOR_TYPE == AllocatorType::RT)
        RT_free_raw(elements, "Vector.hpp");
      else
        V_free(elements);
    }
  }
  
  T &at_internal(int i) const {
    //fprintf(stderr, "\nat_internal. i: %d. _num_elements: %d.\n", i, _num_elements.get());
    R_ASSERT_RETURN_IF_FALSE2(i>=0, _elements[0]);
    R_ASSERT_RETURN_IF_FALSE2(i<_num_elements.get(), _elements[0]);
    
    return _elements[i];
  }
  
public:
  
  T* ref(int i) const {
    LOCKASSERTER_SHARED_NON_RELEASE(&_lockAsserter);

    R_ASSERT_RETURN_IF_FALSE2(i>=0, &_elements[0]);
    R_ASSERT_RETURN_IF_FALSE2(i<_num_elements.get(), &_elements[0]);

    return &_elements[i];
  }

  // This function can be called in parallell with the other const functions (i.e. the non-mutating ones).
  const T* begin() const {
    LOCKASSERTER_SHARED_NON_RELEASE(&_lockAsserter);
    
    return &_elements[0];
  }

  // This function can be called in parallell with the other const functions (i.e. the non-mutating ones).
  const T* end() const {
    LOCKASSERTER_SHARED_NON_RELEASE(&_lockAsserter);
    
    return &_elements[_num_elements.get()];
  }

  // This function can be called in parallell with the other const functions (i.e. the non-mutating ones).
  bool is_empty(void) const {
    LOCKASSERTER_SHARED_NON_RELEASE(&_lockAsserter);
    
    return _num_elements.get() == 0;
  }

  // This function can be called in parallell with the other const functions (i.e. the non-mutating ones).
  int size(void) const {
    LOCKASSERTER_SHARED_NON_RELEASE(&_lockAsserter);
    
    return _num_elements.get();
  }

  // Thread safe function to get size. Note that the result value can not be trusted 100% if called from another thread.
  // Function can be used to find out if size have changed since last time, and it's good enough if the result is correct 99.99% of the time.
  int size_relaxed(void) const {
    return _num_elements.get_relaxed();
  }

  int free_space(void) const {
    LOCKASSERTER_SHARED_NON_RELEASE(&_lockAsserter);

    return _num_elements_max - _num_elements.get();
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
    LOCKASSERTER_SHARED_NON_RELEASE(&_lockAsserter);

    R_ASSERT(_elements_ready_for_freeing == NULL);
    R_ASSERT(_next_elements == NULL);

    int new_num_elements = _num_elements.get() + how_many;

    R_ASSERT(_num_elements_max > 0);
      
    if (new_num_elements > _num_elements_max) {

      _next_num_elements_max = find_next_num_elements_max(new_num_elements);
        
      _next_elements = create_new_elements(_next_num_elements_max);
    }
  }

  void ensure2(int n){
    ensure_there_is_room_for_more_without_having_to_allocate_memory(n - _num_elements.get());
  }
    
  // Must be called after calling 'add' if 'ensure_there_is_room_for_one_more_without_having_to_allocate_memory' was called before 'add'.
  void post_add(void){
    LOCKASSERTER_SHARED_NON_RELEASE(&_lockAsserter);
    
    if (_elements_ready_for_freeing != NULL){
      free_internal(_elements_ready_for_freeing, _num_elements_ready_for_freeing);
      _elements_ready_for_freeing = NULL;
    }
  }
  
private:

  int find_next_num_elements_max(int minimum){
    int new_num_elements_max = _num_elements_max;
    
    while(new_num_elements_max < minimum)
      new_num_elements_max *= 2;
    
    return new_num_elements_max;
  }
  
  T *create_new_elements(int &new_num_elements_max) const {
    T *new_elements;

    if (ALLOCATOR_TYPE == AllocatorType::RT)
      new_elements = RT_alloc_raw2<T>(new_num_elements_max, new_num_elements_max, "Vector.hpp");
    else
      new_elements = (T*) V_calloc(sizeof(T), (size_t)new_num_elements_max);
    
    int size = _num_elements.get();

    std::copy(&_elements[0], &_elements[size], new_elements);
          
    return new_elements;
  }
  
  void reserve_internal(int new_num_elements_max, bool do_lock_player){
#if !defined(RELEASE)
    if (ALLOCATOR_TYPE != AllocatorType::RT){
      ASSERT_NON_RT_NON_RELEASE(); // V_realloc should already have an assertion for this, but the memory functions are a bit chaotic.
    }
#endif
    
    R_ASSERT_NON_RELEASE(new_num_elements_max > _num_elements_max);

    // Scale up to next multiple by 2.
    new_num_elements_max = find_next_num_elements_max(new_num_elements_max);

    int old_num_elements = _num_elements.get();
    
    T* old_elements = _elements;
    T* new_elements = create_new_elements(new_num_elements_max);

    if (do_lock_player){
      
      R_ASSERT_NON_RELEASE(ALLOCATOR_TYPE != AllocatorType::RT);
      
#if defined(TEST_TIMEDATA_MAIN)
      abort();
#else
      radium::PlayerLock lock;
#endif      
      LOCKASSERTER_EXCLUSIVE_NON_RELEASE(&_lockAsserter);

      _elements = new_elements;
      _num_elements_max = new_num_elements_max;

    } else {
      
      _elements = new_elements;
      _num_elements_max = new_num_elements_max;
      
    } 

    R_ASSERT_NON_RELEASE(_num_elements.get()==old_num_elements);
    free_internal(old_elements, old_num_elements);
  }
  
  void basic_push_back(const T &t){
    R_ASSERT(_num_elements_max > 0);

    int new_num_elements = _num_elements.get() + 1;
    
    if (new_num_elements > _num_elements_max) 
      reserve_internal(new_num_elements, false);

    //printf("111111111111. radium::Vector::basic_push_back %p -> %p\n", &t, &_elements[_num_elements.get()]);

    if (std::is_trivial<T>::value)
      _elements[_num_elements.get()] = t;
    else
      new (&_elements[_num_elements.get()]) T(t);
    
    //printf("22222222222. radium::Vector::basic_push_back %p -> %p\n", &t, &_elements[_num_elements.get()]);
    
    _num_elements.set(new_num_elements);
  }

  void basic_push_back(T &&t){    
    R_ASSERT(_num_elements_max > 0);

    int new_num_elements = _num_elements.get() + 1;
    
    if (new_num_elements > _num_elements_max) 
      reserve_internal(new_num_elements, false);

    //printf("111111111111. radium::Vector::basic_push_back2 %p -> %p\n", &t, &_elements[_num_elements.get()]);
    new (&_elements[_num_elements.get()]) T(std::move(t));
    //printf("22222222222. radium::Vector::basic_push_back2 %p -> %p\n", &t, &_elements[_num_elements.get()]);
    
    _num_elements.set(new_num_elements);
  }

  void remove_pos_internal(const int pos, const bool keep_order){
    const int old_size = _num_elements.get();
    
    R_ASSERT_RETURN_IF_FALSE(pos < old_size);
    R_ASSERT_RETURN_IF_FALSE(pos >= 0);
    
    _num_elements.dec();
 
    const int old_last_pos = old_size-1;
    
    if (!std::is_trivially_destructible<T>::value)
      _elements[pos].~T();

    if (pos < old_last_pos) {
      
      if (keep_order) {
        
        if (std::is_trivially_copyable<T>::value) {

          memmove((void*)&_elements[pos], (void*)&_elements[pos+1], (size_t)(old_last_pos - pos) * sizeof(T));
          
        } else {
          
          new (&_elements[pos]) T(_elements[pos+1]);
          
          for(int i=pos+1;i<old_last_pos;i++) {
#if 1
            // Hopefully this one is correct. If not, the other version should be.
            _elements[i] = std::move(_elements[i+1]);
#else
            if (!std::is_trivially_destructible<T>::value)
              _elements[i].~T();
            
            new (&_elements[i]) T(_elements[i+1]);
#endif
          }
          
          if (!std::is_trivially_destructible<T>::value)
            _elements[old_last_pos].~T();
        }
        
      } else {
        
        //printf("...............................assign3 To: %p. From: %p\n", &_elements[pos], &_elements[old_last_pos]);
        
        if (std::is_trivially_copyable<T>::value) {
          
          memcpy((void*)&_elements[pos], (void*)&_elements[old_last_pos], sizeof(T));
          
        } else {
          
#if 1
          // Hopefully this is safe... (if not, the other version should be)
          new (&_elements[pos]) T(std::move(_elements[old_last_pos]));
#else
          new (&_elements[pos]) T(_elements[old_last_pos]);
          
          if (!std::is_trivially_destructible<T>::value)
            _elements[old_last_pos].~T();
#endif
        }
        
        //printf("++++++++++++++++++++...............................assign4\n");
      }
      
    }
    
    
#if !defined(RELEASE)
    memset((void*)&_elements[old_last_pos], 0, sizeof(T)); // for debugging
#endif
  }

  int find_pos_internal(const T &t) const {
    int pos;
    
    for(pos=0 ; pos<_num_elements.get() ; pos++)
      if (_elements[pos]==t)
        break;

    if (pos<_num_elements.get())
      return pos;
    else
      return -1;
  }

public:

  void replace(int i, const T &t){
    LOCKASSERTER_EXCLUSIVE_NON_RELEASE(&_lockAsserter);

    R_ASSERT_RETURN_IF_FALSE(i>=0);
    R_ASSERT_RETURN_IF_FALSE(i<_num_elements.get());

    _elements[i] = t;
  }
  
  // Only RT safe if ensure_there_is_room_for_one_more_without_having_to_allocate_memory is called first AND post_add is called afterwards.
  //
  // This function can NOT be called in parallell with other functions
  //
  void push_back(const T &t){
    LOCKASSERTER_EXCLUSIVE_NON_RELEASE(&_lockAsserter);

    //R_ASSERT(_elements_ready_for_freeing == NULL); <-- Fails when we have preallocated for more than one element.

    if (_next_elements == NULL) {

      basic_push_back(t);
      
    } else {

      if (!is_preallocated(_elements)) {
        
        _num_elements_ready_for_freeing = _num_elements.get();
        _elements_ready_for_freeing = _elements;
        
      }
      
      _num_elements.inc();

      R_ASSERT(_num_elements.get() <= _next_num_elements_max);

      _elements = _next_elements;
      _num_elements_max = _next_num_elements_max;
      
      _next_elements = NULL;
      _next_num_elements_max = 0;

      if (std::is_trivial<T>::value)
        _elements[_num_elements.get()-1] = t;
      else
        new (&_elements[_num_elements.get()-1]) T(t);
    }
  }

  void push_back(T &&t){
    LOCKASSERTER_EXCLUSIVE_NON_RELEASE(&_lockAsserter);

    //R_ASSERT(_elements_ready_for_freeing == NULL); <-- Fails when we have preallocated for more than one element.

    if (_next_elements == NULL) {

      basic_push_back(std::move(t));
      
    } else {

      if (!is_preallocated(_elements)) {
        
        _num_elements_ready_for_freeing = _num_elements.get();
        _elements_ready_for_freeing = _elements;
        
      }
      
      _num_elements.inc();

      R_ASSERT(_num_elements.get() <= _next_num_elements_max);

      _elements = _next_elements;
      _num_elements_max = _next_num_elements_max;
      
      _next_elements = NULL;
      _next_num_elements_max = 0;

      _elements[_num_elements.get()-1] = std::move(t);
    }
  }

  // Obtains the player lock while modifying state.
  //
  // Convenience function. Must not be called from a realtime thread.
  //
  // Can not be mixed with parallel use of 'ensure_there_is_room_for_more_without_having_to_allocate_memory' or 'reserve_in_realtime_safe_manner'.
  void push_back_in_realtime_safe_manner(const T &t){
    R_ASSERT(PLAYER_current_thread_has_lock()==false);

    ensure_there_is_room_for_more_without_having_to_allocate_memory(1);
    PLAYER_lock();{
      push_back(t);
    }PLAYER_unlock();
    post_add();
  }


  // Ensures at least new_num_elements_max is reserved so that add/insert/push_back can be callled without any memory being allocated.
  //
  // If we have to allocate new space, 'new_num_elements_max' will be set to at least the next n^2.
  // Returns immediately if there is already room for new_num_elements_max elements.
  //
  // Not RT safe unless ALLOCATOR_TYPE == AllocatorType::RT.
  //
  void reserve(int new_num_elements_max) {
    LOCKASSERTER_EXCLUSIVE_NON_RELEASE(&_lockAsserter);

    R_ASSERT(_next_elements == NULL);
    
    if (new_num_elements_max > _num_elements_max)
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

    R_ASSERT(_next_elements == NULL);
    
    {
      LOCKASSERTER_SHARED_NON_RELEASE(&_lockAsserter);
      if (new_num_elements_max <= _num_elements_max)
        return;
    }
    
    reserve_internal(new_num_elements_max, do_lock);
  }
  
  // NOT RT safe
  //
  // This function can NOT be called in parallell with other functions
  void append(const Vector<T> *ts){
    LOCKASSERTER_EXCLUSIVE_NON_RELEASE(&_lockAsserter);
    
    R_ASSERT(_next_elements == NULL);

#if 0
    
    for (const T &t : *ts)
      basic_push_back(t);
    
#else
    
    const int size = _num_elements.get();

    const int new_size = size + ts->size();

    if (new_size > _num_elements_max)
      reserve_internal(new_size, false);
    
    int pos = size;
    
    for (const T &t : *ts)
      if (std::is_trivial<T>::value)
        _elements[pos++] = t;
      else
        new (&_elements[pos++]) T(t);

    _num_elements.set(new_size);
    
#endif
  }

  // NOT RT safe
  //
  // This function can NOT be called in parallell with other functions
  void append(const Vector<T> &ts){
    append(&ts);
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

  bool intersects(const Vector<T> &ts, bool (*equal)(const T&, const T&)) const {
    for (const T &t1 : ts)
      for(int i = 0 ; i < _num_elements.get() ; i++)
        if (equal(_elements[i], t1))
          return true;

    return false;
  }

  bool only_unique_elements(bool (*equal)(const T&, const T&)) const {
    for(int i1 = 0 ; i1 < _num_elements.get()-1 ; i1++)
      for(int i2 = i1+1 ; i2 < _num_elements.get() ; i2++){
        //printf("i1: %d, i2: %d\n", i1, i2);
        if (equal(_elements[i1], _elements[i2]))
          return false;
      }

    return true;
  }
  
  // This function can be called in parallell with the other const functions (i.e. the non-mutating ones).
  int find_pos(const T &t) const {
    LOCKASSERTER_SHARED_NON_RELEASE(&_lockAsserter);

    return find_pos_internal(t);
  }

  bool contains(const T &t) const {
    return find_pos(t) >= 0;
  }
    
  // RT safe (except for the O(n) performance)
  //
  // This function can NOT be called in parallell with other functions
  void remove(const T &t, bool keep_order = false){
    LOCKASSERTER_EXCLUSIVE_NON_RELEASE(&_lockAsserter);
    
    R_ASSERT(_next_elements == NULL);
    R_ASSERT(_elements_ready_for_freeing == NULL);

    int pos = find_pos_internal(t);
    R_ASSERT_RETURN_IF_FALSE(pos>=0);
    
    remove_pos_internal(pos, keep_order);
  }

  // RT safe (except for the O(n) performance when keep_order==true)
  //
  // This function can NOT be called in parallell with other functions
  void remove_pos(int pos, bool keep_order = false){
    LOCKASSERTER_EXCLUSIVE_NON_RELEASE(&_lockAsserter);

    remove_pos_internal(pos, keep_order);
  }

  T pop(int pos, bool keep_order = false){
    LOCKASSERTER_EXCLUSIVE_NON_RELEASE(&_lockAsserter);
    
    T ret = at_internal(pos);
    
    remove_pos_internal(pos, keep_order);
    
    return ret;
  }

  T pop_back(void){
    return pop(_num_elements.get()-1, false);
  }
  
  template <class S>
  void sort(S comp){
    LOCKASSERTER_EXCLUSIVE_NON_RELEASE(&_lockAsserter);
    
    std::stable_sort(&_elements[0], &_elements[_num_elements.get()], comp);
  }
    
  std::vector<T> to_std_vector(void) const{
    LOCKASSERTER_SHARED_NON_RELEASE(&_lockAsserter);

    std::vector<T> ret;

    ret.reserve(_num_elements.get());

    for(int pos=0 ; pos<_num_elements.get() ; pos++)
      ret.push_back(_elements[pos]);

    return ret;
  }

  const T* get_array(void) const{
    LOCKASSERTER_SHARED_NON_RELEASE(&_lockAsserter);
    return _elements;
  }

  // RT safe
  //
  // This function can NOT be called in parallell with other functions
  void set_num_elements(int new_num_elements) {
    LOCKASSERTER_EXCLUSIVE_NON_RELEASE(&_lockAsserter);

    R_ASSERT_NON_RELEASE(new_num_elements <= _num_elements.get());
    R_ASSERT(_next_elements == NULL);
    
    if (!std::is_trivially_destructible<T>::value){
      const int old_num_elements = _num_elements.get();

      //printf("   radium::Vector::set_num_elements New/Old: %d/%d\n", new_num_elements, old_num_elements);
      
      //      R_ASSERT_NON_RELEASE(false); // never tested
      for(int i=new_num_elements; i < old_num_elements ; i++){
        //printf("   radium::Vector::set_num_elements %d\n", i);
        _elements[i].~T();
      }
    }

    _num_elements.set(new_num_elements);
  }
  
  // RT safe
  //
  // This function can NOT be called in parallell with other functions
  void clear(void) {
    set_num_elements(0);
  }

};


  // A fixed-size multithread-accessible array. I.e. an array that ensures that only one thread is using an element at the same time.
  // It also ensures that we use as few buffers as possible and that we always use the lowest possible indexed free element.
  // E.g. if we only use max 5 number of simultaneous buffers out of 5000 available buffers, the last 4995 buffers will never be used.
  //
  // All elements in the array are delete-ed in the deconstructor. The "content" argument itself for the constructor is not stored, only the buffers in it.
  //
  //
  // Use ScopedMultiThreadAccessArrayElement to conveniently access a currently unused buffer.
  //
  template <typename T, const int SIZE> class MultiThreadAccessArray {

    DEFINE_ATOMIC(bool*, _in_use) = {};
    
    T **_buffers;

    MultiThreadAccessArray(const MultiThreadAccessArray&) = delete;
    MultiThreadAccessArray& operator=(const MultiThreadAccessArray&) = delete;

  public:

    MultiThreadAccessArray(std::function<T*(int)> create_buffer)
    {
      // Using V_calloc/V_free instead of new[]/delete[] since V_calloc ensures the memory is actually allocated.
      _buffers = (T**)V_calloc(sizeof(T*),  SIZE);
      ATOMIC_NAME(_in_use) = (bool*)V_calloc(sizeof(bool), SIZE);
      
      for(int i=0;i<SIZE;i++)
        _buffers[i] = create_buffer(i);
    }
    
    ~MultiThreadAccessArray(){
      for(auto *t : *this)
        delete t;

      V_free(_buffers);
      V_free(ATOMIC_NAME(_in_use));
    }
      
    T* at(int pos) const {
      R_ASSERT_NON_RELEASE(pos>=0 && pos<SIZE);
      return _buffers[pos];
    }
    
    T* const * begin() const {
      return &_buffers[0];
    }

    T* const * end() const {
      return &_buffers[SIZE];
    }

    void RT_release(int pos){
      R_ASSERT_NON_RELEASE(ATOMIC_GET_ARRAY(_in_use, pos)==true);
      ATOMIC_SET_ARRAY(_in_use, pos, false);
    }
    
    int RT_obtain_may_fail(void) {
      for(int i=0;i<SIZE;i++)
        if(ATOMIC_COMPARE_AND_SET_BOOL_ARRAY(_in_use, i, false, true))
          return i;

      return -1;
    }
    int RT_obtain(void){
      int ret = RT_obtain_may_fail();

      if (ret >= 0)
        return ret;
      
      R_ASSERT(false);

      return 0;
    }
  };


  template <typename MTAT> class ScopedMultiThreadAccessArrayElement{
    
    MTAT &_buffers;
    int _pos;

  public:
    
    ScopedMultiThreadAccessArrayElement(MTAT &buffers)
      : _buffers(buffers)
      , _pos(buffers.RT_obtain())
    {
    }
    
    ~ScopedMultiThreadAccessArrayElement(){
      _buffers.RT_release(_pos);
    }

    decltype(_buffers.at(_pos)) RT_get(void){
      return _buffers.at(_pos);
    }
  };



}


#endif
