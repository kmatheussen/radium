/* Copyright 2016-2020 Kjetil S. Matheussen

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


#pragma once


namespace radium{


// Class to store a pointer.
// * The main thread can set, replace and free the pointer at any time.
// * One RT thread (or non-RT thread) can access the pointer at any time by using the ScopedUsage class.
//   (Use AtomicPointerStorageMultipleReaders instead if you need more than one reader (see below))
//
// This code is genious in what it does, but it takes some time understanding how it works.
// Still, there are good reasons to believe the code is correct:
// * There's lots of assertions
// * Stresstests are run regularly (see "make test_timedata" in Makefile and the test itself in common/TimeData.cpp)
// * asan and ubsan are always enabled in DEBUG mode, and sometimes tsan.
// * When looking over the code I have always concluded that it's correct.
// * There have never been problems with the code
//


template <typename T>
class AtomicPointerStorage{
    
  template <typename T2> friend class RT_AtomicPointerStorage_ScopedUsage;
  template <typename T2> friend class AtomicPointerStorageMultipleReaders;
  template <typename T2> friend class RT_AtomicPointerStorageMultipleReaders_ScopedUsage;
  
  AtomicPointerStorage(const AtomicPointerStorage&) = delete;
  AtomicPointerStorage& operator=(const AtomicPointerStorage&) = delete;
  
#if !defined(RELEASE)
  bool _is_used2 = false;
#endif
  
private:
  
  DEFINE_ATOMIC(T *, _pointer) = NULL;
  DEFINE_ATOMIC(T *, _old_pointer_to_be_freed) = NULL;

  std::function<void(T*)> _free_pointer_function;
  
  void maybe_free_something(T *a, T *b)
  {
	  if (a!=NULL)
		  _free_pointer_function(a);
	  
	  if (b!=NULL)
		  _free_pointer_function(b);
  }
  
  
private:
  
  T *RT_obtain_pointer(void) {
    auto *ret = ATOMIC_SET_RETURN_OLD(_pointer, NULL);
    
#if !defined(RELEASE)
    if(_is_used2)
      abort();
    _is_used2 = true;
#endif
    
    return ret;
  }
  
  void RT_release_pointer(T *pointer) {
    
#if !defined(RELEASE)
    if(!_is_used2)
      abort();
    _is_used2 = false;
#endif

    if(!atomic_compare_and_set_pointer(reinterpret_cast<void**>(&ATOMIC_NAME(_pointer)), NULL, pointer)){ // The void**-cast is a workaround for compiler error. Strange.
      // I.e. the storage got new data since we obtained the pointer.
      
#if !defined(RELEASE)
      T *old_pointer = ATOMIC_GET(_old_pointer_to_be_freed);
      if (old_pointer != NULL && old_pointer!=pointer)
        abort();
#endif

      //printf("AI\n");
      ATOMIC_SET(_old_pointer_to_be_freed, pointer);
      
    } else {
      //printf("   ...NOTAI\n");
    }
  }
  
  
public:
  
  AtomicPointerStorage(std::function<void(T*)> free_pointer_function) __attribute__((nonnull))
	  : _free_pointer_function(free_pointer_function)
  {	  
  }
  
  ~AtomicPointerStorage()
  {
	  maybe_free_something(ATOMIC_GET(_pointer), ATOMIC_GET(_old_pointer_to_be_freed));
  }

  void set_new_pointer(T *new_pointer) {
    R_ASSERT(new_pointer != NULL); // NULL not supported. If needed, NULL can be replaced by a ((T*)-1) value or something to indicate an unused slot instead of NULL.
    
    T *old_pointer_to_be_freed = ATOMIC_SET_RETURN_OLD(_old_pointer_to_be_freed, NULL);

    T *old = ATOMIC_SET_RETURN_OLD(_pointer, new_pointer);

    maybe_free_something(old, old_pointer_to_be_freed);
  }

};



// Class to access an AtomicPointerStorage pointer in a realtime thread.
// Only one instance can be used at the same time.
template <typename T>
class RT_AtomicPointerStorage_ScopedUsage{

  AtomicPointerStorage<T> *_storage;
  T *_pointer;

  RT_AtomicPointerStorage_ScopedUsage(const RT_AtomicPointerStorage_ScopedUsage&) = delete;
  RT_AtomicPointerStorage_ScopedUsage& operator=(const RT_AtomicPointerStorage_ScopedUsage&) = delete;

public:

  T *get_pointer(void) const {
    return _pointer;
  }
  
  RT_AtomicPointerStorage_ScopedUsage(AtomicPointerStorage<T> *storage)
    : _storage(storage)
    , _pointer(storage->RT_obtain_pointer())
  {
  }
    
  ~RT_AtomicPointerStorage_ScopedUsage(){
    _storage->RT_release_pointer(_pointer);
  }
};

extern void check_wheather(void);

  
#define MAX_NUM_READERS 128

// Same as AtomicPointerStorage, but this one also supports up to MAX_NUM_READERS readers at the same time.
//
// If using more readers than MAX_NUM_READERS, the code should not produce the wrong result or crash, but the
// RT_AtomicPointerStorage_ScopedUsage constructor might sleep and an assertion will pop up.
//
// Performance should be approximately the same as AtomicPointerStorage, but this one uses a lot more memory (approx. 4k).
// However, most of the memory will never be CPU-fetched, so CPU cache should not be affected.
//
// (AtomicPointerStorageMultipleReaders::set_new_pointer is approx. 128 times slower than AtomicPointerStorage:set_new_pointer, but this should never matter.)
//
template <typename T>
class AtomicPointerStorageMultipleReaders {

  template <typename T2> friend class RT_AtomicPointerStorageMultipleReaders_ScopedUsage;

  // Also holds the actual pointer. It's basically a shared-pointer.
  // (To avoid const-errors, we don't put '_num_references' in T.)
  struct RefCounter
  {
	  T *_t;
	  int _num_references = MAX_NUM_READERS;

	  RefCounter(T *t)
		  : _t(t)
	  {
	  }
  };

  static void dec_references(RefCounter *ref_counter)
  {
	  R_ASSERT_NON_RELEASE(THREADING_is_main_thread()); // If not, '_num_references' must be atomic, which it currently isn't.
	  
	  ref_counter->_num_references--;
	    
	  if (ref_counter->_num_references==0)
	  {
		  delete ref_counter->_t;
		  delete ref_counter;
	  }
  }

  struct AtomicPointerStorage2 : public AtomicPointerStorage<RefCounter>{

	  DEFINE_ATOMIC(bool, _is_used) = false;

	  AtomicPointerStorage2()
		  : AtomicPointerStorage<RefCounter>(dec_references)
	  {
	  }
  };

  AtomicPointerStorage2 _storage[MAX_NUM_READERS];

public:

  AtomicPointerStorageMultipleReaders(void)
  {
#if !defined(RELEASE)
    for(int i = 0 ; i < MAX_NUM_READERS ; i++){
      R_ASSERT_NON_RELEASE(ATOMIC_GET(_storage[i]._is_used)==false);
    }
#endif
  }

  void set_new_pointer(T *new_pointer)
  {
	  R_ASSERT(new_pointer != NULL); // NULL not supported now. If NULL is needed, we can use ((T*)-1) (or something similar) in AtomicPointerStorage to indicate a used slot instead of NULL.

	  R_ASSERT_NON_RELEASE(THREADING_is_main_thread()); //  // If not '_num_references' must be atomic, which it currently isn't.
	  
	  RefCounter *ref_counter = new RefCounter(new_pointer);
	  
	  for(int i=0;i<MAX_NUM_READERS;i++)
	  {
		  //printf("I: %d\n", i);
		  _storage[i].set_new_pointer(ref_counter);
	  }
  }

private:

  AtomicPointerStorage2 &RT_obtain_storage2(void) {
    
    for(int i0=0 ; i0<1024 ; i0++){
      
      for(int i=0;i<MAX_NUM_READERS;i++)
        if (ATOMIC_COMPARE_AND_SET_BOOL(_storage[i]._is_used, false, true))
          return _storage[i];

      if (i0 > 0){
        
        R_ASSERT_NON_RELEASE(false);
        
        if (i0==1)
          RT_message("Error in AtomicPointerStorageMultipleReaders. More than MAX_NUM_READERS storages are used. Trying to sleep.");

        msleep(5); // Sleeping maximum 5-6 seconds (5ms*1023).
      }
    }
    
    int num = rand() % MAX_NUM_READERS;

    if (!ATOMIC_COMPARE_AND_SET_BOOL(_storage[num]._is_used, false, true))
      RT_message("Error in AtomicPointerStorageMultipleReaders. Unable to find a free storage. Returning a used storage instead.");
    
    R_ASSERT(false);
    
    return _storage[num];
  }

  void RT_release_storage(AtomicPointerStorage2 &storage2, RefCounter *ref_counter) {
    R_ASSERT_NON_RELEASE(ATOMIC_GET(storage2._is_used));
    storage2.RT_release_pointer(ref_counter);
    ATOMIC_SET(storage2._is_used, false);
  }

};

#undef MAX_NUM_READERS


// Class to access an AtomicPointerStorageMultipleReaders pointer in a realtime thread.
// Up to MAX_NUM_READERS instances can be used at the same time (preferably a lot less).
template <typename T>
class RT_AtomicPointerStorageMultipleReaders_ScopedUsage{

  AtomicPointerStorageMultipleReaders<T> &_storage;
  struct AtomicPointerStorageMultipleReaders<T>::AtomicPointerStorage2 &_storage2;
  struct AtomicPointerStorageMultipleReaders<T>::RefCounter *_ref_counter;
  T *_pointer;

  RT_AtomicPointerStorageMultipleReaders_ScopedUsage(const RT_AtomicPointerStorageMultipleReaders_ScopedUsage&) = delete;
  RT_AtomicPointerStorageMultipleReaders_ScopedUsage& operator=(const RT_AtomicPointerStorageMultipleReaders_ScopedUsage&) = delete;

public:

  T *get_pointer(void) const {
    return _pointer;
  }
  
  RT_AtomicPointerStorageMultipleReaders_ScopedUsage(AtomicPointerStorageMultipleReaders<T> &storage)
    : _storage(storage)
    , _storage2(storage.RT_obtain_storage2())
    , _ref_counter(_storage2.RT_obtain_pointer())
    , _pointer(_ref_counter->_t)
  {
  }
    
  ~RT_AtomicPointerStorageMultipleReaders_ScopedUsage(){
    _storage.RT_release_storage(_storage2, _ref_counter);
  }
};

  
  
} // namespace radium
