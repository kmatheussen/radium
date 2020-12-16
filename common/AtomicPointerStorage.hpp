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
//
// This code is genious in what it does, but it takes some time understanding how it works.
// However, there's lots of assertions, there have never been problems with the code,
// I'm regularly running tsan and asan, and I've looked over the code a few times and
// and concluded that it's correct, so I'm confident it's working properly (Note added 2020-12).
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

  std::function<void(T*)> _free_pointer_function = NULL;
  //void (*_free_pointer_function)(T *);
  
  void maybe_free_something(T *a, T *b){
    if (_free_pointer_function) {
      
      if (a!=NULL)
        _free_pointer_function(a);
      
      if (b!=NULL)
        _free_pointer_function(b);
      
    } else {
      
      if (a!=NULL)
        delete a;
      
      if (b!=NULL)
        delete b;
      
    }
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
  
  AtomicPointerStorage(void (*free_pointer_function)(T *) = NULL)
    : _free_pointer_function(free_pointer_function)
  {
  }
  
  ~AtomicPointerStorage(){
    maybe_free_something(ATOMIC_GET(_pointer), ATOMIC_GET(_old_pointer_to_be_freed));
  }
  

  // May be called at any time. 'free_pointer_function' may be called 0, 1, or 2 times. (usually 1 time)
  void set_new_pointer(T *new_pointer) {
    
    R_ASSERT(new_pointer != NULL); // NULL not supported. If nedaed, NULL can be replaced by a ((T*)-1) or something to indicate a used slot instead of NULL.
    
    T *old_pointer_to_be_freed = ATOMIC_SET_RETURN_OLD(_old_pointer_to_be_freed, NULL);

    T *old = ATOMIC_SET_RETURN_OLD(_pointer, new_pointer);

    //printf("Has set. new: %p, old: %p, old_pointer_to_be_freed: %p\n", new_pointer, old, old_pointer_to_be_freed);
    
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



  
#define MAX_NUM_READERS 128
  
// Same as AtomicPointerStorage, but this one also supports up to MAX_NUM_READERS readers at the same time.
// If using more than MAX_NUM_READERS, the RT_AtomicPointerStorage_ScopedUsage constructor might sleep in the constructor.
//
// Performance should be approximately the same as AtomicPointerStorage.
// However, this one uses uses approx. 4k of memory. (Most of the memory will never be CPU-fetched, so CPU cache should not be affected very much).
//
template <typename T>
class AtomicPointerStorageMultipleReaders {

  template <typename T2> friend class RT_AtomicPointerStorageMultipleReaders_ScopedUsage;

  // It's possible to use std::shared_ptr instead of this class, I think, but I don't know if the std::shared_ptr.get() method is guaranteed to be 100% RT safe.
  struct RefcountT{

    T *_t;

    //std::function<void(T*)> _free_pointer_function;
    
    RefcountT(T *t)
      : _t(t)
    {}
    
    int _num_users = MAX_NUM_READERS;

    void operator delete(void *p) {
      RefcountT *dasthis = static_cast<RefcountT*>(p);

      //printf("        NUM_USERS: %d. Freeing %p\n", dasthis->_num_users, p);
      R_ASSERT_NON_RELEASE(dasthis==dynamic_cast<RefcountT*>(dasthis));
      R_ASSERT_NON_RELEASE(dasthis->_num_users > 0 && dasthis->_num_users <= MAX_NUM_READERS);
      
      dasthis->_num_users--;
      if (dasthis->_num_users==0){

        /*
        if (free_pointer_function != NULL)
          free_pointer_function(dasthis->_t);
        else
        */
        delete dasthis->_t;
        
        ::delete dasthis;
      }      
    }
  };
    
  struct AtomicPointerStorage2 : public AtomicPointerStorage<RefcountT>{  
    DEFINE_ATOMIC(bool, _is_used) = false;
  };

  AtomicPointerStorage2 _storage[MAX_NUM_READERS];

public:

  AtomicPointerStorageMultipleReaders(void /* void (*free_pointer_function)(T *) = NULL */)
  {
#if !defined(RELEASE)
    for(int i = 0 ; i < MAX_NUM_READERS ; i++){
      R_ASSERT_NON_RELEASE(ATOMIC_GET(_storage[i]._is_used)==false);
    }
#endif
  }

  void set_new_pointer(T *new_pointer) {
    R_ASSERT(new_pointer != NULL); // NULL not supported. If nedaed, NULL can be replaced by a ((T*)-1) or something in AtomicPointerStorage to indicate a used slot instead of NULL.
    
    RefcountT *refcount_t = new RefcountT(new_pointer);
    
    for(int i=0;i<MAX_NUM_READERS;i++){
      //printf("I: %d\n", i);
      _storage[i].set_new_pointer(refcount_t);
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

  void RT_release_storage(AtomicPointerStorage2 &storage2, RefcountT *pointer) {
    R_ASSERT_NON_RELEASE(ATOMIC_GET(storage2._is_used));
    storage2.RT_release_pointer(pointer);
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
  struct AtomicPointerStorageMultipleReaders<T>::RefcountT *_refcount_t;
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
    , _refcount_t(_storage2.RT_obtain_pointer())
    , _pointer(_refcount_t->_t)
  {
  }
    
  ~RT_AtomicPointerStorageMultipleReaders_ScopedUsage(){
    _storage.RT_release_storage(_storage2, _refcount_t);
  }
};

  
  
} // namespace radium
