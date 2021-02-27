
#ifndef RADIUM_COMMON_DOUBLYLINKEDLIST_HPP
#define RADIUM_COMMON_DOUBLYLINKEDLIST_HPP

#include "LockAsserter.hpp"

namespace radium{

template <typename T> struct DoublyLinkedList{

  T *_first = NULL;
  T *_last = NULL;

#if !defined(RELEASE)
  LockAsserter _lockAsserter;
#endif

public:
  
  DoublyLinkedList()
  {
#if !defined(RELEASE)
    LOCKASSERTER_EXCLUSIVE(&_lockAsserter);
#endif
  }

private:
  
  void clear_p(void){
    _first = NULL;
    _last = NULL;
  }

  void set_p(const DoublyLinkedList<T> &l){    
    _first = l._first;
    _last = l._last;
  }
  
  void validate_list_p(void) const {
    if (_first==NULL)
      R_ASSERT(_last==NULL);
    if (_last==NULL)
      R_ASSERT(_first==NULL);

    if (_first==NULL)
      return;

    T *prev = _first;
    T *element = _first->dll_next;

    while(true){
      R_ASSERT(prev!=NULL);
      R_ASSERT(prev->dll_next == element);

      if (element==NULL){
        R_ASSERT(prev==_last);
        break;
      }

      R_ASSERT(element->dll_prev == prev);

      prev = element;
      element = element->dll_next;
    }
  }

  int size_p(void) const {
    int i = 0;
    T *l = _first;
    while(l!=NULL){
      i++;
      l=l->dll_next;
    }
    return i;
  }
      
  bool in_list_p(T *element) const {
    T *l = _first;
    while(l!=NULL){
      if (l==element)
        return true;
      l=l->dll_next;
    }

    return false;
  }

  void insert_before_p(T *existing_element, T *new_element){
#if !defined(RELEASE)
    validate_list_p();
#endif

    new_element->dll_next = existing_element;
    
    if (existing_element->dll_prev == NULL)
      _first = new_element;
    else
      existing_element->dll_prev->dll_next = new_element;
    
    new_element->dll_prev = existing_element->dll_prev;
    existing_element->dll_prev = new_element;

#if !defined(RELEASE)
    validate_list_p();
#endif         
  }

  void push_front_p(T *element){
#if !defined(RELEASE)
    validate_list_p();
#endif

    if (_first == NULL) {
      _first = element;
      _last = element;
      element->dll_prev = NULL;
      element->dll_next = NULL;
    } else {
      insert_before_p(_first, element);
    }

#if !defined(RELEASE)
    validate_list_p();
#endif

  }

  void insert_after_p(T *existing_element, T *new_element){
#if !defined(RELEASE)
    validate_list_p();
#endif

    new_element->dll_prev = existing_element;
    
    if (existing_element->dll_next == NULL)
      _last = new_element;
    else
      existing_element->dll_next->dll_prev = new_element;
    
    new_element->dll_next = existing_element->dll_next;
    existing_element->dll_next = new_element;

#if !defined(RELEASE)
    validate_list_p();
#endif
  }

  void push_back_p(T *element){
#if !defined(RELEASE)
    validate_list_p();
#endif

    if (_last == NULL)
      push_front_p(element);
    else
      insert_after_p(_last, element);

#if !defined(RELEASE)
    validate_list_p();
#endif
  }


  void remove_p(T *element){
#if !defined(RELEASE)
    validate_list_p();
    int orgsize = size_p();
    R_ASSERT(in_list_p(element));
#endif

    if (element->dll_prev == NULL)
      _first = element->dll_next;
    else
      element->dll_prev->dll_next = element->dll_next;
    
    if (element->dll_next == NULL)
      _last = element->dll_prev;
    else
      element->dll_next->dll_prev = element->dll_prev;

#if !defined(RELEASE)
    validate_list_p();
    int newsize = size_p();
    R_ASSERT(orgsize-newsize == 1);
    R_ASSERT(!in_list_p(element));
#endif
  }

public:

  void clear(void){
#if !defined(RELEASE)
    LOCKASSERTER_EXCLUSIVE(&_lockAsserter);
#endif
    clear_p();
  }

  void set(const DoublyLinkedList<T> &l){    
#if !defined(RELEASE)
    LOCKASSERTER_EXCLUSIVE(&_lockAsserter);
#endif
    set_p(l);
  }
  
  void validate_list(void) const {
#if !defined(RELEASE)
    LOCKASSERTER_SHARED(&_lockAsserter);
#endif
    validate_list_p();
  }
    
  int size(void) const {
#if !defined(RELEASE)
    LOCKASSERTER_SHARED(&_lockAsserter);
#endif
    return size_p();
  }
    
  bool in_list(T *element) const {
#if !defined(RELEASE)
    LOCKASSERTER_SHARED(&_lockAsserter);
#endif
    return in_list_p(element);
  }
    
  void insert_before(T *existing_element, T *new_element){
#if !defined(RELEASE)
    LOCKASSERTER_EXCLUSIVE(&_lockAsserter);
#endif
    insert_before_p(existing_element, new_element);
  }

  void push_front(T *element){
#if !defined(RELEASE)
    LOCKASSERTER_EXCLUSIVE(&_lockAsserter);
#endif
    push_front_p(element);
  }

  void insert_after(T *existing_element, T *new_element){
#if !defined(RELEASE)
    LOCKASSERTER_EXCLUSIVE(&_lockAsserter);
#endif
    insert_after_p(existing_element, new_element);
  }

  void push_back(T *element){
#if !defined(RELEASE)
    LOCKASSERTER_EXCLUSIVE(&_lockAsserter);
#endif
    push_back_p(element);
  }

  void remove(T *element){
#if !defined(RELEASE)
    LOCKASSERTER_EXCLUSIVE(&_lockAsserter);
#endif
    remove_p(element);
  }

};


}

#endif
