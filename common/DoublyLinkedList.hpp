
#ifndef RADIUM_COMMON_DOUBLYLINKEDLIST_HPP
#define RADIUM_COMMON_DOUBLYLINKEDLIST_HPP

namespace radium{

template <typename T> struct DoublyLinkedList{

  T *_first = NULL;
  T *_last = NULL;

  DoublyLinkedList()
  {    
  }

  void clear(void){
    _first = NULL;
    _last = NULL;
  }

  void validate_list(void) const {
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

      R_ASSERT(element->dll_prev = prev);

      prev = element;
      element = element->dll_next;
    }
  }

  int size(void) const {
    int i = 0;
    T *l = _first;
    while(l!=NULL){
      i++;
      l=l->dll_next;
    }
    return i;
  }
      
  bool in_list(T *element) const {
#if !defined(RELEASE)
    validate_list();
#endif
    T *l = _first;
    while(l!=NULL){
      if (l==element)
        return true;
      l=l->dll_next;
    }

    return false;
  }

  void insert_before(T *existing_element, T *new_element){
#if !defined(RELEASE)
    validate_list();
#endif

    new_element->dll_next = existing_element;
    
    if (existing_element->dll_prev == NULL)
      _first = new_element;
    else
      existing_element->dll_prev->dll_next = new_element;
    
    new_element->dll_prev = existing_element->dll_prev;
    existing_element->dll_prev = new_element;

#if !defined(RELEASE)
    validate_list();
#endif         
  }

  void push_front(T *element){
#if !defined(RELEASE)
    validate_list();
#endif

    if (_first == NULL) {
      _first = element;
      _last = element;
      element->dll_prev = NULL;
      element->dll_next = NULL;
    } else {
      insert_before(_first, element);
    }

#if !defined(RELEASE)
    validate_list();
#endif

  }

  void insert_after(T *existing_element, T *new_element){
#if !defined(RELEASE)
    validate_list();
#endif

    new_element->dll_prev = existing_element;
    
    if (existing_element->dll_next == NULL)
      _last = new_element;
    else
      existing_element->dll_next->dll_prev = new_element;
    
    new_element->dll_next = existing_element->dll_next;
    existing_element->dll_next = new_element;

#if !defined(RELEASE)
    validate_list();
#endif
  }

  void push_back(T *element){
#if !defined(RELEASE)
    validate_list();
#endif

    if (_last == NULL)
      push_front(element);
    else
      insert_after(_last, element);

#if !defined(RELEASE)
    validate_list();
#endif
  }


  void remove(T *element){
#if !defined(RELEASE)
    validate_list();
    int orgsize = size();
    R_ASSERT(in_list(element));
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
    validate_list();
    int newsize = size();
    R_ASSERT(orgsize-newsize == 1);
    R_ASSERT(!in_list(element));
#endif
  }
};


}

#endif
