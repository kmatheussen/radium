/* Copyright 2012 Kjetil S. Matheussen

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


#ifndef SOUNDPRODUCER_PROC_H
#define SOUNDPRODUCER_PROC_H

#ifdef __cplusplus

#include <stdint.h>

#include <QAtomicInt>

#include "SoundPlugin.h"


namespace radium{

struct DoublyLinkedList{
  DoublyLinkedList *next;
  DoublyLinkedList *prev;

  DoublyLinkedList()
  : next(NULL)
  , prev(NULL)
  {    
  }

  void add(DoublyLinkedList *l){
    l->next = this->next;
    if(this->next!=NULL)
      this->next->prev = l;
    this->next = l;
    l->prev = NULL;
  }

  void remove(DoublyLinkedList *l){
    if(l->prev!=NULL)
      l->prev->next = l->next;
    else
      this->next = l->next;
    if(l->next!=NULL)
      l->next->prev = l->prev;
  }
};


class LockAsserter{

  QAtomicInt number_of_readers;
  QAtomicInt number_of_writers;

public:
  struct Exclusive {
    LockAsserter *lockAsserter;

    Exclusive(LockAsserter *lockAsserter)
      : lockAsserter(lockAsserter)
    {
      R_ASSERT(int(lockAsserter->number_of_readers)==0);
      R_ASSERT(int(lockAsserter->number_of_writers)==0);
      
      lockAsserter->number_of_writers.ref();
    }
    
    ~Exclusive(){
      lockAsserter->number_of_writers.deref();
    }
  };

  struct Shared {
    LockAsserter *lockAsserter;

    Shared(LockAsserter *lockAsserter)
      : lockAsserter(lockAsserter)
    {
      R_ASSERT(int(lockAsserter->number_of_writers)==0);
      
      lockAsserter->number_of_readers.ref();
    }
    
    ~Shared(){
      lockAsserter->number_of_readers.deref();
    }
  };
};
 


#define LOCKASSERTER_EXCLUSIVE(a) radium::LockAsserter::Exclusive _exclusive___(  const_cast<LockAsserter*>(a)  )
#define LOCKASSERTER_SHARED(a)    radium::LockAsserter::Shared    _shared___(     const_cast<LockAsserter*>(a)  )


// When common/vector can't be used because it allocates the elements with talloc instead of malloc.
template <typename T> struct Vector{
  
private:
  int num_elements_max;
  int num_elements;
  
  T *next_elements;
  int next_num_elements_max;
  
  LockAsserter lockAsserter;
  
  
public:
  
  T *elements;
  
  Vector()
    : num_elements_max(4)
    , num_elements(0)
    , next_elements(NULL)
    , next_num_elements_max(0)
  {
    LOCKASSERTER_EXCLUSIVE(&lockAsserter);

    R_ASSERT(num_elements_max > 0);
    
    elements = (T*)calloc(num_elements_max, sizeof(T));
  }

  ~Vector(){
    LOCKASSERTER_EXCLUSIVE(&lockAsserter);
    
    free(elements);
    elements = NULL; // For debugging
  }

  // This function can be called in parallell with the other const functions (i.e. the non-mutating ones).
  T operator[](int i) const {
    LOCKASSERTER_SHARED(&lockAsserter);

    R_ASSERT(i>=0);
    R_ASSERT(i<num_elements);
    
    return elements[i];
  }

  // This function can be called in parallell with the other const functions (i.e. the non-mutating ones).
  const T* begin() const {
    LOCKASSERTER_SHARED(&lockAsserter);
    
    return &elements[0];
  }

  // This function can be called in parallell with the other const functions (i.e. the non-mutating ones).
  const T* end() const {
    LOCKASSERTER_SHARED(&lockAsserter);
    
    return &elements[num_elements];
  }

  // This function can be called in parallell with the other const functions (i.e. the non-mutating ones).
  bool is_empty(void) const {
    LOCKASSERTER_SHARED(&lockAsserter);
    
    return num_elements == 0;
  }

  // This function can be called in parallell with the other const functions (i.e. the non-mutating ones).
  int size(void) const {
    LOCKASSERTER_SHARED(&lockAsserter);
    
    return num_elements;
  }

  // NOT RT safe
  //
  // This function must _always or never_ be called before calling add. No mixing.
  //
  // This function CAN be called in parallell with the const functions (i.e. the non-mutating ones),
  // but it can not be called in parallel with itself or any other non-const/mutating function.
  // (it is not asserted that this function is not called in parallell with itself)
  void ensure_there_is_room_for_one_more_without_having_to_allocate_memory(void){
    LOCKASSERTER_SHARED(&lockAsserter);
    
    R_ASSERT(next_elements == NULL);
    
    int new_num_elements = num_elements+1;

    R_ASSERT(num_elements_max > 0);
      
    if (new_num_elements > num_elements_max) {

      next_num_elements_max = num_elements_max;
      
      while (new_num_elements > next_num_elements_max)
        next_num_elements_max *= 2;

      next_elements = (T*) calloc(sizeof(T), next_num_elements_max);
      memcpy(next_elements, elements, sizeof(T)*num_elements);      
    }
  }

private:
    
    void basic_add(T t){
      num_elements++;

      R_ASSERT(num_elements_max > 0);

      if (num_elements > num_elements_max) {        
        while (num_elements > num_elements_max)
          num_elements_max *= 2;

        elements = (T*) realloc(elements, sizeof(T) * num_elements_max);
      }
      
      elements[num_elements-1] = t;
    }

public:
  
  // Only RT safe if ensure_there_is_room_for_one_more_without_having_to_allocate_memory is called first.
  //
  // This function can NOT be called in parallell with other functions
  void add(T t){
    LOCKASSERTER_EXCLUSIVE(&lockAsserter);
    
    if (next_elements == NULL) {

      basic_add(t);
      
    } else {

      num_elements++;

      R_ASSERT(num_elements <= next_num_elements_max);
      
      free(elements);
      
      elements = next_elements;
      num_elements_max = next_num_elements_max;
      
      next_elements = NULL;
      next_num_elements_max = 0;

      elements[num_elements-1] = t;
    }
  }

  // NOT RT safe
  //
  // This function can NOT be called in parallell with other functions
  void append(Vector<T> *ts){
    LOCKASSERTER_EXCLUSIVE(&lockAsserter);
    
    R_ASSERT(next_elements == NULL);
    
    for (T t : *ts)
      basic_add(t);
  }

  // NOT RT safe
  //
  // This function can NOT be called in parallell with other functions
  void append(Vector<T> &ts){
    LOCKASSERTER_EXCLUSIVE(&lockAsserter);
    
    R_ASSERT(next_elements == NULL);
    
    for (T t : ts)
      basic_add(t);
  }
  
  // RT safe (except for the O(n) performance)
  //
  // This function can NOT be called in parallell with other functions
  void remove(T t){
    LOCKASSERTER_EXCLUSIVE(&lockAsserter);
    
    R_ASSERT(next_elements == NULL);
    
    int pos;
    
    for(pos=0 ; pos<num_elements ; pos++)
      if (elements[pos]==t)
        break;
    
    R_ASSERT(pos < num_elements);

    if (num_elements==1){
      R_ASSERT(pos==0);
      elements[pos] = (T)NULL;
    } else {
      elements[pos] = elements[num_elements-1];
    }

    num_elements--;
  }

  // RT safe
  //
  // This function can NOT be called in parallell with other functions
  void clear(void) {
    LOCKASSERTER_EXCLUSIVE(&lockAsserter);
    
    R_ASSERT(next_elements == NULL);
    num_elements = 0;
  }


};

}


enum SoundProducerRunningState {HASNT_RUN_YET, IS_RUNNING, FINISHED_RUNNING};

struct SoundProducer;

SoundProducer *SP_create(SoundPlugin *plugin, Buses buses);
void SP_delete(SoundProducer *producer);
bool SP_add_elink(SoundProducer *target, SoundProducer *source);
bool SP_add_link(SoundProducer *target, int target_ch, SoundProducer *source, int source_ch);
void SP_remove_elink(SoundProducer *target, SoundProducer *source);
void SP_remove_link(SoundProducer *target, int target_ch, SoundProducer *source, int source_ch);
void SP_remove_all_links(std::vector<SoundProducer*> soundproducers);
void SP_RT_called_for_each_soundcard_block(SoundProducer *producer);
void SP_RT_process(SoundProducer *producer, int64_t time, int num_frames, bool process_plugins);
void SP_RT_clean_output(SoundProducer *producer, int num_frames);
void SP_RT_process_bus(float **outputs, int64_t time, int num_frames, int bus_num, bool process_plugins);
void SP_RT_set_bus_descendant_type_for_plugin(SoundProducer *producer);
struct SoundPlugin *SP_get_plugin(SoundProducer *producer);
int SP_get_bus_num(SoundProducer *sp);
enum BusDescendantType SP_get_bus_descendant_type(SoundProducer *sp);
SoundProducer *SP_get_SoundProducer(SoundPlugin *plugin);
float SP_get_input_peak(SoundProducer *producer, int ch);
float SP_get_output_peak(SoundProducer *producer, int ch);
void SP_set_buffer_size(SoundProducer *producer,int buffer_size);

// Functions below used in multicore processing
double SP_get_running_time(const SoundProducer *sp);
void SP_RT_reset_running_time(SoundProducer *sp);

class QFile;
void SP_write_mixer_tree_to_disk(QFile *file);

#endif // __cplusplus

extern LANGSPEC bool SP_replace_plugin(SoundPlugin *old_plugin, SoundPlugin *new_plugin);
extern LANGSPEC bool SP_is_plugin_running(SoundPlugin *plugin);

#endif // SOUNDPRODUCER_PROC_H
