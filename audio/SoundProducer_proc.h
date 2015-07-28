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
#include <vector>

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

  
// When common/vector can't be used because it allocates the elements with talloc instead of malloc.
template <typename T> struct Vector{
  
private:
  QAtomicInt num_elements_max;
  QAtomicInt num_elements;
  
public:

  T *elements;

  Vector()
    : num_elements_max(0)
    , num_elements(0)
    , elements(NULL)
  {
  }

  ~Vector(){
    free(elements);
  }

  T operator[](int i){
    R_ASSERT(i>=0);
    R_ASSERT(i<num_elements);
    return elements[i];
  }

  const T* begin(){
    return &elements[0];
  }
  
  const T* end(){
    return &elements[num_elements];
  }

  void clear(void){
    num_elements = 0;
  }

  bool is_empty(void){
    return num_elements==0;
  }

  int size(void){
    return num_elements;
  }

  // This function must _always or never_ be called before calling add. No mixing.
  void ensure_there_is_room_for_one_more_without_having_to_allocate_memory(void){
    int new_num_elements = num_elements+1;
    
    if (new_num_elements > num_elements_max) {
      num_elements_max = new_num_elements;
      elements = (T*) realloc(elements, sizeof(T) * num_elements_max);
    }
  }

  // Only RT safe if ensure_there_is_room_for_one_more_without_having_to_allocate_memory is called first.
  void add(T t){
    num_elements.fetchAndAddOrdered(1);
    
    if (num_elements > num_elements_max) {
      num_elements_max = num_elements;
      elements = (T*) realloc(elements, sizeof(T) * num_elements_max);
    }
    
    elements[num_elements-1] = t;
  }

  // RT safe.
  void remove(T t){
    int pos;
    
    for(pos=0 ; pos<num_elements ; pos++)
      if (elements[pos]==t)
        break;
    
    R_ASSERT(pos < num_elements);

    if (num_elements==1){
      R_ASSERT(pos==0);
      elements[pos] = NULL;
    } else {
      elements[pos] = elements[num_elements-1];
    }

    num_elements.fetchAndAddOrdered(-1);
  }
  
};

}


enum SoundProducerRunningState {HASNT_RUN_YET, IS_RUNNING, FINISHED_RUNNING};

struct SoundProducer;

SoundProducer *SP_create(SoundPlugin *plugin);
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
SoundProducer *SP_get_SoundProducer(SoundPlugin *plugin);
float SP_get_input_peak(SoundProducer *producer, int ch);
float SP_get_output_peak(SoundProducer *producer, int ch);
void SP_set_buffer_size(SoundProducer *producer,int buffer_size);

// Functions below used in multicore processing
double SP_get_running_time(const SoundProducer *sp);
void SP_RT_reset_running_time(SoundProducer *sp);

#endif // __cplusplus

extern LANGSPEC bool SP_replace_plugin(SoundPlugin *old_plugin, SoundPlugin *new_plugin);
extern LANGSPEC bool SP_is_plugin_running(SoundPlugin *plugin);

#endif // SOUNDPRODUCER_PROC_H
