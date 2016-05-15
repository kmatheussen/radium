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

struct SoundProducer;

#ifdef __cplusplus

#include <stdint.h>

#include "SoundPlugin.h"

#include "../common/Vector.hpp"

#if 0
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
}
#endif


enum SoundProducerRunningState {HASNT_RUN_YET, IS_RUNNING, FINISHED_RUNNING};

int64_t SP_get_id(SoundProducer *producer);
bool SP_add_elink(SoundProducer *target, SoundProducer *source);
bool SP_add_link(SoundProducer *target, int target_ch, SoundProducer *source, int source_ch);
void SP_remove_elink(SoundProducer *target, SoundProducer *source);
void SP_remove_link(SoundProducer *target, int target_ch, SoundProducer *source, int source_ch);
void SP_remove_all_links(radium::Vector<SoundProducer*> &soundproducers);
void SP_RT_called_for_each_soundcard_block(SoundProducer *producer);
void SP_RT_process(SoundProducer *producer, int64_t time, int num_frames, bool process_plugins);
void SP_RT_clean_output(SoundProducer *producer, int num_frames);
void SP_RT_process_bus(float **outputs, int64_t time, int num_frames, int bus_num, bool process_plugins);
void SP_RT_set_bus_descendant_type_for_plugin(SoundProducer *producer);
int SP_get_bus_num(SoundProducer *sp);
enum BusDescendantType SP_get_bus_descendant_type(SoundProducer *sp);
float SP_get_input_peak(SoundProducer *producer, int ch);
float SP_get_output_peak(SoundProducer *producer, int ch);
void SP_set_buffer_size(SoundProducer *producer,int buffer_size);

// Functions below used in multicore processing
double SP_get_running_time(const SoundProducer *sp);
void SP_RT_reset_running_time(SoundProducer *sp);

class QFile;
void SP_write_mixer_tree_to_disk(QFile *file);

#endif // __cplusplus

#ifndef AUDIO_SOUNDPLUGIN_H
struct SoundPlugin;
#endif

extern LANGSPEC struct SoundPlugin *SP_get_plugin(struct SoundProducer *producer);

extern LANGSPEC struct SoundProducer *SP_create(struct SoundPlugin *plugin, Buses buses);
extern LANGSPEC void SP_delete(struct SoundProducer *producer);

extern LANGSPEC struct SoundProducer *SP_get_SoundProducer(struct SoundPlugin *plugin);

extern LANGSPEC bool SP_replace_plugin(struct SoundPlugin *old_plugin, struct SoundPlugin *new_plugin);
extern LANGSPEC bool SP_is_plugin_running(struct SoundPlugin *plugin);
extern LANGSPEC void SP_print_tree(void);

#endif // SOUNDPRODUCER_PROC_H
