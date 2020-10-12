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


extern bool g_RT_enable_latency_compensation; // Must/should hold player lock when writing.
extern int64_t g_RT_system_out_input_latency; // Must/should hold player lock when writing.


struct SoundProducer;

#ifdef __cplusplus

#include <stdint.h>

#include "SoundPlugin.h"

#include "../common/Vector.hpp"


#if USE_QT4

class Chip;

namespace radium{

enum class EnableType{
  DONT_CHANGE = -1,
  DISABLE = 0,
  ENABLE = 1,
};

static inline EnableType get_enable_type_from_int(int e){
  if(e < 0){
    R_ASSERT_NON_RELEASE(e==-1);
    return EnableType::DONT_CHANGE;
  }else if (e==0)
    return EnableType::DISABLE;
  else{
    R_ASSERT_NON_RELEASE(e==1);
    return EnableType::ENABLE;
  }
}

static inline EnableType get_enable_type_from_bool(bool enable){
  if(enable)
    return EnableType::ENABLE;
  else
    return EnableType::DISABLE;
}
   
struct LinkParameter{
  SoundProducer *source;
  int source_ch;
  SoundProducer *target;
  int target_ch;

  EnableType enable_type = EnableType::ENABLE;

  float volume = 1.0;
  bool must_set_volume = false;

  /*
  // Various attempts to avoid creating the empty constructor.
  LinkParameter(const LinkParameter&) = default;
  LinkParameter(LinkParameter&) = default;
  LinkParameter(LinkParameter *parm)
    : LinkParameter(*parm)
  {}
  LinkParameter(const LinkParameter *parm)
    : LinkParameter(*parm)
  {}
  */
  
  LinkParameter(SoundProducer *source, int source_ch,
                SoundProducer *target, int target_ch,
                float volume = -1.0,
                EnableType enable_type = EnableType::ENABLE)
    : source(source)
    , source_ch(source_ch)
    , target(target)
    , target_ch(target_ch)
    , enable_type(enable_type)
  {
    if(volume >= 0){
      this->volume = volume;
      this->must_set_volume = true;
    }
  }

  // QVector requires an empty constructor in LinkParameters::add
  LinkParameter()
    :LinkParameter(NULL, -1, NULL, -1)
  {}
};

struct LinkParameters : public QVector<LinkParameter> {
  LinkParameters(){
  }
  
  void add(SoundProducer *source, int source_ch, SoundProducer *target, int target_ch, float volume = -1.0, EnableType enable_type = EnableType::ENABLE){
    push_back(LinkParameter(source, source_ch, target, target_ch, volume, enable_type));
  }

  // Implemented in QM_chip.cpp
  void add(Chip *source, int source_ch, Chip *target, int target_ch, float volume = -1.0, EnableType enable_type = EnableType::ENABLE);
};

}

extern const radium::LinkParameters g_empty_linkparameters;

// Either does everything or nothing. Returns false if it did nothing.
bool SP_add_and_remove_links(const radium::LinkParameters &parm_to_add, const radium::LinkParameters &parm_to_remove); // Shows message before returning false and something was wrong.

#endif


enum SoundProducerRunningState {HASNT_RUN_YET, IS_RUNNING, FINISHED_RUNNING};

int64_t SP_get_id(const SoundProducer *producer);
bool SP_add_elink(SoundProducer *target, SoundProducer *source); // returns true if successful. Shows message before it returns false.
bool SP_add_link(SoundProducer *target, int target_ch, SoundProducer *source, int source_ch); // returns true if successful. Shows message before it returns false.
void SP_remove_elink(SoundProducer *target, const SoundProducer *source);
void SP_remove_link(SoundProducer *target, int target_ch, const SoundProducer *source, int source_ch);
void SP_remove_all_links(const radium::Vector<SoundProducer*> &soundproducers); // Note: Removes BOTH audio and event links.
void SP_remove_all_elinks(const radium::Vector<SoundProducer*> &soundproducers); // Only elinks.
void SP_RT_called_for_each_soundcard_block1(SoundProducer *producer, int64_t time);
void SP_RT_called_for_each_soundcard_block2(SoundProducer *producer, int64_t time);
//void SP_RT_clean_output(SoundProducer *producer, int num_frames);
int SP_get_bus_num(const SoundProducer *sp);
//float SP_get_input_peak(SoundProducer *producer, int ch);
//float SP_get_output_peak(SoundProducer *producer, int ch);
void SP_set_buffer_size(SoundProducer *producer,int buffer_size);

// Functions below used in multicore processing
double SP_get_running_time(const SoundProducer *sp);

class QFile;
void SP_write_mixer_tree_to_disk(QFile *file);

#endif // __cplusplus

#ifndef AUDIO_SOUNDPLUGIN_H
struct SoundPlugin;
#endif

extern LANGSPEC float SP_get_link_gain(const struct SoundProducer *target, const struct SoundProducer *source, const char **error); // Don't use this one. Use getAudioConnectionGain instead.
extern LANGSPEC bool SP_set_link_gain(struct SoundProducer *target, struct SoundProducer *source, float volume, const char **error); // Don't use this one. Use setAudioConnectionGain instead. Returns true if gain was changed.

extern LANGSPEC bool SP_call_me_after_solo_has_changed(void);

//extern LANGSPEC bool SP_get_link_enabled(const struct SoundProducer *target, const struct SoundProducer *source, const char **error); // Link enabled/disabled is kept track of in the AudioConnection class.
extern LANGSPEC bool SP_set_link_explicitly_enabled(struct SoundProducer *target, struct SoundProducer *source, bool is_enabled, const char **error); // Only called from AudioConnection->set_enabled().
extern LANGSPEC bool SP_get_link_explicitly_enabled(SoundProducer *target, SoundProducer *source, const char **error);
extern LANGSPEC bool SP_get_link_enabled(SoundProducer *target, SoundProducer *source, const char **error); // get actualy enabled status, which might be different than explitly enabled status if using solo.

#ifdef __cplusplus
float SP_get_actual_link_gain_and_enabled(const SoundProducer *target, const SoundProducer *source, const char **error, bool &is_enabled);
#endif

extern LANGSPEC struct SoundPlugin *SP_get_plugin(const struct SoundProducer *producer);
extern LANGSPEC struct SoundProducer *SP_get_sound_producer(const struct SoundPlugin *plugin);

extern LANGSPEC struct SoundProducer *SP_create(struct SoundPlugin *plugin, Buses buses);
extern LANGSPEC void SP_delete(struct SoundProducer *producer);

extern LANGSPEC int RT_SP_get_input_latency(const struct SoundProducer *sp);

//extern LANGSPEC bool SP_replace_plugin(struct SoundPlugin *old_plugin, struct SoundPlugin *new_plugin);
extern LANGSPEC bool SP_is_plugin_running(const struct SoundPlugin *plugin);
extern LANGSPEC void SP_print_tree(void);

extern LANGSPEC bool SP_mute_because_someone_else_has_solo_left_parenthesis_and_we_dont_right_parenthesis(struct SoundProducer *sp);

extern LANGSPEC int SP_get_max_input_channels_from_audio_input_links(const struct SoundProducer *sp);
extern LANGSPEC int SP_get_max_visible_input_channels_from_audio_input_links(const struct SoundProducer *sp);

extern LANGSPEC bool SP_has_input_links(const struct SoundProducer *sp);
extern LANGSPEC bool SP_has_audio_input_link(const struct SoundProducer *sp);
extern LANGSPEC bool SP_is_audio_connected(const SoundProducer *start_producer, const SoundProducer *end_producer);

extern LANGSPEC void SP_called_regularly_by_main_thread(struct SoundProducer *sp);
  
#endif // SOUNDPRODUCER_PROC_H
