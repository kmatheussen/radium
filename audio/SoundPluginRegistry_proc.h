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

#ifndef RADIUM_AUDIO_SOUNDPLUGIN_REGISTRY_PROC_H
#define RADIUM_AUDIO_SOUNDPLUGIN_REGISTRY_PROC_H

#ifdef __cplusplus
#  define LANGSPEC "C"
#else
#  define LANGSPEC
#endif

#ifdef USE_QT4

#include <QString>
#include <QVector>

#include "../common/Vector.hpp"

struct NumUsedPluginEntry{
  QString container_name;
  QString type_name;
  QString name;
  int num_uses = 0;
};

// This struct is used a lot of places, so easiest thing is to do a "make clean" after changing it.
struct PluginMenuEntry{
  SoundPluginType *plugin_type = NULL;
  SoundPluginTypeContainer *plugin_type_container = NULL;
  radium::FilePath preset_filename;
  QString level_up_name;

  QString separator_name;
  
  NumUsedPluginEntry hepp;

  enum{
    IS_NORMAL = 0,
    IS_CONTAINER,
    IS_SEPARATOR,
    IS_LEVEL_UP,
    IS_LEVEL_DOWN,
    IS_LOAD_PRESET,
    IS_PASTE_PRESET,
    IS_NUM_USED_PLUGIN
  } type;
  
  static const QString type_to_string(int type) {
    switch(type){
    case PluginMenuEntry::IS_NORMAL:
      return "NORMAL";
    case PluginMenuEntry::IS_CONTAINER:
      return "CONTAINER";
    case PluginMenuEntry::IS_SEPARATOR:
      return "SEPARATOR";
    case PluginMenuEntry::IS_LEVEL_UP:
      return "LEVEL_UP";
    case PluginMenuEntry::IS_LEVEL_DOWN:
      return "LEVEL_DOWN";
    case PluginMenuEntry::IS_LOAD_PRESET:
      return "LOAD_PRESET";
    case PluginMenuEntry::IS_PASTE_PRESET:
      return "PASTE_PRESET";
    case PluginMenuEntry::IS_NUM_USED_PLUGIN:
      return "NUM_USED_PLUGIN";
    default:
      R_ASSERT(false);
      return "UNKNOWN_TYPE";
    }
  }

  static const QString type_to_string(const PluginMenuEntry &entry){
    return type_to_string(entry.type);
  }
    
  static PluginMenuEntry separator(QString separator_name = ""){
    PluginMenuEntry entry;
    entry.type=IS_SEPARATOR;
    entry.separator_name = separator_name;
    return entry;
  }
  static PluginMenuEntry load_preset(void){
    PluginMenuEntry entry;
    entry.type=IS_LOAD_PRESET;
    return entry;
  }
  static PluginMenuEntry load_preset(filepath_t filename){
    PluginMenuEntry entry;
    entry.type=IS_LOAD_PRESET;
    entry.preset_filename = radium::FilePath(filename);
    return entry;
  }
  static PluginMenuEntry paste_preset(void){
    PluginMenuEntry entry;
    entry.type=IS_PASTE_PRESET;
    return entry;
  }
  static PluginMenuEntry num_used_plugin(NumUsedPluginEntry hepp){
    PluginMenuEntry entry;
    entry.type=IS_NUM_USED_PLUGIN;
    entry.hepp = hepp;
    return entry;
  }  
  static PluginMenuEntry level_up(QString name){
    PluginMenuEntry entry;
    entry.type=IS_LEVEL_UP;
    entry.level_up_name=name;
    return entry;
  }
  static PluginMenuEntry level_down(void){
    PluginMenuEntry entry;
    entry.type=IS_LEVEL_DOWN;
    return entry;
  }
  static PluginMenuEntry normal(SoundPluginType *plugin_type){
    PluginMenuEntry entry;
    entry.plugin_type = plugin_type;
    entry.type=IS_NORMAL;
    return entry;
  }
  static PluginMenuEntry container(SoundPluginTypeContainer *plugin_type_container){
    PluginMenuEntry entry;
    entry.plugin_type_container = plugin_type_container;
    entry.type=IS_CONTAINER;
    return entry;
  }
};

const QVector<PluginMenuEntry> PR_get_menu_entries(void); // Can not use radium::Vector if the contents requires copy constructor

void PR_remove_last_menu_entry(void);
void PR_add_menu_entry(PluginMenuEntry entry);

#endif // USE_QT4


extern LANGSPEC void PR_set_init_vst_first(void);
extern LANGSPEC void PR_set_init_ladspa_first(void);
extern LANGSPEC bool PR_is_initing_vst_first(void);

extern LANGSPEC void PR_add_load_preset_menu_entries_in_directory(filepath_t dirname);

extern LANGSPEC void PR_add_plugin_type_no_menu(SoundPluginType *plugin_type);
extern LANGSPEC void PR_add_plugin_type(SoundPluginType *plugin_type);
extern LANGSPEC void PR_add_plugin_container(SoundPluginTypeContainer *container);
extern LANGSPEC int PR_get_num_plugin_types(void);
extern LANGSPEC void PR_inc_plugin_usage_number(SoundPluginType *type);
extern LANGSPEC SoundPluginTypeContainer *PR_get_populated_container(const char *container_name, const char *type_name); // Will populate container if it isn't already populated
extern LANGSPEC bool PR_ensure_container_is_populated(const char *container_name, const char *type_name);
extern LANGSPEC SoundPluginType *PR_get_plugin_type_by_name(const char *container_name, const char *type_name, const char *plugin_name);
extern LANGSPEC SoundPluginType *PR_get_plugin_type(int num);
extern LANGSPEC void PR_init_plugin_types(void);


extern LANGSPEC void create_sine_plugin(void);
extern void create_bus_plugins(bool only_pipe);
extern void create_seqtrack_plugin(void);
extern void create_timeskew_plugin(void);
extern void create_patchbay_plugin(void);
extern void create_juce_plugins(void);
extern void create_ladspa_plugins(void);
extern void create_sample_plugin(bool is_click);
extern LANGSPEC void create_fluidsynth_plugin(void);
extern void create_pd_plugin(void);
extern void create_modulator_plugin(void);

extern LANGSPEC void create_midimessages_plugin(void);
  
extern void create_zita_rev_plugin(void);
extern void create_faust_tapiir_plugin(void);
extern void create_faust_multibandcomp_plugin(void);

#ifdef WITH_FAUST_DEV
extern void create_faust_plugin(void);
#endif

extern void create_pitchshift_plugin(void);

extern void create_faust_system_eq_plugin(void);
extern void create_faust_system_tremolo_plugin(void);
extern void create_faust_system_lowpass_plugin(void);
extern void create_faust_system_highpass_plugin(void);
extern void create_faust_system_lowshelf_plugin(void);
extern void create_faust_system_highshelf_plugin(void);
extern void create_faust_system_pitch_plugin(void);
//extern void create_faust_system_delay_plugin(void);

extern void create_stk_bass_plugin(void);
extern void create_stk_bowed_plugin(void);
extern void create_stk_blow_bottle_plugin(void);
extern void create_stk_blow_hole_plugin(void);
extern void create_stk_brass_plugin(void);
extern void create_stk_clarinet_plugin(void);
extern void create_stk_flute_plugin(void);
extern void create_stk_flute_stk_plugin(void);
extern void create_stk_glass_harmonica_plugin(void);
extern void create_stk_harpsi_plugin(void);
extern void create_stk_modal_bar_plugin(void);
extern void create_stk_NLF_eks_plugin(void);
extern void create_stk_NLF_fm_plugin(void);
extern void create_stk_piano_plugin(void);
extern void create_stk_saxophony_plugin(void);
extern void create_stk_sitar_plugin(void);
extern void create_stk_tibetan_bowl_plugin(void);
extern void create_stk_tuned_bar_plugin(void);
extern void create_stk_uni_bar_plugin(void);
extern void create_stk_voice_form_plugin(void);


#endif
