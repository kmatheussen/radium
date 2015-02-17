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



#ifndef AUDIO_SOUNDPLUGIN_H
#define AUDIO_SOUNDPLUGIN_H

#include "../common/hashmap_proc.h"
#include "Smooth_proc.h"

/*
  get_effect_value / set_effect value is screwed up.
  All the horrible code and hacks stems from the bad decision of scaling values between 0 and 1.
  This must be fixed.
 */


#ifdef __cplusplus
extern "C"{
#endif

enum{
  EFFNUM_INPUT_VOLUME = 0,  // This one must be first.
  EFFNUM_INPUT_VOLUME_ONOFF,

  EFFNUM_VOLUME,
  EFFNUM_VOLUME_ONOFF,

  EFFNUM_OUTPUT_VOLUME,
  EFFNUM_OUTPUT_VOLUME_ONOFF,

  EFFNUM_BUS1,
  EFFNUM_BUS1_ONOFF,

  EFFNUM_BUS2,
  EFFNUM_BUS2_ONOFF,

  EFFNUM_PAN,
  EFFNUM_PAN_ONOFF,

  EFFNUM_DRYWET,
  EFFNUM_EFFECTS_ONOFF,

  EFFNUM_LOWPASS_FREQ,
  EFFNUM_LOWPASS_ONOFF,

  EFFNUM_EQ1_FREQ,
  EFFNUM_EQ1_GAIN,
  EFFNUM_EQ1_ONOFF,

  EFFNUM_EQ2_FREQ,
  EFFNUM_EQ2_GAIN,
  EFFNUM_EQ2_ONOFF,

  EFFNUM_LOWSHELF_FREQ,
  EFFNUM_LOWSHELF_GAIN,
  EFFNUM_LOWSHELF_ONOFF,

  EFFNUM_HIGHSHELF_FREQ,
  EFFNUM_HIGHSHELF_GAIN,
  EFFNUM_HIGHSHELF_ONOFF,

  EFFNUM_EQ_SHOW_GUI,

  EFFNUM_COMP_RATIO, // Note that the order for the compressor parameters must be the same as the compressor parameters in system_compresssor_wrapper_proc.h
  EFFNUM_COMP_THRESHOLD,
  EFFNUM_COMP_ATTACK,
  EFFNUM_COMP_RELEASE,
  EFFNUM_COMP_OUTPUT_VOLUME,
  EFFNUM_COMP_ONOFF,

  EFFNUM_COMP_SHOW_GUI,

  EFFNUM_DELAY_TIME,
  EFFNUM_DELAY_ONOFF,

  EFFNUM_BROWSER_SHOW_GUI,
  EFFNUM_CONTROLS_SHOW_GUI,

  NUM_SYSTEM_EFFECTS
};



enum{
  EFFECT_FORMAT_FLOAT,
  EFFECT_FORMAT_INT,
  EFFECT_FORMAT_BOOL,
  EFFECT_FORMAT_RADIO
};

enum ValueFormat{
  PLUGIN_FORMAT_NATIVE,
  PLUGIN_FORMAT_SCALED // scaled between 0 and 1
};

struct SoundPlugin;

#if 0
  // maybe
struct SoundPluginEffect{
  float (*get_effect_display_min_value)(const struct SoundPluginType *plugin_type);
  float (*get_effect_display_max_value)(const struct SoundPluginType *plugin_type);
  int (*get_effect_format)(const struct SoundPluginType *plugin_type); // Must return one of the EFFECT_* values above.

  const char *(*get_effect_name)(const struct SoundPluginType *plugin_type);
  const char *(*get_effect_description)(const struct SoundPluginType *plugin_type);

  void (*get_display_value_string)(struct SoundPlugin *plugin, char *buffer, int buffersize);

  // Returns true if you want to call SP_RT_get_effect_value_array to get effect values for this effect.
  // If not, set_effect_value will be called instead.
  bool (*effect_is_RT)(const struct SoundPluginType *plugin_type);

  // This functions is called if SoundPluginType->effect_is_RT(effect_num) returns false
  void (*set_effect_value)(struct SoundPlugin *plugin, int64_t block_delta_time, float value);

  float (*get_effect_value)(struct SoundPlugin *plugin_type);
};
#endif


struct SoundPluginTypeContainer;

// Note that only the fields 'name' and 'is_instrument' will be accessed before the call to 'create_plugin_data'.
// The 'is_instrument' field will also be re-read after a call to 'create_plugin_data', in case it had the wrong value before.
typedef struct SoundPluginType{
  const char *type_name; // I.e. Ladspa / Vst / FluidSynth / etc. Must be unique.
  const char *name;      // i.e. zita-reverb / low-pass filter / etc. Must be unique within plugins with the same type_name.

  struct SoundPluginTypeContainer *container; // In case it is loaded from a container. (for instance a vst shell plugin)

  const char *info;     // Contains text inside the info box which appear when pressing the button with the name of the plugin. Can be NULL.                               

  int num_inputs;
  int num_outputs;

  bool is_instrument; // Should be set to true if it is not known until instantiation whether it is an instrument.

  bool note_handling_is_RT;

  int num_effects;

  bool plugin_takes_care_of_savable_values; // For instance, if a VST plugin has it's own editor, we ask the plugin for values instead of using savable_effect_values (which contains the last set value). Then this value is true.

  const char *(*get_effect_description)(const struct SoundPluginType *plugin_type, int effect_num);

  void (*get_display_value_string)(struct SoundPlugin *plugin, int effect_num, char *buffer, int buffersize);

  bool (*effect_is_visible)(struct SoundPlugin *plugin, int effect_num);

  // Returns true if you want to call SP_RT_get_effect_value_array to get effect values for this effect.
  // If not, set_effect_value will be called instead.
  bool (*effect_is_RT)(const struct SoundPluginType *plugin_type, int effect_num);

  void *(*create_plugin_data)(const struct SoundPluginType *plugin_type, struct SoundPlugin *plugin, hash_t *state, float sample_rate, int block_size); // Called by Radium during the instantiation of a plugin. The function returns plugin->data. Note that "state" is the same variable that is sent to "recreate_from_state", but this function is called BEFORE the effect values are set. "state" is NULL when the instance is created from nothing, i.e. not loaded from file or undo information.
  void (*cleanup_plugin_data)(struct SoundPlugin *plugin);

  // If set, this callback will be called when the 'num_frames' argument to RT_process changes. The audio thread is suspended while this function is called.
  void (*buffer_size_is_changed)(struct SoundPlugin *plugin, int new_buffer_size);

  // The sound processing function. Note that the inputs and outputs arrays are likely to point to the same sound buffers.
  // For instance, if the plugin has one input and one output, inputs[0] and outputs[0] are very likely to be equal.
  void (*RT_process)(struct SoundPlugin *plugin, int64_t block_delta_time, int num_frames, float **inputs, float **outputs);

  // These two functions are not used if SoundPluginType->note_handling_is_RT is false (currently, these two functions are note used at all)
  /*
  void (*RT_play_note)(struct SoundPlugin *plugin, int64_t block_delta_time, int note_num, float volume, float pan);
  void (*RT_stop_note)(struct SoundPlugin *plugin, int64_t block_delta_time, int note_num, float volume);
  */

  // These three functions are not used if SoundPluginType->note_handling_is_RT is true
  void (*play_note)(struct SoundPlugin *plugin, int64_t block_delta_time, float note_num, int64_t note_id, float volume, float pan);
  void (*set_note_volume)(struct SoundPlugin *plugin, int64_t block_delta_time, float note_num, int64_t note_id, float volume);
  void (*set_note_pitch)(struct SoundPlugin *plugin, int64_t block_delta_time, float note_num, int64_t note_id, float pitch);
  void (*stop_note)(struct SoundPlugin *plugin, int64_t block_delta_time, float note_num, int64_t note_id);
  
  // Returns the number of channels it can provide peaks for. (calling this function with ch=-1 is considered a dummy operation, except that the return value is correct)
  int (*get_peaks)(struct SoundPlugin *plugin, float note_num, int ch, float pan, int64_t start_time, int64_t end_time, float *min_value, float *max_value);

  int (*get_effect_format)(struct SoundPlugin *plugin, int effect_num); // Must return one of the EFFECT_* values above.
  int (*get_effect_num)(struct SoundPlugin *plugin, const char *effect_name); // Necessary to implement this if the order of effects may change in the future.
  const char *(*get_effect_name)(struct SoundPlugin *plugin, int effect_num); // The effect name is used as effect id. Two effects can not have the same name.

  // This functions is called if SoundPluginType->effect_is_RT(effect_num) returns false
  void (*set_effect_value)(struct SoundPlugin *plugin, int64_t block_delta_time, int effect_num, float value, enum ValueFormat value_format, FX_when when);

  float (*get_effect_value)(struct SoundPlugin *plugin, int effect_num, enum ValueFormat value_format);

  void (*show_gui)(struct SoundPlugin *plugin);
  void (*hide_gui)(struct SoundPlugin *plugin);

  void (*recreate_from_state)(struct SoundPlugin *plugin, hash_t *state); // Optional function. Called after plugin has been created. Note that "state" is the same variable that is sent to "recreate_from_state", but this function is called AFTER the effect values have been set.
  void (*create_state)(struct SoundPlugin *plugin, hash_t *state);

  // Free use by the plugin
  void *data;

  // Used by Radium
  int instance_num; // Only used to autocreate a name

} SoundPluginType;

typedef struct SoundPluginTypeContainer{
  const char *type_name;

  const char *name;
  void *data;

  int num_types;
  SoundPluginType **plugin_types;

  bool is_populated;
  void (*populate)(struct SoundPluginTypeContainer *container); // Note: populate might be called even if 'is_populated' is true. (If that happens, just do nothing.)

} SoundPluginTypeContainer;

typedef struct SystemFilter{
  struct SoundPlugin **plugins;
  bool is_on;
  bool was_on;
  bool was_off;
  int fade_pos;
} SystemFilter;

enum BusDescendantType{
  IS_BUS_DESCENDANT,
  IS_NOT_A_BUS_DESCENDANT,
  MAYBE_A_BUS_DESCENDANT,
};

typedef struct SoundPlugin{
  const SoundPluginType *type;

  // Data used by the plugin (the value returned by 'create_plugin_data')
  void *data;

  // Data below handled by Radium.

  //const char *name; // Used to autocreate instance name. Sometime the type_name is not specific enough. (plugin containers). Can be NULL.
  
  struct Patch *patch; // The patch points to the plugin and the plugin points to the patch. However, the patch outlives the plugin. Plugin comes and goes, while the patch stays.
                       // Beware that this value might be NULL.

  float *savable_effect_values; // When dragging a slider, we want to save that value. But we don't want to save the last sent out automation value. (saving to disk, that is)
  float *initial_effect_values; // Used when resetting.

  bool editor_is_on;

  // Data used by SoundProducer
  Smooth input_volume;
  bool input_volume_is_on;

  float volume;
  bool volume_is_on;

  Smooth output_volume;
  bool output_volume_is_on;

  Smooth bus_volume[2];
  bool bus_volume_is_on[2];

  Smooth pan; // between 0 and 1
  bool pan_is_on;

  Smooth drywet;
  bool effects_are_on;

  SystemFilter lowpass;
  float lowpass_freq;

  SystemFilter eq1;
  float eq1_freq;
  float eq1_db;

  SystemFilter eq2;
  float eq2_freq;
  float eq2_db;

  SystemFilter lowshelf;
  float lowshelf_freq;
  float lowshelf_db;

  SystemFilter highshelf;
  float highshelf_freq;
  float highshelf_db;

  SystemFilter delay;
  float delay_time;

  bool show_browser_gui;
  bool show_controls_gui;

  bool show_equalizer_gui;

  SystemFilter comp;
  void *compressor;

  bool show_compressor_gui;

  float *volume_peak_values;
  float *volume_peak_values_for_chip;

  float *output_volume_peak_values;

  float *input_volume_peak_values;
  float *input_volume_peak_values_for_chip;

  float system_volume_peak_values[2]; // The one in the status bar. (Only if this is the system out plugin.) Set in Jack_plugin.c
  float *bus_volume_peak_values[2];

  enum BusDescendantType bus_descendant_type; // Is 'IS_BUS_DESCENDANT' for all descendants of bus plugins. To prevent accidental feedback loops.

} SoundPlugin;


// Call this function to get effects from the realtime process.
// For instance, if there is a volume starting from 0.5, and ending at 1.0, in the current block of 1024 frames,
// then the function will return {0.5, 0.50048828125, 0.5009765625, ..., 1.0}
//
// This functions can only be called if SoundPluginType->effect_is_RT(effect_num) returns true
float *RT_get_effect_value_array(SoundPlugin *plugin, int effect_num);

// RT_get_note_volume_array works the same way, but for note volumes. If several similar note_nums (i.e. same note_num value)
// are playing at the same time, the returned array will contain the values for one of those notes. It is undefined
// which of them.
//
// The functions can only be called if SoundPluginType->note_handling_is_RT is true
float *RT_get_note_volume_array(SoundPlugin *plugin, int note_num);


#ifdef __cplusplus
}
#endif

#endif // AUDIO_SOUNDPLUGIN_H
