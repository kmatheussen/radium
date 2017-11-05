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

#include <math.h>

#include "../common/hashmap_proc.h"
#include "Smooth_proc.h"



#ifdef __cplusplus
#  include "../common/Vector.hpp"
  struct SoundPluginEffectMidiLearn;
#endif
  

#ifdef __cplusplus
extern "C"{
#endif

#define MIN_AUTOSUSPEND_PEAK 0.00001
  
#define NOTUSED_EFFECT_NAME "NOTUSED"

// Used by the volume sliders and peak meters.
#define MIN_DB -40           // "gain value" = 0.0. "scaled effect value" = 0.0.
#define MIN_DB_THRESHOLD -35 // "gain_value" = 0.01778279410038923. Between MIN_DB and MIN_DB_THRESHOLD, we do linear gain<->db conversion. "scaled effect value" = 0.06666666666666667.
#define MAX_DB 35            // "gain value" = 56.23413251903491. "scaled effect value" = 1.0.
  
#define THRESHOLD_GAIN 0.01778279410038923 // = powf(10, MIN_DB_THRESHOLD / 20.0f);
  
static inline float gain2db(float gain){
  if (gain <= THRESHOLD_GAIN) {

    if(gain<=0.0f)
      return MIN_DB;

    // We need to do linear conversion below MIN_DB_THRESHOLD here in order to convert back and forth between gain2db and db2gain correctly. (that's probably the only reason)
    
    return scale(gain, 0, THRESHOLD_GAIN, MIN_DB, MIN_DB_THRESHOLD);
    
  } else {
    
    return 20*log10(gain);
    
  }
}

static inline float db2gain(float db){
  if (db <= MIN_DB_THRESHOLD){

    if (db <= MIN_DB)
      return 0.0f;
    
    // do linear scale down to zero when db is less than -35 (if not, we won't get 0 gain)
    
    return scale(db, MIN_DB, MIN_DB_THRESHOLD, 0, THRESHOLD_GAIN);
    
  }else{
    
    if (db > MAX_DB)
      db = MAX_DB;
    
    return powf(10, db / 20.0f);
  }
}

  
#define NUM_AB 8
  
enum{
  EFFNUM_INPUT_VOLUME = 0,  // This one must be first.
  EFFNUM_INPUT_VOLUME_ONOFF,

  EFFNUM_SOLO_ONOFF,
  
  EFFNUM_VOLUME,
  EFFNUM_VOLUME_ONOFF,

  EFFNUM_OUTPUT_VOLUME,
  EFFNUM_OUTPUT_VOLUME_ONOFF,

  EFFNUM_BUS1,
  EFFNUM_BUS2,
  EFFNUM_BUS3,
  EFFNUM_BUS4,
  EFFNUM_BUS5,

  EFFNUM_BUS1_ONOFF,
  EFFNUM_BUS2_ONOFF,
  EFFNUM_BUS3_ONOFF,
  EFFNUM_BUS4_ONOFF,
  EFFNUM_BUS5_ONOFF,

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

  EFFNUM_HIGHPASS_FREQ,
  EFFNUM_HIGHPASS_ONOFF,

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

  EFFNUM_VOICE1_ONOFF,
  EFFNUM_VOICE2_ONOFF,
  EFFNUM_VOICE3_ONOFF,
  EFFNUM_VOICE4_ONOFF,
  EFFNUM_VOICE5_ONOFF,
  EFFNUM_VOICE6_ONOFF,
  EFFNUM_VOICE7_ONOFF,
  
  EFFNUM_CHANCE1,
  EFFNUM_CHANCE2,
  EFFNUM_CHANCE3,
  EFFNUM_CHANCE4,
  EFFNUM_CHANCE5,
  EFFNUM_CHANCE6,
  EFFNUM_CHANCE7,
  
  NUM_SYSTEM_EFFECTS
};

#define NUM_BUSES (EFFNUM_BUS1_ONOFF - EFFNUM_BUS1)


enum{
  EFFECT_FORMAT_FLOAT,
  EFFECT_FORMAT_INT,
  EFFECT_FORMAT_BOOL,
  EFFECT_FORMAT_RADIO
};

enum ValueFormat{
  EFFECT_FORMAT_NATIVE,
  EFFECT_FORMAT_SCALED // scaled between 0 and 1
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
  void (*set_effect_value)(struct SoundPlugin *plugin, int block_delta_time, float value);

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

  int version; // Must be increased if either: name of a parameter changes, parameter is added, parameter is removed, parameters changes order, or scaling between native and 0-1 changes (change in max/min native range).

  const char *info;     // Contains text inside the info box which appear when pressing the button with the name of the plugin. Can be NULL.                               

  int num_inputs;
  int num_outputs;

  bool is_instrument; // Should be set to true if it is not known until instantiation whether it is an instrument.

  bool note_handling_is_RT;

  int num_effects;

  //bool plugin_takes_care_of_stored_values; // For instance, if a VST plugin has it's own editor, we ask the plugin for values instead of using stored_effect_values (which contains the last set value). Then this value is true.
  
  bool dont_send_effect_values_from_state_into_plugin; // Can be set to true if all effects are stored in type->create_state. type->effect_value will not be called for all effects then. All effect values are still stored in the state though. In an ideal world, this variable would only have served as an optimization, but it is also a crash fix for buggy plugins ("CM 505").

  bool will_always_autosuspend; // obviously, it doesn't make sense ...
  bool will_never_autosuspend;  // ... if both of these are true

  const char *category;
  const char *creator;
  
  const char *(*get_effect_description)(struct SoundPlugin *plugin, int effect_num);

  void (*get_display_value_string)(struct SoundPlugin *plugin, int effect_num, char *buffer, int buffersize);

  bool (*effect_is_visible)(struct SoundPlugin *plugin, int effect_num);

  // Returns true if you want to call SP_RT_get_effect_value_array to get effect values for this effect.
  // If not, set_effect_value will be called instead.
  bool (*effect_is_RT)(const struct SoundPluginType *plugin_type, int effect_num);

  void *(*create_plugin_data)(const struct SoundPluginType *plugin_type, struct SoundPlugin *plugin, hash_t *state, float sample_rate, int block_size, bool is_loading); // Called by Radium during the instantiation of a plugin. The function returns plugin->data. Note that "state" is the same variable that is sent to "recreate_from_state", but this function is called BEFORE the effect values are set. "state" is NULL when the instance is created from nothing, i.e. not loaded from file or undo information.
  void (*cleanup_plugin_data)(struct SoundPlugin *plugin);

  void (*called_after_plugin_has_been_created)(const struct SoundPluginType *plugin_type, struct SoundPlugin *plugin); // May be NULL
  
  // If set, this callback will be called when the 'num_frames' argument to RT_process changes. The audio thread is suspended while this function is called.
  void (*buffer_size_is_changed)(struct SoundPlugin *plugin, int new_buffer_size);

  // The sound processing function. Note that the inputs and outputs arrays are likely to point to the same sound buffers.
  // For instance, if the plugin has one input and one output, inputs[0] and outputs[0] are very likely to be equal.
  void (*RT_process)(struct SoundPlugin *plugin, int64_t time, int num_frames, float **inputs, float **outputs);

  // These two functions are not used if SoundPluginType->note_handling_is_RT is false (currently, these two functions are note used at all)
  /*
  void (*RT_play_note)(struct SoundPlugin *plugin, int block_delta_time, int note_num, float volume, float pan);
  void (*RT_stop_note)(struct SoundPlugin *plugin, int block_delta_time, int note_num, float volume);
  */

  // These three functions are not used if SoundPluginType->note_handling_is_RT is true
  void (*play_note)(struct SoundPlugin *plugin, int block_delta_time, note_t note);
  void (*set_note_volume)(struct SoundPlugin *plugin, int block_delta_time, note_t note);
  void (*set_note_pitch)(struct SoundPlugin *plugin, int block_delta_time, note_t note);
  void (*send_raw_midi_message)(struct SoundPlugin *plugin, int block_delta_time, uint32_t msg);
  void (*stop_note)(struct SoundPlugin *plugin, int block_delta_time, note_t note);

  void (*player_is_stopped)(struct SoundPlugin *plugin); // Called when player is stopped and after all notes in all instruments have been turned off. May be NULL.
  
  // Used by auto-suspend to determine how long time to wait until suspending. If NULL, the default value will be used instead.
  // * The function can not return a lower value than the actual tail. That may cause an audio tail to suddenly kick in when instrument is brought back from suspension.
  // * The function is allowed to return a higher value than the actual tail.
  // * If the function returns -1, it means that it was unable to report audio tail lenght. (use default/global tail length instead)
  // * If the function returns a value < -1, it means that the tail has infinite length.
  int (*RT_get_audio_tail_length)(struct SoundPlugin *plugin);
  
  int (*RT_get_latency)(struct SoundPlugin *plugin);
  
  // Returns the number of channels it can provide peaks for. (calling this function with ch=-1 is considered a dummy operation, except that the return value is correct)
  int (*get_peaks)(struct SoundPlugin *plugin, float note_num, int ch, float pan, int64_t start_time, int64_t end_time, float *min_value, float *max_value);

  int (*get_effect_format)(struct SoundPlugin *plugin, int effect_num); // Must return one of the EFFECT_* values above.

  const char *(*get_effect_name)(struct SoundPlugin *plugin, int effect_num); // The effect name is used as effect id. Two effects can not have the same name.

  // This functions is called if SoundPluginType->effect_is_RT(effect_num) returns false
  void (*set_effect_value)(struct SoundPlugin *plugin, int block_delta_time, int effect_num, float value, enum ValueFormat value_format, FX_when when);

  float (*get_effect_value)(struct SoundPlugin *plugin, int effect_num, enum ValueFormat value_format);

  bool (*gui_is_visible)(struct SoundPlugin *plugin); // May be NULL
  void (*show_gui)(struct SoundPlugin *plugin); // If NULL, the "GUI" button will not show.
  void (*hide_gui)(struct SoundPlugin *plugin);

  void (*recreate_from_state)(struct SoundPlugin *plugin, hash_t *state, bool is_loading); // Optional function. Called after plugin has been created. Note that "state" is the same variable that is sent to "recreate_from_state", but this function is called AFTER the effect values have been set. It makes sense to to set dont_send_effect_values_from_state_into_plugin==true if 'recreate_from_state' creates and recreates all effects.
  void (*create_state)(struct SoundPlugin *plugin, hash_t *state);

  // Presets (optional)
  int (*get_num_presets)(struct SoundPlugin *plugin);
  int (*get_current_preset)(struct SoundPlugin *plugin);
  void (*set_current_preset)(struct SoundPlugin *plugin, int num);
  const char *(*get_preset_name)(struct SoundPlugin *plugin, int num);
  void (*set_preset_name)(struct SoundPlugin *plugin, int num, const char* new_name);

  void (*set_non_realtime)(struct SoundPlugin *plugin, bool is_non_realtime);

  int num_uses; // Filled in when asking for soundpluginregistry entries
  
  // Free use by the plugin
  void *data;

  // Used by Radium
  int instance_num; // Only used to autocreate a name
} SoundPluginType;

enum PopulateContainerResult{
  POPULATE_RESULT_IS_OKAY = 0,
  POPULATE_RESULT_PLUGIN_MUST_BE_BLACKLISTED = 1,
  POPULATE_RESULT_OTHER_ERROR = 2
};
    
typedef struct SoundPluginTypeContainer{
  const char *type_name;

  const char *name;
  void *data;

  int num_types;
  SoundPluginType **plugin_types;

  bool is_populated;
  enum PopulateContainerResult (*populate)(struct SoundPluginTypeContainer *container);

  wchar_t *filename; // full path to plugin file.
  bool has_saved_disk_entry;
  
  int num_uses; // Filled in when asking for soundpluginregistry entries
} SoundPluginTypeContainer;

typedef struct SystemFilter{
  struct SoundPlugin **plugins;
  DEFINE_ATOMIC(bool, is_on);
  bool was_on;
  bool was_off;
  int fade_pos;
} SystemFilter;

enum BusDescendantType{
  IS_BUS_DESCENDANT,
  IS_BUS_PROVIDER,
  MAYBE_A_BUS_DESCENDANT,
};

enum AutoSuspendBehavior{ // The numbers below can not be changed since they are saved in state.
  DEFAULT_AUTOSUSPEND_BEHAVIOR = 0,
  AUTOSUSPEND_ENABLED = 1,
  AUTOSUSPEND_DISABLED = 2
};

struct SoundProducer;

typedef struct {
  int num_channels;
  DEFINE_ATOMIC(float *, RT_max_gains);   // Gain format. Written to by the realtime thread. Only read from the AudioMeterPeakTreater.  
  float *max_dbs;        // Transfer variable between audio thread and main thread. Contains last max peaks. Read by the gui. Written by the AudioMeterPeakTreater. Db format.
  float *decaying_dbs;    // Db format. Contains decaying peaks. Read by the gui. Written by the AudioMeterPeakTreater.
  float *falloff_dbs;    // Db format. Contains maximum peak values the last x seconds. Read by the gui. Written by the AudioMeterPeakTreater.
  float *peaks;          // Db format. Contains max peaks. Read by the gui. Is set to -100 by the user when clicking on the peak text in the mixer.
} AudioMeterPeaks;

struct SongInitAutomationHelper{
  int64_t abstime;
  enum ValueFormat value_format;
  float value;
  FX_when when;
};
  
typedef struct SoundPlugin{

#ifdef __cplusplus
  SoundPlugin(const SoundPlugin&) = delete;
  SoundPlugin& operator=(const SoundPlugin&) = delete;
#endif
  
  SoundPluginType *type;

  // Data used by the plugin (the value returned by 'create_plugin_data')
  void *data;

  // Data below handled by Radium.

  //const char *name; // Used to autocreate instance name. Sometime the type_name is not specific enough. (plugin containers). Can be NULL.
  
  // TODO: Why was this attribute set volatile? Should it really be atomic?
  volatile struct Patch *patch; // The patch points to the plugin and the plugin points to the patch. However, the patch outlives the plugin. Plugin comes and goes, while the patch stays.
                                // Beware that this value might be NULL.

  // When dragging a slider, we want to save that value. But we don't want to save the last sent out automation value. (saving to disk, that is)
  float *stored_effect_values_native; // native format values
  float *stored_effect_values_scaled; // scaled (0->1) format values
  float *last_written_effect_values_native; // Is set regardless of StoreitType.
  float *last_written_effect_values_scaled; // Is set regardless of StoreitType.
  
  float *initial_effect_values; // Used when resetting. Native format.
  bool *do_random_change;       // Used when selecting "Random".

  struct SongInitAutomationHelper *songinit_automation_helper; // Used when starting to play in the middle of a song.
  
  linked_note_t *playing_voices; // To keep track of voices scheduled into the future because of latency compensation

  bool editor_is_on;

  DEFINE_ATOMIC(bool, solo_is_on);

  struct SoundProducer *sp; // SoundProducer is a helper object for the mixer. It's actually the soundproducer that holds the plugin, and not the other way, but the sp variable is referenced here since we need to access the soundproducer outside of the mixer to get input latency for a plugin and other things. This value is NULL if the plugin is not currently running in the mixer (but use the SP_is_plugin_running function to check for that).
  
  // Data used by SoundProducer
  Smooth input_volume;
  DEFINE_ATOMIC(bool, input_volume_is_on);

  //float volume;
  Smooth volume;
  DEFINE_ATOMIC(bool, volume_is_on);

  float output_volume;
  DEFINE_ATOMIC(bool, output_volume_is_on);

  float bus_volume[NUM_BUSES];
  DEFINE_ATOMIC(bool, bus_volume_is_on)[NUM_BUSES];

  Smooth pan; // between 0 and 1
  DEFINE_ATOMIC(bool, pan_is_on);

  Smooth drywet;
  DEFINE_ATOMIC(bool, effects_are_on);

  SystemFilter lowpass;
  float lowpass_freq;

  SystemFilter highpass;
  float highpass_freq;

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

  void *delay; // a SmoothDelay instance
  float delay_time;
  DEFINE_ATOMIC(bool, delay_is_on);
  
  bool show_browser_gui;
  bool show_controls_gui;

  bool show_equalizer_gui;

  SystemFilter comp;
  void *compressor;

  bool show_compressor_gui;

  float *slider_automation_values; // Only used for visualization. This array holds the automation values shown in the sliders. Scaled format. If value is less than 0, sliders don't show the value.
  
  // peaks
  //
  /*
  float *volume_peak_values;

  float *output_volume_peak_values;
  float *output_volume_peak_values_for_chip;

  float *input_volume_peak_values;
  
  float *bus_volume_peak_values0;
  float *bus_volume_peak_values1;
  float *bus_volume_peak_values2;
  float *bus_volume_peak_values3;
  float *bus_volume_peak_values4;
  */

  AudioMeterPeaks volume_peaks;

  AudioMeterPeaks output_volume_peaks;

  AudioMeterPeaks input_volume_peaks;
  
  AudioMeterPeaks bus0_volume_peaks;
  AudioMeterPeaks bus1_volume_peaks;
  AudioMeterPeaks bus2_volume_peaks;
  AudioMeterPeaks bus3_volume_peaks;
  AudioMeterPeaks bus4_volume_peaks;

  DEFINE_ATOMIC(void *, cpu_usage);
  
  DEFINE_ATOMIC(bool *, is_recording_automation);
  
#ifdef __cplusplus
  radium::Vector<SoundPluginEffectMidiLearn*> *midi_learns;
#else
  void *midi_learns;
#endif

  DEFINE_ATOMIC(bool, is_visible);

  DEFINE_ATOMIC(int, effect_num_to_show_because_it_was_used_externally);

  DEFINE_ATOMIC(bool, auto_suspend_suspended); // Can be set temporarily by plugin
  
  DEFINE_ATOMIC(enum AutoSuspendBehavior, auto_suspend_behavior);
  DEFINE_ATOMIC(int64_t, time_of_last_activity); // used when determining whether to auto-bypass

  int curr_ab_num;
  float *ab_values[NUM_AB]; // each element points to an array of floats. native format.
  hash_t *ab_states[NUM_AB];
  bool ab_is_valid[NUM_AB];

  DEFINE_ATOMIC(bool, enable_sample_seek);
  
  DEFINE_ATOMIC(bool, is_selected);

  DEFINE_ATOMIC(bool, has_initialized);
  DEFINE_ATOMIC(bool, is_shutting_down);
  
} SoundPlugin;


static inline enum ColorNums get_effect_color(const SoundPlugin *plugin, int effect_num){
  const int start = AUTOMATION1_COLOR_NUM;
  const int end = AUTOMATION8_COLOR_NUM;
  const int len = end-start + 1;

  if (effect_num >= plugin->type->num_effects)
    return (enum ColorNums)(start + ((effect_num - plugin->type->num_effects) % len));
  else
    return (enum ColorNums)(start + (effect_num % len));
}

static inline int get_mute_effectnum(const SoundPluginType *type){
  if (type->num_outputs==0)
    return type->num_effects + EFFNUM_INPUT_VOLUME_ONOFF;
  else
    return type->num_effects + EFFNUM_VOLUME_ONOFF;
}

static inline bool is_muted(const SoundPlugin *plugin){
  if (plugin->type->num_outputs==0)
    return !ATOMIC_GET(plugin->input_volume_is_on);
  else
    return !ATOMIC_GET(plugin->volume_is_on);
}

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
