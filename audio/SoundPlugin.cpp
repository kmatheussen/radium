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



#include <string.h>
#include <math.h>
#include <ctype.h>

#include <boost/version.hpp>
#if (BOOST_VERSION < 100000) || ((BOOST_VERSION / 100 % 1000) < 58)
  #error "Boost too old. Need at least 1.58.\n Quick fix: cd $HOME ; wget http://downloads.sourceforge.net/project/boost/boost/1.63.0/boost_1_63_0.tar.bz2 ; tar xvjf boost_1_63_0.tar.bz2 (that's it!)"
#endif
#include <boost/lockfree/queue.hpp>

#include <QDir>
#include <QFileInfo>

#include "../common/nsmtracker.h"
#include "../common/hashmap_proc.h"
#include "../common/vector_proc.h"
#include "../common/undo.h"
#include "../common/OS_Player_proc.h"
#include "../common/visual_proc.h"
//#include "../common/undo_patchlist_proc.h"
#include "../common/player_proc.h"
#include "../common/patch_proc.h"
#include "../common/threading.h"
#include "../common/disk.h"
#include "../common/sequencer_proc.h"

#include "../midi/midi_i_input_proc.h"

#include "../mixergui/QM_chip.h"
#include "../mixergui/QM_MixerWidget.h"
#include "../mixergui/undo_mixer_proc.h"
#include "../Qt/helpers.h"
#include "../Qt/Qt_instruments_proc.h"
#include "../Qt/undo_instruments_widget_proc.h"

#include "SoundPlugin.h"
#include "AudioMeterPeaks_proc.h"
#include "Mixer_proc.h"
#include "SoundPluginRegistry_proc.h"
#include "SoundProducer_proc.h"
#include "undo_audio_effect_proc.h"
#include "undo_plugin_state_proc.h"
#include "system_compressor_wrapper_proc.h"
#include "audio_instrument_proc.h"
#include "CpuUsage.hpp"
#include "SmoothDelay.hpp"
#include "Modulator_plugin_proc.h"
#include "SendReceive_plugins_proc.h"

#include "../embedded_scheme/s7extra_proc.h"

#include "../api/api_gui_proc.h"


#include "SoundPlugin_proc.h"



#define FILTER_MIN_DB -35
#define FILTER_MAX_DB 35

#define MIN_FREQ 50
#define MAX_FREQ 20000

#define DELAY_MIN 0.0f
#define DELAY_MAX 50.0f

#define PITCH_MIN -12
#define PITCH_MAX 12

static bool g_apply_solo_immediately = false;


namespace radium{
  
struct SoundPluginEffectMidiLearn final : MidiLearn {
    
  SoundPlugin *plugin;
  int effect_num;

  SoundPluginEffectMidiLearn(const SoundPluginEffectMidiLearn&) = delete;
  SoundPluginEffectMidiLearn& operator=(const SoundPluginEffectMidiLearn&) = delete;
    
  SoundPluginEffectMidiLearn(SoundPlugin *plugin, int effect_num)
    : plugin(plugin)
    , effect_num(effect_num)
  {
  }

  SoundPluginEffectMidiLearn(SoundPlugin *plugin, hash_t *state)
    : plugin(plugin)
  {
    init_from_state(state);
  }

  /*
    virtual ~SoundPluginEffectMidiLearn(){
    printf("  SoundPluginEffectMidiLearn destructor called\n");
    }
  */
    
  hash_t *create_state(void) override {
    hash_t *state = MidiLearn::create_state();
    HASH_put_int(state, "SoundPluginEffectMidiLearn::effect_num", effect_num);
    return state;
  }

  void init_from_state(hash_t *state) override {
    MidiLearn::init_from_state(state);
    effect_num = HASH_get_int32(state, "SoundPluginEffectMidiLearn::effect_num");
  }

  instrument_t RT_get_instrument_id(void) override {
    struct Patch *patch = plugin->patch;
    if (patch==NULL)
      return make_instrument(-2);
    else
      return patch->id;
  }
  
  QString get_dest_info(void) override;
  void delete_me(void) override;
  void RT_callback(float val) override;
  
  bool RT_get_automation_recording_data(SoundPlugin **plugin, int *effect_num) override {

    if (PLUGIN_is_recording_automation(this->plugin, this->effect_num)==false)
      return false;
                   
    *plugin = this->plugin;
    *effect_num = this->effect_num;

    return true;
  }
};
}

static void add_midi_learn(radium::SoundPluginEffectMidiLearn *midi_learn){
  midi_learn->plugin->midi_learns->push_back(midi_learn);
  MIDI_add_midi_learn(midi_learn);
}  



/*
(define (slider_2_gain val db_min db_max)
  (let* ((threshold_val  0.05)
         (threshold_gain (pow 10
                             (/ (scale threshold_val 
                                       0 1 
                                       db_min db_max)
                                20.0)))
         (gain   (pow 10 
                    (/ (scale val
                              0 1
                              db_min db_max)
                       20.0))))
    (if (< gain threshold_gain)
        (scale val
               0 threshold_val
               0 threshold_gain)
        gain)))
*/

#define threshold_val 0.05f

static float slider_2_gain(float val, const float db_min, const float db_max){
  if(val<=0.0f)
    return 0.0f;

  if(val <= threshold_val){ // Below threshold, we do a linear conversion. If not there will be a jump between 0 and almost 0.
    // threshold_db one should normally be precalculated, I hope, since db_min and db_max are constants from the caller.
    const float threshold_db  = powf(10,
                                     scale(threshold_val, 0.0f, 1.0f, db_min, db_max) / 20.0f);
    
    return scale(val, 
                 0.0f, threshold_val,
                 0.0f, threshold_db);
  }else{
    return powf(10,
                scale(val, 0.0f, 1.0f, db_min, db_max) / 20.0f);
  }
}



/*
(define (gain_2_slider gain db_min db_max)
  (let* ((threshold_val  0.05)
         (threshold_db  (pow 10
                             (/ (scale threshold_val 
                                       0 1 
                                       db_min db_max)
                                20.0))))
    (if (< gain threshold_db)
        (scale gain
               0 threshold_db
               0 threshold_val)
        (scale (* 20
                  (log10 gain))
               db_min db_max
               0 1))))
*/

static float gain_2_slider(float gain, const float db_min, const float db_max){
  if(gain<=0.0f)
    return 0.0f;

  // This one should normally be precalculated, I hope, since db_min and db_max are constants from the caller.
  const float threshold_gain = powf(10,
                                    scale(threshold_val, 0.0f, 1.0f, db_min, db_max) / 20.0f);
  
  float ret;
  
  if(gain <= threshold_gain) // Below threshold, we do a linear conversion. If not there will be a jump between 0 and almost 0.
    ret = scale(gain, 
                 0.0f, threshold_val,
                 0.0f, threshold_gain);
  else
    ret = scale(20 * log10(gain),
                db_min, db_max,
                0.0f, 1.0f);

  return R_BOUNDARIES(0.0f, ret, 1.0f);
}

#undef threshold_val


static float gain_2_db(float val, const float db_min, const float db_max){
  float slider = gain_2_slider(val,
                               MIN_DB, MAX_DB);
  return scale(slider,0.0f,1.0f,db_min,db_max);
}


/*
;; The output is between 0 and 1
(define (frequency-to-slider freq min max)
  (let* ((min-output (/ (log min)
                        (log max))))
    (scale (/ (log freq) (log max))
           min-output 1.0
           0.0 1.0)))

;; val is between 0 and 1.
(define (slider-to-frequency slider min max)
  (let* ((min-output (/ (log min)
                        (log max))))
    (pow max (scale slider 0 1 min-output 1))))
*/

static float frequency_2_slider(float freq, const float min_freq, const float max_freq){
  const float min_output = logf(min_freq)/logf(max_freq);
  return R_BOUNDARIES(0.0f,
                      scale( logf(freq)/logf(max_freq),
                             min_output, 1.0,
                             0.0, 1.0),
                      1.0f);
}

static float slider_2_frequency(float slider, const float min_freq, const float max_freq){
  const float min_output = logf(min_freq)/logf(max_freq);
  return powf(max_freq, scale(slider,
                              0,1,
                              min_output, 1));
}


const char *system_effect_names[NUM_SYSTEM_EFFECTS] = {
  "System In",
  "System In On/Off",

  "System Solo On/Off",
  
  "System Volume",
  "System Volume On/Off",

  "System Out",
  "System Out On/Off",

  "System Reverb",
  "System Chorus",
  "System Aux 1",
  "System Aux 2",
  "System Aux 3",
  
  "System Pan",
  "System Pan On/Off",

  "System Dry/Wet",
  "System Effects On/Off",

  "System Lp F.",
  "System Lowpass On/Off",

  "System Eq1 F.",
  "System Eq1 L.",
  "System Eq1 On/Off",

  "System Eq2 F.",
  "System Eq2 L.",
  "System Eq2 On/Off",

  "System Ls F.",
  "System Ls L.",
  "System Ls On/Off",

  "System Hs F.",
  "System Hs L.",
  "System Hs On/Off",

  "System Hp F.",
  "System Highpass On/Off",

  "System Show Equalizer GUI",

  "System Compression Ratio",
  "System Compression Threshold",
  "System Compression Attack",
  "System Compression Release",
  "System Compression Makeup Gain",
  "System Compression On/Off",

  "System Show Compressor GUI",

  "System Width",
  "System Width On/Off",

  "System Sample Browser On/Off",
  "System Controls On/Off",

  "System On/Off Voice 1",
  "System On/Off Voice 2",
  "System On/Off Voice 3",
  "System On/Off Voice 4",
  "System On/Off Voice 5",
  "System On/Off Voice 6",
  "System On/Off Voice 7",
  
  "System Transpose Voice 1",
  "System Transpose Voice 2",
  "System Transpose Voice 3",
  "System Transpose Voice 4",
  "System Transpose Voice 5",
  "System Transpose Voice 6",
  "System Transpose Voice 7",
  
  "System Volume Voice 1",
  "System Volume Voice 2",
  "System Volume Voice 3",
  "System Volume Voice 4",
  "System Volume Voice 5",
  "System Volume Voice 6",
  "System Volume Voice 7",

  "System Start Voice 1",
  "System Start Voice 2",
  "System Start Voice 3",
  "System Start Voice 4",
  "System Start Voice 5",
  "System Start Voice 6",
  "System Start Voice 7",

  "System Length Voice 1",
  "System Length Voice 2",
  "System Length Voice 3",
  "System Length Voice 4",
  "System Length Voice 5",
  "System Length Voice 6",
  "System Length Voice 7",
  
  "System Pan Voice 1",
  "System Pan Voice 2",
  "System Pan Voice 3",
  "System Pan Voice 4",
  "System Pan Voice 5",
  "System Pan Voice 6",
  "System Pan Voice 7",
  
  "System Chance Voice 1",
  "System Chance Voice 2",
  "System Chance Voice 3",
  "System Chance Voice 4",
  "System Chance Voice 5",
  "System Chance Voice 6",
  "System Chance Voice 7"  
};

static void init_system_filter(SystemFilter *filter, int num_channels, const char *name, bool is_loading){
  int ch;
  filter->plugins=(SoundPlugin**)V_calloc(sizeof(SoundPlugin*),num_channels);
  for(ch=0;ch<num_channels;ch++){
    filter->plugins[ch] = (SoundPlugin*)V_calloc(1, sizeof(SoundPlugin));
    filter->plugins[ch]->type = PR_get_plugin_type_by_name(NULL, "Faust",name);
    filter->plugins[ch]->data = filter->plugins[ch]->type->create_plugin_data(filter->plugins[ch]->type, filter->plugins[ch], NULL, MIXER_get_sample_rate(), MIXER_get_buffer_size(), is_loading);
    filter->was_off = true;
    filter->was_on = false;
  }
}

static void release_system_filter(SystemFilter *filter, int num_channels){
  int ch;
  for(ch=0;ch<num_channels;ch++){
    filter->plugins[ch]->type->cleanup_plugin_data(filter->plugins[ch]);
    V_free(filter->plugins[ch]);
  }
  V_free(filter->plugins);
}

static void reset_gui_parentgui(SoundPlugin *plugin);

SoundPlugin *PLUGIN_create(struct Patch *patch, SoundPluginType *plugin_type, hash_t *plugin_state, bool is_loading){
  printf("PLUGIN_create called\n");
  
  SoundPlugin *plugin = (SoundPlugin*)V_calloc(1,sizeof(SoundPlugin));
  plugin->patch = patch;
  plugin->type = plugin_type;

  plugin->num_visible_outputs = -1;
  
  reset_gui_parentgui(plugin);
  
  ATOMIC_SET(plugin->auto_suspend_behavior, DEFAULT_AUTOSUSPEND_BEHAVIOR);
  PLUGIN_touch(plugin);
    
  ATOMIC_SET(plugin->effect_num_to_show_because_it_was_used_externally, -1);

  ATOMIC_SET(plugin->enable_sample_seek, enableSampleSeekByDefault());
  
  plugin->is_dpi_aware = true;
  
  plugin->RT_input_latency_manifests_into_output_latency = true;

  int buffer_size = MIXER_get_buffer_size();

  plugin->midi_learns = new radium::Vector<radium::SoundPluginEffectMidiLearn*>;

  // We can't do this any later since plugin_type->num_effects may be set here.
  // Use plugin->has_initialized to avoid running code that operates on plugin data that might be called while calling create_plugin_data.
  plugin->data = plugin_type->create_plugin_data(plugin_type, plugin, plugin_state, MIXER_get_sample_rate(), buffer_size, is_loading);
  if(plugin->data==NULL){
    V_free(plugin);
    return NULL;
  }

  for(int i=0;i<NUM_AB;i++){
    plugin->ab_values[i] = (float*) V_calloc(sizeof(float), plugin_type->num_effects+NUM_SYSTEM_EFFECTS);
    plugin->ab_states[i] = (hash_t*)add_gc_root(HASH_create(5));
  }
  
  ATOMIC_NAME(plugin->is_recording_automation) = (bool*) V_calloc(sizeof(bool), plugin_type->num_effects+NUM_SYSTEM_EFFECTS); // plugin_type->num_effects might be set after calling create_plugin_data.

  // peak and automation pointers (for displaying in the sliders)
  plugin->volume_peaks = AUDIOMETERPEAKS_create(plugin_type->num_outputs);

  plugin->output_volume_peaks = AUDIOMETERPEAKS_create(plugin_type->num_outputs);

  if (plugin_type->num_inputs==0)
    plugin->input_volume_peaks = AUDIOMETERPEAKS_create(plugin_type->num_outputs);
  else
    plugin->input_volume_peaks = AUDIOMETERPEAKS_create(plugin_type->num_inputs);
  
  plugin->bus0_volume_peaks = AUDIOMETERPEAKS_create(2);
  plugin->bus1_volume_peaks = AUDIOMETERPEAKS_create(2);
  plugin->bus2_volume_peaks = AUDIOMETERPEAKS_create(2);
  plugin->bus3_volume_peaks = AUDIOMETERPEAKS_create(2);
  plugin->bus4_volume_peaks = AUDIOMETERPEAKS_create(2);

  plugin->slider_automation_values = (float*)V_calloc(sizeof(float),plugin_type->num_effects+NUM_SYSTEM_EFFECTS);
  for(int e = 0 ; e<plugin_type->num_effects+NUM_SYSTEM_EFFECTS ; e++)
    plugin->slider_automation_values[e] = -10;

  
  SMOOTH_init(&plugin->input_volume  , 1.0f, buffer_size);
  plugin->output_volume = 1.0f;
  SMOOTH_init(&plugin->pan           , 0.5f, buffer_size);
  SMOOTH_init(&plugin->drywet        , 1.0f, buffer_size);
  SMOOTH_init(&plugin->volume        , 1.0f, buffer_size);

  ATOMIC_SET(plugin->input_volume_is_on, true);
  ATOMIC_SET(plugin->output_volume_is_on, true);
  
  //for(int i=0;i<5;i++)
  //  ATOMIC_SET_ARRAY(plugin->bus_volume_is_on, i, true);

  ATOMIC_SET(plugin->effects_are_on, true);
             
  //plugin->volume = 1.0f;
  ATOMIC_SET(plugin->volume_is_on, true);

  plugin->preset_filename = createIllegalFilepath();
  
  {
    int num_outputs=plugin_type->num_outputs;

    init_system_filter(&plugin->lowpass, num_outputs, "System Lowpass", is_loading);
    plugin->lowpass_freq = 5000.0f;

    init_system_filter(&plugin->highpass, num_outputs, "System Highpass", is_loading);
    plugin->highpass_freq = 200.0f;

    init_system_filter(&plugin->eq1, num_outputs, "System Eq", is_loading);
    plugin->eq1_freq = 400.0f;
    plugin->eq1_db = 0.0f;

    init_system_filter(&plugin->eq2, num_outputs, "System Eq", is_loading);
    plugin->eq2_freq = 1000.0f;
    plugin->eq2_db = 0.0f;

    init_system_filter(&plugin->lowshelf, num_outputs, "System Lowshelf", is_loading);
    plugin->lowshelf_freq = 400.0f;
    plugin->lowshelf_db = 0.0f;

    init_system_filter(&plugin->highshelf, num_outputs, "System Highshelf", is_loading);
    plugin->highshelf_freq = 1500.0f;
    plugin->highshelf_db = 0.0f;

    if (num_outputs > 0)
      plugin->delay = new radium::SmoothDelay(ms_to_frames(DELAY_MAX));
    //init_system_filter(&plugin->delay, num_outputs, "System Delay", is_loading);
    plugin->delay_time = 0.0f;
  }

  {
    plugin->compressor = COMPRESSOR_create(MIXER_get_sample_rate());
    ATOMIC_SET(plugin->comp.is_on, false);
    plugin->comp.was_off = true;
    plugin->comp.was_on = false;
  }

  plugin->show_browser_gui = true;
  plugin->show_controls_gui = true;
  plugin->show_equalizer_gui = true;
  plugin->show_compressor_gui = false;

  // Set up stored_effect_values, last_written_effect_values, and initial_effect_values
  {
    int i;
    plugin->stored_effect_values_native = (float*)V_calloc(sizeof(float),plugin_type->num_effects+NUM_SYSTEM_EFFECTS);
    plugin->stored_effect_values_scaled = (float*)V_calloc(sizeof(float),plugin_type->num_effects+NUM_SYSTEM_EFFECTS);
    plugin->last_written_effect_values_native = (float*)V_calloc(sizeof(float),plugin_type->num_effects+NUM_SYSTEM_EFFECTS);
    plugin->last_written_effect_values_scaled = (float*)V_calloc(sizeof(float),plugin_type->num_effects+NUM_SYSTEM_EFFECTS);
    plugin->initial_effect_values_native=(float*)V_calloc(sizeof(float),plugin_type->num_effects+NUM_SYSTEM_EFFECTS);
    plugin->initial_effect_values_scaled=(float*)V_calloc(sizeof(float),plugin_type->num_effects+NUM_SYSTEM_EFFECTS);
    
#if 0
    for(i=0;i<plugin_type->num_effects+NUM_SYSTEM_EFFECTS;i++)
      plugin->stored_effect_values[i] = PLUGIN_get_effect_value(plugin,i,VALUE_FROM_PLUGIN);
#else
    for(i=0;i<plugin_type->num_effects+NUM_SYSTEM_EFFECTS;i++){
      plugin->stored_effect_values_native[i] = PLUGIN_get_effect_value2(plugin, i, VALUE_FROM_PLUGIN, EFFECT_FORMAT_NATIVE);
      plugin->stored_effect_values_scaled[i] = PLUGIN_get_effect_value2(plugin, i, VALUE_FROM_PLUGIN, EFFECT_FORMAT_SCALED);
      
      plugin->last_written_effect_values_native[i]  = plugin->stored_effect_values_native[i];
      plugin->last_written_effect_values_scaled[i]  = plugin->stored_effect_values_scaled[i];
      plugin->initial_effect_values_native[i]       = plugin->stored_effect_values_native[i];
      plugin->initial_effect_values_scaled[i]       = plugin->stored_effect_values_scaled[i];
    }

    // ??
    //plugin->stored_effect_values[plugin_type->num_effects+EFFNUM_OUTPUT_VOLUME] = 1.0f;

    // Set system effects. (why?)
    /*
    for(i=plugin_type->num_effects;i<plugin_type->num_effects+NUM_SYSTEM_EFFECTS;i++){
      float value = PLUGIN_get_effect_value(plugin,i,VALUE_FROM_PLUGIN);
      PLUGIN_set_native_effect_value(plugin, 0, i, value, PLUGIN_NONSTORED_TYPE, STORE_VALUE, FX_single);
    }
    */
    
#endif
  }

  {
    plugin->do_random_change = (bool*)V_calloc(sizeof(bool), plugin_type->num_effects+NUM_SYSTEM_EFFECTS);
    for(int i=0;i<plugin_type->num_effects;i++)
      plugin->do_random_change[i] = true;
  }

  if (plugin_type->called_after_plugin_has_been_created != NULL)    
    plugin_type->called_after_plugin_has_been_created(plugin_type, plugin);

  PLUGIN_touch(plugin);

  plugin->has_initialized = true;
  ATOMIC_SET(plugin->MT_has_initialized, true);
  
  return plugin;
}

void PLUGIN_delete(SoundPlugin *plugin){
  ATOMIC_SET(plugin->is_shutting_down, true);
  
  PLUGIN_touch(plugin);
  
  const SoundPluginType *plugin_type = plugin->type;

  while(PLUGIN_remove_midi_learn(plugin, -1, false)==true);
       
  plugin_type->cleanup_plugin_data(plugin);


  //if(!strcmp(plugin_type->type_name,"Bus")) // RT_process needs buses to always be alive. No, not any more. Now, buses are just like other soundproducers in the mixer.
  //  return;

  
  V_free(plugin->do_random_change);
  V_free(plugin->initial_effect_values_native);
  V_free(plugin->initial_effect_values_scaled);
  V_free(plugin->last_written_effect_values_native);
  V_free(plugin->last_written_effect_values_scaled);
  V_free(plugin->stored_effect_values_native);
  V_free(plugin->stored_effect_values_scaled);

  
  SMOOTH_release(&plugin->input_volume);
    
  SMOOTH_release(&plugin->pan);
  
  SMOOTH_release(&plugin->drywet);
  SMOOTH_release(&plugin->volume);
  
  release_system_filter(&plugin->lowpass, plugin_type->num_outputs);
  
  release_system_filter(&plugin->highpass, plugin_type->num_outputs);
  
  release_system_filter(&plugin->eq1, plugin_type->num_outputs);
  
  release_system_filter(&plugin->eq2, plugin_type->num_outputs);
  
  release_system_filter(&plugin->lowshelf, plugin_type->num_outputs);
    
  release_system_filter(&plugin->highshelf, plugin_type->num_outputs);

  delete static_cast<radium::SmoothDelay*>(plugin->delay);
  //release_system_filter(&plugin->delay, plugin_type->num_outputs);

  COMPRESSOR_delete(plugin->compressor);
  
  // peak and automation pointers (for displaying in the sliders)
  V_free(plugin->slider_automation_values);

  AUDIOMETERPEAKS_delete(plugin->volume_peaks);

  AUDIOMETERPEAKS_delete(plugin->output_volume_peaks);

  AUDIOMETERPEAKS_delete(plugin->input_volume_peaks);
  
  AUDIOMETERPEAKS_delete(plugin->bus0_volume_peaks);
  AUDIOMETERPEAKS_delete(plugin->bus1_volume_peaks);
  AUDIOMETERPEAKS_delete(plugin->bus2_volume_peaks);
  AUDIOMETERPEAKS_delete(plugin->bus3_volume_peaks);
  AUDIOMETERPEAKS_delete(plugin->bus4_volume_peaks);

  CpuUsage_delete(ATOMIC_GET(plugin->cpu_usage));

  V_free(ATOMIC_NAME(plugin->is_recording_automation));
  ATOMIC_SET(plugin->is_recording_automation, NULL);

  for(int i=0;i<NUM_AB;i++){
    V_free(plugin->ab_values[i]);
    remove_gc_root(plugin->ab_states[i]);
  }

  
  R_ASSERT(plugin->midi_learns->size()==0);
  delete plugin->midi_learns;
    
  //memset(plugin,-1,sizeof(SoundPlugin)); // for debugging. Crashes faster if something is wrong.
  V_free(plugin);
}

// Called at the start of each block
void PLUGIN_update_smooth_values(SoundPlugin *plugin){
  SMOOTH_called_per_block(&plugin->input_volume);
  SMOOTH_called_per_block(&plugin->pan);
  SMOOTH_called_per_block(&plugin->drywet);
  SMOOTH_called_per_block(&plugin->volume);
}

int PLUGIN_get_num_visible_effects(SoundPlugin *plugin){
  const SoundPluginType *plugin_type = plugin->type;
  int i;
  int result=0;

  if(plugin_type->effect_is_visible != NULL) {

    for(i=0;i<plugin_type->num_effects;i++)
      result += plugin_type->effect_is_visible(plugin, i) ? 1 : 0;

  }else{

    return plugin_type->num_effects;
    
  }
  
  return result;
}

int PLUGIN_get_effect_format(struct SoundPlugin *plugin, int effect_num){
  const struct SoundPluginType *plugin_type = plugin->type;

  if(effect_num<plugin_type->num_effects)
    return plugin_type->get_effect_format(plugin, effect_num);

  int system_effect = effect_num - plugin_type->num_effects;

  RError("The program isn't supposed to be here");

  if(system_effect==EFFNUM_PAN_ONOFF || system_effect==EFFNUM_EFFECTS_ONOFF || system_effect==EFFNUM_LOWPASS_ONOFF || system_effect==EFFNUM_HIGHPASS_ONOFF || system_effect==EFFNUM_EQ1_ONOFF || system_effect==EFFNUM_EQ2_ONOFF)
    return EFFECT_FORMAT_BOOL;
  else
    return EFFECT_FORMAT_FLOAT;
}

/*
static char *substring(char *s,int start,int end){
  char *ret       = talloc_atomic(1,end-start+1);
  int   read_pos  = start;
  int   write_pos = 0;

  while(read_pos<end)
    ret[write_pos++] = s[read_pos++];

  return ret;
}
*/

static int num_digits_in_the_beginning_of_string(const char *string){
  if (string==NULL)
    return 0;
  if (string[0]==0)
    return 0;
  if (isdigit(string[0]))
    return 1 + num_digits_in_the_beginning_of_string(string+1);
  else
    return 0;
}

static const char *get_effect_name_without_digit_prefix(const char *name){
  int len = (int)strlen(name);
  int num_digits = num_digits_in_the_beginning_of_string(name);
  //printf("   num_digits: %d, len: %d, name[num_digits]: -%c-, -%s- -%s-\n",num_digits,len,name[num_digits],name+num_digits+2, effect_name);
  if (num_digits>0 && len>num_digits+1)
    if (name[num_digits]==':')
      return name+num_digits+2;
  
  return NULL;
}

static const char *get_new_name_if_name_has_changed2(struct SoundPlugin *plugin, const char *effect_name){
  const struct SoundPluginType *plugin_type = plugin->type;
  
  int i;
  for(i=0;i<NUM_SYSTEM_EFFECTS;i++)
    if(!strcmp(system_effect_names[i],effect_name))
      return effect_name;

  for(i=0;i<plugin_type->num_effects;i++)
    if(!strcmp(effect_name,plugin_type->get_effect_name(plugin,i)))
      return effect_name;

  // System effects that have changed name.
  if (!strcmp(effect_name, "Gain (dB)"))
    return system_effect_names[EFFNUM_VOLUME];
      
  // See if this is a vst or ladspa effect that was saved before the <n>: prefix was added.
  for(i=0;i<plugin_type->num_effects;i++) {
    const char *name = plugin_type->get_effect_name(plugin,i);
    const char *name_without = get_effect_name_without_digit_prefix(name);
    if (name_without!=NULL && !strcmp(name_without, effect_name))
      return name;
  }

  return NULL;
}
  
// General function that must be called after loading an effect. Returns the new name if the name has changed since the song was saved.
const char *PLUGIN_get_new_name_if_name_has_changed(struct SoundPlugin *plugin, const char *effect_name){

  {
    const char *name = get_new_name_if_name_has_changed2(plugin, effect_name);
    if (name!=NULL)
      return name;
  }
      
  // Check if this is a foreign effect that was saved with the instrument name appended inside paranthesis, e.g. "Pitch (Goat sound 5)". (we don't do that anymore)
  QString name = effect_name;
  if (name.endsWith(")")){
    int index = name.lastIndexOf("(");
    if (index > 0){
      name = name.left(index-1);
      printf("\n\n\n   Cut name: -%s-\n\n",name.toUtf8().constData());
      const char *name2 = get_new_name_if_name_has_changed2(plugin, talloc_strdup(name.toUtf8().constData()));
      if (name2!=NULL)
        return strdup(name2); // Guess we could use talloc_strdup here, but I'm not sure, and the memory leak doesn't matter.
    }
  }
  
  
  
#ifndef RELEASE
  RWarning("\n\n\n   1. ************ WARNING! Effect \"%s\" not found in plugin %s/%s ************\n\n\n",effect_name,plugin->type->type_name,plugin->type->name);
#endif
  
  return effect_name;
}

int PLUGIN_get_effect_num(struct SoundPlugin *plugin, const char *effect_name, char **error_message){
  const struct SoundPluginType *plugin_type = plugin->type;

  int i;
  for(i=0;i<NUM_SYSTEM_EFFECTS;i++)
    if(!strcmp(system_effect_names[i],effect_name))
      return i + plugin_type->num_effects;

  for(i=0;i<plugin_type->num_effects;i++)
    if(!strcmp(effect_name,plugin_type->get_effect_name(plugin,i)))
      return i;

  char *message = talloc_format("The effect names of the instrument %s / %s have changed.\n\"%s\" will be ignored.\n\nIf you know the new name of the effect, you can edit the song manually in a text editor.", plugin_type->type_name, plugin_type->name, effect_name);

  if (error_message==NULL)
    GFX_Message(NULL, "%s", message);
  else
    *error_message = message;
  
  return -1;
}

const char *PLUGIN_get_effect_name(SoundPlugin *plugin, int effect_num){
  const struct SoundPluginType *plugin_type = plugin->type;

  R_ASSERT_RETURN_IF_FALSE2(effect_num >= 0,"error1");
  R_ASSERT_RETURN_IF_FALSE2(effect_num < plugin_type->num_effects + NUM_SYSTEM_EFFECTS,"error2");
  
  if(effect_num<plugin_type->num_effects)
    return plugin_type->get_effect_name(plugin, effect_num);

  int system_effect = effect_num - plugin_type->num_effects;
  return system_effect_names[system_effect];
}

/*
const char *PLUGIN_get_effect_description(const struct SoundPluginType *plugin_type, int effect_num){
  if(effect_num<plugin_type->num_effects)
    return plugin_type->get_effect_description(plugin_type, effect_num);

  return "System effects have no description yet.";
}
*/

static void set_db_display2(char *buffer, int buffersize, float value){
  float db = gain_2_db(value,MIN_DB,MAX_DB);
  
  if(equal_floats(db, MIN_DB))
    snprintf(buffer,buffersize-1,"-inf dB");
  else if (db>-0.01 && db<0.01)
    snprintf(buffer,buffersize-1,"0.00 dB");
  else
    snprintf(buffer,buffersize-1,"%s%.2f dB", db<0.0f?"":"+", db);
}

static void set_freq_display(char *buffer, int buffersize, float freq){
  snprintf(buffer,buffersize-1,"%.1f Hz",freq);
}

// It's not necessary to implement for all EFFNUM_* values. Can probably remove some code.
//void PLUGIN_get_display_value_string(struct SoundPlugin *plugin, int effect_num, bool use_stored_value, float value, char *buffer, int buffersize){
void PLUGIN_get_display_value_string(struct SoundPlugin *plugin, int effect_num, char *buffer, int buffersize){
  if(effect_num<plugin->type->num_effects)
    return plugin->type->get_display_value_string(plugin, effect_num, buffer, buffersize); // TODO: Fix this. We want to display slider value, not automation value. (actually, we probably want to display both though.)

  float store_value = safe_float_read(&plugin->stored_effect_values_native[effect_num]);
    
  int system_effect = effect_num - plugin->type->num_effects;

  switch(system_effect){
    case EFFNUM_INPUT_VOLUME:
    case EFFNUM_VOLUME:
    case EFFNUM_OUTPUT_VOLUME:
      
    case EFFNUM_BUS1:
    case EFFNUM_BUS2:
    case EFFNUM_BUS3:
    case EFFNUM_BUS4:
    case EFFNUM_BUS5:
      set_db_display2(buffer,buffersize,store_value);
      break;
      
    case EFFNUM_EQ1_GAIN:
    case EFFNUM_EQ2_GAIN:
    case EFFNUM_LOWSHELF_GAIN:
    case EFFNUM_HIGHSHELF_GAIN:
      snprintf(buffer,buffersize-1,"%s%.2f dB",store_value<0.0f?"":"+",store_value);
      break;

    case EFFNUM_PAN:
      snprintf(buffer,buffersize-1,"%d %s",(int)scale(store_value,0,1,-90,90),"\u00B0");
      break;
    case EFFNUM_PAN_ONOFF:
      snprintf(buffer,buffersize-1,"%s",ATOMIC_GET(plugin->pan_is_on)==true?"ON":"OFF");
      break;
      
    case EFFNUM_LOWPASS_FREQ:
    case EFFNUM_HIGHPASS_FREQ:
    case EFFNUM_EQ1_FREQ:
    case EFFNUM_EQ2_FREQ:
    case EFFNUM_LOWSHELF_FREQ:
    case EFFNUM_HIGHSHELF_FREQ:
      set_freq_display(buffer, buffersize, store_value);
      break;

    case EFFNUM_DELAY_TIME:
      snprintf(buffer,buffersize-1,"%.2f ms",store_value);
      break;

    case EFFNUM_DRYWET:
      {
        int wet = store_value*100;
        int dry = 100-wet;
        snprintf(buffer,buffersize-1,"Dry: %d%%. Wet: %d%%",dry,wet);
      }
      break;
      
    case EFFNUM_EFFECTS_ONOFF:
      snprintf(buffer,buffersize-1,"%s",store_value >= 0.5 ?"ON":"OFF");
      break;

    default:
      RError("1. Unknown effect number: %d (%d). %s / %s",effect_num,system_effect,plugin->type->type_name,plugin->type->name);
  }
}


// For the four set_* functions below, we sometimes do unncessary calculations if storeit_type==DONT_STORE_VALUE
// (unless the C++ compiler is smart enough to figure out things, which could be).
//
// However, we only need to care about CPU when set_effect_value is called when doing automation.
// And for automation, value_format==EFFECT_FORMAT_SCALED, and therefore we need to calculate the native format anyway,
// hence there is no point optimizing these functions.
//
static void set_gain_store_value(float &native_value, float &scaled_value, enum ValueFormat value_format){
  if(value_format==EFFECT_FORMAT_NATIVE)
    scaled_value = gain_2_slider(native_value, MIN_DB, MAX_DB);
  else
    native_value = slider_2_gain(scaled_value, MIN_DB, MAX_DB);
}

static float get_gain_value(float native_value, enum ValueFormat value_format){
  if(value_format==EFFECT_FORMAT_NATIVE)
    return native_value;
  else
    return gain_2_slider(native_value, MIN_DB, MAX_DB);
}

static void set_filter_gain_store_value(float &native_value, float &scaled_value, enum ValueFormat value_format){
  if(value_format==EFFECT_FORMAT_NATIVE)  
    scaled_value = scale(native_value, FILTER_MIN_DB, FILTER_MAX_DB, 0, 1);
  else
    native_value = scale(scaled_value, 0, 1, FILTER_MIN_DB, FILTER_MAX_DB);
}

static float get_filter_gain_value(float native_value, enum ValueFormat value_format){
  if(value_format==EFFECT_FORMAT_NATIVE)
    return native_value;
  else
    return scale(native_value, FILTER_MIN_DB, FILTER_MAX_DB, 0, 1);
}

static void set_freq_store_value(float &native_value, float &scaled_value, enum ValueFormat value_format){
  if(value_format==EFFECT_FORMAT_NATIVE)
    scaled_value = frequency_2_slider(native_value,MIN_FREQ,MAX_FREQ);
  else
    native_value = slider_2_frequency(scaled_value,MIN_FREQ,MAX_FREQ);
}

static float get_freq_value(float native_value, enum ValueFormat value_format){
  if (value_format==EFFECT_FORMAT_NATIVE)
    return native_value;
  else
    return frequency_2_slider(native_value, MIN_FREQ, MAX_FREQ);
}

static void set_delay_store_value(float &native_value, float &scaled_value, enum ValueFormat value_format){
  if(value_format==EFFECT_FORMAT_NATIVE)
    scaled_value = scale(native_value, DELAY_MIN, DELAY_MAX, 0, 1);
  else
    native_value = scale(scaled_value, 0, 1, DELAY_MIN, DELAY_MAX);
}

static float get_delay_value(float native_value, enum ValueFormat value_format){
  if(value_format==EFFECT_FORMAT_NATIVE)
    return native_value;
  else
    return scale(native_value, DELAY_MIN, DELAY_MAX, 0, 1);
}


static void update_instrument_gui(struct Patch *patch){
  if (patch != NULL)
    GFX_ScheduleInstrumentRedraw(patch);
  else{
    R_ASSERT_NON_RELEASE(false);
  }
}

static void update_instrument_gui(struct SoundPlugin *plugin){
  struct Patch *patch = (struct Patch*)plugin->patch;
  if (patch != NULL)
    update_instrument_gui(patch);
  else{
    //R_ASSERT_NON_RELEASE(false); // happens when creating plugin.
  }
}

namespace{
  struct Voice{
    struct PatchVoice _default_voice = {};
    struct PatchVoice *_voice;
    
    Voice(const struct SoundPlugin *plugin, int num){
      if (plugin->patch==NULL) {

        PATCHVOICE_set_defaults(&_default_voice, num);
        _voice = &_default_voice;
        
      } else {
        
        _voice = &plugin->patch->voices[num];
        
      }
    }
  };
}
                   
static void set_voice_ONOFF(struct SoundPlugin *plugin, int num, float &native_value, float &scaled_value, enum ValueFormat value_format){
  if (plugin->patch != NULL) {
    bool new_val = native_value>=0.5;
    
    if (new_val != plugin->patch->voices[num].is_on){
      plugin->patch->voices[num].is_on = new_val;
      //printf("      Setting voice %d to %d\n", num, plugin->patch->voices[num].is_on );
      update_instrument_gui(plugin);
    }
  }
}

static float get_voice_ONOFF(struct SoundPlugin *plugin, int num, enum ValueFormat value_format){
  const Voice voice(plugin, num);

  return voice._voice->is_on ? 1.0 : 0.0;
}

static void set_voice_value(struct SoundPlugin *plugin, float *voice_value, float min_native, float max_native, float &native_value, float &scaled_value, enum ValueFormat value_format){
  
  if(value_format==EFFECT_FORMAT_NATIVE)
    scaled_value = R_BOUNDARIES(0, scale(native_value, min_native, max_native, 0, 1), 1);
  else
    native_value = scale(scaled_value, 0, 1, min_native, max_native);

  native_value = R_BOUNDARIES(min_native, native_value, max_native);
 
  if (voice_value != NULL) {
    safe_float_write(voice_value, native_value);
    update_instrument_gui(plugin);
    if (plugin->type->get_peaks != NULL)
      GFX_ScheduleEditorRedrawIfPatchIsCurrentlyVisible(const_cast<Patch*>(plugin->patch));
  }
}

static float get_voice_value(const float voice_value, float min_native, float max_native, enum ValueFormat value_format){
  if (value_format==EFFECT_FORMAT_NATIVE)
    return voice_value;
  else
    return R_BOUNDARIES(0, scale(voice_value, min_native, max_native, 0, 1), 1);
}

static void set_voice_TRANSPOSE(struct SoundPlugin *plugin, int num, float &native_value, float &scaled_value, enum ValueFormat value_format){
  struct Patch *patch = const_cast<struct Patch*>(plugin->patch);
  
  set_voice_value(plugin,
                  patch==NULL ? NULL : &patch->voices[num].transpose,
                  -100, 100,
                  native_value, scaled_value, value_format);

  if(patch != NULL)
    RT_PATCH_voice_pitch_has_changed(patch, num);
}
                      
static float get_voice_TRANSPOSE(struct SoundPlugin *plugin, int num, enum ValueFormat value_format){
  const Voice voice(plugin, num);

  return get_voice_value(voice._voice->transpose, -100, 100, value_format);
}

static void set_voice_VOLUME(struct SoundPlugin *plugin, int num, float &native_value, float &scaled_value, enum ValueFormat value_format){
  struct Patch *patch = const_cast<struct Patch*>(plugin->patch);
  
  set_voice_value(plugin,
                  patch==NULL ? NULL : &patch->voices[num].volume,
                  MIN_PATCHVOICE_VOLUME, MAX_PATCHVOICE_VOLUME,
                  native_value, scaled_value, value_format);
  if(patch != NULL)
    RT_PATCH_voice_volume_has_changed(patch, num);
}
                      
static float get_voice_VOLUME(struct SoundPlugin *plugin, int num, enum ValueFormat value_format){
  const Voice voice(plugin, num);

  return get_voice_value(voice._voice->volume, MIN_PATCHVOICE_VOLUME, MAX_PATCHVOICE_VOLUME, value_format);
}

static void set_voice_START(struct SoundPlugin *plugin, int num, float &native_value, float &scaled_value, enum ValueFormat value_format){
  set_voice_value(plugin,
                  plugin->patch==NULL ? NULL : &plugin->patch->voices[num].start,
                  0, 1000,
                  native_value, scaled_value, value_format);
}
                      
static float get_voice_START(struct SoundPlugin *plugin, int num, enum ValueFormat value_format){
  const Voice voice(plugin, num);

  return get_voice_value(voice._voice->start, 0, 1000, value_format);
}

static void set_voice_LENGTH(struct SoundPlugin *plugin, int num, float &native_value, float &scaled_value, enum ValueFormat value_format){
  set_voice_value(plugin,
                  plugin->patch==NULL ? NULL : &plugin->patch->voices[num].length,
                  0, 1000,
                  native_value, scaled_value, value_format);
}
                      
static float get_voice_LENGTH(struct SoundPlugin *plugin, int num, enum ValueFormat value_format){
  const Voice voice(plugin, num);

  return get_voice_value(voice._voice->length, 0, 1000, value_format);
}

static void set_voice_PAN(struct SoundPlugin *plugin, int num, float &native_value, float &scaled_value, enum ValueFormat value_format){
  struct Patch *patch = const_cast<struct Patch*>(plugin->patch);
  
  set_voice_value(plugin,
                  patch==NULL ? NULL : &patch->voices[num].pan,
                  MIN_PATCHVOICE_PAN, MAX_PATCHVOICE_PAN,
                  native_value, scaled_value, value_format);

  if(patch != NULL)
    RT_PATCH_voice_pan_has_changed(patch, num);
}
                      
static float get_voice_PAN(const struct SoundPlugin *plugin, int num, enum ValueFormat value_format){
  const Voice voice(plugin, num);

  return get_voice_value(voice._voice->pan, MIN_PATCHVOICE_PAN, MAX_PATCHVOICE_PAN, value_format);
}

static void set_voice_CHANCE(struct SoundPlugin *plugin, int num, float &native_value, float &scaled_value, enum ValueFormat value_format){
  if(value_format==EFFECT_FORMAT_NATIVE)
    scaled_value = scale_double(native_value, MIN_PATCHVOICE_CHANCE, MAX_PATCHVOICE_CHANCE, 0, 1);
  else if (scaled_value >= 1)
    native_value = MAX_PATCHVOICE_CHANCE;
  else
    native_value = round(scale_double(scaled_value, 0, 1, MIN_PATCHVOICE_CHANCE, MAX_PATCHVOICE_CHANCE));

  native_value = R_BOUNDARIES(MIN_PATCHVOICE_CHANCE, native_value, MAX_PATCHVOICE_CHANCE);
             
  if (plugin->patch != NULL) {

    safe_float_write(&plugin->patch->voices[num].chance, native_value);
    
    //printf("3. Chance %d: %f\n", num, plugin->patch->voices[num].chance);
    update_instrument_gui(plugin);
  }
}
                      
static float get_voice_CHANCE(struct SoundPlugin *plugin, int num, enum ValueFormat value_format){
  const Voice voice(plugin, num);

  return get_voice_value(voice._voice->chance, MIN_PATCHVOICE_CHANCE, MAX_PATCHVOICE_CHANCE, value_format);
}


#define SET_ATOMIC_ON_OFF(on_off, value) { \
    {                                                                   \
      bool old_val = ATOMIC_GET(on_off);                                \
      if(value>=0.5f && !old_val){                                      \
        ATOMIC_SET(on_off, true);                                       \
        update_instrument_gui(plugin);                                  \
      }else if (value<0.5f && old_val){                                 \
        ATOMIC_SET(on_off, false);                                      \
        update_instrument_gui(plugin);                                  \
      }                                                                 \
    }                                                                   \
  }

#define SET_ATOMIC_ON_OFF2(on_off, value, var, on_value, off_value) {   \
    {                                                                   \
      bool old_val = ATOMIC_GET(on_off);                                \
      if(value>=0.5f && !old_val){                                      \
        var = on_value;                                                 \
        ATOMIC_SET(on_off, true);                                       \
        update_instrument_gui(plugin);                                  \
      }else if (value<0.5f && old_val){                                 \
        ATOMIC_SET(on_off, false);                                      \
        var = off_value;                                                \
        update_instrument_gui(plugin);                                  \
      }                                                                 \
    }                                                                   \
  }

#define SET_SMOOTH_ON_OFF(smooth, on_off, value, on_value, off_value) { \
    {                                                                   \
      bool old_val = ATOMIC_GET(on_off);                                \
      if(value>=0.5f && !old_val){                                      \
        SMOOTH_set_target_value(smooth, on_value);                      \
        ATOMIC_SET(on_off, true);                                       \
        update_instrument_gui(plugin);                                  \
      }else if (value<0.5f && old_val){                                 \
        ATOMIC_SET(on_off, false);                                      \
        SMOOTH_set_target_value(smooth, off_value);                     \
        update_instrument_gui(plugin);                                  \
      }                                                                 \
    }                                                                   \
  }

#define SET_BUS_VOLUME(busnum)                                          \
  set_gain_store_value(store_value_native, store_value_scaled, value_format); \
  {                                                                     \
    float old_value = safe_float_read(&plugin->bus_volume[busnum]);       \
    if(!equal_floats(old_value, store_value_native)){                   \
      safe_float_write(&plugin->bus_volume[busnum], store_value_native); \
      struct Patch *patch = plugin->patch;                     \
      update_instrument_gui(const_cast<struct Patch*>(patch));          \
    }                                                                   \
  }                                                                     \
  break;

//RT_schedule_mixer_strips_redraw();

#define GET_BUS_VOLUME(busnum, value_format)    \
  get_gain_value(safe_float_read(&plugin->bus_volume[busnum]), value_format)


// Only called once (when starting to play)
void PLUGIN_call_me_before_starting_to_play_song_END(SoundPlugin *plugin){

  bool has_set_effect_value = false;

  {
    radium::PlayerLockOnlyIfNeeded lock;
    
    for(int i=0;i<plugin->type->num_effects+NUM_SYSTEM_EFFECTS;i++) {
      const auto &helper = plugin->songinit_automation_helper[i];
      
      if (helper.abstime >= 0){
        
        if (i < plugin->type->num_effects)
          lock.maybe_pause_or_lock(i);
        
        //enum StoreitType storeit_type = helper.FX_when==FX_single ? STORE_VALUE : DONT_STORE_VALUE;
        
        PLUGIN_set_effect_value(plugin, 0, i, helper.value, DONT_STORE_VALUE, helper.when, helper.value_format);
        has_set_effect_value = true;
      }
    }
  }
  
  if (has_set_effect_value)
    update_instrument_gui(plugin);

  free(plugin->songinit_automation_helper);
}


// May be called many times (when starting to play), even for the same effect_num
void PLUGIN_call_me_before_starting_to_play_song_MIDDLE(SoundPlugin *plugin, int64_t abstime, int effect_num, float value, FX_when when, enum ValueFormat value_format){
  struct SongInitAutomationHelper *helper = &plugin->songinit_automation_helper[effect_num];
  if (helper->abstime < abstime){
    helper->abstime = abstime;
    helper->value_format = value_format;
    helper->value = value;
    helper->when = when;
  }
}


static void maybe_init_effect_when_playing_from_start(struct SoundPlugin *plugin, int effect_num){
  float automation_value = safe_float_read(&plugin->slider_automation_values[effect_num]);

  if (automation_value >= 0) { // Check if last set value came from automation.
    
    float stored_value = safe_float_read(&plugin->stored_effect_values_native[effect_num]);

    PLUGIN_call_me_before_starting_to_play_song_MIDDLE(plugin, 0, effect_num, stored_value, FX_single, EFFECT_FORMAT_NATIVE);
  }
}


// Only called once (when starting to play)
void PLUGIN_call_me_before_starting_to_play_song_START(SoundPlugin *plugin){

  // 1. Allocate helper data;
  plugin->songinit_automation_helper = (typeof(plugin->songinit_automation_helper))calloc(plugin->type->num_effects+NUM_SYSTEM_EFFECTS, sizeof(struct SongInitAutomationHelper));

  
  // 2. Set abstime of the helper objects to -1.
  for(int i = 0 ; i < plugin->type->num_effects+NUM_SYSTEM_EFFECTS ; i++)
    plugin->songinit_automation_helper[i].abstime = -1;


  // 3. Add effects that have been set by automation since last time.
  for(int i=0;i<plugin->type->num_effects+NUM_SYSTEM_EFFECTS;i++)
    maybe_init_effect_when_playing_from_start(plugin, i);
}



namespace{
  struct EffectUndoData{
    instrument_t patch_id;
    //int undo_generation; // Used for corner cases. Probably not important. We have patch_id, which we use to determine if the plugin still exists, and that's most important.
    int effect_num;
    float effect_value;
  };
}

static boost::lockfree::queue<EffectUndoData, boost::lockfree::capacity<8000> > g_effect_undo_data_buffer;

static void add_eud_undo(QVector<EffectUndoData> &s_euds, QHash<instrument_t, QSet<int>> &s_stored_effect_nums, double curr_time, double &s_last_time, int &s_last_undo_num){
  {
    radium::ScopedUndo scoped_undo;
    
    for(auto &eud : s_euds){
      
      struct Patch *patch = PATCH_get_from_id(eud.patch_id);
      
      if (patch != NULL) {
        //printf("    EUD undo %d: %f\n", eud.effect_num, eud.effect_value);
        ADD_UNDO(AudioEffect_CurrPos2(patch, eud.effect_num, eud.effect_value, AE_ALWAYS_CREATE_SOLO_AND_BYPASS_UNDO));
      }
    }
  }

  //printf("  NUM_UNDOS: %d. euds size: %d\n", num_undos, s_euds.size());

  s_euds.clear();
  s_stored_effect_nums.clear();
  s_last_time = curr_time;
  s_last_undo_num = Undo_num_undos();
}

static DEFINE_ATOMIC(bool, g_atomic_must_update_solo_gui_stuff);

static void update_solo_gui_stuff(void){
  SP_call_me_after_solo_has_changed();
}

void PLUGIN_call_me_very_often_from_main_thread(void){

  static double s_last_time = 0;
  //static int64_t s_last_patch_id = -1;
  static int s_last_undo_num = -1;
  
  static QVector<EffectUndoData> s_euds;
  static QHash<instrument_t, QSet<int>> s_stored_effect_nums;

  double curr_time = TIME_get_ms();
  //printf("curr_time: %f\n", curr_time/1000.0);
  
  bool is_old = s_last_time > 0 && curr_time > s_last_time+5000;

  {
    EffectUndoData eud;
    while(g_effect_undo_data_buffer.pop(eud)==true){

      /*
       * Not much point. And, if two different midi patches have midi learn, we end up with a lot of undo entries.

      if(eud.patch_id != s_last_patch_id){        
        add_eud_undo(s_euds, s_stored_effect_nums, curr_time, s_last_time, s_last_undo_num);
        s_last_patch_id = eud.patch_id;
      }
      */
      
      if(s_stored_effect_nums[eud.patch_id].contains(eud.effect_num)==false){
        s_stored_effect_nums[eud.patch_id].insert(eud.effect_num);
        s_euds.push_back(eud);
        if(equal_doubles(s_last_time, 0))
          s_last_time = curr_time;
      }

    }
  }

  if (is_old && s_euds.size() > 0) // || Undo_num_undos() != s_last_undo_num)
    add_eud_undo(s_euds, s_stored_effect_nums, curr_time, s_last_time, s_last_undo_num);

  if (!g_is_starting_up && !g_is_loading)
    if (ATOMIC_GET_RELAXED(g_atomic_must_update_solo_gui_stuff))
      if (ATOMIC_SET_RETURN_OLD(g_atomic_must_update_solo_gui_stuff, false)==true)
        update_solo_gui_stuff();
}

                                                     
// Must/should be called from the plugin if it changes value by itself. After initialization that is.
//
// The function must be called even if storeit_type==DONT_STORE_VALUE
//
// Both 'native_value' and 'scaled_value' must be valid.
//
// The function must NOT be called if it was triggered by a call to plugin->set_effect_value().
// (It should be simple to remove this limiations though, in a way that should provide a much better solution than hacking around with timers, comparing values, and so forth.)
//
// Thread-safe.
//
void PLUGIN_call_me_when_an_effect_value_has_changed(struct SoundPlugin *plugin,
                                                     int effect_num,
                                                     float native_value,
                                                     float scaled_value,
                                                     bool make_undo,
                                                     enum StoreitType storeit_type,
                                                     FX_when when,
                                                     bool update_instrument_widget,
                                                     bool is_sent_from_midi_learn
                                                     )
{
  R_ASSERT_NON_RELEASE(storeit_type==STORE_VALUE || storeit_type==DONT_STORE_VALUE);

  //printf("has_changed. old: %f. New: %f. is_automation: %d. make_undo: %d. from midi_learn: %d\n", safe_float_read(&plugin->stored_effect_values_native[effect_num]), native_value, FX_when_is_automation(when), make_undo, is_sent_from_midi_learn);

  //is_sent_from_midi_learn==false &&
  
  if (!FX_when_is_automation(when)) {

    if (is_sent_from_midi_learn==false && PLUGIN_is_recording_automation(plugin, effect_num)) // if sent from midi-learn, the event is already in the midi queue.
      MIDI_add_automation_recording_event(plugin, effect_num, scaled_value);
    
    if (make_undo) {
      float old_native_value = safe_float_read(&plugin->stored_effect_values_native[effect_num]);
      if (!equal_floats(old_native_value, native_value)) {
        struct Patch *patch = plugin->patch;
        if (patch != NULL){
          EffectUndoData eud;
          eud.patch_id = patch->id;
          //eud.undo_generation = ...;
          eud.effect_num = effect_num;
          eud.effect_value = old_native_value;

          //printf(" ..... Adding eud %f.\n", eud.effect_value);
          if (!g_effect_undo_data_buffer.bounded_push(eud)){
#if !defined(RELEASE)
            RT_message("g_effect_undo_data buffer full\n");
#endif
          }
        }
      }
    }

  }

  safe_float_write(&plugin->last_written_effect_values_native[effect_num], native_value);
  safe_float_write(&plugin->last_written_effect_values_scaled[effect_num], scaled_value);
  
  safe_float_write(&plugin->slider_automation_values[effect_num],
                   FX_when_is_automation(when) ? scaled_value : -10
                   );
  
  if(storeit_type==STORE_VALUE){
#if !defined(RELEASE)
    //printf("   SETTING Savable effect %d (%s). Native: %f. Scaled: %f\n", effect_num, PLUGIN_get_effect_name(plugin, effect_num), native_value, scaled_value);
#endif
    safe_float_write(&plugin->stored_effect_values_native[effect_num], native_value);
    safe_float_write(&plugin->stored_effect_values_scaled[effect_num], scaled_value);
  }


  if (update_instrument_widget){
    ATOMIC_SET(plugin->effect_num_to_show_because_it_was_used_externally, effect_num); // Used by the plugin widget to know current tab num.

    struct Patch *patch = plugin->patch;
    if (patch != NULL)
      ATOMIC_SET(patch->widget_needs_to_be_updated, true);
  }
}


bool g_calling_set_effect_value_from_pd = false;

static void PLUGIN_set_effect_value2(struct SoundPlugin *plugin, const int time, const int effect_num, float value, const enum StoreitType storeit_type, const FX_when when, const enum ValueFormat value_format, const bool sent_from_midi_learn){
  float store_value_native = value;
  float store_value_scaled = value;
  //printf("set effect value. effect_num: %d, value: %f, num_effects: %d\n",effect_num,value,plugin->type->num_effects);

  PLUGIN_touch(plugin);
  
#if !defined(RELEASE)
  R_ASSERT(storeit_type==STORE_VALUE || storeit_type==DONT_STORE_VALUE);

  /*
    // Happens for MIDI learn.
  if (storeit_type==STORE_VALUE)
    if (THREADING_is_player_thread())
      abort(); // This is most likely an error.
  */
  
  if (storeit_type==DONT_STORE_VALUE) {
    
    if (THREADING_is_main_thread())
      if (g_initing_starting_to_play_song==false)
        abort(); // Not an error, but I'm courious if it happens.

  } else {

    if (g_calling_set_effect_value_from_pd==false && false==THREADING_is_main_thread()) {
      R_ASSERT(THREADING_is_player_thread() || THREADING_is_juce_thread()); // Called from midi learn or juce.

      if (THREADING_is_player_thread())
        R_ASSERT(sent_from_midi_learn);
    }
    
  }

  if (sent_from_midi_learn)
    R_ASSERT(storeit_type==STORE_VALUE);

  if (THREADING_is_juce_thread()){
    
#if !defined(FOR_LINUX)
    R_ASSERT(false); // strange. Only linux have a dedicated juce thread (the other platforms misbehaved I think, because the performance is extremely much better with a dedicated thread for the plugin GUIs).
#endif
    R_ASSERT(effect_num == plugin->type->num_effects+EFFNUM_EFFECTS_ONOFF);
    R_ASSERT(storeit_type == STORE_VALUE);
  }

  
  if (FX_when_is_automation(when))
    if (storeit_type==STORE_VALUE)
      abort(); // This is definitely an error though. We don't want to store automation values.

  if (!FX_when_is_automation(when))
    if (storeit_type==DONT_STORE_VALUE)
      if (g_initing_starting_to_play_song==false)
        abort(); // Not an error, but I'm courious if it happens.
#endif
  
  if(value_format==EFFECT_FORMAT_SCALED) {
#if !defined(RELEASE)
    if (value < -0.01f || value > 1.01f)// don't report floating point rounding errors
      RWarning("value: %f", value);
#endif
    value = R_BOUNDARIES(0.0f, value, 1.0f);
  }      

        
  if(effect_num < plugin->type->num_effects){

    radium::PlayerRecursiveLock lock; // Need lock both because set_effect_value expect player lock to be held, but also to ensure another thread doesn't interfere between set_effect_value() and get_effect_value().

    plugin->type->set_effect_value(plugin,time,effect_num,value,value_format,when);
    
    if(storeit_type==STORE_VALUE) {
      R_ASSERT_NON_RELEASE(!FX_when_is_automation(when));
      
      // Reload both native and scaled values, just to be sure. The native effect we get out might not be the same as we put in.
      // We are not doing automation here either, so performance shouldn't matter (it probably wouldn't have mattered even if we did automation though).
      store_value_native = plugin->type->get_effect_value(plugin, effect_num, EFFECT_FORMAT_NATIVE);
      if (plugin->type->get_scaled_value_from_native_value != NULL)
        store_value_scaled = plugin->type->get_scaled_value_from_native_value(plugin, effect_num, store_value_native);
      else
        store_value_scaled = plugin->type->get_effect_value(plugin, effect_num, EFFECT_FORMAT_SCALED);
    }
    
  }else{

    int num_effects = plugin->type->num_effects;
    int system_effect = effect_num - num_effects;

    switch(system_effect){
      
    case EFFNUM_INPUT_VOLUME:
      set_gain_store_value(store_value_native, store_value_scaled, value_format);
      if(ATOMIC_GET(plugin->input_volume_is_on)==true)
        SMOOTH_set_target_value(&plugin->input_volume, store_value_native);
      break;
      
    case EFFNUM_INPUT_VOLUME_ONOFF:
      SET_SMOOTH_ON_OFF(&plugin->input_volume, plugin->input_volume_is_on, value, plugin->last_written_effect_values_native[plugin->type->num_effects + EFFNUM_INPUT_VOLUME], 0.0f);
      break;


      
    case EFFNUM_SOLO_ONOFF:
      SET_ATOMIC_ON_OFF(plugin->solo_is_on, value);

      if (THREADING_is_main_thread() && g_apply_solo_immediately && PLAYER_current_thread_has_lock()==false && !g_is_starting_up && !g_is_loading)
        update_solo_gui_stuff();
      else
        ATOMIC_SET(g_atomic_must_update_solo_gui_stuff, true); // If we update solo immediately, solo changes will appear before other mixer changes such as add/remove connections, when changing a/b in mixer.
      break;


      
    case EFFNUM_VOLUME:
      set_gain_store_value(store_value_native, store_value_scaled, value_format);
      if (ATOMIC_GET(plugin->volume_is_on))
        SMOOTH_set_target_value(&plugin->volume, store_value_native);
      //printf("Setting volume to %f. (scaled: %f)\n", store_value_native, store_value_scaled);
      break;

      
    case EFFNUM_VOLUME_ONOFF:
      SET_SMOOTH_ON_OFF(&plugin->volume, plugin->volume_is_on, value,
                        plugin->last_written_effect_values_native[plugin->type->num_effects + EFFNUM_VOLUME],
                        0.0f);
      break;


      
    case EFFNUM_OUTPUT_VOLUME:
      set_gain_store_value(store_value_native, store_value_scaled, value_format);
      //printf("***PLUGIN_SET_EFFE_CT_FALUE. ****** store_value: %f\n",store_value);
      plugin->output_volume = store_value_native;
      break;
      
    case EFFNUM_OUTPUT_VOLUME_ONOFF:
      SET_ATOMIC_ON_OFF(plugin->output_volume_is_on, value);
      break;

    case EFFNUM_BUS1:
      SET_BUS_VOLUME(0);
      
    case EFFNUM_BUS2:
      SET_BUS_VOLUME(1);
      
    case EFFNUM_BUS3:
      SET_BUS_VOLUME(2);
      
    case EFFNUM_BUS4:
      SET_BUS_VOLUME(3);
      
    case EFFNUM_BUS5:
      SET_BUS_VOLUME(4);

      
    case EFFNUM_PAN:
      if(ATOMIC_GET(plugin->pan_is_on)==true)
        SMOOTH_set_target_value(&plugin->pan, value); // pan value is 0->1 for both scaled and native
      break;
      
    case EFFNUM_PAN_ONOFF:
      SET_SMOOTH_ON_OFF(&plugin->pan, plugin->pan_is_on, value, plugin->last_written_effect_values_native[plugin->type->num_effects + EFFNUM_PAN], 0.5);
      break;
      
#if 0
    case EFFNUM_EDITOR_ONOFF:
      if(value>0.5f){
        ATOMIC_SET(plugin->editor_is_on, true);
        if(plugin->type->show_gui!=NULL)
          plugin->type->show_gui(plugin);
      }else{
        ATOMIC_SET(plugin->editor_is_on, false);
        if(plugin->type->show_gui!=NULL && plugin->type->hide_gui!=NULL)
          plugin->type->hide_gui(plugin);
      }
      break;
#endif


      
    case EFFNUM_DRYWET:
      if(ATOMIC_GET(plugin->effects_are_on)==true)
        SMOOTH_set_target_value(&plugin->drywet, value);
      break;

      
    case EFFNUM_EFFECTS_ONOFF:
      SET_SMOOTH_ON_OFF(&plugin->drywet, plugin->effects_are_on, value, plugin->last_written_effect_values_native[plugin->type->num_effects + EFFNUM_DRYWET], 0.0f);
      break;

      
    case EFFNUM_LOWPASS_FREQ:
      set_freq_store_value(store_value_native, store_value_scaled, value_format);
      plugin->lowpass_freq = store_value_native;
      for(int ch=0;ch<plugin->type->num_outputs;ch++)
        plugin->lowpass.plugins[ch]->type->set_effect_value(plugin->lowpass.plugins[ch], time, 0, store_value_native, EFFECT_FORMAT_NATIVE, when);
      break;
      
    case EFFNUM_LOWPASS_ONOFF:
      SET_ATOMIC_ON_OFF(plugin->lowpass.is_on, value);
      break;


      
    case EFFNUM_HIGHPASS_FREQ:
      set_freq_store_value(store_value_native, store_value_scaled, value_format);
      plugin->highpass_freq = store_value_native;
      for(int ch=0;ch<plugin->type->num_outputs;ch++)
        plugin->highpass.plugins[ch]->type->set_effect_value(plugin->highpass.plugins[ch], time, 0, store_value_native, EFFECT_FORMAT_NATIVE, when);
      break;
      
    case EFFNUM_HIGHPASS_ONOFF:
      SET_ATOMIC_ON_OFF(plugin->highpass.is_on, value);
      break;


      
    case EFFNUM_EQ1_FREQ:
      set_freq_store_value(store_value_native, store_value_scaled, value_format);
      plugin->eq1_freq = store_value_native;
      for(int ch=0;ch<plugin->type->num_outputs;ch++)
        plugin->eq1.plugins[ch]->type->set_effect_value(plugin->eq1.plugins[ch], time, 0, store_value_native, EFFECT_FORMAT_NATIVE, when);
      break;
      
    case EFFNUM_EQ1_GAIN:
      set_filter_gain_store_value(store_value_native, store_value_scaled, value_format);
      plugin->eq1_db = store_value_native;
      for(int ch=0;ch<plugin->type->num_outputs;ch++)
        plugin->eq1.plugins[ch]->type->set_effect_value(plugin->eq1.plugins[ch], time, 1, store_value_native, EFFECT_FORMAT_NATIVE, when);
      break;
      
    case EFFNUM_EQ1_ONOFF:
      SET_ATOMIC_ON_OFF(plugin->eq1.is_on, value);
      break;


      
    case EFFNUM_EQ2_FREQ:
      set_freq_store_value(store_value_native, store_value_scaled, value_format);
      plugin->eq2_freq = store_value_native;
      for(int ch=0;ch<plugin->type->num_outputs;ch++)
        plugin->eq2.plugins[ch]->type->set_effect_value(plugin->eq2.plugins[ch], time, 0, store_value_native, EFFECT_FORMAT_NATIVE, when);
      break;
      
    case EFFNUM_EQ2_GAIN:
      set_filter_gain_store_value(store_value_native, store_value_scaled, value_format);
      plugin->eq2_db = store_value_native;
      for(int ch=0;ch<plugin->type->num_outputs;ch++)
        plugin->eq2.plugins[ch]->type->set_effect_value(plugin->eq2.plugins[ch], time, 1, store_value_native, EFFECT_FORMAT_NATIVE, when);
      break;
      
    case EFFNUM_EQ2_ONOFF:
      SET_ATOMIC_ON_OFF(plugin->eq2.is_on, value);
      break;


      
    case EFFNUM_LOWSHELF_FREQ:
      set_freq_store_value(store_value_native, store_value_scaled, value_format);
      plugin->lowshelf_freq = store_value_native;
      for(int ch=0;ch<plugin->type->num_outputs;ch++)
        plugin->lowshelf.plugins[ch]->type->set_effect_value(plugin->lowshelf.plugins[ch], time, 0, store_value_native, EFFECT_FORMAT_NATIVE, when);
      break;
      
    case EFFNUM_LOWSHELF_GAIN:
      set_filter_gain_store_value(store_value_native, store_value_scaled, value_format);
      plugin->lowshelf_db = store_value_native;
      for(int ch=0;ch<plugin->type->num_outputs;ch++)
        plugin->lowshelf.plugins[ch]->type->set_effect_value(plugin->lowshelf.plugins[ch], time, 1, store_value_native, EFFECT_FORMAT_NATIVE, when);
      break;
      
    case EFFNUM_LOWSHELF_ONOFF:
      SET_ATOMIC_ON_OFF(plugin->lowshelf.is_on, value);
      break;

    case EFFNUM_HIGHSHELF_FREQ:
      set_freq_store_value(store_value_native, store_value_scaled, value_format);
      plugin->highshelf_freq = store_value_native;
      for(int ch=0;ch<plugin->type->num_outputs;ch++)
        plugin->highshelf.plugins[ch]->type->set_effect_value(plugin->highshelf.plugins[ch], time, 0, store_value_native, EFFECT_FORMAT_NATIVE, when);
      break;
      
    case EFFNUM_HIGHSHELF_GAIN:
      set_filter_gain_store_value(store_value_native, store_value_scaled, value_format);
      plugin->highshelf_db = store_value_native;
      for(int ch=0;ch<plugin->type->num_outputs;ch++)
        plugin->highshelf.plugins[ch]->type->set_effect_value(plugin->highshelf.plugins[ch], time, 1, store_value_native, EFFECT_FORMAT_NATIVE, when);
      break;
      
    case EFFNUM_HIGHSHELF_ONOFF:
      SET_ATOMIC_ON_OFF(plugin->highshelf.is_on, value);
      break;

    case EFFNUM_EQ_SHOW_GUI:
      if ( (value >= 0.5f) != plugin->show_equalizer_gui){
        plugin->show_equalizer_gui = value >= 0.5f;
        update_instrument_gui(plugin);
      }
      break;

      
    case EFFNUM_BROWSER_SHOW_GUI:
      if ( (value >= 0.5f) != plugin->show_browser_gui){
        plugin->show_browser_gui = value >= 0.5f;
        update_instrument_gui(plugin);
      }
      break;


      
    case EFFNUM_CONTROLS_SHOW_GUI:
      if ( (value >= 0.5f) != plugin->show_controls_gui){
        plugin->show_controls_gui = value >= 0.5f;
        update_instrument_gui(plugin);
      }
      break;

      // fix. Must call GUI function, and then the GUI function calls COMPRESSOR_set_parameter.
    case EFFNUM_COMP_RATIO:
      //printf("Setting ratio to %f\n",value);
      COMPRESSOR_set_parameter(plugin->compressor, COMP_EFF_RATIO, value);
      break;
      
    case EFFNUM_COMP_THRESHOLD:
      COMPRESSOR_set_parameter(plugin->compressor, COMP_EFF_THRESHOLD, value);
      break;
      
    case EFFNUM_COMP_ATTACK:
      COMPRESSOR_set_parameter(plugin->compressor, COMP_EFF_ATTACK, value);
      break;
      
    case EFFNUM_COMP_RELEASE:
      COMPRESSOR_set_parameter(plugin->compressor, COMP_EFF_RELEASE, value);
      break;
      
    case EFFNUM_COMP_OUTPUT_VOLUME:
      COMPRESSOR_set_parameter(plugin->compressor, COMP_EFF_OUTPUT_VOLUME, value);
      break;
      
    case EFFNUM_COMP_ONOFF:
      SET_ATOMIC_ON_OFF(plugin->comp.is_on, value);
      break;

    case EFFNUM_COMP_SHOW_GUI:
      plugin->show_compressor_gui = value >= 0.5f;
      update_instrument_gui(plugin);
      break;


      
    case EFFNUM_DELAY_TIME:
      {
        set_delay_store_value(store_value_native, store_value_scaled, value_format);
        {
          radium::PlayerRecursiveLock lock;
          
          plugin->delay_time = store_value_native;
          if (plugin->delay != NULL){
            int val = 0;
            if (ATOMIC_GET(plugin->delay_is_on))
              val = plugin->delay_time*MIXER_get_sample_rate()/1000;
            static_cast<radium::SmoothDelay*>(plugin->delay)->setSize(val);
          }
        }
        break;
      }
      
    case EFFNUM_DELAY_ONOFF:
      {
        radium::PlayerRecursiveLock lock; // Not sure, but we might need exclusive access to read plugin->delay_is_on as well. (performance-vice it doesn't matter where this line is placed. If it's called from automation (where performance matter), the lock has been obtained already)
       
        bool new_value = value >= 0.5f;
        bool old_value = ATOMIC_GET(plugin->delay_is_on);
        
        if (old_value != new_value){
          
          ATOMIC_SET(plugin->delay_is_on, new_value);
          
          if (plugin->delay != NULL){
            int val = new_value ? plugin->delay_time*MIXER_get_sample_rate()/1000 : 0;
            static_cast<radium::SmoothDelay*>(plugin->delay)->setSize(val);
          }
          
          update_instrument_gui(plugin);
        }
        
        break;
      }

#define SET_VOICE(name, n1, n2)                                         \
      case EFFNUM_VOICE##n2##_##name:                                   \
        set_voice_##name(plugin, n1, store_value_native, store_value_scaled, value_format); \
        break;
      
#define SET_ALL_VOICES(name)                       \
      SET_VOICE(name, 0, 1);                       \
      SET_VOICE(name, 1, 2);                       \
      SET_VOICE(name, 2, 3);                       \
      SET_VOICE(name, 3, 4);                       \
      SET_VOICE(name, 4, 5);                       \
      SET_VOICE(name, 5, 6);                       \
      SET_VOICE(name, 6, 7);      

      SET_ALL_VOICES(ONOFF);
      SET_ALL_VOICES(TRANSPOSE);
      SET_ALL_VOICES(VOLUME);
      SET_ALL_VOICES(START);
      SET_ALL_VOICES(LENGTH);
      SET_ALL_VOICES(PAN);
      SET_ALL_VOICES(CHANCE);

#undef SET_ALL_VOICES
#undef SET_VOICE
      

    default:
      RError("2. Unknown effect number: %d (%d). %s / %s",effect_num,system_effect,plugin->type->type_name,plugin->type->name);
    }  
  }

  PLUGIN_call_me_when_an_effect_value_has_changed(plugin,
                                                  effect_num,
                                                  store_value_native,
                                                  store_value_scaled,
                                                  storeit_type==STORE_VALUE && sent_from_midi_learn, // make_undo
                                                  storeit_type,
                                                  when,
                                                  false,
                                                  sent_from_midi_learn);

}

void PLUGIN_set_effect_value(struct SoundPlugin *plugin, int time, int effect_num, float value, enum StoreitType storeit_type, FX_when when, enum ValueFormat value_format){
  PLUGIN_set_effect_value2(plugin, time, effect_num, value, storeit_type, when, value_format, false);
}

float PLUGIN_get_effect_value2(struct SoundPlugin *plugin, int effect_num, enum WhereToGetValue where, enum ValueFormat value_format){

  //RT_PLUGIN_touch(plugin);
    
#if !defined(RELEASE)
  if (where==VALUE_FROM_STORAGE)
    if (!THREADING_is_player_thread())
      if (PLAYER_current_thread_has_lock()) // Should be no need to obtain lock here. Get values first, then obtain lock.
        abort();
  
  //if (where==VALUE_FROM_PLUGIN)
  //  R_ASSERT(PLAYER_current_thread_has_lock());
#endif

  if(effect_num >= plugin->type->num_effects + NUM_SYSTEM_EFFECTS || effect_num<0){
    RError("Illegal effect_num %d",effect_num);
    return 0.0f;
  }


  if (where==VALUE_FROM_STORAGE) {
    if (value_format==EFFECT_FORMAT_SCALED)
      return safe_float_read(&plugin->stored_effect_values_scaled[effect_num]);
    else
      return safe_float_read(&plugin->stored_effect_values_native[effect_num]);
  }

  
  if(effect_num < plugin->type->num_effects){
    if (plugin->type->get_effect_value!=NULL){
      R_ASSERT_NON_RELEASE(THREADING_is_main_thread());
      R_ASSERT_NON_RELEASE(PLAYER_current_thread_has_lock()==false);
      return plugin->type->get_effect_value(plugin, effect_num, value_format);
    }
  }

  if (plugin->has_initialized) {
    if (value_format==EFFECT_FORMAT_SCALED)
      return safe_float_read(&plugin->last_written_effect_values_scaled[effect_num]);
    else
      return safe_float_read(&plugin->last_written_effect_values_native[effect_num]);
  }


  
  
  ///////////////////////////////////////////////////
  // NOTE! We are only here during initialization! //
  ///////////////////////////////////////////////////
  
  R_ASSERT_NON_RELEASE(THREADING_is_main_thread());
  R_ASSERT_NON_RELEASE(!PLAYER_current_thread_has_lock());


  
  int system_effect_num = effect_num - plugin->type->num_effects;

  switch(system_effect_num){
    
  case EFFNUM_INPUT_VOLUME:
    return get_gain_value(SMOOTH_get_target_value(&plugin->input_volume), value_format);    
  case EFFNUM_INPUT_VOLUME_ONOFF:
    return ATOMIC_GET(plugin->input_volume_is_on)==true ? 1.0 : 0.0f;

  case EFFNUM_SOLO_ONOFF:
    return ATOMIC_GET(plugin->solo_is_on)==true ? 1.0 : 0.0f;

  case EFFNUM_VOLUME:
    return get_gain_value(SMOOTH_get_target_value(&plugin->volume), value_format); // drywet is 0-1 both for native and scaled
    //return get_gain_value(plugin->volume, value_format);
  case EFFNUM_VOLUME_ONOFF:
    return ATOMIC_GET(plugin->volume_is_on)==true ? 1.0 : 0.0f;

  case EFFNUM_OUTPUT_VOLUME:
    {
      float val = get_gain_value(plugin->output_volume, value_format);
      //printf(">>>>>>>>>>>>>>>>>>>>>>>>> Get output volume. return val: %f. Target value: %f\n",val, plugin->output_volume.target_value);
      return val;
    }
  case EFFNUM_OUTPUT_VOLUME_ONOFF:
    return ATOMIC_GET(plugin->output_volume_is_on)==true ? 1.0 : 0.0f;

  case EFFNUM_BUS1:
    return GET_BUS_VOLUME(0, value_format);
  case EFFNUM_BUS2:
    return GET_BUS_VOLUME(1, value_format);
  case EFFNUM_BUS3:
    return GET_BUS_VOLUME(2, value_format);
  case EFFNUM_BUS4:
    return GET_BUS_VOLUME(3, value_format);
  case EFFNUM_BUS5:
    return GET_BUS_VOLUME(4, value_format);
      
  case EFFNUM_PAN:
    return SMOOTH_get_target_value(&plugin->pan); // pan is 0-1 both for native and scaled. TODO: Native format should definitely be -90 -> 90. (not efficient though. Ideally, there should be a STORAGE_FORMAT as well.)
  case EFFNUM_PAN_ONOFF:
    return ATOMIC_GET(plugin->pan_is_on)==true ? 1.0 : 0.0f;

  case EFFNUM_DRYWET:
    return SMOOTH_get_target_value(&plugin->drywet); // drywet is 0-1 both for native and scaled
  case EFFNUM_EFFECTS_ONOFF:
    return ATOMIC_GET(plugin->effects_are_on)==true ? 1.0 : 0.0f;

  case EFFNUM_LOWPASS_FREQ:
    return get_freq_value(plugin->lowpass_freq, value_format);
  case EFFNUM_LOWPASS_ONOFF:
    return ATOMIC_GET(plugin->lowpass.is_on)==true ? 1.0 : 0.0f;

  case EFFNUM_HIGHPASS_FREQ:
    return get_freq_value(plugin->highpass_freq, value_format);
  case EFFNUM_HIGHPASS_ONOFF:
    return ATOMIC_GET(plugin->highpass.is_on)==true ? 1.0 : 0.0f;

  case EFFNUM_EQ1_FREQ:
    return get_freq_value(plugin->eq1_freq, value_format);
  case EFFNUM_EQ1_GAIN:
    return scale(plugin->eq1_db,FILTER_MIN_DB,FILTER_MAX_DB,0,1);
  case EFFNUM_EQ1_ONOFF:
    return ATOMIC_GET(plugin->eq1.is_on)==true ? 1.0 : 0.0f;

  case EFFNUM_EQ2_FREQ:
    return get_freq_value(plugin->eq2_freq, value_format);
  case EFFNUM_EQ2_GAIN:
    return scale(plugin->eq2_db,FILTER_MIN_DB,FILTER_MAX_DB,0,1);
  case EFFNUM_EQ2_ONOFF:
    return ATOMIC_GET(plugin->eq2.is_on)==true ? 1.0 : 0.0f;
    
  case EFFNUM_LOWSHELF_FREQ:
    return get_freq_value(plugin->lowshelf_freq, value_format);
  case EFFNUM_LOWSHELF_GAIN:
    return get_filter_gain_value(plugin->lowshelf_db, value_format);
  case EFFNUM_LOWSHELF_ONOFF:
    return ATOMIC_GET(plugin->lowshelf.is_on)==true ? 1.0 : 0.0f;
    
  case EFFNUM_HIGHSHELF_FREQ:
    return get_freq_value(plugin->highshelf_freq, value_format);
  case EFFNUM_HIGHSHELF_GAIN:
    return get_filter_gain_value(plugin->highshelf_db, value_format);
  case EFFNUM_HIGHSHELF_ONOFF:
    return ATOMIC_GET(plugin->highshelf.is_on)==true ? 1.0 : 0.0f;

  case EFFNUM_EQ_SHOW_GUI:
    return plugin->show_equalizer_gui==true ? 1.0 : 0.0f;
  case EFFNUM_BROWSER_SHOW_GUI:
    return plugin->show_browser_gui==true ? 1.0 : 0.0f;
  case EFFNUM_CONTROLS_SHOW_GUI:
    return plugin->show_controls_gui==true ? 1.0 : 0.0f;

  case EFFNUM_COMP_RATIO:
    return COMPRESSOR_get_parameter(plugin->compressor, COMP_EFF_RATIO);
  case EFFNUM_COMP_THRESHOLD:
    return COMPRESSOR_get_parameter(plugin->compressor, COMP_EFF_THRESHOLD);
  case EFFNUM_COMP_ATTACK:
    return COMPRESSOR_get_parameter(plugin->compressor, COMP_EFF_ATTACK);
  case EFFNUM_COMP_RELEASE:
    return COMPRESSOR_get_parameter(plugin->compressor, COMP_EFF_RELEASE);
  case EFFNUM_COMP_OUTPUT_VOLUME:
    return COMPRESSOR_get_parameter(plugin->compressor, COMP_EFF_OUTPUT_VOLUME);
  case EFFNUM_COMP_ONOFF:
    return ATOMIC_GET(plugin->comp.is_on)==true ? 1.0 : 0.0f;

  case EFFNUM_COMP_SHOW_GUI:
    return plugin->show_compressor_gui==true ? 1.0 : 0.0f;
    
  case EFFNUM_DELAY_TIME:
    return get_delay_value(plugin->highshelf_db, value_format);
    
  case EFFNUM_DELAY_ONOFF:
    return ATOMIC_GET(plugin->delay_is_on)==true ? 1.0 : 0.0f;
#if 0    
  case EFFNUM_EDITOR_ONOFF:
    return ATOMIC_GET(plugin->editor_is_on)==true ? 1.0 : 0.0f;
#endif

#define GET_VOICE(name, n1, n2)                                 \
    case EFFNUM_VOICE##n2##_##name:                             \
      return get_voice_##name(plugin, n1, value_format);        \
      break;
    
#define GET_ALL_VOICES(name)                       \
    GET_VOICE(name, 0, 1);                         \
    GET_VOICE(name, 1, 2);                         \
    GET_VOICE(name, 2, 3);                         \
    GET_VOICE(name, 3, 4);                         \
    GET_VOICE(name, 4, 5);                         \
    GET_VOICE(name, 5, 6);                         \
    GET_VOICE(name, 6, 7);
    
    GET_ALL_VOICES(ONOFF);
    GET_ALL_VOICES(TRANSPOSE);
    GET_ALL_VOICES(VOLUME);
    GET_ALL_VOICES(START);
    GET_ALL_VOICES(LENGTH);
    GET_ALL_VOICES(PAN);
    GET_ALL_VOICES(CHANCE);
        
#undef GET_ALL_VOICES
#undef GET_VOICE

  default:
    RError("3. Unknown effect number: %d (%d). %s / %s",effect_num,system_effect_num,plugin->type->type_name,plugin->type->name);
    return 0.0f;
  }  
}

float PLUGIN_get_effect_value(struct SoundPlugin *plugin,
                              int effect_num,
                              enum WhereToGetValue where)
{
  return PLUGIN_get_effect_value2(plugin, effect_num, where, EFFECT_FORMAT_SCALED);
}


hash_t *PLUGIN_get_effects_state(SoundPlugin *plugin){
  const SoundPluginType *type=plugin->type;
  hash_t *effects=HASH_create(type->num_effects);

#if 0
  
  for(int i=0;i<type->num_effects;i++)
    HASH_put_float(effects, PLUGIN_get_effect_name(plugin,i), type->get_effect_value(plugin, i, EFFECT_FORMAT_NATIVE)); // Do this so that the plugin can change min/max values between sessions.

  for(int i=type->num_effects;i<type->num_effects+NUM_SYSTEM_EFFECTS;i++)
    HASH_put_float(effects, PLUGIN_get_effect_name(plugin,i), plugin->stored_effect_values[i]);
#else
  
  for(int i=0;i<type->num_effects+NUM_SYSTEM_EFFECTS;i++){
    
    const_char *effect_name = PLUGIN_get_effect_name(plugin,i);

    if (strcmp(NOTUSED_EFFECT_NAME, effect_name)) {
      if(HASH_has_key(effects, effect_name)){
        RError("Same key used twice: -%s-. Instrument: %s / %s", effect_name, plugin->type->type_name, plugin->type->name);
      }else
        HASH_put_float(effects, effect_name, plugin->stored_effect_values_native[i]);
    }
    
  }
#endif
  
  return effects;
}

#define NON_GUI_PARENTGUI -1000

static void reset_gui_parentgui(SoundPlugin *plugin){
  plugin->gui_parentgui = NON_GUI_PARENTGUI;
}

// Only necessary to call if closed on its own, but its safe to call from 'plugin->type->hide_gui()'. Can be called right before, or right after, the GUI actually closes.
void PLUGIN_call_me_when_gui_closes(SoundPlugin *plugin){
  reset_gui_parentgui(plugin);
}

bool PLUGIN_gui_is_visible(SoundPlugin *plugin, int64_t parentgui){
  
  if (plugin->type->gui_is_visible!=NULL)
    if (plugin->type->gui_is_visible(plugin)==false){
      //R_ASSERT_NON_RELEASE(plugin->gui_parentgui == NON_GUI_PARENTGUI); // The plugin must call PLUGIN_call_me_after_gui_has_closed if the GUI is closed on its own.
      reset_gui_parentgui(plugin); // In case PLUGIN_call_me_after_gui_has_closed wasn't called.
      return false;
    }

  parentgui = gui_getParentWindow(parentgui);

  if (gui_getParentWindow(parentgui) != plugin->gui_parentgui)
    return false;

  return true;
}

void PLUGIN_close_gui(SoundPlugin *plugin){
  if (plugin->type->hide_gui != NULL)
    plugin->type->hide_gui(plugin);

  if (plugin->gui_parentgui >= 0){
    if (plugin->patch != NULL)
      API_remove_child_plugin_gui(plugin->gui_parentgui, const_cast<struct Patch*>(plugin->patch));
    else
      R_ASSERT(false);
  }

  PLUGIN_call_me_when_gui_closes(plugin);
}

bool PLUGIN_open_gui(SoundPlugin *plugin, int64_t parentgui){
#if !defined(RELEASE)
  int64_t p2 = parentgui;
#endif
  
  parentgui = gui_getParentWindow(parentgui);

#if !defined(RELEASE)
  if(QString(plugin->type->type_name) != "Faust Dev"){
    auto *widget = API_gui_get_widget(p2);
    auto *window = widget->window();
    int64_t p3 = API_get_gui_from_existing_widget(window);
    if (p3 != parentgui){ // Note: Earlier we tested for p3!=p2 here. I don't know why, and I don't understand why.
      printf("  GAKKAGKKAKK org parentgui: %d. parentgui: %d. windowgui: %d. widgetgui: %d\n",
             (int)p2, (int)parentgui,
             (int)p3, (int)API_get_gui_from_widget(widget));
      fflush(stdout);
      getchar();
      //abort();
    }
  }
#endif
  
  if (parentgui != plugin->gui_parentgui)
    PLUGIN_close_gui(plugin);

  if (plugin->type->show_gui != NULL){
    bool is_opened = plugin->type->show_gui(plugin, parentgui);
    if (is_opened){
      plugin->gui_parentgui = parentgui;
      if (plugin->patch != NULL)
        API_add_child_plugin_gui(parentgui, const_cast<struct Patch*>(plugin->patch));
      else
        R_ASSERT(false);
      return true;
    }
  }

  return false;
}

/*
hash_t *PLUGIN_get_modulation_state(SoundPlugin *plugin){
  struct Patch *patch = (struct Patch*)plugin->patch;
  if(patch==NULL){
    R_ASSERT(false);
    return NULL;
  }

  const SoundPluginType *type=plugin->type;
  hash_t *modulation=NULL;

  for(int i=0;i<type->num_effects+NUM_SYSTEM_EFFECTS;i++){
    struct Patch *modulator_patch = MODULATOR_get_modulator_patch(patch, i); // We probably need to store modulator data here. During loading, etc., MODULATOR_get_modulator_patch could return NULL. But that won't work either, because the modulator has to inform the targets when it is deleted, and then we lose the information again.
    if (modulator_patch != NULL){
      if (modulation==NULL)
        modulation=HASH_create(3);
      HASH_put_instrument_at(modulation, "patch", i, modulator_patch->id);
    }
  }

  return modulation;
}

void PLUGIN_apply_modulation_state(SoundPlugin *plugin, hash_t *modulation){
  const SoundPluginType *type=plugin->type;

  struct Patch *patch = (struct Patch*)plugin->patch;
  if(patch==NULL){
    R_ASSERT(false);
    return;
  }

  for(int i=0;i<type->num_effects+NUM_SYSTEM_EFFECTS;i++){
    if (HASH_has_key_at(modulation, "patch", i)){

      int64_t modulator_patch_id = HASH_get_instrument_at(modulation, "patch", i);
      R_ASSERT_RETURN_IF_FALSE(modulator_patch_id >= 0);

      struct Patch *modulator_patch;
      if (modulator_patch_id==patch->id)
        modulator_patch = patch; // If we are connected to ourself, the id hasn't been inserted into the system yet, and PATCH_get_from_id won't work.
      else
        modulator_patch = PATCH_get_from_id(modulator_patch_id);

      R_ASSERT_RETURN_IF_FALSE(modulator_patch!=NULL);

      int64_t modulator_id = MODULATOR_get_id_from_modulator_patch(modulator_patch);
      R_ASSERT_RETURN_IF_FALSE(modulator_id >= 0);

      MODULATOR_add_target(modulator_id, patch, i);
    }
  }  
}
*/

void PLUGIN_recreate_from_state(SoundPlugin *plugin, hash_t *state, bool is_loading){
  SoundPluginType *type = plugin->type;

  R_ASSERT_RETURN_IF_FALSE(type->recreate_from_state != NULL);
  
  type->recreate_from_state(plugin, state, false);

  // Put values into storage if the plugin state may contain effect values.
  if (true == type->state_may_contain_effect_values || true == type->state_contains_effect_values){
    for(int i=0 ; i < type->num_effects ; i++){
      float store_value_native = plugin->type->get_effect_value(plugin, i, EFFECT_FORMAT_NATIVE);
      
      float store_value_scaled;
      if (plugin->type->get_scaled_value_from_native_value != NULL)
        store_value_scaled = plugin->type->get_scaled_value_from_native_value(plugin, i, store_value_native);
      else
        store_value_scaled = plugin->type->get_effect_value(plugin, i, EFFECT_FORMAT_SCALED);
      
      PLUGIN_call_me_when_an_effect_value_has_changed(plugin, i, store_value_native, store_value_scaled, false, STORE_VALUE, FX_single, false, false);
    }
  }
}

// Called from the mixer gui. (the a/b/c/d/e/f/g/h buttons in the mixer)
void PLUGIN_apply_ab_state(SoundPlugin *plugin, hash_t *state){
  SoundPluginType *type = plugin->type;

  struct Patch *patch = (struct Patch*)plugin->patch;
  
  int num_effects = type->num_effects+NUM_SYSTEM_EFFECTS;
  
  hash_t *values_state = HASH_get_hash(state, "values");

  if (HASH_get_array_size(values_state, "value") != num_effects){
    addMessage(talloc_format("Old AB state is not compatible with current plugin (%d vs. %d). Can not apply new ab state for \"%s\"\n", HASH_get_array_size(values_state, "value"), num_effects, patch->name));
    return;
  }
  
  ADD_UNDO(PluginState(patch, NULL));

  // system effects
  for(int i=type->num_effects;i<num_effects;i++){
    float value = HASH_get_float_at(values_state,"value",i);
    PLUGIN_set_effect_value(plugin, 0, i, value, STORE_VALUE, FX_single, EFFECT_FORMAT_NATIVE);
  }

  // plugin effects
  if (false == type->state_contains_effect_values) {

    if (type->num_effects > 0) {
      float values[type->num_effects];
      
      for(int i=0;i<type->num_effects;i++)
        values[i] = HASH_get_float_at(values_state,"value",i);
      
      {
        radium::PlayerLock lock;
        
        for(int i=0;i<type->num_effects;i++){
          PLAYER_maybe_pause_lock_a_little_bit(i);
          PLUGIN_set_effect_value(plugin, 0, i, values[i], STORE_VALUE, FX_single, EFFECT_FORMAT_NATIVE);
        }
      }
    }
    
  } else {
    
    R_ASSERT_NON_RELEASE(type->recreate_from_state!=NULL);
    
  }
  
  if(type->recreate_from_state!=NULL)
    PLUGIN_recreate_from_state(plugin, HASH_get_hash(state, "plugin_state"), false);
}

// Called from the mixer gui. (the a/b/c/d/e/f/g/h buttons in the mixer)
hash_t *PLUGIN_get_ab_state(SoundPlugin *plugin){
  hash_t *state = HASH_create(2);

  SoundPluginType *type = plugin->type;
  
  int num_effects = type->num_effects+NUM_SYSTEM_EFFECTS;
    
  hash_t *values_state = HASH_create(num_effects);

  int start_effectnum = 0;
  if (type->state_contains_effect_values)
    start_effectnum = type->num_effects;
  
  for(int n=start_effectnum;n<num_effects;n++)
    HASH_put_float_at(values_state, "value", n, plugin->stored_effect_values_native[n]);
    
  HASH_put_hash(state, "values", values_state);
    
  if(type->create_state!=NULL && type->recreate_from_state!=NULL){
    hash_t *plugin_state = HASH_create(5);
    type->create_state(plugin, plugin_state);
    HASH_put_hash(state, "plugin_state", plugin_state);
  }

  return state;
}

hash_t *PLUGIN_get_state(SoundPlugin *plugin){
  const SoundPluginType *type=plugin->type;

  hash_t *state=HASH_create(5);

  HASH_put_int(state, "plugin_type_version", type->version);

  // type name / name / container name
  {
    HASH_put_chars(state, "type_name", type->type_name);
    HASH_put_chars(state, "name", type->name);
    
    if (type->container!=NULL)
      HASH_put_chars(state, "container_name", type->container->name);
  }
  
  // midi learns
  for(int i = 0 ; i < plugin->midi_learns->size() ; i++){
    auto *midi_learn = plugin->midi_learns->at(i);
    HASH_put_hash_at(state, "midi_learns", i, midi_learn->create_state());
  }

  // random_change
  {
    hash_t *r = HASH_create(type->num_effects+NUM_SYSTEM_EFFECTS);
    
    for(int i=0;i<type->num_effects+NUM_SYSTEM_EFFECTS;i++){
      const_char *effect_name = PLUGIN_get_effect_name(plugin,i);
      if (false==HASH_has_key(r, effect_name)) // Prevent HASH_put_bool from showing assertion window if there are two effects with the same name. Not sure if it can happen though.
        HASH_put_bool(r, effect_name, plugin->do_random_change[i]);
    }
    
    HASH_put_hash(state, "random_change", r);
  }
  
  // effects
  HASH_put_hash(state,"effects",PLUGIN_get_effects_state(plugin));

  /*
  // modulation
  {
    hash_t *modulation_state = PLUGIN_get_modulation_state(plugin);
    if (modulation_state != NULL)
      HASH_put_hash(state,"modulation",modulation_state);
  }
  */

  // auto-suspend
  HASH_put_int(state,"auto_suspend_behavior", PLUGIN_get_autosuspend_behavior(plugin));
  
  // plugin state
  if(type->create_state != NULL){
    hash_t *plugin_state = HASH_create(10);
    HASH_put_hash(state, "plugin_state", plugin_state);
    type->create_state(plugin, plugin_state);
  }

  // A/B
  {
    hash_t *ab_state=HASH_create(NUM_AB);
    
    int num_effects = type->num_effects+NUM_SYSTEM_EFFECTS;
    
    HASH_put_int(ab_state, "curr_ab_num", plugin->curr_ab_num);
    
    for(int i=0;i<NUM_AB;i++){
      
      bool is_valid = plugin->ab_is_valid[i];
      
      HASH_put_bool_at(ab_state,"is_valid",i,is_valid);
      
      if (is_valid){
        HASH_put_hash_at(ab_state, "ab_state", i, plugin->ab_states[i]);
        
        hash_t *values_state = HASH_create(num_effects);
        for(int n=0;n<num_effects;n++)
          HASH_put_float_at(values_state,"value",n,plugin->ab_values[i][n]);
        
        HASH_put_hash_at(ab_state, "ab_values", i, values_state);
      }
      
    }
    
    HASH_put_hash(state,"ab",ab_state);
  }

  HASH_put_bool(state, "enable_sample_seek", ATOMIC_GET(plugin->enable_sample_seek));

  HASH_put_bool(state, "is_dpi_aware", plugin->is_dpi_aware);

  if (isLegalFilepath(plugin->preset_filename))
    HASH_put_filepath(state, "preset_filename", plugin->preset_filename);
  
  HASH_put_int(state, "___radium_plugin_state_v3", 1);
      
  return state;
}

float PLUGIN_get_effect_from_name(SoundPlugin *plugin, const char *effect_name, enum WhereToGetValue where, enum ValueFormat value_format){
  const SoundPluginType *type=plugin->type;
  int i;

  // This should be in a hash table. The function is called quite often.
  for(i=0;i<type->num_effects+NUM_SYSTEM_EFFECTS;i++){
    if (!strcmp(PLUGIN_get_effect_name(plugin, i), effect_name))
      break;
  }

  if (i==type->num_effects+NUM_SYSTEM_EFFECTS) {
    GFX_Message(NULL, "No effect named \"%s\" in %s/%s", effect_name, type->type_name, type->name);
    return 0;
  }

  return PLUGIN_get_effect_value2(plugin, i, where, value_format);
}


void PLUGIN_set_effect_from_name(SoundPlugin *plugin, const char *effect_name, float value){
  const SoundPluginType *type=plugin->type;
  int i;

  for(i=0;i<type->num_effects+NUM_SYSTEM_EFFECTS;i++){
    if (!strcmp(PLUGIN_get_effect_name(plugin, i), effect_name))
      break;
  }

  if (i==type->num_effects+NUM_SYSTEM_EFFECTS) {
    GFX_Message(NULL, "No effect named \"%s\" in %s/%s", effect_name, type->type_name, type->name);
    return;
  }

  //printf("      Going to set %s to %f\n",effect_name,value);
  PLUGIN_set_effect_value(plugin, -1, i, value, STORE_VALUE, FX_single, EFFECT_FORMAT_SCALED);
}


void PLUGIN_set_effects_from_state(SoundPlugin *plugin, hash_t *effects){
  const SoundPluginType *type=plugin->type;

  hash_t *copy = HASH_copy(effects);
      
  bool has_value[type->num_effects+NUM_SYSTEM_EFFECTS];
  float values[type->num_effects+NUM_SYSTEM_EFFECTS];

  // must use memset instead of {} since clang gave error message.
  memset(has_value, 0, sizeof(bool)*(type->num_effects+NUM_SYSTEM_EFFECTS));
  memset(values, 0, sizeof(float)*(type->num_effects+NUM_SYSTEM_EFFECTS));
  
  bool has_given_warning_about_chance = false;
  
  for(int i=0;i<type->num_effects+NUM_SYSTEM_EFFECTS;i++) {

    const char *effect_name = PLUGIN_get_effect_name(plugin,i);

    // Workaround for that pesky old bug that caused all chance values being set to 1 in older songs, e.g. "the elf" and "romance".
    if (g_is_loading && QString(effect_name)=="System On/Off Voice 1")
      break;
    
    has_value[i] = HASH_has_key(effects, effect_name);

    if (!has_value[i]) {
      // workaround for older songs I think
      const char *name = get_effect_name_without_digit_prefix(effect_name);
      if (name!=NULL){
        effect_name = name;
        has_value[i] = HASH_has_key(effects, effect_name);
      }
    }
    
    if (has_value[i]) {
      values[i] = HASH_get_float(effects, effect_name);

      // Fix faulty chance values written to disk for songs with disk version 0.96.
      if(g_is_loading)
        if (disk_load_version>0.955 && disk_load_version<0.965)
          if (QString(effect_name).startsWith("System Chance Voice "))
            if(fabsf(values[i]-1.0f) < 0.001f){
              values[i] = 256.0;
              if(has_given_warning_about_chance==false){
                if(false==is_radium_internal_file(dc.filename_with_full_path))
                  GFX_addMessage("Note: Changed note duplicator chance values from 1 to 256. Most likely, these values were wrongfully saved as 1 instead of 256 because of an earlier bug in the program.");
                has_given_warning_about_chance = true;
              }
            }
          
      HASH_remove(copy, effect_name);
    }
      
  }    

  if (HASH_get_num_elements(copy) > 0){
    char *effect_names = talloc_strdup("");
    hash_t *keys = HASH_get_keys_in_hash(copy);
    int num_added=0;
    
    for(int i = 0 ; i < HASH_get_array_size(keys, "key"); i++){
      const char *effect_name = HASH_get_chars_at(keys, "key", i);
      if (strncmp(effect_name, NOTUSED_EFFECT_NAME, strlen(NOTUSED_EFFECT_NAME)))
        effect_names = talloc_format("%s\n* %s", effect_names, effect_name);
    }

    if (num_added > 0)
      GFX_Message(NULL, "The effect names of %s / %s has changed.\nThe following effects can not be loaded: %s\n\nIf you know the new name of the effect, you can edit the song manually in a text editor.", type->type_name, type->name, effect_names);
  }
  
  // 2. Store system effects
  for(int i=type->num_effects;i<type->num_effects+NUM_SYSTEM_EFFECTS;i++){

    // Workaround for that pesky old bug that caused all chance values being set to 1 in older songs, e.g. "the elf" and "romance".
    if (g_is_loading){
      const char *effect_name = PLUGIN_get_effect_name(plugin,i);
      if (QString(effect_name)=="System On/Off Voice 1")
        break;
    }

    float val = has_value[i] ? values[i] : plugin->initial_effect_values_native[i];
    PLUGIN_set_effect_value(plugin, -1, i, val, STORE_VALUE, FX_single, EFFECT_FORMAT_NATIVE);
    
    /*
      ?? (old code)

      }else if (i-type->num_effects==EFFNUM_HIGHPASS_FREQ)
        plugin->stored_effect_values_native[i] = 200; // Old songs didn't have this slider
      else
        plugin->stored_effect_values_native[i] = PLUGIN_get_effect_value(plugin, i, VALUE_FROM_PLUGIN, EFFECT_FORMAT_NATIVE);
    */
  }


  // 2.5. Load bus on/off effects in older songs
  if (HASH_has_key(effects, "System Reverb On/Off")){
  
    plugin->bus_on_off = (bool*)V_calloc(sizeof(bool), NUM_BUSES); // seems to be freed in QM_chip.cpp
    
    plugin->bus_on_off[0] = HASH_get_float(effects, "System Reverb On/Off") >= 0.5;
    plugin->bus_on_off[1] = HASH_get_float(effects, "System Chorus On/Off") >= 0.5;

    if(HASH_has_key(effects, "System Aux 1 On/Off"))
      plugin->bus_on_off[2] = HASH_get_float(effects, "System Aux 1 On/Off") >= 0.5;

    if(HASH_has_key(effects, "System Aux 2 On/Off"))
      plugin->bus_on_off[3] = HASH_get_float(effects, "System Aux 2 On/Off") >= 0.5;

    if(HASH_has_key(effects, "System Aux 3 On/Off"))
      plugin->bus_on_off[4] = HASH_get_float(effects, "System Aux 3 On/Off") >= 0.5;
    
  }

  
  // 3. Store custom effects
  if (type->state_contains_effect_values == false) {

    if (type->num_effects > 0) {
      
      radium::PlayerLock lock; // To avoid relocking for every effect.

      for(int i=0 ; i<type->num_effects ; i++){
        PLAYER_maybe_pause_lock_a_little_bit(i);
        
        float val = has_value[i] ? values[i] : plugin->initial_effect_values_native[i];
        PLUGIN_set_effect_value(plugin, -1, i, val, STORE_VALUE, FX_single, EFFECT_FORMAT_NATIVE);
      }
      
    }
    
  }else{
#if !defined(RELEASE)
    if (type->recreate_from_state==NULL) // Not necessarily wrong. Just want to know if it ever happens.
      abort();
#endif
  }
}

// Called from DLoadAudioInstrument when loading, and AUDIO_InitPatch2 when not loading.
void PLUGIN_DLoad(SoundPlugin *plugin){
  /*
  if (plugin->modulation_state != NULL){
    PLUGIN_apply_modulation_state(plugin, plugin->modulation_state);
    remove_gc_root(plugin->modulation_state);
    plugin->modulation_state = NULL;
  }
  */
}

SoundPlugin *PLUGIN_create_from_state(struct Patch *patch, hash_t *state, bool is_loading){
  const char *container_name = HASH_has_key(state, "container_name") ? HASH_get_chars(state, "container_name") : NULL;
  const char *type_name = HASH_get_chars(state, "type_name");
  const char *name = HASH_get_chars(state, "name");

  SoundPluginType *type = PR_get_plugin_type_by_name(container_name, type_name, name);
  R_ASSERT(type!=NULL); // PR_get_plugin_type_by_name can not return NULL;

  hash_t *plugin_state;

  // plugin state
  //
  if (strcmp(type->type_name, type_name) || strcmp(type->name, name))
    plugin_state=NULL; //i.e. selected a different plugin.
  
  else if (HASH_has_key(state, "plugin_state"))
    plugin_state=HASH_get_hash(state, "plugin_state");
  
  else
    plugin_state=NULL;

  // Debugging. Program should never crash if PLUGIN_create_from_state returns NULL;
  //if (!strcmp(type_name,"VST"))
  //  return NULL;
  
  SoundPlugin *plugin = PLUGIN_create(patch, type, plugin_state, is_loading);

  if(plugin==NULL)
    return NULL;

  int state_version = HASH_has_key(state, "plugin_type_version") ? HASH_get_int32(state, "plugin_type_version") : -1;
  int plugin_type_version = type->version;

  // effects state
  hash_t *effects = HASH_get_hash(state, "effects");
  PLUGIN_set_effects_from_state(plugin, effects);

  if(plugin_state!=NULL && type->recreate_from_state!=NULL)
    PLUGIN_recreate_from_state(plugin, plugin_state, is_loading);
  
  // auto-suspend
  if (HASH_has_key(state, "auto_suspend_behavior"))
    PLUGIN_set_autosuspend_behavior(plugin, (AutoSuspendBehavior)HASH_get_int(state, "auto_suspend_behavior"));

  // modulation
  /*
  if (HASH_has_key(state, "modulation")){
    hash_t *modulation = HASH_get_hash(state, "modulation");
    if (modulation!=NULL)
      plugin->modulation_state = add_gc_root(modulation); // Apply modulation later since the modulation patches might not exist yet. (especially if it's connected to itself)
  }
  */

  // random change
  if (HASH_has_key(state, "random_change")){
    hash_t *r = HASH_get_hash(state, "random_change");
    
    for(int i=0;i<type->num_effects+NUM_SYSTEM_EFFECTS;i++){
      const char *effect_name = PLUGIN_get_effect_name(plugin,i);
      if (HASH_has_key(r, effect_name))
        plugin->do_random_change[i] = HASH_get_bool(r, effect_name);
    }
  }
  
  // midi learns state
  {
    for(int i = 0 ; i < HASH_get_array_size(state, "midi_learns") ; i++){
      if (HASH_has_key_at(state, "midi_learns", i)){ // In case array is used for something else as well. TODO: Create a HASH_get_array_size function that takes key as argument.
        auto *midi_learn = new radium::SoundPluginEffectMidiLearn(plugin, HASH_get_hash_at(state, "midi_learns", i));
        add_midi_learn(midi_learn);
      }
    }
  }

  bool has_shown_error = false;
  
  // A/B
  if (state_version==plugin_type_version) {
    if (HASH_has_key(state, "ab")){

      int num_effects = type->num_effects+NUM_SYSTEM_EFFECTS;
      
      hash_t *ab_state=HASH_get_hash(state, "ab");

      plugin->curr_ab_num = HASH_get_int32(ab_state, "curr_ab_num");
      
      for(int i=0;i<NUM_AB;i++){
        plugin->ab_is_valid[i] = HASH_get_bool_at(ab_state, "is_valid", i);

        if (plugin->ab_is_valid[i]){
          
          hash_t *values_state = HASH_get_hash_at(ab_state, "ab_values", i);
          
          if (HASH_get_array_size(values_state, "value") != num_effects) {

            if (has_shown_error == false){
              addMessage(talloc_format("Could not load A/B settings for %s / %s since the number of effects have changed", type_name, name));
              has_shown_error = true;
            }
            
            plugin->ab_is_valid[i] = false;
            
          } else {

            plugin->ab_states[i] = (hash_t*)replace_gc_root(plugin->ab_states[i], HASH_get_hash_at(ab_state, "ab_state", i));

            for(int n=0;n<num_effects;n++)
              if (HASH_has_key_at(values_state,"value",n)){
                plugin->ab_values[i][n] = HASH_get_float_at(values_state,"value",n);
              } else{
                RError("Non-release: Unknown key %s / %d while loading A/B values.\n", "value", n);
              }
            
          }
        }
      }
      
    }
  }

  
  if (HASH_has_key(state, "enable_sample_seek"))
    ATOMIC_SET(plugin->enable_sample_seek, HASH_get_bool(state, "enable_sample_seek"));
  
  if (HASH_has_key(state, "is_dpi_aware"))
    plugin->is_dpi_aware = HASH_get_bool(state, "is_dpi_aware");

  if (HASH_has_key(state, "preset_filename"))
    plugin->preset_filename = HASH_get_filepath(state, "preset_filename");

  
  return plugin;
}

void PLUGIN_change_ab(SoundPlugin *plugin, int ab_num){
  R_ASSERT_RETURN_IF_FALSE(ab_num>=0);
  R_ASSERT_RETURN_IF_FALSE(ab_num<NUM_AB);

  const SoundPluginType *type = plugin->type;
  
  const int old_ab_num = plugin->curr_ab_num; // const since old_ab_num is in a call to PluginState_CurrPos further down.
  const int new_ab_num = ab_num;
  
  const int num_effects = type->num_effects+NUM_SYSTEM_EFFECTS;

  if (old_ab_num==new_ab_num){
    R_ASSERT_NON_RELEASE(false);
    return;
  }
  
  // Save old data
  {
    memcpy(plugin->ab_values[old_ab_num], plugin->stored_effect_values_native, sizeof(float)*num_effects);

    if(type->create_state!=NULL && type->recreate_from_state!=NULL) {
      HASH_clear(plugin->ab_states[old_ab_num]);
      type->create_state(plugin, plugin->ab_states[old_ab_num]);
    }

    /*
    printf("\nSetting ab values:\n");
    for(int i=0;i<num_effects;i++)
      printf("  %d (%s): %f / %f, ", i, PLUGIN_get_effect_name(plugin, i), plugin->stored_effect_values_native[i], plugin->stored_effect_values_scaled[i]);
    printf("\n");
    */
    
    plugin->ab_is_valid[old_ab_num] = true;
  }

  // Insert new data
  //
  if(plugin->ab_is_valid[new_ab_num]){
    
    float *new_ab_values = plugin->ab_values[new_ab_num];

    struct Patch *patch = const_cast<struct Patch*>(plugin->patch);

    ADD_UNDO(PluginState(patch, HASH_shallow_copy(plugin->ab_states[old_ab_num])));
    
    // Set system effects
    for(int i=type->num_effects;i<type->num_effects+NUM_SYSTEM_EFFECTS;i++)
      PLUGIN_set_effect_value(plugin, 0, i, new_ab_values[i], STORE_VALUE, FX_single, EFFECT_FORMAT_NATIVE);

    // set custom effects, if necessary
    if (type->state_contains_effect_values == false){

      if (type->num_effects > 0) {
        radium::PlayerLock lock; // To avoid relocking for every effect.
        
        for(int i=0 ; i < type->num_effects ; i++){
          PLAYER_maybe_pause_lock_a_little_bit(i);
          PLUGIN_set_effect_value(plugin, 0, i, new_ab_values[i], STORE_VALUE, FX_single, EFFECT_FORMAT_NATIVE);
        }
      }
        
    }else{
      
      R_ASSERT_NON_RELEASE(type->recreate_from_state!=NULL);
      
    }
    
    if(type->recreate_from_state!=NULL){
      PLUGIN_recreate_from_state(plugin, plugin->ab_states[new_ab_num], false);      
      GFX_update_instrument_widget(patch);
    }
  }
  
  plugin->curr_ab_num = new_ab_num;
}

void PLUGIN_reset_ab(SoundPlugin *plugin, int num){
  if (num==-1)
    for(int ab=0;ab<NUM_AB;ab++)
      plugin->ab_is_valid[ab] = false;
  else
    plugin->ab_is_valid[num] = false;
}

const char *PLUGIN_generate_new_patchname(SoundPlugin *plugin){
  const char *maybe = SEND_RECEIVE_maybe_generate_patch_name(plugin);
  if (maybe)
    return maybe;
  else
    return talloc_format("%s %d",plugin->type->name,++plugin->type->instance_num);    
}

QString radium::SoundPluginEffectMidiLearn::get_dest_info(void){
  QString a = plugin->patch==NULL ? plugin->type->name : plugin->patch->name;

  return a + " / " + PLUGIN_get_effect_name(plugin, effect_num);
}

void radium::SoundPluginEffectMidiLearn::delete_me(void){
  PLUGIN_remove_midi_learn(plugin, effect_num, true);
}

// called from player thread
void radium::SoundPluginEffectMidiLearn::RT_callback(float val) {
  //printf("soundpluginmidilearn %s got %f\n", plugin->patch->name, val);

  int num_effects = plugin->type->num_effects;
  int system_effect = effect_num - num_effects;

  //printf("effect_num: %d. system_effect: %d, comp_eff_attack: %d\n", effect_num, effect_num - num_effects, COMP_EFF_ATTACK);

  if(system_effect==EFFNUM_COMP_ATTACK || system_effect==EFFNUM_COMP_RELEASE) {
    val = scale(val, 0, 1, 0, 500); // Need to clean up the compressor things.
    PLUGIN_set_effect_value2(plugin, -1, effect_num, val, STORE_VALUE, FX_single, EFFECT_FORMAT_NATIVE, true);
  } else
    PLUGIN_set_effect_value2(plugin, -1, effect_num, val, STORE_VALUE, FX_single, EFFECT_FORMAT_SCALED, true);
  
  struct Patch *patch = plugin->patch;
  if (patch != NULL)
    ATOMIC_SET(patch->widget_needs_to_be_updated, true);
}

void PLUGIN_add_midi_learn(SoundPlugin *plugin, int effect_num){
  PLUGIN_touch(plugin);
  
  auto *midi_learn = new radium::SoundPluginEffectMidiLearn(plugin, effect_num);
  add_midi_learn(midi_learn);

  update_instrument_gui(plugin);
  SEQUENCER_update(SEQUPDATE_HEADERS);
  RT_schedule_mixer_strips_redraw();
}

bool PLUGIN_remove_midi_learn(SoundPlugin *plugin, int effect_num, bool show_error_if_not_here){
  PLUGIN_touch(plugin);
  
  radium::SoundPluginEffectMidiLearn *midi_learn=NULL;

  for(auto *maybe_this_midi_learn : *plugin->midi_learns)
    if (effect_num==-1 || maybe_this_midi_learn->effect_num == effect_num){
      midi_learn = maybe_this_midi_learn;
      break;
    }

  if (midi_learn==NULL){
    if (show_error_if_not_here)
      RError("No midi learn for %s / %d\n", plugin->patch->name, effect_num);
    return false;
  }

  plugin->midi_learns->remove(midi_learn, true);
  MIDI_remove_midi_learn(midi_learn, false);

  delete midi_learn;

  update_instrument_gui(plugin);
  SEQUENCER_update(SEQUPDATE_HEADERS);
  RT_schedule_mixer_strips_redraw();
  
  return true;
}

bool PLUGIN_has_midi_learn(SoundPlugin *plugin, int effect_num){
  for(auto *maybe_this_midi_learn : *plugin->midi_learns)
    if (effect_num==-1 || maybe_this_midi_learn->effect_num == effect_num){
      if (ATOMIC_GET(maybe_this_midi_learn->is_enabled))
        return true;
    }

  return false;
}

bool PLUGIN_is_recording_automation(const SoundPlugin *plugin, const int effect_num){
  return ATOMIC_GET_ARRAY(plugin->is_recording_automation, effect_num);
}

void PLUGIN_set_recording_automation(SoundPlugin *plugin, const int effect_num, const bool is_recording){
  ATOMIC_SET_ARRAY(plugin->is_recording_automation, effect_num, is_recording);
}

void PLUGIN_set_all_effects_to_not_recording(SoundPlugin *plugin){
  for(int e = 0 ; e<plugin->type->num_effects+NUM_SYSTEM_EFFECTS ; e++)
    PLUGIN_set_recording_automation(plugin, e, false);
}

void PLUGIN_set_autosuspend_behavior(SoundPlugin *plugin, enum AutoSuspendBehavior new_behavior){
  ATOMIC_SET(plugin->auto_suspend_behavior, new_behavior);
  update_instrument_gui(plugin);
}

enum AutoSuspendBehavior PLUGIN_get_autosuspend_behavior(const SoundPlugin *plugin){
  return ATOMIC_GET(plugin->auto_suspend_behavior);
}

void PLUGIN_set_random_behavior(SoundPlugin *plugin, const int effect_num, bool do_random){
  plugin->do_random_change[effect_num] = do_random;
}

bool PLUGIN_get_random_behavior(SoundPlugin *plugin, const int effect_num){
  return plugin->do_random_change[effect_num];
}


void PLUGIN_set_soloed(SoundPlugin *plugin, bool soloit, bool apply_now){
  R_ASSERT_NON_RELEASE(THREADING_is_main_thread());
  
  float new_value = soloit ? 1.0 : 0.0;
  int effect_num = plugin->type->num_effects + EFFNUM_SOLO_ONOFF;

  g_apply_solo_immediately = apply_now;
  {
    PLUGIN_set_effect_value(plugin, -1, effect_num, new_value, STORE_VALUE, FX_single, EFFECT_FORMAT_NATIVE);
  }
  g_apply_solo_immediately = false;
}

void PLUGIN_set_muted(SoundPlugin *plugin, bool muteit){
  /*
  if (muteit==PLUGIN_get_muted(plugin)) // can't do this when we use STORE_VALUE
    return;
  */
  
  float new_val = muteit ? 0.0 : 1.0;
  int effect_num = get_mute_effectnum(plugin->type);
  
  PLUGIN_set_effect_value(plugin, -1, effect_num, new_val, STORE_VALUE, FX_single, EFFECT_FORMAT_SCALED);
}


#if 0

// These ones are too easy to use the wrong way. Use PLUGIN_set_effect_value / PLUGIN_get_effect_value instead, or plugin->solo_is_on/etc. for graphics.

bool PLUGIN_get_muted(SoundPlugin *plugin){
  return is_muted(plugin);
}

bool PLUGIN_get_soloed(SoundPlugin *plugin){
  return ATOMIC_GET(plugin->solo_is_on);
}

/*
No: Use PLUGIN_get_effect_value instead.
bool PLUGIN_get_soloed_relaxed(SoundPlugin *plugin){
  return ATOMIC_GET_RELAXED(plugin->solo_is_on);
}
*/

#endif


// only called from Soundproducer.cpp, one time per soundcard block per instrument
bool RT_PLUGIN_can_autosuspend(const SoundPlugin *plugin, int64_t time){

  struct SoundPluginType *type = plugin->type;

  {  
    if (type->will_never_autosuspend==true)
      return false;

    bool type_will_always_autosuspend = type->will_always_autosuspend;
    
    if (type_will_always_autosuspend==false){
      enum AutoSuspendBehavior auto_suspend_behavior = PLUGIN_get_autosuspend_behavior(plugin);
      
      if (auto_suspend_behavior==AUTOSUSPEND_DISABLED)
        return false;
      
      if (auto_suspend_behavior==DEFAULT_AUTOSUSPEND_BEHAVIOR)
        if (ATOMIC_GET(g_enable_autobypass) == false)
          return false;
    }
  }

  {
    if (plugin->playing_voices != NULL || plugin->patch->playing_voices != NULL){
      //if(!strcmp(plugin->patch->name, "Paff_snare"))
      //  printf("  auto1\n");
      
      return false;
    }
  }

  {
    if (ATOMIC_GET(plugin->auto_suspend_suspended)){
      //if(!strcmp(plugin->patch->name, "Paff_snare"))
      //  printf("  auto2\n");
      return false;
    }
  }
  
  {
    int delay = -1;

    if (type->RT_get_audio_tail_length != NULL)
      delay = type->RT_get_audio_tail_length(plugin);

    if (delay == -1)
      delay = (double)ATOMIC_GET(g_autobypass_delay) * MIXER_get_sample_rate() / 1000.0;

    if (delay < -1){
      //if(!strcmp(plugin->patch->name, "Paff_snare"))
      //  printf("  delay < -1\n");
      
      return false;
    }
    
    // input latency
    delay += RT_SP_get_input_latency(plugin->sp);

    // plugin latency
    if (plugin->type->RT_get_latency != NULL)
      delay += plugin->type->RT_get_latency(plugin);

    // smooth delay delay
    delay += (plugin->delay_time * MIXER_get_sample_rate() / 1000);
    
    // Add soundcard block size since we won't do this check again until the next soundcard block.
    delay += g_jackblock_size;

    // ...and we add some frames to eliminate rounding errors and possibly other minor things (system filters, etc.). (important for instruments that implement RT_get_audio_tail_length)
    delay += 64;

    int64_t time_since_activity = time - ATOMIC_NAME(plugin->_RT_time_of_last_activity);

    
    //if(!strcmp(plugin->patch->name, "Paff_snare"))
    //  printf("Auto: %d. time since last activity: %d (%fms). Delay: %d (%fms)\n", time_since_activity > delay, (int)time_since_activity, frames_to_ms(time_since_activity), (int)delay, frames_to_ms(delay));
    
    
    if (time_since_activity > delay){
      return true;
    }
  }
  
  return false;
}

/*
bool PLUGIN_can_autobypass(SoundPlugin *plugin){
  return RT_PLUGIN_can_autobypass(plugin, MIXER_get_last_used_time());
}
*/

void PLUGIN_reset(SoundPlugin *plugin){
  const SoundPluginType *type = plugin->type;

  struct Patch *patch = plugin->patch;
  R_ASSERT_RETURN_IF_FALSE(patch!=NULL);
  
  if (type->num_effects==0)
    return;
      
  ADD_UNDO(AudioEffect_CurrPos((struct Patch*)patch, -1, AE_NO_FLAGS));

  // system effects (no, we don't reset those, only plugin effects)
  /*
  for(int i=type->num_effects;i<type->num_effects+NUM_SYSTEM_EFFECTS;i++)
    PLUGIN_set_effect_value(plugin,
                            0,
                            i,
                            plugin->initial_effect_values[i],
                            STORE_VALUE,
                            FX_single,
                            EFFECT_FORMAT_NATIVE);
  */

  
  PLAYER_lock();{  // To avoid relocking for every effect.
    for(int i=0 ; i<type->num_effects ; i++){
      PLAYER_maybe_pause_lock_a_little_bit(i);
      PLUGIN_set_effect_value(plugin,
                              0,
                              i,
                              plugin->initial_effect_values_native[i],
                              STORE_VALUE,
                              FX_single,
                              EFFECT_FORMAT_NATIVE);
    }
  }PLAYER_unlock();
}

void PLUGIN_reset_one_effect(SoundPlugin *plugin, int effect_num){
  struct Patch *patch = plugin->patch;
  R_ASSERT_RETURN_IF_FALSE(patch!=NULL);

  int system_effect = effect_num - plugin->type->num_effects;
  
  bool is_solo = system_effect == EFFNUM_SOLO_ONOFF;
    
  if (is_solo) {
    if (plugin->patch != NULL){
      setInstrumentSolo(false, plugin->patch->id);
      return;
    }else{
      R_ASSERT_NON_RELEASE(false);
    }    
  }

  ADD_UNDO(AudioEffect_CurrPos((struct Patch*)patch, effect_num, AE_NO_FLAGS));

  PLUGIN_set_effect_value(plugin, 0, effect_num, plugin->initial_effect_values_native[effect_num], STORE_VALUE, FX_single, EFFECT_FORMAT_NATIVE);
}

static float get_rand(void){
  float r = (double)rand() / (double)RAND_MAX;
  if(r<0.0f)
    r=0.0f;
  if(r>1.0f)
    r=1.0f;
  return r;
}

void PLUGIN_random(SoundPlugin *plugin){
  const SoundPluginType *type = plugin->type;
  int i;

  struct Patch *patch = plugin->patch;
  R_ASSERT_RETURN_IF_FALSE(patch!=NULL);

  //if (type->num_effects==0)
  //  return;
  
  ADD_UNDO(AudioEffect_CurrPos((struct Patch*)patch, -1, AE_ALWAYS_CREATE_SOLO_AND_BYPASS_UNDO));

  float values[type->num_effects+NUM_SYSTEM_EFFECTS];
  for(i=0;i<type->num_effects+NUM_SYSTEM_EFFECTS;i++)
    if (plugin->do_random_change[i])
      values[i]=get_rand();
  
  {
    radium::PlayerLockOnlyIfNeeded lock; // To avoid relocking for every effect.
    for(i=0;i<type->num_effects+NUM_SYSTEM_EFFECTS;i++){
      if (plugin->do_random_change[i]){
        lock.maybe_pause_or_lock(i);
        PLUGIN_set_effect_value(plugin, 0, i, values[i], STORE_VALUE, FX_single, EFFECT_FORMAT_SCALED);
      }
    }
  }
}

// plugin can be NULL here.
void PLUGIN_show_info_window(const SoundPluginType *type, SoundPlugin *plugin, int64_t parentgui){
  QString info;

  bool uses_html = false;
  
  if(type->info!=NULL) {
    info = type->info;
    if (info.startsWith("HTML: ")){
      uses_html = true;
      info = info.mid(5);
    }
  } else {
    if(!strcmp(type->type_name,type->name))
      info = type->type_name;
    else
      info = QString(type->type_name) + ": " + type->name;
  }

  QString ls = uses_html ? "<br>" : "\n";
  
  info += ls + ls;
      
  info += "Inputs: " + QString::number(type->num_inputs) + ls;
  info += "Outputs: " + QString::number(type->num_outputs) + ls;

  if (plugin != NULL){

    double latency = 0.0;
    double tail = -1;

    if (type->RT_get_latency != NULL || type->RT_get_audio_tail_length != NULL) {
      PLAYER_lock();{
        if (type->RT_get_latency != NULL)
          latency = type->RT_get_latency(plugin);
        
        if (type->RT_get_audio_tail_length != NULL)
          tail = type->RT_get_audio_tail_length(plugin);
      }PLAYER_unlock();
    }

    info += "Latency: " + QString::number(latency*1000/MIXER_get_sample_rate()) + ls;
    info += "Audio tail: " + (tail < 0 ? "undefined" : QString::number(tail*1000.0/MIXER_get_sample_rate()) + "ms") + ls;

    double time_since_last_activity = MIXER_get_last_used_time() - ATOMIC_GET_RELAXED(plugin->_RT_time_of_last_activity);
    info += "Last activity: " + QString::number(time_since_last_activity*1000.0/MIXER_get_sample_rate()) + "ms ago" + ls;

    if (isLegalFilepath(plugin->preset_filename))
      info += "Preset file: \"" + STRING_get_qstring(plugin->preset_filename.id) + "\"." + ls;
  }

  MyQMessageBox *infoBox = MyQMessageBox::create(false, API_gui_get_parentwidget(NULL, parentgui));
  infoBox->setWindowTitle("Instrument info");
                          
  infoBox->setAttribute(Qt::WA_DeleteOnClose);
  
  infoBox->setText(info);

  infoBox->setWindowModality(Qt::NonModal);

  safeShow(infoBox);
}

bool g_curr_song_contains_embedded_samples = false;

filepath_t PLUGIN_DISK_get_audio_filename(hash_t *state){
  /*
  if (HASH_has_key(state, "filename2")){
    const wchar_t *ret = HASH_get_filepath(state, "filename2").id;
    printf("\n\n\n.................... RET: -%S- -%S-\n\n\n", ret, HASH_get_filepath(state, "filename2").id);
    return make_filepath(ret);
  }
  */
  bool audiodata_is_included = HASH_has_key(state, "audiofile");

  filepath_t filename = createIllegalFilepath();
  filepath_t org_filename = HASH_get_filepath(state, "filename");

  if (audiodata_is_included){

    g_curr_song_contains_embedded_samples = true;

    if (dc.has_deleted_files_in_embedded_dir==false){
      DISK_delete_all_files_in_dir(dc.embedded_files_dirname);
      dc.has_deleted_files_in_embedded_dir=true;
    }
    
    if (DISK_create_dir(dc.embedded_files_dirname)==false){
      if (dc.has_shown_embedded_files_dirname_warning==false){
        GFX_Message(NULL, "Unable to create directory \"%S\"", dc.embedded_files_dirname.id);
        dc.has_shown_embedded_files_dirname_warning = true;
      }
    }else{      
      filename = appendFilePaths(dc.embedded_files_dirname,
                                 make_filepath(QFileInfo(STRING_get_qstring(org_filename.id)).fileName())
                                 );
      filename = DISK_create_non_existant_filename(filename);
    }

    filename = DISK_base64_to_file(filename, HASH_get_chars(state, "audiofile"));
    if (isIllegalFilepath(filename))
      filename = DISK_base64_to_file(filename, HASH_get_chars(state, "audiofile"));

  } else {

    filename = org_filename;

  }
  
  R_ASSERT(isLegalFilepath(filename));
  
  return filename;
}
