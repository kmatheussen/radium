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
#include "system_compressor_wrapper_proc.h"
#include "audio_instrument_proc.h"
#include "CpuUsage.hpp"
#include "SmoothDelay.hpp"

#include "../api/api_gui_proc.h"


#include "SoundPlugin_proc.h"



#define FILTER_MIN_DB -35
#define FILTER_MAX_DB 35

#define MIN_FREQ 50
#define MAX_FREQ 20000

#define DELAY_MIN 0.0f
#define DELAY_MAX 50.0f


struct SoundPluginEffectMidiLearn final : public MidiLearn {
    
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
    :plugin(plugin)
    {
      init_from_state(state);
    }

  /*
    virtual ~SoundPluginEffectMidiLearn(){
    printf("  SoundPluginEffectMidiLearn destructor called\n");
    }
  */
    
  hash_t *create_state(void){
    hash_t *state = MidiLearn::create_state();
    HASH_put_int(state, "SoundPluginEffectMidiLearn::effect_num", effect_num);
    return state;
  }

  void init_from_state(hash_t *state){
    MidiLearn::init_from_state(state);
    effect_num = HASH_get_int32(state, "SoundPluginEffectMidiLearn::effect_num");
  }

  virtual int64_t RT_get_instrument_id(void) override {
    volatile struct Patch *patch = plugin->patch;
    if (patch==NULL)
      return -2;
    else
      return patch->id;
  }
  
  virtual QString get_dest_info(void) override;
  virtual void delete_me(void) override;
  virtual void RT_callback(float val) override;
  
  virtual bool RT_get_automation_recording_data(SoundPlugin **plugin, int *effect_num) override{

    if (PLUGIN_is_recording_automation(this->plugin, this->effect_num)==false)
      return false;
                   
    *plugin = this->plugin;
    *effect_num = this->effect_num;

    return true;
  }
};

static void add_midi_learn(SoundPluginEffectMidiLearn *midi_learn){
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

static const float threshold_val = 0.05f;

static float slider_2_gain(float val, const float db_min, const float db_max){
  if(val<=0.0f)
    return 0.0f;

  if(val <= threshold_val){ // Below threshold, we do a linear conversion. If not there will be a jump between 0 and almost 0.
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

static float gain_2_slider(float gain, float db_min, float db_max){
  if(gain==0.0f)
    return 0.0f;

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
  
  "System Reverb On/Off",
  "System Chorus On/Off",
  "System Aux 1 On/Off",
  "System Aux 2 On/Off",
  "System Aux 3 On/Off",

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

SoundPlugin *PLUGIN_create(SoundPluginType *plugin_type, hash_t *plugin_state, bool is_loading){
  printf("PLUGIN_create called\n");
  
  SoundPlugin *plugin = (SoundPlugin*)V_calloc(1,sizeof(SoundPlugin));
  plugin->type = plugin_type;

  ATOMIC_SET(plugin->auto_suspend_behavior, DEFAULT_AUTOSUSPEND_BEHAVIOR);
  PLUGIN_touch(plugin);
    
  ATOMIC_SET(plugin->effect_num_to_show_because_it_was_used_externally, -1);
  
  int buffer_size = MIXER_get_buffer_size();

  plugin->midi_learns = new radium::Vector<SoundPluginEffectMidiLearn*>;

  // TODO: Don't do this. Check if all plugins can be initialized later.
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

  plugin->automation_values = (float*)V_calloc(sizeof(float),plugin_type->num_effects+NUM_SYSTEM_EFFECTS);
  for(int e = 0 ; e<plugin_type->num_effects+NUM_SYSTEM_EFFECTS ; e++)
    plugin->automation_values[e] = -10;

  
  SMOOTH_init(&plugin->input_volume  , 1.0f, buffer_size);
  plugin->output_volume = 1.0f;
  SMOOTH_init(&plugin->pan           , 0.5f, buffer_size);
  SMOOTH_init(&plugin->drywet        , 1.0f, buffer_size);

  ATOMIC_SET(plugin->input_volume_is_on, true);
  ATOMIC_SET(plugin->output_volume_is_on, true);
  ATOMIC_SET_ARRAY(plugin->bus_volume_is_on, 0, false);
  ATOMIC_SET_ARRAY(plugin->bus_volume_is_on, 1, false);
  ATOMIC_SET(plugin->effects_are_on, true);
             
  plugin->volume = 1.0f;
  ATOMIC_SET(plugin->volume_is_on, true);


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
      plugin->delay = new radium::SmoothDelay(DELAY_MAX * MIXER_get_sample_rate() / 1000);
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

  {
    int i;
    plugin->savable_effect_values=(float*)V_calloc(sizeof(float),plugin_type->num_effects+NUM_SYSTEM_EFFECTS);

#if 0
    for(i=0;i<plugin_type->num_effects+NUM_SYSTEM_EFFECTS;i++)
      plugin->savable_effect_values[i] = PLUGIN_get_effect_value(plugin,i,VALUE_FROM_PLUGIN);
#else
    for(i=0;i<plugin_type->num_effects;i++)
      plugin->savable_effect_values[i] = PLUGIN_get_effect_value(plugin,i,VALUE_FROM_PLUGIN);

    plugin->savable_effect_values[plugin_type->num_effects+EFFNUM_OUTPUT_VOLUME] = 1.0f;

    for(i=plugin_type->num_effects;i<plugin_type->num_effects+NUM_SYSTEM_EFFECTS;i++){
      float value = PLUGIN_get_effect_value(plugin,i,VALUE_FROM_PLUGIN);
      PLUGIN_set_native_effect_value(plugin, 0, i, value, PLUGIN_NONSTORED_TYPE, PLUGIN_STORE_VALUE, FX_single);
    }
#endif
  }

  {
    plugin->initial_effect_values=(float*)V_calloc(sizeof(float),plugin_type->num_effects+NUM_SYSTEM_EFFECTS);
    memcpy(plugin->initial_effect_values, plugin->savable_effect_values, sizeof(float) * (plugin_type->num_effects+NUM_SYSTEM_EFFECTS));
  }

  {
    plugin->do_random_change = (bool*)V_calloc(sizeof(bool), plugin_type->num_effects);
    for(int i=0;i<plugin_type->num_effects;i++)
      plugin->do_random_change[i] = true;
  }

  if (plugin_type->called_after_plugin_has_been_created != NULL)    
    plugin_type->called_after_plugin_has_been_created(plugin_type, plugin);

  PLUGIN_touch(plugin);
  
  return plugin;
}

void PLUGIN_delete(SoundPlugin *plugin){
  RT_PLUGIN_touch(plugin);
  
  const SoundPluginType *plugin_type = plugin->type;

  while(PLUGIN_remove_midi_learn(plugin, -1, false)==true);
       
  plugin_type->cleanup_plugin_data(plugin);


  //if(!strcmp(plugin_type->type_name,"Bus")) // RT_process needs buses to always be alive. No, not any more. Now, buses are just like other soundproducers in the mixer.
  //  return;

  
  V_free(plugin->do_random_change);
  V_free(plugin->initial_effect_values);
  V_free(plugin->savable_effect_values);

  
  SMOOTH_release(&plugin->input_volume);
    
  SMOOTH_release(&plugin->pan);
  
  SMOOTH_release(&plugin->drywet);
  
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
  V_free(plugin->automation_values);

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
    
  memset(plugin,-1,sizeof(SoundPlugin)); // for debugging. Crashes faster if something is wrong.
  V_free(plugin);
}

// Called at the start of each block
void PLUGIN_update_smooth_values(SoundPlugin *plugin){
  SMOOTH_called_per_block(&plugin->input_volume);
  SMOOTH_called_per_block(&plugin->pan);
  SMOOTH_called_per_block(&plugin->drywet);
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

int PLUGIN_get_effect_num(struct SoundPlugin *plugin, const char *effect_name){
  const struct SoundPluginType *plugin_type = plugin->type;

  int i;
  for(i=0;i<NUM_SYSTEM_EFFECTS;i++)
    if(!strcmp(system_effect_names[i],effect_name))
      return i + plugin_type->num_effects;

  for(i=0;i<plugin_type->num_effects;i++)
    if(!strcmp(effect_name,plugin_type->get_effect_name(plugin,i)))
      return i;

  GFX_Message(NULL, "The effect names of %s / %s has changed.\n\"%s\" will be ignored.\n\nIf you know the new name of the effect, you can edit the song manually in a text editor.", plugin_type->type_name, plugin_type->name, effect_name);
  
  return -1;
}

const char *PLUGIN_get_effect_name(SoundPlugin *plugin, int effect_num){
  const struct SoundPluginType *plugin_type = plugin->type;

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

static void set_db_display(char *buffer, int buffersize, float value){
  float db = gain_2_db(value,MIN_DB,MAX_DB);
  
  if(db==MIN_DB)
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
    return plugin->type->get_display_value_string(plugin, effect_num, buffer, buffersize);

  float store_value = safe_float_read(&plugin->savable_effect_values[effect_num]);
    
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
      set_db_display(buffer,buffersize,store_value);
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

static float get_general_gain_store_value(float value, enum ValueType value_type, bool is_filter){
  if(value_type==PLUGIN_STORED_TYPE)
    return value; // When saving to disk, we store the gain, not the slider value. We do this to avoid having to do conversions if we later change the slider scale.
  else if(is_filter==true)
    return scale(value, 0, 1, FILTER_MIN_DB, FILTER_MAX_DB);
  else
    return slider_2_gain(value, MIN_DB, MAX_DB);
}

static float get_gain_store_value(float value, enum ValueType value_type){
  return get_general_gain_store_value(value,value_type,false);
}

static float get_filter_gain_store_value(float value, enum ValueType value_type){
  return get_general_gain_store_value(value,value_type,true);
}

static float get_freq_store_value(float value, enum ValueType value_type){
  if(value_type==PLUGIN_STORED_TYPE)
    return value; // For the same reason here. We store Hz, not slider value.
  else
    return slider_2_frequency(value,MIN_FREQ,MAX_FREQ);
}

static void update_instrument_gui(struct SoundPlugin *plugin){
  if (plugin->patch != NULL) {
    GFX_ScheduleInstrumentRedraw((struct Patch*)plugin->patch);
  }
}

static void set_voice_onoff(struct SoundPlugin *plugin, int num, float value){
  if (plugin->patch != NULL) {
    if (value>=0.5)
      plugin->patch->voices[num].is_on = true;
    else
      plugin->patch->voices[num].is_on = false;

    //printf("      Setting voice %d to %d\n", num, plugin->patch->voices[num].is_on );
    
    update_instrument_gui(plugin);
  }
}
                      
static bool get_voice_onoff(struct SoundPlugin *plugin, int num){
  if (plugin->patch != NULL) {
    return plugin->patch->voices[num].is_on;
  } else
    return num==0;
}
                      
static void set_chance(struct SoundPlugin *plugin, int num, float value){
  if (plugin->patch != NULL) {
    if (value>=1)
      plugin->patch->voices[num].chance = 256;
    else
      plugin->patch->voices[num].chance = R_BOUNDARIES(0, round(scale_double(value, 0, 1, 0, 256)), 256);
    
    update_instrument_gui(plugin);
  }
}
                      
static float get_chance(struct SoundPlugin *plugin, int num){
  if (plugin->patch != NULL) {
    return round(scale_double(plugin->patch->voices[num].chance, 0, 256, 0, 1));
  } else
    return 1;
}
                      
#define SET_SMOOTH_ON_OFF(smooth, on_off, value, set_value) {   \
    if(value>0.5f){                                             \
      SMOOTH_set_target_value(smooth, set_value);               \
      ATOMIC_SET(on_off, true);                                 \
    }else{                                                      \
      ATOMIC_SET(on_off, false);                                \
      SMOOTH_set_target_value(smooth, 0.0f);                    \
    }                                                           \
  }

#define SET_BUS_VOLUME(busnum)                                          \
  store_value = get_gain_store_value(value,value_type);                 \
  safe_float_write(&plugin->bus_volume[busnum], store_value);           \
  break;

static void set_bus_onoff(struct SoundPlugin *plugin, float value, int busnum){
  bool newval = value > 0.5f;
  bool oldval = ATOMIC_SET_RETURN_OLD_ARRAY(plugin->bus_volume_is_on, busnum, newval);
  if (oldval != newval){
    volatile struct Patch *patch = plugin->patch;
    if (patch==NULL)
      RT_schedule_mixer_strips_remake(-1);
    else
      RT_schedule_mixer_strips_remake(patch->id);
#if !defined(RELEASE)
    printf("       Remake: BUS_ONOFF %d. On: %d\n", busnum, newval);
#endif
  }
}

void PLUGIN_set_effect_value2(struct SoundPlugin *plugin, int time, int effect_num, float value, enum ValueType value_type, enum SetValueType set_type, FX_when when, enum ValueFormat value_format, bool sent_from_midi_learn){
  float store_value = value;
  //printf("set effect value. effect_num: %d, value: %f, num_effects: %d\n",effect_num,value,plugin->type->num_effects);

  RT_PLUGIN_touch(plugin);
        
  if(value_format==PLUGIN_FORMAT_SCALED && value_type != PLUGIN_STORED_TYPE) { // Messy. I think the thing is that PLUGIN_STORED_TYPE trumps PLUGIN_FORMAT_SCALED. I.e. value_format doesn't matter if value_type is PLUGIN_STORED_TYPE.
#if !defined(RELEASE)
    if (value < -0.01f || value > 1.01f)// don't report floating point rounding errors
      RWarning("value: %f", value);
#endif
    value = R_BOUNDARIES(0.0f, value, 1.0f);
  }      
  
  if(effect_num < plugin->type->num_effects){

    {
      radium::PlayerRecursiveLock lock;
    
      plugin->type->set_effect_value(plugin,time,effect_num,value,value_format,when);

      if (value_format==PLUGIN_FORMAT_NATIVE)
        store_value = plugin->type->get_effect_value(plugin, effect_num, PLUGIN_FORMAT_SCALED);
    }
    
    if (PLUGIN_is_recording_automation(plugin, effect_num) && sent_from_midi_learn==false && when==FX_single)
      MIDI_add_automation_recording_event(plugin, effect_num, store_value);
    
  }else{

    if (PLUGIN_is_recording_automation(plugin, effect_num) && sent_from_midi_learn==false && when==FX_single)
      MIDI_add_automation_recording_event(plugin, effect_num, value);

    int num_effects = plugin->type->num_effects;
    int system_effect = effect_num - num_effects;
    int ch;

    switch(system_effect){
    case EFFNUM_INPUT_VOLUME:
      store_value = get_gain_store_value(value,value_type);
      if(ATOMIC_GET(plugin->input_volume_is_on)==true)
        SMOOTH_set_target_value(&plugin->input_volume, store_value);
      break;
    case EFFNUM_INPUT_VOLUME_ONOFF:
      SET_SMOOTH_ON_OFF(&plugin->input_volume, plugin->input_volume_is_on, store_value, plugin->savable_effect_values[num_effects+EFFNUM_INPUT_VOLUME]);
      break;

    case EFFNUM_SOLO_ONOFF:
      if(value>0.5f) {
        ATOMIC_SET(plugin->solo_is_on, true);
      }else {
        ATOMIC_SET(plugin->solo_is_on, false);
      }
      update_instrument_gui(plugin);
      //RT_schedule_mixer_strips_remake();
      break;

    case EFFNUM_VOLUME:
      store_value = get_gain_store_value(value,value_type);
      if (ATOMIC_GET(plugin->volume_is_on))
        plugin->volume = store_value;
      break;
    case EFFNUM_VOLUME_ONOFF:
      if(value>0.5f) {
        plugin->volume = plugin->savable_effect_values[num_effects+EFFNUM_VOLUME];
        ATOMIC_SET(plugin->volume_is_on, true);
      }else {
        ATOMIC_SET(plugin->volume_is_on, false);
        plugin->volume = 0.0f;
      }
      //update_instrument_gui(plugin);
      break;

    case EFFNUM_OUTPUT_VOLUME:
      store_value = get_gain_store_value(value,value_type);
      //printf("***PLUGIN_SET_EFFE_CT_FALUE. ****** store_value: %f\n",store_value);
      plugin->output_volume = store_value;
      break;
      
    case EFFNUM_OUTPUT_VOLUME_ONOFF:
      if (value > 0.5f)
        ATOMIC_SET(plugin->output_volume_is_on, true);
      else
        ATOMIC_SET(plugin->output_volume_is_on, false);
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
      
    case EFFNUM_BUS1_ONOFF:
      set_bus_onoff(plugin, value, 0);
      break;
    case EFFNUM_BUS2_ONOFF:
      set_bus_onoff(plugin, value, 1);
      break;
    case EFFNUM_BUS3_ONOFF:
      set_bus_onoff(plugin, value, 2);
      break;
      break;
    case EFFNUM_BUS4_ONOFF:
      set_bus_onoff(plugin, value, 3);
      break;
    case EFFNUM_BUS5_ONOFF:
      set_bus_onoff(plugin, value, 4);
      break;
    case EFFNUM_PAN:
      if(ATOMIC_GET(plugin->pan_is_on)==true)
        SMOOTH_set_target_value(&plugin->pan, value);
      break;
    case EFFNUM_PAN_ONOFF:
      if(value>0.5f){
        SMOOTH_set_target_value(&plugin->pan, plugin->savable_effect_values[plugin->type->num_effects+EFFNUM_PAN]);
        ATOMIC_SET(plugin->pan_is_on, true);
      }else{
        ATOMIC_SET(plugin->pan_is_on, false);
        SMOOTH_set_target_value(&plugin->pan, 0.5f);
      }
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
      if(value>0.5f){
        SMOOTH_set_target_value(&plugin->drywet, plugin->savable_effect_values[plugin->type->num_effects+EFFNUM_DRYWET]);
        ATOMIC_SET(plugin->effects_are_on, true);
      }else{
        ATOMIC_SET(plugin->effects_are_on, false);
        SMOOTH_set_target_value(&plugin->drywet, 0.0f);
      }
      //update_instrument_gui(plugin);
      break;
      
    case EFFNUM_LOWPASS_FREQ:
      store_value = get_freq_store_value(value, value_type);
      plugin->lowpass_freq = store_value;
      for(ch=0;ch<plugin->type->num_outputs;ch++)
        plugin->lowpass.plugins[ch]->type->set_effect_value(plugin->lowpass.plugins[ch], time, 0, store_value, PLUGIN_FORMAT_NATIVE, when);
      break;
    case EFFNUM_LOWPASS_ONOFF:
      ATOMIC_SET(plugin->lowpass.is_on, value > 0.5f);
      break;

    case EFFNUM_HIGHPASS_FREQ:
      store_value = get_freq_store_value(value, value_type);
      plugin->highpass_freq = store_value;
      for(ch=0;ch<plugin->type->num_outputs;ch++)
        plugin->highpass.plugins[ch]->type->set_effect_value(plugin->highpass.plugins[ch], time, 0, store_value, PLUGIN_FORMAT_NATIVE, when);
      break;
    case EFFNUM_HIGHPASS_ONOFF:
      ATOMIC_SET(plugin->highpass.is_on, value > 0.5f);
      break;

    case EFFNUM_EQ1_FREQ:
      store_value = get_freq_store_value(value, value_type);
      plugin->eq1_freq = store_value;
      for(ch=0;ch<plugin->type->num_outputs;ch++)
        plugin->eq1.plugins[ch]->type->set_effect_value(plugin->eq1.plugins[ch], time, 0, store_value, PLUGIN_FORMAT_NATIVE, when);
      break;
    case EFFNUM_EQ1_GAIN:
      store_value = get_filter_gain_store_value(value, value_type);
      plugin->eq1_db = store_value;
      for(ch=0;ch<plugin->type->num_outputs;ch++)
        plugin->eq1.plugins[ch]->type->set_effect_value(plugin->eq1.plugins[ch], time, 1, store_value, PLUGIN_FORMAT_NATIVE, when);
      break;
    case EFFNUM_EQ1_ONOFF:
      ATOMIC_SET(plugin->eq1.is_on, store_value > 0.5f);
      break;
      
    case EFFNUM_EQ2_FREQ:
      store_value = get_freq_store_value(value,value_type);
      plugin->eq2_freq = store_value;
      for(ch=0;ch<plugin->type->num_outputs;ch++)
        plugin->eq2.plugins[ch]->type->set_effect_value(plugin->eq2.plugins[ch], time, 0, store_value, PLUGIN_FORMAT_NATIVE, when);
      break;
    case EFFNUM_EQ2_GAIN:
      store_value = get_filter_gain_store_value(value,value_type);
      plugin->eq2_db = store_value;
      for(ch=0;ch<plugin->type->num_outputs;ch++)
        plugin->eq2.plugins[ch]->type->set_effect_value(plugin->eq2.plugins[ch], time, 1, store_value, PLUGIN_FORMAT_NATIVE, when);
      break;
    case EFFNUM_EQ2_ONOFF:
      ATOMIC_SET(plugin->eq2.is_on, store_value > 0.5f);
      break;
      
    case EFFNUM_LOWSHELF_FREQ:
      store_value = get_freq_store_value(value,value_type);
      plugin->lowshelf_freq = store_value;
      for(ch=0;ch<plugin->type->num_outputs;ch++)
        plugin->lowshelf.plugins[ch]->type->set_effect_value(plugin->lowshelf.plugins[ch], time, 0, store_value, PLUGIN_FORMAT_NATIVE, when);
      break;
    case EFFNUM_LOWSHELF_GAIN:
      store_value = get_filter_gain_store_value(value,value_type);
      plugin->lowshelf_db = store_value;
      for(ch=0;ch<plugin->type->num_outputs;ch++)
        plugin->lowshelf.plugins[ch]->type->set_effect_value(plugin->lowshelf.plugins[ch], time, 1, store_value, PLUGIN_FORMAT_NATIVE, when);
      break;
    case EFFNUM_LOWSHELF_ONOFF:
      ATOMIC_SET(plugin->lowshelf.is_on, store_value > 0.5f);
      break;
      
    case EFFNUM_HIGHSHELF_FREQ:
      store_value = get_freq_store_value(value,value_type);
      plugin->highshelf_freq = store_value;
      for(ch=0;ch<plugin->type->num_outputs;ch++)
        plugin->highshelf.plugins[ch]->type->set_effect_value(plugin->highshelf.plugins[ch], time, 0, store_value, PLUGIN_FORMAT_NATIVE, when);
      break;
    case EFFNUM_HIGHSHELF_GAIN:
      store_value = get_filter_gain_store_value(value,value_type);
      plugin->highshelf_db = store_value;
      for(ch=0;ch<plugin->type->num_outputs;ch++)
        plugin->highshelf.plugins[ch]->type->set_effect_value(plugin->highshelf.plugins[ch], time, 1, store_value, PLUGIN_FORMAT_NATIVE, when);
      break;
    case EFFNUM_HIGHSHELF_ONOFF:
      ATOMIC_SET(plugin->highshelf.is_on, store_value > 0.5f);
      break;

    case EFFNUM_EQ_SHOW_GUI:
      plugin->show_equalizer_gui = store_value > 0.5f;
      update_instrument_gui(plugin);
      break;

    case EFFNUM_BROWSER_SHOW_GUI:
      plugin->show_browser_gui = store_value > 0.5f;
      update_instrument_gui(plugin);
      break;

    case EFFNUM_CONTROLS_SHOW_GUI:
      plugin->show_controls_gui = store_value > 0.5f;
      update_instrument_gui(plugin);
      break;
      
    case EFFNUM_VOICE1_ONOFF:
      set_voice_onoff(plugin, 0, store_value);
      break;
    case EFFNUM_VOICE2_ONOFF:
      set_voice_onoff(plugin, 1, store_value);
      break;
    case EFFNUM_VOICE3_ONOFF:
      set_voice_onoff(plugin, 2, store_value);
      break;
    case EFFNUM_VOICE4_ONOFF:
      set_voice_onoff(plugin, 3, store_value);
      break;
    case EFFNUM_VOICE5_ONOFF:
      set_voice_onoff(plugin, 4, store_value);
      break;
    case EFFNUM_VOICE6_ONOFF:
      set_voice_onoff(plugin, 5, store_value);
      break;
    case EFFNUM_VOICE7_ONOFF:
      set_voice_onoff(plugin, 6, store_value);
      break;
      
    case EFFNUM_CHANCE1:
      set_chance(plugin, 0, store_value);
      break;
    case EFFNUM_CHANCE2:
      set_chance(plugin, 1, store_value);
      break;
    case EFFNUM_CHANCE3:
      set_chance(plugin, 2, store_value);
      break;
    case EFFNUM_CHANCE4:
      set_chance(plugin, 3, store_value);
      break;
    case EFFNUM_CHANCE5:
      set_chance(plugin, 4, store_value);
      break;
    case EFFNUM_CHANCE6:
      set_chance(plugin, 5, store_value);
      break;
    case EFFNUM_CHANCE7:
      set_chance(plugin, 6, store_value);
      break;
      
      // fix. Must call GUI function, and then the GUI function calls COMPRESSOR_set_parameter.
    case EFFNUM_COMP_RATIO:
      //printf("Setting ratio to %f\n",store_value);
      COMPRESSOR_set_parameter(plugin->compressor, COMP_EFF_RATIO, store_value);
      break;
    case EFFNUM_COMP_THRESHOLD:
      COMPRESSOR_set_parameter(plugin->compressor, COMP_EFF_THRESHOLD, store_value);
      break;
    case EFFNUM_COMP_ATTACK:
      COMPRESSOR_set_parameter(plugin->compressor, COMP_EFF_ATTACK, store_value);
      break;
    case EFFNUM_COMP_RELEASE:
      COMPRESSOR_set_parameter(plugin->compressor, COMP_EFF_RELEASE, store_value);
      break;
    case EFFNUM_COMP_OUTPUT_VOLUME:
      COMPRESSOR_set_parameter(plugin->compressor, COMP_EFF_OUTPUT_VOLUME, store_value);
      break;
    case EFFNUM_COMP_ONOFF:
      ATOMIC_SET(plugin->comp.is_on, store_value > 0.5f);
      //printf("storing comp. %d %f %f\n",plugin->comp.is_on,store_value,value);
      break;

    case EFFNUM_COMP_SHOW_GUI:
      plugin->show_compressor_gui = store_value > 0.5f;
      update_instrument_gui(plugin);
      break;

    case EFFNUM_DELAY_TIME:
      {
        radium::PlayerRecursiveLock lock;
        
        store_value = value_type==PLUGIN_STORED_TYPE ? value : scale(value, 0, 1, DELAY_MIN, DELAY_MAX);
        plugin->delay_time = store_value;
        if (plugin->delay != NULL){
          int val = 0;
          if (ATOMIC_GET(plugin->delay_is_on))
            val = plugin->delay_time*MIXER_get_sample_rate()/1000;
          static_cast<radium::SmoothDelay*>(plugin->delay)->setSize(val);
        }
        break;
      }
    case EFFNUM_DELAY_ONOFF:
      {
        radium::PlayerRecursiveLock lock;
        
        ATOMIC_SET(plugin->delay_is_on, store_value > 0.5f);
        if (plugin->delay != NULL){
          int val = 0;
          if (ATOMIC_GET(plugin->delay_is_on))
            val = plugin->delay_time*MIXER_get_sample_rate()/1000;
          static_cast<radium::SmoothDelay*>(plugin->delay)->setSize(val);
        }
        break;
      }
      
    default:
      RError("2. Unknown effect number: %d (%d). %s / %s",effect_num,system_effect,plugin->type->type_name,plugin->type->name);
    }  
  }

  if(set_type==PLUGIN_STORE_VALUE)
    plugin->savable_effect_values[effect_num] = store_value;
}

float PLUGIN_get_effect_value(struct SoundPlugin *plugin, int effect_num, enum WhereToGetValue where){

  //RT_PLUGIN_touch(plugin);
    
#if !defined(RELEASE)
  if (where==VALUE_FROM_STORAGE)
    if (!THREADING_is_player_thread())
      if (PLAYER_current_thread_has_lock())
        abort();
  
  //if (where==VALUE_FROM_PLUGIN)
  //  R_ASSERT(PLAYER_current_thread_has_lock());
#endif

  if(effect_num >= plugin->type->num_effects + NUM_SYSTEM_EFFECTS){
    RError("Illegal effect_num %d",effect_num);
    return 0.0f;
  }

  float store_value = safe_float_read(&plugin->savable_effect_values[effect_num]);
  
  if(effect_num < plugin->type->num_effects) {

    if(where==VALUE_FROM_PLUGIN || plugin->type->plugin_takes_care_of_savable_values==true)
      return plugin->type->get_effect_value(plugin,effect_num,PLUGIN_FORMAT_SCALED);
    else
      return store_value;
  }

  int system_effect = effect_num - plugin->type->num_effects;

  if (where == VALUE_FROM_STORAGE) {
    switch(system_effect){
    case EFFNUM_INPUT_VOLUME:
    case EFFNUM_VOLUME:
    case EFFNUM_OUTPUT_VOLUME:
      
    case EFFNUM_BUS1:
    case EFFNUM_BUS2:
    case EFFNUM_BUS3:
    case EFFNUM_BUS4:
    case EFFNUM_BUS5:
      return gain_2_slider(store_value,
                           MIN_DB, MAX_DB);
      
    case EFFNUM_LOWPASS_FREQ:
    case EFFNUM_HIGHPASS_FREQ:
    case EFFNUM_EQ1_FREQ:
    case EFFNUM_EQ2_FREQ:
    case EFFNUM_LOWSHELF_FREQ:
    case EFFNUM_HIGHSHELF_FREQ:
      return frequency_2_slider(store_value,MIN_FREQ,MAX_FREQ);
      
    case EFFNUM_EQ1_GAIN:
    case EFFNUM_EQ2_GAIN:
    case EFFNUM_LOWSHELF_GAIN:
    case EFFNUM_HIGHSHELF_GAIN:
      return scale(store_value,FILTER_MIN_DB,FILTER_MAX_DB,0,1);
      
    case EFFNUM_DELAY_TIME:
      return scale(store_value,DELAY_MIN,DELAY_MAX,0,1);

    default:
      return store_value;
    }
  }

  switch(system_effect){
  case EFFNUM_INPUT_VOLUME:
    return gain_2_slider(SMOOTH_get_target_value(&plugin->input_volume),
                         MIN_DB, MAX_DB);
  case EFFNUM_INPUT_VOLUME_ONOFF:
    return ATOMIC_GET(plugin->input_volume_is_on)==true ? 1.0 : 0.0f;

  case EFFNUM_SOLO_ONOFF:
    return ATOMIC_GET(plugin->solo_is_on)==true ? 1.0 : 0.0f;

  case EFFNUM_VOLUME:
    return gain_2_slider(plugin->volume, MIN_DB, MAX_DB);
  case EFFNUM_VOLUME_ONOFF:
    return ATOMIC_GET(plugin->volume_is_on)==true ? 1.0 : 0.0f;

  case EFFNUM_OUTPUT_VOLUME:
    {
      float val = gain_2_slider(plugin->output_volume, MIN_DB, MAX_DB);
      //printf(">>>>>>>>>>>>>>>>>>>>>>>>> Get output volume. return val: %f. Target value: %f\n",val, plugin->output_volume.target_value);
      return val;
    }
    
  case EFFNUM_OUTPUT_VOLUME_ONOFF:
    return ATOMIC_GET(plugin->output_volume_is_on)==true ? 1.0 : 0.0f;

  case EFFNUM_BUS1:
    return gain_2_slider(plugin->bus_volume[0], MIN_DB, MAX_DB);
  case EFFNUM_BUS2:
    return gain_2_slider(plugin->bus_volume[1], MIN_DB, MAX_DB);
  case EFFNUM_BUS3:
    return gain_2_slider(plugin->bus_volume[2], MIN_DB, MAX_DB);
  case EFFNUM_BUS4:
    return gain_2_slider(plugin->bus_volume[3], MIN_DB, MAX_DB);
  case EFFNUM_BUS5:
    return gain_2_slider(plugin->bus_volume[4], MIN_DB, MAX_DB);
    
  case EFFNUM_BUS1_ONOFF:
    return ATOMIC_GET_ARRAY(plugin->bus_volume_is_on, 0)==true ? 1.0 : 0.0f;
  case EFFNUM_BUS2_ONOFF:
    return ATOMIC_GET_ARRAY(plugin->bus_volume_is_on, 1)==true ? 1.0 : 0.0f;
  case EFFNUM_BUS3_ONOFF:
    return ATOMIC_GET_ARRAY(plugin->bus_volume_is_on, 2)==true ? 1.0 : 0.0f;
  case EFFNUM_BUS4_ONOFF:
    return ATOMIC_GET_ARRAY(plugin->bus_volume_is_on, 3)==true ? 1.0 : 0.0f;
  case EFFNUM_BUS5_ONOFF:
    return ATOMIC_GET_ARRAY(plugin->bus_volume_is_on, 4)==true ? 1.0 : 0.0f;

  case EFFNUM_PAN:
    return SMOOTH_get_target_value(&plugin->pan);
  case EFFNUM_PAN_ONOFF:
    return ATOMIC_GET(plugin->pan_is_on)==true ? 1.0 : 0.0f;

  case EFFNUM_DRYWET:
    return SMOOTH_get_target_value(&plugin->drywet);
  case EFFNUM_EFFECTS_ONOFF:
    return ATOMIC_GET(plugin->effects_are_on)==true ? 1.0 : 0.0f;

  case EFFNUM_LOWPASS_FREQ:
    return frequency_2_slider(plugin->lowpass_freq,MIN_FREQ,MAX_FREQ);
  case EFFNUM_LOWPASS_ONOFF:
    return ATOMIC_GET(plugin->lowpass.is_on)==true ? 1.0 : 0.0f;

  case EFFNUM_HIGHPASS_FREQ:
    return frequency_2_slider(plugin->highpass_freq,MIN_FREQ,MAX_FREQ);
  case EFFNUM_HIGHPASS_ONOFF:
    return ATOMIC_GET(plugin->highpass.is_on)==true ? 1.0 : 0.0f;

  case EFFNUM_EQ1_FREQ:
    return frequency_2_slider(plugin->eq1_freq,MIN_FREQ,MAX_FREQ);
  case EFFNUM_EQ1_GAIN:
    return scale(plugin->eq1_db,FILTER_MIN_DB,FILTER_MAX_DB,0,1);
  case EFFNUM_EQ1_ONOFF:
    return ATOMIC_GET(plugin->eq1.is_on)==true ? 1.0 : 0.0f;

  case EFFNUM_EQ2_FREQ:
    return frequency_2_slider(plugin->eq2_freq,MIN_FREQ,MAX_FREQ);
  case EFFNUM_EQ2_GAIN:
    return scale(plugin->eq2_db,FILTER_MIN_DB,FILTER_MAX_DB,0,1);
  case EFFNUM_EQ2_ONOFF:
    return ATOMIC_GET(plugin->eq2.is_on)==true ? 1.0 : 0.0f;
    
  case EFFNUM_LOWSHELF_FREQ:
    return frequency_2_slider(plugin->lowshelf_freq,MIN_FREQ,MAX_FREQ);
  case EFFNUM_LOWSHELF_GAIN:
    return scale(plugin->lowshelf_db,FILTER_MIN_DB,FILTER_MAX_DB,0,1);
  case EFFNUM_LOWSHELF_ONOFF:
    return ATOMIC_GET(plugin->lowshelf.is_on)==true ? 1.0 : 0.0f;
    
  case EFFNUM_HIGHSHELF_FREQ:
    return frequency_2_slider(plugin->highshelf_freq,MIN_FREQ,MAX_FREQ);
  case EFFNUM_HIGHSHELF_GAIN:
    return scale(plugin->highshelf_db,FILTER_MIN_DB,FILTER_MAX_DB,0,1);
  case EFFNUM_HIGHSHELF_ONOFF:
    return ATOMIC_GET(plugin->highshelf.is_on)==true ? 1.0 : 0.0f;

  case EFFNUM_EQ_SHOW_GUI:
    return plugin->show_equalizer_gui==true ? 1.0 : 0.0f;
  case EFFNUM_BROWSER_SHOW_GUI:
    return plugin->show_browser_gui==true ? 1.0 : 0.0f;
  case EFFNUM_CONTROLS_SHOW_GUI:
    return plugin->show_controls_gui==true ? 1.0 : 0.0f;

  case EFFNUM_VOICE1_ONOFF:
    return get_voice_onoff(plugin, 0);
  case EFFNUM_VOICE2_ONOFF:
    return get_voice_onoff(plugin, 1);
  case EFFNUM_VOICE3_ONOFF:
    return get_voice_onoff(plugin, 2);
  case EFFNUM_VOICE4_ONOFF:
    return get_voice_onoff(plugin, 3);
  case EFFNUM_VOICE5_ONOFF:
    return get_voice_onoff(plugin, 4);
  case EFFNUM_VOICE6_ONOFF:
    return get_voice_onoff(plugin, 5);
  case EFFNUM_VOICE7_ONOFF:
    return get_voice_onoff(plugin, 6);
    
  case EFFNUM_CHANCE1:
    return get_chance(plugin, 0);
  case EFFNUM_CHANCE2:
    return get_chance(plugin, 1);
  case EFFNUM_CHANCE3:
    return get_chance(plugin, 2);
  case EFFNUM_CHANCE4:
    return get_chance(plugin, 3);
  case EFFNUM_CHANCE5:
    return get_chance(plugin, 4);
  case EFFNUM_CHANCE6:
    return get_chance(plugin, 5);
  case EFFNUM_CHANCE7:
    return get_chance(plugin, 6);
    
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
    return scale(plugin->highshelf_db,DELAY_MIN,DELAY_MAX,0,1);
  case EFFNUM_DELAY_ONOFF:
    return ATOMIC_GET(plugin->delay_is_on)==true ? 1.0 : 0.0f;
#if 0    
  case EFFNUM_EDITOR_ONOFF:
    return ATOMIC_GET(plugin->editor_is_on)==true ? 1.0 : 0.0f;
#endif

  default:
    RError("3. Unknown effect number: %d (%d). %s / %s",effect_num,system_effect,plugin->type->type_name,plugin->type->name);
    return 0.0f;
  }  

}

hash_t *PLUGIN_get_effects_state(SoundPlugin *plugin){
  const SoundPluginType *type=plugin->type;
  hash_t *effects=HASH_create(type->num_effects);

  int i;
  
  for(i=0;i<type->num_effects;i++)
    HASH_put_float(effects, PLUGIN_get_effect_name(plugin,i), type->get_effect_value(plugin, i, PLUGIN_FORMAT_NATIVE)); // Do this so that the plugin can change min/max values between sessions.

  for(i=type->num_effects;i<type->num_effects+NUM_SYSTEM_EFFECTS;i++)
    HASH_put_float(effects, PLUGIN_get_effect_name(plugin,i), plugin->savable_effect_values[i]);

  return effects;
}

void PLUGIN_apply_ab_state(SoundPlugin *plugin, hash_t *state){
  SoundPluginType *type = plugin->type;

  volatile struct Patch *patch = plugin->patch;
  
  ADD_UNDO(AudioEffect_CurrPos((struct Patch*)patch, -1));

  int num_effects = type->num_effects+NUM_SYSTEM_EFFECTS;
      
  hash_t *values_state = HASH_get_hash(state, "values");

  float values[num_effects];
  
  for(int i=0;i<num_effects;i++)
    values[i] = HASH_get_float_at(values_state,"value",i);
  
  PLAYER_lock();{
    for(int i=0;i<num_effects;i++)
      PLUGIN_set_effect_value(plugin, 0, i, values[i], PLUGIN_STORED_TYPE, PLUGIN_STORE_VALUE, FX_single);
  }PLAYER_unlock();

  if(type->recreate_from_state!=NULL)
    type->recreate_from_state(plugin, HASH_get_hash(state, "plugin_state"), false);

}

hash_t *PLUGIN_get_ab_state(SoundPlugin *plugin){
  hash_t *state = HASH_create(2);

  SoundPluginType *type = plugin->type;
  
  int num_effects = type->num_effects+NUM_SYSTEM_EFFECTS;
    
  hash_t *values_state = HASH_create(num_effects);
  for(int n=0;n<num_effects;n++)
    HASH_put_float_at(values_state,"value",n,plugin->savable_effect_values[n]);
  
  HASH_put_hash(state, "values", values_state);
    
  if(type->create_state!=NULL){
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

  // do_random
  for(int i=0;i<type->num_effects;i++)
    HASH_put_bool_at(state, "do_random_change", i, plugin->do_random_change[i]);
                     
  // effects
  HASH_put_hash(state,"effects",PLUGIN_get_effects_state(plugin));

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

  // sample seek
  HASH_put_bool(state, "enable_sample_seek", ATOMIC_GET(plugin->enable_sample_seek));
  
  HASH_put_int(state, "___radium_plugin_state_v3", 1);
      
  return state;
}

float PLUGIN_get_effect_from_name(SoundPlugin *plugin, const char *effect_name){
  const SoundPluginType *type=plugin->type;
  int i;

  for(i=0;i<type->num_effects+NUM_SYSTEM_EFFECTS;i++){
    if (!strcmp(PLUGIN_get_effect_name(plugin, i), effect_name))
      break;
  }

  if (i==type->num_effects+NUM_SYSTEM_EFFECTS) {
    GFX_Message(NULL, "No effect named \"%s\" in %s/%s", effect_name, type->type_name, type->name);
    return 0;
  }

  return PLUGIN_get_effect_value(plugin, i, VALUE_FROM_STORAGE);
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
  PLUGIN_set_native_effect_value(plugin, -1, i, value, PLUGIN_NONSTORED_TYPE, PLUGIN_STORE_VALUE, FX_single);
}

void PLUGIN_set_effects_from_state(SoundPlugin *plugin, hash_t *effects){
  const SoundPluginType *type=plugin->type;

#if 0
  // original code:
  for(i=0;i<type->num_effects+NUM_SYSTEM_EFFECTS;i++){
    const char *effect_name = PLUGIN_get_effect_name(plugin,i);
    if(HASH_has_key(effects, effect_name)){
      float val = HASH_get_float(effects, effect_name);
      if(i<type->num_effects){
        radium::PlayerLock lock;
        type->set_effect_value(plugin, -1, i, val, PLUGIN_FORMAT_NATIVE, FX_single);
        plugin->savable_effect_values[i] = type->get_effect_value(plugin, i, PLUGIN_FORMAT_SCALED);
      }else
        PLUGIN_set_effect_value(plugin, -1, i, val, PLUGIN_STORED_TYPE, PLUGIN_STORE_VALUE, FX_single);
    }else
      plugin->savable_effect_values[i] = PLUGIN_get_effect_value(plugin,i,VALUE_FROM_PLUGIN); // state didn't have it. Store default value.
  }
#endif

  hash_t *copy = HASH_copy(effects);
      
  bool has_value[type->num_effects+NUM_SYSTEM_EFFECTS];
  float values[type->num_effects+NUM_SYSTEM_EFFECTS];
    
  for(int i=0;i<type->num_effects+NUM_SYSTEM_EFFECTS;i++) {
    const char *effect_name = PLUGIN_get_effect_name(plugin,i);
    has_value[i] = HASH_has_key(effects, effect_name);

    if (!has_value[i]) {
      const char *name = get_effect_name_without_digit_prefix(effect_name);
      if (name!=NULL){
        effect_name = name;
        has_value[i] = HASH_has_key(effects, effect_name);
      }
    }
    
    if (has_value[i]) {
      values[i] = HASH_get_float(effects, effect_name);
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
    if(has_value[i]){
      float val = values[i];
      PLUGIN_set_native_effect_value(plugin, -1, i, val, PLUGIN_STORED_TYPE, PLUGIN_STORE_VALUE, FX_single);
    }else if (i-type->num_effects==EFFNUM_HIGHPASS_FREQ)
      plugin->savable_effect_values[i] = 200; // Old songs didn't have this slider
    else
      plugin->savable_effect_values[i] = PLUGIN_get_effect_value(plugin,i,VALUE_FROM_PLUGIN);
  }

  // 3. Store custom effects (need lock here)
  if (type->dont_send_effect_values_from_state_into_plugin ==false) {
    radium::PlayerLock lock;

    for(int i=0 ; i<type->num_effects ; i++){
      PLAYER_maybe_pause_lock_a_little_bit(i);
      
      if(has_value[i]){
        float val = values[i];
        type->set_effect_value(plugin, -1, i, val, PLUGIN_FORMAT_NATIVE, FX_single);
        plugin->savable_effect_values[i] = type->get_effect_value(plugin, i, PLUGIN_FORMAT_SCALED);
      }else
        plugin->savable_effect_values[i] = PLUGIN_get_effect_value(plugin,i,VALUE_FROM_PLUGIN);
    }
  }
}

SoundPlugin *PLUGIN_create_from_state(hash_t *state, bool is_loading){
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
  
  SoundPlugin *plugin = PLUGIN_create(type, plugin_state, is_loading);

  if(plugin==NULL)
    return NULL;

  int state_version = HASH_has_key(state, "plugin_type_version") ? HASH_get_int32(state, "plugin_type_version") : -1;
  int plugin_type_version = type->version;

  // effects state
  hash_t *effects = HASH_get_hash(state, "effects");
  PLUGIN_set_effects_from_state(plugin, effects);

  if(plugin_state!=NULL && type->recreate_from_state!=NULL)
    type->recreate_from_state(plugin, plugin_state, is_loading);

  // auto-suspend
  if (HASH_has_key(state, "auto_suspend_behavior"))
    PLUGIN_set_autosuspend_behavior(plugin, (AutoSuspendBehavior)HASH_get_int(state, "auto_suspend_behavior"));

  // do_random
  for(int i=0;i<type->num_effects;i++)
    if (HASH_has_key_at(state, "do_random_change", i))
      plugin->do_random_change[i] = HASH_get_bool_at(state, "do_random_change", i);
  
  // midi learns state
  {
    for(int i = 0 ; i < HASH_get_array_size(state, "midi_learns") ; i++){
      if (HASH_has_key_at(state, "midi_learns", i)){ // In case array is used for something else as well. TODO: Create a HASH_get_array_size function that takes key as argument.
        auto *midi_learn = new SoundPluginEffectMidiLearn(plugin, HASH_get_hash_at(state, "midi_learns", i));
        add_midi_learn(midi_learn);
      }
    }
  }

  // A/B
  if (state_version==plugin_type_version) {
    if (HASH_has_key(state, "ab")){

      int num_effects = type->num_effects+NUM_SYSTEM_EFFECTS;
      
      hash_t *ab_state=HASH_get_hash(state, "ab");

      plugin->curr_ab_num = HASH_get_int32(ab_state, "curr_ab_num");
      
      for(int i=0;i<NUM_AB;i++){
        plugin->ab_is_valid[i] = HASH_get_bool_at(ab_state, "is_valid", i);

        if (plugin->ab_is_valid[i]){
          plugin->ab_states[i] = (hash_t*)replace_gc_root(plugin->ab_states[i], HASH_get_hash_at(ab_state, "ab_state", i));
          
          hash_t *values_state = HASH_get_hash_at(ab_state, "ab_values", i);
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

  
  if (HASH_has_key(state, "enable_sample_seek"))
    ATOMIC_SET(plugin->enable_sample_seek, HASH_get_bool(state, "enable_sample_seek"));
  

  return plugin;
}

void PLUGIN_change_ab(SoundPlugin *plugin, int ab_num){
  R_ASSERT_RETURN_IF_FALSE(ab_num>=0);
  R_ASSERT_RETURN_IF_FALSE(ab_num<NUM_AB);

  SoundPluginType *type = plugin->type;
  
  int old_ab_num = plugin->curr_ab_num;
  int new_ab_num = ab_num;
  
  int num_effects = type->num_effects+NUM_SYSTEM_EFFECTS;

  // Save old data
  {
    memcpy(plugin->ab_values[old_ab_num], plugin->savable_effect_values, sizeof(float)*num_effects);

    if(type->create_state!=NULL) {
      HASH_clear(plugin->ab_states[old_ab_num]);
      type->create_state(plugin, plugin->ab_states[old_ab_num]);
    }
    
    plugin->ab_is_valid[old_ab_num] = true;
  }

  // Insert new data
  //
  if(plugin->ab_is_valid[new_ab_num]){
    
    float *new_ab_values = plugin->ab_values[new_ab_num];

    ADD_UNDO(AudioEffect_CurrPos((struct Patch*)plugin->patch, -1));

    PLAYER_lock();{
      for(int i=0;i<num_effects;i++)
        PLUGIN_set_effect_value(plugin, 0, i, new_ab_values[i], PLUGIN_STORED_TYPE, PLUGIN_STORE_VALUE, FX_single);
    }PLAYER_unlock();

    if(type->recreate_from_state!=NULL)
      type->recreate_from_state(plugin, plugin->ab_states[new_ab_num], false);
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

char *PLUGIN_generate_new_patchname(SoundPluginType *plugin_type){
  return talloc_format("%s %d",plugin_type->name,++plugin_type->instance_num);    
}

QString SoundPluginEffectMidiLearn::get_dest_info(void){
  QString a = plugin->patch==NULL ? plugin->type->name : plugin->patch->name;

  return a + " / " + PLUGIN_get_effect_name(plugin, effect_num);
}

void SoundPluginEffectMidiLearn::delete_me(void){
  PLUGIN_remove_midi_learn(plugin, effect_num, true);
}

// called from player thread
void SoundPluginEffectMidiLearn::RT_callback(float val) {
  //printf("soundpluginmidilearn %s got %f\n", plugin->patch->name, val);

  int num_effects = plugin->type->num_effects;
  int system_effect = effect_num - num_effects;

  //printf("effect_num: %d. system_effect: %d, comp_eff_attack: %d\n", effect_num, effect_num - num_effects, COMP_EFF_ATTACK);

  if(system_effect==EFFNUM_COMP_ATTACK || system_effect==EFFNUM_COMP_RELEASE) {
    val = scale(val, 0, 1, 0, 500);
    PLUGIN_set_effect_value2(plugin, -1, effect_num, val, PLUGIN_NONSTORED_TYPE, PLUGIN_STORE_VALUE, FX_single, PLUGIN_FORMAT_NATIVE, true);
  } else
    PLUGIN_set_effect_value2(plugin, -1, effect_num, val, PLUGIN_NONSTORED_TYPE, PLUGIN_STORE_VALUE, FX_single, PLUGIN_FORMAT_SCALED, true);
  
  volatile struct Patch *patch = plugin->patch;
  if (patch != NULL)
    ATOMIC_SET(patch->widget_needs_to_be_updated, true);
}

void PLUGIN_add_midi_learn(SoundPlugin *plugin, int effect_num){
  RT_PLUGIN_touch(plugin);
  
  auto *midi_learn = new SoundPluginEffectMidiLearn(plugin, effect_num);
  add_midi_learn(midi_learn);
}

bool PLUGIN_remove_midi_learn(SoundPlugin *plugin, int effect_num, bool show_error_if_not_here){
  RT_PLUGIN_touch(plugin);
  
  SoundPluginEffectMidiLearn *midi_learn=NULL;

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
}

enum AutoSuspendBehavior PLUGIN_get_autosuspend_behavior(SoundPlugin *plugin){
  return ATOMIC_GET(plugin->auto_suspend_behavior);
}

void PLUGIN_set_random_behavior(SoundPlugin *plugin, const int effect_num, bool do_random){
  plugin->do_random_change[effect_num] = do_random;
}

bool PLUGIN_get_random_behavior(SoundPlugin *plugin, const int effect_num){
  if (effect_num >= plugin->type->num_effects)
    return false;
  else
    return plugin->do_random_change[effect_num];
}

// only called from MultiCore.cpp, one time per audio block per instrument
bool RT_PLUGIN_can_autosuspend(SoundPlugin *plugin, int64_t time){

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
    if (plugin->playing_voices != NULL || plugin->patch->playing_voices != NULL)
      return false;
  }

  {
    if (ATOMIC_GET(plugin->auto_suspend_suspended))
      return false;
  }
  
  {
    int delay = -1;

    if (type->RT_get_audio_tail_length != NULL)
      delay = type->RT_get_audio_tail_length(plugin);

    if (delay == -1)
      delay = (double)ATOMIC_GET(g_autobypass_delay) * MIXER_get_sample_rate() / 1000.0;

    if (delay < -1)
      return false;
    
    // input latency
    delay += RT_SP_get_input_latency(plugin->sp);

    // plugin latency
    if (plugin->type->RT_get_latency != NULL)
      delay += plugin->type->RT_get_latency(plugin);

    // smooth delay delay
    delay += (plugin->delay_time * MIXER_get_sample_rate() / 1000);
    
    // The timing logic is a little bit uncertain, so we add one jack block just to be sure.
    delay += MIXER_get_jack_block_size();

    // ...and we add some frames to eliminate rounding errors and possibly other minor things (system filters, etc.). (important for instruments that implement RT_get_audio_tail_length)
    delay += 64;

    int64_t time_since_activity = time-ATOMIC_GET(plugin->time_of_last_activity);
    
    if (time_since_activity > delay)
      return true;
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

  volatile struct Patch *patch = plugin->patch;
  R_ASSERT_RETURN_IF_FALSE(patch!=NULL);
  
  ADD_UNDO(AudioEffect_CurrPos((struct Patch*)patch, -1));

  PLAYER_lock();{
    for(int i=0;i<type->num_effects;i++)
      PLUGIN_set_effect_value(plugin, 0, i, plugin->initial_effect_values[i], PLUGIN_STORED_TYPE, PLUGIN_STORE_VALUE, FX_single);
  }PLAYER_unlock();
}

void PLUGIN_reset_one_effect(SoundPlugin *plugin, int effect_num){
  volatile struct Patch *patch = plugin->patch;
  R_ASSERT_RETURN_IF_FALSE(patch!=NULL);
  
  ADD_UNDO(AudioEffect_CurrPos((struct Patch*)patch, effect_num));
  PLUGIN_set_effect_value(plugin, 0, effect_num, plugin->initial_effect_values[effect_num], PLUGIN_STORED_TYPE, PLUGIN_STORE_VALUE, FX_single);
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

  volatile struct Patch *patch = plugin->patch;
  R_ASSERT_RETURN_IF_FALSE(patch!=NULL);

  if (type->num_effects==0)
    return;
  
  ADD_UNDO(AudioEffect_CurrPos((struct Patch*)patch, -1));

  float values[type->num_effects];
  for(i=0;i<type->num_effects;i++)
    if (plugin->do_random_change[i])
      values[i]=get_rand();
  
  PLAYER_lock();{
    for(i=0;i<type->num_effects;i++)
      if (plugin->do_random_change[i])
        PLUGIN_set_effect_value(plugin, 0, i, values[i], PLUGIN_NONSTORED_TYPE, PLUGIN_STORE_VALUE, FX_single);
  };PLAYER_unlock();
}

// plugin can be NULL here.
void PLUGIN_show_info_window(const SoundPluginType *type, SoundPlugin *plugin, int64_t parentgui){
  QString info;
      
  if(type->info!=NULL)
    info = type->info;
  else {
    if(!strcmp(type->type_name,type->name))
      info = type->type_name;
    else
      info = QString(type->type_name) + ": " + type->name;
  }
      
  info += "\n\n";
      
  info += "Inputs: " + QString::number(type->num_inputs) + "\n";
  info += "Outputs: " + QString::number(type->num_outputs) + "\n";

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

    info += "Latency: " + QString::number(latency*1000/MIXER_get_sample_rate()) + "ms\n";
    info += "Audio tail: " + (tail < 0 ? "undefined" : QString::number(tail*1000.0/MIXER_get_sample_rate()) + "ms") + "\n";

    double time_since_last_activity = MIXER_get_last_used_time() - ATOMIC_GET(plugin->time_of_last_activity);      
    info += "Last activity: " + QString::number(time_since_last_activity*1000.0/MIXER_get_sample_rate()) + "ms ago\n";
  }
  
  MyQMessageBox *infoBox = MyQMessageBox::create(false, API_gui_get_parentwidget(parentgui));
  infoBox->setAttribute(Qt::WA_DeleteOnClose);
  
  infoBox->setText(info);

  infoBox->setWindowModality(Qt::NonModal);
  safeShow(infoBox);
}
