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

#include "../common/nsmtracker.h"
#include "../common/hashmap_proc.h"
#include "../common/undo.h"
#include "../common/OS_Player_proc.h"
#include "../common/visual_proc.h"
#include "../common/undo_patch_proc.h"
#include "../common/player_proc.h"
#include "../common/patch_proc.h"

#include "../mixergui/QM_chip.h"
#include "../mixergui/QM_MixerWidget.h"
#include "../mixergui/undo_chip_addremove_proc.h"
#include "../mixergui/undo_mixer_proc.h"
#include "../Qt/Qt_instruments_proc.h"
#include "../Qt/undo_instruments_widget_proc.h"

#include "SoundPlugin.h"
#include "Mixer_proc.h"
#include "SoundPluginRegistry_proc.h"
#include "SoundProducer_proc.h"
#include "undo_audio_effect_proc.h"
#include "system_compressor_wrapper_proc.h"
#include "audio_instrument_proc.h"

#include "SoundPlugin_proc.h"

#define MIN_DB -40
#define MAX_DB 35

#define FILTER_MIN_DB -35
#define FILTER_MAX_DB 35

#define MIN_FREQ 50
#define MAX_FREQ 20000

#define DELAY_MIN 0.0f
#define DELAY_MAX 50.0f


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

const static float threshold_val = 0.05f;

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

  if(gain <= threshold_gain) // Below threshold, we do a linear conversion. If not there will be a jump between 0 and almost 0.
    return scale(gain, 
                 0.0f, threshold_val,
                 0.0f, threshold_gain);
  else
    return scale(20 * log10(gain),
                 db_min, db_max,
                 0.0f, 1.0f);
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
  return scale( logf(freq)/logf(max_freq),
                min_output, 1.0,
                0.0, 1.0);
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

  "System Volume",
  "System Volume On/Off",

  "System Out",
  "System Out On/Off",

  "System Reverb",
  "System Reverb On/Off",
  "System Chorus",
  "System Chorus On/Off",

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
  "System Controls On/Off"
};

static void init_system_filter(SystemFilter *filter, int num_channels, const char *name){
  int ch;
  filter->plugins=calloc(sizeof(SoundPlugin*),num_channels);
  for(ch=0;ch<num_channels;ch++){
    filter->plugins[ch] = calloc(1, sizeof(SoundPlugin));
    filter->plugins[ch]->type = PR_get_plugin_type_by_name(NULL, "Faust",name);
    filter->plugins[ch]->data = filter->plugins[ch]->type->create_plugin_data(filter->plugins[ch]->type, filter->plugins[ch], NULL, MIXER_get_sample_rate(), MIXER_get_buffer_size());
    filter->was_off = true;
    filter->was_on = false;
  }
}

static void release_system_filter(SystemFilter *filter, int num_channels){
  int ch;
  for(ch=0;ch<num_channels;ch++){
    filter->plugins[ch]->type->cleanup_plugin_data(filter->plugins[ch]);
    free(filter->plugins[ch]);
  }
  free(filter->plugins);
}

SoundPlugin *PLUGIN_create_plugin(const SoundPluginType *plugin_type, hash_t *plugin_state){
  SoundPlugin *plugin = calloc(1,sizeof(SoundPlugin));
  plugin->type = plugin_type;

  int buffer_size = MIXER_get_buffer_size();

  // TODO: Don't do this. Check if all plugins can be initialized later.
  plugin->data = plugin_type->create_plugin_data(plugin_type, plugin, plugin_state, MIXER_get_sample_rate(), buffer_size);
  if(plugin->data==NULL){
    free(plugin);
    return NULL;
  }

  SMOOTH_init(&plugin->input_volume  , 1.0f, buffer_size);
  SMOOTH_init(&plugin->output_volume , 1.0f, buffer_size);
  SMOOTH_init(&plugin->bus_volume[0] , 0.0f, buffer_size);
  SMOOTH_init(&plugin->bus_volume[1] , 0.0f, buffer_size);
  SMOOTH_init(&plugin->pan           , 0.5f, buffer_size);
  SMOOTH_init(&plugin->drywet        , 1.0f, buffer_size);

  plugin->input_volume_is_on = true;
  plugin->output_volume_is_on = true;
  plugin->bus_volume_is_on[0] = true;
  plugin->bus_volume_is_on[1] = true;
  plugin->effects_are_on = true;

  plugin->volume = 1.0f;
  plugin->volume_is_on = true;

  if(!strcmp(plugin_type->type_name,"Bus"))
    plugin->bus_descendant_type = IS_BUS_DESCENDANT;
  else
    plugin->bus_descendant_type = IS_NOT_A_BUS_DESCENDANT;


  {
    int num_outputs=plugin_type->num_outputs;

    init_system_filter(&plugin->lowpass, num_outputs, "System Lowpass");
    plugin->lowpass_freq = 5000.0f;

    init_system_filter(&plugin->eq1, num_outputs, "System Eq");
    plugin->eq1_freq = 400.0f;
    plugin->eq1_db = 0.0f;

    init_system_filter(&plugin->eq2, num_outputs, "System Eq");
    plugin->eq2_freq = 1000.0f;
    plugin->eq2_db = 0.0f;

    init_system_filter(&plugin->lowshelf, num_outputs, "System Lowshelf");
    plugin->lowshelf_freq = 400.0f;
    plugin->lowshelf_db = 0.0f;

    init_system_filter(&plugin->highshelf, num_outputs, "System Highshelf");
    plugin->highshelf_freq = 1500.0f;
    plugin->highshelf_db = 0.0f;

    init_system_filter(&plugin->delay, num_outputs, "System Delay");
    plugin->delay_time = 0.0f;
  }

  {
    plugin->compressor = COMPRESSOR_create(MIXER_get_sample_rate());
    plugin->comp.is_on = false;
    plugin->comp.was_off = true;
    plugin->comp.was_on = false;
  }

  plugin->show_browser_gui = true;
  plugin->show_controls_gui = true;
  plugin->show_equalizer_gui = true;
  plugin->show_compressor_gui = false;

  {
    int i;
    plugin->savable_effect_values=calloc(sizeof(float),plugin_type->num_effects+NUM_SYSTEM_EFFECTS);

#if 0
    for(i=0;i<plugin_type->num_effects+NUM_SYSTEM_EFFECTS;i++)
      plugin->savable_effect_values[i] = PLUGIN_get_effect_value(plugin,i,VALUE_FROM_PLUGIN);
#else
    for(i=0;i<plugin_type->num_effects;i++)
      plugin->savable_effect_values[i] = PLUGIN_get_effect_value(plugin,i,VALUE_FROM_PLUGIN);

    plugin->savable_effect_values[plugin_type->num_effects+EFFNUM_OUTPUT_VOLUME] = 1.0f;

    for(i=plugin_type->num_effects;i<plugin_type->num_effects+NUM_SYSTEM_EFFECTS;i++){
      float value = PLUGIN_get_effect_value(plugin,i,VALUE_FROM_PLUGIN);
      PLUGIN_set_effect_value(plugin, 0, i, value, PLUGIN_NONSTORED_TYPE, PLUGIN_STORE_VALUE, FX_single);
    }
#endif
  }

  {
    plugin->initial_effect_values=calloc(sizeof(float),plugin_type->num_effects+NUM_SYSTEM_EFFECTS);
    memcpy(plugin->initial_effect_values, plugin->savable_effect_values, sizeof(float) * (plugin_type->num_effects+NUM_SYSTEM_EFFECTS));
  }

  //plugin->bus_num = -1;

  return plugin;
}

void PLUGIN_delete_plugin(SoundPlugin *plugin){
  const SoundPluginType *plugin_type = plugin->type;

  if(!strcmp(plugin_type->type_name,"Bus")) // RT_process needs buses to always be alive.
    return;

  plugin_type->cleanup_plugin_data(plugin);

  free(plugin->initial_effect_values);
  free(plugin->savable_effect_values);

  
  SMOOTH_release(&plugin->input_volume);
    
  SMOOTH_release(&plugin->output_volume);
  
  SMOOTH_release(&plugin->bus_volume[0]);
  
  SMOOTH_release(&plugin->bus_volume[1]);
  
  SMOOTH_release(&plugin->pan);
  
  SMOOTH_release(&plugin->drywet);
  
  COMPRESSOR_delete(plugin->compressor);
  
  release_system_filter(&plugin->lowpass, plugin_type->num_outputs);
  
  release_system_filter(&plugin->eq1, plugin_type->num_outputs);
  
  release_system_filter(&plugin->eq2, plugin_type->num_outputs);
  
  release_system_filter(&plugin->lowshelf, plugin_type->num_outputs);
    
  release_system_filter(&plugin->highshelf, plugin_type->num_outputs);
  
  release_system_filter(&plugin->delay, plugin_type->num_outputs);
  
  memset(plugin,-1,sizeof(SoundPlugin)); // for debugging. Crashes faster if something is wrong.
  free(plugin);
}

// Called at the start of each block
void PLUGIN_update_smooth_values(SoundPlugin *plugin){
  SMOOTH_called_per_block(&plugin->input_volume);
  SMOOTH_called_per_block(&plugin->output_volume);
  SMOOTH_called_per_block(&plugin->bus_volume[0]);
  SMOOTH_called_per_block(&plugin->bus_volume[1]);
  SMOOTH_called_per_block(&plugin->pan);
  SMOOTH_called_per_block(&plugin->drywet);
}

int PLUGIN_get_num_visible_effects(SoundPlugin *plugin){
  const SoundPluginType *plugin_type = plugin->type;
  int i;
  int result=0;

  if(plugin_type->effect_is_visible != NULL) {

    for(i=0;i<plugin_type->num_effects;i++)
      result += plugin_type->effect_is_visible ? 1 : 0;
    return result;

  }else{

    return plugin_type->num_effects;

  }
}

int PLUGIN_get_effect_format(struct SoundPlugin *plugin, int effect_num){
  const struct SoundPluginType *plugin_type = plugin->type;

  if(effect_num<plugin_type->num_effects)
    return plugin_type->get_effect_format(plugin, effect_num);

  int system_effect = effect_num - plugin_type->num_effects;

  RError("The program isn't supposed to be here");

  if(system_effect==EFFNUM_PAN_ONOFF || system_effect==EFFNUM_EFFECTS_ONOFF || system_effect==EFFNUM_LOWPASS_ONOFF || system_effect==EFFNUM_EQ1_ONOFF || system_effect==EFFNUM_EQ2_ONOFF)
    return EFFECT_FORMAT_BOOL;
  else
    return EFFECT_FORMAT_FLOAT;
}

int PLUGIN_get_effect_num(struct SoundPlugin *plugin, const char *effect_name){
  const struct SoundPluginType *plugin_type = plugin->type;

  int i;
  for(i=0;i<NUM_SYSTEM_EFFECTS;i++)
    if(!strcmp(system_effect_names[i],effect_name))
      return i + plugin_type->num_effects;

  return plugin_type->get_effect_num(plugin,effect_name);
}

const char *PLUGIN_get_effect_name(struct SoundPlugin *plugin, int effect_num){
  const struct SoundPluginType *plugin_type = plugin->type;

  if(effect_num<plugin_type->num_effects)
    return plugin_type->get_effect_name(plugin, effect_num);

  int system_effect = effect_num - plugin_type->num_effects;
  return system_effect_names[system_effect];
}

const char *PLUGIN_get_effect_description(const struct SoundPluginType *plugin_type, int effect_num){
  if(effect_num<plugin_type->num_effects)
    return plugin_type->get_effect_description(plugin_type, effect_num);

  return "System effects have no description yet.";
}

static void set_db_display(char *buffer, int buffersize, float value){
  float db = gain_2_db(value,MIN_DB,MAX_DB);
  //snprintf(buffer,buffersize-1,"%s%s%.2f dB", db<0.0f?"":"+", db>-10.0f && db<10.0f ? "  ":"", db);
  if(db==MIN_DB)
    snprintf(buffer,buffersize-1,"-inf dB");
  else
    snprintf(buffer,buffersize-1,"%s%.2f dB", db<0.0f?"":"+", db);
}

// It's not necessary to implement for all EFFNUM_* values. Can probably remove some code.
//void PLUGIN_get_display_value_string(struct SoundPlugin *plugin, int effect_num, bool use_stored_value, float value, char *buffer, int buffersize){
void PLUGIN_get_display_value_string(struct SoundPlugin *plugin, int effect_num, char *buffer, int buffersize){
  if(effect_num<plugin->type->num_effects)
    return plugin->type->get_display_value_string(plugin, effect_num, buffer, buffersize);

  float val;

  int system_effect = effect_num - plugin->type->num_effects;

  switch(system_effect){
  case EFFNUM_INPUT_VOLUME:
    set_db_display(buffer,buffersize,SMOOTH_get_target_value(&plugin->input_volume));
    //val = gain_2_db(plugin->input_volume.target_value,MIN_DB,MAX_DB);
    //snprintf(buffer,buffersize-1,"%s%.2f dB",val<0.0f?"":"+",val);
    break;
  case EFFNUM_VOLUME:
    val = gain_2_db(plugin->volume,MIN_DB,MAX_DB);
    if(val==MIN_DB)
      snprintf(buffer,buffersize-1,"-inf dB");
    else
      snprintf(buffer,buffersize-1,"%s%.2f dB",val<0.0f?"":"+",val);
    break;
    
  case EFFNUM_OUTPUT_VOLUME:
    val = gain_2_db(plugin->output_volume.target_value/plugin->volume,MIN_DB,MAX_DB);
    if(val==MIN_DB)
      snprintf(buffer,buffersize-1,"-inf dB");
    else
      snprintf(buffer,buffersize-1,"%s%.2f dB",val<0.0f?"":"+",val);
    break;
    
  case EFFNUM_BUS1:
    val = gain_2_db(plugin->bus_volume[0].target_value/plugin->volume,MIN_DB,MAX_DB);
    if(val==MIN_DB)
      snprintf(buffer,buffersize-1,"-inf dB");
    else
      snprintf(buffer,buffersize-1,"%s%.2f dB",val<0.0f?"":"+",val);
    break;
  case EFFNUM_BUS2:
    val = gain_2_db(plugin->bus_volume[1].target_value/plugin->volume,MIN_DB,MAX_DB);
    if(val==MIN_DB)
      snprintf(buffer,buffersize-1,"-inf dB");
    else
      snprintf(buffer,buffersize-1,"%s%.2f dB",val<0.0f?"":"+",val);
    break;

  case EFFNUM_PAN:
    snprintf(buffer,buffersize-1,"%d %s",(int)scale(plugin->pan.target_value,0,1,-90,90),"\u00B0");
    break;
  case EFFNUM_PAN_ONOFF:
    snprintf(buffer,buffersize-1,"%s",plugin->pan_is_on==true?"ON":"OFF");
    break;

  case EFFNUM_LOWPASS_FREQ:
    snprintf(buffer,buffersize-1,"%.1f Hz",plugin->lowpass_freq);
    break;

  case EFFNUM_EQ1_FREQ:
    snprintf(buffer,buffersize-1,"%.1f Hz",plugin->eq1_freq);
    break;
  case EFFNUM_EQ1_GAIN:
    val = plugin->eq1_db;
    snprintf(buffer,buffersize-1,"%s%.2f dB",val<0.0f?"":"+",val);
    break;

  case EFFNUM_EQ2_FREQ:
    snprintf(buffer,buffersize-1,"%.1f Hz",plugin->eq2_freq);
    break;
  case EFFNUM_EQ2_GAIN:
    val = plugin->eq2_db;
    snprintf(buffer,buffersize-1,"%s%.2f dB",val<0.0f?"":"+",val);
    break;

  case EFFNUM_LOWSHELF_FREQ:
    snprintf(buffer,buffersize-1,"%.1f Hz",plugin->lowshelf_freq);
    break;
  case EFFNUM_LOWSHELF_GAIN:
    val = plugin->lowshelf_db;
    snprintf(buffer,buffersize-1,"%s%.2f dB",val<0.0f?"":"+",val);
    break;

  case EFFNUM_HIGHSHELF_FREQ:
    snprintf(buffer,buffersize-1,"%.1f Hz",plugin->highshelf_freq);
    break;
  case EFFNUM_HIGHSHELF_GAIN:
    val = plugin->highshelf_db;
    snprintf(buffer,buffersize-1,"%s%.2f dB",val<0.0f?"":"+",val);
    break;
    
  case EFFNUM_DELAY_TIME:
    val = plugin->delay_time;
    snprintf(buffer,buffersize-1,"%.2f ms",val);
    break;

  case EFFNUM_DRYWET:
    {
      int wet = plugin->drywet.target_value * 100;
      int dry = 100-wet;
      snprintf(buffer,buffersize-1,"Dry: %d%%. Wet: %d%%",dry,wet);
    }
    break;
  case EFFNUM_EFFECTS_ONOFF:
    snprintf(buffer,buffersize-1,"%s",plugin->effects_are_on==true?"ON":"OFF");
    break;

  default:
    RError("Unknown effect number: %d",effect_num);
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

static void set_smooth_on_off(Smooth *smooth, bool *on_off, float value, float set_value){
  if(value>0.5f){
    *on_off = true;
    SMOOTH_set_target_value(smooth, set_value);
  }else{
    *on_off = false;
    SMOOTH_set_target_value(smooth, 0.0f);
  }
}

void PLUGIN_set_effect_value2(struct SoundPlugin *plugin, int64_t time, int effect_num, float value, enum ValueType value_type, enum SetValueType set_type, FX_when when, enum PlayerLockRequired player_lock_required){
  float store_value = value;
  //printf("set effect value. effect_num: %d, value: %f, num_effects: %d\n",effect_num,value,plugin->type->num_effects);

  if(effect_num < plugin->type->num_effects){

    if (PLAYER_current_thread_has_lock()==false && pc->isplaying==true){
      if (player_lock_required==PLAYERLOCK_REQUIRED)
        RWarning("PLUGIN_set_effect_value_internal called without holding the player lock");
      else if (player_lock_required==PLAYERLOCK_MAYBE_REQUIRED)
        RWarning_not_prod("PLUGIN_set_effect_value_internal called without holding the player lock");
    }

    plugin->type->set_effect_value(plugin,time,effect_num,value,PLUGIN_FORMAT_SCALED,when);

  }else{

    int num_effects = plugin->type->num_effects;
    int system_effect = effect_num - num_effects;
    int ch;

    switch(system_effect){
    case EFFNUM_INPUT_VOLUME:
      store_value = get_gain_store_value(value,value_type);
      if(plugin->input_volume_is_on==true)
        SMOOTH_set_target_value(&plugin->input_volume, store_value);
      break;
    case EFFNUM_INPUT_VOLUME_ONOFF:
      set_smooth_on_off(&plugin->input_volume, &plugin->input_volume_is_on, store_value, plugin->savable_effect_values[num_effects+EFFNUM_INPUT_VOLUME]);
      break;

    case EFFNUM_VOLUME:
      store_value = get_gain_store_value(value,value_type);

      if(plugin->volume_is_on==true){
        if(plugin->output_volume_is_on==true)
          SMOOTH_set_target_value(&plugin->output_volume, store_value * plugin->savable_effect_values[num_effects+EFFNUM_OUTPUT_VOLUME]);
        
        if(plugin->bus_volume_is_on[0]==true)
          SMOOTH_set_target_value(&plugin->bus_volume[0], store_value * plugin->savable_effect_values[num_effects+EFFNUM_BUS1]);
        
        if(plugin->bus_volume_is_on[1]==true)
          SMOOTH_set_target_value(&plugin->bus_volume[1], store_value * plugin->savable_effect_values[num_effects+EFFNUM_BUS2]);
      }

      plugin->volume = store_value;
      break;
    case EFFNUM_VOLUME_ONOFF:
      if(value>0.5f)
        plugin->volume = plugin->savable_effect_values[num_effects+EFFNUM_VOLUME];
      else
        plugin->volume = 0.0f;

      if(plugin->output_volume_is_on==true)
        set_smooth_on_off(&plugin->output_volume, &plugin->volume_is_on, store_value, plugin->volume*plugin->savable_effect_values[num_effects+EFFNUM_OUTPUT_VOLUME]);
      if(plugin->bus_volume_is_on[0]==true)
        set_smooth_on_off(&plugin->bus_volume[0], &plugin->volume_is_on, store_value, plugin->volume*plugin->savable_effect_values[num_effects+EFFNUM_BUS1]);
      if(plugin->bus_volume_is_on[1]==true)
        set_smooth_on_off(&plugin->bus_volume[1], &plugin->volume_is_on, store_value, plugin->volume*plugin->savable_effect_values[num_effects+EFFNUM_BUS2]);
      break;

    case EFFNUM_OUTPUT_VOLUME:
      store_value = get_gain_store_value(value,value_type);
      //printf("***PLUGIN_SET_EFFE_CT_FALUE. ****** store_value: %f\n",store_value);
      if(plugin->output_volume_is_on==true)
        SMOOTH_set_target_value(&plugin->output_volume, store_value*plugin->volume);
      break;
    case EFFNUM_OUTPUT_VOLUME_ONOFF:
      set_smooth_on_off(&plugin->output_volume, &plugin->output_volume_is_on, store_value, plugin->volume*plugin->savable_effect_values[num_effects+EFFNUM_OUTPUT_VOLUME]);
      break;

    case EFFNUM_BUS1:
      store_value = get_gain_store_value(value,value_type);
      if(plugin->bus_volume_is_on[0]==true)
        SMOOTH_set_target_value(&plugin->bus_volume[0], store_value*plugin->volume);
      break;
    case EFFNUM_BUS1_ONOFF:
      set_smooth_on_off(&plugin->bus_volume[0], &plugin->bus_volume_is_on[0], store_value, plugin->volume*plugin->savable_effect_values[num_effects+EFFNUM_BUS1]);
      break;

    case EFFNUM_BUS2:
      store_value = get_gain_store_value(value,value_type);
      if(plugin->bus_volume_is_on[1]==true)
        SMOOTH_set_target_value(&plugin->bus_volume[1], store_value*plugin->volume);
      break;
    case EFFNUM_BUS2_ONOFF:
      set_smooth_on_off(&plugin->bus_volume[1], &plugin->bus_volume_is_on[1], store_value, plugin->volume*plugin->savable_effect_values[num_effects+EFFNUM_BUS2]);
      break;

    case EFFNUM_PAN:
      if(plugin->pan_is_on==true)
        SMOOTH_set_target_value(&plugin->pan, value);
      break;
    case EFFNUM_PAN_ONOFF:
      if(value>0.5f){
        plugin->pan_is_on = true;
        SMOOTH_set_target_value(&plugin->pan, plugin->savable_effect_values[plugin->type->num_effects+EFFNUM_PAN]);
      }else{
        plugin->pan_is_on = false;
        SMOOTH_set_target_value(&plugin->pan, 0.5f);
      }
      break;
#if 0
    case EFFNUM_EDITOR_ONOFF:
      if(value>0.5f){
        plugin->editor_is_on = true;
        if(plugin->type->show_gui!=NULL)
          plugin->type->show_gui(plugin);
      }else{
        plugin->editor_is_on = false;
        if(plugin->type->show_gui!=NULL && plugin->type->hide_gui!=NULL)
          plugin->type->hide_gui(plugin);
      }
      break;
#endif

    case EFFNUM_DRYWET:
      if(plugin->effects_are_on==true)
        SMOOTH_set_target_value(&plugin->drywet, value);
      break;
    case EFFNUM_EFFECTS_ONOFF:
      if(value>0.5f){
        plugin->effects_are_on = true;
        SMOOTH_set_target_value(&plugin->drywet, plugin->savable_effect_values[plugin->type->num_effects+EFFNUM_DRYWET]);
      }else{
        plugin->effects_are_on = false;
        SMOOTH_set_target_value(&plugin->drywet, 0.0f);
      }
      break;
      
    case EFFNUM_LOWPASS_FREQ:
      store_value = get_freq_store_value(value, value_type);
      plugin->lowpass_freq = store_value;
      for(ch=0;ch<plugin->type->num_outputs;ch++)
        plugin->lowpass.plugins[ch]->type->set_effect_value(plugin->lowpass.plugins[ch], time, 0, store_value, PLUGIN_FORMAT_NATIVE, when);
      break;
    case EFFNUM_LOWPASS_ONOFF:
      plugin->lowpass.is_on = value > 0.5f;
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
      plugin->eq1.is_on = store_value > 0.5f;
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
      plugin->eq2.is_on = store_value > 0.5f;
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
      plugin->lowshelf.is_on = store_value > 0.5f;
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
      plugin->highshelf.is_on = store_value > 0.5f;
      break;

    case EFFNUM_EQ_SHOW_GUI:
      plugin->show_equalizer_gui = store_value > 0.5f;
      break;

    case EFFNUM_BROWSER_SHOW_GUI:
      plugin->show_browser_gui = store_value > 0.5f;
      break;

    case EFFNUM_CONTROLS_SHOW_GUI:
      plugin->show_controls_gui = store_value > 0.5f;
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
      plugin->comp.is_on=store_value > 0.5f;
      //printf("storing comp. %d %f %f\n",plugin->comp.is_on,store_value,value);
      break;

    case EFFNUM_COMP_SHOW_GUI:
      plugin->show_compressor_gui = store_value > 0.5f;
      break;

    case EFFNUM_DELAY_TIME:
      store_value = value_type==PLUGIN_STORED_TYPE ? value : scale(value, 0, 1, DELAY_MIN, DELAY_MAX);
      plugin->delay_time = store_value;
      for(ch=0;ch<plugin->type->num_outputs;ch++)
        plugin->delay.plugins[ch]->type->set_effect_value(plugin->delay.plugins[ch], time, 0, store_value, PLUGIN_FORMAT_NATIVE, when);
      break;
    case EFFNUM_DELAY_ONOFF:
      plugin->delay.is_on = store_value > 0.5f;
      break;
      
    default:
      RError("Unknown effect number: %d",effect_num);
    }  
  }

  if(set_type==PLUGIN_STORE_VALUE)
    plugin->savable_effect_values[effect_num] = store_value;
}

float PLUGIN_get_effect_value(struct SoundPlugin *plugin, int effect_num, enum WhereToGetValue where){
  if(effect_num >= plugin->type->num_effects + NUM_SYSTEM_EFFECTS){
    RError("Illegal effect_num %d",effect_num);
    return 0.0f;
  }
  float store_value = plugin->savable_effect_values[effect_num];

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
      return gain_2_slider(store_value,
                           MIN_DB, MAX_DB);
      
    case EFFNUM_LOWPASS_FREQ:
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
    return plugin->input_volume_is_on==true ? 1.0 : 0.0f;

  case EFFNUM_VOLUME:
    return gain_2_slider(plugin->volume, MIN_DB, MAX_DB);
  case EFFNUM_VOLUME_ONOFF:
    return plugin->volume_is_on==true ? 1.0 : 0.0f;

  case EFFNUM_OUTPUT_VOLUME:
#if 0
    {
      float val = gain_2_slider(SMOOTH_get_target_value(&plugin->output_volume)/plugin->volume,
                                MIN_DB, MAX_DB);
      //printf(">>>>>>>>>>>>>>>>>>>>>>>>> Get output volume. return val: %f. Target value: %f\n",val, plugin->output_volume.target_value);
    }
#endif
    return gain_2_slider(SMOOTH_get_target_value(&plugin->output_volume)/plugin->volume,
                         MIN_DB, MAX_DB);
  case EFFNUM_OUTPUT_VOLUME_ONOFF:
    return plugin->output_volume_is_on==true ? 1.0 : 0.0f;

  case EFFNUM_BUS1:
    return gain_2_slider(SMOOTH_get_target_value(&plugin->bus_volume[0])/plugin->volume,
                         MIN_DB, MAX_DB);
  case EFFNUM_BUS1_ONOFF:
    return plugin->bus_volume_is_on[0]==true ? 1.0 : 0.0f;

  case EFFNUM_BUS2:
    return gain_2_slider(SMOOTH_get_target_value(&plugin->bus_volume[1])/plugin->volume,
                         MIN_DB, MAX_DB);
  case EFFNUM_BUS2_ONOFF:
    return plugin->bus_volume_is_on[1]==true ? 1.0 : 0.0f;

  case EFFNUM_PAN:
    return SMOOTH_get_target_value(&plugin->pan);
  case EFFNUM_PAN_ONOFF:
    return plugin->pan_is_on==true ? 1.0 : 0.0f;

  case EFFNUM_DRYWET:
    return SMOOTH_get_target_value(&plugin->drywet);
  case EFFNUM_EFFECTS_ONOFF:
    return plugin->effects_are_on==true ? 1.0 : 0.0f;

  case EFFNUM_LOWPASS_FREQ:
    return frequency_2_slider(plugin->lowpass_freq,MIN_FREQ,MAX_FREQ);
  case EFFNUM_LOWPASS_ONOFF:
    return plugin->lowpass.is_on==true ? 1.0 : 0.0f;

  case EFFNUM_EQ1_FREQ:
    return frequency_2_slider(plugin->eq1_freq,MIN_FREQ,MAX_FREQ);
  case EFFNUM_EQ1_GAIN:
    return scale(plugin->eq1_db,FILTER_MIN_DB,FILTER_MAX_DB,0,1);
  case EFFNUM_EQ1_ONOFF:
    return plugin->eq1.is_on==true ? 1.0 : 0.0f;

  case EFFNUM_EQ2_FREQ:
    return frequency_2_slider(plugin->eq2_freq,MIN_FREQ,MAX_FREQ);
  case EFFNUM_EQ2_GAIN:
    return scale(plugin->eq2_db,FILTER_MIN_DB,FILTER_MAX_DB,0,1);
  case EFFNUM_EQ2_ONOFF:
    return plugin->eq2.is_on==true ? 1.0 : 0.0f;
    
  case EFFNUM_LOWSHELF_FREQ:
    return frequency_2_slider(plugin->lowshelf_freq,MIN_FREQ,MAX_FREQ);
  case EFFNUM_LOWSHELF_GAIN:
    return scale(plugin->lowshelf_db,FILTER_MIN_DB,FILTER_MAX_DB,0,1);
  case EFFNUM_LOWSHELF_ONOFF:
    return plugin->lowshelf.is_on==true ? 1.0 : 0.0f;
    
  case EFFNUM_HIGHSHELF_FREQ:
    return frequency_2_slider(plugin->highshelf_freq,MIN_FREQ,MAX_FREQ);
  case EFFNUM_HIGHSHELF_GAIN:
    return scale(plugin->highshelf_db,FILTER_MIN_DB,FILTER_MAX_DB,0,1);
  case EFFNUM_HIGHSHELF_ONOFF:
    return plugin->highshelf.is_on==true ? 1.0 : 0.0f;

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
    return plugin->comp.is_on==true ? 1.0 : 0.0f;

  case EFFNUM_COMP_SHOW_GUI:
    return plugin->show_compressor_gui==true ? 1.0 : 0.0f;
    
  case EFFNUM_DELAY_TIME:
    return scale(plugin->highshelf_db,DELAY_MIN,DELAY_MAX,0,1);
  case EFFNUM_DELAY_ONOFF:
    return plugin->delay.is_on==true ? 1.0 : 0.0f;
#if 0    
  case EFFNUM_EDITOR_ONOFF:
    return plugin->editor_is_on==true ? 1.0 : 0.0f;
#endif

  default:
    RError("Unknown effect number: %d",effect_num);
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

hash_t *PLUGIN_get_state(SoundPlugin *plugin){
  const SoundPluginType *type=plugin->type;

  hash_t *state=HASH_create(5);

  HASH_put_chars(state, "type_name", type->type_name);
  HASH_put_chars(state, "name", type->name);

  if (type->container!=NULL)
    HASH_put_chars(state, "container_name", type->container->name);

  HASH_put_hash(state,"effects",PLUGIN_get_effects_state(plugin));

  if(type->create_state != NULL){
    hash_t *plugin_state = HASH_create(10);
    HASH_put_hash(state, "plugin_state", plugin_state);
    type->create_state(plugin, plugin_state);
  }

  HASH_put_int(state, "___radium_plugin_state_v3", 1);
      
  return state;
}

void PLUGIN_set_effects_from_state(SoundPlugin *plugin, hash_t *effects){
  const SoundPluginType *type=plugin->type;

  int i;
  for(i=0;i<type->num_effects+NUM_SYSTEM_EFFECTS;i++){
    const char *effect_name = PLUGIN_get_effect_name(plugin,i);
    if(HASH_has_key(effects, effect_name)){
      float val = HASH_get_float(effects, effect_name);
      if(i<type->num_effects){
        PLAYER_lock();{
          type->set_effect_value(plugin, -1, i, val, PLUGIN_FORMAT_NATIVE, FX_single);
          plugin->savable_effect_values[i] = type->get_effect_value(plugin, i, PLUGIN_FORMAT_SCALED);
        }PLAYER_unlock();
      }else
        PLUGIN_set_effect_value(plugin, -1, i, val, PLUGIN_STORED_TYPE, PLUGIN_STORE_VALUE, FX_single);
    }else
      plugin->savable_effect_values[i] = PLUGIN_get_effect_value(plugin,i,VALUE_FROM_PLUGIN); // state didn't have it. Store default value.
  }
}

SoundPlugin *PLUGIN_create_from_state(hash_t *state){
  const char *container_name = HASH_has_key(state, "container_name") ? HASH_get_chars(state, "container_name") : NULL;
  const char *type_name = HASH_get_chars(state, "type_name");
  const char *name = HASH_get_chars(state, "name");

  const SoundPluginType *type = PR_get_plugin_type_by_name(container_name, type_name, name);
                          
  if(type==NULL){
    RError("The \"%s\" plugin called \"%s\" was not found",type_name,name);
    return NULL;
  }

  hash_t *plugin_state;

  if (strcmp(type->type_name, type_name) || strcmp(type->name, name))
    plugin_state=NULL; //i.e. selected a different plugin.
  
  else if (HASH_has_key(state, "plugin_state"))
    plugin_state=HASH_get_hash(state, "plugin_state");
  
  else
    plugin_state=NULL;
  
  SoundPlugin *plugin = PLUGIN_create_plugin(type, plugin_state);

  if(plugin==NULL)
    return NULL;

  hash_t *effects = HASH_get_hash(state, "effects");
  PLUGIN_set_effects_from_state(plugin, effects);

  if(plugin_state!=NULL && type->recreate_from_state!=NULL)
    type->recreate_from_state(plugin, plugin_state);
  
  return plugin;
}


char *PLUGIN_generate_new_patchname(SoundPluginType *plugin_type){
  return talloc_format("%s %d",plugin_type->name,++plugin_type->instance_num);    
}


void PLUGIN_set_from_patch(SoundPlugin *old_plugin, struct Patch *new_patch){
  struct Patch *old_patch = (struct Patch*)old_plugin->patch;
  R_ASSERT_RETURN_IF_FALSE2(old_patch!=NULL, NULL);

  CHIP_set_pos(new_patch, CHIP_get_pos_x(old_patch), CHIP_get_pos_y(old_patch)); // Hack. MW_move_chip_to_slot (called from Chip::Chip) sometimes kicks the chip one or to slots to the left.
  
  hash_t *connections_state = MW_get_connections_state();
    
  PATCH_replace_patch_in_song(old_patch, new_patch);
  PATCH_delete(old_patch);
    
  MW_create_connections_from_state_and_replace_patch(connections_state, old_patch->id, new_patch->id);
}

SoundPlugin *PLUGIN_set_from_state(SoundPlugin *old_plugin, hash_t *state){

  R_ASSERT(Undo_Is_Open());
    
  volatile struct Patch *patch = old_plugin->patch;
  if (patch==NULL) {
    RError("patch not found for old plugin");
    return NULL;
  }

  bool can_replace_patch = true;
  
  if (HASH_has_key(state, "___radium_plugin_state_v3")==false)  // Before 3.0.rc15, loading/saving states in the instrument widgets only loaded/saved the effect values, not the complete plugin state.
    can_replace_patch = false;
  
  else if(AUDIO_is_permanent_patch((struct Patch*)patch)) {
    state = HASH_get_hash(state, "effects");
    R_ASSERT(state!=NULL);
    can_replace_patch = false;
  }
        

  
  if (can_replace_patch==false) { 
    for(int i=0;i<old_plugin->type->num_effects+NUM_SYSTEM_EFFECTS;i++)
      Undo_AudioEffect_CurrPos((struct Patch*)patch, i);
    
    PLUGIN_set_effects_from_state(old_plugin, state);

    return old_plugin;
  }

  struct Patch *old_patch = (struct Patch*)old_plugin->patch;
  R_ASSERT_RETURN_IF_FALSE2(old_patch!=NULL, NULL);

  struct Patch *new_patch = InstrumentWidget_new_from_preset(state, old_patch->name, CHIP_get_pos_x(old_patch), CHIP_get_pos_y(old_patch), false);

  if (new_patch!=NULL) {
    PLUGIN_set_from_patch(old_plugin, new_patch);        
    return (SoundPlugin*)new_patch->patchdata;
  } else
    return NULL;
}


void PLUGIN_reset(SoundPlugin *plugin){
  const SoundPluginType *type = plugin->type;
  int i;

  volatile struct Patch *patch = plugin->patch;
  R_ASSERT_RETURN_IF_FALSE(patch!=NULL);
  
  Undo_Open();{
    for(i=0;i<type->num_effects;i++)
      Undo_AudioEffect_CurrPos((struct Patch*)patch, i);
  }Undo_Close();

  PLAYER_lock();{
    for(i=0;i<type->num_effects;i++)
      PLUGIN_set_effect_value(plugin, 0, i, plugin->initial_effect_values[i], PLUGIN_STORED_TYPE, PLUGIN_STORE_VALUE, FX_single);
  }PLAYER_unlock();
}

void PLUGIN_reset_one_effect(SoundPlugin *plugin, int effect_num){
  volatile struct Patch *patch = plugin->patch;
  R_ASSERT_RETURN_IF_FALSE(patch!=NULL);
  
  Undo_AudioEffect_CurrPos((struct Patch*)patch, effect_num);
  PLAYER_lock();{
    PLUGIN_set_effect_value(plugin, 0, effect_num, plugin->initial_effect_values[effect_num], PLUGIN_STORED_TYPE, PLUGIN_STORE_VALUE, FX_single);
  }PLAYER_unlock();
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

  Undo_Open();{
    for(i=0;i<type->num_effects;i++)
      Undo_AudioEffect_CurrPos((struct Patch*)patch, i);
  }Undo_Close();

  float values[type->num_effects];
  for(i=0;i<type->num_effects;i++)
    values[i]=get_rand();
  
  PLAYER_lock();{
    for(i=0;i<type->num_effects;i++)
      PLUGIN_set_effect_value(plugin, 0, i, values[i], PLUGIN_NONSTORED_TYPE, PLUGIN_STORE_VALUE, FX_single);
  };PLAYER_unlock();
}

