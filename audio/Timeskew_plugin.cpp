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



#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <math.h>

#include "../common/nsmtracker.h"
#include "SoundPlugin.h"
#include "SoundPlugin_proc.h"
#include "SoundProducer_proc.h"
#include "Mixer_proc.h"
#include "SmoothDelay.hpp"

#include "SoundPluginRegistry_proc.h"

#include "Bus_plugins_proc.h"


#define TIMESKEW_MIN -1000.0
#define TIMESKEW_MAX 1000.0

static SoundPluginType timeskew_type = {0};

namespace{
  
  struct Data {
    float timeskew;
    radium::SmoothDelay delay1;
    radium::SmoothDelay delay2;
    bool compensate_positive_delay = false;
    
    Data()
      : timeskew(0)
      , delay1(TIMESKEW_MAX * MIXER_get_sample_rate() / 1000)
      , delay2(TIMESKEW_MAX * MIXER_get_sample_rate() / 1000)
    {
    }
  };
}


static void RT_pipeskew_process(SoundPlugin *plugin, int64_t time, int num_frames, float **inputs, float **outputs){
  Data *data = (Data*)plugin->data;
  
  if (!data->delay1.RT_process(num_frames, inputs[0], outputs[0]))
    if (outputs[0] != inputs[0])
      memcpy(outputs[0], inputs[0], sizeof(float)*num_frames);
  
  if (!data->delay2.RT_process(num_frames, inputs[1], outputs[1]))
    if (outputs[1] != inputs[1])
      memcpy(outputs[1], inputs[1], sizeof(float)*num_frames);
}

static int RT_get_timeskew_latency(struct SoundPlugin *plugin){
  Data *data = (Data*)plugin->data;
  
  if (data->timeskew < 0)
    return -1 * (data->timeskew * MIXER_get_sample_rate() / 1000);
  else if(data->compensate_positive_delay)
    return (data->timeskew * MIXER_get_sample_rate() / 1000);
  
  return 0;
}

static void *create_timeskew_plugin_data(const SoundPluginType *plugin_type, struct SoundPlugin *plugin, hash_t *state, float sample_rate, int block_size, bool is_loading){
  Data *data = new Data;
  return data;
}

static void cleanup_timeskew_plugin_data(SoundPlugin *plugin){
  Data *data = (Data*)plugin->data;
  delete data;
}

static void set_timeskew_effect_value(struct SoundPlugin *plugin, int time, int effect_num, float value, enum ValueFormat value_format, FX_when when){
  Data *data = (Data*)plugin->data;

  if (effect_num==1) {
    data->compensate_positive_delay = value >= 0.5;
    return;
  }

  if(value_format==EFFECT_FORMAT_SCALED)
    data->timeskew = scale(value, 0, 1, TIMESKEW_MIN, TIMESKEW_MAX);
  else
    data->timeskew = value;

  int delay = 0;
  
  if (data->timeskew > 0)
    delay = data->timeskew * MIXER_get_sample_rate() / 1000;

  data->delay1.setSize(delay);
  data->delay2.setSize(delay);
}

static float get_timeskew_effect_value(struct SoundPlugin *plugin, int effect_num, enum ValueFormat value_format){
  Data *data = (Data*)plugin->data;

  if (effect_num==1)
    return data->compensate_positive_delay ? 1.0 : 0.0;
  
  if(value_format==EFFECT_FORMAT_SCALED)
    return scale(data->timeskew, TIMESKEW_MIN, TIMESKEW_MAX, 0, 1);
  else
    return data->timeskew;
}

static const char *get_timeskew_effect_name(struct SoundPlugin *plugin, int effect_num){
  if (effect_num==0)
    return "Skew";
  else
    return "Report latency, skew > 0";
}

static void get_timeskew_display_value_string(SoundPlugin *plugin, int effect_num, char *buffer, int buffersize){
  Data *data = (Data*)plugin->data;
  if (effect_num==0)
    snprintf(buffer,buffersize-1,"%.2fms",data->timeskew);
}

static int get_timeskew_effect_format(struct SoundPlugin *plugin, int effect_num){
  if (effect_num==0)
    return EFFECT_FORMAT_FLOAT;
  else
    return EFFECT_FORMAT_BOOL;
}


void create_timeskew_plugin(void){
  static bool has_inited = false;

  if (has_inited==false)
  {
    timeskew_type.type_name                = "Timeskew";
    timeskew_type.name                     = "Timeskew";
    timeskew_type.info                     = "Skews time backwards or forwards. Instead of skewing the time of this object backwards, it skews all parallell objects forward instead.";
    timeskew_type.num_inputs               = 2;
    timeskew_type.num_outputs              = 2;
    timeskew_type.is_instrument            = false;
    timeskew_type.num_effects              = 2;
    timeskew_type.create_plugin_data       = create_timeskew_plugin_data;
    timeskew_type.cleanup_plugin_data      = cleanup_timeskew_plugin_data;
    
    timeskew_type.get_effect_format        = get_timeskew_effect_format;
    timeskew_type.get_effect_name          = get_timeskew_effect_name;
    
    timeskew_type.set_effect_value = set_timeskew_effect_value;
    timeskew_type.get_effect_value = get_timeskew_effect_value;
    timeskew_type.get_display_value_string = get_timeskew_display_value_string;
    
    timeskew_type.RT_process               = RT_pipeskew_process;
    timeskew_type.RT_get_latency           = RT_get_timeskew_latency;
  }
  
  has_inited = true;
  
  PR_add_plugin_type(&timeskew_type);
}
