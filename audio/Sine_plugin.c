
#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <math.h>

#include "../common/nsmtracker.h"
#include "SoundPlugin.h"
#include "SoundPlugin_proc.h"

#include "SoundPluginRegistry_proc.h"

typedef struct{
  double phase;
  double phase_add;
  double volume;
  double sample_rate;
} Data;



static void RT_process(SoundPlugin *plugin, int64_t time, int num_frames, float **inputs, float **outputs){
  //SoundPluginType *type = plugin->type;
  Data *data = (Data*)plugin->data;
  int i;
  float *out = outputs[0];
  double phase = data->phase;
  double phase_add = data->phase_add;

  for(i=0;i<num_frames;i++){
    out[i] = sin(phase) * data->volume;
    phase += phase_add;
  }

  data->phase = phase;
}

static double hz_to_radians(double hz, double sample_rate){
  return hz*((2*3.14159)/sample_rate);
}

static double midi_to_hz(int midi){
  if(midi<=0)
    return 0;
  else
    return 8.17579891564*(expf(.0577622650*midi));
}

static double midi_to_radians(int midi, double sample_rate){
  return hz_to_radians(midi_to_hz(midi),sample_rate);
}

static void play_note(struct SoundPlugin *plugin, int64_t time, int note_num, float volume){
  Data *data = (Data*)plugin->data;
  data->phase_add = midi_to_radians(note_num,data->sample_rate);
  data->volume = volume;
  printf("####################################################### Setting volume to %f (play note)\n",volume);
}

static void set_note_volume(struct SoundPlugin *plugin, int64_t time, int note_num, float volume){
  Data *data = (Data*)plugin->data;
  data->volume = volume;
  printf("####################################################### Setting volume to %f\n",volume);
}

static void stop_note(struct SoundPlugin *plugin, int64_t time, int note_num, float volume){
  Data *data = (Data*)plugin->data;
  data->volume = 0.0f;
  printf("####################################################### Setting sine volume to %f (stop note)\n",0.0f);
}

static void set_effect_value(struct SoundPlugin *plugin, int64_t time, int effect_num, float value){
  Data *data = (Data*)plugin->data;
  printf("####################################################### Setting sine volume to %f\n",value);
  data->volume = value;
}

static float get_effect_value(struct SoundPlugin *plugin, int effect_num){
  Data *data = (Data*)plugin->data;
  return data->volume;
}

static void get_display_value_string(SoundPlugin *plugin, int effect_num, char *buffer, int buffersize){
  Data *data = (Data*)plugin->data;
  snprintf(buffer,buffersize-1,"%f",data->volume);
}

static void *create_plugin_data(const SoundPluginType *plugin_type, struct SoundPlugin *plugin, float sample_rate, int block_size){
  Data *data = calloc(1,sizeof(Data));
  data->phase = 0.0f;
  data->phase_add = 0.062;
  data->volume = 0.5f;
  printf("####################################################### Setting sine volume to 0.5f (create_plugin_data)\n");
  data->sample_rate = sample_rate;
  return data;
}

static void cleanup_plugin_data(SoundPlugin *plugin){
  printf(">>>>>>>>>>>>>> Cleanup_plugin_data called for %p\n",plugin);
  free(plugin->data);
}

static const char *get_effect_name(const struct SoundPluginType *plugin_type, int effect_num){
  return "Volume";
}

static SoundPluginType plugin_type = {
 type_name                : "Sine Synth",
 name                     : "Sine Synth",
 num_inputs               : 0,
 num_outputs              : 1,
 is_instrument            : true,
 note_handling_is_RT      : false,
 num_effects              : 1,
 get_effect_format        : NULL,
 get_effect_name          : get_effect_name,
 effect_is_RT             : NULL,
 create_plugin_data       : create_plugin_data,
 cleanup_plugin_data      : cleanup_plugin_data,

 RT_process       : RT_process,
 play_note        : play_note,
 set_note_volume  : set_note_volume,
 stop_note        : stop_note,
 set_effect_value : set_effect_value,
 get_effect_value : get_effect_value,
 get_display_value_string : get_display_value_string,

 data                     : NULL
};

void create_sine_plugin(void){
  PR_add_plugin_type(&plugin_type);
}
