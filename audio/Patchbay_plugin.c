
#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <math.h>

#include "../common/nsmtracker.h"
#include "SoundPlugin.h"
#include "SoundPlugin_proc.h"

#include "SoundPluginRegistry_proc.h"

#define NUM_CHANNELS 8

typedef struct{
  int routes[NUM_CHANNELS*NUM_CHANNELS];
} Data;



static void RT_process(SoundPlugin *plugin, int64_t time, int num_frames, float **inputs, float **outputs){
  Data *data = (Data*)plugin->data;
  int touched[NUM_CHANNELS]={0};

  int in_ch;
  for(in_ch=0;in_ch<NUM_CHANNELS;in_ch++){
    int out_ch;
    for(out_ch=0;out_ch<NUM_CHANNELS;out_ch++){
      int onoff = data->routes[in_ch*NUM_CHANNELS + out_ch];
      int i;
      if(onoff==1){
        float *in=inputs[in_ch];
        float *out=outputs[out_ch];
        if(touched[out_ch]==0){
          for(i=0;i<num_frames;i++)
            out[i] = in[i];
          touched[out_ch]=1;
        }else
          for(i=0;i<num_frames;i++)
            out[i] += in[i];
      }
    }
  }

  int out_ch;
  for(out_ch=0;out_ch<NUM_CHANNELS;out_ch++){
    if(touched[out_ch]==0)
      memset(outputs[out_ch],0,sizeof(float)*num_frames);
  }
}

static void set_effect_value(SoundPlugin *plugin, int64_t time, int effect_num, float value, enum ValueFormat value_format){
  Data *data = (Data*)plugin->data;
  printf("####################################################### Setting sine volume to %f\n",value);

  if(value_format==PLUGIN_FORMAT_SCALED)
    data->routes[effect_num] = value > 0.5f;
  else
    data->routes[effect_num] = value;
}

static float get_effect_value(struct SoundPlugin *plugin, int effect_num, enum ValueFormat value_format){
  Data *data = (Data*)plugin->data;
  return data->routes[effect_num];
}

static void *create_plugin_data(const SoundPluginType *plugin_type, struct SoundPlugin *plugin, hash_t *state, float sample_rate, int block_size){
  Data *data = calloc(1,sizeof(Data));
  data->routes[0]=0;
  data->routes[1]=1;
  int i;
  for(i=2;i<NUM_CHANNELS;i++)
    data->routes[i]=-1;
  return data;
}

static void cleanup_plugin_data(SoundPlugin *plugin){
  printf(">>>>>>>>>>>>>> Cleanup_plugin_data called for %p\n",plugin);
  free(plugin->data);
}


static const char *get_effect_name(struct SoundPlugin *plugin, int effect_num){
  static char name[64];
  int from = effect_num / NUM_CHANNELS;
  int to = effect_num % NUM_CHANNELS;
  sprintf(name, "%d->%d",from,to);
  return name;
}

static int get_effect_format(SoundPlugin *plugin, int effect_num){
  return EFFECT_FORMAT_BOOL;
}

static SoundPluginType plugin_type = {
 type_name                : "Patchbay",
 name                     : "Patchbay",
 info                     : "Sends any input channel to any output channel",
 num_inputs               : NUM_CHANNELS,
 num_outputs              : NUM_CHANNELS,
 is_instrument            : false,
 note_handling_is_RT      : false,
 num_effects              : NUM_CHANNELS*NUM_CHANNELS,
 get_effect_format        : get_effect_format,
 get_effect_name          : get_effect_name,
 effect_is_RT             : NULL,
 create_plugin_data       : create_plugin_data,
 cleanup_plugin_data      : cleanup_plugin_data,

 RT_process       : RT_process,
 set_effect_value : set_effect_value,
 get_effect_value : get_effect_value,
 get_display_value_string : NULL,

 data                     : NULL
};

void create_patchbay_plugin(void){
  PR_add_plugin_type(&plugin_type);
}
