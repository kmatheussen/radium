
#include <stdlib.h>
#include <string.h>
#include <stdio.h>

#include <jack/jack.h>

#include "../common/nsmtracker.h"
#include "../common/OS_visual_input.h"

#include "SoundPlugin.h"
#include "SoundPlugin_proc.h"
#include "SoundfileSaver_proc.h"
#include "Mixer_proc.h"

#include "SoundPluginRegistry_proc.h"

/*
  Note that plugin_type->num_input corresponds to jack_client->num_outputs, and vica versa.
*/

typedef struct{
  jack_client_t *client;
  jack_port_t **input_ports;
  jack_port_t **output_ports;
} Data;

static Data *create_data(const SoundPluginType *plugin_type, jack_client_t *client, int num_inputs, int num_outputs){
  Data *data = calloc(1,sizeof(Data));
  data->client = client;
  data->input_ports=calloc(num_outputs,sizeof(jack_port_t*));
  data->output_ports=calloc(num_inputs,sizeof(jack_port_t*));

  int i;

  for(i=0;i<num_outputs;i++){
    static int n=0;
    char temp[500];
    sprintf(temp, "in_%d",++n);

    if((data->input_ports[i] = jack_port_register(client,
                                                  strdup(temp),
                                                  JACK_DEFAULT_AUDIO_TYPE,
                                                  JackPortIsInput,
                                                  0
                                                  )
        )==NULL)
      fprintf(stderr, "Error. Could not register jack port.\n");

  }

  if(!strcmp(plugin_type->name,"System In")){
    int ch;
    const char **outportnames=jack_get_ports(client,NULL,NULL,JackPortIsPhysical|JackPortIsOutput);
    for (ch=0;outportnames && outportnames[ch]!=NULL && ch<num_outputs;ch++){
      if (
	  jack_connect(
                       client,
		       outportnames[ch],
		       jack_port_name(data->input_ports[ch])
		       )
	  )
	  fprintf(stderr,"Warning. Cannot connect to jack capture port %d: \"%s\".\n",ch,outportnames[ch]);
    }
    jack_free(outportnames);
  }

  for(i=0;i<num_inputs;i++){
    static int n=0;
    char temp[500];
    sprintf(temp, "out_%d",n++);

    if((data->output_ports[i] = jack_port_register(
                                                   client,
                                                   strdup(temp),
                                                   JACK_DEFAULT_AUDIO_TYPE,
                                                   JackPortIsOutput,
                                                   0
                                                 )
        )==NULL)
      fprintf(stderr, "Error. Could not register jack port.\n");
  }

  if(!strcmp(plugin_type->name,"System Out")){
    int ch;
    const char **inportnames=jack_get_ports(client,NULL,NULL,JackPortIsPhysical|JackPortIsInput);
    for (ch=0;inportnames && inportnames[ch]!=NULL && ch<num_inputs;ch++){
      if (
	  jack_connect(
                       client,
		       jack_port_name(data->output_ports[ch]),
		       inportnames[ch]
		       )
	  )
	  fprintf(stderr,"Warning. Cannot connect to jack playback port %d: \"%s\".\n",ch,inportnames[ch]);
    }
    jack_free(inportnames);
  }


  return data;
}

extern int jackblock_size;
extern jack_time_t jackblock_delta_time;

static void RT_process(SoundPlugin *plugin, int64_t time, int num_frames, float **inputs, float **outputs){
  const SoundPluginType *type = plugin->type;
  Data *data = (Data*)plugin->data;

#if 0
  if(!strcmp(type->name,"System Out")){
    plugin->system_volume_peak_values = GFX_OS_get_system_volume_peak_pointers(type->num_inputs);
    printf("%p\n",plugin->system_volume_peak_values);
  }
#endif

  if(MIXER_is_saving()){

    if(!strcmp(type->name,"System Out"))
      SOUNDFILESAVER_write(inputs, num_frames);

  }

  {
    int ch;
    
    for(ch=0;ch<type->num_inputs;ch++)
      memcpy(((float*)jack_port_get_buffer(data->output_ports[ch],jackblock_size))+jackblock_delta_time,
             inputs[ch],
             sizeof(float)*num_frames);
    
    for(ch=0;ch<type->num_outputs;ch++)
      memcpy(outputs[ch],
             ((float*)jack_port_get_buffer(data->input_ports[ch],jackblock_size))+jackblock_delta_time,
             sizeof(float)*num_frames);

  }
}

static SoundPlugin *system_out = NULL;

void PLAYER_volumeUp(float db){
  OS_GFX_IncVolume(500); // 5 percent
}

void PLAYER_volumeDown(float db){
  OS_GFX_IncVolume(-500); // -5 percent
}

void PLAYER_mute(void){
  OS_GFX_SetVolume(0);
}

void *create_plugin_data(const SoundPluginType *plugin_type, struct SoundPlugin *plugin, hash_t *state, float sample_rate, int block_size){
  if(!strcmp(plugin_type->name,"System Out")) {
    GFX_OS_set_system_volume_peak_pointers(&plugin->system_volume_peak_values[0], plugin_type->num_inputs);
    system_out = plugin;
  }
  return create_data(plugin_type, (jack_client_t*)plugin_type->data,plugin_type->num_inputs,plugin_type->num_outputs);
}

static void cleanup_plugin_data(SoundPlugin *plugin){
  int i;
  Data *data = plugin->data;

  if(!strcmp(plugin->type->name,"System Out")) {
    system_out = NULL;
    GFX_OS_set_system_volume_peak_pointers(NULL, plugin->type->num_inputs);
  }

  for(i=0;i<plugin->type->num_outputs;i++)
    jack_port_unregister(data->client,data->input_ports[i]);

  for(i=0;i<plugin->type->num_inputs;i++)
    jack_port_unregister(data->client,data->output_ports[i]);

  free(data->input_ports);
  free(data->output_ports);

  free(data);
}

static SoundPluginType stereo_in_type = {
 type_name                : "Jack",
 name                     : "Jack Stereo In",
 num_inputs               : 0,
 num_outputs              : 2,
 is_instrument            : false,
 note_handling_is_RT      : false,
 num_effects              : 0,
 get_effect_format        : NULL,
 get_effect_name          : NULL,
 get_display_value_string : NULL,
 effect_is_RT             : NULL,
 create_plugin_data       : create_plugin_data,
 cleanup_plugin_data      : cleanup_plugin_data,

 RT_process       : RT_process,
 play_note        : NULL,
 set_note_volume  : NULL,
 stop_note        : NULL,
 set_effect_value : NULL,

 data                     : NULL
};

static SoundPluginType stereo_out_type = {
 type_name                : "Jack",
 name                     : "Jack Stereo Out",
 num_inputs               : 2,
 num_outputs              : 0,
 is_instrument            : false,
 note_handling_is_RT      : false,
 num_effects              : 0,
 get_effect_format        : NULL,
 get_effect_name          : NULL,
 get_display_value_string : NULL,
 effect_is_RT             : NULL,
 create_plugin_data       : create_plugin_data,
 cleanup_plugin_data      : cleanup_plugin_data,

 RT_process       : RT_process,
 play_note        : NULL,
 set_note_volume  : NULL,
 stop_note        : NULL,
 set_effect_value : NULL,

 data                     : NULL
};

static SoundPluginType system_in_type = {
 type_name                : "Jack",
 name                     : "System In",
 num_inputs               : 0,
 num_outputs              : 2,
 is_instrument            : false,
 note_handling_is_RT      : false,
 num_effects              : 0,
 get_effect_format        : NULL,
 get_effect_name          : NULL,
 get_display_value_string : NULL,
 effect_is_RT             : NULL,
 create_plugin_data       : create_plugin_data,
 cleanup_plugin_data      : cleanup_plugin_data,

 RT_process       : RT_process,
 play_note        : NULL,
 set_note_volume  : NULL,
 stop_note        : NULL,
 set_effect_value : NULL,

 data                     : NULL
};

static SoundPluginType system_out_type = {
 type_name                : "Jack",
 name                     : "System Out",
 num_inputs               : 2,
 num_outputs              : 0,
 is_instrument            : false,
 note_handling_is_RT      : false,
 num_effects              : 0,
 get_effect_format        : NULL,
 get_effect_name          : NULL,
 get_display_value_string : NULL,
 effect_is_RT             : NULL,
 create_plugin_data       : create_plugin_data,
 cleanup_plugin_data      : cleanup_plugin_data,

 RT_process       : RT_process,
 play_note        : NULL,
 set_note_volume  : NULL,
 stop_note        : NULL,
 set_effect_value : NULL,

 data                     : NULL
};

extern jack_client_t *g_jack_client;

void create_jack_plugins(void){
  stereo_in_type.data = g_jack_client;
  PR_add_plugin_type(&stereo_in_type);

  stereo_out_type.data = g_jack_client;
  PR_add_plugin_type(&stereo_out_type);

  system_in_type.data = g_jack_client;
  PR_add_plugin_type(&system_in_type);

  system_out_type.data = g_jack_client;
  PR_add_plugin_type(&system_out_type);
}
