
#include <stdlib.h>
#include <string.h>
#include <stdio.h>


// NOTE! The weakjack C file is included here.
//////////////////////////////////////////////
#include "../weakjack/weak_libjack.c"
//////////////////////////////////////////////


#include "../common/nsmtracker.h"
#include "../common/OS_visual_input.h"
#include "../common/visual_proc.h"

#include "SoundPlugin.h"
#include "SoundPlugin_proc.h"
#include "SoundfileSaver_proc.h"
#include "Mixer_proc.h"

#include "SoundPluginRegistry_proc.h"

#include "Jack_plugin_proc.h"

/*
  Note that plugin_type->num_input corresponds to jack_client->num_outputs, and vica versa.
*/

typedef struct{
  jack_client_t *client;
  jack_port_t **input_ports;
  jack_port_t **output_ports;
} Data;

static Data *create_data(const SoundPluginType *plugin_type, jack_client_t *client, int num_inputs, int num_outputs, const char **input_portnames, const char **output_portnames){
  Data *data = (Data*)V_calloc(1,sizeof(Data));
  data->client = client;
  data->input_ports=(jack_port_t**)V_calloc(num_outputs,sizeof(jack_port_t*));
  data->output_ports=(jack_port_t**)V_calloc(num_inputs,sizeof(jack_port_t*));

  int i;

  for(i=0;i<num_outputs;i++){
    static int n=0;
    char temp[500];

    if(input_portnames[i] != NULL)
      sprintf(temp, "%s", input_portnames[i]);
    else
      sprintf(temp, "in_%d",++n);
    
    if((data->input_ports[i] = jack_port_register(client,
                                                  V_strdup(temp),
                                                  JACK_DEFAULT_AUDIO_TYPE,
                                                  JackPortIsInput,
                                                  0
                                                  )
        )==NULL)
      GFX_Message(NULL, "Error. Could not register jack port.\n");
  }

  if(!strcmp(plugin_type->name,"System In")){
    int ch;
    const char **outportnames=jack_get_ports(client,NULL,NULL,JackPortIsPhysical|JackPortIsOutput);
    for (ch=0;outportnames && outportnames[ch]!=NULL && ch<num_outputs;ch++){
      if (
          data->input_ports[ch]!= NULL &&
	  jack_connect(
                       client,
		       outportnames[ch],
		       jack_port_name(data->input_ports[ch])
		       )
	  )
        GFX_Message(NULL, "Warning. Cannot connect to jack capture port %d: \"%s\".\n",ch,outportnames[ch]);
    }
    jack_free(outportnames);
  }

  for(i=0;i<num_inputs;i++){
    static int n=0;
    char temp[500];

    if(output_portnames[i] != NULL)
      sprintf(temp, "%s",output_portnames[i]);
    else
      sprintf(temp, "out_%d",++n);

    if((data->output_ports[i] = jack_port_register(
                                                   client,
                                                   V_strdup(temp),
                                                   JACK_DEFAULT_AUDIO_TYPE,
                                                   JackPortIsOutput,
                                                   0
                                                 )
        )==NULL)
      GFX_Message(NULL, "Error. Could not register jack port.\n");
  }

  if(!strcmp(plugin_type->name,"System Out") || !strcmp(plugin_type->name,"System Out 8")){
    int ch;
    const char **inportnames=jack_get_ports(client,NULL,NULL,JackPortIsPhysical|JackPortIsInput);
    for (ch=0;inportnames && inportnames[ch]!=NULL && ch<num_inputs;ch++){
      if (
          data->output_ports[ch] != NULL &&
	  jack_connect(
                       client,
		       jack_port_name(data->output_ports[ch]),
		       inportnames[ch]
		       )
	  )
        GFX_addMessage("Warning. Cannot connect to jack playback port %d: \"%s\".\n",ch,inportnames[ch]);
    }
    jack_free(inportnames);
  }


  return data;
}


extern jack_time_t g_jackblock_delta_time;


static void RT_process(SoundPlugin *plugin, int64_t time, int num_frames, float **inputs, float **outputs){
  const SoundPluginType *type = plugin->type;
  Data *data = static_cast<Data*>(plugin->data);

#if 0
  if(!strcmp(type->name,"System Out")){
    plugin->system_volume_peak_values = GFX_OS_get_system_volume_peak_pointers(type->num_inputs);
    printf("%p\n",plugin->system_volume_peak_values);
  }
#endif

  if(MIXER_is_saving()){

    if (GFX_OS_patch_is_system_out((struct Patch*)plugin->patch))
      SOUNDFILESAVER_write(inputs, type->num_inputs, num_frames);
    
  }

  for(int ch=0;ch<type->num_inputs;ch++)
    if (data->output_ports[ch]!=NULL)
      memcpy(((float*)jack_port_get_buffer(data->output_ports[ch],g_jackblock_size))+g_jackblock_delta_time,
             inputs[ch],
             sizeof(float)*num_frames);
  
  for(int ch=0;ch<type->num_outputs;ch++)
    if (data->input_ports[ch]!=NULL)
      memcpy(outputs[ch],
             ((float*)jack_port_get_buffer(data->input_ports[ch],g_jackblock_size))+g_jackblock_delta_time,
             sizeof(float)*num_frames);
}


void PLAYER_volumeUp(float db){
  OS_GFX_IncVolume(500); // 5 percent
}

void PLAYER_volumeDown(float db){
  OS_GFX_IncVolume(-500); // -5 percent
}

void PLAYER_mute(void){
  OS_GFX_SetVolume(0);
}

void *create_plugin_data(const SoundPluginType *plugin_type, struct SoundPlugin *plugin, hash_t *state, float sample_rate, int block_size, bool is_loading){
  const char *input_portnames[R_MAX(1, plugin_type->num_outputs)]; // R_MAX is here to make the undefined sanitizer be quiet
  const char *output_portnames[R_MAX(1, plugin_type->num_inputs)]; // R_MAX is here to make the undefined sanitizer be quiet
  int i;
  for(i=0;i<plugin_type->num_outputs;i++)
    input_portnames[i] = state==NULL ? NULL : HASH_get_chars_at(state, "input_portname",i);
  for(i=0;i<plugin_type->num_inputs;i++)
    output_portnames[i] = state==NULL ? NULL : HASH_get_chars_at(state, "output_portname",i);

  return create_data(plugin_type,
                     (jack_client_t*)plugin_type->data,
                     plugin_type->num_inputs,
                     plugin_type->num_outputs,
                     input_portnames,
                     output_portnames
                     );
}

static void called_after_system_out_has_been_created(const SoundPluginType *plugin_type, struct SoundPlugin *plugin){
  //GFX_OS_set_system_volume_peak_pointers(plugin->input_volume_peak_values, plugin_type->num_inputs);
  
  GFX_OS_set_system_volume_plugin(plugin);
}
  
static void cleanup_plugin_data(SoundPlugin *plugin){
  int i;
  Data *data = static_cast<Data*>(plugin->data);

  if(!strcmp(plugin->type->name,"System Out") || !strcmp(plugin->type->name,"System Out 8")){
    struct SoundPlugin *other_system_out = MIXER_get_soundplugin("Jack", "System Out");
    if (other_system_out != NULL){
      GFX_OS_set_system_volume_plugin(other_system_out);
    } else {
      struct SoundPlugin *other_system_out = MIXER_get_soundplugin("Jack", "System Out 8");
      GFX_OS_set_system_volume_plugin(other_system_out);
    }
  }

  for(i=0;i<plugin->type->num_outputs;i++)
    if(data->input_ports[i]!=NULL)
      jack_port_unregister(data->client,data->input_ports[i]);

  for(i=0;i<plugin->type->num_inputs;i++)
    if(data->output_ports[i]!=NULL)
      jack_port_unregister(data->client,data->output_ports[i]);

  V_free(data->input_ports);
  V_free(data->output_ports);

  V_free(data);
}

static void create_state(struct SoundPlugin *plugin, hash_t *state){
  if(plugin->type->num_outputs>0) {
    for(int i=0 ; i<plugin->type->num_outputs ; i++)
      HASH_put_chars_at(state, "input_portname", i, JACK_get_name(plugin,i));
  } else {
    for(int i=0 ; i<plugin->type->num_inputs ; i++)
      HASH_put_chars_at(state, "output_portname", i, JACK_get_name(plugin,i));
  }
}

static SoundPluginType stereo_in_type = {
 type_name                : "Jack",
 name                     : "Jack Stereo In",
 num_inputs               : 0,
 num_outputs              : 2,
 is_instrument            : false,
 note_handling_is_RT      : false,
 num_effects              : 0,
 will_never_autosuspend   : true,
 get_effect_format        : NULL,
 get_effect_name          : NULL,
 get_display_value_string : NULL,
 effect_is_RT             : NULL,
 create_plugin_data       : create_plugin_data,
 cleanup_plugin_data      : cleanup_plugin_data,

 create_state        : create_state,

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
 will_never_autosuspend   : true,
 get_effect_format        : NULL,
 get_effect_name          : NULL,
 get_display_value_string : NULL,
 effect_is_RT             : NULL,
 create_plugin_data       : create_plugin_data,
 cleanup_plugin_data      : cleanup_plugin_data,

 create_state        : create_state,

 RT_process       : RT_process,
 play_note        : NULL,
 set_note_volume  : NULL,
 stop_note        : NULL,
 set_effect_value : NULL,

 data                     : NULL
};

static SoundPluginType jack8_in_type = {
 type_name                : "Jack",
 name                     : "Jack 8ch In",
 num_inputs               : 0,
 num_outputs              : 8,
 is_instrument            : false,
 note_handling_is_RT      : false,
 num_effects              : 0,
 will_never_autosuspend   : true,
 get_effect_format        : NULL,
 get_effect_name          : NULL,
 get_display_value_string : NULL,
 effect_is_RT             : NULL,
 create_plugin_data       : create_plugin_data,
 cleanup_plugin_data      : cleanup_plugin_data,

 create_state        : create_state,

 RT_process       : RT_process,
 play_note        : NULL,
 set_note_volume  : NULL,
 stop_note        : NULL,
 set_effect_value : NULL,

 data                     : NULL
};

static SoundPluginType jack8_out_type = {
 type_name                : "Jack",
 name                     : "Jack 8ch Out",
 num_inputs               : 8,
 num_outputs              : 0,
 is_instrument            : false,
 note_handling_is_RT      : false,
 num_effects              : 0,
 will_never_autosuspend   : true,
 get_effect_format        : NULL,
 get_effect_name          : NULL,
 get_display_value_string : NULL,
 effect_is_RT             : NULL,
 create_plugin_data       : create_plugin_data,
 cleanup_plugin_data      : cleanup_plugin_data,

 create_state        : create_state,

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
 will_never_autosuspend   : true,
 get_effect_format        : NULL,
 get_effect_name          : NULL,
 get_display_value_string : NULL,
 effect_is_RT             : NULL,
 create_plugin_data       : create_plugin_data,
 cleanup_plugin_data      : cleanup_plugin_data,

 create_state        : create_state,

 RT_process       : RT_process,
 play_note        : NULL,
 set_note_volume  : NULL,
 stop_note        : NULL,
 set_effect_value : NULL,

 data                     : NULL
};

static SoundPluginType system_in_type8 = {
 type_name                : "Jack",
 name                     : "System In 8",
 num_inputs               : 0,
 num_outputs              : 8,
 is_instrument            : false,
 note_handling_is_RT      : false,
 num_effects              : 0,
 will_never_autosuspend   : true,
 get_effect_format        : NULL,
 get_effect_name          : NULL,
 get_display_value_string : NULL,
 effect_is_RT             : NULL,
 create_plugin_data       : create_plugin_data,
 cleanup_plugin_data      : cleanup_plugin_data,

 create_state        : create_state,

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
 will_never_autosuspend   : true,
 get_effect_format        : NULL,
 get_effect_name          : NULL,
 get_display_value_string : NULL,
 effect_is_RT             : NULL,
 create_plugin_data       : create_plugin_data,
 cleanup_plugin_data      : cleanup_plugin_data,

 called_after_plugin_has_been_created : called_after_system_out_has_been_created,
 
 create_state        : create_state,

 RT_process       : RT_process,
 play_note        : NULL,
 set_note_volume  : NULL,
 stop_note        : NULL,
 set_effect_value : NULL,

 data                     : NULL
};

static SoundPluginType system_out_type8 = {
 type_name                : "Jack",
 name                     : "System Out 8",
 num_inputs               : 8,
 num_outputs              : 0,
 is_instrument            : false,
 note_handling_is_RT      : false,
 num_effects              : 0,
 will_never_autosuspend   : true,
 get_effect_format        : NULL,
 get_effect_name          : NULL,
 get_display_value_string : NULL,
 effect_is_RT             : NULL,
 create_plugin_data       : create_plugin_data,
 cleanup_plugin_data      : cleanup_plugin_data,

 called_after_plugin_has_been_created : called_after_system_out_has_been_created,
 
 create_state        : create_state,

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

  jack8_in_type.data = g_jack_client;
  PR_add_plugin_type(&jack8_in_type);

  stereo_out_type.data = g_jack_client;
  PR_add_plugin_type(&stereo_out_type);

  jack8_out_type.data = g_jack_client;
  PR_add_plugin_type(&jack8_out_type);

  //PR_add_menu_entry(PluginMenuEntry::separator());
  
  system_in_type.data = g_jack_client;
  PR_add_plugin_type(&system_in_type);

  system_in_type8.data = g_jack_client;
  PR_add_plugin_type(&system_in_type8);

  system_out_type.data = g_jack_client;
  PR_add_plugin_type(&system_out_type);

  system_out_type8.data = g_jack_client;
  PR_add_plugin_type(&system_out_type8);
}

const char *JACK_get_name(SoundPlugin *plugin, int portnum){
  Data *data = static_cast<Data*>(plugin->data);

  if(plugin->type->num_outputs>0)
    return jack_port_short_name(data->input_ports[portnum]);
  else
    return jack_port_short_name(data->output_ports[portnum]);
}

void JACK_set_name(SoundPlugin *plugin, int portnum, const char *new_name){
  Data *data = static_cast<Data*>(plugin->data);

  if(plugin->type->num_inputs>0)
    jack_port_set_name(data->output_ports[portnum], new_name);
  else
    jack_port_set_name(data->input_ports[portnum], new_name);
}

