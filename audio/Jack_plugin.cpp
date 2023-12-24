
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
#include "Juce_plugins_proc.h"

#include "SoundPluginRegistry_proc.h"

#include "../api/api_proc.h"


#include "Jack_plugin_proc.h"



/*
  Note that plugin_type->num_input corresponds to jack_client->num_outputs, and vica versa.
*/

namespace{
  
struct Data {
  jack_client_t *client; // Is NULL if jack is not used.
  jack_port_t **input_ports;
  jack_port_t **output_ports;
  
  const char **juce_input_portnames; // Need to store jack port names when using juce
  const char **juce_output_portnames; // Need to store jack port names when using juce
  
  bool is_system_in_or_out;
};

}
 
static Data *create_data(const SoundPluginType *plugin_type, jack_client_t *client, int num_inputs, int num_outputs, const char **input_portnames, const char **output_portnames, bool is_system_in_or_out){
  Data *data = (Data*)V_calloc(1,sizeof(Data));
  
  data->client = client;
  
  data->input_ports=(jack_port_t**)V_calloc(num_outputs,sizeof(jack_port_t*));
  data->output_ports=(jack_port_t**)V_calloc(num_inputs,sizeof(jack_port_t*));
  
  data->juce_input_portnames=(const char**)V_calloc(num_outputs,sizeof(const char*));
  data->juce_output_portnames=(const char**)V_calloc(num_inputs,sizeof(const char*));
  
  data->is_system_in_or_out = is_system_in_or_out;


  if (data->client==NULL)
    R_ASSERT(is_system_in_or_out==true);
  

  for(int i=0;i<num_outputs;i++){
    static int n=0;
    char temp[500];

    if(input_portnames[i] != NULL)
	    snprintf(temp,499, "%s", input_portnames[i]);
    else
	    snprintf(temp,499, "in_%d",++n);

    if (client==NULL){
    
      data->juce_input_portnames[i] = V_strdup(temp);
                                               
    } else {
      
      data->input_ports[i] = jack_port_register(client,
                                                V_strdup(temp),
                                                JACK_DEFAULT_AUDIO_TYPE,
                                                JackPortIsInput,
                                                0
                                                );
      if(data->input_ports[i]==NULL)
        GFX_Message(NULL, "Error. Could not register jack input port \"%s\".\n", temp);

    }
  }

  if(data->client != NULL && !strcmp(plugin_type->name,"System In") && !nsmIsActive()){
    
    const char **outportnames=jack_get_ports(client,NULL,NULL,JackPortIsPhysical|JackPortIsOutput);

    if (outportnames != NULL) {

      for(int portnum=0, ch=0 ; ch < num_outputs && outportnames[portnum] != NULL ; portnum++){
        
        jack_port_t *physical_port = jack_port_by_name(client, outportnames[portnum]);
        
        if (physical_port==NULL){
          
          R_ASSERT(false);
          
        } else {

          if (data->input_ports[ch] == NULL) {
            
            ch++;
            
          } else {
            
            const char *radium_port_name = jack_port_name(data->input_ports[ch]);
            
            if (radium_port_name==NULL){
              
              R_ASSERT(false);
              
            } else {
              
              if (!strcmp(jack_port_type(physical_port), jack_port_type(data->input_ports[ch]))){
                if (0 != jack_connect(client,
                                      outportnames[portnum],
                                      radium_port_name
                                      ))
                  {
                    GFX_Message(NULL, "Warning. Could not connect to jack capture port %d: \"%s\".\n",ch,outportnames[portnum]);
                  }
                ch++;
              }
              
            }
            
          }

        }
        
      }

      jack_free(outportnames);

    }

#if 0          
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
#endif
    
  }

  for(int i=0;i<num_inputs;i++){
    static int n=0;
    char temp[500];

    if(output_portnames[i] != NULL)
      sprintf(temp, "%s",output_portnames[i]);
    else
      sprintf(temp, "out_%d",++n);

    printf("\n\nI: %d. num_inputs: %d. portnames[i]: \"%s\". temp: \"%s\"\n", i, num_inputs, output_portnames[i], temp);

    if (data->client == NULL) {

      data->juce_output_portnames[i] = V_strdup(temp);
      
    } else {
      
      data->output_ports[i] = jack_port_register(
                                                 client,
                                                 V_strdup(temp),
                                                 JACK_DEFAULT_AUDIO_TYPE,
                                                 JackPortIsOutput,
                                                 0
                                                 );
      if(data->output_ports[i]==NULL)
        GFX_Message(NULL, "Error. Could not register jack output port \"%s\".\n", temp);

    }
    
  }

  if(data->client!=NULL && !nsmIsActive() && (!strcmp(plugin_type->name,"System Out") || !strcmp(plugin_type->name,"System Out 8"))){

    const char **inportnames=jack_get_ports(client,NULL,NULL,JackPortIsPhysical|JackPortIsInput);

    if (inportnames != NULL) {

      for(int portnum=0, ch=0 ; ch < num_inputs && inportnames[portnum] != NULL ; portnum++){
        
        jack_port_t *physical_port = jack_port_by_name(client, inportnames[portnum]);
        
        if (physical_port==NULL){
          
          R_ASSERT(false);
          
        } else {

          if (g_audio_system_output_latency == 0){
            jack_latency_range_t range;
            jack_port_get_latency_range(physical_port, JackPlaybackLatency, &range);
            g_audio_system_output_latency = R_MAX(0, (int)range.max);
          }

          if (data->output_ports[ch] == NULL) {
            
            ch++;
            
          } else {
            
            const char *radium_port_name = jack_port_name(data->output_ports[ch]);
            
            if (radium_port_name==NULL){
              
              R_ASSERT(false);
              
            } else {

              if (!strcmp(jack_port_type(physical_port), jack_port_type(data->output_ports[ch]))){
                if (0 != jack_connect(client,
                                      radium_port_name,
                                      inportnames[portnum]
                                      ))
                  {
                    GFX_Message(NULL, "Warning. Could not connect to jack playback port %d: \"%s\".\n",ch,inportnames[portnum]);
                  }
                ch++;
              }
              
            }
            
          }

        }
        
      }

      jack_free(inportnames);

    }
    
#if 0
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
#endif
    
  }


  return data;
}


#define NUM_MUTEXES 16
static radium::Mutex *g_audio_out_mutexes = NULL;


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

    if (GFX_OS_patch_is_system_out((struct Patch*)plugin->patch)) {

      // Note: No need for lock here. 'GFX_OS_patch_is_system_out' only returns true for one System Out instance, the one connected to the bottom bar volume control.
      // (Also, SOUNDFILESAVER_write can not be called more than one time per audio block anyway, even if we had a lock here.)
      SOUNDFILESAVER_write(inputs, type->num_inputs, num_frames);
      
    }
        
    if (data->client==NULL) {
      
      for(int ch=0 ; ch<type->num_outputs ; ch++)
        memset(outputs[ch], 0, sizeof(float)*num_frames);
      
      return;
    }
  }

  if (MIXER_dummy_driver_is_running()) {
    
    for(int ch=0 ; ch<type->num_outputs ; ch++)
      memset(outputs[ch], 0, sizeof(float)*num_frames);
    
    return;
  }
  
  if (data->client==NULL) {

    R_ASSERT_NON_RELEASE(data->is_system_in_or_out);

    const int min_num_audio_out_ch = R_MIN(type->num_inputs, g_juce_num_output_audio_channels);
    
    for(int ch=0 ; ch<min_num_audio_out_ch ; ch++){
      
      if (g_juce_output_audio_channels[ch]!=NULL){
        
        radium::ScopedMutex lock(g_audio_out_mutexes[ch % NUM_MUTEXES]);

        //if (ch==0) printf("Writing to ch %d. delta: %d. num_frames: %d\n", ch, g_soundcardblock_delta_time, num_frames);
        JUCE_add_sound(g_juce_output_audio_channels[ch] + g_soundcardblock_delta_time,
                       inputs[ch],
                       num_frames
                       );
      }
    }

    const int min_num_audio_in_ch = R_MIN(type->num_outputs, g_juce_num_input_audio_channels);
    
    for(int ch=0 ; ch<min_num_audio_in_ch ; ch++)
      if (g_juce_input_audio_channels[ch]!=NULL){
        memcpy(outputs[ch],
               g_juce_input_audio_channels[ch]+g_soundcardblock_delta_time,
               sizeof(float)*num_frames
               );
      }
    
    for(int ch=0 ; ch<type->num_outputs ; ch++)
      if (ch < g_juce_num_input_audio_channels && g_juce_input_audio_channels[ch]!=NULL)
        memcpy(outputs[ch],
               g_juce_input_audio_channels[ch]+g_soundcardblock_delta_time,
               sizeof(float)*num_frames
               );
      else
        memset(outputs[ch], 0, sizeof(float)*num_frames);
    
  } else {
    
    for(int ch=0;ch<type->num_inputs;ch++)
      if (data->output_ports[ch]!=NULL)
        memcpy(((float*)jack_port_get_buffer(data->output_ports[ch],g_soundcardblock_size))+g_soundcardblock_delta_time,
               inputs[ch],
               sizeof(float)*num_frames
               );
    
    for(int ch=0;ch<type->num_outputs;ch++)
      if (data->input_ports[ch]!=NULL)
        memcpy(outputs[ch],
               ((float*)jack_port_get_buffer(data->input_ports[ch],g_soundcardblock_size))+g_soundcardblock_delta_time,
               sizeof(float)*num_frames
               );

  }
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

static void *create_plugin_data(const SoundPluginType *plugin_type, struct SoundPlugin *plugin, hash_t *state, float sample_rate, int block_size, bool is_loading, bool is_system_in_or_out){

  const char *input_portnames[R_MAX(1, plugin_type->num_outputs)]; // R_MAX is here to make the undefined sanitizer be quiet
  const char *output_portnames[R_MAX(1, plugin_type->num_inputs)]; // R_MAX is here to make the undefined sanitizer be quiet
  int i;
  for(i=0;i<plugin_type->num_outputs;i++)
    input_portnames[i] = state==NULL ? NULL : HASH_get_chars_at(state, "input_portname",i);
  for(i=0;i<plugin_type->num_inputs;i++){
    output_portnames[i] = state==NULL ? NULL : HASH_get_chars_at(state, "output_portname",i);
    printf("        INIT. Output portname[%d]: \"%s\"\n", i, output_portnames[i]);
  }
  
  return create_data(plugin_type,
                     (jack_client_t*)plugin_type->data,
                     plugin_type->num_inputs,
                     plugin_type->num_outputs,
                     input_portnames,
                     output_portnames,
                     is_system_in_or_out
                     );
}

static void *create_plugin_data_nonsystem(const SoundPluginType *plugin_type, struct SoundPlugin *plugin, hash_t *state, float sample_rate, int block_size, bool is_loading){
  return create_plugin_data(plugin_type, plugin, state, sample_rate, block_size, is_loading, false);
}

static void *create_plugin_data_system(const SoundPluginType *plugin_type, struct SoundPlugin *plugin, hash_t *state, float sample_rate, int block_size, bool is_loading){
  return create_plugin_data(plugin_type, plugin, state, sample_rate, block_size, is_loading, true);
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
      GFX_OS_set_system_volume_plugin(other_system_out); // "other_system_out" might be NULL.
    }
  }

  if (data->client != NULL) {
    
    for(i=0;i<plugin->type->num_outputs;i++)
      if(data->input_ports[i]!=NULL)
        jack_port_unregister(data->client,data->input_ports[i]);
    
    for(i=0;i<plugin->type->num_inputs;i++)
      if(data->output_ports[i]!=NULL)
        jack_port_unregister(data->client,data->output_ports[i]);
    
  }
  
  V_free(data->input_ports);
  V_free(data->output_ports);

  for(int i=0;i<plugin->type->num_outputs;i++)
    V_free((void*)data->juce_input_portnames[i]);
    
  for(int i=0;i<plugin->type->num_inputs;i++)
    V_free((void*)data->juce_output_portnames[i]);

  V_free(data->juce_input_portnames);
  V_free(data->juce_output_portnames);

  V_free(data);
}

static void create_state(const struct SoundPlugin *plugin, hash_t *state){
  if(plugin->type->num_outputs>0) {
    for(int i=0 ; i<plugin->type->num_outputs ; i++)
      HASH_put_chars_at(state, "input_portname", i, JACK_get_name(plugin,i));
  } else {
    for(int i=0 ; i<plugin->type->num_inputs ; i++)
      HASH_put_chars_at(state, "output_portname", i, JACK_get_name(plugin,i));
  }
}

static SoundPluginType stereo_in_type = {};

static SoundPluginType stereo_out_type = {};

static SoundPluginType jack8_in_type = {};

static SoundPluginType jack8_out_type = {};

static SoundPluginType system_in_type = {};

static SoundPluginType system_in_type8 = {};

static SoundPluginType system_out_type = {};

static SoundPluginType system_out_type8 = {};


static void init_types(void){

  
 stereo_in_type.type_name                = "Jack";
 stereo_in_type.name                     = "Jack Stereo In";
 stereo_in_type.num_inputs               = 0;
 stereo_in_type.num_outputs              = 2;
 stereo_in_type.is_instrument            = false;
 stereo_in_type.note_handling_is_RT      = false;
 stereo_in_type.num_effects              = 0;
 stereo_in_type.will_never_autosuspend   = true;
 stereo_in_type.create_plugin_data       = create_plugin_data_nonsystem;
 stereo_in_type.cleanup_plugin_data      = cleanup_plugin_data;

 stereo_in_type.create_state        = create_state;

 stereo_in_type.RT_process       = RT_process;



 
 stereo_out_type.type_name                = "Jack";
 stereo_out_type.name                     = "Jack Stereo Out";
 stereo_out_type.num_inputs               = 2;
 stereo_out_type.num_outputs              = 0;
 stereo_out_type.is_instrument            = false;
 stereo_out_type.note_handling_is_RT      = false;
 stereo_out_type.num_effects              = 0;
 stereo_out_type.will_never_autosuspend   = true;
 stereo_out_type.create_plugin_data       = create_plugin_data_nonsystem;
 stereo_out_type.cleanup_plugin_data      = cleanup_plugin_data;

 stereo_out_type.create_state        = create_state;

 stereo_out_type.RT_process       = RT_process;


    

 jack8_in_type.type_name                = "Jack";
 jack8_in_type.name                     = "Jack 8ch In";
 jack8_in_type.num_inputs               = 0;
 jack8_in_type.num_outputs              = 8;
 jack8_in_type.is_instrument            = false;
 jack8_in_type.note_handling_is_RT      = false;
 jack8_in_type.num_effects              = 0;
 jack8_in_type.will_never_autosuspend   = true;
 jack8_in_type.create_plugin_data       = create_plugin_data_nonsystem;
 jack8_in_type.cleanup_plugin_data      = cleanup_plugin_data;

 jack8_in_type.create_state        = create_state;

 jack8_in_type.RT_process       = RT_process;


    

 jack8_out_type.type_name                = "Jack";
 jack8_out_type.name                     = "Jack 8ch Out";
 jack8_out_type.num_inputs               = 8;
 jack8_out_type.num_outputs              = 0;
 jack8_out_type.is_instrument            = false;
 jack8_out_type.note_handling_is_RT      = false;
 jack8_out_type.num_effects              = 0;
 jack8_out_type.will_never_autosuspend   = true;
 jack8_out_type.create_plugin_data       = create_plugin_data_nonsystem;
 jack8_out_type.cleanup_plugin_data      = cleanup_plugin_data;

 jack8_out_type.create_state        = create_state;

 jack8_out_type.RT_process       = RT_process;


 

 system_in_type.type_name                = "Jack";
 system_in_type.name                     = "System In";
 system_in_type.num_inputs               = 0;
 system_in_type.num_outputs              = 2;
 system_in_type.is_instrument            = false;
 system_in_type.note_handling_is_RT      = false;
 system_in_type.num_effects              = 0;
 system_in_type.will_never_autosuspend   = true;
 system_in_type.create_plugin_data       = create_plugin_data_system;
 system_in_type.cleanup_plugin_data      = cleanup_plugin_data;

 system_in_type.create_state        = create_state;

 system_in_type.RT_process       = RT_process;


 

 system_in_type8.type_name                = "Jack";
 system_in_type8.name                     = "System In 8";
 system_in_type8.num_inputs               = 0;
 system_in_type8.num_outputs              = 8;
 system_in_type8.is_instrument            = false;
 system_in_type8.note_handling_is_RT      = false;
 system_in_type8.num_effects              = 0;
 system_in_type8.will_never_autosuspend   = true;
 system_in_type8.create_plugin_data       = create_plugin_data_system;
 system_in_type8.cleanup_plugin_data      = cleanup_plugin_data;

 system_in_type8.create_state        = create_state;

 system_in_type8.RT_process       = RT_process;




 
 system_out_type.type_name                = "Jack";
 system_out_type.name                     = "System Out";
 system_out_type.num_inputs               = 2;
 system_out_type.num_outputs              = 0;
 system_out_type.is_instrument            = false;
 system_out_type.note_handling_is_RT      = false;
 system_out_type.num_effects              = 0;
 system_out_type.will_never_autosuspend   = true;
 system_out_type.create_plugin_data       = create_plugin_data_system;
 system_out_type.cleanup_plugin_data      = cleanup_plugin_data;

 system_out_type.called_after_plugin_has_been_created = called_after_system_out_has_been_created;
 
 system_out_type.create_state        = create_state;

 system_out_type.RT_process       = RT_process;




 
 system_out_type8.type_name                = "Jack";
 system_out_type8.name                     = "System Out 8";
 system_out_type8.num_inputs               = 8;
 system_out_type8.num_outputs              = 0;
 system_out_type8.is_instrument            = false;
 system_out_type8.note_handling_is_RT      = false;
 system_out_type8.num_effects              = 0;
 system_out_type8.create_plugin_data       = create_plugin_data_system;
 system_out_type8.cleanup_plugin_data      = cleanup_plugin_data;

 system_out_type8.called_after_plugin_has_been_created = called_after_system_out_has_been_created;
 
 system_out_type8.create_state        = create_state;

 system_out_type8.RT_process       = RT_process;

}


void create_jack_plugins(void){
  static bool has_inited = false;
  if (has_inited==false){
    init_types();

    g_audio_out_mutexes = new radium::Mutex[NUM_MUTEXES];
    
    has_inited = true;
  }

  if (g_jack_client != NULL) {
    stereo_in_type.data = g_jack_client;
    PR_add_plugin_type(&stereo_in_type);
    
    jack8_in_type.data = g_jack_client;
    PR_add_plugin_type(&jack8_in_type);
    
    stereo_out_type.data = g_jack_client;
    PR_add_plugin_type(&stereo_out_type);
    
    jack8_out_type.data = g_jack_client;
    PR_add_plugin_type(&jack8_out_type);
  }
  
  //PR_add_menu_entry(PluginMenuEntry::separator());
  
  system_in_type.data = g_jack_client;
  PR_add_plugin_type(&system_in_type);

  system_in_type8.data = g_jack_client;
  PR_add_plugin_type(&system_in_type8);

  PR_add_menu_entry(PluginMenuEntry::separator());
  
  system_out_type.data = g_jack_client;
  PR_add_plugin_type(&system_out_type);

  system_out_type8.data = g_jack_client;
  PR_add_plugin_type(&system_out_type8);
}

const char *JACK_get_name(const SoundPlugin *plugin, int portnum){
  const Data *data = static_cast<Data*>(plugin->data);

  if(plugin->type->num_outputs>0){
    
    if (data->client == NULL) {

      if (data->juce_input_portnames[portnum] != NULL)
        return data->juce_input_portnames[portnum];
      
    } else {

      if (data->input_ports[portnum]!=NULL){
#if 0 //defined(DEBUG)
        jack_latency_range_t range1,range2;
        jack_port_get_latency_range(data->input_ports[portnum], JackCaptureLatency, &range1);
        jack_port_get_latency_range(data->input_ports[portnum], JackPlaybackLatency, &range2);
        
        return strdup(talloc_format("%s. latency: %d->%d / %d->%d total: %d\n",
                                    jack_port_short_name(data->input_ports[portnum]),
                                    range1.min, range1.max,
                                    range2.min, range2.max,
                                    jack_port_get_total_latency(data->client, data->input_ports[portnum])
                                    ));
#else
        return jack_port_short_name(data->input_ports[portnum]);
#endif      
      }
    }
    
  } else {

    if (data->client == NULL) {

      if (data->juce_output_portnames[portnum] != NULL)
        return data->juce_output_portnames[portnum];
      
    } else {

      if (data->output_ports[portnum] != NULL){
#if 0 //defined(DEBUG)
        jack_latency_range_t range1,range2;
        jack_port_get_latency_range(data->output_ports[portnum], JackCaptureLatency, &range1);
        jack_port_get_latency_range(data->output_ports[portnum], JackPlaybackLatency, &range2);
        
        return strdup(talloc_format("%s. latency: %d->%d / %d->%d total: %d\n",
                                    jack_port_short_name(data->output_ports[portnum]),
                                    range1.min, range1.max,
                                    range2.min, range2.max,
                                    jack_port_get_total_latency(data->client, data->output_ports[portnum])
                                    ));
#else
        return jack_port_short_name(data->output_ports[portnum]);
#endif
      }

    }
    
  }

  return "jack port could not be registered";
}

void JACK_set_name(SoundPlugin *plugin, int portnum, const char *new_name){
  Data *data = static_cast<Data*>(plugin->data);

  if(plugin->type->num_inputs>0){
    
    if (data->client==NULL) {
      
      if (data->juce_output_portnames[portnum] != NULL){
        V_free((void*)data->juce_output_portnames[portnum]);
        data->juce_output_portnames[portnum] = V_strdup(new_name);
      }
      
    } else {
      
      if (data->output_ports[portnum] != NULL)
        jack_port_set_name(data->output_ports[portnum], new_name);
      
    }
    
  }else{
    
    if (data->client==NULL) {
      
      if (data->juce_input_portnames[portnum] != NULL){
        V_free((void*)data->juce_input_portnames[portnum]);
        data->juce_input_portnames[portnum] = V_strdup(new_name);
      }
      
    } else {
      
      if (data->input_ports[portnum] != NULL)
        jack_port_set_name(data->input_ports[portnum], new_name);

    }
    
  }
}

