/*

  Example plugin.

*/

#include <math.h>

#include "../common/nsmtracker.h"
#include "SoundPlugin.h"
#include "SoundPlugin_proc.h"

#include "SoundPluginRegistry_proc.h"


namespace{
struct Data {
  clap_host host;
  clap_plugin *plugin;
  uint64_t steady_time;
};
}


static void RT_process(SoundPlugin *plugin, int64_t time, int num_frames, float **inputs, float **outputs){
  //SoundPluginType *type = plugin->type;
  Data *data = (Data*)plugin->data;

  data->steady_time = time;
}

static void play_note(struct SoundPlugin *plugin, int64_t time, float note_num, int64_t note_id, float volume,float pan){
  Data *data = (Data*)plugin->data;
  printf("####################################################### Setting volume to %f (play note)\n",volume);
}

static void set_note_volume(struct SoundPlugin *plugin, int64_t time, float note_num, int64_t note_id, float volume){
  Data *data = (Data*)plugin->data;
  printf("####################################################### Setting volume to %f\n",volume);
}

static void stop_note(struct SoundPlugin *plugin, int64_t time, float note_num, int64_t note_id){
  Data *data = (Data*)plugin->data;
  printf("####################################################### Setting sine volume to %f (stop note)\n",0.0f);
}

static void set_effect_value(struct SoundPlugin *plugin, int64_t time, int effect_num, float value, enum ValueFormat value_format, FX_when when){
  radium::PlayerRecursiveLock lock;
  
  Data *data = (Data*)plugin->data;
  printf("####################################################### Setting sine volume to %f\n",value);
}

float get_effect_value(struct SoundPlugin *plugin, int effect_num, enum ValueFormat value_format){
  radium::PlayerRecursiveLock lock;
        
  Data *data = (Data*)plugin->data;
  return data->volume;
}

static void get_display_value_string(SoundPlugin *plugin, int effect_num, char *buffer, int buffersize){
  Data *data = (Data*)plugin->data;
  snprintf(buffer,buffersize-1,"hepp %d",effect_num);
}

static void host_events(struct clap_host   *host,
                        struct clap_plugin *plugin,
                        struct clap_event  *events)
{
}

static void *host_extension(struct clap_host *host, const char *extension_id)
{
  return NULL;
}

static void host_log(struct clap_host       *host,
                     struct clap_plugin     *plugin,
                     enum clap_log_severity  severity,
                     const char             *msg)
{
  const char *severities[] = {
    "debug",
    "info",
    "warning",
    "error",
    "fatal",
  };

  fprintf(stdout, "[%s] %s\n", severities[severity], msg);
}

static void *create_plugin_data(const SoundPluginType *plugin_type, SoundPlugin *plugin, hash_t *state, float sample_rate, int block_size){
  Data *data = (Data*)V_calloc(1,sizeof(Data));
  
  data->host.clap_version  = CLAP_VERSION;
  data->host.events        = host_events;
  data->host.steady_time   = &data->steady_time;
  data->host.extension     = host_extension;
  data->host.get_attribute = host_attribute;
  data->host.log           = host_log;

  return data;
}

static void cleanup_plugin_data(SoundPlugin *plugin){
  printf(">>>>>>>>>>>>>> Cleanup_plugin_data called for %p\n",plugin);
  V_free(plugin->data);
}

static const char *get_effect_name(const struct SoundPlugin *plugin, int effect_num){
  return "Volume";
}


void create_sine_plugin(void){
  SoundPluginType *plugin_type = (SoundPluginType*)V_calloc(1,sizeof(SoundPluginType));

  plugin_type->type_name                = "CLAP";
  plugin_type->name                     = "Sine Synth";
  plugin_type->num_inputs               = 0;
  plugin_type->num_outputs              = 1;
  plugin_type->is_instrument            = true;
  plugin_type->note_handling_is_RT      = false;
  plugin_type->num_effects              = 1;
  plugin_type->get_effect_format        = NULL;
  plugin_type->get_effect_name          = get_effect_name;
  plugin_type->effect_is_RT             = NULL;
  plugin_type->create_plugin_data       = create_plugin_data;
  plugin_type->cleanup_plugin_data      = cleanup_plugin_data;
  
  plugin_type->RT_process       = RT_process;
  plugin_type->play_note        = play_note;
  plugin_type->set_note_volume  = set_note_volume;
  plugin_type->stop_note        = stop_note;
  plugin_type->set_effect_value = set_effect_value;
  plugin_type->get_effect_value = get_effect_value;
  plugin_type->get_display_value_string = get_display_value_string;

  PR_add_plugin_type(plugin_type);
}
