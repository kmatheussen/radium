

#include "../common/nsmtracker.h"
#include "SoundPlugin.h"
#include "SoundPlugin_proc.h"
#include "../Qt/Qt_instruments_proc.h"
#include "../midi/midi_proc.h"
#include "../common/player_proc.h"
#include "../common/patch_proc.h"

#include "SoundPluginRegistry_proc.h"



typedef struct{
  int channel;
  int program_change;
  int channel_aftertouch;
  int pitch_bend;
  int cc[128];
} Data;


enum{
  CHANNEL = 0,
  PROGRAM_CHANGE,
  CHANNEL_AFTERTOUCH,
  PITCH_BEND,
  CC_START
};


static void RT_process(SoundPlugin *plugin, int64_t time, int num_frames, float **inputs, float **outputs){
}


static void send_msg(struct SoundPlugin *plugin, int64_t block_delta_time, uint byte1, uint byte2, int byte3){
  Data *data = (Data*)plugin->data;
  struct Patch *patch = plugin->patch;

  if (patch==NULL) // happens during initialization
    return;
  
  byte1 |= data->channel;

  uint32_t msg;

  if (byte3==-1)
    msg = MIDI_msg_pack2(byte1, byte2);
  else
    msg = MIDI_msg_pack3(byte1, byte2, byte3);

  int64_t delta_time = PLAYER_get_block_delta_time(pc->start_time+block_delta_time);
  int64_t radium_time = pc->start_time + delta_time;

  RT_PATCH_send_raw_midi_message_to_receivers(patch, msg, radium_time);
}

static void get_minval_and_maxval(int effect_num, int *minval, int *maxval){
  *minval = 0;
  
  switch(effect_num){
  case CHANNEL:
    *maxval = 16;
    break;
  case PROGRAM_CHANGE:
    *maxval = 127;
    break;
  case CHANNEL_AFTERTOUCH:
    *maxval = 127;
    break;
  case PITCH_BEND:
    *minval = -8192;
    *maxval = 8191;
    break;
  default:
    *maxval = 127;
    break;
  }
}

static void RT_set_effect_value(struct SoundPlugin *plugin, int64_t block_delta_time, int effect_num, float floatvalue, enum ValueFormat value_format, FX_when when){
  Data *data = (Data*)plugin->data;

  int minval;
  int maxval;
  
  get_minval_and_maxval(effect_num, &minval, &maxval);
  
  int value;
  
  if(value_format==PLUGIN_FORMAT_SCALED)
    value = scale(floatvalue, 0, 1, minval, maxval);
  else
    value = floatvalue;

  value = R_BOUNDARIES(minval, value, maxval);
    
  switch(effect_num){
  case CHANNEL:
    data->channel = value;
    break;
  case PROGRAM_CHANGE:
    data->program_change = value;
    send_msg(plugin, block_delta_time, 0xc0, value, -1);
    break;
  case CHANNEL_AFTERTOUCH:
    data->channel_aftertouch = value;
    send_msg(plugin, block_delta_time, 0xd0, value, -1);
    break;
  case PITCH_BEND:
    {
      data->pitch_bend = value;
      uint32_t val = value;
      val += 0x2000;
      send_msg(plugin, block_delta_time, 0xe0, val&127, val>>7);
      break;
      }
  default:
    {
      int ccnum = effect_num - CC_START;
      data->cc[ccnum] = value;
      send_msg(plugin, block_delta_time, 0xb0, ccnum, value);
    }
  }    
}

float RT_get_effect_value(struct SoundPlugin *plugin, int effect_num, enum ValueFormat value_format){
  Data *data = (Data*)plugin->data;

  int value;
  
  switch(effect_num){
  case CHANNEL:
    value = data->channel;
    break;
  case PROGRAM_CHANGE:
    value = data->program_change;
    break;
  case CHANNEL_AFTERTOUCH:
    value = data->channel_aftertouch;
    break;
  case PITCH_BEND:
    value = data->pitch_bend;
    break;
  default:
    {
      int ccnum = effect_num - CC_START;
      value = data->cc[ccnum];
    }
  }    

  if(value_format==PLUGIN_FORMAT_SCALED) {
    
    int minval;
    int maxval;
    get_minval_and_maxval(effect_num, &minval, &maxval);
    return scale(value, minval, maxval, 0, 1);
    
  } else {

    return value;

  }
}

static void get_display_value_string(SoundPlugin *plugin, int effect_num, char *buffer, int buffersize){
  snprintf(buffer,buffersize-1,"%d",(int)RT_get_effect_value(plugin, effect_num, PLUGIN_FORMAT_NATIVE));
}

static int get_effect_format(struct SoundPlugin *plugin, int effect_num){
  return EFFECT_FORMAT_INT;
}

static const char *get_effect_name(struct SoundPlugin *plugin, int effect_num){
  switch(effect_num){
  case CHANNEL:
    return "Channel";
  case PROGRAM_CHANGE:
    return "Program change";
  case CHANNEL_AFTERTOUCH:
    return "Channel aftertouch";
  case PITCH_BEND:
    return "Pitch bend";
  default:
    {
      int ccnum = effect_num - CC_START;
      const char **ccnames = get_ccnames();
      return ccnames[ccnum];
    }
  }    
}


static void *create_plugin_data(const SoundPluginType *plugin_type, SoundPlugin *plugin, hash_t *state, float sample_rate, int block_size){
  Data *data = (Data*)calloc(1,sizeof(Data));
  return data;
}

static void cleanup_plugin_data(SoundPlugin *plugin){
  free(plugin->data);
}


void create_midisend_plugin(void){
  SoundPluginType *plugin_type = (SoundPluginType*)calloc(1,sizeof(SoundPluginType));

  plugin_type->type_name                = "MIDI Messages";
  plugin_type->name                     = "MIDI Messages";
  plugin_type->info                     = "MIDI Messages makes it possible to send MIDI messages to other sound objects";
  plugin_type->num_inputs               = 0;
  plugin_type->num_outputs              = 0;
  plugin_type->is_instrument            = false;
  plugin_type->note_handling_is_RT      = false;
  plugin_type->num_effects              = CC_START + 128;
  plugin_type->get_effect_format        = get_effect_format;
  plugin_type->get_effect_name          = get_effect_name;
  plugin_type->effect_is_RT             = NULL;
  plugin_type->create_plugin_data       = create_plugin_data;
  plugin_type->cleanup_plugin_data      = cleanup_plugin_data;
  
  plugin_type->RT_process       = RT_process;
  plugin_type->play_note        = NULL;
  plugin_type->set_note_volume  = NULL;
  plugin_type->stop_note        = NULL;
  plugin_type->set_effect_value = RT_set_effect_value;
  plugin_type->get_effect_value = RT_get_effect_value;
  plugin_type->get_display_value_string = get_display_value_string;

  PR_add_plugin_type(plugin_type);
}
