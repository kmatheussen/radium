

#include "../common/nsmtracker.h"
#include "SoundPlugin.h"
#include "SoundPlugin_proc.h"
#include "../Qt/Qt_instruments_proc.h"
#include "../midi/midi_proc.h"
#include "../common/player_proc.h"
#include "../common/OS_Player_proc.h"
#include "../common/patch_proc.h"
#include "../common/scheduler_proc.h"

#include "SoundPluginRegistry_proc.h"

enum{
  CHANNEL = 0,
  
  PROGRAM_CHANGE, // must be the first non-channel effect
  CHANNEL_AFTERTOUCH,
  PITCH_BEND,

  MSB,
  LSB,
  
  MODULATION_WHEEL,
  BREATH_CONTROLLER,
  FOOT_CONTROLLER,
  PORTAMENTO_TIME,
  CHANNEL_VOLUME,
  BALANCE,
  PAN,
  EXPRESSION_CONTROLLER,
  EFFECT_CONTROLLER_1,
  EFFECT_CONTROLLER_2,
  GENERAL_PURPOSE_CONTROLLER_1,
  GENERAL_PURPOSE_CONTROLLER_2,
  GENERAL_PURPOSE_CONTROLLER_3,
  GENERAL_PURPOSE_CONTROLLER_4,

  REVERB,
  TREMOLO,
  CHORUS,
  CELESTE,
  PHASER,

  // booleans:
  DAMPER_ONOFF,
  PORTAMENTO_ONOFF,
  SOSTENUTO_ONOFF,
  SOFT_PEDAL_ONOFF,
  LEGATO_FOOTSWITCH,
  HOLD2_ONOFF,

  NUM_EFFECTS
};


typedef struct{

  int values[NUM_EFFECTS];

  bool has_sent_initial_values;
  
  /*  
  int channel;
  int program_change;
  int channel_aftertouch;
  int pitch_bend;

  int MSB;
  int LSB;
  
  int modulation_wheel;
  int breath_controller;
  int foot_controller;
  int portamento_time;
  int channel_volume;
  int balance;
  int pan;
  int expression_controller;
  int effect_controller_1;
  int effect_controller_2;
  int general_purpose_controller_1;
  int general_purpose_controller_2;
  int general_purpose_controller_3;
  int general_purpose_controller_4;

  int reverb;
  int tremolo;
  int chorus;
  int celeste;
  int phaser;

  // booleans:
  int damper_onoff;
  int portamento_onoff;
  int sostenuto_onoff;
  int soft_pedal_onoff;
  int legato_footswitch;
  int hold2_onoff;
  */
    //int cc[128];
} Data;



static void RT_set_effect_value(struct SoundPlugin *plugin, int block_delta_time, int effect_num, float floatvalue, enum ValueFormat value_format, FX_when when);

static void RT_process(SoundPlugin *plugin, int64_t time, int num_frames, float **inputs, float **outputs){
  Data *data = (Data*)plugin->data;
  
  if (data->has_sent_initial_values==false) {
    
    data->has_sent_initial_values=true;

    RT_PLAYER_runner_lock();{
      int effect_num;
      for(effect_num = PROGRAM_CHANGE ; effect_num < NUM_EFFECTS ; effect_num++) {
        int value = data->values[effect_num];
        if (value != -1)
          RT_set_effect_value(plugin, 0, effect_num, value, EFFECT_FORMAT_NATIVE, FX_single);
      }
    }RT_PLAYER_runner_unlock();
    
  }
}

static void play_note(struct SoundPlugin *plugin, int time, note_t note){
  Data *data = (Data*)plugin->data;

  note.midi_channel = data->values[CHANNEL];
  
  RT_PATCH_send_play_note_to_receivers(g_RT_curr_scheduling_seqtrack, (struct Patch*)plugin->patch, note, g_last_seq_time_converted_to_delta_time);
}

static void set_note_volume(struct SoundPlugin *plugin, int time, note_t note){
  Data *data = (Data*)plugin->data;

  note.midi_channel = data->values[CHANNEL];
  
  RT_PATCH_send_change_velocity_to_receivers(g_RT_curr_scheduling_seqtrack, (struct Patch*)plugin->patch, note, g_last_seq_time_converted_to_delta_time);
}

static void stop_note(struct SoundPlugin *plugin, int time, note_t note){
  Data *data = (Data*)plugin->data;

  note.midi_channel = data->values[CHANNEL];
  
  RT_PATCH_send_stop_note_to_receivers(g_RT_curr_scheduling_seqtrack, (struct Patch*)plugin->patch, note, g_last_seq_time_converted_to_delta_time);
}


static void send_msg(struct SoundPlugin *plugin, int64_t block_delta_time, unsigned int byte1, unsigned int byte2, int byte3){
  Data *data = (Data*)plugin->data;
  volatile struct Patch *patch = plugin->patch;

  if (patch==NULL) // happens during initialization
    return;
  
  byte1 |= data->values[CHANNEL];

  uint32_t msg;

  if (byte3==-1)
    msg = MIDI_msg_pack2(byte1, byte2);
  else
    msg = MIDI_msg_pack3(byte1, byte2, byte3);

  struct SeqTrack *seqtrack = g_RT_curr_scheduling_seqtrack;
  int64_t delta_time = PLAYER_get_block_delta_time(seqtrack, seqtrack->start_time + block_delta_time);
  int64_t radium_time = seqtrack->start_time + delta_time;

  RT_PATCH_send_raw_midi_message_to_receivers(seqtrack, (struct Patch*)patch, msg, radium_time);
}

static void get_minval_and_maxval(int effect_num, int *minval, int *maxval){
  *minval = -1;
  
  switch(effect_num){
  case CHANNEL:
    *minval = 0;
    *maxval = 15;
    break;
  case PITCH_BEND:
    *minval = -8193;
    *maxval = 8191;
    break;
  case DAMPER_ONOFF:
  case PORTAMENTO_ONOFF:
  case SOSTENUTO_ONOFF:
  case SOFT_PEDAL_ONOFF:
  case LEGATO_FOOTSWITCH:
  case HOLD2_ONOFF:
    *maxval = 1;
    break;
  default:
    *maxval = 127;
    break;
  }
}

static void RT_set_effect_value(struct SoundPlugin *plugin, int block_delta_time, int effect_num, float floatvalue, enum ValueFormat value_format, FX_when when){
  Data *data = (Data*)plugin->data;

  int minval;
  int maxval;
  
  get_minval_and_maxval(effect_num, &minval, &maxval);
  
  int value;
  
  if(value_format==EFFECT_FORMAT_SCALED)
    value = scale(floatvalue, 0, 1, minval-1, maxval+1);
  else
    value = floatvalue;

  value = R_BOUNDARIES(minval, value, maxval);

  data->values[effect_num] = value;

  //printf("minval: %d, maxval: %d, floatvalue: %f, value: %d (%d)\n",minval,maxval,floatvalue,value,(int)scale(floatvalue, 0, 1, minval, maxval));
  
  switch(effect_num){
  case CHANNEL:
    break;
  case PROGRAM_CHANGE:
    if (value!=-1)
      send_msg(plugin, block_delta_time, 0xc0, value, -1);
    break;
  case CHANNEL_AFTERTOUCH:
    if (value!=-1)
      send_msg(plugin, block_delta_time, 0xd0, value, -1);
    break;
  case PITCH_BEND:
    {
      if (value!=-8193) {
        uint32_t val = value;
        val += 0x2000;
        send_msg(plugin, block_delta_time, 0xe0, val&127, val>>7);
      }
      break;
      }
#define CASE(a,b)                                                       \
    case a:                                                             \
      if (value!=-1) send_msg(plugin, block_delta_time, 0xb0, b, value); \
      break;
    
    CASE(MSB,0);
    CASE(LSB,0x20);
    CASE(MODULATION_WHEEL,1);
    CASE(BREATH_CONTROLLER, 2);
    CASE(FOOT_CONTROLLER,4);
    CASE(PORTAMENTO_TIME,5);
    CASE(CHANNEL_VOLUME,7);
    CASE(BALANCE,9);
    CASE(PAN,0xa);
    CASE(EXPRESSION_CONTROLLER,0xb);
    CASE(EFFECT_CONTROLLER_1,0xc);
    CASE(EFFECT_CONTROLLER_2,0xd);
    CASE(GENERAL_PURPOSE_CONTROLLER_1,80);
    CASE(GENERAL_PURPOSE_CONTROLLER_2,81);
    CASE(GENERAL_PURPOSE_CONTROLLER_3,82);
    CASE(GENERAL_PURPOSE_CONTROLLER_4,83);

    CASE(REVERB,0x5b);
    CASE(TREMOLO,0x5c);
    CASE(CHORUS,0x5d);
    CASE(CELESTE,0x5e);
    CASE(PHASER,0x5f);
#undef CASE

#define CASE(a, b)                            \
    case a:                                      \
      if (value!=-1) send_msg(plugin, block_delta_time, 0xb0, b, value*64); \
      break;
    
    // booleans:
    CASE(DAMPER_ONOFF,0x40);
    CASE(PORTAMENTO_ONOFF,0x41);
    CASE(SOSTENUTO_ONOFF,0x42);
    CASE(SOFT_PEDAL_ONOFF,0x43);
    CASE(LEGATO_FOOTSWITCH,0x44);
    CASE(HOLD2_ONOFF,0x45);
#undef CASE

  default:
    RError("Unknown effect_num %d for Midi Message",effect_num);
  }    
}

float RT_get_effect_value(struct SoundPlugin *plugin, int effect_num, enum ValueFormat value_format){
  Data *data = (Data*)plugin->data;

  int value = data->values[effect_num];
  
  if(value_format==EFFECT_FORMAT_SCALED) {
    
    int minval;
    int maxval;
    get_minval_and_maxval(effect_num, &minval, &maxval);
    return scale(value, minval, maxval, 0, 1);
    
  } else {

    return value;

  }
}

static int get_effect_format(struct SoundPlugin *plugin, int effect_num){
  return EFFECT_FORMAT_INT;
}

static const char *get_effect_name(struct SoundPlugin *plugin, int effect_num){
  switch(effect_num){

#define c(a,b) case a: return b

    c(CHANNEL,"Channel");
    c(PROGRAM_CHANGE,"Program Change");
    c(CHANNEL_AFTERTOUCH,"Channel Aftertouch");
    c(PITCH_BEND,"Pitch Bend");

    c(MSB,"MSB (cc=0)");
    c(LSB,"LSB (cc=32)");
    c(MODULATION_WHEEL,"Modulation Wheel (cc=1)");
    c(BREATH_CONTROLLER,"Breath Controller (cc=2)");
    c(FOOT_CONTROLLER,"Foot Controller (cc=4)");
    c(PORTAMENTO_TIME,"Portamento Time (cc=5)");
    c(CHANNEL_VOLUME,"Channel Volume (cc=7)");
    c(BALANCE,"Balance (cc=8)");
    c(PAN,"Pan (cc=10)");
    c(EXPRESSION_CONTROLLER,"Expression (cc=11)");
    c(EFFECT_CONTROLLER_1,"Effect 1 (cc=12)");
    c(EFFECT_CONTROLLER_2,"Effect 2 (cc=13");
    c(GENERAL_PURPOSE_CONTROLLER_1,"General 1 (cc=80)");
    c(GENERAL_PURPOSE_CONTROLLER_2,"General 2 (cc=81)");
    c(GENERAL_PURPOSE_CONTROLLER_3,"General 3 (cc=82)");
    c(GENERAL_PURPOSE_CONTROLLER_4,"General 4 (cc=83)");

    c(REVERB,"Reverb (cc=91)");
    c(TREMOLO,"Tremolo (cc=92)");
    c(CHORUS,"Chorus (cc=93)");
    c(CELESTE,"Celeste (cc=94)");
    c(PHASER,"Phaser (cc=95)");
    
    c(DAMPER_ONOFF,"Damper (cc=64)");
    c(PORTAMENTO_ONOFF,"Portamento (cc=65)");
    c(SOSTENUTO_ONOFF,"Sostenuto (cc=66)");
    c(SOFT_PEDAL_ONOFF,"Soft Pedal (cc=67)");
    c(LEGATO_FOOTSWITCH,"Legato Footswitch (cc=68)");
    c(HOLD2_ONOFF,"Hold2 (cc=69)");
#undef c

  default:
    RError("Unknown effect_num %d for Midi Message",effect_num);
    return "error";
  }    
}

static void get_display_value_string(SoundPlugin *plugin, int effect_num, char *buffer, int buffersize){
  Data *data = (Data*)plugin->data;

  int value = data->values[effect_num];

  int minval;
  int maxval;
  get_minval_and_maxval(effect_num, &minval, &maxval);

  if (effect_num>0 && value==minval) {
    snprintf(buffer,buffersize-1,"Not used");
    return;
  }
  
  switch(effect_num){
    case CHANNEL:
      snprintf(buffer,buffersize-1,"%d",value+1);
      break;
    case PITCH_BEND:
      snprintf(buffer,buffersize-1,"%d",(int)RT_get_effect_value(plugin, effect_num, EFFECT_FORMAT_NATIVE));
      break;
    case DAMPER_ONOFF:
    case PORTAMENTO_ONOFF:
    case SOSTENUTO_ONOFF:
    case SOFT_PEDAL_ONOFF:
    case HOLD2_ONOFF:
      if (value==0)
        snprintf(buffer,buffersize-1,"Off");
      else
        snprintf(buffer,buffersize-1,"On");
      break;
    case LEGATO_FOOTSWITCH:
      if (value==0)
        snprintf(buffer,buffersize-1,"Normal");
      else
        snprintf(buffer,buffersize-1,"Legato");
      break;
    default:   
      snprintf(buffer,buffersize-1,"%d",(int)RT_get_effect_value(plugin, effect_num, EFFECT_FORMAT_NATIVE));
      break;
  }
}


static void *create_plugin_data(const SoundPluginType *plugin_type, SoundPlugin *plugin, hash_t *state, float sample_rate, int block_size, bool is_loading){
  Data *data = (Data*)V_calloc(1,sizeof(Data));
  int i;
  for(i=1;i<NUM_EFFECTS;i++){
    data->values[i] = -1;
  }
  data->values[PITCH_BEND]=-8193;
  return data;
}

static void cleanup_plugin_data(SoundPlugin *plugin){
  V_free(plugin->data);
}


void create_midimessages_plugin(void){
  SoundPluginType *plugin_type = (SoundPluginType*)V_calloc(1,sizeof(SoundPluginType));

  plugin_type->type_name                = "MIDI Messages";
  plugin_type->name                     = "MIDI Messages";
  plugin_type->info                     = "The MIDI Messages object makes it possible to send MIDI messages to other sound objects.\nThis can for instance be used to send pitch bend changes to VST plugins.";
  plugin_type->num_inputs               = 0;
  plugin_type->num_outputs              = 0;
  plugin_type->is_instrument            = false;
  plugin_type->note_handling_is_RT      = false;
  plugin_type->num_effects              = NUM_EFFECTS;
  plugin_type->get_effect_format        = get_effect_format;
  plugin_type->get_effect_name          = get_effect_name;
  plugin_type->effect_is_RT             = NULL;
  plugin_type->create_plugin_data       = create_plugin_data;
  plugin_type->cleanup_plugin_data      = cleanup_plugin_data;
  
  plugin_type->RT_process      = RT_process;
  plugin_type->play_note       = play_note;
  plugin_type->set_note_volume = set_note_volume;
  plugin_type->stop_note       = stop_note;
  
  plugin_type->set_effect_value = RT_set_effect_value;
  plugin_type->get_effect_value = RT_get_effect_value;
  plugin_type->get_display_value_string = get_display_value_string;

  PR_add_plugin_type(plugin_type);
}
