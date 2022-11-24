

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

  // booleans:
  DAMPER_ONOFF,
  PORTAMENTO_ONOFF,
  SOSTENUTO_ONOFF,
  SOFT_PEDAL_ONOFF,
  LEGATO_FOOTSWITCH,
  HOLD2_ONOFF,

  SOUND_VARIATION,
  TIMBRE,
  RELEASE_,
  ATTACK,
  BRIGHTNESS,
  DECAY,
  VIBRATO_RATE,
  VIBRATO_DEPTH,
  VIBRATO_DELAY,

  SOUND_CONTROLLER10,

  GENERAL_PURPOSE_CONTROLLER_1,
  GENERAL_PURPOSE_CONTROLLER_2,
  GENERAL_PURPOSE_CONTROLLER_3,
  GENERAL_PURPOSE_CONTROLLER_4,

  PORTAMENTO,
  REVERB,
  TREMOLO,
  CHORUS,
  CELESTE,
  PHASER,

  CC3,CC6,CC9,
  
  CC14,CC15,CC16,CC17,CC18,CC19,CC20,CC21,CC22,CC23,CC24,CC25,CC26,CC27,CC28,CC29,CC30,CC31,CC32,CC33,CC34,CC35,CC36,CC37,CC38,CC39,CC40,CC41,CC42,CC43,CC44,CC45,CC46,CC47,CC48,CC49,CC50,CC51,CC52,CC53,CC54,CC55,CC56,CC57,CC58,CC59,CC60,CC61,CC62,CC63,
  
  CC85,CC86,CC87,CC88,CC89,CC90,
  
  CC96,CC97,CC98,CC99,CC100,CC101,CC102,CC103,CC104,CC105,CC106,CC107,CC108,CC109,CC110,CC111,CC112,CC113,CC114,CC115,CC116,CC117,CC118,CC119,

  CC120,CC121,CC122,CC123,CC124,CC125,CC126,CC127,
  
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
  if (!RT_do_send_MIDI_to_receivers(plugin))
    return;
    
  Data *data = (Data*)plugin->data;

  note.midi_channel = (char)data->values[CHANNEL];
  
  RT_PATCH_send_play_note_to_receivers(g_RT_curr_scheduling_seqtrack, (struct Patch*)plugin->patch, note, g_last_seq_time_converted_to_delta_time);
}

static void set_note_volume(struct SoundPlugin *plugin, int time, note_t note){
  if (!RT_do_send_MIDI_to_receivers(plugin))
    return;
      
  Data *data = (Data*)plugin->data;

  note.midi_channel = (char)data->values[CHANNEL];
  
  RT_PATCH_send_change_velocity_to_receivers(g_RT_curr_scheduling_seqtrack, (struct Patch*)plugin->patch, note, g_last_seq_time_converted_to_delta_time);
}

static void stop_note(struct SoundPlugin *plugin, int time, note_t note){
  if (!RT_do_send_MIDI_to_receivers(plugin))
    return;
      
  Data *data = (Data*)plugin->data;

  note.midi_channel = (char)data->values[CHANNEL];
  
  RT_PATCH_send_stop_note_to_receivers(g_RT_curr_scheduling_seqtrack, (struct Patch*)plugin->patch, note, g_last_seq_time_converted_to_delta_time);
}


static void send_msg(struct SoundPlugin *plugin, int64_t block_delta_time, unsigned int byte1, unsigned int byte2, int byte3){
  if (!RT_do_send_MIDI_to_receivers(plugin))
    return;
      
  Data *data = (Data*)plugin->data;
  volatile struct Patch *patch = plugin->patch;

  if (patch==NULL) // happens during initialization
    return;

  byte1 |= data->values[CHANNEL];

  //printf("Sending %0x %d %d\n", byte1, byte2, byte3);

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
    CASE(BALANCE,8);
    CASE(PAN,0xa);
    CASE(EXPRESSION_CONTROLLER,0xb);
    CASE(EFFECT_CONTROLLER_1,0xc);
    CASE(EFFECT_CONTROLLER_2,0xd);

    CASE(SOUND_VARIATION,70);
    CASE(TIMBRE,71);
    CASE(RELEASE_,72);
    CASE(ATTACK,73);
    CASE(BRIGHTNESS,74);
    CASE(DECAY,75);
    CASE(VIBRATO_RATE,76);
    CASE(VIBRATO_DEPTH,77);
    CASE(VIBRATO_DELAY,78);
    CASE(SOUND_CONTROLLER10,79);

    CASE(GENERAL_PURPOSE_CONTROLLER_1,80);
    CASE(GENERAL_PURPOSE_CONTROLLER_2,81);
    CASE(GENERAL_PURPOSE_CONTROLLER_3,82);
    CASE(GENERAL_PURPOSE_CONTROLLER_4,83);

    CASE(PORTAMENTO, 84);    
    CASE(REVERB,91);
    CASE(TREMOLO,92);
    CASE(CHORUS,93);
    CASE(CELESTE,94);
    CASE(PHASER,95);

#define C(N) CASE(CC##N, N);
    
    C(3); C(6); C(9);
  
    C(14); C(15); C(16); C(17); C(18); C(19); C(20); C(21); C(22); C(23); C(24); C(25); C(26); C(27); C(28); C(29); C(30); C(31); C(32); C(33); C(34); C(35); C(36); C(37); C(38); C(39); C(40); C(41); C(42); C(43); C(44); C(45); C(46); C(47); C(48); C(49); C(50); C(51); C(52); C(53); C(54); C(55); C(56); C(57); C(58); C(59); C(60); C(61); C(62); C(63);
    
    C(85); C(86); C(87); C(88); C(89); C(90);
    
    C(96); C(97); C(98); C(99); C(100); C(101); C(102); C(103); C(104); C(105); C(106); C(107); C(108); C(109); C(110); C(111); C(112); C(113); C(114); C(115); C(116); C(117); C(118); C(119);
    
    C(120); C(121); C(122); C(123); C(124); C(125); C(126); C(127);

#undef C
    
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

static float RT_get_effect_value(struct SoundPlugin *plugin, int effect_num, enum ValueFormat value_format){
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

static const char *get_effect_name(const struct SoundPlugin *plugin, int effect_num){
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

    c(DAMPER_ONOFF,"Damper (cc=64)");
    c(PORTAMENTO_ONOFF,"Portamento (cc=65)");
    c(SOSTENUTO_ONOFF,"Sostenuto (cc=66)");
    c(SOFT_PEDAL_ONOFF,"Soft Pedal (cc=67)");
    c(LEGATO_FOOTSWITCH,"Legato Footswitch (cc=68)");
    c(HOLD2_ONOFF,"Hold2 (cc=69)");

    c(SOUND_VARIATION,"Sound Variation (cc=70))");    
    c(TIMBRE,"Timbre (cc=71)");    
    c(RELEASE_,"Release (cc=72)");    
    c(ATTACK,"Attack (cc=73)");    
    c(BRIGHTNESS,"Brightness (cc=74)");    
    c(DECAY,"Decay (cc=75)");    
    c(VIBRATO_RATE,"Vibrato Rate (cc=76)");    
    c(VIBRATO_DEPTH,"Vibrato Depth (cc=77)");    
    c(VIBRATO_DELAY,"Vibrato Delay (cc=78)");    
    c(SOUND_CONTROLLER10,"Controller 10 (cc=79)");
    
    c(GENERAL_PURPOSE_CONTROLLER_1,"General 1 (cc=80)");
    c(GENERAL_PURPOSE_CONTROLLER_2,"General 2 (cc=81)");
    c(GENERAL_PURPOSE_CONTROLLER_3,"General 3 (cc=82)");
    c(GENERAL_PURPOSE_CONTROLLER_4,"General 4 (cc=83)");

    c(PORTAMENTO, "Portamento (cc=84)");
    c(REVERB,"Reverb (cc=91)");
    c(TREMOLO,"Tremolo (cc=92)");
    c(CHORUS,"Chorus (cc=93)");
    c(CELESTE,"Celeste (cc=94)");
    c(PHASER,"Phaser (cc=95)");
    
    c(CC3,"cc 3");
    c(CC6,"cc 6");
    c(CC9,"cc 9");
    
    c(CC14,"cc 14");
    c(CC15,"cc 15");
    c(CC16,"cc 16");
    c(CC17,"cc 17");
    c(CC18,"cc 18");
    c(CC19,"cc 19");
    c(CC20,"cc 20");
    c(CC21,"cc 21");
    c(CC22,"cc 22");
    c(CC23,"cc 23");
    c(CC24,"cc 24");
    c(CC25,"cc 25");
    c(CC26,"cc 26");
    c(CC27,"cc 27");
    c(CC28,"cc 28");
    c(CC29,"cc 29");
    c(CC30,"cc 30");
    c(CC31,"cc 31");
    c(CC32,"cc 32");
    c(CC33,"cc 33");
    c(CC34,"cc 34");
    c(CC35,"cc 35");
    c(CC36,"cc 36");
    c(CC37,"cc 37");
    c(CC38,"cc 38");
    c(CC39,"cc 39");
    c(CC40,"cc 40");
    c(CC41,"cc 41");
    c(CC42,"cc 42");
    c(CC43,"cc 43");
    c(CC44,"cc 44");
    c(CC45,"cc 45");
    c(CC46,"cc 46");
    c(CC47,"cc 47");
    c(CC48,"cc 48");
    c(CC49,"cc 49");
    c(CC50,"cc 50");
    c(CC51,"cc 51");
    c(CC52,"cc 52");
    c(CC53,"cc 53");
    c(CC54,"cc 54");
    c(CC55,"cc 55");
    c(CC56,"cc 56");
    c(CC57,"cc 57");
    c(CC58,"cc 58");
    c(CC59,"cc 59");
    c(CC60,"cc 60");
    c(CC61,"cc 61");
    c(CC62,"cc 62");
    c(CC63,"cc 63");

    c(CC85,"cc 85");
    c(CC86,"cc 86");
    c(CC87,"cc 87");
    c(CC88,"cc 88");
    c(CC89,"cc 89");
    c(CC90,"cc 90");
    
    c(CC96,"cc 96");
    c(CC97,"cc 97");
    c(CC98,"cc 98");
    c(CC99,"cc 99");
    c(CC100,"cc 100");
    c(CC101,"cc 101");
    c(CC102,"cc 102");
    c(CC103,"cc 103");
    c(CC104,"cc 104");
    c(CC105,"cc 105");
    c(CC106,"cc 106");
    c(CC107,"cc 107");
    c(CC108,"cc 108");
    c(CC109,"cc 109");
    c(CC110,"cc 110");
    c(CC111,"cc 111");
    c(CC112,"cc 112");
    c(CC113,"cc 113");
    c(CC114,"cc 114");
    c(CC115,"cc 115");
    c(CC116,"cc 116");
    c(CC117,"cc 117");
    c(CC118,"cc 118");
    c(CC119,"cc 119");
    
    c(CC120,"cc 120");
    c(CC121,"cc 121");
    c(CC122,"cc 122");
    c(CC123,"cc 123");
    c(CC124,"cc 124");
    c(CC125,"cc 125");
    c(CC126,"cc 126");
    c(CC127,"cc 127");
    
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
    snprintf(buffer,buffersize-1," ");//Not used");
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
