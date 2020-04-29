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

#include "../bin/packages/fluidsynth-1.1.6/include/fluidsynth.h"

#include "../common/nsmtracker.h"
#include "../common/visual_proc.h"
#include "../common/disk.h"
#include "../common/OS_Player_proc.h"
#include "../common/instruments_proc.h"
#include "../common/OS_settings_proc.h"
#include "../midi/midi_proc.h"
#include "../crashreporter/crashreporter_proc.h"

#include "audio_instrument_proc.h"

#include "SoundPlugin.h"
#include "SoundPlugin_proc.h"
#include "SoundProducer_proc.h"
#include "Mixer_proc.h"
#include "Fade.hpp"

#include "SoundPluginRegistry_proc.h"

#include "../Qt/Qt_instruments_proc.h"

#include "../api/api_proc.h"

#include "FluidSynth_plugin_proc.h"


// Fluidsynth has an interface to schedule notes in advance. Unfortunately, the max resolution of the timing is only 1000 hz.


// Effect order
enum{
  EFF_PITCH,
  EFF_PITCH_RANGE,
  EFF_MODULATION,
  EFF_SUSTAIN_PEDAL,

  //EFF_CHORUS,
  //EFF_REVERB,
  EFF_NUM_EFFECTS
  };


namespace{
  
struct Data{
  fluid_settings_t* settings = NULL;
  fluid_synth_t* synth = NULL;
  //fluid_audio_driver_t* driver; Hopefully there's nothing wrong using fluidsynth without creating a driver.
  fluid_sequencer_t* sequencer = NULL;
  short synth_seq_ID = 0;
  fluid_event_t *event = NULL;
    
  int soundfont_id = 0;

  int64_t time = 0;

  double samplerate = 0;
  double time_scale = 0;

  radium::FilePath filename;
  int bank_num = 0;
  int preset_num = 0;
  bool using_default_sound = false;
  
  // These two are used when switching sound in realtime
  DEFINE_ATOMIC(Data *, new_data) = NULL;
  RSemaphore *signal_from_RT = NULL;

  float pitch = 0;
  float pitch_range = 0;
  float modulation = 0;
  float sustain_on = 0;

};

}



static int get_fluidsynth_time(Data *data, int64_t time){
  return (int)(time * data->time_scale / data->samplerate);
}

static void RT_process(SoundPlugin *plugin, int64_t time, int num_frames, float **inputs, float **outputs){
  Data *data = (Data*)plugin->data;
  //data =NULL; // crashreporter test.
  //R_ASSERT(false);
  //CRASHREPORTER_send_assert_message("AIAI1!");
  //RWarning("oops");

  //printf("telling sequencer that it is time %d\n",(int)data->time);
  fluid_sequencer_process(data->sequencer, get_fluidsynth_time(data,data->time));
  data->time += num_frames;

  // What's the difference between fluid_synth_write_float and fluid_synth_nwrite_float? Sounds to me like they produce exactly the same sound.
#if 0
  if(fluid_synth_write_float(data->synth,num_frames,outputs[0],0,1,outputs[1],0,1)==FLUID_FAILED)
    printf("fluid_synth_write_float failed\n");
#else
  if(fluid_synth_nwrite_float(data->synth,num_frames, &outputs[0], &outputs[1], NULL, NULL)==FLUID_FAILED)
    printf("fluid_synth_write_float failed\n");
#endif

  Data *new_data = ATOMIC_GET(data->new_data);
  
  if(new_data != NULL){
    RT_fade_out(outputs[0],num_frames);
    RT_fade_out(outputs[1],num_frames);

    plugin->data = new_data; // hmm.    
    ATOMIC_SET(data->new_data, NULL);

    RSEMAPHORE_signal(data->signal_from_RT,1);
  }
}

static void sendnoteon(Data *data, int chan, short key, int velocity, unsigned int time) 
{
  int fluid_res;

  if(velocity>127) // not supported by fluidsynth
    velocity=127;

  fluid_event_clear(data->event);
  fluid_event_set_source(data->event, -1);
  fluid_event_set_dest(data->event, data->synth_seq_ID);
  fluid_event_noteon(data->event, chan, key, velocity);
  fluid_res = fluid_sequencer_send_at(data->sequencer, data->event, get_fluidsynth_time(data,time), 1);
  
  if(fluid_res==FLUID_FAILED)
    printf("Unable to send noteON %d (vel: %d) at time %d. Time now: %d\n",key,velocity,time,(int)data->time);
}


static void sendnoteoff(Data *data, int chan, short key, int velocity, unsigned int time) 
{
  int fluid_res;

  if(velocity>127) // not supported by fluidsynth
    velocity=127;

  fluid_event_clear(data->event);
  fluid_event_set_source(data->event, -1);
  fluid_event_set_dest(data->event, data->synth_seq_ID);
  fluid_event_noteoff(data->event, chan, key);
  fluid_res = fluid_sequencer_send_at(data->sequencer, data->event, get_fluidsynth_time(data,time), 1);
  
  if(fluid_res==FLUID_FAILED)
    printf("Unable to send noteOFF %d (vel: %d) at time %d. Time now: %d\n",key,velocity,time,(int)data->time);
}

static void sendcontrolchange(Data *data, int chan, int cc, int val, int time)
{
  int fluid_res;

  fluid_event_clear(data->event);
  fluid_event_set_source(data->event, -1);
  fluid_event_set_dest(data->event, data->synth_seq_ID);
  fluid_event_control_change(data->event, chan, cc, val);
  fluid_res = fluid_sequencer_send_at(data->sequencer, data->event, get_fluidsynth_time(data,time), 1);
  
  if(fluid_res==FLUID_FAILED)
    printf("Unable to send controlchange\n");
}

static void sendpitchbend(Data *data, int chan, int pitch, int time)
{
  int fluid_res;

  fluid_event_clear(data->event);
  fluid_event_set_source(data->event, -1);
  fluid_event_set_dest(data->event, data->synth_seq_ID);
  fluid_event_pitch_bend(data->event, chan, pitch);
  fluid_res = fluid_sequencer_send_at(data->sequencer, data->event, get_fluidsynth_time(data,time), 1);
  
  if(fluid_res==FLUID_FAILED)
    printf("Unable to send pitchbend\n");
}

static void play_note(struct SoundPlugin *plugin, int time, note_t note){
  Data *data = (Data*)plugin->data;

  //fluid_synth_noteon(data->synth, 0, note_num, volume*127);
  sendnoteon(data, 0, (int)note.pitch, note.velocity*127, (unsigned int)(data->time + time));
  //printf("Sending out note at time %d. Time now: %d. Delta: %d\n",(int)get_fluidsynth_time(data,data->time+time),(int)get_fluidsynth_time(data,data->time),(int)get_fluidsynth_time(data,time));
}

static void set_note_volume(struct SoundPlugin *plugin, int time, note_t note){
  // fluidsynth doesn't seem to support polyphonic aftertouch.
#if 0
  Data *data = (Data*)plugin->data;
  fluid_synth_key_pressure(data->synth,0,(int)note.pitch,scale(note.velocity,1,0,0,1)*127); // Nah.
#endif
}

static void stop_note(struct SoundPlugin *plugin, int time, note_t note){
  Data *data = (Data*)plugin->data;
  //fluid_synth_noteoff(data->synth, 0, note_num);
  sendnoteoff(data, 0, note.pitch, 0, (unsigned int)(data->time + time));
}

static void send_raw_midi_message(struct SoundPlugin *plugin, int block_delta_time, uint32_t msg){
  Data *data = (Data*)plugin->data;
  
  int cc = MIDI_msg_byte1(msg);
  int data1 = MIDI_msg_byte2(msg);
  int data2 = MIDI_msg_byte3(msg);

  if (cc>=0xe0 && cc<0xf0) {
    int pitch = (data2<<7) + data1;
    sendpitchbend(data, 0, pitch, (unsigned int)(data->time + block_delta_time));

  } else if (cc >= 0xb0 && cc <0xc0)
    sendcontrolchange(data,0,data1,data2, (unsigned int)(data->time + block_delta_time));
}

static void set_effect_value(struct SoundPlugin *plugin, int time, int effect_num, float value, enum ValueFormat value_format, FX_when when){
  Data *data = (Data*)plugin->data;

  if(value_format==EFFECT_FORMAT_SCALED){
    switch(effect_num){
    case EFF_PITCH:
      data->pitch = scale(value,0.0f,1.0f,0,16383);
      //printf("Setting Pitch to %d\n",(int)data->pitch);
      sendpitchbend(data,0,data->pitch,time);
      break;
    case EFF_PITCH_RANGE:
      data->pitch_range = scale(value,0.0f,1.0f,1,48);
      fluid_synth_pitch_wheel_sens(data->synth,0,data->pitch_range);
      break;
    case EFF_MODULATION:
      data->modulation = value * 127;
      sendcontrolchange(data,0,1,data->modulation,time);
      break;
    case EFF_SUSTAIN_PEDAL:
      data->sustain_on = value*127;
      sendcontrolchange(data,0,64,data->sustain_on,time);
      break;
    default:
      RError("F1. Unknown effect number %d\n",effect_num);
    }
  }else{
    switch(effect_num){
    case EFF_PITCH:
      data->pitch = value;
      sendpitchbend(data,0,value,time);
      break;
    case EFF_PITCH_RANGE:
      data->pitch_range = value;
      fluid_synth_pitch_wheel_sens(data->synth,0,value);
      break;
    case EFF_MODULATION:
      data->modulation = value;
      sendcontrolchange(data,0,1,value,time);
      break;
    case EFF_SUSTAIN_PEDAL:
      data->sustain_on = value;
      sendcontrolchange(data,0,64,value,time);
      break;
    default:
      RError("F2. Unknown effect number %d\n",effect_num);
    }
  }
}

static float get_effect_value(struct SoundPlugin *plugin, int effect_num, enum ValueFormat value_format){
  Data *data = (Data*)plugin->data;
  //int val;

  if(value_format==EFFECT_FORMAT_SCALED){
    switch(effect_num){
    case EFF_PITCH:
      //fluid_synth_get_pitch_bend(data->synth,0,&val);
      return scale(data->pitch,0,16383,0.0f,1.0f);
    case EFF_PITCH_RANGE:
      //fluid_synth_get_pitch_wheel_sens(data->synth,0,&val);
      return scale(data->pitch_range,1,48,0.0f,1.0f);
    case EFF_MODULATION:
      //fluid_synth_get_cc(data->synth,0,1,&val);
      return data->modulation / 127.0;
    case EFF_SUSTAIN_PEDAL:
      //fluid_synth_get_cc(data->synth,0,64,&val);
      return data->sustain_on / 127.0;
    default:
      RError("F3. Unknown effect number %d\n",effect_num);
    }
  }else{
    switch(effect_num){
    case EFF_PITCH:
      //fluid_synth_get_pitch_bend(data->synth,0,&val);
      return data->pitch;
    case EFF_PITCH_RANGE:
      //fluid_synth_get_pitch_wheel_sens(data->synth,0,&val);
      return data->pitch_range;
    case EFF_MODULATION:
      //fluid_synth_get_cc(data->synth,0,1,&val);
      return data->modulation;
    case EFF_SUSTAIN_PEDAL:
      //fluid_synth_get_cc(data->synth,0,64,&val);
      return data->sustain_on;
    default:
      RError("F4. Unknown effect number %d\n",effect_num);
    }
  }

  return 0.0f;
}

static void get_display_value_string(SoundPlugin *plugin, int effect_num, char *buffer, int buffersize){
  Data *data = (Data*)plugin->data;
  //int val;

  switch(effect_num){
  case EFF_PITCH:
    //fluid_synth_get_pitch_wheel_sens(data->synth,0,&val);
    snprintf(buffer,buffersize-1,"%.2f",scale(data->pitch,0,16383,-data->pitch_range,data->pitch_range));
    break;
  case EFF_PITCH_RANGE:
    //fluid_synth_get_pitch_wheel_sens(data->synth,0,&val);
    snprintf(buffer,buffersize-1,"%d",(int)data->pitch_range);
    break;
  case EFF_MODULATION:
    //fluid_synth_get_cc(data->synth,0,1,&val);
    snprintf(buffer,buffersize-1,"%d",(int)data->modulation);
    break;
  case EFF_SUSTAIN_PEDAL:
    //fluid_synth_get_cc(data->synth,0,1,&val);
    snprintf(buffer,buffersize-1,"%d",(int)data->sustain_on);
    break;
  default:
    RError("F5. Unknown effect number %d\n",effect_num);
  }
}

static void delete_data(Data *data){
  if(data->event!=NULL)
    delete_fluid_event(data->event);

  if(data->sequencer!=NULL)
    delete_fluid_sequencer(data->sequencer);

  if(data->synth!=NULL)
    delete_fluid_synth(data->synth);

  if(data->settings!=NULL)
    delete_fluid_settings(data->settings);

  if(data->signal_from_RT!=NULL)
    RSEMAPHORE_delete(data->signal_from_RT);

  delete data;
}

static Data *create_data(filepath_t filename, float samplerate){
  Data *data = new Data;
  //CRASHREPORTER_send_assert_message("AIAI2!");
  //data = NULL;
  
  data->pitch = 16384/2;
  data->pitch_range = 1;
  data->modulation = 0;
  data->sustain_on = 0;

  data->signal_from_RT = RSEMAPHORE_create(0);

  data->settings = new_fluid_settings();
  if(data->settings==NULL){
    GFX_Message(NULL, "Unable to create fluidsynth settings");
    delete_data(data);
    return NULL;
  }

  if(fluid_settings_setnum(data->settings, "synth.sample-rate", samplerate)==0){
    GFX_Message(NULL, "Unable to set sample rate of fluidsynth to %f\n",samplerate);
    //delete_data(data);
    //return NULL;
  }

  if(fluid_settings_setint(data->settings, "synth.threadsafe-api", 0)==0){
    printf("Unable to set threadsafe-api to 0 (we don't need it)\n");
  }

  if(fluid_settings_setint(data->settings, "synth.chorus.active", 0)==0){
    printf("Unable to set synth.chorus.active to 0 (we don't use it)\n");
  }

  if(fluid_settings_setint(data->settings, "synth.reverb.active", 0)==0){
    printf("Unable to set synth.reverb.active to 0 (we don't use it)\n");
  }



  // TODO: Decide whether we need fluidsynth's reverb and chorus.
  //fluid_settings_setstr(settings, "synth.reverb.active", "yes");
  //fluid_settings_setstr(settings, "synth.chorus.active", "no");

  data->synth = new_fluid_synth(data->settings);
  if(data->synth==NULL){
    GFX_Message(NULL, "Unable to create fluidsynth synth");
    delete_data(data);
    return NULL;
  }

  data->sequencer = new_fluid_sequencer2(0);
  if(data->sequencer==NULL){
    GFX_Message(NULL, "Unable to create fluidsynth sequencer");
    delete_data(data);
    return NULL;
  }

  fluid_sequencer_set_time_scale(data->sequencer, samplerate); // it's a shame that time in fluidsynth is only 32 bit. Hopefully the values wrap properly around.......
  data->time_scale = fluid_sequencer_get_time_scale(data->sequencer);
  data->samplerate = samplerate;

#if 0 // This test always succeeds. Max time_scale in fluidsynth is 1000.0. :-(
  if( data->time_scale != sample_rate){
    RError("Could not set time scale to %f (set to %f instead)\n",(float)samplerate,(float)fluid_sequencer_get_time_scale(data->sequencer));
  }
#endif

  data->synth_seq_ID = fluid_sequencer_register_fluidsynth(data->sequencer, data->synth);
  data->event = new_fluid_event();
  if(data->event==NULL){
    GFX_Message(NULL, "Unable to create fluidsynth event");
    delete_data(data);
    return NULL;
  }

  filepath_t something = OS_loading_get_resolved_file_path(filename, false); // program_state_is_valid=false, might probably be.
  if (isIllegalFilepath(something)){
    delete_data(data);
    return NULL;
  }
  
  data->filename = radium::FilePath(something);

  data->soundfont_id = fluid_synth_sfload(data->synth,STRING_get_chars(data->filename.getString()),true);

  if(data->soundfont_id==FLUID_FAILED){
    printf("Soundfont loading failed for \"%S\"\n",data->filename.getString());

    delete_data(data);

    return NULL;

  }

  return data;
}

static void *create_plugin_data(const SoundPluginType *plugin_type, struct SoundPlugin *plugin, hash_t *state, float samplerate, int block_size, bool is_loading){
  //Data *data = create_data("/home/kjetil/SGM-V2.01.sf2",samplerate);
  filepath_t default_sound_filename = OS_get_full_program_file_path(STRING_create("sounds/Orgue.sf2"));
  
  Data *data = create_data(default_sound_filename, samplerate);
  
  if(data!=NULL){
    fluid_synth_bank_select(data->synth,0,0);
    fluid_synth_program_change(data->synth,0,0);
  }

  data->using_default_sound = true;

  return data;
}

static void cleanup_plugin_data(SoundPlugin *plugin){
  Data *data = (Data*)plugin->data;
  delete_data(data);
}

bool FLUIDSYNTH_set_new_preset(SoundPlugin *plugin, filepath_t sf2_file, int bank_num, int preset_num){
  Data *data = (Data*)plugin->data;

  if(!STRING_equals2(sf2_file.id, data->filename.getString())){

    Data *new_data = create_data(sf2_file, data->samplerate);
    if(new_data==NULL)
      return false;

    if(SP_is_plugin_running(plugin)){

      ATOMIC_SET(data->new_data, new_data);

      if (PLAYER_is_running())
        RSEMAPHORE_wait(data->signal_from_RT,1);

    } else{

      plugin->data = new_data;

    }

    delete_data(data);
    data = new_data;
  }

  data->bank_num = bank_num;
  data->preset_num = preset_num;

  fluid_synth_bank_select(data->synth,0,bank_num);
  fluid_synth_program_change(data->synth,0,preset_num);

  volatile struct Patch *patch = plugin->patch;
  if(patch != NULL)
    GFX_update_instrument_widget((struct Patch*)patch);

  return true;
}

static void create_state(struct SoundPlugin *plugin, hash_t *state);

static void recreate_from_state(struct SoundPlugin *plugin, hash_t *state, bool is_loading){
  {
    hash_t *curr_state = HASH_create(10);
    create_state(plugin, curr_state);
    if (HASH_equal(state, curr_state))
      return;
  }
  
  int         bank_num    = HASH_get_int32(state, "bank_num");
  int         preset_num  = HASH_get_int32(state, "preset_num");

  filepath_t filename = PLUGIN_DISK_get_audio_filename(state);

  if(isIllegalFilepath(filename)) // not supposed to happen though. Assertion in PLUGIN_DISK_get_audio_filename.
    return;

  if(FLUIDSYNTH_set_new_preset(plugin, filename, bank_num, preset_num)==false)
    GFX_Message(NULL, "Could not load soundfont \"%S\", bank %d, preset %d",filename.id,bank_num,preset_num);

  // Can not delete now. file is still used when creating/recreating states. Deleting at program end.
  //if (audiodata_is_included)
  //  DISK_delete_base64_file(filename);
}

static void create_state(struct SoundPlugin *plugin, hash_t *state){
  Data *data=(Data*)plugin->data;

  filepath_t filepath = data->filename.get();
  
  if (g_is_saving && nsmIsActive()){
    filepath_t maybe = DISK_link_copy_file(DISK_get_absolute_dir_path(dc.filename_with_full_path), filepath, true);
    if (isLegalFilepath(maybe))
      filepath = maybe;
  }

  filepath_t maybe_relative_filename = OS_saving_get_relative_path_if_possible(filepath);
    
  HASH_put_filepath(state, "filename", maybe_relative_filename);
  HASH_put_int(state, "bank_num", data->bank_num);
  HASH_put_int(state, "preset_num", data->preset_num);

  if (g_embed_samples){
    const char *audiofile = DISK_file_to_base64(data->filename.get());
    if (audiofile != NULL)
      HASH_put_chars(state, "audiofile", audiofile);
    else
      GFX_addMessage("Unable to embed sample \"%S\". Could not read file.", maybe_relative_filename.id);
  }
}

filepath_t FLUIDSYNTH_get_filename(struct SoundPlugin *plugin, bool *is_default_sound){
  Data *data=(Data*)plugin->data;
  
  *is_default_sound = data->using_default_sound;
  return data->filename.get();
}

const wchar_t *FLUIDSYNTH_get_filename_display(struct SoundPlugin *plugin){
  Data *data=(Data*)plugin->data;
  
  char *s2 = talloc_format(", b: %d, p: %d", data->bank_num, data->preset_num);
  
  return STRING_append(data->filename.getString(),
                       STRING_create(s2));
}

static const char *get_effect_name(struct SoundPlugin *plugin_type, int effect_num){
  switch(effect_num){
  case EFF_PITCH:
    return "Pitch";
  case EFF_PITCH_RANGE:
    return "Pitch Range";
  case EFF_MODULATION:
    return "Modulation";
  case EFF_SUSTAIN_PEDAL:
    return "Sustain Pedal";
  default:
    RError("F6. Unknown effect number %d\n",effect_num);
    return "";
  }
}

static int get_effect_format(struct SoundPlugin *plugin, int effect_num){
  if(effect_num==EFF_SUSTAIN_PEDAL)
    return EFFECT_FORMAT_BOOL;
  else
    return EFFECT_FORMAT_FLOAT;
}

static SoundPluginType plugin_type = {};

void create_fluidsynth_plugin(void){
  static bool has_inited=false;
  
  if (has_inited==false){
    plugin_type.type_name                = "FluidSynth";
    plugin_type.name                     = "FluidSynth";
    plugin_type.info                     = "FluidSynth is a real-time software synthesizer based on the SoundFont 2 specifications.\n\nhttp://www.fluidsynth.org\n\n Note that the Sample Player also supports Soundfonts.";
    plugin_type.category                 = "Synth";
    plugin_type.num_inputs               = 0;
    plugin_type.num_outputs              = 2;
    plugin_type.is_instrument            = true;
    plugin_type.note_handling_is_RT      = false;
    plugin_type.num_effects              = EFF_NUM_EFFECTS;
    plugin_type.get_effect_format        = get_effect_format;
    plugin_type.get_effect_name          = get_effect_name;
    plugin_type.effect_is_RT             = NULL;
    plugin_type.create_plugin_data       = create_plugin_data;
    plugin_type.cleanup_plugin_data      = cleanup_plugin_data;
    
    plugin_type.RT_process       = RT_process;
    plugin_type.play_note        = play_note;
    plugin_type.set_note_volume  = set_note_volume;
    plugin_type.stop_note        = stop_note;
    plugin_type.send_raw_midi_message = send_raw_midi_message;
    
    plugin_type.set_effect_value = set_effect_value;
    plugin_type.get_effect_value = get_effect_value;
    plugin_type.get_display_value_string = get_display_value_string;
    
    plugin_type.recreate_from_state = recreate_from_state;
    plugin_type.create_state        = create_state;
  }
  
  PR_add_plugin_type(&plugin_type);
}

