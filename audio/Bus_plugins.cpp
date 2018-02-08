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

#include "../common/nsmtracker.h"
#include "SoundPlugin.h"
#include "SoundPlugin_proc.h"
#include "SoundProducer_proc.h"

#include "SoundPluginRegistry_proc.h"

#include "Bus_plugins_proc.h"

static SoundPluginType bus_type1 = {};
static SoundPluginType bus_type2 = {};
static SoundPluginType bus_type3 = {};
static SoundPluginType bus_type4 = {};
static SoundPluginType bus_type5 = {};
static SoundPluginType pipe_type = {};
static SoundPluginType pipe_type8 = {};

static SoundPluginType left_in_type = {};
static SoundPluginType right_in_type = {};

static SoundPluginType left_out_type = {};
static SoundPluginType right_out_type = {};

#if 0
static void RT_bus_process(SoundPlugin *plugin, int64_t time, int num_frames, float **inputs, float **outputs){
  SP_RT_process_bus(outputs, time, num_frames, plugin->type==&bus_type1 ? 0 : 1, true);
}
#endif

static void RT_pipe_process(SoundPlugin *plugin, int64_t time, int num_frames, float **inputs, float **outputs){
  for(int ch=0;ch<plugin->type->num_outputs;ch++)
    if(inputs[ch] != outputs[ch])
      memcpy(outputs[ch],inputs[ch],sizeof(float)*num_frames);
}

static void RT_left_in_process(SoundPlugin *plugin, int64_t time, int num_frames, float **inputs, float **outputs){
  memcpy(outputs[0],inputs[0],sizeof(float)*num_frames);
}

static void RT_right_in_process(SoundPlugin *plugin, int64_t time, int num_frames, float **inputs, float **outputs){
  memcpy(outputs[0],inputs[1],sizeof(float)*num_frames);
}

static void RT_left_out_process(SoundPlugin *plugin, int64_t time, int num_frames, float **inputs, float **outputs){
  memcpy(outputs[0],inputs[0],sizeof(float)*num_frames);
  memset(outputs[1],0,sizeof(float)*num_frames);
}

static void RT_right_out_process(SoundPlugin *plugin, int64_t time, int num_frames, float **inputs, float **outputs){
  memcpy(outputs[1],inputs[0],sizeof(float)*num_frames);
  memset(outputs[0],0,sizeof(float)*num_frames);
}

static int RT_get_audio_tail_length(struct SoundPlugin *plugin){
  return 0;
}



static SoundPlugin *bus1 = NULL;
static SoundPlugin *bus2 = NULL;
static SoundPlugin *bus3 = NULL;
static SoundPlugin *bus4 = NULL;
static SoundPlugin *bus5 = NULL;

static void *create_plugin_data(const SoundPluginType *plugin_type, struct SoundPlugin *plugin, hash_t *state, float sample_rate, int block_size, bool is_loading){
  if(plugin_type==&bus_type1)
    bus1 = plugin;
  if(plugin_type==&bus_type2)
    bus2 = plugin;
  if(plugin_type==&bus_type3)
    bus3 = plugin;
  if(plugin_type==&bus_type4)
    bus4 = plugin;
  if(plugin_type==&bus_type5)
    bus5 = plugin;
  return (void*)plugin_type;
}

static void cleanup_plugin_data(SoundPlugin *plugin){
  const SoundPluginType *plugin_type = plugin->type;
  
  if(plugin_type==&bus_type1)
    bus1 = NULL;
  if(plugin_type==&bus_type2)
    bus2 = NULL;
  if(plugin_type==&bus_type3)
    bus3 = NULL;
  if(plugin_type==&bus_type4)
    bus4 = NULL;
  if(plugin_type==&bus_type5)
    bus5 = NULL;
}

const char *BUS_get_bus_name(int bus_num){
  if(bus_num==0)
    return bus1==NULL ? "Reverb Bus" : bus1->patch->name;
  else if(bus_num==1)
    return bus2==NULL ? "Chorus Bus" : bus2->patch->name;
  else if(bus_num==2)
    return bus3==NULL ? "Aux 1 Bus" : bus3->patch->name;
  else if(bus_num==3)
    return bus4==NULL ? "Aux 2 Bus" : bus4->patch->name;
  else if(bus_num==4)
    return bus5==NULL ? "Aux 3 Bus" : bus5->patch->name;
  else {
    RError("Unknown bus num %d", bus_num);
    return "???";
  }
}

#if 0
static SoundPluginType mono_plugin_type = {
 name                     : "Mono Bus",
 num_inputs               : 1,
 num_outputs              : 1,
 is_instrument            : false,
 num_effects              : 0,
 create_plugin_data       : create_plugin_data,
 cleanup_plugin_data      : cleanup_plugin_data,

 RT_process               : RT_process,

 data                     : NULL
};


static SoundPluginType bus_type = {
 name                     : "Bus",
 num_inputs               : 2,
 num_outputs              : 2,
 is_instrument            : false,
 num_effects              : 0,
 create_plugin_data       : create_plugin_data,
 cleanup_plugin_data      : cleanup_plugin_data,

 //RT_process               : RT_bus_process,
 RT_process               : RT_pipe_process,
 RT_get_audio_tail_length : RT_get_audio_tail_length,
 
 data                     : NULL
};

static SoundPluginType pipe_type = {
 name                     : "Pipe",
 num_inputs               : 2,
 num_outputs              : 2,
 is_instrument            : false,
 num_effects              : 0,
 create_plugin_data       : create_plugin_data,
 cleanup_plugin_data      : cleanup_plugin_data,

 RT_process               : RT_pipe_process,
 RT_get_audio_tail_length : RT_get_audio_tail_length,
 
 data                     : NULL
};

static SoundPluginType pipe_type = {
 name                     : "Pipe8",
 num_inputs               : 8,
 num_outputs              : 8,
 is_instrument            : false,
 num_effects              : 0,
 create_plugin_data       : create_plugin_data,
 cleanup_plugin_data      : cleanup_plugin_data,

 RT_process               : RT_pipe_process,
 RT_get_audio_tail_length : RT_get_audio_tail_length,
 
 data                     : NULL
};



#endif

void create_bus_plugins(bool only_pipe){
  static bool has_inited = false;

  if (has_inited==false)
  {
    bus_type1.type_name                = "Bus";
    bus_type1.name                     = "Bus 1";
    bus_type1.num_inputs               = 2;
    bus_type1.num_outputs              = 2;
    bus_type1.is_instrument            = false;
    bus_type1.num_effects              = 0;
    bus_type1.will_always_autosuspend  = true,
    bus_type1.create_plugin_data       = create_plugin_data;
    bus_type1.cleanup_plugin_data      = cleanup_plugin_data;
    
    //bus_type1.RT_process               = RT_bus_process;
    bus_type1.RT_process               = RT_pipe_process;
    bus_type1.RT_get_audio_tail_length = RT_get_audio_tail_length;
      
    bus_type1.data                     = NULL;
  }

  if (has_inited==false)
  {
    bus_type2.type_name                = "Bus";
    bus_type2.name                     = "Bus 2";
    bus_type2.num_inputs               = 2;
    bus_type2.num_outputs              = 2;
    bus_type2.is_instrument            = false;
    bus_type2.num_effects              = 0;
    bus_type2.will_always_autosuspend  = true,
    bus_type2.create_plugin_data       = create_plugin_data;
    bus_type2.cleanup_plugin_data      = cleanup_plugin_data;
    
    //bus_type2.RT_process               = RT_bus_process;
    bus_type2.RT_process               = RT_pipe_process;
    bus_type2.RT_get_audio_tail_length = RT_get_audio_tail_length;
    
    bus_type2.data                     = NULL;
  }

  if (has_inited==false)
  {
    bus_type3.type_name                = "Bus";
    bus_type3.name                     = "Bus 3";
    bus_type3.num_inputs               = 2;
    bus_type3.num_outputs              = 2;
    bus_type3.is_instrument            = false;
    bus_type3.num_effects              = 0;
    bus_type3.will_always_autosuspend  = true,
    bus_type3.create_plugin_data       = create_plugin_data;
    bus_type3.cleanup_plugin_data      = cleanup_plugin_data;
    
    //bus_type2.RT_process               = RT_bus_process;
    bus_type3.RT_process               = RT_pipe_process;
    bus_type3.RT_get_audio_tail_length = RT_get_audio_tail_length;
    
    bus_type3.data                     = NULL;
  }

  if (has_inited==false)
  {
    bus_type4.type_name                = "Bus";
    bus_type4.name                     = "Bus 4";
    bus_type4.num_inputs               = 2;
    bus_type4.num_outputs              = 2;
    bus_type4.is_instrument            = false;
    bus_type4.num_effects              = 0;
    bus_type4.will_always_autosuspend  = true,
    bus_type4.create_plugin_data       = create_plugin_data;
    bus_type4.cleanup_plugin_data      = cleanup_plugin_data;
    
    //bus_type2.RT_process               = RT_bus_process;
    bus_type4.RT_process               = RT_pipe_process;
    bus_type4.RT_get_audio_tail_length = RT_get_audio_tail_length;
    
    bus_type4.data                     = NULL;
  }

  if (has_inited==false)
  {
    bus_type5.type_name                = "Bus";
    bus_type5.name                     = "Bus 5";
    bus_type5.num_inputs               = 2;
    bus_type5.num_outputs              = 2;
    bus_type5.is_instrument            = false;
    bus_type5.num_effects              = 0;
    bus_type5.will_always_autosuspend  = true,
    bus_type5.create_plugin_data       = create_plugin_data;
    bus_type5.cleanup_plugin_data      = cleanup_plugin_data;
    
    //bus_type2.RT_process               = RT_bus_process;
    bus_type5.RT_process               = RT_pipe_process;
    bus_type5.RT_get_audio_tail_length = RT_get_audio_tail_length;
    
    bus_type5.data                     = NULL;
  }

  if (has_inited==false)
  {
    pipe_type.type_name                = "Pipe";
    pipe_type.name                     = "Pipe";
    pipe_type.info                     = "A pipe sends the sound through unmodified.";
    pipe_type.num_inputs               = 2;
    pipe_type.num_outputs              = 2;
    pipe_type.is_instrument            = false;
    pipe_type.num_effects              = 0;
    pipe_type.will_always_autosuspend  = true,
    pipe_type.create_plugin_data       = create_plugin_data;
    pipe_type.cleanup_plugin_data      = cleanup_plugin_data;
    
    pipe_type.RT_process               = RT_pipe_process;
    pipe_type.RT_get_audio_tail_length = RT_get_audio_tail_length;
    
    pipe_type.data                     = NULL;
  }

  if (has_inited==false)
  {
    pipe_type8.type_name                = "Pipe";
    pipe_type8.name                     = "Pipe8";
    pipe_type8.info                     = "A pipe sends the sound through unmodified.";
    pipe_type8.num_inputs               = 8;
    pipe_type8.num_outputs              = 8;
    pipe_type8.is_instrument            = false;
    pipe_type8.num_effects              = 0;
    pipe_type8.will_always_autosuspend  = true,
    pipe_type8.create_plugin_data       = create_plugin_data;
    pipe_type8.cleanup_plugin_data      = cleanup_plugin_data;
    
    pipe_type8.RT_process               = RT_pipe_process;
    pipe_type8.RT_get_audio_tail_length = RT_get_audio_tail_length;
    
    pipe_type8.data                     = NULL;
  }

  if (has_inited==false)
  {
    left_in_type.type_name                = "2ch -> 1ch";
    left_in_type.name                     = "Keep Left channel only";
    left_in_type.info                     = "Receives a stereo sound. Sends out the left channel.";
    left_in_type.num_inputs               = 2;
    left_in_type.num_outputs              = 1;
    left_in_type.is_instrument            = false;
    left_in_type.num_effects              = 0;
    left_in_type.create_plugin_data       = create_plugin_data;
    left_in_type.cleanup_plugin_data      = cleanup_plugin_data;
    
    left_in_type.RT_process               = RT_left_in_process;
  }

  if (has_inited==false)
  {
    right_in_type.type_name                = "2ch -> 1ch";
    right_in_type.name                     = "Keep Right channel only";
    right_in_type.info                     = "Receives a stereo sound. Sends out the right channel.";
    right_in_type.num_inputs               = 2;
    right_in_type.num_outputs              = 1;
    right_in_type.is_instrument            = false;
    right_in_type.num_effects              = 0;
    right_in_type.create_plugin_data       = create_plugin_data;
    right_in_type.cleanup_plugin_data      = cleanup_plugin_data;
    
    right_in_type.RT_process               = RT_right_in_process;
  }

  if (has_inited==false)
  {
    left_out_type.type_name                = "1ch -> 2ch";
    left_out_type.name                     = "Send Left channel only";
    left_out_type.info                     = "Receives a mono sound.\nSends out a stereo sound where the Right channel is silent\n and the Left channel contains the input sound";
    left_out_type.num_inputs               = 1;
    left_out_type.num_outputs              = 2;
    left_out_type.is_instrument            = false;
    left_out_type.num_effects              = 0;
    left_out_type.create_plugin_data       = create_plugin_data;
    left_out_type.cleanup_plugin_data      = cleanup_plugin_data;
    
    left_out_type.RT_process               = RT_left_out_process;
  }

  if (has_inited==false)
  {
    right_out_type.type_name                = "1ch -> 2ch";
    right_out_type.name                     = "Send Right channel only";
    right_out_type.info                     = "Receives a mono sound.\nSends out a stereo sound where the Left channel is silent\n and the Right channel contains the input sound";
    right_out_type.num_inputs               = 1;
    right_out_type.num_outputs              = 2;
    right_out_type.is_instrument            = false;
    right_out_type.num_effects              = 0;
    right_out_type.create_plugin_data       = create_plugin_data;
    right_out_type.cleanup_plugin_data      = cleanup_plugin_data;
    
    right_out_type.RT_process               = RT_right_out_process;
  }

  has_inited = true;

  if (only_pipe) {
    PR_add_plugin_type(&pipe_type);
  } else {
    PR_add_plugin_type(&bus_type1);
    PR_add_plugin_type(&bus_type2);
    PR_add_plugin_type(&bus_type3);
    PR_add_plugin_type(&bus_type4);
    PR_add_plugin_type(&bus_type5);
    PR_add_plugin_type(&pipe_type8);
    
    PR_add_plugin_type_no_menu(&left_in_type);
    PR_add_plugin_type_no_menu(&right_in_type);
    PR_add_plugin_type_no_menu(&left_out_type);
    PR_add_plugin_type_no_menu(&right_out_type);
  }
}
