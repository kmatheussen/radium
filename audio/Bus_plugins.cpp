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


#include <inttypes.h>
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

static int RT_get_audio_tail_length(const struct SoundPlugin *plugin){
  return 0;
}




static void *create_plugin_data(const SoundPluginType *plugin_type, struct SoundPlugin *plugin, hash_t *state, float sample_rate, int block_size, bool is_loading){
  /*
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
  */
  return (void*)plugin_type;
}

static void cleanup_plugin_data(SoundPlugin *plugin){
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
    PR_add_plugin_type(&pipe_type8);
    
    PR_add_plugin_type_no_menu(&left_in_type);
    PR_add_plugin_type_no_menu(&right_in_type);
    PR_add_plugin_type_no_menu(&left_out_type);
    PR_add_plugin_type_no_menu(&right_out_type);
  }
}
