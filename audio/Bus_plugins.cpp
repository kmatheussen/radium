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


static SoundPluginType bus_type1 = {0};
static SoundPluginType bus_type2 = {0};
static SoundPluginType pipe_type = {0};

static SoundPluginType left_in_type = {0};
static SoundPluginType right_in_type = {0};

static SoundPluginType left_out_type = {0};
static SoundPluginType right_out_type = {0};

#if 0
static void RT_bus_process(SoundPlugin *plugin, int64_t time, int num_frames, float **inputs, float **outputs){
  SP_RT_process_bus(outputs, time, num_frames, plugin->type==&bus_type1 ? 0 : 1, true);
}
#endif

static void RT_pipe_process(SoundPlugin *plugin, int64_t time, int num_frames, float **inputs, float **outputs){
  int ch;
  for(ch=0;ch<plugin->type->num_outputs;ch++)
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


static SoundPlugin *bus1 = NULL;
static SoundPlugin *bus2 = NULL;

static void *create_plugin_data(const SoundPluginType *plugin_type, struct SoundPlugin *plugin, hash_t *state, float sample_rate, int block_size){
  if(plugin_type==&bus_type1)
    bus1 = plugin;
  if(plugin_type==&bus_type2)
    bus2 = plugin;
  return (void*)plugin_type;
}

static void cleanup_plugin_data(SoundPlugin *plugin_type){
}

const char *BUS_get_bus_name(int bus_num){
  if(bus_num==0)
    return bus1==NULL ? "Reverb Bus" : bus1->patch->name;
  else
    return bus2==NULL ? "Chorus Bus" : bus2->patch->name;
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

 data                     : NULL
};


#endif

void create_bus_plugins(void){
  {
    bus_type1.type_name                = "Bus";
    bus_type1.name                     = "Bus 1";
    bus_type1.num_inputs               = 2;
    bus_type1.num_outputs              = 2;
    bus_type1.is_instrument            = false;
    bus_type1.num_effects              = 0;
    bus_type1.create_plugin_data       = create_plugin_data;
    bus_type1.cleanup_plugin_data      = cleanup_plugin_data;
    
    //bus_type1.RT_process               = RT_bus_process;
    bus_type1.RT_process               = RT_pipe_process;
  
    bus_type1.data                     = NULL;
  }

  {
    bus_type2.type_name                = "Bus";
    bus_type2.name                     = "Bus 2";
    bus_type2.num_inputs               = 2;
    bus_type2.num_outputs              = 2;
    bus_type2.is_instrument            = false;
    bus_type2.num_effects              = 0;
    bus_type2.create_plugin_data       = create_plugin_data;
    bus_type2.cleanup_plugin_data      = cleanup_plugin_data;
    
    //bus_type2.RT_process               = RT_bus_process;
    bus_type2.RT_process               = RT_pipe_process;
  
    bus_type2.data                     = NULL;
  }

  {
    pipe_type.type_name                = "Pipe";
    pipe_type.name                     = "Pipe";
    pipe_type.info                     = "A pipe sends the sound through unmodified.\n";
    pipe_type.num_inputs               = 2;
    pipe_type.num_outputs              = 2;
    pipe_type.is_instrument            = false;
    pipe_type.num_effects              = 0;
    pipe_type.create_plugin_data       = create_plugin_data;
    pipe_type.cleanup_plugin_data      = cleanup_plugin_data;
    
    pipe_type.RT_process               = RT_pipe_process;
    
    pipe_type.data                     = NULL;
  }

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

  PR_add_plugin_type(&bus_type1);
  PR_add_plugin_type(&bus_type2);
  PR_add_plugin_type(&pipe_type);
  
  PR_add_plugin_type(&left_in_type);
  PR_add_plugin_type(&right_in_type);
  PR_add_plugin_type(&left_out_type);
  PR_add_plugin_type(&right_out_type);
}
