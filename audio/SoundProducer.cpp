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



#include <unistd.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>

static inline int myisnormal(float val){
  return isnormal(val);
}

static inline int myfpclassify(float val){
  return fpclassify(val);
}

static inline int myisnan(float val){
  return isnan(val);
}

static inline int myisinf(float val){
  return isinf(val);
}


#include "pa_memorybarrier.h"

#include "monotonic_timer.c"


#include "../common/nsmtracker.h"
#include "../common/OS_Player_proc.h"
#include "../common/OS_visual_input.h"
#include "../common/visual_proc.h"

#include "SoundPlugin.h"
#include "SoundPlugin_proc.h"
#include "system_compressor_wrapper_proc.h"

#include "SoundProducer_proc.h"
#include "Mixer_proc.h"
#include "MultiCore_proc.h"

#include "fade_envelopes.h"



#if 0
faust conversions:
db2linear(x)	= pow(10, x/20.0);
linear2db(x)	= 20*log10(x);
zita output default: curr: -40, min: -70, max: 40
#endif

#if 0

static float scale(float x, float x1, float x2, float y1, float y2){
  return y1 + ( ((x-x1)*(y2-y1))
                /
                (x2-x1)
                );
}

static float linear2db(float val){
  if(val<=0.0f)
    return 0.0f;

  float db = 20*log10(val);
  if(db<-70)
    return 0.0f;
  else if(db>40)
    return 1.0f;
  else
    return scale(db,-70,40,0,1);
}
#endif

struct SoundProducer;

#if 0
// Function iec_scale picked from meterbridge by Steve Harris.
// db is a value between 0 and 1.
static float iec_scale(float db) {
  
  db = 20.0f * log10f(db);


         float def = 0.0f; /* Meter deflection %age */
 
         if (db < -70.0f) {
                 def = 0.0f;
         } else if (db < -60.0f) {
                 def = (db + 70.0f) * 0.25f;
         } else if (db < -50.0f) {
                 def = (db + 60.0f) * 0.5f + 5.0f;
         } else if (db < -40.0f) {
                 def = (db + 50.0f) * 0.75f + 7.5;
         } else if (db < -30.0f) {
                 def = (db + 40.0f) * 1.5f + 15.0f;
         } else if (db < -20.0f) {
                 def = (db + 30.0f) * 2.0f + 30.0f;
         } else if (db < 0.0f) {
                 def = (db + 20.0f) * 2.5f + 50.0f;
         } else {
                 def = 100.0f;
         }
 
         return def * 2.0f / 200.0f;
}
#endif

namespace{
struct SoundProducerLink {

  // used both by audio links and event links
  SoundProducer *source;
  SoundProducer *target;

  // fields below only used by audio links
  int source_ch;
  int target_ch;

  int fade_pos;
  volatile enum {NO_STATE, FADING_IN, STABLE, FADING_OUT, JUST_FADED_OUT, FADED_OUT} state;

  float link_volume; 
  Smooth volume; // volume.target_value = link_volume * source->output_volume * source->volume

  float get_total_link_volume(void){
    SoundPlugin *source_plugin = SP_get_plugin(source);
    R_ASSERT_RETURN_IF_FALSE2(source_plugin != NULL, 0.0f);
    
    if (source_plugin->output_volume_is_on) // (Note that plugin->volume==0 when plugin->volume_onoff==false, so we don't need to test for that)
      return link_volume * source_plugin->output_volume * source_plugin->volume;
    else
      return 0.0f;
  }
  
  SoundProducerLink(SoundProducer *source, SoundProducer *target)
    : source(source)
    , target(target)
    , source_ch(0)
    , target_ch(0)
    , fade_pos(0)
    , state(NO_STATE)
    , link_volume(1.0)
  {
    SMOOTH_init(&volume, get_total_link_volume(), MIXER_get_buffer_size());
  }

  ~SoundProducerLink(){
    SMOOTH_release(&volume);
  }
};
}

#if 0

static int dllen(DoublyLinkedList *l){
  int ret=0;
  while(l!=NULL){
    ret++;
    l=l->next;
  }
  return ret;
}

static float scale(float x, float x1, float x2, float y1, float y2){
  return y1 + ( ((x-x1)*(y2-y1))
                /
                (x2-x1)
                );
}

#endif

// Example using SSE instructions here: http://codereview.stackexchange.com/questions/5143/min-max-function-of-1d-array-in-c-c
// That code is 4 times faster, but requires SSE.
static float RT_get_max_val(float *array, int num_elements){
  float ret=0.0f;
  float minus_ret = 0.0f;
  
  for(int i=0;i<num_elements;i++){
    float val = array[i];
    if(val>ret){
      ret=val;
      minus_ret = -val;
    }else if (val<minus_ret){
      ret = -val;
      minus_ret = val;
    }
  }

  return ret;
}

#if 0
static void RT_apply_volume(float *sound, int num_frames, float volume){
  if(volume!=1.0f)
    for(int i=0;i<num_frames;i++)
      sound[i] *= volume;
}
#endif

#if 0
static void RT_apply_volume2(float *sound, int num_frames, float start_volume, float end_volume){
  if(start_volume==end_volume || fabsf(start_volume-end_volume) < 0.001){
    if(end_volume!=1.0)
      for(int i=0;i<num_frames;i++)
        sound[i] *= end_volume;
  }else{
    float inc = (end_volume-start_volume) / num_frames;
    for(int i=0;i<num_frames;i++){
      start_volume += inc;
      sound[i] *= start_volume;
    }
  }
}
#endif

#if 0
static void RT_copy_sound_and_apply_volume(float *to_sound, float *from_sound, int num_frames, float start_volume, float end_volume){
  if(start_volume==end_volume || fabsf(start_volume-end_volume) < 0.001){
    if(end_volume!=1.0)
      for(int i=0;i<num_frames;i++)
        to_sound[i] = from_sound[i] * end_volume;
    else
      memcpy(to_sound,from_sound,sizeof(float)*num_frames);
  }else{
    float inc = (end_volume-start_volume) / (float)num_frames;
    for(int i=0;i<num_frames;i++){
      start_volume += inc;
      to_sound[i] = from_sound[i]*start_volume;
    }
  }
}
#endif

static void RT_apply_dry_wet(float **dry, int num_dry_channels, float **wet, int num_wet_channels, int num_frames, Smooth *wet_values){
  int num_channels = std::min(num_dry_channels,num_wet_channels);
  for(int ch=0;ch<num_channels;ch++){
    float *w=wet[ch];
    float *d=dry[ch];

    SMOOTH_apply_volume(wet_values, w, num_frames);
    SMOOTH_mix_sounds_using_inverted_values(wet_values, w, d, num_frames);
  }
}

static void RT_fade_in2(float *sound, int pos, int num_frames){
  for(int i=0;i<num_frames;i++)
    sound[i] *= fade_in_envelope[i+pos];
}


static void RT_fade_out2(float *sound, int pos, int num_frames){
  for(int i=0;i<num_frames;i++)
    sound[i] *= fade_out_envelope[i+pos];
}



static RSemaphore *signal_from_RT = NULL;

static void PLUGIN_RT_process(SoundPlugin *plugin, int64_t time, int num_frames, float **inputs, float **outputs, bool process_plugins){

  if (process_plugins == false){
    for(int ch=0;ch<plugin->type->num_outputs;ch++)
      memset(outputs[ch], 0, sizeof(float)*num_frames);
    return;
  }
  
  plugin->type->RT_process(plugin, time, num_frames, inputs, outputs);

  float sum=0.0f;

  for(int ch=0;ch<plugin->type->num_outputs;ch++)
    for(int i=0;i<num_frames;i++)
      sum += outputs[ch][i];

  if(sum!=0.0f && !myisnormal(sum) ){
    for(int ch=0;ch<plugin->type->num_outputs;ch++)
      for(int i=0;i<num_frames;i++)
        outputs[ch][i] = 0.0f;

    volatile struct Patch *patch = plugin->patch;
    const char *sigtype = myisnan(sum)?"nan":myisinf(sum)?"inf":myfpclassify(sum)==FP_SUBNORMAL?"denormal":"<something else\?\?\?>";
    RT_message("Error!\n"
               "\n"
               "The instrument named \"%s\" of type %s/%s\n"
               "generated one or more signals of type \"%s\".\n"
               "\n"
               "These signals have now been replaced with silence.\n"
               "\n"
               "This warning will pop up as long as the instrument misbehaves.\n"
               "\n"
               "If the instrument is a third party instrument, for instance a\n"
               "VST plugin, a LADSPA plugin, or a Pd patch, please contact the\n"
               "third party to fix the bug.\n",
               patch==NULL?"<no name>":patch->name,
               plugin->type->type_name, plugin->type->name,
               sigtype);
  }
}

static int get_bus_num(SoundPlugin *plugin){
  int is_bus = !strcmp(plugin->type->type_name,"Bus");

  if(is_bus){
    if(!strcmp(plugin->type->name,"Bus 1"))
      return 0;
    else
      return 1;
  } else
    return -1;
}

struct SoundProducer {
  SoundPlugin *_plugin;

  QAtomicInt is_processed;

  int _num_inputs;
  int _num_outputs;
  int _num_dry_sounds;
  
  int64_t _last_time;

  double running_time;

  bool _is_bus;
  int _bus_num;
  
  float **_dry_sound;
  float **_input_sound; // I'm not sure if it's best to place this data on the heap or the stack. Currently the heap is used. Advantage of heap: Avoid (having to think about the possibility of) risking running out of stack. Advantage of stack: Fewer cache misses.
  float **_output_sound;

  float *_input_peaks;
  float *_volume_peaks;

  radium::Vector<SoundProducer*> dependants;

  QAtomicInt num_dependencies_left;  // = num_dependencies + (is_bus ? num_not_bus_descendants : 0). Decreased during processing. When the number is zero, it is scheduled for processing.

  int num_dependencies;              // = size(_input_eproducers) + size(_input_producers)

  radium::Vector<SoundProducerLink*> _input_eproducers;
  radium::Vector<SoundProducerLink*> *_input_producers; // one SoundProducerLink per in-channel

  SoundProducer(SoundPlugin *plugin, int num_frames)
    : _plugin(plugin)
    , _num_inputs(plugin->type->num_inputs)
    , _num_outputs(plugin->type->num_outputs)
    , _last_time(-1)
    , running_time(0.0)
    , num_dependencies(0)
  {
    printf("New SoundProducer. Inputs: %d, Ouptuts: %d. plugin->type->name: %s\n",_num_inputs,_num_outputs,plugin->type->name);

    _bus_num = get_bus_num(plugin);
    _is_bus = _bus_num >= 0;

    if(_num_inputs>0)
      _num_dry_sounds = _num_inputs;
    else
      _num_dry_sounds = _num_outputs;

    _input_producers = new radium::Vector<SoundProducerLink*>[_num_inputs];
    //_input_producers = (SoundProducerLink*)calloc(_num_inputs, sizeof(SoundProducerLink));//new SoundProducerLink[_num_inputs];

    allocate_sound_buffers(num_frames);

    _input_peaks = (float*)calloc(sizeof(float),_num_dry_sounds);
    _volume_peaks = (float*)calloc(sizeof(float),_num_outputs);

    MIXER_add_SoundProducer(this);

    //memset(&_input_eproducers,0,sizeof(SoundProducerLink));

    printf("*** Finished... New SoundProducer. Inputs: %d, Ouptuts: %d. plugin->type->name: %s\n",_num_inputs,_num_outputs,plugin->type->name);
    //getchar();
  }

  ~SoundProducer(){    
    MIXER_remove_SoundProducer(this);

    if (PLAYER_is_running()) {

      free(_input_peaks);
      free(_volume_peaks);

      R_ASSERT(_input_eproducers.size()==0);
      
      for(int ch=0;ch<_num_inputs;ch++)
        R_ASSERT(_input_producers[ch].size()==0);
      
      delete[] _input_producers;
      
      free_sound_buffers();
    }
  }

  void free_sound_buffers(){
    for(int ch=0;ch<_num_dry_sounds;ch++)
      free(_dry_sound[ch]);

    for(int ch=0;ch<_num_inputs;ch++)
      free(_input_sound[ch]);

    for(int ch=0;ch<_num_outputs;ch++)
      free(_output_sound[ch]);

    free(_dry_sound);
    free(_input_sound);
    free(_output_sound);
  }

    
  void RT_set_bus_descendant_type_for_plugin(void){
    R_ASSERT(PLAYER_current_thread_has_lock());

    if(_plugin->bus_descendant_type != MAYBE_A_BUS_DESCENDANT)
      return;

    if(_is_bus){
      _plugin->bus_descendant_type = IS_BUS_DESCENDANT;
      return;
    }

    // audio links
    for(int ch=0;ch<_num_inputs;ch++)
      for (SoundProducerLink *link : _input_producers[ch]){
        link->source->RT_set_bus_descendant_type_for_plugin();
        if(link->source->_plugin->bus_descendant_type==IS_BUS_DESCENDANT){
          _plugin->bus_descendant_type = IS_BUS_DESCENDANT;
          return;
        }
      }

    // event links
    for (SoundProducerLink *elink : _input_eproducers) {
      elink->source->RT_set_bus_descendant_type_for_plugin();
      
      if(elink->source->_plugin->bus_descendant_type==IS_BUS_DESCENDANT){
        _plugin->bus_descendant_type = IS_BUS_DESCENDANT;
        return;
      }
    }

    _plugin->bus_descendant_type = IS_BUS_PROVIDER;
  }

  void allocate_sound_buffers(int num_frames){
    _dry_sound = (float**)(calloc(sizeof(float*),_num_dry_sounds));
    for(int ch=0;ch<_num_dry_sounds;ch++)
      _dry_sound[ch] = (float*)calloc(sizeof(float),num_frames);

    _input_sound = (float**)(calloc(sizeof(float*),_num_inputs));
    for(int ch=0;ch<_num_inputs;ch++)
      _input_sound[ch] = (float*)calloc(sizeof(float),num_frames);

    _output_sound = (float**)(calloc(sizeof(float*),_num_outputs));
    for(int ch=0;ch<_num_outputs;ch++)
      _output_sound[ch] = (float*)calloc(sizeof(float),num_frames);
  }

  bool is_recursive(SoundProducer *start_producer){
    if(start_producer==this)
      return true;

    for(int ch=0;ch<_num_inputs;ch++)
      for (SoundProducerLink *link : _input_producers[ch])
        if(link->source->is_recursive(start_producer)==true)
          return true;

    for (SoundProducerLink *elink : _input_eproducers)
      if(elink->source->is_recursive(start_producer)==true)
        return true;

    return false;
  }

  bool add_eventSoundProducerInput(SoundProducer *source){
    if(source->is_recursive(this)==true){
      GFX_Message(NULL, "Recursive graph not supported\n");
      return false;
    }

    SoundProducerLink *elink = new SoundProducerLink(source, this);
    
    fprintf(stderr, "____add einput\n");

    source->dependants.ensure_there_is_room_for_one_more_without_having_to_allocate_memory();
    _input_eproducers.ensure_there_is_room_for_one_more_without_having_to_allocate_memory();

    PLAYER_lock();{

      source->dependants.add(this);
      
      _input_eproducers.add(elink);
      num_dependencies++;
      
      MIXER_RT_set_bus_descendand_type_for_all_plugins(); // hmm.
    }PLAYER_unlock();

    return true;
  }
  
  bool add_SoundProducerInput(SoundProducer *source, int source_ch, int target_ch){
    //fprintf(stderr,"*** this: %p. Adding input %p / %d,%d\n",this,sound_producer,sound_producer_ch,ch);

    if(source->is_recursive(this)==true){
      GFX_Message(NULL, "Recursive graph not supported\n");
      return false;
    }

    SoundProducerLink *link = new SoundProducerLink(source, this);
    link->source_ch = source_ch;
    link->target_ch = target_ch;
    
    link->fade_pos = 0;
    link->state    = SoundProducerLink::FADING_IN;

    fprintf(stderr, "____add input\n");

    source->dependants.ensure_there_is_room_for_one_more_without_having_to_allocate_memory();
    _input_producers[target_ch].ensure_there_is_room_for_one_more_without_having_to_allocate_memory();
      
    PLAYER_lock();{

      source->dependants.add(this);
            
      _input_producers[target_ch].add(link);
      num_dependencies++;
      
      MIXER_RT_set_bus_descendand_type_for_all_plugins();
    }PLAYER_unlock();

    return true;
  }


  void remove_eventSoundProducerInput(SoundProducer *source){
    for (SoundProducerLink *elink : _input_eproducers) {

      if(elink->source==source){

        fprintf(stderr, "____remove einput\n");
        
        PLAYER_lock();{

          _input_eproducers.remove(elink);
          num_dependencies--;
          
          source->dependants.remove(this);
          
          MIXER_RT_set_bus_descendand_type_for_all_plugins();
        }PLAYER_unlock();
                
        delete elink;

        return;
      }
      
    }
    
    fprintf(stderr,"huffda2. links: %p.\n",_input_eproducers.elements); // provoke a crash detected by the crash reporter
    abort();
  }
  
  void remove_SoundProducerInput(SoundProducer *source, int source_ch, int target_ch){
    //printf("**** Asking to remove connection\n");

    if (PLAYER_is_running()==false)
      return;

    //fprintf(stderr,"*** this: %p. Removeing input %p / %d,%d\n",this,sound_producer,sound_producer_ch,ch);
    for (SoundProducerLink *link : _input_producers[target_ch]) {

      if(link->source==source && link->source_ch==source_ch){
        
        while(link->state == SoundProducerLink::FADING_IN) {
          PLAYER_memory_debug_wake_up();
          usleep(3000);
        }

        PLAYER_lock();{
          link->fade_pos = 0;
          link->state    = SoundProducerLink::FADING_OUT;
        }PLAYER_unlock();

        PLAYER_memory_debug_wake_up();

        RSEMAPHORE_wait(signal_from_RT,1);

        fprintf(stderr, "____remove input\n");
        
        PLAYER_lock();{
          
          _input_producers[target_ch].remove(link);
          num_dependencies--;

          source->dependants.remove(this);
          
          MIXER_RT_set_bus_descendand_type_for_all_plugins();
        }PLAYER_unlock();

        delete link;

        return;
      }
    }

    fprintf(stderr,"huffda. links: %p. ch: %d\n",_input_producers[target_ch].elements,target_ch);  // provoke a crash detected by the crash reporter
    R_ASSERT(false);
  }

  std::vector<SoundProducerLink *> get_all_links(void){
    std::vector<SoundProducerLink *> links;

    for(int ch=0;ch<_num_inputs;ch++)
      for (SoundProducerLink *link : _input_producers[ch])
        links.push_back(link);

    return links;
  }

  std::vector<SoundProducerLink *> get_all_elinks(void){
    std::vector<SoundProducerLink *> elinks;

    for (SoundProducerLink *elink : _input_eproducers)
      elinks.push_back(elink);

    return elinks;
  }

  // fade in 'input'
  void RT_crossfade_in2(float *input, float *output, int fade_pos, int num_frames){
    RT_fade_in2(input, fade_pos, num_frames);
    RT_fade_out2(output, fade_pos, num_frames);
          
    for(int i=0;i<num_frames;i++)
      output[i] += input[i];
  }

  // fade out 'input'
  void RT_crossfade_out2(float *input, float *output, int fade_pos, int num_frames){
    RT_fade_out2(input, fade_pos, num_frames);
    RT_fade_in2(output, fade_pos, num_frames);
          
    for(int i=0;i<num_frames;i++)
      output[i] += input[i];
  }

  void RT_apply_system_filter_apply(SystemFilter *filter, float **input, float **output, int num_channels, int num_frames, bool process_plugins){
    if(filter->plugins==NULL)
      if(num_channels==0){
        return;
      }else if(num_channels==1){
        float *s[2];
        float temp_ch2[num_frames];
        s[0] = input[0];
        s[1] = temp_ch2;
        memset(s[1],0,sizeof(float)*num_frames);
        COMPRESSOR_process(_plugin->compressor, s, s, num_frames);
        if(s[0] != output[0])
          memcpy(output[0],s[0],sizeof(float)*num_frames);
      }else{
        COMPRESSOR_process(_plugin->compressor, input, output, num_frames);
      }
    else
      for(int ch=0;ch<num_channels;ch++)
        PLUGIN_RT_process(filter->plugins[ch], 0, num_frames, &input[ch], &output[ch], process_plugins);
  }

  // Quite chaotic with all the on/off is/was booleans.
  void RT_apply_system_filter(SystemFilter *filter, float **sound, int num_channels, int num_frames, bool process_plugins){
    if(filter->is_on==false && filter->was_on==false)
      return;

    {
      float *s[num_channels];
      float filter_sound[num_channels*num_frames];
      
      for(int ch=0;ch<num_channels;ch++)
        s[ch] = &filter_sound[ch*num_frames];
      
      if(filter->is_on==true){
        
        if(filter->was_off==true){ // fade in
          RT_apply_system_filter_apply(filter,sound,s,num_channels,num_frames, process_plugins);
          
          for(int ch=0;ch<num_channels;ch++)
            RT_crossfade_in2(s[ch], sound[ch], filter->fade_pos, num_frames);

          filter->fade_pos += num_frames;
          if(filter->fade_pos==FADE_LEN){
            filter->was_off=false;
            filter->fade_pos = 0;
            filter->was_on=true;
          }
          
        }else{
          
          RT_apply_system_filter_apply(filter,sound,sound,num_channels,num_frames, process_plugins);      
          filter->was_on=true;   
        }
       
        
      }else if(filter->was_on==true){ // fade out.
        
        RT_apply_system_filter_apply(filter,sound,s,num_channels,num_frames, process_plugins);
        
        for(int ch=0;ch<num_channels;ch++)
          RT_crossfade_out2(s[ch], sound[ch], filter->fade_pos, num_frames);
                
        filter->fade_pos += num_frames;
        if(filter->fade_pos==FADE_LEN){
          filter->was_on=false;
          filter->was_off=true;
          filter->fade_pos = 0;
        }
      }

    }
  }

  void RT_called_for_each_soundcard_block(void){
    
    PLUGIN_update_smooth_values(_plugin);

    for(int ch=0;ch<_num_inputs;ch++){
      
      for (SoundProducerLink *link : _input_producers[ch]) {

        SMOOTH_set_target_value(&link->volume, link->get_total_link_volume());
        
        SMOOTH_update_target_will_be_modified_value(&link->volume);

#if 0
        if (SMOOTH_are_we_going_to_modify_target_when_mixing_sounds_questionmark(&link->volume))
          link->target->num_dependencies++;
#endif
      }

    }

#if 0
    for (SoundProducerLink *elink : _input_eproducers)
      elink->target->num_dependencies++;
#endif
  }
  
  bool has_run(int64_t time){
    return _last_time == time;
  }

  void RT_process(int64_t time, int num_frames, bool process_plugins){
    if(has_run(time))
      return;

    _last_time = time;


    // First run SoundProducers that sends events to us
    for (SoundProducerLink *elink : _input_eproducers)
      elink->source->RT_process(time, num_frames, process_plugins);
      
    PLUGIN_update_smooth_values(_plugin);
    
    // Gather sound data
    for(int ch=0;ch<_num_inputs;ch++){
      float *channel_target = _dry_sound[ch];

      memset(channel_target, 0, sizeof(float)*num_frames);

      for (SoundProducerLink *link : _input_producers[ch]) {
        
        if (true || link->volume.target_will_be_modified) {

          SMOOTH_called_per_block(&link->volume);
                  
          float fade_sound[num_frames]; // When fading, 'input_producer_sound' will point to this array.
          
          SoundProducer *source_sound_producer = link->source;
          float *input_producer_sound = source_sound_producer->RT_get_channel(time, num_frames, link->source_ch, process_plugins);
          
          // fade in
          if(link->state==SoundProducerLink::FADING_IN){
            memcpy(fade_sound,input_producer_sound, num_frames*sizeof(float));
            RT_fade_in2(fade_sound, link->fade_pos, num_frames);
            input_producer_sound = &fade_sound[0];
            
            link->fade_pos += num_frames;
            if(link->fade_pos==FADE_LEN)
              link->state=SoundProducerLink::STABLE;
          }
          
          // fade out
          else if(link->state==SoundProducerLink::FADING_OUT){
            memcpy(fade_sound,input_producer_sound, num_frames*sizeof(float));
            RT_fade_out2(fade_sound, link->fade_pos, num_frames);
            input_producer_sound = &fade_sound[0];
            
            link->fade_pos += num_frames;
            if(link->fade_pos==FADE_LEN){
              RSEMAPHORE_signal(signal_from_RT,1);
              link->state=SoundProducerLink::JUST_FADED_OUT;
            }
          }
          
          // Apply volume and mix, unless faded out.
          if (link->state != SoundProducerLink::FADED_OUT)
            SMOOTH_mix_sounds(&link->volume, channel_target, input_producer_sound, num_frames);
          
          if (link->state == SoundProducerLink::JUST_FADED_OUT)
            link->state = SoundProducerLink::FADED_OUT;

        } // end if (SMOOTH_are_we_going_to_modify_target_when_mixing_sounds_questionmark(&link->volume))
        
      } // end for (SoundProducerLink *link : _input_producers[ch])
      
    } // for(int ch=0;ch<_num_inputs;ch++)


    bool is_a_generator = _num_inputs==0;
    bool do_bypass      = _plugin->drywet.smoothing_is_necessary==false && _plugin->drywet.value==0.0f;


    if(is_a_generator)
      PLUGIN_RT_process(_plugin, time, num_frames, _input_sound, _dry_sound, process_plugins);


    // Input peaks
    {
      for(int ch=0;ch<_num_dry_sounds;ch++){
        float peak = do_bypass ? 0.0f : RT_get_max_val(_dry_sound[ch],num_frames) *  _plugin->input_volume.target_value;

        if(_plugin->input_volume_peak_values_for_chip!=NULL)
          _plugin->input_volume_peak_values_for_chip[ch] = peak;

        if(_plugin->input_volume_peak_values!=NULL)
          _plugin->input_volume_peak_values[ch] = peak;

        if (ch<2)
          _plugin->system_volume_peak_values[ch] = peak; // Value only used by the slider at the bottom bar.
      }
    }


    if(do_bypass){

      int num_channels = std::min(_num_dry_sounds,_num_outputs);
      for(int ch=0;ch<num_channels;ch++){
        memcpy(_output_sound[ch], _dry_sound[ch], num_frames*sizeof(float));
      }

    }else{ // do_bypass

      if(is_a_generator){

        // Apply input volume and fill output
        for(int ch=0;ch<_num_outputs;ch++)
          SMOOTH_copy_sound(&_plugin->input_volume, _output_sound[ch], _dry_sound[ch], num_frames);


      }else{

        // Apply input volume
        for(int ch=0;ch<_num_inputs;ch++)
          SMOOTH_copy_sound(&_plugin->input_volume, _input_sound[ch], _dry_sound[ch], num_frames);
        
        // Fill output
        PLUGIN_RT_process(_plugin, time, num_frames, _input_sound, _output_sound, process_plugins);

      }

      // compressor
      RT_apply_system_filter(&_plugin->comp,      _output_sound, _num_outputs, num_frames, process_plugins);

      // filters
      RT_apply_system_filter(&_plugin->lowshelf,  _output_sound, _num_outputs, num_frames, process_plugins);
      RT_apply_system_filter(&_plugin->eq1,       _output_sound, _num_outputs, num_frames, process_plugins);
      RT_apply_system_filter(&_plugin->eq2,       _output_sound, _num_outputs, num_frames, process_plugins);
      RT_apply_system_filter(&_plugin->highshelf, _output_sound, _num_outputs, num_frames, process_plugins);
      RT_apply_system_filter(&_plugin->lowpass,   _output_sound, _num_outputs, num_frames, process_plugins);
  
      // dry/wet
      RT_apply_dry_wet(_dry_sound, _num_dry_sounds, _output_sound, _num_outputs, num_frames, &_plugin->drywet);

    } // else do_bypass

    
    // Output pan
    SMOOTH_apply_pan(&_plugin->pan, _output_sound, _num_outputs, num_frames);

    // Right channel delay ("width")
    if(_num_outputs>1)
      RT_apply_system_filter(&_plugin->delay, &_output_sound[1], _num_outputs-1, num_frames, process_plugins);


    // Output peaks
    {
      for(int ch=0;ch<_num_outputs;ch++){
        float output_peak = RT_get_max_val(_output_sound[ch],num_frames);

        float output_volume_peak = output_peak * _plugin->volume;

        // Chip volume
        if(_plugin->volume_peak_values_for_chip!=NULL)
          _plugin->volume_peak_values_for_chip[ch] = output_volume_peak;

        // "Volume"
        if(_plugin->volume_peak_values!=NULL)
          _plugin->volume_peak_values[ch] = output_volume_peak;

        // "Reverb Bus" and "Chorus Bus"
        if (ch < 2) // buses only have two channels
          for(int bus=0;bus<2;bus++)
            if(_plugin->bus_volume_peak_values[bus]!=NULL)
              _plugin->bus_volume_peak_values[bus][ch] = output_peak * _plugin->bus_volume[bus].target_value;
        
        // "Out"
        if(_plugin->output_volume_peak_values!=NULL) {
          if (_plugin->output_volume_is_on)
            _plugin->output_volume_peak_values[ch] = output_peak * _plugin->output_volume;
          else
            _plugin->output_volume_peak_values[ch] = 0.0;
        }
      }
    }
  }

  float *RT_get_channel(int64_t time, int num_frames, int ret_ch, bool process_plugins){
    if(!has_run(time))
      R_ASSERT(!g_running_multicore);

    RT_process(time, num_frames, process_plugins);
    return _output_sound[ret_ch];
  }
};



SoundProducer *SP_create(SoundPlugin *plugin){
  static bool semaphore_inited=false;

  if(semaphore_inited==false){
    signal_from_RT = RSEMAPHORE_create(0);
    semaphore_inited=true;
  }

#if 0
  int bus_num = get_bus_num(plugin);

  if (bus_num != -1){
    SoundProducer *bus1,*bus2;
    MIXER_get_buses(bus1,bus2);
  }
#endif

  return new SoundProducer(plugin, MIXER_get_buffer_size());
}

void SP_delete(SoundProducer *producer){
  printf("Deleting \"%s\"\n",producer->_plugin->type->type_name);
  delete producer;
}

// Returns false if the link could not be made. (i.e. the link was recursive)
bool SP_add_elink(SoundProducer *target, SoundProducer *source){
  return target->add_eventSoundProducerInput(source);
}

bool SP_add_link(SoundProducer *target, int target_ch, SoundProducer *source, int source_ch){
  return target->add_SoundProducerInput(source,source_ch,target_ch);
}

void SP_remove_elink(SoundProducer *target, SoundProducer *source){
  target->remove_eventSoundProducerInput(source);
}

void SP_remove_link(SoundProducer *target, int target_ch, SoundProducer *source, int source_ch){
  target->remove_SoundProducerInput(source,source_ch,target_ch);
}


#define STD_VECTOR_APPEND(a,b) a.insert(a.end(),b.begin(),b.end());

void SP_remove_all_links(std::vector<SoundProducer*> soundproducers){

  std::vector<SoundProducerLink *> links_to_delete;
  std::vector<SoundProducerLink *> elinks_to_delete;
  
  // Find links
  for(unsigned int i=0;i<soundproducers.size();i++){
    std::vector<SoundProducerLink *> links_to_delete_here = soundproducers.at(i)->get_all_links();
    STD_VECTOR_APPEND(links_to_delete, links_to_delete_here);

    std::vector<SoundProducerLink *> elinks_to_delete_here = soundproducers.at(i)->get_all_elinks();
    STD_VECTOR_APPEND(elinks_to_delete, elinks_to_delete_here);
  }

  // Wait until all links are finished fading in.
  if (PLAYER_is_running())
    for(unsigned int i=0;i<links_to_delete.size();i++) {
      while(links_to_delete.at(i)->state == SoundProducerLink::FADING_IN) {
        PLAYER_memory_debug_wake_up();
        usleep(3000);
      }
    }

  // Change state
  PLAYER_lock();{
    for(unsigned int i=0;i<links_to_delete.size();i++){
      links_to_delete.at(i)->state = SoundProducerLink::FADING_OUT;
      links_to_delete.at(i)->fade_pos = 0;      
    }
  }PLAYER_unlock();

  
  if (PLAYER_is_running()) {
    PLAYER_memory_debug_wake_up();
    RSEMAPHORE_wait(signal_from_RT,links_to_delete.size());
    
    PLAYER_lock();{
      for(unsigned int i=0;i<links_to_delete.size();i++){
        SoundProducerLink *link = links_to_delete.at(i);
        link->target->_input_producers[link->target_ch].remove(link);
        link->source->dependants.remove(link->target);
        link->target->num_dependencies--;
      }
      for(unsigned int i=0;i<elinks_to_delete.size();i++){
        SoundProducerLink *elink = elinks_to_delete.at(i);
        elink->target->_input_eproducers.remove(elink);
        elink->source->dependants.remove(elink->target);
        elink->target->num_dependencies--;
      }
    }PLAYER_unlock();
    
  }
  
  // Delete
  for(unsigned int i=0;i<links_to_delete.size();i++)
    delete links_to_delete.at(i);
  for(unsigned int i=0;i<elinks_to_delete.size();i++)
    delete elinks_to_delete.at(i);
}

void SP_RT_called_for_each_soundcard_block(SoundProducer *producer){
  producer->RT_called_for_each_soundcard_block();
}
    
void SP_RT_process(SoundProducer *producer, int64_t time, int num_frames, bool process_plugins){
  producer->RT_process(time, num_frames, process_plugins);
}



/*********************************************************
 *************** MULTICORE start *************************
 *********************************************************/

#include "MultiCore.cpp"

/*********************************************************
 *************** MULTICORE end ***************************
 *********************************************************/



#if 0
void SP_RT_clean_output(SoundProducer *producer, int num_frames){
  for(int ch=0;ch<producer->_num_outputs;ch++)
    memset(producer->_output_sound[ch],0,sizeof(float)*num_frames);
}
#endif

// This function is called from bus_type->RT_process.
void SP_RT_process_bus(float **outputs, int64_t time, int num_frames, int bus_num, bool process_plugins){

  memset(outputs[0],0,sizeof(float)*num_frames);
  memset(outputs[1],0,sizeof(float)*num_frames);

  radium::Vector<SoundProducer*> *all_sp = MIXER_get_all_SoundProducers();

  for (SoundProducer *sp : *all_sp) {
    SoundPlugin           *plugin = SP_get_plugin(sp);
    const SoundPluginType *type   = plugin->type;
    
    if(plugin->bus_descendant_type==IS_BUS_PROVIDER){
      Smooth *smooth = &plugin->bus_volume[bus_num];
      if (smooth->target_will_be_modified) { // SMOOTH_are_we_going_to_modify_target_when_mixing_sounds_questionmark(smooth)){
        if(type->num_outputs==1){
          float *channel_data0 = sp->RT_get_channel(time, num_frames, 0, process_plugins);
          SMOOTH_mix_sounds(smooth, outputs[0], channel_data0, num_frames);
          SMOOTH_mix_sounds(smooth, outputs[1], channel_data0, num_frames);
        }else if(type->num_outputs>1){
          float *channel_data0 = sp->RT_get_channel(time, num_frames, 0, process_plugins);
          float *channel_data1 = sp->RT_get_channel(time, num_frames, 1, process_plugins);
          SMOOTH_mix_sounds(smooth, outputs[0], channel_data0, num_frames);
          SMOOTH_mix_sounds(smooth, outputs[1], channel_data1, num_frames);
        }
      }
    }
  }
}

void SP_RT_set_bus_descendant_type_for_plugin(SoundProducer *producer){
  producer->RT_set_bus_descendant_type_for_plugin();
}

struct SoundPlugin *SP_get_plugin(SoundProducer *producer){
  return producer->_plugin;
}

int SP_get_bus_num(SoundProducer *sp){
  return sp->_bus_num;
}

SoundProducer *SP_get_SoundProducer(SoundPlugin *plugin){
  radium::Vector<SoundProducer*> *all_sp = MIXER_get_all_SoundProducers();

  for (SoundProducer *sp : *all_sp)
    if(SP_get_plugin(sp)==plugin)
      return sp;
  
  return NULL;
}

bool SP_replace_plugin(SoundPlugin *old_plugin, SoundPlugin *new_plugin){
  if (!PLAYER_current_thread_has_lock()) {
    RError("Current thread is not holding player lock");
    return false;
  }

  SoundProducer *sp = SP_get_SoundProducer(old_plugin);
  if (sp==NULL) {
    RError("Could not find soundproducer for plugin");
    return false;
  }

  sp->_plugin = new_plugin;
  return true;  
}

bool SP_is_plugin_running(SoundPlugin *plugin){
  return SP_get_SoundProducer(plugin)!=NULL;
}

void SP_set_buffer_size(SoundProducer *producer,int buffer_size){
  if(producer->_plugin->type->buffer_size_is_changed != NULL)
    producer->_plugin->type->buffer_size_is_changed(producer->_plugin,buffer_size);

  producer->free_sound_buffers();
  producer->allocate_sound_buffers(buffer_size);
}

double SP_get_running_time(const SoundProducer *sp){
  return sp->running_time;
}

void SP_RT_reset_running_time(SoundProducer *sp){
  sp->running_time = 0.0;
}
