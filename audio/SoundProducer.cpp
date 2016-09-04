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


#include "../weakjack/weak_libjack.h"


#include <QFile>


//#include "pa_memorybarrier.h"


#include "monotonic_timer.c"


#include "../common/nsmtracker.h"

#include "../common/OS_Player_proc.h"
#include "../common/OS_visual_input.h"
#include "../common/threading.h"
#include "../common/visual_proc.h"
#include "../common/Vector.hpp"
#include "../common/Queue.hpp"

#include "SoundPlugin.h"
#include "SoundPlugin_proc.h"
#include "system_compressor_wrapper_proc.h"
#include "Juce_plugins_proc.h"

#include "SoundPluginRegistry_proc.h"
#include "Mixer_proc.h"
#include "MultiCore_proc.h"
#include "CpuUsage.hpp"
#include "SmoothDelay.hpp"

#include "../mixergui/QM_MixerWidget.h"

#include "fade_envelopes.h"


#include "SoundProducer_proc.h"


#if 0
faust conversions:
db2linear(x)	= pow(10, x/20.0);
linear2db(x)	= 20*log10(x);
zita output default: curr: -40, min: -70, max: 40
#endif

#if 0

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

#if 0
  static void RT_fade_in(float *sound, int num_frames){
    int i;
    for(i=0;i<num_frames;i++)
      sound[i] *= scale(i,0,num_frames,0,1);
  }

  static void RT_fade_out(float *sound, int num_frames){
    float num_frames_plus_1 = num_frames+1.0f;
    int i;
    float val = (num_frames / num_frames_plus_1);
    float inc = val - ( (num_frames-1) / num_frames_plus_1);
    
    for(i=0;i<num_frames;i++){
      sound[i] *= val;
      val -= inc;
    }
  }

  static int find_next_power2(int i){
    int ret = 2;
    while (ret < i)
      ret *= 2;
    
    return ret;
  }
#endif


#define MAX_COMPENSATED_LATENCY 1000

static float *g_empty_sound = NULL;
  
struct LatencyCompensatorDelay {
  radium::SmoothDelay _delay;
  
  float *_output_sound;
  
  LatencyCompensatorDelay()
    :_delay(MAX_COMPENSATED_LATENCY*MIXER_get_sample_rate()/1000)
  {
    _output_sound = (float*)V_calloc(sizeof(float), MIXER_get_buffer_size());
    
    if (g_empty_sound==NULL)
      g_empty_sound = (float*)V_calloc(sizeof(float), MIXER_get_buffer_size());
  }
  
  ~LatencyCompensatorDelay(){
    V_free(_output_sound);
    _output_sound = NULL;
  }

  void RT_set_preferred_delay(int preferred_delay){
    _delay.setSize(preferred_delay);
  }

  // Should be called instead of RT_process if we don't need any sound.
  void RT_call_instead_of_process(int num_frames){
    _delay.RT_process(num_frames, g_empty_sound, g_empty_sound); // Avoid leftovers from last time.
  }
  
  // May return 'input_sound'. Also, 'input_sound' is never modified.
  float *RT_process(float *input_sound, int num_frames){

    R_ASSERT_RETURN_IF_FALSE2(input_sound!=NULL, g_empty_sound);
    R_ASSERT_RETURN_IF_FALSE2(_output_sound!=NULL, g_empty_sound);
    R_ASSERT_RETURN_IF_FALSE2(_delay.fVec0!=NULL, g_empty_sound);
    R_ASSERT_RETURN_IF_FALSE2(num_frames==MIXER_get_buffer_size(), g_empty_sound);
    
    _delay.RT_process(num_frames, input_sound, _output_sound);
    
    return _output_sound;
  }
};
  
struct SoundProducerLink {

  
  SoundProducerLink(const SoundProducerLink&) = delete;
  SoundProducerLink& operator=(const SoundProducerLink&) = delete;

  LatencyCompensatorDelay _delay;
  
  // used both by audio links and event links
  SoundProducer *source;
  SoundProducer *target;

  const bool is_event_link;
  bool is_bus_link;
  
  // fields below only used by audio links
  int source_ch;
  int target_ch;

  float link_volume; 
  Smooth volume; // volume.target_value = link_volume * source->output_volume * source->volume

  bool is_active;

  DEFINE_ATOMIC(bool, should_be_turned_off);

  void turn_off(void){
    R_ASSERT(ATOMIC_GET(should_be_turned_off)==false);
    ATOMIC_SET(should_be_turned_off, true);
  }

  bool turn_off_because_someone_else_has_solo_left_parenthesis_and_we_dont_right_parenthesis(void){
    if (MIXER_someone_has_solo()){
      
      SoundPlugin *plugin = SP_get_plugin(source);

      if (ATOMIC_GET(plugin->solo_is_on) == false) {
        
        if (SP_has_input_links(source)==false) {

          if (SP_get_bus_num(source) == -1) {
            
            return true;
            
          }          
        }
      }      
    }

    return false;
  }
  
  float get_total_link_volume(void){    
    SoundPlugin *source_plugin = SP_get_plugin(source);
    float plugin_volume = source_plugin->volume;  // (Note that plugin->volume==0 when plugin->volume_onoff==false, so we don't need to test for that)

    if (turn_off_because_someone_else_has_solo_left_parenthesis_and_we_dont_right_parenthesis())
      return 0.0f;

    if (ATOMIC_GET_RELAXED(should_be_turned_off))
      return 0.0f;

    else if (is_bus_link){
      
      int bus_num = SP_get_bus_num(target);
      
      if (ATOMIC_GET_ARRAY(source_plugin->bus_volume_is_on, bus_num))
        return source_plugin->bus_volume[bus_num] * plugin_volume; // The links are invisible here, so it doesn't make sense multiplying with link_volume
      else
        return 0.0f;
      
    } else {
    
      if (ATOMIC_GET(source_plugin->output_volume_is_on))
        return source_plugin->output_volume * plugin_volume; // * link_volume (Not included since there currently isn't an interface to set the link volume.)
      else
        return 0.0f;

    }
  }


  // Called by main mixer thread before starting multicore.
  bool RT_called_for_each_soundcard_block(void){
    //R_ASSERT(THREADING_is_player_thread());

    if (is_event_link) {

      R_ASSERT(is_active==true);

      return true;
      
    } else {

      if (is_bus_link)
        if (SP_get_bus_descendant_type(source)==IS_BUS_DESCENDANT) { // need comment here what this is about
          is_active = false;
          return false;
        }

      SMOOTH_set_target_value(&volume, get_total_link_volume());        
      SMOOTH_update_target_audio_will_be_modified_value(&volume);

      bool new_is_active = volume.target_audio_will_be_modified;
      
      is_active = new_is_active;
      
      return new_is_active;
    }

    
  }

  bool can_be_removed(void){
    if (is_event_link)
      return true;

    bool ret = false;
    
    PLAYER_lock();{
      if(is_active==false)
        ret = true;
    }PLAYER_unlock();
    
    return ret;
  }
  
  SoundProducerLink(SoundProducer *source, SoundProducer *target, bool is_event_link)
    : source(source)
    , target(target)
    , is_event_link(is_event_link)
    , is_bus_link(false)
    , source_ch(0)
    , target_ch(0)
    , link_volume(1.0)
    , is_active(is_event_link)
  {
    ATOMIC_SET(should_be_turned_off, false);
    
    //SMOOTH_init(&volume, get_total_link_volume(), MIXER_get_buffer_size());
    SMOOTH_init(&volume, 0.0f, MIXER_get_buffer_size()); // To fade in, we start with 0.0f as initial volume.
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

#if 0
// Example using SSE instructions here: http://codereview.stackexchange.com/questions/5143/min-max-function-of-1d-array-in-c-c
// That code is 4 times faster, but requires SSE.
static float RT_get_max_val(const float *array, const int num_elements){
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

#else

#define RT_get_max_val JUCE_get_max_val

#endif


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

static const char *RT_check_abnormal_signal(const SoundPlugin *plugin, int num_frames, float **outputs){
  const int num_channels = plugin->type->num_outputs;
  float sum=0.0f;
  
  for(int ch=0;ch<num_channels;ch++) {
    const float *out = outputs[ch];
    float sum2 = 0.0f;
    
    for(int i=0;i<num_frames;i++)
      sum2 += out[i];
    
    sum += sum2;
  }
  
  if(sum!=0.0f && !myisnormal(sum) )
    return myisnan(sum)?"nan":myisinf(sum)?"inf":myfpclassify(sum)==FP_SUBNORMAL?"denormal":"<something else\?\?\?>";
  else
    return NULL;
}

static void PLUGIN_RT_process(SoundPlugin *plugin, int64_t time, int num_frames, float **inputs, float **outputs, bool process_plugins){

  if (process_plugins == false){
    
    for(int ch=0;ch<plugin->type->num_outputs;ch++)
      memset(outputs[ch], 0, sizeof(float)*num_frames);
    
  } else {
  
    plugin->type->RT_process(plugin, time, num_frames, inputs, outputs);

    const char *abnormal_signal_type = RT_check_abnormal_signal(plugin, num_frames, outputs);

    if (abnormal_signal_type != NULL){
      for(int ch=0;ch<plugin->type->num_outputs;ch++)
        memset(outputs[ch], 0, sizeof(float)*num_frames);
      
      volatile struct Patch *patch = plugin->patch;
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
                 abnormal_signal_type
                 );
    }

    if (RT_message_will_be_sent()==true) {
      
      for(int ch=0;ch<plugin->type->num_outputs;ch++) {
        float *out = outputs[ch];
        float peak = RT_get_max_val(out,num_frames);

        if (peak > MIN_AUTOSUSPEND_PEAK)
          RT_PLUGIN_touch(plugin);

        if (peak > 317) {
          volatile struct Patch *patch = plugin->patch;
          RT_message("Warning! (1)\n"
                     "\n"
                     "The instrument named \"%s\" of type %s/%s\n"
                     "has generated a signal of at least 50dB in channel %d.\n"
                     "Raw peak value: %f.\n"
                     "10 first frames: %f %f %f %f %f %f %f %f %f %f."
                     "\n"
                     "This warning will pop up as long as the instrument does so.\n",
                     patch==NULL?"<no name>":patch->name,
                     plugin->type->type_name, plugin->type->name,
                     ch,peak,
                     out[0], out[1],out[2],out[2],out[3],out[4],out[5],out[6],out[7],out[8],out[9]
                     );        
        }
      }
    }

  }

}

static int get_bus_num(SoundPlugin *plugin){
  if (strcmp(plugin->type->type_name,"Bus"))
    return -1;

  if(!strcmp(plugin->type->name,"Bus 1"))
    return 0;
  else if(!strcmp(plugin->type->name,"Bus 2"))
    return 1;
  else if(!strcmp(plugin->type->name,"Bus 3"))
    return 2;
  else if(!strcmp(plugin->type->name,"Bus 4"))
    return 3;
  else if(!strcmp(plugin->type->name,"Bus 5"))
    return 4;

  RError("Unknown bus: -%s-", plugin->type->name);
  return 0;
}

static int id_counter = 0;


struct SoundProducer {
  int64_t _id;
  
  SoundPlugin *_plugin;
  int _latency;
  int _highest_input_link_latency;
  
  DEFINE_ATOMIC(bool, is_processed);

  int _num_inputs;
  int _num_outputs;
  int _num_dry_sounds;
  
  int64_t _last_time;

  double running_time;
  bool has_run_for_each_block2;

  bool _autosuspending_this_cycle;
  DEFINE_ATOMIC(bool, _is_autosuspending); // Relaxed version of _autosuspending_this_cycle. Can be accessed from any thread.
  
  bool _is_bus;
  int _bus_num;
  enum BusDescendantType _bus_descendant_type; // Is 'IS_BUS_DESCENDANT' for all descendants of bus plugins. To prevent accidental feedback loops.

  LatencyCompensatorDelay *_dry_sound_latencycompensator_delays;
  
  float **_output_sound;

  float *_input_peaks;
  float *_volume_peaks;

  DEFINE_ATOMIC(int, num_dependencies_left);  // = num_dependencies + (is_bus ? num_not_bus_descendants : 0). Decreased during processing. When the number is zero, it is scheduled for processing.

  int num_dependencies;              // number of active input links

  radium::Vector<SoundProducerLink*> _input_links;
  radium::Vector<SoundProducerLink*> _output_links; // Used by MultiCore.

  radium::Vector<SoundProducerLink*> _linkbuses; // These are all ouput links.

  SoundProducer(const SoundProducer&) = delete;
  SoundProducer& operator=(const SoundProducer&) = delete;

public:
  
  SoundProducer(SoundPlugin *plugin, int num_frames, Buses buses) // buses.bus1 and buses.bus2 must be NULL if the plugin itself is a bus.
    : _id(id_counter++)
    , _plugin(plugin)
    , _latency(0)
    , _highest_input_link_latency(0)
    , _num_inputs(plugin->type->num_inputs)
    , _num_outputs(plugin->type->num_outputs)
    , _last_time(-1)
    , running_time(0.0)
    , num_dependencies(0)
  {    
    printf("New SoundProducer. Inputs: %d, Ouptuts: %d. plugin->type->name: %s\n",_num_inputs,_num_outputs,plugin->type->name);

    plugin->sp = this;
    
    ATOMIC_SET(is_processed, false);
    ATOMIC_SET(num_dependencies_left, 0);
    
    R_ASSERT(THREADING_is_main_thread());
    
    _bus_num = get_bus_num(plugin);
    _is_bus = _bus_num >= 0;

    if (_is_bus) {
      if (_bus_num == 0)
        R_ASSERT(buses.bus1==NULL);
      if (_bus_num == 1)
        R_ASSERT(buses.bus2==NULL);
      if (_bus_num == 2)
        R_ASSERT(buses.bus3==NULL);
      if (_bus_num == 3)
        R_ASSERT(buses.bus4==NULL);
      if (_bus_num == 4)
        R_ASSERT(buses.bus5==NULL);
    }else{
      R_ASSERT(buses.bus1!=NULL);
      R_ASSERT(buses.bus2!=NULL);
      R_ASSERT(buses.bus3!=NULL);
      R_ASSERT(buses.bus4!=NULL);
      R_ASSERT(buses.bus5!=NULL);
    }

    if(_is_bus)
      _bus_descendant_type = IS_BUS_DESCENDANT;
    else
      _bus_descendant_type = IS_BUS_PROVIDER;


    if(_num_inputs>0)
      _num_dry_sounds = _num_inputs;
    else
      _num_dry_sounds = _num_outputs;

    allocate_sound_buffers(num_frames);

    _input_peaks = (float*)V_calloc(sizeof(float),_num_dry_sounds);
    _volume_peaks = (float*)V_calloc(sizeof(float),_num_outputs);

    _dry_sound_latencycompensator_delays = new LatencyCompensatorDelay[_num_dry_sounds];
    
    MIXER_add_SoundProducer(this);

    if (!_is_bus && _num_outputs>0){
      SoundProducerLink *linkbus1a = new SoundProducerLink(this, buses.bus1, false);
      SoundProducerLink *linkbus1b = new SoundProducerLink(this, buses.bus1, false);
      SoundProducerLink *linkbus2a = new SoundProducerLink(this, buses.bus2, false);
      SoundProducerLink *linkbus2b = new SoundProducerLink(this, buses.bus2, false);
      SoundProducerLink *linkbus3a = new SoundProducerLink(this, buses.bus3, false);
      SoundProducerLink *linkbus3b = new SoundProducerLink(this, buses.bus3, false);
      SoundProducerLink *linkbus4a = new SoundProducerLink(this, buses.bus4, false);
      SoundProducerLink *linkbus4b = new SoundProducerLink(this, buses.bus4, false);
      SoundProducerLink *linkbus5a = new SoundProducerLink(this, buses.bus5, false);
      SoundProducerLink *linkbus5b = new SoundProducerLink(this, buses.bus5, false);

      linkbus1a->is_bus_link = true;
      linkbus1b->is_bus_link = true;
      linkbus2a->is_bus_link = true;
      linkbus2b->is_bus_link = true;
      linkbus3a->is_bus_link = true;
      linkbus3b->is_bus_link = true;
      linkbus4a->is_bus_link = true;
      linkbus4b->is_bus_link = true;
      linkbus5a->is_bus_link = true;
      linkbus5b->is_bus_link = true;

      // ch 1
      linkbus1a->source_ch = 0;
      linkbus1a->target_ch = 0;
      linkbus2a->source_ch = 0;
      linkbus2a->target_ch = 0;
      linkbus3a->source_ch = 0;
      linkbus3a->target_ch = 0;
      linkbus4a->source_ch = 0;
      linkbus4a->target_ch = 0;
      linkbus5a->source_ch = 0;
      linkbus5a->target_ch = 0;

      // ch 2
      if (_num_outputs==1) {
        linkbus1b->source_ch = 0;
        linkbus2b->source_ch = 0;
        linkbus3b->source_ch = 0;
        linkbus4b->source_ch = 0;
        linkbus5b->source_ch = 0;
      } else {
        linkbus1b->source_ch = 1;
        linkbus2b->source_ch = 1;
        linkbus3b->source_ch = 1;
        linkbus4b->source_ch = 1;
        linkbus5b->source_ch = 1;
      }
      
      linkbus1b->target_ch = 1;
      linkbus2b->target_ch = 1;
      linkbus3b->target_ch = 1;
      linkbus4b->target_ch = 1;
      linkbus5b->target_ch = 1;

      SoundProducer::add_link(linkbus1a);
      SoundProducer::add_link(linkbus1b);
      SoundProducer::add_link(linkbus2a);
      SoundProducer::add_link(linkbus2b);
      SoundProducer::add_link(linkbus3a);
      SoundProducer::add_link(linkbus3b);
      SoundProducer::add_link(linkbus4a);
      SoundProducer::add_link(linkbus4b);
      SoundProducer::add_link(linkbus5a);
      SoundProducer::add_link(linkbus5b);

      _linkbuses.push_back(linkbus1a);
      _linkbuses.push_back(linkbus1b);
      _linkbuses.push_back(linkbus2a);
      _linkbuses.push_back(linkbus2b);
      _linkbuses.push_back(linkbus3a);
      _linkbuses.push_back(linkbus3b);
      _linkbuses.push_back(linkbus4a);
      _linkbuses.push_back(linkbus4b);
      _linkbuses.push_back(linkbus5a);
      _linkbuses.push_back(linkbus5b);
    }    

    printf("*** Finished... New SoundProducer. Inputs: %d, Ouptuts: %d. plugin->type->name: %s\n",_num_inputs,_num_outputs,plugin->type->name);
    //getchar();
  }

  ~SoundProducer(){
    R_ASSERT(THREADING_is_main_thread());

    if (PLAYER_is_running()) {
      if (!_linkbuses.is_empty())
        SoundProducer::remove_links(_linkbuses);

      R_ASSERT(_input_links.size()==0);
      R_ASSERT(_output_links.size()==0);
    }

    MIXER_remove_SoundProducer(this);

    _plugin->sp = NULL;

    delete[] _dry_sound_latencycompensator_delays;
    
    V_free(_input_peaks);
    V_free(_volume_peaks);
          
    free_sound_buffers();
  }
  
  void free_sound_buffers(){

    for(int ch=0;ch<_num_outputs;ch++)
      V_free(_output_sound[ch]);

    V_free(_output_sound);
  }

    
  void RT_set_bus_descendant_type(void){
    if(_bus_descendant_type != MAYBE_A_BUS_DESCENDANT)
      return;

    if(_is_bus){
      _bus_descendant_type = IS_BUS_DESCENDANT;
      return;
    }

    for (SoundProducerLink *link : _input_links){
      if (!link->is_bus_link){
        link->source->RT_set_bus_descendant_type();
        if(link->source->_bus_descendant_type==IS_BUS_DESCENDANT){
          _bus_descendant_type = IS_BUS_DESCENDANT;
          return;
        }
      }
    }
    
    _bus_descendant_type = IS_BUS_PROVIDER;
  }

  static void RT_set_bus_descendant_types(void){
    const radium::Vector<SoundProducer*> *sp_all = MIXER_get_all_SoundProducers();
  
    // First set all descendant types to MAYBE.
    for (SoundProducer *sp : *sp_all)
      sp->_bus_descendant_type = MAYBE_A_BUS_DESCENDANT;
    
    // Then set one by one.
    for (SoundProducer *sp : *sp_all)
      sp->RT_set_bus_descendant_type();
  }

  void allocate_sound_buffers(int num_frames){
    R_ASSERT(num_frames==RADIUM_BLOCKSIZE);
    
    _output_sound = (float**)(V_calloc(sizeof(float*),_num_outputs));
    for(int ch=0;ch<_num_outputs;ch++)
      _output_sound[ch] = (float*)V_calloc(sizeof(float),num_frames);
  }

  bool is_recursive(SoundProducer *start_producer){
    if(start_producer==this)
      return true;

    for (SoundProducerLink *link : _input_links)
      if (!link->is_bus_link)
        if(link->source->is_recursive(start_producer)==true)
          return true;

    return false;
  }

  static bool add_link(SoundProducerLink *link){
    R_ASSERT(THREADING_is_main_thread());

    link->source->_output_links.ensure_there_is_room_for_one_more_without_having_to_allocate_memory();
    link->target->_input_links.ensure_there_is_room_for_one_more_without_having_to_allocate_memory();

    PLAYER_lock();{

      link->source->_output_links.push_back(link);
      
      link->target->_input_links.push_back(link);
      
      SoundProducer::RT_set_bus_descendant_types();
    }PLAYER_unlock();

    link->source->_output_links.post_add();
    link->target->_input_links.post_add();

    return true;
  }

  bool add_eventSoundProducerInput(SoundProducer *source){
    if (PLAYER_is_running()==false)
      return false;

    if(source->is_recursive(this)==true){
      GFX_Message(NULL, "Recursive graph not supported\n");
      return false;
    }

    SoundProducerLink *elink = new SoundProducerLink(source, this, true);
    
    return SoundProducer::add_link(elink);
  }
  
  bool add_SoundProducerInput(SoundProducer *source, int source_ch, int target_ch){
    //fprintf(stderr,"*** this: %p. Adding input %p / %d,%d\n",this,sound_producer,sound_producer_ch,ch);

    if (PLAYER_is_running()==false)
      return false;
    
    if(source->is_recursive(this)==true){
      GFX_Message(NULL, "Recursive graph not supported\n");
      return false;
    }

    SoundProducerLink *link = new SoundProducerLink(source, this, false);
    link->source_ch = source_ch;
    link->target_ch = target_ch;
    
    return SoundProducer::add_link(link);
  }

  static void remove_links(radium::Vector<SoundProducerLink*> &links){

      // tell them to turn off
    for(auto link : links)
      link->turn_off();
  
    if (PLAYER_is_running()) {
      PLAYER_memory_debug_wake_up();

      // Wait until all sound links can be removed
      for(auto link : links)
        while(link->can_be_removed()==false){
          PLUGIN_touch(link->source->_plugin);
          PLUGIN_touch(link->target->_plugin);
          PLAYER_memory_debug_wake_up();
          usleep(3000);
        }
      
      // Remove
      PLAYER_lock();{
        for(auto link : links){
          link->target->_input_links.remove(link);
          link->source->_output_links.remove(link);
        }
      }PLAYER_unlock();
      
    }

    // Delete
    for(auto link : links)
      delete link;
  }
  
  static void remove_link(SoundProducerLink *link){
    R_ASSERT(THREADING_is_main_thread());

    link->turn_off();

    do{
      PLUGIN_touch(link->source->_plugin);
      PLUGIN_touch(link->target->_plugin);
      PLAYER_memory_debug_wake_up();
      usleep(1000);
    }while(link->can_be_removed()==false);

    PLAYER_lock();{

      link->target->_input_links.remove(link);
      
      link->source->_output_links.remove(link);

      SoundProducer::RT_set_bus_descendant_types();
    }PLAYER_unlock();

    delete link;    
  }
  
  void remove_eventSoundProducerInput(SoundProducer *source){
    if (PLAYER_is_running()==false)
      return;
     
    for (SoundProducerLink *link : _input_links) {

      if(link->source==source && link->is_event_link==true){
        SoundProducer::remove_link(link);
        return;
      }
      
    }
    
    fprintf(stderr,"huffda2. links: %p.\n",_input_links.elements);
    R_ASSERT(false);
  }


  void remove_SoundProducerInput(SoundProducer *source, int source_ch, int target_ch){
    //printf("**** Asking to remove connection\n");

    if (PLAYER_is_running()==false)
      return;

    //fprintf(stderr,"*** this: %p. Removeing input %p / %d,%d\n",this,sound_producer,sound_producer_ch,ch);
    for (SoundProducerLink *link : _input_links) {

      if(link->is_bus_link==false && link->is_event_link==false && link->source==source && link->source_ch==source_ch && link->target_ch==target_ch){

        SoundProducer::remove_link(link);

        return;
      }
    }

    fprintf(stderr,"huffda. links: %p. ch: %d\n",_input_links.elements,target_ch);
    R_ASSERT(false);
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
    if(ATOMIC_GET(filter->is_on)==false && filter->was_on==false)
      return;

    {
      float *s[num_channels];
      float filter_sound[num_channels*num_frames];
      
      for(int ch=0;ch<num_channels;ch++)
        s[ch] = &filter_sound[ch*num_frames];
      
      if(ATOMIC_GET(filter->is_on)==true){
        
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

  bool should_consider_latency(SoundProducerLink *input_audio_link){
    if (input_audio_link->is_active==false) {
          
      if (input_audio_link->is_bus_link){

        SoundProducer *source = input_audio_link->source;

        bool legal_bus_provider = source->_bus_descendant_type==IS_BUS_PROVIDER;
        
        if (!legal_bus_provider) // We could also ignore all non-active links, but then latencies would be recalculated when turning on/off buses.
          return false;
        
      }
      
    }

    return true;
  }
  
  void RT_called_for_each_soundcard_block1(int64_t time){
    running_time = 0.0;
    has_run_for_each_block2 = false;
  }

  // Called by main mixer thread before starting multicore.
  void RT_called_for_each_soundcard_block2(int64_t time){
    if (has_run_for_each_block2 == true)
      return;

    has_run_for_each_block2 = true;

    
    // 1. Find num_dependencies
    //
    num_dependencies = 0;

    for (SoundProducerLink *link : _input_links) {

      if (link->RT_called_for_each_soundcard_block())
        num_dependencies++;
    }

    // 2. Ensure RT_called_for_each_soundcard_block2 is called for all soundobjects sending sound here. (since we read the _latency variable from those a little bit further down in this function)
    //
    for (SoundProducerLink *link : _input_links) {
      if (link->is_event_link)
        continue;

      if (!should_consider_latency(link))
        continue;

      link->source->RT_called_for_each_soundcard_block2(time);
    }

    
    // 3. Find and set _latency and _highest_input_link_latency
    //
    _highest_input_link_latency = 0;

    int my_latency = _plugin->type->RT_get_latency!=NULL ? _plugin->type->RT_get_latency(_plugin) : 0;

    {
      for (SoundProducerLink *link : _input_links) {

        if (link->is_event_link)
          continue;

        if (!should_consider_latency(link))
          continue;
        
        SoundProducer *source = link->source;

        int source_latency = source->_latency;
        if (source_latency > _highest_input_link_latency)
          _highest_input_link_latency = source_latency;
      }
      
      //int prev=_latency;
      _latency = _highest_input_link_latency + my_latency;
      //if (prev != _latency)
      //  printf("    Set latency to %d. (%s). Highest: %d. My: %d, Prev: %d\n", _latency, _plugin->patch->name, _highest_input_link_latency, my_latency,prev);

    }

    for(int ch = 0 ; ch < _num_dry_sounds ; ch++)
      _dry_sound_latencycompensator_delays[ch].RT_set_preferred_delay(my_latency);
  }
  
  bool has_run(int64_t time){
    return _last_time == time;
  }

  void RT_set_input_peak_values(float *input_peaks, float **dry_sound){
    for(int ch=0;ch<_num_dry_sounds;ch++){
      float peak = input_peaks[ch];
      
      if (RT_message_will_be_sent()==true) {
        if (peak > 317) {
          volatile struct Patch *patch = _plugin->patch;
          
          const float *out = dry_sound[ch];
          
          RT_message("Warning! (2)\n"
                     "\n"
                     "The instrument named \"%s\" received\n"
                     "a signal of at least 50dB in channel %d.\n"
                     "Raw peak value: %f.\n"
                     "10 first frames: %f %f %f %f %f %f %f %f %f %f."
                     "Input volume: %f.\n"
                     "\n"
                     "This warning will pop up as long as the instrument does so.\n",
                     patch==NULL?"<no name>":patch->name,
                     _plugin->type->type_name, _plugin->type->name,
                     ch,peak,
                     out[0], out[1],out[2],out[2],out[3],out[4],out[5],out[6],out[7],out[8],out[9],
                     SMOOTH_get_target_value(&_plugin->input_volume)
                     );        
        }
      }
      
      safe_float_write(&_plugin->input_volume_peak_values[ch], peak);
      
      //        if (ch<2)
      //          safe_volatile_float_write(&_plugin->system_volume_peak_values[ch], peak); // Value only used by the slider at the bottom bar.
    }
  }
    
  void RT_set_output_peak_values(float *volume_peaks){
    // Output peaks
    {
      for(int ch=0;ch<_num_outputs;ch++){
        float volume_peak = volume_peaks[ch];

        if (RT_message_will_be_sent()==true) {
          if (volume_peak > 317) {
            volatile struct Patch *patch = _plugin->patch;

            float *out = _output_sound[ch];
                    
            RT_message("Warning! (3)\n"
                       "\n"
                       "The instrument named \"%s\" has generated\n"
                       "a signal of at least 50dB in channel %d.\n"
                       "Raw peak value: %f.\n"
                       "10 first frames: %f %f %f %f %f %f %f %f %f %f."
                       "Volume: %f.\n"
                       "\n"
                       "This warning will pop up as long as the instrument does so.\n",
                       patch==NULL?"<no name>":patch->name,
                       _plugin->type->type_name, _plugin->type->name,
                       ch,volume_peak,
                       out[0], out[1],out[2],out[2],out[3],out[4],out[5],out[6],out[7],out[8],out[9],
                       _plugin->volume
                       );        
          }
        }
          
        // "Volume"
        safe_float_write(&_plugin->volume_peak_values[ch], volume_peak);
        
        // "Reverb Bus" and "Chorus Bus"
        if (ch < 2) { // buses only have two channels
          safe_float_write(&_plugin->bus_volume_peak_values0[ch], volume_peak * _plugin->bus_volume[0]);
          safe_float_write(&_plugin->bus_volume_peak_values1[ch], volume_peak * _plugin->bus_volume[1]);
          safe_float_write(&_plugin->bus_volume_peak_values2[ch], volume_peak * _plugin->bus_volume[2]);
          safe_float_write(&_plugin->bus_volume_peak_values3[ch], volume_peak * _plugin->bus_volume[3]);
          safe_float_write(&_plugin->bus_volume_peak_values4[ch], volume_peak * _plugin->bus_volume[4]);
        }
        
        // "Out" and Chip volume  (same value)
        {
          float output_volume_peak = volume_peak * _plugin->output_volume;
          
          safe_float_write(&_plugin->output_volume_peak_values[ch], output_volume_peak);
          
          if (ATOMIC_GET(_plugin->output_volume_is_on))
            safe_float_write(&_plugin->output_volume_peak_values_for_chip[ch], output_volume_peak);
          else
            safe_float_write(&_plugin->output_volume_peak_values_for_chip[ch], 0.0f); // The chip volume slider is not grayed out, like the out "out" slider.
        }
      }
    }
  }
  
  void RT_process(int64_t time, int num_frames, bool process_plugins){

    float dry_sound_sound[R_MAX(1,_num_dry_sounds)*num_frames] = {0};
    float *dry_sound[R_MAX(1,_num_dry_sounds)];
    for(int ch=0;ch<_num_dry_sounds;ch++)
      dry_sound[ch] = &dry_sound_sound[ch*num_frames];

    float input_sound_sound[R_MAX(1, _num_inputs)*num_frames] = {0};
    float *input_sound[R_MAX(1, _num_inputs)];
    for(int ch=0;ch<_num_inputs;ch++)
      input_sound[ch] = &input_sound_sound[ch*num_frames];
    
    R_ASSERT(has_run(time)==false);
    _last_time = time;
    
    PLUGIN_update_smooth_values(_plugin);

    // Fill inn target channels    
    for (SoundProducerLink *link : _input_links) {

      if (link->is_event_link)
        continue;
      
      if (!should_consider_latency(link))
        continue;
      
      SoundProducer *source = link->source;
      
      int latency = _highest_input_link_latency - source->_latency;
      
      if (latency >= link->_delay._delay.buffer_size) {
        RT_message("%s -> %s: Compensating for a latency of more than %dms is not supported.\nNumber of frames: %d",  source->_plugin->patch->name, link->target->_plugin->patch->name, MAX_COMPENSATED_LATENCY, latency);
        latency = link->_delay._delay.buffer_size-1;
      }

      if (latency != link->_delay._delay.getSize()) {
        link->_delay.RT_set_preferred_delay(latency);
#ifndef RELEASE
        printf("    Set latency %d. (%s -> %s)\n", latency, source->_plugin->patch->name, link->target->_plugin->patch->name);
#endif
      }
    
      if (false==link->is_active) {        
        link->_delay.RT_call_instead_of_process(num_frames);        
        continue;
      }

      if (source->_autosuspending_this_cycle) {
        link->_delay.RT_call_instead_of_process(num_frames);
        continue;
      }
      
      R_ASSERT(source->has_run(time));
      
      if (false==link->is_event_link) {

        SMOOTH_called_per_block(&link->volume);

        float *input_producer_sound = link->source->_output_sound[link->source_ch];

        float *latency_compensated_input_producer_sound = link->_delay.RT_process(input_producer_sound, num_frames);

        SMOOTH_mix_sounds(&link->volume,
                          dry_sound[link->target_ch],
                          latency_compensated_input_producer_sound,
                          num_frames
                          );

      } // end !link->is_event_link
      
    } // end for (SoundProducerLink *link : _input_links)

    
    bool is_a_generator = _num_inputs==0;
    bool do_bypass      = _plugin->drywet.smoothing_is_necessary==false && _plugin->drywet.value==0.0f;

  
    if(is_a_generator)
      PLUGIN_RT_process(_plugin, time, num_frames, input_sound, dry_sound, process_plugins);
  

    // Input peaks
    if (_num_dry_sounds > 0){
      float input_peaks[_num_dry_sounds];
      for(int ch=0;ch<_num_dry_sounds;ch++) {
        
        float dry_peak = RT_get_max_val(dry_sound[ch],num_frames);
        
        if (dry_peak > MIN_AUTOSUSPEND_PEAK)
          RT_PLUGIN_touch(_plugin);
        
        float input_volume = SMOOTH_get_target_value(&_plugin->input_volume);

        input_peaks[ch] = do_bypass ? 0.0f : dry_peak *  input_volume;
      }
      RT_set_input_peak_values(input_peaks, dry_sound);
    }


    float *_latency_dry_sound[_num_dry_sounds];
    for(int ch=0;ch<_num_dry_sounds;ch++)        
      _latency_dry_sound[ch] = _dry_sound_latencycompensator_delays[ch].RT_process(dry_sound[ch], num_frames);

    
    if(is_a_generator){
      
      // Apply input volume and fill output
      for(int ch=0;ch<_num_outputs;ch++)
        SMOOTH_copy_sound(&_plugin->input_volume, _output_sound[ch], dry_sound[ch], num_frames);
            
    }else{
      
      // Apply input volume
      for(int ch=0;ch<_num_inputs;ch++)
        SMOOTH_copy_sound(&_plugin->input_volume, input_sound[ch], dry_sound[ch], num_frames);
      
      // Fill output
      PLUGIN_RT_process(_plugin, time, num_frames, input_sound, _output_sound, process_plugins);      
    }

    
    // compressor
    RT_apply_system_filter(&_plugin->comp,      _output_sound, _num_outputs, num_frames, process_plugins);
    
    // filters
    RT_apply_system_filter(&_plugin->lowshelf,  _output_sound, _num_outputs, num_frames, process_plugins);
    RT_apply_system_filter(&_plugin->eq1,       _output_sound, _num_outputs, num_frames, process_plugins);
    RT_apply_system_filter(&_plugin->eq2,       _output_sound, _num_outputs, num_frames, process_plugins);
    RT_apply_system_filter(&_plugin->highshelf, _output_sound, _num_outputs, num_frames, process_plugins);
    RT_apply_system_filter(&_plugin->lowpass,   _output_sound, _num_outputs, num_frames, process_plugins);
    RT_apply_system_filter(&_plugin->highpass,  _output_sound, _num_outputs, num_frames, process_plugins);
    
    // dry/wet              
    RT_apply_dry_wet(_latency_dry_sound, _num_dry_sounds, _output_sound, _num_outputs, num_frames, &_plugin->drywet);

    
    // Output pan
    SMOOTH_apply_pan(&_plugin->pan, _output_sound, _num_outputs, num_frames);

    // Right channel delay ("width")
    if(_num_outputs>1)
      static_cast<radium::SmoothDelay*>(_plugin->delay)->RT_process(num_frames, _output_sound[1], _output_sound[1]);
    
    // Output peaks
    if (_num_outputs > 0) {
      float volume_peaks[_num_outputs];
      bool is_touched = false;
      
      for(int ch=0;ch<_num_outputs;ch++) {

        float out_peak = RT_get_max_val(_output_sound[ch],num_frames);
        
        if (!is_touched && out_peak > MIN_AUTOSUSPEND_PEAK)
          is_touched = true;

        volume_peaks[ch] = out_peak * _plugin->volume;
      }

      if(is_touched) {
        RT_PLUGIN_touch(_plugin);
        for(auto link : _output_links){
          if (false==link->is_event_link && link->is_active)
            RT_PLUGIN_touch(link->target->_plugin);
        }
      }
      
      RT_set_output_peak_values(volume_peaks);
    }
  }
};



SoundProducer *SP_create(SoundPlugin *plugin, Buses buses){
  return new SoundProducer(plugin, MIXER_get_buffer_size(), buses);
}

void SP_delete(SoundProducer *producer){
  printf("Deleting \"%s\"\n",producer->_plugin->type->type_name);
  delete producer;
}

int64_t SP_get_id(SoundProducer *producer){
  return producer->_id;
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


// Does NOT delete the bus links. Those are deleted in the SoundProducer destructor.
void SP_remove_all_links(radium::Vector<SoundProducer*> &soundproducers){

  radium::Vector<SoundProducerLink *> links_to_delete;
  
  // Find all non-bus links
  for(auto soundproducer : soundproducers)
    for(auto link : soundproducer->_input_links)
      if (link->is_bus_link==false)
        links_to_delete.push_back(link);
  
  SoundProducer::remove_links(links_to_delete);
}


// Called by main mixer thread before starting multicore.
void SP_RT_called_for_each_soundcard_block1(SoundProducer *producer, int64_t time){
  producer->RT_called_for_each_soundcard_block1(time);
}

void SP_RT_called_for_each_soundcard_block2(SoundProducer *producer, int64_t time){
  producer->RT_called_for_each_soundcard_block2(time);
}

void CpuUsage_delete(void *cpu_usage){
  delete (CpuUsage*)cpu_usage;
}

void SP_RT_process(SoundProducer *producer, int64_t time, int num_frames, bool process_plugins){
  SoundPlugin *plugin = producer->_plugin;
  
  bool is_visible = ATOMIC_GET(g_show_cpu_usage_in_mixer) || ATOMIC_GET(plugin->is_visible);
  CpuUsage *cpu_usage = (CpuUsage*)ATOMIC_GET(plugin->cpu_usage);
  
  bool add_cpu_data = is_visible && cpu_usage!=NULL;

  //double start_time;
  jack_time_t start_time = 0;
  
  if (add_cpu_data)
    //start_time = monotonic_rdtsc_seconds();
    start_time = jack_get_time();
    //start_time = monotonic_seconds(); // Checking if max time would fluctuate less with this timer. Didn't make a difference though. Both are probably using CLOCK_MONOTONIC.
  
  producer->RT_process(time, num_frames, process_plugins);

  if (add_cpu_data){
    //double end_time = monotonic_rdtsc_seconds();
    jack_time_t end_time = jack_get_time();
    //double end_time = monotonic_seconds();
          
    //float new_cpu_usage = (end_time-start_time) * 100.0 * MIXER_get_sample_rate() / (double)num_frames;
    float new_cpu_usage = (double)(end_time-start_time) * 0.0001 * MIXER_get_sample_rate() / num_frames;

    //printf("Adding cpu usage for %s\n",plugin->patch->name);
    cpu_usage->addUsage(new_cpu_usage);
  }
}

void SP_write_mixer_tree_to_disk(QFile *file){
  const radium::Vector<SoundProducer*> *sp_all = MIXER_get_all_SoundProducers();
  if (sp_all==NULL)
    return;
  
  int num=0;
  
  for (SoundProducer *sp : *sp_all){
    SoundPlugin *plugin = sp->_plugin;
    volatile Patch *patch = plugin==NULL ? NULL : plugin->patch;
    const char *name = patch==NULL ? "<null>" : patch->name;
    
    file->write(QString().sprintf("%d: sp: %p (%s). num_dep: %d, num_dep_left: %d: num_dependant: %d, bus provider: %d\n",num++,sp,name,sp->num_dependencies,ATOMIC_GET(sp->num_dependencies_left), sp->_output_links.size(), sp->_bus_descendant_type==IS_BUS_PROVIDER).toUtf8());
    
    for (SoundProducerLink *link : sp->_output_links){
      SoundPlugin *plugin = link->target->_plugin;
      volatile Patch *patch = plugin==NULL ? NULL : plugin->patch;
      const char *name = patch==NULL ? "<null>" : patch->name;
      file->write(QString().sprintf("  %s%s\n",name, link->is_active?"":" (inactive)").toUtf8());
    }    
  }
}

void SP_print_tree(void){
  int num=0;

  const radium::Vector<SoundProducer*> *sp_all = MIXER_get_all_SoundProducers();
  
  for (SoundProducer *sp : *sp_all){
    fprintf(stderr,"%d (%d): sp: %p (%s). num_dep: %d, num_dep_left: %d: num_dependant: %d, bus provider: %d.\n",
            num++,
            sp->_plugin->patch==NULL?-1:(int)sp->_plugin->patch->id,
            sp,
            sp->_plugin->patch==NULL?"<null>":sp->_plugin->patch->name,
            sp->num_dependencies,
            ATOMIC_GET(sp->num_dependencies_left),
            sp->_output_links.size(),
            sp->_bus_descendant_type==IS_BUS_PROVIDER
            
            );
    /*
    fprintf(stderr,"  inputs:\n");
    for (SoundProducerLink *link : sp->_input_links){
      fprintf(stderr, "    %s%s\n",link->source->_plugin->patch->name,link->is_active?"":" (inactive)");
    }
    fprintf(stderr, "  outputs:\n");
    */
    for (SoundProducerLink *link : sp->_output_links){
      fprintf(stderr, "  %s%s. Latency: %d\n",link->target->_plugin->patch->name,link->is_active?"":" (inactive)",link->_delay._delay.getSize());
    }    
  }
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


struct SoundPlugin *SP_get_plugin(SoundProducer *producer){
  return producer->_plugin;
}

int SP_get_bus_num(SoundProducer *sp){
  return sp->_bus_num;
}

enum BusDescendantType SP_get_bus_descendant_type(SoundProducer *sp){
  if(sp==NULL){
    RError("SP_get_bus_descendant_type: sp==NULL\n");
    return IS_BUS_PROVIDER;
  }
  return sp->_bus_descendant_type;
}

bool SP_replace_plugin(SoundPlugin *old_plugin, SoundPlugin *new_plugin){
  if (!PLAYER_current_thread_has_lock()) {
    RError("Current thread is not holding player lock");
    return false;
  }

  SoundProducer *sp = old_plugin->sp;
  if (sp==NULL) {
    RError("Could not find soundproducer for plugin");
    return false;
  }

  sp->_plugin = new_plugin;
  return true;  
}

bool SP_is_plugin_running(SoundPlugin *plugin){
  return plugin->sp != NULL;
}

int RT_SP_get_input_latency(SoundProducer *sp){
  return sp->_highest_input_link_latency;
}

bool SP_is_autosuspending(SoundProducer *sp){
  return ATOMIC_GET_RELAXED(sp->_is_autosuspending);
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

bool SP_has_input_links(SoundProducer *sp){
  return sp->_input_links.size() > 0;
}

void SP_called_regularly_by_main_thread(SoundProducer *sp){
  // Not enabled. Enable in Qt_Main.cpp.
}
