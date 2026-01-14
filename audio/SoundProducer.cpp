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



#include "../weakjack/weak_libjack.h"


#include <QFile>


//#include "pa_memorybarrier.h"


#include "../common/nsmtracker.h"

#include "monotonic_timer.c"


#include "../common/OS_Player_proc.h"
#include "../common/OS_visual_input.h"
#include "../common/threading.h"
#include "../common/visual_proc.h"
#include "../common/Vector.hpp"
#include "../common/QueueStack.hpp"
#include "../common/Time.hpp"

#include "../midi/midi_i_input_proc.h"

#include "SoundPlugin.h"
#include "SoundPlugin_proc.h"
#include "AudioMeterPeaks_proc.h"
#include "system_compressor_wrapper_proc.h"
#include "Juce_plugins_proc.h"

#include "SoundPluginRegistry_proc.h"
#include "Mixer_proc.h"
#include "MultiCore_proc.h"
#include "CpuUsage.hpp"
#include "SmoothDelay.hpp"
#include "AudioBuffer.hpp"
#include "Seqtrack_plugin_proc.h"

#include "../mixergui/QM_MixerWidget.h"

#include "fade_envelopes.h"


#include "SoundProducer_proc.h"

DEFINE_ATOMIC(bool, g_check_abnormal_signals) = true;

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


bool g_RT_enable_latency_compensation = true;
int64_t g_RT_system_out_input_latency = 0;

static bool g_has_solo = false;


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


static const float *g_empty_sound = NULL;
  
struct LatencyCompensatorDelay {
  radium::SmoothDelay _delay;

  LatencyCompensatorDelay()
    : _delay(ms_to_frames(MAX_COMPENSATED_LATENCY))
  {
    if (g_empty_sound==NULL){
		g_empty_sound = (float*)V_calloc(sizeof(float), RADIUM_BLOCKSIZE); // Note: RADIUM_BLOCKSIZE can not change value after program startup.
    }
  }
	
  ~LatencyCompensatorDelay(){
  }

  void RT_set_preferred_delay(int preferred_delay){
    _delay.setSize(preferred_delay);
  }

  bool RT_delay_line_is_empty(void) const {
    return _delay.RT_delay_line_is_empty();
  }
  
  // Should be called instead of RT_process if we don't need any sound.
  const float *RT_call_instead_of_process_if_no_sound(int num_frames, float *output_sound){
    R_ASSERT_NON_RELEASE(num_frames==RADIUM_BLOCKSIZE);

#if !defined(RELEASE)
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wfloat-equal"
    for(int i=0;i<num_frames;i++){
      if(g_empty_sound[i] != 0)
        abort();
    }
#pragma GCC diagnostic pop
#endif

    if (num_frames==64)
      return _delay.RT_call_instead_of_process_if_no_sound(64, g_empty_sound, output_sound);
    else
      return _delay.RT_call_instead_of_process_if_no_sound(num_frames, g_empty_sound, output_sound);
  }
  
  // May return 'input_sound'. Also, 'input_sound' is never modified.
  const float *RT_process(const float *input_sound, float *output_sound, int num_frames){
#if !defined(RELEASE)
    R_ASSERT_RETURN_IF_FALSE2(input_sound!=NULL, g_empty_sound);
    R_ASSERT_RETURN_IF_FALSE2(output_sound!=NULL, g_empty_sound);
    //R_ASSERT_RETURN_IF_FALSE2(_delay.fVec0!=NULL, g_empty_sound);
    R_ASSERT_RETURN_IF_FALSE2(num_frames==MIXER_get_buffer_size(), g_empty_sound);
#endif

    if (num_frames==64){
      if(_delay.RT_process(64, input_sound, output_sound))
        return output_sound;
      else
        return input_sound;
    } else {
      if(_delay.RT_process(num_frames, input_sound, output_sound))
        return output_sound;
      else
        return input_sound;
    }
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
  int _bus_num;
  bool _is_bus_link;
  
  // fields below only used by audio links
  int source_ch;
  int target_ch;

  bool is_enabled = true; // Only accessed by the main thread when applying changes, and in SP_set_link_enabled.

  bool RT_is_enabled = true;  // Contains the same value as "is_enabled", but can only be accessed if holding the player lock.

  bool is_explicitly_enabled = true; // Set by the user.
  
  bool is_implicitly_muted = false; // is muted because of Solo configuration in the audio graph.
  bool is_implicitly_soloed = false; // is soloed either because source or target is solo, or a soundproducer that is in direct line from source or target is solo.

  bool _last_buffer_was_silent = true;
  
  Smooth volume; // volume.target_value = link_volume * source->output_volume * source->volume

private:
  
  float RT_link_volume;  // Contains the same value as "link_volume", but can only be accessed if holding the player lock.
  float link_volume;  // Set by the user. Only accessed by the main thread.
  
public:
  
  bool RT_is_active; // this is an internal variable used for whether the link should run or not. It's not the same as "is_enabled" above (naturally).

  // Can be called when both is_explicitly_enabled and is_implicitly_muted is updated.
  bool should_be_enabled(void) const {
    return is_explicitly_enabled && !is_implicitly_muted;
  }

  bool equals(const SoundProducerLink *b) const {
    if (source!=b->source)
      return false;
    
    if (target!=b->target)
      return false;

    if (is_event_link!=b->is_event_link)
      return false;
    
    if (is_event_link)
      return true;

    if (source_ch!=b->source_ch)
      return false;

    if (target_ch!=b->target_ch)
      return false;

    return true;
  }

  static bool equal(SoundProducerLink *const &a, SoundProducerLink *const &b){
    return a->equals(b);
  }

  bool set_link_volume(float volume){
    if (_is_bus_link){
      
      if (source_ch==0 && target_ch==0){

        SoundPlugin *plugin = SP_get_plugin(source);
#if 0 //!defined(RELEASE)
        if(plugin==NULL){ // silence "null-dereference" warning.
          abort();
          return false;
        }
#endif
        const SoundPluginType *type = plugin->type;
        int effect_num = type->num_effects + EFFNUM_BUS1 + _bus_num;
        
        //printf("Setting bus volume for %d to %f\n", bus_num, volume);
        
        PLUGIN_set_effect_value(plugin, -1, effect_num, volume, STORE_VALUE, FX_single, EFFECT_FORMAT_NATIVE);

        return true;
      }

    } else {

      // Assert that we don't set link volume of a sink target (e.g. system out or jack out) to anything other than 1.0. The reason is that there isn't an interface in the mixer to set volume for links to sinks, so it must always stay at 1.0 / 0dB.
      {
        const SoundPlugin *plugin = SP_get_plugin(target);
        if (plugin!=NULL && plugin->type->num_outputs==0){
          if (!equal_floats(volume, 1.0f)){
            printf("Warning! Trying to set volume of a sink-link. To avoid unintentional muted sink-links, a sink-link is always set to have volume 1.0. Now trying to set it to %f\n", volume);
            R_ASSERT_NON_RELEASE(false);
            return false;
          }
        }
      }
      
      if (!equal_floats(link_volume, volume)){
        
        link_volume = volume;
        safe_float_write(&RT_link_volume, volume);

        return true;
      }
      
    }

    return false;
  }

  float get_link_volume(void) const {
    ASSERT_IS_NONRT_MAIN_THREAD_NON_RELEASE();
    
    if (_is_bus_link){
      
      SoundPlugin *plugin = SP_get_plugin(source);
#if !defined(RELEASE)
      if(plugin==NULL){ // silence "null-dereference" warning.
        abort();
        return 0;
      }
#endif

      const SoundPluginType *type = plugin->type;      
      int effect_num = type->num_effects + EFFNUM_BUS1 + _bus_num;
      
      //printf("Setting bus volume for %d to %f\n", bus_num, volume);
      
      return PLUGIN_get_effect_value2(plugin, effect_num, VALUE_FROM_STORAGE, EFFECT_FORMAT_NATIVE); // From storage, so we can't just return plugin->bus_volume[_bus_num]. Reading from storage is extremely efficient though, so it doesn't matter.

    } else {

      return link_volume;
      
    }
  }
    
  bool need_to_create_volume_change(float new_volume) const {
    ASSERT_IS_NONRT_MAIN_THREAD_NON_RELEASE();

    if (_is_bus_link)
      if (source_ch > 0 || target_ch > 0)
        return false;

    return !equal_floats(get_link_volume(), new_volume);
  }
  
  void RT_request_turn_off(void){
    RT_is_enabled = false;
  }

  float RT_get_total_link_volume(void) const {
    if (!RT_is_enabled)
      return 0.0f;
    
    const SoundPlugin *source_plugin = SP_get_plugin(source);
    
    if (_is_bus_link){

      if (root->song->mute_system_buses_when_bypassed && is_bypassed(source_plugin))
        return 0.0f;
        
      return source_plugin->bus_volume[_bus_num];
      
    } else {
    
      if (ATOMIC_GET(source_plugin->output_volume_is_on))
        return source_plugin->output_volume * RT_link_volume;
      else
        return 0.0f;

    }
  }


  // Called by main mixer thread before starting multicore.
  void RT_called_for_each_soundcard_block(void){
    //R_ASSERT(THREADING_is_player_thread());

    if (is_event_link) {

      R_ASSERT_NON_RELEASE(RT_is_active==true);

    } else {

      float new_volume = RT_get_total_link_volume();

      //if (!strcmp(SP_get_plugin(source)->patch->name, "Pipe 1") && source_ch==0 && target_ch==0)
      //  printf("     Vol: %f\n", new_volume);
      
      if (equal_floats(new_volume, 0.0f) && _last_buffer_was_silent)
        SMOOTH_force_target_value(&volume, 0.0f);
      else
        SMOOTH_set_target_value(&volume, new_volume);
      
      SMOOTH_update_target_audio_will_be_modified_value(&volume);

      RT_is_active = volume.target_audio_will_be_modified;
    }
    
  }

  bool can_be_removed(void) const {
    if (is_event_link)
      return true;

    if(safe_bool_read(&RT_is_active))
      return false;
      
    bool ret = false;

    PLAYER_lock();{
      if(RT_is_active==false)
        ret = true;
    }PLAYER_unlock();
    
    return ret;
  }
  
  SoundProducerLink(SoundProducer *source, SoundProducer *target, bool is_event_link)
    : source(source)
    , target(target)
    , is_event_link(is_event_link)
    , _bus_num(SP_get_bus_num(target))
    , _is_bus_link(_bus_num >= 0)
    , source_ch(0)
    , target_ch(0)
    , RT_link_volume(1.0)
    , link_volume(1.0)
    , RT_is_active(is_event_link)
  {
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

#if 0
static float RT_get_max_val2(const float *array, const int num_elements){
  return 1.0;
}
#endif

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
static void RT_copy_sound_and_apply_volume(float *to_sound, const float *from_sound, int num_frames, float start_volume, float end_volume){
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


static void RT_apply_dry_wet(const float **__restrict__ dry, int num_dry_channels,
                             float **__restrict__ wet, int num_wet_channels,
                             int num_frames,
                             const Smooth *__restrict__ wet_values){
  //int num_channels = num_wet_channels; //std::min(num_dry_channels,num_wet_channels);
  for(int ch=0;ch<num_wet_channels;ch++){
    float       *__restrict__ w = wet[ch];

    SMOOTH_apply_volume(wet_values, w, num_frames);

    if (ch < num_dry_channels)
      SMOOTH_mix_sounds_using_inverted_values(wet_values, w, dry[ch], num_frames);
  }
}

static void RT_fade_in2(float *__restrict__ sound, int pos, int num_frames){
  if (pos + num_frames > FADE_LEN)
    num_frames = FADE_LEN-pos;
      
#if 1

  float mul = scale(pos+1, 0, FADE_LEN+1, 0.0, 1.0);
  float mulinc = scale(1, 0, FADE_LEN+1, 0.0, 1.0);

#if 0
  if(num_frames==64)
    for(int i=0;i<64;i++){
      sound[i] *= mul;
      mul += mulinc;
    }
  else
    for(int i=0;i<num_frames;i++){
      sound[i] *= mul;
      mul += mulinc;
    }
#else
  if(num_frames==64)
    for(int i=0;i<64;i++)
      sound[i] *= mul + i*mulinc;
  else
    for(int i=0;i<num_frames;i++)
      sound[i] *= mul + i*mulinc;
#endif
  
  
#else
  R_ASSERT_NON_RELEASE(num_frames+pos < FADE_LEN);
  
  for(int i=0;i<num_frames;i++)
    sound[i] *= fade_in_envelope[i+pos];
#endif
  
}


static void RT_fade_out2(float *__restrict__ sound, int pos, int num_frames){
  int frames_to_iterate = num_frames;
  
  if (pos + num_frames > FADE_LEN) {

    frames_to_iterate = FADE_LEN-pos;

    R_ASSERT_NON_RELEASE(frames_to_iterate < num_frames);
    
    if (frames_to_iterate <= 0){
      memset(sound, 0, sizeof(float)*num_frames);
      return;
    }

    memset(sound + frames_to_iterate, 0, sizeof(float)*(num_frames-frames_to_iterate));    
  }
    
#if 1

  float mul = scale(pos+1, 0, FADE_LEN+1, 1.0, 0.0);
  float mulinc = -1.0 * scale(1, 0, FADE_LEN+1, 0.0, 1.0);

#if 0
  if(frames_to_iterate==64)
    for(int i=0;i<64;i++){
      sound[i] *= mul;
      mul += mulinc;
    }
  else
    for(int i=0;i<frames_to_iterate;i++){
      sound[i] *= mul;
      mul += mulinc;
    }
#else
  if(frames_to_iterate==64)
    for(int i=0;i<64;i++)
      sound[i] *= mul + mulinc*i;
  else
    for(int i=0;i<frames_to_iterate;i++)
      sound[i] *= mul + mulinc*i;
#endif
  
#else
  
  R_ASSERT_NON_RELEASE(num_frames+pos < FADE_LEN);
  
  for(int i=0;i<num_frames;i++)
    sound[i] *= fade_out_envelope[i+pos];
  
#endif
}

static const char *RT_check_abnormal_signal(const SoundPlugin *plugin, int num_frames, float **__restrict__ outputs){
  R_ASSERT_NON_RELEASE(num_frames==RADIUM_BLOCKSIZE);

  if (!ATOMIC_GET_RELAXED(g_check_abnormal_signals))
    return NULL;
  
  const int num_channels = plugin->type->num_outputs;
  float sum=0.0f;
  
  for(int ch=0;ch<num_channels;ch++) {
    const float *out = outputs[ch];
    float sum2 = 0.0f;

    if(num_frames==64)
      for(int i=0;i<64;i++)
        sum2 += out[i];
    else
      for(int i=0;i<num_frames;i++)
        sum2 += out[i];
      
    sum += sum2;
  }

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wfloat-equal"
  if(sum!=0.0f && !std::isnormal(sum) )
    return isnan(sum)?"nan":isinf(sum)?"inf":std::fpclassify(sum)==FP_SUBNORMAL?"denormal":"<something else\?\?\?>";
  else
    return NULL;
#pragma GCC diagnostic pop
}

static void show_high_peak_message(const SoundPlugin *plugin, const char *what_did_it_do_questionmark, const float *out, int ch, float peak){
  volatile struct Patch *patch = plugin->patch;
  RT_message("Warning!\n"
             "\n"
             "The instrument named \"%s\" of type %s/%s has %s a signal of at least 50dB in channel %d. (total number of channels: %d)\n"
             "\n"
             "Raw peak value: %f.\n"
             "\n"
             "10 first frames: %f %f %f %f %f %f %f %f %f %f."
             "\n"
             "This warning will pop up as long as the instrument does so.\n",
             
             patch==NULL?"<no name>":patch->name,
             plugin->type->type_name, plugin->type->name,             
             what_did_it_do_questionmark,
             ch,plugin->type->num_outputs,
             
             peak,
             out[0],out[1],out[2],out[3],out[4],out[5],out[6],out[7],out[8],out[9]
             );
}

static void PLUGIN_RT_process(SoundPlugin *plugin, int64_t time, int num_frames, float **inputs, float **outputs, bool process_plugins){

  if (process_plugins == false){
    
    for(int ch=0;ch<plugin->type->num_outputs;ch++)
      memset(outputs[ch], 0, sizeof(float)*num_frames);
    
  } else {

#if !defined(RELEASE)
    // assert that inputs and outputs are not changed by the plugin. (is there any way to mark the first layer of a 2d array as const, but not the second layer)
	  float **inputs_check = RT_ALLOC_ARRAY_STACK(float*, R_MAX(1, plugin->type->num_inputs));
	  float **outputs_check = RT_ALLOC_ARRAY_STACK(float*, R_MAX(1, plugin->type->num_outputs));
	  for(int i=0;i<plugin->type->num_inputs;i++)
		  inputs_check[i]=inputs[i];
	  for(int i=0;i<plugin->type->num_outputs;i++)
		  outputs_check[i]=outputs[i];
#endif

    plugin->type->RT_process(plugin, time, num_frames, inputs, outputs);

#if !defined(RELEASE)
    // assert that inputs and outputs are not changed by the plugin.
    for(int i=0;i<plugin->type->num_inputs;i++)
      R_ASSERT(inputs_check[i]==inputs[i]);
    for(int i=0;i<plugin->type->num_outputs;i++)
      R_ASSERT(outputs_check[i]==outputs[i]);
#endif

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

        if (peak > 317)
          show_high_peak_message(plugin, "generated", out, ch, peak);
      }
    }

  }

}

static int id_counter = 0;

namespace{
  struct Owner;
}

namespace{
  struct VolumeChange{
    SoundProducerLink *link;
    float new_volume;
    VolumeChange(SoundProducerLink *link, float new_volume)
      : link(link)
      , new_volume(new_volume)
    {}
  };
  struct VolumeChanges : public radium::Vector<VolumeChange> {
    VolumeChanges(){
    }
  };
  
  const VolumeChanges g_empty_volume_changes;

  struct LinkEnabledChange{
    SoundProducerLink *link;
    bool link_enabled;
    LinkEnabledChange(SoundProducerLink *link, bool link_enabled)
      : link(link)
      , link_enabled(link_enabled)
    {}
  };
  struct LinkEnabledChanges : public radium::Vector<LinkEnabledChange> {
    LinkEnabledChanges(){
    }
  };
  
  struct LinkExplicitlyEnabledChanges : public radium::Vector<LinkEnabledChange> {
    LinkExplicitlyEnabledChanges(){
    }
  };
  
  const LinkExplicitlyEnabledChanges g_empty_link_explicitly_enabled_changes;

  struct PluginImplicitlyMutedChange{
    SoundPlugin *_plugin;
    bool _is_implicitly_muted;
    PluginImplicitlyMutedChange(SoundPlugin *plugin, bool is_implicitly_muted)
      : _plugin(plugin)
      , _is_implicitly_muted(is_implicitly_muted)
    {}
  };
  
  struct PluginImplicitlyMutedChanges : public radium::Vector<PluginImplicitlyMutedChange> {
    PluginImplicitlyMutedChanges(){
    }
  };
  
}

//struct Owner *owner;

struct SoundProducer {
  /*
  SoundProducer *next; // Used by the MultiCore scheduler. (soundproducers ready to run are stored in a linked list)
  int cpunum = 0;
  */

  int64_t _id;

  SoundPlugin *_plugin;
  int _latency;
  int _highest_input_link_latency;
  
  int _num_inputs;
  int _num_outputs;
  int _num_dry_sounds;

  bool *_curr_output_is_silent;
  
  int64_t _last_time;

  double running_time;
  bool has_run_for_each_block2;

  bool _is_bus;
  int _bus_num;

  LatencyCompensatorDelay *_dry_sound_latencycompensator_delays;
  
  //radium::AudioBuffer _output_buffer;
  //radium::AudioBuffer **_input_buffers;
  //radium::MultiThreadAccessAudioBuffers _input_buffers;
  radium::MultiThreadAccessArray<radium::AudioBuffer, MAX_NUM_CPUS> _input_buffers;
  
  // Scheduling, start
  //
#if !defined(RELEASE)
  DEFINE_ATOMIC(bool, is_processed) = false;
#endif

  DEFINE_ATOMIC(int, _num_active_input_links_left);  // Decreased during processing. When the number is zero, it is scheduled for processing. 
  int _num_active_input_links;

#if 0
  int downcounter = 1; // When zero, the soundproducer can change order. We do this to avoid too much fluctation, which is likely to be bad for the cache.

  //Owner *owner = NULL;
  SoundProducer *dll_prev;
  SoundProducer *dll_next;
  //
  // Scheduling, end
#endif
  
  
  radium::Vector<SoundProducerLink*> _input_links;
  radium::Vector<SoundProducerLink*> _output_links; // Used by MultiCore.

  SoundProducer(const SoundProducer&) = delete;
  SoundProducer& operator=(const SoundProducer&) = delete;

public:
  
  SoundProducer(SoundPlugin *plugin, int num_frames, Buses buses)
    : _id(id_counter++)
    , _plugin(plugin)
    , _latency(0)
    , _highest_input_link_latency(0)
    , _num_inputs(plugin->type->num_inputs)
    , _num_outputs(plugin->type->num_outputs)
    , _curr_output_is_silent(new bool[_num_outputs])
    , _last_time(-1)
    , running_time(0.0)
      //, _output_buffer(_num_outputs)
    , _input_buffers([this](int buffer_num){return new radium::AudioBuffer(_num_inputs);})
    , _num_active_input_links(0)
  {    
    printf("New SoundProducer. Inputs: %d, Ouptuts: %d. plugin->type->name: %s\n",_num_inputs,_num_outputs,plugin->type->name);

    plugin->sp = this;
    
    ATOMIC_SET(_num_active_input_links_left, 0);
    
    R_ASSERT(THREADING_is_main_thread());
    
    _bus_num = PLUGIN_get_bus_num(plugin->type);
    _is_bus = _bus_num >= 0;

#if !defined(RELEASE)
    if (_is_bus){
      //fprintf(stderr,"type_name: -%s-\n", plugin->type->type_name);
      if (strcmp(plugin->type->type_name, "Bus"))
        abort();
      if (!strcmp(plugin->type->name, "Bus 1") ||          
          !strcmp(plugin->type->name, "Bus 2") ||
          !strcmp(plugin->type->name, "Bus 3") ||
          !strcmp(plugin->type->name, "Bus 4") ||
          !strcmp(plugin->type->name, "Bus 5")
          )
        {
        }else{
          abort();
        }

      if (plugin->patch->id.id==0)
        abort();
    }
#endif
    
    if (_is_bus) {
      /*
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
      */
    }else{
      R_ASSERT(buses.bus1!=NULL);
      R_ASSERT(buses.bus2!=NULL);
      R_ASSERT(buses.bus3!=NULL);
      R_ASSERT(buses.bus4!=NULL);
      R_ASSERT(buses.bus5!=NULL);
    }

    if(_num_inputs>0)
      _num_dry_sounds = _num_inputs;
    else
      _num_dry_sounds = _num_outputs;

    for(int ch=0;ch<_num_outputs;ch++)
      _curr_output_is_silent[ch] = true;
      
    _dry_sound_latencycompensator_delays = new LatencyCompensatorDelay[_num_dry_sounds];
    
    MIXER_add_SoundProducer(this);

#if !defined(RELEASE)
    printf("*** Finished... New SoundProducer. Inputs: %d, Ouptuts: %d. plugin->type->name: %s\n",_num_inputs,_num_outputs,plugin->type->name);
    //getchar();
#endif
  }

  // Called from SP_delete
  ~SoundProducer(){
    R_ASSERT(THREADING_is_main_thread());

    R_ASSERT(_input_links.size()==0);
    R_ASSERT(_output_links.size()==0);

    MIXER_remove_SoundProducer(this);

    _plugin->sp = NULL;

    delete[] _curr_output_is_silent;
    delete[] _dry_sound_latencycompensator_delays;
  }
  
  static bool is_audio_connected(const SoundProducer *start_producer, const SoundProducer *end_producer, int &safety) {
    safety++;
    
    if (safety > 10000){
      R_ASSERT(false);
      return false;
    }
    
    for (SoundProducerLink *link : end_producer->_input_links){
      if (link->is_event_link)
        continue;
      else if (link->source == start_producer)
        return true;
      else if (is_audio_connected(start_producer, link->source, safety))
        return true;
    }

    return false;
  }
  
  // Traverse graph backwards and see if we end up in the same position as we started.
  bool is_recursive(const SoundProducer *start_producer) const {
    if(start_producer==this)
      return true;

    for (SoundProducerLink *link : _input_links)
      if(link->source->is_recursive(start_producer)==true)
        return true;

    return false;
  }

#if defined(RELEASE)
#define D(n)
#else
#define D(n) n
#endif

#define P(n) D(printf("        %d: visited.size(): %d, link: %p, to_add.size(): %d, to_remove.size(): %d, remove_first.size(): %d\n", n, (int)visited.size(), link, (int)to_add.size(), (int)to_remove.size(), (int)links_that_must_be_removed_first.size()));
  
  // Traverse graph backwards and see if we end up a place we have been before.
  bool is_recursive(const radium::Vector<SoundProducerLink*> &to_add,
                    radium::Vector<SoundProducerLink*> &to_remove,
                    radium::Vector<SoundProducerLink*> &links_that_must_be_removed_first,
                    QSet<const SoundProducer*> visited,
                    QSet<const SoundProducer*> &already_checked
                    )
  {
                    
    if (already_checked.contains(this))
      return false;

    if(visited.contains(this)){
      D(const SoundProducerLink *link = NULL;)
      P(0);
      return true;
    }

    visited.insert(this);
    
    int num_input_links = _input_links.size();
    
    for(int pos = 0 ; pos < num_input_links + to_add.size() ; pos++){
      SoundProducerLink *link;

      if (pos < num_input_links)
        link = _input_links.at(pos);      
      else
        link = to_add.at(pos-num_input_links);

      D(printf("     B. Check %s(%d) -> %s(%d)\n",
               link->source->_plugin->patch->name, link->source_ch,
               link->target->_plugin->patch->name, link->target_ch));

      bool is_recursive = true
        && link->target == this
        && !links_that_must_be_removed_first.contains(link)
        && link->source->is_recursive(to_add, to_remove, links_that_must_be_removed_first, visited, already_checked);

      if (is_recursive) {
        
        if (to_remove.contains(link)){
                
          // transfer link from to_remove -> links_that_must_be_removed_first
          links_that_must_be_removed_first.push_back(link);
          to_remove.remove(link);
          
        } else {
          
          P(2);
          return true;
          
        }

      }
    }

    already_checked.insert(this);

    return false;
  }
  
  // The actual links to to_remove are not removed until after this function has returned.
  // In step 3 in 'add_and_remove_links', we are just requesting the engine to make it safe to remove the links, not actually remove the links.
  // (this way we can fade out / fade in several links simultaneously.)
  //
  static bool is_recursive(const radium::Vector<SoundProducerLink*> &to_add,
                           radium::Vector<SoundProducerLink*> &to_remove,
                           radium::Vector<SoundProducerLink*> &links_that_must_be_removed_first
                           )
  {
    D(printf("\n\n IS_RECURSIVE. to_add.size(): %d, to_remove.size(): %d, remove_first.size(): %d\n", to_add.size(), to_remove.size(), links_that_must_be_removed_first.size()));

    QSet<const SoundProducer*> already_checked; // Not strictly necessary. Only used to speed up.

    for(const auto *link : to_add){
      D(printf("   C. Check %s -> %s\n", link->source->_plugin->patch->name, link->target->_plugin->patch->name););

      QSet<const SoundProducer*> visited;
        
      if (link->target->is_recursive(to_add, to_remove, links_that_must_be_removed_first, visited, already_checked)==true){
        P(3);
        return true;
      }          
    }

    return false;
  }
  
#undef P


  struct UpdateImplicitSolo{
    const radium::Vector<SoundProducerLink*> &_to_add;
    const radium::Vector<SoundProducerLink*> &_to_remove;
    const QVector<radium::SoloChange> _solo_changes;
    QHash<SoundPlugin*, bool> _solo_changes_hash;
    
    mutable QSet<SoundProducer*> _updated_sps[2]; // to avoid updating the same soundproducer more than once
    const bool _implicitly_mute_event_links = root->song->RT_implicitly_mute_plugin_MIDI;
    LinkEnabledChanges &_link_enabled_changes;
    PluginImplicitlyMutedChanges &_plugin_implicitly_muted_changes;
    
    UpdateImplicitSolo(const radium::Vector<SoundProducerLink*> &to_add,
                       const radium::Vector<SoundProducerLink*> &to_remove,
                       radium::SoloChanges &solo_changes,
                       LinkEnabledChanges &link_enabled_changes,
                       PluginImplicitlyMutedChanges &plugin_implicitly_muted_changes
                       )
      : _to_add(to_add)
      , _to_remove(to_remove)
      , _solo_changes(solo_changes.take_over())
      , _link_enabled_changes(link_enabled_changes)
      , _plugin_implicitly_muted_changes(plugin_implicitly_muted_changes)
    {
      //printf("SOLO changes: %d / %d\n", _solo_changes.size(), solo_changes.size()); // problemet er at solo-status ikke blir lagra i forrige ab når man switcher.
      ///R_ASSERT_NON_RELEASE(_solo_changes.size()==solo_changes.size());
      
      for(auto &change : _solo_changes)
        _solo_changes_hash[change._plugin] = change._solo_is_on;
      
      process();
#if 0 //!defined(RELEASE)
      static int num=0;
      printf("---------------UpdateImplicitSolo #%d. Size: %d\n", num++, _link_enabled_changes.size());
#endif

      // Update various solo and mute buttons.
      if (_plugin_implicitly_muted_changes.size() > 0 || _solo_changes.size() > 0 || _link_enabled_changes.size()>0){
        THREADING_run_on_main_thread_async([]
           {
             MW_update_all_chips();
             redrawMixerStrips(true);
             GFX_update_current_instrument_widget();
           },
           true);
      }

    }
    
    void maybe_add_link_enabled_change(SoundProducerLink *link){
      R_ASSERT_NON_RELEASE(_implicitly_mute_event_links || !link->is_event_link);

      if (link->is_event_link)
        return; // RT_is_enabled is not used for event links. (it's just used to set volume)
      
      bool should_be_enabled = link->should_be_enabled();
      
      if (link->is_enabled != should_be_enabled)
        _link_enabled_changes.push_back(LinkEnabledChange(link, should_be_enabled));
    }

    void update_is_implicitly_soloed_link(SoundProducerLink* link, bool link_forward) const {
      if (!_implicitly_mute_event_links && link->is_event_link)
        return;
      
      link->is_implicitly_soloed = true;
      update_is_implicitly_soloed_sp(link_forward ? link->target : link->source, link_forward);
    }
    
    void update_is_implicitly_soloed_sp(SoundProducer *sp, bool link_forward) const {
      R_ASSERT_RETURN_IF_FALSE(sp!=NULL);
      
      if (!_updated_sps[link_forward?0:1].contains(sp)) {
        
        _updated_sps[link_forward?0:1].insert(sp);
        
        sp->_plugin->is_implicitly_soloed = true;
        
        for(auto *link : link_forward ? sp->_output_links : sp->_input_links)
          if (!_to_remove.contains(link))
            update_is_implicitly_soloed_link(link, link_forward);
        
        for(SoundProducerLink *link : _to_add)
          if (link_forward ? (sp==link->source) : (sp==link->target)){
            update_is_implicitly_soloed_link(link, link_forward);
          }
      }
    }
  
    void process(void){
      
      const radium::Vector<SoundProducer*> &sp_all = MIXER_get_all_SoundProducers();
      
      bool has_solo = false;

      // 0. Check if we can do a quick exit.
      if (!g_has_solo && _solo_changes.size()==0){
        for(SoundProducer *sp : sp_all)
          if (ATOMIC_GET_RELAXED(sp->_plugin->solo_is_on))
            goto we_have_or_had_solo;

        return; // We usually exits here.
      }
      
    
    we_have_or_had_solo:
      
      // 1. Set "is_implicitly_soloed" to false for all links
      //
      for(SoundProducer *sp : sp_all){

        // update plugin->is_implicitly* stuff here as well. (only used by the UI)
        sp->_plugin->is_implicitly_soloed = false;
        
        for(SoundProducerLink *link : sp->_output_links)
          link->is_implicitly_soloed = false;
        
#if !defined(RELEASE)
        for(SoundProducerLink *link : _to_add)
          if (link->is_implicitly_soloed==true)
            abort();
#endif
      }
      
      
      // 2. Set "is_implicitly_soloed" to correct value for all links and plugins
      //
      auto updateit = [&has_solo, this](SoundProducer *sp){
        has_solo = true;
        update_is_implicitly_soloed_sp(sp, true);
        update_is_implicitly_soloed_sp(sp, false);
      };
      
      for(SoundProducer *sp : sp_all)
        if (ATOMIC_GET_RELAXED(sp->_plugin->solo_is_on)){
          if (!_solo_changes_hash.contains(sp->_plugin) || _solo_changes_hash[sp->_plugin]==true){
            updateit(sp);
          }
        }

      for(const auto &change : _solo_changes){
        if (change._solo_is_on){
          if (change._plugin->sp==NULL){
            R_ASSERT_NON_RELEASE(false);
          } else {
            updateit(change._plugin->sp);
          }
        }
      }
    
      // 3. Set "is_implicilty_muted" to correct value for all existing links and plugins, and add LinkEnabledChanges to link_enabled_changes.
      //
      for(SoundProducer *sp : sp_all){
                
        bool is_implicitly_muted = has_solo && !sp->_plugin->is_implicitly_soloed;
        
        // update plugin->is_implicitly* stuff here as well. (only used by the UI)
        if (sp->_plugin->is_implicitly_muted != is_implicitly_muted){
          sp->_plugin->is_implicitly_muted = is_implicitly_muted;
          _plugin_implicitly_muted_changes.push_back(PluginImplicitlyMutedChange(sp->_plugin, is_implicitly_muted));
        }
        
        for(SoundProducerLink *link : sp->_output_links)
          if (_implicitly_mute_event_links || !link->is_event_link) {
            if (is_implicitly_muted != link->is_implicitly_muted){
              link->is_implicitly_muted = is_implicitly_muted;
              maybe_add_link_enabled_change(link);
            }
          }
      }

      // 4. Set "RT_is_enabled", "is_enabled", and "is_implicitly_muted" for new links.
      // At this point, only "is_implicitly_soloed" is known to have the correct value.
      // ("RT_is_enabled"/"is_enabled" is true by default, and "is_implicitly_muted" is false by default.)
      //
      for(SoundProducerLink *link : _to_add){
        if (!link->is_event_link) {

          if (has_solo && !link->is_implicitly_soloed)
            link->is_implicitly_muted = true;
          
          if (!link->should_be_enabled()){
            link->is_enabled = false;
            link->RT_is_enabled = false; // We can write directly since the link is not alive yet.
          }

        }
      }
      
      g_has_solo = has_solo;
    }

    void schedule_RT_funcs(radium::Scheduled_RT_functions &rt_functions){

      // Enable/disable
      //
      for(const auto &link_enabled_change : _link_enabled_changes){
        auto *link = link_enabled_change.link;
        bool enabled = link_enabled_change.link_enabled;
        
        link->is_enabled = enabled;
        
        rt_functions.add([link, enabled]
          {
            link->RT_is_enabled = enabled;
          });
      }

      
      // Solo
      for(const auto change : _solo_changes){
        SoundPlugin *plugin = change._plugin;
        bool solo_is_on = change._solo_is_on;
        rt_functions.add([plugin, solo_is_on]
          {
            //printf(".....555. RT. Setting %s solo to %d\n", plugin->patch->name, solo_is_on);
            PLUGIN_set_soloed(plugin, solo_is_on, false);
          });
      }

      
      // Change implicitly muted
      for(const auto change : _plugin_implicitly_muted_changes){
        SoundPlugin *plugin = change._plugin;
        bool is_implicitly_muted = change._is_implicitly_muted;
        rt_functions.add([plugin, is_implicitly_muted]
          {
            //printf(".....RT. %s. RT_is_implicitly_muted: %d\n", plugin->patch->name, is_implicitly_muted);        
            plugin->RT_is_implicitly_muted = is_implicitly_muted;
          });
      }
      

    }
  };


  static bool call_me_after_solo_has_changed(void){
    LinkEnabledChanges link_enabled_changes;
    PluginImplicitlyMutedChanges plugin_implicitly_muted_changes;
    
    const radium::Vector<SoundProducerLink*> empty;

    R_ASSERT(g_empty_solochanges.size()==0);

    UpdateImplicitSolo implicit(empty, empty, g_empty_solochanges, link_enabled_changes, plugin_implicitly_muted_changes);

    R_ASSERT(g_empty_solochanges.size()==0);
    
    if (link_enabled_changes.size()==0 && plugin_implicitly_muted_changes.size()==0)
      return false;

    radium::Scheduled_RT_functions rt_functions;
      
    implicit.schedule_RT_funcs(rt_functions);
    
    // Run it.
    D(printf("* handle_solo_changes: Waiting\n"));
    rt_functions.schedule_to_run_on_player_thread_and_wait();

    //MW_update_all_chips();
    
    return true;
  }

  static void handle_solo_changes(radium::SoloChanges &solo_changes){
    if (solo_changes.size()==0)
      return;

    LinkEnabledChanges link_enabled_changes;
    PluginImplicitlyMutedChanges plugin_implicitly_muted_changes;
    const radium::Vector<SoundProducerLink*> empty;
    UpdateImplicitSolo implicit(empty, empty, solo_changes, link_enabled_changes, plugin_implicitly_muted_changes);

    implicit.schedule_RT_funcs(solo_changes._rt_functions);
    
    // Run it.
    D(printf("* handle_solo_changes: Waiting\n"));
    solo_changes._rt_functions.schedule_to_run_on_player_thread_and_wait();
  }

  // Note 1: Will delete all links in "to_add" if it succeeded.
  // Note 2: Might modify to_remove.
  //
  static bool add_and_remove_links(const radium::Vector<SoundProducerLink*> &to_add,
                                   radium::Vector<SoundProducerLink*> &to_remove,
                                   radium::Scheduled_RT_functions &rt_functions,
                                   bool *isrecursive_feedback = NULL,
                                   const VolumeChanges &volume_changes = g_empty_volume_changes,
                                   const LinkExplicitlyEnabledChanges &link_explicitly_enabled_changes = g_empty_link_explicitly_enabled_changes,
                                   radium::SoloChanges &solo_changes = g_empty_solochanges
                                   )
  {
    
    R_ASSERT_RETURN_IF_FALSE2(THREADING_is_main_thread(), false);

    R_ASSERT_RETURN_IF_FALSE2(to_add.only_unique_elements(SoundProducerLink::equal), false);
    R_ASSERT_RETURN_IF_FALSE2(to_remove.only_unique_elements(SoundProducerLink::equal), false);
    R_ASSERT_RETURN_IF_FALSE2(!to_add.intersects(to_remove, SoundProducerLink::equal), false);


    //printf("SoundProducer::add_and_remove_links. Num to add: %d. Num to remove: %d. num volume changes: %d. Num link enabled changes: %d\n", to_add.size(), to_remove.size(), volume_changes.size(), link_enabled_changes.size());
    
    radium::Vector<SoundProducerLink*> links_that_must_be_removed_first; // Sometimes it is not possible to add and remove everything in one go without creating a recursive graph.

    
    // 1. ADDING/REMOVING: Check if graph will be recursive
    //
    bool isrecursive = SoundProducer::is_recursive(to_add, to_remove, links_that_must_be_removed_first);
    if (isrecursive_feedback != NULL)
      *isrecursive_feedback = isrecursive;

    if (to_add.size()==0 && to_remove.size()==0 && volume_changes.size()==0 && link_explicitly_enabled_changes.size()==0){
      R_ASSERT(links_that_must_be_removed_first.size()==0);
      return false;
    }

    R_ASSERT(!to_remove.intersects(links_that_must_be_removed_first, SoundProducerLink::equal));
        
    if (isrecursive) {

      R_ASSERT_NON_RELEASE(isrecursive_feedback != NULL);
      
      GFX_Message(NULL, "Recursive graphs are not automatically supported. Use the Send and Receive objects to create a recursive connection.\n");

      for(auto *link : to_add)
        delete link;

      return false;      
    }

    
    // 2. REMOVING: Remove links that must be removed first to avoid recursive graph. (these are found in the is_recursive function)
    //
    {
      radium::Vector<SoundProducerLink*> empty;
      radium::Scheduled_RT_functions rt_functions;
      SoundProducer::add_and_remove_links(empty, links_that_must_be_removed_first, rt_functions);
    }

    
    // 2.5 Create link_enabled_changes from explicitly_link_enabled_changes and Solo settings.
    //
    LinkEnabledChanges link_enabled_changes;
    PluginImplicitlyMutedChanges plugin_implicitly_muted_changes;
    {
      // First update all "is_explicitly_enabled" variables.
      for(const auto &link_explicitly_enabled_change : link_explicitly_enabled_changes)
        link_explicitly_enabled_change.link->is_explicitly_enabled = link_explicitly_enabled_change.link_enabled;
      
      // Then create the link_enabled_changes.
      UpdateImplicitSolo implicit(to_add, to_remove, solo_changes, link_enabled_changes, plugin_implicitly_muted_changes);
      
      for(const auto &link_enabled_change : link_enabled_changes)
        link_enabled_change.link->is_enabled = link_enabled_change.link_enabled;
      //}
      
      implicit.schedule_RT_funcs(rt_functions);
    }
    
    // 3. ADDING: Allocate memory for new links in the radium::Vector vectors.
    //
    {
      // A little bit tricky. Find how many more elements that is added to each soundproducerlink vector.
      // (can't call ensure_there_is_room_for_more_without_having_to_allocate_memory several times)
      
      QMap < radium::Vector<SoundProducerLink*>* , int > howmanys;
        
      for(auto *link : to_add){
        
        auto *input_linkvector = &link->source->_output_links;
        auto *output_linkvector = &link->target->_input_links;
        
        howmanys[input_linkvector] = howmanys.value(input_linkvector) + 1;
        howmanys[output_linkvector] = howmanys.value(output_linkvector) + 1;
        
        //printf("..Adding 1 to %p (%d)\n", input_linkvector, howmanys.value(input_linkvector));
        //printf("..Adding 1 to %p (%d)\n", output_linkvector, howmanys.value(output_linkvector));
      }

      for(auto *linkvector : howmanys.keys()){
        //printf("...Asking %p to ensure room for %d new. (now: %d)\n", linkvector, howmanys.value(linkvector), linkvector->size());
        linkvector->ensure_there_is_room_for_more_without_having_to_allocate_memory(howmanys.value(linkvector));
      }
    }

    // 4. REMOVING/ADDING/VOLUME: Request links to be removed to turn off, add new links, and set new link volumes.
    //
    {      
      // REMOVE
      for(auto *link : to_remove)
        rt_functions.add([link]
          {
            //printf("   ___ Requesting to turn off link %p\n", link);
            link->RT_request_turn_off();
          });
      
      // ADD
      for(auto *link : to_add){
        rt_functions.add([link]
          {
            link->source->_output_links.push_back(link);
            link->target->_input_links.push_back(link);
          });
      }

      // Change volume
      for(const auto &volume_change : volume_changes){
        auto *link = volume_change.link;
        float new_volume = volume_change.new_volume;
        rt_functions.add([link, new_volume]
          {        
            link->set_link_volume(new_volume);
          });
      }
    }


    // Run it.
    D(printf("* add_and_remove_links: Waiting\n"));
    rt_functions.schedule_to_run_on_player_thread_and_wait();

    
    // 5. ADDING: Do some post-add stuff in the radium::Vector vectors. (free previously used memory, if any.)
    //
    for(auto *link : to_add){
      link->source->_output_links.post_add();
      link->target->_input_links.post_add();
    }

    
    // 6. REMOVING: Wait until all links in 'to_remove' can be removed (when volume is finished fading down to 0).
    //
    for(auto *link : to_remove){
      PLUGIN_touch(link->source->_plugin);
      PLUGIN_touch(link->target->_plugin);
      //printf("   _______ waiting for link %p to be turned off...\n", link);
      while(link->can_be_removed()==false){
        PLUGIN_touch(link->source->_plugin);
        PLUGIN_touch(link->target->_plugin);
        PLAYER_memory_debug_wake_up();
        msleep(5);
      }
      //printf("   ___________got it. link %p was turned off...\n", link);
    }


    // 7. REMOVING: Remove the 'to_remove' links from the graph.
    //
    if (to_remove.size() > 0){
      radium::PlayerLock lock;

      int i = 0;
      for(auto *link : to_remove){
        PLAYER_maybe_pause_lock_a_little_bit(i++);
        link->target->_input_links.remove(link);
        link->source->_output_links.remove(link);
      }
    }

    
    // 8. REMOVING: Deallocate the 'to_remove' links.
    //
    for(auto *link : to_remove)
      delete link;

    GFX_ScheduleInstrumentRedraw(PATCH_get_current());

    return true;
  }
  
  static bool add_link(SoundProducerLink *link, bool *was_recursive = NULL){
    radium::Vector<SoundProducerLink*> to_add;
    radium::Vector<SoundProducerLink*> to_remove;

    to_add.push_back(link);

    radium::Scheduled_RT_functions rt_functions;
    return add_and_remove_links(to_add, to_remove, rt_functions, was_recursive);
  }

  bool add_eventSoundProducerInput(SoundProducer *source){
    bool is_recursive = source->is_recursive(this); // Just used for assertion.
    /*
    if(source->is_recursive(this)==true){
      GFX_Message(NULL, "Recursive graph not supported\n");
      return false;
    }
    */
    
    SoundProducerLink *elink = new SoundProducerLink(source, this, true);

    bool is_recursive2;

    bool ret = SoundProducer::add_link(elink, &is_recursive2);

    R_ASSERT(is_recursive==is_recursive2);

    return ret;
  }
  
  bool add_SoundProducerInput(SoundProducer *source, int source_ch, int target_ch){
    //fprintf(stderr,"*** this: %p. Adding input %p / %d,%d\n",this,sound_producer,sound_producer_ch,ch);

    bool is_recursive = source->is_recursive(this); // Only used for assertion.

    /*
    if(is_recursive){
      GFX_Message(NULL, "Recursive graph not supported\n");
      //return false;
    }
    */
    
    SoundProducerLink *link = new SoundProducerLink(source, this, false);
    link->source_ch = source_ch;
    link->target_ch = target_ch;

    bool is_recursive2;

    bool ret = SoundProducer::add_link(link, &is_recursive2);

    R_ASSERT(is_recursive == is_recursive2);

    return ret;
  }

  static void remove_links(radium::Vector<SoundProducerLink*> &links){
    radium::Vector<SoundProducerLink*> to_add; // empty

    radium::Scheduled_RT_functions rt_functions;
    add_and_remove_links(to_add, links, rt_functions);
  }
  
  static void remove_link(SoundProducerLink *link){
    radium::Vector<SoundProducerLink*> to_add;
    radium::Vector<SoundProducerLink*> to_remove;
    radium::Scheduled_RT_functions rt_functions;
    
    to_remove.push_back(link);

    add_and_remove_links(to_add, to_remove, rt_functions);
  }
  
  void remove_eventSoundProducerInput(const SoundProducer *source){
    SoundProducerLink *link = find_input_event_link(source);
    if (link!=NULL){
      SoundProducer::remove_link(link);
      //call_me_after_solo_has_changed();
    }
  }

  SoundProducerLink *find_input_link(const SoundProducer *source, bool may_return_null = false){
    for (SoundProducerLink *link : _input_links)
      if(link->source==source)
        return link;

    if (may_return_null==false)
      RError("Could not find input link. Size: %d. source: %p.\n",_input_links.size(), source);
    
    return NULL;
  }
  
  SoundProducerLink *find_input_audio_link(const SoundProducer *source, int source_ch, int target_ch, bool may_return_null = false){
    for (SoundProducerLink *link : _input_links) {

      if(true
         && link->is_event_link==false
         && link->source==source
         && link->source_ch==source_ch
         && link->target_ch==target_ch
         )
        {
          return link;
        }
    }

    if (may_return_null==false)
      RError("Could not find input audio link. Size: %d. source: %p. sch: %d, tch: %d\n",_input_links.size(), source, source_ch, target_ch);
    
    return NULL;
  }
  
  SoundProducerLink *find_input_event_link(const SoundProducer *source, bool may_return_null = false){
    for (SoundProducerLink *link : _input_links) {

      if(true
         && link->is_event_link==true
         && link->source==source
         )
        {
          return link;
        }
    }

    if (may_return_null==false)
      RError("Could not find input event link. Size: %d. source: %p.\n",_input_links.size(), source);
    
    return NULL;
  }
  
  void remove_SoundProducerInput(const SoundProducer *source, int source_ch, int target_ch){
    //printf("**** Asking to remove connection\n");

    SoundProducerLink *link = find_input_audio_link(source, source_ch, target_ch);
    if (link!=NULL)
      SoundProducer::remove_link(link);
  }

  // fade in 'input'
  void RT_crossfade_in2(float *input, float *output, int fade_pos, int num_frames) const {
    RT_fade_in2(input, fade_pos, num_frames);
    RT_fade_out2(output, fade_pos, num_frames);

    JUCE_add_sound(output, input, num_frames);
  }

  // fade out 'input'
  void RT_crossfade_out2(float *input, float *output, int fade_pos, int num_frames) const {
    RT_fade_out2(input, fade_pos, num_frames);
    RT_fade_in2(output, fade_pos, num_frames);

    JUCE_add_sound(output, input, num_frames);
  }

  void RT_apply_system_filter2(SystemFilter *filter, float **input, float **output, int num_channels, int num_frames, bool process_plugins) const {
    if(filter->plugins==NULL)
      if(num_channels==0){
        return;
      }else if(num_channels==1){
        float *s[2];
        float *temp_ch2 = RT_ALLOC_ARRAY_STACK(float, num_frames);
        s[0] = input[0];
        s[1] = temp_ch2;
        memset(s[1],0,sizeof(float)*num_frames);
        COMPRESSOR_process(_plugin->compressor, s, s, num_frames);
        if(s[0] != output[0])
          memcpy(output[0],s[0],sizeof(float)*num_frames);
      }else{
        COMPRESSOR_process(_plugin->compressor, input, output, num_frames); // Two first channels
        for(int ch=2 ; ch < num_channels ; ch++) // Rest of the channels. (no compression)
          if (input[ch] != output[ch])
            memcpy(output[ch], input[ch], sizeof(float)*num_frames);
      }
    else
      for(int ch=0;ch<num_channels;ch++)
        PLUGIN_RT_process(filter->plugins[ch], 0, num_frames, &input[ch], &output[ch], process_plugins);
  }

  // Quite chaotic with all the on/off is/was booleans.
  void RT_apply_system_filter(SystemFilter *filter, float **sound, int num_channels, int num_frames, bool process_plugins) const {
    if (num_channels==0)
      return;
    
    if(ATOMIC_GET(filter->is_on)==false && filter->was_on==false)
      return;

    float **s = RT_ALLOC_ARRAY_STACK(float*, R_MAX(1, num_channels));
    float *filter_sound = RT_ALLOC_ARRAY_STACK(float, R_MAX(1, num_channels*num_frames));
    for(int ch=0;ch<num_channels;ch++)
      s[ch] = &filter_sound[ch*num_frames];

    {
      // Set up buffers used when fading in/out
      if(ATOMIC_GET(filter->is_on)==true){
        
        if(filter->was_off==true){ // fade in

          for(int ch=0;ch<num_channels;ch++){
            memcpy(s[ch], sound[ch], sizeof(float) * num_frames);
            RT_fade_in2(s[ch], filter->fade_pos, num_frames);
          }
          
          RT_apply_system_filter2(filter,s,s,num_channels,num_frames, process_plugins);
          
          for(int ch=0;ch<num_channels;ch++)
            RT_crossfade_in2(s[ch], sound[ch], filter->fade_pos, num_frames);

          filter->fade_pos += num_frames;
          if(filter->fade_pos>=FADE_LEN){
            R_ASSERT_NON_RELEASE(filter->fade_pos==FADE_LEN);
            filter->was_off=false;
            filter->fade_pos = 0;
            filter->was_on=true;
          }
          
        }else{
          
          RT_apply_system_filter2(filter,sound,sound,num_channels,num_frames, process_plugins);      
          filter->was_on=true;   
        }
       
        
      }else if(filter->was_on==true){ // fade out.

        for(int ch=0;ch<num_channels;ch++){
          memcpy(s[ch], sound[ch], sizeof(float) * num_frames);
          RT_fade_out2(s[ch], filter->fade_pos, num_frames);
        }
          
        RT_apply_system_filter2(filter,s,s,num_channels,num_frames, process_plugins);
        
        for(int ch=0;ch<num_channels;ch++)
          RT_crossfade_out2(s[ch], sound[ch], filter->fade_pos, num_frames);
                
        filter->fade_pos += num_frames;
        if(filter->fade_pos>=FADE_LEN){
          R_ASSERT_NON_RELEASE(filter->fade_pos==FADE_LEN);
          filter->was_on=false;
          filter->was_off=true;
          filter->fade_pos = 0;

          // Fill output buffer with empty data
          if (filter->plugins != NULL){
            SoundPlugin *plugin = filter->plugins[0];
            int latency = plugin->type->RT_get_latency!=NULL ? plugin->type->RT_get_latency(plugin) : 0;
            if (latency > 0)
			{
				float ** empty = RT_ALLOC_ARRAY_STACK(float*, num_channels);
				for(int ch=0;ch<num_channels;ch++)
					empty[ch] = (float*)g_empty_sound;
				
				for(int i=0;i<ceil(latency/RADIUM_BLOCKSIZE);i++)
					RT_apply_system_filter2(filter, empty, s, num_channels, RADIUM_BLOCKSIZE, process_plugins);
            }
          }
        }
      }

    }
  }

  bool RT_is_autosuspending(void) const{
	  return ATOMIC_NAME(_plugin->_RT_is_autosuspending);
	  //return ATOMIC_GET_RELAXED(_plugin->_RT_is_autosuspending);
  }
  
  // Called by main mixer thread before starting multicore, and before RT_called_for_each_soundcard_block2.
  // Note: Called for each _soundcard_ block, not each radium block.
  void RT_called_for_each_soundcard_block1(int64_t time){
    running_time = 0.0;
    has_run_for_each_block2 = false;

    // Set initial autosuspend. Autosuspend is also set to false when a soundproducer sends audio to another soundproducer.
    {      
      bool autosuspend = RT_PLUGIN_can_autosuspend(_plugin, time);
      
      // We don't autosuspend current patch when not playing.
      if (autosuspend && !is_playing()){
        struct Patch *current_patch = ATOMIC_GET(g_through_patch);
        if (_plugin->patch == current_patch)
          autosuspend = false;
      }

      bool was_autosuspending_last_cycle = RT_is_autosuspending();

      // Set _is_autosuspending. _is_autosuspending is only used by the GUI.
      ATOMIC_SET_RELAXED(_plugin->_is_autosuspending, autosuspend && was_autosuspending_last_cycle);

#if 0
      if (ATOMIC_NAME(_plugin->_RT_is_autosuspending) != autosuspend)
        printf("%s: Autosuspend %s\n", _plugin->patch->name, autosuspend ? "ON" : "OFF");
#endif
      
      ATOMIC_NAME(_plugin->_RT_is_autosuspending) = autosuspend;
    }
  }

  // Called by main mixer thread before starting multicore.
  // Note: Called for each _soundcard_ block, not each radium block.
  void RT_called_for_each_soundcard_block2(int64_t time){
    if (has_run_for_each_block2 == true)
      return;

    has_run_for_each_block2 = true;
    
    _num_active_input_links = 0;

    // 1. Find _num_active_input_links, and call RT_called_for_each_soundcard_block2 for all active input links.
    //
    for (SoundProducerLink *link : _input_links) {

      //printf("hepp %s->%s: %d. is_active: %d\n", link->source->_plugin->patch->name, link->target->_plugin->patch->name, link->is_active, link->gakk++);

      link->RT_called_for_each_soundcard_block(); // Updates link->is_active and link->volume.

      if(link->RT_is_active) {
        _num_active_input_links++;

        // 2. Ensure RT_called_for_each_soundcard_block2 is called for all soundobjects sending sound here. (since we read the _latency variable from those a little bit further down in this function)
        
        // if(!link->is_event) // Not faster since since RT_called_for_each_soundcard_block2 returns immediately when it's already been called.
        link->source->RT_called_for_each_soundcard_block2(time);
      }
    }


    // 3. Find and set plugin latency, _latency and _highest_input_link_latency
    //
    
    const int plugin_latency = (_plugin->type->RT_get_latency==NULL || !g_RT_enable_latency_compensation) ? 0 : _plugin->type->RT_get_latency(_plugin);

    int pitch_latency = 0;
    
    if (_num_outputs > 0 && ATOMIC_GET(_plugin->pitch.is_on)){
      SoundPlugin *plugin = _plugin->pitch.plugins[0];
      pitch_latency = plugin->type->RT_get_latency!=NULL ? plugin->type->RT_get_latency(plugin) : 0;
    }
    
    // Used by dry/wet
    for(int ch = 0 ; ch < _num_dry_sounds ; ch++){
      int latency = plugin_latency;
      
      if (root->song->include_pan_and_dry_in_wet_signal)
        latency += pitch_latency;
      
      _dry_sound_latencycompensator_delays[ch].RT_set_preferred_delay(latency); // <- Note! NOT set to _latency. Set to plugin_latency.
    }

    {
      int highest_input_link_latency = 0;
    
      for (SoundProducerLink *link : _input_links) {

        // Commented out.
        // A link can be set non-active when it's muted, implicitly muted, link-volume set to 0, auto-suspended, and maybe other reasons.
        // We don't want to potentially recalculate latency for the whole audio graph when any of these things happen.
        //if (false==link->RT_is_active)
        //  continue;
        
        if (link->is_event_link)
          continue;
        
        SoundProducer *source = link->source;
        
        int source_latency = source->_latency;
        if (source_latency > highest_input_link_latency)
          highest_input_link_latency = source_latency;        
      }
      
      if (highest_input_link_latency != _highest_input_link_latency)
        _highest_input_link_latency = highest_input_link_latency;

      if (_plugin==RT_get_system_out_plugin())
        g_RT_system_out_input_latency = _highest_input_link_latency;
    }
    
    {    
      int new_latency = plugin_latency + pitch_latency;

      if (_plugin->RT_input_latency_manifests_into_output_latency)
        new_latency +=_highest_input_link_latency;
      
      if (new_latency != _latency){
        _latency = new_latency;
        D(printf("    Set latency to %d. (%s). Highest: %d. plugin latency: %d\n", _latency, _plugin->patch->name, _highest_input_link_latency, plugin_latency));
      }
    }
  }
  
  bool has_run(int64_t time) const {
    return _last_time == time;
  }

  void RT_set_input_peak_values(const float *input_peaks, float **dry_sound){
    for(int ch=0;ch<_num_dry_sounds;ch++){
      float peak = input_peaks[ch];
      
      if (RT_message_will_be_sent()==true)
        if (peak > 317)
          show_high_peak_message(_plugin, "received", dry_sound[ch], ch, peak);

      RT_AUDIOMETERPEAKS_add(_plugin->input_volume_peaks, ch, peak);
      
      //safe_float_write(&_plugin->input_volume_peak_values[ch], peak);
      
      //        if (ch<2)
      //          safe_volatile_float_write(&_plugin->system_volume_peak_values[ch], peak); // Value only used by the slider at the bottom bar.
    }
  }
    
  void RT_set_output_peak_values(const float *volume_peaks, float **output_sound) const {
    // Output peaks
    {
      for(int ch=0;ch<_num_outputs;ch++){
        float volume_peak = volume_peaks[ch];

        if (RT_message_will_be_sent()==true)
          if (volume_peak > 317)
            show_high_peak_message(_plugin, "generated (after applying volume and system effects)", output_sound[ch], ch, volume_peak);
          
        // "Volume"
        //safe_float_write(&_plugin->volume_peak_values[ch], volume_peak);
        RT_AUDIOMETERPEAKS_add(_plugin->volume_peaks, ch, volume_peak);
          
        // "Reverb Bus" and "Chorus Bus"
        if (ch < 2) { // buses only have two channels
          RT_AUDIOMETERPEAKS_add(_plugin->bus0_volume_peaks,ch, volume_peak * _plugin->bus_volume[0]);
          RT_AUDIOMETERPEAKS_add(_plugin->bus1_volume_peaks,ch, volume_peak * _plugin->bus_volume[1]);
          RT_AUDIOMETERPEAKS_add(_plugin->bus2_volume_peaks,ch, volume_peak * _plugin->bus_volume[2]);
          RT_AUDIOMETERPEAKS_add(_plugin->bus3_volume_peaks,ch, volume_peak * _plugin->bus_volume[3]);
          RT_AUDIOMETERPEAKS_add(_plugin->bus4_volume_peaks,ch, volume_peak * _plugin->bus_volume[4]);
          /*
          safe_float_write(&_plugin->bus_volume_peak_values0[ch], volume_peak * _plugin->bus_volume[0]);
          safe_float_write(&_plugin->bus_volume_peak_values1[ch], volume_peak * _plugin->bus_volume[1]);
          safe_float_write(&_plugin->bus_volume_peak_values2[ch], volume_peak * _plugin->bus_volume[2]);
          safe_float_write(&_plugin->bus_volume_peak_values3[ch], volume_peak * _plugin->bus_volume[3]);
          safe_float_write(&_plugin->bus_volume_peak_values4[ch], volume_peak * _plugin->bus_volume[4]);
          */
        }
        
        // "Out" and Chip volume  (same value)
        {
          float output_volume_peak = volume_peak * _plugin->output_volume;
          
          RT_AUDIOMETERPEAKS_add(_plugin->output_volume_peaks, ch, output_volume_peak);

          /*
          if (ATOMIC_GET(_plugin->output_volume_is_on))
            safe_float_write(&_plugin->output_volume_peak_values_for_chip[ch], output_volume_peak);
          else
            safe_float_write(&_plugin->output_volume_peak_values_for_chip[ch], 0.0f); // The chip volume slider is not grayed out, like the out "out" slider.
          */
        }
      }
    }
  }

  void RT_process_pan_width_and_pitch(float **outputs, int num_frames, bool process_plugins) const {
    // Output pan
    SMOOTH_apply_pan(&_plugin->pan, outputs, _num_outputs, num_frames);

    // Right channel delay ("width")
    if(_num_outputs>1)
      static_cast<radium::SmoothDelay*>(_plugin->delay)->RT_process(num_frames, outputs[1], outputs[1]);

    RT_apply_system_filter(&_plugin->pitch, outputs, _num_outputs, num_frames, process_plugins);
  }

  // Note: Always called for all audio links, active or not, autosuspended or not, bus or not, going to be mixed or not.
  void RT_update_link_before_mixing(SoundProducerLink *link, int64_t time, int num_frames) const {

    SoundProducer *source = link->source;
    
    int latency = _highest_input_link_latency - source->_latency;

    latency = R_MAX(0, latency);
    
    if (latency >= link->_delay._delay.get_max_delay_size()) {
      RT_message("%s -> %s: Compensating for a latency of more than %dms is not supported.\nNumber of frames: %d",  source->_plugin->patch->name, link->target->_plugin->patch->name, MAX_COMPENSATED_LATENCY, latency);
      latency = link->_delay._delay.get_max_delay_size()-1;
    }
    
    if (latency != link->_delay._delay.getSize()) {
      link->_delay.RT_set_preferred_delay(latency);
#ifndef RELEASE
      printf("    Set latency %d. (%s -> %s)\n", latency, source->_plugin->patch->name, link->target->_plugin->patch->name);
#endif
    }
    
    if (link->RT_is_active)
      SMOOTH_called_per_block(&link->volume); // A link can't turn inactive before link->volume has been smoothed down to 0.
  }
  
  // This function is called while processing link->source.
  // "this" is link->target.
  // If "source_output_sound" can be NULL.
  void RT_create_dry_sound_from_input_link(SoundProducerLink *link, int64_t time, int num_frames, float **source_output_sound, float **target_dry_sound) const {
#if !defined(RELEASE)
    
    if (this != link->target)
      abort();
    
    if (target_dry_sound==NULL)
      abort();

    if (!link->RT_is_active){
      if (source_output_sound!=NULL)
        abort();
    }

    if(link->source->RT_is_autosuspending())
      if (source_output_sound!=NULL)
        abort();
    
    if (source_output_sound != NULL)
      if (link->source->has_run(time)==false)
        abort();
#endif
    
    //const float *input_producer_sound = link->source->_output_buffer.get_channel(link->source_ch);
    
    float *latency_sound_sound = RT_ALLOC_ARRAY_STACK(float, num_frames);
    const float *latency_compensated_input_producer_sound;
    
    if (source_output_sound==NULL)
      latency_compensated_input_producer_sound = link->_delay.RT_call_instead_of_process_if_no_sound(num_frames, latency_sound_sound);
    else
      latency_compensated_input_producer_sound = link->_delay.RT_process(source_output_sound[link->source_ch], latency_sound_sound, num_frames);

    if (latency_compensated_input_producer_sound != NULL){
      
      if (link->source->_curr_output_is_silent[link->source_ch])
        link->_last_buffer_was_silent = true;
      else
        link->_last_buffer_was_silent = !SMOOTH_mix_sounds(&link->volume,
                                                           target_dry_sound[link->target_ch], // out
                                                           latency_compensated_input_producer_sound, // in
                                                           num_frames
                                                           );

    }else{
      
      link->_last_buffer_was_silent = true;
      
    }    
  }

  // This function is called while processing link->source. "this" is link->target.
  // Note: Always called for all audio links, active or not, autosuspended or not, bus or not.
  void RT_process_link(SoundProducerLink *link, int64_t time, float **source_output_sound, int num_frames) {
#if !defined(RELEASE)    
    if (this != link->target)
      abort();
#endif

    RT_update_link_before_mixing(link, time, num_frames);

    bool input_line_is_empty = source_output_sound==NULL && link->_delay.RT_delay_line_is_empty();
    
    if (link->RT_is_active==false || input_line_is_empty) {
      link->_delay.RT_call_instead_of_process_if_no_sound(num_frames, NULL);
      return;
    }

    RT_PLUGIN_touch(link->target->_plugin);

    {
      radium::ScopedMultiThreadAccessArrayElement<decltype(_input_buffers)> scoped_buffer(_input_buffers);

      auto *buffer = scoped_buffer.RT_get();
      
      if (buffer->RT_obtain_channels_if_necessary(radium::NeedsLock::YES))
        buffer->clean();
      
      float **target_dry_sound = buffer->get_channels();
      
      RT_create_dry_sound_from_input_link(link, time, num_frames, source_output_sound, target_dry_sound);
    }
  }
  
  void RT_iterate_output_links_after_process(int64_t time, int num_frames, float **output_sound) const {
      
    for(auto link : _output_links){
          
      if (link->is_event_link)
        continue;

      link->target->RT_process_link(link, time, output_sound, num_frames);
    }
  }
  
  void RT_process(int64_t time, int num_frames, bool process_plugins)
  {
    float *dry_sound_sound = RT_ALLOC_ARRAY_STACK(float, _num_dry_sounds*num_frames);
    
    float **dry_sound = RT_ALLOC_ARRAY_STACK(float*, R_MAX(1,_num_dry_sounds)); // r_max to avoid ubsan hit
    for(int ch=0;ch<_num_dry_sounds;ch++)
		dry_sound[ch] = &dry_sound_sound[ch*num_frames];

    R_ASSERT(has_run(time)==false);
    _last_time = time;

    if (_num_inputs > 0){
      
		bool *has_set = RT_ALLOC_ARRAY_STACK(bool, _num_inputs);
		memset(has_set, 0, sizeof(bool)*_num_inputs);
		
		for(auto *buffer : _input_buffers)
		{
			if (buffer->has_channels())
			{
				for(int ch=0;ch<_num_inputs;ch++)
				{
					if (has_set[ch])
					{
						JUCE_add_sound(dry_sound[ch], buffer->get_channel(ch), num_frames);
					}
					else
					{
						memcpy(dry_sound[ch], buffer->get_channel(ch), sizeof(float)*num_frames);
						has_set[ch] = true;
					}
				}

				buffer->RT_release_channels(radium::NeedsLock::YES);
			}
		}

		for(int ch=0;ch<_num_inputs;ch++)
			if (!has_set[ch])
				memset(dry_sound[ch], 0, sizeof(float)*num_frames);
    }
    
    PLUGIN_update_smooth_values(_plugin);

    
    // Note: output_sound and wet_sound is actually the same variable (optimization)
    
    float *output_sound_sound = RT_ALLOC_ARRAY_STACK(float, R_MAX(1, _num_outputs)*num_frames);
    float **output_sound = RT_ALLOC_ARRAY_STACK(float*, R_MAX(1, _num_outputs));
    for(int ch=0;ch<_num_outputs;ch++)
      output_sound[ch] = &output_sound_sound[ch*num_frames];

    float **wet_sound = output_sound;

    {
		float *temp_sound_sound = RT_ALLOC_ARRAY_STACK(float, R_MAX(1, _num_inputs)*num_frames);
		float **temp_sound = RT_ALLOC_ARRAY_STACK(float*, R_MAX(1, _num_inputs));
		for(int ch=0;ch<_num_inputs;ch++)
			temp_sound[ch] = &temp_sound_sound[ch*num_frames];
      
      
      bool is_a_generator = _num_inputs==0;
      
      
      if(is_a_generator)
        PLUGIN_RT_process(_plugin, time, num_frames, temp_sound, dry_sound, process_plugins); // I guess we could send NULL instead of temp_sound.
      
      
      // Input peaks
      if (_num_dry_sounds > 0){
        bool do_bypass = is_bypassed(_plugin); //_plugin->drywet.smoothing_is_necessary==false && _plugin->drywet.value==0.0f;
        
        float *input_peaks = RT_ALLOC_ARRAY_STACK(float, _num_dry_sounds);
        for(int ch=0;ch<_num_dry_sounds;ch++) {
          
          float dry_peak = RT_get_max_val(dry_sound[ch],num_frames);
          
          if (dry_peak > MIN_AUTOSUSPEND_PEAK)
            RT_PLUGIN_touch(_plugin);
          
          float input_volume = SMOOTH_get_target_value(&_plugin->input_volume);
          
          input_peaks[ch] = do_bypass ? 0.0f : dry_peak *  input_volume;
        }
        RT_set_input_peak_values(input_peaks, dry_sound);
      }
      
      if(is_a_generator){
        
        // Apply input volume and fill output
        for(int ch=0;ch<_num_outputs;ch++)
          SMOOTH_copy_sound(&_plugin->input_volume, dry_sound[ch], wet_sound[ch], num_frames);
        
      }else{
        
        // Apply input volume
        for(int ch=0;ch<_num_inputs;ch++)
          SMOOTH_copy_sound(&_plugin->input_volume, dry_sound[ch], temp_sound[ch], num_frames);
        
        // Fill output
        PLUGIN_RT_process(_plugin, time, num_frames, temp_sound, wet_sound, process_plugins);
      }
    }

    
    // compressor
    RT_apply_system_filter(&_plugin->comp,      wet_sound, _num_outputs, num_frames, process_plugins);

    
    // filters
    RT_apply_system_filter(&_plugin->lowshelf,  wet_sound, _num_outputs, num_frames, process_plugins);
    RT_apply_system_filter(&_plugin->eq1,       wet_sound, _num_outputs, num_frames, process_plugins);
    RT_apply_system_filter(&_plugin->eq2,       wet_sound, _num_outputs, num_frames, process_plugins);
    RT_apply_system_filter(&_plugin->highshelf, wet_sound, _num_outputs, num_frames, process_plugins);
    RT_apply_system_filter(&_plugin->lowpass,   wet_sound, _num_outputs, num_frames, process_plugins);
    RT_apply_system_filter(&_plugin->highpass,  wet_sound, _num_outputs, num_frames, process_plugins);

    // Apply volume to the wet values.
    for(int ch=0 ; ch < _num_outputs ; ch++)
      SMOOTH_apply_volume(&_plugin->volume, wet_sound[ch], num_frames);
      
    // Note: We could have optimized applying volume and dry/wet into one step. But that would have made the code a little bit more complicated.
    // In addition, applying dry/wet is a dummy operation if wet=1.0 and dry=0.0, which is the normal situation, so it would normally
    // not make a difference in CPU usage. (and even when drywet!=1.0, there would probably be very little difference in CPU usage)

    const bool include_pan_and_width_in_wet = root->song->include_pan_and_dry_in_wet_signal;
    
    
    if (include_pan_and_width_in_wet)
      RT_process_pan_width_and_pitch(wet_sound, num_frames, process_plugins);

    {
		float *latency_dry_sound_sound = RT_ALLOC_ARRAY_STACK(float, R_MAX(1,_num_dry_sounds)*num_frames); // Using 'R_MAX' since array[0] is undefined behavior. (ubsan fix only, most likely nothing bad would happen)
		const float **latency_dry_sound = RT_ALLOC_ARRAY_STACK(const float*, R_MAX(1,_num_dry_sounds));
	  
		for(int ch=0;ch<_num_dry_sounds;ch++)
			// Usually a no-op. If the plugin has no latency, latency_dry_sound[ch] will just be set to dry_sound[ch].
			latency_dry_sound[ch] = _dry_sound_latencycompensator_delays[ch].RT_process(dry_sound[ch],
																						&latency_dry_sound_sound[ch*num_frames],
																						num_frames);
		
		RT_apply_dry_wet(latency_dry_sound, _num_dry_sounds, // dry
						 wet_sound, _num_outputs,        // wet (-> becomes output sound)
						 num_frames,
						 &_plugin->drywet);
    }


    //_input_buffers[cpunum]->RT_release_channels_if_necessary(radium::NeedsLock::YES);
    

    if (!include_pan_and_width_in_wet)
      RT_process_pan_width_and_pitch(output_sound, num_frames, process_plugins);
    

    bool is_touched = false;
    
    // Output peaks
    if (_num_outputs > 0)
	{
		float *volume_peaks = RT_ALLOC_ARRAY_STACK(float, _num_outputs);
      
		for(int ch=0;ch<_num_outputs;ch++) {

			const float out_peak = RT_get_max_val(output_sound[ch],num_frames);
			volume_peaks[ch] = out_peak;

#if 1
			if (out_peak <= MIN_NONSILENT_PEAK)
			{
				_curr_output_is_silent[ch] = true;
			}
			else
			{
				_curr_output_is_silent[ch] = false;
				if (false==is_touched)
					is_touched = true;
			}
#else
			_curr_output_is_silent[ch] = SMOOTH_peak_is_below_lowest_fade_value(out_peak);
			
			if(false==is_touched && out_peak > MIN_AUTOSUSPEND_PEAK)
				is_touched = true;
#endif
		}
      
		if(is_touched)
			RT_PLUGIN_touch(_plugin);
        
		RT_set_output_peak_values(volume_peaks, output_sound);
    }
    
    RT_iterate_output_links_after_process(time, num_frames, is_touched ? output_sound : NULL);
  }
};


// Only called from audio/audio_instrument.cpp/AUDIO_InitPatch2.
SoundProducer *SP_create(SoundPlugin *plugin, Buses buses){
  return new SoundProducer(plugin, MIXER_get_buffer_size(), buses);
}

// Called from AUDIO_remove_patchdata.
// AUDIO_remove_patchdata has already called CHIP_delete, which took care of deleting all non-bus links.
//
void SP_delete(SoundProducer *producer){
  D(printf("Deleting \"%s\"\n",producer->_plugin->type->type_name));
  delete producer;
}

int64_t SP_get_id(const SoundProducer *producer){
  return producer->_id;
}

// Returns false if the link could not be made. (i.e. the link was recursive)
bool SP_add_elink(SoundProducer *target, SoundProducer *source){
  return target->add_eventSoundProducerInput(source);
}

const radium::LinkParameters g_empty_linkparameters;

// Should simultaneously fade out the old and fade in the new.
// Only audio links.
bool SP_add_and_remove_links(const radium::LinkParameters &parm_to_add,
                             const radium::LinkParameters &parm_to_remove,
                             radium::SoloChanges &solo_changes,
                             radium::Scheduled_RT_functions &rt_functions
                             )
{  
  ASSERT_IS_NONRT_MAIN_THREAD_NON_RELEASE();

  //printf("     SP_add_and-remove. Num to add: %d. Num to remove: %d.\n", parm_to_add.size(), parm_to_remove.size());
  
  if (parm_to_add.size()==0 && parm_to_remove.size()==0)
    return false;
  
  radium::Vector<SoundProducerLink*> to_add;
  radium::Vector<SoundProducerLink*> to_remove;
  LinkExplicitlyEnabledChanges link_explicitly_enable_changes;
  VolumeChanges volume_changes;

  for(auto &parm : parm_to_add){
    
    SoundProducerLink *existing_link = parm.target->find_input_audio_link(parm.source, parm.source_ch, parm.target_ch, true);

    if (existing_link != NULL) {
      
      if (parm.enable_type != radium::EnableType::DONT_CHANGE) {
        bool link_is_explicitly_enabled = parm.enable_type==radium::EnableType::DISABLE ? false : true;
        if (existing_link->is_explicitly_enabled != link_is_explicitly_enabled)
          link_explicitly_enable_changes.push_back(LinkEnabledChange(existing_link, link_is_explicitly_enabled));
      }
      
      if (parm.must_set_volume && existing_link->need_to_create_volume_change(parm.volume)) {
        //printf("    Old volume: %f. New volume: %f. ch %d -> ch %d\n", existing_link->get_link_volume(), parm.volume, existing_link->source_ch, existing_link->target_ch);
        volume_changes.push_back(VolumeChange(existing_link, parm.volume));
      }
      
    } else {
      
      SoundProducerLink *link = new SoundProducerLink(parm.source, parm.target, false);
      link->source_ch = parm.source_ch;
      link->target_ch = parm.target_ch;
      
      if (parm.must_set_volume)
        link->set_link_volume(parm.volume);

      if (parm.enable_type != radium::EnableType::DONT_CHANGE) {
        bool link_is_explicitly_enabled = parm.enable_type==radium::EnableType::DISABLE ? false : true;
        link->is_explicitly_enabled = link_is_explicitly_enabled;
      }
      
      to_add.push_back(link);
      
    }
  }

  for(auto &parm : parm_to_remove){
    SoundProducerLink *link = parm.target->find_input_audio_link(parm.source, parm.source_ch, parm.target_ch);
    if (link != NULL)
      to_remove.push_back(link);
  }

  bool isrecursive;
  return SoundProducer::add_and_remove_links(to_add, to_remove, rt_functions, &isrecursive, volume_changes, link_explicitly_enable_changes, solo_changes);
}

radium::SoloChanges g_empty_solochanges;

radium::SoloChanges::~SoloChanges(){
#if 1
  SoundProducer::handle_solo_changes(*this);
#else
  for(const SoloChange &change : _changes){
    SoundPlugin *plugin = change._plugin;
    float val = change._solo_is_on ? 1.0 : 0.0;
    int effect_num = change._plugin->type->num_effects+EFFNUM_SOLO_ONOFF;
    _rt_functions.add([=](){
        PLUGIN_set_effect_value(plugin, 0, effect_num, val, STORE_VALUE, FX_single, EFFECT_FORMAT_NATIVE);
      });
  }
#endif
}

bool SP_add_link(SoundProducer *target, int target_ch, SoundProducer *source, int source_ch){
  return target->add_SoundProducerInput(source,source_ch,target_ch);
}

void SP_remove_elink(SoundProducer *target, const SoundProducer *source){
  target->remove_eventSoundProducerInput(source);
}

void SP_remove_link(SoundProducer *target, int target_ch, const SoundProducer *source, int source_ch){
  target->remove_SoundProducerInput(source,source_ch,target_ch);
}


// Removes all links, both audio and event.
void SP_remove_all_links(const radium::Vector<SoundProducer*> &soundproducers){

  radium::Vector<SoundProducerLink *> links_to_delete;
  
  // Find all non-bus links
  for(auto soundproducer : soundproducers)
    for(auto link : soundproducer->_input_links)
      links_to_delete.push_back(link);
  
  SoundProducer::remove_links(links_to_delete);
}

void SP_remove_all_elinks(const radium::Vector<SoundProducer*> &soundproducers){

  radium::Vector<SoundProducerLink *> links_to_delete;
  
  // Find all non-bus links
  for(auto soundproducer : soundproducers)
    for(auto link : soundproducer->_input_links)
      if (link->is_event_link==true)
        links_to_delete.push_back(link);
  
  SoundProducer::remove_links(links_to_delete);
}

float SP_get_link_gain(const SoundProducer *target, const SoundProducer *source, const char **error){
  ASSERT_IS_NONRT_MAIN_THREAD_NON_RELEASE();
  
  for (SoundProducerLink *link : target->_input_links)
    if(link->is_event_link==false && link->source==source)
      return link->get_link_volume();

  if (error != NULL)
    *error = talloc_strdup("Could not find link");
  
  return 0.0;
}

float SP_get_actual_link_gain_and_enabled(const SoundProducer *target, const SoundProducer *source, const char **error, bool &is_enabled){
  ASSERT_IS_NONRT_MAIN_THREAD_NON_RELEASE();
  
  for (SoundProducerLink *link : target->_input_links)
    if(link->is_event_link==false && link->source==source){
      is_enabled = link->is_enabled;
      if (is_enabled)
        return link->get_link_volume();
      else
        return 0.0f;
    }
  
  if (error != NULL)
    *error = talloc_strdup("Could not find link");
  
  return 0.0;
}

bool SP_set_link_gain(SoundProducer *target, SoundProducer *source, float volume, const char **error){
  bool ret = false;
  bool found = false;
 
  for (SoundProducerLink *link : target->_input_links) {
    if(link->is_event_link==false && link->source==source){

      found=true;

      if(link->set_link_volume(volume))
        ret = true;
    }
  }

  if (!found)
    *error = talloc_strdup("Could not find link");

  return ret;
}

bool SP_call_me_after_solo_has_changed(void){
  return SoundProducer::call_me_after_solo_has_changed();
}

// Only called from AudioConnection->set_enabled().
// 'is_enabled' will only be true if AudioConnection::_is_enabled==true && AudioConnection::_is_implicitly_enabled==true.
bool SP_set_link_explicitly_enabled(SoundProducer *target, SoundProducer *source, bool is_explicitly_enabled, const char **error){
  R_ASSERT_RETURN_IF_FALSE2(target!=NULL && source!=NULL, false);
  
  bool ret = false;
  bool found = false;

  for (SoundProducerLink *link : target->_input_links) {
    if(link->is_event_link==false && link->source==source){ // link->is_bus_link==false &&
      
      found=true;
      
      if (link->is_explicitly_enabled != is_explicitly_enabled){
        
        link->is_explicitly_enabled = is_explicitly_enabled;

        bool new_link_enabled = link->should_be_enabled();
        
        if (link->is_enabled != new_link_enabled){
          link->is_enabled = new_link_enabled;
          {
            radium::PlayerLock lock;
            link->RT_is_enabled = new_link_enabled;
          }
        }
        
        //CONNECTION_update((struct Patch*)link->source->_plugin->patch, (struct Patch*)link->target->_plugin->patch);
        ret = true;
      }
    }
  }

  if (!found && error)
    *error = talloc_strdup("Could not find link");

  return ret;
}

bool SP_get_link_explicitly_enabled(SoundProducer *target, SoundProducer *source, const char **error){
  if (target==NULL || source==NULL)
    return false;
  
  for (SoundProducerLink *link : target->_input_links)
    if(link->is_event_link==false && link->source==source)
      return link->is_explicitly_enabled;

  if (error != NULL)
    *error = talloc_strdup("Could not find link");

  return false;
}
  
bool SP_get_link_implicitly_muted(SoundProducer *target, SoundProducer *source, const char **error){
  if (target==NULL || source==NULL)
    return false;
  
  for (SoundProducerLink *link : target->_input_links)
    if(link->is_event_link==false && link->source==source)
      return link->is_implicitly_muted;

  if (error != NULL)
    *error = talloc_strdup("Could not find link");

  return false;
}
  
bool SP_get_link_enabled(SoundProducer *target, SoundProducer *source, const char **error){
  if (target==NULL || source==NULL)
    return false;
  
  for (SoundProducerLink *link : target->_input_links)
    if(link->is_event_link==false && link->source==source)
      return link->is_enabled;

  if (error != NULL)
    *error = talloc_strdup("Could not find link");
  
  return false;
}

bool SP_all_output_links_were_silent(SoundProducer *sp){
  for (SoundProducerLink *link : sp->_output_links)
    if (link->is_event_link==false && !link->_last_buffer_was_silent)
      return false;

  return true;
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

static void SP_RT_process(SoundProducer *producer, int64_t time, int num_frames, bool process_plugins){
#if !defined(RELEASE)
#if !defined(FOR_MACOSX)
  if (!MIXER_is_saving()){
	  R_ASSERT_NON_RELEASE(THREADING_has_player_thread_priority());
  }
#endif
#endif
      
  SoundPlugin *plugin = producer->_plugin;
  
  bool is_visible = ATOMIC_GET_RELAXED(g_show_cpu_usage_in_mixer) || ATOMIC_GET_RELAXED(plugin->is_visible);
  CpuUsage *cpu_usage = (CpuUsage*)ATOMIC_GET_RELAXED(plugin->cpu_usage);
  
  bool add_cpu_data = is_visible && cpu_usage!=NULL;

  //double start_time;  // <--- This one must be used when testing a time function that provides seconds.
  double start_time = 0;
  
  if (add_cpu_data) {
    //start_time = monotonic_seconds(); // Checking if max time would fluctuate less with this timer. Didn't make a difference though. Both are probably using CLOCK_MONOTONIC.
    //start_time = monotonic_rdtsc_seconds();
    start_time = RT_TIME_get_ms();
  }
  
  producer->RT_process(time, num_frames, process_plugins);

  if (add_cpu_data){
    //double end_time = monotonic_rdtsc_seconds();
    //double end_time = monotonic_seconds();
    double end_time = RT_TIME_get_ms();

    plugin->processing_time_so_far_in_jack_block += (end_time-start_time);
    
    //float new_cpu_usage = 100.0 * (end_time-start_time) / ((double)num_frames / MIXER_get_sample_rate());

    //printf("Adding cpu usage for %s\n",plugin->patch->name);
    if (MIXER_get_remaining_num_audioblock_frames()==num_frames){//g_soundcardblock_delta_time==0){
      float new_cpu_usage = plugin->processing_time_so_far_in_jack_block * 0.1 * MIXER_get_sample_rate() / g_soundcardblock_size; //num_frames;
      cpu_usage->addUsage(new_cpu_usage);
      plugin->processing_time_so_far_in_jack_block = 0.0;
    }
  }
}

void SP_write_mixer_tree_to_disk(QFile *file){
  const radium::Vector<SoundProducer*> &sp_all = MIXER_get_all_SoundProducers();
  
  int num=0;
  
  for (const SoundProducer *sp : sp_all){
    SoundPlugin *plugin = sp->_plugin;
    volatile Patch *patch = plugin==NULL ? NULL : plugin->patch;
    const char *name = patch==NULL ? "<null>" : patch->name;

    file->write(QString().asprintf("%d: sp: %p (%s). num_dep: %d, num_dep_left: %d: num_dependant: %d\n",num++,sp,name,sp->_num_active_input_links,ATOMIC_GET(sp->_num_active_input_links_left), sp->_output_links.size()).toUtf8());
    
    for (SoundProducerLink *link : sp->_output_links){
      SoundPlugin *plugin = link->target->_plugin;
      volatile Patch *patch = plugin==NULL ? NULL : plugin->patch;
      const char *name = patch==NULL ? "<null>" : patch->name;
      file->write(QString().asprintf("  %s%s\n",name, link->RT_is_active?"":" (inactive)").toUtf8());
    }    
  }
}

void SP_print_tree(void){
  int num=0;

  const radium::Vector<SoundProducer*> &sp_all = MIXER_get_all_SoundProducers();
  
  for (const SoundProducer *sp : sp_all){
    fprintf(stderr,"%d (%d): sp: %p (%s). num_dep: %d, num_dep_left: %d: num_dependant: %d.\n",
            num++,
            sp->_plugin->patch==NULL?-1:(int)sp->_plugin->patch->id.id,
            sp,
            sp->_plugin->patch==NULL?"<null>":sp->_plugin->patch->name,
            sp->_num_active_input_links,
            ATOMIC_GET(sp->_num_active_input_links_left),
            sp->_output_links.size()
            );
    /*
    fprintf(stderr,"  inputs:\n");
    for (SoundProducerLink *link : sp->_input_links){
      fprintf(stderr, "    %s%s\n",link->source->_plugin->patch->name,link->RT_is_active?"":" (inactive)");
    }
    fprintf(stderr, "  outputs:\n");
    */
    for (const SoundProducerLink *link : sp->_output_links){
      fprintf(stderr, "  %s%s. Ch: %d->%d. Latency: %d\n",
              link->target->_plugin->patch->name,
              link->RT_is_active?"":" (inactive)",
              //link->volume.target_audio_will_be_modified?"":" (inactive2)",
              link->source_ch, link->target_ch,
              link->_delay._delay.getSize()
              );
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
    memset(producer->_output_buffer[ch],0,sizeof(float)*num_frames);
}
#endif


struct SoundPlugin *SP_get_plugin(const SoundProducer *producer){
  R_ASSERT_RETURN_IF_FALSE2(producer!=NULL, NULL);
  return producer->_plugin;
}

struct SoundProducer *SP_get_sound_producer(const struct SoundPlugin *plugin){
  const radium::Vector<SoundProducer*> &sp_all = MIXER_get_all_SoundProducers();

  for(auto *sp : sp_all)
    if (sp->_plugin==plugin)
      return sp;

  return NULL;
}

int SP_get_bus_num(const SoundProducer *sp){
  return sp->_bus_num;
}

#if 0
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
#endif

bool SP_is_plugin_running(const SoundPlugin *plugin){
  return plugin->sp != NULL;
}

int RT_SP_get_input_latency(const SoundProducer *sp){
  R_ASSERT_NON_RELEASE(THREADING_is_runner_thread() || PLAYER_current_thread_has_lock());
  return sp->_highest_input_link_latency;
}

int RT_SP_get_latency(const SoundProducer *sp){
  R_ASSERT_NON_RELEASE(THREADING_is_runner_thread() || PLAYER_current_thread_has_lock());
  return sp->_latency;
}

double SP_get_running_time(const SoundProducer *sp){
  return sp->running_time;
}


bool SP_has_input_links(const SoundProducer *sp){
  return sp->_input_links.size() > 0;
}

bool SP_has_output_links(const SoundProducer *sp){
  return sp->_output_links.size() > 0;
}

int SP_get_max_input_channels_from_audio_input_links(const struct SoundProducer *sp){
  int ret = 0;
  for(auto *link : sp->_input_links)
    ret = R_MAX(ret, link->source->_plugin->type->num_outputs);

  return ret;
}

int SP_get_max_visible_input_channels_from_audio_input_links(const struct SoundProducer *sp){
  int ret = 0;
  
  for(const auto *link : sp->_input_links){
    const auto *plugin = link->source->_plugin;
    if (plugin->num_visible_outputs >= 0)
      ret = R_MAX(ret, plugin->num_visible_outputs);
    else
      ret = R_MAX(ret, plugin->type->num_outputs);
  }
  
  return ret;
}

bool SP_has_audio_input_link(const SoundProducer *sp){
  for(auto *link : sp->_input_links){
    if (!link->is_event_link)
      return true;
  }

  return false;
}

bool SP_is_audio_connected(const SoundProducer *start_producer, const SoundProducer *end_producer){
  int safety = 0;
  return SoundProducer::is_audio_connected(start_producer, end_producer, safety);
}

bool SP_is_bus(const SoundProducer *sp){
  return sp->_is_bus;
}

void SP_called_regularly_by_main_thread(SoundProducer *sp){
  // Not enabled. Enable in Qt_Main.cpp.
}

