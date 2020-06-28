
#include <math.h>
#include <atomic>

#include "../common/nsmtracker.h"
#include "../common/instruments_proc.h"

#include "../audio/SoundPlugin.h"


#include "AudioMeterPeaks_proc.h"

#define UNTOUCHED_MAX_GAIN_VALUE -100

static bool gain_is_untouched(float gain){
  return gain < -0.1;
}

// NOTE. All the 'call_very_often' functions can be called from a custom exec().
// This means that _patch->plugin might be gone, and the same goes for soundproducer.
// (Note that _patch is never deleted, except when loading song.)

static void call_very_often(AudioMeterPeaks &peaks, bool reset_falloff, float ms){

  for(int ch=0;ch<peaks.num_channels;ch++){

    float max_gain;

    // Atomically read and write RT_max_gains[ch]
    //
    for(int i = 0 ; ; i++){
      max_gain = atomic_get_float_relaxed(&ATOMIC_NAME(peaks.RT_max_gains)[ch]);
      if(atomic_compare_and_set_float(&ATOMIC_NAME(peaks.RT_max_gains)[ch], max_gain, UNTOUCHED_MAX_GAIN_VALUE)==true)
        break;

      R_ASSERT_NON_RELEASE(equal_doubles(max_gain, UNTOUCHED_MAX_GAIN_VALUE) || max_gain >= -0.000000001);

      if (gain_is_untouched(max_gain)){
#if !defined(RELEASE)
        printf("    Warning: Gain is untouched: %f\n", max_gain);
#endif

        // Returning here screws up fall off speed, but it probably only happens if jack has crashed or something like that. (and in any case, we have a bigger problem then screwing up fallof speed if max gain hasn't been updated since last time.
        return;
      }
      
      // Give up after 64 tries. If the RT thread is very busy, I guess it could happen that we stall here.
      // (also note that RT_max_gains[ch] is not resetted when we fail, so we don't destroy max peak detection, which is most important)
      if(i==64)
        return;
    }

    float max_db = gain2db(max_gain);

    // max db
    //
    peaks.max_dbs[ch] = max_db;

    // decaying db
    //
    if (max_db > peaks.decaying_dbs[ch])
      peaks.decaying_dbs[ch] = max_db;
    else
      peaks.decaying_dbs[ch] -= ms / 50;
    
    if (max_db > peaks.decaying_dbs_10x[ch])
      peaks.decaying_dbs_10x[ch] = max_db;
    else
      peaks.decaying_dbs_10x[ch] -= ms / 5; //(ATOMIC_GET(root->editonoff) ? 10 : 5);
    
    // falloff db
    if (reset_falloff || max_db > peaks.falloff_dbs[ch])
      peaks.falloff_dbs[ch] = max_db;

    if (max_db > peaks.peaks[ch])
      peaks.peaks[ch] = max_db;
  }
}

static void call_very_often(SoundPlugin *plugin, bool reset_falloff, int ms){
  call_very_often(plugin->volume_peaks, reset_falloff, ms);

  call_very_often(plugin->output_volume_peaks, reset_falloff, ms);

  call_very_often(plugin->input_volume_peaks, reset_falloff, ms);
  
  call_very_often(plugin->bus0_volume_peaks, reset_falloff, ms);
  call_very_often(plugin->bus1_volume_peaks, reset_falloff, ms);
  call_very_often(plugin->bus2_volume_peaks, reset_falloff, ms);
  call_very_often(plugin->bus3_volume_peaks, reset_falloff, ms);
  call_very_often(plugin->bus4_volume_peaks, reset_falloff, ms);

}

static const int g_falloff_reset = 5; // 5 seconds between each falloff reset.

/*
  what_to_update:
     -1 => Update all
      0 => First half
      1 => Second half
 */

static double g_last_time;

// NOTE. This function can be called from a custom exec().
void AUDIOMETERPEAKS_call_very_often(int what_to_update){
  static double time_since_last_reset_fallof = 0;

  double time = TIME_get_ms();
  double ms = time - g_last_time;

  if (ms < 10){ // in case qt bubbles up calls to timer callbacks. don't know if it will though, but the documentation is unclear. Really need a homemade and trusted timer.
#if !defined(RELEASE)
    printf("Note: AUDIOMETERPEAKS_call_very_often called less than 5ms since last time: %f\n", ms);
#endif
    return;
  }
  
  g_last_time = time;
  
  time_since_last_reset_fallof += ms;

  bool reset_falloff = false;

  if (time_since_last_reset_fallof >= 1000*g_falloff_reset){
    time_since_last_reset_fallof = 0;
    reset_falloff = true;
  }

  vector_t *audio_patches = &(get_audio_instrument()->patches);

  const int num_patches = audio_patches->num_elements;

  VECTOR_FOR_EACH(struct Patch *, patch, audio_patches){
    SoundPlugin *plugin = (SoundPlugin*)patch->patchdata;
    if(plugin != NULL){

      bool doit = true;

      int ms_to_use = what_to_update==-1 ? ms : ms*2;

      if (what_to_update==0 || what_to_update==1){
        if (what_to_update==0 && iterator666 >= int(num_patches/2))
          break;
        if (what_to_update==1 && !(iterator666 >= int(num_patches/2)))
          doit = false;
      }

      if (doit){
        //printf("%d: Updating %d / %d\n", what_to_update, iterator666, num_patches);
        call_very_often(plugin, reset_falloff, ms_to_use);
      }
    }
  }END_VECTOR_FOR_EACH;
}

AudioMeterPeaks AUDIOMETERPEAKS_create(int num_channels){
  AudioMeterPeaks peaks = {};

  peaks.num_channels = num_channels;

  ATOMIC_NAME(peaks.RT_max_gains) = (float*)V_calloc(num_channels, sizeof(float));
  peaks.max_dbs = (float*)V_calloc(num_channels, sizeof(float));
  peaks.decaying_dbs = (float*)V_calloc(num_channels, sizeof(float));
  peaks.decaying_dbs_10x = (float*)V_calloc(num_channels, sizeof(float));
  peaks.falloff_dbs = (float*)V_calloc(num_channels, sizeof(float));  
  peaks.peaks = (float*)V_calloc(num_channels, sizeof(float));  

  g_last_time = TIME_get_ms();

  for(int ch=0;ch<num_channels;ch++){
    peaks.max_dbs[ch] = UNTOUCHED_MAX_GAIN_VALUE;
    peaks.decaying_dbs[ch] = UNTOUCHED_MAX_GAIN_VALUE;
    peaks.decaying_dbs_10x[ch] = UNTOUCHED_MAX_GAIN_VALUE;
    peaks.falloff_dbs[ch] = UNTOUCHED_MAX_GAIN_VALUE;
    peaks.peaks[ch] = UNTOUCHED_MAX_GAIN_VALUE;
  }

  return peaks;
}

void AUDIOMETERPEAKS_delete(AudioMeterPeaks peaks){
  V_free(ATOMIC_NAME(peaks.RT_max_gains));
  V_free(peaks.max_dbs);
  V_free(peaks.decaying_dbs);
  V_free(peaks.decaying_dbs_10x);
  V_free(peaks.falloff_dbs);
  V_free(peaks.peaks);
}
