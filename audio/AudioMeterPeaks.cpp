
#include <math.h>
#include <atomic>

#include "../common/nsmtracker.h"
#include "../common/instruments_proc.h"

#include "../audio/SoundPlugin.h"
#include "../audio/Mixer_proc.h"


#include "AudioMeterPeaks_proc.h"


// NOTE. All the 'call_very_often' functions can be called from a custom exec().
// This means that _patch->plugin might be gone, and the same goes for soundproducer.
// (Note that _patch is never deleted, except when loading song.)

static void call_very_often(AudioMeterPeaks &peaks, bool reset_falloff, float ms){

  for(int ch=0;ch<peaks.num_channels;ch++){

    float max_gain;

    float *pos = &ATOMIC_NAME(peaks.RT_max_gains)[ch];

    // Atomically read and write RT_max_gains[ch]
    //
    
    for(int i = 0 ; ; i++){
      
      max_gain = atomic_get_float_relaxed(pos);
      
      R_ASSERT_NON_RELEASE(max_gain >= 0.0f);

      if (equal_floats(max_gain, 0.0f))
        break;
      
      if (atomic_compare_and_set_float(pos, max_gain, 0.0f))
        break;
      
      // Give up after 64 tries. Should not happen, but just in case.
      // (also note that RT_max_gains[ch] is not resetted when we fail, so we don't destroy max peak detection, which is most important)
      if(i==64){
#if !defined(RELEASE)
        printf("\n\n\n........................    Warning: Giving up getting max gain ...........\n\n\n\n");
#endif        
        return;
      }

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

// NOTE. This function can be called from a custom exec().
void AUDIOMETERPEAKS_call_very_often(int what_to_update){
  
  static int64_t s_last_mixer_time = MIXER_get_last_used_time();

  int64_t mixer_time = MIXER_get_last_used_time();

  if (mixer_time+(RADIUM_BLOCKSIZE*3) <= s_last_mixer_time) // must add some radium_bloksize as well to ensure a cycle really has had time to run.
    return;
  
  static double time_since_last_reset_falloff = 0;

  double ms = 1000.0 * (double)(mixer_time - s_last_mixer_time) / (double)pc->pfreq;

  s_last_mixer_time = mixer_time;
  
  time_since_last_reset_falloff += ms;

  bool reset_falloff = false;

  if (time_since_last_reset_falloff >= 1000*g_falloff_reset){
    time_since_last_reset_falloff = 0;
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

  for(int ch=0;ch<num_channels;ch++){
    peaks.max_dbs[ch] = MIN_DB;
    peaks.decaying_dbs[ch] = MIN_DB;
    peaks.decaying_dbs_10x[ch] = MIN_DB;
    peaks.falloff_dbs[ch] = MIN_DB;
    peaks.peaks[ch] = MIN_DB;
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
