
#include <string.h>
#include <math.h>

#include "../common/nsmtracker.h"
#include "Smooth_proc.h"

static void init_it(Smooth *smooth, float value, int blocksize, int smooth_length){

  if(blocksize!=RADIUM_BLOCKSIZE){
    R_ASSERT(false);
    blocksize=RADIUM_BLOCKSIZE;
  }

#if (RADIUM_BLOCKSIZE!=DEFAULT_SMOOTH_LENGTH)
  if (smooth_length!=RADIUM_BLOCKSIZE && smooth_length!=DEFAULT_SMOOTH_LENGTH){
    R_ASSERT(false);
    smooth_length = DEFAULT_SMOOTH_LENGTH;
  }
#endif
  
  memset(smooth, 0, sizeof(Smooth));

  smooth->target_audio_will_be_modified = true;      

  smooth->smooth_length = smooth_length;
    
  //smooth->num_values = blocksize;
  smooth->values = V_calloc(sizeof(float), blocksize);
  
  SMOOTH_force_target_value(smooth, value);
}

void SMOOTH_init(Smooth *smooth, float value, int blocksize){
  init_it(smooth, value, blocksize, DEFAULT_SMOOTH_LENGTH);
}
void SMOOTH_init_immediate_smoothing(Smooth *smooth, float value, int blocksize){
  init_it(smooth, value, blocksize, RADIUM_BLOCKSIZE);
}

void SMOOTH_force_target_value(Smooth *smooth, float value){
  safe_volatile_float_write(&smooth->next_target_value, value);
  smooth->target_value = value;
  smooth->value = value;

  int i;
  for(i=0;i<RADIUM_BLOCKSIZE;i++)
    smooth->values[i] = value;
}

/*
void SMOOTH_new_blocksize(Smooth *smooth, int blocksize){
  smooth->values = V_realloc(smooth->values, sizeof(float)*blocksize); // this function is never called. blocksize is hardcoded to 64.
}
*/

void SMOOTH_release(Smooth *smooth){
  V_free(smooth->values);
}


// Can be called at any time
void SMOOTH_set_target_value(Smooth *smooth, float value){
  safe_volatile_float_write(&smooth->next_target_value, value);
}

float SMOOTH_get_target_value(const Smooth *smooth){
  return safe_volatile_float_read(&smooth->next_target_value);
}

void SMOOTH_update_target_audio_will_be_modified_value(Smooth *smooth){
  smooth->target_audio_will_be_modified = is_smoothing_necessary(smooth) ||
                                          smooth->value > 0.0f ||
                                          smooth->target_value != safe_volatile_float_read(&smooth->next_target_value);
}


// Must be called before processing a new block. (a Radium block, NOT a soundcard block)
//
void SMOOTH_called_per_block(Smooth *smooth){
  R_ASSERT_NON_RELEASE(smooth->target_audio_will_be_modified==true);
    
  //int num_values = smooth->num_values;

  float next_target_value = safe_volatile_float_read(&smooth->next_target_value); // Only one read. smooth->next_target_value can be written at any time from any thread.

  if(smooth->value == next_target_value && smooth->pos==0){

    if(smooth->smoothing_is_necessary==true){
      int i;
      for(i=0;i<RADIUM_BLOCKSIZE;i++) // shouldn't be necessary.
        smooth->values[i] = next_target_value;

      smooth->smoothing_is_necessary = false;
    }

  }else{    
    smooth->smoothing_is_necessary = true;

    //printf("pos: %d, start: %f, current_target: %f, target: %f\n",smooth->pos,smooth->start_value,smooth->current_target_value,target_value);

    if(smooth->pos==0)
      smooth->target_value = next_target_value;

    const float start_value = smooth->value;
    const float end_value = smooth->target_value;
    
    if(smooth->smooth_length==RADIUM_BLOCKSIZE) {

      R_ASSERT_RETURN_IF_FALSE(smooth->pos==0);
      
      for(int i=0;i<RADIUM_BLOCKSIZE;i++){
        smooth->values[i] = scale(i,
                                  0, RADIUM_BLOCKSIZE,
                                  start_value, end_value);
      }

    } else {

      R_ASSERT_NON_RELEASE(smooth->smooth_length==DEFAULT_SMOOTH_LENGTH);
      
      for(int i=0;i<RADIUM_BLOCKSIZE;i++){
        smooth->values[i] = scale(smooth->pos + i,
                                  0, DEFAULT_SMOOTH_LENGTH,
                                  start_value, end_value);
      }
    }
    
    smooth->pos += RADIUM_BLOCKSIZE;
    if(smooth->pos==smooth->smooth_length){
      smooth->value = smooth->target_value;
      smooth->pos = 0;
    }
  }
}

void SMOOTH_apply_volume(const Smooth *smooth, float *sound, int num_frames){
  R_ASSERT(smooth->target_audio_will_be_modified==true);
    
  int i;
  if(is_smoothing_necessary(smooth)==true){
    float *values = smooth->values;
    for(i=0;i<num_frames;i++){
      //if(smooth->last_target_value==0.0f || smooth->target_value==0.0f)
      //  printf("val %d: %f\n",i,values[i]);
      sound[i] *= values[i];
    }
  }else{
    float volume = smooth->value; // might be a click here if volume is changed between the is_smoothing test and here, but I guess it is extremely unlikely to happen. (edited much later: I guess "target_value" was introduced to eliminate this possibility, and perhaps also because "value" is not an atomic variable.)
    if(volume != 1.0f)
      for(i=0;i<num_frames;i++)
        sound[i] *= volume;
  }
}


// Note: SMOOTH_copy_sound has been moved to Juce_plugins.cpp in order to use the optimized vector functions in Juce.
// (SMOOTH_copy_sound uses a significant amount of CPU when there's many instruments.)

  
#if 0
void SMOOTH_apply_volume_using_inverted_values(const Smooth *smooth, float *sound, int num_frames){
  R_ASSERT(smooth->target_audio_will_be_modified==true);
  
  int i;
  if(is_smoothing_necessary(smooth)==true){
    float *values = smooth->values;
    for(i=0;i<num_frames;i++)
      sound[i] *= (1.0f-values[i]);
  }else{
    float volume = 1.0f - smooth->value; // might be a click here if target_value is changed between the is_smoothing test and here, but I guess it is extremely unlikely to happen.
    if(volume != 1.0f)
      for(i=0;i<num_frames;i++)
        sound[i] *= volume;
  }
}
#endif

void SMOOTH_mix_sounds_raw(float *target, const float *source, int num_frames, float start_volume, float end_volume){

  if (num_frames==0) {
#if defined(RELEASE)
    return;
#else
    abort();
#endif
  }
  
  int i;
  float diff = end_volume - start_volume;

  if(fabsf(diff) < 0.0005){ 

    for(i=0;i<num_frames;i++)
      target[i] += source[i] * end_volume;
   
  }else{
    float val = start_volume;
    float inc = diff/num_frames;

    for(i=0;i<num_frames;i++){
      target[i] += source[i] * val;
      val += inc;
    }

    //printf("Start: %f. End: %f. inc: %f. End: %f\n\n",start_volume,end_volume,inc, val);
  }
}


bool SMOOTH_are_we_going_to_modify_target_when_mixing_sounds_questionmark(const Smooth *smooth){
  return smooth->target_audio_will_be_modified;
}


void SMOOTH_mix_sounds(const Smooth *smooth, float *target, const float *source, int num_frames){
  R_ASSERT(smooth->target_audio_will_be_modified==true);
  
  int i;
  if(is_smoothing_necessary(smooth)==true){
    float *values = smooth->values;
    for(i=0;i<num_frames;i++)
      target[i] += source[i] * values[i];
  }else{
    //printf("%p smooth->get: %f, smooth->set: %f. start: %f, end: %f\n",smooth,smooth->get,smooth->set,smooth->start_value,smooth->end_value);
    float volume = smooth->value;
    
    if(volume == 1.0f)
      for(i=0;i<num_frames;i++)
        target[i] += source[i];
    
    else if(volume > 0.0f)
      for(i=0;i<num_frames;i++)
        target[i] += source[i] * volume;
  }
}

void SMOOTH_mix_sounds_from_mono_to_stereo(const Smooth *smooth, float *target_ch0, float *target_ch1, const float *source, int num_frames){
  R_ASSERT(smooth->target_audio_will_be_modified==true);
  
  int i;
  if(is_smoothing_necessary(smooth)==true){
    float *values = smooth->values;
    for(i=0;i<num_frames;i++){
      float sample = source[i] * values[i];
      target_ch0[i] += sample;
      target_ch1[i] += sample;
    }
  }else{
    //printf("%p smooth->get: %f, smooth->set: %f. start: %f, end: %f\n",smooth,smooth->get,smooth->set,smooth->start_value,smooth->end_value);
    float volume = smooth->value;
    
    if(volume == 1.0f)
      for(i=0;i<num_frames;i++){
        float sample = source[i];
        target_ch0[i] += sample;
        target_ch1[i] += sample;
      }
    
    else if(volume > 0.0f)
      for(i=0;i<num_frames;i++){
        float sample = source[i] * volume;
        target_ch0[i] += sample;
        target_ch1[i] += sample;
      }
  }
}

void SMOOTH_mix_sounds_using_inverted_values(const Smooth *smooth, float *wet, const float *dry, int num_frames){
  R_ASSERT(smooth->target_audio_will_be_modified==true);
  
  int i;
  if(is_smoothing_necessary(smooth)==true){
    float *values = smooth->values;
    for(i=0;i<num_frames;i++)
      wet[i] += dry[i] * (1.0f-values[i]);
  }else{
    //printf("%p smooth->get: %f, smooth->set: %f. start: %f, end: %f\n",smooth,smooth->get,smooth->set,smooth->start_value,smooth->end_value);
    float volume = (1.0f-smooth->value);
    if(volume == 1.0f)
      for(i=0;i<num_frames;i++)
        wet[i] += dry[i];
    else if(volume > 0.0f)
      for(i=0;i<num_frames;i++)
        wet[i] += dry[i] * volume;
  }
}

#if 1
// Something in get_pan_vals_vector crashes on windows XP. It's hard to reproduce, but doesn't seem to happen very often (or at all) if the function is not inlined.
static Panvals das_get_pan_vals_vector(float pan, int num_source_channels){
  Panvals p;

  const float sqrt2              = 1.414213562373095;
  const float pan_scaleval       = 2.0f-(2*sqrt2);

  if(num_source_channels==1){

    float x=scale(pan,-1,1,0,1);
    p.vals[0][0] = ((1.0f-x)*((pan_scaleval*(1.0f-x))+(1.0f-pan_scaleval)));
    p.vals[0][1] = x * ( (pan_scaleval*x) + (1.0f-pan_scaleval));

  }else{

    if(pan<=0){
      //fprintf(stderr,"p1\n");
      p.vals[0][0] = 1.0f;
      p.vals[0][1] = 0.0f;
      
      pan=scale(pan,-1,0,-1,1);
      float x=scale(pan,-1,1,0,1);
      //fprintf(stderr,"p2\n");
      p.vals[1][0] = ((1.0f-x)*((pan_scaleval*(1.0f-x))+(1.0f-pan_scaleval)));
      //fprintf(stderr,"p3\n");
      p.vals[1][1] = x * ( (pan_scaleval*x) + (1.0f-pan_scaleval));
      //fprintf(stderr,"p4\n");
      //System.out.println("l/r/pan "+p.vals[1][0]+" "+p.vals[1][1]+" "+pan);

    }else{
      //fprintf(stderr,"p5\n");
      pan=scale(pan,0,1,-1,1);
      float x=scale(pan,-1,1,0,1);
      //fprintf(stderr,"p6\n");
      p.vals[0][0] = ((1.0f-x)*((pan_scaleval*(1.0f-x))+(1.0f-pan_scaleval)));
      //fprintf(stderr,"p7\n");
      p.vals[0][1] = x * ( (pan_scaleval*x) + (1.0f-pan_scaleval));
      //fprintf(stderr,"p8\n");

      p.vals[1][0] = 0.0f;
      p.vals[1][1] = 1.0f;
      //System.out.println("l/r/pan "+p.vals[0][0]+" "+p.vals[0][1]+" "+pan);

    }
  }

  return p;
}
#endif

// Think I found this pan calculation method in the ardour source many years ago.
// TODO: Optimize panning when smoothing is necessary.
void SMOOTH_apply_pan(const Smooth *smooth, float **sound, int num_channels, int num_frames){
  R_ASSERT(smooth->target_audio_will_be_modified==true);
  
  int i;
  if(num_channels>=2){
    float *sound0 = sound[0];
    float *sound1 = sound[1];

    if(is_smoothing_necessary(smooth)==true){

      //fprintf(stderr,"s1\n");
      Panvals pan_start = das_get_pan_vals_vector(scale(smooth->values[0],0,1,-1,1),2);
      //fprintf(stderr,"s2\n");
      Panvals pan_end   = das_get_pan_vals_vector(scale(smooth->values[num_frames-1],0,1,-1,1),2);
      //fprintf(stderr,"s3\n");

      for(i=0;i<num_frames;i++) {
        float sound0i = sound0[i];
        float sound1i = sound1[i];
        sound0[i] = sound0i*scale(i,0,num_frames,pan_start.vals[0][0],pan_end.vals[0][0]) + sound1i*scale(i,0,num_frames,pan_start.vals[1][0],pan_end.vals[1][0]);
        sound1[i] = sound0i*scale(i,0,num_frames,pan_start.vals[0][1],pan_end.vals[0][1]) + sound1i*scale(i,0,num_frames,pan_start.vals[1][1],pan_end.vals[1][1]);
      }

    } else {

      if(smooth->value!=0.5f){
        Panvals pan = get_pan_vals_vector(scale(smooth->value,0,1,-1,1), 2);

#if 1
        if(smooth->value < 0.5f){

          for(i=0;i<num_frames;i++)
            sound0[i] += sound1[i]*pan.vals[1][0];

          for(i=0;i<num_frames;i++)
            sound1[i] *= pan.vals[1][1];

        } else {

          for(i=0;i<num_frames;i++)
            sound1[i] += sound0[i]*pan.vals[0][1];

          for(i=0;i<num_frames;i++)
            sound0[i] *= pan.vals[0][0];

        }
#else
        // Same, but perhaps a bit slower.
        for(i=0;i<num_frames;i++) {
          float sound0i = sound0[i];
          float sound1i = sound1[i];
          sound0[i] = sound0i*pan.vals[0][0] + sound1i*pan.vals[1][0];
          sound1[i] = sound0i*pan.vals[0][1] + sound1i*pan.vals[1][1];
        }
#endif
      }
    }
  }
}


