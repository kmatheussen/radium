
#include <math.h>

#include "../common/nsmtracker.h"
#include "Smooth_proc.h"

void SMOOTH_init(Smooth *smooth, float value, int blocksize){

  smooth->target_value = value;
  smooth->start_value = value;
  smooth->end_value = value;

  smooth->num_values = blocksize;
  smooth->values = malloc(sizeof(float)*blocksize);

  int i;
  for(i=0;i<blocksize;i++)
    smooth->values[i] = value;
}

void SMOOTH_new_blocksize(Smooth *smooth, int blocksize){
  smooth->values = realloc(smooth->values, sizeof(float)*blocksize);
}

void SMOOTH_release(Smooth *smooth){
  free(smooth->values);
}

static bool is_smoothing_necessary(Smooth *smooth){
  return smooth->smoothing_is_necessary;
#if 0
  if(smooth->last_target_value==smooth->target_value)
    return false;
  else
    return true;
#endif

#if 0
  else if(fabsf(smooth->end_value - smooth->start_value)<0.0005){
    smooth->get = smooth->set;
    return false;
  }else
    return true;
#endif
}

// Can be called at any time
void SMOOTH_set_target_value(Smooth *smooth, float value){
  smooth->target_value = value;
}

float SMOOTH_get_target_value(Smooth *smooth){
  return smooth->target_value;
}

// Must be called before processing a new block.
void SMOOTH_called_per_block(Smooth *smooth){
  int num_values = smooth->num_values;

  float target_value = smooth->target_value; // Only one read. smooth->target_value can be written at any time from any thread.

  if(smooth->end_value == target_value){
    smooth->start_value = smooth->end_value;
    smooth->smoothing_is_necessary = false;
    int i;    
    for(i=0;i<num_values;i++)
      smooth->values[i] = target_value;
  }else{
    smooth->smoothing_is_necessary = true;

    smooth->start_value = smooth->end_value;
    smooth->end_value = target_value;

    float val = smooth->start_value;
    float inc = (smooth->end_value - smooth->start_value) / num_values;
    int i;
    
    for(i=0;i<num_values;i++){
      smooth->values[i] = val;
      val += inc;
    }
  }
}

void SMOOTH_apply_volume(Smooth *smooth, float *sound, int num_frames){
  int i;
  if(is_smoothing_necessary(smooth)==true){
    float *values = smooth->values;
    for(i=0;i<num_frames;i++){
      //if(smooth->last_target_value==0.0f || smooth->target_value==0.0f)
      //  printf("val %d: %f\n",i,values[i]);
      sound[i] *= values[i];
    }
  }else{
    float volume = smooth->end_value; // might be a click here if volume is changed between the is_smoothing test and here, but I guess it is extremely unlikely to happen.
    if(volume != 1.0f)
      for(i=0;i<num_frames;i++)
        sound[i] *= volume;
  }
}

void SMOOTH_copy_sound(Smooth *smooth, float *dst, float *src, int num_frames){
  int i;
  if(is_smoothing_necessary(smooth)==true){
    float *values = smooth->values;
    for(i=0;i<num_frames;i++){
      //if(smooth->last_target_value==0.0f || smooth->target_value==0.0f)
      //  printf("val %d: %f\n",i,values[i]);
      dst[i] = src[i] * values[i];
    }
  }else{
    float volume = smooth->end_value; // might be a click here if volume is changed between the is_smoothing test and here, but I guess it is extremely unlikely to happen.
    if(volume != 1.0f)
      for(i=0;i<num_frames;i++)
        dst[i] = src[i] * volume;
  }
}

void SMOOTH_apply_volume_using_inverted_values(Smooth *smooth, float *sound, int num_frames){
  int i;
  if(is_smoothing_necessary(smooth)==true){
    float *values = smooth->values;
    for(i=0;i<num_frames;i++)
      sound[i] *= (1.0f-values[i]);
  }else{
    float volume = 1.0f - smooth->end_value; // might be a click here if target_value is changed between the is_smoothing test and here, but I guess it is extremely unlikely to happen.
    if(volume != 1.0f)
      for(i=0;i<num_frames;i++)
        sound[i] *= volume;
  }
}

void SMOOTH_mix_sounds_raw(float *target, float *source, int num_frames, float start_volume, float end_volume){
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

void SMOOTH_mix_sounds(Smooth *smooth, float *target, float *source, int num_frames){
  int i;
  if(is_smoothing_necessary(smooth)==true){
    float *values = smooth->values;
    for(i=0;i<num_frames;i++)
      target[i] += source[i] * values[i];
  }else{
    //printf("%p smooth->get: %f, smooth->set: %f. start: %f, end: %f\n",smooth,smooth->get,smooth->set,smooth->start_value,smooth->end_value);
    float volume = smooth->end_value;
    if(volume == 1.0f)
      for(i=0;i<num_frames;i++)
        target[i] += source[i];
    else if(volume > 0.0f)
      for(i=0;i<num_frames;i++)
        target[i] += source[i] * volume;
  }
}

void SMOOTH_mix_sounds_using_inverted_values(Smooth *smooth, float *target, float *source, int num_frames){
  int i;
  if(is_smoothing_necessary(smooth)==true){
    float *values = smooth->values;
    for(i=0;i<num_frames;i++)
      target[i] += source[i] * (1.0f-values[i]);
  }else{
    //printf("%p smooth->get: %f, smooth->set: %f. start: %f, end: %f\n",smooth,smooth->get,smooth->set,smooth->start_value,smooth->end_value);
    float volume = (1.0f-smooth->end_value);
    if(volume == 1.0f)
      for(i=0;i<num_frames;i++)
        target[i] += source[i];
    else if(volume > 0.0f)
      for(i=0;i<num_frames;i++)
        target[i] += source[i] * volume;
  }
}

// Think I found this pan calculation method in the ardour source many years ago.
// TODO: Panning can be optimized by only calculating new pan values at block time, and do linear smoothing inbetween.
void SMOOTH_apply_pan(Smooth *smooth, float **sound, int num_channels, int num_frames){
  const float sqrt2              = 1.414213562373095;
  const float pan_scaleval       = 2.0f-(2*sqrt2);
  const float one_minus_panscale = 1.0f-pan_scaleval;

  int i;
  if(num_channels>=2){
    float *sound0 = sound[0];
    float *sound1 = sound[1];

    if(is_smoothing_necessary(smooth)==true){

      for(i=0;i<num_frames;i++){
        float x = smooth->values[i];
        float one_minus_x = 1.0f-x;
        
        float pan_ch_0 = one_minus_x*((pan_scaleval*one_minus_x)+one_minus_panscale);
        float pan_ch_1 = x * ( (pan_scaleval*x) + one_minus_panscale);
        
        sound0[i] *= pan_ch_0;
        sound1[i] *= pan_ch_1;
      }

    } else {

      float x = smooth->end_value;
      float one_minus_x = 1.0f-x;
        
      float pan_ch_0 = one_minus_x*((pan_scaleval*one_minus_x)+one_minus_panscale);
      float pan_ch_1 = x * ( (pan_scaleval*x) + one_minus_panscale);

      if(pan_ch_0 != 1.0f)
        for(i=0;i<num_frames;i++)
          sound0[i] *= pan_ch_0;

      if(pan_ch_1 != 1.0f)
        for(i=0;i<num_frames;i++)
          sound1[i] *= pan_ch_1;
    }
  }
}


