
#include <string.h>
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
    if(volume > 0.0f)
      for(i=0;i<num_frames;i++)
        dst[i] = src[i] * volume;
    else
      memset(dst,0,sizeof(float)*num_frames);
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
// TODO: Optimize panning when smoothing is necessary.
void SMOOTH_apply_pan(Smooth *smooth, float **sound, int num_channels, int num_frames){

  int i;
  if(num_channels>=2){
    float *sound0 = sound[0];
    float *sound1 = sound[1];

    if(is_smoothing_necessary(smooth)==true){

      Panvals pan_start = get_pan_vals_vector(scale(smooth->values[0],0,1,-1,1),2);
      Panvals pan_end   = get_pan_vals_vector(scale(smooth->values[num_frames-1],0,1,-1,1),2);

      for(i=0;i<num_frames;i++) {
        float sound0i = sound0[i];
        float sound1i = sound1[i];
        sound0[i] = sound0i*scale(i,0,num_frames,pan_start.vals[0][0],pan_end.vals[0][0]) + sound1i*scale(i,0,num_frames,pan_start.vals[1][0],pan_end.vals[1][0]);
        sound1[i] = sound0i*scale(i,0,num_frames,pan_start.vals[0][1],pan_end.vals[0][1]) + sound1i*scale(i,0,num_frames,pan_start.vals[1][1],pan_end.vals[1][1]);
      }

    } else {

      if(smooth->end_value!=0.5f){
        Panvals pan = get_pan_vals_vector(scale(smooth->end_value,0,1,-1,1), 2);

#if 1
        if(smooth->end_value < 0.5f){

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


