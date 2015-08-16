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

#ifndef AUDIO_SMOOTH_PROC_H
#define AUDIO_SMOOTH_PROC_H

// Must be a multiple of RADIUM_BLOCKSIZE
#define SMOOTH_LENGTH 1024

typedef struct{
  float* values;

  volatile float next_target_value;
  float target_value;
  float value;

  int num_values; // same as RADIUM_BLOCKSIZE. (i.e. 64)
  int pos; // between 0 and SMOOTH_LENGTH (in intervals of 64)

  bool smoothing_is_necessary;
  volatile bool target_audio_will_be_modified;
} Smooth;

typedef struct{
  float vals [2][2];
} Panvals;



// Copied from https://github.com/kmatheussen/soundengine/blob/master/SoundObject.java (written by me)
//
static inline Panvals get_pan_vals_vector(float pan, int num_source_channels){
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


extern LANGSPEC void SMOOTH_init(Smooth *smooth, float value, int blocksize);
extern LANGSPEC void SMOOTH_new_blocksize(Smooth *smooth, int blockframes);
extern LANGSPEC void SMOOTH_release(Smooth *smooth);
extern LANGSPEC void SMOOTH_set_target_value(Smooth *smooth, float value);
extern LANGSPEC float SMOOTH_get_target_value(Smooth *smooth);
extern LANGSPEC void SMOOTH_update_target_audio_will_be_modified_value(Smooth *smooth);
extern LANGSPEC void SMOOTH_called_per_block(Smooth *smooth);
extern LANGSPEC void SMOOTH_apply_volume(Smooth *smooth, float *sound, int num_frames);
extern LANGSPEC void SMOOTH_apply_inverted_volume(Smooth *smooth, float *sound, int num_frames);
extern LANGSPEC void SMOOTH_copy_sound(Smooth *smooth, float *dst, float *src, int num_frames);
extern LANGSPEC void SMOOTH_mix_sounds_raw(float *target, float *source, int num_frames, float start_volume, float end_volume);
extern LANGSPEC bool SMOOTH_are_we_going_to_modify_target_when_mixing_sounds_questionmark(Smooth *smooth);
extern LANGSPEC void SMOOTH_mix_sounds(Smooth *smooth, float *target, float *source, int num_frames);
extern LANGSPEC void SMOOTH_mix_sounds_using_inverted_values(Smooth *smooth, float *target, float *source, int num_frames);
extern LANGSPEC void SMOOTH_apply_pan(Smooth *smooth, float **sound, int num_channels, int num_frames);

#endif // AUDIO_SMOOTH_PROC_H
