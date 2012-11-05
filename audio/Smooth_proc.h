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



typedef struct{
  float* values;

  volatile float target_value;

  float start_value;
  float end_value;

  int num_values;

  bool smoothing_is_necessary;
} Smooth;

extern LANGSPEC void SMOOTH_init(Smooth *smooth, float value, int blocksize);
extern LANGSPEC void SMOOTH_new_blocksize(Smooth *smooth, int blockframes);
extern LANGSPEC void SMOOTH_release(Smooth *smooth);
extern LANGSPEC void SMOOTH_set_target_value(Smooth *smooth, float value);
extern LANGSPEC float SMOOTH_get_target_value(Smooth *smooth);
extern LANGSPEC void SMOOTH_called_per_block(Smooth *smooth);
extern LANGSPEC void SMOOTH_apply_volume(Smooth *smooth, float *sound, int num_frames);
extern LANGSPEC void SMOOTH_apply_inverted_volume(Smooth *smooth, float *sound, int num_frames);
extern LANGSPEC void SMOOTH_copy_sound(Smooth *smooth, float *dst, float *src, int num_frames);
extern LANGSPEC void SMOOTH_mix_sounds_raw(float *target, float *source, int num_frames, float start_volume, float end_volume);
extern LANGSPEC void SMOOTH_mix_sounds(Smooth *smooth, float *target, float *source, int num_frames);
extern LANGSPEC void SMOOTH_mix_sounds_using_inverted_values(Smooth *smooth, float *target, float *source, int num_frames);
extern LANGSPEC void SMOOTH_apply_pan(Smooth *smooth, float **sound, int num_channels, int num_frames);
