/* Copyright 2013 Kjetil S. Matheussen

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

#ifndef SYSTEM_COMPRESSOR_WRAPPER_PROC_H
#define SYSTEM_COMPRESSOR_WRAPPER_PROC_H

enum{
  COMP_EFF_RATIO=0,
  COMP_EFF_THRESHOLD,
  COMP_EFF_ATTACK,
  COMP_EFF_RELEASE,
  //INPUT_VOLUME,
  COMP_EFF_OUTPUT_VOLUME,
  COMP_EFF_BYPASS, // only used by the standalone version of the compressor, and the vst plugin version.
  COMP_EFF_NUM_PARAMETERS
};

#ifdef __cplusplus
extern "C"{
#endif

extern void *COMPRESSOR_create(float sample_rate);

extern void COMPRESSOR_delete(void *das_wrapper);

extern float COMPRESSOR_get_parameter(void *das_wrapper,int num);

extern void COMPRESSOR_set_parameter(void *das_wrapper,int num,float value);

extern float COMPRESSOR_get_graph_value(void *das_wrapper, int num);

extern void COMPRESSOR_process(void *das_wrapper, float **inputs, float **outputs, int num_frames);

extern void *COMPRESSOR_create_ladspa(const char *key);
extern float COMPRESSOR_get_ladspa_parameter(void *Instance,int num);
extern void COMPRESSOR_set_ladspa_parameter(void *Instance,int num,float value);
extern float COMPRESSOR_get_ladspa_graph_value(void *Instance, int num);
extern void COMPRESSOR_delete_ladspa(void *Instance);


#ifdef __cplusplus
}
#endif

#endif // SYSTEM_COMPRESSOR_WRAPPER_PROC_H
