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

enum{
  COMP_EFF_RATIO=0,
  COMP_EFF_THRESHOLD,
  COMP_EFF_ATTACK,
  COMP_EFF_RELEASE,
  //INPUT_VOLUME,
  COMP_EFF_OUTPUT_VOLUME,
  COMP_EFF_BYPASS // only used by the standalone version of the compressor.
};


extern LANGSPEC void *COMPRESSOR_create(float sample_rate);

extern LANGSPEC void COMPRESSOR_delete(void *das_wrapper);

extern LANGSPEC float COMPRESSOR_get_parameter(void *das_wrapper,int num);

extern LANGSPEC void COMPRESSOR_set_parameter(void *das_wrapper,int num,float value);

extern LANGSPEC float COMPRESSOR_get_graph_value(void *das_wrapper, int num);

extern LANGSPEC void COMPRESSOR_process(void *das_wrapper, float **inputs, float **outputs, int num_frames);
