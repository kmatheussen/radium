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


extern LANGSPEC void *ENVELOPE_create(int num_breaks, float samplerate);
extern LANGSPEC void ENVELOPE_delete(void *env);

extern LANGSPEC void ENVELOPE_setBreaks(void *env, int len, double *x, double *y);

extern LANGSPEC void ENVELOPE_set_freeze_breakpoint(void *env,int breakpoint);

extern LANGSPEC void ENVELOPE_unfreeze(void *env);
extern LANGSPEC void ENVELOPE_reset(void *env); // RT safe function. Call before starting to apply envelope from the start.

extern LANGSPEC int ENVELOPE_apply(void *env, float **buf, int num_channels, int num_frames);

extern LANGSPEC void *ADSR_create(float samplerate);
extern LANGSPEC void ADSR_delete(void *adsr);
  
extern LANGSPEC int ADSR_apply(void *adsr, float **buf, int num_channels, int num_frames);

extern LANGSPEC void ADSR_release(void *adsr);

extern LANGSPEC void ADSR_reset(void *adsr);

extern LANGSPEC void ADSR_set_adsr(void *adsr, float a, float h, float d, float s, float r);
