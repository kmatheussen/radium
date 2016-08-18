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


extern LANGSPEC bool SAMPLER_set_new_sample(struct SoundPlugin *plugin, const wchar_t *filename, int instrument_number);
extern LANGSPEC void SAMPLER_set_loop_data(struct SoundPlugin *plugin, int start, int length);
extern LANGSPEC bool SAMPLER_set_resampler_type(struct SoundPlugin *plugin, int resampler_type);
extern LANGSPEC int SAMPLER_get_resampler_type(struct SoundPlugin *plugin);
extern LANGSPEC void SAMPLER_save_sample(struct SoundPlugin *plugin, const wchar_t *filename, int sample_number);
extern LANGSPEC void SAMPLER_start_recording(struct SoundPlugin *plugin, const wchar_t *pathdir, int num_channels, bool recording_from_main_input);
extern LANGSPEC const char *SAMPLER_get_recording_status(struct SoundPlugin *plugin);
extern LANGSPEC const wchar_t *SAMPLER_get_filename(struct SoundPlugin *plugin, bool *is_default_sound);
extern LANGSPEC const wchar_t *SAMPLER_get_filename_display(struct SoundPlugin *plugin);

