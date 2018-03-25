/* Copyright 2017-2018 Kjetil S. Matheussen

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


#ifndef _RADIUM_AUDIO_SAMPLEREADER_PROC_H
#define _RADIUM_AUDIO_SAMPLEREADER_PROC_H

#if defined(RELEASE)
#  define SLICE_SIZE 4096 // in frames
#else
#  define SLICE_SIZE 3
#endif


extern LANGSPEC int64_t SAMPLEREADER_get_sample_duration(const wchar_t *filename);

extern LANGSPEC unsigned int SAMPLEREADER_get_sample_color(const wchar_t *filename);
extern LANGSPEC void SAMPLEREADER_set_sample_color(const wchar_t *filename, unsigned int color);



#if __cplusplus

namespace radium{
  class FutureSignalTrackingSemaphore;
  class SampleReader;
}

extern radium::SampleReader *SAMPLEREADER_create(const wchar_t *filename);
extern void SAMPLEREADER_delete(radium::SampleReader *reader);

extern void SAMPLEREADER_prepare_to_play(radium::SampleReader *reader, int64_t pos, int64_t how_much_to_prepare, radium::FutureSignalTrackingSemaphore *gotit);

extern int64_t SAMPLEREADER_get_total_num_frames_in_sample(radium::SampleReader *reader);
extern void SAMPLEREADER_set_permanent_samples(radium::SampleReader *reader, int64_t first_sample, int64_t first_sample2);
extern int SAMPLEREADER_get_num_channels(radium::SampleReader *reader);
extern double SAMPLEREADER_get_samplerate(radium::SampleReader *reader);
extern const wchar_t *SAMPLEREADER_get_sample_name(radium::SampleReader *reader);
extern const wchar_t *SAMPLEREADER_get_filename(radium::SampleReader *reader);

extern bool RT_SAMPLEREADER_release_all_cached_data(radium::SampleReader *reader);
extern float *RT_SAMPLEREADER_get_buffer(radium::SampleReader *reader, const int ch, int &num_frames);
extern void RT_SAMPLEREADER_called_per_block(radium::SampleReader *reader, const int64_t how_much_to_prepare_for_next_time);
extern int RT_SAMPLEREADER_read(radium::SampleReader *reader, float **samples, int num_frames);
extern void SAMPLEREADER_shut_down(void);

#endif

#endif
