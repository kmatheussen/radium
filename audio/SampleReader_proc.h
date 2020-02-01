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

#if 1 //defined(RELEASE)
#  define SLICE_SIZE 4096 // in frames
#else
#  define SLICE_SIZE 3
#endif

enum WhatToDoWithDeletableFileWhenLoadingOrQuitting{
  WTT_DONT_KNOW, // When this is true, we are not loading or quitting and the user has not decided what to do about this file.
  WTT_DELETE, // when this is true, the user has decided that the file should be deleted. (note that we might not load or delete when we read this variable, but that should be okay)
  WTT_KEEP // when this is true, the user has decided that the file should NOT be deleted. (note that we might not load or delete when we read this variable, but that should be okay)
};

extern int g_sample_reader_filenames_generation;
extern LANGSPEC vector_t SAMPLEREADER_get_all_filenames(void);
extern LANGSPEC bool SAMPLEREADER_remove_filename_from_filenames(filepath_t filename); // returns true if filename was removed from filenames
extern LANGSPEC vector_t SAMPLEREADER_get_all_deletable_filenames(void);

extern LANGSPEC bool SAMPLEREADER_register_deletable_audio_file(filepath_t filename);
extern LANGSPEC bool SAMPLEREADER_is_deletable_audio_file(filepath_t filename);
extern LANGSPEC void SAMPLEREADER_mark_what_to_do_with_deletable_file_when_loading_or_quitting(filepath_t filename, enum WhatToDoWithDeletableFileWhenLoadingOrQuitting wtt);
extern LANGSPEC void SAMPLEREADER_maybe_make_audio_file_undeletable(filepath_t filename);
extern LANGSPEC void SAMPLEREADER_delete_all_deletable_audio_files(void);
extern LANGSPEC bool SAMPLEREADER_call_very_often(void);
extern LANGSPEC void SAMPLEREADER_inc_users(filepath_t filename);
extern LANGSPEC void SAMPLEREADER_dec_users(filepath_t filename);
extern LANGSPEC void SAMPLEREADER_dec_users_undo_callback(void *data);
extern LANGSPEC bool SAMPLEREADER_has_file(filepath_t filename); // can be called from any thread, but not while holding player lock.

extern LANGSPEC bool SAMPLEREADER_add_audiofile(filepath_t filename);
extern LANGSPEC int64_t SAMPLEREADER_get_sample_duration(filepath_t filename);
extern LANGSPEC double SAMPLEREADER_get_samplerate(filepath_t filename);

extern LANGSPEC unsigned int SAMPLEREADER_get_sample_color(filepath_t filename);
extern LANGSPEC void SAMPLEREADER_set_sample_color(filepath_t filename, unsigned int color);



#if __cplusplus

namespace radium{
  class FutureSignalTrackingSemaphore;
  class SampleReader;
}

extern radium::SampleReader *SAMPLEREADER_create(filepath_t filename);
extern void SAMPLEREADER_delete(radium::SampleReader *reader);

extern void SAMPLEREADER_prepare_to_play(radium::SampleReader *reader, int64_t pos, int64_t how_much_to_prepare, radium::FutureSignalTrackingSemaphore *gotit);

extern int64_t SAMPLEREADER_get_total_num_frames_in_sample(radium::SampleReader *reader);
extern void SAMPLEREADER_set_permanent_samples(radium::SampleReader *reader, int64_t first_sample, int64_t first_sample2);
extern int SAMPLEREADER_get_num_channels(radium::SampleReader *reader);
extern double SAMPLEREADER_get_samplerate(radium::SampleReader *reader);
extern filepath_t SAMPLEREADER_get_sample_name(radium::SampleReader *reader);
extern filepath_t SAMPLEREADER_get_filename(radium::SampleReader *reader);

extern bool RT_SAMPLEREADER_release_all_cached_data(radium::SampleReader *reader);
extern float *RT_SAMPLEREADER_get_buffer(radium::SampleReader *reader, const int ch, int &num_frames); // NOTE! num_frames can be set to a higher value than requested.
extern void RT_SAMPLEREADER_called_per_block(radium::SampleReader *reader, const int64_t how_much_to_prepare_for_next_time);
extern int RT_SAMPLEREADER_read(radium::SampleReader *reader, float **samples, int num_frames);
extern void SAMPLEREADER_shut_down(void);

#endif

#endif
