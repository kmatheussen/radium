#ifndef _RADIUM_AUDIO_SAMPLEREADER_PROC_H
#define _RADIUM_AUDIO_SAMPLEREADER_PROC_H

#if defined(RELEASE)
#  define SLICE_SIZE 4096 // in frames
#else
#  define SLICE_SIZE 3
#endif


extern LANGSPEC int64_t SAMPLEREADER_get_sample_duration(const wchar_t *filename);


#if __cplusplus

namespace radium{
  class FutureSignalTrackingSemaphore;
  struct SampleReader;
}

extern radium::SampleReader *SAMPLEREADER_create(const wchar_t *filename);
extern void SAMPLEREADER_delete(radium::SampleReader *reader);

extern void SAMPLEREADER_prepare_to_play(radium::SampleReader *reader, int64_t pos, int64_t how_much_to_prepare, radium::FutureSignalTrackingSemaphore *gotit);

extern int64_t SAMPLEREADER_get_num_frames(radium::SampleReader *reader);
extern int SAMPLEREADER_get_num_channels(radium::SampleReader *reader);
extern const wchar_t *SAMPLEREADER_get_sample_name(radium::SampleReader *reader);
extern unsigned int SAMPLEREADER_get_sample_color(radium::SampleReader *reader);

extern void RT_SAMPLEREADER_release_all_cached_data(radium::SampleReader *reader);
extern float *RT_SAMPLEREADER_get_buffer(radium::SampleReader *reader, const int ch, int &num_frames);
extern void RT_SAMPLEREADER_called_per_block(radium::SampleReader *reader, const int64_t how_much_to_prepare_for_next_time);
extern int RT_SAMPLEREADER_read(radium::SampleReader *reader, float **samples, int num_frames, bool do_add);
extern void SAMPLEREADER_shut_down(void);

#endif

#endif
