#ifndef _RADIUM_AUDIO_SAMPLEREADER_PROC_H
#define _RADIUM_AUDIO_SAMPLEREADER_PROC_H

namespace radium{
  struct SampleReader;
}

extern radium::SampleReader *SAMPLEREADER_create(const wchar_t *filename);
extern void SAMPLEREADER_delete(radium::SampleReader *reader);

extern int SAMPLEREADER_get_num_channels(radium::SampleReader *reader);
extern int64_t SAMPLEREADER_get_num_frames(radium::SampleReader *reader);

extern void SAMPLEREADER_prepare_to_play(radium::SampleReader *reader, int64_t pos, int64_t how_much_to_prepare); // Waits until 'how_much_to_prepare' frames have been loaded into memory.

extern int SAMPLEREADER_get_num_channels(radium::SampleReader *reader);
extern const wchar_t *SAMPLEREADER_get_sample_name(radium::SampleReader *reader);
  
extern int RT_SAMPLEREADER_read(radium::SampleReader *reader, float **samples, int num_ch, int num_frames, int64_t how_much_to_prepare);
extern void SAMPLEREADER_shut_down(void);


#endif
