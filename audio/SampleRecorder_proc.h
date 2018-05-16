#ifndef _RADIUM_AUDIO_SAMPLERECORDER_PROC_H
#define _RADIUM_AUDIO_SAMPLERECORDER_PROC_H

namespace radium{
struct SampleRecorderInstance{

  wchar_t *recording_path;
  int num_ch;
  float middle_note;

  SampleRecorderInstance(wchar_t *recording_path, int num_ch, float middle_note)
    : recording_path(wcsdup(recording_path))
    , num_ch(num_ch)
    , middle_note(middle_note)
  {
  }

  virtual ~SampleRecorderInstance(){
    free(recording_path);
  }

  // Called when recording is finished.
  // Called by the main thread.
  virtual void is_finished(bool success, wchar_t *filename) = 0;

  // Called when peaks have been generated for the samples provided by RT_SampleRecorder_add_audio.
  // Called by the main thread.
  virtual void add_recorded_peak(int ch, float min_peak, float max_peak) = 0;
};
}

extern void SampleRecorder_called_regularly(void);
extern void RT_SampleRecorder_start_recording(radium::SampleRecorderInstance *instance);
extern void RT_SampleRecorder_add_audio(radium::SampleRecorderInstance *instance, float **audio, int num_frames);
extern void RT_SampleRecorder_stop_recording(radium::SampleRecorderInstance *instance);
extern void SampleRecorder_Init(void);

#endif

