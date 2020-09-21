#ifndef _RADIUM_AUDIO_SAMPLERECORDER_PROC_H
#define _RADIUM_AUDIO_SAMPLERECORDER_PROC_H

namespace radium{
  struct SampleRecorderInstance;
}

void SampleRecorder_register_instance(radium::SampleRecorderInstance *instance);
void SampleRecorder_unregister_instance(radium::SampleRecorderInstance *instance);


namespace radium{


struct SampleRecorderInstance {

  int64_t id;

  radium::String recording_path;
  int num_ch;
  float middle_note;
  const int64_t _latency_compensation;  
  
  int64_t start, end; // Can be accessed from another thread after is_finished has been called. Can also be accessed if holding the player lock.
  
  int64_t _start_delay; // internal
  int64_t _end_delay; // internal

  bool _requested_to_stop = false;
  
  SampleRecorderInstance(filepath_t recording_path, int num_ch, float middle_note, int latency_compensation)  // Note that the sample recorder adds 12 to the middle note.
    : recording_path(recording_path.id)
    , num_ch(num_ch)
    , middle_note(middle_note)
    , _latency_compensation(latency_compensation)
    , _start_delay(latency_compensation)
    , _end_delay(latency_compensation)
  {
    SampleRecorder_register_instance(this); // sets id.
  }


  virtual ~SampleRecorderInstance(){
    SampleRecorder_unregister_instance(this);
  }

  // Called when recording is finished.
  // Called by the main thread.
  virtual void is_finished(bool success, filepath_t filename) = 0;

  // Called when peaks have been generated for the samples provided by RT_SampleRecorder_add_audio.
  // Note that this function is not always called if the internal queue is full. Therefore,
  // peaks should always be reloaded from disk after recording is finished.
  // Called by the main thread.
  virtual void add_recorded_peak(int ch, float min_peak, float max_peak) = 0;
};
}

extern void SampleRecorder_called_regularly(void);
extern void RT_SampleRecorder_start_recording(radium::SampleRecorderInstance *instance, int64_t pos);
extern bool RT_SampleRecorder_add_audio(radium::SampleRecorderInstance *instance, const float **audio, int num_frames); // Returns false when finished.
extern void RT_SampleRecorder_request_stop_recording(radium::SampleRecorderInstance *instance);
extern int SampleRecorder_Get_Num_Instances(void);
extern void SampleRecorder_Init(void);
extern void SampleRecorder_shut_down(void);

#endif

