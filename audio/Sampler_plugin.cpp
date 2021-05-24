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

#define __STDC_FORMAT_MACROS 1

#include <inttypes.h>

#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <math.h>

#include <QFileInfo>
#include <QDir>
#include <QPointer>
#include <QWidget>

#define INCLUDE_SNDFILE_OPEN_FUNCTIONS 1

#include "../common/nsmtracker.h"
#include "../common/visual_proc.h"
#include "../common/OS_Player_proc.h"
#include "../common/OS_settings_proc.h"
#include "../common/read_binary.h"
//#include "../common/PEQ_LPB_proc.h"
//#include "../common/PEQ_Signature_proc.h"
#include "../common/visual_proc.h"
#include "../common/disk.h"
#include "../common/spinlock.h"

#include "SoundPlugin.h"
#include "SoundPlugin_proc.h"
#include "SoundProducer_proc.h"
#include "Mixer_proc.h"
#include "SampleRecorder_proc.h"
#include "Peaks.hpp"
#include "undo_plugin_state_proc.h"

#include "../Qt/Qt_instruments_proc.h"

#include "SoundPluginRegistry_proc.h"

#include "GranResampler.hpp"
#include "Granulator.hpp"
#include "Fade.hpp"

#include "Resampler_proc.h"
#include "Envelope_proc.h"
#include "SoundFonts_proc.h"

#include "../embedded_scheme/s7extra_proc.h"

#include "../api/api_gui_proc.h"
#include "../api/api_proc.h"


#include "Sampler_plugin_proc.h"

#define POLYPHONY 256
#define MAX_NUM_SAMPLES 256
#define CROSSFADE_BUFFER_LENGTH 128
#define MAX_CROSSFADE_LENGTH (48000*5) // in samples. (By using samples instead of e.g. milliseconds or seconds, we ensure that loops sound the same if changing sample rate. There are probably disadvantages too though.)

#define MAX_PORTAMENTO 1000

//#define DEFAULT_A 20
//#define DEFAULT_H 5
//#define DEFAULT_D 20
//#define DEFAULT_S 0.5
//#define DEFAULT_R 20

#define DEFAULT_A 0
#define DEFAULT_H 0
#define DEFAULT_D 0
#define DEFAULT_S 1.0
#define DEFAULT_R 0

#define MAX_A 1000
#define MAX_H 40
#define MAX_D 1000
#define MAX_S 1.0
#define MAX_R 2000

#define MAX_VIBRATO_SPEED 20
#define MAX_VIBRATO_DEPTH 5

#define MAX_TREMOLO_SPEED 50
#define MAX_TREMOLO_DEPTH 1

// Granulation constants

#define MIN_stretch 0.01
#define MAX_stretch 100.0

#define MIN_fine_stretch 0.25
#define MAX_fine_stretch 4.0

#define MIN_overlap 0.1
#define MAX_overlap 50.0

#define MIN_length 0.1
#define MAX_length 1000.0

#define MIN_ramp 0.0
#define MAX_ramp 0.5

#define MIN_jitter 0.0
#define MAX_jitter 1.0

#define MIN_volume MIN_DB
#define MAX_volume MAX_DB



const char *g_click_name = "Click";

static void update_editor_graphics(SoundPlugin *plugin){
#if 0
  struct Tracker_Windows *window=root->song->tracker_windows;
  struct WBlocks *wblock=window->wblock;
  TRACKREALLINES_update_peak_tracks(window,plugin->patch);
  DrawUpAllPeakWTracks(window,wblock,plugin->patch);
#endif

#if USE_OPENGL
  GFX_ScheduleEditorRedrawIfPatchIsCurrentlyVisible(const_cast<Patch*>(plugin->patch));

#else
  if(plugin->patch!=NULL)
    RT_TRACKREALLINES_schedule_update_peak_tracks(plugin->patch);
#endif
}



// Effect order.
//
enum{
  EFF_STARTPOS,
  EFF_FINETUNE,
  EFF_NOTE_ADJUST,
  //  EFF_OCTAVE_ADJUST,
  EFF_PORTAMENTO,
  EFF_VIBRATO_SPEED,
  EFF_VIBRATO_DEPTH,
  EFF_TREMOLO_SPEED,
  EFF_TREMOLO_DEPTH,
  EFF_REVERSE,

  EFF_AHDSR_ONOFF,
  EFF_A,
  EFF_H,
  EFF_D,
  EFF_S,
  EFF_R,

  EFF_LOOP_ONOFF,
  EFF_CROSSFADE_LENGTH,
  EFF_PINGPONG,
  EFF_LOOP_OVERRIDE_DEFAULT,
  EFF_LOOP_START,
  EFF_LOOP_END,
  EFF_LOOP_WINDOW,
  
  EFF_GRAN_onoff,
  EFF_GRAN_coarse_stretch,
  EFF_GRAN_fine_stretch,
  EFF_GRAN_overlap,
  EFF_GRAN_length,
  EFF_GRAN_ramp,
  EFF_GRAN_jitter,
  EFF_GRAN_strict_no_jitter,
  EFF_GRAN_volume,

  // Note: If adding more effects, workaround code for loading older songs must be updated in Modulator_plugin.cpp. (just increase that number to increase with the number of added effects.)
  
  EFF_NUM_EFFECTS,
  };

#define SAMPLES_PER_PEAK 64


namespace{

struct ResettableGranResamplerCallback : public radium::GranResamplerCallback, public radium::AudioPickuper {
  virtual void reset2(void) = 0; // There is already a reset() function in AudioPickuper, so we call this one reset2 to avoid confusion about which one is called.
};

static radium::Spinlock _gran_pool_spinlock;
static radium::Granulator *_gran_pool; // access protected by obtaining _gran_pool_spinlock.

 
static void RT_release_granulator(radium::Granulator *granulator){
  radium::ScopedSpinlock lock(_gran_pool_spinlock);
  
#if !defined(RELEASE)
  granulator->set_sample_up_picker(NULL);
#endif

  granulator->_next = _gran_pool;
  _gran_pool = granulator;
}

 
static radium::Granulator *RT_obtain_granulator(radium::AudioPickuper *sample_up_picker){
  R_ASSERT_NON_RELEASE(_gran_pool_spinlock._holds_lock==false); // This function is only called from RT_play or set_effect_value, which should only be called from the main player thread, or when the player thread is locked.

  radium::Granulator *ret = NULL;
  
  {
    radium::ScopedSpinlock lock(_gran_pool_spinlock); // We obtain anyway, since I'm only 99% sure that the lock is not needed. (note that release is also called from the main thread, which further complicates things.)
    
    if (_gran_pool!=NULL) {
      
      ret = _gran_pool;
      _gran_pool = ret->_next;

      R_ASSERT_NON_RELEASE(ret->get_sample_up_picker()==NULL);
    }
  }

  if (ret != NULL)    
    ret->set_sample_up_picker(sample_up_picker);
  else
    RT_message("<center>No more free granulators.</center><p>"
               "If you need more voices doing granulation, please create a ticket at<br>"
               "<A href=\"https://github.com/kmatheussen/radium/issues\">https://github.com/kmatheussen/radium/issues</A>"
               );
  
  return ret;
}


static void init_granulator_pool(void){
  static bool has_inited=false;

  R_ASSERT(has_inited==false);
  has_inited = true;
  
  int pool_size = 128;

  double max_frames_between_grains = R_MIN(pc->pfreq*2, ms_to_frames(MAX_length / MIN_overlap)); // Don"t need more than 1 second between each grain. Just fail if trying to add more (possibly audible failure, not error message). Probably bad for CPU cache to allocate more than 1 second. Without the R_MIN, we would have allocated 10 seconds.
    
  for(int i=0;i<pool_size;i++){
    auto *granulator = new radium::Granulator(ms_to_frames(MAX_length), max_frames_between_grains, MAX_overlap, 1, NULL);
    granulator->_next = _gran_pool;
    _gran_pool = granulator;
  }
}

struct Data;

struct Sample{
  float volume = 0.0;

  int64_t num_frames = 0;
  int64_t loop_start = 0;
  int64_t loop_end = 0;

  int64_t loop_start_org = 0;
  int64_t loop_end_org = 0;

  int ch = 0;        // -1 means both channels.
  float *sound = NULL;

  //float *min_peaks = NULL;
  //float *max_peaks = NULL;
  radium::Peaks *peaks = new radium::Peaks();

#if !defined(RELEASE)
  radium::FilePath filename;
#endif

  double frequency_table[128] = {};

  Data *data = NULL;

  Sample()
#if !defined(RELEASE)
    : filename(L"")
#endif
  {}

  ~Sample(){
    delete peaks;
  }
};


#define MIN_LOOP_LENGTH 2


// Never access sample->loop_start and sample->loop_end directly. Instead use this one to always have legal values.
//
struct LoopData{
  int64_t _start;
  int64_t _end;
  int64_t _length;

  LoopData(){
  }

  LoopData(const Sample &sample, bool loop_start_was_set_last){
    if (sample.num_frames <= MIN_LOOP_LENGTH) {
      
      _start = 0;
      _end = MIN_LOOP_LENGTH;
      _length = MIN_LOOP_LENGTH;

    } else {
      
      _start = sample.loop_start;
      _end = sample.loop_end;
      
      if (_start < 0)
        _start = 0;
      
      if (_end >= sample.num_frames)
        _end = sample.num_frames;
      
      if ((_end-_start) < MIN_LOOP_LENGTH){
        
        if (loop_start_was_set_last){
          
          if (_end >= MIN_LOOP_LENGTH) {

            _start = _end - MIN_LOOP_LENGTH;
            
          } else {
            
            _start = 0;
            _end = MIN_LOOP_LENGTH;
            
          }
          
        } else {
          
          if (_start <= sample.num_frames - MIN_LOOP_LENGTH) {
            
            _end = _start + MIN_LOOP_LENGTH;
            
          } else {
            
            _start = sample.num_frames-MIN_LOOP_LENGTH;
            _end = sample.num_frames;
            
          }
          
        }
      }
      
      _length = _end - _start;
      R_ASSERT_NON_RELEASE(_length >= MIN_LOOP_LENGTH);

    }
  }

  LoopData(const Sample *sample, bool loop_start_was_set_last)
    : LoopData(*sample, loop_start_was_set_last)
  {}
};


// A voice object points to only one sample. Stereo-files uses two voice objects. Soundfonts using x sounds to play a note, need x voice objects to play that note.
//
// A voice is inited in the function "init_voice", and released in "release_voice".
struct Voice{
  Voice *prev = NULL;
  Voice *next = NULL;

  float last_finetune_value = 0;

  float note_num = 0;
  int64_t note_id = 0;
  const struct SeqBlock *seqblock = NULL; // Not quite sure, but this variable could perhaps be gc-ed while its here, so it should only be used for comparison. (pretty sure it can not be gc-ed though)

  // These two variables are used when setting velocity after a note has started playing.
  float start_volume = 0;
  float end_volume = 0;
  //double gain;

  float crossfade_buffer[CROSSFADE_BUFFER_LENGTH] = {};  
  
  // Same for pitch
  float pitch = 0;
  //float start_pitch;
  float end_pitch = 0;
  float pitch_inc = 0; // For portamento.
  int portamento_channel = 0;
  bool set_last_end_pitch = 0; // true for all new voices, but is set false after player has been stopped.
  
  Panvals pan = {};
  
  int64_t pos = 0;

  LoopData loop_data;
  
  bool reverse = false;

  std::unique_ptr<ResettableGranResamplerCallback> _get_samples;
  
  radium::Granulator *_granulator = NULL;
  radium::Resampler2 *_resampler2;
  radium::GranResampler *_granresampler;

  //radium::Resampler *resampler = NULL;
  void *adsr = NULL;

  int delta_pos_at_start = 0; // Within the current block. Set when starting a note.
  int delta_pos_at_end = 0; // Within the current block. Set when stopping a note.

  int num_samples = 0;
  const Sample *sample = NULL;

  double vibrato_phase;
  double vibrato_value;
};

struct Note{
  Note(const Note&) = delete;
  Note& operator=(const Note&) = delete;

  radium::Vector<const Sample*> samples;

#if !defined(RELEASE)
  int num_samples_when_sorted = -1;
#endif
  
  // Must be called before calling is_equal.
  void sort_samples(void){
    samples.sort([](const Sample *a, const Sample *b){
        return ((const char*)a) > ((const char*)b);   
      });
#if !defined(RELEASE)
    num_samples_when_sorted = samples.size();
#endif
  }
  
  // Both this->samples and note2->sample must be sorted (sort_samples()) before calling.
  bool is_equal(const Note *note2) const {
    R_ASSERT_NON_RELEASE(num_samples_when_sorted == samples.size());
    R_ASSERT_NON_RELEASE(note2->num_samples_when_sorted == note2->samples.size());
    
    if(samples.size() != note2->samples.size())
      return false;

    for(int i=0;i<samples.size();i++){
      if(samples.at(i) != note2->samples.at(i))
        return false;
    }
    
    return true;
  }
  
  Note(){
  }

  //int num_samples = 0;
  //const Sample *samples[MAX_NUM_SAMPLES] = {};
};

// The part of "Data" that can be memcpy-ed when creating a new Data from old Data.
struct CopyData {
  float startpos;

  float finetune; // -100 -> +100
  float note_adjust; // -6 -> +6      (must be float because of conversions)
  //float octave_adjust; // -10 -> +10. (must be float because of conversions)

  float portamento;

  bool ahdsr_onoff;
  float a,h,d,s,r;

  DEFINE_ATOMIC(bool, loop_onoff);
  int crossfade_length;

  bool reverse;
  bool pingpong;
  
  bool loop_override_default;
  int64_t loop_start;
  int64_t loop_end;   // if loop_end<=loop_start, use loop data in sample file instead, if there is any. (loop_start has no meaning if loop_length is 0)
  bool loop_start_was_set_last;
  
  double vibrato_depth;
  double vibrato_speed;
  double vibrato_phase_add;
  double vibrato_phase;
  double vibrato_value;

  float tremolo_depth;
  float tremolo_speed;

  radium::GranulatorParameters gran_parms;

  bool gran_enabled;

  double gran_coarse_stretch;
  double gran_fine_stretch;
  float gran_volume;
};


 
enum{
  NOT_RECORDING = 0,
  BEFORE_RECORDING,
  IS_RECORDING
};

struct MySampleRecorderInstance;

struct Data{

  CopyData p = {};

  bool use_sample_file_middle_note = false; // Set to true by default now, but not included (or set to false) in the state of older states. Without this flag, loading older sounds could sound wrong.
  
  struct SoundPlugin *tremolo = NULL;
  
  float samplerate = 0; // The system samplerate. I.e. the jack samplerate, not soundfile samplerate.

  enum ResamplerType org_resampler_type = RESAMPLER_NON; // Used to remember the original resampler type when rendering soundfile.
  enum ResamplerType resampler_type = RESAMPLER_NON;
  radium::FilePath filename;
  int instrument_number = 0;
  bool using_default_sound = false;
  
  //int num_channels; // not used for anything, I think.

  radium::Vector<Note*> note_storage;
  const Note *notes[128] = {};

  Voice *voices_playing = NULL;
  Voice *voices_not_playing = NULL;
  float last_end_pitch[NUM_PATCH_VOICES << 4] = {};
  
  int num_different_samples = 0;  // not used for anything (important).

  Voice voices[POLYPHONY] = {};
  
  // These two are used when switching sound on the fly
  DEFINE_ATOMIC(struct Data *, new_data) = NULL;
  RSemaphore *signal_from_RT = NULL;

  DEFINE_ATOMIC(int, recording_status) = 0;
  MySampleRecorderInstance *recorder_instance = NULL;  // thread-protected by recording_status
  int recording_start_frame = 0;  // thread-protected by recording_status
  bool recording_from_main_input;  // thread-protected by recording_status
  int64_t recording_note_id = 0;  // thread-protected by recording_status
  const struct SeqBlock *recording_seqblock = NULL;  // thread-protected by recording_status
  
  // No need to clear these two fields after usage (to save some memory) since plugin->data is reloaded after recording.
  radium::Vector<float> min_recording_peaks[2];
  radium::Vector<float> max_recording_peaks[2];

  QPointer<QWidget> gui;
  DEFINE_ATOMIC(int, rtwidget_pos) = -1;
  
  // There should be a compiler option in c++ to make this the default behavior. It would automatically eliminate a billion bugs in the world.
  /*
    Commented out. Doesn't work with gcc 7.
  void *operator new(size_t size) {
    void *mem = ::operator new(size);
    memset(mem, 0, size);
    return mem;
  }
  */

  Sample samples[MAX_NUM_SAMPLES] = {}; // Put last to keep data togheter. (only the first elements of "samples" are used)
  
  Data(const Data&) = delete;
  Data& operator=(const Data&) = delete;
 
  Data(filepath_t filename)
    : filename(filename)
  {
    R_ASSERT(voices_playing==NULL);
    R_ASSERT(voices_not_playing==NULL);
    R_ASSERT(p.crossfade_length==0);
    R_ASSERT(voices[0].next==NULL);
    R_ASSERT(samples[0].sound==NULL);

    //for(int i=0;i<128;i++)
    //  notes[i] = new Note;
  }

  ~Data(){
    for(Note *note : note_storage)
      delete note;
  }
};

 
 

struct MySampleRecorderInstance : radium::SampleRecorderInstance{

  struct Patch *_patch;

  MySampleRecorderInstance(struct Patch *patch, filepath_t recording_path, int num_ch, int64_t latency)
    : SampleRecorderInstance(recording_path, num_ch, 48, latency)
    , _patch(patch)
  {
  }
  
  void is_finished(bool success, filepath_t filename) override {
    SoundPlugin *plugin = (SoundPlugin*)_patch->patchdata;
    
    if (plugin != NULL){
      
      Data *data=(Data*)plugin->data;
      
      data->min_recording_peaks[0].clear();
      data->max_recording_peaks[0].clear();
      
      data->min_recording_peaks[1].clear();
      data->max_recording_peaks[1].clear();
      
      update_editor_graphics(plugin);
      
      if (success) {
        
        ADD_UNDO(PluginState(_patch, NULL));
        
        SAMPLER_set_new_sample(plugin,
                               filename,
                               0);
      }
    }
  }
  
  void add_recorded_peak(int ch, float min_peak, float max_peak) override {
    SoundPlugin *plugin = (SoundPlugin*)_patch->patchdata;
    
    if (plugin != NULL){
      Data *data=(Data*)plugin->data;
      
      if (ATOMIC_GET(data->recording_status) != IS_RECORDING)
        return;
      
      R_ASSERT_RETURN_IF_FALSE(ch==0 || ch==1);
      
      data->min_recording_peaks[ch].push_back(min_peak);
      data->max_recording_peaks[ch].push_back(max_peak);
      
      update_editor_graphics(plugin);
    }
  }
};


} // end anon. namespace

static bool get_granulation_enabled(Data *data);



#if 0
static double get_ratio(int sample_note_num, int play_note_num){
  return midi_to_hz(sample_note_num) / midi_to_hz(play_note_num);
}
#endif

static int get_portamento_channel(int midi_channel, int voicenum) __attribute__ ((pure));
static int get_portamento_channel(int midi_channel, int voicenum) {
  R_ASSERT_NON_RELEASE(NUM_PATCH_VOICES < 16);
  return (voicenum << 4) | midi_channel;
}



// input is between 0 and 1.
// output is between 0 and 1.
static float velocity2gain(float val){
  return val;
#if 0
  if(val<=0.0f)
    return 0.0f;
  else if(val>=1.0f)
    return 1.0f;
  else
    return powf(10, scale(val,0.0, 1.0 ,-40, 20) / 20.0f) / 10.0f;
#endif
}


static void RT_add_voice(Voice **root, Voice *voice){
  voice->next = *root;
  if(*root!=NULL)
    (*root)->prev = voice;
  *root = voice;
  voice->prev = NULL;
}

static void RT_remove_voice(Voice **root, Voice *voice){
  if(voice->prev!=NULL)
    voice->prev->next = voice->next;
  else
    *root=voice->next;

  if(voice->next!=NULL)
    voice->next->prev = voice->prev;
}


/**********************************
  Crossfading code
***********************************/
static void RT_fade_replace(float *dst, float *src, int num_frames, float start_val, float end_val){
  float mul = start_val;
  float inc = (end_val-start_val)/(float)num_frames;
  int i;
#if 0
   for(i=0;i<num_frames;i++){
    dst[i] = src[i]*mul;
    mul += inc;
  }
#else
  for(i=0;i<num_frames;i++)
    dst[i] = src[i] * (mul + inc*i);
#endif
}

static void RT_fade_add(float *dst, float *src, int num_frames, float start_val, float end_val){
  float mul = start_val;
  float inc = (end_val-start_val)/(float)num_frames;
  int i;
#if 0
  for(i=0;i<num_frames;i++){
    dst[i] += src[i]*mul;
    mul += inc;
  }
#else
  for(i=0;i<num_frames;i++)
    dst[i] += src[i] * (mul + inc*i);
#endif
}

static int RT_crossfade(int64_t start_pos, int64_t end_pos, int64_t crossfade_start, int64_t crossfade_end, int64_t loop_start, float *out_data, float *in_data){
  const int num_frames = (int)(end_pos - start_pos);
  R_ASSERT_NON_RELEASE(num_frames >= 0);
  
  float start_fade_val = scale_double(start_pos,
                                      crossfade_start, crossfade_end,
                                      1.0f, 0.0f
                                      );

  float end_fade_val  = scale_double(end_pos,
                                     crossfade_start, crossfade_end,
                                     1.0f, 0.0f
                                     );

  //printf("fade %s: %d -> %d (%f -> %f)\n", end_fade_val > start_fade_val ? "in" : "out", (int)start_pos, (int)start_pos+num_frames, start_fade_val, end_fade_val);
  int64_t fade_in_start_pos = loop_start + (start_pos - crossfade_start);
  //printf("fade %s:  %d -> %d. (%f -> %f)\n\n", end_fade_val <= start_fade_val ? "in" : "out", (int)fade_in_start_pos, (int)fade_in_start_pos+num_frames, 1.0f - start_fade_val, 1.0f - end_fade_val);
  //len_in_data-end_pos, len_in_data-end_pos+num_frames);
  //printf("%f -> %f\n\n",start_fade_val,end_fade_val);

  RT_fade_replace(
                  out_data,
                  in_data + start_pos,
                  num_frames,
                  start_fade_val, end_fade_val
                  );
  RT_fade_add(
                  out_data,
                  in_data + fade_in_start_pos,
                  num_frames,
                  1.0f - start_fade_val, 1.0f - end_fade_val
              );

  return num_frames;
}

static int RT_legal_crossfade_length(const LoopData &loop_data, Data *data){
  int crossfade_length = data->p.crossfade_length;
  int64_t loop_length = loop_data._length;

  return (int)R_MIN(crossfade_length, loop_length/2);
}

static int RT_src_callback_with_crossfade_do_looping(Voice *voice, const LoopData &loop_data, const Sample *sample, Data *data, int64_t start_pos, float **out_data, int crossfade_length){
  *out_data = voice->crossfade_buffer;

  int64_t end_pos = start_pos + CROSSFADE_BUFFER_LENGTH;
  if (end_pos > loop_data._end)
    end_pos = loop_data._end;

  voice->pos = end_pos; // next
  if (voice->pos>=loop_data._end){
    R_ASSERT_NON_RELEASE(voice->pos==loop_data._end);
    voice->pos = loop_data._start + crossfade_length;
    //printf("crossfading %d -> %d-%d -> %d (%d)\n",sample->loop_start,start_pos,end_pos,sample->loop_end,voice->pos);
    R_ASSERT_NON_RELEASE(voice->pos < loop_data._end);
  }

  R_ASSERT_NON_RELEASE(voice->pos < loop_data._end);
  

  //printf("do looping %d\n");

  float *in_data = voice->sample->sound;

  return RT_crossfade(start_pos, end_pos,
                      loop_data._end - crossfade_length, loop_data._end,
                      loop_data._start,
                      *out_data, in_data
                      );
}

static int64_t RT_src_callback_with_crossfade_between_looping(Voice *voice, const LoopData &loop_data, const Sample *sample, Data *data, int64_t start_pos, float **out_data, const int64_t crossfade_start){
  *out_data = sample->sound + start_pos;
  
  // We add R_MAX(prev_voice_pos+1, ...) because othervice prev_voice_pos can be equal or larger than voice->pos when changing crossfade and/or changing loop points while playing.
  voice->pos = R_MAX(start_pos+1, crossfade_start); // next
  
  R_ASSERT_NON_RELEASE(voice->pos < loop_data._end);

  R_ASSERT_NON_RELEASE( (voice->pos - start_pos) >= 0);
  
  return voice->pos - start_pos;
}

static int64_t RT_src_callback_with_crossfade_before_looping(Voice *voice, const LoopData &loop_data, const Sample *sample, Data *data, int64_t start_pos, float **out_data, int64_t crossfade_start){  
  *out_data = sample->sound;
  voice->pos = crossfade_start; // next
  R_ASSERT_NON_RELEASE(voice->pos < loop_data._end);

  R_ASSERT_NON_RELEASE(voice->pos >= 0);
  return voice->pos; // start_pos==0 here.
}

static int64_t RT_src_callback_with_crossfade_looping(Voice *voice, const LoopData &loop_data, const Sample *sample, Data *data, int64_t start_pos, float **out_data){
  //printf("crossfading %d -> %d -> %d (%d)\n",sample->loop_start,start_pos,sample->loop_end,voice->pos);

  const int crossfade_length = RT_legal_crossfade_length(loop_data, data);
  const int64_t crossfade_start = loop_data._end - crossfade_length;
  
  if (start_pos==0 && loop_data._start>0)
    return RT_src_callback_with_crossfade_before_looping(voice, loop_data, sample, data, start_pos, out_data, crossfade_start);

  else if (start_pos >= crossfade_start)
    return RT_src_callback_with_crossfade_do_looping(voice, loop_data, sample, data, start_pos, out_data, crossfade_length);

  else
    return RT_src_callback_with_crossfade_between_looping(voice, loop_data, sample, data, start_pos, out_data, crossfade_start);
}
/**********************************
  End of crossfading code
***********************************/



static int64_t RT_src_callback_with_normal_looping(Voice *voice, const LoopData &loop_data, const Sample *sample, Data *data, int64_t start_pos, float **out_data){
  *out_data = &sample->sound[start_pos];

  voice->pos = loop_data._start; // next

  if(start_pos >= loop_data._end) // just in case. not sure if this can happen
    return 0;

  else
    return loop_data._end - start_pos;
}



static int64_t RT_src_callback_nolooping(Voice *voice, const Sample *sample, Data *data, int64_t start_pos, float **out_data){
  *out_data = &sample->sound[start_pos];

  if(start_pos >= sample->num_frames){
    R_ASSERT_NON_RELEASE(start_pos==sample->num_frames);
    return 0;
  }

  voice->pos = sample->num_frames; // next

  int64_t ret = sample->num_frames - start_pos;

  if (ret < 0){
#if !defined(RELEASE)
    fprintf(stderr, "num_frames: %d, start_pos: %d\n",(int)sample->num_frames, (int)start_pos);
    abort();
#endif
    ret = 0;
  }

  return ret;
}

static int64_t RT_src_callback_reverse(Voice *voice, const LoopData &loop_data, const Sample *sample, Data *data, int64_t start_pos, float **out_data, bool do_looping){
  *out_data = &voice->crossfade_buffer[0];

  int64_t end;
  
  if (do_looping) {
    
    end = loop_data._end;
    
    if(start_pos >= end){
      //R_ASSERT_NON_RELEASE(start_pos == end);
      start_pos = loop_data._start;
    }
    
  } else {
    
    end = sample->num_frames;
    
    if(start_pos >= end)
      return 0;
    
  }

      

  const int64_t samples_left = end - start_pos;
  
  const int64_t num_samples_to_return = R_MIN(samples_left, CROSSFADE_BUFFER_LENGTH);

  //printf("....num_to_return: %d. samples_left: %d. end: %d. start_pos: %d\n", (int)num_samples_to_return, (int) samples_left, (int)end, (int)start_pos);
  
  float *source_sound = sample->sound;
  float *dest_sound = &voice->crossfade_buffer[0];
  const int64_t sample_pos = end-1 - start_pos;
 
  for(int64_t i=0 ; i< num_samples_to_return ; i++){
    dest_sound[i] = source_sound[sample_pos - i];
    //dest_sound[i] = source_sound[sample_pos--];
  }
  
  voice->pos = start_pos + num_samples_to_return; // i.e. next pos

  return num_samples_to_return;
}

static int64_t RT_src_callback_ping_pong_looping(Voice *voice, const LoopData &loop_data, const Sample *sample, Data *data, int64_t start_pos, float **out_data){  
  R_ASSERT(start_pos <= sample->num_frames*2);
  
  if (start_pos >= loop_data._end + loop_data._length)
    start_pos = loop_data._start;

  if (start_pos >= loop_data._end) {

    int64_t org_voice_pos = voice->pos;
    
    int64_t ret = RT_src_callback_reverse(voice, loop_data, sample, data, start_pos - loop_data._length, out_data, true);
    voice->pos = org_voice_pos + ret;
    
    R_ASSERT_NON_RELEASE(voice->pos <= loop_data._end + loop_data._length);
    
    return ret;
    
  } else {
    
    int64_t ret = loop_data._end - start_pos;
    *out_data = &sample->sound[start_pos];    
    voice->pos = loop_data._end;
    
    return ret;
  }
}


/*
  Too much technical dept in this part of the file. Player routine should be rewritten. There's also some limitations on ping-ping and crossfade, and there's clicks when changing crossfade length.
 */

static long RT_src_callback(void *cb_data, float **out_data){
  Voice *voice = (Voice*)cb_data;
  const Sample *sample = voice->sample;
  int64_t start_pos = voice->pos;
  Data  *data = sample->data;

  bool pingpong = sample->data->p.pingpong;
  bool reverse = voice->reverse;

  LoopData loop_data(sample, data->p.loop_start_was_set_last);

  bool loop = ATOMIC_GET_RELAXED(data->p.loop_onoff) && loop_data._end > loop_data._start;

  //printf("loop: %d / %d\n", ATOMIC_GET(data->p.loop_onoff), loop_data._end > loop_data._start);
  
  int64_t ret;

  if (start_pos < 0) {
    start_pos = 0;
    R_ASSERT_NON_RELEASE(false);
  }
  
  if (loop && pingpong) {
    
    ret = RT_src_callback_ping_pong_looping(voice, loop_data, sample, data, start_pos, out_data); // ping pong looping

  } else if (!loop) {

    //printf("NOlooping. %d\n", (int)start_pos);
    if (reverse)
      ret = RT_src_callback_reverse(voice, loop_data, sample, data, start_pos, out_data, false);
    else
      ret = RT_src_callback_nolooping(voice, sample, data, start_pos, out_data);
      
  } else {

    if (start_pos >= sample->num_frames){
      // Happens when switching from ping-pong to non-ping-pong while playing.
      start_pos -= sample->num_frames; // Keep same sample position.
      
      if (start_pos >= sample->num_frames){
        R_ASSERT_NON_RELEASE(false);
        start_pos = 0;
      }
    }

    if (reverse) {
    
      ret = RT_src_callback_reverse(voice, loop_data, sample, data, start_pos, out_data, loop);
      
    } else {

      //int bef = start_pos;
      
      if (start_pos >= loop_data._end)
        start_pos = loop_data._start;
      
      if(data->p.crossfade_length > 0)
        ret = RT_src_callback_with_crossfade_looping(voice, loop_data, sample, data, start_pos, out_data);    
      else
        ret = RT_src_callback_with_normal_looping(voice, loop_data, sample, data, start_pos, out_data);

      /*
      printf("   RET: %d. voice->pos: %d. bef: %d. loop_data._start: %d. loop_data._end: %d. data: %p\n",
             (int)ret,
             (int)voice->pos,
             (int)bef,
             (int)start_pos,
             (int)loop_data._end,
             *out_data);
             */
             
      if (loop){
        R_ASSERT_NON_RELEASE(voice->pos < loop_data._end); // happens when changing between ping-poing and non-ping-pong
      }

    }
  
  }

  return (long)ret;
}




static double RT_get_src_ratio3(Data *data, const Sample *sample, float pitch){    
  if(pitch<=0.0)
    pitch=0.0f;
  if(pitch>126)
    pitch=126.0f;

  int notenum = (int)pitch;
  float finetune = pitch - notenum;

  //printf("finetune: %f, scale: %f. First: %f\n", finetune, scale(finetune, 0, 1, sample->frequency_table[notenum], sample->frequency_table[notenum+1]), sample->frequency_table[notenum]);
  return data->samplerate / scale_double(finetune, 0, 1, sample->frequency_table[notenum], sample->frequency_table[notenum+1]);
}

// Note: Also called from get_peaks
static double RT_get_src_ratio2(Data *data, const Sample *sample, float pitch){

  //printf("note_adjust: %d (%f)\n",(int)data->p.note_adjust,data->p.note_adjust);
  double adjusted_pitch = pitch + scale_double(safe_float_read(&data->p.finetune), 0, 1, -1, 1) + data->p.note_adjust;
  return RT_get_src_ratio3(data, sample, adjusted_pitch);
}

static double RT_get_src_ratio(Data *data, Voice *voice){
  const Sample *sample = voice->sample;

  //int notenum = voice->note_num + (int)data->octave_adjust*12 + (int)data->p.note_adjust;
  //int notenum = voice->note_num + (int)data->p.note_adjust;
  //float pitch = voice->end_pitch + scale(data->p.finetune, 0, 1, -1, 1) + (int)data->p.note_adjust;

  float pitch = voice->pitch;

  // Add vibrato here instead of in get_src_ratio3 to avoid weird peaks
  if (data->p.vibrato_phase_add > 0.0) {
    pitch += voice->vibrato_value; //data->p.vibrato_value;
    //printf("%f ,%f",data->p.vibrato_depth,data->p.vibrato_value);
  }

  return RT_get_src_ratio2(data,sample,pitch);
}

static int RT_get_resampled_data(Data *data, Voice *voice, float *out, int num_frames){
#if 0
  printf("samplerate: %f, sample frequency: %f, ratio: %f\n",
         data->samplerate,
         voice->sample->frequency_table[voice->note_num],
         data->samplerate / voice->sample->frequency_table[voice->note_num]);
#endif

  double ratio = RT_get_src_ratio(data,voice);
  
#if 0
  if (fabs(ratio-1.0) < 1.01)
    ratio = 1.0;
  printf("  src ratio: %f\n", ratio);
#endif
  
  voice->_resampler2->_ratio = ratio;
  //voice->_granulator->set_stretch(2.0);
  //return voice->_resampler2->RT_process(out, 0, num_frames);
  float *buffer[1] = {out};
  return voice->_granresampler->RT_process(buffer, num_frames);
  //return RESAMPLER_read(voice->resampler, ratio, num_frames, out);
}

#if 0
static float get_peak(float *samples, int num_samples){
  float ret=0.0f;
  int i;
  for(i=0;i<num_samples;i++){
    float m=fabsf(samples[i]);
    if(m>ret)
      ret=m;
  }
  return ret;
}
#endif

static bool RT_play_voice(Data *data, Voice *voice, int num_frames_to_produce, float **outputs){
  // portamento
  {
#if 0
    voice->pitch = voice->end_pitch;
#else
    if (!equal_floats(voice->end_pitch, voice->pitch)){
      voice->pitch += voice->pitch_inc*num_frames_to_produce;
      if (voice->pitch_inc < 0){
        if (voice->end_pitch > voice->pitch)
          voice->pitch = voice->end_pitch;
      } else {
        if (voice->end_pitch < voice->pitch)
          voice->pitch = voice->end_pitch;
      }
    }

    if (voice->set_last_end_pitch){
      data->last_end_pitch[voice->portamento_channel] = voice->pitch;
    }
    
#endif
  }
  
  int startpos = voice->delta_pos_at_start;
  int endpos = voice->delta_pos_at_end;

  if(startpos>=0)
    voice->delta_pos_at_start = 0;
  if(endpos>=0)
    voice->delta_pos_at_end = -1;


  if(endpos>=0 && endpos<startpos) // Should not happen. Test for it just in case. It's a bit messy when notes are generated by the keyboard, player and other places at the same time.
    {
#if !defined(RELEASE)
      printf("Oops. Endpos: %d. startpos: %d\n",endpos,startpos); // FIX: We get here when the CPU is very buzy.
#endif
      return false;
    }

  int num_frames = num_frames_to_produce-startpos;

  R_ASSERT_NON_RELEASE(num_frames >= 0);
    
  if (num_frames <= 0)
    return false;
  
  float resampled_data[num_frames];
  int frames_created_by_resampler = 0;

  {
    bool do_add_vibrato = data->p.vibrato_phase_add > 0.0;
    
    int num_frames_inc;
    
    if (do_add_vibrato){
      
      voice->vibrato_value = data->p.vibrato_value;
      voice->vibrato_phase = data->p.vibrato_phase;
      
      num_frames_inc = 64;  // Run smaller inner loop if using vibrato.
      
    } else {
      
      num_frames_inc = num_frames;

    }

    for(int i=0;i<num_frames;i+=num_frames_inc){
      
      int num_frames2 = num_frames_inc;

      if( i + num_frames2 > num_frames)
        num_frames2 = num_frames - i;
      
      frames_created_by_resampler += RT_get_resampled_data(data,voice,resampled_data+i,num_frames2);
      //printf("Frames created by resampler: %d\n",frames_created_by_resampler);
      //printf("peak: %f\n",get_peak(resampled_data,frames_created_by_resampler));
      
      if(do_add_vibrato){
        voice->vibrato_value = data->p.vibrato_depth * sin(voice->vibrato_phase);
        voice->vibrato_phase += data->p.vibrato_phase_add*(double)num_frames2;
      }
    }
  }
  
  if (frames_created_by_resampler==0)
    return true;
  
  int frames_created_by_envelope;

  float *adsr_sound_data[1]={&resampled_data[0]};

  bool adsr_onoff = data->p.ahdsr_onoff;
  
  if(endpos>=0){
    int pre_release_len = endpos-startpos;

    //printf("********** endpos>0: %d. prelen: %d, frames_created_by_resampler: %d\n",endpos,prelen,frames_created_by_resampler);

    if (!adsr_onoff) {

      frames_created_by_envelope = pre_release_len;

    } else if(frames_created_by_resampler <= pre_release_len){ // i.e. we reached the end of sound before beginning to release the ADSR envelope.

      frames_created_by_envelope = ADSR_apply(voice->adsr, adsr_sound_data, 1, frames_created_by_resampler);

    }else{
      
      if (pre_release_len > 0)
        frames_created_by_envelope = ADSR_apply(voice->adsr, adsr_sound_data, 1, pre_release_len);
      else
        frames_created_by_envelope = 0;
      
      //printf("************************ Calling adsr release\n");
      ADSR_release(voice->adsr);

      int post_release_len = frames_created_by_resampler - frames_created_by_envelope;
      adsr_sound_data[0] = &resampled_data[frames_created_by_envelope];        
      frames_created_by_envelope += ADSR_apply(voice->adsr, adsr_sound_data, 1, post_release_len);
    }

  }else{

    if (!adsr_onoff)
      frames_created_by_envelope = frames_created_by_resampler;
    else
      frames_created_by_envelope = ADSR_apply(voice->adsr, adsr_sound_data, 1, frames_created_by_resampler);
    
    //printf("Frames created by envelope: %d, peak: %f\n",frames_created_by_envelope,get_peak(resampled_data,frames_created_by_envelope));
    //printf("peak: %f\n",get_peak(resampled_data,frames_created_by_resampler));
  }

  //float peak_in = get_peak(resampled_data,frames_created_by_envelope);

  if (frames_created_by_envelope==0)
    return true;

  const Sample *sample = voice->sample;

#define mix(input_channel, output_channel) do{                          \
    float panval = voice->pan.vals[input_channel][output_channel];      \
    if(panval>0.0f){                                                    \
      float *out          = outputs[output_channel] + startpos;         \
      float  start_volume = voice->start_volume*panval;                 \
      float  end_volume   = voice->end_volume*panval;                   \
      SMOOTH_mix_sounds_raw(out, resampled_data, frames_created_by_envelope, start_volume, end_volume); \
    }                                                                   \
  }while(0)

  if(sample->ch == -1){
    mix(0,0);
    mix(0,1);
  }else{
    mix(sample->ch,0);
    mix(sample->ch,1);
  }
  
#undef mix
  
  //printf("peak in/out: %.3f - %.3f\n",peak_in,get_peak(outputs[0], num_frames_to_produce));

  voice->start_volume = voice->end_volume;
  //voice->start_pitch = voice->end_pitch;
  
  if(startpos+frames_created_by_envelope < num_frames_to_produce)
    return true;
  else
    return false;
}


static void RT_prepare_voice_for_playing_with_granulation(Data *data, Voice *voice){
  auto *granulator = RT_obtain_granulator(voice->_get_samples.get());
  if(granulator==NULL)
    return;

  granulator->apply_parameters_and_reset(data->p.gran_parms);
  
  voice->_granulator = granulator;

  voice->_resampler2->set_callback(granulator);
}


static void RT_release_voice_for_playing_with_granulation(Voice *voice){
  auto *granulator = voice->_granulator;
  if (granulator==NULL)
    return;

  voice->_granulator = NULL;
    
  RT_release_granulator(granulator);
}


static void RT_process(SoundPlugin *plugin, int64_t time, int num_frames, float **inputs, float **outputs){
  Data *data = (Data*)plugin->data;
  Voice *voice = data->voices_playing;

  memset(outputs[0],0,num_frames*sizeof(float));
  memset(outputs[1],0,num_frames*sizeof(float));

  //printf("Recording status: %d\n", ATOMIC_GET(data->recording_status));
      
  if (ATOMIC_GET(data->recording_status)==IS_RECORDING){
    float *audio_[data->recorder_instance->num_ch];
    float **audio = audio_;

    int num_ch;
    
    if (data->recording_from_main_input) {
      num_ch = MIXER_get_main_inputs(const_cast<const float**>(audio), data->recorder_instance->num_ch);
    } else {
      num_ch = R_MIN(2, data->recorder_instance->num_ch);
      for(int ch=0;ch<num_ch;ch++)
        audio_[ch] = inputs[ch];
    }

    for(int ch=0;ch<num_ch;ch++)
      audio_[ch] += data->recording_start_frame;

    //R_ASSERT_RETURN_IF_FALSE(num_ch==data->recorder_instance->num_ch);
    
    /*
      // No need. Mixer always have at least 2 channels, and data->recorder->num_ch can never have more than 2 channels.
    const float empty_block[RADIUM_BLOCKSIZE] = {}; // The stack might be hotter than the heap.
    for(int ch = num_ch ; ch < data->recorder->num_ch ; ch++)
      audio[ch] = empty_block;
    */
    
    if (num_ch!=data->recorder_instance->num_ch || false==RT_SampleRecorder_add_audio(data->recorder_instance,
                                                                                      const_cast<const float**>(audio),
                                                                                      RADIUM_BLOCKSIZE - data->recording_start_frame
                                                                                      ))
      {
        struct Patch *patch = (struct Patch*)plugin->patch;
        
        ATOMIC_SET(data->recording_status, NOT_RECORDING);
        ATOMIC_SET(patch->is_recording, false);
        //printf("            ...and STOPPED --------------------\n\n\n");
      }
    
    data->recording_start_frame = 0;

    RT_PLUGIN_touch(plugin); // Make sure RT_process is called next frame as well.
    
    return;
  }

  bool do_add_vibrato = data->p.vibrato_phase_add > 0.0;
  
  if (do_add_vibrato){
    data->p.vibrato_value = data->p.vibrato_depth * sin(data->p.vibrato_phase);
    data->p.vibrato_phase += data->p.vibrato_phase_add*(double)num_frames;
  }

  bool was_playing_something = data->voices_playing != NULL;
    
  while(voice!=NULL){
    Voice *next = voice->next;
    
    if(RT_play_voice(data, voice, num_frames, outputs)==true){
      RT_release_voice_for_playing_with_granulation(voice);
      RT_remove_voice(&data->voices_playing, voice);
      RT_add_voice(&data->voices_not_playing, voice);
    }
    
    voice = next;
  }
    
  if (was_playing_something){
    data->tremolo->type->RT_process(data->tremolo, time, num_frames, outputs, outputs);
    if(data->p.gran_enabled){
      float gran_volume = data->p.gran_volume;
      if (!equal_floats(gran_volume, 0.0)){
        gran_volume = db2gain(gran_volume);
        for(int ch=0;ch<2;ch++)
          for(int i=0;i<num_frames;i++)
            outputs[ch][i] *= gran_volume;
      }
    }
  }
  
  Data *new_data = ATOMIC_GET(data->new_data);
  
  if(new_data != NULL){
    RT_fade_out(outputs[0],num_frames);
    RT_fade_out(outputs[1],num_frames);
    
    plugin->data = new_data; // Bang! (hmm.)
    ATOMIC_SET(data->new_data, NULL);
    
    RSEMAPHORE_signal(data->signal_from_RT,1);
  }
}



static void play_note(struct SoundPlugin *plugin, int time, note_t note2){
  Data *data = (Data*)plugin->data;

  //printf("  Sampler_plugin.cpp: Request to play note %f. Id: %d\n", note2.pitch, (int)note2.id);
  //printf("  Sampler_plugin.cpp: Request to play note %d at %d\n", (int)note2.id, time);

  R_ASSERT_NON_RELEASE(time>=0);
  R_ASSERT_NON_RELEASE(time<RADIUM_BLOCKSIZE);
  
  //fprintf(stderr,"playing note %d. Pitch: %d, time: %d\n",(int)note_id,(int)note_num,(int)time);

  if (ATOMIC_GET(data->recording_status)==BEFORE_RECORDING && note2.sample_pos==0){

    data->recorder_instance->middle_note = note2.pitch;

    struct Patch *patch = (struct Patch*)plugin->patch;
    RT_SampleRecorder_start_recording(data->recorder_instance, 0);
    
    data->recording_note_id = note2.id;
    data->recording_seqblock = note2.seqblock;
    data->recording_start_frame = time;
    
    ATOMIC_SET(data->recording_status, IS_RECORDING);
    ATOMIC_SET(patch->is_recording, true); // Used to determine whether to paint the chip background red.
    
    return;
  }

  if (ATOMIC_GET(data->recording_status)==IS_RECORDING)
    return;
  
  const Note *note = data->notes[(int)note2.pitch];

  int portamento_channel = get_portamento_channel(note2.midi_channel, note2.voicenum);
  
  for(const Sample* sample : note->samples){ //i=0;i<note->num_samples;i++){

    LoopData loop_data(sample, data->p.loop_start_was_set_last);
    
    if(data->voices_not_playing==NULL){
      printf("No more free voices\n");
      return;
    }

    Voice *voice = data->voices_not_playing;
    
    RT_remove_voice(&data->voices_not_playing, voice);
    RT_add_voice(&data->voices_playing, voice);

    voice->set_last_end_pitch = true;
    
    voice->last_finetune_value = data->p.finetune;
    
    voice->note_num = note2.pitch;
    voice->note_id = note2.id;
    voice->seqblock = note2.seqblock;
    voice->portamento_channel = portamento_channel; //note2.midi_channel * 16 + note2.voicenum;
    
    voice->start_volume = velocity2gain(note2.velocity);
    voice->end_volume = voice->start_volume;

    const float portamento = data->p.portamento;

    voice->end_pitch = note2.pitch;

    if (portamento < 0.001)
      voice->pitch = note2.pitch;
    else {
      float last_end_pitch = data->last_end_pitch[portamento_channel];
      if (last_end_pitch <= 0.001) {
        voice->pitch = note2.pitch;
      } else {
        voice->pitch = last_end_pitch;
        voice->pitch_inc = (voice->end_pitch - voice->pitch) * 1000.0f / (data->samplerate * portamento);
      }
    }
    
    //const Sample *sample = note->samples[i];
    
    voice->sample = sample;

    voice->loop_data = loop_data;

    bool is_looping = ATOMIC_GET_RELAXED(data->p.loop_onoff)==true && loop_data._end > loop_data._start;
  
    if(is_looping)
      voice->pos=scale(data->p.startpos, // set startpos between 0 and loop_end
                       0,1,
                       0,loop_data._end);
    else
      voice->pos=scale(data->p.startpos,  // set startpos between 0 and sound length
                       0,1,
                       0,sample->num_frames);

    //printf("Sample_pos: %d\n",(int)note2.sample_pos);
    if (note2.sample_pos > 0){
      
      voice->pos += note2.sample_pos / RT_get_src_ratio(data, voice);
      
      if (is_looping) {
        
        if (voice->pos >= loop_data._end) {
          
          int num_loops = (voice->pos - loop_data._start) / loop_data._length;
          voice->pos = voice->pos - (num_loops * loop_data._length);
          
          if (voice->pos < loop_data._start){
            
            R_ASSERT_NON_RELEASE(false);
            
            voice->pos = loop_data._start;
            
          } else if (voice->pos >= loop_data._end){
            
            R_ASSERT_NON_RELEASE(voice->pos == loop_data._end);
            
            voice->pos = loop_data._start;
            
          }
          
        }
        
      } else {
        
        if (voice->pos >= sample->num_frames){
          RT_remove_voice(&data->voices_playing, voice);
          RT_add_voice(&data->voices_not_playing, voice);
          return;
        }
        
      }
    }

    voice->reverse = sample->data->p.reverse;
    
    voice->pan = get_pan_vals_vector(note2.pan,voice->sample->ch==-1?1:2);

    if (data->p.gran_enabled)
      RT_prepare_voice_for_playing_with_granulation(data, voice);

    if (voice->_granulator==NULL)
      voice->_resampler2->set_callback(voice->_get_samples.get());
    
    voice->_resampler2->reset();
    voice->_get_samples->reset2();
    //RESAMPLER_reset(voice->resampler);

    ADSR_reset(voice->adsr);
    ADSR_set_adsr(voice->adsr, data->p.a, data->p.h, data->p.d, data->p.s, data->p.r);

    voice->delta_pos_at_start=time;
    voice->delta_pos_at_end=-1;
  }

  data->last_end_pitch[portamento_channel] = note2.pitch;
}


static void set_note_volume(struct SoundPlugin *plugin, int time, note_t note){
  Data *data = (Data*)plugin->data;

  if (ATOMIC_GET(data->recording_status)==IS_RECORDING)
    return;

  Voice *voice = data->voices_playing;

  while(voice!=NULL){
    //printf("Setting volume to %f. note_num: %d. voice: %d\n",volume,note_num,voice->note_num);

    if(is_note(note, voice->note_id, voice->seqblock))
      voice->end_volume = velocity2gain(note.velocity);

    voice = voice->next;
  }
}

static void set_note_pitch(struct SoundPlugin *plugin, int time, note_t note){
  Data *data = (Data*)plugin->data;

  if (ATOMIC_GET(data->recording_status)==IS_RECORDING)
    return;

  Voice *voice = data->voices_playing;

  //printf("Setting pitch to %f.\n",note.pitch);

  while(voice!=NULL){

    if(is_note(note, voice->note_id, voice->seqblock)){

      const float portamento = data->p.portamento;
      
      voice->end_pitch = note.pitch;
              
      if (portamento < 0.001)
        voice->pitch = note.pitch;
      else
        voice->pitch_inc = (voice->end_pitch - voice->pitch) * 1000.0f / (data->samplerate * portamento);

      //printf("Pitch. voice->pitch_inc: %f\n", voice->pitch_inc);
      //printf("Got it\n");
    }

    voice = voice->next;
  }
}

static void set_note_pan(struct SoundPlugin *plugin, int time, note_t note){
  Data *data = (Data*)plugin->data;

  if (ATOMIC_GET(data->recording_status)==IS_RECORDING)
    return;

  Voice *voice = data->voices_playing;

  //printf("Setting pan to %f.\n",note.pan);

  while(voice!=NULL){

    if(is_note(note, voice->note_id, voice->seqblock)){
      
      voice->pan = get_pan_vals_vector(note.pan,voice->sample->ch==-1?1:2);
      
    }

    voice = voice->next;
  }
}

static void stop_note(struct SoundPlugin *plugin, int time, note_t note){

  Data *data = (Data*)plugin->data;

  //printf("  Sampler_plugin.cpp: Request to stop note %f. Id: %d\n", note.pitch, (int)note.id);
  
  if (ATOMIC_GET(data->recording_status)==IS_RECORDING){
    if (is_note(note, data->recording_note_id, data->recording_seqblock)){

      //printf("            STOP --------------------\n\n\n");
      RT_SampleRecorder_request_stop_recording(data->recorder_instance);
      
      return;
    }
  }
  
  Voice *voice = data->voices_playing;

  R_ASSERT_NON_RELEASE(time>=0);
  R_ASSERT_NON_RELEASE(time<RADIUM_BLOCKSIZE);
  
  if (time < 0)
    time = 0;
  else if (time >= RADIUM_BLOCKSIZE)
    time = RADIUM_BLOCKSIZE -1;
  
  while(voice!=NULL){
    if(is_note(note, voice->note_id, voice->seqblock)){
      if(voice->delta_pos_at_end == -1)
        voice->delta_pos_at_end = time;
    }

    voice = voice->next;
  }
}

static void player_is_stopped(struct SoundPlugin *plugin){
  R_ASSERT(PLAYER_current_thread_has_lock());
  
  Data *data = (Data*)plugin->data;
  
  memset(data->last_end_pitch, 0, sizeof(float)*(NUM_PATCH_VOICES << 4));
  Voice *voice = data->voices_playing;
  while(voice!=NULL){
    voice->set_last_end_pitch = false;
    voice = voice->next;      
  }
  //printf("********** Player is stopped called\n");
}

// returns the attack+decay+release value (i.e. A+D+R in ADSR) as number of samples.
static int RT_get_audio_tail_length(const struct SoundPlugin *plugin){
  Data *data = (Data*)plugin->data;
  return (data->p.a+data->p.d+data->p.r) * (double)MIXER_get_sample_rate() / 1000.0;
}

static void called_after_plugin_has_been_created(const SoundPluginType *plugin_type, struct SoundPlugin *plugin){
  plugin->RT_input_latency_manifests_into_output_latency = false;
}


static bool note_has_sample(const Note *note){
  return note->samples.size() > 0;
}

static double gran_get_stretch(Data *data);

namespace{

  // TimeToFrame class used to prevent logical errors. When converting more than one time-value to frame simultaneously, we want to use the same src and stretch values for all computations. If not, the result may not only be slighly wrong now and then (no big deal), but we also risk end frame starting before start frame.
  struct TimeToFrame{

    int _A;
    double _B;
    
    TimeToFrame(Data *data, float f_note_num){

      int i_note_num = (int)f_note_num;
      
      const Note *note = data->notes[(int)i_note_num];
      
      if (note->samples.is_empty()){
        RError("note->samples is empty. %f\n",f_note_num);
        _A = data->p.startpos*10000;
        _B = 30000.0;
        return;
      }
      
      const Sample *sample = note->samples.at(0);
      
      const double src_ratio = RT_get_src_ratio2(data, sample, f_note_num);
      const double stretch = get_granulation_enabled(data) ? gran_get_stretch(data) : 1.0;

      _A = data->p.startpos*sample->num_frames;
      _B = src_ratio*stretch;
    }
    
    int get(double time){
      return _A + time/_B;
    }
  };
}


static void apply_adsr_to_peak(Data *data, int64_t time, radium::Peak &peak){
  float ms = time*1000 / data->samplerate;
  float mul;

  if(ms >= data->p.a+data->p.h+data->p.d)
    mul = data->p.s;

  else if(ms >= data->p.a+data->p.h)
    mul = scale(ms,
                (data->p.a+data->p.h),
                (data->p.a+data->p.h+data->p.d), 
                1.0f,
                data->p.s);

  else if(ms >= data->p.a)
    return;

  else
    mul = scale(ms,
                0.0f,data->p.a,
                0.0f,1.0f);


  peak.scale(mul);
}


static radium::Peak get_peak_from_sample(Data *data, const Sample *sample, int64_t start_time, const int64_t duration, int rec_level){
  R_ASSERT(rec_level <= 1); // This function can only be called recursively once; when starting in the middle of a loop and ask for more data than is available in the remainder of the loop.
  
  radium::Peak peak;

  int64_t end;

  LoopData loop_data(sample, data->p.loop_start_was_set_last);
  //printf("Peak: loop start: %d. End: %d\n", (int)loop_data._start, (int)loop_data._end);
  
  bool is_looping = ATOMIC_GET(data->p.loop_onoff)==true;

  if(is_looping && start_time>=loop_data._end){

    if (loop_data._end <= loop_data._start)
      return peak;

    start_time -= loop_data._end; // i.e. how far after loop end are we?
      
    int64_t num_loops = start_time / loop_data._length;
    start_time -= (num_loops*loop_data._length);
    
    start_time += loop_data._start;

    if (start_time < 0){
      R_ASSERT_NON_RELEASE(false);
      start_time = 0;
    }
    
    end = loop_data._end;

  } else {

    end = sample->num_frames;
    
  }
  

  int64_t duration_now = R_MIN(duration, end - start_time);
  
  if(duration_now <= 0){
    //printf("    Duration now: %d. duration: %d. end: %d, start_time: %d. end-start_time: %d\n", (int)duration_now, (int)duration, (int)end, (int)start_time, (int)(end-start_time));
    //R_ASSERT_NON_RELEASE(is_looping==false); // INVESTIGATE: This happens when assigning modulator to note adjustment.
    return peak;
  }

  int64_t end_time = start_time + duration_now;

  //printf("    Calculating %d -> %d (%d frames)\n", (int)start_time, (int)(end_time), (int)(duration_now));
  peak = sample->peaks->get(start_time, end_time);

  // If start_time==sample->p.loop_start, we would not get hold of any new peaks in the call to get_peak_from_sample below. (it would also stall the program in some situations)
  if (is_looping && start_time > loop_data._start){
    int64_t duration_left = duration - duration_now;
    
    if (duration_left > 0)
      peak.merge(get_peak_from_sample(data, sample, loop_data._start, duration_left, rec_level+1));
  }
  
  return peak;
}

static int get_peaks(struct SoundPlugin *plugin,
                     float note_num,
                     int ch,
                     float das_pan,
                     int64_t start_time,
                     int64_t end_time,
                     float *min_value, float *max_value
                     )
{
  //printf("Get peaks. %d -> %d (%d frames)\n", (int)start_time, (int)end_time, (int)(end_time-start_time));
  Data *data = (Data*)plugin->data;

  //printf("  get_peaks. Start_time: %d, end_time: %d\n\n",(int)start_time,(int)end_time);
  
  if(ch==-1){
    if (ATOMIC_GET(data->recording_status) == IS_RECORDING){
      R_ASSERT_RETURN_IF_FALSE2(data->recorder_instance!=NULL, 1);
      return data->recorder_instance->num_ch;
    }

    for(int i=0;i<MAX_NUM_SAMPLES;i++){
      const Sample &sample=data->samples[i];
      if (sample.sound==NULL){
#if !defined(RELEASE)
        for(int i2=i+1;i2<MAX_NUM_SAMPLES;i2++){
          const Sample &sample=data->samples[i2];
          R_ASSERT(sample.sound==NULL);
        }
#endif
        break;
      }
        
      if(sample.ch==1)
        return 2;
    }
    return 1;
  }

  R_ASSERT_RETURN_IF_FALSE2(end_time>=start_time, 2);

  R_ASSERT_RETURN_IF_FALSE2(note_num >= 0.0f, 2);

  if (data->min_recording_peaks[0].size() > 0) {
      
    R_ASSERT_RETURN_IF_FALSE2(data->recorder_instance!=NULL, 2);

    double recording_note = data->recorder_instance->middle_note; //(float)ATOMIC_GET(data->recording_note) / 10000.0;

    double ratio = midi_to_hz(note_num) / midi_to_hz(recording_note);

    //printf("  Peak ratio: %f  (%f, %f)\n", ratio,note_num,recording_note);
    
    int start_index = ratio * (double)start_time / (double)RADIUM_BLOCKSIZE;
    int end_index   = ratio * (double)end_time / (double)RADIUM_BLOCKSIZE;
    
    {
      bool has_set_min = false;
      float min=0.0f;

      for(int i = start_index ; i < end_index ; i++) {
        if (i >= data->min_recording_peaks[ch].size())
          break;
        
        float val = data->min_recording_peaks[ch].at(i);
        if (has_set_min){
          if (val < min)
            min = val;
        } else {
          min = val;
          has_set_min = true;
        }
      }
      
      *min_value = min;
    }

    {
      bool has_set_max = false;
      float max=0.0f;
      
      for(int i = start_index ; i < end_index ; i++) {
        if (i >= data->max_recording_peaks[ch].size())
          break;
        
        float val = data->max_recording_peaks[ch].at(i);
        if (has_set_max){
          if (val > max)
            max = val;
        } else {
          max = val;
          has_set_max = true;
        }
      }

      *max_value = max;
    }
    
    return 2;
  }

#if !defined(RELEASE)
  bool should_have_gotten_data = false;
#endif
  
    
  radium::Peak peak;

  const Note *note = data->notes[R_BOUNDARIES(0, (int)note_num, 127)];

  if (!note_has_sample(note) || start_time >= end_time){
    R_ASSERT_NON_RELEASE(end_time > start_time);
    goto return_empty;
  } 

  //printf("Creating new peak for ch %d, frames %d -> %d\n", ch, (int)start_frame, (int)end_frame);

  {
    TimeToFrame time_to_frame(data, note_num);
    const int64_t start_frame = time_to_frame.get(start_time);
    const int64_t end_frame = time_to_frame.get(end_time);
    
    if (start_frame>=end_frame){
      R_ASSERT_NON_RELEASE(start_frame==end_frame);
      goto return_empty;
    }
  
    for(const Sample *sample : note->samples){
    
      Panvals pan = get_pan_vals_vector(das_pan, sample->ch==-1 ? 1 : 2);
      int input_channel = sample->ch==-1 ? 0 : sample->ch;
      float panval = pan.vals[input_channel][ch];

      //printf("  Sample %d. Panval: %f\n", samplenum, panval);
      
      if(panval>0.0f){

        //printf("Asking for peak %d %d (%d). Total sample duration: %d\n", (int)start_frame, (int)end_frame, (int)(end_frame-start_frame), (int)sample->num_frames);
        radium::Peak new_peak = get_peak_from_sample(data, sample, start_frame, end_frame-start_frame,0);
        
        if (new_peak.has_data()){
          new_peak.scale(panval);
#if !defined(RELEASE)
          should_have_gotten_data = true;
#endif
        }else{
#if !defined(RELEASE)
          /*
          if(ATOMIC_GET(sample->data->p.loop_onoff)==true) //INVESTIGATE: Happens when assigning modulator to note adjustment.
            abort();
          */
          if (start_frame < sample->num_frames)
            abort();
#endif
        }

        peak.merge(new_peak);
      }
    }
  }

  if (peak.has_data()==false){ // happens if note is playing longer than there are samples.
    R_ASSERT_NON_RELEASE(should_have_gotten_data==false);
    goto return_empty;
  }

  if (data->p.ahdsr_onoff)
    apply_adsr_to_peak(data, (start_time+end_time)/2, peak);

  *min_value = peak.get_min();
  *max_value = peak.get_max();  
  return 2;

 return_empty:
  *min_value = 0.0f;
  *max_value = 0.0f;
  return 2;
}


/************* Granulation *****************/

#define ALL_GRAN_CASES()                              \
  GRAN_CASE(overlap);                                 \
  GRAN_CASE(length);                                  \
  GRAN_CASE(ramp);                                    \
  GRAN_CASE(jitter);                                  \
  GRAN_CASE(volume);                                  \
  

static void set_granulation_enabled(SoundPlugin *plugin, Data *data, bool enabled){
  bool is_enabled = data->p.gran_enabled;
  if (enabled==is_enabled)
    return;

  R_ASSERT_NON_RELEASE(PLAYER_current_thread_has_lock());

  if (enabled){

    for(Voice *voice = data->voices_playing ; voice!=NULL ; voice=voice->next)
      RT_prepare_voice_for_playing_with_granulation(data, voice);
    
  } else {
    
    for(Voice *voice = data->voices_playing ; voice!=NULL ; voice=voice->next)
      voice->_resampler2->set_callback(voice->_get_samples.get());
    
  }

  data->p.gran_enabled = enabled;

  update_editor_graphics(plugin);
}

static bool get_granulation_enabled(Data *data){
  return data->p.gran_enabled;
}




#define GRAN_FUNCS(Methodname)                                          \
  template<typename Type> static void gran_set_##Methodname(Data *data, Type val){ \
    R_ASSERT_NON_RELEASE(PLAYER_current_thread_has_lock());             \
                                                                        \
    data->p.gran_parms.Methodname = val;                                \
                                                                        \
    for(Voice *voice = data->voices_playing ; voice!=NULL ; voice=voice->next) \
      if(voice->_granulator != NULL)                                    \
        voice->_granulator->set_##Methodname(val);                      \
                                                                        \
    RT_RTWIDGET_mark_needing_update(ATOMIC_GET_RELAXED(data->rtwidget_pos)); \
  }                                                                     \
                                                                        \
  static double gran_get_##Methodname(Data *data){                      \
    return data->p.gran_parms.Methodname;                               \
  }


GRAN_FUNCS(stretch)
GRAN_FUNCS(overlap)
GRAN_FUNCS(grain_length)
GRAN_FUNCS(ramp)
GRAN_FUNCS(jitter)
GRAN_FUNCS(strict_no_jitter)

static void gran_set_length(Data *data, double val){
  gran_set_grain_length(data, ms_to_frames(val));
}

static double gran_get_length(Data *data){
  return frames_to_ms(gran_get_grain_length(data));
}

  
static void gran_set_coarse_stretch(Data *data, double val){
  //printf("               2. SETTING stretch to %f\n", val);
  data->p.gran_coarse_stretch = val;
  gran_set_stretch(data, data->p.gran_coarse_stretch * data->p.gran_fine_stretch);
}

static double gran_get_coarse_stretch(Data *data){
  return data->p.gran_coarse_stretch;
}

static void gran_set_fine_stretch(Data *data, double val){
  data->p.gran_fine_stretch = val;
  gran_set_stretch(data, data->p.gran_coarse_stretch * data->p.gran_fine_stretch);
}

static double gran_get_fine_stretch(Data *data){
  return data->p.gran_fine_stretch;
}

// val between MIN_DB and MAX_DB
static void gran_set_volume(Data *data, float val){
  data->p.gran_volume = val;
}

static float gran_get_volume(Data *data){
  return data->p.gran_volume;
}

  

static void RT_set_loop_points_internal(const SoundPlugin *plugin, Data *data, int64_t start, int64_t end, bool force_use_org = false){
  R_ASSERT_NON_RELEASE(plugin->data!=data || !SP_is_plugin_running(plugin) || PLAYER_current_thread_has_lock());
 
  data->p.loop_start = start;
  data->p.loop_end = end;

  //printf("   X internal. Setting %d %d\n", (int)start, (int)end);
  
  float *prev=NULL;
  
  for(int i=0;i<MAX_NUM_SAMPLES;i++){
    Sample &sample=data->samples[i];
    if (sample.sound==NULL)
      break;
    
    if(sample.sound != prev){
      prev = sample.sound;
      
      if (data->p.loop_override_default && !force_use_org) {
        
        sample.loop_start = start;
        sample.loop_end = end;

        //printf("   2. Setting %d: %d %d\n", i, (int)start, (int)end);
        
      } else {

        //printf("   3. Setting %d: %d %d\n", i, (int)start, (int)end);
        
        sample.loop_start = sample.loop_start_org;
        sample.loop_end = sample.loop_end_org;
        
      }
    }
  }

}

// Use this one if setting both at the same time (if not it might not work).
static void RT_set_loop_points_complete(SoundPlugin *plugin, Data *new_data, int64_t start, int64_t end, bool force_use_org = false){
  if (force_use_org) {
    Sample &sample=new_data->samples[0];
    if (sample.sound!=NULL){
      start = sample.loop_start_org;
      end = sample.loop_end_org;
    }
  }

  Data *plugin_data=(Data*)plugin->data;
  
  //printf("   X complete. Setting %d %d. force_use_org: %d. Datas: %p / %p\n", (int)start, (int)end, force_use_org, plugin_data, new_data);

  plugin_data->p.loop_start = 0; // Ensure EFF_LOOP_END works when calling PLUGIN_set_effect_value.
  
  if (new_data==plugin_data) {
    
    PLUGIN_set_effect_value(plugin, -1, EFF_LOOP_END, end, STORE_VALUE, FX_single, EFFECT_FORMAT_NATIVE); // can set end first since we forced loop_start to 0 above.
    PLUGIN_set_effect_value(plugin, -1, EFF_LOOP_START, start, STORE_VALUE, FX_single, EFFECT_FORMAT_NATIVE);
    
  } else {

    // Send scaled values instead. Can't send native value for a different sample.
    
    Sample &sample=new_data->samples[0];
    if (sample.sound!=NULL){

      int64_t num_frames = sample.num_frames;

      if (num_frames > 0) {
        
        float scaled_loop_start = R_BOUNDARIES(0, scale(start, 0, num_frames, 0, 1), 1);
        float scaled_loop_end = R_BOUNDARIES(0, scale(end, 0, num_frames, 0, 1), 1);
        
        PLUGIN_set_effect_value(plugin, -1, EFF_LOOP_END, scaled_loop_end, STORE_VALUE, FX_single, EFFECT_FORMAT_SCALED); // can set end first since we forced loop_start to 0 above.
        PLUGIN_set_effect_value(plugin, -1, EFF_LOOP_START, scaled_loop_start, STORE_VALUE, FX_single, EFFECT_FORMAT_SCALED);
        
      } else {
        
        R_ASSERT_NON_RELEASE(false);
        
      }
      
    } else {
      
      R_ASSERT_NON_RELEASE(false);
      
    }
  }
  
  RT_set_loop_points_internal(plugin, new_data, start, end, force_use_org); // The calls to PLUGIN_set_effect_value above might not have set accurate values due to int64_t -> float conversion.
}

  
static void set_loop_onoff(Data *data, bool loop_onoff){
  ATOMIC_SET_RELAXED(data->p.loop_onoff, loop_onoff);
}

static bool get_loop_onoff(Data *data){
  return ATOMIC_GET_RELAXED(data->p.loop_onoff);
}

static bool can_crossfade(Data *data){
  return data->p.reverse==false && data->p.pingpong==false;
}

static float get_effect_value(struct SoundPlugin *plugin, int effect_num, enum ValueFormat value_format);

static void maybe_update_loop_slider_boundaries(struct SoundPlugin *plugin, const Sample &sample){
  if (plugin->curr_storeit_type!=STORE_VALUE)
    return;
  
  if (!THREADING_is_main_thread())
    return;

  struct Patch *patch = plugin->patch;
  
  if (patch==NULL){
    R_ASSERT_NON_RELEASE(false);
    return;
  }
  
  int64_t start = get_effect_value(plugin, EFF_LOOP_START, EFFECT_FORMAT_NATIVE);
  int64_t end = get_effect_value(plugin, EFF_LOOP_END, EFFECT_FORMAT_NATIVE);
  int64_t len = end-start;
  
  int minval = scale_int64(start + MIN_LOOP_LENGTH, 0, sample.num_frames, 0, 10000);  
  int maxval = scale_int64(end - MIN_LOOP_LENGTH, 0, sample.num_frames, 0, 10000);
  
  GFX_set_effect_display_boundaries(patch, EFF_LOOP_START, 0, maxval);

  GFX_set_effect_display_boundaries(patch, EFF_LOOP_END, minval, 10000);
  
  GFX_set_effect_display_boundaries(patch, EFF_LOOP_WINDOW, 0, 10000 - scale_int64(len, 0, sample.num_frames, 0, 10000));
}

static void set_effect_value(struct SoundPlugin *plugin, int time, int effect_num, float value, enum ValueFormat value_format, FX_when when){
  Data *data = (Data*)plugin->data;

  switch(effect_num){
    case EFF_AHDSR_ONOFF:
      data->p.ahdsr_onoff = value >= 0.5;
      update_editor_graphics(plugin);
      return;
      
    case EFF_LOOP_ONOFF:
      set_loop_onoff(data, value>=0.5f);
      update_editor_graphics(plugin);
      return;

    case EFF_LOOP_OVERRIDE_DEFAULT:
      {
        data->p.loop_override_default = value>=0.5f;
        RT_set_loop_points_internal(plugin, data, data->p.loop_start, data->p.loop_end);
        update_editor_graphics(plugin);
        if (plugin->patch != NULL)
          GFX_ScheduleInstrumentRedraw((struct Patch*)plugin->patch);
      }
      return;

    case EFF_LOOP_START:
      {
        Sample &sample=data->samples[0];
        if (sample.sound != NULL){

          int64_t start = R_BOUNDARIES(0,
                                       value_format==EFFECT_FORMAT_NATIVE ? value : scale_double(value,
                                                                                                 0,1,
                                                                                                 0,sample.num_frames),
                                       sample.num_frames-1);
          
          RT_set_loop_points_internal(plugin,
                                      data,
                                      start,
                                      data->p.loop_end
                                      );
          data->p.loop_start_was_set_last = true;

          PLUGIN_call_me_when_an_effect_value_has_changed(plugin,
                                                          EFF_LOOP_WINDOW,
                                                          data->p.loop_start,
                                                          get_effect_value(plugin, EFF_LOOP_START, EFFECT_FORMAT_SCALED),
                                                          false, // make undo
                                                          plugin->curr_storeit_type,
                                                          when,
                                                          false //update_instrument_widget
                                                          );

          maybe_update_loop_slider_boundaries(plugin, sample);
          
          update_editor_graphics(plugin);

          if (when==FX_single && plugin->patch != NULL)
            GFX_ScheduleInstrumentRedraw((struct Patch*)plugin->patch);

        }
      }
      return;
      
    case EFF_LOOP_END:
      {
        Sample &sample=data->samples[0];
        //printf("loop end. %p\n", &sample);
        if (sample.sound != NULL){
          //printf("       EFF_LOOP_END: %f (%s). (loop_start: %d. num_frames: %d)\n", value, value_format==EFFECT_FORMAT_NATIVE ? "native" : "scaled", (int)data->p.loop_start, (int)sample.num_frames);

          LoopData loop_data(sample, data->p.loop_start_was_set_last);
          int64_t old_start = loop_data._start;
          
          int64_t end = R_BOUNDARIES(old_start + MIN_LOOP_LENGTH,
                                     value_format==EFFECT_FORMAT_NATIVE ? value : scale_double(value,
                                                                                               0,1,
                                                                                               0,sample.num_frames),
                                     sample.num_frames
                                     );

          RT_set_loop_points_internal(plugin,
                                      data,
                                      old_start, //data->p.loop_start,
                                      end
                                      );
          data->p.loop_start_was_set_last = false;

          maybe_update_loop_slider_boundaries(plugin, sample);
          
          update_editor_graphics(plugin);

          if (when==FX_single && plugin->patch != NULL)
            GFX_ScheduleInstrumentRedraw((struct Patch*)plugin->patch);          
          
        }
      }
      return;
      
    case EFF_LOOP_WINDOW:
      {
        Sample &sample=data->samples[0];
        if (sample.sound != NULL){

          int64_t length;
          
          {
            LoopData loop_data(sample, data->p.loop_start_was_set_last);
            length = loop_data._end - loop_data._start;
            //printf("start: %d end: %d length: %d. Length: %d\n", (int)loop_data._start, (int)loop_data._end, (int)length, (int)sample.num_frames);
          }

          int64_t start = R_BOUNDARIES(0,
                                       value_format==EFFECT_FORMAT_NATIVE ? value : scale_double(value,
                                                                                                 0,1,
                                                                                                 0,sample.num_frames),
                                       sample.num_frames-length);
          
          RT_set_loop_points_internal(plugin,
                                      data,
                                      start,
                                      R_BOUNDARIES(1, (start + length), sample.num_frames)
                                      );
          data->p.loop_start_was_set_last = true;

          PLUGIN_call_me_when_an_effect_value_has_changed(plugin,
                                                          EFF_LOOP_START,
                                                          data->p.loop_start,
                                                          get_effect_value(plugin, EFF_LOOP_START, EFFECT_FORMAT_SCALED),
                                                          false, // make undo
                                                          plugin->curr_storeit_type,
                                                          when,
                                                          false //update_instrument_widget
                                                          );
                                                          
          PLUGIN_call_me_when_an_effect_value_has_changed(plugin,
                                                          EFF_LOOP_END,
                                                          data->p.loop_end,
                                                          get_effect_value(plugin, EFF_LOOP_END, EFFECT_FORMAT_SCALED),
                                                          false, // make undo
                                                          plugin->curr_storeit_type,
                                                          when,
                                                          false //update_instrument_widget
                                                          );

          maybe_update_loop_slider_boundaries(plugin, sample);
          
          update_editor_graphics(plugin);

          if (when==FX_single && plugin->patch != NULL)
            GFX_ScheduleInstrumentRedraw((struct Patch*)plugin->patch);

        }
      }
      return;
      
    default:
      break;
  }
  
  if(value_format==EFFECT_FORMAT_SCALED){
    switch(effect_num){
    case EFF_STARTPOS:
      //printf("    Samp: start pos to %f at %d\n", value , time);
      data->p.startpos = value;
      update_editor_graphics(plugin);
      break;
    case EFF_FINETUNE:
      safe_float_write(&data->p.finetune, value);
      update_editor_graphics(plugin);
      break;
    case EFF_A:
      data->p.a = scale(value,
                      0.0,1.0,
                      0,MAX_A);
      update_editor_graphics(plugin);
      break;
    case EFF_H:
      data->p.h = scale(value,
                      0.0,1.0,
                      0,MAX_H);
      update_editor_graphics(plugin);
      break;
    case EFF_D:
      data->p.d = scale(value,
                      0.0,1.0,
                      0,MAX_D);
      update_editor_graphics(plugin);
      break;
    case EFF_S:
      data->p.s = scale(value,
                      0.0,1.0,
                      0,MAX_S);
      update_editor_graphics(plugin);
      break;
    case EFF_R:
      data->p.r = scale(value,
                        0.0,1.0,
                        0,MAX_R);
      break;
    case EFF_PORTAMENTO:
      data->p.portamento = scale(value,
                                 0,1,
                                 0,MAX_PORTAMENTO);
      break;
    case EFF_VIBRATO_SPEED:
      data->p.vibrato_speed = scale(value,
                                  0.0,1.0,
                                  0,MAX_VIBRATO_SPEED);

      if (data->p.vibrato_speed <= 0.001) {
        data->p.vibrato_value = 0.0;
        data->p.vibrato_phase = 4.71239;
        data->p.vibrato_phase_add = -1;
      } else
        data->p.vibrato_phase_add = data->p.vibrato_speed * 2.0 * M_PI / data->samplerate;
      
      break;
    case EFF_VIBRATO_DEPTH:
      data->p.vibrato_depth = scale(value,
                                  0.0,1.0,
                                  0,MAX_VIBRATO_DEPTH);
      if (data->p.vibrato_depth <= 0.001) {
        data->p.vibrato_value = 0.0;
        data->p.vibrato_phase = 4.71239;
        data->p.vibrato_phase_add = -1;    
      } else
        data->p.vibrato_phase_add = data->p.vibrato_speed * 2.0 * M_PI / data->samplerate;
      break;
    case EFF_TREMOLO_SPEED:
      data->p.tremolo_speed = scale(value,
                                  0.0,1.0,
                                  0,MAX_TREMOLO_SPEED);
      data->tremolo->type->set_effect_value(data->tremolo, time, 0, data->p.tremolo_speed, EFFECT_FORMAT_NATIVE, when);
      break;
    case EFF_TREMOLO_DEPTH:
      data->p.tremolo_depth = scale(value,
                                  0.0,1.0,
                                  0,MAX_TREMOLO_DEPTH);
      data->tremolo->type->set_effect_value(data->tremolo, time, 1, data->p.tremolo_depth, EFFECT_FORMAT_NATIVE, when);
      break;
    case EFF_NOTE_ADJUST:
      data->p.note_adjust = scale(value,
                                0.0,1.0,
                                -6.99,6.99);
      update_editor_graphics(plugin);
      break;
#if 0
    case EFF_OCTAVE_ADJUST:
      data->p.octave_adjust = scale(value,
                                  0,1,
                                  -10.99,10.99);
      break;
#endif
    case EFF_CROSSFADE_LENGTH:
      if (can_crossfade(data)) {
        double dvalue = value;
        data->p.crossfade_length = scale_double(dvalue*dvalue*dvalue*dvalue,
                                                0.0, 1.0,
                                                0, MAX_CROSSFADE_LENGTH
                                                );
        //GFX_ScheduleInstrumentRedraw((struct Patch*)plugin->patch);
      } else
        data->p.crossfade_length = 0;
      
      break;
      
    case EFF_REVERSE:      
      data->p.reverse = value>=0.5f;
      if (!can_crossfade(data)){
        
        //printf("Doing it %p\n",plugin->patch);
        PLUGIN_set_effect_value(plugin, time, EFF_CROSSFADE_LENGTH, 0, THREADING_is_main_thread() ? STORE_VALUE : DONT_STORE_VALUE, when, EFFECT_FORMAT_NATIVE);
        
        if (plugin->patch != NULL)
          GFX_ScheduleInstrumentRedraw((struct Patch*)plugin->patch);
      }
                                 
      break;
      
    case EFF_PINGPONG:
      data->p.pingpong =  value>=0.5f;
      if (!can_crossfade(data)){

        PLUGIN_set_effect_value(plugin, time, EFF_CROSSFADE_LENGTH, 0, THREADING_is_main_thread() ? STORE_VALUE : DONT_STORE_VALUE, when, EFFECT_FORMAT_NATIVE);
        
        if (plugin->patch != NULL)
          GFX_ScheduleInstrumentRedraw((struct Patch*)plugin->patch);
      }
      break;

    case EFF_GRAN_onoff:
      set_granulation_enabled(plugin, data, value >= 0.5);
      break;
      
    case EFF_GRAN_coarse_stretch:
      if (value < 0.5)
        gran_set_coarse_stretch(data, 1.0 / scale_double(value, 0.5, 0.0, 1.0, MAX_stretch));
      //gran_set_stretch(data, scale_double(value, 0, 0.5, MIN_stretch, 1.0));
      else
        gran_set_coarse_stretch(data, scale_double(value, 0.5, 1.0, 1.0, MAX_stretch));
      update_editor_graphics(plugin);
      break;

    case EFF_GRAN_fine_stretch:
      if (value < 0.5)
        gran_set_fine_stretch(data, 1.0 / scale_double(value, 0.5, 0.0, 1.0, MAX_fine_stretch));
      //gran_set_stretch(data, scale_double(value, 0, 0.5, MIN_stretch, 1.0));
      else
        gran_set_fine_stretch(data, scale_double(value, 0.5, 1.0, 1.0, MAX_fine_stretch));
      update_editor_graphics(plugin);
      break;

#define GRAN_CASE(Name)                         \
      case EFF_GRAN_##Name:                                             \
        gran_set_##Name(data, scale_double(value, 0, 1, MIN_##Name, MAX_##Name)); \
        break
      ALL_GRAN_CASES();
#undef GRAN_CASE

    case EFF_GRAN_strict_no_jitter:
      gran_set_strict_no_jitter(data, value >= 0.5);      
      break;
      
    default:
      RError("S1. Unknown effect number %d\n",effect_num);
    }
  }else{
    switch(effect_num){
    case EFF_STARTPOS:
      R_ASSERT(value >= 0.0f);
      R_ASSERT(value <= 1.0f);
      data->p.startpos = value;
      update_editor_graphics(plugin);
      break;
    case EFF_FINETUNE:
      data->p.finetune = value;
      update_editor_graphics(plugin);
      break;
    case EFF_A:
      data->p.a = value;
      update_editor_graphics(plugin);
      break;
    case EFF_H:
      data->p.h = value;
      update_editor_graphics(plugin);
      break;
    case EFF_D:
      data->p.d = value;
      update_editor_graphics(plugin);
      break;
    case EFF_S:
      data->p.s = value;
      update_editor_graphics(plugin);
      break;
    case EFF_R:
      data->p.r = value;
      break;
    case EFF_PORTAMENTO:
      data->p.portamento = value;
      break;
    case EFF_VIBRATO_SPEED:
      data->p.vibrato_speed = value;
      data->p.vibrato_phase_add = data->p.vibrato_speed * 2.0 * M_PI / data->samplerate;
      break;
    case EFF_VIBRATO_DEPTH:
      data->p.vibrato_depth = value;
      break;
    case EFF_TREMOLO_SPEED:
      data->p.tremolo_speed = value;
      data->tremolo->type->set_effect_value(data->tremolo, time, 0, data->p.tremolo_speed, EFFECT_FORMAT_NATIVE, when);
      break;
    case EFF_TREMOLO_DEPTH:
      data->p.tremolo_depth = value;
      data->tremolo->type->set_effect_value(data->tremolo, time, 1, data->p.tremolo_depth, EFFECT_FORMAT_NATIVE, when);
      break;
    case EFF_NOTE_ADJUST:
      data->p.note_adjust = value;
      update_editor_graphics(plugin);
      break;
#if 0
    case EFF_OCTAVE_ADJUST:
      data->p.octave_adjust = value;
      break;
#endif

    case EFF_CROSSFADE_LENGTH:
      data->p.crossfade_length = value;
      break;

    case EFF_REVERSE:
      data->p.reverse = value>=0.5f;
      break;
      
    case EFF_PINGPONG:
      data->p.pingpong = value>=0.5f;
      break;

    case EFF_GRAN_onoff:
      set_granulation_enabled(plugin, data, value >= 0.5);
      break;

    case EFF_GRAN_coarse_stretch:
      //printf("               SETTING stretch to %f\n", value);
      gran_set_coarse_stretch(data, value);
      update_editor_graphics(plugin);
      break;

    case EFF_GRAN_fine_stretch:
      gran_set_fine_stretch(data, value);
      update_editor_graphics(plugin);
      break;

#define GRAN_CASE(Name)                                                 \
      case EFF_GRAN_##Name:                                             \
        gran_set_##Name(data, value);                                   \
        break
      ALL_GRAN_CASES();
#undef GRAN_CASE
      
    case EFF_GRAN_strict_no_jitter:
      gran_set_strict_no_jitter(data, value >= 0.5);
      break;
      
    default:
      RError("S2. Unknown effect number %d\n",effect_num);
    }
  }
}

static float get_effect_value(struct SoundPlugin *plugin, int effect_num, enum ValueFormat value_format){
  Data *data = (Data*)plugin->data;

  switch(effect_num){
    case EFF_LOOP_OVERRIDE_DEFAULT:
      return data->p.loop_override_default ? 1.0 : 0.0;
    case EFF_LOOP_START:
    case EFF_LOOP_WINDOW:
      {
        Sample &sample = data->samples[0];
        if (sample.sound==NULL){
          if (value_format==EFFECT_FORMAT_NATIVE)
            return data->p.loop_start;
          else
            return 0;
        }else{
          LoopData loop_data(sample, data->p.loop_start_was_set_last);
          if (value_format==EFFECT_FORMAT_NATIVE)
            return loop_data._start;
          else
            return scale(loop_data._start, 0, sample.num_frames, 0, 1);
        }
      }
    case EFF_LOOP_END:
      {
        Sample &sample = data->samples[0];
        if (sample.sound==NULL){
          if (value_format==EFFECT_FORMAT_NATIVE)
            return data->p.loop_end;
          else
            return 0;
        }else{
          LoopData loop_data(sample, data->p.loop_start_was_set_last);
          if (value_format==EFFECT_FORMAT_NATIVE)
            return loop_data._end;
          else
            return scale(loop_data._end, 0, sample.num_frames, 0, 1);
        }
      }
#if 0
      if (value_format==EFFECT_FORMAT_NATIVE)
        return data->p.loop_end;
      else {
        Sample &sample = data->samples[0];
        if (sample.sound==NULL)
          return 0;
        else
          return scale(data->p.loop_end, 0, sample.num_frames, 0, 1);
      }
#endif
  }
  
  if(value_format==EFFECT_FORMAT_SCALED){
    switch(effect_num){
    case EFF_STARTPOS:
      return data->p.startpos;
    case EFF_FINETUNE:
      return data->p.finetune;
    case EFF_AHDSR_ONOFF:
      return data->p.ahdsr_onoff ? 1.0 : 0.0;
    case EFF_A:
      return scale(data->p.a,
                   0,MAX_A,
                   0.0,1.0);
    case EFF_H:
      return scale(data->p.h,
                   0,MAX_H,
                   0.0,1.0);
    case EFF_D:
      return scale(data->p.d,
                   0,MAX_D,
                   0.0,1.0);
    case EFF_S:
      return scale(data->p.s,
                   0,MAX_S,
                   0.0,1.0);
    case EFF_R:
      return scale(data->p.r,
                   0,MAX_R,
                   0.0,1.0);
    case EFF_PORTAMENTO:
      return scale(data->p.portamento,
                   0,MAX_PORTAMENTO,
                   0,1);      
    case EFF_VIBRATO_SPEED:
      return scale(data->p.vibrato_speed,
                   0,MAX_VIBRATO_SPEED,
                   0.0,1.0
                   );
    case EFF_VIBRATO_DEPTH:
      return scale(data->p.vibrato_depth,
                   0,MAX_VIBRATO_DEPTH,
                   0.0,1.0
                   );      
    case EFF_TREMOLO_SPEED:
      return scale(data->p.tremolo_speed,
                   0,MAX_TREMOLO_SPEED,
                   0.0,1.0
                   );
    case EFF_TREMOLO_DEPTH:
      return scale(data->p.tremolo_depth,
                   0,MAX_TREMOLO_DEPTH,
                   0.0,1.0
                   );      
    case EFF_NOTE_ADJUST:
      return scale(data->p.note_adjust,
                   -6.99,6.99,
                   0,1);
#if 0
    case EFF_OCTAVE_ADJUST:
      return scale(data->p.octave_adjust,
                   -10.99,10.99,
                   0,1);
#endif
    case EFF_LOOP_ONOFF:
      return get_loop_onoff(data)==true?1.0f:0.0f;
      break;

    case EFF_CROSSFADE_LENGTH:
      {
        double normalized = scale_double(data->p.crossfade_length,0,MAX_CROSSFADE_LENGTH,0.0,1.0);
        return pow(normalized, 1.0/4.0);
      }
      break;

    case EFF_REVERSE:
      return data->p.reverse==true?1.0f:0.0f;
      break;

    case EFF_PINGPONG:
      return data->p.pingpong==true?1.0f:0.0f;
      break;

    case EFF_GRAN_onoff:
      return get_granulation_enabled(data)==true?1.0f:0.0f;
      break;

    case EFF_GRAN_coarse_stretch:
      {
        float stretch = gran_get_coarse_stretch(data);
        if (stretch < 1.0)
          return scale(1.0 / stretch, 1.0, MAX_stretch, 0.5, 0.0);
        else
          return scale(stretch, 1.0, MAX_stretch, 0.5, 1.0);
      }
      break;
      
    case EFF_GRAN_fine_stretch:
      {
        float stretch = gran_get_fine_stretch(data);
        if (stretch < 1.0)
          return scale(1.0 / stretch, 1.0, MAX_fine_stretch, 0.5, 0.0);
        else
          return scale(stretch, 1.0, MAX_fine_stretch, 0.5, 1.0);
      }
      break;
      
#define GRAN_CASE(Name)                                                 \
      case EFF_GRAN_##Name:                                             \
        return scale_double(gran_get_##Name(data), MIN_##Name, MAX_##Name, 0, 1); \
        break
      ALL_GRAN_CASES();
#undef GRAN_CASE

    case EFF_GRAN_strict_no_jitter:
      return gran_get_strict_no_jitter(data) >= 0.5 ? 1.0 : 0.0;
      
    default:
      RError("S3. Unknown effect number %d\n",effect_num);
      return 0.5f;
    }
  }else{
    switch(effect_num){
    case EFF_STARTPOS:
      return data->p.startpos;
    case EFF_FINETUNE:
      return data->p.finetune;
    case EFF_AHDSR_ONOFF:
      return data->p.ahdsr_onoff ? 1.0 : 0.0;
    case EFF_A:
      return data->p.a;
    case EFF_H:
      return data->p.h;
    case EFF_D:
      return data->p.d;
    case EFF_S:
      return data->p.s;
    case EFF_R:
      return data->p.r;
    case EFF_PORTAMENTO:
      return data->p.portamento;
    case EFF_VIBRATO_SPEED:
      return data->p.vibrato_speed;
    case EFF_VIBRATO_DEPTH:
      return data->p.vibrato_depth;
    case EFF_TREMOLO_SPEED:
      return data->p.tremolo_speed;
    case EFF_TREMOLO_DEPTH:
      return data->p.tremolo_depth;
    case EFF_NOTE_ADJUST:
      return data->p.note_adjust;
#if 0
    case EFF_OCTAVE_ADJUST:
      return data->p.octave_adjust;
#endif
    case EFF_LOOP_ONOFF:
      return get_loop_onoff(data)==true?1.0f:0.0f;

    case EFF_CROSSFADE_LENGTH:
      return data->p.crossfade_length;

    case EFF_REVERSE:
      return data->p.reverse==true?1.0f:0.0f;
      break;

    case EFF_PINGPONG:
      return data->p.pingpong==true?1.0f:0.0f;
      break;

    case EFF_GRAN_onoff:
      return get_granulation_enabled(data)==true?1.0f:0.0f;
      break;
      
    case EFF_GRAN_coarse_stretch:
      return gran_get_coarse_stretch(data);
      break;

    case EFF_GRAN_fine_stretch:
      return gran_get_fine_stretch(data);
      break;

#define GRAN_CASE(Name)                                                 \
      case EFF_GRAN_##Name:                                             \
        return gran_get_##Name(data);                                   \
        break;
      ALL_GRAN_CASES();
#undef GRAN_CASE

    case EFF_GRAN_strict_no_jitter:
      return gran_get_strict_no_jitter(data) >= 0.5 ? 1.0 : 0.0;
      
    default:
      RError("S4. Unknown effect number %d\n",effect_num);
      return 0.5f;
    }
  }
}

static const char *get_effect_name(const struct SoundPlugin *plugin, int effect_num);
  
static void get_display_value_string(SoundPlugin *plugin, int effect_num, char *buffer, int buffersize){
  Data *data = (Data*)plugin->data;
  switch(effect_num){
  case EFF_STARTPOS:
    snprintf(buffer,buffersize-1,"%.2f%%",scale(data->p.startpos,0.0,1.0,0,100));
    break;
  case EFF_FINETUNE:
    snprintf(buffer,buffersize-1,"%f cents",scale(data->p.finetune,0.0,1.0,-100,100));
    break;
  case EFF_A:
    snprintf(buffer,buffersize-1,"%f ms",data->p.a);
    break;
  case EFF_H:
    snprintf(buffer,buffersize-1,"%f ms",data->p.h);
    break;
  case EFF_D:
    snprintf(buffer,buffersize-1,"%f ms",data->p.d);
    break;
  case EFF_S:
    set_db_display(buffer, buffersize, gain2db(data->p.s));
    break;
  case EFF_R:
    snprintf(buffer,buffersize-1,"%f ms",data->p.r);
    break;
  case EFF_PORTAMENTO:
    snprintf(buffer,buffersize-1,"%.2fms",data->p.portamento);
    break;
  case EFF_VIBRATO_SPEED:
    snprintf(buffer,buffersize-1,"%.1fHz",data->p.vibrato_speed);
    break;
  case EFF_VIBRATO_DEPTH:
    snprintf(buffer,buffersize-1,"%.2f",data->p.vibrato_depth);
    break;
  case EFF_TREMOLO_SPEED:
    snprintf(buffer,buffersize-1,"%.1fHz",data->p.tremolo_speed);
    break;
  case EFF_TREMOLO_DEPTH:
    snprintf(buffer,buffersize-1,"%.2f",data->p.tremolo_depth);
    break;
  case EFF_NOTE_ADJUST:
    if(false && data->num_different_samples>1)
      snprintf(buffer,buffersize-1,"disabled (multi-sample instrument)");
    else{
      float adjust = data->p.note_adjust;
      snprintf(buffer,buffersize-1,"%s%.2f note%s",adjust>0?"+":"",adjust,equal_floats(adjust, -1)?"":equal_floats(adjust,1)?"":"s");
    }
    break;
#if 0
  case EFF_OCTAVE_ADJUST:
    if(false && data->num_different_samples>1)
      snprintf(buffer,buffersize-1,"disabled (multi-sample instrument)");
    else{
      int adjust=data->p.octave_adjust;
      snprintf(buffer,buffersize-1,"%s%d octave%s",adjust>0?"+":"",adjust,adjust==-1?"":adjust==1?"":"s");
    }
    break;
#endif
  case EFF_LOOP_START:
    {
      Sample &sample = data->samples[0];
      if (sample.sound!=NULL){
        LoopData loop_data(sample, data->p.loop_start_was_set_last);
        snprintf(buffer,buffersize-1,"%" PRId64 " samples", loop_data._start);
      }else
        snprintf(buffer,buffersize-1,"%" PRId64 " samples", data->p.loop_start);
    }
    break;
  case EFF_LOOP_END:
    {
      Sample &sample = data->samples[0];
      if (sample.sound!=NULL){
        LoopData loop_data(sample, data->p.loop_start_was_set_last);
        snprintf(buffer,buffersize-1,"%" PRId64 " samples", loop_data._end);
      }else
        snprintf(buffer,buffersize-1,"%" PRId64 " samples", data->p.loop_end);
    }
    break;
  case EFF_LOOP_WINDOW:
    {
      Sample &sample = data->samples[0];
      if (sample.sound!=NULL){
        LoopData loop_data(sample, data->p.loop_start_was_set_last);
        
#if ((LONG_MAX) == (INT64_MAX)) // Workaround. Sometimes PRId64 doesn't work.
        snprintf(buffer,buffersize-1,"%" PRId64 "->%ld samples", loop_data._start, loop_data._end);
#elif ((LONG_LONG_MAX) == (INT64_MAX))
        snprintf(buffer,buffersize-1,"%" PRId64 "->%lld samples", loop_data._start, loop_data._end);
#else
#       error "don't know what to do."
#endif
        
      }else {
        
#if ((LONG_MAX) == (INT64_MAX)) // Workaround. Sometimes PRId64 doesn't work.
        snprintf(buffer,buffersize-1,"%" PRId64 "->%ld samples", data->p.loop_start, data->p.loop_end);
#elif ((LONG_LONG_MAX) == (INT64_MAX))
        snprintf(buffer,buffersize-1,"%" PRId64 "->%lld samples", data->p.loop_start, data->p.loop_end);
#else
#       error "don't know what to do."
#endif
      
      }
      
    }
    break;
    
  case EFF_CROSSFADE_LENGTH:
    snprintf(buffer,buffersize-1,"%d samples",safe_int_read(&data->p.crossfade_length));
    break;

  case EFF_GRAN_coarse_stretch:
    {
      double stretch = gran_get_coarse_stretch(data);
      if (stretch < 1.0)
        snprintf(buffer,buffersize-1,"1/%.2fX" , 1.0 / stretch);
      else
        snprintf(buffer,buffersize-1,"%.2fX" , stretch);
      break;
    }
    
  case EFF_GRAN_fine_stretch:
    {
      double stretch = gran_get_fine_stretch(data);
      if (stretch < 1.0)
        snprintf(buffer,buffersize-1,"1/%.2fX" , 1.0 / stretch);
      else
        snprintf(buffer,buffersize-1,"%.2fX" , stretch);
      break;
    }
    
  case EFF_GRAN_overlap:
    snprintf(buffer, buffersize-1, "%.2fX", gran_get_overlap(data));
    break;
    
  case EFF_GRAN_length:
    snprintf(buffer, buffersize-1, "%.2fms", gran_get_length(data));
    break;
    
  case EFF_GRAN_ramp:
    snprintf(buffer, buffersize-1, "%.2f%%", 100.0*gran_get_ramp(data));
    break;
    
  case EFF_GRAN_jitter:
    snprintf(buffer, buffersize-1, "%.2f%%", 100.0*gran_get_jitter(data));
    break;
    
  case EFF_GRAN_volume:
    set_db_display(buffer, buffersize, gran_get_volume(data));
    break;
    
  default:
    RError("S5. Unknown effect number %d\n",effect_num);
  }
}
             

// Called after loading sample.
// Note, if start==-1 and end==-1, loop_start is set to 0 and loop_end is set to sample->num_frames, and loop_onoff is not set.
static void set_legal_loop_points(Sample &sample, int64_t start, int64_t end, bool set_loop_on_off){
  if(start==-1 && end==-1){
    
    sample.loop_start=0;
    sample.loop_end=sample.num_frames;

  } else {

    if(start<0)
      start=0;

    if(end>sample.num_frames)
      end=sample.num_frames;

    if(end<=start){
      sample.loop_start=0;
      sample.loop_end=sample.num_frames;
    }else{
      sample.loop_start=start;
      sample.loop_end=end;
      if (set_loop_on_off)
        ATOMIC_SET(sample.data->p.loop_onoff, true);
    }

  }

  sample.loop_start_org = sample.loop_start;
  sample.loop_end_org = sample.loop_end;
}


#include "Sampler_plugin_wav_parse.c"
#include "Sampler_plugin_xi_load.c"
#include "Sampler_plugin_sf2_load.c"

static float *load_interleaved_samples(filepath_t filename, SF_INFO *sf_info){
  SNDFILE *sndfile = radium_sf_open(filename,SFM_READ,sf_info);
  
  if(sndfile==NULL)
    return NULL;

  int64_t alloc_size = sizeof(float) * sf_info->channels * sf_info->frames;

  if (alloc_size > INT32_MAX){
    GFX_Message(NULL, "File too large");
    return NULL;
  }


  float   *ret              = (float*)talloc_atomic((int)alloc_size);
  int64_t  allocated_frames = sf_info->frames;

  int64_t total_read_frames = sf_readf_float(sndfile, ret, sf_info->frames);

  if(total_read_frames==0){
    fprintf(stderr, "   libsndfile could open the file, but couldn't read data from it\n");
    return NULL;
  }

  while(true){
    float samples[1024*sf_info->channels];
    int64_t read_now = sf_readf_float(sndfile, samples, 1024);
    if(read_now==0)
      break;

    if(total_read_frames + read_now > allocated_frames){ // what's happening here?
      allocated_frames = (total_read_frames+read_now) * 2;
      ret = (float*)talloc_realloc(ret, (int)(allocated_frames * sizeof(float) * sf_info->channels));
    }

    memcpy(ret + (total_read_frames*sf_info->channels), samples, sizeof(float)*1024*sf_info->channels);

    total_read_frames += read_now;
  }

  sf_close(sndfile);

  sf_info->frames = total_read_frames;
  return ret;
}

static bool load_sample_with_libsndfile(Data *data, filepath_t filename, bool set_loop_on_off){
  EVENTLOG_add_event(talloc_format("load_sample_with_libsndfile -%S-", filename.id));
    
  SF_INFO sf_info; memset(&sf_info,0,sizeof(sf_info));

  data->num_different_samples = 1;

  float *samples = load_interleaved_samples(filename, &sf_info);

  if(samples==NULL){
    fprintf(stderr,"  Libsndfile could not open file \"%S\"\n", filename.id);
    return false;
  }

  {
    int num_channels = sf_info.channels;

    //printf("Num channels: %d\n",num_channels);

    if(num_channels > 2) // TODO
      num_channels = 2;

    int ch;
    for(ch=0;ch<num_channels;ch++){
      Sample &sample     = data->samples[ch];
      sample.num_frames = sf_info.frames;
      sample.sound       = (float*)V_malloc(sizeof(float)*sample.num_frames);
    }

    int interleaved_pos=0;
    int i;
    for(i=0;i<sf_info.frames;i++){
      for(ch=0;ch<sf_info.channels;ch++){
        if(ch<2){
          Sample &sample=data->samples[ch];
          sample.sound[i] = samples[interleaved_pos];
#if !defined(RELEASE)
          sample.filename = filename;
          sample.ch = ch;
#endif
        }
        interleaved_pos++;
      }
    }

    Note *note = new Note;
    data->note_storage.push_back(note);

    for(ch=0;ch<num_channels;ch++){     
      Sample &sample=data->samples[ch];

      double middle_note = 48;

      set_legal_loop_points(sample,-1,-1, set_loop_on_off); // By default, don't loop, but if set, loop all.
              
      if((sf_info.format&0xffff0000) == SF_FORMAT_WAV){
        //printf("format: 0x%x. sections: %d, num_frames: %d. SF_FORMAT_WAV: 0x%x. og: 0x%x\n",sf_info.format,sf_info.sections,(int)sf_info.frames,SF_FORMAT_WAV,sf_info.format&SF_FORMAT_WAV);
        set_wav_loop_points(sample,filename,set_loop_on_off);
        if (data->use_sample_file_middle_note)
          middle_note = get_wav_middle_note(filename, middle_note);
      }
      
      if(num_channels==1)
        sample.ch = -1; // i.e play the sample in both channels.
      else
        sample.ch = ch;

      note->samples.push_back(&sample);
      
      for(int i=0;i<128;i++){
        //data->notes[i] = note;
        /*
        Note *note=(Note*)data->notes[i];
        
        note->samples.push_back(&sample);
        */
        //note->num_samples = num_channels;
        //note->samples[ch] = &sample;

        sample.frequency_table[i] = sf_info.samplerate * midi_to_hz(i)/midi_to_hz(middle_note);
        
        //printf("%d: %f, data: %f, sample: %f, midifreq: %f\n",i,sample.samplerate,(float)data->samplerate,(float)sf_info.samplerate,midi_to_hz(i));
      }
    }

    for(int i=0;i<128;i++)
      data->notes[i] = note;
  }

  return true;
}

static void generate_peaks(Data *data){
  float *prev=NULL;
  int sample_num;

  for(sample_num=0;sample_num<MAX_NUM_SAMPLES;sample_num++){
    Sample &sample=data->samples[sample_num];

    if (sample.sound==NULL)
      break;
    
    if(sample.sound!=NULL && sample.sound != prev){
      prev = sample.sound;

      sample.peaks->add_samples(sample.sound, sample.num_frames, radium::Peaks::THIS_IS_THE_LAST_CALL);

#if 0 //!defined(RELEASE)
      if (sample.filename!=NULL){
        radium::DiskPeaks *disk_peaks = DISKPEAKS_get(sample.filename);
        if(disk_peaks->is_valid()==false)
          abort();
        disk_peaks->wait();
        sample.peaks = disk_peaks->_peaks[sample.ch==-1 ? 0 : sample.ch];
      }
#endif

    }

  }
}

namespace{
  struct GetSampleCallback : public ResettableGranResamplerCallback{
    Voice &_voice;

    GetSampleCallback(Voice &voice)
      : _voice(voice)
    {}

    ~GetSampleCallback(){
      //printf("        YES %d\n", _voice.voice_num);
      //getchar();
    }
          
    int _buffer_size = 0;
    float *_buffer;

    void reset2(void) override {
      _buffer_size = 0;
    }

    void maybe_fill_buffer(void){
      if (_buffer_size==0)
        _buffer_size = RT_src_callback(&_voice, &_buffer);
    }

    float *get_next_granresampler_sample_block(const int ch, int &num_frames) override {
      maybe_fill_buffer();

      float *ret = _buffer;
      num_frames = R_MIN(1024, _buffer_size);

      _buffer_size -= num_frames;
      _buffer += num_frames;

      return ret;
    }

    int pick_up_data_for_granulator(float **samples, int max_num_frames) override {
      maybe_fill_buffer();

      int ret = R_MIN(_buffer_size, R_MIN(1024, max_num_frames));

      memcpy(samples[0], _buffer, ret*sizeof(float));

      _buffer_size -= ret;
      _buffer += ret;
      
      return ret;
    }

  };
}

static void init_voice(Data *data, Voice &voice){
  //voice.resampler = RESAMPLER_create(RT_src_callback, 1, &voice, data->resampler_type);
  voice.adsr = ADSR_create(data->samplerate);
  RT_add_voice(&data->voices_not_playing, &voice);

  R_ASSERT(voice._get_samples.get()==NULL);
  
  auto *get_samples = new GetSampleCallback(voice);
  voice._get_samples = std::unique_ptr<GetSampleCallback>(get_samples);

  voice._resampler2 = new radium::Resampler2(1, data->resampler_type, get_samples);

  voice._granresampler = voice._resampler2;
  
  //voice._resampler2->set_callback(voice._granulator);
  //voice._granulator.set_callback(get_samples);
}

static void release_voice(Voice &voice){
  //RESAMPLER_delete(voice.resampler);
  ADSR_delete(voice.adsr);
  
  // more debugging, trying to track down a crash.
  //voice.resampler = NULL;
  voice.adsr = NULL;

  delete voice._resampler2;
  //delete voice._granulator;

  if(voice._granulator!=NULL){
    radium::PlayerLock lock; // To avoid priority inversion in RT_release_granulator while holding spinlock. This should only happen if instrument is removed while playing, so probably no need to optimize.
    RT_release_voice_for_playing_with_granulation(&voice);
  }
}


static bool load_sample(Data *data, filepath_t filename, int instrument_number, bool set_loop_on_off){
  if(load_xi_instrument(data,filename, set_loop_on_off)==false){
    if(load_sample_with_libsndfile(data,filename, set_loop_on_off)==false){
      if(load_sf2_instrument(data,filename,instrument_number, set_loop_on_off)==false){        
        GFX_Message(NULL,"Unable to load soundfile %S.", filename.id);
        return false;
      }
    }
  }

  for(int i=0;i<MAX_NUM_SAMPLES;i++){
    Sample &sample=data->samples[i];
    if (sample.sound==NULL){
      if (i==0){
        R_ASSERT(false);
        return false;
      }
        
      break;
    }
    
    if (sample.num_frames < 0){
      R_ASSERT(false);
      return false;
    }
    
    if (sample.num_frames < MIN_LOOP_LENGTH) {
      GFX_Message(NULL,"Unable to load soundfile %S. File contains too few samples: %d", filename.id, (int)sample.num_frames);
      return false;
    }
  }
  
  //data->num_channels = data->samples[0].num_channels; // All samples must contain the same number of channels.

  generate_peaks(data);

  // Instead of calling RESAMPLER_reset/ADSR_reset every time we load a new sample, we recreate resampler and adsr instead. I.e. 'data' is recreated every time.
  for(int i=0;i<POLYPHONY;i++)
    init_voice(data, data->voices[i]);

  return true;
}

static SoundPlugin *create_tremolo(bool is_loading){
  SoundPlugin *ret = (SoundPlugin*)V_calloc(1, sizeof(SoundPlugin));
  
  ret->type = PR_get_plugin_type_by_name(NULL, "Faust", "System Tremolo");
  ret->data = ret->type->create_plugin_data(ret->type, ret, NULL, MIXER_get_sample_rate(), MIXER_get_buffer_size(), is_loading);
  
  return ret;
}

static void free_tremolo(SoundPlugin *tremolo){
  tremolo->type->cleanup_plugin_data(tremolo);
  V_free(tremolo);
}

static Data *create_data(float samplerate, Data *old_data, filepath_t filename, int instrument_number, enum ResamplerType resampler_type, bool use_sample_file_middle_note, bool is_loading){
  Data *data = new Data(filename);

  data->signal_from_RT = RSEMAPHORE_create(0);

  data->tremolo = create_tremolo(is_loading);
      
  if(old_data==NULL){

    data->p.finetune = 0.5f;

    data->p.ahdsr_onoff = true;
    
    data->p.a=DEFAULT_A;
    data->p.d=DEFAULT_D;
    data->p.s=DEFAULT_S;
    data->p.r=DEFAULT_R;

    data->p.vibrato_phase_add = -1;
    
    data->p.gran_coarse_stretch = 1;
    data->p.gran_fine_stretch = 1;
    data->p.gran_volume = -6;

  }else{

    data->p = old_data->p;
 
    data->p.vibrato_value = 0.0;
    data->p.vibrato_phase = 4.71239;
    
    data->tremolo->type->set_effect_value(data->tremolo, 0, 0, data->p.tremolo_speed, EFFECT_FORMAT_NATIVE, FX_single);
    data->tremolo->type->set_effect_value(data->tremolo, 0, 1, data->p.tremolo_depth, EFFECT_FORMAT_NATIVE, FX_single);
    
  }

  data->use_sample_file_middle_note = use_sample_file_middle_note;

  data->samplerate = samplerate;
  data->resampler_type = resampler_type;
  data->instrument_number = instrument_number;

  int i;
  for(i=0;i<MAX_NUM_SAMPLES;i++){
    Sample &sample=data->samples[i];
    sample.data = data;
  }

  return data;
}

// State is not used here. We just create the default instrument. State is used in recreate_from_state instead.
static void *create_plugin_data(const SoundPluginType *plugin_type, struct SoundPlugin *plugin, hash_t *state, float samplerate, int block_size, bool is_loading){

  //const char *filename="/home/kjetil/brenn/downloaded/temp/CATEGORY/SYNTH/PAD/NAMED1/etrnpadl.xi"; // one sample
  //const char *filename="/home/kjetil/brenn/downloaded/temp/CATEGORY/SYNTH/PAD/NAMED1/elecpad.xi"; // multile samples
  //const char *filename="/home/kjetil/brenn/downloaded/temp/CATEGORY/SYNTH/PAD/NAMED1/flpad2.xi"; // multile samples
  //const char *filename="/home/kjetil/brenn/downloaded/temp/CATEGORY/SYNTH/PAD/NAMED1/BIGPAD1.XI"; // multile samples
  //const char *filename="/home/kjetil/brenn/downloaded/temp/waveworld/wav/synths/temp/prophe07.wav";
  //const char *filename="/gammelhd/home/kjetil/poing-imperatif/karin.wav";
  //const char *filename="/home/kjetil/brenn/downloaded/temp/CATEGORY/BASS/ACOUSTIC/acbs02r.xi";
  //const char *filename="/home/kjetil/brenn/downloaded/temp/CATEGORY/SYNTH/SEQUENCE/SH101/sh101sq1.xi";
  //const char *filename="/home/kjetil/brenn/downloaded/temp/CATEGORY/BELL/CHURCH/CHRBEL01.XI";
  //const char *filename="/gammelhd/gammelhd/gammel_lyd/2_channel_short.wav";
  //const char *filename="/gammelhd/gammelhd/gammelhd/gammel_lyd/d_lydfiler/instrument/keyboard/mellotron.sf2";
  filepath_t default_sound_filename = appendFilePaths(OS_get_program_path2(),
                                                      appendFilePaths(make_filepath(L"sounds"),
                                                                      !strcmp(plugin_type->name, g_click_name)
                                                                      ? make_filepath(L"243749__unfa__metronome-1khz-weak-pulse.flac")
                                                                      : make_filepath(L"016.WAV")));
  
  Data *data = create_data(samplerate,NULL,default_sound_filename,0,RESAMPLER_CUBIC, true, is_loading); // cubic is the default

  // Add a little bit release to click instrument to avoid noise when unpressing the "Click" button in the bottom bar.
  if (!strcmp(plugin_type->name, g_click_name)){
    data->p.r = 50;
  }

  if(load_sample(data,default_sound_filename,0, true)==false){
    delete data;
    return NULL;
  }

  data->using_default_sound = true;

  return data;
}

static void delete_data(Data *data){
  int i;

  float *prev=NULL;
  R_ASSERT_RETURN_IF_FALSE(data!=NULL);

  EVENTLOG_add_event("sampler_plugin: delete_data 1");
    
  for(i=0;i<MAX_NUM_SAMPLES;i++){
    Sample &sample=data->samples[i];
        
    if(sample.sound!=NULL && sample.sound != prev){
      prev = sample.sound;
      V_free(sample.sound);
      
      //V_free(sample.min_peaks);
      //V_free(sample.max_peaks);
    }
  }

  // null out, trying to track down a crash.
  for(i=0;i<MAX_NUM_SAMPLES;i++){
    Sample &sample=data->samples[i];
    sample.sound = NULL;
  }

  EVENTLOG_add_event("sampler_plugin: delete_data 2");
  
  for(i=0;i<POLYPHONY;i++)
    release_voice(data->voices[i]);

  EVENTLOG_add_event("sampler_plugin: delete_data 3");
  
  RSEMAPHORE_delete(data->signal_from_RT);

  free_tremolo(data->tremolo);

  delete data->recorder_instance;

  if (data->gui.data() != NULL)
    data->gui->close();
      
  delete data;
}

void SAMPLER_set_loop_data(struct SoundPlugin *plugin, int64_t start, int64_t length){
  R_ASSERT_RETURN_IF_FALSE(!strcmp("Sample Player", plugin->type->type_name));
  
  Data *data=(Data*)plugin->data;

  PLAYER_lock();{
    PLUGIN_set_effect_value(plugin, -1, EFF_LOOP_OVERRIDE_DEFAULT, 1.0f, STORE_VALUE, FX_single, EFFECT_FORMAT_NATIVE);
    RT_set_loop_points_complete(plugin, data, start, start+length);
    PLUGIN_set_effect_value(plugin, -1, EFF_LOOP_ONOFF, length > 0 ? 1.0f : 0.0f, STORE_VALUE, FX_single, EFFECT_FORMAT_NATIVE);
  }PLAYER_unlock();

  update_editor_graphics(plugin);

  {
    struct Patch *patch = plugin->patch;
    if(patch!=NULL)
      GFX_update_instrument_widget((struct Patch*)patch);
  }
}

static bool set_new_sample(struct SoundPlugin *plugin,
                           filepath_t filename,
                           int instrument_number,
                           enum ResamplerType resampler_type,
                           bool use_sample_file_middle_note,
                           bool is_loading,
                           int64_t loop_start = -1,
                           int64_t loop_end = -1)
{
  bool success=false;

  Data *data = NULL;
  Data *old_data = (Data*)plugin->data;

#if !defined(RELEASE)
  if (loop_start < 0 || loop_end < 0)
    R_ASSERT(loop_start==-1 && loop_end==-1);
  else if (loop_start != 0 || loop_end != 0)
    R_ASSERT(loop_end > loop_start);
#endif
  
  /*
  int64_t org_loop_start = data->loop_start; //PLUGIN_get_effect_value2(plugin, EFF_LOOP_START, VALUE_FROM_STORAGE, EFFECT_FORMAT_NATIVE);
  int64_t org_loop_end = data->loop_end; //PLUGIN_get_effect_value2(plugin, EFF_LOOP_END, VALUE_FROM_STORAGE, EFFECT_FORMAT_NATIVE);
  */

  //bool org_loop_override_default = data->p.loop_override_default;
  //float loop_override_default = PLUGIN_get_effect_value2(plugin, EFF_LOOP_OVERRIDE_DEFAULT, VALUE_FROM_STORAGE, EFFECT_FORMAT_NATIVE);
  

  filename = OS_loading_get_resolved_file_path(filename, false); // set program_state_is_valid=false. Might not be necessary, but I'm not sure.
  if (isIllegalFilepath(filename))
    goto exit;

  data = create_data(old_data->samplerate, old_data, filename, instrument_number, resampler_type, use_sample_file_middle_note, is_loading);
  
  if(load_sample(data,filename,instrument_number, false)==false)
    goto exit;


  if (keepOldLoopWhenLoadingNewSample()){

    int64_t loop_start2 = loop_start >= 0 ? loop_start : data->p.loop_start;
    int64_t loop_end2 = loop_end >= 0 ? loop_end : data->p.loop_end;
    
    if (useSameLoopFramesWhenLoadingNewSample()) {
      
      RT_set_loop_points_complete(plugin, data, loop_start2, loop_end2);
        
    } else {
      
      int64_t org_num_frames = old_data->samples[0].sound==NULL ? 0 : old_data->samples[0].num_frames;
      int64_t new_num_frames = data->samples[0].sound==NULL ? 0 : data->samples[0].num_frames;
      
      if (org_num_frames > 0 && new_num_frames > 0) {
        
        RT_set_loop_points_complete(plugin, data,
                                    scale_int64(loop_start2, 0, org_num_frames, 0, new_num_frames),
                                    scale_int64(loop_end2, 0, org_num_frames, 0, new_num_frames)
                                    );
        
      } else {
        
        R_ASSERT_NON_RELEASE(false);
        
      }
      
    }
    
  } else {

    int64_t loop_start2 = loop_start >= 0 ? loop_start : 0;
    int64_t loop_end2 = loop_end >= 0 ? loop_end : 0;

    //int64_t new_num_frames = data->samples[0]==NULL ? 0 : data->samples[0].num_frames;

    //printf("**STARTING. Org loop points: %d -> %d. New loop points: %d -> %d\n", (int)old_data->p.loop_start, (int)old_data->p.loop_end, (int)data->p.loop_start, (int)data->p.loop_end);
    
    PLUGIN_set_effect_value(plugin, -1, EFF_LOOP_OVERRIDE_DEFAULT, 0.0f, STORE_VALUE, FX_single, EFFECT_FORMAT_NATIVE); // turn off override default.
    RT_set_loop_points_complete(plugin, data, loop_start2, loop_end2, true);
    
    // Put loop_onoff into storage.
    //PLUGIN_set_effect_value(plugin, -1, EFF_LOOP_ONOFF, ATOMIC_GET(data->p.loop_onoff)==true?1.0f:0.0f, STORE_VALUE, FX_single, EFFECT_FORMAT_NATIVE);

  }
  


  if(SP_is_plugin_running(plugin)){

    //fprintf(stderr, "    *************** 11111. plugin IS running **********\n");
    
    ATOMIC_SET(old_data->new_data, data);

    RSEMAPHORE_wait(old_data->signal_from_RT,1);

  } else {

    //fprintf(stderr, "    *************** 0000. plugin is NOT running **********\n");
    
    plugin->data = data;
  
    
  }


  {
    data->gui = old_data->gui.data();
    ATOMIC_SET_RELAXED(data->rtwidget_pos, ATOMIC_GET(old_data->rtwidget_pos));
    old_data->gui = NULL;
  }
  
  delete_data(old_data);

  update_editor_graphics(plugin);

  {
    volatile struct Patch *patch = plugin->patch;
    if(patch!=NULL)
      GFX_update_instrument_widget((struct Patch*)patch); // Update "loop" button.
  }
  
  success = true;

 exit:
  if(success==false)
    delete data;

  return success;
}


bool SAMPLER_set_new_sample(struct SoundPlugin *plugin, filepath_t filename, int instrument_number){
  R_ASSERT_RETURN_IF_FALSE2(!strcmp("Sample Player", plugin->type->type_name), false);

  Data *data=(Data*)plugin->data;
  if (!set_new_sample(plugin,filename,instrument_number,data->resampler_type, true, false))
    return false;

  struct Patch *patch = plugin->patch;
  if(patch!=NULL){
    //printf("       1. UPDATE %s\n", patch->name);
    GFX_update_instrument_widget(patch);
  }
  
  return true;
}


bool SAMPLER_set_random_sample(struct SoundPlugin *plugin, filepath_t path){
  R_ASSERT_RETURN_IF_FALSE2(!strcmp("Sample Player", plugin->type->type_name), false);
    
  bool is_default_sound;

  QDir dir;
  
  if (isIllegalFilepath(path)){
    
    filepath_t filename = SAMPLER_get_filename(plugin, &is_default_sound);
    
    QFileInfo info(STRING_get_qstring(filename.id));
  
    dir = info.absoluteDir();
    
  } else {
    
    dir = QDir(STRING_get_qstring(path.id));
    
  }
  
  
  if (dir.exists()==false){
    fprintf(stderr, "SAMPLER_set_random_sample: Directory %s does not exist", dir.absolutePath().toUtf8().constData());
    GFX_Message2(NULL, true, "Directory %s does not exist", dir.absolutePath().toUtf8().constData());
    return false;
  }
  
  QStringList list = dir.entryList(get_sample_name_filters(), QDir::Files|QDir::NoDotAndDotDot|QDir::Readable);

  if(list.size()==0){
    GFX_Message2(NULL, true, "No samples found in %s", dir.absolutePath().toUtf8().constData());
    return false;
  }
  
  QString new_filename = dir.absoluteFilePath(list.at(rand() % list.size()));

  //printf("*********** filename: -%s-\n", new_filename.toUtf8().constData());
  
  if (SAMPLER_set_new_sample(plugin, make_filepath(new_filename), 0)==false){
    //GFX_Message(NULL, "Unable to set sample %s", new_filename.toUtf8().constData()); // SAMPLER_set_new_sample has already given a message.
    return false;
  }
  
  return true;
}


bool SAMPLER_set_temp_resampler_type(struct SoundPlugin *plugin, enum ResamplerType resampler_type){
  Data *data=(Data*)plugin->data;
  data->org_resampler_type = resampler_type;
  if (resampler_type != data->resampler_type)
    return set_new_sample(plugin,data->filename.get(),data->instrument_number,resampler_type, data->use_sample_file_middle_note, false);
  else
    return true;
}

void SAMPLER_set_org_resampler_type(struct SoundPlugin *plugin){
  Data *data=(Data*)plugin->data;
  if (data->org_resampler_type != data->resampler_type)
    set_new_sample(plugin,data->filename.get(),data->instrument_number,data->org_resampler_type, data->use_sample_file_middle_note, false);
}

bool SAMPLER_set_resampler_type(struct SoundPlugin *plugin, enum ResamplerType resampler_type){
  R_ASSERT_RETURN_IF_FALSE2(!strcmp("Sample Player", plugin->type->type_name), false);
  
  Data *data=(Data*)plugin->data;
  if (resampler_type != data->resampler_type)
    return set_new_sample(plugin,data->filename.get(),data->instrument_number,resampler_type, data->use_sample_file_middle_note, false);
  else
    return true;
}

enum ResamplerType SAMPLER_get_resampler_type(struct SoundPlugin *plugin){
  R_ASSERT_RETURN_IF_FALSE2(!strcmp("Sample Player", plugin->type->type_name), RESAMPLER_NON);
  
  Data *data=(Data*)plugin->data;
  return data->resampler_type;
}

// Has been used for debugging. Not sure if I planned to use it for anything else.
void SAMPLER_save_sample(struct SoundPlugin *plugin, filepath_t filename, int sample_number){
  R_ASSERT_RETURN_IF_FALSE(!strcmp("Sample Player", plugin->type->type_name));
  
  Data *data = (Data*)plugin->data;
  const Sample &sample = data->samples[sample_number];

  SF_INFO sf_info; memset(&sf_info,0,sizeof(sf_info));

  sf_info.samplerate = 22050;
  sf_info.channels = 1;
  sf_info.format = SF_FORMAT_WAV | SF_FORMAT_PCM_16;

  if(sf_format_check(&sf_info)==0){
    fprintf (stderr, "\nFileformat not supported by libsndfile.\n");
    return;
  }

  SNDFILE *sndfile = radium_sf_open(filename,SFM_WRITE,&sf_info);

  if(sndfile==NULL){
    fprintf(stderr,"could not open file\n");
    return;
  }

  sf_writef_float(sndfile,sample.sound,sample.num_frames);

  sf_close(sndfile);
}

void SAMPLER_start_recording(struct SoundPlugin *plugin, filepath_t pathdir, int num_channels, bool recording_from_main_input){
#if !defined(RELEASE)
  R_ASSERT_RETURN_IF_FALSE(!strcmp("Sample Player", plugin->type->type_name));
#endif

  if (MIXER_dummy_driver_is_running())
    return;
  
  R_ASSERT_RETURN_IF_FALSE(num_channels > 0);

  Data *data = (Data*)plugin->data;

  if (ATOMIC_GET(data->recording_status) != NOT_RECORDING)
    return;

  filepath_t recording_path = appendFilePaths(pathdir,DISK_create_legal_filename(make_filepath(STRING_create(plugin->patch->name))));
  
  QDir dir = QFileInfo(STRING_get_qstring(recording_path.id)).dir();
  if (dir.exists()==false){
    GFX_Message2(NULL, true, "Error. Could not find the directory \"%s\".\n", dir.absolutePath().toUtf8().constData());
    return;
  }
  
  data->recording_from_main_input = recording_from_main_input;
  ATOMIC_SET(data->recording_status, BEFORE_RECORDING);

  int64_t latency = 0;
  
  if(recording_from_main_input){
    
    latency = MIXER_get_recording_latency_compensation_from_system_in(); // Recording latency from the sound card + Playback latency from the sound card.
    latency += MIXER_get_latency_for_main_system_out(); // In case there are parallel running plugins introducing latency in the chain.
    
  } else {

    auto *soundproducer = SP_get_sound_producer(plugin);
    if (soundproducer==NULL){
      R_ASSERT_NON_RELEASE(false);
    }else{
      latency = RT_SP_get_input_latency(soundproducer);
    }

  }
  
    
  auto *old = data->recorder_instance;
  
  data->recorder_instance = new MySampleRecorderInstance(const_cast<struct Patch*>(plugin->patch), recording_path, num_channels, latency);
  
  delete old; // Wait before deleting this one since data from it is being used here and there (although it shouldn't really be used after finished recording);
}

const char *SAMPLER_get_recording_status(struct SoundPlugin *plugin){
#if !defined(RELEASE)
  R_ASSERT_RETURN_IF_FALSE2(!strcmp("Sample Player", plugin->type->type_name), NULL);
#endif
  
  Data *data = (Data*)plugin->data;
    
  int status = ATOMIC_GET(data->recording_status);
  
  switch(status){
    case NOT_RECORDING: return "Record";
    case BEFORE_RECORDING: return "Waiting for note...";
    case IS_RECORDING: return "Recording";
  }
  
  return "(Error)"; // not supposed to happen
}

static void cleanup_plugin_data(SoundPlugin *plugin){
  printf(">>>>>>>>>>>>>> Cleanup_plugin_data called for %p\n",plugin);
  Data *data=(Data*)plugin->data;

  delete_data(data);
}

static const char *get_effect_name(const struct SoundPlugin *plugin, int effect_num){
  switch(effect_num){
  case EFF_STARTPOS:
    return "Start Position";
  case EFF_FINETUNE:
    return "Finetune";
  case EFF_AHDSR_ONOFF:
    return "AHDSR";
  case EFF_A:
    return "Attack";
  case EFF_H:
    return "Hold";
  case EFF_D:
    return "Decay";
  case EFF_S:
    return "Sustain";
  case EFF_R:
    return "Release";
  case EFF_PORTAMENTO:
    return "Portamento";
  case EFF_VIBRATO_SPEED:
    return "Vibrato Speed";
  case EFF_VIBRATO_DEPTH:
    return "Vibrato Depth";
  case EFF_TREMOLO_SPEED:
    return "Tremolo Speed";
  case EFF_TREMOLO_DEPTH:
    return "Tremolo Depth";
  case EFF_NOTE_ADJUST:
    return "Note adjustment";
#if 0
  case EFF_OCTAVE_ADJUST:
    return "Octave adjustment";      
#endif
  case EFF_LOOP_ONOFF:
    return "Loop";
  case  EFF_LOOP_OVERRIDE_DEFAULT:
    return "Custom loop start/end";
  case EFF_LOOP_START:
    return "Loop start";
  case EFF_LOOP_END:
    return "Loop end";
  case EFF_LOOP_WINDOW:
    return "Loop window";
  case EFF_CROSSFADE_LENGTH:
    return "Crossfade";
  case EFF_REVERSE:
    return "Reverse";
  case EFF_PINGPONG:
    return "Ping-Pong Loop";
  case EFF_GRAN_onoff:
    return "Granulate";
  case EFF_GRAN_coarse_stretch:
    return "Gran. Stretch";
  case EFF_GRAN_fine_stretch:
    return "Gran. Fine Stretch";
  case EFF_GRAN_overlap:
    return "Grain Overlap";
  case EFF_GRAN_length:
    return "Grain Length";
  case EFF_GRAN_ramp:
    return "Grain Ramp";
  case EFF_GRAN_jitter:
    return "Gran. Jitter";
  case EFF_GRAN_strict_no_jitter:
    return "Gran. Strict no Jitter";
  case EFF_GRAN_volume:
    return "Gran. Volume";
  default:
    RError("S6. Unknown effect number %d\n",effect_num);
    return NULL;
  }
}

static int get_effect_format(struct SoundPlugin *plugin, int effect_num){
  if(effect_num==EFF_LOOP_ONOFF || effect_num==EFF_REVERSE || effect_num==EFF_PINGPONG || effect_num==EFF_GRAN_onoff || effect_num==EFF_GRAN_strict_no_jitter || effect_num==EFF_LOOP_OVERRIDE_DEFAULT || effect_num==EFF_AHDSR_ONOFF)
    return EFFECT_FORMAT_BOOL;
  else if (effect_num==EFF_CROSSFADE_LENGTH)
    return EFFECT_FORMAT_INT;
  else
    return EFFECT_FORMAT_FLOAT;
}

static const char *get_effect_description(struct SoundPlugin *plugin, int effect_num){
  //Data *data=(Data*)plugin->data;

#define STUFF "\n\nOpen GUI to visualize."
  
  switch(effect_num){
    case EFF_GRAN_onoff:
      return "Enable granular synthesis.\n"
        "Granular synthesis cuts the sound into smaller pieces (\"grains\") and plays them back in various ways." STUFF;
    case EFF_GRAN_coarse_stretch:
      return "How much to stretch the sound (coarse)";
    case EFF_GRAN_fine_stretch:
      return "How much to stretch the sound (fine)";
    case EFF_GRAN_overlap:
      return "How much each grain overlap on average." STUFF;
    case EFF_GRAN_length:
      return "The duration of each grain." STUFF;
    case EFF_GRAN_ramp:
      return "The fade in / fade out duration of each grain. 33%, or thereabout, usually works well." STUFF;
    case EFF_GRAN_jitter:
      return "A jitter of 0% means a juniform distribution of the grains. A higher jitter value should reduce comb filter effects." STUFF;
    case EFF_GRAN_strict_no_jitter:
      return
        "Strict no jitter when jitter is 0.00%.\n"
        "\n"
        "If set, the distance between the start of all grains will always be the same when jitter is 0.00%.\n"
        "\n"
        "The duration of the generated sound will be slightly wrong if this mode is set,\n"
        "but the sound will contain a purer comb filter effect, if you are looking for that effect.\n"
        "\n"
        "If this mode is not set, the distances will differ in size by at most 1 frame in such a way\n"
        "that the total duration of the generated sound will be correct, at the cost of a less pure\n"
        "comb filter effect.\n"
        "\n"
        "It's easier to hear the difference if overlap is set high, and grain length is set low."
        ;
    case EFF_GRAN_volume:
      return "Volume compensation for granulation.";
  }
  
  return "";
}

static bool gui_is_visible(struct SoundPlugin *plugin){
  Data *data=(Data*)plugin->data;

  //printf("  Is visible called\n");

  auto *gui = data->gui.data();
  
  if (gui==NULL)
    return false;

  return gui->isVisible();
}

static bool show_gui(struct SoundPlugin *plugin, int64_t parentgui){
  Data *data=(Data*)plugin->data;

  printf("  show_gui called\n");
  
  if (data->gui.data()==NULL){
                                     
    struct Patch *patch = (struct Patch*)plugin->patch;
    
    int64_t guinum = S7CALL2(int_instrument, "FROM_C-create-granular-vizualization-gui-for-sample-player", patch->id);

    data->gui = API_gui_get_widget(guinum);

  }

  if (ATOMIC_GET_RELAXED(data->rtwidget_pos) == -1)
    ATOMIC_SET_RELAXED(data->rtwidget_pos, RTWIDGET_allocate_slot(data->gui.data()));

  data->gui->show();

  return true;
}

static void hide_gui(struct SoundPlugin *plugin){
  Data *data=(Data*)plugin->data;
  
  if(data->gui.data() != NULL){
    if (ATOMIC_GET_RELAXED(data->rtwidget_pos) != -1){
      RTWIDGET_release_slot(ATOMIC_GET_RELAXED(data->rtwidget_pos));
      ATOMIC_SET_RELAXED(data->rtwidget_pos, -1);
    }
    data->gui->hide();
  }
}

/*
static QString get_final_embedded_filename(QString org_filename, QString new_filename){
}
*/

static void create_state(const struct SoundPlugin *plugin, hash_t *state);
  
static void recreate_from_state(struct SoundPlugin *plugin, hash_t *state, bool is_loading){
  Data *data=(Data*)plugin->data;
    
  {
    hash_t *curr_state = HASH_create(10);
    create_state(plugin, curr_state);
    if (HASH_equal(state, curr_state))
      goto exit;
  }

  {
    bool           use_sample_file_middle_note = true;
    int            instrument_number = HASH_get_int32(state, "instrument_number");
    enum ResamplerType resampler_type = (enum ResamplerType)HASH_get_int(state, "resampler_type");
    int64_t        loop_start        = -1;
    int64_t        loop_length       = 0;

    if (HASH_has_key(state, "use_sample_file_middle_note"))
      use_sample_file_middle_note = HASH_get_bool(state, "use_sample_file_middle_note");
    
    if (HASH_has_key(state, "loop_start"))
      loop_start  = HASH_get_int(state, "loop_start");

    if (HASH_has_key(state, "loop_length"))
      loop_length = HASH_get_int(state, "loop_length");
    
    int64_t loop_end = loop_start + loop_length;
    
    filepath_t filename = PLUGIN_DISK_get_audio_filename(state);
    
    if(isIllegalFilepath(filename)) // not supposed to happen though. Assertion in PLUGIN_DISK_get_audio_filename.
      goto exit;
    
    bool successfully_set_new_sample = set_new_sample(plugin,filename,instrument_number,resampler_type, use_sample_file_middle_note, is_loading, loop_start, loop_end);
    
    if(!successfully_set_new_sample)
      GFX_addMessage("Could not load soundfile \"%S\". (instrument number: %d)\n", filename.id,instrument_number);

    data=(Data*)plugin->data; // set_new_sample created new data.

    if (is_loading && disk_load_version <= 0.865){
      data->p.note_adjust = int(data->p.note_adjust);
    }
  }
  
 exit:  

  return;
  
  /*  
  if (audiodata_is_included) {    
    if (data!=NULL)
      data->filename = wcsdup(org_filename);
  }
  */

  // Can not delete now. file is still used when creating/recreating states. Deleting at program end.
  //if (audiodata_is_included)
  //  DISK_delete_base64_file(filename);
}

static void create_state(const struct SoundPlugin *plugin, hash_t *state){
  R_ASSERT_RETURN_IF_FALSE(state!=NULL);
  
  Data *data=(Data*)plugin->data;

  filepath_t filepath = data->filename.get();
  
  if (g_is_saving && nsmIsActive()){
    filepath_t maybe = DISK_link_copy_file(DISK_get_absolute_dir_path(dc.filename_with_full_path), filepath, true);
    if (isLegalFilepath(maybe))
      filepath = maybe;
  }
    
  filepath_t maybe_relative_filename = OS_saving_get_relative_path_if_possible(filepath);
  
  //printf("maybe: -%S- -%S-\n", data->filename.getString(), maybe_relative_filename.id);

  HASH_put_filepath(state, "filename", maybe_relative_filename);
  //HASH_put_filepath(state, "filename2", maybe_relative_filename);

  HASH_put_bool(state, "use_sample_file_middle_note", data->use_sample_file_middle_note);

  HASH_put_int(state, "instrument_number",data->instrument_number);
  HASH_put_int(state, "resampler_type",data->resampler_type);

  HASH_put_int(state, "loop_start",data->p.loop_start);
  HASH_put_int(state, "loop_length",data->p.loop_end - data->p.loop_start);

  if (g_embed_samples){
    const char *audiofile = DISK_file_to_base64(data->filename.get());
    if (audiofile != NULL)
      HASH_put_chars(state, "audiofile", audiofile);
    else
      GFX_addMessage("Unable to embed sample \"%S\". Could not read file.", data->filename.getString());
  }
}


filepath_t SAMPLER_get_filename(struct SoundPlugin *plugin, bool *is_default_sound){
  R_ASSERT_RETURN_IF_FALSE2(!strcmp("Sample Player", plugin->type->type_name), createIllegalFilepath());

  Data *data=(Data*)plugin->data;
  *is_default_sound = data->using_default_sound;
  return data->filename.get();
}

const wchar_t* SAMPLER_get_filename_display(struct SoundPlugin *plugin){
  R_ASSERT_RETURN_IF_FALSE2(!strcmp("Sample Player", plugin->type->type_name), L"");

  Data *data=(Data*)plugin->data;
  return data->filename.getString();
}

static SoundPluginType plugin_type = {};

static void init_plugin_type(void){

 plugin_type.type_name                = "Sample Player";
 plugin_type.name                     = "Sample Player";
 plugin_type.info                     = "Sample Player can load XI intruments, Soundfonts, and all types of sample formats supported by libsndfile. WAV files are looped if they have loops defined in the \"sampl\" chunk, or they have \"Loop Start\" and \"Loop End\" cue id's.\n\nSoundFonts often sound better when played with FluidSynth instead of the Sample Player. The Soundfont handling in Sample Player needs more care. However, the Sample Player uses less memory, are faster to create, has sample-accurate note scheduling, supports pitch changes and polyphonic aftertouch (velocity can be changed while a note is playing), and has configurable options such as attack, decay, sustain, and release.";
 plugin_type.category                 = "Synth",
 plugin_type.num_inputs               = 2;
 plugin_type.num_outputs              = 2;
 plugin_type.is_instrument            = true;
 plugin_type.note_handling_is_RT      = false;
 plugin_type.num_effects              = EFF_NUM_EFFECTS;
 plugin_type.will_always_autosuspend  = true,
 plugin_type.get_effect_format        = get_effect_format;
 plugin_type.get_effect_name          = get_effect_name;
 plugin_type.get_effect_description   = get_effect_description;
 plugin_type.effect_is_RT             = NULL;
 plugin_type.create_plugin_data       = create_plugin_data;
 plugin_type.cleanup_plugin_data      = cleanup_plugin_data;

 plugin_type.RT_process       = RT_process;
 plugin_type.play_note        = play_note;
 plugin_type.set_note_volume  = set_note_volume;
 plugin_type.set_note_pitch   = set_note_pitch;
 plugin_type.set_note_pan     = set_note_pan;
 plugin_type.stop_note        = stop_note;

 plugin_type.player_is_stopped = player_is_stopped;

 plugin_type.RT_get_audio_tail_length = RT_get_audio_tail_length;
 plugin_type.called_after_plugin_has_been_created = called_after_plugin_has_been_created;
  
 plugin_type.get_peaks        = get_peaks;
 plugin_type.set_effect_value = set_effect_value;
 plugin_type.get_effect_value = get_effect_value;
 plugin_type.get_display_value_string = get_display_value_string;
   
 plugin_type.gui_is_visible = gui_is_visible;
 plugin_type.show_gui = show_gui;
 plugin_type.hide_gui = hide_gui;

 plugin_type.recreate_from_state = recreate_from_state;
 plugin_type.create_state        = create_state;

 plugin_type.data                     = NULL;
};

static SoundPluginType click_type;

void create_sample_plugin(void){

  static bool has_inited = false;

  if (has_inited==false) {

    init_granulator_pool();
    
    init_plugin_type();

    click_type = plugin_type;

    click_type.name = g_click_name;

    has_inited = true;
  }

  PR_add_plugin_type(&plugin_type);

  PR_add_plugin_type(&click_type);
}
