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

#include "Resampler_proc.h"
#include "Envelope_proc.h"
#include "SoundFonts_proc.h"

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

#define MIN_STRETCH 0.01
#define MAX_STRETCH 100

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
  EFF_A,
  EFF_H,
  EFF_D,
  EFF_S,
  EFF_R,
  EFF_PORTAMENTO,
  EFF_VIBRATO_SPEED,
  EFF_VIBRATO_DEPTH,
  EFF_TREMOLO_SPEED,
  EFF_TREMOLO_DEPTH,
  EFF_LOOP_ONOFF,
  EFF_CROSSFADE_LENGTH,
  EFF_REVERSE,
  EFF_PINGPONG,

  EFF_GRAN_ONOFF,
  EFF_GRAN_STRETCH,

  EFF_NUM_EFFECTS,
  };

#define SAMPLES_PER_PEAK 64

namespace{
  
struct Data;

struct Sample{
  float volume = 0.0;

  int64_t num_frames = 0;
  int64_t loop_start = 0;
  int64_t loop_end = 0;

  int ch = 0;        // -1 means both channels.
  float *sound = NULL;

  //float *min_peaks = NULL;
  //float *max_peaks = NULL;
  radium::Peaks *peaks = new radium::Peaks();

#if !defined(RELEASE)
  const wchar_t *filename = NULL;
#endif

  double frequency_table[128] = {};

  Data *data = NULL;

  Sample(){
  }

  ~Sample(){
    delete peaks;
  }
};

namespace{
  struct ResettableGranResamplerCallback : public radium::GranResamplerCallback{
    virtual void reset(void) = 0;
  };
}

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

  int voice_num;
  
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

  bool reverse = 0;

  ResettableGranResamplerCallback *_get_samples;
  
  radium::Granulator *_granulator;
  radium::Resampler2 *_resampler2;
  radium::GranResampler *_granresampler;

  //radium::Resampler *resampler = NULL;
  void *adsr = NULL;

  int delta_pos_at_start = 0; // Within the current block. Set when starting a note.
  int delta_pos_at_end = 0; // Within the current block. Set when stopping a note.

  bool is_fading_out = false;
  int fading_pos = 0;
  int fading_len = 0;

  int num_samples = 0;
  const Sample *sample = NULL;

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
  
  float a,h,d,s,r;

  DEFINE_ATOMIC(bool, loop_onoff);
  int crossfade_length;

  DEFINE_ATOMIC(bool, reverse);
  DEFINE_ATOMIC(bool, pingpong);
  
  double vibrato_depth;
  double vibrato_speed;
  double vibrato_phase_add;
  double vibrato_phase;
  double vibrato_value;

  float tremolo_depth;
  float tremolo_speed;

  DEFINE_ATOMIC(bool, gran_enabled);
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

  // Should loop start/length be placed in CopyData?
  int64_t loop_start = 0;
  int64_t loop_length = 0;   // if 0, use loop data in sample file instead, if there is any. (loop_start has no meaning if loop_length is 0)

  struct SoundPlugin *tremolo = NULL;
  
  float samplerate = 0; // The system samplerate. I.e. the jack samplerate, not soundfile samplerate.

  enum ResamplerType org_resampler_type = RESAMPLER_NON; // Used to remember the original resampler type when rendering soundfile.
  enum ResamplerType resampler_type = RESAMPLER_NON;
  const wchar_t *filename = NULL;
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
  bool _gran_voice_has_updated_parameters[POLYPHONY] = {};
  
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
 
  Data(){
    R_ASSERT(voices_playing==NULL);
    R_ASSERT(voices_not_playing==NULL);
    R_ASSERT(p.crossfade_length==0);
    R_ASSERT(voices[0].next==NULL);
    R_ASSERT(samples[0].sound==NULL);

    _gran_voice_has_updated_parameters[0] = true; // always true.
    for(int i=0;i<POLYPHONY;i++)
      voices[i].voice_num=i;
        
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

  MySampleRecorderInstance(struct Patch *patch, wchar_t *recording_path, int num_ch)
    : SampleRecorderInstance(recording_path, num_ch, 48)
    , _patch(patch)
  {
  }
  
  void is_finished(bool success, wchar_t *filename) override {
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


static void RT_fade_out(float *sound, int num_frames){
  float num_frames_plus_1 = num_frames+1.0f;
  int i;
  float val = (num_frames / num_frames_plus_1);
  float inc = val - ( (num_frames-1) / num_frames_plus_1);

  for(i=0;i<num_frames;i++){
    sound[i] *= val;
    val -= inc;
  }
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
  for(i=0;i<num_frames;i++){
    dst[i] = src[i]*mul;
    mul += inc;
  }
}

static void RT_fade_add(float *dst, float *src, int num_frames, float start_val, float end_val){
  float mul = start_val;
  float inc = (end_val-start_val)/(float)num_frames;
  int i;
  for(i=0;i<num_frames;i++){
    dst[i] += src[i]*mul;
    mul += inc;
  }
}

static int RT_crossfade(int64_t start_pos, int64_t end_pos, int64_t crossfade_start, int64_t crossfade_end, float *out_data, float *in_data){
  int num_frames = (int)(end_pos - start_pos);

  float start_fade_val = scale_double(start_pos,
                                      crossfade_start, crossfade_end,
                                      1.0f, 0.0f
                                      );

  float end_fade_val  = scale_double(end_pos,
                                     crossfade_start, crossfade_end,
                                     1.0f, 0.0f
                                     );

  //printf("fade out: %d -> %d\n",start_pos, start_pos+num_frames);
  //printf("fade in:  %d -> %d\n\n", start_pos2, start_pos2+num_frames);
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
                 in_data + (start_pos - crossfade_start),
                 num_frames,
                 1.0f - start_fade_val, 1.0f - end_fade_val
              );

  return num_frames;
}

static int RT_legal_crossfade_length(const Sample *sample, Data *data){
  int crossfade_length = data->p.crossfade_length;
  int64_t loop_length = sample->loop_end - sample->loop_start;

  return (int)R_MIN(crossfade_length, loop_length/2);
}

static int RT_src_callback_with_crossfade_do_looping(Voice *voice, const Sample *sample, Data *data, int64_t start_pos, float **out_data){
  *out_data = voice->crossfade_buffer;
  int len_out_data = CROSSFADE_BUFFER_LENGTH;

  int64_t end_pos = start_pos + len_out_data;
  if (end_pos > sample->loop_end)
    end_pos = sample->loop_end;

  int legal_crossfade_length = RT_legal_crossfade_length(sample, data);

  voice->pos = end_pos; // next
  if (voice->pos==sample->loop_end)
    voice->pos = sample->loop_start + legal_crossfade_length;
  //printf("crossfading %d -> %d-%d -> %d (%d)\n",sample->loop_start,start_pos,end_pos,sample->loop_end,voice->pos);

  //printf("do looping %d\n");

  float *in_data = voice->sample->sound;

  return RT_crossfade(start_pos, end_pos,
                      sample->loop_end - legal_crossfade_length, sample->loop_end,
                      *out_data, in_data
                      );
}

static int64_t RT_src_callback_with_crossfade_between_looping(Voice *voice, const Sample *sample, Data *data, int64_t start_pos, float **out_data){
  *out_data = sample->sound + voice->pos;
  int64_t prev_voice_pos = voice->pos;
  voice->pos = sample->loop_end - RT_legal_crossfade_length(sample, data); // next
  return voice->pos - prev_voice_pos;
}

static int64_t RT_src_callback_with_crossfade_before_looping(Voice *voice, const Sample *sample, Data *data, int64_t start_pos, float **out_data){
  *out_data = sample->sound;
  voice->pos = sample->loop_end - RT_legal_crossfade_length(sample, data); // next
  return voice->pos; // start_pos==0 here.
}

static int64_t RT_src_callback_with_crossfade_looping(Voice *voice, const Sample *sample, Data *data, int64_t start_pos, float **out_data){
  //printf("crossfading %d -> %d -> %d (%d)\n",sample->loop_start,start_pos,sample->loop_end,voice->pos);

  if(start_pos==0 && sample->loop_start>0)
    return RT_src_callback_with_crossfade_before_looping(voice, sample, data, start_pos, out_data);

  else if (start_pos >= sample->loop_end - RT_legal_crossfade_length(sample, data))
    return RT_src_callback_with_crossfade_do_looping(voice, sample, data, start_pos, out_data);

  else
    return RT_src_callback_with_crossfade_between_looping(voice, sample, data, start_pos, out_data);
}
/**********************************
  End of crossfading code
***********************************/



static int64_t RT_src_callback_with_normal_looping(Voice *voice, const Sample *sample, Data *data, int64_t start_pos, float **out_data){
  *out_data = &sample->sound[start_pos];

  voice->pos = sample->loop_start; // next

  if(start_pos >= sample->loop_end) // just in case. not sure if this can happen
    return 0;

  else
    return sample->loop_end - start_pos;
}



static int64_t RT_src_callback_nolooping(Voice *voice, const Sample *sample, Data *data, int64_t start_pos, float **out_data){
  *out_data = &sample->sound[start_pos];

  if(start_pos==sample->num_frames)
    return 0;

  voice->pos=sample->num_frames; // next

  int64_t ret = sample->num_frames - start_pos;

  if (ret < 0){
#if defined(RELEASE)
    ret = 0;
#else
    fprintf(stderr, "num_frames: %d, start_pos: %d\n",(int)sample->num_frames, (int)start_pos);
    abort();
#endif
  }

  return ret;
}

static int64_t RT_src_callback_reverse(Voice *voice, const Sample *sample, Data *data, int64_t start_pos, float **out_data, bool do_looping){
  *out_data = &voice->crossfade_buffer[0];

  if(start_pos==sample->num_frames) {
    if (do_looping)
      start_pos = 0;
    else
      return 0;
  }

  int64_t samples_left = sample->num_frames - start_pos;

  int64_t num_samples_to_return = R_MIN(samples_left, CROSSFADE_BUFFER_LENGTH);

  float *source_sound = sample->sound;
  float *dest_sound = &voice->crossfade_buffer[0];
  int64_t sample_pos = sample->num_frames-1 - start_pos;
 
  int64_t i;
  for(i=0 ; i< num_samples_to_return ; i++)
    dest_sound[i] = source_sound[sample_pos--];

  
  voice->pos = start_pos + num_samples_to_return; // i.e. next pos

  return num_samples_to_return;
}

static int64_t RT_src_callback_ping_pong_looping(Voice *voice, const Sample *sample, Data *data, int64_t start_pos, float **out_data){
  const int64_t num_sample_frames = sample->num_frames;
  
  R_ASSERT(start_pos <= num_sample_frames*2);
  
  if (start_pos >= num_sample_frames*2)
    start_pos = 0;

  if (start_pos >= num_sample_frames) {
    int64_t ret = RT_src_callback_reverse(voice, sample, data, start_pos - num_sample_frames, out_data, false);
    voice->pos += num_sample_frames;
    return ret;
  } else
    return RT_src_callback_nolooping(voice, sample, data, start_pos, out_data);
}



static long RT_src_callback(void *cb_data, float **out_data){
  Voice *voice         = (Voice*)cb_data;
  const Sample *sample = voice->sample;
  int64_t start_pos    = voice->pos;
  Data  *data          = sample->data;

  bool pingpong = ATOMIC_GET(sample->data->p.pingpong);
  bool reverse = voice->reverse;
  bool loop = ATOMIC_GET(sample->data->p.loop_onoff);

  int64_t ret;
  
  if (pingpong) {
    
    ret = RT_src_callback_ping_pong_looping(voice, sample, data, start_pos, out_data); // ping pong looping

  } else {

    if (start_pos >= sample->num_frames) // Happens when switching from ping-pong to non-ping-pong while playing.
      start_pos = sample->num_frames - (start_pos - sample->num_frames); // Keep same sample position.
    
    if (reverse && loop)
      ret = RT_src_callback_reverse(voice, sample, data, start_pos, out_data, true); //loop reverse
    
    else if (reverse && !loop)
      ret = RT_src_callback_reverse(voice, sample, data, start_pos, out_data, false); //only reverse, no looping
    
    else if(!loop || sample->loop_end <= sample->loop_start)
      ret = RT_src_callback_nolooping(voice, sample, data, start_pos, out_data);
    
    else if(data->p.crossfade_length > 0)
      ret = RT_src_callback_with_crossfade_looping(voice, sample, data, start_pos, out_data);
    
    else
      ret = RT_src_callback_with_normal_looping(voice, sample, data, start_pos, out_data);

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
    pitch += data->p.vibrato_value;
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
    if (voice->end_pitch != voice->pitch){
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
  int frames_created_by_resampler = RT_get_resampled_data(data,voice,resampled_data,num_frames);
  //printf("Frames created by resampler: %d\n",frames_created_by_resampler);
  //printf("peak: %f\n",get_peak(resampled_data,frames_created_by_resampler));

  if (frames_created_by_resampler==0)
    return true;
  
  int frames_created_by_envelope;

  float *adsr_sound_data[1]={&resampled_data[0]};

  if(endpos>=0){
    int pre_release_len = endpos-startpos;

    //printf("********** endpos>0: %d. prelen: %d, frames_created_by_resampler: %d\n",endpos,prelen,frames_created_by_resampler);

    if(frames_created_by_resampler <= pre_release_len){ // i.e. we reached the end of sound before beginning to release the ADSR envelope.

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


static void RT_process(SoundPlugin *plugin, int64_t time, R_NUM_FRAMES_DECL float **inputs, float **outputs){
  Data *data = (Data*)plugin->data;
  Voice *voice = data->voices_playing;

  memset(outputs[0],0,R_NUM_FRAMES*sizeof(float));
  memset(outputs[1],0,R_NUM_FRAMES*sizeof(float));

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

    R_ASSERT_RETURN_IF_FALSE(num_ch==data->recorder_instance->num_ch);
    
    /*
      // No need. Mixer always have at least 2 channels, and data->recorder->num_ch can never have more than 2 channels.
    const float empty_block[RADIUM_BLOCKSIZE] = {}; // The stack might be hotter than the heap.
    for(int ch = num_ch ; ch < data->recorder->num_ch ; ch++)
      audio[ch] = empty_block;
    */
    
    RT_SampleRecorder_add_audio(data->recorder_instance,
                                const_cast<const float**>(audio),
                                RADIUM_BLOCKSIZE - data->recording_start_frame
                                );
    
    data->recording_start_frame = 0;

    return;
  }
  
  if (data->p.vibrato_phase_add > 0.0) {
    data->p.vibrato_value = data->p.vibrato_depth * sin(data->p.vibrato_phase);
    data->p.vibrato_phase += data->p.vibrato_phase_add*(double)R_NUM_FRAMES;
  }
  
  bool was_playing_something = data->voices_playing != NULL;
  
  while(voice!=NULL){
    Voice *next = voice->next;

    if(RT_play_voice(data, voice, R_NUM_FRAMES, outputs)==true){
      RT_remove_voice(&data->voices_playing, voice);
      RT_add_voice(&data->voices_not_playing, voice);
    }

    voice = next;
  }

  if (was_playing_something)
    data->tremolo->type->RT_process(data->tremolo, time, R_NUM_FRAMES, outputs, outputs);

  Data *new_data = ATOMIC_GET(data->new_data);
  
  if(new_data != NULL){
    RT_fade_out(outputs[0],R_NUM_FRAMES);
    RT_fade_out(outputs[1],R_NUM_FRAMES);

    plugin->data = new_data; // Bang! (hmm.)
    ATOMIC_SET(data->new_data, NULL);

    RSEMAPHORE_signal(data->signal_from_RT,1);
  }
}

static void prepare_voice_for_playing_with_granulation(Data *data, Voice *voice){
  if (data->_gran_voice_has_updated_parameters[voice->voice_num]){
    
    voice->_granulator->reset(false);
    
  } else {
    
    voice->_granulator->apply_parameters_and_reset(*data->voices[0]._granulator);
    data->_gran_voice_has_updated_parameters[voice->voice_num] = true;
    
  }
  
  voice->_resampler2->set_callback(voice->_granulator);
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

    if(ATOMIC_GET(data->p.loop_onoff)==true && sample->loop_end > sample->loop_start)
      voice->pos=scale(data->p.startpos, // set startpos between 0 and loop_end
                       0,1,
                       0,sample->loop_end);
    else
      voice->pos=scale(data->p.startpos,  // set startpos between 0 and sound length
                       0,1,
                       0,sample->num_frames);

    //printf("Sample_pos: %d\n",(int)note2.sample_pos);
    if (note2.sample_pos > 0){
      voice->pos += note2.sample_pos / RT_get_src_ratio(data, voice);
      if (voice->pos >= sample->num_frames){
        RT_remove_voice(&data->voices_playing, voice);
        RT_add_voice(&data->voices_not_playing, voice);
        return;
      }
    }

    voice->reverse = ATOMIC_GET(sample->data->p.reverse);
    
    voice->pan = get_pan_vals_vector(note2.pan,voice->sample->ch==-1?1:2);

    if (ATOMIC_GET_RELAXED(data->p.gran_enabled)){

      prepare_voice_for_playing_with_granulation(data, voice);
      
    } else {
      
      voice->_resampler2->set_callback(voice->_get_samples);
      
    }
    
    voice->_resampler2->reset();
    voice->_get_samples->reset();
    //RESAMPLER_reset(voice->resampler);

    ADSR_reset(voice->adsr);
    ADSR_set_adsr(voice->adsr, data->p.a, data->p.h, data->p.d, data->p.s, data->p.r);

    voice->delta_pos_at_start=time;
    voice->delta_pos_at_end=-1;
    voice->is_fading_out=false;
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
      
      struct Patch *patch = (struct Patch*)plugin->patch;
      
      RT_SampleRecorder_stop_recording(data->recorder_instance);
      
      ATOMIC_SET(data->recording_status, NOT_RECORDING);
      ATOMIC_SET(patch->is_recording, false);
      
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
static int RT_get_audio_tail_length(struct SoundPlugin *plugin){
  Data *data = (Data*)plugin->data;
  return (data->p.a+data->p.d+data->p.r) * (double)MIXER_get_sample_rate() / 1000.0;
}

static bool note_has_sample(const Note *note){
  return note->samples.size() > 0;
}

static int time_to_frame(Data *data, double time, float f_note_num){

  int i_note_num = (int)f_note_num;
  
  const Note *note = data->notes[(int)i_note_num];

  if (note->samples.is_empty()){
    RError("note->samples is empty. %f\n",f_note_num);
    return data->p.startpos*10000 + time/30000.0f;
  }

  const Sample *sample = note->samples.at(0);

  double src_ratio = RT_get_src_ratio2(data, sample, f_note_num);

  return
    data->p.startpos*sample->num_frames 
    + time/src_ratio ;
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


static radium::Peak get_peak_from_sample(const Sample *sample, int64_t start_time, const int64_t duration, int rec_level){
  R_ASSERT(rec_level <= 1); // This function can only be called recursively once; when starting in the middle of a loop and ask for more data than is available in the remainder of the loop.
  
  radium::Peak peak;

  int64_t end;

  bool is_looping = ATOMIC_GET(sample->data->p.loop_onoff)==true;

  if(is_looping && start_time>=sample->loop_end){

    if (sample->loop_end <= sample->loop_start)
      return peak;

    start_time -= sample->loop_end; // i.e. how far after loop end are we?
      
    int64_t loop_length = sample->loop_end - sample->loop_start;
    int64_t num_loops = start_time / loop_length;
    start_time -= (num_loops*loop_length);
    
    start_time += sample->loop_start;

    end = sample->loop_end;

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

  if (is_looping && start_time > sample->loop_start){ // If start_time==sample->loop_start, we would not get hold of any new peaks in the call to get_peak_from_sample below. (it would also stall the program in some situations)
    int64_t duration_left = duration - duration_now;
    
    if (duration_left > 0)
      peak.merge(get_peak_from_sample(sample, sample->loop_start, duration_left, rec_level+1));
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
    
    int i;
    for(i=0;i<MAX_NUM_SAMPLES;i++){
      const Sample &sample=data->samples[i];
      if(sample.sound!=NULL){
        if(sample.ch==1)
          return 2;
      }
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

  if (!note_has_sample(note) || start_time==end_time){
    R_ASSERT_NON_RELEASE(end_time > start_time);
    goto return_empty;
  } 

  //printf("Creating new peak for ch %d, frames %d -> %d\n", ch, (int)start_frame, (int)end_frame);

  {
    int64_t start_frame = time_to_frame(data, start_time, note_num);
    int64_t end_frame = time_to_frame(data, end_time, note_num);
    
    if (start_frame==end_frame)
      goto return_empty;
  
    for(const Sample *sample : note->samples){
    
      Panvals pan = get_pan_vals_vector(das_pan, sample->ch==-1 ? 1 : 2);
      int input_channel = sample->ch==-1 ? 0 : sample->ch;
      float panval = pan.vals[input_channel][ch];

      //printf("  Sample %d. Panval: %f\n", samplenum, panval);
      
      if(panval>0.0f){

        //printf("Asking for peak %d %d (%d). Total sample duration: %d\n", (int)start_frame, (int)end_frame, (int)(end_frame-start_frame), (int)sample->num_frames);
        radium::Peak new_peak = get_peak_from_sample(sample, start_frame, end_frame-start_frame,0);
        
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

static void set_gran_stretch(Data *data, double stretch){
  // radium::PlayerRecursiveLock lock; // Player is always locked when calling set_effect_value
  R_ASSERT_NON_RELEASE(PLAYER_current_thread_has_lock());
    
  memset(&data->_gran_voice_has_updated_parameters[1], 0, sizeof(bool)*(POLYPHONY-1));
  
  data->voices[0]._granulator->set_stretch(stretch);

  for(Voice *voice = data->voices_playing ; voice!=NULL ; voice=voice->next){
    if(voice->voice_num != 0){
      data->_gran_voice_has_updated_parameters[voice->voice_num] = true;
      voice->_granulator->set_stretch(stretch);
    }
  }
}

static double get_gran_stretch(Data *data){
  double ret = data->voices[0]._granulator->get_stretch();
  //printf("   RET: %f\n", ret);
  //getchar();
  return ret;
}

static void set_granulation_enabled(Data *data, bool enabled){
  bool is_enabled = ATOMIC_GET_RELAXED(data->p.gran_enabled);
  if (enabled==is_enabled)
    return;

  R_ASSERT_NON_RELEASE(PLAYER_current_thread_has_lock());

  if (enabled){

    for(Voice *voice = data->voices_playing ; voice!=NULL ; voice=voice->next)
      prepare_voice_for_playing_with_granulation(data, voice);
    
  } else {
    
    for(Voice *voice = data->voices_playing ; voice!=NULL ; voice=voice->next)
      voice->_resampler2->set_callback(voice->_get_samples);
    
  }

  ATOMIC_SET(data->p.gran_enabled, enabled);
}

static bool get_granulation_enabled(Data *data){
  return ATOMIC_GET(data->p.gran_enabled);
}




static void set_loop_onoff(Data *data, bool loop_onoff){
  ATOMIC_SET(data->p.loop_onoff, loop_onoff);
}

static bool get_loop_onoff(Data *data){
  return ATOMIC_GET(data->p.loop_onoff);
}

static bool can_crossfade(Data *data){
  return ATOMIC_GET(data->p.reverse)==false && ATOMIC_GET(data->p.pingpong)==false;
}

static void set_effect_value(struct SoundPlugin *plugin, int time, int effect_num, float value, enum ValueFormat value_format, FX_when when){
  Data *data = (Data*)plugin->data;

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
    case EFF_LOOP_ONOFF:
      set_loop_onoff(data, value>=0.5f);
      update_editor_graphics(plugin);
      break;
      
    case EFF_CROSSFADE_LENGTH:
      if (can_crossfade(data))
        data->p.crossfade_length = scale(value,
                                         0.0, 1.0,
                                         0, MAX_CROSSFADE_LENGTH
                                         );
      else
        data->p.crossfade_length = 0;
      
      break;
      
    case EFF_REVERSE:      
      ATOMIC_SET(data->p.reverse, value>=0.5f);
      if (!can_crossfade(data)){
        
        //printf("Doing it %p\n",plugin->patch);
        PLUGIN_set_effect_value(plugin, time, EFF_CROSSFADE_LENGTH, 0, THREADING_is_main_thread() ? STORE_VALUE : DONT_STORE_VALUE, when, EFFECT_FORMAT_NATIVE);
        
        if (plugin->patch != NULL)
          GFX_ScheduleInstrumentRedraw((struct Patch*)plugin->patch);
      }
                                 
      break;
      
    case EFF_PINGPONG:
      ATOMIC_SET(data->p.pingpong, value>=0.5f);
      if (!can_crossfade(data)){

        PLUGIN_set_effect_value(plugin, time, EFF_CROSSFADE_LENGTH, 0, THREADING_is_main_thread() ? STORE_VALUE : DONT_STORE_VALUE, when, EFFECT_FORMAT_NATIVE);
        
        if (plugin->patch != NULL)
          GFX_ScheduleInstrumentRedraw((struct Patch*)plugin->patch);
      }
      break;

    case EFF_GRAN_ONOFF:
      set_granulation_enabled(data, value >= 0.5);
      break;
      
    case EFF_GRAN_STRETCH:
      if (value < 0.5)
        set_gran_stretch(data, scale(value, 0, 0.5, MIN_STRETCH, 1.0));
      else
        set_gran_stretch(data, scale(value, 0.5, 1.0, 1.0, MAX_STRETCH));
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
    case EFF_LOOP_ONOFF:
      set_loop_onoff(data, value>=0.5f);
      update_editor_graphics(plugin);
      break;

    case EFF_CROSSFADE_LENGTH:
      data->p.crossfade_length = value;
      break;

    case EFF_REVERSE:
      ATOMIC_SET(data->p.reverse, value>=0.5f);
      break;
      
    case EFF_PINGPONG:
      ATOMIC_SET(data->p.pingpong, value>=0.5f);
      break;

    case EFF_GRAN_ONOFF:
      set_granulation_enabled(data, value >= 0.5);
      break;

    case EFF_GRAN_STRETCH:
      set_gran_stretch(data, value);
      break;

    default:
      RError("S2. Unknown effect number %d\n",effect_num);
    }
  }
}

static float get_effect_value(struct SoundPlugin *plugin, int effect_num, enum ValueFormat value_format){
  Data *data = (Data*)plugin->data;

  if(value_format==EFFECT_FORMAT_SCALED){
    switch(effect_num){
    case EFF_STARTPOS:
      return data->p.startpos;
    case EFF_FINETUNE:
      return data->p.finetune;
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
      return scale(data->p.crossfade_length,0,MAX_CROSSFADE_LENGTH,0,1);
      break;

    case EFF_REVERSE:
      return ATOMIC_GET(data->p.reverse)==true?1.0f:0.0f;
      break;

    case EFF_PINGPONG:
      return ATOMIC_GET(data->p.pingpong)==true?1.0f:0.0f;
      break;

    case EFF_GRAN_ONOFF:
      return get_granulation_enabled(data)==true?1.0f:0.0f;
      break;

    case EFF_GRAN_STRETCH:
      {
        float stretch = get_gran_stretch(data);
        if (stretch < 1.0)
          return scale(stretch, MIN_STRETCH, 1.0, 0, 0.5);
        else
          return scale(stretch, 1.0, MAX_STRETCH, 0.5, 1.0);
      }
      break;

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
      return ATOMIC_GET(data->p.reverse)==true?1.0f:0.0f;
      break;

    case EFF_PINGPONG:
      return ATOMIC_GET(data->p.pingpong)==true?1.0f:0.0f;
      break;

    case EFF_GRAN_ONOFF:
      return get_granulation_enabled(data)==true?1.0f:0.0f;
      break;
      
    case EFF_GRAN_STRETCH:
      return get_gran_stretch(data);
      break;

    default:
      RError("S4. Unknown effect number %d\n",effect_num);
      return 0.5f;
    }
  }
}

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
    snprintf(buffer,buffersize-1,"%f",data->p.s);
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
      snprintf(buffer,buffersize-1,"%s%.2f note%s",adjust>0?"+":"",adjust,adjust==-1?"":adjust==1?"":"s");
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
  case EFF_CROSSFADE_LENGTH:
    snprintf(buffer,buffersize-1,"%d samples",safe_int_read(&data->p.crossfade_length));
    break;

  case EFF_GRAN_STRETCH:
    snprintf(buffer,buffersize-1,"%.2fX",get_gran_stretch(data));
    break;

  default:
    RError("S5. Unknown effect number %d\n",effect_num);
  }
}
             

// Note, if start==-1 and end==-1, loop_start is set to 0 and loop_end is set to sample->num_frames, and loop_onoff is not set.
static void set_legal_loop_points(Sample &sample, int64_t start, int64_t end, bool set_loop_on_off){
  if(start==-1 && end==-1){ 
    sample.loop_start=0;
    sample.loop_end=sample.num_frames;
    return;
  }

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


#include "Sampler_plugin_wav_parse.c"
#include "Sampler_plugin_xi_load.c"
#include "Sampler_plugin_sf2_load.c"

static float *load_interleaved_samples(const wchar_t *filename, SF_INFO *sf_info){
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

static bool load_sample_with_libsndfile(Data *data, const wchar_t *filename, bool set_loop_on_off){
  EVENTLOG_add_event(talloc_format("load_sample_with_libsndfile -%S-", filename));
    
  SF_INFO sf_info; memset(&sf_info,0,sizeof(sf_info));

  data->num_different_samples = 1;

  float *samples = load_interleaved_samples(filename, &sf_info);

  if(samples==NULL){
    fprintf(stderr,"  Libsndfile could not open file \"%S\"\n", filename);
    return false;
  }

  {
    int num_channels = sf_info.channels;

    printf("Num channels: %d\n",num_channels);

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
        printf("format: 0x%x. sections: %d, num_frames: %d. SF_FORMAT_WAV: 0x%x. og: 0x%x\n",sf_info.format,sf_info.sections,(int)sf_info.frames,SF_FORMAT_WAV,sf_info.format&SF_FORMAT_WAV);
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
  struct GetSampleCallback : public ResettableGranResamplerCallback, public radium::AudioPickuper {
    Voice &_voice;

    GetSampleCallback(Voice &voice)
      : _voice(voice)
    {}

    int _buffer_size = 0;
    float *_buffer;

    void reset(void) override {
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

  auto *get_samples = new GetSampleCallback(voice);
  voice._get_samples = get_samples;

  voice._resampler2 = new radium::Resampler2(1, data->resampler_type, get_samples);
  voice._granulator = new radium::Granulator(44100, 100000, 50, 1, get_samples);

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
  delete voice._granulator;
  delete voice._get_samples;
}


static bool load_sample(Data *data, const wchar_t *filename, int instrument_number, bool set_loop_on_off){
  if(load_xi_instrument(data,filename, set_loop_on_off)==false){
    if(load_sample_with_libsndfile(data,filename, set_loop_on_off)==false){
      if(load_sf2_instrument(data,filename,instrument_number, set_loop_on_off)==false){        
        GFX_Message(NULL,"Unable to load %S as soundfile.", filename);
        return false;
      }
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

static Data *create_data(float samplerate, Data *old_data, const wchar_t *filename, int instrument_number, enum ResamplerType resampler_type, bool use_sample_file_middle_note, bool is_loading){
  Data *data = new Data;
  
  data->signal_from_RT = RSEMAPHORE_create(0);

  data->tremolo = create_tremolo(is_loading);
      
  if(old_data==NULL){

    data->p.finetune = 0.5f;
    
    data->p.a=DEFAULT_A;
    data->p.d=DEFAULT_D;
    data->p.s=DEFAULT_S;
    data->p.r=DEFAULT_R;

    data->p.vibrato_phase_add = -1;
        
  }else{

    memcpy(&data->p, &old_data->p, sizeof(CopyData));
 
    data->p.vibrato_value = 0.0;
    data->p.vibrato_phase = 4.71239;
    
    data->tremolo->type->set_effect_value(data->tremolo, 0, 0, data->p.tremolo_speed, EFFECT_FORMAT_NATIVE, FX_single);
    data->tremolo->type->set_effect_value(data->tremolo, 0, 1, data->p.tremolo_depth, EFFECT_FORMAT_NATIVE, FX_single);
    
  }

  data->use_sample_file_middle_note = use_sample_file_middle_note;

  data->samplerate = samplerate;
  data->resampler_type = resampler_type;
  data->filename = wcsdup(filename);
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
  wchar_t *default_sound_filename = STRING_append(OS_get_program_path2(),
                                    STRING_append(STRING_create(OS_get_directory_separator()),
                                    STRING_append(STRING_create("sounds"),
                                    STRING_append(STRING_create(OS_get_directory_separator()),
                                                  !strcmp(plugin_type->name, "Click")
                                                  ? STRING_create("243749__unfa__metronome-1khz-weak-pulse.flac")
                                                  : STRING_create("016.WAV")))));
    
  Data *data = create_data(samplerate,NULL,default_sound_filename,0,RESAMPLER_CUBIC, true, is_loading); // cubic is the default
  
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
  
  free((char*)data->filename);

  RSEMAPHORE_delete(data->signal_from_RT);

  free_tremolo(data->tremolo);

  delete data->recorder_instance;

  delete data;
}

static void set_loop_data(Data *data, int64_t start, int64_t length, bool set_loop_on_off){
  
  if (set_loop_on_off) {
    if (length==0)
      ATOMIC_SET(data->p.loop_onoff, false);
    if (length > 0)
      ATOMIC_SET(data->p.loop_onoff, true);
  }
  
  data->loop_start = start;
  data->loop_length = length;
  
  if (length!=0) {
    
    int i;
    for(i=0;i<MAX_NUM_SAMPLES;i++){
      Sample &sample=data->samples[i];
      if(sample.sound!=NULL){
        if (start < sample.num_frames)
          sample.loop_start = start;
        else
          sample.loop_start = sample.num_frames - 1;
        
        int64_t loop_end = sample.loop_start + length;
        if (loop_end <= sample.num_frames)
          sample.loop_end = loop_end;
        else
          sample.loop_end = sample.num_frames;
      }
    }
  }
}

void SAMPLER_set_loop_data(struct SoundPlugin *plugin, int64_t start, int64_t length){
  R_ASSERT_RETURN_IF_FALSE(!strcmp("Sample Player", plugin->type->type_name));
  
  Data *data=(Data*)plugin->data;

  PLAYER_lock();{  // Altough we don't need lock for PLUGIN_set_effect_value, it's possible that set_loop_data, or a combination of set_loop_data and PLUGIN_set_effect_value needs the lock.
    set_loop_data(data, start, length, true);
    PLUGIN_set_effect_value(plugin, -1, EFF_LOOP_ONOFF, ATOMIC_GET(data->p.loop_onoff)==true?1.0f:0.0f, STORE_VALUE, FX_single, EFFECT_FORMAT_NATIVE);
  }PLAYER_unlock();

  update_editor_graphics(plugin);

  {
    volatile struct Patch *patch = plugin->patch;
    if(patch!=NULL)
      GFX_update_instrument_widget((struct Patch*)patch);
  }
}

static bool set_new_sample(struct SoundPlugin *plugin,
                           const wchar_t *filename,
                           int instrument_number,
                           enum ResamplerType resampler_type,
                           int64_t loop_start,
                           int64_t loop_end,
                           bool use_sample_file_middle_note,
                           bool is_loading)
{
  bool success=false;

  Data *data = NULL;
  Data *old_data = (Data*)plugin->data;

  filename = OS_loading_get_resolved_file_path(filename, false); // set program_state_is_valid=false. Might not be necessary, but I'm not sure.
  if (filename==NULL)
    goto exit;

  data = create_data(old_data->samplerate, old_data, filename, instrument_number, resampler_type, use_sample_file_middle_note, is_loading);
  
  if(load_sample(data,filename,instrument_number, false)==false)
    goto exit;

  set_loop_data(data, loop_start, loop_end, false);
  
  // Put loop_onoff into storage.
  PLUGIN_set_effect_value(plugin, -1, EFF_LOOP_ONOFF, ATOMIC_GET(data->p.loop_onoff)==true?1.0f:0.0f, STORE_VALUE, FX_single, EFFECT_FORMAT_NATIVE);

  if(SP_is_plugin_running(plugin)){

    //fprintf(stderr, "    *************** 11111. plugin IS running **********\n");
    
    ATOMIC_SET(old_data->new_data, data);

    if (PLAYER_is_running())
      RSEMAPHORE_wait(old_data->signal_from_RT,1);

  } else {

    //fprintf(stderr, "    *************** 0000. plugin is NOT running **********\n");
    
    plugin->data = data;

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


bool SAMPLER_set_new_sample(struct SoundPlugin *plugin, const wchar_t *filename, int instrument_number){
  R_ASSERT_RETURN_IF_FALSE2(!strcmp("Sample Player", plugin->type->type_name), false);
  
  Data *data=(Data*)plugin->data;
  return set_new_sample(plugin,filename,instrument_number,data->resampler_type,data->loop_start,data->loop_length, true, false);
}


bool SAMPLER_set_random_sample(struct SoundPlugin *plugin, const wchar_t *path){
  R_ASSERT_RETURN_IF_FALSE2(!strcmp("Sample Player", plugin->type->type_name), false);
    
  bool is_default_sound;

  QDir dir;
  
  if (path==NULL){
    
    const wchar_t *filename = SAMPLER_get_filename(plugin, &is_default_sound);
    
    QFileInfo info(STRING_get_qstring(filename));
  
    dir = info.absoluteDir();
    
  } else {
    
    dir = QDir(STRING_get_qstring(path));
    
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
  
  QString new_filename = dir.absoluteFilePath(list.at(qrand() % list.size()));

  printf("*********** filename: -%s-\n", new_filename.toUtf8().constData());
  
  if (SAMPLER_set_new_sample(plugin, STRING_create(new_filename), 0)==false){
    //GFX_Message(NULL, "Unable to set sample %s", new_filename.toUtf8().constData()); // SAMPLER_set_new_sample has already given a message.
    return false;
  }
  
  return true;
}


bool SAMPLER_set_temp_resampler_type(struct SoundPlugin *plugin, enum ResamplerType resampler_type){
  Data *data=(Data*)plugin->data;
  data->org_resampler_type = resampler_type;
  if (resampler_type != data->resampler_type)
    return set_new_sample(plugin,data->filename,data->instrument_number,resampler_type,data->loop_start,data->loop_length, data->use_sample_file_middle_note, false);
  else
    return true;
}

void SAMPLER_set_org_resampler_type(struct SoundPlugin *plugin){
  Data *data=(Data*)plugin->data;
  if (data->org_resampler_type != data->resampler_type)
    set_new_sample(plugin,data->filename,data->instrument_number,data->org_resampler_type,data->loop_start,data->loop_length, data->use_sample_file_middle_note, false);
}

bool SAMPLER_set_resampler_type(struct SoundPlugin *plugin, enum ResamplerType resampler_type){
  R_ASSERT_RETURN_IF_FALSE2(!strcmp("Sample Player", plugin->type->type_name), false);
  
  Data *data=(Data*)plugin->data;
  if (resampler_type != data->resampler_type)
    return set_new_sample(plugin,data->filename,data->instrument_number,resampler_type,data->loop_start,data->loop_length, data->use_sample_file_middle_note, false);
  else
    return true;
}

enum ResamplerType SAMPLER_get_resampler_type(struct SoundPlugin *plugin){
  R_ASSERT_RETURN_IF_FALSE2(!strcmp("Sample Player", plugin->type->type_name), RESAMPLER_NON);
  
  Data *data=(Data*)plugin->data;
  return data->resampler_type;
}

// Has been used for debugging. Not sure if I planned to use it for anything else.
void SAMPLER_save_sample(struct SoundPlugin *plugin, const wchar_t *filename, int sample_number){
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

void SAMPLER_start_recording(struct SoundPlugin *plugin, const wchar_t *pathdir, int num_channels, bool recording_from_main_input){
#if !defined(RELEASE)
  R_ASSERT_RETURN_IF_FALSE(!strcmp("Sample Player", plugin->type->type_name));
#endif
  
  R_ASSERT_RETURN_IF_FALSE(num_channels > 0);

  Data *data = (Data*)plugin->data;

  if (ATOMIC_GET(data->recording_status) != NOT_RECORDING)
    return;

  wchar_t *recording_path = STRING_append(pathdir,
                                          STRING_append(STRING_create(OS_get_directory_separator()),
                                                        STRING_replace(STRING_replace(STRING_create(plugin->patch->name),
                                                                                      "/",
                                                                                      "_slash_"),
                                                                       "\\",
                                                                       "_backslash_")));
  QDir dir = QFileInfo(STRING_get_qstring(recording_path)).dir();
  if (dir.exists()==false){
    GFX_Message2(NULL, true, "Error. Could not find the directory \"%s\".\n", dir.absolutePath().toUtf8().constData());
    return;
  }
  
  data->recording_from_main_input = recording_from_main_input;
  ATOMIC_SET(data->recording_status, BEFORE_RECORDING);

  auto *old = data->recorder_instance;
  data->recorder_instance = new MySampleRecorderInstance(const_cast<struct Patch*>(plugin->patch), recording_path, num_channels);
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

static const char *get_effect_name(struct SoundPlugin *plugin, int effect_num){
  switch(effect_num){
  case EFF_STARTPOS:
    return "Start Position";
  case EFF_FINETUNE:
    return "Finetune";
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
  case EFF_CROSSFADE_LENGTH:
    return "Crossfade";
  case EFF_REVERSE:
    return "Reverse";
  case EFF_PINGPONG:
    return "Ping-Pong Loop";
  case EFF_GRAN_ONOFF:
    return "Granulate";
  case EFF_GRAN_STRETCH:
    return "Gran. Stretch";
  default:
    RError("S6. Unknown effect number %d\n",effect_num);
    return NULL;
  }
}

static int get_effect_format(struct SoundPlugin *plugin, int effect_num){
  if(effect_num==EFF_LOOP_ONOFF || effect_num==EFF_REVERSE || effect_num==EFF_PINGPONG || effect_num==EFF_GRAN_ONOFF)
    return EFFECT_FORMAT_BOOL;
  else if (effect_num==EFF_CROSSFADE_LENGTH)
    return EFFECT_FORMAT_INT;
  else
    return EFFECT_FORMAT_FLOAT;
}

/*
static QString get_final_embedded_filename(QString org_filename, QString new_filename){
}
*/

static void create_state(struct SoundPlugin *plugin, hash_t *state);
  
static void recreate_from_state(struct SoundPlugin *plugin, hash_t *state, bool is_loading){
  {
    hash_t *curr_state = HASH_create(10);
    create_state(plugin, curr_state);
    if (HASH_equal(state, curr_state))
      return;
  }
                                      
  bool           use_sample_file_middle_note = true ; if (HASH_has_key(state, "use_sample_file_middle_note")) use_sample_file_middle_note = HASH_get_bool(state, "use_sample_file_middle_note");
  int            instrument_number = HASH_get_int32(state, "instrument_number");
  enum ResamplerType resampler_type    = (enum ResamplerType)HASH_get_int(state, "resampler_type");
  int64_t        loop_start        = 0; if (HASH_has_key(state, "loop_start"))  loop_start  = HASH_get_int(state, "loop_start");
  int64_t        loop_length       = 0; if (HASH_has_key(state, "loop_length")) loop_length = HASH_get_int(state, "loop_length");

  const wchar_t *filename = PLUGIN_DISK_get_audio_filename(state);

  if(filename==NULL) // not supposed to happen though. Assertion in PLUGIN_DISK_get_audio_filename.
    return;

  if(set_new_sample(plugin,filename,instrument_number,resampler_type,loop_start,loop_length, use_sample_file_middle_note, is_loading)==false)
    GFX_Message(NULL, "Could not load soundfile \"%S\". (instrument number: %d)\n", filename,instrument_number);

  Data *data=(Data*)plugin->data;

  if (is_loading && disk_load_version <= 0.865){
    data->p.note_adjust = int(data->p.note_adjust);
  }

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

static void create_state(struct SoundPlugin *plugin, hash_t *state){
  R_ASSERT_RETURN_IF_FALSE(state!=NULL);
  
  Data *data=(Data*)plugin->data;

  const wchar_t *maybe_relative_filename = OS_saving_get_relative_path_if_possible(data->filename);
  
  //printf("maybe: -%s- -%s-\n", data->filename, maybe_relative_filename);
  HASH_put_string(state, "filename", maybe_relative_filename);

  HASH_put_bool(state, "use_sample_file_middle_note", data->use_sample_file_middle_note);

  HASH_put_int(state, "instrument_number",data->instrument_number);
  HASH_put_int(state, "resampler_type",data->resampler_type);

  HASH_put_int(state, "loop_start",data->loop_start);
  HASH_put_int(state, "loop_length",data->loop_length);

  if (g_embed_samples){
    const char *audiofile = DISK_file_to_base64(data->filename);
    if (audiofile != NULL)
      HASH_put_chars(state, "audiofile", audiofile);
    else
      GFX_addMessage("Unable to embed sample \"%S\". Could not read file.", maybe_relative_filename);
  }
}

const wchar_t *SAMPLER_get_filename(struct SoundPlugin *plugin, bool *is_default_sound){
  R_ASSERT_RETURN_IF_FALSE2(!strcmp("Sample Player", plugin->type->type_name), NULL);  

  Data *data=(Data*)plugin->data;
  *is_default_sound = data->using_default_sound;
  return data->filename;
}

const wchar_t *SAMPLER_get_filename_display(struct SoundPlugin *plugin){
  R_ASSERT_RETURN_IF_FALSE2(!strcmp("Sample Player", plugin->type->type_name), NULL);

  Data *data=(Data*)plugin->data;
  return data->filename;
}

static SoundPluginType plugin_type = {};

static void init_plugin_type(void){

 plugin_type.type_name                = "Sample Player";
 plugin_type.name                     = "Sample Player";
 plugin_type.info                     = "Sample Player can load XI intruments; Soundfonts, and all types of sample formats supported by libsndfile. WAV files are looped if they have loops defined in the \"sampl\" chunk, or they have \"Loop Start\" and \"Loop End\" cue id's.\n\nSoundFonts often sound better when played with FluidSynth instead of the Sample Player. The Soundfont handling in Sample Player needs more care. However, the Sample Player uses less memory, are faster to create, has sample-accurate note scheduling, supports pitch changes and polyphonic aftertouch (velocity can be changed while a note is playing), and has configurable options such as attack, decay, sustain, and release.";
 plugin_type.category                 = "Synth",
 plugin_type.num_inputs               = 2;
 plugin_type.num_outputs              = 2;
 plugin_type.is_instrument            = true;
 plugin_type.note_handling_is_RT      = false;
 plugin_type.num_effects              = EFF_NUM_EFFECTS;
 plugin_type.will_always_autosuspend  = true,
 plugin_type.get_effect_format        = get_effect_format;
 plugin_type.get_effect_name          = get_effect_name;
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
   
 plugin_type.get_peaks        = get_peaks;
 plugin_type.set_effect_value = set_effect_value;
 plugin_type.get_effect_value = get_effect_value;
 plugin_type.get_display_value_string = get_display_value_string;

 plugin_type.recreate_from_state = recreate_from_state;
 plugin_type.create_state        = create_state;

 plugin_type.data                     = NULL;
};

static SoundPluginType click_type;

void create_sample_plugin(void){

  static bool has_inited = false;

  if (has_inited==false) {
    
    init_plugin_type();

    click_type = plugin_type;

    click_type.name = g_click_name;

    has_inited = true;
  }

  PR_add_plugin_type(&plugin_type);

  PR_add_plugin_type(&click_type);
}
