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



#define __STDC_FORMAT_MACROS 1

#include <inttypes.h>

#include <math.h>

#include <QFileInfo>
#include <QSet>

#define INCLUDE_SNDFILE_OPEN_FUNCTIONS 1

#include "../common/nsmtracker.h"
#include "../common/SeqAutomation.hpp"
#include "../common/Array.hpp"
#include "../common/undo_sequencer_proc.h"

#include "SoundPlugin.h"
#include "SoundPlugin_proc.h"
#include "Peaks.hpp"
#include "SampleReader_proc.h"
#include "Granulator.hpp"
#include "Resampler_proc.h"
#include "SampleRecorder_proc.h"
#include "SoundPluginRegistry_proc.h"

#include "../api/api_proc.h"


#include "Seqtrack_plugin_proc.h"



#define NUM_INPUTS 8
#define NUM_OUTPUTS 8

#if 1
#  define FADE_OUT_MS 100
#  define FADE_IN_MS 4
#else
#  define FADE_OUT_MS 2
#  define FADE_IN_MS 2
#endif


//#define HOW_MUCH_NONSTRETCHED_TO_PREPARE (0.05 * 0.015
//#define HOW_MUCH_NONSTRETCHED_TO_PREPARE_BEFORE_STARTING 0.2

#define HOW_MUCH_TO_PREPARE 2 // in seconds. TODO: We need to slowly increase the buffer from 0.1 to 2. TODO2: Make these two numbers configurable.
#define HOW_MUCH_TO_PREPARE_BEFORE_STARTING 2 //in seconds

#define RESAMPLER_BUFFER_SIZE 1024

// This is the instrument that is bounded to ONE track in the sequencer.

/*
  Possible to change pitch while playing? (Hard to calculate sample seek then)
  Possible to change stretch while playing? Also makes it hard to calculate sample seek.
 */

enum{
  //EFF_SAMPLENUM,
  //EFF_PITCH,
  EFF_NUM_EFFECTS
};


static void set_num_visible_outputs(SoundPlugin *plugin);

static QSet<QString> g_resampler_warnings;

#define INITIAL_RECORDER_ID 0

namespace{

static int64_t g_id = INITIAL_RECORDER_ID+1;

struct MyReader : public radium::GranulatorCallback{
  radium::LockAsserter lockAsserter;

private:

  float _curr_fade_volume;
  
  float _fadeout_inc;
  float _fadein_inc;
  int _fadeout_countdown;

  Smooth _volume;
  
public:

  radium::SampleReader *_reader;
  
  const int _num_ch;
  
  radium::Granulator _granulator;

  bool _do_granulate = false;

  struct ResamplerData{
    int _ch;
    float *_buffer;
    radium::Resampler *_resampler;
    radium::SampleReader *_reader;

    ResamplerData(int ch, radium::SampleReader *reader)
      : _ch(ch)
      , _reader(reader)
    {
      _resampler = RESAMPLER_create(ResamplerData::RT_src_callback, 1, this, RESAMPLER_SINC1);
      _buffer = (float*)calloc(sizeof(float), RESAMPLER_BUFFER_SIZE);
    }

    ~ResamplerData(){
      RESAMPLER_delete(_resampler);
      free(_buffer);
    }

    // samplerater callback
    static long RT_src_callback(void *cb_data, float **out_data){
      ResamplerData *data = static_cast<ResamplerData*>(cb_data);
      
      int num_frames;
      *out_data = RT_SAMPLEREADER_get_buffer(data->_reader, data->_ch, num_frames);
      
      return num_frames;
    }

  };

  ResamplerData **_resamplers;
  bool _do_resampling = false;
  double _resampler_ratio = 1.0; // Note: Must be set between processing buffers so that channels aren't resampled differently.


  MyReader(radium::SampleReader *reader)
    : _reader(reader)
    , _num_ch(SAMPLEREADER_get_num_channels(reader))
    , _granulator(_num_ch, this)
  {
    
    SMOOTH_init(&_volume, 0.0f, MIXER_get_buffer_size());
    
    _resamplers = (ResamplerData**)calloc(sizeof(ResamplerData*), _num_ch);
    for(int ch=0 ; ch<_num_ch ; ch++)
      _resamplers[ch] = new ResamplerData(ch, reader);

    double samplerate = SAMPLEREADER_get_samplerate(reader);

    if (fabs(samplerate-pc->pfreq) > 1){

      const wchar_t *filename = SAMPLEREADER_get_filename(reader);
      QString qfilename = STRING_get_qstring(filename);

      if (g_resampler_warnings.contains(qfilename)==false){
        GFX_addMessage("Warning: \"%S\" has a samplerate of %d, while radium runs at %d\n. To compensate the difference, Radium will perform high quality samplerate conversion during runtime, which will use a bit CPU.",
                       SAMPLEREADER_get_sample_name(reader),
                       (int)samplerate,
                       (int)pc->pfreq);                     
        g_resampler_warnings.insert(qfilename);
      }

      _resampler_ratio = (double)pc->pfreq / samplerate;
      _do_resampling = true;
    }
  }
    

  ~MyReader(){
    {
      for(int ch=0 ; ch<_num_ch ; ch++)
        delete _resamplers[ch];
      free(_resamplers);
      _resamplers=NULL;
    }

    {      
      SAMPLEREADER_delete(_reader);
      //printf("     =========SAMPLE c/d: Deleted 1: %p\n", _reader);
      _reader = NULL;
    }

    SMOOTH_release(&_volume);
  }

private:
  
  void RT_get_samples_from_disk(float **outputs, int num_frames) const {
    R_ASSERT_RETURN_IF_FALSE(_reader != NULL);

    float *outputs2[_num_ch];

    int total_read = 0;

    while(total_read < num_frames){

      for(int ch = 0 ; ch < _num_ch ; ch++)
        outputs2[ch] = &outputs[ch][total_read];

      const int num_frames_left = num_frames - total_read;

      // Note: Both RT_SAMPLEREADER_read and RT_SAMPLEREADER_get_buffer are used, but not in the same play session.
      const int num_read = RT_SAMPLEREADER_read(_reader, outputs2, num_frames_left);

      R_ASSERT_RETURN_IF_FALSE(num_read>0);

      total_read += num_read;
      R_ASSERT(total_read <= num_frames);
    }
  }

  // granulator callback
  float *get_next_granulator_sample_block(const int ch, int &num_frames) override {
    //printf("   ************************** ASKING for %d frames\n", num_frames);
    if (_do_resampling){

      num_frames = RESAMPLER_read(_resamplers[ch]->_resampler, _resampler_ratio, RESAMPLER_BUFFER_SIZE, _resamplers[ch]->_buffer);

      return _resamplers[ch]->_buffer;

    } else {

      return RT_SAMPLEREADER_get_buffer(_reader, ch, num_frames);

    }
  }

  void RT_only_resample_not_granulate(float **output2, const int num_frames) const {

    for(int ch = 0 ; ch < _num_ch ; ch++){
      int samples_left = num_frames;

      int pos = 0;
      while (samples_left > 0){

        int num_frames_read = RESAMPLER_read(_resamplers[ch]->_resampler, _resampler_ratio, samples_left, &output2[ch][pos]);

        R_ASSERT_RETURN_IF_FALSE(num_frames_read > 0);
        
        pos += num_frames_read;
        samples_left -= num_frames_read;
      }
    }
  }

public:

  void RT_get_samples_with_volume_applied(int num_frames, float **outputs, bool do_add, bool add_to_ch1_too) const {

    R_ASSERT_NON_RELEASE(num_frames<=RADIUM_BLOCKSIZE);

    // 1. Set up temporary buffers
    //
    float temp_buffer_sound[_num_ch*num_frames];
    
    float *temp_buffers[_num_ch];
    for(int ch=0;ch<_num_ch;ch++)
      temp_buffers[ch] = &temp_buffer_sound[ch*num_frames];

    
    // 2. Get samples
    //
    if (_do_granulate){

      _granulator.RT_process(do_add ? temp_buffers : outputs, num_frames);

    } else if (_do_resampling){

      RT_only_resample_not_granulate(do_add ? temp_buffers : outputs, num_frames);

    } else {

      RT_get_samples_from_disk(do_add ? temp_buffers : outputs, num_frames);

    }


    // 3. Apply volume
    //
    if (do_add){
        
      if (add_to_ch1_too && _num_ch==1) {
        
        SMOOTH_mix_sounds_from_mono_to_stereo(&_volume, outputs[0], outputs[1], temp_buffers[0], num_frames);
        
      } else {

        for(int ch=0;ch<_num_ch;ch++)
          SMOOTH_mix_sounds(&_volume, outputs[ch], temp_buffers[ch], num_frames);
        
      }
      
    } else {

      for(int ch=0;ch<_num_ch;ch++)
        SMOOTH_apply_volume(&_volume, outputs[ch], num_frames);
      
    }

    //printf("volume: %f. _num_ch: %d. num_frames: %d\n", _volume, _num_ch, num_frames);
    /*
    // This is not correct when adding.
    float volume = _volume;
    for(int ch=0;ch<_num_ch;ch++){
      float *output = outputs[ch];
      for(int i=0;i<num_frames;i++)
        output[i] *= volume;
    }
    */
  }
  
private:
  
  void RT_add_fade_in_samples(int num_frames, float **outputs){
    float outputs_buffer[_num_ch * num_frames];
    float *inputs[_num_ch];
    
    for(int ch = 0 ; ch < _num_ch ; ch++)
      inputs[ch] = &outputs_buffer[ch*num_frames];

    RT_get_samples_with_volume_applied(num_frames, inputs, false, false);

#define INC_FADE_VOLUME()                       \
    _curr_fade_volume += _fadein_inc;           \
    if (_curr_fade_volume > 1.0)                \
      _curr_fade_volume = 1.0;

    //printf("Fade in %f\n", _curr_fade_volume);

    if (_num_ch == 1) {

      float *input0 = inputs[0];
      float *output0 = outputs[0];
      float *output1 = outputs[0];
      
      for(int i=0;i<num_frames;i++){
        
        float sample = _curr_fade_volume * input0[i];
        output0[i] += sample;
        output1[i] += sample;

        INC_FADE_VOLUME();
        
      }
      
    } else {
    
      for(int i=0;i<num_frames;i++){
        
        for(int ch = 0 ; ch < _num_ch ; ch++){
          float sample = _curr_fade_volume * inputs[ch][i];
          outputs[ch][i] += sample;
        }

        INC_FADE_VOLUME();
      }
    }

#undef INC_FADE_VOLUME
    
  }
  
public:
  
  void RT_add_samples(int num_frames, float **outputs){
    SMOOTH_called_per_block(&_volume);
    
    if (_curr_fade_volume < 1.0)
      RT_add_fade_in_samples(num_frames, outputs);
    else
      RT_get_samples_with_volume_applied(num_frames, outputs, true, true);
  }

  void RT_add_fade_out_samples(int num_frames, float **outputs){
    LOCKASSERTER_EXCLUSIVE(&lockAsserter);

    SMOOTH_called_per_block(&_volume);
      
    R_ASSERT_RETURN_IF_FALSE(_fadeout_countdown > 0);

    int num_fade_out_frames = R_MIN(num_frames, _fadeout_countdown);
    
    float outputs_buffer[_num_ch * num_fade_out_frames];
    float *inputs[_num_ch];
    
    for(int ch = 0 ; ch < _num_ch ; ch++)
      inputs[ch] = &outputs_buffer[ch*num_fade_out_frames];
    
    //printf("Fading out %p. Countdown: %d\n", _reader, _fadeout_countdown);
    
    RT_get_samples_with_volume_applied(num_fade_out_frames, inputs, false, false);

    for(int i=0;i<num_fade_out_frames;i++){

      float volume = _curr_fade_volume;

      if (_num_ch == 1){
        
        float *from = inputs[0];
        float *to0 = outputs[0];
        float *to1 = outputs[1];
        float sample = volume * from[i];
        to0[i] += sample;
        to1[i] += sample;
        
      } else {
        
        for(int ch = 0 ; ch < _num_ch ; ch++) {
          float *from = inputs[ch];
          float *to = outputs[ch];
          float sample = volume * from[i];
          to[i] += sample;
        }
        
      }

      
      _curr_fade_volume -= _fadeout_inc;
      if (_curr_fade_volume <= 0){
        _fadeout_countdown = 0;
        _curr_fade_volume = 0.0; // should not be necessary.
        return;
      }
    }

    _fadeout_countdown -= num_fade_out_frames;
  }

  void prepare_for_fadein(void){
    _curr_fade_volume = 0.0;
    double fadein_duration = (double)FADE_IN_MS * (double)pc->pfreq / 1000.0;
    _fadein_inc = 1.0 / fadein_duration;
  }
  
  void prepare_for_fadeout(void){
    int fadeout_duration = (double)FADE_OUT_MS * (double)pc->pfreq / 1000.0;
    _fadeout_inc = 1.0 / fadeout_duration;
    _fadeout_countdown = fadeout_duration + 16; // Add a little bit to avoid tiny tiny chance that the fade curve doesn't quite reach 0.
  }

  bool _has_set_volume = false;
  
  void set_volume(float new_volume){
    if (new_volume >= 0){
      
      //printf("Seqtrackplugin volume set to %f\n", new_volume);
      if (_has_set_volume==false) {
        SMOOTH_force_target_value(&_volume, new_volume);
        _has_set_volume = true;
      } else
        SMOOTH_set_target_value(&_volume, new_volume);
      
    }else{
      R_ASSERT_NON_RELEASE(false);
    }
  }

  int get_fadeout_duration(){
    double stretch = mus_increment(_granulator._clm_granulators[0]);
    return (_fadeout_countdown / stretch);
  }


  void prepare_for_playing(int64_t sample_start_pos, double stretch, bool do_fade_in, radium::FutureSignalTrackingSemaphore *gotit){
    _do_granulate = fabs(stretch - 1.0) > 0.001;
    
    for(int ch=0;ch<_num_ch;ch++){
      mus_reset(_granulator._clm_granulators[ch]);
      RESAMPLER_reset(_resamplers[ch]->_resampler);
    }

    _has_set_volume = false; // To avoid smoothly gliding from previous volume (the last time we played) to the current.
      
    if (do_fade_in==false){
      //printf("No fadein\n");
      _curr_fade_volume = 1.0;
    }else
      prepare_for_fadein();

    // TODO: This number should be dynamically calculated somehow depending on how long time it takes to read from disk.
    int64_t how_much_to_prepare = double(HOW_MUCH_TO_PREPARE_BEFORE_STARTING) * (double)pc->pfreq / (stretch * _resampler_ratio);

    for(int ch=0;ch<_num_ch;ch++)
      how_much_to_prepare = R_MAX(how_much_to_prepare,
                                  2 * mus_granulate_grain_max_length(_granulator._clm_granulators[ch]));
                                        

    {
      LOCKASSERTER_EXCLUSIVE(&lockAsserter);
      
      SAMPLEREADER_prepare_to_play(_reader, sample_start_pos / _resampler_ratio, how_much_to_prepare, gotit);
    }

    //printf("      ====   %p has now been prepared\n", this);
  }
  
  bool is_finished(void){
    return _fadeout_countdown == 0;
  }
};


struct Sample{
  int64_t _id = g_id++;
  const wchar_t *_filename;

  enum State{ // clang doesn't allow enum class here.
    RUNNING,
    RT_REQUEST_DELETION,
    READY_FOR_DELETION
  };
  DEFINE_ATOMIC(enum State, _state) = State::RUNNING;

  MyReader *_curr_reader = NULL; // Currently playing reader.
  radium::Vector<MyReader*> _fade_out_readers; // Currently playing fade-out readers
  radium::Vector<MyReader*> _free_readers;
  
  radium::SampleReader *_reader_holding_permanent_samples;

  DEFINE_ATOMIC(bool, _is_fading_out) = false;
  
  int _num_readers = 0; // Only read/written by the main thread.

  radium::DiskPeaks *_peaks;

  radium::GcHolder<const struct SeqBlock> _seqblock;
  Seqblock_Type _type;
  
  float _pitch = 1.0;
  
  const int _num_ch;
  const int64_t _total_num_frames_in_sample;
  bool _do_resampling;
  double _resampler_ratio;

  const unsigned int _color;
  const wchar_t *_filename_without_path;
  
  bool _is_playing = false; // If false, we are not playing current. But fade-outs can play even if _is_playing==false.
  int _click_avoidance_fade_in = 0; // frames left while fading in.

  bool _do_looping = false;

  radium::LockAsserter lockAsserter;
  
  Sample(const wchar_t *filename, radium::SampleReader *reader1, radium::SampleReader *reader2, const struct SeqBlock *seqblock, Seqblock_Type type)
    : _filename(wcsdup(filename))
      //, _reader(NULL) //new MyReader(reader))
      //, _fade_out_reader(new MyReader(fade_out_reader))
    , _reader_holding_permanent_samples(SAMPLEREADER_create(filename))
    , _peaks(DISKPEAKS_get(filename))
    , _seqblock(seqblock)
    , _type(type)
    , _num_ch(SAMPLEREADER_get_num_channels(reader1))
    , _total_num_frames_in_sample(SAMPLEREADER_get_total_num_frames_in_sample(reader1))
    , _color(SAMPLEREADER_get_sample_color(filename))
    , _filename_without_path(wcsdup(SAMPLEREADER_get_sample_name(reader1)))
  {
    //printf("     =========SAMPLE c/d: Alloced 1: %p\n", _reader_holding_permanent_samples);
    
    R_ASSERT(THREADING_is_main_thread());
    
    LOCKASSERTER_EXCLUSIVE(&lockAsserter);

#if !defined(RELEASE)
    QFileInfo info(STRING_get_qstring(_filename));
    if (!info.isAbsolute())
      abort();
#endif


    // prepare readers.
    {
      _free_readers.push_back(new MyReader(reader1));
      _free_readers.push_back(new MyReader(reader2));

      _num_readers = 2;

#if defined(RELEASE)
      _fade_out_readers.reserve(32); // Reserve some elements so we (probably) don't have to allocate later. (32 is NOT the maxium number of fade-out readers)
      _free_readers.reserve(32);     // ---""----
#else
      _fade_out_readers.reserve(_num_readers);
#endif
    }

    MyReader *myreader = _free_readers.at(0);
    _do_resampling = myreader->_do_resampling;
    _resampler_ratio = myreader->_resampler_ratio;

    if(type==Seqblock_Type::REGULAR)
      interior_start_may_have_changed();
  }

  ~Sample(){
    R_ASSERT(THREADING_is_main_thread());
    
    LOCKASSERTER_EXCLUSIVE(&lockAsserter);

    R_ASSERT(ATOMIC_GET(_state)!=State::RUNNING);
    
    R_ASSERT_NON_RELEASE(_curr_reader==NULL);

    if(ATOMIC_GET(_state)==State::READY_FOR_DELETION){
      R_ASSERT_NON_RELEASE(_fade_out_readers.size()==0);
      R_ASSERT_NON_RELEASE(ATOMIC_GET(_is_fading_out)==false);
      R_ASSERT_NON_RELEASE(_free_readers.size()==_num_readers);
    }

    delete _curr_reader;

    for(auto *reader : _fade_out_readers){
      delete reader;
    }
    for(auto *reader : _free_readers){
      delete reader;
    }
    
    SAMPLEREADER_delete(_reader_holding_permanent_samples);
    //printf("      =========SAMPLE c/d: Deleted 2: %p\n", _reader_holding_permanent_samples);
    _reader_holding_permanent_samples = NULL;
    
    DISKPEAKS_remove(_peaks);
    
    free((void*)_filename);
    free((void*)_filename_without_path);
  }

  void interior_start_may_have_changed(void){
    double start = _seqblock->t.interior_start;
    double end = _seqblock->t.interior_start + ((double)HOW_MUCH_TO_PREPARE_BEFORE_STARTING * (double)pc->pfreq / _seqblock->t.stretch);
    //printf("o: %f / %f\n", start, end);
    end = R_MIN(end, _seqblock->t.interior_end);

    start /= _resampler_ratio;
    start = R_MAX(0, start-1); // subtract 1 in case floating point inaccuracy caused number (for some reason) to be one higher.

    end /= _resampler_ratio;
    end = R_MIN(end+1, _total_num_frames_in_sample); // add 1 in case floating point inaccuracy caused number (for some reason) to be one higher.

    if (start >= end){
      R_ASSERT_NON_RELEASE(start-16 < end);
      return;
    }

    SAMPLEREADER_set_permanent_samples(_reader_holding_permanent_samples, start, end);
  }


  /*
  void set_playing(bool is_playing){
    if (is_playing != _is_playing) // usually they have the same value.
      _is_playing = is_playing;
  }
             
  void start_playing(void){
    R_ASSERT(_is_playing==false);
    _is_playing = true;
  }

  void stop_playing(void){
    R_ASSERT(_is_playing==true);
    _is_playing = false;
  }
  */

  void RT_stop_playing(bool do_fade_out){    
    //printf("   player_is_stopped called %f. _curr_reader: %p\n", TIME_get_ms() / 1000.0, _curr_reader);
  
    if (_curr_reader!=NULL) {

      LOCKASSERTER_EXCLUSIVE(&_curr_reader->lockAsserter);
      
      //printf("    ==== RT_stop_playing stopping player. Fading out: %d\n", do_fade_out);

      if(do_fade_out && _is_playing){
        
        _curr_reader->prepare_for_fadeout();
        
        _fade_out_readers.push_back(_curr_reader);
        ATOMIC_SET(_is_fading_out, true);
        
      } else {

        int available_space = _free_readers.free_space();

        RT_SAMPLEREADER_release_all_cached_data(_curr_reader->_reader);
        
        if (available_space > 0)
          _free_readers.push_back(_curr_reader);
        else
          R_ASSERT(false);
        
      }
      
      _curr_reader = NULL;
      _is_playing = false;
    }

  }

  bool I_am_playing(void){
    return is_playing_song()==true || ATOMIC_GET(_is_fading_out)==true;
  }
  
  void prepare_to_play(int64_t seqtime, radium::FutureSignalTrackingSemaphore *gotit){
    R_ASSERT(THREADING_is_main_thread());
    
    //printf("   PREPARE_TO_PLAY called %f. _curr_reader: %p\n", TIME_get_ms() / 1000.0, _curr_reader);
    
    R_ASSERT_RETURN_IF_FALSE(is_playing()==false);
    //R_ASSERT_RETURN_IF_FALSE(_is_playing==false);
    R_ASSERT_RETURN_IF_FALSE(_curr_reader==NULL);

    if (ATOMIC_GET(_state)!=State::RUNNING)
      return;
      
    if (seqtime >= _seqblock->t.time2)
      return;

    int64_t sample_start_pos;

    if(seqtime <= _seqblock->t.time)
      sample_start_pos = _seqblock->t.interior_start;
    else
      sample_start_pos = scale_int64(seqtime,
                                     _seqblock->t.time, _seqblock->t.time2,
                                     _seqblock->t.interior_start, _seqblock->t.interior_end);
    //printf("sample_start_pos: %d. _interior_start: %d. _seqblock->t.time: %d. %d vs. %d\n",int(sample_start_pos), int(_seqblock->t.interior_start), int(_seqblock->t.time), int(_seqblock->t.time2-_seqblock->t.time), int(_seqblock->t.interior_end-_seqblock->t.interior_start));
    //sample_start_pos = _interior_start + seqtime - _seqblock->t.time;
    
    if (sample_start_pos < 0){
      R_ASSERT(false);
      return;
    }
    
    if (sample_start_pos >= _seqblock->t.interior_end){
      R_ASSERT(false);
      return;
    }

    //printf("  PREPARE TO PLAY. Time: %f. sample_start_pos: %f\n", (double)seqtime / (double)pc->pfreq, (double)sample_start_pos / (double)pc->pfreq);
    
    MyReader *allocated_reader;

    {
      radium::SampleReader *samplereader = SAMPLEREADER_create(_filename); // light operation
      if (samplereader==NULL)
        return;
      //printf("     =========SAMPLE c/d: Alloced 2: %p\n", samplereader);
      allocated_reader = new MyReader(samplereader); // light operation
    }
    
    MyReader *reader;

    
    bool num_free_readers;

    bool i_am_playing = I_am_playing();
    
    {
      radium::PlayerLock lock; // Must lock even if playing since RT_process (which sometimes pushes to _free_readers) is called when not playing.
      
      LOCKASSERTER_EXCLUSIVE(&lockAsserter);
      
      num_free_readers = _free_readers.size();
      if (num_free_readers > 0){
        reader = _free_readers.pop_back();
      } else {
        reader = allocated_reader;
        _num_readers++;
      }
    }
    
    if (num_free_readers==0){
      
      // Make sure the myreader vectors are large enough (TODO: Avoid extra locking of player here)
      _fade_out_readers.reserve_in_realtime_safe_manner(_num_readers, i_am_playing);
      _free_readers.reserve_in_realtime_safe_manner(_num_readers, i_am_playing);
      
    }

    bool do_fade_in = sample_start_pos > _seqblock->t.interior_start;
    reader->prepare_for_playing(sample_start_pos, _seqblock->t.stretch, do_fade_in, gotit);

    {
      _curr_reader = reader; // Don't need player lock here since _is_playing==false now.
    }

    if (reader != allocated_reader){
      R_ASSERT(num_free_readers>0);
      delete allocated_reader; // light operation
    }else{
      R_ASSERT(num_free_readers==0);
    }
  }

  /*
  void set_interior_start(int64_t interior_start){
    radium::PlayerLock lock;
    _seqblock->t.interior_start = interior_start;
    _seqblock->gfx.interior_start = interior_start;
  }

  void set_interior_end(int64_t interior_end){
    radium::PlayerLock lock;
    _seqblock->t.interior_end = interior_end;
    _seqblock->gfx.interior_end = interior_end;
  }
  */
  
  void RT_process(int num_frames, float **outputs){
    LOCKASSERTER_EXCLUSIVE(&lockAsserter);

    //printf("RT_Process. _curr_reader: %p. Volume: %f. is1: %d. is2: %d\n", _curr_reader, _seqblock->curr_gain, is_really_playing_song(), _is_playing);
    
    // Playing Fadeouters
    //
    {
      
      // process audio
      for(auto *fade_out_reader : _fade_out_readers)
        fade_out_reader->RT_add_fade_out_samples(num_frames, outputs);

      // remove finished fade-out readers.
      {
      again:

        for(auto *fade_out_reader : _fade_out_readers){
          
          if (fade_out_reader->is_finished()) {
            
            RT_SAMPLEREADER_release_all_cached_data(fade_out_reader->_reader);
            
            _fade_out_readers.remove(fade_out_reader);

            if(_fade_out_readers.size()==0)
              ATOMIC_SET(_is_fading_out, false);
              
            int available_space = _free_readers.free_space();
            
            if (available_space > 0)
              _free_readers.push_back(fade_out_reader);
            else
              R_ASSERT(false);
            
            goto again;
            
          }

        }
      }

    }

    
    
    // Various
    //
    {
      
      if (ATOMIC_GET(_state)==State::RT_REQUEST_DELETION){
        
        RT_stop_playing(true);
        R_ASSERT(_curr_reader==NULL);
        
        if (_fade_out_readers.size()==0)
          ATOMIC_SET(_state, State::READY_FOR_DELETION);
        
      }
      

      /*
        This test should not be necessary since rt_stop_playing should always be called when stopping to play.
        The test also causes a click when _curr_reader is being transferred into _fade_out_readers. We lose one block then.

        In short: _curr_reader must always be played when it is there.
        In addition: If we return here, we risk not preparing enough data for the fade out readers since RT_SAMPLEREADER_called_per_block is not called.

        if (is_really_playing_song()==false)
          return;
      */

      /*
        Not sure if we can have this test either. It could seem like _is_playing might be false even when fading out, and then RT_SAMPLEREADER_called_per_block wouldn't be called.

        if (_is_playing==false)
          return;
      */
    }

    bool is_playing = _is_playing;

    // Playing Current
    //
    if (is_playing && _curr_reader != NULL){ // move the _is_playing test above here instead. Note: Must check 'is_playing' before '_curr_reader' to avoid tsan hit.
      LOCKASSERTER_EXCLUSIVE(&_curr_reader->lockAsserter);

      // Set expansion (stretch/expansion has probably not changed since last time, but this is a light operation)
      for(int ch=0;ch<_num_ch;ch++)
        mus_set_increment(_curr_reader->_granulator._clm_granulators[ch], _seqblock->t.stretch);

      // Set volume
      _curr_reader->set_volume(_seqblock->curr_gain);

      // Add samples
      _curr_reader->RT_add_samples(num_frames, outputs);
    }

    
    // Request reading from disk for Fadeouters
    //
    for(auto *fade_out_reader : _fade_out_readers){
      int how_much_to_prepare = 1 + SLICE_SIZE + fade_out_reader->get_fadeout_duration();
      RT_SAMPLEREADER_called_per_block(fade_out_reader->_reader, how_much_to_prepare);
    }

    
    // Request reading from disk for Current
    //
    if (is_playing && _curr_reader){ // Note: Must check 'is_playing' before '_curr_reader' to avoid tsan hit.
      int64_t how_much_to_prepare = double(HOW_MUCH_TO_PREPARE) * (double)pc->pfreq / _seqblock->t.stretch;

      for(int ch=0;ch<_num_ch;ch++)
        how_much_to_prepare = R_MAX(how_much_to_prepare,
                                    2 * mus_granulate_grain_max_length(_curr_reader->_granulator._clm_granulators[ch]));
      
      RT_SAMPLEREADER_called_per_block(_curr_reader->_reader, how_much_to_prepare);
    }
  }

  // Called after scheduler and before audio processing.
  bool RT_called_per_block(struct SeqTrack *seqtrack, int64_t curr_start_time, int64_t curr_end_time){ // (end_time-start_time is usually RADIUM_BLOCKSIZE.)
  
    if (ATOMIC_GET(_state)!=Sample::State::RUNNING || is_really_playing_song()==false){
      //_is_playing = false;
      return false;
    }

    // Must place this line after the is_really_playing_song() check. If not, we sometimes (quite rarely) gets lockAsserter hit here and in prepare_to_play.
    LOCKASSERTER_EXCLUSIVE(&lockAsserter);
    
    //printf("   RT_called_per_block %d -> %d (%d)\n", (int)curr_start_time, (int)curr_end_time, int(curr_end_time-curr_start_time));
    
    bool inside = curr_start_time >= _seqblock->t.time && curr_start_time < _seqblock->t.time2;

    if (inside != _is_playing) {
    
      if (inside) {
        
        //printf(" PLAYING SAMPLE. Time: %f\n", (double)curr_start_time / (double)pc->pfreq);
        
        atomic_pointer_write_relaxed((void**)&seqtrack->curr_sample_seqblock, (void*)_seqblock.data()); // bang!
        GFX_ScheduleEditorRedraw();
        
        _is_playing = true;
        
        // Don't do this if starting to play from the beginning.
        _click_avoidance_fade_in = FADE_IN_MS * pc->pfreq / 1000;
        
      } else {
        
        if (atomic_pointer_read_relaxed((void**)&seqtrack->curr_sample_seqblock)==_seqblock.data()){
          atomic_pointer_write_relaxed((void**)&seqtrack->curr_sample_seqblock, NULL);
          GFX_ScheduleEditorRedraw();
        }
        
        RT_stop_playing(false);
      }
    }

    return _is_playing || _seqblock->t.time > curr_start_time;
  }

};

 
enum{
 NOT_RECORDING = 0,
 READY_TO_RECORD,
 IS_RECORDING,
 STOP_RECORDING
};

#define HIGHEST_RECORDER_ID -2
static int64_t g_recorder_id = HIGHEST_RECORDER_ID;

struct Recorder;
static QHash<int64_t, Recorder*> g_recorders;
static QVector<Recorder*> g_successfully_finished_recorders;
 
struct Recorder : public radium::SampleRecorderInstance{

  const int64_t _id = g_recorder_id--;
  
  radium::Array<radium::Peaks*> _peaks;

  //Data *_data;
  
  radium::GcHolder<struct SeqBlock> _seqblock;
  radium::GcHolder<struct SeqTrack> _seqtrack;

  const SeqtrackRecordingConfig _config;
  
  radium::String _filename;
  
  int _seqtrack_recording_generation;
  
  Recorder(struct SeqTrack *seqtrack, const wchar_t *recording_path, const SeqtrackRecordingConfig *config)
    : SampleRecorderInstance(recording_path, get_num_recording_soundfile_channels(config), 48) // 60 is standard value, I think, but the sample recorder adds 12 to the middle note, for some reason.
    , _peaks(num_ch)
      //, _data(data)
    , _seqtrack(seqtrack)
    , _config(*config)
    , _seqtrack_recording_generation(++seqtrack->recording_generation)
  {
    printf("-------------------------     recorder CONSTRUCTOR %d (%p)\n", (int)_seqtrack_recording_generation, this);
    R_ASSERT(THREADING_is_main_thread());
    R_ASSERT(seqtrack->for_audiofiles);
      
    for(int ch=0;ch<num_ch;ch++)
      _peaks.set(ch, new radium::Peaks);

    g_recorders[_id] = this;
  }

  ~Recorder(){
    R_ASSERT(THREADING_is_main_thread());
    
    printf("-------------------------     recorder DESTRUCTOR %d (%p)\n", (int)_seqtrack_recording_generation, this);
    g_recorders.remove(_id);
    
    for(int ch=0;ch<num_ch;ch++)
      delete _peaks[ch];
  }

  void insert_audiofile(void){
    R_ASSERT(THREADING_is_main_thread());
    R_ASSERT_RETURN_IF_FALSE(g_successfully_finished_recorders.contains(this));

    struct SeqTrack *seqtrack = _seqtrack.data();

    int seqtracknum = get_seqtracknum(seqtrack);

    if (seqtracknum < 0){
      
      printf(" Recorder::insert_audiofile: Seqtrack not found in song. Deleted?\n");
      
    } else {

      if (_seqblock.data() != NULL)
        SEQTRACK_remove_recording_seqblock(seqtrack, _seqblock.data());
      else{
        R_ASSERT_NON_RELEASE(false);
      }

      if (SAMPLEREADER_register_deletable_audio_file(_filename.get())==true) {

        ADD_UNDO(Sequencer());

        SEQTRACK_insert_sample(seqtrack, seqtracknum, _filename.get(), start, end);
      }

    }
  }
 
  void is_finished(bool success, wchar_t *filename){
    struct SeqTrack *seqtrack = _seqtrack.data();
    
    printf("-------------------------        recorder IS_FINISHED start %d / %d  (%p)\n", (int)seqtrack->recording_generation, (int)_seqtrack_recording_generation, this);

    int seqtracknum = get_seqtracknum(seqtrack);
    if (seqtracknum < 0){
      printf(" Recorder::is_finished: Seqtrack not in song. Deleted?\n");
      R_ASSERT_NON_RELEASE(false);
      return;
    }

    if(_seqtrack_recording_generation==seqtrack->recording_generation){
      SEQTRACK_set_recording(seqtrack, false);
    }

    if (_has_started==false)
      R_ASSERT(_seqblock.data()==NULL);
    
    printf("-------------------------        recorder IS_FINISHED end   %d / %d  (%p). Success: %d. Filename: %S\n", (int)seqtrack->recording_generation, (int)_seqtrack_recording_generation, this, success, filename);

    if (success) {

      QFileInfo info(STRING_get_qstring(filename));
      _seqblock->sample_filename_without_path = talloc_wcsdup(STRING_create(info.fileName()));
      
      _filename = filename;
      g_successfully_finished_recorders.push_back(this);
      
    } else {
      
      delete this;
      
    }

    SEQTRACK_update(seqtrack);
  }

  
private:

  int64_t _start;
  int64_t _end;

  void called_when_started(void){
    R_ASSERT(_seqblock.data()==NULL);
    
    PLAYER_lock();{
      _start = radium::SampleRecorderInstance::start;
    }PLAYER_unlock();
    
    _end = _start + RADIUM_BLOCKSIZE;
    
    _seqblock.set(SEQTRACK_add_recording_seqblock(_seqtrack.data(), _start, _end));
    
    if (_seqblock.data() != NULL){ // Not supposed to be NULL, but we should have already gotten an assertion report if it has happened. This check is only to avoid crash.
      _seqblock->sample_id = _id;
      _seqblock->sample_filename_without_path = L"Recording...";
      SEQTRACK_update(_seqtrack.data());
    }
  }

  bool _has_started = false;
  
  
public:
  
  void add_recorded_peak(int ch, float min_peak, float max_peak){

    _peaks[ch]->add(radium::Peak(min_peak, max_peak));

    if (ch==num_ch-1){

      if (_has_started==false){

        called_when_started();
        
        _has_started = true;
        
      } else {

        _end += RADIUM_BLOCKSIZE;

        if (_seqblock.data() != NULL){
          _seqblock->t.time2 = _end;
          _seqblock->t.interior_end = _end - _start;
        }
        
      }      

      //printf("Seqblock: %f -> %f. (%f -> %f). Stretch: %f. _start: %f\n", (double)_seqblock->t.time / 48000.0, (double)_seqblock->t.time2 / 48000.0, (double)_seqblock->t.interior_start / 48000.0, (double)_seqblock->t.interior_end / 48000.0, (double)_seqblock->t.stretch, (double)start / 48000.0);
      
      SEQTRACK_update(_seqtrack.data(), _end-RADIUM_BLOCKSIZE, _end);
    }

  }
};


 
struct Data {
  radium::Vector<Sample*> _samples;
  radium::Vector<Sample*> _gfx_samples;
  radium::Vector<Sample*> _gfx_gfx_samples; // used when moving several samples.

  DEFINE_ATOMIC(int, _recording_status) = 0;
  Recorder *_recorder = NULL;
    
  float _sample_rate;

#if !defined(RELEASE)
  
private:
  
  bool wait_for_main_thread_before_asserting_samples = false;
  radium::Mutex assert_samples_mutex; // Need lock (i.e. atomic variable is not enough) since all of assert_samples needs to be protected, not just access to wait_for_main_thread_before_asserting_samples.
  
public:
  
  struct SeqTrack *get_seqtrack(void) const {
    VECTOR_FOR_EACH(struct SeqTrack *, seqtrack, &root->song->seqtracks){
      if (seqtrack->patch != NULL){
        SoundPlugin *plugin = (SoundPlugin*) seqtrack->patch->patchdata;        
        R_ASSERT(plugin!=NULL);
        if(plugin->data==this)
          return seqtrack;
      }
    }END_VECTOR_FOR_EACH;
    return NULL;
  }
  
  void assert_samples(const struct SeqTrack *seqtrack = NULL) {    
    if(THREADING_is_main_thread()==false)  // Too much work to maintain multithreaded sample assertion. _samples can only be modified from main thread anyway.
      return;
    
    radium::ScopedMutex lock(assert_samples_mutex);
      
    if(THREADING_is_main_thread()==false){
      if (wait_for_main_thread_before_asserting_samples==true)
        return;
    } else {
      wait_for_main_thread_before_asserting_samples = false;
    }
    
    if (seqtrack==NULL)
      seqtrack=get_seqtrack();
    if(seqtrack==NULL){
      if (g_is_loading)
        return;
      abort();
    }
    for(auto *sample : _samples){
      if (ATOMIC_GET(sample->_state)==Sample::State::RUNNING){
        int num = 0;
        VECTOR_FOR_EACH(struct SeqBlock *, seqblock, &seqtrack->seqblocks){
          if(seqblock==sample->_seqblock.data()){
            R_ASSERT(seqblock->block==NULL);
            R_ASSERT(seqblock->sample_id == sample->_id);
            num++;
          }
        }END_VECTOR_FOR_EACH;
        if (num != 1){
          abort();
          fprintf(stderr,"NUM: %d (%d)\n", num, num != 1);
          getchar(); //abort();
        }
      }
    }
  }
  
  void temporarily_suspend_sample_assertions_from_other_threads(void){
    R_ASSERT(THREADING_is_main_thread());
    radium::ScopedMutex lock(assert_samples_mutex);
    wait_for_main_thread_before_asserting_samples = true;
  }
  
#else
  
  void assert_samples(const struct SeqTrack *seqtrack = NULL) {
  }

  void temporarily_suspend_sample_assertions_from_other_threads(void){
  }
  
#endif


  Data(float sample_rate)
    :_sample_rate(sample_rate)
  {
    R_ASSERT(THREADING_is_main_thread());
  }

  ~Data(){
    R_ASSERT(THREADING_is_main_thread());
    
    //R_ASSERT(g_is_loading || _samples.size()==0);
    
    for(auto *sample : _samples){
      ATOMIC_SET(sample->_state, Sample::State::RT_REQUEST_DELETION); // To avoid assertion hit.
      
      //R_ASSERT_NON_RELEASE(sample->_curr_reader==NULL);
      sample->RT_stop_playing(false); // just in case _curr_reader!=NULL.
      
      delete sample;
    }
  }

private:

  void start_recording(int64_t start_time){
    ATOMIC_SET(_recording_status, IS_RECORDING);
    RT_SampleRecorder_start_recording(_recorder, start_time);
  }
  
  void stop_recording(void){
    RT_SampleRecorder_stop_recording(_recorder); // Fix: Make sure deleting plugin doesn't crash anything.
    _recorder = NULL; // It deletes itself when finished.
    ATOMIC_SET(_recording_status, NOT_RECORDING); // This line must be placed after "_recorder = NULL".
  }
  
public:
  
  bool RT_called_per_block(struct SeqTrack *seqtrack){
    R_ASSERT(PLAYER_current_thread_has_lock());
    
    bool more_to_play = false;
    
    int64_t start_time = seqtrack->start_time;
    int64_t end_time = seqtrack->end_time;

    switch(ATOMIC_GET(_recording_status)){
      
      case READY_TO_RECORD:
        {
          R_ASSERT_RETURN_IF_FALSE2(_recorder!=NULL, false);

          if (is_playing_song()) {
            const bool is_punching = ATOMIC_GET(root->song->punching.enabled);
            const int64_t punching_start = is_punching ? ATOMIC_GET(root->song->punching.start) : 0;
            
            if (is_punching==false || start_time >= punching_start) {
              
              const int64_t punching_end = is_punching ? ATOMIC_GET(root->song->punching.end) : 0;
              
              if (is_punching==false || start_time < punching_end){
                start_recording(start_time);
                more_to_play = true;
              }
            }
          }
          
          break;
        }

      case IS_RECORDING:
        {
          R_ASSERT_RETURN_IF_FALSE2(_recorder!=NULL, false);

          const bool is_punching = ATOMIC_GET(root->song->punching.enabled);
          const int64_t punching_end = is_punching ? ATOMIC_GET(root->song->punching.end) : 0;

          if (is_playing_song()==false || (is_punching && start_time >= punching_end))
            stop_recording();
          else
            more_to_play = true;
          
          break;
        }
        
      case STOP_RECORDING:
        stop_recording();
        break;
    }
    
    
    for(auto *sample : _samples){
      if (sample->RT_called_per_block(seqtrack, start_time, end_time)==true)
        more_to_play = true;
      assert_samples(seqtrack);
    }

    return more_to_play;
  }

  void called_very_often(SoundPlugin *plugin){
    R_ASSERT(THREADING_is_main_thread());
    
  again:
    for(auto *sample : _samples){
      if (ATOMIC_GET(sample->_state)==Sample::State::READY_FOR_DELETION){
        PLAYER_lock();{
          _samples.remove(sample);
        }PLAYER_unlock();
        assert_samples();
#if !defined(RELEASE)
        printf("    REMOVING Sample \"%S\". Reader: %p\n", sample->_filename_without_path, sample->_reader_holding_permanent_samples);
#endif
        delete sample;
        set_num_visible_outputs(plugin);

        goto again;
      }

      sample->interior_start_may_have_changed(); // light operation.
    }
  }

  void prepare_to_play(const struct SeqTrack *seqtrack, int64_t seqtime, radium::FutureSignalTrackingSemaphore *gotit){
    R_ASSERT(THREADING_is_main_thread());

    assert_samples(seqtrack);
    
    for(auto *sample : _samples){
      assert_samples(seqtrack);
      sample->prepare_to_play(seqtime, gotit);
    }
    
    assert_samples(seqtrack);
  }
};

} // end anon. namespace


void SEQTRACKPLUGIN_clear_resampler_warning_hashmap(void){
  g_resampler_warnings.clear();
}

static void set_num_visible_outputs(SoundPlugin *plugin){
  R_ASSERT(THREADING_is_main_thread());
  
  Data *data = (Data*)plugin->data;

  //int old_visible_channels = plugin->num_visible_outputs;
  
  int new_visible_channels = 2; // Need at least two visible output channels in case user has changed stereo.
  for(auto *sample : data->_samples)
    new_visible_channels = R_MAX(new_visible_channels, sample->_num_ch);

  plugin->num_visible_outputs = new_visible_channels;
}

static int64_t add_sample(Data *data, const wchar_t *filename, const struct SeqBlock *seqblock, enum Seqblock_Type type){
  R_ASSERT(THREADING_is_main_thread());
  
  radium::SampleReader *reader1 = SAMPLEREADER_create(filename);

  if(reader1==NULL){
    //abort();
    return -1;
  }

  //printf("     =========SAMPLE c/d: Alloced 3: %p\n", reader1);
  
  radium::SampleReader *reader2 = SAMPLEREADER_create(filename);

  if(reader2==NULL){
    //abort();
    SAMPLEREADER_delete(reader1);
    //printf("      =========SAMPLE c/d: Deleted 3: %p\n", reader1);
    return -1;
  }

  //printf("     =========SAMPLE c/d: Alloced 4: %p\n", reader2);
  
  Sample *sample = new Sample(filename, reader1, reader2, seqblock, type);

  switch(type){ 
  case Seqblock_Type::REGULAR:
    data->temporarily_suspend_sample_assertions_from_other_threads();
    data->_samples.push_back_in_realtime_safe_manner(sample);
    break;
  case Seqblock_Type::GFX:
    data->_gfx_samples.push_back(sample);
    break;
  case Seqblock_Type::GFX_GFX:
    data->_gfx_gfx_samples.push_back(sample);
    break;
  case Seqblock_Type::RECORDING:
    R_ASSERT(false);
    break;
  }

  return sample->_id;
}

int64_t SEQTRACKPLUGIN_add_sample(struct SeqTrack *seqtrack, SoundPlugin *plugin, const wchar_t *filename, const struct SeqBlock *seqblock, Seqblock_Type type){
  R_ASSERT(THREADING_is_main_thread());
  
  R_ASSERT_RETURN_IF_FALSE2(!strcmp(SEQTRACKPLUGIN_NAME, plugin->type->type_name), -1);

  Data *data = (Data*)plugin->data;

  if (type==Seqblock_Type::RECORDING)
    return INITIAL_RECORDER_ID;

  int64_t ret = add_sample(data, filename, seqblock, type);

  //printf("   ADD (is_gfx: %d). NUM samples: %d.  NUM gfx samples: %d\n", is_gfx, data->_samples.size(), data->_gfx_samples.size());

  if (type==Seqblock_Type::REGULAR)
    set_num_visible_outputs(plugin);
    
  return ret;
}

// Called when user enables the "R" checkbox.
void SEQTRACKPLUGIN_enable_recording(struct SeqTrack *seqtrack, SoundPlugin *plugin, const wchar_t *path){
  R_ASSERT(THREADING_is_main_thread());

  Data *data = (Data*)plugin->data;

  int status = ATOMIC_GET(data->_recording_status);

  if (status == NOT_RECORDING) {

    R_ASSERT_RETURN_IF_FALSE(data->_recorder == NULL);
    
    data->_recorder = new Recorder(seqtrack, path, get_seqtrack_recording_config(seqtrack));
    
    ATOMIC_SET(data->_recording_status, READY_TO_RECORD);
  }
}

static bool disable_running_recorder(Data *data){
  if (ATOMIC_COMPARE_AND_SET_INT(data->_recording_status, IS_RECORDING, STOP_RECORDING)){

    const int ms_to_wait = 5000; // 5 seconds
    const int ms_to_wait_per_call = 10;
    int ms = 0;
    
    while(ATOMIC_GET(data->_recording_status) != NOT_RECORDING && ms < ms_to_wait){
      msleep(ms_to_wait_per_call);
      ms += ms_to_wait_per_call;
    }
    
    R_ASSERT_NON_RELEASE(ATOMIC_GET(data->_recording_status)==NOT_RECORDING);

  } else {
    
    R_ASSERT_NON_RELEASE(false);
    
  }

  return false; // to avoid deadlock.
}



// Called when user disables the "R" checkbox.
// If it returns true, it means that we have to wait for SEQTRACK_call_me_when_recorder_is_finished.
bool SEQTRACKPLUGIN_disable_recording(struct SeqTrack *seqtrack, SoundPlugin *plugin){
  R_ASSERT(THREADING_is_main_thread());

  Data *data = (Data*)plugin->data;

  int status = ATOMIC_GET(data->_recording_status);

  if (status==NOT_RECORDING)
    return false;
  
  if (status == READY_TO_RECORD) {
    
    Recorder *recorder = NULL;

    {
      radium::PlayerLock lock;

      if (ATOMIC_COMPARE_AND_SET_INT(data->_recording_status, READY_TO_RECORD, NOT_RECORDING)){
        recorder = data->_recorder;        
        data->_recorder = NULL;
      }
    }

    if (recorder != NULL)
      delete recorder;
    else{
      R_ASSERT_NON_RELEASE(ATOMIC_GET(data->_recording_status)==IS_RECORDING);
      return disable_running_recorder(data); // Between the first and the second test whether data->_recording_status==READY_TO_RECORD, it switched status to IS_RECORDING.
    }

    return false;
    
  } else {

    return disable_running_recorder(data);

  }
}

void SEQTRACKPLUGIN_apply_gfx_samples(SoundPlugin *plugin){
  R_ASSERT(THREADING_is_main_thread());

  Data *data = (Data*)plugin->data;
  
  int bef = data->_samples.size();
  int bef_gfx = data->_gfx_samples.size();

  for(auto *sample : data->_samples){
    //data->assert_samples(); Commented out since it fails when creating seqtrack from state. (it seems like a useless place to assert anyway)
    if (ATOMIC_GET(sample->_state)==Sample::State::RUNNING)
      ATOMIC_SET(sample->_state, Sample::State::RT_REQUEST_DELETION);
  }

  if(data->_gfx_samples.size()==0){
    //R_ASSERT_NON_RELEASE(false); // Happens when redoing deletion of sample seqblock. (and perhaps other situations)
    return;
  }

  data->temporarily_suspend_sample_assertions_from_other_threads();

  {
    data->_samples.ensure_there_is_room_for_more_without_having_to_allocate_memory(data->_gfx_samples.size());
    
    {
      radium::PlayerLock lock; // Can not use 'seqtrack_is_live' as argument for radium::PlayerLock here since the plugin might be alive. I.e. data->_samples can be iterated even if the seqtrack isn't live yet.
      for(auto *sample : data->_gfx_samples){
        data->_samples.push_back(sample);
      }
    }
    
    data->_samples.post_add();

    data->_gfx_samples.clear();
  }

  set_num_visible_outputs(plugin);

  int aft = data->_samples.size();
  int aft_gfx = data->_gfx_samples.size();

  R_ASSERT(aft==bef+bef_gfx);
  R_ASSERT(aft_gfx==0);
  printf("   APPLY. NUM samples: %d.  NUM gfx samples: %d. Bef: %d / %d\n", aft, aft_gfx, bef, bef_gfx);
}

// For debugging only.
void SEQTRACKPLUGIN_assert_samples(const SoundPlugin *plugin){
#if !defined(RELEASE)
  Data *data = (Data*)plugin->data;
  data->assert_samples();
#endif
}
void SEQTRACKPLUGIN_assert_samples2(const struct SeqTrack *seqtrack){
#if !defined(RELEASE)
  if(seqtrack->patch==NULL)
    return;
  const SoundPlugin *plugin = (SoundPlugin*) seqtrack->patch->patchdata;        
  Data *data = (Data*)plugin->data;
  data->assert_samples(seqtrack);
#endif
}

static Sample *get_sample2(const radium::Vector<Sample*> &vec, int64_t id, bool must_be_present){
  for(Sample *sample : vec){
    if (sample->_id==id)
      return sample;
  }

  if(must_be_present)
    R_ASSERT(false);
  
  return NULL;
}

static Sample *get_sample(const SoundPlugin *plugin, int64_t id, bool search_non_gfx, bool search_gfx, bool search_gfx_gfx){
  R_ASSERT(THREADING_is_main_thread());
  
  Data *data = (Data*)plugin->data;

  Sample *sample = NULL;
  
  if (search_gfx)
    sample = get_sample2(data->_gfx_samples, id, false);

  if (sample==NULL && search_gfx_gfx)
    sample = get_sample2(data->_gfx_gfx_samples, id, false);
  
  if (sample==NULL && search_non_gfx)
    sample = get_sample2(data->_samples, id, false);

  if (sample==NULL)
    R_ASSERT(false);

  return sample;
}

void SEQTRACKPLUGIN_request_remove_sample(SoundPlugin *plugin, int64_t id, enum Seqblock_Type type){
  R_ASSERT(THREADING_is_main_thread());
  
  R_ASSERT_RETURN_IF_FALSE(!strcmp(SEQTRACKPLUGIN_NAME, plugin->type->type_name));
  
  Data *data = (Data*)plugin->data;

  data->temporarily_suspend_sample_assertions_from_other_threads();
  
  
  Sample *sample
    = type==Seqblock_Type::REGULAR ? get_sample(plugin, id, true, false, false)
    : type==Seqblock_Type::GFX ? get_sample(plugin, id, false, true, false)
    : get_sample(plugin, id, false, false, true);

  if(sample==NULL)
    return;

  if (ATOMIC_GET(sample->_state) != Sample::State::RUNNING){
    RError("Sample \"%S\" has already been requested removed from sequencer track. type: %d", sample->_filename, (int)type);
    return;
  }
    
  if (type == Seqblock_Type::REGULAR){

    ATOMIC_SET(sample->_state, Sample::State::RT_REQUEST_DELETION);
 
  } else {

    if(type==Seqblock_Type::GFX)
      data->_gfx_samples.remove(sample);
    else
      data->_gfx_gfx_samples.remove(sample);
    
    ATOMIC_SET_RELAXED(sample->_state, Sample::State::READY_FOR_DELETION);
    delete sample;

  }
}

// Called from AUDIO_is_permanent_patch. Returns false if there are any running samples.
bool SEQTRACKPLUGIN_can_be_deleted(SoundPlugin *plugin){
  R_ASSERT(THREADING_is_main_thread());
  
  Data *data = (Data*)plugin->data;

  for(auto *sample : data->_samples){
    if (ATOMIC_GET(sample->_state) == Sample::State::RUNNING)
      return false;
  }

  return true;
}

int SEQTRACKPLUGIN_get_num_samples(SoundPlugin *plugin){
  R_ASSERT(THREADING_is_main_thread());
  
  Data *data = (Data*)plugin->data;

  return data->_samples.size();
}

int SEQTRACKPLUGIN_get_num_channels(const SoundPlugin *plugin, int64_t id){
  R_ASSERT(THREADING_is_main_thread());
  
  R_ASSERT_RETURN_IF_FALSE2(!strcmp(SEQTRACKPLUGIN_NAME, plugin->type->type_name), -1);

  if (id <= HIGHEST_RECORDER_ID){
    Recorder *recorder = g_recorders[id];
    
    if (recorder==NULL){
      R_ASSERT_NON_RELEASE(false);
      return 1;
    }

    return recorder->num_ch;
  }
  
  Sample *sample = get_sample(plugin, id, true, true, true);
  if (sample==NULL)
    return -1;

  return sample->_num_ch;
}

int64_t SEQTRACKPLUGIN_get_total_num_frames_for_sample(const SoundPlugin *plugin, int64_t id){
  R_ASSERT_RETURN_IF_FALSE2(!strcmp(SEQTRACKPLUGIN_NAME, plugin->type->type_name), -1);
  
  Sample *sample = get_sample(plugin, id, true, true, true);
  if (sample==NULL)
    return -1;

  int64_t num_frames = sample->_total_num_frames_in_sample;
  
  bool do_resampling = sample->_do_resampling;

  if (!do_resampling)
    return num_frames;

  double resampler_ratio = sample->_resampler_ratio;
  
  return (double)num_frames * resampler_ratio;
  //return sample->_interior_end - sample->_interior_start;
}

/*
int64_t SEQTRACKPLUGIN_get_total_num_frames_in_sample(const SoundPlugin *plugin, int64_t id){
  R_ASSERT_RETURN_IF_FALSE2(!strcmp(SEQTRACKPLUGIN_NAME, plugin->type->type_name), -1);
  
  Sample *sample = get_sample(plugin, id, true, true, true);
  if (sample==NULL)
    return -1;

  return sample->_total_num_frames_in_sample;
  //return sample->_interior_end - sample->_interior_start;
}
*/

const wchar_t *SEQTRACKPLUGIN_get_sample_name(const SoundPlugin *plugin, int64_t id, bool full_path){
  R_ASSERT_RETURN_IF_FALSE2(!strcmp(SEQTRACKPLUGIN_NAME, plugin->type->type_name), L"");

  if (id == INITIAL_RECORDER_ID)
    return L"";

  if (id <= HIGHEST_RECORDER_ID){
    Recorder *recorder = g_recorders[id];
    
    if (recorder==NULL){
      R_ASSERT_NON_RELEASE(false);
      return L"";
    }

    if (recorder->_filename.is_empty()==false)
      return recorder->_filename.get();
    else
      return recorder->recording_path.get();    
  }

  Sample *sample = get_sample(plugin, id, true, true, true);
  if (sample==NULL)
    return L"";

#if !defined(RELEASE)
  if (full_path){
    QFileInfo info(STRING_get_qstring(sample->_filename));
    if (!info.isAbsolute())
      abort();
  }
#endif

  if (full_path)
    return sample->_filename;
  else
    return sample->_filename_without_path;
}

unsigned int SEQTRACKPLUGIN_get_sample_color(const SoundPlugin *plugin, int64_t id){
  if (id <= HIGHEST_RECORDER_ID)
    return 0x00ff0000;

  return SAMPLEREADER_get_sample_color(SEQTRACKPLUGIN_get_sample_name(plugin, id, true));
}
  
radium::Peaks **SEQTRACKPLUGIN_get_peaks(const SoundPlugin *plugin, int64_t id){
  R_ASSERT_RETURN_IF_FALSE2(!strcmp(SEQTRACKPLUGIN_NAME, plugin->type->type_name), NULL);

  if (id <= HIGHEST_RECORDER_ID){
    Recorder *recorder = g_recorders[id];
    
    if (recorder==NULL){
      R_ASSERT_NON_RELEASE(false);
      printf("SEQTRACKPLUGIN_get_peaks: recorder==NULL\n");
      return NULL;
    }

    return recorder->_peaks.get_array();
  }

  Sample *sample = get_sample(plugin, id, true, true, true);
  if (sample==NULL)
    return NULL;

  return sample->_peaks->_peaks;
}

double SEQTRACKPLUGIN_get_resampler_ratio(const SoundPlugin *plugin, int64_t id){
  R_ASSERT_RETURN_IF_FALSE2(!strcmp(SEQTRACKPLUGIN_NAME, plugin->type->type_name), 1.0);

  if (id <= HIGHEST_RECORDER_ID)
    return 1.0;
  
  Sample *sample = get_sample(plugin, id, true, true, true);
  if (sample==NULL)
    return 1.0;

  return sample->_resampler_ratio;
}

/*
void SEQTRACKPLUGIN_set_interior_start(struct SoundPlugin *plugin, int64_t id, int64_t interior_start){
  R_ASSERT_RETURN_IF_FALSE(!strcmp(SEQTRACKPLUGIN_NAME, plugin->type->type_name));
  
  Sample *sample = get_sample(plugin, id);
  if (sample==NULL)
    return;
  
  sample->set_interior_start(interior_start);
}

void SEQTRACKPLUGIN_set_interior_end(struct SoundPlugin *plugin, int64_t id, int64_t interior_end){
  R_ASSERT_RETURN_IF_FALSE(!strcmp(SEQTRACKPLUGIN_NAME, plugin->type->type_name));
  
  Sample *sample = get_sample(plugin, id);
  if (sample==NULL)
    return;
  
  sample->set_interior_end(interior_end);
}
*/

/*
void RT_HDSAMPLER_start_playing_sample(SoundPlugin *plugin, int64_t id){
  Sample *sample = get_sample(plugin, id);
  if (sample!=NULL)
    sample->start_playing();  
}

void RT_HDSAMPLER_stop_playing_sample(SoundPlugin *plugin, int64_t id){
  Sample *sample = get_sample(plugin, id);
  if (sample!=NULL)
    sample->stop_playing();  
}

void RT_HDSAMPLE_set_sample_playing(SoundPlugin *plugin, int64_t id, bool is_playing){
 Sample *sample = get_sample(plugin, id);
  if (sample!=NULL)
    sample->set_sample_playing(is_playing);  
}
*/

// returns true if there is more to play
bool RT_SEQTRACKPLUGIN_called_per_block(struct SoundPlugin *plugin, struct SeqTrack *seqtrack){
  Data *data = (Data*)plugin->data;

  return data->RT_called_per_block(seqtrack);
}

void SEQTRACKPLUGIN_prepare_to_play(SoundPlugin *plugin, const struct SeqTrack *seqtrack, int64_t seqtime, radium::FutureSignalTrackingSemaphore *gotit){
  Data *data = (Data*)plugin->data;

  data->prepare_to_play(seqtrack, seqtime, gotit);
}

vector_t SEQTRACKPLUGIN_get_all_used_audio_filenames(struct SoundPlugin *plugin){
  Data *data = (Data*)plugin->data;

  vector_t ret = {};
  
  for(Sample *sample : data->_samples)
    VECTOR_push_back(&ret, talloc_wcsdup(sample->_filename));

  return ret;
}

void SEQTRACKPLUGIN_called_very_often(SoundPlugin *plugin){
  Data *data = (Data*)plugin->data;

  data->called_very_often(plugin);

  if (is_playing_song()==false){
    QVector<Recorder*> to_remove;
    
    for(auto *recorder : g_successfully_finished_recorders){
      recorder->insert_audiofile();
      to_remove.push_back(recorder);
    }
    
    for(auto *recorder : to_remove){
      R_ASSERT(g_successfully_finished_recorders.removeAll(recorder)==1);
      delete recorder;
    }
  }
}

static void RT_record(Data *data, int num_frames, const float **instrument_inputs){
  R_ASSERT_RETURN_IF_FALSE(num_frames==RADIUM_BLOCKSIZE);

  
  Recorder *recorder = data->_recorder;
  const SeqtrackRecordingConfig &config = recorder->_config;

  bool empty_block_has_been_cleared = false;
  float empty_block[RADIUM_BLOCKSIZE];

  
  // Input buffer
  //
  float *inputs_[NUM_CHANNELS_RECORDING_MATRIX];
  float **inputs = inputs_;
  
  const int num_ch = NUM_CHANNELS_RECORDING_MATRIX;
  
  if (config.record_from_system_input)
    R_ASSERT(MIXER_get_main_inputs(const_cast<const float**>(inputs), num_ch)==num_ch);
  else
    memcpy(inputs, instrument_inputs, sizeof(float*)*num_ch);

  
  // Output buffer (i.e. audio sent to soundfile)
  //
  int num_used_output_buffers = 0;
  float output_buffer[NUM_CHANNELS_RECORDING_MATRIX*RADIUM_BLOCKSIZE];

  float *outputs_[NUM_CHANNELS_RECORDING_MATRIX];
  float **outputs = outputs_;


  // Fill output buffer
  //
  for (int output_ch = 0 ; output_ch < NUM_CHANNELS_RECORDING_MATRIX ; output_ch++){

    bool has_been_used = false;
    bool has_been_used_more_than_once = false;
    
    for(int input_ch=0;input_ch<NUM_CHANNELS_RECORDING_MATRIX;input_ch++) {
      
      if (config.matrix[input_ch][output_ch]==true) {

        if(has_been_used) {

          if (false==has_been_used_more_than_once){

            const float *old_output = outputs[output_ch];
            
            outputs[output_ch] = &output_buffer[num_used_output_buffers*RADIUM_BLOCKSIZE];
            num_used_output_buffers++;          

            for(int i=0 ; i < RADIUM_BLOCKSIZE ; i++)
              outputs[output_ch][i] = old_output[i] + inputs[input_ch][i];

            has_been_used_more_than_once = true;

          } else {
          
            for(int i=0 ; i < RADIUM_BLOCKSIZE ; i++)
              outputs[output_ch][i] += inputs[input_ch][i];

          }
          
        } else {
          
          outputs[output_ch] = inputs[input_ch];
          
          has_been_used = true;

          
        }
      }

    }

    if (false==has_been_used){
      
      if (false==empty_block_has_been_cleared){
        memset(empty_block, 0, sizeof(float)*RADIUM_BLOCKSIZE);
        empty_block_has_been_cleared = true;
      }
      
      outputs[output_ch] = empty_block;
          
    }
    
  }



  // Record
  //
  RT_SampleRecorder_add_audio(data->_recorder,
                              const_cast<const float**>(outputs),
                              RADIUM_BLOCKSIZE
                              );
}

static void RT_process(SoundPlugin *plugin, int64_t time, int num_frames, float **inputs, float **outputs){
  //SoundPluginType *type = plugin->type;
  Data *data = (Data*)plugin->data;

  if (ATOMIC_GET(data->_recording_status)==IS_RECORDING)
    RT_record(data, num_frames, const_cast<const float**>(inputs));
  
  // Null out channels
  for(int ch = 0 ; ch < NUM_OUTPUTS ; ch++)
    memset(outputs[ch], 0, num_frames*sizeof(float));

  //if (is_really_playing_song()==false)
  //  return;

  //printf("Num samples: %d\n", data->_samples.size());

  // Read samples
  for(Sample *sample : data->_samples)
    sample->RT_process(num_frames, outputs);
}

static void RT_player_is_stopped(struct SoundPlugin *plugin){
  //printf("   RT_player_is_stopped called %f\n", TIME_get_ms() / 1000.0);
  
  Data *data = (Data*)plugin->data;

  for(Sample *sample : data->_samples)
    sample->RT_stop_playing(true);
}

static void set_effect_value(struct SoundPlugin *plugin, int time, int effect_num, float value, enum ValueFormat value_format, FX_when when){
  //Data *data = (Data*)plugin->data;
  printf("####################################################### Setting sine volume to %f\n",value);
  //data->volume = value;
}

static float get_effect_value(struct SoundPlugin *plugin, int effect_num, enum ValueFormat value_format){
  //Data *data = (Data*)plugin->data;
  return 0;//data->volume;
}

static void get_display_value_string(SoundPlugin *plugin, int effect_num, char *buffer, int buffersize){
  //Data *data = (Data*)plugin->data;
  snprintf(buffer,buffersize-1,"%f",0.1);//data->volume);
}

static void *create_plugin_data(const SoundPluginType *plugin_type, SoundPlugin *plugin, hash_t *state, float sample_rate, int block_size, bool is_loading){
  Data *data = new Data(sample_rate);
  printf("####################################################### Setting sine volume to 0.5f (create_plugin_data)\n");

  //add_sample(L"/home/kjetil/radium/bin/sounds/bbs2.wav");
  return data;
}

static void cleanup_plugin_data(SoundPlugin *plugin){
  Data *data = (Data*)plugin->data;
  printf(">>>>>>>>>>>>>> Cleanup_plugin_data called for %p\n",plugin);
  delete data;
}

static const char *get_effect_name(struct SoundPlugin *plugin, int effect_num){
  return "Volume";
}


void create_seqtrack_plugin(void){
  SoundPluginType *plugin_type = (SoundPluginType*)V_calloc(1,sizeof(SoundPluginType));

  plugin_type->type_name                = SEQTRACKPLUGIN_NAME;
  plugin_type->name                     = SEQTRACKPLUGIN_NAME;
  plugin_type->num_inputs               = NUM_INPUTS;
  plugin_type->num_outputs              = NUM_OUTPUTS;
  plugin_type->is_instrument            = false;
  plugin_type->note_handling_is_RT      = false;
  plugin_type->num_effects              = EFF_NUM_EFFECTS,
  plugin_type->get_effect_format        = NULL;
  plugin_type->get_effect_name          = get_effect_name;
  plugin_type->effect_is_RT             = NULL;
  plugin_type->create_plugin_data       = create_plugin_data;
  plugin_type->cleanup_plugin_data      = cleanup_plugin_data;
  
  plugin_type->RT_process       = RT_process;
  plugin_type->RT_player_is_stopped = RT_player_is_stopped;
  
  plugin_type->set_effect_value = set_effect_value;
  plugin_type->get_effect_value = get_effect_value;
  plugin_type->get_display_value_string = get_display_value_string;

  plugin_type->will_never_autosuspend = true; // TODO: Touch plugin manually instead.
  
  PR_add_plugin_type_no_menu(plugin_type);
  //PR_add_plugin_type(plugin_type);
}
