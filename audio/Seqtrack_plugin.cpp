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
#include "../common/disk.h"
#include "../common/SeqAutomation.hpp"
#include "../common/Array.hpp"
#include "../common/undo_sequencer_proc.h"
#include "../common/seqblock_automation_proc.h"

#include "SoundPlugin.h"
#include "SoundPlugin_proc.h"
#include "SoundProducer_proc.h"
#include "Peaks.hpp"
#include "SampleReader_proc.h"
#include "GranResampler.hpp"
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

#define MAX_GRAIN_LENGTH_IN_SECONDS 1.0
#define MAX_GRAIN_FREQUENCY_IN_SECONDS 2.0
#define MAX_OVERLAP 50

//#define HOW_MUCH_NONSTRETCHED_TO_PREPARE (0.05 * 0.015
//#define HOW_MUCH_NONSTRETCHED_TO_PREPARE_BEFORE_STARTING 0.2

#define HOW_MUCH_TO_PREPARE 2 // in seconds. TODO: We need to slowly increase the buffer from 0.1 to 2. TODO2: Make these two numbers configurable.
#define HOW_MUCH_TO_PREPARE_BEFORE_STARTING 2 //in seconds

#define RESAMPLER_BUFFER_SIZE 64 //(RADIUM_BLOCKSIZE*2) //1024
// TODO: print out how often data is picked up, and so forth. It probably picks up more when stretch is high.

/*
namespace radium{
  template <typename T>
  struct GcHolder2{

  private:
    T *t;

    GcHolder2(const GcHolder2&) = delete;
    GcHolder2& operator=(const GcHolder2&) = delete;

  public:
    GcHolder2(T *t = NULL)
      : t(t)
    {  
    }
    ~GcHolder2(){
    }
    T *operator->() const {
      return t;
    }
    void set(T *new_t){
    }
    T *data(void) const {
      return t;
    }
  };
}
*/

// This is the instrument that is bounded to ONE track in the sequencer.

/*
  Possible to change pitch while playing? (Hard to calculate sample seek then)
  Possible to change stretch while playing? Also makes it hard to calculate sample seek.
 */

enum{
  EFF_ENABLE_PIPING = 0,
  EFF_NUM_EFFECTS
};

enum{
  //EFF_NUM_VISIBLE_PORTS = 0,
  EFF_NUM_BUS_EFFECTS
};

//#define MIN_NUM_VISIBLE_PORTS 1
//#define MAX_NUM_VISIBLE_PORTS 8


static SoundPluginType bus_type1 = {};
static SoundPluginType bus_type2 = {};
static SoundPluginType bus_type3 = {};
static SoundPluginType bus_type4 = {};
static SoundPluginType bus_type5 = {};

static SoundPluginType seqtrack_type_general = {};
static SoundPluginType bus_type_general = {};


static void set_num_visible_outputs(SoundPlugin *plugin);

static QSet<QString> g_resampler_warnings;

#define INITIAL_RECORDER_ID 0

namespace{

static int64_t g_id = INITIAL_RECORDER_ID+1;

struct MyReader : radium::AudioPickuper {
  
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

  radium::SampleReaderCallback _sample_reader_callback;
  radium::Granulator _granulator;
  radium::Resampler2 _resampler2;
  radium::GranResampler *_granresampler;
  
  bool _do_granulate = false;

  bool _do_constant_resampling = false;
  bool _do_resampling = false;
  double _resampler_ratio = 1.0;
  double _curr_automation_resampler_ratio = 1.0;

  MyReader(radium::SampleReader *reader, enum ResamplerType resampler_type)
    : _reader(reader)
    , _num_ch(SAMPLEREADER_get_num_channels(reader))

    , _sample_reader_callback(reader)
      //, _granulator(_num_ch, MAX_GRAIN_LENGTH_IN_SECONDS, MAX_GRAIN_FREQUENCY_IN_SECONDS, NULL)
    , _granulator(MAX_GRAIN_LENGTH_IN_SECONDS * pc->pfreq, MAX_GRAIN_FREQUENCY_IN_SECONDS * pc->pfreq, MAX_OVERLAP, _num_ch, this)
    , _resampler2(_num_ch, resampler_type, NULL)
  {
    
    SMOOTH_init_immediate_smoothing(&_volume, 0.0f, RADIUM_BLOCKSIZE);

    double samplerate = SAMPLEREADER_get_samplerate(reader);

    if (fabs(samplerate-pc->pfreq) > 1){

      filepath_t filename = SAMPLEREADER_get_filename(reader);
      QString qfilename = STRING_get_qstring(filename.id);

      if (g_resampler_warnings.contains(qfilename)==false){
        GFX_addMessage("Warning: \"%S\" has a samplerate of %d, while radium runs at %d\n. To compensate the difference, Radium will perform high quality samplerate conversion during runtime, which will use a bit CPU.",
                       SAMPLEREADER_get_sample_name(reader).id,
                       (int)samplerate,
                       (int)pc->pfreq);                     
        g_resampler_warnings.insert(qfilename);
      }

      _resampler_ratio = (double)pc->pfreq / samplerate;
      _do_constant_resampling = true;

    }
  }
    

  virtual ~MyReader(){
    {      
      SAMPLEREADER_delete(_reader);
      //printf("     =========SAMPLE c/d: Deleted 1: %p\n", _reader);
      _reader = NULL;
    }

    SMOOTH_release(&_volume);
  }

private:

  int pick_up_data_for_granulator(float **samples, int max_num_frames) override {
    return RT_SAMPLEREADER_read(_reader, samples, max_num_frames);
    //RT_get_samples_from_disk(samples, max_num_frames);
    //return max_num_frames;
  }
  
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
  
public:

  void set_final_resampler_ratio(double curr_automation_ratio, double seqblock_speed) {
    _resampler2._ratio = _resampler_ratio * curr_automation_ratio * seqblock_speed;
  }
  
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
    if (_granresampler != NULL) {
      
      R_ASSERT_NON_RELEASE(_do_granulate || _do_resampling);
      
      _granresampler->GranResampler::RT_process(do_add ? temp_buffers : outputs, num_frames);
      
    } else {

      R_ASSERT_NON_RELEASE(_do_granulate==false && _do_resampling==false);
            
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

    //SMOOTH_print("volume: ", &_volume); printf("_num_ch: %d. num_frames: %d\n", _num_ch, num_frames);
    
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
      float *output1 = outputs[1];
      
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
  
  int _total_num_frames2 = 0; // for debugging

  void RT_add_samples(int num_frames, float **outputs){
    //printf(" *** total: %d\n", _total_num_frames2);
    _total_num_frames2 += num_frames;

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
    //double stretch = mus_increment(_granulator._clm_granulators[0]);
    double stretch = _granulator.get_stretch();
    return (_fadeout_countdown / stretch);
  }


  void prepare_for_playing(const int64_t sample_start_pos, const struct SeqBlock *seqblock, bool do_fade_in, radium::FutureSignalTrackingSemaphore *gotit){
    _do_granulate = RT_seqblock_automation_is_enabled(seqblock->automations[SAT_STRETCH]) || seqblock_is_stretched(seqblock);
    _do_resampling = _do_constant_resampling || seqblock_is_speeded(seqblock) || RT_seqblock_automation_is_enabled(seqblock->automations[SAT_SPEED]);

    _granulator.reset();
    _resampler2.reset();

    if (_do_granulate && _do_resampling){

      // Run resampler before granulator since the resampler buffers less data (hence geting less inaccurate automation),
      // plus that the granulator uses less CPU than the resampler (at least when using SINC), so we also get a more stable CPU usage.
      
      _granresampler = &_resampler2;
      _resampler2.set_callback(&_granulator);
      _granulator.set_callback(&_sample_reader_callback);
      
    } else if (_do_granulate){
      
      _granresampler = &_granulator;
      _granulator.set_callback(&_sample_reader_callback);
      
    } else if (_do_resampling){

      _granresampler = &_resampler2;
      _resampler2.set_callback(&_sample_reader_callback);
      
    } else {

      _granresampler = NULL;
      
    }
    

    _has_set_volume = false; // To avoid smoothly gliding from previous volume (the last time we played) to the current.
      
    if (do_fade_in==false){
      //printf("No fadein\n");
      _curr_fade_volume = 1.0;
    }else
      prepare_for_fadein();

    // TODO: This number should be dynamically calculated somehow depending on how long time it takes to read from disk.
    int64_t how_much_to_prepare = double(HOW_MUCH_TO_PREPARE_BEFORE_STARTING) * (double)pc->pfreq / (seqblock->t.stretch * seqblock->t.speed * _resampler_ratio);

    how_much_to_prepare = R_MAX(how_much_to_prepare,
                                2 * pc->pfreq * (MAX_GRAIN_LENGTH_IN_SECONDS + MAX_GRAIN_FREQUENCY_IN_SECONDS)
                                );
    
    //for(int ch=0;ch<_num_ch;ch++)
    //how_much_to_prepare = R_MAX(how_much_to_prepare,
    //                              2 * mus_granulate_grain_max_length(_granulator._clm_granulators[ch]));
                                        

    {
      LOCKASSERTER_EXCLUSIVE(&lockAsserter);
      
      SAMPLEREADER_prepare_to_play(_reader, sample_start_pos, how_much_to_prepare, gotit);
    }

    //printf("      ====   %p has now been prepared\n", this);
  }
  
  bool is_finished(void){
    return _fadeout_countdown == 0;
  }
};


struct Sample{
  int64_t _id = g_id++;
  radium::FilePath _filename;

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

  enum ResamplerType _resampler_type;
    
  radium::GcHolder<const struct SeqBlock> _seqblock; // Don't think this causes memory leak. Sample instances are stored in the audio instrument, not in the seqblock.
  radium::GcHolder<const struct SeqTrack> _seqtrack;
  Seqblock_Type _type;
  
  float _pitch = 1.0;

  bool _grain_strict_no_jitter = false;
  double _grain_overlap = 3.6;
  double _grain_length = 300; // in ms
  double _grain_jitter = 1.0;
  double _grain_ramp = 0.33;
  
  DEFINE_ATOMIC(bool, _has_updates) = true;
  
  const int _num_ch;
  const int64_t _total_num_frames_in_sample;
  bool _do_resampling;
  double _resampler_ratio;

  const unsigned int _color;
  radium::FilePath _filename_without_path;
  
  bool _is_playing = false; // If false, we are not playing current. But fade-outs can play even if _is_playing==false.
  int _click_avoidance_fade_in = 0; // frames left while fading in.

  bool _do_looping = false;

#if !defined(RELEASE)
  radium::LockAsserter lockAsserter;
#endif
  
  Sample(filepath_t filename, radium::SampleReader *reader1, radium::SampleReader *reader2, enum ResamplerType resampler_type, const struct SeqBlock *seqblock, const struct SeqTrack *seqtrack, Seqblock_Type type)
    : _filename(filename)
      //, _reader(NULL) //new MyReader(reader))
      //, _fade_out_reader(new MyReader(fade_out_reader))
    , _reader_holding_permanent_samples(SAMPLEREADER_create(filename))
    , _peaks(DISKPEAKS_get(filename))
    , _resampler_type(resampler_type)
    , _seqblock(seqblock)
    , _seqtrack(seqtrack)
    , _type(type)
    , _num_ch(SAMPLEREADER_get_num_channels(reader1))
    , _total_num_frames_in_sample(SAMPLEREADER_get_total_num_frames_in_sample(reader1))
    , _color(SAMPLEREADER_get_sample_color(filename))
    , _filename_without_path(SAMPLEREADER_get_sample_name(reader1))
  {
    //printf("     =========SAMPLE c/d: Alloced 1: %p\n", _reader_holding_permanent_samples);
    
    R_ASSERT(THREADING_is_main_thread());

#if !defined(RELEASE)
    LOCKASSERTER_EXCLUSIVE(&lockAsserter);
    
    QFileInfo info(STRING_get_qstring(_filename.getString()));
    if (!info.isAbsolute())
      abort();
#endif


    // prepare readers.
    {
      _free_readers.push_back(new MyReader(reader1, _resampler_type));
      _free_readers.push_back(new MyReader(reader2, _resampler_type));

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

#if !defined(RELEASE)
    LOCKASSERTER_EXCLUSIVE(&lockAsserter);
#endif
    
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
  }

  void interior_start_may_have_changed(void){
    double start = _seqblock->t.interior_start;
    double end = _seqblock->t.interior_start + ((double)HOW_MUCH_TO_PREPARE_BEFORE_STARTING * (double)pc->pfreq / (_seqblock->t.stretch*_seqblock->t.speed));
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

#if !defined(RELEASE)
      LOCKASSERTER_EXCLUSIVE(&_curr_reader->lockAsserter);
#endif
      
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

    int64_t sample_start_pos1;

    int64_t p_interior_start = _seqblock->t.interior_start / _resampler_ratio;
    int64_t p_interior_end = _seqblock->t.interior_end / _resampler_ratio;

    if(seqtime <= _seqblock->t.time)
      sample_start_pos1 = p_interior_start;
    else
      sample_start_pos1 = scale_int64(seqtime,
                                      _seqblock->t.time, _seqblock->t.time2,
                                      p_interior_start, p_interior_end);
    //printf("sample_start_pos: %d. _interior_start: %d. _seqblock->t.time: %d. %d vs. %d\n",int(sample_start_pos), int(_seqblock->t.interior_start), int(_seqblock->t.time), int(_seqblock->t.time2-_seqblock->t.time), int(_seqblock->t.interior_end-_seqblock->t.interior_start));
    //sample_start_pos = _interior_start + seqtime - _seqblock->t.time;
    
    if (sample_start_pos1 < 0){
      R_ASSERT(false);
      sample_start_pos1 = 0;
    }
    
    if (sample_start_pos1 >= p_interior_end){
      R_ASSERT(false);
      return;
    }

    int64_t sample_start_pos = get_stretch_automation_sample_pos(_seqblock.data(), sample_start_pos1);

    /*
    printf("sample_start_pos1: %d. interior start: %f / %d. end: %f / %d. _resampler_ratio: %f\n", (int)sample_start_pos1,
           (double)_seqblock->t.interior_start / pc->pfreq, (int)p_interior_start,
           (double)_seqblock->t.interior_end / pc->pfreq, (int)p_interior_end,
           _resampler_ratio);
    */

    if (sample_start_pos < 0){
      R_ASSERT(false);
      return;
    }
    
    if (sample_start_pos >= _seqblock->t.num_samples){
      R_ASSERT(false);
      return;
    }


    //printf("  PREPARE TO PLAY. Time: %f. sample_start_pos: %f\n", (double)seqtime / (double)pc->pfreq, (double)sample_start_pos / (double)pc->pfreq);
    
    MyReader *allocated_reader;

    {
      radium::SampleReader *samplereader = SAMPLEREADER_create(_filename.get()); // light operation
      if (samplereader==NULL)
        return;
      //printf("     =========SAMPLE c/d: Alloced 2: %p\n", samplereader);
      allocated_reader = new MyReader(samplereader, _resampler_type); // light operation
    }
    
    MyReader *reader;

    
    bool num_free_readers;

    bool i_am_playing = I_am_playing();
    
    {
      radium::PlayerLock lock; // Must lock even if playing since RT_process (which sometimes pushes to _free_readers) is called when not playing.

#if !defined(RELEASE)
      LOCKASSERTER_EXCLUSIVE(&lockAsserter);
#endif
      
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

    bool do_fade_in = sample_start_pos > p_interior_start;
    //printf("do_fade_in: %d. %d > %d. Sample_start_pos: %f\n", do_fade_in, (int)sample_start_pos, (int)p_interior_start, (double)sample_start_pos/pc->pfreq);
    reader->prepare_for_playing(sample_start_pos, _seqblock.data(), do_fade_in, gotit);

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
#if !defined(RELEASE)
    LOCKASSERTER_EXCLUSIVE(&lockAsserter);
#endif
    
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
#if !defined(RELEASE)
      LOCKASSERTER_EXCLUSIVE(&_curr_reader->lockAsserter);
#endif
      if (true || ATOMIC_COMPARE_AND_SET_BOOL(_has_updates, true, false)){ // TODO: Fix the _has_updates thing.
        
        double grain_overlap = _grain_overlap;
        
        double grain_length = _grain_length;
        
        double grain_jitter = _grain_jitter;
        double grain_ramp = _grain_ramp;
                
        double stretch = _seqblock->t.stretch;
        double automation_resampler_ratio = 1.0;
        
        {
          const int64_t start_time = _seqtrack->start_time;
          const int64_t end_time = _seqtrack->end_time;
          if (end_time >= _seqblock->t.time && start_time <= _seqblock->t.time2){

            const double speed = _seqblock->t.speed;
            const double stretchspeed = stretch*speed;

            double s1 = get_seqblock_noninterior_start(_seqblock.data());
            double time = (start_time-s1) / stretchspeed;

            RT_maybe_get_seqblock_automation_value(_seqblock->automations[SAT_GRAIN_OVERLAP], time, grain_overlap);
            RT_maybe_get_seqblock_automation_value(_seqblock->automations[SAT_GRAIN_LENGTH], time, grain_length);
            RT_maybe_get_seqblock_automation_value(_seqblock->automations[SAT_GRAIN_JITTER], time, grain_jitter);
            RT_maybe_get_seqblock_automation_value(_seqblock->automations[SAT_GRAIN_RAMP], time, grain_ramp);

            double automation_stretch;
            if (RT_maybe_get_seqblock_automation_value(_seqblock->automations[SAT_STRETCH], time, automation_stretch))
              stretch *= automation_stretch;

            RT_maybe_get_seqblock_automation_value(_seqblock->automations[SAT_SPEED], time, automation_resampler_ratio);
          }
        }
        
        grain_length = ms_to_frames(grain_length);

        //grain_ramp *= grain_length;

        //printf("length: %f (%f). frequency: %f. jitter: %f. ramp: %f. Stretch: %f\n", grain_length, grain_length*1000.0/pc->pfreq, grain_frequency, grain_jitter, grain_ramp, stretch);

        if (grain_length > MAX_GRAIN_LENGTH_IN_SECONDS*pc->pfreq){
#if !defined(RELEASE)
          printf("    RT: Illegal grain length: %f > %f\n", grain_length, MAX_GRAIN_LENGTH_IN_SECONDS*pc->pfreq);
#endif
          grain_length = MAX_GRAIN_LENGTH_IN_SECONDS*pc->pfreq;
        }

#if 0
        double grain_frequency = (grain_length / grain_overlap);
        //printf("   freq: %f. len: %f. overlap: %f\n", grain_frequency, grain_length, grain_overlap);

        if (grain_frequency > MAX_GRAIN_FREQUENCY_IN_SECONDS*pc->pfreq){
#if !defined(RELEASE)
          //  printf("    RT: Illegal grain frequency: %f > %f\n", grain_frequency, MAX_GRAIN_FREQUENCY_IN_SECONDS*pc->pfreq);
#endif
          grain_frequency = MAX_GRAIN_FREQUENCY_IN_SECONDS*pc->pfreq;
        }
#endif
        
        _curr_reader->_granulator.set_strict_no_jitter(_grain_strict_no_jitter);
        _curr_reader->_granulator.set_overlap(grain_overlap);
        _curr_reader->_granulator.set_stretch(stretch);
        _curr_reader->_granulator.set_jitter(grain_jitter);
        _curr_reader->_granulator.set_ramp(grain_ramp);
        _curr_reader->_granulator.set_grain_length(grain_length);
        
        
        /*
        for(int ch=0;ch<_num_ch;ch++){
          mus_set_hop(_curr_reader->_granulator._clm_granulators[ch], grain_frequency);
          mus_set_length(_curr_reader->_granulator._clm_granulators[ch], grain_length);
          mus_set_offset(_curr_reader->_granulator._clm_granulators[ch], grain_jitter);
          mus_set_ramp(_curr_reader->_granulator._clm_granulators[ch], grain_ramp);
          mus_set_increment(_curr_reader->_granulator._clm_granulators[ch], stretch);
        }
        */
        
        _curr_reader->set_final_resampler_ratio(automation_resampler_ratio, _seqblock->t.speed);
          
        /*
        double ramp = 0.4;
        double grain_frequency2 = grain_frequency*1000;
        double grain_length2 = grain_length * 1000.0 / (double)pc->pfreq;
        grain_length2 = grain_length2 * (1.0 - ramp);
        
        double grain_often = grain_frequency2 / grain_length2;
        _grain_volume_compensation = R_MIN(pow(grain_often, 0.3), 1.0);
        //printf("   Grain volume compensation: %f (frequency: %f / Length: %f). grain_often: %f.\n", _grain_volume_compensation, grain_frequency2, grain_length2, grain_often);
        */
        
      }
      
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
      int64_t how_much_to_prepare = double(HOW_MUCH_TO_PREPARE) * (double)pc->pfreq / (_seqblock->t.stretch*_seqblock->t.speed);

      how_much_to_prepare = R_MAX(how_much_to_prepare,
                                  2 * pc->pfreq * (MAX_GRAIN_LENGTH_IN_SECONDS + MAX_GRAIN_FREQUENCY_IN_SECONDS)
                                  );
      
      RT_SAMPLEREADER_called_per_block(_curr_reader->_reader, how_much_to_prepare);
    }
  }

  // Called after scheduler and before audio processing.
  bool RT_called_per_block(struct SeqTrack *seqtrack, int64_t curr_start_time, int64_t curr_end_time){ // (end_time-start_time is usually RADIUM_BLOCKSIZE.)

#if !defined(RELEASE)
    if (is_really_playing_song()==false){
      //_is_playing = false;
      R_ASSERT(false);
      return false;
    }
#endif

    if (ATOMIC_GET(_state)!=Sample::State::RUNNING)
      return false;

#if !defined(RELEASE)
    // Must place this line after the is_really_playing_song() check. If not, we sometimes (quite rarely) gets lockAsserter hit here and in prepare_to_play.
    LOCKASSERTER_EXCLUSIVE(&lockAsserter);
#endif
    
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
          atomic_pointer_write_relaxed((void**)&seqtrack->curr_sample_seqblock, NULL); // For rendering name of sample in editor when this seqtrack is current seqtrack.
          GFX_ScheduleEditorRedraw();
        }

        //printf("Calling RT_stop_playing\n");
        RT_stop_playing(false);
      }
    }

    return _is_playing || _seqblock->t.time > curr_start_time;
  }

  void convert_old_granular_parameters(struct SeqBlock *seqblock){

    bool show_warning = false;
    
    // 1. Reduce gain if granulating
    {
      if (RT_seqblock_automation_is_enabled(seqblock->automations[SAT_STRETCH]) || seqblock_is_stretched(seqblock)){
        seqblock->gain *= 0.6; // Not quite the same though. This value is applied after granulation, while 0.6 in the granulator in clm applied this to the raw samples used to granulate.
        show_warning = true;
      }
    }
    
    
    // 2. Adjust overlap when grain length is over 100ms.
    {
      if (_grain_length > 100) {
      
        double old_grain_length = _grain_length;
        
        _grain_length = 100.0;
        
        _grain_overlap = R_MIN(50, _grain_overlap / (old_grain_length-100) / 100.0);

        show_warning = true;
      }
    }

    // 3. The clm granulator only has strict no jitter.
    {
      safe_bool_write(&_grain_strict_no_jitter, true);
    }

    // 4. Maybe show warning
    {
      if (show_warning && dc.has_warned_about_different_granulator==false){
        evalScheme("(ra:schedule 100 (lambda () (ra:add-message "
                   "\"This song was created with an older version of Radium that used a different granulator. Beware that the new granulator might not sound exactly the same. In addition, overall volume can be sligthy different, and automation of grain length will sound different when grain length is > 100ms.\""
                   ") #f))");
        dc.has_warned_about_different_granulator = true;
      }
    }
  }
  
};

 
enum{
 NOT_RECORDING = 0,
 READY_TO_RECORD,
 IS_RECORDING,
 REQUEST_STOP_RECORDING
};

#define HIGHEST_RECORDER_ID -2
static int64_t g_recorder_id = HIGHEST_RECORDER_ID;

struct Recorder;
static QHash<int64_t, Recorder*> g_recorders;
static QVector<Recorder*> g_successfully_finished_recorders;
 
struct Recorder : public radium::SampleRecorderInstance{

  const int64_t _id = g_recorder_id--;
  
  radium::Peaks *_peaks;

  //Data *_data;
  
  radium::GcHolder<struct SeqBlock> _seqblock;
  radium::GcHolder<struct SeqTrack> _seqtrack;

  const SeqtrackRecordingConfig _config;
  
  radium::FilePath _filename;
  
  int _seqtrack_recording_generation;
  
  Recorder(struct SeqTrack *seqtrack, filepath_t recording_path, const SeqtrackRecordingConfig *config, int64_t latency)
    : SampleRecorderInstance(recording_path, get_num_recording_soundfile_channels(config), 48, latency) // 60 is standard value, I think, but the sample recorder adds 12 to the middle note, for some reason.
    , _peaks(new radium::Peaks[num_ch])
      //, _data(data)
    , _seqtrack(seqtrack)
    , _config(*config)
    , _seqtrack_recording_generation(++seqtrack->recording_generation)
  {
    printf("-------------------------     recorder CONSTRUCTOR %d (%p)\n", (int)_seqtrack_recording_generation, this);
    R_ASSERT(THREADING_is_main_thread());
    R_ASSERT(seqtrack->for_audiofiles);

    /*
    for(int ch=0;ch<num_ch;ch++)
      _peaks.set(ch, new radium::Peaks);
    */
    
    g_recorders[_id] = this;
  }

  ~Recorder(){
    R_ASSERT(THREADING_is_main_thread());
    
    printf("-------------------------     recorder DESTRUCTOR %d (%p)\n", (int)_seqtrack_recording_generation, this);
    g_recorders.remove(_id);

    /*
    for(int ch=0;ch<num_ch;ch++)
      delete _peaks[ch];
    */
    delete[] _peaks;
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
 
  void is_finished(bool success, filepath_t filename) override {
    struct SeqTrack *seqtrack = _seqtrack.data();
    
    printf("-------------------------        recorder IS_FINISHED start %d / %d  (%p)\n", (int)seqtrack->recording_generation, (int)_seqtrack_recording_generation, this);

    int seqtracknum = get_seqtracknum(seqtrack);
    if (seqtracknum < 0){
      printf(" Recorder::is_finished: Seqtrack not in song. Deleted?\n");
      
      //R_ASSERT_NON_RELEASE(false); // Happens if deleting seqtrack while recording.
      
      return;
    }

    if(_seqtrack_recording_generation==seqtrack->recording_generation){
      SEQTRACK_set_recording(seqtrack, false);
    }

    if (_has_started==false){
      R_ASSERT(_seqblock.data()==NULL);
      R_ASSERT_NON_RELEASE(success==false);
    }

    printf("-------------------------        recorder IS_FINISHED end   %d / %d  (%p). Success: %d. Filename: %S\n", (int)seqtrack->recording_generation, (int)_seqtrack_recording_generation, this, success, filename.id);

    if (success && _has_started) {

      QFileInfo info(STRING_get_qstring(filename.id));
      _seqblock->sample_filename_without_path = make_filepath(info.fileName());
      
      _filename = radium::FilePath(filename);
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
      _seqblock->sample_filename_without_path = make_filepath(L"Recording...");
      SEQTRACK_update(_seqtrack.data());
    }
  }

  bool _has_started = false;
  
  
public:
  
  void add_recorded_peak(int ch, float min_peak, float max_peak) override {

    _peaks[ch].add(radium::Peak(min_peak, max_peak));

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

  Smooth _piping_volume;
  bool _enable_piping = false;

  //bool _is_bus;
  
#if !defined(RELEASE)
  
private:
  
  bool wait_for_main_thread_before_asserting_samples = false;
  radium::Mutex assert_samples_mutex; // Need lock (i.e. atomic variable is not enough) since all of assert_samples needs to be protected, not just access to wait_for_main_thread_before_asserting_samples.
  
public:
  
  struct SeqTrack *get_seqtrack(void) const {
    VECTOR_FOR_EACH(struct SeqTrack *, seqtrack, &root->song->seqtracks){
      if (seqtrack->patch != NULL){
        SoundPlugin *plugin = (SoundPlugin*) seqtrack->patch->patchdata;
        if (plugin==NULL){
          R_ASSERT(false);
        } else {
          if(plugin->data==this)
            return seqtrack;
        }
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
          fprintf(stderr,"NUM: %d (%d)\n", num, num != 1);
          abort();
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
  
  void assert_samples(const struct SeqTrack *seqtrack = NULL) const {
  }

  void temporarily_suspend_sample_assertions_from_other_threads(void) const {
  }
  
#endif


  Data(float sample_rate, bool enable_piping)
    : _sample_rate(sample_rate)
    , _enable_piping(enable_piping)
  {
    R_ASSERT(THREADING_is_main_thread());
    
    SMOOTH_init(&_piping_volume, enable_piping ? 1.0 : 0.0, MIXER_get_buffer_size());
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

    SMOOTH_release(&_piping_volume);
  }


private:

  void RT_start_recording(int64_t start_time){
    ATOMIC_SET(_recording_status, IS_RECORDING);
    RT_SampleRecorder_start_recording(_recorder, start_time);
  }
  
  void RT_request_stop_recording(void){
    RT_SampleRecorder_request_stop_recording(_recorder);
  }
  

public:

  void RT_recording_has_stopped(void){
    _recorder = NULL; // It deletes itself when finished.
    ATOMIC_SET(_recording_status, NOT_RECORDING); // This line must be placed after "_recorder = NULL".
  }
  
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
                RT_start_recording(start_time);
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
            RT_request_stop_recording();
          
          more_to_play = true;          
          break;
        }
        
      case REQUEST_STOP_RECORDING:
        RT_request_stop_recording();
        more_to_play = true;
        break;
    }
    
    if (is_really_playing_song()){
      
      for(auto *sample : _samples){
        
        if (sample->RT_called_per_block(seqtrack, start_time, end_time)==true)
          more_to_play = true;
        
        assert_samples(seqtrack);
      }
      
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
        printf("    REMOVING Sample \"%S\". Reader: %p\n", sample->_filename_without_path.getString(), sample->_reader_holding_permanent_samples);
#endif
        delete sample;
        set_num_visible_outputs(plugin);

        goto again;
      }

      sample->interior_start_may_have_changed(); // light operation.
    }

    if (is_called_every_ms(1005))
      set_num_visible_outputs(plugin);
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

  if (plugin->type != &seqtrack_type_general)
    return; // Number of visible outputs for buses is static.
  
  R_ASSERT(THREADING_is_main_thread());
  
  Data *data = (Data*)plugin->data;

  //int old_visible_channels = plugin->num_visible_outputs;
  
  int new_visible_channels = 2; // Need at least two visible output channels in case user has changed stereo.
  for(auto *sample : data->_samples)
    new_visible_channels = R_MAX(new_visible_channels, sample->_num_ch);

  new_visible_channels = R_MAX(new_visible_channels,
                               SP_get_max_visible_input_channels_from_audio_input_links(SP_get_sound_producer(plugin))
                               );
    
  if (new_visible_channels > NUM_OUTPUTS)
    new_visible_channels = NUM_OUTPUTS;

  plugin->num_visible_outputs = new_visible_channels;
}

static int64_t add_sample(Data *data, filepath_t filename, enum ResamplerType resampler_type, const struct SeqBlock *seqblock, const struct SeqTrack *seqtrack, enum Seqblock_Type type){
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
  
  Sample *sample = new Sample(filename, reader1, reader2, resampler_type, seqblock, seqtrack, type);

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

int64_t SEQTRACKPLUGIN_add_sample(const struct SeqTrack *seqtrack, SoundPlugin *plugin, filepath_t filename, enum ResamplerType resampler_type, const struct SeqBlock *seqblock, Seqblock_Type type){
  R_ASSERT(THREADING_is_main_thread());
  
  R_ASSERT_RETURN_IF_FALSE2(PLUGIN_is_for_seqtrack(plugin), -1);

  Data *data = (Data*)plugin->data;

  if (type==Seqblock_Type::RECORDING)
    return INITIAL_RECORDER_ID;

  int64_t ret = add_sample(data, filename, resampler_type, seqblock, seqtrack, type);

  //printf("   ADD (is_gfx: %d). NUM samples: %d.  NUM gfx samples: %d\n", is_gfx, data->_samples.size(), data->_gfx_samples.size());

  if (type==Seqblock_Type::REGULAR)
    set_num_visible_outputs(plugin);
    
  return ret;
}

// Called when user enables the "R" checkbox.
void SEQTRACKPLUGIN_enable_recording(struct SeqTrack *seqtrack, SoundPlugin *plugin, filepath_t path){
  R_ASSERT(THREADING_is_main_thread());

  Data *data = (Data*)plugin->data;

  int status = ATOMIC_GET(data->_recording_status);

  if (status == NOT_RECORDING) {

    R_ASSERT_RETURN_IF_FALSE(data->_recorder == NULL);

    auto *config = get_seqtrack_recording_config(seqtrack);
    
    int64_t latency = 0;

    if (config->compensate_latency){

      int64_t my_latency = 0;
      
      bool manifests = false;

      auto *soundproducer = SP_get_sound_producer(plugin);
      if (soundproducer==NULL){
        R_ASSERT_NON_RELEASE(false);
      }else{
        radium::PlayerLock lock;
        manifests = plugin->RT_input_latency_manifests_into_output_latency;
        my_latency = RT_SP_get_input_latency(soundproducer);
      }
      
      if (config->record_from_system_input) {
        
        latency = MIXER_get_recording_latency_compensation_from_system_in(); // Recording latency from the sound card + Playback latency from the sound card.
        latency += MIXER_get_latency_for_main_system_out(); // In case there are parallel running plugins introducing latency in the chain.

        if (manifests && MIXER_is_connected_to_system_out(soundproducer)){
          latency -= my_latency; // If piping, subtract latency at this point from the total latency.
          if (latency < 0){
            R_ASSERT_NON_RELEASE(false);
            latency = 0;
          }
        }
        
      } else {
        
        latency = my_latency;
        
      }
      
    }
    
    data->_recorder = new Recorder(seqtrack, path, config, latency);
    
    ATOMIC_SET(data->_recording_status, READY_TO_RECORD);
  }
}

static bool request_stop_running_recorder(Data *data){
  if (ATOMIC_COMPARE_AND_SET_INT(data->_recording_status, IS_RECORDING, REQUEST_STOP_RECORDING)){

#if 0
    const int ms_to_wait = 5000; // 5 seconds
    const int ms_to_wait_per_call = 10;
    int ms = 0;
    
    while(ATOMIC_GET(data->_recording_status) != NOT_RECORDING && ms < ms_to_wait){
      msleep(ms_to_wait_per_call);
      ms += ms_to_wait_per_call;
    }
    
    R_ASSERT_NON_RELEASE(ATOMIC_GET(data->_recording_status)==NOT_RECORDING);
#endif
    
  } else {
    
    R_ASSERT_NON_RELEASE(false);
    
  }

  return false;
}



// Called when user disables the "R" checkbox.
// If it returns true, it means that we have to wait for SEQTRACK_call_me_when_recorder_is_finished.
bool SEQTRACKPLUGIN_request_stop_recording(struct SeqTrack *seqtrack, SoundPlugin *plugin){
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
#if !defined(RELEASE)
      int status = ATOMIC_GET(data->_recording_status);
      R_ASSERT_NON_RELEASE(status==IS_RECORDING);
#endif
      return request_stop_running_recorder(data); // Between the first and the second test whether data->_recording_status==READY_TO_RECORD, it switched status to IS_RECORDING.
    }

    return false;
    
  } else {

    return request_stop_running_recorder(data);

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
  //printf("   APPLY. NUM samples: %d.  NUM gfx samples: %d. Bef: %d / %d\n", aft, aft_gfx, bef, bef_gfx);
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

  R_ASSERT_RETURN_IF_FALSE(PLUGIN_is_for_seqtrack(plugin));
  
  Data *data = (Data*)plugin->data;

  data->temporarily_suspend_sample_assertions_from_other_threads();
  
  
  Sample *sample
    = type==Seqblock_Type::REGULAR ? get_sample(plugin, id, true, false, false)
    : type==Seqblock_Type::GFX ? get_sample(plugin, id, false, true, false)
    : get_sample(plugin, id, false, false, true);

  if(sample==NULL)
    return;

  if (ATOMIC_GET(sample->_state) != Sample::State::RUNNING){
    RError("Sample \"%S\" has already been requested removed from sequencer track. type: %d", sample->_filename.getString(), (int)type);
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

int SEQTRACKPLUGIN_get_num_samples(const SoundPlugin *plugin){
  R_ASSERT(THREADING_is_main_thread());
  
  const Data *data = (const Data*)plugin->data;

  return data->_samples.size();
}

int SEQTRACKPLUGIN_get_num_channels(const SoundPlugin *plugin, int64_t id){
  R_ASSERT(THREADING_is_main_thread());

  R_ASSERT_RETURN_IF_FALSE2(PLUGIN_is_for_seqtrack(plugin), -1);

  if (id <= HIGHEST_RECORDER_ID){
    Recorder *recorder = g_recorders.value(id);
    
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
  R_ASSERT_RETURN_IF_FALSE2(PLUGIN_is_for_seqtrack(plugin), -1);
  
  Sample *sample = get_sample(plugin, id, true, true, true);
  if (sample==NULL)
    return -1;

  return sample->_total_num_frames_in_sample;
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

filepath_t SEQTRACKPLUGIN_get_sample_name(const SoundPlugin *plugin, int64_t id, bool full_path){
  R_ASSERT_RETURN_IF_FALSE2(PLUGIN_is_for_seqtrack(plugin), make_filepath(L""));

  if (id == INITIAL_RECORDER_ID)
    return make_filepath(L"");

  if (id <= HIGHEST_RECORDER_ID){
    Recorder *recorder = g_recorders.value(id);
    
    if (recorder==NULL){
      R_ASSERT_NON_RELEASE(false);
      return make_filepath(L"");
    }

    if (recorder->_filename.isEmpty()==false)
      return recorder->_filename.get();
    else
      return make_filepath(recorder->recording_path.get());
  }

  Sample *sample = get_sample(plugin, id, true, true, true);
  if (sample==NULL)
    return make_filepath(L"");

#if !defined(RELEASE)
  if (full_path){
    QFileInfo info(STRING_get_qstring(sample->_filename.getString()));
    if (!info.isAbsolute())
      abort();
  }
#endif

  if (full_path)
    return sample->_filename.get();
  else
    return sample->_filename_without_path.get();
}

unsigned int SEQTRACKPLUGIN_get_sample_color(const SoundPlugin *plugin, int64_t id){
  if (id <= HIGHEST_RECORDER_ID)
    return 0x00ff0000;

  return SAMPLEREADER_get_sample_color(SEQTRACKPLUGIN_get_sample_name(plugin, id, true));
}
  
radium::Peakss SEQTRACKPLUGIN_get_peaks(const SoundPlugin *plugin, int64_t id){
  R_ASSERT_RETURN_IF_FALSE2(PLUGIN_is_for_seqtrack(plugin), radium::Peakss(NULL));

  if (id <= HIGHEST_RECORDER_ID){
    Recorder *recorder = g_recorders.value(id);
    
    if (recorder==NULL){
      R_ASSERT_NON_RELEASE(false);
      printf("SEQTRACKPLUGIN_get_peaks: recorder==NULL\n");
      return radium::Peakss(NULL);
    }

    return radium::Peakss(recorder->_peaks, recorder->num_ch); //.get_array();
  }

  Sample *sample = get_sample(plugin, id, true, true, true);
  if (sample==NULL)
    return radium::Peakss(NULL);

  return radium::Peakss(sample->_peaks->_peaks, sample->_peaks->_num_ch);
}

/*
void SEQTRACKPLUGIN_set_resampler_ratio(const SoundPlugin *plugin, int64_t id, double new_ratio){
  R_ASSERT_RETURN_IF_FALSE2(!strcmp(SEQTRACKPLUGIN_NAME, plugin->type->type_name), 1.0);

  if (id <= HIGHEST_RECORDER_ID)
    return 1.0;
  
  Sample *sample = get_sample(plugin, id, true, true, true);
  if (sample==NULL)
    return 1.0;

  return sample->_resampler_ratio;
}
*/

double SEQTRACKPLUGIN_get_resampler_ratio(const SoundPlugin *plugin, int64_t id){
  R_ASSERT_RETURN_IF_FALSE2(PLUGIN_is_for_seqtrack(plugin), 1.0);

  if (id <= HIGHEST_RECORDER_ID)
    return 1.0;
  
  Sample *sample = get_sample(plugin, id, true, true, true);
  if (sample==NULL)
    return 1.0;

  return sample->_resampler_ratio;
}

void SEQTRACKPLUGIN_set_grain_strict_no_jitter(SoundPlugin *plugin, int64_t id, bool new_strict_no_jitter){
  R_ASSERT_RETURN_IF_FALSE(PLUGIN_is_for_seqtrack(plugin));

  Sample *sample = get_sample(plugin, id, true, true, true);
  if (sample==NULL)
    return;
  
  safe_bool_write(&sample->_grain_strict_no_jitter, new_strict_no_jitter); //R_MAX(0.000001, new_gf / 1000.0f));
  ATOMIC_SET(sample->_has_updates, true);
  
  //printf("  SETTING it to %f\n", sample->_strict_no_jitter);
}

bool SEQTRACKPLUGIN_get_grain_strict_no_jitter(const struct SoundPlugin *plugin, int64_t id){
  R_ASSERT_RETURN_IF_FALSE2(PLUGIN_is_for_seqtrack(plugin), 50);

  Sample *sample = get_sample(plugin, id, true, true, true);
  if (sample==NULL)
    return false;
  
  return safe_bool_read(&sample->_grain_strict_no_jitter);
}
    
void SEQTRACKPLUGIN_set_grain_overlap(SoundPlugin *plugin, int64_t id, double new_gf){
  R_ASSERT_RETURN_IF_FALSE(PLUGIN_is_for_seqtrack(plugin));

  Sample *sample = get_sample(plugin, id, true, true, true);
  if (sample==NULL)
    return;
  safe_double_write(&sample->_grain_overlap, new_gf); //R_MAX(0.000001, new_gf / 1000.0f));
  ATOMIC_SET(sample->_has_updates, true);
  
  //printf("  SETTING it to %f\n", sample->_grain_overlap);
}

double SEQTRACKPLUGIN_get_grain_overlap(const struct SoundPlugin *plugin, int64_t id){
  R_ASSERT_RETURN_IF_FALSE2(PLUGIN_is_for_seqtrack(plugin), 50);

  Sample *sample = get_sample(plugin, id, true, true, true);
  if (sample==NULL)
    return 50;
  
  return safe_double_read(&sample->_grain_overlap); // * 1000.0f;
}
    
void SEQTRACKPLUGIN_set_grain_length(SoundPlugin *plugin, int64_t id, double new_gf){
  R_ASSERT_RETURN_IF_FALSE(PLUGIN_is_for_seqtrack(plugin));

  Sample *sample = get_sample(plugin, id, true, true, true);
  if (sample==NULL)
    return;
  safe_double_write(&sample->_grain_length, new_gf); //R_MAX(1, new_gf * (double)pc->pfreq / 1000.0));
  ATOMIC_SET(sample->_has_updates, true);
  
  //printf("  SETTING length to %f\n", new_gf);
}

double SEQTRACKPLUGIN_get_grain_length(const struct SoundPlugin *plugin, int64_t id){
  R_ASSERT_RETURN_IF_FALSE2(PLUGIN_is_for_seqtrack(plugin), 50);

  Sample *sample = get_sample(plugin, id, true, true, true);
  if (sample==NULL)
    return 50;
  
  return (double)safe_double_read(&sample->_grain_length);// * 1000.0 / (double)pc->pfreq;
}
    
void SEQTRACKPLUGIN_set_grain_jitter(struct SoundPlugin *plugin, int64_t id, double new_gf){
  R_ASSERT_RETURN_IF_FALSE(PLUGIN_is_for_seqtrack(plugin));

  Sample *sample = get_sample(plugin, id, true, true, true);
  if (sample==NULL)
    return;
  safe_double_write(&sample->_grain_jitter, R_BOUNDARIES(0, new_gf, 1));
  ATOMIC_SET(sample->_has_updates, true);
  
  //printf("  SETTING jitter to %f\n", sample->_grain_jitter);
}

double SEQTRACKPLUGIN_get_grain_jitter(const struct SoundPlugin *plugin, int64_t id){
  R_ASSERT_RETURN_IF_FALSE2(PLUGIN_is_for_seqtrack(plugin), 50);

  Sample *sample = get_sample(plugin, id, true, true, true);
  if (sample==NULL)
    return 50;
  
  return safe_double_read(&sample->_grain_jitter);
}
    
void SEQTRACKPLUGIN_set_grain_ramp(struct SoundPlugin *plugin, int64_t id, double new_gf){
  R_ASSERT_RETURN_IF_FALSE(PLUGIN_is_for_seqtrack(plugin));

  Sample *sample = get_sample(plugin, id, true, true, true);
  if (sample==NULL)
    return;
  safe_double_write(&sample->_grain_ramp, R_BOUNDARIES(0, new_gf, 0.5));
  ATOMIC_SET(sample->_has_updates, true);
  
  //printf("  SETTING ramp to %f\n", sample->_grain_ramp);
}

double SEQTRACKPLUGIN_get_grain_ramp(const struct SoundPlugin *plugin, int64_t id){
  R_ASSERT_RETURN_IF_FALSE2(PLUGIN_is_for_seqtrack(plugin), 33.3);

  Sample *sample = get_sample(plugin, id, true, true, true);
  if (sample==NULL)
    return 33.3;
  
  return safe_double_read(&sample->_grain_ramp);
}
    
enum ResamplerType SEQTRACKPLUGIN_get_resampler_type(const struct SoundPlugin *plugin, int64_t id){
  R_ASSERT_RETURN_IF_FALSE2(PLUGIN_is_for_seqtrack(plugin), RESAMPLER_SINC1);

  Sample *sample = get_sample(plugin, id, true, true, true);
  if (sample==NULL)
    return RESAMPLER_SINC1;

  return sample->_resampler_type;
}

// Called when loading a song saved with radium 9.9.12 or older.
void SEQTRACKPLUGIN_convert_old_granular_parameters(const struct SoundPlugin *plugin, struct SeqBlock *seqblock){
  Sample *sample = get_sample(plugin, seqblock->sample_id, true, true, true);
  if (sample==NULL)
    return;

  sample->convert_old_granular_parameters(seqblock);
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
    VECTOR_push_back(&ret, talloc_wcsdup(sample->_filename.getString()));

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

  //printf("RT_record called\n");
  
  Recorder *recorder = data->_recorder;
  const SeqtrackRecordingConfig &config = recorder->_config;

  bool empty_block_has_been_cleared = false;
  float empty_block[RADIUM_BLOCKSIZE];

  
  // Input buffer
  //
  float *inputs_[NUM_CHANNELS_RECORDING_MATRIX];
  float **inputs = inputs_;

  static_assert(NUM_CHANNELS_RECORDING_MATRIX==NUM_INPUTS, "hmm");
  static_assert(NUM_CHANNELS_RECORDING_MATRIX==NUM_OUTPUTS, "hmm");

  int num_input_channels;
    
  if (config.record_from_system_input) {
    num_input_channels = MIXER_get_main_inputs(const_cast<const float**>(inputs), NUM_CHANNELS_RECORDING_MATRIX);
  } else {
    memcpy(inputs, instrument_inputs, sizeof(float*)*NUM_CHANNELS_RECORDING_MATRIX);
    num_input_channels = NUM_CHANNELS_RECORDING_MATRIX;
  }
  
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
    
    for(int input_ch=0;input_ch<num_input_channels;input_ch++) {
      
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
  if (false==RT_SampleRecorder_add_audio(data->_recorder,
                                         const_cast<const float**>(outputs),
                                         RADIUM_BLOCKSIZE
                                         ))
    {
      data->RT_recording_has_stopped();
    }
}


static void RT_process_play_audiofiles(Data *data, SoundPlugin *plugin, int num_frames, float **__restrict__ inputs, float **__restrict__ outputs){
  if (data->_samples.size()==0)
    return;
  
  //if (is_really_playing_song()==false)
  //  return;

  //printf("Num samples: %d\n", data->_samples.size());

  int max_ch = 0;
  for(Sample *sample : data->_samples)
    max_ch = R_MAX(max_ch, sample->_num_ch);

  int num_outputs = plugin->type->num_outputs;
  
  // change outputs in case there are more channels in the audio file than NUM_OUTPUTS
  int extra_num_ch = max_ch - num_outputs;
  float *extra_outputs[R_MAX(1, max_ch)];
  float extra[extra_num_ch > 0 ? num_frames : 1];

  if (extra_num_ch > 0){

    memset(extra, 0, num_frames*sizeof(float)); // This data is not used for anything, but since we add data to it, we null it out first avoid nominalization/inf/nan/etc. problems.

    for(int ch=0;ch<num_outputs;ch++)
      extra_outputs[ch] = outputs[ch];

    for(int ch=num_outputs;ch<max_ch;ch++){
      extra_outputs[ch] = extra;
    }

    outputs = extra_outputs;
  }
  

  // Read samples
  for(Sample *sample : data->_samples)
#if 1

    sample->RT_process(num_frames, outputs);

#else

  // for debugging

  warning, not tested for a while.

    for(int i=0;i<num_frames;i++){
      float *outputs2[NUM_OUTPUTS];
      for(int ch=0;ch<NUM_OUTPUTS;ch++)
        outputs2[ch] = outputs[ch]+i;
      sample->RT_process(1, outputs2);
    }

#endif
}

static void RT_process_the_other_one(SoundPlugin *plugin, int64_t time, int num_frames, float **__restrict__ inputs, float **__restrict__ outputs){

  //SoundPluginType *type = plugin->type;
  Data *data = (Data*)plugin->data;

  SMOOTH_called_per_block(&data->_piping_volume);

  int status = ATOMIC_GET(data->_recording_status);
  if (status==IS_RECORDING || status==REQUEST_STOP_RECORDING)
    RT_record(data, num_frames, const_cast<const float**>(inputs));
  
  for(int ch = 0 ; ch < NUM_OUTPUTS ; ch++)
    if(ch < NUM_INPUTS) {
      SMOOTH_copy_sound(&data->_piping_volume, inputs[ch], outputs[ch], num_frames);
    } else {
      memset(outputs[ch], 0, num_frames*sizeof(float));    
    }

  RT_process_play_audiofiles(data, plugin, num_frames, inputs, outputs);
}

static void RT_process_bus(SoundPlugin *plugin, int64_t time, int num_frames, float **__restrict__ inputs, float **__restrict__ outputs){
  int num_ch = plugin->type->num_outputs;
  R_ASSERT_NON_RELEASE(num_ch==plugin->type->num_inputs);
  
  for(int ch = 0 ; ch < num_ch ; ch++)
    memcpy(outputs[ch], inputs[ch], sizeof(float) * num_frames);

  Data *data = (Data*)plugin->data;
  
  RT_process_play_audiofiles(data, plugin, num_frames, inputs, outputs);
}


static void RT_player_is_stopped(struct SoundPlugin *plugin){
  //printf("   RT_player_is_stopped called %f\n", TIME_get_ms() / 1000.0);
  
  Data *data = (Data*)plugin->data;

  for(Sample *sample : data->_samples)
    sample->RT_stop_playing(true);
}

static void set_effect_value(struct SoundPlugin *plugin, int time, int effect_num, float value, enum ValueFormat value_format, FX_when when){
  Data *data = (Data*)plugin->data;
  //printf("####################################################### Setting sine volume to %f\n",value);
  
  bool new_val = value >= 0.5;

  if (new_val != data->_enable_piping){
    
    if (g_is_loading)
      SMOOTH_force_target_value(&data->_piping_volume, new_val ? 1.0 : 0.0);
    else
      SMOOTH_set_target_value(&data->_piping_volume, new_val ? 1.0 : 0.0);
    
    data->_enable_piping = new_val;

    plugin->RT_input_latency_manifests_into_output_latency = data->_enable_piping;

  }
}

static void set_bus_effect_value(struct SoundPlugin *plugin, int time, int effect_num, float value, enum ValueFormat value_format, FX_when when){
  R_ASSERT_NON_RELEASE(false);
  /*
  int num_visible_outputs = round(scale(value, 0, 1, MIN_NUM_VISIBLE_PORTS-1, MAX_NUM_VISIBLE_PORTS));
  if (num_visible_outputs < MIN_NUM_VISIBLE_PORTS)
    plugin->num_visible_outputs = -1;
  else
    plugin->num_visible_outputs = R_MIN(num_visible_outputs, MAX_NUM_VISIBLE_PORTS);
  */
}

static float get_effect_value(struct SoundPlugin *plugin, int effect_num, enum ValueFormat value_format){
  Data *data = (Data*)plugin->data;
  return data->_enable_piping ? 1.0 : 0.0;
}

static float get_bus_effect_value(struct SoundPlugin *plugin, int effect_num, enum ValueFormat value_format){
  R_ASSERT_NON_RELEASE(false);
  return 0;
  
  /*
  if (plugin->num_visible_outputs == -1)
    return 0;
  else
    return R_BOUNDARIES(0, scale(plugin->num_visible_outputs, MIN_NUM_VISIBLE_PORTS-1, MAX_NUM_VISIBLE_PORTS, 0, 1), 1);
  */
}

static void get_display_value_string(SoundPlugin *plugin, int effect_num, char *buffer, int buffersize){
  Data *data = (Data*)plugin->data;
  snprintf(buffer,buffersize-1,"%s",data->_enable_piping ? "On" : "Off");
}

static void get_bus_display_value_string(SoundPlugin *plugin, int effect_num, char *buffer, int buffersize){
  R_ASSERT_NON_RELEASE(false);
  /*
  if (plugin->num_visible_outputs == -1)
    snprintf(buffer,buffersize-1,"Auto");
  else
    snprintf(buffer,buffersize-1,"%d",plugin->num_visible_outputs);
  */
}

static SoundPlugin *bus1 = NULL;
static SoundPlugin *bus2 = NULL;
static SoundPlugin *bus3 = NULL;
static SoundPlugin *bus4 = NULL;
static SoundPlugin *bus5 = NULL;


int PLUGIN_get_bus_num(SoundPluginType *type){
  if (type==&bus_type1)
    return 0;
  
  if (type==&bus_type2)
    return 1;
  
  if (type==&bus_type3)
    return 2;
  
  if (type==&bus_type4)
    return 3;
  
  if (type==&bus_type5)
    return 4;
  
  return -1;
}

bool PLUGIN_is_permanent_bus(SoundPluginType *type){
  return PLUGIN_get_bus_num(type) >= 0;
}

const char *PLUGIN_get_bus_plugin_name(int bus_num, int num_ch){
  switch(bus_num){
    case 0: return "Bus 1";
    case 1: return "Bus 2";
    case 2: return "Bus 3";
    case 3: return "Bus 4";
    case 4: return "Bus 5";
    default:{
      R_ASSERT(bus_num==-1);
      return SEQTRACKPLUGIN_BUS_NAME;
    }
  }
  
  /*
  if (num_ch==8) {
    switch(bus_num){
      case 0: return "Bus 1 (8ch)";
      case 1: return "Bus 2 (8ch)";
      case 2: return "Bus 3 (8ch)";
      case 3: return "Bus 4 (8ch)";
      case 4: return "Bus 5 (8ch)";
      default:{
        R_ASSERT(bus_num==-1);
        return SEQTRACKPLUGIN_BUS8_NAME;
      }
    }
  } else {
    R_ASSERT(num_ch==2);
    switch(bus_num){
      case 0: return "Bus 1";
      case 1: return "Bus 2";
      case 2: return "Bus 3";
      case 3: return "Bus 4";
      case 4: return "Bus 5";
      default:{
        R_ASSERT(bus_num==-1);
        return SEQTRACKPLUGIN_BUS2_NAME;
      }
    }
  }
  */
}
                                       
static void *create_plugin_data(const SoundPluginType *plugin_type, SoundPlugin *plugin, hash_t *state, float sample_rate, int block_size, bool is_loading){
  
  if(plugin_type==&bus_type1)
    bus1 = plugin;
  if(plugin_type==&bus_type2)
    bus2 = plugin;
  if(plugin_type==&bus_type3)
    bus3 = plugin;
  if(plugin_type==&bus_type4)
    bus4 = plugin;
  if(plugin_type==&bus_type5)
    bus5 = plugin;

  bool enable_piping = false;
  
  Data *data = new Data(sample_rate, enable_piping);
  //printf("####################################################### Setting sine volume to 0.5f (create_plugin_data)\n");

  //add_sample(L"/home/kjetil/radium/bin/sounds/bbs2.wav");
  
  return data;
}

static void cleanup_plugin_data(SoundPlugin *plugin){
  const SoundPluginType *plugin_type = plugin->type;
  
  if(plugin_type==&bus_type1)
    bus1 = NULL;
  if(plugin_type==&bus_type2)
    bus2 = NULL;
  if(plugin_type==&bus_type3)
    bus3 = NULL;
  if(plugin_type==&bus_type4)
    bus4 = NULL;
  if(plugin_type==&bus_type5)
    bus5 = NULL;

  Data *data = (Data*)plugin->data;
  //printf(">>>>>>>>>>>>>> Cleanup_plugin_data called for %p\n",plugin);
  delete data;
}

static void called_after_plugin_has_been_created(const SoundPluginType *plugin_type, struct SoundPlugin *plugin){
  plugin->RT_input_latency_manifests_into_output_latency = false;
}

static const char *get_effect_name(const struct SoundPlugin *plugin, int effect_num){
  return "Enable piping";
}

static const char *get_bus_effect_name(const struct SoundPlugin *plugin, int effect_num){
  R_ASSERT_NON_RELEASE(false);
  return "Number of audio meters";
}

static int get_effect_format(struct SoundPlugin *plugin, int effect_num){
  return EFFECT_FORMAT_BOOL;
}

static int get_bus_effect_format(struct SoundPlugin *plugin, int effect_num){
  R_ASSERT_NON_RELEASE(false);
  return EFFECT_FORMAT_INT;
}

const char *BUS_get_bus_name(int bus_num){
  if(bus_num==0)
    return bus1==NULL ? "Reverb Bus" : bus1->patch->name;
  else if(bus_num==1)
    return bus2==NULL ? "Chorus Bus" : bus2->patch->name;
  else if(bus_num==2)
    return bus3==NULL ? "Aux 1 Bus" : bus3->patch->name;
  else if(bus_num==3)
    return bus4==NULL ? "Aux 2 Bus" : bus4->patch->name;
  else if(bus_num==4)
    return bus5==NULL ? "Aux 3 Bus" : bus5->patch->name;
  else {
    RError("Unknown bus num %d", bus_num);
    return "???";
  }
}

static void init_num_ch(SoundPluginType *plugin_type, int num_ch){
  plugin_type->num_inputs               = num_ch; // Note: For bus plugins, this value is overwritten each time when loading song
  plugin_type->num_outputs              = num_ch; // Note: For bus plugins, this value is overwritten each time when loading song.
}
  
static void init_type(SoundPluginType *plugin_type, const char *type_name, const char *name, int num_ch){

  bool is_bus = !strcmp("Bus", type_name);

  init_num_ch(plugin_type, num_ch);

  plugin_type->type_name                = type_name;
  plugin_type->name                     = name;
  plugin_type->is_instrument            = false;
  plugin_type->note_handling_is_RT      = false;
  plugin_type->num_effects              = is_bus ? (int)EFF_NUM_BUS_EFFECTS : (int)EFF_NUM_EFFECTS;
  plugin_type->get_effect_format        = is_bus ? get_bus_effect_format : get_effect_format;
  plugin_type->get_effect_name          = is_bus ? get_bus_effect_name : get_effect_name;
  plugin_type->effect_is_RT             = NULL;
  plugin_type->create_plugin_data       = create_plugin_data;
  plugin_type->cleanup_plugin_data      = cleanup_plugin_data;
  plugin_type->called_after_plugin_has_been_created = called_after_plugin_has_been_created;
  
  plugin_type->RT_process       = is_bus ? RT_process_bus : RT_process_the_other_one;
  plugin_type->RT_player_is_stopped = RT_player_is_stopped;
  
  plugin_type->set_effect_value = is_bus ? set_bus_effect_value : set_effect_value;
  plugin_type->get_effect_value = is_bus ? get_bus_effect_value : get_effect_value;
  plugin_type->get_display_value_string = is_bus ? get_bus_display_value_string : get_display_value_string;

  plugin_type->will_never_autosuspend = true; // TODO: Touch plugin manually instead.
}

// Called from disk_song.cpp.
void BUS_set_num_channels(int num_channels){
  init_num_ch(&bus_type1, num_channels);
  init_num_ch(&bus_type2, num_channels);
  init_num_ch(&bus_type3, num_channels);
  init_num_ch(&bus_type4, num_channels);
  init_num_ch(&bus_type5, num_channels);
  init_num_ch(&bus_type_general, num_channels);
}


void create_seqtrack_plugin(void){
  static bool has_inited = false;

  if (has_inited==false) {
    has_inited = true;

    init_type(&bus_type1, "Bus", PLUGIN_get_bus_plugin_name(0, 2), 2);
    init_type(&bus_type2, "Bus", PLUGIN_get_bus_plugin_name(1, 2), 2);
    init_type(&bus_type3, "Bus", PLUGIN_get_bus_plugin_name(2, 2), 2);
    init_type(&bus_type4, "Bus", PLUGIN_get_bus_plugin_name(3, 2), 2);
    init_type(&bus_type5, "Bus", PLUGIN_get_bus_plugin_name(4, 2), 2);
    
    static_assert(NUM_INPUTS==NUM_OUTPUTS, "hmm");
    init_type(&seqtrack_type_general, SEQTRACKPLUGIN_NAME, SEQTRACKPLUGIN_NAME, NUM_INPUTS);
    
    init_type(&bus_type_general, "Bus", SEQTRACKPLUGIN_BUS_NAME, 2);
  }

  PR_add_plugin_type_no_menu(&bus_type1);
  PR_add_plugin_type_no_menu(&bus_type2);
  PR_add_plugin_type_no_menu(&bus_type3);
  PR_add_plugin_type_no_menu(&bus_type4);
  PR_add_plugin_type_no_menu(&bus_type5);

  PR_add_plugin_type_no_menu(&seqtrack_type_general);
  PR_add_plugin_type_no_menu(&bus_type_general);
}
