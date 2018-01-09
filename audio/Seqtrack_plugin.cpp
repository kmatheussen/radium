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

#define INCLUDE_SNDFILE_OPEN_FUNCTIONS 1

#include "../common/nsmtracker.h"
#include "../common/SeqAutomation.hpp"

#include "SoundPlugin.h"
#include "SoundPlugin_proc.h"
#include "Peaks.hpp"
#include "SampleReader_proc.h"
#include "Granulator.hpp"

#include "SoundPluginRegistry_proc.h"


#include "Seqtrack_plugin_proc.h"


#define NUM_INPUTS 8
#define NUM_OUTPUTS 8

#if 1
#  define FADE_OUT_MS 100
#  define FADE_IN_MS 2
#else
#  define FADE_OUT_MS 2
#  define FADE_IN_MS 2
#endif


//#define HOW_MUCH_NONSTRETCHED_TO_PREPARE (0.05 * 0.015
//#define HOW_MUCH_NONSTRETCHED_TO_PREPARE_BEFORE_STARTING 0.2

#define HOW_MUCH_TO_PREPARE 2 // in seconds. TODO: We need to slowly increase the buffer from 0.1 to 2. TODO2: Make these two numbers configurable.
#define HOW_MUCH_TO_PREPARE_BEFORE_STARTING 2 //in seconds

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


namespace{

static int64_t g_id = 0;


// TODO: Always keep start of sample from disk in memory to lower the time needed to start playing when there's lots of samples.
// Then we only need to read in audio for those samples that starts playing in the middle.
 
struct MyReader : public radium::GranulatorCallback{
  radium::LockAsserter lockAsserter;

private:

  float _curr_fade_volume;
  
  float _fadeout_inc;
  float _fadein_inc;
  int _fadeout_countdown;

  float _volume = 1.0;
  
public:

  radium::SampleReader *_reader;
  
  const int _num_ch;
  
  radium::Granulator _granulator;

  bool _do_granulate = false;
  
  MyReader(radium::SampleReader *reader)
    : _reader(reader)
    , _num_ch(SAMPLEREADER_get_num_channels(reader))
    , _granulator(_num_ch, this)
  {
  }
    

  ~MyReader(){
    SAMPLEREADER_delete(_reader);
    _reader = NULL;
  }

private:
  
  void RT_get_samples_from_disk(int num_frames, float **outputs, bool do_add, bool add_to_ch1_too, float volume) const {
    R_ASSERT_RETURN_IF_FALSE(_reader != NULL);

    int num_ch = _num_ch;
    
    if (do_add && add_to_ch1_too && num_ch==1)
      num_ch = 2;
    
    float *outputs2[num_ch];

    int total_read = 0;

    while(total_read < num_frames){

      for(int ch = 0 ; ch < num_ch ; ch++)
        outputs2[ch] = &outputs[ch][total_read];

      const int num_frames_left = num_frames - total_read;
      
      const int num_read = RT_SAMPLEREADER_read(_reader, outputs2, num_frames_left, do_add, add_to_ch1_too, volume);

      R_ASSERT_RETURN_IF_FALSE(num_read>0);

      total_read += num_read;
      R_ASSERT(total_read <= num_frames);
    }
  }

public:

  // granulator callback
  virtual float *get_next_sample_block(const int ch, int &num_frames) override {
    //printf("   ************************** ASKING for %d frames\n", num_frames);
    return RT_SAMPLEREADER_get_buffer(_reader, ch, num_frames);
  }

  void RT_get_samples2(int num_frames, float **outputs, bool do_add, bool add_to_ch1_too) const {
    //bool do_granulate = false;
    //bool do_granulate = true; //;
    
    if (_do_granulate){
      _granulator.RT_process(outputs, num_frames, do_add, add_to_ch1_too, _volume);
    } else {
      RT_get_samples_from_disk(num_frames, outputs, do_add, add_to_ch1_too, _volume);
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

    RT_get_samples2(num_frames, inputs, false, false);

#define INC_FADE_VOLUME()                       \
    _curr_fade_volume += _fadein_inc;           \
    if (_curr_fade_volume > 1.0)                \
      _curr_fade_volume = 1.0;
    
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
    if (_curr_fade_volume < 1.0)
      RT_add_fade_in_samples(num_frames, outputs);
    else
      RT_get_samples2(num_frames, outputs, true, true);
  }

  void RT_add_fade_out_samples(int num_frames, float **outputs){
    LOCKASSERTER_EXCLUSIVE(&lockAsserter);
    
    R_ASSERT_RETURN_IF_FALSE(_fadeout_countdown > 0);

    int num_fade_out_frames = R_MIN(num_frames, _fadeout_countdown);
    
    float outputs_buffer[_num_ch * num_fade_out_frames];
    float *inputs[_num_ch];
    
    for(int ch = 0 ; ch < _num_ch ; ch++)
      inputs[ch] = &outputs_buffer[ch*num_fade_out_frames];
    
    //printf("Fading out %p. Countdown: %d\n", _reader, _fadeout_countdown);
    
    RT_get_samples2(num_fade_out_frames, inputs, false, false);

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

  void set_volume(float new_volume){
    _volume = new_volume;
  }

  int get_fadeout_duration(){
    double stretch = mus_increment(_granulator._clm_granulators[0]);
    return (_fadeout_countdown / stretch);
  }

  void prepare_for_playing(int64_t sample_start_pos, double stretch, radium::FutureSignalTrackingSemaphore *gotit){
    _do_granulate = fabs(stretch - 1.0) > 0.001;
    
    for(int ch=0;ch<_num_ch;ch++)
      mus_reset(_granulator._clm_granulators[ch]);
    
    prepare_for_fadein();

    // TODO: This number should be dynamically calculated somehow depending on how long time it takes to read from disk.
    int64_t how_much_to_prepare = double(HOW_MUCH_TO_PREPARE_BEFORE_STARTING) * (double)pc->pfreq / stretch;

    for(int ch=0;ch<_num_ch;ch++)
      how_much_to_prepare = R_MAX(how_much_to_prepare,
                                  2 * mus_granulate_grain_max_length(_granulator._clm_granulators[ch]));
                                        

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
  const wchar_t *_filename;

  enum State{
    RUNNING,
    RT_REQUEST_DELETION,
    READY_FOR_DELETION
  };
  DEFINE_ATOMIC(enum State, _state) = RUNNING;

  MyReader *_curr_reader = NULL; // Currently playing reader.
  radium::Vector<MyReader*> _fade_out_readers; // Currently playing fade-out readers
  radium::Vector<MyReader*> _free_readers;

  DEFINE_ATOMIC(bool, _is_fading_out) = false;
  
  int _num_readers = 0; // Only read/written by the main thread.

  radium::DiskPeaks *_peaks;

  const struct SeqBlock *_seqblock;

  float _pitch = 1.0;
  
  const int _num_ch;
  const int64_t _total_num_frames_in_sample;
  const unsigned int _color;
  const wchar_t *_filename_without_path;
  
  bool _is_playing = false; // If false, we are not playing current. But fade-outs can play even if _is_playing==false.
  int _click_avoidance_fade_in = 0; // frames left while fading in.

  bool _do_looping = false;

  radium::LockAsserter lockAsserter;
  
  Sample(const wchar_t *filename, radium::SampleReader *reader1, radium::SampleReader *reader2, const struct SeqBlock *seqblock)
    : _filename(wcsdup(filename))
      //, _reader(NULL) //new MyReader(reader))
      //, _fade_out_reader(new MyReader(fade_out_reader))
    , _peaks(new radium::DiskPeaks(filename))
    , _seqblock(seqblock)
    , _num_ch(SAMPLEREADER_get_num_channels(reader1))
    , _total_num_frames_in_sample(SAMPLEREADER_get_total_num_frames_in_sample(reader1))
    , _color(SAMPLEREADER_get_sample_color(reader1))
    , _filename_without_path(wcsdup(SAMPLEREADER_get_sample_name(reader1)))
  {
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

  }

  ~Sample(){
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
    
    delete _peaks;
    
    free((void*)_filename);
    free((void*)_filename_without_path);
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

  void RT_stop_playing(void){    
    //printf("   player_is_stopped called %f. _curr_reader: %p\n", TIME_get_ms() / 1000.0, _curr_reader);
  
    if (_curr_reader!=NULL) {

      LOCKASSERTER_EXCLUSIVE(&_curr_reader->lockAsserter);
      
      //printf("    ==== RT_stop_playing stopping player: %p\n", _curr_reader);

      _curr_reader->prepare_for_fadeout();
      
      _fade_out_readers.push_back(_curr_reader);
      ATOMIC_SET(_is_fading_out, true);
        
      _curr_reader = NULL;
      _is_playing = false;
    }

  }

  bool I_am_playing(void){
    return is_playing_song()==true || ATOMIC_GET(_is_fading_out)==true;
  }
  
  void prepare_to_play(int64_t seqtime, radium::FutureSignalTrackingSemaphore *gotit){
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
      allocated_reader = new MyReader(samplereader); // light operation
    }
    
    MyReader *reader;

    
    bool num_free_readers;

    bool i_am_playing = I_am_playing();
    
    {
      radium::PlayerLock lock(i_am_playing);
      
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

    reader->prepare_for_playing(sample_start_pos, _seqblock->t.stretch, gotit);

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
        RT_stop_playing();
        if (_fade_out_readers.size()==0)
          ATOMIC_SET(_state, State::READY_FOR_DELETION);
      }
      
      if (is_really_playing_song()==false)
        return;
      
      if (_is_playing==false)
        return;
    }

    
    // Playing Current
    //
    if (_curr_reader != NULL){
      LOCKASSERTER_EXCLUSIVE(&_curr_reader->lockAsserter);

      // Set expansion (stretch/expansion has probably not changed since last time, but this is a light operation)
      for(int ch=0;ch<_num_ch;ch++)
        mus_set_increment(_curr_reader->_granulator._clm_granulators[ch], _seqblock->t.stretch);

      // Set volume
      _curr_reader->set_volume(_seqblock->envelope_volume);

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
    if (_curr_reader != NULL){
      int64_t how_much_to_prepare = double(HOW_MUCH_TO_PREPARE) * (double)pc->pfreq / _seqblock->t.stretch;

      for(int ch=0;ch<_num_ch;ch++)
        how_much_to_prepare = R_MAX(how_much_to_prepare,
                                    2 * mus_granulate_grain_max_length(_curr_reader->_granulator._clm_granulators[ch]));
      
      RT_SAMPLEREADER_called_per_block(_curr_reader->_reader, how_much_to_prepare);
    }
  }

  // Called after scheduler and before audio processing.
  bool RT_called_per_block(struct SeqTrack *seqtrack, int64_t curr_start_time, int64_t curr_end_time){ // (end_time-start_time is usually RADIUM_BLOCKSIZE.)
    LOCKASSERTER_EXCLUSIVE(&lockAsserter);

    if (is_really_playing_song()==false){
      //_is_playing = false;
      return false;
    }
    
    //printf("   RT_called_per_block %d -> %d (%d)\n", (int)curr_start_time, (int)curr_end_time, int(curr_end_time-curr_start_time));
    
    bool inside = curr_start_time >= _seqblock->t.time && curr_start_time < _seqblock->t.time2;

    if (inside != _is_playing) {
    
      if (inside) {
        
        //printf("   Play. RT_called_per_block %d -> %d (%d). Seqblock start/end: %d / %d\n", (int)curr_start_time, (int)curr_end_time, int(curr_end_time-curr_start_time), (int)_seqblock->time, (int)_seqblock->time2);
        //printf(" PLAYING SAMPLE. Time: %f\n", (double)curr_start_time / (double)pc->pfreq);
        
        atomic_pointer_write_relaxed((void**)&seqtrack->curr_sample_seqblock, (void*)_seqblock); // bang!
        GFX_ScheduleEditorRedraw();
        
        _is_playing = true;
        
        // Don't do this if starting to play from the beginning.
        _click_avoidance_fade_in = FADE_IN_MS * pc->pfreq / 1000;
        
      } else {
        
        if (atomic_pointer_read_relaxed((void**)&seqtrack->curr_sample_seqblock)==_seqblock){
          atomic_pointer_write_relaxed((void**)&seqtrack->curr_sample_seqblock, NULL);
          GFX_ScheduleEditorRedraw();
        }
        
        RT_stop_playing();
      }
    }

    return _is_playing || _seqblock->t.time > curr_start_time;
  }

};

 
struct Data{
  radium::Vector<Sample*> _samples;
  radium::Vector<Sample*> _gfx_samples;

  float _sample_rate;

  Data(float sample_rate)
    :_sample_rate(sample_rate)
  {
  }

  ~Data(){
    for(auto *sample : _samples){
      ATOMIC_SET(sample->_state, Sample::State::RT_REQUEST_DELETION); // To avoid assertion hit.
      
      R_ASSERT_NON_RELEASE(sample->_curr_reader==NULL);
      sample->RT_stop_playing(); // just in case _curr_reader!=NULL.
      
      delete sample;
    }
  }
  
  bool RT_called_per_block(struct SeqTrack *seqtrack){
    int64_t start_time = seqtrack->start_time;
    int64_t end_time = seqtrack->end_time;

    bool more_to_play = false;
    
    for(auto *sample : _samples)
      if (sample->RT_called_per_block(seqtrack, start_time, end_time)==true)
        more_to_play = true;

    return more_to_play;
  }

  void called_very_often(void){
  again:
    for(auto *sample : _samples){
      if (ATOMIC_GET(sample->_state)==Sample::State::READY_FOR_DELETION){
        PLAYER_lock();{
          _samples.remove(sample);
        }PLAYER_unlock();
        delete sample;
        goto again;
      }
    }
  }

  void prepare_to_play(const struct SeqTrack *seqtrack, int64_t seqtime, radium::FutureSignalTrackingSemaphore *gotit){
    for(auto *sample : _samples){
#if !defined(RELEASE)
      if (ATOMIC_GET(sample->_state)==Sample::State::RUNNING){
        int num = 0;
        VECTOR_FOR_EACH(struct SeqBlock *, seqblock, &seqtrack->seqblocks){
          if(seqblock==sample->_seqblock){
            R_ASSERT(seqblock->block==NULL);
            R_ASSERT(seqblock->sample_id == sample->_id);
            num++;
          }
        }END_VECTOR_FOR_EACH;
        if (num != 1){
          fprintf(stderr,"NUM: %d (%d)\n", num, num != 1);
          abort();
        }
      }
#endif
      sample->prepare_to_play(seqtime, gotit);
    }
  }
};
}

static int64_t add_sample(Data *data, const wchar_t *filename, const struct SeqBlock *seqblock, bool is_gfx){
  radium::SampleReader *reader1 = SAMPLEREADER_create(filename);

  if(reader1==NULL){
    //abort();
    return -1;
  }

  radium::SampleReader *reader2 = SAMPLEREADER_create(filename);

  if(reader2==NULL){
    //abort();
    SAMPLEREADER_delete(reader1);
    return -1;
  }

  Sample *sample = new Sample(filename, reader1, reader2, seqblock);

  if (is_gfx)
    data->_gfx_samples.push_back(sample);
  else
    data->_samples.push_back_in_realtime_safe_manner(sample);

  return sample->_id;
}

int64_t SEQTRACKPLUGIN_add_sample(SoundPlugin *plugin, const wchar_t *filename, const struct SeqBlock *seqblock, bool is_gfx){
  R_ASSERT_RETURN_IF_FALSE2(!strcmp(SEQTRACKPLUGIN_NAME, plugin->type->type_name), -1);
  
  Data *data = (Data*)plugin->data;
  int64_t ret = add_sample(data, filename, seqblock, is_gfx);

  printf("   ADD (is_gfx: %d). NUM samples: %d.  NUM gfx samples: %d\n", is_gfx, data->_samples.size(), data->_gfx_samples.size());

  return ret;
}

void SEQTRACKPLUGIN_apply_gfx_samples(SoundPlugin *plugin, bool seqtrack_is_live){
  Data *data = (Data*)plugin->data;
    
  for(auto *sample : data->_samples){
    if (ATOMIC_GET(sample->_state)==Sample::State::RUNNING)
      ATOMIC_SET(sample->_state, Sample::State::RT_REQUEST_DELETION);
  }

  if(data->_gfx_samples.size()==0){
    R_ASSERT_NON_RELEASE(false);
    return;
  }

  {
    data->_samples.ensure_there_is_room_for_more_without_having_to_allocate_memory(data->_gfx_samples.size());
    
    {
      radium::PlayerLock lock(seqtrack_is_live);
      for(auto *sample : data->_gfx_samples)
        data->_samples.push_back(sample);
    }
    
    data->_samples.post_add();

    data->_gfx_samples.clear();
  }

  printf("   APPLY. NUM samples: %d.  NUM gfx samples: %d\n", data->_samples.size(), data->_gfx_samples.size());
}
  
static Sample *get_sample(const SoundPlugin *plugin, int64_t id, bool search_non_gfx, bool search_gfx){
  Data *data = (Data*)plugin->data;

  if (search_gfx){
    for(auto *sample : data->_gfx_samples){
      if (sample->_id==id)
        return sample;
    }
  }

  if (search_non_gfx){
    for(auto *sample : data->_samples){
      if (sample->_id==id)
        return sample;
    }
  }

  R_ASSERT(false);
  return NULL;
}

void SEQTRACKPLUGIN_request_remove_sample(SoundPlugin *plugin, int64_t id, bool is_gfx){
  R_ASSERT_RETURN_IF_FALSE(!strcmp(SEQTRACKPLUGIN_NAME, plugin->type->type_name));
  
  Sample *sample = get_sample(plugin, id, !is_gfx, is_gfx);
  if (sample==NULL){
    GFX_addMessage("Warning: Unknown sample id %d requested removed from sequencer track", (int)id);
    return;
  }

  if (is_gfx){
    
    Data *data = (Data*)plugin->data;
    data->_gfx_samples.remove(sample);
    ATOMIC_SET_RELAXED(sample->_state, Sample::State::READY_FOR_DELETION);
    delete sample;

  } else {
  
    if (ATOMIC_GET(sample->_state)==Sample::State::RUNNING)
      ATOMIC_SET(sample->_state, Sample::State::RT_REQUEST_DELETION);
    else
      GFX_addMessage("Warning: Sample %s has already been requested removed from sequencer track", STRING_get_chars(sample->_filename));

  }
}

bool SEQTRACKPLUGIN_can_be_deleted(SoundPlugin *plugin){
  Data *data = (Data*)plugin->data;

  for(auto *sample : data->_samples){
    if (ATOMIC_GET(sample->_state) == Sample::State::RUNNING)
      return false;
  }

  return true;
}

int SEQTRACKPLUGIN_get_num_samples(SoundPlugin *plugin){
  Data *data = (Data*)plugin->data;

  return data->_samples.size();
}

int SEQTRACKPLUGIN_get_num_channels(const SoundPlugin *plugin, int64_t id){
  R_ASSERT_RETURN_IF_FALSE2(!strcmp(SEQTRACKPLUGIN_NAME, plugin->type->type_name), -1);
  
  Sample *sample = get_sample(plugin, id, true, true);
  if (sample==NULL)
    return -1;

  return sample->_num_ch;
}

int64_t SEQTRACKPLUGIN_get_total_num_frames_in_sample(const SoundPlugin *plugin, int64_t id){
  R_ASSERT_RETURN_IF_FALSE2(!strcmp(SEQTRACKPLUGIN_NAME, plugin->type->type_name), -1);
  
  Sample *sample = get_sample(plugin, id, true, true);
  if (sample==NULL)
    return -1;

  return sample->_total_num_frames_in_sample;
  //return sample->_interior_end - sample->_interior_start;
}

const wchar_t *SEQTRACKPLUGIN_get_sample_name(const SoundPlugin *plugin, int64_t id, bool full_path){
  R_ASSERT_RETURN_IF_FALSE2(!strcmp(SEQTRACKPLUGIN_NAME, plugin->type->type_name), L"");
  
  Sample *sample = get_sample(plugin, id, true, true);
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
  R_ASSERT_RETURN_IF_FALSE2(!strcmp(SEQTRACKPLUGIN_NAME, plugin->type->type_name), 0x505050);
  
  Sample *sample = get_sample(plugin, id, true, true);
  if (sample==NULL)
    return 0x505050;

  return sample->_color;
}

const radium::DiskPeaks *SEQTRACKPLUGIN_get_peaks(const SoundPlugin *plugin, int64_t id){
  R_ASSERT_RETURN_IF_FALSE2(!strcmp(SEQTRACKPLUGIN_NAME, plugin->type->type_name), NULL);
  
  Sample *sample = get_sample(plugin, id, true, true);
  if (sample==NULL)
    return NULL;

  return sample->_peaks;
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

void SEQTRACKPLUGIN_called_very_often(SoundPlugin *plugin){
  Data *data = (Data*)plugin->data;

  data->called_very_often();
}

static void RT_process(SoundPlugin *plugin, int64_t time, int num_frames, float **inputs, float **outputs){
  //SoundPluginType *type = plugin->type;
  Data *data = (Data*)plugin->data;

  // Null out channels
  for(int ch = 0 ; ch < NUM_OUTPUTS ; ch++)
    memset(outputs[ch], 0, num_frames*sizeof(float));

  //if (is_really_playing_song()==false)
  //  return;
  
  // Read samples
  for(Sample *sample : data->_samples)
    sample->RT_process(num_frames, outputs);
}

static void RT_player_is_stopped(struct SoundPlugin *plugin){
  //printf("   RT_player_is_stopped called %f\n", TIME_get_ms() / 1000.0);
  
  Data *data = (Data*)plugin->data;

  for(Sample *sample : data->_samples)
    sample->RT_stop_playing();
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

  PR_add_plugin_type_no_menu(plugin_type);
  //PR_add_plugin_type(plugin_type);
}
