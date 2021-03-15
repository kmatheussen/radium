/* Copyright 2018 Kjetil S. Matheussen

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

#ifndef _RADIUM_AUDIO_GRANULATOR_HPP
#define _RADIUM_AUDIO_GRANULATOR_HPP

#include "../common/Random.hpp"
#include "../common/Vector.hpp"

#include "Mixer_proc.h"
#include "Fade.hpp"
#include "Juce_plugins_proc.h"

#include "GranResampler.hpp"


// TODO: Investigate AudioPickupBuffer::_size. I think it can be set much lower. (or maybe it needs to be set higher)


#define D(a)


namespace radium{


struct AudioPickuper{
  bool _there_is_more_data_to_pick_up = true;
  
protected:

  virtual int pick_up_data_for_granulator(float **samples, int max_num_frames) = 0;

public:

  void reset(void){
    _there_is_more_data_to_pick_up = true;
  }

  int pick_up(float **samples, int num_ch, int max_num_frames){
    int ret = pick_up_data_for_granulator(samples, max_num_frames);

    if (ret==0){

      _there_is_more_data_to_pick_up = false;

      for(int ch=0;ch<num_ch;ch++)
        memset(samples[ch], 0, sizeof(float)*max_num_frames);

      ret = max_num_frames;
    }
    
    return ret;
  }
};


class AudioPickupBuffer{
  float **_data;
  
  const int _num_ch;

  int _start_pos = 0;
  int _end_pos = 0;
  int _num_frames_available = 0;

  int64_t _start_pos_global_pos = 0;
  
  AudioPickuper *_sample_up_picker = NULL;

  const int _size;
  
public:

  AudioPickupBuffer(const int num_ch, const int size, AudioPickuper *sample_up_picker)
    : _num_ch(num_ch)
    , _sample_up_picker(sample_up_picker)
    , _size(size + R_MAX(64, SLICE_SIZE)*2)
  {
    _data = (float**)V_malloc(sizeof(float*)*num_ch);
    
    for(int ch=0;ch<num_ch;ch++)
      _data[ch] = (float*)V_malloc(sizeof(float)*_size);
  }

  ~AudioPickupBuffer(){
    for(int ch=0;ch<_num_ch;ch++)
      V_free(_data[ch]);

    V_free(_data);
  }

  void set_sample_up_picker(AudioPickuper *sample_up_picker){
    _sample_up_picker = sample_up_picker;
  }
  
  void reset(void){
    _start_pos = 0;
    _end_pos = 0;
    _num_frames_available = 0;
    _start_pos_global_pos = 0;
    _sample_up_picker->reset();
  }

  bool is_filled_up(void) const {
    return _size==_num_frames_available;
  }

  int get_num_possible_frames_to_pick_up(void) const {
    R_ASSERT_NON_RELEASE((_size - _end_pos) > 0);
    return _size - _end_pos;
  }

  void push_or_pick_postop(int num_frames){

    // Adjust _end_pos
    {    
      _end_pos += num_frames;
      if (_end_pos >= _size){
        R_ASSERT_NON_RELEASE(_end_pos==_size);
        _end_pos = 0;
      }
    }

    if (is_filled_up()){
      
      _start_pos_global_pos += num_frames;
      _start_pos = _end_pos;
      
    } else {
    
      _num_frames_available += num_frames;
      
      int to_cut = _num_frames_available - _size;

      if (to_cut > 0) {
        
        _start_pos_global_pos += to_cut;
        _start_pos += to_cut;
        
        if (_start_pos >= _size) {
          _start_pos -= _size;
          R_ASSERT_NON_RELEASE(_start_pos < _size);
        }
        
        R_ASSERT_NON_RELEASE(_start_pos == _end_pos);
        
        _num_frames_available = _size;
        
      }
    }
  }

  int push_samples(const float **samples, int num_frames){
    if (num_frames==0)
      return 0;
    
    const int num_possible_frames_to_pick_up = get_num_possible_frames_to_pick_up();
    if (num_possible_frames_to_pick_up==0)
      return 0;

    if (num_possible_frames_to_pick_up < num_frames)
      num_frames = num_possible_frames_to_pick_up;
    
    for(int ch=0;ch<_num_ch;ch++)
      memcpy(_data[ch] + _end_pos, samples[ch], sizeof(float) * num_frames);

    push_or_pick_postop(num_frames);

    return num_frames;
  }
  
  void pick_up_more_samples(int num_frames){
    float *samples[_num_ch];
    
    for(int ch=0;ch<_num_ch;ch++)
      samples[ch] = _data[ch] + _end_pos;

    R_ASSERT_NON_RELEASE(num_frames <= MIXER_get_remaining_num_audioblock_frames());
    
    const int num_frames_to_pick_up = R_MIN(R_MAX(num_frames, MIXER_get_remaining_num_audioblock_frames()),
                                            get_num_possible_frames_to_pick_up());
    num_frames = _sample_up_picker->pick_up(samples, _num_ch, num_frames_to_pick_up);
    //printf("     .... Got %d samples. Wanted: %d\n", num_frames, wanted);

    R_ASSERT_NON_RELEASE(num_frames > 0);
    R_ASSERT_NON_RELEASE(num_frames <= (_size-_end_pos));
    R_ASSERT_NON_RELEASE(num_frames <= num_frames_to_pick_up);

    push_or_pick_postop(num_frames);
  }
  
  int get_local_pos_from_global_pos(int64_t global_pos){
    int diff = global_pos - _start_pos_global_pos;
    
    R_ASSERT_NON_RELEASE(diff >= 0);

    int ret = _start_pos + diff;    
    if (ret >= _size){
      ret -= _size;
      while (ret >= _size){
        R_ASSERT_NON_RELEASE(false);
        ret -= _size;
      }
    }

    return ret;
  }

  bool need_to_pick_up_more_samples(int64_t global_pos, int how_much_minimum) const {
    int64_t last_readable_global_pos = _start_pos_global_pos + _num_frames_available;
    
    if ((global_pos + how_much_minimum) < last_readable_global_pos)
      return false;
    else
      return true;
  }

  const float *get(int ch, int64_t global_pos, int &num_frames){
    
    R_ASSERT_NON_RELEASE(ch>=0 && ch<_num_ch);
    
    if (_start_pos_global_pos > global_pos){
      // R_ASSERT_NON_RELEASE(false); FIX ME. This happens when setting stretch to 0.01 in the sample player.
      return NULL;
    }

    while(need_to_pick_up_more_samples(global_pos, num_frames)){

      //printf("Picking up for ch %d. (global: %d)\n", ch, (int)global_pos);
      //R_ASSERT_NON_RELEASE(ch==0);
      
      pick_up_more_samples(num_frames);
      
      if (_start_pos_global_pos > global_pos){
        R_ASSERT_NON_RELEASE(false);
        return NULL;
      }
    }

    int local_pos = get_local_pos_from_global_pos(global_pos);
    
    int last_readable_pos;
    if (local_pos < _end_pos) // || !is_filled_up()) ['local_pos < _end pos' is always true before it's filled up]
      last_readable_pos = _end_pos;
    else
      last_readable_pos = _size;

    if(last_readable_pos <= local_pos){
      R_ASSERT_NON_RELEASE(false);
      return NULL;
    }
    
    num_frames = last_readable_pos - local_pos;

    R_ASSERT_NON_RELEASE(num_frames > 0);
    
    return _data[ch] + local_pos;
  }
  
};


class Grain{
  int64_t _global_read_pos;
  int _grain_pos;
  int _duration;

  int _fade_length;

public:
  
  void init(double ramp, int duration, int64_t global_read_pos, int wait){
    _fade_length = ramp*duration;;
    _duration = duration;
    _grain_pos = -wait;
    _global_read_pos = global_read_pos;

    R_ASSERT_NON_RELEASE(_grain_pos <= 0);
  }

  void RT_apply2(const float *in, float *out, const int ch, const int num_frames) const {
#if 0
    for(int i=0;i<num_frames;i++)
      out[i] += in[i];
    return;
#endif

    int local_pos = 0;
    int grain_pos = _grain_pos;

    // Fade in
    if (grain_pos < _fade_length){

      const int local_pos2 = R_MIN(_fade_length - grain_pos, num_frames);
      const int num_frames2 = local_pos2;

#define USE_FADE_CLASS 1

#if !USE_FADE_CLASS

      const float inc_inc = 1.0 / _fade_length;
      float inc = grain_pos * inc_inc;
      for(int i=0;i<local_pos2;i++){
        out[i] += in[i] * inc;
        D(if(true && ch==0)
            printf("A. %d: %f  (a: %d). Fade length: %d\n", i+grain_pos, inc, i, _fade_length););
        inc += inc_inc;
      }

#else

      RT_fade_in_and_add(out, in, _fade_length, grain_pos, grain_pos + local_pos2);

#endif
    
      if(num_frames2==num_frames)
        return;

      grain_pos += num_frames2;
      local_pos = local_pos2;
    }

    const int fade_pos2 = _duration-_fade_length;

    // Fade out and return
    if (grain_pos >= fade_pos2){

      int local_pos2 = R_MIN(num_frames, _duration-grain_pos);

#if !USE_FADE_CLASS
      const float inc_inc = 1.0 / _fade_length;
      float inc = (_duration-grain_pos) * inc_inc;
      for(int i=local_pos;i<local_pos2;i++){
        out[i] += in[i] * inc;
        D(if(true && ch==0)
            printf("B. %d: %f  (b: %d). Duration: %d\n", i-local_pos + grain_pos, inc, i, _duration););
        inc -= inc_inc;
      }
#else
      int startpos = _fade_length - (_duration - grain_pos);
      int dur = local_pos2 - local_pos;
      RT_fade_out_and_add(out + local_pos, in + local_pos, _fade_length, startpos, startpos+dur);
#endif

      R_ASSERT_NON_RELEASE(grain_pos+(local_pos2-local_pos)<=_duration);
      
      return;
    }

    // No fading
    {
      const int local_pos2 = R_MIN(num_frames, fade_pos2 - grain_pos);
      const int num_frames2 = local_pos2 - local_pos;

      D(if(ch==0)
          printf(" D. %d -> %d. (d: %d -> %d)\n", grain_pos, grain_pos+num_frames2, local_pos, local_pos+num_frames2););
      
      JUCE_add_sound(out+local_pos, in+local_pos, num_frames2);
      
      local_pos = local_pos2;
      grain_pos += num_frames2;
    }
    
    // Fade out after no fading
    if (local_pos < num_frames){
      const int local_pos2 = R_MIN(num_frames, _duration - grain_pos);

#if !USE_FADE_CLASS
      const float inc_inc = 1.0 / _fade_length;
      float inc = (_duration-grain_pos) * inc_inc;
      for(int i=local_pos;i<local_pos2;i++){
        out[i] += in[i] * inc;
        D(if(true && ch==0)
            printf("C. %d: %f  (c: %d). Duration: %d\n", i-local_pos + grain_pos, inc, i, _duration);)
        inc -= inc_inc;
      }
#else
      int startpos = _fade_length - (_duration - grain_pos);
      int dur = local_pos2 - local_pos;
      RT_fade_out_and_add(out + local_pos, in + local_pos, _fade_length, startpos, startpos+dur);
#endif
    }
  }

  // returns true if there's more to do.
  bool RT_apply(AudioPickupBuffer &src, float *out, int ch, int num_frames, int64_t total){

    //printf("%d: RT_apply. ch: %d: num_frames: %d. _duration: %d. _grain_pos: %d\n", (int)_id, ch, num_frames, _duration, _grain_pos);

    int out_pos = 0;
    
    if (_grain_pos < 0){
      
      _grain_pos += num_frames;
      
      if (_grain_pos <= 0)
        return true;

      out_pos = num_frames - _grain_pos;
      num_frames = _grain_pos;
      _grain_pos = 0;

    }

    /*
    if(ch==0 && _grain_pos==0){
      static int last_write;
      static int last_read;
      int write = (int)total+out_pos;
      int read = (int)_global_read_pos;
      printf("   %d: Writing %d  (dx: %d / %d)\n", write, read, write-last_write, read-last_read);
      last_write = write;
      last_read = read;      
    }
    */
    
    int frames_left = R_MIN(_duration-_grain_pos, num_frames);
    
    while(frames_left > 0) {

      int num_in_frames = frames_left;
      const float *in = src.get(ch, _global_read_pos, num_in_frames);

      if (in==NULL)
        return false;
      
      /*
      if(ch==0)
        printf("0: num_in_frames: %d\n", num_in_frames);
      else
        printf("1:                    %d:\n", num_in_frames);
      */
      
      int num_frames2 = R_MIN(num_in_frames, frames_left);

      RT_apply2(in, out + out_pos, ch, num_frames2);
      
      _global_read_pos += num_frames2;
      _grain_pos += num_frames2;
      out_pos += num_frames2;
      
      frames_left -= num_in_frames;
    }

    return _grain_pos < _duration;
  }
};

struct GranulatorParameters{
  double jitter;
  bool strict_no_jitter;

  double overlap;
  double grain_length;
  double stretch;
  double ramp;

  GranulatorParameters(){
    reset();
  }
  
  void reset(){
    jitter = 0.5; // 0 -> 1
    strict_no_jitter = false;
    overlap = 3.0;
    grain_length = 2009; // in frames
    stretch = 1.0;
    ramp = 0.3; // 0 -> 0.5
  }
};
  
class Granulator : public GranResampler{  
  Grain *_grains;

  radium::Vector<Grain*> _free_grains;
  radium::Vector<Grain*> *_playing_grains;

  GranulatorParameters _p;
  bool _using_jitter = true;

  double _write_frames_between_grains = 600;
  double _read_frames_between_grains = 600;

  double _global_write_pos = 0;
  double _global_write_pos_of_next_frame = 0;
  
  double _global_read_pos = 0;
  double _global_read_pos_of_next_frame = 0;

  double _last_global_write_pos = 0;
  double _last_global_read_pos = 0;

  radium::AudioPickupBuffer _pickup_buffer;
  radium::AudioPickuper *_sample_up_picker;

  radium::Random _random;

  bool _is_processing = false; // is true if last ch for RT_process was not equal to _num_ch-1.

  
public:

  Granulator *_next; // Used by the sample player which reuses Granulator instances by putting them in a pool.

  Granulator(const Granulator&) = delete;
  Granulator& operator=(const Granulator&) = delete;
    
  Granulator(int max_grain_length, int max_frames_between_grains, int max_overlap, int num_ch, AudioPickuper *sample_up_picker)
    : GranResampler(num_ch, NULL)
    , _pickup_buffer(num_ch, (max_grain_length+max_frames_between_grains), sample_up_picker)
    , _sample_up_picker(sample_up_picker)
  {
    int num_grains = max_overlap*num_ch*2;

    _grains = new Grain[num_grains];
    _playing_grains = new radium::Vector<Grain*>[num_ch];

    for(int i=0;i<num_grains;i++)
      _free_grains.push_back(&_grains[i]);
    
    for(int ch=0;ch<num_ch;ch++)
      _playing_grains[ch].reserve(num_grains); // It's a bit unclear how many grains there will be per channel, so we just reserve num_grains to be sure it's enough. (got error when using 1+num_grains/2).

    if(sample_up_picker != NULL)
      reset();
  }

  ~Granulator(){
    /*
    for(int ch=0;ch<_num_ch;ch++){
      R_ASSERT_NON_RELEASE(_playing_grains[ch].size() == 0);
    }
    */

    delete[] _grains;
    delete[] _playing_grains;
  }

  void set_sample_up_picker(AudioPickuper *sample_up_picker){
    _sample_up_picker = sample_up_picker;
    _pickup_buffer.set_sample_up_picker(sample_up_picker);
  }

  AudioPickuper *get_sample_up_picker(void) const {
    return _sample_up_picker;
  }

  int push_samples(const float **samples, int num_frames){
    return _pickup_buffer.push_samples(samples, num_frames);
  }
  
private:

  void increase_write_and_read_pos(void){
    if (!_using_jitter){
      
      if (_p.strict_no_jitter) {
        _global_write_pos_of_next_frame += R_MAX(1, round(_write_frames_between_grains));
        _global_read_pos_of_next_frame += R_MAX(1, round(_read_frames_between_grains));
      } else {
        _global_write_pos_of_next_frame += _write_frames_between_grains;
        _global_read_pos_of_next_frame += _read_frames_between_grains;
      }
      
    } else {
      
      double to_dec = _random.get_next(); // don't need to use _random.get_next_adjusted() since both write_pos and read_pos are increased by to_dec.
      
      if (to_dec < 0.0){
        R_ASSERT_NON_RELEASE(false);
        to_dec = 0.0;
      }
      
      _global_write_pos_of_next_frame += to_dec;
      _global_read_pos_of_next_frame += to_dec/_p.stretch;
      
    }
  }
  
  void set_frames_between_grains(const bool length_changed, const bool overlap_changed, const bool stretch_changed, const bool jitter_changed, const bool global_pos_changed = false){
    R_ASSERT_NON_RELEASE(length_changed || overlap_changed || stretch_changed || jitter_changed || global_pos_changed);
    
    const bool hop_changed = length_changed || overlap_changed;
    
    if (hop_changed)
      _write_frames_between_grains = _p.grain_length / _p.overlap;

    if (hop_changed || stretch_changed)
      _read_frames_between_grains = _write_frames_between_grains / _p.stretch;

    if (jitter_changed)
      _using_jitter = _p.jitter>0.001;

    if (_using_jitter && (hop_changed || jitter_changed)) {

      const double jittered = _write_frames_between_grains * _p.jitter;
      const double mi = _write_frames_between_grains - jittered;
      const double ma = _write_frames_between_grains + jittered;
      
      _random.set_boundaries(mi, ma, ms_to_frames(20));
    }

    if (hop_changed || stretch_changed || global_pos_changed){

      _global_write_pos_of_next_frame = _last_global_write_pos;
      _global_read_pos_of_next_frame = _last_global_read_pos;

      while (_global_write_pos_of_next_frame < _global_write_pos)
        increase_write_and_read_pos();              
    }

  }

public:

  void set_strict_no_jitter(bool strict_no_jitter){
    if (strict_no_jitter != _p.strict_no_jitter)
      _p.strict_no_jitter = strict_no_jitter;
  }

  bool get_strict_no_jitter(void) const {
    return _p.strict_no_jitter;
  }
    
  void set_overlap(double overlap){
    R_ASSERT(_is_processing==false);
    if (equal_doubles(overlap, _p.overlap))
      return;
    
    _p.overlap = overlap;

    set_frames_between_grains(false, true, false, false);
  }

  double get_overlap(void) const {
    return _p.overlap;
  }
  
  void set_grain_length(double grain_length){
    R_ASSERT(_is_processing==false);

    if (equal_doubles(grain_length, _p.grain_length))
      return;

    //printf("Setting grain length to %fms\n", frames_to_ms(grain_length));
    _p.grain_length = grain_length;

    set_frames_between_grains(true, false, false, false);
  }

  double get_grain_length(void) const {
    return _p.grain_length;
  }

  void set_stretch(double stretch){
    R_ASSERT(_is_processing==false);

    if (equal_doubles(stretch, _p.stretch))
      return;

    _p.stretch = stretch;

    set_frames_between_grains(false, false, true, false);
  }

  double get_stretch(void) const {
    return _p.stretch;
  }

  void set_jitter(double jitter){
    R_ASSERT(_is_processing==false);
    R_ASSERT_RETURN_IF_FALSE(jitter >= 0 && jitter <= 1);

    if (equal_doubles(jitter, _p.jitter))
      return;

    _p.jitter = jitter;
    set_frames_between_grains(false, false, false, true);
  }

  double get_jitter(void) const {
    return _p.jitter;
  }

  void set_ramp(double ramp){
    R_ASSERT(_is_processing==false);
    R_ASSERT_RETURN_IF_FALSE(ramp >= 0 && ramp <= 0.5);
    _p.ramp = ramp;
  }

  double get_ramp(void) const {
    return _p.ramp;
  }


private:

  void reset_step1(bool reset_parameters){
    R_ASSERT(_is_processing==false);
    
    for(int ch=0;ch<_num_ch;ch++){
      while(_playing_grains[ch].size() > 0){
        Grain *grain = _playing_grains[ch].at(0);
        _playing_grains[ch].remove_pos(0);
        _free_grains.push_back(grain);
      }
    }

    _global_write_pos = 0;
    _global_read_pos = 0;

    _global_read_pos_of_next_frame = 0;
    _global_write_pos_of_next_frame = 0;

    _last_global_write_pos = 0;
    _last_global_read_pos = 0;

    _random.reset();

    _pickup_buffer.reset();
  }


  void apply_parameters_step1(const GranulatorParameters &parms){
    _p = parms;
  }

  
public:
  
  void apply_parameters_and_reset(const GranulatorParameters &parms){
    reset_step1(false);
    apply_parameters_step1(parms);
    set_frames_between_grains(true, true, true, true, true);
  }

  void apply_parameters_and_reset(const Granulator &from){
    apply_parameters_and_reset(from._p);
  }

  void reset(bool reset_parameters = true){
    reset_step1(reset_parameters);
    
    if(reset_parameters){
      
      _p.reset();

      set_frames_between_grains(true, true, true, true, true);

    } else {

      set_frames_between_grains(false, false, false, false, true);
      
    }

  }

  void apply_parameters(const Granulator &from){
    apply_parameters_step1(from._p);

    set_frames_between_grains(true, true, true, true);
  }
  
private:
  
  void add_new_grains(const int num_samples){

    int64_t upper_global_write_pos = _global_write_pos + num_samples;
    
    //printf("upper: %d. %d (%d)\n", (int)_global_write_pos_of_next_frame, (int)upper_global_write_pos, _global_write_pos_of_next_frame <= upper_global_write_pos);
    
    while (_global_write_pos_of_next_frame <= upper_global_write_pos){
    
      int wait = _global_write_pos_of_next_frame - _global_write_pos;
      R_ASSERT_NON_RELEASE(wait >= 0);
      R_ASSERT_NON_RELEASE(wait <= num_samples); // Grains do work with wait > num_samples, but it is not supposed to be higher than num_samples here.
      
#if 0 //!defined(RELEASE)
      static int last_write_dx = 0;
      static int last_read_dx = 0;
      //printf("    grainread: %d (dx: %d %d) (wait: %d (%f)) (this: %p)\n", (int)_global_read_pos_of_next_frame, (int)(_global_read_pos_of_next_frame-last_global_read_pos), (int)(_global_write_pos_of_next_frame-last_global_write_pos), wait, (wait / _p.stretch), this);
      int write_dx = (int)_global_write_pos_of_next_frame - (int)_last_global_write_pos;
      int read_dx = (int)_global_read_pos_of_next_frame - (int)_last_global_read_pos;

      if (!_using_jitter)
        if (write_dx != last_write_dx || read_dx != last_read_dx){
          if (_p.strict_no_jitter || abs(write_dx-last_write_dx)>1 || abs(read_dx-last_read_dx)>1)
            printf("    dx differs: Read:%d %d. Write: %d %d\n", read_dx, last_read_dx, write_dx, last_write_dx);
        }
      
      last_write_dx = write_dx;
      last_read_dx = read_dx;
#endif

      //printf(" Adding grain at readpos %d. Len: %d. Wait: %d\n", (int)_global_read_pos_of_next_frame, (int)_grain_length, wait);
      
      for(int ch=0;ch<_num_ch;ch++){
          
        if (_free_grains.size() > 0){
          Grain *grain = _free_grains.pop_back();

          grain->init(_p.ramp, _p.grain_length, _global_read_pos_of_next_frame, wait);
          _playing_grains[ch].push_back(grain);
            
          //if(ch==1)
          //  printf(" Added grains. Size: %d / %d\n", _playing_grains[0].size(), _playing_grains[1].size());
            
        } else {
    
#if !defined(RELEASE)
          printf(" Not enough free grains\n");
#endif
            
        }
      }
      
      _last_global_read_pos = _global_read_pos_of_next_frame;
      _last_global_write_pos = _global_write_pos_of_next_frame;

      increase_write_and_read_pos();        
    }

  }
  
  void RT_process2(float *data, const int ch, int num_samples) {
    
    memset(data, 0, sizeof(float)*num_samples);

    for(int grain_pos = 0 ; grain_pos < _playing_grains[ch].size() ; ) {

      Grain *grain = _playing_grains[ch].at(grain_pos);
      
      if (grain->RT_apply(_pickup_buffer, data, ch, num_samples, _global_write_pos)) {
        
        grain_pos++;

      } else {

#if 1 // defined(RELEASE)
        _playing_grains[ch].remove_pos(grain_pos);
#else
        _playing_grains[ch].remove_pos(grain_pos, true); // keep order
#endif
        _free_grains.push_back(grain);

        //printf(" %d: Removed grain %d. Size: %d, %d\n", ch, grain_pos, _playing_grains[0].size(), _playing_grains[1].size());
          
      }
    }

  }

public:
  
  int RT_process(float *data, const int ch, int num_samples) {

    if (_playing_grains[ch].is_empty() && _sample_up_picker->_there_is_more_data_to_pick_up==false)
      return 0;

    const bool is_ch0 = (ch==0);
    const bool is_last_ch = (ch==_num_ch-1);
    
    if (is_ch0)
      _is_processing=true;

    if (is_ch0 && _sample_up_picker->_there_is_more_data_to_pick_up)
      add_new_grains(num_samples);
    
#if 0   
    bool doit = ATOMIC_GET(root->editonoff);
    if(doit)
      for(int i = 0 ; i < num_samples ; i++)
        RT_process2(data + i, ch, 1);
    else
      RT_process2(data, ch, num_samples);
#else
    RT_process2(data, ch, num_samples);
#endif      

    if (is_ch0) {
      _global_write_pos += num_samples;
      _global_read_pos += (double)num_samples / _p.stretch;
    }
    
    if (is_last_ch)
      _is_processing=false;

    return num_samples;
  }
  
};



}

#endif
