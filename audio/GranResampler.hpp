/* Copyright 2017 Kjetil S. Matheussen

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

#ifndef _RADIUM_AUDIO_GRANRESAMPLER_HPP
#define _RADIUM_AUDIO_GRANRESAMPLER_HPP


#include "../bin/packages/sndlib/clm.h"
#include "Resampler_proc.h"


namespace radium{
  

  
// GranResamplerCallback
////////////////////////
  
class GranResamplerCallback{
public:
  virtual float *get_next_granresampler_sample_block(const int ch, int &num_frames) = 0;

  virtual ~GranResamplerCallback() = default; // Crazy c++ stuff.
};


  
// SampleReaderCallback
////////////////////////

class SampleReaderCallback : public GranResamplerCallback{

 private:
  radium::SampleReader *_reader;

 public:
  SampleReaderCallback(radium::SampleReader *reader)
    : _reader(reader)
  {}

 private:
  float *get_next_granresampler_sample_block(const int ch, int &num_frames) override {
    return RT_SAMPLEREADER_get_buffer(_reader, ch, num_frames);
  }
};


 
// GranResampler
////////////////////////

#define GRANRESAMPLER_BUFFER_SIZE 64
 
class GranResampler : public GranResamplerCallback{

protected:
  
  int _num_ch;

private:
  
  GranResamplerCallback *_callback;
  float** _buffer;
  
  virtual void RT_process(float *output, const int ch, int num_frames) const = 0;

protected:
  
  float *call_callback(const int ch, int &num_frames) {
    if (_callback==NULL){
      
      R_ASSERT(false);
      
      static float temp[64] = {};
      num_frames = 64;
      
      return temp;
      
    } else {
      
      return _callback->get_next_granresampler_sample_block(ch, num_frames);
      
    }
  }
  
public:
  
  void RT_process(float **output, int num_frames) const {
    for(int ch=0;ch<_num_ch;ch++)
      RT_process(output[ch], ch, num_frames);
  } 

  void set_callback(GranResamplerCallback *callback){
    _callback = callback;
  }

private:
  
  // Used when instance is supplied as a callback to another GranResampler.
  float *get_next_granresampler_sample_block(const int ch, int &num_frames) override {
    RT_process(_buffer[ch], ch, GRANRESAMPLER_BUFFER_SIZE);
    num_frames = GRANRESAMPLER_BUFFER_SIZE;
    return _buffer[ch];
  }

public:
  
  GranResampler(int num_ch, GranResamplerCallback *callback)
    : _num_ch(num_ch)
    , _callback(callback)
  {
    _buffer = (float**)V_calloc(sizeof(float*), num_ch);
    for(int ch=0;ch<num_ch;ch++)
      _buffer[ch] = (float*)V_calloc(sizeof(float), GRANRESAMPLER_BUFFER_SIZE);
  }

  ~GranResampler(){    
    for(int ch=0;ch<_num_ch;ch++)
      V_free(_buffer[ch]);
    
    V_free(_buffer);        
  }
};


/*
class SampleReader2 : public GranResampler{

 private:
  radium::SampleReader *_reader;

 public:
  
  SampleReader2(radium::SampleReader *reader)
    : GranResampler(SAMPLEREADER_get_num_channels(reader), NULL)
    , _reader(reader)
  {}

  // Not necessary to override this one, but it's faster.
  float *get_next_granresampler_sample_block(const int ch, int &num_frames) override {
    return RT_SAMPLEREADER_get_buffer(_reader, ch, num_frames);
  }

private:
  
  void RT_process(float *output, const int ch, int num_frames) const override {
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
};
*/
 
// Resampler2
////////////////////////
 
class Resampler2 : public GranResampler{
  radium::Resampler **_resamplers;

  mutable int _curr_ch = 0;
  
public:

  float _ratio = 1.0;

  Resampler2(int num_ch, enum ResamplerType resampler_type, GranResamplerCallback *callback)
    : GranResampler(num_ch, callback)
  {
    _resamplers = (radium::Resampler**)V_calloc(sizeof(radium::Resampler*), num_ch);
    for(int ch=0;ch<num_ch;ch++)
      _resamplers[ch] = RESAMPLER_create(Resampler2::RT_src_callback, 1, this, resampler_type);
  }

  ~Resampler2(){
    for(int ch=0;ch<_num_ch;ch++)
      RESAMPLER_delete(_resamplers[ch]);
    
    V_free(_resamplers);
  }

  void reset(void){
    for(int ch=0;ch<_num_ch;ch++)
      RESAMPLER_reset(_resamplers[ch]);
  }
  
  static long RT_src_callback(void *cb_data, float **out_data){
    Resampler2 *data = static_cast<Resampler2*>(cb_data);

    int num_frames;
    *out_data = data->call_callback(data->_curr_ch, num_frames);
    
    return num_frames;
  }

  void RT_process(float *output, const int ch, int num_frames) const override {
    _curr_ch = ch;
      
    int samples_left = num_frames;
    int pos = 0;
    
    while(samples_left > 0){
      
      int num_frames_read = RESAMPLER_read(_resamplers[ch], _ratio, samples_left, &output[pos]);
      
      if (num_frames_read==0){
        R_ASSERT_NON_RELEASE(false);
        return;
      }
      
      pos += num_frames_read;
      samples_left -= num_frames_read;
    }
    
    R_ASSERT_NON_RELEASE(samples_left==0);      
  }

};



// Granulator
////////////////////////
 
class Granulator : public GranResampler{

public:
  
  mus_any **_clm_granulators;

  
private:

  class Buffer{
    float *_samples;
    int _num_frames;
    int _pos;

    int _ch;
    Granulator *_granulator;
    
  public:
    
    void init(int ch, Granulator *granulator){
      _ch = ch;
      _granulator = granulator;
      _num_frames = 0;
      _pos = 0;
    }

    float get_sample(void){
      if (_pos==_num_frames){
        _samples = _granulator->call_callback(_ch, _num_frames);
        _pos = 0;
      }
      
      float ret = _samples[_pos];
      
      _pos++;
      
      return ret;
    }    
  };
 
  Buffer *_buffers;
 

public:

  Granulator(int num_ch, double max_grain_size_in_seconds, double max_grain_frequency_in_seconds, GranResamplerCallback *callback)
    : GranResampler(num_ch, callback)
    , _clm_granulators((mus_any**)calloc(sizeof(mus_any*), num_ch))
    , _buffers(new Buffer[num_ch])  
      //, _curr_buffer((float**)calloc(sizeof(float*), num_ch))
  {

    for(int ch=0;ch<num_ch;ch++)
      _buffers[ch].init(ch, this);

    mus_float_t expansion = 1.0;
    mus_float_t scaler = 0.6;
    mus_float_t grain_length = 0.05;
    mus_float_t hop = 0.05;
    mus_float_t ramp = 0.4;
    mus_float_t jitter = 0.0;
    //int max_size = 1;

    unsigned long seed = mus_rand_seed();

    mus_set_srate(pc->pfreq);

    for(int ch=0;ch<num_ch;ch++) {
      _clm_granulators[ch] = mus_make_granulate(Granulator::static_get_next_raw_sample,
                                                expansion,
                                                grain_length,
                                                scaler,
                                                hop,
                                                ramp,
                                                jitter,
                                                (max_grain_size_in_seconds + max_grain_frequency_in_seconds) * pc->pfreq, // [1]
                                                NULL,
                                                &_buffers[ch]);
      /*
        [1] FIX: A too high value here is cluttered CPU usage since we read a lot of data less often rather than reading less data often.
        (it's really cluttered now)
       */
      mus_set_location(_clm_granulators[ch], seed); // Set all granulators to use the same seed (for random operations) so the channels don't get out of sync.
    }
    
  }

  ~Granulator(){
    for(int ch=0;ch<_num_ch;ch++)
      mus_free(_clm_granulators[ch]);
    
    free(_clm_granulators);
      
    delete[] _buffers;
  }

  static mus_float_t static_get_next_raw_sample(void *arg, int direction){
    R_ASSERT_NON_RELEASE(direction==1);
    
    Buffer *buffer = static_cast<Buffer*>(arg);
    
    return buffer->get_sample();
  }

  void reset(void){
    for(int ch=0;ch<_num_ch;ch++)
      mus_reset(_clm_granulators[ch]);
  }

  void RT_process(float *output, const int ch, int num_frames) const override {
    for(int i=0;i<num_frames;i++)
      output[i] = mus_granulate(_clm_granulators[ch], NULL);
  }
  
};


}


#endif

