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



#include "../bin/packages/sndlib/clm.h"

namespace radium{

class GranulatorCallback{
public:
  virtual float *get_next_granulator_sample_block(const int ch, int &num_frames) = 0;

  virtual ~GranulatorCallback() = default; // Crazy c++ stuff.
};

namespace{
  
class Granulator{

public:
  
  int _num_ch;
  mus_any **_clm_granulators;

private:

  class Buffer{
    float *_samples;
    int _num_frames;
    int _pos;

    int _ch;
    GranulatorCallback *_callback;

  public:
    
    void init(int ch, GranulatorCallback *callback){
      _ch = ch;
      _callback = callback;
      _num_frames = 0;
      _pos = 0;
    }
    
    float get_sample(void){
      if (_pos==_num_frames){
        _samples = _callback->get_next_granulator_sample_block(_ch, _num_frames);
        _pos = 0;
      }
      
      float ret = _samples[_pos];
      
      _pos++;
      
      return ret;
    }    
  };
 

  static mus_float_t static_get_next_raw_sample(void *arg, int direction){
    R_ASSERT_NON_RELEASE(direction==1);
    
    Buffer *buffer = static_cast<Buffer*>(arg);
    
    return buffer->get_sample();
  }

  Buffer *_buffers;
  
public:
  
  Granulator(int num_ch, GranulatorCallback *callback)
    : _num_ch(num_ch)
    , _clm_granulators((mus_any**)calloc(sizeof(mus_any*), num_ch))
      //, _curr_buffer((float**)calloc(sizeof(float*), num_ch))
    , _buffers(new Buffer[num_ch])
  {

    mus_float_t expansion = 1.0;
    mus_float_t length = 0.15;
    mus_float_t scaler = 0.6;
    mus_float_t hop = 0.05;
    mus_float_t ramp = 0.4;
    mus_float_t jitter = 1.0;
    int max_size = 1;

    unsigned long seed = mus_rand_seed();

    mus_set_srate(pc->pfreq);

    for(int ch=0;ch<num_ch;ch++) {
      _buffers[ch].init(ch, callback);
      _clm_granulators[ch] = mus_make_granulate(Granulator::static_get_next_raw_sample,
                                                expansion,
                                                length,
                                                scaler,
                                                hop,
                                                ramp,
                                                jitter,
                                                max_size,
                                                NULL,
                                                &_buffers[ch]);

      mus_set_location(_clm_granulators[ch], seed); // make sure all channels use the same random seed.
    }
    
  }

  ~Granulator(){
    for(int ch=0;ch<_num_ch;ch++)
      mus_free(_clm_granulators[ch]);
    
    delete[] _buffers;
  }
      
  void RT_process(float **output2, int num_frames) const {

    for(int ch=0;ch<_num_ch;ch++){
      float *output = output2[ch];
      for(int i=0;i<num_frames;i++)
        output[i] = mus_granulate(_clm_granulators[ch], NULL);
    }
    
  }
  
};


}
}
  
