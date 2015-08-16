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

#include <math.h>

#include "../common/nsmtracker.h"

#include "Resampler_proc.h"


// The cubic interpolation code is just horror. Is there any ready-made code out there which can be just plugged in here? (seems like it's just horror when it comes to cubic interpolation in other programs too)


struct Resampler{
  virtual ~Resampler(){}
  virtual int read(double ratio,int num_frames, float *out) = 0;
  virtual void reset() = 0;
};


struct SincResampler : public Resampler{
  SRC_STATE *_src_state;
  double _last_ratio;

  SincResampler(src_callback_t callback, int num_channels, void *arg, int type){
    //_src_state = src_callback_new(callback, SRC_SINC_FASTEST, num_channels, NULL, arg);
    const static int typemap[]={SRC_ZERO_ORDER_HOLD,SRC_LINEAR,-1,SRC_SINC_FASTEST,SRC_SINC_BEST_QUALITY};
    _src_state = src_callback_new(callback, typemap[type], num_channels, NULL, arg);
    _last_ratio = -1.0;
  }
  ~SincResampler(){
    src_delete(_src_state);
  }

  int read(double ratio,int num_frames, float *out){
    if(_last_ratio != ratio){
      if(_last_ratio >= 0.0)
        src_set_ratio(_src_state, ratio);
      _last_ratio = ratio;
    }

    int ret = src_callback_read(_src_state, ratio, num_frames, out);

    if(ret==0){
      if(src_error(_src_state)!=0)
        printf("Error? %d: \"%s\". ratio: %f\n",
               src_error(_src_state),
               src_strerror(src_error(_src_state)),
               ratio);
      return 0;
    }else
      return ret;
  }

  void reset(){
    src_reset(_src_state);
  }
};

// cubic interpolate function from http://paulbourke.net/miscellaneous/interpolation/
// Written by Paul Burke.

static float cubic_interpolate(
                               float y0,float y1,
                               float y2,float y3,
                               float mu)
{
#if 0
  float a0,a1,a2,a3;

  //float mu2 = mu*mu;

  #if 0
  // This sounds horrible! Far far worse than linear interpolation. Is there an error on the web page of Paul Burke? (most likely an error in the code in this file though)
  a0 = y3 - y2 - y0 + y1;
  a1 = y0 - y1 - a0;
  a2 = y2 - y0;
  a3 = y1;

  // The "Catmull-Rom splines" sounds correct though:

  #else

  float c = -0.5*y0;
  float d = 0.5*y3;

  a0 = c + 1.5*y1 - 1.5*y2 + d;
  a1 = y0 - 2.5*y1 + 2*y2 - d;
  a2 = c + 0.5*y2;
  a3 = y1;

  #endif

  //return (a0*mu*mu2 + a1*mu2 + a2*mu + a3);
  //return mu*(a0*mu2 + a1*mu + a2) + a3; // one less multiplication.
  return mu*(mu*(a0*mu + a1) + a2) + a3; // two less multiplications.

#else

  // Optimized by faust [1]. Same number of multiplications though, so it's probably the same speed.
  float h_y0 = 0.5f * y0;
  float h_y3 = 0.5f * y3;
  return y1 + (mu * (((0.5f * y2) - h_y0) + (mu * (((y0 + (2 * y2)) - (h_y3 + (2.5f * y1))) + (mu * ((h_y3 + (1.5f * y1)) - ((1.5f * y2) + h_y0)))))));

  #if 0
[1]
process(y0,y1,y2,y3,mu) =  a0*mu*mu2 + a1*mu2 + a2*mu + a3 with{
   mu2 = mu*mu;
   a0 = -0.5*y0 + 1.5*y1 - 1.5*y2 + 0.5*y3;
   a1 = y0 - 2.5*y1 + 2*y2 - 0.5*y3;
   a2 = -0.5*y0 + 0.5*y2;
   a3 = y1;
};
  #endif

#endif
}

struct InterpolatedResampler : public Resampler{
  float *_data;
  int _data_length;
  int _data_pos;

  src_callback_t _callback;
  int _num_channels;
  void *_arg;

  bool do_linearly;

  InterpolatedResampler(src_callback_t callback,int num_channels,void *arg){
    _callback=callback;
    _num_channels=num_channels;
    _arg=arg;

    reset();
  }

  ~InterpolatedResampler(){
  }

  void pull_more_data(){
    _data_length = _callback(_arg,&_data);
    _data_pos=0;
  }

  float getNextSample(){    
    R_ASSERT_NON_RELEASE(_data_pos <= _data_length);
    R_ASSERT_NON_RELEASE(_data_pos >= 0);

    if(_data_pos==_data_length)
      pull_more_data();
    if(_data_length==0)
      return 0.0f;

    R_ASSERT_NON_RELEASE(_data_pos < _data_length);
    R_ASSERT_NON_RELEASE(_data_pos >= 0);

    float ret=_data[_data_pos];
    _data_pos++;

    R_ASSERT_NON_RELEASE(_data_pos <= _data_length);
    R_ASSERT_NON_RELEASE(_data_pos >= 0);

    return ret;
  }

  double _curr_read_pos;
  float _y0;
  float _y1;
  float _y2;
  float _y3;

  float getLinearlyInterpolatedSample(double read_increment){
    while(_curr_read_pos > 1.0){
      _curr_read_pos-=1.0;
      _y0=_y1;
      _y1=getNextSample();
    }
    if(_data_length==0)
      return 0.0f;

    //float ret=scale( _curr_read_pos, 0, 1, _y0, _y1);
    // is probably a slower way to do:
    float ret=_y0 + (_curr_read_pos * (_y1-_y0));
    
    _curr_read_pos += read_increment;
    
    return ret;
  }

  float getCubicInterpolatedSample(double read_increment){
    R_ASSERT_NON_RELEASE(isfinite(_curr_read_pos));
    R_ASSERT_NON_RELEASE(isfinite(read_increment));
    R_ASSERT_NON_RELEASE(isfinite(_curr_read_pos));
    
    while(_curr_read_pos >= 1.0){
      _curr_read_pos -= 1.0;
      _y0             = _y1;
      _y1             = _y2;
      _y2             = _y3;
      _y3             = getNextSample();

      R_ASSERT_NON_RELEASE(isfinite(_y0));
      R_ASSERT_NON_RELEASE(isfinite(_y1));
      R_ASSERT_NON_RELEASE(isfinite(_y2));
      R_ASSERT_NON_RELEASE(isfinite(_y3));
    }

    R_ASSERT_NON_RELEASE(_curr_read_pos >= 0);
    R_ASSERT_NON_RELEASE(_curr_read_pos < 1.0);
    
    if(_data_length==0)
      return 0.0f;
        
    float ret=cubic_interpolate(_y0,_y1,_y2,_y3,_curr_read_pos);
    R_ASSERT_NON_RELEASE(isfinite(ret));
    
    _curr_read_pos += read_increment;
    
    return ret;
  }

  // somewhat optimized cubic interpolation. Can only be called when not at the beginning or at the end of the buffer.
  int read_without_pulling(int num_frames, float *out, double read_increment){
    double curr_read_pos = _curr_read_pos;

    float y0=_y0;
    float y1=_y1;
    float y2=_y2;
    float y3=_y3;

    int data_pos = _data_pos;

    for(int i=0;i<num_frames;i++){
      if(curr_read_pos>=1.0){
        int offset = curr_read_pos;
        data_pos += offset;

        R_ASSERT_NON_RELEASE(data_pos <= _data_length);
        R_ASSERT_NON_RELEASE(data_pos >= 0);

        y0 = _data[data_pos-4];
        y1 = _data[data_pos-3];
        y2 = _data[data_pos-2];
        y3 = _data[data_pos-1];

        curr_read_pos -= offset;
      }

      R_ASSERT_NON_RELEASE(curr_read_pos >= 0);
      R_ASSERT_NON_RELEASE(curr_read_pos < 1.0);

      out[i] = cubic_interpolate(y0,y1,y2,y3,curr_read_pos);
      
      curr_read_pos += read_increment;
    }
    _y0=y0;
    _y1=y1;
    _y2=y2;
    _y3=y3;
    _curr_read_pos = curr_read_pos;

    _data_pos = data_pos;

    R_ASSERT_NON_RELEASE(_data_pos <= _data_length);
    R_ASSERT_NON_RELEASE(_data_pos >= 0);

    return num_frames;
  }

  int find_num_samples_left_before_pulling(double read_increment){
    R_ASSERT_NON_RELEASE(isfinite(read_increment));

    return ( (double)(_data_length - _data_pos) / read_increment) - 2; // somewhat conservative;
  }


  // a bit chaotic. Quick optimizations.
  int read(double ratio,int num_frames, float *out){
    double read_increment = 1.0 / ratio;

    R_ASSERT_NON_RELEASE(isfinite(read_increment));
    R_ASSERT_NON_RELEASE(isfinite(ratio));
    
    int i=0;
    int frames_left = num_frames;

    bool use_opt = true;

#if 0
    // There's something wrong with this (very complex) code. Don't think it should affect performance very much just commenting it out.
    if(_data_pos>6){
      int num_frames_left_before_pulling = R_MIN(num_frames, find_num_samples_left_before_pulling(read_increment));
      if(num_frames_left_before_pulling>0){
        read_without_pulling(num_frames_left_before_pulling, out, read_increment);
        i           += num_frames_left_before_pulling;
        frames_left -= num_frames_left_before_pulling;
      }
    }
#endif
    
    //printf("num samples left: %d. (%d/%d) %f\n",find_num_samples_left_before_pulling(read_increment),_data_pos,_data_length,read_increment);

    for(;i<num_frames;i++){
      if(use_opt && _data_pos>6 && find_num_samples_left_before_pulling(read_increment) > frames_left){
        read_without_pulling(frames_left,out+i,read_increment); // should be a little bit faster than iterating inside this loop.
        break;
      }
      
      float val = getCubicInterpolatedSample(read_increment);
      //float val = getLinearlyInterpolatedSample(read_increment);
      if(_data_length==0)
        return i;
      else
        out[i] = val;

      frames_left--;
    }

    return num_frames;
  }

  virtual void reset(){
    _y0=0.0f;
    _y1=0.0f;
    _y2=0.0f;
    _y3=0.0f;

    _curr_read_pos=3.0;

    _data_length=0;
    _data_pos=0;
  }
};



void *RESAMPLER_create(src_callback_t callback, int num_channels, void *arg, int type){
  Resampler *resampler;
  //printf("_________ RESAMPLER_create: %d\n",type);

  if(type==RESAMPLER_CUBIC)
    resampler = new InterpolatedResampler(callback, num_channels, arg);
  else
    resampler = new SincResampler(callback, num_channels, arg, type);
  return resampler;
}

void RESAMPLER_delete(void *res){
  Resampler *resampler = static_cast<Resampler*>(res);
  delete resampler;
}

int RESAMPLER_read(void *res,double ratio,int num_frames, float *out){
  Resampler *resampler = static_cast<Resampler*>(res);
  return resampler->read(ratio,num_frames,out);
}

void RESAMPLER_reset(void *res){
  Resampler *resampler = static_cast<Resampler*>(res);
  return resampler->reset();
}
