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


#ifndef TEST_MAIN

  #include "../common/nsmtracker.h"
  #include "../common/OS_visual_input.h"

  #include "Resampler_proc.h"

#else

  #include <stdio.h>
  #include <samplerate.h>


  struct Root{
    bool editonoff;
  };

  struct Root das_root;
  struct Root *root = &das_root;

  #define ATOMIC_GET(a) a

  #define R_ASSERT_NON_RELEASE(a)
  #define R_ASSERT(a)

  #define R_MAX(a,b) (((a)>(b))?(a):(b))
  #define R_MIN(a,b) (((a)<(b))?(a):(b))

#endif




namespace radium{
  

#include "SampleInterpolator.cpp"


struct Resampler{
  virtual ~Resampler(){}
  
  //virtual void init(src_callback_t callback, int num_channels, void *callback_arg, void *other_init_args) = 0;
  virtual int read(double ratio,int num_frames, float *out) = 0;
  virtual void reset() = 0;
};


struct InterpolatingResampler : public Resampler{

  Interpolator m_interpolator;

  src_callback_t m_callback;
  void *m_callback_arg;
  
  float *m_in;
  int    m_num_in_frames_left;
  
  InterpolatingResampler(src_callback_t callback, void *callback_arg)
    : m_callback(callback)
    , m_callback_arg(callback_arg)
    , m_in(NULL)
    , m_num_in_frames_left(0)
  {
  }

private:
  
  int read_internal(
                    const double actual_ratio,
                    int num_out_frames_left,
                    float *out,
                    int total_num_frames_produced
                    )
  {
    
  top:
    
    if (num_out_frames_left==0)
      return total_num_frames_produced;

    R_ASSERT_NON_RELEASE(m_num_in_frames_left >= 0);
    //printf("m_num_in_frames_left: %d, actual_ratio: %f\n",m_num_in_frames_left,actual_ratio);
    
    if (m_num_in_frames_left <= 0)
      m_num_in_frames_left = (int)m_callback(m_callback_arg, &m_in);
     
    R_ASSERT_NON_RELEASE(m_num_in_frames_left >= 0);
        
    if (m_num_in_frames_left <= 0)
      return total_num_frames_produced;

    int num_frames_consumed;    
    int num_frames_produced = m_interpolator.process(
                                                     actual_ratio,
                                                     m_in,
                                                     m_num_in_frames_left,
                                                     out,
                                                     num_out_frames_left,
                                                     &num_frames_consumed
                                                     );
    
    R_ASSERT_NON_RELEASE(num_frames_consumed <= m_num_in_frames_left);
    R_ASSERT_NON_RELEASE(num_frames_produced <= num_out_frames_left);
    
    m_num_in_frames_left  -= num_frames_consumed;
    m_in                  += num_frames_consumed;

#if 1
    // Simulate recursive function here. Doesn't seem like gcc does tail optimization, at least not with -Og. Got crash because it runs out of stack when setting radium block size to 4096.
    num_out_frames_left -= num_frames_produced;
    out += num_frames_produced;
    total_num_frames_produced += num_frames_produced;

    goto top;
#else
    return read_internal(
                         actual_ratio,
                         num_out_frames_left       - num_frames_produced,
                         out                       + num_frames_produced,
                         total_num_frames_produced + num_frames_produced
                         );
#endif
  }
    
public:
  
  int read(double ratio, int num_out_frames_left, float *out) override {
    return read_internal(
                         1.0 / ratio,
                         num_out_frames_left,
                         out,
                         0);
  }
  
  void reset() override {
    m_interpolator.reset();
    m_num_in_frames_left = 0;
  }

};


struct SincResampler : public Resampler{
  SRC_STATE *_src_state;
  double _last_ratio;

  SincResampler(src_callback_t callback, int num_channels, void *arg, enum ResamplerType type){
    //_src_state = src_callback_new(callback, SRC_SINC_FASTEST, num_channels, NULL, arg);    
    //const static int typemap[]={,SRC_LINEAR,-1,SRC_SINC_FASTEST,SRC_SINC_BEST_QUALITY};
    
    int t = SRC_SINC_FASTEST;
    
    if (type==RESAMPLER_NON)
      t = SRC_ZERO_ORDER_HOLD;
    else if (type==RESAMPLER_LINEAR)
      t = SRC_LINEAR;
    else if (type==RESAMPLER_SINC1)
      t = SRC_SINC_FASTEST;
    else if (type==RESAMPLER_SINC2)
      t = SRC_SINC_BEST_QUALITY;
    else
      R_ASSERT(false);
             
    _src_state = src_callback_new(callback, t, num_channels, NULL, arg);
    _last_ratio = -1.0;
  }
  ~SincResampler(){
    src_delete(_src_state);
  }

  int read(double ratio,int num_frames, float *out) override {

    // sndlib only handle ratios of 1/256 -> 256
    {
      if (ratio <= 1/256.0){
        RT_message("Resampler: Ratio less than 1/256: %f", ratio);
#if !defined(RELEASE)
        abort();
#endif
        ratio = 1/256.0;
      }
      if (ratio > 256.0){
        RT_message("Resampler: Ratio higher than 256: %f", ratio);
#if !defined(RELEASE)
        abort();
#endif
        ratio = 256.0;
      }
    }
    
    if(_last_ratio != ratio){
      if(_last_ratio >= 0.0){        
        src_set_ratio(_src_state, ratio);
      }
      _last_ratio = ratio;
    }

    int ret = (int)src_callback_read(_src_state, ratio, num_frames, out);

    if(ret==0 && src_error(_src_state)!=0)
      RError("libsamplerate returned an error: %d: \"%s\". ratio: %f\n",
             src_error(_src_state),
             src_strerror(src_error(_src_state)),
             ratio);
    
    return ret;
  }

  void reset() override {
    _last_ratio = -1.0;
    src_reset(_src_state);
  }
};


} // end anon. namespace



struct radium::Resampler *RESAMPLER_create(src_callback_t callback, int num_channels, void *arg, enum ResamplerType type){
  radium::Resampler *resampler;

  R_ASSERT(num_channels = 1);

  if(type==RESAMPLER_CUBIC) {
    resampler = new radium::InterpolatingResampler(callback, arg);
  } else
    resampler = new radium::SincResampler(callback, num_channels, arg, type);
  
  return resampler;
}

void RESAMPLER_delete(struct radium::Resampler *resampler){
  delete resampler;
}

int RESAMPLER_read(struct radium::Resampler *resampler,double ratio,int num_frames, float *out){
  return resampler->read(ratio,num_frames,out);
}

void RESAMPLER_reset(struct radium::Resampler *resampler){
  return resampler->reset();
}




#ifdef TEST_MAIN

#include <stdlib.h>
#include <stdio.h>

static long src_callback(void *cb_data, float **out_data){
  static float data[1420]; // [NO_STATIC_ARRAY_WARNING]
  *out_data = data;
  return 1420;
}

#  include <xmmintrin.h>

static inline float scale(float x, float x1, float x2, float y1, float y2){
  return y1 + ( ((x-x1)*(y2-y1))
                /
                (x2-x1)
                );
}

int main(int argc, char **argv){
  
  _mm_setcsr(_mm_getcsr() | 0x8040);

  int num_frames = atoi(argv[1]); // If we hardcode the value, the compiler is likely to optimize things which it wouldn't have done normally
  //fprintf(stderr, "num_frames: %d\n",num_frames);
  
  float *data = (float*)malloc(sizeof(float)*num_frames);
 
  for(int i=0;i<num_frames;i++){
    data[i] = (float)i / (float)num_frames; // Insert some some legal values.
  }

  void *resampler = RESAMPLER_create(src_callback, 1, NULL, RESAMPLER_CUBIC);

  int top = 1024*1024*8 * 64 / num_frames;
  
  for(int i=0 ; i < top ; i ++)
    RESAMPLER_read(resampler, scale(i,0,top,0.1,8.0), num_frames, data);
  
  //printf("hello\n");
  return 0;
}
#endif
