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


#include "../common/nsmtracker.h"
#include "../common/OS_Player_proc.h"

#include "Envelope_proc.h"



static int ms_to_frames(float ms, float samplerate){
  return ms * samplerate / 1000.0f;
}

namespace{
struct Envelope{
  int   *_x;
  float *_y;
  int   _num_breaks;

  float _samplerate;
  int _frame_pos;
  int _break_pos;

  int _freeze_breakpoint;

  Envelope(int num_breaks, float samplerate){
    _x = new int[num_breaks];
    _y = new float[num_breaks];
    _num_breaks = num_breaks;
    _samplerate = samplerate;

    reset();
  }

  ~Envelope(){
    delete[] _x;
    delete[] _y;
  }

  void reset(){
    _frame_pos = 0;
    _break_pos = 0;
    _freeze_breakpoint = -1;
  }

  void set_freeze_breakpoint(int num){
    _freeze_breakpoint = num;
  }

  void unfreeze(){
    _freeze_breakpoint = -1;
  }

  // x is in milliseconds
  void set_break(int pos, float x, float y){
    _x[pos] = ms_to_frames(x,_samplerate);
    _y[pos] = y;
  }

  int apply_data(float *data, int num_frames){
    int i=0;
    while(i < num_frames && _break_pos<_num_breaks-1){

      //printf("get_data. This: %p, _freeze_breakpoint: %d, _break_pos: %d. _frame_pos: %d, i: %d, num_frames: %d\n",this,_freeze_breakpoint,_break_pos,_frame_pos,i,num_frames);

      if(_freeze_breakpoint==_break_pos){
        int frames_left = num_frames - i;
        float val = _y[_break_pos];
        int x;

        if (!equal_floats(val, 1.0f)){
          for(x=0; x < frames_left ; x++)
            data[x] *= val;
        }

        i = num_frames;
        break;
      }

      float y0 = _y[_break_pos];
      float y1 = _y[_break_pos+1];
      
      int x0  = _x[_break_pos];
      int x1  = _x[_break_pos+1];
      
      int loop_len = x1-_frame_pos;

      if (loop_len > 0) {
      
        if(i+loop_len > num_frames)
          loop_len = num_frames - i;

        if (equal_floats(y0, y1)) {

          if (!equal_floats(y0, 1.0f))
            for(int x=0;x<loop_len;x++)
              data[x] *= y0;
          
        } else {
          
          float val     = scale(_frame_pos, x0, x1, y0, y1);
          float inc_val = scale(_frame_pos+1, x0, x1, y0, y1) - val;

          for(int x=0;x<loop_len;x++)
            data[x] *= (val + x*inc_val);
          
        }
        
        i          += loop_len;
        data       += loop_len;
        _frame_pos += loop_len;

      }
      
      if(_frame_pos==x1)
        _break_pos++;
    }

    return i;
  }

  int RT_apply(float **buf,int num_channels, int num_frames){
    if (num_frames==0){
#if defined(RELEASE)
      return 0;
#else
      abort();
#endif
    }

    int len = 0;
    for(int ch=0;ch<num_channels;ch++){
      int new_len = apply_data(buf[ch], num_frames);
      R_ASSERT_NON_RELEASE(ch==0 || new_len==len);
      len=new_len;
    }
    
    return len;
  }
};
}

void *ENVELOPE_create(int num_breaks, float samplerate){
  return new Envelope(num_breaks,samplerate);
}

void ENVELOPE_delete(void *env){
  Envelope *envelope=(Envelope*)env;
  delete envelope;
}

void ENVELOPE_set_freeze_breakpoint(void *env,int breakpoint){
  Envelope *envelope=(Envelope*)env;
  envelope->set_freeze_breakpoint(breakpoint);
}

void ENVELOPE_unfreeze(void *env){
  Envelope *envelope=(Envelope*)env;
  envelope->unfreeze();
}

void ENVELOPE_reset(void *env){
  Envelope *envelope=(Envelope*)env;
  envelope->reset();
}

// envelope_length is the number of frames the envelope covers:
// start <= end <= envelope_length
int ENVELOPE_apply(void *env, float **buf, int num_channels, int num_frames){
  Envelope *envelope=(Envelope*)env;
  return envelope->RT_apply(buf,num_channels,num_frames);
}

//void ENVELOPE_num_samples_left(void *env, int pos);
namespace{
struct ADSR{
  Envelope *env;

  ADSR(float samplerate)
  {
    env = new Envelope(5,samplerate);    
    env->set_freeze_breakpoint(3);
  }

  ~ADSR(){
    delete env;
  }
  
  void set_adsr(float a, float h, float d, float s, float r){
    //printf("Setting adsr to %f %f %f %f\n",a,d,s,r);
    env->set_break(0, 0,   0.0);
    env->set_break(1, a,   1.0);
    env->set_break(2, a+h,   1.0);
    env->set_break(3, a+h+d, s);
    env->set_break(4, a+h+d+r, 0.0);
  }
};
}

void *ADSR_create(float samplerate){
  return new ADSR(samplerate);
}

void ADSR_delete(void *adsr){
  ADSR *das_adsr=(ADSR*)adsr;
  delete das_adsr;
}

int ADSR_apply(void *adsr, float **buf, int num_channels, int num_frames){
  ADSR *das_adsr=(ADSR*)adsr;
  return das_adsr->env->RT_apply(buf,num_channels,num_frames);
}

void ADSR_release(void *adsr){
  ADSR *das_adsr=(ADSR*)adsr;
  das_adsr->env->unfreeze();
}

void ADSR_reset(void *adsr){
  ADSR *das_adsr=(ADSR*)adsr;
  das_adsr->env->reset();
  das_adsr->env->set_freeze_breakpoint(3);
}

void ADSR_set_adsr(void *adsr, float a, float h, float d, float s, float r){
  ADSR *das_adsr=(ADSR*)adsr;
  das_adsr->set_adsr(a,h,d,s,r);
}
