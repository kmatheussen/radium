/*
  ==============================================================================
   This file is part of the JUCE library.
   Copyright (c) 2015 - ROLI Ltd.
   Permission is granted to use this software under the terms of either:
   a) the GPL v2 (or any later version)
   b) the Affero GPL v3
   Details of these licenses can be found at: www.gnu.org/licenses
   JUCE is distributed in the hope that it will be useful, but WITHOUT ANY
   WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR
   A PARTICULAR PURPOSE.  See the GNU General Public License for more details.
   ------------------------------------------------------------------------------
   To release a closed-source product which uses JUCE, commercial licenses are
   available: visit www.juce.com for more information.
  ==============================================================================
*/

//
// The Interpolator class below is based on the LagrangeInterpolator class in JUCE, written by Julian Storer.
//
// I've changed it to use the Catmull-Rom spline interpolator instead (since I found that it sounded a lot better),
// plus that the 'process' function takes two extra parameters:
//
// 1. 'numOut': The maximum number of samples the 'process' method are allowed to write to 'out'.
// 2. 'numCounsumed': After 'process' is finished, this variable will contain the number of samples read from 'in'.
//
// In addition, the 'process' function now returns the number of frames put into 'out', while previously it returned the number
// of samples read from 'in'.
//
// My benchmarking shows that the degrade in performance compared to the original LagrangeInterpolator class is hardly measurable.
//
// -Kjetil Matheussen


#include <string.h>


// cubic interpolate function from http://paulbourke.net/miscellaneous/interpolation/
// Written by Paul Burke.
//
static float catmull_rom_spline(
                                float y0,float y1,
                                float y2,float y3,
                                float mu)
{
  // Optimized by faust [1]. Same number of multiplications though, so it's probably the same speed.
  float h_y0 = 0.5f * y0;
  float h_y3 = 0.5f * y3;
  return y1 + (mu * (((0.5f * y2) - h_y0) + (mu * (((y0 + (2 * y2)) - (h_y3 + (2.5f * y1))) + (mu * ((h_y3 + (1.5f * y1)) - ((1.5f * y2) + h_y0)))))));

  /*
[1]
process(y0,y1,y2,y3,mu) =  a0*mu*mu2 + a1*mu2 + a2*mu + a3 with{
   mu2 = mu*mu;
   a0 = -0.5*y0 + 1.5*y1 - 1.5*y2 + 0.5*y3;
   a1 = y0 - 2.5*y1 + 2*y2 - 0.5*y3;
   a2 = -0.5*y0 + 0.5*y2;
   a3 = y1;
};
  */
}



class Interpolator
{

private:
    float lastInputSamples[4];
    double subSamplePos;

  
public:
  
  void reset()
  {
    subSamplePos = 1.0;
    
    for (int i = 0; i < 4; ++i)
      lastInputSamples[i] = 0;
  }


private:

  void push (const float newValue)
  {
    lastInputSamples[3] = lastInputSamples[2];
    lastInputSamples[2] = lastInputSamples[1];
    lastInputSamples[1] = lastInputSamples[0];
    lastInputSamples[0] = newValue;
  }


  float interpolate(const float pos){
    return catmull_rom_spline(lastInputSamples[3], lastInputSamples[2], lastInputSamples[1], lastInputSamples[0], pos);
  }


  int process_no_interpolation(const float* in, const int numIn,
                               float* out, const int numOut,
                               int *numConsumed
                               )
  {
    int num_samples = R_MIN(numIn, numOut);
    
    memcpy (out, in, (size_t) num_samples * sizeof (float));
    
    if (num_samples >= 4) {
      
      const float* end = in + num_samples;
      
      for (int i = 0; i < 4; ++i)
        lastInputSamples[i] = *--end;
      
    } else {
      
      for (int i = 0; i < num_samples; ++i)
        push(in[i]);
    }
    
    *numConsumed = num_samples;
    return num_samples;
  }

  
public:

  int process (const double actualRatio,
               const float* in, const int numIn,
               float* out, const int numOut,
               int *numConsumed
               )
  {

    R_ASSERT_NON_RELEASE(numIn > 0);
    R_ASSERT_NON_RELEASE(numOut > 0);


    if (actualRatio > 0.9999 && actualRatio < 1.00001)
      return process_no_interpolation(in, numIn, out, numOut, numConsumed);

    
    double pos = subSamplePos;

    int num_produced = 0;
    int num_consumed = 0;
    
    if (actualRatio < 1.0) {
      
      for ( ; num_produced < numOut ; num_produced++){
        
        if (pos >= 1.0) {
          if (num_consumed == numIn)
            goto bail_out;
          
          push(in[num_consumed]);
          num_consumed++;
          
          pos -= 1.0;
        }
        
        out[num_produced] = interpolate(pos);
        
        pos += actualRatio;
      }
      
    } else {
      
      for ( ; num_produced < numOut ; num_produced++){
        
        while (pos < actualRatio) {
          if (num_consumed == numIn) // I've benchmarked running two different types of inner loops, depending on whether we would risk reading in[numIns].
                                     // The point was that the test on this line was removed in one of those inner loops.
                                     // However, the increase in performance was hardly measurable for larger input arrays (i.e. numOut >= 1024),
                                     // while for smaller input arrays the performance was significantly worse. -Kjetil
            goto bail_out;
          
          push(in[num_consumed]);
          num_consumed++;
          
          pos += 1.0;                
        }
        
        pos -= actualRatio; // I don't understand why pos handling is done differently when sampling down or up. And does it work properly if the ratio crosses 1.0 in a swipe?
        out[num_produced] = interpolate(1.0f - (float)pos);
      }
      
    }

    
bail_out:
    
    subSamplePos = pos;

    *numConsumed    = num_consumed;
    
    return num_produced;
  }

  
};


