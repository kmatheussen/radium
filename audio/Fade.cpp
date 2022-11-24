/*
  The code in this file might be the most CPU intensive code in the program.

  If using "-march=native -ffast-math", g++10 is slightly faster than clang++12.

  If only using "-ffast-math", clang++12 is slightly faster than g++10.
 */


#include "../common/nsmtracker.h"

#include "Fade.hpp"

#ifdef BENCHMARK_SMOOTHDELAY
extern double g_fade_benchmark_time;
#endif

namespace radium{

void Fade::RT_fade(const int num_frames, float *__restrict__ data, const float *__restrict in, bool add){
#ifdef BENCHMARK_SMOOTHDELAY
    const double time = TIME_get_ms();
#endif

    const int how_many = R_MIN(num_frames, _num_fade_frames_left);

    {
      const float fade_mul = _fade_mul;

      const double d_how_many = how_many;

      const double d_fade_span = _inc_fade*d_how_many;


#define USE_VECTORIZABLE_FADE_LOOP 1 // Approx. nine times faster in both gcc and clang! (clang 8.0.0 is ~11% faster than gcc 8.1.0 here)


#if USE_VECTORIZABLE_FADE_LOOP

      const float fade_inc = d_fade_span / d_how_many;

      if (in == NULL) {

#if !defined(RADIUM_USES_UBSAN)
        #pragma clang loop vectorize(enable) interleave(enable)
#endif
        for(int i=0;i<how_many;i++)
          data[i] *= fade_mul + i*fade_inc;

      } else {

        if (add) {
#if !defined(RADIUM_USES_UBSAN)
          #pragma clang loop vectorize(enable) interleave(enable)
#endif
          for(int i=0;i<how_many;i++)
            data[i] += in[i] * (fade_mul + i*fade_inc);

         } else {
#if !defined(RADIUM_USES_UBSAN)
           #pragma clang loop vectorize(enable) interleave(enable)
#endif
           for(int i=0;i<how_many;i++)
             data[i] = in[i] * (fade_mul + i*fade_inc);

         }

      }

      // same as:
      // for(int i=0;i<how_many;i++)
      //   data[i] *= R_SCALE(i, 0, how_many, fade_mul, fade_mul+fade_span);

#else

      R_ASSERT(in == NULL);
      R_ASSERT(add==false);

      for(int i=0;i<how_many;i++){
        data[i] *= fade_mul;
        fade_mul += _inc_fade;
      }

#endif

      _fade_mul += d_fade_span;
    }

#ifdef BENCHMARK_SMOOTHDELAY
    g_fade_benchmark_time += TIME_get_ms()-time;
#endif

    if (!_is_fading_in){
      const int extra = num_frames-how_many;
      if(extra > 0)
        memset(data + how_many, 0, extra*sizeof(float));
    }

    _num_fade_frames_left -= how_many;
  }
}
