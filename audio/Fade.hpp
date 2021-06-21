#ifndef _RADIUM_AUDIO_FADE_HPP
#define _RADIUM_AUDIO_FADE_HPP

/*
  test: make test_smoothdelay
  behcmark: make benchmark_smoothdelay
 */

namespace radium{

class Fade{
  
  int _num_fade_frames_left;
  double _inc_fade;
  double _fade_mul;

  bool _is_fading_in;
#if !defined(RELEASE)
  const bool _fixed_fade_type;
#endif

public:

  Fade(const bool is_fading_in)
    : _is_fading_in(is_fading_in)
#if !defined(RELEASE)
    , _fixed_fade_type(true)
#endif
  {}

  Fade(void)
#if !defined(RELEASE)
    : _fixed_fade_type(false)
#endif
  {}

  bool is_fading_in(void) const {
    return _is_fading_in;
  }

  void RT_start_fading_in(int fade_len, int pos = 0){
    R_ASSERT_NON_RELEASE(!_fixed_fade_type || _is_fading_in);
    R_ASSERT_NON_RELEASE(pos <= fade_len);

    _num_fade_frames_left = fade_len - pos;
    _inc_fade = 1.0 / (double)(2 + fade_len); // Add 2 since we don't multiply by 0.0 and 1.0 in the fades.
    _fade_mul = _inc_fade * pos;
    _is_fading_in = true;
  }

  void RT_start_fading_out(int fade_len, int pos = 0){
    R_ASSERT_NON_RELEASE(!_fixed_fade_type || !_is_fading_in);
    R_ASSERT_NON_RELEASE(pos <= fade_len);

    _num_fade_frames_left = fade_len - pos;
    _inc_fade = - 1.0 / (double)(2 + fade_len); // Add 2 since we don't multiply by 0.0 and 1.0 in the fades.
    _fade_mul = 1.0 + _inc_fade * pos;
    _is_fading_in = false;
  }

  void RT_fade(int num_frames, float *__restrict__ data, const float *__restrict in = NULL, bool add = false){
#if TEST_SMOOTHDELAY
    double time = TIME_get_ms();
#endif

    int how_many = R_MIN(num_frames, _num_fade_frames_left);

    {
      float fade_mul = _fade_mul;

      double d_how_many = how_many;

      double d_fade_span = _inc_fade*d_how_many;


#define USE_VECTORIZABLE_FADE_LOOP 1 // Approx. nine times faster in both gcc and clang! (clang 8.0.0 is ~11% faster than gcc 8.1.0 here)


#if USE_VECTORIZABLE_FADE_LOOP

      float fade_inc = d_fade_span / d_how_many;

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

#if TEST_SMOOTHDELAY
    g_fade_benchmark_time += TIME_get_ms()-time;
#endif

    if (!_is_fading_in){
      int extra = num_frames-how_many;
      if(extra > 0)
        memset(data + how_many, 0, extra*sizeof(float));
    }

    _num_fade_frames_left -= how_many;
  }

  void RT_fade_in(int num_frames, float *__restrict__ data){
    R_ASSERT_NON_RELEASE(is_fading_in());
    return RT_fade(num_frames, data);
  }

  void RT_fade_out(int num_frames, float *__restrict__ data){
    R_ASSERT_NON_RELEASE(!is_fading_in());
    return RT_fade(num_frames, data);
  }

  bool RT_is_finished_fading(void) const {
    return _num_fade_frames_left==0;
  }
};

struct FadeIn : public Fade{
  FadeIn(void)
    : Fade(true)
  {}
};

struct FadeOut : public Fade{
  FadeOut(void)
    : Fade(false)
  {}
};
}



static inline void RT_fade_in(float *data, int num_frames, int pos1, int pos2){
  R_ASSERT_NON_RELEASE(pos1 >= 0);
  R_ASSERT_NON_RELEASE(num_frames > 0);
  
  R_ASSERT_NON_RELEASE(pos1 <= pos2);
  R_ASSERT_NON_RELEASE(pos1 >= 0);
  R_ASSERT_NON_RELEASE(pos2 <= num_frames);

  radium::FadeIn fader;

  fader.RT_start_fading_in(num_frames, pos1);
  
  fader.RT_fade(pos2-pos1, data);
}

static inline void RT_fade_in(float *data, int num_frames){
  RT_fade_in(data, num_frames, 0, num_frames);
}
 
static inline void RT_fade_in(float *__restrict__ out, const float *__restrict__ in, int num_frames, int pos1, int pos2, bool do_add = false){
  R_ASSERT_NON_RELEASE(pos1 >= 0);
  R_ASSERT_NON_RELEASE(num_frames > 0);
  
  R_ASSERT_NON_RELEASE(pos1 <= pos2);
  R_ASSERT_NON_RELEASE(pos1 >= 0);
  R_ASSERT_NON_RELEASE(pos2 <= num_frames);

  radium::FadeIn fader;

  fader.RT_start_fading_in(num_frames, pos1);
  
  fader.RT_fade(pos2-pos1, out, in, do_add);
}

static inline void RT_fade_in(float *__restrict__ out, const float *__restrict__ in, int num_frames){
  RT_fade_in(out, in, num_frames, 0, num_frames, false);
}

static inline void RT_fade_in_and_add(float *__restrict__ out, const float *__restrict__ in, int num_frames, int pos1, int pos2){
  RT_fade_in(out, in, num_frames, pos1, pos2, true);
}

static inline void RT_fade_in_and_add(float *__restrict__ out, const float *__restrict__ in, int num_frames){
  RT_fade_in_and_add(out, in, num_frames, 0, num_frames);
}


static inline void RT_fade_out(float *data, int num_frames, int pos1, int pos2){
  R_ASSERT_NON_RELEASE(pos1 >= 0);
  R_ASSERT_NON_RELEASE(num_frames > 0);
  
  R_ASSERT_NON_RELEASE(pos1 <= pos2);
  R_ASSERT_NON_RELEASE(pos1 >= 0);
  R_ASSERT_NON_RELEASE(pos2 <= num_frames);

  radium::FadeOut fader;

  fader.RT_start_fading_out(num_frames, pos1);
  
  fader.RT_fade(pos2-pos1, data);
}

static inline void RT_fade_out(float *data, int num_frames){
  RT_fade_out(data, num_frames, 0, num_frames);
}
 

static inline void RT_fade_out(float *__restrict__ out, const float *__restrict__ in, int num_frames, int pos1, int pos2, bool do_add = false){
  R_ASSERT_NON_RELEASE(pos1 >= 0);
  R_ASSERT_NON_RELEASE(num_frames > 0);
  
  R_ASSERT_NON_RELEASE(pos1 <= pos2);
  R_ASSERT_NON_RELEASE(pos1 >= 0);
  R_ASSERT_NON_RELEASE(pos2 <= num_frames);

  radium::FadeOut fader;

  fader.RT_start_fading_out(num_frames, pos1);
  
  fader.RT_fade(pos2-pos1, out, in, do_add);
}

static inline void RT_fade_out(float *__restrict__ out, const float *__restrict__ in, int num_frames){
  RT_fade_out(out, in, num_frames, 0, num_frames, false);
}

static inline void RT_fade_out_and_add(float *__restrict__ out, const float *__restrict__ in, int num_frames, int pos1, int pos2){
  RT_fade_out(out, in, num_frames, pos1, pos2, true);
}

static inline void RT_fade_out_and_add(float *__restrict__ out, const float *__restrict__ in, int num_frames){
  RT_fade_out_and_add(out, in, num_frames, 0, num_frames);
}


#endif
