#include <samplerate.h>

enum{
  RESAMPLER_NON=0,
  RESAMPLER_LINEAR=1,
  RESAMPLER_CUBIC=2,
  RESAMPLER_SINC1=3,
  RESAMPLER_SINC2=4
};
// (Both SincResampler() and the interpolation type widget depends on the order above)


extern LANGSPEC void *RESAMPLER_create(src_callback_t callback, int num_channels, void *arg, int type);

extern LANGSPEC void RESAMPLER_delete(void *res);

extern LANGSPEC int RESAMPLER_read(void *res,double ratio,int num_frames, float *out);

extern LANGSPEC void RESAMPLER_reset(void *res);

