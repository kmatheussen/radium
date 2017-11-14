#include <samplerate.h>

namespace radium{
  struct Resampler;
}

extern struct radium::Resampler *RESAMPLER_create(src_callback_t callback, int num_channels, void *arg, enum ResamplerType type);

extern void RESAMPLER_delete(struct radium::Resampler *res);

extern int RESAMPLER_read(struct radium::Resampler *res,double ratio,int num_frames, float *out);

extern void RESAMPLER_reset(struct radium::Resampler *res);


