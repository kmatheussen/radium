#include <samplerate.h>


extern LANGSPEC void *RESAMPLER_create(src_callback_t callback, int num_channels, void *arg, enum ResamplerType type);

extern LANGSPEC void RESAMPLER_delete(void *res);

extern LANGSPEC int RESAMPLER_read(void *res,double ratio,int num_frames, float *out);

extern LANGSPEC void RESAMPLER_reset(void *res);

