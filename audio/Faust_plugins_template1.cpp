#include <math.h>
#include <string>

#include <vector>

#include "../common/nsmtracker.h"

static void RT_fade_out(float *sound, int num_frames){
  float num_frames_plus_1 = 1.0 / (num_frames+1.0f);
  for(int i=0;i<num_frames;i++)
    sound[i] *= (num_frames-i) * num_frames_plus_1;
}


#if 0
static float linear2db(float val){
  if(val<=0.0f)
    return 0.0f;

  float db = 20*log10(val);
  if(db<-70)
    return 0.0f;
  else if(db>40)
    return 1.0f;
  else
    return scale(db,-70,40,0,1);
}
#endif

#define MIN_LINEAR_VELOCITY 0.1
static constexpr float g_min_linear_gain = 0.001995; // = powf(10, R_SCALE(MIN_LINEAR_VELOCITY, 0.0, 1.0 ,-40, 20) / 20.0f) / 10.0f;

// input is between 0 and 1.
// output is between 0 and 1.
static float velocity2gain(float val){
#if 0
	g_min_linear_gain = powf(10, R_SCALE(MIN_LINEAR_VELOCITY, 0.0, 1.0 ,-40, 20) / 20.0f) / 10.0f;
	printf("%f\n",g_min_linear_gain);
	getchar();
#endif
	
  if(val<=0.0f)
    return 0.0f;
  else if(val>=1.0f)
    return 1.0f;
  else if (val < MIN_LINEAR_VELOCITY)
	  return scale(val,
		       0, MIN_LINEAR_VELOCITY,
		       0, g_min_linear_gain);
  else
    return powf(10, scale(val,0.0, 1.0 ,-40, 20) / 20.0f) / 10.0f;
}


#if 0
// For some reason, it won't compile with the usual min/max macros.
template<typename T> static inline T min(T a,T b){return a<b ? a : b;}
template<typename T> static inline T max(T a,T b){return a>b ? a : b;}
static inline float min(float a,int b){return a<b ? a : b;}
static inline float max(float a,int b){return a>b ? a : b;}
static inline float min(int a,float b){return a<b ? a : b;}
static inline float max(int a,float b){return a>b ? a : b;}
#endif

static inline int 	max (unsigned int a, unsigned int b) { return (a>b) ? a : b; }
static inline int 	max (int a, int b)		{ return (a>b) ? a : b; }

static inline long 	max (long a, long b) 		{ return (a>b) ? a : b; }
static inline long 	max (int a, long b) 		{ return (a>b) ? a : b; }
static inline long 	max (long a, int b) 		{ return (a>b) ? a : b; }

static inline float 	max (float a, float b) 		{ return (a>b) ? a : b; }
static inline float 	max (int a, float b) 		{ return (a>b) ? a : b; }
static inline float 	max (float a, int b) 		{ return (a>b) ? a : b; }
static inline float 	max (long a, float b) 		{ return (a>b) ? a : b; }
static inline float 	max (float a, long b) 		{ return (a>b) ? a : b; }

static inline double 	max (double a, double b) 	{ return (a>b) ? a : b; }
static inline double 	max (int a, double b) 		{ return (a>b) ? a : b; }
static inline double 	max (double a, int b) 		{ return (a>b) ? a : b; }
static inline double 	max (long a, double b) 		{ return (a>b) ? a : b; }
static inline double 	max (double a, long b) 		{ return (a>b) ? a : b; }
static inline double 	max (float a, double b) 	{ return (a>b) ? a : b; }
static inline double 	max (double a, float b) 	{ return (a>b) ? a : b; }


static inline int	min (int a, int b)		{ return (a<b) ? a : b; }

static inline long 	min (long a, long b) 		{ return (a<b) ? a : b; }
static inline long 	min (int a, long b) 		{ return (a<b) ? a : b; }
static inline long 	min (long a, int b) 		{ return (a<b) ? a : b; }

static inline float 	min (float a, float b) 		{ return (a<b) ? a : b; }
static inline float 	min (int a, float b) 		{ return (a<b) ? a : b; }
static inline float 	min (float a, int b) 		{ return (a<b) ? a : b; }
static inline float 	min (long a, float b) 		{ return (a<b) ? a : b; }
static inline float 	min (float a, long b) 		{ return (a<b) ? a : b; }

static inline double 	min (double a, double b) 	{ return (a<b) ? a : b; }
static inline double 	min (int a, double b) 		{ return (a<b) ? a : b; }
static inline double 	min (double a, int b) 		{ return (a<b) ? a : b; }
static inline double 	min (long a, double b) 		{ return (a<b) ? a : b; }
static inline double 	min (double a, long b) 		{ return (a<b) ? a : b; }
static inline double 	min (float a, double b) 	{ return (a<b) ? a : b; }
static inline double 	min (double a, float b) 	{ return (a<b) ? a : b; }

namespace
{
