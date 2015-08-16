//-----------------------------------------------------
//
// Code generated with Faust 0.9.58 (http://faust.grame.fr)
//-----------------------------------------------------
/* link with : "" */
#include "typepunning.h"
#include <math.h>
#include <cmath>
template <int N> inline float faustpower(float x) 		{ return powf(x,N); } 
template <int N> inline double faustpower(double x) 	{ return pow(x,N); }
template <int N> inline int faustpower(int x) 			{ return faustpower<N/2>(x) * faustpower<N-N/2>(x); } 
template <> 	 inline int faustpower<0>(int x) 		{ return 1; }
template <> 	 inline int faustpower<1>(int x) 		{ return x; }
#include <math.h>
#include <string>

#include <vector>

/*
#include "/usr/share/faust/audio/dsp.h"
#include "/usr/share/faust/gui/UI.h"
*/

#include "faudiostream/architecture/faust/audio/dsp.h"
#include "faudiostream/architecture/faust/gui/UI.h"

#include "../common/nsmtracker.h"

struct Meta
{
    void declare (const char* key, const char* value) { }
};

static void RT_fade_out(float *sound, int num_frames){
  float num_frames_plus_1 = num_frames+1.0f;
  for(int i=0;i<num_frames;i++)
    sound[i] *= (num_frames-i)/num_frames_plus_1;
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

// input is between 0 and 1.
// output is between 0 and 1.
static float velocity2gain(float val){
  if(val<=0.0f)
    return 0.0f;
  else if(val>=1.0f)
    return 1.0f;
  else
    return powf(10, scale(val,0.0, 1.0 ,-40, 20) / 20.0f) / 10.0f;
}


static double midi_to_hz(float midi){
  if(midi<=0)
    return 0;
  else
    //  return 1;
  return 8.17579891564*(expf(.0577622650*midi));
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

inline int 	max (unsigned int a, unsigned int b) { return (a>b) ? a : b; }
inline int 	max (int a, int b)		{ return (a>b) ? a : b; }

inline long 	max (long a, long b) 		{ return (a>b) ? a : b; }
inline long 	max (int a, long b) 		{ return (a>b) ? a : b; }
inline long 	max (long a, int b) 		{ return (a>b) ? a : b; }

inline float 	max (float a, float b) 		{ return (a>b) ? a : b; }
inline float 	max (int a, float b) 		{ return (a>b) ? a : b; }
inline float 	max (float a, int b) 		{ return (a>b) ? a : b; }
inline float 	max (long a, float b) 		{ return (a>b) ? a : b; }
inline float 	max (float a, long b) 		{ return (a>b) ? a : b; }

inline double 	max (double a, double b) 	{ return (a>b) ? a : b; }
inline double 	max (int a, double b) 		{ return (a>b) ? a : b; }
inline double 	max (double a, int b) 		{ return (a>b) ? a : b; }
inline double 	max (long a, double b) 		{ return (a>b) ? a : b; }
inline double 	max (double a, long b) 		{ return (a>b) ? a : b; }
inline double 	max (float a, double b) 	{ return (a>b) ? a : b; }
inline double 	max (double a, float b) 	{ return (a>b) ? a : b; }


inline int	min (int a, int b)		{ return (a<b) ? a : b; }

inline long 	min (long a, long b) 		{ return (a<b) ? a : b; }
inline long 	min (int a, long b) 		{ return (a<b) ? a : b; }
inline long 	min (long a, int b) 		{ return (a<b) ? a : b; }

inline float 	min (float a, float b) 		{ return (a<b) ? a : b; }
inline float 	min (int a, float b) 		{ return (a<b) ? a : b; }
inline float 	min (float a, int b) 		{ return (a<b) ? a : b; }
inline float 	min (long a, float b) 		{ return (a<b) ? a : b; }
inline float 	min (float a, long b) 		{ return (a<b) ? a : b; }

inline double 	min (double a, double b) 	{ return (a<b) ? a : b; }
inline double 	min (int a, double b) 		{ return (a<b) ? a : b; }
inline double 	min (double a, int b) 		{ return (a<b) ? a : b; }
inline double 	min (long a, double b) 		{ return (a<b) ? a : b; }
inline double 	min (double a, long b) 		{ return (a<b) ? a : b; }
inline double 	min (float a, double b) 	{ return (a<b) ? a : b; }
inline double 	min (double a, float b) 	{ return (a<b) ? a : b; }


/******************************************************************************
*******************************************************************************

							       VECTOR INTRINSICS

*******************************************************************************
*******************************************************************************/


/******************************************************************************
*******************************************************************************

			ABSTRACT USER INTERFACE

*******************************************************************************
*******************************************************************************/

//----------------------------------------------------------------------------
//  FAUST generated signal processor
//----------------------------------------------------------------------------

#ifndef FAUSTFLOAT
#define FAUSTFLOAT float
#endif  

typedef long double quad;

#ifndef FAUSTCLASS 
#define FAUSTCLASS Multibandcomp_dsp
#endif

class Multibandcomp_dsp : public dsp {
  private:
	FAUSTFLOAT 	fslider0;
	int 	iConst0;
	float 	fConst1;
	float 	fYec0_perm[4];
	float 	fRec3_perm[4];
	float 	fRec2_perm[4];
	FAUSTFLOAT 	fslider1;
	float 	fYec1_perm[4];
	float 	fRec1_perm[4];
	float 	fRec0_perm[4];
	float 	fYec2_perm[4];
	float 	fRec10_perm[4];
	float 	fRec9_perm[4];
	float 	fYec3_perm[4];
	float 	fRec8_perm[4];
	float 	fRec7_perm[4];
	FAUSTFLOAT 	fcheckbox0;
	FAUSTFLOAT 	fslider2;
	float 	fConst2;
	FAUSTFLOAT 	fslider3;
	float 	fRec6_perm[4];
	FAUSTFLOAT 	fslider4;
	float 	fRec5_perm[4];
	float 	fConst3;
	FAUSTFLOAT 	fslider5;
	FAUSTFLOAT 	fslider6;
	float 	fRec4_perm[4];
	float 	fRec13_perm[4];
	float 	fRec12_perm[4];
	float 	fRec11_perm[4];
	float 	fRec19_perm[4];
	float 	fRec18_perm[4];
	float 	fRec17_perm[4];
	FAUSTFLOAT 	fcheckbox1;
	FAUSTFLOAT 	fslider7;
	FAUSTFLOAT 	fslider8;
	float 	fRec16_perm[4];
	FAUSTFLOAT 	fslider9;
	float 	fRec15_perm[4];
	FAUSTFLOAT 	fslider10;
	FAUSTFLOAT 	fslider11;
	float 	fRec14_perm[4];
	float 	fRec21_perm[4];
	float 	fRec20_perm[4];
	float 	fRec26_perm[4];
	float 	fRec25_perm[4];
	FAUSTFLOAT 	fcheckbox2;
	FAUSTFLOAT 	fslider12;
	FAUSTFLOAT 	fslider13;
	float 	fRec24_perm[4];
	FAUSTFLOAT 	fslider14;
	float 	fRec23_perm[4];
	FAUSTFLOAT 	fslider15;
	FAUSTFLOAT 	fslider16;
	float 	fRec22_perm[4];
	float 	fRec32_perm[4];
	float 	fRec31_perm[4];
	float 	fRec30_perm[4];
	float 	fRec35_perm[4];
	float 	fRec34_perm[4];
	float 	fRec33_perm[4];
	float 	fRec38_perm[4];
	float 	fRec37_perm[4];
	float 	fRec36_perm[4];
	FAUSTFLOAT 	fslider17;
	FAUSTFLOAT 	fcheckbox3;
	FAUSTFLOAT 	fcheckbox4;
	FAUSTFLOAT 	fcheckbox5;
	FAUSTFLOAT 	fslider18;
	FAUSTFLOAT 	fslider19;
	FAUSTFLOAT 	fcheckbox6;
	FAUSTFLOAT 	fslider20;
	FAUSTFLOAT 	fbargraph0;
	FAUSTFLOAT 	fbargraph1;
	FAUSTFLOAT 	fbargraph2;
	FAUSTFLOAT 	fbargraph3;
	FAUSTFLOAT 	fbargraph4;
	FAUSTFLOAT 	fbargraph5;
	FAUSTFLOAT 	fslider21;
	float 	fConst4;
	float 	fRec29_perm[4];
	FAUSTFLOAT 	fslider22;
	float 	fConst5;
	float 	fRec28_perm[4];
	float 	fConst6;
	FAUSTFLOAT 	fslider23;
	float 	fRec27_perm[4];
	FAUSTFLOAT 	fslider24;
	FAUSTFLOAT 	fslider25;
  public:
	static void metadata(Meta* m) 	{ 
		m->declare("filter.lib/name", "Faust Filter Library");
		m->declare("filter.lib/author", "Julius O. Smith (jos at ccrma.stanford.edu)");
		m->declare("filter.lib/copyright", "Julius O. Smith III");
		m->declare("filter.lib/version", "1.29");
		m->declare("filter.lib/license", "STK-4.3");
		m->declare("filter.lib/reference", "https://ccrma.stanford.edu/~jos/filters/");
		m->declare("music.lib/name", "Music Library");
		m->declare("music.lib/author", "GRAME");
		m->declare("music.lib/copyright", "GRAME");
		m->declare("music.lib/version", "1.0");
		m->declare("music.lib/license", "LGPL with exception");
		m->declare("math.lib/name", "Math Library");
		m->declare("math.lib/author", "GRAME");
		m->declare("math.lib/copyright", "GRAME");
		m->declare("math.lib/version", "1.0");
		m->declare("math.lib/license", "LGPL with exception");
		m->declare("effect.lib/name", "Faust Audio Effect Library");
		m->declare("effect.lib/author", "Julius O. Smith (jos at ccrma.stanford.edu)");
		m->declare("effect.lib/copyright", "Julius O. Smith III");
		m->declare("effect.lib/version", "1.33");
		m->declare("effect.lib/license", "STK-4.3");
	}

	virtual int getNumInputs() 	{ return 2; }
	virtual int getNumOutputs() 	{ return 2; }
	static void classInit(int samplingFreq) {
	}
	virtual void instanceInit(int samplingFreq) {
		fSamplingFreq = samplingFreq;
		fslider0 = 1.5e+03f;
		iConst0 = min(192000, max(1, fSamplingFreq));
		fConst1 = (3.141592653589793f / float(iConst0));
		for (int i=0; i<4; i++) fYec0_perm[i]=0;
		for (int i=0; i<4; i++) fRec3_perm[i]=0;
		for (int i=0; i<4; i++) fRec2_perm[i]=0;
		fslider1 = 166.0f;
		for (int i=0; i<4; i++) fYec1_perm[i]=0;
		for (int i=0; i<4; i++) fRec1_perm[i]=0;
		for (int i=0; i<4; i++) fRec0_perm[i]=0;
		for (int i=0; i<4; i++) fYec2_perm[i]=0;
		for (int i=0; i<4; i++) fRec10_perm[i]=0;
		for (int i=0; i<4; i++) fRec9_perm[i]=0;
		for (int i=0; i<4; i++) fYec3_perm[i]=0;
		for (int i=0; i<4; i++) fRec8_perm[i]=0;
		for (int i=0; i<4; i++) fRec7_perm[i]=0;
		fcheckbox0 = 0.0;
		fslider2 = 0.0f;
		fConst2 = (1.0f / float(iConst0));
		fslider3 = 2e+02f;
		for (int i=0; i<4; i++) fRec6_perm[i]=0;
		fslider4 = 1e+02f;
		for (int i=0; i<4; i++) fRec5_perm[i]=0;
		fConst3 = (2.0f / float(iConst0));
		fslider5 = -2e+01f;
		fslider6 = 2.0f;
		for (int i=0; i<4; i++) fRec4_perm[i]=0;
		for (int i=0; i<4; i++) fRec13_perm[i]=0;
		for (int i=0; i<4; i++) fRec12_perm[i]=0;
		for (int i=0; i<4; i++) fRec11_perm[i]=0;
		for (int i=0; i<4; i++) fRec19_perm[i]=0;
		for (int i=0; i<4; i++) fRec18_perm[i]=0;
		for (int i=0; i<4; i++) fRec17_perm[i]=0;
		fcheckbox1 = 0.0;
		fslider7 = 0.0f;
		fslider8 = 2e+02f;
		for (int i=0; i<4; i++) fRec16_perm[i]=0;
		fslider9 = 1e+02f;
		for (int i=0; i<4; i++) fRec15_perm[i]=0;
		fslider10 = -2e+01f;
		fslider11 = 2.0f;
		for (int i=0; i<4; i++) fRec14_perm[i]=0;
		for (int i=0; i<4; i++) fRec21_perm[i]=0;
		for (int i=0; i<4; i++) fRec20_perm[i]=0;
		for (int i=0; i<4; i++) fRec26_perm[i]=0;
		for (int i=0; i<4; i++) fRec25_perm[i]=0;
		fcheckbox2 = 0.0;
		fslider12 = 0.0f;
		fslider13 = 2e+02f;
		for (int i=0; i<4; i++) fRec24_perm[i]=0;
		fslider14 = 1e+02f;
		for (int i=0; i<4; i++) fRec23_perm[i]=0;
		fslider15 = -2e+01f;
		fslider16 = 2.0f;
		for (int i=0; i<4; i++) fRec22_perm[i]=0;
		for (int i=0; i<4; i++) fRec32_perm[i]=0;
		for (int i=0; i<4; i++) fRec31_perm[i]=0;
		for (int i=0; i<4; i++) fRec30_perm[i]=0;
		for (int i=0; i<4; i++) fRec35_perm[i]=0;
		for (int i=0; i<4; i++) fRec34_perm[i]=0;
		for (int i=0; i<4; i++) fRec33_perm[i]=0;
		for (int i=0; i<4; i++) fRec38_perm[i]=0;
		for (int i=0; i<4; i++) fRec37_perm[i]=0;
		for (int i=0; i<4; i++) fRec36_perm[i]=0;
		fslider17 = 0.0f;
		fcheckbox3 = 0.0;
		fcheckbox4 = 0.0;
		fcheckbox5 = 0.0;
		fslider18 = 0.0f;
		fslider19 = 0.0f;
		fcheckbox6 = 0.0;
		fslider20 = 0.0f;
		fslider21 = 5e+02f;
		fConst4 = (1e+03f / float(iConst0));
		for (int i=0; i<4; i++) fRec29_perm[i]=0;
		fslider22 = 8e+02f;
		fConst5 = (1e+06f / float(iConst0));
		for (int i=0; i<4; i++) fRec28_perm[i]=0;
		fConst6 = (2e+06f / float(iConst0));
		fslider23 = 4.0f;
		for (int i=0; i<4; i++) fRec27_perm[i]=0;
		fslider24 = 0.0f;
		fslider25 = 0.0f;
	}
	virtual void init(int samplingFreq) {
		classInit(samplingFreq);
		instanceInit(samplingFreq);
	}
	virtual void buildUserInterface(UI* interface) {
		interface->openVerticalBox("faust_multibandcomp");
		interface->declare(&fcheckbox5, "0", "");
		interface->declare(&fcheckbox5, "tooltip", "When this is checked, the compressor is enabled. If not, sound is muted.");
		interface->addCheckButton("Band 1:  Solo", &fcheckbox5);
		interface->declare(&fbargraph5, "1", "");
		interface->declare(&fbargraph5, "7", "");
		interface->addHorizontalBargraph("Band 1:   Outgain", &fbargraph5, 0.0f, 1.0f);
		interface->declare(&fcheckbox0, "0.5", "");
		interface->declare(&fcheckbox0, "tooltip", "When this is checked, the compressor is enabled. If not, sound is muted.");
		interface->addCheckButton("Band 1: Bypass", &fcheckbox0);
		interface->declare(&fslider6, "2", "");
		interface->declare(&fslider6, "style", "slider");
		interface->declare(&fslider6, "tooltip", "A compression Ratio of N means that for each N dB increase in input signal level above Threshold, the output level goes up 1 dB");
		interface->addHorizontalSlider("Band 1: Ratio", &fslider6, 2.0f, 1.0f, 2e+01f, 0.1f);
		interface->declare(&fslider5, "3", "");
		interface->declare(&fslider5, "style", "slider");
		interface->declare(&fslider5, "tooltip", "When the signal level exceeds the Threshold (in dB), its level is compressed according to the Ratio");
		interface->declare(&fslider5, "unit", "dB");
		interface->addHorizontalSlider("Band 1: Threshold", &fslider5, -2e+01f, -1e+02f, 1e+01f, 0.1f);
		interface->declare(&fslider4, "4", "");
		interface->declare(&fslider4, "style", "slider");
		interface->declare(&fslider4, "tooltip", "Time constant in ms (1/e smoothing time) for the compression gain to approach (exponentially) a new lower target level (the compression `kicking in')");
		interface->declare(&fslider4, "unit", "ms");
		interface->addHorizontalSlider("Band 1: Attack", &fslider4, 1e+02f, 0.0f, 5e+02f, 0.1f);
		interface->declare(&fslider3, "5", "");
		interface->declare(&fslider3, "style", "slider");
		interface->declare(&fslider3, "tooltip", "Time constant in ms (1/e smoothing time) for the compression gain to approach (exponentially) a new higher target level (the compression 'releasing')");
		interface->declare(&fslider3, "unit", "ms");
		interface->addHorizontalSlider("Band 1: Release", &fslider3, 2e+02f, 0.0f, 1e+03f, 0.1f);
		interface->declare(&fbargraph4, "1", "");
		interface->declare(&fbargraph4, "6", "");
		interface->declare(&fbargraph4, "tooltip", "dummy tooltip");
		interface->addHorizontalBargraph("Band 1: Input Gain", &fbargraph4, 0.0f, 1.0f);
		interface->declare(&fslider2, "2", "");
		interface->declare(&fslider2, "6", "");
		interface->declare(&fslider2, "tooltip", "The compressed-signal output level is increased by this amount (in dB) to make up for the level lost due to compression");
		interface->declare(&fslider2, "unit", "dB");
		interface->addHorizontalSlider("Band 1: Input Gain", &fslider2, 0.0f, -4e+01f, 4e+01f, 0.1f);
		interface->declare(&fslider19, "2", "");
		interface->declare(&fslider19, "7", "");
		interface->declare(&fslider19, "tooltip", "The compressed-signal output level is increased by this amount (in dB) to make up for the level lost due to compression");
		interface->declare(&fslider19, "unit", "dB");
		interface->addHorizontalSlider("Band 1: Output Gain", &fslider19, 0.0f, -4e+01f, 4e+01f, 0.1f);
		interface->declare(&fcheckbox4, "0", "");
		interface->declare(&fcheckbox4, "tooltip", "When this is checked, the compressor is enabled. If not, sound is muted.");
		interface->addCheckButton("Band 2:  Solo", &fcheckbox4);
		interface->declare(&fbargraph1, "1", "");
		interface->declare(&fbargraph1, "7", "");
		interface->addHorizontalBargraph("Band 2:   Outgain", &fbargraph1, 0.0f, 1.0f);
		interface->declare(&fcheckbox2, "0.5", "");
		interface->declare(&fcheckbox2, "tooltip", "When this is checked, the compressor is enabled. If not, sound is muted.");
		interface->addCheckButton("Band 2: Bypass", &fcheckbox2);
		interface->declare(&fslider16, "2", "");
		interface->declare(&fslider16, "style", "slider");
		interface->declare(&fslider16, "tooltip", "A compression Ratio of N means that for each N dB increase in input signal level above Threshold, the output level goes up 1 dB");
		interface->addHorizontalSlider("Band 2: Ratio", &fslider16, 2.0f, 1.0f, 2e+01f, 0.1f);
		interface->declare(&fslider15, "3", "");
		interface->declare(&fslider15, "style", "slider");
		interface->declare(&fslider15, "tooltip", "When the signal level exceeds the Threshold (in dB), its level is compressed according to the Ratio");
		interface->declare(&fslider15, "unit", "dB");
		interface->addHorizontalSlider("Band 2: Threshold", &fslider15, -2e+01f, -1e+02f, 1e+01f, 0.1f);
		interface->declare(&fslider14, "4", "");
		interface->declare(&fslider14, "style", "slider");
		interface->declare(&fslider14, "tooltip", "Time constant in ms (1/e smoothing time) for the compression gain to approach (exponentially) a new lower target level (the compression `kicking in')");
		interface->declare(&fslider14, "unit", "ms");
		interface->addHorizontalSlider("Band 2: Attack", &fslider14, 1e+02f, 0.0f, 5e+02f, 0.1f);
		interface->declare(&fslider13, "5", "");
		interface->declare(&fslider13, "style", "slider");
		interface->declare(&fslider13, "tooltip", "Time constant in ms (1/e smoothing time) for the compression gain to approach (exponentially) a new higher target level (the compression 'releasing')");
		interface->declare(&fslider13, "unit", "ms");
		interface->addHorizontalSlider("Band 2: Release", &fslider13, 2e+02f, 0.0f, 1e+03f, 0.1f);
		interface->declare(&fbargraph0, "1", "");
		interface->declare(&fbargraph0, "6", "");
		interface->declare(&fbargraph0, "tooltip", "dummy tooltip");
		interface->addHorizontalBargraph("Band 2: Input Gain", &fbargraph0, 0.0f, 1.0f);
		interface->declare(&fslider12, "2", "");
		interface->declare(&fslider12, "6", "");
		interface->declare(&fslider12, "tooltip", "The compressed-signal output level is increased by this amount (in dB) to make up for the level lost due to compression");
		interface->declare(&fslider12, "unit", "dB");
		interface->addHorizontalSlider("Band 2: Input Gain", &fslider12, 0.0f, -4e+01f, 4e+01f, 0.1f);
		interface->declare(&fslider17, "2", "");
		interface->declare(&fslider17, "7", "");
		interface->declare(&fslider17, "tooltip", "The compressed-signal output level is increased by this amount (in dB) to make up for the level lost due to compression");
		interface->declare(&fslider17, "unit", "dB");
		interface->addHorizontalSlider("Band 2: Output Gain", &fslider17, 0.0f, -4e+01f, 4e+01f, 0.1f);
		interface->declare(&fcheckbox3, "0", "");
		interface->declare(&fcheckbox3, "tooltip", "When this is checked, the compressor is enabled. If not, sound is muted.");
		interface->addCheckButton("Band 3:  Solo", &fcheckbox3);
		interface->declare(&fbargraph3, "1", "");
		interface->declare(&fbargraph3, "7", "");
		interface->addHorizontalBargraph("Band 3:   Outgain", &fbargraph3, 0.0f, 1.0f);
		interface->declare(&fcheckbox1, "0.5", "");
		interface->declare(&fcheckbox1, "tooltip", "When this is checked, the compressor is enabled. If not, sound is muted.");
		interface->addCheckButton("Band 3: Bypass", &fcheckbox1);
		interface->declare(&fslider11, "2", "");
		interface->declare(&fslider11, "style", "slider");
		interface->declare(&fslider11, "tooltip", "A compression Ratio of N means that for each N dB increase in input signal level above Threshold, the output level goes up 1 dB");
		interface->addHorizontalSlider("Band 3: Ratio", &fslider11, 2.0f, 1.0f, 2e+01f, 0.1f);
		interface->declare(&fslider10, "3", "");
		interface->declare(&fslider10, "style", "slider");
		interface->declare(&fslider10, "tooltip", "When the signal level exceeds the Threshold (in dB), its level is compressed according to the Ratio");
		interface->declare(&fslider10, "unit", "dB");
		interface->addHorizontalSlider("Band 3: Threshold", &fslider10, -2e+01f, -1e+02f, 1e+01f, 0.1f);
		interface->declare(&fslider9, "4", "");
		interface->declare(&fslider9, "style", "slider");
		interface->declare(&fslider9, "tooltip", "Time constant in ms (1/e smoothing time) for the compression gain to approach (exponentially) a new lower target level (the compression `kicking in')");
		interface->declare(&fslider9, "unit", "ms");
		interface->addHorizontalSlider("Band 3: Attack", &fslider9, 1e+02f, 0.0f, 5e+02f, 0.1f);
		interface->declare(&fslider8, "5", "");
		interface->declare(&fslider8, "style", "slider");
		interface->declare(&fslider8, "tooltip", "Time constant in ms (1/e smoothing time) for the compression gain to approach (exponentially) a new higher target level (the compression 'releasing')");
		interface->declare(&fslider8, "unit", "ms");
		interface->addHorizontalSlider("Band 3: Release", &fslider8, 2e+02f, 0.0f, 1e+03f, 0.1f);
		interface->declare(&fbargraph2, "1", "");
		interface->declare(&fbargraph2, "6", "");
		interface->declare(&fbargraph2, "tooltip", "dummy tooltip");
		interface->addHorizontalBargraph("Band 3: Input Gain", &fbargraph2, 0.0f, 1.0f);
		interface->declare(&fslider7, "2", "");
		interface->declare(&fslider7, "6", "");
		interface->declare(&fslider7, "tooltip", "The compressed-signal output level is increased by this amount (in dB) to make up for the level lost due to compression");
		interface->declare(&fslider7, "unit", "dB");
		interface->addHorizontalSlider("Band 3: Input Gain", &fslider7, 0.0f, -4e+01f, 4e+01f, 0.1f);
		interface->declare(&fslider18, "2", "");
		interface->declare(&fslider18, "7", "");
		interface->declare(&fslider18, "tooltip", "The compressed-signal output level is increased by this amount (in dB) to make up for the level lost due to compression");
		interface->declare(&fslider18, "unit", "dB");
		interface->addHorizontalSlider("Band 3: Output Gain", &fslider18, 0.0f, -4e+01f, 4e+01f, 0.1f);
		interface->declare(&fslider1, "C", "");
		interface->declare(&fslider1, "style", "knob");
		interface->declare(&fslider1, "tooltip", "Center-frequency of second-order Regalia-Mitra peaking equalizer section 1");
		interface->declare(&fslider1, "unit", "Hz");
		interface->addVerticalSlider("Split Freq 1", &fslider1, 166.0f, 4e+01f, 999.0f, 1.0f);
		interface->declare(&fslider0, "D", "");
		interface->declare(&fslider0, "style", "knob");
		interface->declare(&fslider0, "tooltip", "Center-frequency of second-order Regalia-Mitra peaking equalizer section 1");
		interface->declare(&fslider0, "unit", "Hz");
		interface->addVerticalSlider("Split Freq 2", &fslider0, 1.5e+03f, 1e+03f, 1.5e+04f, 1.0f);
		interface->declare(&fcheckbox6, "E", "");
		interface->addCheckButton("Limiter Bypass", &fcheckbox6);
		interface->declare(&fslider20, "F", "");
		interface->declare(&fslider20, "tooltip", "Adjust overall gain.");
		interface->declare(&fslider20, "unit", "dB");
		interface->addHorizontalSlider("Limiter Input Gain", &fslider20, 0.0f, -4e+01f, 4e+01f, 0.1f);
		interface->declare(&fslider23, "G", "");
		interface->declare(&fslider23, "unit", ":1");
		interface->addVerticalSlider("Limiter Ratio", &fslider23, 4.0f, 4.0f, 2e+01f, 1.0f);
		interface->declare(&fslider22, "H", "");
		interface->declare(&fslider22, "unit", "us");
		interface->addVerticalSlider("Limiter Attack", &fslider22, 8e+02f, 2e+01f, 8e+02f, 1.0f);
		interface->declare(&fslider21, "I", "");
		interface->declare(&fslider21, "unit", "ms");
		interface->addVerticalSlider("Limiter Release", &fslider21, 5e+02f, 5e+01f, 1.1e+03f, 1.0f);
		interface->declare(&fslider24, "J", "");
		interface->declare(&fslider24, "tooltip", "Adjust overall gain.");
		interface->declare(&fslider24, "unit", "dB");
		interface->addHorizontalSlider("Limiter Output Gain", &fslider24, 0.0f, -4e+01f, 4e+01f, 0.1f);
		interface->declare(&fslider25, "K", "");
		interface->declare(&fslider25, "tooltip", "Adjust overall gain.");
		interface->declare(&fslider25, "unit", "dB");
		interface->addHorizontalSlider("Final Output Gain", &fslider25, 0.0f, -4e+01f, 4e+01f, 0.1f);
		interface->closeBox();
	}
	virtual void compute (int fullcount, FAUSTFLOAT** input, FAUSTFLOAT** output) {
		float 	fYec0_tmp[32+4];
		float 	fRec3_tmp[32+4];
		float 	fRec2_tmp[32+4];
		float 	fZec0[32];
		float 	fYec1_tmp[32+4];
		float 	fRec1_tmp[32+4];
		float 	fRec0_tmp[32+4];
		float 	fYec2_tmp[32+4];
		float 	fRec10_tmp[32+4];
		float 	fRec9_tmp[32+4];
		float 	fZec1[32];
		float 	fYec3_tmp[32+4];
		float 	fRec8_tmp[32+4];
		float 	fRec7_tmp[32+4];
		float 	fZec2[32];
		float 	fZec3[32];
		float 	fZec4[32];
		float 	fZec5[32];
		float 	fZec6[32];
		float 	fRec6_tmp[32+4];
		float 	fRec5_tmp[32+4];
		float 	fRec4_tmp[32+4];
		float 	fRec13_tmp[32+4];
		float 	fRec12_tmp[32+4];
		float 	fZec7[32];
		float 	fRec11_tmp[32+4];
		float 	fRec19_tmp[32+4];
		float 	fRec18_tmp[32+4];
		float 	fZec8[32];
		float 	fRec17_tmp[32+4];
		float 	fZec9[32];
		float 	fZec10[32];
		float 	fZec11[32];
		float 	fZec12[32];
		float 	fZec13[32];
		float 	fRec16_tmp[32+4];
		float 	fRec15_tmp[32+4];
		float 	fRec14_tmp[32+4];
		float 	fRec21_tmp[32+4];
		float 	fRec20_tmp[32+4];
		float 	fRec26_tmp[32+4];
		float 	fRec25_tmp[32+4];
		float 	fZec14[32];
		float 	fZec15[32];
		float 	fZec16[32];
		float 	fZec17[32];
		float 	fZec18[32];
		float 	fRec24_tmp[32+4];
		float 	fRec23_tmp[32+4];
		float 	fRec22_tmp[32+4];
		float 	fZec19[32];
		float 	fZec20[32];
		float 	fZec21[32];
		float 	fZec22[32];
		float 	fRec32_tmp[32+4];
		float 	fRec31_tmp[32+4];
		float 	fRec30_tmp[32+4];
		float 	fZec23[32];
		float 	fZec24[32];
		float 	fZec25[32];
		float 	fZec26[32];
		float 	fRec35_tmp[32+4];
		float 	fRec34_tmp[32+4];
		float 	fRec33_tmp[32+4];
		float 	fZec27[32];
		float 	fZec28[32];
		float 	fZec29[32];
		float 	fZec30[32];
		float 	fRec38_tmp[32+4];
		float 	fRec37_tmp[32+4];
		float 	fRec36_tmp[32+4];
		float 	fZec31[32];
		float 	fZec32[32];
		float 	fZec33[32];
		float 	fZec34[32];
		float 	fZec35[32];
		float 	fZec36[32];
		float 	fZec37[32];
		float 	fZec38[32];
		float 	fZec39[32];
		float 	fZec40[32];
		float 	fZec41[32];
		float 	fRec29_tmp[32+4];
		float 	fRec28_tmp[32+4];
		float 	fRec27_tmp[32+4];
		float 	fZec42[32];
		float 	fSlow0 = tanf((fConst1 * fslider0));
		float 	fSlow1 = (1.0f / fSlow0);
		float 	fSlow2 = (1 + fSlow1);
		float 	fSlow3 = (0 - ((1 - fSlow1) / fSlow2));
		float* 	fYec0 = &fYec0_tmp[4];
		float 	fSlow4 = (1.0f / fSlow2);
		float* 	fRec3 = &fRec3_tmp[4];
		float 	fSlow5 = (1.0f / faustpower<2>(fSlow0));
		float 	fSlow6 = (2 * (1 - fSlow5));
		float 	fSlow7 = (1 + ((fSlow1 - 1.0000000000000004f) / fSlow0));
		float 	fSlow8 = (1 + ((1.0000000000000004f + fSlow1) / fSlow0));
		float 	fSlow9 = (1.0f / fSlow8);
		float* 	fRec2 = &fRec2_tmp[4];
		float 	fSlow10 = tanf((fConst1 * fslider1));
		float 	fSlow11 = (1.0f / fSlow10);
		float 	fSlow12 = (1 + fSlow11);
		float 	fSlow13 = (0 - ((1 - fSlow11) / fSlow12));
		float* 	fYec1 = &fYec1_tmp[4];
		float 	fSlow14 = (1.0f / fSlow12);
		float* 	fRec1 = &fRec1_tmp[4];
		float 	fSlow15 = (1.0f / faustpower<2>(fSlow10));
		float 	fSlow16 = (2 * (1 - fSlow15));
		float 	fSlow17 = (1 + ((fSlow11 - 1.0000000000000004f) / fSlow10));
		float 	fSlow18 = (1.0f / (1 + ((fSlow11 + 1.0000000000000004f) / fSlow10)));
		float* 	fRec0 = &fRec0_tmp[4];
		float* 	fYec2 = &fYec2_tmp[4];
		float* 	fRec10 = &fRec10_tmp[4];
		float* 	fRec9 = &fRec9_tmp[4];
		float* 	fYec3 = &fYec3_tmp[4];
		float* 	fRec8 = &fRec8_tmp[4];
		float* 	fRec7 = &fRec7_tmp[4];
		int 	iSlow19 = int(fcheckbox0);
		float 	fSlow20 = powf(10,(0.05f * fslider2));
		float 	fSlow21 = expf((0 - (fConst2 / max(fConst2, (0.001f * fslider3)))));
		float 	fSlow22 = (1.0f - fSlow21);
		float* 	fRec6 = &fRec6_tmp[4];
		float 	fSlow23 = max(fConst2, (0.001f * fslider4));
		float 	fSlow24 = expf((0 - (fConst2 / fSlow23)));
		float 	fSlow25 = (1.0f - fSlow24);
		float* 	fRec5 = &fRec5_tmp[4];
		float 	fSlow26 = expf((0 - (fConst3 / fSlow23)));
		float 	fSlow27 = fslider5;
		float 	fSlow28 = (((1.0f / float(fslider6)) - 1.0f) * (1.0f - fSlow26));
		float* 	fRec4 = &fRec4_tmp[4];
		float 	fSlow29 = (0 - fSlow1);
		float* 	fRec13 = &fRec13_tmp[4];
		float* 	fRec12 = &fRec12_tmp[4];
		float 	fSlow30 = (1 + ((fSlow11 - 1.0f) / fSlow10));
		float 	fSlow31 = (1.0f / (1 + ((1.0f + fSlow11) / fSlow10)));
		float 	fSlow32 = (2 * (0 - fSlow5));
		float* 	fRec11 = &fRec11_tmp[4];
		float* 	fRec19 = &fRec19_tmp[4];
		float* 	fRec18 = &fRec18_tmp[4];
		float* 	fRec17 = &fRec17_tmp[4];
		int 	iSlow33 = int(fcheckbox1);
		float 	fSlow34 = powf(10,(0.05f * fslider7));
		float 	fSlow35 = expf((0 - (fConst2 / max(fConst2, (0.001f * fslider8)))));
		float 	fSlow36 = (1.0f - fSlow35);
		float* 	fRec16 = &fRec16_tmp[4];
		float 	fSlow37 = max(fConst2, (0.001f * fslider9));
		float 	fSlow38 = expf((0 - (fConst2 / fSlow37)));
		float 	fSlow39 = (1.0f - fSlow38);
		float* 	fRec15 = &fRec15_tmp[4];
		float 	fSlow40 = expf((0 - (fConst3 / fSlow37)));
		float 	fSlow41 = fslider10;
		float 	fSlow42 = (((1.0f / float(fslider11)) - 1.0f) * (1.0f - fSlow40));
		float* 	fRec14 = &fRec14_tmp[4];
		float 	fSlow43 = (1.0f / (fSlow10 * fSlow8));
		float 	fSlow44 = (0 - fSlow11);
		float* 	fRec21 = &fRec21_tmp[4];
		float* 	fRec20 = &fRec20_tmp[4];
		float* 	fRec26 = &fRec26_tmp[4];
		float* 	fRec25 = &fRec25_tmp[4];
		float 	fSlow45 = (2 * (0 - fSlow15));
		int 	iSlow46 = int(fcheckbox2);
		float 	fSlow47 = powf(10,(0.05f * fslider12));
		float 	fSlow48 = expf((0 - (fConst2 / max(fConst2, (0.001f * fslider13)))));
		float 	fSlow49 = (1.0f - fSlow48);
		float* 	fRec24 = &fRec24_tmp[4];
		float 	fSlow50 = max(fConst2, (0.001f * fslider14));
		float 	fSlow51 = expf((0 - (fConst2 / fSlow50)));
		float 	fSlow52 = (1.0f - fSlow51);
		float* 	fRec23 = &fRec23_tmp[4];
		float 	fSlow53 = expf((0 - (fConst3 / fSlow50)));
		float 	fSlow54 = fslider15;
		float 	fSlow55 = (((1.0f / float(fslider16)) - 1.0f) * (1.0f - fSlow53));
		float* 	fRec22 = &fRec22_tmp[4];
		float* 	fRec32 = &fRec32_tmp[4];
		float* 	fRec31 = &fRec31_tmp[4];
		float* 	fRec30 = &fRec30_tmp[4];
		float* 	fRec35 = &fRec35_tmp[4];
		float* 	fRec34 = &fRec34_tmp[4];
		float* 	fRec33 = &fRec33_tmp[4];
		float* 	fRec38 = &fRec38_tmp[4];
		float* 	fRec37 = &fRec37_tmp[4];
		float* 	fRec36 = &fRec36_tmp[4];
		int 	iSlow56 = int(fcheckbox3);
		int 	iSlow57 = int(fcheckbox4);
		int 	iSlow58 = int(fcheckbox5);
		int 	iSlow59 = (((iSlow58 == 0) & (iSlow57 == 0)) & (iSlow56 == 0));
		float 	fSlow60 = (float((iSlow57 | iSlow59)) * powf(10,(0.05f * fslider17)));
		float 	fSlow61 = (fSlow60 * fSlow47);
		float 	fSlow62 = (float((iSlow56 | iSlow59)) * powf(10,(0.05f * fslider18)));
		float 	fSlow63 = (fSlow62 * fSlow34);
		float 	fSlow64 = (float((iSlow58 | iSlow59)) * powf(10,(0.05f * fslider19)));
		float 	fSlow65 = (fSlow64 * fSlow20);
		int 	iSlow66 = int(fcheckbox6);
		float 	fSlow67 = pun_int_to_float((8388608 * (126.94269504f + max(-126.0f, (0.16609640464202244f * fslider20)))));
		float 	fSlow68 = expf((0 - (fConst4 / fslider21)));
		float 	fSlow69 = (1.0f - fSlow68);
		float* 	fRec29 = &fRec29_tmp[4];
		float 	fSlow70 = fslider22;
		float 	fSlow71 = expf((0 - (fConst5 / fSlow70)));
		float 	fSlow72 = (1.0f - fSlow71);
		float* 	fRec28 = &fRec28_tmp[4];
		float 	fSlow73 = expf((0 - (fConst6 / fSlow70)));
		float 	fSlow74 = (((1.0f / float(fslider23)) - 1.0f) * (1.0f - fSlow73));
		float* 	fRec27 = &fRec27_tmp[4];
		float 	fSlow75 = (pun_int_to_float((8388608 * (126.94269504f + max(-126.0f, (0.16609640464202244f * fslider24))))) * fSlow67);
		float 	fSlow76 = powf(10,(0.05f * fslider25));
		int index;
		for (index = 0; index <= fullcount - 32; index += 32) {
			// compute by blocks of 32 samples
			const int count = 32;
			FAUSTFLOAT* input0 = &input[0][index];
			FAUSTFLOAT* input1 = &input[1][index];
			FAUSTFLOAT* output0 = &output[0][index];
			FAUSTFLOAT* output1 = &output[1][index];
			// SECTION : 1
			// LOOP 0x2421b20
			// pre processing
			for (int i=0; i<4; i++) fYec0_tmp[i]=fYec0_perm[i];
			// exec code
			for (int i=0; i<count; i++) {
				fYec0[i] = (float)input0[i];
			}
			// post processing
			for (int i=0; i<4; i++) fYec0_perm[i]=fYec0_tmp[count+i];
			
			// LOOP 0x242c6d0
			// pre processing
			for (int i=0; i<4; i++) fYec2_tmp[i]=fYec2_perm[i];
			// exec code
			for (int i=0; i<count; i++) {
				fYec2[i] = (float)input1[i];
			}
			// post processing
			for (int i=0; i<4; i++) fYec2_perm[i]=fYec2_tmp[count+i];
			
			// SECTION : 2
			// LOOP 0x241f860
			// pre processing
			for (int i=0; i<4; i++) fRec3_tmp[i]=fRec3_perm[i];
			// exec code
			for (int i=0; i<count; i++) {
				fRec3[i] = ((fSlow4 * ((float)input0[i] + fYec0[i-1])) + (fSlow3 * fRec3[i-1]));
			}
			// post processing
			for (int i=0; i<4; i++) fRec3_perm[i]=fRec3_tmp[count+i];
			
			// LOOP 0x242c130
			// pre processing
			for (int i=0; i<4; i++) fRec10_tmp[i]=fRec10_perm[i];
			// exec code
			for (int i=0; i<count; i++) {
				fRec10[i] = ((fSlow4 * ((float)input1[i] + fYec2[i-1])) + (fSlow3 * fRec10[i-1]));
			}
			// post processing
			for (int i=0; i<4; i++) fRec10_perm[i]=fRec10_tmp[count+i];
			
			// SECTION : 3
			// LOOP 0x241f530
			// pre processing
			for (int i=0; i<4; i++) fRec2_tmp[i]=fRec2_perm[i];
			// exec code
			for (int i=0; i<count; i++) {
				fRec2[i] = (fRec3[i] - (fSlow9 * ((fSlow7 * fRec2[i-2]) + (fSlow6 * fRec2[i-1]))));
			}
			// post processing
			for (int i=0; i<4; i++) fRec2_perm[i]=fRec2_tmp[count+i];
			
			// LOOP 0x242bd70
			// pre processing
			for (int i=0; i<4; i++) fRec9_tmp[i]=fRec9_perm[i];
			// exec code
			for (int i=0; i<count; i++) {
				fRec9[i] = (fRec10[i] - (fSlow9 * ((fSlow7 * fRec9[i-2]) + (fSlow6 * fRec9[i-1]))));
			}
			// post processing
			for (int i=0; i<4; i++) fRec9_perm[i]=fRec9_tmp[count+i];
			
			// SECTION : 4
			// LOOP 0x24268d0
			// exec code
			for (int i=0; i<count; i++) {
				fZec0[i] = (fRec2[i-2] + (fRec2[i] + (2 * fRec2[i-1])));
			}
			
			// LOOP 0x242f140
			// exec code
			for (int i=0; i<count; i++) {
				fZec1[i] = (fRec9[i-2] + (fRec9[i] + (2 * fRec9[i-1])));
			}
			
			// SECTION : 5
			// LOOP 0x24267f0
			// pre processing
			for (int i=0; i<4; i++) fYec1_tmp[i]=fYec1_perm[i];
			// exec code
			for (int i=0; i<count; i++) {
				fYec1[i] = (fSlow9 * fZec0[i]);
			}
			// post processing
			for (int i=0; i<4; i++) fYec1_perm[i]=fYec1_tmp[count+i];
			
			// LOOP 0x242f060
			// pre processing
			for (int i=0; i<4; i++) fYec3_tmp[i]=fYec3_perm[i];
			// exec code
			for (int i=0; i<count; i++) {
				fYec3[i] = (fSlow9 * fZec1[i]);
			}
			// post processing
			for (int i=0; i<4; i++) fYec3_perm[i]=fYec3_tmp[count+i];
			
			// LOOP 0x243d090
			// pre processing
			for (int i=0; i<4; i++) fRec13_tmp[i]=fRec13_perm[i];
			// exec code
			for (int i=0; i<count; i++) {
				fRec13[i] = ((fSlow4 * ((fSlow29 * fYec0[i-1]) + (fSlow1 * (float)input0[i]))) + (fSlow3 * fRec13[i-1]));
			}
			// post processing
			for (int i=0; i<4; i++) fRec13_perm[i]=fRec13_tmp[count+i];
			
			// LOOP 0x2443540
			// pre processing
			for (int i=0; i<4; i++) fRec19_tmp[i]=fRec19_perm[i];
			// exec code
			for (int i=0; i<count; i++) {
				fRec19[i] = ((fSlow4 * ((fSlow29 * fYec2[i-1]) + (fSlow1 * (float)input1[i]))) + (fSlow3 * fRec19[i-1]));
			}
			// post processing
			for (int i=0; i<4; i++) fRec19_perm[i]=fRec19_tmp[count+i];
			
			// SECTION : 6
			// LOOP 0x241f1d0
			// pre processing
			for (int i=0; i<4; i++) fRec1_tmp[i]=fRec1_perm[i];
			// exec code
			for (int i=0; i<count; i++) {
				fRec1[i] = ((fSlow14 * (fYec1[i] + fYec1[i-1])) + (fSlow13 * fRec1[i-1]));
			}
			// post processing
			for (int i=0; i<4; i++) fRec1_perm[i]=fRec1_tmp[count+i];
			
			// LOOP 0x242b930
			// pre processing
			for (int i=0; i<4; i++) fRec8_tmp[i]=fRec8_perm[i];
			// exec code
			for (int i=0; i<count; i++) {
				fRec8[i] = ((fSlow14 * (fYec3[i] + fYec3[i-1])) + (fSlow13 * fRec8[i-1]));
			}
			// post processing
			for (int i=0; i<4; i++) fRec8_perm[i]=fRec8_tmp[count+i];
			
			// LOOP 0x243ccd0
			// pre processing
			for (int i=0; i<4; i++) fRec12_tmp[i]=fRec12_perm[i];
			// exec code
			for (int i=0; i<count; i++) {
				fRec12[i] = (fRec13[i] - (fSlow9 * ((fSlow7 * fRec12[i-2]) + (fSlow6 * fRec12[i-1]))));
			}
			// post processing
			for (int i=0; i<4; i++) fRec12_perm[i]=fRec12_tmp[count+i];
			
			// LOOP 0x2443180
			// pre processing
			for (int i=0; i<4; i++) fRec18_tmp[i]=fRec18_perm[i];
			// exec code
			for (int i=0; i<count; i++) {
				fRec18[i] = (fRec19[i] - (fSlow9 * ((fSlow7 * fRec18[i-2]) + (fSlow6 * fRec18[i-1]))));
			}
			// post processing
			for (int i=0; i<4; i++) fRec18_perm[i]=fRec18_tmp[count+i];
			
			// LOOP 0x2452e30
			// pre processing
			for (int i=0; i<4; i++) fRec21_tmp[i]=fRec21_perm[i];
			// exec code
			for (int i=0; i<count; i++) {
				fRec21[i] = ((fSlow14 * ((fSlow44 * fYec1[i-1]) + (fSlow43 * fZec0[i]))) + (fSlow13 * fRec21[i-1]));
			}
			// post processing
			for (int i=0; i<4; i++) fRec21_perm[i]=fRec21_tmp[count+i];
			
			// LOOP 0x24569f0
			// pre processing
			for (int i=0; i<4; i++) fRec26_tmp[i]=fRec26_perm[i];
			// exec code
			for (int i=0; i<count; i++) {
				fRec26[i] = ((fSlow14 * ((fSlow44 * fYec3[i-1]) + (fSlow43 * fZec1[i]))) + (fSlow13 * fRec26[i-1]));
			}
			// post processing
			for (int i=0; i<4; i++) fRec26_perm[i]=fRec26_tmp[count+i];
			
			// SECTION : 7
			// LOOP 0x241edd0
			// pre processing
			for (int i=0; i<4; i++) fRec0_tmp[i]=fRec0_perm[i];
			// exec code
			for (int i=0; i<count; i++) {
				fRec0[i] = (fRec1[i] - (fSlow18 * ((fSlow17 * fRec0[i-2]) + (fSlow16 * fRec0[i-1]))));
			}
			// post processing
			for (int i=0; i<4; i++) fRec0_perm[i]=fRec0_tmp[count+i];
			
			// LOOP 0x242b570
			// pre processing
			for (int i=0; i<4; i++) fRec7_tmp[i]=fRec7_perm[i];
			// exec code
			for (int i=0; i<count; i++) {
				fRec7[i] = (fRec8[i] - (fSlow18 * ((fSlow17 * fRec7[i-2]) + (fSlow16 * fRec7[i-1]))));
			}
			// post processing
			for (int i=0; i<4; i++) fRec7_perm[i]=fRec7_tmp[count+i];
			
			// LOOP 0x243ca10
			// pre processing
			for (int i=0; i<4; i++) fRec11_tmp[i]=fRec11_perm[i];
			// exec code
			for (int i=0; i<count; i++) {
				fZec7[i] = (fSlow16 * fRec11[i-1]);
				fRec11[i] = ((fSlow9 * (((fSlow5 * fRec12[i]) + (fSlow32 * fRec12[i-1])) + (fSlow5 * fRec12[i-2]))) - (fSlow31 * ((fSlow30 * fRec11[i-2]) + fZec7[i])));
			}
			// post processing
			for (int i=0; i<4; i++) fRec11_perm[i]=fRec11_tmp[count+i];
			
			// LOOP 0x2442d40
			// pre processing
			for (int i=0; i<4; i++) fRec17_tmp[i]=fRec17_perm[i];
			// exec code
			for (int i=0; i<count; i++) {
				fZec8[i] = (fSlow16 * fRec17[i-1]);
				fRec17[i] = ((fSlow9 * (((fSlow5 * fRec18[i]) + (fSlow32 * fRec18[i-1])) + (fSlow5 * fRec18[i-2]))) - (fSlow31 * ((fSlow30 * fRec17[i-2]) + fZec8[i])));
			}
			// post processing
			for (int i=0; i<4; i++) fRec17_perm[i]=fRec17_tmp[count+i];
			
			// LOOP 0x2452b90
			// pre processing
			for (int i=0; i<4; i++) fRec20_tmp[i]=fRec20_perm[i];
			// exec code
			for (int i=0; i<count; i++) {
				fRec20[i] = (fRec21[i] - (fSlow18 * ((fSlow17 * fRec20[i-2]) + (fSlow16 * fRec20[i-1]))));
			}
			// post processing
			for (int i=0; i<4; i++) fRec20_perm[i]=fRec20_tmp[count+i];
			
			// LOOP 0x2456630
			// pre processing
			for (int i=0; i<4; i++) fRec25_tmp[i]=fRec25_perm[i];
			// exec code
			for (int i=0; i<count; i++) {
				fRec25[i] = (fRec26[i] - (fSlow18 * ((fSlow17 * fRec25[i-2]) + (fSlow16 * fRec25[i-1]))));
			}
			// post processing
			for (int i=0; i<4; i++) fRec25_perm[i]=fRec25_tmp[count+i];
			
			// SECTION : 8
			// LOOP 0x2431d80
			// exec code
			for (int i=0; i<count; i++) {
				fZec2[i] = (fSlow18 * (fRec0[i-2] + (fRec0[i] + (2 * fRec0[i-1]))));
			}
			
			// LOOP 0x2433c60
			// exec code
			for (int i=0; i<count; i++) {
				fZec4[i] = (fSlow18 * (fRec7[i-2] + (fRec7[i] + (2 * fRec7[i-1]))));
			}
			
			// LOOP 0x24478a0
			// exec code
			for (int i=0; i<count; i++) {
				fZec9[i] = (fRec11[i-2] + (fSlow31 * (fZec7[i] + (fSlow30 * fRec11[i]))));
			}
			
			// LOOP 0x2449d90
			// exec code
			for (int i=0; i<count; i++) {
				fZec11[i] = (fRec17[i-2] + (fSlow31 * (fZec8[i] + (fSlow30 * fRec17[i]))));
			}
			
			// LOOP 0x24593f0
			// exec code
			for (int i=0; i<count; i++) {
				fZec14[i] = (fSlow18 * (((fSlow15 * fRec20[i]) + (fSlow45 * fRec20[i-1])) + (fSlow15 * fRec20[i-2])));
			}
			
			// LOOP 0x245beb0
			// exec code
			for (int i=0; i<count; i++) {
				fZec16[i] = (fSlow18 * (((fSlow15 * fRec25[i]) + (fSlow45 * fRec25[i-1])) + (fSlow15 * fRec25[i-2])));
			}
			
			// SECTION : 9
			// LOOP 0x2431ca0
			// exec code
			for (int i=0; i<count; i++) {
				fZec3[i] = ((iSlow19)?0:fZec2[i]);
			}
			
			// LOOP 0x2433b80
			// exec code
			for (int i=0; i<count; i++) {
				fZec5[i] = ((iSlow19)?0:fZec4[i]);
			}
			
			// LOOP 0x24477c0
			// exec code
			for (int i=0; i<count; i++) {
				fZec10[i] = ((iSlow33)?0:fZec9[i]);
			}
			
			// LOOP 0x2449cb0
			// exec code
			for (int i=0; i<count; i++) {
				fZec12[i] = ((iSlow33)?0:fZec11[i]);
			}
			
			// LOOP 0x2459310
			// exec code
			for (int i=0; i<count; i++) {
				fZec15[i] = ((iSlow46)?0:fZec14[i]);
			}
			
			// LOOP 0x245bdd0
			// exec code
			for (int i=0; i<count; i++) {
				fZec17[i] = ((iSlow46)?0:fZec16[i]);
			}
			
			// SECTION : 10
			// LOOP 0x2431bc0
			// exec code
			for (int i=0; i<count; i++) {
				fZec6[i] = fabsf((fabsf((fSlow20 * fZec5[i])) + fabsf((fSlow20 * fZec3[i]))));
			}
			
			// LOOP 0x24476e0
			// exec code
			for (int i=0; i<count; i++) {
				fZec13[i] = fabsf((fabsf((fSlow34 * fZec12[i])) + fabsf((fSlow34 * fZec10[i]))));
			}
			
			// LOOP 0x2459230
			// exec code
			for (int i=0; i<count; i++) {
				fZec18[i] = fabsf((fabsf((fSlow47 * fZec17[i])) + fabsf((fSlow47 * fZec15[i]))));
			}
			
			// SECTION : 11
			// LOOP 0x242b150
			// pre processing
			for (int i=0; i<4; i++) fRec6_tmp[i]=fRec6_perm[i];
			// exec code
			for (int i=0; i<count; i++) {
				fRec6[i] = ((fSlow22 * fZec6[i]) + (fSlow21 * max(fZec6[i], fRec6[i-1])));
			}
			// post processing
			for (int i=0; i<4; i++) fRec6_perm[i]=fRec6_tmp[count+i];
			
			// LOOP 0x2442940
			// pre processing
			for (int i=0; i<4; i++) fRec16_tmp[i]=fRec16_perm[i];
			// exec code
			for (int i=0; i<count; i++) {
				fRec16[i] = ((fSlow36 * fZec13[i]) + (fSlow35 * max(fZec13[i], fRec16[i-1])));
			}
			// post processing
			for (int i=0; i<4; i++) fRec16_perm[i]=fRec16_tmp[count+i];
			
			// LOOP 0x24561d0
			// pre processing
			for (int i=0; i<4; i++) fRec24_tmp[i]=fRec24_perm[i];
			// exec code
			for (int i=0; i<count; i++) {
				fRec24[i] = ((fSlow49 * fZec18[i]) + (fSlow48 * max(fZec18[i], fRec24[i-1])));
			}
			// post processing
			for (int i=0; i<4; i++) fRec24_perm[i]=fRec24_tmp[count+i];
			
			// SECTION : 12
			// LOOP 0x242acf0
			// pre processing
			for (int i=0; i<4; i++) fRec5_tmp[i]=fRec5_perm[i];
			// exec code
			for (int i=0; i<count; i++) {
				fRec5[i] = ((fSlow25 * fRec6[i]) + (fSlow24 * fRec5[i-1]));
			}
			// post processing
			for (int i=0; i<4; i++) fRec5_perm[i]=fRec5_tmp[count+i];
			
			// LOOP 0x24424e0
			// pre processing
			for (int i=0; i<4; i++) fRec15_tmp[i]=fRec15_perm[i];
			// exec code
			for (int i=0; i<count; i++) {
				fRec15[i] = ((fSlow39 * fRec16[i]) + (fSlow38 * fRec15[i-1]));
			}
			// post processing
			for (int i=0; i<4; i++) fRec15_perm[i]=fRec15_tmp[count+i];
			
			// LOOP 0x2455d70
			// pre processing
			for (int i=0; i<4; i++) fRec23_tmp[i]=fRec23_perm[i];
			// exec code
			for (int i=0; i<count; i++) {
				fRec23[i] = ((fSlow52 * fRec24[i]) + (fSlow51 * fRec23[i-1]));
			}
			// post processing
			for (int i=0; i<4; i++) fRec23_perm[i]=fRec23_tmp[count+i];
			
			// SECTION : 13
			// LOOP 0x242a930
			// pre processing
			for (int i=0; i<4; i++) fRec4_tmp[i]=fRec4_perm[i];
			// exec code
			for (int i=0; i<count; i++) {
				fRec4[i] = ((fSlow28 * max(((8.685889638065037f * ((8.262958288192749e-08f * float(pun_float_to_int(fRec5[i]))) - 87.989971088f)) - fSlow27), 0.0f)) + (fSlow26 * fRec4[i-1]));
			}
			// post processing
			for (int i=0; i<4; i++) fRec4_perm[i]=fRec4_tmp[count+i];
			
			// LOOP 0x2442100
			// pre processing
			for (int i=0; i<4; i++) fRec14_tmp[i]=fRec14_perm[i];
			// exec code
			for (int i=0; i<count; i++) {
				fRec14[i] = ((fSlow42 * max(((8.685889638065037f * ((8.262958288192749e-08f * float(pun_float_to_int(fRec15[i]))) - 87.989971088f)) - fSlow41), 0.0f)) + (fSlow40 * fRec14[i-1]));
			}
			// post processing
			for (int i=0; i<4; i++) fRec14_perm[i]=fRec14_tmp[count+i];
			
			// LOOP 0x24559b0
			// pre processing
			for (int i=0; i<4; i++) fRec22_tmp[i]=fRec22_perm[i];
			// exec code
			for (int i=0; i<count; i++) {
				fRec22[i] = ((fSlow55 * max(((8.685889638065037f * ((8.262958288192749e-08f * float(pun_float_to_int(fRec23[i]))) - 87.989971088f)) - fSlow54), 0.0f)) + (fSlow53 * fRec22[i-1]));
			}
			// post processing
			for (int i=0; i<4; i++) fRec22_perm[i]=fRec22_tmp[count+i];
			
			// SECTION : 14
			// LOOP 0x2467870
			// exec code
			for (int i=0; i<count; i++) {
				fZec19[i] = pun_int_to_float((8388608 * (126.94269504f + max(-126.0f, (0.16609640464202244f * fRec4[i])))));
			}
			
			// LOOP 0x246f5c0
			// exec code
			for (int i=0; i<count; i++) {
				fZec23[i] = pun_int_to_float((8388608 * (126.94269504f + max(-126.0f, (0.16609640464202244f * fRec14[i])))));
			}
			
			// LOOP 0x24768c0
			// exec code
			for (int i=0; i<count; i++) {
				fZec27[i] = pun_int_to_float((8388608 * (126.94269504f + max(-126.0f, (0.16609640464202244f * fRec22[i])))));
			}
			
			// SECTION : 15
			// LOOP 0x2467790
			// exec code
			for (int i=0; i<count; i++) {
				fZec20[i] = (fZec3[i] * fZec19[i]);
			}
			
			// LOOP 0x2468d60
			// exec code
			for (int i=0; i<count; i++) {
				fZec21[i] = (fSlow20 * (fZec5[i] * fZec19[i]));
			}
			
			// LOOP 0x246f4e0
			// exec code
			for (int i=0; i<count; i++) {
				fZec24[i] = (fZec10[i] * fZec23[i]);
			}
			
			// LOOP 0x2470620
			// exec code
			for (int i=0; i<count; i++) {
				fZec25[i] = (fSlow34 * (fZec12[i] * fZec23[i]));
			}
			
			// LOOP 0x24767e0
			// exec code
			for (int i=0; i<count; i++) {
				fZec28[i] = (fZec15[i] * fZec27[i]);
			}
			
			// LOOP 0x2477920
			// exec code
			for (int i=0; i<count; i++) {
				fZec29[i] = (fSlow47 * (fZec17[i] * fZec27[i]));
			}
			
			// SECTION : 16
			// LOOP 0x24676b0
			// exec code
			for (int i=0; i<count; i++) {
				fZec22[i] = fabsf((fabsf(fZec21[i]) + fabsf((fSlow20 * fZec20[i]))));
			}
			
			// LOOP 0x246f400
			// exec code
			for (int i=0; i<count; i++) {
				fZec26[i] = fabsf((fabsf(fZec25[i]) + fabsf((fSlow34 * fZec24[i]))));
			}
			
			// LOOP 0x2476700
			// exec code
			for (int i=0; i<count; i++) {
				fZec30[i] = fabsf((fabsf(fZec29[i]) + fabsf((fSlow47 * fZec28[i]))));
			}
			
			// SECTION : 17
			// LOOP 0x2467290
			// pre processing
			for (int i=0; i<4; i++) fRec32_tmp[i]=fRec32_perm[i];
			// exec code
			for (int i=0; i<count; i++) {
				fRec32[i] = ((fSlow22 * fZec22[i]) + (fSlow21 * max(fZec22[i], fRec32[i-1])));
			}
			// post processing
			for (int i=0; i<4; i++) fRec32_perm[i]=fRec32_tmp[count+i];
			
			// LOOP 0x246efe0
			// pre processing
			for (int i=0; i<4; i++) fRec35_tmp[i]=fRec35_perm[i];
			// exec code
			for (int i=0; i<count; i++) {
				fRec35[i] = ((fSlow36 * fZec26[i]) + (fSlow35 * max(fZec26[i], fRec35[i-1])));
			}
			// post processing
			for (int i=0; i<4; i++) fRec35_perm[i]=fRec35_tmp[count+i];
			
			// LOOP 0x24762e0
			// pre processing
			for (int i=0; i<4; i++) fRec38_tmp[i]=fRec38_perm[i];
			// exec code
			for (int i=0; i<count; i++) {
				fRec38[i] = ((fSlow49 * fZec30[i]) + (fSlow48 * max(fZec30[i], fRec38[i-1])));
			}
			// post processing
			for (int i=0; i<4; i++) fRec38_perm[i]=fRec38_tmp[count+i];
			
			// SECTION : 18
			// LOOP 0x2466eb0
			// pre processing
			for (int i=0; i<4; i++) fRec31_tmp[i]=fRec31_perm[i];
			// exec code
			for (int i=0; i<count; i++) {
				fRec31[i] = ((fSlow25 * fRec32[i]) + (fSlow24 * fRec31[i-1]));
			}
			// post processing
			for (int i=0; i<4; i++) fRec31_perm[i]=fRec31_tmp[count+i];
			
			// LOOP 0x246ec00
			// pre processing
			for (int i=0; i<4; i++) fRec34_tmp[i]=fRec34_perm[i];
			// exec code
			for (int i=0; i<count; i++) {
				fRec34[i] = ((fSlow39 * fRec35[i]) + (fSlow38 * fRec34[i-1]));
			}
			// post processing
			for (int i=0; i<4; i++) fRec34_perm[i]=fRec34_tmp[count+i];
			
			// LOOP 0x2475f00
			// pre processing
			for (int i=0; i<4; i++) fRec37_tmp[i]=fRec37_perm[i];
			// exec code
			for (int i=0; i<count; i++) {
				fRec37[i] = ((fSlow52 * fRec38[i]) + (fSlow51 * fRec37[i-1]));
			}
			// post processing
			for (int i=0; i<4; i++) fRec37_perm[i]=fRec37_tmp[count+i];
			
			// SECTION : 19
			// LOOP 0x24669f0
			// pre processing
			for (int i=0; i<4; i++) fRec30_tmp[i]=fRec30_perm[i];
			// exec code
			for (int i=0; i<count; i++) {
				fRec30[i] = ((fSlow28 * max(((8.685889638065037f * ((8.262958288192749e-08f * float(pun_float_to_int(fRec31[i]))) - 87.989971088f)) - fSlow27), 0.0f)) + (fSlow26 * fRec30[i-1]));
			}
			// post processing
			for (int i=0; i<4; i++) fRec30_perm[i]=fRec30_tmp[count+i];
			
			// LOOP 0x246e760
			// pre processing
			for (int i=0; i<4; i++) fRec33_tmp[i]=fRec33_perm[i];
			// exec code
			for (int i=0; i<count; i++) {
				fRec33[i] = ((fSlow42 * max(((8.685889638065037f * ((8.262958288192749e-08f * float(pun_float_to_int(fRec34[i]))) - 87.989971088f)) - fSlow41), 0.0f)) + (fSlow40 * fRec33[i-1]));
			}
			// post processing
			for (int i=0; i<4; i++) fRec33_perm[i]=fRec33_tmp[count+i];
			
			// LOOP 0x2475a80
			// pre processing
			for (int i=0; i<4; i++) fRec36_tmp[i]=fRec36_perm[i];
			// exec code
			for (int i=0; i<count; i++) {
				fRec36[i] = ((fSlow55 * max(((8.685889638065037f * ((8.262958288192749e-08f * float(pun_float_to_int(fRec37[i]))) - 87.989971088f)) - fSlow54), 0.0f)) + (fSlow53 * fRec36[i-1]));
			}
			// post processing
			for (int i=0; i<4; i++) fRec36_perm[i]=fRec36_tmp[count+i];
			
			// SECTION : 20
			// LOOP 0x247c690
			// exec code
			for (int i=0; i<count; i++) {
				fZec32[i] = (fSlow63 * fZec24[i]);
			}
			
			// LOOP 0x247d300
			// exec code
			for (int i=0; i<count; i++) {
				fZec31[i] = (fSlow61 * fZec28[i]);
			}
			
			// LOOP 0x2483570
			// exec code
			for (int i=0; i<count; i++) {
				fZec33[i] = (fSlow65 * fZec20[i]);
			}
			
			// LOOP 0x24893c0
			// exec code
			for (int i=0; i<count; i++) {
				fbargraph0 = (0.5f * pun_int_to_float((8388608 * (126.94269504f + max(-126.0f, (0.16609640464202244f * fRec36[i]))))));
				fZec36[i] = (fSlow60 * fZec29[i]);
			}
			
			// LOOP 0x248ca80
			// exec code
			for (int i=0; i<count; i++) {
				fbargraph2 = (0.5f * pun_int_to_float((8388608 * (126.94269504f + max(-126.0f, (0.16609640464202244f * fRec33[i]))))));
				fZec37[i] = (fSlow62 * fZec25[i]);
			}
			
			// LOOP 0x2490a80
			// exec code
			for (int i=0; i<count; i++) {
				fbargraph4 = (0.5f * pun_int_to_float((8388608 * (126.94269504f + max(-126.0f, (0.16609640464202244f * fRec30[i]))))));
				fZec38[i] = (fSlow64 * fZec21[i]);
			}
			
			// SECTION : 21
			// LOOP 0x247d220
			// exec code
			for (int i=0; i<count; i++) {
				fZec34[i] = (((iSlow19)?fZec2[i]:fZec33[i]) + (((iSlow33)?fZec9[i]:fZec32[i]) + ((iSlow46)?fZec14[i]:fZec31[i])));
			}
			
			// LOOP 0x24891e0
			// exec code
			for (int i=0; i<count; i++) {
				fbargraph1 = (fabsf(fZec36[i]) + fabsf(fZec31[i]));
				fbargraph3 = (fabsf(fZec37[i]) + fabsf(fZec32[i]));
				fbargraph5 = (fabsf(fZec38[i]) + fabsf(fZec33[i]));
				fZec39[i] = (((iSlow19)?fZec4[i]:fZec38[i]) + (((iSlow33)?fZec11[i]:fZec37[i]) + ((iSlow46)?fZec16[i]:fZec36[i])));
			}
			
			// SECTION : 22
			// LOOP 0x247d140
			// exec code
			for (int i=0; i<count; i++) {
				fZec35[i] = ((iSlow66)?0:fZec34[i]);
			}
			
			// LOOP 0x2489100
			// exec code
			for (int i=0; i<count; i++) {
				fZec40[i] = ((iSlow66)?0:fZec39[i]);
			}
			
			// SECTION : 23
			// LOOP 0x247d060
			// exec code
			for (int i=0; i<count; i++) {
				fZec41[i] = fabsf((fabsf((fSlow67 * fZec40[i])) + fabsf((fSlow67 * fZec35[i]))));
			}
			
			// SECTION : 24
			// LOOP 0x2466490
			// pre processing
			for (int i=0; i<4; i++) fRec29_tmp[i]=fRec29_perm[i];
			// exec code
			for (int i=0; i<count; i++) {
				fRec29[i] = ((fSlow69 * fZec41[i]) + (fSlow68 * max(fZec41[i], fRec29[i-1])));
			}
			// post processing
			for (int i=0; i<4; i++) fRec29_perm[i]=fRec29_tmp[count+i];
			
			// SECTION : 25
			// LOOP 0x2466050
			// pre processing
			for (int i=0; i<4; i++) fRec28_tmp[i]=fRec28_perm[i];
			// exec code
			for (int i=0; i<count; i++) {
				fRec28[i] = ((fSlow72 * fRec29[i]) + (fSlow71 * fRec28[i-1]));
			}
			// post processing
			for (int i=0; i<4; i++) fRec28_perm[i]=fRec28_tmp[count+i];
			
			// SECTION : 26
			// LOOP 0x2465cb0
			// pre processing
			for (int i=0; i<4; i++) fRec27_tmp[i]=fRec27_perm[i];
			// exec code
			for (int i=0; i<count; i++) {
				fRec27[i] = ((fSlow74 * max((6 + (8.685889638065037f * ((8.262958288192749e-08f * float(pun_float_to_int(fRec28[i]))) - 87.989971088f))), 0.0f)) + (fSlow73 * fRec27[i-1]));
			}
			// post processing
			for (int i=0; i<4; i++) fRec27_perm[i]=fRec27_tmp[count+i];
			
			// SECTION : 27
			// LOOP 0x249efc0
			// exec code
			for (int i=0; i<count; i++) {
				fZec42[i] = pun_int_to_float((8388608 * (126.94269504f + max(-126.0f, (0.16609640464202244f * fRec27[i])))));
			}
			
			// SECTION : 28
			// LOOP 0x241eb10
			// exec code
			for (int i=0; i<count; i++) {
				output0[i] = (FAUSTFLOAT)(fSlow76 * ((iSlow66)?fZec34[i]:(fSlow75 * (fZec35[i] * fZec42[i]))));
			}
			
			// LOOP 0x24a4870
			// exec code
			for (int i=0; i<count; i++) {
				output1[i] = (FAUSTFLOAT)(fSlow76 * ((iSlow66)?fZec39[i]:(fSlow75 * (fZec40[i] * fZec42[i]))));
			}
			
		}
		if (index < fullcount) {
			// compute the remaining samples if any
			int count = fullcount-index;
			FAUSTFLOAT* input0 = &input[0][index];
			FAUSTFLOAT* input1 = &input[1][index];
			FAUSTFLOAT* output0 = &output[0][index];
			FAUSTFLOAT* output1 = &output[1][index];
			// SECTION : 1
			// LOOP 0x2421b20
			// pre processing
			for (int i=0; i<4; i++) fYec0_tmp[i]=fYec0_perm[i];
			// exec code
			for (int i=0; i<count; i++) {
				fYec0[i] = (float)input0[i];
			}
			// post processing
			for (int i=0; i<4; i++) fYec0_perm[i]=fYec0_tmp[count+i];
			
			// LOOP 0x242c6d0
			// pre processing
			for (int i=0; i<4; i++) fYec2_tmp[i]=fYec2_perm[i];
			// exec code
			for (int i=0; i<count; i++) {
				fYec2[i] = (float)input1[i];
			}
			// post processing
			for (int i=0; i<4; i++) fYec2_perm[i]=fYec2_tmp[count+i];
			
			// SECTION : 2
			// LOOP 0x241f860
			// pre processing
			for (int i=0; i<4; i++) fRec3_tmp[i]=fRec3_perm[i];
			// exec code
			for (int i=0; i<count; i++) {
				fRec3[i] = ((fSlow4 * ((float)input0[i] + fYec0[i-1])) + (fSlow3 * fRec3[i-1]));
			}
			// post processing
			for (int i=0; i<4; i++) fRec3_perm[i]=fRec3_tmp[count+i];
			
			// LOOP 0x242c130
			// pre processing
			for (int i=0; i<4; i++) fRec10_tmp[i]=fRec10_perm[i];
			// exec code
			for (int i=0; i<count; i++) {
				fRec10[i] = ((fSlow4 * ((float)input1[i] + fYec2[i-1])) + (fSlow3 * fRec10[i-1]));
			}
			// post processing
			for (int i=0; i<4; i++) fRec10_perm[i]=fRec10_tmp[count+i];
			
			// SECTION : 3
			// LOOP 0x241f530
			// pre processing
			for (int i=0; i<4; i++) fRec2_tmp[i]=fRec2_perm[i];
			// exec code
			for (int i=0; i<count; i++) {
				fRec2[i] = (fRec3[i] - (fSlow9 * ((fSlow7 * fRec2[i-2]) + (fSlow6 * fRec2[i-1]))));
			}
			// post processing
			for (int i=0; i<4; i++) fRec2_perm[i]=fRec2_tmp[count+i];
			
			// LOOP 0x242bd70
			// pre processing
			for (int i=0; i<4; i++) fRec9_tmp[i]=fRec9_perm[i];
			// exec code
			for (int i=0; i<count; i++) {
				fRec9[i] = (fRec10[i] - (fSlow9 * ((fSlow7 * fRec9[i-2]) + (fSlow6 * fRec9[i-1]))));
			}
			// post processing
			for (int i=0; i<4; i++) fRec9_perm[i]=fRec9_tmp[count+i];
			
			// SECTION : 4
			// LOOP 0x24268d0
			// exec code
			for (int i=0; i<count; i++) {
				fZec0[i] = (fRec2[i-2] + (fRec2[i] + (2 * fRec2[i-1])));
			}
			
			// LOOP 0x242f140
			// exec code
			for (int i=0; i<count; i++) {
				fZec1[i] = (fRec9[i-2] + (fRec9[i] + (2 * fRec9[i-1])));
			}
			
			// SECTION : 5
			// LOOP 0x24267f0
			// pre processing
			for (int i=0; i<4; i++) fYec1_tmp[i]=fYec1_perm[i];
			// exec code
			for (int i=0; i<count; i++) {
				fYec1[i] = (fSlow9 * fZec0[i]);
			}
			// post processing
			for (int i=0; i<4; i++) fYec1_perm[i]=fYec1_tmp[count+i];
			
			// LOOP 0x242f060
			// pre processing
			for (int i=0; i<4; i++) fYec3_tmp[i]=fYec3_perm[i];
			// exec code
			for (int i=0; i<count; i++) {
				fYec3[i] = (fSlow9 * fZec1[i]);
			}
			// post processing
			for (int i=0; i<4; i++) fYec3_perm[i]=fYec3_tmp[count+i];
			
			// LOOP 0x243d090
			// pre processing
			for (int i=0; i<4; i++) fRec13_tmp[i]=fRec13_perm[i];
			// exec code
			for (int i=0; i<count; i++) {
				fRec13[i] = ((fSlow4 * ((fSlow29 * fYec0[i-1]) + (fSlow1 * (float)input0[i]))) + (fSlow3 * fRec13[i-1]));
			}
			// post processing
			for (int i=0; i<4; i++) fRec13_perm[i]=fRec13_tmp[count+i];
			
			// LOOP 0x2443540
			// pre processing
			for (int i=0; i<4; i++) fRec19_tmp[i]=fRec19_perm[i];
			// exec code
			for (int i=0; i<count; i++) {
				fRec19[i] = ((fSlow4 * ((fSlow29 * fYec2[i-1]) + (fSlow1 * (float)input1[i]))) + (fSlow3 * fRec19[i-1]));
			}
			// post processing
			for (int i=0; i<4; i++) fRec19_perm[i]=fRec19_tmp[count+i];
			
			// SECTION : 6
			// LOOP 0x241f1d0
			// pre processing
			for (int i=0; i<4; i++) fRec1_tmp[i]=fRec1_perm[i];
			// exec code
			for (int i=0; i<count; i++) {
				fRec1[i] = ((fSlow14 * (fYec1[i] + fYec1[i-1])) + (fSlow13 * fRec1[i-1]));
			}
			// post processing
			for (int i=0; i<4; i++) fRec1_perm[i]=fRec1_tmp[count+i];
			
			// LOOP 0x242b930
			// pre processing
			for (int i=0; i<4; i++) fRec8_tmp[i]=fRec8_perm[i];
			// exec code
			for (int i=0; i<count; i++) {
				fRec8[i] = ((fSlow14 * (fYec3[i] + fYec3[i-1])) + (fSlow13 * fRec8[i-1]));
			}
			// post processing
			for (int i=0; i<4; i++) fRec8_perm[i]=fRec8_tmp[count+i];
			
			// LOOP 0x243ccd0
			// pre processing
			for (int i=0; i<4; i++) fRec12_tmp[i]=fRec12_perm[i];
			// exec code
			for (int i=0; i<count; i++) {
				fRec12[i] = (fRec13[i] - (fSlow9 * ((fSlow7 * fRec12[i-2]) + (fSlow6 * fRec12[i-1]))));
			}
			// post processing
			for (int i=0; i<4; i++) fRec12_perm[i]=fRec12_tmp[count+i];
			
			// LOOP 0x2443180
			// pre processing
			for (int i=0; i<4; i++) fRec18_tmp[i]=fRec18_perm[i];
			// exec code
			for (int i=0; i<count; i++) {
				fRec18[i] = (fRec19[i] - (fSlow9 * ((fSlow7 * fRec18[i-2]) + (fSlow6 * fRec18[i-1]))));
			}
			// post processing
			for (int i=0; i<4; i++) fRec18_perm[i]=fRec18_tmp[count+i];
			
			// LOOP 0x2452e30
			// pre processing
			for (int i=0; i<4; i++) fRec21_tmp[i]=fRec21_perm[i];
			// exec code
			for (int i=0; i<count; i++) {
				fRec21[i] = ((fSlow14 * ((fSlow44 * fYec1[i-1]) + (fSlow43 * fZec0[i]))) + (fSlow13 * fRec21[i-1]));
			}
			// post processing
			for (int i=0; i<4; i++) fRec21_perm[i]=fRec21_tmp[count+i];
			
			// LOOP 0x24569f0
			// pre processing
			for (int i=0; i<4; i++) fRec26_tmp[i]=fRec26_perm[i];
			// exec code
			for (int i=0; i<count; i++) {
				fRec26[i] = ((fSlow14 * ((fSlow44 * fYec3[i-1]) + (fSlow43 * fZec1[i]))) + (fSlow13 * fRec26[i-1]));
			}
			// post processing
			for (int i=0; i<4; i++) fRec26_perm[i]=fRec26_tmp[count+i];
			
			// SECTION : 7
			// LOOP 0x241edd0
			// pre processing
			for (int i=0; i<4; i++) fRec0_tmp[i]=fRec0_perm[i];
			// exec code
			for (int i=0; i<count; i++) {
				fRec0[i] = (fRec1[i] - (fSlow18 * ((fSlow17 * fRec0[i-2]) + (fSlow16 * fRec0[i-1]))));
			}
			// post processing
			for (int i=0; i<4; i++) fRec0_perm[i]=fRec0_tmp[count+i];
			
			// LOOP 0x242b570
			// pre processing
			for (int i=0; i<4; i++) fRec7_tmp[i]=fRec7_perm[i];
			// exec code
			for (int i=0; i<count; i++) {
				fRec7[i] = (fRec8[i] - (fSlow18 * ((fSlow17 * fRec7[i-2]) + (fSlow16 * fRec7[i-1]))));
			}
			// post processing
			for (int i=0; i<4; i++) fRec7_perm[i]=fRec7_tmp[count+i];
			
			// LOOP 0x243ca10
			// pre processing
			for (int i=0; i<4; i++) fRec11_tmp[i]=fRec11_perm[i];
			// exec code
			for (int i=0; i<count; i++) {
				fZec7[i] = (fSlow16 * fRec11[i-1]);
				fRec11[i] = ((fSlow9 * (((fSlow5 * fRec12[i]) + (fSlow32 * fRec12[i-1])) + (fSlow5 * fRec12[i-2]))) - (fSlow31 * ((fSlow30 * fRec11[i-2]) + fZec7[i])));
			}
			// post processing
			for (int i=0; i<4; i++) fRec11_perm[i]=fRec11_tmp[count+i];
			
			// LOOP 0x2442d40
			// pre processing
			for (int i=0; i<4; i++) fRec17_tmp[i]=fRec17_perm[i];
			// exec code
			for (int i=0; i<count; i++) {
				fZec8[i] = (fSlow16 * fRec17[i-1]);
				fRec17[i] = ((fSlow9 * (((fSlow5 * fRec18[i]) + (fSlow32 * fRec18[i-1])) + (fSlow5 * fRec18[i-2]))) - (fSlow31 * ((fSlow30 * fRec17[i-2]) + fZec8[i])));
			}
			// post processing
			for (int i=0; i<4; i++) fRec17_perm[i]=fRec17_tmp[count+i];
			
			// LOOP 0x2452b90
			// pre processing
			for (int i=0; i<4; i++) fRec20_tmp[i]=fRec20_perm[i];
			// exec code
			for (int i=0; i<count; i++) {
				fRec20[i] = (fRec21[i] - (fSlow18 * ((fSlow17 * fRec20[i-2]) + (fSlow16 * fRec20[i-1]))));
			}
			// post processing
			for (int i=0; i<4; i++) fRec20_perm[i]=fRec20_tmp[count+i];
			
			// LOOP 0x2456630
			// pre processing
			for (int i=0; i<4; i++) fRec25_tmp[i]=fRec25_perm[i];
			// exec code
			for (int i=0; i<count; i++) {
				fRec25[i] = (fRec26[i] - (fSlow18 * ((fSlow17 * fRec25[i-2]) + (fSlow16 * fRec25[i-1]))));
			}
			// post processing
			for (int i=0; i<4; i++) fRec25_perm[i]=fRec25_tmp[count+i];
			
			// SECTION : 8
			// LOOP 0x2431d80
			// exec code
			for (int i=0; i<count; i++) {
				fZec2[i] = (fSlow18 * (fRec0[i-2] + (fRec0[i] + (2 * fRec0[i-1]))));
			}
			
			// LOOP 0x2433c60
			// exec code
			for (int i=0; i<count; i++) {
				fZec4[i] = (fSlow18 * (fRec7[i-2] + (fRec7[i] + (2 * fRec7[i-1]))));
			}
			
			// LOOP 0x24478a0
			// exec code
			for (int i=0; i<count; i++) {
				fZec9[i] = (fRec11[i-2] + (fSlow31 * (fZec7[i] + (fSlow30 * fRec11[i]))));
			}
			
			// LOOP 0x2449d90
			// exec code
			for (int i=0; i<count; i++) {
				fZec11[i] = (fRec17[i-2] + (fSlow31 * (fZec8[i] + (fSlow30 * fRec17[i]))));
			}
			
			// LOOP 0x24593f0
			// exec code
			for (int i=0; i<count; i++) {
				fZec14[i] = (fSlow18 * (((fSlow15 * fRec20[i]) + (fSlow45 * fRec20[i-1])) + (fSlow15 * fRec20[i-2])));
			}
			
			// LOOP 0x245beb0
			// exec code
			for (int i=0; i<count; i++) {
				fZec16[i] = (fSlow18 * (((fSlow15 * fRec25[i]) + (fSlow45 * fRec25[i-1])) + (fSlow15 * fRec25[i-2])));
			}
			
			// SECTION : 9
			// LOOP 0x2431ca0
			// exec code
			for (int i=0; i<count; i++) {
				fZec3[i] = ((iSlow19)?0:fZec2[i]);
			}
			
			// LOOP 0x2433b80
			// exec code
			for (int i=0; i<count; i++) {
				fZec5[i] = ((iSlow19)?0:fZec4[i]);
			}
			
			// LOOP 0x24477c0
			// exec code
			for (int i=0; i<count; i++) {
				fZec10[i] = ((iSlow33)?0:fZec9[i]);
			}
			
			// LOOP 0x2449cb0
			// exec code
			for (int i=0; i<count; i++) {
				fZec12[i] = ((iSlow33)?0:fZec11[i]);
			}
			
			// LOOP 0x2459310
			// exec code
			for (int i=0; i<count; i++) {
				fZec15[i] = ((iSlow46)?0:fZec14[i]);
			}
			
			// LOOP 0x245bdd0
			// exec code
			for (int i=0; i<count; i++) {
				fZec17[i] = ((iSlow46)?0:fZec16[i]);
			}
			
			// SECTION : 10
			// LOOP 0x2431bc0
			// exec code
			for (int i=0; i<count; i++) {
				fZec6[i] = fabsf((fabsf((fSlow20 * fZec5[i])) + fabsf((fSlow20 * fZec3[i]))));
			}
			
			// LOOP 0x24476e0
			// exec code
			for (int i=0; i<count; i++) {
				fZec13[i] = fabsf((fabsf((fSlow34 * fZec12[i])) + fabsf((fSlow34 * fZec10[i]))));
			}
			
			// LOOP 0x2459230
			// exec code
			for (int i=0; i<count; i++) {
				fZec18[i] = fabsf((fabsf((fSlow47 * fZec17[i])) + fabsf((fSlow47 * fZec15[i]))));
			}
			
			// SECTION : 11
			// LOOP 0x242b150
			// pre processing
			for (int i=0; i<4; i++) fRec6_tmp[i]=fRec6_perm[i];
			// exec code
			for (int i=0; i<count; i++) {
				fRec6[i] = ((fSlow22 * fZec6[i]) + (fSlow21 * max(fZec6[i], fRec6[i-1])));
			}
			// post processing
			for (int i=0; i<4; i++) fRec6_perm[i]=fRec6_tmp[count+i];
			
			// LOOP 0x2442940
			// pre processing
			for (int i=0; i<4; i++) fRec16_tmp[i]=fRec16_perm[i];
			// exec code
			for (int i=0; i<count; i++) {
				fRec16[i] = ((fSlow36 * fZec13[i]) + (fSlow35 * max(fZec13[i], fRec16[i-1])));
			}
			// post processing
			for (int i=0; i<4; i++) fRec16_perm[i]=fRec16_tmp[count+i];
			
			// LOOP 0x24561d0
			// pre processing
			for (int i=0; i<4; i++) fRec24_tmp[i]=fRec24_perm[i];
			// exec code
			for (int i=0; i<count; i++) {
				fRec24[i] = ((fSlow49 * fZec18[i]) + (fSlow48 * max(fZec18[i], fRec24[i-1])));
			}
			// post processing
			for (int i=0; i<4; i++) fRec24_perm[i]=fRec24_tmp[count+i];
			
			// SECTION : 12
			// LOOP 0x242acf0
			// pre processing
			for (int i=0; i<4; i++) fRec5_tmp[i]=fRec5_perm[i];
			// exec code
			for (int i=0; i<count; i++) {
				fRec5[i] = ((fSlow25 * fRec6[i]) + (fSlow24 * fRec5[i-1]));
			}
			// post processing
			for (int i=0; i<4; i++) fRec5_perm[i]=fRec5_tmp[count+i];
			
			// LOOP 0x24424e0
			// pre processing
			for (int i=0; i<4; i++) fRec15_tmp[i]=fRec15_perm[i];
			// exec code
			for (int i=0; i<count; i++) {
				fRec15[i] = ((fSlow39 * fRec16[i]) + (fSlow38 * fRec15[i-1]));
			}
			// post processing
			for (int i=0; i<4; i++) fRec15_perm[i]=fRec15_tmp[count+i];
			
			// LOOP 0x2455d70
			// pre processing
			for (int i=0; i<4; i++) fRec23_tmp[i]=fRec23_perm[i];
			// exec code
			for (int i=0; i<count; i++) {
				fRec23[i] = ((fSlow52 * fRec24[i]) + (fSlow51 * fRec23[i-1]));
			}
			// post processing
			for (int i=0; i<4; i++) fRec23_perm[i]=fRec23_tmp[count+i];
			
			// SECTION : 13
			// LOOP 0x242a930
			// pre processing
			for (int i=0; i<4; i++) fRec4_tmp[i]=fRec4_perm[i];
			// exec code
			for (int i=0; i<count; i++) {
				fRec4[i] = ((fSlow28 * max(((8.685889638065037f * ((8.262958288192749e-08f * float(pun_float_to_int(fRec5[i]))) - 87.989971088f)) - fSlow27), 0.0f)) + (fSlow26 * fRec4[i-1]));
			}
			// post processing
			for (int i=0; i<4; i++) fRec4_perm[i]=fRec4_tmp[count+i];
			
			// LOOP 0x2442100
			// pre processing
			for (int i=0; i<4; i++) fRec14_tmp[i]=fRec14_perm[i];
			// exec code
			for (int i=0; i<count; i++) {
				fRec14[i] = ((fSlow42 * max(((8.685889638065037f * ((8.262958288192749e-08f * float(pun_float_to_int(fRec15[i]))) - 87.989971088f)) - fSlow41), 0.0f)) + (fSlow40 * fRec14[i-1]));
			}
			// post processing
			for (int i=0; i<4; i++) fRec14_perm[i]=fRec14_tmp[count+i];
			
			// LOOP 0x24559b0
			// pre processing
			for (int i=0; i<4; i++) fRec22_tmp[i]=fRec22_perm[i];
			// exec code
			for (int i=0; i<count; i++) {
				fRec22[i] = ((fSlow55 * max(((8.685889638065037f * ((8.262958288192749e-08f * float(pun_float_to_int(fRec23[i]))) - 87.989971088f)) - fSlow54), 0.0f)) + (fSlow53 * fRec22[i-1]));
			}
			// post processing
			for (int i=0; i<4; i++) fRec22_perm[i]=fRec22_tmp[count+i];
			
			// SECTION : 14
			// LOOP 0x2467870
			// exec code
			for (int i=0; i<count; i++) {
				fZec19[i] = pun_int_to_float((8388608 * (126.94269504f + max(-126.0f, (0.16609640464202244f * fRec4[i])))));
			}
			
			// LOOP 0x246f5c0
			// exec code
			for (int i=0; i<count; i++) {
				fZec23[i] = pun_int_to_float((8388608 * (126.94269504f + max(-126.0f, (0.16609640464202244f * fRec14[i])))));
			}
			
			// LOOP 0x24768c0
			// exec code
			for (int i=0; i<count; i++) {
				fZec27[i] = pun_int_to_float((8388608 * (126.94269504f + max(-126.0f, (0.16609640464202244f * fRec22[i])))));
			}
			
			// SECTION : 15
			// LOOP 0x2467790
			// exec code
			for (int i=0; i<count; i++) {
				fZec20[i] = (fZec3[i] * fZec19[i]);
			}
			
			// LOOP 0x2468d60
			// exec code
			for (int i=0; i<count; i++) {
				fZec21[i] = (fSlow20 * (fZec5[i] * fZec19[i]));
			}
			
			// LOOP 0x246f4e0
			// exec code
			for (int i=0; i<count; i++) {
				fZec24[i] = (fZec10[i] * fZec23[i]);
			}
			
			// LOOP 0x2470620
			// exec code
			for (int i=0; i<count; i++) {
				fZec25[i] = (fSlow34 * (fZec12[i] * fZec23[i]));
			}
			
			// LOOP 0x24767e0
			// exec code
			for (int i=0; i<count; i++) {
				fZec28[i] = (fZec15[i] * fZec27[i]);
			}
			
			// LOOP 0x2477920
			// exec code
			for (int i=0; i<count; i++) {
				fZec29[i] = (fSlow47 * (fZec17[i] * fZec27[i]));
			}
			
			// SECTION : 16
			// LOOP 0x24676b0
			// exec code
			for (int i=0; i<count; i++) {
				fZec22[i] = fabsf((fabsf(fZec21[i]) + fabsf((fSlow20 * fZec20[i]))));
			}
			
			// LOOP 0x246f400
			// exec code
			for (int i=0; i<count; i++) {
				fZec26[i] = fabsf((fabsf(fZec25[i]) + fabsf((fSlow34 * fZec24[i]))));
			}
			
			// LOOP 0x2476700
			// exec code
			for (int i=0; i<count; i++) {
				fZec30[i] = fabsf((fabsf(fZec29[i]) + fabsf((fSlow47 * fZec28[i]))));
			}
			
			// SECTION : 17
			// LOOP 0x2467290
			// pre processing
			for (int i=0; i<4; i++) fRec32_tmp[i]=fRec32_perm[i];
			// exec code
			for (int i=0; i<count; i++) {
				fRec32[i] = ((fSlow22 * fZec22[i]) + (fSlow21 * max(fZec22[i], fRec32[i-1])));
			}
			// post processing
			for (int i=0; i<4; i++) fRec32_perm[i]=fRec32_tmp[count+i];
			
			// LOOP 0x246efe0
			// pre processing
			for (int i=0; i<4; i++) fRec35_tmp[i]=fRec35_perm[i];
			// exec code
			for (int i=0; i<count; i++) {
				fRec35[i] = ((fSlow36 * fZec26[i]) + (fSlow35 * max(fZec26[i], fRec35[i-1])));
			}
			// post processing
			for (int i=0; i<4; i++) fRec35_perm[i]=fRec35_tmp[count+i];
			
			// LOOP 0x24762e0
			// pre processing
			for (int i=0; i<4; i++) fRec38_tmp[i]=fRec38_perm[i];
			// exec code
			for (int i=0; i<count; i++) {
				fRec38[i] = ((fSlow49 * fZec30[i]) + (fSlow48 * max(fZec30[i], fRec38[i-1])));
			}
			// post processing
			for (int i=0; i<4; i++) fRec38_perm[i]=fRec38_tmp[count+i];
			
			// SECTION : 18
			// LOOP 0x2466eb0
			// pre processing
			for (int i=0; i<4; i++) fRec31_tmp[i]=fRec31_perm[i];
			// exec code
			for (int i=0; i<count; i++) {
				fRec31[i] = ((fSlow25 * fRec32[i]) + (fSlow24 * fRec31[i-1]));
			}
			// post processing
			for (int i=0; i<4; i++) fRec31_perm[i]=fRec31_tmp[count+i];
			
			// LOOP 0x246ec00
			// pre processing
			for (int i=0; i<4; i++) fRec34_tmp[i]=fRec34_perm[i];
			// exec code
			for (int i=0; i<count; i++) {
				fRec34[i] = ((fSlow39 * fRec35[i]) + (fSlow38 * fRec34[i-1]));
			}
			// post processing
			for (int i=0; i<4; i++) fRec34_perm[i]=fRec34_tmp[count+i];
			
			// LOOP 0x2475f00
			// pre processing
			for (int i=0; i<4; i++) fRec37_tmp[i]=fRec37_perm[i];
			// exec code
			for (int i=0; i<count; i++) {
				fRec37[i] = ((fSlow52 * fRec38[i]) + (fSlow51 * fRec37[i-1]));
			}
			// post processing
			for (int i=0; i<4; i++) fRec37_perm[i]=fRec37_tmp[count+i];
			
			// SECTION : 19
			// LOOP 0x24669f0
			// pre processing
			for (int i=0; i<4; i++) fRec30_tmp[i]=fRec30_perm[i];
			// exec code
			for (int i=0; i<count; i++) {
				fRec30[i] = ((fSlow28 * max(((8.685889638065037f * ((8.262958288192749e-08f * float(pun_float_to_int(fRec31[i]))) - 87.989971088f)) - fSlow27), 0.0f)) + (fSlow26 * fRec30[i-1]));
			}
			// post processing
			for (int i=0; i<4; i++) fRec30_perm[i]=fRec30_tmp[count+i];
			
			// LOOP 0x246e760
			// pre processing
			for (int i=0; i<4; i++) fRec33_tmp[i]=fRec33_perm[i];
			// exec code
			for (int i=0; i<count; i++) {
				fRec33[i] = ((fSlow42 * max(((8.685889638065037f * ((8.262958288192749e-08f * float(pun_float_to_int(fRec34[i]))) - 87.989971088f)) - fSlow41), 0.0f)) + (fSlow40 * fRec33[i-1]));
			}
			// post processing
			for (int i=0; i<4; i++) fRec33_perm[i]=fRec33_tmp[count+i];
			
			// LOOP 0x2475a80
			// pre processing
			for (int i=0; i<4; i++) fRec36_tmp[i]=fRec36_perm[i];
			// exec code
			for (int i=0; i<count; i++) {
				fRec36[i] = ((fSlow55 * max(((8.685889638065037f * ((8.262958288192749e-08f * float(pun_float_to_int(fRec37[i]))) - 87.989971088f)) - fSlow54), 0.0f)) + (fSlow53 * fRec36[i-1]));
			}
			// post processing
			for (int i=0; i<4; i++) fRec36_perm[i]=fRec36_tmp[count+i];
			
			// SECTION : 20
			// LOOP 0x247c690
			// exec code
			for (int i=0; i<count; i++) {
				fZec32[i] = (fSlow63 * fZec24[i]);
			}
			
			// LOOP 0x247d300
			// exec code
			for (int i=0; i<count; i++) {
				fZec31[i] = (fSlow61 * fZec28[i]);
			}
			
			// LOOP 0x2483570
			// exec code
			for (int i=0; i<count; i++) {
				fZec33[i] = (fSlow65 * fZec20[i]);
			}
			
			// LOOP 0x24893c0
			// exec code
			for (int i=0; i<count; i++) {
				fbargraph0 = (0.5f * pun_int_to_float((8388608 * (126.94269504f + max(-126.0f, (0.16609640464202244f * fRec36[i]))))));
				fZec36[i] = (fSlow60 * fZec29[i]);
			}
			
			// LOOP 0x248ca80
			// exec code
			for (int i=0; i<count; i++) {
				fbargraph2 = (0.5f * pun_int_to_float((8388608 * (126.94269504f + max(-126.0f, (0.16609640464202244f * fRec33[i]))))));
				fZec37[i] = (fSlow62 * fZec25[i]);
			}
			
			// LOOP 0x2490a80
			// exec code
			for (int i=0; i<count; i++) {
				fbargraph4 = (0.5f * pun_int_to_float((8388608 * (126.94269504f + max(-126.0f, (0.16609640464202244f * fRec30[i]))))));
				fZec38[i] = (fSlow64 * fZec21[i]);
			}
			
			// SECTION : 21
			// LOOP 0x247d220
			// exec code
			for (int i=0; i<count; i++) {
				fZec34[i] = (((iSlow19)?fZec2[i]:fZec33[i]) + (((iSlow33)?fZec9[i]:fZec32[i]) + ((iSlow46)?fZec14[i]:fZec31[i])));
			}
			
			// LOOP 0x24891e0
			// exec code
			for (int i=0; i<count; i++) {
				fbargraph1 = (fabsf(fZec36[i]) + fabsf(fZec31[i]));
				fbargraph3 = (fabsf(fZec37[i]) + fabsf(fZec32[i]));
				fbargraph5 = (fabsf(fZec38[i]) + fabsf(fZec33[i]));
				fZec39[i] = (((iSlow19)?fZec4[i]:fZec38[i]) + (((iSlow33)?fZec11[i]:fZec37[i]) + ((iSlow46)?fZec16[i]:fZec36[i])));
			}
			
			// SECTION : 22
			// LOOP 0x247d140
			// exec code
			for (int i=0; i<count; i++) {
				fZec35[i] = ((iSlow66)?0:fZec34[i]);
			}
			
			// LOOP 0x2489100
			// exec code
			for (int i=0; i<count; i++) {
				fZec40[i] = ((iSlow66)?0:fZec39[i]);
			}
			
			// SECTION : 23
			// LOOP 0x247d060
			// exec code
			for (int i=0; i<count; i++) {
				fZec41[i] = fabsf((fabsf((fSlow67 * fZec40[i])) + fabsf((fSlow67 * fZec35[i]))));
			}
			
			// SECTION : 24
			// LOOP 0x2466490
			// pre processing
			for (int i=0; i<4; i++) fRec29_tmp[i]=fRec29_perm[i];
			// exec code
			for (int i=0; i<count; i++) {
				fRec29[i] = ((fSlow69 * fZec41[i]) + (fSlow68 * max(fZec41[i], fRec29[i-1])));
			}
			// post processing
			for (int i=0; i<4; i++) fRec29_perm[i]=fRec29_tmp[count+i];
			
			// SECTION : 25
			// LOOP 0x2466050
			// pre processing
			for (int i=0; i<4; i++) fRec28_tmp[i]=fRec28_perm[i];
			// exec code
			for (int i=0; i<count; i++) {
				fRec28[i] = ((fSlow72 * fRec29[i]) + (fSlow71 * fRec28[i-1]));
			}
			// post processing
			for (int i=0; i<4; i++) fRec28_perm[i]=fRec28_tmp[count+i];
			
			// SECTION : 26
			// LOOP 0x2465cb0
			// pre processing
			for (int i=0; i<4; i++) fRec27_tmp[i]=fRec27_perm[i];
			// exec code
			for (int i=0; i<count; i++) {
				fRec27[i] = ((fSlow74 * max((6 + (8.685889638065037f * ((8.262958288192749e-08f * float(pun_float_to_int(fRec28[i]))) - 87.989971088f))), 0.0f)) + (fSlow73 * fRec27[i-1]));
			}
			// post processing
			for (int i=0; i<4; i++) fRec27_perm[i]=fRec27_tmp[count+i];
			
			// SECTION : 27
			// LOOP 0x249efc0
			// exec code
			for (int i=0; i<count; i++) {
				fZec42[i] = pun_int_to_float((8388608 * (126.94269504f + max(-126.0f, (0.16609640464202244f * fRec27[i])))));
			}
			
			// SECTION : 28
			// LOOP 0x241eb10
			// exec code
			for (int i=0; i<count; i++) {
				output0[i] = (FAUSTFLOAT)(fSlow76 * ((iSlow66)?fZec34[i]:(fSlow75 * (fZec35[i] * fZec42[i]))));
			}
			
			// LOOP 0x24a4870
			// exec code
			for (int i=0; i<count; i++) {
				output1[i] = (FAUSTFLOAT)(fSlow76 * ((iSlow66)?fZec39[i]:(fSlow75 * (fZec40[i] * fZec42[i]))));
			}
			
		}
	}
};





#include "SoundPlugin.h"
#include "SoundPlugin_proc.h"

#include "Mixer_proc.h"
#include "SoundPluginRegistry_proc.h"

#include "Faust_plugins_proc.h"


namespace{

class MyUI : public UI
{

 public:

  MyUI() 
    : next_peak(NULL)
    , _gate_control(NULL)
    , _freq_control(NULL)
    , _gain_control(NULL)
    , _num_effects(0)
    , _effect_tooltip("")
    , _curr_box_name(NULL)
  { }

  ~MyUI() {	}

  float *next_peak;

  float *_gate_control;
  float *_freq_control;
  float *_gain_control;

  struct Controller{
    float* control_port;

    float *peak_port;

    float min_value;
    float default_value;
    float max_value;

    std::string name;
    int type;

    const char *tooltip;
    const char *unit;

    Controller(float *control_port)
      : control_port(control_port)
      , peak_port(NULL)
      , min_value(0.0f)
      , default_value(0.5f)
      , max_value(1.0f)
      , name("<no name>")
      , type(EFFECT_FORMAT_FLOAT)
      , tooltip("")
      , unit("")
    { }
  };

  std::vector<Controller> _controllers;

  int get_controller_num(float *control_port){
    for(unsigned int i=0;i<_controllers.size();i++){
      if(control_port == _controllers.at(i).control_port)
        return i;
    }

    Controller controller(control_port);

    _controllers.push_back(controller);
    _num_effects++;

    return _controllers.size()-1;
  }

  bool is_instrument(){
    if(_gate_control!=NULL && _freq_control!=NULL && _gain_control!=NULL)
      return true;
    else
      return false;
  }

  // Remove gain/gate/freq sliders for instruments.
  void remove_instrument_notecontrol_effects(){
    if(is_instrument()){
      _controllers.erase(_controllers.begin() + get_controller_num(_gate_control));
      _controllers.erase(_controllers.begin() + get_controller_num(_freq_control));
      _controllers.erase(_controllers.begin() + get_controller_num(_gain_control));
      _num_effects -= 3;
    }
  }

  // We don't use passive items. (it would have been nice to have a passive effect telling when an instrument is finished playing)
  void remove_last_item(){
    _controllers.pop_back();
    _num_effects--;
  }
  

  int _num_effects;

  const char *_effect_tooltip;

  const char* _curr_box_name;

  // -- widget's layouts
  
  void openFrameBox(const char* label) {_curr_box_name = label;}
  void openTabBox(const char* label) {_curr_box_name = label;}
  void openHorizontalBox(const char* label) {_curr_box_name = label;}
  void openVerticalBox(const char* label) {_curr_box_name = label;}
  void closeBox() {_curr_box_name = NULL;}
  
  // -- active widgets

  void addEffect(const char *name, float* control_port, int type, float min_value, float default_value, float max_value){
    int effect_num = get_controller_num(control_port);

    Controller *controller = &_controllers.at(effect_num);

    if(_curr_box_name != NULL && strlen(_curr_box_name) < 10){
      controller->name = std::string(_curr_box_name) + ": " + name;
    }else{
      controller->name = name;
    }
    //printf("Controller name: \"%s\"\n",controller->name.c_str());
    controller->type = type;
    controller->min_value = min_value;
    controller->default_value = default_value;
    controller->max_value = max_value;

    if(next_peak != NULL){
      controller->peak_port = next_peak;
      next_peak = NULL;
    }

    if(!strcmp(name,"gate"))
      _gate_control = control_port;

    if(!strcmp(name,"freq"))
      _freq_control = control_port;

    if(!strcmp(name,"gain"))
      _gain_control = control_port;
  }

  void addButton(const char* label, float* zone) {
    addEffect(label, zone, EFFECT_FORMAT_BOOL, 0, 0, 1);
  }
  void addToggleButton(const char* label, float* zone) {
    addEffect(label, zone, EFFECT_FORMAT_BOOL, 0, 0, 1);
  }
  void addCheckButton(const char* label, float* zone) {
    addEffect(label, zone, EFFECT_FORMAT_BOOL, 0, 0, 1);
  }
  void addVerticalSlider(const char* label, float* zone, float init, float min, float max, float step) {
    addEffect(label, zone,  step==1.0f ? EFFECT_FORMAT_INT : EFFECT_FORMAT_FLOAT, min, init, max);
  }
  void addHorizontalSlider(const char* label, float* zone, float init, float min, float max, float step) {
    addEffect(label, zone,  step==1.0f ? EFFECT_FORMAT_INT : EFFECT_FORMAT_FLOAT, min, init, max);
  }
  void addNumEntry(const char* label, float* zone, float init, float min, float max, float step) {
    addEffect(label, zone, step==1.0f ? EFFECT_FORMAT_INT : EFFECT_FORMAT_FLOAT, min, init, max); // The INT effect format might not work. Need to go through the code first.
  }
  
  // -- passive widgets

  void addNumDisplay(const char* label, float* zone, int precision) {remove_last_item();}
  void addTextDisplay(const char* label, float* zone, const char* names[], float min, float max) {remove_last_item();}
  void addHorizontalBargraph(const char* label, float* zone, float min, float max) {
    remove_last_item(); // remove metadata
    next_peak = zone;
  }
  void addVerticalBargraph(const char* label, float* zone, float min, float max) {
    remove_last_item(); // remove metadata
    next_peak = zone;
  }
  
  // -- metadata declarations
  
  void declare(float* control_port, const char* key, const char* value) {
    if(control_port==NULL){
      if(!strcmp(key,"tooltip"))
        _effect_tooltip = value;
    } else {
      int effect_num = get_controller_num(control_port);
      Controller *controller = &_controllers.at(effect_num);
      if(!strcmp(key,"tooltip"))
        controller->tooltip = value;
      else if(!strcmp(key,"unit"))
        controller->unit = value;
    }
  }
};


#define MAX_POLYPHONY 32

struct Voice{
  struct Voice *prev;
  struct Voice *next;
  dsp *dsp_instance;
  MyUI myUI;
  float note_num;
  int64_t note_id;

  int frames_since_stop;

  int delta_pos_at_start; // Within the current block. Set when starting a note.
  int delta_pos_at_end; // Within the current block. Set when stopping a note.

  Voice()
    : prev(NULL)
    , next(NULL)
    , dsp_instance(NULL)
    , note_num(0)
    , note_id(-1)
    , delta_pos_at_start(0)
    , delta_pos_at_end(-1)
  { }
};

struct Data{
  Voice *voices_playing; // not used by effects
  Voice *voices_not_playing; // not used by effects
  Voice voices[MAX_POLYPHONY];   // Only voices[0] is used by effects.
  float samplerate;
  Data()
    : voices_playing(NULL)
    , voices_not_playing(NULL)
  {}
};

} // end anonymous namespace

static void RT_add_voice(Voice **root, Voice *voice){
  voice->next = *root;
  if(*root!=NULL)
    (*root)->prev = voice;
  *root = voice;
  voice->prev = NULL;
}

static void RT_remove_voice(Voice **root, Voice *voice){
  if(voice->prev!=NULL)
    voice->prev->next = voice->next;
  else
    *root=voice->next;

  if(voice->next!=NULL)
    voice->next->prev = voice->prev;
}

static bool RT_is_silent(float *sound, int num_frames){
  for(int i=0;i<num_frames;i++)
    if(sound[i]>0.05f)
      return false;
  return true;
}

enum VoiceOp{
  VOICE_KEEP,
  VOICE_REMOVE
};

static void RT_process_between(Voice *voice, float **inputs, float **outputs, int start, int end){
  if(end==start)
    return;

  int num_inputs = voice->dsp_instance->getNumInputs();
  float *offsetted_inputs[num_inputs];
  for(int ch=0;ch<num_inputs; ch++)
    offsetted_inputs[ch] = &inputs[ch][start];

  int num_outputs = voice->dsp_instance->getNumOutputs();
  float *offsetted_outputs[num_outputs];
  for(int ch=0;ch<num_outputs; ch++)
    offsetted_outputs[ch] = &outputs[ch][start];

  //printf("Computing Delta start / end: %d / %d\n",start,end);

  voice->dsp_instance->compute(end-start, offsetted_inputs, offsetted_outputs);
}

static VoiceOp RT_play_voice(Data *data, Voice *voice, int num_frames, float **inputs, float **outputs, int *start_process){

  int delta_pos_at_start = voice->delta_pos_at_start;
  int delta_pos_at_end = voice->delta_pos_at_end;

  if(delta_pos_at_start==0 && delta_pos_at_end==-1){

    voice->dsp_instance->compute(num_frames, inputs, outputs);

    *start_process = 0;

    if( *(voice->myUI._gate_control)==0.0f){
      if(false
         || RT_is_silent(outputs[0],num_frames)
         || voice->frames_since_stop > data->samplerate) // Safety mechanism. Force voice to stop after about 1 second.
        return VOICE_REMOVE;

      voice->frames_since_stop += num_frames;
    }

  }else if(delta_pos_at_start>0 && delta_pos_at_end==-1){

    RT_process_between(voice, inputs, outputs, delta_pos_at_start, num_frames);

    *start_process = delta_pos_at_start;
    voice->delta_pos_at_start = 0;

  }else{
    //printf("Delta start / end: %d / %d\n",delta_pos_at_start,delta_pos_at_end);

    RT_process_between(voice, inputs, outputs, delta_pos_at_start, delta_pos_at_end);
    {
      *(voice->myUI._gate_control)=0.0f;
    }
    RT_process_between(voice, inputs, outputs, delta_pos_at_end, num_frames);

    voice->frames_since_stop = num_frames-delta_pos_at_end;

    *start_process = delta_pos_at_start;
    voice->delta_pos_at_start = 0;
    voice->delta_pos_at_end = -1;

  }

  return VOICE_KEEP;
}

static void RT_process_instrument(SoundPlugin *plugin, int64_t time, int num_frames, float **inputs, float **outputs){
  Data *data = (Data*)plugin->data;
  int num_outputs = plugin->type->num_outputs;

  for(int i=0;i<num_outputs;i++)
    memset(outputs[i],0,num_frames*sizeof(float));

  float *tempsounds[num_outputs];
  float tempdata[num_outputs][num_frames];
  for(int i=0;i<num_outputs;i++)
    tempsounds[i] = &tempdata[i][0];

  Voice *voice = data->voices_playing;
  //printf("Voices? %s\n",voice==NULL?"No":"Yes");

  while(voice!=NULL){
    Voice *next = voice->next;
    int start_process;

    if(RT_play_voice(data,voice,num_frames,inputs,tempsounds,&start_process)==VOICE_REMOVE){
      RT_remove_voice(&data->voices_playing, voice);
      RT_add_voice(&data->voices_not_playing, voice);

      for(int ch=0;ch<num_outputs;ch++)
        RT_fade_out(tempsounds[ch], num_frames-start_process);
    }

    for(int ch=0;ch<num_outputs;ch++){
      float *source = tempsounds[ch];
      float *target = outputs[ch];
      for(int i=start_process;i<num_frames;i++)
        target[i] += source[i];
    }

    voice=next;
  }
}

static void play_note(struct SoundPlugin *plugin, int64_t time, float note_num, int64_t note_id, float volume, float pan){
  Data *data = (Data*)plugin->data;

  //printf("Playing %d\n",note_num);

  Voice *voice = data->voices_not_playing;

  if(voice==NULL){
    printf("no more free voices\n");
    return;
  }

  RT_remove_voice(&data->voices_not_playing, voice);
  RT_add_voice(&data->voices_playing, voice);

  //voice->dsp_instance->init((int)data->samplerate);

  *(voice->myUI._gate_control) = 1.0f;
  *(voice->myUI._freq_control) = midi_to_hz(note_num);
  *(voice->myUI._gain_control) = velocity2gain(volume);

  voice->note_num = note_num;
  voice->note_id = note_id;

  voice->frames_since_stop = 0;
  voice->delta_pos_at_start = time;
  voice->delta_pos_at_end = -1;
}

static void set_note_volume(struct SoundPlugin *plugin, int64_t time, float note_num, int64_t note_id, float volume){
  Data *data = (Data*)plugin->data;
  Voice *voice = data->voices_playing;
  //printf("Setting volume %f / %f\n",volume,velocity2gain(volume));
  while(voice!=NULL){
    if(voice->note_id==note_id)
      *(voice->myUI._gain_control) = velocity2gain(volume);
    voice=voice->next;
  }
}

static void set_note_pitch(struct SoundPlugin *plugin, int64_t time, float note_num, int64_t note_id, float pitch){
  Data *data = (Data*)plugin->data;
  Voice *voice = data->voices_playing;
  //printf("Setting volume %f / %f\n",volume,velocity2gain(volume));
  while(voice!=NULL){
    if(voice->note_id==note_id)
      *(voice->myUI._freq_control) = midi_to_hz(pitch);
    voice=voice->next;
  }
}

static void stop_note(struct SoundPlugin *plugin, int64_t time, float note_num, int64_t note_id){
  Data *data = (Data*)plugin->data;
  Voice *voice = data->voices_playing;
  while(voice!=NULL){
    if(voice->note_id==note_id)
      voice->delta_pos_at_end = time;
    voice=voice->next;
  }
}

static void RT_process_effect(SoundPlugin *plugin, int64_t time, int num_frames, float **inputs, float **outputs){
  //SoundPluginType *type = plugin->type;
  Data *data = (Data*)plugin->data;

  data->voices[0].dsp_instance->compute(num_frames, inputs, outputs);
  //printf("in00: %f, in10: %f\n",inputs[0][0],inputs[1][0]);
  //printf("out00: %f, out10: %f\n",outputs[0][0],outputs[1][0]);
}

static void *create_effect_plugin_data(const SoundPluginType *plugin_type, struct SoundPlugin *plugin, hash_t *state, float samplerate, int blocksize){
  Data *data = new Data;
  data->samplerate = samplerate;

  Voice *voice = &data->voices[0];
  voice->dsp_instance = new CLASSNAME;
  //printf("Creating %s / %s. samplerate: %d\n",plugin_type->type_name,plugin_type->name,(int)samplerate);
  voice->dsp_instance->init(samplerate);
  voice->dsp_instance->buildUserInterface(&voice->myUI);
  return data;
}

static void *create_instrument_plugin_data(const SoundPluginType *plugin_type, struct SoundPlugin *plugin, hash_t *state, float samplerate, int blocksize){
  Data *data = new Data;
  data->samplerate = samplerate;

  for(int i=0;i<MAX_POLYPHONY;i++){
    Voice *voice = &data->voices[i];
    voice->dsp_instance = new CLASSNAME;
    voice->dsp_instance->init(samplerate);
    voice->dsp_instance->buildUserInterface(&voice->myUI);
    voice->myUI.remove_instrument_notecontrol_effects();

    RT_add_voice(&data->voices_not_playing, voice);
  }


  return data;
}

static void cleanup_plugin_data(SoundPlugin *plugin){
  Data *data = (Data*)plugin->data;

  for(int i=0;i<MAX_POLYPHONY;i++){
    Voice *voice = &data->voices[i];
    if(voice->dsp_instance==NULL) // an effect
      break;
    else
      delete voice->dsp_instance;
  }

  delete data;
}

#ifdef FAUST_THAT_ONE
float *FAUST_get_peak_value_pointer(SoundPlugin *plugin, int effect_num){
  Data *data = (Data*)plugin->data;
  MyUI::Controller *controller = &data->voices[0].myUI._controllers.at(effect_num);
  if(controller->peak_port!=NULL)
    return controller->peak_port;
  else
    return NULL;
}
#endif

static int get_effect_format(struct SoundPlugin *plugin, int effect_num){
  const struct SoundPluginType *plugin_type = plugin->type;
  Data *data = (Data*)plugin_type->data;
  Voice *voice = &data->voices[0];
  MyUI::Controller *controller = &voice->myUI._controllers.at(effect_num);
  return controller->type;
}

static const char *get_effect_name(struct SoundPlugin *plugin, int effect_num){
  const struct SoundPluginType *plugin_type = plugin->type;
  Data *data = (Data*)plugin_type->data;
  Voice *voice = &data->voices[0];
  MyUI::Controller *controller = &voice->myUI._controllers.at(effect_num);
  return controller->name.c_str();
}

static void set_effect_value(struct SoundPlugin *plugin, int64_t time, int effect_num, float value, enum ValueFormat value_format, FX_when when){
  Data *data = (Data*)plugin->data;
  float scaled_value;

  if(value_format==PLUGIN_FORMAT_SCALED){
#ifdef DONT_NORMALIZE_EFFECT_VALUES
    scaled_value = value;
#else
    MyUI::Controller *controller = &data->voices[0].myUI._controllers.at(effect_num);
    float min = controller->min_value;
    float max = controller->max_value;
    scaled_value = scale(value,0,1,min,max);
#endif
  }else{
    scaled_value = value;
  }

  //printf("Setting effect %d to %f. input: %f\n",effect_num,scaled_value,value);

  for(int i=0;i<MAX_POLYPHONY;i++){
    Voice *voice = &data->voices[i];
    if(voice->dsp_instance==NULL) // an effect
      break;
    MyUI::Controller *controller = &voice->myUI._controllers.at(effect_num);
    *(controller->control_port) = scaled_value;
  }
}

static float get_effect_value(struct SoundPlugin *plugin, int effect_num, enum ValueFormat value_format){
  Data *data = (Data*)plugin->data;
  Voice *voice = &data->voices[0];
  MyUI::Controller *controller = &voice->myUI._controllers.at(effect_num);

  if(value_format==PLUGIN_FORMAT_SCALED){
#ifdef DONT_NORMALIZE_EFFECT_VALUES
    return *(controller->control_port);
#else
    float min = controller->min_value;
    float max = controller->max_value;
    return scale(*(controller->control_port),min,max,0.0f,1.0f);
#endif
  }else{
    return *(controller->control_port);
  }
}

static void get_display_value_string(struct SoundPlugin *plugin, int effect_num, char *buffer, int buffersize){
  Data *data = (Data*)plugin->data;
  Voice *voice = &data->voices[0];
  MyUI::Controller *controller = &voice->myUI._controllers.at(effect_num);

  if(controller->type==EFFECT_FORMAT_INT)
    snprintf(buffer,buffersize-1,"%d %s",(int)*(controller->control_port), controller->unit);
  else
    snprintf(buffer,buffersize-1,"%.2f %s",*(controller->control_port), controller->unit);
}

static const char *get_effect_description(const struct SoundPluginType *plugin_type, int effect_num){
  Data *data = (Data*)plugin_type->data;
  Voice *voice = &data->voices[0];
  MyUI::Controller *controller = &voice->myUI._controllers.at(effect_num);

  return controller->tooltip;
}

static int get_effect_num(struct SoundPlugin *plugin, const char *effect_name){
  const struct SoundPluginType *plugin_type = plugin->type;
  Data *data = (Data*)plugin_type->data;
  Voice *voice = &data->voices[0];
  for(unsigned int i=0;i<voice->myUI._controllers.size();i++){
    MyUI::Controller *controller = &voice->myUI._controllers.at(i);
    if(!strcmp(controller->name.c_str(),effect_name))
      return i;
  }
  RError("Couldn't find effect name \"%s\" in plugin %s/%s\n",plugin_type->type_name,plugin_type->name);
  return 0;
}

static void fill_type(SoundPluginType *type){
 type->type_name                = "Faust";
 type->note_handling_is_RT      = false;
 type->get_effect_format        = get_effect_format;
 type->get_effect_name          = get_effect_name;
 type->effect_is_RT             = NULL;
 type->cleanup_plugin_data      = cleanup_plugin_data;

 type->play_note       = play_note;
 type->set_note_volume = set_note_volume;
 type->set_note_pitch  = set_note_pitch;

 type->stop_note       = stop_note;

 type->set_effect_value         = set_effect_value;
 type->get_effect_value         = get_effect_value;
 type->get_display_value_string = get_display_value_string;
 type->get_effect_description   = get_effect_description;

 type->get_effect_num = get_effect_num;

 type->data                     = NULL;
};

static SoundPluginType faust_type = {0};

void CREATE_NAME (void){
  fill_type(&faust_type);

  Data *data = (Data*)create_effect_plugin_data(&faust_type, NULL, NULL, MIXER_get_sample_rate(), MIXER_get_buffer_size());
  faust_type.data = data;

  faust_type.name = DSP_NAME;

  faust_type.num_inputs = data->voices[0].dsp_instance->getNumInputs();
  faust_type.num_outputs = data->voices[0].dsp_instance->getNumOutputs();

  if(data->voices[0].myUI.is_instrument()){

    faust_type.is_instrument      = true;

    faust_type.RT_process         = RT_process_instrument;
    faust_type.create_plugin_data = create_instrument_plugin_data;

    data->voices[0].myUI.remove_instrument_notecontrol_effects();

  }else{

    faust_type.is_instrument      = false;

    faust_type.RT_process         = RT_process_effect;
    faust_type.create_plugin_data = create_effect_plugin_data;

    faust_type.play_note          = NULL;
    faust_type.set_note_volume    = NULL;
    faust_type.stop_note          = NULL;

  }

  faust_type.num_effects = data->voices[0].myUI._num_effects;

  PR_add_plugin_type(&faust_type);
}
