//-----------------------------------------------------
//
// Code generated with Faust 0.9.55 (http://faust.grame.fr)
//-----------------------------------------------------
/* link with  */
#include <math.h>
#include <cmath>
template <int N> inline float faustpower(float x) 		{ return powf(x,N); } 
template <int N> inline double faustpower(double x) 	{ return pow(x,N); }
template <int N> inline int faustpower(int x) 			{ return faustpower<N/2>(x) * faustpower<N-N/2>(x); } 
template <> 	 inline int faustpower<0>(int x) 		{ return 1; }
template <> 	 inline int faustpower<1>(int x) 		{ return x; }
#include <math.h>
#include <string>

/*
#include "/usr/share/faust/audio/dsp.h"
#include "/usr/share/faust/gui/UI.h"
*/

#include "faudiostream/architecture/faust/audio/dsp.h"
#include "faudiostream/architecture/faust/gui/UI.h"

struct Meta
{
    void declare (const char* key, const char* value) { }
};

static float scale(float x, float x1, float x2, float y1, float y2){
  return y1 + ( ((x-x1)*(y2-y1))
                /
                (x2-x1)
                );
}

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
	float 	fConst2;
	float 	fConst3;
	FAUSTFLOAT 	fslider1;
	float 	fConst4;
	FAUSTFLOAT 	fslider2;
	FAUSTFLOAT 	fslider3;
	float 	fConst5;
	FAUSTFLOAT 	fslider4;
	float 	fVec0[2];
	float 	fRec9[2];
	float 	fRec8[3];
	float 	fVec1[2];
	float 	fRec7[2];
	float 	fRec6[3];
	FAUSTFLOAT 	fcheckbox0;
	FAUSTFLOAT 	fslider5;
	float 	fVec2[2];
	float 	fRec13[2];
	float 	fRec12[3];
	float 	fVec3[2];
	float 	fRec11[2];
	float 	fRec10[3];
	FAUSTFLOAT 	fslider6;
	float 	fRec5[2];
	float 	fRec4[2];
	FAUSTFLOAT 	fslider7;
	float 	fRec3[2];
	FAUSTFLOAT 	fslider8;
	FAUSTFLOAT 	fcheckbox1;
	FAUSTFLOAT 	fcheckbox2;
	FAUSTFLOAT 	fcheckbox3;
	FAUSTFLOAT 	fslider9;
	FAUSTFLOAT 	fslider10;
	float 	fRec19[2];
	float 	fRec18[3];
	float 	fRec17[3];
	FAUSTFLOAT 	fcheckbox4;
	FAUSTFLOAT 	fslider11;
	float 	fRec22[2];
	float 	fRec21[3];
	float 	fRec20[3];
	FAUSTFLOAT 	fslider12;
	float 	fRec16[2];
	float 	fRec15[2];
	FAUSTFLOAT 	fslider13;
	float 	fRec14[2];
	FAUSTFLOAT 	fslider14;
	FAUSTFLOAT 	fslider15;
	FAUSTFLOAT 	fslider16;
	float 	fRec27[2];
	float 	fRec26[3];
	FAUSTFLOAT 	fcheckbox5;
	FAUSTFLOAT 	fslider17;
	float 	fRec29[2];
	float 	fRec28[3];
	FAUSTFLOAT 	fslider18;
	float 	fRec25[2];
	float 	fRec24[2];
	FAUSTFLOAT 	fslider19;
	float 	fRec23[2];
	FAUSTFLOAT 	fslider20;
	FAUSTFLOAT 	fcheckbox6;
	FAUSTFLOAT 	fslider21;
	float 	fRec32[2];
	float 	fRec31[2];
	float 	fRec30[2];
	FAUSTFLOAT 	fbargraph0;
	FAUSTFLOAT 	fbargraph1;
	float 	fRec35[2];
	float 	fRec34[2];
	float 	fRec33[2];
	FAUSTFLOAT 	fbargraph2;
	FAUSTFLOAT 	fbargraph3;
	float 	fRec38[2];
	float 	fRec37[2];
	float 	fRec36[2];
	FAUSTFLOAT 	fbargraph4;
	FAUSTFLOAT 	fbargraph5;
	FAUSTFLOAT 	fslider22;
	float 	fConst6;
	float 	fRec2[2];
	float 	fRec1[2];
	FAUSTFLOAT 	fslider23;
	float 	fRec0[2];
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
		fslider0 = 8e+02f;
		iConst0 = min(192000, max(1, fSamplingFreq));
		fConst1 = (2e+06f / float(iConst0));
		fConst2 = (1e+06f / float(iConst0));
		fConst3 = (1.0f / float(iConst0));
		fslider1 = 1e+02f;
		fConst4 = (2.0f / float(iConst0));
		fslider2 = -2e+01f;
		fslider3 = 166.0f;
		fConst5 = (3.141592653589793f / float(iConst0));
		fslider4 = 1.5e+03f;
		for (int i=0; i<2; i++) fVec0[i] = 0;
		for (int i=0; i<2; i++) fRec9[i] = 0;
		for (int i=0; i<3; i++) fRec8[i] = 0;
		for (int i=0; i<2; i++) fVec1[i] = 0;
		for (int i=0; i<2; i++) fRec7[i] = 0;
		for (int i=0; i<3; i++) fRec6[i] = 0;
		fcheckbox0 = 0.0;
		fslider5 = 0.0f;
		for (int i=0; i<2; i++) fVec2[i] = 0;
		for (int i=0; i<2; i++) fRec13[i] = 0;
		for (int i=0; i<3; i++) fRec12[i] = 0;
		for (int i=0; i<2; i++) fVec3[i] = 0;
		for (int i=0; i<2; i++) fRec11[i] = 0;
		for (int i=0; i<3; i++) fRec10[i] = 0;
		fslider6 = 2e+02f;
		for (int i=0; i<2; i++) fRec5[i] = 0;
		for (int i=0; i<2; i++) fRec4[i] = 0;
		fslider7 = 2.0f;
		for (int i=0; i<2; i++) fRec3[i] = 0;
		fslider8 = 0.0f;
		fcheckbox1 = 0.0;
		fcheckbox2 = 0.0;
		fcheckbox3 = 0.0;
		fslider9 = 1e+02f;
		fslider10 = -2e+01f;
		for (int i=0; i<2; i++) fRec19[i] = 0;
		for (int i=0; i<3; i++) fRec18[i] = 0;
		for (int i=0; i<3; i++) fRec17[i] = 0;
		fcheckbox4 = 0.0;
		fslider11 = 0.0f;
		for (int i=0; i<2; i++) fRec22[i] = 0;
		for (int i=0; i<3; i++) fRec21[i] = 0;
		for (int i=0; i<3; i++) fRec20[i] = 0;
		fslider12 = 2e+02f;
		for (int i=0; i<2; i++) fRec16[i] = 0;
		for (int i=0; i<2; i++) fRec15[i] = 0;
		fslider13 = 2.0f;
		for (int i=0; i<2; i++) fRec14[i] = 0;
		fslider14 = 0.0f;
		fslider15 = 1e+02f;
		fslider16 = -2e+01f;
		for (int i=0; i<2; i++) fRec27[i] = 0;
		for (int i=0; i<3; i++) fRec26[i] = 0;
		fcheckbox5 = 0.0;
		fslider17 = 0.0f;
		for (int i=0; i<2; i++) fRec29[i] = 0;
		for (int i=0; i<3; i++) fRec28[i] = 0;
		fslider18 = 2e+02f;
		for (int i=0; i<2; i++) fRec25[i] = 0;
		for (int i=0; i<2; i++) fRec24[i] = 0;
		fslider19 = 2.0f;
		for (int i=0; i<2; i++) fRec23[i] = 0;
		fslider20 = 0.0f;
		fcheckbox6 = 0.0;
		fslider21 = 0.0f;
		for (int i=0; i<2; i++) fRec32[i] = 0;
		for (int i=0; i<2; i++) fRec31[i] = 0;
		for (int i=0; i<2; i++) fRec30[i] = 0;
		for (int i=0; i<2; i++) fRec35[i] = 0;
		for (int i=0; i<2; i++) fRec34[i] = 0;
		for (int i=0; i<2; i++) fRec33[i] = 0;
		for (int i=0; i<2; i++) fRec38[i] = 0;
		for (int i=0; i<2; i++) fRec37[i] = 0;
		for (int i=0; i<2; i++) fRec36[i] = 0;
		fslider22 = 5e+02f;
		fConst6 = (1e+03f / float(iConst0));
		for (int i=0; i<2; i++) fRec2[i] = 0;
		for (int i=0; i<2; i++) fRec1[i] = 0;
		fslider23 = 4.0f;
		for (int i=0; i<2; i++) fRec0[i] = 0;
		fslider24 = 0.0f;
		fslider25 = 0.0f;
	}
	virtual void init(int samplingFreq) {
		classInit(samplingFreq);
		instanceInit(samplingFreq);
	}
	virtual void buildUserInterface(UI* interface) {
		interface->openVerticalBox("faust_multibandcomp");
		interface->declare(&fcheckbox3, "0", "");
		interface->declare(&fcheckbox3, "tooltip", "When this is checked, the compressor is enabled. If not, sound is muted.");
		interface->addCheckButton("Band 1:  Solo", &fcheckbox3);
		interface->declare(&fbargraph5, "1", "");
		interface->declare(&fbargraph5, "7", "");
		interface->addHorizontalBargraph("Band 1:   Outgain", &fbargraph5, 0.0f, 1.0f);
		interface->declare(&fcheckbox5, "0.5", "");
		interface->declare(&fcheckbox5, "tooltip", "When this is checked, the compressor is enabled. If not, sound is muted.");
		interface->addCheckButton("Band 1: Bypass", &fcheckbox5);
		interface->declare(&fslider19, "2", "");
		interface->declare(&fslider19, "style", "slider");
		interface->declare(&fslider19, "tooltip", "A compression Ratio of N means that for each N dB increase in input signal level above Threshold, the output level goes up 1 dB");
		interface->addHorizontalSlider("Band 1: Ratio", &fslider19, 2.0f, 1.0f, 2e+01f, 0.1f);
		interface->declare(&fslider16, "3", "");
		interface->declare(&fslider16, "style", "slider");
		interface->declare(&fslider16, "tooltip", "When the signal level exceeds the Threshold (in dB), its level is compressed according to the Ratio");
		interface->declare(&fslider16, "unit", "dB");
		interface->addHorizontalSlider("Band 1: Threshold", &fslider16, -2e+01f, -1e+02f, 1e+01f, 0.1f);
		interface->declare(&fslider15, "4", "");
		interface->declare(&fslider15, "style", "slider");
		interface->declare(&fslider15, "tooltip", "Time constant in ms (1/e smoothing time) for the compression gain to approach (exponentially) a new lower target level (the compression `kicking in')");
		interface->declare(&fslider15, "unit", "ms");
		interface->addHorizontalSlider("Band 1: Attack", &fslider15, 1e+02f, 0.0f, 5e+02f, 0.1f);
		interface->declare(&fslider18, "5", "");
		interface->declare(&fslider18, "style", "slider");
		interface->declare(&fslider18, "tooltip", "Time constant in ms (1/e smoothing time) for the compression gain to approach (exponentially) a new higher target level (the compression 'releasing')");
		interface->declare(&fslider18, "unit", "ms");
		interface->addHorizontalSlider("Band 1: Release", &fslider18, 2e+02f, 0.0f, 1e+03f, 0.1f);
		interface->declare(&fbargraph4, "1", "");
		interface->declare(&fbargraph4, "6", "");
		interface->declare(&fbargraph4, "tooltip", "dummy tooltip");
		interface->addHorizontalBargraph("Band 1: Input Gain", &fbargraph4, 0.0f, 1.0f);
		interface->declare(&fslider17, "2", "");
		interface->declare(&fslider17, "6", "");
		interface->declare(&fslider17, "tooltip", "The compressed-signal output level is increased by this amount (in dB) to make up for the level lost due to compression");
		interface->declare(&fslider17, "unit", "dB");
		interface->addHorizontalSlider("Band 1: Input Gain", &fslider17, 0.0f, -4e+01f, 4e+01f, 0.1f);
		interface->declare(&fslider20, "2", "");
		interface->declare(&fslider20, "7", "");
		interface->declare(&fslider20, "tooltip", "The compressed-signal output level is increased by this amount (in dB) to make up for the level lost due to compression");
		interface->declare(&fslider20, "unit", "dB");
		interface->addHorizontalSlider("Band 1: Output Gain", &fslider20, 0.0f, -4e+01f, 4e+01f, 0.1f);
		interface->declare(&fcheckbox2, "0", "");
		interface->declare(&fcheckbox2, "tooltip", "When this is checked, the compressor is enabled. If not, sound is muted.");
		interface->addCheckButton("Band 2:  Solo", &fcheckbox2);
		interface->declare(&fbargraph1, "1", "");
		interface->declare(&fbargraph1, "7", "");
		interface->addHorizontalBargraph("Band 2:   Outgain", &fbargraph1, 0.0f, 1.0f);
		interface->declare(&fcheckbox0, "0.5", "");
		interface->declare(&fcheckbox0, "tooltip", "When this is checked, the compressor is enabled. If not, sound is muted.");
		interface->addCheckButton("Band 2: Bypass", &fcheckbox0);
		interface->declare(&fslider7, "2", "");
		interface->declare(&fslider7, "style", "slider");
		interface->declare(&fslider7, "tooltip", "A compression Ratio of N means that for each N dB increase in input signal level above Threshold, the output level goes up 1 dB");
		interface->addHorizontalSlider("Band 2: Ratio", &fslider7, 2.0f, 1.0f, 2e+01f, 0.1f);
		interface->declare(&fslider2, "3", "");
		interface->declare(&fslider2, "style", "slider");
		interface->declare(&fslider2, "tooltip", "When the signal level exceeds the Threshold (in dB), its level is compressed according to the Ratio");
		interface->declare(&fslider2, "unit", "dB");
		interface->addHorizontalSlider("Band 2: Threshold", &fslider2, -2e+01f, -1e+02f, 1e+01f, 0.1f);
		interface->declare(&fslider1, "4", "");
		interface->declare(&fslider1, "style", "slider");
		interface->declare(&fslider1, "tooltip", "Time constant in ms (1/e smoothing time) for the compression gain to approach (exponentially) a new lower target level (the compression `kicking in')");
		interface->declare(&fslider1, "unit", "ms");
		interface->addHorizontalSlider("Band 2: Attack", &fslider1, 1e+02f, 0.0f, 5e+02f, 0.1f);
		interface->declare(&fslider6, "5", "");
		interface->declare(&fslider6, "style", "slider");
		interface->declare(&fslider6, "tooltip", "Time constant in ms (1/e smoothing time) for the compression gain to approach (exponentially) a new higher target level (the compression 'releasing')");
		interface->declare(&fslider6, "unit", "ms");
		interface->addHorizontalSlider("Band 2: Release", &fslider6, 2e+02f, 0.0f, 1e+03f, 0.1f);
		interface->declare(&fbargraph0, "1", "");
		interface->declare(&fbargraph0, "6", "");
		interface->declare(&fbargraph0, "tooltip", "dummy tooltip");
		interface->addHorizontalBargraph("Band 2: Input Gain", &fbargraph0, 0.0f, 1.0f);
		interface->declare(&fslider5, "2", "");
		interface->declare(&fslider5, "6", "");
		interface->declare(&fslider5, "tooltip", "The compressed-signal output level is increased by this amount (in dB) to make up for the level lost due to compression");
		interface->declare(&fslider5, "unit", "dB");
		interface->addHorizontalSlider("Band 2: Input Gain", &fslider5, 0.0f, -4e+01f, 4e+01f, 0.1f);
		interface->declare(&fslider8, "2", "");
		interface->declare(&fslider8, "7", "");
		interface->declare(&fslider8, "tooltip", "The compressed-signal output level is increased by this amount (in dB) to make up for the level lost due to compression");
		interface->declare(&fslider8, "unit", "dB");
		interface->addHorizontalSlider("Band 2: Output Gain", &fslider8, 0.0f, -4e+01f, 4e+01f, 0.1f);
		interface->declare(&fcheckbox1, "0", "");
		interface->declare(&fcheckbox1, "tooltip", "When this is checked, the compressor is enabled. If not, sound is muted.");
		interface->addCheckButton("Band 3:  Solo", &fcheckbox1);
		interface->declare(&fbargraph3, "1", "");
		interface->declare(&fbargraph3, "7", "");
		interface->addHorizontalBargraph("Band 3:   Outgain", &fbargraph3, 0.0f, 1.0f);
		interface->declare(&fcheckbox4, "0.5", "");
		interface->declare(&fcheckbox4, "tooltip", "When this is checked, the compressor is enabled. If not, sound is muted.");
		interface->addCheckButton("Band 3: Bypass", &fcheckbox4);
		interface->declare(&fslider13, "2", "");
		interface->declare(&fslider13, "style", "slider");
		interface->declare(&fslider13, "tooltip", "A compression Ratio of N means that for each N dB increase in input signal level above Threshold, the output level goes up 1 dB");
		interface->addHorizontalSlider("Band 3: Ratio", &fslider13, 2.0f, 1.0f, 2e+01f, 0.1f);
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
		interface->declare(&fslider12, "5", "");
		interface->declare(&fslider12, "style", "slider");
		interface->declare(&fslider12, "tooltip", "Time constant in ms (1/e smoothing time) for the compression gain to approach (exponentially) a new higher target level (the compression 'releasing')");
		interface->declare(&fslider12, "unit", "ms");
		interface->addHorizontalSlider("Band 3: Release", &fslider12, 2e+02f, 0.0f, 1e+03f, 0.1f);
		interface->declare(&fbargraph2, "1", "");
		interface->declare(&fbargraph2, "6", "");
		interface->declare(&fbargraph2, "tooltip", "dummy tooltip");
		interface->addHorizontalBargraph("Band 3: Input Gain", &fbargraph2, 0.0f, 1.0f);
		interface->declare(&fslider11, "2", "");
		interface->declare(&fslider11, "6", "");
		interface->declare(&fslider11, "tooltip", "The compressed-signal output level is increased by this amount (in dB) to make up for the level lost due to compression");
		interface->declare(&fslider11, "unit", "dB");
		interface->addHorizontalSlider("Band 3: Input Gain", &fslider11, 0.0f, -4e+01f, 4e+01f, 0.1f);
		interface->declare(&fslider14, "2", "");
		interface->declare(&fslider14, "7", "");
		interface->declare(&fslider14, "tooltip", "The compressed-signal output level is increased by this amount (in dB) to make up for the level lost due to compression");
		interface->declare(&fslider14, "unit", "dB");
		interface->addHorizontalSlider("Band 3: Output Gain", &fslider14, 0.0f, -4e+01f, 4e+01f, 0.1f);
		interface->declare(&fslider3, "C", "");
		interface->declare(&fslider3, "style", "knob");
		interface->declare(&fslider3, "tooltip", "Center-frequency of second-order Regalia-Mitra peaking equalizer section 1");
		interface->declare(&fslider3, "unit", "Hz");
		interface->addVerticalSlider("Split Freq 1", &fslider3, 166.0f, 4e+01f, 999.0f, 1.0f);
		interface->declare(&fslider4, "D", "");
		interface->declare(&fslider4, "style", "knob");
		interface->declare(&fslider4, "tooltip", "Center-frequency of second-order Regalia-Mitra peaking equalizer section 1");
		interface->declare(&fslider4, "unit", "Hz");
		interface->addVerticalSlider("Split Freq 2", &fslider4, 1.5e+03f, 1e+03f, 1.5e+04f, 1.0f);
		interface->declare(&fcheckbox6, "E", "");
		interface->addCheckButton("Limiter Bypass", &fcheckbox6);
		interface->declare(&fslider21, "F", "");
		interface->declare(&fslider21, "tooltip", "Adjust overall gain.");
		interface->declare(&fslider21, "unit", "dB");
		interface->addHorizontalSlider("Limiter Input Gain", &fslider21, 0.0f, -4e+01f, 4e+01f, 0.1f);
		interface->declare(&fslider23, "G", "");
		interface->declare(&fslider23, "unit", ":1");
		interface->addVerticalSlider("Limiter Ratio", &fslider23, 4.0f, 4.0f, 2e+01f, 1.0f);
		interface->declare(&fslider0, "H", "");
		interface->declare(&fslider0, "unit", "us");
		interface->addVerticalSlider("Limiter Attack", &fslider0, 8e+02f, 2e+01f, 8e+02f, 1.0f);
		interface->declare(&fslider22, "I", "");
		interface->declare(&fslider22, "unit", "ms");
		interface->addVerticalSlider("Limiter Release", &fslider22, 5e+02f, 5e+01f, 1.1e+03f, 1.0f);
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
	virtual void compute (int count, FAUSTFLOAT** input, FAUSTFLOAT** output) {
		float 	fSlow0 = fslider0;
		float 	fSlow1 = expf((0 - (fConst1 / fSlow0)));
		float 	fSlow2 = expf((0 - (fConst2 / fSlow0)));
		float 	fSlow3 = max(fConst3, (0.001f * fslider1));
		float 	fSlow4 = expf((0 - (fConst4 / fSlow3)));
		float 	fSlow5 = fslider2;
		float 	fSlow6 = expf((0 - (fConst3 / fSlow3)));
		float 	fSlow7 = tanf((fConst5 * fslider3));
		float 	fSlow8 = (1.0f / faustpower<2>(fSlow7));
		float 	fSlow9 = (2 * (1 - fSlow8));
		float 	fSlow10 = (1.0f / fSlow7);
		float 	fSlow11 = (1 + ((fSlow10 - 1.0000000000000004f) / fSlow7));
		float 	fSlow12 = (1.0f / (1 + ((fSlow10 + 1.0000000000000004f) / fSlow7)));
		float 	fSlow13 = (1 + fSlow10);
		float 	fSlow14 = (0 - ((1 - fSlow10) / fSlow13));
		float 	fSlow15 = tanf((fConst5 * fslider4));
		float 	fSlow16 = (1.0f / faustpower<2>(fSlow15));
		float 	fSlow17 = (2 * (1 - fSlow16));
		float 	fSlow18 = (1.0f / fSlow15);
		float 	fSlow19 = (1 + ((fSlow18 - 1.0000000000000004f) / fSlow15));
		float 	fSlow20 = (1 + ((1.0000000000000004f + fSlow18) / fSlow15));
		float 	fSlow21 = (1.0f / fSlow20);
		float 	fSlow22 = (1 + fSlow18);
		float 	fSlow23 = (0 - ((1 - fSlow18) / fSlow22));
		float 	fSlow24 = (1.0f / fSlow22);
		float 	fSlow25 = (1.0f / (fSlow7 * fSlow20));
		float 	fSlow26 = (0 - fSlow10);
		float 	fSlow27 = (1.0f / fSlow13);
		float 	fSlow28 = (2 * (0 - fSlow8));
		int 	iSlow29 = int(fcheckbox0);
		float 	fSlow30 = powf(10,(0.05f * fslider5));
		float 	fSlow31 = expf((0 - (fConst3 / max(fConst3, (0.001f * fslider6)))));
		float 	fSlow32 = (1.0f - fSlow31);
		float 	fSlow33 = (1.0f - fSlow6);
		float 	fSlow34 = (((1.0f / float(fslider7)) - 1.0f) * (1.0f - fSlow4));
		int 	iSlow35 = int(fcheckbox1);
		int 	iSlow36 = int(fcheckbox2);
		int 	iSlow37 = int(fcheckbox3);
		int 	iSlow38 = (((iSlow37 == 0) & (iSlow36 == 0)) & (iSlow35 == 0));
		float 	fSlow39 = (float((iSlow36 | iSlow38)) * powf(10,(0.05f * fslider8)));
		float 	fSlow40 = (fSlow39 * fSlow30);
		float 	fSlow41 = max(fConst3, (0.001f * fslider9));
		float 	fSlow42 = expf((0 - (fConst4 / fSlow41)));
		float 	fSlow43 = fslider10;
		float 	fSlow44 = expf((0 - (fConst3 / fSlow41)));
		float 	fSlow45 = (1 + ((fSlow10 - 1.0f) / fSlow7));
		float 	fSlow46 = (1.0f / (1 + ((1.0f + fSlow10) / fSlow7)));
		float 	fSlow47 = (0 - fSlow18);
		float 	fSlow48 = (2 * (0 - fSlow16));
		int 	iSlow49 = int(fcheckbox4);
		float 	fSlow50 = powf(10,(0.05f * fslider11));
		float 	fSlow51 = expf((0 - (fConst3 / max(fConst3, (0.001f * fslider12)))));
		float 	fSlow52 = (1.0f - fSlow51);
		float 	fSlow53 = (1.0f - fSlow44);
		float 	fSlow54 = (((1.0f / float(fslider13)) - 1.0f) * (1.0f - fSlow42));
		float 	fSlow55 = (float((iSlow35 | iSlow38)) * powf(10,(0.05f * fslider14)));
		float 	fSlow56 = (fSlow55 * fSlow50);
		float 	fSlow57 = max(fConst3, (0.001f * fslider15));
		float 	fSlow58 = expf((0 - (fConst4 / fSlow57)));
		float 	fSlow59 = fslider16;
		float 	fSlow60 = expf((0 - (fConst3 / fSlow57)));
		int 	iSlow61 = int(fcheckbox5);
		float 	fSlow62 = powf(10,(0.05f * fslider17));
		float 	fSlow63 = expf((0 - (fConst3 / max(fConst3, (0.001f * fslider18)))));
		float 	fSlow64 = (1.0f - fSlow63);
		float 	fSlow65 = (1.0f - fSlow60);
		float 	fSlow66 = (((1.0f / float(fslider19)) - 1.0f) * (1.0f - fSlow58));
		float 	fSlow67 = (float((iSlow37 | iSlow38)) * powf(10,(0.05f * fslider20)));
		float 	fSlow68 = (fSlow67 * fSlow62);
		int 	iSlow69 = int(fcheckbox6);
		float 	fSlow70 = powf(10,(0.05f * fslider21));
		float 	fSlow71 = expf((0 - (fConst6 / fslider22)));
		float 	fSlow72 = (1.0f - fSlow71);
		float 	fSlow73 = (1.0f - fSlow2);
		float 	fSlow74 = (((1.0f / float(fslider23)) - 1.0f) * (1.0f - fSlow1));
		float 	fSlow75 = (powf(10,(0.05f * fslider24)) * fSlow70);
		float 	fSlow76 = powf(10,(0.05f * fslider25));
		FAUSTFLOAT* input0 = input[0];
		FAUSTFLOAT* input1 = input[1];
		FAUSTFLOAT* output0 = output[0];
		FAUSTFLOAT* output1 = output[1];
		for (int i=0; i<count; i++) {
			float fTemp0 = (float)input0[i];
			fVec0[0] = fTemp0;
			fRec9[0] = ((fSlow24 * (fVec0[0] + fVec0[1])) + (fSlow23 * fRec9[1]));
			fRec8[0] = (fRec9[0] - (fSlow21 * ((fSlow19 * fRec8[2]) + (fSlow17 * fRec8[1]))));
			float fTemp1 = (fRec8[2] + (fRec8[0] + (2 * fRec8[1])));
			float fTemp2 = (fSlow21 * fTemp1);
			fVec1[0] = fTemp2;
			fRec7[0] = ((fSlow27 * ((fSlow26 * fVec1[1]) + (fSlow25 * fTemp1))) + (fSlow14 * fRec7[1]));
			fRec6[0] = (fRec7[0] - (fSlow12 * ((fSlow11 * fRec6[2]) + (fSlow9 * fRec6[1]))));
			float fTemp3 = (fSlow12 * (((fSlow8 * fRec6[0]) + (fSlow28 * fRec6[1])) + (fSlow8 * fRec6[2])));
			float fTemp4 = ((iSlow29)?0:fTemp3);
			float fTemp5 = (float)input1[i];
			fVec2[0] = fTemp5;
			fRec13[0] = ((fSlow24 * (fVec2[0] + fVec2[1])) + (fSlow23 * fRec13[1]));
			fRec12[0] = (fRec13[0] - (fSlow21 * ((fSlow19 * fRec12[2]) + (fSlow17 * fRec12[1]))));
			float fTemp6 = (fRec12[2] + (fRec12[0] + (2 * fRec12[1])));
			float fTemp7 = (fSlow21 * fTemp6);
			fVec3[0] = fTemp7;
			fRec11[0] = ((fSlow27 * ((fSlow26 * fVec3[1]) + (fSlow25 * fTemp6))) + (fSlow14 * fRec11[1]));
			fRec10[0] = (fRec11[0] - (fSlow12 * ((fSlow11 * fRec10[2]) + (fSlow9 * fRec10[1]))));
			float fTemp8 = (fSlow12 * (((fSlow8 * fRec10[0]) + (fSlow28 * fRec10[1])) + (fSlow8 * fRec10[2])));
			float fTemp9 = ((iSlow29)?0:fTemp8);
			float fTemp10 = fabsf((fabsf((fSlow30 * fTemp9)) + fabsf((fSlow30 * fTemp4))));
			fRec5[0] = ((fSlow32 * fTemp10) + (fSlow31 * max(fTemp10, fRec5[1])));
			fRec4[0] = ((fSlow33 * fRec5[0]) + (fSlow6 * fRec4[1]));
			fRec3[0] = ((fSlow34 * max(((20 * log10f(fRec4[0])) - fSlow5), 0.0f)) + (fSlow4 * fRec3[1]));
			float fTemp11 = powf(10,(0.05f * fRec3[0]));
			float fTemp12 = (fTemp4 * fTemp11);
			float fTemp13 = (fSlow40 * fTemp12);
			float fTemp14 = (fSlow9 * fRec17[1]);
			fRec19[0] = ((fSlow24 * ((fSlow47 * fVec0[1]) + (fSlow18 * fVec0[0]))) + (fSlow23 * fRec19[1]));
			fRec18[0] = (fRec19[0] - (fSlow21 * ((fSlow19 * fRec18[2]) + (fSlow17 * fRec18[1]))));
			fRec17[0] = ((fSlow21 * (((fSlow16 * fRec18[0]) + (fSlow48 * fRec18[1])) + (fSlow16 * fRec18[2]))) - (fSlow46 * ((fSlow45 * fRec17[2]) + fTemp14)));
			float fTemp15 = (fRec17[2] + (fSlow46 * (fTemp14 + (fSlow45 * fRec17[0]))));
			float fTemp16 = ((iSlow49)?0:fTemp15);
			float fTemp17 = (fSlow9 * fRec20[1]);
			fRec22[0] = ((fSlow24 * ((fSlow47 * fVec2[1]) + (fSlow18 * fVec2[0]))) + (fSlow23 * fRec22[1]));
			fRec21[0] = (fRec22[0] - (fSlow21 * ((fSlow19 * fRec21[2]) + (fSlow17 * fRec21[1]))));
			fRec20[0] = ((fSlow21 * (((fSlow16 * fRec21[0]) + (fSlow48 * fRec21[1])) + (fSlow16 * fRec21[2]))) - (fSlow46 * ((fSlow45 * fRec20[2]) + fTemp17)));
			float fTemp18 = (fRec20[2] + (fSlow46 * (fTemp17 + (fSlow45 * fRec20[0]))));
			float fTemp19 = ((iSlow49)?0:fTemp18);
			float fTemp20 = fabsf((fabsf((fSlow50 * fTemp19)) + fabsf((fSlow50 * fTemp16))));
			fRec16[0] = ((fSlow52 * fTemp20) + (fSlow51 * max(fTemp20, fRec16[1])));
			fRec15[0] = ((fSlow53 * fRec16[0]) + (fSlow44 * fRec15[1]));
			fRec14[0] = ((fSlow54 * max(((20 * log10f(fRec15[0])) - fSlow43), 0.0f)) + (fSlow42 * fRec14[1]));
			float fTemp21 = powf(10,(0.05f * fRec14[0]));
			float fTemp22 = (fTemp16 * fTemp21);
			float fTemp23 = (fSlow56 * fTemp22);
			fRec27[0] = ((fSlow27 * (fVec1[0] + fVec1[1])) + (fSlow14 * fRec27[1]));
			fRec26[0] = (fRec27[0] - (fSlow12 * ((fSlow11 * fRec26[2]) + (fSlow9 * fRec26[1]))));
			float fTemp24 = (fSlow12 * (fRec26[2] + (fRec26[0] + (2 * fRec26[1]))));
			float fTemp25 = ((iSlow61)?0:fTemp24);
			fRec29[0] = ((fSlow27 * (fVec3[0] + fVec3[1])) + (fSlow14 * fRec29[1]));
			fRec28[0] = (fRec29[0] - (fSlow12 * ((fSlow11 * fRec28[2]) + (fSlow9 * fRec28[1]))));
			float fTemp26 = (fSlow12 * (fRec28[2] + (fRec28[0] + (2 * fRec28[1]))));
			float fTemp27 = ((iSlow61)?0:fTemp26);
			float fTemp28 = fabsf((fabsf((fSlow62 * fTemp27)) + fabsf((fSlow62 * fTemp25))));
			fRec25[0] = ((fSlow64 * fTemp28) + (fSlow63 * max(fTemp28, fRec25[1])));
			fRec24[0] = ((fSlow65 * fRec25[0]) + (fSlow60 * fRec24[1]));
			fRec23[0] = ((fSlow66 * max(((20 * log10f(fRec24[0])) - fSlow59), 0.0f)) + (fSlow58 * fRec23[1]));
			float fTemp29 = powf(10,(0.05f * fRec23[0]));
			float fTemp30 = (fTemp25 * fTemp29);
			float fTemp31 = (fSlow68 * fTemp30);
			float fTemp32 = (((iSlow61)?fTemp24:fTemp31) + (((iSlow49)?fTemp15:fTemp23) + ((iSlow29)?fTemp3:fTemp13)));
			float fTemp33 = ((iSlow69)?0:fTemp32);
			float fTemp34 = (fSlow30 * (fTemp9 * fTemp11));
			float fTemp35 = fabsf((fabsf(fTemp34) + fabsf((fSlow30 * fTemp12))));
			fRec32[0] = ((fSlow32 * fTemp35) + (fSlow31 * max(fTemp35, fRec32[1])));
			fRec31[0] = ((fSlow33 * fRec32[0]) + (fSlow6 * fRec31[1]));
			fRec30[0] = ((fSlow34 * max(((20 * log10f(fRec31[0])) - fSlow5), 0.0f)) + (fSlow4 * fRec30[1]));
			fbargraph0 = (0.5f * powf(10,(0.05f * fRec30[0])));
			float fTemp36 = (fSlow39 * fTemp34);
			fbargraph1 = (fabsf(fTemp36) + fabsf(fTemp13));
			float fTemp37 = (fSlow50 * (fTemp19 * fTemp21));
			float fTemp38 = fabsf((fabsf(fTemp37) + fabsf((fSlow50 * fTemp22))));
			fRec35[0] = ((fSlow52 * fTemp38) + (fSlow51 * max(fTemp38, fRec35[1])));
			fRec34[0] = ((fSlow53 * fRec35[0]) + (fSlow44 * fRec34[1]));
			fRec33[0] = ((fSlow54 * max(((20 * log10f(fRec34[0])) - fSlow43), 0.0f)) + (fSlow42 * fRec33[1]));
			fbargraph2 = (0.5f * powf(10,(0.05f * fRec33[0])));
			float fTemp39 = (fSlow55 * fTemp37);
			fbargraph3 = (fabsf(fTemp39) + fabsf(fTemp23));
			float fTemp40 = (fSlow62 * (fTemp27 * fTemp29));
			float fTemp41 = fabsf((fabsf(fTemp40) + fabsf((fSlow62 * fTemp30))));
			fRec38[0] = ((fSlow64 * fTemp41) + (fSlow63 * max(fTemp41, fRec38[1])));
			fRec37[0] = ((fSlow65 * fRec38[0]) + (fSlow60 * fRec37[1]));
			fRec36[0] = ((fSlow66 * max(((20 * log10f(fRec37[0])) - fSlow59), 0.0f)) + (fSlow58 * fRec36[1]));
			fbargraph4 = (0.5f * powf(10,(0.05f * fRec36[0])));
			float fTemp42 = (fSlow67 * fTemp40);
			fbargraph5 = (fabsf(fTemp42) + fabsf(fTemp31));
			float fTemp43 = (((iSlow61)?fTemp26:fTemp42) + (((iSlow49)?fTemp18:fTemp39) + ((iSlow29)?fTemp8:fTemp36)));
			float fTemp44 = ((iSlow69)?0:fTemp43);
			float fTemp45 = fabsf((fabsf((fSlow70 * fTemp44)) + fabsf((fSlow70 * fTemp33))));
			fRec2[0] = ((fSlow72 * fTemp45) + (fSlow71 * max(fTemp45, fRec2[1])));
			fRec1[0] = ((fSlow73 * fRec2[0]) + (fSlow2 * fRec1[1]));
			fRec0[0] = ((fSlow74 * max((6 + (20 * log10f(fRec1[0]))), 0.0f)) + (fSlow1 * fRec0[1]));
			float fTemp46 = powf(10,(0.05f * fRec0[0]));
			output0[i] = (FAUSTFLOAT)(fSlow76 * ((iSlow69)?fTemp32:(fSlow75 * (fTemp33 * fTemp46))));
			output1[i] = (FAUSTFLOAT)(fSlow76 * ((iSlow69)?fTemp43:(fSlow75 * (fTemp44 * fTemp46))));
			// post processing
			fRec0[1] = fRec0[0];
			fRec1[1] = fRec1[0];
			fRec2[1] = fRec2[0];
			fRec36[1] = fRec36[0];
			fRec37[1] = fRec37[0];
			fRec38[1] = fRec38[0];
			fRec33[1] = fRec33[0];
			fRec34[1] = fRec34[0];
			fRec35[1] = fRec35[0];
			fRec30[1] = fRec30[0];
			fRec31[1] = fRec31[0];
			fRec32[1] = fRec32[0];
			fRec23[1] = fRec23[0];
			fRec24[1] = fRec24[0];
			fRec25[1] = fRec25[0];
			fRec28[2] = fRec28[1]; fRec28[1] = fRec28[0];
			fRec29[1] = fRec29[0];
			fRec26[2] = fRec26[1]; fRec26[1] = fRec26[0];
			fRec27[1] = fRec27[0];
			fRec14[1] = fRec14[0];
			fRec15[1] = fRec15[0];
			fRec16[1] = fRec16[0];
			fRec20[2] = fRec20[1]; fRec20[1] = fRec20[0];
			fRec21[2] = fRec21[1]; fRec21[1] = fRec21[0];
			fRec22[1] = fRec22[0];
			fRec17[2] = fRec17[1]; fRec17[1] = fRec17[0];
			fRec18[2] = fRec18[1]; fRec18[1] = fRec18[0];
			fRec19[1] = fRec19[0];
			fRec3[1] = fRec3[0];
			fRec4[1] = fRec4[0];
			fRec5[1] = fRec5[0];
			fRec10[2] = fRec10[1]; fRec10[1] = fRec10[0];
			fRec11[1] = fRec11[0];
			fVec3[1] = fVec3[0];
			fRec12[2] = fRec12[1]; fRec12[1] = fRec12[0];
			fRec13[1] = fRec13[0];
			fVec2[1] = fVec2[0];
			fRec6[2] = fRec6[1]; fRec6[1] = fRec6[0];
			fRec7[1] = fRec7[0];
			fVec1[1] = fVec1[0];
			fRec8[2] = fRec8[1]; fRec8[1] = fRec8[0];
			fRec9[1] = fRec9[0];
			fVec0[1] = fVec0[0];
		}
	}
};





#include "../common/nsmtracker.h"
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
  int note_num;

  int frames_since_stop;

  int delta_pos_at_start; // Within the current block. Set when starting a note.
  int delta_pos_at_end; // Within the current block. Set when stopping a note.

  Voice()
    : prev(NULL)
    , next(NULL)
    , dsp_instance(NULL)
    , note_num(0)
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

static void play_note(struct SoundPlugin *plugin, int64_t time, int note_num, float volume){
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

  voice->frames_since_stop = 0;
  voice->delta_pos_at_start = time;
  voice->delta_pos_at_end = -1;
}

static void set_note_volume(struct SoundPlugin *plugin, int64_t time, int note_num, float volume){
  Data *data = (Data*)plugin->data;
  Voice *voice = data->voices_playing;
  //printf("Setting volume %f / %f\n",volume,velocity2gain(volume));
  while(voice!=NULL){
    if(voice->note_num==note_num)
      *(voice->myUI._gain_control) = velocity2gain(volume);
    voice=voice->next;
  }
}

static void stop_note(struct SoundPlugin *plugin, int64_t time, int note_num, float volume){
  Data *data = (Data*)plugin->data;
  Voice *voice = data->voices_playing;
  while(voice!=NULL){
    if(voice->note_num==note_num)
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

static void *create_effect_plugin_data(const SoundPluginType *plugin_type, struct SoundPlugin *plugin, float samplerate, int blocksize){
  Data *data = new Data;
  data->samplerate = samplerate;

  Voice *voice = &data->voices[0];
  voice->dsp_instance = new CLASSNAME;
  //printf("Creating %s / %s. samplerate: %d\n",plugin_type->type_name,plugin_type->name,(int)samplerate);
  voice->dsp_instance->init(samplerate);
  voice->dsp_instance->buildUserInterface(&voice->myUI);
  return data;
}

static void *create_instrument_plugin_data(const SoundPluginType *plugin_type, struct SoundPlugin *plugin, float samplerate, int blocksize){
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

static int get_effect_format(const struct SoundPluginType *plugin_type, int effect_num){
  Data *data = (Data*)plugin_type->data;
  Voice *voice = &data->voices[0];
  MyUI::Controller *controller = &voice->myUI._controllers.at(effect_num);
  return controller->type;
}

static const char *get_effect_name(const struct SoundPluginType *plugin_type, int effect_num){
  Data *data = (Data*)plugin_type->data;
  Voice *voice = &data->voices[0];
  MyUI::Controller *controller = &voice->myUI._controllers.at(effect_num);
  return controller->name.c_str();
}

static void set_effect_value(struct SoundPlugin *plugin, int64_t time, int effect_num, float value, enum ValueFormat value_format){
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

static int get_effect_num(const struct SoundPluginType *plugin_type, const char *effect_name){
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

  Data *data = (Data*)create_effect_plugin_data(&faust_type, NULL, MIXER_get_sample_rate(), MIXER_get_buffer_size());
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
