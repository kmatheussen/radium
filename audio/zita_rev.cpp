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
#define FAUSTCLASS Zita_dsp
#endif

class Zita_dsp : public dsp {
  private:
	FAUSTFLOAT 	fslider0;
	int 	iConst0;
	float 	fConst1;
	float 	fConst2;
	FAUSTFLOAT 	fslider1;
	float 	fConst3;
	FAUSTFLOAT 	fslider2;
	float 	fConst4;
	float 	fRec11[2];
	FAUSTFLOAT 	fslider3;
	float 	fRec10[2];
	int 	IOTA;
	float 	fVec0[8192];
	float 	fConst5;
	int 	iConst6;
	float 	fVec1[8192];
	FAUSTFLOAT 	fslider4;
	float 	fConst7;
	float 	fVec2[2048];
	int 	iConst8;
	float 	fRec8[2];
	float 	fConst9;
	float 	fConst10;
	float 	fRec15[2];
	float 	fRec14[2];
	float 	fVec3[8192];
	float 	fConst11;
	int 	iConst12;
	float 	fVec4[1024];
	int 	iConst13;
	float 	fRec12[2];
	float 	fConst14;
	float 	fConst15;
	float 	fRec19[2];
	float 	fRec18[2];
	float 	fVec5[8192];
	float 	fConst16;
	int 	iConst17;
	float 	fVec6[2048];
	int 	iConst18;
	float 	fRec16[2];
	float 	fConst19;
	float 	fConst20;
	float 	fRec23[2];
	float 	fRec22[2];
	float 	fVec7[8192];
	float 	fConst21;
	int 	iConst22;
	float 	fVec8[1024];
	int 	iConst23;
	float 	fRec20[2];
	float 	fConst24;
	float 	fConst25;
	float 	fRec27[2];
	float 	fRec26[2];
	float 	fVec9[16384];
	float 	fConst26;
	int 	iConst27;
	float 	fVec10[8192];
	float 	fVec11[2048];
	int 	iConst28;
	float 	fRec24[2];
	float 	fConst29;
	float 	fConst30;
	float 	fRec31[2];
	float 	fRec30[2];
	float 	fVec12[8192];
	float 	fConst31;
	int 	iConst32;
	float 	fVec13[2048];
	int 	iConst33;
	float 	fRec28[2];
	float 	fConst34;
	float 	fConst35;
	float 	fRec35[2];
	float 	fRec34[2];
	float 	fVec14[16384];
	float 	fConst36;
	int 	iConst37;
	float 	fVec15[2048];
	int 	iConst38;
	float 	fRec32[2];
	float 	fConst39;
	float 	fConst40;
	float 	fRec39[2];
	float 	fRec38[2];
	float 	fVec16[16384];
	float 	fConst41;
	int 	iConst42;
	float 	fVec17[1024];
	int 	iConst43;
	float 	fRec36[2];
	float 	fRec0[3];
	float 	fRec1[3];
	float 	fRec2[3];
	float 	fRec3[3];
	float 	fRec4[3];
	float 	fRec5[3];
	float 	fRec6[3];
	float 	fRec7[3];
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
	}

	virtual int getNumInputs() 	{ return 2; }
	virtual int getNumOutputs() 	{ return 2; }
	static void classInit(int samplingFreq) {
	}
	virtual void instanceInit(int samplingFreq) {
		fSamplingFreq = samplingFreq;
		fslider0 = 2.0f;
		iConst0 = min(192000, max(1, fSamplingFreq));
		fConst1 = floorf((0.5f + (0.174713f * iConst0)));
		fConst2 = ((0 - (6.907755278982138f * fConst1)) / float(iConst0));
		fslider1 = 6e+03f;
		fConst3 = (6.283185307179586f / float(iConst0));
		fslider2 = 2e+02f;
		fConst4 = (3.141592653589793f / float(iConst0));
		for (int i=0; i<2; i++) fRec11[i] = 0;
		fslider3 = 3.0f;
		for (int i=0; i<2; i++) fRec10[i] = 0;
		IOTA = 0;
		for (int i=0; i<8192; i++) fVec0[i] = 0;
		fConst5 = floorf((0.5f + (0.022904f * iConst0)));
		iConst6 = int((int((fConst1 - fConst5)) & 8191));
		for (int i=0; i<8192; i++) fVec1[i] = 0;
		fslider4 = 0.0f;
		fConst7 = (0.001f * iConst0);
		for (int i=0; i<2048; i++) fVec2[i] = 0;
		iConst8 = int((int((fConst5 - 1)) & 2047));
		for (int i=0; i<2; i++) fRec8[i] = 0;
		fConst9 = floorf((0.5f + (0.153129f * iConst0)));
		fConst10 = ((0 - (6.907755278982138f * fConst9)) / float(iConst0));
		for (int i=0; i<2; i++) fRec15[i] = 0;
		for (int i=0; i<2; i++) fRec14[i] = 0;
		for (int i=0; i<8192; i++) fVec3[i] = 0;
		fConst11 = floorf((0.5f + (0.020346f * iConst0)));
		iConst12 = int((int((fConst9 - fConst11)) & 8191));
		for (int i=0; i<1024; i++) fVec4[i] = 0;
		iConst13 = int((int((fConst11 - 1)) & 1023));
		for (int i=0; i<2; i++) fRec12[i] = 0;
		fConst14 = floorf((0.5f + (0.127837f * iConst0)));
		fConst15 = ((0 - (6.907755278982138f * fConst14)) / float(iConst0));
		for (int i=0; i<2; i++) fRec19[i] = 0;
		for (int i=0; i<2; i++) fRec18[i] = 0;
		for (int i=0; i<8192; i++) fVec5[i] = 0;
		fConst16 = floorf((0.5f + (0.031604f * iConst0)));
		iConst17 = int((int((fConst14 - fConst16)) & 8191));
		for (int i=0; i<2048; i++) fVec6[i] = 0;
		iConst18 = int((int((fConst16 - 1)) & 2047));
		for (int i=0; i<2; i++) fRec16[i] = 0;
		fConst19 = floorf((0.5f + (0.125f * iConst0)));
		fConst20 = ((0 - (6.907755278982138f * fConst19)) / float(iConst0));
		for (int i=0; i<2; i++) fRec23[i] = 0;
		for (int i=0; i<2; i++) fRec22[i] = 0;
		for (int i=0; i<8192; i++) fVec7[i] = 0;
		fConst21 = floorf((0.5f + (0.013458f * iConst0)));
		iConst22 = int((int((fConst19 - fConst21)) & 8191));
		for (int i=0; i<1024; i++) fVec8[i] = 0;
		iConst23 = int((int((fConst21 - 1)) & 1023));
		for (int i=0; i<2; i++) fRec20[i] = 0;
		fConst24 = floorf((0.5f + (0.210389f * iConst0)));
		fConst25 = ((0 - (6.907755278982138f * fConst24)) / float(iConst0));
		for (int i=0; i<2; i++) fRec27[i] = 0;
		for (int i=0; i<2; i++) fRec26[i] = 0;
		for (int i=0; i<16384; i++) fVec9[i] = 0;
		fConst26 = floorf((0.5f + (0.024421f * iConst0)));
		iConst27 = int((int((fConst24 - fConst26)) & 16383));
		for (int i=0; i<8192; i++) fVec10[i] = 0;
		for (int i=0; i<2048; i++) fVec11[i] = 0;
		iConst28 = int((int((fConst26 - 1)) & 2047));
		for (int i=0; i<2; i++) fRec24[i] = 0;
		fConst29 = floorf((0.5f + (0.192303f * iConst0)));
		fConst30 = ((0 - (6.907755278982138f * fConst29)) / float(iConst0));
		for (int i=0; i<2; i++) fRec31[i] = 0;
		for (int i=0; i<2; i++) fRec30[i] = 0;
		for (int i=0; i<8192; i++) fVec12[i] = 0;
		fConst31 = floorf((0.5f + (0.029291f * iConst0)));
		iConst32 = int((int((fConst29 - fConst31)) & 8191));
		for (int i=0; i<2048; i++) fVec13[i] = 0;
		iConst33 = int((int((fConst31 - 1)) & 2047));
		for (int i=0; i<2; i++) fRec28[i] = 0;
		fConst34 = floorf((0.5f + (0.256891f * iConst0)));
		fConst35 = ((0 - (6.907755278982138f * fConst34)) / float(iConst0));
		for (int i=0; i<2; i++) fRec35[i] = 0;
		for (int i=0; i<2; i++) fRec34[i] = 0;
		for (int i=0; i<16384; i++) fVec14[i] = 0;
		fConst36 = floorf((0.5f + (0.027333f * iConst0)));
		iConst37 = int((int((fConst34 - fConst36)) & 16383));
		for (int i=0; i<2048; i++) fVec15[i] = 0;
		iConst38 = int((int((fConst36 - 1)) & 2047));
		for (int i=0; i<2; i++) fRec32[i] = 0;
		fConst39 = floorf((0.5f + (0.219991f * iConst0)));
		fConst40 = ((0 - (6.907755278982138f * fConst39)) / float(iConst0));
		for (int i=0; i<2; i++) fRec39[i] = 0;
		for (int i=0; i<2; i++) fRec38[i] = 0;
		for (int i=0; i<16384; i++) fVec16[i] = 0;
		fConst41 = floorf((0.5f + (0.019123f * iConst0)));
		iConst42 = int((int((fConst39 - fConst41)) & 16383));
		for (int i=0; i<1024; i++) fVec17[i] = 0;
		iConst43 = int((int((fConst41 - 1)) & 1023));
		for (int i=0; i<2; i++) fRec36[i] = 0;
		for (int i=0; i<3; i++) fRec0[i] = 0;
		for (int i=0; i<3; i++) fRec1[i] = 0;
		for (int i=0; i<3; i++) fRec2[i] = 0;
		for (int i=0; i<3; i++) fRec3[i] = 0;
		for (int i=0; i<3; i++) fRec4[i] = 0;
		for (int i=0; i<3; i++) fRec5[i] = 0;
		for (int i=0; i<3; i++) fRec6[i] = 0;
		for (int i=0; i<3; i++) fRec7[i] = 0;
	}
	virtual void init(int samplingFreq) {
		classInit(samplingFreq);
		instanceInit(samplingFreq);
	}
	virtual void buildUserInterface(UI* interface) {
		interface->declare(0, "0", "");
		interface->declare(0, "tooltip", "~ ZITA REV1 FEEDBACK DELAY NETWORK (FDN) & SCHROEDER ALLPASS-COMB REVERBERATOR (8x8). See Faust's effect.lib for documentation and references");
		interface->openHorizontalBox("Zita_Rev1");
		interface->declare(0, "1", "");
		interface->openHorizontalBox("Input");
		interface->declare(&fslider4, "1", "");
		interface->declare(&fslider4, "style", "knob");
		interface->declare(&fslider4, "tooltip", "Delay in ms before reverberation begins");
		interface->declare(&fslider4, "unit", "ms");
		interface->addVerticalSlider("In Delay", &fslider4, 0.0f, 0.0f, 1e+02f, 1.0f);
		interface->closeBox();
		interface->declare(0, "2", "");
		interface->openHorizontalBox("Decay Times in Bands (see tooltips)");
		interface->declare(&fslider2, "1", "");
		interface->declare(&fslider2, "style", "knob");
		interface->declare(&fslider2, "tooltip", "Crossover frequency (Hz) separating low and middle frequencies");
		interface->declare(&fslider2, "unit", "Hz");
		interface->addVerticalSlider("LF X", &fslider2, 2e+02f, 5e+01f, 1e+03f, 1.0f);
		interface->declare(&fslider3, "2", "");
		interface->declare(&fslider3, "style", "knob");
		interface->declare(&fslider3, "tooltip", "T60 = time (in seconds) to decay 60dB in low-frequency band");
		interface->declare(&fslider3, "unit", "s");
		interface->addVerticalSlider("Low RT60", &fslider3, 3.0f, 1.0f, 8.0f, 0.1f);
		interface->declare(&fslider0, "3", "");
		interface->declare(&fslider0, "style", "knob");
		interface->declare(&fslider0, "tooltip", "T60 = time (in seconds) to decay 60dB in middle band");
		interface->declare(&fslider0, "unit", "s");
		interface->addVerticalSlider("Mid RT60", &fslider0, 2.0f, 1.0f, 8.0f, 0.1f);
		interface->declare(&fslider1, "4", "");
		interface->declare(&fslider1, "style", "knob");
		interface->declare(&fslider1, "tooltip", "Frequency (Hz) at which the high-frequency T60 is half the middle-band's T60");
		interface->declare(&fslider1, "unit", "Hz");
		interface->addVerticalSlider("HF Damping", &fslider1, 6e+03f, 1.5e+03f, 2.352e+04f, 1.0f);
		interface->closeBox();
		interface->closeBox();
	}
	virtual void compute (int count, FAUSTFLOAT** input, FAUSTFLOAT** output) {
		float 	fSlow0 = fslider0;
		float 	fSlow1 = expf((fConst2 / fSlow0));
		float 	fSlow2 = faustpower<2>(fSlow1);
		float 	fSlow3 = (1.0f - fSlow2);
		float 	fSlow4 = cosf((fConst3 * fslider1));
		float 	fSlow5 = (1.0f - (fSlow4 * fSlow2));
		float 	fSlow6 = sqrtf(max((float)0, ((faustpower<2>(fSlow5) / faustpower<2>(fSlow3)) - 1.0f)));
		float 	fSlow7 = (fSlow5 / fSlow3);
		float 	fSlow8 = (fSlow7 - fSlow6);
		float 	fSlow9 = (1.0f / tanf((fConst4 * fslider2)));
		float 	fSlow10 = (1 + fSlow9);
		float 	fSlow11 = (0 - ((1 - fSlow9) / fSlow10));
		float 	fSlow12 = (1.0f / fSlow10);
		float 	fSlow13 = fslider3;
		float 	fSlow14 = ((expf((fConst2 / fSlow13)) / fSlow1) - 1);
		float 	fSlow15 = (fSlow1 * ((1.0f + fSlow6) - fSlow7));
		int 	iSlow16 = int((int((fConst7 * fslider4)) & 8191));
		float 	fSlow17 = expf((fConst10 / fSlow0));
		float 	fSlow18 = faustpower<2>(fSlow17);
		float 	fSlow19 = (1.0f - fSlow18);
		float 	fSlow20 = (1.0f - (fSlow4 * fSlow18));
		float 	fSlow21 = sqrtf(max((float)0, ((faustpower<2>(fSlow20) / faustpower<2>(fSlow19)) - 1.0f)));
		float 	fSlow22 = (fSlow20 / fSlow19);
		float 	fSlow23 = (fSlow22 - fSlow21);
		float 	fSlow24 = ((expf((fConst10 / fSlow13)) / fSlow17) - 1);
		float 	fSlow25 = (fSlow17 * ((1.0f + fSlow21) - fSlow22));
		float 	fSlow26 = expf((fConst15 / fSlow0));
		float 	fSlow27 = faustpower<2>(fSlow26);
		float 	fSlow28 = (1.0f - fSlow27);
		float 	fSlow29 = (1.0f - (fSlow4 * fSlow27));
		float 	fSlow30 = sqrtf(max((float)0, ((faustpower<2>(fSlow29) / faustpower<2>(fSlow28)) - 1.0f)));
		float 	fSlow31 = (fSlow29 / fSlow28);
		float 	fSlow32 = (fSlow31 - fSlow30);
		float 	fSlow33 = ((expf((fConst15 / fSlow13)) / fSlow26) - 1);
		float 	fSlow34 = (fSlow26 * ((1.0f + fSlow30) - fSlow31));
		float 	fSlow35 = expf((fConst20 / fSlow0));
		float 	fSlow36 = faustpower<2>(fSlow35);
		float 	fSlow37 = (1.0f - fSlow36);
		float 	fSlow38 = (1.0f - (fSlow4 * fSlow36));
		float 	fSlow39 = sqrtf(max((float)0, ((faustpower<2>(fSlow38) / faustpower<2>(fSlow37)) - 1.0f)));
		float 	fSlow40 = (fSlow38 / fSlow37);
		float 	fSlow41 = (fSlow40 - fSlow39);
		float 	fSlow42 = ((expf((fConst20 / fSlow13)) / fSlow35) - 1);
		float 	fSlow43 = (fSlow35 * ((1.0f + fSlow39) - fSlow40));
		float 	fSlow44 = expf((fConst25 / fSlow0));
		float 	fSlow45 = faustpower<2>(fSlow44);
		float 	fSlow46 = (1.0f - fSlow45);
		float 	fSlow47 = (1.0f - (fSlow4 * fSlow45));
		float 	fSlow48 = sqrtf(max((float)0, ((faustpower<2>(fSlow47) / faustpower<2>(fSlow46)) - 1.0f)));
		float 	fSlow49 = (fSlow47 / fSlow46);
		float 	fSlow50 = (fSlow49 - fSlow48);
		float 	fSlow51 = ((expf((fConst25 / fSlow13)) / fSlow44) - 1);
		float 	fSlow52 = (fSlow44 * ((1.0f + fSlow48) - fSlow49));
		float 	fSlow53 = expf((fConst30 / fSlow0));
		float 	fSlow54 = faustpower<2>(fSlow53);
		float 	fSlow55 = (1.0f - fSlow54);
		float 	fSlow56 = (1.0f - (fSlow4 * fSlow54));
		float 	fSlow57 = sqrtf(max((float)0, ((faustpower<2>(fSlow56) / faustpower<2>(fSlow55)) - 1.0f)));
		float 	fSlow58 = (fSlow56 / fSlow55);
		float 	fSlow59 = (fSlow58 - fSlow57);
		float 	fSlow60 = ((expf((fConst30 / fSlow13)) / fSlow53) - 1);
		float 	fSlow61 = (fSlow53 * ((1.0f + fSlow57) - fSlow58));
		float 	fSlow62 = expf((fConst35 / fSlow0));
		float 	fSlow63 = faustpower<2>(fSlow62);
		float 	fSlow64 = (1.0f - fSlow63);
		float 	fSlow65 = (1.0f - (fSlow4 * fSlow63));
		float 	fSlow66 = sqrtf(max((float)0, ((faustpower<2>(fSlow65) / faustpower<2>(fSlow64)) - 1.0f)));
		float 	fSlow67 = (fSlow65 / fSlow64);
		float 	fSlow68 = (fSlow67 - fSlow66);
		float 	fSlow69 = ((expf((fConst35 / fSlow13)) / fSlow62) - 1);
		float 	fSlow70 = (fSlow62 * ((1.0f + fSlow66) - fSlow67));
		float 	fSlow71 = expf((fConst40 / fSlow0));
		float 	fSlow72 = faustpower<2>(fSlow71);
		float 	fSlow73 = (1.0f - fSlow72);
		float 	fSlow74 = (1.0f - (fSlow72 * fSlow4));
		float 	fSlow75 = sqrtf(max((float)0, ((faustpower<2>(fSlow74) / faustpower<2>(fSlow73)) - 1.0f)));
		float 	fSlow76 = (fSlow74 / fSlow73);
		float 	fSlow77 = (fSlow76 - fSlow75);
		float 	fSlow78 = ((expf((fConst40 / fSlow13)) / fSlow71) - 1);
		float 	fSlow79 = (fSlow71 * ((1.0f + fSlow75) - fSlow76));
		FAUSTFLOAT* input0 = input[0];
		FAUSTFLOAT* input1 = input[1];
		FAUSTFLOAT* output0 = output[0];
		FAUSTFLOAT* output1 = output[1];
		for (int i=0; i<count; i++) {
			fRec11[0] = ((fSlow12 * (fRec4[1] + fRec4[2])) + (fSlow11 * fRec11[1]));
			fRec10[0] = ((fSlow15 * (fRec4[1] + (fSlow14 * fRec11[0]))) + (fSlow8 * fRec10[1]));
			fVec0[IOTA&8191] = (1e-20f + (0.35355339059327373f * fRec10[0]));
			fVec1[IOTA&8191] = (float)input0[i];
			float fTemp0 = (0.3f * fVec1[(IOTA-iSlow16)&8191]);
			float fTemp1 = ((fTemp0 + fVec0[(IOTA-iConst6)&8191]) - (0.6f * fRec8[1]));
			fVec2[IOTA&2047] = fTemp1;
			fRec8[0] = fVec2[(IOTA-iConst8)&2047];
			float 	fRec9 = (0.6f * fVec2[IOTA&2047]);
			fRec15[0] = ((fSlow12 * (fRec0[1] + fRec0[2])) + (fSlow11 * fRec15[1]));
			fRec14[0] = ((fSlow25 * (fRec0[1] + (fSlow24 * fRec15[0]))) + (fSlow23 * fRec14[1]));
			fVec3[IOTA&8191] = (1e-20f + (0.35355339059327373f * fRec14[0]));
			float fTemp2 = ((fTemp0 + fVec3[(IOTA-iConst12)&8191]) - (0.6f * fRec12[1]));
			fVec4[IOTA&1023] = fTemp2;
			fRec12[0] = fVec4[(IOTA-iConst13)&1023];
			float 	fRec13 = (0.6f * fVec4[IOTA&1023]);
			float fTemp3 = (fRec13 + fRec9);
			fRec19[0] = ((fSlow12 * (fRec2[1] + fRec2[2])) + (fSlow11 * fRec19[1]));
			fRec18[0] = ((fSlow34 * (fRec2[1] + (fSlow33 * fRec19[0]))) + (fSlow32 * fRec18[1]));
			fVec5[IOTA&8191] = (1e-20f + (0.35355339059327373f * fRec18[0]));
			float fTemp4 = (fVec5[(IOTA-iConst17)&8191] - (fTemp0 + (0.6f * fRec16[1])));
			fVec6[IOTA&2047] = fTemp4;
			fRec16[0] = fVec6[(IOTA-iConst18)&2047];
			float 	fRec17 = (0.6f * fVec6[IOTA&2047]);
			fRec23[0] = ((fSlow12 * (fRec6[1] + fRec6[2])) + (fSlow11 * fRec23[1]));
			fRec22[0] = ((fSlow43 * (fRec6[1] + (fSlow42 * fRec23[0]))) + (fSlow41 * fRec22[1]));
			fVec7[IOTA&8191] = (1e-20f + (0.35355339059327373f * fRec22[0]));
			float fTemp5 = (fVec7[(IOTA-iConst22)&8191] - (fTemp0 + (0.6f * fRec20[1])));
			fVec8[IOTA&1023] = fTemp5;
			fRec20[0] = fVec8[(IOTA-iConst23)&1023];
			float 	fRec21 = (0.6f * fVec8[IOTA&1023]);
			float fTemp6 = (fRec21 + (fRec17 + fTemp3));
			fRec27[0] = ((fSlow12 * (fRec1[2] + fRec1[1])) + (fSlow11 * fRec27[1]));
			fRec26[0] = ((fSlow52 * (fRec1[1] + (fSlow51 * fRec27[0]))) + (fSlow50 * fRec26[1]));
			fVec9[IOTA&16383] = (1e-20f + (0.35355339059327373f * fRec26[0]));
			fVec10[IOTA&8191] = (float)input1[i];
			float fTemp7 = (0.3f * fVec10[(IOTA-iSlow16)&8191]);
			float fTemp8 = ((fTemp7 + fVec9[(IOTA-iConst27)&16383]) + (0.6f * fRec24[1]));
			fVec11[IOTA&2047] = fTemp8;
			fRec24[0] = fVec11[(IOTA-iConst28)&2047];
			float 	fRec25 = (0 - (0.6f * fVec11[IOTA&2047]));
			fRec31[0] = ((fSlow12 * (fRec5[1] + fRec5[2])) + (fSlow11 * fRec31[1]));
			fRec30[0] = ((fSlow61 * (fRec5[1] + (fSlow60 * fRec31[0]))) + (fSlow59 * fRec30[1]));
			fVec12[IOTA&8191] = (1e-20f + (0.35355339059327373f * fRec30[0]));
			float fTemp9 = ((fTemp7 + fVec12[(IOTA-iConst32)&8191]) + (0.6f * fRec28[1]));
			fVec13[IOTA&2047] = fTemp9;
			fRec28[0] = fVec13[(IOTA-iConst33)&2047];
			float 	fRec29 = (0 - (0.6f * fVec13[IOTA&2047]));
			fRec35[0] = ((fSlow12 * (fRec3[1] + fRec3[2])) + (fSlow11 * fRec35[1]));
			fRec34[0] = ((fSlow70 * (fRec3[1] + (fSlow69 * fRec35[0]))) + (fSlow68 * fRec34[1]));
			fVec14[IOTA&16383] = (1e-20f + (0.35355339059327373f * fRec34[0]));
			float fTemp10 = ((fVec14[(IOTA-iConst37)&16383] + (0.6f * fRec32[1])) - fTemp7);
			fVec15[IOTA&2047] = fTemp10;
			fRec32[0] = fVec15[(IOTA-iConst38)&2047];
			float 	fRec33 = (0 - (0.6f * fVec15[IOTA&2047]));
			fRec39[0] = ((fSlow12 * (fRec7[1] + fRec7[2])) + (fSlow11 * fRec39[1]));
			fRec38[0] = ((fSlow79 * (fRec7[1] + (fSlow78 * fRec39[0]))) + (fSlow77 * fRec38[1]));
			fVec16[IOTA&16383] = (1e-20f + (0.35355339059327373f * fRec38[0]));
			float fTemp11 = ((fVec16[(IOTA-iConst42)&16383] + (0.6f * fRec36[1])) - fTemp7);
			fVec17[IOTA&1023] = fTemp11;
			fRec36[0] = fVec17[(IOTA-iConst43)&1023];
			float 	fRec37 = (0 - (0.6f * fVec17[IOTA&1023]));
			fRec0[0] = (fRec12[1] + (fRec8[1] + (fRec16[1] + (fRec20[1] + (fRec24[1] + (fRec28[1] + (fRec32[1] + (fRec36[1] + (fRec37 + (fRec33 + (fRec29 + (fRec25 + fTemp6))))))))))));
			fRec1[0] = (0 - ((fRec24[1] + (fRec28[1] + (fRec32[1] + (fRec36[1] + (fRec37 + (fRec33 + (fRec25 + fRec29))))))) - (fRec12[1] + (fRec8[1] + (fRec16[1] + (fRec20[1] + fTemp6))))));
			float fTemp12 = (fRec17 + fRec21);
			fRec2[0] = (0 - ((fRec16[1] + (fRec20[1] + (fRec32[1] + (fRec36[1] + (fRec37 + (fRec33 + fTemp12)))))) - (fRec12[1] + (fRec8[1] + (fRec24[1] + (fRec28[1] + (fRec29 + (fRec25 + fTemp3))))))));
			fRec3[0] = (0 - ((fRec16[1] + (fRec20[1] + (fRec24[1] + (fRec28[1] + (fRec29 + (fRec25 + fTemp12)))))) - (fRec12[1] + (fRec8[1] + (fRec32[1] + (fRec36[1] + (fRec37 + (fRec33 + fTemp3))))))));
			float fTemp13 = (fRec13 + fRec17);
			float fTemp14 = (fRec9 + fRec21);
			fRec4[0] = (0 - ((fRec8[1] + (fRec20[1] + (fRec28[1] + (fRec36[1] + (fRec37 + (fRec29 + fTemp14)))))) - (fRec12[1] + (fRec16[1] + (fRec24[1] + (fRec32[1] + (fRec33 + (fRec25 + fTemp13))))))));
			fRec5[0] = (0 - ((fRec8[1] + (fRec20[1] + (fRec24[1] + (fRec32[1] + (fRec33 + (fRec25 + fTemp14)))))) - (fRec12[1] + (fRec16[1] + (fRec28[1] + (fRec36[1] + (fRec37 + (fRec29 + fTemp13))))))));
			float fTemp15 = (fRec13 + fRec21);
			float fTemp16 = (fRec9 + fRec17);
			fRec6[0] = (0 - ((fRec8[1] + (fRec16[1] + (fRec28[1] + (fRec32[1] + (fRec33 + (fRec29 + fTemp16)))))) - (fRec12[1] + (fRec20[1] + (fRec24[1] + (fRec36[1] + (fRec37 + (fRec25 + fTemp15))))))));
			fRec7[0] = (0 - ((fRec8[1] + (fRec16[1] + (fRec24[1] + (fRec36[1] + (fRec37 + (fRec25 + fTemp16)))))) - (fRec12[1] + (fRec20[1] + (fRec28[1] + (fRec32[1] + (fRec33 + (fRec29 + fTemp15))))))));
			output0[i] = (FAUSTFLOAT)(0.37f * (fRec1[0] + fRec2[0]));
			output1[i] = (FAUSTFLOAT)(0.37f * (fRec1[0] - fRec2[0]));
			// post processing
			fRec7[2] = fRec7[1]; fRec7[1] = fRec7[0];
			fRec6[2] = fRec6[1]; fRec6[1] = fRec6[0];
			fRec5[2] = fRec5[1]; fRec5[1] = fRec5[0];
			fRec4[2] = fRec4[1]; fRec4[1] = fRec4[0];
			fRec3[2] = fRec3[1]; fRec3[1] = fRec3[0];
			fRec2[2] = fRec2[1]; fRec2[1] = fRec2[0];
			fRec1[2] = fRec1[1]; fRec1[1] = fRec1[0];
			fRec0[2] = fRec0[1]; fRec0[1] = fRec0[0];
			fRec36[1] = fRec36[0];
			fRec38[1] = fRec38[0];
			fRec39[1] = fRec39[0];
			fRec32[1] = fRec32[0];
			fRec34[1] = fRec34[0];
			fRec35[1] = fRec35[0];
			fRec28[1] = fRec28[0];
			fRec30[1] = fRec30[0];
			fRec31[1] = fRec31[0];
			fRec24[1] = fRec24[0];
			fRec26[1] = fRec26[0];
			fRec27[1] = fRec27[0];
			fRec20[1] = fRec20[0];
			fRec22[1] = fRec22[0];
			fRec23[1] = fRec23[0];
			fRec16[1] = fRec16[0];
			fRec18[1] = fRec18[0];
			fRec19[1] = fRec19[0];
			fRec12[1] = fRec12[0];
			fRec14[1] = fRec14[0];
			fRec15[1] = fRec15[0];
			fRec8[1] = fRec8[0];
			IOTA = IOTA+1;
			fRec10[1] = fRec10[0];
			fRec11[1] = fRec11[0];
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

static void play_note(struct SoundPlugin *plugin, int64_t time, int note_num, float volume, float pan){
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

static void set_note_pitch(struct SoundPlugin *plugin, int64_t time, int note_num, float pitch){
  Data *data = (Data*)plugin->data;
  Voice *voice = data->voices_playing;
  //printf("Setting volume %f / %f\n",volume,velocity2gain(volume));
  while(voice!=NULL){
    if(voice->note_num==note_num)
      *(voice->myUI._freq_control) = midi_to_hz(pitch);
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
