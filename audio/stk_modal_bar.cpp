//-----------------------------------------------------
// name: "Modal Bar"
// author: "Romain Michon (rmichon@ccrma.stanford.edu)"
// copyright: "Romain Michon"
// version: "1.0"
//
// Code generated with Faust 0.9.55 (http://faust.grame.fr)
//-----------------------------------------------------
/* link with : "" */
#include <instrument.h>
#include <math.h>
#include <modalBar.h>
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
#define FAUSTCLASS Modal_Bar_dsp
#endif

class Modal_Bar_dsp : public dsp {
  private:
	class SIG0 {
	  private:
		int 	fSamplingFreq;
		int 	iRec0[2];
	  public:
		int getNumInputs() 	{ return 0; }
		int getNumOutputs() 	{ return 1; }
		void init(int samplingFreq) {
			fSamplingFreq = samplingFreq;
			for (int i=0; i<2; i++) iRec0[i] = 0;
		}
		void fill (int count, float output[]) {
			for (int i=0; i<count; i++) {
				iRec0[0] = (1 + iRec0[1]);
				output[i] = sinf((9.587379924285257e-05f * float((iRec0[0] - 1))));
				// post processing
				iRec0[1] = iRec0[0];
			}
		}
	};


	class SIG1 {
	  private:
		int 	fSamplingFreq;
		int 	iRec8[2];
	  public:
		int getNumInputs() 	{ return 0; }
		int getNumOutputs() 	{ return 1; }
		void init(int samplingFreq) {
			fSamplingFreq = samplingFreq;
			for (int i=0; i<2; i++) iRec8[i] = 0;
		}
		void fill (int count, float output[]) {
			for (int i=0; i<count; i++) {
				iRec8[0] = (1 + iRec8[1]);
				output[i] = readMarmstk1(int(((iRec8[0] - 1) % 246)));
				// post processing
				iRec8[1] = iRec8[0];
			}
		}
	};


	static float 	ftbl0[65536];
	FAUSTFLOAT 	fslider0;
	float 	fRec2[2];
	FAUSTFLOAT 	fentry0;
	FAUSTFLOAT 	fentry1;
	int 	iConst0;
	float 	fConst1;
	float 	fRec1[2];
	FAUSTFLOAT 	fslider1;
	float 	fRec3[2];
	FAUSTFLOAT 	fentry2;
	float 	fRec5[2];
	FAUSTFLOAT 	fslider2;
	FAUSTFLOAT 	fbutton0;
	float 	fRec7[2];
	static float 	ftbl1[246];
	float 	fRec9[2];
	float 	fRec6[2];
	FAUSTFLOAT 	fentry3;
	float 	fRec10[2];
	FAUSTFLOAT 	fentry4;
	float 	fConst2;
	float 	fRec4[3];
	float 	fRec12[2];
	float 	fRec13[2];
	float 	fRec11[3];
	float 	fRec15[2];
	float 	fRec16[2];
	float 	fRec14[3];
	float 	fRec18[2];
	float 	fRec19[2];
	float 	fRec17[3];
	FAUSTFLOAT 	fslider3;
	float 	fRec20[2];
	FAUSTFLOAT 	fslider4;
	float 	fVec0[2];
	float 	fRec26[2];
	float 	fRec25[2];
	float 	fRec24[2];
	float 	fRec23[2];
	float 	fRec22[2];
	float 	fRec21[2];
	float 	fRec32[2];
	float 	fRec31[2];
	float 	fRec30[2];
	float 	fRec29[2];
	float 	fRec28[2];
	float 	fRec27[2];
	int 	iRec33[2];
	FAUSTFLOAT 	fslider5;
	FAUSTFLOAT 	fslider6;
	float 	fRec34[2];
	int 	IOTA;
	float 	fVec1[4096];
	FAUSTFLOAT 	fslider7;
	FAUSTFLOAT 	fslider8;
	float 	fConst3;
  public:
	static void metadata(Meta* m) 	{ 
		m->declare("name", "Modal Bar");
		m->declare("description", "Nonlinear Modal percussive instruments");
		m->declare("author", "Romain Michon (rmichon@ccrma.stanford.edu)");
		m->declare("copyright", "Romain Michon");
		m->declare("version", "1.0");
		m->declare("licence", "STK-4.3");
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
		m->declare("instrument.lib/name", "Faust-STK Tools Library");
		m->declare("instrument.lib/author", "Romain Michon (rmichon@ccrma.stanford.edu)");
		m->declare("instrument.lib/copyright", "Romain Michon");
		m->declare("instrument.lib/version", "1.0");
		m->declare("instrument.lib/licence", "STK-4.3");
		m->declare("filter.lib/name", "Faust Filter Library");
		m->declare("filter.lib/author", "Julius O. Smith (jos at ccrma.stanford.edu)");
		m->declare("filter.lib/copyright", "Julius O. Smith III");
		m->declare("filter.lib/version", "1.29");
		m->declare("filter.lib/license", "STK-4.3");
		m->declare("filter.lib/reference", "https://ccrma.stanford.edu/~jos/filters/");
		m->declare("effect.lib/name", "Faust Audio Effect Library");
		m->declare("effect.lib/author", "Julius O. Smith (jos at ccrma.stanford.edu)");
		m->declare("effect.lib/copyright", "Julius O. Smith III");
		m->declare("effect.lib/version", "1.33");
		m->declare("effect.lib/license", "STK-4.3");
	}

	virtual int getNumInputs() 	{ return 0; }
	virtual int getNumOutputs() 	{ return 2; }
	static void classInit(int samplingFreq) {
		SIG0 sig0;
		sig0.init(samplingFreq);
		sig0.fill(65536,ftbl0);
		SIG1 sig1;
		sig1.init(samplingFreq);
		sig1.fill(246,ftbl1);
	}
	virtual void instanceInit(int samplingFreq) {
		fSamplingFreq = samplingFreq;
		fslider0 = 2.2e+02f;
		for (int i=0; i<2; i++) fRec2[i] = 0;
		fentry0 = 0.0f;
		fentry1 = 4.4e+02f;
		iConst0 = min(192000, max(1, fSamplingFreq));
		fConst1 = (1.0f / float(iConst0));
		for (int i=0; i<2; i++) fRec1[i] = 0;
		fslider1 = 0.0f;
		for (int i=0; i<2; i++) fRec3[i] = 0;
		fentry2 = 1.0f;
		for (int i=0; i<2; i++) fRec5[i] = 0;
		fslider2 = 0.25f;
		fbutton0 = 0.0;
		for (int i=0; i<2; i++) fRec7[i] = 0;
		for (int i=0; i<2; i++) fRec9[i] = 0;
		for (int i=0; i<2; i++) fRec6[i] = 0;
		fentry3 = 0.8f;
		for (int i=0; i<2; i++) fRec10[i] = 0;
		fentry4 = 1.0f;
		fConst2 = (6.283185307179586f / float(iConst0));
		for (int i=0; i<3; i++) fRec4[i] = 0;
		for (int i=0; i<2; i++) fRec12[i] = 0;
		for (int i=0; i<2; i++) fRec13[i] = 0;
		for (int i=0; i<3; i++) fRec11[i] = 0;
		for (int i=0; i<2; i++) fRec15[i] = 0;
		for (int i=0; i<2; i++) fRec16[i] = 0;
		for (int i=0; i<3; i++) fRec14[i] = 0;
		for (int i=0; i<2; i++) fRec18[i] = 0;
		for (int i=0; i<2; i++) fRec19[i] = 0;
		for (int i=0; i<3; i++) fRec17[i] = 0;
		fslider3 = 6.0f;
		for (int i=0; i<2; i++) fRec20[i] = 0;
		fslider4 = 0.1f;
		for (int i=0; i<2; i++) fVec0[i] = 0;
		for (int i=0; i<2; i++) fRec26[i] = 0;
		for (int i=0; i<2; i++) fRec25[i] = 0;
		for (int i=0; i<2; i++) fRec24[i] = 0;
		for (int i=0; i<2; i++) fRec23[i] = 0;
		for (int i=0; i<2; i++) fRec22[i] = 0;
		for (int i=0; i<2; i++) fRec21[i] = 0;
		for (int i=0; i<2; i++) fRec32[i] = 0;
		for (int i=0; i<2; i++) fRec31[i] = 0;
		for (int i=0; i<2; i++) fRec30[i] = 0;
		for (int i=0; i<2; i++) fRec29[i] = 0;
		for (int i=0; i<2; i++) fRec28[i] = 0;
		for (int i=0; i<2; i++) fRec27[i] = 0;
		for (int i=0; i<2; i++) iRec33[i] = 0;
		fslider5 = 0.05f;
		fslider6 = 0.0f;
		for (int i=0; i<2; i++) fRec34[i] = 0;
		IOTA = 0;
		for (int i=0; i<4096; i++) fVec1[i] = 0;
		fslider7 = 0.6f;
		fslider8 = 0.5f;
		fConst3 = (0.5f * iConst0);
	}
	virtual void init(int samplingFreq) {
		classInit(samplingFreq);
		instanceInit(samplingFreq);
	}
	virtual void buildUserInterface(UI* interface) {
		interface->openVerticalBox("modalBar");
		interface->openHorizontalBox("Basic_Parameters");
		interface->declare(&fentry1, "1", "");
		interface->declare(&fentry1, "tooltip", "Tone frequency");
		interface->declare(&fentry1, "unit", "Hz");
		interface->addNumEntry("freq", &fentry1, 4.4e+02f, 2e+01f, 2e+04f, 1.0f);
		interface->declare(&fentry3, "1", "");
		interface->declare(&fentry3, "tooltip", "Gain (value between 0 and 1)");
		interface->addNumEntry("gain", &fentry3, 0.8f, 0.0f, 1.0f, 0.01f);
		interface->declare(&fbutton0, "1", "");
		interface->declare(&fbutton0, "tooltip", "noteOn = 1, noteOff = 0");
		interface->addButton("gate", &fbutton0);
		interface->closeBox();
		interface->openHorizontalBox("Envelopes_and_Vibrato");
		interface->openVerticalBox("Global_Envelope_Parameters");
		interface->declare(&fslider6, "5", "");
		interface->declare(&fslider6, "tooltip", "Global envelope attack duration");
		interface->declare(&fslider6, "unit", "s");
		interface->addHorizontalSlider("Glob_Env_Attack", &fslider6, 0.0f, 0.0f, 2.0f, 0.01f);
		interface->declare(&fslider5, "5", "");
		interface->declare(&fslider5, "tooltip", "Global envelope release duration");
		interface->declare(&fslider5, "unit", "s");
		interface->addHorizontalSlider("Glob_Env_Release", &fslider5, 0.05f, 0.0f, 2.0f, 0.01f);
		interface->closeBox();
		interface->openVerticalBox("Vibrato_Parameters");
		interface->declare(&fslider3, "4", "");
		interface->declare(&fslider3, "unit", "Hz");
		interface->addHorizontalSlider("Vibrato_Freq", &fslider3, 6.0f, 1.0f, 15.0f, 0.1f);
		interface->declare(&fslider4, "4", "");
		interface->declare(&fslider4, "tooltip", "A value between 0 and 1");
		interface->addHorizontalSlider("Vibrato_Gain", &fslider4, 0.1f, 0.0f, 1.0f, 0.01f);
		interface->closeBox();
		interface->closeBox();
		interface->openHorizontalBox("Physical_and_Nonlinearity");
		interface->openVerticalBox("Nonlinear_Filter_Parameters");
		interface->declare(&fslider0, "3", "");
		interface->declare(&fslider0, "tooltip", "Frequency of the sine wave for the modulation of theta (works if Modulation Type=3)");
		interface->declare(&fslider0, "unit", "Hz");
		interface->addHorizontalSlider("Modulation_Frequency", &fslider0, 2.2e+02f, 2e+01f, 1e+03f, 0.1f);
		interface->declare(&fentry0, "3", "");
		interface->declare(&fentry0, "tooltip", "0=theta is modulated by the incoming signal; 1=theta is modulated by the averaged incoming signal; 2=theta is modulated by the squared incoming signal; 3=theta is modulated by a sine wave of frequency freqMod; 4=theta is modulated by a sine wave of frequency freq;");
		interface->addNumEntry("Modulation_Type", &fentry0, 0.0f, 0.0f, 4.0f, 1.0f);
		interface->declare(&fslider1, "3", "");
		interface->declare(&fslider1, "tooltip", "Nonlinearity factor (value between 0 and 1)");
		interface->addHorizontalSlider("Nonlinearity", &fslider1, 0.0f, 0.0f, 1.0f, 0.01f);
		interface->closeBox();
		interface->openVerticalBox("Physical_Parameters");
		interface->declare(&fentry2, "2", "");
		interface->declare(&fentry2, "tooltip", "0->Marimba, 1->Vibraphone, 2->Agogo, 3->Wood1, 4->Reso, 5->Wood2, 6->Beats, 7->2Fix; 8->Clump");
		interface->addNumEntry("Preset", &fentry2, 1.0f, 0.0f, 8.0f, 1.0f);
		interface->declare(&fentry4, "2", "");
		interface->declare(&fentry4, "tooltip", "A value between 0 and 1");
		interface->addNumEntry("Resonance", &fentry4, 1.0f, 0.0f, 1.0f, 1.0f);
		interface->declare(&fslider2, "2", "");
		interface->declare(&fslider2, "tooltip", "A value between 0 and 1");
		interface->addHorizontalSlider("Stick_Hardness", &fslider2, 0.25f, 0.0f, 1.0f, 0.01f);
		interface->closeBox();
		interface->closeBox();
		interface->openVerticalBox("Spat");
		interface->addHorizontalSlider("pan angle", &fslider7, 0.6f, 0.0f, 1.0f, 0.01f);
		interface->addHorizontalSlider("spatial width", &fslider8, 0.5f, 0.0f, 1.0f, 0.01f);
		interface->closeBox();
		interface->closeBox();
	}
	virtual void compute (int count, FAUSTFLOAT** input, FAUSTFLOAT** output) {
		float 	fSlow0 = (0.0010000000000000009f * fslider0);
		float 	fSlow1 = fentry0;
		int 	iSlow2 = (fSlow1 != 4);
		float 	fSlow3 = fentry1;
		float 	fSlow4 = (fSlow3 * (fSlow1 == 4));
		float 	fSlow5 = (0.0010000000000000009f * fslider1);
		float 	fSlow6 = fentry2;
		float 	fSlow7 = (0.0010000000000000009f * loadPreset(fSlow6, 2, 1));
		float 	fSlow8 = powf(4,fslider2);
		float 	fSlow9 = (61.5f * fSlow8);
		float 	fSlow10 = fbutton0;
		float 	fSlow11 = (0.25f * fSlow8);
		float 	fSlow12 = (246.0f * fSlow10);
		float 	fSlow13 = (0.09999999999999998f * fSlow10);
		float 	fSlow14 = fentry3;
		float 	fSlow15 = (1 - (0.03f * (fSlow14 * ((fSlow10 < 1) & (fentry4 != 1)))));
		float 	fSlow16 = faustpower<2>(fSlow15);
		float 	fSlow17 = (2 * fSlow15);
		float 	fSlow18 = (0.0010000000000000009f * loadPreset(fSlow6, 2, 0));
		float 	fSlow19 = (0.0010000000000000009f * loadPreset(fSlow6, 2, 2));
		float 	fSlow20 = (0.0010000000000000009f * loadPreset(fSlow6, 2, 3));
		float 	fSlow21 = loadPreset(fSlow6, 3, 2);
		float 	fSlow22 = (fConst1 * fslider3);
		float 	fSlow23 = ((fSlow6 == 1) * fslider4);
		int 	iSlow24 = (fSlow1 >= 3);
		float 	fSlow25 = (3.141592653589793f * (fSlow1 == 2));
		float 	fSlow26 = (3.141592653589793f * (fSlow1 == 0));
		float 	fSlow27 = (1.5707963267948966f * (fSlow1 == 1));
		int 	iSlow28 = (fSlow1 < 3);
		int 	iSlow29 = (fSlow10 > 0);
		int 	iSlow30 = (fSlow10 <= 0);
		float 	fSlow31 = fslider5;
		float 	fSlow32 = (1 - (1.0f / powf(1e+05f,(1.0f / ((fSlow31 == 0.0f) + (iConst0 * fSlow31))))));
		float 	fSlow33 = fslider6;
		float 	fSlow34 = (1.0f / ((fSlow33 == 0.0f) + (iConst0 * fSlow33)));
		float 	fSlow35 = fslider7;
		float 	fSlow36 = (0.3f * (1.0f - fSlow35));
		int 	iSlow37 = int((int((fConst3 * (fslider8 / fSlow3))) & 4095));
		float 	fSlow38 = (0.3f * fSlow35);
		FAUSTFLOAT* output0 = output[0];
		FAUSTFLOAT* output1 = output[1];
		for (int i=0; i<count; i++) {
			fRec2[0] = (fSlow0 + (0.999f * fRec2[1]));
			float fTemp0 = ((fConst1 * (fSlow4 + (iSlow2 * fRec2[0]))) + fRec1[1]);
			fRec1[0] = (fTemp0 - floorf(fTemp0));
			fRec3[0] = (fSlow5 + (0.999f * fRec3[1]));
			float fTemp1 = (3.141592653589793f * (fRec3[0] * ftbl0[int((65536.0f * fRec1[0]))]));
			float fTemp2 = sinf(fTemp1);
			fRec5[0] = (fSlow7 + (0.999f * fRec5[1]));
			fRec7[0] = (1 + (fSlow10 * fRec7[1]));
			float fTemp3 = (fSlow11 + fRec9[1]);
			fRec9[0] = (fTemp3 - floorf(fTemp3));
			fRec6[0] = ((0.9f * fRec6[1]) + (fSlow13 * (ftbl1[int((fSlow12 * fRec9[0]))] * ((fRec7[0] - 1) < fSlow9))));
			fRec10[0] = (0.0010000000000000009f + (0.999f * fRec10[1]));
			float fTemp4 = loadPreset(fSlow6, 1, fRec10[0]);
			float fTemp5 = loadPreset(fSlow6, 0, fRec10[0]);
			int iTemp6 = (fTemp5 < 0);
			fRec4[0] = (0 - ((((fRec4[1] * cosf((fConst2 * ((iTemp6 * (0 - fTemp5)) + (fSlow3 * (fTemp5 * (0 - (iTemp6 - 1)))))))) * (0 - (fSlow17 * fTemp4))) + (fSlow16 * (faustpower<2>(fTemp4) * fRec4[2]))) - (fSlow14 * (fRec6[0] * fRec5[0]))));
			fRec12[0] = (fSlow18 + (0.999f * fRec12[1]));
			fRec13[0] = (0.999f * fRec13[1]);
			float fTemp7 = loadPreset(fSlow6, 1, fRec13[0]);
			float fTemp8 = loadPreset(fSlow6, 0, fRec13[0]);
			int iTemp9 = (fTemp8 < 0);
			fRec11[0] = (0 - (((fSlow16 * (faustpower<2>(fTemp7) * fRec11[2])) + ((fRec11[1] * cosf((fConst2 * ((iTemp9 * (0 - fTemp8)) + (fSlow3 * (fTemp8 * (0 - (iTemp9 - 1)))))))) * (0 - (fSlow17 * fTemp7)))) - (fSlow14 * (fRec6[0] * fRec12[0]))));
			fRec15[0] = (fSlow19 + (0.999f * fRec15[1]));
			fRec16[0] = (0.0020000000000000018f + (0.999f * fRec16[1]));
			float fTemp10 = loadPreset(fSlow6, 1, fRec16[0]);
			float fTemp11 = loadPreset(fSlow6, 0, fRec16[0]);
			int iTemp12 = (fTemp11 < 0);
			fRec14[0] = (0 - (((fSlow16 * (faustpower<2>(fTemp10) * fRec14[2])) + ((fRec14[1] * cosf((fConst2 * ((iTemp12 * (0 - fTemp11)) + (fSlow3 * (fTemp11 * (0 - (iTemp12 - 1)))))))) * (0 - (fSlow17 * fTemp10)))) - (fSlow14 * (fRec6[0] * fRec15[0]))));
			fRec18[0] = (fSlow20 + (0.999f * fRec18[1]));
			fRec19[0] = (0.0030000000000000027f + (0.999f * fRec19[1]));
			float fTemp13 = loadPreset(fSlow6, 1, fRec19[0]);
			float fTemp14 = loadPreset(fSlow6, 0, fRec19[0]);
			int iTemp15 = (fTemp14 < 0);
			fRec17[0] = (0 - (((fSlow16 * (faustpower<2>(fTemp13) * fRec17[2])) + ((fRec17[1] * cosf((fConst2 * ((iTemp15 * (0 - fTemp14)) + (fSlow3 * (fTemp14 * (0 - (iTemp15 - 1)))))))) * (0 - (fSlow17 * fTemp13)))) - (fSlow14 * (fRec6[0] * fRec18[0]))));
			float fTemp16 = (fRec17[0] + (fRec14[0] + (fRec11[0] + fRec4[0])));
			float fTemp17 = (fTemp16 + (fSlow21 * ((fSlow14 * fRec6[0]) - fTemp16)));
			float fTemp18 = (fSlow22 + fRec20[1]);
			fRec20[0] = (fTemp18 - floorf(fTemp18));
			float fTemp19 = (1 + (fSlow23 * ftbl0[int((65536.0f * fRec20[0]))]));
			float fTemp20 = (fTemp19 * fTemp17);
			fVec0[0] = fTemp20;
			float fTemp21 = (0 - fTemp2);
			float fTemp22 = cosf(fTemp1);
			float fTemp23 = ((fVec0[0] * fTemp22) + (fTemp21 * fRec21[1]));
			float fTemp24 = ((fTemp22 * fTemp23) + (fTemp21 * fRec22[1]));
			float fTemp25 = ((fTemp22 * fTemp24) + (fTemp21 * fRec23[1]));
			float fTemp26 = ((fTemp22 * fTemp25) + (fTemp21 * fRec24[1]));
			float fTemp27 = ((fTemp22 * fTemp26) + (fTemp21 * fRec25[1]));
			fRec26[0] = ((fTemp22 * fTemp27) + (fTemp21 * fRec26[1]));
			fRec25[0] = ((fTemp22 * fRec26[1]) + (fTemp2 * fTemp27));
			fRec24[0] = ((fTemp22 * fRec25[1]) + (fTemp2 * fTemp26));
			fRec23[0] = ((fTemp22 * fRec24[1]) + (fTemp2 * fTemp25));
			fRec22[0] = ((fTemp22 * fRec23[1]) + (fTemp2 * fTemp24));
			fRec21[0] = ((fTemp22 * fRec22[1]) + (fTemp2 * fTemp23));
			float fTemp28 = (fRec3[0] * (((fSlow27 * (fVec0[0] + fVec0[1])) + (fSlow26 * fVec0[0])) + (fSlow25 * (faustpower<2>(fTemp19) * faustpower<2>(fTemp17)))));
			float fTemp29 = sinf(fTemp28);
			float fTemp30 = (0 - fTemp29);
			float fTemp31 = cosf(fTemp28);
			float fTemp32 = ((fVec0[0] * fTemp31) + (fTemp30 * fRec27[1]));
			float fTemp33 = ((fTemp31 * fTemp32) + (fTemp30 * fRec28[1]));
			float fTemp34 = ((fTemp31 * fTemp33) + (fTemp30 * fRec29[1]));
			float fTemp35 = ((fTemp31 * fTemp34) + (fTemp30 * fRec30[1]));
			float fTemp36 = ((fTemp31 * fTemp35) + (fTemp30 * fRec31[1]));
			fRec32[0] = ((fTemp31 * fTemp36) + (fTemp30 * fRec32[1]));
			fRec31[0] = ((fTemp31 * fRec32[1]) + (fTemp29 * fTemp36));
			fRec30[0] = ((fTemp31 * fRec31[1]) + (fTemp29 * fTemp35));
			fRec29[0] = ((fTemp31 * fRec30[1]) + (fTemp29 * fTemp34));
			fRec28[0] = ((fTemp31 * fRec29[1]) + (fTemp29 * fTemp33));
			fRec27[0] = ((fTemp31 * fRec28[1]) + (fTemp29 * fTemp32));
			iRec33[0] = (iSlow29 & (iRec33[1] | (fRec34[1] >= 1)));
			int iTemp37 = (iSlow30 & (fRec34[1] > 0));
			fRec34[0] = (((iTemp37 == 0) | (fRec34[1] >= 1e-06f)) * ((fSlow34 * (((iRec33[1] == 0) & iSlow29) & (fRec34[1] < 1))) + (fRec34[1] * (1 - (fSlow32 * iTemp37)))));
			float fTemp38 = (fRec34[0] * ((iSlow28 * ((((1 - fRec3[0]) * fTemp19) * fTemp17) + (fRec3[0] * ((fTemp31 * fRec27[1]) + (fVec0[0] * fTemp29))))) + (iSlow24 * ((fTemp22 * fRec21[1]) + (fVec0[0] * fTemp2)))));
			fVec1[IOTA&4095] = fTemp38;
			output0[i] = (FAUSTFLOAT)(fSlow36 * fVec1[IOTA&4095]);
			output1[i] = (FAUSTFLOAT)(fSlow38 * fVec1[(IOTA-iSlow37)&4095]);
			// post processing
			IOTA = IOTA+1;
			fRec34[1] = fRec34[0];
			iRec33[1] = iRec33[0];
			fRec27[1] = fRec27[0];
			fRec28[1] = fRec28[0];
			fRec29[1] = fRec29[0];
			fRec30[1] = fRec30[0];
			fRec31[1] = fRec31[0];
			fRec32[1] = fRec32[0];
			fRec21[1] = fRec21[0];
			fRec22[1] = fRec22[0];
			fRec23[1] = fRec23[0];
			fRec24[1] = fRec24[0];
			fRec25[1] = fRec25[0];
			fRec26[1] = fRec26[0];
			fVec0[1] = fVec0[0];
			fRec20[1] = fRec20[0];
			fRec17[2] = fRec17[1]; fRec17[1] = fRec17[0];
			fRec19[1] = fRec19[0];
			fRec18[1] = fRec18[0];
			fRec14[2] = fRec14[1]; fRec14[1] = fRec14[0];
			fRec16[1] = fRec16[0];
			fRec15[1] = fRec15[0];
			fRec11[2] = fRec11[1]; fRec11[1] = fRec11[0];
			fRec13[1] = fRec13[0];
			fRec12[1] = fRec12[0];
			fRec4[2] = fRec4[1]; fRec4[1] = fRec4[0];
			fRec10[1] = fRec10[0];
			fRec6[1] = fRec6[0];
			fRec9[1] = fRec9[0];
			fRec7[1] = fRec7[0];
			fRec5[1] = fRec5[0];
			fRec3[1] = fRec3[0];
			fRec1[1] = fRec1[0];
			fRec2[1] = fRec2[0];
		}
	}
};


float 	Modal_Bar_dsp::ftbl0[65536];
float 	Modal_Bar_dsp::ftbl1[246];



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
  for(int i=0;i<num_outputs;i++){
    //memset(tempdata[i],0,sizeof(float)*num_frames);
    tempsounds[i] = &tempdata[i][0];
  }

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
