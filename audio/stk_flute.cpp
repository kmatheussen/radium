//-----------------------------------------------------
// name: "Flute"
// author: "Romain Michon (rmichon@ccrma.stanford.edu)"
// copyright: "Romain Michon"
// version: "1.0"
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
#define FAUSTCLASS Flute_dsp
#endif

class Flute_dsp : public dsp {
  private:
	class SIG0 {
	  private:
		int 	fSamplingFreq;
		int 	iRec5[2];
	  public:
		int getNumInputs() 	{ return 0; }
		int getNumOutputs() 	{ return 1; }
		void init(int samplingFreq) {
			fSamplingFreq = samplingFreq;
			for (int i=0; i<2; i++) iRec5[i] = 0;
		}
		void fill (int count, float output[]) {
			for (int i=0; i<count; i++) {
				iRec5[0] = (1 + iRec5[1]);
				output[i] = sinf((9.587379924285257e-05f * float((iRec5[0] - 1))));
				// post processing
				iRec5[1] = iRec5[0];
			}
		}
	};


	FAUSTFLOAT 	fentry0;
	float 	fRec0[2];
	FAUSTFLOAT 	fbutton0;
	int 	iRec1[2];
	FAUSTFLOAT 	fslider0;
	int 	iConst0;
	FAUSTFLOAT 	fslider1;
	float 	fRec2[2];
	float 	fConst1;
	float 	fConst2;
	float 	fConst3;
	static float 	ftbl0[65536];
	FAUSTFLOAT 	fslider2;
	float 	fConst4;
	float 	fRec6[2];
	int 	iRec7[2];
	int 	iRec8[2];
	FAUSTFLOAT 	fslider3;
	FAUSTFLOAT 	fslider4;
	FAUSTFLOAT 	fslider5;
	float 	fRec9[2];
	FAUSTFLOAT 	fslider6;
	FAUSTFLOAT 	fslider7;
	float 	fRec11[2];
	FAUSTFLOAT 	fentry1;
	FAUSTFLOAT 	fentry2;
	float 	fRec10[2];
	int 	iRec12[2];
	float 	fConst5;
	FAUSTFLOAT 	fslider8;
	float 	fRec13[2];
	FAUSTFLOAT 	fslider9;
	float 	fRec14[2];
	float 	fVec0[2];
	float 	fRec20[2];
	float 	fRec19[2];
	float 	fRec18[2];
	float 	fRec17[2];
	float 	fRec16[2];
	float 	fRec15[2];
	float 	fRec26[2];
	float 	fRec25[2];
	float 	fRec24[2];
	float 	fRec23[2];
	float 	fRec22[2];
	float 	fRec21[2];
	int 	iRec27[2];
	FAUSTFLOAT 	fslider10;
	FAUSTFLOAT 	fslider11;
	float 	fRec28[2];
	FAUSTFLOAT 	fcheckbox0;
	int 	iRec29[2];
	FAUSTFLOAT 	fslider12;
	FAUSTFLOAT 	fslider13;
	FAUSTFLOAT 	fslider14;
	float 	fRec30[2];
	int 	IOTA;
	float 	fVec1[4096];
	float 	fConst6;
	float 	fVec2[2];
	float 	fConst7;
	float 	fRec4[2];
	float 	fRec3[8192];
	float 	fVec3[4096];
	FAUSTFLOAT 	fslider15;
	FAUSTFLOAT 	fslider16;
  public:
	static void metadata(Meta* m) 	{ 
		m->declare("name", "Flute");
		m->declare("description", "Nonlinear WaveGuide Flute");
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
	}
	virtual void instanceInit(int samplingFreq) {
		fSamplingFreq = samplingFreq;
		fentry0 = 1.0f;
		for (int i=0; i<2; i++) fRec0[i] = 0;
		fbutton0 = 0.0;
		for (int i=0; i<2; i++) iRec1[i] = 0;
		fslider0 = 0.1f;
		iConst0 = min(192000, max(1, fSamplingFreq));
		fslider1 = 0.1f;
		for (int i=0; i<2; i++) fRec2[i] = 0;
		fConst1 = (1.0f / tanf((6283.185307179586f / float(iConst0))));
		fConst2 = (1 + fConst1);
		fConst3 = (0 - ((1 - fConst1) / fConst2));
		fslider2 = 5.0f;
		fConst4 = (1.0f / float(iConst0));
		for (int i=0; i<2; i++) fRec6[i] = 0;
		for (int i=0; i<2; i++) iRec7[i] = 0;
		for (int i=0; i<2; i++) iRec8[i] = 0;
		fslider3 = 0.2f;
		fslider4 = 0.1f;
		fslider5 = 0.5f;
		for (int i=0; i<2; i++) fRec9[i] = 0;
		fslider6 = 0.1f;
		fslider7 = 2.2e+02f;
		for (int i=0; i<2; i++) fRec11[i] = 0;
		fentry1 = 0.0f;
		fentry2 = 4.4e+02f;
		for (int i=0; i<2; i++) fRec10[i] = 0;
		for (int i=0; i<2; i++) iRec12[i] = 0;
		fConst5 = (1 - (1.0f / powf(1e+05f,(1e+01f / float(iConst0)))));
		fslider8 = 0.1f;
		for (int i=0; i<2; i++) fRec13[i] = 0;
		fslider9 = 0.0f;
		for (int i=0; i<2; i++) fRec14[i] = 0;
		for (int i=0; i<2; i++) fVec0[i] = 0;
		for (int i=0; i<2; i++) fRec20[i] = 0;
		for (int i=0; i<2; i++) fRec19[i] = 0;
		for (int i=0; i<2; i++) fRec18[i] = 0;
		for (int i=0; i<2; i++) fRec17[i] = 0;
		for (int i=0; i<2; i++) fRec16[i] = 0;
		for (int i=0; i<2; i++) fRec15[i] = 0;
		for (int i=0; i<2; i++) fRec26[i] = 0;
		for (int i=0; i<2; i++) fRec25[i] = 0;
		for (int i=0; i<2; i++) fRec24[i] = 0;
		for (int i=0; i<2; i++) fRec23[i] = 0;
		for (int i=0; i<2; i++) fRec22[i] = 0;
		for (int i=0; i<2; i++) fRec21[i] = 0;
		for (int i=0; i<2; i++) iRec27[i] = 0;
		fslider10 = 0.1f;
		fslider11 = 0.9f;
		for (int i=0; i<2; i++) fRec28[i] = 0;
		fcheckbox0 = 0.0;
		for (int i=0; i<2; i++) iRec29[i] = 0;
		fslider12 = 1.0f;
		fslider13 = 0.2f;
		fslider14 = 0.05f;
		for (int i=0; i<2; i++) fRec30[i] = 0;
		IOTA = 0;
		for (int i=0; i<4096; i++) fVec1[i] = 0;
		fConst6 = (0.5f * iConst0);
		for (int i=0; i<2; i++) fVec2[i] = 0;
		fConst7 = (1.0f / fConst2);
		for (int i=0; i<2; i++) fRec4[i] = 0;
		for (int i=0; i<8192; i++) fRec3[i] = 0;
		for (int i=0; i<4096; i++) fVec3[i] = 0;
		fslider15 = 0.6f;
		fslider16 = 0.5f;
	}
	virtual void init(int samplingFreq) {
		classInit(samplingFreq);
		instanceInit(samplingFreq);
	}
	virtual void buildUserInterface(UI* interface) {
		interface->openVerticalBox("flute");
		interface->openHorizontalBox("Basic_Parameters");
		interface->declare(&fentry2, "1", "");
		interface->declare(&fentry2, "tooltip", "Tone frequency");
		interface->declare(&fentry2, "unit", "Hz");
		interface->addNumEntry("freq", &fentry2, 4.4e+02f, 2e+01f, 2e+04f, 1.0f);
		interface->declare(&fentry0, "1", "");
		interface->declare(&fentry0, "tooltip", "Gain (value between 0 and 1)");
		interface->addNumEntry("gain", &fentry0, 1.0f, 0.0f, 1.0f, 0.01f);
		interface->declare(&fbutton0, "1", "");
		interface->declare(&fbutton0, "tooltip", "noteOn = 1, noteOff = 0");
		interface->addButton("gate", &fbutton0);
		interface->closeBox();
		interface->openHorizontalBox("Envelopes_and_Vibrato");
		interface->openVerticalBox("Global_Envelope_Parameters");
		interface->declare(&fslider1, "6", "");
		interface->declare(&fslider1, "tooltip", "Global envelope attack duration");
		interface->declare(&fslider1, "unit", "s");
		interface->addHorizontalSlider("Glob_Env_Attack", &fslider1, 0.1f, 0.0f, 2.0f, 0.01f);
		interface->declare(&fslider0, "6", "");
		interface->declare(&fslider0, "tooltip", "Global envelope release duration");
		interface->declare(&fslider0, "unit", "s");
		interface->addHorizontalSlider("Glob_Env_Release", &fslider0, 0.1f, 0.0f, 2.0f, 0.01f);
		interface->closeBox();
		interface->openVerticalBox("Pressure_Envelope_Parameters");
		interface->declare(&fslider14, "5", "");
		interface->declare(&fslider14, "tooltip", "Pressure envelope attack duration");
		interface->declare(&fslider14, "unit", "s");
		interface->addHorizontalSlider("Press_Env_Attack", &fslider14, 0.05f, 0.0f, 2.0f, 0.01f);
		interface->declare(&fslider13, "5", "");
		interface->declare(&fslider13, "tooltip", "Pressure envelope decay duration");
		interface->declare(&fslider13, "unit", "s");
		interface->addHorizontalSlider("Press_Env_Decay", &fslider13, 0.2f, 0.0f, 2.0f, 0.01f);
		interface->declare(&fslider12, "5", "");
		interface->declare(&fslider12, "tooltip", "Pressure envelope release duration");
		interface->declare(&fslider12, "unit", "s");
		interface->addHorizontalSlider("Press_Env_Release", &fslider12, 1.0f, 0.0f, 2.0f, 0.01f);
		interface->declare(&fcheckbox0, "5", "");
		interface->declare(&fcheckbox0, "tooltip", "Activate Pressure envelope");
		interface->declare(&fcheckbox0, "unit", "s");
		interface->addCheckButton("Pressure_Env", &fcheckbox0);
		interface->closeBox();
		interface->openVerticalBox("Vibrato_Parameters");
		interface->declare(&fslider5, "4", "");
		interface->declare(&fslider5, "tooltip", "Vibrato attack duration");
		interface->declare(&fslider5, "unit", "s");
		interface->addHorizontalSlider("Vibrato_Attack", &fslider5, 0.5f, 0.0f, 2.0f, 0.01f);
		interface->declare(&fslider4, "4", "");
		interface->declare(&fslider4, "tooltip", "Vibrato silence duration before attack");
		interface->declare(&fslider4, "unit", "s");
		interface->addHorizontalSlider("Vibrato_Begin", &fslider4, 0.1f, 0.0f, 2.0f, 0.01f);
		interface->declare(&fslider2, "4", "");
		interface->declare(&fslider2, "unit", "Hz");
		interface->addHorizontalSlider("Vibrato_Freq", &fslider2, 5.0f, 1.0f, 15.0f, 0.1f);
		interface->declare(&fslider6, "4", "");
		interface->declare(&fslider6, "tooltip", "A value between 0 and 1");
		interface->addHorizontalSlider("Vibrato_Gain", &fslider6, 0.1f, 0.0f, 1.0f, 0.01f);
		interface->declare(&fslider3, "4", "");
		interface->declare(&fslider3, "tooltip", "Vibrato release duration");
		interface->declare(&fslider3, "unit", "s");
		interface->addHorizontalSlider("Vibrato_Release", &fslider3, 0.2f, 0.0f, 2.0f, 0.01f);
		interface->closeBox();
		interface->closeBox();
		interface->openHorizontalBox("Physical_and_Nonlinearity");
		interface->openVerticalBox("Nonlinear_Filter_Parameters");
		interface->declare(&fslider7, "3", "");
		interface->declare(&fslider7, "tooltip", "Frequency of the sine wave for the modulation of theta (works if Modulation Type=3)");
		interface->declare(&fslider7, "unit", "Hz");
		interface->addHorizontalSlider("Modulation_Frequency", &fslider7, 2.2e+02f, 2e+01f, 1e+03f, 0.1f);
		interface->declare(&fentry1, "3", "");
		interface->declare(&fentry1, "tooltip", "0=theta is modulated by the incoming signal; 1=theta is modulated by the averaged incoming signal; 2=theta is modulated by the squared incoming signal; 3=theta is modulated by a sine wave of frequency freqMod; 4=theta is modulated by a sine wave of frequency freq;");
		interface->addNumEntry("Modulation_Type", &fentry1, 0.0f, 0.0f, 4.0f, 1.0f);
		interface->declare(&fslider9, "3", "");
		interface->declare(&fslider9, "tooltip", "Nonlinearity factor (value between 0 and 1)");
		interface->addHorizontalSlider("Nonlinearity", &fslider9, 0.0f, 0.0f, 1.0f, 0.01f);
		interface->declare(&fslider8, "3", "");
		interface->declare(&fslider8, "Attack duration of the nonlinearity", "");
		interface->declare(&fslider8, "unit", "s");
		interface->addHorizontalSlider("Nonlinearity Attack", &fslider8, 0.1f, 0.0f, 2.0f, 0.01f);
		interface->closeBox();
		interface->openVerticalBox("Physical_Parameters");
		interface->declare(&fslider10, "2", "");
		interface->declare(&fslider10, "tooltip", "Breath noise gain (value between 0 and 1)");
		interface->addHorizontalSlider("Noise Gain", &fslider10, 0.1f, 0.0f, 1.0f, 0.01f);
		interface->declare(&fslider11, "2", "");
		interface->declare(&fslider11, "tooltip", "Breath pressure (value bewteen 0 and 1)");
		interface->addHorizontalSlider("Pressure", &fslider11, 0.9f, 0.0f, 1.5f, 0.01f);
		interface->closeBox();
		interface->closeBox();
		interface->openVerticalBox("Spat");
		interface->addHorizontalSlider("pan angle", &fslider15, 0.6f, 0.0f, 1.0f, 0.01f);
		interface->addHorizontalSlider("spatial width", &fslider16, 0.5f, 0.0f, 1.0f, 0.01f);
		interface->closeBox();
		interface->closeBox();
	}
	virtual void compute (int count, FAUSTFLOAT** input, FAUSTFLOAT** output) {
		float 	fSlow0 = (0.0010000000000000009f * fentry0);
		int 	iSlow1 = int(fbutton0);
		int 	iSlow2 = (iSlow1 > 0);
		int 	iSlow3 = (iSlow1 <= 0);
		float 	fSlow4 = fslider0;
		float 	fSlow5 = (1 - (1.0f / powf(1e+05f,(1.0f / ((fSlow4 == 0.0f) + (iConst0 * fSlow4))))));
		float 	fSlow6 = fslider1;
		float 	fSlow7 = (1.0f / ((fSlow6 == 0.0f) + (iConst0 * fSlow6)));
		float 	fSlow8 = (fConst4 * fslider2);
		float 	fSlow9 = fslider3;
		float 	fSlow10 = (1 - (1.0f / powf(1e+05f,(1.0f / ((fSlow9 == 0.0f) + (iConst0 * fSlow9))))));
		float 	fSlow11 = fslider4;
		float 	fSlow12 = (iConst0 * fSlow11);
		float 	fSlow13 = ((fSlow11 == 0.0f) + fSlow12);
		float 	fSlow14 = fslider5;
		float 	fSlow15 = (1.0f / ((fSlow14 == 0.0f) + (iConst0 * fSlow14)));
		float 	fSlow16 = fslider6;
		float 	fSlow17 = (0.0010000000000000009f * fslider7);
		float 	fSlow18 = fentry1;
		int 	iSlow19 = (fSlow18 != 4);
		float 	fSlow20 = fentry2;
		float 	fSlow21 = (fSlow20 * (fSlow18 == 4));
		float 	fSlow22 = fslider8;
		float 	fSlow23 = (1.0f / ((fSlow22 == 0.0f) + (iConst0 * fSlow22)));
		float 	fSlow24 = (0.0010000000000000009f * fslider9);
		float 	fSlow25 = (float(iConst0) / fSlow20);
		int 	iSlow26 = int((fSlow25 - 2));
		int 	iSlow27 = int((1 + int((iSlow26 & 4095))));
		float 	fSlow28 = ((3 + iSlow26) - fSlow25);
		int 	iSlow29 = int((1 + int((int((1 + iSlow26)) & 4095))));
		float 	fSlow30 = (fSlow25 - (2 + iSlow26));
		int 	iSlow31 = (fSlow18 >= 3);
		float 	fSlow32 = (3.141592653589793f * (fSlow18 == 2));
		float 	fSlow33 = (3.141592653589793f * (fSlow18 == 0));
		float 	fSlow34 = (1.5707963267948966f * (fSlow18 == 1));
		int 	iSlow35 = (fSlow18 < 3);
		float 	fSlow36 = (5.122274162770377e-11f * fslider10);
		float 	fSlow37 = (0.0010000000000000009f * fslider11);
		int 	iSlow38 = (iSlow1 | int(fcheckbox0));
		int 	iSlow39 = (iSlow38 > 0);
		int 	iSlow40 = (iSlow38 <= 0);
		float 	fSlow41 = fslider12;
		float 	fSlow42 = (1 - (1.0f / powf(9e+04f,(1.0f / ((fSlow41 == 0.0f) + (iConst0 * fSlow41))))));
		float 	fSlow43 = fslider13;
		float 	fSlow44 = (1 - powf(9e+01f,(1.0f / ((fSlow43 == 0.0f) + (iConst0 * fSlow43)))));
		float 	fSlow45 = fslider14;
		float 	fSlow46 = (1.0f / ((fSlow45 == 0.0f) + (iConst0 * fSlow45)));
		float 	fSlow47 = (fConst6 / fSlow20);
		int 	iSlow48 = int((fSlow47 - 2));
		int 	iSlow49 = int((iSlow48 & 4095));
		float 	fSlow50 = ((3 + iSlow48) - fSlow47);
		int 	iSlow51 = int((int((1 + iSlow48)) & 4095));
		float 	fSlow52 = (fSlow47 - (2 + iSlow48));
		float 	fSlow53 = fslider15;
		float 	fSlow54 = (0.5f * (1.0f - fSlow53));
		int 	iSlow55 = int((int((fConst6 * (fslider16 / fSlow20))) & 4095));
		float 	fSlow56 = (0.5f * fSlow53);
		FAUSTFLOAT* output0 = output[0];
		FAUSTFLOAT* output1 = output[1];
		for (int i=0; i<count; i++) {
			fRec0[0] = (fSlow0 + (0.999f * fRec0[1]));
			iRec1[0] = (iSlow2 & (iRec1[1] | (fRec2[1] >= 1)));
			int iTemp0 = (iSlow3 & (fRec2[1] > 0));
			fRec2[0] = (((iTemp0 == 0) | (fRec2[1] >= 1e-06f)) * ((fSlow7 * (((iRec1[1] == 0) & iSlow2) & (fRec2[1] < 1))) + (fRec2[1] * (1 - (fSlow5 * iTemp0)))));
			float fTemp1 = (fSlow8 + fRec6[1]);
			fRec6[0] = (fTemp1 - floorf(fTemp1));
			iRec7[0] = (iSlow2 & (iRec7[1] | (fRec9[1] >= 1)));
			iRec8[0] = (iSlow2 * (1 + iRec8[1]));
			int iTemp2 = (iSlow3 & (fRec9[1] > 0));
			fRec9[0] = (((iTemp2 == 0) | (fRec9[1] >= 1e-06f)) * ((fSlow15 * ((1 - (iRec8[1] < fSlow13)) * ((((iRec7[1] == 0) & iSlow2) & (fRec9[1] < 1)) & (iRec8[1] > fSlow12)))) + (fRec9[1] * (1 - (fSlow10 * iTemp2)))));
			fRec11[0] = (fSlow17 + (0.999f * fRec11[1]));
			float fTemp3 = ((fConst4 * (fSlow21 + (iSlow19 * fRec11[0]))) + fRec10[1]);
			fRec10[0] = (fTemp3 - floorf(fTemp3));
			iRec12[0] = (iSlow2 & (iRec12[1] | (fRec13[1] >= 1)));
			int iTemp4 = (iSlow3 & (fRec13[1] > 0));
			fRec13[0] = (((iTemp4 == 0) | (fRec13[1] >= 1e-06f)) * ((fSlow23 * (((iRec12[1] == 0) & iSlow2) & (fRec13[1] < 1))) + (fRec13[1] * (1 - (fConst5 * iTemp4)))));
			fRec14[0] = (fSlow24 + (0.999f * fRec14[1]));
			float fTemp5 = (fRec14[0] * fRec13[0]);
			float fTemp6 = (3.141592653589793f * (fTemp5 * ftbl0[int((65536.0f * fRec10[0]))]));
			float fTemp7 = sinf(fTemp6);
			float fTemp8 = ((fSlow30 * fRec3[(IOTA-iSlow29)&8191]) + (fSlow28 * fRec3[(IOTA-iSlow27)&8191]));
			fVec0[0] = fTemp8;
			float fTemp9 = (0 - fTemp7);
			float fTemp10 = cosf(fTemp6);
			float fTemp11 = ((fVec0[0] * fTemp10) + (fTemp9 * fRec15[1]));
			float fTemp12 = ((fTemp10 * fTemp11) + (fTemp9 * fRec16[1]));
			float fTemp13 = ((fTemp10 * fTemp12) + (fTemp9 * fRec17[1]));
			float fTemp14 = ((fTemp10 * fTemp13) + (fTemp9 * fRec18[1]));
			float fTemp15 = ((fTemp10 * fTemp14) + (fTemp9 * fRec19[1]));
			fRec20[0] = ((fTemp10 * fTemp15) + (fTemp9 * fRec20[1]));
			fRec19[0] = ((fTemp10 * fRec20[1]) + (fTemp7 * fTemp15));
			fRec18[0] = ((fTemp10 * fRec19[1]) + (fTemp7 * fTemp14));
			fRec17[0] = ((fTemp10 * fRec18[1]) + (fTemp7 * fTemp13));
			fRec16[0] = ((fTemp10 * fRec17[1]) + (fTemp7 * fTemp12));
			fRec15[0] = ((fTemp10 * fRec16[1]) + (fTemp7 * fTemp11));
			float fTemp16 = (fTemp5 * (((fSlow34 * (fVec0[0] + fVec0[1])) + (fSlow33 * fVec0[0])) + (fSlow32 * faustpower<2>(fVec0[0]))));
			float fTemp17 = sinf(fTemp16);
			float fTemp18 = (0 - fTemp17);
			float fTemp19 = cosf(fTemp16);
			float fTemp20 = ((fVec0[0] * fTemp19) + (fTemp18 * fRec21[1]));
			float fTemp21 = ((fTemp19 * fTemp20) + (fTemp18 * fRec22[1]));
			float fTemp22 = ((fTemp19 * fTemp21) + (fTemp18 * fRec23[1]));
			float fTemp23 = ((fTemp19 * fTemp22) + (fTemp18 * fRec24[1]));
			float fTemp24 = ((fTemp19 * fTemp23) + (fTemp18 * fRec25[1]));
			fRec26[0] = ((fTemp19 * fTemp24) + (fTemp18 * fRec26[1]));
			fRec25[0] = ((fTemp19 * fRec26[1]) + (fTemp17 * fTemp24));
			fRec24[0] = ((fTemp19 * fRec25[1]) + (fTemp17 * fTemp23));
			fRec23[0] = ((fTemp19 * fRec24[1]) + (fTemp17 * fTemp22));
			fRec22[0] = ((fTemp19 * fRec23[1]) + (fTemp17 * fTemp21));
			fRec21[0] = ((fTemp19 * fRec22[1]) + (fTemp17 * fTemp20));
			float fTemp25 = (0.4f * ((iSlow35 * (((1 - fRec14[0]) * fVec0[0]) + (fRec14[0] * ((fTemp19 * fRec21[1]) + (fVec0[0] * fTemp17))))) + (iSlow31 * ((fTemp10 * fRec15[1]) + (fVec0[0] * fTemp7)))));
			iRec27[0] = (12345 + (1103515245 * iRec27[1]));
			fRec28[0] = (fSlow37 + (0.999f * fRec28[1]));
			iRec29[0] = (iSlow39 & (iRec29[1] | (fRec30[1] >= 1)));
			int iTemp26 = (iSlow40 & (fRec30[1] > 0));
			fRec30[0] = (((iTemp26 == 0) | (fRec30[1] >= 1e-06f)) * ((fSlow46 * (((iRec29[1] == 0) & iSlow39) & (fRec30[1] < 1))) + (fRec30[1] * ((1 - (fSlow44 * (iRec29[1] & (fRec30[1] > 90)))) - (fSlow42 * iTemp26)))));
			float fTemp27 = (((fRec30[0] * fRec28[0]) * (1.1f + (fSlow36 * iRec27[0]))) + (fTemp25 + (fSlow16 * (fRec9[0] * ftbl0[int((65536.0f * fRec6[0]))]))));
			fVec1[IOTA&4095] = fTemp27;
			float fTemp28 = (fSlow50 * fVec1[(IOTA-iSlow49)&4095]);
			float fTemp29 = (fSlow52 * fVec1[(IOTA-iSlow51)&4095]);
			float fTemp30 = ((fTemp28 + (fTemp25 + fTemp29)) - faustpower<3>((fTemp29 + fTemp28)));
			fVec2[0] = fTemp30;
			fRec4[0] = ((fConst7 * (fVec2[0] + fVec2[1])) + (fConst3 * fRec4[1]));
			fRec3[IOTA&8191] = fRec4[0];
			float fTemp31 = ((fRec3[(IOTA-0)&8191] * fRec2[0]) * fRec0[0]);
			fVec3[IOTA&4095] = fTemp31;
			output0[i] = (FAUSTFLOAT)(fSlow54 * fVec3[IOTA&4095]);
			output1[i] = (FAUSTFLOAT)(fSlow56 * fVec3[(IOTA-iSlow55)&4095]);
			// post processing
			fRec4[1] = fRec4[0];
			fVec2[1] = fVec2[0];
			IOTA = IOTA+1;
			fRec30[1] = fRec30[0];
			iRec29[1] = iRec29[0];
			fRec28[1] = fRec28[0];
			iRec27[1] = iRec27[0];
			fRec21[1] = fRec21[0];
			fRec22[1] = fRec22[0];
			fRec23[1] = fRec23[0];
			fRec24[1] = fRec24[0];
			fRec25[1] = fRec25[0];
			fRec26[1] = fRec26[0];
			fRec15[1] = fRec15[0];
			fRec16[1] = fRec16[0];
			fRec17[1] = fRec17[0];
			fRec18[1] = fRec18[0];
			fRec19[1] = fRec19[0];
			fRec20[1] = fRec20[0];
			fVec0[1] = fVec0[0];
			fRec14[1] = fRec14[0];
			fRec13[1] = fRec13[0];
			iRec12[1] = iRec12[0];
			fRec10[1] = fRec10[0];
			fRec11[1] = fRec11[0];
			fRec9[1] = fRec9[0];
			iRec8[1] = iRec8[0];
			iRec7[1] = iRec7[0];
			fRec6[1] = fRec6[0];
			fRec2[1] = fRec2[0];
			iRec1[1] = iRec1[0];
			fRec0[1] = fRec0[0];
		}
	}
};


float 	Flute_dsp::ftbl0[65536];



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
