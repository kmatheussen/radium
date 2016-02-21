//-----------------------------------------------------
// name: "Tibetan Bowl"
// author: "Romain Michon"
// copyright: "Romain Michon (rmichon@ccrma.stanford.edu)"
// version: "1.0"
//
// Code generated with Faust 0.9.73 (http://faust.grame.fr)
//-----------------------------------------------------
/* link with  */
#include <math.h>
#ifndef FAUSTPOWER
#define FAUSTPOWER
#include <cmath>
template <int N> inline float faustpower(float x)          { return powf(x,N); } 
template <int N> inline double faustpower(double x)        { return pow(x,N); }
template <int N> inline int faustpower(int x)              { return faustpower<N/2>(x) * faustpower<N-N/2>(x); } 
template <> 	 inline int faustpower<0>(int x)            { return 1; }
template <> 	 inline int faustpower<1>(int x)            { return x; }
template <> 	 inline int faustpower<2>(int x)            { return x*x; }
#endif
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


#ifndef FAUSTCLASS 
#define FAUSTCLASS Tibetan_Bowl_dsp
#endif

class Tibetan_Bowl_dsp : public dsp {
  private:
	class SIG0 {
	  private:
		int 	fSamplingFreq;
		int 	iRec43[2];
	  public:
		int getNumInputs() 	{ return 0; }
		int getNumOutputs() 	{ return 1; }
		void init(int samplingFreq) {
			fSamplingFreq = samplingFreq;
			for (int i=0; i<2; i++) iRec43[i] = 0;
		}
		void fill (int count, float output[]) {
			for (int i=0; i<count; i++) {
				iRec43[0] = (1 + iRec43[1]);
				output[i] = sinf((9.587379924285257e-05f * float((iRec43[0] - 1))));
				// post processing
				iRec43[1] = iRec43[0];
			}
		}
	};


	FAUSTFLOAT 	fentry0;
	FAUSTFLOAT 	fbutton0;
	int 	iRec14[2];
	int 	iConst0;
	float 	fConst1;
	float 	fConst2;
	float 	fConst3;
	float 	fRec15[2];
	FAUSTFLOAT 	fentry1;
	FAUSTFLOAT 	fslider0;
	FAUSTFLOAT 	fslider1;
	FAUSTFLOAT 	fslider2;
	int 	IOTA;
	float 	fVec0[4096];
	FAUSTFLOAT 	fentry2;
	float 	fConst4;
	float 	fConst5;
	float 	fConst6;
	float 	fConst7;
	float 	fConst8;
	float 	fRec13[3];
	float 	fConst9;
	float 	fConst10;
	float 	fConst11;
	float 	fRec12[2];
	float 	fRec0[2];
	float 	fVec1[4096];
	float 	fConst12;
	float 	fConst13;
	float 	fRec17[3];
	float 	fRec16[2];
	float 	fRec1[2];
	float 	fVec2[4096];
	float 	fConst14;
	float 	fConst15;
	float 	fRec19[3];
	float 	fRec18[2];
	float 	fRec2[2];
	float 	fVec3[4096];
	float 	fConst16;
	float 	fConst17;
	float 	fRec21[3];
	float 	fRec20[2];
	float 	fRec3[2];
	float 	fVec4[2048];
	float 	fConst18;
	float 	fConst19;
	float 	fRec23[3];
	float 	fRec22[2];
	float 	fRec4[2];
	float 	fRec5[2];
	float 	fVec5[2048];
	float 	fConst20;
	float 	fConst21;
	float 	fRec25[3];
	float 	fRec24[2];
	float 	fRec6[2];
	float 	fVec6[2048];
	float 	fConst22;
	float 	fConst23;
	float 	fRec27[3];
	float 	fRec26[2];
	float 	fRec7[2];
	float 	fVec7[1024];
	float 	fConst24;
	float 	fConst25;
	float 	fRec29[3];
	float 	fRec28[2];
	float 	fRec8[2];
	float 	fVec8[1024];
	float 	fConst26;
	float 	fConst27;
	float 	fRec31[3];
	float 	fRec30[2];
	float 	fRec9[2];
	float 	fVec9[1024];
	float 	fConst28;
	float 	fConst29;
	float 	fRec33[3];
	float 	fRec32[2];
	float 	fRec10[2];
	float 	fVec10[512];
	float 	fConst30;
	float 	fConst31;
	float 	fRec35[3];
	float 	fRec34[2];
	float 	fRec11[2];
	float 	fVec11[2];
	FAUSTFLOAT 	fslider3;
	float 	fRec36[2];
	FAUSTFLOAT 	fentry3;
	float 	fRec42[2];
	float 	fRec41[2];
	float 	fRec40[2];
	float 	fRec39[2];
	float 	fRec38[2];
	float 	fRec37[2];
	static float 	ftbl0[65536];
	FAUSTFLOAT 	fslider4;
	float 	fRec45[2];
	float 	fConst32;
	float 	fRec44[2];
	float 	fRec51[2];
	float 	fRec50[2];
	float 	fRec49[2];
	float 	fRec48[2];
	float 	fRec47[2];
	float 	fRec46[2];
	int 	iRec52[2];
	FAUSTFLOAT 	fslider5;
	FAUSTFLOAT 	fslider6;
	float 	fRec53[2];
	float 	fVec12[4096];
	FAUSTFLOAT 	fslider7;
	FAUSTFLOAT 	fslider8;
	float 	fConst33;
  public:
	static void metadata(Meta* m) 	{ 
		m->declare("name", "Tibetan Bowl");
		m->declare("description", "Banded Waveguide Modeld Tibetan Bowl");
		m->declare("author", "Romain Michon");
		m->declare("copyright", "Romain Michon (rmichon@ccrma.stanford.edu)");
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
		m->declare("effect.lib/exciter_name", "Harmonic Exciter");
		m->declare("effect.lib/exciter_author", "Priyanka Shekar (pshekar@ccrma.stanford.edu)");
		m->declare("effect.lib/exciter_copyright", "Copyright (c) 2013 Priyanka Shekar");
		m->declare("effect.lib/exciter_version", "1.0");
		m->declare("effect.lib/exciter_license", "MIT License (MIT)");
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
		fentry0 = 0.0f;
		fbutton0 = 0.0;
		for (int i=0; i<2; i++) iRec14[i] = 0;
		iConst0 = min(192000, max(1, fSamplingFreq));
		fConst1 = (1 - (1.0f / powf(9e+04f,(1e+02f / float(iConst0)))));
		fConst2 = (1 - powf(9e+01f,(2e+02f / float(iConst0))));
		fConst3 = (5e+01f / float(iConst0));
		for (int i=0; i<2; i++) fRec15[i] = 0;
		fentry1 = 0.8f;
		fslider0 = 1.0f;
		fslider1 = 0.0f;
		fslider2 = 0.2f;
		IOTA = 0;
		for (int i=0; i<4096; i++) fVec0[i] = 0;
		fentry2 = 4.4e+02f;
		fConst4 = (1.0039068601557664f * iConst0);
		fConst5 = (1 - (100.53096491487338f / float(iConst0)));
		fConst6 = faustpower<2>(fConst5);
		fConst7 = (6.258733311379789f / float(iConst0));
		fConst8 = (0 - (2 * fConst5));
		for (int i=0; i<3; i++) fRec13[i] = 0;
		fConst9 = (0.5f * fConst6);
		fConst10 = (fConst9 - 0.5f);
		fConst11 = (0.5f - fConst9);
		for (int i=0; i<2; i++) fRec12[i] = 0;
		for (int i=0; i<2; i++) fRec0[i] = 0;
		for (int i=0; i<4096; i++) fVec1[i] = 0;
		fConst12 = (0.9961234300773741f * iConst0);
		fConst13 = (6.30763730423602f / float(iConst0));
		for (int i=0; i<3; i++) fRec17[i] = 0;
		for (int i=0; i<2; i++) fRec16[i] = 0;
		for (int i=0; i<2; i++) fRec1[i] = 0;
		for (int i=0; i<4096; i++) fVec2[i] = 0;
		fConst14 = (0.335663058736336f * iConst0);
		fConst15 = (18.718727437072666f / float(iConst0));
		for (int i=0; i<3; i++) fRec19[i] = 0;
		for (int i=0; i<2; i++) fRec18[i] = 0;
		for (int i=0; i<2; i++) fRec2[i] = 0;
		for (int i=0; i<4096; i++) fVec3[i] = 0;
		fConst16 = (0.3340797041411521f * iConst0);
		fConst17 = (18.80744394015889f / float(iConst0));
		for (int i=0; i<3; i++) fRec21[i] = 0;
		for (int i=0; i<2; i++) fRec20[i] = 0;
		for (int i=0; i<2; i++) fRec3[i] = 0;
		for (int i=0; i<2048; i++) fVec4[i] = 0;
		fConst18 = (0.1753016766553562f * iConst0);
		fConst19 = (35.8421289919112f / float(iConst0));
		for (int i=0; i<3; i++) fRec23[i] = 0;
		for (int i=0; i<2; i++) fRec22[i] = 0;
		for (int i=0; i<2; i++) fRec4[i] = 0;
		for (int i=0; i<2; i++) fRec5[i] = 0;
		for (int i=0; i<2048; i++) fVec5[i] = 0;
		fConst20 = (0.11113333777866684f * iConst0);
		fConst21 = (56.537358031063356f / float(iConst0));
		for (int i=0; i<3; i++) fRec25[i] = 0;
		for (int i=0; i<2; i++) fRec24[i] = 0;
		for (int i=0; i<2; i++) fRec6[i] = 0;
		for (int i=0; i<2048; i++) fVec6[i] = 0;
		fConst22 = (0.11092011579181602f * iConst0);
		fConst23 = (56.64603992094982f / float(iConst0));
		for (int i=0; i<3; i++) fRec27[i] = 0;
		for (int i=0; i<2; i++) fRec26[i] = 0;
		for (int i=0; i<2; i++) fRec7[i] = 0;
		for (int i=0; i<1024; i++) fVec7[i] = 0;
		fConst24 = (0.07792391976018134f * iConst0);
		fConst25 = (80.63230554259485f / float(iConst0));
		for (int i=0; i<3; i++) fRec29[i] = 0;
		for (int i=0; i<2; i++) fRec28[i] = 0;
		for (int i=0; i<2; i++) fRec8[i] = 0;
		for (int i=0; i<1024; i++) fVec8[i] = 0;
		fConst26 = (0.07807996981740686f * iConst0);
		fConst27 = (80.4711544058363f / float(iConst0));
		for (int i=0; i<3; i++) fRec31[i] = 0;
		for (int i=0; i<2; i++) fRec30[i] = 0;
		for (int i=0; i<2; i++) fRec9[i] = 0;
		for (int i=0; i<1024; i++) fVec9[i] = 0;
		fConst28 = (0.05786761797481404f * iConst0);
		fConst29 = (108.57860625806721f / float(iConst0));
		for (int i=0; i<3; i++) fRec33[i] = 0;
		for (int i=0; i<2; i++) fRec32[i] = 0;
		for (int i=0; i<2; i++) fRec10[i] = 0;
		for (int i=0; i<512; i++) fVec10[i] = 0;
		fConst30 = (0.04550412965560287f * iConst0);
		fConst31 = (138.07945245264008f / float(iConst0));
		for (int i=0; i<3; i++) fRec35[i] = 0;
		for (int i=0; i<2; i++) fRec34[i] = 0;
		for (int i=0; i<2; i++) fRec11[i] = 0;
		for (int i=0; i<2; i++) fVec11[i] = 0;
		fslider3 = 0.0f;
		for (int i=0; i<2; i++) fRec36[i] = 0;
		fentry3 = 0.0f;
		for (int i=0; i<2; i++) fRec42[i] = 0;
		for (int i=0; i<2; i++) fRec41[i] = 0;
		for (int i=0; i<2; i++) fRec40[i] = 0;
		for (int i=0; i<2; i++) fRec39[i] = 0;
		for (int i=0; i<2; i++) fRec38[i] = 0;
		for (int i=0; i<2; i++) fRec37[i] = 0;
		fslider4 = 2.2e+02f;
		for (int i=0; i<2; i++) fRec45[i] = 0;
		fConst32 = (1.0f / float(iConst0));
		for (int i=0; i<2; i++) fRec44[i] = 0;
		for (int i=0; i<2; i++) fRec51[i] = 0;
		for (int i=0; i<2; i++) fRec50[i] = 0;
		for (int i=0; i<2; i++) fRec49[i] = 0;
		for (int i=0; i<2; i++) fRec48[i] = 0;
		for (int i=0; i<2; i++) fRec47[i] = 0;
		for (int i=0; i<2; i++) fRec46[i] = 0;
		for (int i=0; i<2; i++) iRec52[i] = 0;
		fslider5 = 0.1f;
		fslider6 = 0.02f;
		for (int i=0; i<2; i++) fRec53[i] = 0;
		for (int i=0; i<4096; i++) fVec12[i] = 0;
		fslider7 = 0.6f;
		fslider8 = 0.5f;
		fConst33 = (0.5f * iConst0);
	}
	virtual void init(int samplingFreq) {
		classInit(samplingFreq);
		instanceInit(samplingFreq);
	}
	virtual void buildUserInterface(UI* interface) {
		interface->openVerticalBox("0x00");
		interface->openHorizontalBox("Basic_Parameters");
		interface->declare(&fentry2, "1", "");
		interface->declare(&fentry2, "tooltip", "Tone frequency");
		interface->declare(&fentry2, "unit", "Hz");
		interface->addNumEntry("freq", &fentry2, 4.4e+02f, 2e+01f, 2e+04f, 1.0f);
		interface->declare(&fentry1, "1", "");
		interface->declare(&fentry1, "tooltip", "Gain (value between 0 and 1)");
		interface->addNumEntry("gain", &fentry1, 0.8f, 0.0f, 1.0f, 0.01f);
		interface->declare(&fbutton0, "1", "");
		interface->declare(&fbutton0, "tooltip", "noteOn = 1, noteOff = 0");
		interface->addButton("gate", &fbutton0);
		interface->closeBox();
		interface->openHorizontalBox("Envelopes_and_Vibrato");
		interface->openVerticalBox("Global_Envelope_Parameters");
		interface->declare(&fslider6, "4", "");
		interface->declare(&fslider6, "tooltip", "Global envelope attack duration");
		interface->declare(&fslider6, "unit", "s");
		interface->addHorizontalSlider("Glob_Env_Attack", &fslider6, 0.02f, 0.0f, 2.0f, 0.01f);
		interface->declare(&fslider5, "4", "");
		interface->declare(&fslider5, "tooltip", "Global envelope release duration");
		interface->declare(&fslider5, "unit", "s");
		interface->addHorizontalSlider("Glob_Env_Release", &fslider5, 0.1f, 0.0f, 2.0f, 0.01f);
		interface->closeBox();
		interface->closeBox();
		interface->openHorizontalBox("Physical_and_Nonlinearity");
		interface->openVerticalBox("Nonlinear_Filter_Parameters");
		interface->declare(&fslider4, "3", "");
		interface->declare(&fslider4, "tooltip", "Frequency of the sine wave for the modulation of theta (works if Modulation Type=3)");
		interface->declare(&fslider4, "unit", "Hz");
		interface->addHorizontalSlider("Modulation_Frequency", &fslider4, 2.2e+02f, 2e+01f, 1e+03f, 0.1f);
		interface->declare(&fentry3, "3", "");
		interface->declare(&fentry3, "tooltip", "0=theta is modulated by the incoming signal; 1=theta is modulated by the averaged incoming signal; 2=theta is modulated by the squared incoming signal; 3=theta is modulated by a sine wave of frequency freqMod; 4=theta is modulated by a sine wave of frequency freq;");
		interface->addNumEntry("Modulation_Type", &fentry3, 0.0f, 0.0f, 4.0f, 1.0f);
		interface->declare(&fslider3, "3", "");
		interface->declare(&fslider3, "tooltip", "Nonlinearity factor (value between 0 and 1)");
		interface->addHorizontalSlider("Nonlinearity", &fslider3, 0.0f, 0.0f, 1.0f, 0.01f);
		interface->closeBox();
		interface->openVerticalBox("Physical_Parameters");
		interface->declare(&fslider0, "2", "");
		interface->declare(&fslider0, "tooltip", "A value between 0 and 1");
		interface->addHorizontalSlider("Base_Gain", &fslider0, 1.0f, 0.0f, 1.0f, 0.01f);
		interface->declare(&fslider2, "2", "");
		interface->declare(&fslider2, "tooltip", "Bow pressure on the instrument (Value between 0 and 1)");
		interface->addHorizontalSlider("Bow_Pressure", &fslider2, 0.2f, 0.0f, 1.0f, 0.01f);
		interface->declare(&fentry0, "2", "");
		interface->declare(&fentry0, "tooltip", "0=Bow; 1=Strike");
		interface->addNumEntry("Excitation_Selector", &fentry0, 0.0f, 0.0f, 1.0f, 1.0f);
		interface->declare(&fslider1, "2", "");
		interface->declare(&fslider1, "tooltip", "A value between 0 and 1");
		interface->addHorizontalSlider("Integration_Constant", &fslider1, 0.0f, 0.0f, 1.0f, 0.01f);
		interface->closeBox();
		interface->closeBox();
		interface->openVerticalBox("Spat");
		interface->addHorizontalSlider("pan angle", &fslider7, 0.6f, 0.0f, 1.0f, 0.01f);
		interface->addHorizontalSlider("spatial width", &fslider8, 0.5f, 0.0f, 1.0f, 0.01f);
		interface->closeBox();
		interface->closeBox();
	}
	virtual void compute (int count, FAUSTFLOAT** input, FAUSTFLOAT** output) {
		float 	fSlow0 = float(fentry0);
		float 	fSlow1 = (1.1900357f * fSlow0);
		float 	fSlow2 = float(fbutton0);
		int 	iSlow3 = (fSlow2 > 0);
		int 	iSlow4 = (fSlow2 <= 0);
		float 	fSlow5 = (0.03f + (0.1f * float(fentry1)));
		float 	fSlow6 = (0.8999999999999999f + (0.1f * float(fslider0)));
		float 	fSlow7 = float(fslider1);
		float 	fSlow8 = (10 - (9 * float(fslider2)));
		float 	fSlow9 = (0.08333333333333333f * (0 - (fSlow0 - 1)));
		float 	fSlow10 = float(fentry2);
		int 	iSlow11 = int((int((fConst4 / fSlow10)) & 4095));
		float 	fSlow12 = (fConst8 * cosf((fConst7 * fSlow10)));
		int 	iSlow13 = int((int((fConst12 / fSlow10)) & 4095));
		float 	fSlow14 = (fConst8 * cosf((fConst13 * fSlow10)));
		float 	fSlow15 = (1.0914886f * fSlow0);
		int 	iSlow16 = int((int((fConst14 / fSlow10)) & 4095));
		float 	fSlow17 = (fConst8 * cosf((fConst15 * fSlow10)));
		int 	iSlow18 = int((int((fConst16 / fSlow10)) & 4095));
		float 	fSlow19 = (fConst8 * cosf((fConst17 * fSlow10)));
		float 	fSlow20 = (4.2995041f * fSlow0);
		int 	iSlow21 = int((int((fConst18 / fSlow10)) & 4095));
		float 	fSlow22 = (fConst8 * cosf((fConst19 * fSlow10)));
		float 	fSlow23 = (4.0063034f * fSlow0);
		int 	iSlow24 = int((int((fConst20 / fSlow10)) & 4095));
		float 	fSlow25 = (fConst8 * cosf((fConst21 * fSlow10)));
		int 	iSlow26 = int((int((fConst22 / fSlow10)) & 4095));
		float 	fSlow27 = (fConst8 * cosf((fConst23 * fSlow10)));
		float 	fSlow28 = (0.7063034f * fSlow0);
		int 	iSlow29 = int((int((fConst24 / fSlow10)) & 4095));
		float 	fSlow30 = (fConst8 * cosf((fConst25 * fSlow10)));
		int 	iSlow31 = int((int((fConst26 / fSlow10)) & 4095));
		float 	fSlow32 = (fConst8 * cosf((fConst27 * fSlow10)));
		float 	fSlow33 = (5.7063034f * fSlow0);
		int 	iSlow34 = int((int((fConst28 / fSlow10)) & 4095));
		float 	fSlow35 = (fConst8 * cosf((fConst29 * fSlow10)));
		int 	iSlow36 = int((int((fConst30 / fSlow10)) & 4095));
		float 	fSlow37 = (fConst8 * cosf((fConst31 * fSlow10)));
		float 	fSlow38 = (0.0010000000000000009f * float(fslider3));
		float 	fSlow39 = float(fentry3);
		float 	fSlow40 = (3.141592653589793f * (fSlow39 == 2));
		float 	fSlow41 = (1.5707963267948966f * (fSlow39 == 1));
		float 	fSlow42 = (3.141592653589793f * (fSlow39 == 0));
		int 	iSlow43 = (fSlow39 < 3);
		float 	fSlow44 = (0.0010000000000000009f * float(fslider4));
		int 	iSlow45 = (fSlow39 != 4);
		float 	fSlow46 = (fSlow10 * (fSlow39 == 4));
		int 	iSlow47 = (fSlow39 >= 3);
		float 	fSlow48 = float(fslider5);
		float 	fSlow49 = (1 - (1.0f / powf(1e+05f,(1.0f / ((fSlow48 == 0.0f) + (iConst0 * fSlow48))))));
		float 	fSlow50 = float(fslider6);
		float 	fSlow51 = (1.0f / ((fSlow50 == 0.0f) + (iConst0 * fSlow50)));
		float 	fSlow52 = float(fslider7);
		float 	fSlow53 = (0.5f * (1.0f - fSlow52));
		int 	iSlow54 = int((int((fConst33 * (float(fslider8) / fSlow10))) & 4095));
		float 	fSlow55 = (0.5f * fSlow52);
		FAUSTFLOAT* output0 = output[0];
		FAUSTFLOAT* output1 = output[1];
		for (int i=0; i<count; i++) {
			iRec14[0] = (iSlow3 & (iRec14[1] | (fRec15[1] >= 1)));
			int iTemp0 = (iSlow4 & (fRec15[1] > 0));
			fRec15[0] = (((fConst3 * (((iRec14[1] == 0) & iSlow3) & (fRec15[1] < 1))) + (fRec15[1] * ((1 - (fConst2 * (iRec14[1] & (fRec15[1] > 90)))) - (fConst1 * iTemp0)))) * ((iTemp0 == 0) | (fRec15[1] >= 1e-06f)));
			float fTemp1 = (0 - ((fSlow7 + (fSlow6 * ((((((fRec0[1] + fRec2[1]) + fRec4[1]) + fRec6[1]) + fRec8[1]) + fRec10[1]) + (((((fRec1[1] + fRec3[1]) + fRec5[1]) + fRec7[1]) + fRec9[1]) + fRec11[1])))) - (fSlow5 * fRec15[0])));
			float fTemp2 = faustpower<4>((0.75f + fabsf((fSlow8 * fTemp1))));
			float fTemp3 = (1.0f / fTemp2);
			float fTemp4 = (fSlow9 * (fTemp1 * ((fTemp3 > 1) + (float((fTemp3 <= 1)) / fTemp2))));
			fVec0[IOTA&4095] = ((fRec12[1] + fTemp4) + fSlow1);
			fRec13[0] = (0 - (((fSlow12 * fRec13[1]) + (fConst6 * fRec13[2])) - (0.999925960128219f * fVec0[(IOTA-iSlow11)&4095])));
			fRec12[0] = ((fConst11 * fRec13[0]) + (fConst10 * fRec13[2]));
			fRec0[0] = fRec12[0];
			fVec1[IOTA&4095] = (fSlow1 + (fTemp4 + fRec16[1]));
			fRec17[0] = (0 - (((fSlow14 * fRec17[1]) + (fConst6 * fRec17[2])) - (0.999925960128219f * fVec1[(IOTA-iSlow13)&4095])));
			fRec16[0] = ((fConst11 * fRec17[0]) + (fConst10 * fRec17[2]));
			fRec1[0] = fRec16[0];
			fVec2[IOTA&4095] = ((fTemp4 + fRec18[1]) + fSlow15);
			fRec19[0] = (0 - (((fSlow17 * fRec19[1]) + (fConst6 * fRec19[2])) - (0.999982774366897f * fVec2[(IOTA-iSlow16)&4095])));
			fRec18[0] = ((fConst11 * fRec19[0]) + (fConst10 * fRec19[2]));
			fRec2[0] = fRec18[0];
			fVec3[IOTA&4095] = (fSlow15 + (fTemp4 + fRec20[1]));
			fRec21[0] = (0 - (((fSlow19 * fRec21[1]) + (fConst6 * fRec21[2])) - (0.999982774366897f * fVec3[(IOTA-iSlow18)&4095])));
			fRec20[0] = ((fConst11 * fRec21[0]) + (fConst10 * fRec21[2]));
			fRec3[0] = fRec20[0];
			fVec4[IOTA&2047] = ((fTemp4 + fRec22[1]) + fSlow20);
			fRec23[0] = (0 - (((fSlow22 * fRec23[1]) + (fConst6 * fRec23[2])) - fVec4[(IOTA-iSlow21)&2047]));
			fRec22[0] = ((fConst11 * fRec23[0]) + (fConst10 * fRec23[2]));
			fRec4[0] = fRec22[0];
			fRec5[0] = fRec22[0];
			fVec5[IOTA&2047] = ((fTemp4 + fRec24[1]) + fSlow23);
			fRec25[0] = (0 - (((fSlow25 * fRec25[1]) + (fConst6 * fRec25[2])) - fVec5[(IOTA-iSlow24)&2047]));
			fRec24[0] = ((fConst11 * fRec25[0]) + (fConst10 * fRec25[2]));
			fRec6[0] = fRec24[0];
			fVec6[IOTA&2047] = (fSlow23 + (fTemp4 + fRec26[1]));
			fRec27[0] = (0 - (((fSlow27 * fRec27[1]) + (fConst6 * fRec27[2])) - fVec6[(IOTA-iSlow26)&2047]));
			fRec26[0] = ((fConst11 * fRec27[0]) + (fConst10 * fRec27[2]));
			fRec7[0] = fRec26[0];
			fVec7[IOTA&1023] = ((fTemp4 + fRec28[1]) + fSlow28);
			fRec29[0] = (0 - (((fSlow30 * fRec29[1]) + (fConst6 * fRec29[2])) - (0.999965497558225f * fVec7[(IOTA-iSlow29)&1023])));
			fRec28[0] = ((fConst11 * fRec29[0]) + (fConst10 * fRec29[2]));
			fRec8[0] = fRec28[0];
			fVec8[IOTA&1023] = (fSlow28 + (fTemp4 + fRec30[1]));
			fRec31[0] = (0 - (((fSlow32 * fRec31[1]) + (fConst6 * fRec31[2])) - (0.999965497558225f * fVec8[(IOTA-iSlow31)&1023])));
			fRec30[0] = ((fConst11 * fRec31[0]) + (fConst10 * fRec31[2]));
			fRec9[0] = fRec30[0];
			fVec9[IOTA&1023] = ((fTemp4 + fRec32[1]) + fSlow33);
			fRec33[0] = (0 - (((fSlow35 * fRec33[1]) + (fConst6 * fRec33[2])) - fVec9[(IOTA-iSlow34)&1023]));
			fRec32[0] = ((fConst11 * fRec33[0]) + (fConst10 * fRec33[2]));
			fRec10[0] = fRec32[0];
			fVec10[IOTA&511] = (fSlow33 + (fTemp4 + fRec34[1]));
			fRec35[0] = (0 - (((fSlow37 * fRec35[1]) + (fConst6 * fRec35[2])) - fVec10[(IOTA-iSlow36)&511]));
			fRec34[0] = ((fConst11 * fRec35[0]) + (fConst10 * fRec35[2]));
			fRec11[0] = fRec34[0];
			float fTemp5 = (fRec11[0] + (fRec9[0] + (fRec7[0] + (fRec5[0] + (fRec3[0] + ((((((fRec0[0] + fRec2[0]) + fRec4[0]) + fRec6[0]) + fRec8[0]) + fRec10[0]) + fRec1[0]))))));
			fVec11[0] = fTemp5;
			fRec36[0] = (fSlow38 + (0.999f * fRec36[1]));
			float fTemp6 = (fRec36[0] * (((fSlow42 * fVec11[0]) + (fSlow41 * (fVec11[0] + fVec11[1]))) + (fSlow40 * faustpower<2>(fVec11[0]))));
			float fTemp7 = cosf(fTemp6);
			float fTemp8 = sinf(fTemp6);
			float fTemp9 = (0 - fTemp8);
			float fTemp10 = ((fRec37[1] * fTemp9) + (fVec11[0] * fTemp7));
			float fTemp11 = ((fTemp9 * fRec38[1]) + (fTemp7 * fTemp10));
			float fTemp12 = ((fTemp9 * fRec39[1]) + (fTemp7 * fTemp11));
			float fTemp13 = ((fTemp9 * fRec40[1]) + (fTemp7 * fTemp12));
			float fTemp14 = ((fTemp9 * fRec41[1]) + (fTemp7 * fTemp13));
			fRec42[0] = ((fTemp9 * fRec42[1]) + (fTemp7 * fTemp14));
			fRec41[0] = ((fTemp8 * fTemp14) + (fTemp7 * fRec42[1]));
			fRec40[0] = ((fTemp8 * fTemp13) + (fTemp7 * fRec41[1]));
			fRec39[0] = ((fTemp8 * fTemp12) + (fTemp7 * fRec40[1]));
			fRec38[0] = ((fTemp8 * fTemp11) + (fTemp7 * fRec39[1]));
			fRec37[0] = ((fTemp8 * fTemp10) + (fTemp7 * fRec38[1]));
			fRec45[0] = (fSlow44 + (0.999f * fRec45[1]));
			float fTemp15 = (fRec44[1] + (fConst32 * (fSlow46 + (iSlow45 * fRec45[0]))));
			fRec44[0] = (fTemp15 - floorf(fTemp15));
			float fTemp16 = (3.141592653589793f * (fRec36[0] * ftbl0[int((65536.0f * fRec44[0]))]));
			float fTemp17 = cosf(fTemp16);
			float fTemp18 = sinf(fTemp16);
			float fTemp19 = (0 - fTemp18);
			float fTemp20 = ((fRec46[1] * fTemp19) + (fVec11[0] * fTemp17));
			float fTemp21 = ((fTemp19 * fRec47[1]) + (fTemp17 * fTemp20));
			float fTemp22 = ((fTemp19 * fRec48[1]) + (fTemp17 * fTemp21));
			float fTemp23 = ((fTemp19 * fRec49[1]) + (fTemp17 * fTemp22));
			float fTemp24 = ((fTemp19 * fRec50[1]) + (fTemp17 * fTemp23));
			fRec51[0] = ((fTemp19 * fRec51[1]) + (fTemp17 * fTemp24));
			fRec50[0] = ((fTemp18 * fTemp24) + (fTemp17 * fRec51[1]));
			fRec49[0] = ((fTemp18 * fTemp23) + (fTemp17 * fRec50[1]));
			fRec48[0] = ((fTemp18 * fTemp22) + (fTemp17 * fRec49[1]));
			fRec47[0] = ((fTemp18 * fTemp21) + (fTemp17 * fRec48[1]));
			fRec46[0] = ((fTemp18 * fTemp20) + (fTemp17 * fRec47[1]));
			iRec52[0] = (iSlow3 & (iRec52[1] | (fRec53[1] >= 1)));
			int iTemp25 = (iSlow4 & (fRec53[1] > 0));
			fRec53[0] = (((fSlow51 * (((iRec52[1] == 0) & iSlow3) & (fRec53[1] < 1))) + (fRec53[1] * (1 - (fSlow49 * iTemp25)))) * ((iTemp25 == 0) | (fRec53[1] >= 1e-06f)));
			float fTemp26 = (fRec53[0] * ((iSlow47 * ((fVec11[0] * fTemp18) + (fRec46[1] * fTemp17))) + (iSlow43 * ((fRec36[0] * ((fVec11[0] * fTemp8) + (fRec37[1] * fTemp7))) + ((1 - fRec36[0]) * fVec11[0])))));
			fVec12[IOTA&4095] = fTemp26;
			output0[i] = (FAUSTFLOAT)(fSlow53 * fVec12[IOTA&4095]);
			output1[i] = (FAUSTFLOAT)(fSlow55 * fVec12[(IOTA-iSlow54)&4095]);
			// post processing
			fRec53[1] = fRec53[0];
			iRec52[1] = iRec52[0];
			fRec46[1] = fRec46[0];
			fRec47[1] = fRec47[0];
			fRec48[1] = fRec48[0];
			fRec49[1] = fRec49[0];
			fRec50[1] = fRec50[0];
			fRec51[1] = fRec51[0];
			fRec44[1] = fRec44[0];
			fRec45[1] = fRec45[0];
			fRec37[1] = fRec37[0];
			fRec38[1] = fRec38[0];
			fRec39[1] = fRec39[0];
			fRec40[1] = fRec40[0];
			fRec41[1] = fRec41[0];
			fRec42[1] = fRec42[0];
			fRec36[1] = fRec36[0];
			fVec11[1] = fVec11[0];
			fRec11[1] = fRec11[0];
			fRec34[1] = fRec34[0];
			fRec35[2] = fRec35[1]; fRec35[1] = fRec35[0];
			fRec10[1] = fRec10[0];
			fRec32[1] = fRec32[0];
			fRec33[2] = fRec33[1]; fRec33[1] = fRec33[0];
			fRec9[1] = fRec9[0];
			fRec30[1] = fRec30[0];
			fRec31[2] = fRec31[1]; fRec31[1] = fRec31[0];
			fRec8[1] = fRec8[0];
			fRec28[1] = fRec28[0];
			fRec29[2] = fRec29[1]; fRec29[1] = fRec29[0];
			fRec7[1] = fRec7[0];
			fRec26[1] = fRec26[0];
			fRec27[2] = fRec27[1]; fRec27[1] = fRec27[0];
			fRec6[1] = fRec6[0];
			fRec24[1] = fRec24[0];
			fRec25[2] = fRec25[1]; fRec25[1] = fRec25[0];
			fRec5[1] = fRec5[0];
			fRec4[1] = fRec4[0];
			fRec22[1] = fRec22[0];
			fRec23[2] = fRec23[1]; fRec23[1] = fRec23[0];
			fRec3[1] = fRec3[0];
			fRec20[1] = fRec20[0];
			fRec21[2] = fRec21[1]; fRec21[1] = fRec21[0];
			fRec2[1] = fRec2[0];
			fRec18[1] = fRec18[0];
			fRec19[2] = fRec19[1]; fRec19[1] = fRec19[0];
			fRec1[1] = fRec1[0];
			fRec16[1] = fRec16[0];
			fRec17[2] = fRec17[1]; fRec17[1] = fRec17[0];
			fRec0[1] = fRec0[0];
			fRec12[1] = fRec12[0];
			fRec13[2] = fRec13[1]; fRec13[1] = fRec13[0];
			IOTA = IOTA+1;
			fRec15[1] = fRec15[0];
			iRec14[1] = iRec14[0];
		}
	}
};


float 	Tibetan_Bowl_dsp::ftbl0[65536];



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

    if(_curr_box_name != NULL && strlen(_curr_box_name) < 10 && strcmp(_curr_box_name, "0x00")){
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
  CLASSNAME *dsp_instance;
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
  voice->dsp_instance->instanceInit(samplerate);
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
    safe_float_write(controller->control_port, scaled_value);
  }
}

static float get_effect_value(struct SoundPlugin *plugin, int effect_num, enum ValueFormat value_format){
  Data *data = (Data*)plugin->data;
  Voice *voice = &data->voices[0];
  MyUI::Controller *controller = &voice->myUI._controllers.at(effect_num);

  if(value_format==PLUGIN_FORMAT_SCALED){
#ifdef DONT_NORMALIZE_EFFECT_VALUES
    return safe_float_read(controller->control_port);
#else
    float min = controller->min_value;
    float max = controller->max_value;
    return scale(safe_float_read(controller->control_port),min,max,0.0f,1.0f);
#endif
  }else{
    return safe_float_read(controller->control_port);
  }
}

static void get_display_value_string(struct SoundPlugin *plugin, int effect_num, char *buffer, int buffersize){
  Data *data = (Data*)plugin->data;
  Voice *voice = &data->voices[0];
  MyUI::Controller *controller = &voice->myUI._controllers.at(effect_num);

  if(controller->type==EFFECT_FORMAT_INT)
    snprintf(buffer,buffersize-1,"%d %s",(int)safe_float_read(controller->control_port), controller->unit);
  else
    snprintf(buffer,buffersize-1,"%.2f %s",safe_float_read(controller->control_port), controller->unit);
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

static SoundPluginType faust_type = {};  // c++ way of zero-initialization without getting missing-field-initializers warning.

void CREATE_NAME (void){
  static bool has_inited = false;

  if (has_inited==false) {
    
    fill_type(&faust_type);
  
    CLASSNAME::classInit(MIXER_get_sample_rate());
  
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

    has_inited = true;

  }
  
  PR_add_plugin_type(&faust_type);
}
