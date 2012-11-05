//-----------------------------------------------------
// name: "Commuted Piano"
// author: "Romain Michon (rmichon@ccrma.stanford.edu)"
// copyright: "Romain Michon"
// version: "1.0"
//
// Code generated with Faust 0.9.55 (http://faust.grame.fr)
//-----------------------------------------------------
/* link with : "" */
#include <math.h>
#include <piano.h>
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

#include "/home/kjetil/faudiostream/architecture/faust/audio/dsp.h"
#include "/home/kjetil/faudiostream/architecture/faust/gui/UI.h"

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
#define FAUSTCLASS Piano_dsp
#endif

class Piano_dsp : public dsp {
  private:
	FAUSTFLOAT 	fentry0;
	int 	iConst0;
	float 	fConst1;
	FAUSTFLOAT 	fslider0;
	FAUSTFLOAT 	fslider1;
	float 	fConst2;
	float 	fConst3;
	FAUSTFLOAT 	fbutton0;
	float 	fRec11[2];
	float 	fConst4;
	float 	fConst5;
	float 	fConst6;
	float 	fRec10[2];
	FAUSTFLOAT 	fentry1;
	float 	fConst7;
	float 	fRec12[2];
	int 	iRec13[2];
	float 	fVec0[2];
	float 	fVec1[2];
	float 	fRec9[2];
	float 	fRec8[2];
	float 	fRec7[2];
	float 	fRec6[2];
	float 	fRec5[2];
	float 	fConst8;
	float 	fRec4[3];
	float 	fRec3[3];
	float 	fRec2[3];
	float 	fRec1[3];
	float 	fRec0[2];
	float 	fRec24[2];
	float 	fRec23[2];
	float 	fRec22[2];
	float 	fRec21[2];
	float 	fRec20[2];
	float 	fRec25[2];
	float 	fVec2[2];
	FAUSTFLOAT 	fslider2;
	float 	fRec19[2];
	float 	fRec18[2];
	int 	IOTA;
	float 	fRec17[4096];
	FAUSTFLOAT 	fslider3;
	float 	fConst9;
	float 	fVec3[2];
	float 	fRec29[2];
	float 	fRec28[2];
	float 	fRec27[4096];
	float 	fVec4[2];
	float 	fRec26[2];
	float 	fRec14[2];
	float 	fRec15[2];
	float 	fConst10;
	int 	iConst11;
	float 	fConst12;
	float 	fRec30[3];
	float 	fConst13;
	float 	fVec5[4096];
	FAUSTFLOAT 	fslider4;
	FAUSTFLOAT 	fslider5;
	float 	fConst14;
  public:
	static void metadata(Meta* m) 	{ 
		m->declare("name", "Commuted Piano");
		m->declare("description", "WaveGuide Commuted Piano");
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
	}
	virtual void instanceInit(int samplingFreq) {
		fSamplingFreq = samplingFreq;
		fentry0 = 4.4e+02f;
		iConst0 = min(192000, max(1, fSamplingFreq));
		fConst1 = (0.05f / float(iConst0));
		fslider0 = 0.0f;
		fslider1 = 0.1f;
		fConst2 = float(iConst0);
		fConst3 = (0.1f * fConst2);
		fbutton0 = 0.0;
		for (int i=0; i<2; i++) fRec11[i] = 0;
		fConst4 = (1e+01f / fConst2);
		fConst5 = expf((0 - (0.5f / fConst2)));
		fConst6 = expf((0 - (5.0f / fConst2)));
		for (int i=0; i<2; i++) fRec10[i] = 0;
		fentry1 = 1.0f;
		fConst7 = (float(7) / fConst2);
		for (int i=0; i<2; i++) fRec12[i] = 0;
		for (int i=0; i<2; i++) iRec13[i] = 0;
		for (int i=0; i<2; i++) fVec0[i] = 0;
		for (int i=0; i<2; i++) fVec1[i] = 0;
		for (int i=0; i<2; i++) fRec9[i] = 0;
		for (int i=0; i<2; i++) fRec8[i] = 0;
		for (int i=0; i<2; i++) fRec7[i] = 0;
		for (int i=0; i<2; i++) fRec6[i] = 0;
		for (int i=0; i<2; i++) fRec5[i] = 0;
		fConst8 = (6.283185307179586f / float(iConst0));
		for (int i=0; i<3; i++) fRec4[i] = 0;
		for (int i=0; i<3; i++) fRec3[i] = 0;
		for (int i=0; i<3; i++) fRec2[i] = 0;
		for (int i=0; i<3; i++) fRec1[i] = 0;
		for (int i=0; i<2; i++) fRec0[i] = 0;
		for (int i=0; i<2; i++) fRec24[i] = 0;
		for (int i=0; i<2; i++) fRec23[i] = 0;
		for (int i=0; i<2; i++) fRec22[i] = 0;
		for (int i=0; i<2; i++) fRec21[i] = 0;
		for (int i=0; i<2; i++) fRec20[i] = 0;
		for (int i=0; i<2; i++) fRec25[i] = 0;
		for (int i=0; i<2; i++) fVec2[i] = 0;
		fslider2 = 0.28f;
		for (int i=0; i<2; i++) fRec19[i] = 0;
		for (int i=0; i<2; i++) fRec18[i] = 0;
		IOTA = 0;
		for (int i=0; i<4096; i++) fRec17[i] = 0;
		fslider3 = 0.1f;
		fConst9 = (0.15915494309189535f * iConst0);
		for (int i=0; i<2; i++) fVec3[i] = 0;
		for (int i=0; i<2; i++) fRec29[i] = 0;
		for (int i=0; i<2; i++) fRec28[i] = 0;
		for (int i=0; i<4096; i++) fRec27[i] = 0;
		for (int i=0; i<2; i++) fVec4[i] = 0;
		for (int i=0; i<2; i++) fRec26[i] = 0;
		for (int i=0; i<2; i++) fRec14[i] = 0;
		for (int i=0; i<2; i++) fRec15[i] = 0;
		fConst10 = (2.0f / float(iConst0));
		iConst11 = faustpower<2>(iConst0);
		fConst12 = (1.0f / float(iConst11));
		for (int i=0; i<3; i++) fRec30[i] = 0;
		fConst13 = (0.5f / float(iConst11));
		for (int i=0; i<4096; i++) fVec5[i] = 0;
		fslider4 = 0.6f;
		fslider5 = 0.5f;
		fConst14 = (0.5f * iConst0);
	}
	virtual void init(int samplingFreq) {
		classInit(samplingFreq);
		instanceInit(samplingFreq);
	}
	virtual void buildUserInterface(UI* interface) {
		interface->openVerticalBox("piano");
		interface->openHorizontalBox("Basic_Parameters");
		interface->declare(&fentry0, "1", "");
		interface->declare(&fentry0, "tooltip", "Tone frequency");
		interface->declare(&fentry0, "unit", "Hz");
		interface->addNumEntry("freq", &fentry0, 4.4e+02f, 2e+01f, 2e+04f, 1.0f);
		interface->declare(&fentry1, "1", "");
		interface->declare(&fentry1, "tooltip", "Gain (value between 0 and 1)");
		interface->addNumEntry("gain", &fentry1, 1.0f, 0.0f, 1.0f, 0.01f);
		interface->declare(&fbutton0, "1", "");
		interface->declare(&fbutton0, "tooltip", "noteOn = 1, noteOff = 0");
		interface->addButton("gate", &fbutton0);
		interface->closeBox();
		interface->openVerticalBox("Physical_Parameters");
		interface->declare(&fslider0, "2", "");
		interface->declare(&fslider0, "tooltip", "A value between 0 and 1");
		interface->addHorizontalSlider("Brightness_Factor", &fslider0, 0.0f, 0.0f, 1.0f, 0.01f);
		interface->declare(&fslider3, "2", "");
		interface->declare(&fslider3, "tooltip", "A value between 0 and 1");
		interface->addHorizontalSlider("Detuning_Factor", &fslider3, 0.1f, 0.0f, 1.0f, 0.01f);
		interface->declare(&fslider1, "2", "");
		interface->declare(&fslider1, "tooltip", "A value between 0 and 1");
		interface->addHorizontalSlider("Hammer_Hardness", &fslider1, 0.1f, 0.0f, 1.0f, 0.01f);
		interface->declare(&fslider2, "2", "");
		interface->declare(&fslider2, "tooltip", "A value between 0 and 1");
		interface->addHorizontalSlider("Stiffness_Factor", &fslider2, 0.28f, 0.0f, 1.0f, 0.01f);
		interface->closeBox();
		interface->openVerticalBox("Spat");
		interface->addHorizontalSlider("pan angle", &fslider4, 0.6f, 0.0f, 1.0f, 0.01f);
		interface->addHorizontalSlider("spatial width", &fslider5, 0.5f, 0.0f, 1.0f, 0.01f);
		interface->closeBox();
		interface->closeBox();
	}
	virtual void compute (int count, FAUSTFLOAT** input, FAUSTFLOAT** output) {
		float 	fSlow0 = fentry0;
		int 	iSlow1 = int(((17.31234049066756f * (logf(fSlow0) - 6.0867747269123065f)) + 69.5f));
		float 	fSlow2 = getValueDCBa1(iSlow1);
		float 	fSlow3 = powf(10,(fConst1 * getValuer1_2db(iSlow1)));
		float 	fSlow4 = faustpower<2>(fSlow3);
		float 	fSlow5 = (0 - (2 * fSlow3));
		float 	fSlow6 = getValueLoudPole(iSlow1);
		float 	fSlow7 = (0.25f * fslider0);
		float 	fSlow8 = (fSlow7 - (0.02f + fSlow6));
		float 	fSlow9 = fslider1;
		float 	fSlow10 = (fConst3 * fSlow9);
		float 	fSlow11 = fbutton0;
		float 	fSlow12 = expf((0 - (fConst4 / fSlow9)));
		float 	fSlow13 = (0 - (fSlow11 - 1));
		float 	fSlow14 = (fConst6 * fSlow13);
		float 	fSlow15 = (fSlow14 - 1);
		int 	iSlow16 = (fSlow11 > 0);
		float 	fSlow17 = (0.2f * getValueSustainPedalLevel(iSlow1));
		int 	iSlow18 = (fSlow11 < 1);
		float 	fSlow19 = expf((0 - (fConst7 / (fentry1 * getValueDryTapAmpT60(iSlow1)))));
		int 	iSlow20 = (iSlow1 >= 88);
		float 	fSlow21 = (2.3283064376228985e-10f * iSlow20);
		float 	fSlow22 = (1.1641532188114492e-10f * iSlow20);
		float 	fSlow23 = (1 - fSlow2);
		float 	fSlow24 = (0.5f * fSlow23);
		float 	fSlow25 = (0 - fSlow24);
		float 	fSlow26 = (getValueLoudGain(iSlow1) * ((fSlow7 + 0.98f) - fSlow6));
		float 	fSlow27 = (2.0f * getValueBq4_gEarBalled(iSlow1));
		float 	fSlow28 = powf(10,(fConst1 * getValuer3db(iSlow1)));
		float 	fSlow29 = (cosf((fConst8 * (fSlow0 * getValueThirdPartialFactor(iSlow1)))) * (0 - (2 * fSlow28)));
		float 	fSlow30 = faustpower<2>(fSlow28);
		float 	fSlow31 = powf(10,(fConst1 * getValuer2db(iSlow1)));
		float 	fSlow32 = (cosf((fConst8 * (fSlow0 * getValueSecondPartialFactor(iSlow1)))) * (0 - (2 * fSlow31)));
		float 	fSlow33 = faustpower<2>(fSlow31);
		float 	fSlow34 = powf(10,(fConst1 * getValuer1_1db(iSlow1)));
		float 	fSlow35 = cosf((fConst8 * fSlow0));
		float 	fSlow36 = (fSlow35 * (0 - (2 * fSlow34)));
		float 	fSlow37 = faustpower<2>(fSlow34);
		float 	fSlow38 = powf(10,(0.05f * getValueSecondStageAmpRatio(iSlow1)));
		float 	fSlow39 = (1 - fSlow38);
		float 	fSlow40 = (0 - (2 * ((fSlow3 * fSlow39) + (fSlow38 * fSlow34))));
		float 	fSlow41 = ((fSlow4 * fSlow39) + (fSlow38 * fSlow37));
		float 	fSlow42 = (1.396983862573739e-09f * (fSlow26 * (iSlow1 < 88)));
		float 	fSlow43 = (0.0010000000000000009f * ((0.9f * (fSlow13 * getValueReleaseLoopGain(iSlow1))) + (0.9996f * fSlow11)));
		float 	fSlow44 = getValueStiffnessCoefficient(iSlow1);
		float 	fSlow45 = fslider2;
		float 	fSlow46 = (fSlow45 * fSlow44);
		float 	fSlow47 = (5.0f * (fslider3 * getValueDetuningHz(iSlow1)));
		float 	fSlow48 = (fSlow0 + fSlow47);
		float 	fSlow49 = getValueSingleStringPole(iSlow1);
		float 	fSlow50 = ((1 - fSlow49) * powf(10,(0.05f * (getValueSingleStringDecayRate(iSlow1) / fSlow0))));
		float 	fSlow51 = getValueSingleStringZero(iSlow1);
		float 	fSlow52 = (1 - fSlow51);
		float 	fSlow53 = ((3 * fSlow52) - fSlow50);
		float 	fSlow54 = (fSlow49 * fSlow52);
		float 	fSlow55 = (3 * fSlow54);
		float 	fSlow56 = (fSlow50 * fSlow51);
		float 	fSlow57 = (fSlow54 - fSlow56);
		float 	fSlow58 = (4 * fSlow57);
		float 	fSlow59 = ((fSlow56 + fSlow58) - fSlow55);
		float 	fSlow60 = (fConst8 * fSlow48);
		float 	fSlow61 = cosf(fSlow60);
		float 	fSlow62 = ((fSlow50 + fSlow51) - 1);
		float 	fSlow63 = (4 * fSlow62);
		float 	fSlow64 = (1 + ((fSlow63 + (fSlow61 * fSlow59)) / fSlow53));
		float 	fSlow65 = (fSlow56 - fSlow55);
		float 	fSlow66 = (0 - (fSlow59 / fSlow53));
		float 	fSlow67 = (1 + ((fSlow65 * fSlow61) / fSlow53));
		float 	fSlow68 = sinf(fSlow60);
		float 	fSlow69 = faustpower<2>(fSlow53);
		float 	fSlow70 = faustpower<2>(fSlow68);
		float 	fSlow71 = (13.690000000000001f * (faustpower<2>(fSlow45) * faustpower<2>(fSlow44)));
		float 	fSlow72 = (fSlow71 - 1.0f);
		float 	fSlow73 = (1.0f + fSlow71);
		float 	fSlow74 = (7.4f * fSlow46);
		float 	fSlow75 = (3 * atan2f((fSlow72 * fSlow68),(fSlow74 + (fSlow73 * fSlow61))));
		int 	iSlow76 = int((fConst9 * ((6.283185307179586f + (fSlow75 + atan2f((fSlow68 * ((fSlow67 * fSlow66) + ((fSlow65 * fSlow64) / fSlow53))),((fSlow67 * fSlow64) + (((fSlow65 * fSlow70) * fSlow59) / fSlow69))))) / fSlow48)));
		int 	iSlow77 = int((int((1 + iSlow76)) & 4095));
		float 	fSlow78 = (fSlow65 + fSlow58);
		float 	fSlow79 = (0 - (fSlow78 / fSlow53));
		float 	fSlow80 = (1 + ((fSlow63 + (fSlow78 * fSlow61)) / fSlow53));
		float 	fSlow81 = (fSlow65 * fSlow78);
		float 	fSlow82 = (fConst9 * ((6.283185307179586f + (atan2f((fSlow68 * (((fSlow65 * fSlow80) / fSlow53) + (fSlow79 * fSlow67))),(((fSlow81 * fSlow70) / fSlow69) + (fSlow67 * fSlow80))) + fSlow75)) / fSlow48));
		int 	iSlow83 = int(fSlow82);
		float 	fSlow84 = (fSlow82 - iSlow83);
		float 	fSlow85 = (fSlow0 - fSlow47);
		float 	fSlow86 = (fConst8 * fSlow85);
		float 	fSlow87 = cosf(fSlow86);
		float 	fSlow88 = (1 + ((fSlow63 + (fSlow87 * fSlow59)) / fSlow53));
		float 	fSlow89 = (1 + ((fSlow65 * fSlow87) / fSlow53));
		float 	fSlow90 = sinf(fSlow86);
		float 	fSlow91 = faustpower<2>(fSlow90);
		float 	fSlow92 = (3 * atan2f((fSlow90 * fSlow72),((fSlow87 * fSlow73) + fSlow74)));
		int 	iSlow93 = int((fConst9 * ((6.283185307179586f + (fSlow92 + atan2f((fSlow90 * ((fSlow89 * fSlow66) + ((fSlow65 * fSlow88) / fSlow53))),((fSlow89 * fSlow88) + (((fSlow65 * fSlow91) * fSlow59) / fSlow69))))) / fSlow85)));
		int 	iSlow94 = int((int((1 + iSlow93)) & 4095));
		float 	fSlow95 = (1 + (((fSlow78 * fSlow87) + fSlow63) / fSlow53));
		float 	fSlow96 = (fConst9 * ((6.283185307179586f + (atan2f((fSlow90 * (((fSlow65 * fSlow95) / fSlow53) + (fSlow89 * fSlow79))),(((fSlow81 * fSlow91) / fSlow69) + (fSlow89 * fSlow95))) + fSlow92)) / fSlow85));
		int 	iSlow97 = int(fSlow96);
		float 	fSlow98 = (fSlow96 - iSlow97);
		int 	iSlow99 = int((iSlow76 & 4095));
		float 	fSlow100 = ((1 + iSlow83) - fSlow82);
		int 	iSlow101 = int((iSlow93 & 4095));
		float 	fSlow102 = ((1 + iSlow97) - fSlow96);
		float 	fSlow103 = (1.0f / fSlow53);
		float 	fSlow104 = getValueEQBandWidthFactor(iSlow1);
		float 	fSlow105 = (cosf((fConst8 * (fSlow0 / getValueStrikePosition(iSlow1)))) * (0 - (fConst10 * (fSlow0 * fSlow104))));
		float 	fSlow106 = (faustpower<2>(fSlow0) * faustpower<2>(fSlow104));
		float 	fSlow107 = (fConst12 * fSlow106);
		float 	fSlow108 = getValueEQGain(iSlow1);
		float 	fSlow109 = (fConst13 * fSlow106);
		float 	fSlow110 = (fSlow109 - 0.5f);
		float 	fSlow111 = (0.5f - fSlow109);
		float 	fSlow112 = fslider4;
		float 	fSlow113 = (12 * (1.0f - fSlow112));
		int 	iSlow114 = int((int((fConst14 * (fslider5 / fSlow0))) & 4095));
		float 	fSlow115 = (12 * fSlow112);
		FAUSTFLOAT* output0 = output[0];
		FAUSTFLOAT* output1 = output[1];
		for (int i=0; i<count; i++) {
			fRec11[0] = (1 + (fSlow11 * fRec11[1]));
			float fTemp0 = (fRec11[0] - 1);
			int iTemp1 = (fTemp0 < fSlow10);
			float fTemp2 = (fSlow11 * ((fConst5 * (fTemp0 >= fSlow10)) + (fSlow12 * iTemp1)));
			fRec10[0] = ((fSlow17 * ((iTemp1 & iSlow16) * (0 - (fTemp2 + fSlow15)))) + ((fSlow14 + fTemp2) * fRec10[1]));
			int iTemp3 = ((fTemp0 < 2.0f) & iSlow16);
			float fTemp4 = ((fSlow19 * ((fTemp0 >= 2.0f) | iSlow18)) + (0.0301973834223185f * iTemp3));
			fRec12[0] = ((0.15f * (iTemp3 * (1 - fTemp4))) + (fTemp4 * fRec12[1]));
			iRec13[0] = (12345 + (1103515245 * iRec13[1]));
			float fTemp5 = (iRec13[0] * (fRec12[0] + fRec10[0]));
			fVec0[0] = (fSlow21 * fTemp5);
			float fTemp6 = (0 - ((fSlow22 * fTemp5) + (0.5f * fVec0[1])));
			fVec1[0] = fTemp6;
			fRec9[0] = (((fSlow25 * fVec1[1]) + (fSlow24 * fVec1[0])) - (fSlow2 * fRec9[1]));
			fRec8[0] = ((fSlow26 * fRec9[0]) - (fSlow8 * fRec8[1]));
			fRec7[0] = ((fSlow26 * fRec8[0]) - (fSlow8 * fRec7[1]));
			fRec6[0] = ((fSlow26 * fRec7[0]) - (fSlow8 * fRec6[1]));
			fRec5[0] = ((fSlow26 * fRec6[0]) - (fSlow8 * fRec5[1]));
			fRec4[0] = (0 - (((fSlow30 * fRec4[2]) + (fSlow29 * fRec4[1])) - (fSlow27 * ((0.5f * fRec5[0]) - (0.5f * fRec5[1])))));
			fRec3[0] = (0 - (((fSlow33 * fRec3[2]) + (fSlow32 * fRec3[1])) - (fSlow27 * fRec4[0])));
			fRec2[0] = (0 - (((fSlow37 * fRec2[2]) + (fSlow36 * fRec2[1])) - fRec3[0]));
			fRec1[0] = (((fRec2[0] + (fSlow41 * fRec2[2])) + (fSlow35 * ((fSlow40 * fRec2[1]) - (fSlow5 * fRec1[1])))) - (fSlow4 * fRec1[2]));
			fRec0[0] = ((fSlow23 * fRec1[0]) - (fSlow2 * fRec0[1]));
			fRec24[0] = ((fSlow42 * fTemp5) - (fSlow8 * fRec24[1]));
			fRec23[0] = ((fSlow26 * fRec24[0]) - (fSlow8 * fRec23[1]));
			fRec22[0] = ((fSlow26 * fRec23[0]) - (fSlow8 * fRec22[1]));
			fRec21[0] = ((fSlow26 * fRec22[0]) - (fSlow8 * fRec21[1]));
			fRec20[0] = (((fSlow25 * fRec21[1]) + (fSlow24 * fRec21[0])) - (fSlow2 * fRec20[1]));
			fRec25[0] = (fSlow43 + (0.999f * fRec25[1]));
			float fTemp7 = (fRec25[0] * (fRec20[0] + fRec14[1]));
			fVec2[0] = fTemp7;
			fRec19[0] = (fVec2[1] + (fSlow46 * ((3.7f * fVec2[0]) - (3.7f * fRec19[1]))));
			fRec18[0] = (fRec19[1] + (fSlow46 * ((3.7f * fRec19[0]) - (3.7f * fRec18[1]))));
			fRec17[IOTA&4095] = (fRec18[1] + (fSlow46 * ((3.7f * fRec18[0]) - (3.7f * fRec17[(IOTA-1)&4095]))));
			float fTemp8 = (fSlow84 * fRec17[(IOTA-iSlow77)&4095]);
			float fTemp9 = (fRec20[0] + (fRec25[0] * fRec15[1]));
			fVec3[0] = fTemp9;
			fRec29[0] = (fVec3[1] + (fSlow46 * ((3.7f * fVec3[0]) - (3.7f * fRec29[1]))));
			fRec28[0] = (fRec29[1] + (fSlow46 * ((3.7f * fRec29[0]) - (3.7f * fRec28[1]))));
			fRec27[IOTA&4095] = (fRec28[1] + (fSlow46 * ((3.7f * fRec28[0]) - (3.7f * fRec27[(IOTA-1)&4095]))));
			float fTemp10 = (fSlow98 * fRec27[(IOTA-iSlow94)&4095]);
			float fTemp11 = (fSlow100 * fRec17[(IOTA-iSlow99)&4095]);
			float fTemp12 = (fSlow102 * fRec27[(IOTA-iSlow101)&4095]);
			float fTemp13 = (fTemp12 + ((fTemp8 + fTemp11) + fTemp10));
			fVec4[0] = fTemp13;
			fRec26[0] = (fSlow103 * ((2 * ((fSlow57 * fVec4[1]) + (fSlow62 * fVec4[0]))) - (fSlow65 * fRec26[1])));
			fRec14[0] = (fTemp11 + (fRec26[0] + fTemp8));
			fRec15[0] = (fTemp12 + (fRec26[0] + fTemp10));
			float 	fRec16 = fVec4[0];
			fRec30[0] = ((fSlow108 * fRec16) - ((fSlow107 * fRec30[2]) + (fSlow105 * fRec30[1])));
			float fTemp14 = ((fSlow111 * fRec30[0]) + ((fSlow110 * fRec30[2]) + (fRec16 + fRec0[0])));
			fVec5[IOTA&4095] = fTemp14;
			output0[i] = (FAUSTFLOAT)(fSlow113 * fVec5[IOTA&4095]);
			output1[i] = (FAUSTFLOAT)(fSlow115 * fVec5[(IOTA-iSlow114)&4095]);
			// post processing
			fRec30[2] = fRec30[1]; fRec30[1] = fRec30[0];
			fRec15[1] = fRec15[0];
			fRec14[1] = fRec14[0];
			fRec26[1] = fRec26[0];
			fVec4[1] = fVec4[0];
			fRec28[1] = fRec28[0];
			fRec29[1] = fRec29[0];
			fVec3[1] = fVec3[0];
			IOTA = IOTA+1;
			fRec18[1] = fRec18[0];
			fRec19[1] = fRec19[0];
			fVec2[1] = fVec2[0];
			fRec25[1] = fRec25[0];
			fRec20[1] = fRec20[0];
			fRec21[1] = fRec21[0];
			fRec22[1] = fRec22[0];
			fRec23[1] = fRec23[0];
			fRec24[1] = fRec24[0];
			fRec0[1] = fRec0[0];
			fRec1[2] = fRec1[1]; fRec1[1] = fRec1[0];
			fRec2[2] = fRec2[1]; fRec2[1] = fRec2[0];
			fRec3[2] = fRec3[1]; fRec3[1] = fRec3[0];
			fRec4[2] = fRec4[1]; fRec4[1] = fRec4[0];
			fRec5[1] = fRec5[0];
			fRec6[1] = fRec6[0];
			fRec7[1] = fRec7[0];
			fRec8[1] = fRec8[0];
			fRec9[1] = fRec9[0];
			fVec1[1] = fVec1[0];
			fVec0[1] = fVec0[0];
			iRec13[1] = iRec13[0];
			fRec12[1] = fRec12[0];
			fRec10[1] = fRec10[0];
			fRec11[1] = fRec11[0];
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
