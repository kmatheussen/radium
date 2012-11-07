//-----------------------------------------------------
// name: "Voice Formant"
// author: "Romain Michon (rmichon@ccrma.stanford.edu)"
// copyright: "Romain Michon"
// version: "1.0"
//
// Code generated with Faust 0.9.55 (http://faust.grame.fr)
//-----------------------------------------------------
/* link with : "" */
#include <math.h>
#include <phonemes.h>
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
#define FAUSTCLASS Voice_Form_dsp
#endif

class Voice_Form_dsp : public dsp {
  private:
	class SIG0 {
	  private:
		int 	fSamplingFreq;
		int 	iRec11[2];
	  public:
		int getNumInputs() 	{ return 0; }
		int getNumOutputs() 	{ return 1; }
		void init(int samplingFreq) {
			fSamplingFreq = samplingFreq;
			for (int i=0; i<2; i++) iRec11[i] = 0;
		}
		void fill (int count, float output[]) {
			for (int i=0; i<count; i++) {
				iRec11[0] = (1 + iRec11[1]);
				output[i] = sinf((9.587379924285257e-05f * float((iRec11[0] - 1))));
				// post processing
				iRec11[1] = iRec11[0];
			}
		}
	};


	int 	iVec0[2];
	FAUSTFLOAT 	fslider0;
	float 	fRec1[2];
	FAUSTFLOAT 	fbutton0;
	int 	iRec2[2];
	FAUSTFLOAT 	fslider1;
	int 	iConst0;
	FAUSTFLOAT 	fslider2;
	float 	fRec3[2];
	int 	iRec4[2];
	FAUSTFLOAT 	fentry0;
	int 	iRec6[2];
	FAUSTFLOAT 	fslider3;
	FAUSTFLOAT 	fslider4;
	float 	fRec7[2];
	float 	fRec8[2];
	float 	fConst1;
	float 	fConst2;
	float 	fConst3;
	float 	fConst4;
	float 	fConst5;
	float 	fConst6;
	float 	fConst7;
	float 	fConst8;
	float 	fConst9;
	static float 	ftbl0[65536];
	FAUSTFLOAT 	fslider5;
	float 	fConst10;
	float 	fConst11;
	float 	fRec12[2];
	int 	iRec13[2];
	int 	iRec14[2];
	FAUSTFLOAT 	fslider6;
	FAUSTFLOAT 	fslider7;
	FAUSTFLOAT 	fslider8;
	float 	fRec15[2];
	FAUSTFLOAT 	fslider9;
	FAUSTFLOAT 	fentry1;
	float 	fRec16[2];
	float 	fVec1[2];
	float 	fVec2[2];
	float 	fConst12;
	float 	fRec10[3];
	float 	fConst13;
	float 	fConst14;
	float 	fConst15;
	float 	fVec3[2];
	float 	fConst16;
	float 	fRec9[2];
	float 	fVec4[2];
	float 	fRec5[2];
	float 	fRec17[2];
	float 	fRec18[2];
	float 	fConst17;
	float 	fRec0[3];
	float 	fRec20[2];
	float 	fRec21[2];
	float 	fRec19[3];
	float 	fRec23[2];
	float 	fRec24[2];
	float 	fRec22[3];
	float 	fRec26[2];
	float 	fRec27[2];
	float 	fRec25[3];
	int 	IOTA;
	float 	fVec5[4096];
	FAUSTFLOAT 	fslider10;
	FAUSTFLOAT 	fslider11;
	float 	fConst18;
  public:
	static void metadata(Meta* m) 	{ 
		m->declare("name", "Voice Formant");
		m->declare("description", "Voice Formant Instrument");
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
		m->declare("oscillator.lib/name", "Faust Oscillator Library");
		m->declare("oscillator.lib/author", "Julius O. Smith (jos at ccrma.stanford.edu)");
		m->declare("oscillator.lib/copyright", "Julius O. Smith III");
		m->declare("oscillator.lib/version", "1.11");
		m->declare("oscillator.lib/license", "STK-4.3");
		m->declare("filter.lib/name", "Faust Filter Library");
		m->declare("filter.lib/author", "Julius O. Smith (jos at ccrma.stanford.edu)");
		m->declare("filter.lib/copyright", "Julius O. Smith III");
		m->declare("filter.lib/version", "1.29");
		m->declare("filter.lib/license", "STK-4.3");
		m->declare("filter.lib/reference", "https://ccrma.stanford.edu/~jos/filters/");
		m->declare("instrument.lib/name", "Faust-STK Tools Library");
		m->declare("instrument.lib/author", "Romain Michon (rmichon@ccrma.stanford.edu)");
		m->declare("instrument.lib/copyright", "Romain Michon");
		m->declare("instrument.lib/version", "1.0");
		m->declare("instrument.lib/licence", "STK-4.3");
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
		for (int i=0; i<2; i++) iVec0[i] = 0;
		fslider0 = 4.0f;
		for (int i=0; i<2; i++) fRec1[i] = 0;
		fbutton0 = 0.0;
		for (int i=0; i<2; i++) iRec2[i] = 0;
		fslider1 = 0.001f;
		iConst0 = min(192000, max(1, fSamplingFreq));
		fslider2 = 0.001f;
		for (int i=0; i<2; i++) fRec3[i] = 0;
		for (int i=0; i<2; i++) iRec4[i] = 0;
		fentry0 = 1.0f;
		for (int i=0; i<2; i++) iRec6[i] = 0;
		fslider3 = 0.01f;
		fslider4 = 0.01f;
		for (int i=0; i<2; i++) fRec7[i] = 0;
		for (int i=0; i<2; i++) fRec8[i] = 0;
		fConst1 = tanf((10367.255756846318f / float(iConst0)));
		fConst2 = (1.0f / fConst1);
		fConst3 = (0.822445908998816f + fConst2);
		fConst4 = (0 - ((0.822445908998816f - fConst2) / fConst3));
		fConst5 = faustpower<2>(fConst1);
		fConst6 = (2 * (1.412270893774204f - (1.0f / fConst5)));
		fConst7 = (1.412270893774204f + ((fConst2 - 0.80263676416103f) / fConst1));
		fConst8 = (1.412270893774204f + ((0.80263676416103f + fConst2) / fConst1));
		fConst9 = (1.0f / fConst8);
		fslider5 = 6.0f;
		fConst10 = float(iConst0);
		fConst11 = (1.0f / fConst10);
		for (int i=0; i<2; i++) fRec12[i] = 0;
		for (int i=0; i<2; i++) iRec13[i] = 0;
		for (int i=0; i<2; i++) iRec14[i] = 0;
		fslider6 = 0.1f;
		fslider7 = 0.05f;
		fslider8 = 0.5f;
		for (int i=0; i<2; i++) fRec15[i] = 0;
		fslider9 = 0.05f;
		fentry1 = 4.4e+02f;
		for (int i=0; i<2; i++) fRec16[i] = 0;
		for (int i=0; i<2; i++) fVec1[i] = 0;
		for (int i=0; i<2; i++) fVec2[i] = 0;
		fConst12 = (0.5f * fConst10);
		for (int i=0; i<3; i++) fRec10[i] = 0;
		fConst13 = (0.019809144837789f / fConst5);
		fConst14 = (1.161516418982696f + fConst13);
		fConst15 = (2 * (1.161516418982696f - fConst13));
		for (int i=0; i<2; i++) fVec3[i] = 0;
		fConst16 = (1.0f / (fConst3 * fConst8));
		for (int i=0; i<2; i++) fRec9[i] = 0;
		for (int i=0; i<2; i++) fVec4[i] = 0;
		for (int i=0; i<2; i++) fRec5[i] = 0;
		for (int i=0; i<2; i++) fRec17[i] = 0;
		for (int i=0; i<2; i++) fRec18[i] = 0;
		fConst17 = (6.283185307179586f / float(iConst0));
		for (int i=0; i<3; i++) fRec0[i] = 0;
		for (int i=0; i<2; i++) fRec20[i] = 0;
		for (int i=0; i<2; i++) fRec21[i] = 0;
		for (int i=0; i<3; i++) fRec19[i] = 0;
		for (int i=0; i<2; i++) fRec23[i] = 0;
		for (int i=0; i<2; i++) fRec24[i] = 0;
		for (int i=0; i<3; i++) fRec22[i] = 0;
		for (int i=0; i<2; i++) fRec26[i] = 0;
		for (int i=0; i<2; i++) fRec27[i] = 0;
		for (int i=0; i<3; i++) fRec25[i] = 0;
		IOTA = 0;
		for (int i=0; i<4096; i++) fVec5[i] = 0;
		fslider10 = 0.6f;
		fslider11 = 0.5f;
		fConst18 = (0.5f * iConst0);
	}
	virtual void init(int samplingFreq) {
		classInit(samplingFreq);
		instanceInit(samplingFreq);
	}
	virtual void buildUserInterface(UI* interface) {
		interface->openVerticalBox("voiceForm");
		interface->openHorizontalBox("Basic_Parameters");
		interface->declare(&fentry1, "1", "");
		interface->declare(&fentry1, "tooltip", "Tone frequency");
		interface->declare(&fentry1, "unit", "Hz");
		interface->addNumEntry("freq", &fentry1, 4.4e+02f, 2e+01f, 2e+04f, 1.0f);
		interface->declare(&fentry0, "1", "");
		interface->declare(&fentry0, "tooltip", "Gain (value between 0 and 1)");
		interface->addNumEntry("gain", &fentry0, 1.0f, 0.0f, 1.0f, 0.01f);
		interface->declare(&fbutton0, "1", "");
		interface->declare(&fbutton0, "tooltip", "noteOn = 1, noteOff = 0");
		interface->addButton("gate", &fbutton0);
		interface->closeBox();
		interface->openHorizontalBox("Envelopes_and_Vibrato");
		interface->openVerticalBox("Envelope_Parameters");
		interface->declare(&fslider2, "4", "");
		interface->declare(&fslider2, "tooltip", "Noised sounds attack duration");
		interface->declare(&fslider2, "unit", "s");
		interface->addHorizontalSlider("Noised_Attack", &fslider2, 0.001f, 0.0f, 2.0f, 0.001f);
		interface->declare(&fslider1, "4", "");
		interface->declare(&fslider1, "tooltip", "Noised sounds release duration");
		interface->declare(&fslider1, "unit", "s");
		interface->addHorizontalSlider("Noised_Release", &fslider1, 0.001f, 0.0f, 2.0f, 0.001f);
		interface->declare(&fslider4, "4", "");
		interface->declare(&fslider4, "tooltip", "Voiced sounds attack duration");
		interface->declare(&fslider4, "unit", "s");
		interface->addHorizontalSlider("Voiced_Attack", &fslider4, 0.01f, 0.0f, 2.0f, 0.01f);
		interface->declare(&fslider3, "4", "");
		interface->declare(&fslider3, "tooltip", "Voiced sounds release duration");
		interface->declare(&fslider3, "unit", "s");
		interface->addHorizontalSlider("Voiced_Release", &fslider3, 0.01f, 0.0f, 2.0f, 0.01f);
		interface->closeBox();
		interface->openVerticalBox("Vibrato_Parameters");
		interface->declare(&fslider8, "3", "");
		interface->declare(&fslider8, "tooltip", "Vibrato attack duration");
		interface->declare(&fslider8, "unit", "s");
		interface->addHorizontalSlider("Vibrato_Attack", &fslider8, 0.5f, 0.0f, 2.0f, 0.01f);
		interface->declare(&fslider7, "3", "");
		interface->declare(&fslider7, "tooltip", "Vibrato silence duration before attack");
		interface->declare(&fslider7, "unit", "s");
		interface->addHorizontalSlider("Vibrato_Begin", &fslider7, 0.05f, 0.0f, 2.0f, 0.01f);
		interface->declare(&fslider5, "3", "");
		interface->declare(&fslider5, "unit", "Hz");
		interface->addHorizontalSlider("Vibrato_Freq", &fslider5, 6.0f, 1.0f, 15.0f, 0.1f);
		interface->declare(&fslider9, "3", "");
		interface->declare(&fslider9, "tooltip", "A value between 0 and 1");
		interface->addHorizontalSlider("Vibrato_Gain", &fslider9, 0.05f, 0.0f, 1.0f, 0.01f);
		interface->declare(&fslider6, "3", "");
		interface->declare(&fslider6, "tooltip", "Vibrato release duration");
		interface->declare(&fslider6, "unit", "s");
		interface->addHorizontalSlider("Vibrato_Release", &fslider6, 0.1f, 0.0f, 2.0f, 0.01f);
		interface->closeBox();
		interface->closeBox();
		interface->openVerticalBox("Physical_Parameters");
		interface->declare(&fslider0, "2", "");
		interface->declare(&fslider0, "tooltip", "0->eee, 1->ihh, 2->ehh, 3->aaa, 4->ahh, 5->aww, 6->ohh, 7->uhh, 8->uuu, 9->ooo, 10->rrr, 11->lll, 12->mmm, 13->nnn, 14->nng, 15->ngg, 16->fff, 17->sss, 18->thh, 19->shh, 20->xxx, 21->hee, 22->hoo, 23->hah, 24->bbb, 25->ddd, 26->jjj, 27->ggg, 28->vvv, 29->zzz, 30->thz, 31->zhh");
		interface->addHorizontalSlider("Phoneme", &fslider0, 4.0f, 0.0f, 31.0f, 1.0f);
		interface->closeBox();
		interface->openVerticalBox("Spat");
		interface->addHorizontalSlider("pan angle", &fslider10, 0.6f, 0.0f, 1.0f, 0.01f);
		interface->addHorizontalSlider("spatial width", &fslider11, 0.5f, 0.0f, 1.0f, 0.01f);
		interface->closeBox();
		interface->closeBox();
	}
	virtual void compute (int count, FAUSTFLOAT** input, FAUSTFLOAT** output) {
		float 	fSlow0 = fslider0;
		float 	fSlow1 = (0.0010000000000000009f * loadPhonemeGains(fSlow0, 1));
		float 	fSlow2 = fbutton0;
		int 	iSlow3 = (fSlow2 > 0);
		int 	iSlow4 = (fSlow2 <= 0);
		float 	fSlow5 = fslider1;
		float 	fSlow6 = (1 - (1.0f / powf(1e+05f,(1.0f / ((fSlow5 == 0.0f) + (iConst0 * fSlow5))))));
		float 	fSlow7 = fslider2;
		float 	fSlow8 = (1.0f / ((fSlow7 == 0.0f) + (iConst0 * fSlow7)));
		float 	fSlow9 = (0.2f * fentry0);
		float 	fSlow10 = (fSlow9 - 0.97f);
		float 	fSlow11 = fslider3;
		float 	fSlow12 = (1 - (1.0f / powf(1e+05f,(1.0f / ((fSlow11 == 0.0f) + (iConst0 * fSlow11))))));
		float 	fSlow13 = fslider4;
		float 	fSlow14 = (1.0f / ((fSlow13 == 0.0f) + (iConst0 * fSlow13)));
		float 	fSlow15 = (0.0010000000000000009f * loadPhonemeGains(fSlow0, 0));
		float 	fSlow16 = (fConst11 * fslider5);
		float 	fSlow17 = fslider6;
		float 	fSlow18 = (1 - (1.0f / powf(1e+05f,(1.0f / ((fSlow17 == 0.0f) + (iConst0 * fSlow17))))));
		float 	fSlow19 = fslider7;
		float 	fSlow20 = (iConst0 * fSlow19);
		float 	fSlow21 = ((fSlow19 == 0.0f) + fSlow20);
		float 	fSlow22 = fslider8;
		float 	fSlow23 = (1.0f / ((fSlow22 == 0.0f) + (iConst0 * fSlow22)));
		float 	fSlow24 = (100 * fslider9);
		float 	fSlow25 = fentry1;
		float 	fSlow26 = (fSlow9 + 0.030000000000000027f);
		float 	fSlow27 = (0.0010000000000000009f * powf(10,(0.05f * loadPhonemeParameters(fSlow0, 2, 2))));
		float 	fSlow28 = (0.0010000000000000009f * loadPhonemeParameters(fSlow0, 2, 0));
		float 	fSlow29 = loadPhonemeParameters(fSlow0, 2, 1);
		float 	fSlow30 = (0 - (2 * fSlow29));
		float 	fSlow31 = faustpower<2>(fSlow29);
		float 	fSlow32 = (0.5f * fSlow31);
		float 	fSlow33 = (fSlow32 - 0.5f);
		float 	fSlow34 = (0.0010000000000000009f * powf(10,(0.05f * loadPhonemeParameters(fSlow0, 1, 2))));
		float 	fSlow35 = (0.0010000000000000009f * loadPhonemeParameters(fSlow0, 1, 0));
		float 	fSlow36 = loadPhonemeParameters(fSlow0, 1, 1);
		float 	fSlow37 = (0 - (2 * fSlow36));
		float 	fSlow38 = faustpower<2>(fSlow36);
		float 	fSlow39 = (0.5f * fSlow38);
		float 	fSlow40 = (fSlow39 - 0.5f);
		float 	fSlow41 = (0.0010000000000000009f * powf(10,(0.05f * loadPhonemeParameters(fSlow0, 3, 2))));
		float 	fSlow42 = (0.0010000000000000009f * loadPhonemeParameters(fSlow0, 3, 0));
		float 	fSlow43 = loadPhonemeParameters(fSlow0, 3, 1);
		float 	fSlow44 = (0 - (2 * fSlow43));
		float 	fSlow45 = faustpower<2>(fSlow43);
		float 	fSlow46 = (0.5f * fSlow45);
		float 	fSlow47 = (0.5f - fSlow46);
		float 	fSlow48 = (fSlow46 - 0.5f);
		float 	fSlow49 = (0.5f - fSlow39);
		float 	fSlow50 = (0.5f - fSlow32);
		float 	fSlow51 = (0.0010000000000000009f * powf(10,(0.05f * loadPhonemeParameters(fSlow0, 0, 2))));
		float 	fSlow52 = (0.0010000000000000009f * loadPhonemeParameters(fSlow0, 0, 0));
		float 	fSlow53 = loadPhonemeParameters(fSlow0, 0, 1);
		float 	fSlow54 = (0 - (2 * fSlow53));
		float 	fSlow55 = faustpower<2>(fSlow53);
		float 	fSlow56 = (0.5f * fSlow55);
		float 	fSlow57 = (fSlow56 - 0.5f);
		float 	fSlow58 = (0.5f - fSlow56);
		float 	fSlow59 = fslider10;
		float 	fSlow60 = (1.0f - fSlow59);
		int 	iSlow61 = int((int((fConst18 * (fslider11 / fSlow25))) & 4095));
		FAUSTFLOAT* output0 = output[0];
		FAUSTFLOAT* output1 = output[1];
		for (int i=0; i<count; i++) {
			iVec0[0] = 1;
			fRec1[0] = (fSlow1 + (0.999f * fRec1[1]));
			iRec2[0] = (iSlow3 & (iRec2[1] | (fRec3[1] >= 1)));
			int iTemp0 = (iSlow4 & (fRec3[1] > 0));
			fRec3[0] = (((iTemp0 == 0) | (fRec3[1] >= 1e-06f)) * ((fSlow8 * (((iRec2[1] == 0) & iSlow3) & (fRec3[1] < 1))) + (fRec3[1] * (1 - (fSlow6 * iTemp0)))));
			iRec4[0] = (12345 + (1103515245 * iRec4[1]));
			iRec6[0] = (iSlow3 & (iRec6[1] | (fRec7[1] >= 1)));
			int iTemp1 = (iSlow4 & (fRec7[1] > 0));
			fRec7[0] = (((iTemp1 == 0) | (fRec7[1] >= 1e-06f)) * ((fSlow14 * (((iRec6[1] == 0) & iSlow3) & (fRec7[1] < 1))) + (fRec7[1] * (1 - (fSlow12 * iTemp1)))));
			fRec8[0] = (fSlow15 + (0.999f * fRec8[1]));
			float fTemp2 = (fSlow16 + fRec12[1]);
			fRec12[0] = (fTemp2 - floorf(fTemp2));
			iRec13[0] = (iSlow3 & (iRec13[1] | (fRec15[1] >= 1)));
			iRec14[0] = (iSlow3 * (1 + iRec14[1]));
			int iTemp3 = (iSlow4 & (fRec15[1] > 0));
			fRec15[0] = (((iTemp3 == 0) | (fRec15[1] >= 1e-06f)) * ((fSlow23 * ((1 - (iRec14[1] < fSlow21)) * ((((iRec13[1] == 0) & iSlow3) & (fRec15[1] < 1)) & (iRec14[1] > fSlow20)))) + (fRec15[1] * (1 - (fSlow18 * iTemp3)))));
			float fTemp4 = float((fSlow25 + (fSlow24 * (fRec15[0] * ftbl0[int((65536.0f * fRec12[0]))]))));
			fRec16[0] = fmodf(((fConst11 * fTemp4) + fRec16[1]),1);
			float fTemp5 = faustpower<2>(((2 * fRec16[0]) - 1));
			fVec1[0] = fTemp5;
			float fTemp6 = ((iVec0[1] * (fVec1[0] - fVec1[1])) / fTemp4);
			fVec2[0] = fTemp6;
			fRec10[0] = ((1 + (fConst12 * ((0.25f * fVec2[1]) - (0.25f * fVec2[0])))) - (iVec0[1] + (fConst9 * ((fConst7 * fRec10[2]) + (fConst6 * fRec10[1])))));
			float fTemp7 = (((fConst14 * fRec10[0]) + (fConst15 * fRec10[1])) + (fConst14 * fRec10[2]));
			fVec3[0] = fTemp7;
			fRec9[0] = ((fConst16 * (fVec3[0] + fVec3[1])) + (fConst4 * fRec9[1]));
			float fTemp8 = ((fRec9[0] * fRec8[0]) * fRec7[0]);
			fVec4[0] = fTemp8;
			fRec5[0] = ((fSlow26 * ((0.5263157894736842f * fVec4[0]) + (0.47368421052631576f * fVec4[1]))) - (fSlow10 * fRec5[1]));
			float fTemp9 = (fRec5[0] + (4.656612875245797e-10f * ((iRec4[0] * fRec3[0]) * fRec1[0])));
			fRec17[0] = (fSlow27 + (0.999f * fRec17[1]));
			fRec18[0] = (fSlow28 + (0.999f * fRec18[1]));
			fRec0[0] = (0 - (((fSlow31 * fRec0[2]) + (fSlow30 * (fRec0[1] * cosf((fConst17 * fRec18[0]))))) - (fRec17[0] * fTemp9)));
			fRec20[0] = (fSlow34 + (0.999f * fRec20[1]));
			fRec21[0] = (fSlow35 + (0.999f * fRec21[1]));
			fRec19[0] = (0 - (((fSlow38 * fRec19[2]) + (fSlow37 * (fRec19[1] * cosf((fConst17 * fRec21[0]))))) - (fRec20[0] * fTemp9)));
			fRec23[0] = (fSlow41 + (0.999f * fRec23[1]));
			fRec24[0] = (fSlow42 + (0.999f * fRec24[1]));
			fRec22[0] = (0 - (((fSlow45 * fRec22[2]) + (fSlow44 * (fRec22[1] * cosf((fConst17 * fRec24[0]))))) - (fRec23[0] * fTemp9)));
			fRec26[0] = (fSlow51 + (0.999f * fRec26[1]));
			fRec27[0] = (fSlow52 + (0.999f * fRec27[1]));
			fRec25[0] = (0 - (((fSlow55 * fRec25[2]) + (fSlow54 * (fRec25[1] * cosf((fConst17 * fRec27[0]))))) - (fRec26[0] * fTemp9)));
			float fTemp10 = ((fSlow58 * fRec25[0]) + ((fSlow57 * fRec25[2]) + ((fSlow50 * fRec0[0]) + (((fSlow49 * fRec19[0]) + (((fSlow48 * fRec22[2]) + (fSlow47 * fRec22[0])) + (fSlow40 * fRec19[2]))) + (fSlow33 * fRec0[2])))));
			fVec5[IOTA&4095] = fTemp10;
			output0[i] = (FAUSTFLOAT)(fSlow60 * fVec5[IOTA&4095]);
			output1[i] = (FAUSTFLOAT)(fSlow59 * fVec5[(IOTA-iSlow61)&4095]);
			// post processing
			IOTA = IOTA+1;
			fRec25[2] = fRec25[1]; fRec25[1] = fRec25[0];
			fRec27[1] = fRec27[0];
			fRec26[1] = fRec26[0];
			fRec22[2] = fRec22[1]; fRec22[1] = fRec22[0];
			fRec24[1] = fRec24[0];
			fRec23[1] = fRec23[0];
			fRec19[2] = fRec19[1]; fRec19[1] = fRec19[0];
			fRec21[1] = fRec21[0];
			fRec20[1] = fRec20[0];
			fRec0[2] = fRec0[1]; fRec0[1] = fRec0[0];
			fRec18[1] = fRec18[0];
			fRec17[1] = fRec17[0];
			fRec5[1] = fRec5[0];
			fVec4[1] = fVec4[0];
			fRec9[1] = fRec9[0];
			fVec3[1] = fVec3[0];
			fRec10[2] = fRec10[1]; fRec10[1] = fRec10[0];
			fVec2[1] = fVec2[0];
			fVec1[1] = fVec1[0];
			fRec16[1] = fRec16[0];
			fRec15[1] = fRec15[0];
			iRec14[1] = iRec14[0];
			iRec13[1] = iRec13[0];
			fRec12[1] = fRec12[0];
			fRec8[1] = fRec8[0];
			fRec7[1] = fRec7[0];
			iRec6[1] = iRec6[0];
			iRec4[1] = iRec4[0];
			fRec3[1] = fRec3[0];
			iRec2[1] = iRec2[0];
			fRec1[1] = fRec1[0];
			iVec0[1] = iVec0[0];
		}
	}
};


float 	Voice_Form_dsp::ftbl0[65536];



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
