//-----------------------------------------------------
// name: "Bowed"
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

/*
#include "/usr/share/faust/audio/dsp.h"
#include "/usr/share/faust/gui/UI.h"
*/

// We use faust1 here.

struct Meta
{
    void declare (const char* key, const char* value) { }
};

#include "faudiostream/architecture/faust/audio/dsp.h"
#include "faudiostream/architecture/faust/gui/UI.h"


#include "Faust_plugins_template1.cpp"

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
#define FAUSTCLASS Bowed_dsp
#endif

class Bowed_dsp : public dsp {
  private:
	class SIG0 {
	  private:
		int 	fSamplingFreq;
		int 	iRec14[2];
	  public:
		int getNumInputs() 	{ return 0; }
		int getNumOutputs() 	{ return 1; }
		void init(int samplingFreq) {
			fSamplingFreq = samplingFreq;
			for (int i=0; i<2; i++) iRec14[i] = 0;
		}
		void fill (int count, float output[]) {
			for (int i=0; i<count; i++) {
				iRec14[0] = (1 + iRec14[1]);
				output[i] = sinf((9.587379924285257e-05f * float((iRec14[0] - 1))));
				// post processing
				iRec14[1] = iRec14[0];
			}
		}
	};


	int 	iConst0;
	float 	fConst1;
	float 	fConst2;
	float 	fConst3;
	FAUSTFLOAT 	fentry0;
	FAUSTFLOAT 	fslider0;
	FAUSTFLOAT 	fslider1;
	float 	fRec5[2];
	FAUSTFLOAT 	fentry1;
	FAUSTFLOAT 	fbutton0;
	int 	iRec6[2];
	FAUSTFLOAT 	fslider2;
	FAUSTFLOAT 	fslider3;
	float 	fRec7[2];
	float 	fRec13[2];
	float 	fRec12[2];
	float 	fRec11[2];
	float 	fRec10[2];
	float 	fRec9[2];
	float 	fRec8[2];
	static float 	ftbl0[65536];
	FAUSTFLOAT 	fslider4;
	float 	fRec16[2];
	float 	fConst4;
	float 	fRec15[2];
	float 	fRec22[2];
	float 	fRec21[2];
	float 	fRec20[2];
	float 	fRec19[2];
	float 	fRec18[2];
	float 	fRec17[2];
	float 	fConst5;
	float 	fRec4[2];
	int 	iRec23[2];
	FAUSTFLOAT 	fentry2;
	FAUSTFLOAT 	fslider5;
	FAUSTFLOAT 	fslider6;
	float 	fRec24[2];
	FAUSTFLOAT 	fslider7;
	float 	fRec25[2];
	int 	iRec26[2];
	int 	iRec27[2];
	FAUSTFLOAT 	fslider8;
	FAUSTFLOAT 	fslider9;
	FAUSTFLOAT 	fslider10;
	float 	fRec28[2];
	FAUSTFLOAT 	fslider11;
	FAUSTFLOAT 	fslider12;
	int 	IOTA;
	float 	fRec2[8192];
	float 	fRec1[8192];
	float 	fRec0[3];
	FAUSTFLOAT 	fslider13;
	float 	fVec0[4096];
	FAUSTFLOAT 	fslider14;
	float 	fConst6;
  public:
	static void metadata(Meta* m) 	{ 
		m->declare("name", "Bowed");
		m->declare("description", "Nonlinear WaveGuide Bowed Instrument");
		m->declare("author", "Romain Michon");
		m->declare("copyright", "Romain Michon (rmichon@ccrma.stanford.edu)");
		m->declare("version", "1.0");
		m->declare("licence", "STK-4.3");
		m->declare("reference", "https://ccrma.stanford.edu/~jos/pasp/Bowed_Strings.html");
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
		iConst0 = min(192000, max(1, fSamplingFreq));
		fConst1 = (0 - (1.7f * cosf((3141.592653589793f / float(iConst0)))));
		fConst2 = (2205.0f / float(iConst0));
		fConst3 = (fConst2 - 0.6f);
		fentry0 = 4.4e+02f;
		fslider0 = 0.7f;
		fslider1 = 0.0f;
		for (int i=0; i<2; i++) fRec5[i] = 0;
		fentry1 = 0.0f;
		fbutton0 = 0.0;
		for (int i=0; i<2; i++) iRec6[i] = 0;
		fslider2 = 0.1f;
		fslider3 = 0.1f;
		for (int i=0; i<2; i++) fRec7[i] = 0;
		for (int i=0; i<2; i++) fRec13[i] = 0;
		for (int i=0; i<2; i++) fRec12[i] = 0;
		for (int i=0; i<2; i++) fRec11[i] = 0;
		for (int i=0; i<2; i++) fRec10[i] = 0;
		for (int i=0; i<2; i++) fRec9[i] = 0;
		for (int i=0; i<2; i++) fRec8[i] = 0;
		fslider4 = 2.2e+02f;
		for (int i=0; i<2; i++) fRec16[i] = 0;
		fConst4 = (1.0f / float(iConst0));
		for (int i=0; i<2; i++) fRec15[i] = 0;
		for (int i=0; i<2; i++) fRec22[i] = 0;
		for (int i=0; i<2; i++) fRec21[i] = 0;
		for (int i=0; i<2; i++) fRec20[i] = 0;
		for (int i=0; i<2; i++) fRec19[i] = 0;
		for (int i=0; i<2; i++) fRec18[i] = 0;
		for (int i=0; i<2; i++) fRec17[i] = 0;
		fConst5 = (0.95f * (0.4f + fConst2));
		for (int i=0; i<2; i++) fRec4[i] = 0;
		for (int i=0; i<2; i++) iRec23[i] = 0;
		fentry2 = 1.0f;
		fslider5 = 0.05f;
		fslider6 = 0.01f;
		for (int i=0; i<2; i++) fRec24[i] = 0;
		fslider7 = 6.0f;
		for (int i=0; i<2; i++) fRec25[i] = 0;
		for (int i=0; i<2; i++) iRec26[i] = 0;
		for (int i=0; i<2; i++) iRec27[i] = 0;
		fslider8 = 0.01f;
		fslider9 = 0.05f;
		fslider10 = 0.5f;
		for (int i=0; i<2; i++) fRec28[i] = 0;
		fslider11 = 0.01f;
		fslider12 = 0.75f;
		IOTA = 0;
		for (int i=0; i<8192; i++) fRec2[i] = 0;
		for (int i=0; i<8192; i++) fRec1[i] = 0;
		for (int i=0; i<3; i++) fRec0[i] = 0;
		fslider13 = 0.6f;
		for (int i=0; i<4096; i++) fVec0[i] = 0;
		fslider14 = 0.5f;
		fConst6 = (0.5f * iConst0);
	}
	virtual void init(int samplingFreq) {
		classInit(samplingFreq);
		instanceInit(samplingFreq);
	}
	virtual void buildUserInterface(UI* faust_interface) {
		faust_interface->openVerticalBox("0x00");
		faust_interface->openHorizontalBox("Basic_Parameters");
		faust_interface->declare(&fentry0, "1", "");
		faust_interface->declare(&fentry0, "tooltip", "Tone frequency");
		faust_interface->declare(&fentry0, "unit", "Hz");
		faust_interface->addNumEntry("freq", &fentry0, 4.4e+02f, 2e+01f, 2e+04f, 1.0f);
		faust_interface->declare(&fentry2, "1", "");
		faust_interface->declare(&fentry2, "tooltip", "Gain (value between 0 and 1)");
		faust_interface->addNumEntry("gain", &fentry2, 1.0f, 0.0f, 1.0f, 0.01f);
		faust_interface->declare(&fbutton0, "1", "");
		faust_interface->declare(&fbutton0, "tooltip", "noteOn = 1, noteOff = 0");
		faust_interface->addButton("gate", &fbutton0);
		faust_interface->closeBox();
		faust_interface->openHorizontalBox("Envelopes_and_Vibrato");
		faust_interface->openVerticalBox("Envelope_Parameters");
		faust_interface->declare(&fslider6, "5", "");
		faust_interface->declare(&fslider6, "tooltip", "Envelope attack duration");
		faust_interface->declare(&fslider6, "unit", "s");
		faust_interface->addHorizontalSlider("Envelope_Attack", &fslider6, 0.01f, 0.0f, 2.0f, 0.01f);
		faust_interface->declare(&fslider5, "5", "");
		faust_interface->declare(&fslider5, "tooltip", "Envelope decay duration");
		faust_interface->declare(&fslider5, "unit", "s");
		faust_interface->addHorizontalSlider("Envelope_Decay", &fslider5, 0.05f, 0.0f, 2.0f, 0.01f);
		faust_interface->declare(&fslider2, "5", "");
		faust_interface->declare(&fslider2, "tooltip", "Envelope release duration");
		faust_interface->declare(&fslider2, "unit", "s");
		faust_interface->addHorizontalSlider("Envelope_Release", &fslider2, 0.1f, 0.0f, 2.0f, 0.01f);
		faust_interface->closeBox();
		faust_interface->openVerticalBox("Vibrato_Parameters");
		faust_interface->declare(&fslider10, "4", "");
		faust_interface->declare(&fslider10, "tooltip", "Vibrato attack duration");
		faust_interface->declare(&fslider10, "unit", "s");
		faust_interface->addHorizontalSlider("Vibrato_Attack", &fslider10, 0.5f, 0.0f, 2.0f, 0.01f);
		faust_interface->declare(&fslider9, "4", "");
		faust_interface->declare(&fslider9, "tooltip", "Vibrato silence duration before attack");
		faust_interface->declare(&fslider9, "unit", "s");
		faust_interface->addHorizontalSlider("Vibrato_Begin", &fslider9, 0.05f, 0.0f, 2.0f, 0.01f);
		faust_interface->declare(&fslider11, "4", "");
		faust_interface->declare(&fslider11, "tooltip", "A value between 0 and 1");
		faust_interface->addHorizontalSlider("Vibrato_Gain", &fslider11, 0.01f, 0.0f, 1.0f, 0.01f);
		faust_interface->declare(&fslider7, "4", "");
		faust_interface->declare(&fslider7, "unit", "Hz");
		faust_interface->addHorizontalSlider("Vibrato_Gain", &fslider7, 6.0f, 1.0f, 15.0f, 0.1f);
		faust_interface->declare(&fslider8, "4", "");
		faust_interface->declare(&fslider8, "tooltip", "Vibrato release duration");
		faust_interface->declare(&fslider8, "unit", "s");
		faust_interface->addHorizontalSlider("Vibrato_Release", &fslider8, 0.01f, 0.0f, 2.0f, 0.01f);
		faust_interface->closeBox();
		faust_interface->closeBox();
		faust_interface->openHorizontalBox("Physical_and_Nonlinearity");
		faust_interface->openVerticalBox("Nonlinear_Filter_Parameters");
		faust_interface->declare(&fslider4, "3", "");
		faust_interface->declare(&fslider4, "tooltip", "Frequency of the sine wave for the modulation of theta (works if Modulation Type=3)");
		faust_interface->declare(&fslider4, "unit", "Hz");
		faust_interface->addHorizontalSlider("Modulation_Frequency", &fslider4, 2.2e+02f, 2e+01f, 1e+03f, 0.1f);
		faust_interface->declare(&fentry1, "3", "");
		faust_interface->declare(&fentry1, "tooltip", "0=theta is modulated by the incoming signal; 1=theta is modulated by the averaged incoming signal; 2=theta is modulated by the squared incoming signal; 3=theta is modulated by a sine wave of frequency freqMod; 4=theta is modulated by a sine wave of frequency freq;");
		faust_interface->addNumEntry("Modulation_Type", &fentry1, 0.0f, 0.0f, 4.0f, 1.0f);
		faust_interface->declare(&fslider1, "3", "");
		faust_interface->declare(&fslider1, "tooltip", "Nonlinearity factor (value between 0 and 1)");
		faust_interface->addHorizontalSlider("Nonlinearity", &fslider1, 0.0f, 0.0f, 1.0f, 0.01f);
		faust_interface->declare(&fslider3, "3", "");
		faust_interface->declare(&fslider3, "Attack duration of the nonlinearity", "");
		faust_interface->declare(&fslider3, "unit", "s");
		faust_interface->addHorizontalSlider("Nonlinearity_Attack", &fslider3, 0.1f, 0.0f, 2.0f, 0.01f);
		faust_interface->closeBox();
		faust_interface->openVerticalBox("Physical_Parameters");
		faust_interface->declare(&fslider0, "2", "");
		faust_interface->declare(&fslider0, "tooltip", "Bow position along the string (value between 0 and 1)");
		faust_interface->addHorizontalSlider("Bow_Pressure", &fslider0, 0.7f, 0.01f, 1.0f, 0.01f);
		faust_interface->declare(&fslider12, "2", "");
		faust_interface->declare(&fslider12, "tooltip", "Bow pressure on the string (value between 0 and 1)");
		faust_interface->addHorizontalSlider("Bow_Pressure", &fslider12, 0.75f, 0.0f, 1.0f, 0.01f);
		faust_interface->closeBox();
		faust_interface->closeBox();
		faust_interface->openVerticalBox("Spat");
		faust_interface->addHorizontalSlider("pan angle", &fslider13, 0.6f, 0.0f, 1.0f, 0.01f);
		faust_interface->addHorizontalSlider("spatial width", &fslider14, 0.5f, 0.0f, 1.0f, 0.01f);
		faust_interface->closeBox();
		faust_interface->closeBox();
	}
	virtual void compute (int count, FAUSTFLOAT** input, FAUSTFLOAT** output) {
		float 	fSlow0 = float(fentry0);
		float 	fSlow1 = ((float(iConst0) / fSlow0) - 4);
		float 	fSlow2 = (0.2f * float(fslider0));
		int 	iSlow3 = int((1 + int((int(((0.027236f + fSlow2) * fSlow1)) & 4095))));
		float 	fSlow4 = (0.0010000000000000009f * float(fslider1));
		float 	fSlow5 = float(fentry1);
		float 	fSlow6 = (3.141592653589793f * (fSlow5 == 2));
		int 	iSlow7 = int((1 + iSlow3));
		float 	fSlow8 = (1.5707963267948966f * (fSlow5 == 1));
		float 	fSlow9 = (3.141592653589793f * (fSlow5 == 0));
		float 	fSlow10 = float(fbutton0);
		int 	iSlow11 = (fSlow10 > 0);
		int 	iSlow12 = (fSlow10 <= 0);
		float 	fSlow13 = float(fslider2);
		float 	fSlow14 = (1 - (1.0f / powf(1e+05f,(1.0f / ((fSlow13 == 0.0f) + (iConst0 * fSlow13))))));
		float 	fSlow15 = float(fslider3);
		float 	fSlow16 = (1.0f / ((fSlow15 == 0.0f) + (iConst0 * fSlow15)));
		int 	iSlow17 = (fSlow5 < 3);
		float 	fSlow18 = (0.0010000000000000009f * float(fslider4));
		int 	iSlow19 = (fSlow5 != 4);
		float 	fSlow20 = (fSlow0 * (fSlow5 == 4));
		int 	iSlow21 = (fSlow5 >= 3);
		float 	fSlow22 = float(fentry2);
		float 	fSlow23 = (fSlow13 * (1 - fSlow22));
		float 	fSlow24 = (1 - (1.0f / powf(9e+04f,(1.0f / ((iConst0 * fSlow23) + (fSlow23 == 0.0f))))));
		float 	fSlow25 = float(fslider5);
		float 	fSlow26 = (1 - powf(9e+01f,(1.0f / ((fSlow25 == 0.0f) + (iConst0 * fSlow25)))));
		float 	fSlow27 = (float(fslider6) * fSlow22);
		float 	fSlow28 = (1.0f / ((iConst0 * fSlow27) + (fSlow27 == 0.0f)));
		float 	fSlow29 = (0.03f + (0.2f * fSlow22));
		float 	fSlow30 = (fConst4 * float(fslider7));
		float 	fSlow31 = float(fslider8);
		float 	fSlow32 = (1 - (1.0f / powf(1e+05f,(1.0f / ((fSlow31 == 0.0f) + (iConst0 * fSlow31))))));
		float 	fSlow33 = float(fslider9);
		float 	fSlow34 = (iConst0 * fSlow33);
		float 	fSlow35 = ((fSlow33 == 0.0f) + fSlow34);
		float 	fSlow36 = float(fslider10);
		float 	fSlow37 = (1.0f / ((fSlow36 == 0.0f) + (iConst0 * fSlow36)));
		float 	fSlow38 = float(fslider11);
		float 	fSlow39 = (0.972764f - fSlow2);
		float 	fSlow40 = (5 - (4 * float(fslider12)));
		float 	fSlow41 = float(fslider13);
		float 	fSlow42 = (8 * (fSlow22 * (1.0f - fSlow41)));
		float 	fSlow43 = (8 * fSlow22);
		int 	iSlow44 = int((int((fConst6 * (float(fslider14) / fSlow0))) & 4095));
		FAUSTFLOAT* output0 = output[0];
		FAUSTFLOAT* output1 = output[1];
		for (int i=0; i<count; i++) {
			float fTemp0 = fRec1[(IOTA-iSlow3)&8191];
			fRec5[0] = (fSlow4 + (0.999f * fRec5[1]));
			iRec6[0] = (iSlow11 & (iRec6[1] | (fRec7[1] >= 1)));
			int iTemp1 = (iSlow12 & (fRec7[1] > 0));
			fRec7[0] = (((fSlow16 * (((iRec6[1] == 0) & iSlow11) & (fRec7[1] < 1))) + (fRec7[1] * (1 - (fSlow14 * iTemp1)))) * ((iTemp1 == 0) | (fRec7[1] >= 1e-06f)));
			float fTemp2 = (fRec5[0] * fRec7[0]);
			float fTemp3 = (fTemp2 * (((fSlow9 * fTemp0) + (fSlow8 * (fTemp0 + fRec1[(IOTA-iSlow7)&8191]))) + (fSlow6 * faustpower<2>(fTemp0))));
			float fTemp4 = cosf(fTemp3);
			float fTemp5 = sinf(fTemp3);
			float fTemp6 = (0 - fTemp5);
			float fTemp7 = ((fRec8[1] * fTemp6) + (fTemp0 * fTemp4));
			float fTemp8 = ((fTemp6 * fRec9[1]) + (fTemp4 * fTemp7));
			float fTemp9 = ((fTemp6 * fRec10[1]) + (fTemp4 * fTemp8));
			float fTemp10 = ((fTemp6 * fRec11[1]) + (fTemp4 * fTemp9));
			float fTemp11 = ((fTemp6 * fRec12[1]) + (fTemp4 * fTemp10));
			fRec13[0] = ((fTemp6 * fRec13[1]) + (fTemp4 * fTemp11));
			fRec12[0] = ((fTemp5 * fTemp11) + (fTemp4 * fRec13[1]));
			fRec11[0] = ((fTemp5 * fTemp10) + (fTemp4 * fRec12[1]));
			fRec10[0] = ((fTemp5 * fTemp9) + (fTemp4 * fRec11[1]));
			fRec9[0] = ((fTemp5 * fTemp8) + (fTemp4 * fRec10[1]));
			fRec8[0] = ((fTemp5 * fTemp7) + (fTemp4 * fRec9[1]));
			fRec16[0] = (fSlow18 + (0.999f * fRec16[1]));
			float fTemp12 = (fRec15[1] + (fConst4 * (fSlow20 + (iSlow19 * fRec16[0]))));
			fRec15[0] = (fTemp12 - floorf(fTemp12));
			float fTemp13 = (3.141592653589793f * (fTemp2 * ftbl0[int((65536.0f * fRec15[0]))]));
			float fTemp14 = cosf(fTemp13);
			float fTemp15 = sinf(fTemp13);
			float fTemp16 = (0 - fTemp15);
			float fTemp17 = ((fRec17[1] * fTemp16) + (fTemp0 * fTemp14));
			float fTemp18 = ((fTemp16 * fRec18[1]) + (fTemp14 * fTemp17));
			float fTemp19 = ((fTemp16 * fRec19[1]) + (fTemp14 * fTemp18));
			float fTemp20 = ((fTemp16 * fRec20[1]) + (fTemp14 * fTemp19));
			float fTemp21 = ((fTemp16 * fRec21[1]) + (fTemp14 * fTemp20));
			fRec22[0] = ((fTemp16 * fRec22[1]) + (fTemp14 * fTemp21));
			fRec21[0] = ((fTemp15 * fTemp21) + (fTemp14 * fRec22[1]));
			fRec20[0] = ((fTemp15 * fTemp20) + (fTemp14 * fRec21[1]));
			fRec19[0] = ((fTemp15 * fTemp19) + (fTemp14 * fRec20[1]));
			fRec18[0] = ((fTemp15 * fTemp18) + (fTemp14 * fRec19[1]));
			fRec17[0] = ((fTemp15 * fTemp17) + (fTemp14 * fRec18[1]));
			fRec4[0] = ((fConst5 * ((iSlow21 * ((fTemp0 * fTemp15) + (fRec17[1] * fTemp14))) + (iSlow17 * ((fRec5[0] * ((fTemp0 * fTemp5) + (fRec8[1] * fTemp4))) + ((1 - fRec5[0]) * fTemp0))))) - (fConst3 * fRec4[1]));
			iRec23[0] = (iSlow11 & (iRec23[1] | (fRec24[1] >= 1)));
			int iTemp22 = (iSlow12 & (fRec24[1] > 0));
			fRec24[0] = (((fSlow28 * (((iRec23[1] == 0) & iSlow11) & (fRec24[1] < 1))) + (fRec24[1] * ((1 - (fSlow26 * (iRec23[1] & (fRec24[1] > 90)))) - (fSlow24 * iTemp22)))) * ((iTemp22 == 0) | (fRec24[1] >= 1e-06f)));
			float fTemp23 = (fSlow30 + fRec25[1]);
			fRec25[0] = (fTemp23 - floorf(fTemp23));
			iRec26[0] = (iSlow11 & (iRec26[1] | (fRec28[1] >= 1)));
			iRec27[0] = (iSlow11 * (1 + iRec27[1]));
			int iTemp24 = (iSlow12 & (fRec28[1] > 0));
			fRec28[0] = (((fSlow37 * (((((iRec26[1] == 0) & iSlow11) & (fRec28[1] < 1)) & (iRec27[1] > fSlow34)) * (1 - (iRec27[1] < fSlow35)))) + (fRec28[1] * (1 - (fSlow32 * iTemp24)))) * ((iTemp24 == 0) | (fRec28[1] >= 1e-06f)));
			float fTemp25 = (fSlow1 * (fSlow39 + (fSlow38 * (fRec28[0] * ftbl0[int((65536.0f * fRec25[0]))]))));
			int iTemp26 = int(fTemp25);
			int iTemp27 = (1 + iTemp26);
			float fTemp28 = (fRec2[(IOTA-int((1 + int((iTemp26 & 4095)))))&8191] * (iTemp27 - fTemp25));
			float fTemp29 = ((fTemp25 - iTemp26) * fRec2[(IOTA-int((1 + int((int(iTemp27) & 4095)))))&8191]);
			float fTemp30 = (fTemp29 + (fTemp28 + (fRec4[0] + (fSlow29 * fRec24[0]))));
			float fTemp31 = faustpower<4>((0.75f + fabsf((fSlow40 * fTemp30))));
			float fTemp32 = (1.0f / fTemp31);
			float fTemp33 = (fTemp30 * ((fTemp32 > 1) + (float((fTemp32 <= 1)) / fTemp31)));
			fRec2[IOTA&8191] = (fTemp33 - fRec4[0]);
			float 	fRec3 = (0 - ((fTemp28 + fTemp29) - fTemp33));
			fRec1[IOTA&8191] = fRec3;
			fRec0[0] = ((0.2f * fRec1[(IOTA-0)&8191]) - ((fConst1 * fRec0[1]) + (0.7224999999999999f * fRec0[2])));
			float fTemp34 = ((0.13875000000000004f * fRec0[0]) - (0.13875000000000004f * fRec0[2]));
			output0[i] = (FAUSTFLOAT)(fSlow42 * fTemp34);
			fVec0[IOTA&4095] = (fSlow43 * fTemp34);
			output1[i] = (FAUSTFLOAT)(fSlow41 * fVec0[(IOTA-iSlow44)&4095]);
			// post processing
			fRec0[2] = fRec0[1]; fRec0[1] = fRec0[0];
			IOTA = IOTA+1;
			fRec28[1] = fRec28[0];
			iRec27[1] = iRec27[0];
			iRec26[1] = iRec26[0];
			fRec25[1] = fRec25[0];
			fRec24[1] = fRec24[0];
			iRec23[1] = iRec23[0];
			fRec4[1] = fRec4[0];
			fRec17[1] = fRec17[0];
			fRec18[1] = fRec18[0];
			fRec19[1] = fRec19[0];
			fRec20[1] = fRec20[0];
			fRec21[1] = fRec21[0];
			fRec22[1] = fRec22[0];
			fRec15[1] = fRec15[0];
			fRec16[1] = fRec16[0];
			fRec8[1] = fRec8[0];
			fRec9[1] = fRec9[0];
			fRec10[1] = fRec10[0];
			fRec11[1] = fRec11[0];
			fRec12[1] = fRec12[0];
			fRec13[1] = fRec13[0];
			fRec7[1] = fRec7[0];
			iRec6[1] = iRec6[0];
			fRec5[1] = fRec5[0];
		}
	}
};


float 	Bowed_dsp::ftbl0[65536];


#include "Faust_plugins_template2.cpp"

