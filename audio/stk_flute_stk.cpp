//-----------------------------------------------------
// name: "FluteSTK"
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
#define FAUSTCLASS Flute_Stk_dsp
#endif

class Flute_Stk_dsp : public dsp {
  private:
	class SIG0 {
	  private:
		int 	fSamplingFreq;
		int 	iRec13[2];
	  public:
		int getNumInputs() 	{ return 0; }
		int getNumOutputs() 	{ return 1; }
		void init(int samplingFreq) {
			fSamplingFreq = samplingFreq;
			for (int i=0; i<2; i++) iRec13[i] = 0;
		}
		void fill (int count, float output[]) {
			for (int i=0; i<count; i++) {
				iRec13[0] = (1 + iRec13[1]);
				output[i] = sinf((9.587379924285257e-05f * float((iRec13[0] - 1))));
				// post processing
				iRec13[1] = iRec13[0];
			}
		}
	};


	FAUSTFLOAT 	fentry0;
	float 	fRec0[2];
	int 	iConst0;
	float 	fConst1;
	float 	fConst2;
	FAUSTFLOAT 	fentry1;
	float 	fConst3;
	float 	fVec0[2];
	FAUSTFLOAT 	fslider0;
	float 	fRec4[2];
	FAUSTFLOAT 	fentry2;
	FAUSTFLOAT 	fbutton0;
	int 	iRec5[2];
	FAUSTFLOAT 	fslider1;
	FAUSTFLOAT 	fslider2;
	float 	fRec6[2];
	float 	fRec12[2];
	float 	fRec11[2];
	float 	fRec10[2];
	float 	fRec9[2];
	float 	fRec8[2];
	float 	fRec7[2];
	static float 	ftbl0[65536];
	FAUSTFLOAT 	fslider3;
	float 	fRec15[2];
	float 	fConst4;
	float 	fRec14[2];
	float 	fRec21[2];
	float 	fRec20[2];
	float 	fRec19[2];
	float 	fRec18[2];
	float 	fRec17[2];
	float 	fRec16[2];
	float 	fConst5;
	float 	fRec3[2];
	float 	fRec2[2];
	FAUSTFLOAT 	fslider4;
	float 	fRec22[2];
	int 	iRec23[2];
	int 	iRec24[2];
	FAUSTFLOAT 	fslider5;
	FAUSTFLOAT 	fslider6;
	FAUSTFLOAT 	fslider7;
	float 	fRec25[2];
	FAUSTFLOAT 	fslider8;
	int 	iRec26[2];
	FAUSTFLOAT 	fslider9;
	int 	iRec27[2];
	FAUSTFLOAT 	fslider10;
	FAUSTFLOAT 	fslider11;
	FAUSTFLOAT 	fslider12;
	float 	fRec28[2];
	int 	IOTA;
	float 	fVec1[4096];
	FAUSTFLOAT 	fslider13;
	float 	fRec1[8192];
	float 	fVec2[4096];
	FAUSTFLOAT 	fslider14;
	FAUSTFLOAT 	fslider15;
  public:
	static void metadata(Meta* m) 	{ 
		m->declare("name", "FluteSTK");
		m->declare("description", "Nonlinear WaveGuide Flute from STK");
		m->declare("author", "Romain Michon");
		m->declare("copyright", "Romain Michon (rmichon@ccrma.stanford.edu)");
		m->declare("version", "1.0");
		m->declare("licence", "STK-4.3");
		m->declare("reference", "https://ccrma.stanford.edu/~jos/pasp/Flutes_Recorders_Pipe_Organs.html");
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
		fentry0 = 1.0f;
		for (int i=0; i<2; i++) fRec0[i] = 0;
		iConst0 = min(192000, max(1, fSamplingFreq));
		fConst1 = (2205.0f / float(iConst0));
		fConst2 = (fConst1 - 0.7f);
		fentry1 = 4.4e+02f;
		fConst3 = (0.5f * iConst0);
		for (int i=0; i<2; i++) fVec0[i] = 0;
		fslider0 = 0.0f;
		for (int i=0; i<2; i++) fRec4[i] = 0;
		fentry2 = 0.0f;
		fbutton0 = 0.0;
		for (int i=0; i<2; i++) iRec5[i] = 0;
		fslider1 = 0.3f;
		fslider2 = 0.1f;
		for (int i=0; i<2; i++) fRec6[i] = 0;
		for (int i=0; i<2; i++) fRec12[i] = 0;
		for (int i=0; i<2; i++) fRec11[i] = 0;
		for (int i=0; i<2; i++) fRec10[i] = 0;
		for (int i=0; i<2; i++) fRec9[i] = 0;
		for (int i=0; i<2; i++) fRec8[i] = 0;
		for (int i=0; i<2; i++) fRec7[i] = 0;
		fslider3 = 2.2e+02f;
		for (int i=0; i<2; i++) fRec15[i] = 0;
		fConst4 = (1.0f / float(iConst0));
		for (int i=0; i<2; i++) fRec14[i] = 0;
		for (int i=0; i<2; i++) fRec21[i] = 0;
		for (int i=0; i<2; i++) fRec20[i] = 0;
		for (int i=0; i<2; i++) fRec19[i] = 0;
		for (int i=0; i<2; i++) fRec18[i] = 0;
		for (int i=0; i<2; i++) fRec17[i] = 0;
		for (int i=0; i<2; i++) fRec16[i] = 0;
		fConst5 = (fConst1 + 0.30000000000000004f);
		for (int i=0; i<2; i++) fRec3[i] = 0;
		for (int i=0; i<2; i++) fRec2[i] = 0;
		fslider4 = 6.0f;
		for (int i=0; i<2; i++) fRec22[i] = 0;
		for (int i=0; i<2; i++) iRec23[i] = 0;
		for (int i=0; i<2; i++) iRec24[i] = 0;
		fslider5 = 0.1f;
		fslider6 = 0.05f;
		fslider7 = 0.5f;
		for (int i=0; i<2; i++) fRec25[i] = 0;
		fslider8 = 0.05f;
		for (int i=0; i<2; i++) iRec26[i] = 0;
		fslider9 = 0.03f;
		for (int i=0; i<2; i++) iRec27[i] = 0;
		fslider10 = 0.01f;
		fslider11 = 1.0f;
		fslider12 = 0.03f;
		for (int i=0; i<2; i++) fRec28[i] = 0;
		IOTA = 0;
		for (int i=0; i<4096; i++) fVec1[i] = 0;
		fslider13 = 0.5f;
		for (int i=0; i<8192; i++) fRec1[i] = 0;
		for (int i=0; i<4096; i++) fVec2[i] = 0;
		fslider14 = 0.6f;
		fslider15 = 0.5f;
	}
	virtual void init(int samplingFreq) {
		classInit(samplingFreq);
		instanceInit(samplingFreq);
	}
	virtual void buildUserInterface(UI* faust_interface) {
		faust_interface->openVerticalBox("0x00");
		faust_interface->openHorizontalBox("Basic_Parameters");
		faust_interface->declare(&fentry1, "1", "");
		faust_interface->declare(&fentry1, "tooltip", "Tone frequency");
		faust_interface->declare(&fentry1, "unit", "Hz");
		faust_interface->addNumEntry("freq", &fentry1, 4.4e+02f, 2e+01f, 2e+04f, 1.0f);
		faust_interface->declare(&fentry0, "1", "");
		faust_interface->declare(&fentry0, "tooltip", "Gain (value between 0 and 1)");
		faust_interface->addNumEntry("gain", &fentry0, 1.0f, 0.0f, 1.0f, 0.01f);
		faust_interface->declare(&fbutton0, "1", "");
		faust_interface->declare(&fbutton0, "tooltip", "noteOn = 1, noteOff = 0");
		faust_interface->addButton("gate", &fbutton0);
		faust_interface->closeBox();
		faust_interface->openHorizontalBox("Envelopes_and_Vibrato");
		faust_interface->openVerticalBox("Envelope_Parameters");
		faust_interface->declare(&fslider12, "5", "");
		faust_interface->declare(&fslider12, "tooltip", "Envelope attack duration");
		faust_interface->declare(&fslider12, "unit", "s");
		faust_interface->addHorizontalSlider("Envelope_Attack", &fslider12, 0.03f, 0.0f, 2.0f, 0.01f);
		faust_interface->declare(&fslider10, "5", "");
		faust_interface->declare(&fslider10, "tooltip", "Envelope decay duration");
		faust_interface->declare(&fslider10, "unit", "s");
		faust_interface->addHorizontalSlider("Envelope_Decay", &fslider10, 0.01f, 0.0f, 2.0f, 0.01f);
		faust_interface->declare(&fslider1, "5", "");
		faust_interface->declare(&fslider1, "tooltip", "Envelope release duration");
		faust_interface->declare(&fslider1, "unit", "s");
		faust_interface->addHorizontalSlider("Envelope_Release", &fslider1, 0.3f, 0.0f, 2.0f, 0.01f);
		faust_interface->closeBox();
		faust_interface->openVerticalBox("Vibrato_Parameters");
		faust_interface->declare(&fslider7, "4", "");
		faust_interface->declare(&fslider7, "tooltip", "Vibrato attack duration");
		faust_interface->declare(&fslider7, "unit", "s");
		faust_interface->addHorizontalSlider("Vibrato_Attack", &fslider7, 0.5f, 0.0f, 2.0f, 0.01f);
		faust_interface->declare(&fslider6, "4", "");
		faust_interface->declare(&fslider6, "tooltip", "Vibrato silence duration before attack");
		faust_interface->declare(&fslider6, "unit", "s");
		faust_interface->addHorizontalSlider("Vibrato_Begin", &fslider6, 0.05f, 0.0f, 2.0f, 0.01f);
		faust_interface->declare(&fslider4, "4", "");
		faust_interface->declare(&fslider4, "unit", "Hz");
		faust_interface->addHorizontalSlider("Vibrato_Freq", &fslider4, 6.0f, 1.0f, 15.0f, 0.1f);
		faust_interface->declare(&fslider8, "4", "");
		faust_interface->declare(&fslider8, "tooltip", "A value between 0 and 1");
		faust_interface->addHorizontalSlider("Vibrato_Gain", &fslider8, 0.05f, 0.0f, 1.0f, 0.01f);
		faust_interface->declare(&fslider5, "4", "");
		faust_interface->declare(&fslider5, "tooltip", "Vibrato release duration");
		faust_interface->declare(&fslider5, "unit", "s");
		faust_interface->addHorizontalSlider("Vibrato_Release", &fslider5, 0.1f, 0.0f, 2.0f, 0.01f);
		faust_interface->closeBox();
		faust_interface->closeBox();
		faust_interface->openHorizontalBox("Physical_and_Nonlinearity");
		faust_interface->openVerticalBox("Nonlinear_Filter_Parameters");
		faust_interface->declare(&fslider3, "3", "");
		faust_interface->declare(&fslider3, "tooltip", "Frequency of the sine wave for the modulation of theta (works if Modulation Type=3)");
		faust_interface->declare(&fslider3, "unit", "Hz");
		faust_interface->addHorizontalSlider("Modulation_Frequency", &fslider3, 2.2e+02f, 2e+01f, 1e+03f, 0.1f);
		faust_interface->declare(&fentry2, "3", "");
		faust_interface->declare(&fentry2, "tooltip", "0=theta is modulated by the incoming signal; 1=theta is modulated by the averaged incoming signal; 2=theta is modulated by the squared incoming signal; 3=theta is modulated by a sine wave of frequency freqMod; 4=theta is modulated by a sine wave of frequency freq;");
		faust_interface->addNumEntry("Modulation_Type", &fentry2, 0.0f, 0.0f, 4.0f, 1.0f);
		faust_interface->declare(&fslider0, "3", "");
		faust_interface->declare(&fslider0, "tooltip", "Nonlinearity factor (value between 0 and 1)");
		faust_interface->addHorizontalSlider("Nonlinearity", &fslider0, 0.0f, 0.0f, 1.0f, 0.01f);
		faust_interface->declare(&fslider2, "3", "");
		faust_interface->declare(&fslider2, "Attack duration of the nonlinearity", "");
		faust_interface->declare(&fslider2, "unit", "s");
		faust_interface->addHorizontalSlider("Nonlinearity_Attack", &fslider2, 0.1f, 0.0f, 2.0f, 0.01f);
		faust_interface->closeBox();
		faust_interface->openVerticalBox("Physical_Parameters");
		faust_interface->declare(&fslider13, "2", "");
		faust_interface->declare(&fslider13, "tooltip", "A value between 0 and 1");
		faust_interface->addHorizontalSlider("Embouchure_Ajust", &fslider13, 0.5f, 0.0f, 1.0f, 0.01f);
		faust_interface->declare(&fslider9, "2", "");
		faust_interface->declare(&fslider9, "tooltip", "A value between 0 and 1");
		faust_interface->addHorizontalSlider("Noise_Gain", &fslider9, 0.03f, 0.0f, 1.0f, 0.01f);
		faust_interface->declare(&fslider11, "2", "");
		faust_interface->declare(&fslider11, "tooltip", "Breath pressure (value between 0 and 1)");
		faust_interface->addHorizontalSlider("Pressure", &fslider11, 1.0f, 0.0f, 1.0f, 0.01f);
		faust_interface->closeBox();
		faust_interface->closeBox();
		faust_interface->openVerticalBox("Spat");
		faust_interface->addHorizontalSlider("pan angle", &fslider14, 0.6f, 0.0f, 1.0f, 0.01f);
		faust_interface->addHorizontalSlider("spatial width", &fslider15, 0.5f, 0.0f, 1.0f, 0.01f);
		faust_interface->closeBox();
		faust_interface->closeBox();
	}
	virtual void compute (int count, FAUSTFLOAT** input, FAUSTFLOAT** output) {
		float 	fSlow0 = (0.0010000000000000009f * float(fentry0));
		float 	fSlow1 = float(fentry1);
		float 	fSlow2 = (fConst3 / fSlow1);
		float 	fSlow3 = (fSlow2 - 2);
		int 	iSlow4 = int(fSlow3);
		int 	iSlow5 = int((1 + int((int((1 + iSlow4)) & 4095))));
		float 	fSlow6 = (fSlow2 - (2 + iSlow4));
		int 	iSlow7 = int((1 + int((iSlow4 & 4095))));
		float 	fSlow8 = ((3 + iSlow4) - fSlow2);
		float 	fSlow9 = (0.0010000000000000009f * float(fslider0));
		float 	fSlow10 = float(fentry2);
		float 	fSlow11 = (3.141592653589793f * (fSlow10 == 2));
		float 	fSlow12 = (1.5707963267948966f * (fSlow10 == 1));
		float 	fSlow13 = (3.141592653589793f * (fSlow10 == 0));
		float 	fSlow14 = float(fbutton0);
		int 	iSlow15 = (fSlow14 > 0);
		int 	iSlow16 = (fSlow14 <= 0);
		float 	fSlow17 = float(fslider1);
		float 	fSlow18 = (1.0f / ((fSlow17 == 0.0f) + (iConst0 * fSlow17)));
		float 	fSlow19 = (1 - (1.0f / powf(1e+05f,fSlow18)));
		float 	fSlow20 = float(fslider2);
		float 	fSlow21 = (1.0f / ((fSlow20 == 0.0f) + (iConst0 * fSlow20)));
		int 	iSlow22 = (fSlow10 < 3);
		float 	fSlow23 = (0.0010000000000000009f * float(fslider3));
		int 	iSlow24 = (fSlow10 != 4);
		float 	fSlow25 = (fSlow1 * (fSlow10 == 4));
		int 	iSlow26 = (fSlow10 >= 3);
		float 	fSlow27 = (fConst4 * float(fslider4));
		float 	fSlow28 = float(fslider5);
		float 	fSlow29 = (1 - (1.0f / powf(1e+05f,(1.0f / ((fSlow28 == 0.0f) + (iConst0 * fSlow28))))));
		float 	fSlow30 = float(fslider6);
		float 	fSlow31 = (iConst0 * fSlow30);
		float 	fSlow32 = ((fSlow30 == 0.0f) + fSlow31);
		float 	fSlow33 = float(fslider7);
		float 	fSlow34 = (1.0f / ((fSlow33 == 0.0f) + (iConst0 * fSlow33)));
		float 	fSlow35 = float(fslider8);
		float 	fSlow36 = (4.656612875245797e-10f * float(fslider9));
		float 	fSlow37 = (1 - (1.0f / powf(8e+04f,fSlow18)));
		float 	fSlow38 = float(fslider10);
		float 	fSlow39 = (1 - powf(8e+01f,(1.0f / ((fSlow38 == 0.0f) + (iConst0 * fSlow38)))));
		float 	fSlow40 = float(fslider11);
		float 	fSlow41 = (float(fslider12) * fSlow40);
		float 	fSlow42 = (1.0f / ((iConst0 * fSlow41) + (fSlow41 == 0.0f)));
		float 	fSlow43 = (fSlow3 * (1.5f - float(fslider13)));
		int 	iSlow44 = int(fSlow43);
		int 	iSlow45 = (1 + iSlow44);
		int 	iSlow46 = int((int(iSlow45) & 4095));
		float 	fSlow47 = (fSlow43 - iSlow44);
		int 	iSlow48 = int((iSlow44 & 4095));
		float 	fSlow49 = (iSlow45 - fSlow43);
		float 	fSlow50 = float(fslider14);
		float 	fSlow51 = (0.3f * (1.0f - fSlow50));
		int 	iSlow52 = int((int((fConst3 * (float(fslider15) / fSlow1))) & 4095));
		float 	fSlow53 = (0.3f * fSlow50);
		FAUSTFLOAT* output0 = output[0];
		FAUSTFLOAT* output1 = output[1];
		for (int i=0; i<count; i++) {
			fRec0[0] = ((0.999f * fRec0[1]) + fSlow0);
			float fTemp0 = ((fSlow8 * fRec1[(IOTA-iSlow7)&8191]) + (fSlow6 * fRec1[(IOTA-iSlow5)&8191]));
			fVec0[0] = fTemp0;
			fRec4[0] = (fSlow9 + (0.999f * fRec4[1]));
			iRec5[0] = (iSlow15 & (iRec5[1] | (fRec6[1] >= 1)));
			int iTemp1 = (iSlow16 & (fRec6[1] > 0));
			fRec6[0] = (((fSlow21 * (((iRec5[1] == 0) & iSlow15) & (fRec6[1] < 1))) + (fRec6[1] * (1 - (fSlow19 * iTemp1)))) * ((iTemp1 == 0) | (fRec6[1] >= 1e-06f)));
			float fTemp2 = (fRec4[0] * fRec6[0]);
			float fTemp3 = (fTemp2 * (((fSlow13 * fVec0[0]) + (fSlow12 * (fVec0[0] + fVec0[1]))) + (fSlow11 * faustpower<2>(fVec0[0]))));
			float fTemp4 = cosf(fTemp3);
			float fTemp5 = sinf(fTemp3);
			float fTemp6 = (0 - fTemp5);
			float fTemp7 = ((fRec7[1] * fTemp6) + (fVec0[0] * fTemp4));
			float fTemp8 = ((fTemp6 * fRec8[1]) + (fTemp4 * fTemp7));
			float fTemp9 = ((fTemp6 * fRec9[1]) + (fTemp4 * fTemp8));
			float fTemp10 = ((fTemp6 * fRec10[1]) + (fTemp4 * fTemp9));
			float fTemp11 = ((fTemp6 * fRec11[1]) + (fTemp4 * fTemp10));
			fRec12[0] = ((fTemp6 * fRec12[1]) + (fTemp4 * fTemp11));
			fRec11[0] = ((fTemp5 * fTemp11) + (fTemp4 * fRec12[1]));
			fRec10[0] = ((fTemp5 * fTemp10) + (fTemp4 * fRec11[1]));
			fRec9[0] = ((fTemp5 * fTemp9) + (fTemp4 * fRec10[1]));
			fRec8[0] = ((fTemp5 * fTemp8) + (fTemp4 * fRec9[1]));
			fRec7[0] = ((fTemp5 * fTemp7) + (fTemp4 * fRec8[1]));
			fRec15[0] = (fSlow23 + (0.999f * fRec15[1]));
			float fTemp12 = (fRec14[1] + (fConst4 * (fSlow25 + (iSlow24 * fRec15[0]))));
			fRec14[0] = (fTemp12 - floorf(fTemp12));
			float fTemp13 = (3.141592653589793f * (fTemp2 * ftbl0[int((65536.0f * fRec14[0]))]));
			float fTemp14 = cosf(fTemp13);
			float fTemp15 = sinf(fTemp13);
			float fTemp16 = (0 - fTemp15);
			float fTemp17 = ((fRec16[1] * fTemp16) + (fVec0[0] * fTemp14));
			float fTemp18 = ((fTemp16 * fRec17[1]) + (fTemp14 * fTemp17));
			float fTemp19 = ((fTemp16 * fRec18[1]) + (fTemp14 * fTemp18));
			float fTemp20 = ((fTemp16 * fRec19[1]) + (fTemp14 * fTemp19));
			float fTemp21 = ((fTemp16 * fRec20[1]) + (fTemp14 * fTemp20));
			fRec21[0] = ((fTemp16 * fRec21[1]) + (fTemp14 * fTemp21));
			fRec20[0] = ((fTemp15 * fTemp21) + (fTemp14 * fRec21[1]));
			fRec19[0] = ((fTemp15 * fTemp20) + (fTemp14 * fRec20[1]));
			fRec18[0] = ((fTemp15 * fTemp19) + (fTemp14 * fRec19[1]));
			fRec17[0] = ((fTemp15 * fTemp18) + (fTemp14 * fRec18[1]));
			fRec16[0] = ((fTemp15 * fTemp17) + (fTemp14 * fRec17[1]));
			fRec3[0] = ((fConst5 * (0 - ((iSlow26 * ((fVec0[0] * fTemp15) + (fRec16[1] * fTemp14))) + (iSlow22 * ((fRec4[0] * ((fVec0[0] * fTemp5) + (fRec7[1] * fTemp4))) + ((1 - fRec4[0]) * fVec0[0])))))) - (fConst2 * fRec3[1]));
			fRec2[0] = ((fRec3[0] + (0.995f * fRec2[1])) - fRec3[1]);
			float fTemp22 = (0.5f * fRec2[0]);
			float fTemp23 = (fRec22[1] + fSlow27);
			fRec22[0] = (fTemp23 - floorf(fTemp23));
			iRec23[0] = (iSlow15 & (iRec23[1] | (fRec25[1] >= 1)));
			iRec24[0] = (iSlow15 * (1 + iRec24[1]));
			int iTemp24 = (iSlow16 & (fRec25[1] > 0));
			fRec25[0] = (((fSlow34 * (((((iRec23[1] == 0) & iSlow15) & (fRec25[1] < 1)) & (iRec24[1] > fSlow31)) * (1 - (iRec24[1] < fSlow32)))) + (fRec25[1] * (1 - (fSlow29 * iTemp24)))) * ((iTemp24 == 0) | (fRec25[1] >= 1e-06f)));
			iRec26[0] = (12345 + (1103515245 * iRec26[1]));
			iRec27[0] = (iSlow15 & (iRec27[1] | (fRec28[1] >= 1)));
			int iTemp25 = (iSlow16 & (fRec28[1] > 0));
			fRec28[0] = (((fSlow42 * (((iRec27[1] == 0) & iSlow15) & (fRec28[1] < 1))) + (fRec28[1] * ((1 - (fSlow39 * (iRec27[1] & (fRec28[1] > 80)))) - (fSlow37 * iTemp25)))) * ((iTemp25 == 0) | (fRec28[1] >= 1e-06f)));
			float fTemp26 = ((1e-15f + (fSlow40 * (fRec28[0] * (1 + ((fSlow36 * iRec26[0]) + (fSlow35 * (fRec25[0] * ftbl0[int((65536.0f * fRec22[0]))]))))))) - fTemp22);
			fVec1[IOTA&4095] = fTemp26;
			float fTemp27 = ((fSlow49 * fVec1[(IOTA-iSlow48)&4095]) + (fSlow47 * fVec1[(IOTA-iSlow46)&4095]));
			float fTemp28 = (fTemp27 * (faustpower<2>(fTemp27) - 1));
			float fTemp29 = ((fTemp28 > 1) + (fTemp28 * (fTemp28 <= 1)));
			fRec1[IOTA&8191] = ((fTemp22 + (fTemp29 * (fTemp29 >= -1))) - (fTemp29 < -1));
			float fTemp30 = (fRec1[(IOTA-0)&8191] * fRec0[0]);
			fVec2[IOTA&4095] = fTemp30;
			output0[i] = (FAUSTFLOAT)(fSlow51 * fVec2[IOTA&4095]);
			output1[i] = (FAUSTFLOAT)(fSlow53 * fVec2[(IOTA-iSlow52)&4095]);
			// post processing
			IOTA = IOTA+1;
			fRec28[1] = fRec28[0];
			iRec27[1] = iRec27[0];
			iRec26[1] = iRec26[0];
			fRec25[1] = fRec25[0];
			iRec24[1] = iRec24[0];
			iRec23[1] = iRec23[0];
			fRec22[1] = fRec22[0];
			fRec2[1] = fRec2[0];
			fRec3[1] = fRec3[0];
			fRec16[1] = fRec16[0];
			fRec17[1] = fRec17[0];
			fRec18[1] = fRec18[0];
			fRec19[1] = fRec19[0];
			fRec20[1] = fRec20[0];
			fRec21[1] = fRec21[0];
			fRec14[1] = fRec14[0];
			fRec15[1] = fRec15[0];
			fRec7[1] = fRec7[0];
			fRec8[1] = fRec8[0];
			fRec9[1] = fRec9[0];
			fRec10[1] = fRec10[0];
			fRec11[1] = fRec11[0];
			fRec12[1] = fRec12[0];
			fRec6[1] = fRec6[0];
			iRec5[1] = iRec5[0];
			fRec4[1] = fRec4[0];
			fVec0[1] = fVec0[0];
			fRec0[1] = fRec0[0];
		}
	}
};


float 	Flute_Stk_dsp::ftbl0[65536];


#include "Faust_plugins_template2.cpp"

