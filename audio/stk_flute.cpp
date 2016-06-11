//-----------------------------------------------------
// name: "Flute"
// author: "Romain Michon (rmichon@ccrma.stanford.edu)"
// copyright: "Romain Michon"
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
#define FAUSTCLASS Flute_dsp
#endif

class Flute_dsp : public dsp {
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


	FAUSTFLOAT 	fentry0;
	float 	fRec0[2];
	FAUSTFLOAT 	fbutton0;
	int 	iRec1[2];
	FAUSTFLOAT 	fslider0;
	int 	iConst0;
	FAUSTFLOAT 	fslider1;
	float 	fRec2[2];
	FAUSTFLOAT 	fentry1;
	float 	fVec0[2];
	FAUSTFLOAT 	fslider2;
	float 	fRec5[2];
	FAUSTFLOAT 	fentry2;
	int 	iRec6[2];
	float 	fConst1;
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
	float 	fConst2;
	float 	fRec15[2];
	float 	fRec22[2];
	float 	fRec21[2];
	float 	fRec20[2];
	float 	fRec19[2];
	float 	fRec18[2];
	float 	fRec17[2];
	FAUSTFLOAT 	fslider5;
	float 	fRec23[2];
	int 	iRec24[2];
	int 	iRec25[2];
	FAUSTFLOAT 	fslider6;
	FAUSTFLOAT 	fslider7;
	FAUSTFLOAT 	fslider8;
	float 	fRec26[2];
	FAUSTFLOAT 	fslider9;
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
	float 	fConst3;
	float 	fVec2[2];
	float 	fConst4;
	float 	fConst5;
	float 	fConst6;
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
		fbutton0 = 0.0;
		for (int i=0; i<2; i++) iRec1[i] = 0;
		fslider0 = 0.1f;
		iConst0 = min(192000, max(1, fSamplingFreq));
		fslider1 = 0.1f;
		for (int i=0; i<2; i++) fRec2[i] = 0;
		fentry1 = 4.4e+02f;
		for (int i=0; i<2; i++) fVec0[i] = 0;
		fslider2 = 0.0f;
		for (int i=0; i<2; i++) fRec5[i] = 0;
		fentry2 = 0.0f;
		for (int i=0; i<2; i++) iRec6[i] = 0;
		fConst1 = (1 - (1.0f / powf(1e+05f,(1e+01f / float(iConst0)))));
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
		fConst2 = (1.0f / float(iConst0));
		for (int i=0; i<2; i++) fRec15[i] = 0;
		for (int i=0; i<2; i++) fRec22[i] = 0;
		for (int i=0; i<2; i++) fRec21[i] = 0;
		for (int i=0; i<2; i++) fRec20[i] = 0;
		for (int i=0; i<2; i++) fRec19[i] = 0;
		for (int i=0; i<2; i++) fRec18[i] = 0;
		for (int i=0; i<2; i++) fRec17[i] = 0;
		fslider5 = 5.0f;
		for (int i=0; i<2; i++) fRec23[i] = 0;
		for (int i=0; i<2; i++) iRec24[i] = 0;
		for (int i=0; i<2; i++) iRec25[i] = 0;
		fslider6 = 0.2f;
		fslider7 = 0.1f;
		fslider8 = 0.5f;
		for (int i=0; i<2; i++) fRec26[i] = 0;
		fslider9 = 0.1f;
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
		fConst3 = (0.5f * iConst0);
		for (int i=0; i<2; i++) fVec2[i] = 0;
		fConst4 = (1.0f / tanf((6283.185307179586f / float(iConst0))));
		fConst5 = (1 + fConst4);
		fConst6 = (1.0f / fConst5);
		fConst7 = (0 - ((1 - fConst4) / fConst5));
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
		faust_interface->openVerticalBox("Global_Envelope_Parameters");
		faust_interface->declare(&fslider1, "6", "");
		faust_interface->declare(&fslider1, "tooltip", "Global envelope attack duration");
		faust_interface->declare(&fslider1, "unit", "s");
		faust_interface->addHorizontalSlider("Glob_Env_Attack", &fslider1, 0.1f, 0.0f, 2.0f, 0.01f);
		faust_interface->declare(&fslider0, "6", "");
		faust_interface->declare(&fslider0, "tooltip", "Global envelope release duration");
		faust_interface->declare(&fslider0, "unit", "s");
		faust_interface->addHorizontalSlider("Glob_Env_Release", &fslider0, 0.1f, 0.0f, 2.0f, 0.01f);
		faust_interface->closeBox();
		faust_interface->openVerticalBox("Pressure_Envelope_Parameters");
		faust_interface->declare(&fslider14, "5", "");
		faust_interface->declare(&fslider14, "tooltip", "Pressure envelope attack duration");
		faust_interface->declare(&fslider14, "unit", "s");
		faust_interface->addHorizontalSlider("Press_Env_Attack", &fslider14, 0.05f, 0.0f, 2.0f, 0.01f);
		faust_interface->declare(&fslider13, "5", "");
		faust_interface->declare(&fslider13, "tooltip", "Pressure envelope decay duration");
		faust_interface->declare(&fslider13, "unit", "s");
		faust_interface->addHorizontalSlider("Press_Env_Decay", &fslider13, 0.2f, 0.0f, 2.0f, 0.01f);
		faust_interface->declare(&fslider12, "5", "");
		faust_interface->declare(&fslider12, "tooltip", "Pressure envelope release duration");
		faust_interface->declare(&fslider12, "unit", "s");
		faust_interface->addHorizontalSlider("Press_Env_Release", &fslider12, 1.0f, 0.0f, 2.0f, 0.01f);
		faust_interface->declare(&fcheckbox0, "5", "");
		faust_interface->declare(&fcheckbox0, "tooltip", "Activate Pressure envelope");
		faust_interface->declare(&fcheckbox0, "unit", "s");
		faust_interface->addCheckButton("Pressure_Env", &fcheckbox0);
		faust_interface->closeBox();
		faust_interface->openVerticalBox("Vibrato_Parameters");
		faust_interface->declare(&fslider8, "4", "");
		faust_interface->declare(&fslider8, "tooltip", "Vibrato attack duration");
		faust_interface->declare(&fslider8, "unit", "s");
		faust_interface->addHorizontalSlider("Vibrato_Attack", &fslider8, 0.5f, 0.0f, 2.0f, 0.01f);
		faust_interface->declare(&fslider7, "4", "");
		faust_interface->declare(&fslider7, "tooltip", "Vibrato silence duration before attack");
		faust_interface->declare(&fslider7, "unit", "s");
		faust_interface->addHorizontalSlider("Vibrato_Begin", &fslider7, 0.1f, 0.0f, 2.0f, 0.01f);
		faust_interface->declare(&fslider5, "4", "");
		faust_interface->declare(&fslider5, "unit", "Hz");
		faust_interface->addHorizontalSlider("Vibrato_Freq", &fslider5, 5.0f, 1.0f, 15.0f, 0.1f);
		faust_interface->declare(&fslider9, "4", "");
		faust_interface->declare(&fslider9, "tooltip", "A value between 0 and 1");
		faust_interface->addHorizontalSlider("Vibrato_Gain", &fslider9, 0.1f, 0.0f, 1.0f, 0.01f);
		faust_interface->declare(&fslider6, "4", "");
		faust_interface->declare(&fslider6, "tooltip", "Vibrato release duration");
		faust_interface->declare(&fslider6, "unit", "s");
		faust_interface->addHorizontalSlider("Vibrato_Release", &fslider6, 0.2f, 0.0f, 2.0f, 0.01f);
		faust_interface->closeBox();
		faust_interface->closeBox();
		faust_interface->openHorizontalBox("Physical_and_Nonlinearity");
		faust_interface->openVerticalBox("Nonlinear_Filter_Parameters");
		faust_interface->declare(&fslider4, "3", "");
		faust_interface->declare(&fslider4, "tooltip", "Frequency of the sine wave for the modulation of theta (works if Modulation Type=3)");
		faust_interface->declare(&fslider4, "unit", "Hz");
		faust_interface->addHorizontalSlider("Modulation_Frequency", &fslider4, 2.2e+02f, 2e+01f, 1e+03f, 0.1f);
		faust_interface->declare(&fentry2, "3", "");
		faust_interface->declare(&fentry2, "tooltip", "0=theta is modulated by the incoming signal; 1=theta is modulated by the averaged incoming signal; 2=theta is modulated by the squared incoming signal; 3=theta is modulated by a sine wave of frequency freqMod; 4=theta is modulated by a sine wave of frequency freq;");
		faust_interface->addNumEntry("Modulation_Type", &fentry2, 0.0f, 0.0f, 4.0f, 1.0f);
		faust_interface->declare(&fslider2, "3", "");
		faust_interface->declare(&fslider2, "tooltip", "Nonlinearity factor (value between 0 and 1)");
		faust_interface->addHorizontalSlider("Nonlinearity", &fslider2, 0.0f, 0.0f, 1.0f, 0.01f);
		faust_interface->declare(&fslider3, "3", "");
		faust_interface->declare(&fslider3, "Attack duration of the nonlinearity", "");
		faust_interface->declare(&fslider3, "unit", "s");
		faust_interface->addHorizontalSlider("Nonlinearity Attack", &fslider3, 0.1f, 0.0f, 2.0f, 0.01f);
		faust_interface->closeBox();
		faust_interface->openVerticalBox("Physical_Parameters");
		faust_interface->declare(&fslider10, "2", "");
		faust_interface->declare(&fslider10, "tooltip", "Breath noise gain (value between 0 and 1)");
		faust_interface->addHorizontalSlider("Noise Gain", &fslider10, 0.1f, 0.0f, 1.0f, 0.01f);
		faust_interface->declare(&fslider11, "2", "");
		faust_interface->declare(&fslider11, "tooltip", "Breath pressure (value bewteen 0 and 1)");
		faust_interface->addHorizontalSlider("Pressure", &fslider11, 0.9f, 0.0f, 1.5f, 0.01f);
		faust_interface->closeBox();
		faust_interface->closeBox();
		faust_interface->openVerticalBox("Spat");
		faust_interface->addHorizontalSlider("pan angle", &fslider15, 0.6f, 0.0f, 1.0f, 0.01f);
		faust_interface->addHorizontalSlider("spatial width", &fslider16, 0.5f, 0.0f, 1.0f, 0.01f);
		faust_interface->closeBox();
		faust_interface->closeBox();
	}
	virtual void compute (int count, FAUSTFLOAT** input, FAUSTFLOAT** output) {
		float 	fSlow0 = (0.0010000000000000009f * float(fentry0));
		int 	iSlow1 = int(float(fbutton0));
		int 	iSlow2 = (iSlow1 > 0);
		int 	iSlow3 = (iSlow1 <= 0);
		float 	fSlow4 = float(fslider0);
		float 	fSlow5 = (1 - (1.0f / powf(1e+05f,(1.0f / ((fSlow4 == 0.0f) + (iConst0 * fSlow4))))));
		float 	fSlow6 = float(fslider1);
		float 	fSlow7 = (1.0f / ((fSlow6 == 0.0f) + (iConst0 * fSlow6)));
		float 	fSlow8 = float(fentry1);
		float 	fSlow9 = (float(iConst0) / fSlow8);
		int 	iSlow10 = int((fSlow9 - 2));
		int 	iSlow11 = int((1 + int((int((1 + iSlow10)) & 4095))));
		float 	fSlow12 = (fSlow9 - (2 + iSlow10));
		int 	iSlow13 = int((1 + int((iSlow10 & 4095))));
		float 	fSlow14 = ((3 + iSlow10) - fSlow9);
		float 	fSlow15 = (0.0010000000000000009f * float(fslider2));
		float 	fSlow16 = float(fentry2);
		float 	fSlow17 = (3.141592653589793f * (fSlow16 == 2));
		float 	fSlow18 = (1.5707963267948966f * (fSlow16 == 1));
		float 	fSlow19 = (3.141592653589793f * (fSlow16 == 0));
		float 	fSlow20 = float(fslider3);
		float 	fSlow21 = (1.0f / ((fSlow20 == 0.0f) + (iConst0 * fSlow20)));
		int 	iSlow22 = (fSlow16 < 3);
		float 	fSlow23 = (0.0010000000000000009f * float(fslider4));
		int 	iSlow24 = (fSlow16 != 4);
		float 	fSlow25 = (fSlow8 * (fSlow16 == 4));
		int 	iSlow26 = (fSlow16 >= 3);
		float 	fSlow27 = (fConst2 * float(fslider5));
		float 	fSlow28 = float(fslider6);
		float 	fSlow29 = (1 - (1.0f / powf(1e+05f,(1.0f / ((fSlow28 == 0.0f) + (iConst0 * fSlow28))))));
		float 	fSlow30 = float(fslider7);
		float 	fSlow31 = (iConst0 * fSlow30);
		float 	fSlow32 = ((fSlow30 == 0.0f) + fSlow31);
		float 	fSlow33 = float(fslider8);
		float 	fSlow34 = (1.0f / ((fSlow33 == 0.0f) + (iConst0 * fSlow33)));
		float 	fSlow35 = float(fslider9);
		float 	fSlow36 = (5.122274162770377e-11f * float(fslider10));
		float 	fSlow37 = (0.0010000000000000009f * float(fslider11));
		int 	iSlow38 = (iSlow1 | int(float(fcheckbox0)));
		int 	iSlow39 = (iSlow38 > 0);
		int 	iSlow40 = (iSlow38 <= 0);
		float 	fSlow41 = float(fslider12);
		float 	fSlow42 = (1 - (1.0f / powf(9e+04f,(1.0f / ((fSlow41 == 0.0f) + (iConst0 * fSlow41))))));
		float 	fSlow43 = float(fslider13);
		float 	fSlow44 = (1 - powf(9e+01f,(1.0f / ((fSlow43 == 0.0f) + (iConst0 * fSlow43)))));
		float 	fSlow45 = float(fslider14);
		float 	fSlow46 = (1.0f / ((fSlow45 == 0.0f) + (iConst0 * fSlow45)));
		float 	fSlow47 = (fConst3 / fSlow8);
		int 	iSlow48 = int((fSlow47 - 2));
		int 	iSlow49 = int((int((1 + iSlow48)) & 4095));
		float 	fSlow50 = (fSlow47 - (2 + iSlow48));
		int 	iSlow51 = int((iSlow48 & 4095));
		float 	fSlow52 = ((3 + iSlow48) - fSlow47);
		float 	fSlow53 = float(fslider15);
		float 	fSlow54 = (0.5f * (1.0f - fSlow53));
		int 	iSlow55 = int((int((fConst3 * (float(fslider16) / fSlow8))) & 4095));
		float 	fSlow56 = (0.5f * fSlow53);
		FAUSTFLOAT* output0 = output[0];
		FAUSTFLOAT* output1 = output[1];
		for (int i=0; i<count; i++) {
			fRec0[0] = ((0.999f * fRec0[1]) + fSlow0);
			iRec1[0] = (iSlow2 & (iRec1[1] | (fRec2[1] >= 1)));
			int iTemp0 = (iSlow3 & (fRec2[1] > 0));
			fRec2[0] = (((fSlow7 * (((iRec1[1] == 0) & iSlow2) & (fRec2[1] < 1))) + (fRec2[1] * (1 - (fSlow5 * iTemp0)))) * ((iTemp0 == 0) | (fRec2[1] >= 1e-06f)));
			float fTemp1 = ((fSlow14 * fRec3[(IOTA-iSlow13)&8191]) + (fSlow12 * fRec3[(IOTA-iSlow11)&8191]));
			fVec0[0] = fTemp1;
			fRec5[0] = (fSlow15 + (0.999f * fRec5[1]));
			iRec6[0] = (iSlow2 & (iRec6[1] | (fRec7[1] >= 1)));
			int iTemp2 = (iSlow3 & (fRec7[1] > 0));
			fRec7[0] = (((fSlow21 * (((iRec6[1] == 0) & iSlow2) & (fRec7[1] < 1))) + (fRec7[1] * (1 - (fConst1 * iTemp2)))) * ((iTemp2 == 0) | (fRec7[1] >= 1e-06f)));
			float fTemp3 = (fRec5[0] * fRec7[0]);
			float fTemp4 = (fTemp3 * (((fSlow19 * fVec0[0]) + (fSlow18 * (fVec0[0] + fVec0[1]))) + (fSlow17 * faustpower<2>(fVec0[0]))));
			float fTemp5 = cosf(fTemp4);
			float fTemp6 = sinf(fTemp4);
			float fTemp7 = (0 - fTemp6);
			float fTemp8 = ((fRec8[1] * fTemp7) + (fVec0[0] * fTemp5));
			float fTemp9 = ((fTemp7 * fRec9[1]) + (fTemp5 * fTemp8));
			float fTemp10 = ((fTemp7 * fRec10[1]) + (fTemp5 * fTemp9));
			float fTemp11 = ((fTemp7 * fRec11[1]) + (fTemp5 * fTemp10));
			float fTemp12 = ((fTemp7 * fRec12[1]) + (fTemp5 * fTemp11));
			fRec13[0] = ((fTemp7 * fRec13[1]) + (fTemp5 * fTemp12));
			fRec12[0] = ((fTemp6 * fTemp12) + (fTemp5 * fRec13[1]));
			fRec11[0] = ((fTemp6 * fTemp11) + (fTemp5 * fRec12[1]));
			fRec10[0] = ((fTemp6 * fTemp10) + (fTemp5 * fRec11[1]));
			fRec9[0] = ((fTemp6 * fTemp9) + (fTemp5 * fRec10[1]));
			fRec8[0] = ((fTemp6 * fTemp8) + (fTemp5 * fRec9[1]));
			fRec16[0] = (fSlow23 + (0.999f * fRec16[1]));
			float fTemp13 = (fRec15[1] + (fConst2 * (fSlow25 + (iSlow24 * fRec16[0]))));
			fRec15[0] = (fTemp13 - floorf(fTemp13));
			float fTemp14 = (3.141592653589793f * (fTemp3 * ftbl0[int((65536.0f * fRec15[0]))]));
			float fTemp15 = cosf(fTemp14);
			float fTemp16 = sinf(fTemp14);
			float fTemp17 = (0 - fTemp16);
			float fTemp18 = ((fRec17[1] * fTemp17) + (fVec0[0] * fTemp15));
			float fTemp19 = ((fTemp17 * fRec18[1]) + (fTemp15 * fTemp18));
			float fTemp20 = ((fTemp17 * fRec19[1]) + (fTemp15 * fTemp19));
			float fTemp21 = ((fTemp17 * fRec20[1]) + (fTemp15 * fTemp20));
			float fTemp22 = ((fTemp17 * fRec21[1]) + (fTemp15 * fTemp21));
			fRec22[0] = ((fTemp17 * fRec22[1]) + (fTemp15 * fTemp22));
			fRec21[0] = ((fTemp16 * fTemp22) + (fTemp15 * fRec22[1]));
			fRec20[0] = ((fTemp16 * fTemp21) + (fTemp15 * fRec21[1]));
			fRec19[0] = ((fTemp16 * fTemp20) + (fTemp15 * fRec20[1]));
			fRec18[0] = ((fTemp16 * fTemp19) + (fTemp15 * fRec19[1]));
			fRec17[0] = ((fTemp16 * fTemp18) + (fTemp15 * fRec18[1]));
			float fTemp23 = (0.4f * ((iSlow26 * ((fVec0[0] * fTemp16) + (fRec17[1] * fTemp15))) + (iSlow22 * ((fRec5[0] * ((fVec0[0] * fTemp6) + (fRec8[1] * fTemp5))) + ((1 - fRec5[0]) * fVec0[0])))));
			float fTemp24 = (fRec23[1] + fSlow27);
			fRec23[0] = (fTemp24 - floorf(fTemp24));
			iRec24[0] = (iSlow2 & (iRec24[1] | (fRec26[1] >= 1)));
			iRec25[0] = (iSlow2 * (1 + iRec25[1]));
			int iTemp25 = (iSlow3 & (fRec26[1] > 0));
			fRec26[0] = (((fSlow34 * (((((iRec24[1] == 0) & iSlow2) & (fRec26[1] < 1)) & (iRec25[1] > fSlow31)) * (1 - (iRec25[1] < fSlow32)))) + (fRec26[1] * (1 - (fSlow29 * iTemp25)))) * ((iTemp25 == 0) | (fRec26[1] >= 1e-06f)));
			iRec27[0] = (12345 + (1103515245 * iRec27[1]));
			fRec28[0] = ((0.999f * fRec28[1]) + fSlow37);
			iRec29[0] = (iSlow39 & (iRec29[1] | (fRec30[1] >= 1)));
			int iTemp26 = (iSlow40 & (fRec30[1] > 0));
			fRec30[0] = (((fSlow46 * (((iRec29[1] == 0) & iSlow39) & (fRec30[1] < 1))) + (fRec30[1] * ((1 - (fSlow44 * (iRec29[1] & (fRec30[1] > 90)))) - (fSlow42 * iTemp26)))) * ((iTemp26 == 0) | (fRec30[1] >= 1e-06f)));
			float fTemp27 = ((((fRec30[0] * fRec28[0]) * (1.1f + (fSlow36 * iRec27[0]))) + (fSlow35 * (fRec26[0] * ftbl0[int((65536.0f * fRec23[0]))]))) + fTemp23);
			fVec1[IOTA&4095] = fTemp27;
			float fTemp28 = (fSlow50 * fVec1[(IOTA-iSlow49)&4095]);
			float fTemp29 = (fSlow52 * fVec1[(IOTA-iSlow51)&4095]);
			float fTemp30 = ((fTemp28 + (fTemp23 + fTemp29)) - faustpower<3>((fTemp29 + fTemp28)));
			fVec2[0] = fTemp30;
			fRec4[0] = ((fConst7 * fRec4[1]) + (fConst6 * (fVec2[0] + fVec2[1])));
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
			fRec26[1] = fRec26[0];
			iRec25[1] = iRec25[0];
			iRec24[1] = iRec24[0];
			fRec23[1] = fRec23[0];
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
			fVec0[1] = fVec0[0];
			fRec2[1] = fRec2[0];
			iRec1[1] = iRec1[0];
			fRec0[1] = fRec0[0];
		}
	}
};


float 	Flute_dsp::ftbl0[65536];


#include "Faust_plugins_template2.cpp"

