//-----------------------------------------------------
// name: "Tuned Bar"
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
#define FAUSTCLASS Tuned_Bar_dsp
#endif

class Tuned_Bar_dsp : public dsp {
  private:
	class SIG0 {
	  private:
		int 	fSamplingFreq;
		int 	iRec22[2];
	  public:
		int getNumInputs() 	{ return 0; }
		int getNumOutputs() 	{ return 1; }
		void init(int samplingFreq) {
			fSamplingFreq = samplingFreq;
			for (int i=0; i<2; i++) iRec22[i] = 0;
		}
		void fill (int count, float output[]) {
			for (int i=0; i<count; i++) {
				iRec22[0] = (1 + iRec22[1]);
				output[i] = sinf((9.587379924285257e-05f * float((iRec22[0] - 1))));
				// post processing
				iRec22[1] = iRec22[0];
			}
		}
	};


	FAUSTFLOAT 	fbutton0;
	int 	iRec6[2];
	int 	iConst0;
	float 	fConst1;
	float 	fConst2;
	float 	fConst3;
	float 	fRec7[2];
	FAUSTFLOAT 	fentry0;
	float 	fRec8[2];
	FAUSTFLOAT 	fslider0;
	FAUSTFLOAT 	fslider1;
	FAUSTFLOAT 	fslider2;
	FAUSTFLOAT 	fentry1;
	int 	IOTA;
	float 	fVec0[4096];
	FAUSTFLOAT 	fentry2;
	float 	fConst4;
	float 	fConst5;
	float 	fConst6;
	float 	fConst7;
	float 	fRec5[3];
	float 	fConst8;
	float 	fConst9;
	float 	fConst10;
	float 	fRec4[2];
	float 	fRec0[2];
	float 	fVec1[4096];
	float 	fConst11;
	float 	fConst12;
	float 	fRec10[3];
	float 	fRec9[2];
	float 	fRec1[2];
	float 	fVec2[1024];
	float 	fConst13;
	float 	fConst14;
	float 	fRec12[3];
	float 	fRec11[2];
	float 	fRec2[2];
	float 	fVec3[1024];
	float 	fConst15;
	float 	fConst16;
	float 	fRec14[3];
	float 	fRec13[2];
	float 	fRec3[2];
	float 	fVec4[2];
	FAUSTFLOAT 	fslider3;
	float 	fRec15[2];
	FAUSTFLOAT 	fentry3;
	float 	fRec21[2];
	float 	fRec20[2];
	float 	fRec19[2];
	float 	fRec18[2];
	float 	fRec17[2];
	float 	fRec16[2];
	static float 	ftbl0[65536];
	FAUSTFLOAT 	fslider4;
	float 	fRec24[2];
	float 	fConst17;
	float 	fRec23[2];
	float 	fRec30[2];
	float 	fRec29[2];
	float 	fRec28[2];
	float 	fRec27[2];
	float 	fRec26[2];
	float 	fRec25[2];
	int 	iRec31[2];
	FAUSTFLOAT 	fslider5;
	FAUSTFLOAT 	fslider6;
	float 	fRec32[2];
	float 	fVec5[4096];
	FAUSTFLOAT 	fslider7;
	FAUSTFLOAT 	fslider8;
	float 	fConst18;
  public:
	static void metadata(Meta* m) 	{ 
		m->declare("name", "Tuned Bar");
		m->declare("description", "Nonlinear Banded Waveguide Models");
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
		fbutton0 = 0.0;
		for (int i=0; i<2; i++) iRec6[i] = 0;
		iConst0 = min(192000, max(1, fSamplingFreq));
		fConst1 = (1 - (1.0f / powf(9e+04f,(1e+02f / float(iConst0)))));
		fConst2 = (1 - powf(9e+01f,(2e+02f / float(iConst0))));
		fConst3 = (5e+01f / float(iConst0));
		for (int i=0; i<2; i++) fRec7[i] = 0;
		fentry0 = 0.8f;
		for (int i=0; i<2; i++) fRec8[i] = 0;
		fslider0 = 1.0f;
		fslider1 = 0.0f;
		fslider2 = 0.2f;
		fentry1 = 0.0f;
		IOTA = 0;
		for (int i=0; i<4096; i++) fVec0[i] = 0;
		fentry2 = 4.4e+02f;
		fConst4 = (1 - (100.53096491487338f / float(iConst0)));
		fConst5 = faustpower<2>(fConst4);
		fConst6 = (6.283185307179586f / float(iConst0));
		fConst7 = (0 - (2 * fConst4));
		for (int i=0; i<3; i++) fRec5[i] = 0;
		fConst8 = (0.5f * fConst5);
		fConst9 = (fConst8 - 0.5f);
		fConst10 = (0.5f - fConst8);
		for (int i=0; i<2; i++) fRec4[i] = 0;
		for (int i=0; i<2; i++) fRec0[i] = 0;
		for (int i=0; i<4096; i++) fVec1[i] = 0;
		fConst11 = (0.24876617314156196f * iConst0);
		fConst12 = (25.257394234239797f / float(iConst0));
		for (int i=0; i<3; i++) fRec10[i] = 0;
		for (int i=0; i<2; i++) fRec9[i] = 0;
		for (int i=0; i<2; i++) fRec1[i] = 0;
		for (int i=0; i<1024; i++) fVec2[i] = 0;
		fConst13 = (0.09329664832431377f * iConst0);
		fConst14 = (67.34631329239448f / float(iConst0));
		for (int i=0; i<3; i++) fRec12[i] = 0;
		for (int i=0; i<2; i++) fRec11[i] = 0;
		for (int i=0; i<2; i++) fRec2[i] = 0;
		for (int i=0; i<1024; i++) fVec3[i] = 0;
		fConst15 = (0.055341246290904644f * iConst0);
		fConst16 = (113.53530555043228f / float(iConst0));
		for (int i=0; i<3; i++) fRec14[i] = 0;
		for (int i=0; i<2; i++) fRec13[i] = 0;
		for (int i=0; i<2; i++) fRec3[i] = 0;
		for (int i=0; i<2; i++) fVec4[i] = 0;
		fslider3 = 0.0f;
		for (int i=0; i<2; i++) fRec15[i] = 0;
		fentry3 = 0.0f;
		for (int i=0; i<2; i++) fRec21[i] = 0;
		for (int i=0; i<2; i++) fRec20[i] = 0;
		for (int i=0; i<2; i++) fRec19[i] = 0;
		for (int i=0; i<2; i++) fRec18[i] = 0;
		for (int i=0; i<2; i++) fRec17[i] = 0;
		for (int i=0; i<2; i++) fRec16[i] = 0;
		fslider4 = 2.2e+02f;
		for (int i=0; i<2; i++) fRec24[i] = 0;
		fConst17 = (1.0f / float(iConst0));
		for (int i=0; i<2; i++) fRec23[i] = 0;
		for (int i=0; i<2; i++) fRec30[i] = 0;
		for (int i=0; i<2; i++) fRec29[i] = 0;
		for (int i=0; i<2; i++) fRec28[i] = 0;
		for (int i=0; i<2; i++) fRec27[i] = 0;
		for (int i=0; i<2; i++) fRec26[i] = 0;
		for (int i=0; i<2; i++) fRec25[i] = 0;
		for (int i=0; i<2; i++) iRec31[i] = 0;
		fslider5 = 0.1f;
		fslider6 = 0.02f;
		for (int i=0; i<2; i++) fRec32[i] = 0;
		for (int i=0; i<4096; i++) fVec5[i] = 0;
		fslider7 = 0.6f;
		fslider8 = 0.5f;
		fConst18 = (0.5f * iConst0);
	}
	virtual void init(int samplingFreq) {
		classInit(samplingFreq);
		instanceInit(samplingFreq);
	}
	virtual void buildUserInterface(UI* faust_interface) {
		faust_interface->openVerticalBox("0x00");
		faust_interface->openHorizontalBox("Basic_Parameters");
		faust_interface->declare(&fentry2, "1", "");
		faust_interface->declare(&fentry2, "tooltip", "Tone frequency");
		faust_interface->declare(&fentry2, "unit", "Hz");
		faust_interface->addNumEntry("freq", &fentry2, 4.4e+02f, 2e+01f, 2e+04f, 1.0f);
		faust_interface->declare(&fentry0, "1", "");
		faust_interface->declare(&fentry0, "tooltip", "Gain (value between 0 and 1)");
		faust_interface->addNumEntry("gain", &fentry0, 0.8f, 0.0f, 1.0f, 0.01f);
		faust_interface->declare(&fbutton0, "1", "");
		faust_interface->declare(&fbutton0, "tooltip", "noteOn = 1, noteOff = 0");
		faust_interface->addButton("gate", &fbutton0);
		faust_interface->closeBox();
		faust_interface->openHorizontalBox("Envelopes_and_Vibrato");
		faust_interface->openVerticalBox("Global_Envelope_Parameters");
		faust_interface->declare(&fslider6, "4", "");
		faust_interface->declare(&fslider6, "tooltip", "Global envelope attack duration");
		faust_interface->declare(&fslider6, "unit", "s");
		faust_interface->addHorizontalSlider("Glob_Env_Attack", &fslider6, 0.02f, 0.0f, 2.0f, 0.01f);
		faust_interface->declare(&fslider5, "4", "");
		faust_interface->declare(&fslider5, "tooltip", "Global envelope release duration");
		faust_interface->declare(&fslider5, "unit", "s");
		faust_interface->addHorizontalSlider("Glob_Env_Release", &fslider5, 0.1f, 0.0f, 2.0f, 0.01f);
		faust_interface->closeBox();
		faust_interface->closeBox();
		faust_interface->openHorizontalBox("Physical_and_Nonlinearity");
		faust_interface->openVerticalBox("Nonlinear_Filter_Parameters");
		faust_interface->declare(&fslider4, "3", "");
		faust_interface->declare(&fslider4, "tooltip", "Frequency of the sine wave for the modulation of theta (works if Modulation Type=3)");
		faust_interface->declare(&fslider4, "unit", "Hz");
		faust_interface->addHorizontalSlider("Modulation_Frequency", &fslider4, 2.2e+02f, 2e+01f, 1e+03f, 0.1f);
		faust_interface->declare(&fentry3, "3", "");
		faust_interface->declare(&fentry3, "tooltip", "0=theta is modulated by the incoming signal; 1=theta is modulated by the averaged incoming signal; 2=theta is modulated by the squared incoming signal; 3=theta is modulated by a sine wave of frequency freqMod; 4=theta is modulated by a sine wave of frequency freq;");
		faust_interface->addNumEntry("Modulation_Type", &fentry3, 0.0f, 0.0f, 4.0f, 1.0f);
		faust_interface->declare(&fslider3, "3", "");
		faust_interface->declare(&fslider3, "tooltip", "Nonlinearity factor (value between 0 and 1)");
		faust_interface->addHorizontalSlider("Nonlinearity", &fslider3, 0.0f, 0.0f, 1.0f, 0.01f);
		faust_interface->closeBox();
		faust_interface->openVerticalBox("Physical_Parameters");
		faust_interface->declare(&fslider0, "2", "");
		faust_interface->declare(&fslider0, "tooltip", "A value between 0 and 1");
		faust_interface->addHorizontalSlider("Base_Gain", &fslider0, 1.0f, 0.0f, 1.0f, 0.01f);
		faust_interface->declare(&fslider2, "2", "");
		faust_interface->declare(&fslider2, "tooltip", "Bow pressure on the instrument (Value between 0 and 1)");
		faust_interface->addHorizontalSlider("Bow_Pressure", &fslider2, 0.2f, 0.0f, 1.0f, 0.01f);
		faust_interface->declare(&fentry1, "2", "");
		faust_interface->declare(&fentry1, "tooltip", "0=Bow; 1=Strike");
		faust_interface->addNumEntry("Excitation_Selector", &fentry1, 0.0f, 0.0f, 1.0f, 1.0f);
		faust_interface->declare(&fslider1, "2", "");
		faust_interface->declare(&fslider1, "tooltip", "A value between 0 and 1");
		faust_interface->addHorizontalSlider("Integration_Constant", &fslider1, 0.0f, 0.0f, 1.0f, 0.01f);
		faust_interface->closeBox();
		faust_interface->closeBox();
		faust_interface->openVerticalBox("Spat");
		faust_interface->addHorizontalSlider("pan angle", &fslider7, 0.6f, 0.0f, 1.0f, 0.01f);
		faust_interface->addHorizontalSlider("spatial width", &fslider8, 0.5f, 0.0f, 1.0f, 0.01f);
		faust_interface->closeBox();
		faust_interface->closeBox();
	}
	virtual void compute (int count, FAUSTFLOAT** input, FAUSTFLOAT** output) {
		float 	fSlow0 = float(fbutton0);
		int 	iSlow1 = (fSlow0 > 0);
		int 	iSlow2 = (fSlow0 <= 0);
		float 	fSlow3 = (0.0010000000000000009f * float(fentry0));
		float 	fSlow4 = (0.8999999999999999f + (0.1f * float(fslider0)));
		float 	fSlow5 = float(fslider1);
		float 	fSlow6 = (10 - (9 * float(fslider2)));
		float 	fSlow7 = float(fentry1);
		float 	fSlow8 = (0 - (fSlow7 - 1));
		float 	fSlow9 = (fSlow0 * fSlow7);
		float 	fSlow10 = float(fentry2);
		int 	iSlow11 = int((int((float(iConst0) / fSlow10)) & 4095));
		float 	fSlow12 = (fConst7 * cosf((fConst6 * fSlow10)));
		int 	iSlow13 = int((int((fConst11 / fSlow10)) & 4095));
		float 	fSlow14 = (fConst7 * cosf((fConst12 * fSlow10)));
		int 	iSlow15 = int((int((fConst13 / fSlow10)) & 4095));
		float 	fSlow16 = (fConst7 * cosf((fConst14 * fSlow10)));
		int 	iSlow17 = int((int((fConst15 / fSlow10)) & 4095));
		float 	fSlow18 = (fConst7 * cosf((fConst16 * fSlow10)));
		float 	fSlow19 = (0.0010000000000000009f * float(fslider3));
		float 	fSlow20 = float(fentry3);
		float 	fSlow21 = (50.26548245743669f * (fSlow20 == 2));
		float 	fSlow22 = (6.283185307179586f * (fSlow20 == 1));
		float 	fSlow23 = (12.566370614359172f * (fSlow20 == 0));
		int 	iSlow24 = (fSlow20 < 3);
		float 	fSlow25 = (0.0010000000000000009f * float(fslider4));
		int 	iSlow26 = (fSlow20 != 4);
		float 	fSlow27 = (fSlow10 * (fSlow20 == 4));
		int 	iSlow28 = (fSlow20 >= 3);
		float 	fSlow29 = float(fslider5);
		float 	fSlow30 = (1 - (1.0f / powf(1e+05f,(1.0f / ((fSlow29 == 0.0f) + (iConst0 * fSlow29))))));
		float 	fSlow31 = float(fslider6);
		float 	fSlow32 = (1.0f / ((fSlow31 == 0.0f) + (iConst0 * fSlow31)));
		float 	fSlow33 = float(fslider7);
		float 	fSlow34 = (0.5f * (1.0f - fSlow33));
		int 	iSlow35 = int((int((fConst18 * (float(fslider8) / fSlow10))) & 4095));
		float 	fSlow36 = (0.5f * fSlow33);
		FAUSTFLOAT* output0 = output[0];
		FAUSTFLOAT* output1 = output[1];
		for (int i=0; i<count; i++) {
			iRec6[0] = (iSlow1 & (iRec6[1] | (fRec7[1] >= 1)));
			int iTemp0 = (iSlow2 & (fRec7[1] > 0));
			fRec7[0] = (((fConst3 * (((iRec6[1] == 0) & iSlow1) & (fRec7[1] < 1))) + (fRec7[1] * ((1 - (fConst2 * (iRec6[1] & (fRec7[1] > 90)))) - (fConst1 * iTemp0)))) * ((iTemp0 == 0) | (fRec7[1] >= 1e-06f)));
			fRec8[0] = ((0.999f * fRec8[1]) + fSlow3);
			float fTemp1 = (0 - ((fSlow5 + (fSlow4 * ((fRec0[1] + fRec2[1]) + (fRec1[1] + fRec3[1])))) - ((0.03f + (0.1f * fRec8[0])) * fRec7[0])));
			float fTemp2 = faustpower<4>((0.75f + fabsf((fSlow6 * fTemp1))));
			float fTemp3 = (1.0f / fTemp2);
			float fTemp4 = (fSlow8 * (fTemp1 * ((fTemp3 > 1) + (float((fTemp3 <= 1)) / fTemp2))));
			float fTemp5 = (fSlow9 * fRec8[0]);
			fVec0[IOTA&4095] = (fTemp5 + (fTemp4 + (4.0f * fRec4[1])));
			fRec5[0] = (0 - (((fSlow12 * fRec5[1]) + (fConst5 * fRec5[2])) - (0.24975f * fVec0[(IOTA-iSlow11)&4095])));
			fRec4[0] = ((fConst10 * fRec5[0]) + (fConst9 * fRec5[2]));
			fRec0[0] = fRec4[0];
			float fTemp6 = (fTemp4 + fTemp5);
			fVec1[IOTA&4095] = (fTemp6 + (4.0f * fRec9[1]));
			fRec10[0] = (0 - (((fSlow14 * fRec10[1]) + (fConst5 * fRec10[2])) - (0.24950025f * fVec1[(IOTA-iSlow13)&4095])));
			fRec9[0] = ((fConst10 * fRec10[0]) + (fConst9 * fRec10[2]));
			fRec1[0] = fRec9[0];
			fVec2[IOTA&1023] = (fTemp6 + (4.0f * fRec11[1]));
			fRec12[0] = (0 - (((fSlow16 * fRec12[1]) + (fConst5 * fRec12[2])) - (0.24925074975f * fVec2[(IOTA-iSlow15)&1023])));
			fRec11[0] = ((fConst10 * fRec12[0]) + (fConst9 * fRec12[2]));
			fRec2[0] = fRec11[0];
			fVec3[IOTA&1023] = (fTemp6 + (4.0f * fRec13[1]));
			fRec14[0] = (0 - (((fSlow18 * fRec14[1]) + (fConst5 * fRec14[2])) - (0.24900149900025f * fVec3[(IOTA-iSlow17)&1023])));
			fRec13[0] = ((fConst10 * fRec14[0]) + (fConst9 * fRec14[2]));
			fRec3[0] = fRec13[0];
			float fTemp7 = (fRec3[0] + ((fRec0[0] + fRec2[0]) + fRec1[0]));
			fVec4[0] = fTemp7;
			fRec15[0] = (fSlow19 + (0.999f * fRec15[1]));
			float fTemp8 = (fRec15[0] * (((fSlow23 * fVec4[0]) + (fSlow22 * (fVec4[0] + fVec4[1]))) + (fSlow21 * faustpower<2>(fVec4[0]))));
			float fTemp9 = cosf(fTemp8);
			float fTemp10 = sinf(fTemp8);
			float fTemp11 = (0 - fTemp10);
			float fTemp12 = ((fRec16[1] * fTemp11) + (4 * (fVec4[0] * fTemp9)));
			float fTemp13 = ((fTemp11 * fRec17[1]) + (fTemp9 * fTemp12));
			float fTemp14 = ((fTemp11 * fRec18[1]) + (fTemp9 * fTemp13));
			float fTemp15 = ((fTemp11 * fRec19[1]) + (fTemp9 * fTemp14));
			float fTemp16 = ((fTemp11 * fRec20[1]) + (fTemp9 * fTemp15));
			fRec21[0] = ((fTemp11 * fRec21[1]) + (fTemp9 * fTemp16));
			fRec20[0] = ((fTemp10 * fTemp16) + (fTemp9 * fRec21[1]));
			fRec19[0] = ((fTemp10 * fTemp15) + (fTemp9 * fRec20[1]));
			fRec18[0] = ((fTemp10 * fTemp14) + (fTemp9 * fRec19[1]));
			fRec17[0] = ((fTemp10 * fTemp13) + (fTemp9 * fRec18[1]));
			fRec16[0] = ((fTemp10 * fTemp12) + (fTemp9 * fRec17[1]));
			fRec24[0] = (fSlow25 + (0.999f * fRec24[1]));
			float fTemp17 = (fRec23[1] + (fConst17 * (fSlow27 + (iSlow26 * fRec24[0]))));
			fRec23[0] = (fTemp17 - floorf(fTemp17));
			float fTemp18 = (3.141592653589793f * (fRec15[0] * ftbl0[int((65536.0f * fRec23[0]))]));
			float fTemp19 = cosf(fTemp18);
			float fTemp20 = sinf(fTemp18);
			float fTemp21 = (0 - fTemp20);
			float fTemp22 = ((fRec25[1] * fTemp21) + (4 * (fVec4[0] * fTemp19)));
			float fTemp23 = ((fTemp21 * fRec26[1]) + (fTemp19 * fTemp22));
			float fTemp24 = ((fTemp21 * fRec27[1]) + (fTemp19 * fTemp23));
			float fTemp25 = ((fTemp21 * fRec28[1]) + (fTemp19 * fTemp24));
			float fTemp26 = ((fTemp21 * fRec29[1]) + (fTemp19 * fTemp25));
			fRec30[0] = ((fTemp21 * fRec30[1]) + (fTemp19 * fTemp26));
			fRec29[0] = ((fTemp20 * fTemp26) + (fTemp19 * fRec30[1]));
			fRec28[0] = ((fTemp20 * fTemp25) + (fTemp19 * fRec29[1]));
			fRec27[0] = ((fTemp20 * fTemp24) + (fTemp19 * fRec28[1]));
			fRec26[0] = ((fTemp20 * fTemp23) + (fTemp19 * fRec27[1]));
			fRec25[0] = ((fTemp20 * fTemp22) + (fTemp19 * fRec26[1]));
			iRec31[0] = (iSlow1 & (iRec31[1] | (fRec32[1] >= 1)));
			int iTemp27 = (iSlow2 & (fRec32[1] > 0));
			fRec32[0] = (((fSlow32 * (((iRec31[1] == 0) & iSlow1) & (fRec32[1] < 1))) + (fRec32[1] * (1 - (fSlow30 * iTemp27)))) * ((iTemp27 == 0) | (fRec32[1] >= 1e-06f)));
			float fTemp28 = (fRec32[0] * ((iSlow28 * ((4 * (fVec4[0] * fTemp20)) + (fRec25[1] * fTemp19))) + (iSlow24 * ((fRec15[0] * ((4 * (fVec4[0] * fTemp10)) + (fRec16[1] * fTemp9))) + (4 * ((1 - fRec15[0]) * fVec4[0]))))));
			fVec5[IOTA&4095] = fTemp28;
			output0[i] = (FAUSTFLOAT)(fSlow34 * fVec5[IOTA&4095]);
			output1[i] = (FAUSTFLOAT)(fSlow36 * fVec5[(IOTA-iSlow35)&4095]);
			// post processing
			fRec32[1] = fRec32[0];
			iRec31[1] = iRec31[0];
			fRec25[1] = fRec25[0];
			fRec26[1] = fRec26[0];
			fRec27[1] = fRec27[0];
			fRec28[1] = fRec28[0];
			fRec29[1] = fRec29[0];
			fRec30[1] = fRec30[0];
			fRec23[1] = fRec23[0];
			fRec24[1] = fRec24[0];
			fRec16[1] = fRec16[0];
			fRec17[1] = fRec17[0];
			fRec18[1] = fRec18[0];
			fRec19[1] = fRec19[0];
			fRec20[1] = fRec20[0];
			fRec21[1] = fRec21[0];
			fRec15[1] = fRec15[0];
			fVec4[1] = fVec4[0];
			fRec3[1] = fRec3[0];
			fRec13[1] = fRec13[0];
			fRec14[2] = fRec14[1]; fRec14[1] = fRec14[0];
			fRec2[1] = fRec2[0];
			fRec11[1] = fRec11[0];
			fRec12[2] = fRec12[1]; fRec12[1] = fRec12[0];
			fRec1[1] = fRec1[0];
			fRec9[1] = fRec9[0];
			fRec10[2] = fRec10[1]; fRec10[1] = fRec10[0];
			fRec0[1] = fRec0[0];
			fRec4[1] = fRec4[0];
			fRec5[2] = fRec5[1]; fRec5[1] = fRec5[0];
			IOTA = IOTA+1;
			fRec8[1] = fRec8[0];
			fRec7[1] = fRec7[0];
			iRec6[1] = iRec6[0];
		}
	}
};


float 	Tuned_Bar_dsp::ftbl0[65536];


#include "Faust_plugins_template2.cpp"

