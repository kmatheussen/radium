//-----------------------------------------------------
// name: "Bass"
// author: "Romain Michon"
// copyright: "Romain Michon (rmichon@ccrma.stanford.edu)"
// version: "1.0"
//
// Code generated with Faust 0.9.73 (http://faust.grame.fr)
//-----------------------------------------------------
/* link with : "" */
#include <bass.h>
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
#define FAUSTCLASS Bass_dsp
#endif

class Bass_dsp : public dsp {
  private:
	class SIG0 {
	  private:
		int 	fSamplingFreq;
		int 	iRec18[2];
	  public:
		int getNumInputs() 	{ return 0; }
		int getNumOutputs() 	{ return 1; }
		void init(int samplingFreq) {
			fSamplingFreq = samplingFreq;
			for (int i=0; i<2; i++) iRec18[i] = 0;
		}
		void fill (int count, float output[]) {
			for (int i=0; i<count; i++) {
				iRec18[0] = (1 + iRec18[1]);
				output[i] = sinf((9.587379924285257e-05f * float((iRec18[0] - 1))));
				// post processing
				iRec18[1] = iRec18[0];
			}
		}
	};


	FAUSTFLOAT 	fbutton0;
	float 	fRec4[2];
	FAUSTFLOAT 	fslider0;
	int 	iConst0;
	float 	fConst1;
	float 	fConst2;
	FAUSTFLOAT 	fentry0;
	float 	fRec3[2];
	int 	iRec5[2];
	float 	fVec0[2];
	float 	fConst3;
	float 	fRec6[2];
	float 	fRec2[2];
	float 	fRec1[2];
	int 	iRec8[2];
	float 	fConst4;
	float 	fRec7[2];
	FAUSTFLOAT 	fentry1;
	float 	fConst5;
	float 	fRec10[2];
	float 	fVec1[2];
	FAUSTFLOAT 	fslider1;
	float 	fRec11[2];
	FAUSTFLOAT 	fentry2;
	float 	fRec17[2];
	float 	fRec16[2];
	float 	fRec15[2];
	float 	fRec14[2];
	float 	fRec13[2];
	float 	fRec12[2];
	static float 	ftbl0[65536];
	FAUSTFLOAT 	fslider2;
	float 	fRec20[2];
	float 	fConst6;
	float 	fRec19[2];
	float 	fRec26[2];
	float 	fRec25[2];
	float 	fRec24[2];
	float 	fRec23[2];
	float 	fRec22[2];
	float 	fRec21[2];
	float 	fVec2[2];
	float 	fRec9[2];
	int 	IOTA;
	float 	fRec0[8192];
	float 	fConst7;
	float 	fRec27[3];
	int 	iRec28[2];
	FAUSTFLOAT 	fslider3;
	FAUSTFLOAT 	fslider4;
	float 	fRec29[2];
	float 	fVec3[4096];
	FAUSTFLOAT 	fslider5;
	FAUSTFLOAT 	fslider6;
	float 	fConst8;
  public:
	static void metadata(Meta* m) 	{ 
		m->declare("name", "Bass");
		m->declare("description", "Nonlinear WaveGuide Acoustic Bass");
		m->declare("author", "Romain Michon");
		m->declare("copyright", "Romain Michon (rmichon@ccrma.stanford.edu)");
		m->declare("version", "1.0");
		m->declare("licence", "STK-4.3");
		m->declare("instrument.lib/name", "Faust-STK Tools Library");
		m->declare("instrument.lib/author", "Romain Michon (rmichon@ccrma.stanford.edu)");
		m->declare("instrument.lib/copyright", "Romain Michon");
		m->declare("instrument.lib/version", "1.0");
		m->declare("instrument.lib/licence", "STK-4.3");
		m->declare("math.lib/name", "Math Library");
		m->declare("math.lib/author", "GRAME");
		m->declare("math.lib/copyright", "GRAME");
		m->declare("math.lib/version", "1.0");
		m->declare("math.lib/license", "LGPL with exception");
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
		for (int i=0; i<2; i++) fRec4[i] = 0;
		fslider0 = 0.15f;
		iConst0 = min(192000, max(1, fSamplingFreq));
		fConst1 = float(iConst0);
		fConst2 = (3.5f / fConst1);
		fentry0 = 1.0f;
		for (int i=0; i<2; i++) fRec3[i] = 0;
		for (int i=0; i<2; i++) iRec5[i] = 0;
		for (int i=0; i<2; i++) fVec0[i] = 0;
		fConst3 = expf((0 - (3.5e+02f / fConst1)));
		for (int i=0; i<2; i++) fRec6[i] = 0;
		for (int i=0; i<2; i++) fRec2[i] = 0;
		for (int i=0; i<2; i++) fRec1[i] = 0;
		for (int i=0; i<2; i++) iRec8[i] = 0;
		fConst4 = expf((0 - (1.4e+02f / fConst1)));
		for (int i=0; i<2; i++) fRec7[i] = 0;
		fentry1 = 1.2e+02f;
		fConst5 = expf((0 - (7e+02f / fConst1)));
		for (int i=0; i<2; i++) fRec10[i] = 0;
		for (int i=0; i<2; i++) fVec1[i] = 0;
		fslider1 = 0.0f;
		for (int i=0; i<2; i++) fRec11[i] = 0;
		fentry2 = 0.0f;
		for (int i=0; i<2; i++) fRec17[i] = 0;
		for (int i=0; i<2; i++) fRec16[i] = 0;
		for (int i=0; i<2; i++) fRec15[i] = 0;
		for (int i=0; i<2; i++) fRec14[i] = 0;
		for (int i=0; i<2; i++) fRec13[i] = 0;
		for (int i=0; i<2; i++) fRec12[i] = 0;
		fslider2 = 2.2e+02f;
		for (int i=0; i<2; i++) fRec20[i] = 0;
		fConst6 = (1.0f / fConst1);
		for (int i=0; i<2; i++) fRec19[i] = 0;
		for (int i=0; i<2; i++) fRec26[i] = 0;
		for (int i=0; i<2; i++) fRec25[i] = 0;
		for (int i=0; i<2; i++) fRec24[i] = 0;
		for (int i=0; i<2; i++) fRec23[i] = 0;
		for (int i=0; i<2; i++) fRec22[i] = 0;
		for (int i=0; i<2; i++) fRec21[i] = 0;
		for (int i=0; i<2; i++) fVec2[i] = 0;
		for (int i=0; i<2; i++) fRec9[i] = 0;
		IOTA = 0;
		for (int i=0; i<8192; i++) fRec0[i] = 0;
		fConst7 = (0 - (1.994f * cosf((678.5840131753953f / float(iConst0)))));
		for (int i=0; i<3; i++) fRec27[i] = 0;
		for (int i=0; i<2; i++) iRec28[i] = 0;
		fslider3 = 0.1f;
		fslider4 = 0.0f;
		for (int i=0; i<2; i++) fRec29[i] = 0;
		for (int i=0; i<4096; i++) fVec3[i] = 0;
		fslider5 = 0.6f;
		fslider6 = 0.5f;
		fConst8 = (0.5f * fConst1);
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
		faust_interface->addNumEntry("freq", &fentry1, 1.2e+02f, 2e+01f, 2e+04f, 1.0f);
		faust_interface->declare(&fentry0, "1", "");
		faust_interface->declare(&fentry0, "tooltip", "Gain (value between 0 and 1)");
		faust_interface->addNumEntry("gain", &fentry0, 1.0f, 0.0f, 1.0f, 0.01f);
		faust_interface->declare(&fbutton0, "1", "");
		faust_interface->declare(&fbutton0, "tooltip", "noteOn = 1, noteOff = 0");
		faust_interface->addButton("gate", &fbutton0);
		faust_interface->closeBox();
		faust_interface->openHorizontalBox("Envelopes_and_Vibrato");
		faust_interface->openVerticalBox("Global_Envelope_Parameters");
		faust_interface->declare(&fslider4, "4", "");
		faust_interface->declare(&fslider4, "tooltip", "Global envelope attack duration");
		faust_interface->declare(&fslider4, "unit", "s");
		faust_interface->addHorizontalSlider("Glob_Env_Attack", &fslider4, 0.0f, 0.0f, 2.0f, 0.01f);
		faust_interface->declare(&fslider3, "4", "");
		faust_interface->declare(&fslider3, "tooltip", "Global envelope release duration");
		faust_interface->declare(&fslider3, "unit", "s");
		faust_interface->addHorizontalSlider("Glob_Env_Release", &fslider3, 0.1f, 0.0f, 2.0f, 0.01f);
		faust_interface->closeBox();
		faust_interface->closeBox();
		faust_interface->openVerticalBox("Nonlinear_Filter_Parameters");
		faust_interface->declare(&fslider2, "3", "");
		faust_interface->declare(&fslider2, "tooltip", "Frequency of the sine wave for the modulation of theta (works if Modulation Type=3)");
		faust_interface->declare(&fslider2, "unit", "Hz");
		faust_interface->addHorizontalSlider("Modulation_Frequency", &fslider2, 2.2e+02f, 2e+01f, 1e+03f, 0.1f);
		faust_interface->declare(&fentry2, "3", "");
		faust_interface->declare(&fentry2, "tooltip", "0=theta is modulated by the incoming signal; 1=theta is modulated by the averaged incoming signal; 2=theta is modulated by the squared incoming signal; 3=theta is modulated by a sine wave of frequency freqMod; 4=theta is modulated by a sine wave of frequency freq;");
		faust_interface->addNumEntry("Modulation_Type", &fentry2, 0.0f, 0.0f, 4.0f, 1.0f);
		faust_interface->declare(&fslider1, "3", "");
		faust_interface->declare(&fslider1, "tooltip", "Nonlinearity factor (value between 0 and 1)");
		faust_interface->addHorizontalSlider("Nonlinearity", &fslider1, 0.0f, 0.0f, 1.0f, 0.01f);
		faust_interface->closeBox();
		faust_interface->openVerticalBox("Physical_Parameters");
		faust_interface->declare(&fslider0, "2", "");
		faust_interface->declare(&fslider0, "tooltip", "A value between 0 and 1");
		faust_interface->addHorizontalSlider("Touch_Length", &fslider0, 0.15f, 0.0f, 1.0f, 0.01f);
		faust_interface->closeBox();
		faust_interface->openVerticalBox("Spat");
		faust_interface->addHorizontalSlider("pan angle", &fslider5, 0.6f, 0.0f, 1.0f, 0.01f);
		faust_interface->addHorizontalSlider("spatial width", &fslider6, 0.5f, 0.0f, 1.0f, 0.01f);
		faust_interface->closeBox();
		faust_interface->closeBox();
	}
	virtual void compute (int count, FAUSTFLOAT** input, FAUSTFLOAT** output) {
		float 	fSlow0 = float(fbutton0);
		int 	iSlow1 = (fSlow0 < 1);
		float 	fSlow2 = expf((0 - (fConst2 / float(fslider0))));
		int 	iSlow3 = (fSlow0 > 0);
		float 	fSlow4 = float(fentry0);
		int 	iSlow5 = (iSlow1 > 0);
		int 	iSlow6 = (iSlow1 < 1);
		float 	fSlow7 = float(fentry1);
		int 	iSlow8 = int(((17.31234049066756f * (logf(fSlow7) - 6.0867747269123065f)) + 69.5f));
		float 	fSlow9 = getValueBassLoopFiltera1(iSlow8);
		float 	fSlow10 = (fConst1 / fSlow7);
		float 	fSlow11 = (0.0010000000000000009f * float(fslider1));
		float 	fSlow12 = float(fentry2);
		float 	fSlow13 = (3.141592653589793f * (fSlow12 == 2));
		float 	fSlow14 = (1.5707963267948966f * (fSlow12 == 1));
		float 	fSlow15 = (3.141592653589793f * (fSlow12 == 0));
		int 	iSlow16 = (fSlow12 < 3);
		float 	fSlow17 = (0.0010000000000000009f * float(fslider2));
		int 	iSlow18 = (fSlow12 != 4);
		float 	fSlow19 = (fSlow7 * (fSlow12 == 4));
		int 	iSlow20 = (fSlow12 >= 3);
		float 	fSlow21 = getValueBassLoopFilterb1(iSlow8);
		float 	fSlow22 = getValueBassLoopFilterb0(iSlow8);
		int 	iSlow23 = (fSlow0 <= 0);
		float 	fSlow24 = float(fslider3);
		float 	fSlow25 = (1 - (1.0f / powf(1e+05f,(1.0f / ((fSlow24 == 0.0f) + (iConst0 * fSlow24))))));
		float 	fSlow26 = float(fslider4);
		float 	fSlow27 = (1.0f / ((fSlow26 == 0.0f) + (iConst0 * fSlow26)));
		float 	fSlow28 = float(fslider5);
		float 	fSlow29 = (2.0f * (1.0f - fSlow28));
		int 	iSlow30 = int((int((fConst8 * (float(fslider6) / fSlow7))) & 4095));
		float 	fSlow31 = (2.0f * fSlow28);
		FAUSTFLOAT* output0 = output[0];
		FAUSTFLOAT* output1 = output[1];
		for (int i=0; i<count; i++) {
			fRec4[0] = (1 + (fSlow0 * fRec4[1]));
			float fTemp0 = (fRec4[0] - 1);
			int iTemp1 = ((fTemp0 >= 2.0f) | iSlow1);
			int iTemp2 = ((fTemp0 < 2.0f) & iSlow3);
			float fTemp3 = (0.0301973834223185f * iTemp2);
			float fTemp4 = (fTemp3 + (fSlow2 * iTemp1));
			fRec3[0] = ((fRec3[1] * fTemp4) + (fSlow4 * (iTemp2 * (1 - fTemp4))));
			iRec5[0] = (12345 + (1103515245 * iRec5[1]));
			fVec0[0] = (iRec5[0] * fRec3[0]);
			int iTemp5 = (iTemp2 < 1);
			float fTemp6 = (fTemp3 + (fConst3 * iTemp1));
			fRec6[0] = ((fRec6[1] * fTemp6) + ((1 - fTemp6) * (0 - ((0.5f * iTemp2) + (0.985f * iTemp5)))));
			fRec2[0] = ((0.035f * ((4.656612875245797e-10f * (((1 + fRec6[0]) * iRec5[0]) * fRec3[0])) - (4.656612875245797e-10f * (fRec6[0] * fVec0[1])))) + (0.965f * fRec2[1]));
			fRec1[0] = ((0.035f * fRec2[0]) + (0.965f * fRec1[1]));
			iRec8[0] = (1 + (iSlow1 * iRec8[1]));
			int iTemp7 = (iRec8[0] - 1);
			int iTemp8 = ((iTemp7 < 2.0f) & iSlow5);
			float fTemp9 = ((0.0301973834223185f * iTemp8) + (fConst4 * ((iTemp7 >= 2.0f) | iSlow6)));
			fRec7[0] = ((fRec7[1] * fTemp9) + ((1 - fTemp9) * (iTemp8 + (0.9f * (iTemp8 < 1)))));
			float fTemp10 = (fTemp3 + (fConst5 * iTemp1));
			fRec10[0] = ((fRec10[1] * fTemp10) + (fSlow10 * (iTemp5 * (1 - fTemp10))));
			int iTemp11 = int(fRec10[0]);
			int iTemp12 = (1 + iTemp11);
			float fTemp13 = ((fRec0[(IOTA-int((1 + int((iTemp11 & 4095)))))&8191] * (iTemp12 - fRec10[0])) + ((fRec10[0] - iTemp11) * fRec0[(IOTA-int((1 + int((int(iTemp12) & 4095)))))&8191]));
			fVec1[0] = fTemp13;
			fRec11[0] = (fSlow11 + (0.999f * fRec11[1]));
			float fTemp14 = (fRec11[0] * (((fSlow15 * fVec1[0]) + (fSlow14 * (fVec1[0] + fVec1[1]))) + (fSlow13 * faustpower<2>(fVec1[0]))));
			float fTemp15 = cosf(fTemp14);
			float fTemp16 = sinf(fTemp14);
			float fTemp17 = (0 - fTemp16);
			float fTemp18 = ((fRec12[1] * fTemp17) + (fVec1[0] * fTemp15));
			float fTemp19 = ((fTemp17 * fRec13[1]) + (fTemp15 * fTemp18));
			float fTemp20 = ((fTemp17 * fRec14[1]) + (fTemp15 * fTemp19));
			float fTemp21 = ((fTemp17 * fRec15[1]) + (fTemp15 * fTemp20));
			float fTemp22 = ((fTemp17 * fRec16[1]) + (fTemp15 * fTemp21));
			fRec17[0] = ((fTemp17 * fRec17[1]) + (fTemp15 * fTemp22));
			fRec16[0] = ((fTemp16 * fTemp22) + (fTemp15 * fRec17[1]));
			fRec15[0] = ((fTemp16 * fTemp21) + (fTemp15 * fRec16[1]));
			fRec14[0] = ((fTemp16 * fTemp20) + (fTemp15 * fRec15[1]));
			fRec13[0] = ((fTemp16 * fTemp19) + (fTemp15 * fRec14[1]));
			fRec12[0] = ((fTemp16 * fTemp18) + (fTemp15 * fRec13[1]));
			fRec20[0] = (fSlow17 + (0.999f * fRec20[1]));
			float fTemp23 = (fRec19[1] + (fConst6 * (fSlow19 + (iSlow18 * fRec20[0]))));
			fRec19[0] = (fTemp23 - floorf(fTemp23));
			float fTemp24 = (3.141592653589793f * (fRec11[0] * ftbl0[int((65536.0f * fRec19[0]))]));
			float fTemp25 = cosf(fTemp24);
			float fTemp26 = sinf(fTemp24);
			float fTemp27 = (0 - fTemp26);
			float fTemp28 = ((fRec21[1] * fTemp27) + (fVec1[0] * fTemp25));
			float fTemp29 = ((fTemp27 * fRec22[1]) + (fTemp25 * fTemp28));
			float fTemp30 = ((fTemp27 * fRec23[1]) + (fTemp25 * fTemp29));
			float fTemp31 = ((fTemp27 * fRec24[1]) + (fTemp25 * fTemp30));
			float fTemp32 = ((fTemp27 * fRec25[1]) + (fTemp25 * fTemp31));
			fRec26[0] = ((fTemp27 * fRec26[1]) + (fTemp25 * fTemp32));
			fRec25[0] = ((fTemp26 * fTemp32) + (fTemp25 * fRec26[1]));
			fRec24[0] = ((fTemp26 * fTemp31) + (fTemp25 * fRec25[1]));
			fRec23[0] = ((fTemp26 * fTemp30) + (fTemp25 * fRec24[1]));
			fRec22[0] = ((fTemp26 * fTemp29) + (fTemp25 * fRec23[1]));
			fRec21[0] = ((fTemp26 * fTemp28) + (fTemp25 * fRec22[1]));
			float fTemp33 = ((iSlow20 * ((fVec1[0] * fTemp26) + (fRec21[1] * fTemp25))) + (iSlow16 * ((fRec11[0] * ((fVec1[0] * fTemp16) + (fRec12[1] * fTemp15))) + ((1 - fRec11[0]) * fVec1[0]))));
			fVec2[0] = fTemp33;
			fRec9[0] = (((fSlow22 * fVec2[0]) + (fSlow21 * fVec2[1])) - (fSlow9 * fRec9[1]));
			fRec0[IOTA&8191] = ((fRec9[0] * (fSlow0 + (iSlow1 * fRec7[0]))) + fRec1[0]);
			float fTemp34 = fRec0[(IOTA-0)&8191];
			fRec27[0] = (fTemp34 - ((fConst7 * fRec27[1]) + (0.994009f * fRec27[2])));
			iRec28[0] = (iSlow3 & (iRec28[1] | (fRec29[1] >= 1)));
			int iTemp35 = (iSlow23 & (fRec29[1] > 0));
			fRec29[0] = (((fSlow27 * (((iRec28[1] == 0) & iSlow3) & (fRec29[1] < 1))) + (fRec29[1] * (1 - (fSlow25 * iTemp35)))) * ((iTemp35 == 0) | (fRec29[1] >= 1e-06f)));
			float fTemp36 = (fRec29[0] * ((1.5f * ((0.0029954999999999843f * fRec27[0]) - (0.0029954999999999843f * fRec27[2]))) + (0.5f * fTemp34)));
			fVec3[IOTA&4095] = fTemp36;
			output0[i] = (FAUSTFLOAT)(fSlow29 * fVec3[IOTA&4095]);
			output1[i] = (FAUSTFLOAT)(fSlow31 * fVec3[(IOTA-iSlow30)&4095]);
			// post processing
			fRec29[1] = fRec29[0];
			iRec28[1] = iRec28[0];
			fRec27[2] = fRec27[1]; fRec27[1] = fRec27[0];
			IOTA = IOTA+1;
			fRec9[1] = fRec9[0];
			fVec2[1] = fVec2[0];
			fRec21[1] = fRec21[0];
			fRec22[1] = fRec22[0];
			fRec23[1] = fRec23[0];
			fRec24[1] = fRec24[0];
			fRec25[1] = fRec25[0];
			fRec26[1] = fRec26[0];
			fRec19[1] = fRec19[0];
			fRec20[1] = fRec20[0];
			fRec12[1] = fRec12[0];
			fRec13[1] = fRec13[0];
			fRec14[1] = fRec14[0];
			fRec15[1] = fRec15[0];
			fRec16[1] = fRec16[0];
			fRec17[1] = fRec17[0];
			fRec11[1] = fRec11[0];
			fVec1[1] = fVec1[0];
			fRec10[1] = fRec10[0];
			fRec7[1] = fRec7[0];
			iRec8[1] = iRec8[0];
			fRec1[1] = fRec1[0];
			fRec2[1] = fRec2[0];
			fRec6[1] = fRec6[0];
			fVec0[1] = fVec0[0];
			iRec5[1] = iRec5[0];
			fRec3[1] = fRec3[0];
			fRec4[1] = fRec4[0];
		}
	}
};


float 	Bass_dsp::ftbl0[65536];


#include "Faust_plugins_template2.cpp"

