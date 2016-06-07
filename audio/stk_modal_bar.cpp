//-----------------------------------------------------
// name: "Modal Bar"
// author: "Romain Michon (rmichon@ccrma.stanford.edu)"
// copyright: "Romain Michon"
// version: "1.0"
//
// Code generated with Faust 0.9.73 (http://faust.grame.fr)
//-----------------------------------------------------
/* link with : "" */
#include <instrument.h>
#include <math.h>
#include <modalBar.h>
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
				output[i] = readMarmstk1(int(((iRec5[0] - 1) % 246)));
				// post processing
				iRec5[1] = iRec5[0];
			}
		}
	};


	static float 	ftbl0[65536];
	FAUSTFLOAT 	fslider0;
	int 	iConst0;
	float 	fConst1;
	float 	fRec1[2];
	FAUSTFLOAT 	fslider1;
	FAUSTFLOAT 	fentry0;
	float 	fRec3[2];
	static float 	ftbl1[246];
	FAUSTFLOAT 	fslider2;
	float 	fRec6[2];
	FAUSTFLOAT 	fbutton0;
	float 	fRec7[2];
	float 	fRec4[2];
	FAUSTFLOAT 	fentry1;
	float 	fRec8[2];
	FAUSTFLOAT 	fentry2;
	FAUSTFLOAT 	fentry3;
	float 	fConst2;
	float 	fRec2[3];
	float 	fRec10[2];
	float 	fRec11[2];
	float 	fRec9[3];
	float 	fRec13[2];
	float 	fRec14[2];
	float 	fRec12[3];
	float 	fRec16[2];
	float 	fRec17[2];
	float 	fRec15[3];
	FAUSTFLOAT 	fslider3;
	float 	fRec18[2];
	FAUSTFLOAT 	fentry4;
	float 	fVec0[2];
	float 	fRec24[2];
	float 	fRec23[2];
	float 	fRec22[2];
	float 	fRec21[2];
	float 	fRec20[2];
	float 	fRec19[2];
	FAUSTFLOAT 	fslider4;
	float 	fRec26[2];
	float 	fRec25[2];
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
		SIG1 sig1;
		sig1.init(samplingFreq);
		sig1.fill(246,ftbl1);
	}
	virtual void instanceInit(int samplingFreq) {
		fSamplingFreq = samplingFreq;
		fslider0 = 6.0f;
		iConst0 = min(192000, max(1, fSamplingFreq));
		fConst1 = (1.0f / float(iConst0));
		for (int i=0; i<2; i++) fRec1[i] = 0;
		fslider1 = 0.1f;
		fentry0 = 1.0f;
		for (int i=0; i<2; i++) fRec3[i] = 0;
		fslider2 = 0.25f;
		for (int i=0; i<2; i++) fRec6[i] = 0;
		fbutton0 = 0.0;
		for (int i=0; i<2; i++) fRec7[i] = 0;
		for (int i=0; i<2; i++) fRec4[i] = 0;
		fentry1 = 0.8f;
		for (int i=0; i<2; i++) fRec8[i] = 0;
		fentry2 = 1.0f;
		fentry3 = 4.4e+02f;
		fConst2 = (6.283185307179586f / float(iConst0));
		for (int i=0; i<3; i++) fRec2[i] = 0;
		for (int i=0; i<2; i++) fRec10[i] = 0;
		for (int i=0; i<2; i++) fRec11[i] = 0;
		for (int i=0; i<3; i++) fRec9[i] = 0;
		for (int i=0; i<2; i++) fRec13[i] = 0;
		for (int i=0; i<2; i++) fRec14[i] = 0;
		for (int i=0; i<3; i++) fRec12[i] = 0;
		for (int i=0; i<2; i++) fRec16[i] = 0;
		for (int i=0; i<2; i++) fRec17[i] = 0;
		for (int i=0; i<3; i++) fRec15[i] = 0;
		fslider3 = 0.0f;
		for (int i=0; i<2; i++) fRec18[i] = 0;
		fentry4 = 0.0f;
		for (int i=0; i<2; i++) fVec0[i] = 0;
		for (int i=0; i<2; i++) fRec24[i] = 0;
		for (int i=0; i<2; i++) fRec23[i] = 0;
		for (int i=0; i<2; i++) fRec22[i] = 0;
		for (int i=0; i<2; i++) fRec21[i] = 0;
		for (int i=0; i<2; i++) fRec20[i] = 0;
		for (int i=0; i<2; i++) fRec19[i] = 0;
		fslider4 = 2.2e+02f;
		for (int i=0; i<2; i++) fRec26[i] = 0;
		for (int i=0; i<2; i++) fRec25[i] = 0;
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
	virtual void buildUserInterface(UI* faust_interface) {
		faust_interface->openVerticalBox("0x00");
		faust_interface->openHorizontalBox("Basic_Parameters");
		faust_interface->declare(&fentry3, "1", "");
		faust_interface->declare(&fentry3, "tooltip", "Tone frequency");
		faust_interface->declare(&fentry3, "unit", "Hz");
		faust_interface->addNumEntry("freq", &fentry3, 4.4e+02f, 2e+01f, 2e+04f, 1.0f);
		faust_interface->declare(&fentry1, "1", "");
		faust_interface->declare(&fentry1, "tooltip", "Gain (value between 0 and 1)");
		faust_interface->addNumEntry("gain", &fentry1, 0.8f, 0.0f, 1.0f, 0.01f);
		faust_interface->declare(&fbutton0, "1", "");
		faust_interface->declare(&fbutton0, "tooltip", "noteOn = 1, noteOff = 0");
		faust_interface->addButton("gate", &fbutton0);
		faust_interface->closeBox();
		faust_interface->openHorizontalBox("Envelopes_and_Vibrato");
		faust_interface->openVerticalBox("Global_Envelope_Parameters");
		faust_interface->declare(&fslider6, "5", "");
		faust_interface->declare(&fslider6, "tooltip", "Global envelope attack duration");
		faust_interface->declare(&fslider6, "unit", "s");
		faust_interface->addHorizontalSlider("Glob_Env_Attack", &fslider6, 0.0f, 0.0f, 2.0f, 0.01f);
		faust_interface->declare(&fslider5, "5", "");
		faust_interface->declare(&fslider5, "tooltip", "Global envelope release duration");
		faust_interface->declare(&fslider5, "unit", "s");
		faust_interface->addHorizontalSlider("Glob_Env_Release", &fslider5, 0.05f, 0.0f, 2.0f, 0.01f);
		faust_interface->closeBox();
		faust_interface->openVerticalBox("Vibrato_Parameters");
		faust_interface->declare(&fslider0, "4", "");
		faust_interface->declare(&fslider0, "unit", "Hz");
		faust_interface->addHorizontalSlider("Vibrato_Freq", &fslider0, 6.0f, 1.0f, 15.0f, 0.1f);
		faust_interface->declare(&fslider1, "4", "");
		faust_interface->declare(&fslider1, "tooltip", "A value between 0 and 1");
		faust_interface->addHorizontalSlider("Vibrato_Gain", &fslider1, 0.1f, 0.0f, 1.0f, 0.01f);
		faust_interface->closeBox();
		faust_interface->closeBox();
		faust_interface->openHorizontalBox("Physical_and_Nonlinearity");
		faust_interface->openVerticalBox("Nonlinear_Filter_Parameters");
		faust_interface->declare(&fslider4, "3", "");
		faust_interface->declare(&fslider4, "tooltip", "Frequency of the sine wave for the modulation of theta (works if Modulation Type=3)");
		faust_interface->declare(&fslider4, "unit", "Hz");
		faust_interface->addHorizontalSlider("Modulation_Frequency", &fslider4, 2.2e+02f, 2e+01f, 1e+03f, 0.1f);
		faust_interface->declare(&fentry4, "3", "");
		faust_interface->declare(&fentry4, "tooltip", "0=theta is modulated by the incoming signal; 1=theta is modulated by the averaged incoming signal; 2=theta is modulated by the squared incoming signal; 3=theta is modulated by a sine wave of frequency freqMod; 4=theta is modulated by a sine wave of frequency freq;");
		faust_interface->addNumEntry("Modulation_Type", &fentry4, 0.0f, 0.0f, 4.0f, 1.0f);
		faust_interface->declare(&fslider3, "3", "");
		faust_interface->declare(&fslider3, "tooltip", "Nonlinearity factor (value between 0 and 1)");
		faust_interface->addHorizontalSlider("Nonlinearity", &fslider3, 0.0f, 0.0f, 1.0f, 0.01f);
		faust_interface->closeBox();
		faust_interface->openVerticalBox("Physical_Parameters");
		faust_interface->declare(&fentry0, "2", "");
		faust_interface->declare(&fentry0, "tooltip", "0->Marimba, 1->Vibraphone, 2->Agogo, 3->Wood1, 4->Reso, 5->Wood2, 6->Beats, 7->2Fix; 8->Clump");
		faust_interface->addNumEntry("Preset", &fentry0, 1.0f, 0.0f, 8.0f, 1.0f);
		faust_interface->declare(&fentry2, "2", "");
		faust_interface->declare(&fentry2, "tooltip", "A value between 0 and 1");
		faust_interface->addNumEntry("Resonance", &fentry2, 1.0f, 0.0f, 1.0f, 1.0f);
		faust_interface->declare(&fslider2, "2", "");
		faust_interface->declare(&fslider2, "tooltip", "A value between 0 and 1");
		faust_interface->addHorizontalSlider("Stick_Hardness", &fslider2, 0.25f, 0.0f, 1.0f, 0.01f);
		faust_interface->closeBox();
		faust_interface->closeBox();
		faust_interface->openVerticalBox("Spat");
		faust_interface->addHorizontalSlider("pan angle", &fslider7, 0.6f, 0.0f, 1.0f, 0.01f);
		faust_interface->addHorizontalSlider("spatial width", &fslider8, 0.5f, 0.0f, 1.0f, 0.01f);
		faust_interface->closeBox();
		faust_interface->closeBox();
	}
	virtual void compute (int count, FAUSTFLOAT** input, FAUSTFLOAT** output) {
		float 	fSlow0 = (fConst1 * float(fslider0));
		float 	fSlow1 = float(fentry0);
		float 	fSlow2 = ((fSlow1 == 1) * float(fslider1));
		float 	fSlow3 = (0.0010000000000000009f * loadPreset(fSlow1, 2, 1));
		float 	fSlow4 = powf(4,float(fslider2));
		float 	fSlow5 = (0.25f * fSlow4);
		float 	fSlow6 = float(fbutton0);
		float 	fSlow7 = (246.0f * fSlow6);
		float 	fSlow8 = (61.5f * fSlow4);
		float 	fSlow9 = (0.09999999999999998f * fSlow6);
		float 	fSlow10 = float(fentry1);
		float 	fSlow11 = (1 - (0.03f * (fSlow10 * ((fSlow6 < 1) & (float(fentry2) != 1)))));
		float 	fSlow12 = faustpower<2>(fSlow11);
		float 	fSlow13 = float(fentry3);
		float 	fSlow14 = (2 * fSlow11);
		float 	fSlow15 = (0.0010000000000000009f * loadPreset(fSlow1, 2, 0));
		float 	fSlow16 = (0.0010000000000000009f * loadPreset(fSlow1, 2, 2));
		float 	fSlow17 = (0.0010000000000000009f * loadPreset(fSlow1, 2, 3));
		float 	fSlow18 = loadPreset(fSlow1, 3, 2);
		float 	fSlow19 = (0.0010000000000000009f * float(fslider3));
		float 	fSlow20 = float(fentry4);
		float 	fSlow21 = (3.141592653589793f * (fSlow20 == 2));
		float 	fSlow22 = (1.5707963267948966f * (fSlow20 == 1));
		float 	fSlow23 = (3.141592653589793f * (fSlow20 == 0));
		int 	iSlow24 = (fSlow20 < 3);
		float 	fSlow25 = (0.0010000000000000009f * float(fslider4));
		int 	iSlow26 = (fSlow20 != 4);
		float 	fSlow27 = (fSlow13 * (fSlow20 == 4));
		int 	iSlow28 = (fSlow20 >= 3);
		int 	iSlow29 = (fSlow6 > 0);
		int 	iSlow30 = (fSlow6 <= 0);
		float 	fSlow31 = float(fslider5);
		float 	fSlow32 = (1 - (1.0f / powf(1e+05f,(1.0f / ((fSlow31 == 0.0f) + (iConst0 * fSlow31))))));
		float 	fSlow33 = float(fslider6);
		float 	fSlow34 = (1.0f / ((fSlow33 == 0.0f) + (iConst0 * fSlow33)));
		float 	fSlow35 = float(fslider7);
		float 	fSlow36 = (0.3f * (1.0f - fSlow35));
		int 	iSlow37 = int((int((fConst3 * (float(fslider8) / fSlow13))) & 4095));
		float 	fSlow38 = (0.3f * fSlow35);
		FAUSTFLOAT* output0 = output[0];
		FAUSTFLOAT* output1 = output[1];
		for (int i=0; i<count; i++) {
			float fTemp0 = (fRec1[1] + fSlow0);
			fRec1[0] = (fTemp0 - floorf(fTemp0));
			float fTemp1 = (1 + (fSlow2 * ftbl0[int((65536.0f * fRec1[0]))]));
			fRec3[0] = ((0.999f * fRec3[1]) + fSlow3);
			float fTemp2 = (fSlow5 + fRec6[1]);
			fRec6[0] = (fTemp2 - floorf(fTemp2));
			fRec7[0] = (1 + (fSlow6 * fRec7[1]));
			fRec4[0] = ((fSlow9 * (((fRec7[0] - 1) < fSlow8) * ftbl1[int((fSlow7 * fRec6[0]))])) + (0.9f * fRec4[1]));
			fRec8[0] = (0.0010000000000000009f + (0.999f * fRec8[1]));
			float fTemp3 = loadPreset(fSlow1, 1, fRec8[0]);
			float fTemp4 = loadPreset(fSlow1, 0, fRec8[0]);
			int iTemp5 = (fTemp4 < 0);
			fRec2[0] = (0 - (((((0 - (fSlow14 * fTemp3)) * cosf((fConst2 * ((iTemp5 * (0 - fTemp4)) + (fSlow13 * (fTemp4 * (0 - (iTemp5 - 1)))))))) * fRec2[1]) + (fSlow12 * (faustpower<2>(fTemp3) * fRec2[2]))) - (fSlow10 * (fRec4[0] * fRec3[0]))));
			fRec10[0] = ((0.999f * fRec10[1]) + fSlow15);
			fRec11[0] = (0.999f * fRec11[1]);
			float fTemp6 = loadPreset(fSlow1, 1, fRec11[0]);
			float fTemp7 = loadPreset(fSlow1, 0, fRec11[0]);
			int iTemp8 = (fTemp7 < 0);
			fRec9[0] = (0 - (((((0 - (fSlow14 * fTemp6)) * cosf((fConst2 * ((iTemp8 * (0 - fTemp7)) + (fSlow13 * (fTemp7 * (0 - (iTemp8 - 1)))))))) * fRec9[1]) + (fSlow12 * (faustpower<2>(fTemp6) * fRec9[2]))) - (fSlow10 * (fRec4[0] * fRec10[0]))));
			fRec13[0] = ((0.999f * fRec13[1]) + fSlow16);
			fRec14[0] = ((0.999f * fRec14[1]) + 0.0020000000000000018f);
			float fTemp9 = loadPreset(fSlow1, 1, fRec14[0]);
			float fTemp10 = loadPreset(fSlow1, 0, fRec14[0]);
			int iTemp11 = (fTemp10 < 0);
			fRec12[0] = (0 - (((((0 - (fSlow14 * fTemp9)) * cosf((fConst2 * ((iTemp11 * (0 - fTemp10)) + (fSlow13 * (fTemp10 * (0 - (iTemp11 - 1)))))))) * fRec12[1]) + (fSlow12 * (faustpower<2>(fTemp9) * fRec12[2]))) - (fSlow10 * (fRec4[0] * fRec13[0]))));
			fRec16[0] = ((0.999f * fRec16[1]) + fSlow17);
			fRec17[0] = ((0.999f * fRec17[1]) + 0.0030000000000000027f);
			float fTemp12 = loadPreset(fSlow1, 1, fRec17[0]);
			float fTemp13 = loadPreset(fSlow1, 0, fRec17[0]);
			int iTemp14 = (fTemp13 < 0);
			fRec15[0] = (0 - (((((0 - (fSlow14 * fTemp12)) * cosf((fConst2 * ((iTemp14 * (0 - fTemp13)) + (fSlow13 * (fTemp13 * (0 - (iTemp14 - 1)))))))) * fRec15[1]) + (fSlow12 * (faustpower<2>(fTemp12) * fRec15[2]))) - (fSlow10 * (fRec4[0] * fRec16[0]))));
			float fTemp15 = (fRec15[0] + (fRec12[0] + (fRec9[0] + fRec2[0])));
			float fTemp16 = (fTemp15 + (fSlow18 * ((fSlow10 * fRec4[0]) - fTemp15)));
			fRec18[0] = (fSlow19 + (0.999f * fRec18[1]));
			float fTemp17 = (fTemp16 * fTemp1);
			fVec0[0] = fTemp17;
			float fTemp18 = (fRec18[0] * (((fSlow23 * fVec0[0]) + (fSlow22 * (fVec0[0] + fVec0[1]))) + (fSlow21 * (faustpower<2>(fTemp16) * faustpower<2>(fTemp1)))));
			float fTemp19 = cosf(fTemp18);
			float fTemp20 = sinf(fTemp18);
			float fTemp21 = (0 - fTemp20);
			float fTemp22 = ((fRec19[1] * fTemp21) + (fVec0[0] * fTemp19));
			float fTemp23 = ((fTemp21 * fRec20[1]) + (fTemp19 * fTemp22));
			float fTemp24 = ((fTemp21 * fRec21[1]) + (fTemp19 * fTemp23));
			float fTemp25 = ((fTemp21 * fRec22[1]) + (fTemp19 * fTemp24));
			float fTemp26 = ((fTemp21 * fRec23[1]) + (fTemp19 * fTemp25));
			fRec24[0] = ((fTemp21 * fRec24[1]) + (fTemp19 * fTemp26));
			fRec23[0] = ((fTemp20 * fTemp26) + (fTemp19 * fRec24[1]));
			fRec22[0] = ((fTemp20 * fTemp25) + (fTemp19 * fRec23[1]));
			fRec21[0] = ((fTemp20 * fTemp24) + (fTemp19 * fRec22[1]));
			fRec20[0] = ((fTemp20 * fTemp23) + (fTemp19 * fRec21[1]));
			fRec19[0] = ((fTemp20 * fTemp22) + (fTemp19 * fRec20[1]));
			fRec26[0] = (fSlow25 + (0.999f * fRec26[1]));
			float fTemp27 = (fRec25[1] + (fConst1 * (fSlow27 + (iSlow26 * fRec26[0]))));
			fRec25[0] = (fTemp27 - floorf(fTemp27));
			float fTemp28 = (3.141592653589793f * (fRec18[0] * ftbl0[int((65536.0f * fRec25[0]))]));
			float fTemp29 = cosf(fTemp28);
			float fTemp30 = sinf(fTemp28);
			float fTemp31 = (0 - fTemp30);
			float fTemp32 = ((fRec27[1] * fTemp31) + (fVec0[0] * fTemp29));
			float fTemp33 = ((fTemp31 * fRec28[1]) + (fTemp29 * fTemp32));
			float fTemp34 = ((fTemp31 * fRec29[1]) + (fTemp29 * fTemp33));
			float fTemp35 = ((fTemp31 * fRec30[1]) + (fTemp29 * fTemp34));
			float fTemp36 = ((fTemp31 * fRec31[1]) + (fTemp29 * fTemp35));
			fRec32[0] = ((fTemp31 * fRec32[1]) + (fTemp29 * fTemp36));
			fRec31[0] = ((fTemp30 * fTemp36) + (fTemp29 * fRec32[1]));
			fRec30[0] = ((fTemp30 * fTemp35) + (fTemp29 * fRec31[1]));
			fRec29[0] = ((fTemp30 * fTemp34) + (fTemp29 * fRec30[1]));
			fRec28[0] = ((fTemp30 * fTemp33) + (fTemp29 * fRec29[1]));
			fRec27[0] = ((fTemp30 * fTemp32) + (fTemp29 * fRec28[1]));
			iRec33[0] = (iSlow29 & (iRec33[1] | (fRec34[1] >= 1)));
			int iTemp37 = (iSlow30 & (fRec34[1] > 0));
			fRec34[0] = (((fSlow34 * (((iRec33[1] == 0) & iSlow29) & (fRec34[1] < 1))) + (fRec34[1] * (1 - (fSlow32 * iTemp37)))) * ((iTemp37 == 0) | (fRec34[1] >= 1e-06f)));
			float fTemp38 = (fRec34[0] * ((iSlow28 * ((fVec0[0] * fTemp30) + (fRec27[1] * fTemp29))) + (iSlow24 * ((fRec18[0] * ((fVec0[0] * fTemp20) + (fRec19[1] * fTemp19))) + (((1 - fRec18[0]) * fTemp16) * fTemp1)))));
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
			fRec25[1] = fRec25[0];
			fRec26[1] = fRec26[0];
			fRec19[1] = fRec19[0];
			fRec20[1] = fRec20[0];
			fRec21[1] = fRec21[0];
			fRec22[1] = fRec22[0];
			fRec23[1] = fRec23[0];
			fRec24[1] = fRec24[0];
			fVec0[1] = fVec0[0];
			fRec18[1] = fRec18[0];
			fRec15[2] = fRec15[1]; fRec15[1] = fRec15[0];
			fRec17[1] = fRec17[0];
			fRec16[1] = fRec16[0];
			fRec12[2] = fRec12[1]; fRec12[1] = fRec12[0];
			fRec14[1] = fRec14[0];
			fRec13[1] = fRec13[0];
			fRec9[2] = fRec9[1]; fRec9[1] = fRec9[0];
			fRec11[1] = fRec11[0];
			fRec10[1] = fRec10[0];
			fRec2[2] = fRec2[1]; fRec2[1] = fRec2[0];
			fRec8[1] = fRec8[0];
			fRec4[1] = fRec4[0];
			fRec7[1] = fRec7[0];
			fRec6[1] = fRec6[0];
			fRec3[1] = fRec3[0];
			fRec1[1] = fRec1[0];
		}
	}
};


float 	Modal_Bar_dsp::ftbl0[65536];
float 	Modal_Bar_dsp::ftbl1[246];


#include "Faust_plugins_template2.cpp"

