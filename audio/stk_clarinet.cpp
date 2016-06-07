//-----------------------------------------------------
// name: "Clarinet"
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
#define FAUSTCLASS Clarinet_dsp
#endif

class Clarinet_dsp : public dsp {
  private:
	class SIG0 {
	  private:
		int 	fSamplingFreq;
		int 	iRec2[2];
	  public:
		int getNumInputs() 	{ return 0; }
		int getNumOutputs() 	{ return 1; }
		void init(int samplingFreq) {
			fSamplingFreq = samplingFreq;
			for (int i=0; i<2; i++) iRec2[i] = 0;
		}
		void fill (int count, float output[]) {
			for (int i=0; i<count; i++) {
				iRec2[0] = (1 + iRec2[1]);
				output[i] = sinf((9.587379924285257e-05f * float((iRec2[0] - 1))));
				// post processing
				iRec2[1] = iRec2[0];
			}
		}
	};


	FAUSTFLOAT 	fentry0;
	float 	fRec0[2];
	static float 	ftbl0[65536];
	FAUSTFLOAT 	fslider0;
	int 	iConst0;
	float 	fConst1;
	float 	fRec3[2];
	FAUSTFLOAT 	fbutton0;
	int 	iRec4[2];
	int 	iRec5[2];
	FAUSTFLOAT 	fslider1;
	FAUSTFLOAT 	fslider2;
	float 	fConst2;
	float 	fConst3;
	float 	fRec6[2];
	FAUSTFLOAT 	fslider3;
	int 	iRec7[2];
	FAUSTFLOAT 	fslider4;
	int 	iRec8[2];
	FAUSTFLOAT 	fslider5;
	FAUSTFLOAT 	fslider6;
	FAUSTFLOAT 	fslider7;
	float 	fRec9[2];
	FAUSTFLOAT 	fslider8;
	FAUSTFLOAT 	fentry1;
	FAUSTFLOAT 	fslider9;
	FAUSTFLOAT 	fentry2;
	float 	fConst4;
	float 	fVec0[2];
	float 	fRec11[2];
	int 	iRec12[2];
	FAUSTFLOAT 	fslider10;
	float 	fRec13[2];
	float 	fRec19[2];
	float 	fRec18[2];
	float 	fRec17[2];
	float 	fRec16[2];
	float 	fRec15[2];
	float 	fRec14[2];
	FAUSTFLOAT 	fslider11;
	float 	fRec21[2];
	float 	fRec20[2];
	float 	fRec27[2];
	float 	fRec26[2];
	float 	fRec25[2];
	float 	fRec24[2];
	float 	fRec23[2];
	float 	fRec22[2];
	float 	fRec10[2];
	FAUSTFLOAT 	fslider12;
	int 	IOTA;
	float 	fRec1[8192];
	float 	fVec1[4096];
	FAUSTFLOAT 	fslider13;
	FAUSTFLOAT 	fslider14;
  public:
	static void metadata(Meta* m) 	{ 
		m->declare("name", "Clarinet");
		m->declare("description", "Nonlinear WaveGuide Clarinet");
		m->declare("author", "Romain Michon");
		m->declare("copyright", "Romain Michon (rmichon@ccrma.stanford.edu)");
		m->declare("version", "1.0");
		m->declare("licence", "STK-4.3");
		m->declare("reference", "https://ccrma.stanford.edu/~jos/pasp/Woodwinds.html");
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
		fslider0 = 5.0f;
		iConst0 = min(192000, max(1, fSamplingFreq));
		fConst1 = (1.0f / float(iConst0));
		for (int i=0; i<2; i++) fRec3[i] = 0;
		fbutton0 = 0.0;
		for (int i=0; i<2; i++) iRec4[i] = 0;
		for (int i=0; i<2; i++) iRec5[i] = 0;
		fslider1 = 0.01f;
		fslider2 = 0.5f;
		fConst2 = (0.2f * iConst0);
		fConst3 = (1.8f * iConst0);
		for (int i=0; i<2; i++) fRec6[i] = 0;
		fslider3 = 0.1f;
		for (int i=0; i<2; i++) iRec7[i] = 0;
		fslider4 = 0.0f;
		for (int i=0; i<2; i++) iRec8[i] = 0;
		fslider5 = 0.1f;
		fslider6 = 0.05f;
		fslider7 = 0.01f;
		for (int i=0; i<2; i++) fRec9[i] = 0;
		fslider8 = 1.0f;
		fentry1 = 0.0f;
		fslider9 = 0.0f;
		fentry2 = 4.4e+02f;
		fConst4 = (0.5f * iConst0);
		for (int i=0; i<2; i++) fVec0[i] = 0;
		for (int i=0; i<2; i++) fRec11[i] = 0;
		for (int i=0; i<2; i++) iRec12[i] = 0;
		fslider10 = 0.1f;
		for (int i=0; i<2; i++) fRec13[i] = 0;
		for (int i=0; i<2; i++) fRec19[i] = 0;
		for (int i=0; i<2; i++) fRec18[i] = 0;
		for (int i=0; i<2; i++) fRec17[i] = 0;
		for (int i=0; i<2; i++) fRec16[i] = 0;
		for (int i=0; i<2; i++) fRec15[i] = 0;
		for (int i=0; i<2; i++) fRec14[i] = 0;
		fslider11 = 2.2e+02f;
		for (int i=0; i<2; i++) fRec21[i] = 0;
		for (int i=0; i<2; i++) fRec20[i] = 0;
		for (int i=0; i<2; i++) fRec27[i] = 0;
		for (int i=0; i<2; i++) fRec26[i] = 0;
		for (int i=0; i<2; i++) fRec25[i] = 0;
		for (int i=0; i<2; i++) fRec24[i] = 0;
		for (int i=0; i<2; i++) fRec23[i] = 0;
		for (int i=0; i<2; i++) fRec22[i] = 0;
		for (int i=0; i<2; i++) fRec10[i] = 0;
		fslider12 = 0.5f;
		IOTA = 0;
		for (int i=0; i<8192; i++) fRec1[i] = 0;
		for (int i=0; i<4096; i++) fVec1[i] = 0;
		fslider13 = 0.6f;
		fslider14 = 0.5f;
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
		faust_interface->addNumEntry("gain", &fentry0, 1.0f, 0.0f, 1.0f, 0.01f);
		faust_interface->declare(&fbutton0, "1", "");
		faust_interface->declare(&fbutton0, "tooltip", "noteOn = 1, noteOff = 0");
		faust_interface->addButton("gate", &fbutton0);
		faust_interface->closeBox();
		faust_interface->openHorizontalBox("Envelopes_and_Vibrato");
		faust_interface->openVerticalBox("Envelope_Parameters");
		faust_interface->declare(&fslider7, "5", "");
		faust_interface->declare(&fslider7, "tooltip", "Envelope attack duration");
		faust_interface->declare(&fslider7, "unit", "s");
		faust_interface->addHorizontalSlider("Envelope_Attack", &fslider7, 0.01f, 0.0f, 2.0f, 0.01f);
		faust_interface->declare(&fslider6, "5", "");
		faust_interface->declare(&fslider6, "tooltip", "Envelope decay duration");
		faust_interface->declare(&fslider6, "unit", "s");
		faust_interface->addHorizontalSlider("Envelope_Decay", &fslider6, 0.05f, 0.0f, 2.0f, 0.01f);
		faust_interface->declare(&fslider5, "5", "");
		faust_interface->declare(&fslider5, "tooltip", "Envelope release duration");
		faust_interface->declare(&fslider5, "unit", "s");
		faust_interface->addHorizontalSlider("Envelope_Release", &fslider5, 0.1f, 0.0f, 2.0f, 0.01f);
		faust_interface->closeBox();
		faust_interface->openVerticalBox("Vibrato_Parameters");
		faust_interface->declare(&fslider2, "4", "");
		faust_interface->declare(&fslider2, "tooltip", "Vibrato attack duration");
		faust_interface->declare(&fslider2, "unit", "s");
		faust_interface->addHorizontalSlider("Vibrato_Attack", &fslider2, 0.5f, 0.0f, 2.0f, 0.01f);
		faust_interface->declare(&fslider0, "4", "");
		faust_interface->declare(&fslider0, "unit", "Hz");
		faust_interface->addHorizontalSlider("Vibrato_Freq", &fslider0, 5.0f, 1.0f, 15.0f, 0.1f);
		faust_interface->declare(&fslider3, "4", "");
		faust_interface->declare(&fslider3, "tooltip", "A value between 0 and 1");
		faust_interface->addHorizontalSlider("Vibrato_Gain", &fslider3, 0.1f, 0.0f, 1.0f, 0.01f);
		faust_interface->declare(&fslider1, "4", "");
		faust_interface->declare(&fslider1, "tooltip", "Vibrato release duration");
		faust_interface->declare(&fslider1, "unit", "s");
		faust_interface->addHorizontalSlider("Vibrato_Release", &fslider1, 0.01f, 0.0f, 2.0f, 0.01f);
		faust_interface->closeBox();
		faust_interface->closeBox();
		faust_interface->openHorizontalBox("Physical_and_Nonlinearity");
		faust_interface->openVerticalBox("Nonlinear_Filter_Parameters");
		faust_interface->declare(&fslider11, "3", "");
		faust_interface->declare(&fslider11, "tooltip", "Frequency of the sine wave for the modulation of theta (works if Modulation Type=3)");
		faust_interface->declare(&fslider11, "unit", "Hz");
		faust_interface->addHorizontalSlider("Modulation_Frequency", &fslider11, 2.2e+02f, 2e+01f, 1e+03f, 0.1f);
		faust_interface->declare(&fentry1, "3", "");
		faust_interface->declare(&fentry1, "tooltip", "0=theta is modulated by the incoming signal; 1=theta is modulated by the averaged incoming signal; 2=theta is modulated by the squared incoming signal; 3=theta is modulated by a sine wave of frequency freqMod; 4=theta is modulated by a sine wave of frequency freq;");
		faust_interface->addNumEntry("Modulation_Type", &fentry1, 0.0f, 0.0f, 4.0f, 1.0f);
		faust_interface->declare(&fslider9, "3", "");
		faust_interface->declare(&fslider9, "tooltip", "Nonlinearity factor (value between 0 and 1)");
		faust_interface->addHorizontalSlider("Nonlinearity", &fslider9, 0.0f, 0.0f, 1.0f, 0.01f);
		faust_interface->declare(&fslider10, "3", "");
		faust_interface->declare(&fslider10, "Attack duration of the nonlinearity", "");
		faust_interface->declare(&fslider10, "unit", "s");
		faust_interface->addHorizontalSlider("Nonlinearity_Attack", &fslider10, 0.1f, 0.0f, 2.0f, 0.01f);
		faust_interface->closeBox();
		faust_interface->openVerticalBox("Physical_Parameters");
		faust_interface->declare(&fslider4, "2", "");
		faust_interface->declare(&fslider4, "tooltip", "Breath noise gain (value between 0 and 1)");
		faust_interface->addHorizontalSlider("Noise_Gain", &fslider4, 0.0f, 0.0f, 1.0f, 0.01f);
		faust_interface->declare(&fslider8, "2", "");
		faust_interface->declare(&fslider8, "tooltip", "Breath pressure (value bewteen 0 and 1)");
		faust_interface->addHorizontalSlider("Pressure", &fslider8, 1.0f, 0.0f, 1.0f, 0.01f);
		faust_interface->declare(&fslider12, "2", "");
		faust_interface->declare(&fslider12, "tooltip", "Reed stiffness (value between 0 and 1)");
		faust_interface->addHorizontalSlider("Reed_Stiffness", &fslider12, 0.5f, 0.0f, 1.0f, 0.01f);
		faust_interface->closeBox();
		faust_interface->closeBox();
		faust_interface->openVerticalBox("Spat");
		faust_interface->addHorizontalSlider("pan angle", &fslider13, 0.6f, 0.0f, 1.0f, 0.01f);
		faust_interface->addHorizontalSlider("spatial width", &fslider14, 0.5f, 0.0f, 1.0f, 0.01f);
		faust_interface->closeBox();
		faust_interface->closeBox();
	}
	virtual void compute (int count, FAUSTFLOAT** input, FAUSTFLOAT** output) {
		float 	fSlow0 = (0.0010000000000000009f * float(fentry0));
		float 	fSlow1 = (fConst1 * float(fslider0));
		float 	fSlow2 = float(fbutton0);
		int 	iSlow3 = (fSlow2 > 0);
		int 	iSlow4 = (fSlow2 <= 0);
		float 	fSlow5 = float(fslider1);
		float 	fSlow6 = (1 - (1.0f / powf(1e+05f,(1.0f / ((fSlow5 == 0.0f) + (iConst0 * fSlow5))))));
		float 	fSlow7 = float(fslider2);
		float 	fSlow8 = (fConst2 * fSlow7);
		float 	fSlow9 = (fSlow8 + ((0.2f * fSlow7) == 0.0f));
		float 	fSlow10 = (1.0f / ((fConst3 * fSlow7) + ((1.8f * fSlow7) == 0.0f)));
		float 	fSlow11 = float(fslider3);
		float 	fSlow12 = (4.1909515877212175e-10f * float(fslider4));
		float 	fSlow13 = float(fslider5);
		float 	fSlow14 = (1 - (1.0f / powf(1e+05f,(1.0f / ((fSlow13 == 0.0f) + (iConst0 * fSlow13))))));
		float 	fSlow15 = float(fslider6);
		float 	fSlow16 = (1 - powf(1e+02f,(1.0f / ((fSlow15 == 0.0f) + (iConst0 * fSlow15)))));
		float 	fSlow17 = float(fslider7);
		float 	fSlow18 = (1.0f / ((fSlow17 == 0.0f) + (iConst0 * fSlow17)));
		float 	fSlow19 = float(fslider8);
		float 	fSlow20 = float(fentry1);
		float 	fSlow21 = float(fslider9);
		float 	fSlow22 = (6 * (fSlow21 * (fSlow20 < 2)));
		float 	fSlow23 = float(fentry2);
		float 	fSlow24 = (fConst4 / fSlow23);
		int 	iSlow25 = int((fSlow24 - (1.5f + fSlow22)));
		int 	iSlow26 = int((1 + int((int((1 + iSlow25)) & 4095))));
		float 	fSlow27 = (fSlow22 + iSlow25);
		float 	fSlow28 = (fSlow24 - (1.5f + fSlow27));
		int 	iSlow29 = int((1 + int((iSlow25 & 4095))));
		float 	fSlow30 = ((fSlow27 + 2.5f) - fSlow24);
		float 	fSlow31 = (0.0010000000000000009f * fSlow21);
		float 	fSlow32 = (3.141592653589793f * (fSlow20 == 2));
		float 	fSlow33 = (1.5707963267948966f * (fSlow20 == 1));
		float 	fSlow34 = (3.141592653589793f * (fSlow20 == 0));
		float 	fSlow35 = float(fslider10);
		float 	fSlow36 = (1.0f / ((fSlow35 == 0.0f) + (iConst0 * fSlow35)));
		int 	iSlow37 = (fSlow20 < 3);
		float 	fSlow38 = (0.0010000000000000009f * float(fslider11));
		int 	iSlow39 = (fSlow20 != 4);
		float 	fSlow40 = (fSlow23 * (fSlow20 == 4));
		int 	iSlow41 = (fSlow20 >= 3);
		float 	fSlow42 = ((0.26f * float(fslider12)) - 0.44f);
		float 	fSlow43 = float(fslider13);
		float 	fSlow44 = (1.5f * (1.0f - fSlow43));
		int 	iSlow45 = int((int((fConst4 * (float(fslider14) / fSlow23))) & 4095));
		float 	fSlow46 = (1.5f * fSlow43);
		FAUSTFLOAT* output0 = output[0];
		FAUSTFLOAT* output1 = output[1];
		for (int i=0; i<count; i++) {
			fRec0[0] = ((0.999f * fRec0[1]) + fSlow0);
			float fTemp0 = (fRec3[1] + fSlow1);
			fRec3[0] = (fTemp0 - floorf(fTemp0));
			iRec4[0] = (iSlow3 & (iRec4[1] | (fRec6[1] >= 1)));
			iRec5[0] = (iSlow3 * (1 + iRec5[1]));
			int iTemp1 = (iSlow4 & (fRec6[1] > 0));
			fRec6[0] = (((fSlow10 * (((((iRec4[1] == 0) & iSlow3) & (fRec6[1] < 1)) & (iRec5[1] > fSlow8)) * (1 - (iRec5[1] < fSlow9)))) + (fRec6[1] * (1 - (fSlow6 * iTemp1)))) * ((iTemp1 == 0) | (fRec6[1] >= 1e-06f)));
			iRec7[0] = (12345 + (1103515245 * iRec7[1]));
			iRec8[0] = (iSlow3 & (iRec8[1] | (fRec9[1] >= 1)));
			int iTemp2 = (iSlow4 & (fRec9[1] > 0));
			fRec9[0] = (((fSlow18 * (((iRec8[1] == 0) & iSlow3) & (fRec9[1] < 1))) + (fRec9[1] * ((1 - (fSlow16 * (iRec8[1] & (fRec9[1] > 100)))) - (fSlow14 * iTemp2)))) * ((iTemp2 == 0) | (fRec9[1] >= 1e-06f)));
			float fTemp3 = (fSlow19 * ((fRec9[0] * (0.9f + (fSlow12 * iRec7[0]))) * (1 + (fSlow11 * (fRec6[0] * ftbl0[int((65536.0f * fRec3[0]))])))));
			float fTemp4 = ((fSlow30 * fRec1[(IOTA-iSlow29)&8191]) + (fSlow28 * fRec1[(IOTA-iSlow26)&8191]));
			fVec0[0] = fTemp4;
			fRec11[0] = (fSlow31 + (0.999f * fRec11[1]));
			iRec12[0] = (iSlow3 & (iRec12[1] | (fRec13[1] >= 1)));
			int iTemp5 = (iSlow4 & (fRec13[1] > 0));
			fRec13[0] = (((fSlow36 * (((iRec12[1] == 0) & iSlow3) & (fRec13[1] < 1))) + (fRec13[1] * (1 - (fSlow14 * iTemp5)))) * ((iTemp5 == 0) | (fRec13[1] >= 1e-06f)));
			float fTemp6 = (fRec11[0] * fRec13[0]);
			float fTemp7 = (fTemp6 * (((fSlow34 * fVec0[0]) + (fSlow33 * (fVec0[0] + fVec0[1]))) + (fSlow32 * faustpower<2>(fVec0[0]))));
			float fTemp8 = cosf(fTemp7);
			float fTemp9 = sinf(fTemp7);
			float fTemp10 = (0 - fTemp9);
			float fTemp11 = ((fRec14[1] * fTemp10) + (fVec0[0] * fTemp8));
			float fTemp12 = ((fTemp10 * fRec15[1]) + (fTemp8 * fTemp11));
			float fTemp13 = ((fTemp10 * fRec16[1]) + (fTemp8 * fTemp12));
			float fTemp14 = ((fTemp10 * fRec17[1]) + (fTemp8 * fTemp13));
			float fTemp15 = ((fTemp10 * fRec18[1]) + (fTemp8 * fTemp14));
			fRec19[0] = ((fTemp10 * fRec19[1]) + (fTemp8 * fTemp15));
			fRec18[0] = ((fTemp9 * fTemp15) + (fTemp8 * fRec19[1]));
			fRec17[0] = ((fTemp9 * fTemp14) + (fTemp8 * fRec18[1]));
			fRec16[0] = ((fTemp9 * fTemp13) + (fTemp8 * fRec17[1]));
			fRec15[0] = ((fTemp9 * fTemp12) + (fTemp8 * fRec16[1]));
			fRec14[0] = ((fTemp9 * fTemp11) + (fTemp8 * fRec15[1]));
			fRec21[0] = (fSlow38 + (0.999f * fRec21[1]));
			float fTemp16 = (fRec20[1] + (fConst1 * (fSlow40 + (iSlow39 * fRec21[0]))));
			fRec20[0] = (fTemp16 - floorf(fTemp16));
			float fTemp17 = (3.141592653589793f * (fTemp6 * ftbl0[int((65536.0f * fRec20[0]))]));
			float fTemp18 = cosf(fTemp17);
			float fTemp19 = sinf(fTemp17);
			float fTemp20 = (0 - fTemp19);
			float fTemp21 = ((fRec22[1] * fTemp20) + (fVec0[0] * fTemp18));
			float fTemp22 = ((fTemp20 * fRec23[1]) + (fTemp18 * fTemp21));
			float fTemp23 = ((fTemp20 * fRec24[1]) + (fTemp18 * fTemp22));
			float fTemp24 = ((fTemp20 * fRec25[1]) + (fTemp18 * fTemp23));
			float fTemp25 = ((fTemp20 * fRec26[1]) + (fTemp18 * fTemp24));
			fRec27[0] = ((fTemp20 * fRec27[1]) + (fTemp18 * fTemp25));
			fRec26[0] = ((fTemp19 * fTemp25) + (fTemp18 * fRec27[1]));
			fRec25[0] = ((fTemp19 * fTemp24) + (fTemp18 * fRec26[1]));
			fRec24[0] = ((fTemp19 * fTemp23) + (fTemp18 * fRec25[1]));
			fRec23[0] = ((fTemp19 * fTemp22) + (fTemp18 * fRec24[1]));
			fRec22[0] = ((fTemp19 * fTemp21) + (fTemp18 * fRec23[1]));
			fRec10[0] = (0.5f * (fRec10[1] + ((iSlow41 * ((fVec0[0] * fTemp19) + (fRec22[1] * fTemp18))) + (iSlow37 * ((fRec11[0] * ((fVec0[0] * fTemp9) + (fRec14[1] * fTemp8))) + ((1 - fRec11[0]) * fVec0[0]))))));
			float fTemp26 = (0 - ((0.95f * fRec10[0]) + fTemp3));
			float fTemp27 = (0.7f + (fSlow42 * fTemp26));
			float fTemp28 = ((fTemp27 > 1) + (fTemp27 * (fTemp27 <= 1)));
			fRec1[IOTA&8191] = (fTemp3 + (fTemp26 * ((fTemp28 * (fTemp28 >= -1)) - (fTemp28 < -1))));
			float fTemp29 = (fRec1[(IOTA-0)&8191] * fRec0[0]);
			fVec1[IOTA&4095] = fTemp29;
			output0[i] = (FAUSTFLOAT)(fSlow44 * fVec1[IOTA&4095]);
			output1[i] = (FAUSTFLOAT)(fSlow46 * fVec1[(IOTA-iSlow45)&4095]);
			// post processing
			IOTA = IOTA+1;
			fRec10[1] = fRec10[0];
			fRec22[1] = fRec22[0];
			fRec23[1] = fRec23[0];
			fRec24[1] = fRec24[0];
			fRec25[1] = fRec25[0];
			fRec26[1] = fRec26[0];
			fRec27[1] = fRec27[0];
			fRec20[1] = fRec20[0];
			fRec21[1] = fRec21[0];
			fRec14[1] = fRec14[0];
			fRec15[1] = fRec15[0];
			fRec16[1] = fRec16[0];
			fRec17[1] = fRec17[0];
			fRec18[1] = fRec18[0];
			fRec19[1] = fRec19[0];
			fRec13[1] = fRec13[0];
			iRec12[1] = iRec12[0];
			fRec11[1] = fRec11[0];
			fVec0[1] = fVec0[0];
			fRec9[1] = fRec9[0];
			iRec8[1] = iRec8[0];
			iRec7[1] = iRec7[0];
			fRec6[1] = fRec6[0];
			iRec5[1] = iRec5[0];
			iRec4[1] = iRec4[0];
			fRec3[1] = fRec3[0];
			fRec0[1] = fRec0[0];
		}
	}
};


float 	Clarinet_dsp::ftbl0[65536];


#include "Faust_plugins_template2.cpp"

