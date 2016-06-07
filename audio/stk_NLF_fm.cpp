//-----------------------------------------------------
// name: "NLFfm"
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
#define FAUSTCLASS NLF_Fm_dsp
#endif

class NLF_Fm_dsp : public dsp {
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


	static float 	ftbl0[65536];
	FAUSTFLOAT 	fslider0;
	int 	iConst0;
	float 	fConst1;
	float 	fRec1[2];
	FAUSTFLOAT 	fbutton0;
	int 	iRec2[2];
	int 	iRec3[2];
	FAUSTFLOAT 	fslider1;
	FAUSTFLOAT 	fslider2;
	float 	fConst2;
	float 	fConst3;
	float 	fRec4[2];
	FAUSTFLOAT 	fslider3;
	FAUSTFLOAT 	fentry0;
	float 	fRec5[2];
	FAUSTFLOAT 	fslider4;
	float 	fRec6[2];
	FAUSTFLOAT 	fentry1;
	float 	fRec7[2];
	int 	iRec8[2];
	FAUSTFLOAT 	fslider5;
	FAUSTFLOAT 	fslider6;
	FAUSTFLOAT 	fslider7;
	float 	fRec9[2];
	FAUSTFLOAT 	fentry2;
	float 	fVec0[2];
	int 	iRec10[2];
	FAUSTFLOAT 	fslider8;
	float 	fRec11[2];
	float 	fRec14[2];
	float 	fRec13[2];
	float 	fRec12[2];
	FAUSTFLOAT 	fslider9;
	float 	fRec16[2];
	float 	fRec15[2];
	float 	fRec19[2];
	float 	fRec18[2];
	float 	fRec17[2];
	int 	IOTA;
	float 	fVec1[4096];
	FAUSTFLOAT 	fslider10;
	FAUSTFLOAT 	fslider11;
	float 	fConst4;
  public:
	static void metadata(Meta* m) 	{ 
		m->declare("name", "NLFfm");
		m->declare("description", "FM synthesizer implemented with a nonlinear passive allpass filter");
		m->declare("author", "Romain Michon");
		m->declare("copyright", "Romain Michon (rmichon@ccrma.stanford.edu)");
		m->declare("version", "1.0");
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
		fslider0 = 5.0f;
		iConst0 = min(192000, max(1, fSamplingFreq));
		fConst1 = (1.0f / float(iConst0));
		for (int i=0; i<2; i++) fRec1[i] = 0;
		fbutton0 = 0.0;
		for (int i=0; i<2; i++) iRec2[i] = 0;
		for (int i=0; i<2; i++) iRec3[i] = 0;
		fslider1 = 0.01f;
		fslider2 = 0.5f;
		fConst2 = (0.2f * iConst0);
		fConst3 = (1.8f * iConst0);
		for (int i=0; i<2; i++) fRec4[i] = 0;
		fslider3 = 0.1f;
		fentry0 = 4.4e+02f;
		for (int i=0; i<2; i++) fRec5[i] = 0;
		fslider4 = 0.0f;
		for (int i=0; i<2; i++) fRec6[i] = 0;
		fentry1 = 0.8f;
		for (int i=0; i<2; i++) fRec7[i] = 0;
		for (int i=0; i<2; i++) iRec8[i] = 0;
		fslider5 = 0.05f;
		fslider6 = 0.05f;
		fslider7 = 0.05f;
		for (int i=0; i<2; i++) fRec9[i] = 0;
		fentry2 = 0.0f;
		for (int i=0; i<2; i++) fVec0[i] = 0;
		for (int i=0; i<2; i++) iRec10[i] = 0;
		fslider8 = 0.1f;
		for (int i=0; i<2; i++) fRec11[i] = 0;
		for (int i=0; i<2; i++) fRec14[i] = 0;
		for (int i=0; i<2; i++) fRec13[i] = 0;
		for (int i=0; i<2; i++) fRec12[i] = 0;
		fslider9 = 2.2e+02f;
		for (int i=0; i<2; i++) fRec16[i] = 0;
		for (int i=0; i<2; i++) fRec15[i] = 0;
		for (int i=0; i<2; i++) fRec19[i] = 0;
		for (int i=0; i<2; i++) fRec18[i] = 0;
		for (int i=0; i<2; i++) fRec17[i] = 0;
		IOTA = 0;
		for (int i=0; i<4096; i++) fVec1[i] = 0;
		fslider10 = 0.6f;
		fslider11 = 0.5f;
		fConst4 = (0.5f * iConst0);
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
		faust_interface->declare(&fentry1, "1", "");
		faust_interface->declare(&fentry1, "tooltip", "Gain (value between 0 and 1)");
		faust_interface->addNumEntry("gain", &fentry1, 0.8f, 0.0f, 1.0f, 0.01f);
		faust_interface->declare(&fbutton0, "1", "");
		faust_interface->declare(&fbutton0, "tooltip", "noteOn = 1, noteOff = 0");
		faust_interface->addButton("gate", &fbutton0);
		faust_interface->closeBox();
		faust_interface->openHorizontalBox("Envelopes_and_Vibrato");
		faust_interface->openVerticalBox("Envelope_Parameters");
		faust_interface->declare(&fslider7, "4", "");
		faust_interface->declare(&fslider7, "tooltip", "Envelope attack duration");
		faust_interface->declare(&fslider7, "unit", "s");
		faust_interface->addHorizontalSlider("Envelope_Attack", &fslider7, 0.05f, 0.0f, 2.0f, 0.01f);
		faust_interface->declare(&fslider6, "4", "");
		faust_interface->declare(&fslider6, "tooltip", "Envelope decay duration");
		faust_interface->declare(&fslider6, "unit", "s");
		faust_interface->addHorizontalSlider("Envelope_Decay", &fslider6, 0.05f, 0.0f, 2.0f, 0.01f);
		faust_interface->declare(&fslider5, "4", "");
		faust_interface->declare(&fslider5, "tooltip", "Envelope release duration");
		faust_interface->declare(&fslider5, "unit", "s");
		faust_interface->addHorizontalSlider("Envelope_Release", &fslider5, 0.05f, 0.0f, 2.0f, 0.01f);
		faust_interface->closeBox();
		faust_interface->openVerticalBox("Vibrato_Parameters");
		faust_interface->declare(&fslider2, "3", "");
		faust_interface->declare(&fslider2, "tooltip", "Vibrato attack duration");
		faust_interface->declare(&fslider2, "unit", "s");
		faust_interface->addHorizontalSlider("Vibrato_Attack", &fslider2, 0.5f, 0.0f, 2.0f, 0.01f);
		faust_interface->declare(&fslider0, "3", "");
		faust_interface->declare(&fslider0, "unit", "Hz");
		faust_interface->addHorizontalSlider("Vibrato_Freq", &fslider0, 5.0f, 1.0f, 15.0f, 0.1f);
		faust_interface->declare(&fslider3, "3", "");
		faust_interface->declare(&fslider3, "tooltip", "A value between 0 and 1");
		faust_interface->addHorizontalSlider("Vibrato_Gain", &fslider3, 0.1f, 0.0f, 1.0f, 0.01f);
		faust_interface->declare(&fslider1, "3", "");
		faust_interface->declare(&fslider1, "tooltip", "Vibrato release duration");
		faust_interface->declare(&fslider1, "unit", "s");
		faust_interface->addHorizontalSlider("Vibrato_Release", &fslider1, 0.01f, 0.0f, 2.0f, 0.01f);
		faust_interface->closeBox();
		faust_interface->closeBox();
		faust_interface->openHorizontalBox("Physical_and_Nonlinearity");
		faust_interface->openVerticalBox("Nonlinear_Filter_Parameters");
		faust_interface->declare(&fslider9, "2", "");
		faust_interface->declare(&fslider9, "tooltip", "Frequency of the sine wave for the modulation of theta (works if Modulation Type=3)");
		faust_interface->declare(&fslider9, "unit", "Hz");
		faust_interface->addHorizontalSlider("Modulation_Frequency", &fslider9, 2.2e+02f, 2e+01f, 1e+03f, 0.1f);
		faust_interface->declare(&fentry2, "2", "");
		faust_interface->declare(&fentry2, "tooltip", "0=theta is modulated by the incoming signal; 1=theta is modulated by the averaged incoming signal; 2=theta is modulated by the squared incoming signal; 3=theta is modulated by a sine wave of frequency freqMod; 4=theta is modulated by a sine wave of frequency freq;");
		faust_interface->addNumEntry("Modulation_Type", &fentry2, 0.0f, 0.0f, 4.0f, 1.0f);
		faust_interface->declare(&fslider4, "2", "");
		faust_interface->declare(&fslider4, "tooltip", "Nonlinearity factor (value between 0 and 1)");
		faust_interface->addHorizontalSlider("Nonlinearity", &fslider4, 0.0f, 0.0f, 1.0f, 0.01f);
		faust_interface->declare(&fslider8, "2", "");
		faust_interface->declare(&fslider8, "Attack duration of the nonlinearity", "");
		faust_interface->declare(&fslider8, "unit", "s");
		faust_interface->addHorizontalSlider("Nonlinearity_Attack", &fslider8, 0.1f, 0.0f, 2.0f, 0.01f);
		faust_interface->closeBox();
		faust_interface->closeBox();
		faust_interface->openVerticalBox("Spat");
		faust_interface->addHorizontalSlider("pan angle", &fslider10, 0.6f, 0.0f, 1.0f, 0.01f);
		faust_interface->addHorizontalSlider("spatial width", &fslider11, 0.5f, 0.0f, 1.0f, 0.01f);
		faust_interface->closeBox();
		faust_interface->closeBox();
	}
	virtual void compute (int count, FAUSTFLOAT** input, FAUSTFLOAT** output) {
		float 	fSlow0 = (fConst1 * float(fslider0));
		float 	fSlow1 = float(fbutton0);
		int 	iSlow2 = (fSlow1 > 0);
		int 	iSlow3 = (fSlow1 <= 0);
		float 	fSlow4 = float(fslider1);
		float 	fSlow5 = (1 - (1.0f / powf(1e+05f,(1.0f / ((fSlow4 == 0.0f) + (iConst0 * fSlow4))))));
		float 	fSlow6 = float(fslider2);
		float 	fSlow7 = (fConst2 * fSlow6);
		float 	fSlow8 = (fSlow7 + ((0.2f * fSlow6) == 0.0f));
		float 	fSlow9 = (1.0f / ((fConst3 * fSlow6) + ((1.8f * fSlow6) == 0.0f)));
		float 	fSlow10 = float(fslider3);
		float 	fSlow11 = float(fentry0);
		float 	fSlow12 = (fConst1 * fSlow11);
		float 	fSlow13 = (0.0010000000000000009f * float(fslider4));
		float 	fSlow14 = (0.0010000000000000009f * float(fentry1));
		float 	fSlow15 = float(fslider5);
		float 	fSlow16 = (1.0f / ((fSlow15 == 0.0f) + (iConst0 * fSlow15)));
		float 	fSlow17 = (1 - (1.0f / powf(9e+04f,fSlow16)));
		float 	fSlow18 = float(fslider6);
		float 	fSlow19 = (1 - powf(9e+01f,(1.0f / ((fSlow18 == 0.0f) + (iConst0 * fSlow18)))));
		float 	fSlow20 = float(fslider7);
		float 	fSlow21 = (1.0f / ((fSlow20 == 0.0f) + (iConst0 * fSlow20)));
		float 	fSlow22 = float(fentry2);
		float 	fSlow23 = (3.141592653589793f * (fSlow22 == 2));
		float 	fSlow24 = (1.5707963267948966f * (fSlow22 == 1));
		float 	fSlow25 = (3.141592653589793f * (fSlow22 == 0));
		float 	fSlow26 = (1 - (1.0f / powf(1e+05f,fSlow16)));
		float 	fSlow27 = float(fslider8);
		float 	fSlow28 = (1.0f / ((fSlow27 == 0.0f) + (iConst0 * fSlow27)));
		int 	iSlow29 = (fSlow22 < 3);
		float 	fSlow30 = (0.0010000000000000009f * float(fslider9));
		int 	iSlow31 = (fSlow22 != 4);
		float 	fSlow32 = (fSlow11 * (fSlow22 == 4));
		int 	iSlow33 = (fSlow22 >= 3);
		float 	fSlow34 = float(fslider10);
		float 	fSlow35 = (1.0f - fSlow34);
		int 	iSlow36 = int((int((fConst4 * (float(fslider11) / fSlow11))) & 4095));
		FAUSTFLOAT* output0 = output[0];
		FAUSTFLOAT* output1 = output[1];
		for (int i=0; i<count; i++) {
			float fTemp0 = (fRec1[1] + fSlow0);
			fRec1[0] = (fTemp0 - floorf(fTemp0));
			iRec2[0] = (iSlow2 & (iRec2[1] | (fRec4[1] >= 1)));
			iRec3[0] = (iSlow2 * (1 + iRec3[1]));
			int iTemp1 = (iSlow3 & (fRec4[1] > 0));
			fRec4[0] = (((fSlow9 * (((((iRec2[1] == 0) & iSlow2) & (fRec4[1] < 1)) & (iRec3[1] > fSlow7)) * (1 - (iRec3[1] < fSlow8)))) + (fRec4[1] * (1 - (fSlow5 * iTemp1)))) * ((iTemp1 == 0) | (fRec4[1] >= 1e-06f)));
			float fTemp2 = (1 + (fSlow10 * (fRec4[0] * ftbl0[int((65536.0f * fRec1[0]))])));
			float fTemp3 = (fRec5[1] + fSlow12);
			fRec5[0] = (fTemp3 - floorf(fTemp3));
			float fTemp4 = ftbl0[int((65536.0f * fRec5[0]))];
			fRec6[0] = (fSlow13 + (0.999f * fRec6[1]));
			fRec7[0] = ((0.999f * fRec7[1]) + fSlow14);
			iRec8[0] = (iSlow2 & (iRec8[1] | (fRec9[1] >= 1)));
			int iTemp5 = (iSlow3 & (fRec9[1] > 0));
			fRec9[0] = (((fSlow21 * (((iRec8[1] == 0) & iSlow2) & (fRec9[1] < 1))) + (fRec9[1] * ((1 - (fSlow19 * (iRec8[1] & (fRec9[1] > 90)))) - (fSlow17 * iTemp5)))) * ((iTemp5 == 0) | (fRec9[1] >= 1e-06f)));
			float fTemp6 = (fRec9[0] * fRec7[0]);
			float fTemp7 = ((fTemp6 * fTemp4) * fTemp2);
			fVec0[0] = fTemp7;
			iRec10[0] = (iSlow2 & (iRec10[1] | (fRec11[1] >= 1)));
			int iTemp8 = (iSlow3 & (fRec11[1] > 0));
			fRec11[0] = (((fSlow28 * (((iRec10[1] == 0) & iSlow2) & (fRec11[1] < 1))) + (fRec11[1] * (1 - (fSlow26 * iTemp8)))) * ((iTemp8 == 0) | (fRec11[1] >= 1e-06f)));
			float fTemp9 = (fRec6[0] * fRec11[0]);
			float fTemp10 = (fTemp9 * (((fSlow25 * fVec0[0]) + (fSlow24 * (fVec0[0] + fVec0[1]))) + (fSlow23 * (((faustpower<2>(fRec9[0]) * faustpower<2>(fRec7[0])) * faustpower<2>(fTemp4)) * faustpower<2>(fTemp2)))));
			float fTemp11 = cosf(fTemp10);
			float fTemp12 = sinf(fTemp10);
			float fTemp13 = (0 - fTemp12);
			float fTemp14 = ((fRec12[1] * fTemp13) + (fVec0[0] * fTemp11));
			float fTemp15 = ((fTemp13 * fRec13[1]) + (fTemp11 * fTemp14));
			fRec14[0] = ((fTemp13 * fRec14[1]) + (fTemp11 * fTemp15));
			fRec13[0] = ((fTemp12 * fTemp15) + (fTemp11 * fRec14[1]));
			fRec12[0] = ((fTemp12 * fTemp14) + (fTemp11 * fRec13[1]));
			fRec16[0] = (fSlow30 + (0.999f * fRec16[1]));
			float fTemp16 = (fRec15[1] + (fConst1 * (fSlow32 + (iSlow31 * fRec16[0]))));
			fRec15[0] = (fTemp16 - floorf(fTemp16));
			float fTemp17 = (3.141592653589793f * (fTemp9 * ftbl0[int((65536.0f * fRec15[0]))]));
			float fTemp18 = cosf(fTemp17);
			float fTemp19 = sinf(fTemp17);
			float fTemp20 = (0 - fTemp19);
			float fTemp21 = ((fRec17[1] * fTemp20) + (fVec0[0] * fTemp18));
			float fTemp22 = ((fTemp20 * fRec18[1]) + (fTemp18 * fTemp21));
			fRec19[0] = ((fTemp20 * fRec19[1]) + (fTemp18 * fTemp22));
			fRec18[0] = ((fTemp19 * fTemp22) + (fTemp18 * fRec19[1]));
			fRec17[0] = ((fTemp19 * fTemp21) + (fTemp18 * fRec18[1]));
			float fTemp23 = ((iSlow33 * ((fVec0[0] * fTemp19) + (fRec17[1] * fTemp18))) + (iSlow29 * ((fRec6[0] * ((fVec0[0] * fTemp12) + (fRec12[1] * fTemp11))) + (((fTemp6 * (1 - fRec6[0])) * fTemp4) * fTemp2))));
			fVec1[IOTA&4095] = fTemp23;
			output0[i] = (FAUSTFLOAT)(fSlow35 * fVec1[IOTA&4095]);
			output1[i] = (FAUSTFLOAT)(fSlow34 * fVec1[(IOTA-iSlow36)&4095]);
			// post processing
			IOTA = IOTA+1;
			fRec17[1] = fRec17[0];
			fRec18[1] = fRec18[0];
			fRec19[1] = fRec19[0];
			fRec15[1] = fRec15[0];
			fRec16[1] = fRec16[0];
			fRec12[1] = fRec12[0];
			fRec13[1] = fRec13[0];
			fRec14[1] = fRec14[0];
			fRec11[1] = fRec11[0];
			iRec10[1] = iRec10[0];
			fVec0[1] = fVec0[0];
			fRec9[1] = fRec9[0];
			iRec8[1] = iRec8[0];
			fRec7[1] = fRec7[0];
			fRec6[1] = fRec6[0];
			fRec5[1] = fRec5[0];
			fRec4[1] = fRec4[0];
			iRec3[1] = iRec3[0];
			iRec2[1] = iRec2[0];
			fRec1[1] = fRec1[0];
		}
	}
};


float 	NLF_Fm_dsp::ftbl0[65536];


#include "Faust_plugins_template2.cpp"

