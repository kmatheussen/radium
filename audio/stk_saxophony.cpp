//-----------------------------------------------------
// name: "Saxophone"
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
#define FAUSTCLASS Saxophony_dsp
#endif

class Saxophony_dsp : public dsp {
  private:
	class SIG0 {
	  private:
		int 	fSamplingFreq;
		int 	iRec12[2];
	  public:
		int getNumInputs() 	{ return 0; }
		int getNumOutputs() 	{ return 1; }
		void init(int samplingFreq) {
			fSamplingFreq = samplingFreq;
			for (int i=0; i<2; i++) iRec12[i] = 0;
		}
		void fill (int count, float output[]) {
			for (int i=0; i<count; i++) {
				iRec12[0] = (1 + iRec12[1]);
				output[i] = sinf((9.587379924285257e-05f * float((iRec12[0] - 1))));
				// post processing
				iRec12[1] = iRec12[0];
			}
		}
	};


	FAUSTFLOAT 	fslider0;
	FAUSTFLOAT 	fentry0;
	int 	iConst0;
	float 	fVec0[2];
	FAUSTFLOAT 	fslider1;
	float 	fRec2[2];
	FAUSTFLOAT 	fentry1;
	FAUSTFLOAT 	fbutton0;
	float 	fRec5[2];
	int 	iRec3[2];
	FAUSTFLOAT 	fslider2;
	FAUSTFLOAT 	fslider3;
	float 	fRec4[2];
	float 	fRec11[2];
	float 	fRec10[2];
	float 	fRec9[2];
	float 	fRec8[2];
	float 	fRec7[2];
	float 	fRec6[2];
	static float 	ftbl0[65536];
	FAUSTFLOAT 	fslider4;
	float 	fRec14[2];
	float 	fConst1;
	float 	fRec13[2];
	float 	fRec20[2];
	float 	fRec19[2];
	float 	fRec18[2];
	float 	fRec17[2];
	float 	fRec16[2];
	float 	fRec15[2];
	float 	fVec1[2];
	int 	IOTA;
	float 	fVec2[4096];
	FAUSTFLOAT 	fslider5;
	float 	fRec21[2];
	FAUSTFLOAT 	fslider6;
	int 	iRec22[2];
	FAUSTFLOAT 	fslider7;
	int 	iRec23[2];
	FAUSTFLOAT 	fslider8;
	FAUSTFLOAT 	fslider9;
	float 	fRec24[2];
	FAUSTFLOAT 	fslider10;
	float 	fRec0[8192];
	FAUSTFLOAT 	fslider11;
	FAUSTFLOAT 	fentry2;
	float 	fVec3[4096];
	FAUSTFLOAT 	fslider12;
	float 	fConst2;
  public:
	static void metadata(Meta* m) 	{ 
		m->declare("name", "Saxophone");
		m->declare("description", "Nonlinear WaveGuide Saxophone");
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
		fslider0 = 0.5f;
		fentry0 = 4.4e+02f;
		iConst0 = min(192000, max(1, fSamplingFreq));
		for (int i=0; i<2; i++) fVec0[i] = 0;
		fslider1 = 0.0f;
		for (int i=0; i<2; i++) fRec2[i] = 0;
		fentry1 = 0.0f;
		fbutton0 = 0.0;
		for (int i=0; i<2; i++) fRec5[i] = 0;
		for (int i=0; i<2; i++) iRec3[i] = 0;
		fslider2 = 0.01f;
		fslider3 = 0.1f;
		for (int i=0; i<2; i++) fRec4[i] = 0;
		for (int i=0; i<2; i++) fRec11[i] = 0;
		for (int i=0; i<2; i++) fRec10[i] = 0;
		for (int i=0; i<2; i++) fRec9[i] = 0;
		for (int i=0; i<2; i++) fRec8[i] = 0;
		for (int i=0; i<2; i++) fRec7[i] = 0;
		for (int i=0; i<2; i++) fRec6[i] = 0;
		fslider4 = 2.2e+02f;
		for (int i=0; i<2; i++) fRec14[i] = 0;
		fConst1 = (1.0f / float(iConst0));
		for (int i=0; i<2; i++) fRec13[i] = 0;
		for (int i=0; i<2; i++) fRec20[i] = 0;
		for (int i=0; i<2; i++) fRec19[i] = 0;
		for (int i=0; i<2; i++) fRec18[i] = 0;
		for (int i=0; i<2; i++) fRec17[i] = 0;
		for (int i=0; i<2; i++) fRec16[i] = 0;
		for (int i=0; i<2; i++) fRec15[i] = 0;
		for (int i=0; i<2; i++) fVec1[i] = 0;
		IOTA = 0;
		for (int i=0; i<4096; i++) fVec2[i] = 0;
		fslider5 = 6.0f;
		for (int i=0; i<2; i++) fRec21[i] = 0;
		fslider6 = 0.1f;
		for (int i=0; i<2; i++) iRec22[i] = 0;
		fslider7 = 0.05f;
		for (int i=0; i<2; i++) iRec23[i] = 0;
		fslider8 = 1.0f;
		fslider9 = 0.05f;
		for (int i=0; i<2; i++) fRec24[i] = 0;
		fslider10 = 0.3f;
		for (int i=0; i<8192; i++) fRec0[i] = 0;
		fslider11 = 0.6f;
		fentry2 = 1.0f;
		for (int i=0; i<4096; i++) fVec3[i] = 0;
		fslider12 = 0.5f;
		fConst2 = (0.5f * iConst0);
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
		faust_interface->declare(&fslider9, "5", "");
		faust_interface->declare(&fslider9, "tooltip", "Envelope attack duration");
		faust_interface->declare(&fslider9, "unit", "s");
		faust_interface->addHorizontalSlider("Envelope_Attack", &fslider9, 0.05f, 0.0f, 2.0f, 0.01f);
		faust_interface->declare(&fslider2, "5", "");
		faust_interface->declare(&fslider2, "tooltip", "Envelope release duration");
		faust_interface->declare(&fslider2, "unit", "s");
		faust_interface->addHorizontalSlider("Envelope_Release", &fslider2, 0.01f, 0.0f, 2.0f, 0.01f);
		faust_interface->closeBox();
		faust_interface->openVerticalBox("Vibrato_Parameters");
		faust_interface->declare(&fslider5, "4", "");
		faust_interface->declare(&fslider5, "unit", "Hz");
		faust_interface->addHorizontalSlider("Vibrato_Freq", &fslider5, 6.0f, 1.0f, 15.0f, 0.1f);
		faust_interface->declare(&fslider6, "4", "");
		faust_interface->declare(&fslider6, "tooltip", "A value between 0 and 1");
		faust_interface->addHorizontalSlider("Vibrato_Gain", &fslider6, 0.1f, 0.0f, 1.0f, 0.01f);
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
		faust_interface->declare(&fslider0, "tooltip", "A value between 0 and 1");
		faust_interface->addHorizontalSlider("Blow_Position", &fslider0, 0.5f, 0.0f, 1.0f, 0.01f);
		faust_interface->addHorizontalSlider("Noise_Gain", &fslider7, 0.05f, 0.0f, 1.0f, 0.01f);
		faust_interface->declare(&fslider8, "2", "");
		faust_interface->declare(&fslider8, "tooltip", "Breath pressure (a value between 0 and 1)");
		faust_interface->addHorizontalSlider("Pressure", &fslider8, 1.0f, 0.0f, 1.0f, 0.01f);
		faust_interface->declare(&fslider10, "2", "");
		faust_interface->declare(&fslider10, "tooltip", "A value between 0 and 1");
		faust_interface->addHorizontalSlider("Reed_Stiffness", &fslider10, 0.3f, 0.0f, 1.0f, 0.01f);
		faust_interface->closeBox();
		faust_interface->closeBox();
		faust_interface->openVerticalBox("Spat");
		faust_interface->addHorizontalSlider("pan angle", &fslider11, 0.6f, 0.0f, 1.0f, 0.01f);
		faust_interface->addHorizontalSlider("spatial width", &fslider12, 0.5f, 0.0f, 1.0f, 0.01f);
		faust_interface->closeBox();
		faust_interface->closeBox();
	}
	virtual void compute (int count, FAUSTFLOAT** input, FAUSTFLOAT** output) {
		float 	fSlow0 = float(fslider0);
		float 	fSlow1 = float(fentry0);
		float 	fSlow2 = ((float(iConst0) / fSlow1) - 3);
		float 	fSlow3 = (fSlow2 * (1 - fSlow0));
		int 	iSlow4 = int(fSlow3);
		int 	iSlow5 = (1 + iSlow4);
		int 	iSlow6 = int((1 + int((int(iSlow5) & 4095))));
		float 	fSlow7 = (fSlow3 - iSlow4);
		int 	iSlow8 = int((1 + int((iSlow4 & 4095))));
		float 	fSlow9 = (iSlow5 - fSlow3);
		float 	fSlow10 = (0.0010000000000000009f * float(fslider1));
		float 	fSlow11 = float(fentry1);
		float 	fSlow12 = (3.141592653589793f * (fSlow11 == 2));
		float 	fSlow13 = (1.5707963267948966f * (fSlow11 == 1));
		float 	fSlow14 = (3.141592653589793f * (fSlow11 == 0));
		float 	fSlow15 = (0.0010000000000000009f * float(fbutton0));
		float 	fSlow16 = float(fslider2);
		float 	fSlow17 = (1 - (1.0f / powf(1e+05f,(1.0f / ((fSlow16 == 0.0f) + (iConst0 * fSlow16))))));
		float 	fSlow18 = float(fslider3);
		float 	fSlow19 = (1.0f / ((fSlow18 == 0.0f) + (iConst0 * fSlow18)));
		int 	iSlow20 = (fSlow11 < 3);
		float 	fSlow21 = (0.0010000000000000009f * float(fslider4));
		int 	iSlow22 = (fSlow11 != 4);
		float 	fSlow23 = (fSlow1 * (fSlow11 == 4));
		int 	iSlow24 = (fSlow11 >= 3);
		float 	fSlow25 = (fSlow2 * fSlow0);
		float 	fSlow26 = (1 + fSlow25);
		int 	iSlow27 = int(fSlow26);
		int 	iSlow28 = int((int((1 + iSlow27)) & 4095));
		float 	fSlow29 = (fSlow26 - iSlow27);
		int 	iSlow30 = int((iSlow27 & 4095));
		float 	fSlow31 = (iSlow27 - fSlow25);
		float 	fSlow32 = (fConst1 * float(fslider5));
		float 	fSlow33 = float(fslider6);
		float 	fSlow34 = (4.656612875245797e-10f * float(fslider7));
		float 	fSlow35 = float(fslider8);
		float 	fSlow36 = (fSlow16 * fSlow35);
		float 	fSlow37 = (1 - (1.0f / powf(1e+05f,(1.0f / ((iConst0 * fSlow36) + (fSlow36 == 0.0f))))));
		float 	fSlow38 = (float(fslider9) * fSlow35);
		float 	fSlow39 = (1.0f / ((iConst0 * fSlow38) + (fSlow38 == 0.0f)));
		float 	fSlow40 = (0.55f + (0.3f * fSlow35));
		float 	fSlow41 = (0.1f + (0.4f * float(fslider10)));
		float 	fSlow42 = float(fslider11);
		float 	fSlow43 = float(fentry2);
		float 	fSlow44 = (fSlow43 * (1.0f - fSlow42));
		int 	iSlow45 = int((int((fConst2 * (float(fslider12) / fSlow1))) & 4095));
		FAUSTFLOAT* output0 = output[0];
		FAUSTFLOAT* output1 = output[1];
		for (int i=0; i<count; i++) {
			float fTemp0 = ((fSlow9 * fRec0[(IOTA-iSlow8)&8191]) + (fSlow7 * fRec0[(IOTA-iSlow6)&8191]));
			fVec0[0] = fTemp0;
			fRec2[0] = (fSlow10 + (0.999f * fRec2[1]));
			fRec5[0] = (fSlow15 + (0.999f * fRec5[1]));
			int iTemp1 = (fRec5[0] > 0);
			iRec3[0] = (iTemp1 & (iRec3[1] | (fRec4[1] >= 1)));
			int iTemp2 = (fRec5[0] <= 0);
			int iTemp3 = (iTemp2 & (fRec4[1] > 0));
			fRec4[0] = (((fSlow19 * (((iRec3[1] == 0) & iTemp1) & (fRec4[1] < 1))) + (fRec4[1] * (1 - (fSlow17 * iTemp3)))) * ((iTemp3 == 0) | (fRec4[1] >= 1e-06f)));
			float fTemp4 = (fRec2[0] * fRec4[0]);
			float fTemp5 = (fTemp4 * (((fSlow14 * fVec0[0]) + (fSlow13 * (fVec0[0] + fVec0[1]))) + (fSlow12 * faustpower<2>(fVec0[0]))));
			float fTemp6 = cosf(fTemp5);
			float fTemp7 = sinf(fTemp5);
			float fTemp8 = (0 - fTemp7);
			float fTemp9 = ((fRec6[1] * fTemp8) + (fVec0[0] * fTemp6));
			float fTemp10 = ((fTemp8 * fRec7[1]) + (fTemp6 * fTemp9));
			float fTemp11 = ((fTemp8 * fRec8[1]) + (fTemp6 * fTemp10));
			float fTemp12 = ((fTemp8 * fRec9[1]) + (fTemp6 * fTemp11));
			float fTemp13 = ((fTemp8 * fRec10[1]) + (fTemp6 * fTemp12));
			fRec11[0] = ((fTemp8 * fRec11[1]) + (fTemp6 * fTemp13));
			fRec10[0] = ((fTemp7 * fTemp13) + (fTemp6 * fRec11[1]));
			fRec9[0] = ((fTemp7 * fTemp12) + (fTemp6 * fRec10[1]));
			fRec8[0] = ((fTemp7 * fTemp11) + (fTemp6 * fRec9[1]));
			fRec7[0] = ((fTemp7 * fTemp10) + (fTemp6 * fRec8[1]));
			fRec6[0] = ((fTemp7 * fTemp9) + (fTemp6 * fRec7[1]));
			fRec14[0] = (fSlow21 + (0.999f * fRec14[1]));
			float fTemp14 = (fRec13[1] + (fConst1 * (fSlow23 + (iSlow22 * fRec14[0]))));
			fRec13[0] = (fTemp14 - floorf(fTemp14));
			float fTemp15 = (3.141592653589793f * (fTemp4 * ftbl0[int((65536.0f * fRec13[0]))]));
			float fTemp16 = cosf(fTemp15);
			float fTemp17 = sinf(fTemp15);
			float fTemp18 = (0 - fTemp17);
			float fTemp19 = ((fRec15[1] * fTemp18) + (fVec0[0] * fTemp16));
			float fTemp20 = ((fTemp18 * fRec16[1]) + (fTemp16 * fTemp19));
			float fTemp21 = ((fTemp18 * fRec17[1]) + (fTemp16 * fTemp20));
			float fTemp22 = ((fTemp18 * fRec18[1]) + (fTemp16 * fTemp21));
			float fTemp23 = ((fTemp18 * fRec19[1]) + (fTemp16 * fTemp22));
			fRec20[0] = ((fTemp18 * fRec20[1]) + (fTemp16 * fTemp23));
			fRec19[0] = ((fTemp17 * fTemp23) + (fTemp16 * fRec20[1]));
			fRec18[0] = ((fTemp17 * fTemp22) + (fTemp16 * fRec19[1]));
			fRec17[0] = ((fTemp17 * fTemp21) + (fTemp16 * fRec18[1]));
			fRec16[0] = ((fTemp17 * fTemp20) + (fTemp16 * fRec17[1]));
			fRec15[0] = ((fTemp17 * fTemp19) + (fTemp16 * fRec16[1]));
			float fTemp24 = (0 - (0.95f * ((iSlow24 * ((fVec0[0] * fTemp17) + (fRec15[1] * fTemp16))) + (iSlow20 * ((fRec2[0] * ((fVec0[0] * fTemp7) + (fRec6[1] * fTemp6))) + ((1 - fRec2[0]) * fVec0[0]))))));
			fVec1[0] = fTemp24;
			float fTemp25 = (fVec1[0] + fVec1[1]);
			fVec2[IOTA&4095] = fTemp25;
			float fTemp26 = (0.5f * fVec2[IOTA&4095]);
			float fTemp27 = (0.5f * ((fSlow31 * fVec2[(IOTA-iSlow30)&4095]) + (fSlow29 * fVec2[(IOTA-iSlow28)&4095])));
			float fTemp28 = (fRec21[1] + fSlow32);
			fRec21[0] = (fTemp28 - floorf(fTemp28));
			iRec22[0] = (12345 + (1103515245 * iRec22[1]));
			iRec23[0] = (iTemp1 & (iRec23[1] | (fRec24[1] >= 1)));
			int iTemp29 = (iTemp2 & (fRec24[1] > 0));
			fRec24[0] = (((fSlow39 * (((iRec23[1] == 0) & iTemp1) & (fRec24[1] < 1))) + (fRec24[1] * (1 - (fSlow37 * iTemp29)))) * ((iTemp29 == 0) | (fRec24[1] >= 1e-06f)));
			float fTemp30 = (fSlow40 * ((fRec24[0] * (1 + (fSlow34 * iRec22[0]))) * (1 + (fSlow33 * ftbl0[int((65536.0f * fRec21[0]))]))));
			float fTemp31 = ((fTemp30 + fTemp27) - fTemp26);
			float fTemp32 = (0.7f + (fSlow41 * fTemp31));
			float fTemp33 = ((fTemp32 > 1) + (fTemp32 * (fTemp32 <= 1)));
			fRec0[IOTA&8191] = (fTemp30 - (fTemp26 + (fTemp31 * ((fTemp33 * (fTemp33 >= -1)) - (fTemp33 < -1)))));
			float 	fRec1 = (fTemp26 - fTemp27);
			output0[i] = (FAUSTFLOAT)(fSlow44 * fRec1);
			fVec3[IOTA&4095] = (fSlow43 * fRec1);
			output1[i] = (FAUSTFLOAT)(fSlow42 * fVec3[(IOTA-iSlow45)&4095]);
			// post processing
			fRec24[1] = fRec24[0];
			iRec23[1] = iRec23[0];
			iRec22[1] = iRec22[0];
			fRec21[1] = fRec21[0];
			IOTA = IOTA+1;
			fVec1[1] = fVec1[0];
			fRec15[1] = fRec15[0];
			fRec16[1] = fRec16[0];
			fRec17[1] = fRec17[0];
			fRec18[1] = fRec18[0];
			fRec19[1] = fRec19[0];
			fRec20[1] = fRec20[0];
			fRec13[1] = fRec13[0];
			fRec14[1] = fRec14[0];
			fRec6[1] = fRec6[0];
			fRec7[1] = fRec7[0];
			fRec8[1] = fRec8[0];
			fRec9[1] = fRec9[0];
			fRec10[1] = fRec10[0];
			fRec11[1] = fRec11[0];
			fRec4[1] = fRec4[0];
			iRec3[1] = iRec3[0];
			fRec5[1] = fRec5[0];
			fRec2[1] = fRec2[0];
			fVec0[1] = fVec0[0];
		}
	}
};


float 	Saxophony_dsp::ftbl0[65536];


#include "Faust_plugins_template2.cpp"

