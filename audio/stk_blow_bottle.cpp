//-----------------------------------------------------
// name: "BlowBottle"
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
#define FAUSTCLASS Blow_Bottle_dsp
#endif

class Blow_Bottle_dsp : public dsp {
  private:
	class SIG0 {
	  private:
		int 	fSamplingFreq;
		int 	iRec16[2];
	  public:
		int getNumInputs() 	{ return 0; }
		int getNumOutputs() 	{ return 1; }
		void init(int samplingFreq) {
			fSamplingFreq = samplingFreq;
			for (int i=0; i<2; i++) iRec16[i] = 0;
		}
		void fill (int count, float output[]) {
			for (int i=0; i<count; i++) {
				iRec16[0] = (1 + iRec16[1]);
				output[i] = sinf((9.587379924285257e-05f * float((iRec16[0] - 1))));
				// post processing
				iRec16[1] = iRec16[0];
			}
		}
	};


	FAUSTFLOAT 	fbutton0;
	int 	iRec0[2];
	FAUSTFLOAT 	fslider0;
	int 	iConst0;
	FAUSTFLOAT 	fslider1;
	FAUSTFLOAT 	fentry0;
	float 	fRec2[2];
	FAUSTFLOAT 	fslider2;
	float 	fRec1[2];
	FAUSTFLOAT 	fslider3;
	float 	fRec7[2];
	FAUSTFLOAT 	fentry1;
	int 	iRec8[2];
	FAUSTFLOAT 	fslider4;
	float 	fRec9[2];
	float 	fRec15[2];
	float 	fRec14[2];
	float 	fRec13[2];
	float 	fRec12[2];
	float 	fRec11[2];
	float 	fRec10[2];
	static float 	ftbl0[65536];
	FAUSTFLOAT 	fslider5;
	float 	fRec18[2];
	FAUSTFLOAT 	fentry2;
	float 	fConst1;
	float 	fRec17[2];
	float 	fRec24[2];
	float 	fRec23[2];
	float 	fRec22[2];
	float 	fRec21[2];
	float 	fRec20[2];
	float 	fRec19[2];
	FAUSTFLOAT 	fslider6;
	float 	fRec25[2];
	int 	iRec26[2];
	int 	iRec27[2];
	FAUSTFLOAT 	fslider7;
	FAUSTFLOAT 	fslider8;
	FAUSTFLOAT 	fslider9;
	float 	fRec28[2];
	FAUSTFLOAT 	fslider10;
	int 	iRec29[2];
	float 	fConst2;
	float 	fConst3;
	float 	fConst4;
	float 	fRec30[2];
	FAUSTFLOAT 	fslider11;
	int 	iRec31[2];
	FAUSTFLOAT 	fslider12;
	float 	fConst5;
	float 	fRec6[3];
	float 	fRec4[3];
	float 	fRec5[2];
	float 	fRec3[2];
	int 	IOTA;
	float 	fVec0[4096];
	FAUSTFLOAT 	fslider13;
	FAUSTFLOAT 	fslider14;
	float 	fConst6;
  public:
	static void metadata(Meta* m) 	{ 
		m->declare("name", "BlowBottle");
		m->declare("description", "Blown Bottle Instrument");
		m->declare("author", "Romain Michon (rmichon@ccrma.stanford.edu)");
		m->declare("copyright", "Romain Michon");
		m->declare("version", "1.0");
		m->declare("licence", "STK-4.3");
		m->declare("math.lib/name", "Math Library");
		m->declare("math.lib/author", "GRAME");
		m->declare("math.lib/copyright", "GRAME");
		m->declare("math.lib/version", "1.0");
		m->declare("math.lib/license", "LGPL with exception");
		m->declare("music.lib/name", "Music Library");
		m->declare("music.lib/author", "GRAME");
		m->declare("music.lib/copyright", "GRAME");
		m->declare("music.lib/version", "1.0");
		m->declare("music.lib/license", "LGPL with exception");
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
		for (int i=0; i<2; i++) iRec0[i] = 0;
		fslider0 = 0.5f;
		iConst0 = min(192000, max(1, fSamplingFreq));
		fslider1 = 0.01f;
		fentry0 = 1.0f;
		for (int i=0; i<2; i++) fRec2[i] = 0;
		fslider2 = 0.01f;
		for (int i=0; i<2; i++) fRec1[i] = 0;
		fslider3 = 0.0f;
		for (int i=0; i<2; i++) fRec7[i] = 0;
		fentry1 = 0.0f;
		for (int i=0; i<2; i++) iRec8[i] = 0;
		fslider4 = 0.1f;
		for (int i=0; i<2; i++) fRec9[i] = 0;
		for (int i=0; i<2; i++) fRec15[i] = 0;
		for (int i=0; i<2; i++) fRec14[i] = 0;
		for (int i=0; i<2; i++) fRec13[i] = 0;
		for (int i=0; i<2; i++) fRec12[i] = 0;
		for (int i=0; i<2; i++) fRec11[i] = 0;
		for (int i=0; i<2; i++) fRec10[i] = 0;
		fslider5 = 2.2e+02f;
		for (int i=0; i<2; i++) fRec18[i] = 0;
		fentry2 = 4.4e+02f;
		fConst1 = (1.0f / float(iConst0));
		for (int i=0; i<2; i++) fRec17[i] = 0;
		for (int i=0; i<2; i++) fRec24[i] = 0;
		for (int i=0; i<2; i++) fRec23[i] = 0;
		for (int i=0; i<2; i++) fRec22[i] = 0;
		for (int i=0; i<2; i++) fRec21[i] = 0;
		for (int i=0; i<2; i++) fRec20[i] = 0;
		for (int i=0; i<2; i++) fRec19[i] = 0;
		fslider6 = 5.0f;
		for (int i=0; i<2; i++) fRec25[i] = 0;
		for (int i=0; i<2; i++) iRec26[i] = 0;
		for (int i=0; i<2; i++) iRec27[i] = 0;
		fslider7 = 0.01f;
		fslider8 = 0.05f;
		fslider9 = 0.5f;
		for (int i=0; i<2; i++) fRec28[i] = 0;
		fslider10 = 0.1f;
		for (int i=0; i<2; i++) iRec29[i] = 0;
		fConst2 = (0.2f * iConst0);
		fConst3 = (1 - powf(8e+01f,(1e+02f / float(iConst0))));
		fConst4 = (0.02f * iConst0);
		for (int i=0; i<2; i++) fRec30[i] = 0;
		fslider11 = 1.0f;
		for (int i=0; i<2; i++) iRec31[i] = 0;
		fslider12 = 0.5f;
		fConst5 = (6.283185307179586f / float(iConst0));
		for (int i=0; i<3; i++) fRec6[i] = 0;
		for (int i=0; i<3; i++) fRec4[i] = 0;
		for (int i=0; i<2; i++) fRec5[i] = 0;
		for (int i=0; i<2; i++) fRec3[i] = 0;
		IOTA = 0;
		for (int i=0; i<4096; i++) fVec0[i] = 0;
		fslider13 = 0.6f;
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
		faust_interface->declare(&fslider2, "5", "");
		faust_interface->declare(&fslider2, "tooltip", "Envelope attack duration");
		faust_interface->declare(&fslider2, "unit", "s");
		faust_interface->addHorizontalSlider("Envelope_Attack", &fslider2, 0.01f, 0.0f, 2.0f, 0.01f);
		faust_interface->declare(&fslider1, "5", "");
		faust_interface->declare(&fslider1, "tooltip", "Envelope decay duration");
		faust_interface->declare(&fslider1, "unit", "s");
		faust_interface->addHorizontalSlider("Envelope_Decay", &fslider1, 0.01f, 0.0f, 2.0f, 0.01f);
		faust_interface->declare(&fslider0, "5", "");
		faust_interface->declare(&fslider0, "tooltip", "Envelope release duration");
		faust_interface->declare(&fslider0, "unit", "s");
		faust_interface->addHorizontalSlider("Envelope_Release", &fslider0, 0.5f, 0.0f, 2.0f, 0.01f);
		faust_interface->closeBox();
		faust_interface->openVerticalBox("Vibrato_Parameters");
		faust_interface->declare(&fslider9, "4", "");
		faust_interface->declare(&fslider9, "tooltip", "Vibrato attack duration");
		faust_interface->declare(&fslider9, "unit", "s");
		faust_interface->addHorizontalSlider("Vibrato_Attack", &fslider9, 0.5f, 0.0f, 2.0f, 0.01f);
		faust_interface->declare(&fslider8, "4", "");
		faust_interface->declare(&fslider8, "tooltip", "Vibrato silence duration before attack");
		faust_interface->declare(&fslider8, "unit", "s");
		faust_interface->addHorizontalSlider("Vibrato_Begin", &fslider8, 0.05f, 0.0f, 2.0f, 0.01f);
		faust_interface->declare(&fslider6, "4", "");
		faust_interface->declare(&fslider6, "unit", "Hz");
		faust_interface->addHorizontalSlider("Vibrato_Freq", &fslider6, 5.0f, 1.0f, 15.0f, 0.1f);
		faust_interface->declare(&fslider10, "4", "");
		faust_interface->declare(&fslider10, "tooltip", "A value between 0 and 1");
		faust_interface->addHorizontalSlider("Vibrato_Gain", &fslider10, 0.1f, 0.0f, 1.0f, 0.01f);
		faust_interface->declare(&fslider7, "4", "");
		faust_interface->declare(&fslider7, "tooltip", "Vibrato release duration");
		faust_interface->declare(&fslider7, "unit", "s");
		faust_interface->addHorizontalSlider("Vibrato_Release", &fslider7, 0.01f, 0.0f, 2.0f, 0.01f);
		faust_interface->closeBox();
		faust_interface->closeBox();
		faust_interface->openHorizontalBox("Physical_and_Nonlinearity");
		faust_interface->openVerticalBox("Nonlinear_Filter_Parameters");
		faust_interface->declare(&fslider5, "3", "");
		faust_interface->declare(&fslider5, "tooltip", "Frequency of the sine wave for the modulation of theta (works if Modulation Type=3)");
		faust_interface->declare(&fslider5, "unit", "Hz");
		faust_interface->addHorizontalSlider("Modulation_Frequency", &fslider5, 2.2e+02f, 2e+01f, 1e+03f, 0.1f);
		faust_interface->declare(&fentry1, "3", "");
		faust_interface->declare(&fentry1, "tooltip", "0=theta is modulated by the incoming signal; 1=theta is modulated by the averaged incoming signal; 2=theta is modulated by the squared incoming signal; 3=theta is modulated by a sine wave of frequency freqMod; 4=theta is modulated by a sine wave of frequency freq;");
		faust_interface->addNumEntry("Modulation_Type", &fentry1, 0.0f, 0.0f, 4.0f, 1.0f);
		faust_interface->declare(&fslider3, "3", "");
		faust_interface->declare(&fslider3, "tooltip", "Nonlinearity factor (value between 0 and 1)");
		faust_interface->addHorizontalSlider("Nonlinearity", &fslider3, 0.0f, 0.0f, 1.0f, 0.01f);
		faust_interface->declare(&fslider4, "3", "");
		faust_interface->declare(&fslider4, "Attack duration of the nonlinearity", "");
		faust_interface->declare(&fslider4, "unit", "s");
		faust_interface->addHorizontalSlider("Nonlinearity_Attack", &fslider4, 0.1f, 0.0f, 2.0f, 0.01f);
		faust_interface->closeBox();
		faust_interface->openVerticalBox("Physical_Parameters");
		faust_interface->declare(&fslider12, "2", "");
		faust_interface->declare(&fslider12, "tooltip", "Breath noise gain (value between 0 and 1)");
		faust_interface->addHorizontalSlider("Noise_Gain", &fslider12, 0.5f, 0.0f, 1.0f, 0.01f);
		faust_interface->declare(&fslider11, "2", "");
		faust_interface->declare(&fslider11, "tooltip", "Breath pressure (value bewteen 0 and 1)");
		faust_interface->addHorizontalSlider("Pressure", &fslider11, 1.0f, 0.0f, 1.0f, 0.01f);
		faust_interface->closeBox();
		faust_interface->closeBox();
		faust_interface->openVerticalBox("Spat");
		faust_interface->addHorizontalSlider("pan angle", &fslider13, 0.6f, 0.0f, 1.0f, 0.01f);
		faust_interface->addHorizontalSlider("spatial width", &fslider14, 0.5f, 0.0f, 1.0f, 0.01f);
		faust_interface->closeBox();
		faust_interface->closeBox();
	}
	virtual void compute (int count, FAUSTFLOAT** input, FAUSTFLOAT** output) {
		float 	fSlow0 = float(fbutton0);
		int 	iSlow1 = (fSlow0 > 0);
		int 	iSlow2 = (fSlow0 <= 0);
		float 	fSlow3 = float(fslider0);
		float 	fSlow4 = (1.0f / ((fSlow3 == 0.0f) + (iConst0 * fSlow3)));
		float 	fSlow5 = (1 - (1.0f / powf(8e+04f,fSlow4)));
		float 	fSlow6 = float(fslider1);
		float 	fSlow7 = (1 - powf(8e+01f,(1.0f / ((fSlow6 == 0.0f) + (iConst0 * fSlow6)))));
		float 	fSlow8 = (0.0010000000000000009f * float(fentry0));
		float 	fSlow9 = float(fslider2);
		float 	fSlow10 = (iConst0 * fSlow9);
		float 	fSlow11 = (0.0010000000000000009f * float(fslider3));
		float 	fSlow12 = float(fentry1);
		float 	fSlow13 = (3.141592653589793f * (fSlow12 == 2));
		float 	fSlow14 = (1.5707963267948966f * (fSlow12 == 1));
		float 	fSlow15 = (3.141592653589793f * (fSlow12 == 0));
		float 	fSlow16 = (1 - (1.0f / powf(1e+05f,fSlow4)));
		float 	fSlow17 = float(fslider4);
		float 	fSlow18 = (1.0f / ((fSlow17 == 0.0f) + (iConst0 * fSlow17)));
		int 	iSlow19 = (fSlow12 < 3);
		float 	fSlow20 = (0.0010000000000000009f * float(fslider5));
		int 	iSlow21 = (fSlow12 != 4);
		float 	fSlow22 = float(fentry2);
		float 	fSlow23 = (fSlow22 * (fSlow12 == 4));
		int 	iSlow24 = (fSlow12 >= 3);
		float 	fSlow25 = (fConst1 * float(fslider6));
		float 	fSlow26 = float(fslider7);
		float 	fSlow27 = (1 - (1.0f / powf(1e+05f,(1.0f / ((fSlow26 == 0.0f) + (iConst0 * fSlow26))))));
		float 	fSlow28 = float(fslider8);
		float 	fSlow29 = (iConst0 * fSlow28);
		float 	fSlow30 = ((fSlow28 == 0.0f) + fSlow29);
		float 	fSlow31 = float(fslider9);
		float 	fSlow32 = (1.0f / ((fSlow31 == 0.0f) + (iConst0 * fSlow31)));
		float 	fSlow33 = float(fslider10);
		float 	fSlow34 = float(fslider11);
		float 	fSlow35 = (9.313225750491594e-10f * float(fslider12));
		float 	fSlow36 = (0 - (1.998f * cosf((fConst5 * fSlow22))));
		float 	fSlow37 = float(fslider13);
		float 	fSlow38 = (0.5f * (1.0f - fSlow37));
		int 	iSlow39 = int((int((fConst6 * (float(fslider14) / fSlow22))) & 4095));
		float 	fSlow40 = (0.5f * fSlow37);
		FAUSTFLOAT* output0 = output[0];
		FAUSTFLOAT* output1 = output[1];
		for (int i=0; i<count; i++) {
			iRec0[0] = (iSlow1 & (iRec0[1] | (fRec1[1] >= 1)));
			int iTemp0 = (iSlow2 & (fRec1[1] > 0));
			fRec2[0] = (fSlow8 + (0.999f * fRec2[1]));
			fRec1[0] = (((float((((iRec0[1] == 0) & iSlow1) & (fRec1[1] < 1))) / ((fSlow10 * fRec2[0]) + ((fSlow9 * fRec2[0]) == 0.0f))) + (fRec1[1] * ((1 - (fSlow7 * (iRec0[1] & (fRec1[1] > 80)))) - (fSlow5 * iTemp0)))) * ((iTemp0 == 0) | (fRec1[1] >= 1e-06f)));
			fRec7[0] = (fSlow11 + (0.999f * fRec7[1]));
			iRec8[0] = (iSlow1 & (iRec8[1] | (fRec9[1] >= 1)));
			int iTemp1 = (iSlow2 & (fRec9[1] > 0));
			fRec9[0] = (((fSlow18 * (((iRec8[1] == 0) & iSlow1) & (fRec9[1] < 1))) + (fRec9[1] * (1 - (fSlow16 * iTemp1)))) * ((iTemp1 == 0) | (fRec9[1] >= 1e-06f)));
			float fTemp2 = (fRec7[0] * fRec9[0]);
			float fTemp3 = (fTemp2 * (((fSlow15 * fRec4[1]) + (fSlow14 * (fRec4[1] + fRec4[2]))) + (fSlow13 * faustpower<2>(fRec4[1]))));
			float fTemp4 = cosf(fTemp3);
			float fTemp5 = sinf(fTemp3);
			float fTemp6 = (0 - fTemp5);
			float fTemp7 = ((fRec10[1] * fTemp6) + (fRec4[1] * fTemp4));
			float fTemp8 = ((fTemp6 * fRec11[1]) + (fTemp4 * fTemp7));
			float fTemp9 = ((fTemp6 * fRec12[1]) + (fTemp4 * fTemp8));
			float fTemp10 = ((fTemp6 * fRec13[1]) + (fTemp4 * fTemp9));
			float fTemp11 = ((fTemp6 * fRec14[1]) + (fTemp4 * fTemp10));
			fRec15[0] = ((fTemp6 * fRec15[1]) + (fTemp4 * fTemp11));
			fRec14[0] = ((fTemp5 * fTemp11) + (fTemp4 * fRec15[1]));
			fRec13[0] = ((fTemp5 * fTemp10) + (fTemp4 * fRec14[1]));
			fRec12[0] = ((fTemp5 * fTemp9) + (fTemp4 * fRec13[1]));
			fRec11[0] = ((fTemp5 * fTemp8) + (fTemp4 * fRec12[1]));
			fRec10[0] = ((fTemp5 * fTemp7) + (fTemp4 * fRec11[1]));
			fRec18[0] = (fSlow20 + (0.999f * fRec18[1]));
			float fTemp12 = (fRec17[1] + (fConst1 * (fSlow23 + (iSlow21 * fRec18[0]))));
			fRec17[0] = (fTemp12 - floorf(fTemp12));
			float fTemp13 = (3.141592653589793f * (fTemp2 * ftbl0[int((65536.0f * fRec17[0]))]));
			float fTemp14 = cosf(fTemp13);
			float fTemp15 = sinf(fTemp13);
			float fTemp16 = (0 - fTemp15);
			float fTemp17 = ((fRec19[1] * fTemp16) + (fRec4[1] * fTemp14));
			float fTemp18 = ((fTemp16 * fRec20[1]) + (fTemp14 * fTemp17));
			float fTemp19 = ((fTemp16 * fRec21[1]) + (fTemp14 * fTemp18));
			float fTemp20 = ((fTemp16 * fRec22[1]) + (fTemp14 * fTemp19));
			float fTemp21 = ((fTemp16 * fRec23[1]) + (fTemp14 * fTemp20));
			fRec24[0] = ((fTemp16 * fRec24[1]) + (fTemp14 * fTemp21));
			fRec23[0] = ((fTemp15 * fTemp21) + (fTemp14 * fRec24[1]));
			fRec22[0] = ((fTemp15 * fTemp20) + (fTemp14 * fRec23[1]));
			fRec21[0] = ((fTemp15 * fTemp19) + (fTemp14 * fRec22[1]));
			fRec20[0] = ((fTemp15 * fTemp18) + (fTemp14 * fRec21[1]));
			fRec19[0] = ((fTemp15 * fTemp17) + (fTemp14 * fRec20[1]));
			float fTemp22 = ((iSlow24 * ((fRec4[1] * fTemp15) + (fRec19[1] * fTemp14))) + (iSlow19 * ((fRec7[0] * ((fRec4[1] * fTemp5) + (fRec10[1] * fTemp4))) + ((1 - fRec7[0]) * fRec4[1]))));
			float fTemp23 = (fRec25[1] + fSlow25);
			fRec25[0] = (fTemp23 - floorf(fTemp23));
			iRec26[0] = (iSlow1 & (iRec26[1] | (fRec28[1] >= 1)));
			iRec27[0] = (iSlow1 * (1 + iRec27[1]));
			int iTemp24 = (iSlow2 & (fRec28[1] > 0));
			fRec28[0] = (((fSlow32 * (((((iRec26[1] == 0) & iSlow1) & (fRec28[1] < 1)) & (iRec27[1] > fSlow29)) * (1 - (iRec27[1] < fSlow30)))) + (fRec28[1] * (1 - (fSlow27 * iTemp24)))) * ((iTemp24 == 0) | (fRec28[1] >= 1e-06f)));
			iRec29[0] = (iSlow1 & (iRec29[1] | (fRec30[1] >= 1)));
			int iTemp25 = (iSlow2 & (fRec30[1] > 0));
			fRec30[0] = (((float((((iRec29[1] == 0) & iSlow1) & (fRec30[1] < 1))) / ((fConst4 * fRec2[0]) + ((0.02f * fRec2[0]) == 0.0f))) + (fRec30[1] * ((1 - (fConst3 * (iRec29[1] & (fRec30[1] > 80)))) - (iTemp25 * (1 - (1.0f / powf(8e+04f,(1.0f / ((fConst2 * fRec2[0]) + ((0.2f * fRec2[0]) == 0.0f)))))))))) * ((iTemp25 == 0) | (fRec30[1] >= 1e-06f)));
			float fTemp26 = ((fSlow34 * fRec30[0]) + (fSlow33 * (fRec28[0] * faustpower<2>(ftbl0[int((65536.0f * fRec25[0]))]))));
			iRec31[0] = (12345 + (1103515245 * iRec31[1]));
			float fTemp27 = (0 - (fTemp26 - fTemp22));
			float fTemp28 = (fTemp27 * (faustpower<2>(fTemp27) - 1));
			float fTemp29 = ((fTemp28 > 1) + (fTemp28 * (fTemp28 <= 1)));
			fRec6[0] = (0 - (((0.998001f * fRec6[2]) + ((fTemp27 * ((fTemp29 * (fTemp29 >= -1)) - (fTemp29 < -1))) + (fSlow36 * fRec6[1]))) - (fTemp26 + (fSlow35 * ((iRec31[0] * fTemp26) * (0 - (fTemp26 - (1 + fTemp22))))))));
			fRec4[0] = ((0.0009994999999999865f * fRec6[0]) - (0.0009994999999999865f * fRec6[2]));
			fRec5[0] = fTemp27;
			fRec3[0] = ((fRec5[0] + (0.995f * fRec3[1])) - fRec5[1]);
			float fTemp30 = ((fRec2[0] * fRec3[0]) * fRec1[0]);
			fVec0[IOTA&4095] = fTemp30;
			output0[i] = (FAUSTFLOAT)(fSlow38 * fVec0[IOTA&4095]);
			output1[i] = (FAUSTFLOAT)(fSlow40 * fVec0[(IOTA-iSlow39)&4095]);
			// post processing
			IOTA = IOTA+1;
			fRec3[1] = fRec3[0];
			fRec5[1] = fRec5[0];
			fRec4[2] = fRec4[1]; fRec4[1] = fRec4[0];
			fRec6[2] = fRec6[1]; fRec6[1] = fRec6[0];
			iRec31[1] = iRec31[0];
			fRec30[1] = fRec30[0];
			iRec29[1] = iRec29[0];
			fRec28[1] = fRec28[0];
			iRec27[1] = iRec27[0];
			iRec26[1] = iRec26[0];
			fRec25[1] = fRec25[0];
			fRec19[1] = fRec19[0];
			fRec20[1] = fRec20[0];
			fRec21[1] = fRec21[0];
			fRec22[1] = fRec22[0];
			fRec23[1] = fRec23[0];
			fRec24[1] = fRec24[0];
			fRec17[1] = fRec17[0];
			fRec18[1] = fRec18[0];
			fRec10[1] = fRec10[0];
			fRec11[1] = fRec11[0];
			fRec12[1] = fRec12[0];
			fRec13[1] = fRec13[0];
			fRec14[1] = fRec14[0];
			fRec15[1] = fRec15[0];
			fRec9[1] = fRec9[0];
			iRec8[1] = iRec8[0];
			fRec7[1] = fRec7[0];
			fRec1[1] = fRec1[0];
			fRec2[1] = fRec2[0];
			iRec0[1] = iRec0[0];
		}
	}
};


float 	Blow_Bottle_dsp::ftbl0[65536];


#include "Faust_plugins_template2.cpp"

