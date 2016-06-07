//-----------------------------------------------------
// name: "BlowHole"
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
#define FAUSTCLASS Blow_Hole_dsp
#endif

class Blow_Hole_dsp : public dsp {
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
	int 	iConst0;
	float 	fConst1;
	int 	iConst2;
	int 	iConst3;
	int 	iConst4;
	float 	fConst5;
	static float 	ftbl0[65536];
	FAUSTFLOAT 	fslider0;
	float 	fRec16[2];
	FAUSTFLOAT 	fentry1;
	FAUSTFLOAT 	fentry2;
	float 	fConst6;
	float 	fRec15[2];
	FAUSTFLOAT 	fbutton0;
	int 	iRec17[2];
	FAUSTFLOAT 	fslider1;
	FAUSTFLOAT 	fslider2;
	float 	fRec18[2];
	FAUSTFLOAT 	fslider3;
	float 	fRec19[2];
	float 	fVec0[2];
	float 	fRec21[2];
	float 	fRec20[2];
	float 	fConst7;
	float 	fConst8;
	float 	fConst9;
	FAUSTFLOAT 	fslider4;
	float 	fRec23[2];
	int 	iRec24[2];
	int 	iRec25[2];
	FAUSTFLOAT 	fslider5;
	FAUSTFLOAT 	fslider6;
	float 	fConst10;
	float 	fConst11;
	float 	fRec26[2];
	FAUSTFLOAT 	fslider7;
	int 	iRec27[2];
	FAUSTFLOAT 	fslider8;
	int 	iRec28[2];
	FAUSTFLOAT 	fslider9;
	FAUSTFLOAT 	fslider10;
	float 	fRec29[2];
	float 	fConst12;
	int 	iConst13;
	int 	iConst14;
	int 	iConst15;
	float 	fConst16;
	int 	iConst17;
	float 	fConst18;
	FAUSTFLOAT 	fslider11;
	float 	fRec31[2];
	float 	fRec30[2];
	FAUSTFLOAT 	fslider12;
	float 	fConst19;
	float 	fVec1[2];
	float 	fRec22[2];
	int 	IOTA;
	float 	fRec11[128];
	int 	iConst20;
	float 	fConst21;
	float 	fVec2[2];
	FAUSTFLOAT 	fslider13;
	float 	fConst22;
	float 	fConst23;
	float 	fRec10[2];
	float 	fRec9[2];
	float 	fRec6[2];
	float 	fRec5[2];
	float 	fRec3[128];
	float 	fRec1[8192];
	float 	fVec3[4096];
	FAUSTFLOAT 	fslider14;
	FAUSTFLOAT 	fslider15;
	float 	fConst24;
  public:
	static void metadata(Meta* m) 	{ 
		m->declare("name", "BlowHole");
		m->declare("description", "Nonlinear WaveGuide Clarinet with one register hole and one tonehole");
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
		m->declare("effect.lib/author", "Julius O. Smith (jos at ccrma.stanford.edu)");
		m->declare("effect.lib/name", "Faust Audio Effect Library");
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
		fConst1 = (0.00018140589569160998f * iConst0);
		iConst2 = int(fConst1);
		iConst3 = int((1 + int((iConst2 & 4095))));
		iConst4 = (1 + iConst2);
		fConst5 = (iConst4 - fConst1);
		fslider0 = 2.2e+02f;
		for (int i=0; i<2; i++) fRec16[i] = 0;
		fentry1 = 0.0f;
		fentry2 = 4.4e+02f;
		fConst6 = (1.0f / float(iConst0));
		for (int i=0; i<2; i++) fRec15[i] = 0;
		fbutton0 = 0.0;
		for (int i=0; i<2; i++) iRec17[i] = 0;
		fslider1 = 0.1f;
		fslider2 = 0.1f;
		for (int i=0; i<2; i++) fRec18[i] = 0;
		fslider3 = 0.0f;
		for (int i=0; i<2; i++) fRec19[i] = 0;
		for (int i=0; i<2; i++) fVec0[i] = 0;
		for (int i=0; i<2; i++) fRec21[i] = 0;
		for (int i=0; i<2; i++) fRec20[i] = 0;
		fConst7 = (0.20999999999999996f * iConst0);
		fConst8 = (347.23f + fConst7);
		fConst9 = ((347.23f - fConst7) / fConst8);
		fslider4 = 5.0f;
		for (int i=0; i<2; i++) fRec23[i] = 0;
		for (int i=0; i<2; i++) iRec24[i] = 0;
		for (int i=0; i<2; i++) iRec25[i] = 0;
		fslider5 = 0.01f;
		fslider6 = 0.5f;
		fConst10 = (0.2f * iConst0);
		fConst11 = (1.8f * iConst0);
		for (int i=0; i<2; i++) fRec26[i] = 0;
		fslider7 = 0.1f;
		for (int i=0; i<2; i++) iRec27[i] = 0;
		fslider8 = 0.0f;
		for (int i=0; i<2; i++) iRec28[i] = 0;
		fslider9 = 0.35f;
		fslider10 = 0.01f;
		for (int i=0; i<2; i++) fRec29[i] = 0;
		fConst12 = (0.00022675736961451248f * iConst0);
		iConst13 = int(fConst12);
		iConst14 = (1 + iConst13);
		iConst15 = int((1 + int((int(iConst14) & 4095))));
		fConst16 = (fConst12 - iConst13);
		iConst17 = int((1 + int((iConst13 & 4095))));
		fConst18 = (iConst14 - fConst12);
		fslider11 = 0.35f;
		for (int i=0; i<2; i++) fRec31[i] = 0;
		for (int i=0; i<2; i++) fRec30[i] = 0;
		fslider12 = 0.0f;
		fConst19 = (0 - (347.23f / fConst8));
		for (int i=0; i<2; i++) fVec1[i] = 0;
		for (int i=0; i<2; i++) fRec22[i] = 0;
		IOTA = 0;
		for (int i=0; i<128; i++) fRec11[i] = 0;
		iConst20 = int((1 + int((int(iConst4) & 4095))));
		fConst21 = (fConst1 - iConst2);
		for (int i=0; i<2; i++) fVec2[i] = 0;
		fslider13 = 0.12f;
		fConst22 = (0.0084f * iConst0);
		fConst23 = (((fConst22 - 347.23f) / (347.23f + fConst22)) - 0.9995f);
		for (int i=0; i<2; i++) fRec10[i] = 0;
		for (int i=0; i<2; i++) fRec9[i] = 0;
		for (int i=0; i<2; i++) fRec6[i] = 0;
		for (int i=0; i<2; i++) fRec5[i] = 0;
		for (int i=0; i<128; i++) fRec3[i] = 0;
		for (int i=0; i<8192; i++) fRec1[i] = 0;
		for (int i=0; i<4096; i++) fVec3[i] = 0;
		fslider14 = 0.6f;
		fslider15 = 0.5f;
		fConst24 = (0.5f * iConst0);
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
		faust_interface->declare(&fslider10, "5", "");
		faust_interface->declare(&fslider10, "tooltip", "Envelope attack duration");
		faust_interface->declare(&fslider10, "unit", "s");
		faust_interface->addHorizontalSlider("Envelope_Attack", &fslider10, 0.01f, 0.0f, 2.0f, 0.01f);
		faust_interface->declare(&fslider1, "5", "");
		faust_interface->declare(&fslider1, "tooltip", "Envelope release duration");
		faust_interface->declare(&fslider1, "unit", "s");
		faust_interface->addHorizontalSlider("Envelope_Release", &fslider1, 0.1f, 0.0f, 2.0f, 0.01f);
		faust_interface->closeBox();
		faust_interface->openVerticalBox("Vibrato_Parameters");
		faust_interface->declare(&fslider6, "4", "");
		faust_interface->declare(&fslider6, "tooltip", "Vibrato attack duration");
		faust_interface->declare(&fslider6, "unit", "s");
		faust_interface->addHorizontalSlider("Vibrato_Attack", &fslider6, 0.5f, 0.0f, 2.0f, 0.01f);
		faust_interface->declare(&fslider4, "4", "");
		faust_interface->declare(&fslider4, "unit", "Hz");
		faust_interface->addHorizontalSlider("Vibrato_Freq", &fslider4, 5.0f, 1.0f, 15.0f, 0.1f);
		faust_interface->declare(&fslider7, "4", "");
		faust_interface->declare(&fslider7, "tooltip", "A value between 0 and 1");
		faust_interface->addHorizontalSlider("Vibrato_Gain", &fslider7, 0.1f, 0.0f, 1.0f, 0.01f);
		faust_interface->declare(&fslider5, "4", "");
		faust_interface->declare(&fslider5, "tooltip", "Vibrato release duration");
		faust_interface->declare(&fslider5, "unit", "s");
		faust_interface->addHorizontalSlider("Vibrato_Release", &fslider5, 0.01f, 0.0f, 2.0f, 0.01f);
		faust_interface->closeBox();
		faust_interface->closeBox();
		faust_interface->openHorizontalBox("Physical_and_Nonlinearity");
		faust_interface->openVerticalBox("Nonlinear_Filter_Parameters");
		faust_interface->declare(&fslider0, "3", "");
		faust_interface->declare(&fslider0, "tooltip", "Frequency of the sine wave for the modulation of theta (works if Modulation Type=3)");
		faust_interface->declare(&fslider0, "unit", "Hz");
		faust_interface->addHorizontalSlider("Modulation_Frequency", &fslider0, 2.2e+02f, 2e+01f, 1e+03f, 0.1f);
		faust_interface->declare(&fentry1, "3", "");
		faust_interface->declare(&fentry1, "tooltip", "0=theta is modulated by the incoming signal; 1=theta is modulated by the averaged incoming signal; 2=theta is modulated by the squared incoming signal; 3=theta is modulated by a sine wave of frequency freqMod; 4=theta is modulated by a sine wave of frequency freq;");
		faust_interface->addNumEntry("Modulation_Type", &fentry1, 0.0f, 0.0f, 4.0f, 1.0f);
		faust_interface->declare(&fslider3, "3", "");
		faust_interface->declare(&fslider3, "tooltip", "Nonlinearity factor (value between 0 and 1)");
		faust_interface->addHorizontalSlider("Nonlinearity", &fslider3, 0.0f, 0.0f, 1.0f, 0.01f);
		faust_interface->declare(&fslider2, "3", "");
		faust_interface->declare(&fslider2, "Attack duration of the nonlinearity", "");
		faust_interface->declare(&fslider2, "unit", "s");
		faust_interface->addHorizontalSlider("Nonlinearity_Attack", &fslider2, 0.1f, 0.0f, 2.0f, 0.01f);
		faust_interface->closeBox();
		faust_interface->openVerticalBox("Physical_Parameters");
		faust_interface->declare(&fslider8, "2", "");
		faust_interface->declare(&fslider8, "tooltip", "Breath noise gain (value between 0 and 1)");
		faust_interface->addHorizontalSlider("Noise_Gain", &fslider8, 0.0f, 0.0f, 1.0f, 0.01f);
		faust_interface->declare(&fslider9, "2", "");
		faust_interface->declare(&fslider9, "tooltip", "Breath pressure (value bewteen 0 and 1)");
		faust_interface->addHorizontalSlider("Pressure", &fslider9, 0.35f, 0.0f, 1.0f, 0.01f);
		faust_interface->declare(&fslider11, "2", "");
		faust_interface->declare(&fslider11, "tooltip", "Reed stiffness (value between 0 and 1)");
		faust_interface->addHorizontalSlider("Reed_Stiffness", &fslider11, 0.35f, 0.0f, 1.0f, 0.01f);
		faust_interface->declare(&fslider13, "2", "");
		faust_interface->declare(&fslider13, "tooltip", "A value between 0 and 1");
		faust_interface->addHorizontalSlider("Tone_Hole_Openness", &fslider13, 0.12f, 0.0f, 1.0f, 0.01f);
		faust_interface->declare(&fslider12, "2", "");
		faust_interface->declare(&fslider12, "tooltip", "A value between 0 and 1");
		faust_interface->addHorizontalSlider("Vent_Openness", &fslider12, 0.0f, 0.0f, 1.0f, 0.01f);
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
		float 	fSlow1 = (0.0010000000000000009f * float(fslider0));
		float 	fSlow2 = float(fentry1);
		int 	iSlow3 = (fSlow2 != 4);
		float 	fSlow4 = float(fentry2);
		float 	fSlow5 = (fSlow4 * (fSlow2 == 4));
		float 	fSlow6 = float(fbutton0);
		int 	iSlow7 = (fSlow6 > 0);
		int 	iSlow8 = (fSlow6 <= 0);
		float 	fSlow9 = float(fslider1);
		float 	fSlow10 = (1 - (1.0f / powf(1e+05f,(1.0f / ((fSlow9 == 0.0f) + (iConst0 * fSlow9))))));
		float 	fSlow11 = float(fslider2);
		float 	fSlow12 = (1.0f / ((fSlow11 == 0.0f) + (iConst0 * fSlow11)));
		float 	fSlow13 = float(fslider3);
		float 	fSlow14 = (0.0010000000000000009f * fSlow13);
		float 	fSlow15 = (2 * (fSlow13 * (fSlow2 < 2)));
		float 	fSlow16 = (iConst0 * ((0.5f / fSlow4) - 0.00040816326530612246f));
		int 	iSlow17 = int((fSlow16 - (3.5f + fSlow15)));
		int 	iSlow18 = int((1 + int((int((1 + iSlow17)) & 4095))));
		float 	fSlow19 = (fSlow15 + iSlow17);
		float 	fSlow20 = (fSlow16 - (3.5f + fSlow19));
		int 	iSlow21 = int((1 + int((iSlow17 & 4095))));
		float 	fSlow22 = ((fSlow19 + 4.5f) - fSlow16);
		int 	iSlow23 = (fSlow2 >= 3);
		float 	fSlow24 = (fConst6 * float(fslider4));
		float 	fSlow25 = float(fslider5);
		float 	fSlow26 = (1 - (1.0f / powf(1e+05f,(1.0f / ((fSlow25 == 0.0f) + (iConst0 * fSlow25))))));
		float 	fSlow27 = float(fslider6);
		float 	fSlow28 = (fConst10 * fSlow27);
		float 	fSlow29 = (fSlow28 + ((0.2f * fSlow27) == 0.0f));
		float 	fSlow30 = (1.0f / ((fConst11 * fSlow27) + ((1.8f * fSlow27) == 0.0f)));
		float 	fSlow31 = float(fslider7);
		float 	fSlow32 = (4.656612875245797e-10f * float(fslider8));
		float 	fSlow33 = float(fslider9);
		float 	fSlow34 = (fSlow9 * fSlow33);
		float 	fSlow35 = (1 - (1.0f / powf(1e+05f,(1.0f / ((iConst0 * fSlow34) + (fSlow34 == 0.0f))))));
		float 	fSlow36 = (float(fslider10) * fSlow33);
		float 	fSlow37 = (1.0f / ((iConst0 * fSlow36) + (fSlow36 == 0.0f)));
		float 	fSlow38 = (0.55f + (0.3f * fSlow33));
		float 	fSlow39 = ((0.26f * float(fslider11)) - 0.44f);
		float 	fSlow40 = (3.141592653589793f * (fSlow2 == 2));
		float 	fSlow41 = (1.5707963267948966f * (fSlow2 == 1));
		float 	fSlow42 = (3.141592653589793f * (fSlow2 == 0));
		int 	iSlow43 = (fSlow2 < 3);
		float 	fSlow44 = (fConst19 * float(fslider12));
		float 	fSlow45 = (0.9995f + (fConst23 * float(fslider13)));
		float 	fSlow46 = (0 - fSlow45);
		float 	fSlow47 = float(fslider14);
		float 	fSlow48 = (1.5f * (1.0f - fSlow47));
		int 	iSlow49 = int((int((fConst24 * (float(fslider15) / fSlow4))) & 4095));
		float 	fSlow50 = (1.5f * fSlow47);
		FAUSTFLOAT* output0 = output[0];
		FAUSTFLOAT* output1 = output[1];
		for (int i=0; i<count; i++) {
			fRec0[0] = ((0.999f * fRec0[1]) + fSlow0);
			float fTemp0 = (fConst5 * fRec3[(IOTA-iConst3)&127]);
			fRec16[0] = (fSlow1 + (0.999f * fRec16[1]));
			float fTemp1 = (fRec15[1] + (fConst6 * (fSlow5 + (iSlow3 * fRec16[0]))));
			fRec15[0] = (fTemp1 - floorf(fTemp1));
			iRec17[0] = (iSlow7 & (iRec17[1] | (fRec18[1] >= 1)));
			int iTemp2 = (iSlow8 & (fRec18[1] > 0));
			fRec18[0] = (((fSlow12 * (((iRec17[1] == 0) & iSlow7) & (fRec18[1] < 1))) + (fRec18[1] * (1 - (fSlow10 * iTemp2)))) * ((iTemp2 == 0) | (fRec18[1] >= 1e-06f)));
			fRec19[0] = (fSlow14 + (0.999f * fRec19[1]));
			float fTemp3 = (fRec19[0] * fRec18[0]);
			float fTemp4 = (3.141592653589793f * (fTemp3 * ftbl0[int((65536.0f * fRec15[0]))]));
			float fTemp5 = cosf(fTemp4);
			float fTemp6 = ((fSlow22 * fRec1[(IOTA-iSlow21)&8191]) + (fSlow20 * fRec1[(IOTA-iSlow18)&8191]));
			fVec0[0] = fTemp6;
			float fTemp7 = sinf(fTemp4);
			float fTemp8 = (0 - fTemp7);
			float fTemp9 = ((fRec20[1] * fTemp8) + (fVec0[0] * fTemp5));
			fRec21[0] = ((fTemp8 * fRec21[1]) + (fTemp5 * fTemp9));
			fRec20[0] = ((fTemp7 * fTemp9) + (fTemp5 * fRec21[1]));
			float fTemp10 = (iSlow23 * ((fVec0[0] * fTemp7) + (fRec20[1] * fTemp5)));
			float fTemp11 = (fRec23[1] + fSlow24);
			fRec23[0] = (fTemp11 - floorf(fTemp11));
			iRec24[0] = (iSlow7 & (iRec24[1] | (fRec26[1] >= 1)));
			iRec25[0] = (iSlow7 * (1 + iRec25[1]));
			int iTemp12 = (iSlow8 & (fRec26[1] > 0));
			fRec26[0] = (((fSlow30 * (((((iRec24[1] == 0) & iSlow7) & (fRec26[1] < 1)) & (iRec25[1] > fSlow28)) * (1 - (iRec25[1] < fSlow29)))) + (fRec26[1] * (1 - (fSlow26 * iTemp12)))) * ((iTemp12 == 0) | (fRec26[1] >= 1e-06f)));
			iRec27[0] = (12345 + (1103515245 * iRec27[1]));
			iRec28[0] = (iSlow7 & (iRec28[1] | (fRec29[1] >= 1)));
			int iTemp13 = (iSlow8 & (fRec29[1] > 0));
			fRec29[0] = (((fSlow37 * (((iRec28[1] == 0) & iSlow7) & (fRec29[1] < 1))) + (fRec29[1] * (1 - (fSlow35 * iTemp13)))) * ((iTemp13 == 0) | (fRec29[1] >= 1e-06f)));
			float fTemp14 = (fSlow38 * ((fRec29[0] * (1 + (fSlow32 * iRec27[0]))) * (1 + (fSlow31 * (fRec26[0] * ftbl0[int((65536.0f * fRec23[0]))])))));
			float fTemp15 = (((fConst18 * fRec11[(IOTA-iConst17)&127]) + (fConst16 * fRec11[(IOTA-iConst15)&127])) - fTemp14);
			float fTemp16 = (0.7f + (fSlow39 * fTemp15));
			float fTemp17 = ((fTemp16 > 1) + (fTemp16 * (fTemp16 <= 1)));
			float fTemp18 = (fTemp14 + (fTemp15 * ((fTemp17 * (fTemp17 >= -1)) - (fTemp17 < -1))));
			float fTemp19 = (fTemp3 * (((fSlow42 * fVec0[0]) + (fSlow41 * (fVec0[0] + fVec0[1]))) + (fSlow40 * faustpower<2>(fVec0[0]))));
			float fTemp20 = cosf(fTemp19);
			float fTemp21 = sinf(fTemp19);
			float fTemp22 = (0 - fTemp21);
			float fTemp23 = ((fRec30[1] * fTemp22) + (fVec0[0] * fTemp20));
			fRec31[0] = ((fTemp22 * fRec31[1]) + (fTemp20 * fTemp23));
			fRec30[0] = ((fTemp21 * fTemp23) + (fTemp20 * fRec31[1]));
			float fTemp24 = (iSlow43 * ((fRec19[0] * ((fVec0[0] * fTemp21) + (fRec30[1] * fTemp20))) + ((1 - fRec19[0]) * fVec0[0])));
			float fTemp25 = (fSlow44 * (fTemp24 + (fTemp18 + fTemp10)));
			fVec1[0] = fTemp25;
			fRec22[0] = ((fVec1[0] + fVec1[1]) - (fConst9 * fRec22[1]));
			fRec11[IOTA&127] = (fTemp24 + (fRec22[0] + fTemp10));
			float 	fRec12 = fRec22[0];
			float 	fRec13 = fTemp18;
			float fTemp26 = (fRec12 + fRec13);
			float fTemp27 = (fConst21 * fRec3[(IOTA-iConst20)&127]);
			float fTemp28 = (fTemp27 + (fTemp26 + fTemp0));
			float fTemp29 = (0.07407407407407407f * (fTemp28 - (2 * fRec6[1])));
			float fTemp30 = (fTemp28 - (fRec9[1] + fTemp29));
			fVec2[0] = fTemp30;
			fRec10[0] = (0 - ((fVec2[1] + (fSlow46 * fRec10[1])) - (fSlow45 * fVec2[0])));
			fRec9[0] = fRec10[0];
			fRec6[0] = fRec9[0];
			float fTemp31 = (0 - fTemp29);
			float 	fRec7 = fTemp31;
			float 	fRec8 = fTemp31;
			fRec5[0] = (0.5f * (fRec5[1] + (fTemp26 + fRec7)));
			fRec3[IOTA&127] = (0 - (0.95f * fRec5[0]));
			float 	fRec4 = (fTemp27 + (fRec8 + fTemp0));
			fRec1[IOTA&8191] = fRec4;
			float 	fRec2 = fRec11[(IOTA-0)&127];
			float fTemp32 = (fRec2 * fRec0[0]);
			fVec3[IOTA&4095] = fTemp32;
			output0[i] = (FAUSTFLOAT)(fSlow48 * fVec3[IOTA&4095]);
			output1[i] = (FAUSTFLOAT)(fSlow50 * fVec3[(IOTA-iSlow49)&4095]);
			// post processing
			fRec5[1] = fRec5[0];
			fRec6[1] = fRec6[0];
			fRec9[1] = fRec9[0];
			fRec10[1] = fRec10[0];
			fVec2[1] = fVec2[0];
			IOTA = IOTA+1;
			fRec22[1] = fRec22[0];
			fVec1[1] = fVec1[0];
			fRec30[1] = fRec30[0];
			fRec31[1] = fRec31[0];
			fRec29[1] = fRec29[0];
			iRec28[1] = iRec28[0];
			iRec27[1] = iRec27[0];
			fRec26[1] = fRec26[0];
			iRec25[1] = iRec25[0];
			iRec24[1] = iRec24[0];
			fRec23[1] = fRec23[0];
			fRec20[1] = fRec20[0];
			fRec21[1] = fRec21[0];
			fVec0[1] = fVec0[0];
			fRec19[1] = fRec19[0];
			fRec18[1] = fRec18[0];
			iRec17[1] = iRec17[0];
			fRec15[1] = fRec15[0];
			fRec16[1] = fRec16[0];
			fRec0[1] = fRec0[0];
		}
	}
};


float 	Blow_Hole_dsp::ftbl0[65536];


#include "Faust_plugins_template2.cpp"

