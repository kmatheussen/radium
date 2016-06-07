//-----------------------------------------------------
// name: "Glass Harmonica"
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
#define FAUSTCLASS Glass_Harmonica_dsp
#endif

class Glass_Harmonica_dsp : public dsp {
  private:
	class SIG0 {
	  private:
		int 	fSamplingFreq;
		int 	iRec27[2];
	  public:
		int getNumInputs() 	{ return 0; }
		int getNumOutputs() 	{ return 1; }
		void init(int samplingFreq) {
			fSamplingFreq = samplingFreq;
			for (int i=0; i<2; i++) iRec27[i] = 0;
		}
		void fill (int count, float output[]) {
			for (int i=0; i<count; i++) {
				iRec27[0] = (1 + iRec27[1]);
				output[i] = sinf((9.587379924285257e-05f * float((iRec27[0] - 1))));
				// post processing
				iRec27[1] = iRec27[0];
			}
		}
	};


	FAUSTFLOAT 	fentry0;
	FAUSTFLOAT 	fentry1;
	FAUSTFLOAT 	fbutton0;
	int 	iRec8[2];
	int 	iConst0;
	float 	fConst1;
	float 	fConst2;
	float 	fConst3;
	float 	fRec9[2];
	FAUSTFLOAT 	fslider0;
	FAUSTFLOAT 	fslider1;
	FAUSTFLOAT 	fslider2;
	int 	IOTA;
	float 	fVec0[4096];
	FAUSTFLOAT 	fentry2;
	float 	fConst4;
	float 	fConst5;
	float 	fConst6;
	float 	fConst7;
	float 	fRec7[3];
	float 	fConst8;
	float 	fConst9;
	float 	fConst10;
	float 	fRec6[2];
	float 	fRec0[2];
	float 	fVec1[4096];
	float 	fConst11;
	float 	fConst12;
	float 	fRec11[3];
	float 	fRec10[2];
	float 	fRec1[2];
	float 	fVec2[4096];
	float 	fConst13;
	float 	fConst14;
	float 	fRec13[3];
	float 	fRec12[2];
	float 	fRec2[2];
	float 	fVec3[2048];
	float 	fConst15;
	float 	fConst16;
	float 	fRec15[3];
	float 	fRec14[2];
	float 	fRec3[2];
	float 	fVec4[1024];
	float 	fConst17;
	float 	fConst18;
	float 	fRec17[3];
	float 	fRec16[2];
	float 	fRec4[2];
	float 	fRec19[3];
	float 	fRec5[2];
	float 	fVec5[2];
	FAUSTFLOAT 	fslider3;
	float 	fRec20[2];
	FAUSTFLOAT 	fentry3;
	float 	fRec26[2];
	float 	fRec25[2];
	float 	fRec24[2];
	float 	fRec23[2];
	float 	fRec22[2];
	float 	fRec21[2];
	static float 	ftbl0[65536];
	FAUSTFLOAT 	fslider4;
	float 	fRec29[2];
	float 	fConst19;
	float 	fRec28[2];
	float 	fRec35[2];
	float 	fRec34[2];
	float 	fRec33[2];
	float 	fRec32[2];
	float 	fRec31[2];
	float 	fRec30[2];
	int 	iRec36[2];
	FAUSTFLOAT 	fslider5;
	FAUSTFLOAT 	fslider6;
	float 	fRec37[2];
	float 	fVec6[4096];
	FAUSTFLOAT 	fslider7;
	FAUSTFLOAT 	fslider8;
	float 	fConst20;
  public:
	static void metadata(Meta* m) 	{ 
		m->declare("name", "Glass Harmonica");
		m->declare("description", "Nonlinear Banded Waveguide Modeled Glass Harmonica");
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
		fentry0 = 0.8f;
		fentry1 = 0.0f;
		fbutton0 = 0.0;
		for (int i=0; i<2; i++) iRec8[i] = 0;
		iConst0 = min(192000, max(1, fSamplingFreq));
		fConst1 = (1 - (1.0f / powf(9e+04f,(1e+02f / float(iConst0)))));
		fConst2 = (1 - powf(9e+01f,(2e+02f / float(iConst0))));
		fConst3 = (5e+01f / float(iConst0));
		for (int i=0; i<2; i++) fRec9[i] = 0;
		fslider0 = 1.0f;
		fslider1 = 0.0f;
		fslider2 = 0.2f;
		IOTA = 0;
		for (int i=0; i<4096; i++) fVec0[i] = 0;
		fentry2 = 4.4e+02f;
		fConst4 = (1 - (100.53096491487338f / float(iConst0)));
		fConst5 = faustpower<2>(fConst4);
		fConst6 = (6.283185307179586f / float(iConst0));
		fConst7 = (0 - (2 * fConst4));
		for (int i=0; i<3; i++) fRec7[i] = 0;
		fConst8 = (0.5f * fConst5);
		fConst9 = (fConst8 - 0.5f);
		fConst10 = (0.5f - fConst8);
		for (int i=0; i<2; i++) fRec6[i] = 0;
		for (int i=0; i<2; i++) fRec0[i] = 0;
		for (int i=0; i<4096; i++) fVec1[i] = 0;
		fConst11 = (0.4310344827586207f * iConst0);
		fConst12 = (14.57698991265664f / float(iConst0));
		for (int i=0; i<3; i++) fRec11[i] = 0;
		for (int i=0; i<2; i++) fRec10[i] = 0;
		for (int i=0; i<2; i++) fRec1[i] = 0;
		for (int i=0; i<4096; i++) fVec2[i] = 0;
		fConst13 = (0.23529411764705882f * iConst0);
		fConst14 = (26.703537555513243f / float(iConst0));
		for (int i=0; i<3; i++) fRec13[i] = 0;
		for (int i=0; i<2; i++) fRec12[i] = 0;
		for (int i=0; i<2; i++) fRec2[i] = 0;
		for (int i=0; i<2048; i++) fVec3[i] = 0;
		fConst15 = (0.15082956259426847f * iConst0);
		fConst16 = (41.65751858660066f / float(iConst0));
		for (int i=0; i<3; i++) fRec15[i] = 0;
		for (int i=0; i<2; i++) fRec14[i] = 0;
		for (int i=0; i<2; i++) fRec3[i] = 0;
		for (int i=0; i<1024; i++) fVec4[i] = 0;
		fConst17 = (0.10660980810234541f * iConst0);
		fConst18 = (58.93627818134453f / float(iConst0));
		for (int i=0; i<3; i++) fRec17[i] = 0;
		for (int i=0; i<2; i++) fRec16[i] = 0;
		for (int i=0; i<2; i++) fRec4[i] = 0;
		for (int i=0; i<3; i++) fRec19[i] = 0;
		for (int i=0; i<2; i++) fRec5[i] = 0;
		for (int i=0; i<2; i++) fVec5[i] = 0;
		fslider3 = 0.0f;
		for (int i=0; i<2; i++) fRec20[i] = 0;
		fentry3 = 0.0f;
		for (int i=0; i<2; i++) fRec26[i] = 0;
		for (int i=0; i<2; i++) fRec25[i] = 0;
		for (int i=0; i<2; i++) fRec24[i] = 0;
		for (int i=0; i<2; i++) fRec23[i] = 0;
		for (int i=0; i<2; i++) fRec22[i] = 0;
		for (int i=0; i<2; i++) fRec21[i] = 0;
		fslider4 = 2.2e+02f;
		for (int i=0; i<2; i++) fRec29[i] = 0;
		fConst19 = (1.0f / float(iConst0));
		for (int i=0; i<2; i++) fRec28[i] = 0;
		for (int i=0; i<2; i++) fRec35[i] = 0;
		for (int i=0; i<2; i++) fRec34[i] = 0;
		for (int i=0; i<2; i++) fRec33[i] = 0;
		for (int i=0; i<2; i++) fRec32[i] = 0;
		for (int i=0; i<2; i++) fRec31[i] = 0;
		for (int i=0; i<2; i++) fRec30[i] = 0;
		for (int i=0; i<2; i++) iRec36[i] = 0;
		fslider5 = 0.1f;
		fslider6 = 0.02f;
		for (int i=0; i<2; i++) fRec37[i] = 0;
		for (int i=0; i<4096; i++) fVec6[i] = 0;
		fslider7 = 0.6f;
		fslider8 = 0.5f;
		fConst20 = (0.5f * iConst0);
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
		float 	fSlow0 = float(fentry0);
		float 	fSlow1 = float(fentry1);
		float 	fSlow2 = float(fbutton0);
		float 	fSlow3 = (0.2f * ((fSlow2 * fSlow1) * fSlow0));
		int 	iSlow4 = (fSlow2 > 0);
		int 	iSlow5 = (fSlow2 <= 0);
		float 	fSlow6 = (0.03f + (0.1f * fSlow0));
		float 	fSlow7 = (0.8999999999999999f + (0.1f * float(fslider0)));
		float 	fSlow8 = float(fslider1);
		float 	fSlow9 = (10 - (9 * float(fslider2)));
		float 	fSlow10 = (0.16666666666666666f * (0 - (fSlow1 - 1)));
		float 	fSlow11 = float(fentry2);
		int 	iSlow12 = int((int((float(iConst0) / fSlow11)) & 4095));
		float 	fSlow13 = (fConst7 * cosf((fConst6 * fSlow11)));
		int 	iSlow14 = int((int((fConst11 / fSlow11)) & 4095));
		float 	fSlow15 = (fConst7 * cosf((fConst12 * fSlow11)));
		int 	iSlow16 = int((int((fConst13 / fSlow11)) & 4095));
		float 	fSlow17 = (fConst7 * cosf((fConst14 * fSlow11)));
		int 	iSlow18 = int((int((fConst15 / fSlow11)) & 4095));
		float 	fSlow19 = (fConst7 * cosf((fConst16 * fSlow11)));
		int 	iSlow20 = int((int((fConst17 / fSlow11)) & 4095));
		float 	fSlow21 = (fConst7 * cosf((fConst18 * fSlow11)));
		float 	fSlow22 = (0.0010000000000000009f * float(fslider3));
		float 	fSlow23 = float(fentry3);
		float 	fSlow24 = (50.26548245743669f * (fSlow23 == 2));
		float 	fSlow25 = (6.283185307179586f * (fSlow23 == 1));
		float 	fSlow26 = (12.566370614359172f * (fSlow23 == 0));
		int 	iSlow27 = (fSlow23 < 3);
		float 	fSlow28 = (0.0010000000000000009f * float(fslider4));
		int 	iSlow29 = (fSlow23 != 4);
		float 	fSlow30 = (fSlow11 * (fSlow23 == 4));
		int 	iSlow31 = (fSlow23 >= 3);
		float 	fSlow32 = float(fslider5);
		float 	fSlow33 = (1 - (1.0f / powf(1e+05f,(1.0f / ((fSlow32 == 0.0f) + (iConst0 * fSlow32))))));
		float 	fSlow34 = float(fslider6);
		float 	fSlow35 = (1.0f / ((fSlow34 == 0.0f) + (iConst0 * fSlow34)));
		float 	fSlow36 = float(fslider7);
		float 	fSlow37 = (0.5f * (1.0f - fSlow36));
		int 	iSlow38 = int((int((fConst20 * (float(fslider8) / fSlow11))) & 4095));
		float 	fSlow39 = (0.5f * fSlow36);
		FAUSTFLOAT* output0 = output[0];
		FAUSTFLOAT* output1 = output[1];
		for (int i=0; i<count; i++) {
			iRec8[0] = (iSlow4 & (iRec8[1] | (fRec9[1] >= 1)));
			int iTemp0 = (iSlow5 & (fRec9[1] > 0));
			fRec9[0] = (((fConst3 * (((iRec8[1] == 0) & iSlow4) & (fRec9[1] < 1))) + (fRec9[1] * ((1 - (fConst2 * (iRec8[1] & (fRec9[1] > 90)))) - (fConst1 * iTemp0)))) * ((iTemp0 == 0) | (fRec9[1] >= 1e-06f)));
			float fTemp1 = (0 - ((fSlow8 + (fSlow7 * (((fRec0[1] + fRec2[1]) + fRec4[1]) + ((fRec1[1] + fRec3[1]) + fRec5[1])))) - (fSlow6 * fRec9[0])));
			float fTemp2 = faustpower<4>((0.75f + fabsf((fSlow9 * fTemp1))));
			float fTemp3 = (1.0f / fTemp2);
			float fTemp4 = (fSlow10 * (fTemp1 * ((fTemp3 > 1) + (float((fTemp3 <= 1)) / fTemp2))));
			fVec0[IOTA&4095] = ((fRec6[1] + fTemp4) + fSlow3);
			fRec7[0] = (0 - (((fSlow13 * fRec7[1]) + (fConst5 * fRec7[2])) - (0.999f * fVec0[(IOTA-iSlow12)&4095])));
			fRec6[0] = ((fConst10 * fRec7[0]) + (fConst9 * fRec7[2]));
			fRec0[0] = fRec6[0];
			fVec1[IOTA&4095] = (fSlow3 + (fTemp4 + fRec10[1]));
			fRec11[0] = (0 - (((fSlow15 * fRec11[1]) + (fConst5 * fRec11[2])) - (0.998001f * fVec1[(IOTA-iSlow14)&4095])));
			fRec10[0] = ((fConst10 * fRec11[0]) + (fConst9 * fRec11[2]));
			fRec1[0] = fRec10[0];
			fVec2[IOTA&4095] = (fSlow3 + (fTemp4 + fRec12[1]));
			fRec13[0] = (0 - (((fSlow17 * fRec13[1]) + (fConst5 * fRec13[2])) - (0.997002999f * fVec2[(IOTA-iSlow16)&4095])));
			fRec12[0] = ((fConst10 * fRec13[0]) + (fConst9 * fRec13[2]));
			fRec2[0] = fRec12[0];
			fVec3[IOTA&2047] = (fSlow3 + (fTemp4 + fRec14[1]));
			fRec15[0] = (0 - (((fSlow19 * fRec15[1]) + (fConst5 * fRec15[2])) - (0.996005996001f * fVec3[(IOTA-iSlow18)&2047])));
			fRec14[0] = ((fConst10 * fRec15[0]) + (fConst9 * fRec15[2]));
			fRec3[0] = fRec14[0];
			fVec4[IOTA&1023] = (fSlow3 + (fTemp4 + fRec16[1]));
			fRec17[0] = (0 - (((fSlow21 * fRec17[1]) + (fConst5 * fRec17[2])) - (0.995009990004999f * fVec4[(IOTA-iSlow20)&1023])));
			fRec16[0] = ((fConst10 * fRec17[0]) + (fConst9 * fRec17[2]));
			fRec4[0] = fRec16[0];
			fRec19[0] = (0 - ((fConst7 * fRec19[1]) + (fConst5 * fRec19[2])));
			float 	fRec18 = ((fConst10 * fRec19[0]) + (fConst9 * fRec19[2]));
			fRec5[0] = fRec18;
			float fTemp5 = (fRec5[0] + (fRec3[0] + (((fRec0[0] + fRec2[0]) + fRec4[0]) + fRec1[0])));
			fVec5[0] = fTemp5;
			fRec20[0] = (fSlow22 + (0.999f * fRec20[1]));
			float fTemp6 = (fRec20[0] * (((fSlow26 * fVec5[0]) + (fSlow25 * (fVec5[0] + fVec5[1]))) + (fSlow24 * faustpower<2>(fVec5[0]))));
			float fTemp7 = cosf(fTemp6);
			float fTemp8 = sinf(fTemp6);
			float fTemp9 = (0 - fTemp8);
			float fTemp10 = ((fRec21[1] * fTemp9) + (4 * (fVec5[0] * fTemp7)));
			float fTemp11 = ((fTemp9 * fRec22[1]) + (fTemp7 * fTemp10));
			float fTemp12 = ((fTemp9 * fRec23[1]) + (fTemp7 * fTemp11));
			float fTemp13 = ((fTemp9 * fRec24[1]) + (fTemp7 * fTemp12));
			float fTemp14 = ((fTemp9 * fRec25[1]) + (fTemp7 * fTemp13));
			fRec26[0] = ((fTemp9 * fRec26[1]) + (fTemp7 * fTemp14));
			fRec25[0] = ((fTemp8 * fTemp14) + (fTemp7 * fRec26[1]));
			fRec24[0] = ((fTemp8 * fTemp13) + (fTemp7 * fRec25[1]));
			fRec23[0] = ((fTemp8 * fTemp12) + (fTemp7 * fRec24[1]));
			fRec22[0] = ((fTemp8 * fTemp11) + (fTemp7 * fRec23[1]));
			fRec21[0] = ((fTemp8 * fTemp10) + (fTemp7 * fRec22[1]));
			fRec29[0] = (fSlow28 + (0.999f * fRec29[1]));
			float fTemp15 = (fRec28[1] + (fConst19 * (fSlow30 + (iSlow29 * fRec29[0]))));
			fRec28[0] = (fTemp15 - floorf(fTemp15));
			float fTemp16 = (3.141592653589793f * (fRec20[0] * ftbl0[int((65536.0f * fRec28[0]))]));
			float fTemp17 = cosf(fTemp16);
			float fTemp18 = sinf(fTemp16);
			float fTemp19 = (0 - fTemp18);
			float fTemp20 = ((fRec30[1] * fTemp19) + (4 * (fVec5[0] * fTemp17)));
			float fTemp21 = ((fTemp19 * fRec31[1]) + (fTemp17 * fTemp20));
			float fTemp22 = ((fTemp19 * fRec32[1]) + (fTemp17 * fTemp21));
			float fTemp23 = ((fTemp19 * fRec33[1]) + (fTemp17 * fTemp22));
			float fTemp24 = ((fTemp19 * fRec34[1]) + (fTemp17 * fTemp23));
			fRec35[0] = ((fTemp19 * fRec35[1]) + (fTemp17 * fTemp24));
			fRec34[0] = ((fTemp18 * fTemp24) + (fTemp17 * fRec35[1]));
			fRec33[0] = ((fTemp18 * fTemp23) + (fTemp17 * fRec34[1]));
			fRec32[0] = ((fTemp18 * fTemp22) + (fTemp17 * fRec33[1]));
			fRec31[0] = ((fTemp18 * fTemp21) + (fTemp17 * fRec32[1]));
			fRec30[0] = ((fTemp18 * fTemp20) + (fTemp17 * fRec31[1]));
			iRec36[0] = (iSlow4 & (iRec36[1] | (fRec37[1] >= 1)));
			int iTemp25 = (iSlow5 & (fRec37[1] > 0));
			fRec37[0] = (((fSlow35 * (((iRec36[1] == 0) & iSlow4) & (fRec37[1] < 1))) + (fRec37[1] * (1 - (fSlow33 * iTemp25)))) * ((iTemp25 == 0) | (fRec37[1] >= 1e-06f)));
			float fTemp26 = (fRec37[0] * ((iSlow31 * ((4 * (fVec5[0] * fTemp18)) + (fRec30[1] * fTemp17))) + (iSlow27 * ((fRec20[0] * ((4 * (fVec5[0] * fTemp8)) + (fRec21[1] * fTemp7))) + (4 * ((1 - fRec20[0]) * fVec5[0]))))));
			fVec6[IOTA&4095] = fTemp26;
			output0[i] = (FAUSTFLOAT)(fSlow37 * fVec6[IOTA&4095]);
			output1[i] = (FAUSTFLOAT)(fSlow39 * fVec6[(IOTA-iSlow38)&4095]);
			// post processing
			fRec37[1] = fRec37[0];
			iRec36[1] = iRec36[0];
			fRec30[1] = fRec30[0];
			fRec31[1] = fRec31[0];
			fRec32[1] = fRec32[0];
			fRec33[1] = fRec33[0];
			fRec34[1] = fRec34[0];
			fRec35[1] = fRec35[0];
			fRec28[1] = fRec28[0];
			fRec29[1] = fRec29[0];
			fRec21[1] = fRec21[0];
			fRec22[1] = fRec22[0];
			fRec23[1] = fRec23[0];
			fRec24[1] = fRec24[0];
			fRec25[1] = fRec25[0];
			fRec26[1] = fRec26[0];
			fRec20[1] = fRec20[0];
			fVec5[1] = fVec5[0];
			fRec5[1] = fRec5[0];
			fRec19[2] = fRec19[1]; fRec19[1] = fRec19[0];
			fRec4[1] = fRec4[0];
			fRec16[1] = fRec16[0];
			fRec17[2] = fRec17[1]; fRec17[1] = fRec17[0];
			fRec3[1] = fRec3[0];
			fRec14[1] = fRec14[0];
			fRec15[2] = fRec15[1]; fRec15[1] = fRec15[0];
			fRec2[1] = fRec2[0];
			fRec12[1] = fRec12[0];
			fRec13[2] = fRec13[1]; fRec13[1] = fRec13[0];
			fRec1[1] = fRec1[0];
			fRec10[1] = fRec10[0];
			fRec11[2] = fRec11[1]; fRec11[1] = fRec11[0];
			fRec0[1] = fRec0[0];
			fRec6[1] = fRec6[0];
			fRec7[2] = fRec7[1]; fRec7[1] = fRec7[0];
			IOTA = IOTA+1;
			fRec9[1] = fRec9[0];
			iRec8[1] = iRec8[0];
		}
	}
};


float 	Glass_Harmonica_dsp::ftbl0[65536];


#include "Faust_plugins_template2.cpp"

