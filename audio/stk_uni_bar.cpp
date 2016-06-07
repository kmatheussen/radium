//-----------------------------------------------------
// name: "UniBar"
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
#define FAUSTCLASS Uni_Bar_dsp
#endif

class Uni_Bar_dsp : public dsp {
  private:
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
	float 	fVec2[2048];
	float 	fConst13;
	float 	fConst14;
	float 	fRec12[3];
	float 	fRec11[2];
	float 	fRec2[2];
	float 	fVec3[2048];
	float 	fConst15;
	float 	fConst16;
	float 	fRec14[3];
	float 	fRec13[2];
	float 	fRec3[2];
	int 	iRec15[2];
	FAUSTFLOAT 	fslider3;
	FAUSTFLOAT 	fslider4;
	float 	fRec16[2];
	float 	fVec4[4096];
	FAUSTFLOAT 	fslider5;
	FAUSTFLOAT 	fslider6;
	float 	fConst17;
  public:
	static void metadata(Meta* m) 	{ 
		m->declare("name", "UniBar");
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
		m->declare("effect.lib/name", "Faust Audio Effect Library");
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
		fConst11 = (0.362844702467344f * iConst0);
		fConst12 = (17.31645870658694f / float(iConst0));
		for (int i=0; i<3; i++) fRec10[i] = 0;
		for (int i=0; i<2; i++) fRec9[i] = 0;
		for (int i=0; i<2; i++) fRec1[i] = 0;
		for (int i=0; i<2048; i++) fVec2[i] = 0;
		fConst13 = (0.1850481125092524f * iConst0);
		fConst14 = (33.95433339999848f / float(iConst0));
		for (int i=0; i<3; i++) fRec12[i] = 0;
		for (int i=0; i<2; i++) fRec11[i] = 0;
		for (int i=0; i<2; i++) fRec2[i] = 0;
		for (int i=0; i<2048; i++) fVec3[i] = 0;
		fConst15 = (0.1119444755401321f * iConst0);
		fConst16 = (56.127694349035245f / float(iConst0));
		for (int i=0; i<3; i++) fRec14[i] = 0;
		for (int i=0; i<2; i++) fRec13[i] = 0;
		for (int i=0; i<2; i++) fRec3[i] = 0;
		for (int i=0; i<2; i++) iRec15[i] = 0;
		fslider3 = 0.05f;
		fslider4 = 0.01f;
		for (int i=0; i<2; i++) fRec16[i] = 0;
		for (int i=0; i<4096; i++) fVec4[i] = 0;
		fslider5 = 0.6f;
		fslider6 = 0.5f;
		fConst17 = (0.5f * iConst0);
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
		faust_interface->declare(&fslider4, "3", "");
		faust_interface->declare(&fslider4, "tooltip", "Global envelope attack duration");
		faust_interface->declare(&fslider4, "unit", "s");
		faust_interface->addHorizontalSlider("Glob_Env_Attack", &fslider4, 0.01f, 0.0f, 2.0f, 0.01f);
		faust_interface->declare(&fslider3, "3", "");
		faust_interface->declare(&fslider3, "tooltip", "Global envelope release duration");
		faust_interface->declare(&fslider3, "unit", "s");
		faust_interface->addHorizontalSlider("Glob_Env_Release", &fslider3, 0.05f, 0.0f, 2.0f, 0.01f);
		faust_interface->closeBox();
		faust_interface->closeBox();
		faust_interface->openHorizontalBox("Physical_and_Nonlinearity");
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
		faust_interface->addHorizontalSlider("pan angle", &fslider5, 0.6f, 0.0f, 1.0f, 0.01f);
		faust_interface->addHorizontalSlider("spatial width", &fslider6, 0.5f, 0.0f, 1.0f, 0.01f);
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
		float 	fSlow9 = float(fentry2);
		int 	iSlow10 = int((int((float(iConst0) / fSlow9)) & 4095));
		float 	fSlow11 = (fConst7 * cosf((fConst6 * fSlow9)));
		int 	iSlow12 = int((int((fConst11 / fSlow9)) & 4095));
		float 	fSlow13 = (fConst7 * cosf((fConst12 * fSlow9)));
		int 	iSlow14 = int((int((fConst13 / fSlow9)) & 4095));
		float 	fSlow15 = (fConst7 * cosf((fConst14 * fSlow9)));
		int 	iSlow16 = int((int((fConst15 / fSlow9)) & 4095));
		float 	fSlow17 = (fConst7 * cosf((fConst16 * fSlow9)));
		float 	fSlow18 = float(fslider3);
		float 	fSlow19 = (1 - (1.0f / powf(1e+05f,(1.0f / ((fSlow18 == 0.0f) + (iConst0 * fSlow18))))));
		float 	fSlow20 = float(fslider4);
		float 	fSlow21 = (1.0f / ((fSlow20 == 0.0f) + (iConst0 * fSlow20)));
		float 	fSlow22 = float(fslider5);
		float 	fSlow23 = (7.0f * (1.0f - fSlow22));
		int 	iSlow24 = int((int((fConst17 * (float(fslider6) / fSlow9))) & 4095));
		float 	fSlow25 = (7.0f * fSlow22);
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
			float fTemp5 = (fSlow7 * fRec8[0]);
			fVec0[IOTA&4095] = (fTemp5 + (fTemp4 + (4.0f * fRec4[1])));
			fRec5[0] = (0 - (((fSlow11 * fRec5[1]) + (fConst5 * fRec5[2])) - (0.225f * fVec0[(IOTA-iSlow10)&4095])));
			fRec4[0] = ((fConst10 * fRec5[0]) + (fConst9 * fRec5[2]));
			fRec0[0] = fRec4[0];
			float fTemp6 = (fTemp4 + fTemp5);
			fVec1[IOTA&4095] = (fTemp6 + (4.0f * fRec9[1]));
			fRec10[0] = (0 - (((fSlow13 * fRec10[1]) + (fConst5 * fRec10[2])) - (0.2025f * fVec1[(IOTA-iSlow12)&4095])));
			fRec9[0] = ((fConst10 * fRec10[0]) + (fConst9 * fRec10[2]));
			fRec1[0] = fRec9[0];
			fVec2[IOTA&2047] = (fTemp6 + (4.0f * fRec11[1]));
			fRec12[0] = (0 - (((fSlow15 * fRec12[1]) + (fConst5 * fRec12[2])) - (0.18225000000000002f * fVec2[(IOTA-iSlow14)&2047])));
			fRec11[0] = ((fConst10 * fRec12[0]) + (fConst9 * fRec12[2]));
			fRec2[0] = fRec11[0];
			fVec3[IOTA&2047] = (fTemp6 + (4.0f * fRec13[1]));
			fRec14[0] = (0 - (((fSlow17 * fRec14[1]) + (fConst5 * fRec14[2])) - (0.164025f * fVec3[(IOTA-iSlow16)&2047])));
			fRec13[0] = ((fConst10 * fRec14[0]) + (fConst9 * fRec14[2]));
			fRec3[0] = fRec13[0];
			iRec15[0] = (iSlow1 & (iRec15[1] | (fRec16[1] >= 1)));
			int iTemp7 = (iSlow2 & (fRec16[1] > 0));
			fRec16[0] = (((fSlow21 * (((iRec15[1] == 0) & iSlow1) & (fRec16[1] < 1))) + (fRec16[1] * (1 - (fSlow19 * iTemp7)))) * ((iTemp7 == 0) | (fRec16[1] >= 1e-06f)));
			float fTemp8 = (fRec16[0] * (fRec3[0] + ((fRec0[0] + fRec2[0]) + fRec1[0])));
			fVec4[IOTA&4095] = fTemp8;
			output0[i] = (FAUSTFLOAT)(fSlow23 * fVec4[IOTA&4095]);
			output1[i] = (FAUSTFLOAT)(fSlow25 * fVec4[(IOTA-iSlow24)&4095]);
			// post processing
			fRec16[1] = fRec16[0];
			iRec15[1] = iRec15[0];
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




#include "Faust_plugins_template2.cpp"

