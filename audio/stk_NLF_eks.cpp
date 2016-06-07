//-----------------------------------------------------
// name: "Nonlinear EKS"
// author: "Julius Smith and Romain Michon"
// version: "1.0"
// license: "STK-4.3"
// copyright: "Julius Smith"
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
#define FAUSTCLASS NLF_Eks_dsp
#endif

class NLF_Eks_dsp : public dsp {
  private:
	class SIG0 {
	  private:
		int 	fSamplingFreq;
		int 	iRec11[2];
	  public:
		int getNumInputs() 	{ return 0; }
		int getNumOutputs() 	{ return 1; }
		void init(int samplingFreq) {
			fSamplingFreq = samplingFreq;
			for (int i=0; i<2; i++) iRec11[i] = 0;
		}
		void fill (int count, float output[]) {
			for (int i=0; i<count; i++) {
				iRec11[0] = (1 + iRec11[1]);
				output[i] = sinf((9.587379924285257e-05f * float((iRec11[0] - 1))));
				// post processing
				iRec11[1] = iRec11[0];
			}
		}
	};


	FAUSTFLOAT 	fentry0;
	int 	iConst0;
	float 	fConst1;
	FAUSTFLOAT 	fbutton0;
	float 	fVec0[2];
	float 	fRec2[2];
	int 	iRec3[2];
	FAUSTFLOAT 	fslider0;
	FAUSTFLOAT 	fentry1;
	int 	IOTA;
	float 	fRec1[4096];
	FAUSTFLOAT 	fslider1;
	FAUSTFLOAT 	fslider2;
	FAUSTFLOAT 	fslider3;
	FAUSTFLOAT 	fslider4;
	float 	fRec4[2];
	FAUSTFLOAT 	fslider5;
	FAUSTFLOAT 	fentry2;
	float 	fVec1[2];
	float 	fRec10[2];
	float 	fRec9[2];
	float 	fRec8[2];
	float 	fRec7[2];
	float 	fRec6[2];
	float 	fRec5[2];
	static float 	ftbl0[65536];
	FAUSTFLOAT 	fslider6;
	float 	fRec13[2];
	float 	fConst2;
	float 	fRec12[2];
	float 	fRec19[2];
	float 	fRec18[2];
	float 	fRec17[2];
	float 	fRec16[2];
	float 	fRec15[2];
	float 	fRec14[2];
	float 	fConst3;
	float 	fRec20[2];
	float 	fVec2[4096];
	float 	fRec0[4096];
	FAUSTFLOAT 	fslider7;
	FAUSTFLOAT 	fslider8;
	float 	fConst4;
  public:
	static void metadata(Meta* m) 	{ 
		m->declare("name", "Nonlinear EKS");
		m->declare("author", "Julius Smith and Romain Michon");
		m->declare("version", "1.0");
		m->declare("license", "STK-4.3");
		m->declare("copyright", "Julius Smith");
		m->declare("reference", "http://ccrma.stanford.edu/~jos/pasp/vegf.html");
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
		fentry0 = 4.4e+02f;
		iConst0 = min(192000, max(1, fSamplingFreq));
		fConst1 = (1.0f / float(iConst0));
		fbutton0 = 0.0;
		for (int i=0; i<2; i++) fVec0[i] = 0;
		for (int i=0; i<2; i++) fRec2[i] = 0;
		for (int i=0; i<2; i++) iRec3[i] = 0;
		fslider0 = 0.0f;
		fentry1 = 1.0f;
		IOTA = 0;
		for (int i=0; i<4096; i++) fRec1[i] = 0;
		fslider1 = 0.13f;
		fslider2 = -1e+01f;
		fslider3 = 0.5f;
		fslider4 = 0.0f;
		for (int i=0; i<2; i++) fRec4[i] = 0;
		fslider5 = 4.0f;
		fentry2 = 0.0f;
		for (int i=0; i<2; i++) fVec1[i] = 0;
		for (int i=0; i<2; i++) fRec10[i] = 0;
		for (int i=0; i<2; i++) fRec9[i] = 0;
		for (int i=0; i<2; i++) fRec8[i] = 0;
		for (int i=0; i<2; i++) fRec7[i] = 0;
		for (int i=0; i<2; i++) fRec6[i] = 0;
		for (int i=0; i<2; i++) fRec5[i] = 0;
		fslider6 = 2.2e+02f;
		for (int i=0; i<2; i++) fRec13[i] = 0;
		fConst2 = (1.0f / float(iConst0));
		for (int i=0; i<2; i++) fRec12[i] = 0;
		for (int i=0; i<2; i++) fRec19[i] = 0;
		for (int i=0; i<2; i++) fRec18[i] = 0;
		for (int i=0; i<2; i++) fRec17[i] = 0;
		for (int i=0; i<2; i++) fRec16[i] = 0;
		for (int i=0; i<2; i++) fRec15[i] = 0;
		for (int i=0; i<2; i++) fRec14[i] = 0;
		fConst3 = (3.141592653589793f / float(iConst0));
		for (int i=0; i<2; i++) fRec20[i] = 0;
		for (int i=0; i<4096; i++) fVec2[i] = 0;
		for (int i=0; i<4096; i++) fRec0[i] = 0;
		fslider7 = 0.6f;
		fslider8 = 0.5f;
		fConst4 = (0.5f * iConst0);
	}
	virtual void init(int samplingFreq) {
		classInit(samplingFreq);
		instanceInit(samplingFreq);
	}
	virtual void buildUserInterface(UI* faust_interface) {
		faust_interface->openVerticalBox("0x00");
		faust_interface->openVerticalBox("Nonlinear Filter");
		faust_interface->addNumEntry("typeMod", &fentry2, 0.0f, 0.0f, 4.0f, 1.0f);
		faust_interface->closeBox();
		faust_interface->addHorizontalSlider("Nonlinearity", &fslider4, 0.0f, 0.0f, 1.0f, 0.01f);
		faust_interface->openVerticalBox("Spat");
		faust_interface->addHorizontalSlider("pan angle", &fslider7, 0.6f, 0.0f, 1.0f, 0.01f);
		faust_interface->addHorizontalSlider("spatial width", &fslider8, 0.5f, 0.0f, 1.0f, 0.01f);
		faust_interface->closeBox();
		faust_interface->declare(&fslider3, "midi", "ctrl 0x74");
		faust_interface->addHorizontalSlider("brightness", &fslider3, 0.5f, 0.0f, 1.0f, 0.01f);
		faust_interface->addHorizontalSlider("decaytime_T60", &fslider5, 4.0f, 0.0f, 1e+01f, 0.01f);
		faust_interface->addHorizontalSlider("dynamic_level", &fslider2, -1e+01f, -6e+01f, 0.0f, 1.0f);
		faust_interface->addNumEntry("freq", &fentry0, 4.4e+02f, 2e+01f, 7.04e+03f, 1.0f);
		faust_interface->addHorizontalSlider("freqMod", &fslider6, 2.2e+02f, 2e+01f, 1e+03f, 0.1f);
		faust_interface->addNumEntry("gain", &fentry1, 1.0f, 0.0f, 1e+01f, 0.01f);
		faust_interface->addButton("gate", &fbutton0);
		faust_interface->addHorizontalSlider("pick_angle", &fslider0, 0.0f, 0.0f, 0.9f, 0.1f);
		faust_interface->declare(&fslider1, "midi", "ctrl 0x81");
		faust_interface->addHorizontalSlider("pick_position", &fslider1, 0.13f, 0.02f, 0.5f, 0.01f);
		faust_interface->closeBox();
	}
	virtual void compute (int count, FAUSTFLOAT** input, FAUSTFLOAT** output) {
		float 	fSlow0 = float(fentry0);
		float 	fSlow1 = (fConst1 * fSlow0);
		float 	fSlow2 = float(fbutton0);
		float 	fSlow3 = (0.9f * float(fslider0));
		float 	fSlow4 = (4.656612875245797e-10f * (float(fentry1) * (1.0f - fSlow3)));
		int 	iSlow5 = int((int((iConst0 * (float(fslider1) / fSlow0))) & 4095));
		float 	fSlow6 = powf(10,(0.05f * float(fslider2)));
		float 	fSlow7 = (fSlow6 * powf(fSlow6,0.3333333333333333f));
		float 	fSlow8 = float(fslider3);
		float 	fSlow9 = (0.25f * (1.0f - fSlow8));
		float 	fSlow10 = (0.5f * (1.0f + fSlow8));
		float 	fSlow11 = (0.0010000000000000009f * float(fslider4));
		float 	fSlow12 = powf(0.001f,(1.0f / (fSlow0 * float(fslider5))));
		float 	fSlow13 = float(fentry2);
		float 	fSlow14 = (3.141592653589793f * (faustpower<2>(fSlow12) * (fSlow13 == 2)));
		float 	fSlow15 = (1.5707963267948966f * (fSlow13 == 1));
		float 	fSlow16 = (3.141592653589793f * (fSlow12 * (fSlow13 == 0)));
		int 	iSlow17 = (fSlow13 < 3);
		float 	fSlow18 = (0.0010000000000000009f * float(fslider6));
		int 	iSlow19 = (fSlow13 != 4);
		float 	fSlow20 = (fSlow0 * (fSlow13 == 4));
		int 	iSlow21 = (fSlow13 >= 3);
		float 	fSlow22 = (fConst3 * fSlow0);
		float 	fSlow23 = (1.0f - fSlow22);
		float 	fSlow24 = (1.0f / (1.0f + fSlow22));
		float 	fSlow25 = (1.0f - fSlow6);
		float 	fSlow26 = (float(iConst0) / fSlow0);
		int 	iSlow27 = int((fSlow26 - 3.499995f));
		int 	iSlow28 = int((int((4 + iSlow27)) & 4095));
		float 	fSlow29 = (fSlow26 - (5.0f + iSlow27));
		float 	fSlow30 = (fSlow26 - (4.0f + iSlow27));
		float 	fSlow31 = (fSlow26 - (iSlow27 + 3.0f));
		float 	fSlow32 = (fSlow31 * fSlow30);
		float 	fSlow33 = (0.041666666666666664f * (fSlow32 * fSlow29));
		int 	iSlow34 = int((int((3 + iSlow27)) & 4095));
		float 	fSlow35 = (fSlow26 - (iSlow27 + 6.0f));
		float 	fSlow36 = (0.16666666666666666f * (fSlow32 * (0 - fSlow35)));
		int 	iSlow37 = int((int((2 + iSlow27)) & 4095));
		float 	fSlow38 = (0.5f * ((fSlow31 * (0 - fSlow29)) * (0 - (0.5f * fSlow35))));
		int 	iSlow39 = int((int((1 + iSlow27)) & 4095));
		float 	fSlow40 = (((0 - fSlow30) * (0 - (0.5f * fSlow29))) * (0 - (0.3333333333333333f * fSlow35)));
		float 	fSlow41 = (fSlow26 - (2.0f + iSlow27));
		int 	iSlow42 = int((iSlow27 & 4095));
		float 	fSlow43 = ((((0 - fSlow31) * (0 - (0.5f * fSlow30))) * (0 - (0.3333333333333333f * fSlow29))) * (0 - (0.25f * fSlow35)));
		float 	fSlow44 = float(fslider7);
		float 	fSlow45 = (1.0f - fSlow44);
		int 	iSlow46 = int((int((fConst4 * (float(fslider8) / fSlow0))) & 4095));
		FAUSTFLOAT* output0 = output[0];
		FAUSTFLOAT* output1 = output[1];
		for (int i=0; i<count; i++) {
			fVec0[0] = fSlow2;
			fRec2[0] = ((fRec2[1] + ((fSlow2 - fVec0[1]) > 0)) - (fSlow1 * (fRec2[1] > 0)));
			iRec3[0] = (12345 + (1103515245 * iRec3[1]));
			fRec1[IOTA&4095] = ((fSlow3 * fRec1[(IOTA-1)&4095]) + (fSlow4 * (iRec3[0] * (fRec2[0] > 0.0f))));
			float fTemp0 = (fRec1[(IOTA-0)&4095] - fRec1[(IOTA-iSlow5)&4095]);
			float fTemp1 = ((fSlow10 * fRec0[(IOTA-2)&4095]) + (fSlow9 * (fRec0[(IOTA-1)&4095] + fRec0[(IOTA-3)&4095])));
			fRec4[0] = (fSlow11 + (0.999f * fRec4[1]));
			float fTemp2 = (fSlow12 * fTemp1);
			fVec1[0] = fTemp2;
			float fTemp3 = (fRec4[0] * (((fSlow16 * fTemp1) + (fSlow15 * (fVec1[0] + fVec1[1]))) + (fSlow14 * faustpower<2>(fTemp1))));
			float fTemp4 = cosf(fTemp3);
			float fTemp5 = sinf(fTemp3);
			float fTemp6 = (0 - fTemp5);
			float fTemp7 = ((fRec5[1] * fTemp6) + (fSlow12 * (fTemp1 * fTemp4)));
			float fTemp8 = ((fTemp6 * fRec6[1]) + (fTemp4 * fTemp7));
			float fTemp9 = ((fTemp6 * fRec7[1]) + (fTemp4 * fTemp8));
			float fTemp10 = ((fTemp6 * fRec8[1]) + (fTemp4 * fTemp9));
			float fTemp11 = ((fTemp6 * fRec9[1]) + (fTemp4 * fTemp10));
			fRec10[0] = ((fTemp6 * fRec10[1]) + (fTemp4 * fTemp11));
			fRec9[0] = ((fTemp5 * fTemp11) + (fTemp4 * fRec10[1]));
			fRec8[0] = ((fTemp5 * fTemp10) + (fTemp4 * fRec9[1]));
			fRec7[0] = ((fTemp5 * fTemp9) + (fTemp4 * fRec8[1]));
			fRec6[0] = ((fTemp5 * fTemp8) + (fTemp4 * fRec7[1]));
			fRec5[0] = ((fTemp5 * fTemp7) + (fTemp4 * fRec6[1]));
			fRec13[0] = (fSlow18 + (0.999f * fRec13[1]));
			float fTemp12 = (fRec12[1] + (fConst2 * (fSlow20 + (iSlow19 * fRec13[0]))));
			fRec12[0] = (fTemp12 - floorf(fTemp12));
			float fTemp13 = (3.141592653589793f * (fRec4[0] * ftbl0[int((65536.0f * fRec12[0]))]));
			float fTemp14 = cosf(fTemp13);
			float fTemp15 = sinf(fTemp13);
			float fTemp16 = (0 - fTemp15);
			float fTemp17 = ((fRec14[1] * fTemp16) + (fSlow12 * (fTemp1 * fTemp14)));
			float fTemp18 = ((fTemp16 * fRec15[1]) + (fTemp14 * fTemp17));
			float fTemp19 = ((fTemp16 * fRec16[1]) + (fTemp14 * fTemp18));
			float fTemp20 = ((fTemp16 * fRec17[1]) + (fTemp14 * fTemp19));
			float fTemp21 = ((fTemp16 * fRec18[1]) + (fTemp14 * fTemp20));
			fRec19[0] = ((fTemp16 * fRec19[1]) + (fTemp14 * fTemp21));
			fRec18[0] = ((fTemp15 * fTemp21) + (fTemp14 * fRec19[1]));
			fRec17[0] = ((fTemp15 * fTemp20) + (fTemp14 * fRec18[1]));
			fRec16[0] = ((fTemp15 * fTemp19) + (fTemp14 * fRec17[1]));
			fRec15[0] = ((fTemp15 * fTemp18) + (fTemp14 * fRec16[1]));
			fRec14[0] = ((fTemp15 * fTemp17) + (fTemp14 * fRec15[1]));
			fRec20[0] = (fSlow24 * ((fSlow23 * fRec20[1]) + (fSlow22 * fTemp0)));
			float fTemp22 = ((fSlow25 * fRec20[0]) + (((iSlow21 * ((fSlow12 * (fTemp1 * fTemp15)) + (fRec14[1] * fTemp14))) + (iSlow17 * ((fRec4[0] * ((fSlow12 * (fTemp1 * fTemp5)) + (fRec5[1] * fTemp4))) + (fSlow12 * ((1 - fRec4[0]) * fTemp1))))) + (fSlow7 * fTemp0)));
			fVec2[IOTA&4095] = fTemp22;
			fRec0[IOTA&4095] = ((fSlow43 * fVec2[(IOTA-iSlow42)&4095]) + (fSlow41 * ((((fSlow40 * fVec2[(IOTA-iSlow39)&4095]) + (fSlow38 * fVec2[(IOTA-iSlow37)&4095])) + (fSlow36 * fVec2[(IOTA-iSlow34)&4095])) + (fSlow33 * fVec2[(IOTA-iSlow28)&4095]))));
			output0[i] = (FAUSTFLOAT)(fSlow45 * fRec0[(IOTA-0)&4095]);
			output1[i] = (FAUSTFLOAT)(fSlow44 * fRec0[(IOTA-iSlow46)&4095]);
			// post processing
			fRec20[1] = fRec20[0];
			fRec14[1] = fRec14[0];
			fRec15[1] = fRec15[0];
			fRec16[1] = fRec16[0];
			fRec17[1] = fRec17[0];
			fRec18[1] = fRec18[0];
			fRec19[1] = fRec19[0];
			fRec12[1] = fRec12[0];
			fRec13[1] = fRec13[0];
			fRec5[1] = fRec5[0];
			fRec6[1] = fRec6[0];
			fRec7[1] = fRec7[0];
			fRec8[1] = fRec8[0];
			fRec9[1] = fRec9[0];
			fRec10[1] = fRec10[0];
			fVec1[1] = fVec1[0];
			fRec4[1] = fRec4[0];
			IOTA = IOTA+1;
			iRec3[1] = iRec3[0];
			fRec2[1] = fRec2[0];
			fVec0[1] = fVec0[0];
		}
	}
};


float 	NLF_Eks_dsp::ftbl0[65536];


#include "Faust_plugins_template2.cpp"

