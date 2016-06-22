/* ------------------------------------------------------------
author: "Julius Smith and Romain Michon"
copyright: "Julius Smith"
license: "STK-4.3"
name: "Nonlinear EKS"
version: "1.0"
Code generated with Faust 2.0.a43 (http://faust.grame.fr)
------------------------------------------------------------ */

#ifndef  __NLF_Eks_dsp_H__
#define  __NLF_Eks_dsp_H__

/*
#include "/usr/share/faust/audio/dsp.h"
#include "/usr/share/faust/gui/UI.h"
*/

// We use faust1 here.

struct Meta
{
    void declare (const char* key, const char* value) { }
};


#include <faust/dsp/dsp.h>


#if 0 //CREATE_NAME==create_zita_rev_plugin

  #include "mfaustqt1.cpp"

#else

  #include <faust/gui/faustqt.h>

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

#include <math.h>


class NLF_Eks_dspSIG0 {
	
  private:
	
	int iRec6[2];
	
  public:
	
	int getNumInputsNLF_Eks_dspSIG0() {
		return 0;
		
	}
	int getNumOutputsNLF_Eks_dspSIG0() {
		return 1;
		
	}
	int getInputRateNLF_Eks_dspSIG0(int channel) {
		int rate;
		switch (channel) {
			default: {
				rate = -1;
				break;
			}
			
		}
		return rate;
		
	}
	int getOutputRateNLF_Eks_dspSIG0(int channel) {
		int rate;
		switch (channel) {
			case 0: {
				rate = 0;
				break;
			}
			default: {
				rate = -1;
				break;
			}
			
		}
		return rate;
		
	}
	
	void instanceInitNLF_Eks_dspSIG0(int samplingFreq) {
		for (int i6 = 0; (i6 < 2); i6 = (i6 + 1)) {
			iRec6[i6] = 0;
			
		}
		
	}
	
	void fillNLF_Eks_dspSIG0(int count, float* output) {
		for (int i = 0; (i < count); i = (i + 1)) {
			iRec6[0] = (1 + iRec6[1]);
			output[i] = sinf((9.58738e-05f * float((iRec6[0] - 1))));
			iRec6[1] = iRec6[0];
			
		}
		
	}
};

NLF_Eks_dspSIG0* newNLF_Eks_dspSIG0() { return (NLF_Eks_dspSIG0*)new NLF_Eks_dspSIG0(); }
void deleteNLF_Eks_dspSIG0(NLF_Eks_dspSIG0* dsp) { delete dsp; }

static float ftbl0NLF_Eks_dspSIG0[65536];
static float faustpower2_f(float value) {
	return (value * value);
	
}

#ifndef FAUSTCLASS 
#define FAUSTCLASS NLF_Eks_dsp
#endif

class NLF_Eks_dsp : public dsp {
	
  private:
	
	float fRec2[4096];
	float fVec2[4096];
	float fRec0[4096];
	int iRec3[2];
	float fVec0[2];
	float fRec4[2];
	float fRec1[2];
	float fRec5[2];
	float fRec8[2];
	float fRec7[2];
	float fRec14[2];
	float fRec13[2];
	float fRec12[2];
	float fRec11[2];
	float fRec10[2];
	float fRec9[2];
	float fVec1[2];
	float fRec20[2];
	float fRec19[2];
	float fRec18[2];
	float fRec17[2];
	float fRec16[2];
	float fRec15[2];
	FAUSTFLOAT fHslider0;
	int fSamplingFreq;
	float fConst0;
	FAUSTFLOAT fEntry0;
	FAUSTFLOAT fHslider1;
	float fConst1;
	FAUSTFLOAT fHslider2;
	FAUSTFLOAT fEntry1;
	FAUSTFLOAT fButton0;
	float fConst2;
	int IOTA;
	FAUSTFLOAT fHslider3;
	FAUSTFLOAT fEntry2;
	FAUSTFLOAT fHslider4;
	FAUSTFLOAT fHslider5;
	FAUSTFLOAT fHslider6;
	float fConst3;
	FAUSTFLOAT fHslider7;
	float fConst4;
	FAUSTFLOAT fHslider8;
	
  public:
	
	void static metadata(Meta* m) { 
		m->declare("author", "Julius Smith and Romain Michon");
		m->declare("copyright", "Julius Smith");
		m->declare("effect.lib/author", "Julius O. Smith (jos at ccrma.stanford.edu)");
		m->declare("effect.lib/copyright", "Julius O. Smith III");
		m->declare("effect.lib/exciter_author", "Priyanka Shekar (pshekar@ccrma.stanford.edu)");
		m->declare("effect.lib/exciter_copyright", "Copyright (c) 2013 Priyanka Shekar");
		m->declare("effect.lib/exciter_license", "MIT License (MIT)");
		m->declare("effect.lib/exciter_name", "Harmonic Exciter");
		m->declare("effect.lib/exciter_version", "1.0");
		m->declare("effect.lib/license", "STK-4.3");
		m->declare("effect.lib/name", "Faust Audio Effect Library");
		m->declare("effect.lib/version", "1.33");
		m->declare("filter.lib/author", "Julius O. Smith (jos at ccrma.stanford.edu)");
		m->declare("filter.lib/copyright", "Julius O. Smith III");
		m->declare("filter.lib/license", "STK-4.3");
		m->declare("filter.lib/name", "Faust Filter Library");
		m->declare("filter.lib/reference", "https://ccrma.stanford.edu/~jos/filters/");
		m->declare("filter.lib/version", "1.29");
		m->declare("instrument.lib/author", "Romain Michon (rmichon@ccrma.stanford.edu)");
		m->declare("instrument.lib/copyright", "Romain Michon");
		m->declare("instrument.lib/licence", "STK-4.3");
		m->declare("instrument.lib/name", "Faust-STK Tools Library");
		m->declare("instrument.lib/version", "1.0");
		m->declare("license", "STK-4.3");
		m->declare("math.lib/author", "GRAME");
		m->declare("math.lib/copyright", "GRAME");
		m->declare("math.lib/license", "LGPL with exception");
		m->declare("math.lib/name", "Math Library");
		m->declare("math.lib/version", "1.0");
		m->declare("music.lib/author", "GRAME");
		m->declare("music.lib/copyright", "GRAME");
		m->declare("music.lib/license", "LGPL with exception");
		m->declare("music.lib/name", "Music Library");
		m->declare("music.lib/version", "1.0");
		m->declare("name", "Nonlinear EKS");
		m->declare("reference", "http://ccrma.stanford.edu/~jos/pasp/vegf.html");
		m->declare("version", "1.0");
	}

	virtual int getNumInputs() {
		return 0;
		
	}
	virtual int getNumOutputs() {
		return 2;
		
	}
	virtual int getInputRate(int channel) {
		int rate;
		switch (channel) {
			default: {
				rate = -1;
				break;
			}
			
		}
		return rate;
		
	}
	virtual int getOutputRate(int channel) {
		int rate;
		switch (channel) {
			case 0: {
				rate = 1;
				break;
			}
			case 1: {
				rate = 1;
				break;
			}
			default: {
				rate = -1;
				break;
			}
			
		}
		return rate;
		
	}
	
	static void classInit(int samplingFreq) {
		NLF_Eks_dspSIG0* sig0 = newNLF_Eks_dspSIG0();
		sig0->instanceInitNLF_Eks_dspSIG0(samplingFreq);
		sig0->fillNLF_Eks_dspSIG0(65536, ftbl0NLF_Eks_dspSIG0);
		deleteNLF_Eks_dspSIG0(sig0);
		
	}
	
	virtual void instanceInit(int samplingFreq) {
		fSamplingFreq = samplingFreq;
		fHslider0 = FAUSTFLOAT(0.6f);
		fConst0 = min(1.92e+05f, max(1.0f, float(fSamplingFreq)));
		fEntry0 = FAUSTFLOAT(4.4e+02f);
		fHslider1 = FAUSTFLOAT(-1e+01f);
		fConst1 = (3.1415927f / fConst0);
		fHslider2 = FAUSTFLOAT(0.0f);
		fEntry1 = FAUSTFLOAT(1.0f);
		for (int i0 = 0; (i0 < 2); i0 = (i0 + 1)) {
			iRec3[i0] = 0;
			
		}
		fButton0 = FAUSTFLOAT(0.0f);
		for (int i1 = 0; (i1 < 2); i1 = (i1 + 1)) {
			fVec0[i1] = 0.0f;
			
		}
		fConst2 = (1.0f / fConst0);
		for (int i2 = 0; (i2 < 2); i2 = (i2 + 1)) {
			fRec4[i2] = 0.0f;
			
		}
		IOTA = 0;
		for (int i3 = 0; (i3 < 4096); i3 = (i3 + 1)) {
			fRec2[i3] = 0.0f;
			
		}
		fHslider3 = FAUSTFLOAT(0.13f);
		for (int i4 = 0; (i4 < 2); i4 = (i4 + 1)) {
			fRec1[i4] = 0.0f;
			
		}
		fEntry2 = FAUSTFLOAT(0.0f);
		fHslider4 = FAUSTFLOAT(4.0f);
		fHslider5 = FAUSTFLOAT(0.5f);
		fHslider6 = FAUSTFLOAT(0.0f);
		for (int i5 = 0; (i5 < 2); i5 = (i5 + 1)) {
			fRec5[i5] = 0.0f;
			
		}
		fConst3 = (1.0f / fConst0);
		fHslider7 = FAUSTFLOAT(2.2e+02f);
		for (int i7 = 0; (i7 < 2); i7 = (i7 + 1)) {
			fRec8[i7] = 0.0f;
			
		}
		for (int i8 = 0; (i8 < 2); i8 = (i8 + 1)) {
			fRec7[i8] = 0.0f;
			
		}
		for (int i9 = 0; (i9 < 2); i9 = (i9 + 1)) {
			fRec14[i9] = 0.0f;
			
		}
		for (int i10 = 0; (i10 < 2); i10 = (i10 + 1)) {
			fRec13[i10] = 0.0f;
			
		}
		for (int i11 = 0; (i11 < 2); i11 = (i11 + 1)) {
			fRec12[i11] = 0.0f;
			
		}
		for (int i12 = 0; (i12 < 2); i12 = (i12 + 1)) {
			fRec11[i12] = 0.0f;
			
		}
		for (int i13 = 0; (i13 < 2); i13 = (i13 + 1)) {
			fRec10[i13] = 0.0f;
			
		}
		for (int i14 = 0; (i14 < 2); i14 = (i14 + 1)) {
			fRec9[i14] = 0.0f;
			
		}
		for (int i15 = 0; (i15 < 2); i15 = (i15 + 1)) {
			fVec1[i15] = 0.0f;
			
		}
		for (int i16 = 0; (i16 < 2); i16 = (i16 + 1)) {
			fRec20[i16] = 0.0f;
			
		}
		for (int i17 = 0; (i17 < 2); i17 = (i17 + 1)) {
			fRec19[i17] = 0.0f;
			
		}
		for (int i18 = 0; (i18 < 2); i18 = (i18 + 1)) {
			fRec18[i18] = 0.0f;
			
		}
		for (int i19 = 0; (i19 < 2); i19 = (i19 + 1)) {
			fRec17[i19] = 0.0f;
			
		}
		for (int i20 = 0; (i20 < 2); i20 = (i20 + 1)) {
			fRec16[i20] = 0.0f;
			
		}
		for (int i21 = 0; (i21 < 2); i21 = (i21 + 1)) {
			fRec15[i21] = 0.0f;
			
		}
		for (int i22 = 0; (i22 < 4096); i22 = (i22 + 1)) {
			fVec2[i22] = 0.0f;
			
		}
		for (int i23 = 0; (i23 < 4096); i23 = (i23 + 1)) {
			fRec0[i23] = 0.0f;
			
		}
		fConst4 = (0.5f * fConst0);
		fHslider8 = FAUSTFLOAT(0.5f);
		
	}
	
	virtual void init(int samplingFreq) {
		classInit(samplingFreq);
		instanceInit(samplingFreq);
	}
	
	virtual void buildUserInterface(UI* interface) {
		interface->openVerticalBox("0x00");
		interface->openVerticalBox("Nonlinear Filter");
		interface->addNumEntry("typeMod", &fEntry2, 0.0f, 0.0f, 4.0f, 1.0f);
		interface->closeBox();
		interface->addHorizontalSlider("Nonlinearity", &fHslider6, 0.0f, 0.0f, 1.0f, 0.01f);
		interface->openVerticalBox("Spat");
		interface->addHorizontalSlider("pan angle", &fHslider0, 0.6f, 0.0f, 1.0f, 0.01f);
		interface->addHorizontalSlider("spatial width", &fHslider8, 0.5f, 0.0f, 1.0f, 0.01f);
		interface->closeBox();
		interface->declare(&fHslider5, "midi", "ctrl 0x74");
		interface->addHorizontalSlider("brightness", &fHslider5, 0.5f, 0.0f, 1.0f, 0.01f);
		interface->addHorizontalSlider("decaytime_T60", &fHslider4, 4.0f, 0.0f, 1e+01f, 0.01f);
		interface->addHorizontalSlider("dynamic_level", &fHslider1, -1e+01f, -6e+01f, 0.0f, 1.0f);
		interface->addNumEntry("freq", &fEntry0, 4.4e+02f, 2e+01f, 7.04e+03f, 1.0f);
		interface->addHorizontalSlider("freqMod", &fHslider7, 2.2e+02f, 2e+01f, 1e+03f, 0.1f);
		interface->addNumEntry("gain", &fEntry1, 1.0f, 0.0f, 1e+01f, 0.01f);
		interface->addButton("gate",&fButton0);
		interface->addHorizontalSlider("pick_angle", &fHslider2, 0.0f, 0.0f, 0.9f, 0.1f);
		interface->declare(&fHslider3, "midi", "ctrl 0x81");
		interface->addHorizontalSlider("pick_position", &fHslider3, 0.13f, 0.02f, 0.5f, 0.01f);
		interface->closeBox();
		
	}
	
	virtual void compute(int count, FAUSTFLOAT** inputs, FAUSTFLOAT** outputs) {
		FAUSTFLOAT* output0 = outputs[0];
		FAUSTFLOAT* output1 = outputs[1];
		float fSlow0 = float(fHslider0);
		float fSlow1 = (1.0f - fSlow0);
		float fSlow2 = float(fEntry0);
		float fSlow3 = (fConst0 / fSlow2);
		int iSlow4 = int((fSlow3 - 3.499995f));
		float fSlow5 = (fSlow3 - (3.0f + float(iSlow4)));
		float fSlow6 = (fSlow3 - (4.0f + float(iSlow4)));
		float fSlow7 = (fSlow3 - (5.0f + float(iSlow4)));
		float fSlow8 = (fSlow3 - (6.0f + float(iSlow4)));
		float fSlow9 = ((((0.0f - fSlow5) * (0.0f - (0.5f * fSlow6))) * (0.0f - (0.33333334f * fSlow7))) * (0.0f - (0.25f * fSlow8)));
		float fSlow10 = powf(1e+01f, (0.05f * float(fHslider1)));
		float fSlow11 = (1.0f - fSlow10);
		float fSlow12 = (fConst1 * fSlow2);
		float fSlow13 = (1.0f / (1.0f + fSlow12));
		float fSlow14 = (1.0f - fSlow12);
		float fSlow15 = (0.9f * float(fHslider2));
		float fSlow16 = (4.656613e-10f * (float(fEntry1) * (1.0f - fSlow15)));
		float fSlow17 = float(fButton0);
		float fSlow18 = (fConst2 * fSlow2);
		int iSlow19 = (int((fConst0 * (float(fHslider3) / fSlow2))) & 4095);
		float fSlow20 = float(fEntry2);
		int iSlow21 = (fSlow20 >= 3.0f);
		float fSlow22 = powf(0.001f, (1.0f / (fSlow2 * float(fHslider4))));
		float fSlow23 = float(fHslider5);
		float fSlow24 = (0.5f * (1.0f + fSlow23));
		float fSlow25 = (0.25f * (1.0f - fSlow23));
		float fSlow26 = (0.001f * float(fHslider6));
		int iSlow27 = (fSlow20 != 4.0f);
		float fSlow28 = (0.001f * float(fHslider7));
		float fSlow29 = (fSlow2 * float((fSlow20 == 4.0f)));
		int iSlow30 = (fSlow20 < 3.0f);
		float fSlow31 = (3.1415927f * (fSlow22 * float((fSlow20 == 0.0f))));
		float fSlow32 = (1.5707964f * float((fSlow20 == 1.0f)));
		float fSlow33 = (3.1415927f * (faustpower2_f(fSlow22) * float((fSlow20 == 2.0f))));
		float fSlow34 = (powf(fSlow10, 0.33333334f) * fSlow10);
		int iSlow35 = (iSlow4 & 4095);
		float fSlow36 = (fSlow3 - (2.0f + float(iSlow4)));
		float fSlow37 = (((0.0f - fSlow6) * (0.0f - (0.5f * fSlow7))) * (0.0f - (0.33333334f * fSlow8)));
		int iSlow38 = ((1 + iSlow4) & 4095);
		float fSlow39 = (0.5f * ((fSlow5 * (0.0f - fSlow7)) * (0.0f - (0.5f * fSlow8))));
		int iSlow40 = ((2 + iSlow4) & 4095);
		float fSlow41 = (fSlow5 * fSlow6);
		float fSlow42 = (0.16666667f * (fSlow41 * (0.0f - fSlow8)));
		int iSlow43 = ((3 + iSlow4) & 4095);
		float fSlow44 = (0.041666668f * (fSlow41 * fSlow7));
		int iSlow45 = ((4 + iSlow4) & 4095);
		int iSlow46 = (int((fConst4 * (float(fHslider8) / fSlow2))) & 4095);
		for (int i = 0; (i < count); i = (i + 1)) {
			iRec3[0] = (12345 + (1103515245 * iRec3[1]));
			fVec0[0] = fSlow17;
			fRec4[0] = ((fRec4[1] + float(((fSlow17 - fVec0[1]) > 0.0f))) - (fSlow18 * float((fRec4[1] > 0.0f))));
			fRec2[(IOTA & 4095)] = ((fSlow15 * fRec2[((IOTA - 1) & 4095)]) + (fSlow16 * float((iRec3[0] * (fRec4[0] > 0.0f)))));
			float fTemp0 = (fRec2[((IOTA - 0) & 4095)] - fRec2[((IOTA - iSlow19) & 4095)]);
			fRec1[0] = (fSlow13 * ((fSlow14 * fRec1[1]) + (fSlow12 * fTemp0)));
			float fTemp1 = ((fSlow24 * fRec0[((IOTA - 2) & 4095)]) + (fSlow25 * (fRec0[((IOTA - 1) & 4095)] + fRec0[((IOTA - 3) & 4095)])));
			fRec5[0] = ((0.999f * fRec5[1]) + fSlow26);
			fRec8[0] = ((0.999f * fRec8[1]) + fSlow28);
			float fTemp2 = (fRec7[1] + (fConst3 * ((float(iSlow27) * fRec8[0]) + fSlow29)));
			fRec7[0] = (fTemp2 - floorf(fTemp2));
			float fTemp3 = (3.1415927f * (fRec5[0] * ftbl0NLF_Eks_dspSIG0[int((65536.0f * fRec7[0]))]));
			float fTemp4 = sinf(fTemp3);
			float fTemp5 = (0.0f - fTemp4);
			float fTemp6 = cosf(fTemp3);
			float fTemp7 = ((fRec9[1] * fTemp5) + (fSlow22 * (fTemp1 * fTemp6)));
			float fTemp8 = ((fTemp5 * fRec10[1]) + (fTemp6 * fTemp7));
			float fTemp9 = ((fTemp5 * fRec11[1]) + (fTemp6 * fTemp8));
			float fTemp10 = ((fTemp5 * fRec12[1]) + (fTemp6 * fTemp9));
			float fTemp11 = ((fTemp5 * fRec13[1]) + (fTemp6 * fTemp10));
			fRec14[0] = ((fTemp5 * fRec14[1]) + (fTemp6 * fTemp11));
			fRec13[0] = ((fTemp4 * fTemp11) + (fTemp6 * fRec14[1]));
			fRec12[0] = ((fTemp4 * fTemp10) + (fTemp6 * fRec13[1]));
			fRec11[0] = ((fTemp4 * fTemp9) + (fTemp6 * fRec12[1]));
			fRec10[0] = ((fTemp4 * fTemp8) + (fTemp6 * fRec11[1]));
			fRec9[0] = ((fTemp4 * fTemp7) + (fTemp6 * fRec10[1]));
			float fTemp12 = (fSlow22 * fTemp1);
			fVec1[0] = fTemp12;
			float fTemp13 = (fRec5[0] * (((fSlow31 * fTemp1) + (fSlow32 * (fTemp12 + fVec1[1]))) + (fSlow33 * faustpower2_f(fTemp1))));
			float fTemp14 = sinf(fTemp13);
			float fTemp15 = (0.0f - fTemp14);
			float fTemp16 = cosf(fTemp13);
			float fTemp17 = ((fRec15[1] * fTemp15) + (fSlow22 * (fTemp1 * fTemp16)));
			float fTemp18 = ((fTemp15 * fRec16[1]) + (fTemp16 * fTemp17));
			float fTemp19 = ((fTemp15 * fRec17[1]) + (fTemp16 * fTemp18));
			float fTemp20 = ((fTemp15 * fRec18[1]) + (fTemp16 * fTemp19));
			float fTemp21 = ((fTemp15 * fRec19[1]) + (fTemp16 * fTemp20));
			fRec20[0] = ((fTemp15 * fRec20[1]) + (fTemp16 * fTemp21));
			fRec19[0] = ((fTemp14 * fTemp21) + (fTemp16 * fRec20[1]));
			fRec18[0] = ((fTemp14 * fTemp20) + (fTemp16 * fRec19[1]));
			fRec17[0] = ((fTemp14 * fTemp19) + (fTemp16 * fRec18[1]));
			fRec16[0] = ((fTemp14 * fTemp18) + (fTemp16 * fRec17[1]));
			fRec15[0] = ((fTemp14 * fTemp17) + (fTemp16 * fRec16[1]));
			float fTemp22 = ((fSlow11 * fRec1[0]) + (((float(iSlow21) * ((fSlow22 * (fTemp1 * fTemp4)) + (fRec9[1] * fTemp6))) + (float(iSlow30) * ((fRec5[0] * ((fSlow22 * (fTemp1 * fTemp14)) + (fRec15[1] * fTemp16))) + (fSlow22 * ((1.0f - fRec5[0]) * fTemp1))))) + (fSlow34 * fTemp0)));
			fVec2[(IOTA & 4095)] = fTemp22;
			fRec0[(IOTA & 4095)] = ((fSlow9 * fVec2[((IOTA - iSlow35) & 4095)]) + (fSlow36 * ((((fSlow37 * fVec2[((IOTA - iSlow38) & 4095)]) + (fSlow39 * fVec2[((IOTA - iSlow40) & 4095)])) + (fSlow42 * fVec2[((IOTA - iSlow43) & 4095)])) + (fSlow44 * fVec2[((IOTA - iSlow45) & 4095)]))));
			output0[i] = FAUSTFLOAT((fSlow1 * fRec0[((IOTA - 0) & 4095)]));
			output1[i] = FAUSTFLOAT((fSlow0 * fRec0[((IOTA - iSlow46) & 4095)]));
			iRec3[1] = iRec3[0];
			fVec0[1] = fVec0[0];
			fRec4[1] = fRec4[0];
			IOTA = (IOTA + 1);
			fRec1[1] = fRec1[0];
			fRec5[1] = fRec5[0];
			fRec8[1] = fRec8[0];
			fRec7[1] = fRec7[0];
			fRec14[1] = fRec14[0];
			fRec13[1] = fRec13[0];
			fRec12[1] = fRec12[0];
			fRec11[1] = fRec11[0];
			fRec10[1] = fRec10[0];
			fRec9[1] = fRec9[0];
			fVec1[1] = fVec1[0];
			fRec20[1] = fRec20[0];
			fRec19[1] = fRec19[0];
			fRec18[1] = fRec18[0];
			fRec17[1] = fRec17[0];
			fRec16[1] = fRec16[0];
			fRec15[1] = fRec15[0];
			
		}
		
	}

	
};



#include "Faust_plugins_template2.cpp"


#endif
