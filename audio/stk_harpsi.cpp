/* ------------------------------------------------------------
author: "Romain Michon (rmichon@ccrma.stanford.edu)"
copyright: "Romain Michon"
name: "Harpsichord"
version: "1.0"
Code generated with Faust 2.0.a43 (http://faust.grame.fr)
------------------------------------------------------------ */

#ifndef  __Harpsi_dsp_H__
#define  __Harpsi_dsp_H__

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

/* link with : "" */
#include <harpsichord.h>
#include <math.h>


class Harpsi_dspSIG0 {
	
  private:
	
	int iRec4[2];
	
  public:
	
	int getNumInputsHarpsi_dspSIG0() {
		return 0;
		
	}
	int getNumOutputsHarpsi_dspSIG0() {
		return 1;
		
	}
	int getInputRateHarpsi_dspSIG0(int channel) {
		int rate;
		switch (channel) {
			default: {
				rate = -1;
				break;
			}
			
		}
		return rate;
		
	}
	int getOutputRateHarpsi_dspSIG0(int channel) {
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
	
	void instanceInitHarpsi_dspSIG0(int samplingFreq) {
		for (int i2 = 0; (i2 < 2); i2 = (i2 + 1)) {
			iRec4[i2] = 0;
			
		}
		
	}
	
	void fillHarpsi_dspSIG0(int count, float* output) {
		for (int i = 0; (i < count); i = (i + 1)) {
			iRec4[0] = (1 + iRec4[1]);
			output[i] = sinf((9.58738e-05f * float((iRec4[0] - 1))));
			iRec4[1] = iRec4[0];
			
		}
		
	}
};

Harpsi_dspSIG0* newHarpsi_dspSIG0() { return (Harpsi_dspSIG0*)new Harpsi_dspSIG0(); }
void deleteHarpsi_dspSIG0(Harpsi_dspSIG0* dsp) { delete dsp; }

static float ftbl0Harpsi_dspSIG0[65536];
static float faustpower2_f(float value) {
	return (value * value);
	
}

#ifndef FAUSTCLASS 
#define FAUSTCLASS Harpsi_dsp
#endif

class Harpsi_dsp : public dsp {
	
  private:
	
	float fVec0[4096];
	float fRec0[4096];
	float fRec1[3];
	float fRec2[2];
	float fRec3[2];
	float fRec6[2];
	float fRec5[2];
	float fRec12[2];
	float fRec11[2];
	float fRec10[2];
	float fRec9[2];
	float fRec8[2];
	float fRec7[2];
	float fRec18[2];
	float fRec17[2];
	float fRec16[2];
	float fRec15[2];
	float fRec14[2];
	float fRec13[2];
	float fRec20[2];
	float fRec19[2];
	int iRec21[2];
	FAUSTFLOAT fHslider0;
	FAUSTFLOAT fEntry0;
	FAUSTFLOAT fButton0;
	FAUSTFLOAT fEntry1;
	FAUSTFLOAT fHslider1;
	int fSamplingFreq;
	float fConst0;
	float fConst1;
	FAUSTFLOAT fHslider2;
	float fConst2;
	FAUSTFLOAT fEntry2;
	int IOTA;
	float fConst3;
	FAUSTFLOAT fHslider3;
	
  public:
	
	void static metadata(Meta* m) { 
		m->declare("author", "Romain Michon (rmichon@ccrma.stanford.edu)");
		m->declare("copyright", "Romain Michon");
		m->declare("description", "Nonlinear WaveGuide Commuted Harpsichord");
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
		m->declare("licence", "STK-4.3");
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
		m->declare("name", "Harpsichord");
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
		Harpsi_dspSIG0* sig0 = newHarpsi_dspSIG0();
		sig0->instanceInitHarpsi_dspSIG0(samplingFreq);
		sig0->fillHarpsi_dspSIG0(65536, ftbl0Harpsi_dspSIG0);
		deleteHarpsi_dspSIG0(sig0);
		
	}
	
	virtual void instanceInit(int samplingFreq) {
		fSamplingFreq = samplingFreq;
		fHslider0 = FAUSTFLOAT(0.6f);
		fEntry0 = FAUSTFLOAT(4.4e+02f);
		fButton0 = FAUSTFLOAT(0.0f);
		for (int i0 = 0; (i0 < 2); i0 = (i0 + 1)) {
			fRec2[i0] = 0.0f;
			
		}
		fEntry1 = FAUSTFLOAT(0.0f);
		fHslider1 = FAUSTFLOAT(0.0f);
		for (int i1 = 0; (i1 < 2); i1 = (i1 + 1)) {
			fRec3[i1] = 0.0f;
			
		}
		fConst0 = min(1.92e+05f, max(1.0f, float(fSamplingFreq)));
		fConst1 = (1.0f / fConst0);
		fHslider2 = FAUSTFLOAT(2.2e+02f);
		for (int i3 = 0; (i3 < 2); i3 = (i3 + 1)) {
			fRec6[i3] = 0.0f;
			
		}
		for (int i4 = 0; (i4 < 2); i4 = (i4 + 1)) {
			fRec5[i4] = 0.0f;
			
		}
		for (int i5 = 0; (i5 < 2); i5 = (i5 + 1)) {
			fRec12[i5] = 0.0f;
			
		}
		for (int i6 = 0; (i6 < 2); i6 = (i6 + 1)) {
			fRec11[i6] = 0.0f;
			
		}
		for (int i7 = 0; (i7 < 2); i7 = (i7 + 1)) {
			fRec10[i7] = 0.0f;
			
		}
		for (int i8 = 0; (i8 < 2); i8 = (i8 + 1)) {
			fRec9[i8] = 0.0f;
			
		}
		for (int i9 = 0; (i9 < 2); i9 = (i9 + 1)) {
			fRec8[i9] = 0.0f;
			
		}
		for (int i10 = 0; (i10 < 2); i10 = (i10 + 1)) {
			fRec7[i10] = 0.0f;
			
		}
		for (int i11 = 0; (i11 < 2); i11 = (i11 + 1)) {
			fRec18[i11] = 0.0f;
			
		}
		for (int i12 = 0; (i12 < 2); i12 = (i12 + 1)) {
			fRec17[i12] = 0.0f;
			
		}
		for (int i13 = 0; (i13 < 2); i13 = (i13 + 1)) {
			fRec16[i13] = 0.0f;
			
		}
		for (int i14 = 0; (i14 < 2); i14 = (i14 + 1)) {
			fRec15[i14] = 0.0f;
			
		}
		for (int i15 = 0; (i15 < 2); i15 = (i15 + 1)) {
			fRec14[i15] = 0.0f;
			
		}
		for (int i16 = 0; (i16 < 2); i16 = (i16 + 1)) {
			fRec13[i16] = 0.0f;
			
		}
		for (int i17 = 0; (i17 < 2); i17 = (i17 + 1)) {
			fRec20[i17] = 0.0f;
			
		}
		fConst2 = (7.0f / fConst0);
		fEntry2 = FAUSTFLOAT(0.8f);
		for (int i18 = 0; (i18 < 2); i18 = (i18 + 1)) {
			fRec19[i18] = 0.0f;
			
		}
		for (int i19 = 0; (i19 < 2); i19 = (i19 + 1)) {
			iRec21[i19] = 0;
			
		}
		IOTA = 0;
		for (int i20 = 0; (i20 < 4096); i20 = (i20 + 1)) {
			fVec0[i20] = 0.0f;
			
		}
		for (int i21 = 0; (i21 < 3); i21 = (i21 + 1)) {
			fRec1[i21] = 0.0f;
			
		}
		for (int i22 = 0; (i22 < 4096); i22 = (i22 + 1)) {
			fRec0[i22] = 0.0f;
			
		}
		fConst3 = (0.5f * fConst0);
		fHslider3 = FAUSTFLOAT(0.5f);
		
	}
	
	virtual void init(int samplingFreq) {
		classInit(samplingFreq);
		instanceInit(samplingFreq);
	}
	
	virtual void buildUserInterface(UI* interface) {
		interface->openVerticalBox("0x00");
		interface->openHorizontalBox("Basic_Parameters");
		interface->declare(&fEntry0, "1", "");
		interface->declare(&fEntry0, "tooltip", "Tone frequency");
		interface->declare(&fEntry0, "unit", "Hz");
		interface->addNumEntry("freq", &fEntry0, 4.4e+02f, 2e+01f, 2e+04f, 1.0f);
		interface->declare(&fEntry2, "1", "");
		interface->declare(&fEntry2, "tooltip", "Gain (value between 0 and 1)");
		interface->addNumEntry("gain", &fEntry2, 0.8f, 0.0f, 1.0f, 0.01f);
		interface->declare(&fButton0, "1", "");
		interface->declare(&fButton0, "tooltip", "noteOn = 1, noteOff = 0");
		interface->addButton("gate",&fButton0);
		interface->closeBox();
		interface->openHorizontalBox("Physical_and_Nonlinearity");
		interface->openVerticalBox("Nonlinear_Filter_Parameters");
		interface->declare(&fHslider2, "2", "");
		interface->declare(&fHslider2, "tooltip", "Frequency of the sine wave for the modulation of theta (works if Modulation Type=3)");
		interface->declare(&fHslider2, "unit", "Hz");
		interface->addHorizontalSlider("Modulation_Frequency", &fHslider2, 2.2e+02f, 2e+01f, 1e+03f, 0.1f);
		interface->declare(&fEntry1, "2", "");
		interface->declare(&fEntry1, "tooltip", "0=theta is modulated by the incoming signal; 1=theta is modulated by the averaged incoming signal; 2=theta is modulated by the squared incoming signal; 3=theta is modulated by a sine wave of frequency freqMod; 4=theta is modulated by a sine wave of frequency freq;");
		interface->addNumEntry("Modulation_Type", &fEntry1, 0.0f, 0.0f, 4.0f, 1.0f);
		interface->declare(&fHslider1, "2", "");
		interface->declare(&fHslider1, "tooltip", "Nonlinearity factor (value between 0 and 1)");
		interface->addHorizontalSlider("Nonlinearity", &fHslider1, 0.0f, 0.0f, 1.0f, 0.01f);
		interface->closeBox();
		interface->closeBox();
		interface->openVerticalBox("Spat");
		interface->addHorizontalSlider("pan angle", &fHslider0, 0.6f, 0.0f, 1.0f, 0.01f);
		interface->addHorizontalSlider("spatial width", &fHslider3, 0.5f, 0.0f, 1.0f, 0.01f);
		interface->closeBox();
		interface->closeBox();
		
	}
	
	virtual void compute(int count, FAUSTFLOAT** inputs, FAUSTFLOAT** outputs) {
		FAUSTFLOAT* output0 = outputs[0];
		FAUSTFLOAT* output1 = outputs[1];
		float fSlow0 = float(fHslider0);
		float fSlow1 = (1.0f - fSlow0);
		float fSlow2 = float(fEntry0);
		int iSlow3 = int(((17.31234f * (logf(fSlow2) - 6.086775f)) + 69.5f));
		float fSlow4 = float(getValueLoopFilterb0(float(iSlow3)));
		float fSlow5 = float(getValueLoopFiltera1(float(iSlow3)));
		float fSlow6 = float(getValueLoopFiltera2(float(iSlow3)));
		float fSlow7 = float(fButton0);
		int iSlow8 = (fSlow7 < 1.0f);
		float fSlow9 = (0.001f * ((0.9996f * fSlow7) + (0.9f * (float(getValueReleaseLoopGain(float(iSlow3))) * float(iSlow8)))));
		float fSlow10 = float(fEntry1);
		int iSlow11 = (fSlow10 >= 3.0f);
		float fSlow12 = (0.001f * float(fHslider1));
		int iSlow13 = (fSlow10 != 4.0f);
		float fSlow14 = (0.001f * float(fHslider2));
		float fSlow15 = (fSlow2 * float((fSlow10 == 4.0f)));
		int iSlow16 = (fSlow10 < 3.0f);
		float fSlow17 = (3.1415927f * float((fSlow10 == 0.0f)));
		float fSlow18 = (1.5707964f * float((fSlow10 == 1.0f)));
		float fSlow19 = (3.1415927f * float((fSlow10 == 2.0f)));
		int iSlow20 = (fSlow7 > 0.0f);
		float fSlow21 = expf((0.0f - (fConst2 / (float(fEntry2) * float(getValueDryTapAmpT60(float(iSlow3)))))));
		int iSlow22 = (int((fConst0 / fSlow2)) & 4095);
		float fSlow23 = float(getValueLoopFilterb1(float(iSlow3)));
		float fSlow24 = float(getValueLoopFilterb2(float(iSlow3)));
		int iSlow25 = (int((fConst3 * (float(fHslider3) / fSlow2))) & 4095);
		for (int i = 0; (i < count); i = (i + 1)) {
			fRec2[0] = ((0.999f * fRec2[1]) + fSlow9);
			fRec3[0] = ((0.999f * fRec3[1]) + fSlow12);
			fRec6[0] = ((0.999f * fRec6[1]) + fSlow14);
			float fTemp0 = (fRec5[1] + (fConst1 * ((float(iSlow13) * fRec6[0]) + fSlow15)));
			fRec5[0] = (fTemp0 - floorf(fTemp0));
			float fTemp1 = (3.1415927f * (fRec3[0] * ftbl0Harpsi_dspSIG0[int((65536.0f * fRec5[0]))]));
			float fTemp2 = sinf(fTemp1);
			float fTemp3 = (0.0f - fTemp2);
			float fTemp4 = cosf(fTemp1);
			float fTemp5 = ((fRec7[1] * fTemp3) + (fRec0[((IOTA - 1) & 4095)] * fTemp4));
			float fTemp6 = ((fTemp3 * fRec8[1]) + (fTemp4 * fTemp5));
			float fTemp7 = ((fTemp3 * fRec9[1]) + (fTemp4 * fTemp6));
			float fTemp8 = ((fTemp3 * fRec10[1]) + (fTemp4 * fTemp7));
			float fTemp9 = ((fTemp3 * fRec11[1]) + (fTemp4 * fTemp8));
			fRec12[0] = ((fTemp3 * fRec12[1]) + (fTemp4 * fTemp9));
			fRec11[0] = ((fTemp2 * fTemp9) + (fTemp4 * fRec12[1]));
			fRec10[0] = ((fTemp2 * fTemp8) + (fTemp4 * fRec11[1]));
			fRec9[0] = ((fTemp2 * fTemp7) + (fTemp4 * fRec10[1]));
			fRec8[0] = ((fTemp2 * fTemp6) + (fTemp4 * fRec9[1]));
			fRec7[0] = ((fTemp2 * fTemp5) + (fTemp4 * fRec8[1]));
			float fTemp10 = (fRec3[0] * (((fSlow17 * fRec0[((IOTA - 1) & 4095)]) + (fSlow18 * (fRec0[((IOTA - 1) & 4095)] + fRec0[((IOTA - 2) & 4095)]))) + (fSlow19 * faustpower2_f(fRec0[((IOTA - 1) & 4095)]))));
			float fTemp11 = sinf(fTemp10);
			float fTemp12 = (0.0f - fTemp11);
			float fTemp13 = cosf(fTemp10);
			float fTemp14 = ((fRec13[1] * fTemp12) + (fRec0[((IOTA - 1) & 4095)] * fTemp13));
			float fTemp15 = ((fTemp12 * fRec14[1]) + (fTemp13 * fTemp14));
			float fTemp16 = ((fTemp12 * fRec15[1]) + (fTemp13 * fTemp15));
			float fTemp17 = ((fTemp12 * fRec16[1]) + (fTemp13 * fTemp16));
			float fTemp18 = ((fTemp12 * fRec17[1]) + (fTemp13 * fTemp17));
			fRec18[0] = ((fTemp12 * fRec18[1]) + (fTemp13 * fTemp18));
			fRec17[0] = ((fTemp11 * fTemp18) + (fTemp13 * fRec18[1]));
			fRec16[0] = ((fTemp11 * fTemp17) + (fTemp13 * fRec17[1]));
			fRec15[0] = ((fTemp11 * fTemp16) + (fTemp13 * fRec16[1]));
			fRec14[0] = ((fTemp11 * fTemp15) + (fTemp13 * fRec15[1]));
			fRec13[0] = ((fTemp11 * fTemp14) + (fTemp13 * fRec14[1]));
			fRec20[0] = (1.0f + (fSlow7 * fRec20[1]));
			float fTemp19 = (fRec20[0] - 1.0f);
			int iTemp20 = ((fTemp19 < 2.0f) & iSlow20);
			float fTemp21 = ((0.030197384f * float(iTemp20)) + (fSlow21 * float(((fTemp19 >= 2.0f) | iSlow8))));
			fRec19[0] = ((fRec19[1] * fTemp21) + (0.15f * (float(iTemp20) * (1.0f - fTemp21))));
			iRec21[0] = (12345 + (1103515245 * iRec21[1]));
			fVec0[(IOTA & 4095)] = ((fRec2[0] * ((float(iSlow11) * ((fRec0[((IOTA - 1) & 4095)] * fTemp2) + (fRec7[1] * fTemp4))) + (float(iSlow16) * ((fRec3[0] * ((fRec0[((IOTA - 1) & 4095)] * fTemp11) + (fRec13[1] * fTemp13))) + ((1.0f - fRec3[0]) * fRec0[((IOTA - 1) & 4095)]))))) + (4.656613e-10f * (fRec19[0] * float(iRec21[0]))));
			fRec1[0] = (0.0f - (((fSlow5 * fRec1[1]) + (fSlow6 * fRec1[2])) - fVec0[((IOTA - iSlow22) & 4095)]));
			fRec0[(IOTA & 4095)] = (((fSlow4 * fRec1[0]) + (fSlow23 * fRec1[1])) + (fSlow24 * fRec1[2]));
			output0[i] = FAUSTFLOAT((fSlow1 * fRec0[((IOTA - 0) & 4095)]));
			output1[i] = FAUSTFLOAT((fSlow0 * fRec0[((IOTA - iSlow25) & 4095)]));
			fRec2[1] = fRec2[0];
			fRec3[1] = fRec3[0];
			fRec6[1] = fRec6[0];
			fRec5[1] = fRec5[0];
			fRec12[1] = fRec12[0];
			fRec11[1] = fRec11[0];
			fRec10[1] = fRec10[0];
			fRec9[1] = fRec9[0];
			fRec8[1] = fRec8[0];
			fRec7[1] = fRec7[0];
			fRec18[1] = fRec18[0];
			fRec17[1] = fRec17[0];
			fRec16[1] = fRec16[0];
			fRec15[1] = fRec15[0];
			fRec14[1] = fRec14[0];
			fRec13[1] = fRec13[0];
			fRec20[1] = fRec20[0];
			fRec19[1] = fRec19[0];
			iRec21[1] = iRec21[0];
			IOTA = (IOTA + 1);
			fRec1[2] = fRec1[1];
			fRec1[1] = fRec1[0];
			
		}
		
	}

	
};



#include "Faust_plugins_template2.cpp"


#endif
