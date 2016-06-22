/* ------------------------------------------------------------
author: "Romain Michon"
copyright: "Romain Michon (rmichon@ccrma.stanford.edu)"
name: "NLFfm"
version: "1.0"
Code generated with Faust 2.0.a43 (http://faust.grame.fr)
------------------------------------------------------------ */

#ifndef  __NLF_Fm_dsp_H__
#define  __NLF_Fm_dsp_H__

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


class NLF_Fm_dspSIG0 {
	
  private:
	
	int iRec3[2];
	
  public:
	
	int getNumInputsNLF_Fm_dspSIG0() {
		return 0;
		
	}
	int getNumOutputsNLF_Fm_dspSIG0() {
		return 1;
		
	}
	int getInputRateNLF_Fm_dspSIG0(int channel) {
		int rate;
		switch (channel) {
			default: {
				rate = -1;
				break;
			}
			
		}
		return rate;
		
	}
	int getOutputRateNLF_Fm_dspSIG0(int channel) {
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
	
	void instanceInitNLF_Fm_dspSIG0(int samplingFreq) {
		for (int i3 = 0; (i3 < 2); i3 = (i3 + 1)) {
			iRec3[i3] = 0;
			
		}
		
	}
	
	void fillNLF_Fm_dspSIG0(int count, float* output) {
		for (int i = 0; (i < count); i = (i + 1)) {
			iRec3[0] = (1 + iRec3[1]);
			output[i] = sinf((9.58738e-05f * float((iRec3[0] - 1))));
			iRec3[1] = iRec3[0];
			
		}
		
	}
};

NLF_Fm_dspSIG0* newNLF_Fm_dspSIG0() { return (NLF_Fm_dspSIG0*)new NLF_Fm_dspSIG0(); }
void deleteNLF_Fm_dspSIG0(NLF_Fm_dspSIG0* dsp) { delete dsp; }

static float ftbl0NLF_Fm_dspSIG0[65536];
static float faustpower2_f(float value) {
	return (value * value);
	
}

#ifndef FAUSTCLASS 
#define FAUSTCLASS NLF_Fm_dsp
#endif

class NLF_Fm_dsp : public dsp {
	
  private:
	
	float fVec1[4096];
	int iRec0[2];
	float fRec1[2];
	float fRec2[2];
	float fRec4[2];
	int iRec5[2];
	int iRec6[2];
	float fRec7[2];
	float fRec8[2];
	float fVec0[2];
	float fRec9[2];
	int iRec10[2];
	float fRec11[2];
	float fRec13[2];
	float fRec12[2];
	float fRec16[2];
	float fRec15[2];
	float fRec14[2];
	float fRec19[2];
	float fRec18[2];
	float fRec17[2];
	FAUSTFLOAT fHslider0;
	FAUSTFLOAT fEntry0;
	FAUSTFLOAT fButton0;
	FAUSTFLOAT fHslider1;
	int fSamplingFreq;
	float fConst0;
	FAUSTFLOAT fHslider2;
	FAUSTFLOAT fHslider3;
	FAUSTFLOAT fEntry1;
	float fConst1;
	FAUSTFLOAT fEntry2;
	FAUSTFLOAT fHslider4;
	float fConst2;
	FAUSTFLOAT fHslider5;
	float fConst3;
	FAUSTFLOAT fHslider6;
	FAUSTFLOAT fHslider7;
	FAUSTFLOAT fHslider8;
	FAUSTFLOAT fHslider9;
	FAUSTFLOAT fHslider10;
	int IOTA;
	float fConst4;
	FAUSTFLOAT fHslider11;
	
  public:
	
	void static metadata(Meta* m) { 
		m->declare("author", "Romain Michon");
		m->declare("copyright", "Romain Michon (rmichon@ccrma.stanford.edu)");
		m->declare("description", "FM synthesizer implemented with a nonlinear passive allpass filter");
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
		m->declare("name", "NLFfm");
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
		NLF_Fm_dspSIG0* sig0 = newNLF_Fm_dspSIG0();
		sig0->instanceInitNLF_Fm_dspSIG0(samplingFreq);
		sig0->fillNLF_Fm_dspSIG0(65536, ftbl0NLF_Fm_dspSIG0);
		deleteNLF_Fm_dspSIG0(sig0);
		
	}
	
	virtual void instanceInit(int samplingFreq) {
		fSamplingFreq = samplingFreq;
		fHslider0 = FAUSTFLOAT(0.6f);
		fEntry0 = FAUSTFLOAT(0.0f);
		fButton0 = FAUSTFLOAT(0.0f);
		for (int i0 = 0; (i0 < 2); i0 = (i0 + 1)) {
			iRec0[i0] = 0;
			
		}
		fHslider1 = FAUSTFLOAT(0.05f);
		fConst0 = min(1.92e+05f, max(1.0f, float(fSamplingFreq)));
		fHslider2 = FAUSTFLOAT(0.05f);
		fHslider3 = FAUSTFLOAT(0.05f);
		for (int i1 = 0; (i1 < 2); i1 = (i1 + 1)) {
			fRec1[i1] = 0.0f;
			
		}
		fEntry1 = FAUSTFLOAT(0.8f);
		for (int i2 = 0; (i2 < 2); i2 = (i2 + 1)) {
			fRec2[i2] = 0.0f;
			
		}
		fConst1 = (1.0f / fConst0);
		fEntry2 = FAUSTFLOAT(4.4e+02f);
		for (int i4 = 0; (i4 < 2); i4 = (i4 + 1)) {
			fRec4[i4] = 0.0f;
			
		}
		fHslider4 = FAUSTFLOAT(0.1f);
		for (int i5 = 0; (i5 < 2); i5 = (i5 + 1)) {
			iRec5[i5] = 0;
			
		}
		for (int i6 = 0; (i6 < 2); i6 = (i6 + 1)) {
			iRec6[i6] = 0;
			
		}
		fConst2 = (1.8f * fConst0);
		fHslider5 = FAUSTFLOAT(0.5f);
		fConst3 = (0.2f * fConst0);
		fHslider6 = FAUSTFLOAT(0.01f);
		for (int i7 = 0; (i7 < 2); i7 = (i7 + 1)) {
			fRec7[i7] = 0.0f;
			
		}
		fHslider7 = FAUSTFLOAT(5.0f);
		for (int i8 = 0; (i8 < 2); i8 = (i8 + 1)) {
			fRec8[i8] = 0.0f;
			
		}
		for (int i9 = 0; (i9 < 2); i9 = (i9 + 1)) {
			fVec0[i9] = 0.0f;
			
		}
		fHslider8 = FAUSTFLOAT(0.0f);
		for (int i10 = 0; (i10 < 2); i10 = (i10 + 1)) {
			fRec9[i10] = 0.0f;
			
		}
		for (int i11 = 0; (i11 < 2); i11 = (i11 + 1)) {
			iRec10[i11] = 0;
			
		}
		fHslider9 = FAUSTFLOAT(0.1f);
		for (int i12 = 0; (i12 < 2); i12 = (i12 + 1)) {
			fRec11[i12] = 0.0f;
			
		}
		fHslider10 = FAUSTFLOAT(2.2e+02f);
		for (int i13 = 0; (i13 < 2); i13 = (i13 + 1)) {
			fRec13[i13] = 0.0f;
			
		}
		for (int i14 = 0; (i14 < 2); i14 = (i14 + 1)) {
			fRec12[i14] = 0.0f;
			
		}
		for (int i15 = 0; (i15 < 2); i15 = (i15 + 1)) {
			fRec16[i15] = 0.0f;
			
		}
		for (int i16 = 0; (i16 < 2); i16 = (i16 + 1)) {
			fRec15[i16] = 0.0f;
			
		}
		for (int i17 = 0; (i17 < 2); i17 = (i17 + 1)) {
			fRec14[i17] = 0.0f;
			
		}
		for (int i18 = 0; (i18 < 2); i18 = (i18 + 1)) {
			fRec19[i18] = 0.0f;
			
		}
		for (int i19 = 0; (i19 < 2); i19 = (i19 + 1)) {
			fRec18[i19] = 0.0f;
			
		}
		for (int i20 = 0; (i20 < 2); i20 = (i20 + 1)) {
			fRec17[i20] = 0.0f;
			
		}
		IOTA = 0;
		for (int i21 = 0; (i21 < 4096); i21 = (i21 + 1)) {
			fVec1[i21] = 0.0f;
			
		}
		fConst4 = (0.5f * fConst0);
		fHslider11 = FAUSTFLOAT(0.5f);
		
	}
	
	virtual void init(int samplingFreq) {
		classInit(samplingFreq);
		instanceInit(samplingFreq);
	}
	
	virtual void buildUserInterface(UI* interface) {
		interface->openVerticalBox("0x00");
		interface->openHorizontalBox("Basic_Parameters");
		interface->declare(&fEntry2, "1", "");
		interface->declare(&fEntry2, "tooltip", "Tone frequency");
		interface->declare(&fEntry2, "unit", "Hz");
		interface->addNumEntry("freq", &fEntry2, 4.4e+02f, 2e+01f, 2e+04f, 1.0f);
		interface->declare(&fEntry1, "1", "");
		interface->declare(&fEntry1, "tooltip", "Gain (value between 0 and 1)");
		interface->addNumEntry("gain", &fEntry1, 0.8f, 0.0f, 1.0f, 0.01f);
		interface->declare(&fButton0, "1", "");
		interface->declare(&fButton0, "tooltip", "noteOn = 1, noteOff = 0");
		interface->addButton("gate",&fButton0);
		interface->closeBox();
		interface->openHorizontalBox("Envelopes_and_Vibrato");
		interface->openVerticalBox("Envelope_Parameters");
		interface->declare(&fHslider1, "4", "");
		interface->declare(&fHslider1, "tooltip", "Envelope attack duration");
		interface->declare(&fHslider1, "unit", "s");
		interface->addHorizontalSlider("Envelope_Attack", &fHslider1, 0.05f, 0.0f, 2.0f, 0.01f);
		interface->declare(&fHslider2, "4", "");
		interface->declare(&fHslider2, "tooltip", "Envelope decay duration");
		interface->declare(&fHslider2, "unit", "s");
		interface->addHorizontalSlider("Envelope_Decay", &fHslider2, 0.05f, 0.0f, 2.0f, 0.01f);
		interface->declare(&fHslider3, "4", "");
		interface->declare(&fHslider3, "tooltip", "Envelope release duration");
		interface->declare(&fHslider3, "unit", "s");
		interface->addHorizontalSlider("Envelope_Release", &fHslider3, 0.05f, 0.0f, 2.0f, 0.01f);
		interface->closeBox();
		interface->openVerticalBox("Vibrato_Parameters");
		interface->declare(&fHslider5, "3", "");
		interface->declare(&fHslider5, "tooltip", "Vibrato attack duration");
		interface->declare(&fHslider5, "unit", "s");
		interface->addHorizontalSlider("Vibrato_Attack", &fHslider5, 0.5f, 0.0f, 2.0f, 0.01f);
		interface->declare(&fHslider7, "3", "");
		interface->declare(&fHslider7, "unit", "Hz");
		interface->addHorizontalSlider("Vibrato_Freq", &fHslider7, 5.0f, 1.0f, 15.0f, 0.1f);
		interface->declare(&fHslider4, "3", "");
		interface->declare(&fHslider4, "tooltip", "A value between 0 and 1");
		interface->addHorizontalSlider("Vibrato_Gain", &fHslider4, 0.1f, 0.0f, 1.0f, 0.01f);
		interface->declare(&fHslider6, "3", "");
		interface->declare(&fHslider6, "tooltip", "Vibrato release duration");
		interface->declare(&fHslider6, "unit", "s");
		interface->addHorizontalSlider("Vibrato_Release", &fHslider6, 0.01f, 0.0f, 2.0f, 0.01f);
		interface->closeBox();
		interface->closeBox();
		interface->openHorizontalBox("Physical_and_Nonlinearity");
		interface->openVerticalBox("Nonlinear_Filter_Parameters");
		interface->declare(&fHslider10, "2", "");
		interface->declare(&fHslider10, "tooltip", "Frequency of the sine wave for the modulation of theta (works if Modulation Type=3)");
		interface->declare(&fHslider10, "unit", "Hz");
		interface->addHorizontalSlider("Modulation_Frequency", &fHslider10, 2.2e+02f, 2e+01f, 1e+03f, 0.1f);
		interface->declare(&fEntry0, "2", "");
		interface->declare(&fEntry0, "tooltip", "0=theta is modulated by the incoming signal; 1=theta is modulated by the averaged incoming signal; 2=theta is modulated by the squared incoming signal; 3=theta is modulated by a sine wave of frequency freqMod; 4=theta is modulated by a sine wave of frequency freq;");
		interface->addNumEntry("Modulation_Type", &fEntry0, 0.0f, 0.0f, 4.0f, 1.0f);
		interface->declare(&fHslider8, "2", "");
		interface->declare(&fHslider8, "tooltip", "Nonlinearity factor (value between 0 and 1)");
		interface->addHorizontalSlider("Nonlinearity", &fHslider8, 0.0f, 0.0f, 1.0f, 0.01f);
		interface->declare(&fHslider9, "2", "");
		interface->declare(&fHslider9, "Attack duration of the nonlinearity", "");
		interface->declare(&fHslider9, "unit", "s");
		interface->addHorizontalSlider("Nonlinearity_Attack", &fHslider9, 0.1f, 0.0f, 2.0f, 0.01f);
		interface->closeBox();
		interface->closeBox();
		interface->openVerticalBox("Spat");
		interface->addHorizontalSlider("pan angle", &fHslider0, 0.6f, 0.0f, 1.0f, 0.01f);
		interface->addHorizontalSlider("spatial width", &fHslider11, 0.5f, 0.0f, 1.0f, 0.01f);
		interface->closeBox();
		interface->closeBox();
		
	}
	
	virtual void compute(int count, FAUSTFLOAT** inputs, FAUSTFLOAT** outputs) {
		FAUSTFLOAT* output0 = outputs[0];
		FAUSTFLOAT* output1 = outputs[1];
		float fSlow0 = float(fHslider0);
		float fSlow1 = (1.0f - fSlow0);
		float fSlow2 = float(fEntry0);
		int iSlow3 = (fSlow2 >= 3.0f);
		float fSlow4 = float(fButton0);
		int iSlow5 = (fSlow4 > 0.0f);
		float fSlow6 = float(fHslider1);
		float fSlow7 = (1.0f / (float((fSlow6 == 0.0f)) + (fConst0 * fSlow6)));
		float fSlow8 = float(fHslider2);
		float fSlow9 = (1.0f - powf(9e+01f, (1.0f / (float((fSlow8 == 0.0f)) + (fConst0 * fSlow8)))));
		float fSlow10 = float(fHslider3);
		float fSlow11 = (1.0f / (float((fSlow10 == 0.0f)) + (fConst0 * fSlow10)));
		float fSlow12 = (1.0f - (1.0f / powf(9e+04f, fSlow11)));
		int iSlow13 = (fSlow4 <= 0.0f);
		float fSlow14 = (0.001f * float(fEntry1));
		float fSlow15 = float(fEntry2);
		float fSlow16 = (fConst1 * fSlow15);
		float fSlow17 = float(fHslider4);
		float fSlow18 = float(fHslider5);
		float fSlow19 = (1.0f / ((fConst2 * fSlow18) + float(((1.8f * fSlow18) == 0.0f))));
		float fSlow20 = (fConst3 * fSlow18);
		float fSlow21 = (fSlow20 + float(((0.2f * fSlow18) == 0.0f)));
		float fSlow22 = float(fHslider6);
		float fSlow23 = (1.0f - (1.0f / powf(1e+05f, (1.0f / (float((fSlow22 == 0.0f)) + (fConst0 * fSlow22))))));
		float fSlow24 = (fConst1 * float(fHslider7));
		float fSlow25 = (0.001f * float(fHslider8));
		float fSlow26 = float(fHslider9);
		float fSlow27 = (1.0f / (float((fSlow26 == 0.0f)) + (fConst0 * fSlow26)));
		float fSlow28 = (1.0f - (1.0f / powf(1e+05f, fSlow11)));
		int iSlow29 = (fSlow2 != 4.0f);
		float fSlow30 = (0.001f * float(fHslider10));
		float fSlow31 = (fSlow15 * float((fSlow2 == 4.0f)));
		int iSlow32 = (fSlow2 < 3.0f);
		float fSlow33 = (3.1415927f * float((fSlow2 == 0.0f)));
		float fSlow34 = (1.5707964f * float((fSlow2 == 1.0f)));
		float fSlow35 = (3.1415927f * float((fSlow2 == 2.0f)));
		int iSlow36 = (int((fConst4 * (float(fHslider11) / fSlow15))) & 4095);
		for (int i = 0; (i < count); i = (i + 1)) {
			iRec0[0] = (iSlow5 & (iRec0[1] | (fRec1[1] >= 1.0f)));
			int iTemp0 = (iSlow13 & (fRec1[1] > 0.0f));
			fRec1[0] = (((fSlow7 * float((((iRec0[1] == 0) & iSlow5) & (fRec1[1] < 1.0f)))) + (fRec1[1] * ((1.0f - (fSlow9 * float((iRec0[1] & (fRec1[1] > 9e+01f))))) - (fSlow12 * float(iTemp0))))) * float(((iTemp0 == 0) | (fRec1[1] >= 1e-06f))));
			fRec2[0] = ((0.999f * fRec2[1]) + fSlow14);
			float fTemp1 = (fRec1[0] * fRec2[0]);
			float fTemp2 = (fRec4[1] + fSlow16);
			fRec4[0] = (fTemp2 - floorf(fTemp2));
			iRec5[0] = (iSlow5 & (iRec5[1] | (fRec7[1] >= 1.0f)));
			iRec6[0] = (iSlow5 * (1 + iRec6[1]));
			int iTemp3 = (iSlow13 & (fRec7[1] > 0.0f));
			fRec7[0] = (((fSlow19 * float((((((iRec5[1] == 0) & iSlow5) & (fRec7[1] < 1.0f)) & (float(iRec6[1]) > fSlow20)) * (1 - (float(iRec6[1]) < fSlow21))))) + (fRec7[1] * (1.0f - (fSlow23 * float(iTemp3))))) * float(((iTemp3 == 0) | (fRec7[1] >= 1e-06f))));
			float fTemp4 = (fRec8[1] + fSlow24);
			fRec8[0] = (fTemp4 - floorf(fTemp4));
			float fTemp5 = (1.0f + (fSlow17 * (fRec7[0] * ftbl0NLF_Fm_dspSIG0[int((65536.0f * fRec8[0]))])));
			float fTemp6 = ((fTemp1 * ftbl0NLF_Fm_dspSIG0[int((65536.0f * fRec4[0]))]) * fTemp5);
			fVec0[0] = fTemp6;
			fRec9[0] = ((0.999f * fRec9[1]) + fSlow25);
			iRec10[0] = (iSlow5 & (iRec10[1] | (fRec11[1] >= 1.0f)));
			int iTemp7 = (iSlow13 & (fRec11[1] > 0.0f));
			fRec11[0] = (((fSlow27 * float((((iRec10[1] == 0) & iSlow5) & (fRec11[1] < 1.0f)))) + (fRec11[1] * (1.0f - (fSlow28 * float(iTemp7))))) * float(((iTemp7 == 0) | (fRec11[1] >= 1e-06f))));
			float fTemp8 = (fRec9[0] * fRec11[0]);
			fRec13[0] = ((0.999f * fRec13[1]) + fSlow30);
			float fTemp9 = (fRec12[1] + (fConst1 * ((float(iSlow29) * fRec13[0]) + fSlow31)));
			fRec12[0] = (fTemp9 - floorf(fTemp9));
			float fTemp10 = (3.1415927f * (fTemp8 * ftbl0NLF_Fm_dspSIG0[int((65536.0f * fRec12[0]))]));
			float fTemp11 = sinf(fTemp10);
			float fTemp12 = (0.0f - fTemp11);
			float fTemp13 = cosf(fTemp10);
			float fTemp14 = ((fRec14[1] * fTemp12) + (fTemp6 * fTemp13));
			float fTemp15 = ((fTemp12 * fRec15[1]) + (fTemp13 * fTemp14));
			fRec16[0] = ((fTemp12 * fRec16[1]) + (fTemp13 * fTemp15));
			fRec15[0] = ((fTemp11 * fTemp15) + (fTemp13 * fRec16[1]));
			fRec14[0] = ((fTemp11 * fTemp14) + (fTemp13 * fRec15[1]));
			float fTemp16 = (fTemp8 * (((fSlow33 * fTemp6) + (fSlow34 * (fTemp6 + fVec0[1]))) + (fSlow35 * (((faustpower2_f(fRec1[0]) * faustpower2_f(fRec2[0])) * faustpower2_f(ftbl0NLF_Fm_dspSIG0[int((65536.0f * fRec4[0]))])) * faustpower2_f(fTemp5)))));
			float fTemp17 = sinf(fTemp16);
			float fTemp18 = (0.0f - fTemp17);
			float fTemp19 = cosf(fTemp16);
			float fTemp20 = ((fRec17[1] * fTemp18) + (fTemp6 * fTemp19));
			float fTemp21 = ((fTemp18 * fRec18[1]) + (fTemp19 * fTemp20));
			fRec19[0] = ((fTemp18 * fRec19[1]) + (fTemp19 * fTemp21));
			fRec18[0] = ((fTemp17 * fTemp21) + (fTemp19 * fRec19[1]));
			fRec17[0] = ((fTemp17 * fTemp20) + (fTemp19 * fRec18[1]));
			float fTemp22 = ((float(iSlow3) * ((fTemp6 * fTemp11) + (fRec14[1] * fTemp13))) + (float(iSlow32) * ((fRec9[0] * ((fTemp6 * fTemp17) + (fRec17[1] * fTemp19))) + (((fTemp1 * (1.0f - fRec9[0])) * ftbl0NLF_Fm_dspSIG0[int((65536.0f * fRec4[0]))]) * fTemp5))));
			fVec1[(IOTA & 4095)] = fTemp22;
			output0[i] = FAUSTFLOAT((fSlow1 * fTemp22));
			output1[i] = FAUSTFLOAT((fSlow0 * fVec1[((IOTA - iSlow36) & 4095)]));
			iRec0[1] = iRec0[0];
			fRec1[1] = fRec1[0];
			fRec2[1] = fRec2[0];
			fRec4[1] = fRec4[0];
			iRec5[1] = iRec5[0];
			iRec6[1] = iRec6[0];
			fRec7[1] = fRec7[0];
			fRec8[1] = fRec8[0];
			fVec0[1] = fVec0[0];
			fRec9[1] = fRec9[0];
			iRec10[1] = iRec10[0];
			fRec11[1] = fRec11[0];
			fRec13[1] = fRec13[0];
			fRec12[1] = fRec12[0];
			fRec16[1] = fRec16[0];
			fRec15[1] = fRec15[0];
			fRec14[1] = fRec14[0];
			fRec19[1] = fRec19[0];
			fRec18[1] = fRec18[0];
			fRec17[1] = fRec17[0];
			IOTA = (IOTA + 1);
			
		}
		
	}

	
};



#include "Faust_plugins_template2.cpp"


#endif
