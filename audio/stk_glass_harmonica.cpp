/* ------------------------------------------------------------
author: "Romain Michon"
copyright: "Romain Michon (rmichon@ccrma.stanford.edu)"
name: "Glass Harmonica"
version: "1.0"
Code generated with Faust 2.0.a43 (http://faust.grame.fr)
------------------------------------------------------------ */

#ifndef  __Glass_Harmonica_dsp_H__
#define  __Glass_Harmonica_dsp_H__

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


class Glass_Harmonica_dspSIG0 {
	
  private:
	
	int iRec23[2];
	
  public:
	
	int getNumInputsGlass_Harmonica_dspSIG0() {
		return 0;
		
	}
	int getNumOutputsGlass_Harmonica_dspSIG0() {
		return 1;
		
	}
	int getInputRateGlass_Harmonica_dspSIG0(int channel) {
		int rate;
		switch (channel) {
			default: {
				rate = -1;
				break;
			}
			
		}
		return rate;
		
	}
	int getOutputRateGlass_Harmonica_dspSIG0(int channel) {
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
	
	void instanceInitGlass_Harmonica_dspSIG0(int samplingFreq) {
		for (int i28 = 0; (i28 < 2); i28 = (i28 + 1)) {
			iRec23[i28] = 0;
			
		}
		
	}
	
	void fillGlass_Harmonica_dspSIG0(int count, float* output) {
		for (int i = 0; (i < count); i = (i + 1)) {
			iRec23[0] = (1 + iRec23[1]);
			output[i] = sinf((9.58738e-05f * float((iRec23[0] - 1))));
			iRec23[1] = iRec23[0];
			
		}
		
	}
};

Glass_Harmonica_dspSIG0* newGlass_Harmonica_dspSIG0() { return (Glass_Harmonica_dspSIG0*)new Glass_Harmonica_dspSIG0(); }
void deleteGlass_Harmonica_dspSIG0(Glass_Harmonica_dspSIG0* dsp) { delete dsp; }

static float faustpower2_f(float value) {
	return (value * value);
	
}
static float faustpower4_f(float value) {
	return (((value * value) * value) * value);
	
}
static float ftbl0Glass_Harmonica_dspSIG0[65536];

#ifndef FAUSTCLASS 
#define FAUSTCLASS Glass_Harmonica_dsp
#endif

class Glass_Harmonica_dsp : public dsp {
	
  private:
	
	float fVec0[4096];
	float fVec1[4096];
	float fVec2[4096];
	float fVec6[4096];
	float fVec3[2048];
	float fVec4[1024];
	float fRec9[3];
	float fRec13[3];
	float fRec15[3];
	float fRec17[3];
	float fRec19[3];
	float fRec21[3];
	int iRec0[2];
	float fRec1[2];
	int iRec10[2];
	float fRec11[2];
	float fRec8[2];
	float fRec2[2];
	float fRec12[2];
	float fRec3[2];
	float fRec14[2];
	float fRec4[2];
	float fRec16[2];
	float fRec5[2];
	float fRec18[2];
	float fRec6[2];
	float fRec7[2];
	float fVec5[2];
	float fRec22[2];
	float fRec25[2];
	float fRec24[2];
	float fRec31[2];
	float fRec30[2];
	float fRec29[2];
	float fRec28[2];
	float fRec27[2];
	float fRec26[2];
	float fRec37[2];
	float fRec36[2];
	float fRec35[2];
	float fRec34[2];
	float fRec33[2];
	float fRec32[2];
	FAUSTFLOAT fHslider0;
	FAUSTFLOAT fButton0;
	FAUSTFLOAT fHslider1;
	int fSamplingFreq;
	float fConst0;
	FAUSTFLOAT fHslider2;
	FAUSTFLOAT fEntry0;
	float fConst1;
	float fConst2;
	float fConst3;
	float fConst4;
	float fConst5;
	float fConst6;
	FAUSTFLOAT fEntry1;
	FAUSTFLOAT fEntry2;
	FAUSTFLOAT fHslider3;
	FAUSTFLOAT fHslider4;
	FAUSTFLOAT fEntry3;
	float fConst7;
	float fConst8;
	float fConst9;
	FAUSTFLOAT fHslider5;
	int IOTA;
	float fConst10;
	float fConst11;
	float fConst12;
	float fConst13;
	float fConst14;
	float fConst15;
	float fConst16;
	float fConst17;
	float fConst18;
	FAUSTFLOAT fHslider6;
	float fConst19;
	FAUSTFLOAT fHslider7;
	float fConst20;
	FAUSTFLOAT fHslider8;
	
  public:
	
	void static metadata(Meta* m) { 
		m->declare("author", "Romain Michon");
		m->declare("copyright", "Romain Michon (rmichon@ccrma.stanford.edu)");
		m->declare("description", "Nonlinear Banded Waveguide Modeled Glass Harmonica");
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
		m->declare("name", "Glass Harmonica");
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
		Glass_Harmonica_dspSIG0* sig0 = newGlass_Harmonica_dspSIG0();
		sig0->instanceInitGlass_Harmonica_dspSIG0(samplingFreq);
		sig0->fillGlass_Harmonica_dspSIG0(65536, ftbl0Glass_Harmonica_dspSIG0);
		deleteGlass_Harmonica_dspSIG0(sig0);
		
	}
	
	virtual void instanceInit(int samplingFreq) {
		fSamplingFreq = samplingFreq;
		fHslider0 = FAUSTFLOAT(0.6f);
		fButton0 = FAUSTFLOAT(0.0f);
		for (int i0 = 0; (i0 < 2); i0 = (i0 + 1)) {
			iRec0[i0] = 0;
			
		}
		fHslider1 = FAUSTFLOAT(0.02f);
		fConst0 = min(1.92e+05f, max(1.0f, float(fSamplingFreq)));
		fHslider2 = FAUSTFLOAT(0.1f);
		for (int i1 = 0; (i1 < 2); i1 = (i1 + 1)) {
			fRec1[i1] = 0.0f;
			
		}
		fEntry0 = FAUSTFLOAT(0.0f);
		fConst1 = (1.0f - (100.53097f / fConst0));
		fConst2 = faustpower2_f(fConst1);
		fConst3 = (0.5f * fConst2);
		fConst4 = (0.5f - fConst3);
		fConst5 = (0.0f - (2.0f * fConst1));
		fConst6 = (6.2831855f / fConst0);
		fEntry1 = FAUSTFLOAT(4.4e+02f);
		fEntry2 = FAUSTFLOAT(0.0f);
		fHslider3 = FAUSTFLOAT(1.0f);
		fHslider4 = FAUSTFLOAT(0.0f);
		fEntry3 = FAUSTFLOAT(0.8f);
		for (int i2 = 0; (i2 < 2); i2 = (i2 + 1)) {
			iRec10[i2] = 0;
			
		}
		fConst7 = (5e+01f / fConst0);
		fConst8 = (1.0f - powf(9e+01f, (2e+02f / fConst0)));
		fConst9 = (1.0f - (1.0f / powf(9e+04f, (1e+02f / fConst0))));
		for (int i3 = 0; (i3 < 2); i3 = (i3 + 1)) {
			fRec11[i3] = 0.0f;
			
		}
		fHslider5 = FAUSTFLOAT(0.2f);
		IOTA = 0;
		for (int i4 = 0; (i4 < 4096); i4 = (i4 + 1)) {
			fVec0[i4] = 0.0f;
			
		}
		for (int i5 = 0; (i5 < 3); i5 = (i5 + 1)) {
			fRec9[i5] = 0.0f;
			
		}
		fConst10 = (fConst3 - 0.5f);
		for (int i6 = 0; (i6 < 2); i6 = (i6 + 1)) {
			fRec8[i6] = 0.0f;
			
		}
		for (int i7 = 0; (i7 < 2); i7 = (i7 + 1)) {
			fRec2[i7] = 0.0f;
			
		}
		fConst11 = (14.57699f / fConst0);
		for (int i8 = 0; (i8 < 4096); i8 = (i8 + 1)) {
			fVec1[i8] = 0.0f;
			
		}
		fConst12 = (0.43103448f * fConst0);
		for (int i9 = 0; (i9 < 3); i9 = (i9 + 1)) {
			fRec13[i9] = 0.0f;
			
		}
		for (int i10 = 0; (i10 < 2); i10 = (i10 + 1)) {
			fRec12[i10] = 0.0f;
			
		}
		for (int i11 = 0; (i11 < 2); i11 = (i11 + 1)) {
			fRec3[i11] = 0.0f;
			
		}
		fConst13 = (26.703537f / fConst0);
		for (int i12 = 0; (i12 < 4096); i12 = (i12 + 1)) {
			fVec2[i12] = 0.0f;
			
		}
		fConst14 = (0.23529412f * fConst0);
		for (int i13 = 0; (i13 < 3); i13 = (i13 + 1)) {
			fRec15[i13] = 0.0f;
			
		}
		for (int i14 = 0; (i14 < 2); i14 = (i14 + 1)) {
			fRec14[i14] = 0.0f;
			
		}
		for (int i15 = 0; (i15 < 2); i15 = (i15 + 1)) {
			fRec4[i15] = 0.0f;
			
		}
		fConst15 = (41.65752f / fConst0);
		for (int i16 = 0; (i16 < 2048); i16 = (i16 + 1)) {
			fVec3[i16] = 0.0f;
			
		}
		fConst16 = (0.15082957f * fConst0);
		for (int i17 = 0; (i17 < 3); i17 = (i17 + 1)) {
			fRec17[i17] = 0.0f;
			
		}
		for (int i18 = 0; (i18 < 2); i18 = (i18 + 1)) {
			fRec16[i18] = 0.0f;
			
		}
		for (int i19 = 0; (i19 < 2); i19 = (i19 + 1)) {
			fRec5[i19] = 0.0f;
			
		}
		fConst17 = (58.93628f / fConst0);
		for (int i20 = 0; (i20 < 1024); i20 = (i20 + 1)) {
			fVec4[i20] = 0.0f;
			
		}
		fConst18 = (0.10660981f * fConst0);
		for (int i21 = 0; (i21 < 3); i21 = (i21 + 1)) {
			fRec19[i21] = 0.0f;
			
		}
		for (int i22 = 0; (i22 < 2); i22 = (i22 + 1)) {
			fRec18[i22] = 0.0f;
			
		}
		for (int i23 = 0; (i23 < 2); i23 = (i23 + 1)) {
			fRec6[i23] = 0.0f;
			
		}
		for (int i24 = 0; (i24 < 3); i24 = (i24 + 1)) {
			fRec21[i24] = 0.0f;
			
		}
		for (int i25 = 0; (i25 < 2); i25 = (i25 + 1)) {
			fRec7[i25] = 0.0f;
			
		}
		for (int i26 = 0; (i26 < 2); i26 = (i26 + 1)) {
			fVec5[i26] = 0.0f;
			
		}
		fHslider6 = FAUSTFLOAT(0.0f);
		for (int i27 = 0; (i27 < 2); i27 = (i27 + 1)) {
			fRec22[i27] = 0.0f;
			
		}
		fConst19 = (1.0f / fConst0);
		fHslider7 = FAUSTFLOAT(2.2e+02f);
		for (int i29 = 0; (i29 < 2); i29 = (i29 + 1)) {
			fRec25[i29] = 0.0f;
			
		}
		for (int i30 = 0; (i30 < 2); i30 = (i30 + 1)) {
			fRec24[i30] = 0.0f;
			
		}
		for (int i31 = 0; (i31 < 2); i31 = (i31 + 1)) {
			fRec31[i31] = 0.0f;
			
		}
		for (int i32 = 0; (i32 < 2); i32 = (i32 + 1)) {
			fRec30[i32] = 0.0f;
			
		}
		for (int i33 = 0; (i33 < 2); i33 = (i33 + 1)) {
			fRec29[i33] = 0.0f;
			
		}
		for (int i34 = 0; (i34 < 2); i34 = (i34 + 1)) {
			fRec28[i34] = 0.0f;
			
		}
		for (int i35 = 0; (i35 < 2); i35 = (i35 + 1)) {
			fRec27[i35] = 0.0f;
			
		}
		for (int i36 = 0; (i36 < 2); i36 = (i36 + 1)) {
			fRec26[i36] = 0.0f;
			
		}
		for (int i37 = 0; (i37 < 2); i37 = (i37 + 1)) {
			fRec37[i37] = 0.0f;
			
		}
		for (int i38 = 0; (i38 < 2); i38 = (i38 + 1)) {
			fRec36[i38] = 0.0f;
			
		}
		for (int i39 = 0; (i39 < 2); i39 = (i39 + 1)) {
			fRec35[i39] = 0.0f;
			
		}
		for (int i40 = 0; (i40 < 2); i40 = (i40 + 1)) {
			fRec34[i40] = 0.0f;
			
		}
		for (int i41 = 0; (i41 < 2); i41 = (i41 + 1)) {
			fRec33[i41] = 0.0f;
			
		}
		for (int i42 = 0; (i42 < 2); i42 = (i42 + 1)) {
			fRec32[i42] = 0.0f;
			
		}
		for (int i43 = 0; (i43 < 4096); i43 = (i43 + 1)) {
			fVec6[i43] = 0.0f;
			
		}
		fConst20 = (0.5f * fConst0);
		fHslider8 = FAUSTFLOAT(0.5f);
		
	}
	
	virtual void init(int samplingFreq) {
		classInit(samplingFreq);
		instanceInit(samplingFreq);
	}
	
	virtual void buildUserInterface(UI* interface) {
		interface->openVerticalBox("0x00");
		interface->openHorizontalBox("Basic_Parameters");
		interface->declare(&fEntry1, "1", "");
		interface->declare(&fEntry1, "tooltip", "Tone frequency");
		interface->declare(&fEntry1, "unit", "Hz");
		interface->addNumEntry("freq", &fEntry1, 4.4e+02f, 2e+01f, 2e+04f, 1.0f);
		interface->declare(&fEntry3, "1", "");
		interface->declare(&fEntry3, "tooltip", "Gain (value between 0 and 1)");
		interface->addNumEntry("gain", &fEntry3, 0.8f, 0.0f, 1.0f, 0.01f);
		interface->declare(&fButton0, "1", "");
		interface->declare(&fButton0, "tooltip", "noteOn = 1, noteOff = 0");
		interface->addButton("gate",&fButton0);
		interface->closeBox();
		interface->openHorizontalBox("Envelopes_and_Vibrato");
		interface->openVerticalBox("Global_Envelope_Parameters");
		interface->declare(&fHslider1, "4", "");
		interface->declare(&fHslider1, "tooltip", "Global envelope attack duration");
		interface->declare(&fHslider1, "unit", "s");
		interface->addHorizontalSlider("Glob_Env_Attack", &fHslider1, 0.02f, 0.0f, 2.0f, 0.01f);
		interface->declare(&fHslider2, "4", "");
		interface->declare(&fHslider2, "tooltip", "Global envelope release duration");
		interface->declare(&fHslider2, "unit", "s");
		interface->addHorizontalSlider("Glob_Env_Release", &fHslider2, 0.1f, 0.0f, 2.0f, 0.01f);
		interface->closeBox();
		interface->closeBox();
		interface->openHorizontalBox("Physical_and_Nonlinearity");
		interface->openVerticalBox("Nonlinear_Filter_Parameters");
		interface->declare(&fHslider7, "3", "");
		interface->declare(&fHslider7, "tooltip", "Frequency of the sine wave for the modulation of theta (works if Modulation Type=3)");
		interface->declare(&fHslider7, "unit", "Hz");
		interface->addHorizontalSlider("Modulation_Frequency", &fHslider7, 2.2e+02f, 2e+01f, 1e+03f, 0.1f);
		interface->declare(&fEntry0, "3", "");
		interface->declare(&fEntry0, "tooltip", "0=theta is modulated by the incoming signal; 1=theta is modulated by the averaged incoming signal; 2=theta is modulated by the squared incoming signal; 3=theta is modulated by a sine wave of frequency freqMod; 4=theta is modulated by a sine wave of frequency freq;");
		interface->addNumEntry("Modulation_Type", &fEntry0, 0.0f, 0.0f, 4.0f, 1.0f);
		interface->declare(&fHslider6, "3", "");
		interface->declare(&fHslider6, "tooltip", "Nonlinearity factor (value between 0 and 1)");
		interface->addHorizontalSlider("Nonlinearity", &fHslider6, 0.0f, 0.0f, 1.0f, 0.01f);
		interface->closeBox();
		interface->openVerticalBox("Physical_Parameters");
		interface->declare(&fHslider3, "2", "");
		interface->declare(&fHslider3, "tooltip", "A value between 0 and 1");
		interface->addHorizontalSlider("Base_Gain", &fHslider3, 1.0f, 0.0f, 1.0f, 0.01f);
		interface->declare(&fHslider5, "2", "");
		interface->declare(&fHslider5, "tooltip", "Bow pressure on the instrument (Value between 0 and 1)");
		interface->addHorizontalSlider("Bow_Pressure", &fHslider5, 0.2f, 0.0f, 1.0f, 0.01f);
		interface->declare(&fEntry2, "2", "");
		interface->declare(&fEntry2, "tooltip", "0=Bow; 1=Strike");
		interface->addNumEntry("Excitation_Selector", &fEntry2, 0.0f, 0.0f, 1.0f, 1.0f);
		interface->declare(&fHslider4, "2", "");
		interface->declare(&fHslider4, "tooltip", "A value between 0 and 1");
		interface->addHorizontalSlider("Integration_Constant", &fHslider4, 0.0f, 0.0f, 1.0f, 0.01f);
		interface->closeBox();
		interface->closeBox();
		interface->openVerticalBox("Spat");
		interface->addHorizontalSlider("pan angle", &fHslider0, 0.6f, 0.0f, 1.0f, 0.01f);
		interface->addHorizontalSlider("spatial width", &fHslider8, 0.5f, 0.0f, 1.0f, 0.01f);
		interface->closeBox();
		interface->closeBox();
		
	}
	
	virtual void compute(int count, FAUSTFLOAT** inputs, FAUSTFLOAT** outputs) {
		FAUSTFLOAT* output0 = outputs[0];
		FAUSTFLOAT* output1 = outputs[1];
		float fSlow0 = float(fHslider0);
		float fSlow1 = (0.5f * (1.0f - fSlow0));
		float fSlow2 = float(fButton0);
		int iSlow3 = (fSlow2 > 0.0f);
		float fSlow4 = float(fHslider1);
		float fSlow5 = (1.0f / (float((fSlow4 == 0.0f)) + (fConst0 * fSlow4)));
		float fSlow6 = float(fHslider2);
		float fSlow7 = (1.0f - (1.0f / powf(1e+05f, (1.0f / (float((fSlow6 == 0.0f)) + (fConst0 * fSlow6))))));
		int iSlow8 = (fSlow2 <= 0.0f);
		float fSlow9 = float(fEntry0);
		int iSlow10 = (fSlow9 >= 3.0f);
		float fSlow11 = float(fEntry1);
		float fSlow12 = (fConst5 * cosf((fConst6 * fSlow11)));
		float fSlow13 = float(fEntry2);
		float fSlow14 = (0.16666667f * (0.0f - (fSlow13 - 1.0f)));
		float fSlow15 = (0.9f + (0.1f * float(fHslider3)));
		float fSlow16 = float(fHslider4);
		float fSlow17 = float(fEntry3);
		float fSlow18 = (0.03f + (0.1f * fSlow17));
		float fSlow19 = (1e+01f - (9.0f * float(fHslider5)));
		float fSlow20 = (0.2f * ((fSlow2 * fSlow13) * fSlow17));
		int iSlow21 = (int((fConst0 / fSlow11)) & 4095);
		float fSlow22 = (fConst5 * cosf((fConst11 * fSlow11)));
		int iSlow23 = (int((fConst12 / fSlow11)) & 4095);
		float fSlow24 = (fConst5 * cosf((fConst13 * fSlow11)));
		int iSlow25 = (int((fConst14 / fSlow11)) & 4095);
		float fSlow26 = (fConst5 * cosf((fConst15 * fSlow11)));
		int iSlow27 = (int((fConst16 / fSlow11)) & 4095);
		float fSlow28 = (fConst5 * cosf((fConst17 * fSlow11)));
		int iSlow29 = (int((fConst18 / fSlow11)) & 4095);
		float fSlow30 = (0.001f * float(fHslider6));
		int iSlow31 = (fSlow9 != 4.0f);
		float fSlow32 = (0.001f * float(fHslider7));
		float fSlow33 = (fSlow11 * float((fSlow9 == 4.0f)));
		int iSlow34 = (fSlow9 < 3.0f);
		float fSlow35 = (12.566371f * float((fSlow9 == 0.0f)));
		float fSlow36 = (6.2831855f * float((fSlow9 == 1.0f)));
		float fSlow37 = (50.265484f * float((fSlow9 == 2.0f)));
		float fSlow38 = (0.5f * fSlow0);
		int iSlow39 = (int((fConst20 * (float(fHslider8) / fSlow11))) & 4095);
		for (int i = 0; (i < count); i = (i + 1)) {
			iRec0[0] = (iSlow3 & (iRec0[1] | (fRec1[1] >= 1.0f)));
			int iTemp0 = (iSlow8 & (fRec1[1] > 0.0f));
			fRec1[0] = (((fSlow5 * float((((iRec0[1] == 0) & iSlow3) & (fRec1[1] < 1.0f)))) + (fRec1[1] * (1.0f - (fSlow7 * float(iTemp0))))) * float(((iTemp0 == 0) | (fRec1[1] >= 1e-06f))));
			iRec10[0] = (iSlow3 & (iRec10[1] | (fRec11[1] >= 1.0f)));
			int iTemp1 = (iSlow8 & (fRec11[1] > 0.0f));
			fRec11[0] = (((fConst7 * float((((iRec10[1] == 0) & iSlow3) & (fRec11[1] < 1.0f)))) + (fRec11[1] * ((1.0f - (fConst8 * float((iRec10[1] & (fRec11[1] > 9e+01f))))) - (fConst9 * float(iTemp1))))) * float(((iTemp1 == 0) | (fRec11[1] >= 1e-06f))));
			float fTemp2 = (0.0f - (((fSlow15 * (((fRec2[1] + fRec4[1]) + fRec6[1]) + ((fRec3[1] + fRec5[1]) + fRec7[1]))) + fSlow16) - (fSlow18 * fRec11[0])));
			float fTemp3 = faustpower4_f((0.75f + fabsf((fSlow19 * fTemp2))));
			float fTemp4 = (1.0f / fTemp3);
			float fTemp5 = (fSlow14 * (fTemp2 * (float((fTemp4 > 1.0f)) + (float((fTemp4 <= 1.0f)) / fTemp3))));
			fVec0[(IOTA & 4095)] = ((fRec8[1] + fTemp5) + fSlow20);
			fRec9[0] = (0.0f - (((fSlow12 * fRec9[1]) + (fConst2 * fRec9[2])) - (0.999f * fVec0[((IOTA - iSlow21) & 4095)])));
			fRec8[0] = ((fConst4 * fRec9[0]) + (fConst10 * fRec9[2]));
			fRec2[0] = fRec8[0];
			fVec1[(IOTA & 4095)] = (fSlow20 + (fTemp5 + fRec12[1]));
			fRec13[0] = (0.0f - (((fSlow22 * fRec13[1]) + (fConst2 * fRec13[2])) - (0.998001f * fVec1[((IOTA - iSlow23) & 4095)])));
			fRec12[0] = ((fConst4 * fRec13[0]) + (fConst10 * fRec13[2]));
			fRec3[0] = fRec12[0];
			fVec2[(IOTA & 4095)] = (fSlow20 + (fTemp5 + fRec14[1]));
			fRec15[0] = (0.0f - (((fSlow24 * fRec15[1]) + (fConst2 * fRec15[2])) - (0.997003f * fVec2[((IOTA - iSlow25) & 4095)])));
			fRec14[0] = ((fConst4 * fRec15[0]) + (fConst10 * fRec15[2]));
			fRec4[0] = fRec14[0];
			fVec3[(IOTA & 2047)] = (fSlow20 + (fTemp5 + fRec16[1]));
			fRec17[0] = (0.0f - (((fSlow26 * fRec17[1]) + (fConst2 * fRec17[2])) - (0.996006f * fVec3[((IOTA - iSlow27) & 2047)])));
			fRec16[0] = ((fConst4 * fRec17[0]) + (fConst10 * fRec17[2]));
			fRec5[0] = fRec16[0];
			fVec4[(IOTA & 1023)] = (fSlow20 + (fTemp5 + fRec18[1]));
			fRec19[0] = (0.0f - (((fSlow28 * fRec19[1]) + (fConst2 * fRec19[2])) - (0.99501f * fVec4[((IOTA - iSlow29) & 1023)])));
			fRec18[0] = ((fConst4 * fRec19[0]) + (fConst10 * fRec19[2]));
			fRec6[0] = fRec18[0];
			fRec21[0] = (0.0f - ((fConst5 * fRec21[1]) + (fConst2 * fRec21[2])));
			float fRec20 = ((fConst4 * fRec21[0]) + (fConst10 * fRec21[2]));
			fRec7[0] = fRec20;
			float fTemp6 = (fRec7[0] + (fRec5[0] + (((fRec2[0] + fRec4[0]) + fRec6[0]) + fRec3[0])));
			fVec5[0] = fTemp6;
			fRec22[0] = ((0.999f * fRec22[1]) + fSlow30);
			fRec25[0] = ((0.999f * fRec25[1]) + fSlow32);
			float fTemp7 = (fRec24[1] + (fConst19 * ((float(iSlow31) * fRec25[0]) + fSlow33)));
			fRec24[0] = (fTemp7 - floorf(fTemp7));
			float fTemp8 = (3.1415927f * (fRec22[0] * ftbl0Glass_Harmonica_dspSIG0[int((65536.0f * fRec24[0]))]));
			float fTemp9 = sinf(fTemp8);
			float fTemp10 = (0.0f - fTemp9);
			float fTemp11 = cosf(fTemp8);
			float fTemp12 = ((fRec26[1] * fTemp10) + (4.0f * (fTemp6 * fTemp11)));
			float fTemp13 = ((fTemp10 * fRec27[1]) + (fTemp11 * fTemp12));
			float fTemp14 = ((fTemp10 * fRec28[1]) + (fTemp11 * fTemp13));
			float fTemp15 = ((fTemp10 * fRec29[1]) + (fTemp11 * fTemp14));
			float fTemp16 = ((fTemp10 * fRec30[1]) + (fTemp11 * fTemp15));
			fRec31[0] = ((fTemp10 * fRec31[1]) + (fTemp11 * fTemp16));
			fRec30[0] = ((fTemp9 * fTemp16) + (fTemp11 * fRec31[1]));
			fRec29[0] = ((fTemp9 * fTemp15) + (fTemp11 * fRec30[1]));
			fRec28[0] = ((fTemp9 * fTemp14) + (fTemp11 * fRec29[1]));
			fRec27[0] = ((fTemp9 * fTemp13) + (fTemp11 * fRec28[1]));
			fRec26[0] = ((fTemp9 * fTemp12) + (fTemp11 * fRec27[1]));
			float fTemp17 = (fRec22[0] * (((fSlow35 * fTemp6) + (fSlow36 * (fTemp6 + fVec5[1]))) + (fSlow37 * faustpower2_f(fTemp6))));
			float fTemp18 = sinf(fTemp17);
			float fTemp19 = (0.0f - fTemp18);
			float fTemp20 = cosf(fTemp17);
			float fTemp21 = ((fRec32[1] * fTemp19) + (4.0f * (fTemp6 * fTemp20)));
			float fTemp22 = ((fTemp19 * fRec33[1]) + (fTemp20 * fTemp21));
			float fTemp23 = ((fTemp19 * fRec34[1]) + (fTemp20 * fTemp22));
			float fTemp24 = ((fTemp19 * fRec35[1]) + (fTemp20 * fTemp23));
			float fTemp25 = ((fTemp19 * fRec36[1]) + (fTemp20 * fTemp24));
			fRec37[0] = ((fTemp19 * fRec37[1]) + (fTemp20 * fTemp25));
			fRec36[0] = ((fTemp18 * fTemp25) + (fTemp20 * fRec37[1]));
			fRec35[0] = ((fTemp18 * fTemp24) + (fTemp20 * fRec36[1]));
			fRec34[0] = ((fTemp18 * fTemp23) + (fTemp20 * fRec35[1]));
			fRec33[0] = ((fTemp18 * fTemp22) + (fTemp20 * fRec34[1]));
			fRec32[0] = ((fTemp18 * fTemp21) + (fTemp20 * fRec33[1]));
			float fTemp26 = (fRec1[0] * ((float(iSlow10) * ((4.0f * (fTemp6 * fTemp9)) + (fRec26[1] * fTemp11))) + (float(iSlow34) * ((fRec22[0] * ((4.0f * (fTemp6 * fTemp18)) + (fRec32[1] * fTemp20))) + (4.0f * ((1.0f - fRec22[0]) * fTemp6))))));
			fVec6[(IOTA & 4095)] = fTemp26;
			output0[i] = FAUSTFLOAT((fSlow1 * fTemp26));
			output1[i] = FAUSTFLOAT((fSlow38 * fVec6[((IOTA - iSlow39) & 4095)]));
			iRec0[1] = iRec0[0];
			fRec1[1] = fRec1[0];
			iRec10[1] = iRec10[0];
			fRec11[1] = fRec11[0];
			IOTA = (IOTA + 1);
			fRec9[2] = fRec9[1];
			fRec9[1] = fRec9[0];
			fRec8[1] = fRec8[0];
			fRec2[1] = fRec2[0];
			fRec13[2] = fRec13[1];
			fRec13[1] = fRec13[0];
			fRec12[1] = fRec12[0];
			fRec3[1] = fRec3[0];
			fRec15[2] = fRec15[1];
			fRec15[1] = fRec15[0];
			fRec14[1] = fRec14[0];
			fRec4[1] = fRec4[0];
			fRec17[2] = fRec17[1];
			fRec17[1] = fRec17[0];
			fRec16[1] = fRec16[0];
			fRec5[1] = fRec5[0];
			fRec19[2] = fRec19[1];
			fRec19[1] = fRec19[0];
			fRec18[1] = fRec18[0];
			fRec6[1] = fRec6[0];
			fRec21[2] = fRec21[1];
			fRec21[1] = fRec21[0];
			fRec7[1] = fRec7[0];
			fVec5[1] = fVec5[0];
			fRec22[1] = fRec22[0];
			fRec25[1] = fRec25[0];
			fRec24[1] = fRec24[0];
			fRec31[1] = fRec31[0];
			fRec30[1] = fRec30[0];
			fRec29[1] = fRec29[0];
			fRec28[1] = fRec28[0];
			fRec27[1] = fRec27[0];
			fRec26[1] = fRec26[0];
			fRec37[1] = fRec37[0];
			fRec36[1] = fRec36[0];
			fRec35[1] = fRec35[0];
			fRec34[1] = fRec34[0];
			fRec33[1] = fRec33[0];
			fRec32[1] = fRec32[0];
			
		}
		
	}

	
};



#include "Faust_plugins_template2.cpp"


#endif
