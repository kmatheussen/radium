/* ------------------------------------------------------------
author: "Romain Michon (rmichon@ccrma.stanford.edu)"
copyright: "Romain Michon"
name: "Flute"
version: "1.0"
Code generated with Faust 2.0.a43 (http://faust.grame.fr)
------------------------------------------------------------ */

#ifndef  __Flute_dsp_H__
#define  __Flute_dsp_H__

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


class Flute_dspSIG0 {
	
  private:
	
	int iRec9[2];
	
  public:
	
	int getNumInputsFlute_dspSIG0() {
		return 0;
		
	}
	int getNumOutputsFlute_dspSIG0() {
		return 1;
		
	}
	int getInputRateFlute_dspSIG0(int channel) {
		int rate;
		switch (channel) {
			default: {
				rate = -1;
				break;
			}
			
		}
		return rate;
		
	}
	int getOutputRateFlute_dspSIG0(int channel) {
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
	
	void instanceInitFlute_dspSIG0(int samplingFreq) {
		for (int i7 = 0; (i7 < 2); i7 = (i7 + 1)) {
			iRec9[i7] = 0;
			
		}
		
	}
	
	void fillFlute_dspSIG0(int count, float* output) {
		for (int i = 0; (i < count); i = (i + 1)) {
			iRec9[0] = (1 + iRec9[1]);
			output[i] = sinf((9.58738e-05f * float((iRec9[0] - 1))));
			iRec9[1] = iRec9[0];
			
		}
		
	}
};

Flute_dspSIG0* newFlute_dspSIG0() { return (Flute_dspSIG0*)new Flute_dspSIG0(); }
void deleteFlute_dspSIG0(Flute_dspSIG0* dsp) { delete dsp; }

static float ftbl0Flute_dspSIG0[65536];
static float faustpower2_f(float value) {
	return (value * value);
	
}
static float faustpower3_f(float value) {
	return ((value * value) * value);
	
}

#ifndef FAUSTCLASS 
#define FAUSTCLASS Flute_dsp
#endif

class Flute_dsp : public dsp {
	
  private:
	
	float fRec0[8192];
	float fVec1[4096];
	float fVec3[4096];
	int iRec2[2];
	float fRec3[2];
	float fRec4[2];
	int iRec5[2];
	int iRec6[2];
	int iRec7[2];
	float fRec8[2];
	float fRec10[2];
	float fVec0[2];
	float fRec11[2];
	int iRec12[2];
	float fRec13[2];
	float fRec15[2];
	float fRec14[2];
	float fRec21[2];
	float fRec20[2];
	float fRec19[2];
	float fRec18[2];
	float fRec17[2];
	float fRec16[2];
	float fRec27[2];
	float fRec26[2];
	float fRec25[2];
	float fRec24[2];
	float fRec23[2];
	float fRec22[2];
	float fVec2[2];
	float fRec1[2];
	int iRec28[2];
	float fRec29[2];
	float fRec30[2];
	FAUSTFLOAT fHslider0;
	int fSamplingFreq;
	float fConst0;
	float fConst1;
	float fConst2;
	float fConst3;
	float fConst4;
	float fConst5;
	FAUSTFLOAT fEntry0;
	FAUSTFLOAT fButton0;
	FAUSTFLOAT fCheckbox0;
	FAUSTFLOAT fHslider1;
	FAUSTFLOAT fHslider2;
	FAUSTFLOAT fHslider3;
	FAUSTFLOAT fHslider4;
	FAUSTFLOAT fHslider5;
	FAUSTFLOAT fHslider6;
	FAUSTFLOAT fHslider7;
	FAUSTFLOAT fHslider8;
	FAUSTFLOAT fHslider9;
	float fConst6;
	FAUSTFLOAT fHslider10;
	FAUSTFLOAT fEntry1;
	FAUSTFLOAT fHslider11;
	FAUSTFLOAT fHslider12;
	float fConst7;
	FAUSTFLOAT fHslider13;
	int IOTA;
	FAUSTFLOAT fHslider14;
	FAUSTFLOAT fHslider15;
	FAUSTFLOAT fEntry2;
	FAUSTFLOAT fHslider16;
	
  public:
	
	void static metadata(Meta* m) { 
		m->declare("author", "Romain Michon (rmichon@ccrma.stanford.edu)");
		m->declare("copyright", "Romain Michon");
		m->declare("description", "Nonlinear WaveGuide Flute");
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
		m->declare("name", "Flute");
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
		Flute_dspSIG0* sig0 = newFlute_dspSIG0();
		sig0->instanceInitFlute_dspSIG0(samplingFreq);
		sig0->fillFlute_dspSIG0(65536, ftbl0Flute_dspSIG0);
		deleteFlute_dspSIG0(sig0);
		
	}
	
	virtual void instanceInit(int samplingFreq) {
		fSamplingFreq = samplingFreq;
		fHslider0 = FAUSTFLOAT(0.6f);
		fConst0 = min(1.92e+05f, max(1.0f, float(fSamplingFreq)));
		fConst1 = (1.0f / tanf((6283.1855f / fConst0)));
		fConst2 = (1.0f + fConst1);
		fConst3 = (0.0f - ((1.0f - fConst1) / fConst2));
		fConst4 = (1.0f / fConst2);
		fConst5 = (0.5f * fConst0);
		fEntry0 = FAUSTFLOAT(4.4e+02f);
		fButton0 = FAUSTFLOAT(0.0f);
		fCheckbox0 = FAUSTFLOAT(0.0f);
		for (int i0 = 0; (i0 < 2); i0 = (i0 + 1)) {
			iRec2[i0] = 0;
			
		}
		fHslider1 = FAUSTFLOAT(0.05f);
		fHslider2 = FAUSTFLOAT(0.2f);
		fHslider3 = FAUSTFLOAT(1.0f);
		for (int i1 = 0; (i1 < 2); i1 = (i1 + 1)) {
			fRec3[i1] = 0.0f;
			
		}
		fHslider4 = FAUSTFLOAT(0.9f);
		for (int i2 = 0; (i2 < 2); i2 = (i2 + 1)) {
			fRec4[i2] = 0.0f;
			
		}
		fHslider5 = FAUSTFLOAT(0.1f);
		for (int i3 = 0; (i3 < 2); i3 = (i3 + 1)) {
			iRec5[i3] = 0;
			
		}
		fHslider6 = FAUSTFLOAT(0.1f);
		for (int i4 = 0; (i4 < 2); i4 = (i4 + 1)) {
			iRec6[i4] = 0;
			
		}
		for (int i5 = 0; (i5 < 2); i5 = (i5 + 1)) {
			iRec7[i5] = 0;
			
		}
		fHslider7 = FAUSTFLOAT(0.5f);
		fHslider8 = FAUSTFLOAT(0.1f);
		fHslider9 = FAUSTFLOAT(0.2f);
		for (int i6 = 0; (i6 < 2); i6 = (i6 + 1)) {
			fRec8[i6] = 0.0f;
			
		}
		fConst6 = (1.0f / fConst0);
		fHslider10 = FAUSTFLOAT(5.0f);
		for (int i8 = 0; (i8 < 2); i8 = (i8 + 1)) {
			fRec10[i8] = 0.0f;
			
		}
		fEntry1 = FAUSTFLOAT(0.0f);
		for (int i9 = 0; (i9 < 2); i9 = (i9 + 1)) {
			fVec0[i9] = 0.0f;
			
		}
		fHslider11 = FAUSTFLOAT(0.0f);
		for (int i10 = 0; (i10 < 2); i10 = (i10 + 1)) {
			fRec11[i10] = 0.0f;
			
		}
		for (int i11 = 0; (i11 < 2); i11 = (i11 + 1)) {
			iRec12[i11] = 0;
			
		}
		fHslider12 = FAUSTFLOAT(0.1f);
		fConst7 = (1.0f - (1.0f / powf(1e+05f, (1e+01f / fConst0))));
		for (int i12 = 0; (i12 < 2); i12 = (i12 + 1)) {
			fRec13[i12] = 0.0f;
			
		}
		fHslider13 = FAUSTFLOAT(2.2e+02f);
		for (int i13 = 0; (i13 < 2); i13 = (i13 + 1)) {
			fRec15[i13] = 0.0f;
			
		}
		for (int i14 = 0; (i14 < 2); i14 = (i14 + 1)) {
			fRec14[i14] = 0.0f;
			
		}
		for (int i15 = 0; (i15 < 2); i15 = (i15 + 1)) {
			fRec21[i15] = 0.0f;
			
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
			fRec27[i21] = 0.0f;
			
		}
		for (int i22 = 0; (i22 < 2); i22 = (i22 + 1)) {
			fRec26[i22] = 0.0f;
			
		}
		for (int i23 = 0; (i23 < 2); i23 = (i23 + 1)) {
			fRec25[i23] = 0.0f;
			
		}
		for (int i24 = 0; (i24 < 2); i24 = (i24 + 1)) {
			fRec24[i24] = 0.0f;
			
		}
		for (int i25 = 0; (i25 < 2); i25 = (i25 + 1)) {
			fRec23[i25] = 0.0f;
			
		}
		for (int i26 = 0; (i26 < 2); i26 = (i26 + 1)) {
			fRec22[i26] = 0.0f;
			
		}
		IOTA = 0;
		for (int i27 = 0; (i27 < 4096); i27 = (i27 + 1)) {
			fVec1[i27] = 0.0f;
			
		}
		for (int i28 = 0; (i28 < 2); i28 = (i28 + 1)) {
			fVec2[i28] = 0.0f;
			
		}
		for (int i29 = 0; (i29 < 2); i29 = (i29 + 1)) {
			fRec1[i29] = 0.0f;
			
		}
		for (int i30 = 0; (i30 < 8192); i30 = (i30 + 1)) {
			fRec0[i30] = 0.0f;
			
		}
		for (int i31 = 0; (i31 < 2); i31 = (i31 + 1)) {
			iRec28[i31] = 0;
			
		}
		fHslider14 = FAUSTFLOAT(0.1f);
		fHslider15 = FAUSTFLOAT(0.1f);
		for (int i32 = 0; (i32 < 2); i32 = (i32 + 1)) {
			fRec29[i32] = 0.0f;
			
		}
		fEntry2 = FAUSTFLOAT(1.0f);
		for (int i33 = 0; (i33 < 2); i33 = (i33 + 1)) {
			fRec30[i33] = 0.0f;
			
		}
		for (int i34 = 0; (i34 < 4096); i34 = (i34 + 1)) {
			fVec3[i34] = 0.0f;
			
		}
		fHslider16 = FAUSTFLOAT(0.5f);
		
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
		interface->addNumEntry("gain", &fEntry2, 1.0f, 0.0f, 1.0f, 0.01f);
		interface->declare(&fButton0, "1", "");
		interface->declare(&fButton0, "tooltip", "noteOn = 1, noteOff = 0");
		interface->addButton("gate",&fButton0);
		interface->closeBox();
		interface->openHorizontalBox("Envelopes_and_Vibrato");
		interface->openVerticalBox("Global_Envelope_Parameters");
		interface->declare(&fHslider14, "6", "");
		interface->declare(&fHslider14, "tooltip", "Global envelope attack duration");
		interface->declare(&fHslider14, "unit", "s");
		interface->addHorizontalSlider("Glob_Env_Attack", &fHslider14, 0.1f, 0.0f, 2.0f, 0.01f);
		interface->declare(&fHslider15, "6", "");
		interface->declare(&fHslider15, "tooltip", "Global envelope release duration");
		interface->declare(&fHslider15, "unit", "s");
		interface->addHorizontalSlider("Glob_Env_Release", &fHslider15, 0.1f, 0.0f, 2.0f, 0.01f);
		interface->closeBox();
		interface->openVerticalBox("Pressure_Envelope_Parameters");
		interface->declare(&fHslider1, "5", "");
		interface->declare(&fHslider1, "tooltip", "Pressure envelope attack duration");
		interface->declare(&fHslider1, "unit", "s");
		interface->addHorizontalSlider("Press_Env_Attack", &fHslider1, 0.05f, 0.0f, 2.0f, 0.01f);
		interface->declare(&fHslider2, "5", "");
		interface->declare(&fHslider2, "tooltip", "Pressure envelope decay duration");
		interface->declare(&fHslider2, "unit", "s");
		interface->addHorizontalSlider("Press_Env_Decay", &fHslider2, 0.2f, 0.0f, 2.0f, 0.01f);
		interface->declare(&fHslider3, "5", "");
		interface->declare(&fHslider3, "tooltip", "Pressure envelope release duration");
		interface->declare(&fHslider3, "unit", "s");
		interface->addHorizontalSlider("Press_Env_Release", &fHslider3, 1.0f, 0.0f, 2.0f, 0.01f);
		interface->declare(&fCheckbox0, "5", "");
		interface->declare(&fCheckbox0, "tooltip", "Activate Pressure envelope");
		interface->declare(&fCheckbox0, "unit", "s");
		interface->addCheckButton("Pressure_Env",&fCheckbox0);
		interface->closeBox();
		interface->openVerticalBox("Vibrato_Parameters");
		interface->declare(&fHslider7, "4", "");
		interface->declare(&fHslider7, "tooltip", "Vibrato attack duration");
		interface->declare(&fHslider7, "unit", "s");
		interface->addHorizontalSlider("Vibrato_Attack", &fHslider7, 0.5f, 0.0f, 2.0f, 0.01f);
		interface->declare(&fHslider8, "4", "");
		interface->declare(&fHslider8, "tooltip", "Vibrato silence duration before attack");
		interface->declare(&fHslider8, "unit", "s");
		interface->addHorizontalSlider("Vibrato_Begin", &fHslider8, 0.1f, 0.0f, 2.0f, 0.01f);
		interface->declare(&fHslider10, "4", "");
		interface->declare(&fHslider10, "unit", "Hz");
		interface->addHorizontalSlider("Vibrato_Freq", &fHslider10, 5.0f, 1.0f, 15.0f, 0.1f);
		interface->declare(&fHslider6, "4", "");
		interface->declare(&fHslider6, "tooltip", "A value between 0 and 1");
		interface->addHorizontalSlider("Vibrato_Gain", &fHslider6, 0.1f, 0.0f, 1.0f, 0.01f);
		interface->declare(&fHslider9, "4", "");
		interface->declare(&fHslider9, "tooltip", "Vibrato release duration");
		interface->declare(&fHslider9, "unit", "s");
		interface->addHorizontalSlider("Vibrato_Release", &fHslider9, 0.2f, 0.0f, 2.0f, 0.01f);
		interface->closeBox();
		interface->closeBox();
		interface->openHorizontalBox("Physical_and_Nonlinearity");
		interface->openVerticalBox("Nonlinear_Filter_Parameters");
		interface->declare(&fHslider13, "3", "");
		interface->declare(&fHslider13, "tooltip", "Frequency of the sine wave for the modulation of theta (works if Modulation Type=3)");
		interface->declare(&fHslider13, "unit", "Hz");
		interface->addHorizontalSlider("Modulation_Frequency", &fHslider13, 2.2e+02f, 2e+01f, 1e+03f, 0.1f);
		interface->declare(&fEntry1, "3", "");
		interface->declare(&fEntry1, "tooltip", "0=theta is modulated by the incoming signal; 1=theta is modulated by the averaged incoming signal; 2=theta is modulated by the squared incoming signal; 3=theta is modulated by a sine wave of frequency freqMod; 4=theta is modulated by a sine wave of frequency freq;");
		interface->addNumEntry("Modulation_Type", &fEntry1, 0.0f, 0.0f, 4.0f, 1.0f);
		interface->declare(&fHslider11, "3", "");
		interface->declare(&fHslider11, "tooltip", "Nonlinearity factor (value between 0 and 1)");
		interface->addHorizontalSlider("Nonlinearity", &fHslider11, 0.0f, 0.0f, 1.0f, 0.01f);
		interface->declare(&fHslider12, "3", "");
		interface->declare(&fHslider12, "Attack duration of the nonlinearity", "");
		interface->declare(&fHslider12, "unit", "s");
		interface->addHorizontalSlider("Nonlinearity Attack", &fHslider12, 0.1f, 0.0f, 2.0f, 0.01f);
		interface->closeBox();
		interface->openVerticalBox("Physical_Parameters");
		interface->declare(&fHslider5, "2", "");
		interface->declare(&fHslider5, "tooltip", "Breath noise gain (value between 0 and 1)");
		interface->addHorizontalSlider("Noise Gain", &fHslider5, 0.1f, 0.0f, 1.0f, 0.01f);
		interface->declare(&fHslider4, "2", "");
		interface->declare(&fHslider4, "tooltip", "Breath pressure (value bewteen 0 and 1)");
		interface->addHorizontalSlider("Pressure", &fHslider4, 0.9f, 0.0f, 1.5f, 0.01f);
		interface->closeBox();
		interface->closeBox();
		interface->openVerticalBox("Spat");
		interface->addHorizontalSlider("pan angle", &fHslider0, 0.6f, 0.0f, 1.0f, 0.01f);
		interface->addHorizontalSlider("spatial width", &fHslider16, 0.5f, 0.0f, 1.0f, 0.01f);
		interface->closeBox();
		interface->closeBox();
		
	}
	
	virtual void compute(int count, FAUSTFLOAT** inputs, FAUSTFLOAT** outputs) {
		FAUSTFLOAT* output0 = outputs[0];
		FAUSTFLOAT* output1 = outputs[1];
		float fSlow0 = float(fHslider0);
		float fSlow1 = (0.5f * (1.0f - fSlow0));
		float fSlow2 = float(fEntry0);
		float fSlow3 = (fConst5 / fSlow2);
		int iSlow4 = int((fSlow3 - 2.0f));
		float fSlow5 = (fSlow3 - float((2 + iSlow4)));
		int iSlow6 = int(float(fButton0));
		int iSlow7 = (iSlow6 | int(float(fCheckbox0)));
		int iSlow8 = (iSlow7 > 0);
		float fSlow9 = float(fHslider1);
		float fSlow10 = (1.0f / (float((fSlow9 == 0.0f)) + (fConst0 * fSlow9)));
		float fSlow11 = float(fHslider2);
		float fSlow12 = (1.0f - powf(9e+01f, (1.0f / (float((fSlow11 == 0.0f)) + (fConst0 * fSlow11)))));
		float fSlow13 = float(fHslider3);
		float fSlow14 = (1.0f - (1.0f / powf(9e+04f, (1.0f / (float((fSlow13 == 0.0f)) + (fConst0 * fSlow13))))));
		int iSlow15 = (iSlow7 <= 0);
		float fSlow16 = (0.001f * float(fHslider4));
		float fSlow17 = (5.122274e-11f * float(fHslider5));
		float fSlow18 = float(fHslider6);
		int iSlow19 = (iSlow6 > 0);
		float fSlow20 = float(fHslider7);
		float fSlow21 = (1.0f / (float((fSlow20 == 0.0f)) + (fConst0 * fSlow20)));
		float fSlow22 = float(fHslider8);
		float fSlow23 = (fConst0 * fSlow22);
		float fSlow24 = (float((fSlow22 == 0.0f)) + fSlow23);
		float fSlow25 = float(fHslider9);
		float fSlow26 = (1.0f - (1.0f / powf(1e+05f, (1.0f / (float((fSlow25 == 0.0f)) + (fConst0 * fSlow25))))));
		int iSlow27 = (iSlow6 <= 0);
		float fSlow28 = (fConst6 * float(fHslider10));
		float fSlow29 = float(fEntry1);
		int iSlow30 = (fSlow29 >= 3.0f);
		float fSlow31 = (fConst0 / fSlow2);
		int iSlow32 = int((fSlow31 - 2.0f));
		float fSlow33 = (float((3 + iSlow32)) - fSlow31);
		int iSlow34 = (1 + (iSlow32 & 4095));
		float fSlow35 = (fSlow31 - float((2 + iSlow32)));
		int iSlow36 = (1 + ((1 + iSlow32) & 4095));
		float fSlow37 = (0.001f * float(fHslider11));
		float fSlow38 = float(fHslider12);
		float fSlow39 = (1.0f / (float((fSlow38 == 0.0f)) + (fConst0 * fSlow38)));
		int iSlow40 = (fSlow29 != 4.0f);
		float fSlow41 = (0.001f * float(fHslider13));
		float fSlow42 = (fSlow2 * float((fSlow29 == 4.0f)));
		int iSlow43 = (fSlow29 < 3.0f);
		float fSlow44 = (3.1415927f * float((fSlow29 == 0.0f)));
		float fSlow45 = (1.5707964f * float((fSlow29 == 1.0f)));
		float fSlow46 = (3.1415927f * float((fSlow29 == 2.0f)));
		int iSlow47 = ((1 + iSlow4) & 4095);
		float fSlow48 = (float((3 + iSlow4)) - fSlow3);
		int iSlow49 = (iSlow4 & 4095);
		float fSlow50 = float(fHslider14);
		float fSlow51 = (1.0f / (float((fSlow50 == 0.0f)) + (fConst0 * fSlow50)));
		float fSlow52 = float(fHslider15);
		float fSlow53 = (1.0f - (1.0f / powf(1e+05f, (1.0f / (float((fSlow52 == 0.0f)) + (fConst0 * fSlow52))))));
		float fSlow54 = (0.001f * float(fEntry2));
		float fSlow55 = (0.5f * fSlow0);
		int iSlow56 = (int((fConst5 * (float(fHslider16) / fSlow2))) & 4095);
		for (int i = 0; (i < count); i = (i + 1)) {
			iRec2[0] = (iSlow8 & (iRec2[1] | (fRec3[1] >= 1.0f)));
			int iTemp0 = (iSlow15 & (fRec3[1] > 0.0f));
			fRec3[0] = (((fSlow10 * float((((iRec2[1] == 0) & iSlow8) & (fRec3[1] < 1.0f)))) + (fRec3[1] * ((1.0f - (fSlow12 * float((iRec2[1] & (fRec3[1] > 9e+01f))))) - (fSlow14 * float(iTemp0))))) * float(((iTemp0 == 0) | (fRec3[1] >= 1e-06f))));
			fRec4[0] = ((0.999f * fRec4[1]) + fSlow16);
			iRec5[0] = (12345 + (1103515245 * iRec5[1]));
			iRec6[0] = (iSlow19 & (iRec6[1] | (fRec8[1] >= 1.0f)));
			iRec7[0] = (iSlow19 * (1 + iRec7[1]));
			int iTemp1 = (iSlow27 & (fRec8[1] > 0.0f));
			fRec8[0] = (((fSlow21 * float((((((iRec6[1] == 0) & iSlow19) & (fRec8[1] < 1.0f)) & (float(iRec7[1]) > fSlow23)) * (1 - (float(iRec7[1]) < fSlow24))))) + (fRec8[1] * (1.0f - (fSlow26 * float(iTemp1))))) * float(((iTemp1 == 0) | (fRec8[1] >= 1e-06f))));
			float fTemp2 = (fRec10[1] + fSlow28);
			fRec10[0] = (fTemp2 - floorf(fTemp2));
			float fTemp3 = ((fSlow33 * fRec0[((IOTA - iSlow34) & 8191)]) + (fSlow35 * fRec0[((IOTA - iSlow36) & 8191)]));
			fVec0[0] = fTemp3;
			fRec11[0] = ((0.999f * fRec11[1]) + fSlow37);
			iRec12[0] = (iSlow19 & (iRec12[1] | (fRec13[1] >= 1.0f)));
			int iTemp4 = (iSlow27 & (fRec13[1] > 0.0f));
			fRec13[0] = (((fSlow39 * float((((iRec12[1] == 0) & iSlow19) & (fRec13[1] < 1.0f)))) + (fRec13[1] * (1.0f - (fConst7 * float(iTemp4))))) * float(((iTemp4 == 0) | (fRec13[1] >= 1e-06f))));
			float fTemp5 = (fRec11[0] * fRec13[0]);
			fRec15[0] = ((0.999f * fRec15[1]) + fSlow41);
			float fTemp6 = (fRec14[1] + (fConst6 * ((float(iSlow40) * fRec15[0]) + fSlow42)));
			fRec14[0] = (fTemp6 - floorf(fTemp6));
			float fTemp7 = (3.1415927f * (fTemp5 * ftbl0Flute_dspSIG0[int((65536.0f * fRec14[0]))]));
			float fTemp8 = sinf(fTemp7);
			float fTemp9 = (0.0f - fTemp8);
			float fTemp10 = cosf(fTemp7);
			float fTemp11 = ((fRec16[1] * fTemp9) + (fTemp3 * fTemp10));
			float fTemp12 = ((fTemp9 * fRec17[1]) + (fTemp10 * fTemp11));
			float fTemp13 = ((fTemp9 * fRec18[1]) + (fTemp10 * fTemp12));
			float fTemp14 = ((fTemp9 * fRec19[1]) + (fTemp10 * fTemp13));
			float fTemp15 = ((fTemp9 * fRec20[1]) + (fTemp10 * fTemp14));
			fRec21[0] = ((fTemp9 * fRec21[1]) + (fTemp10 * fTemp15));
			fRec20[0] = ((fTemp8 * fTemp15) + (fTemp10 * fRec21[1]));
			fRec19[0] = ((fTemp8 * fTemp14) + (fTemp10 * fRec20[1]));
			fRec18[0] = ((fTemp8 * fTemp13) + (fTemp10 * fRec19[1]));
			fRec17[0] = ((fTemp8 * fTemp12) + (fTemp10 * fRec18[1]));
			fRec16[0] = ((fTemp8 * fTemp11) + (fTemp10 * fRec17[1]));
			float fTemp16 = (fTemp5 * (((fSlow44 * fTemp3) + (fSlow45 * (fTemp3 + fVec0[1]))) + (fSlow46 * faustpower2_f(fTemp3))));
			float fTemp17 = sinf(fTemp16);
			float fTemp18 = (0.0f - fTemp17);
			float fTemp19 = cosf(fTemp16);
			float fTemp20 = ((fRec22[1] * fTemp18) + (fTemp3 * fTemp19));
			float fTemp21 = ((fTemp18 * fRec23[1]) + (fTemp19 * fTemp20));
			float fTemp22 = ((fTemp18 * fRec24[1]) + (fTemp19 * fTemp21));
			float fTemp23 = ((fTemp18 * fRec25[1]) + (fTemp19 * fTemp22));
			float fTemp24 = ((fTemp18 * fRec26[1]) + (fTemp19 * fTemp23));
			fRec27[0] = ((fTemp18 * fRec27[1]) + (fTemp19 * fTemp24));
			fRec26[0] = ((fTemp17 * fTemp24) + (fTemp19 * fRec27[1]));
			fRec25[0] = ((fTemp17 * fTemp23) + (fTemp19 * fRec26[1]));
			fRec24[0] = ((fTemp17 * fTemp22) + (fTemp19 * fRec25[1]));
			fRec23[0] = ((fTemp17 * fTemp21) + (fTemp19 * fRec24[1]));
			fRec22[0] = ((fTemp17 * fTemp20) + (fTemp19 * fRec23[1]));
			float fTemp25 = (0.4f * ((float(iSlow30) * ((fTemp3 * fTemp8) + (fRec16[1] * fTemp10))) + (float(iSlow43) * ((fRec11[0] * ((fTemp3 * fTemp17) + (fRec22[1] * fTemp19))) + ((1.0f - fRec11[0]) * fTemp3)))));
			float fTemp26 = ((((fRec3[0] * fRec4[0]) * (1.1f + (fSlow17 * float(iRec5[0])))) + (fSlow18 * (fRec8[0] * ftbl0Flute_dspSIG0[int((65536.0f * fRec10[0]))]))) + fTemp25);
			fVec1[(IOTA & 4095)] = fTemp26;
			float fTemp27 = (fSlow5 * fVec1[((IOTA - iSlow47) & 4095)]);
			float fTemp28 = (fSlow48 * fVec1[((IOTA - iSlow49) & 4095)]);
			float fTemp29 = ((fTemp27 + (fTemp25 + fTemp28)) - faustpower3_f((fTemp28 + fTemp27)));
			fVec2[0] = fTemp29;
			fRec1[0] = ((fConst3 * fRec1[1]) + (fConst4 * (fTemp29 + fVec2[1])));
			fRec0[(IOTA & 8191)] = fRec1[0];
			iRec28[0] = (iSlow19 & (iRec28[1] | (fRec29[1] >= 1.0f)));
			int iTemp30 = (iSlow27 & (fRec29[1] > 0.0f));
			fRec29[0] = (((fSlow51 * float((((iRec28[1] == 0) & iSlow19) & (fRec29[1] < 1.0f)))) + (fRec29[1] * (1.0f - (fSlow53 * float(iTemp30))))) * float(((iTemp30 == 0) | (fRec29[1] >= 1e-06f))));
			fRec30[0] = ((0.999f * fRec30[1]) + fSlow54);
			float fTemp31 = ((fRec0[((IOTA - 0) & 8191)] * fRec29[0]) * fRec30[0]);
			fVec3[(IOTA & 4095)] = fTemp31;
			output0[i] = FAUSTFLOAT((fSlow1 * fTemp31));
			output1[i] = FAUSTFLOAT((fSlow55 * fVec3[((IOTA - iSlow56) & 4095)]));
			iRec2[1] = iRec2[0];
			fRec3[1] = fRec3[0];
			fRec4[1] = fRec4[0];
			iRec5[1] = iRec5[0];
			iRec6[1] = iRec6[0];
			iRec7[1] = iRec7[0];
			fRec8[1] = fRec8[0];
			fRec10[1] = fRec10[0];
			fVec0[1] = fVec0[0];
			fRec11[1] = fRec11[0];
			iRec12[1] = iRec12[0];
			fRec13[1] = fRec13[0];
			fRec15[1] = fRec15[0];
			fRec14[1] = fRec14[0];
			fRec21[1] = fRec21[0];
			fRec20[1] = fRec20[0];
			fRec19[1] = fRec19[0];
			fRec18[1] = fRec18[0];
			fRec17[1] = fRec17[0];
			fRec16[1] = fRec16[0];
			fRec27[1] = fRec27[0];
			fRec26[1] = fRec26[0];
			fRec25[1] = fRec25[0];
			fRec24[1] = fRec24[0];
			fRec23[1] = fRec23[0];
			fRec22[1] = fRec22[0];
			IOTA = (IOTA + 1);
			fVec2[1] = fVec2[0];
			fRec1[1] = fRec1[0];
			iRec28[1] = iRec28[0];
			fRec29[1] = fRec29[0];
			fRec30[1] = fRec30[0];
			
		}
		
	}

	
};



#include "Faust_plugins_template2.cpp"


#endif
