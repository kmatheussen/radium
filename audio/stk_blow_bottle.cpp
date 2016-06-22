/* ------------------------------------------------------------
author: "Romain Michon (rmichon@ccrma.stanford.edu)"
copyright: "Romain Michon"
name: "BlowBottle"
version: "1.0"
Code generated with Faust 2.0.a43 (http://faust.grame.fr)
------------------------------------------------------------ */

#ifndef  __Blow_Bottle_dsp_H__
#define  __Blow_Bottle_dsp_H__

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


class Blow_Bottle_dspSIG0 {
	
  private:
	
	int iRec10[2];
	
  public:
	
	int getNumInputsBlow_Bottle_dspSIG0() {
		return 0;
		
	}
	int getNumOutputsBlow_Bottle_dspSIG0() {
		return 1;
		
	}
	int getInputRateBlow_Bottle_dspSIG0(int channel) {
		int rate;
		switch (channel) {
			default: {
				rate = -1;
				break;
			}
			
		}
		return rate;
		
	}
	int getOutputRateBlow_Bottle_dspSIG0(int channel) {
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
	
	void instanceInitBlow_Bottle_dspSIG0(int samplingFreq) {
		for (int i6 = 0; (i6 < 2); i6 = (i6 + 1)) {
			iRec10[i6] = 0;
			
		}
		
	}
	
	void fillBlow_Bottle_dspSIG0(int count, float* output) {
		for (int i = 0; (i < count); i = (i + 1)) {
			iRec10[0] = (1 + iRec10[1]);
			output[i] = sinf((9.58738e-05f * float((iRec10[0] - 1))));
			iRec10[1] = iRec10[0];
			
		}
		
	}
};

Blow_Bottle_dspSIG0* newBlow_Bottle_dspSIG0() { return (Blow_Bottle_dspSIG0*)new Blow_Bottle_dspSIG0(); }
void deleteBlow_Bottle_dspSIG0(Blow_Bottle_dspSIG0* dsp) { delete dsp; }

static float ftbl0Blow_Bottle_dspSIG0[65536];
static float faustpower2_f(float value) {
	return (value * value);
	
}

#ifndef FAUSTCLASS 
#define FAUSTCLASS Blow_Bottle_dsp
#endif

class Blow_Bottle_dsp : public dsp {
	
  private:
	
	float fVec0[4096];
	float fRec4[3];
	float fRec2[3];
	float fRec0[2];
	int iRec5[2];
	float fRec6[2];
	int iRec7[2];
	int iRec8[2];
	float fRec9[2];
	float fRec11[2];
	float fRec12[2];
	int iRec13[2];
	float fRec14[2];
	float fRec16[2];
	float fRec15[2];
	float fRec22[2];
	float fRec21[2];
	float fRec20[2];
	float fRec19[2];
	float fRec18[2];
	float fRec17[2];
	float fRec28[2];
	float fRec27[2];
	float fRec26[2];
	float fRec25[2];
	float fRec24[2];
	float fRec23[2];
	int iRec29[2];
	float fRec3[2];
	float fRec1[2];
	int iRec30[2];
	float fRec31[2];
	FAUSTFLOAT fHslider0;
	FAUSTFLOAT fEntry0;
	FAUSTFLOAT fHslider1;
	FAUSTFLOAT fButton0;
	int fSamplingFreq;
	float fConst0;
	float fConst1;
	float fConst2;
	float fConst3;
	FAUSTFLOAT fHslider2;
	FAUSTFLOAT fHslider3;
	FAUSTFLOAT fHslider4;
	FAUSTFLOAT fHslider5;
	float fConst4;
	FAUSTFLOAT fHslider6;
	FAUSTFLOAT fEntry1;
	FAUSTFLOAT fHslider7;
	FAUSTFLOAT fHslider8;
	FAUSTFLOAT fHslider9;
	FAUSTFLOAT fHslider10;
	FAUSTFLOAT fEntry2;
	float fConst5;
	FAUSTFLOAT fHslider11;
	FAUSTFLOAT fHslider12;
	FAUSTFLOAT fHslider13;
	int IOTA;
	float fConst6;
	FAUSTFLOAT fHslider14;
	
  public:
	
	void static metadata(Meta* m) { 
		m->declare("author", "Romain Michon (rmichon@ccrma.stanford.edu)");
		m->declare("copyright", "Romain Michon");
		m->declare("description", "Blown Bottle Instrument");
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
		m->declare("name", "BlowBottle");
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
		Blow_Bottle_dspSIG0* sig0 = newBlow_Bottle_dspSIG0();
		sig0->instanceInitBlow_Bottle_dspSIG0(samplingFreq);
		sig0->fillBlow_Bottle_dspSIG0(65536, ftbl0Blow_Bottle_dspSIG0);
		deleteBlow_Bottle_dspSIG0(sig0);
		
	}
	
	virtual void instanceInit(int samplingFreq) {
		fSamplingFreq = samplingFreq;
		fHslider0 = FAUSTFLOAT(0.6f);
		fEntry0 = FAUSTFLOAT(1.0f);
		for (int i0 = 0; (i0 < 2); i0 = (i0 + 1)) {
			fRec0[i0] = 0.0f;
			
		}
		fHslider1 = FAUSTFLOAT(1.0f);
		fButton0 = FAUSTFLOAT(0.0f);
		for (int i1 = 0; (i1 < 2); i1 = (i1 + 1)) {
			iRec5[i1] = 0;
			
		}
		fConst0 = min(1.92e+05f, max(1.0f, float(fSamplingFreq)));
		fConst1 = (0.02f * fConst0);
		fConst2 = (1.0f - powf(8e+01f, (1e+02f / fConst0)));
		fConst3 = (0.2f * fConst0);
		for (int i2 = 0; (i2 < 2); i2 = (i2 + 1)) {
			fRec6[i2] = 0.0f;
			
		}
		fHslider2 = FAUSTFLOAT(0.1f);
		for (int i3 = 0; (i3 < 2); i3 = (i3 + 1)) {
			iRec7[i3] = 0;
			
		}
		for (int i4 = 0; (i4 < 2); i4 = (i4 + 1)) {
			iRec8[i4] = 0;
			
		}
		fHslider3 = FAUSTFLOAT(0.5f);
		fHslider4 = FAUSTFLOAT(0.05f);
		fHslider5 = FAUSTFLOAT(0.01f);
		for (int i5 = 0; (i5 < 2); i5 = (i5 + 1)) {
			fRec9[i5] = 0.0f;
			
		}
		fConst4 = (1.0f / fConst0);
		fHslider6 = FAUSTFLOAT(5.0f);
		for (int i7 = 0; (i7 < 2); i7 = (i7 + 1)) {
			fRec11[i7] = 0.0f;
			
		}
		fEntry1 = FAUSTFLOAT(0.0f);
		fHslider7 = FAUSTFLOAT(0.0f);
		for (int i8 = 0; (i8 < 2); i8 = (i8 + 1)) {
			fRec12[i8] = 0.0f;
			
		}
		for (int i9 = 0; (i9 < 2); i9 = (i9 + 1)) {
			iRec13[i9] = 0;
			
		}
		fHslider8 = FAUSTFLOAT(0.1f);
		fHslider9 = FAUSTFLOAT(0.5f);
		for (int i10 = 0; (i10 < 2); i10 = (i10 + 1)) {
			fRec14[i10] = 0.0f;
			
		}
		fHslider10 = FAUSTFLOAT(2.2e+02f);
		for (int i11 = 0; (i11 < 2); i11 = (i11 + 1)) {
			fRec16[i11] = 0.0f;
			
		}
		fEntry2 = FAUSTFLOAT(4.4e+02f);
		for (int i12 = 0; (i12 < 2); i12 = (i12 + 1)) {
			fRec15[i12] = 0.0f;
			
		}
		for (int i13 = 0; (i13 < 2); i13 = (i13 + 1)) {
			fRec22[i13] = 0.0f;
			
		}
		for (int i14 = 0; (i14 < 2); i14 = (i14 + 1)) {
			fRec21[i14] = 0.0f;
			
		}
		for (int i15 = 0; (i15 < 2); i15 = (i15 + 1)) {
			fRec20[i15] = 0.0f;
			
		}
		for (int i16 = 0; (i16 < 2); i16 = (i16 + 1)) {
			fRec19[i16] = 0.0f;
			
		}
		for (int i17 = 0; (i17 < 2); i17 = (i17 + 1)) {
			fRec18[i17] = 0.0f;
			
		}
		for (int i18 = 0; (i18 < 2); i18 = (i18 + 1)) {
			fRec17[i18] = 0.0f;
			
		}
		for (int i19 = 0; (i19 < 2); i19 = (i19 + 1)) {
			fRec28[i19] = 0.0f;
			
		}
		for (int i20 = 0; (i20 < 2); i20 = (i20 + 1)) {
			fRec27[i20] = 0.0f;
			
		}
		for (int i21 = 0; (i21 < 2); i21 = (i21 + 1)) {
			fRec26[i21] = 0.0f;
			
		}
		for (int i22 = 0; (i22 < 2); i22 = (i22 + 1)) {
			fRec25[i22] = 0.0f;
			
		}
		for (int i23 = 0; (i23 < 2); i23 = (i23 + 1)) {
			fRec24[i23] = 0.0f;
			
		}
		for (int i24 = 0; (i24 < 2); i24 = (i24 + 1)) {
			fRec23[i24] = 0.0f;
			
		}
		fConst5 = (6.2831855f / fConst0);
		fHslider11 = FAUSTFLOAT(0.5f);
		for (int i25 = 0; (i25 < 2); i25 = (i25 + 1)) {
			iRec29[i25] = 0;
			
		}
		for (int i26 = 0; (i26 < 3); i26 = (i26 + 1)) {
			fRec4[i26] = 0.0f;
			
		}
		for (int i27 = 0; (i27 < 3); i27 = (i27 + 1)) {
			fRec2[i27] = 0.0f;
			
		}
		for (int i28 = 0; (i28 < 2); i28 = (i28 + 1)) {
			fRec3[i28] = 0.0f;
			
		}
		for (int i29 = 0; (i29 < 2); i29 = (i29 + 1)) {
			fRec1[i29] = 0.0f;
			
		}
		for (int i30 = 0; (i30 < 2); i30 = (i30 + 1)) {
			iRec30[i30] = 0;
			
		}
		fHslider12 = FAUSTFLOAT(0.01f);
		fHslider13 = FAUSTFLOAT(0.01f);
		for (int i31 = 0; (i31 < 2); i31 = (i31 + 1)) {
			fRec31[i31] = 0.0f;
			
		}
		IOTA = 0;
		for (int i32 = 0; (i32 < 4096); i32 = (i32 + 1)) {
			fVec0[i32] = 0.0f;
			
		}
		fConst6 = (0.5f * fConst0);
		fHslider14 = FAUSTFLOAT(0.5f);
		
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
		interface->declare(&fEntry0, "1", "");
		interface->declare(&fEntry0, "tooltip", "Gain (value between 0 and 1)");
		interface->addNumEntry("gain", &fEntry0, 1.0f, 0.0f, 1.0f, 0.01f);
		interface->declare(&fButton0, "1", "");
		interface->declare(&fButton0, "tooltip", "noteOn = 1, noteOff = 0");
		interface->addButton("gate",&fButton0);
		interface->closeBox();
		interface->openHorizontalBox("Envelopes_and_Vibrato");
		interface->openVerticalBox("Envelope_Parameters");
		interface->declare(&fHslider12, "5", "");
		interface->declare(&fHslider12, "tooltip", "Envelope attack duration");
		interface->declare(&fHslider12, "unit", "s");
		interface->addHorizontalSlider("Envelope_Attack", &fHslider12, 0.01f, 0.0f, 2.0f, 0.01f);
		interface->declare(&fHslider13, "5", "");
		interface->declare(&fHslider13, "tooltip", "Envelope decay duration");
		interface->declare(&fHslider13, "unit", "s");
		interface->addHorizontalSlider("Envelope_Decay", &fHslider13, 0.01f, 0.0f, 2.0f, 0.01f);
		interface->declare(&fHslider9, "5", "");
		interface->declare(&fHslider9, "tooltip", "Envelope release duration");
		interface->declare(&fHslider9, "unit", "s");
		interface->addHorizontalSlider("Envelope_Release", &fHslider9, 0.5f, 0.0f, 2.0f, 0.01f);
		interface->closeBox();
		interface->openVerticalBox("Vibrato_Parameters");
		interface->declare(&fHslider3, "4", "");
		interface->declare(&fHslider3, "tooltip", "Vibrato attack duration");
		interface->declare(&fHslider3, "unit", "s");
		interface->addHorizontalSlider("Vibrato_Attack", &fHslider3, 0.5f, 0.0f, 2.0f, 0.01f);
		interface->declare(&fHslider4, "4", "");
		interface->declare(&fHslider4, "tooltip", "Vibrato silence duration before attack");
		interface->declare(&fHslider4, "unit", "s");
		interface->addHorizontalSlider("Vibrato_Begin", &fHslider4, 0.05f, 0.0f, 2.0f, 0.01f);
		interface->declare(&fHslider6, "4", "");
		interface->declare(&fHslider6, "unit", "Hz");
		interface->addHorizontalSlider("Vibrato_Freq", &fHslider6, 5.0f, 1.0f, 15.0f, 0.1f);
		interface->declare(&fHslider2, "4", "");
		interface->declare(&fHslider2, "tooltip", "A value between 0 and 1");
		interface->addHorizontalSlider("Vibrato_Gain", &fHslider2, 0.1f, 0.0f, 1.0f, 0.01f);
		interface->declare(&fHslider5, "4", "");
		interface->declare(&fHslider5, "tooltip", "Vibrato release duration");
		interface->declare(&fHslider5, "unit", "s");
		interface->addHorizontalSlider("Vibrato_Release", &fHslider5, 0.01f, 0.0f, 2.0f, 0.01f);
		interface->closeBox();
		interface->closeBox();
		interface->openHorizontalBox("Physical_and_Nonlinearity");
		interface->openVerticalBox("Nonlinear_Filter_Parameters");
		interface->declare(&fHslider10, "3", "");
		interface->declare(&fHslider10, "tooltip", "Frequency of the sine wave for the modulation of theta (works if Modulation Type=3)");
		interface->declare(&fHslider10, "unit", "Hz");
		interface->addHorizontalSlider("Modulation_Frequency", &fHslider10, 2.2e+02f, 2e+01f, 1e+03f, 0.1f);
		interface->declare(&fEntry1, "3", "");
		interface->declare(&fEntry1, "tooltip", "0=theta is modulated by the incoming signal; 1=theta is modulated by the averaged incoming signal; 2=theta is modulated by the squared incoming signal; 3=theta is modulated by a sine wave of frequency freqMod; 4=theta is modulated by a sine wave of frequency freq;");
		interface->addNumEntry("Modulation_Type", &fEntry1, 0.0f, 0.0f, 4.0f, 1.0f);
		interface->declare(&fHslider7, "3", "");
		interface->declare(&fHslider7, "tooltip", "Nonlinearity factor (value between 0 and 1)");
		interface->addHorizontalSlider("Nonlinearity", &fHslider7, 0.0f, 0.0f, 1.0f, 0.01f);
		interface->declare(&fHslider8, "3", "");
		interface->declare(&fHslider8, "Attack duration of the nonlinearity", "");
		interface->declare(&fHslider8, "unit", "s");
		interface->addHorizontalSlider("Nonlinearity_Attack", &fHslider8, 0.1f, 0.0f, 2.0f, 0.01f);
		interface->closeBox();
		interface->openVerticalBox("Physical_Parameters");
		interface->declare(&fHslider11, "2", "");
		interface->declare(&fHslider11, "tooltip", "Breath noise gain (value between 0 and 1)");
		interface->addHorizontalSlider("Noise_Gain", &fHslider11, 0.5f, 0.0f, 1.0f, 0.01f);
		interface->declare(&fHslider1, "2", "");
		interface->declare(&fHslider1, "tooltip", "Breath pressure (value bewteen 0 and 1)");
		interface->addHorizontalSlider("Pressure", &fHslider1, 1.0f, 0.0f, 1.0f, 0.01f);
		interface->closeBox();
		interface->closeBox();
		interface->openVerticalBox("Spat");
		interface->addHorizontalSlider("pan angle", &fHslider0, 0.6f, 0.0f, 1.0f, 0.01f);
		interface->addHorizontalSlider("spatial width", &fHslider14, 0.5f, 0.0f, 1.0f, 0.01f);
		interface->closeBox();
		interface->closeBox();
		
	}
	
	virtual void compute(int count, FAUSTFLOAT** inputs, FAUSTFLOAT** outputs) {
		FAUSTFLOAT* output0 = outputs[0];
		FAUSTFLOAT* output1 = outputs[1];
		float fSlow0 = float(fHslider0);
		float fSlow1 = (0.5f * (1.0f - fSlow0));
		float fSlow2 = (0.001f * float(fEntry0));
		float fSlow3 = float(fHslider1);
		float fSlow4 = float(fButton0);
		int iSlow5 = (fSlow4 > 0.0f);
		int iSlow6 = (fSlow4 <= 0.0f);
		float fSlow7 = float(fHslider2);
		float fSlow8 = float(fHslider3);
		float fSlow9 = (1.0f / (float((fSlow8 == 0.0f)) + (fConst0 * fSlow8)));
		float fSlow10 = float(fHslider4);
		float fSlow11 = (fConst0 * fSlow10);
		float fSlow12 = (float((fSlow10 == 0.0f)) + fSlow11);
		float fSlow13 = float(fHslider5);
		float fSlow14 = (1.0f - (1.0f / powf(1e+05f, (1.0f / (float((fSlow13 == 0.0f)) + (fConst0 * fSlow13))))));
		float fSlow15 = (fConst4 * float(fHslider6));
		float fSlow16 = float(fEntry1);
		int iSlow17 = (fSlow16 >= 3.0f);
		float fSlow18 = (0.001f * float(fHslider7));
		float fSlow19 = float(fHslider8);
		float fSlow20 = (1.0f / (float((fSlow19 == 0.0f)) + (fConst0 * fSlow19)));
		float fSlow21 = float(fHslider9);
		float fSlow22 = (1.0f / (float((fSlow21 == 0.0f)) + (fConst0 * fSlow21)));
		float fSlow23 = (1.0f - (1.0f / powf(1e+05f, fSlow22)));
		int iSlow24 = (fSlow16 != 4.0f);
		float fSlow25 = (0.001f * float(fHslider10));
		float fSlow26 = float(fEntry2);
		float fSlow27 = (fSlow26 * float((fSlow16 == 4.0f)));
		int iSlow28 = (fSlow16 < 3.0f);
		float fSlow29 = (3.1415927f * float((fSlow16 == 0.0f)));
		float fSlow30 = (1.5707964f * float((fSlow16 == 1.0f)));
		float fSlow31 = (3.1415927f * float((fSlow16 == 2.0f)));
		float fSlow32 = (0.0f - (1.998f * cosf((fConst5 * fSlow26))));
		float fSlow33 = (9.313226e-10f * float(fHslider11));
		float fSlow34 = float(fHslider12);
		float fSlow35 = (fConst0 * fSlow34);
		float fSlow36 = float(fHslider13);
		float fSlow37 = (1.0f - powf(8e+01f, (1.0f / (float((fSlow36 == 0.0f)) + (fConst0 * fSlow36)))));
		float fSlow38 = (1.0f - (1.0f / powf(8e+04f, fSlow22)));
		float fSlow39 = (0.5f * fSlow0);
		int iSlow40 = (int((fConst6 * (float(fHslider14) / fSlow26))) & 4095);
		for (int i = 0; (i < count); i = (i + 1)) {
			fRec0[0] = ((0.999f * fRec0[1]) + fSlow2);
			iRec5[0] = (iSlow5 & (iRec5[1] | (fRec6[1] >= 1.0f)));
			int iTemp0 = (iSlow6 & (fRec6[1] > 0.0f));
			fRec6[0] = (((float((((iRec5[1] == 0) & iSlow5) & (fRec6[1] < 1.0f))) / ((fConst1 * fRec0[0]) + float(((0.02f * fRec0[0]) == 0.0f)))) + (fRec6[1] * ((1.0f - (fConst2 * float((iRec5[1] & (fRec6[1] > 8e+01f))))) - (float(iTemp0) * (1.0f - (1.0f / powf(8e+04f, (1.0f / ((fConst3 * fRec0[0]) + float(((0.2f * fRec0[0]) == 0.0f))))))))))) * float(((iTemp0 == 0) | (fRec6[1] >= 1e-06f))));
			iRec7[0] = (iSlow5 & (iRec7[1] | (fRec9[1] >= 1.0f)));
			iRec8[0] = (iSlow5 * (1 + iRec8[1]));
			int iTemp1 = (iSlow6 & (fRec9[1] > 0.0f));
			fRec9[0] = (((fSlow9 * float((((((iRec7[1] == 0) & iSlow5) & (fRec9[1] < 1.0f)) & (float(iRec8[1]) > fSlow11)) * (1 - (float(iRec8[1]) < fSlow12))))) + (fRec9[1] * (1.0f - (fSlow14 * float(iTemp1))))) * float(((iTemp1 == 0) | (fRec9[1] >= 1e-06f))));
			float fTemp2 = (fRec11[1] + fSlow15);
			fRec11[0] = (fTemp2 - floorf(fTemp2));
			float fTemp3 = ((fSlow3 * fRec6[0]) + (fSlow7 * (fRec9[0] * faustpower2_f(ftbl0Blow_Bottle_dspSIG0[int((65536.0f * fRec11[0]))]))));
			fRec12[0] = ((0.999f * fRec12[1]) + fSlow18);
			iRec13[0] = (iSlow5 & (iRec13[1] | (fRec14[1] >= 1.0f)));
			int iTemp4 = (iSlow6 & (fRec14[1] > 0.0f));
			fRec14[0] = (((fSlow20 * float((((iRec13[1] == 0) & iSlow5) & (fRec14[1] < 1.0f)))) + (fRec14[1] * (1.0f - (fSlow23 * float(iTemp4))))) * float(((iTemp4 == 0) | (fRec14[1] >= 1e-06f))));
			float fTemp5 = (fRec12[0] * fRec14[0]);
			fRec16[0] = ((0.999f * fRec16[1]) + fSlow25);
			float fTemp6 = (fRec15[1] + (fConst4 * ((float(iSlow24) * fRec16[0]) + fSlow27)));
			fRec15[0] = (fTemp6 - floorf(fTemp6));
			float fTemp7 = (3.1415927f * (fTemp5 * ftbl0Blow_Bottle_dspSIG0[int((65536.0f * fRec15[0]))]));
			float fTemp8 = sinf(fTemp7);
			float fTemp9 = (0.0f - fTemp8);
			float fTemp10 = cosf(fTemp7);
			float fTemp11 = ((fRec17[1] * fTemp9) + (fRec2[1] * fTemp10));
			float fTemp12 = ((fTemp9 * fRec18[1]) + (fTemp10 * fTemp11));
			float fTemp13 = ((fTemp9 * fRec19[1]) + (fTemp10 * fTemp12));
			float fTemp14 = ((fTemp9 * fRec20[1]) + (fTemp10 * fTemp13));
			float fTemp15 = ((fTemp9 * fRec21[1]) + (fTemp10 * fTemp14));
			fRec22[0] = ((fTemp9 * fRec22[1]) + (fTemp10 * fTemp15));
			fRec21[0] = ((fTemp8 * fTemp15) + (fTemp10 * fRec22[1]));
			fRec20[0] = ((fTemp8 * fTemp14) + (fTemp10 * fRec21[1]));
			fRec19[0] = ((fTemp8 * fTemp13) + (fTemp10 * fRec20[1]));
			fRec18[0] = ((fTemp8 * fTemp12) + (fTemp10 * fRec19[1]));
			fRec17[0] = ((fTemp8 * fTemp11) + (fTemp10 * fRec18[1]));
			float fTemp16 = (fTemp5 * (((fSlow29 * fRec2[1]) + (fSlow30 * (fRec2[1] + fRec2[2]))) + (fSlow31 * faustpower2_f(fRec2[1]))));
			float fTemp17 = sinf(fTemp16);
			float fTemp18 = (0.0f - fTemp17);
			float fTemp19 = cosf(fTemp16);
			float fTemp20 = ((fRec23[1] * fTemp18) + (fRec2[1] * fTemp19));
			float fTemp21 = ((fTemp18 * fRec24[1]) + (fTemp19 * fTemp20));
			float fTemp22 = ((fTemp18 * fRec25[1]) + (fTemp19 * fTemp21));
			float fTemp23 = ((fTemp18 * fRec26[1]) + (fTemp19 * fTemp22));
			float fTemp24 = ((fTemp18 * fRec27[1]) + (fTemp19 * fTemp23));
			fRec28[0] = ((fTemp18 * fRec28[1]) + (fTemp19 * fTemp24));
			fRec27[0] = ((fTemp17 * fTemp24) + (fTemp19 * fRec28[1]));
			fRec26[0] = ((fTemp17 * fTemp23) + (fTemp19 * fRec27[1]));
			fRec25[0] = ((fTemp17 * fTemp22) + (fTemp19 * fRec26[1]));
			fRec24[0] = ((fTemp17 * fTemp21) + (fTemp19 * fRec25[1]));
			fRec23[0] = ((fTemp17 * fTemp20) + (fTemp19 * fRec24[1]));
			float fTemp25 = ((float(iSlow17) * ((fRec2[1] * fTemp8) + (fRec17[1] * fTemp10))) + (float(iSlow28) * ((fRec12[0] * ((fRec2[1] * fTemp17) + (fRec23[1] * fTemp19))) + ((1.0f - fRec12[0]) * fRec2[1]))));
			float fTemp26 = (0.0f - (fTemp3 - fTemp25));
			float fTemp27 = (fTemp26 * (faustpower2_f(fTemp26) - 1.0f));
			float fTemp28 = (float((fTemp27 > 1.0f)) + (fTemp27 * float((fTemp27 <= 1.0f))));
			iRec29[0] = (12345 + (1103515245 * iRec29[1]));
			fRec4[0] = (0.0f - (((0.998001f * fRec4[2]) + ((fTemp26 * ((fTemp28 * float((fTemp28 >= -1.0f))) - float((fTemp28 < -1.0f)))) + (fSlow32 * fRec4[1]))) - (fTemp3 + (fSlow33 * ((float(iRec29[0]) * fTemp3) * (0.0f - (fTemp3 - (1.0f + fTemp25))))))));
			fRec2[0] = ((0.0009995f * fRec4[0]) - (0.0009995f * fRec4[2]));
			fRec3[0] = fTemp26;
			fRec1[0] = ((fRec3[0] + (0.995f * fRec1[1])) - fRec3[1]);
			iRec30[0] = (iSlow5 & (iRec30[1] | (fRec31[1] >= 1.0f)));
			int iTemp29 = (iSlow6 & (fRec31[1] > 0.0f));
			fRec31[0] = (((float((((iRec30[1] == 0) & iSlow5) & (fRec31[1] < 1.0f))) / ((fSlow35 * fRec0[0]) + float(((fSlow34 * fRec0[0]) == 0.0f)))) + (fRec31[1] * ((1.0f - (fSlow37 * float((iRec30[1] & (fRec31[1] > 8e+01f))))) - (fSlow38 * float(iTemp29))))) * float(((iTemp29 == 0) | (fRec31[1] >= 1e-06f))));
			float fTemp30 = ((fRec0[0] * fRec1[0]) * fRec31[0]);
			fVec0[(IOTA & 4095)] = fTemp30;
			output0[i] = FAUSTFLOAT((fSlow1 * fTemp30));
			output1[i] = FAUSTFLOAT((fSlow39 * fVec0[((IOTA - iSlow40) & 4095)]));
			fRec0[1] = fRec0[0];
			iRec5[1] = iRec5[0];
			fRec6[1] = fRec6[0];
			iRec7[1] = iRec7[0];
			iRec8[1] = iRec8[0];
			fRec9[1] = fRec9[0];
			fRec11[1] = fRec11[0];
			fRec12[1] = fRec12[0];
			iRec13[1] = iRec13[0];
			fRec14[1] = fRec14[0];
			fRec16[1] = fRec16[0];
			fRec15[1] = fRec15[0];
			fRec22[1] = fRec22[0];
			fRec21[1] = fRec21[0];
			fRec20[1] = fRec20[0];
			fRec19[1] = fRec19[0];
			fRec18[1] = fRec18[0];
			fRec17[1] = fRec17[0];
			fRec28[1] = fRec28[0];
			fRec27[1] = fRec27[0];
			fRec26[1] = fRec26[0];
			fRec25[1] = fRec25[0];
			fRec24[1] = fRec24[0];
			fRec23[1] = fRec23[0];
			iRec29[1] = iRec29[0];
			fRec4[2] = fRec4[1];
			fRec4[1] = fRec4[0];
			fRec2[2] = fRec2[1];
			fRec2[1] = fRec2[0];
			fRec3[1] = fRec3[0];
			fRec1[1] = fRec1[0];
			iRec30[1] = iRec30[0];
			fRec31[1] = fRec31[0];
			IOTA = (IOTA + 1);
			
		}
		
	}

	
};



#include "Faust_plugins_template2.cpp"


#endif
