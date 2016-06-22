/* ------------------------------------------------------------
author: "Romain Michon"
copyright: "Romain Michon (rmichon@ccrma.stanford.edu)"
name: "Bowed"
version: "1.0"
Code generated with Faust 2.0.a43 (http://faust.grame.fr)
------------------------------------------------------------ */

#ifndef  __Bowed_dsp_H__
#define  __Bowed_dsp_H__

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


class Bowed_dspSIG0 {
	
  private:
	
	int iRec7[2];
	
  public:
	
	int getNumInputsBowed_dspSIG0() {
		return 0;
		
	}
	int getNumOutputsBowed_dspSIG0() {
		return 1;
		
	}
	int getInputRateBowed_dspSIG0(int channel) {
		int rate;
		switch (channel) {
			default: {
				rate = -1;
				break;
			}
			
		}
		return rate;
		
	}
	int getOutputRateBowed_dspSIG0(int channel) {
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
	
	void instanceInitBowed_dspSIG0(int samplingFreq) {
		for (int i3 = 0; (i3 < 2); i3 = (i3 + 1)) {
			iRec7[i3] = 0;
			
		}
		
	}
	
	void fillBowed_dspSIG0(int count, float* output) {
		for (int i = 0; (i < count); i = (i + 1)) {
			iRec7[0] = (1 + iRec7[1]);
			output[i] = sinf((9.58738e-05f * float((iRec7[0] - 1))));
			iRec7[1] = iRec7[0];
			
		}
		
	}
};

Bowed_dspSIG0* newBowed_dspSIG0() { return (Bowed_dspSIG0*)new Bowed_dspSIG0(); }
void deleteBowed_dspSIG0(Bowed_dspSIG0* dsp) { delete dsp; }

static float ftbl0Bowed_dspSIG0[65536];
static float faustpower2_f(float value) {
	return (value * value);
	
}
static float faustpower4_f(float value) {
	return (((value * value) * value) * value);
	
}

#ifndef FAUSTCLASS 
#define FAUSTCLASS Bowed_dsp
#endif

class Bowed_dsp : public dsp {
	
  private:
	
	float fRec2[8192];
	float fRec1[8192];
	float fVec0[4096];
	float fRec0[3];
	int iRec4[2];
	int iRec5[2];
	float fRec6[2];
	float fRec8[2];
	float fRec10[2];
	int iRec11[2];
	float fRec12[2];
	float fRec14[2];
	float fRec13[2];
	float fRec20[2];
	float fRec19[2];
	float fRec18[2];
	float fRec17[2];
	float fRec16[2];
	float fRec15[2];
	float fRec26[2];
	float fRec25[2];
	float fRec24[2];
	float fRec23[2];
	float fRec22[2];
	float fRec21[2];
	float fRec9[2];
	int iRec27[2];
	float fRec28[2];
	FAUSTFLOAT fEntry0;
	FAUSTFLOAT fHslider0;
	int fSamplingFreq;
	float fConst0;
	FAUSTFLOAT fEntry1;
	FAUSTFLOAT fHslider1;
	FAUSTFLOAT fButton0;
	FAUSTFLOAT fHslider2;
	FAUSTFLOAT fHslider3;
	FAUSTFLOAT fHslider4;
	float fConst1;
	FAUSTFLOAT fHslider5;
	FAUSTFLOAT fHslider6;
	float fConst2;
	float fConst3;
	FAUSTFLOAT fEntry2;
	FAUSTFLOAT fHslider7;
	FAUSTFLOAT fHslider8;
	FAUSTFLOAT fHslider9;
	FAUSTFLOAT fHslider10;
	float fConst4;
	FAUSTFLOAT fHslider11;
	FAUSTFLOAT fHslider12;
	FAUSTFLOAT fHslider13;
	int IOTA;
	float fConst5;
	float fConst6;
	FAUSTFLOAT fHslider14;
	
  public:
	
	void static metadata(Meta* m) { 
		m->declare("author", "Romain Michon");
		m->declare("copyright", "Romain Michon (rmichon@ccrma.stanford.edu)");
		m->declare("description", "Nonlinear WaveGuide Bowed Instrument");
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
		m->declare("name", "Bowed");
		m->declare("reference", "https://ccrma.stanford.edu/~jos/pasp/Bowed_Strings.html");
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
		Bowed_dspSIG0* sig0 = newBowed_dspSIG0();
		sig0->instanceInitBowed_dspSIG0(samplingFreq);
		sig0->fillBowed_dspSIG0(65536, ftbl0Bowed_dspSIG0);
		deleteBowed_dspSIG0(sig0);
		
	}
	
	virtual void instanceInit(int samplingFreq) {
		fSamplingFreq = samplingFreq;
		fEntry0 = FAUSTFLOAT(1.0f);
		fHslider0 = FAUSTFLOAT(0.6f);
		fConst0 = min(1.92e+05f, max(1.0f, float(fSamplingFreq)));
		fEntry1 = FAUSTFLOAT(4.4e+02f);
		fHslider1 = FAUSTFLOAT(0.01f);
		fButton0 = FAUSTFLOAT(0.0f);
		for (int i0 = 0; (i0 < 2); i0 = (i0 + 1)) {
			iRec4[i0] = 0;
			
		}
		for (int i1 = 0; (i1 < 2); i1 = (i1 + 1)) {
			iRec5[i1] = 0;
			
		}
		fHslider2 = FAUSTFLOAT(0.5f);
		fHslider3 = FAUSTFLOAT(0.05f);
		fHslider4 = FAUSTFLOAT(0.01f);
		for (int i2 = 0; (i2 < 2); i2 = (i2 + 1)) {
			fRec6[i2] = 0.0f;
			
		}
		fConst1 = (1.0f / fConst0);
		fHslider5 = FAUSTFLOAT(6.0f);
		for (int i4 = 0; (i4 < 2); i4 = (i4 + 1)) {
			fRec8[i4] = 0.0f;
			
		}
		fHslider6 = FAUSTFLOAT(0.7f);
		fConst2 = (2205.0f / fConst0);
		fConst3 = (0.95f * (0.4f + fConst2));
		fEntry2 = FAUSTFLOAT(0.0f);
		fHslider7 = FAUSTFLOAT(0.0f);
		for (int i5 = 0; (i5 < 2); i5 = (i5 + 1)) {
			fRec10[i5] = 0.0f;
			
		}
		for (int i6 = 0; (i6 < 2); i6 = (i6 + 1)) {
			iRec11[i6] = 0;
			
		}
		fHslider8 = FAUSTFLOAT(0.1f);
		fHslider9 = FAUSTFLOAT(0.1f);
		for (int i7 = 0; (i7 < 2); i7 = (i7 + 1)) {
			fRec12[i7] = 0.0f;
			
		}
		fHslider10 = FAUSTFLOAT(2.2e+02f);
		for (int i8 = 0; (i8 < 2); i8 = (i8 + 1)) {
			fRec14[i8] = 0.0f;
			
		}
		for (int i9 = 0; (i9 < 2); i9 = (i9 + 1)) {
			fRec13[i9] = 0.0f;
			
		}
		for (int i10 = 0; (i10 < 2); i10 = (i10 + 1)) {
			fRec20[i10] = 0.0f;
			
		}
		for (int i11 = 0; (i11 < 2); i11 = (i11 + 1)) {
			fRec19[i11] = 0.0f;
			
		}
		for (int i12 = 0; (i12 < 2); i12 = (i12 + 1)) {
			fRec18[i12] = 0.0f;
			
		}
		for (int i13 = 0; (i13 < 2); i13 = (i13 + 1)) {
			fRec17[i13] = 0.0f;
			
		}
		for (int i14 = 0; (i14 < 2); i14 = (i14 + 1)) {
			fRec16[i14] = 0.0f;
			
		}
		for (int i15 = 0; (i15 < 2); i15 = (i15 + 1)) {
			fRec15[i15] = 0.0f;
			
		}
		for (int i16 = 0; (i16 < 2); i16 = (i16 + 1)) {
			fRec26[i16] = 0.0f;
			
		}
		for (int i17 = 0; (i17 < 2); i17 = (i17 + 1)) {
			fRec25[i17] = 0.0f;
			
		}
		for (int i18 = 0; (i18 < 2); i18 = (i18 + 1)) {
			fRec24[i18] = 0.0f;
			
		}
		for (int i19 = 0; (i19 < 2); i19 = (i19 + 1)) {
			fRec23[i19] = 0.0f;
			
		}
		for (int i20 = 0; (i20 < 2); i20 = (i20 + 1)) {
			fRec22[i20] = 0.0f;
			
		}
		for (int i21 = 0; (i21 < 2); i21 = (i21 + 1)) {
			fRec21[i21] = 0.0f;
			
		}
		fConst4 = (fConst2 - 0.6f);
		for (int i22 = 0; (i22 < 2); i22 = (i22 + 1)) {
			fRec9[i22] = 0.0f;
			
		}
		for (int i23 = 0; (i23 < 2); i23 = (i23 + 1)) {
			iRec27[i23] = 0;
			
		}
		fHslider11 = FAUSTFLOAT(0.01f);
		fHslider12 = FAUSTFLOAT(0.05f);
		for (int i24 = 0; (i24 < 2); i24 = (i24 + 1)) {
			fRec28[i24] = 0.0f;
			
		}
		fHslider13 = FAUSTFLOAT(0.75f);
		IOTA = 0;
		for (int i25 = 0; (i25 < 8192); i25 = (i25 + 1)) {
			fRec2[i25] = 0.0f;
			
		}
		for (int i26 = 0; (i26 < 8192); i26 = (i26 + 1)) {
			fRec1[i26] = 0.0f;
			
		}
		fConst5 = (0.0f - (1.7f * cosf((3141.5928f / fConst0))));
		for (int i27 = 0; (i27 < 3); i27 = (i27 + 1)) {
			fRec0[i27] = 0.0f;
			
		}
		for (int i28 = 0; (i28 < 4096); i28 = (i28 + 1)) {
			fVec0[i28] = 0.0f;
			
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
		interface->declare(&fEntry1, "1", "");
		interface->declare(&fEntry1, "tooltip", "Tone frequency");
		interface->declare(&fEntry1, "unit", "Hz");
		interface->addNumEntry("freq", &fEntry1, 4.4e+02f, 2e+01f, 2e+04f, 1.0f);
		interface->declare(&fEntry0, "1", "");
		interface->declare(&fEntry0, "tooltip", "Gain (value between 0 and 1)");
		interface->addNumEntry("gain", &fEntry0, 1.0f, 0.0f, 1.0f, 0.01f);
		interface->declare(&fButton0, "1", "");
		interface->declare(&fButton0, "tooltip", "noteOn = 1, noteOff = 0");
		interface->addButton("gate",&fButton0);
		interface->closeBox();
		interface->openHorizontalBox("Envelopes_and_Vibrato");
		interface->openVerticalBox("Envelope_Parameters");
		interface->declare(&fHslider11, "5", "");
		interface->declare(&fHslider11, "tooltip", "Envelope attack duration");
		interface->declare(&fHslider11, "unit", "s");
		interface->addHorizontalSlider("Envelope_Attack", &fHslider11, 0.01f, 0.0f, 2.0f, 0.01f);
		interface->declare(&fHslider12, "5", "");
		interface->declare(&fHslider12, "tooltip", "Envelope decay duration");
		interface->declare(&fHslider12, "unit", "s");
		interface->addHorizontalSlider("Envelope_Decay", &fHslider12, 0.05f, 0.0f, 2.0f, 0.01f);
		interface->declare(&fHslider9, "5", "");
		interface->declare(&fHslider9, "tooltip", "Envelope release duration");
		interface->declare(&fHslider9, "unit", "s");
		interface->addHorizontalSlider("Envelope_Release", &fHslider9, 0.1f, 0.0f, 2.0f, 0.01f);
		interface->closeBox();
		interface->openVerticalBox("Vibrato_Parameters");
		interface->declare(&fHslider2, "4", "");
		interface->declare(&fHslider2, "tooltip", "Vibrato attack duration");
		interface->declare(&fHslider2, "unit", "s");
		interface->addHorizontalSlider("Vibrato_Attack", &fHslider2, 0.5f, 0.0f, 2.0f, 0.01f);
		interface->declare(&fHslider3, "4", "");
		interface->declare(&fHslider3, "tooltip", "Vibrato silence duration before attack");
		interface->declare(&fHslider3, "unit", "s");
		interface->addHorizontalSlider("Vibrato_Begin", &fHslider3, 0.05f, 0.0f, 2.0f, 0.01f);
		interface->declare(&fHslider5, "4", "");
		interface->declare(&fHslider5, "unit", "Hz");
		interface->addHorizontalSlider("Vibrato_Freq", &fHslider5, 6.0f, 1.0f, 15.0f, 0.1f);
		interface->declare(&fHslider1, "4", "");
		interface->declare(&fHslider1, "tooltip", "A value between 0 and 1");
		interface->addHorizontalSlider("Vibrato_Gain", &fHslider1, 0.01f, 0.0f, 1.0f, 0.01f);
		interface->declare(&fHslider4, "4", "");
		interface->declare(&fHslider4, "tooltip", "Vibrato release duration");
		interface->declare(&fHslider4, "unit", "s");
		interface->addHorizontalSlider("Vibrato_Release", &fHslider4, 0.01f, 0.0f, 2.0f, 0.01f);
		interface->closeBox();
		interface->closeBox();
		interface->openHorizontalBox("Physical_and_Nonlinearity");
		interface->openVerticalBox("Nonlinear_Filter_Parameters");
		interface->declare(&fHslider10, "3", "");
		interface->declare(&fHslider10, "tooltip", "Frequency of the sine wave for the modulation of theta (works if Modulation Type=3)");
		interface->declare(&fHslider10, "unit", "Hz");
		interface->addHorizontalSlider("Modulation_Frequency", &fHslider10, 2.2e+02f, 2e+01f, 1e+03f, 0.1f);
		interface->declare(&fEntry2, "3", "");
		interface->declare(&fEntry2, "tooltip", "0=theta is modulated by the incoming signal; 1=theta is modulated by the averaged incoming signal; 2=theta is modulated by the squared incoming signal; 3=theta is modulated by a sine wave of frequency freqMod; 4=theta is modulated by a sine wave of frequency freq;");
		interface->addNumEntry("Modulation_Type", &fEntry2, 0.0f, 0.0f, 4.0f, 1.0f);
		interface->declare(&fHslider7, "3", "");
		interface->declare(&fHslider7, "tooltip", "Nonlinearity factor (value between 0 and 1)");
		interface->addHorizontalSlider("Nonlinearity", &fHslider7, 0.0f, 0.0f, 1.0f, 0.01f);
		interface->declare(&fHslider8, "3", "");
		interface->declare(&fHslider8, "Attack duration of the nonlinearity", "");
		interface->declare(&fHslider8, "unit", "s");
		interface->addHorizontalSlider("Nonlinearity_Attack", &fHslider8, 0.1f, 0.0f, 2.0f, 0.01f);
		interface->closeBox();
		interface->openVerticalBox("Physical_Parameters");
		interface->declare(&fHslider6, "2", "");
		interface->declare(&fHslider6, "tooltip", "Bow position along the string (value between 0 and 1)");
		interface->addHorizontalSlider("Bow_Position", &fHslider6, 0.7f, 0.01f, 1.0f, 0.01f);
		interface->declare(&fHslider13, "2", "");
		interface->declare(&fHslider13, "tooltip", "Bow pressure on the string (value between 0 and 1)");
		interface->addHorizontalSlider("Bow_Pressure", &fHslider13, 0.75f, 0.0f, 1.0f, 0.01f);
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
		float fSlow0 = float(fEntry0);
		float fSlow1 = float(fHslider0);
		float fSlow2 = (8.0f * (fSlow0 * (1.0f - fSlow1)));
		float fSlow3 = float(fEntry1);
		float fSlow4 = ((fConst0 / fSlow3) - 4.0f);
		float fSlow5 = float(fHslider1);
		float fSlow6 = float(fButton0);
		int iSlow7 = (fSlow6 > 0.0f);
		float fSlow8 = float(fHslider2);
		float fSlow9 = (1.0f / (float((fSlow8 == 0.0f)) + (fConst0 * fSlow8)));
		float fSlow10 = float(fHslider3);
		float fSlow11 = (fConst0 * fSlow10);
		float fSlow12 = (float((fSlow10 == 0.0f)) + fSlow11);
		float fSlow13 = float(fHslider4);
		float fSlow14 = (1.0f - (1.0f / powf(1e+05f, (1.0f / (float((fSlow13 == 0.0f)) + (fConst0 * fSlow13))))));
		int iSlow15 = (fSlow6 <= 0.0f);
		float fSlow16 = (fConst1 * float(fHslider5));
		float fSlow17 = (0.2f * float(fHslider6));
		float fSlow18 = (0.972764f - fSlow17);
		float fSlow19 = float(fEntry2);
		int iSlow20 = (fSlow19 >= 3.0f);
		int iSlow21 = (1 + (int(((0.027236f + fSlow17) * fSlow4)) & 4095));
		float fSlow22 = (0.001f * float(fHslider7));
		float fSlow23 = float(fHslider8);
		float fSlow24 = (1.0f / (float((fSlow23 == 0.0f)) + (fConst0 * fSlow23)));
		float fSlow25 = float(fHslider9);
		float fSlow26 = (1.0f - (1.0f / powf(1e+05f, (1.0f / (float((fSlow25 == 0.0f)) + (fConst0 * fSlow25))))));
		int iSlow27 = (fSlow19 != 4.0f);
		float fSlow28 = (0.001f * float(fHslider10));
		float fSlow29 = (fSlow3 * float((fSlow19 == 4.0f)));
		int iSlow30 = (fSlow19 < 3.0f);
		float fSlow31 = (3.1415927f * float((fSlow19 == 0.0f)));
		float fSlow32 = (1.5707964f * float((fSlow19 == 1.0f)));
		int iSlow33 = (1 + iSlow21);
		float fSlow34 = (3.1415927f * float((fSlow19 == 2.0f)));
		float fSlow35 = (0.03f + (0.2f * fSlow0));
		float fSlow36 = (float(fHslider11) * fSlow0);
		float fSlow37 = (1.0f / ((fConst0 * fSlow36) + float((fSlow36 == 0.0f))));
		float fSlow38 = float(fHslider12);
		float fSlow39 = (1.0f - powf(9e+01f, (1.0f / (float((fSlow38 == 0.0f)) + (fConst0 * fSlow38)))));
		float fSlow40 = (fSlow25 * (1.0f - fSlow0));
		float fSlow41 = (1.0f - (1.0f / powf(9e+04f, (1.0f / ((fConst0 * fSlow40) + float((fSlow40 == 0.0f)))))));
		float fSlow42 = (5.0f - (4.0f * float(fHslider13)));
		float fSlow43 = (8.0f * fSlow0);
		int iSlow44 = (int((fConst6 * (float(fHslider14) / fSlow3))) & 4095);
		for (int i = 0; (i < count); i = (i + 1)) {
			iRec4[0] = (iSlow7 & (iRec4[1] | (fRec6[1] >= 1.0f)));
			iRec5[0] = (iSlow7 * (1 + iRec5[1]));
			int iTemp0 = (iSlow15 & (fRec6[1] > 0.0f));
			fRec6[0] = (((fSlow9 * float((((((iRec4[1] == 0) & iSlow7) & (fRec6[1] < 1.0f)) & (float(iRec5[1]) > fSlow11)) * (1 - (float(iRec5[1]) < fSlow12))))) + (fRec6[1] * (1.0f - (fSlow14 * float(iTemp0))))) * float(((iTemp0 == 0) | (fRec6[1] >= 1e-06f))));
			float fTemp1 = (fRec8[1] + fSlow16);
			fRec8[0] = (fTemp1 - floorf(fTemp1));
			float fTemp2 = (fSlow4 * ((fSlow5 * (fRec6[0] * ftbl0Bowed_dspSIG0[int((65536.0f * fRec8[0]))])) + fSlow18));
			int iTemp3 = int(fTemp2);
			int iTemp4 = (1 + iTemp3);
			float fTemp5 = ((fTemp2 - float(iTemp3)) * fRec2[((IOTA - (1 + (iTemp4 & 4095))) & 8191)]);
			float fTemp6 = (fRec2[((IOTA - (1 + (iTemp3 & 4095))) & 8191)] * (float(iTemp4) - fTemp2));
			fRec10[0] = ((0.999f * fRec10[1]) + fSlow22);
			iRec11[0] = (iSlow7 & (iRec11[1] | (fRec12[1] >= 1.0f)));
			int iTemp7 = (iSlow15 & (fRec12[1] > 0.0f));
			fRec12[0] = (((fSlow24 * float((((iRec11[1] == 0) & iSlow7) & (fRec12[1] < 1.0f)))) + (fRec12[1] * (1.0f - (fSlow26 * float(iTemp7))))) * float(((iTemp7 == 0) | (fRec12[1] >= 1e-06f))));
			float fTemp8 = (fRec10[0] * fRec12[0]);
			fRec14[0] = ((0.999f * fRec14[1]) + fSlow28);
			float fTemp9 = (fRec13[1] + (fConst1 * ((float(iSlow27) * fRec14[0]) + fSlow29)));
			fRec13[0] = (fTemp9 - floorf(fTemp9));
			float fTemp10 = (3.1415927f * (fTemp8 * ftbl0Bowed_dspSIG0[int((65536.0f * fRec13[0]))]));
			float fTemp11 = sinf(fTemp10);
			float fTemp12 = (0.0f - fTemp11);
			float fTemp13 = cosf(fTemp10);
			float fTemp14 = ((fRec15[1] * fTemp12) + (fRec1[((IOTA - iSlow21) & 8191)] * fTemp13));
			float fTemp15 = ((fTemp12 * fRec16[1]) + (fTemp13 * fTemp14));
			float fTemp16 = ((fTemp12 * fRec17[1]) + (fTemp13 * fTemp15));
			float fTemp17 = ((fTemp12 * fRec18[1]) + (fTemp13 * fTemp16));
			float fTemp18 = ((fTemp12 * fRec19[1]) + (fTemp13 * fTemp17));
			fRec20[0] = ((fTemp12 * fRec20[1]) + (fTemp13 * fTemp18));
			fRec19[0] = ((fTemp11 * fTemp18) + (fTemp13 * fRec20[1]));
			fRec18[0] = ((fTemp11 * fTemp17) + (fTemp13 * fRec19[1]));
			fRec17[0] = ((fTemp11 * fTemp16) + (fTemp13 * fRec18[1]));
			fRec16[0] = ((fTemp11 * fTemp15) + (fTemp13 * fRec17[1]));
			fRec15[0] = ((fTemp11 * fTemp14) + (fTemp13 * fRec16[1]));
			float fTemp19 = (fTemp8 * (((fSlow31 * fRec1[((IOTA - iSlow21) & 8191)]) + (fSlow32 * (fRec1[((IOTA - iSlow21) & 8191)] + fRec1[((IOTA - iSlow33) & 8191)]))) + (fSlow34 * faustpower2_f(fRec1[((IOTA - iSlow21) & 8191)]))));
			float fTemp20 = sinf(fTemp19);
			float fTemp21 = (0.0f - fTemp20);
			float fTemp22 = cosf(fTemp19);
			float fTemp23 = ((fRec21[1] * fTemp21) + (fRec1[((IOTA - iSlow21) & 8191)] * fTemp22));
			float fTemp24 = ((fTemp21 * fRec22[1]) + (fTemp22 * fTemp23));
			float fTemp25 = ((fTemp21 * fRec23[1]) + (fTemp22 * fTemp24));
			float fTemp26 = ((fTemp21 * fRec24[1]) + (fTemp22 * fTemp25));
			float fTemp27 = ((fTemp21 * fRec25[1]) + (fTemp22 * fTemp26));
			fRec26[0] = ((fTemp21 * fRec26[1]) + (fTemp22 * fTemp27));
			fRec25[0] = ((fTemp20 * fTemp27) + (fTemp22 * fRec26[1]));
			fRec24[0] = ((fTemp20 * fTemp26) + (fTemp22 * fRec25[1]));
			fRec23[0] = ((fTemp20 * fTemp25) + (fTemp22 * fRec24[1]));
			fRec22[0] = ((fTemp20 * fTemp24) + (fTemp22 * fRec23[1]));
			fRec21[0] = ((fTemp20 * fTemp23) + (fTemp22 * fRec22[1]));
			fRec9[0] = ((fConst3 * ((float(iSlow20) * ((fRec1[((IOTA - iSlow21) & 8191)] * fTemp11) + (fRec15[1] * fTemp13))) + (float(iSlow30) * ((fRec10[0] * ((fRec1[((IOTA - iSlow21) & 8191)] * fTemp20) + (fRec21[1] * fTemp22))) + ((1.0f - fRec10[0]) * fRec1[((IOTA - iSlow21) & 8191)]))))) - (fConst4 * fRec9[1]));
			iRec27[0] = (iSlow7 & (iRec27[1] | (fRec28[1] >= 1.0f)));
			int iTemp28 = (iSlow15 & (fRec28[1] > 0.0f));
			fRec28[0] = (((fSlow37 * float((((iRec27[1] == 0) & iSlow7) & (fRec28[1] < 1.0f)))) + (fRec28[1] * ((1.0f - (fSlow39 * float((iRec27[1] & (fRec28[1] > 9e+01f))))) - (fSlow41 * float(iTemp28))))) * float(((iTemp28 == 0) | (fRec28[1] >= 1e-06f))));
			float fTemp29 = (fTemp5 + (fTemp6 + (fRec9[0] + (fSlow35 * fRec28[0]))));
			float fTemp30 = faustpower4_f((0.75f + fabsf((fSlow42 * fTemp29))));
			float fTemp31 = (1.0f / fTemp30);
			float fTemp32 = (fTemp29 * (float((fTemp31 > 1.0f)) + (float((fTemp31 <= 1.0f)) / fTemp30)));
			fRec2[(IOTA & 8191)] = (fTemp32 - fRec9[0]);
			float fRec3 = (0.0f - ((fTemp6 + fTemp5) - fTemp32));
			fRec1[(IOTA & 8191)] = fRec3;
			fRec0[0] = ((0.2f * fRec1[((IOTA - 0) & 8191)]) - ((fConst5 * fRec0[1]) + (0.7225f * fRec0[2])));
			float fTemp33 = ((0.13875f * fRec0[0]) - (0.13875f * fRec0[2]));
			output0[i] = FAUSTFLOAT((fSlow2 * fTemp33));
			fVec0[(IOTA & 4095)] = (fSlow43 * fTemp33);
			output1[i] = FAUSTFLOAT((fSlow1 * fVec0[((IOTA - iSlow44) & 4095)]));
			iRec4[1] = iRec4[0];
			iRec5[1] = iRec5[0];
			fRec6[1] = fRec6[0];
			fRec8[1] = fRec8[0];
			fRec10[1] = fRec10[0];
			iRec11[1] = iRec11[0];
			fRec12[1] = fRec12[0];
			fRec14[1] = fRec14[0];
			fRec13[1] = fRec13[0];
			fRec20[1] = fRec20[0];
			fRec19[1] = fRec19[0];
			fRec18[1] = fRec18[0];
			fRec17[1] = fRec17[0];
			fRec16[1] = fRec16[0];
			fRec15[1] = fRec15[0];
			fRec26[1] = fRec26[0];
			fRec25[1] = fRec25[0];
			fRec24[1] = fRec24[0];
			fRec23[1] = fRec23[0];
			fRec22[1] = fRec22[0];
			fRec21[1] = fRec21[0];
			fRec9[1] = fRec9[0];
			iRec27[1] = iRec27[0];
			fRec28[1] = fRec28[0];
			IOTA = (IOTA + 1);
			fRec0[2] = fRec0[1];
			fRec0[1] = fRec0[0];
			
		}
		
	}

	
};



#include "Faust_plugins_template2.cpp"


#endif
