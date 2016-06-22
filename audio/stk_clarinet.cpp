/* ------------------------------------------------------------
author: "Romain Michon"
copyright: "Romain Michon (rmichon@ccrma.stanford.edu)"
name: "Clarinet"
version: "1.0"
Code generated with Faust 2.0.a43 (http://faust.grame.fr)
------------------------------------------------------------ */

#ifndef  __Clarinet_dsp_H__
#define  __Clarinet_dsp_H__

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


class Clarinet_dspSIG0 {
	
  private:
	
	int iRec7[2];
	
  public:
	
	int getNumInputsClarinet_dspSIG0() {
		return 0;
		
	}
	int getNumOutputsClarinet_dspSIG0() {
		return 1;
		
	}
	int getInputRateClarinet_dspSIG0(int channel) {
		int rate;
		switch (channel) {
			default: {
				rate = -1;
				break;
			}
			
		}
		return rate;
		
	}
	int getOutputRateClarinet_dspSIG0(int channel) {
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
	
	void instanceInitClarinet_dspSIG0(int samplingFreq) {
		for (int i6 = 0; (i6 < 2); i6 = (i6 + 1)) {
			iRec7[i6] = 0;
			
		}
		
	}
	
	void fillClarinet_dspSIG0(int count, float* output) {
		for (int i = 0; (i < count); i = (i + 1)) {
			iRec7[0] = (1 + iRec7[1]);
			output[i] = sinf((9.58738e-05f * float((iRec7[0] - 1))));
			iRec7[1] = iRec7[0];
			
		}
		
	}
};

Clarinet_dspSIG0* newClarinet_dspSIG0() { return (Clarinet_dspSIG0*)new Clarinet_dspSIG0(); }
void deleteClarinet_dspSIG0(Clarinet_dspSIG0* dsp) { delete dsp; }

static float ftbl0Clarinet_dspSIG0[65536];
static float faustpower2_f(float value) {
	return (value * value);
	
}

#ifndef FAUSTCLASS 
#define FAUSTCLASS Clarinet_dsp
#endif

class Clarinet_dsp : public dsp {
	
  private:
	
	float fRec0[8192];
	float fVec1[4096];
	int iRec1[2];
	float fRec2[2];
	int iRec3[2];
	int iRec4[2];
	int iRec5[2];
	float fRec6[2];
	float fRec8[2];
	float fVec0[2];
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
	float fRec27[2];
	FAUSTFLOAT fHslider0;
	FAUSTFLOAT fHslider1;
	FAUSTFLOAT fButton0;
	FAUSTFLOAT fHslider2;
	int fSamplingFreq;
	float fConst0;
	FAUSTFLOAT fHslider3;
	FAUSTFLOAT fHslider4;
	FAUSTFLOAT fHslider5;
	FAUSTFLOAT fHslider6;
	float fConst1;
	FAUSTFLOAT fHslider7;
	float fConst2;
	FAUSTFLOAT fHslider8;
	float fConst3;
	FAUSTFLOAT fHslider9;
	FAUSTFLOAT fEntry0;
	FAUSTFLOAT fHslider10;
	float fConst4;
	FAUSTFLOAT fEntry1;
	FAUSTFLOAT fHslider11;
	FAUSTFLOAT fHslider12;
	FAUSTFLOAT fHslider13;
	int IOTA;
	FAUSTFLOAT fEntry2;
	FAUSTFLOAT fHslider14;
	
  public:
	
	void static metadata(Meta* m) { 
		m->declare("author", "Romain Michon");
		m->declare("copyright", "Romain Michon (rmichon@ccrma.stanford.edu)");
		m->declare("description", "Nonlinear WaveGuide Clarinet");
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
		m->declare("name", "Clarinet");
		m->declare("reference", "https://ccrma.stanford.edu/~jos/pasp/Woodwinds.html");
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
		Clarinet_dspSIG0* sig0 = newClarinet_dspSIG0();
		sig0->instanceInitClarinet_dspSIG0(samplingFreq);
		sig0->fillClarinet_dspSIG0(65536, ftbl0Clarinet_dspSIG0);
		deleteClarinet_dspSIG0(sig0);
		
	}
	
	virtual void instanceInit(int samplingFreq) {
		fSamplingFreq = samplingFreq;
		fHslider0 = FAUSTFLOAT(0.6f);
		fHslider1 = FAUSTFLOAT(1.0f);
		fButton0 = FAUSTFLOAT(0.0f);
		for (int i0 = 0; (i0 < 2); i0 = (i0 + 1)) {
			iRec1[i0] = 0;
			
		}
		fHslider2 = FAUSTFLOAT(0.01f);
		fConst0 = min(1.92e+05f, max(1.0f, float(fSamplingFreq)));
		fHslider3 = FAUSTFLOAT(0.05f);
		fHslider4 = FAUSTFLOAT(0.1f);
		for (int i1 = 0; (i1 < 2); i1 = (i1 + 1)) {
			fRec2[i1] = 0.0f;
			
		}
		fHslider5 = FAUSTFLOAT(0.0f);
		for (int i2 = 0; (i2 < 2); i2 = (i2 + 1)) {
			iRec3[i2] = 0;
			
		}
		fHslider6 = FAUSTFLOAT(0.1f);
		for (int i3 = 0; (i3 < 2); i3 = (i3 + 1)) {
			iRec4[i3] = 0;
			
		}
		for (int i4 = 0; (i4 < 2); i4 = (i4 + 1)) {
			iRec5[i4] = 0;
			
		}
		fConst1 = (1.8f * fConst0);
		fHslider7 = FAUSTFLOAT(0.5f);
		fConst2 = (0.2f * fConst0);
		fHslider8 = FAUSTFLOAT(0.01f);
		for (int i5 = 0; (i5 < 2); i5 = (i5 + 1)) {
			fRec6[i5] = 0.0f;
			
		}
		fConst3 = (1.0f / fConst0);
		fHslider9 = FAUSTFLOAT(5.0f);
		for (int i7 = 0; (i7 < 2); i7 = (i7 + 1)) {
			fRec8[i7] = 0.0f;
			
		}
		fEntry0 = FAUSTFLOAT(0.0f);
		fHslider10 = FAUSTFLOAT(0.0f);
		fConst4 = (0.5f * fConst0);
		fEntry1 = FAUSTFLOAT(4.4e+02f);
		for (int i8 = 0; (i8 < 2); i8 = (i8 + 1)) {
			fVec0[i8] = 0.0f;
			
		}
		for (int i9 = 0; (i9 < 2); i9 = (i9 + 1)) {
			fRec10[i9] = 0.0f;
			
		}
		for (int i10 = 0; (i10 < 2); i10 = (i10 + 1)) {
			iRec11[i10] = 0;
			
		}
		fHslider11 = FAUSTFLOAT(0.1f);
		for (int i11 = 0; (i11 < 2); i11 = (i11 + 1)) {
			fRec12[i11] = 0.0f;
			
		}
		fHslider12 = FAUSTFLOAT(2.2e+02f);
		for (int i12 = 0; (i12 < 2); i12 = (i12 + 1)) {
			fRec14[i12] = 0.0f;
			
		}
		for (int i13 = 0; (i13 < 2); i13 = (i13 + 1)) {
			fRec13[i13] = 0.0f;
			
		}
		for (int i14 = 0; (i14 < 2); i14 = (i14 + 1)) {
			fRec20[i14] = 0.0f;
			
		}
		for (int i15 = 0; (i15 < 2); i15 = (i15 + 1)) {
			fRec19[i15] = 0.0f;
			
		}
		for (int i16 = 0; (i16 < 2); i16 = (i16 + 1)) {
			fRec18[i16] = 0.0f;
			
		}
		for (int i17 = 0; (i17 < 2); i17 = (i17 + 1)) {
			fRec17[i17] = 0.0f;
			
		}
		for (int i18 = 0; (i18 < 2); i18 = (i18 + 1)) {
			fRec16[i18] = 0.0f;
			
		}
		for (int i19 = 0; (i19 < 2); i19 = (i19 + 1)) {
			fRec15[i19] = 0.0f;
			
		}
		for (int i20 = 0; (i20 < 2); i20 = (i20 + 1)) {
			fRec26[i20] = 0.0f;
			
		}
		for (int i21 = 0; (i21 < 2); i21 = (i21 + 1)) {
			fRec25[i21] = 0.0f;
			
		}
		for (int i22 = 0; (i22 < 2); i22 = (i22 + 1)) {
			fRec24[i22] = 0.0f;
			
		}
		for (int i23 = 0; (i23 < 2); i23 = (i23 + 1)) {
			fRec23[i23] = 0.0f;
			
		}
		for (int i24 = 0; (i24 < 2); i24 = (i24 + 1)) {
			fRec22[i24] = 0.0f;
			
		}
		for (int i25 = 0; (i25 < 2); i25 = (i25 + 1)) {
			fRec21[i25] = 0.0f;
			
		}
		for (int i26 = 0; (i26 < 2); i26 = (i26 + 1)) {
			fRec9[i26] = 0.0f;
			
		}
		fHslider13 = FAUSTFLOAT(0.5f);
		IOTA = 0;
		for (int i27 = 0; (i27 < 8192); i27 = (i27 + 1)) {
			fRec0[i27] = 0.0f;
			
		}
		fEntry2 = FAUSTFLOAT(1.0f);
		for (int i28 = 0; (i28 < 2); i28 = (i28 + 1)) {
			fRec27[i28] = 0.0f;
			
		}
		for (int i29 = 0; (i29 < 4096); i29 = (i29 + 1)) {
			fVec1[i29] = 0.0f;
			
		}
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
		interface->declare(&fEntry2, "1", "");
		interface->declare(&fEntry2, "tooltip", "Gain (value between 0 and 1)");
		interface->addNumEntry("gain", &fEntry2, 1.0f, 0.0f, 1.0f, 0.01f);
		interface->declare(&fButton0, "1", "");
		interface->declare(&fButton0, "tooltip", "noteOn = 1, noteOff = 0");
		interface->addButton("gate",&fButton0);
		interface->closeBox();
		interface->openHorizontalBox("Envelopes_and_Vibrato");
		interface->openVerticalBox("Envelope_Parameters");
		interface->declare(&fHslider2, "5", "");
		interface->declare(&fHslider2, "tooltip", "Envelope attack duration");
		interface->declare(&fHslider2, "unit", "s");
		interface->addHorizontalSlider("Envelope_Attack", &fHslider2, 0.01f, 0.0f, 2.0f, 0.01f);
		interface->declare(&fHslider3, "5", "");
		interface->declare(&fHslider3, "tooltip", "Envelope decay duration");
		interface->declare(&fHslider3, "unit", "s");
		interface->addHorizontalSlider("Envelope_Decay", &fHslider3, 0.05f, 0.0f, 2.0f, 0.01f);
		interface->declare(&fHslider4, "5", "");
		interface->declare(&fHslider4, "tooltip", "Envelope release duration");
		interface->declare(&fHslider4, "unit", "s");
		interface->addHorizontalSlider("Envelope_Release", &fHslider4, 0.1f, 0.0f, 2.0f, 0.01f);
		interface->closeBox();
		interface->openVerticalBox("Vibrato_Parameters");
		interface->declare(&fHslider7, "4", "");
		interface->declare(&fHslider7, "tooltip", "Vibrato attack duration");
		interface->declare(&fHslider7, "unit", "s");
		interface->addHorizontalSlider("Vibrato_Attack", &fHslider7, 0.5f, 0.0f, 2.0f, 0.01f);
		interface->declare(&fHslider9, "4", "");
		interface->declare(&fHslider9, "unit", "Hz");
		interface->addHorizontalSlider("Vibrato_Freq", &fHslider9, 5.0f, 1.0f, 15.0f, 0.1f);
		interface->declare(&fHslider6, "4", "");
		interface->declare(&fHslider6, "tooltip", "A value between 0 and 1");
		interface->addHorizontalSlider("Vibrato_Gain", &fHslider6, 0.1f, 0.0f, 1.0f, 0.01f);
		interface->declare(&fHslider8, "4", "");
		interface->declare(&fHslider8, "tooltip", "Vibrato release duration");
		interface->declare(&fHslider8, "unit", "s");
		interface->addHorizontalSlider("Vibrato_Release", &fHslider8, 0.01f, 0.0f, 2.0f, 0.01f);
		interface->closeBox();
		interface->closeBox();
		interface->openHorizontalBox("Physical_and_Nonlinearity");
		interface->openVerticalBox("Nonlinear_Filter_Parameters");
		interface->declare(&fHslider12, "3", "");
		interface->declare(&fHslider12, "tooltip", "Frequency of the sine wave for the modulation of theta (works if Modulation Type=3)");
		interface->declare(&fHslider12, "unit", "Hz");
		interface->addHorizontalSlider("Modulation_Frequency", &fHslider12, 2.2e+02f, 2e+01f, 1e+03f, 0.1f);
		interface->declare(&fEntry0, "3", "");
		interface->declare(&fEntry0, "tooltip", "0=theta is modulated by the incoming signal; 1=theta is modulated by the averaged incoming signal; 2=theta is modulated by the squared incoming signal; 3=theta is modulated by a sine wave of frequency freqMod; 4=theta is modulated by a sine wave of frequency freq;");
		interface->addNumEntry("Modulation_Type", &fEntry0, 0.0f, 0.0f, 4.0f, 1.0f);
		interface->declare(&fHslider10, "3", "");
		interface->declare(&fHslider10, "tooltip", "Nonlinearity factor (value between 0 and 1)");
		interface->addHorizontalSlider("Nonlinearity", &fHslider10, 0.0f, 0.0f, 1.0f, 0.01f);
		interface->declare(&fHslider11, "3", "");
		interface->declare(&fHslider11, "Attack duration of the nonlinearity", "");
		interface->declare(&fHslider11, "unit", "s");
		interface->addHorizontalSlider("Nonlinearity_Attack", &fHslider11, 0.1f, 0.0f, 2.0f, 0.01f);
		interface->closeBox();
		interface->openVerticalBox("Physical_Parameters");
		interface->declare(&fHslider5, "2", "");
		interface->declare(&fHslider5, "tooltip", "Breath noise gain (value between 0 and 1)");
		interface->addHorizontalSlider("Noise_Gain", &fHslider5, 0.0f, 0.0f, 1.0f, 0.01f);
		interface->declare(&fHslider1, "2", "");
		interface->declare(&fHslider1, "tooltip", "Breath pressure (value bewteen 0 and 1)");
		interface->addHorizontalSlider("Pressure", &fHslider1, 1.0f, 0.0f, 1.0f, 0.01f);
		interface->declare(&fHslider13, "2", "");
		interface->declare(&fHslider13, "tooltip", "Reed stiffness (value between 0 and 1)");
		interface->addHorizontalSlider("Reed_Stiffness", &fHslider13, 0.5f, 0.0f, 1.0f, 0.01f);
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
		float fSlow1 = (1.5f * (1.0f - fSlow0));
		float fSlow2 = float(fHslider1);
		float fSlow3 = float(fButton0);
		int iSlow4 = (fSlow3 > 0.0f);
		float fSlow5 = float(fHslider2);
		float fSlow6 = (1.0f / (float((fSlow5 == 0.0f)) + (fConst0 * fSlow5)));
		float fSlow7 = float(fHslider3);
		float fSlow8 = (1.0f - powf(1e+02f, (1.0f / (float((fSlow7 == 0.0f)) + (fConst0 * fSlow7)))));
		float fSlow9 = float(fHslider4);
		float fSlow10 = (1.0f - (1.0f / powf(1e+05f, (1.0f / (float((fSlow9 == 0.0f)) + (fConst0 * fSlow9))))));
		int iSlow11 = (fSlow3 <= 0.0f);
		float fSlow12 = (4.1909515e-10f * float(fHslider5));
		float fSlow13 = float(fHslider6);
		float fSlow14 = float(fHslider7);
		float fSlow15 = (1.0f / ((fConst1 * fSlow14) + float(((1.8f * fSlow14) == 0.0f))));
		float fSlow16 = (fConst2 * fSlow14);
		float fSlow17 = (fSlow16 + float(((0.2f * fSlow14) == 0.0f)));
		float fSlow18 = float(fHslider8);
		float fSlow19 = (1.0f - (1.0f / powf(1e+05f, (1.0f / (float((fSlow18 == 0.0f)) + (fConst0 * fSlow18))))));
		float fSlow20 = (fConst3 * float(fHslider9));
		float fSlow21 = float(fEntry0);
		int iSlow22 = (fSlow21 >= 3.0f);
		float fSlow23 = float(fHslider10);
		float fSlow24 = (6.0f * (fSlow23 * float((fSlow21 < 2.0f))));
		float fSlow25 = float(fEntry1);
		float fSlow26 = (fConst4 / fSlow25);
		int iSlow27 = int((fSlow26 - (1.5f + fSlow24)));
		float fSlow28 = (fSlow24 + float(iSlow27));
		float fSlow29 = ((fSlow28 + 2.5f) - fSlow26);
		int iSlow30 = (1 + (iSlow27 & 4095));
		float fSlow31 = (fSlow26 - (1.5f + fSlow28));
		int iSlow32 = (1 + ((1 + iSlow27) & 4095));
		float fSlow33 = (0.001f * fSlow23);
		float fSlow34 = float(fHslider11);
		float fSlow35 = (1.0f / (float((fSlow34 == 0.0f)) + (fConst0 * fSlow34)));
		int iSlow36 = (fSlow21 != 4.0f);
		float fSlow37 = (0.001f * float(fHslider12));
		float fSlow38 = (fSlow25 * float((fSlow21 == 4.0f)));
		int iSlow39 = (fSlow21 < 3.0f);
		float fSlow40 = (3.1415927f * float((fSlow21 == 0.0f)));
		float fSlow41 = (1.5707964f * float((fSlow21 == 1.0f)));
		float fSlow42 = (3.1415927f * float((fSlow21 == 2.0f)));
		float fSlow43 = ((0.26f * float(fHslider13)) - 0.44f);
		float fSlow44 = (0.001f * float(fEntry2));
		float fSlow45 = (1.5f * fSlow0);
		int iSlow46 = (int((fConst4 * (float(fHslider14) / fSlow25))) & 4095);
		for (int i = 0; (i < count); i = (i + 1)) {
			iRec1[0] = (iSlow4 & (iRec1[1] | (fRec2[1] >= 1.0f)));
			int iTemp0 = (iSlow11 & (fRec2[1] > 0.0f));
			fRec2[0] = (((fSlow6 * float((((iRec1[1] == 0) & iSlow4) & (fRec2[1] < 1.0f)))) + (fRec2[1] * ((1.0f - (fSlow8 * float((iRec1[1] & (fRec2[1] > 1e+02f))))) - (fSlow10 * float(iTemp0))))) * float(((iTemp0 == 0) | (fRec2[1] >= 1e-06f))));
			iRec3[0] = (12345 + (1103515245 * iRec3[1]));
			iRec4[0] = (iSlow4 & (iRec4[1] | (fRec6[1] >= 1.0f)));
			iRec5[0] = (iSlow4 * (1 + iRec5[1]));
			int iTemp1 = (iSlow11 & (fRec6[1] > 0.0f));
			fRec6[0] = (((fSlow15 * float((((((iRec4[1] == 0) & iSlow4) & (fRec6[1] < 1.0f)) & (float(iRec5[1]) > fSlow16)) * (1 - (float(iRec5[1]) < fSlow17))))) + (fRec6[1] * (1.0f - (fSlow19 * float(iTemp1))))) * float(((iTemp1 == 0) | (fRec6[1] >= 1e-06f))));
			float fTemp2 = (fRec8[1] + fSlow20);
			fRec8[0] = (fTemp2 - floorf(fTemp2));
			float fTemp3 = (fSlow2 * ((fRec2[0] * (0.9f + (fSlow12 * float(iRec3[0])))) * (1.0f + (fSlow13 * (fRec6[0] * ftbl0Clarinet_dspSIG0[int((65536.0f * fRec8[0]))])))));
			float fTemp4 = ((fSlow29 * fRec0[((IOTA - iSlow30) & 8191)]) + (fSlow31 * fRec0[((IOTA - iSlow32) & 8191)]));
			fVec0[0] = fTemp4;
			fRec10[0] = ((0.999f * fRec10[1]) + fSlow33);
			iRec11[0] = (iSlow4 & (iRec11[1] | (fRec12[1] >= 1.0f)));
			int iTemp5 = (iSlow11 & (fRec12[1] > 0.0f));
			fRec12[0] = (((fSlow35 * float((((iRec11[1] == 0) & iSlow4) & (fRec12[1] < 1.0f)))) + (fRec12[1] * (1.0f - (fSlow10 * float(iTemp5))))) * float(((iTemp5 == 0) | (fRec12[1] >= 1e-06f))));
			float fTemp6 = (fRec10[0] * fRec12[0]);
			fRec14[0] = ((0.999f * fRec14[1]) + fSlow37);
			float fTemp7 = (fRec13[1] + (fConst3 * ((float(iSlow36) * fRec14[0]) + fSlow38)));
			fRec13[0] = (fTemp7 - floorf(fTemp7));
			float fTemp8 = (3.1415927f * (fTemp6 * ftbl0Clarinet_dspSIG0[int((65536.0f * fRec13[0]))]));
			float fTemp9 = sinf(fTemp8);
			float fTemp10 = (0.0f - fTemp9);
			float fTemp11 = cosf(fTemp8);
			float fTemp12 = ((fRec15[1] * fTemp10) + (fTemp4 * fTemp11));
			float fTemp13 = ((fTemp10 * fRec16[1]) + (fTemp11 * fTemp12));
			float fTemp14 = ((fTemp10 * fRec17[1]) + (fTemp11 * fTemp13));
			float fTemp15 = ((fTemp10 * fRec18[1]) + (fTemp11 * fTemp14));
			float fTemp16 = ((fTemp10 * fRec19[1]) + (fTemp11 * fTemp15));
			fRec20[0] = ((fTemp10 * fRec20[1]) + (fTemp11 * fTemp16));
			fRec19[0] = ((fTemp9 * fTemp16) + (fTemp11 * fRec20[1]));
			fRec18[0] = ((fTemp9 * fTemp15) + (fTemp11 * fRec19[1]));
			fRec17[0] = ((fTemp9 * fTemp14) + (fTemp11 * fRec18[1]));
			fRec16[0] = ((fTemp9 * fTemp13) + (fTemp11 * fRec17[1]));
			fRec15[0] = ((fTemp9 * fTemp12) + (fTemp11 * fRec16[1]));
			float fTemp17 = (fTemp6 * (((fSlow40 * fTemp4) + (fSlow41 * (fTemp4 + fVec0[1]))) + (fSlow42 * faustpower2_f(fTemp4))));
			float fTemp18 = sinf(fTemp17);
			float fTemp19 = (0.0f - fTemp18);
			float fTemp20 = cosf(fTemp17);
			float fTemp21 = ((fRec21[1] * fTemp19) + (fTemp4 * fTemp20));
			float fTemp22 = ((fTemp19 * fRec22[1]) + (fTemp20 * fTemp21));
			float fTemp23 = ((fTemp19 * fRec23[1]) + (fTemp20 * fTemp22));
			float fTemp24 = ((fTemp19 * fRec24[1]) + (fTemp20 * fTemp23));
			float fTemp25 = ((fTemp19 * fRec25[1]) + (fTemp20 * fTemp24));
			fRec26[0] = ((fTemp19 * fRec26[1]) + (fTemp20 * fTemp25));
			fRec25[0] = ((fTemp18 * fTemp25) + (fTemp20 * fRec26[1]));
			fRec24[0] = ((fTemp18 * fTemp24) + (fTemp20 * fRec25[1]));
			fRec23[0] = ((fTemp18 * fTemp23) + (fTemp20 * fRec24[1]));
			fRec22[0] = ((fTemp18 * fTemp22) + (fTemp20 * fRec23[1]));
			fRec21[0] = ((fTemp18 * fTemp21) + (fTemp20 * fRec22[1]));
			fRec9[0] = (0.5f * (fRec9[1] + ((float(iSlow22) * ((fTemp4 * fTemp9) + (fRec15[1] * fTemp11))) + (float(iSlow39) * ((fRec10[0] * ((fTemp4 * fTemp18) + (fRec21[1] * fTemp20))) + ((1.0f - fRec10[0]) * fTemp4))))));
			float fTemp26 = (0.0f - ((0.95f * fRec9[0]) + fTemp3));
			float fTemp27 = (0.7f + (fSlow43 * fTemp26));
			float fTemp28 = (float((fTemp27 > 1.0f)) + (fTemp27 * float((fTemp27 <= 1.0f))));
			fRec0[(IOTA & 8191)] = (fTemp3 + (fTemp26 * ((fTemp28 * float((fTemp28 >= -1.0f))) - float((fTemp28 < -1.0f)))));
			fRec27[0] = ((0.999f * fRec27[1]) + fSlow44);
			float fTemp29 = (fRec0[((IOTA - 0) & 8191)] * fRec27[0]);
			fVec1[(IOTA & 4095)] = fTemp29;
			output0[i] = FAUSTFLOAT((fSlow1 * fTemp29));
			output1[i] = FAUSTFLOAT((fSlow45 * fVec1[((IOTA - iSlow46) & 4095)]));
			iRec1[1] = iRec1[0];
			fRec2[1] = fRec2[0];
			iRec3[1] = iRec3[0];
			iRec4[1] = iRec4[0];
			iRec5[1] = iRec5[0];
			fRec6[1] = fRec6[0];
			fRec8[1] = fRec8[0];
			fVec0[1] = fVec0[0];
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
			IOTA = (IOTA + 1);
			fRec27[1] = fRec27[0];
			
		}
		
	}

	
};



#include "Faust_plugins_template2.cpp"


#endif
