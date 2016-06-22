/* ------------------------------------------------------------
author: "Romain Michon"
copyright: "Romain Michon (rmichon@ccrma.stanford.edu)"
name: "FluteSTK"
version: "1.0"
Code generated with Faust 2.0.a43 (http://faust.grame.fr)
------------------------------------------------------------ */

#ifndef  __Flute_Stk_dsp_H__
#define  __Flute_Stk_dsp_H__

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


class Flute_Stk_dspSIG0 {
	
  private:
	
	int iRec6[2];
	
  public:
	
	int getNumInputsFlute_Stk_dspSIG0() {
		return 0;
		
	}
	int getNumOutputsFlute_Stk_dspSIG0() {
		return 1;
		
	}
	int getInputRateFlute_Stk_dspSIG0(int channel) {
		int rate;
		switch (channel) {
			default: {
				rate = -1;
				break;
			}
			
		}
		return rate;
		
	}
	int getOutputRateFlute_Stk_dspSIG0(int channel) {
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
	
	void instanceInitFlute_Stk_dspSIG0(int samplingFreq) {
		for (int i4 = 0; (i4 < 2); i4 = (i4 + 1)) {
			iRec6[i4] = 0;
			
		}
		
	}
	
	void fillFlute_Stk_dspSIG0(int count, float* output) {
		for (int i = 0; (i < count); i = (i + 1)) {
			iRec6[0] = (1 + iRec6[1]);
			output[i] = sinf((9.58738e-05f * float((iRec6[0] - 1))));
			iRec6[1] = iRec6[0];
			
		}
		
	}
};

Flute_Stk_dspSIG0* newFlute_Stk_dspSIG0() { return (Flute_Stk_dspSIG0*)new Flute_Stk_dspSIG0(); }
void deleteFlute_Stk_dspSIG0(Flute_Stk_dspSIG0* dsp) { delete dsp; }

static float ftbl0Flute_Stk_dspSIG0[65536];
static float faustpower2_f(float value) {
	return (value * value);
	
}

#ifndef FAUSTCLASS 
#define FAUSTCLASS Flute_Stk_dsp
#endif

class Flute_Stk_dsp : public dsp {
	
  private:
	
	float fRec0[8192];
	float fVec1[4096];
	float fVec2[4096];
	float fVec0[2];
	float fRec3[2];
	int iRec4[2];
	float fRec5[2];
	float fRec8[2];
	float fRec7[2];
	float fRec14[2];
	float fRec13[2];
	float fRec12[2];
	float fRec11[2];
	float fRec10[2];
	float fRec9[2];
	float fRec20[2];
	float fRec19[2];
	float fRec18[2];
	float fRec17[2];
	float fRec16[2];
	float fRec15[2];
	float fRec2[2];
	float fRec1[2];
	int iRec21[2];
	float fRec22[2];
	int iRec23[2];
	int iRec24[2];
	int iRec25[2];
	float fRec26[2];
	float fRec27[2];
	float fRec28[2];
	FAUSTFLOAT fHslider0;
	int fSamplingFreq;
	float fConst0;
	float fConst1;
	float fConst2;
	FAUSTFLOAT fEntry0;
	float fConst3;
	FAUSTFLOAT fEntry1;
	FAUSTFLOAT fHslider1;
	FAUSTFLOAT fButton0;
	FAUSTFLOAT fHslider2;
	FAUSTFLOAT fHslider3;
	float fConst4;
	FAUSTFLOAT fHslider4;
	float fConst5;
	FAUSTFLOAT fHslider5;
	FAUSTFLOAT fHslider6;
	FAUSTFLOAT fHslider7;
	FAUSTFLOAT fHslider8;
	FAUSTFLOAT fHslider9;
	FAUSTFLOAT fHslider10;
	FAUSTFLOAT fHslider11;
	FAUSTFLOAT fHslider12;
	FAUSTFLOAT fHslider13;
	FAUSTFLOAT fHslider14;
	int IOTA;
	FAUSTFLOAT fEntry2;
	FAUSTFLOAT fHslider15;
	
  public:
	
	void static metadata(Meta* m) { 
		m->declare("author", "Romain Michon");
		m->declare("copyright", "Romain Michon (rmichon@ccrma.stanford.edu)");
		m->declare("description", "Nonlinear WaveGuide Flute from STK");
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
		m->declare("name", "FluteSTK");
		m->declare("reference", "https://ccrma.stanford.edu/~jos/pasp/Flutes_Recorders_Pipe_Organs.html");
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
		Flute_Stk_dspSIG0* sig0 = newFlute_Stk_dspSIG0();
		sig0->instanceInitFlute_Stk_dspSIG0(samplingFreq);
		sig0->fillFlute_Stk_dspSIG0(65536, ftbl0Flute_Stk_dspSIG0);
		deleteFlute_Stk_dspSIG0(sig0);
		
	}
	
	virtual void instanceInit(int samplingFreq) {
		fSamplingFreq = samplingFreq;
		fHslider0 = FAUSTFLOAT(0.6f);
		fConst0 = min(1.92e+05f, max(1.0f, float(fSamplingFreq)));
		fConst1 = (2205.0f / fConst0);
		fConst2 = (fConst1 + 0.3f);
		fEntry0 = FAUSTFLOAT(0.0f);
		fConst3 = (0.5f * fConst0);
		fEntry1 = FAUSTFLOAT(4.4e+02f);
		for (int i0 = 0; (i0 < 2); i0 = (i0 + 1)) {
			fVec0[i0] = 0.0f;
			
		}
		fHslider1 = FAUSTFLOAT(0.0f);
		for (int i1 = 0; (i1 < 2); i1 = (i1 + 1)) {
			fRec3[i1] = 0.0f;
			
		}
		fButton0 = FAUSTFLOAT(0.0f);
		for (int i2 = 0; (i2 < 2); i2 = (i2 + 1)) {
			iRec4[i2] = 0;
			
		}
		fHslider2 = FAUSTFLOAT(0.1f);
		fHslider3 = FAUSTFLOAT(0.3f);
		for (int i3 = 0; (i3 < 2); i3 = (i3 + 1)) {
			fRec5[i3] = 0.0f;
			
		}
		fConst4 = (1.0f / fConst0);
		fHslider4 = FAUSTFLOAT(2.2e+02f);
		for (int i5 = 0; (i5 < 2); i5 = (i5 + 1)) {
			fRec8[i5] = 0.0f;
			
		}
		for (int i6 = 0; (i6 < 2); i6 = (i6 + 1)) {
			fRec7[i6] = 0.0f;
			
		}
		for (int i7 = 0; (i7 < 2); i7 = (i7 + 1)) {
			fRec14[i7] = 0.0f;
			
		}
		for (int i8 = 0; (i8 < 2); i8 = (i8 + 1)) {
			fRec13[i8] = 0.0f;
			
		}
		for (int i9 = 0; (i9 < 2); i9 = (i9 + 1)) {
			fRec12[i9] = 0.0f;
			
		}
		for (int i10 = 0; (i10 < 2); i10 = (i10 + 1)) {
			fRec11[i10] = 0.0f;
			
		}
		for (int i11 = 0; (i11 < 2); i11 = (i11 + 1)) {
			fRec10[i11] = 0.0f;
			
		}
		for (int i12 = 0; (i12 < 2); i12 = (i12 + 1)) {
			fRec9[i12] = 0.0f;
			
		}
		for (int i13 = 0; (i13 < 2); i13 = (i13 + 1)) {
			fRec20[i13] = 0.0f;
			
		}
		for (int i14 = 0; (i14 < 2); i14 = (i14 + 1)) {
			fRec19[i14] = 0.0f;
			
		}
		for (int i15 = 0; (i15 < 2); i15 = (i15 + 1)) {
			fRec18[i15] = 0.0f;
			
		}
		for (int i16 = 0; (i16 < 2); i16 = (i16 + 1)) {
			fRec17[i16] = 0.0f;
			
		}
		for (int i17 = 0; (i17 < 2); i17 = (i17 + 1)) {
			fRec16[i17] = 0.0f;
			
		}
		for (int i18 = 0; (i18 < 2); i18 = (i18 + 1)) {
			fRec15[i18] = 0.0f;
			
		}
		fConst5 = (fConst1 - 0.7f);
		for (int i19 = 0; (i19 < 2); i19 = (i19 + 1)) {
			fRec2[i19] = 0.0f;
			
		}
		for (int i20 = 0; (i20 < 2); i20 = (i20 + 1)) {
			fRec1[i20] = 0.0f;
			
		}
		fHslider5 = FAUSTFLOAT(0.5f);
		fHslider6 = FAUSTFLOAT(1.0f);
		for (int i21 = 0; (i21 < 2); i21 = (i21 + 1)) {
			iRec21[i21] = 0;
			
		}
		fHslider7 = FAUSTFLOAT(0.03f);
		fHslider8 = FAUSTFLOAT(0.01f);
		for (int i22 = 0; (i22 < 2); i22 = (i22 + 1)) {
			fRec22[i22] = 0.0f;
			
		}
		fHslider9 = FAUSTFLOAT(0.03f);
		for (int i23 = 0; (i23 < 2); i23 = (i23 + 1)) {
			iRec23[i23] = 0;
			
		}
		fHslider10 = FAUSTFLOAT(0.05f);
		for (int i24 = 0; (i24 < 2); i24 = (i24 + 1)) {
			iRec24[i24] = 0;
			
		}
		for (int i25 = 0; (i25 < 2); i25 = (i25 + 1)) {
			iRec25[i25] = 0;
			
		}
		fHslider11 = FAUSTFLOAT(0.5f);
		fHslider12 = FAUSTFLOAT(0.05f);
		fHslider13 = FAUSTFLOAT(0.1f);
		for (int i26 = 0; (i26 < 2); i26 = (i26 + 1)) {
			fRec26[i26] = 0.0f;
			
		}
		fHslider14 = FAUSTFLOAT(6.0f);
		for (int i27 = 0; (i27 < 2); i27 = (i27 + 1)) {
			fRec27[i27] = 0.0f;
			
		}
		IOTA = 0;
		for (int i28 = 0; (i28 < 4096); i28 = (i28 + 1)) {
			fVec1[i28] = 0.0f;
			
		}
		for (int i29 = 0; (i29 < 8192); i29 = (i29 + 1)) {
			fRec0[i29] = 0.0f;
			
		}
		fEntry2 = FAUSTFLOAT(1.0f);
		for (int i30 = 0; (i30 < 2); i30 = (i30 + 1)) {
			fRec28[i30] = 0.0f;
			
		}
		for (int i31 = 0; (i31 < 4096); i31 = (i31 + 1)) {
			fVec2[i31] = 0.0f;
			
		}
		fHslider15 = FAUSTFLOAT(0.5f);
		
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
		interface->declare(&fHslider7, "5", "");
		interface->declare(&fHslider7, "tooltip", "Envelope attack duration");
		interface->declare(&fHslider7, "unit", "s");
		interface->addHorizontalSlider("Envelope_Attack", &fHslider7, 0.03f, 0.0f, 2.0f, 0.01f);
		interface->declare(&fHslider8, "5", "");
		interface->declare(&fHslider8, "tooltip", "Envelope decay duration");
		interface->declare(&fHslider8, "unit", "s");
		interface->addHorizontalSlider("Envelope_Decay", &fHslider8, 0.01f, 0.0f, 2.0f, 0.01f);
		interface->declare(&fHslider3, "5", "");
		interface->declare(&fHslider3, "tooltip", "Envelope release duration");
		interface->declare(&fHslider3, "unit", "s");
		interface->addHorizontalSlider("Envelope_Release", &fHslider3, 0.3f, 0.0f, 2.0f, 0.01f);
		interface->closeBox();
		interface->openVerticalBox("Vibrato_Parameters");
		interface->declare(&fHslider11, "4", "");
		interface->declare(&fHslider11, "tooltip", "Vibrato attack duration");
		interface->declare(&fHslider11, "unit", "s");
		interface->addHorizontalSlider("Vibrato_Attack", &fHslider11, 0.5f, 0.0f, 2.0f, 0.01f);
		interface->declare(&fHslider12, "4", "");
		interface->declare(&fHslider12, "tooltip", "Vibrato silence duration before attack");
		interface->declare(&fHslider12, "unit", "s");
		interface->addHorizontalSlider("Vibrato_Begin", &fHslider12, 0.05f, 0.0f, 2.0f, 0.01f);
		interface->declare(&fHslider14, "4", "");
		interface->declare(&fHslider14, "unit", "Hz");
		interface->addHorizontalSlider("Vibrato_Freq", &fHslider14, 6.0f, 1.0f, 15.0f, 0.1f);
		interface->declare(&fHslider10, "4", "");
		interface->declare(&fHslider10, "tooltip", "A value between 0 and 1");
		interface->addHorizontalSlider("Vibrato_Gain", &fHslider10, 0.05f, 0.0f, 1.0f, 0.01f);
		interface->declare(&fHslider13, "4", "");
		interface->declare(&fHslider13, "tooltip", "Vibrato release duration");
		interface->declare(&fHslider13, "unit", "s");
		interface->addHorizontalSlider("Vibrato_Release", &fHslider13, 0.1f, 0.0f, 2.0f, 0.01f);
		interface->closeBox();
		interface->closeBox();
		interface->openHorizontalBox("Physical_and_Nonlinearity");
		interface->openVerticalBox("Nonlinear_Filter_Parameters");
		interface->declare(&fHslider4, "3", "");
		interface->declare(&fHslider4, "tooltip", "Frequency of the sine wave for the modulation of theta (works if Modulation Type=3)");
		interface->declare(&fHslider4, "unit", "Hz");
		interface->addHorizontalSlider("Modulation_Frequency", &fHslider4, 2.2e+02f, 2e+01f, 1e+03f, 0.1f);
		interface->declare(&fEntry0, "3", "");
		interface->declare(&fEntry0, "tooltip", "0=theta is modulated by the incoming signal; 1=theta is modulated by the averaged incoming signal; 2=theta is modulated by the squared incoming signal; 3=theta is modulated by a sine wave of frequency freqMod; 4=theta is modulated by a sine wave of frequency freq;");
		interface->addNumEntry("Modulation_Type", &fEntry0, 0.0f, 0.0f, 4.0f, 1.0f);
		interface->declare(&fHslider1, "3", "");
		interface->declare(&fHslider1, "tooltip", "Nonlinearity factor (value between 0 and 1)");
		interface->addHorizontalSlider("Nonlinearity", &fHslider1, 0.0f, 0.0f, 1.0f, 0.01f);
		interface->declare(&fHslider2, "3", "");
		interface->declare(&fHslider2, "Attack duration of the nonlinearity", "");
		interface->declare(&fHslider2, "unit", "s");
		interface->addHorizontalSlider("Nonlinearity_Attack", &fHslider2, 0.1f, 0.0f, 2.0f, 0.01f);
		interface->closeBox();
		interface->openVerticalBox("Physical_Parameters");
		interface->declare(&fHslider5, "2", "");
		interface->declare(&fHslider5, "tooltip", "A value between 0 and 1");
		interface->addHorizontalSlider("Embouchure_Ajust", &fHslider5, 0.5f, 0.0f, 1.0f, 0.01f);
		interface->declare(&fHslider9, "2", "");
		interface->declare(&fHslider9, "tooltip", "A value between 0 and 1");
		interface->addHorizontalSlider("Noise_Gain", &fHslider9, 0.03f, 0.0f, 1.0f, 0.01f);
		interface->declare(&fHslider6, "2", "");
		interface->declare(&fHslider6, "tooltip", "Breath pressure (value between 0 and 1)");
		interface->addHorizontalSlider("Pressure", &fHslider6, 1.0f, 0.0f, 1.0f, 0.01f);
		interface->closeBox();
		interface->closeBox();
		interface->openVerticalBox("Spat");
		interface->addHorizontalSlider("pan angle", &fHslider0, 0.6f, 0.0f, 1.0f, 0.01f);
		interface->addHorizontalSlider("spatial width", &fHslider15, 0.5f, 0.0f, 1.0f, 0.01f);
		interface->closeBox();
		interface->closeBox();
		
	}
	
	virtual void compute(int count, FAUSTFLOAT** inputs, FAUSTFLOAT** outputs) {
		FAUSTFLOAT* output0 = outputs[0];
		FAUSTFLOAT* output1 = outputs[1];
		float fSlow0 = float(fHslider0);
		float fSlow1 = (0.3f * (1.0f - fSlow0));
		float fSlow2 = float(fEntry0);
		int iSlow3 = (fSlow2 >= 3.0f);
		float fSlow4 = float(fEntry1);
		float fSlow5 = (fConst3 / fSlow4);
		float fSlow6 = (fSlow5 - 2.0f);
		int iSlow7 = int(fSlow6);
		float fSlow8 = (float((3 + iSlow7)) - fSlow5);
		int iSlow9 = (1 + (iSlow7 & 4095));
		float fSlow10 = (fSlow5 - float((2 + iSlow7)));
		int iSlow11 = (1 + ((1 + iSlow7) & 4095));
		float fSlow12 = (0.001f * float(fHslider1));
		float fSlow13 = float(fButton0);
		int iSlow14 = (fSlow13 > 0.0f);
		float fSlow15 = float(fHslider2);
		float fSlow16 = (1.0f / (float((fSlow15 == 0.0f)) + (fConst0 * fSlow15)));
		float fSlow17 = float(fHslider3);
		float fSlow18 = (1.0f / (float((fSlow17 == 0.0f)) + (fConst0 * fSlow17)));
		float fSlow19 = (1.0f - (1.0f / powf(1e+05f, fSlow18)));
		int iSlow20 = (fSlow13 <= 0.0f);
		int iSlow21 = (fSlow2 != 4.0f);
		float fSlow22 = (0.001f * float(fHslider4));
		float fSlow23 = (fSlow4 * float((fSlow2 == 4.0f)));
		int iSlow24 = (fSlow2 < 3.0f);
		float fSlow25 = (3.1415927f * float((fSlow2 == 0.0f)));
		float fSlow26 = (1.5707964f * float((fSlow2 == 1.0f)));
		float fSlow27 = (3.1415927f * float((fSlow2 == 2.0f)));
		float fSlow28 = (fSlow6 * (1.5f - float(fHslider5)));
		int iSlow29 = int(fSlow28);
		int iSlow30 = (1 + iSlow29);
		float fSlow31 = (float(iSlow30) - fSlow28);
		float fSlow32 = float(fHslider6);
		float fSlow33 = (float(fHslider7) * fSlow32);
		float fSlow34 = (1.0f / ((fConst0 * fSlow33) + float((fSlow33 == 0.0f))));
		float fSlow35 = float(fHslider8);
		float fSlow36 = (1.0f - powf(8e+01f, (1.0f / (float((fSlow35 == 0.0f)) + (fConst0 * fSlow35)))));
		float fSlow37 = (1.0f - (1.0f / powf(8e+04f, fSlow18)));
		float fSlow38 = (4.656613e-10f * float(fHslider9));
		float fSlow39 = float(fHslider10);
		float fSlow40 = float(fHslider11);
		float fSlow41 = (1.0f / (float((fSlow40 == 0.0f)) + (fConst0 * fSlow40)));
		float fSlow42 = float(fHslider12);
		float fSlow43 = (fConst0 * fSlow42);
		float fSlow44 = (float((fSlow42 == 0.0f)) + fSlow43);
		float fSlow45 = float(fHslider13);
		float fSlow46 = (1.0f - (1.0f / powf(1e+05f, (1.0f / (float((fSlow45 == 0.0f)) + (fConst0 * fSlow45))))));
		float fSlow47 = (fConst4 * float(fHslider14));
		int iSlow48 = (iSlow29 & 4095);
		float fSlow49 = (fSlow28 - float(iSlow29));
		int iSlow50 = (iSlow30 & 4095);
		float fSlow51 = (0.001f * float(fEntry2));
		float fSlow52 = (0.3f * fSlow0);
		int iSlow53 = (int((fConst3 * (float(fHslider15) / fSlow4))) & 4095);
		for (int i = 0; (i < count); i = (i + 1)) {
			float fTemp0 = ((fSlow8 * fRec0[((IOTA - iSlow9) & 8191)]) + (fSlow10 * fRec0[((IOTA - iSlow11) & 8191)]));
			fVec0[0] = fTemp0;
			fRec3[0] = ((0.999f * fRec3[1]) + fSlow12);
			iRec4[0] = (iSlow14 & (iRec4[1] | (fRec5[1] >= 1.0f)));
			int iTemp1 = (iSlow20 & (fRec5[1] > 0.0f));
			fRec5[0] = (((fSlow16 * float((((iRec4[1] == 0) & iSlow14) & (fRec5[1] < 1.0f)))) + (fRec5[1] * (1.0f - (fSlow19 * float(iTemp1))))) * float(((iTemp1 == 0) | (fRec5[1] >= 1e-06f))));
			float fTemp2 = (fRec3[0] * fRec5[0]);
			fRec8[0] = ((0.999f * fRec8[1]) + fSlow22);
			float fTemp3 = (fRec7[1] + (fConst4 * ((float(iSlow21) * fRec8[0]) + fSlow23)));
			fRec7[0] = (fTemp3 - floorf(fTemp3));
			float fTemp4 = (3.1415927f * (fTemp2 * ftbl0Flute_Stk_dspSIG0[int((65536.0f * fRec7[0]))]));
			float fTemp5 = sinf(fTemp4);
			float fTemp6 = (0.0f - fTemp5);
			float fTemp7 = cosf(fTemp4);
			float fTemp8 = ((fRec9[1] * fTemp6) + (fTemp0 * fTemp7));
			float fTemp9 = ((fTemp6 * fRec10[1]) + (fTemp7 * fTemp8));
			float fTemp10 = ((fTemp6 * fRec11[1]) + (fTemp7 * fTemp9));
			float fTemp11 = ((fTemp6 * fRec12[1]) + (fTemp7 * fTemp10));
			float fTemp12 = ((fTemp6 * fRec13[1]) + (fTemp7 * fTemp11));
			fRec14[0] = ((fTemp6 * fRec14[1]) + (fTemp7 * fTemp12));
			fRec13[0] = ((fTemp5 * fTemp12) + (fTemp7 * fRec14[1]));
			fRec12[0] = ((fTemp5 * fTemp11) + (fTemp7 * fRec13[1]));
			fRec11[0] = ((fTemp5 * fTemp10) + (fTemp7 * fRec12[1]));
			fRec10[0] = ((fTemp5 * fTemp9) + (fTemp7 * fRec11[1]));
			fRec9[0] = ((fTemp5 * fTemp8) + (fTemp7 * fRec10[1]));
			float fTemp13 = (fTemp2 * (((fSlow25 * fTemp0) + (fSlow26 * (fTemp0 + fVec0[1]))) + (fSlow27 * faustpower2_f(fTemp0))));
			float fTemp14 = sinf(fTemp13);
			float fTemp15 = (0.0f - fTemp14);
			float fTemp16 = cosf(fTemp13);
			float fTemp17 = ((fRec15[1] * fTemp15) + (fTemp0 * fTemp16));
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
			fRec2[0] = ((fConst2 * (0.0f - ((float(iSlow3) * ((fTemp0 * fTemp5) + (fRec9[1] * fTemp7))) + (float(iSlow24) * ((fRec3[0] * ((fTemp0 * fTemp14) + (fRec15[1] * fTemp16))) + ((1.0f - fRec3[0]) * fTemp0)))))) - (fConst5 * fRec2[1]));
			fRec1[0] = ((fRec2[0] + (0.995f * fRec1[1])) - fRec2[1]);
			float fTemp22 = (0.5f * fRec1[0]);
			iRec21[0] = (iSlow14 & (iRec21[1] | (fRec22[1] >= 1.0f)));
			int iTemp23 = (iSlow20 & (fRec22[1] > 0.0f));
			fRec22[0] = (((fSlow34 * float((((iRec21[1] == 0) & iSlow14) & (fRec22[1] < 1.0f)))) + (fRec22[1] * ((1.0f - (fSlow36 * float((iRec21[1] & (fRec22[1] > 8e+01f))))) - (fSlow37 * float(iTemp23))))) * float(((iTemp23 == 0) | (fRec22[1] >= 1e-06f))));
			iRec23[0] = (12345 + (1103515245 * iRec23[1]));
			iRec24[0] = (iSlow14 & (iRec24[1] | (fRec26[1] >= 1.0f)));
			iRec25[0] = (iSlow14 * (1 + iRec25[1]));
			int iTemp24 = (iSlow20 & (fRec26[1] > 0.0f));
			fRec26[0] = (((fSlow41 * float((((((iRec24[1] == 0) & iSlow14) & (fRec26[1] < 1.0f)) & (float(iRec25[1]) > fSlow43)) * (1 - (float(iRec25[1]) < fSlow44))))) + (fRec26[1] * (1.0f - (fSlow46 * float(iTemp24))))) * float(((iTemp24 == 0) | (fRec26[1] >= 1e-06f))));
			float fTemp25 = (fRec27[1] + fSlow47);
			fRec27[0] = (fTemp25 - floorf(fTemp25));
			float fTemp26 = (((fSlow32 * (fRec22[0] * (1.0f + ((fSlow38 * float(iRec23[0])) + (fSlow39 * (fRec26[0] * ftbl0Flute_Stk_dspSIG0[int((65536.0f * fRec27[0]))])))))) + 1e-15f) - fTemp22);
			fVec1[(IOTA & 4095)] = fTemp26;
			float fTemp27 = ((fSlow31 * fVec1[((IOTA - iSlow48) & 4095)]) + (fSlow49 * fVec1[((IOTA - iSlow50) & 4095)]));
			float fTemp28 = (fTemp27 * (faustpower2_f(fTemp27) - 1.0f));
			float fTemp29 = (float((fTemp28 > 1.0f)) + (fTemp28 * float((fTemp28 <= 1.0f))));
			fRec0[(IOTA & 8191)] = ((fTemp22 + (fTemp29 * float((fTemp29 >= -1.0f)))) - float((fTemp29 < -1.0f)));
			fRec28[0] = ((0.999f * fRec28[1]) + fSlow51);
			float fTemp30 = (fRec0[((IOTA - 0) & 8191)] * fRec28[0]);
			fVec2[(IOTA & 4095)] = fTemp30;
			output0[i] = FAUSTFLOAT((fSlow1 * fTemp30));
			output1[i] = FAUSTFLOAT((fSlow52 * fVec2[((IOTA - iSlow53) & 4095)]));
			fVec0[1] = fVec0[0];
			fRec3[1] = fRec3[0];
			iRec4[1] = iRec4[0];
			fRec5[1] = fRec5[0];
			fRec8[1] = fRec8[0];
			fRec7[1] = fRec7[0];
			fRec14[1] = fRec14[0];
			fRec13[1] = fRec13[0];
			fRec12[1] = fRec12[0];
			fRec11[1] = fRec11[0];
			fRec10[1] = fRec10[0];
			fRec9[1] = fRec9[0];
			fRec20[1] = fRec20[0];
			fRec19[1] = fRec19[0];
			fRec18[1] = fRec18[0];
			fRec17[1] = fRec17[0];
			fRec16[1] = fRec16[0];
			fRec15[1] = fRec15[0];
			fRec2[1] = fRec2[0];
			fRec1[1] = fRec1[0];
			iRec21[1] = iRec21[0];
			fRec22[1] = fRec22[0];
			iRec23[1] = iRec23[0];
			iRec24[1] = iRec24[0];
			iRec25[1] = iRec25[0];
			fRec26[1] = fRec26[0];
			fRec27[1] = fRec27[0];
			IOTA = (IOTA + 1);
			fRec28[1] = fRec28[0];
			
		}
		
	}

	
};



#include "Faust_plugins_template2.cpp"


#endif
