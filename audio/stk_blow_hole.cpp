/* ------------------------------------------------------------
author: "Romain Michon"
copyright: "Romain Michon (rmichon@ccrma.stanford.edu)"
name: "BlowHole"
version: "1.0"
Code generated with Faust 2.0.a43 (http://faust.grame.fr)
------------------------------------------------------------ */

#ifndef  __Blow_Hole_dsp_H__
#define  __Blow_Hole_dsp_H__

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


class Blow_Hole_dspSIG0 {
	
  private:
	
	int iRec20[2];
	
  public:
	
	int getNumInputsBlow_Hole_dspSIG0() {
		return 0;
		
	}
	int getNumOutputsBlow_Hole_dspSIG0() {
		return 1;
		
	}
	int getInputRateBlow_Hole_dspSIG0(int channel) {
		int rate;
		switch (channel) {
			default: {
				rate = -1;
				break;
			}
			
		}
		return rate;
		
	}
	int getOutputRateBlow_Hole_dspSIG0(int channel) {
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
	
	void instanceInitBlow_Hole_dspSIG0(int samplingFreq) {
		for (int i12 = 0; (i12 < 2); i12 = (i12 + 1)) {
			iRec20[i12] = 0;
			
		}
		
	}
	
	void fillBlow_Hole_dspSIG0(int count, float* output) {
		for (int i = 0; (i < count); i = (i + 1)) {
			iRec20[0] = (1 + iRec20[1]);
			output[i] = sinf((9.58738e-05f * float((iRec20[0] - 1))));
			iRec20[1] = iRec20[0];
			
		}
		
	}
};

Blow_Hole_dspSIG0* newBlow_Hole_dspSIG0() { return (Blow_Hole_dspSIG0*)new Blow_Hole_dspSIG0(); }
void deleteBlow_Hole_dspSIG0(Blow_Hole_dspSIG0* dsp) { delete dsp; }

static float faustpower2_f(float value) {
	return (value * value);
	
}
static float ftbl0Blow_Hole_dspSIG0[65536];

#ifndef FAUSTCLASS 
#define FAUSTCLASS Blow_Hole_dsp
#endif

class Blow_Hole_dsp : public dsp {
	
  private:
	
	float fRec0[8192];
	float fVec3[4096];
	float fRec5[128];
	float fRec2[128];
	float fRec8[2];
	float fVec0[2];
	int iRec9[2];
	float fRec10[2];
	float fRec12[2];
	float fRec11[2];
	int iRec14[2];
	float fRec15[2];
	int iRec16[2];
	int iRec17[2];
	int iRec18[2];
	float fRec19[2];
	float fRec21[2];
	float fRec23[2];
	float fRec22[2];
	float fRec25[2];
	float fRec24[2];
	float fVec1[2];
	float fRec13[2];
	float fVec2[2];
	float fRec30[2];
	float fRec29[2];
	float fRec26[2];
	float fRec4[2];
	float fRec31[2];
	FAUSTFLOAT fHslider0;
	FAUSTFLOAT fEntry0;
	FAUSTFLOAT fHslider1;
	int fSamplingFreq;
	float fConst0;
	FAUSTFLOAT fEntry1;
	FAUSTFLOAT fButton0;
	FAUSTFLOAT fHslider2;
	FAUSTFLOAT fHslider3;
	float fConst1;
	float fConst2;
	float fConst3;
	FAUSTFLOAT fHslider4;
	FAUSTFLOAT fHslider5;
	FAUSTFLOAT fHslider6;
	FAUSTFLOAT fHslider7;
	FAUSTFLOAT fHslider8;
	float fConst4;
	FAUSTFLOAT fHslider9;
	float fConst5;
	FAUSTFLOAT fHslider10;
	float fConst6;
	FAUSTFLOAT fHslider11;
	float fConst7;
	int iConst8;
	int iConst9;
	float fConst10;
	int iConst11;
	float fConst12;
	int iConst13;
	FAUSTFLOAT fHslider12;
	FAUSTFLOAT fHslider13;
	float fConst14;
	int IOTA;
	float fConst15;
	int iConst16;
	float fConst17;
	int iConst18;
	int iConst19;
	float fConst20;
	int iConst21;
	float fConst22;
	float fConst23;
	FAUSTFLOAT fHslider14;
	FAUSTFLOAT fEntry2;
	float fConst24;
	FAUSTFLOAT fHslider15;
	
  public:
	
	void static metadata(Meta* m) { 
		m->declare("author", "Romain Michon");
		m->declare("copyright", "Romain Michon (rmichon@ccrma.stanford.edu)");
		m->declare("description", "Nonlinear WaveGuide Clarinet with one register hole and one tonehole");
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
		m->declare("name", "BlowHole");
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
		Blow_Hole_dspSIG0* sig0 = newBlow_Hole_dspSIG0();
		sig0->instanceInitBlow_Hole_dspSIG0(samplingFreq);
		sig0->fillBlow_Hole_dspSIG0(65536, ftbl0Blow_Hole_dspSIG0);
		deleteBlow_Hole_dspSIG0(sig0);
		
	}
	
	virtual void instanceInit(int samplingFreq) {
		fSamplingFreq = samplingFreq;
		fHslider0 = FAUSTFLOAT(0.6f);
		fEntry0 = FAUSTFLOAT(0.0f);
		fHslider1 = FAUSTFLOAT(0.0f);
		for (int i0 = 0; (i0 < 2); i0 = (i0 + 1)) {
			fRec8[i0] = 0.0f;
			
		}
		fConst0 = min(1.92e+05f, max(1.0f, float(fSamplingFreq)));
		fEntry1 = FAUSTFLOAT(4.4e+02f);
		for (int i1 = 0; (i1 < 2); i1 = (i1 + 1)) {
			fVec0[i1] = 0.0f;
			
		}
		fButton0 = FAUSTFLOAT(0.0f);
		for (int i2 = 0; (i2 < 2); i2 = (i2 + 1)) {
			iRec9[i2] = 0;
			
		}
		fHslider2 = FAUSTFLOAT(0.1f);
		fHslider3 = FAUSTFLOAT(0.1f);
		for (int i3 = 0; (i3 < 2); i3 = (i3 + 1)) {
			fRec10[i3] = 0.0f;
			
		}
		for (int i4 = 0; (i4 < 2); i4 = (i4 + 1)) {
			fRec12[i4] = 0.0f;
			
		}
		for (int i5 = 0; (i5 < 2); i5 = (i5 + 1)) {
			fRec11[i5] = 0.0f;
			
		}
		fConst1 = (0.21f * fConst0);
		fConst2 = (347.23f + fConst1);
		fConst3 = (0.0f - (347.23f / fConst2));
		fHslider4 = FAUSTFLOAT(0.0f);
		fHslider5 = FAUSTFLOAT(0.35f);
		for (int i6 = 0; (i6 < 2); i6 = (i6 + 1)) {
			iRec14[i6] = 0;
			
		}
		fHslider6 = FAUSTFLOAT(0.01f);
		for (int i7 = 0; (i7 < 2); i7 = (i7 + 1)) {
			fRec15[i7] = 0.0f;
			
		}
		fHslider7 = FAUSTFLOAT(0.0f);
		for (int i8 = 0; (i8 < 2); i8 = (i8 + 1)) {
			iRec16[i8] = 0;
			
		}
		fHslider8 = FAUSTFLOAT(0.1f);
		for (int i9 = 0; (i9 < 2); i9 = (i9 + 1)) {
			iRec17[i9] = 0;
			
		}
		for (int i10 = 0; (i10 < 2); i10 = (i10 + 1)) {
			iRec18[i10] = 0;
			
		}
		fConst4 = (1.8f * fConst0);
		fHslider9 = FAUSTFLOAT(0.5f);
		fConst5 = (0.2f * fConst0);
		fHslider10 = FAUSTFLOAT(0.01f);
		for (int i11 = 0; (i11 < 2); i11 = (i11 + 1)) {
			fRec19[i11] = 0.0f;
			
		}
		fConst6 = (1.0f / fConst0);
		fHslider11 = FAUSTFLOAT(5.0f);
		for (int i13 = 0; (i13 < 2); i13 = (i13 + 1)) {
			fRec21[i13] = 0.0f;
			
		}
		fConst7 = (0.00022675737f * fConst0);
		iConst8 = int(fConst7);
		iConst9 = (1 + iConst8);
		fConst10 = (float(iConst9) - fConst7);
		iConst11 = (1 + (iConst8 & 4095));
		fConst12 = (fConst7 - float(iConst8));
		iConst13 = (1 + (iConst9 & 4095));
		fHslider12 = FAUSTFLOAT(0.35f);
		fHslider13 = FAUSTFLOAT(2.2e+02f);
		for (int i14 = 0; (i14 < 2); i14 = (i14 + 1)) {
			fRec23[i14] = 0.0f;
			
		}
		for (int i15 = 0; (i15 < 2); i15 = (i15 + 1)) {
			fRec22[i15] = 0.0f;
			
		}
		for (int i16 = 0; (i16 < 2); i16 = (i16 + 1)) {
			fRec25[i16] = 0.0f;
			
		}
		for (int i17 = 0; (i17 < 2); i17 = (i17 + 1)) {
			fRec24[i17] = 0.0f;
			
		}
		for (int i18 = 0; (i18 < 2); i18 = (i18 + 1)) {
			fVec1[i18] = 0.0f;
			
		}
		fConst14 = ((347.23f - fConst1) / fConst2);
		for (int i19 = 0; (i19 < 2); i19 = (i19 + 1)) {
			fRec13[i19] = 0.0f;
			
		}
		IOTA = 0;
		for (int i20 = 0; (i20 < 128); i20 = (i20 + 1)) {
			fRec5[i20] = 0.0f;
			
		}
		fConst15 = (0.00018140589f * fConst0);
		iConst16 = int(fConst15);
		fConst17 = (fConst15 - float(iConst16));
		iConst18 = (1 + iConst16);
		iConst19 = (1 + (iConst18 & 4095));
		fConst20 = (float(iConst18) - fConst15);
		iConst21 = (1 + (iConst16 & 4095));
		for (int i21 = 0; (i21 < 2); i21 = (i21 + 1)) {
			fVec2[i21] = 0.0f;
			
		}
		fConst22 = (0.0084f * fConst0);
		fConst23 = (((fConst22 - 347.23f) / (347.23f + fConst22)) - 0.9995f);
		fHslider14 = FAUSTFLOAT(0.12f);
		for (int i22 = 0; (i22 < 2); i22 = (i22 + 1)) {
			fRec30[i22] = 0.0f;
			
		}
		for (int i23 = 0; (i23 < 2); i23 = (i23 + 1)) {
			fRec29[i23] = 0.0f;
			
		}
		for (int i24 = 0; (i24 < 2); i24 = (i24 + 1)) {
			fRec26[i24] = 0.0f;
			
		}
		for (int i25 = 0; (i25 < 2); i25 = (i25 + 1)) {
			fRec4[i25] = 0.0f;
			
		}
		for (int i26 = 0; (i26 < 128); i26 = (i26 + 1)) {
			fRec2[i26] = 0.0f;
			
		}
		for (int i27 = 0; (i27 < 8192); i27 = (i27 + 1)) {
			fRec0[i27] = 0.0f;
			
		}
		fEntry2 = FAUSTFLOAT(1.0f);
		for (int i28 = 0; (i28 < 2); i28 = (i28 + 1)) {
			fRec31[i28] = 0.0f;
			
		}
		for (int i29 = 0; (i29 < 4096); i29 = (i29 + 1)) {
			fVec3[i29] = 0.0f;
			
		}
		fConst24 = (0.5f * fConst0);
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
		interface->declare(&fHslider6, "5", "");
		interface->declare(&fHslider6, "tooltip", "Envelope attack duration");
		interface->declare(&fHslider6, "unit", "s");
		interface->addHorizontalSlider("Envelope_Attack", &fHslider6, 0.01f, 0.0f, 2.0f, 0.01f);
		interface->declare(&fHslider3, "5", "");
		interface->declare(&fHslider3, "tooltip", "Envelope release duration");
		interface->declare(&fHslider3, "unit", "s");
		interface->addHorizontalSlider("Envelope_Release", &fHslider3, 0.1f, 0.0f, 2.0f, 0.01f);
		interface->closeBox();
		interface->openVerticalBox("Vibrato_Parameters");
		interface->declare(&fHslider9, "4", "");
		interface->declare(&fHslider9, "tooltip", "Vibrato attack duration");
		interface->declare(&fHslider9, "unit", "s");
		interface->addHorizontalSlider("Vibrato_Attack", &fHslider9, 0.5f, 0.0f, 2.0f, 0.01f);
		interface->declare(&fHslider11, "4", "");
		interface->declare(&fHslider11, "unit", "Hz");
		interface->addHorizontalSlider("Vibrato_Freq", &fHslider11, 5.0f, 1.0f, 15.0f, 0.1f);
		interface->declare(&fHslider8, "4", "");
		interface->declare(&fHslider8, "tooltip", "A value between 0 and 1");
		interface->addHorizontalSlider("Vibrato_Gain", &fHslider8, 0.1f, 0.0f, 1.0f, 0.01f);
		interface->declare(&fHslider10, "4", "");
		interface->declare(&fHslider10, "tooltip", "Vibrato release duration");
		interface->declare(&fHslider10, "unit", "s");
		interface->addHorizontalSlider("Vibrato_Release", &fHslider10, 0.01f, 0.0f, 2.0f, 0.01f);
		interface->closeBox();
		interface->closeBox();
		interface->openHorizontalBox("Physical_and_Nonlinearity");
		interface->openVerticalBox("Nonlinear_Filter_Parameters");
		interface->declare(&fHslider13, "3", "");
		interface->declare(&fHslider13, "tooltip", "Frequency of the sine wave for the modulation of theta (works if Modulation Type=3)");
		interface->declare(&fHslider13, "unit", "Hz");
		interface->addHorizontalSlider("Modulation_Frequency", &fHslider13, 2.2e+02f, 2e+01f, 1e+03f, 0.1f);
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
		interface->declare(&fHslider7, "2", "");
		interface->declare(&fHslider7, "tooltip", "Breath noise gain (value between 0 and 1)");
		interface->addHorizontalSlider("Noise_Gain", &fHslider7, 0.0f, 0.0f, 1.0f, 0.01f);
		interface->declare(&fHslider5, "2", "");
		interface->declare(&fHslider5, "tooltip", "Breath pressure (value bewteen 0 and 1)");
		interface->addHorizontalSlider("Pressure", &fHslider5, 0.35f, 0.0f, 1.0f, 0.01f);
		interface->declare(&fHslider12, "2", "");
		interface->declare(&fHslider12, "tooltip", "Reed stiffness (value between 0 and 1)");
		interface->addHorizontalSlider("Reed_Stiffness", &fHslider12, 0.35f, 0.0f, 1.0f, 0.01f);
		interface->declare(&fHslider14, "2", "");
		interface->declare(&fHslider14, "tooltip", "A value between 0 and 1");
		interface->addHorizontalSlider("Tone_Hole_Openness", &fHslider14, 0.12f, 0.0f, 1.0f, 0.01f);
		interface->declare(&fHslider4, "2", "");
		interface->declare(&fHslider4, "tooltip", "A value between 0 and 1");
		interface->addHorizontalSlider("Vent_Openness", &fHslider4, 0.0f, 0.0f, 1.0f, 0.01f);
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
		float fSlow1 = (1.5f * (1.0f - fSlow0));
		float fSlow2 = float(fEntry0);
		int iSlow3 = (fSlow2 < 3.0f);
		float fSlow4 = float(fHslider1);
		float fSlow5 = (0.001f * fSlow4);
		float fSlow6 = (2.0f * (fSlow4 * float((fSlow2 < 2.0f))));
		float fSlow7 = float(fEntry1);
		float fSlow8 = (fConst0 * ((0.5f / fSlow7) - 0.00040816327f));
		int iSlow9 = int((fSlow8 - (3.5f + fSlow6)));
		float fSlow10 = (fSlow6 + float(iSlow9));
		float fSlow11 = ((fSlow10 + 4.5f) - fSlow8);
		int iSlow12 = (1 + (iSlow9 & 4095));
		float fSlow13 = (fSlow8 - (3.5f + fSlow10));
		int iSlow14 = (1 + ((1 + iSlow9) & 4095));
		float fSlow15 = float(fButton0);
		int iSlow16 = (fSlow15 > 0.0f);
		float fSlow17 = float(fHslider2);
		float fSlow18 = (1.0f / (float((fSlow17 == 0.0f)) + (fConst0 * fSlow17)));
		float fSlow19 = float(fHslider3);
		float fSlow20 = (1.0f - (1.0f / powf(1e+05f, (1.0f / (float((fSlow19 == 0.0f)) + (fConst0 * fSlow19))))));
		int iSlow21 = (fSlow15 <= 0.0f);
		float fSlow22 = (3.1415927f * float((fSlow2 == 0.0f)));
		float fSlow23 = (1.5707964f * float((fSlow2 == 1.0f)));
		float fSlow24 = (3.1415927f * float((fSlow2 == 2.0f)));
		float fSlow25 = (fConst3 * float(fHslider4));
		float fSlow26 = float(fHslider5);
		float fSlow27 = (0.55f + (0.3f * fSlow26));
		float fSlow28 = (float(fHslider6) * fSlow26);
		float fSlow29 = (1.0f / ((fConst0 * fSlow28) + float((fSlow28 == 0.0f))));
		float fSlow30 = (fSlow19 * fSlow26);
		float fSlow31 = (1.0f - (1.0f / powf(1e+05f, (1.0f / ((fConst0 * fSlow30) + float((fSlow30 == 0.0f)))))));
		float fSlow32 = (4.656613e-10f * float(fHslider7));
		float fSlow33 = float(fHslider8);
		float fSlow34 = float(fHslider9);
		float fSlow35 = (1.0f / ((fConst4 * fSlow34) + float(((1.8f * fSlow34) == 0.0f))));
		float fSlow36 = (fConst5 * fSlow34);
		float fSlow37 = (fSlow36 + float(((0.2f * fSlow34) == 0.0f)));
		float fSlow38 = float(fHslider10);
		float fSlow39 = (1.0f - (1.0f / powf(1e+05f, (1.0f / (float((fSlow38 == 0.0f)) + (fConst0 * fSlow38))))));
		float fSlow40 = (fConst6 * float(fHslider11));
		float fSlow41 = ((0.26f * float(fHslider12)) - 0.44f);
		int iSlow42 = (fSlow2 >= 3.0f);
		int iSlow43 = (fSlow2 != 4.0f);
		float fSlow44 = (0.001f * float(fHslider13));
		float fSlow45 = (fSlow7 * float((fSlow2 == 4.0f)));
		float fSlow46 = (0.9995f + (fConst23 * float(fHslider14)));
		float fSlow47 = (0.0f - fSlow46);
		float fSlow48 = (0.001f * float(fEntry2));
		float fSlow49 = (1.5f * fSlow0);
		int iSlow50 = (int((fConst24 * (float(fHslider15) / fSlow7))) & 4095);
		for (int i = 0; (i < count); i = (i + 1)) {
			fRec8[0] = ((0.999f * fRec8[1]) + fSlow5);
			float fTemp0 = ((fSlow11 * fRec0[((IOTA - iSlow12) & 8191)]) + (fSlow13 * fRec0[((IOTA - iSlow14) & 8191)]));
			fVec0[0] = fTemp0;
			iRec9[0] = (iSlow16 & (iRec9[1] | (fRec10[1] >= 1.0f)));
			int iTemp1 = (iSlow21 & (fRec10[1] > 0.0f));
			fRec10[0] = (((fSlow18 * float((((iRec9[1] == 0) & iSlow16) & (fRec10[1] < 1.0f)))) + (fRec10[1] * (1.0f - (fSlow20 * float(iTemp1))))) * float(((iTemp1 == 0) | (fRec10[1] >= 1e-06f))));
			float fTemp2 = (fRec8[0] * fRec10[0]);
			float fTemp3 = (fTemp2 * (((fSlow22 * fTemp0) + (fSlow23 * (fTemp0 + fVec0[1]))) + (fSlow24 * faustpower2_f(fTemp0))));
			float fTemp4 = sinf(fTemp3);
			float fTemp5 = (0.0f - fTemp4);
			float fTemp6 = cosf(fTemp3);
			float fTemp7 = ((fRec11[1] * fTemp5) + (fTemp0 * fTemp6));
			fRec12[0] = ((fTemp5 * fRec12[1]) + (fTemp6 * fTemp7));
			fRec11[0] = ((fTemp4 * fTemp7) + (fTemp6 * fRec12[1]));
			float fTemp8 = (float(iSlow3) * ((fRec8[0] * ((fTemp0 * fTemp4) + (fRec11[1] * fTemp6))) + ((1.0f - fRec8[0]) * fTemp0)));
			iRec14[0] = (iSlow16 & (iRec14[1] | (fRec15[1] >= 1.0f)));
			int iTemp9 = (iSlow21 & (fRec15[1] > 0.0f));
			fRec15[0] = (((fSlow29 * float((((iRec14[1] == 0) & iSlow16) & (fRec15[1] < 1.0f)))) + (fRec15[1] * (1.0f - (fSlow31 * float(iTemp9))))) * float(((iTemp9 == 0) | (fRec15[1] >= 1e-06f))));
			iRec16[0] = (12345 + (1103515245 * iRec16[1]));
			iRec17[0] = (iSlow16 & (iRec17[1] | (fRec19[1] >= 1.0f)));
			iRec18[0] = (iSlow16 * (1 + iRec18[1]));
			int iTemp10 = (iSlow21 & (fRec19[1] > 0.0f));
			fRec19[0] = (((fSlow35 * float((((((iRec17[1] == 0) & iSlow16) & (fRec19[1] < 1.0f)) & (float(iRec18[1]) > fSlow36)) * (1 - (float(iRec18[1]) < fSlow37))))) + (fRec19[1] * (1.0f - (fSlow39 * float(iTemp10))))) * float(((iTemp10 == 0) | (fRec19[1] >= 1e-06f))));
			float fTemp11 = (fRec21[1] + fSlow40);
			fRec21[0] = (fTemp11 - floorf(fTemp11));
			float fTemp12 = (fSlow27 * ((fRec15[0] * (1.0f + (fSlow32 * float(iRec16[0])))) * (1.0f + (fSlow33 * (fRec19[0] * ftbl0Blow_Hole_dspSIG0[int((65536.0f * fRec21[0]))])))));
			float fTemp13 = (((fConst10 * fRec5[((IOTA - iConst11) & 127)]) + (fConst12 * fRec5[((IOTA - iConst13) & 127)])) - fTemp12);
			float fTemp14 = (0.7f + (fSlow41 * fTemp13));
			float fTemp15 = (float((fTemp14 > 1.0f)) + (fTemp14 * float((fTemp14 <= 1.0f))));
			float fTemp16 = (fTemp12 + (fTemp13 * ((fTemp15 * float((fTemp15 >= -1.0f))) - float((fTemp15 < -1.0f)))));
			fRec23[0] = ((0.999f * fRec23[1]) + fSlow44);
			float fTemp17 = (fRec22[1] + (fConst6 * ((float(iSlow43) * fRec23[0]) + fSlow45)));
			fRec22[0] = (fTemp17 - floorf(fTemp17));
			float fTemp18 = (3.1415927f * (fTemp2 * ftbl0Blow_Hole_dspSIG0[int((65536.0f * fRec22[0]))]));
			float fTemp19 = sinf(fTemp18);
			float fTemp20 = (0.0f - fTemp19);
			float fTemp21 = cosf(fTemp18);
			float fTemp22 = ((fRec24[1] * fTemp20) + (fTemp0 * fTemp21));
			fRec25[0] = ((fTemp20 * fRec25[1]) + (fTemp21 * fTemp22));
			fRec24[0] = ((fTemp19 * fTemp22) + (fTemp21 * fRec25[1]));
			float fTemp23 = (float(iSlow42) * ((fTemp0 * fTemp19) + (fRec24[1] * fTemp21)));
			float fTemp24 = (fSlow25 * (fTemp8 + (fTemp16 + fTemp23)));
			fVec1[0] = fTemp24;
			fRec13[0] = ((fTemp24 + fVec1[1]) - (fConst14 * fRec13[1]));
			fRec5[(IOTA & 127)] = (fTemp8 + (fRec13[0] + fTemp23));
			float fRec6 = fRec13[0];
			float fRec7 = fTemp16;
			float fTemp25 = (fRec6 + fRec7);
			float fTemp26 = (fConst17 * fRec2[((IOTA - iConst19) & 127)]);
			float fTemp27 = (fConst20 * fRec2[((IOTA - iConst21) & 127)]);
			float fTemp28 = (fTemp26 + (fTemp25 + fTemp27));
			float fTemp29 = (0.074074075f * (fTemp28 - (2.0f * fRec26[1])));
			float fTemp30 = (fTemp28 - (fRec29[1] + fTemp29));
			fVec2[0] = fTemp30;
			fRec30[0] = (0.0f - ((fVec2[1] + (fSlow47 * fRec30[1])) - (fSlow46 * fTemp30)));
			fRec29[0] = fRec30[0];
			fRec26[0] = fRec29[0];
			float fTemp31 = (0.0f - fTemp29);
			float fRec27 = fTemp31;
			float fRec28 = fTemp31;
			fRec4[0] = (0.5f * (fRec4[1] + (fTemp25 + fRec27)));
			fRec2[(IOTA & 127)] = (0.0f - (0.95f * fRec4[0]));
			float fRec3 = (fTemp26 + (fRec28 + fTemp27));
			fRec0[(IOTA & 8191)] = fRec3;
			float fRec1 = fRec5[((IOTA - 0) & 127)];
			fRec31[0] = ((0.999f * fRec31[1]) + fSlow48);
			float fTemp32 = (fRec1 * fRec31[0]);
			fVec3[(IOTA & 4095)] = fTemp32;
			output0[i] = FAUSTFLOAT((fSlow1 * fTemp32));
			output1[i] = FAUSTFLOAT((fSlow49 * fVec3[((IOTA - iSlow50) & 4095)]));
			fRec8[1] = fRec8[0];
			fVec0[1] = fVec0[0];
			iRec9[1] = iRec9[0];
			fRec10[1] = fRec10[0];
			fRec12[1] = fRec12[0];
			fRec11[1] = fRec11[0];
			iRec14[1] = iRec14[0];
			fRec15[1] = fRec15[0];
			iRec16[1] = iRec16[0];
			iRec17[1] = iRec17[0];
			iRec18[1] = iRec18[0];
			fRec19[1] = fRec19[0];
			fRec21[1] = fRec21[0];
			fRec23[1] = fRec23[0];
			fRec22[1] = fRec22[0];
			fRec25[1] = fRec25[0];
			fRec24[1] = fRec24[0];
			fVec1[1] = fVec1[0];
			fRec13[1] = fRec13[0];
			IOTA = (IOTA + 1);
			fVec2[1] = fVec2[0];
			fRec30[1] = fRec30[0];
			fRec29[1] = fRec29[0];
			fRec26[1] = fRec26[0];
			fRec4[1] = fRec4[0];
			fRec31[1] = fRec31[0];
			
		}
		
	}

	
};



#include "Faust_plugins_template2.cpp"


#endif
