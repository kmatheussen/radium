/* ------------------------------------------------------------
author: "Romain Michon (rmichon@ccrma.stanford.edu)"
copyright: "Romain Michon"
name: "Modal Bar"
version: "1.0"
Code generated with Faust 2.0.a43 (http://faust.grame.fr)
------------------------------------------------------------ */

#ifndef  __Modal_Bar_dsp_H__
#define  __Modal_Bar_dsp_H__

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
#include <instrument.h>
#include <math.h>
#include <modalBar.h>


class Modal_Bar_dspSIG0 {
	
  private:
	
	int iRec6[2];
	
  public:
	
	int getNumInputsModal_Bar_dspSIG0() {
		return 0;
		
	}
	int getNumOutputsModal_Bar_dspSIG0() {
		return 1;
		
	}
	int getInputRateModal_Bar_dspSIG0(int channel) {
		int rate;
		switch (channel) {
			default: {
				rate = -1;
				break;
			}
			
		}
		return rate;
		
	}
	int getOutputRateModal_Bar_dspSIG0(int channel) {
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
	
	void instanceInitModal_Bar_dspSIG0(int samplingFreq) {
		for (int i4 = 0; (i4 < 2); i4 = (i4 + 1)) {
			iRec6[i4] = 0;
			
		}
		
	}
	
	void fillModal_Bar_dspSIG0(int count, float* output) {
		for (int i = 0; (i < count); i = (i + 1)) {
			iRec6[0] = (1 + iRec6[1]);
			output[i] = float(readMarmstk1(int(((iRec6[0] - 1) % 246))));
			iRec6[1] = iRec6[0];
			
		}
		
	}
};

Modal_Bar_dspSIG0* newModal_Bar_dspSIG0() { return (Modal_Bar_dspSIG0*)new Modal_Bar_dspSIG0(); }
void deleteModal_Bar_dspSIG0(Modal_Bar_dspSIG0* dsp) { delete dsp; }


class Modal_Bar_dspSIG1 {
	
  private:
	
	int iRec18[2];
	
  public:
	
	int getNumInputsModal_Bar_dspSIG1() {
		return 0;
		
	}
	int getNumOutputsModal_Bar_dspSIG1() {
		return 1;
		
	}
	int getInputRateModal_Bar_dspSIG1(int channel) {
		int rate;
		switch (channel) {
			default: {
				rate = -1;
				break;
			}
			
		}
		return rate;
		
	}
	int getOutputRateModal_Bar_dspSIG1(int channel) {
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
	
	void instanceInitModal_Bar_dspSIG1(int samplingFreq) {
		for (int i18 = 0; (i18 < 2); i18 = (i18 + 1)) {
			iRec18[i18] = 0;
			
		}
		
	}
	
	void fillModal_Bar_dspSIG1(int count, float* output) {
		for (int i = 0; (i < count); i = (i + 1)) {
			iRec18[0] = (1 + iRec18[1]);
			output[i] = sinf((9.58738e-05f * float((iRec18[0] - 1))));
			iRec18[1] = iRec18[0];
			
		}
		
	}
};

Modal_Bar_dspSIG1* newModal_Bar_dspSIG1() { return (Modal_Bar_dspSIG1*)new Modal_Bar_dspSIG1(); }
void deleteModal_Bar_dspSIG1(Modal_Bar_dspSIG1* dsp) { delete dsp; }

static float faustpower2_f(float value) {
	return (value * value);
	
}
static float ftbl0Modal_Bar_dspSIG0[246];
static float ftbl1Modal_Bar_dspSIG1[65536];

#ifndef FAUSTCLASS 
#define FAUSTCLASS Modal_Bar_dsp
#endif

class Modal_Bar_dsp : public dsp {
	
  private:
	
	float fVec1[4096];
	float fRec2[3];
	float fRec9[3];
	float fRec12[3];
	float fRec15[3];
	int iRec0[2];
	float fRec1[2];
	float fRec3[2];
	float fRec5[2];
	float fRec7[2];
	float fRec4[2];
	float fRec8[2];
	float fRec10[2];
	float fRec11[2];
	float fRec13[2];
	float fRec14[2];
	float fRec16[2];
	float fRec17[2];
	float fRec19[2];
	float fVec0[2];
	float fRec20[2];
	float fRec22[2];
	float fRec21[2];
	float fRec28[2];
	float fRec27[2];
	float fRec26[2];
	float fRec25[2];
	float fRec24[2];
	float fRec23[2];
	float fRec34[2];
	float fRec33[2];
	float fRec32[2];
	float fRec31[2];
	float fRec30[2];
	float fRec29[2];
	FAUSTFLOAT fHslider0;
	FAUSTFLOAT fButton0;
	FAUSTFLOAT fHslider1;
	int fSamplingFreq;
	float fConst0;
	FAUSTFLOAT fHslider2;
	FAUSTFLOAT fEntry0;
	FAUSTFLOAT fEntry1;
	FAUSTFLOAT fEntry2;
	FAUSTFLOAT fEntry3;
	float fConst1;
	FAUSTFLOAT fEntry4;
	FAUSTFLOAT fHslider3;
	FAUSTFLOAT fHslider4;
	float fConst2;
	FAUSTFLOAT fHslider5;
	FAUSTFLOAT fHslider6;
	FAUSTFLOAT fHslider7;
	int IOTA;
	float fConst3;
	FAUSTFLOAT fHslider8;
	
  public:
	
	void static metadata(Meta* m) { 
		m->declare("author", "Romain Michon (rmichon@ccrma.stanford.edu)");
		m->declare("copyright", "Romain Michon");
		m->declare("description", "Nonlinear Modal percussive instruments");
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
		m->declare("name", "Modal Bar");
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
		Modal_Bar_dspSIG0* sig0 = newModal_Bar_dspSIG0();
		sig0->instanceInitModal_Bar_dspSIG0(samplingFreq);
		sig0->fillModal_Bar_dspSIG0(246, ftbl0Modal_Bar_dspSIG0);
		Modal_Bar_dspSIG1* sig1 = newModal_Bar_dspSIG1();
		sig1->instanceInitModal_Bar_dspSIG1(samplingFreq);
		sig1->fillModal_Bar_dspSIG1(65536, ftbl1Modal_Bar_dspSIG1);
		deleteModal_Bar_dspSIG0(sig0);
		deleteModal_Bar_dspSIG1(sig1);
		
	}
	
	virtual void instanceInit(int samplingFreq) {
		fSamplingFreq = samplingFreq;
		fHslider0 = FAUSTFLOAT(0.6f);
		fButton0 = FAUSTFLOAT(0.0f);
		for (int i0 = 0; (i0 < 2); i0 = (i0 + 1)) {
			iRec0[i0] = 0;
			
		}
		fHslider1 = FAUSTFLOAT(0.0f);
		fConst0 = min(1.92e+05f, max(1.0f, float(fSamplingFreq)));
		fHslider2 = FAUSTFLOAT(0.05f);
		for (int i1 = 0; (i1 < 2); i1 = (i1 + 1)) {
			fRec1[i1] = 0.0f;
			
		}
		fEntry0 = FAUSTFLOAT(0.0f);
		fEntry1 = FAUSTFLOAT(0.8f);
		fEntry2 = FAUSTFLOAT(1.0f);
		fEntry3 = FAUSTFLOAT(1.0f);
		for (int i2 = 0; (i2 < 2); i2 = (i2 + 1)) {
			fRec3[i2] = 0.0f;
			
		}
		fConst1 = (6.2831855f / fConst0);
		fEntry4 = FAUSTFLOAT(4.4e+02f);
		for (int i3 = 0; (i3 < 2); i3 = (i3 + 1)) {
			fRec5[i3] = 0.0f;
			
		}
		fHslider3 = FAUSTFLOAT(0.25f);
		for (int i5 = 0; (i5 < 2); i5 = (i5 + 1)) {
			fRec7[i5] = 0.0f;
			
		}
		for (int i6 = 0; (i6 < 2); i6 = (i6 + 1)) {
			fRec4[i6] = 0.0f;
			
		}
		for (int i7 = 0; (i7 < 2); i7 = (i7 + 1)) {
			fRec8[i7] = 0.0f;
			
		}
		for (int i8 = 0; (i8 < 3); i8 = (i8 + 1)) {
			fRec2[i8] = 0.0f;
			
		}
		for (int i9 = 0; (i9 < 2); i9 = (i9 + 1)) {
			fRec10[i9] = 0.0f;
			
		}
		for (int i10 = 0; (i10 < 2); i10 = (i10 + 1)) {
			fRec11[i10] = 0.0f;
			
		}
		for (int i11 = 0; (i11 < 3); i11 = (i11 + 1)) {
			fRec9[i11] = 0.0f;
			
		}
		for (int i12 = 0; (i12 < 2); i12 = (i12 + 1)) {
			fRec13[i12] = 0.0f;
			
		}
		for (int i13 = 0; (i13 < 2); i13 = (i13 + 1)) {
			fRec14[i13] = 0.0f;
			
		}
		for (int i14 = 0; (i14 < 3); i14 = (i14 + 1)) {
			fRec12[i14] = 0.0f;
			
		}
		for (int i15 = 0; (i15 < 2); i15 = (i15 + 1)) {
			fRec16[i15] = 0.0f;
			
		}
		for (int i16 = 0; (i16 < 2); i16 = (i16 + 1)) {
			fRec17[i16] = 0.0f;
			
		}
		for (int i17 = 0; (i17 < 3); i17 = (i17 + 1)) {
			fRec15[i17] = 0.0f;
			
		}
		fHslider4 = FAUSTFLOAT(0.1f);
		fConst2 = (1.0f / fConst0);
		fHslider5 = FAUSTFLOAT(6.0f);
		for (int i19 = 0; (i19 < 2); i19 = (i19 + 1)) {
			fRec19[i19] = 0.0f;
			
		}
		for (int i20 = 0; (i20 < 2); i20 = (i20 + 1)) {
			fVec0[i20] = 0.0f;
			
		}
		fHslider6 = FAUSTFLOAT(0.0f);
		for (int i21 = 0; (i21 < 2); i21 = (i21 + 1)) {
			fRec20[i21] = 0.0f;
			
		}
		fHslider7 = FAUSTFLOAT(2.2e+02f);
		for (int i22 = 0; (i22 < 2); i22 = (i22 + 1)) {
			fRec22[i22] = 0.0f;
			
		}
		for (int i23 = 0; (i23 < 2); i23 = (i23 + 1)) {
			fRec21[i23] = 0.0f;
			
		}
		for (int i24 = 0; (i24 < 2); i24 = (i24 + 1)) {
			fRec28[i24] = 0.0f;
			
		}
		for (int i25 = 0; (i25 < 2); i25 = (i25 + 1)) {
			fRec27[i25] = 0.0f;
			
		}
		for (int i26 = 0; (i26 < 2); i26 = (i26 + 1)) {
			fRec26[i26] = 0.0f;
			
		}
		for (int i27 = 0; (i27 < 2); i27 = (i27 + 1)) {
			fRec25[i27] = 0.0f;
			
		}
		for (int i28 = 0; (i28 < 2); i28 = (i28 + 1)) {
			fRec24[i28] = 0.0f;
			
		}
		for (int i29 = 0; (i29 < 2); i29 = (i29 + 1)) {
			fRec23[i29] = 0.0f;
			
		}
		for (int i30 = 0; (i30 < 2); i30 = (i30 + 1)) {
			fRec34[i30] = 0.0f;
			
		}
		for (int i31 = 0; (i31 < 2); i31 = (i31 + 1)) {
			fRec33[i31] = 0.0f;
			
		}
		for (int i32 = 0; (i32 < 2); i32 = (i32 + 1)) {
			fRec32[i32] = 0.0f;
			
		}
		for (int i33 = 0; (i33 < 2); i33 = (i33 + 1)) {
			fRec31[i33] = 0.0f;
			
		}
		for (int i34 = 0; (i34 < 2); i34 = (i34 + 1)) {
			fRec30[i34] = 0.0f;
			
		}
		for (int i35 = 0; (i35 < 2); i35 = (i35 + 1)) {
			fRec29[i35] = 0.0f;
			
		}
		IOTA = 0;
		for (int i36 = 0; (i36 < 4096); i36 = (i36 + 1)) {
			fVec1[i36] = 0.0f;
			
		}
		fConst3 = (0.5f * fConst0);
		fHslider8 = FAUSTFLOAT(0.5f);
		
	}
	
	virtual void init(int samplingFreq) {
		classInit(samplingFreq);
		instanceInit(samplingFreq);
	}
	
	virtual void buildUserInterface(UI* interface) {
		interface->openVerticalBox("0x00");
		interface->openHorizontalBox("Basic_Parameters");
		interface->declare(&fEntry4, "1", "");
		interface->declare(&fEntry4, "tooltip", "Tone frequency");
		interface->declare(&fEntry4, "unit", "Hz");
		interface->addNumEntry("freq", &fEntry4, 4.4e+02f, 2e+01f, 2e+04f, 1.0f);
		interface->declare(&fEntry1, "1", "");
		interface->declare(&fEntry1, "tooltip", "Gain (value between 0 and 1)");
		interface->addNumEntry("gain", &fEntry1, 0.8f, 0.0f, 1.0f, 0.01f);
		interface->declare(&fButton0, "1", "");
		interface->declare(&fButton0, "tooltip", "noteOn = 1, noteOff = 0");
		interface->addButton("gate",&fButton0);
		interface->closeBox();
		interface->openHorizontalBox("Envelopes_and_Vibrato");
		interface->openVerticalBox("Global_Envelope_Parameters");
		interface->declare(&fHslider1, "5", "");
		interface->declare(&fHslider1, "tooltip", "Global envelope attack duration");
		interface->declare(&fHslider1, "unit", "s");
		interface->addHorizontalSlider("Glob_Env_Attack", &fHslider1, 0.0f, 0.0f, 2.0f, 0.01f);
		interface->declare(&fHslider2, "5", "");
		interface->declare(&fHslider2, "tooltip", "Global envelope release duration");
		interface->declare(&fHslider2, "unit", "s");
		interface->addHorizontalSlider("Glob_Env_Release", &fHslider2, 0.05f, 0.0f, 2.0f, 0.01f);
		interface->closeBox();
		interface->openVerticalBox("Vibrato_Parameters");
		interface->declare(&fHslider5, "4", "");
		interface->declare(&fHslider5, "unit", "Hz");
		interface->addHorizontalSlider("Vibrato_Freq", &fHslider5, 6.0f, 1.0f, 15.0f, 0.1f);
		interface->declare(&fHslider4, "4", "");
		interface->declare(&fHslider4, "tooltip", "A value between 0 and 1");
		interface->addHorizontalSlider("Vibrato_Gain", &fHslider4, 0.1f, 0.0f, 1.0f, 0.01f);
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
		interface->declare(&fEntry3, "2", "");
		interface->declare(&fEntry3, "tooltip", "0->Marimba, 1->Vibraphone, 2->Agogo, 3->Wood1, 4->Reso, 5->Wood2, 6->Beats, 7->2Fix; 8->Clump");
		interface->addNumEntry("Preset", &fEntry3, 1.0f, 0.0f, 8.0f, 1.0f);
		interface->declare(&fEntry2, "2", "");
		interface->declare(&fEntry2, "tooltip", "A value between 0 and 1");
		interface->addNumEntry("Resonance", &fEntry2, 1.0f, 0.0f, 1.0f, 1.0f);
		interface->declare(&fHslider3, "2", "");
		interface->declare(&fHslider3, "tooltip", "A value between 0 and 1");
		interface->addHorizontalSlider("Stick_Hardness", &fHslider3, 0.25f, 0.0f, 1.0f, 0.01f);
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
		float fSlow1 = (0.3f * (1.0f - fSlow0));
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
		float fSlow12 = (1.0f - (0.03f * (fSlow11 * float(((fSlow2 < 1.0f) & (float(fEntry2) != 1.0f))))));
		float fSlow13 = (2.0f * fSlow12);
		float fSlow14 = float(fEntry3);
		float fSlow15 = float(fEntry4);
		float fSlow16 = faustpower2_f(fSlow12);
		float fSlow17 = (0.1f * fSlow2);
		float fSlow18 = powf(4.0f, float(fHslider3));
		float fSlow19 = (61.5f * fSlow18);
		float fSlow20 = (246.0f * fSlow2);
		float fSlow21 = (0.25f * fSlow18);
		float fSlow22 = (0.001f * float(loadPreset(int(fSlow14), 2, 3)));
		float fSlow23 = (0.001f * float(loadPreset(int(fSlow14), 2, 2)));
		float fSlow24 = (0.001f * float(loadPreset(int(fSlow14), 2, 0)));
		float fSlow25 = (0.001f * float(loadPreset(int(fSlow14), 2, 1)));
		float fSlow26 = float(loadPreset(int(fSlow14), 3, 2));
		float fSlow27 = (float((fSlow14 == 1.0f)) * float(fHslider4));
		float fSlow28 = (fConst2 * float(fHslider5));
		float fSlow29 = (0.001f * float(fHslider6));
		int iSlow30 = (fSlow9 != 4.0f);
		float fSlow31 = (0.001f * float(fHslider7));
		float fSlow32 = (fSlow15 * float((fSlow9 == 4.0f)));
		int iSlow33 = (fSlow9 < 3.0f);
		float fSlow34 = (3.1415927f * float((fSlow9 == 0.0f)));
		float fSlow35 = (1.5707964f * float((fSlow9 == 1.0f)));
		float fSlow36 = (3.1415927f * float((fSlow9 == 2.0f)));
		float fSlow37 = (0.3f * fSlow0);
		int iSlow38 = (int((fConst3 * (float(fHslider8) / fSlow15))) & 4095);
		for (int i = 0; (i < count); i = (i + 1)) {
			iRec0[0] = (iSlow3 & (iRec0[1] | (fRec1[1] >= 1.0f)));
			int iTemp0 = (iSlow8 & (fRec1[1] > 0.0f));
			fRec1[0] = (((fSlow5 * float((((iRec0[1] == 0) & iSlow3) & (fRec1[1] < 1.0f)))) + (fRec1[1] * (1.0f - (fSlow7 * float(iTemp0))))) * float(((iTemp0 == 0) | (fRec1[1] >= 1e-06f))));
			fRec3[0] = ((0.999f * fRec3[1]) + 0.003f);
			float fTemp1 = float(loadPreset(int(fSlow14), 1, int(fRec3[0])));
			float fTemp2 = float(loadPreset(int(fSlow14), 0, int(fRec3[0])));
			int iTemp3 = (fTemp2 < 0.0f);
			fRec5[0] = (1.0f + (fSlow2 * fRec5[1]));
			float fTemp4 = (fRec7[1] + fSlow21);
			fRec7[0] = (fTemp4 - floorf(fTemp4));
			fRec4[0] = ((fSlow17 * (float(((fRec5[0] - 1.0f) < fSlow19)) * ftbl0Modal_Bar_dspSIG0[int((fSlow20 * fRec7[0]))])) + (0.9f * fRec4[1]));
			fRec8[0] = ((0.999f * fRec8[1]) + fSlow22);
			fRec2[0] = (0.0f - (((((0.0f - (fSlow13 * fTemp1)) * cosf((fConst1 * ((float(iTemp3) * (0.0f - fTemp2)) + (fSlow15 * (fTemp2 * float((0 - (iTemp3 - 1))))))))) * fRec2[1]) + (fSlow16 * (faustpower2_f(fTemp1) * fRec2[2]))) - (fSlow11 * (fRec4[0] * fRec8[0]))));
			fRec10[0] = ((0.999f * fRec10[1]) + 0.002f);
			float fTemp5 = float(loadPreset(int(fSlow14), 1, int(fRec10[0])));
			float fTemp6 = float(loadPreset(int(fSlow14), 0, int(fRec10[0])));
			int iTemp7 = (fTemp6 < 0.0f);
			fRec11[0] = ((0.999f * fRec11[1]) + fSlow23);
			fRec9[0] = (0.0f - (((((0.0f - (fSlow13 * fTemp5)) * cosf((fConst1 * ((float(iTemp7) * (0.0f - fTemp6)) + (fSlow15 * (fTemp6 * float((0 - (iTemp7 - 1))))))))) * fRec9[1]) + (fSlow16 * (faustpower2_f(fTemp5) * fRec9[2]))) - (fSlow11 * (fRec4[0] * fRec11[0]))));
			fRec13[0] = (0.999f * fRec13[1]);
			float fTemp8 = float(loadPreset(int(fSlow14), 1, int(fRec13[0])));
			float fTemp9 = float(loadPreset(int(fSlow14), 0, int(fRec13[0])));
			int iTemp10 = (fTemp9 < 0.0f);
			fRec14[0] = ((0.999f * fRec14[1]) + fSlow24);
			fRec12[0] = (0.0f - (((((0.0f - (fSlow13 * fTemp8)) * cosf((fConst1 * ((float(iTemp10) * (0.0f - fTemp9)) + (fSlow15 * (fTemp9 * float((0 - (iTemp10 - 1))))))))) * fRec12[1]) + (fSlow16 * (faustpower2_f(fTemp8) * fRec12[2]))) - (fSlow11 * (fRec4[0] * fRec14[0]))));
			fRec16[0] = ((0.999f * fRec16[1]) + 0.001f);
			float fTemp11 = float(loadPreset(int(fSlow14), 1, int(fRec16[0])));
			float fTemp12 = float(loadPreset(int(fSlow14), 0, int(fRec16[0])));
			int iTemp13 = (fTemp12 < 0.0f);
			fRec17[0] = ((0.999f * fRec17[1]) + fSlow25);
			fRec15[0] = (0.0f - (((((0.0f - (fSlow13 * fTemp11)) * cosf((fConst1 * ((float(iTemp13) * (0.0f - fTemp12)) + (fSlow15 * (fTemp12 * float((0 - (iTemp13 - 1))))))))) * fRec15[1]) + (fSlow16 * (faustpower2_f(fTemp11) * fRec15[2]))) - (fSlow11 * (fRec4[0] * fRec17[0]))));
			float fTemp14 = (fRec2[0] + (fRec9[0] + (fRec12[0] + fRec15[0])));
			float fTemp15 = (fTemp14 + (fSlow26 * ((fSlow11 * fRec4[0]) - fTemp14)));
			float fTemp16 = (fRec19[1] + fSlow28);
			fRec19[0] = (fTemp16 - floorf(fTemp16));
			float fTemp17 = (1.0f + (fSlow27 * ftbl1Modal_Bar_dspSIG1[int((65536.0f * fRec19[0]))]));
			float fTemp18 = (fTemp15 * fTemp17);
			fVec0[0] = fTemp18;
			fRec20[0] = ((0.999f * fRec20[1]) + fSlow29);
			fRec22[0] = ((0.999f * fRec22[1]) + fSlow31);
			float fTemp19 = (fRec21[1] + (fConst2 * ((float(iSlow30) * fRec22[0]) + fSlow32)));
			fRec21[0] = (fTemp19 - floorf(fTemp19));
			float fTemp20 = (3.1415927f * (fRec20[0] * ftbl1Modal_Bar_dspSIG1[int((65536.0f * fRec21[0]))]));
			float fTemp21 = sinf(fTemp20);
			float fTemp22 = (0.0f - fTemp21);
			float fTemp23 = cosf(fTemp20);
			float fTemp24 = ((fRec23[1] * fTemp22) + (fTemp18 * fTemp23));
			float fTemp25 = ((fTemp22 * fRec24[1]) + (fTemp23 * fTemp24));
			float fTemp26 = ((fTemp22 * fRec25[1]) + (fTemp23 * fTemp25));
			float fTemp27 = ((fTemp22 * fRec26[1]) + (fTemp23 * fTemp26));
			float fTemp28 = ((fTemp22 * fRec27[1]) + (fTemp23 * fTemp27));
			fRec28[0] = ((fTemp22 * fRec28[1]) + (fTemp23 * fTemp28));
			fRec27[0] = ((fTemp21 * fTemp28) + (fTemp23 * fRec28[1]));
			fRec26[0] = ((fTemp21 * fTemp27) + (fTemp23 * fRec27[1]));
			fRec25[0] = ((fTemp21 * fTemp26) + (fTemp23 * fRec26[1]));
			fRec24[0] = ((fTemp21 * fTemp25) + (fTemp23 * fRec25[1]));
			fRec23[0] = ((fTemp21 * fTemp24) + (fTemp23 * fRec24[1]));
			float fTemp29 = (fRec20[0] * (((fSlow34 * fTemp18) + (fSlow35 * (fTemp18 + fVec0[1]))) + (fSlow36 * (faustpower2_f(fTemp15) * faustpower2_f(fTemp17)))));
			float fTemp30 = sinf(fTemp29);
			float fTemp31 = (0.0f - fTemp30);
			float fTemp32 = cosf(fTemp29);
			float fTemp33 = ((fRec29[1] * fTemp31) + (fTemp18 * fTemp32));
			float fTemp34 = ((fTemp31 * fRec30[1]) + (fTemp32 * fTemp33));
			float fTemp35 = ((fTemp31 * fRec31[1]) + (fTemp32 * fTemp34));
			float fTemp36 = ((fTemp31 * fRec32[1]) + (fTemp32 * fTemp35));
			float fTemp37 = ((fTemp31 * fRec33[1]) + (fTemp32 * fTemp36));
			fRec34[0] = ((fTemp31 * fRec34[1]) + (fTemp32 * fTemp37));
			fRec33[0] = ((fTemp30 * fTemp37) + (fTemp32 * fRec34[1]));
			fRec32[0] = ((fTemp30 * fTemp36) + (fTemp32 * fRec33[1]));
			fRec31[0] = ((fTemp30 * fTemp35) + (fTemp32 * fRec32[1]));
			fRec30[0] = ((fTemp30 * fTemp34) + (fTemp32 * fRec31[1]));
			fRec29[0] = ((fTemp30 * fTemp33) + (fTemp32 * fRec30[1]));
			float fTemp38 = (fRec1[0] * ((float(iSlow10) * ((fTemp18 * fTemp21) + (fRec23[1] * fTemp23))) + (float(iSlow33) * ((fRec20[0] * ((fTemp18 * fTemp30) + (fRec29[1] * fTemp32))) + (((1.0f - fRec20[0]) * fTemp15) * fTemp17)))));
			fVec1[(IOTA & 4095)] = fTemp38;
			output0[i] = FAUSTFLOAT((fSlow1 * fTemp38));
			output1[i] = FAUSTFLOAT((fSlow37 * fVec1[((IOTA - iSlow38) & 4095)]));
			iRec0[1] = iRec0[0];
			fRec1[1] = fRec1[0];
			fRec3[1] = fRec3[0];
			fRec5[1] = fRec5[0];
			fRec7[1] = fRec7[0];
			fRec4[1] = fRec4[0];
			fRec8[1] = fRec8[0];
			fRec2[2] = fRec2[1];
			fRec2[1] = fRec2[0];
			fRec10[1] = fRec10[0];
			fRec11[1] = fRec11[0];
			fRec9[2] = fRec9[1];
			fRec9[1] = fRec9[0];
			fRec13[1] = fRec13[0];
			fRec14[1] = fRec14[0];
			fRec12[2] = fRec12[1];
			fRec12[1] = fRec12[0];
			fRec16[1] = fRec16[0];
			fRec17[1] = fRec17[0];
			fRec15[2] = fRec15[1];
			fRec15[1] = fRec15[0];
			fRec19[1] = fRec19[0];
			fVec0[1] = fVec0[0];
			fRec20[1] = fRec20[0];
			fRec22[1] = fRec22[0];
			fRec21[1] = fRec21[0];
			fRec28[1] = fRec28[0];
			fRec27[1] = fRec27[0];
			fRec26[1] = fRec26[0];
			fRec25[1] = fRec25[0];
			fRec24[1] = fRec24[0];
			fRec23[1] = fRec23[0];
			fRec34[1] = fRec34[0];
			fRec33[1] = fRec33[0];
			fRec32[1] = fRec32[0];
			fRec31[1] = fRec31[0];
			fRec30[1] = fRec30[0];
			fRec29[1] = fRec29[0];
			IOTA = (IOTA + 1);
			
		}
		
	}

	
};



#include "Faust_plugins_template2.cpp"


#endif
