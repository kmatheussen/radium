/* ------------------------------------------------------------
author: "Romain Michon (rmichon@ccrma.stanford.edu)"
copyright: "Romain Michon"
name: "Brass"
version: "1.0"
Code generated with Faust 2.81.2 (https://faust.grame.fr)
Compilation options: -a ../../Faust_plugins_template.cpp -lang cpp -nvi -ct 1 -cn Brass_dsp -es 1 -mcd 16 -mdd 1024 -mdy 33 -single -ftz 0
------------------------------------------------------------ */

#ifndef  __Brass_dsp_H__
#define  __Brass_dsp_H__


/*
#include "/usr/share/faust/audio/dsp.h"
#include "/usr/share/faust/gui/UI.h"
*/

#include <inttypes.h>

// We use faust1 here.
#include <math.h>

/*
struct Meta
{
    void declare (const char* key, const char* value) { }
};
*/

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wfloat-equal"

#include "faust/gui/meta.h"

#if __GNUC__ >= 5
#  pragma GCC diagnostic push
#  pragma GCC diagnostic ignored "-Wsuggest-override"
#endif

#include <faust/dsp/dsp.h>

#pragma GCC diagnostic pop


#if 0 //CREATE_NAME==create_zita_rev_plugin

  #include "mfaustqt1.cpp"

#else

#pragma clang diagnostic push
#pragma clang diagnostic ignored "-Wshorten-64-to-32"

//  #include <faust/gui/faustqt.h>
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wfloat-equal"
  #include <faust/gui/QTUI.h>
#pragma GCC diagnostic pop

#pragma clang diagnostic pop

#endif


#include "../common/nsmtracker.h"
#include "../Qt/FocusSniffers.h"


#if __GNUC__ >= 5
#  pragma GCC diagnostic pop
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

#include <algorithm>
#include <cmath>
#include <cstdint>
#include <math.h>

#ifndef FAUSTCLASS 
#define FAUSTCLASS Brass_dsp
#endif

#ifdef __APPLE__ 
#define exp10f __exp10f
#define exp10 __exp10
#endif

#if defined(_WIN32)
#define RESTRICT __restrict
#else
#define RESTRICT __restrict__
#endif

class Brass_dspSIG0 {
	
  private:
	
	int iRec5[2];
	
  public:
	
	int getNumInputsBrass_dspSIG0() {
		return 0;
	}
	int getNumOutputsBrass_dspSIG0() {
		return 1;
	}
	
	void instanceInitBrass_dspSIG0(int sample_rate) {
		for (int l4 = 0; l4 < 2; l4 = l4 + 1) {
			iRec5[l4] = 0;
		}
	}
	
	void fillBrass_dspSIG0(int count, float* table) {
		for (int i1 = 0; i1 < count; i1 = i1 + 1) {
			iRec5[0] = iRec5[1] + 1;
			table[i1] = std::sin(9.58738e-05f * float(iRec5[0] + -1));
			iRec5[1] = iRec5[0];
		}
	}

};

static Brass_dspSIG0* newBrass_dspSIG0() { return (Brass_dspSIG0*)new Brass_dspSIG0(); }
static void deleteBrass_dspSIG0(Brass_dspSIG0* dsp) { delete dsp; }

static float *ftbl0Brass_dspSIG0; __attribute__((constructor)) static void initialize_ftbl0Brass_dspSIG0() { ftbl0Brass_dspSIG0 = (float*)calloc(65536, sizeof(float));};
static float Brass_dsp_faustpower2_f(float value) {
	return value * value;
}

class Brass_dsp final : public dsp {
	
 private:
	
	FAUSTFLOAT fHslider0;
	FAUSTFLOAT fEntry0;
	FAUSTFLOAT fHslider1;
	int fSampleRate;
	float fConst0;
	float fConst1;
	FAUSTFLOAT fEntry1;
	int IOTA0;
	float fVec0[2];
	FAUSTFLOAT fHslider2;
	float fRec2[2];
	FAUSTFLOAT fButton0;
	int iRec3[2];
	FAUSTFLOAT fHslider3;
	FAUSTFLOAT fHslider4;
	float fRec4[2];
	float fConst2;
	FAUSTFLOAT fHslider5;
	float fRec7[2];
	float fRec6[2];
	float fRec13[2];
	float fRec12[2];
	float fRec11[2];
	float fRec10[2];
	float fRec9[2];
	float fRec8[2];
	float fRec19[2];
	float fRec18[2];
	float fRec17[2];
	float fRec16[2];
	float fRec15[2];
	float fRec14[2];
	FAUSTFLOAT fHslider6;
	int iRec21[2];
	FAUSTFLOAT fHslider7;
	FAUSTFLOAT fHslider8;
	float fRec22[2];
	FAUSTFLOAT fHslider9;
	int iRec23[2];
	int iRec24[2];
	FAUSTFLOAT fHslider10;
	FAUSTFLOAT fHslider11;
	FAUSTFLOAT fHslider12;
	float fRec25[2];
	FAUSTFLOAT fHslider13;
	float fRec26[2];
	float fConst3;
	FAUSTFLOAT fHslider14;
	float fRec20[3];
	float fVec1[2];
	float fRec1[2];
	float fRec0[8192];
	FAUSTFLOAT fEntry2;
	float fRec27[2];
	float fVec2[4096];
	float fConst4;
	FAUSTFLOAT fHslider15;
	
 public:
	Brass_dsp() {
	}
	
	void metadata(Meta* m) { 
		m->declare("author", "Romain Michon (rmichon@ccrma.stanford.edu)");
		m->declare("compile_options", "-a ../../Faust_plugins_template.cpp -lang cpp -nvi -ct 1 -cn Brass_dsp -es 1 -mcd 16 -mdd 1024 -mdy 33 -single -ftz 0");
		m->declare("copyright", "Romain Michon");
		m->declare("description", "WaveGuide Brass instrument from STK");
		m->declare("effect.lib/author", "Julius O. Smith (jos at ccrma.stanford.edu)");
		m->declare("effect.lib/copyright", "Julius O. Smith III");
		m->declare("effect.lib/deprecated", "This library is deprecated and is not maintained anymore. It will be removed in August 2017.");
		m->declare("effect.lib/exciter_author", "Priyanka Shekar (pshekar@ccrma.stanford.edu)");
		m->declare("effect.lib/exciter_copyright", "Copyright (c) 2013 Priyanka Shekar");
		m->declare("effect.lib/exciter_license", "MIT License (MIT)");
		m->declare("effect.lib/exciter_name", "Harmonic Exciter");
		m->declare("effect.lib/exciter_version", "1.0");
		m->declare("effect.lib/license", "STK-4.3");
		m->declare("effect.lib/name", "Faust Audio Effect Library");
		m->declare("effect.lib/version", "1.33");
		m->declare("filename", "brass.dsp");
		m->declare("filter.lib/author", "Julius O. Smith (jos at ccrma.stanford.edu)");
		m->declare("filter.lib/copyright", "Julius O. Smith III");
		m->declare("filter.lib/deprecated", "This library is deprecated and is not maintained anymore. It will be removed in August 2017.");
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
		m->declare("math.lib/deprecated", "This library is deprecated and is not maintained anymore. It will be removed in August 2017.");
		m->declare("math.lib/license", "LGPL with exception");
		m->declare("math.lib/name", "Math Library");
		m->declare("math.lib/version", "1.0");
		m->declare("music.lib/author", "GRAME");
		m->declare("music.lib/copyright", "GRAME");
		m->declare("music.lib/deprecated", "This library is deprecated and is not maintained anymore. It will be removed in August 2017.");
		m->declare("music.lib/license", "LGPL with exception");
		m->declare("music.lib/name", "Music Library");
		m->declare("music.lib/version", "1.0");
		m->declare("name", "Brass");
		m->declare("reference", "https://ccrma.stanford.edu/~jos/pasp/Brasses.html");
		m->declare("version", "1.0");
	}

	static constexpr int getStaticNumInputs() {
		return 0;
	}
	static constexpr int getStaticNumOutputs() {
		return 2;
	}
	int getNumInputs() {
		return 0;
	}
	int getNumOutputs() {
		return 2;
	}
	
	static void classInit(int sample_rate) {
		Brass_dspSIG0* sig0 = newBrass_dspSIG0();
		sig0->instanceInitBrass_dspSIG0(sample_rate);
		sig0->fillBrass_dspSIG0(65536, ftbl0Brass_dspSIG0);
		deleteBrass_dspSIG0(sig0);
	}
	
	void instanceConstants(int sample_rate) {
		fSampleRate = sample_rate;
		fConst0 = std::min<float>(1.92e+05f, std::max<float>(1.0f, float(fSampleRate)));
		fConst1 = 2.0f * fConst0;
		fConst2 = 1.0f / fConst0;
		fConst3 = 6.2831855f / fConst0;
		fConst4 = 0.5f * fConst0;
	}
	
	void instanceResetUserInterface() {
		fHslider0 = FAUSTFLOAT(0.6f);
		fEntry0 = FAUSTFLOAT(0.0f);
		fHslider1 = FAUSTFLOAT(0.041f);
		fEntry1 = FAUSTFLOAT(4.4e+02f);
		fHslider2 = FAUSTFLOAT(0.0f);
		fButton0 = FAUSTFLOAT(0.0f);
		fHslider3 = FAUSTFLOAT(0.1f);
		fHslider4 = FAUSTFLOAT(0.07f);
		fHslider5 = FAUSTFLOAT(2.2e+02f);
		fHslider6 = FAUSTFLOAT(1.0f);
		fHslider7 = FAUSTFLOAT(0.005f);
		fHslider8 = FAUSTFLOAT(0.001f);
		fHslider9 = FAUSTFLOAT(0.05f);
		fHslider10 = FAUSTFLOAT(0.5f);
		fHslider11 = FAUSTFLOAT(0.05f);
		fHslider12 = FAUSTFLOAT(0.1f);
		fHslider13 = FAUSTFLOAT(6.0f);
		fHslider14 = FAUSTFLOAT(0.78f);
		fEntry2 = FAUSTFLOAT(1.0f);
		fHslider15 = FAUSTFLOAT(0.5f);
	}
	
	void instanceClear() {
		IOTA0 = 0;
		for (int l0 = 0; l0 < 2; l0 = l0 + 1) {
			fVec0[l0] = 0.0f;
		}
		for (int l1 = 0; l1 < 2; l1 = l1 + 1) {
			fRec2[l1] = 0.0f;
		}
		for (int l2 = 0; l2 < 2; l2 = l2 + 1) {
			iRec3[l2] = 0;
		}
		for (int l3 = 0; l3 < 2; l3 = l3 + 1) {
			fRec4[l3] = 0.0f;
		}
		for (int l5 = 0; l5 < 2; l5 = l5 + 1) {
			fRec7[l5] = 0.0f;
		}
		for (int l6 = 0; l6 < 2; l6 = l6 + 1) {
			fRec6[l6] = 0.0f;
		}
		for (int l7 = 0; l7 < 2; l7 = l7 + 1) {
			fRec13[l7] = 0.0f;
		}
		for (int l8 = 0; l8 < 2; l8 = l8 + 1) {
			fRec12[l8] = 0.0f;
		}
		for (int l9 = 0; l9 < 2; l9 = l9 + 1) {
			fRec11[l9] = 0.0f;
		}
		for (int l10 = 0; l10 < 2; l10 = l10 + 1) {
			fRec10[l10] = 0.0f;
		}
		for (int l11 = 0; l11 < 2; l11 = l11 + 1) {
			fRec9[l11] = 0.0f;
		}
		for (int l12 = 0; l12 < 2; l12 = l12 + 1) {
			fRec8[l12] = 0.0f;
		}
		for (int l13 = 0; l13 < 2; l13 = l13 + 1) {
			fRec19[l13] = 0.0f;
		}
		for (int l14 = 0; l14 < 2; l14 = l14 + 1) {
			fRec18[l14] = 0.0f;
		}
		for (int l15 = 0; l15 < 2; l15 = l15 + 1) {
			fRec17[l15] = 0.0f;
		}
		for (int l16 = 0; l16 < 2; l16 = l16 + 1) {
			fRec16[l16] = 0.0f;
		}
		for (int l17 = 0; l17 < 2; l17 = l17 + 1) {
			fRec15[l17] = 0.0f;
		}
		for (int l18 = 0; l18 < 2; l18 = l18 + 1) {
			fRec14[l18] = 0.0f;
		}
		for (int l19 = 0; l19 < 2; l19 = l19 + 1) {
			iRec21[l19] = 0;
		}
		for (int l20 = 0; l20 < 2; l20 = l20 + 1) {
			fRec22[l20] = 0.0f;
		}
		for (int l21 = 0; l21 < 2; l21 = l21 + 1) {
			iRec23[l21] = 0;
		}
		for (int l22 = 0; l22 < 2; l22 = l22 + 1) {
			iRec24[l22] = 0;
		}
		for (int l23 = 0; l23 < 2; l23 = l23 + 1) {
			fRec25[l23] = 0.0f;
		}
		for (int l24 = 0; l24 < 2; l24 = l24 + 1) {
			fRec26[l24] = 0.0f;
		}
		for (int l25 = 0; l25 < 3; l25 = l25 + 1) {
			fRec20[l25] = 0.0f;
		}
		for (int l26 = 0; l26 < 2; l26 = l26 + 1) {
			fVec1[l26] = 0.0f;
		}
		for (int l27 = 0; l27 < 2; l27 = l27 + 1) {
			fRec1[l27] = 0.0f;
		}
		for (int l28 = 0; l28 < 8192; l28 = l28 + 1) {
			fRec0[l28] = 0.0f;
		}
		for (int l29 = 0; l29 < 2; l29 = l29 + 1) {
			fRec27[l29] = 0.0f;
		}
		for (int l30 = 0; l30 < 4096; l30 = l30 + 1) {
			fVec2[l30] = 0.0f;
		}
	}
	
	void init(int sample_rate) {
		classInit(sample_rate);
		instanceInit(sample_rate);
	}
	
	void instanceInit(int sample_rate) {
		instanceConstants(sample_rate);
		instanceResetUserInterface();
		instanceClear();
	}
	
	Brass_dsp* clone() {
		return new Brass_dsp();
	}
	
	int getSampleRate() {
		return fSampleRate;
	}
	
	void buildUserInterface(UI* ui_interface) {
		ui_interface->openVerticalBox("Brass");
		ui_interface->openHorizontalBox("Basic_Parameters");
		ui_interface->declare(&fEntry1, "1", "");
		ui_interface->declare(&fEntry1, "tooltip", "Tone frequency");
		ui_interface->declare(&fEntry1, "unit", "Hz");
		ui_interface->addNumEntry("freq", &fEntry1, FAUSTFLOAT(4.4e+02f), FAUSTFLOAT(2e+01f), FAUSTFLOAT(2e+04f), FAUSTFLOAT(1.0f));
		ui_interface->declare(&fEntry2, "1", "");
		ui_interface->declare(&fEntry2, "tooltip", "Gain (value between 0 and 1)");
		ui_interface->addNumEntry("gain", &fEntry2, FAUSTFLOAT(1.0f), FAUSTFLOAT(0.0f), FAUSTFLOAT(1.0f), FAUSTFLOAT(0.01f));
		ui_interface->declare(&fButton0, "1", "");
		ui_interface->declare(&fButton0, "tooltip", "noteOn = 1, noteOff = 0");
		ui_interface->addButton("gate", &fButton0);
		ui_interface->closeBox();
		ui_interface->openHorizontalBox("Envelopes_and_Vibrato");
		ui_interface->openVerticalBox("Envelope_Parameters");
		ui_interface->declare(&fHslider7, "5", "");
		ui_interface->declare(&fHslider7, "tooltip", "Envelope attack duration");
		ui_interface->declare(&fHslider7, "unit", "s");
		ui_interface->addHorizontalSlider("Envelope_Attack", &fHslider7, FAUSTFLOAT(0.005f), FAUSTFLOAT(0.0f), FAUSTFLOAT(2.0f), FAUSTFLOAT(0.01f));
		ui_interface->declare(&fHslider8, "5", "");
		ui_interface->declare(&fHslider8, "tooltip", "Envelope decay duration");
		ui_interface->declare(&fHslider8, "unit", "s");
		ui_interface->addHorizontalSlider("Envelope_Decay", &fHslider8, FAUSTFLOAT(0.001f), FAUSTFLOAT(0.0f), FAUSTFLOAT(2.0f), FAUSTFLOAT(0.01f));
		ui_interface->declare(&fHslider4, "5", "");
		ui_interface->declare(&fHslider4, "tooltip", "Envelope release duration");
		ui_interface->declare(&fHslider4, "unit", "s");
		ui_interface->addHorizontalSlider("Envelope_Release", &fHslider4, FAUSTFLOAT(0.07f), FAUSTFLOAT(0.0f), FAUSTFLOAT(2.0f), FAUSTFLOAT(0.01f));
		ui_interface->closeBox();
		ui_interface->openVerticalBox("Vibrato_Parameters");
		ui_interface->declare(&fHslider10, "4", "");
		ui_interface->declare(&fHslider10, "tooltip", "Vibrato attack duration");
		ui_interface->declare(&fHslider10, "unit", "s");
		ui_interface->addHorizontalSlider("Vibrato_Attack", &fHslider10, FAUSTFLOAT(0.5f), FAUSTFLOAT(0.0f), FAUSTFLOAT(2.0f), FAUSTFLOAT(0.01f));
		ui_interface->declare(&fHslider11, "4", "");
		ui_interface->declare(&fHslider11, "tooltip", "Vibrato silence duration before attack");
		ui_interface->declare(&fHslider11, "unit", "s");
		ui_interface->addHorizontalSlider("Vibrato_Begin", &fHslider11, FAUSTFLOAT(0.05f), FAUSTFLOAT(0.0f), FAUSTFLOAT(2.0f), FAUSTFLOAT(0.01f));
		ui_interface->declare(&fHslider13, "4", "");
		ui_interface->declare(&fHslider13, "unit", "Hz");
		ui_interface->addHorizontalSlider("Vibrato_Freq", &fHslider13, FAUSTFLOAT(6.0f), FAUSTFLOAT(1.0f), FAUSTFLOAT(15.0f), FAUSTFLOAT(0.1f));
		ui_interface->declare(&fHslider9, "4", "");
		ui_interface->declare(&fHslider9, "tooltip", "A value between 0 and 1");
		ui_interface->addHorizontalSlider("Vibrato_Gain", &fHslider9, FAUSTFLOAT(0.05f), FAUSTFLOAT(0.0f), FAUSTFLOAT(1.0f), FAUSTFLOAT(0.01f));
		ui_interface->declare(&fHslider12, "4", "");
		ui_interface->declare(&fHslider12, "tooltip", "Vibrato release duration");
		ui_interface->declare(&fHslider12, "unit", "s");
		ui_interface->addHorizontalSlider("Vibrato_Release", &fHslider12, FAUSTFLOAT(0.1f), FAUSTFLOAT(0.0f), FAUSTFLOAT(2.0f), FAUSTFLOAT(0.01f));
		ui_interface->closeBox();
		ui_interface->closeBox();
		ui_interface->openHorizontalBox("Physical_and_Nonlinearity");
		ui_interface->openVerticalBox("Nonlinear_Filter_Parameters");
		ui_interface->declare(&fHslider5, "3", "");
		ui_interface->declare(&fHslider5, "tooltip", "Frequency of the sine wave for the modulation of theta (works if Modulation Type=3)");
		ui_interface->declare(&fHslider5, "unit", "Hz");
		ui_interface->addHorizontalSlider("Modulation_Frequency", &fHslider5, FAUSTFLOAT(2.2e+02f), FAUSTFLOAT(2e+01f), FAUSTFLOAT(1e+03f), FAUSTFLOAT(0.1f));
		ui_interface->declare(&fEntry0, "3", "");
		ui_interface->declare(&fEntry0, "tooltip", "0=theta is modulated by the incoming signal; 1=theta is modulated by the averaged incoming signal; 2=theta is modulated by the squared incoming signal; 3=theta is modulated by a sine wave of frequency freqMod; 4=theta is modulated by a sine wave of frequency freq;");
		ui_interface->addNumEntry("Modulation_Type", &fEntry0, FAUSTFLOAT(0.0f), FAUSTFLOAT(0.0f), FAUSTFLOAT(4.0f), FAUSTFLOAT(1.0f));
		ui_interface->declare(&fHslider2, "3", "");
		ui_interface->declare(&fHslider2, "tooltip", "Nonlinearity factor (value between 0 and 1)");
		ui_interface->addHorizontalSlider("Nonlinearity", &fHslider2, FAUSTFLOAT(0.0f), FAUSTFLOAT(0.0f), FAUSTFLOAT(1.0f), FAUSTFLOAT(0.01f));
		ui_interface->declare(&fHslider3, "3", "");
		ui_interface->declare(&fHslider3, "Attack duration of the nonlinearity", "");
		ui_interface->declare(&fHslider3, "unit", "s");
		ui_interface->addHorizontalSlider("Nonlinearity_Attack", &fHslider3, FAUSTFLOAT(0.1f), FAUSTFLOAT(0.0f), FAUSTFLOAT(2.0f), FAUSTFLOAT(0.01f));
		ui_interface->closeBox();
		ui_interface->openVerticalBox("Physical_Parameters");
		ui_interface->declare(&fHslider14, "2", "");
		ui_interface->declare(&fHslider14, "tooltip", "A value between 0 and 1");
		ui_interface->addHorizontalSlider("Lip_Tension", &fHslider14, FAUSTFLOAT(0.78f), FAUSTFLOAT(0.01f), FAUSTFLOAT(1.0f), FAUSTFLOAT(0.001f));
		ui_interface->declare(&fHslider6, "2", "");
		ui_interface->declare(&fHslider6, "tooltip", "A value between 0 and 1");
		ui_interface->addHorizontalSlider("Pressure", &fHslider6, FAUSTFLOAT(1.0f), FAUSTFLOAT(0.01f), FAUSTFLOAT(1.0f), FAUSTFLOAT(0.01f));
		ui_interface->declare(&fHslider1, "2", "");
		ui_interface->declare(&fHslider1, "tooltip", "A value between 0 and 1");
		ui_interface->addHorizontalSlider("Slide_Length", &fHslider1, FAUSTFLOAT(0.041f), FAUSTFLOAT(0.01f), FAUSTFLOAT(1.0f), FAUSTFLOAT(0.001f));
		ui_interface->closeBox();
		ui_interface->closeBox();
		ui_interface->openVerticalBox("Spat");
		ui_interface->addHorizontalSlider("pan angle", &fHslider0, FAUSTFLOAT(0.6f), FAUSTFLOAT(0.0f), FAUSTFLOAT(1.0f), FAUSTFLOAT(0.01f));
		ui_interface->addHorizontalSlider("spatial width", &fHslider15, FAUSTFLOAT(0.5f), FAUSTFLOAT(0.0f), FAUSTFLOAT(1.0f), FAUSTFLOAT(0.01f));
		ui_interface->closeBox();
		ui_interface->closeBox();
	}
	
	void compute(int count, FAUSTFLOAT** RESTRICT inputs, FAUSTFLOAT** RESTRICT outputs) {
		FAUSTFLOAT* output0 = outputs[0];
		FAUSTFLOAT* output1 = outputs[1];
		float fSlow0 = float(fHslider0);
		float fSlow1 = 4.0f * (1.0f - fSlow0);
		float fSlow2 = float(fEntry0);
		float fSlow3 = float(fSlow2 >= 3.0f);
		float fSlow4 = float(fEntry1);
		float fSlow5 = (float(fHslider1) + 0.5f) * (fConst1 / fSlow4 + 3.0f);
		int iSlow6 = int(fSlow5);
		float fSlow7 = float(iSlow6);
		float fSlow8 = fSlow7 + (1.0f - fSlow5);
		int iSlow9 = (iSlow6 & 4095) + 1;
		float fSlow10 = fSlow5 - fSlow7;
		int iSlow11 = ((iSlow6 + 1) & 4095) + 1;
		float fSlow12 = 0.001f * float(fHslider2);
		float fSlow13 = float(fButton0);
		int iSlow14 = fSlow13 > 0.0f;
		float fSlow15 = float(fHslider3);
		float fSlow16 = 1.0f / (fConst0 * fSlow15 + float(fSlow15 == 0.0f));
		float fSlow17 = float(fHslider4);
		float fSlow18 = 1.0f - 1.0f / std::pow(1e+05f, 1.0f / (fConst0 * fSlow17 + float(fSlow17 == 0.0f)));
		int iSlow19 = fSlow13 <= 0.0f;
		float fSlow20 = fSlow4 * float(fSlow2 == 4.0f);
		float fSlow21 = float(fSlow2 != 4.0f);
		float fSlow22 = 0.001f * float(fHslider5);
		float fSlow23 = float(fSlow2 < 3.0f);
		float fSlow24 = 3.1415927f * float(fSlow2 == 0.0f);
		float fSlow25 = 1.5707964f * float(fSlow2 == 1.0f);
		float fSlow26 = 3.1415927f * float(fSlow2 == 2.0f);
		float fSlow27 = float(fHslider6);
		float fSlow28 = float(fHslider7);
		float fSlow29 = 1.0f / (fConst0 * fSlow28 + float(fSlow28 == 0.0f));
		float fSlow30 = float(fHslider8);
		float fSlow31 = 1.0f - std::pow(1e+02f, 1.0f / (fConst0 * fSlow30 + float(fSlow30 == 0.0f)));
		float fSlow32 = float(fHslider9);
		float fSlow33 = float(fHslider10);
		float fSlow34 = 1.0f / (fConst0 * fSlow33 + float(fSlow33 == 0.0f));
		float fSlow35 = float(fHslider11);
		float fSlow36 = fConst0 * fSlow35;
		float fSlow37 = fSlow36 + float(fSlow35 == 0.0f);
		float fSlow38 = float(fHslider12);
		float fSlow39 = 1.0f - 1.0f / std::pow(1e+05f, 1.0f / (fConst0 * fSlow38 + float(fSlow38 == 0.0f)));
		float fSlow40 = fConst2 * float(fHslider13);
		float fSlow41 = 1.994f * std::cos(fConst3 * fSlow4 * std::pow(4.0f, 2.0f * float(fHslider14) + -1.0f));
		float fSlow42 = 0.001f * float(fEntry2);
		float fSlow43 = 4.0f * fSlow0;
		int iSlow44 = int(fConst4 * (float(fHslider15) / fSlow4)) & 4095;
		for (int i0 = 0; i0 < count; i0 = i0 + 1) {
			float fTemp0 = fSlow8 * fRec0[(IOTA0 - iSlow9) & 8191] + fSlow10 * fRec0[(IOTA0 - iSlow11) & 8191];
			fVec0[0] = fTemp0;
			fRec2[0] = fSlow12 + 0.999f * fRec2[1];
			iRec3[0] = iSlow14 & (iRec3[1] | (fRec4[1] >= 1.0f));
			int iTemp1 = iSlow19 & (fRec4[1] > 0.0f);
			fRec4[0] = (fSlow16 * float(((iRec3[1] == 0) & iSlow14) & (fRec4[1] < 1.0f)) + fRec4[1] * (1.0f - fSlow18 * float(iTemp1))) * float((iTemp1 == 0) | (fRec4[1] >= 1e-06f));
			float fTemp2 = fRec2[0] * fRec4[0];
			fRec7[0] = fSlow22 + 0.999f * fRec7[1];
			float fTemp3 = fRec6[1] + fConst2 * (fSlow20 + fSlow21 * fRec7[0]);
			fRec6[0] = fTemp3 - std::floor(fTemp3);
			float fTemp4 = 3.1415927f * fTemp2 * ftbl0Brass_dspSIG0[std::max<int>(0, std::min<int>(int(65536.0f * fRec6[0]), 65535))];
			float fTemp5 = std::sin(fTemp4);
			float fTemp6 = std::cos(fTemp4);
			float fTemp7 = fTemp0 * fTemp6 - fTemp5 * fRec8[1];
			float fTemp8 = fTemp6 * fTemp7 - fTemp5 * fRec9[1];
			float fTemp9 = fTemp6 * fTemp8 - fTemp5 * fRec10[1];
			float fTemp10 = fTemp6 * fTemp9 - fTemp5 * fRec11[1];
			float fTemp11 = fTemp6 * fTemp10 - fTemp5 * fRec12[1];
			fRec13[0] = fTemp6 * fTemp11 - fTemp5 * fRec13[1];
			fRec12[0] = fTemp5 * fTemp11 + fTemp6 * fRec13[1];
			fRec11[0] = fTemp5 * fTemp10 + fTemp6 * fRec12[1];
			fRec10[0] = fTemp5 * fTemp9 + fTemp6 * fRec11[1];
			fRec9[0] = fTemp5 * fTemp8 + fTemp6 * fRec10[1];
			fRec8[0] = fTemp5 * fTemp7 + fTemp6 * fRec9[1];
			float fTemp12 = fTemp2 * (fSlow24 * fTemp0 + fSlow25 * (fTemp0 + fVec0[1]) + fSlow26 * Brass_dsp_faustpower2_f(fTemp0));
			float fTemp13 = std::sin(fTemp12);
			float fTemp14 = std::cos(fTemp12);
			float fTemp15 = fTemp0 * fTemp14 - fTemp13 * fRec14[1];
			float fTemp16 = fTemp14 * fTemp15 - fTemp13 * fRec15[1];
			float fTemp17 = fTemp14 * fTemp16 - fTemp13 * fRec16[1];
			float fTemp18 = fTemp14 * fTemp17 - fTemp13 * fRec17[1];
			float fTemp19 = fTemp14 * fTemp18 - fTemp13 * fRec18[1];
			fRec19[0] = fTemp14 * fTemp19 - fTemp13 * fRec19[1];
			fRec18[0] = fTemp13 * fTemp19 + fTemp14 * fRec19[1];
			fRec17[0] = fTemp13 * fTemp18 + fTemp14 * fRec18[1];
			fRec16[0] = fTemp13 * fTemp17 + fTemp14 * fRec17[1];
			fRec15[0] = fTemp13 * fTemp16 + fTemp14 * fRec16[1];
			fRec14[0] = fTemp13 * fTemp15 + fTemp14 * fRec15[1];
			float fTemp20 = fSlow3 * (fTemp0 * fTemp5 + fRec8[1] * fTemp6) + fSlow23 * (fRec2[0] * (fTemp0 * fTemp13 + fRec14[1] * fTemp14) + (1.0f - fRec2[0]) * fTemp0);
			iRec21[0] = iSlow14 & (iRec21[1] | (fRec22[1] >= 1.0f));
			int iTemp21 = iSlow19 & (fRec22[1] > 0.0f);
			fRec22[0] = (fSlow29 * float(((iRec21[1] == 0) & iSlow14) & (fRec22[1] < 1.0f)) + fRec22[1] * (1.0f - fSlow31 * float(iRec21[1] & (fRec22[1] > 1e+02f)) - fSlow18 * float(iTemp21))) * float((iTemp21 == 0) | (fRec22[1] >= 1e-06f));
			iRec23[0] = iSlow14 & (iRec23[1] | (fRec25[1] >= 1.0f));
			iRec24[0] = iSlow14 * (iRec24[1] + 1);
			float fTemp22 = float(iRec24[1]);
			int iTemp23 = iSlow19 & (fRec25[1] > 0.0f);
			fRec25[0] = (fSlow34 * float(((((iRec23[1] == 0) & iSlow14) & (fRec25[1] < 1.0f)) & (fTemp22 > fSlow36)) * (1 - (fTemp22 < fSlow37))) + fRec25[1] * (1.0f - fSlow39 * float(iTemp23))) * float((iTemp23 == 0) | (fRec25[1] >= 1e-06f));
			fRec26[0] = fSlow40 + (fRec26[1] - std::floor(fSlow40 + fRec26[1]));
			float fTemp24 = fSlow27 * fRec22[0] + fSlow32 * fRec25[0] * ftbl0Brass_dspSIG0[std::max<int>(0, std::min<int>(int(65536.0f * fRec26[0]), 65535))];
			fRec20[0] = 0.03f * (0.3f * fTemp24 - 0.85f * fTemp20) + fSlow41 * fRec20[1] - 0.994009f * fRec20[2];
			float fTemp25 = Brass_dsp_faustpower2_f(fRec20[0]);
			float fTemp26 = float(fTemp25 > 1.0f) + fTemp25 * float(fTemp25 <= 1.0f);
			float fTemp27 = 0.85f * fTemp20 * (1.0f - fTemp26);
			float fTemp28 = 0.3f * fTemp24 * fTemp26;
			fVec1[0] = fTemp28 + fTemp27;
			fRec1[0] = fTemp27 + 0.995f * fRec1[1] + fTemp28 - fVec1[1];
			fRec0[IOTA0 & 8191] = fRec1[0];
			fRec27[0] = fSlow42 + 0.999f * fRec27[1];
			float fTemp29 = fRec0[IOTA0 & 8191] * fRec27[0];
			fVec2[IOTA0 & 4095] = fTemp29;
			output0[i0] = FAUSTFLOAT(fSlow1 * fTemp29);
			output1[i0] = FAUSTFLOAT(fSlow43 * fVec2[(IOTA0 - iSlow44) & 4095]);
			IOTA0 = IOTA0 + 1;
			fVec0[1] = fVec0[0];
			fRec2[1] = fRec2[0];
			iRec3[1] = iRec3[0];
			fRec4[1] = fRec4[0];
			fRec7[1] = fRec7[0];
			fRec6[1] = fRec6[0];
			fRec13[1] = fRec13[0];
			fRec12[1] = fRec12[0];
			fRec11[1] = fRec11[0];
			fRec10[1] = fRec10[0];
			fRec9[1] = fRec9[0];
			fRec8[1] = fRec8[0];
			fRec19[1] = fRec19[0];
			fRec18[1] = fRec18[0];
			fRec17[1] = fRec17[0];
			fRec16[1] = fRec16[0];
			fRec15[1] = fRec15[0];
			fRec14[1] = fRec14[0];
			iRec21[1] = iRec21[0];
			fRec22[1] = fRec22[0];
			iRec23[1] = iRec23[0];
			iRec24[1] = iRec24[0];
			fRec25[1] = fRec25[0];
			fRec26[1] = fRec26[0];
			fRec20[2] = fRec20[1];
			fRec20[1] = fRec20[0];
			fVec1[1] = fVec1[0];
			fRec1[1] = fRec1[0];
			fRec27[1] = fRec27[0];
		}
	}

};


#include "Faust_plugins_template2.cpp"


#endif
