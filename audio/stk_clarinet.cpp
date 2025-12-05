/* ------------------------------------------------------------
author: "Romain Michon"
copyright: "Romain Michon (rmichon@ccrma.stanford.edu)"
name: "Clarinet"
version: "1.0"
Code generated with Faust 2.81.2 (https://faust.grame.fr)
Compilation options: -a ../../Faust_plugins_template.cpp -lang cpp -nvi -ct 1 -cn Clarinet_dsp -es 1 -mcd 16 -mdd 1024 -mdy 33 -single -ftz 0
------------------------------------------------------------ */

#ifndef  __Clarinet_dsp_H__
#define  __Clarinet_dsp_H__


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
#define FAUSTCLASS Clarinet_dsp
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
	
	void instanceInitClarinet_dspSIG0(int sample_rate) {
		for (int l6 = 0; l6 < 2; l6 = l6 + 1) {
			iRec7[l6] = 0;
		}
	}
	
	void fillClarinet_dspSIG0(int count, float* table) {
		for (int i1 = 0; i1 < count; i1 = i1 + 1) {
			iRec7[0] = iRec7[1] + 1;
			table[i1] = std::sin(9.58738e-05f * float(iRec7[0] + -1));
			iRec7[1] = iRec7[0];
		}
	}

};

static Clarinet_dspSIG0* newClarinet_dspSIG0() { return (Clarinet_dspSIG0*)new Clarinet_dspSIG0(); }
static void deleteClarinet_dspSIG0(Clarinet_dspSIG0* dsp) { delete dsp; }

static float *ftbl0Clarinet_dspSIG0; __attribute__((constructor)) static void initialize_ftbl0Clarinet_dspSIG0() { ftbl0Clarinet_dspSIG0 = (float*)calloc(65536, sizeof(float));};
static float Clarinet_dsp_faustpower2_f(float value) {
	return value * value;
}

class Clarinet_dsp final : public dsp {
	
 private:
	
	FAUSTFLOAT fHslider0;
	FAUSTFLOAT fHslider1;
	FAUSTFLOAT fButton0;
	int iRec1[2];
	int fSampleRate;
	float fConst0;
	FAUSTFLOAT fHslider2;
	FAUSTFLOAT fHslider3;
	FAUSTFLOAT fHslider4;
	float fRec2[2];
	FAUSTFLOAT fHslider5;
	int iRec3[2];
	FAUSTFLOAT fHslider6;
	int iRec4[2];
	int iRec5[2];
	FAUSTFLOAT fHslider7;
	float fConst1;
	float fConst2;
	FAUSTFLOAT fHslider8;
	float fRec6[2];
	float fConst3;
	FAUSTFLOAT fHslider9;
	float fRec8[2];
	FAUSTFLOAT fEntry0;
	FAUSTFLOAT fHslider10;
	float fConst4;
	FAUSTFLOAT fEntry1;
	int IOTA0;
	float fVec0[2];
	float fRec10[2];
	int iRec11[2];
	FAUSTFLOAT fHslider11;
	float fRec12[2];
	FAUSTFLOAT fHslider12;
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
	FAUSTFLOAT fHslider13;
	float fRec0[8192];
	FAUSTFLOAT fEntry2;
	float fRec27[2];
	float fVec1[4096];
	FAUSTFLOAT fHslider14;
	
 public:
	Clarinet_dsp() {
	}
	
	void metadata(Meta* m) { 
		m->declare("author", "Romain Michon");
		m->declare("compile_options", "-a ../../Faust_plugins_template.cpp -lang cpp -nvi -ct 1 -cn Clarinet_dsp -es 1 -mcd 16 -mdd 1024 -mdy 33 -single -ftz 0");
		m->declare("copyright", "Romain Michon (rmichon@ccrma.stanford.edu)");
		m->declare("description", "Nonlinear WaveGuide Clarinet");
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
		m->declare("filename", "clarinet.dsp");
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
		m->declare("name", "Clarinet");
		m->declare("reference", "https://ccrma.stanford.edu/~jos/pasp/Woodwinds.html");
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
		Clarinet_dspSIG0* sig0 = newClarinet_dspSIG0();
		sig0->instanceInitClarinet_dspSIG0(sample_rate);
		sig0->fillClarinet_dspSIG0(65536, ftbl0Clarinet_dspSIG0);
		deleteClarinet_dspSIG0(sig0);
	}
	
	void instanceConstants(int sample_rate) {
		fSampleRate = sample_rate;
		fConst0 = std::min<float>(1.92e+05f, std::max<float>(1.0f, float(fSampleRate)));
		fConst1 = 1.8f * fConst0;
		fConst2 = 0.2f * fConst0;
		fConst3 = 1.0f / fConst0;
		fConst4 = 0.5f * fConst0;
	}
	
	void instanceResetUserInterface() {
		fHslider0 = FAUSTFLOAT(0.6f);
		fHslider1 = FAUSTFLOAT(1.0f);
		fButton0 = FAUSTFLOAT(0.0f);
		fHslider2 = FAUSTFLOAT(0.01f);
		fHslider3 = FAUSTFLOAT(0.05f);
		fHslider4 = FAUSTFLOAT(0.1f);
		fHslider5 = FAUSTFLOAT(0.0f);
		fHslider6 = FAUSTFLOAT(0.1f);
		fHslider7 = FAUSTFLOAT(0.5f);
		fHslider8 = FAUSTFLOAT(0.01f);
		fHslider9 = FAUSTFLOAT(5.0f);
		fEntry0 = FAUSTFLOAT(0.0f);
		fHslider10 = FAUSTFLOAT(0.0f);
		fEntry1 = FAUSTFLOAT(4.4e+02f);
		fHslider11 = FAUSTFLOAT(0.1f);
		fHslider12 = FAUSTFLOAT(2.2e+02f);
		fHslider13 = FAUSTFLOAT(0.5f);
		fEntry2 = FAUSTFLOAT(1.0f);
		fHslider14 = FAUSTFLOAT(0.5f);
	}
	
	void instanceClear() {
		for (int l0 = 0; l0 < 2; l0 = l0 + 1) {
			iRec1[l0] = 0;
		}
		for (int l1 = 0; l1 < 2; l1 = l1 + 1) {
			fRec2[l1] = 0.0f;
		}
		for (int l2 = 0; l2 < 2; l2 = l2 + 1) {
			iRec3[l2] = 0;
		}
		for (int l3 = 0; l3 < 2; l3 = l3 + 1) {
			iRec4[l3] = 0;
		}
		for (int l4 = 0; l4 < 2; l4 = l4 + 1) {
			iRec5[l4] = 0;
		}
		for (int l5 = 0; l5 < 2; l5 = l5 + 1) {
			fRec6[l5] = 0.0f;
		}
		for (int l7 = 0; l7 < 2; l7 = l7 + 1) {
			fRec8[l7] = 0.0f;
		}
		IOTA0 = 0;
		for (int l8 = 0; l8 < 2; l8 = l8 + 1) {
			fVec0[l8] = 0.0f;
		}
		for (int l9 = 0; l9 < 2; l9 = l9 + 1) {
			fRec10[l9] = 0.0f;
		}
		for (int l10 = 0; l10 < 2; l10 = l10 + 1) {
			iRec11[l10] = 0;
		}
		for (int l11 = 0; l11 < 2; l11 = l11 + 1) {
			fRec12[l11] = 0.0f;
		}
		for (int l12 = 0; l12 < 2; l12 = l12 + 1) {
			fRec14[l12] = 0.0f;
		}
		for (int l13 = 0; l13 < 2; l13 = l13 + 1) {
			fRec13[l13] = 0.0f;
		}
		for (int l14 = 0; l14 < 2; l14 = l14 + 1) {
			fRec20[l14] = 0.0f;
		}
		for (int l15 = 0; l15 < 2; l15 = l15 + 1) {
			fRec19[l15] = 0.0f;
		}
		for (int l16 = 0; l16 < 2; l16 = l16 + 1) {
			fRec18[l16] = 0.0f;
		}
		for (int l17 = 0; l17 < 2; l17 = l17 + 1) {
			fRec17[l17] = 0.0f;
		}
		for (int l18 = 0; l18 < 2; l18 = l18 + 1) {
			fRec16[l18] = 0.0f;
		}
		for (int l19 = 0; l19 < 2; l19 = l19 + 1) {
			fRec15[l19] = 0.0f;
		}
		for (int l20 = 0; l20 < 2; l20 = l20 + 1) {
			fRec26[l20] = 0.0f;
		}
		for (int l21 = 0; l21 < 2; l21 = l21 + 1) {
			fRec25[l21] = 0.0f;
		}
		for (int l22 = 0; l22 < 2; l22 = l22 + 1) {
			fRec24[l22] = 0.0f;
		}
		for (int l23 = 0; l23 < 2; l23 = l23 + 1) {
			fRec23[l23] = 0.0f;
		}
		for (int l24 = 0; l24 < 2; l24 = l24 + 1) {
			fRec22[l24] = 0.0f;
		}
		for (int l25 = 0; l25 < 2; l25 = l25 + 1) {
			fRec21[l25] = 0.0f;
		}
		for (int l26 = 0; l26 < 2; l26 = l26 + 1) {
			fRec9[l26] = 0.0f;
		}
		for (int l27 = 0; l27 < 8192; l27 = l27 + 1) {
			fRec0[l27] = 0.0f;
		}
		for (int l28 = 0; l28 < 2; l28 = l28 + 1) {
			fRec27[l28] = 0.0f;
		}
		for (int l29 = 0; l29 < 4096; l29 = l29 + 1) {
			fVec1[l29] = 0.0f;
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
	
	Clarinet_dsp* clone() {
		return new Clarinet_dsp();
	}
	
	int getSampleRate() {
		return fSampleRate;
	}
	
	void buildUserInterface(UI* ui_interface) {
		ui_interface->openVerticalBox("Clarinet");
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
		ui_interface->declare(&fHslider2, "5", "");
		ui_interface->declare(&fHslider2, "tooltip", "Envelope attack duration");
		ui_interface->declare(&fHslider2, "unit", "s");
		ui_interface->addHorizontalSlider("Envelope_Attack", &fHslider2, FAUSTFLOAT(0.01f), FAUSTFLOAT(0.0f), FAUSTFLOAT(2.0f), FAUSTFLOAT(0.01f));
		ui_interface->declare(&fHslider3, "5", "");
		ui_interface->declare(&fHslider3, "tooltip", "Envelope decay duration");
		ui_interface->declare(&fHslider3, "unit", "s");
		ui_interface->addHorizontalSlider("Envelope_Decay", &fHslider3, FAUSTFLOAT(0.05f), FAUSTFLOAT(0.0f), FAUSTFLOAT(2.0f), FAUSTFLOAT(0.01f));
		ui_interface->declare(&fHslider4, "5", "");
		ui_interface->declare(&fHslider4, "tooltip", "Envelope release duration");
		ui_interface->declare(&fHslider4, "unit", "s");
		ui_interface->addHorizontalSlider("Envelope_Release", &fHslider4, FAUSTFLOAT(0.1f), FAUSTFLOAT(0.0f), FAUSTFLOAT(2.0f), FAUSTFLOAT(0.01f));
		ui_interface->closeBox();
		ui_interface->openVerticalBox("Vibrato_Parameters");
		ui_interface->declare(&fHslider7, "4", "");
		ui_interface->declare(&fHslider7, "tooltip", "Vibrato attack duration");
		ui_interface->declare(&fHslider7, "unit", "s");
		ui_interface->addHorizontalSlider("Vibrato_Attack", &fHslider7, FAUSTFLOAT(0.5f), FAUSTFLOAT(0.0f), FAUSTFLOAT(2.0f), FAUSTFLOAT(0.01f));
		ui_interface->declare(&fHslider9, "4", "");
		ui_interface->declare(&fHslider9, "unit", "Hz");
		ui_interface->addHorizontalSlider("Vibrato_Freq", &fHslider9, FAUSTFLOAT(5.0f), FAUSTFLOAT(1.0f), FAUSTFLOAT(15.0f), FAUSTFLOAT(0.1f));
		ui_interface->declare(&fHslider6, "4", "");
		ui_interface->declare(&fHslider6, "tooltip", "A value between 0 and 1");
		ui_interface->addHorizontalSlider("Vibrato_Gain", &fHslider6, FAUSTFLOAT(0.1f), FAUSTFLOAT(0.0f), FAUSTFLOAT(1.0f), FAUSTFLOAT(0.01f));
		ui_interface->declare(&fHslider8, "4", "");
		ui_interface->declare(&fHslider8, "tooltip", "Vibrato release duration");
		ui_interface->declare(&fHslider8, "unit", "s");
		ui_interface->addHorizontalSlider("Vibrato_Release", &fHslider8, FAUSTFLOAT(0.01f), FAUSTFLOAT(0.0f), FAUSTFLOAT(2.0f), FAUSTFLOAT(0.01f));
		ui_interface->closeBox();
		ui_interface->closeBox();
		ui_interface->openHorizontalBox("Physical_and_Nonlinearity");
		ui_interface->openVerticalBox("Nonlinear_Filter_Parameters");
		ui_interface->declare(&fHslider12, "3", "");
		ui_interface->declare(&fHslider12, "tooltip", "Frequency of the sine wave for the modulation of theta (works if Modulation Type=3)");
		ui_interface->declare(&fHslider12, "unit", "Hz");
		ui_interface->addHorizontalSlider("Modulation_Frequency", &fHslider12, FAUSTFLOAT(2.2e+02f), FAUSTFLOAT(2e+01f), FAUSTFLOAT(1e+03f), FAUSTFLOAT(0.1f));
		ui_interface->declare(&fEntry0, "3", "");
		ui_interface->declare(&fEntry0, "tooltip", "0=theta is modulated by the incoming signal; 1=theta is modulated by the averaged incoming signal; 2=theta is modulated by the squared incoming signal; 3=theta is modulated by a sine wave of frequency freqMod; 4=theta is modulated by a sine wave of frequency freq;");
		ui_interface->addNumEntry("Modulation_Type", &fEntry0, FAUSTFLOAT(0.0f), FAUSTFLOAT(0.0f), FAUSTFLOAT(4.0f), FAUSTFLOAT(1.0f));
		ui_interface->declare(&fHslider10, "3", "");
		ui_interface->declare(&fHslider10, "tooltip", "Nonlinearity factor (value between 0 and 1)");
		ui_interface->addHorizontalSlider("Nonlinearity", &fHslider10, FAUSTFLOAT(0.0f), FAUSTFLOAT(0.0f), FAUSTFLOAT(1.0f), FAUSTFLOAT(0.01f));
		ui_interface->declare(&fHslider11, "3", "");
		ui_interface->declare(&fHslider11, "Attack duration of the nonlinearity", "");
		ui_interface->declare(&fHslider11, "unit", "s");
		ui_interface->addHorizontalSlider("Nonlinearity_Attack", &fHslider11, FAUSTFLOAT(0.1f), FAUSTFLOAT(0.0f), FAUSTFLOAT(2.0f), FAUSTFLOAT(0.01f));
		ui_interface->closeBox();
		ui_interface->openVerticalBox("Physical_Parameters");
		ui_interface->declare(&fHslider5, "2", "");
		ui_interface->declare(&fHslider5, "tooltip", "Breath noise gain (value between 0 and 1)");
		ui_interface->addHorizontalSlider("Noise_Gain", &fHslider5, FAUSTFLOAT(0.0f), FAUSTFLOAT(0.0f), FAUSTFLOAT(1.0f), FAUSTFLOAT(0.01f));
		ui_interface->declare(&fHslider1, "2", "");
		ui_interface->declare(&fHslider1, "tooltip", "Breath pressure (value bewteen 0 and 1)");
		ui_interface->addHorizontalSlider("Pressure", &fHslider1, FAUSTFLOAT(1.0f), FAUSTFLOAT(0.0f), FAUSTFLOAT(1.0f), FAUSTFLOAT(0.01f));
		ui_interface->declare(&fHslider13, "2", "");
		ui_interface->declare(&fHslider13, "tooltip", "Reed stiffness (value between 0 and 1)");
		ui_interface->addHorizontalSlider("Reed_Stiffness", &fHslider13, FAUSTFLOAT(0.5f), FAUSTFLOAT(0.0f), FAUSTFLOAT(1.0f), FAUSTFLOAT(0.01f));
		ui_interface->closeBox();
		ui_interface->closeBox();
		ui_interface->openVerticalBox("Spat");
		ui_interface->addHorizontalSlider("pan angle", &fHslider0, FAUSTFLOAT(0.6f), FAUSTFLOAT(0.0f), FAUSTFLOAT(1.0f), FAUSTFLOAT(0.01f));
		ui_interface->addHorizontalSlider("spatial width", &fHslider14, FAUSTFLOAT(0.5f), FAUSTFLOAT(0.0f), FAUSTFLOAT(1.0f), FAUSTFLOAT(0.01f));
		ui_interface->closeBox();
		ui_interface->closeBox();
	}
	
	void compute(int count, FAUSTFLOAT** RESTRICT inputs, FAUSTFLOAT** RESTRICT outputs) {
		FAUSTFLOAT* output0 = outputs[0];
		FAUSTFLOAT* output1 = outputs[1];
		float fSlow0 = float(fHslider0);
		float fSlow1 = 1.5f * (1.0f - fSlow0);
		float fSlow2 = float(fHslider1);
		float fSlow3 = float(fButton0);
		int iSlow4 = fSlow3 > 0.0f;
		float fSlow5 = float(fHslider2);
		float fSlow6 = 1.0f / (fConst0 * fSlow5 + float(fSlow5 == 0.0f));
		float fSlow7 = float(fHslider3);
		float fSlow8 = 1.0f - std::pow(1e+02f, 1.0f / (fConst0 * fSlow7 + float(fSlow7 == 0.0f)));
		float fSlow9 = float(fHslider4);
		float fSlow10 = 1.0f - 1.0f / std::pow(1e+05f, 1.0f / (fConst0 * fSlow9 + float(fSlow9 == 0.0f)));
		int iSlow11 = fSlow3 <= 0.0f;
		float fSlow12 = 4.1909515e-10f * float(fHslider5);
		float fSlow13 = float(fHslider6);
		float fSlow14 = float(fHslider7);
		float fSlow15 = 1.0f / (float((1.8f * fSlow14) == 0.0f) + fConst1 * fSlow14);
		float fSlow16 = fConst2 * fSlow14;
		float fSlow17 = float((0.2f * fSlow14) == 0.0f) + fSlow16;
		float fSlow18 = float(fHslider8);
		float fSlow19 = 1.0f - 1.0f / std::pow(1e+05f, 1.0f / (fConst0 * fSlow18 + float(fSlow18 == 0.0f)));
		float fSlow20 = fConst3 * float(fHslider9);
		float fSlow21 = float(fEntry0);
		float fSlow22 = float(fSlow21 >= 3.0f);
		float fSlow23 = float(fHslider10);
		float fSlow24 = 6.0f * fSlow23 * float(fSlow21 < 2.0f);
		float fSlow25 = float(fEntry1);
		float fSlow26 = fConst4 / fSlow25;
		int iSlow27 = int(fSlow26 + (-1.5f - fSlow24));
		float fSlow28 = fSlow24 + float(iSlow27);
		float fSlow29 = fSlow28 + (2.5f - fSlow26);
		int iSlow30 = (iSlow27 & 4095) + 1;
		float fSlow31 = fSlow26 + (-1.5f - fSlow28);
		int iSlow32 = ((iSlow27 + 1) & 4095) + 1;
		float fSlow33 = 0.001f * fSlow23;
		float fSlow34 = float(fHslider11);
		float fSlow35 = 1.0f / (fConst0 * fSlow34 + float(fSlow34 == 0.0f));
		float fSlow36 = fSlow25 * float(fSlow21 == 4.0f);
		float fSlow37 = float(fSlow21 != 4.0f);
		float fSlow38 = 0.001f * float(fHslider12);
		float fSlow39 = float(fSlow21 < 3.0f);
		float fSlow40 = 3.1415927f * float(fSlow21 == 0.0f);
		float fSlow41 = 1.5707964f * float(fSlow21 == 1.0f);
		float fSlow42 = 3.1415927f * float(fSlow21 == 2.0f);
		float fSlow43 = 0.26f * float(fHslider13) + -0.44f;
		float fSlow44 = 0.001f * float(fEntry2);
		float fSlow45 = 1.5f * fSlow0;
		int iSlow46 = int(fConst4 * (float(fHslider14) / fSlow25)) & 4095;
		for (int i0 = 0; i0 < count; i0 = i0 + 1) {
			iRec1[0] = iSlow4 & (iRec1[1] | (fRec2[1] >= 1.0f));
			int iTemp0 = iSlow11 & (fRec2[1] > 0.0f);
			fRec2[0] = (fSlow6 * float(((iRec1[1] == 0) & iSlow4) & (fRec2[1] < 1.0f)) + fRec2[1] * (1.0f - fSlow8 * float(iRec1[1] & (fRec2[1] > 1e+02f)) - fSlow10 * float(iTemp0))) * float((iTemp0 == 0) | (fRec2[1] >= 1e-06f));
			iRec3[0] = 1103515245 * iRec3[1] + 12345;
			iRec4[0] = iSlow4 & (iRec4[1] | (fRec6[1] >= 1.0f));
			iRec5[0] = iSlow4 * (iRec5[1] + 1);
			float fTemp1 = float(iRec5[1]);
			int iTemp2 = iSlow11 & (fRec6[1] > 0.0f);
			fRec6[0] = (fSlow15 * float(((((iRec4[1] == 0) & iSlow4) & (fRec6[1] < 1.0f)) & (fTemp1 > fSlow16)) * (1 - (fTemp1 < fSlow17))) + fRec6[1] * (1.0f - fSlow19 * float(iTemp2))) * float((iTemp2 == 0) | (fRec6[1] >= 1e-06f));
			fRec8[0] = fSlow20 + (fRec8[1] - std::floor(fSlow20 + fRec8[1]));
			float fTemp3 = fSlow2 * fRec2[0] * (fSlow12 * float(iRec3[0]) + 0.9f) * (fSlow13 * fRec6[0] * ftbl0Clarinet_dspSIG0[std::max<int>(0, std::min<int>(int(65536.0f * fRec8[0]), 65535))] + 1.0f);
			float fTemp4 = fSlow29 * fRec0[(IOTA0 - iSlow30) & 8191] + fSlow31 * fRec0[(IOTA0 - iSlow32) & 8191];
			fVec0[0] = fTemp4;
			fRec10[0] = fSlow33 + 0.999f * fRec10[1];
			iRec11[0] = iSlow4 & (iRec11[1] | (fRec12[1] >= 1.0f));
			int iTemp5 = iSlow11 & (fRec12[1] > 0.0f);
			fRec12[0] = (fSlow35 * float(((iRec11[1] == 0) & iSlow4) & (fRec12[1] < 1.0f)) + fRec12[1] * (1.0f - fSlow10 * float(iTemp5))) * float((iTemp5 == 0) | (fRec12[1] >= 1e-06f));
			float fTemp6 = fRec10[0] * fRec12[0];
			fRec14[0] = fSlow38 + 0.999f * fRec14[1];
			float fTemp7 = fRec13[1] + fConst3 * (fSlow36 + fSlow37 * fRec14[0]);
			fRec13[0] = fTemp7 - std::floor(fTemp7);
			float fTemp8 = 3.1415927f * fTemp6 * ftbl0Clarinet_dspSIG0[std::max<int>(0, std::min<int>(int(65536.0f * fRec13[0]), 65535))];
			float fTemp9 = std::sin(fTemp8);
			float fTemp10 = std::cos(fTemp8);
			float fTemp11 = fTemp4 * fTemp10 - fTemp9 * fRec15[1];
			float fTemp12 = fTemp10 * fTemp11 - fTemp9 * fRec16[1];
			float fTemp13 = fTemp10 * fTemp12 - fTemp9 * fRec17[1];
			float fTemp14 = fTemp10 * fTemp13 - fTemp9 * fRec18[1];
			float fTemp15 = fTemp10 * fTemp14 - fTemp9 * fRec19[1];
			fRec20[0] = fTemp10 * fTemp15 - fTemp9 * fRec20[1];
			fRec19[0] = fTemp9 * fTemp15 + fTemp10 * fRec20[1];
			fRec18[0] = fTemp9 * fTemp14 + fTemp10 * fRec19[1];
			fRec17[0] = fTemp9 * fTemp13 + fTemp10 * fRec18[1];
			fRec16[0] = fTemp9 * fTemp12 + fTemp10 * fRec17[1];
			fRec15[0] = fTemp9 * fTemp11 + fTemp10 * fRec16[1];
			float fTemp16 = fTemp6 * (fSlow40 * fTemp4 + fSlow41 * (fTemp4 + fVec0[1]) + fSlow42 * Clarinet_dsp_faustpower2_f(fTemp4));
			float fTemp17 = std::sin(fTemp16);
			float fTemp18 = std::cos(fTemp16);
			float fTemp19 = fTemp4 * fTemp18 - fTemp17 * fRec21[1];
			float fTemp20 = fTemp18 * fTemp19 - fTemp17 * fRec22[1];
			float fTemp21 = fTemp18 * fTemp20 - fTemp17 * fRec23[1];
			float fTemp22 = fTemp18 * fTemp21 - fTemp17 * fRec24[1];
			float fTemp23 = fTemp18 * fTemp22 - fTemp17 * fRec25[1];
			fRec26[0] = fTemp18 * fTemp23 - fTemp17 * fRec26[1];
			fRec25[0] = fTemp17 * fTemp23 + fTemp18 * fRec26[1];
			fRec24[0] = fTemp17 * fTemp22 + fTemp18 * fRec25[1];
			fRec23[0] = fTemp17 * fTemp21 + fTemp18 * fRec24[1];
			fRec22[0] = fTemp17 * fTemp20 + fTemp18 * fRec23[1];
			fRec21[0] = fTemp17 * fTemp19 + fTemp18 * fRec22[1];
			fRec9[0] = 0.5f * (fRec9[1] + fSlow22 * (fTemp4 * fTemp9 + fRec15[1] * fTemp10) + fSlow39 * (fRec10[0] * (fTemp4 * fTemp17 + fRec21[1] * fTemp18) + (1.0f - fRec10[0]) * fTemp4));
			float fTemp24 = 0.95f * fRec9[0] + fTemp3;
			float fTemp25 = 0.7f - fSlow43 * fTemp24;
			float fTemp26 = float(fTemp25 > 1.0f) + fTemp25 * float(fTemp25 <= 1.0f);
			fRec0[IOTA0 & 8191] = fTemp3 - fTemp24 * (float(-(fTemp26 < -1.0f)) + fTemp26 * float(fTemp26 >= -1.0f));
			fRec27[0] = fSlow44 + 0.999f * fRec27[1];
			float fTemp27 = fRec0[IOTA0 & 8191] * fRec27[0];
			fVec1[IOTA0 & 4095] = fTemp27;
			output0[i0] = FAUSTFLOAT(fSlow1 * fTemp27);
			output1[i0] = FAUSTFLOAT(fSlow45 * fVec1[(IOTA0 - iSlow46) & 4095]);
			iRec1[1] = iRec1[0];
			fRec2[1] = fRec2[0];
			iRec3[1] = iRec3[0];
			iRec4[1] = iRec4[0];
			iRec5[1] = iRec5[0];
			fRec6[1] = fRec6[0];
			fRec8[1] = fRec8[0];
			IOTA0 = IOTA0 + 1;
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
			fRec27[1] = fRec27[0];
		}
	}

};


#include "Faust_plugins_template2.cpp"


#endif
