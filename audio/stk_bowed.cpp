/* ------------------------------------------------------------
author: "Romain Michon"
copyright: "Romain Michon (rmichon@ccrma.stanford.edu)"
name: "Bowed"
version: "1.0"
Code generated with Faust 2.81.2 (https://faust.grame.fr)
Compilation options: -a ../../Faust_plugins_template.cpp -lang cpp -nvi -ct 1 -cn Bowed_dsp -es 1 -mcd 16 -mdd 1024 -mdy 33 -single -ftz 0
------------------------------------------------------------ */

#ifndef  __Bowed_dsp_H__
#define  __Bowed_dsp_H__


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
#define FAUSTCLASS Bowed_dsp
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

class Bowed_dspSIG0 {
	
  private:
	
	int iRec10[2];
	
  public:
	
	int getNumInputsBowed_dspSIG0() {
		return 0;
	}
	int getNumOutputsBowed_dspSIG0() {
		return 1;
	}
	
	void instanceInitBowed_dspSIG0(int sample_rate) {
		for (int l6 = 0; l6 < 2; l6 = l6 + 1) {
			iRec10[l6] = 0;
		}
	}
	
	void fillBowed_dspSIG0(int count, float* table) {
		for (int i1 = 0; i1 < count; i1 = i1 + 1) {
			iRec10[0] = iRec10[1] + 1;
			table[i1] = std::sin(9.58738e-05f * float(iRec10[0] + -1));
			iRec10[1] = iRec10[0];
		}
	}

};

static Bowed_dspSIG0* newBowed_dspSIG0() { return (Bowed_dspSIG0*)new Bowed_dspSIG0(); }
static void deleteBowed_dspSIG0(Bowed_dspSIG0* dsp) { delete dsp; }

static float *ftbl0Bowed_dspSIG0; __attribute__((constructor)) static void initialize_ftbl0Bowed_dspSIG0() { ftbl0Bowed_dspSIG0 = (float*)calloc(65536, sizeof(float));};
static float Bowed_dsp_faustpower2_f(float value) {
	return value * value;
}

class Bowed_dsp final : public dsp {
	
 private:
	
	FAUSTFLOAT fEntry0;
	FAUSTFLOAT fHslider0;
	FAUSTFLOAT fButton0;
	int iRec4[2];
	int fSampleRate;
	float fConst0;
	FAUSTFLOAT fHslider1;
	FAUSTFLOAT fHslider2;
	FAUSTFLOAT fHslider3;
	float fRec5[2];
	float fConst1;
	float fConst2;
	FAUSTFLOAT fEntry1;
	int IOTA0;
	FAUSTFLOAT fHslider4;
	FAUSTFLOAT fEntry2;
	float fVec0[2];
	FAUSTFLOAT fHslider5;
	float fRec7[2];
	int iRec8[2];
	FAUSTFLOAT fHslider6;
	float fRec9[2];
	float fConst3;
	FAUSTFLOAT fHslider7;
	float fRec12[2];
	float fRec11[2];
	float fRec18[2];
	float fRec17[2];
	float fRec16[2];
	float fRec15[2];
	float fRec14[2];
	float fRec13[2];
	float fRec24[2];
	float fRec23[2];
	float fRec22[2];
	float fRec21[2];
	float fRec20[2];
	float fRec19[2];
	float fConst4;
	float fRec6[2];
	FAUSTFLOAT fHslider8;
	int iRec25[2];
	int iRec26[2];
	FAUSTFLOAT fHslider9;
	FAUSTFLOAT fHslider10;
	FAUSTFLOAT fHslider11;
	float fRec27[2];
	FAUSTFLOAT fHslider12;
	float fRec28[2];
	FAUSTFLOAT fHslider13;
	float fRec2[8192];
	float fRec1[4096];
	float fConst5;
	float fRec0[3];
	float fVec1[4096];
	float fConst6;
	FAUSTFLOAT fHslider14;
	
 public:
	Bowed_dsp() {
	}
	
	void metadata(Meta* m) { 
		m->declare("author", "Romain Michon");
		m->declare("compile_options", "-a ../../Faust_plugins_template.cpp -lang cpp -nvi -ct 1 -cn Bowed_dsp -es 1 -mcd 16 -mdd 1024 -mdy 33 -single -ftz 0");
		m->declare("copyright", "Romain Michon (rmichon@ccrma.stanford.edu)");
		m->declare("description", "Nonlinear WaveGuide Bowed Instrument");
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
		m->declare("filename", "bowed.dsp");
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
		m->declare("name", "Bowed");
		m->declare("reference", "https://ccrma.stanford.edu/~jos/pasp/Bowed_Strings.html");
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
		Bowed_dspSIG0* sig0 = newBowed_dspSIG0();
		sig0->instanceInitBowed_dspSIG0(sample_rate);
		sig0->fillBowed_dspSIG0(65536, ftbl0Bowed_dspSIG0);
		deleteBowed_dspSIG0(sig0);
	}
	
	void instanceConstants(int sample_rate) {
		fSampleRate = sample_rate;
		fConst0 = std::min<float>(1.92e+05f, std::max<float>(1.0f, float(fSampleRate)));
		fConst1 = 2205.0f / fConst0;
		fConst2 = 0.95f * (fConst1 + 0.4f);
		fConst3 = 1.0f / fConst0;
		fConst4 = 0.6f - fConst1;
		fConst5 = 1.7f * std::cos(3141.5928f / fConst0);
		fConst6 = 0.5f * fConst0;
	}
	
	void instanceResetUserInterface() {
		fEntry0 = FAUSTFLOAT(1.0f);
		fHslider0 = FAUSTFLOAT(0.6f);
		fButton0 = FAUSTFLOAT(0.0f);
		fHslider1 = FAUSTFLOAT(0.01f);
		fHslider2 = FAUSTFLOAT(0.05f);
		fHslider3 = FAUSTFLOAT(0.1f);
		fEntry1 = FAUSTFLOAT(0.0f);
		fHslider4 = FAUSTFLOAT(0.7f);
		fEntry2 = FAUSTFLOAT(4.4e+02f);
		fHslider5 = FAUSTFLOAT(0.0f);
		fHslider6 = FAUSTFLOAT(0.1f);
		fHslider7 = FAUSTFLOAT(2.2e+02f);
		fHslider8 = FAUSTFLOAT(0.01f);
		fHslider9 = FAUSTFLOAT(0.5f);
		fHslider10 = FAUSTFLOAT(0.05f);
		fHslider11 = FAUSTFLOAT(0.01f);
		fHslider12 = FAUSTFLOAT(6.0f);
		fHslider13 = FAUSTFLOAT(0.75f);
		fHslider14 = FAUSTFLOAT(0.5f);
	}
	
	void instanceClear() {
		for (int l0 = 0; l0 < 2; l0 = l0 + 1) {
			iRec4[l0] = 0;
		}
		for (int l1 = 0; l1 < 2; l1 = l1 + 1) {
			fRec5[l1] = 0.0f;
		}
		IOTA0 = 0;
		for (int l2 = 0; l2 < 2; l2 = l2 + 1) {
			fVec0[l2] = 0.0f;
		}
		for (int l3 = 0; l3 < 2; l3 = l3 + 1) {
			fRec7[l3] = 0.0f;
		}
		for (int l4 = 0; l4 < 2; l4 = l4 + 1) {
			iRec8[l4] = 0;
		}
		for (int l5 = 0; l5 < 2; l5 = l5 + 1) {
			fRec9[l5] = 0.0f;
		}
		for (int l7 = 0; l7 < 2; l7 = l7 + 1) {
			fRec12[l7] = 0.0f;
		}
		for (int l8 = 0; l8 < 2; l8 = l8 + 1) {
			fRec11[l8] = 0.0f;
		}
		for (int l9 = 0; l9 < 2; l9 = l9 + 1) {
			fRec18[l9] = 0.0f;
		}
		for (int l10 = 0; l10 < 2; l10 = l10 + 1) {
			fRec17[l10] = 0.0f;
		}
		for (int l11 = 0; l11 < 2; l11 = l11 + 1) {
			fRec16[l11] = 0.0f;
		}
		for (int l12 = 0; l12 < 2; l12 = l12 + 1) {
			fRec15[l12] = 0.0f;
		}
		for (int l13 = 0; l13 < 2; l13 = l13 + 1) {
			fRec14[l13] = 0.0f;
		}
		for (int l14 = 0; l14 < 2; l14 = l14 + 1) {
			fRec13[l14] = 0.0f;
		}
		for (int l15 = 0; l15 < 2; l15 = l15 + 1) {
			fRec24[l15] = 0.0f;
		}
		for (int l16 = 0; l16 < 2; l16 = l16 + 1) {
			fRec23[l16] = 0.0f;
		}
		for (int l17 = 0; l17 < 2; l17 = l17 + 1) {
			fRec22[l17] = 0.0f;
		}
		for (int l18 = 0; l18 < 2; l18 = l18 + 1) {
			fRec21[l18] = 0.0f;
		}
		for (int l19 = 0; l19 < 2; l19 = l19 + 1) {
			fRec20[l19] = 0.0f;
		}
		for (int l20 = 0; l20 < 2; l20 = l20 + 1) {
			fRec19[l20] = 0.0f;
		}
		for (int l21 = 0; l21 < 2; l21 = l21 + 1) {
			fRec6[l21] = 0.0f;
		}
		for (int l22 = 0; l22 < 2; l22 = l22 + 1) {
			iRec25[l22] = 0;
		}
		for (int l23 = 0; l23 < 2; l23 = l23 + 1) {
			iRec26[l23] = 0;
		}
		for (int l24 = 0; l24 < 2; l24 = l24 + 1) {
			fRec27[l24] = 0.0f;
		}
		for (int l25 = 0; l25 < 2; l25 = l25 + 1) {
			fRec28[l25] = 0.0f;
		}
		for (int l26 = 0; l26 < 8192; l26 = l26 + 1) {
			fRec2[l26] = 0.0f;
		}
		for (int l27 = 0; l27 < 4096; l27 = l27 + 1) {
			fRec1[l27] = 0.0f;
		}
		for (int l28 = 0; l28 < 3; l28 = l28 + 1) {
			fRec0[l28] = 0.0f;
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
	
	Bowed_dsp* clone() {
		return new Bowed_dsp();
	}
	
	int getSampleRate() {
		return fSampleRate;
	}
	
	void buildUserInterface(UI* ui_interface) {
		ui_interface->openVerticalBox("Bowed");
		ui_interface->openHorizontalBox("Basic_Parameters");
		ui_interface->declare(&fEntry2, "1", "");
		ui_interface->declare(&fEntry2, "tooltip", "Tone frequency");
		ui_interface->declare(&fEntry2, "unit", "Hz");
		ui_interface->addNumEntry("freq", &fEntry2, FAUSTFLOAT(4.4e+02f), FAUSTFLOAT(2e+01f), FAUSTFLOAT(2e+04f), FAUSTFLOAT(1.0f));
		ui_interface->declare(&fEntry0, "1", "");
		ui_interface->declare(&fEntry0, "tooltip", "Gain (value between 0 and 1)");
		ui_interface->addNumEntry("gain", &fEntry0, FAUSTFLOAT(1.0f), FAUSTFLOAT(0.0f), FAUSTFLOAT(1.0f), FAUSTFLOAT(0.01f));
		ui_interface->declare(&fButton0, "1", "");
		ui_interface->declare(&fButton0, "tooltip", "noteOn = 1, noteOff = 0");
		ui_interface->addButton("gate", &fButton0);
		ui_interface->closeBox();
		ui_interface->openHorizontalBox("Envelopes_and_Vibrato");
		ui_interface->openVerticalBox("Envelope_Parameters");
		ui_interface->declare(&fHslider1, "5", "");
		ui_interface->declare(&fHslider1, "tooltip", "Envelope attack duration");
		ui_interface->declare(&fHslider1, "unit", "s");
		ui_interface->addHorizontalSlider("Envelope_Attack", &fHslider1, FAUSTFLOAT(0.01f), FAUSTFLOAT(0.0f), FAUSTFLOAT(2.0f), FAUSTFLOAT(0.01f));
		ui_interface->declare(&fHslider2, "5", "");
		ui_interface->declare(&fHslider2, "tooltip", "Envelope decay duration");
		ui_interface->declare(&fHslider2, "unit", "s");
		ui_interface->addHorizontalSlider("Envelope_Decay", &fHslider2, FAUSTFLOAT(0.05f), FAUSTFLOAT(0.0f), FAUSTFLOAT(2.0f), FAUSTFLOAT(0.01f));
		ui_interface->declare(&fHslider3, "5", "");
		ui_interface->declare(&fHslider3, "tooltip", "Envelope release duration");
		ui_interface->declare(&fHslider3, "unit", "s");
		ui_interface->addHorizontalSlider("Envelope_Release", &fHslider3, FAUSTFLOAT(0.1f), FAUSTFLOAT(0.0f), FAUSTFLOAT(2.0f), FAUSTFLOAT(0.01f));
		ui_interface->closeBox();
		ui_interface->openVerticalBox("Vibrato_Parameters");
		ui_interface->declare(&fHslider9, "4", "");
		ui_interface->declare(&fHslider9, "tooltip", "Vibrato attack duration");
		ui_interface->declare(&fHslider9, "unit", "s");
		ui_interface->addHorizontalSlider("Vibrato_Attack", &fHslider9, FAUSTFLOAT(0.5f), FAUSTFLOAT(0.0f), FAUSTFLOAT(2.0f), FAUSTFLOAT(0.01f));
		ui_interface->declare(&fHslider10, "4", "");
		ui_interface->declare(&fHslider10, "tooltip", "Vibrato silence duration before attack");
		ui_interface->declare(&fHslider10, "unit", "s");
		ui_interface->addHorizontalSlider("Vibrato_Begin", &fHslider10, FAUSTFLOAT(0.05f), FAUSTFLOAT(0.0f), FAUSTFLOAT(2.0f), FAUSTFLOAT(0.01f));
		ui_interface->declare(&fHslider12, "4", "");
		ui_interface->declare(&fHslider12, "unit", "Hz");
		ui_interface->addHorizontalSlider("Vibrato_Freq", &fHslider12, FAUSTFLOAT(6.0f), FAUSTFLOAT(1.0f), FAUSTFLOAT(15.0f), FAUSTFLOAT(0.1f));
		ui_interface->declare(&fHslider8, "4", "");
		ui_interface->declare(&fHslider8, "tooltip", "A value between 0 and 1");
		ui_interface->addHorizontalSlider("Vibrato_Gain", &fHslider8, FAUSTFLOAT(0.01f), FAUSTFLOAT(0.0f), FAUSTFLOAT(1.0f), FAUSTFLOAT(0.01f));
		ui_interface->declare(&fHslider11, "4", "");
		ui_interface->declare(&fHslider11, "tooltip", "Vibrato release duration");
		ui_interface->declare(&fHslider11, "unit", "s");
		ui_interface->addHorizontalSlider("Vibrato_Release", &fHslider11, FAUSTFLOAT(0.01f), FAUSTFLOAT(0.0f), FAUSTFLOAT(2.0f), FAUSTFLOAT(0.01f));
		ui_interface->closeBox();
		ui_interface->closeBox();
		ui_interface->openHorizontalBox("Physical_and_Nonlinearity");
		ui_interface->openVerticalBox("Nonlinear_Filter_Parameters");
		ui_interface->declare(&fHslider7, "3", "");
		ui_interface->declare(&fHslider7, "tooltip", "Frequency of the sine wave for the modulation of theta (works if Modulation Type=3)");
		ui_interface->declare(&fHslider7, "unit", "Hz");
		ui_interface->addHorizontalSlider("Modulation_Frequency", &fHslider7, FAUSTFLOAT(2.2e+02f), FAUSTFLOAT(2e+01f), FAUSTFLOAT(1e+03f), FAUSTFLOAT(0.1f));
		ui_interface->declare(&fEntry1, "3", "");
		ui_interface->declare(&fEntry1, "tooltip", "0=theta is modulated by the incoming signal; 1=theta is modulated by the averaged incoming signal; 2=theta is modulated by the squared incoming signal; 3=theta is modulated by a sine wave of frequency freqMod; 4=theta is modulated by a sine wave of frequency freq;");
		ui_interface->addNumEntry("Modulation_Type", &fEntry1, FAUSTFLOAT(0.0f), FAUSTFLOAT(0.0f), FAUSTFLOAT(4.0f), FAUSTFLOAT(1.0f));
		ui_interface->declare(&fHslider5, "3", "");
		ui_interface->declare(&fHslider5, "tooltip", "Nonlinearity factor (value between 0 and 1)");
		ui_interface->addHorizontalSlider("Nonlinearity", &fHslider5, FAUSTFLOAT(0.0f), FAUSTFLOAT(0.0f), FAUSTFLOAT(1.0f), FAUSTFLOAT(0.01f));
		ui_interface->declare(&fHslider6, "3", "");
		ui_interface->declare(&fHslider6, "Attack duration of the nonlinearity", "");
		ui_interface->declare(&fHslider6, "unit", "s");
		ui_interface->addHorizontalSlider("Nonlinearity_Attack", &fHslider6, FAUSTFLOAT(0.1f), FAUSTFLOAT(0.0f), FAUSTFLOAT(2.0f), FAUSTFLOAT(0.01f));
		ui_interface->closeBox();
		ui_interface->openVerticalBox("Physical_Parameters");
		ui_interface->declare(&fHslider4, "2", "");
		ui_interface->declare(&fHslider4, "tooltip", "Bow position along the string (value between 0 and 1)");
		ui_interface->addHorizontalSlider("Bow_Position", &fHslider4, FAUSTFLOAT(0.7f), FAUSTFLOAT(0.01f), FAUSTFLOAT(1.0f), FAUSTFLOAT(0.01f));
		ui_interface->declare(&fHslider13, "2", "");
		ui_interface->declare(&fHslider13, "tooltip", "Bow pressure on the string (value between 0 and 1)");
		ui_interface->addHorizontalSlider("Bow_Pressure", &fHslider13, FAUSTFLOAT(0.75f), FAUSTFLOAT(0.0f), FAUSTFLOAT(1.0f), FAUSTFLOAT(0.01f));
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
		float fSlow0 = float(fEntry0);
		float fSlow1 = float(fHslider0);
		float fSlow2 = 1.11f * fSlow0 * (1.0f - fSlow1);
		float fSlow3 = 0.2f * fSlow0 + 0.03f;
		float fSlow4 = float(fButton0);
		int iSlow5 = fSlow4 > 0.0f;
		float fSlow6 = fSlow0 * float(fHslider1);
		float fSlow7 = 1.0f / (fConst0 * fSlow6 + float(fSlow6 == 0.0f));
		float fSlow8 = float(fHslider2);
		float fSlow9 = 1.0f - std::pow(9e+01f, 1.0f / (fConst0 * fSlow8 + float(fSlow8 == 0.0f)));
		float fSlow10 = float(fHslider3);
		float fSlow11 = fSlow10 * (1.0f - fSlow0);
		float fSlow12 = 1.0f - 1.0f / std::pow(9e+04f, 1.0f / (fConst0 * fSlow11 + float(fSlow11 == 0.0f)));
		int iSlow13 = fSlow4 <= 0.0f;
		float fSlow14 = float(fEntry1);
		float fSlow15 = float(fSlow14 >= 3.0f);
		float fSlow16 = 0.2f * float(fHslider4);
		float fSlow17 = float(fEntry2);
		float fSlow18 = fConst0 / fSlow17 + -4.0f;
		int iSlow19 = (int((fSlow16 + 0.027236f) * fSlow18) & 4095) + 1;
		float fSlow20 = 0.001f * float(fHslider5);
		float fSlow21 = float(fHslider6);
		float fSlow22 = 1.0f / (fConst0 * fSlow21 + float(fSlow21 == 0.0f));
		float fSlow23 = 1.0f - 1.0f / std::pow(1e+05f, 1.0f / (fConst0 * fSlow10 + float(fSlow10 == 0.0f)));
		float fSlow24 = fSlow17 * float(fSlow14 == 4.0f);
		float fSlow25 = float(fSlow14 != 4.0f);
		float fSlow26 = 0.001f * float(fHslider7);
		float fSlow27 = float(fSlow14 < 3.0f);
		float fSlow28 = 3.1415927f * float(fSlow14 == 0.0f);
		float fSlow29 = 1.5707964f * float(fSlow14 == 1.0f);
		float fSlow30 = 3.1415927f * float(fSlow14 == 2.0f);
		float fSlow31 = float(fHslider8);
		float fSlow32 = float(fHslider9);
		float fSlow33 = 1.0f / (fConst0 * fSlow32 + float(fSlow32 == 0.0f));
		float fSlow34 = float(fHslider10);
		float fSlow35 = fConst0 * fSlow34;
		float fSlow36 = fSlow35 + float(fSlow34 == 0.0f);
		float fSlow37 = float(fHslider11);
		float fSlow38 = 1.0f - 1.0f / std::pow(1e+05f, 1.0f / (fConst0 * fSlow37 + float(fSlow37 == 0.0f)));
		float fSlow39 = fConst3 * float(fHslider12);
		float fSlow40 = 0.972764f - fSlow16;
		float fSlow41 = 5.0f - 4.0f * float(fHslider13);
		float fSlow42 = 1.11f * fSlow0;
		int iSlow43 = int(fConst6 * (float(fHslider14) / fSlow17)) & 4095;
		for (int i0 = 0; i0 < count; i0 = i0 + 1) {
			iRec4[0] = iSlow5 & (iRec4[1] | (fRec5[1] >= 1.0f));
			int iTemp0 = iSlow13 & (fRec5[1] > 0.0f);
			fRec5[0] = (fSlow7 * float(((iRec4[1] == 0) & iSlow5) & (fRec5[1] < 1.0f)) + fRec5[1] * (1.0f - fSlow9 * float(iRec4[1] & (fRec5[1] > 9e+01f)) - fSlow12 * float(iTemp0))) * float((iTemp0 == 0) | (fRec5[1] >= 1e-06f));
			float fTemp1 = fRec1[(IOTA0 - iSlow19) & 4095];
			fVec0[0] = fTemp1;
			fRec7[0] = fSlow20 + 0.999f * fRec7[1];
			iRec8[0] = iSlow5 & (iRec8[1] | (fRec9[1] >= 1.0f));
			int iTemp2 = iSlow13 & (fRec9[1] > 0.0f);
			fRec9[0] = (fSlow22 * float(((iRec8[1] == 0) & iSlow5) & (fRec9[1] < 1.0f)) + fRec9[1] * (1.0f - fSlow23 * float(iTemp2))) * float((iTemp2 == 0) | (fRec9[1] >= 1e-06f));
			float fTemp3 = fRec7[0] * fRec9[0];
			fRec12[0] = fSlow26 + 0.999f * fRec12[1];
			float fTemp4 = fRec11[1] + fConst3 * (fSlow24 + fSlow25 * fRec12[0]);
			fRec11[0] = fTemp4 - std::floor(fTemp4);
			float fTemp5 = 3.1415927f * fTemp3 * ftbl0Bowed_dspSIG0[std::max<int>(0, std::min<int>(int(65536.0f * fRec11[0]), 65535))];
			float fTemp6 = std::sin(fTemp5);
			float fTemp7 = std::cos(fTemp5);
			float fTemp8 = fTemp1 * fTemp7 - fTemp6 * fRec13[1];
			float fTemp9 = fTemp7 * fTemp8 - fTemp6 * fRec14[1];
			float fTemp10 = fTemp7 * fTemp9 - fTemp6 * fRec15[1];
			float fTemp11 = fTemp7 * fTemp10 - fTemp6 * fRec16[1];
			float fTemp12 = fTemp7 * fTemp11 - fTemp6 * fRec17[1];
			fRec18[0] = fTemp7 * fTemp12 - fTemp6 * fRec18[1];
			fRec17[0] = fTemp6 * fTemp12 + fTemp7 * fRec18[1];
			fRec16[0] = fTemp6 * fTemp11 + fTemp7 * fRec17[1];
			fRec15[0] = fTemp6 * fTemp10 + fTemp7 * fRec16[1];
			fRec14[0] = fTemp6 * fTemp9 + fTemp7 * fRec15[1];
			fRec13[0] = fTemp6 * fTemp8 + fTemp7 * fRec14[1];
			float fTemp13 = fTemp3 * (fSlow28 * fTemp1 + fSlow29 * (fTemp1 + fVec0[1]) + fSlow30 * Bowed_dsp_faustpower2_f(fTemp1));
			float fTemp14 = std::sin(fTemp13);
			float fTemp15 = std::cos(fTemp13);
			float fTemp16 = fTemp1 * fTemp15 - fTemp14 * fRec19[1];
			float fTemp17 = fTemp15 * fTemp16 - fTemp14 * fRec20[1];
			float fTemp18 = fTemp15 * fTemp17 - fTemp14 * fRec21[1];
			float fTemp19 = fTemp15 * fTemp18 - fTemp14 * fRec22[1];
			float fTemp20 = fTemp15 * fTemp19 - fTemp14 * fRec23[1];
			fRec24[0] = fTemp15 * fTemp20 - fTemp14 * fRec24[1];
			fRec23[0] = fTemp14 * fTemp20 + fTemp15 * fRec24[1];
			fRec22[0] = fTemp14 * fTemp19 + fTemp15 * fRec23[1];
			fRec21[0] = fTemp14 * fTemp18 + fTemp15 * fRec22[1];
			fRec20[0] = fTemp14 * fTemp17 + fTemp15 * fRec21[1];
			fRec19[0] = fTemp14 * fTemp16 + fTemp15 * fRec20[1];
			fRec6[0] = fConst2 * (fSlow15 * (fTemp1 * fTemp6 + fRec13[1] * fTemp7) + fSlow27 * (fRec7[0] * (fTemp1 * fTemp14 + fRec19[1] * fTemp15) + (1.0f - fRec7[0]) * fTemp1)) + fConst4 * fRec6[1];
			iRec25[0] = iSlow5 & (iRec25[1] | (fRec27[1] >= 1.0f));
			iRec26[0] = iSlow5 * (iRec26[1] + 1);
			float fTemp21 = float(iRec26[1]);
			int iTemp22 = iSlow13 & (fRec27[1] > 0.0f);
			fRec27[0] = (fSlow33 * float(((((iRec25[1] == 0) & iSlow5) & (fRec27[1] < 1.0f)) & (fTemp21 > fSlow35)) * (1 - (fTemp21 < fSlow36))) + fRec27[1] * (1.0f - fSlow38 * float(iTemp22))) * float((iTemp22 == 0) | (fRec27[1] >= 1e-06f));
			fRec28[0] = fSlow39 + (fRec28[1] - std::floor(fSlow39 + fRec28[1]));
			float fTemp23 = fSlow31 * fRec27[0] * ftbl0Bowed_dspSIG0[std::max<int>(0, std::min<int>(int(65536.0f * fRec28[0]), 65535))];
			int iTemp24 = int(fSlow18 * (fTemp23 + 0.972764f - fSlow16));
			float fTemp25 = fSlow18 * (fSlow40 + fTemp23);
			float fTemp26 = float(int(fTemp25));
			float fTemp27 = fRec2[(IOTA0 - ((iTemp24 & 4095) + 1)) & 8191] * (fTemp26 + (1.0f - fTemp25)) + (fTemp25 - fTemp26) * fRec2[(IOTA0 - (((iTemp24 + 1) & 4095) + 1)) & 8191];
			float fTemp28 = fSlow3 * fRec5[0] + fRec6[0] + fTemp27;
			float fTemp29 = std::pow(std::fabs(fSlow41 * fTemp28) + 0.75f, -4.0f);
			float fTemp30 = fTemp28 * (float(fTemp29 > 1.0f) + fTemp29 * float(fTemp29 <= 1.0f));
			fRec2[IOTA0 & 8191] = fTemp30 - fRec6[0];
			float fRec3 = fTemp30 - fTemp27;
			fRec1[IOTA0 & 4095] = fRec3;
			fRec0[0] = 0.2f * fRec1[IOTA0 & 4095] + fConst5 * fRec0[1] - 0.7225f * fRec0[2];
			float fTemp31 = fRec0[0] - fRec0[2];
			output0[i0] = FAUSTFLOAT(fSlow2 * fTemp31);
			fVec1[IOTA0 & 4095] = fSlow42 * fTemp31;
			output1[i0] = FAUSTFLOAT(fSlow1 * fVec1[(IOTA0 - iSlow43) & 4095]);
			iRec4[1] = iRec4[0];
			fRec5[1] = fRec5[0];
			IOTA0 = IOTA0 + 1;
			fVec0[1] = fVec0[0];
			fRec7[1] = fRec7[0];
			iRec8[1] = iRec8[0];
			fRec9[1] = fRec9[0];
			fRec12[1] = fRec12[0];
			fRec11[1] = fRec11[0];
			fRec18[1] = fRec18[0];
			fRec17[1] = fRec17[0];
			fRec16[1] = fRec16[0];
			fRec15[1] = fRec15[0];
			fRec14[1] = fRec14[0];
			fRec13[1] = fRec13[0];
			fRec24[1] = fRec24[0];
			fRec23[1] = fRec23[0];
			fRec22[1] = fRec22[0];
			fRec21[1] = fRec21[0];
			fRec20[1] = fRec20[0];
			fRec19[1] = fRec19[0];
			fRec6[1] = fRec6[0];
			iRec25[1] = iRec25[0];
			iRec26[1] = iRec26[0];
			fRec27[1] = fRec27[0];
			fRec28[1] = fRec28[0];
			fRec0[2] = fRec0[1];
			fRec0[1] = fRec0[0];
		}
	}

};


#include "Faust_plugins_template2.cpp"


#endif
