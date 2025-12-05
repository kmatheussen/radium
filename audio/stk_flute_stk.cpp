/* ------------------------------------------------------------
author: "Romain Michon"
copyright: "Romain Michon (rmichon@ccrma.stanford.edu)"
name: "FluteSTK"
version: "1.0"
Code generated with Faust 2.81.2 (https://faust.grame.fr)
Compilation options: -a ../../Faust_plugins_template.cpp -lang cpp -nvi -ct 1 -cn Flute_Stk_dsp -es 1 -mcd 16 -mdd 1024 -mdy 33 -single -ftz 0
------------------------------------------------------------ */

#ifndef  __Flute_Stk_dsp_H__
#define  __Flute_Stk_dsp_H__


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
#define FAUSTCLASS Flute_Stk_dsp
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

class Flute_Stk_dspSIG0 {
	
  private:
	
	int iRec7[2];
	
  public:
	
	int getNumInputsFlute_Stk_dspSIG0() {
		return 0;
	}
	int getNumOutputsFlute_Stk_dspSIG0() {
		return 1;
	}
	
	void instanceInitFlute_Stk_dspSIG0(int sample_rate) {
		for (int l6 = 0; l6 < 2; l6 = l6 + 1) {
			iRec7[l6] = 0;
		}
	}
	
	void fillFlute_Stk_dspSIG0(int count, float* table) {
		for (int i1 = 0; i1 < count; i1 = i1 + 1) {
			iRec7[0] = iRec7[1] + 1;
			table[i1] = std::sin(9.58738e-05f * float(iRec7[0] + -1));
			iRec7[1] = iRec7[0];
		}
	}

};

static Flute_Stk_dspSIG0* newFlute_Stk_dspSIG0() { return (Flute_Stk_dspSIG0*)new Flute_Stk_dspSIG0(); }
static void deleteFlute_Stk_dspSIG0(Flute_Stk_dspSIG0* dsp) { delete dsp; }

static float *ftbl0Flute_Stk_dspSIG0; __attribute__((constructor)) static void initialize_ftbl0Flute_Stk_dspSIG0() { ftbl0Flute_Stk_dspSIG0 = (float*)calloc(65536, sizeof(float));};
static float Flute_Stk_dsp_faustpower2_f(float value) {
	return value * value;
}

class Flute_Stk_dsp final : public dsp {
	
 private:
	
	FAUSTFLOAT fHslider0;
	FAUSTFLOAT fHslider1;
	int fSampleRate;
	float fConst0;
	float fConst1;
	FAUSTFLOAT fEntry0;
	FAUSTFLOAT fHslider2;
	FAUSTFLOAT fButton0;
	int iRec1[2];
	FAUSTFLOAT fHslider3;
	FAUSTFLOAT fHslider4;
	FAUSTFLOAT fHslider5;
	float fRec2[2];
	FAUSTFLOAT fHslider6;
	int iRec3[2];
	FAUSTFLOAT fHslider7;
	int iRec4[2];
	int iRec5[2];
	FAUSTFLOAT fHslider8;
	FAUSTFLOAT fHslider9;
	FAUSTFLOAT fHslider10;
	float fRec6[2];
	float fConst2;
	FAUSTFLOAT fHslider11;
	float fRec8[2];
	float fConst3;
	float fConst4;
	float fConst5;
	FAUSTFLOAT fEntry1;
	int IOTA0;
	float fVec0[2];
	FAUSTFLOAT fHslider12;
	float fRec11[2];
	int iRec12[2];
	FAUSTFLOAT fHslider13;
	float fRec13[2];
	FAUSTFLOAT fHslider14;
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
	float fRec10[2];
	float fRec9[2];
	float fVec1[4096];
	float fRec0[8192];
	FAUSTFLOAT fEntry2;
	float fRec28[2];
	float fVec2[4096];
	FAUSTFLOAT fHslider15;
	
 public:
	Flute_Stk_dsp() {
	}
	
	void metadata(Meta* m) { 
		m->declare("author", "Romain Michon");
		m->declare("compile_options", "-a ../../Faust_plugins_template.cpp -lang cpp -nvi -ct 1 -cn Flute_Stk_dsp -es 1 -mcd 16 -mdd 1024 -mdy 33 -single -ftz 0");
		m->declare("copyright", "Romain Michon (rmichon@ccrma.stanford.edu)");
		m->declare("description", "Nonlinear WaveGuide Flute from STK");
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
		m->declare("filename", "flutestk.dsp");
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
		m->declare("name", "FluteSTK");
		m->declare("reference", "https://ccrma.stanford.edu/~jos/pasp/Flutes_Recorders_Pipe_Organs.html");
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
		Flute_Stk_dspSIG0* sig0 = newFlute_Stk_dspSIG0();
		sig0->instanceInitFlute_Stk_dspSIG0(sample_rate);
		sig0->fillFlute_Stk_dspSIG0(65536, ftbl0Flute_Stk_dspSIG0);
		deleteFlute_Stk_dspSIG0(sig0);
	}
	
	void instanceConstants(int sample_rate) {
		fSampleRate = sample_rate;
		fConst0 = std::min<float>(1.92e+05f, std::max<float>(1.0f, float(fSampleRate)));
		fConst1 = 0.5f * fConst0;
		fConst2 = 1.0f / fConst0;
		fConst3 = 2205.0f / fConst0;
		fConst4 = 0.7f - fConst3;
		fConst5 = fConst3 + 0.3f;
	}
	
	void instanceResetUserInterface() {
		fHslider0 = FAUSTFLOAT(0.6f);
		fHslider1 = FAUSTFLOAT(0.5f);
		fEntry0 = FAUSTFLOAT(4.4e+02f);
		fHslider2 = FAUSTFLOAT(1.0f);
		fButton0 = FAUSTFLOAT(0.0f);
		fHslider3 = FAUSTFLOAT(0.03f);
		fHslider4 = FAUSTFLOAT(0.01f);
		fHslider5 = FAUSTFLOAT(0.3f);
		fHslider6 = FAUSTFLOAT(0.03f);
		fHslider7 = FAUSTFLOAT(0.05f);
		fHslider8 = FAUSTFLOAT(0.5f);
		fHslider9 = FAUSTFLOAT(0.05f);
		fHslider10 = FAUSTFLOAT(0.1f);
		fHslider11 = FAUSTFLOAT(6.0f);
		fEntry1 = FAUSTFLOAT(0.0f);
		fHslider12 = FAUSTFLOAT(0.0f);
		fHslider13 = FAUSTFLOAT(0.1f);
		fHslider14 = FAUSTFLOAT(2.2e+02f);
		fEntry2 = FAUSTFLOAT(1.0f);
		fHslider15 = FAUSTFLOAT(0.5f);
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
			fRec11[l9] = 0.0f;
		}
		for (int l10 = 0; l10 < 2; l10 = l10 + 1) {
			iRec12[l10] = 0;
		}
		for (int l11 = 0; l11 < 2; l11 = l11 + 1) {
			fRec13[l11] = 0.0f;
		}
		for (int l12 = 0; l12 < 2; l12 = l12 + 1) {
			fRec15[l12] = 0.0f;
		}
		for (int l13 = 0; l13 < 2; l13 = l13 + 1) {
			fRec14[l13] = 0.0f;
		}
		for (int l14 = 0; l14 < 2; l14 = l14 + 1) {
			fRec21[l14] = 0.0f;
		}
		for (int l15 = 0; l15 < 2; l15 = l15 + 1) {
			fRec20[l15] = 0.0f;
		}
		for (int l16 = 0; l16 < 2; l16 = l16 + 1) {
			fRec19[l16] = 0.0f;
		}
		for (int l17 = 0; l17 < 2; l17 = l17 + 1) {
			fRec18[l17] = 0.0f;
		}
		for (int l18 = 0; l18 < 2; l18 = l18 + 1) {
			fRec17[l18] = 0.0f;
		}
		for (int l19 = 0; l19 < 2; l19 = l19 + 1) {
			fRec16[l19] = 0.0f;
		}
		for (int l20 = 0; l20 < 2; l20 = l20 + 1) {
			fRec27[l20] = 0.0f;
		}
		for (int l21 = 0; l21 < 2; l21 = l21 + 1) {
			fRec26[l21] = 0.0f;
		}
		for (int l22 = 0; l22 < 2; l22 = l22 + 1) {
			fRec25[l22] = 0.0f;
		}
		for (int l23 = 0; l23 < 2; l23 = l23 + 1) {
			fRec24[l23] = 0.0f;
		}
		for (int l24 = 0; l24 < 2; l24 = l24 + 1) {
			fRec23[l24] = 0.0f;
		}
		for (int l25 = 0; l25 < 2; l25 = l25 + 1) {
			fRec22[l25] = 0.0f;
		}
		for (int l26 = 0; l26 < 2; l26 = l26 + 1) {
			fRec10[l26] = 0.0f;
		}
		for (int l27 = 0; l27 < 2; l27 = l27 + 1) {
			fRec9[l27] = 0.0f;
		}
		for (int l28 = 0; l28 < 4096; l28 = l28 + 1) {
			fVec1[l28] = 0.0f;
		}
		for (int l29 = 0; l29 < 8192; l29 = l29 + 1) {
			fRec0[l29] = 0.0f;
		}
		for (int l30 = 0; l30 < 2; l30 = l30 + 1) {
			fRec28[l30] = 0.0f;
		}
		for (int l31 = 0; l31 < 4096; l31 = l31 + 1) {
			fVec2[l31] = 0.0f;
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
	
	Flute_Stk_dsp* clone() {
		return new Flute_Stk_dsp();
	}
	
	int getSampleRate() {
		return fSampleRate;
	}
	
	void buildUserInterface(UI* ui_interface) {
		ui_interface->openVerticalBox("FluteSTK");
		ui_interface->openHorizontalBox("Basic_Parameters");
		ui_interface->declare(&fEntry0, "1", "");
		ui_interface->declare(&fEntry0, "tooltip", "Tone frequency");
		ui_interface->declare(&fEntry0, "unit", "Hz");
		ui_interface->addNumEntry("freq", &fEntry0, FAUSTFLOAT(4.4e+02f), FAUSTFLOAT(2e+01f), FAUSTFLOAT(2e+04f), FAUSTFLOAT(1.0f));
		ui_interface->declare(&fEntry2, "1", "");
		ui_interface->declare(&fEntry2, "tooltip", "Gain (value between 0 and 1)");
		ui_interface->addNumEntry("gain", &fEntry2, FAUSTFLOAT(1.0f), FAUSTFLOAT(0.0f), FAUSTFLOAT(1.0f), FAUSTFLOAT(0.01f));
		ui_interface->declare(&fButton0, "1", "");
		ui_interface->declare(&fButton0, "tooltip", "noteOn = 1, noteOff = 0");
		ui_interface->addButton("gate", &fButton0);
		ui_interface->closeBox();
		ui_interface->openHorizontalBox("Envelopes_and_Vibrato");
		ui_interface->openVerticalBox("Envelope_Parameters");
		ui_interface->declare(&fHslider3, "5", "");
		ui_interface->declare(&fHslider3, "tooltip", "Envelope attack duration");
		ui_interface->declare(&fHslider3, "unit", "s");
		ui_interface->addHorizontalSlider("Envelope_Attack", &fHslider3, FAUSTFLOAT(0.03f), FAUSTFLOAT(0.0f), FAUSTFLOAT(2.0f), FAUSTFLOAT(0.01f));
		ui_interface->declare(&fHslider4, "5", "");
		ui_interface->declare(&fHslider4, "tooltip", "Envelope decay duration");
		ui_interface->declare(&fHslider4, "unit", "s");
		ui_interface->addHorizontalSlider("Envelope_Decay", &fHslider4, FAUSTFLOAT(0.01f), FAUSTFLOAT(0.0f), FAUSTFLOAT(2.0f), FAUSTFLOAT(0.01f));
		ui_interface->declare(&fHslider5, "5", "");
		ui_interface->declare(&fHslider5, "tooltip", "Envelope release duration");
		ui_interface->declare(&fHslider5, "unit", "s");
		ui_interface->addHorizontalSlider("Envelope_Release", &fHslider5, FAUSTFLOAT(0.3f), FAUSTFLOAT(0.0f), FAUSTFLOAT(2.0f), FAUSTFLOAT(0.01f));
		ui_interface->closeBox();
		ui_interface->openVerticalBox("Vibrato_Parameters");
		ui_interface->declare(&fHslider8, "4", "");
		ui_interface->declare(&fHslider8, "tooltip", "Vibrato attack duration");
		ui_interface->declare(&fHslider8, "unit", "s");
		ui_interface->addHorizontalSlider("Vibrato_Attack", &fHslider8, FAUSTFLOAT(0.5f), FAUSTFLOAT(0.0f), FAUSTFLOAT(2.0f), FAUSTFLOAT(0.01f));
		ui_interface->declare(&fHslider9, "4", "");
		ui_interface->declare(&fHslider9, "tooltip", "Vibrato silence duration before attack");
		ui_interface->declare(&fHslider9, "unit", "s");
		ui_interface->addHorizontalSlider("Vibrato_Begin", &fHslider9, FAUSTFLOAT(0.05f), FAUSTFLOAT(0.0f), FAUSTFLOAT(2.0f), FAUSTFLOAT(0.01f));
		ui_interface->declare(&fHslider11, "4", "");
		ui_interface->declare(&fHslider11, "unit", "Hz");
		ui_interface->addHorizontalSlider("Vibrato_Freq", &fHslider11, FAUSTFLOAT(6.0f), FAUSTFLOAT(1.0f), FAUSTFLOAT(15.0f), FAUSTFLOAT(0.1f));
		ui_interface->declare(&fHslider7, "4", "");
		ui_interface->declare(&fHslider7, "tooltip", "A value between 0 and 1");
		ui_interface->addHorizontalSlider("Vibrato_Gain", &fHslider7, FAUSTFLOAT(0.05f), FAUSTFLOAT(0.0f), FAUSTFLOAT(1.0f), FAUSTFLOAT(0.01f));
		ui_interface->declare(&fHslider10, "4", "");
		ui_interface->declare(&fHslider10, "tooltip", "Vibrato release duration");
		ui_interface->declare(&fHslider10, "unit", "s");
		ui_interface->addHorizontalSlider("Vibrato_Release", &fHslider10, FAUSTFLOAT(0.1f), FAUSTFLOAT(0.0f), FAUSTFLOAT(2.0f), FAUSTFLOAT(0.01f));
		ui_interface->closeBox();
		ui_interface->closeBox();
		ui_interface->openHorizontalBox("Physical_and_Nonlinearity");
		ui_interface->openVerticalBox("Nonlinear_Filter_Parameters");
		ui_interface->declare(&fHslider14, "3", "");
		ui_interface->declare(&fHslider14, "tooltip", "Frequency of the sine wave for the modulation of theta (works if Modulation Type=3)");
		ui_interface->declare(&fHslider14, "unit", "Hz");
		ui_interface->addHorizontalSlider("Modulation_Frequency", &fHslider14, FAUSTFLOAT(2.2e+02f), FAUSTFLOAT(2e+01f), FAUSTFLOAT(1e+03f), FAUSTFLOAT(0.1f));
		ui_interface->declare(&fEntry1, "3", "");
		ui_interface->declare(&fEntry1, "tooltip", "0=theta is modulated by the incoming signal; 1=theta is modulated by the averaged incoming signal; 2=theta is modulated by the squared incoming signal; 3=theta is modulated by a sine wave of frequency freqMod; 4=theta is modulated by a sine wave of frequency freq;");
		ui_interface->addNumEntry("Modulation_Type", &fEntry1, FAUSTFLOAT(0.0f), FAUSTFLOAT(0.0f), FAUSTFLOAT(4.0f), FAUSTFLOAT(1.0f));
		ui_interface->declare(&fHslider12, "3", "");
		ui_interface->declare(&fHslider12, "tooltip", "Nonlinearity factor (value between 0 and 1)");
		ui_interface->addHorizontalSlider("Nonlinearity", &fHslider12, FAUSTFLOAT(0.0f), FAUSTFLOAT(0.0f), FAUSTFLOAT(1.0f), FAUSTFLOAT(0.01f));
		ui_interface->declare(&fHslider13, "3", "");
		ui_interface->declare(&fHslider13, "Attack duration of the nonlinearity", "");
		ui_interface->declare(&fHslider13, "unit", "s");
		ui_interface->addHorizontalSlider("Nonlinearity_Attack", &fHslider13, FAUSTFLOAT(0.1f), FAUSTFLOAT(0.0f), FAUSTFLOAT(2.0f), FAUSTFLOAT(0.01f));
		ui_interface->closeBox();
		ui_interface->openVerticalBox("Physical_Parameters");
		ui_interface->declare(&fHslider1, "2", "");
		ui_interface->declare(&fHslider1, "tooltip", "A value between 0 and 1");
		ui_interface->addHorizontalSlider("Embouchure_Ajust", &fHslider1, FAUSTFLOAT(0.5f), FAUSTFLOAT(0.0f), FAUSTFLOAT(1.0f), FAUSTFLOAT(0.01f));
		ui_interface->declare(&fHslider6, "2", "");
		ui_interface->declare(&fHslider6, "tooltip", "A value between 0 and 1");
		ui_interface->addHorizontalSlider("Noise_Gain", &fHslider6, FAUSTFLOAT(0.03f), FAUSTFLOAT(0.0f), FAUSTFLOAT(1.0f), FAUSTFLOAT(0.01f));
		ui_interface->declare(&fHslider2, "2", "");
		ui_interface->declare(&fHslider2, "tooltip", "Breath pressure (value between 0 and 1)");
		ui_interface->addHorizontalSlider("Pressure", &fHslider2, FAUSTFLOAT(1.0f), FAUSTFLOAT(0.0f), FAUSTFLOAT(1.0f), FAUSTFLOAT(0.01f));
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
		float fSlow1 = 0.3f * (1.0f - fSlow0);
		float fSlow2 = float(fEntry0);
		float fSlow3 = fConst1 / fSlow2;
		float fSlow4 = fSlow3 + -2.0f;
		float fSlow5 = (1.5f - float(fHslider1)) * fSlow4;
		int iSlow6 = int(fSlow5);
		float fSlow7 = float(iSlow6);
		float fSlow8 = fSlow7 + (1.0f - fSlow5);
		float fSlow9 = float(fHslider2);
		float fSlow10 = float(fButton0);
		int iSlow11 = fSlow10 > 0.0f;
		float fSlow12 = fSlow9 * float(fHslider3);
		float fSlow13 = 1.0f / (fConst0 * fSlow12 + float(fSlow12 == 0.0f));
		float fSlow14 = float(fHslider4);
		float fSlow15 = 1.0f - std::pow(8e+01f, 1.0f / (fConst0 * fSlow14 + float(fSlow14 == 0.0f)));
		float fSlow16 = float(fHslider5);
		float fSlow17 = 1.0f / (fConst0 * fSlow16 + float(fSlow16 == 0.0f));
		float fSlow18 = 1.0f - 1.0f / std::pow(8e+04f, fSlow17);
		int iSlow19 = fSlow10 <= 0.0f;
		float fSlow20 = 4.656613e-10f * float(fHslider6);
		float fSlow21 = float(fHslider7);
		float fSlow22 = float(fHslider8);
		float fSlow23 = 1.0f / (fConst0 * fSlow22 + float(fSlow22 == 0.0f));
		float fSlow24 = float(fHslider9);
		float fSlow25 = fConst0 * fSlow24;
		float fSlow26 = fSlow25 + float(fSlow24 == 0.0f);
		float fSlow27 = float(fHslider10);
		float fSlow28 = 1.0f - 1.0f / std::pow(1e+05f, 1.0f / (fConst0 * fSlow27 + float(fSlow27 == 0.0f)));
		float fSlow29 = fConst2 * float(fHslider11);
		float fSlow30 = float(fEntry1);
		float fSlow31 = float(fSlow30 >= 3.0f);
		int iSlow32 = int(fSlow4);
		float fSlow33 = float(iSlow32);
		float fSlow34 = fSlow33 + (3.0f - fSlow3);
		int iSlow35 = (iSlow32 & 4095) + 1;
		float fSlow36 = fSlow3 + (-2.0f - fSlow33);
		int iSlow37 = ((iSlow32 + 1) & 4095) + 1;
		float fSlow38 = 0.001f * float(fHslider12);
		float fSlow39 = float(fHslider13);
		float fSlow40 = 1.0f / (fConst0 * fSlow39 + float(fSlow39 == 0.0f));
		float fSlow41 = 1.0f - 1.0f / std::pow(1e+05f, fSlow17);
		float fSlow42 = fSlow2 * float(fSlow30 == 4.0f);
		float fSlow43 = float(fSlow30 != 4.0f);
		float fSlow44 = 0.001f * float(fHslider14);
		float fSlow45 = float(fSlow30 < 3.0f);
		float fSlow46 = 3.1415927f * float(fSlow30 == 0.0f);
		float fSlow47 = 1.5707964f * float(fSlow30 == 1.0f);
		float fSlow48 = 3.1415927f * float(fSlow30 == 2.0f);
		int iSlow49 = iSlow6 & 4095;
		float fSlow50 = fSlow5 - fSlow7;
		int iSlow51 = (iSlow6 + 1) & 4095;
		float fSlow52 = 0.001f * float(fEntry2);
		float fSlow53 = 0.3f * fSlow0;
		int iSlow54 = int(fConst1 * (float(fHslider15) / fSlow2)) & 4095;
		for (int i0 = 0; i0 < count; i0 = i0 + 1) {
			iRec1[0] = iSlow11 & (iRec1[1] | (fRec2[1] >= 1.0f));
			int iTemp0 = iSlow19 & (fRec2[1] > 0.0f);
			fRec2[0] = (fSlow13 * float(((iRec1[1] == 0) & iSlow11) & (fRec2[1] < 1.0f)) + fRec2[1] * (1.0f - fSlow15 * float(iRec1[1] & (fRec2[1] > 8e+01f)) - fSlow18 * float(iTemp0))) * float((iTemp0 == 0) | (fRec2[1] >= 1e-06f));
			iRec3[0] = 1103515245 * iRec3[1] + 12345;
			iRec4[0] = iSlow11 & (iRec4[1] | (fRec6[1] >= 1.0f));
			iRec5[0] = iSlow11 * (iRec5[1] + 1);
			float fTemp1 = float(iRec5[1]);
			int iTemp2 = iSlow19 & (fRec6[1] > 0.0f);
			fRec6[0] = (fSlow23 * float(((((iRec4[1] == 0) & iSlow11) & (fRec6[1] < 1.0f)) & (fTemp1 > fSlow25)) * (1 - (fTemp1 < fSlow26))) + fRec6[1] * (1.0f - fSlow28 * float(iTemp2))) * float((iTemp2 == 0) | (fRec6[1] >= 1e-06f));
			fRec8[0] = fSlow29 + (fRec8[1] - std::floor(fSlow29 + fRec8[1]));
			float fTemp3 = fSlow34 * fRec0[(IOTA0 - iSlow35) & 8191] + fSlow36 * fRec0[(IOTA0 - iSlow37) & 8191];
			fVec0[0] = fTemp3;
			fRec11[0] = fSlow38 + 0.999f * fRec11[1];
			iRec12[0] = iSlow11 & (iRec12[1] | (fRec13[1] >= 1.0f));
			int iTemp4 = iSlow19 & (fRec13[1] > 0.0f);
			fRec13[0] = (fSlow40 * float(((iRec12[1] == 0) & iSlow11) & (fRec13[1] < 1.0f)) + fRec13[1] * (1.0f - fSlow41 * float(iTemp4))) * float((iTemp4 == 0) | (fRec13[1] >= 1e-06f));
			float fTemp5 = fRec11[0] * fRec13[0];
			fRec15[0] = fSlow44 + 0.999f * fRec15[1];
			float fTemp6 = fRec14[1] + fConst2 * (fSlow42 + fSlow43 * fRec15[0]);
			fRec14[0] = fTemp6 - std::floor(fTemp6);
			float fTemp7 = 3.1415927f * fTemp5 * ftbl0Flute_Stk_dspSIG0[std::max<int>(0, std::min<int>(int(65536.0f * fRec14[0]), 65535))];
			float fTemp8 = std::sin(fTemp7);
			float fTemp9 = std::cos(fTemp7);
			float fTemp10 = fTemp3 * fTemp9 - fTemp8 * fRec16[1];
			float fTemp11 = fTemp9 * fTemp10 - fTemp8 * fRec17[1];
			float fTemp12 = fTemp9 * fTemp11 - fTemp8 * fRec18[1];
			float fTemp13 = fTemp9 * fTemp12 - fTemp8 * fRec19[1];
			float fTemp14 = fTemp9 * fTemp13 - fTemp8 * fRec20[1];
			fRec21[0] = fTemp9 * fTemp14 - fTemp8 * fRec21[1];
			fRec20[0] = fTemp8 * fTemp14 + fTemp9 * fRec21[1];
			fRec19[0] = fTemp8 * fTemp13 + fTemp9 * fRec20[1];
			fRec18[0] = fTemp8 * fTemp12 + fTemp9 * fRec19[1];
			fRec17[0] = fTemp8 * fTemp11 + fTemp9 * fRec18[1];
			fRec16[0] = fTemp8 * fTemp10 + fTemp9 * fRec17[1];
			float fTemp15 = fTemp5 * (fSlow46 * fTemp3 + fSlow47 * (fTemp3 + fVec0[1]) + fSlow48 * Flute_Stk_dsp_faustpower2_f(fTemp3));
			float fTemp16 = std::sin(fTemp15);
			float fTemp17 = std::cos(fTemp15);
			float fTemp18 = fTemp3 * fTemp17 - fTemp16 * fRec22[1];
			float fTemp19 = fTemp17 * fTemp18 - fTemp16 * fRec23[1];
			float fTemp20 = fTemp17 * fTemp19 - fTemp16 * fRec24[1];
			float fTemp21 = fTemp17 * fTemp20 - fTemp16 * fRec25[1];
			float fTemp22 = fTemp17 * fTemp21 - fTemp16 * fRec26[1];
			fRec27[0] = fTemp17 * fTemp22 - fTemp16 * fRec27[1];
			fRec26[0] = fTemp16 * fTemp22 + fTemp17 * fRec27[1];
			fRec25[0] = fTemp16 * fTemp21 + fTemp17 * fRec26[1];
			fRec24[0] = fTemp16 * fTemp20 + fTemp17 * fRec25[1];
			fRec23[0] = fTemp16 * fTemp19 + fTemp17 * fRec24[1];
			fRec22[0] = fTemp16 * fTemp18 + fTemp17 * fRec23[1];
			fRec10[0] = fConst4 * fRec10[1] - fConst5 * (fSlow31 * (fTemp3 * fTemp8 + fRec16[1] * fTemp9) + fSlow45 * (fRec11[0] * (fTemp3 * fTemp16 + fRec22[1] * fTemp17) + (1.0f - fRec11[0]) * fTemp3));
			fRec9[0] = fRec10[0] + 0.995f * fRec9[1] - fRec10[1];
			float fTemp23 = 0.5f * fRec9[0];
			float fTemp24 = fSlow9 * fRec2[0] * (fSlow20 * float(iRec3[0]) + fSlow21 * fRec6[0] * ftbl0Flute_Stk_dspSIG0[std::max<int>(0, std::min<int>(int(65536.0f * fRec8[0]), 65535))] + 1.0f) + (1e-15f - fTemp23);
			fVec1[IOTA0 & 4095] = fTemp24;
			float fTemp25 = fSlow8 * fVec1[(IOTA0 - iSlow49) & 4095] + fSlow50 * fVec1[(IOTA0 - iSlow51) & 4095];
			float fTemp26 = fTemp25 * (Flute_Stk_dsp_faustpower2_f(fTemp25) + -1.0f);
			float fTemp27 = float(fTemp26 > 1.0f) + fTemp26 * float(fTemp26 <= 1.0f);
			fRec0[IOTA0 & 8191] = fTemp27 * float(fTemp27 >= -1.0f) + fTemp23 + float(-(fTemp27 < -1.0f));
			fRec28[0] = fSlow52 + 0.999f * fRec28[1];
			float fTemp28 = fRec0[IOTA0 & 8191] * fRec28[0];
			fVec2[IOTA0 & 4095] = fTemp28;
			output0[i0] = FAUSTFLOAT(fSlow1 * fTemp28);
			output1[i0] = FAUSTFLOAT(fSlow53 * fVec2[(IOTA0 - iSlow54) & 4095]);
			iRec1[1] = iRec1[0];
			fRec2[1] = fRec2[0];
			iRec3[1] = iRec3[0];
			iRec4[1] = iRec4[0];
			iRec5[1] = iRec5[0];
			fRec6[1] = fRec6[0];
			fRec8[1] = fRec8[0];
			IOTA0 = IOTA0 + 1;
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
			fRec10[1] = fRec10[0];
			fRec9[1] = fRec9[0];
			fRec28[1] = fRec28[0];
		}
	}

};


#include "Faust_plugins_template2.cpp"


#endif
