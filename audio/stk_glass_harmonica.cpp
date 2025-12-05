/* ------------------------------------------------------------
author: "Romain Michon"
copyright: "Romain Michon (rmichon@ccrma.stanford.edu)"
name: "Glass Harmonica"
version: "1.0"
Code generated with Faust 2.81.2 (https://faust.grame.fr)
Compilation options: -a ../../Faust_plugins_template.cpp -lang cpp -nvi -ct 1 -cn Glass_Harmonica_dsp -es 1 -mcd 16 -mdd 1024 -mdy 33 -single -ftz 0
------------------------------------------------------------ */

#ifndef  __Glass_Harmonica_dsp_H__
#define  __Glass_Harmonica_dsp_H__


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
#define FAUSTCLASS Glass_Harmonica_dsp
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

class Glass_Harmonica_dspSIG0 {
	
  private:
	
	int iRec23[2];
	
  public:
	
	int getNumInputsGlass_Harmonica_dspSIG0() {
		return 0;
	}
	int getNumOutputsGlass_Harmonica_dspSIG0() {
		return 1;
	}
	
	void instanceInitGlass_Harmonica_dspSIG0(int sample_rate) {
		for (int l28 = 0; l28 < 2; l28 = l28 + 1) {
			iRec23[l28] = 0;
		}
	}
	
	void fillGlass_Harmonica_dspSIG0(int count, float* table) {
		for (int i1 = 0; i1 < count; i1 = i1 + 1) {
			iRec23[0] = iRec23[1] + 1;
			table[i1] = std::sin(9.58738e-05f * float(iRec23[0] + -1));
			iRec23[1] = iRec23[0];
		}
	}

};

static Glass_Harmonica_dspSIG0* newGlass_Harmonica_dspSIG0() { return (Glass_Harmonica_dspSIG0*)new Glass_Harmonica_dspSIG0(); }
static void deleteGlass_Harmonica_dspSIG0(Glass_Harmonica_dspSIG0* dsp) { delete dsp; }

static float Glass_Harmonica_dsp_faustpower2_f(float value) {
	return value * value;
}
static float *ftbl0Glass_Harmonica_dspSIG0; __attribute__((constructor)) static void initialize_ftbl0Glass_Harmonica_dspSIG0() { ftbl0Glass_Harmonica_dspSIG0 = (float*)calloc(65536, sizeof(float));};

class Glass_Harmonica_dsp final : public dsp {
	
 private:
	
	FAUSTFLOAT fHslider0;
	FAUSTFLOAT fButton0;
	int iRec0[2];
	int fSampleRate;
	float fConst0;
	FAUSTFLOAT fHslider1;
	FAUSTFLOAT fHslider2;
	float fRec1[2];
	FAUSTFLOAT fEntry0;
	float fConst1;
	float fConst2;
	float fConst3;
	FAUSTFLOAT fEntry1;
	FAUSTFLOAT fEntry2;
	int iRec10[2];
	float fConst4;
	float fConst5;
	float fConst6;
	float fRec11[2];
	FAUSTFLOAT fHslider3;
	FAUSTFLOAT fHslider4;
	FAUSTFLOAT fHslider5;
	int IOTA0;
	float fVec0[4096];
	FAUSTFLOAT fEntry3;
	float fConst7;
	float fConst8;
	float fRec9[3];
	float fRec8[2];
	float fRec2[2];
	float fVec1[4096];
	float fConst9;
	float fConst10;
	float fRec13[3];
	float fRec12[2];
	float fRec3[2];
	float fVec2[4096];
	float fConst11;
	float fConst12;
	float fRec15[3];
	float fRec14[2];
	float fRec4[2];
	float fVec3[2048];
	float fConst13;
	float fConst14;
	float fRec17[3];
	float fRec16[2];
	float fRec5[2];
	float fVec4[1024];
	float fConst15;
	float fConst16;
	float fRec19[3];
	float fRec18[2];
	float fRec6[2];
	float fConst17;
	float fRec21[3];
	float fRec7[2];
	float fVec5[2];
	FAUSTFLOAT fHslider6;
	float fRec22[2];
	float fConst18;
	FAUSTFLOAT fHslider7;
	float fRec25[2];
	float fRec24[2];
	float fRec31[2];
	float fRec30[2];
	float fRec29[2];
	float fRec28[2];
	float fRec27[2];
	float fRec26[2];
	float fRec37[2];
	float fRec36[2];
	float fRec35[2];
	float fRec34[2];
	float fRec33[2];
	float fRec32[2];
	float fVec6[4096];
	float fConst19;
	FAUSTFLOAT fHslider8;
	
 public:
	Glass_Harmonica_dsp() {
	}
	
	void metadata(Meta* m) { 
		m->declare("author", "Romain Michon");
		m->declare("compile_options", "-a ../../Faust_plugins_template.cpp -lang cpp -nvi -ct 1 -cn Glass_Harmonica_dsp -es 1 -mcd 16 -mdd 1024 -mdy 33 -single -ftz 0");
		m->declare("copyright", "Romain Michon (rmichon@ccrma.stanford.edu)");
		m->declare("description", "Nonlinear Banded Waveguide Modeled Glass Harmonica");
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
		m->declare("filename", "glassHarmonica.dsp");
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
		m->declare("name", "Glass Harmonica");
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
		Glass_Harmonica_dspSIG0* sig0 = newGlass_Harmonica_dspSIG0();
		sig0->instanceInitGlass_Harmonica_dspSIG0(sample_rate);
		sig0->fillGlass_Harmonica_dspSIG0(65536, ftbl0Glass_Harmonica_dspSIG0);
		deleteGlass_Harmonica_dspSIG0(sig0);
	}
	
	void instanceConstants(int sample_rate) {
		fSampleRate = sample_rate;
		fConst0 = std::min<float>(1.92e+05f, std::max<float>(1.0f, float(fSampleRate)));
		fConst1 = 100.53097f / fConst0;
		fConst2 = Glass_Harmonica_dsp_faustpower2_f(1.0f - fConst1);
		fConst3 = 0.5f * (1.0f - fConst2);
		fConst4 = 5e+01f / fConst0;
		fConst5 = 1.0f - std::pow(9e+01f, 2e+02f / fConst0);
		fConst6 = 1.0f - 1.0f / std::pow(9e+04f, 1e+02f / fConst0);
		fConst7 = 2.0f * (fConst1 - 1.0f);
		fConst8 = 6.2831855f / fConst0;
		fConst9 = 0.43103448f * fConst0;
		fConst10 = 14.57699f / fConst0;
		fConst11 = 0.23529412f * fConst0;
		fConst12 = 26.703537f / fConst0;
		fConst13 = 0.15082957f * fConst0;
		fConst14 = 41.65752f / fConst0;
		fConst15 = 0.10660981f * fConst0;
		fConst16 = 58.93628f / fConst0;
		fConst17 = 56.548668f / fConst0;
		fConst18 = 1.0f / fConst0;
		fConst19 = 0.5f * fConst0;
	}
	
	void instanceResetUserInterface() {
		fHslider0 = FAUSTFLOAT(0.6f);
		fButton0 = FAUSTFLOAT(0.0f);
		fHslider1 = FAUSTFLOAT(0.02f);
		fHslider2 = FAUSTFLOAT(0.1f);
		fEntry0 = FAUSTFLOAT(0.0f);
		fEntry1 = FAUSTFLOAT(0.8f);
		fEntry2 = FAUSTFLOAT(0.0f);
		fHslider3 = FAUSTFLOAT(1.0f);
		fHslider4 = FAUSTFLOAT(0.0f);
		fHslider5 = FAUSTFLOAT(0.2f);
		fEntry3 = FAUSTFLOAT(4.4e+02f);
		fHslider6 = FAUSTFLOAT(0.0f);
		fHslider7 = FAUSTFLOAT(2.2e+02f);
		fHslider8 = FAUSTFLOAT(0.5f);
	}
	
	void instanceClear() {
		for (int l0 = 0; l0 < 2; l0 = l0 + 1) {
			iRec0[l0] = 0;
		}
		for (int l1 = 0; l1 < 2; l1 = l1 + 1) {
			fRec1[l1] = 0.0f;
		}
		for (int l2 = 0; l2 < 2; l2 = l2 + 1) {
			iRec10[l2] = 0;
		}
		for (int l3 = 0; l3 < 2; l3 = l3 + 1) {
			fRec11[l3] = 0.0f;
		}
		IOTA0 = 0;
		for (int l4 = 0; l4 < 4096; l4 = l4 + 1) {
			fVec0[l4] = 0.0f;
		}
		for (int l5 = 0; l5 < 3; l5 = l5 + 1) {
			fRec9[l5] = 0.0f;
		}
		for (int l6 = 0; l6 < 2; l6 = l6 + 1) {
			fRec8[l6] = 0.0f;
		}
		for (int l7 = 0; l7 < 2; l7 = l7 + 1) {
			fRec2[l7] = 0.0f;
		}
		for (int l8 = 0; l8 < 4096; l8 = l8 + 1) {
			fVec1[l8] = 0.0f;
		}
		for (int l9 = 0; l9 < 3; l9 = l9 + 1) {
			fRec13[l9] = 0.0f;
		}
		for (int l10 = 0; l10 < 2; l10 = l10 + 1) {
			fRec12[l10] = 0.0f;
		}
		for (int l11 = 0; l11 < 2; l11 = l11 + 1) {
			fRec3[l11] = 0.0f;
		}
		for (int l12 = 0; l12 < 4096; l12 = l12 + 1) {
			fVec2[l12] = 0.0f;
		}
		for (int l13 = 0; l13 < 3; l13 = l13 + 1) {
			fRec15[l13] = 0.0f;
		}
		for (int l14 = 0; l14 < 2; l14 = l14 + 1) {
			fRec14[l14] = 0.0f;
		}
		for (int l15 = 0; l15 < 2; l15 = l15 + 1) {
			fRec4[l15] = 0.0f;
		}
		for (int l16 = 0; l16 < 2048; l16 = l16 + 1) {
			fVec3[l16] = 0.0f;
		}
		for (int l17 = 0; l17 < 3; l17 = l17 + 1) {
			fRec17[l17] = 0.0f;
		}
		for (int l18 = 0; l18 < 2; l18 = l18 + 1) {
			fRec16[l18] = 0.0f;
		}
		for (int l19 = 0; l19 < 2; l19 = l19 + 1) {
			fRec5[l19] = 0.0f;
		}
		for (int l20 = 0; l20 < 1024; l20 = l20 + 1) {
			fVec4[l20] = 0.0f;
		}
		for (int l21 = 0; l21 < 3; l21 = l21 + 1) {
			fRec19[l21] = 0.0f;
		}
		for (int l22 = 0; l22 < 2; l22 = l22 + 1) {
			fRec18[l22] = 0.0f;
		}
		for (int l23 = 0; l23 < 2; l23 = l23 + 1) {
			fRec6[l23] = 0.0f;
		}
		for (int l24 = 0; l24 < 3; l24 = l24 + 1) {
			fRec21[l24] = 0.0f;
		}
		for (int l25 = 0; l25 < 2; l25 = l25 + 1) {
			fRec7[l25] = 0.0f;
		}
		for (int l26 = 0; l26 < 2; l26 = l26 + 1) {
			fVec5[l26] = 0.0f;
		}
		for (int l27 = 0; l27 < 2; l27 = l27 + 1) {
			fRec22[l27] = 0.0f;
		}
		for (int l29 = 0; l29 < 2; l29 = l29 + 1) {
			fRec25[l29] = 0.0f;
		}
		for (int l30 = 0; l30 < 2; l30 = l30 + 1) {
			fRec24[l30] = 0.0f;
		}
		for (int l31 = 0; l31 < 2; l31 = l31 + 1) {
			fRec31[l31] = 0.0f;
		}
		for (int l32 = 0; l32 < 2; l32 = l32 + 1) {
			fRec30[l32] = 0.0f;
		}
		for (int l33 = 0; l33 < 2; l33 = l33 + 1) {
			fRec29[l33] = 0.0f;
		}
		for (int l34 = 0; l34 < 2; l34 = l34 + 1) {
			fRec28[l34] = 0.0f;
		}
		for (int l35 = 0; l35 < 2; l35 = l35 + 1) {
			fRec27[l35] = 0.0f;
		}
		for (int l36 = 0; l36 < 2; l36 = l36 + 1) {
			fRec26[l36] = 0.0f;
		}
		for (int l37 = 0; l37 < 2; l37 = l37 + 1) {
			fRec37[l37] = 0.0f;
		}
		for (int l38 = 0; l38 < 2; l38 = l38 + 1) {
			fRec36[l38] = 0.0f;
		}
		for (int l39 = 0; l39 < 2; l39 = l39 + 1) {
			fRec35[l39] = 0.0f;
		}
		for (int l40 = 0; l40 < 2; l40 = l40 + 1) {
			fRec34[l40] = 0.0f;
		}
		for (int l41 = 0; l41 < 2; l41 = l41 + 1) {
			fRec33[l41] = 0.0f;
		}
		for (int l42 = 0; l42 < 2; l42 = l42 + 1) {
			fRec32[l42] = 0.0f;
		}
		for (int l43 = 0; l43 < 4096; l43 = l43 + 1) {
			fVec6[l43] = 0.0f;
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
	
	Glass_Harmonica_dsp* clone() {
		return new Glass_Harmonica_dsp();
	}
	
	int getSampleRate() {
		return fSampleRate;
	}
	
	void buildUserInterface(UI* ui_interface) {
		ui_interface->openVerticalBox("Glass Harmonica");
		ui_interface->openHorizontalBox("Basic_Parameters");
		ui_interface->declare(&fEntry3, "1", "");
		ui_interface->declare(&fEntry3, "tooltip", "Tone frequency");
		ui_interface->declare(&fEntry3, "unit", "Hz");
		ui_interface->addNumEntry("freq", &fEntry3, FAUSTFLOAT(4.4e+02f), FAUSTFLOAT(2e+01f), FAUSTFLOAT(2e+04f), FAUSTFLOAT(1.0f));
		ui_interface->declare(&fEntry1, "1", "");
		ui_interface->declare(&fEntry1, "tooltip", "Gain (value between 0 and 1)");
		ui_interface->addNumEntry("gain", &fEntry1, FAUSTFLOAT(0.8f), FAUSTFLOAT(0.0f), FAUSTFLOAT(1.0f), FAUSTFLOAT(0.01f));
		ui_interface->declare(&fButton0, "1", "");
		ui_interface->declare(&fButton0, "tooltip", "noteOn = 1, noteOff = 0");
		ui_interface->addButton("gate", &fButton0);
		ui_interface->closeBox();
		ui_interface->openHorizontalBox("Envelopes_and_Vibrato");
		ui_interface->openVerticalBox("Global_Envelope_Parameters");
		ui_interface->declare(&fHslider1, "4", "");
		ui_interface->declare(&fHslider1, "tooltip", "Global envelope attack duration");
		ui_interface->declare(&fHslider1, "unit", "s");
		ui_interface->addHorizontalSlider("Glob_Env_Attack", &fHslider1, FAUSTFLOAT(0.02f), FAUSTFLOAT(0.0f), FAUSTFLOAT(2.0f), FAUSTFLOAT(0.01f));
		ui_interface->declare(&fHslider2, "4", "");
		ui_interface->declare(&fHslider2, "tooltip", "Global envelope release duration");
		ui_interface->declare(&fHslider2, "unit", "s");
		ui_interface->addHorizontalSlider("Glob_Env_Release", &fHslider2, FAUSTFLOAT(0.1f), FAUSTFLOAT(0.0f), FAUSTFLOAT(2.0f), FAUSTFLOAT(0.01f));
		ui_interface->closeBox();
		ui_interface->closeBox();
		ui_interface->openHorizontalBox("Physical_and_Nonlinearity");
		ui_interface->openVerticalBox("Nonlinear_Filter_Parameters");
		ui_interface->declare(&fHslider7, "3", "");
		ui_interface->declare(&fHslider7, "tooltip", "Frequency of the sine wave for the modulation of theta (works if Modulation Type=3)");
		ui_interface->declare(&fHslider7, "unit", "Hz");
		ui_interface->addHorizontalSlider("Modulation_Frequency", &fHslider7, FAUSTFLOAT(2.2e+02f), FAUSTFLOAT(2e+01f), FAUSTFLOAT(1e+03f), FAUSTFLOAT(0.1f));
		ui_interface->declare(&fEntry0, "3", "");
		ui_interface->declare(&fEntry0, "tooltip", "0=theta is modulated by the incoming signal; 1=theta is modulated by the averaged incoming signal; 2=theta is modulated by the squared incoming signal; 3=theta is modulated by a sine wave of frequency freqMod; 4=theta is modulated by a sine wave of frequency freq;");
		ui_interface->addNumEntry("Modulation_Type", &fEntry0, FAUSTFLOAT(0.0f), FAUSTFLOAT(0.0f), FAUSTFLOAT(4.0f), FAUSTFLOAT(1.0f));
		ui_interface->declare(&fHslider6, "3", "");
		ui_interface->declare(&fHslider6, "tooltip", "Nonlinearity factor (value between 0 and 1)");
		ui_interface->addHorizontalSlider("Nonlinearity", &fHslider6, FAUSTFLOAT(0.0f), FAUSTFLOAT(0.0f), FAUSTFLOAT(1.0f), FAUSTFLOAT(0.01f));
		ui_interface->closeBox();
		ui_interface->openVerticalBox("Physical_Parameters");
		ui_interface->declare(&fHslider3, "2", "");
		ui_interface->declare(&fHslider3, "tooltip", "A value between 0 and 1");
		ui_interface->addHorizontalSlider("Base_Gain", &fHslider3, FAUSTFLOAT(1.0f), FAUSTFLOAT(0.0f), FAUSTFLOAT(1.0f), FAUSTFLOAT(0.01f));
		ui_interface->declare(&fHslider5, "2", "");
		ui_interface->declare(&fHslider5, "tooltip", "Bow pressure on the instrument (Value between 0 and 1)");
		ui_interface->addHorizontalSlider("Bow_Pressure", &fHslider5, FAUSTFLOAT(0.2f), FAUSTFLOAT(0.0f), FAUSTFLOAT(1.0f), FAUSTFLOAT(0.01f));
		ui_interface->declare(&fEntry2, "2", "");
		ui_interface->declare(&fEntry2, "tooltip", "0=Bow; 1=Strike");
		ui_interface->addNumEntry("Excitation_Selector", &fEntry2, FAUSTFLOAT(0.0f), FAUSTFLOAT(0.0f), FAUSTFLOAT(1.0f), FAUSTFLOAT(1.0f));
		ui_interface->declare(&fHslider4, "2", "");
		ui_interface->declare(&fHslider4, "tooltip", "A value between 0 and 1");
		ui_interface->addHorizontalSlider("Integration_Constant", &fHslider4, FAUSTFLOAT(0.0f), FAUSTFLOAT(0.0f), FAUSTFLOAT(1.0f), FAUSTFLOAT(0.01f));
		ui_interface->closeBox();
		ui_interface->closeBox();
		ui_interface->openVerticalBox("Spat");
		ui_interface->addHorizontalSlider("pan angle", &fHslider0, FAUSTFLOAT(0.6f), FAUSTFLOAT(0.0f), FAUSTFLOAT(1.0f), FAUSTFLOAT(0.01f));
		ui_interface->addHorizontalSlider("spatial width", &fHslider8, FAUSTFLOAT(0.5f), FAUSTFLOAT(0.0f), FAUSTFLOAT(1.0f), FAUSTFLOAT(0.01f));
		ui_interface->closeBox();
		ui_interface->closeBox();
	}
	
	void compute(int count, FAUSTFLOAT** RESTRICT inputs, FAUSTFLOAT** RESTRICT outputs) {
		FAUSTFLOAT* output0 = outputs[0];
		FAUSTFLOAT* output1 = outputs[1];
		float fSlow0 = float(fHslider0);
		float fSlow1 = 0.5f * (1.0f - fSlow0);
		float fSlow2 = float(fButton0);
		int iSlow3 = fSlow2 > 0.0f;
		float fSlow4 = float(fHslider1);
		float fSlow5 = 1.0f / (fConst0 * fSlow4 + float(fSlow4 == 0.0f));
		float fSlow6 = float(fHslider2);
		float fSlow7 = 1.0f - 1.0f / std::pow(1e+05f, 1.0f / (fConst0 * fSlow6 + float(fSlow6 == 0.0f)));
		int iSlow8 = fSlow2 <= 0.0f;
		float fSlow9 = float(fEntry0);
		float fSlow10 = float(fSlow9 >= 3.0f);
		float fSlow11 = float(fEntry1);
		float fSlow12 = float(fEntry2);
		float fSlow13 = 0.2f * fSlow2 * fSlow11 * fSlow12;
		float fSlow14 = 0.16666667f * (fSlow12 + -1.0f);
		float fSlow15 = 0.1f * fSlow11 + 0.03f;
		float fSlow16 = 0.1f * float(fHslider3) + 0.9f;
		float fSlow17 = float(fHslider4);
		float fSlow18 = 1e+01f - 9.0f * float(fHslider5);
		float fSlow19 = float(fEntry3);
		int iSlow20 = int(fConst0 / fSlow19) & 4095;
		float fSlow21 = fConst7 * std::cos(fConst8 * fSlow19);
		int iSlow22 = int(fConst9 / fSlow19) & 4095;
		float fSlow23 = fConst7 * std::cos(fConst10 * fSlow19);
		int iSlow24 = int(fConst11 / fSlow19) & 4095;
		float fSlow25 = fConst7 * std::cos(fConst12 * fSlow19);
		int iSlow26 = int(fConst13 / fSlow19) & 4095;
		float fSlow27 = fConst7 * std::cos(fConst14 * fSlow19);
		int iSlow28 = int(fConst15 / fSlow19) & 4095;
		float fSlow29 = fConst7 * std::cos(fConst16 * fSlow19);
		float fSlow30 = fConst7 * std::cos(fConst17 * fSlow19);
		float fSlow31 = 0.001f * float(fHslider6);
		float fSlow32 = fSlow19 * float(fSlow9 == 4.0f);
		float fSlow33 = float(fSlow9 != 4.0f);
		float fSlow34 = 0.001f * float(fHslider7);
		float fSlow35 = float(fSlow9 < 3.0f);
		float fSlow36 = 12.566371f * float(fSlow9 == 0.0f);
		float fSlow37 = 6.2831855f * float(fSlow9 == 1.0f);
		float fSlow38 = 50.265484f * float(fSlow9 == 2.0f);
		float fSlow39 = 0.5f * fSlow0;
		int iSlow40 = int(fConst19 * (float(fHslider8) / fSlow19)) & 4095;
		for (int i0 = 0; i0 < count; i0 = i0 + 1) {
			iRec0[0] = iSlow3 & (iRec0[1] | (fRec1[1] >= 1.0f));
			int iTemp0 = iSlow8 & (fRec1[1] > 0.0f);
			fRec1[0] = (fSlow5 * float(((iRec0[1] == 0) & iSlow3) & (fRec1[1] < 1.0f)) + fRec1[1] * (1.0f - fSlow7 * float(iTemp0))) * float((iTemp0 == 0) | (fRec1[1] >= 1e-06f));
			iRec10[0] = iSlow3 & (iRec10[1] | (fRec11[1] >= 1.0f));
			int iTemp1 = iSlow8 & (fRec11[1] > 0.0f);
			fRec11[0] = (fConst4 * float(((iRec10[1] == 0) & iSlow3) & (fRec11[1] < 1.0f)) + fRec11[1] * (1.0f - fConst5 * float(iRec10[1] & (fRec11[1] > 9e+01f)) - fConst6 * float(iTemp1))) * float((iTemp1 == 0) | (fRec11[1] >= 1e-06f));
			float fTemp2 = fSlow15 * fRec11[0] - fSlow16 * (fRec2[1] + fRec4[1] + fRec6[1] + fRec3[1] + fRec5[1] + fRec7[1]) - fSlow17;
			float fTemp3 = std::pow(std::fabs(fSlow18 * fTemp2) + 0.75f, -4.0f);
			float fTemp4 = fSlow14 * fTemp2 * (float(fTemp3 > 1.0f) + fTemp3 * float(fTemp3 <= 1.0f));
			fVec0[IOTA0 & 4095] = fSlow13 + (fRec8[1] - fTemp4);
			fRec9[0] = 0.999f * fVec0[(IOTA0 - iSlow20) & 4095] - (fSlow21 * fRec9[1] + fConst2 * fRec9[2]);
			fRec8[0] = fConst3 * (fRec9[0] - fRec9[2]);
			fRec2[0] = fRec8[0];
			fVec1[IOTA0 & 4095] = fSlow13 + (fRec12[1] - fTemp4);
			fRec13[0] = 0.998001f * fVec1[(IOTA0 - iSlow22) & 4095] - (fSlow23 * fRec13[1] + fConst2 * fRec13[2]);
			fRec12[0] = fConst3 * (fRec13[0] - fRec13[2]);
			fRec3[0] = fRec12[0];
			fVec2[IOTA0 & 4095] = fSlow13 + (fRec14[1] - fTemp4);
			fRec15[0] = 0.997003f * fVec2[(IOTA0 - iSlow24) & 4095] - (fSlow25 * fRec15[1] + fConst2 * fRec15[2]);
			fRec14[0] = fConst3 * (fRec15[0] - fRec15[2]);
			fRec4[0] = fRec14[0];
			fVec3[IOTA0 & 2047] = fSlow13 + (fRec16[1] - fTemp4);
			fRec17[0] = 0.996006f * fVec3[(IOTA0 - iSlow26) & 2047] - (fSlow27 * fRec17[1] + fConst2 * fRec17[2]);
			fRec16[0] = fConst3 * (fRec17[0] - fRec17[2]);
			fRec5[0] = fRec16[0];
			fVec4[IOTA0 & 1023] = fSlow13 + (fRec18[1] - fTemp4);
			fRec19[0] = 0.99501f * fVec4[(IOTA0 - iSlow28) & 1023] - (fSlow29 * fRec19[1] + fConst2 * fRec19[2]);
			fRec18[0] = fConst3 * (fRec19[0] - fRec19[2]);
			fRec6[0] = fRec18[0];
			fRec21[0] = -(fSlow30 * fRec21[1] + fConst2 * fRec21[2]);
			float fRec20 = fConst3 * (fRec21[0] - fRec21[2]);
			fRec7[0] = fRec20;
			float fTemp5 = fRec7[0] + fRec5[0] + fRec2[0] + fRec4[0] + fRec6[0] + fRec3[0];
			fVec5[0] = fTemp5;
			fRec22[0] = fSlow31 + 0.999f * fRec22[1];
			fRec25[0] = fSlow34 + 0.999f * fRec25[1];
			float fTemp6 = fRec24[1] + fConst18 * (fSlow32 + fSlow33 * fRec25[0]);
			fRec24[0] = fTemp6 - std::floor(fTemp6);
			float fTemp7 = 3.1415927f * fRec22[0] * ftbl0Glass_Harmonica_dspSIG0[std::max<int>(0, std::min<int>(int(65536.0f * fRec24[0]), 65535))];
			float fTemp8 = std::sin(fTemp7);
			float fTemp9 = std::cos(fTemp7);
			float fTemp10 = 4.0f * fTemp5 * fTemp9 - fTemp8 * fRec26[1];
			float fTemp11 = fTemp9 * fTemp10 - fTemp8 * fRec27[1];
			float fTemp12 = fTemp9 * fTemp11 - fTemp8 * fRec28[1];
			float fTemp13 = fTemp9 * fTemp12 - fTemp8 * fRec29[1];
			float fTemp14 = fTemp9 * fTemp13 - fTemp8 * fRec30[1];
			fRec31[0] = fTemp9 * fTemp14 - fTemp8 * fRec31[1];
			fRec30[0] = fTemp8 * fTemp14 + fTemp9 * fRec31[1];
			fRec29[0] = fTemp8 * fTemp13 + fTemp9 * fRec30[1];
			fRec28[0] = fTemp8 * fTemp12 + fTemp9 * fRec29[1];
			fRec27[0] = fTemp8 * fTemp11 + fTemp9 * fRec28[1];
			fRec26[0] = fTemp8 * fTemp10 + fTemp9 * fRec27[1];
			float fTemp15 = fRec22[0] * (fSlow36 * fTemp5 + fSlow37 * (fTemp5 + fVec5[1]) + fSlow38 * Glass_Harmonica_dsp_faustpower2_f(fTemp5));
			float fTemp16 = std::sin(fTemp15);
			float fTemp17 = std::cos(fTemp15);
			float fTemp18 = 4.0f * fTemp5 * fTemp17 - fTemp16 * fRec32[1];
			float fTemp19 = fTemp17 * fTemp18 - fTemp16 * fRec33[1];
			float fTemp20 = fTemp17 * fTemp19 - fTemp16 * fRec34[1];
			float fTemp21 = fTemp17 * fTemp20 - fTemp16 * fRec35[1];
			float fTemp22 = fTemp17 * fTemp21 - fTemp16 * fRec36[1];
			fRec37[0] = fTemp17 * fTemp22 - fTemp16 * fRec37[1];
			fRec36[0] = fTemp16 * fTemp22 + fTemp17 * fRec37[1];
			fRec35[0] = fTemp16 * fTemp21 + fTemp17 * fRec36[1];
			fRec34[0] = fTemp16 * fTemp20 + fTemp17 * fRec35[1];
			fRec33[0] = fTemp16 * fTemp19 + fTemp17 * fRec34[1];
			fRec32[0] = fTemp16 * fTemp18 + fTemp17 * fRec33[1];
			float fTemp23 = fRec1[0] * (fSlow10 * (4.0f * fTemp5 * fTemp8 + fRec26[1] * fTemp9) + fSlow35 * (fRec22[0] * (4.0f * fTemp5 * fTemp16 + fRec32[1] * fTemp17) + 4.0f * (1.0f - fRec22[0]) * fTemp5));
			fVec6[IOTA0 & 4095] = fTemp23;
			output0[i0] = FAUSTFLOAT(fSlow1 * fTemp23);
			output1[i0] = FAUSTFLOAT(fSlow39 * fVec6[(IOTA0 - iSlow40) & 4095]);
			iRec0[1] = iRec0[0];
			fRec1[1] = fRec1[0];
			iRec10[1] = iRec10[0];
			fRec11[1] = fRec11[0];
			IOTA0 = IOTA0 + 1;
			fRec9[2] = fRec9[1];
			fRec9[1] = fRec9[0];
			fRec8[1] = fRec8[0];
			fRec2[1] = fRec2[0];
			fRec13[2] = fRec13[1];
			fRec13[1] = fRec13[0];
			fRec12[1] = fRec12[0];
			fRec3[1] = fRec3[0];
			fRec15[2] = fRec15[1];
			fRec15[1] = fRec15[0];
			fRec14[1] = fRec14[0];
			fRec4[1] = fRec4[0];
			fRec17[2] = fRec17[1];
			fRec17[1] = fRec17[0];
			fRec16[1] = fRec16[0];
			fRec5[1] = fRec5[0];
			fRec19[2] = fRec19[1];
			fRec19[1] = fRec19[0];
			fRec18[1] = fRec18[0];
			fRec6[1] = fRec6[0];
			fRec21[2] = fRec21[1];
			fRec21[1] = fRec21[0];
			fRec7[1] = fRec7[0];
			fVec5[1] = fVec5[0];
			fRec22[1] = fRec22[0];
			fRec25[1] = fRec25[0];
			fRec24[1] = fRec24[0];
			fRec31[1] = fRec31[0];
			fRec30[1] = fRec30[0];
			fRec29[1] = fRec29[0];
			fRec28[1] = fRec28[0];
			fRec27[1] = fRec27[0];
			fRec26[1] = fRec26[0];
			fRec37[1] = fRec37[0];
			fRec36[1] = fRec36[0];
			fRec35[1] = fRec35[0];
			fRec34[1] = fRec34[0];
			fRec33[1] = fRec33[0];
			fRec32[1] = fRec32[0];
		}
	}

};


#include "Faust_plugins_template2.cpp"


#endif
