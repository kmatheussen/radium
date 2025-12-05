/* ------------------------------------------------------------
author: "Romain Michon (rmichon@ccrma.stanford.edu)"
copyright: "Romain Michon"
name: "Modal Bar"
version: "1.0"
Code generated with Faust 2.81.2 (https://faust.grame.fr)
Compilation options: -a ../../Faust_plugins_template.cpp -lang cpp -nvi -ct 1 -cn Modal_Bar_dsp -es 1 -mcd 16 -mdd 1024 -mdy 33 -single -ftz 0
------------------------------------------------------------ */

#ifndef  __Modal_Bar_dsp_H__
#define  __Modal_Bar_dsp_H__


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

/* link with : "" */
#include <algorithm>
#include <cmath>
#include <cstdint>
#include <instrument.h>
#include <math.h>
#include <modalBar.h>

#ifndef FAUSTCLASS 
#define FAUSTCLASS Modal_Bar_dsp
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

class Modal_Bar_dspSIG0 {
	
  private:
	
	int iRec5[2];
	
  public:
	
	int getNumInputsModal_Bar_dspSIG0() {
		return 0;
	}
	int getNumOutputsModal_Bar_dspSIG0() {
		return 1;
	}
	
	void instanceInitModal_Bar_dspSIG0(int sample_rate) {
		for (int l3 = 0; l3 < 2; l3 = l3 + 1) {
			iRec5[l3] = 0;
		}
	}
	
	void fillModal_Bar_dspSIG0(int count, float* table) {
		for (int i1 = 0; i1 < count; i1 = i1 + 1) {
			iRec5[0] = iRec5[1] + 1;
			table[i1] = readMarmstk1((iRec5[0] + -1) % 246);
			iRec5[1] = iRec5[0];
		}
	}

};

static Modal_Bar_dspSIG0* newModal_Bar_dspSIG0() { return (Modal_Bar_dspSIG0*)new Modal_Bar_dspSIG0(); }
static void deleteModal_Bar_dspSIG0(Modal_Bar_dspSIG0* dsp) { delete dsp; }

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
	
	void instanceInitModal_Bar_dspSIG1(int sample_rate) {
		for (int l18 = 0; l18 < 2; l18 = l18 + 1) {
			iRec18[l18] = 0;
		}
	}
	
	void fillModal_Bar_dspSIG1(int count, float* table) {
		for (int i2 = 0; i2 < count; i2 = i2 + 1) {
			iRec18[0] = iRec18[1] + 1;
			table[i2] = std::sin(9.58738e-05f * float(iRec18[0] + -1));
			iRec18[1] = iRec18[0];
		}
	}

};

static Modal_Bar_dspSIG1* newModal_Bar_dspSIG1() { return (Modal_Bar_dspSIG1*)new Modal_Bar_dspSIG1(); }
static void deleteModal_Bar_dspSIG1(Modal_Bar_dspSIG1* dsp) { delete dsp; }

static float *ftbl0Modal_Bar_dspSIG0; __attribute__((constructor)) static void initialize_ftbl0Modal_Bar_dspSIG0() { ftbl0Modal_Bar_dspSIG0 = (float*)calloc(246, sizeof(float));};
static float *ftbl1Modal_Bar_dspSIG1; __attribute__((constructor)) static void initialize_ftbl1Modal_Bar_dspSIG1() { ftbl1Modal_Bar_dspSIG1 = (float*)calloc(65536, sizeof(float));};
static float Modal_Bar_dsp_faustpower2_f(float value) {
	return value * value;
}

class Modal_Bar_dsp final : public dsp {
	
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
	FAUSTFLOAT fEntry1;
	float fRec4[2];
	FAUSTFLOAT fHslider3;
	float fRec6[2];
	float fRec3[2];
	FAUSTFLOAT fEntry2;
	float fRec7[2];
	FAUSTFLOAT fEntry3;
	float fRec8[2];
	float fConst1;
	FAUSTFLOAT fEntry4;
	float fRec2[3];
	float fRec10[2];
	float fRec11[2];
	float fRec9[3];
	float fRec13[2];
	float fRec14[2];
	float fRec12[3];
	float fRec16[2];
	float fRec17[2];
	float fRec15[3];
	FAUSTFLOAT fHslider4;
	float fConst2;
	FAUSTFLOAT fHslider5;
	float fRec19[2];
	float fVec0[2];
	FAUSTFLOAT fHslider6;
	float fRec20[2];
	FAUSTFLOAT fHslider7;
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
	int IOTA0;
	float fVec1[4096];
	float fConst3;
	FAUSTFLOAT fHslider8;
	
 public:
	Modal_Bar_dsp() {
	}
	
	void metadata(Meta* m) { 
		m->declare("author", "Romain Michon (rmichon@ccrma.stanford.edu)");
		m->declare("compile_options", "-a ../../Faust_plugins_template.cpp -lang cpp -nvi -ct 1 -cn Modal_Bar_dsp -es 1 -mcd 16 -mdd 1024 -mdy 33 -single -ftz 0");
		m->declare("copyright", "Romain Michon");
		m->declare("description", "Nonlinear Modal percussive instruments");
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
		m->declare("filename", "modalBar.dsp");
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
		m->declare("name", "Modal Bar");
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
		Modal_Bar_dspSIG0* sig0 = newModal_Bar_dspSIG0();
		sig0->instanceInitModal_Bar_dspSIG0(sample_rate);
		sig0->fillModal_Bar_dspSIG0(246, ftbl0Modal_Bar_dspSIG0);
		Modal_Bar_dspSIG1* sig1 = newModal_Bar_dspSIG1();
		sig1->instanceInitModal_Bar_dspSIG1(sample_rate);
		sig1->fillModal_Bar_dspSIG1(65536, ftbl1Modal_Bar_dspSIG1);
		deleteModal_Bar_dspSIG0(sig0);
		deleteModal_Bar_dspSIG1(sig1);
	}
	
	void instanceConstants(int sample_rate) {
		fSampleRate = sample_rate;
		fConst0 = std::min<float>(1.92e+05f, std::max<float>(1.0f, float(fSampleRate)));
		fConst1 = 6.2831855f / fConst0;
		fConst2 = 1.0f / fConst0;
		fConst3 = 0.5f * fConst0;
	}
	
	void instanceResetUserInterface() {
		fHslider0 = FAUSTFLOAT(0.6f);
		fButton0 = FAUSTFLOAT(0.0f);
		fHslider1 = FAUSTFLOAT(0.0f);
		fHslider2 = FAUSTFLOAT(0.05f);
		fEntry0 = FAUSTFLOAT(0.0f);
		fEntry1 = FAUSTFLOAT(0.8f);
		fHslider3 = FAUSTFLOAT(0.25f);
		fEntry2 = FAUSTFLOAT(1.0f);
		fEntry3 = FAUSTFLOAT(1.0f);
		fEntry4 = FAUSTFLOAT(4.4e+02f);
		fHslider4 = FAUSTFLOAT(0.1f);
		fHslider5 = FAUSTFLOAT(6.0f);
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
			fRec4[l2] = 0.0f;
		}
		for (int l4 = 0; l4 < 2; l4 = l4 + 1) {
			fRec6[l4] = 0.0f;
		}
		for (int l5 = 0; l5 < 2; l5 = l5 + 1) {
			fRec3[l5] = 0.0f;
		}
		for (int l6 = 0; l6 < 2; l6 = l6 + 1) {
			fRec7[l6] = 0.0f;
		}
		for (int l7 = 0; l7 < 2; l7 = l7 + 1) {
			fRec8[l7] = 0.0f;
		}
		for (int l8 = 0; l8 < 3; l8 = l8 + 1) {
			fRec2[l8] = 0.0f;
		}
		for (int l9 = 0; l9 < 2; l9 = l9 + 1) {
			fRec10[l9] = 0.0f;
		}
		for (int l10 = 0; l10 < 2; l10 = l10 + 1) {
			fRec11[l10] = 0.0f;
		}
		for (int l11 = 0; l11 < 3; l11 = l11 + 1) {
			fRec9[l11] = 0.0f;
		}
		for (int l12 = 0; l12 < 2; l12 = l12 + 1) {
			fRec13[l12] = 0.0f;
		}
		for (int l13 = 0; l13 < 2; l13 = l13 + 1) {
			fRec14[l13] = 0.0f;
		}
		for (int l14 = 0; l14 < 3; l14 = l14 + 1) {
			fRec12[l14] = 0.0f;
		}
		for (int l15 = 0; l15 < 2; l15 = l15 + 1) {
			fRec16[l15] = 0.0f;
		}
		for (int l16 = 0; l16 < 2; l16 = l16 + 1) {
			fRec17[l16] = 0.0f;
		}
		for (int l17 = 0; l17 < 3; l17 = l17 + 1) {
			fRec15[l17] = 0.0f;
		}
		for (int l19 = 0; l19 < 2; l19 = l19 + 1) {
			fRec19[l19] = 0.0f;
		}
		for (int l20 = 0; l20 < 2; l20 = l20 + 1) {
			fVec0[l20] = 0.0f;
		}
		for (int l21 = 0; l21 < 2; l21 = l21 + 1) {
			fRec20[l21] = 0.0f;
		}
		for (int l22 = 0; l22 < 2; l22 = l22 + 1) {
			fRec22[l22] = 0.0f;
		}
		for (int l23 = 0; l23 < 2; l23 = l23 + 1) {
			fRec21[l23] = 0.0f;
		}
		for (int l24 = 0; l24 < 2; l24 = l24 + 1) {
			fRec28[l24] = 0.0f;
		}
		for (int l25 = 0; l25 < 2; l25 = l25 + 1) {
			fRec27[l25] = 0.0f;
		}
		for (int l26 = 0; l26 < 2; l26 = l26 + 1) {
			fRec26[l26] = 0.0f;
		}
		for (int l27 = 0; l27 < 2; l27 = l27 + 1) {
			fRec25[l27] = 0.0f;
		}
		for (int l28 = 0; l28 < 2; l28 = l28 + 1) {
			fRec24[l28] = 0.0f;
		}
		for (int l29 = 0; l29 < 2; l29 = l29 + 1) {
			fRec23[l29] = 0.0f;
		}
		for (int l30 = 0; l30 < 2; l30 = l30 + 1) {
			fRec34[l30] = 0.0f;
		}
		for (int l31 = 0; l31 < 2; l31 = l31 + 1) {
			fRec33[l31] = 0.0f;
		}
		for (int l32 = 0; l32 < 2; l32 = l32 + 1) {
			fRec32[l32] = 0.0f;
		}
		for (int l33 = 0; l33 < 2; l33 = l33 + 1) {
			fRec31[l33] = 0.0f;
		}
		for (int l34 = 0; l34 < 2; l34 = l34 + 1) {
			fRec30[l34] = 0.0f;
		}
		for (int l35 = 0; l35 < 2; l35 = l35 + 1) {
			fRec29[l35] = 0.0f;
		}
		IOTA0 = 0;
		for (int l36 = 0; l36 < 4096; l36 = l36 + 1) {
			fVec1[l36] = 0.0f;
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
	
	Modal_Bar_dsp* clone() {
		return new Modal_Bar_dsp();
	}
	
	int getSampleRate() {
		return fSampleRate;
	}
	
	void buildUserInterface(UI* ui_interface) {
		ui_interface->openVerticalBox("Modal Bar");
		ui_interface->openHorizontalBox("Basic_Parameters");
		ui_interface->declare(&fEntry4, "1", "");
		ui_interface->declare(&fEntry4, "tooltip", "Tone frequency");
		ui_interface->declare(&fEntry4, "unit", "Hz");
		ui_interface->addNumEntry("freq", &fEntry4, FAUSTFLOAT(4.4e+02f), FAUSTFLOAT(2e+01f), FAUSTFLOAT(2e+04f), FAUSTFLOAT(1.0f));
		ui_interface->declare(&fEntry1, "1", "");
		ui_interface->declare(&fEntry1, "tooltip", "Gain (value between 0 and 1)");
		ui_interface->addNumEntry("gain", &fEntry1, FAUSTFLOAT(0.8f), FAUSTFLOAT(0.0f), FAUSTFLOAT(1.0f), FAUSTFLOAT(0.01f));
		ui_interface->declare(&fButton0, "1", "");
		ui_interface->declare(&fButton0, "tooltip", "noteOn = 1, noteOff = 0");
		ui_interface->addButton("gate", &fButton0);
		ui_interface->closeBox();
		ui_interface->openHorizontalBox("Envelopes_and_Vibrato");
		ui_interface->openVerticalBox("Global_Envelope_Parameters");
		ui_interface->declare(&fHslider1, "5", "");
		ui_interface->declare(&fHslider1, "tooltip", "Global envelope attack duration");
		ui_interface->declare(&fHslider1, "unit", "s");
		ui_interface->addHorizontalSlider("Glob_Env_Attack", &fHslider1, FAUSTFLOAT(0.0f), FAUSTFLOAT(0.0f), FAUSTFLOAT(2.0f), FAUSTFLOAT(0.01f));
		ui_interface->declare(&fHslider2, "5", "");
		ui_interface->declare(&fHslider2, "tooltip", "Global envelope release duration");
		ui_interface->declare(&fHslider2, "unit", "s");
		ui_interface->addHorizontalSlider("Glob_Env_Release", &fHslider2, FAUSTFLOAT(0.05f), FAUSTFLOAT(0.0f), FAUSTFLOAT(2.0f), FAUSTFLOAT(0.01f));
		ui_interface->closeBox();
		ui_interface->openVerticalBox("Vibrato_Parameters");
		ui_interface->declare(&fHslider5, "4", "");
		ui_interface->declare(&fHslider5, "unit", "Hz");
		ui_interface->addHorizontalSlider("Vibrato_Freq", &fHslider5, FAUSTFLOAT(6.0f), FAUSTFLOAT(1.0f), FAUSTFLOAT(15.0f), FAUSTFLOAT(0.1f));
		ui_interface->declare(&fHslider4, "4", "");
		ui_interface->declare(&fHslider4, "tooltip", "A value between 0 and 1");
		ui_interface->addHorizontalSlider("Vibrato_Gain", &fHslider4, FAUSTFLOAT(0.1f), FAUSTFLOAT(0.0f), FAUSTFLOAT(1.0f), FAUSTFLOAT(0.01f));
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
		ui_interface->declare(&fEntry2, "2", "");
		ui_interface->declare(&fEntry2, "tooltip", "0->Marimba, 1->Vibraphone, 2->Agogo, 3->Wood1, 4->Reso, 5->Wood2, 6->Beats, 7->2Fix; 8->Clump");
		ui_interface->addNumEntry("Preset", &fEntry2, FAUSTFLOAT(1.0f), FAUSTFLOAT(0.0f), FAUSTFLOAT(8.0f), FAUSTFLOAT(1.0f));
		ui_interface->declare(&fEntry3, "2", "");
		ui_interface->declare(&fEntry3, "tooltip", "A value between 0 and 1");
		ui_interface->addNumEntry("Resonance", &fEntry3, FAUSTFLOAT(1.0f), FAUSTFLOAT(0.0f), FAUSTFLOAT(1.0f), FAUSTFLOAT(1.0f));
		ui_interface->declare(&fHslider3, "2", "");
		ui_interface->declare(&fHslider3, "tooltip", "A value between 0 and 1");
		ui_interface->addHorizontalSlider("Stick_Hardness", &fHslider3, FAUSTFLOAT(0.25f), FAUSTFLOAT(0.0f), FAUSTFLOAT(1.0f), FAUSTFLOAT(0.01f));
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
		float fSlow1 = 0.3f * (1.0f - fSlow0);
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
		float fSlow12 = 0.1f * fSlow2;
		float fSlow13 = std::pow(4.0f, float(fHslider3));
		float fSlow14 = 61.5f * fSlow13;
		float fSlow15 = 246.0f * fSlow2;
		float fSlow16 = 0.25f * fSlow13;
		float fSlow17 = float(fEntry2);
		int iSlow18 = int(fSlow17);
		float fSlow19 = 0.001f * loadPreset(iSlow18, 2, 3);
		float fSlow20 = 1.0f - 0.03f * fSlow11 * float((fSlow2 < 1.0f) & (float(fEntry3) != 1.0f));
		float fSlow21 = float(fEntry4);
		float fSlow22 = 0.001f * loadPreset(iSlow18, 2, 2);
		float fSlow23 = 0.001f * loadPreset(iSlow18, 2, 0);
		float fSlow24 = 0.001f * loadPreset(iSlow18, 2, 1);
		float fSlow25 = loadPreset(iSlow18, 3, 2);
		float fSlow26 = float(fHslider4) * float(fSlow17 == 1.0f);
		float fSlow27 = fConst2 * float(fHslider5);
		float fSlow28 = 0.001f * float(fHslider6);
		float fSlow29 = fSlow21 * float(fSlow9 == 4.0f);
		float fSlow30 = float(fSlow9 != 4.0f);
		float fSlow31 = 0.001f * float(fHslider7);
		float fSlow32 = float(fSlow9 < 3.0f);
		float fSlow33 = 3.1415927f * float(fSlow9 == 0.0f);
		float fSlow34 = 1.5707964f * float(fSlow9 == 1.0f);
		float fSlow35 = 3.1415927f * float(fSlow9 == 2.0f);
		float fSlow36 = 0.3f * fSlow0;
		int iSlow37 = int(fConst3 * (float(fHslider8) / fSlow21)) & 4095;
		for (int i0 = 0; i0 < count; i0 = i0 + 1) {
			iRec0[0] = iSlow3 & (iRec0[1] | (fRec1[1] >= 1.0f));
			int iTemp0 = iSlow8 & (fRec1[1] > 0.0f);
			fRec1[0] = (fSlow5 * float(((iRec0[1] == 0) & iSlow3) & (fRec1[1] < 1.0f)) + fRec1[1] * (1.0f - fSlow7 * float(iTemp0))) * float((iTemp0 == 0) | (fRec1[1] >= 1e-06f));
			fRec4[0] = fSlow2 * fRec4[1] + 1.0f;
			fRec6[0] = fSlow16 + (fRec6[1] - std::floor(fSlow16 + fRec6[1]));
			fRec3[0] = fSlow12 * float((fRec4[0] + -1.0f) < fSlow14) * ftbl0Modal_Bar_dspSIG0[std::max<int>(0, std::min<int>(int(fSlow15 * fRec6[0]), 245))] + 0.9f * fRec3[1];
			fRec7[0] = fSlow19 + 0.999f * fRec7[1];
			fRec8[0] = 0.999f * fRec8[1] + 0.003f;
			int iTemp1 = int(fRec8[0]);
			float fTemp2 = loadPreset(iSlow18, 1, iTemp1);
			float fTemp3 = loadPreset(iSlow18, 0, iTemp1);
			int iTemp4 = fTemp3 < 0.0f;
			fRec2[0] = fSlow11 * fRec3[0] * fRec7[0] - fSlow20 * fTemp2 * (fSlow20 * fTemp2 * fRec2[2] - 2.0f * std::cos(-(fConst1 * fTemp3 * (float(iTemp4) - fSlow21 * float(-(iTemp4 + -1))))) * fRec2[1]);
			fRec10[0] = fSlow22 + 0.999f * fRec10[1];
			fRec11[0] = 0.999f * fRec11[1] + 0.002f;
			int iTemp5 = int(fRec11[0]);
			float fTemp6 = loadPreset(iSlow18, 1, iTemp5);
			float fTemp7 = loadPreset(iSlow18, 0, iTemp5);
			int iTemp8 = fTemp7 < 0.0f;
			fRec9[0] = fSlow11 * fRec3[0] * fRec10[0] - fSlow20 * fTemp6 * (fSlow20 * fTemp6 * fRec9[2] - 2.0f * std::cos(-(fConst1 * fTemp7 * (float(iTemp8) - fSlow21 * float(-(iTemp8 + -1))))) * fRec9[1]);
			fRec13[0] = fSlow23 + 0.999f * fRec13[1];
			fRec14[0] = 0.999f * fRec14[1];
			int iTemp9 = int(fRec14[0]);
			float fTemp10 = loadPreset(iSlow18, 1, iTemp9);
			float fTemp11 = loadPreset(iSlow18, 0, iTemp9);
			int iTemp12 = fTemp11 < 0.0f;
			fRec12[0] = fSlow11 * fRec3[0] * fRec13[0] - fSlow20 * fTemp10 * (fSlow20 * fTemp10 * fRec12[2] - 2.0f * std::cos(-(fConst1 * fTemp11 * (float(iTemp12) - fSlow21 * float(-(iTemp12 + -1))))) * fRec12[1]);
			fRec16[0] = fSlow24 + 0.999f * fRec16[1];
			fRec17[0] = 0.999f * fRec17[1] + 0.001f;
			int iTemp13 = int(fRec17[0]);
			float fTemp14 = loadPreset(iSlow18, 1, iTemp13);
			float fTemp15 = loadPreset(iSlow18, 0, iTemp13);
			int iTemp16 = fTemp15 < 0.0f;
			fRec15[0] = fSlow11 * fRec3[0] * fRec16[0] - fSlow20 * fTemp14 * (fSlow20 * fTemp14 * fRec15[2] - 2.0f * std::cos(-(fConst1 * fTemp15 * (float(iTemp16) - fSlow21 * float(-(iTemp16 + -1))))) * fRec15[1]);
			float fTemp17 = fRec2[0] + fRec9[0] + fRec12[0] + fRec15[0];
			float fTemp18 = fTemp17 - fSlow25 * (fTemp17 - fSlow11 * fRec3[0]);
			fRec19[0] = fSlow27 + (fRec19[1] - std::floor(fSlow27 + fRec19[1]));
			float fTemp19 = fSlow26 * ftbl1Modal_Bar_dspSIG1[std::max<int>(0, std::min<int>(int(65536.0f * fRec19[0]), 65535))] + 1.0f;
			float fTemp20 = fTemp18 * fTemp19;
			fVec0[0] = fTemp20;
			fRec20[0] = fSlow28 + 0.999f * fRec20[1];
			fRec22[0] = fSlow31 + 0.999f * fRec22[1];
			float fTemp21 = fRec21[1] + fConst2 * (fSlow29 + fSlow30 * fRec22[0]);
			fRec21[0] = fTemp21 - std::floor(fTemp21);
			float fTemp22 = 3.1415927f * fRec20[0] * ftbl1Modal_Bar_dspSIG1[std::max<int>(0, std::min<int>(int(65536.0f * fRec21[0]), 65535))];
			float fTemp23 = std::sin(fTemp22);
			float fTemp24 = std::cos(fTemp22);
			float fTemp25 = fTemp20 * fTemp24 - fTemp23 * fRec23[1];
			float fTemp26 = fTemp24 * fTemp25 - fTemp23 * fRec24[1];
			float fTemp27 = fTemp24 * fTemp26 - fTemp23 * fRec25[1];
			float fTemp28 = fTemp24 * fTemp27 - fTemp23 * fRec26[1];
			float fTemp29 = fTemp24 * fTemp28 - fTemp23 * fRec27[1];
			fRec28[0] = fTemp24 * fTemp29 - fTemp23 * fRec28[1];
			fRec27[0] = fTemp23 * fTemp29 + fTemp24 * fRec28[1];
			fRec26[0] = fTemp23 * fTemp28 + fTemp24 * fRec27[1];
			fRec25[0] = fTemp23 * fTemp27 + fTemp24 * fRec26[1];
			fRec24[0] = fTemp23 * fTemp26 + fTemp24 * fRec25[1];
			fRec23[0] = fTemp23 * fTemp25 + fTemp24 * fRec24[1];
			float fTemp30 = fRec20[0] * (fSlow33 * fTemp20 + fSlow34 * (fTemp20 + fVec0[1]) + fSlow35 * Modal_Bar_dsp_faustpower2_f(fTemp18) * Modal_Bar_dsp_faustpower2_f(fTemp19));
			float fTemp31 = std::sin(fTemp30);
			float fTemp32 = std::cos(fTemp30);
			float fTemp33 = fTemp20 * fTemp32 - fTemp31 * fRec29[1];
			float fTemp34 = fTemp32 * fTemp33 - fTemp31 * fRec30[1];
			float fTemp35 = fTemp32 * fTemp34 - fTemp31 * fRec31[1];
			float fTemp36 = fTemp32 * fTemp35 - fTemp31 * fRec32[1];
			float fTemp37 = fTemp32 * fTemp36 - fTemp31 * fRec33[1];
			fRec34[0] = fTemp32 * fTemp37 - fTemp31 * fRec34[1];
			fRec33[0] = fTemp31 * fTemp37 + fTemp32 * fRec34[1];
			fRec32[0] = fTemp31 * fTemp36 + fTemp32 * fRec33[1];
			fRec31[0] = fTemp31 * fTemp35 + fTemp32 * fRec32[1];
			fRec30[0] = fTemp31 * fTemp34 + fTemp32 * fRec31[1];
			fRec29[0] = fTemp31 * fTemp33 + fTemp32 * fRec30[1];
			float fTemp38 = fRec1[0] * (fSlow10 * (fTemp20 * fTemp23 + fRec23[1] * fTemp24) + fSlow32 * (fRec20[0] * (fTemp20 * fTemp31 + fRec29[1] * fTemp32) + (1.0f - fRec20[0]) * fTemp18 * fTemp19));
			fVec1[IOTA0 & 4095] = fTemp38;
			output0[i0] = FAUSTFLOAT(fSlow1 * fTemp38);
			output1[i0] = FAUSTFLOAT(fSlow36 * fVec1[(IOTA0 - iSlow37) & 4095]);
			iRec0[1] = iRec0[0];
			fRec1[1] = fRec1[0];
			fRec4[1] = fRec4[0];
			fRec6[1] = fRec6[0];
			fRec3[1] = fRec3[0];
			fRec7[1] = fRec7[0];
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
			IOTA0 = IOTA0 + 1;
		}
	}

};


#include "Faust_plugins_template2.cpp"


#endif
