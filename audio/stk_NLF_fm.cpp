/* ------------------------------------------------------------
author: "Romain Michon"
copyright: "Romain Michon (rmichon@ccrma.stanford.edu)"
name: "NLFfm"
version: "1.0"
Code generated with Faust 2.81.2 (https://faust.grame.fr)
Compilation options: -a ../../Faust_plugins_template.cpp -lang cpp -nvi -ct 1 -cn NLF_Fm_dsp -es 1 -mcd 16 -mdd 1024 -mdy 33 -single -ftz 0
------------------------------------------------------------ */

#ifndef  __NLF_Fm_dsp_H__
#define  __NLF_Fm_dsp_H__


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
#define FAUSTCLASS NLF_Fm_dsp
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

class NLF_Fm_dspSIG0 {
	
  private:
	
	int iRec3[2];
	
  public:
	
	int getNumInputsNLF_Fm_dspSIG0() {
		return 0;
	}
	int getNumOutputsNLF_Fm_dspSIG0() {
		return 1;
	}
	
	void instanceInitNLF_Fm_dspSIG0(int sample_rate) {
		for (int l3 = 0; l3 < 2; l3 = l3 + 1) {
			iRec3[l3] = 0;
		}
	}
	
	void fillNLF_Fm_dspSIG0(int count, float* table) {
		for (int i1 = 0; i1 < count; i1 = i1 + 1) {
			iRec3[0] = iRec3[1] + 1;
			table[i1] = std::sin(9.58738e-05f * float(iRec3[0] + -1));
			iRec3[1] = iRec3[0];
		}
	}

};

static NLF_Fm_dspSIG0* newNLF_Fm_dspSIG0() { return (NLF_Fm_dspSIG0*)new NLF_Fm_dspSIG0(); }
static void deleteNLF_Fm_dspSIG0(NLF_Fm_dspSIG0* dsp) { delete dsp; }

static float *ftbl0NLF_Fm_dspSIG0; __attribute__((constructor)) static void initialize_ftbl0NLF_Fm_dspSIG0() { ftbl0NLF_Fm_dspSIG0 = (float*)calloc(65536, sizeof(float));};
static float NLF_Fm_dsp_faustpower2_f(float value) {
	return value * value;
}

class NLF_Fm_dsp final : public dsp {
	
 private:
	
	FAUSTFLOAT fHslider0;
	FAUSTFLOAT fEntry0;
	FAUSTFLOAT fButton0;
	int iRec0[2];
	int fSampleRate;
	float fConst0;
	FAUSTFLOAT fHslider1;
	FAUSTFLOAT fHslider2;
	FAUSTFLOAT fHslider3;
	float fRec1[2];
	FAUSTFLOAT fEntry1;
	float fRec2[2];
	float fConst1;
	FAUSTFLOAT fEntry2;
	float fRec4[2];
	FAUSTFLOAT fHslider4;
	int iRec5[2];
	int iRec6[2];
	FAUSTFLOAT fHslider5;
	float fConst2;
	float fConst3;
	FAUSTFLOAT fHslider6;
	float fRec7[2];
	FAUSTFLOAT fHslider7;
	float fRec8[2];
	float fVec0[2];
	FAUSTFLOAT fHslider8;
	float fRec9[2];
	int iRec10[2];
	FAUSTFLOAT fHslider9;
	float fRec11[2];
	FAUSTFLOAT fHslider10;
	float fRec13[2];
	float fRec12[2];
	float fRec16[2];
	float fRec15[2];
	float fRec14[2];
	float fRec19[2];
	float fRec18[2];
	float fRec17[2];
	int IOTA0;
	float fVec1[4096];
	float fConst4;
	FAUSTFLOAT fHslider11;
	
 public:
	NLF_Fm_dsp() {
	}
	
	void metadata(Meta* m) { 
		m->declare("author", "Romain Michon");
		m->declare("compile_options", "-a ../../Faust_plugins_template.cpp -lang cpp -nvi -ct 1 -cn NLF_Fm_dsp -es 1 -mcd 16 -mdd 1024 -mdy 33 -single -ftz 0");
		m->declare("copyright", "Romain Michon (rmichon@ccrma.stanford.edu)");
		m->declare("description", "FM synthesizer implemented with a nonlinear passive allpass filter");
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
		m->declare("filename", "NLFfm.dsp");
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
		m->declare("name", "NLFfm");
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
		NLF_Fm_dspSIG0* sig0 = newNLF_Fm_dspSIG0();
		sig0->instanceInitNLF_Fm_dspSIG0(sample_rate);
		sig0->fillNLF_Fm_dspSIG0(65536, ftbl0NLF_Fm_dspSIG0);
		deleteNLF_Fm_dspSIG0(sig0);
	}
	
	void instanceConstants(int sample_rate) {
		fSampleRate = sample_rate;
		fConst0 = std::min<float>(1.92e+05f, std::max<float>(1.0f, float(fSampleRate)));
		fConst1 = 1.0f / fConst0;
		fConst2 = 1.8f * fConst0;
		fConst3 = 0.2f * fConst0;
		fConst4 = 0.5f * fConst0;
	}
	
	void instanceResetUserInterface() {
		fHslider0 = FAUSTFLOAT(0.6f);
		fEntry0 = FAUSTFLOAT(0.0f);
		fButton0 = FAUSTFLOAT(0.0f);
		fHslider1 = FAUSTFLOAT(0.05f);
		fHslider2 = FAUSTFLOAT(0.05f);
		fHslider3 = FAUSTFLOAT(0.05f);
		fEntry1 = FAUSTFLOAT(0.8f);
		fEntry2 = FAUSTFLOAT(4.4e+02f);
		fHslider4 = FAUSTFLOAT(0.1f);
		fHslider5 = FAUSTFLOAT(0.5f);
		fHslider6 = FAUSTFLOAT(0.01f);
		fHslider7 = FAUSTFLOAT(5.0f);
		fHslider8 = FAUSTFLOAT(0.0f);
		fHslider9 = FAUSTFLOAT(0.1f);
		fHslider10 = FAUSTFLOAT(2.2e+02f);
		fHslider11 = FAUSTFLOAT(0.5f);
	}
	
	void instanceClear() {
		for (int l0 = 0; l0 < 2; l0 = l0 + 1) {
			iRec0[l0] = 0;
		}
		for (int l1 = 0; l1 < 2; l1 = l1 + 1) {
			fRec1[l1] = 0.0f;
		}
		for (int l2 = 0; l2 < 2; l2 = l2 + 1) {
			fRec2[l2] = 0.0f;
		}
		for (int l4 = 0; l4 < 2; l4 = l4 + 1) {
			fRec4[l4] = 0.0f;
		}
		for (int l5 = 0; l5 < 2; l5 = l5 + 1) {
			iRec5[l5] = 0;
		}
		for (int l6 = 0; l6 < 2; l6 = l6 + 1) {
			iRec6[l6] = 0;
		}
		for (int l7 = 0; l7 < 2; l7 = l7 + 1) {
			fRec7[l7] = 0.0f;
		}
		for (int l8 = 0; l8 < 2; l8 = l8 + 1) {
			fRec8[l8] = 0.0f;
		}
		for (int l9 = 0; l9 < 2; l9 = l9 + 1) {
			fVec0[l9] = 0.0f;
		}
		for (int l10 = 0; l10 < 2; l10 = l10 + 1) {
			fRec9[l10] = 0.0f;
		}
		for (int l11 = 0; l11 < 2; l11 = l11 + 1) {
			iRec10[l11] = 0;
		}
		for (int l12 = 0; l12 < 2; l12 = l12 + 1) {
			fRec11[l12] = 0.0f;
		}
		for (int l13 = 0; l13 < 2; l13 = l13 + 1) {
			fRec13[l13] = 0.0f;
		}
		for (int l14 = 0; l14 < 2; l14 = l14 + 1) {
			fRec12[l14] = 0.0f;
		}
		for (int l15 = 0; l15 < 2; l15 = l15 + 1) {
			fRec16[l15] = 0.0f;
		}
		for (int l16 = 0; l16 < 2; l16 = l16 + 1) {
			fRec15[l16] = 0.0f;
		}
		for (int l17 = 0; l17 < 2; l17 = l17 + 1) {
			fRec14[l17] = 0.0f;
		}
		for (int l18 = 0; l18 < 2; l18 = l18 + 1) {
			fRec19[l18] = 0.0f;
		}
		for (int l19 = 0; l19 < 2; l19 = l19 + 1) {
			fRec18[l19] = 0.0f;
		}
		for (int l20 = 0; l20 < 2; l20 = l20 + 1) {
			fRec17[l20] = 0.0f;
		}
		IOTA0 = 0;
		for (int l21 = 0; l21 < 4096; l21 = l21 + 1) {
			fVec1[l21] = 0.0f;
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
	
	NLF_Fm_dsp* clone() {
		return new NLF_Fm_dsp();
	}
	
	int getSampleRate() {
		return fSampleRate;
	}
	
	void buildUserInterface(UI* ui_interface) {
		ui_interface->openVerticalBox("NLFfm");
		ui_interface->openHorizontalBox("Basic_Parameters");
		ui_interface->declare(&fEntry2, "1", "");
		ui_interface->declare(&fEntry2, "tooltip", "Tone frequency");
		ui_interface->declare(&fEntry2, "unit", "Hz");
		ui_interface->addNumEntry("freq", &fEntry2, FAUSTFLOAT(4.4e+02f), FAUSTFLOAT(2e+01f), FAUSTFLOAT(2e+04f), FAUSTFLOAT(1.0f));
		ui_interface->declare(&fEntry1, "1", "");
		ui_interface->declare(&fEntry1, "tooltip", "Gain (value between 0 and 1)");
		ui_interface->addNumEntry("gain", &fEntry1, FAUSTFLOAT(0.8f), FAUSTFLOAT(0.0f), FAUSTFLOAT(1.0f), FAUSTFLOAT(0.01f));
		ui_interface->declare(&fButton0, "1", "");
		ui_interface->declare(&fButton0, "tooltip", "noteOn = 1, noteOff = 0");
		ui_interface->addButton("gate", &fButton0);
		ui_interface->closeBox();
		ui_interface->openHorizontalBox("Envelopes_and_Vibrato");
		ui_interface->openVerticalBox("Envelope_Parameters");
		ui_interface->declare(&fHslider1, "4", "");
		ui_interface->declare(&fHslider1, "tooltip", "Envelope attack duration");
		ui_interface->declare(&fHslider1, "unit", "s");
		ui_interface->addHorizontalSlider("Envelope_Attack", &fHslider1, FAUSTFLOAT(0.05f), FAUSTFLOAT(0.0f), FAUSTFLOAT(2.0f), FAUSTFLOAT(0.01f));
		ui_interface->declare(&fHslider2, "4", "");
		ui_interface->declare(&fHslider2, "tooltip", "Envelope decay duration");
		ui_interface->declare(&fHslider2, "unit", "s");
		ui_interface->addHorizontalSlider("Envelope_Decay", &fHslider2, FAUSTFLOAT(0.05f), FAUSTFLOAT(0.0f), FAUSTFLOAT(2.0f), FAUSTFLOAT(0.01f));
		ui_interface->declare(&fHslider3, "4", "");
		ui_interface->declare(&fHslider3, "tooltip", "Envelope release duration");
		ui_interface->declare(&fHslider3, "unit", "s");
		ui_interface->addHorizontalSlider("Envelope_Release", &fHslider3, FAUSTFLOAT(0.05f), FAUSTFLOAT(0.0f), FAUSTFLOAT(2.0f), FAUSTFLOAT(0.01f));
		ui_interface->closeBox();
		ui_interface->openVerticalBox("Vibrato_Parameters");
		ui_interface->declare(&fHslider5, "3", "");
		ui_interface->declare(&fHslider5, "tooltip", "Vibrato attack duration");
		ui_interface->declare(&fHslider5, "unit", "s");
		ui_interface->addHorizontalSlider("Vibrato_Attack", &fHslider5, FAUSTFLOAT(0.5f), FAUSTFLOAT(0.0f), FAUSTFLOAT(2.0f), FAUSTFLOAT(0.01f));
		ui_interface->declare(&fHslider7, "3", "");
		ui_interface->declare(&fHslider7, "unit", "Hz");
		ui_interface->addHorizontalSlider("Vibrato_Freq", &fHslider7, FAUSTFLOAT(5.0f), FAUSTFLOAT(1.0f), FAUSTFLOAT(15.0f), FAUSTFLOAT(0.1f));
		ui_interface->declare(&fHslider4, "3", "");
		ui_interface->declare(&fHslider4, "tooltip", "A value between 0 and 1");
		ui_interface->addHorizontalSlider("Vibrato_Gain", &fHslider4, FAUSTFLOAT(0.1f), FAUSTFLOAT(0.0f), FAUSTFLOAT(1.0f), FAUSTFLOAT(0.01f));
		ui_interface->declare(&fHslider6, "3", "");
		ui_interface->declare(&fHslider6, "tooltip", "Vibrato release duration");
		ui_interface->declare(&fHslider6, "unit", "s");
		ui_interface->addHorizontalSlider("Vibrato_Release", &fHslider6, FAUSTFLOAT(0.01f), FAUSTFLOAT(0.0f), FAUSTFLOAT(2.0f), FAUSTFLOAT(0.01f));
		ui_interface->closeBox();
		ui_interface->closeBox();
		ui_interface->openHorizontalBox("Physical_and_Nonlinearity");
		ui_interface->openVerticalBox("Nonlinear_Filter_Parameters");
		ui_interface->declare(&fHslider10, "2", "");
		ui_interface->declare(&fHslider10, "tooltip", "Frequency of the sine wave for the modulation of theta (works if Modulation Type=3)");
		ui_interface->declare(&fHslider10, "unit", "Hz");
		ui_interface->addHorizontalSlider("Modulation_Frequency", &fHslider10, FAUSTFLOAT(2.2e+02f), FAUSTFLOAT(2e+01f), FAUSTFLOAT(1e+03f), FAUSTFLOAT(0.1f));
		ui_interface->declare(&fEntry0, "2", "");
		ui_interface->declare(&fEntry0, "tooltip", "0=theta is modulated by the incoming signal; 1=theta is modulated by the averaged incoming signal; 2=theta is modulated by the squared incoming signal; 3=theta is modulated by a sine wave of frequency freqMod; 4=theta is modulated by a sine wave of frequency freq;");
		ui_interface->addNumEntry("Modulation_Type", &fEntry0, FAUSTFLOAT(0.0f), FAUSTFLOAT(0.0f), FAUSTFLOAT(4.0f), FAUSTFLOAT(1.0f));
		ui_interface->declare(&fHslider8, "2", "");
		ui_interface->declare(&fHslider8, "tooltip", "Nonlinearity factor (value between 0 and 1)");
		ui_interface->addHorizontalSlider("Nonlinearity", &fHslider8, FAUSTFLOAT(0.0f), FAUSTFLOAT(0.0f), FAUSTFLOAT(1.0f), FAUSTFLOAT(0.01f));
		ui_interface->declare(&fHslider9, "2", "");
		ui_interface->declare(&fHslider9, "Attack duration of the nonlinearity", "");
		ui_interface->declare(&fHslider9, "unit", "s");
		ui_interface->addHorizontalSlider("Nonlinearity_Attack", &fHslider9, FAUSTFLOAT(0.1f), FAUSTFLOAT(0.0f), FAUSTFLOAT(2.0f), FAUSTFLOAT(0.01f));
		ui_interface->closeBox();
		ui_interface->closeBox();
		ui_interface->openVerticalBox("Spat");
		ui_interface->addHorizontalSlider("pan angle", &fHslider0, FAUSTFLOAT(0.6f), FAUSTFLOAT(0.0f), FAUSTFLOAT(1.0f), FAUSTFLOAT(0.01f));
		ui_interface->addHorizontalSlider("spatial width", &fHslider11, FAUSTFLOAT(0.5f), FAUSTFLOAT(0.0f), FAUSTFLOAT(1.0f), FAUSTFLOAT(0.01f));
		ui_interface->closeBox();
		ui_interface->closeBox();
	}
	
	void compute(int count, FAUSTFLOAT** RESTRICT inputs, FAUSTFLOAT** RESTRICT outputs) {
		FAUSTFLOAT* output0 = outputs[0];
		FAUSTFLOAT* output1 = outputs[1];
		float fSlow0 = float(fHslider0);
		float fSlow1 = 1.0f - fSlow0;
		float fSlow2 = float(fEntry0);
		float fSlow3 = float(fSlow2 >= 3.0f);
		float fSlow4 = float(fButton0);
		int iSlow5 = fSlow4 > 0.0f;
		float fSlow6 = float(fHslider1);
		float fSlow7 = 1.0f / (fConst0 * fSlow6 + float(fSlow6 == 0.0f));
		float fSlow8 = float(fHslider2);
		float fSlow9 = 1.0f - std::pow(9e+01f, 1.0f / (fConst0 * fSlow8 + float(fSlow8 == 0.0f)));
		float fSlow10 = float(fHslider3);
		float fSlow11 = 1.0f / (fConst0 * fSlow10 + float(fSlow10 == 0.0f));
		float fSlow12 = 1.0f - 1.0f / std::pow(9e+04f, fSlow11);
		int iSlow13 = fSlow4 <= 0.0f;
		float fSlow14 = 0.001f * float(fEntry1);
		float fSlow15 = float(fEntry2);
		float fSlow16 = fConst1 * fSlow15;
		float fSlow17 = float(fHslider4);
		float fSlow18 = float(fHslider5);
		float fSlow19 = 1.0f / (float((1.8f * fSlow18) == 0.0f) + fConst2 * fSlow18);
		float fSlow20 = fConst3 * fSlow18;
		float fSlow21 = float((0.2f * fSlow18) == 0.0f) + fSlow20;
		float fSlow22 = float(fHslider6);
		float fSlow23 = 1.0f - 1.0f / std::pow(1e+05f, 1.0f / (fConst0 * fSlow22 + float(fSlow22 == 0.0f)));
		float fSlow24 = fConst1 * float(fHslider7);
		float fSlow25 = 0.001f * float(fHslider8);
		float fSlow26 = float(fHslider9);
		float fSlow27 = 1.0f / (fConst0 * fSlow26 + float(fSlow26 == 0.0f));
		float fSlow28 = 1.0f - 1.0f / std::pow(1e+05f, fSlow11);
		float fSlow29 = fSlow15 * float(fSlow2 == 4.0f);
		float fSlow30 = float(fSlow2 != 4.0f);
		float fSlow31 = 0.001f * float(fHslider10);
		float fSlow32 = float(fSlow2 < 3.0f);
		float fSlow33 = 3.1415927f * float(fSlow2 == 0.0f);
		float fSlow34 = 1.5707964f * float(fSlow2 == 1.0f);
		float fSlow35 = 3.1415927f * float(fSlow2 == 2.0f);
		int iSlow36 = int(fConst4 * (float(fHslider11) / fSlow15)) & 4095;
		for (int i0 = 0; i0 < count; i0 = i0 + 1) {
			iRec0[0] = iSlow5 & (iRec0[1] | (fRec1[1] >= 1.0f));
			int iTemp0 = iSlow13 & (fRec1[1] > 0.0f);
			fRec1[0] = (fSlow7 * float(((iRec0[1] == 0) & iSlow5) & (fRec1[1] < 1.0f)) + fRec1[1] * (1.0f - fSlow9 * float(iRec0[1] & (fRec1[1] > 9e+01f)) - fSlow12 * float(iTemp0))) * float((iTemp0 == 0) | (fRec1[1] >= 1e-06f));
			fRec2[0] = fSlow14 + 0.999f * fRec2[1];
			float fTemp1 = fRec1[0] * fRec2[0];
			fRec4[0] = fSlow16 + (fRec4[1] - std::floor(fSlow16 + fRec4[1]));
			float fTemp2 = ftbl0NLF_Fm_dspSIG0[std::max<int>(0, std::min<int>(int(65536.0f * fRec4[0]), 65535))];
			iRec5[0] = iSlow5 & (iRec5[1] | (fRec7[1] >= 1.0f));
			iRec6[0] = iSlow5 * (iRec6[1] + 1);
			float fTemp3 = float(iRec6[1]);
			int iTemp4 = iSlow13 & (fRec7[1] > 0.0f);
			fRec7[0] = (fSlow19 * float(((((iRec5[1] == 0) & iSlow5) & (fRec7[1] < 1.0f)) & (fTemp3 > fSlow20)) * (1 - (fTemp3 < fSlow21))) + fRec7[1] * (1.0f - fSlow23 * float(iTemp4))) * float((iTemp4 == 0) | (fRec7[1] >= 1e-06f));
			fRec8[0] = fSlow24 + (fRec8[1] - std::floor(fSlow24 + fRec8[1]));
			float fTemp5 = fSlow17 * fRec7[0] * ftbl0NLF_Fm_dspSIG0[std::max<int>(0, std::min<int>(int(65536.0f * fRec8[0]), 65535))] + 1.0f;
			float fTemp6 = fTemp1 * fTemp2 * fTemp5;
			fVec0[0] = fTemp6;
			fRec9[0] = fSlow25 + 0.999f * fRec9[1];
			iRec10[0] = iSlow5 & (iRec10[1] | (fRec11[1] >= 1.0f));
			int iTemp7 = iSlow13 & (fRec11[1] > 0.0f);
			fRec11[0] = (fSlow27 * float(((iRec10[1] == 0) & iSlow5) & (fRec11[1] < 1.0f)) + fRec11[1] * (1.0f - fSlow28 * float(iTemp7))) * float((iTemp7 == 0) | (fRec11[1] >= 1e-06f));
			float fTemp8 = fRec9[0] * fRec11[0];
			fRec13[0] = fSlow31 + 0.999f * fRec13[1];
			float fTemp9 = fRec12[1] + fConst1 * (fSlow29 + fSlow30 * fRec13[0]);
			fRec12[0] = fTemp9 - std::floor(fTemp9);
			float fTemp10 = 3.1415927f * fTemp8 * ftbl0NLF_Fm_dspSIG0[std::max<int>(0, std::min<int>(int(65536.0f * fRec12[0]), 65535))];
			float fTemp11 = std::sin(fTemp10);
			float fTemp12 = std::cos(fTemp10);
			float fTemp13 = fTemp6 * fTemp12 - fTemp11 * fRec14[1];
			float fTemp14 = fTemp12 * fTemp13 - fTemp11 * fRec15[1];
			fRec16[0] = fTemp12 * fTemp14 - fTemp11 * fRec16[1];
			fRec15[0] = fTemp11 * fTemp14 + fTemp12 * fRec16[1];
			fRec14[0] = fTemp11 * fTemp13 + fTemp12 * fRec15[1];
			float fTemp15 = fTemp8 * (fSlow33 * fTemp6 + fSlow34 * (fTemp6 + fVec0[1]) + fSlow35 * NLF_Fm_dsp_faustpower2_f(fRec1[0]) * NLF_Fm_dsp_faustpower2_f(fRec2[0]) * NLF_Fm_dsp_faustpower2_f(fTemp2) * NLF_Fm_dsp_faustpower2_f(fTemp5));
			float fTemp16 = std::sin(fTemp15);
			float fTemp17 = std::cos(fTemp15);
			float fTemp18 = fTemp6 * fTemp17 - fTemp16 * fRec17[1];
			float fTemp19 = fTemp17 * fTemp18 - fTemp16 * fRec18[1];
			fRec19[0] = fTemp17 * fTemp19 - fTemp16 * fRec19[1];
			fRec18[0] = fTemp16 * fTemp19 + fTemp17 * fRec19[1];
			fRec17[0] = fTemp16 * fTemp18 + fTemp17 * fRec18[1];
			float fTemp20 = fSlow3 * (fTemp6 * fTemp11 + fRec14[1] * fTemp12) + fSlow32 * (fRec9[0] * (fTemp6 * fTemp16 + fRec17[1] * fTemp17) + fTemp1 * (1.0f - fRec9[0]) * fTemp2 * fTemp5);
			fVec1[IOTA0 & 4095] = fTemp20;
			output0[i0] = FAUSTFLOAT(fSlow1 * fTemp20);
			output1[i0] = FAUSTFLOAT(fSlow0 * fVec1[(IOTA0 - iSlow36) & 4095]);
			iRec0[1] = iRec0[0];
			fRec1[1] = fRec1[0];
			fRec2[1] = fRec2[0];
			fRec4[1] = fRec4[0];
			iRec5[1] = iRec5[0];
			iRec6[1] = iRec6[0];
			fRec7[1] = fRec7[0];
			fRec8[1] = fRec8[0];
			fVec0[1] = fVec0[0];
			fRec9[1] = fRec9[0];
			iRec10[1] = iRec10[0];
			fRec11[1] = fRec11[0];
			fRec13[1] = fRec13[0];
			fRec12[1] = fRec12[0];
			fRec16[1] = fRec16[0];
			fRec15[1] = fRec15[0];
			fRec14[1] = fRec14[0];
			fRec19[1] = fRec19[0];
			fRec18[1] = fRec18[0];
			fRec17[1] = fRec17[0];
			IOTA0 = IOTA0 + 1;
		}
	}

};


#include "Faust_plugins_template2.cpp"


#endif
