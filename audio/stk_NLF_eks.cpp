/* ------------------------------------------------------------
author: "Julius Smith and Romain Michon"
copyright: "Julius Smith"
license: "STK-4.3"
name: "Nonlinear EKS"
version: "1.0"
Code generated with Faust 2.81.2 (https://faust.grame.fr)
Compilation options: -a ../../Faust_plugins_template.cpp -lang cpp -nvi -ct 1 -cn NLF_Eks_dsp -es 1 -mcd 16 -mdd 1024 -mdy 33 -single -ftz 0
------------------------------------------------------------ */

#ifndef  __NLF_Eks_dsp_H__
#define  __NLF_Eks_dsp_H__


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
#define FAUSTCLASS NLF_Eks_dsp
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

class NLF_Eks_dspSIG0 {
	
  private:
	
	int iRec6[2];
	
  public:
	
	int getNumInputsNLF_Eks_dspSIG0() {
		return 0;
	}
	int getNumOutputsNLF_Eks_dspSIG0() {
		return 1;
	}
	
	void instanceInitNLF_Eks_dspSIG0(int sample_rate) {
		for (int l6 = 0; l6 < 2; l6 = l6 + 1) {
			iRec6[l6] = 0;
		}
	}
	
	void fillNLF_Eks_dspSIG0(int count, float* table) {
		for (int i1 = 0; i1 < count; i1 = i1 + 1) {
			iRec6[0] = iRec6[1] + 1;
			table[i1] = std::sin(9.58738e-05f * float(iRec6[0] + -1));
			iRec6[1] = iRec6[0];
		}
	}

};

static NLF_Eks_dspSIG0* newNLF_Eks_dspSIG0() { return (NLF_Eks_dspSIG0*)new NLF_Eks_dspSIG0(); }
static void deleteNLF_Eks_dspSIG0(NLF_Eks_dspSIG0* dsp) { delete dsp; }

static float *ftbl0NLF_Eks_dspSIG0; __attribute__((constructor)) static void initialize_ftbl0NLF_Eks_dspSIG0() { ftbl0NLF_Eks_dspSIG0 = (float*)calloc(65536, sizeof(float));};
static float NLF_Eks_dsp_faustpower2_f(float value) {
	return value * value;
}

class NLF_Eks_dsp final : public dsp {
	
 private:
	
	FAUSTFLOAT fHslider0;
	int fSampleRate;
	float fConst0;
	FAUSTFLOAT fEntry0;
	FAUSTFLOAT fHslider1;
	float fConst1;
	FAUSTFLOAT fHslider2;
	int IOTA0;
	FAUSTFLOAT fEntry1;
	int iRec3[2];
	FAUSTFLOAT fButton0;
	float fVec0[2];
	float fConst2;
	float fRec4[2];
	float fRec2[4096];
	FAUSTFLOAT fHslider3;
	float fRec1[2];
	FAUSTFLOAT fEntry2;
	FAUSTFLOAT fHslider4;
	FAUSTFLOAT fHslider5;
	FAUSTFLOAT fHslider6;
	float fRec5[2];
	FAUSTFLOAT fHslider7;
	float fRec8[2];
	float fRec7[2];
	float fRec14[2];
	float fRec13[2];
	float fRec12[2];
	float fRec11[2];
	float fRec10[2];
	float fRec9[2];
	float fVec1[2];
	float fRec20[2];
	float fRec19[2];
	float fRec18[2];
	float fRec17[2];
	float fRec16[2];
	float fRec15[2];
	float fVec2[4096];
	float fRec0[4096];
	float fConst3;
	FAUSTFLOAT fHslider8;
	
 public:
	NLF_Eks_dsp() {
	}
	
	void metadata(Meta* m) { 
		m->declare("author", "Julius Smith and Romain Michon");
		m->declare("compile_options", "-a ../../Faust_plugins_template.cpp -lang cpp -nvi -ct 1 -cn NLF_Eks_dsp -es 1 -mcd 16 -mdd 1024 -mdy 33 -single -ftz 0");
		m->declare("copyright", "Julius Smith");
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
		m->declare("filename", "NLFeks.dsp");
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
		m->declare("license", "STK-4.3");
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
		m->declare("name", "Nonlinear EKS");
		m->declare("reference", "http://ccrma.stanford.edu/~jos/pasp/vegf.html");
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
		NLF_Eks_dspSIG0* sig0 = newNLF_Eks_dspSIG0();
		sig0->instanceInitNLF_Eks_dspSIG0(sample_rate);
		sig0->fillNLF_Eks_dspSIG0(65536, ftbl0NLF_Eks_dspSIG0);
		deleteNLF_Eks_dspSIG0(sig0);
	}
	
	void instanceConstants(int sample_rate) {
		fSampleRate = sample_rate;
		fConst0 = std::min<float>(1.92e+05f, std::max<float>(1.0f, float(fSampleRate)));
		fConst1 = 3.1415927f / fConst0;
		fConst2 = 1.0f / fConst0;
		fConst3 = 0.5f * fConst0;
	}
	
	void instanceResetUserInterface() {
		fHslider0 = FAUSTFLOAT(0.6f);
		fEntry0 = FAUSTFLOAT(4.4e+02f);
		fHslider1 = FAUSTFLOAT(-1e+01f);
		fHslider2 = FAUSTFLOAT(0.0f);
		fEntry1 = FAUSTFLOAT(1.0f);
		fButton0 = FAUSTFLOAT(0.0f);
		fHslider3 = FAUSTFLOAT(0.13f);
		fEntry2 = FAUSTFLOAT(0.0f);
		fHslider4 = FAUSTFLOAT(4.0f);
		fHslider5 = FAUSTFLOAT(0.5f);
		fHslider6 = FAUSTFLOAT(0.0f);
		fHslider7 = FAUSTFLOAT(2.2e+02f);
		fHslider8 = FAUSTFLOAT(0.5f);
	}
	
	void instanceClear() {
		IOTA0 = 0;
		for (int l0 = 0; l0 < 2; l0 = l0 + 1) {
			iRec3[l0] = 0;
		}
		for (int l1 = 0; l1 < 2; l1 = l1 + 1) {
			fVec0[l1] = 0.0f;
		}
		for (int l2 = 0; l2 < 2; l2 = l2 + 1) {
			fRec4[l2] = 0.0f;
		}
		for (int l3 = 0; l3 < 4096; l3 = l3 + 1) {
			fRec2[l3] = 0.0f;
		}
		for (int l4 = 0; l4 < 2; l4 = l4 + 1) {
			fRec1[l4] = 0.0f;
		}
		for (int l5 = 0; l5 < 2; l5 = l5 + 1) {
			fRec5[l5] = 0.0f;
		}
		for (int l7 = 0; l7 < 2; l7 = l7 + 1) {
			fRec8[l7] = 0.0f;
		}
		for (int l8 = 0; l8 < 2; l8 = l8 + 1) {
			fRec7[l8] = 0.0f;
		}
		for (int l9 = 0; l9 < 2; l9 = l9 + 1) {
			fRec14[l9] = 0.0f;
		}
		for (int l10 = 0; l10 < 2; l10 = l10 + 1) {
			fRec13[l10] = 0.0f;
		}
		for (int l11 = 0; l11 < 2; l11 = l11 + 1) {
			fRec12[l11] = 0.0f;
		}
		for (int l12 = 0; l12 < 2; l12 = l12 + 1) {
			fRec11[l12] = 0.0f;
		}
		for (int l13 = 0; l13 < 2; l13 = l13 + 1) {
			fRec10[l13] = 0.0f;
		}
		for (int l14 = 0; l14 < 2; l14 = l14 + 1) {
			fRec9[l14] = 0.0f;
		}
		for (int l15 = 0; l15 < 2; l15 = l15 + 1) {
			fVec1[l15] = 0.0f;
		}
		for (int l16 = 0; l16 < 2; l16 = l16 + 1) {
			fRec20[l16] = 0.0f;
		}
		for (int l17 = 0; l17 < 2; l17 = l17 + 1) {
			fRec19[l17] = 0.0f;
		}
		for (int l18 = 0; l18 < 2; l18 = l18 + 1) {
			fRec18[l18] = 0.0f;
		}
		for (int l19 = 0; l19 < 2; l19 = l19 + 1) {
			fRec17[l19] = 0.0f;
		}
		for (int l20 = 0; l20 < 2; l20 = l20 + 1) {
			fRec16[l20] = 0.0f;
		}
		for (int l21 = 0; l21 < 2; l21 = l21 + 1) {
			fRec15[l21] = 0.0f;
		}
		for (int l22 = 0; l22 < 4096; l22 = l22 + 1) {
			fVec2[l22] = 0.0f;
		}
		for (int l23 = 0; l23 < 4096; l23 = l23 + 1) {
			fRec0[l23] = 0.0f;
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
	
	NLF_Eks_dsp* clone() {
		return new NLF_Eks_dsp();
	}
	
	int getSampleRate() {
		return fSampleRate;
	}
	
	void buildUserInterface(UI* ui_interface) {
		ui_interface->openVerticalBox("Nonlinear EKS");
		ui_interface->openVerticalBox("Nonlinear Filter");
		ui_interface->addNumEntry("typeMod", &fEntry2, FAUSTFLOAT(0.0f), FAUSTFLOAT(0.0f), FAUSTFLOAT(4.0f), FAUSTFLOAT(1.0f));
		ui_interface->closeBox();
		ui_interface->addHorizontalSlider("Nonlinearity", &fHslider6, FAUSTFLOAT(0.0f), FAUSTFLOAT(0.0f), FAUSTFLOAT(1.0f), FAUSTFLOAT(0.01f));
		ui_interface->openVerticalBox("Spat");
		ui_interface->addHorizontalSlider("pan angle", &fHslider0, FAUSTFLOAT(0.6f), FAUSTFLOAT(0.0f), FAUSTFLOAT(1.0f), FAUSTFLOAT(0.01f));
		ui_interface->addHorizontalSlider("spatial width", &fHslider8, FAUSTFLOAT(0.5f), FAUSTFLOAT(0.0f), FAUSTFLOAT(1.0f), FAUSTFLOAT(0.01f));
		ui_interface->closeBox();
		ui_interface->declare(&fHslider5, "midi", "ctrl 0x74");
		ui_interface->addHorizontalSlider("brightness", &fHslider5, FAUSTFLOAT(0.5f), FAUSTFLOAT(0.0f), FAUSTFLOAT(1.0f), FAUSTFLOAT(0.01f));
		ui_interface->addHorizontalSlider("decaytime_T60", &fHslider4, FAUSTFLOAT(4.0f), FAUSTFLOAT(0.0f), FAUSTFLOAT(1e+01f), FAUSTFLOAT(0.01f));
		ui_interface->addHorizontalSlider("dynamic_level", &fHslider1, FAUSTFLOAT(-1e+01f), FAUSTFLOAT(-6e+01f), FAUSTFLOAT(0.0f), FAUSTFLOAT(1.0f));
		ui_interface->addNumEntry("freq", &fEntry0, FAUSTFLOAT(4.4e+02f), FAUSTFLOAT(2e+01f), FAUSTFLOAT(7.04e+03f), FAUSTFLOAT(1.0f));
		ui_interface->addHorizontalSlider("freqMod", &fHslider7, FAUSTFLOAT(2.2e+02f), FAUSTFLOAT(2e+01f), FAUSTFLOAT(1e+03f), FAUSTFLOAT(0.1f));
		ui_interface->addNumEntry("gain", &fEntry1, FAUSTFLOAT(1.0f), FAUSTFLOAT(0.0f), FAUSTFLOAT(1e+01f), FAUSTFLOAT(0.01f));
		ui_interface->addButton("gate", &fButton0);
		ui_interface->addHorizontalSlider("pick_angle", &fHslider2, FAUSTFLOAT(0.0f), FAUSTFLOAT(0.0f), FAUSTFLOAT(0.9f), FAUSTFLOAT(0.1f));
		ui_interface->declare(&fHslider3, "midi", "ctrl 0x81");
		ui_interface->addHorizontalSlider("pick_position", &fHslider3, FAUSTFLOAT(0.13f), FAUSTFLOAT(0.02f), FAUSTFLOAT(0.5f), FAUSTFLOAT(0.01f));
		ui_interface->closeBox();
	}
	
	void compute(int count, FAUSTFLOAT** RESTRICT inputs, FAUSTFLOAT** RESTRICT outputs) {
		FAUSTFLOAT* output0 = outputs[0];
		FAUSTFLOAT* output1 = outputs[1];
		float fSlow0 = float(fHslider0);
		float fSlow1 = 1.0f - fSlow0;
		float fSlow2 = float(fEntry0);
		float fSlow3 = fConst0 / fSlow2;
		int iSlow4 = int(fSlow3 + -3.499995f);
		float fSlow5 = float(iSlow4);
		float fSlow6 = fSlow3 + (-6.0f - fSlow5);
		float fSlow7 = fSlow3 + (-5.0f - fSlow5);
		float fSlow8 = fSlow3 + (-4.0f - fSlow5);
		float fSlow9 = fSlow3 + (-3.0f - fSlow5);
		float fSlow10 = 0.041666668f * fSlow9;
		float fSlow11 = std::pow(1e+01f, 0.05f * float(fHslider1));
		float fSlow12 = 1.0f - fSlow11;
		float fSlow13 = fConst1 * fSlow2;
		float fSlow14 = 1.0f / (fSlow13 + 1.0f);
		float fSlow15 = 1.0f - fSlow13;
		float fSlow16 = 0.9f * float(fHslider2);
		float fSlow17 = 4.656613e-10f * float(fEntry1) * (1.0f - fSlow16);
		float fSlow18 = float(fButton0);
		float fSlow19 = fConst2 * fSlow2;
		int iSlow20 = int(fConst0 * (float(fHslider3) / fSlow2)) & 4095;
		float fSlow21 = float(fEntry2);
		float fSlow22 = float(fSlow21 >= 3.0f);
		float fSlow23 = std::pow(0.001f, 1.0f / (fSlow2 * float(fHslider4)));
		float fSlow24 = float(fHslider5);
		float fSlow25 = 0.5f * (fSlow24 + 1.0f);
		float fSlow26 = 0.25f * (1.0f - fSlow24);
		float fSlow27 = 0.001f * float(fHslider6);
		float fSlow28 = fSlow2 * float(fSlow21 == 4.0f);
		float fSlow29 = float(fSlow21 != 4.0f);
		float fSlow30 = 0.001f * float(fHslider7);
		float fSlow31 = float(fSlow21 < 3.0f);
		float fSlow32 = 3.1415927f * fSlow23 * float(fSlow21 == 0.0f);
		float fSlow33 = 1.5707964f * float(fSlow21 == 1.0f);
		float fSlow34 = 3.1415927f * NLF_Eks_dsp_faustpower2_f(fSlow23) * float(fSlow21 == 2.0f);
		float fSlow35 = fSlow11 * std::pow(fSlow11, 0.33333334f);
		int iSlow36 = iSlow4 & 4095;
		float fSlow37 = fSlow3 + (-2.0f - fSlow5);
		float fSlow38 = 0.16666667f * fSlow37;
		int iSlow39 = (iSlow4 + 1) & 4095;
		float fSlow40 = fSlow37 * fSlow9;
		float fSlow41 = 0.25f * fSlow40;
		int iSlow42 = (iSlow4 + 2) & 4095;
		float fSlow43 = fSlow40 * fSlow8;
		float fSlow44 = 0.16666667f * fSlow43;
		int iSlow45 = (iSlow4 + 3) & 4095;
		float fSlow46 = 0.041666668f * fSlow43 * fSlow7;
		int iSlow47 = (iSlow4 + 4) & 4095;
		int iSlow48 = int(fConst3 * (float(fHslider8) / fSlow2)) & 4095;
		for (int i0 = 0; i0 < count; i0 = i0 + 1) {
			iRec3[0] = 1103515245 * iRec3[1] + 12345;
			fVec0[0] = fSlow18;
			fRec4[0] = fRec4[1] + float((fSlow18 - fVec0[1]) > 0.0f) - fSlow19 * float(fRec4[1] > 0.0f);
			fRec2[IOTA0 & 4095] = fSlow16 * fRec2[(IOTA0 - 1) & 4095] + fSlow17 * float(iRec3[0]) * float(fRec4[0] > 0.0f);
			float fTemp0 = fRec2[IOTA0 & 4095] - fRec2[(IOTA0 - iSlow20) & 4095];
			fRec1[0] = fSlow14 * (fSlow15 * fRec1[1] + fSlow13 * fTemp0);
			float fTemp1 = fSlow25 * fRec0[(IOTA0 - 2) & 4095] + fSlow26 * (fRec0[(IOTA0 - 1) & 4095] + fRec0[(IOTA0 - 3) & 4095]);
			fRec5[0] = fSlow27 + 0.999f * fRec5[1];
			fRec8[0] = fSlow30 + 0.999f * fRec8[1];
			float fTemp2 = fRec7[1] + fConst2 * (fSlow28 + fSlow29 * fRec8[0]);
			fRec7[0] = fTemp2 - std::floor(fTemp2);
			float fTemp3 = 3.1415927f * fRec5[0] * ftbl0NLF_Eks_dspSIG0[std::max<int>(0, std::min<int>(int(65536.0f * fRec7[0]), 65535))];
			float fTemp4 = std::sin(fTemp3);
			float fTemp5 = std::cos(fTemp3);
			float fTemp6 = fSlow23 * fTemp1 * fTemp5 - fTemp4 * fRec9[1];
			float fTemp7 = fTemp5 * fTemp6 - fTemp4 * fRec10[1];
			float fTemp8 = fTemp5 * fTemp7 - fTemp4 * fRec11[1];
			float fTemp9 = fTemp5 * fTemp8 - fTemp4 * fRec12[1];
			float fTemp10 = fTemp5 * fTemp9 - fTemp4 * fRec13[1];
			fRec14[0] = fTemp5 * fTemp10 - fTemp4 * fRec14[1];
			fRec13[0] = fTemp4 * fTemp10 + fTemp5 * fRec14[1];
			fRec12[0] = fTemp4 * fTemp9 + fTemp5 * fRec13[1];
			fRec11[0] = fTemp4 * fTemp8 + fTemp5 * fRec12[1];
			fRec10[0] = fTemp4 * fTemp7 + fTemp5 * fRec11[1];
			fRec9[0] = fTemp4 * fTemp6 + fTemp5 * fRec10[1];
			float fTemp11 = fSlow23 * fTemp1;
			fVec1[0] = fTemp11;
			float fTemp12 = fRec5[0] * (fSlow32 * fTemp1 + fSlow33 * (fTemp11 + fVec1[1]) + fSlow34 * NLF_Eks_dsp_faustpower2_f(fTemp1));
			float fTemp13 = std::sin(fTemp12);
			float fTemp14 = std::cos(fTemp12);
			float fTemp15 = fSlow23 * fTemp1 * fTemp14 - fTemp13 * fRec15[1];
			float fTemp16 = fTemp14 * fTemp15 - fTemp13 * fRec16[1];
			float fTemp17 = fTemp14 * fTemp16 - fTemp13 * fRec17[1];
			float fTemp18 = fTemp14 * fTemp17 - fTemp13 * fRec18[1];
			float fTemp19 = fTemp14 * fTemp18 - fTemp13 * fRec19[1];
			fRec20[0] = fTemp14 * fTemp19 - fTemp13 * fRec20[1];
			fRec19[0] = fTemp13 * fTemp19 + fTemp14 * fRec20[1];
			fRec18[0] = fTemp13 * fTemp18 + fTemp14 * fRec19[1];
			fRec17[0] = fTemp13 * fTemp17 + fTemp14 * fRec18[1];
			fRec16[0] = fTemp13 * fTemp16 + fTemp14 * fRec17[1];
			fRec15[0] = fTemp13 * fTemp15 + fTemp14 * fRec16[1];
			float fTemp20 = fSlow12 * fRec1[0] + fSlow22 * (fSlow23 * fTemp1 * fTemp4 + fRec9[1] * fTemp5) + fSlow31 * (fRec5[0] * (fSlow23 * fTemp1 * fTemp13 + fRec15[1] * fTemp14) + fSlow23 * (1.0f - fRec5[0]) * fTemp1) + fSlow35 * fTemp0;
			fVec2[IOTA0 & 4095] = fTemp20;
			fRec0[IOTA0 & 4095] = fSlow6 * (fSlow7 * (fSlow8 * (fSlow10 * fVec2[(IOTA0 - iSlow36) & 4095] - fSlow38 * fVec2[(IOTA0 - iSlow39) & 4095]) + fSlow41 * fVec2[(IOTA0 - iSlow42) & 4095]) - fSlow44 * fVec2[(IOTA0 - iSlow45) & 4095]) + fSlow46 * fVec2[(IOTA0 - iSlow47) & 4095];
			output0[i0] = FAUSTFLOAT(fSlow1 * fRec0[IOTA0 & 4095]);
			output1[i0] = FAUSTFLOAT(fSlow0 * fRec0[(IOTA0 - iSlow48) & 4095]);
			IOTA0 = IOTA0 + 1;
			iRec3[1] = iRec3[0];
			fVec0[1] = fVec0[0];
			fRec4[1] = fRec4[0];
			fRec1[1] = fRec1[0];
			fRec5[1] = fRec5[0];
			fRec8[1] = fRec8[0];
			fRec7[1] = fRec7[0];
			fRec14[1] = fRec14[0];
			fRec13[1] = fRec13[0];
			fRec12[1] = fRec12[0];
			fRec11[1] = fRec11[0];
			fRec10[1] = fRec10[0];
			fRec9[1] = fRec9[0];
			fVec1[1] = fVec1[0];
			fRec20[1] = fRec20[0];
			fRec19[1] = fRec19[0];
			fRec18[1] = fRec18[0];
			fRec17[1] = fRec17[0];
			fRec16[1] = fRec16[0];
			fRec15[1] = fRec15[0];
		}
	}

};


#include "Faust_plugins_template2.cpp"


#endif
