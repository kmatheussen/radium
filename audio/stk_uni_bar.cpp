/* ------------------------------------------------------------
author: "Romain Michon"
copyright: "Romain Michon (rmichon@ccrma.stanford.edu)"
name: "UniBar"
version: "1.0"
Code generated with Faust 2.81.2 (https://faust.grame.fr)
Compilation options: -a ../../Faust_plugins_template.cpp -lang cpp -nvi -ct 1 -cn Uni_Bar_dsp -es 1 -mcd 16 -mdd 1024 -mdy 33 -single -ftz 0
------------------------------------------------------------ */

#ifndef  __Uni_Bar_dsp_H__
#define  __Uni_Bar_dsp_H__


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
#define FAUSTCLASS Uni_Bar_dsp
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

static float Uni_Bar_dsp_faustpower2_f(float value) {
	return value * value;
}

class Uni_Bar_dsp final : public dsp {
	
 private:
	
	FAUSTFLOAT fHslider0;
	FAUSTFLOAT fButton0;
	int iRec0[2];
	int fSampleRate;
	float fConst0;
	FAUSTFLOAT fHslider1;
	FAUSTFLOAT fHslider2;
	float fRec1[2];
	float fConst1;
	float fConst2;
	float fConst3;
	FAUSTFLOAT fEntry0;
	int iRec8[2];
	float fConst4;
	float fConst5;
	float fConst6;
	float fRec9[2];
	FAUSTFLOAT fEntry1;
	float fRec10[2];
	FAUSTFLOAT fHslider3;
	FAUSTFLOAT fHslider4;
	FAUSTFLOAT fHslider5;
	int IOTA0;
	float fVec0[4096];
	FAUSTFLOAT fEntry2;
	float fConst7;
	float fConst8;
	float fRec7[3];
	float fRec6[2];
	float fRec2[2];
	float fVec1[4096];
	float fConst9;
	float fConst10;
	float fRec12[3];
	float fRec11[2];
	float fRec3[2];
	float fVec2[2048];
	float fConst11;
	float fConst12;
	float fRec14[3];
	float fRec13[2];
	float fRec4[2];
	float fVec3[2048];
	float fConst13;
	float fConst14;
	float fRec16[3];
	float fRec15[2];
	float fRec5[2];
	float fVec4[4096];
	float fConst15;
	FAUSTFLOAT fHslider6;
	
 public:
	Uni_Bar_dsp() {
	}
	
	void metadata(Meta* m) { 
		m->declare("author", "Romain Michon");
		m->declare("compile_options", "-a ../../Faust_plugins_template.cpp -lang cpp -nvi -ct 1 -cn Uni_Bar_dsp -es 1 -mcd 16 -mdd 1024 -mdy 33 -single -ftz 0");
		m->declare("copyright", "Romain Michon (rmichon@ccrma.stanford.edu)");
		m->declare("description", "Nonlinear Banded Waveguide Models");
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
		m->declare("filename", "uniBar.dsp");
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
		m->declare("name", "UniBar");
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
	}
	
	void instanceConstants(int sample_rate) {
		fSampleRate = sample_rate;
		fConst0 = std::min<float>(1.92e+05f, std::max<float>(1.0f, float(fSampleRate)));
		fConst1 = 100.53097f / fConst0;
		fConst2 = Uni_Bar_dsp_faustpower2_f(1.0f - fConst1);
		fConst3 = 0.5f * (1.0f - fConst2);
		fConst4 = 5e+01f / fConst0;
		fConst5 = 1.0f - std::pow(9e+01f, 2e+02f / fConst0);
		fConst6 = 1.0f - 1.0f / std::pow(9e+04f, 1e+02f / fConst0);
		fConst7 = 2.0f * (fConst1 - 1.0f);
		fConst8 = 6.2831855f / fConst0;
		fConst9 = 0.3628447f * fConst0;
		fConst10 = 17.31646f / fConst0;
		fConst11 = 0.18504812f * fConst0;
		fConst12 = 33.954334f / fConst0;
		fConst13 = 0.111944474f * fConst0;
		fConst14 = 56.127693f / fConst0;
		fConst15 = 0.5f * fConst0;
	}
	
	void instanceResetUserInterface() {
		fHslider0 = FAUSTFLOAT(0.6f);
		fButton0 = FAUSTFLOAT(0.0f);
		fHslider1 = FAUSTFLOAT(0.01f);
		fHslider2 = FAUSTFLOAT(0.05f);
		fEntry0 = FAUSTFLOAT(0.0f);
		fEntry1 = FAUSTFLOAT(0.8f);
		fHslider3 = FAUSTFLOAT(1.0f);
		fHslider4 = FAUSTFLOAT(0.0f);
		fHslider5 = FAUSTFLOAT(0.2f);
		fEntry2 = FAUSTFLOAT(4.4e+02f);
		fHslider6 = FAUSTFLOAT(0.5f);
	}
	
	void instanceClear() {
		for (int l0 = 0; l0 < 2; l0 = l0 + 1) {
			iRec0[l0] = 0;
		}
		for (int l1 = 0; l1 < 2; l1 = l1 + 1) {
			fRec1[l1] = 0.0f;
		}
		for (int l2 = 0; l2 < 2; l2 = l2 + 1) {
			iRec8[l2] = 0;
		}
		for (int l3 = 0; l3 < 2; l3 = l3 + 1) {
			fRec9[l3] = 0.0f;
		}
		for (int l4 = 0; l4 < 2; l4 = l4 + 1) {
			fRec10[l4] = 0.0f;
		}
		IOTA0 = 0;
		for (int l5 = 0; l5 < 4096; l5 = l5 + 1) {
			fVec0[l5] = 0.0f;
		}
		for (int l6 = 0; l6 < 3; l6 = l6 + 1) {
			fRec7[l6] = 0.0f;
		}
		for (int l7 = 0; l7 < 2; l7 = l7 + 1) {
			fRec6[l7] = 0.0f;
		}
		for (int l8 = 0; l8 < 2; l8 = l8 + 1) {
			fRec2[l8] = 0.0f;
		}
		for (int l9 = 0; l9 < 4096; l9 = l9 + 1) {
			fVec1[l9] = 0.0f;
		}
		for (int l10 = 0; l10 < 3; l10 = l10 + 1) {
			fRec12[l10] = 0.0f;
		}
		for (int l11 = 0; l11 < 2; l11 = l11 + 1) {
			fRec11[l11] = 0.0f;
		}
		for (int l12 = 0; l12 < 2; l12 = l12 + 1) {
			fRec3[l12] = 0.0f;
		}
		for (int l13 = 0; l13 < 2048; l13 = l13 + 1) {
			fVec2[l13] = 0.0f;
		}
		for (int l14 = 0; l14 < 3; l14 = l14 + 1) {
			fRec14[l14] = 0.0f;
		}
		for (int l15 = 0; l15 < 2; l15 = l15 + 1) {
			fRec13[l15] = 0.0f;
		}
		for (int l16 = 0; l16 < 2; l16 = l16 + 1) {
			fRec4[l16] = 0.0f;
		}
		for (int l17 = 0; l17 < 2048; l17 = l17 + 1) {
			fVec3[l17] = 0.0f;
		}
		for (int l18 = 0; l18 < 3; l18 = l18 + 1) {
			fRec16[l18] = 0.0f;
		}
		for (int l19 = 0; l19 < 2; l19 = l19 + 1) {
			fRec15[l19] = 0.0f;
		}
		for (int l20 = 0; l20 < 2; l20 = l20 + 1) {
			fRec5[l20] = 0.0f;
		}
		for (int l21 = 0; l21 < 4096; l21 = l21 + 1) {
			fVec4[l21] = 0.0f;
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
	
	Uni_Bar_dsp* clone() {
		return new Uni_Bar_dsp();
	}
	
	int getSampleRate() {
		return fSampleRate;
	}
	
	void buildUserInterface(UI* ui_interface) {
		ui_interface->openVerticalBox("UniBar");
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
		ui_interface->openVerticalBox("Global_Envelope_Parameters");
		ui_interface->declare(&fHslider1, "3", "");
		ui_interface->declare(&fHslider1, "tooltip", "Global envelope attack duration");
		ui_interface->declare(&fHslider1, "unit", "s");
		ui_interface->addHorizontalSlider("Glob_Env_Attack", &fHslider1, FAUSTFLOAT(0.01f), FAUSTFLOAT(0.0f), FAUSTFLOAT(2.0f), FAUSTFLOAT(0.01f));
		ui_interface->declare(&fHslider2, "3", "");
		ui_interface->declare(&fHslider2, "tooltip", "Global envelope release duration");
		ui_interface->declare(&fHslider2, "unit", "s");
		ui_interface->addHorizontalSlider("Glob_Env_Release", &fHslider2, FAUSTFLOAT(0.05f), FAUSTFLOAT(0.0f), FAUSTFLOAT(2.0f), FAUSTFLOAT(0.01f));
		ui_interface->closeBox();
		ui_interface->closeBox();
		ui_interface->openHorizontalBox("Physical_and_Nonlinearity");
		ui_interface->openVerticalBox("Physical_Parameters");
		ui_interface->declare(&fHslider3, "2", "");
		ui_interface->declare(&fHslider3, "tooltip", "A value between 0 and 1");
		ui_interface->addHorizontalSlider("Base_Gain", &fHslider3, FAUSTFLOAT(1.0f), FAUSTFLOAT(0.0f), FAUSTFLOAT(1.0f), FAUSTFLOAT(0.01f));
		ui_interface->declare(&fHslider5, "2", "");
		ui_interface->declare(&fHslider5, "tooltip", "Bow pressure on the instrument (Value between 0 and 1)");
		ui_interface->addHorizontalSlider("Bow_Pressure", &fHslider5, FAUSTFLOAT(0.2f), FAUSTFLOAT(0.0f), FAUSTFLOAT(1.0f), FAUSTFLOAT(0.01f));
		ui_interface->declare(&fEntry0, "2", "");
		ui_interface->declare(&fEntry0, "tooltip", "0=Bow; 1=Strike");
		ui_interface->addNumEntry("Excitation_Selector", &fEntry0, FAUSTFLOAT(0.0f), FAUSTFLOAT(0.0f), FAUSTFLOAT(1.0f), FAUSTFLOAT(1.0f));
		ui_interface->declare(&fHslider4, "2", "");
		ui_interface->declare(&fHslider4, "tooltip", "A value between 0 and 1");
		ui_interface->addHorizontalSlider("Integration_Constant", &fHslider4, FAUSTFLOAT(0.0f), FAUSTFLOAT(0.0f), FAUSTFLOAT(1.0f), FAUSTFLOAT(0.01f));
		ui_interface->closeBox();
		ui_interface->closeBox();
		ui_interface->openVerticalBox("Spat");
		ui_interface->addHorizontalSlider("pan angle", &fHslider0, FAUSTFLOAT(0.6f), FAUSTFLOAT(0.0f), FAUSTFLOAT(1.0f), FAUSTFLOAT(0.01f));
		ui_interface->addHorizontalSlider("spatial width", &fHslider6, FAUSTFLOAT(0.5f), FAUSTFLOAT(0.0f), FAUSTFLOAT(1.0f), FAUSTFLOAT(0.01f));
		ui_interface->closeBox();
		ui_interface->closeBox();
	}
	
	void compute(int count, FAUSTFLOAT** RESTRICT inputs, FAUSTFLOAT** RESTRICT outputs) {
		FAUSTFLOAT* output0 = outputs[0];
		FAUSTFLOAT* output1 = outputs[1];
		float fSlow0 = float(fHslider0);
		float fSlow1 = 7.0f * (1.0f - fSlow0);
		float fSlow2 = float(fButton0);
		int iSlow3 = fSlow2 > 0.0f;
		float fSlow4 = float(fHslider1);
		float fSlow5 = 1.0f / (fConst0 * fSlow4 + float(fSlow4 == 0.0f));
		float fSlow6 = float(fHslider2);
		float fSlow7 = 1.0f - 1.0f / std::pow(1e+05f, 1.0f / (fConst0 * fSlow6 + float(fSlow6 == 0.0f)));
		int iSlow8 = fSlow2 <= 0.0f;
		float fSlow9 = float(fEntry0);
		float fSlow10 = fSlow9 + -1.0f;
		float fSlow11 = 0.001f * float(fEntry1);
		float fSlow12 = 0.1f * float(fHslider3) + 0.9f;
		float fSlow13 = float(fHslider4);
		float fSlow14 = 1e+01f - 9.0f * float(fHslider5);
		float fSlow15 = float(fEntry2);
		int iSlow16 = int(fConst0 / fSlow15) & 4095;
		float fSlow17 = fConst7 * std::cos(fConst8 * fSlow15);
		int iSlow18 = int(fConst9 / fSlow15) & 4095;
		float fSlow19 = fConst7 * std::cos(fConst10 * fSlow15);
		int iSlow20 = int(fConst11 / fSlow15) & 4095;
		float fSlow21 = fConst7 * std::cos(fConst12 * fSlow15);
		int iSlow22 = int(fConst13 / fSlow15) & 4095;
		float fSlow23 = fConst7 * std::cos(fConst14 * fSlow15);
		float fSlow24 = 7.0f * fSlow0;
		int iSlow25 = int(fConst15 * (float(fHslider6) / fSlow15)) & 4095;
		for (int i0 = 0; i0 < count; i0 = i0 + 1) {
			iRec0[0] = iSlow3 & (iRec0[1] | (fRec1[1] >= 1.0f));
			int iTemp0 = iSlow8 & (fRec1[1] > 0.0f);
			fRec1[0] = (fSlow5 * float(((iRec0[1] == 0) & iSlow3) & (fRec1[1] < 1.0f)) + fRec1[1] * (1.0f - fSlow7 * float(iTemp0))) * float((iTemp0 == 0) | (fRec1[1] >= 1e-06f));
			iRec8[0] = iSlow3 & (iRec8[1] | (fRec9[1] >= 1.0f));
			int iTemp1 = iSlow8 & (fRec9[1] > 0.0f);
			fRec9[0] = (fConst4 * float(((iRec8[1] == 0) & iSlow3) & (fRec9[1] < 1.0f)) + fRec9[1] * (1.0f - fConst5 * float(iRec8[1] & (fRec9[1] > 9e+01f)) - fConst6 * float(iTemp1))) * float((iTemp1 == 0) | (fRec9[1] >= 1e-06f));
			fRec10[0] = fSlow11 + 0.999f * fRec10[1];
			float fTemp2 = fRec9[0] * (0.1f * fRec10[0] + 0.03f) - fSlow12 * (fRec2[1] + fRec4[1] + fRec3[1] + fRec5[1]) - fSlow13;
			float fTemp3 = std::pow(std::fabs(fSlow14 * fTemp2) + 0.75f, -4.0f);
			float fTemp4 = 0.25f * (fSlow10 * fTemp2 * (float(fTemp3 > 1.0f) + fTemp3 * float(fTemp3 <= 1.0f)) - fSlow9 * fRec10[0]);
			fVec0[IOTA0 & 4095] = fRec6[1] - fTemp4;
			fRec7[0] = 0.9f * fVec0[(IOTA0 - iSlow16) & 4095] - (fSlow17 * fRec7[1] + fConst2 * fRec7[2]);
			fRec6[0] = fConst3 * (fRec7[0] - fRec7[2]);
			fRec2[0] = fRec6[0];
			fVec1[IOTA0 & 4095] = fRec11[1] - fTemp4;
			fRec12[0] = 0.81f * fVec1[(IOTA0 - iSlow18) & 4095] - (fSlow19 * fRec12[1] + fConst2 * fRec12[2]);
			fRec11[0] = fConst3 * (fRec12[0] - fRec12[2]);
			fRec3[0] = fRec11[0];
			fVec2[IOTA0 & 2047] = fRec13[1] - fTemp4;
			fRec14[0] = 0.729f * fVec2[(IOTA0 - iSlow20) & 2047] - (fSlow21 * fRec14[1] + fConst2 * fRec14[2]);
			fRec13[0] = fConst3 * (fRec14[0] - fRec14[2]);
			fRec4[0] = fRec13[0];
			fVec3[IOTA0 & 2047] = fRec15[1] - fTemp4;
			fRec16[0] = 0.6561f * fVec3[(IOTA0 - iSlow22) & 2047] - (fSlow23 * fRec16[1] + fConst2 * fRec16[2]);
			fRec15[0] = fConst3 * (fRec16[0] - fRec16[2]);
			fRec5[0] = fRec15[0];
			float fTemp5 = fRec1[0] * (fRec5[0] + fRec2[0] + fRec4[0] + fRec3[0]);
			fVec4[IOTA0 & 4095] = fTemp5;
			output0[i0] = FAUSTFLOAT(fSlow1 * fTemp5);
			output1[i0] = FAUSTFLOAT(fSlow24 * fVec4[(IOTA0 - iSlow25) & 4095]);
			iRec0[1] = iRec0[0];
			fRec1[1] = fRec1[0];
			iRec8[1] = iRec8[0];
			fRec9[1] = fRec9[0];
			fRec10[1] = fRec10[0];
			IOTA0 = IOTA0 + 1;
			fRec7[2] = fRec7[1];
			fRec7[1] = fRec7[0];
			fRec6[1] = fRec6[0];
			fRec2[1] = fRec2[0];
			fRec12[2] = fRec12[1];
			fRec12[1] = fRec12[0];
			fRec11[1] = fRec11[0];
			fRec3[1] = fRec3[0];
			fRec14[2] = fRec14[1];
			fRec14[1] = fRec14[0];
			fRec13[1] = fRec13[0];
			fRec4[1] = fRec4[0];
			fRec16[2] = fRec16[1];
			fRec16[1] = fRec16[0];
			fRec15[1] = fRec15[0];
			fRec5[1] = fRec5[0];
		}
	}

};


#include "Faust_plugins_template2.cpp"


#endif
