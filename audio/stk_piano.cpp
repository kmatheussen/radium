/* ------------------------------------------------------------
author: "Romain Michon (rmichon@ccrma.stanford.edu)"
copyright: "Romain Michon"
name: "Commuted Piano"
version: "1.0"
Code generated with Faust 2.81.2 (https://faust.grame.fr)
Compilation options: -a ../../Faust_plugins_template.cpp -lang cpp -nvi -ct 1 -cn Piano_dsp -es 1 -mcd 16 -mdd 1024 -mdy 33 -single -ftz 0
------------------------------------------------------------ */

#ifndef  __Piano_dsp_H__
#define  __Piano_dsp_H__


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
#include <math.h>
#include <piano.h>

#ifndef FAUSTCLASS 
#define FAUSTCLASS Piano_dsp
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

static float Piano_dsp_faustpower2_f(float value) {
	return value * value;
}

class Piano_dsp final : public dsp {
	
 private:
	
	FAUSTFLOAT fHslider0;
	int fSampleRate;
	float fConst0;
	float fConst1;
	FAUSTFLOAT fEntry0;
	float fConst2;
	FAUSTFLOAT fHslider1;
	float fConst3;
	FAUSTFLOAT fHslider2;
	FAUSTFLOAT fButton0;
	float fRec7[2];
	FAUSTFLOAT fHslider3;
	int iRec13[2];
	float fRec15[2];
	float fConst4;
	FAUSTFLOAT fEntry1;
	float fRec14[2];
	float fConst5;
	FAUSTFLOAT fHslider4;
	float fConst6;
	float fConst7;
	float fConst8;
	float fRec16[2];
	float fRec12[2];
	float fRec11[2];
	float fRec10[2];
	float fRec9[2];
	float fRec8[2];
	float fVec0[2];
	float fRec6[2];
	float fRec5[2];
	int IOTA0;
	float fRec4[4096];
	float fVec1[2];
	float fRec20[2];
	float fRec19[2];
	float fRec18[4096];
	float fVec2[2];
	float fRec17[2];
	float fRec1[2];
	float fRec2[2];
	float fConst9;
	float fRec0[3];
	float fVec3[2];
	float fVec4[2];
	float fRec30[2];
	float fRec29[2];
	float fRec28[2];
	float fRec27[2];
	float fRec26[2];
	float fConst10;
	float fRec25[3];
	float fRec24[3];
	float fRec23[3];
	float fRec22[3];
	float fRec21[2];
	float fVec5[4096];
	float fConst11;
	FAUSTFLOAT fHslider5;
	
 public:
	Piano_dsp() {
	}
	
	void metadata(Meta* m) { 
		m->declare("author", "Romain Michon (rmichon@ccrma.stanford.edu)");
		m->declare("compile_options", "-a ../../Faust_plugins_template.cpp -lang cpp -nvi -ct 1 -cn Piano_dsp -es 1 -mcd 16 -mdd 1024 -mdy 33 -single -ftz 0");
		m->declare("copyright", "Romain Michon");
		m->declare("description", "WaveGuide Commuted Piano");
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
		m->declare("filename", "piano.dsp");
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
		m->declare("name", "Commuted Piano");
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
		fConst1 = 1.0f / Piano_dsp_faustpower2_f(fConst0);
		fConst2 = 0.15915494f * fConst0;
		fConst3 = 6.2831855f / fConst0;
		fConst4 = 7.0f / fConst0;
		fConst5 = 1e+01f / fConst0;
		fConst6 = 0.1f * fConst0;
		fConst7 = std::exp(-(0.5f / fConst0));
		fConst8 = std::exp(-(5.0f / fConst0));
		fConst9 = 1.0f / fConst0;
		fConst10 = 0.05f / fConst0;
		fConst11 = 0.5f * fConst0;
	}
	
	void instanceResetUserInterface() {
		fHslider0 = FAUSTFLOAT(0.6f);
		fEntry0 = FAUSTFLOAT(4.4e+02f);
		fHslider1 = FAUSTFLOAT(0.28f);
		fHslider2 = FAUSTFLOAT(0.1f);
		fButton0 = FAUSTFLOAT(0.0f);
		fHslider3 = FAUSTFLOAT(0.0f);
		fEntry1 = FAUSTFLOAT(1.0f);
		fHslider4 = FAUSTFLOAT(0.1f);
		fHslider5 = FAUSTFLOAT(0.5f);
	}
	
	void instanceClear() {
		for (int l0 = 0; l0 < 2; l0 = l0 + 1) {
			fRec7[l0] = 0.0f;
		}
		for (int l1 = 0; l1 < 2; l1 = l1 + 1) {
			iRec13[l1] = 0;
		}
		for (int l2 = 0; l2 < 2; l2 = l2 + 1) {
			fRec15[l2] = 0.0f;
		}
		for (int l3 = 0; l3 < 2; l3 = l3 + 1) {
			fRec14[l3] = 0.0f;
		}
		for (int l4 = 0; l4 < 2; l4 = l4 + 1) {
			fRec16[l4] = 0.0f;
		}
		for (int l5 = 0; l5 < 2; l5 = l5 + 1) {
			fRec12[l5] = 0.0f;
		}
		for (int l6 = 0; l6 < 2; l6 = l6 + 1) {
			fRec11[l6] = 0.0f;
		}
		for (int l7 = 0; l7 < 2; l7 = l7 + 1) {
			fRec10[l7] = 0.0f;
		}
		for (int l8 = 0; l8 < 2; l8 = l8 + 1) {
			fRec9[l8] = 0.0f;
		}
		for (int l9 = 0; l9 < 2; l9 = l9 + 1) {
			fRec8[l9] = 0.0f;
		}
		for (int l10 = 0; l10 < 2; l10 = l10 + 1) {
			fVec0[l10] = 0.0f;
		}
		for (int l11 = 0; l11 < 2; l11 = l11 + 1) {
			fRec6[l11] = 0.0f;
		}
		for (int l12 = 0; l12 < 2; l12 = l12 + 1) {
			fRec5[l12] = 0.0f;
		}
		IOTA0 = 0;
		for (int l13 = 0; l13 < 4096; l13 = l13 + 1) {
			fRec4[l13] = 0.0f;
		}
		for (int l14 = 0; l14 < 2; l14 = l14 + 1) {
			fVec1[l14] = 0.0f;
		}
		for (int l15 = 0; l15 < 2; l15 = l15 + 1) {
			fRec20[l15] = 0.0f;
		}
		for (int l16 = 0; l16 < 2; l16 = l16 + 1) {
			fRec19[l16] = 0.0f;
		}
		for (int l17 = 0; l17 < 4096; l17 = l17 + 1) {
			fRec18[l17] = 0.0f;
		}
		for (int l18 = 0; l18 < 2; l18 = l18 + 1) {
			fVec2[l18] = 0.0f;
		}
		for (int l19 = 0; l19 < 2; l19 = l19 + 1) {
			fRec17[l19] = 0.0f;
		}
		for (int l20 = 0; l20 < 2; l20 = l20 + 1) {
			fRec1[l20] = 0.0f;
		}
		for (int l21 = 0; l21 < 2; l21 = l21 + 1) {
			fRec2[l21] = 0.0f;
		}
		for (int l22 = 0; l22 < 3; l22 = l22 + 1) {
			fRec0[l22] = 0.0f;
		}
		for (int l23 = 0; l23 < 2; l23 = l23 + 1) {
			fVec3[l23] = 0.0f;
		}
		for (int l24 = 0; l24 < 2; l24 = l24 + 1) {
			fVec4[l24] = 0.0f;
		}
		for (int l25 = 0; l25 < 2; l25 = l25 + 1) {
			fRec30[l25] = 0.0f;
		}
		for (int l26 = 0; l26 < 2; l26 = l26 + 1) {
			fRec29[l26] = 0.0f;
		}
		for (int l27 = 0; l27 < 2; l27 = l27 + 1) {
			fRec28[l27] = 0.0f;
		}
		for (int l28 = 0; l28 < 2; l28 = l28 + 1) {
			fRec27[l28] = 0.0f;
		}
		for (int l29 = 0; l29 < 2; l29 = l29 + 1) {
			fRec26[l29] = 0.0f;
		}
		for (int l30 = 0; l30 < 3; l30 = l30 + 1) {
			fRec25[l30] = 0.0f;
		}
		for (int l31 = 0; l31 < 3; l31 = l31 + 1) {
			fRec24[l31] = 0.0f;
		}
		for (int l32 = 0; l32 < 3; l32 = l32 + 1) {
			fRec23[l32] = 0.0f;
		}
		for (int l33 = 0; l33 < 3; l33 = l33 + 1) {
			fRec22[l33] = 0.0f;
		}
		for (int l34 = 0; l34 < 2; l34 = l34 + 1) {
			fRec21[l34] = 0.0f;
		}
		for (int l35 = 0; l35 < 4096; l35 = l35 + 1) {
			fVec5[l35] = 0.0f;
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
	
	Piano_dsp* clone() {
		return new Piano_dsp();
	}
	
	int getSampleRate() {
		return fSampleRate;
	}
	
	void buildUserInterface(UI* ui_interface) {
		ui_interface->openVerticalBox("Commuted Piano");
		ui_interface->openHorizontalBox("Basic_Parameters");
		ui_interface->declare(&fEntry0, "1", "");
		ui_interface->declare(&fEntry0, "tooltip", "Tone frequency");
		ui_interface->declare(&fEntry0, "unit", "Hz");
		ui_interface->addNumEntry("freq", &fEntry0, FAUSTFLOAT(4.4e+02f), FAUSTFLOAT(2e+01f), FAUSTFLOAT(2e+04f), FAUSTFLOAT(1.0f));
		ui_interface->declare(&fEntry1, "1", "");
		ui_interface->declare(&fEntry1, "tooltip", "Gain (value between 0 and 1)");
		ui_interface->addNumEntry("gain", &fEntry1, FAUSTFLOAT(1.0f), FAUSTFLOAT(0.0f), FAUSTFLOAT(1.0f), FAUSTFLOAT(0.01f));
		ui_interface->declare(&fButton0, "1", "");
		ui_interface->declare(&fButton0, "tooltip", "noteOn = 1, noteOff = 0");
		ui_interface->addButton("gate", &fButton0);
		ui_interface->closeBox();
		ui_interface->openVerticalBox("Physical_Parameters");
		ui_interface->declare(&fHslider3, "2", "");
		ui_interface->declare(&fHslider3, "tooltip", "A value between 0 and 1");
		ui_interface->addHorizontalSlider("Brightness_Factor", &fHslider3, FAUSTFLOAT(0.0f), FAUSTFLOAT(0.0f), FAUSTFLOAT(1.0f), FAUSTFLOAT(0.01f));
		ui_interface->declare(&fHslider2, "2", "");
		ui_interface->declare(&fHslider2, "tooltip", "A value between 0 and 1");
		ui_interface->addHorizontalSlider("Detuning_Factor", &fHslider2, FAUSTFLOAT(0.1f), FAUSTFLOAT(0.0f), FAUSTFLOAT(1.0f), FAUSTFLOAT(0.01f));
		ui_interface->declare(&fHslider4, "2", "");
		ui_interface->declare(&fHslider4, "tooltip", "A value between 0 and 1");
		ui_interface->addHorizontalSlider("Hammer_Hardness", &fHslider4, FAUSTFLOAT(0.1f), FAUSTFLOAT(0.0f), FAUSTFLOAT(1.0f), FAUSTFLOAT(0.01f));
		ui_interface->declare(&fHslider1, "2", "");
		ui_interface->declare(&fHslider1, "tooltip", "A value between 0 and 1");
		ui_interface->addHorizontalSlider("Stiffness_Factor", &fHslider1, FAUSTFLOAT(0.28f), FAUSTFLOAT(0.0f), FAUSTFLOAT(1.0f), FAUSTFLOAT(0.01f));
		ui_interface->closeBox();
		ui_interface->openVerticalBox("Spat");
		ui_interface->addHorizontalSlider("pan angle", &fHslider0, FAUSTFLOAT(0.6f), FAUSTFLOAT(0.0f), FAUSTFLOAT(1.0f), FAUSTFLOAT(0.01f));
		ui_interface->addHorizontalSlider("spatial width", &fHslider5, FAUSTFLOAT(0.5f), FAUSTFLOAT(0.0f), FAUSTFLOAT(1.0f), FAUSTFLOAT(0.01f));
		ui_interface->closeBox();
		ui_interface->closeBox();
	}
	
	void compute(int count, FAUSTFLOAT** RESTRICT inputs, FAUSTFLOAT** RESTRICT outputs) {
		FAUSTFLOAT* output0 = outputs[0];
		FAUSTFLOAT* output1 = outputs[1];
		float fSlow0 = float(fHslider0);
		float fSlow1 = 12.0f * (1.0f - fSlow0);
		float fSlow2 = float(fEntry0);
		int iSlow3 = int(17.31234f * (std::log(fSlow2) + -6.086775f) + 69.5f);
		float fSlow4 = float(iSlow3);
		float fSlow5 = getValueEQBandWidthFactor(fSlow4);
		float fSlow6 = 0.5f * (1.0f - fConst1 * Piano_dsp_faustpower2_f(fSlow2) * Piano_dsp_faustpower2_f(fSlow5));
		float fSlow7 = getValueEQGain(fSlow4);
		float fSlow8 = float(fHslider1);
		float fSlow9 = getValueStiffnessCoefficient(fSlow4);
		float fSlow10 = 13.69f * Piano_dsp_faustpower2_f(fSlow8) * Piano_dsp_faustpower2_f(fSlow9);
		float fSlow11 = fSlow10 + -1.0f;
		float fSlow12 = 5.0f * float(fHslider2) * getValueDetuningHz(fSlow4);
		float fSlow13 = fSlow2 + fSlow12;
		float fSlow14 = fConst3 * fSlow13;
		float fSlow15 = std::sin(fSlow14);
		float fSlow16 = fSlow8 * fSlow9;
		float fSlow17 = 7.4f * fSlow16;
		float fSlow18 = fSlow10 + 1.0f;
		float fSlow19 = std::cos(fSlow14);
		float fSlow20 = 3.0f * std::atan2(fSlow11 * fSlow15, fSlow17 + fSlow18 * fSlow19);
		float fSlow21 = getValueSingleStringPole(fSlow4);
		float fSlow22 = std::pow(1e+01f, 0.05f * (getValueSingleStringDecayRate(fSlow4) / fSlow2)) * (1.0f - fSlow21);
		float fSlow23 = getValueSingleStringZero(fSlow4);
		float fSlow24 = fSlow22 * fSlow23;
		float fSlow25 = 1.0f - fSlow23;
		float fSlow26 = fSlow21 * fSlow25;
		float fSlow27 = 3.0f * fSlow26;
		float fSlow28 = fSlow24 - fSlow27;
		float fSlow29 = fSlow26 - fSlow24;
		float fSlow30 = 4.0f * fSlow29;
		float fSlow31 = fSlow28 + fSlow30;
		float fSlow32 = 3.0f * fSlow25 - fSlow22;
		float fSlow33 = fSlow28 * fSlow19 / fSlow32 + 1.0f;
		float fSlow34 = fSlow22 + fSlow23 + -1.0f;
		float fSlow35 = 4.0f * fSlow34;
		float fSlow36 = (fSlow35 + fSlow31 * fSlow19) / fSlow32 + 1.0f;
		float fSlow37 = Piano_dsp_faustpower2_f(fSlow15);
		float fSlow38 = Piano_dsp_faustpower2_f(fSlow32);
		float fSlow39 = fConst2 * ((fSlow20 + std::atan2(-(fSlow15 * (fSlow31 * fSlow33 - fSlow28 * fSlow36) / fSlow32), fSlow33 * fSlow36 + fSlow28 * fSlow31 * fSlow37 / fSlow38) + 6.2831855f) / fSlow13);
		float fSlow40 = float(int(fSlow39));
		float fSlow41 = fSlow39 - fSlow40;
		float fSlow42 = float(fButton0);
		float fSlow43 = fSlow42 + -1.0f;
		float fSlow44 = 0.001f * (0.9996f * fSlow42 - 0.9f * fSlow43 * getValueReleaseLoopGain(fSlow4));
		float fSlow45 = getValueDCBa1(fSlow4);
		float fSlow46 = 1.0f - fSlow45;
		float fSlow47 = 0.25f * float(fHslider3);
		float fSlow48 = getValueLoudPole(fSlow4);
		float fSlow49 = (fSlow47 + (0.98f - fSlow48)) * getValueLoudGain(fSlow4);
		float fSlow50 = 1.3969839e-09f * fSlow49 * float(iSlow3 < 88);
		int iSlow51 = fSlow42 > 0.0f;
		float fSlow52 = std::exp(-(fConst4 / (float(fEntry1) * getValueDryTapAmpT60(fSlow4))));
		int iSlow53 = fSlow42 < 1.0f;
		float fSlow54 = float(fHslider4);
		float fSlow55 = std::exp(-(fConst5 / fSlow54));
		float fSlow56 = fConst6 * fSlow54;
		float fSlow57 = fConst8 * fSlow43;
		float fSlow58 = 0.2f * getValueSustainPedalLevel(fSlow4);
		float fSlow59 = fSlow48 + (0.02f - fSlow47);
		float fSlow60 = fSlow45 - 1.0f;
		float fSlow61 = 3.7f * fSlow16;
		float fSlow62 = fSlow24 + fSlow30 - fSlow27;
		float fSlow63 = (fSlow35 + fSlow62 * fSlow19) / fSlow32 + 1.0f;
		int iSlow64 = int(fConst2 * ((fSlow20 + std::atan2(-(fSlow15 * (fSlow62 * fSlow33 - fSlow28 * fSlow63) / fSlow32), fSlow33 * fSlow63 + fSlow28 * fSlow62 * fSlow37 / fSlow38) + 6.2831855f) / fSlow13));
		int iSlow65 = (iSlow64 + 1) & 4095;
		float fSlow66 = 1.0f / fSlow32;
		float fSlow67 = fSlow2 - fSlow12;
		float fSlow68 = fConst3 * fSlow67;
		float fSlow69 = std::sin(fSlow68);
		float fSlow70 = std::cos(fSlow68);
		float fSlow71 = 3.0f * std::atan2(fSlow11 * fSlow69, fSlow17 + fSlow18 * fSlow70);
		float fSlow72 = fSlow70 * fSlow28 / fSlow32 + 1.0f;
		float fSlow73 = (fSlow70 * fSlow31 + fSlow35) / fSlow32 + 1.0f;
		float fSlow74 = Piano_dsp_faustpower2_f(fSlow69) * fSlow28;
		float fSlow75 = fConst2 * ((fSlow71 + std::atan2(-(fSlow69 * (fSlow31 * fSlow72 - fSlow28 * fSlow73) / fSlow32), fSlow72 * fSlow73 + fSlow74 * fSlow31 / fSlow38) + 6.2831855f) / fSlow67);
		float fSlow76 = float(int(fSlow75));
		float fSlow77 = fSlow76 + (1.0f - fSlow75);
		float fSlow78 = (fSlow35 + fSlow70 * fSlow62) / fSlow32 + 1.0f;
		int iSlow79 = int(fConst2 * ((fSlow71 + std::atan2(-(fSlow69 * (fSlow72 * fSlow62 - fSlow28 * fSlow78) / fSlow32), fSlow72 * fSlow78 + fSlow74 * fSlow62 / fSlow38) + 6.2831855f) / fSlow67));
		int iSlow80 = iSlow79 & 4095;
		float fSlow81 = fSlow75 - fSlow76;
		int iSlow82 = (iSlow79 + 1) & 4095;
		float fSlow83 = fSlow40 + (1.0f - fSlow39);
		int iSlow84 = iSlow64 & 4095;
		float fSlow85 = fConst9 * fSlow2 * fSlow5;
		float fSlow86 = 2.0f * std::cos(fConst3 * (fSlow2 / getValueStrikePosition(fSlow4)));
		float fSlow87 = getValueBq4_gEarBalled(fSlow4);
		float fSlow88 = 2.0f * fSlow87;
		float fSlow89 = float(iSlow3 >= 88);
		float fSlow90 = 2.3283064e-10f * fSlow89;
		float fSlow91 = 1.1641532e-10f * fSlow89;
		float fSlow92 = std::pow(1e+01f, fConst10 * getValuer3db(fSlow4));
		float fSlow93 = 2.0f * std::cos(fConst3 * fSlow2 * getValueThirdPartialFactor(fSlow4));
		float fSlow94 = std::pow(1e+01f, fConst10 * getValuer2db(fSlow4));
		float fSlow95 = 2.0f * std::cos(fConst3 * fSlow2 * getValueSecondPartialFactor(fSlow4));
		float fSlow96 = std::pow(1e+01f, fConst10 * getValuer1_1db(fSlow4));
		float fSlow97 = std::cos(fConst3 * fSlow2);
		float fSlow98 = 2.0f * fSlow97;
		float fSlow99 = std::pow(1e+01f, 0.05f * getValueSecondStageAmpRatio(fSlow4));
		float fSlow100 = 1.0f - fSlow99;
		float fSlow101 = std::pow(1e+01f, fConst10 * getValuer1_2db(fSlow4));
		float fSlow102 = Piano_dsp_faustpower2_f(fSlow96) * fSlow99 + fSlow100 * Piano_dsp_faustpower2_f(fSlow101);
		float fSlow103 = 2.0f * fSlow97 * (fSlow96 * fSlow99 + fSlow100 * fSlow101);
		float fSlow104 = 12.0f * fSlow0;
		int iSlow105 = int(fConst11 * (float(fHslider5) / fSlow2)) & 4095;
		for (int i0 = 0; i0 < count; i0 = i0 + 1) {
			fRec7[0] = fSlow44 + 0.999f * fRec7[1];
			iRec13[0] = 1103515245 * iRec13[1] + 12345;
			fRec15[0] = fSlow42 * fRec15[1] + 1.0f;
			float fTemp0 = fRec15[0] + -1.0f;
			float fTemp1 = float((fTemp0 < 2.0f) & iSlow51);
			float fTemp2 = 0.030197384f * fTemp1 + fSlow52 * float((fTemp0 >= 2.0f) | iSlow53);
			fRec14[0] = fRec14[1] * fTemp2 + 0.15f * fTemp1 * (1.0f - fTemp2);
			int iTemp3 = fTemp0 < fSlow56;
			float fTemp4 = fSlow42 * (fSlow55 * float(iTemp3) + fConst7 * float(fTemp0 >= fSlow56));
			fRec16[0] = fRec16[1] * (fTemp4 - fSlow57) + fSlow58 * (fSlow57 + (1.0f - fTemp4)) * float(iTemp3 & iSlow51);
			float fTemp5 = float(iRec13[0]) * (fRec14[0] + fRec16[0]);
			fRec12[0] = fSlow50 * fTemp5 + fSlow59 * fRec12[1];
			fRec11[0] = fSlow49 * fRec12[0] + fSlow59 * fRec11[1];
			fRec10[0] = fSlow49 * fRec11[0] + fSlow59 * fRec10[1];
			fRec9[0] = fSlow49 * fRec10[0] + fSlow59 * fRec9[1];
			fRec8[0] = 0.5f * (fSlow46 * fRec9[0] + fSlow60 * fRec9[1]) - fSlow45 * fRec8[1];
			float fTemp6 = fRec7[0] * (fRec8[0] + fRec1[1]);
			fVec0[0] = fTemp6;
			fRec6[0] = fVec0[1] + fSlow61 * (fTemp6 - fRec6[1]);
			fRec5[0] = fRec6[1] + fSlow61 * (fRec6[0] - fRec5[1]);
			fRec4[IOTA0 & 4095] = fRec5[1] + fSlow61 * (fRec5[0] - fRec4[(IOTA0 - 1) & 4095]);
			float fTemp7 = fSlow41 * fRec4[(IOTA0 - iSlow65) & 4095];
			float fTemp8 = fRec8[0] + fRec7[0] * fRec2[1];
			fVec1[0] = fTemp8;
			fRec20[0] = fVec1[1] + fSlow61 * (fTemp8 - fRec20[1]);
			fRec19[0] = fRec20[1] + fSlow61 * (fRec20[0] - fRec19[1]);
			fRec18[IOTA0 & 4095] = fRec19[1] + fSlow61 * (fRec19[0] - fRec18[(IOTA0 - 1) & 4095]);
			float fTemp9 = fSlow77 * fRec18[(IOTA0 - iSlow80) & 4095];
			float fTemp10 = fSlow81 * fRec18[(IOTA0 - iSlow82) & 4095];
			float fTemp11 = fSlow83 * fRec4[(IOTA0 - iSlow84) & 4095];
			float fTemp12 = fTemp7 + fTemp9 + fTemp10 + fTemp11;
			fVec2[0] = fTemp12;
			fRec17[0] = fSlow66 * (2.0f * (fSlow34 * fTemp12 + fSlow29 * fVec2[1]) - fSlow28 * fRec17[1]);
			fRec1[0] = fTemp7 + fRec17[0] + fTemp11;
			fRec2[0] = fTemp10 + fRec17[0] + fTemp9;
			float fRec3 = fTemp12;
			fRec0[0] = fSlow7 * fRec3 - fSlow85 * (fSlow85 * fRec0[2] - fSlow86 * fRec0[1]);
			fVec3[0] = fSlow90 * fTemp5;
			float fTemp13 = 0.5f * fVec3[1] + fSlow91 * fTemp5;
			fVec4[0] = fTemp13;
			fRec30[0] = -(0.5f * (fSlow46 * fTemp13 + fSlow60 * fVec4[1]) + fSlow45 * fRec30[1]);
			fRec29[0] = fSlow49 * fRec30[0] + fSlow59 * fRec29[1];
			fRec28[0] = fSlow49 * fRec29[0] + fSlow59 * fRec28[1];
			fRec27[0] = fSlow49 * fRec28[0] + fSlow59 * fRec27[1];
			fRec26[0] = fSlow49 * fRec27[0] + fSlow59 * fRec26[1];
			fRec25[0] = fSlow87 * (fRec26[0] - fRec26[1]) - fSlow92 * (fSlow92 * fRec25[2] - fSlow93 * fRec25[1]);
			fRec24[0] = fSlow88 * fRec25[0] - fSlow94 * (fSlow94 * fRec24[2] - fSlow95 * fRec24[1]);
			fRec23[0] = fRec24[0] - fSlow96 * (fSlow96 * fRec23[2] - fSlow98 * fRec23[1]);
			fRec22[0] = fRec23[0] + fSlow102 * fRec23[2] - (fSlow103 * fRec23[1] + fSlow101 * (fSlow101 * fRec22[2] - fSlow98 * fRec22[1]));
			fRec21[0] = fSlow46 * fRec22[0] - fSlow45 * fRec21[1];
			float fTemp14 = fSlow6 * (fRec0[0] - fRec0[2]) + fRec3 + fRec21[0];
			fVec5[IOTA0 & 4095] = fTemp14;
			output0[i0] = FAUSTFLOAT(fSlow1 * fTemp14);
			output1[i0] = FAUSTFLOAT(fSlow104 * fVec5[(IOTA0 - iSlow105) & 4095]);
			fRec7[1] = fRec7[0];
			iRec13[1] = iRec13[0];
			fRec15[1] = fRec15[0];
			fRec14[1] = fRec14[0];
			fRec16[1] = fRec16[0];
			fRec12[1] = fRec12[0];
			fRec11[1] = fRec11[0];
			fRec10[1] = fRec10[0];
			fRec9[1] = fRec9[0];
			fRec8[1] = fRec8[0];
			fVec0[1] = fVec0[0];
			fRec6[1] = fRec6[0];
			fRec5[1] = fRec5[0];
			IOTA0 = IOTA0 + 1;
			fVec1[1] = fVec1[0];
			fRec20[1] = fRec20[0];
			fRec19[1] = fRec19[0];
			fVec2[1] = fVec2[0];
			fRec17[1] = fRec17[0];
			fRec1[1] = fRec1[0];
			fRec2[1] = fRec2[0];
			fRec0[2] = fRec0[1];
			fRec0[1] = fRec0[0];
			fVec3[1] = fVec3[0];
			fVec4[1] = fVec4[0];
			fRec30[1] = fRec30[0];
			fRec29[1] = fRec29[0];
			fRec28[1] = fRec28[0];
			fRec27[1] = fRec27[0];
			fRec26[1] = fRec26[0];
			fRec25[2] = fRec25[1];
			fRec25[1] = fRec25[0];
			fRec24[2] = fRec24[1];
			fRec24[1] = fRec24[0];
			fRec23[2] = fRec23[1];
			fRec23[1] = fRec23[0];
			fRec22[2] = fRec22[1];
			fRec22[1] = fRec22[0];
			fRec21[1] = fRec21[0];
		}
	}

};


#include "Faust_plugins_template2.cpp"


#endif
