/* ------------------------------------------------------------
author: "Romain Michon (rmichon@ccrma.stanford.edu)"
copyright: "Romain Michon"
name: "Sitar"
version: "1.0"
Code generated with Faust 2.81.2 (https://faust.grame.fr)
Compilation options: -a ../../Faust_plugins_template.cpp -lang cpp -nvi -ct 1 -cn Sitar_dsp -es 1 -mcd 16 -mdd 1024 -mdy 33 -single -ftz 0
------------------------------------------------------------ */

#ifndef  __Sitar_dsp_H__
#define  __Sitar_dsp_H__


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
#define FAUSTCLASS Sitar_dsp
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


class Sitar_dsp final : public dsp {
	
 private:
	
	FAUSTFLOAT fHslider0;
	FAUSTFLOAT fHslider1;
	FAUSTFLOAT fEntry0;
	int IOTA0;
	int fSampleRate;
	float fConst0;
	int iRec2[2];
	float fRec1[2];
	FAUSTFLOAT fEntry1;
	FAUSTFLOAT fButton0;
	int iRec3[2];
	float fConst1;
	float fConst2;
	float fConst3;
	float fRec4[2];
	float fVec0[2];
	float fRec0[8192];
	float fConst4;
	FAUSTFLOAT fHslider2;
	
 public:
	Sitar_dsp() {
	}
	
	void metadata(Meta* m) { 
		m->declare("author", "Romain Michon (rmichon@ccrma.stanford.edu)");
		m->declare("compile_options", "-a ../../Faust_plugins_template.cpp -lang cpp -nvi -ct 1 -cn Sitar_dsp -es 1 -mcd 16 -mdd 1024 -mdy 33 -single -ftz 0");
		m->declare("copyright", "Romain Michon");
		m->declare("description", "WaveGuide Sitar");
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
		m->declare("filename", "sitar.dsp");
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
		m->declare("name", "Sitar");
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
		fConst1 = 1e+03f / fConst0;
		fConst2 = 1.0f - std::pow(0.001f, 25.0f / fConst0);
		fConst3 = 1.0f - 1.0f / std::pow(1.0f, 2.0f / fConst0);
		fConst4 = 0.5f * fConst0;
	}
	
	void instanceResetUserInterface() {
		fHslider0 = FAUSTFLOAT(0.6f);
		fHslider1 = FAUSTFLOAT(0.7f);
		fEntry0 = FAUSTFLOAT(4.4e+02f);
		fEntry1 = FAUSTFLOAT(1.0f);
		fButton0 = FAUSTFLOAT(0.0f);
		fHslider2 = FAUSTFLOAT(0.5f);
	}
	
	void instanceClear() {
		IOTA0 = 0;
		for (int l0 = 0; l0 < 2; l0 = l0 + 1) {
			iRec2[l0] = 0;
		}
		for (int l1 = 0; l1 < 2; l1 = l1 + 1) {
			fRec1[l1] = 0.0f;
		}
		for (int l2 = 0; l2 < 2; l2 = l2 + 1) {
			iRec3[l2] = 0;
		}
		for (int l3 = 0; l3 < 2; l3 = l3 + 1) {
			fRec4[l3] = 0.0f;
		}
		for (int l4 = 0; l4 < 2; l4 = l4 + 1) {
			fVec0[l4] = 0.0f;
		}
		for (int l5 = 0; l5 < 8192; l5 = l5 + 1) {
			fRec0[l5] = 0.0f;
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
	
	Sitar_dsp* clone() {
		return new Sitar_dsp();
	}
	
	int getSampleRate() {
		return fSampleRate;
	}
	
	void buildUserInterface(UI* ui_interface) {
		ui_interface->openVerticalBox("Sitar");
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
		ui_interface->declare(&fHslider1, "2", "");
		ui_interface->declare(&fHslider1, "tooltip", "A value between 0 and 1");
		ui_interface->addHorizontalSlider("Resonance", &fHslider1, FAUSTFLOAT(0.7f), FAUSTFLOAT(0.0f), FAUSTFLOAT(1.0f), FAUSTFLOAT(0.01f));
		ui_interface->closeBox();
		ui_interface->openVerticalBox("Spat");
		ui_interface->addHorizontalSlider("pan angle", &fHslider0, FAUSTFLOAT(0.6f), FAUSTFLOAT(0.0f), FAUSTFLOAT(1.0f), FAUSTFLOAT(0.01f));
		ui_interface->addHorizontalSlider("spatial width", &fHslider2, FAUSTFLOAT(0.5f), FAUSTFLOAT(0.0f), FAUSTFLOAT(1.0f), FAUSTFLOAT(0.01f));
		ui_interface->closeBox();
		ui_interface->closeBox();
	}
	
	void compute(int count, FAUSTFLOAT** RESTRICT inputs, FAUSTFLOAT** RESTRICT outputs) {
		FAUSTFLOAT* output0 = outputs[0];
		FAUSTFLOAT* output1 = outputs[1];
		float fSlow0 = float(fHslider0);
		float fSlow1 = 8.0f * (1.0f - fSlow0);
		float fSlow2 = float(fEntry0);
		float fSlow3 = 0.1f * float(fHslider1) + 5e-07f * fSlow2 + 0.895f;
		float fSlow4 = 0.990099f * fSlow3;
		float fSlow5 = fConst0 / fSlow2;
		float fSlow6 = 4.656613e-11f * float(fEntry1);
		float fSlow7 = float(fButton0);
		int iSlow8 = fSlow7 > 0.0f;
		int iSlow9 = fSlow7 <= 0.0f;
		float fSlow10 = 8.0f * fSlow0;
		int iSlow11 = int(fConst4 * (float(fHslider2) / fSlow2)) & 4095;
		for (int i0 = 0; i0 < count; i0 = i0 + 1) {
			iRec2[0] = 1103515245 * iRec2[1] + 12345;
			float fTemp0 = float(iRec2[0]);
			fRec1[0] = 0.9992f * fRec1[1] + 0.0008f * (2.3283064e-10f * fTemp0 + 1.0f);
			float fTemp1 = fRec0[(IOTA0 - ((int(fSlow5 * fRec1[0]) & 4095) + 1)) & 8191];
			iRec3[0] = iSlow8 & (iRec3[1] | (fRec4[1] >= 1.0f));
			int iTemp2 = fRec4[1] > 0.0f;
			int iTemp3 = iSlow9 & iTemp2;
			fRec4[0] = (fConst1 * float(((iRec3[1] == 0) & iSlow8) & (fRec4[1] < 1.0f)) + fRec4[1] * (1.0f - fConst2 * float(iRec3[1] & iTemp2) - fConst3 * float(iTemp3))) * float((iTemp3 == 0) | (fRec4[1] >= 1e-06f));
			fVec0[0] = fSlow3 * fTemp1;
			fRec0[IOTA0 & 8191] = fSlow4 * fTemp1 + fSlow6 * fRec4[0] * fTemp0 - 0.00990099f * fVec0[1];
			output0[i0] = FAUSTFLOAT(fSlow1 * fRec0[IOTA0 & 8191]);
			output1[i0] = FAUSTFLOAT(fSlow10 * fRec0[(IOTA0 - iSlow11) & 8191]);
			IOTA0 = IOTA0 + 1;
			iRec2[1] = iRec2[0];
			fRec1[1] = fRec1[0];
			iRec3[1] = iRec3[0];
			fRec4[1] = fRec4[0];
			fVec0[1] = fVec0[0];
		}
	}

};


#include "Faust_plugins_template2.cpp"


#endif
