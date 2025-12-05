/* ------------------------------------------------------------
name: "system_lowpass"
Code generated with Faust 2.81.2 (https://faust.grame.fr)
Compilation options: -a Faust_plugins_template.cpp -lang cpp -nvi -ct 1 -cn System_Lowpass_dsp -es 1 -mcd 16 -mdd 1024 -mdy 33 -single -ftz 0
------------------------------------------------------------ */

#ifndef  __System_Lowpass_dsp_H__
#define  __System_Lowpass_dsp_H__


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
#define FAUSTCLASS System_Lowpass_dsp
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

static float System_Lowpass_dsp_faustpower2_f(float value) {
	return value * value;
}

class System_Lowpass_dsp final : public dsp {
	
 private:
	
	int fSampleRate;
	float fConst0;
	float fConst1;
	float fConst2;
	FAUSTFLOAT fVslider0;
	float fRec1[2];
	float fRec0[3];
	
 public:
	System_Lowpass_dsp() {
	}
	
	void metadata(Meta* m) { 
		m->declare("compile_options", "-a Faust_plugins_template.cpp -lang cpp -nvi -ct 1 -cn System_Lowpass_dsp -es 1 -mcd 16 -mdd 1024 -mdy 33 -single -ftz 0");
		m->declare("filename", "system_lowpass.dsp");
		m->declare("filter_smoothing.lib/author", "Julius O. Smith (jos at ccrma.stanford.edu)");
		m->declare("filter_smoothing.lib/copyright", "Julius O. Smith III");
		m->declare("filter_smoothing.lib/license", "STK-4.3");
		m->declare("filter_smoothing.lib/name", "Faust Filter Library");
		m->declare("filter_smoothing.lib/reference", "https://ccrma.stanford.edu/~jos/filters/");
		m->declare("filter_smoothing.lib/version", "1.29");
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
		m->declare("name", "system_lowpass");
	}

	static constexpr int getStaticNumInputs() {
		return 1;
	}
	static constexpr int getStaticNumOutputs() {
		return 1;
	}
	int getNumInputs() {
		return 1;
	}
	int getNumOutputs() {
		return 1;
	}
	
	static void classInit(int sample_rate) {
	}
	
	void instanceConstants(int sample_rate) {
		fSampleRate = sample_rate;
		fConst0 = std::min<float>(1.92e+05f, std::max<float>(1.0f, float(fSampleRate)));
		fConst1 = 3.1415927f / fConst0;
		fConst2 = 0.5f * fConst0 + -1.0f;
	}
	
	void instanceResetUserInterface() {
		fVslider0 = FAUSTFLOAT(315.0f);
	}
	
	void instanceClear() {
		for (int l0 = 0; l0 < 2; l0 = l0 + 1) {
			fRec1[l0] = 0.0f;
		}
		for (int l1 = 0; l1 < 3; l1 = l1 + 1) {
			fRec0[l1] = 0.0f;
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
	
	System_Lowpass_dsp* clone() {
		return new System_Lowpass_dsp();
	}
	
	int getSampleRate() {
		return fSampleRate;
	}
	
	void buildUserInterface(UI* ui_interface) {
		ui_interface->openVerticalBox("system_lowpass");
		ui_interface->declare(&fVslider0, "1", "");
		ui_interface->declare(&fVslider0, "style", "knob");
		ui_interface->declare(&fVslider0, "unit", "Hz");
		ui_interface->addVerticalSlider("Eq1 Freq", &fVslider0, FAUSTFLOAT(315.0f), FAUSTFLOAT(4e+01f), FAUSTFLOAT(2e+04f), FAUSTFLOAT(1.0f));
		ui_interface->closeBox();
	}
	
	void compute(int count, FAUSTFLOAT** RESTRICT inputs, FAUSTFLOAT** RESTRICT outputs) {
		FAUSTFLOAT* input0 = inputs[0];
		FAUSTFLOAT* output0 = outputs[0];
		float fSlow0 = 0.001f / std::tan(fConst1 * std::min<float>(fConst2, float(fVslider0)));
		for (int i0 = 0; i0 < count; i0 = i0 + 1) {
			fRec1[0] = fSlow0 + 0.999f * fRec1[1];
			float fTemp0 = fRec1[0] * (fRec1[0] + 1.4142135f) + 1.0f;
			fRec0[0] = float(input0[i0]) - ((fRec1[0] * (fRec1[0] + -1.4142135f) + 1.0f) * fRec0[2] + 2.0f * (1.0f - System_Lowpass_dsp_faustpower2_f(fRec1[0])) * fRec0[1]) / fTemp0;
			output0[i0] = FAUSTFLOAT((fRec0[2] + fRec0[0] + 2.0f * fRec0[1]) / fTemp0);
			fRec1[1] = fRec1[0];
			fRec0[2] = fRec0[1];
			fRec0[1] = fRec0[0];
		}
	}

};


#include "Faust_plugins_template2.cpp"


#endif
