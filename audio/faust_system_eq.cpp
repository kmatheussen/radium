/* ------------------------------------------------------------
name: "system_eq"
Code generated with Faust 2.81.2 (https://faust.grame.fr)
Compilation options: -a Faust_plugins_template.cpp -lang cpp -nvi -ct 1 -cn System_Eq_dsp -es 1 -mcd 16 -mdd 1024 -mdy 33 -single -ftz 0
------------------------------------------------------------ */

#ifndef  __System_Eq_dsp_H__
#define  __System_Eq_dsp_H__


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
#define FAUSTCLASS System_Eq_dsp
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


class System_Eq_dsp final : public dsp {
	
 private:
	
	int fSampleRate;
	float fConst0;
	FAUSTFLOAT fVslider0;
	FAUSTFLOAT fVslider1;
	float fRec0[2];
	float fRec2[2];
	float fRec1[3];
	float fRec3[2];
	
 public:
	System_Eq_dsp() {
	}
	
	void metadata(Meta* m) { 
		m->declare("compile_options", "-a Faust_plugins_template.cpp -lang cpp -nvi -ct 1 -cn System_Eq_dsp -es 1 -mcd 16 -mdd 1024 -mdy 33 -single -ftz 0");
		m->declare("filename", "system_eq.dsp");
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
		m->declare("name", "system_eq");
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
		fConst0 = 6.2831855f / std::min<float>(1.92e+05f, std::max<float>(1.0f, float(fSampleRate)));
	}
	
	void instanceResetUserInterface() {
		fVslider0 = FAUSTFLOAT(315.0f);
		fVslider1 = FAUSTFLOAT(0.0f);
	}
	
	void instanceClear() {
		for (int l0 = 0; l0 < 2; l0 = l0 + 1) {
			fRec0[l0] = 0.0f;
		}
		for (int l1 = 0; l1 < 2; l1 = l1 + 1) {
			fRec2[l1] = 0.0f;
		}
		for (int l2 = 0; l2 < 3; l2 = l2 + 1) {
			fRec1[l2] = 0.0f;
		}
		for (int l3 = 0; l3 < 2; l3 = l3 + 1) {
			fRec3[l3] = 0.0f;
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
	
	System_Eq_dsp* clone() {
		return new System_Eq_dsp();
	}
	
	int getSampleRate() {
		return fSampleRate;
	}
	
	void buildUserInterface(UI* ui_interface) {
		ui_interface->declare(0, "3", "");
		ui_interface->openHorizontalBox("RM Peaking Equalizer 1");
		ui_interface->declare(&fVslider0, "1", "");
		ui_interface->declare(&fVslider0, "style", "knob");
		ui_interface->declare(&fVslider0, "tooltip", "Center-frequency of second-order Regalia-Mitra peaking equalizer section 1");
		ui_interface->declare(&fVslider0, "unit", "Hz");
		ui_interface->addVerticalSlider("Eq1 Freq", &fVslider0, FAUSTFLOAT(315.0f), FAUSTFLOAT(4e+01f), FAUSTFLOAT(2e+04f), FAUSTFLOAT(1.0f));
		ui_interface->declare(&fVslider1, "2", "");
		ui_interface->declare(&fVslider1, "style", "knob");
		ui_interface->declare(&fVslider1, "tooltip", "Peak level in dB of second-order Regalia-Mitra peaking equalizer section 1");
		ui_interface->declare(&fVslider1, "unit", "dB");
		ui_interface->addVerticalSlider("Eq1 Level", &fVslider1, FAUSTFLOAT(0.0f), FAUSTFLOAT(-35.0f), FAUSTFLOAT(35.0f), FAUSTFLOAT(0.1f));
		ui_interface->closeBox();
	}
	
	void compute(int count, FAUSTFLOAT** RESTRICT inputs, FAUSTFLOAT** RESTRICT outputs) {
		FAUSTFLOAT* input0 = inputs[0];
		FAUSTFLOAT* output0 = outputs[0];
		float fSlow0 = float(fVslider0);
		float fSlow1 = std::pow(1e+01f, 0.05f * float(fVslider1));
		float fSlow2 = fConst0 * (fSlow0 / std::sqrt(std::max<float>(0.0f, fSlow1)));
		float fSlow3 = 0.001f * ((1.0f - fSlow2) / (fSlow2 + 1.0f));
		float fSlow4 = 0.001f * std::cos(fConst0 * fSlow0);
		float fSlow5 = 0.0005f * fSlow1;
		for (int i0 = 0; i0 < count; i0 = i0 + 1) {
			fRec0[0] = fSlow3 + 0.999f * fRec0[1];
			float fTemp0 = float(input0[i0]);
			fRec2[0] = 0.999f * fRec2[1] - fSlow4;
			float fTemp1 = fRec2[0] * (fRec0[0] + 1.0f) * fRec1[1];
			fRec1[0] = fTemp0 - (fTemp1 + fRec0[0] * fRec1[2]);
			float fTemp2 = fRec0[0] * fRec1[0];
			fRec3[0] = fSlow5 + 0.999f * fRec3[1];
			output0[i0] = FAUSTFLOAT(0.5f * (fTemp2 + fRec1[2] + fTemp0 + fTemp1) + fRec3[0] * (fTemp2 + fTemp1 + fRec1[2] - fTemp0));
			fRec0[1] = fRec0[0];
			fRec2[1] = fRec2[0];
			fRec1[2] = fRec1[1];
			fRec1[1] = fRec1[0];
			fRec3[1] = fRec3[0];
		}
	}

};


#include "Faust_plugins_template2.cpp"


#endif
