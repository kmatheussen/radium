/* ------------------------------------------------------------
name: "system_tremolo"
Code generated with Faust 2.81.2 (https://faust.grame.fr)
Compilation options: -a Faust_plugins_template.cpp -lang cpp -nvi -ct 1 -cn System_Tremolo_dsp -es 1 -mcd 16 -mdd 1024 -mdy 33 -single -ftz 0
------------------------------------------------------------ */

#ifndef  __System_Tremolo_dsp_H__
#define  __System_Tremolo_dsp_H__


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
#define FAUSTCLASS System_Tremolo_dsp
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

class System_Tremolo_dspSIG0 {
	
  private:
	
	int iRec1[2];
	
  public:
	
	int getNumInputsSystem_Tremolo_dspSIG0() {
		return 0;
	}
	int getNumOutputsSystem_Tremolo_dspSIG0() {
		return 1;
	}
	
	void instanceInitSystem_Tremolo_dspSIG0(int sample_rate) {
		for (int l1 = 0; l1 < 2; l1 = l1 + 1) {
			iRec1[l1] = 0;
		}
	}
	
	void fillSystem_Tremolo_dspSIG0(int count, float* table) {
		for (int i1 = 0; i1 < count; i1 = i1 + 1) {
			iRec1[0] = iRec1[1] + 1;
			table[i1] = std::sin(9.58738e-05f * float(iRec1[0] + -1));
			iRec1[1] = iRec1[0];
		}
	}

};

static System_Tremolo_dspSIG0* newSystem_Tremolo_dspSIG0() { return (System_Tremolo_dspSIG0*)new System_Tremolo_dspSIG0(); }
static void deleteSystem_Tremolo_dspSIG0(System_Tremolo_dspSIG0* dsp) { delete dsp; }

static float *ftbl0System_Tremolo_dspSIG0; __attribute__((constructor)) static void initialize_ftbl0System_Tremolo_dspSIG0() { ftbl0System_Tremolo_dspSIG0 = (float*)calloc(65536, sizeof(float));};

class System_Tremolo_dsp final : public dsp {
	
 private:
	
	FAUSTFLOAT fHslider0;
	float fRec0[2];
	int fSampleRate;
	float fConst0;
	FAUSTFLOAT fHslider1;
	float fRec3[2];
	float fRec2[2];
	
 public:
	System_Tremolo_dsp() {
	}
	
	void metadata(Meta* m) { 
		m->declare("compile_options", "-a Faust_plugins_template.cpp -lang cpp -nvi -ct 1 -cn System_Tremolo_dsp -es 1 -mcd 16 -mdd 1024 -mdy 33 -single -ftz 0");
		m->declare("filename", "system_tremolo.dsp");
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
		m->declare("name", "system_tremolo");
	}

	static constexpr int getStaticNumInputs() {
		return 2;
	}
	static constexpr int getStaticNumOutputs() {
		return 2;
	}
	int getNumInputs() {
		return 2;
	}
	int getNumOutputs() {
		return 2;
	}
	
	static void classInit(int sample_rate) {
		System_Tremolo_dspSIG0* sig0 = newSystem_Tremolo_dspSIG0();
		sig0->instanceInitSystem_Tremolo_dspSIG0(sample_rate);
		sig0->fillSystem_Tremolo_dspSIG0(65536, ftbl0System_Tremolo_dspSIG0);
		deleteSystem_Tremolo_dspSIG0(sig0);
	}
	
	void instanceConstants(int sample_rate) {
		fSampleRate = sample_rate;
		fConst0 = 1.0f / std::min<float>(1.92e+05f, std::max<float>(1.0f, float(fSampleRate)));
	}
	
	void instanceResetUserInterface() {
		fHslider0 = FAUSTFLOAT(0.0f);
		fHslider1 = FAUSTFLOAT(5.0f);
	}
	
	void instanceClear() {
		for (int l0 = 0; l0 < 2; l0 = l0 + 1) {
			fRec0[l0] = 0.0f;
		}
		for (int l2 = 0; l2 < 2; l2 = l2 + 1) {
			fRec3[l2] = 0.0f;
		}
		for (int l3 = 0; l3 < 2; l3 = l3 + 1) {
			fRec2[l3] = 0.0f;
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
	
	System_Tremolo_dsp* clone() {
		return new System_Tremolo_dsp();
	}
	
	int getSampleRate() {
		return fSampleRate;
	}
	
	void buildUserInterface(UI* ui_interface) {
		ui_interface->openVerticalBox("system_tremolo");
		ui_interface->declare(&fHslider1, "1", "");
		ui_interface->addHorizontalSlider("frequency", &fHslider1, FAUSTFLOAT(5.0f), FAUSTFLOAT(0.1f), FAUSTFLOAT(15.0f), FAUSTFLOAT(0.01f));
		ui_interface->declare(&fHslider0, "2", "");
		ui_interface->addHorizontalSlider("depth", &fHslider0, FAUSTFLOAT(0.0f), FAUSTFLOAT(0.0f), FAUSTFLOAT(1.0f), FAUSTFLOAT(0.01f));
		ui_interface->closeBox();
	}
	
	void compute(int count, FAUSTFLOAT** RESTRICT inputs, FAUSTFLOAT** RESTRICT outputs) {
		FAUSTFLOAT* input0 = inputs[0];
		FAUSTFLOAT* input1 = inputs[1];
		FAUSTFLOAT* output0 = outputs[0];
		FAUSTFLOAT* output1 = outputs[1];
		float fSlow0 = 0.001f * float(fHslider0);
		float fSlow1 = 0.001f * float(fHslider1);
		for (int i0 = 0; i0 < count; i0 = i0 + 1) {
			fRec0[0] = fSlow0 + 0.999f * fRec0[1];
			fRec3[0] = fSlow1 + 0.999f * fRec3[1];
			float fTemp0 = fRec2[1] + fConst0 * fRec3[0];
			fRec2[0] = fTemp0 - std::floor(fTemp0);
			float fTemp1 = 1.0f - 0.5f * fRec0[0] * (ftbl0System_Tremolo_dspSIG0[std::max<int>(0, std::min<int>(int(65536.0f * fRec2[0]), 65535))] + 1.0f);
			output0[i0] = FAUSTFLOAT(float(input0[i0]) * fTemp1);
			output1[i0] = FAUSTFLOAT(float(input1[i0]) * fTemp1);
			fRec0[1] = fRec0[0];
			fRec3[1] = fRec3[0];
			fRec2[1] = fRec2[0];
		}
	}

};


#include "Faust_plugins_template2.cpp"


#endif
