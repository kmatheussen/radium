/* ------------------------------------------------------------
author: "Romain Michon"
copyright: "Romain Michon (rmichon@ccrma.stanford.edu)"
name: "Bass"
version: "1.0"
Code generated with Faust 2.81.2 (https://faust.grame.fr)
Compilation options: -a ../../Faust_plugins_template.cpp -lang cpp -nvi -ct 1 -cn Bass_dsp -es 1 -mcd 16 -mdd 1024 -mdy 33 -single -ftz 0
------------------------------------------------------------ */

#ifndef  __Bass_dsp_H__
#define  __Bass_dsp_H__


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
#include <bass.h>
#include <cmath>
#include <cstdint>
#include <math.h>

#ifndef FAUSTCLASS 
#define FAUSTCLASS Bass_dsp
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

class Bass_dspSIG0 {
	
  private:
	
	int iRec13[2];
	
  public:
	
	int getNumInputsBass_dspSIG0() {
		return 0;
	}
	int getNumOutputsBass_dspSIG0() {
		return 1;
	}
	
	void instanceInitBass_dspSIG0(int sample_rate) {
		for (int l12 = 0; l12 < 2; l12 = l12 + 1) {
			iRec13[l12] = 0;
		}
	}
	
	void fillBass_dspSIG0(int count, float* table) {
		for (int i1 = 0; i1 < count; i1 = i1 + 1) {
			iRec13[0] = iRec13[1] + 1;
			table[i1] = std::sin(9.58738e-05f * float(iRec13[0] + -1));
			iRec13[1] = iRec13[0];
		}
	}

};

static Bass_dspSIG0* newBass_dspSIG0() { return (Bass_dspSIG0*)new Bass_dspSIG0(); }
static void deleteBass_dspSIG0(Bass_dspSIG0* dsp) { delete dsp; }

static float *ftbl0Bass_dspSIG0; __attribute__((constructor)) static void initialize_ftbl0Bass_dspSIG0() { ftbl0Bass_dspSIG0 = (float*)calloc(65536, sizeof(float));};
static float Bass_dsp_faustpower2_f(float value) {
	return value * value;
}

class Bass_dsp final : public dsp {
	
 private:
	
	FAUSTFLOAT fHslider0;
	FAUSTFLOAT fButton0;
	int iRec0[2];
	int fSampleRate;
	float fConst0;
	FAUSTFLOAT fHslider1;
	FAUSTFLOAT fHslider2;
	float fRec1[2];
	float fRec7[2];
	float fConst1;
	FAUSTFLOAT fHslider3;
	FAUSTFLOAT fEntry0;
	float fRec6[2];
	int iRec8[2];
	float fVec0[2];
	float fConst2;
	float fRec9[2];
	float fRec5[2];
	float fRec4[2];
	FAUSTFLOAT fEntry1;
	FAUSTFLOAT fEntry2;
	int IOTA0;
	float fConst3;
	float fRec11[2];
	float fVec1[2];
	FAUSTFLOAT fHslider4;
	float fRec12[2];
	float fConst4;
	FAUSTFLOAT fHslider5;
	float fRec15[2];
	float fRec14[2];
	float fRec21[2];
	float fRec20[2];
	float fRec19[2];
	float fRec18[2];
	float fRec17[2];
	float fRec16[2];
	float fRec27[2];
	float fRec26[2];
	float fRec25[2];
	float fRec24[2];
	float fRec23[2];
	float fRec22[2];
	float fVec2[2];
	float fRec10[2];
	int iRec29[2];
	float fConst5;
	float fRec28[2];
	float fRec3[8192];
	float fConst6;
	float fRec2[3];
	float fVec3[4096];
	float fConst7;
	FAUSTFLOAT fHslider6;
	
 public:
	Bass_dsp() {
	}
	
	void metadata(Meta* m) { 
		m->declare("author", "Romain Michon");
		m->declare("compile_options", "-a ../../Faust_plugins_template.cpp -lang cpp -nvi -ct 1 -cn Bass_dsp -es 1 -mcd 16 -mdd 1024 -mdy 33 -single -ftz 0");
		m->declare("copyright", "Romain Michon (rmichon@ccrma.stanford.edu)");
		m->declare("description", "Nonlinear WaveGuide Acoustic Bass");
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
		m->declare("filename", "bass.dsp");
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
		m->declare("name", "Bass");
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
		Bass_dspSIG0* sig0 = newBass_dspSIG0();
		sig0->instanceInitBass_dspSIG0(sample_rate);
		sig0->fillBass_dspSIG0(65536, ftbl0Bass_dspSIG0);
		deleteBass_dspSIG0(sig0);
	}
	
	void instanceConstants(int sample_rate) {
		fSampleRate = sample_rate;
		fConst0 = std::min<float>(1.92e+05f, std::max<float>(1.0f, float(fSampleRate)));
		fConst1 = 3.5f / fConst0;
		fConst2 = std::exp(-(3.5e+02f / fConst0));
		fConst3 = std::exp(-(7e+02f / fConst0));
		fConst4 = 1.0f / fConst0;
		fConst5 = std::exp(-(1.4e+02f / fConst0));
		fConst6 = 1.994f * std::cos(678.584f / fConst0);
		fConst7 = 0.5f * fConst0;
	}
	
	void instanceResetUserInterface() {
		fHslider0 = FAUSTFLOAT(0.6f);
		fButton0 = FAUSTFLOAT(0.0f);
		fHslider1 = FAUSTFLOAT(0.0f);
		fHslider2 = FAUSTFLOAT(0.1f);
		fHslider3 = FAUSTFLOAT(0.15f);
		fEntry0 = FAUSTFLOAT(1.0f);
		fEntry1 = FAUSTFLOAT(1.2e+02f);
		fEntry2 = FAUSTFLOAT(0.0f);
		fHslider4 = FAUSTFLOAT(0.0f);
		fHslider5 = FAUSTFLOAT(2.2e+02f);
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
			fRec7[l2] = 0.0f;
		}
		for (int l3 = 0; l3 < 2; l3 = l3 + 1) {
			fRec6[l3] = 0.0f;
		}
		for (int l4 = 0; l4 < 2; l4 = l4 + 1) {
			iRec8[l4] = 0;
		}
		for (int l5 = 0; l5 < 2; l5 = l5 + 1) {
			fVec0[l5] = 0.0f;
		}
		for (int l6 = 0; l6 < 2; l6 = l6 + 1) {
			fRec9[l6] = 0.0f;
		}
		for (int l7 = 0; l7 < 2; l7 = l7 + 1) {
			fRec5[l7] = 0.0f;
		}
		for (int l8 = 0; l8 < 2; l8 = l8 + 1) {
			fRec4[l8] = 0.0f;
		}
		IOTA0 = 0;
		for (int l9 = 0; l9 < 2; l9 = l9 + 1) {
			fRec11[l9] = 0.0f;
		}
		for (int l10 = 0; l10 < 2; l10 = l10 + 1) {
			fVec1[l10] = 0.0f;
		}
		for (int l11 = 0; l11 < 2; l11 = l11 + 1) {
			fRec12[l11] = 0.0f;
		}
		for (int l13 = 0; l13 < 2; l13 = l13 + 1) {
			fRec15[l13] = 0.0f;
		}
		for (int l14 = 0; l14 < 2; l14 = l14 + 1) {
			fRec14[l14] = 0.0f;
		}
		for (int l15 = 0; l15 < 2; l15 = l15 + 1) {
			fRec21[l15] = 0.0f;
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
			fRec27[l21] = 0.0f;
		}
		for (int l22 = 0; l22 < 2; l22 = l22 + 1) {
			fRec26[l22] = 0.0f;
		}
		for (int l23 = 0; l23 < 2; l23 = l23 + 1) {
			fRec25[l23] = 0.0f;
		}
		for (int l24 = 0; l24 < 2; l24 = l24 + 1) {
			fRec24[l24] = 0.0f;
		}
		for (int l25 = 0; l25 < 2; l25 = l25 + 1) {
			fRec23[l25] = 0.0f;
		}
		for (int l26 = 0; l26 < 2; l26 = l26 + 1) {
			fRec22[l26] = 0.0f;
		}
		for (int l27 = 0; l27 < 2; l27 = l27 + 1) {
			fVec2[l27] = 0.0f;
		}
		for (int l28 = 0; l28 < 2; l28 = l28 + 1) {
			fRec10[l28] = 0.0f;
		}
		for (int l29 = 0; l29 < 2; l29 = l29 + 1) {
			iRec29[l29] = 0;
		}
		for (int l30 = 0; l30 < 2; l30 = l30 + 1) {
			fRec28[l30] = 0.0f;
		}
		for (int l31 = 0; l31 < 8192; l31 = l31 + 1) {
			fRec3[l31] = 0.0f;
		}
		for (int l32 = 0; l32 < 3; l32 = l32 + 1) {
			fRec2[l32] = 0.0f;
		}
		for (int l33 = 0; l33 < 4096; l33 = l33 + 1) {
			fVec3[l33] = 0.0f;
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
	
	Bass_dsp* clone() {
		return new Bass_dsp();
	}
	
	int getSampleRate() {
		return fSampleRate;
	}
	
	void buildUserInterface(UI* ui_interface) {
		ui_interface->openVerticalBox("Bass");
		ui_interface->openHorizontalBox("Basic_Parameters");
		ui_interface->declare(&fEntry1, "1", "");
		ui_interface->declare(&fEntry1, "tooltip", "Tone frequency");
		ui_interface->declare(&fEntry1, "unit", "Hz");
		ui_interface->addNumEntry("freq", &fEntry1, FAUSTFLOAT(1.2e+02f), FAUSTFLOAT(2e+01f), FAUSTFLOAT(2e+04f), FAUSTFLOAT(1.0f));
		ui_interface->declare(&fEntry0, "1", "");
		ui_interface->declare(&fEntry0, "tooltip", "Gain (value between 0 and 1)");
		ui_interface->addNumEntry("gain", &fEntry0, FAUSTFLOAT(1.0f), FAUSTFLOAT(0.0f), FAUSTFLOAT(1.0f), FAUSTFLOAT(0.01f));
		ui_interface->declare(&fButton0, "1", "");
		ui_interface->declare(&fButton0, "tooltip", "noteOn = 1, noteOff = 0");
		ui_interface->addButton("gate", &fButton0);
		ui_interface->closeBox();
		ui_interface->openHorizontalBox("Envelopes_and_Vibrato");
		ui_interface->openVerticalBox("Global_Envelope_Parameters");
		ui_interface->declare(&fHslider1, "4", "");
		ui_interface->declare(&fHslider1, "tooltip", "Global envelope attack duration");
		ui_interface->declare(&fHslider1, "unit", "s");
		ui_interface->addHorizontalSlider("Glob_Env_Attack", &fHslider1, FAUSTFLOAT(0.0f), FAUSTFLOAT(0.0f), FAUSTFLOAT(2.0f), FAUSTFLOAT(0.01f));
		ui_interface->declare(&fHslider2, "4", "");
		ui_interface->declare(&fHslider2, "tooltip", "Global envelope release duration");
		ui_interface->declare(&fHslider2, "unit", "s");
		ui_interface->addHorizontalSlider("Glob_Env_Release", &fHslider2, FAUSTFLOAT(0.1f), FAUSTFLOAT(0.0f), FAUSTFLOAT(2.0f), FAUSTFLOAT(0.01f));
		ui_interface->closeBox();
		ui_interface->closeBox();
		ui_interface->openVerticalBox("Nonlinear_Filter_Parameters");
		ui_interface->declare(&fHslider5, "3", "");
		ui_interface->declare(&fHslider5, "tooltip", "Frequency of the sine wave for the modulation of theta (works if Modulation Type=3)");
		ui_interface->declare(&fHslider5, "unit", "Hz");
		ui_interface->addHorizontalSlider("Modulation_Frequency", &fHslider5, FAUSTFLOAT(2.2e+02f), FAUSTFLOAT(2e+01f), FAUSTFLOAT(1e+03f), FAUSTFLOAT(0.1f));
		ui_interface->declare(&fEntry2, "3", "");
		ui_interface->declare(&fEntry2, "tooltip", "0=theta is modulated by the incoming signal; 1=theta is modulated by the averaged incoming signal; 2=theta is modulated by the squared incoming signal; 3=theta is modulated by a sine wave of frequency freqMod; 4=theta is modulated by a sine wave of frequency freq;");
		ui_interface->addNumEntry("Modulation_Type", &fEntry2, FAUSTFLOAT(0.0f), FAUSTFLOAT(0.0f), FAUSTFLOAT(4.0f), FAUSTFLOAT(1.0f));
		ui_interface->declare(&fHslider4, "3", "");
		ui_interface->declare(&fHslider4, "tooltip", "Nonlinearity factor (value between 0 and 1)");
		ui_interface->addHorizontalSlider("Nonlinearity", &fHslider4, FAUSTFLOAT(0.0f), FAUSTFLOAT(0.0f), FAUSTFLOAT(1.0f), FAUSTFLOAT(0.01f));
		ui_interface->closeBox();
		ui_interface->openVerticalBox("Physical_Parameters");
		ui_interface->declare(&fHslider3, "2", "");
		ui_interface->declare(&fHslider3, "tooltip", "A value between 0 and 1");
		ui_interface->addHorizontalSlider("Touch_Length", &fHslider3, FAUSTFLOAT(0.15f), FAUSTFLOAT(0.0f), FAUSTFLOAT(1.0f), FAUSTFLOAT(0.01f));
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
		float fSlow1 = 2.0f * (1.0f - fSlow0);
		float fSlow2 = float(fButton0);
		int iSlow3 = fSlow2 > 0.0f;
		float fSlow4 = float(fHslider1);
		float fSlow5 = 1.0f / (fConst0 * fSlow4 + float(fSlow4 == 0.0f));
		float fSlow6 = float(fHslider2);
		float fSlow7 = 1.0f - 1.0f / std::pow(1e+05f, 1.0f / (fConst0 * fSlow6 + float(fSlow6 == 0.0f)));
		int iSlow8 = fSlow2 <= 0.0f;
		float fSlow9 = std::exp(-(fConst1 / float(fHslider3)));
		int iSlow10 = fSlow2 < 1.0f;
		float fSlow11 = float(fEntry0);
		float fSlow12 = float(fEntry1);
		float fSlow13 = float(int(17.31234f * (std::log(fSlow12) + -6.086775f) + 69.5f));
		float fSlow14 = getValueBassLoopFilterb0(fSlow13);
		float fSlow15 = float(fEntry2);
		float fSlow16 = float(fSlow15 >= 3.0f);
		float fSlow17 = fConst0 / fSlow12;
		float fSlow18 = 0.001f * float(fHslider4);
		float fSlow19 = fSlow12 * float(fSlow15 == 4.0f);
		float fSlow20 = float(fSlow15 != 4.0f);
		float fSlow21 = 0.001f * float(fHslider5);
		float fSlow22 = float(fSlow15 < 3.0f);
		float fSlow23 = 3.1415927f * float(fSlow15 == 0.0f);
		float fSlow24 = 1.5707964f * float(fSlow15 == 1.0f);
		float fSlow25 = 3.1415927f * float(fSlow15 == 2.0f);
		float fSlow26 = getValueBassLoopFilterb1(fSlow13);
		float fSlow27 = getValueBassLoopFiltera1(fSlow13);
		float fSlow28 = float(iSlow10);
		int iSlow29 = iSlow10 > 0;
		int iSlow30 = iSlow10 < 1;
		float fSlow31 = 2.0f * fSlow0;
		int iSlow32 = int(fConst7 * (float(fHslider6) / fSlow12)) & 4095;
		for (int i0 = 0; i0 < count; i0 = i0 + 1) {
			iRec0[0] = iSlow3 & (iRec0[1] | (fRec1[1] >= 1.0f));
			int iTemp0 = iSlow8 & (fRec1[1] > 0.0f);
			fRec1[0] = (fSlow5 * float(((iRec0[1] == 0) & iSlow3) & (fRec1[1] < 1.0f)) + fRec1[1] * (1.0f - fSlow7 * float(iTemp0))) * float((iTemp0 == 0) | (fRec1[1] >= 1e-06f));
			fRec7[0] = fSlow2 * fRec7[1] + 1.0f;
			float fTemp1 = fRec7[0] + -1.0f;
			int iTemp2 = (fTemp1 < 2.0f) & iSlow3;
			float fTemp3 = float(iTemp2);
			float fTemp4 = 0.030197384f * fTemp3;
			float fTemp5 = float((fTemp1 >= 2.0f) | iSlow10);
			float fTemp6 = fTemp4 + fSlow9 * fTemp5;
			fRec6[0] = fRec6[1] * fTemp6 + fSlow11 * fTemp3 * (1.0f - fTemp6);
			iRec8[0] = 1103515245 * iRec8[1] + 12345;
			float fTemp7 = fRec6[0] * float(iRec8[0]);
			fVec0[0] = fTemp7;
			float fTemp8 = fTemp4 + fConst2 * fTemp5;
			float fTemp9 = float(iTemp2 < 1);
			fRec9[0] = fRec9[1] * fTemp8 - (1.0f - fTemp8) * (0.5f * fTemp3 + 0.985f * fTemp9);
			fRec5[0] = 1.6298145e-11f * (fTemp7 * (fRec9[0] + 1.0f) - fRec9[0] * fVec0[1]) + 0.965f * fRec5[1];
			fRec4[0] = 0.035f * fRec5[0] + 0.965f * fRec4[1];
			float fTemp10 = fTemp4 + fConst3 * fTemp5;
			fRec11[0] = fRec11[1] * fTemp10 + fSlow17 * (1.0f - fTemp10) * fTemp9;
			int iTemp11 = int(fRec11[0]);
			float fTemp12 = float(iTemp11);
			float fTemp13 = fRec3[(IOTA0 - ((iTemp11 & 4095) + 1)) & 8191] * (fTemp12 + (1.0f - fRec11[0])) + (fRec11[0] - fTemp12) * fRec3[(IOTA0 - (((iTemp11 + 1) & 4095) + 1)) & 8191];
			fVec1[0] = fTemp13;
			fRec12[0] = fSlow18 + 0.999f * fRec12[1];
			fRec15[0] = fSlow21 + 0.999f * fRec15[1];
			float fTemp14 = fRec14[1] + fConst4 * (fSlow19 + fSlow20 * fRec15[0]);
			fRec14[0] = fTemp14 - std::floor(fTemp14);
			float fTemp15 = 3.1415927f * fRec12[0] * ftbl0Bass_dspSIG0[std::max<int>(0, std::min<int>(int(65536.0f * fRec14[0]), 65535))];
			float fTemp16 = std::sin(fTemp15);
			float fTemp17 = std::cos(fTemp15);
			float fTemp18 = fTemp13 * fTemp17 - fTemp16 * fRec16[1];
			float fTemp19 = fTemp17 * fTemp18 - fTemp16 * fRec17[1];
			float fTemp20 = fTemp17 * fTemp19 - fTemp16 * fRec18[1];
			float fTemp21 = fTemp17 * fTemp20 - fTemp16 * fRec19[1];
			float fTemp22 = fTemp17 * fTemp21 - fTemp16 * fRec20[1];
			fRec21[0] = fTemp17 * fTemp22 - fTemp16 * fRec21[1];
			fRec20[0] = fTemp16 * fTemp22 + fTemp17 * fRec21[1];
			fRec19[0] = fTemp16 * fTemp21 + fTemp17 * fRec20[1];
			fRec18[0] = fTemp16 * fTemp20 + fTemp17 * fRec19[1];
			fRec17[0] = fTemp16 * fTemp19 + fTemp17 * fRec18[1];
			fRec16[0] = fTemp16 * fTemp18 + fTemp17 * fRec17[1];
			float fTemp23 = fRec12[0] * (fSlow23 * fTemp13 + fSlow24 * (fTemp13 + fVec1[1]) + fSlow25 * Bass_dsp_faustpower2_f(fTemp13));
			float fTemp24 = std::sin(fTemp23);
			float fTemp25 = std::cos(fTemp23);
			float fTemp26 = fTemp13 * fTemp25 - fTemp24 * fRec22[1];
			float fTemp27 = fTemp25 * fTemp26 - fTemp24 * fRec23[1];
			float fTemp28 = fTemp25 * fTemp27 - fTemp24 * fRec24[1];
			float fTemp29 = fTemp25 * fTemp28 - fTemp24 * fRec25[1];
			float fTemp30 = fTemp25 * fTemp29 - fTemp24 * fRec26[1];
			fRec27[0] = fTemp25 * fTemp30 - fTemp24 * fRec27[1];
			fRec26[0] = fTemp24 * fTemp30 + fTemp25 * fRec27[1];
			fRec25[0] = fTemp24 * fTemp29 + fTemp25 * fRec26[1];
			fRec24[0] = fTemp24 * fTemp28 + fTemp25 * fRec25[1];
			fRec23[0] = fTemp24 * fTemp27 + fTemp25 * fRec24[1];
			fRec22[0] = fTemp24 * fTemp26 + fTemp25 * fRec23[1];
			float fTemp31 = fSlow16 * (fTemp13 * fTemp16 + fRec16[1] * fTemp17) + fSlow22 * (fRec12[0] * (fTemp13 * fTemp24 + fRec22[1] * fTemp25) + (1.0f - fRec12[0]) * fTemp13);
			fVec2[0] = fTemp31;
			fRec10[0] = fSlow14 * fTemp31 + fSlow26 * fVec2[1] - fSlow27 * fRec10[1];
			iRec29[0] = iSlow10 * iRec29[1] + 1;
			float fTemp32 = float(iRec29[0] + -1);
			int iTemp33 = (fTemp32 < 2.0f) & iSlow29;
			float fTemp34 = float(iTemp33);
			float fTemp35 = 0.030197384f * fTemp34 + fConst5 * float((fTemp32 >= 2.0f) | iSlow30);
			fRec28[0] = fRec28[1] * fTemp35 + (1.0f - fTemp35) * (fTemp34 + 0.9f * float(iTemp33 < 1));
			fRec3[IOTA0 & 8191] = fRec4[0] + fRec10[0] * (fSlow2 + fSlow28 * fRec28[0]);
			float fTemp36 = fRec3[IOTA0 & 8191];
			fRec2[0] = fTemp36 + fConst6 * fRec2[1] - 0.994009f * fRec2[2];
			float fTemp37 = fRec1[0] * (0.00449325f * (fRec2[0] - fRec2[2]) + 0.5f * fTemp36);
			fVec3[IOTA0 & 4095] = fTemp37;
			output0[i0] = FAUSTFLOAT(fSlow1 * fTemp37);
			output1[i0] = FAUSTFLOAT(fSlow31 * fVec3[(IOTA0 - iSlow32) & 4095]);
			iRec0[1] = iRec0[0];
			fRec1[1] = fRec1[0];
			fRec7[1] = fRec7[0];
			fRec6[1] = fRec6[0];
			iRec8[1] = iRec8[0];
			fVec0[1] = fVec0[0];
			fRec9[1] = fRec9[0];
			fRec5[1] = fRec5[0];
			fRec4[1] = fRec4[0];
			IOTA0 = IOTA0 + 1;
			fRec11[1] = fRec11[0];
			fVec1[1] = fVec1[0];
			fRec12[1] = fRec12[0];
			fRec15[1] = fRec15[0];
			fRec14[1] = fRec14[0];
			fRec21[1] = fRec21[0];
			fRec20[1] = fRec20[0];
			fRec19[1] = fRec19[0];
			fRec18[1] = fRec18[0];
			fRec17[1] = fRec17[0];
			fRec16[1] = fRec16[0];
			fRec27[1] = fRec27[0];
			fRec26[1] = fRec26[0];
			fRec25[1] = fRec25[0];
			fRec24[1] = fRec24[0];
			fRec23[1] = fRec23[0];
			fRec22[1] = fRec22[0];
			fVec2[1] = fVec2[0];
			fRec10[1] = fRec10[0];
			iRec29[1] = iRec29[0];
			fRec28[1] = fRec28[0];
			fRec2[2] = fRec2[1];
			fRec2[1] = fRec2[0];
		}
	}

};


#include "Faust_plugins_template2.cpp"


#endif
