/* ------------------------------------------------------------
author: "Romain Michon (rmichon@ccrma.stanford.edu)"
copyright: "Romain Michon"
name: "Harpsichord"
version: "1.0"
Code generated with Faust 2.81.2 (https://faust.grame.fr)
Compilation options: -a ../../Faust_plugins_template.cpp -lang cpp -nvi -ct 1 -cn Harpsi_dsp -es 1 -mcd 16 -mdd 1024 -mdy 33 -single -ftz 0
------------------------------------------------------------ */

#ifndef  __Harpsi_dsp_H__
#define  __Harpsi_dsp_H__


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
#include <harpsichord.h>
#include <math.h>

#ifndef FAUSTCLASS 
#define FAUSTCLASS Harpsi_dsp
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

class Harpsi_dspSIG0 {
	
  private:
	
	int iRec4[2];
	
  public:
	
	int getNumInputsHarpsi_dspSIG0() {
		return 0;
	}
	int getNumOutputsHarpsi_dspSIG0() {
		return 1;
	}
	
	void instanceInitHarpsi_dspSIG0(int sample_rate) {
		for (int l2 = 0; l2 < 2; l2 = l2 + 1) {
			iRec4[l2] = 0;
		}
	}
	
	void fillHarpsi_dspSIG0(int count, float* table) {
		for (int i1 = 0; i1 < count; i1 = i1 + 1) {
			iRec4[0] = iRec4[1] + 1;
			table[i1] = std::sin(9.58738e-05f * float(iRec4[0] + -1));
			iRec4[1] = iRec4[0];
		}
	}

};

static Harpsi_dspSIG0* newHarpsi_dspSIG0() { return (Harpsi_dspSIG0*)new Harpsi_dspSIG0(); }
static void deleteHarpsi_dspSIG0(Harpsi_dspSIG0* dsp) { delete dsp; }

static float *ftbl0Harpsi_dspSIG0; __attribute__((constructor)) static void initialize_ftbl0Harpsi_dspSIG0() { ftbl0Harpsi_dspSIG0 = (float*)calloc(65536, sizeof(float));};
static float Harpsi_dsp_faustpower2_f(float value) {
	return value * value;
}

class Harpsi_dsp final : public dsp {
	
 private:
	
	FAUSTFLOAT fHslider0;
	FAUSTFLOAT fEntry0;
	FAUSTFLOAT fButton0;
	float fRec2[2];
	FAUSTFLOAT fEntry1;
	int IOTA0;
	FAUSTFLOAT fHslider1;
	float fRec3[2];
	int fSampleRate;
	float fConst0;
	float fConst1;
	FAUSTFLOAT fHslider2;
	float fRec6[2];
	float fRec5[2];
	float fRec12[2];
	float fRec11[2];
	float fRec10[2];
	float fRec9[2];
	float fRec8[2];
	float fRec7[2];
	float fRec18[2];
	float fRec17[2];
	float fRec16[2];
	float fRec15[2];
	float fRec14[2];
	float fRec13[2];
	float fRec20[2];
	float fConst2;
	FAUSTFLOAT fEntry2;
	float fRec19[2];
	int iRec21[2];
	float fVec0[4096];
	float fRec1[3];
	float fRec0[4096];
	float fConst3;
	FAUSTFLOAT fHslider3;
	
 public:
	Harpsi_dsp() {
	}
	
	void metadata(Meta* m) { 
		m->declare("author", "Romain Michon (rmichon@ccrma.stanford.edu)");
		m->declare("compile_options", "-a ../../Faust_plugins_template.cpp -lang cpp -nvi -ct 1 -cn Harpsi_dsp -es 1 -mcd 16 -mdd 1024 -mdy 33 -single -ftz 0");
		m->declare("copyright", "Romain Michon");
		m->declare("description", "Nonlinear WaveGuide Commuted Harpsichord");
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
		m->declare("filename", "harpsi.dsp");
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
		m->declare("name", "Harpsichord");
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
		Harpsi_dspSIG0* sig0 = newHarpsi_dspSIG0();
		sig0->instanceInitHarpsi_dspSIG0(sample_rate);
		sig0->fillHarpsi_dspSIG0(65536, ftbl0Harpsi_dspSIG0);
		deleteHarpsi_dspSIG0(sig0);
	}
	
	void instanceConstants(int sample_rate) {
		fSampleRate = sample_rate;
		fConst0 = std::min<float>(1.92e+05f, std::max<float>(1.0f, float(fSampleRate)));
		fConst1 = 1.0f / fConst0;
		fConst2 = 7.0f / fConst0;
		fConst3 = 0.5f * fConst0;
	}
	
	void instanceResetUserInterface() {
		fHslider0 = FAUSTFLOAT(0.6f);
		fEntry0 = FAUSTFLOAT(4.4e+02f);
		fButton0 = FAUSTFLOAT(0.0f);
		fEntry1 = FAUSTFLOAT(0.0f);
		fHslider1 = FAUSTFLOAT(0.0f);
		fHslider2 = FAUSTFLOAT(2.2e+02f);
		fEntry2 = FAUSTFLOAT(0.8f);
		fHslider3 = FAUSTFLOAT(0.5f);
	}
	
	void instanceClear() {
		for (int l0 = 0; l0 < 2; l0 = l0 + 1) {
			fRec2[l0] = 0.0f;
		}
		IOTA0 = 0;
		for (int l1 = 0; l1 < 2; l1 = l1 + 1) {
			fRec3[l1] = 0.0f;
		}
		for (int l3 = 0; l3 < 2; l3 = l3 + 1) {
			fRec6[l3] = 0.0f;
		}
		for (int l4 = 0; l4 < 2; l4 = l4 + 1) {
			fRec5[l4] = 0.0f;
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
			fRec7[l10] = 0.0f;
		}
		for (int l11 = 0; l11 < 2; l11 = l11 + 1) {
			fRec18[l11] = 0.0f;
		}
		for (int l12 = 0; l12 < 2; l12 = l12 + 1) {
			fRec17[l12] = 0.0f;
		}
		for (int l13 = 0; l13 < 2; l13 = l13 + 1) {
			fRec16[l13] = 0.0f;
		}
		for (int l14 = 0; l14 < 2; l14 = l14 + 1) {
			fRec15[l14] = 0.0f;
		}
		for (int l15 = 0; l15 < 2; l15 = l15 + 1) {
			fRec14[l15] = 0.0f;
		}
		for (int l16 = 0; l16 < 2; l16 = l16 + 1) {
			fRec13[l16] = 0.0f;
		}
		for (int l17 = 0; l17 < 2; l17 = l17 + 1) {
			fRec20[l17] = 0.0f;
		}
		for (int l18 = 0; l18 < 2; l18 = l18 + 1) {
			fRec19[l18] = 0.0f;
		}
		for (int l19 = 0; l19 < 2; l19 = l19 + 1) {
			iRec21[l19] = 0;
		}
		for (int l20 = 0; l20 < 4096; l20 = l20 + 1) {
			fVec0[l20] = 0.0f;
		}
		for (int l21 = 0; l21 < 3; l21 = l21 + 1) {
			fRec1[l21] = 0.0f;
		}
		for (int l22 = 0; l22 < 4096; l22 = l22 + 1) {
			fRec0[l22] = 0.0f;
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
	
	Harpsi_dsp* clone() {
		return new Harpsi_dsp();
	}
	
	int getSampleRate() {
		return fSampleRate;
	}
	
	void buildUserInterface(UI* ui_interface) {
		ui_interface->openVerticalBox("Harpsichord");
		ui_interface->openHorizontalBox("Basic_Parameters");
		ui_interface->declare(&fEntry0, "1", "");
		ui_interface->declare(&fEntry0, "tooltip", "Tone frequency");
		ui_interface->declare(&fEntry0, "unit", "Hz");
		ui_interface->addNumEntry("freq", &fEntry0, FAUSTFLOAT(4.4e+02f), FAUSTFLOAT(2e+01f), FAUSTFLOAT(2e+04f), FAUSTFLOAT(1.0f));
		ui_interface->declare(&fEntry2, "1", "");
		ui_interface->declare(&fEntry2, "tooltip", "Gain (value between 0 and 1)");
		ui_interface->addNumEntry("gain", &fEntry2, FAUSTFLOAT(0.8f), FAUSTFLOAT(0.0f), FAUSTFLOAT(1.0f), FAUSTFLOAT(0.01f));
		ui_interface->declare(&fButton0, "1", "");
		ui_interface->declare(&fButton0, "tooltip", "noteOn = 1, noteOff = 0");
		ui_interface->addButton("gate", &fButton0);
		ui_interface->closeBox();
		ui_interface->openHorizontalBox("Physical_and_Nonlinearity");
		ui_interface->openVerticalBox("Nonlinear_Filter_Parameters");
		ui_interface->declare(&fHslider2, "2", "");
		ui_interface->declare(&fHslider2, "tooltip", "Frequency of the sine wave for the modulation of theta (works if Modulation Type=3)");
		ui_interface->declare(&fHslider2, "unit", "Hz");
		ui_interface->addHorizontalSlider("Modulation_Frequency", &fHslider2, FAUSTFLOAT(2.2e+02f), FAUSTFLOAT(2e+01f), FAUSTFLOAT(1e+03f), FAUSTFLOAT(0.1f));
		ui_interface->declare(&fEntry1, "2", "");
		ui_interface->declare(&fEntry1, "tooltip", "0=theta is modulated by the incoming signal; 1=theta is modulated by the averaged incoming signal; 2=theta is modulated by the squared incoming signal; 3=theta is modulated by a sine wave of frequency freqMod; 4=theta is modulated by a sine wave of frequency freq;");
		ui_interface->addNumEntry("Modulation_Type", &fEntry1, FAUSTFLOAT(0.0f), FAUSTFLOAT(0.0f), FAUSTFLOAT(4.0f), FAUSTFLOAT(1.0f));
		ui_interface->declare(&fHslider1, "2", "");
		ui_interface->declare(&fHslider1, "tooltip", "Nonlinearity factor (value between 0 and 1)");
		ui_interface->addHorizontalSlider("Nonlinearity", &fHslider1, FAUSTFLOAT(0.0f), FAUSTFLOAT(0.0f), FAUSTFLOAT(1.0f), FAUSTFLOAT(0.01f));
		ui_interface->closeBox();
		ui_interface->closeBox();
		ui_interface->openVerticalBox("Spat");
		ui_interface->addHorizontalSlider("pan angle", &fHslider0, FAUSTFLOAT(0.6f), FAUSTFLOAT(0.0f), FAUSTFLOAT(1.0f), FAUSTFLOAT(0.01f));
		ui_interface->addHorizontalSlider("spatial width", &fHslider3, FAUSTFLOAT(0.5f), FAUSTFLOAT(0.0f), FAUSTFLOAT(1.0f), FAUSTFLOAT(0.01f));
		ui_interface->closeBox();
		ui_interface->closeBox();
	}
	
	void compute(int count, FAUSTFLOAT** RESTRICT inputs, FAUSTFLOAT** RESTRICT outputs) {
		FAUSTFLOAT* output0 = outputs[0];
		FAUSTFLOAT* output1 = outputs[1];
		float fSlow0 = float(fHslider0);
		float fSlow1 = 1.0f - fSlow0;
		float fSlow2 = float(fEntry0);
		float fSlow3 = float(int(17.31234f * (std::log(fSlow2) + -6.086775f) + 69.5f));
		float fSlow4 = getValueLoopFilterb0(fSlow3);
		float fSlow5 = float(fButton0);
		int iSlow6 = fSlow5 < 1.0f;
		float fSlow7 = 0.001f * (0.9996f * fSlow5 + 0.9f * float(iSlow6) * getValueReleaseLoopGain(fSlow3));
		float fSlow8 = float(fEntry1);
		float fSlow9 = float(fSlow8 >= 3.0f);
		float fSlow10 = 0.001f * float(fHslider1);
		float fSlow11 = fSlow2 * float(fSlow8 == 4.0f);
		float fSlow12 = float(fSlow8 != 4.0f);
		float fSlow13 = 0.001f * float(fHslider2);
		float fSlow14 = float(fSlow8 < 3.0f);
		float fSlow15 = 3.1415927f * float(fSlow8 == 0.0f);
		float fSlow16 = 1.5707964f * float(fSlow8 == 1.0f);
		float fSlow17 = 3.1415927f * float(fSlow8 == 2.0f);
		int iSlow18 = fSlow5 > 0.0f;
		float fSlow19 = std::exp(-(fConst2 / (float(fEntry2) * getValueDryTapAmpT60(fSlow3))));
		int iSlow20 = int(fConst0 / fSlow2) & 4095;
		float fSlow21 = getValueLoopFiltera1(fSlow3);
		float fSlow22 = getValueLoopFiltera2(fSlow3);
		float fSlow23 = getValueLoopFilterb1(fSlow3);
		float fSlow24 = getValueLoopFilterb2(fSlow3);
		int iSlow25 = int(fConst3 * (float(fHslider3) / fSlow2)) & 4095;
		for (int i0 = 0; i0 < count; i0 = i0 + 1) {
			fRec2[0] = fSlow7 + 0.999f * fRec2[1];
			float fTemp0 = fRec0[(IOTA0 - 1) & 4095];
			fRec3[0] = fSlow10 + 0.999f * fRec3[1];
			fRec6[0] = fSlow13 + 0.999f * fRec6[1];
			float fTemp1 = fRec5[1] + fConst1 * (fSlow11 + fSlow12 * fRec6[0]);
			fRec5[0] = fTemp1 - std::floor(fTemp1);
			float fTemp2 = 3.1415927f * fRec3[0] * ftbl0Harpsi_dspSIG0[std::max<int>(0, std::min<int>(int(65536.0f * fRec5[0]), 65535))];
			float fTemp3 = std::sin(fTemp2);
			float fTemp4 = std::cos(fTemp2);
			float fTemp5 = fTemp0 * fTemp4 - fTemp3 * fRec7[1];
			float fTemp6 = fTemp4 * fTemp5 - fTemp3 * fRec8[1];
			float fTemp7 = fTemp4 * fTemp6 - fTemp3 * fRec9[1];
			float fTemp8 = fTemp4 * fTemp7 - fTemp3 * fRec10[1];
			float fTemp9 = fTemp4 * fTemp8 - fTemp3 * fRec11[1];
			fRec12[0] = fTemp4 * fTemp9 - fTemp3 * fRec12[1];
			fRec11[0] = fTemp3 * fTemp9 + fTemp4 * fRec12[1];
			fRec10[0] = fTemp3 * fTemp8 + fTemp4 * fRec11[1];
			fRec9[0] = fTemp3 * fTemp7 + fTemp4 * fRec10[1];
			fRec8[0] = fTemp3 * fTemp6 + fTemp4 * fRec9[1];
			fRec7[0] = fTemp3 * fTemp5 + fTemp4 * fRec8[1];
			float fTemp10 = fRec3[0] * (fSlow15 * fTemp0 + fSlow16 * (fTemp0 + fRec0[(IOTA0 - 2) & 4095]) + fSlow17 * Harpsi_dsp_faustpower2_f(fTemp0));
			float fTemp11 = std::sin(fTemp10);
			float fTemp12 = std::cos(fTemp10);
			float fTemp13 = fTemp0 * fTemp12 - fTemp11 * fRec13[1];
			float fTemp14 = fTemp12 * fTemp13 - fTemp11 * fRec14[1];
			float fTemp15 = fTemp12 * fTemp14 - fTemp11 * fRec15[1];
			float fTemp16 = fTemp12 * fTemp15 - fTemp11 * fRec16[1];
			float fTemp17 = fTemp12 * fTemp16 - fTemp11 * fRec17[1];
			fRec18[0] = fTemp12 * fTemp17 - fTemp11 * fRec18[1];
			fRec17[0] = fTemp11 * fTemp17 + fTemp12 * fRec18[1];
			fRec16[0] = fTemp11 * fTemp16 + fTemp12 * fRec17[1];
			fRec15[0] = fTemp11 * fTemp15 + fTemp12 * fRec16[1];
			fRec14[0] = fTemp11 * fTemp14 + fTemp12 * fRec15[1];
			fRec13[0] = fTemp11 * fTemp13 + fTemp12 * fRec14[1];
			fRec20[0] = fSlow5 * fRec20[1] + 1.0f;
			float fTemp18 = fRec20[0] + -1.0f;
			float fTemp19 = float((fTemp18 < 2.0f) & iSlow18);
			float fTemp20 = 0.030197384f * fTemp19 + fSlow19 * float((fTemp18 >= 2.0f) | iSlow6);
			fRec19[0] = fRec19[1] * fTemp20 + 0.15f * fTemp19 * (1.0f - fTemp20);
			iRec21[0] = 1103515245 * iRec21[1] + 12345;
			fVec0[IOTA0 & 4095] = fRec2[0] * (fSlow9 * (fTemp0 * fTemp3 + fRec7[1] * fTemp4) + fSlow14 * (fRec3[0] * (fTemp0 * fTemp11 + fRec13[1] * fTemp12) + (1.0f - fRec3[0]) * fTemp0)) + 4.656613e-10f * fRec19[0] * float(iRec21[0]);
			fRec1[0] = fVec0[(IOTA0 - iSlow20) & 4095] - (fSlow21 * fRec1[1] + fSlow22 * fRec1[2]);
			fRec0[IOTA0 & 4095] = fSlow4 * fRec1[0] + fSlow23 * fRec1[1] + fSlow24 * fRec1[2];
			output0[i0] = FAUSTFLOAT(fSlow1 * fRec0[IOTA0 & 4095]);
			output1[i0] = FAUSTFLOAT(fSlow0 * fRec0[(IOTA0 - iSlow25) & 4095]);
			fRec2[1] = fRec2[0];
			IOTA0 = IOTA0 + 1;
			fRec3[1] = fRec3[0];
			fRec6[1] = fRec6[0];
			fRec5[1] = fRec5[0];
			fRec12[1] = fRec12[0];
			fRec11[1] = fRec11[0];
			fRec10[1] = fRec10[0];
			fRec9[1] = fRec9[0];
			fRec8[1] = fRec8[0];
			fRec7[1] = fRec7[0];
			fRec18[1] = fRec18[0];
			fRec17[1] = fRec17[0];
			fRec16[1] = fRec16[0];
			fRec15[1] = fRec15[0];
			fRec14[1] = fRec14[0];
			fRec13[1] = fRec13[0];
			fRec20[1] = fRec20[0];
			fRec19[1] = fRec19[0];
			iRec21[1] = iRec21[0];
			fRec1[2] = fRec1[1];
			fRec1[1] = fRec1[0];
		}
	}

};


#include "Faust_plugins_template2.cpp"


#endif
