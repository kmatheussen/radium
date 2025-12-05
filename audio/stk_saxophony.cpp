/* ------------------------------------------------------------
author: "Romain Michon"
copyright: "Romain Michon (rmichon@ccrma.stanford.edu)"
name: "Saxophone"
version: "1.0"
Code generated with Faust 2.81.2 (https://faust.grame.fr)
Compilation options: -a ../../Faust_plugins_template.cpp -lang cpp -nvi -ct 1 -cn Saxophony_dsp -es 1 -mcd 16 -mdd 1024 -mdy 33 -single -ftz 0
------------------------------------------------------------ */

#ifndef  __Saxophony_dsp_H__
#define  __Saxophony_dsp_H__


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
#define FAUSTCLASS Saxophony_dsp
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

class Saxophony_dspSIG0 {
	
  private:
	
	int iRec6[2];
	
  public:
	
	int getNumInputsSaxophony_dspSIG0() {
		return 0;
	}
	int getNumOutputsSaxophony_dspSIG0() {
		return 1;
	}
	
	void instanceInitSaxophony_dspSIG0(int sample_rate) {
		for (int l4 = 0; l4 < 2; l4 = l4 + 1) {
			iRec6[l4] = 0;
		}
	}
	
	void fillSaxophony_dspSIG0(int count, float* table) {
		for (int i1 = 0; i1 < count; i1 = i1 + 1) {
			iRec6[0] = iRec6[1] + 1;
			table[i1] = std::sin(9.58738e-05f * float(iRec6[0] + -1));
			iRec6[1] = iRec6[0];
		}
	}

};

static Saxophony_dspSIG0* newSaxophony_dspSIG0() { return (Saxophony_dspSIG0*)new Saxophony_dspSIG0(); }
static void deleteSaxophony_dspSIG0(Saxophony_dspSIG0* dsp) { delete dsp; }

static float *ftbl0Saxophony_dspSIG0; __attribute__((constructor)) static void initialize_ftbl0Saxophony_dspSIG0() { ftbl0Saxophony_dspSIG0 = (float*)calloc(65536, sizeof(float));};
static float Saxophony_dsp_faustpower2_f(float value) {
	return value * value;
}

class Saxophony_dsp final : public dsp {
	
 private:
	
	FAUSTFLOAT fEntry0;
	FAUSTFLOAT fHslider0;
	FAUSTFLOAT fHslider1;
	FAUSTFLOAT fButton0;
	float fRec4[2];
	int iRec2[2];
	int fSampleRate;
	float fConst0;
	FAUSTFLOAT fHslider2;
	FAUSTFLOAT fHslider3;
	float fRec3[2];
	FAUSTFLOAT fHslider4;
	int iRec5[2];
	FAUSTFLOAT fHslider5;
	float fConst1;
	FAUSTFLOAT fHslider6;
	float fRec7[2];
	FAUSTFLOAT fEntry1;
	FAUSTFLOAT fHslider7;
	FAUSTFLOAT fEntry2;
	int IOTA0;
	float fVec0[2];
	FAUSTFLOAT fHslider8;
	float fRec8[2];
	int iRec9[2];
	FAUSTFLOAT fHslider9;
	float fRec10[2];
	FAUSTFLOAT fHslider10;
	float fRec12[2];
	float fRec11[2];
	float fRec18[2];
	float fRec17[2];
	float fRec16[2];
	float fRec15[2];
	float fRec14[2];
	float fRec13[2];
	float fRec24[2];
	float fRec23[2];
	float fRec22[2];
	float fRec21[2];
	float fRec20[2];
	float fRec19[2];
	float fVec1[2];
	float fVec2[4096];
	FAUSTFLOAT fHslider11;
	float fRec0[8192];
	float fVec3[4096];
	float fConst2;
	FAUSTFLOAT fHslider12;
	
 public:
	Saxophony_dsp() {
	}
	
	void metadata(Meta* m) { 
		m->declare("author", "Romain Michon");
		m->declare("compile_options", "-a ../../Faust_plugins_template.cpp -lang cpp -nvi -ct 1 -cn Saxophony_dsp -es 1 -mcd 16 -mdd 1024 -mdy 33 -single -ftz 0");
		m->declare("copyright", "Romain Michon (rmichon@ccrma.stanford.edu)");
		m->declare("description", "Nonlinear WaveGuide Saxophone");
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
		m->declare("filename", "saxophony.dsp");
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
		m->declare("name", "Saxophone");
		m->declare("reference", "https://ccrma.stanford.edu/~jos/pasp/Woodwinds.html");
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
		Saxophony_dspSIG0* sig0 = newSaxophony_dspSIG0();
		sig0->instanceInitSaxophony_dspSIG0(sample_rate);
		sig0->fillSaxophony_dspSIG0(65536, ftbl0Saxophony_dspSIG0);
		deleteSaxophony_dspSIG0(sig0);
	}
	
	void instanceConstants(int sample_rate) {
		fSampleRate = sample_rate;
		fConst0 = std::min<float>(1.92e+05f, std::max<float>(1.0f, float(fSampleRate)));
		fConst1 = 1.0f / fConst0;
		fConst2 = 0.5f * fConst0;
	}
	
	void instanceResetUserInterface() {
		fEntry0 = FAUSTFLOAT(1.0f);
		fHslider0 = FAUSTFLOAT(0.6f);
		fHslider1 = FAUSTFLOAT(1.0f);
		fButton0 = FAUSTFLOAT(0.0f);
		fHslider2 = FAUSTFLOAT(0.05f);
		fHslider3 = FAUSTFLOAT(0.01f);
		fHslider4 = FAUSTFLOAT(0.05f);
		fHslider5 = FAUSTFLOAT(0.1f);
		fHslider6 = FAUSTFLOAT(6.0f);
		fEntry1 = FAUSTFLOAT(0.0f);
		fHslider7 = FAUSTFLOAT(0.5f);
		fEntry2 = FAUSTFLOAT(4.4e+02f);
		fHslider8 = FAUSTFLOAT(0.0f);
		fHslider9 = FAUSTFLOAT(0.1f);
		fHslider10 = FAUSTFLOAT(2.2e+02f);
		fHslider11 = FAUSTFLOAT(0.3f);
		fHslider12 = FAUSTFLOAT(0.5f);
	}
	
	void instanceClear() {
		for (int l0 = 0; l0 < 2; l0 = l0 + 1) {
			fRec4[l0] = 0.0f;
		}
		for (int l1 = 0; l1 < 2; l1 = l1 + 1) {
			iRec2[l1] = 0;
		}
		for (int l2 = 0; l2 < 2; l2 = l2 + 1) {
			fRec3[l2] = 0.0f;
		}
		for (int l3 = 0; l3 < 2; l3 = l3 + 1) {
			iRec5[l3] = 0;
		}
		for (int l5 = 0; l5 < 2; l5 = l5 + 1) {
			fRec7[l5] = 0.0f;
		}
		IOTA0 = 0;
		for (int l6 = 0; l6 < 2; l6 = l6 + 1) {
			fVec0[l6] = 0.0f;
		}
		for (int l7 = 0; l7 < 2; l7 = l7 + 1) {
			fRec8[l7] = 0.0f;
		}
		for (int l8 = 0; l8 < 2; l8 = l8 + 1) {
			iRec9[l8] = 0;
		}
		for (int l9 = 0; l9 < 2; l9 = l9 + 1) {
			fRec10[l9] = 0.0f;
		}
		for (int l10 = 0; l10 < 2; l10 = l10 + 1) {
			fRec12[l10] = 0.0f;
		}
		for (int l11 = 0; l11 < 2; l11 = l11 + 1) {
			fRec11[l11] = 0.0f;
		}
		for (int l12 = 0; l12 < 2; l12 = l12 + 1) {
			fRec18[l12] = 0.0f;
		}
		for (int l13 = 0; l13 < 2; l13 = l13 + 1) {
			fRec17[l13] = 0.0f;
		}
		for (int l14 = 0; l14 < 2; l14 = l14 + 1) {
			fRec16[l14] = 0.0f;
		}
		for (int l15 = 0; l15 < 2; l15 = l15 + 1) {
			fRec15[l15] = 0.0f;
		}
		for (int l16 = 0; l16 < 2; l16 = l16 + 1) {
			fRec14[l16] = 0.0f;
		}
		for (int l17 = 0; l17 < 2; l17 = l17 + 1) {
			fRec13[l17] = 0.0f;
		}
		for (int l18 = 0; l18 < 2; l18 = l18 + 1) {
			fRec24[l18] = 0.0f;
		}
		for (int l19 = 0; l19 < 2; l19 = l19 + 1) {
			fRec23[l19] = 0.0f;
		}
		for (int l20 = 0; l20 < 2; l20 = l20 + 1) {
			fRec22[l20] = 0.0f;
		}
		for (int l21 = 0; l21 < 2; l21 = l21 + 1) {
			fRec21[l21] = 0.0f;
		}
		for (int l22 = 0; l22 < 2; l22 = l22 + 1) {
			fRec20[l22] = 0.0f;
		}
		for (int l23 = 0; l23 < 2; l23 = l23 + 1) {
			fRec19[l23] = 0.0f;
		}
		for (int l24 = 0; l24 < 2; l24 = l24 + 1) {
			fVec1[l24] = 0.0f;
		}
		for (int l25 = 0; l25 < 4096; l25 = l25 + 1) {
			fVec2[l25] = 0.0f;
		}
		for (int l26 = 0; l26 < 8192; l26 = l26 + 1) {
			fRec0[l26] = 0.0f;
		}
		for (int l27 = 0; l27 < 4096; l27 = l27 + 1) {
			fVec3[l27] = 0.0f;
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
	
	Saxophony_dsp* clone() {
		return new Saxophony_dsp();
	}
	
	int getSampleRate() {
		return fSampleRate;
	}
	
	void buildUserInterface(UI* ui_interface) {
		ui_interface->openVerticalBox("Saxophone");
		ui_interface->openHorizontalBox("Basic_Parameters");
		ui_interface->declare(&fEntry2, "1", "");
		ui_interface->declare(&fEntry2, "tooltip", "Tone frequency");
		ui_interface->declare(&fEntry2, "unit", "Hz");
		ui_interface->addNumEntry("freq", &fEntry2, FAUSTFLOAT(4.4e+02f), FAUSTFLOAT(2e+01f), FAUSTFLOAT(2e+04f), FAUSTFLOAT(1.0f));
		ui_interface->declare(&fEntry0, "1", "");
		ui_interface->declare(&fEntry0, "tooltip", "Gain (value between 0 and 1)");
		ui_interface->addNumEntry("gain", &fEntry0, FAUSTFLOAT(1.0f), FAUSTFLOAT(0.0f), FAUSTFLOAT(1.0f), FAUSTFLOAT(0.01f));
		ui_interface->declare(&fButton0, "1", "");
		ui_interface->declare(&fButton0, "tooltip", "noteOn = 1, noteOff = 0");
		ui_interface->addButton("gate", &fButton0);
		ui_interface->closeBox();
		ui_interface->openHorizontalBox("Envelopes_and_Vibrato");
		ui_interface->openVerticalBox("Envelope_Parameters");
		ui_interface->declare(&fHslider2, "5", "");
		ui_interface->declare(&fHslider2, "tooltip", "Envelope attack duration");
		ui_interface->declare(&fHslider2, "unit", "s");
		ui_interface->addHorizontalSlider("Envelope_Attack", &fHslider2, FAUSTFLOAT(0.05f), FAUSTFLOAT(0.0f), FAUSTFLOAT(2.0f), FAUSTFLOAT(0.01f));
		ui_interface->declare(&fHslider3, "5", "");
		ui_interface->declare(&fHslider3, "tooltip", "Envelope release duration");
		ui_interface->declare(&fHslider3, "unit", "s");
		ui_interface->addHorizontalSlider("Envelope_Release", &fHslider3, FAUSTFLOAT(0.01f), FAUSTFLOAT(0.0f), FAUSTFLOAT(2.0f), FAUSTFLOAT(0.01f));
		ui_interface->closeBox();
		ui_interface->openVerticalBox("Vibrato_Parameters");
		ui_interface->declare(&fHslider6, "4", "");
		ui_interface->declare(&fHslider6, "unit", "Hz");
		ui_interface->addHorizontalSlider("Vibrato_Freq", &fHslider6, FAUSTFLOAT(6.0f), FAUSTFLOAT(1.0f), FAUSTFLOAT(15.0f), FAUSTFLOAT(0.1f));
		ui_interface->declare(&fHslider5, "4", "");
		ui_interface->declare(&fHslider5, "tooltip", "A value between 0 and 1");
		ui_interface->addHorizontalSlider("Vibrato_Gain", &fHslider5, FAUSTFLOAT(0.1f), FAUSTFLOAT(0.0f), FAUSTFLOAT(1.0f), FAUSTFLOAT(0.01f));
		ui_interface->closeBox();
		ui_interface->closeBox();
		ui_interface->openHorizontalBox("Physical_and_Nonlinearity");
		ui_interface->openVerticalBox("Nonlinear_Filter_Parameters");
		ui_interface->declare(&fHslider10, "3", "");
		ui_interface->declare(&fHslider10, "tooltip", "Frequency of the sine wave for the modulation of theta (works if Modulation Type=3)");
		ui_interface->declare(&fHslider10, "unit", "Hz");
		ui_interface->addHorizontalSlider("Modulation_Frequency", &fHslider10, FAUSTFLOAT(2.2e+02f), FAUSTFLOAT(2e+01f), FAUSTFLOAT(1e+03f), FAUSTFLOAT(0.1f));
		ui_interface->declare(&fEntry1, "3", "");
		ui_interface->declare(&fEntry1, "tooltip", "0=theta is modulated by the incoming signal; 1=theta is modulated by the averaged incoming signal; 2=theta is modulated by the squared incoming signal; 3=theta is modulated by a sine wave of frequency freqMod; 4=theta is modulated by a sine wave of frequency freq;");
		ui_interface->addNumEntry("Modulation_Type", &fEntry1, FAUSTFLOAT(0.0f), FAUSTFLOAT(0.0f), FAUSTFLOAT(4.0f), FAUSTFLOAT(1.0f));
		ui_interface->declare(&fHslider8, "3", "");
		ui_interface->declare(&fHslider8, "tooltip", "Nonlinearity factor (value between 0 and 1)");
		ui_interface->addHorizontalSlider("Nonlinearity", &fHslider8, FAUSTFLOAT(0.0f), FAUSTFLOAT(0.0f), FAUSTFLOAT(1.0f), FAUSTFLOAT(0.01f));
		ui_interface->declare(&fHslider9, "3", "");
		ui_interface->declare(&fHslider9, "Attack duration of the nonlinearity", "");
		ui_interface->declare(&fHslider9, "unit", "s");
		ui_interface->addHorizontalSlider("Nonlinearity_Attack", &fHslider9, FAUSTFLOAT(0.1f), FAUSTFLOAT(0.0f), FAUSTFLOAT(2.0f), FAUSTFLOAT(0.01f));
		ui_interface->closeBox();
		ui_interface->openVerticalBox("Physical_Parameters");
		ui_interface->declare(&fHslider7, "2", "");
		ui_interface->declare(&fHslider7, "tooltip", "A value between 0 and 1");
		ui_interface->addHorizontalSlider("Blow_Position", &fHslider7, FAUSTFLOAT(0.5f), FAUSTFLOAT(0.0f), FAUSTFLOAT(1.0f), FAUSTFLOAT(0.01f));
		ui_interface->addHorizontalSlider("Noise_Gain", &fHslider4, FAUSTFLOAT(0.05f), FAUSTFLOAT(0.0f), FAUSTFLOAT(1.0f), FAUSTFLOAT(0.01f));
		ui_interface->declare(&fHslider1, "2", "");
		ui_interface->declare(&fHslider1, "tooltip", "Breath pressure (a value between 0 and 1)");
		ui_interface->addHorizontalSlider("Pressure", &fHslider1, FAUSTFLOAT(1.0f), FAUSTFLOAT(0.0f), FAUSTFLOAT(1.0f), FAUSTFLOAT(0.01f));
		ui_interface->declare(&fHslider11, "2", "");
		ui_interface->declare(&fHslider11, "tooltip", "A value between 0 and 1");
		ui_interface->addHorizontalSlider("Reed_Stiffness", &fHslider11, FAUSTFLOAT(0.3f), FAUSTFLOAT(0.0f), FAUSTFLOAT(1.0f), FAUSTFLOAT(0.01f));
		ui_interface->closeBox();
		ui_interface->closeBox();
		ui_interface->openVerticalBox("Spat");
		ui_interface->addHorizontalSlider("pan angle", &fHslider0, FAUSTFLOAT(0.6f), FAUSTFLOAT(0.0f), FAUSTFLOAT(1.0f), FAUSTFLOAT(0.01f));
		ui_interface->addHorizontalSlider("spatial width", &fHslider12, FAUSTFLOAT(0.5f), FAUSTFLOAT(0.0f), FAUSTFLOAT(1.0f), FAUSTFLOAT(0.01f));
		ui_interface->closeBox();
		ui_interface->closeBox();
	}
	
	void compute(int count, FAUSTFLOAT** RESTRICT inputs, FAUSTFLOAT** RESTRICT outputs) {
		FAUSTFLOAT* output0 = outputs[0];
		FAUSTFLOAT* output1 = outputs[1];
		float fSlow0 = float(fEntry0);
		float fSlow1 = float(fHslider0);
		float fSlow2 = fSlow0 * (1.0f - fSlow1);
		float fSlow3 = float(fHslider1);
		float fSlow4 = 0.3f * fSlow3 + 0.55f;
		float fSlow5 = 0.001f * float(fButton0);
		float fSlow6 = fSlow3 * float(fHslider2);
		float fSlow7 = 1.0f / (fConst0 * fSlow6 + float(fSlow6 == 0.0f));
		float fSlow8 = float(fHslider3);
		float fSlow9 = fSlow3 * fSlow8;
		float fSlow10 = 1.0f - 1.0f / std::pow(1e+05f, 1.0f / (fConst0 * fSlow9 + float(fSlow9 == 0.0f)));
		float fSlow11 = 4.656613e-10f * float(fHslider4);
		float fSlow12 = float(fHslider5);
		float fSlow13 = fConst1 * float(fHslider6);
		float fSlow14 = float(fEntry1);
		float fSlow15 = float(fSlow14 >= 3.0f);
		float fSlow16 = float(fHslider7);
		float fSlow17 = float(fEntry2);
		float fSlow18 = fConst0 / fSlow17 + -3.0f;
		float fSlow19 = (1.0f - fSlow16) * fSlow18;
		int iSlow20 = int(fSlow19);
		float fSlow21 = float(iSlow20);
		float fSlow22 = fSlow21 + (1.0f - fSlow19);
		int iSlow23 = (iSlow20 & 4095) + 1;
		float fSlow24 = fSlow19 - fSlow21;
		int iSlow25 = ((iSlow20 + 1) & 4095) + 1;
		float fSlow26 = 0.001f * float(fHslider8);
		float fSlow27 = float(fHslider9);
		float fSlow28 = 1.0f / (fConst0 * fSlow27 + float(fSlow27 == 0.0f));
		float fSlow29 = 1.0f - 1.0f / std::pow(1e+05f, 1.0f / (fConst0 * fSlow8 + float(fSlow8 == 0.0f)));
		float fSlow30 = fSlow17 * float(fSlow14 == 4.0f);
		float fSlow31 = float(fSlow14 != 4.0f);
		float fSlow32 = 0.001f * float(fHslider10);
		float fSlow33 = float(fSlow14 < 3.0f);
		float fSlow34 = 3.1415927f * float(fSlow14 == 0.0f);
		float fSlow35 = 1.5707964f * float(fSlow14 == 1.0f);
		float fSlow36 = 3.1415927f * float(fSlow14 == 2.0f);
		float fSlow37 = fSlow16 * fSlow18;
		int iSlow38 = int(fSlow37 + 1.0f);
		float fSlow39 = float(iSlow38);
		float fSlow40 = fSlow39 - fSlow37;
		int iSlow41 = iSlow38 & 4095;
		float fSlow42 = fSlow37 + (1.0f - fSlow39);
		int iSlow43 = (iSlow38 + 1) & 4095;
		float fSlow44 = 0.4f * float(fHslider11) + 0.1f;
		int iSlow45 = int(fConst2 * (float(fHslider12) / fSlow17)) & 4095;
		for (int i0 = 0; i0 < count; i0 = i0 + 1) {
			fRec4[0] = fSlow5 + 0.999f * fRec4[1];
			int iTemp0 = fRec4[0] > 0.0f;
			iRec2[0] = iTemp0 & (iRec2[1] | (fRec3[1] >= 1.0f));
			int iTemp1 = fRec4[0] <= 0.0f;
			int iTemp2 = iTemp1 & (fRec3[1] > 0.0f);
			fRec3[0] = (fSlow7 * float(((iRec2[1] == 0) & iTemp0) & (fRec3[1] < 1.0f)) + fRec3[1] * (1.0f - fSlow10 * float(iTemp2))) * float((iTemp2 == 0) | (fRec3[1] >= 1e-06f));
			iRec5[0] = 1103515245 * iRec5[1] + 12345;
			fRec7[0] = fSlow13 + (fRec7[1] - std::floor(fSlow13 + fRec7[1]));
			float fTemp3 = fSlow4 * fRec3[0] * (fSlow11 * float(iRec5[0]) + 1.0f) * (fSlow12 * ftbl0Saxophony_dspSIG0[std::max<int>(0, std::min<int>(int(65536.0f * fRec7[0]), 65535))] + 1.0f);
			float fTemp4 = fSlow22 * fRec0[(IOTA0 - iSlow23) & 8191] + fSlow24 * fRec0[(IOTA0 - iSlow25) & 8191];
			fVec0[0] = fTemp4;
			fRec8[0] = fSlow26 + 0.999f * fRec8[1];
			iRec9[0] = iTemp0 & (iRec9[1] | (fRec10[1] >= 1.0f));
			int iTemp5 = iTemp1 & (fRec10[1] > 0.0f);
			fRec10[0] = (fSlow28 * float(((iRec9[1] == 0) & iTemp0) & (fRec10[1] < 1.0f)) + fRec10[1] * (1.0f - fSlow29 * float(iTemp5))) * float((iTemp5 == 0) | (fRec10[1] >= 1e-06f));
			float fTemp6 = fRec8[0] * fRec10[0];
			fRec12[0] = fSlow32 + 0.999f * fRec12[1];
			float fTemp7 = fRec11[1] + fConst1 * (fSlow30 + fSlow31 * fRec12[0]);
			fRec11[0] = fTemp7 - std::floor(fTemp7);
			float fTemp8 = 3.1415927f * fTemp6 * ftbl0Saxophony_dspSIG0[std::max<int>(0, std::min<int>(int(65536.0f * fRec11[0]), 65535))];
			float fTemp9 = std::sin(fTemp8);
			float fTemp10 = std::cos(fTemp8);
			float fTemp11 = fTemp4 * fTemp10 - fTemp9 * fRec13[1];
			float fTemp12 = fTemp10 * fTemp11 - fTemp9 * fRec14[1];
			float fTemp13 = fTemp10 * fTemp12 - fTemp9 * fRec15[1];
			float fTemp14 = fTemp10 * fTemp13 - fTemp9 * fRec16[1];
			float fTemp15 = fTemp10 * fTemp14 - fTemp9 * fRec17[1];
			fRec18[0] = fTemp10 * fTemp15 - fTemp9 * fRec18[1];
			fRec17[0] = fTemp9 * fTemp15 + fTemp10 * fRec18[1];
			fRec16[0] = fTemp9 * fTemp14 + fTemp10 * fRec17[1];
			fRec15[0] = fTemp9 * fTemp13 + fTemp10 * fRec16[1];
			fRec14[0] = fTemp9 * fTemp12 + fTemp10 * fRec15[1];
			fRec13[0] = fTemp9 * fTemp11 + fTemp10 * fRec14[1];
			float fTemp16 = fTemp6 * (fSlow34 * fTemp4 + fSlow35 * (fTemp4 + fVec0[1]) + fSlow36 * Saxophony_dsp_faustpower2_f(fTemp4));
			float fTemp17 = std::sin(fTemp16);
			float fTemp18 = std::cos(fTemp16);
			float fTemp19 = fTemp4 * fTemp18 - fTemp17 * fRec19[1];
			float fTemp20 = fTemp18 * fTemp19 - fTemp17 * fRec20[1];
			float fTemp21 = fTemp18 * fTemp20 - fTemp17 * fRec21[1];
			float fTemp22 = fTemp18 * fTemp21 - fTemp17 * fRec22[1];
			float fTemp23 = fTemp18 * fTemp22 - fTemp17 * fRec23[1];
			fRec24[0] = fTemp18 * fTemp23 - fTemp17 * fRec24[1];
			fRec23[0] = fTemp17 * fTemp23 + fTemp18 * fRec24[1];
			fRec22[0] = fTemp17 * fTemp22 + fTemp18 * fRec23[1];
			fRec21[0] = fTemp17 * fTemp21 + fTemp18 * fRec22[1];
			fRec20[0] = fTemp17 * fTemp20 + fTemp18 * fRec21[1];
			fRec19[0] = fTemp17 * fTemp19 + fTemp18 * fRec20[1];
			float fTemp24 = fSlow15 * (fTemp4 * fTemp9 + fRec13[1] * fTemp10) + fSlow33 * (fRec8[0] * (fTemp4 * fTemp17 + fRec19[1] * fTemp18) + (1.0f - fRec8[0]) * fTemp4);
			fVec1[0] = fTemp24;
			float fTemp25 = fTemp24 + fVec1[1];
			fVec2[IOTA0 & 4095] = fTemp25;
			float fTemp26 = 0.475f * (fTemp25 - (fSlow40 * fVec2[(IOTA0 - iSlow41) & 4095] + fSlow42 * fVec2[(IOTA0 - iSlow43) & 4095]));
			float fTemp27 = fTemp3 + fTemp26;
			float fTemp28 = fSlow44 * fTemp27 + 0.7f;
			float fTemp29 = float(fTemp28 > 1.0f) + fTemp28 * float(fTemp28 <= 1.0f);
			fRec0[IOTA0 & 8191] = fTemp3 + 0.475f * fTemp25 - fTemp27 * (float(-(fTemp29 < -1.0f)) + fTemp29 * float(fTemp29 >= -1.0f));
			float fRec1 = -fTemp26;
			output0[i0] = FAUSTFLOAT(fSlow2 * fRec1);
			fVec3[IOTA0 & 4095] = fSlow0 * fRec1;
			output1[i0] = FAUSTFLOAT(fSlow1 * fVec3[(IOTA0 - iSlow45) & 4095]);
			fRec4[1] = fRec4[0];
			iRec2[1] = iRec2[0];
			fRec3[1] = fRec3[0];
			iRec5[1] = iRec5[0];
			fRec7[1] = fRec7[0];
			IOTA0 = IOTA0 + 1;
			fVec0[1] = fVec0[0];
			fRec8[1] = fRec8[0];
			iRec9[1] = iRec9[0];
			fRec10[1] = fRec10[0];
			fRec12[1] = fRec12[0];
			fRec11[1] = fRec11[0];
			fRec18[1] = fRec18[0];
			fRec17[1] = fRec17[0];
			fRec16[1] = fRec16[0];
			fRec15[1] = fRec15[0];
			fRec14[1] = fRec14[0];
			fRec13[1] = fRec13[0];
			fRec24[1] = fRec24[0];
			fRec23[1] = fRec23[0];
			fRec22[1] = fRec22[0];
			fRec21[1] = fRec21[0];
			fRec20[1] = fRec20[0];
			fRec19[1] = fRec19[0];
			fVec1[1] = fVec1[0];
		}
	}

};


#include "Faust_plugins_template2.cpp"


#endif
