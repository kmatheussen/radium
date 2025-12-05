/* ------------------------------------------------------------
author: "Romain Michon"
copyright: "Romain Michon (rmichon@ccrma.stanford.edu)"
name: "BlowHole"
version: "1.0"
Code generated with Faust 2.81.2 (https://faust.grame.fr)
Compilation options: -a ../../Faust_plugins_template.cpp -lang cpp -nvi -ct 1 -cn Blow_Hole_dsp -es 1 -mcd 16 -mdd 1024 -mdy 33 -single -ftz 0
------------------------------------------------------------ */

#ifndef  __Blow_Hole_dsp_H__
#define  __Blow_Hole_dsp_H__


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
#define FAUSTCLASS Blow_Hole_dsp
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

class Blow_Hole_dspSIG0 {
	
  private:
	
	int iRec20[2];
	
  public:
	
	int getNumInputsBlow_Hole_dspSIG0() {
		return 0;
	}
	int getNumOutputsBlow_Hole_dspSIG0() {
		return 1;
	}
	
	void instanceInitBlow_Hole_dspSIG0(int sample_rate) {
		for (int l12 = 0; l12 < 2; l12 = l12 + 1) {
			iRec20[l12] = 0;
		}
	}
	
	void fillBlow_Hole_dspSIG0(int count, float* table) {
		for (int i1 = 0; i1 < count; i1 = i1 + 1) {
			iRec20[0] = iRec20[1] + 1;
			table[i1] = std::sin(9.58738e-05f * float(iRec20[0] + -1));
			iRec20[1] = iRec20[0];
		}
	}

};

static Blow_Hole_dspSIG0* newBlow_Hole_dspSIG0() { return (Blow_Hole_dspSIG0*)new Blow_Hole_dspSIG0(); }
static void deleteBlow_Hole_dspSIG0(Blow_Hole_dspSIG0* dsp) { delete dsp; }

static float Blow_Hole_dsp_faustpower2_f(float value) {
	return value * value;
}
static float *ftbl0Blow_Hole_dspSIG0; __attribute__((constructor)) static void initialize_ftbl0Blow_Hole_dspSIG0() { ftbl0Blow_Hole_dspSIG0 = (float*)calloc(65536, sizeof(float));};

class Blow_Hole_dsp final : public dsp {
	
 private:
	
	FAUSTFLOAT fHslider0;
	FAUSTFLOAT fEntry0;
	FAUSTFLOAT fHslider1;
	float fRec8[2];
	int fSampleRate;
	float fConst0;
	FAUSTFLOAT fEntry1;
	int IOTA0;
	float fVec0[2];
	FAUSTFLOAT fButton0;
	int iRec9[2];
	FAUSTFLOAT fHslider2;
	FAUSTFLOAT fHslider3;
	float fRec10[2];
	float fRec12[2];
	float fRec11[2];
	float fConst1;
	float fConst2;
	float fConst3;
	FAUSTFLOAT fHslider4;
	FAUSTFLOAT fHslider5;
	int iRec14[2];
	FAUSTFLOAT fHslider6;
	float fRec15[2];
	FAUSTFLOAT fHslider7;
	int iRec16[2];
	FAUSTFLOAT fHslider8;
	int iRec17[2];
	int iRec18[2];
	FAUSTFLOAT fHslider9;
	float fConst4;
	float fConst5;
	FAUSTFLOAT fHslider10;
	float fRec19[2];
	float fConst6;
	FAUSTFLOAT fHslider11;
	float fRec21[2];
	float fConst7;
	int iConst8;
	float fConst9;
	float fConst10;
	int iConst11;
	float fConst12;
	int iConst13;
	FAUSTFLOAT fHslider12;
	FAUSTFLOAT fHslider13;
	float fRec23[2];
	float fRec22[2];
	float fRec25[2];
	float fRec24[2];
	float fVec1[2];
	float fConst14;
	float fRec13[2];
	float fRec5[64];
	float fConst15;
	float fConst16;
	FAUSTFLOAT fHslider14;
	float fConst17;
	int iConst18;
	float fConst19;
	float fConst20;
	int iConst21;
	float fConst22;
	int iConst23;
	float fVec2[2];
	float fRec30[2];
	float fRec29[2];
	float fRec26[2];
	float fRec4[2];
	float fRec2[64];
	float fRec0[8192];
	FAUSTFLOAT fEntry2;
	float fRec31[2];
	float fVec3[4096];
	float fConst24;
	FAUSTFLOAT fHslider15;
	
 public:
	Blow_Hole_dsp() {
	}
	
	void metadata(Meta* m) { 
		m->declare("author", "Romain Michon");
		m->declare("compile_options", "-a ../../Faust_plugins_template.cpp -lang cpp -nvi -ct 1 -cn Blow_Hole_dsp -es 1 -mcd 16 -mdd 1024 -mdy 33 -single -ftz 0");
		m->declare("copyright", "Romain Michon (rmichon@ccrma.stanford.edu)");
		m->declare("description", "Nonlinear WaveGuide Clarinet with one register hole and one tonehole");
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
		m->declare("filename", "blowHole.dsp");
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
		m->declare("name", "BlowHole");
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
		Blow_Hole_dspSIG0* sig0 = newBlow_Hole_dspSIG0();
		sig0->instanceInitBlow_Hole_dspSIG0(sample_rate);
		sig0->fillBlow_Hole_dspSIG0(65536, ftbl0Blow_Hole_dspSIG0);
		deleteBlow_Hole_dspSIG0(sig0);
	}
	
	void instanceConstants(int sample_rate) {
		fSampleRate = sample_rate;
		fConst0 = std::min<float>(1.92e+05f, std::max<float>(1.0f, float(fSampleRate)));
		fConst1 = 0.21f * fConst0;
		fConst2 = fConst1 + 347.23f;
		fConst3 = 347.23f / fConst2;
		fConst4 = 1.8f * fConst0;
		fConst5 = 0.2f * fConst0;
		fConst6 = 1.0f / fConst0;
		fConst7 = 0.00022675737f * fConst0;
		iConst8 = int(fConst7);
		fConst9 = float(iConst8);
		fConst10 = fConst9 + (1.0f - fConst7);
		iConst11 = (iConst8 & 4095) + 1;
		fConst12 = fConst7 - fConst9;
		iConst13 = ((iConst8 + 1) & 4095) + 1;
		fConst14 = (347.23f - fConst1) / fConst2;
		fConst15 = 0.0084f * fConst0;
		fConst16 = (fConst15 + -347.23f) / (fConst15 + 347.23f) + -0.9995f;
		fConst17 = 0.00018140589f * fConst0;
		iConst18 = int(fConst17);
		fConst19 = float(iConst18);
		fConst20 = fConst17 - fConst19;
		iConst21 = ((iConst18 + 1) & 4095) + 1;
		fConst22 = fConst19 + (1.0f - fConst17);
		iConst23 = (iConst18 & 4095) + 1;
		fConst24 = 0.5f * fConst0;
	}
	
	void instanceResetUserInterface() {
		fHslider0 = FAUSTFLOAT(0.6f);
		fEntry0 = FAUSTFLOAT(0.0f);
		fHslider1 = FAUSTFLOAT(0.0f);
		fEntry1 = FAUSTFLOAT(4.4e+02f);
		fButton0 = FAUSTFLOAT(0.0f);
		fHslider2 = FAUSTFLOAT(0.1f);
		fHslider3 = FAUSTFLOAT(0.1f);
		fHslider4 = FAUSTFLOAT(0.0f);
		fHslider5 = FAUSTFLOAT(0.35f);
		fHslider6 = FAUSTFLOAT(0.01f);
		fHslider7 = FAUSTFLOAT(0.0f);
		fHslider8 = FAUSTFLOAT(0.1f);
		fHslider9 = FAUSTFLOAT(0.5f);
		fHslider10 = FAUSTFLOAT(0.01f);
		fHslider11 = FAUSTFLOAT(5.0f);
		fHslider12 = FAUSTFLOAT(0.35f);
		fHslider13 = FAUSTFLOAT(2.2e+02f);
		fHslider14 = FAUSTFLOAT(0.12f);
		fEntry2 = FAUSTFLOAT(1.0f);
		fHslider15 = FAUSTFLOAT(0.5f);
	}
	
	void instanceClear() {
		for (int l0 = 0; l0 < 2; l0 = l0 + 1) {
			fRec8[l0] = 0.0f;
		}
		IOTA0 = 0;
		for (int l1 = 0; l1 < 2; l1 = l1 + 1) {
			fVec0[l1] = 0.0f;
		}
		for (int l2 = 0; l2 < 2; l2 = l2 + 1) {
			iRec9[l2] = 0;
		}
		for (int l3 = 0; l3 < 2; l3 = l3 + 1) {
			fRec10[l3] = 0.0f;
		}
		for (int l4 = 0; l4 < 2; l4 = l4 + 1) {
			fRec12[l4] = 0.0f;
		}
		for (int l5 = 0; l5 < 2; l5 = l5 + 1) {
			fRec11[l5] = 0.0f;
		}
		for (int l6 = 0; l6 < 2; l6 = l6 + 1) {
			iRec14[l6] = 0;
		}
		for (int l7 = 0; l7 < 2; l7 = l7 + 1) {
			fRec15[l7] = 0.0f;
		}
		for (int l8 = 0; l8 < 2; l8 = l8 + 1) {
			iRec16[l8] = 0;
		}
		for (int l9 = 0; l9 < 2; l9 = l9 + 1) {
			iRec17[l9] = 0;
		}
		for (int l10 = 0; l10 < 2; l10 = l10 + 1) {
			iRec18[l10] = 0;
		}
		for (int l11 = 0; l11 < 2; l11 = l11 + 1) {
			fRec19[l11] = 0.0f;
		}
		for (int l13 = 0; l13 < 2; l13 = l13 + 1) {
			fRec21[l13] = 0.0f;
		}
		for (int l14 = 0; l14 < 2; l14 = l14 + 1) {
			fRec23[l14] = 0.0f;
		}
		for (int l15 = 0; l15 < 2; l15 = l15 + 1) {
			fRec22[l15] = 0.0f;
		}
		for (int l16 = 0; l16 < 2; l16 = l16 + 1) {
			fRec25[l16] = 0.0f;
		}
		for (int l17 = 0; l17 < 2; l17 = l17 + 1) {
			fRec24[l17] = 0.0f;
		}
		for (int l18 = 0; l18 < 2; l18 = l18 + 1) {
			fVec1[l18] = 0.0f;
		}
		for (int l19 = 0; l19 < 2; l19 = l19 + 1) {
			fRec13[l19] = 0.0f;
		}
		for (int l20 = 0; l20 < 64; l20 = l20 + 1) {
			fRec5[l20] = 0.0f;
		}
		for (int l21 = 0; l21 < 2; l21 = l21 + 1) {
			fVec2[l21] = 0.0f;
		}
		for (int l22 = 0; l22 < 2; l22 = l22 + 1) {
			fRec30[l22] = 0.0f;
		}
		for (int l23 = 0; l23 < 2; l23 = l23 + 1) {
			fRec29[l23] = 0.0f;
		}
		for (int l24 = 0; l24 < 2; l24 = l24 + 1) {
			fRec26[l24] = 0.0f;
		}
		for (int l25 = 0; l25 < 2; l25 = l25 + 1) {
			fRec4[l25] = 0.0f;
		}
		for (int l26 = 0; l26 < 64; l26 = l26 + 1) {
			fRec2[l26] = 0.0f;
		}
		for (int l27 = 0; l27 < 8192; l27 = l27 + 1) {
			fRec0[l27] = 0.0f;
		}
		for (int l28 = 0; l28 < 2; l28 = l28 + 1) {
			fRec31[l28] = 0.0f;
		}
		for (int l29 = 0; l29 < 4096; l29 = l29 + 1) {
			fVec3[l29] = 0.0f;
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
	
	Blow_Hole_dsp* clone() {
		return new Blow_Hole_dsp();
	}
	
	int getSampleRate() {
		return fSampleRate;
	}
	
	void buildUserInterface(UI* ui_interface) {
		ui_interface->openVerticalBox("BlowHole");
		ui_interface->openHorizontalBox("Basic_Parameters");
		ui_interface->declare(&fEntry1, "1", "");
		ui_interface->declare(&fEntry1, "tooltip", "Tone frequency");
		ui_interface->declare(&fEntry1, "unit", "Hz");
		ui_interface->addNumEntry("freq", &fEntry1, FAUSTFLOAT(4.4e+02f), FAUSTFLOAT(2e+01f), FAUSTFLOAT(2e+04f), FAUSTFLOAT(1.0f));
		ui_interface->declare(&fEntry2, "1", "");
		ui_interface->declare(&fEntry2, "tooltip", "Gain (value between 0 and 1)");
		ui_interface->addNumEntry("gain", &fEntry2, FAUSTFLOAT(1.0f), FAUSTFLOAT(0.0f), FAUSTFLOAT(1.0f), FAUSTFLOAT(0.01f));
		ui_interface->declare(&fButton0, "1", "");
		ui_interface->declare(&fButton0, "tooltip", "noteOn = 1, noteOff = 0");
		ui_interface->addButton("gate", &fButton0);
		ui_interface->closeBox();
		ui_interface->openHorizontalBox("Envelopes_and_Vibrato");
		ui_interface->openVerticalBox("Envelope_Parameters");
		ui_interface->declare(&fHslider6, "5", "");
		ui_interface->declare(&fHslider6, "tooltip", "Envelope attack duration");
		ui_interface->declare(&fHslider6, "unit", "s");
		ui_interface->addHorizontalSlider("Envelope_Attack", &fHslider6, FAUSTFLOAT(0.01f), FAUSTFLOAT(0.0f), FAUSTFLOAT(2.0f), FAUSTFLOAT(0.01f));
		ui_interface->declare(&fHslider3, "5", "");
		ui_interface->declare(&fHslider3, "tooltip", "Envelope release duration");
		ui_interface->declare(&fHslider3, "unit", "s");
		ui_interface->addHorizontalSlider("Envelope_Release", &fHslider3, FAUSTFLOAT(0.1f), FAUSTFLOAT(0.0f), FAUSTFLOAT(2.0f), FAUSTFLOAT(0.01f));
		ui_interface->closeBox();
		ui_interface->openVerticalBox("Vibrato_Parameters");
		ui_interface->declare(&fHslider9, "4", "");
		ui_interface->declare(&fHslider9, "tooltip", "Vibrato attack duration");
		ui_interface->declare(&fHslider9, "unit", "s");
		ui_interface->addHorizontalSlider("Vibrato_Attack", &fHslider9, FAUSTFLOAT(0.5f), FAUSTFLOAT(0.0f), FAUSTFLOAT(2.0f), FAUSTFLOAT(0.01f));
		ui_interface->declare(&fHslider11, "4", "");
		ui_interface->declare(&fHslider11, "unit", "Hz");
		ui_interface->addHorizontalSlider("Vibrato_Freq", &fHslider11, FAUSTFLOAT(5.0f), FAUSTFLOAT(1.0f), FAUSTFLOAT(15.0f), FAUSTFLOAT(0.1f));
		ui_interface->declare(&fHslider8, "4", "");
		ui_interface->declare(&fHslider8, "tooltip", "A value between 0 and 1");
		ui_interface->addHorizontalSlider("Vibrato_Gain", &fHslider8, FAUSTFLOAT(0.1f), FAUSTFLOAT(0.0f), FAUSTFLOAT(1.0f), FAUSTFLOAT(0.01f));
		ui_interface->declare(&fHslider10, "4", "");
		ui_interface->declare(&fHslider10, "tooltip", "Vibrato release duration");
		ui_interface->declare(&fHslider10, "unit", "s");
		ui_interface->addHorizontalSlider("Vibrato_Release", &fHslider10, FAUSTFLOAT(0.01f), FAUSTFLOAT(0.0f), FAUSTFLOAT(2.0f), FAUSTFLOAT(0.01f));
		ui_interface->closeBox();
		ui_interface->closeBox();
		ui_interface->openHorizontalBox("Physical_and_Nonlinearity");
		ui_interface->openVerticalBox("Nonlinear_Filter_Parameters");
		ui_interface->declare(&fHslider13, "3", "");
		ui_interface->declare(&fHslider13, "tooltip", "Frequency of the sine wave for the modulation of theta (works if Modulation Type=3)");
		ui_interface->declare(&fHslider13, "unit", "Hz");
		ui_interface->addHorizontalSlider("Modulation_Frequency", &fHslider13, FAUSTFLOAT(2.2e+02f), FAUSTFLOAT(2e+01f), FAUSTFLOAT(1e+03f), FAUSTFLOAT(0.1f));
		ui_interface->declare(&fEntry0, "3", "");
		ui_interface->declare(&fEntry0, "tooltip", "0=theta is modulated by the incoming signal; 1=theta is modulated by the averaged incoming signal; 2=theta is modulated by the squared incoming signal; 3=theta is modulated by a sine wave of frequency freqMod; 4=theta is modulated by a sine wave of frequency freq;");
		ui_interface->addNumEntry("Modulation_Type", &fEntry0, FAUSTFLOAT(0.0f), FAUSTFLOAT(0.0f), FAUSTFLOAT(4.0f), FAUSTFLOAT(1.0f));
		ui_interface->declare(&fHslider1, "3", "");
		ui_interface->declare(&fHslider1, "tooltip", "Nonlinearity factor (value between 0 and 1)");
		ui_interface->addHorizontalSlider("Nonlinearity", &fHslider1, FAUSTFLOAT(0.0f), FAUSTFLOAT(0.0f), FAUSTFLOAT(1.0f), FAUSTFLOAT(0.01f));
		ui_interface->declare(&fHslider2, "3", "");
		ui_interface->declare(&fHslider2, "Attack duration of the nonlinearity", "");
		ui_interface->declare(&fHslider2, "unit", "s");
		ui_interface->addHorizontalSlider("Nonlinearity_Attack", &fHslider2, FAUSTFLOAT(0.1f), FAUSTFLOAT(0.0f), FAUSTFLOAT(2.0f), FAUSTFLOAT(0.01f));
		ui_interface->closeBox();
		ui_interface->openVerticalBox("Physical_Parameters");
		ui_interface->declare(&fHslider7, "2", "");
		ui_interface->declare(&fHslider7, "tooltip", "Breath noise gain (value between 0 and 1)");
		ui_interface->addHorizontalSlider("Noise_Gain", &fHslider7, FAUSTFLOAT(0.0f), FAUSTFLOAT(0.0f), FAUSTFLOAT(1.0f), FAUSTFLOAT(0.01f));
		ui_interface->declare(&fHslider5, "2", "");
		ui_interface->declare(&fHslider5, "tooltip", "Breath pressure (value bewteen 0 and 1)");
		ui_interface->addHorizontalSlider("Pressure", &fHslider5, FAUSTFLOAT(0.35f), FAUSTFLOAT(0.0f), FAUSTFLOAT(1.0f), FAUSTFLOAT(0.01f));
		ui_interface->declare(&fHslider12, "2", "");
		ui_interface->declare(&fHslider12, "tooltip", "Reed stiffness (value between 0 and 1)");
		ui_interface->addHorizontalSlider("Reed_Stiffness", &fHslider12, FAUSTFLOAT(0.35f), FAUSTFLOAT(0.0f), FAUSTFLOAT(1.0f), FAUSTFLOAT(0.01f));
		ui_interface->declare(&fHslider14, "2", "");
		ui_interface->declare(&fHslider14, "tooltip", "A value between 0 and 1");
		ui_interface->addHorizontalSlider("Tone_Hole_Openness", &fHslider14, FAUSTFLOAT(0.12f), FAUSTFLOAT(0.0f), FAUSTFLOAT(1.0f), FAUSTFLOAT(0.01f));
		ui_interface->declare(&fHslider4, "2", "");
		ui_interface->declare(&fHslider4, "tooltip", "A value between 0 and 1");
		ui_interface->addHorizontalSlider("Vent_Openness", &fHslider4, FAUSTFLOAT(0.0f), FAUSTFLOAT(0.0f), FAUSTFLOAT(1.0f), FAUSTFLOAT(0.01f));
		ui_interface->closeBox();
		ui_interface->closeBox();
		ui_interface->openVerticalBox("Spat");
		ui_interface->addHorizontalSlider("pan angle", &fHslider0, FAUSTFLOAT(0.6f), FAUSTFLOAT(0.0f), FAUSTFLOAT(1.0f), FAUSTFLOAT(0.01f));
		ui_interface->addHorizontalSlider("spatial width", &fHslider15, FAUSTFLOAT(0.5f), FAUSTFLOAT(0.0f), FAUSTFLOAT(1.0f), FAUSTFLOAT(0.01f));
		ui_interface->closeBox();
		ui_interface->closeBox();
	}
	
	void compute(int count, FAUSTFLOAT** RESTRICT inputs, FAUSTFLOAT** RESTRICT outputs) {
		FAUSTFLOAT* output0 = outputs[0];
		FAUSTFLOAT* output1 = outputs[1];
		float fSlow0 = float(fHslider0);
		float fSlow1 = 1.5f * (1.0f - fSlow0);
		float fSlow2 = float(fEntry0);
		float fSlow3 = float(fSlow2 < 3.0f);
		float fSlow4 = float(fHslider1);
		float fSlow5 = 0.001f * fSlow4;
		float fSlow6 = 2.0f * fSlow4 * float(fSlow2 < 2.0f);
		float fSlow7 = float(fEntry1);
		float fSlow8 = fConst0 * (0.5f / fSlow7 + -0.00040816327f);
		int iSlow9 = int(fSlow8 + (-3.5f - fSlow6));
		float fSlow10 = fSlow6 + float(iSlow9);
		float fSlow11 = fSlow10 + (4.5f - fSlow8);
		int iSlow12 = (iSlow9 & 4095) + 1;
		float fSlow13 = fSlow8 + (-3.5f - fSlow10);
		int iSlow14 = ((iSlow9 + 1) & 4095) + 1;
		float fSlow15 = float(fButton0);
		int iSlow16 = fSlow15 > 0.0f;
		float fSlow17 = float(fHslider2);
		float fSlow18 = 1.0f / (fConst0 * fSlow17 + float(fSlow17 == 0.0f));
		float fSlow19 = float(fHslider3);
		float fSlow20 = 1.0f - 1.0f / std::pow(1e+05f, 1.0f / (fConst0 * fSlow19 + float(fSlow19 == 0.0f)));
		int iSlow21 = fSlow15 <= 0.0f;
		float fSlow22 = 3.1415927f * float(fSlow2 == 0.0f);
		float fSlow23 = 1.5707964f * float(fSlow2 == 1.0f);
		float fSlow24 = 3.1415927f * float(fSlow2 == 2.0f);
		float fSlow25 = fConst3 * float(fHslider4);
		float fSlow26 = float(fHslider5);
		float fSlow27 = 0.3f * fSlow26 + 0.55f;
		float fSlow28 = fSlow26 * float(fHslider6);
		float fSlow29 = 1.0f / (fConst0 * fSlow28 + float(fSlow28 == 0.0f));
		float fSlow30 = fSlow19 * fSlow26;
		float fSlow31 = 1.0f - 1.0f / std::pow(1e+05f, 1.0f / (fConst0 * fSlow30 + float(fSlow30 == 0.0f)));
		float fSlow32 = 4.656613e-10f * float(fHslider7);
		float fSlow33 = float(fHslider8);
		float fSlow34 = float(fHslider9);
		float fSlow35 = 1.0f / (float((1.8f * fSlow34) == 0.0f) + fConst4 * fSlow34);
		float fSlow36 = fConst5 * fSlow34;
		float fSlow37 = float((0.2f * fSlow34) == 0.0f) + fSlow36;
		float fSlow38 = float(fHslider10);
		float fSlow39 = 1.0f - 1.0f / std::pow(1e+05f, 1.0f / (fConst0 * fSlow38 + float(fSlow38 == 0.0f)));
		float fSlow40 = fConst6 * float(fHslider11);
		float fSlow41 = 0.26f * float(fHslider12) + -0.44f;
		float fSlow42 = float(fSlow2 >= 3.0f);
		float fSlow43 = fSlow7 * float(fSlow2 == 4.0f);
		float fSlow44 = float(fSlow2 != 4.0f);
		float fSlow45 = 0.001f * float(fHslider13);
		float fSlow46 = fConst16 * float(fHslider14) + 0.9995f;
		float fSlow47 = 0.001f * float(fEntry2);
		float fSlow48 = 1.5f * fSlow0;
		int iSlow49 = int(fConst24 * (float(fHslider15) / fSlow7)) & 4095;
		for (int i0 = 0; i0 < count; i0 = i0 + 1) {
			fRec8[0] = fSlow5 + 0.999f * fRec8[1];
			float fTemp0 = fSlow11 * fRec0[(IOTA0 - iSlow12) & 8191] + fSlow13 * fRec0[(IOTA0 - iSlow14) & 8191];
			fVec0[0] = fTemp0;
			iRec9[0] = iSlow16 & (iRec9[1] | (fRec10[1] >= 1.0f));
			int iTemp1 = iSlow21 & (fRec10[1] > 0.0f);
			fRec10[0] = (fSlow18 * float(((iRec9[1] == 0) & iSlow16) & (fRec10[1] < 1.0f)) + fRec10[1] * (1.0f - fSlow20 * float(iTemp1))) * float((iTemp1 == 0) | (fRec10[1] >= 1e-06f));
			float fTemp2 = fRec8[0] * fRec10[0];
			float fTemp3 = fTemp2 * (fSlow22 * fTemp0 + fSlow23 * (fTemp0 + fVec0[1]) + fSlow24 * Blow_Hole_dsp_faustpower2_f(fTemp0));
			float fTemp4 = std::sin(fTemp3);
			float fTemp5 = std::cos(fTemp3);
			float fTemp6 = fTemp0 * fTemp5 - fTemp4 * fRec11[1];
			fRec12[0] = fTemp5 * fTemp6 - fTemp4 * fRec12[1];
			fRec11[0] = fTemp4 * fTemp6 + fTemp5 * fRec12[1];
			float fTemp7 = fSlow3 * (fRec8[0] * (fTemp0 * fTemp4 + fRec11[1] * fTemp5) + (1.0f - fRec8[0]) * fTemp0);
			iRec14[0] = iSlow16 & (iRec14[1] | (fRec15[1] >= 1.0f));
			int iTemp8 = iSlow21 & (fRec15[1] > 0.0f);
			fRec15[0] = (fSlow29 * float(((iRec14[1] == 0) & iSlow16) & (fRec15[1] < 1.0f)) + fRec15[1] * (1.0f - fSlow31 * float(iTemp8))) * float((iTemp8 == 0) | (fRec15[1] >= 1e-06f));
			iRec16[0] = 1103515245 * iRec16[1] + 12345;
			iRec17[0] = iSlow16 & (iRec17[1] | (fRec19[1] >= 1.0f));
			iRec18[0] = iSlow16 * (iRec18[1] + 1);
			float fTemp9 = float(iRec18[1]);
			int iTemp10 = iSlow21 & (fRec19[1] > 0.0f);
			fRec19[0] = (fSlow35 * float(((((iRec17[1] == 0) & iSlow16) & (fRec19[1] < 1.0f)) & (fTemp9 > fSlow36)) * (1 - (fTemp9 < fSlow37))) + fRec19[1] * (1.0f - fSlow39 * float(iTemp10))) * float((iTemp10 == 0) | (fRec19[1] >= 1e-06f));
			fRec21[0] = fSlow40 + (fRec21[1] - std::floor(fSlow40 + fRec21[1]));
			float fTemp11 = fSlow27 * fRec15[0] * (fSlow32 * float(iRec16[0]) + 1.0f) * (fSlow33 * fRec19[0] * ftbl0Blow_Hole_dspSIG0[std::max<int>(0, std::min<int>(int(65536.0f * fRec21[0]), 65535))] + 1.0f);
			float fTemp12 = fConst10 * fRec5[(IOTA0 - iConst11) & 63] + fConst12 * fRec5[(IOTA0 - iConst13) & 63] - fTemp11;
			float fTemp13 = fSlow41 * fTemp12 + 0.7f;
			float fTemp14 = float(fTemp13 > 1.0f) + fTemp13 * float(fTemp13 <= 1.0f);
			float fTemp15 = fTemp11 + fTemp12 * (float(-(fTemp14 < -1.0f)) + fTemp14 * float(fTemp14 >= -1.0f));
			fRec23[0] = fSlow45 + 0.999f * fRec23[1];
			float fTemp16 = fRec22[1] + fConst6 * (fSlow43 + fSlow44 * fRec23[0]);
			fRec22[0] = fTemp16 - std::floor(fTemp16);
			float fTemp17 = 3.1415927f * fTemp2 * ftbl0Blow_Hole_dspSIG0[std::max<int>(0, std::min<int>(int(65536.0f * fRec22[0]), 65535))];
			float fTemp18 = std::sin(fTemp17);
			float fTemp19 = std::cos(fTemp17);
			float fTemp20 = fTemp0 * fTemp19 - fTemp18 * fRec24[1];
			fRec25[0] = fTemp19 * fTemp20 - fTemp18 * fRec25[1];
			fRec24[0] = fTemp18 * fTemp20 + fTemp19 * fRec25[1];
			float fTemp21 = fSlow42 * (fTemp0 * fTemp18 + fRec24[1] * fTemp19);
			float fTemp22 = fSlow25 * (fTemp7 + fTemp15 + fTemp21);
			fVec1[0] = fTemp22;
			fRec13[0] = -(fTemp22 + fVec1[1] + fConst14 * fRec13[1]);
			fRec5[IOTA0 & 63] = fTemp7 + fRec13[0] + fTemp21;
			float fRec6 = fRec13[0];
			float fRec7 = fTemp15;
			float fTemp23 = fRec6 + fRec7;
			float fTemp24 = fConst20 * fRec2[(IOTA0 - iConst21) & 63];
			float fTemp25 = fConst22 * fRec2[(IOTA0 - iConst23) & 63];
			float fTemp26 = fTemp24 + fTemp23 + fTemp25;
			float fTemp27 = 0.074074075f * (2.0f * fRec26[1] - fTemp26);
			float fTemp28 = fTemp26 + fTemp27 - fRec29[1];
			fVec2[0] = fTemp28;
			fRec30[0] = fSlow46 * (fTemp28 + fRec30[1]) - fVec2[1];
			fRec29[0] = fRec30[0];
			fRec26[0] = fRec29[0];
			float fRec27 = fTemp27;
			float fRec28 = fTemp27;
			fRec4[0] = 0.5f * (fRec4[1] + fTemp23 + fRec27);
			fRec2[IOTA0 & 63] = -(0.95f * fRec4[0]);
			float fRec3 = fTemp24 + fRec28 + fTemp25;
			fRec0[IOTA0 & 8191] = fRec3;
			float fRec1 = fRec5[IOTA0 & 63];
			fRec31[0] = fSlow47 + 0.999f * fRec31[1];
			float fTemp29 = fRec1 * fRec31[0];
			fVec3[IOTA0 & 4095] = fTemp29;
			output0[i0] = FAUSTFLOAT(fSlow1 * fTemp29);
			output1[i0] = FAUSTFLOAT(fSlow48 * fVec3[(IOTA0 - iSlow49) & 4095]);
			fRec8[1] = fRec8[0];
			IOTA0 = IOTA0 + 1;
			fVec0[1] = fVec0[0];
			iRec9[1] = iRec9[0];
			fRec10[1] = fRec10[0];
			fRec12[1] = fRec12[0];
			fRec11[1] = fRec11[0];
			iRec14[1] = iRec14[0];
			fRec15[1] = fRec15[0];
			iRec16[1] = iRec16[0];
			iRec17[1] = iRec17[0];
			iRec18[1] = iRec18[0];
			fRec19[1] = fRec19[0];
			fRec21[1] = fRec21[0];
			fRec23[1] = fRec23[0];
			fRec22[1] = fRec22[0];
			fRec25[1] = fRec25[0];
			fRec24[1] = fRec24[0];
			fVec1[1] = fVec1[0];
			fRec13[1] = fRec13[0];
			fVec2[1] = fVec2[0];
			fRec30[1] = fRec30[0];
			fRec29[1] = fRec29[0];
			fRec26[1] = fRec26[0];
			fRec4[1] = fRec4[0];
			fRec31[1] = fRec31[0];
		}
	}

};


#include "Faust_plugins_template2.cpp"


#endif
