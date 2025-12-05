/* ------------------------------------------------------------
name: "zita_rev"
Code generated with Faust 2.81.2 (https://faust.grame.fr)
Compilation options: -a Faust_plugins_template.cpp -lang cpp -nvi -ct 1 -cn Zita_dsp -es 1 -mcd 16 -mdd 1024 -mdy 33 -single -ftz 0
------------------------------------------------------------ */

#ifndef  __Zita_dsp_H__
#define  __Zita_dsp_H__


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
#define FAUSTCLASS Zita_dsp
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

static float Zita_dsp_faustpower2_f(float value) {
	return value * value;
}

class Zita_dsp final : public dsp {
	
 private:
	
	int fSampleRate;
	float fConst0;
	float fConst1;
	FAUSTFLOAT fVslider0;
	float fConst2;
	float fConst3;
	FAUSTFLOAT fVslider1;
	FAUSTFLOAT fVslider2;
	float fConst4;
	FAUSTFLOAT fVslider3;
	float fRec11[2];
	float fRec10[2];
	int IOTA0;
	float fVec0[16384];
	float fConst5;
	int iConst6;
	float fVec1[8192];
	float fConst7;
	FAUSTFLOAT fVslider4;
	float fVec2[1024];
	int iConst8;
	float fRec8[2];
	float fConst9;
	float fConst10;
	float fRec15[2];
	float fRec14[2];
	float fVec3[16384];
	float fConst11;
	int iConst12;
	float fVec4[2048];
	int iConst13;
	float fRec12[2];
	float fConst14;
	float fConst15;
	float fRec19[2];
	float fRec18[2];
	float fVec5[8192];
	float fConst16;
	int iConst17;
	float fVec6[2048];
	int iConst18;
	float fRec16[2];
	float fConst19;
	float fConst20;
	float fRec23[2];
	float fRec22[2];
	float fVec7[16384];
	float fConst21;
	int iConst22;
	float fVec8[2048];
	int iConst23;
	float fRec20[2];
	float fConst24;
	float fConst25;
	float fRec27[2];
	float fRec26[2];
	float fVec9[8192];
	float fConst26;
	int iConst27;
	float fVec10[8192];
	float fVec11[1024];
	int iConst28;
	float fRec24[2];
	float fConst29;
	float fConst30;
	float fRec31[2];
	float fRec30[2];
	float fVec12[8192];
	float fConst31;
	int iConst32;
	float fVec13[2048];
	int iConst33;
	float fRec28[2];
	float fConst34;
	float fConst35;
	float fRec35[2];
	float fRec34[2];
	float fVec14[8192];
	float fConst36;
	int iConst37;
	float fVec15[2048];
	int iConst38;
	float fRec32[2];
	float fConst39;
	float fConst40;
	float fRec39[2];
	float fRec38[2];
	float fVec16[8192];
	float fConst41;
	int iConst42;
	float fVec17[1024];
	int iConst43;
	float fRec36[2];
	float fRec0[3];
	float fRec1[3];
	float fRec2[3];
	float fRec3[3];
	float fRec4[3];
	float fRec5[3];
	float fRec6[3];
	float fRec7[3];
	
 public:
	Zita_dsp() {
	}
	
	void metadata(Meta* m) { 
		m->declare("compile_options", "-a Faust_plugins_template.cpp -lang cpp -nvi -ct 1 -cn Zita_dsp -es 1 -mcd 16 -mdd 1024 -mdy 33 -single -ftz 0");
		m->declare("filename", "zita_rev.dsp");
		m->declare("filter.lib/author", "Julius O. Smith (jos at ccrma.stanford.edu)");
		m->declare("filter.lib/copyright", "Julius O. Smith III");
		m->declare("filter.lib/deprecated", "This library is deprecated and is not maintained anymore. It will be removed in August 2017.");
		m->declare("filter.lib/license", "STK-4.3");
		m->declare("filter.lib/name", "Faust Filter Library");
		m->declare("filter.lib/reference", "https://ccrma.stanford.edu/~jos/filters/");
		m->declare("filter.lib/version", "1.29");
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
		m->declare("name", "zita_rev");
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
	}
	
	void instanceConstants(int sample_rate) {
		fSampleRate = sample_rate;
		fConst0 = std::min<float>(1.92e+05f, std::max<float>(1.0f, float(fSampleRate)));
		fConst1 = 6.2831855f / fConst0;
		fConst2 = std::floor(0.219991f * fConst0 + 0.5f);
		fConst3 = 6.9077554f * (fConst2 / fConst0);
		fConst4 = 3.1415927f / fConst0;
		fConst5 = std::floor(0.019123f * fConst0 + 0.5f);
		iConst6 = int(fConst2 - fConst5) & 16383;
		fConst7 = 0.001f * fConst0;
		iConst8 = int(fConst5 + -1.0f) & 1023;
		fConst9 = std::floor(0.256891f * fConst0 + 0.5f);
		fConst10 = 6.9077554f * (fConst9 / fConst0);
		fConst11 = std::floor(0.027333f * fConst0 + 0.5f);
		iConst12 = int(fConst9 - fConst11) & 16383;
		iConst13 = int(fConst11 + -1.0f) & 2047;
		fConst14 = std::floor(0.192303f * fConst0 + 0.5f);
		fConst15 = 6.9077554f * (fConst14 / fConst0);
		fConst16 = std::floor(0.029291f * fConst0 + 0.5f);
		iConst17 = int(fConst14 - fConst16) & 8191;
		iConst18 = int(fConst16 + -1.0f) & 2047;
		fConst19 = std::floor(0.210389f * fConst0 + 0.5f);
		fConst20 = 6.9077554f * (fConst19 / fConst0);
		fConst21 = std::floor(0.024421f * fConst0 + 0.5f);
		iConst22 = int(fConst19 - fConst21) & 16383;
		iConst23 = int(fConst21 + -1.0f) & 2047;
		fConst24 = std::floor(0.125f * fConst0 + 0.5f);
		fConst25 = 6.9077554f * (fConst24 / fConst0);
		fConst26 = std::floor(0.013458f * fConst0 + 0.5f);
		iConst27 = int(fConst24 - fConst26) & 8191;
		iConst28 = int(fConst26 + -1.0f) & 1023;
		fConst29 = std::floor(0.127837f * fConst0 + 0.5f);
		fConst30 = 6.9077554f * (fConst29 / fConst0);
		fConst31 = std::floor(0.031604f * fConst0 + 0.5f);
		iConst32 = int(fConst29 - fConst31) & 8191;
		iConst33 = int(fConst31 + -1.0f) & 2047;
		fConst34 = std::floor(0.174713f * fConst0 + 0.5f);
		fConst35 = 6.9077554f * (fConst34 / fConst0);
		fConst36 = std::floor(0.022904f * fConst0 + 0.5f);
		iConst37 = int(fConst34 - fConst36) & 8191;
		iConst38 = int(fConst36 + -1.0f) & 2047;
		fConst39 = std::floor(0.153129f * fConst0 + 0.5f);
		fConst40 = 6.9077554f * (fConst39 / fConst0);
		fConst41 = std::floor(0.020346f * fConst0 + 0.5f);
		iConst42 = int(fConst39 - fConst41) & 8191;
		iConst43 = int(fConst41 + -1.0f) & 1023;
	}
	
	void instanceResetUserInterface() {
		fVslider0 = FAUSTFLOAT(6e+03f);
		fVslider1 = FAUSTFLOAT(2.0f);
		fVslider2 = FAUSTFLOAT(3.0f);
		fVslider3 = FAUSTFLOAT(2e+02f);
		fVslider4 = FAUSTFLOAT(0.0f);
	}
	
	void instanceClear() {
		for (int l0 = 0; l0 < 2; l0 = l0 + 1) {
			fRec11[l0] = 0.0f;
		}
		for (int l1 = 0; l1 < 2; l1 = l1 + 1) {
			fRec10[l1] = 0.0f;
		}
		IOTA0 = 0;
		for (int l2 = 0; l2 < 16384; l2 = l2 + 1) {
			fVec0[l2] = 0.0f;
		}
		for (int l3 = 0; l3 < 8192; l3 = l3 + 1) {
			fVec1[l3] = 0.0f;
		}
		for (int l4 = 0; l4 < 1024; l4 = l4 + 1) {
			fVec2[l4] = 0.0f;
		}
		for (int l5 = 0; l5 < 2; l5 = l5 + 1) {
			fRec8[l5] = 0.0f;
		}
		for (int l6 = 0; l6 < 2; l6 = l6 + 1) {
			fRec15[l6] = 0.0f;
		}
		for (int l7 = 0; l7 < 2; l7 = l7 + 1) {
			fRec14[l7] = 0.0f;
		}
		for (int l8 = 0; l8 < 16384; l8 = l8 + 1) {
			fVec3[l8] = 0.0f;
		}
		for (int l9 = 0; l9 < 2048; l9 = l9 + 1) {
			fVec4[l9] = 0.0f;
		}
		for (int l10 = 0; l10 < 2; l10 = l10 + 1) {
			fRec12[l10] = 0.0f;
		}
		for (int l11 = 0; l11 < 2; l11 = l11 + 1) {
			fRec19[l11] = 0.0f;
		}
		for (int l12 = 0; l12 < 2; l12 = l12 + 1) {
			fRec18[l12] = 0.0f;
		}
		for (int l13 = 0; l13 < 8192; l13 = l13 + 1) {
			fVec5[l13] = 0.0f;
		}
		for (int l14 = 0; l14 < 2048; l14 = l14 + 1) {
			fVec6[l14] = 0.0f;
		}
		for (int l15 = 0; l15 < 2; l15 = l15 + 1) {
			fRec16[l15] = 0.0f;
		}
		for (int l16 = 0; l16 < 2; l16 = l16 + 1) {
			fRec23[l16] = 0.0f;
		}
		for (int l17 = 0; l17 < 2; l17 = l17 + 1) {
			fRec22[l17] = 0.0f;
		}
		for (int l18 = 0; l18 < 16384; l18 = l18 + 1) {
			fVec7[l18] = 0.0f;
		}
		for (int l19 = 0; l19 < 2048; l19 = l19 + 1) {
			fVec8[l19] = 0.0f;
		}
		for (int l20 = 0; l20 < 2; l20 = l20 + 1) {
			fRec20[l20] = 0.0f;
		}
		for (int l21 = 0; l21 < 2; l21 = l21 + 1) {
			fRec27[l21] = 0.0f;
		}
		for (int l22 = 0; l22 < 2; l22 = l22 + 1) {
			fRec26[l22] = 0.0f;
		}
		for (int l23 = 0; l23 < 8192; l23 = l23 + 1) {
			fVec9[l23] = 0.0f;
		}
		for (int l24 = 0; l24 < 8192; l24 = l24 + 1) {
			fVec10[l24] = 0.0f;
		}
		for (int l25 = 0; l25 < 1024; l25 = l25 + 1) {
			fVec11[l25] = 0.0f;
		}
		for (int l26 = 0; l26 < 2; l26 = l26 + 1) {
			fRec24[l26] = 0.0f;
		}
		for (int l27 = 0; l27 < 2; l27 = l27 + 1) {
			fRec31[l27] = 0.0f;
		}
		for (int l28 = 0; l28 < 2; l28 = l28 + 1) {
			fRec30[l28] = 0.0f;
		}
		for (int l29 = 0; l29 < 8192; l29 = l29 + 1) {
			fVec12[l29] = 0.0f;
		}
		for (int l30 = 0; l30 < 2048; l30 = l30 + 1) {
			fVec13[l30] = 0.0f;
		}
		for (int l31 = 0; l31 < 2; l31 = l31 + 1) {
			fRec28[l31] = 0.0f;
		}
		for (int l32 = 0; l32 < 2; l32 = l32 + 1) {
			fRec35[l32] = 0.0f;
		}
		for (int l33 = 0; l33 < 2; l33 = l33 + 1) {
			fRec34[l33] = 0.0f;
		}
		for (int l34 = 0; l34 < 8192; l34 = l34 + 1) {
			fVec14[l34] = 0.0f;
		}
		for (int l35 = 0; l35 < 2048; l35 = l35 + 1) {
			fVec15[l35] = 0.0f;
		}
		for (int l36 = 0; l36 < 2; l36 = l36 + 1) {
			fRec32[l36] = 0.0f;
		}
		for (int l37 = 0; l37 < 2; l37 = l37 + 1) {
			fRec39[l37] = 0.0f;
		}
		for (int l38 = 0; l38 < 2; l38 = l38 + 1) {
			fRec38[l38] = 0.0f;
		}
		for (int l39 = 0; l39 < 8192; l39 = l39 + 1) {
			fVec16[l39] = 0.0f;
		}
		for (int l40 = 0; l40 < 1024; l40 = l40 + 1) {
			fVec17[l40] = 0.0f;
		}
		for (int l41 = 0; l41 < 2; l41 = l41 + 1) {
			fRec36[l41] = 0.0f;
		}
		for (int l42 = 0; l42 < 3; l42 = l42 + 1) {
			fRec0[l42] = 0.0f;
		}
		for (int l43 = 0; l43 < 3; l43 = l43 + 1) {
			fRec1[l43] = 0.0f;
		}
		for (int l44 = 0; l44 < 3; l44 = l44 + 1) {
			fRec2[l44] = 0.0f;
		}
		for (int l45 = 0; l45 < 3; l45 = l45 + 1) {
			fRec3[l45] = 0.0f;
		}
		for (int l46 = 0; l46 < 3; l46 = l46 + 1) {
			fRec4[l46] = 0.0f;
		}
		for (int l47 = 0; l47 < 3; l47 = l47 + 1) {
			fRec5[l47] = 0.0f;
		}
		for (int l48 = 0; l48 < 3; l48 = l48 + 1) {
			fRec6[l48] = 0.0f;
		}
		for (int l49 = 0; l49 < 3; l49 = l49 + 1) {
			fRec7[l49] = 0.0f;
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
	
	Zita_dsp* clone() {
		return new Zita_dsp();
	}
	
	int getSampleRate() {
		return fSampleRate;
	}
	
	void buildUserInterface(UI* ui_interface) {
		ui_interface->declare(0, "0", "");
		ui_interface->declare(0, "tooltip", "~ ZITA REV1 FEEDBACK DELAY NETWORK (FDN) & SCHROEDER ALLPASS-COMB REVERBERATOR (8x8). See Faust's effect.lib for documentation and references");
		ui_interface->openHorizontalBox("Zita_Rev1");
		ui_interface->declare(0, "1", "");
		ui_interface->openHorizontalBox("Input");
		ui_interface->declare(&fVslider4, "1", "");
		ui_interface->declare(&fVslider4, "style", "knob");
		ui_interface->declare(&fVslider4, "tooltip", "Delay in ms before reverberation begins");
		ui_interface->declare(&fVslider4, "unit", "ms");
		ui_interface->addVerticalSlider("In Delay", &fVslider4, FAUSTFLOAT(0.0f), FAUSTFLOAT(0.0f), FAUSTFLOAT(1e+02f), FAUSTFLOAT(1.0f));
		ui_interface->closeBox();
		ui_interface->declare(0, "2", "");
		ui_interface->openHorizontalBox("Decay Times in Bands (see tooltips)");
		ui_interface->declare(&fVslider3, "1", "");
		ui_interface->declare(&fVslider3, "style", "knob");
		ui_interface->declare(&fVslider3, "tooltip", "Crossover frequency (Hz) separating low and middle frequencies");
		ui_interface->declare(&fVslider3, "unit", "Hz");
		ui_interface->addVerticalSlider("LF X", &fVslider3, FAUSTFLOAT(2e+02f), FAUSTFLOAT(5e+01f), FAUSTFLOAT(1e+03f), FAUSTFLOAT(1.0f));
		ui_interface->declare(&fVslider2, "2", "");
		ui_interface->declare(&fVslider2, "style", "knob");
		ui_interface->declare(&fVslider2, "tooltip", "T60 = time (in seconds) to decay 60dB in low-frequency band");
		ui_interface->declare(&fVslider2, "unit", "s");
		ui_interface->addVerticalSlider("Low RT60", &fVslider2, FAUSTFLOAT(3.0f), FAUSTFLOAT(1.0f), FAUSTFLOAT(8.0f), FAUSTFLOAT(0.1f));
		ui_interface->declare(&fVslider1, "3", "");
		ui_interface->declare(&fVslider1, "style", "knob");
		ui_interface->declare(&fVslider1, "tooltip", "T60 = time (in seconds) to decay 60dB in middle band");
		ui_interface->declare(&fVslider1, "unit", "s");
		ui_interface->addVerticalSlider("Mid RT60", &fVslider1, FAUSTFLOAT(2.0f), FAUSTFLOAT(1.0f), FAUSTFLOAT(8.0f), FAUSTFLOAT(0.1f));
		ui_interface->declare(&fVslider0, "4", "");
		ui_interface->declare(&fVslider0, "style", "knob");
		ui_interface->declare(&fVslider0, "tooltip", "Frequency (Hz) at which the high-frequency T60 is half the middle-band's T60");
		ui_interface->declare(&fVslider0, "unit", "Hz");
		ui_interface->addVerticalSlider("HF Damping", &fVslider0, FAUSTFLOAT(6e+03f), FAUSTFLOAT(1.5e+03f), FAUSTFLOAT(2.352e+04f), FAUSTFLOAT(1.0f));
		ui_interface->closeBox();
		ui_interface->closeBox();
	}
	
	void compute(int count, FAUSTFLOAT** RESTRICT inputs, FAUSTFLOAT** RESTRICT outputs) {
		FAUSTFLOAT* input0 = inputs[0];
		FAUSTFLOAT* input1 = inputs[1];
		FAUSTFLOAT* output0 = outputs[0];
		FAUSTFLOAT* output1 = outputs[1];
		float fSlow0 = std::cos(fConst1 * float(fVslider0));
		float fSlow1 = float(fVslider1);
		float fSlow2 = std::exp(-(fConst3 / fSlow1));
		float fSlow3 = Zita_dsp_faustpower2_f(fSlow2);
		float fSlow4 = 1.0f - fSlow0 * fSlow3;
		float fSlow5 = 1.0f - fSlow3;
		float fSlow6 = fSlow4 / fSlow5;
		float fSlow7 = std::sqrt(std::max<float>(0.0f, Zita_dsp_faustpower2_f(fSlow4) / Zita_dsp_faustpower2_f(fSlow5) + -1.0f));
		float fSlow8 = fSlow6 - fSlow7;
		float fSlow9 = fSlow2 * (fSlow7 + (1.0f - fSlow6));
		float fSlow10 = float(fVslider2);
		float fSlow11 = std::exp(-(fConst3 / fSlow10)) / fSlow2 + -1.0f;
		float fSlow12 = 1.0f / std::tan(fConst4 * float(fVslider3));
		float fSlow13 = 1.0f / (fSlow12 + 1.0f);
		float fSlow14 = 1.0f - fSlow12;
		int iSlow15 = int(fConst7 * float(fVslider4)) & 8191;
		float fSlow16 = std::exp(-(fConst10 / fSlow1));
		float fSlow17 = Zita_dsp_faustpower2_f(fSlow16);
		float fSlow18 = 1.0f - fSlow0 * fSlow17;
		float fSlow19 = 1.0f - fSlow17;
		float fSlow20 = fSlow18 / fSlow19;
		float fSlow21 = std::sqrt(std::max<float>(0.0f, Zita_dsp_faustpower2_f(fSlow18) / Zita_dsp_faustpower2_f(fSlow19) + -1.0f));
		float fSlow22 = fSlow20 - fSlow21;
		float fSlow23 = fSlow16 * (fSlow21 + (1.0f - fSlow20));
		float fSlow24 = std::exp(-(fConst10 / fSlow10)) / fSlow16 + -1.0f;
		float fSlow25 = std::exp(-(fConst15 / fSlow1));
		float fSlow26 = Zita_dsp_faustpower2_f(fSlow25);
		float fSlow27 = 1.0f - fSlow0 * fSlow26;
		float fSlow28 = 1.0f - fSlow26;
		float fSlow29 = fSlow27 / fSlow28;
		float fSlow30 = std::sqrt(std::max<float>(0.0f, Zita_dsp_faustpower2_f(fSlow27) / Zita_dsp_faustpower2_f(fSlow28) + -1.0f));
		float fSlow31 = fSlow29 - fSlow30;
		float fSlow32 = fSlow25 * (fSlow30 + (1.0f - fSlow29));
		float fSlow33 = std::exp(-(fConst15 / fSlow10)) / fSlow25 + -1.0f;
		float fSlow34 = std::exp(-(fConst20 / fSlow1));
		float fSlow35 = Zita_dsp_faustpower2_f(fSlow34);
		float fSlow36 = 1.0f - fSlow0 * fSlow35;
		float fSlow37 = 1.0f - fSlow35;
		float fSlow38 = fSlow36 / fSlow37;
		float fSlow39 = std::sqrt(std::max<float>(0.0f, Zita_dsp_faustpower2_f(fSlow36) / Zita_dsp_faustpower2_f(fSlow37) + -1.0f));
		float fSlow40 = fSlow38 - fSlow39;
		float fSlow41 = fSlow34 * (fSlow39 + (1.0f - fSlow38));
		float fSlow42 = std::exp(-(fConst20 / fSlow10)) / fSlow34 + -1.0f;
		float fSlow43 = std::exp(-(fConst25 / fSlow1));
		float fSlow44 = Zita_dsp_faustpower2_f(fSlow43);
		float fSlow45 = 1.0f - fSlow0 * fSlow44;
		float fSlow46 = 1.0f - fSlow44;
		float fSlow47 = fSlow45 / fSlow46;
		float fSlow48 = std::sqrt(std::max<float>(0.0f, Zita_dsp_faustpower2_f(fSlow45) / Zita_dsp_faustpower2_f(fSlow46) + -1.0f));
		float fSlow49 = fSlow47 - fSlow48;
		float fSlow50 = fSlow43 * (fSlow48 + (1.0f - fSlow47));
		float fSlow51 = std::exp(-(fConst25 / fSlow10)) / fSlow43 + -1.0f;
		float fSlow52 = std::exp(-(fConst30 / fSlow1));
		float fSlow53 = Zita_dsp_faustpower2_f(fSlow52);
		float fSlow54 = 1.0f - fSlow0 * fSlow53;
		float fSlow55 = 1.0f - fSlow53;
		float fSlow56 = fSlow54 / fSlow55;
		float fSlow57 = std::sqrt(std::max<float>(0.0f, Zita_dsp_faustpower2_f(fSlow54) / Zita_dsp_faustpower2_f(fSlow55) + -1.0f));
		float fSlow58 = fSlow56 - fSlow57;
		float fSlow59 = fSlow52 * (fSlow57 + (1.0f - fSlow56));
		float fSlow60 = std::exp(-(fConst30 / fSlow10)) / fSlow52 + -1.0f;
		float fSlow61 = std::exp(-(fConst35 / fSlow1));
		float fSlow62 = Zita_dsp_faustpower2_f(fSlow61);
		float fSlow63 = 1.0f - fSlow0 * fSlow62;
		float fSlow64 = 1.0f - fSlow62;
		float fSlow65 = fSlow63 / fSlow64;
		float fSlow66 = std::sqrt(std::max<float>(0.0f, Zita_dsp_faustpower2_f(fSlow63) / Zita_dsp_faustpower2_f(fSlow64) + -1.0f));
		float fSlow67 = fSlow65 - fSlow66;
		float fSlow68 = fSlow61 * (fSlow66 + (1.0f - fSlow65));
		float fSlow69 = std::exp(-(fConst35 / fSlow10)) / fSlow61 + -1.0f;
		float fSlow70 = std::exp(-(fConst40 / fSlow1));
		float fSlow71 = Zita_dsp_faustpower2_f(fSlow70);
		float fSlow72 = 1.0f - fSlow71 * fSlow0;
		float fSlow73 = 1.0f - fSlow71;
		float fSlow74 = fSlow72 / fSlow73;
		float fSlow75 = std::sqrt(std::max<float>(0.0f, Zita_dsp_faustpower2_f(fSlow72) / Zita_dsp_faustpower2_f(fSlow73) + -1.0f));
		float fSlow76 = fSlow74 - fSlow75;
		float fSlow77 = fSlow70 * (fSlow75 + (1.0f - fSlow74));
		float fSlow78 = std::exp(-(fConst40 / fSlow10)) / fSlow70 + -1.0f;
		for (int i0 = 0; i0 < count; i0 = i0 + 1) {
			fRec11[0] = -(fSlow13 * (fSlow14 * fRec11[1] - (fRec7[1] + fRec7[2])));
			fRec10[0] = fSlow8 * fRec10[1] + fSlow9 * (fRec7[1] + fSlow11 * fRec11[0]);
			fVec0[IOTA0 & 16383] = 0.35355338f * fRec10[0] + 1e-20f;
			float fTemp0 = 0.6f * fRec8[1] + fVec0[(IOTA0 - iConst6) & 16383];
			fVec1[IOTA0 & 8191] = float(input1[i0]);
			float fTemp1 = 0.3f * fVec1[(IOTA0 - iSlow15) & 8191];
			fVec2[IOTA0 & 1023] = fTemp0 - fTemp1;
			fRec8[0] = fVec2[(IOTA0 - iConst8) & 1023];
			float fRec9 = 0.6f * (fTemp1 - fTemp0);
			fRec15[0] = -(fSlow13 * (fSlow14 * fRec15[1] - (fRec3[1] + fRec3[2])));
			fRec14[0] = fSlow22 * fRec14[1] + fSlow23 * (fRec3[1] + fSlow24 * fRec15[0]);
			fVec3[IOTA0 & 16383] = 0.35355338f * fRec14[0] + 1e-20f;
			float fTemp2 = 0.6f * fRec12[1] + fVec3[(IOTA0 - iConst12) & 16383];
			fVec4[IOTA0 & 2047] = fTemp2 - fTemp1;
			fRec12[0] = fVec4[(IOTA0 - iConst13) & 2047];
			float fRec13 = 0.6f * (fTemp1 - fTemp2);
			fRec19[0] = -(fSlow13 * (fSlow14 * fRec19[1] - (fRec5[1] + fRec5[2])));
			fRec18[0] = fSlow31 * fRec18[1] + fSlow32 * (fRec5[1] + fSlow33 * fRec19[0]);
			fVec5[IOTA0 & 8191] = 0.35355338f * fRec18[0] + 1e-20f;
			float fTemp3 = fVec5[(IOTA0 - iConst17) & 8191] + fTemp1 + 0.6f * fRec16[1];
			fVec6[IOTA0 & 2047] = fTemp3;
			fRec16[0] = fVec6[(IOTA0 - iConst18) & 2047];
			float fRec17 = -(0.6f * fTemp3);
			fRec23[0] = -(fSlow13 * (fSlow14 * fRec23[1] - (fRec1[1] + fRec1[2])));
			fRec22[0] = fSlow40 * fRec22[1] + fSlow41 * (fRec1[1] + fSlow42 * fRec23[0]);
			fVec7[IOTA0 & 16383] = 0.35355338f * fRec22[0] + 1e-20f;
			float fTemp4 = fTemp1 + 0.6f * fRec20[1] + fVec7[(IOTA0 - iConst22) & 16383];
			fVec8[IOTA0 & 2047] = fTemp4;
			fRec20[0] = fVec8[(IOTA0 - iConst23) & 2047];
			float fRec21 = -(0.6f * fTemp4);
			fRec27[0] = -(fSlow13 * (fSlow14 * fRec27[1] - (fRec6[1] + fRec6[2])));
			fRec26[0] = fSlow49 * fRec26[1] + fSlow50 * (fRec6[1] + fSlow51 * fRec27[0]);
			fVec9[IOTA0 & 8191] = 0.35355338f * fRec26[0] + 1e-20f;
			fVec10[IOTA0 & 8191] = float(input0[i0]);
			float fTemp5 = 0.3f * fVec10[(IOTA0 - iSlow15) & 8191];
			float fTemp6 = fVec9[(IOTA0 - iConst27) & 8191] - (fTemp5 + 0.6f * fRec24[1]);
			fVec11[IOTA0 & 1023] = fTemp6;
			fRec24[0] = fVec11[(IOTA0 - iConst28) & 1023];
			float fRec25 = 0.6f * fTemp6;
			fRec31[0] = -(fSlow13 * (fSlow14 * fRec31[1] - (fRec2[1] + fRec2[2])));
			fRec30[0] = fSlow58 * fRec30[1] + fSlow59 * (fRec2[1] + fSlow60 * fRec31[0]);
			fVec12[IOTA0 & 8191] = 0.35355338f * fRec30[0] + 1e-20f;
			float fTemp7 = fVec12[(IOTA0 - iConst32) & 8191] - (fTemp5 + 0.6f * fRec28[1]);
			fVec13[IOTA0 & 2047] = fTemp7;
			fRec28[0] = fVec13[(IOTA0 - iConst33) & 2047];
			float fRec29 = 0.6f * fTemp7;
			fRec35[0] = -(fSlow13 * (fSlow14 * fRec35[1] - (fRec4[1] + fRec4[2])));
			fRec34[0] = fSlow67 * fRec34[1] + fSlow68 * (fRec4[1] + fSlow69 * fRec35[0]);
			fVec14[IOTA0 & 8191] = 0.35355338f * fRec34[0] + 1e-20f;
			float fTemp8 = fTemp5 + fVec14[(IOTA0 - iConst37) & 8191] - 0.6f * fRec32[1];
			fVec15[IOTA0 & 2047] = fTemp8;
			fRec32[0] = fVec15[(IOTA0 - iConst38) & 2047];
			float fRec33 = 0.6f * fTemp8;
			fRec39[0] = -(fSlow13 * (fSlow14 * fRec39[1] - (fRec0[1] + fRec0[2])));
			fRec38[0] = fSlow76 * fRec38[1] + fSlow77 * (fRec0[1] + fSlow78 * fRec39[0]);
			fVec16[IOTA0 & 8191] = 0.35355338f * fRec38[0] + 1e-20f;
			float fTemp9 = fVec16[(IOTA0 - iConst42) & 8191] + fTemp5 - 0.6f * fRec36[1];
			fVec17[IOTA0 & 1023] = fTemp9;
			fRec36[0] = fVec17[(IOTA0 - iConst43) & 1023];
			float fRec37 = 0.6f * fTemp9;
			float fTemp10 = fRec37 + fRec33;
			float fTemp11 = fRec25 + fRec29 + fTemp10;
			fRec0[0] = fRec8[1] + fRec12[1] + fRec16[1] + fRec20[1] + fRec24[1] + fRec28[1] + fRec32[1] + fRec36[1] + fRec9 + fRec13 + fRec17 + fRec21 + fTemp11;
			fRec1[0] = fRec24[1] + fRec28[1] + fRec32[1] + fRec36[1] + fTemp11 - (fRec8[1] + fRec12[1] + fRec16[1] + fRec20[1] + fRec9 + fRec13 + fRec21 + fRec17);
			float fTemp12 = fRec29 + fRec25;
			fRec2[0] = fRec16[1] + fRec20[1] + fRec32[1] + fRec36[1] + fRec17 + fRec21 + fTemp10 - (fRec8[1] + fRec12[1] + fRec24[1] + fRec28[1] + fRec9 + fRec13 + fTemp12);
			fRec3[0] = fRec8[1] + fRec12[1] + fRec32[1] + fRec36[1] + fRec9 + fRec13 + fTemp10 - (fRec16[1] + fRec20[1] + fRec24[1] + fRec28[1] + fRec17 + fRec21 + fTemp12);
			float fTemp13 = fRec37 + fRec29;
			float fTemp14 = fRec33 + fRec25;
			fRec4[0] = fRec12[1] + fRec20[1] + fRec28[1] + fRec36[1] + fRec13 + fRec21 + fTemp13 - (fRec8[1] + fRec16[1] + fRec24[1] + fRec32[1] + fRec9 + fRec17 + fTemp14);
			fRec5[0] = fRec8[1] + fRec16[1] + fRec28[1] + fRec36[1] + fRec9 + fRec17 + fTemp13 - (fRec12[1] + fRec20[1] + fRec24[1] + fRec32[1] + fRec13 + fRec21 + fTemp14);
			float fTemp15 = fRec37 + fRec25;
			float fTemp16 = fRec33 + fRec29;
			fRec6[0] = fRec8[1] + fRec20[1] + fRec24[1] + fRec36[1] + fRec9 + fRec21 + fTemp15 - (fRec12[1] + fRec16[1] + fRec28[1] + fRec32[1] + fRec13 + fRec17 + fTemp16);
			fRec7[0] = fRec12[1] + fRec16[1] + fRec24[1] + fRec36[1] + fRec13 + fRec17 + fTemp15 - (fRec8[1] + fRec20[1] + fRec28[1] + fRec32[1] + fRec9 + fRec21 + fTemp16);
			output0[i0] = FAUSTFLOAT(0.37f * (fRec1[0] + fRec2[0]));
			output1[i0] = FAUSTFLOAT(0.37f * (fRec1[0] - fRec2[0]));
			fRec11[1] = fRec11[0];
			fRec10[1] = fRec10[0];
			IOTA0 = IOTA0 + 1;
			fRec8[1] = fRec8[0];
			fRec15[1] = fRec15[0];
			fRec14[1] = fRec14[0];
			fRec12[1] = fRec12[0];
			fRec19[1] = fRec19[0];
			fRec18[1] = fRec18[0];
			fRec16[1] = fRec16[0];
			fRec23[1] = fRec23[0];
			fRec22[1] = fRec22[0];
			fRec20[1] = fRec20[0];
			fRec27[1] = fRec27[0];
			fRec26[1] = fRec26[0];
			fRec24[1] = fRec24[0];
			fRec31[1] = fRec31[0];
			fRec30[1] = fRec30[0];
			fRec28[1] = fRec28[0];
			fRec35[1] = fRec35[0];
			fRec34[1] = fRec34[0];
			fRec32[1] = fRec32[0];
			fRec39[1] = fRec39[0];
			fRec38[1] = fRec38[0];
			fRec36[1] = fRec36[0];
			fRec0[2] = fRec0[1];
			fRec0[1] = fRec0[0];
			fRec1[2] = fRec1[1];
			fRec1[1] = fRec1[0];
			fRec2[2] = fRec2[1];
			fRec2[1] = fRec2[0];
			fRec3[2] = fRec3[1];
			fRec3[1] = fRec3[0];
			fRec4[2] = fRec4[1];
			fRec4[1] = fRec4[0];
			fRec5[2] = fRec5[1];
			fRec5[1] = fRec5[0];
			fRec6[2] = fRec6[1];
			fRec6[1] = fRec6[0];
			fRec7[2] = fRec7[1];
			fRec7[1] = fRec7[0];
		}
	}

};


#include "Faust_plugins_template2.cpp"


#endif
