/* ------------------------------------------------------------
name: "faust_multibandcomp"
Code generated with Faust 2.81.2 (https://faust.grame.fr)
Compilation options: -a Faust_plugins_template.cpp -lang cpp -nvi -ct 1 -cn Multibandcomp_dsp -es 1 -mcd 16 -mdd 1024 -mdy 33 -single -ftz 0 -vec -lv 0 -vs 32
------------------------------------------------------------ */

#ifndef  __Multibandcomp_dsp_H__
#define  __Multibandcomp_dsp_H__


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
#include "typepunning.h"
#include <algorithm>
#include <cmath>
#include <cstdint>
#include <math.h>

#ifndef FAUSTCLASS 
#define FAUSTCLASS Multibandcomp_dsp
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

static float Multibandcomp_dsp_faustpower2_f(float value) {
	return value * value;
}

class Multibandcomp_dsp final : public dsp {
	
 private:
	
	int fSampleRate;
	float fConst0;
	float fConst1;
	FAUSTFLOAT fVslider0;
	float fYec0_perm[4];
	float fRec3_perm[4];
	float fRec2_perm[4];
	FAUSTFLOAT fVslider1;
	float fYec1_perm[4];
	float fRec1_perm[4];
	float fRec0_perm[4];
	float fYec2_perm[4];
	float fRec10_perm[4];
	float fRec9_perm[4];
	float fYec3_perm[4];
	float fRec8_perm[4];
	float fRec7_perm[4];
	FAUSTFLOAT fHslider0;
	FAUSTFLOAT fCheckbox0;
	float fConst2;
	FAUSTFLOAT fHslider1;
	float fRec6_perm[4];
	FAUSTFLOAT fHslider2;
	float fRec5_perm[4];
	float fConst3;
	FAUSTFLOAT fHslider3;
	FAUSTFLOAT fHslider4;
	float fRec4_perm[4];
	float fRec12_perm[4];
	float fRec11_perm[4];
	float fRec17_perm[4];
	float fRec16_perm[4];
	FAUSTFLOAT fHslider5;
	FAUSTFLOAT fCheckbox1;
	FAUSTFLOAT fHslider6;
	float fRec15_perm[4];
	FAUSTFLOAT fHslider7;
	float fRec14_perm[4];
	FAUSTFLOAT fHslider8;
	FAUSTFLOAT fHslider9;
	float fRec13_perm[4];
	float fRec20_perm[4];
	float fRec19_perm[4];
	float fRec18_perm[4];
	float fRec26_perm[4];
	float fRec25_perm[4];
	float fRec24_perm[4];
	FAUSTFLOAT fHslider10;
	FAUSTFLOAT fCheckbox2;
	FAUSTFLOAT fHslider11;
	float fRec23_perm[4];
	FAUSTFLOAT fHslider12;
	float fRec22_perm[4];
	FAUSTFLOAT fHslider13;
	FAUSTFLOAT fHslider14;
	float fRec21_perm[4];
	float fRec32_perm[4];
	float fRec31_perm[4];
	float fRec30_perm[4];
	float fRec35_perm[4];
	float fRec34_perm[4];
	float fRec33_perm[4];
	float fRec38_perm[4];
	float fRec37_perm[4];
	float fRec36_perm[4];
	FAUSTFLOAT fHslider15;
	FAUSTFLOAT fCheckbox3;
	FAUSTFLOAT fCheckbox4;
	FAUSTFLOAT fCheckbox5;
	FAUSTFLOAT fCheckbox6;
	FAUSTFLOAT fHslider16;
	FAUSTFLOAT fHslider17;
	FAUSTFLOAT fHslider18;
	FAUSTFLOAT fHbargraph0;
	FAUSTFLOAT fHbargraph1;
	FAUSTFLOAT fHbargraph2;
	FAUSTFLOAT fHbargraph3;
	FAUSTFLOAT fHbargraph4;
	FAUSTFLOAT fHbargraph5;
	FAUSTFLOAT fVslider2;
	float fConst4;
	float fRec29_perm[4];
	FAUSTFLOAT fVslider3;
	float fConst5;
	float fRec28_perm[4];
	float fConst6;
	FAUSTFLOAT fVslider4;
	float fRec27_perm[4];
	FAUSTFLOAT fHslider19;
	FAUSTFLOAT fHslider20;
	
 public:
	Multibandcomp_dsp() {
	}
	
	void metadata(Meta* m) { 
		m->declare("compile_options", "-a Faust_plugins_template.cpp -lang cpp -nvi -ct 1 -cn Multibandcomp_dsp -es 1 -mcd 16 -mdd 1024 -mdy 33 -single -ftz 0 -vec -lv 0 -vs 32");
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
		m->declare("filename", "faust_multibandcomp.dsp");
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
		m->declare("name", "faust_multibandcomp");
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
		fConst1 = 3.1415927f / fConst0;
		fConst2 = 1.0f / fConst0;
		fConst3 = 2.0f / fConst0;
		fConst4 = 1e+03f / fConst0;
		fConst5 = 1e+06f / fConst0;
		fConst6 = 2e+06f / fConst0;
	}
	
	void instanceResetUserInterface() {
		fVslider0 = FAUSTFLOAT(1.5e+03f);
		fVslider1 = FAUSTFLOAT(166.0f);
		fHslider0 = FAUSTFLOAT(0.0f);
		fCheckbox0 = FAUSTFLOAT(0.0f);
		fHslider1 = FAUSTFLOAT(2e+02f);
		fHslider2 = FAUSTFLOAT(1e+02f);
		fHslider3 = FAUSTFLOAT(2.0f);
		fHslider4 = FAUSTFLOAT(-2e+01f);
		fHslider5 = FAUSTFLOAT(0.0f);
		fCheckbox1 = FAUSTFLOAT(0.0f);
		fHslider6 = FAUSTFLOAT(2e+02f);
		fHslider7 = FAUSTFLOAT(1e+02f);
		fHslider8 = FAUSTFLOAT(2.0f);
		fHslider9 = FAUSTFLOAT(-2e+01f);
		fHslider10 = FAUSTFLOAT(0.0f);
		fCheckbox2 = FAUSTFLOAT(0.0f);
		fHslider11 = FAUSTFLOAT(2e+02f);
		fHslider12 = FAUSTFLOAT(1e+02f);
		fHslider13 = FAUSTFLOAT(2.0f);
		fHslider14 = FAUSTFLOAT(-2e+01f);
		fHslider15 = FAUSTFLOAT(0.0f);
		fCheckbox3 = FAUSTFLOAT(0.0f);
		fCheckbox4 = FAUSTFLOAT(0.0f);
		fCheckbox5 = FAUSTFLOAT(0.0f);
		fCheckbox6 = FAUSTFLOAT(0.0f);
		fHslider16 = FAUSTFLOAT(0.0f);
		fHslider17 = FAUSTFLOAT(0.0f);
		fHslider18 = FAUSTFLOAT(0.0f);
		fVslider2 = FAUSTFLOAT(5e+02f);
		fVslider3 = FAUSTFLOAT(8e+02f);
		fVslider4 = FAUSTFLOAT(4.0f);
		fHslider19 = FAUSTFLOAT(0.0f);
		fHslider20 = FAUSTFLOAT(0.0f);
	}
	
	void instanceClear() {
		for (int l0 = 0; l0 < 4; l0 = l0 + 1) {
			fYec0_perm[l0] = 0.0f;
		}
		for (int l1 = 0; l1 < 4; l1 = l1 + 1) {
			fRec3_perm[l1] = 0.0f;
		}
		for (int l2 = 0; l2 < 4; l2 = l2 + 1) {
			fRec2_perm[l2] = 0.0f;
		}
		for (int l3 = 0; l3 < 4; l3 = l3 + 1) {
			fYec1_perm[l3] = 0.0f;
		}
		for (int l4 = 0; l4 < 4; l4 = l4 + 1) {
			fRec1_perm[l4] = 0.0f;
		}
		for (int l5 = 0; l5 < 4; l5 = l5 + 1) {
			fRec0_perm[l5] = 0.0f;
		}
		for (int l6 = 0; l6 < 4; l6 = l6 + 1) {
			fYec2_perm[l6] = 0.0f;
		}
		for (int l7 = 0; l7 < 4; l7 = l7 + 1) {
			fRec10_perm[l7] = 0.0f;
		}
		for (int l8 = 0; l8 < 4; l8 = l8 + 1) {
			fRec9_perm[l8] = 0.0f;
		}
		for (int l9 = 0; l9 < 4; l9 = l9 + 1) {
			fYec3_perm[l9] = 0.0f;
		}
		for (int l10 = 0; l10 < 4; l10 = l10 + 1) {
			fRec8_perm[l10] = 0.0f;
		}
		for (int l11 = 0; l11 < 4; l11 = l11 + 1) {
			fRec7_perm[l11] = 0.0f;
		}
		for (int l12 = 0; l12 < 4; l12 = l12 + 1) {
			fRec6_perm[l12] = 0.0f;
		}
		for (int l13 = 0; l13 < 4; l13 = l13 + 1) {
			fRec5_perm[l13] = 0.0f;
		}
		for (int l14 = 0; l14 < 4; l14 = l14 + 1) {
			fRec4_perm[l14] = 0.0f;
		}
		for (int l15 = 0; l15 < 4; l15 = l15 + 1) {
			fRec12_perm[l15] = 0.0f;
		}
		for (int l16 = 0; l16 < 4; l16 = l16 + 1) {
			fRec11_perm[l16] = 0.0f;
		}
		for (int l17 = 0; l17 < 4; l17 = l17 + 1) {
			fRec17_perm[l17] = 0.0f;
		}
		for (int l18 = 0; l18 < 4; l18 = l18 + 1) {
			fRec16_perm[l18] = 0.0f;
		}
		for (int l19 = 0; l19 < 4; l19 = l19 + 1) {
			fRec15_perm[l19] = 0.0f;
		}
		for (int l20 = 0; l20 < 4; l20 = l20 + 1) {
			fRec14_perm[l20] = 0.0f;
		}
		for (int l21 = 0; l21 < 4; l21 = l21 + 1) {
			fRec13_perm[l21] = 0.0f;
		}
		for (int l22 = 0; l22 < 4; l22 = l22 + 1) {
			fRec20_perm[l22] = 0.0f;
		}
		for (int l23 = 0; l23 < 4; l23 = l23 + 1) {
			fRec19_perm[l23] = 0.0f;
		}
		for (int l24 = 0; l24 < 4; l24 = l24 + 1) {
			fRec18_perm[l24] = 0.0f;
		}
		for (int l25 = 0; l25 < 4; l25 = l25 + 1) {
			fRec26_perm[l25] = 0.0f;
		}
		for (int l26 = 0; l26 < 4; l26 = l26 + 1) {
			fRec25_perm[l26] = 0.0f;
		}
		for (int l27 = 0; l27 < 4; l27 = l27 + 1) {
			fRec24_perm[l27] = 0.0f;
		}
		for (int l28 = 0; l28 < 4; l28 = l28 + 1) {
			fRec23_perm[l28] = 0.0f;
		}
		for (int l29 = 0; l29 < 4; l29 = l29 + 1) {
			fRec22_perm[l29] = 0.0f;
		}
		for (int l30 = 0; l30 < 4; l30 = l30 + 1) {
			fRec21_perm[l30] = 0.0f;
		}
		for (int l31 = 0; l31 < 4; l31 = l31 + 1) {
			fRec32_perm[l31] = 0.0f;
		}
		for (int l32 = 0; l32 < 4; l32 = l32 + 1) {
			fRec31_perm[l32] = 0.0f;
		}
		for (int l33 = 0; l33 < 4; l33 = l33 + 1) {
			fRec30_perm[l33] = 0.0f;
		}
		for (int l34 = 0; l34 < 4; l34 = l34 + 1) {
			fRec35_perm[l34] = 0.0f;
		}
		for (int l35 = 0; l35 < 4; l35 = l35 + 1) {
			fRec34_perm[l35] = 0.0f;
		}
		for (int l36 = 0; l36 < 4; l36 = l36 + 1) {
			fRec33_perm[l36] = 0.0f;
		}
		for (int l37 = 0; l37 < 4; l37 = l37 + 1) {
			fRec38_perm[l37] = 0.0f;
		}
		for (int l38 = 0; l38 < 4; l38 = l38 + 1) {
			fRec37_perm[l38] = 0.0f;
		}
		for (int l39 = 0; l39 < 4; l39 = l39 + 1) {
			fRec36_perm[l39] = 0.0f;
		}
		for (int l40 = 0; l40 < 4; l40 = l40 + 1) {
			fRec29_perm[l40] = 0.0f;
		}
		for (int l41 = 0; l41 < 4; l41 = l41 + 1) {
			fRec28_perm[l41] = 0.0f;
		}
		for (int l42 = 0; l42 < 4; l42 = l42 + 1) {
			fRec27_perm[l42] = 0.0f;
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
	
	Multibandcomp_dsp* clone() {
		return new Multibandcomp_dsp();
	}
	
	int getSampleRate() {
		return fSampleRate;
	}
	
	void buildUserInterface(UI* ui_interface) {
		ui_interface->openVerticalBox("faust_multibandcomp");
		ui_interface->declare(&fCheckbox4, "0", "");
		ui_interface->declare(&fCheckbox4, "tooltip", "When this is checked, the compressor is enabled. If not, sound is muted.");
		ui_interface->addCheckButton("Band 1:  Solo", &fCheckbox4);
		ui_interface->declare(&fHbargraph1, "1", "");
		ui_interface->declare(&fHbargraph1, "7", "");
		ui_interface->addHorizontalBargraph("Band 1:   Outgain", &fHbargraph1, FAUSTFLOAT(0.0f), FAUSTFLOAT(1.0f));
		ui_interface->declare(&fCheckbox0, "0.5", "");
		ui_interface->declare(&fCheckbox0, "tooltip", "When this is checked, the compressor is enabled. If not, sound is muted.");
		ui_interface->addCheckButton("Band 1: Bypass", &fCheckbox0);
		ui_interface->declare(&fHslider3, "2", "");
		ui_interface->declare(&fHslider3, "style", "slider");
		ui_interface->declare(&fHslider3, "tooltip", "A compression Ratio of N means that for each N dB increase in input signal level above Threshold, the output level goes up 1 dB");
		ui_interface->addHorizontalSlider("Band 1: Ratio", &fHslider3, FAUSTFLOAT(2.0f), FAUSTFLOAT(1.0f), FAUSTFLOAT(2e+01f), FAUSTFLOAT(0.1f));
		ui_interface->declare(&fHslider4, "3", "");
		ui_interface->declare(&fHslider4, "style", "slider");
		ui_interface->declare(&fHslider4, "tooltip", "When the signal level exceeds the Threshold (in dB), its level is compressed according to the Ratio");
		ui_interface->declare(&fHslider4, "unit", "dB");
		ui_interface->addHorizontalSlider("Band 1: Threshold", &fHslider4, FAUSTFLOAT(-2e+01f), FAUSTFLOAT(-1e+02f), FAUSTFLOAT(1e+01f), FAUSTFLOAT(0.1f));
		ui_interface->declare(&fHslider2, "4", "");
		ui_interface->declare(&fHslider2, "style", "slider");
		ui_interface->declare(&fHslider2, "tooltip", "Time constant in ms (1/e smoothing time) for the compression gain to approach (exponentially) a new lower target level (the compression `kicking in')");
		ui_interface->declare(&fHslider2, "unit", "ms");
		ui_interface->addHorizontalSlider("Band 1: Attack", &fHslider2, FAUSTFLOAT(1e+02f), FAUSTFLOAT(0.0f), FAUSTFLOAT(5e+02f), FAUSTFLOAT(0.1f));
		ui_interface->declare(&fHslider1, "5", "");
		ui_interface->declare(&fHslider1, "style", "slider");
		ui_interface->declare(&fHslider1, "tooltip", "Time constant in ms (1/e smoothing time) for the compression gain to approach (exponentially) a new higher target level (the compression 'releasing')");
		ui_interface->declare(&fHslider1, "unit", "ms");
		ui_interface->addHorizontalSlider("Band 1: Release", &fHslider1, FAUSTFLOAT(2e+02f), FAUSTFLOAT(0.0f), FAUSTFLOAT(1e+03f), FAUSTFLOAT(0.1f));
		ui_interface->declare(&fHbargraph0, "1", "");
		ui_interface->declare(&fHbargraph0, "6", "");
		ui_interface->declare(&fHbargraph0, "tooltip", "dummy tooltip");
		ui_interface->addHorizontalBargraph("Band 1: Input Gain bargraph", &fHbargraph0, FAUSTFLOAT(0.0f), FAUSTFLOAT(1.0f));
		ui_interface->declare(&fHslider0, "2", "");
		ui_interface->declare(&fHslider0, "6", "");
		ui_interface->declare(&fHslider0, "tooltip", "The compressed-signal output level is increased by this amount (in dB) to make up for the level lost due to compression");
		ui_interface->declare(&fHslider0, "unit", "dB");
		ui_interface->addHorizontalSlider("Band 1: Input Gain", &fHslider0, FAUSTFLOAT(0.0f), FAUSTFLOAT(-4e+01f), FAUSTFLOAT(4e+01f), FAUSTFLOAT(0.1f));
		ui_interface->declare(&fHslider16, "2", "");
		ui_interface->declare(&fHslider16, "7", "");
		ui_interface->declare(&fHslider16, "tooltip", "The compressed-signal output level is increased by this amount (in dB) to make up for the level lost due to compression");
		ui_interface->declare(&fHslider16, "unit", "dB");
		ui_interface->addHorizontalSlider("Band 1: Output Gain", &fHslider16, FAUSTFLOAT(0.0f), FAUSTFLOAT(-4e+01f), FAUSTFLOAT(4e+01f), FAUSTFLOAT(0.1f));
		ui_interface->declare(&fCheckbox5, "0", "");
		ui_interface->declare(&fCheckbox5, "tooltip", "When this is checked, the compressor is enabled. If not, sound is muted.");
		ui_interface->addCheckButton("Band 2:  Solo", &fCheckbox5);
		ui_interface->declare(&fHbargraph3, "1", "");
		ui_interface->declare(&fHbargraph3, "7", "");
		ui_interface->addHorizontalBargraph("Band 2:   Outgain", &fHbargraph3, FAUSTFLOAT(0.0f), FAUSTFLOAT(1.0f));
		ui_interface->declare(&fCheckbox1, "0.5", "");
		ui_interface->declare(&fCheckbox1, "tooltip", "When this is checked, the compressor is enabled. If not, sound is muted.");
		ui_interface->addCheckButton("Band 2: Bypass", &fCheckbox1);
		ui_interface->declare(&fHslider8, "2", "");
		ui_interface->declare(&fHslider8, "style", "slider");
		ui_interface->declare(&fHslider8, "tooltip", "A compression Ratio of N means that for each N dB increase in input signal level above Threshold, the output level goes up 1 dB");
		ui_interface->addHorizontalSlider("Band 2: Ratio", &fHslider8, FAUSTFLOAT(2.0f), FAUSTFLOAT(1.0f), FAUSTFLOAT(2e+01f), FAUSTFLOAT(0.1f));
		ui_interface->declare(&fHslider9, "3", "");
		ui_interface->declare(&fHslider9, "style", "slider");
		ui_interface->declare(&fHslider9, "tooltip", "When the signal level exceeds the Threshold (in dB), its level is compressed according to the Ratio");
		ui_interface->declare(&fHslider9, "unit", "dB");
		ui_interface->addHorizontalSlider("Band 2: Threshold", &fHslider9, FAUSTFLOAT(-2e+01f), FAUSTFLOAT(-1e+02f), FAUSTFLOAT(1e+01f), FAUSTFLOAT(0.1f));
		ui_interface->declare(&fHslider7, "4", "");
		ui_interface->declare(&fHslider7, "style", "slider");
		ui_interface->declare(&fHslider7, "tooltip", "Time constant in ms (1/e smoothing time) for the compression gain to approach (exponentially) a new lower target level (the compression `kicking in')");
		ui_interface->declare(&fHslider7, "unit", "ms");
		ui_interface->addHorizontalSlider("Band 2: Attack", &fHslider7, FAUSTFLOAT(1e+02f), FAUSTFLOAT(0.0f), FAUSTFLOAT(5e+02f), FAUSTFLOAT(0.1f));
		ui_interface->declare(&fHslider6, "5", "");
		ui_interface->declare(&fHslider6, "style", "slider");
		ui_interface->declare(&fHslider6, "tooltip", "Time constant in ms (1/e smoothing time) for the compression gain to approach (exponentially) a new higher target level (the compression 'releasing')");
		ui_interface->declare(&fHslider6, "unit", "ms");
		ui_interface->addHorizontalSlider("Band 2: Release", &fHslider6, FAUSTFLOAT(2e+02f), FAUSTFLOAT(0.0f), FAUSTFLOAT(1e+03f), FAUSTFLOAT(0.1f));
		ui_interface->declare(&fHbargraph2, "1", "");
		ui_interface->declare(&fHbargraph2, "6", "");
		ui_interface->declare(&fHbargraph2, "tooltip", "dummy tooltip");
		ui_interface->addHorizontalBargraph("Band 2: Input Gain bargraph", &fHbargraph2, FAUSTFLOAT(0.0f), FAUSTFLOAT(1.0f));
		ui_interface->declare(&fHslider5, "2", "");
		ui_interface->declare(&fHslider5, "6", "");
		ui_interface->declare(&fHslider5, "tooltip", "The compressed-signal output level is increased by this amount (in dB) to make up for the level lost due to compression");
		ui_interface->declare(&fHslider5, "unit", "dB");
		ui_interface->addHorizontalSlider("Band 2: Input Gain", &fHslider5, FAUSTFLOAT(0.0f), FAUSTFLOAT(-4e+01f), FAUSTFLOAT(4e+01f), FAUSTFLOAT(0.1f));
		ui_interface->declare(&fHslider17, "2", "");
		ui_interface->declare(&fHslider17, "7", "");
		ui_interface->declare(&fHslider17, "tooltip", "The compressed-signal output level is increased by this amount (in dB) to make up for the level lost due to compression");
		ui_interface->declare(&fHslider17, "unit", "dB");
		ui_interface->addHorizontalSlider("Band 2: Output Gain", &fHslider17, FAUSTFLOAT(0.0f), FAUSTFLOAT(-4e+01f), FAUSTFLOAT(4e+01f), FAUSTFLOAT(0.1f));
		ui_interface->declare(&fCheckbox6, "0", "");
		ui_interface->declare(&fCheckbox6, "tooltip", "When this is checked, the compressor is enabled. If not, sound is muted.");
		ui_interface->addCheckButton("Band 3:  Solo", &fCheckbox6);
		ui_interface->declare(&fHbargraph5, "1", "");
		ui_interface->declare(&fHbargraph5, "7", "");
		ui_interface->addHorizontalBargraph("Band 3:   Outgain", &fHbargraph5, FAUSTFLOAT(0.0f), FAUSTFLOAT(1.0f));
		ui_interface->declare(&fCheckbox2, "0.5", "");
		ui_interface->declare(&fCheckbox2, "tooltip", "When this is checked, the compressor is enabled. If not, sound is muted.");
		ui_interface->addCheckButton("Band 3: Bypass", &fCheckbox2);
		ui_interface->declare(&fHslider13, "2", "");
		ui_interface->declare(&fHslider13, "style", "slider");
		ui_interface->declare(&fHslider13, "tooltip", "A compression Ratio of N means that for each N dB increase in input signal level above Threshold, the output level goes up 1 dB");
		ui_interface->addHorizontalSlider("Band 3: Ratio", &fHslider13, FAUSTFLOAT(2.0f), FAUSTFLOAT(1.0f), FAUSTFLOAT(2e+01f), FAUSTFLOAT(0.1f));
		ui_interface->declare(&fHslider14, "3", "");
		ui_interface->declare(&fHslider14, "style", "slider");
		ui_interface->declare(&fHslider14, "tooltip", "When the signal level exceeds the Threshold (in dB), its level is compressed according to the Ratio");
		ui_interface->declare(&fHslider14, "unit", "dB");
		ui_interface->addHorizontalSlider("Band 3: Threshold", &fHslider14, FAUSTFLOAT(-2e+01f), FAUSTFLOAT(-1e+02f), FAUSTFLOAT(1e+01f), FAUSTFLOAT(0.1f));
		ui_interface->declare(&fHslider12, "4", "");
		ui_interface->declare(&fHslider12, "style", "slider");
		ui_interface->declare(&fHslider12, "tooltip", "Time constant in ms (1/e smoothing time) for the compression gain to approach (exponentially) a new lower target level (the compression `kicking in')");
		ui_interface->declare(&fHslider12, "unit", "ms");
		ui_interface->addHorizontalSlider("Band 3: Attack", &fHslider12, FAUSTFLOAT(1e+02f), FAUSTFLOAT(0.0f), FAUSTFLOAT(5e+02f), FAUSTFLOAT(0.1f));
		ui_interface->declare(&fHslider11, "5", "");
		ui_interface->declare(&fHslider11, "style", "slider");
		ui_interface->declare(&fHslider11, "tooltip", "Time constant in ms (1/e smoothing time) for the compression gain to approach (exponentially) a new higher target level (the compression 'releasing')");
		ui_interface->declare(&fHslider11, "unit", "ms");
		ui_interface->addHorizontalSlider("Band 3: Release", &fHslider11, FAUSTFLOAT(2e+02f), FAUSTFLOAT(0.0f), FAUSTFLOAT(1e+03f), FAUSTFLOAT(0.1f));
		ui_interface->declare(&fHbargraph4, "1", "");
		ui_interface->declare(&fHbargraph4, "6", "");
		ui_interface->declare(&fHbargraph4, "tooltip", "dummy tooltip");
		ui_interface->addHorizontalBargraph("Band 3: Input Gain bargraph", &fHbargraph4, FAUSTFLOAT(0.0f), FAUSTFLOAT(1.0f));
		ui_interface->declare(&fHslider10, "2", "");
		ui_interface->declare(&fHslider10, "6", "");
		ui_interface->declare(&fHslider10, "tooltip", "The compressed-signal output level is increased by this amount (in dB) to make up for the level lost due to compression");
		ui_interface->declare(&fHslider10, "unit", "dB");
		ui_interface->addHorizontalSlider("Band 3: Input Gain", &fHslider10, FAUSTFLOAT(0.0f), FAUSTFLOAT(-4e+01f), FAUSTFLOAT(4e+01f), FAUSTFLOAT(0.1f));
		ui_interface->declare(&fHslider18, "2", "");
		ui_interface->declare(&fHslider18, "7", "");
		ui_interface->declare(&fHslider18, "tooltip", "The compressed-signal output level is increased by this amount (in dB) to make up for the level lost due to compression");
		ui_interface->declare(&fHslider18, "unit", "dB");
		ui_interface->addHorizontalSlider("Band 3: Output Gain", &fHslider18, FAUSTFLOAT(0.0f), FAUSTFLOAT(-4e+01f), FAUSTFLOAT(4e+01f), FAUSTFLOAT(0.1f));
		ui_interface->declare(&fVslider1, "C", "");
		ui_interface->declare(&fVslider1, "style", "knob");
		ui_interface->declare(&fVslider1, "tooltip", "Center-frequency of second-order Regalia-Mitra peaking equalizer section 1");
		ui_interface->declare(&fVslider1, "unit", "Hz");
		ui_interface->addVerticalSlider("Split Freq 1", &fVslider1, FAUSTFLOAT(166.0f), FAUSTFLOAT(4e+01f), FAUSTFLOAT(999.0f), FAUSTFLOAT(1.0f));
		ui_interface->declare(&fVslider0, "D", "");
		ui_interface->declare(&fVslider0, "style", "knob");
		ui_interface->declare(&fVslider0, "tooltip", "Center-frequency of second-order Regalia-Mitra peaking equalizer section 1");
		ui_interface->declare(&fVslider0, "unit", "Hz");
		ui_interface->addVerticalSlider("Split Freq 2", &fVslider0, FAUSTFLOAT(1.5e+03f), FAUSTFLOAT(1e+03f), FAUSTFLOAT(1.5e+04f), FAUSTFLOAT(1.0f));
		ui_interface->declare(&fCheckbox3, "E", "");
		ui_interface->addCheckButton("Limiter Bypass", &fCheckbox3);
		ui_interface->declare(&fHslider15, "F", "");
		ui_interface->declare(&fHslider15, "tooltip", "Adjust overall gain.");
		ui_interface->declare(&fHslider15, "unit", "dB");
		ui_interface->addHorizontalSlider("Limiter Input Gain", &fHslider15, FAUSTFLOAT(0.0f), FAUSTFLOAT(-4e+01f), FAUSTFLOAT(4e+01f), FAUSTFLOAT(0.1f));
		ui_interface->declare(&fVslider4, "G", "");
		ui_interface->declare(&fVslider4, "unit", ":1");
		ui_interface->addVerticalSlider("Limiter Ratio", &fVslider4, FAUSTFLOAT(4.0f), FAUSTFLOAT(4.0f), FAUSTFLOAT(2e+01f), FAUSTFLOAT(1.0f));
		ui_interface->declare(&fVslider3, "H", "");
		ui_interface->declare(&fVslider3, "unit", "us");
		ui_interface->addVerticalSlider("Limiter Attack", &fVslider3, FAUSTFLOAT(8e+02f), FAUSTFLOAT(2e+01f), FAUSTFLOAT(8e+02f), FAUSTFLOAT(1.0f));
		ui_interface->declare(&fVslider2, "I", "");
		ui_interface->declare(&fVslider2, "unit", "ms");
		ui_interface->addVerticalSlider("Limiter Release", &fVslider2, FAUSTFLOAT(5e+02f), FAUSTFLOAT(5e+01f), FAUSTFLOAT(1.1e+03f), FAUSTFLOAT(1.0f));
		ui_interface->declare(&fHslider20, "J", "");
		ui_interface->declare(&fHslider20, "tooltip", "Adjust overall gain.");
		ui_interface->declare(&fHslider20, "unit", "dB");
		ui_interface->addHorizontalSlider("Limiter Output Gain", &fHslider20, FAUSTFLOAT(0.0f), FAUSTFLOAT(-4e+01f), FAUSTFLOAT(4e+01f), FAUSTFLOAT(0.1f));
		ui_interface->declare(&fHslider19, "K", "");
		ui_interface->declare(&fHslider19, "tooltip", "Adjust overall gain.");
		ui_interface->declare(&fHslider19, "unit", "dB");
		ui_interface->addHorizontalSlider("Final Output Gain", &fHslider19, FAUSTFLOAT(0.0f), FAUSTFLOAT(-4e+01f), FAUSTFLOAT(4e+01f), FAUSTFLOAT(0.1f));
		ui_interface->closeBox();
	}
	
	void compute(int count, FAUSTFLOAT** RESTRICT inputs, FAUSTFLOAT** RESTRICT outputs) {
		FAUSTFLOAT* input0_ptr = inputs[0];
		FAUSTFLOAT* input1_ptr = inputs[1];
		FAUSTFLOAT* output0_ptr = outputs[0];
		FAUSTFLOAT* output1_ptr = outputs[1];
		float fSlow0 = std::tan(fConst1 * float(fVslider0));
		float fSlow1 = 1.0f / fSlow0;
		float fSlow2 = 1.0f / (fSlow1 + 1.0f);
		float fSlow3 = 1.0f - fSlow1;
		float fYec0_tmp[36];
		float* fYec0 = &fYec0_tmp[4];
		float fRec3_tmp[36];
		float* fRec3 = &fRec3_tmp[4];
		float fSlow4 = (fSlow1 + 1.0f) / fSlow0 + 1.0f;
		float fSlow5 = 1.0f / fSlow4;
		float fSlow6 = (fSlow1 + -1.0f) / fSlow0 + 1.0f;
		float fSlow7 = Multibandcomp_dsp_faustpower2_f(fSlow0);
		float fSlow8 = 2.0f * (1.0f - 1.0f / fSlow7);
		float fRec2_tmp[36];
		float* fRec2 = &fRec2_tmp[4];
		float fSlow9 = std::tan(fConst1 * float(fVslider1));
		float fSlow10 = 1.0f / fSlow9;
		float fSlow11 = fSlow10 + 1.0f;
		float fSlow12 = 1.0f / fSlow11;
		float fSlow13 = 1.0f - fSlow10;
		float fYec1_tmp[36];
		float* fYec1 = &fYec1_tmp[4];
		float fRec1_tmp[36];
		float* fRec1 = &fRec1_tmp[4];
		float fSlow14 = (fSlow10 + 1.0f) / fSlow9 + 1.0f;
		float fSlow15 = 1.0f / fSlow14;
		float fSlow16 = (fSlow10 + -1.0f) / fSlow9 + 1.0f;
		float fSlow17 = Multibandcomp_dsp_faustpower2_f(fSlow9);
		float fSlow18 = 2.0f * (1.0f - 1.0f / fSlow17);
		float fRec0_tmp[36];
		float* fRec0 = &fRec0_tmp[4];
		float fYec2_tmp[36];
		float* fYec2 = &fYec2_tmp[4];
		float fRec10_tmp[36];
		float* fRec10 = &fRec10_tmp[4];
		float fRec9_tmp[36];
		float* fRec9 = &fRec9_tmp[4];
		float fYec3_tmp[36];
		float* fYec3 = &fYec3_tmp[4];
		float fRec8_tmp[36];
		float* fRec8 = &fRec8_tmp[4];
		float fRec7_tmp[36];
		float* fRec7 = &fRec7_tmp[4];
		float fSlow19 = std::pow(1e+01f, 0.05f * float(fHslider0));
		int iSlow20 = int(float(fCheckbox0));
		float fZec0[32];
		float fZec1[32];
		float fZec2[32];
		float fZec3[32];
		float fZec4[32];
		float fSlow21 = std::max<float>(fConst2, 0.001f * float(fHslider1));
		float fSlow22 = ((fSlow21 > 0.0f) ? std::exp(-(fConst2 / fSlow21)) : 0.0f);
		float fSlow23 = 1.0f - fSlow22;
		float fRec6_tmp[36];
		float* fRec6 = &fRec6_tmp[4];
		float fSlow24 = std::max<float>(fConst2, 0.001f * float(fHslider2));
		float fSlow25 = ((fSlow24 > 0.0f) ? std::exp(-(fConst2 / fSlow24)) : 0.0f);
		float fSlow26 = 1.0f - fSlow25;
		float fRec5_tmp[36];
		float* fRec5 = &fRec5_tmp[4];
		float fSlow27 = (((0.5f * fSlow24) > 0.0f) ? std::exp(-(fConst3 / fSlow24)) : 0.0f);
		float fSlow28 = 1.0f / float(fHslider3) + -1.0f;
		float fSlow29 = float(fHslider4);
		float fSlow30 = 1.0f - fSlow27;
		float fRec4_tmp[36];
		float* fRec4 = &fRec4_tmp[4];
		float fRec12_tmp[36];
		float* fRec12 = &fRec12_tmp[4];
		float fRec11_tmp[36];
		float* fRec11 = &fRec11_tmp[4];
		float fRec17_tmp[36];
		float* fRec17 = &fRec17_tmp[4];
		float fRec16_tmp[36];
		float* fRec16 = &fRec16_tmp[4];
		float fSlow31 = std::pow(1e+01f, 0.05f * float(fHslider5));
		int iSlow32 = int(float(fCheckbox1));
		float fSlow33 = 1.0f / (fSlow17 * fSlow14);
		float fZec5[32];
		float fZec6[32];
		float fZec7[32];
		float fZec8[32];
		float fZec9[32];
		float fSlow34 = std::max<float>(fConst2, 0.001f * float(fHslider6));
		float fSlow35 = ((fSlow34 > 0.0f) ? std::exp(-(fConst2 / fSlow34)) : 0.0f);
		float fSlow36 = 1.0f - fSlow35;
		float fRec15_tmp[36];
		float* fRec15 = &fRec15_tmp[4];
		float fSlow37 = std::max<float>(fConst2, 0.001f * float(fHslider7));
		float fSlow38 = ((fSlow37 > 0.0f) ? std::exp(-(fConst2 / fSlow37)) : 0.0f);
		float fSlow39 = 1.0f - fSlow38;
		float fRec14_tmp[36];
		float* fRec14 = &fRec14_tmp[4];
		float fSlow40 = (((0.5f * fSlow37) > 0.0f) ? std::exp(-(fConst3 / fSlow37)) : 0.0f);
		float fSlow41 = 1.0f / float(fHslider8) + -1.0f;
		float fSlow42 = float(fHslider9);
		float fSlow43 = 1.0f - fSlow40;
		float fRec13_tmp[36];
		float* fRec13 = &fRec13_tmp[4];
		float fRec20_tmp[36];
		float* fRec20 = &fRec20_tmp[4];
		float fRec19_tmp[36];
		float* fRec19 = &fRec19_tmp[4];
		float fSlow44 = 1.0f / (fSlow7 * fSlow4);
		float fSlow45 = 1.0f / (fSlow11 / fSlow9 + 1.0f);
		float fSlow46 = 1.0f - fSlow13 / fSlow9;
		float fZec10[32];
		float fRec18_tmp[36];
		float* fRec18 = &fRec18_tmp[4];
		float fRec26_tmp[36];
		float* fRec26 = &fRec26_tmp[4];
		float fRec25_tmp[36];
		float* fRec25 = &fRec25_tmp[4];
		float fZec11[32];
		float fRec24_tmp[36];
		float* fRec24 = &fRec24_tmp[4];
		float fSlow47 = std::pow(1e+01f, 0.05f * float(fHslider10));
		int iSlow48 = int(float(fCheckbox2));
		float fZec12[32];
		float fZec13[32];
		float fZec14[32];
		float fZec15[32];
		float fZec16[32];
		float fSlow49 = std::max<float>(fConst2, 0.001f * float(fHslider11));
		float fSlow50 = ((fSlow49 > 0.0f) ? std::exp(-(fConst2 / fSlow49)) : 0.0f);
		float fSlow51 = 1.0f - fSlow50;
		float fRec23_tmp[36];
		float* fRec23 = &fRec23_tmp[4];
		float fSlow52 = std::max<float>(fConst2, 0.001f * float(fHslider12));
		float fSlow53 = ((fSlow52 > 0.0f) ? std::exp(-(fConst2 / fSlow52)) : 0.0f);
		float fSlow54 = 1.0f - fSlow53;
		float fRec22_tmp[36];
		float* fRec22 = &fRec22_tmp[4];
		float fSlow55 = (((0.5f * fSlow52) > 0.0f) ? std::exp(-(fConst3 / fSlow52)) : 0.0f);
		float fSlow56 = 1.0f / float(fHslider13) + -1.0f;
		float fSlow57 = float(fHslider14);
		float fSlow58 = 1.0f - fSlow55;
		float fRec21_tmp[36];
		float* fRec21 = &fRec21_tmp[4];
		float fZec17[32];
		float fZec18[32];
		float fZec19[32];
		float fZec20[32];
		float fRec32_tmp[36];
		float* fRec32 = &fRec32_tmp[4];
		float fRec31_tmp[36];
		float* fRec31 = &fRec31_tmp[4];
		float fRec30_tmp[36];
		float* fRec30 = &fRec30_tmp[4];
		float fZec21[32];
		float fZec22[32];
		float fZec23[32];
		float fZec24[32];
		float fRec35_tmp[36];
		float* fRec35 = &fRec35_tmp[4];
		float fRec34_tmp[36];
		float* fRec34 = &fRec34_tmp[4];
		float fRec33_tmp[36];
		float* fRec33 = &fRec33_tmp[4];
		float fZec25[32];
		float fZec26[32];
		float fZec27[32];
		float fZec28[32];
		float fRec38_tmp[36];
		float* fRec38 = &fRec38_tmp[4];
		float fRec37_tmp[36];
		float* fRec37 = &fRec37_tmp[4];
		float fRec36_tmp[36];
		float* fRec36 = &fRec36_tmp[4];
		float fSlow59 = pun_int_to_float(int(8388608.0f * (std::max<float>(-126.0f, 0.1660964f * float(fHslider15)) + 126.942696f)));
		int iSlow60 = int(float(fCheckbox3));
		int iSlow61 = int(float(fCheckbox4));
		int iSlow62 = int(float(fCheckbox5));
		int iSlow63 = int(float(fCheckbox6));
		int iSlow64 = ((iSlow61 == 0) & (iSlow62 == 0)) & (iSlow63 == 0);
		float fSlow65 = float(iSlow61 | iSlow64);
		float fSlow66 = std::pow(1e+01f, 0.05f * float(fHslider16));
		float fSlow67 = fSlow65 * fSlow19 * fSlow66;
		float fZec29[32];
		float fSlow68 = float(iSlow62 | iSlow64);
		float fSlow69 = std::pow(1e+01f, 0.05f * float(fHslider17));
		float fSlow70 = fSlow68 * fSlow31 * fSlow69;
		float fZec30[32];
		float fSlow71 = float(iSlow63 | iSlow64);
		float fSlow72 = std::pow(1e+01f, 0.05f * float(fHslider18));
		float fSlow73 = fSlow71 * fSlow47 * fSlow72;
		float fZec31[32];
		float fZec32[32];
		float fZec33[32];
		float fSlow74 = fSlow65 * fSlow66;
		float fZec34[32];
		float fSlow75 = fSlow68 * fSlow69;
		float fZec35[32];
		float fSlow76 = fSlow71 * fSlow72;
		float fZec36[32];
		float fZec37[32];
		float fZec38[32];
		float fZec39[32];
		float fSlow77 = float(fVslider2);
		float fSlow78 = (((0.001f * fSlow77) > 0.0f) ? std::exp(-(fConst4 / fSlow77)) : 0.0f);
		float fSlow79 = 1.0f - fSlow78;
		float fRec29_tmp[36];
		float* fRec29 = &fRec29_tmp[4];
		float fSlow80 = float(fVslider3);
		float fSlow81 = (((1e-06f * fSlow80) > 0.0f) ? std::exp(-(fConst5 / fSlow80)) : 0.0f);
		float fSlow82 = 1.0f - fSlow81;
		float fRec28_tmp[36];
		float* fRec28 = &fRec28_tmp[4];
		float fSlow83 = (((5e-07f * fSlow80) > 0.0f) ? std::exp(-(fConst6 / fSlow80)) : 0.0f);
		float fSlow84 = 1.0f / float(fVslider4) + -1.0f;
		float fSlow85 = 1.0f - fSlow83;
		float fRec27_tmp[36];
		float* fRec27 = &fRec27_tmp[4];
		float fSlow86 = std::pow(1e+01f, 0.05f * float(fHslider19));
		float fSlow87 = fSlow59 * pun_int_to_float(int(8388608.0f * (std::max<float>(-126.0f, 0.1660964f * float(fHslider20)) + 126.942696f)));
		float fZec40[32];
		int vindex = 0;
		/* Main loop */
		for (vindex = 0; vindex <= (count - 32); vindex = vindex + 32) {
			FAUSTFLOAT* input0 = &input0_ptr[vindex];
			FAUSTFLOAT* input1 = &input1_ptr[vindex];
			FAUSTFLOAT* output0 = &output0_ptr[vindex];
			FAUSTFLOAT* output1 = &output1_ptr[vindex];
			int vsize = 32;
			/* Vectorizable loop 0 */
			/* Pre code */
			for (int j0 = 0; j0 < 4; j0 = j0 + 1) {
				fYec0_tmp[j0] = fYec0_perm[j0];
			}
			/* Compute code */
			for (int i = 0; i < vsize; i = i + 1) {
				fYec0[i] = float(input0[i]);
			}
			/* Post code */
			for (int j1 = 0; j1 < 4; j1 = j1 + 1) {
				fYec0_perm[j1] = fYec0_tmp[vsize + j1];
			}
			/* Vectorizable loop 1 */
			/* Pre code */
			for (int j12 = 0; j12 < 4; j12 = j12 + 1) {
				fYec2_tmp[j12] = fYec2_perm[j12];
			}
			/* Compute code */
			for (int i = 0; i < vsize; i = i + 1) {
				fYec2[i] = float(input1[i]);
			}
			/* Post code */
			for (int j13 = 0; j13 < 4; j13 = j13 + 1) {
				fYec2_perm[j13] = fYec2_tmp[vsize + j13];
			}
			/* Recursive loop 2 */
			/* Pre code */
			for (int j2 = 0; j2 < 4; j2 = j2 + 1) {
				fRec3_tmp[j2] = fRec3_perm[j2];
			}
			/* Compute code */
			for (int i = 0; i < vsize; i = i + 1) {
				fRec3[i] = -(fSlow2 * (fSlow3 * fRec3[i - 1] - (float(input0[i]) + fYec0[i - 1])));
			}
			/* Post code */
			for (int j3 = 0; j3 < 4; j3 = j3 + 1) {
				fRec3_perm[j3] = fRec3_tmp[vsize + j3];
			}
			/* Recursive loop 3 */
			/* Pre code */
			for (int j14 = 0; j14 < 4; j14 = j14 + 1) {
				fRec10_tmp[j14] = fRec10_perm[j14];
			}
			/* Compute code */
			for (int i = 0; i < vsize; i = i + 1) {
				fRec10[i] = -(fSlow2 * (fSlow3 * fRec10[i - 1] - (float(input1[i]) + fYec2[i - 1])));
			}
			/* Post code */
			for (int j15 = 0; j15 < 4; j15 = j15 + 1) {
				fRec10_perm[j15] = fRec10_tmp[vsize + j15];
			}
			/* Recursive loop 4 */
			/* Pre code */
			for (int j4 = 0; j4 < 4; j4 = j4 + 1) {
				fRec2_tmp[j4] = fRec2_perm[j4];
			}
			/* Compute code */
			for (int i = 0; i < vsize; i = i + 1) {
				fRec2[i] = fRec3[i] - fSlow5 * (fSlow6 * fRec2[i - 2] + fSlow8 * fRec2[i - 1]);
			}
			/* Post code */
			for (int j5 = 0; j5 < 4; j5 = j5 + 1) {
				fRec2_perm[j5] = fRec2_tmp[vsize + j5];
			}
			/* Recursive loop 5 */
			/* Pre code */
			for (int j16 = 0; j16 < 4; j16 = j16 + 1) {
				fRec9_tmp[j16] = fRec9_perm[j16];
			}
			/* Compute code */
			for (int i = 0; i < vsize; i = i + 1) {
				fRec9[i] = fRec10[i] - fSlow5 * (fSlow6 * fRec9[i - 2] + fSlow8 * fRec9[i - 1]);
			}
			/* Post code */
			for (int j17 = 0; j17 < 4; j17 = j17 + 1) {
				fRec9_perm[j17] = fRec9_tmp[vsize + j17];
			}
			/* Vectorizable loop 6 */
			/* Pre code */
			for (int j6 = 0; j6 < 4; j6 = j6 + 1) {
				fYec1_tmp[j6] = fYec1_perm[j6];
			}
			/* Compute code */
			for (int i = 0; i < vsize; i = i + 1) {
				fYec1[i] = fSlow5 * (fRec2[i - 2] + fRec2[i] + 2.0f * fRec2[i - 1]);
			}
			/* Post code */
			for (int j7 = 0; j7 < 4; j7 = j7 + 1) {
				fYec1_perm[j7] = fYec1_tmp[vsize + j7];
			}
			/* Vectorizable loop 7 */
			/* Pre code */
			for (int j18 = 0; j18 < 4; j18 = j18 + 1) {
				fYec3_tmp[j18] = fYec3_perm[j18];
			}
			/* Compute code */
			for (int i = 0; i < vsize; i = i + 1) {
				fYec3[i] = fSlow5 * (fRec9[i - 2] + fRec9[i] + 2.0f * fRec9[i - 1]);
			}
			/* Post code */
			for (int j19 = 0; j19 < 4; j19 = j19 + 1) {
				fYec3_perm[j19] = fYec3_tmp[vsize + j19];
			}
			/* Recursive loop 8 */
			/* Pre code */
			for (int j44 = 0; j44 < 4; j44 = j44 + 1) {
				fRec20_tmp[j44] = fRec20_perm[j44];
			}
			/* Compute code */
			for (int i = 0; i < vsize; i = i + 1) {
				fRec20[i] = -(fSlow2 * (fSlow3 * fRec20[i - 1] - fSlow1 * (float(input0[i]) - fYec0[i - 1])));
			}
			/* Post code */
			for (int j45 = 0; j45 < 4; j45 = j45 + 1) {
				fRec20_perm[j45] = fRec20_tmp[vsize + j45];
			}
			/* Recursive loop 9 */
			/* Pre code */
			for (int j50 = 0; j50 < 4; j50 = j50 + 1) {
				fRec26_tmp[j50] = fRec26_perm[j50];
			}
			/* Compute code */
			for (int i = 0; i < vsize; i = i + 1) {
				fRec26[i] = -(fSlow2 * (fSlow3 * fRec26[i - 1] - fSlow1 * (float(input1[i]) - fYec2[i - 1])));
			}
			/* Post code */
			for (int j51 = 0; j51 < 4; j51 = j51 + 1) {
				fRec26_perm[j51] = fRec26_tmp[vsize + j51];
			}
			/* Recursive loop 10 */
			/* Pre code */
			for (int j8 = 0; j8 < 4; j8 = j8 + 1) {
				fRec1_tmp[j8] = fRec1_perm[j8];
			}
			/* Compute code */
			for (int i = 0; i < vsize; i = i + 1) {
				fRec1[i] = -(fSlow12 * (fSlow13 * fRec1[i - 1] - (fYec1[i] + fYec1[i - 1])));
			}
			/* Post code */
			for (int j9 = 0; j9 < 4; j9 = j9 + 1) {
				fRec1_perm[j9] = fRec1_tmp[vsize + j9];
			}
			/* Recursive loop 11 */
			/* Pre code */
			for (int j20 = 0; j20 < 4; j20 = j20 + 1) {
				fRec8_tmp[j20] = fRec8_perm[j20];
			}
			/* Compute code */
			for (int i = 0; i < vsize; i = i + 1) {
				fRec8[i] = -(fSlow12 * (fSlow13 * fRec8[i - 1] - (fYec3[i] + fYec3[i - 1])));
			}
			/* Post code */
			for (int j21 = 0; j21 < 4; j21 = j21 + 1) {
				fRec8_perm[j21] = fRec8_tmp[vsize + j21];
			}
			/* Recursive loop 12 */
			/* Pre code */
			for (int j30 = 0; j30 < 4; j30 = j30 + 1) {
				fRec12_tmp[j30] = fRec12_perm[j30];
			}
			/* Compute code */
			for (int i = 0; i < vsize; i = i + 1) {
				fRec12[i] = -(fSlow12 * (fSlow13 * fRec12[i - 1] - fSlow10 * (fYec1[i] - fYec1[i - 1])));
			}
			/* Post code */
			for (int j31 = 0; j31 < 4; j31 = j31 + 1) {
				fRec12_perm[j31] = fRec12_tmp[vsize + j31];
			}
			/* Recursive loop 13 */
			/* Pre code */
			for (int j34 = 0; j34 < 4; j34 = j34 + 1) {
				fRec17_tmp[j34] = fRec17_perm[j34];
			}
			/* Compute code */
			for (int i = 0; i < vsize; i = i + 1) {
				fRec17[i] = -(fSlow12 * (fSlow13 * fRec17[i - 1] - fSlow10 * (fYec3[i] - fYec3[i - 1])));
			}
			/* Post code */
			for (int j35 = 0; j35 < 4; j35 = j35 + 1) {
				fRec17_perm[j35] = fRec17_tmp[vsize + j35];
			}
			/* Recursive loop 14 */
			/* Pre code */
			for (int j46 = 0; j46 < 4; j46 = j46 + 1) {
				fRec19_tmp[j46] = fRec19_perm[j46];
			}
			/* Compute code */
			for (int i = 0; i < vsize; i = i + 1) {
				fRec19[i] = fRec20[i] - fSlow5 * (fSlow6 * fRec19[i - 2] + fSlow8 * fRec19[i - 1]);
			}
			/* Post code */
			for (int j47 = 0; j47 < 4; j47 = j47 + 1) {
				fRec19_perm[j47] = fRec19_tmp[vsize + j47];
			}
			/* Recursive loop 15 */
			/* Pre code */
			for (int j52 = 0; j52 < 4; j52 = j52 + 1) {
				fRec25_tmp[j52] = fRec25_perm[j52];
			}
			/* Compute code */
			for (int i = 0; i < vsize; i = i + 1) {
				fRec25[i] = fRec26[i] - fSlow5 * (fSlow6 * fRec25[i - 2] + fSlow8 * fRec25[i - 1]);
			}
			/* Post code */
			for (int j53 = 0; j53 < 4; j53 = j53 + 1) {
				fRec25_perm[j53] = fRec25_tmp[vsize + j53];
			}
			/* Recursive loop 16 */
			/* Pre code */
			for (int j10 = 0; j10 < 4; j10 = j10 + 1) {
				fRec0_tmp[j10] = fRec0_perm[j10];
			}
			/* Compute code */
			for (int i = 0; i < vsize; i = i + 1) {
				fRec0[i] = fRec1[i] - fSlow15 * (fSlow16 * fRec0[i - 2] + fSlow18 * fRec0[i - 1]);
			}
			/* Post code */
			for (int j11 = 0; j11 < 4; j11 = j11 + 1) {
				fRec0_perm[j11] = fRec0_tmp[vsize + j11];
			}
			/* Recursive loop 17 */
			/* Pre code */
			for (int j22 = 0; j22 < 4; j22 = j22 + 1) {
				fRec7_tmp[j22] = fRec7_perm[j22];
			}
			/* Compute code */
			for (int i = 0; i < vsize; i = i + 1) {
				fRec7[i] = fRec8[i] - fSlow15 * (fSlow16 * fRec7[i - 2] + fSlow18 * fRec7[i - 1]);
			}
			/* Post code */
			for (int j23 = 0; j23 < 4; j23 = j23 + 1) {
				fRec7_perm[j23] = fRec7_tmp[vsize + j23];
			}
			/* Recursive loop 18 */
			/* Pre code */
			for (int j32 = 0; j32 < 4; j32 = j32 + 1) {
				fRec11_tmp[j32] = fRec11_perm[j32];
			}
			/* Compute code */
			for (int i = 0; i < vsize; i = i + 1) {
				fRec11[i] = fRec12[i] - fSlow15 * (fSlow16 * fRec11[i - 2] + fSlow18 * fRec11[i - 1]);
			}
			/* Post code */
			for (int j33 = 0; j33 < 4; j33 = j33 + 1) {
				fRec11_perm[j33] = fRec11_tmp[vsize + j33];
			}
			/* Recursive loop 19 */
			/* Pre code */
			for (int j36 = 0; j36 < 4; j36 = j36 + 1) {
				fRec16_tmp[j36] = fRec16_perm[j36];
			}
			/* Compute code */
			for (int i = 0; i < vsize; i = i + 1) {
				fRec16[i] = fRec17[i] - fSlow15 * (fSlow16 * fRec16[i - 2] + fSlow18 * fRec16[i - 1]);
			}
			/* Post code */
			for (int j37 = 0; j37 < 4; j37 = j37 + 1) {
				fRec16_perm[j37] = fRec16_tmp[vsize + j37];
			}
			/* Recursive loop 20 */
			/* Pre code */
			for (int j48 = 0; j48 < 4; j48 = j48 + 1) {
				fRec18_tmp[j48] = fRec18_perm[j48];
			}
			/* Compute code */
			for (int i = 0; i < vsize; i = i + 1) {
				fZec10[i] = fSlow18 * fRec18[i - 1];
				fRec18[i] = fSlow44 * (fRec19[i - 2] + (fRec19[i] - 2.0f * fRec19[i - 1])) - fSlow45 * (fSlow46 * fRec18[i - 2] + fZec10[i]);
			}
			/* Post code */
			for (int j49 = 0; j49 < 4; j49 = j49 + 1) {
				fRec18_perm[j49] = fRec18_tmp[vsize + j49];
			}
			/* Recursive loop 21 */
			/* Pre code */
			for (int j54 = 0; j54 < 4; j54 = j54 + 1) {
				fRec24_tmp[j54] = fRec24_perm[j54];
			}
			/* Compute code */
			for (int i = 0; i < vsize; i = i + 1) {
				fZec11[i] = fSlow18 * fRec24[i - 1];
				fRec24[i] = fSlow44 * (fRec25[i - 2] + (fRec25[i] - 2.0f * fRec25[i - 1])) - fSlow45 * (fSlow46 * fRec24[i - 2] + fZec11[i]);
			}
			/* Post code */
			for (int j55 = 0; j55 < 4; j55 = j55 + 1) {
				fRec24_perm[j55] = fRec24_tmp[vsize + j55];
			}
			/* Vectorizable loop 22 */
			/* Compute code */
			for (int i = 0; i < vsize; i = i + 1) {
				fZec0[i] = fSlow15 * (fRec0[i - 2] + fRec0[i] + 2.0f * fRec0[i - 1]);
			}
			/* Vectorizable loop 23 */
			/* Compute code */
			for (int i = 0; i < vsize; i = i + 1) {
				fZec2[i] = fSlow15 * (fRec7[i - 2] + fRec7[i] + 2.0f * fRec7[i - 1]);
			}
			/* Vectorizable loop 24 */
			/* Compute code */
			for (int i = 0; i < vsize; i = i + 1) {
				fZec5[i] = fSlow33 * (fRec11[i - 2] + (fRec11[i] - 2.0f * fRec11[i - 1]));
			}
			/* Vectorizable loop 25 */
			/* Compute code */
			for (int i = 0; i < vsize; i = i + 1) {
				fZec7[i] = fSlow33 * (fRec16[i - 2] + (fRec16[i] - 2.0f * fRec16[i - 1]));
			}
			/* Vectorizable loop 26 */
			/* Compute code */
			for (int i = 0; i < vsize; i = i + 1) {
				fZec12[i] = fRec18[i - 2] + fSlow45 * (fZec10[i] + fSlow46 * fRec18[i]);
			}
			/* Vectorizable loop 27 */
			/* Compute code */
			for (int i = 0; i < vsize; i = i + 1) {
				fZec14[i] = fRec24[i - 2] + fSlow45 * (fZec11[i] + fSlow46 * fRec24[i]);
			}
			/* Vectorizable loop 28 */
			/* Compute code */
			for (int i = 0; i < vsize; i = i + 1) {
				fZec1[i] = ((iSlow20) ? 0.0f : fZec0[i]);
			}
			/* Vectorizable loop 29 */
			/* Compute code */
			for (int i = 0; i < vsize; i = i + 1) {
				fZec3[i] = ((iSlow20) ? 0.0f : fZec2[i]);
			}
			/* Vectorizable loop 30 */
			/* Compute code */
			for (int i = 0; i < vsize; i = i + 1) {
				fZec6[i] = ((iSlow32) ? 0.0f : fZec5[i]);
			}
			/* Vectorizable loop 31 */
			/* Compute code */
			for (int i = 0; i < vsize; i = i + 1) {
				fZec8[i] = ((iSlow32) ? 0.0f : fZec7[i]);
			}
			/* Vectorizable loop 32 */
			/* Compute code */
			for (int i = 0; i < vsize; i = i + 1) {
				fZec13[i] = ((iSlow48) ? 0.0f : fZec12[i]);
			}
			/* Vectorizable loop 33 */
			/* Compute code */
			for (int i = 0; i < vsize; i = i + 1) {
				fZec15[i] = ((iSlow48) ? 0.0f : fZec14[i]);
			}
			/* Vectorizable loop 34 */
			/* Compute code */
			for (int i = 0; i < vsize; i = i + 1) {
				fZec4[i] = std::fabs(std::fabs(fSlow19 * fZec1[i]) + std::fabs(fSlow19 * fZec3[i]));
			}
			/* Vectorizable loop 35 */
			/* Compute code */
			for (int i = 0; i < vsize; i = i + 1) {
				fZec9[i] = std::fabs(std::fabs(fSlow31 * fZec6[i]) + std::fabs(fSlow31 * fZec8[i]));
			}
			/* Vectorizable loop 36 */
			/* Compute code */
			for (int i = 0; i < vsize; i = i + 1) {
				fZec16[i] = std::fabs(std::fabs(fSlow47 * fZec13[i]) + std::fabs(fSlow47 * fZec15[i]));
			}
			/* Recursive loop 37 */
			/* Pre code */
			for (int j24 = 0; j24 < 4; j24 = j24 + 1) {
				fRec6_tmp[j24] = fRec6_perm[j24];
			}
			/* Compute code */
			for (int i = 0; i < vsize; i = i + 1) {
				fRec6[i] = std::max<float>(fZec4[i], fRec6[i - 1] * fSlow22 + fZec4[i] * fSlow23);
			}
			/* Post code */
			for (int j25 = 0; j25 < 4; j25 = j25 + 1) {
				fRec6_perm[j25] = fRec6_tmp[vsize + j25];
			}
			/* Recursive loop 38 */
			/* Pre code */
			for (int j38 = 0; j38 < 4; j38 = j38 + 1) {
				fRec15_tmp[j38] = fRec15_perm[j38];
			}
			/* Compute code */
			for (int i = 0; i < vsize; i = i + 1) {
				fRec15[i] = std::max<float>(fZec9[i], fRec15[i - 1] * fSlow35 + fZec9[i] * fSlow36);
			}
			/* Post code */
			for (int j39 = 0; j39 < 4; j39 = j39 + 1) {
				fRec15_perm[j39] = fRec15_tmp[vsize + j39];
			}
			/* Recursive loop 39 */
			/* Pre code */
			for (int j56 = 0; j56 < 4; j56 = j56 + 1) {
				fRec23_tmp[j56] = fRec23_perm[j56];
			}
			/* Compute code */
			for (int i = 0; i < vsize; i = i + 1) {
				fRec23[i] = std::max<float>(fZec16[i], fRec23[i - 1] * fSlow50 + fZec16[i] * fSlow51);
			}
			/* Post code */
			for (int j57 = 0; j57 < 4; j57 = j57 + 1) {
				fRec23_perm[j57] = fRec23_tmp[vsize + j57];
			}
			/* Recursive loop 40 */
			/* Pre code */
			for (int j26 = 0; j26 < 4; j26 = j26 + 1) {
				fRec5_tmp[j26] = fRec5_perm[j26];
			}
			/* Compute code */
			for (int i = 0; i < vsize; i = i + 1) {
				fRec5[i] = fRec5[i - 1] * fSlow25 + fRec6[i] * fSlow26;
			}
			/* Post code */
			for (int j27 = 0; j27 < 4; j27 = j27 + 1) {
				fRec5_perm[j27] = fRec5_tmp[vsize + j27];
			}
			/* Recursive loop 41 */
			/* Pre code */
			for (int j40 = 0; j40 < 4; j40 = j40 + 1) {
				fRec14_tmp[j40] = fRec14_perm[j40];
			}
			/* Compute code */
			for (int i = 0; i < vsize; i = i + 1) {
				fRec14[i] = fRec14[i - 1] * fSlow38 + fRec15[i] * fSlow39;
			}
			/* Post code */
			for (int j41 = 0; j41 < 4; j41 = j41 + 1) {
				fRec14_perm[j41] = fRec14_tmp[vsize + j41];
			}
			/* Recursive loop 42 */
			/* Pre code */
			for (int j58 = 0; j58 < 4; j58 = j58 + 1) {
				fRec22_tmp[j58] = fRec22_perm[j58];
			}
			/* Compute code */
			for (int i = 0; i < vsize; i = i + 1) {
				fRec22[i] = fRec22[i - 1] * fSlow53 + fRec23[i] * fSlow54;
			}
			/* Post code */
			for (int j59 = 0; j59 < 4; j59 = j59 + 1) {
				fRec22_perm[j59] = fRec22_tmp[vsize + j59];
			}
			/* Recursive loop 43 */
			/* Pre code */
			for (int j28 = 0; j28 < 4; j28 = j28 + 1) {
				fRec4_tmp[j28] = fRec4_perm[j28];
			}
			/* Compute code */
			for (int i = 0; i < vsize; i = i + 1) {
				fRec4[i] = fRec4[i - 1] * fSlow27 + fSlow28 * std::max<float>(8.685889f * (8.262958e-08f * float(pun_float_to_int(fRec5[i])) + -87.98997f) - fSlow29, 0.0f) * fSlow30;
			}
			/* Post code */
			for (int j29 = 0; j29 < 4; j29 = j29 + 1) {
				fRec4_perm[j29] = fRec4_tmp[vsize + j29];
			}
			/* Recursive loop 44 */
			/* Pre code */
			for (int j42 = 0; j42 < 4; j42 = j42 + 1) {
				fRec13_tmp[j42] = fRec13_perm[j42];
			}
			/* Compute code */
			for (int i = 0; i < vsize; i = i + 1) {
				fRec13[i] = fRec13[i - 1] * fSlow40 + fSlow41 * std::max<float>(8.685889f * (8.262958e-08f * float(pun_float_to_int(fRec14[i])) + -87.98997f) - fSlow42, 0.0f) * fSlow43;
			}
			/* Post code */
			for (int j43 = 0; j43 < 4; j43 = j43 + 1) {
				fRec13_perm[j43] = fRec13_tmp[vsize + j43];
			}
			/* Recursive loop 45 */
			/* Pre code */
			for (int j60 = 0; j60 < 4; j60 = j60 + 1) {
				fRec21_tmp[j60] = fRec21_perm[j60];
			}
			/* Compute code */
			for (int i = 0; i < vsize; i = i + 1) {
				fRec21[i] = fRec21[i - 1] * fSlow55 + fSlow56 * std::max<float>(8.685889f * (8.262958e-08f * float(pun_float_to_int(fRec22[i])) + -87.98997f) - fSlow57, 0.0f) * fSlow58;
			}
			/* Post code */
			for (int j61 = 0; j61 < 4; j61 = j61 + 1) {
				fRec21_perm[j61] = fRec21_tmp[vsize + j61];
			}
			/* Vectorizable loop 46 */
			/* Compute code */
			for (int i = 0; i < vsize; i = i + 1) {
				fZec17[i] = pun_int_to_float(int(8388608.0f * (std::max<float>(-126.0f, 0.1660964f * fRec4[i]) + 126.942696f)));
			}
			/* Vectorizable loop 47 */
			/* Compute code */
			for (int i = 0; i < vsize; i = i + 1) {
				fZec21[i] = pun_int_to_float(int(8388608.0f * (std::max<float>(-126.0f, 0.1660964f * fRec13[i]) + 126.942696f)));
			}
			/* Vectorizable loop 48 */
			/* Compute code */
			for (int i = 0; i < vsize; i = i + 1) {
				fZec25[i] = pun_int_to_float(int(8388608.0f * (std::max<float>(-126.0f, 0.1660964f * fRec21[i]) + 126.942696f)));
			}
			/* Vectorizable loop 49 */
			/* Compute code */
			for (int i = 0; i < vsize; i = i + 1) {
				fZec18[i] = fZec1[i] * fZec17[i];
			}
			/* Vectorizable loop 50 */
			/* Compute code */
			for (int i = 0; i < vsize; i = i + 1) {
				fZec19[i] = fSlow19 * fZec3[i] * fZec17[i];
			}
			/* Vectorizable loop 51 */
			/* Compute code */
			for (int i = 0; i < vsize; i = i + 1) {
				fZec22[i] = fZec6[i] * fZec21[i];
			}
			/* Vectorizable loop 52 */
			/* Compute code */
			for (int i = 0; i < vsize; i = i + 1) {
				fZec23[i] = fSlow31 * fZec8[i] * fZec21[i];
			}
			/* Vectorizable loop 53 */
			/* Compute code */
			for (int i = 0; i < vsize; i = i + 1) {
				fZec26[i] = fZec13[i] * fZec25[i];
			}
			/* Vectorizable loop 54 */
			/* Compute code */
			for (int i = 0; i < vsize; i = i + 1) {
				fZec27[i] = fSlow47 * fZec15[i] * fZec25[i];
			}
			/* Vectorizable loop 55 */
			/* Compute code */
			for (int i = 0; i < vsize; i = i + 1) {
				fZec20[i] = std::fabs(std::fabs(fSlow19 * fZec18[i]) + std::fabs(fZec19[i]));
			}
			/* Vectorizable loop 56 */
			/* Compute code */
			for (int i = 0; i < vsize; i = i + 1) {
				fZec24[i] = std::fabs(std::fabs(fSlow31 * fZec22[i]) + std::fabs(fZec23[i]));
			}
			/* Vectorizable loop 57 */
			/* Compute code */
			for (int i = 0; i < vsize; i = i + 1) {
				fZec28[i] = std::fabs(std::fabs(fSlow47 * fZec26[i]) + std::fabs(fZec27[i]));
			}
			/* Recursive loop 58 */
			/* Pre code */
			for (int j62 = 0; j62 < 4; j62 = j62 + 1) {
				fRec32_tmp[j62] = fRec32_perm[j62];
			}
			/* Compute code */
			for (int i = 0; i < vsize; i = i + 1) {
				fRec32[i] = std::max<float>(fZec20[i], fSlow22 * fRec32[i - 1] + fSlow23 * fZec20[i]);
			}
			/* Post code */
			for (int j63 = 0; j63 < 4; j63 = j63 + 1) {
				fRec32_perm[j63] = fRec32_tmp[vsize + j63];
			}
			/* Recursive loop 59 */
			/* Pre code */
			for (int j68 = 0; j68 < 4; j68 = j68 + 1) {
				fRec35_tmp[j68] = fRec35_perm[j68];
			}
			/* Compute code */
			for (int i = 0; i < vsize; i = i + 1) {
				fRec35[i] = std::max<float>(fZec24[i], fSlow35 * fRec35[i - 1] + fSlow36 * fZec24[i]);
			}
			/* Post code */
			for (int j69 = 0; j69 < 4; j69 = j69 + 1) {
				fRec35_perm[j69] = fRec35_tmp[vsize + j69];
			}
			/* Recursive loop 60 */
			/* Pre code */
			for (int j74 = 0; j74 < 4; j74 = j74 + 1) {
				fRec38_tmp[j74] = fRec38_perm[j74];
			}
			/* Compute code */
			for (int i = 0; i < vsize; i = i + 1) {
				fRec38[i] = std::max<float>(fZec28[i], fSlow50 * fRec38[i - 1] + fSlow51 * fZec28[i]);
			}
			/* Post code */
			for (int j75 = 0; j75 < 4; j75 = j75 + 1) {
				fRec38_perm[j75] = fRec38_tmp[vsize + j75];
			}
			/* Recursive loop 61 */
			/* Pre code */
			for (int j64 = 0; j64 < 4; j64 = j64 + 1) {
				fRec31_tmp[j64] = fRec31_perm[j64];
			}
			/* Compute code */
			for (int i = 0; i < vsize; i = i + 1) {
				fRec31[i] = fSlow25 * fRec31[i - 1] + fRec32[i] * fSlow26;
			}
			/* Post code */
			for (int j65 = 0; j65 < 4; j65 = j65 + 1) {
				fRec31_perm[j65] = fRec31_tmp[vsize + j65];
			}
			/* Recursive loop 62 */
			/* Pre code */
			for (int j70 = 0; j70 < 4; j70 = j70 + 1) {
				fRec34_tmp[j70] = fRec34_perm[j70];
			}
			/* Compute code */
			for (int i = 0; i < vsize; i = i + 1) {
				fRec34[i] = fSlow38 * fRec34[i - 1] + fRec35[i] * fSlow39;
			}
			/* Post code */
			for (int j71 = 0; j71 < 4; j71 = j71 + 1) {
				fRec34_perm[j71] = fRec34_tmp[vsize + j71];
			}
			/* Recursive loop 63 */
			/* Pre code */
			for (int j76 = 0; j76 < 4; j76 = j76 + 1) {
				fRec37_tmp[j76] = fRec37_perm[j76];
			}
			/* Compute code */
			for (int i = 0; i < vsize; i = i + 1) {
				fRec37[i] = fSlow53 * fRec37[i - 1] + fRec38[i] * fSlow54;
			}
			/* Post code */
			for (int j77 = 0; j77 < 4; j77 = j77 + 1) {
				fRec37_perm[j77] = fRec37_tmp[vsize + j77];
			}
			/* Recursive loop 64 */
			/* Pre code */
			for (int j66 = 0; j66 < 4; j66 = j66 + 1) {
				fRec30_tmp[j66] = fRec30_perm[j66];
			}
			/* Compute code */
			for (int i = 0; i < vsize; i = i + 1) {
				fRec30[i] = fSlow27 * fRec30[i - 1] + fSlow28 * fSlow30 * std::max<float>(8.685889f * (8.262958e-08f * float(pun_float_to_int(fRec31[i])) + -87.98997f) - fSlow29, 0.0f);
			}
			/* Post code */
			for (int j67 = 0; j67 < 4; j67 = j67 + 1) {
				fRec30_perm[j67] = fRec30_tmp[vsize + j67];
			}
			/* Recursive loop 65 */
			/* Pre code */
			for (int j72 = 0; j72 < 4; j72 = j72 + 1) {
				fRec33_tmp[j72] = fRec33_perm[j72];
			}
			/* Compute code */
			for (int i = 0; i < vsize; i = i + 1) {
				fRec33[i] = fSlow40 * fRec33[i - 1] + fSlow41 * fSlow43 * std::max<float>(8.685889f * (8.262958e-08f * float(pun_float_to_int(fRec34[i])) + -87.98997f) - fSlow42, 0.0f);
			}
			/* Post code */
			for (int j73 = 0; j73 < 4; j73 = j73 + 1) {
				fRec33_perm[j73] = fRec33_tmp[vsize + j73];
			}
			/* Recursive loop 66 */
			/* Pre code */
			for (int j78 = 0; j78 < 4; j78 = j78 + 1) {
				fRec36_tmp[j78] = fRec36_perm[j78];
			}
			/* Compute code */
			for (int i = 0; i < vsize; i = i + 1) {
				fRec36[i] = fSlow55 * fRec36[i - 1] + fSlow56 * fSlow58 * std::max<float>(8.685889f * (8.262958e-08f * float(pun_float_to_int(fRec37[i])) + -87.98997f) - fSlow57, 0.0f);
			}
			/* Post code */
			for (int j79 = 0; j79 < 4; j79 = j79 + 1) {
				fRec36_perm[j79] = fRec36_tmp[vsize + j79];
			}
			/* Vectorizable loop 67 */
			/* Compute code */
			for (int i = 0; i < vsize; i = i + 1) {
				fZec29[i] = fSlow67 * fZec18[i];
			}
			/* Vectorizable loop 68 */
			/* Compute code */
			for (int i = 0; i < vsize; i = i + 1) {
				fZec30[i] = fSlow70 * fZec22[i];
			}
			/* Vectorizable loop 69 */
			/* Compute code */
			for (int i = 0; i < vsize; i = i + 1) {
				fZec31[i] = fSlow73 * fZec26[i];
			}
			/* Vectorizable loop 70 */
			/* Compute code */
			for (int i = 0; i < vsize; i = i + 1) {
				fHbargraph0 = FAUSTFLOAT(0.5f * pun_int_to_float(int(8388608.0f * (std::max<float>(-126.0f, 0.1660964f * fRec30[i]) + 126.942696f))));
				fZec34[i] = fSlow74 * fZec19[i];
			}
			/* Vectorizable loop 71 */
			/* Compute code */
			for (int i = 0; i < vsize; i = i + 1) {
				fHbargraph2 = FAUSTFLOAT(0.5f * pun_int_to_float(int(8388608.0f * (std::max<float>(-126.0f, 0.1660964f * fRec33[i]) + 126.942696f))));
				fZec35[i] = fSlow75 * fZec23[i];
			}
			/* Vectorizable loop 72 */
			/* Compute code */
			for (int i = 0; i < vsize; i = i + 1) {
				fHbargraph4 = FAUSTFLOAT(0.5f * pun_int_to_float(int(8388608.0f * (std::max<float>(-126.0f, 0.1660964f * fRec36[i]) + 126.942696f))));
				fZec36[i] = fSlow76 * fZec27[i];
			}
			/* Vectorizable loop 73 */
			/* Compute code */
			for (int i = 0; i < vsize; i = i + 1) {
				fZec32[i] = ((iSlow20) ? fZec0[i] : fZec29[i]) + ((iSlow32) ? fZec5[i] : fZec30[i]) + ((iSlow48) ? fZec12[i] : fZec31[i]);
			}
			/* Vectorizable loop 74 */
			/* Compute code */
			for (int i = 0; i < vsize; i = i + 1) {
				fHbargraph1 = FAUSTFLOAT(std::fabs(fZec29[i]) + std::fabs(fZec34[i]));
				fHbargraph3 = FAUSTFLOAT(std::fabs(fZec30[i]) + std::fabs(fZec35[i]));
				fHbargraph5 = FAUSTFLOAT(std::fabs(fZec31[i]) + std::fabs(fZec36[i]));
				fZec37[i] = ((iSlow20) ? fZec2[i] : fZec34[i]) + ((iSlow32) ? fZec7[i] : fZec35[i]) + ((iSlow48) ? fZec14[i] : fZec36[i]);
			}
			/* Vectorizable loop 75 */
			/* Compute code */
			for (int i = 0; i < vsize; i = i + 1) {
				fZec33[i] = ((iSlow60) ? 0.0f : fZec32[i]);
			}
			/* Vectorizable loop 76 */
			/* Compute code */
			for (int i = 0; i < vsize; i = i + 1) {
				fZec38[i] = ((iSlow60) ? 0.0f : fZec37[i]);
			}
			/* Vectorizable loop 77 */
			/* Compute code */
			for (int i = 0; i < vsize; i = i + 1) {
				fZec39[i] = std::fabs(std::fabs(fSlow59 * fZec33[i]) + std::fabs(fSlow59 * fZec38[i]));
			}
			/* Recursive loop 78 */
			/* Pre code */
			for (int j80 = 0; j80 < 4; j80 = j80 + 1) {
				fRec29_tmp[j80] = fRec29_perm[j80];
			}
			/* Compute code */
			for (int i = 0; i < vsize; i = i + 1) {
				fRec29[i] = std::max<float>(fZec39[i], fRec29[i - 1] * fSlow78 + fZec39[i] * fSlow79);
			}
			/* Post code */
			for (int j81 = 0; j81 < 4; j81 = j81 + 1) {
				fRec29_perm[j81] = fRec29_tmp[vsize + j81];
			}
			/* Recursive loop 79 */
			/* Pre code */
			for (int j82 = 0; j82 < 4; j82 = j82 + 1) {
				fRec28_tmp[j82] = fRec28_perm[j82];
			}
			/* Compute code */
			for (int i = 0; i < vsize; i = i + 1) {
				fRec28[i] = fRec28[i - 1] * fSlow81 + fRec29[i] * fSlow82;
			}
			/* Post code */
			for (int j83 = 0; j83 < 4; j83 = j83 + 1) {
				fRec28_perm[j83] = fRec28_tmp[vsize + j83];
			}
			/* Recursive loop 80 */
			/* Pre code */
			for (int j84 = 0; j84 < 4; j84 = j84 + 1) {
				fRec27_tmp[j84] = fRec27_perm[j84];
			}
			/* Compute code */
			for (int i = 0; i < vsize; i = i + 1) {
				fRec27[i] = fRec27[i - 1] * fSlow83 + fSlow84 * std::max<float>(8.685889f * (8.262958e-08f * float(pun_float_to_int(fRec28[i])) + -87.98997f) + 6.0f, 0.0f) * fSlow85;
			}
			/* Post code */
			for (int j85 = 0; j85 < 4; j85 = j85 + 1) {
				fRec27_perm[j85] = fRec27_tmp[vsize + j85];
			}
			/* Vectorizable loop 81 */
			/* Compute code */
			for (int i = 0; i < vsize; i = i + 1) {
				fZec40[i] = pun_int_to_float(int(8388608.0f * (std::max<float>(-126.0f, 0.1660964f * fRec27[i]) + 126.942696f)));
			}
			/* Vectorizable loop 82 */
			/* Compute code */
			for (int i = 0; i < vsize; i = i + 1) {
				output0[i] = FAUSTFLOAT(fSlow86 * ((iSlow60) ? fZec32[i] : fSlow87 * fZec33[i] * fZec40[i]));
			}
			/* Vectorizable loop 83 */
			/* Compute code */
			for (int i = 0; i < vsize; i = i + 1) {
				output1[i] = FAUSTFLOAT(fSlow86 * ((iSlow60) ? fZec37[i] : fSlow87 * fZec38[i] * fZec40[i]));
			}
		}
		/* Remaining frames */
		if (vindex < count) {
			FAUSTFLOAT* input0 = &input0_ptr[vindex];
			FAUSTFLOAT* input1 = &input1_ptr[vindex];
			FAUSTFLOAT* output0 = &output0_ptr[vindex];
			FAUSTFLOAT* output1 = &output1_ptr[vindex];
			int vsize = count - vindex;
			/* Vectorizable loop 0 */
			/* Pre code */
			for (int j0 = 0; j0 < 4; j0 = j0 + 1) {
				fYec0_tmp[j0] = fYec0_perm[j0];
			}
			/* Compute code */
			for (int i = 0; i < vsize; i = i + 1) {
				fYec0[i] = float(input0[i]);
			}
			/* Post code */
			for (int j1 = 0; j1 < 4; j1 = j1 + 1) {
				fYec0_perm[j1] = fYec0_tmp[vsize + j1];
			}
			/* Vectorizable loop 1 */
			/* Pre code */
			for (int j12 = 0; j12 < 4; j12 = j12 + 1) {
				fYec2_tmp[j12] = fYec2_perm[j12];
			}
			/* Compute code */
			for (int i = 0; i < vsize; i = i + 1) {
				fYec2[i] = float(input1[i]);
			}
			/* Post code */
			for (int j13 = 0; j13 < 4; j13 = j13 + 1) {
				fYec2_perm[j13] = fYec2_tmp[vsize + j13];
			}
			/* Recursive loop 2 */
			/* Pre code */
			for (int j2 = 0; j2 < 4; j2 = j2 + 1) {
				fRec3_tmp[j2] = fRec3_perm[j2];
			}
			/* Compute code */
			for (int i = 0; i < vsize; i = i + 1) {
				fRec3[i] = -(fSlow2 * (fSlow3 * fRec3[i - 1] - (float(input0[i]) + fYec0[i - 1])));
			}
			/* Post code */
			for (int j3 = 0; j3 < 4; j3 = j3 + 1) {
				fRec3_perm[j3] = fRec3_tmp[vsize + j3];
			}
			/* Recursive loop 3 */
			/* Pre code */
			for (int j14 = 0; j14 < 4; j14 = j14 + 1) {
				fRec10_tmp[j14] = fRec10_perm[j14];
			}
			/* Compute code */
			for (int i = 0; i < vsize; i = i + 1) {
				fRec10[i] = -(fSlow2 * (fSlow3 * fRec10[i - 1] - (float(input1[i]) + fYec2[i - 1])));
			}
			/* Post code */
			for (int j15 = 0; j15 < 4; j15 = j15 + 1) {
				fRec10_perm[j15] = fRec10_tmp[vsize + j15];
			}
			/* Recursive loop 4 */
			/* Pre code */
			for (int j4 = 0; j4 < 4; j4 = j4 + 1) {
				fRec2_tmp[j4] = fRec2_perm[j4];
			}
			/* Compute code */
			for (int i = 0; i < vsize; i = i + 1) {
				fRec2[i] = fRec3[i] - fSlow5 * (fSlow6 * fRec2[i - 2] + fSlow8 * fRec2[i - 1]);
			}
			/* Post code */
			for (int j5 = 0; j5 < 4; j5 = j5 + 1) {
				fRec2_perm[j5] = fRec2_tmp[vsize + j5];
			}
			/* Recursive loop 5 */
			/* Pre code */
			for (int j16 = 0; j16 < 4; j16 = j16 + 1) {
				fRec9_tmp[j16] = fRec9_perm[j16];
			}
			/* Compute code */
			for (int i = 0; i < vsize; i = i + 1) {
				fRec9[i] = fRec10[i] - fSlow5 * (fSlow6 * fRec9[i - 2] + fSlow8 * fRec9[i - 1]);
			}
			/* Post code */
			for (int j17 = 0; j17 < 4; j17 = j17 + 1) {
				fRec9_perm[j17] = fRec9_tmp[vsize + j17];
			}
			/* Vectorizable loop 6 */
			/* Pre code */
			for (int j6 = 0; j6 < 4; j6 = j6 + 1) {
				fYec1_tmp[j6] = fYec1_perm[j6];
			}
			/* Compute code */
			for (int i = 0; i < vsize; i = i + 1) {
				fYec1[i] = fSlow5 * (fRec2[i - 2] + fRec2[i] + 2.0f * fRec2[i - 1]);
			}
			/* Post code */
			for (int j7 = 0; j7 < 4; j7 = j7 + 1) {
				fYec1_perm[j7] = fYec1_tmp[vsize + j7];
			}
			/* Vectorizable loop 7 */
			/* Pre code */
			for (int j18 = 0; j18 < 4; j18 = j18 + 1) {
				fYec3_tmp[j18] = fYec3_perm[j18];
			}
			/* Compute code */
			for (int i = 0; i < vsize; i = i + 1) {
				fYec3[i] = fSlow5 * (fRec9[i - 2] + fRec9[i] + 2.0f * fRec9[i - 1]);
			}
			/* Post code */
			for (int j19 = 0; j19 < 4; j19 = j19 + 1) {
				fYec3_perm[j19] = fYec3_tmp[vsize + j19];
			}
			/* Recursive loop 8 */
			/* Pre code */
			for (int j44 = 0; j44 < 4; j44 = j44 + 1) {
				fRec20_tmp[j44] = fRec20_perm[j44];
			}
			/* Compute code */
			for (int i = 0; i < vsize; i = i + 1) {
				fRec20[i] = -(fSlow2 * (fSlow3 * fRec20[i - 1] - fSlow1 * (float(input0[i]) - fYec0[i - 1])));
			}
			/* Post code */
			for (int j45 = 0; j45 < 4; j45 = j45 + 1) {
				fRec20_perm[j45] = fRec20_tmp[vsize + j45];
			}
			/* Recursive loop 9 */
			/* Pre code */
			for (int j50 = 0; j50 < 4; j50 = j50 + 1) {
				fRec26_tmp[j50] = fRec26_perm[j50];
			}
			/* Compute code */
			for (int i = 0; i < vsize; i = i + 1) {
				fRec26[i] = -(fSlow2 * (fSlow3 * fRec26[i - 1] - fSlow1 * (float(input1[i]) - fYec2[i - 1])));
			}
			/* Post code */
			for (int j51 = 0; j51 < 4; j51 = j51 + 1) {
				fRec26_perm[j51] = fRec26_tmp[vsize + j51];
			}
			/* Recursive loop 10 */
			/* Pre code */
			for (int j8 = 0; j8 < 4; j8 = j8 + 1) {
				fRec1_tmp[j8] = fRec1_perm[j8];
			}
			/* Compute code */
			for (int i = 0; i < vsize; i = i + 1) {
				fRec1[i] = -(fSlow12 * (fSlow13 * fRec1[i - 1] - (fYec1[i] + fYec1[i - 1])));
			}
			/* Post code */
			for (int j9 = 0; j9 < 4; j9 = j9 + 1) {
				fRec1_perm[j9] = fRec1_tmp[vsize + j9];
			}
			/* Recursive loop 11 */
			/* Pre code */
			for (int j20 = 0; j20 < 4; j20 = j20 + 1) {
				fRec8_tmp[j20] = fRec8_perm[j20];
			}
			/* Compute code */
			for (int i = 0; i < vsize; i = i + 1) {
				fRec8[i] = -(fSlow12 * (fSlow13 * fRec8[i - 1] - (fYec3[i] + fYec3[i - 1])));
			}
			/* Post code */
			for (int j21 = 0; j21 < 4; j21 = j21 + 1) {
				fRec8_perm[j21] = fRec8_tmp[vsize + j21];
			}
			/* Recursive loop 12 */
			/* Pre code */
			for (int j30 = 0; j30 < 4; j30 = j30 + 1) {
				fRec12_tmp[j30] = fRec12_perm[j30];
			}
			/* Compute code */
			for (int i = 0; i < vsize; i = i + 1) {
				fRec12[i] = -(fSlow12 * (fSlow13 * fRec12[i - 1] - fSlow10 * (fYec1[i] - fYec1[i - 1])));
			}
			/* Post code */
			for (int j31 = 0; j31 < 4; j31 = j31 + 1) {
				fRec12_perm[j31] = fRec12_tmp[vsize + j31];
			}
			/* Recursive loop 13 */
			/* Pre code */
			for (int j34 = 0; j34 < 4; j34 = j34 + 1) {
				fRec17_tmp[j34] = fRec17_perm[j34];
			}
			/* Compute code */
			for (int i = 0; i < vsize; i = i + 1) {
				fRec17[i] = -(fSlow12 * (fSlow13 * fRec17[i - 1] - fSlow10 * (fYec3[i] - fYec3[i - 1])));
			}
			/* Post code */
			for (int j35 = 0; j35 < 4; j35 = j35 + 1) {
				fRec17_perm[j35] = fRec17_tmp[vsize + j35];
			}
			/* Recursive loop 14 */
			/* Pre code */
			for (int j46 = 0; j46 < 4; j46 = j46 + 1) {
				fRec19_tmp[j46] = fRec19_perm[j46];
			}
			/* Compute code */
			for (int i = 0; i < vsize; i = i + 1) {
				fRec19[i] = fRec20[i] - fSlow5 * (fSlow6 * fRec19[i - 2] + fSlow8 * fRec19[i - 1]);
			}
			/* Post code */
			for (int j47 = 0; j47 < 4; j47 = j47 + 1) {
				fRec19_perm[j47] = fRec19_tmp[vsize + j47];
			}
			/* Recursive loop 15 */
			/* Pre code */
			for (int j52 = 0; j52 < 4; j52 = j52 + 1) {
				fRec25_tmp[j52] = fRec25_perm[j52];
			}
			/* Compute code */
			for (int i = 0; i < vsize; i = i + 1) {
				fRec25[i] = fRec26[i] - fSlow5 * (fSlow6 * fRec25[i - 2] + fSlow8 * fRec25[i - 1]);
			}
			/* Post code */
			for (int j53 = 0; j53 < 4; j53 = j53 + 1) {
				fRec25_perm[j53] = fRec25_tmp[vsize + j53];
			}
			/* Recursive loop 16 */
			/* Pre code */
			for (int j10 = 0; j10 < 4; j10 = j10 + 1) {
				fRec0_tmp[j10] = fRec0_perm[j10];
			}
			/* Compute code */
			for (int i = 0; i < vsize; i = i + 1) {
				fRec0[i] = fRec1[i] - fSlow15 * (fSlow16 * fRec0[i - 2] + fSlow18 * fRec0[i - 1]);
			}
			/* Post code */
			for (int j11 = 0; j11 < 4; j11 = j11 + 1) {
				fRec0_perm[j11] = fRec0_tmp[vsize + j11];
			}
			/* Recursive loop 17 */
			/* Pre code */
			for (int j22 = 0; j22 < 4; j22 = j22 + 1) {
				fRec7_tmp[j22] = fRec7_perm[j22];
			}
			/* Compute code */
			for (int i = 0; i < vsize; i = i + 1) {
				fRec7[i] = fRec8[i] - fSlow15 * (fSlow16 * fRec7[i - 2] + fSlow18 * fRec7[i - 1]);
			}
			/* Post code */
			for (int j23 = 0; j23 < 4; j23 = j23 + 1) {
				fRec7_perm[j23] = fRec7_tmp[vsize + j23];
			}
			/* Recursive loop 18 */
			/* Pre code */
			for (int j32 = 0; j32 < 4; j32 = j32 + 1) {
				fRec11_tmp[j32] = fRec11_perm[j32];
			}
			/* Compute code */
			for (int i = 0; i < vsize; i = i + 1) {
				fRec11[i] = fRec12[i] - fSlow15 * (fSlow16 * fRec11[i - 2] + fSlow18 * fRec11[i - 1]);
			}
			/* Post code */
			for (int j33 = 0; j33 < 4; j33 = j33 + 1) {
				fRec11_perm[j33] = fRec11_tmp[vsize + j33];
			}
			/* Recursive loop 19 */
			/* Pre code */
			for (int j36 = 0; j36 < 4; j36 = j36 + 1) {
				fRec16_tmp[j36] = fRec16_perm[j36];
			}
			/* Compute code */
			for (int i = 0; i < vsize; i = i + 1) {
				fRec16[i] = fRec17[i] - fSlow15 * (fSlow16 * fRec16[i - 2] + fSlow18 * fRec16[i - 1]);
			}
			/* Post code */
			for (int j37 = 0; j37 < 4; j37 = j37 + 1) {
				fRec16_perm[j37] = fRec16_tmp[vsize + j37];
			}
			/* Recursive loop 20 */
			/* Pre code */
			for (int j48 = 0; j48 < 4; j48 = j48 + 1) {
				fRec18_tmp[j48] = fRec18_perm[j48];
			}
			/* Compute code */
			for (int i = 0; i < vsize; i = i + 1) {
				fZec10[i] = fSlow18 * fRec18[i - 1];
				fRec18[i] = fSlow44 * (fRec19[i - 2] + (fRec19[i] - 2.0f * fRec19[i - 1])) - fSlow45 * (fSlow46 * fRec18[i - 2] + fZec10[i]);
			}
			/* Post code */
			for (int j49 = 0; j49 < 4; j49 = j49 + 1) {
				fRec18_perm[j49] = fRec18_tmp[vsize + j49];
			}
			/* Recursive loop 21 */
			/* Pre code */
			for (int j54 = 0; j54 < 4; j54 = j54 + 1) {
				fRec24_tmp[j54] = fRec24_perm[j54];
			}
			/* Compute code */
			for (int i = 0; i < vsize; i = i + 1) {
				fZec11[i] = fSlow18 * fRec24[i - 1];
				fRec24[i] = fSlow44 * (fRec25[i - 2] + (fRec25[i] - 2.0f * fRec25[i - 1])) - fSlow45 * (fSlow46 * fRec24[i - 2] + fZec11[i]);
			}
			/* Post code */
			for (int j55 = 0; j55 < 4; j55 = j55 + 1) {
				fRec24_perm[j55] = fRec24_tmp[vsize + j55];
			}
			/* Vectorizable loop 22 */
			/* Compute code */
			for (int i = 0; i < vsize; i = i + 1) {
				fZec0[i] = fSlow15 * (fRec0[i - 2] + fRec0[i] + 2.0f * fRec0[i - 1]);
			}
			/* Vectorizable loop 23 */
			/* Compute code */
			for (int i = 0; i < vsize; i = i + 1) {
				fZec2[i] = fSlow15 * (fRec7[i - 2] + fRec7[i] + 2.0f * fRec7[i - 1]);
			}
			/* Vectorizable loop 24 */
			/* Compute code */
			for (int i = 0; i < vsize; i = i + 1) {
				fZec5[i] = fSlow33 * (fRec11[i - 2] + (fRec11[i] - 2.0f * fRec11[i - 1]));
			}
			/* Vectorizable loop 25 */
			/* Compute code */
			for (int i = 0; i < vsize; i = i + 1) {
				fZec7[i] = fSlow33 * (fRec16[i - 2] + (fRec16[i] - 2.0f * fRec16[i - 1]));
			}
			/* Vectorizable loop 26 */
			/* Compute code */
			for (int i = 0; i < vsize; i = i + 1) {
				fZec12[i] = fRec18[i - 2] + fSlow45 * (fZec10[i] + fSlow46 * fRec18[i]);
			}
			/* Vectorizable loop 27 */
			/* Compute code */
			for (int i = 0; i < vsize; i = i + 1) {
				fZec14[i] = fRec24[i - 2] + fSlow45 * (fZec11[i] + fSlow46 * fRec24[i]);
			}
			/* Vectorizable loop 28 */
			/* Compute code */
			for (int i = 0; i < vsize; i = i + 1) {
				fZec1[i] = ((iSlow20) ? 0.0f : fZec0[i]);
			}
			/* Vectorizable loop 29 */
			/* Compute code */
			for (int i = 0; i < vsize; i = i + 1) {
				fZec3[i] = ((iSlow20) ? 0.0f : fZec2[i]);
			}
			/* Vectorizable loop 30 */
			/* Compute code */
			for (int i = 0; i < vsize; i = i + 1) {
				fZec6[i] = ((iSlow32) ? 0.0f : fZec5[i]);
			}
			/* Vectorizable loop 31 */
			/* Compute code */
			for (int i = 0; i < vsize; i = i + 1) {
				fZec8[i] = ((iSlow32) ? 0.0f : fZec7[i]);
			}
			/* Vectorizable loop 32 */
			/* Compute code */
			for (int i = 0; i < vsize; i = i + 1) {
				fZec13[i] = ((iSlow48) ? 0.0f : fZec12[i]);
			}
			/* Vectorizable loop 33 */
			/* Compute code */
			for (int i = 0; i < vsize; i = i + 1) {
				fZec15[i] = ((iSlow48) ? 0.0f : fZec14[i]);
			}
			/* Vectorizable loop 34 */
			/* Compute code */
			for (int i = 0; i < vsize; i = i + 1) {
				fZec4[i] = std::fabs(std::fabs(fSlow19 * fZec1[i]) + std::fabs(fSlow19 * fZec3[i]));
			}
			/* Vectorizable loop 35 */
			/* Compute code */
			for (int i = 0; i < vsize; i = i + 1) {
				fZec9[i] = std::fabs(std::fabs(fSlow31 * fZec6[i]) + std::fabs(fSlow31 * fZec8[i]));
			}
			/* Vectorizable loop 36 */
			/* Compute code */
			for (int i = 0; i < vsize; i = i + 1) {
				fZec16[i] = std::fabs(std::fabs(fSlow47 * fZec13[i]) + std::fabs(fSlow47 * fZec15[i]));
			}
			/* Recursive loop 37 */
			/* Pre code */
			for (int j24 = 0; j24 < 4; j24 = j24 + 1) {
				fRec6_tmp[j24] = fRec6_perm[j24];
			}
			/* Compute code */
			for (int i = 0; i < vsize; i = i + 1) {
				fRec6[i] = std::max<float>(fZec4[i], fRec6[i - 1] * fSlow22 + fZec4[i] * fSlow23);
			}
			/* Post code */
			for (int j25 = 0; j25 < 4; j25 = j25 + 1) {
				fRec6_perm[j25] = fRec6_tmp[vsize + j25];
			}
			/* Recursive loop 38 */
			/* Pre code */
			for (int j38 = 0; j38 < 4; j38 = j38 + 1) {
				fRec15_tmp[j38] = fRec15_perm[j38];
			}
			/* Compute code */
			for (int i = 0; i < vsize; i = i + 1) {
				fRec15[i] = std::max<float>(fZec9[i], fRec15[i - 1] * fSlow35 + fZec9[i] * fSlow36);
			}
			/* Post code */
			for (int j39 = 0; j39 < 4; j39 = j39 + 1) {
				fRec15_perm[j39] = fRec15_tmp[vsize + j39];
			}
			/* Recursive loop 39 */
			/* Pre code */
			for (int j56 = 0; j56 < 4; j56 = j56 + 1) {
				fRec23_tmp[j56] = fRec23_perm[j56];
			}
			/* Compute code */
			for (int i = 0; i < vsize; i = i + 1) {
				fRec23[i] = std::max<float>(fZec16[i], fRec23[i - 1] * fSlow50 + fZec16[i] * fSlow51);
			}
			/* Post code */
			for (int j57 = 0; j57 < 4; j57 = j57 + 1) {
				fRec23_perm[j57] = fRec23_tmp[vsize + j57];
			}
			/* Recursive loop 40 */
			/* Pre code */
			for (int j26 = 0; j26 < 4; j26 = j26 + 1) {
				fRec5_tmp[j26] = fRec5_perm[j26];
			}
			/* Compute code */
			for (int i = 0; i < vsize; i = i + 1) {
				fRec5[i] = fRec5[i - 1] * fSlow25 + fRec6[i] * fSlow26;
			}
			/* Post code */
			for (int j27 = 0; j27 < 4; j27 = j27 + 1) {
				fRec5_perm[j27] = fRec5_tmp[vsize + j27];
			}
			/* Recursive loop 41 */
			/* Pre code */
			for (int j40 = 0; j40 < 4; j40 = j40 + 1) {
				fRec14_tmp[j40] = fRec14_perm[j40];
			}
			/* Compute code */
			for (int i = 0; i < vsize; i = i + 1) {
				fRec14[i] = fRec14[i - 1] * fSlow38 + fRec15[i] * fSlow39;
			}
			/* Post code */
			for (int j41 = 0; j41 < 4; j41 = j41 + 1) {
				fRec14_perm[j41] = fRec14_tmp[vsize + j41];
			}
			/* Recursive loop 42 */
			/* Pre code */
			for (int j58 = 0; j58 < 4; j58 = j58 + 1) {
				fRec22_tmp[j58] = fRec22_perm[j58];
			}
			/* Compute code */
			for (int i = 0; i < vsize; i = i + 1) {
				fRec22[i] = fRec22[i - 1] * fSlow53 + fRec23[i] * fSlow54;
			}
			/* Post code */
			for (int j59 = 0; j59 < 4; j59 = j59 + 1) {
				fRec22_perm[j59] = fRec22_tmp[vsize + j59];
			}
			/* Recursive loop 43 */
			/* Pre code */
			for (int j28 = 0; j28 < 4; j28 = j28 + 1) {
				fRec4_tmp[j28] = fRec4_perm[j28];
			}
			/* Compute code */
			for (int i = 0; i < vsize; i = i + 1) {
				fRec4[i] = fRec4[i - 1] * fSlow27 + fSlow28 * std::max<float>(8.685889f * (8.262958e-08f * float(pun_float_to_int(fRec5[i])) + -87.98997f) - fSlow29, 0.0f) * fSlow30;
			}
			/* Post code */
			for (int j29 = 0; j29 < 4; j29 = j29 + 1) {
				fRec4_perm[j29] = fRec4_tmp[vsize + j29];
			}
			/* Recursive loop 44 */
			/* Pre code */
			for (int j42 = 0; j42 < 4; j42 = j42 + 1) {
				fRec13_tmp[j42] = fRec13_perm[j42];
			}
			/* Compute code */
			for (int i = 0; i < vsize; i = i + 1) {
				fRec13[i] = fRec13[i - 1] * fSlow40 + fSlow41 * std::max<float>(8.685889f * (8.262958e-08f * float(pun_float_to_int(fRec14[i])) + -87.98997f) - fSlow42, 0.0f) * fSlow43;
			}
			/* Post code */
			for (int j43 = 0; j43 < 4; j43 = j43 + 1) {
				fRec13_perm[j43] = fRec13_tmp[vsize + j43];
			}
			/* Recursive loop 45 */
			/* Pre code */
			for (int j60 = 0; j60 < 4; j60 = j60 + 1) {
				fRec21_tmp[j60] = fRec21_perm[j60];
			}
			/* Compute code */
			for (int i = 0; i < vsize; i = i + 1) {
				fRec21[i] = fRec21[i - 1] * fSlow55 + fSlow56 * std::max<float>(8.685889f * (8.262958e-08f * float(pun_float_to_int(fRec22[i])) + -87.98997f) - fSlow57, 0.0f) * fSlow58;
			}
			/* Post code */
			for (int j61 = 0; j61 < 4; j61 = j61 + 1) {
				fRec21_perm[j61] = fRec21_tmp[vsize + j61];
			}
			/* Vectorizable loop 46 */
			/* Compute code */
			for (int i = 0; i < vsize; i = i + 1) {
				fZec17[i] = pun_int_to_float(int(8388608.0f * (std::max<float>(-126.0f, 0.1660964f * fRec4[i]) + 126.942696f)));
			}
			/* Vectorizable loop 47 */
			/* Compute code */
			for (int i = 0; i < vsize; i = i + 1) {
				fZec21[i] = pun_int_to_float(int(8388608.0f * (std::max<float>(-126.0f, 0.1660964f * fRec13[i]) + 126.942696f)));
			}
			/* Vectorizable loop 48 */
			/* Compute code */
			for (int i = 0; i < vsize; i = i + 1) {
				fZec25[i] = pun_int_to_float(int(8388608.0f * (std::max<float>(-126.0f, 0.1660964f * fRec21[i]) + 126.942696f)));
			}
			/* Vectorizable loop 49 */
			/* Compute code */
			for (int i = 0; i < vsize; i = i + 1) {
				fZec18[i] = fZec1[i] * fZec17[i];
			}
			/* Vectorizable loop 50 */
			/* Compute code */
			for (int i = 0; i < vsize; i = i + 1) {
				fZec19[i] = fSlow19 * fZec3[i] * fZec17[i];
			}
			/* Vectorizable loop 51 */
			/* Compute code */
			for (int i = 0; i < vsize; i = i + 1) {
				fZec22[i] = fZec6[i] * fZec21[i];
			}
			/* Vectorizable loop 52 */
			/* Compute code */
			for (int i = 0; i < vsize; i = i + 1) {
				fZec23[i] = fSlow31 * fZec8[i] * fZec21[i];
			}
			/* Vectorizable loop 53 */
			/* Compute code */
			for (int i = 0; i < vsize; i = i + 1) {
				fZec26[i] = fZec13[i] * fZec25[i];
			}
			/* Vectorizable loop 54 */
			/* Compute code */
			for (int i = 0; i < vsize; i = i + 1) {
				fZec27[i] = fSlow47 * fZec15[i] * fZec25[i];
			}
			/* Vectorizable loop 55 */
			/* Compute code */
			for (int i = 0; i < vsize; i = i + 1) {
				fZec20[i] = std::fabs(std::fabs(fSlow19 * fZec18[i]) + std::fabs(fZec19[i]));
			}
			/* Vectorizable loop 56 */
			/* Compute code */
			for (int i = 0; i < vsize; i = i + 1) {
				fZec24[i] = std::fabs(std::fabs(fSlow31 * fZec22[i]) + std::fabs(fZec23[i]));
			}
			/* Vectorizable loop 57 */
			/* Compute code */
			for (int i = 0; i < vsize; i = i + 1) {
				fZec28[i] = std::fabs(std::fabs(fSlow47 * fZec26[i]) + std::fabs(fZec27[i]));
			}
			/* Recursive loop 58 */
			/* Pre code */
			for (int j62 = 0; j62 < 4; j62 = j62 + 1) {
				fRec32_tmp[j62] = fRec32_perm[j62];
			}
			/* Compute code */
			for (int i = 0; i < vsize; i = i + 1) {
				fRec32[i] = std::max<float>(fZec20[i], fSlow22 * fRec32[i - 1] + fSlow23 * fZec20[i]);
			}
			/* Post code */
			for (int j63 = 0; j63 < 4; j63 = j63 + 1) {
				fRec32_perm[j63] = fRec32_tmp[vsize + j63];
			}
			/* Recursive loop 59 */
			/* Pre code */
			for (int j68 = 0; j68 < 4; j68 = j68 + 1) {
				fRec35_tmp[j68] = fRec35_perm[j68];
			}
			/* Compute code */
			for (int i = 0; i < vsize; i = i + 1) {
				fRec35[i] = std::max<float>(fZec24[i], fSlow35 * fRec35[i - 1] + fSlow36 * fZec24[i]);
			}
			/* Post code */
			for (int j69 = 0; j69 < 4; j69 = j69 + 1) {
				fRec35_perm[j69] = fRec35_tmp[vsize + j69];
			}
			/* Recursive loop 60 */
			/* Pre code */
			for (int j74 = 0; j74 < 4; j74 = j74 + 1) {
				fRec38_tmp[j74] = fRec38_perm[j74];
			}
			/* Compute code */
			for (int i = 0; i < vsize; i = i + 1) {
				fRec38[i] = std::max<float>(fZec28[i], fSlow50 * fRec38[i - 1] + fSlow51 * fZec28[i]);
			}
			/* Post code */
			for (int j75 = 0; j75 < 4; j75 = j75 + 1) {
				fRec38_perm[j75] = fRec38_tmp[vsize + j75];
			}
			/* Recursive loop 61 */
			/* Pre code */
			for (int j64 = 0; j64 < 4; j64 = j64 + 1) {
				fRec31_tmp[j64] = fRec31_perm[j64];
			}
			/* Compute code */
			for (int i = 0; i < vsize; i = i + 1) {
				fRec31[i] = fSlow25 * fRec31[i - 1] + fRec32[i] * fSlow26;
			}
			/* Post code */
			for (int j65 = 0; j65 < 4; j65 = j65 + 1) {
				fRec31_perm[j65] = fRec31_tmp[vsize + j65];
			}
			/* Recursive loop 62 */
			/* Pre code */
			for (int j70 = 0; j70 < 4; j70 = j70 + 1) {
				fRec34_tmp[j70] = fRec34_perm[j70];
			}
			/* Compute code */
			for (int i = 0; i < vsize; i = i + 1) {
				fRec34[i] = fSlow38 * fRec34[i - 1] + fRec35[i] * fSlow39;
			}
			/* Post code */
			for (int j71 = 0; j71 < 4; j71 = j71 + 1) {
				fRec34_perm[j71] = fRec34_tmp[vsize + j71];
			}
			/* Recursive loop 63 */
			/* Pre code */
			for (int j76 = 0; j76 < 4; j76 = j76 + 1) {
				fRec37_tmp[j76] = fRec37_perm[j76];
			}
			/* Compute code */
			for (int i = 0; i < vsize; i = i + 1) {
				fRec37[i] = fSlow53 * fRec37[i - 1] + fRec38[i] * fSlow54;
			}
			/* Post code */
			for (int j77 = 0; j77 < 4; j77 = j77 + 1) {
				fRec37_perm[j77] = fRec37_tmp[vsize + j77];
			}
			/* Recursive loop 64 */
			/* Pre code */
			for (int j66 = 0; j66 < 4; j66 = j66 + 1) {
				fRec30_tmp[j66] = fRec30_perm[j66];
			}
			/* Compute code */
			for (int i = 0; i < vsize; i = i + 1) {
				fRec30[i] = fSlow27 * fRec30[i - 1] + fSlow28 * fSlow30 * std::max<float>(8.685889f * (8.262958e-08f * float(pun_float_to_int(fRec31[i])) + -87.98997f) - fSlow29, 0.0f);
			}
			/* Post code */
			for (int j67 = 0; j67 < 4; j67 = j67 + 1) {
				fRec30_perm[j67] = fRec30_tmp[vsize + j67];
			}
			/* Recursive loop 65 */
			/* Pre code */
			for (int j72 = 0; j72 < 4; j72 = j72 + 1) {
				fRec33_tmp[j72] = fRec33_perm[j72];
			}
			/* Compute code */
			for (int i = 0; i < vsize; i = i + 1) {
				fRec33[i] = fSlow40 * fRec33[i - 1] + fSlow41 * fSlow43 * std::max<float>(8.685889f * (8.262958e-08f * float(pun_float_to_int(fRec34[i])) + -87.98997f) - fSlow42, 0.0f);
			}
			/* Post code */
			for (int j73 = 0; j73 < 4; j73 = j73 + 1) {
				fRec33_perm[j73] = fRec33_tmp[vsize + j73];
			}
			/* Recursive loop 66 */
			/* Pre code */
			for (int j78 = 0; j78 < 4; j78 = j78 + 1) {
				fRec36_tmp[j78] = fRec36_perm[j78];
			}
			/* Compute code */
			for (int i = 0; i < vsize; i = i + 1) {
				fRec36[i] = fSlow55 * fRec36[i - 1] + fSlow56 * fSlow58 * std::max<float>(8.685889f * (8.262958e-08f * float(pun_float_to_int(fRec37[i])) + -87.98997f) - fSlow57, 0.0f);
			}
			/* Post code */
			for (int j79 = 0; j79 < 4; j79 = j79 + 1) {
				fRec36_perm[j79] = fRec36_tmp[vsize + j79];
			}
			/* Vectorizable loop 67 */
			/* Compute code */
			for (int i = 0; i < vsize; i = i + 1) {
				fZec29[i] = fSlow67 * fZec18[i];
			}
			/* Vectorizable loop 68 */
			/* Compute code */
			for (int i = 0; i < vsize; i = i + 1) {
				fZec30[i] = fSlow70 * fZec22[i];
			}
			/* Vectorizable loop 69 */
			/* Compute code */
			for (int i = 0; i < vsize; i = i + 1) {
				fZec31[i] = fSlow73 * fZec26[i];
			}
			/* Vectorizable loop 70 */
			/* Compute code */
			for (int i = 0; i < vsize; i = i + 1) {
				fHbargraph0 = FAUSTFLOAT(0.5f * pun_int_to_float(int(8388608.0f * (std::max<float>(-126.0f, 0.1660964f * fRec30[i]) + 126.942696f))));
				fZec34[i] = fSlow74 * fZec19[i];
			}
			/* Vectorizable loop 71 */
			/* Compute code */
			for (int i = 0; i < vsize; i = i + 1) {
				fHbargraph2 = FAUSTFLOAT(0.5f * pun_int_to_float(int(8388608.0f * (std::max<float>(-126.0f, 0.1660964f * fRec33[i]) + 126.942696f))));
				fZec35[i] = fSlow75 * fZec23[i];
			}
			/* Vectorizable loop 72 */
			/* Compute code */
			for (int i = 0; i < vsize; i = i + 1) {
				fHbargraph4 = FAUSTFLOAT(0.5f * pun_int_to_float(int(8388608.0f * (std::max<float>(-126.0f, 0.1660964f * fRec36[i]) + 126.942696f))));
				fZec36[i] = fSlow76 * fZec27[i];
			}
			/* Vectorizable loop 73 */
			/* Compute code */
			for (int i = 0; i < vsize; i = i + 1) {
				fZec32[i] = ((iSlow20) ? fZec0[i] : fZec29[i]) + ((iSlow32) ? fZec5[i] : fZec30[i]) + ((iSlow48) ? fZec12[i] : fZec31[i]);
			}
			/* Vectorizable loop 74 */
			/* Compute code */
			for (int i = 0; i < vsize; i = i + 1) {
				fHbargraph1 = FAUSTFLOAT(std::fabs(fZec29[i]) + std::fabs(fZec34[i]));
				fHbargraph3 = FAUSTFLOAT(std::fabs(fZec30[i]) + std::fabs(fZec35[i]));
				fHbargraph5 = FAUSTFLOAT(std::fabs(fZec31[i]) + std::fabs(fZec36[i]));
				fZec37[i] = ((iSlow20) ? fZec2[i] : fZec34[i]) + ((iSlow32) ? fZec7[i] : fZec35[i]) + ((iSlow48) ? fZec14[i] : fZec36[i]);
			}
			/* Vectorizable loop 75 */
			/* Compute code */
			for (int i = 0; i < vsize; i = i + 1) {
				fZec33[i] = ((iSlow60) ? 0.0f : fZec32[i]);
			}
			/* Vectorizable loop 76 */
			/* Compute code */
			for (int i = 0; i < vsize; i = i + 1) {
				fZec38[i] = ((iSlow60) ? 0.0f : fZec37[i]);
			}
			/* Vectorizable loop 77 */
			/* Compute code */
			for (int i = 0; i < vsize; i = i + 1) {
				fZec39[i] = std::fabs(std::fabs(fSlow59 * fZec33[i]) + std::fabs(fSlow59 * fZec38[i]));
			}
			/* Recursive loop 78 */
			/* Pre code */
			for (int j80 = 0; j80 < 4; j80 = j80 + 1) {
				fRec29_tmp[j80] = fRec29_perm[j80];
			}
			/* Compute code */
			for (int i = 0; i < vsize; i = i + 1) {
				fRec29[i] = std::max<float>(fZec39[i], fRec29[i - 1] * fSlow78 + fZec39[i] * fSlow79);
			}
			/* Post code */
			for (int j81 = 0; j81 < 4; j81 = j81 + 1) {
				fRec29_perm[j81] = fRec29_tmp[vsize + j81];
			}
			/* Recursive loop 79 */
			/* Pre code */
			for (int j82 = 0; j82 < 4; j82 = j82 + 1) {
				fRec28_tmp[j82] = fRec28_perm[j82];
			}
			/* Compute code */
			for (int i = 0; i < vsize; i = i + 1) {
				fRec28[i] = fRec28[i - 1] * fSlow81 + fRec29[i] * fSlow82;
			}
			/* Post code */
			for (int j83 = 0; j83 < 4; j83 = j83 + 1) {
				fRec28_perm[j83] = fRec28_tmp[vsize + j83];
			}
			/* Recursive loop 80 */
			/* Pre code */
			for (int j84 = 0; j84 < 4; j84 = j84 + 1) {
				fRec27_tmp[j84] = fRec27_perm[j84];
			}
			/* Compute code */
			for (int i = 0; i < vsize; i = i + 1) {
				fRec27[i] = fRec27[i - 1] * fSlow83 + fSlow84 * std::max<float>(8.685889f * (8.262958e-08f * float(pun_float_to_int(fRec28[i])) + -87.98997f) + 6.0f, 0.0f) * fSlow85;
			}
			/* Post code */
			for (int j85 = 0; j85 < 4; j85 = j85 + 1) {
				fRec27_perm[j85] = fRec27_tmp[vsize + j85];
			}
			/* Vectorizable loop 81 */
			/* Compute code */
			for (int i = 0; i < vsize; i = i + 1) {
				fZec40[i] = pun_int_to_float(int(8388608.0f * (std::max<float>(-126.0f, 0.1660964f * fRec27[i]) + 126.942696f)));
			}
			/* Vectorizable loop 82 */
			/* Compute code */
			for (int i = 0; i < vsize; i = i + 1) {
				output0[i] = FAUSTFLOAT(fSlow86 * ((iSlow60) ? fZec32[i] : fSlow87 * fZec33[i] * fZec40[i]));
			}
			/* Vectorizable loop 83 */
			/* Compute code */
			for (int i = 0; i < vsize; i = i + 1) {
				output1[i] = FAUSTFLOAT(fSlow86 * ((iSlow60) ? fZec37[i] : fSlow87 * fZec38[i] * fZec40[i]));
			}
		}
	}

};


#include "Faust_plugins_template2.cpp"


#endif
