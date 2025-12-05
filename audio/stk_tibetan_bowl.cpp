/* ------------------------------------------------------------
author: "Romain Michon"
copyright: "Romain Michon (rmichon@ccrma.stanford.edu)"
name: "Tibetan Bowl"
version: "1.0"
Code generated with Faust 2.81.2 (https://faust.grame.fr)
Compilation options: -a ../../Faust_plugins_template.cpp -lang cpp -nvi -ct 1 -cn Tibetan_Bowl_dsp -es 1 -mcd 16 -mdd 1024 -mdy 33 -single -ftz 0
------------------------------------------------------------ */

#ifndef  __Tibetan_Bowl_dsp_H__
#define  __Tibetan_Bowl_dsp_H__


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
#define FAUSTCLASS Tibetan_Bowl_dsp
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

class Tibetan_Bowl_dspSIG0 {
	
  private:
	
	int iRec39[2];
	
  public:
	
	int getNumInputsTibetan_Bowl_dspSIG0() {
		return 0;
	}
	int getNumOutputsTibetan_Bowl_dspSIG0() {
		return 1;
	}
	
	void instanceInitTibetan_Bowl_dspSIG0(int sample_rate) {
		for (int l51 = 0; l51 < 2; l51 = l51 + 1) {
			iRec39[l51] = 0;
		}
	}
	
	void fillTibetan_Bowl_dspSIG0(int count, float* table) {
		for (int i1 = 0; i1 < count; i1 = i1 + 1) {
			iRec39[0] = iRec39[1] + 1;
			table[i1] = std::sin(9.58738e-05f * float(iRec39[0] + -1));
			iRec39[1] = iRec39[0];
		}
	}

};

static Tibetan_Bowl_dspSIG0* newTibetan_Bowl_dspSIG0() { return (Tibetan_Bowl_dspSIG0*)new Tibetan_Bowl_dspSIG0(); }
static void deleteTibetan_Bowl_dspSIG0(Tibetan_Bowl_dspSIG0* dsp) { delete dsp; }

static float Tibetan_Bowl_dsp_faustpower2_f(float value) {
	return value * value;
}
static float *ftbl0Tibetan_Bowl_dspSIG0; __attribute__((constructor)) static void initialize_ftbl0Tibetan_Bowl_dspSIG0() { ftbl0Tibetan_Bowl_dspSIG0 = (float*)calloc(65536, sizeof(float));};

class Tibetan_Bowl_dsp final : public dsp {
	
 private:
	
	FAUSTFLOAT fHslider0;
	FAUSTFLOAT fButton0;
	int iRec0[2];
	int fSampleRate;
	float fConst0;
	FAUSTFLOAT fHslider1;
	FAUSTFLOAT fHslider2;
	float fRec1[2];
	FAUSTFLOAT fEntry0;
	float fConst1;
	float fConst2;
	float fConst3;
	FAUSTFLOAT fEntry1;
	FAUSTFLOAT fEntry2;
	int iRec16[2];
	float fConst4;
	float fConst5;
	float fConst6;
	float fRec17[2];
	FAUSTFLOAT fHslider3;
	FAUSTFLOAT fHslider4;
	FAUSTFLOAT fHslider5;
	int IOTA0;
	float fVec0[4096];
	float fConst7;
	FAUSTFLOAT fEntry3;
	float fConst8;
	float fConst9;
	float fRec15[3];
	float fRec14[2];
	float fRec2[2];
	float fVec1[4096];
	float fConst10;
	float fConst11;
	float fRec19[3];
	float fRec18[2];
	float fRec3[2];
	float fVec2[4096];
	float fConst12;
	float fConst13;
	float fRec21[3];
	float fRec20[2];
	float fRec4[2];
	float fVec3[4096];
	float fConst14;
	float fConst15;
	float fRec23[3];
	float fRec22[2];
	float fRec5[2];
	float fVec4[2048];
	float fConst16;
	float fConst17;
	float fRec25[3];
	float fRec24[2];
	float fRec6[2];
	float fRec7[2];
	float fVec5[2048];
	float fConst18;
	float fConst19;
	float fRec27[3];
	float fRec26[2];
	float fRec8[2];
	float fVec6[2048];
	float fConst20;
	float fConst21;
	float fRec29[3];
	float fRec28[2];
	float fRec9[2];
	float fVec7[1024];
	float fConst22;
	float fConst23;
	float fRec31[3];
	float fRec30[2];
	float fRec10[2];
	float fVec8[1024];
	float fConst24;
	float fConst25;
	float fRec33[3];
	float fRec32[2];
	float fRec11[2];
	float fVec9[1024];
	float fConst26;
	float fConst27;
	float fRec35[3];
	float fRec34[2];
	float fRec12[2];
	float fVec10[512];
	float fConst28;
	float fConst29;
	float fRec37[3];
	float fRec36[2];
	float fRec13[2];
	float fVec11[2];
	FAUSTFLOAT fHslider6;
	float fRec38[2];
	float fConst30;
	FAUSTFLOAT fHslider7;
	float fRec41[2];
	float fRec40[2];
	float fRec47[2];
	float fRec46[2];
	float fRec45[2];
	float fRec44[2];
	float fRec43[2];
	float fRec42[2];
	float fRec53[2];
	float fRec52[2];
	float fRec51[2];
	float fRec50[2];
	float fRec49[2];
	float fRec48[2];
	float fVec12[4096];
	float fConst31;
	FAUSTFLOAT fHslider8;
	
 public:
	Tibetan_Bowl_dsp() {
	}
	
	void metadata(Meta* m) { 
		m->declare("author", "Romain Michon");
		m->declare("compile_options", "-a ../../Faust_plugins_template.cpp -lang cpp -nvi -ct 1 -cn Tibetan_Bowl_dsp -es 1 -mcd 16 -mdd 1024 -mdy 33 -single -ftz 0");
		m->declare("copyright", "Romain Michon (rmichon@ccrma.stanford.edu)");
		m->declare("description", "Banded Waveguide Modeld Tibetan Bowl");
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
		m->declare("filename", "tibetanBowl.dsp");
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
		m->declare("name", "Tibetan Bowl");
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
		Tibetan_Bowl_dspSIG0* sig0 = newTibetan_Bowl_dspSIG0();
		sig0->instanceInitTibetan_Bowl_dspSIG0(sample_rate);
		sig0->fillTibetan_Bowl_dspSIG0(65536, ftbl0Tibetan_Bowl_dspSIG0);
		deleteTibetan_Bowl_dspSIG0(sig0);
	}
	
	void instanceConstants(int sample_rate) {
		fSampleRate = sample_rate;
		fConst0 = std::min<float>(1.92e+05f, std::max<float>(1.0f, float(fSampleRate)));
		fConst1 = 100.53097f / fConst0;
		fConst2 = Tibetan_Bowl_dsp_faustpower2_f(1.0f - fConst1);
		fConst3 = 0.5f * (1.0f - fConst2);
		fConst4 = 5e+01f / fConst0;
		fConst5 = 1.0f - std::pow(9e+01f, 2e+02f / fConst0);
		fConst6 = 1.0f - 1.0f / std::pow(9e+04f, 1e+02f / fConst0);
		fConst7 = 1.0039068f * fConst0;
		fConst8 = 2.0f * (fConst1 - 1.0f);
		fConst9 = 6.2587333f / fConst0;
		fConst10 = 0.99612343f * fConst0;
		fConst11 = 6.307637f / fConst0;
		fConst12 = 0.33566305f * fConst0;
		fConst13 = 18.718727f / fConst0;
		fConst14 = 0.3340797f * fConst0;
		fConst15 = 18.807444f / fConst0;
		fConst16 = 0.17530167f * fConst0;
		fConst17 = 35.84213f / fConst0;
		fConst18 = 0.11113334f * fConst0;
		fConst19 = 56.537357f / fConst0;
		fConst20 = 0.11092012f * fConst0;
		fConst21 = 56.646038f / fConst0;
		fConst22 = 0.07792392f * fConst0;
		fConst23 = 80.63231f / fConst0;
		fConst24 = 0.07807997f * fConst0;
		fConst25 = 80.47115f / fConst0;
		fConst26 = 0.057867616f * fConst0;
		fConst27 = 108.578606f / fConst0;
		fConst28 = 0.04550413f * fConst0;
		fConst29 = 138.07945f / fConst0;
		fConst30 = 1.0f / fConst0;
		fConst31 = 0.5f * fConst0;
	}
	
	void instanceResetUserInterface() {
		fHslider0 = FAUSTFLOAT(0.6f);
		fButton0 = FAUSTFLOAT(0.0f);
		fHslider1 = FAUSTFLOAT(0.02f);
		fHslider2 = FAUSTFLOAT(0.1f);
		fEntry0 = FAUSTFLOAT(0.0f);
		fEntry1 = FAUSTFLOAT(0.0f);
		fEntry2 = FAUSTFLOAT(0.8f);
		fHslider3 = FAUSTFLOAT(1.0f);
		fHslider4 = FAUSTFLOAT(0.0f);
		fHslider5 = FAUSTFLOAT(0.2f);
		fEntry3 = FAUSTFLOAT(4.4e+02f);
		fHslider6 = FAUSTFLOAT(0.0f);
		fHslider7 = FAUSTFLOAT(2.2e+02f);
		fHslider8 = FAUSTFLOAT(0.5f);
	}
	
	void instanceClear() {
		for (int l0 = 0; l0 < 2; l0 = l0 + 1) {
			iRec0[l0] = 0;
		}
		for (int l1 = 0; l1 < 2; l1 = l1 + 1) {
			fRec1[l1] = 0.0f;
		}
		for (int l2 = 0; l2 < 2; l2 = l2 + 1) {
			iRec16[l2] = 0;
		}
		for (int l3 = 0; l3 < 2; l3 = l3 + 1) {
			fRec17[l3] = 0.0f;
		}
		IOTA0 = 0;
		for (int l4 = 0; l4 < 4096; l4 = l4 + 1) {
			fVec0[l4] = 0.0f;
		}
		for (int l5 = 0; l5 < 3; l5 = l5 + 1) {
			fRec15[l5] = 0.0f;
		}
		for (int l6 = 0; l6 < 2; l6 = l6 + 1) {
			fRec14[l6] = 0.0f;
		}
		for (int l7 = 0; l7 < 2; l7 = l7 + 1) {
			fRec2[l7] = 0.0f;
		}
		for (int l8 = 0; l8 < 4096; l8 = l8 + 1) {
			fVec1[l8] = 0.0f;
		}
		for (int l9 = 0; l9 < 3; l9 = l9 + 1) {
			fRec19[l9] = 0.0f;
		}
		for (int l10 = 0; l10 < 2; l10 = l10 + 1) {
			fRec18[l10] = 0.0f;
		}
		for (int l11 = 0; l11 < 2; l11 = l11 + 1) {
			fRec3[l11] = 0.0f;
		}
		for (int l12 = 0; l12 < 4096; l12 = l12 + 1) {
			fVec2[l12] = 0.0f;
		}
		for (int l13 = 0; l13 < 3; l13 = l13 + 1) {
			fRec21[l13] = 0.0f;
		}
		for (int l14 = 0; l14 < 2; l14 = l14 + 1) {
			fRec20[l14] = 0.0f;
		}
		for (int l15 = 0; l15 < 2; l15 = l15 + 1) {
			fRec4[l15] = 0.0f;
		}
		for (int l16 = 0; l16 < 4096; l16 = l16 + 1) {
			fVec3[l16] = 0.0f;
		}
		for (int l17 = 0; l17 < 3; l17 = l17 + 1) {
			fRec23[l17] = 0.0f;
		}
		for (int l18 = 0; l18 < 2; l18 = l18 + 1) {
			fRec22[l18] = 0.0f;
		}
		for (int l19 = 0; l19 < 2; l19 = l19 + 1) {
			fRec5[l19] = 0.0f;
		}
		for (int l20 = 0; l20 < 2048; l20 = l20 + 1) {
			fVec4[l20] = 0.0f;
		}
		for (int l21 = 0; l21 < 3; l21 = l21 + 1) {
			fRec25[l21] = 0.0f;
		}
		for (int l22 = 0; l22 < 2; l22 = l22 + 1) {
			fRec24[l22] = 0.0f;
		}
		for (int l23 = 0; l23 < 2; l23 = l23 + 1) {
			fRec6[l23] = 0.0f;
		}
		for (int l24 = 0; l24 < 2; l24 = l24 + 1) {
			fRec7[l24] = 0.0f;
		}
		for (int l25 = 0; l25 < 2048; l25 = l25 + 1) {
			fVec5[l25] = 0.0f;
		}
		for (int l26 = 0; l26 < 3; l26 = l26 + 1) {
			fRec27[l26] = 0.0f;
		}
		for (int l27 = 0; l27 < 2; l27 = l27 + 1) {
			fRec26[l27] = 0.0f;
		}
		for (int l28 = 0; l28 < 2; l28 = l28 + 1) {
			fRec8[l28] = 0.0f;
		}
		for (int l29 = 0; l29 < 2048; l29 = l29 + 1) {
			fVec6[l29] = 0.0f;
		}
		for (int l30 = 0; l30 < 3; l30 = l30 + 1) {
			fRec29[l30] = 0.0f;
		}
		for (int l31 = 0; l31 < 2; l31 = l31 + 1) {
			fRec28[l31] = 0.0f;
		}
		for (int l32 = 0; l32 < 2; l32 = l32 + 1) {
			fRec9[l32] = 0.0f;
		}
		for (int l33 = 0; l33 < 1024; l33 = l33 + 1) {
			fVec7[l33] = 0.0f;
		}
		for (int l34 = 0; l34 < 3; l34 = l34 + 1) {
			fRec31[l34] = 0.0f;
		}
		for (int l35 = 0; l35 < 2; l35 = l35 + 1) {
			fRec30[l35] = 0.0f;
		}
		for (int l36 = 0; l36 < 2; l36 = l36 + 1) {
			fRec10[l36] = 0.0f;
		}
		for (int l37 = 0; l37 < 1024; l37 = l37 + 1) {
			fVec8[l37] = 0.0f;
		}
		for (int l38 = 0; l38 < 3; l38 = l38 + 1) {
			fRec33[l38] = 0.0f;
		}
		for (int l39 = 0; l39 < 2; l39 = l39 + 1) {
			fRec32[l39] = 0.0f;
		}
		for (int l40 = 0; l40 < 2; l40 = l40 + 1) {
			fRec11[l40] = 0.0f;
		}
		for (int l41 = 0; l41 < 1024; l41 = l41 + 1) {
			fVec9[l41] = 0.0f;
		}
		for (int l42 = 0; l42 < 3; l42 = l42 + 1) {
			fRec35[l42] = 0.0f;
		}
		for (int l43 = 0; l43 < 2; l43 = l43 + 1) {
			fRec34[l43] = 0.0f;
		}
		for (int l44 = 0; l44 < 2; l44 = l44 + 1) {
			fRec12[l44] = 0.0f;
		}
		for (int l45 = 0; l45 < 512; l45 = l45 + 1) {
			fVec10[l45] = 0.0f;
		}
		for (int l46 = 0; l46 < 3; l46 = l46 + 1) {
			fRec37[l46] = 0.0f;
		}
		for (int l47 = 0; l47 < 2; l47 = l47 + 1) {
			fRec36[l47] = 0.0f;
		}
		for (int l48 = 0; l48 < 2; l48 = l48 + 1) {
			fRec13[l48] = 0.0f;
		}
		for (int l49 = 0; l49 < 2; l49 = l49 + 1) {
			fVec11[l49] = 0.0f;
		}
		for (int l50 = 0; l50 < 2; l50 = l50 + 1) {
			fRec38[l50] = 0.0f;
		}
		for (int l52 = 0; l52 < 2; l52 = l52 + 1) {
			fRec41[l52] = 0.0f;
		}
		for (int l53 = 0; l53 < 2; l53 = l53 + 1) {
			fRec40[l53] = 0.0f;
		}
		for (int l54 = 0; l54 < 2; l54 = l54 + 1) {
			fRec47[l54] = 0.0f;
		}
		for (int l55 = 0; l55 < 2; l55 = l55 + 1) {
			fRec46[l55] = 0.0f;
		}
		for (int l56 = 0; l56 < 2; l56 = l56 + 1) {
			fRec45[l56] = 0.0f;
		}
		for (int l57 = 0; l57 < 2; l57 = l57 + 1) {
			fRec44[l57] = 0.0f;
		}
		for (int l58 = 0; l58 < 2; l58 = l58 + 1) {
			fRec43[l58] = 0.0f;
		}
		for (int l59 = 0; l59 < 2; l59 = l59 + 1) {
			fRec42[l59] = 0.0f;
		}
		for (int l60 = 0; l60 < 2; l60 = l60 + 1) {
			fRec53[l60] = 0.0f;
		}
		for (int l61 = 0; l61 < 2; l61 = l61 + 1) {
			fRec52[l61] = 0.0f;
		}
		for (int l62 = 0; l62 < 2; l62 = l62 + 1) {
			fRec51[l62] = 0.0f;
		}
		for (int l63 = 0; l63 < 2; l63 = l63 + 1) {
			fRec50[l63] = 0.0f;
		}
		for (int l64 = 0; l64 < 2; l64 = l64 + 1) {
			fRec49[l64] = 0.0f;
		}
		for (int l65 = 0; l65 < 2; l65 = l65 + 1) {
			fRec48[l65] = 0.0f;
		}
		for (int l66 = 0; l66 < 4096; l66 = l66 + 1) {
			fVec12[l66] = 0.0f;
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
	
	Tibetan_Bowl_dsp* clone() {
		return new Tibetan_Bowl_dsp();
	}
	
	int getSampleRate() {
		return fSampleRate;
	}
	
	void buildUserInterface(UI* ui_interface) {
		ui_interface->openVerticalBox("Tibetan Bowl");
		ui_interface->openHorizontalBox("Basic_Parameters");
		ui_interface->declare(&fEntry3, "1", "");
		ui_interface->declare(&fEntry3, "tooltip", "Tone frequency");
		ui_interface->declare(&fEntry3, "unit", "Hz");
		ui_interface->addNumEntry("freq", &fEntry3, FAUSTFLOAT(4.4e+02f), FAUSTFLOAT(2e+01f), FAUSTFLOAT(2e+04f), FAUSTFLOAT(1.0f));
		ui_interface->declare(&fEntry2, "1", "");
		ui_interface->declare(&fEntry2, "tooltip", "Gain (value between 0 and 1)");
		ui_interface->addNumEntry("gain", &fEntry2, FAUSTFLOAT(0.8f), FAUSTFLOAT(0.0f), FAUSTFLOAT(1.0f), FAUSTFLOAT(0.01f));
		ui_interface->declare(&fButton0, "1", "");
		ui_interface->declare(&fButton0, "tooltip", "noteOn = 1, noteOff = 0");
		ui_interface->addButton("gate", &fButton0);
		ui_interface->closeBox();
		ui_interface->openHorizontalBox("Envelopes_and_Vibrato");
		ui_interface->openVerticalBox("Global_Envelope_Parameters");
		ui_interface->declare(&fHslider1, "4", "");
		ui_interface->declare(&fHslider1, "tooltip", "Global envelope attack duration");
		ui_interface->declare(&fHslider1, "unit", "s");
		ui_interface->addHorizontalSlider("Glob_Env_Attack", &fHslider1, FAUSTFLOAT(0.02f), FAUSTFLOAT(0.0f), FAUSTFLOAT(2.0f), FAUSTFLOAT(0.01f));
		ui_interface->declare(&fHslider2, "4", "");
		ui_interface->declare(&fHslider2, "tooltip", "Global envelope release duration");
		ui_interface->declare(&fHslider2, "unit", "s");
		ui_interface->addHorizontalSlider("Glob_Env_Release", &fHslider2, FAUSTFLOAT(0.1f), FAUSTFLOAT(0.0f), FAUSTFLOAT(2.0f), FAUSTFLOAT(0.01f));
		ui_interface->closeBox();
		ui_interface->closeBox();
		ui_interface->openHorizontalBox("Physical_and_Nonlinearity");
		ui_interface->openVerticalBox("Nonlinear_Filter_Parameters");
		ui_interface->declare(&fHslider7, "3", "");
		ui_interface->declare(&fHslider7, "tooltip", "Frequency of the sine wave for the modulation of theta (works if Modulation Type=3)");
		ui_interface->declare(&fHslider7, "unit", "Hz");
		ui_interface->addHorizontalSlider("Modulation_Frequency", &fHslider7, FAUSTFLOAT(2.2e+02f), FAUSTFLOAT(2e+01f), FAUSTFLOAT(1e+03f), FAUSTFLOAT(0.1f));
		ui_interface->declare(&fEntry0, "3", "");
		ui_interface->declare(&fEntry0, "tooltip", "0=theta is modulated by the incoming signal; 1=theta is modulated by the averaged incoming signal; 2=theta is modulated by the squared incoming signal; 3=theta is modulated by a sine wave of frequency freqMod; 4=theta is modulated by a sine wave of frequency freq;");
		ui_interface->addNumEntry("Modulation_Type", &fEntry0, FAUSTFLOAT(0.0f), FAUSTFLOAT(0.0f), FAUSTFLOAT(4.0f), FAUSTFLOAT(1.0f));
		ui_interface->declare(&fHslider6, "3", "");
		ui_interface->declare(&fHslider6, "tooltip", "Nonlinearity factor (value between 0 and 1)");
		ui_interface->addHorizontalSlider("Nonlinearity", &fHslider6, FAUSTFLOAT(0.0f), FAUSTFLOAT(0.0f), FAUSTFLOAT(1.0f), FAUSTFLOAT(0.01f));
		ui_interface->closeBox();
		ui_interface->openVerticalBox("Physical_Parameters");
		ui_interface->declare(&fHslider3, "2", "");
		ui_interface->declare(&fHslider3, "tooltip", "A value between 0 and 1");
		ui_interface->addHorizontalSlider("Base_Gain", &fHslider3, FAUSTFLOAT(1.0f), FAUSTFLOAT(0.0f), FAUSTFLOAT(1.0f), FAUSTFLOAT(0.01f));
		ui_interface->declare(&fHslider5, "2", "");
		ui_interface->declare(&fHslider5, "tooltip", "Bow pressure on the instrument (Value between 0 and 1)");
		ui_interface->addHorizontalSlider("Bow_Pressure", &fHslider5, FAUSTFLOAT(0.2f), FAUSTFLOAT(0.0f), FAUSTFLOAT(1.0f), FAUSTFLOAT(0.01f));
		ui_interface->declare(&fEntry1, "2", "");
		ui_interface->declare(&fEntry1, "tooltip", "0=Bow; 1=Strike");
		ui_interface->addNumEntry("Excitation_Selector", &fEntry1, FAUSTFLOAT(0.0f), FAUSTFLOAT(0.0f), FAUSTFLOAT(1.0f), FAUSTFLOAT(1.0f));
		ui_interface->declare(&fHslider4, "2", "");
		ui_interface->declare(&fHslider4, "tooltip", "A value between 0 and 1");
		ui_interface->addHorizontalSlider("Integration_Constant", &fHslider4, FAUSTFLOAT(0.0f), FAUSTFLOAT(0.0f), FAUSTFLOAT(1.0f), FAUSTFLOAT(0.01f));
		ui_interface->closeBox();
		ui_interface->closeBox();
		ui_interface->openVerticalBox("Spat");
		ui_interface->addHorizontalSlider("pan angle", &fHslider0, FAUSTFLOAT(0.6f), FAUSTFLOAT(0.0f), FAUSTFLOAT(1.0f), FAUSTFLOAT(0.01f));
		ui_interface->addHorizontalSlider("spatial width", &fHslider8, FAUSTFLOAT(0.5f), FAUSTFLOAT(0.0f), FAUSTFLOAT(1.0f), FAUSTFLOAT(0.01f));
		ui_interface->closeBox();
		ui_interface->closeBox();
	}
	
	void compute(int count, FAUSTFLOAT** RESTRICT inputs, FAUSTFLOAT** RESTRICT outputs) {
		FAUSTFLOAT* output0 = outputs[0];
		FAUSTFLOAT* output1 = outputs[1];
		float fSlow0 = float(fHslider0);
		float fSlow1 = 0.5f * (1.0f - fSlow0);
		float fSlow2 = float(fButton0);
		int iSlow3 = fSlow2 > 0.0f;
		float fSlow4 = float(fHslider1);
		float fSlow5 = 1.0f / (fConst0 * fSlow4 + float(fSlow4 == 0.0f));
		float fSlow6 = float(fHslider2);
		float fSlow7 = 1.0f - 1.0f / std::pow(1e+05f, 1.0f / (fConst0 * fSlow6 + float(fSlow6 == 0.0f)));
		int iSlow8 = fSlow2 <= 0.0f;
		float fSlow9 = float(fEntry0);
		float fSlow10 = float(fSlow9 >= 3.0f);
		float fSlow11 = float(fEntry1);
		float fSlow12 = 1.1900357f * fSlow11;
		float fSlow13 = 0.083333336f * (fSlow11 + -1.0f);
		float fSlow14 = 0.1f * float(fEntry2) + 0.03f;
		float fSlow15 = 0.1f * float(fHslider3) + 0.9f;
		float fSlow16 = float(fHslider4);
		float fSlow17 = 1e+01f - 9.0f * float(fHslider5);
		float fSlow18 = float(fEntry3);
		int iSlow19 = int(fConst7 / fSlow18) & 4095;
		float fSlow20 = fConst8 * std::cos(fConst9 * fSlow18);
		int iSlow21 = int(fConst10 / fSlow18) & 4095;
		float fSlow22 = fConst8 * std::cos(fConst11 * fSlow18);
		float fSlow23 = 1.0914886f * fSlow11;
		int iSlow24 = int(fConst12 / fSlow18) & 4095;
		float fSlow25 = fConst8 * std::cos(fConst13 * fSlow18);
		int iSlow26 = int(fConst14 / fSlow18) & 4095;
		float fSlow27 = fConst8 * std::cos(fConst15 * fSlow18);
		float fSlow28 = 4.2995043f * fSlow11;
		int iSlow29 = int(fConst16 / fSlow18) & 4095;
		float fSlow30 = fConst8 * std::cos(fConst17 * fSlow18);
		float fSlow31 = 4.0063033f * fSlow11;
		int iSlow32 = int(fConst18 / fSlow18) & 4095;
		float fSlow33 = fConst8 * std::cos(fConst19 * fSlow18);
		int iSlow34 = int(fConst20 / fSlow18) & 4095;
		float fSlow35 = fConst8 * std::cos(fConst21 * fSlow18);
		float fSlow36 = 0.7063034f * fSlow11;
		int iSlow37 = int(fConst22 / fSlow18) & 4095;
		float fSlow38 = fConst8 * std::cos(fConst23 * fSlow18);
		int iSlow39 = int(fConst24 / fSlow18) & 4095;
		float fSlow40 = fConst8 * std::cos(fConst25 * fSlow18);
		float fSlow41 = 5.7063036f * fSlow11;
		int iSlow42 = int(fConst26 / fSlow18) & 4095;
		float fSlow43 = fConst8 * std::cos(fConst27 * fSlow18);
		int iSlow44 = int(fConst28 / fSlow18) & 4095;
		float fSlow45 = fConst8 * std::cos(fConst29 * fSlow18);
		float fSlow46 = 0.001f * float(fHslider6);
		float fSlow47 = fSlow18 * float(fSlow9 == 4.0f);
		float fSlow48 = float(fSlow9 != 4.0f);
		float fSlow49 = 0.001f * float(fHslider7);
		float fSlow50 = float(fSlow9 < 3.0f);
		float fSlow51 = 3.1415927f * float(fSlow9 == 0.0f);
		float fSlow52 = 1.5707964f * float(fSlow9 == 1.0f);
		float fSlow53 = 3.1415927f * float(fSlow9 == 2.0f);
		float fSlow54 = 0.5f * fSlow0;
		int iSlow55 = int(fConst31 * (float(fHslider8) / fSlow18)) & 4095;
		for (int i0 = 0; i0 < count; i0 = i0 + 1) {
			iRec0[0] = iSlow3 & (iRec0[1] | (fRec1[1] >= 1.0f));
			int iTemp0 = iSlow8 & (fRec1[1] > 0.0f);
			fRec1[0] = (fSlow5 * float(((iRec0[1] == 0) & iSlow3) & (fRec1[1] < 1.0f)) + fRec1[1] * (1.0f - fSlow7 * float(iTemp0))) * float((iTemp0 == 0) | (fRec1[1] >= 1e-06f));
			iRec16[0] = iSlow3 & (iRec16[1] | (fRec17[1] >= 1.0f));
			int iTemp1 = iSlow8 & (fRec17[1] > 0.0f);
			fRec17[0] = (fConst4 * float(((iRec16[1] == 0) & iSlow3) & (fRec17[1] < 1.0f)) + fRec17[1] * (1.0f - fConst5 * float(iRec16[1] & (fRec17[1] > 9e+01f)) - fConst6 * float(iTemp1))) * float((iTemp1 == 0) | (fRec17[1] >= 1e-06f));
			float fTemp2 = fSlow14 * fRec17[0] - fSlow15 * (fRec2[1] + fRec4[1] + fRec6[1] + fRec8[1] + fRec10[1] + fRec12[1] + fRec3[1] + fRec5[1] + fRec7[1] + fRec9[1] + fRec11[1] + fRec13[1]) - fSlow16;
			float fTemp3 = std::pow(std::fabs(fSlow17 * fTemp2) + 0.75f, -4.0f);
			float fTemp4 = fSlow13 * fTemp2 * (float(fTemp3 > 1.0f) + fTemp3 * float(fTemp3 <= 1.0f));
			fVec0[IOTA0 & 4095] = fSlow12 + (fRec14[1] - fTemp4);
			fRec15[0] = 0.999926f * fVec0[(IOTA0 - iSlow19) & 4095] - (fSlow20 * fRec15[1] + fConst2 * fRec15[2]);
			fRec14[0] = fConst3 * (fRec15[0] - fRec15[2]);
			fRec2[0] = fRec14[0];
			fVec1[IOTA0 & 4095] = fSlow12 + (fRec18[1] - fTemp4);
			fRec19[0] = 0.999926f * fVec1[(IOTA0 - iSlow21) & 4095] - (fSlow22 * fRec19[1] + fConst2 * fRec19[2]);
			fRec18[0] = fConst3 * (fRec19[0] - fRec19[2]);
			fRec3[0] = fRec18[0];
			fVec2[IOTA0 & 4095] = fSlow23 + (fRec20[1] - fTemp4);
			fRec21[0] = 0.9999828f * fVec2[(IOTA0 - iSlow24) & 4095] - (fSlow25 * fRec21[1] + fConst2 * fRec21[2]);
			fRec20[0] = fConst3 * (fRec21[0] - fRec21[2]);
			fRec4[0] = fRec20[0];
			fVec3[IOTA0 & 4095] = fSlow23 + (fRec22[1] - fTemp4);
			fRec23[0] = 0.9999828f * fVec3[(IOTA0 - iSlow26) & 4095] - (fSlow27 * fRec23[1] + fConst2 * fRec23[2]);
			fRec22[0] = fConst3 * (fRec23[0] - fRec23[2]);
			fRec5[0] = fRec22[0];
			fVec4[IOTA0 & 2047] = fSlow28 + (fRec24[1] - fTemp4);
			fRec25[0] = fVec4[(IOTA0 - iSlow29) & 2047] - (fSlow30 * fRec25[1] + fConst2 * fRec25[2]);
			fRec24[0] = fConst3 * (fRec25[0] - fRec25[2]);
			fRec6[0] = fRec24[0];
			fRec7[0] = fRec24[0];
			fVec5[IOTA0 & 2047] = fSlow31 + (fRec26[1] - fTemp4);
			fRec27[0] = fVec5[(IOTA0 - iSlow32) & 2047] - (fSlow33 * fRec27[1] + fConst2 * fRec27[2]);
			fRec26[0] = fConst3 * (fRec27[0] - fRec27[2]);
			fRec8[0] = fRec26[0];
			fVec6[IOTA0 & 2047] = fSlow31 + (fRec28[1] - fTemp4);
			fRec29[0] = fVec6[(IOTA0 - iSlow34) & 2047] - (fSlow35 * fRec29[1] + fConst2 * fRec29[2]);
			fRec28[0] = fConst3 * (fRec29[0] - fRec29[2]);
			fRec9[0] = fRec28[0];
			fVec7[IOTA0 & 1023] = fSlow36 + (fRec30[1] - fTemp4);
			fRec31[0] = 0.9999655f * fVec7[(IOTA0 - iSlow37) & 1023] - (fSlow38 * fRec31[1] + fConst2 * fRec31[2]);
			fRec30[0] = fConst3 * (fRec31[0] - fRec31[2]);
			fRec10[0] = fRec30[0];
			fVec8[IOTA0 & 1023] = fSlow36 + (fRec32[1] - fTemp4);
			fRec33[0] = 0.9999655f * fVec8[(IOTA0 - iSlow39) & 1023] - (fSlow40 * fRec33[1] + fConst2 * fRec33[2]);
			fRec32[0] = fConst3 * (fRec33[0] - fRec33[2]);
			fRec11[0] = fRec32[0];
			fVec9[IOTA0 & 1023] = fSlow41 + (fRec34[1] - fTemp4);
			fRec35[0] = fVec9[(IOTA0 - iSlow42) & 1023] - (fSlow43 * fRec35[1] + fConst2 * fRec35[2]);
			fRec34[0] = fConst3 * (fRec35[0] - fRec35[2]);
			fRec12[0] = fRec34[0];
			fVec10[IOTA0 & 511] = fSlow41 + (fRec36[1] - fTemp4);
			fRec37[0] = fVec10[(IOTA0 - iSlow44) & 511] - (fSlow45 * fRec37[1] + fConst2 * fRec37[2]);
			fRec36[0] = fConst3 * (fRec37[0] - fRec37[2]);
			fRec13[0] = fRec36[0];
			float fTemp5 = fRec13[0] + fRec11[0] + fRec9[0] + fRec7[0] + fRec5[0] + fRec2[0] + fRec4[0] + fRec6[0] + fRec8[0] + fRec10[0] + fRec12[0] + fRec3[0];
			fVec11[0] = fTemp5;
			fRec38[0] = fSlow46 + 0.999f * fRec38[1];
			fRec41[0] = fSlow49 + 0.999f * fRec41[1];
			float fTemp6 = fRec40[1] + fConst30 * (fSlow47 + fSlow48 * fRec41[0]);
			fRec40[0] = fTemp6 - std::floor(fTemp6);
			float fTemp7 = 3.1415927f * fRec38[0] * ftbl0Tibetan_Bowl_dspSIG0[std::max<int>(0, std::min<int>(int(65536.0f * fRec40[0]), 65535))];
			float fTemp8 = std::sin(fTemp7);
			float fTemp9 = std::cos(fTemp7);
			float fTemp10 = fTemp5 * fTemp9 - fTemp8 * fRec42[1];
			float fTemp11 = fTemp9 * fTemp10 - fTemp8 * fRec43[1];
			float fTemp12 = fTemp9 * fTemp11 - fTemp8 * fRec44[1];
			float fTemp13 = fTemp9 * fTemp12 - fTemp8 * fRec45[1];
			float fTemp14 = fTemp9 * fTemp13 - fTemp8 * fRec46[1];
			fRec47[0] = fTemp9 * fTemp14 - fTemp8 * fRec47[1];
			fRec46[0] = fTemp8 * fTemp14 + fTemp9 * fRec47[1];
			fRec45[0] = fTemp8 * fTemp13 + fTemp9 * fRec46[1];
			fRec44[0] = fTemp8 * fTemp12 + fTemp9 * fRec45[1];
			fRec43[0] = fTemp8 * fTemp11 + fTemp9 * fRec44[1];
			fRec42[0] = fTemp8 * fTemp10 + fTemp9 * fRec43[1];
			float fTemp15 = fRec38[0] * (fSlow51 * fTemp5 + fSlow52 * (fTemp5 + fVec11[1]) + fSlow53 * Tibetan_Bowl_dsp_faustpower2_f(fTemp5));
			float fTemp16 = std::sin(fTemp15);
			float fTemp17 = std::cos(fTemp15);
			float fTemp18 = fTemp5 * fTemp17 - fTemp16 * fRec48[1];
			float fTemp19 = fTemp17 * fTemp18 - fTemp16 * fRec49[1];
			float fTemp20 = fTemp17 * fTemp19 - fTemp16 * fRec50[1];
			float fTemp21 = fTemp17 * fTemp20 - fTemp16 * fRec51[1];
			float fTemp22 = fTemp17 * fTemp21 - fTemp16 * fRec52[1];
			fRec53[0] = fTemp17 * fTemp22 - fTemp16 * fRec53[1];
			fRec52[0] = fTemp16 * fTemp22 + fTemp17 * fRec53[1];
			fRec51[0] = fTemp16 * fTemp21 + fTemp17 * fRec52[1];
			fRec50[0] = fTemp16 * fTemp20 + fTemp17 * fRec51[1];
			fRec49[0] = fTemp16 * fTemp19 + fTemp17 * fRec50[1];
			fRec48[0] = fTemp16 * fTemp18 + fTemp17 * fRec49[1];
			float fTemp23 = fRec1[0] * (fSlow10 * (fTemp5 * fTemp8 + fRec42[1] * fTemp9) + fSlow50 * (fRec38[0] * (fTemp5 * fTemp16 + fRec48[1] * fTemp17) + (1.0f - fRec38[0]) * fTemp5));
			fVec12[IOTA0 & 4095] = fTemp23;
			output0[i0] = FAUSTFLOAT(fSlow1 * fTemp23);
			output1[i0] = FAUSTFLOAT(fSlow54 * fVec12[(IOTA0 - iSlow55) & 4095]);
			iRec0[1] = iRec0[0];
			fRec1[1] = fRec1[0];
			iRec16[1] = iRec16[0];
			fRec17[1] = fRec17[0];
			IOTA0 = IOTA0 + 1;
			fRec15[2] = fRec15[1];
			fRec15[1] = fRec15[0];
			fRec14[1] = fRec14[0];
			fRec2[1] = fRec2[0];
			fRec19[2] = fRec19[1];
			fRec19[1] = fRec19[0];
			fRec18[1] = fRec18[0];
			fRec3[1] = fRec3[0];
			fRec21[2] = fRec21[1];
			fRec21[1] = fRec21[0];
			fRec20[1] = fRec20[0];
			fRec4[1] = fRec4[0];
			fRec23[2] = fRec23[1];
			fRec23[1] = fRec23[0];
			fRec22[1] = fRec22[0];
			fRec5[1] = fRec5[0];
			fRec25[2] = fRec25[1];
			fRec25[1] = fRec25[0];
			fRec24[1] = fRec24[0];
			fRec6[1] = fRec6[0];
			fRec7[1] = fRec7[0];
			fRec27[2] = fRec27[1];
			fRec27[1] = fRec27[0];
			fRec26[1] = fRec26[0];
			fRec8[1] = fRec8[0];
			fRec29[2] = fRec29[1];
			fRec29[1] = fRec29[0];
			fRec28[1] = fRec28[0];
			fRec9[1] = fRec9[0];
			fRec31[2] = fRec31[1];
			fRec31[1] = fRec31[0];
			fRec30[1] = fRec30[0];
			fRec10[1] = fRec10[0];
			fRec33[2] = fRec33[1];
			fRec33[1] = fRec33[0];
			fRec32[1] = fRec32[0];
			fRec11[1] = fRec11[0];
			fRec35[2] = fRec35[1];
			fRec35[1] = fRec35[0];
			fRec34[1] = fRec34[0];
			fRec12[1] = fRec12[0];
			fRec37[2] = fRec37[1];
			fRec37[1] = fRec37[0];
			fRec36[1] = fRec36[0];
			fRec13[1] = fRec13[0];
			fVec11[1] = fVec11[0];
			fRec38[1] = fRec38[0];
			fRec41[1] = fRec41[0];
			fRec40[1] = fRec40[0];
			fRec47[1] = fRec47[0];
			fRec46[1] = fRec46[0];
			fRec45[1] = fRec45[0];
			fRec44[1] = fRec44[0];
			fRec43[1] = fRec43[0];
			fRec42[1] = fRec42[0];
			fRec53[1] = fRec53[0];
			fRec52[1] = fRec52[0];
			fRec51[1] = fRec51[0];
			fRec50[1] = fRec50[0];
			fRec49[1] = fRec49[0];
			fRec48[1] = fRec48[0];
		}
	}

};


#include "Faust_plugins_template2.cpp"


#endif
