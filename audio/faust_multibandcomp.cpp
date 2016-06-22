/* ------------------------------------------------------------
Code generated with Faust 2.0.a43 (http://faust.grame.fr)
------------------------------------------------------------ */

#ifndef  __Multibandcomp_dsp_H__
#define  __Multibandcomp_dsp_H__

/*
#include "/usr/share/faust/audio/dsp.h"
#include "/usr/share/faust/gui/UI.h"
*/

// We use faust1 here.

struct Meta
{
    void declare (const char* key, const char* value) { }
};


#include <faust/dsp/dsp.h>


#if 0 //CREATE_NAME==create_zita_rev_plugin

  #include "mfaustqt1.cpp"

#else

  #include <faust/gui/faustqt.h>

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
#include <math.h>

static float faustpower2_f(float value) {
	return (value * value);
	
}

#ifndef FAUSTCLASS 
#define FAUSTCLASS Multibandcomp_dsp
#endif

class Multibandcomp_dsp : public dsp {
	
  private:
	
	float fYec0_perm[4];
	float fRec3_perm[4];
	float fRec2_perm[4];
	float fYec1_perm[4];
	float fRec1_perm[4];
	float fRec0_perm[4];
	float fYec2_perm[4];
	float fRec10_perm[4];
	float fRec9_perm[4];
	float fYec3_perm[4];
	float fRec8_perm[4];
	float fRec7_perm[4];
	float fRec6_perm[4];
	float fRec5_perm[4];
	float fRec4_perm[4];
	float fRec12_perm[4];
	float fRec11_perm[4];
	float fRec17_perm[4];
	float fRec16_perm[4];
	float fRec15_perm[4];
	float fRec14_perm[4];
	float fRec13_perm[4];
	float fRec20_perm[4];
	float fRec19_perm[4];
	float fRec18_perm[4];
	float fRec26_perm[4];
	float fRec25_perm[4];
	float fRec24_perm[4];
	float fRec23_perm[4];
	float fRec22_perm[4];
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
	float fRec29_perm[4];
	float fRec28_perm[4];
	float fRec27_perm[4];
	FAUSTFLOAT* fInput0_ptr;
	FAUSTFLOAT* fInput1_ptr;
	FAUSTFLOAT* fOutput0_ptr;
	FAUSTFLOAT* fOutput1_ptr;
	int fSamplingFreq;
	float fConst0;
	float fConst1;
	FAUSTFLOAT fVslider0;
	FAUSTFLOAT fVslider1;
	FAUSTFLOAT fHslider0;
	FAUSTFLOAT fCheckbox0;
	float fConst2;
	FAUSTFLOAT fHslider1;
	FAUSTFLOAT fHslider2;
	float fConst3;
	FAUSTFLOAT fHslider3;
	FAUSTFLOAT fHslider4;
	FAUSTFLOAT fHslider5;
	FAUSTFLOAT fCheckbox1;
	FAUSTFLOAT fHslider6;
	FAUSTFLOAT fHslider7;
	FAUSTFLOAT fHslider8;
	FAUSTFLOAT fHslider9;
	FAUSTFLOAT fHslider10;
	FAUSTFLOAT fCheckbox2;
	FAUSTFLOAT fHslider11;
	FAUSTFLOAT fHslider12;
	FAUSTFLOAT fHslider13;
	FAUSTFLOAT fHslider14;
	FAUSTFLOAT fHslider15;
	FAUSTFLOAT fCheckbox3;
	FAUSTFLOAT fHslider16;
	FAUSTFLOAT fCheckbox4;
	FAUSTFLOAT fCheckbox5;
	FAUSTFLOAT fCheckbox6;
	FAUSTFLOAT fHslider17;
	FAUSTFLOAT fHslider18;
	FAUSTFLOAT fHbargraph0;
	FAUSTFLOAT fHbargraph1;
	FAUSTFLOAT fHbargraph2;
	FAUSTFLOAT fHbargraph3;
	FAUSTFLOAT fHbargraph4;
	FAUSTFLOAT fHbargraph5;
	float fConst4;
	FAUSTFLOAT fVslider2;
	float fConst5;
	FAUSTFLOAT fVslider3;
	float fConst6;
	FAUSTFLOAT fVslider4;
	FAUSTFLOAT fHslider19;
	FAUSTFLOAT fHslider20;
	
  public:
	
	void static metadata(Meta* m) { 
		m->declare("effect.lib/author", "Julius O. Smith (jos at ccrma.stanford.edu)");
		m->declare("effect.lib/copyright", "Julius O. Smith III");
		m->declare("effect.lib/exciter_author", "Priyanka Shekar (pshekar@ccrma.stanford.edu)");
		m->declare("effect.lib/exciter_copyright", "Copyright (c) 2013 Priyanka Shekar");
		m->declare("effect.lib/exciter_license", "MIT License (MIT)");
		m->declare("effect.lib/exciter_name", "Harmonic Exciter");
		m->declare("effect.lib/exciter_version", "1.0");
		m->declare("effect.lib/license", "STK-4.3");
		m->declare("effect.lib/name", "Faust Audio Effect Library");
		m->declare("effect.lib/version", "1.33");
		m->declare("filter.lib/author", "Julius O. Smith (jos at ccrma.stanford.edu)");
		m->declare("filter.lib/copyright", "Julius O. Smith III");
		m->declare("filter.lib/license", "STK-4.3");
		m->declare("filter.lib/name", "Faust Filter Library");
		m->declare("filter.lib/reference", "https://ccrma.stanford.edu/~jos/filters/");
		m->declare("filter.lib/version", "1.29");
		m->declare("math.lib/author", "GRAME");
		m->declare("math.lib/copyright", "GRAME");
		m->declare("math.lib/license", "LGPL with exception");
		m->declare("math.lib/name", "Math Library");
		m->declare("math.lib/version", "1.0");
		m->declare("music.lib/author", "GRAME");
		m->declare("music.lib/copyright", "GRAME");
		m->declare("music.lib/license", "LGPL with exception");
		m->declare("music.lib/name", "Music Library");
		m->declare("music.lib/version", "1.0");
	}

	virtual int getNumInputs() {
		return 2;
		
	}
	virtual int getNumOutputs() {
		return 2;
		
	}
	virtual int getInputRate(int channel) {
		int rate;
		switch (channel) {
			case 0: {
				rate = 0;
				break;
			}
			case 1: {
				rate = 0;
				break;
			}
			default: {
				rate = -1;
				break;
			}
			
		}
		return rate;
		
	}
	virtual int getOutputRate(int channel) {
		int rate;
		switch (channel) {
			case 0: {
				rate = 1;
				break;
			}
			case 1: {
				rate = 1;
				break;
			}
			default: {
				rate = -1;
				break;
			}
			
		}
		return rate;
		
	}
	
	static void classInit(int samplingFreq) {
		
	}
	
	virtual void instanceInit(int samplingFreq) {
		fSamplingFreq = samplingFreq;
		fConst0 = min(1.92e+05f, max(1.0f, float(fSamplingFreq)));
		fConst1 = (3.1415927f / fConst0);
		fVslider0 = FAUSTFLOAT(1.5e+03f);
		for (int i0 = 0; (i0 < 4); i0 = (i0 + 1)) {
			fYec0_perm[i0] = 0.0f;
			
		}
		for (int i1 = 0; (i1 < 4); i1 = (i1 + 1)) {
			fRec3_perm[i1] = 0.0f;
			
		}
		for (int i2 = 0; (i2 < 4); i2 = (i2 + 1)) {
			fRec2_perm[i2] = 0.0f;
			
		}
		fVslider1 = FAUSTFLOAT(166.0f);
		for (int i3 = 0; (i3 < 4); i3 = (i3 + 1)) {
			fYec1_perm[i3] = 0.0f;
			
		}
		for (int i4 = 0; (i4 < 4); i4 = (i4 + 1)) {
			fRec1_perm[i4] = 0.0f;
			
		}
		for (int i5 = 0; (i5 < 4); i5 = (i5 + 1)) {
			fRec0_perm[i5] = 0.0f;
			
		}
		for (int i6 = 0; (i6 < 4); i6 = (i6 + 1)) {
			fYec2_perm[i6] = 0.0f;
			
		}
		for (int i7 = 0; (i7 < 4); i7 = (i7 + 1)) {
			fRec10_perm[i7] = 0.0f;
			
		}
		for (int i8 = 0; (i8 < 4); i8 = (i8 + 1)) {
			fRec9_perm[i8] = 0.0f;
			
		}
		for (int i9 = 0; (i9 < 4); i9 = (i9 + 1)) {
			fYec3_perm[i9] = 0.0f;
			
		}
		for (int i10 = 0; (i10 < 4); i10 = (i10 + 1)) {
			fRec8_perm[i10] = 0.0f;
			
		}
		for (int i11 = 0; (i11 < 4); i11 = (i11 + 1)) {
			fRec7_perm[i11] = 0.0f;
			
		}
		fHslider0 = FAUSTFLOAT(0.0f);
		fCheckbox0 = FAUSTFLOAT(0.0f);
		fConst2 = (1.0f / fConst0);
		fHslider1 = FAUSTFLOAT(2e+02f);
		for (int i12 = 0; (i12 < 4); i12 = (i12 + 1)) {
			fRec6_perm[i12] = 0.0f;
			
		}
		fHslider2 = FAUSTFLOAT(1e+02f);
		for (int i13 = 0; (i13 < 4); i13 = (i13 + 1)) {
			fRec5_perm[i13] = 0.0f;
			
		}
		fConst3 = (2.0f / fConst0);
		fHslider3 = FAUSTFLOAT(2.0f);
		fHslider4 = FAUSTFLOAT(-2e+01f);
		for (int i14 = 0; (i14 < 4); i14 = (i14 + 1)) {
			fRec4_perm[i14] = 0.0f;
			
		}
		for (int i15 = 0; (i15 < 4); i15 = (i15 + 1)) {
			fRec12_perm[i15] = 0.0f;
			
		}
		for (int i16 = 0; (i16 < 4); i16 = (i16 + 1)) {
			fRec11_perm[i16] = 0.0f;
			
		}
		for (int i17 = 0; (i17 < 4); i17 = (i17 + 1)) {
			fRec17_perm[i17] = 0.0f;
			
		}
		for (int i18 = 0; (i18 < 4); i18 = (i18 + 1)) {
			fRec16_perm[i18] = 0.0f;
			
		}
		fHslider5 = FAUSTFLOAT(0.0f);
		fCheckbox1 = FAUSTFLOAT(0.0f);
		fHslider6 = FAUSTFLOAT(2e+02f);
		for (int i19 = 0; (i19 < 4); i19 = (i19 + 1)) {
			fRec15_perm[i19] = 0.0f;
			
		}
		fHslider7 = FAUSTFLOAT(1e+02f);
		for (int i20 = 0; (i20 < 4); i20 = (i20 + 1)) {
			fRec14_perm[i20] = 0.0f;
			
		}
		fHslider8 = FAUSTFLOAT(2.0f);
		fHslider9 = FAUSTFLOAT(-2e+01f);
		for (int i21 = 0; (i21 < 4); i21 = (i21 + 1)) {
			fRec13_perm[i21] = 0.0f;
			
		}
		for (int i22 = 0; (i22 < 4); i22 = (i22 + 1)) {
			fRec20_perm[i22] = 0.0f;
			
		}
		for (int i23 = 0; (i23 < 4); i23 = (i23 + 1)) {
			fRec19_perm[i23] = 0.0f;
			
		}
		for (int i24 = 0; (i24 < 4); i24 = (i24 + 1)) {
			fRec18_perm[i24] = 0.0f;
			
		}
		for (int i25 = 0; (i25 < 4); i25 = (i25 + 1)) {
			fRec26_perm[i25] = 0.0f;
			
		}
		for (int i26 = 0; (i26 < 4); i26 = (i26 + 1)) {
			fRec25_perm[i26] = 0.0f;
			
		}
		for (int i27 = 0; (i27 < 4); i27 = (i27 + 1)) {
			fRec24_perm[i27] = 0.0f;
			
		}
		fHslider10 = FAUSTFLOAT(0.0f);
		fCheckbox2 = FAUSTFLOAT(0.0f);
		fHslider11 = FAUSTFLOAT(2e+02f);
		for (int i28 = 0; (i28 < 4); i28 = (i28 + 1)) {
			fRec23_perm[i28] = 0.0f;
			
		}
		fHslider12 = FAUSTFLOAT(1e+02f);
		for (int i29 = 0; (i29 < 4); i29 = (i29 + 1)) {
			fRec22_perm[i29] = 0.0f;
			
		}
		fHslider13 = FAUSTFLOAT(2.0f);
		fHslider14 = FAUSTFLOAT(-2e+01f);
		for (int i30 = 0; (i30 < 4); i30 = (i30 + 1)) {
			fRec21_perm[i30] = 0.0f;
			
		}
		for (int i31 = 0; (i31 < 4); i31 = (i31 + 1)) {
			fRec32_perm[i31] = 0.0f;
			
		}
		for (int i32 = 0; (i32 < 4); i32 = (i32 + 1)) {
			fRec31_perm[i32] = 0.0f;
			
		}
		for (int i33 = 0; (i33 < 4); i33 = (i33 + 1)) {
			fRec30_perm[i33] = 0.0f;
			
		}
		for (int i34 = 0; (i34 < 4); i34 = (i34 + 1)) {
			fRec35_perm[i34] = 0.0f;
			
		}
		for (int i35 = 0; (i35 < 4); i35 = (i35 + 1)) {
			fRec34_perm[i35] = 0.0f;
			
		}
		for (int i36 = 0; (i36 < 4); i36 = (i36 + 1)) {
			fRec33_perm[i36] = 0.0f;
			
		}
		for (int i37 = 0; (i37 < 4); i37 = (i37 + 1)) {
			fRec38_perm[i37] = 0.0f;
			
		}
		for (int i38 = 0; (i38 < 4); i38 = (i38 + 1)) {
			fRec37_perm[i38] = 0.0f;
			
		}
		for (int i39 = 0; (i39 < 4); i39 = (i39 + 1)) {
			fRec36_perm[i39] = 0.0f;
			
		}
		fHslider15 = FAUSTFLOAT(0.0f);
		fCheckbox3 = FAUSTFLOAT(0.0f);
		fHslider16 = FAUSTFLOAT(0.0f);
		fCheckbox4 = FAUSTFLOAT(0.0f);
		fCheckbox5 = FAUSTFLOAT(0.0f);
		fCheckbox6 = FAUSTFLOAT(0.0f);
		fHslider17 = FAUSTFLOAT(0.0f);
		fHslider18 = FAUSTFLOAT(0.0f);
		fConst4 = (1e+03f / fConst0);
		fVslider2 = FAUSTFLOAT(5e+02f);
		for (int i40 = 0; (i40 < 4); i40 = (i40 + 1)) {
			fRec29_perm[i40] = 0.0f;
			
		}
		fConst5 = (1e+06f / fConst0);
		fVslider3 = FAUSTFLOAT(8e+02f);
		for (int i41 = 0; (i41 < 4); i41 = (i41 + 1)) {
			fRec28_perm[i41] = 0.0f;
			
		}
		fConst6 = (2e+06f / fConst0);
		fVslider4 = FAUSTFLOAT(4.0f);
		for (int i42 = 0; (i42 < 4); i42 = (i42 + 1)) {
			fRec27_perm[i42] = 0.0f;
			
		}
		fHslider19 = FAUSTFLOAT(0.0f);
		fHslider20 = FAUSTFLOAT(0.0f);
		
	}
	
	virtual void init(int samplingFreq) {
		classInit(samplingFreq);
		instanceInit(samplingFreq);
	}
	
	virtual void buildUserInterface(UI* interface) {
		interface->openVerticalBox("0x00");
		interface->declare(&fCheckbox4, "0", "");
		interface->declare(&fCheckbox4, "tooltip", "When this is checked, the compressor is enabled. If not, sound is muted.");
		interface->addCheckButton("Band 1:  Solo",&fCheckbox4);
		interface->declare(&fHbargraph1, "1", "");
		interface->declare(&fHbargraph1, "7", "");
		interface->addHorizontalBargraph("Band 1:   Outgain", &fHbargraph1, 0.0f, 1.0f);
		interface->declare(&fCheckbox0, "0.5", "");
		interface->declare(&fCheckbox0, "tooltip", "When this is checked, the compressor is enabled. If not, sound is muted.");
		interface->addCheckButton("Band 1: Bypass",&fCheckbox0);
		interface->declare(&fHslider3, "2", "");
		interface->declare(&fHslider3, "style", "slider");
		interface->declare(&fHslider3, "tooltip", "A compression Ratio of N means that for each N dB increase in input signal level above Threshold, the output level goes up 1 dB");
		interface->addHorizontalSlider("Band 1: Ratio", &fHslider3, 2.0f, 1.0f, 2e+01f, 0.1f);
		interface->declare(&fHslider4, "3", "");
		interface->declare(&fHslider4, "style", "slider");
		interface->declare(&fHslider4, "tooltip", "When the signal level exceeds the Threshold (in dB), its level is compressed according to the Ratio");
		interface->declare(&fHslider4, "unit", "dB");
		interface->addHorizontalSlider("Band 1: Threshold", &fHslider4, -2e+01f, -1e+02f, 1e+01f, 0.1f);
		interface->declare(&fHslider2, "4", "");
		interface->declare(&fHslider2, "style", "slider");
		interface->declare(&fHslider2, "tooltip", "Time constant in ms (1/e smoothing time) for the compression gain to approach (exponentially) a new lower target level (the compression `kicking in')");
		interface->declare(&fHslider2, "unit", "ms");
		interface->addHorizontalSlider("Band 1: Attack", &fHslider2, 1e+02f, 0.0f, 5e+02f, 0.1f);
		interface->declare(&fHslider1, "5", "");
		interface->declare(&fHslider1, "style", "slider");
		interface->declare(&fHslider1, "tooltip", "Time constant in ms (1/e smoothing time) for the compression gain to approach (exponentially) a new higher target level (the compression 'releasing')");
		interface->declare(&fHslider1, "unit", "ms");
		interface->addHorizontalSlider("Band 1: Release", &fHslider1, 2e+02f, 0.0f, 1e+03f, 0.1f);
		interface->declare(&fHbargraph0, "1", "");
		interface->declare(&fHbargraph0, "6", "");
		interface->declare(&fHbargraph0, "tooltip", "dummy tooltip");
		interface->addHorizontalBargraph("Band 1: Input Gain", &fHbargraph0, 0.0f, 1.0f);
		interface->declare(&fHslider0, "2", "");
		interface->declare(&fHslider0, "6", "");
		interface->declare(&fHslider0, "tooltip", "The compressed-signal output level is increased by this amount (in dB) to make up for the level lost due to compression");
		interface->declare(&fHslider0, "unit", "dB");
		interface->addHorizontalSlider("Band 1: Input Gain", &fHslider0, 0.0f, -4e+01f, 4e+01f, 0.1f);
		interface->declare(&fHslider16, "2", "");
		interface->declare(&fHslider16, "7", "");
		interface->declare(&fHslider16, "tooltip", "The compressed-signal output level is increased by this amount (in dB) to make up for the level lost due to compression");
		interface->declare(&fHslider16, "unit", "dB");
		interface->addHorizontalSlider("Band 1: Output Gain", &fHslider16, 0.0f, -4e+01f, 4e+01f, 0.1f);
		interface->declare(&fCheckbox5, "0", "");
		interface->declare(&fCheckbox5, "tooltip", "When this is checked, the compressor is enabled. If not, sound is muted.");
		interface->addCheckButton("Band 2:  Solo",&fCheckbox5);
		interface->declare(&fHbargraph3, "1", "");
		interface->declare(&fHbargraph3, "7", "");
		interface->addHorizontalBargraph("Band 2:   Outgain", &fHbargraph3, 0.0f, 1.0f);
		interface->declare(&fCheckbox1, "0.5", "");
		interface->declare(&fCheckbox1, "tooltip", "When this is checked, the compressor is enabled. If not, sound is muted.");
		interface->addCheckButton("Band 2: Bypass",&fCheckbox1);
		interface->declare(&fHslider8, "2", "");
		interface->declare(&fHslider8, "style", "slider");
		interface->declare(&fHslider8, "tooltip", "A compression Ratio of N means that for each N dB increase in input signal level above Threshold, the output level goes up 1 dB");
		interface->addHorizontalSlider("Band 2: Ratio", &fHslider8, 2.0f, 1.0f, 2e+01f, 0.1f);
		interface->declare(&fHslider9, "3", "");
		interface->declare(&fHslider9, "style", "slider");
		interface->declare(&fHslider9, "tooltip", "When the signal level exceeds the Threshold (in dB), its level is compressed according to the Ratio");
		interface->declare(&fHslider9, "unit", "dB");
		interface->addHorizontalSlider("Band 2: Threshold", &fHslider9, -2e+01f, -1e+02f, 1e+01f, 0.1f);
		interface->declare(&fHslider7, "4", "");
		interface->declare(&fHslider7, "style", "slider");
		interface->declare(&fHslider7, "tooltip", "Time constant in ms (1/e smoothing time) for the compression gain to approach (exponentially) a new lower target level (the compression `kicking in')");
		interface->declare(&fHslider7, "unit", "ms");
		interface->addHorizontalSlider("Band 2: Attack", &fHslider7, 1e+02f, 0.0f, 5e+02f, 0.1f);
		interface->declare(&fHslider6, "5", "");
		interface->declare(&fHslider6, "style", "slider");
		interface->declare(&fHslider6, "tooltip", "Time constant in ms (1/e smoothing time) for the compression gain to approach (exponentially) a new higher target level (the compression 'releasing')");
		interface->declare(&fHslider6, "unit", "ms");
		interface->addHorizontalSlider("Band 2: Release", &fHslider6, 2e+02f, 0.0f, 1e+03f, 0.1f);
		interface->declare(&fHbargraph2, "1", "");
		interface->declare(&fHbargraph2, "6", "");
		interface->declare(&fHbargraph2, "tooltip", "dummy tooltip");
		interface->addHorizontalBargraph("Band 2: Input Gain", &fHbargraph2, 0.0f, 1.0f);
		interface->declare(&fHslider5, "2", "");
		interface->declare(&fHslider5, "6", "");
		interface->declare(&fHslider5, "tooltip", "The compressed-signal output level is increased by this amount (in dB) to make up for the level lost due to compression");
		interface->declare(&fHslider5, "unit", "dB");
		interface->addHorizontalSlider("Band 2: Input Gain", &fHslider5, 0.0f, -4e+01f, 4e+01f, 0.1f);
		interface->declare(&fHslider17, "2", "");
		interface->declare(&fHslider17, "7", "");
		interface->declare(&fHslider17, "tooltip", "The compressed-signal output level is increased by this amount (in dB) to make up for the level lost due to compression");
		interface->declare(&fHslider17, "unit", "dB");
		interface->addHorizontalSlider("Band 2: Output Gain", &fHslider17, 0.0f, -4e+01f, 4e+01f, 0.1f);
		interface->declare(&fCheckbox6, "0", "");
		interface->declare(&fCheckbox6, "tooltip", "When this is checked, the compressor is enabled. If not, sound is muted.");
		interface->addCheckButton("Band 3:  Solo",&fCheckbox6);
		interface->declare(&fHbargraph5, "1", "");
		interface->declare(&fHbargraph5, "7", "");
		interface->addHorizontalBargraph("Band 3:   Outgain", &fHbargraph5, 0.0f, 1.0f);
		interface->declare(&fCheckbox2, "0.5", "");
		interface->declare(&fCheckbox2, "tooltip", "When this is checked, the compressor is enabled. If not, sound is muted.");
		interface->addCheckButton("Band 3: Bypass",&fCheckbox2);
		interface->declare(&fHslider13, "2", "");
		interface->declare(&fHslider13, "style", "slider");
		interface->declare(&fHslider13, "tooltip", "A compression Ratio of N means that for each N dB increase in input signal level above Threshold, the output level goes up 1 dB");
		interface->addHorizontalSlider("Band 3: Ratio", &fHslider13, 2.0f, 1.0f, 2e+01f, 0.1f);
		interface->declare(&fHslider14, "3", "");
		interface->declare(&fHslider14, "style", "slider");
		interface->declare(&fHslider14, "tooltip", "When the signal level exceeds the Threshold (in dB), its level is compressed according to the Ratio");
		interface->declare(&fHslider14, "unit", "dB");
		interface->addHorizontalSlider("Band 3: Threshold", &fHslider14, -2e+01f, -1e+02f, 1e+01f, 0.1f);
		interface->declare(&fHslider12, "4", "");
		interface->declare(&fHslider12, "style", "slider");
		interface->declare(&fHslider12, "tooltip", "Time constant in ms (1/e smoothing time) for the compression gain to approach (exponentially) a new lower target level (the compression `kicking in')");
		interface->declare(&fHslider12, "unit", "ms");
		interface->addHorizontalSlider("Band 3: Attack", &fHslider12, 1e+02f, 0.0f, 5e+02f, 0.1f);
		interface->declare(&fHslider11, "5", "");
		interface->declare(&fHslider11, "style", "slider");
		interface->declare(&fHslider11, "tooltip", "Time constant in ms (1/e smoothing time) for the compression gain to approach (exponentially) a new higher target level (the compression 'releasing')");
		interface->declare(&fHslider11, "unit", "ms");
		interface->addHorizontalSlider("Band 3: Release", &fHslider11, 2e+02f, 0.0f, 1e+03f, 0.1f);
		interface->declare(&fHbargraph4, "1", "");
		interface->declare(&fHbargraph4, "6", "");
		interface->declare(&fHbargraph4, "tooltip", "dummy tooltip");
		interface->addHorizontalBargraph("Band 3: Input Gain", &fHbargraph4, 0.0f, 1.0f);
		interface->declare(&fHslider10, "2", "");
		interface->declare(&fHslider10, "6", "");
		interface->declare(&fHslider10, "tooltip", "The compressed-signal output level is increased by this amount (in dB) to make up for the level lost due to compression");
		interface->declare(&fHslider10, "unit", "dB");
		interface->addHorizontalSlider("Band 3: Input Gain", &fHslider10, 0.0f, -4e+01f, 4e+01f, 0.1f);
		interface->declare(&fHslider18, "2", "");
		interface->declare(&fHslider18, "7", "");
		interface->declare(&fHslider18, "tooltip", "The compressed-signal output level is increased by this amount (in dB) to make up for the level lost due to compression");
		interface->declare(&fHslider18, "unit", "dB");
		interface->addHorizontalSlider("Band 3: Output Gain", &fHslider18, 0.0f, -4e+01f, 4e+01f, 0.1f);
		interface->declare(&fVslider1, "C", "");
		interface->declare(&fVslider1, "style", "knob");
		interface->declare(&fVslider1, "tooltip", "Center-frequency of second-order Regalia-Mitra peaking equalizer section 1");
		interface->declare(&fVslider1, "unit", "Hz");
		interface->addVerticalSlider("Split Freq 1", &fVslider1, 166.0f, 4e+01f, 999.0f, 1.0f);
		interface->declare(&fVslider0, "D", "");
		interface->declare(&fVslider0, "style", "knob");
		interface->declare(&fVslider0, "tooltip", "Center-frequency of second-order Regalia-Mitra peaking equalizer section 1");
		interface->declare(&fVslider0, "unit", "Hz");
		interface->addVerticalSlider("Split Freq 2", &fVslider0, 1.5e+03f, 1e+03f, 1.5e+04f, 1.0f);
		interface->declare(&fCheckbox3, "E", "");
		interface->addCheckButton("Limiter Bypass",&fCheckbox3);
		interface->declare(&fHslider15, "F", "");
		interface->declare(&fHslider15, "tooltip", "Adjust overall gain.");
		interface->declare(&fHslider15, "unit", "dB");
		interface->addHorizontalSlider("Limiter Input Gain", &fHslider15, 0.0f, -4e+01f, 4e+01f, 0.1f);
		interface->declare(&fVslider4, "G", "");
		interface->declare(&fVslider4, "unit", ":1");
		interface->addVerticalSlider("Limiter Ratio", &fVslider4, 4.0f, 4.0f, 2e+01f, 1.0f);
		interface->declare(&fVslider3, "H", "");
		interface->declare(&fVslider3, "unit", "us");
		interface->addVerticalSlider("Limiter Attack", &fVslider3, 8e+02f, 2e+01f, 8e+02f, 1.0f);
		interface->declare(&fVslider2, "I", "");
		interface->declare(&fVslider2, "unit", "ms");
		interface->addVerticalSlider("Limiter Release", &fVslider2, 5e+02f, 5e+01f, 1.1e+03f, 1.0f);
		interface->declare(&fHslider20, "J", "");
		interface->declare(&fHslider20, "tooltip", "Adjust overall gain.");
		interface->declare(&fHslider20, "unit", "dB");
		interface->addHorizontalSlider("Limiter Output Gain", &fHslider20, 0.0f, -4e+01f, 4e+01f, 0.1f);
		interface->declare(&fHslider19, "K", "");
		interface->declare(&fHslider19, "tooltip", "Adjust overall gain.");
		interface->declare(&fHslider19, "unit", "dB");
		interface->addHorizontalSlider("Final Output Gain", &fHslider19, 0.0f, -4e+01f, 4e+01f, 0.1f);
		interface->closeBox();
		
	}
	
	virtual void compute(int count, FAUSTFLOAT** inputs, FAUSTFLOAT** outputs) {
		float fYec0_tmp[36];
		float fRec3_tmp[36];
		float fRec2_tmp[36];
		float fYec1_tmp[36];
		float fRec1_tmp[36];
		float fRec0_tmp[36];
		float fYec2_tmp[36];
		float fRec10_tmp[36];
		float fRec9_tmp[36];
		float fYec3_tmp[36];
		float fRec8_tmp[36];
		float fRec7_tmp[36];
		float fRec6_tmp[36];
		float fRec5_tmp[36];
		float fRec4_tmp[36];
		float fRec12_tmp[36];
		float fRec11_tmp[36];
		float fRec17_tmp[36];
		float fRec16_tmp[36];
		float fRec15_tmp[36];
		float fRec14_tmp[36];
		float fRec13_tmp[36];
		float fRec20_tmp[36];
		float fRec19_tmp[36];
		float fRec18_tmp[36];
		float fRec26_tmp[36];
		float fRec25_tmp[36];
		float fRec24_tmp[36];
		float fRec23_tmp[36];
		float fRec22_tmp[36];
		float fRec21_tmp[36];
		float fRec32_tmp[36];
		float fRec31_tmp[36];
		float fRec30_tmp[36];
		float fRec35_tmp[36];
		float fRec34_tmp[36];
		float fRec33_tmp[36];
		float fRec38_tmp[36];
		float fRec37_tmp[36];
		float fRec36_tmp[36];
		float fRec29_tmp[36];
		float fRec28_tmp[36];
		float fRec27_tmp[36];
		float fZec0[32];
		float fZec1[32];
		float fZec2[32];
		float fZec3[32];
		float fZec4[32];
		float fZec5[32];
		float fZec6[32];
		float fZec7[32];
		float fZec8[32];
		float fZec9[32];
		float fZec10[32];
		float fZec11[32];
		float fZec12[32];
		float fZec13[32];
		float fZec14[32];
		float fZec15[32];
		float fZec16[32];
		float fZec17[32];
		float fZec18[32];
		float fZec19[32];
		float fZec20[32];
		float fZec21[32];
		float fZec22[32];
		float fZec23[32];
		float fZec24[32];
		float fZec25[32];
		float fZec26[32];
		float fZec27[32];
		float fZec28[32];
		float fZec29[32];
		float fZec30[32];
		float fZec31[32];
		float fZec32[32];
		float fZec33[32];
		float fZec34[32];
		float fZec35[32];
		float fZec36[32];
		float fZec37[32];
		float fZec38[32];
		float fZec39[32];
		float fZec40[32];
		float fZec41[32];
		float fZec42[32];
		FAUSTFLOAT* fInput0 = 0;
		FAUSTFLOAT* fInput1 = 0;
		FAUSTFLOAT* fOutput0 = 0;
		FAUSTFLOAT* fOutput1 = 0;
		float* fYec0 = &fYec0_tmp[4];
		float* fRec3 = &fRec3_tmp[4];
		float* fRec2 = &fRec2_tmp[4];
		float* fYec1 = &fYec1_tmp[4];
		float* fRec1 = &fRec1_tmp[4];
		float* fRec0 = &fRec0_tmp[4];
		float* fYec2 = &fYec2_tmp[4];
		float* fRec10 = &fRec10_tmp[4];
		float* fRec9 = &fRec9_tmp[4];
		float* fYec3 = &fYec3_tmp[4];
		float* fRec8 = &fRec8_tmp[4];
		float* fRec7 = &fRec7_tmp[4];
		float* fRec6 = &fRec6_tmp[4];
		float* fRec5 = &fRec5_tmp[4];
		float* fRec4 = &fRec4_tmp[4];
		float* fRec12 = &fRec12_tmp[4];
		float* fRec11 = &fRec11_tmp[4];
		float* fRec17 = &fRec17_tmp[4];
		float* fRec16 = &fRec16_tmp[4];
		float* fRec15 = &fRec15_tmp[4];
		float* fRec14 = &fRec14_tmp[4];
		float* fRec13 = &fRec13_tmp[4];
		float* fRec20 = &fRec20_tmp[4];
		float* fRec19 = &fRec19_tmp[4];
		float* fRec18 = &fRec18_tmp[4];
		float* fRec26 = &fRec26_tmp[4];
		float* fRec25 = &fRec25_tmp[4];
		float* fRec24 = &fRec24_tmp[4];
		float* fRec23 = &fRec23_tmp[4];
		float* fRec22 = &fRec22_tmp[4];
		float* fRec21 = &fRec21_tmp[4];
		float* fRec32 = &fRec32_tmp[4];
		float* fRec31 = &fRec31_tmp[4];
		float* fRec30 = &fRec30_tmp[4];
		float* fRec35 = &fRec35_tmp[4];
		float* fRec34 = &fRec34_tmp[4];
		float* fRec33 = &fRec33_tmp[4];
		float* fRec38 = &fRec38_tmp[4];
		float* fRec37 = &fRec37_tmp[4];
		float* fRec36 = &fRec36_tmp[4];
		float* fRec29 = &fRec29_tmp[4];
		float* fRec28 = &fRec28_tmp[4];
		float* fRec27 = &fRec27_tmp[4];
		fInput0_ptr = inputs[0];
		fInput1_ptr = inputs[1];
		fOutput0_ptr = outputs[0];
		fOutput1_ptr = outputs[1];
		float fSlow0 = tanf((fConst1 * float(fVslider0)));
		float fSlow1 = (1.0f / fSlow0);
		float fSlow2 = (1.0f + fSlow1);
		float fSlow3 = (0.0f - ((1.0f - fSlow1) / fSlow2));
		float fSlow4 = (1.0f / fSlow2);
		float fSlow5 = (1.0f + ((1.0f + fSlow1) / fSlow0));
		float fSlow6 = (1.0f / fSlow5);
		float fSlow7 = (1.0f + ((fSlow1 - 1.0f) / fSlow0));
		float fSlow8 = (1.0f / faustpower2_f(fSlow0));
		float fSlow9 = (2.0f * (1.0f - fSlow8));
		float fSlow10 = tanf((fConst1 * float(fVslider1)));
		float fSlow11 = (1.0f / fSlow10);
		float fSlow12 = (1.0f + fSlow11);
		float fSlow13 = (0.0f - ((1.0f - fSlow11) / fSlow12));
		float fSlow14 = (1.0f / fSlow12);
		float fSlow15 = (1.0f / (1.0f + ((fSlow11 + 1.0f) / fSlow10)));
		float fSlow16 = (1.0f + ((fSlow11 - 1.0f) / fSlow10));
		float fSlow17 = (1.0f / faustpower2_f(fSlow10));
		float fSlow18 = (2.0f * (1.0f - fSlow17));
		float fSlow19 = powf(1e+01f, (0.05f * float(fHslider0)));
		int iSlow20 = int(float(fCheckbox0));
		float fSlow21 = expf((0.0f - (fConst2 / max(fConst2, (0.001f * float(fHslider1))))));
		float fSlow22 = (1.0f - fSlow21);
		float fSlow23 = max(fConst2, (0.001f * float(fHslider2)));
		float fSlow24 = expf((0.0f - (fConst2 / fSlow23)));
		float fSlow25 = (1.0f - fSlow24);
		float fSlow26 = expf((0.0f - (fConst3 / fSlow23)));
		float fSlow27 = ((1.0f - fSlow26) * ((1.0f / float(fHslider3)) - 1.0f));
		float fSlow28 = float(fHslider4);
		float fSlow29 = (1.0f / (fSlow10 * fSlow5));
		float fSlow30 = (0.0f - fSlow11);
		float fSlow31 = powf(1e+01f, (0.05f * float(fHslider5)));
		int iSlow32 = int(float(fCheckbox1));
		float fSlow33 = (2.0f * (0.0f - fSlow17));
		float fSlow34 = expf((0.0f - (fConst2 / max(fConst2, (0.001f * float(fHslider6))))));
		float fSlow35 = (1.0f - fSlow34);
		float fSlow36 = max(fConst2, (0.001f * float(fHslider7)));
		float fSlow37 = expf((0.0f - (fConst2 / fSlow36)));
		float fSlow38 = (1.0f - fSlow37);
		float fSlow39 = expf((0.0f - (fConst3 / fSlow36)));
		float fSlow40 = ((1.0f - fSlow39) * ((1.0f / float(fHslider8)) - 1.0f));
		float fSlow41 = float(fHslider9);
		float fSlow42 = (0.0f - fSlow1);
		float fSlow43 = (2.0f * (0.0f - fSlow8));
		float fSlow44 = (1.0f / (1.0f + ((1.0f + fSlow11) / fSlow10)));
		float fSlow45 = (1.0f + ((fSlow11 - 1.0f) / fSlow10));
		float fSlow46 = powf(1e+01f, (0.05f * float(fHslider10)));
		int iSlow47 = int(float(fCheckbox2));
		float fSlow48 = expf((0.0f - (fConst2 / max(fConst2, (0.001f * float(fHslider11))))));
		float fSlow49 = (1.0f - fSlow48);
		float fSlow50 = max(fConst2, (0.001f * float(fHslider12)));
		float fSlow51 = expf((0.0f - (fConst2 / fSlow50)));
		float fSlow52 = (1.0f - fSlow51);
		float fSlow53 = expf((0.0f - (fConst3 / fSlow50)));
		float fSlow54 = ((1.0f - fSlow53) * ((1.0f / float(fHslider13)) - 1.0f));
		float fSlow55 = float(fHslider14);
		float fSlow56 = float(pun_int_to_float(int((8388608.0f * (126.942696f + max(-126.0f, (0.1660964f * float(fHslider15))))))));
		int iSlow57 = int(float(fCheckbox3));
		float fSlow58 = powf(1e+01f, (0.05f * float(fHslider16)));
		int iSlow59 = int(float(fCheckbox4));
		int iSlow60 = int(float(fCheckbox5));
		int iSlow61 = int(float(fCheckbox6));
		int iSlow62 = (((iSlow59 == 0) & (iSlow60 == 0)) & (iSlow61 == 0));
		float fSlow63 = float((iSlow59 | iSlow62));
		float fSlow64 = ((fSlow19 * fSlow58) * fSlow63);
		float fSlow65 = powf(1e+01f, (0.05f * float(fHslider17)));
		float fSlow66 = float((iSlow60 | iSlow62));
		float fSlow67 = ((fSlow31 * fSlow65) * fSlow66);
		float fSlow68 = powf(1e+01f, (0.05f * float(fHslider18)));
		float fSlow69 = float((iSlow61 | iSlow62));
		float fSlow70 = ((fSlow46 * fSlow68) * fSlow69);
		float fSlow71 = (fSlow58 * fSlow63);
		float fSlow72 = (fSlow65 * fSlow66);
		float fSlow73 = (fSlow68 * fSlow69);
		float fSlow74 = expf((0.0f - (fConst4 / float(fVslider2))));
		float fSlow75 = (1.0f - fSlow74);
		float fSlow76 = float(fVslider3);
		float fSlow77 = expf((0.0f - (fConst5 / fSlow76)));
		float fSlow78 = (1.0f - fSlow77);
		float fSlow79 = expf((0.0f - (fConst6 / fSlow76)));
		float fSlow80 = ((1.0f - fSlow79) * ((1.0f / float(fVslider4)) - 1.0f));
		float fSlow81 = powf(1e+01f, (0.05f * float(fHslider19)));
		float fSlow82 = (fSlow56 * float(pun_int_to_float(int((8388608.0f * (126.942696f + max(-126.0f, (0.1660964f * float(fHslider20)))))))));
		int fullcount = count;
		int index = 0;
		/* Main loop */
		for (index = 0; (index <= (fullcount - 32)); index = (index + 32)) {
			fInput0 = &fInput0_ptr[index];
			fInput1 = &fInput1_ptr[index];
			fOutput0 = &fOutput0_ptr[index];
			fOutput1 = &fOutput1_ptr[index];
			int count = 32;
			/* Recursive loop 0 */
			/* Pre code */
			for (int j0 = 0; (j0 < 4); j0 = (j0 + 1)) {
				fYec0_tmp[j0] = fYec0_perm[j0];
				
			}
			/* Compute code */
			for (int i = 0; (i < count); i = (i + 1)) {
				fYec0[i] = float(fInput0[i]);
				
			}
			/* Post code */
			for (int j = 0; (j < 4); j = (j + 1)) {
				fYec0_perm[j] = fYec0_tmp[(count + j)];
				
			}
			/* Recursive loop 1 */
			/* Pre code */
			for (int j6 = 0; (j6 < 4); j6 = (j6 + 1)) {
				fYec2_tmp[j6] = fYec2_perm[j6];
				
			}
			/* Compute code */
			for (int i = 0; (i < count); i = (i + 1)) {
				fYec2[i] = float(fInput1[i]);
				
			}
			/* Post code */
			for (int j = 0; (j < 4); j = (j + 1)) {
				fYec2_perm[j] = fYec2_tmp[(count + j)];
				
			}
			/* Recursive loop 2 */
			/* Pre code */
			for (int j1 = 0; (j1 < 4); j1 = (j1 + 1)) {
				fRec3_tmp[j1] = fRec3_perm[j1];
				
			}
			/* Compute code */
			for (int i = 0; (i < count); i = (i + 1)) {
				fRec3[i] = ((fSlow3 * fRec3[(i - 1)]) + (fSlow4 * (fYec0[(i - 1)] + float(fInput0[i]))));
				
			}
			/* Post code */
			for (int j = 0; (j < 4); j = (j + 1)) {
				fRec3_perm[j] = fRec3_tmp[(count + j)];
				
			}
			/* Recursive loop 3 */
			/* Pre code */
			for (int j7 = 0; (j7 < 4); j7 = (j7 + 1)) {
				fRec10_tmp[j7] = fRec10_perm[j7];
				
			}
			/* Compute code */
			for (int i = 0; (i < count); i = (i + 1)) {
				fRec10[i] = ((fSlow3 * fRec10[(i - 1)]) + (fSlow4 * (fYec2[(i - 1)] + float(fInput1[i]))));
				
			}
			/* Post code */
			for (int j = 0; (j < 4); j = (j + 1)) {
				fRec10_perm[j] = fRec10_tmp[(count + j)];
				
			}
			/* Recursive loop 4 */
			/* Pre code */
			for (int j2 = 0; (j2 < 4); j2 = (j2 + 1)) {
				fRec2_tmp[j2] = fRec2_perm[j2];
				
			}
			/* Compute code */
			for (int i = 0; (i < count); i = (i + 1)) {
				fRec2[i] = (fRec3[i] - (fSlow6 * ((fSlow7 * fRec2[(i - 2)]) + (fSlow9 * fRec2[(i - 1)]))));
				
			}
			/* Post code */
			for (int j = 0; (j < 4); j = (j + 1)) {
				fRec2_perm[j] = fRec2_tmp[(count + j)];
				
			}
			/* Recursive loop 5 */
			/* Pre code */
			for (int j8 = 0; (j8 < 4); j8 = (j8 + 1)) {
				fRec9_tmp[j8] = fRec9_perm[j8];
				
			}
			/* Compute code */
			for (int i = 0; (i < count); i = (i + 1)) {
				fRec9[i] = (fRec10[i] - (fSlow6 * ((fSlow7 * fRec9[(i - 2)]) + (fSlow9 * fRec9[(i - 1)]))));
				
			}
			/* Post code */
			for (int j = 0; (j < 4); j = (j + 1)) {
				fRec9_perm[j] = fRec9_tmp[(count + j)];
				
			}
			/* Recursive loop 6 */
			/* Compute code */
			for (int i = 0; (i < count); i = (i + 1)) {
				fZec0[i] = (fRec2[(i - 2)] + (fRec2[i] + (2.0f * fRec2[(i - 1)])));
				
			}
			/* Recursive loop 7 */
			/* Compute code */
			for (int i = 0; (i < count); i = (i + 1)) {
				fZec1[i] = (fRec9[(i - 2)] + (fRec9[i] + (2.0f * fRec9[(i - 1)])));
				
			}
			/* Recursive loop 8 */
			/* Pre code */
			for (int j3 = 0; (j3 < 4); j3 = (j3 + 1)) {
				fYec1_tmp[j3] = fYec1_perm[j3];
				
			}
			/* Compute code */
			for (int i = 0; (i < count); i = (i + 1)) {
				fYec1[i] = (fSlow6 * fZec0[i]);
				
			}
			/* Post code */
			for (int j = 0; (j < 4); j = (j + 1)) {
				fYec1_perm[j] = fYec1_tmp[(count + j)];
				
			}
			/* Recursive loop 9 */
			/* Pre code */
			for (int j9 = 0; (j9 < 4); j9 = (j9 + 1)) {
				fYec3_tmp[j9] = fYec3_perm[j9];
				
			}
			/* Compute code */
			for (int i = 0; (i < count); i = (i + 1)) {
				fYec3[i] = (fSlow6 * fZec1[i]);
				
			}
			/* Post code */
			for (int j = 0; (j < 4); j = (j + 1)) {
				fYec3_perm[j] = fYec3_tmp[(count + j)];
				
			}
			/* Recursive loop 10 */
			/* Pre code */
			for (int j22 = 0; (j22 < 4); j22 = (j22 + 1)) {
				fRec20_tmp[j22] = fRec20_perm[j22];
				
			}
			/* Compute code */
			for (int i = 0; (i < count); i = (i + 1)) {
				fRec20[i] = ((fSlow3 * fRec20[(i - 1)]) + (fSlow4 * ((fSlow1 * float(fInput0[i])) + (fSlow42 * fYec0[(i - 1)]))));
				
			}
			/* Post code */
			for (int j = 0; (j < 4); j = (j + 1)) {
				fRec20_perm[j] = fRec20_tmp[(count + j)];
				
			}
			/* Recursive loop 11 */
			/* Pre code */
			for (int j25 = 0; (j25 < 4); j25 = (j25 + 1)) {
				fRec26_tmp[j25] = fRec26_perm[j25];
				
			}
			/* Compute code */
			for (int i = 0; (i < count); i = (i + 1)) {
				fRec26[i] = ((fSlow3 * fRec26[(i - 1)]) + (fSlow4 * ((fSlow1 * float(fInput1[i])) + (fSlow42 * fYec2[(i - 1)]))));
				
			}
			/* Post code */
			for (int j = 0; (j < 4); j = (j + 1)) {
				fRec26_perm[j] = fRec26_tmp[(count + j)];
				
			}
			/* Recursive loop 12 */
			/* Pre code */
			for (int j4 = 0; (j4 < 4); j4 = (j4 + 1)) {
				fRec1_tmp[j4] = fRec1_perm[j4];
				
			}
			/* Compute code */
			for (int i = 0; (i < count); i = (i + 1)) {
				fRec1[i] = ((fSlow13 * fRec1[(i - 1)]) + (fSlow14 * (fYec1[i] + fYec1[(i - 1)])));
				
			}
			/* Post code */
			for (int j = 0; (j < 4); j = (j + 1)) {
				fRec1_perm[j] = fRec1_tmp[(count + j)];
				
			}
			/* Recursive loop 13 */
			/* Pre code */
			for (int j10 = 0; (j10 < 4); j10 = (j10 + 1)) {
				fRec8_tmp[j10] = fRec8_perm[j10];
				
			}
			/* Compute code */
			for (int i = 0; (i < count); i = (i + 1)) {
				fRec8[i] = ((fSlow13 * fRec8[(i - 1)]) + (fSlow14 * (fYec3[i] + fYec3[(i - 1)])));
				
			}
			/* Post code */
			for (int j = 0; (j < 4); j = (j + 1)) {
				fRec8_perm[j] = fRec8_tmp[(count + j)];
				
			}
			/* Recursive loop 14 */
			/* Pre code */
			for (int j15 = 0; (j15 < 4); j15 = (j15 + 1)) {
				fRec12_tmp[j15] = fRec12_perm[j15];
				
			}
			/* Compute code */
			for (int i = 0; (i < count); i = (i + 1)) {
				fRec12[i] = ((fSlow13 * fRec12[(i - 1)]) + (fSlow14 * ((fSlow29 * fZec0[i]) + (fSlow30 * fYec1[(i - 1)]))));
				
			}
			/* Post code */
			for (int j = 0; (j < 4); j = (j + 1)) {
				fRec12_perm[j] = fRec12_tmp[(count + j)];
				
			}
			/* Recursive loop 15 */
			/* Pre code */
			for (int j17 = 0; (j17 < 4); j17 = (j17 + 1)) {
				fRec17_tmp[j17] = fRec17_perm[j17];
				
			}
			/* Compute code */
			for (int i = 0; (i < count); i = (i + 1)) {
				fRec17[i] = ((fSlow13 * fRec17[(i - 1)]) + (fSlow14 * ((fSlow29 * fZec1[i]) + (fSlow30 * fYec3[(i - 1)]))));
				
			}
			/* Post code */
			for (int j = 0; (j < 4); j = (j + 1)) {
				fRec17_perm[j] = fRec17_tmp[(count + j)];
				
			}
			/* Recursive loop 16 */
			/* Pre code */
			for (int j23 = 0; (j23 < 4); j23 = (j23 + 1)) {
				fRec19_tmp[j23] = fRec19_perm[j23];
				
			}
			/* Compute code */
			for (int i = 0; (i < count); i = (i + 1)) {
				fRec19[i] = (fRec20[i] - (fSlow6 * ((fSlow7 * fRec19[(i - 2)]) + (fSlow9 * fRec19[(i - 1)]))));
				
			}
			/* Post code */
			for (int j = 0; (j < 4); j = (j + 1)) {
				fRec19_perm[j] = fRec19_tmp[(count + j)];
				
			}
			/* Recursive loop 17 */
			/* Pre code */
			for (int j26 = 0; (j26 < 4); j26 = (j26 + 1)) {
				fRec25_tmp[j26] = fRec25_perm[j26];
				
			}
			/* Compute code */
			for (int i = 0; (i < count); i = (i + 1)) {
				fRec25[i] = (fRec26[i] - (fSlow6 * ((fSlow7 * fRec25[(i - 2)]) + (fSlow9 * fRec25[(i - 1)]))));
				
			}
			/* Post code */
			for (int j = 0; (j < 4); j = (j + 1)) {
				fRec25_perm[j] = fRec25_tmp[(count + j)];
				
			}
			/* Recursive loop 18 */
			/* Pre code */
			for (int j5 = 0; (j5 < 4); j5 = (j5 + 1)) {
				fRec0_tmp[j5] = fRec0_perm[j5];
				
			}
			/* Compute code */
			for (int i = 0; (i < count); i = (i + 1)) {
				fRec0[i] = (fRec1[i] - (fSlow15 * ((fSlow16 * fRec0[(i - 2)]) + (fSlow18 * fRec0[(i - 1)]))));
				
			}
			/* Post code */
			for (int j = 0; (j < 4); j = (j + 1)) {
				fRec0_perm[j] = fRec0_tmp[(count + j)];
				
			}
			/* Recursive loop 19 */
			/* Pre code */
			for (int j11 = 0; (j11 < 4); j11 = (j11 + 1)) {
				fRec7_tmp[j11] = fRec7_perm[j11];
				
			}
			/* Compute code */
			for (int i = 0; (i < count); i = (i + 1)) {
				fRec7[i] = (fRec8[i] - (fSlow15 * ((fSlow16 * fRec7[(i - 2)]) + (fSlow18 * fRec7[(i - 1)]))));
				
			}
			/* Post code */
			for (int j = 0; (j < 4); j = (j + 1)) {
				fRec7_perm[j] = fRec7_tmp[(count + j)];
				
			}
			/* Recursive loop 20 */
			/* Pre code */
			for (int j16 = 0; (j16 < 4); j16 = (j16 + 1)) {
				fRec11_tmp[j16] = fRec11_perm[j16];
				
			}
			/* Compute code */
			for (int i = 0; (i < count); i = (i + 1)) {
				fRec11[i] = (fRec12[i] - (fSlow15 * ((fSlow16 * fRec11[(i - 2)]) + (fSlow18 * fRec11[(i - 1)]))));
				
			}
			/* Post code */
			for (int j = 0; (j < 4); j = (j + 1)) {
				fRec11_perm[j] = fRec11_tmp[(count + j)];
				
			}
			/* Recursive loop 21 */
			/* Pre code */
			for (int j18 = 0; (j18 < 4); j18 = (j18 + 1)) {
				fRec16_tmp[j18] = fRec16_perm[j18];
				
			}
			/* Compute code */
			for (int i = 0; (i < count); i = (i + 1)) {
				fRec16[i] = (fRec17[i] - (fSlow15 * ((fSlow16 * fRec16[(i - 2)]) + (fSlow18 * fRec16[(i - 1)]))));
				
			}
			/* Post code */
			for (int j = 0; (j < 4); j = (j + 1)) {
				fRec16_perm[j] = fRec16_tmp[(count + j)];
				
			}
			/* Recursive loop 22 */
			/* Pre code */
			for (int j24 = 0; (j24 < 4); j24 = (j24 + 1)) {
				fRec18_tmp[j24] = fRec18_perm[j24];
				
			}
			/* Compute code */
			for (int i = 0; (i < count); i = (i + 1)) {
				fZec12[i] = (fSlow18 * fRec18[(i - 1)]);
				fRec18[i] = ((fSlow6 * (((fSlow8 * fRec19[i]) + (fSlow43 * fRec19[(i - 1)])) + (fSlow8 * fRec19[(i - 2)]))) - (fSlow44 * ((fSlow45 * fRec18[(i - 2)]) + fZec12[i])));
				
			}
			/* Post code */
			for (int j = 0; (j < 4); j = (j + 1)) {
				fRec18_perm[j] = fRec18_tmp[(count + j)];
				
			}
			/* Recursive loop 23 */
			/* Pre code */
			for (int j27 = 0; (j27 < 4); j27 = (j27 + 1)) {
				fRec24_tmp[j27] = fRec24_perm[j27];
				
			}
			/* Compute code */
			for (int i = 0; (i < count); i = (i + 1)) {
				fZec13[i] = (fSlow18 * fRec24[(i - 1)]);
				fRec24[i] = ((fSlow6 * (((fSlow8 * fRec25[i]) + (fSlow43 * fRec25[(i - 1)])) + (fSlow8 * fRec25[(i - 2)]))) - (fSlow44 * ((fSlow45 * fRec24[(i - 2)]) + fZec13[i])));
				
			}
			/* Post code */
			for (int j = 0; (j < 4); j = (j + 1)) {
				fRec24_perm[j] = fRec24_tmp[(count + j)];
				
			}
			/* Recursive loop 24 */
			/* Compute code */
			for (int i = 0; (i < count); i = (i + 1)) {
				fZec2[i] = (fSlow15 * (fRec0[(i - 2)] + (fRec0[i] + (2.0f * fRec0[(i - 1)]))));
				
			}
			/* Recursive loop 25 */
			/* Compute code */
			for (int i = 0; (i < count); i = (i + 1)) {
				fZec4[i] = (fSlow15 * (fRec7[(i - 2)] + (fRec7[i] + (2.0f * fRec7[(i - 1)]))));
				
			}
			/* Recursive loop 26 */
			/* Compute code */
			for (int i = 0; (i < count); i = (i + 1)) {
				fZec7[i] = (fSlow15 * (((fSlow17 * fRec11[i]) + (fSlow33 * fRec11[(i - 1)])) + (fSlow17 * fRec11[(i - 2)])));
				
			}
			/* Recursive loop 27 */
			/* Compute code */
			for (int i = 0; (i < count); i = (i + 1)) {
				fZec9[i] = (fSlow15 * (((fSlow17 * fRec16[i]) + (fSlow33 * fRec16[(i - 1)])) + (fSlow17 * fRec16[(i - 2)])));
				
			}
			/* Recursive loop 28 */
			/* Compute code */
			for (int i = 0; (i < count); i = (i + 1)) {
				fZec14[i] = (fRec18[(i - 2)] + (fSlow44 * (fZec12[i] + (fSlow45 * fRec18[i]))));
				
			}
			/* Recursive loop 29 */
			/* Compute code */
			for (int i = 0; (i < count); i = (i + 1)) {
				fZec16[i] = (fRec24[(i - 2)] + (fSlow44 * (fZec13[i] + (fSlow45 * fRec24[i]))));
				
			}
			/* Recursive loop 30 */
			/* Compute code */
			for (int i = 0; (i < count); i = (i + 1)) {
				fZec3[i] = (iSlow20?0.0f:fZec2[i]);
				
			}
			/* Recursive loop 31 */
			/* Compute code */
			for (int i = 0; (i < count); i = (i + 1)) {
				fZec5[i] = (iSlow20?0.0f:fZec4[i]);
				
			}
			/* Recursive loop 32 */
			/* Compute code */
			for (int i = 0; (i < count); i = (i + 1)) {
				fZec8[i] = (iSlow32?0.0f:fZec7[i]);
				
			}
			/* Recursive loop 33 */
			/* Compute code */
			for (int i = 0; (i < count); i = (i + 1)) {
				fZec10[i] = (iSlow32?0.0f:fZec9[i]);
				
			}
			/* Recursive loop 34 */
			/* Compute code */
			for (int i = 0; (i < count); i = (i + 1)) {
				fZec15[i] = (iSlow47?0.0f:fZec14[i]);
				
			}
			/* Recursive loop 35 */
			/* Compute code */
			for (int i = 0; (i < count); i = (i + 1)) {
				fZec17[i] = (iSlow47?0.0f:fZec16[i]);
				
			}
			/* Recursive loop 36 */
			/* Compute code */
			for (int i = 0; (i < count); i = (i + 1)) {
				fZec6[i] = fabsf((fabsf((fSlow19 * fZec3[i])) + fabsf((fSlow19 * fZec5[i]))));
				
			}
			/* Recursive loop 37 */
			/* Compute code */
			for (int i = 0; (i < count); i = (i + 1)) {
				fZec11[i] = fabsf((fabsf((fSlow31 * fZec8[i])) + fabsf((fSlow31 * fZec10[i]))));
				
			}
			/* Recursive loop 38 */
			/* Compute code */
			for (int i = 0; (i < count); i = (i + 1)) {
				fZec18[i] = fabsf((fabsf((fSlow46 * fZec15[i])) + fabsf((fSlow46 * fZec17[i]))));
				
			}
			/* Recursive loop 39 */
			/* Pre code */
			for (int j12 = 0; (j12 < 4); j12 = (j12 + 1)) {
				fRec6_tmp[j12] = fRec6_perm[j12];
				
			}
			/* Compute code */
			for (int i = 0; (i < count); i = (i + 1)) {
				fRec6[i] = max(fZec6[i], ((fSlow21 * fRec6[(i - 1)]) + (fSlow22 * fZec6[i])));
				
			}
			/* Post code */
			for (int j = 0; (j < 4); j = (j + 1)) {
				fRec6_perm[j] = fRec6_tmp[(count + j)];
				
			}
			/* Recursive loop 40 */
			/* Pre code */
			for (int j19 = 0; (j19 < 4); j19 = (j19 + 1)) {
				fRec15_tmp[j19] = fRec15_perm[j19];
				
			}
			/* Compute code */
			for (int i = 0; (i < count); i = (i + 1)) {
				fRec15[i] = max(fZec11[i], ((fSlow34 * fRec15[(i - 1)]) + (fSlow35 * fZec11[i])));
				
			}
			/* Post code */
			for (int j = 0; (j < 4); j = (j + 1)) {
				fRec15_perm[j] = fRec15_tmp[(count + j)];
				
			}
			/* Recursive loop 41 */
			/* Pre code */
			for (int j28 = 0; (j28 < 4); j28 = (j28 + 1)) {
				fRec23_tmp[j28] = fRec23_perm[j28];
				
			}
			/* Compute code */
			for (int i = 0; (i < count); i = (i + 1)) {
				fRec23[i] = max(fZec18[i], ((fSlow48 * fRec23[(i - 1)]) + (fSlow49 * fZec18[i])));
				
			}
			/* Post code */
			for (int j = 0; (j < 4); j = (j + 1)) {
				fRec23_perm[j] = fRec23_tmp[(count + j)];
				
			}
			/* Recursive loop 42 */
			/* Pre code */
			for (int j13 = 0; (j13 < 4); j13 = (j13 + 1)) {
				fRec5_tmp[j13] = fRec5_perm[j13];
				
			}
			/* Compute code */
			for (int i = 0; (i < count); i = (i + 1)) {
				fRec5[i] = ((fSlow24 * fRec5[(i - 1)]) + (fSlow25 * fRec6[i]));
				
			}
			/* Post code */
			for (int j = 0; (j < 4); j = (j + 1)) {
				fRec5_perm[j] = fRec5_tmp[(count + j)];
				
			}
			/* Recursive loop 43 */
			/* Pre code */
			for (int j20 = 0; (j20 < 4); j20 = (j20 + 1)) {
				fRec14_tmp[j20] = fRec14_perm[j20];
				
			}
			/* Compute code */
			for (int i = 0; (i < count); i = (i + 1)) {
				fRec14[i] = ((fSlow37 * fRec14[(i - 1)]) + (fSlow38 * fRec15[i]));
				
			}
			/* Post code */
			for (int j = 0; (j < 4); j = (j + 1)) {
				fRec14_perm[j] = fRec14_tmp[(count + j)];
				
			}
			/* Recursive loop 44 */
			/* Pre code */
			for (int j29 = 0; (j29 < 4); j29 = (j29 + 1)) {
				fRec22_tmp[j29] = fRec22_perm[j29];
				
			}
			/* Compute code */
			for (int i = 0; (i < count); i = (i + 1)) {
				fRec22[i] = ((fSlow51 * fRec22[(i - 1)]) + (fSlow52 * fRec23[i]));
				
			}
			/* Post code */
			for (int j = 0; (j < 4); j = (j + 1)) {
				fRec22_perm[j] = fRec22_tmp[(count + j)];
				
			}
			/* Recursive loop 45 */
			/* Pre code */
			for (int j14 = 0; (j14 < 4); j14 = (j14 + 1)) {
				fRec4_tmp[j14] = fRec4_perm[j14];
				
			}
			/* Compute code */
			for (int i = 0; (i < count); i = (i + 1)) {
				fRec4[i] = ((fSlow26 * fRec4[(i - 1)]) + (fSlow27 * max(((8.685889f * ((8.262958e-08f * float(int(pun_float_to_int(float(fRec5[i]))))) - 87.98997f)) - fSlow28), 0.0f)));
				
			}
			/* Post code */
			for (int j = 0; (j < 4); j = (j + 1)) {
				fRec4_perm[j] = fRec4_tmp[(count + j)];
				
			}
			/* Recursive loop 46 */
			/* Pre code */
			for (int j21 = 0; (j21 < 4); j21 = (j21 + 1)) {
				fRec13_tmp[j21] = fRec13_perm[j21];
				
			}
			/* Compute code */
			for (int i = 0; (i < count); i = (i + 1)) {
				fRec13[i] = ((fSlow39 * fRec13[(i - 1)]) + (fSlow40 * max(((8.685889f * ((8.262958e-08f * float(int(pun_float_to_int(float(fRec14[i]))))) - 87.98997f)) - fSlow41), 0.0f)));
				
			}
			/* Post code */
			for (int j = 0; (j < 4); j = (j + 1)) {
				fRec13_perm[j] = fRec13_tmp[(count + j)];
				
			}
			/* Recursive loop 47 */
			/* Pre code */
			for (int j30 = 0; (j30 < 4); j30 = (j30 + 1)) {
				fRec21_tmp[j30] = fRec21_perm[j30];
				
			}
			/* Compute code */
			for (int i = 0; (i < count); i = (i + 1)) {
				fRec21[i] = ((fSlow53 * fRec21[(i - 1)]) + (fSlow54 * max(((8.685889f * ((8.262958e-08f * float(int(pun_float_to_int(float(fRec22[i]))))) - 87.98997f)) - fSlow55), 0.0f)));
				
			}
			/* Post code */
			for (int j = 0; (j < 4); j = (j + 1)) {
				fRec21_perm[j] = fRec21_tmp[(count + j)];
				
			}
			/* Recursive loop 48 */
			/* Compute code */
			for (int i = 0; (i < count); i = (i + 1)) {
				fZec19[i] = float(pun_int_to_float(int((8388608.0f * (126.942696f + max(-126.0f, (0.1660964f * fRec4[i])))))));
				
			}
			/* Recursive loop 49 */
			/* Compute code */
			for (int i = 0; (i < count); i = (i + 1)) {
				fZec23[i] = float(pun_int_to_float(int((8388608.0f * (126.942696f + max(-126.0f, (0.1660964f * fRec13[i])))))));
				
			}
			/* Recursive loop 50 */
			/* Compute code */
			for (int i = 0; (i < count); i = (i + 1)) {
				fZec27[i] = float(pun_int_to_float(int((8388608.0f * (126.942696f + max(-126.0f, (0.1660964f * fRec21[i])))))));
				
			}
			/* Recursive loop 51 */
			/* Compute code */
			for (int i = 0; (i < count); i = (i + 1)) {
				fZec20[i] = (fZec3[i] * fZec19[i]);
				
			}
			/* Recursive loop 52 */
			/* Compute code */
			for (int i = 0; (i < count); i = (i + 1)) {
				fZec21[i] = (fSlow19 * (fZec5[i] * fZec19[i]));
				
			}
			/* Recursive loop 53 */
			/* Compute code */
			for (int i = 0; (i < count); i = (i + 1)) {
				fZec24[i] = (fZec8[i] * fZec23[i]);
				
			}
			/* Recursive loop 54 */
			/* Compute code */
			for (int i = 0; (i < count); i = (i + 1)) {
				fZec25[i] = (fSlow31 * (fZec10[i] * fZec23[i]));
				
			}
			/* Recursive loop 55 */
			/* Compute code */
			for (int i = 0; (i < count); i = (i + 1)) {
				fZec28[i] = (fZec15[i] * fZec27[i]);
				
			}
			/* Recursive loop 56 */
			/* Compute code */
			for (int i = 0; (i < count); i = (i + 1)) {
				fZec29[i] = (fSlow46 * (fZec17[i] * fZec27[i]));
				
			}
			/* Recursive loop 57 */
			/* Compute code */
			for (int i = 0; (i < count); i = (i + 1)) {
				fZec22[i] = fabsf((fabsf((fSlow19 * fZec20[i])) + fabsf(fZec21[i])));
				
			}
			/* Recursive loop 58 */
			/* Compute code */
			for (int i = 0; (i < count); i = (i + 1)) {
				fZec26[i] = fabsf((fabsf((fSlow31 * fZec24[i])) + fabsf(fZec25[i])));
				
			}
			/* Recursive loop 59 */
			/* Compute code */
			for (int i = 0; (i < count); i = (i + 1)) {
				fZec30[i] = fabsf((fabsf((fSlow46 * fZec28[i])) + fabsf(fZec29[i])));
				
			}
			/* Recursive loop 60 */
			/* Pre code */
			for (int j31 = 0; (j31 < 4); j31 = (j31 + 1)) {
				fRec32_tmp[j31] = fRec32_perm[j31];
				
			}
			/* Compute code */
			for (int i = 0; (i < count); i = (i + 1)) {
				fRec32[i] = max(fZec22[i], ((fSlow21 * fRec32[(i - 1)]) + (fSlow22 * fZec22[i])));
				
			}
			/* Post code */
			for (int j = 0; (j < 4); j = (j + 1)) {
				fRec32_perm[j] = fRec32_tmp[(count + j)];
				
			}
			/* Recursive loop 61 */
			/* Pre code */
			for (int j34 = 0; (j34 < 4); j34 = (j34 + 1)) {
				fRec35_tmp[j34] = fRec35_perm[j34];
				
			}
			/* Compute code */
			for (int i = 0; (i < count); i = (i + 1)) {
				fRec35[i] = max(fZec26[i], ((fSlow34 * fRec35[(i - 1)]) + (fSlow35 * fZec26[i])));
				
			}
			/* Post code */
			for (int j = 0; (j < 4); j = (j + 1)) {
				fRec35_perm[j] = fRec35_tmp[(count + j)];
				
			}
			/* Recursive loop 62 */
			/* Pre code */
			for (int j37 = 0; (j37 < 4); j37 = (j37 + 1)) {
				fRec38_tmp[j37] = fRec38_perm[j37];
				
			}
			/* Compute code */
			for (int i = 0; (i < count); i = (i + 1)) {
				fRec38[i] = max(fZec30[i], ((fSlow48 * fRec38[(i - 1)]) + (fSlow49 * fZec30[i])));
				
			}
			/* Post code */
			for (int j = 0; (j < 4); j = (j + 1)) {
				fRec38_perm[j] = fRec38_tmp[(count + j)];
				
			}
			/* Recursive loop 63 */
			/* Pre code */
			for (int j32 = 0; (j32 < 4); j32 = (j32 + 1)) {
				fRec31_tmp[j32] = fRec31_perm[j32];
				
			}
			/* Compute code */
			for (int i = 0; (i < count); i = (i + 1)) {
				fRec31[i] = ((fSlow24 * fRec31[(i - 1)]) + (fSlow25 * fRec32[i]));
				
			}
			/* Post code */
			for (int j = 0; (j < 4); j = (j + 1)) {
				fRec31_perm[j] = fRec31_tmp[(count + j)];
				
			}
			/* Recursive loop 64 */
			/* Pre code */
			for (int j35 = 0; (j35 < 4); j35 = (j35 + 1)) {
				fRec34_tmp[j35] = fRec34_perm[j35];
				
			}
			/* Compute code */
			for (int i = 0; (i < count); i = (i + 1)) {
				fRec34[i] = ((fSlow37 * fRec34[(i - 1)]) + (fSlow38 * fRec35[i]));
				
			}
			/* Post code */
			for (int j = 0; (j < 4); j = (j + 1)) {
				fRec34_perm[j] = fRec34_tmp[(count + j)];
				
			}
			/* Recursive loop 65 */
			/* Pre code */
			for (int j38 = 0; (j38 < 4); j38 = (j38 + 1)) {
				fRec37_tmp[j38] = fRec37_perm[j38];
				
			}
			/* Compute code */
			for (int i = 0; (i < count); i = (i + 1)) {
				fRec37[i] = ((fSlow51 * fRec37[(i - 1)]) + (fSlow52 * fRec38[i]));
				
			}
			/* Post code */
			for (int j = 0; (j < 4); j = (j + 1)) {
				fRec37_perm[j] = fRec37_tmp[(count + j)];
				
			}
			/* Recursive loop 66 */
			/* Pre code */
			for (int j33 = 0; (j33 < 4); j33 = (j33 + 1)) {
				fRec30_tmp[j33] = fRec30_perm[j33];
				
			}
			/* Compute code */
			for (int i = 0; (i < count); i = (i + 1)) {
				fRec30[i] = ((fSlow26 * fRec30[(i - 1)]) + (fSlow27 * max(((8.685889f * ((8.262958e-08f * float(int(pun_float_to_int(float(fRec31[i]))))) - 87.98997f)) - fSlow28), 0.0f)));
				
			}
			/* Post code */
			for (int j = 0; (j < 4); j = (j + 1)) {
				fRec30_perm[j] = fRec30_tmp[(count + j)];
				
			}
			/* Recursive loop 67 */
			/* Pre code */
			for (int j36 = 0; (j36 < 4); j36 = (j36 + 1)) {
				fRec33_tmp[j36] = fRec33_perm[j36];
				
			}
			/* Compute code */
			for (int i = 0; (i < count); i = (i + 1)) {
				fRec33[i] = ((fSlow39 * fRec33[(i - 1)]) + (fSlow40 * max(((8.685889f * ((8.262958e-08f * float(int(pun_float_to_int(float(fRec34[i]))))) - 87.98997f)) - fSlow41), 0.0f)));
				
			}
			/* Post code */
			for (int j = 0; (j < 4); j = (j + 1)) {
				fRec33_perm[j] = fRec33_tmp[(count + j)];
				
			}
			/* Recursive loop 68 */
			/* Pre code */
			for (int j39 = 0; (j39 < 4); j39 = (j39 + 1)) {
				fRec36_tmp[j39] = fRec36_perm[j39];
				
			}
			/* Compute code */
			for (int i = 0; (i < count); i = (i + 1)) {
				fRec36[i] = ((fSlow53 * fRec36[(i - 1)]) + (fSlow54 * max(((8.685889f * ((8.262958e-08f * float(int(pun_float_to_int(float(fRec37[i]))))) - 87.98997f)) - fSlow55), 0.0f)));
				
			}
			/* Post code */
			for (int j = 0; (j < 4); j = (j + 1)) {
				fRec36_perm[j] = fRec36_tmp[(count + j)];
				
			}
			/* Recursive loop 69 */
			/* Compute code */
			for (int i = 0; (i < count); i = (i + 1)) {
				fZec31[i] = (fSlow64 * fZec20[i]);
				
			}
			/* Recursive loop 70 */
			/* Compute code */
			for (int i = 0; (i < count); i = (i + 1)) {
				fZec32[i] = (fSlow67 * fZec24[i]);
				
			}
			/* Recursive loop 71 */
			/* Compute code */
			for (int i = 0; (i < count); i = (i + 1)) {
				fZec33[i] = (fSlow70 * fZec28[i]);
				
			}
			/* Recursive loop 72 */
			/* Compute code */
			for (int i = 0; (i < count); i = (i + 1)) {
				fHbargraph0 = FAUSTFLOAT((0.5f * float(pun_int_to_float(int((8388608.0f * (126.942696f + max(-126.0f, (0.1660964f * fRec30[i])))))))));
				fZec36[i] = (fSlow71 * fZec21[i]);
				
			}
			/* Recursive loop 73 */
			/* Compute code */
			for (int i = 0; (i < count); i = (i + 1)) {
				fHbargraph2 = FAUSTFLOAT((0.5f * float(pun_int_to_float(int((8388608.0f * (126.942696f + max(-126.0f, (0.1660964f * fRec33[i])))))))));
				fZec37[i] = (fSlow72 * fZec25[i]);
				
			}
			/* Recursive loop 74 */
			/* Compute code */
			for (int i = 0; (i < count); i = (i + 1)) {
				fHbargraph4 = FAUSTFLOAT((0.5f * float(pun_int_to_float(int((8388608.0f * (126.942696f + max(-126.0f, (0.1660964f * fRec36[i])))))))));
				fZec38[i] = (fSlow73 * fZec29[i]);
				
			}
			/* Recursive loop 75 */
			/* Compute code */
			for (int i = 0; (i < count); i = (i + 1)) {
				fZec34[i] = (((iSlow20?fZec2[i]:fZec31[i]) + (iSlow32?fZec7[i]:fZec32[i])) + (iSlow47?fZec14[i]:fZec33[i]));
				
			}
			/* Recursive loop 76 */
			/* Compute code */
			for (int i = 0; (i < count); i = (i + 1)) {
				fHbargraph1 = FAUSTFLOAT((fabsf(fZec31[i]) + fabsf(fZec36[i])));
				fHbargraph3 = FAUSTFLOAT((fabsf(fZec32[i]) + fabsf(fZec37[i])));
				fHbargraph5 = FAUSTFLOAT((fabsf(fZec33[i]) + fabsf(fZec38[i])));
				fZec39[i] = (((iSlow20?fZec4[i]:fZec36[i]) + (iSlow32?fZec9[i]:fZec37[i])) + (iSlow47?fZec16[i]:fZec38[i]));
				
			}
			/* Recursive loop 77 */
			/* Compute code */
			for (int i = 0; (i < count); i = (i + 1)) {
				fZec35[i] = (iSlow57?0.0f:fZec34[i]);
				
			}
			/* Recursive loop 78 */
			/* Compute code */
			for (int i = 0; (i < count); i = (i + 1)) {
				fZec40[i] = (iSlow57?0.0f:fZec39[i]);
				
			}
			/* Recursive loop 79 */
			/* Compute code */
			for (int i = 0; (i < count); i = (i + 1)) {
				fZec41[i] = fabsf((fabsf((fSlow56 * fZec35[i])) + fabsf((fSlow56 * fZec40[i]))));
				
			}
			/* Recursive loop 80 */
			/* Pre code */
			for (int j40 = 0; (j40 < 4); j40 = (j40 + 1)) {
				fRec29_tmp[j40] = fRec29_perm[j40];
				
			}
			/* Compute code */
			for (int i = 0; (i < count); i = (i + 1)) {
				fRec29[i] = max(fZec41[i], ((fSlow74 * fRec29[(i - 1)]) + (fSlow75 * fZec41[i])));
				
			}
			/* Post code */
			for (int j = 0; (j < 4); j = (j + 1)) {
				fRec29_perm[j] = fRec29_tmp[(count + j)];
				
			}
			/* Recursive loop 81 */
			/* Pre code */
			for (int j41 = 0; (j41 < 4); j41 = (j41 + 1)) {
				fRec28_tmp[j41] = fRec28_perm[j41];
				
			}
			/* Compute code */
			for (int i = 0; (i < count); i = (i + 1)) {
				fRec28[i] = ((fSlow77 * fRec28[(i - 1)]) + (fSlow78 * fRec29[i]));
				
			}
			/* Post code */
			for (int j = 0; (j < 4); j = (j + 1)) {
				fRec28_perm[j] = fRec28_tmp[(count + j)];
				
			}
			/* Recursive loop 82 */
			/* Pre code */
			for (int j42 = 0; (j42 < 4); j42 = (j42 + 1)) {
				fRec27_tmp[j42] = fRec27_perm[j42];
				
			}
			/* Compute code */
			for (int i = 0; (i < count); i = (i + 1)) {
				fRec27[i] = ((fSlow79 * fRec27[(i - 1)]) + (fSlow80 * max((6.0f + (8.685889f * ((8.262958e-08f * float(int(pun_float_to_int(float(fRec28[i]))))) - 87.98997f))), 0.0f)));
				
			}
			/* Post code */
			for (int j = 0; (j < 4); j = (j + 1)) {
				fRec27_perm[j] = fRec27_tmp[(count + j)];
				
			}
			/* Recursive loop 83 */
			/* Compute code */
			for (int i = 0; (i < count); i = (i + 1)) {
				fZec42[i] = float(pun_int_to_float(int((8388608.0f * (126.942696f + max(-126.0f, (0.1660964f * fRec27[i])))))));
				
			}
			/* Vectorizable loop 84 */
			/* Compute code */
			for (int i = 0; (i < count); i = (i + 1)) {
				float fSel0 = 0.0f;
				if (iSlow57 != 0) {
					fSel0 = fZec34[i];
					
				} else {
					fSel0 = (fSlow82 * (fZec35[i] * fZec42[i]));
					
				}
				fOutput0[i] = FAUSTFLOAT((fSlow81 * fSel0));
				
			}
			/* Vectorizable loop 85 */
			/* Compute code */
			for (int i = 0; (i < count); i = (i + 1)) {
				float fSel1 = 0.0f;
				if (iSlow57 != 0) {
					fSel1 = fZec39[i];
					
				} else {
					fSel1 = (fSlow82 * (fZec40[i] * fZec42[i]));
					
				}
				fOutput1[i] = FAUSTFLOAT((fSlow81 * fSel1));
				
			}
			
		}
		/* Remaining frames */
		if (index < fullcount) {
			fInput0 = &fInput0_ptr[index];
			fInput1 = &fInput1_ptr[index];
			fOutput0 = &fOutput0_ptr[index];
			fOutput1 = &fOutput1_ptr[index];
			int count = (fullcount - index);
			/* Recursive loop 0 */
			/* Pre code */
			for (int j0 = 0; (j0 < 4); j0 = (j0 + 1)) {
				fYec0_tmp[j0] = fYec0_perm[j0];
				
			}
			/* Compute code */
			for (int i = 0; (i < count); i = (i + 1)) {
				fYec0[i] = float(fInput0[i]);
				
			}
			/* Post code */
			for (int j = 0; (j < 4); j = (j + 1)) {
				fYec0_perm[j] = fYec0_tmp[(count + j)];
				
			}
			/* Recursive loop 1 */
			/* Pre code */
			for (int j6 = 0; (j6 < 4); j6 = (j6 + 1)) {
				fYec2_tmp[j6] = fYec2_perm[j6];
				
			}
			/* Compute code */
			for (int i = 0; (i < count); i = (i + 1)) {
				fYec2[i] = float(fInput1[i]);
				
			}
			/* Post code */
			for (int j = 0; (j < 4); j = (j + 1)) {
				fYec2_perm[j] = fYec2_tmp[(count + j)];
				
			}
			/* Recursive loop 2 */
			/* Pre code */
			for (int j1 = 0; (j1 < 4); j1 = (j1 + 1)) {
				fRec3_tmp[j1] = fRec3_perm[j1];
				
			}
			/* Compute code */
			for (int i = 0; (i < count); i = (i + 1)) {
				fRec3[i] = ((fSlow3 * fRec3[(i - 1)]) + (fSlow4 * (fYec0[(i - 1)] + float(fInput0[i]))));
				
			}
			/* Post code */
			for (int j = 0; (j < 4); j = (j + 1)) {
				fRec3_perm[j] = fRec3_tmp[(count + j)];
				
			}
			/* Recursive loop 3 */
			/* Pre code */
			for (int j7 = 0; (j7 < 4); j7 = (j7 + 1)) {
				fRec10_tmp[j7] = fRec10_perm[j7];
				
			}
			/* Compute code */
			for (int i = 0; (i < count); i = (i + 1)) {
				fRec10[i] = ((fSlow3 * fRec10[(i - 1)]) + (fSlow4 * (fYec2[(i - 1)] + float(fInput1[i]))));
				
			}
			/* Post code */
			for (int j = 0; (j < 4); j = (j + 1)) {
				fRec10_perm[j] = fRec10_tmp[(count + j)];
				
			}
			/* Recursive loop 4 */
			/* Pre code */
			for (int j2 = 0; (j2 < 4); j2 = (j2 + 1)) {
				fRec2_tmp[j2] = fRec2_perm[j2];
				
			}
			/* Compute code */
			for (int i = 0; (i < count); i = (i + 1)) {
				fRec2[i] = (fRec3[i] - (fSlow6 * ((fSlow7 * fRec2[(i - 2)]) + (fSlow9 * fRec2[(i - 1)]))));
				
			}
			/* Post code */
			for (int j = 0; (j < 4); j = (j + 1)) {
				fRec2_perm[j] = fRec2_tmp[(count + j)];
				
			}
			/* Recursive loop 5 */
			/* Pre code */
			for (int j8 = 0; (j8 < 4); j8 = (j8 + 1)) {
				fRec9_tmp[j8] = fRec9_perm[j8];
				
			}
			/* Compute code */
			for (int i = 0; (i < count); i = (i + 1)) {
				fRec9[i] = (fRec10[i] - (fSlow6 * ((fSlow7 * fRec9[(i - 2)]) + (fSlow9 * fRec9[(i - 1)]))));
				
			}
			/* Post code */
			for (int j = 0; (j < 4); j = (j + 1)) {
				fRec9_perm[j] = fRec9_tmp[(count + j)];
				
			}
			/* Recursive loop 6 */
			/* Compute code */
			for (int i = 0; (i < count); i = (i + 1)) {
				fZec0[i] = (fRec2[(i - 2)] + (fRec2[i] + (2.0f * fRec2[(i - 1)])));
				
			}
			/* Recursive loop 7 */
			/* Compute code */
			for (int i = 0; (i < count); i = (i + 1)) {
				fZec1[i] = (fRec9[(i - 2)] + (fRec9[i] + (2.0f * fRec9[(i - 1)])));
				
			}
			/* Recursive loop 8 */
			/* Pre code */
			for (int j3 = 0; (j3 < 4); j3 = (j3 + 1)) {
				fYec1_tmp[j3] = fYec1_perm[j3];
				
			}
			/* Compute code */
			for (int i = 0; (i < count); i = (i + 1)) {
				fYec1[i] = (fSlow6 * fZec0[i]);
				
			}
			/* Post code */
			for (int j = 0; (j < 4); j = (j + 1)) {
				fYec1_perm[j] = fYec1_tmp[(count + j)];
				
			}
			/* Recursive loop 9 */
			/* Pre code */
			for (int j9 = 0; (j9 < 4); j9 = (j9 + 1)) {
				fYec3_tmp[j9] = fYec3_perm[j9];
				
			}
			/* Compute code */
			for (int i = 0; (i < count); i = (i + 1)) {
				fYec3[i] = (fSlow6 * fZec1[i]);
				
			}
			/* Post code */
			for (int j = 0; (j < 4); j = (j + 1)) {
				fYec3_perm[j] = fYec3_tmp[(count + j)];
				
			}
			/* Recursive loop 10 */
			/* Pre code */
			for (int j22 = 0; (j22 < 4); j22 = (j22 + 1)) {
				fRec20_tmp[j22] = fRec20_perm[j22];
				
			}
			/* Compute code */
			for (int i = 0; (i < count); i = (i + 1)) {
				fRec20[i] = ((fSlow3 * fRec20[(i - 1)]) + (fSlow4 * ((fSlow1 * float(fInput0[i])) + (fSlow42 * fYec0[(i - 1)]))));
				
			}
			/* Post code */
			for (int j = 0; (j < 4); j = (j + 1)) {
				fRec20_perm[j] = fRec20_tmp[(count + j)];
				
			}
			/* Recursive loop 11 */
			/* Pre code */
			for (int j25 = 0; (j25 < 4); j25 = (j25 + 1)) {
				fRec26_tmp[j25] = fRec26_perm[j25];
				
			}
			/* Compute code */
			for (int i = 0; (i < count); i = (i + 1)) {
				fRec26[i] = ((fSlow3 * fRec26[(i - 1)]) + (fSlow4 * ((fSlow1 * float(fInput1[i])) + (fSlow42 * fYec2[(i - 1)]))));
				
			}
			/* Post code */
			for (int j = 0; (j < 4); j = (j + 1)) {
				fRec26_perm[j] = fRec26_tmp[(count + j)];
				
			}
			/* Recursive loop 12 */
			/* Pre code */
			for (int j4 = 0; (j4 < 4); j4 = (j4 + 1)) {
				fRec1_tmp[j4] = fRec1_perm[j4];
				
			}
			/* Compute code */
			for (int i = 0; (i < count); i = (i + 1)) {
				fRec1[i] = ((fSlow13 * fRec1[(i - 1)]) + (fSlow14 * (fYec1[i] + fYec1[(i - 1)])));
				
			}
			/* Post code */
			for (int j = 0; (j < 4); j = (j + 1)) {
				fRec1_perm[j] = fRec1_tmp[(count + j)];
				
			}
			/* Recursive loop 13 */
			/* Pre code */
			for (int j10 = 0; (j10 < 4); j10 = (j10 + 1)) {
				fRec8_tmp[j10] = fRec8_perm[j10];
				
			}
			/* Compute code */
			for (int i = 0; (i < count); i = (i + 1)) {
				fRec8[i] = ((fSlow13 * fRec8[(i - 1)]) + (fSlow14 * (fYec3[i] + fYec3[(i - 1)])));
				
			}
			/* Post code */
			for (int j = 0; (j < 4); j = (j + 1)) {
				fRec8_perm[j] = fRec8_tmp[(count + j)];
				
			}
			/* Recursive loop 14 */
			/* Pre code */
			for (int j15 = 0; (j15 < 4); j15 = (j15 + 1)) {
				fRec12_tmp[j15] = fRec12_perm[j15];
				
			}
			/* Compute code */
			for (int i = 0; (i < count); i = (i + 1)) {
				fRec12[i] = ((fSlow13 * fRec12[(i - 1)]) + (fSlow14 * ((fSlow29 * fZec0[i]) + (fSlow30 * fYec1[(i - 1)]))));
				
			}
			/* Post code */
			for (int j = 0; (j < 4); j = (j + 1)) {
				fRec12_perm[j] = fRec12_tmp[(count + j)];
				
			}
			/* Recursive loop 15 */
			/* Pre code */
			for (int j17 = 0; (j17 < 4); j17 = (j17 + 1)) {
				fRec17_tmp[j17] = fRec17_perm[j17];
				
			}
			/* Compute code */
			for (int i = 0; (i < count); i = (i + 1)) {
				fRec17[i] = ((fSlow13 * fRec17[(i - 1)]) + (fSlow14 * ((fSlow29 * fZec1[i]) + (fSlow30 * fYec3[(i - 1)]))));
				
			}
			/* Post code */
			for (int j = 0; (j < 4); j = (j + 1)) {
				fRec17_perm[j] = fRec17_tmp[(count + j)];
				
			}
			/* Recursive loop 16 */
			/* Pre code */
			for (int j23 = 0; (j23 < 4); j23 = (j23 + 1)) {
				fRec19_tmp[j23] = fRec19_perm[j23];
				
			}
			/* Compute code */
			for (int i = 0; (i < count); i = (i + 1)) {
				fRec19[i] = (fRec20[i] - (fSlow6 * ((fSlow7 * fRec19[(i - 2)]) + (fSlow9 * fRec19[(i - 1)]))));
				
			}
			/* Post code */
			for (int j = 0; (j < 4); j = (j + 1)) {
				fRec19_perm[j] = fRec19_tmp[(count + j)];
				
			}
			/* Recursive loop 17 */
			/* Pre code */
			for (int j26 = 0; (j26 < 4); j26 = (j26 + 1)) {
				fRec25_tmp[j26] = fRec25_perm[j26];
				
			}
			/* Compute code */
			for (int i = 0; (i < count); i = (i + 1)) {
				fRec25[i] = (fRec26[i] - (fSlow6 * ((fSlow7 * fRec25[(i - 2)]) + (fSlow9 * fRec25[(i - 1)]))));
				
			}
			/* Post code */
			for (int j = 0; (j < 4); j = (j + 1)) {
				fRec25_perm[j] = fRec25_tmp[(count + j)];
				
			}
			/* Recursive loop 18 */
			/* Pre code */
			for (int j5 = 0; (j5 < 4); j5 = (j5 + 1)) {
				fRec0_tmp[j5] = fRec0_perm[j5];
				
			}
			/* Compute code */
			for (int i = 0; (i < count); i = (i + 1)) {
				fRec0[i] = (fRec1[i] - (fSlow15 * ((fSlow16 * fRec0[(i - 2)]) + (fSlow18 * fRec0[(i - 1)]))));
				
			}
			/* Post code */
			for (int j = 0; (j < 4); j = (j + 1)) {
				fRec0_perm[j] = fRec0_tmp[(count + j)];
				
			}
			/* Recursive loop 19 */
			/* Pre code */
			for (int j11 = 0; (j11 < 4); j11 = (j11 + 1)) {
				fRec7_tmp[j11] = fRec7_perm[j11];
				
			}
			/* Compute code */
			for (int i = 0; (i < count); i = (i + 1)) {
				fRec7[i] = (fRec8[i] - (fSlow15 * ((fSlow16 * fRec7[(i - 2)]) + (fSlow18 * fRec7[(i - 1)]))));
				
			}
			/* Post code */
			for (int j = 0; (j < 4); j = (j + 1)) {
				fRec7_perm[j] = fRec7_tmp[(count + j)];
				
			}
			/* Recursive loop 20 */
			/* Pre code */
			for (int j16 = 0; (j16 < 4); j16 = (j16 + 1)) {
				fRec11_tmp[j16] = fRec11_perm[j16];
				
			}
			/* Compute code */
			for (int i = 0; (i < count); i = (i + 1)) {
				fRec11[i] = (fRec12[i] - (fSlow15 * ((fSlow16 * fRec11[(i - 2)]) + (fSlow18 * fRec11[(i - 1)]))));
				
			}
			/* Post code */
			for (int j = 0; (j < 4); j = (j + 1)) {
				fRec11_perm[j] = fRec11_tmp[(count + j)];
				
			}
			/* Recursive loop 21 */
			/* Pre code */
			for (int j18 = 0; (j18 < 4); j18 = (j18 + 1)) {
				fRec16_tmp[j18] = fRec16_perm[j18];
				
			}
			/* Compute code */
			for (int i = 0; (i < count); i = (i + 1)) {
				fRec16[i] = (fRec17[i] - (fSlow15 * ((fSlow16 * fRec16[(i - 2)]) + (fSlow18 * fRec16[(i - 1)]))));
				
			}
			/* Post code */
			for (int j = 0; (j < 4); j = (j + 1)) {
				fRec16_perm[j] = fRec16_tmp[(count + j)];
				
			}
			/* Recursive loop 22 */
			/* Pre code */
			for (int j24 = 0; (j24 < 4); j24 = (j24 + 1)) {
				fRec18_tmp[j24] = fRec18_perm[j24];
				
			}
			/* Compute code */
			for (int i = 0; (i < count); i = (i + 1)) {
				fZec12[i] = (fSlow18 * fRec18[(i - 1)]);
				fRec18[i] = ((fSlow6 * (((fSlow8 * fRec19[i]) + (fSlow43 * fRec19[(i - 1)])) + (fSlow8 * fRec19[(i - 2)]))) - (fSlow44 * ((fSlow45 * fRec18[(i - 2)]) + fZec12[i])));
				
			}
			/* Post code */
			for (int j = 0; (j < 4); j = (j + 1)) {
				fRec18_perm[j] = fRec18_tmp[(count + j)];
				
			}
			/* Recursive loop 23 */
			/* Pre code */
			for (int j27 = 0; (j27 < 4); j27 = (j27 + 1)) {
				fRec24_tmp[j27] = fRec24_perm[j27];
				
			}
			/* Compute code */
			for (int i = 0; (i < count); i = (i + 1)) {
				fZec13[i] = (fSlow18 * fRec24[(i - 1)]);
				fRec24[i] = ((fSlow6 * (((fSlow8 * fRec25[i]) + (fSlow43 * fRec25[(i - 1)])) + (fSlow8 * fRec25[(i - 2)]))) - (fSlow44 * ((fSlow45 * fRec24[(i - 2)]) + fZec13[i])));
				
			}
			/* Post code */
			for (int j = 0; (j < 4); j = (j + 1)) {
				fRec24_perm[j] = fRec24_tmp[(count + j)];
				
			}
			/* Recursive loop 24 */
			/* Compute code */
			for (int i = 0; (i < count); i = (i + 1)) {
				fZec2[i] = (fSlow15 * (fRec0[(i - 2)] + (fRec0[i] + (2.0f * fRec0[(i - 1)]))));
				
			}
			/* Recursive loop 25 */
			/* Compute code */
			for (int i = 0; (i < count); i = (i + 1)) {
				fZec4[i] = (fSlow15 * (fRec7[(i - 2)] + (fRec7[i] + (2.0f * fRec7[(i - 1)]))));
				
			}
			/* Recursive loop 26 */
			/* Compute code */
			for (int i = 0; (i < count); i = (i + 1)) {
				fZec7[i] = (fSlow15 * (((fSlow17 * fRec11[i]) + (fSlow33 * fRec11[(i - 1)])) + (fSlow17 * fRec11[(i - 2)])));
				
			}
			/* Recursive loop 27 */
			/* Compute code */
			for (int i = 0; (i < count); i = (i + 1)) {
				fZec9[i] = (fSlow15 * (((fSlow17 * fRec16[i]) + (fSlow33 * fRec16[(i - 1)])) + (fSlow17 * fRec16[(i - 2)])));
				
			}
			/* Recursive loop 28 */
			/* Compute code */
			for (int i = 0; (i < count); i = (i + 1)) {
				fZec14[i] = (fRec18[(i - 2)] + (fSlow44 * (fZec12[i] + (fSlow45 * fRec18[i]))));
				
			}
			/* Recursive loop 29 */
			/* Compute code */
			for (int i = 0; (i < count); i = (i + 1)) {
				fZec16[i] = (fRec24[(i - 2)] + (fSlow44 * (fZec13[i] + (fSlow45 * fRec24[i]))));
				
			}
			/* Recursive loop 30 */
			/* Compute code */
			for (int i = 0; (i < count); i = (i + 1)) {
				fZec3[i] = (iSlow20?0.0f:fZec2[i]);
				
			}
			/* Recursive loop 31 */
			/* Compute code */
			for (int i = 0; (i < count); i = (i + 1)) {
				fZec5[i] = (iSlow20?0.0f:fZec4[i]);
				
			}
			/* Recursive loop 32 */
			/* Compute code */
			for (int i = 0; (i < count); i = (i + 1)) {
				fZec8[i] = (iSlow32?0.0f:fZec7[i]);
				
			}
			/* Recursive loop 33 */
			/* Compute code */
			for (int i = 0; (i < count); i = (i + 1)) {
				fZec10[i] = (iSlow32?0.0f:fZec9[i]);
				
			}
			/* Recursive loop 34 */
			/* Compute code */
			for (int i = 0; (i < count); i = (i + 1)) {
				fZec15[i] = (iSlow47?0.0f:fZec14[i]);
				
			}
			/* Recursive loop 35 */
			/* Compute code */
			for (int i = 0; (i < count); i = (i + 1)) {
				fZec17[i] = (iSlow47?0.0f:fZec16[i]);
				
			}
			/* Recursive loop 36 */
			/* Compute code */
			for (int i = 0; (i < count); i = (i + 1)) {
				fZec6[i] = fabsf((fabsf((fSlow19 * fZec3[i])) + fabsf((fSlow19 * fZec5[i]))));
				
			}
			/* Recursive loop 37 */
			/* Compute code */
			for (int i = 0; (i < count); i = (i + 1)) {
				fZec11[i] = fabsf((fabsf((fSlow31 * fZec8[i])) + fabsf((fSlow31 * fZec10[i]))));
				
			}
			/* Recursive loop 38 */
			/* Compute code */
			for (int i = 0; (i < count); i = (i + 1)) {
				fZec18[i] = fabsf((fabsf((fSlow46 * fZec15[i])) + fabsf((fSlow46 * fZec17[i]))));
				
			}
			/* Recursive loop 39 */
			/* Pre code */
			for (int j12 = 0; (j12 < 4); j12 = (j12 + 1)) {
				fRec6_tmp[j12] = fRec6_perm[j12];
				
			}
			/* Compute code */
			for (int i = 0; (i < count); i = (i + 1)) {
				fRec6[i] = max(fZec6[i], ((fSlow21 * fRec6[(i - 1)]) + (fSlow22 * fZec6[i])));
				
			}
			/* Post code */
			for (int j = 0; (j < 4); j = (j + 1)) {
				fRec6_perm[j] = fRec6_tmp[(count + j)];
				
			}
			/* Recursive loop 40 */
			/* Pre code */
			for (int j19 = 0; (j19 < 4); j19 = (j19 + 1)) {
				fRec15_tmp[j19] = fRec15_perm[j19];
				
			}
			/* Compute code */
			for (int i = 0; (i < count); i = (i + 1)) {
				fRec15[i] = max(fZec11[i], ((fSlow34 * fRec15[(i - 1)]) + (fSlow35 * fZec11[i])));
				
			}
			/* Post code */
			for (int j = 0; (j < 4); j = (j + 1)) {
				fRec15_perm[j] = fRec15_tmp[(count + j)];
				
			}
			/* Recursive loop 41 */
			/* Pre code */
			for (int j28 = 0; (j28 < 4); j28 = (j28 + 1)) {
				fRec23_tmp[j28] = fRec23_perm[j28];
				
			}
			/* Compute code */
			for (int i = 0; (i < count); i = (i + 1)) {
				fRec23[i] = max(fZec18[i], ((fSlow48 * fRec23[(i - 1)]) + (fSlow49 * fZec18[i])));
				
			}
			/* Post code */
			for (int j = 0; (j < 4); j = (j + 1)) {
				fRec23_perm[j] = fRec23_tmp[(count + j)];
				
			}
			/* Recursive loop 42 */
			/* Pre code */
			for (int j13 = 0; (j13 < 4); j13 = (j13 + 1)) {
				fRec5_tmp[j13] = fRec5_perm[j13];
				
			}
			/* Compute code */
			for (int i = 0; (i < count); i = (i + 1)) {
				fRec5[i] = ((fSlow24 * fRec5[(i - 1)]) + (fSlow25 * fRec6[i]));
				
			}
			/* Post code */
			for (int j = 0; (j < 4); j = (j + 1)) {
				fRec5_perm[j] = fRec5_tmp[(count + j)];
				
			}
			/* Recursive loop 43 */
			/* Pre code */
			for (int j20 = 0; (j20 < 4); j20 = (j20 + 1)) {
				fRec14_tmp[j20] = fRec14_perm[j20];
				
			}
			/* Compute code */
			for (int i = 0; (i < count); i = (i + 1)) {
				fRec14[i] = ((fSlow37 * fRec14[(i - 1)]) + (fSlow38 * fRec15[i]));
				
			}
			/* Post code */
			for (int j = 0; (j < 4); j = (j + 1)) {
				fRec14_perm[j] = fRec14_tmp[(count + j)];
				
			}
			/* Recursive loop 44 */
			/* Pre code */
			for (int j29 = 0; (j29 < 4); j29 = (j29 + 1)) {
				fRec22_tmp[j29] = fRec22_perm[j29];
				
			}
			/* Compute code */
			for (int i = 0; (i < count); i = (i + 1)) {
				fRec22[i] = ((fSlow51 * fRec22[(i - 1)]) + (fSlow52 * fRec23[i]));
				
			}
			/* Post code */
			for (int j = 0; (j < 4); j = (j + 1)) {
				fRec22_perm[j] = fRec22_tmp[(count + j)];
				
			}
			/* Recursive loop 45 */
			/* Pre code */
			for (int j14 = 0; (j14 < 4); j14 = (j14 + 1)) {
				fRec4_tmp[j14] = fRec4_perm[j14];
				
			}
			/* Compute code */
			for (int i = 0; (i < count); i = (i + 1)) {
				fRec4[i] = ((fSlow26 * fRec4[(i - 1)]) + (fSlow27 * max(((8.685889f * ((8.262958e-08f * float(int(pun_float_to_int(float(fRec5[i]))))) - 87.98997f)) - fSlow28), 0.0f)));
				
			}
			/* Post code */
			for (int j = 0; (j < 4); j = (j + 1)) {
				fRec4_perm[j] = fRec4_tmp[(count + j)];
				
			}
			/* Recursive loop 46 */
			/* Pre code */
			for (int j21 = 0; (j21 < 4); j21 = (j21 + 1)) {
				fRec13_tmp[j21] = fRec13_perm[j21];
				
			}
			/* Compute code */
			for (int i = 0; (i < count); i = (i + 1)) {
				fRec13[i] = ((fSlow39 * fRec13[(i - 1)]) + (fSlow40 * max(((8.685889f * ((8.262958e-08f * float(int(pun_float_to_int(float(fRec14[i]))))) - 87.98997f)) - fSlow41), 0.0f)));
				
			}
			/* Post code */
			for (int j = 0; (j < 4); j = (j + 1)) {
				fRec13_perm[j] = fRec13_tmp[(count + j)];
				
			}
			/* Recursive loop 47 */
			/* Pre code */
			for (int j30 = 0; (j30 < 4); j30 = (j30 + 1)) {
				fRec21_tmp[j30] = fRec21_perm[j30];
				
			}
			/* Compute code */
			for (int i = 0; (i < count); i = (i + 1)) {
				fRec21[i] = ((fSlow53 * fRec21[(i - 1)]) + (fSlow54 * max(((8.685889f * ((8.262958e-08f * float(int(pun_float_to_int(float(fRec22[i]))))) - 87.98997f)) - fSlow55), 0.0f)));
				
			}
			/* Post code */
			for (int j = 0; (j < 4); j = (j + 1)) {
				fRec21_perm[j] = fRec21_tmp[(count + j)];
				
			}
			/* Recursive loop 48 */
			/* Compute code */
			for (int i = 0; (i < count); i = (i + 1)) {
				fZec19[i] = float(pun_int_to_float(int((8388608.0f * (126.942696f + max(-126.0f, (0.1660964f * fRec4[i])))))));
				
			}
			/* Recursive loop 49 */
			/* Compute code */
			for (int i = 0; (i < count); i = (i + 1)) {
				fZec23[i] = float(pun_int_to_float(int((8388608.0f * (126.942696f + max(-126.0f, (0.1660964f * fRec13[i])))))));
				
			}
			/* Recursive loop 50 */
			/* Compute code */
			for (int i = 0; (i < count); i = (i + 1)) {
				fZec27[i] = float(pun_int_to_float(int((8388608.0f * (126.942696f + max(-126.0f, (0.1660964f * fRec21[i])))))));
				
			}
			/* Recursive loop 51 */
			/* Compute code */
			for (int i = 0; (i < count); i = (i + 1)) {
				fZec20[i] = (fZec3[i] * fZec19[i]);
				
			}
			/* Recursive loop 52 */
			/* Compute code */
			for (int i = 0; (i < count); i = (i + 1)) {
				fZec21[i] = (fSlow19 * (fZec5[i] * fZec19[i]));
				
			}
			/* Recursive loop 53 */
			/* Compute code */
			for (int i = 0; (i < count); i = (i + 1)) {
				fZec24[i] = (fZec8[i] * fZec23[i]);
				
			}
			/* Recursive loop 54 */
			/* Compute code */
			for (int i = 0; (i < count); i = (i + 1)) {
				fZec25[i] = (fSlow31 * (fZec10[i] * fZec23[i]));
				
			}
			/* Recursive loop 55 */
			/* Compute code */
			for (int i = 0; (i < count); i = (i + 1)) {
				fZec28[i] = (fZec15[i] * fZec27[i]);
				
			}
			/* Recursive loop 56 */
			/* Compute code */
			for (int i = 0; (i < count); i = (i + 1)) {
				fZec29[i] = (fSlow46 * (fZec17[i] * fZec27[i]));
				
			}
			/* Recursive loop 57 */
			/* Compute code */
			for (int i = 0; (i < count); i = (i + 1)) {
				fZec22[i] = fabsf((fabsf((fSlow19 * fZec20[i])) + fabsf(fZec21[i])));
				
			}
			/* Recursive loop 58 */
			/* Compute code */
			for (int i = 0; (i < count); i = (i + 1)) {
				fZec26[i] = fabsf((fabsf((fSlow31 * fZec24[i])) + fabsf(fZec25[i])));
				
			}
			/* Recursive loop 59 */
			/* Compute code */
			for (int i = 0; (i < count); i = (i + 1)) {
				fZec30[i] = fabsf((fabsf((fSlow46 * fZec28[i])) + fabsf(fZec29[i])));
				
			}
			/* Recursive loop 60 */
			/* Pre code */
			for (int j31 = 0; (j31 < 4); j31 = (j31 + 1)) {
				fRec32_tmp[j31] = fRec32_perm[j31];
				
			}
			/* Compute code */
			for (int i = 0; (i < count); i = (i + 1)) {
				fRec32[i] = max(fZec22[i], ((fSlow21 * fRec32[(i - 1)]) + (fSlow22 * fZec22[i])));
				
			}
			/* Post code */
			for (int j = 0; (j < 4); j = (j + 1)) {
				fRec32_perm[j] = fRec32_tmp[(count + j)];
				
			}
			/* Recursive loop 61 */
			/* Pre code */
			for (int j34 = 0; (j34 < 4); j34 = (j34 + 1)) {
				fRec35_tmp[j34] = fRec35_perm[j34];
				
			}
			/* Compute code */
			for (int i = 0; (i < count); i = (i + 1)) {
				fRec35[i] = max(fZec26[i], ((fSlow34 * fRec35[(i - 1)]) + (fSlow35 * fZec26[i])));
				
			}
			/* Post code */
			for (int j = 0; (j < 4); j = (j + 1)) {
				fRec35_perm[j] = fRec35_tmp[(count + j)];
				
			}
			/* Recursive loop 62 */
			/* Pre code */
			for (int j37 = 0; (j37 < 4); j37 = (j37 + 1)) {
				fRec38_tmp[j37] = fRec38_perm[j37];
				
			}
			/* Compute code */
			for (int i = 0; (i < count); i = (i + 1)) {
				fRec38[i] = max(fZec30[i], ((fSlow48 * fRec38[(i - 1)]) + (fSlow49 * fZec30[i])));
				
			}
			/* Post code */
			for (int j = 0; (j < 4); j = (j + 1)) {
				fRec38_perm[j] = fRec38_tmp[(count + j)];
				
			}
			/* Recursive loop 63 */
			/* Pre code */
			for (int j32 = 0; (j32 < 4); j32 = (j32 + 1)) {
				fRec31_tmp[j32] = fRec31_perm[j32];
				
			}
			/* Compute code */
			for (int i = 0; (i < count); i = (i + 1)) {
				fRec31[i] = ((fSlow24 * fRec31[(i - 1)]) + (fSlow25 * fRec32[i]));
				
			}
			/* Post code */
			for (int j = 0; (j < 4); j = (j + 1)) {
				fRec31_perm[j] = fRec31_tmp[(count + j)];
				
			}
			/* Recursive loop 64 */
			/* Pre code */
			for (int j35 = 0; (j35 < 4); j35 = (j35 + 1)) {
				fRec34_tmp[j35] = fRec34_perm[j35];
				
			}
			/* Compute code */
			for (int i = 0; (i < count); i = (i + 1)) {
				fRec34[i] = ((fSlow37 * fRec34[(i - 1)]) + (fSlow38 * fRec35[i]));
				
			}
			/* Post code */
			for (int j = 0; (j < 4); j = (j + 1)) {
				fRec34_perm[j] = fRec34_tmp[(count + j)];
				
			}
			/* Recursive loop 65 */
			/* Pre code */
			for (int j38 = 0; (j38 < 4); j38 = (j38 + 1)) {
				fRec37_tmp[j38] = fRec37_perm[j38];
				
			}
			/* Compute code */
			for (int i = 0; (i < count); i = (i + 1)) {
				fRec37[i] = ((fSlow51 * fRec37[(i - 1)]) + (fSlow52 * fRec38[i]));
				
			}
			/* Post code */
			for (int j = 0; (j < 4); j = (j + 1)) {
				fRec37_perm[j] = fRec37_tmp[(count + j)];
				
			}
			/* Recursive loop 66 */
			/* Pre code */
			for (int j33 = 0; (j33 < 4); j33 = (j33 + 1)) {
				fRec30_tmp[j33] = fRec30_perm[j33];
				
			}
			/* Compute code */
			for (int i = 0; (i < count); i = (i + 1)) {
				fRec30[i] = ((fSlow26 * fRec30[(i - 1)]) + (fSlow27 * max(((8.685889f * ((8.262958e-08f * float(int(pun_float_to_int(float(fRec31[i]))))) - 87.98997f)) - fSlow28), 0.0f)));
				
			}
			/* Post code */
			for (int j = 0; (j < 4); j = (j + 1)) {
				fRec30_perm[j] = fRec30_tmp[(count + j)];
				
			}
			/* Recursive loop 67 */
			/* Pre code */
			for (int j36 = 0; (j36 < 4); j36 = (j36 + 1)) {
				fRec33_tmp[j36] = fRec33_perm[j36];
				
			}
			/* Compute code */
			for (int i = 0; (i < count); i = (i + 1)) {
				fRec33[i] = ((fSlow39 * fRec33[(i - 1)]) + (fSlow40 * max(((8.685889f * ((8.262958e-08f * float(int(pun_float_to_int(float(fRec34[i]))))) - 87.98997f)) - fSlow41), 0.0f)));
				
			}
			/* Post code */
			for (int j = 0; (j < 4); j = (j + 1)) {
				fRec33_perm[j] = fRec33_tmp[(count + j)];
				
			}
			/* Recursive loop 68 */
			/* Pre code */
			for (int j39 = 0; (j39 < 4); j39 = (j39 + 1)) {
				fRec36_tmp[j39] = fRec36_perm[j39];
				
			}
			/* Compute code */
			for (int i = 0; (i < count); i = (i + 1)) {
				fRec36[i] = ((fSlow53 * fRec36[(i - 1)]) + (fSlow54 * max(((8.685889f * ((8.262958e-08f * float(int(pun_float_to_int(float(fRec37[i]))))) - 87.98997f)) - fSlow55), 0.0f)));
				
			}
			/* Post code */
			for (int j = 0; (j < 4); j = (j + 1)) {
				fRec36_perm[j] = fRec36_tmp[(count + j)];
				
			}
			/* Recursive loop 69 */
			/* Compute code */
			for (int i = 0; (i < count); i = (i + 1)) {
				fZec31[i] = (fSlow64 * fZec20[i]);
				
			}
			/* Recursive loop 70 */
			/* Compute code */
			for (int i = 0; (i < count); i = (i + 1)) {
				fZec32[i] = (fSlow67 * fZec24[i]);
				
			}
			/* Recursive loop 71 */
			/* Compute code */
			for (int i = 0; (i < count); i = (i + 1)) {
				fZec33[i] = (fSlow70 * fZec28[i]);
				
			}
			/* Recursive loop 72 */
			/* Compute code */
			for (int i = 0; (i < count); i = (i + 1)) {
				fHbargraph0 = FAUSTFLOAT((0.5f * float(pun_int_to_float(int((8388608.0f * (126.942696f + max(-126.0f, (0.1660964f * fRec30[i])))))))));
				fZec36[i] = (fSlow71 * fZec21[i]);
				
			}
			/* Recursive loop 73 */
			/* Compute code */
			for (int i = 0; (i < count); i = (i + 1)) {
				fHbargraph2 = FAUSTFLOAT((0.5f * float(pun_int_to_float(int((8388608.0f * (126.942696f + max(-126.0f, (0.1660964f * fRec33[i])))))))));
				fZec37[i] = (fSlow72 * fZec25[i]);
				
			}
			/* Recursive loop 74 */
			/* Compute code */
			for (int i = 0; (i < count); i = (i + 1)) {
				fHbargraph4 = FAUSTFLOAT((0.5f * float(pun_int_to_float(int((8388608.0f * (126.942696f + max(-126.0f, (0.1660964f * fRec36[i])))))))));
				fZec38[i] = (fSlow73 * fZec29[i]);
				
			}
			/* Recursive loop 75 */
			/* Compute code */
			for (int i = 0; (i < count); i = (i + 1)) {
				fZec34[i] = (((iSlow20?fZec2[i]:fZec31[i]) + (iSlow32?fZec7[i]:fZec32[i])) + (iSlow47?fZec14[i]:fZec33[i]));
				
			}
			/* Recursive loop 76 */
			/* Compute code */
			for (int i = 0; (i < count); i = (i + 1)) {
				fHbargraph1 = FAUSTFLOAT((fabsf(fZec31[i]) + fabsf(fZec36[i])));
				fHbargraph3 = FAUSTFLOAT((fabsf(fZec32[i]) + fabsf(fZec37[i])));
				fHbargraph5 = FAUSTFLOAT((fabsf(fZec33[i]) + fabsf(fZec38[i])));
				fZec39[i] = (((iSlow20?fZec4[i]:fZec36[i]) + (iSlow32?fZec9[i]:fZec37[i])) + (iSlow47?fZec16[i]:fZec38[i]));
				
			}
			/* Recursive loop 77 */
			/* Compute code */
			for (int i = 0; (i < count); i = (i + 1)) {
				fZec35[i] = (iSlow57?0.0f:fZec34[i]);
				
			}
			/* Recursive loop 78 */
			/* Compute code */
			for (int i = 0; (i < count); i = (i + 1)) {
				fZec40[i] = (iSlow57?0.0f:fZec39[i]);
				
			}
			/* Recursive loop 79 */
			/* Compute code */
			for (int i = 0; (i < count); i = (i + 1)) {
				fZec41[i] = fabsf((fabsf((fSlow56 * fZec35[i])) + fabsf((fSlow56 * fZec40[i]))));
				
			}
			/* Recursive loop 80 */
			/* Pre code */
			for (int j40 = 0; (j40 < 4); j40 = (j40 + 1)) {
				fRec29_tmp[j40] = fRec29_perm[j40];
				
			}
			/* Compute code */
			for (int i = 0; (i < count); i = (i + 1)) {
				fRec29[i] = max(fZec41[i], ((fSlow74 * fRec29[(i - 1)]) + (fSlow75 * fZec41[i])));
				
			}
			/* Post code */
			for (int j = 0; (j < 4); j = (j + 1)) {
				fRec29_perm[j] = fRec29_tmp[(count + j)];
				
			}
			/* Recursive loop 81 */
			/* Pre code */
			for (int j41 = 0; (j41 < 4); j41 = (j41 + 1)) {
				fRec28_tmp[j41] = fRec28_perm[j41];
				
			}
			/* Compute code */
			for (int i = 0; (i < count); i = (i + 1)) {
				fRec28[i] = ((fSlow77 * fRec28[(i - 1)]) + (fSlow78 * fRec29[i]));
				
			}
			/* Post code */
			for (int j = 0; (j < 4); j = (j + 1)) {
				fRec28_perm[j] = fRec28_tmp[(count + j)];
				
			}
			/* Recursive loop 82 */
			/* Pre code */
			for (int j42 = 0; (j42 < 4); j42 = (j42 + 1)) {
				fRec27_tmp[j42] = fRec27_perm[j42];
				
			}
			/* Compute code */
			for (int i = 0; (i < count); i = (i + 1)) {
				fRec27[i] = ((fSlow79 * fRec27[(i - 1)]) + (fSlow80 * max((6.0f + (8.685889f * ((8.262958e-08f * float(int(pun_float_to_int(float(fRec28[i]))))) - 87.98997f))), 0.0f)));
				
			}
			/* Post code */
			for (int j = 0; (j < 4); j = (j + 1)) {
				fRec27_perm[j] = fRec27_tmp[(count + j)];
				
			}
			/* Recursive loop 83 */
			/* Compute code */
			for (int i = 0; (i < count); i = (i + 1)) {
				fZec42[i] = float(pun_int_to_float(int((8388608.0f * (126.942696f + max(-126.0f, (0.1660964f * fRec27[i])))))));
				
			}
			/* Vectorizable loop 84 */
			/* Compute code */
			for (int i = 0; (i < count); i = (i + 1)) {
				float fSel0 = 0.0f;
				if (iSlow57 != 0) {
					fSel0 = fZec34[i];
					
				} else {
					fSel0 = (fSlow82 * (fZec35[i] * fZec42[i]));
					
				}
				fOutput0[i] = FAUSTFLOAT((fSlow81 * fSel0));
				
			}
			/* Vectorizable loop 85 */
			/* Compute code */
			for (int i = 0; (i < count); i = (i + 1)) {
				float fSel1 = 0.0f;
				if (iSlow57 != 0) {
					fSel1 = fZec39[i];
					
				} else {
					fSel1 = (fSlow82 * (fZec40[i] * fZec42[i]));
					
				}
				fOutput1[i] = FAUSTFLOAT((fSlow81 * fSel1));
				
			}
			
		}
		
	}

	
};



#include "Faust_plugins_template2.cpp"


#endif
