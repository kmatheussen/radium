//-----------------------------------------------------
//
// Code generated with Faust 0.9.73 (http://faust.grame.fr)
//-----------------------------------------------------
/* link with : "" */
#include "typepunning.h"
#include <math.h>
#ifndef FAUSTPOWER
#define FAUSTPOWER
#include <cmath>
template <int N> inline float faustpower(float x)          { return powf(x,N); } 
template <int N> inline double faustpower(double x)        { return pow(x,N); }
template <int N> inline int faustpower(int x)              { return faustpower<N/2>(x) * faustpower<N-N/2>(x); } 
template <> 	 inline int faustpower<0>(int x)            { return 1; }
template <> 	 inline int faustpower<1>(int x)            { return x; }
template <> 	 inline int faustpower<2>(int x)            { return x*x; }
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


#ifndef FAUSTCLASS 
#define FAUSTCLASS Multibandcomp_dsp
#endif

class Multibandcomp_dsp : public dsp {
  private:
	float 	fYec0_perm[4];
	FAUSTFLOAT 	fslider0;
	int 	iConst0;
	float 	fConst1;
	float 	fRec3_perm[4];
	float 	fRec2_perm[4];
	float 	fYec1_perm[4];
	FAUSTFLOAT 	fslider1;
	float 	fRec1_perm[4];
	float 	fRec0_perm[4];
	float 	fYec2_perm[4];
	float 	fRec10_perm[4];
	float 	fRec9_perm[4];
	float 	fYec3_perm[4];
	float 	fRec8_perm[4];
	float 	fRec7_perm[4];
	FAUSTFLOAT 	fcheckbox0;
	FAUSTFLOAT 	fslider2;
	float 	fConst2;
	FAUSTFLOAT 	fslider3;
	float 	fRec6_perm[4];
	FAUSTFLOAT 	fslider4;
	float 	fRec5_perm[4];
	FAUSTFLOAT 	fslider5;
	float 	fConst3;
	FAUSTFLOAT 	fslider6;
	float 	fRec4_perm[4];
	float 	fRec12_perm[4];
	float 	fRec11_perm[4];
	float 	fRec17_perm[4];
	float 	fRec16_perm[4];
	FAUSTFLOAT 	fcheckbox1;
	FAUSTFLOAT 	fslider7;
	FAUSTFLOAT 	fslider8;
	float 	fRec15_perm[4];
	FAUSTFLOAT 	fslider9;
	float 	fRec14_perm[4];
	FAUSTFLOAT 	fslider10;
	FAUSTFLOAT 	fslider11;
	float 	fRec13_perm[4];
	float 	fRec20_perm[4];
	float 	fRec19_perm[4];
	float 	fRec18_perm[4];
	float 	fRec26_perm[4];
	float 	fRec25_perm[4];
	float 	fRec24_perm[4];
	FAUSTFLOAT 	fcheckbox2;
	FAUSTFLOAT 	fslider12;
	FAUSTFLOAT 	fslider13;
	float 	fRec23_perm[4];
	FAUSTFLOAT 	fslider14;
	float 	fRec22_perm[4];
	FAUSTFLOAT 	fslider15;
	FAUSTFLOAT 	fslider16;
	float 	fRec21_perm[4];
	float 	fRec32_perm[4];
	float 	fRec31_perm[4];
	float 	fRec30_perm[4];
	float 	fRec35_perm[4];
	float 	fRec34_perm[4];
	float 	fRec33_perm[4];
	float 	fRec38_perm[4];
	float 	fRec37_perm[4];
	float 	fRec36_perm[4];
	FAUSTFLOAT 	fbargraph0;
	FAUSTFLOAT 	fslider17;
	FAUSTFLOAT 	fcheckbox3;
	FAUSTFLOAT 	fcheckbox4;
	FAUSTFLOAT 	fcheckbox5;
	FAUSTFLOAT 	fbargraph1;
	FAUSTFLOAT 	fbargraph2;
	FAUSTFLOAT 	fslider18;
	FAUSTFLOAT 	fbargraph3;
	FAUSTFLOAT 	fbargraph4;
	FAUSTFLOAT 	fslider19;
	FAUSTFLOAT 	fbargraph5;
	FAUSTFLOAT 	fcheckbox6;
	FAUSTFLOAT 	fslider20;
	FAUSTFLOAT 	fslider21;
	float 	fConst4;
	float 	fRec29_perm[4];
	FAUSTFLOAT 	fslider22;
	float 	fConst5;
	float 	fRec28_perm[4];
	float 	fConst6;
	FAUSTFLOAT 	fslider23;
	float 	fRec27_perm[4];
	FAUSTFLOAT 	fslider24;
	FAUSTFLOAT 	fslider25;
  public:
	static void metadata(Meta* m) 	{ 
		m->declare("filter.lib/name", "Faust Filter Library");
		m->declare("filter.lib/author", "Julius O. Smith (jos at ccrma.stanford.edu)");
		m->declare("filter.lib/copyright", "Julius O. Smith III");
		m->declare("filter.lib/version", "1.29");
		m->declare("filter.lib/license", "STK-4.3");
		m->declare("filter.lib/reference", "https://ccrma.stanford.edu/~jos/filters/");
		m->declare("music.lib/name", "Music Library");
		m->declare("music.lib/author", "GRAME");
		m->declare("music.lib/copyright", "GRAME");
		m->declare("music.lib/version", "1.0");
		m->declare("music.lib/license", "LGPL with exception");
		m->declare("math.lib/name", "Math Library");
		m->declare("math.lib/author", "GRAME");
		m->declare("math.lib/copyright", "GRAME");
		m->declare("math.lib/version", "1.0");
		m->declare("math.lib/license", "LGPL with exception");
		m->declare("effect.lib/name", "Faust Audio Effect Library");
		m->declare("effect.lib/author", "Julius O. Smith (jos at ccrma.stanford.edu)");
		m->declare("effect.lib/copyright", "Julius O. Smith III");
		m->declare("effect.lib/version", "1.33");
		m->declare("effect.lib/license", "STK-4.3");
		m->declare("effect.lib/exciter_name", "Harmonic Exciter");
		m->declare("effect.lib/exciter_author", "Priyanka Shekar (pshekar@ccrma.stanford.edu)");
		m->declare("effect.lib/exciter_copyright", "Copyright (c) 2013 Priyanka Shekar");
		m->declare("effect.lib/exciter_version", "1.0");
		m->declare("effect.lib/exciter_license", "MIT License (MIT)");
	}

	virtual int getNumInputs() 	{ return 2; }
	virtual int getNumOutputs() 	{ return 2; }
	static void classInit(int samplingFreq) {
	}
	virtual void instanceInit(int samplingFreq) {
		fSamplingFreq = samplingFreq;
		for (int i=0; i<4; i++) fYec0_perm[i]=0;
		fslider0 = 1.5e+03f;
		iConst0 = min(192000, max(1, fSamplingFreq));
		fConst1 = (3.141592653589793f / float(iConst0));
		for (int i=0; i<4; i++) fRec3_perm[i]=0;
		for (int i=0; i<4; i++) fRec2_perm[i]=0;
		for (int i=0; i<4; i++) fYec1_perm[i]=0;
		fslider1 = 166.0f;
		for (int i=0; i<4; i++) fRec1_perm[i]=0;
		for (int i=0; i<4; i++) fRec0_perm[i]=0;
		for (int i=0; i<4; i++) fYec2_perm[i]=0;
		for (int i=0; i<4; i++) fRec10_perm[i]=0;
		for (int i=0; i<4; i++) fRec9_perm[i]=0;
		for (int i=0; i<4; i++) fYec3_perm[i]=0;
		for (int i=0; i<4; i++) fRec8_perm[i]=0;
		for (int i=0; i<4; i++) fRec7_perm[i]=0;
		fcheckbox0 = 0.0;
		fslider2 = 0.0f;
		fConst2 = (1.0f / float(iConst0));
		fslider3 = 2e+02f;
		for (int i=0; i<4; i++) fRec6_perm[i]=0;
		fslider4 = 1e+02f;
		for (int i=0; i<4; i++) fRec5_perm[i]=0;
		fslider5 = -2e+01f;
		fConst3 = (2.0f / float(iConst0));
		fslider6 = 2.0f;
		for (int i=0; i<4; i++) fRec4_perm[i]=0;
		for (int i=0; i<4; i++) fRec12_perm[i]=0;
		for (int i=0; i<4; i++) fRec11_perm[i]=0;
		for (int i=0; i<4; i++) fRec17_perm[i]=0;
		for (int i=0; i<4; i++) fRec16_perm[i]=0;
		fcheckbox1 = 0.0;
		fslider7 = 0.0f;
		fslider8 = 2e+02f;
		for (int i=0; i<4; i++) fRec15_perm[i]=0;
		fslider9 = 1e+02f;
		for (int i=0; i<4; i++) fRec14_perm[i]=0;
		fslider10 = -2e+01f;
		fslider11 = 2.0f;
		for (int i=0; i<4; i++) fRec13_perm[i]=0;
		for (int i=0; i<4; i++) fRec20_perm[i]=0;
		for (int i=0; i<4; i++) fRec19_perm[i]=0;
		for (int i=0; i<4; i++) fRec18_perm[i]=0;
		for (int i=0; i<4; i++) fRec26_perm[i]=0;
		for (int i=0; i<4; i++) fRec25_perm[i]=0;
		for (int i=0; i<4; i++) fRec24_perm[i]=0;
		fcheckbox2 = 0.0;
		fslider12 = 0.0f;
		fslider13 = 2e+02f;
		for (int i=0; i<4; i++) fRec23_perm[i]=0;
		fslider14 = 1e+02f;
		for (int i=0; i<4; i++) fRec22_perm[i]=0;
		fslider15 = -2e+01f;
		fslider16 = 2.0f;
		for (int i=0; i<4; i++) fRec21_perm[i]=0;
		for (int i=0; i<4; i++) fRec32_perm[i]=0;
		for (int i=0; i<4; i++) fRec31_perm[i]=0;
		for (int i=0; i<4; i++) fRec30_perm[i]=0;
		for (int i=0; i<4; i++) fRec35_perm[i]=0;
		for (int i=0; i<4; i++) fRec34_perm[i]=0;
		for (int i=0; i<4; i++) fRec33_perm[i]=0;
		for (int i=0; i<4; i++) fRec38_perm[i]=0;
		for (int i=0; i<4; i++) fRec37_perm[i]=0;
		for (int i=0; i<4; i++) fRec36_perm[i]=0;
		fslider17 = 0.0f;
		fcheckbox3 = 0.0;
		fcheckbox4 = 0.0;
		fcheckbox5 = 0.0;
		fslider18 = 0.0f;
		fslider19 = 0.0f;
		fcheckbox6 = 0.0;
		fslider20 = 0.0f;
		fslider21 = 5e+02f;
		fConst4 = (1e+03f / float(iConst0));
		for (int i=0; i<4; i++) fRec29_perm[i]=0;
		fslider22 = 8e+02f;
		fConst5 = (1e+06f / float(iConst0));
		for (int i=0; i<4; i++) fRec28_perm[i]=0;
		fConst6 = (2e+06f / float(iConst0));
		fslider23 = 4.0f;
		for (int i=0; i<4; i++) fRec27_perm[i]=0;
		fslider24 = 0.0f;
		fslider25 = 0.0f;
	}
	virtual void init(int samplingFreq) {
		classInit(samplingFreq);
		instanceInit(samplingFreq);
	}
	virtual void buildUserInterface(UI* faust_interface) {
		faust_interface->openVerticalBox("0x00");
		faust_interface->declare(&fcheckbox5, "0", "");
		faust_interface->declare(&fcheckbox5, "tooltip", "When this is checked, the compressor is enabled. If not, sound is muted.");
		faust_interface->addCheckButton("Band 1:  Solo", &fcheckbox5);
		faust_interface->declare(&fbargraph5, "1", "");
		faust_interface->declare(&fbargraph5, "7", "");
		faust_interface->addHorizontalBargraph("Band 1:   Outgain", &fbargraph5, 0.0f, 1.0f);
		faust_interface->declare(&fcheckbox0, "0.5", "");
		faust_interface->declare(&fcheckbox0, "tooltip", "When this is checked, the compressor is enabled. If not, sound is muted.");
		faust_interface->addCheckButton("Band 1: Bypass", &fcheckbox0);
		faust_interface->declare(&fslider6, "2", "");
		faust_interface->declare(&fslider6, "style", "slider");
		faust_interface->declare(&fslider6, "tooltip", "A compression Ratio of N means that for each N dB increase in input signal level above Threshold, the output level goes up 1 dB");
		faust_interface->addHorizontalSlider("Band 1: Ratio", &fslider6, 2.0f, 1.0f, 2e+01f, 0.1f);
		faust_interface->declare(&fslider5, "3", "");
		faust_interface->declare(&fslider5, "style", "slider");
		faust_interface->declare(&fslider5, "tooltip", "When the signal level exceeds the Threshold (in dB), its level is compressed according to the Ratio");
		faust_interface->declare(&fslider5, "unit", "dB");
		faust_interface->addHorizontalSlider("Band 1: Threshold", &fslider5, -2e+01f, -1e+02f, 1e+01f, 0.1f);
		faust_interface->declare(&fslider4, "4", "");
		faust_interface->declare(&fslider4, "style", "slider");
		faust_interface->declare(&fslider4, "tooltip", "Time constant in ms (1/e smoothing time) for the compression gain to approach (exponentially) a new lower target level (the compression `kicking in')");
		faust_interface->declare(&fslider4, "unit", "ms");
		faust_interface->addHorizontalSlider("Band 1: Attack", &fslider4, 1e+02f, 0.0f, 5e+02f, 0.1f);
		faust_interface->declare(&fslider3, "5", "");
		faust_interface->declare(&fslider3, "style", "slider");
		faust_interface->declare(&fslider3, "tooltip", "Time constant in ms (1/e smoothing time) for the compression gain to approach (exponentially) a new higher target level (the compression 'releasing')");
		faust_interface->declare(&fslider3, "unit", "ms");
		faust_interface->addHorizontalSlider("Band 1: Release", &fslider3, 2e+02f, 0.0f, 1e+03f, 0.1f);
		faust_interface->declare(&fbargraph4, "1", "");
		faust_interface->declare(&fbargraph4, "6", "");
		faust_interface->declare(&fbargraph4, "tooltip", "dummy tooltip");
		faust_interface->addHorizontalBargraph("Band 1: Input Gain", &fbargraph4, 0.0f, 1.0f);
		faust_interface->declare(&fslider2, "2", "");
		faust_interface->declare(&fslider2, "6", "");
		faust_interface->declare(&fslider2, "tooltip", "The compressed-signal output level is increased by this amount (in dB) to make up for the level lost due to compression");
		faust_interface->declare(&fslider2, "unit", "dB");
		faust_interface->addHorizontalSlider("Band 1: Input Gain", &fslider2, 0.0f, -4e+01f, 4e+01f, 0.1f);
		faust_interface->declare(&fslider19, "2", "");
		faust_interface->declare(&fslider19, "7", "");
		faust_interface->declare(&fslider19, "tooltip", "The compressed-signal output level is increased by this amount (in dB) to make up for the level lost due to compression");
		faust_interface->declare(&fslider19, "unit", "dB");
		faust_interface->addHorizontalSlider("Band 1: Output Gain", &fslider19, 0.0f, -4e+01f, 4e+01f, 0.1f);
		faust_interface->declare(&fcheckbox4, "0", "");
		faust_interface->declare(&fcheckbox4, "tooltip", "When this is checked, the compressor is enabled. If not, sound is muted.");
		faust_interface->addCheckButton("Band 2:  Solo", &fcheckbox4);
		faust_interface->declare(&fbargraph3, "1", "");
		faust_interface->declare(&fbargraph3, "7", "");
		faust_interface->addHorizontalBargraph("Band 2:   Outgain", &fbargraph3, 0.0f, 1.0f);
		faust_interface->declare(&fcheckbox1, "0.5", "");
		faust_interface->declare(&fcheckbox1, "tooltip", "When this is checked, the compressor is enabled. If not, sound is muted.");
		faust_interface->addCheckButton("Band 2: Bypass", &fcheckbox1);
		faust_interface->declare(&fslider11, "2", "");
		faust_interface->declare(&fslider11, "style", "slider");
		faust_interface->declare(&fslider11, "tooltip", "A compression Ratio of N means that for each N dB increase in input signal level above Threshold, the output level goes up 1 dB");
		faust_interface->addHorizontalSlider("Band 2: Ratio", &fslider11, 2.0f, 1.0f, 2e+01f, 0.1f);
		faust_interface->declare(&fslider10, "3", "");
		faust_interface->declare(&fslider10, "style", "slider");
		faust_interface->declare(&fslider10, "tooltip", "When the signal level exceeds the Threshold (in dB), its level is compressed according to the Ratio");
		faust_interface->declare(&fslider10, "unit", "dB");
		faust_interface->addHorizontalSlider("Band 2: Threshold", &fslider10, -2e+01f, -1e+02f, 1e+01f, 0.1f);
		faust_interface->declare(&fslider9, "4", "");
		faust_interface->declare(&fslider9, "style", "slider");
		faust_interface->declare(&fslider9, "tooltip", "Time constant in ms (1/e smoothing time) for the compression gain to approach (exponentially) a new lower target level (the compression `kicking in')");
		faust_interface->declare(&fslider9, "unit", "ms");
		faust_interface->addHorizontalSlider("Band 2: Attack", &fslider9, 1e+02f, 0.0f, 5e+02f, 0.1f);
		faust_interface->declare(&fslider8, "5", "");
		faust_interface->declare(&fslider8, "style", "slider");
		faust_interface->declare(&fslider8, "tooltip", "Time constant in ms (1/e smoothing time) for the compression gain to approach (exponentially) a new higher target level (the compression 'releasing')");
		faust_interface->declare(&fslider8, "unit", "ms");
		faust_interface->addHorizontalSlider("Band 2: Release", &fslider8, 2e+02f, 0.0f, 1e+03f, 0.1f);
		faust_interface->declare(&fbargraph2, "1", "");
		faust_interface->declare(&fbargraph2, "6", "");
		faust_interface->declare(&fbargraph2, "tooltip", "dummy tooltip");
		faust_interface->addHorizontalBargraph("Band 2: Input Gain", &fbargraph2, 0.0f, 1.0f);
		faust_interface->declare(&fslider7, "2", "");
		faust_interface->declare(&fslider7, "6", "");
		faust_interface->declare(&fslider7, "tooltip", "The compressed-signal output level is increased by this amount (in dB) to make up for the level lost due to compression");
		faust_interface->declare(&fslider7, "unit", "dB");
		faust_interface->addHorizontalSlider("Band 2: Input Gain", &fslider7, 0.0f, -4e+01f, 4e+01f, 0.1f);
		faust_interface->declare(&fslider18, "2", "");
		faust_interface->declare(&fslider18, "7", "");
		faust_interface->declare(&fslider18, "tooltip", "The compressed-signal output level is increased by this amount (in dB) to make up for the level lost due to compression");
		faust_interface->declare(&fslider18, "unit", "dB");
		faust_interface->addHorizontalSlider("Band 2: Output Gain", &fslider18, 0.0f, -4e+01f, 4e+01f, 0.1f);
		faust_interface->declare(&fcheckbox3, "0", "");
		faust_interface->declare(&fcheckbox3, "tooltip", "When this is checked, the compressor is enabled. If not, sound is muted.");
		faust_interface->addCheckButton("Band 3:  Solo", &fcheckbox3);
		faust_interface->declare(&fbargraph1, "1", "");
		faust_interface->declare(&fbargraph1, "7", "");
		faust_interface->addHorizontalBargraph("Band 3:   Outgain", &fbargraph1, 0.0f, 1.0f);
		faust_interface->declare(&fcheckbox2, "0.5", "");
		faust_interface->declare(&fcheckbox2, "tooltip", "When this is checked, the compressor is enabled. If not, sound is muted.");
		faust_interface->addCheckButton("Band 3: Bypass", &fcheckbox2);
		faust_interface->declare(&fslider16, "2", "");
		faust_interface->declare(&fslider16, "style", "slider");
		faust_interface->declare(&fslider16, "tooltip", "A compression Ratio of N means that for each N dB increase in input signal level above Threshold, the output level goes up 1 dB");
		faust_interface->addHorizontalSlider("Band 3: Ratio", &fslider16, 2.0f, 1.0f, 2e+01f, 0.1f);
		faust_interface->declare(&fslider15, "3", "");
		faust_interface->declare(&fslider15, "style", "slider");
		faust_interface->declare(&fslider15, "tooltip", "When the signal level exceeds the Threshold (in dB), its level is compressed according to the Ratio");
		faust_interface->declare(&fslider15, "unit", "dB");
		faust_interface->addHorizontalSlider("Band 3: Threshold", &fslider15, -2e+01f, -1e+02f, 1e+01f, 0.1f);
		faust_interface->declare(&fslider14, "4", "");
		faust_interface->declare(&fslider14, "style", "slider");
		faust_interface->declare(&fslider14, "tooltip", "Time constant in ms (1/e smoothing time) for the compression gain to approach (exponentially) a new lower target level (the compression `kicking in')");
		faust_interface->declare(&fslider14, "unit", "ms");
		faust_interface->addHorizontalSlider("Band 3: Attack", &fslider14, 1e+02f, 0.0f, 5e+02f, 0.1f);
		faust_interface->declare(&fslider13, "5", "");
		faust_interface->declare(&fslider13, "style", "slider");
		faust_interface->declare(&fslider13, "tooltip", "Time constant in ms (1/e smoothing time) for the compression gain to approach (exponentially) a new higher target level (the compression 'releasing')");
		faust_interface->declare(&fslider13, "unit", "ms");
		faust_interface->addHorizontalSlider("Band 3: Release", &fslider13, 2e+02f, 0.0f, 1e+03f, 0.1f);
		faust_interface->declare(&fbargraph0, "1", "");
		faust_interface->declare(&fbargraph0, "6", "");
		faust_interface->declare(&fbargraph0, "tooltip", "dummy tooltip");
		faust_interface->addHorizontalBargraph("Band 3: Input Gain", &fbargraph0, 0.0f, 1.0f);
		faust_interface->declare(&fslider12, "2", "");
		faust_interface->declare(&fslider12, "6", "");
		faust_interface->declare(&fslider12, "tooltip", "The compressed-signal output level is increased by this amount (in dB) to make up for the level lost due to compression");
		faust_interface->declare(&fslider12, "unit", "dB");
		faust_interface->addHorizontalSlider("Band 3: Input Gain", &fslider12, 0.0f, -4e+01f, 4e+01f, 0.1f);
		faust_interface->declare(&fslider17, "2", "");
		faust_interface->declare(&fslider17, "7", "");
		faust_interface->declare(&fslider17, "tooltip", "The compressed-signal output level is increased by this amount (in dB) to make up for the level lost due to compression");
		faust_interface->declare(&fslider17, "unit", "dB");
		faust_interface->addHorizontalSlider("Band 3: Output Gain", &fslider17, 0.0f, -4e+01f, 4e+01f, 0.1f);
		faust_interface->declare(&fslider1, "C", "");
		faust_interface->declare(&fslider1, "style", "knob");
		faust_interface->declare(&fslider1, "tooltip", "Center-frequency of second-order Regalia-Mitra peaking equalizer section 1");
		faust_interface->declare(&fslider1, "unit", "Hz");
		faust_interface->addVerticalSlider("Split Freq 1", &fslider1, 166.0f, 4e+01f, 999.0f, 1.0f);
		faust_interface->declare(&fslider0, "D", "");
		faust_interface->declare(&fslider0, "style", "knob");
		faust_interface->declare(&fslider0, "tooltip", "Center-frequency of second-order Regalia-Mitra peaking equalizer section 1");
		faust_interface->declare(&fslider0, "unit", "Hz");
		faust_interface->addVerticalSlider("Split Freq 2", &fslider0, 1.5e+03f, 1e+03f, 1.5e+04f, 1.0f);
		faust_interface->declare(&fcheckbox6, "E", "");
		faust_interface->addCheckButton("Limiter Bypass", &fcheckbox6);
		faust_interface->declare(&fslider20, "F", "");
		faust_interface->declare(&fslider20, "tooltip", "Adjust overall gain.");
		faust_interface->declare(&fslider20, "unit", "dB");
		faust_interface->addHorizontalSlider("Limiter Input Gain", &fslider20, 0.0f, -4e+01f, 4e+01f, 0.1f);
		faust_interface->declare(&fslider23, "G", "");
		faust_interface->declare(&fslider23, "unit", ":1");
		faust_interface->addVerticalSlider("Limiter Ratio", &fslider23, 4.0f, 4.0f, 2e+01f, 1.0f);
		faust_interface->declare(&fslider22, "H", "");
		faust_interface->declare(&fslider22, "unit", "us");
		faust_interface->addVerticalSlider("Limiter Attack", &fslider22, 8e+02f, 2e+01f, 8e+02f, 1.0f);
		faust_interface->declare(&fslider21, "I", "");
		faust_interface->declare(&fslider21, "unit", "ms");
		faust_interface->addVerticalSlider("Limiter Release", &fslider21, 5e+02f, 5e+01f, 1.1e+03f, 1.0f);
		faust_interface->declare(&fslider24, "J", "");
		faust_interface->declare(&fslider24, "tooltip", "Adjust overall gain.");
		faust_interface->declare(&fslider24, "unit", "dB");
		faust_interface->addHorizontalSlider("Limiter Output Gain", &fslider24, 0.0f, -4e+01f, 4e+01f, 0.1f);
		faust_interface->declare(&fslider25, "K", "");
		faust_interface->declare(&fslider25, "tooltip", "Adjust overall gain.");
		faust_interface->declare(&fslider25, "unit", "dB");
		faust_interface->addHorizontalSlider("Final Output Gain", &fslider25, 0.0f, -4e+01f, 4e+01f, 0.1f);
		faust_interface->closeBox();
	}
	virtual void compute (int count, FAUSTFLOAT** input, FAUSTFLOAT** output) {
		float 	fYec0_tmp[32+4];
		float 	fRec3_tmp[32+4];
		float 	fRec2_tmp[32+4];
		float 	fZec0[32];
		float 	fYec1_tmp[32+4];
		float 	fRec1_tmp[32+4];
		float 	fRec0_tmp[32+4];
		float 	fYec2_tmp[32+4];
		float 	fRec10_tmp[32+4];
		float 	fRec9_tmp[32+4];
		float 	fZec1[32];
		float 	fYec3_tmp[32+4];
		float 	fRec8_tmp[32+4];
		float 	fRec7_tmp[32+4];
		float 	fZec2[32];
		float 	fZec3[32];
		float 	fZec4[32];
		float 	fZec5[32];
		float 	fZec6[32];
		float 	fRec6_tmp[32+4];
		float 	fRec5_tmp[32+4];
		float 	fRec4_tmp[32+4];
		float 	fRec12_tmp[32+4];
		float 	fRec11_tmp[32+4];
		float 	fRec17_tmp[32+4];
		float 	fRec16_tmp[32+4];
		float 	fZec7[32];
		float 	fZec8[32];
		float 	fZec9[32];
		float 	fZec10[32];
		float 	fZec11[32];
		float 	fRec15_tmp[32+4];
		float 	fRec14_tmp[32+4];
		float 	fRec13_tmp[32+4];
		float 	fRec20_tmp[32+4];
		float 	fRec19_tmp[32+4];
		float 	fZec12[32];
		float 	fRec18_tmp[32+4];
		float 	fRec26_tmp[32+4];
		float 	fRec25_tmp[32+4];
		float 	fZec13[32];
		float 	fRec24_tmp[32+4];
		float 	fZec14[32];
		float 	fZec15[32];
		float 	fZec16[32];
		float 	fZec17[32];
		float 	fZec18[32];
		float 	fRec23_tmp[32+4];
		float 	fRec22_tmp[32+4];
		float 	fRec21_tmp[32+4];
		float 	fZec19[32];
		float 	fZec20[32];
		float 	fZec21[32];
		float 	fZec22[32];
		float 	fRec32_tmp[32+4];
		float 	fRec31_tmp[32+4];
		float 	fRec30_tmp[32+4];
		float 	fZec23[32];
		float 	fZec24[32];
		float 	fZec25[32];
		float 	fZec26[32];
		float 	fRec35_tmp[32+4];
		float 	fRec34_tmp[32+4];
		float 	fRec33_tmp[32+4];
		float 	fZec27[32];
		float 	fZec28[32];
		float 	fZec29[32];
		float 	fZec30[32];
		float 	fRec38_tmp[32+4];
		float 	fRec37_tmp[32+4];
		float 	fRec36_tmp[32+4];
		float 	fZec31[32];
		float 	fZec32[32];
		float 	fZec33[32];
		float 	fZec34[32];
		float 	fZec35[32];
		float 	fZec36[32];
		float 	fZec37[32];
		float 	fZec38[32];
		float 	fZec39[32];
		float 	fZec40[32];
		float 	fZec41[32];
		float 	fRec29_tmp[32+4];
		float 	fRec28_tmp[32+4];
		float 	fRec27_tmp[32+4];
		float 	fZec42[32];
		float* 	fYec0 = &fYec0_tmp[4];
		float 	fSlow0 = tanf((fConst1 * float(fslider0)));
		float 	fSlow1 = (1.0f / fSlow0);
		float 	fSlow2 = (1 + fSlow1);
		float 	fSlow3 = (1.0f / fSlow2);
		float 	fSlow4 = (0 - ((1 - fSlow1) / fSlow2));
		float* 	fRec3 = &fRec3_tmp[4];
		float 	fSlow5 = (1.0f / faustpower<2>(fSlow0));
		float 	fSlow6 = (2 * (1 - fSlow5));
		float 	fSlow7 = (1 + ((fSlow1 - 1.0000000000000004f) / fSlow0));
		float 	fSlow8 = (1 + ((1.0000000000000004f + fSlow1) / fSlow0));
		float 	fSlow9 = (1.0f / fSlow8);
		float* 	fRec2 = &fRec2_tmp[4];
		float* 	fYec1 = &fYec1_tmp[4];
		float 	fSlow10 = tanf((fConst1 * float(fslider1)));
		float 	fSlow11 = (1.0f / fSlow10);
		float 	fSlow12 = (1 + fSlow11);
		float 	fSlow13 = (1.0f / fSlow12);
		float 	fSlow14 = (0 - ((1 - fSlow11) / fSlow12));
		float* 	fRec1 = &fRec1_tmp[4];
		float 	fSlow15 = (1.0f / faustpower<2>(fSlow10));
		float 	fSlow16 = (2 * (1 - fSlow15));
		float 	fSlow17 = (1 + ((fSlow11 - 1.0000000000000004f) / fSlow10));
		float 	fSlow18 = (1.0f / (1 + ((fSlow11 + 1.0000000000000004f) / fSlow10)));
		float* 	fRec0 = &fRec0_tmp[4];
		float* 	fYec2 = &fYec2_tmp[4];
		float* 	fRec10 = &fRec10_tmp[4];
		float* 	fRec9 = &fRec9_tmp[4];
		float* 	fYec3 = &fYec3_tmp[4];
		float* 	fRec8 = &fRec8_tmp[4];
		float* 	fRec7 = &fRec7_tmp[4];
		int 	iSlow19 = int(float(fcheckbox0));
		float 	fSlow20 = powf(10,(0.05f * float(fslider2)));
		float 	fSlow21 = expf((0 - (fConst2 / max(fConst2, (0.001f * float(fslider3))))));
		float 	fSlow22 = (1.0f - fSlow21);
		float* 	fRec6 = &fRec6_tmp[4];
		float 	fSlow23 = max(fConst2, (0.001f * float(fslider4)));
		float 	fSlow24 = expf((0 - (fConst2 / fSlow23)));
		float 	fSlow25 = (1.0f - fSlow24);
		float* 	fRec5 = &fRec5_tmp[4];
		float 	fSlow26 = float(fslider5);
		float 	fSlow27 = expf((0 - (fConst3 / fSlow23)));
		float 	fSlow28 = (((1.0f / float(float(fslider6))) - 1.0f) * (1.0f - fSlow27));
		float* 	fRec4 = &fRec4_tmp[4];
		float 	fSlow29 = (0 - fSlow11);
		float 	fSlow30 = (1.0f / (fSlow10 * fSlow8));
		float* 	fRec12 = &fRec12_tmp[4];
		float* 	fRec11 = &fRec11_tmp[4];
		float* 	fRec17 = &fRec17_tmp[4];
		float* 	fRec16 = &fRec16_tmp[4];
		float 	fSlow31 = (2 * (0 - fSlow15));
		int 	iSlow32 = int(float(fcheckbox1));
		float 	fSlow33 = powf(10,(0.05f * float(fslider7)));
		float 	fSlow34 = expf((0 - (fConst2 / max(fConst2, (0.001f * float(fslider8))))));
		float 	fSlow35 = (1.0f - fSlow34);
		float* 	fRec15 = &fRec15_tmp[4];
		float 	fSlow36 = max(fConst2, (0.001f * float(fslider9)));
		float 	fSlow37 = expf((0 - (fConst2 / fSlow36)));
		float 	fSlow38 = (1.0f - fSlow37);
		float* 	fRec14 = &fRec14_tmp[4];
		float 	fSlow39 = float(fslider10);
		float 	fSlow40 = expf((0 - (fConst3 / fSlow36)));
		float 	fSlow41 = (((1.0f / float(float(fslider11))) - 1.0f) * (1.0f - fSlow40));
		float* 	fRec13 = &fRec13_tmp[4];
		float 	fSlow42 = (0 - fSlow1);
		float* 	fRec20 = &fRec20_tmp[4];
		float* 	fRec19 = &fRec19_tmp[4];
		float 	fSlow43 = (1 + ((fSlow11 - 1.0f) / fSlow10));
		float 	fSlow44 = (1.0f / (1 + ((1.0f + fSlow11) / fSlow10)));
		float 	fSlow45 = (2 * (0 - fSlow5));
		float* 	fRec18 = &fRec18_tmp[4];
		float* 	fRec26 = &fRec26_tmp[4];
		float* 	fRec25 = &fRec25_tmp[4];
		float* 	fRec24 = &fRec24_tmp[4];
		int 	iSlow46 = int(float(fcheckbox2));
		float 	fSlow47 = powf(10,(0.05f * float(fslider12)));
		float 	fSlow48 = expf((0 - (fConst2 / max(fConst2, (0.001f * float(fslider13))))));
		float 	fSlow49 = (1.0f - fSlow48);
		float* 	fRec23 = &fRec23_tmp[4];
		float 	fSlow50 = max(fConst2, (0.001f * float(fslider14)));
		float 	fSlow51 = expf((0 - (fConst2 / fSlow50)));
		float 	fSlow52 = (1.0f - fSlow51);
		float* 	fRec22 = &fRec22_tmp[4];
		float 	fSlow53 = float(fslider15);
		float 	fSlow54 = expf((0 - (fConst3 / fSlow50)));
		float 	fSlow55 = (((1.0f / float(float(fslider16))) - 1.0f) * (1.0f - fSlow54));
		float* 	fRec21 = &fRec21_tmp[4];
		float* 	fRec32 = &fRec32_tmp[4];
		float* 	fRec31 = &fRec31_tmp[4];
		float* 	fRec30 = &fRec30_tmp[4];
		float* 	fRec35 = &fRec35_tmp[4];
		float* 	fRec34 = &fRec34_tmp[4];
		float* 	fRec33 = &fRec33_tmp[4];
		float* 	fRec38 = &fRec38_tmp[4];
		float* 	fRec37 = &fRec37_tmp[4];
		float* 	fRec36 = &fRec36_tmp[4];
		float 	fSlow56 = powf(10,(0.05f * float(fslider17)));
		int 	iSlow57 = int(float(fcheckbox3));
		int 	iSlow58 = int(float(fcheckbox4));
		int 	iSlow59 = int(float(fcheckbox5));
		int 	iSlow60 = (((iSlow59 == 0) & (iSlow58 == 0)) & (iSlow57 == 0));
		float 	fSlow61 = float((iSlow57 | iSlow60));
		float 	fSlow62 = (fSlow61 * fSlow56);
		float 	fSlow63 = ((fSlow61 * fSlow47) * fSlow56);
		float 	fSlow64 = powf(10,(0.05f * float(fslider18)));
		float 	fSlow65 = float((iSlow58 | iSlow60));
		float 	fSlow66 = (fSlow65 * fSlow64);
		float 	fSlow67 = ((fSlow65 * fSlow33) * fSlow64);
		float 	fSlow68 = powf(10,(0.05f * float(fslider19)));
		float 	fSlow69 = float((iSlow59 | iSlow60));
		float 	fSlow70 = (fSlow69 * fSlow68);
		float 	fSlow71 = ((fSlow69 * fSlow20) * fSlow68);
		int 	iSlow72 = int(float(fcheckbox6));
		float 	fSlow73 = pun_int_to_float((8388608 * (126.94269504f + max(-126.0f, (0.16609640464202244f * float(fslider20))))));
		float 	fSlow74 = expf((0 - (fConst4 / float(fslider21))));
		float 	fSlow75 = (1.0f - fSlow74);
		float* 	fRec29 = &fRec29_tmp[4];
		float 	fSlow76 = float(fslider22);
		float 	fSlow77 = expf((0 - (fConst5 / fSlow76)));
		float 	fSlow78 = (1.0f - fSlow77);
		float* 	fRec28 = &fRec28_tmp[4];
		float 	fSlow79 = expf((0 - (fConst6 / fSlow76)));
		float 	fSlow80 = (((1.0f / float(float(fslider23))) - 1.0f) * (1.0f - fSlow79));
		float* 	fRec27 = &fRec27_tmp[4];
		float 	fSlow81 = (fSlow73 * pun_int_to_float((8388608 * (126.94269504f + max(-126.0f, (0.16609640464202244f * float(fslider24)))))));
		float 	fSlow82 = powf(10,(0.05f * float(fslider25)));
		int index;
		int fullcount = count;
		for (index = 0; index <= fullcount - 32; index += 32) {
			// compute by blocks of 32 samples
			const int count = 32;
			FAUSTFLOAT* input0 = &input[0][index];
			FAUSTFLOAT* input1 = &input[1][index];
			FAUSTFLOAT* output0 = &output[0][index];
			FAUSTFLOAT* output1 = &output[1][index];
			// SECTION : 1
			// LOOP 0x27862c0
			// pre processing
			for (int i=0; i<4; i++) fYec0_tmp[i]=fYec0_perm[i];
			// exec code
			for (int i=0; i<count; i++) {
				fYec0[i] = (float)input0[i];
			}
			// post processing
			for (int i=0; i<4; i++) fYec0_perm[i]=fYec0_tmp[count+i];
			
			// LOOP 0x27916c0
			// pre processing
			for (int i=0; i<4; i++) fYec2_tmp[i]=fYec2_perm[i];
			// exec code
			for (int i=0; i<count; i++) {
				fYec2[i] = (float)input1[i];
			}
			// post processing
			for (int i=0; i<4; i++) fYec2_perm[i]=fYec2_tmp[count+i];
			
			// SECTION : 2
			// LOOP 0x2786020
			// pre processing
			for (int i=0; i<4; i++) fRec3_tmp[i]=fRec3_perm[i];
			// exec code
			for (int i=0; i<count; i++) {
				fRec3[i] = ((fSlow4 * fRec3[i-1]) + (fSlow3 * ((float)input0[i] + fYec0[i-1])));
			}
			// post processing
			for (int i=0; i<4; i++) fRec3_perm[i]=fRec3_tmp[count+i];
			
			// LOOP 0x2791380
			// pre processing
			for (int i=0; i<4; i++) fRec10_tmp[i]=fRec10_perm[i];
			// exec code
			for (int i=0; i<count; i++) {
				fRec10[i] = ((fSlow4 * fRec10[i-1]) + (fSlow3 * ((float)input1[i] + fYec2[i-1])));
			}
			// post processing
			for (int i=0; i<4; i++) fRec10_perm[i]=fRec10_tmp[count+i];
			
			// SECTION : 3
			// LOOP 0x2785cc0
			// pre processing
			for (int i=0; i<4; i++) fRec2_tmp[i]=fRec2_perm[i];
			// exec code
			for (int i=0; i<count; i++) {
				fRec2[i] = (fRec3[i] - (fSlow9 * ((fSlow7 * fRec2[i-2]) + (fSlow6 * fRec2[i-1]))));
			}
			// post processing
			for (int i=0; i<4; i++) fRec2_perm[i]=fRec2_tmp[count+i];
			
			// LOOP 0x2790f60
			// pre processing
			for (int i=0; i<4; i++) fRec9_tmp[i]=fRec9_perm[i];
			// exec code
			for (int i=0; i<count; i++) {
				fRec9[i] = (fRec10[i] - (fSlow9 * ((fSlow7 * fRec9[i-2]) + (fSlow6 * fRec9[i-1]))));
			}
			// post processing
			for (int i=0; i<4; i++) fRec9_perm[i]=fRec9_tmp[count+i];
			
			// SECTION : 4
			// LOOP 0x278b930
			// exec code
			for (int i=0; i<count; i++) {
				fZec0[i] = (fRec2[i-2] + (fRec2[i] + (2 * fRec2[i-1])));
			}
			
			// LOOP 0x27942b0
			// exec code
			for (int i=0; i<count; i++) {
				fZec1[i] = (fRec9[i-2] + (fRec9[i] + (2 * fRec9[i-1])));
			}
			
			// SECTION : 5
			// LOOP 0x278b810
			// pre processing
			for (int i=0; i<4; i++) fYec1_tmp[i]=fYec1_perm[i];
			// exec code
			for (int i=0; i<count; i++) {
				fYec1[i] = (fSlow9 * fZec0[i]);
			}
			// post processing
			for (int i=0; i<4; i++) fYec1_perm[i]=fYec1_tmp[count+i];
			
			// LOOP 0x2794190
			// pre processing
			for (int i=0; i<4; i++) fYec3_tmp[i]=fYec3_perm[i];
			// exec code
			for (int i=0; i<count; i++) {
				fYec3[i] = (fSlow9 * fZec1[i]);
			}
			// post processing
			for (int i=0; i<4; i++) fYec3_perm[i]=fYec3_tmp[count+i];
			
			// LOOP 0x27b2710
			// pre processing
			for (int i=0; i<4; i++) fRec20_tmp[i]=fRec20_perm[i];
			// exec code
			for (int i=0; i<count; i++) {
				fRec20[i] = ((fSlow4 * fRec20[i-1]) + (fSlow3 * ((fSlow1 * (float)input0[i]) + (fSlow42 * fYec0[i-1]))));
			}
			// post processing
			for (int i=0; i<4; i++) fRec20_perm[i]=fRec20_tmp[count+i];
			
			// LOOP 0x27b8360
			// pre processing
			for (int i=0; i<4; i++) fRec26_tmp[i]=fRec26_perm[i];
			// exec code
			for (int i=0; i<count; i++) {
				fRec26[i] = ((fSlow4 * fRec26[i-1]) + (fSlow3 * ((fSlow1 * (float)input1[i]) + (fSlow42 * fYec2[i-1]))));
			}
			// post processing
			for (int i=0; i<4; i++) fRec26_perm[i]=fRec26_tmp[count+i];
			
			// SECTION : 6
			// LOOP 0x2785960
			// pre processing
			for (int i=0; i<4; i++) fRec1_tmp[i]=fRec1_perm[i];
			// exec code
			for (int i=0; i<count; i++) {
				fRec1[i] = ((fSlow14 * fRec1[i-1]) + (fSlow13 * (fYec1[i] + fYec1[i-1])));
			}
			// post processing
			for (int i=0; i<4; i++) fRec1_perm[i]=fRec1_tmp[count+i];
			
			// LOOP 0x2790ac0
			// pre processing
			for (int i=0; i<4; i++) fRec8_tmp[i]=fRec8_perm[i];
			// exec code
			for (int i=0; i<count; i++) {
				fRec8[i] = ((fSlow14 * fRec8[i-1]) + (fSlow13 * (fYec3[i] + fYec3[i-1])));
			}
			// post processing
			for (int i=0; i<4; i++) fRec8_perm[i]=fRec8_tmp[count+i];
			
			// LOOP 0x27a0f70
			// pre processing
			for (int i=0; i<4; i++) fRec12_tmp[i]=fRec12_perm[i];
			// exec code
			for (int i=0; i<count; i++) {
				fRec12[i] = ((fSlow14 * fRec12[i-1]) + (fSlow13 * ((fSlow30 * fZec0[i]) + (fSlow29 * fYec1[i-1]))));
			}
			// post processing
			for (int i=0; i<4; i++) fRec12_perm[i]=fRec12_tmp[count+i];
			
			// LOOP 0x27a46a0
			// pre processing
			for (int i=0; i<4; i++) fRec17_tmp[i]=fRec17_perm[i];
			// exec code
			for (int i=0; i<count; i++) {
				fRec17[i] = ((fSlow14 * fRec17[i-1]) + (fSlow13 * ((fSlow30 * fZec1[i]) + (fSlow29 * fYec3[i-1]))));
			}
			// post processing
			for (int i=0; i<4; i++) fRec17_perm[i]=fRec17_tmp[count+i];
			
			// LOOP 0x27b1ef0
			// pre processing
			for (int i=0; i<4; i++) fRec19_tmp[i]=fRec19_perm[i];
			// exec code
			for (int i=0; i<count; i++) {
				fRec19[i] = (fRec20[i] - (fSlow9 * ((fSlow7 * fRec19[i-2]) + (fSlow6 * fRec19[i-1]))));
			}
			// post processing
			for (int i=0; i<4; i++) fRec19_perm[i]=fRec19_tmp[count+i];
			
			// LOOP 0x27b7f40
			// pre processing
			for (int i=0; i<4; i++) fRec25_tmp[i]=fRec25_perm[i];
			// exec code
			for (int i=0; i<count; i++) {
				fRec25[i] = (fRec26[i] - (fSlow9 * ((fSlow7 * fRec25[i-2]) + (fSlow6 * fRec25[i-1]))));
			}
			// post processing
			for (int i=0; i<4; i++) fRec25_perm[i]=fRec25_tmp[count+i];
			
			// SECTION : 7
			// LOOP 0x2785600
			// pre processing
			for (int i=0; i<4; i++) fRec0_tmp[i]=fRec0_perm[i];
			// exec code
			for (int i=0; i<count; i++) {
				fRec0[i] = (fRec1[i] - (fSlow18 * ((fSlow17 * fRec0[i-2]) + (fSlow16 * fRec0[i-1]))));
			}
			// post processing
			for (int i=0; i<4; i++) fRec0_perm[i]=fRec0_tmp[count+i];
			
			// LOOP 0x27906a0
			// pre processing
			for (int i=0; i<4; i++) fRec7_tmp[i]=fRec7_perm[i];
			// exec code
			for (int i=0; i<count; i++) {
				fRec7[i] = (fRec8[i] - (fSlow18 * ((fSlow17 * fRec7[i-2]) + (fSlow16 * fRec7[i-1]))));
			}
			// post processing
			for (int i=0; i<4; i++) fRec7_perm[i]=fRec7_tmp[count+i];
			
			// LOOP 0x27a0cd0
			// pre processing
			for (int i=0; i<4; i++) fRec11_tmp[i]=fRec11_perm[i];
			// exec code
			for (int i=0; i<count; i++) {
				fRec11[i] = (fRec12[i] - (fSlow18 * ((fSlow17 * fRec11[i-2]) + (fSlow16 * fRec11[i-1]))));
			}
			// post processing
			for (int i=0; i<4; i++) fRec11_perm[i]=fRec11_tmp[count+i];
			
			// LOOP 0x27a4340
			// pre processing
			for (int i=0; i<4; i++) fRec16_tmp[i]=fRec16_perm[i];
			// exec code
			for (int i=0; i<count; i++) {
				fRec16[i] = (fRec17[i] - (fSlow18 * ((fSlow17 * fRec16[i-2]) + (fSlow16 * fRec16[i-1]))));
			}
			// post processing
			for (int i=0; i<4; i++) fRec16_perm[i]=fRec16_tmp[count+i];
			
			// LOOP 0x27b25f0
			// pre processing
			for (int i=0; i<4; i++) fRec18_tmp[i]=fRec18_perm[i];
			// exec code
			for (int i=0; i<count; i++) {
				fZec12[i] = (fSlow16 * fRec18[i-1]);
				fRec18[i] = ((fSlow9 * (((fSlow5 * fRec19[i]) + (fSlow45 * fRec19[i-1])) + (fSlow5 * fRec19[i-2]))) - (fSlow44 * ((fSlow43 * fRec18[i-2]) + fZec12[i])));
			}
			// post processing
			for (int i=0; i<4; i++) fRec18_perm[i]=fRec18_tmp[count+i];
			
			// LOOP 0x27b7aa0
			// pre processing
			for (int i=0; i<4; i++) fRec24_tmp[i]=fRec24_perm[i];
			// exec code
			for (int i=0; i<count; i++) {
				fZec13[i] = (fSlow16 * fRec24[i-1]);
				fRec24[i] = ((fSlow9 * (((fSlow5 * fRec25[i]) + (fSlow45 * fRec25[i-1])) + (fSlow5 * fRec25[i-2]))) - (fSlow44 * ((fSlow43 * fRec24[i-2]) + fZec13[i])));
			}
			// post processing
			for (int i=0; i<4; i++) fRec24_perm[i]=fRec24_tmp[count+i];
			
			// SECTION : 8
			// LOOP 0x2796e30
			// exec code
			for (int i=0; i<count; i++) {
				fZec2[i] = (fSlow18 * (fRec7[i-2] + (fRec7[i] + (2 * fRec7[i-1]))));
			}
			
			// LOOP 0x27988a0
			// exec code
			for (int i=0; i<count; i++) {
				fZec4[i] = (fSlow18 * (fRec0[i-2] + (fRec0[i] + (2 * fRec0[i-1]))));
			}
			
			// LOOP 0x27a6e10
			// exec code
			for (int i=0; i<count; i++) {
				fZec7[i] = (fSlow18 * (((fSlow15 * fRec16[i]) + (fSlow31 * fRec16[i-1])) + (fSlow15 * fRec16[i-2])));
			}
			
			// LOOP 0x27a9690
			// exec code
			for (int i=0; i<count; i++) {
				fZec9[i] = (fSlow18 * (((fSlow15 * fRec11[i]) + (fSlow31 * fRec11[i-1])) + (fSlow15 * fRec11[i-2])));
			}
			
			// LOOP 0x27bc7b0
			// exec code
			for (int i=0; i<count; i++) {
				fZec14[i] = (fRec24[i-2] + (fSlow44 * (fZec13[i] + (fSlow43 * fRec24[i]))));
			}
			
			// LOOP 0x27bedc0
			// exec code
			for (int i=0; i<count; i++) {
				fZec16[i] = (fRec18[i-2] + (fSlow44 * (fZec12[i] + (fSlow43 * fRec18[i]))));
			}
			
			// SECTION : 9
			// LOOP 0x2796d10
			// exec code
			for (int i=0; i<count; i++) {
				fZec3[i] = ((iSlow19)?0:fZec2[i]);
			}
			
			// LOOP 0x2798780
			// exec code
			for (int i=0; i<count; i++) {
				fZec5[i] = ((iSlow19)?0:fZec4[i]);
			}
			
			// LOOP 0x27a6cf0
			// exec code
			for (int i=0; i<count; i++) {
				fZec8[i] = ((iSlow32)?0:fZec7[i]);
			}
			
			// LOOP 0x27a9570
			// exec code
			for (int i=0; i<count; i++) {
				fZec10[i] = ((iSlow32)?0:fZec9[i]);
			}
			
			// LOOP 0x27bc690
			// exec code
			for (int i=0; i<count; i++) {
				fZec15[i] = ((iSlow46)?0:fZec14[i]);
			}
			
			// LOOP 0x27beca0
			// exec code
			for (int i=0; i<count; i++) {
				fZec17[i] = ((iSlow46)?0:fZec16[i]);
			}
			
			// SECTION : 10
			// LOOP 0x2796bf0
			// exec code
			for (int i=0; i<count; i++) {
				fZec6[i] = fabsf((fabsf((fSlow20 * fZec5[i])) + fabsf((fSlow20 * fZec3[i]))));
			}
			
			// LOOP 0x27a6bd0
			// exec code
			for (int i=0; i<count; i++) {
				fZec11[i] = fabsf((fabsf((fSlow33 * fZec10[i])) + fabsf((fSlow33 * fZec8[i]))));
			}
			
			// LOOP 0x27bc570
			// exec code
			for (int i=0; i<count; i++) {
				fZec18[i] = fabsf((fabsf((fSlow47 * fZec17[i])) + fabsf((fSlow47 * fZec15[i]))));
			}
			
			// SECTION : 11
			// LOOP 0x27901d0
			// pre processing
			for (int i=0; i<4; i++) fRec6_tmp[i]=fRec6_perm[i];
			// exec code
			for (int i=0; i<count; i++) {
				fRec6[i] = max(fZec6[i], ((fSlow21 * fRec6[i-1]) + (fSlow22 * fZec6[i])));
			}
			// post processing
			for (int i=0; i<4; i++) fRec6_perm[i]=fRec6_tmp[count+i];
			
			// LOOP 0x27a3f20
			// pre processing
			for (int i=0; i<4; i++) fRec15_tmp[i]=fRec15_perm[i];
			// exec code
			for (int i=0; i<count; i++) {
				fRec15[i] = max(fZec11[i], ((fSlow34 * fRec15[i-1]) + (fSlow35 * fZec11[i])));
			}
			// post processing
			for (int i=0; i<4; i++) fRec15_perm[i]=fRec15_tmp[count+i];
			
			// LOOP 0x27b6eb0
			// pre processing
			for (int i=0; i<4; i++) fRec23_tmp[i]=fRec23_perm[i];
			// exec code
			for (int i=0; i<count; i++) {
				fRec23[i] = max(fZec18[i], ((fSlow48 * fRec23[i-1]) + (fSlow49 * fZec18[i])));
			}
			// post processing
			for (int i=0; i<4; i++) fRec23_perm[i]=fRec23_tmp[count+i];
			
			// SECTION : 12
			// LOOP 0x278fd50
			// pre processing
			for (int i=0; i<4; i++) fRec5_tmp[i]=fRec5_perm[i];
			// exec code
			for (int i=0; i<count; i++) {
				fRec5[i] = ((fSlow24 * fRec5[i-1]) + (fSlow25 * fRec6[i]));
			}
			// post processing
			for (int i=0; i<4; i++) fRec5_perm[i]=fRec5_tmp[count+i];
			
			// LOOP 0x27a3b60
			// pre processing
			for (int i=0; i<4; i++) fRec14_tmp[i]=fRec14_perm[i];
			// exec code
			for (int i=0; i<count; i++) {
				fRec14[i] = ((fSlow37 * fRec14[i-1]) + (fSlow38 * fRec15[i]));
			}
			// post processing
			for (int i=0; i<4; i++) fRec14_perm[i]=fRec14_tmp[count+i];
			
			// LOOP 0x27b6c20
			// pre processing
			for (int i=0; i<4; i++) fRec22_tmp[i]=fRec22_perm[i];
			// exec code
			for (int i=0; i<count; i++) {
				fRec22[i] = ((fSlow51 * fRec22[i-1]) + (fSlow52 * fRec23[i]));
			}
			// post processing
			for (int i=0; i<4; i++) fRec22_perm[i]=fRec22_tmp[count+i];
			
			// SECTION : 13
			// LOOP 0x278fa90
			// pre processing
			for (int i=0; i<4; i++) fRec4_tmp[i]=fRec4_perm[i];
			// exec code
			for (int i=0; i<count; i++) {
				fRec4[i] = ((fSlow27 * fRec4[i-1]) + (fSlow28 * max(((8.685889638065037f * ((8.262958288192749e-08f * float(pun_float_to_int(fRec5[i]))) - 87.989971088f)) - fSlow26), 0.0f)));
			}
			// post processing
			for (int i=0; i<4; i++) fRec4_perm[i]=fRec4_tmp[count+i];
			
			// LOOP 0x27a37c0
			// pre processing
			for (int i=0; i<4; i++) fRec13_tmp[i]=fRec13_perm[i];
			// exec code
			for (int i=0; i<count; i++) {
				fRec13[i] = ((fSlow40 * fRec13[i-1]) + (fSlow41 * max(((8.685889638065037f * ((8.262958288192749e-08f * float(pun_float_to_int(fRec14[i]))) - 87.989971088f)) - fSlow39), 0.0f)));
			}
			// post processing
			for (int i=0; i<4; i++) fRec13_perm[i]=fRec13_tmp[count+i];
			
			// LOOP 0x27b7670
			// pre processing
			for (int i=0; i<4; i++) fRec21_tmp[i]=fRec21_perm[i];
			// exec code
			for (int i=0; i<count; i++) {
				fRec21[i] = ((fSlow54 * fRec21[i-1]) + (fSlow55 * max(((8.685889638065037f * ((8.262958288192749e-08f * float(pun_float_to_int(fRec22[i]))) - 87.989971088f)) - fSlow53), 0.0f)));
			}
			// post processing
			for (int i=0; i<4; i++) fRec21_perm[i]=fRec21_tmp[count+i];
			
			// SECTION : 14
			// LOOP 0x27ca230
			// exec code
			for (int i=0; i<count; i++) {
				fZec19[i] = pun_int_to_float((8388608 * (126.94269504f + max(-126.0f, (0.16609640464202244f * fRec4[i])))));
			}
			
			// LOOP 0x27d0da0
			// exec code
			for (int i=0; i<count; i++) {
				fZec23[i] = pun_int_to_float((8388608 * (126.94269504f + max(-126.0f, (0.16609640464202244f * fRec13[i])))));
			}
			
			// LOOP 0x27d7b90
			// exec code
			for (int i=0; i<count; i++) {
				fZec27[i] = pun_int_to_float((8388608 * (126.94269504f + max(-126.0f, (0.16609640464202244f * fRec21[i])))));
			}
			
			// SECTION : 15
			// LOOP 0x27ca110
			// exec code
			for (int i=0; i<count; i++) {
				fZec20[i] = (fSlow20 * (fZec3[i] * fZec19[i]));
			}
			
			// LOOP 0x27cb5d0
			// exec code
			for (int i=0; i<count; i++) {
				fZec21[i] = (fZec5[i] * fZec19[i]);
			}
			
			// LOOP 0x27d0c80
			// exec code
			for (int i=0; i<count; i++) {
				fZec24[i] = (fSlow33 * (fZec8[i] * fZec23[i]));
			}
			
			// LOOP 0x27d1f90
			// exec code
			for (int i=0; i<count; i++) {
				fZec25[i] = (fZec10[i] * fZec23[i]);
			}
			
			// LOOP 0x27d7a70
			// exec code
			for (int i=0; i<count; i++) {
				fZec28[i] = (fSlow47 * (fZec15[i] * fZec27[i]));
			}
			
			// LOOP 0x27d8d80
			// exec code
			for (int i=0; i<count; i++) {
				fZec29[i] = (fZec17[i] * fZec27[i]);
			}
			
			// SECTION : 16
			// LOOP 0x27c9ff0
			// exec code
			for (int i=0; i<count; i++) {
				fZec22[i] = fabsf((fabsf((fSlow20 * fZec21[i])) + fabsf(fZec20[i])));
			}
			
			// LOOP 0x27d0b60
			// exec code
			for (int i=0; i<count; i++) {
				fZec26[i] = fabsf((fabsf((fSlow33 * fZec25[i])) + fabsf(fZec24[i])));
			}
			
			// LOOP 0x27d7950
			// exec code
			for (int i=0; i<count; i++) {
				fZec30[i] = fabsf((fabsf((fSlow47 * fZec29[i])) + fabsf(fZec28[i])));
			}
			
			// SECTION : 17
			// LOOP 0x27c9c30
			// pre processing
			for (int i=0; i<4; i++) fRec32_tmp[i]=fRec32_perm[i];
			// exec code
			for (int i=0; i<count; i++) {
				fRec32[i] = max(fZec22[i], ((fSlow21 * fRec32[i-1]) + (fSlow22 * fZec22[i])));
			}
			// post processing
			for (int i=0; i<4; i++) fRec32_perm[i]=fRec32_tmp[count+i];
			
			// LOOP 0x27d0770
			// pre processing
			for (int i=0; i<4; i++) fRec35_tmp[i]=fRec35_perm[i];
			// exec code
			for (int i=0; i<count; i++) {
				fRec35[i] = max(fZec26[i], ((fSlow34 * fRec35[i-1]) + (fSlow35 * fZec26[i])));
			}
			// post processing
			for (int i=0; i<4; i++) fRec35_perm[i]=fRec35_tmp[count+i];
			
			// LOOP 0x27d74d0
			// pre processing
			for (int i=0; i<4; i++) fRec38_tmp[i]=fRec38_perm[i];
			// exec code
			for (int i=0; i<count; i++) {
				fRec38[i] = max(fZec30[i], ((fSlow48 * fRec38[i-1]) + (fSlow49 * fZec30[i])));
			}
			// post processing
			for (int i=0; i<4; i++) fRec38_perm[i]=fRec38_tmp[count+i];
			
			// SECTION : 18
			// LOOP 0x27c98b0
			// pre processing
			for (int i=0; i<4; i++) fRec31_tmp[i]=fRec31_perm[i];
			// exec code
			for (int i=0; i<count; i++) {
				fRec31[i] = ((fSlow24 * fRec31[i-1]) + (fSlow25 * fRec32[i]));
			}
			// post processing
			for (int i=0; i<4; i++) fRec31_perm[i]=fRec31_tmp[count+i];
			
			// LOOP 0x27cfcb0
			// pre processing
			for (int i=0; i<4; i++) fRec34_tmp[i]=fRec34_perm[i];
			// exec code
			for (int i=0; i<count; i++) {
				fRec34[i] = ((fSlow37 * fRec34[i-1]) + (fSlow38 * fRec35[i]));
			}
			// post processing
			for (int i=0; i<4; i++) fRec34_perm[i]=fRec34_tmp[count+i];
			
			// LOOP 0x27d6750
			// pre processing
			for (int i=0; i<4; i++) fRec37_tmp[i]=fRec37_perm[i];
			// exec code
			for (int i=0; i<count; i++) {
				fRec37[i] = ((fSlow51 * fRec37[i-1]) + (fSlow52 * fRec38[i]));
			}
			// post processing
			for (int i=0; i<4; i++) fRec37_perm[i]=fRec37_tmp[count+i];
			
			// SECTION : 19
			// LOOP 0x27c9450
			// pre processing
			for (int i=0; i<4; i++) fRec30_tmp[i]=fRec30_perm[i];
			// exec code
			for (int i=0; i<count; i++) {
				fRec30[i] = ((fSlow27 * fRec30[i-1]) + (fSlow28 * max(((8.685889638065037f * ((8.262958288192749e-08f * float(pun_float_to_int(fRec31[i]))) - 87.989971088f)) - fSlow26), 0.0f)));
			}
			// post processing
			for (int i=0; i<4; i++) fRec30_perm[i]=fRec30_tmp[count+i];
			
			// LOOP 0x27d05d0
			// pre processing
			for (int i=0; i<4; i++) fRec33_tmp[i]=fRec33_perm[i];
			// exec code
			for (int i=0; i<count; i++) {
				fRec33[i] = ((fSlow40 * fRec33[i-1]) + (fSlow41 * max(((8.685889638065037f * ((8.262958288192749e-08f * float(pun_float_to_int(fRec34[i]))) - 87.989971088f)) - fSlow39), 0.0f)));
			}
			// post processing
			for (int i=0; i<4; i++) fRec33_perm[i]=fRec33_tmp[count+i];
			
			// LOOP 0x27d7330
			// pre processing
			for (int i=0; i<4; i++) fRec36_tmp[i]=fRec36_perm[i];
			// exec code
			for (int i=0; i<count; i++) {
				fRec36[i] = ((fSlow54 * fRec36[i-1]) + (fSlow55 * max(((8.685889638065037f * ((8.262958288192749e-08f * float(pun_float_to_int(fRec37[i]))) - 87.989971088f)) - fSlow53), 0.0f)));
			}
			// post processing
			for (int i=0; i<4; i++) fRec36_perm[i]=fRec36_tmp[count+i];
			
			// SECTION : 20
			// LOOP 0x27ddfe0
			// exec code
			for (int i=0; i<count; i++) {
				fbargraph0 = (0.5f * pun_int_to_float((8388608 * (126.94269504f + max(-126.0f, (0.16609640464202244f * fRec36[i]))))));
				fZec31[i] = (fSlow62 * fZec28[i]);
			}
			
			// LOOP 0x27e3c60
			// exec code
			for (int i=0; i<count; i++) {
				fZec32[i] = (fSlow63 * fZec29[i]);
			}
			
			// LOOP 0x27e5660
			// exec code
			for (int i=0; i<count; i++) {
				fbargraph2 = (0.5f * pun_int_to_float((8388608 * (126.94269504f + max(-126.0f, (0.16609640464202244f * fRec33[i]))))));
				fZec33[i] = (fSlow66 * fZec24[i]);
			}
			
			// LOOP 0x27e8e50
			// exec code
			for (int i=0; i<count; i++) {
				fZec34[i] = (fSlow67 * fZec25[i]);
			}
			
			// LOOP 0x27ea300
			// exec code
			for (int i=0; i<count; i++) {
				fbargraph4 = (0.5f * pun_int_to_float((8388608 * (126.94269504f + max(-126.0f, (0.16609640464202244f * fRec30[i]))))));
				fZec35[i] = (fSlow70 * fZec20[i]);
			}
			
			// LOOP 0x27ed060
			// exec code
			for (int i=0; i<count; i++) {
				fZec36[i] = (fSlow71 * fZec21[i]);
			}
			
			// SECTION : 21
			// LOOP 0x27ddec0
			// exec code
			for (int i=0; i<count; i++) {
				fbargraph1 = (fabsf(fZec32[i]) + fabsf(fZec31[i]));
				fbargraph3 = (fabsf(fZec34[i]) + fabsf(fZec33[i]));
				fbargraph5 = (fabsf(fZec36[i]) + fabsf(fZec35[i]));
				fZec37[i] = ((((iSlow19)?fZec2[i]:fZec35[i]) + ((iSlow32)?fZec7[i]:fZec33[i])) + ((iSlow46)?fZec14[i]:fZec31[i]));
			}
			
			// LOOP 0x27f2ea0
			// exec code
			for (int i=0; i<count; i++) {
				fZec39[i] = ((((iSlow19)?fZec4[i]:fZec36[i]) + ((iSlow32)?fZec9[i]:fZec34[i])) + ((iSlow46)?fZec16[i]:fZec32[i]));
			}
			
			// SECTION : 22
			// LOOP 0x27ddda0
			// exec code
			for (int i=0; i<count; i++) {
				fZec38[i] = ((iSlow72)?0:fZec37[i]);
			}
			
			// LOOP 0x27f2d80
			// exec code
			for (int i=0; i<count; i++) {
				fZec40[i] = ((iSlow72)?0:fZec39[i]);
			}
			
			// SECTION : 23
			// LOOP 0x27ddc80
			// exec code
			for (int i=0; i<count; i++) {
				fZec41[i] = fabsf((fabsf((fSlow73 * fZec40[i])) + fabsf((fSlow73 * fZec38[i]))));
			}
			
			// SECTION : 24
			// LOOP 0x27c8f10
			// pre processing
			for (int i=0; i<4; i++) fRec29_tmp[i]=fRec29_perm[i];
			// exec code
			for (int i=0; i<count; i++) {
				fRec29[i] = max(fZec41[i], ((fSlow74 * fRec29[i-1]) + (fSlow75 * fZec41[i])));
			}
			// post processing
			for (int i=0; i<4; i++) fRec29_perm[i]=fRec29_tmp[count+i];
			
			// SECTION : 25
			// LOOP 0x27c8b70
			// pre processing
			for (int i=0; i<4; i++) fRec28_tmp[i]=fRec28_perm[i];
			// exec code
			for (int i=0; i<count; i++) {
				fRec28[i] = ((fSlow77 * fRec28[i-1]) + (fSlow78 * fRec29[i]));
			}
			// post processing
			for (int i=0; i<4; i++) fRec28_perm[i]=fRec28_tmp[count+i];
			
			// SECTION : 26
			// LOOP 0x27c8850
			// pre processing
			for (int i=0; i<4; i++) fRec27_tmp[i]=fRec27_perm[i];
			// exec code
			for (int i=0; i<count; i++) {
				fRec27[i] = ((fSlow79 * fRec27[i-1]) + (fSlow80 * max((6 + (8.685889638065037f * ((8.262958288192749e-08f * float(pun_float_to_int(fRec28[i]))) - 87.989971088f))), 0.0f)));
			}
			// post processing
			for (int i=0; i<4; i++) fRec27_perm[i]=fRec27_tmp[count+i];
			
			// SECTION : 27
			// LOOP 0x27fe4c0
			// exec code
			for (int i=0; i<count; i++) {
				fZec42[i] = pun_int_to_float((8388608 * (126.94269504f + max(-126.0f, (0.16609640464202244f * fRec27[i])))));
			}
			
			// SECTION : 28
			// LOOP 0x27853c0
			// exec code
			for (int i=0; i<count; i++) {
				output0[i] = (FAUSTFLOAT)(fSlow82 * ((iSlow72)?fZec39[i]:(fSlow81 * (fZec40[i] * fZec42[i]))));
			}
			
			// LOOP 0x2803970
			// exec code
			for (int i=0; i<count; i++) {
				output1[i] = (FAUSTFLOAT)(fSlow82 * ((iSlow72)?fZec37[i]:(fSlow81 * (fZec38[i] * fZec42[i]))));
			}
			
		}
		if (index < fullcount) {
			// compute the remaining samples if any
			int count = fullcount-index;
			FAUSTFLOAT* input0 = &input[0][index];
			FAUSTFLOAT* input1 = &input[1][index];
			FAUSTFLOAT* output0 = &output[0][index];
			FAUSTFLOAT* output1 = &output[1][index];
			// SECTION : 1
			// LOOP 0x27862c0
			// pre processing
			for (int i=0; i<4; i++) fYec0_tmp[i]=fYec0_perm[i];
			// exec code
			for (int i=0; i<count; i++) {
				fYec0[i] = (float)input0[i];
			}
			// post processing
			for (int i=0; i<4; i++) fYec0_perm[i]=fYec0_tmp[count+i];
			
			// LOOP 0x27916c0
			// pre processing
			for (int i=0; i<4; i++) fYec2_tmp[i]=fYec2_perm[i];
			// exec code
			for (int i=0; i<count; i++) {
				fYec2[i] = (float)input1[i];
			}
			// post processing
			for (int i=0; i<4; i++) fYec2_perm[i]=fYec2_tmp[count+i];
			
			// SECTION : 2
			// LOOP 0x2786020
			// pre processing
			for (int i=0; i<4; i++) fRec3_tmp[i]=fRec3_perm[i];
			// exec code
			for (int i=0; i<count; i++) {
				fRec3[i] = ((fSlow4 * fRec3[i-1]) + (fSlow3 * ((float)input0[i] + fYec0[i-1])));
			}
			// post processing
			for (int i=0; i<4; i++) fRec3_perm[i]=fRec3_tmp[count+i];
			
			// LOOP 0x2791380
			// pre processing
			for (int i=0; i<4; i++) fRec10_tmp[i]=fRec10_perm[i];
			// exec code
			for (int i=0; i<count; i++) {
				fRec10[i] = ((fSlow4 * fRec10[i-1]) + (fSlow3 * ((float)input1[i] + fYec2[i-1])));
			}
			// post processing
			for (int i=0; i<4; i++) fRec10_perm[i]=fRec10_tmp[count+i];
			
			// SECTION : 3
			// LOOP 0x2785cc0
			// pre processing
			for (int i=0; i<4; i++) fRec2_tmp[i]=fRec2_perm[i];
			// exec code
			for (int i=0; i<count; i++) {
				fRec2[i] = (fRec3[i] - (fSlow9 * ((fSlow7 * fRec2[i-2]) + (fSlow6 * fRec2[i-1]))));
			}
			// post processing
			for (int i=0; i<4; i++) fRec2_perm[i]=fRec2_tmp[count+i];
			
			// LOOP 0x2790f60
			// pre processing
			for (int i=0; i<4; i++) fRec9_tmp[i]=fRec9_perm[i];
			// exec code
			for (int i=0; i<count; i++) {
				fRec9[i] = (fRec10[i] - (fSlow9 * ((fSlow7 * fRec9[i-2]) + (fSlow6 * fRec9[i-1]))));
			}
			// post processing
			for (int i=0; i<4; i++) fRec9_perm[i]=fRec9_tmp[count+i];
			
			// SECTION : 4
			// LOOP 0x278b930
			// exec code
			for (int i=0; i<count; i++) {
				fZec0[i] = (fRec2[i-2] + (fRec2[i] + (2 * fRec2[i-1])));
			}
			
			// LOOP 0x27942b0
			// exec code
			for (int i=0; i<count; i++) {
				fZec1[i] = (fRec9[i-2] + (fRec9[i] + (2 * fRec9[i-1])));
			}
			
			// SECTION : 5
			// LOOP 0x278b810
			// pre processing
			for (int i=0; i<4; i++) fYec1_tmp[i]=fYec1_perm[i];
			// exec code
			for (int i=0; i<count; i++) {
				fYec1[i] = (fSlow9 * fZec0[i]);
			}
			// post processing
			for (int i=0; i<4; i++) fYec1_perm[i]=fYec1_tmp[count+i];
			
			// LOOP 0x2794190
			// pre processing
			for (int i=0; i<4; i++) fYec3_tmp[i]=fYec3_perm[i];
			// exec code
			for (int i=0; i<count; i++) {
				fYec3[i] = (fSlow9 * fZec1[i]);
			}
			// post processing
			for (int i=0; i<4; i++) fYec3_perm[i]=fYec3_tmp[count+i];
			
			// LOOP 0x27b2710
			// pre processing
			for (int i=0; i<4; i++) fRec20_tmp[i]=fRec20_perm[i];
			// exec code
			for (int i=0; i<count; i++) {
				fRec20[i] = ((fSlow4 * fRec20[i-1]) + (fSlow3 * ((fSlow1 * (float)input0[i]) + (fSlow42 * fYec0[i-1]))));
			}
			// post processing
			for (int i=0; i<4; i++) fRec20_perm[i]=fRec20_tmp[count+i];
			
			// LOOP 0x27b8360
			// pre processing
			for (int i=0; i<4; i++) fRec26_tmp[i]=fRec26_perm[i];
			// exec code
			for (int i=0; i<count; i++) {
				fRec26[i] = ((fSlow4 * fRec26[i-1]) + (fSlow3 * ((fSlow1 * (float)input1[i]) + (fSlow42 * fYec2[i-1]))));
			}
			// post processing
			for (int i=0; i<4; i++) fRec26_perm[i]=fRec26_tmp[count+i];
			
			// SECTION : 6
			// LOOP 0x2785960
			// pre processing
			for (int i=0; i<4; i++) fRec1_tmp[i]=fRec1_perm[i];
			// exec code
			for (int i=0; i<count; i++) {
				fRec1[i] = ((fSlow14 * fRec1[i-1]) + (fSlow13 * (fYec1[i] + fYec1[i-1])));
			}
			// post processing
			for (int i=0; i<4; i++) fRec1_perm[i]=fRec1_tmp[count+i];
			
			// LOOP 0x2790ac0
			// pre processing
			for (int i=0; i<4; i++) fRec8_tmp[i]=fRec8_perm[i];
			// exec code
			for (int i=0; i<count; i++) {
				fRec8[i] = ((fSlow14 * fRec8[i-1]) + (fSlow13 * (fYec3[i] + fYec3[i-1])));
			}
			// post processing
			for (int i=0; i<4; i++) fRec8_perm[i]=fRec8_tmp[count+i];
			
			// LOOP 0x27a0f70
			// pre processing
			for (int i=0; i<4; i++) fRec12_tmp[i]=fRec12_perm[i];
			// exec code
			for (int i=0; i<count; i++) {
				fRec12[i] = ((fSlow14 * fRec12[i-1]) + (fSlow13 * ((fSlow30 * fZec0[i]) + (fSlow29 * fYec1[i-1]))));
			}
			// post processing
			for (int i=0; i<4; i++) fRec12_perm[i]=fRec12_tmp[count+i];
			
			// LOOP 0x27a46a0
			// pre processing
			for (int i=0; i<4; i++) fRec17_tmp[i]=fRec17_perm[i];
			// exec code
			for (int i=0; i<count; i++) {
				fRec17[i] = ((fSlow14 * fRec17[i-1]) + (fSlow13 * ((fSlow30 * fZec1[i]) + (fSlow29 * fYec3[i-1]))));
			}
			// post processing
			for (int i=0; i<4; i++) fRec17_perm[i]=fRec17_tmp[count+i];
			
			// LOOP 0x27b1ef0
			// pre processing
			for (int i=0; i<4; i++) fRec19_tmp[i]=fRec19_perm[i];
			// exec code
			for (int i=0; i<count; i++) {
				fRec19[i] = (fRec20[i] - (fSlow9 * ((fSlow7 * fRec19[i-2]) + (fSlow6 * fRec19[i-1]))));
			}
			// post processing
			for (int i=0; i<4; i++) fRec19_perm[i]=fRec19_tmp[count+i];
			
			// LOOP 0x27b7f40
			// pre processing
			for (int i=0; i<4; i++) fRec25_tmp[i]=fRec25_perm[i];
			// exec code
			for (int i=0; i<count; i++) {
				fRec25[i] = (fRec26[i] - (fSlow9 * ((fSlow7 * fRec25[i-2]) + (fSlow6 * fRec25[i-1]))));
			}
			// post processing
			for (int i=0; i<4; i++) fRec25_perm[i]=fRec25_tmp[count+i];
			
			// SECTION : 7
			// LOOP 0x2785600
			// pre processing
			for (int i=0; i<4; i++) fRec0_tmp[i]=fRec0_perm[i];
			// exec code
			for (int i=0; i<count; i++) {
				fRec0[i] = (fRec1[i] - (fSlow18 * ((fSlow17 * fRec0[i-2]) + (fSlow16 * fRec0[i-1]))));
			}
			// post processing
			for (int i=0; i<4; i++) fRec0_perm[i]=fRec0_tmp[count+i];
			
			// LOOP 0x27906a0
			// pre processing
			for (int i=0; i<4; i++) fRec7_tmp[i]=fRec7_perm[i];
			// exec code
			for (int i=0; i<count; i++) {
				fRec7[i] = (fRec8[i] - (fSlow18 * ((fSlow17 * fRec7[i-2]) + (fSlow16 * fRec7[i-1]))));
			}
			// post processing
			for (int i=0; i<4; i++) fRec7_perm[i]=fRec7_tmp[count+i];
			
			// LOOP 0x27a0cd0
			// pre processing
			for (int i=0; i<4; i++) fRec11_tmp[i]=fRec11_perm[i];
			// exec code
			for (int i=0; i<count; i++) {
				fRec11[i] = (fRec12[i] - (fSlow18 * ((fSlow17 * fRec11[i-2]) + (fSlow16 * fRec11[i-1]))));
			}
			// post processing
			for (int i=0; i<4; i++) fRec11_perm[i]=fRec11_tmp[count+i];
			
			// LOOP 0x27a4340
			// pre processing
			for (int i=0; i<4; i++) fRec16_tmp[i]=fRec16_perm[i];
			// exec code
			for (int i=0; i<count; i++) {
				fRec16[i] = (fRec17[i] - (fSlow18 * ((fSlow17 * fRec16[i-2]) + (fSlow16 * fRec16[i-1]))));
			}
			// post processing
			for (int i=0; i<4; i++) fRec16_perm[i]=fRec16_tmp[count+i];
			
			// LOOP 0x27b25f0
			// pre processing
			for (int i=0; i<4; i++) fRec18_tmp[i]=fRec18_perm[i];
			// exec code
			for (int i=0; i<count; i++) {
				fZec12[i] = (fSlow16 * fRec18[i-1]);
				fRec18[i] = ((fSlow9 * (((fSlow5 * fRec19[i]) + (fSlow45 * fRec19[i-1])) + (fSlow5 * fRec19[i-2]))) - (fSlow44 * ((fSlow43 * fRec18[i-2]) + fZec12[i])));
			}
			// post processing
			for (int i=0; i<4; i++) fRec18_perm[i]=fRec18_tmp[count+i];
			
			// LOOP 0x27b7aa0
			// pre processing
			for (int i=0; i<4; i++) fRec24_tmp[i]=fRec24_perm[i];
			// exec code
			for (int i=0; i<count; i++) {
				fZec13[i] = (fSlow16 * fRec24[i-1]);
				fRec24[i] = ((fSlow9 * (((fSlow5 * fRec25[i]) + (fSlow45 * fRec25[i-1])) + (fSlow5 * fRec25[i-2]))) - (fSlow44 * ((fSlow43 * fRec24[i-2]) + fZec13[i])));
			}
			// post processing
			for (int i=0; i<4; i++) fRec24_perm[i]=fRec24_tmp[count+i];
			
			// SECTION : 8
			// LOOP 0x2796e30
			// exec code
			for (int i=0; i<count; i++) {
				fZec2[i] = (fSlow18 * (fRec7[i-2] + (fRec7[i] + (2 * fRec7[i-1]))));
			}
			
			// LOOP 0x27988a0
			// exec code
			for (int i=0; i<count; i++) {
				fZec4[i] = (fSlow18 * (fRec0[i-2] + (fRec0[i] + (2 * fRec0[i-1]))));
			}
			
			// LOOP 0x27a6e10
			// exec code
			for (int i=0; i<count; i++) {
				fZec7[i] = (fSlow18 * (((fSlow15 * fRec16[i]) + (fSlow31 * fRec16[i-1])) + (fSlow15 * fRec16[i-2])));
			}
			
			// LOOP 0x27a9690
			// exec code
			for (int i=0; i<count; i++) {
				fZec9[i] = (fSlow18 * (((fSlow15 * fRec11[i]) + (fSlow31 * fRec11[i-1])) + (fSlow15 * fRec11[i-2])));
			}
			
			// LOOP 0x27bc7b0
			// exec code
			for (int i=0; i<count; i++) {
				fZec14[i] = (fRec24[i-2] + (fSlow44 * (fZec13[i] + (fSlow43 * fRec24[i]))));
			}
			
			// LOOP 0x27bedc0
			// exec code
			for (int i=0; i<count; i++) {
				fZec16[i] = (fRec18[i-2] + (fSlow44 * (fZec12[i] + (fSlow43 * fRec18[i]))));
			}
			
			// SECTION : 9
			// LOOP 0x2796d10
			// exec code
			for (int i=0; i<count; i++) {
				fZec3[i] = ((iSlow19)?0:fZec2[i]);
			}
			
			// LOOP 0x2798780
			// exec code
			for (int i=0; i<count; i++) {
				fZec5[i] = ((iSlow19)?0:fZec4[i]);
			}
			
			// LOOP 0x27a6cf0
			// exec code
			for (int i=0; i<count; i++) {
				fZec8[i] = ((iSlow32)?0:fZec7[i]);
			}
			
			// LOOP 0x27a9570
			// exec code
			for (int i=0; i<count; i++) {
				fZec10[i] = ((iSlow32)?0:fZec9[i]);
			}
			
			// LOOP 0x27bc690
			// exec code
			for (int i=0; i<count; i++) {
				fZec15[i] = ((iSlow46)?0:fZec14[i]);
			}
			
			// LOOP 0x27beca0
			// exec code
			for (int i=0; i<count; i++) {
				fZec17[i] = ((iSlow46)?0:fZec16[i]);
			}
			
			// SECTION : 10
			// LOOP 0x2796bf0
			// exec code
			for (int i=0; i<count; i++) {
				fZec6[i] = fabsf((fabsf((fSlow20 * fZec5[i])) + fabsf((fSlow20 * fZec3[i]))));
			}
			
			// LOOP 0x27a6bd0
			// exec code
			for (int i=0; i<count; i++) {
				fZec11[i] = fabsf((fabsf((fSlow33 * fZec10[i])) + fabsf((fSlow33 * fZec8[i]))));
			}
			
			// LOOP 0x27bc570
			// exec code
			for (int i=0; i<count; i++) {
				fZec18[i] = fabsf((fabsf((fSlow47 * fZec17[i])) + fabsf((fSlow47 * fZec15[i]))));
			}
			
			// SECTION : 11
			// LOOP 0x27901d0
			// pre processing
			for (int i=0; i<4; i++) fRec6_tmp[i]=fRec6_perm[i];
			// exec code
			for (int i=0; i<count; i++) {
				fRec6[i] = max(fZec6[i], ((fSlow21 * fRec6[i-1]) + (fSlow22 * fZec6[i])));
			}
			// post processing
			for (int i=0; i<4; i++) fRec6_perm[i]=fRec6_tmp[count+i];
			
			// LOOP 0x27a3f20
			// pre processing
			for (int i=0; i<4; i++) fRec15_tmp[i]=fRec15_perm[i];
			// exec code
			for (int i=0; i<count; i++) {
				fRec15[i] = max(fZec11[i], ((fSlow34 * fRec15[i-1]) + (fSlow35 * fZec11[i])));
			}
			// post processing
			for (int i=0; i<4; i++) fRec15_perm[i]=fRec15_tmp[count+i];
			
			// LOOP 0x27b6eb0
			// pre processing
			for (int i=0; i<4; i++) fRec23_tmp[i]=fRec23_perm[i];
			// exec code
			for (int i=0; i<count; i++) {
				fRec23[i] = max(fZec18[i], ((fSlow48 * fRec23[i-1]) + (fSlow49 * fZec18[i])));
			}
			// post processing
			for (int i=0; i<4; i++) fRec23_perm[i]=fRec23_tmp[count+i];
			
			// SECTION : 12
			// LOOP 0x278fd50
			// pre processing
			for (int i=0; i<4; i++) fRec5_tmp[i]=fRec5_perm[i];
			// exec code
			for (int i=0; i<count; i++) {
				fRec5[i] = ((fSlow24 * fRec5[i-1]) + (fSlow25 * fRec6[i]));
			}
			// post processing
			for (int i=0; i<4; i++) fRec5_perm[i]=fRec5_tmp[count+i];
			
			// LOOP 0x27a3b60
			// pre processing
			for (int i=0; i<4; i++) fRec14_tmp[i]=fRec14_perm[i];
			// exec code
			for (int i=0; i<count; i++) {
				fRec14[i] = ((fSlow37 * fRec14[i-1]) + (fSlow38 * fRec15[i]));
			}
			// post processing
			for (int i=0; i<4; i++) fRec14_perm[i]=fRec14_tmp[count+i];
			
			// LOOP 0x27b6c20
			// pre processing
			for (int i=0; i<4; i++) fRec22_tmp[i]=fRec22_perm[i];
			// exec code
			for (int i=0; i<count; i++) {
				fRec22[i] = ((fSlow51 * fRec22[i-1]) + (fSlow52 * fRec23[i]));
			}
			// post processing
			for (int i=0; i<4; i++) fRec22_perm[i]=fRec22_tmp[count+i];
			
			// SECTION : 13
			// LOOP 0x278fa90
			// pre processing
			for (int i=0; i<4; i++) fRec4_tmp[i]=fRec4_perm[i];
			// exec code
			for (int i=0; i<count; i++) {
				fRec4[i] = ((fSlow27 * fRec4[i-1]) + (fSlow28 * max(((8.685889638065037f * ((8.262958288192749e-08f * float(pun_float_to_int(fRec5[i]))) - 87.989971088f)) - fSlow26), 0.0f)));
			}
			// post processing
			for (int i=0; i<4; i++) fRec4_perm[i]=fRec4_tmp[count+i];
			
			// LOOP 0x27a37c0
			// pre processing
			for (int i=0; i<4; i++) fRec13_tmp[i]=fRec13_perm[i];
			// exec code
			for (int i=0; i<count; i++) {
				fRec13[i] = ((fSlow40 * fRec13[i-1]) + (fSlow41 * max(((8.685889638065037f * ((8.262958288192749e-08f * float(pun_float_to_int(fRec14[i]))) - 87.989971088f)) - fSlow39), 0.0f)));
			}
			// post processing
			for (int i=0; i<4; i++) fRec13_perm[i]=fRec13_tmp[count+i];
			
			// LOOP 0x27b7670
			// pre processing
			for (int i=0; i<4; i++) fRec21_tmp[i]=fRec21_perm[i];
			// exec code
			for (int i=0; i<count; i++) {
				fRec21[i] = ((fSlow54 * fRec21[i-1]) + (fSlow55 * max(((8.685889638065037f * ((8.262958288192749e-08f * float(pun_float_to_int(fRec22[i]))) - 87.989971088f)) - fSlow53), 0.0f)));
			}
			// post processing
			for (int i=0; i<4; i++) fRec21_perm[i]=fRec21_tmp[count+i];
			
			// SECTION : 14
			// LOOP 0x27ca230
			// exec code
			for (int i=0; i<count; i++) {
				fZec19[i] = pun_int_to_float((8388608 * (126.94269504f + max(-126.0f, (0.16609640464202244f * fRec4[i])))));
			}
			
			// LOOP 0x27d0da0
			// exec code
			for (int i=0; i<count; i++) {
				fZec23[i] = pun_int_to_float((8388608 * (126.94269504f + max(-126.0f, (0.16609640464202244f * fRec13[i])))));
			}
			
			// LOOP 0x27d7b90
			// exec code
			for (int i=0; i<count; i++) {
				fZec27[i] = pun_int_to_float((8388608 * (126.94269504f + max(-126.0f, (0.16609640464202244f * fRec21[i])))));
			}
			
			// SECTION : 15
			// LOOP 0x27ca110
			// exec code
			for (int i=0; i<count; i++) {
				fZec20[i] = (fSlow20 * (fZec3[i] * fZec19[i]));
			}
			
			// LOOP 0x27cb5d0
			// exec code
			for (int i=0; i<count; i++) {
				fZec21[i] = (fZec5[i] * fZec19[i]);
			}
			
			// LOOP 0x27d0c80
			// exec code
			for (int i=0; i<count; i++) {
				fZec24[i] = (fSlow33 * (fZec8[i] * fZec23[i]));
			}
			
			// LOOP 0x27d1f90
			// exec code
			for (int i=0; i<count; i++) {
				fZec25[i] = (fZec10[i] * fZec23[i]);
			}
			
			// LOOP 0x27d7a70
			// exec code
			for (int i=0; i<count; i++) {
				fZec28[i] = (fSlow47 * (fZec15[i] * fZec27[i]));
			}
			
			// LOOP 0x27d8d80
			// exec code
			for (int i=0; i<count; i++) {
				fZec29[i] = (fZec17[i] * fZec27[i]);
			}
			
			// SECTION : 16
			// LOOP 0x27c9ff0
			// exec code
			for (int i=0; i<count; i++) {
				fZec22[i] = fabsf((fabsf((fSlow20 * fZec21[i])) + fabsf(fZec20[i])));
			}
			
			// LOOP 0x27d0b60
			// exec code
			for (int i=0; i<count; i++) {
				fZec26[i] = fabsf((fabsf((fSlow33 * fZec25[i])) + fabsf(fZec24[i])));
			}
			
			// LOOP 0x27d7950
			// exec code
			for (int i=0; i<count; i++) {
				fZec30[i] = fabsf((fabsf((fSlow47 * fZec29[i])) + fabsf(fZec28[i])));
			}
			
			// SECTION : 17
			// LOOP 0x27c9c30
			// pre processing
			for (int i=0; i<4; i++) fRec32_tmp[i]=fRec32_perm[i];
			// exec code
			for (int i=0; i<count; i++) {
				fRec32[i] = max(fZec22[i], ((fSlow21 * fRec32[i-1]) + (fSlow22 * fZec22[i])));
			}
			// post processing
			for (int i=0; i<4; i++) fRec32_perm[i]=fRec32_tmp[count+i];
			
			// LOOP 0x27d0770
			// pre processing
			for (int i=0; i<4; i++) fRec35_tmp[i]=fRec35_perm[i];
			// exec code
			for (int i=0; i<count; i++) {
				fRec35[i] = max(fZec26[i], ((fSlow34 * fRec35[i-1]) + (fSlow35 * fZec26[i])));
			}
			// post processing
			for (int i=0; i<4; i++) fRec35_perm[i]=fRec35_tmp[count+i];
			
			// LOOP 0x27d74d0
			// pre processing
			for (int i=0; i<4; i++) fRec38_tmp[i]=fRec38_perm[i];
			// exec code
			for (int i=0; i<count; i++) {
				fRec38[i] = max(fZec30[i], ((fSlow48 * fRec38[i-1]) + (fSlow49 * fZec30[i])));
			}
			// post processing
			for (int i=0; i<4; i++) fRec38_perm[i]=fRec38_tmp[count+i];
			
			// SECTION : 18
			// LOOP 0x27c98b0
			// pre processing
			for (int i=0; i<4; i++) fRec31_tmp[i]=fRec31_perm[i];
			// exec code
			for (int i=0; i<count; i++) {
				fRec31[i] = ((fSlow24 * fRec31[i-1]) + (fSlow25 * fRec32[i]));
			}
			// post processing
			for (int i=0; i<4; i++) fRec31_perm[i]=fRec31_tmp[count+i];
			
			// LOOP 0x27cfcb0
			// pre processing
			for (int i=0; i<4; i++) fRec34_tmp[i]=fRec34_perm[i];
			// exec code
			for (int i=0; i<count; i++) {
				fRec34[i] = ((fSlow37 * fRec34[i-1]) + (fSlow38 * fRec35[i]));
			}
			// post processing
			for (int i=0; i<4; i++) fRec34_perm[i]=fRec34_tmp[count+i];
			
			// LOOP 0x27d6750
			// pre processing
			for (int i=0; i<4; i++) fRec37_tmp[i]=fRec37_perm[i];
			// exec code
			for (int i=0; i<count; i++) {
				fRec37[i] = ((fSlow51 * fRec37[i-1]) + (fSlow52 * fRec38[i]));
			}
			// post processing
			for (int i=0; i<4; i++) fRec37_perm[i]=fRec37_tmp[count+i];
			
			// SECTION : 19
			// LOOP 0x27c9450
			// pre processing
			for (int i=0; i<4; i++) fRec30_tmp[i]=fRec30_perm[i];
			// exec code
			for (int i=0; i<count; i++) {
				fRec30[i] = ((fSlow27 * fRec30[i-1]) + (fSlow28 * max(((8.685889638065037f * ((8.262958288192749e-08f * float(pun_float_to_int(fRec31[i]))) - 87.989971088f)) - fSlow26), 0.0f)));
			}
			// post processing
			for (int i=0; i<4; i++) fRec30_perm[i]=fRec30_tmp[count+i];
			
			// LOOP 0x27d05d0
			// pre processing
			for (int i=0; i<4; i++) fRec33_tmp[i]=fRec33_perm[i];
			// exec code
			for (int i=0; i<count; i++) {
				fRec33[i] = ((fSlow40 * fRec33[i-1]) + (fSlow41 * max(((8.685889638065037f * ((8.262958288192749e-08f * float(pun_float_to_int(fRec34[i]))) - 87.989971088f)) - fSlow39), 0.0f)));
			}
			// post processing
			for (int i=0; i<4; i++) fRec33_perm[i]=fRec33_tmp[count+i];
			
			// LOOP 0x27d7330
			// pre processing
			for (int i=0; i<4; i++) fRec36_tmp[i]=fRec36_perm[i];
			// exec code
			for (int i=0; i<count; i++) {
				fRec36[i] = ((fSlow54 * fRec36[i-1]) + (fSlow55 * max(((8.685889638065037f * ((8.262958288192749e-08f * float(pun_float_to_int(fRec37[i]))) - 87.989971088f)) - fSlow53), 0.0f)));
			}
			// post processing
			for (int i=0; i<4; i++) fRec36_perm[i]=fRec36_tmp[count+i];
			
			// SECTION : 20
			// LOOP 0x27ddfe0
			// exec code
			for (int i=0; i<count; i++) {
				fbargraph0 = (0.5f * pun_int_to_float((8388608 * (126.94269504f + max(-126.0f, (0.16609640464202244f * fRec36[i]))))));
				fZec31[i] = (fSlow62 * fZec28[i]);
			}
			
			// LOOP 0x27e3c60
			// exec code
			for (int i=0; i<count; i++) {
				fZec32[i] = (fSlow63 * fZec29[i]);
			}
			
			// LOOP 0x27e5660
			// exec code
			for (int i=0; i<count; i++) {
				fbargraph2 = (0.5f * pun_int_to_float((8388608 * (126.94269504f + max(-126.0f, (0.16609640464202244f * fRec33[i]))))));
				fZec33[i] = (fSlow66 * fZec24[i]);
			}
			
			// LOOP 0x27e8e50
			// exec code
			for (int i=0; i<count; i++) {
				fZec34[i] = (fSlow67 * fZec25[i]);
			}
			
			// LOOP 0x27ea300
			// exec code
			for (int i=0; i<count; i++) {
				fbargraph4 = (0.5f * pun_int_to_float((8388608 * (126.94269504f + max(-126.0f, (0.16609640464202244f * fRec30[i]))))));
				fZec35[i] = (fSlow70 * fZec20[i]);
			}
			
			// LOOP 0x27ed060
			// exec code
			for (int i=0; i<count; i++) {
				fZec36[i] = (fSlow71 * fZec21[i]);
			}
			
			// SECTION : 21
			// LOOP 0x27ddec0
			// exec code
			for (int i=0; i<count; i++) {
				fbargraph1 = (fabsf(fZec32[i]) + fabsf(fZec31[i]));
				fbargraph3 = (fabsf(fZec34[i]) + fabsf(fZec33[i]));
				fbargraph5 = (fabsf(fZec36[i]) + fabsf(fZec35[i]));
				fZec37[i] = ((((iSlow19)?fZec2[i]:fZec35[i]) + ((iSlow32)?fZec7[i]:fZec33[i])) + ((iSlow46)?fZec14[i]:fZec31[i]));
			}
			
			// LOOP 0x27f2ea0
			// exec code
			for (int i=0; i<count; i++) {
				fZec39[i] = ((((iSlow19)?fZec4[i]:fZec36[i]) + ((iSlow32)?fZec9[i]:fZec34[i])) + ((iSlow46)?fZec16[i]:fZec32[i]));
			}
			
			// SECTION : 22
			// LOOP 0x27ddda0
			// exec code
			for (int i=0; i<count; i++) {
				fZec38[i] = ((iSlow72)?0:fZec37[i]);
			}
			
			// LOOP 0x27f2d80
			// exec code
			for (int i=0; i<count; i++) {
				fZec40[i] = ((iSlow72)?0:fZec39[i]);
			}
			
			// SECTION : 23
			// LOOP 0x27ddc80
			// exec code
			for (int i=0; i<count; i++) {
				fZec41[i] = fabsf((fabsf((fSlow73 * fZec40[i])) + fabsf((fSlow73 * fZec38[i]))));
			}
			
			// SECTION : 24
			// LOOP 0x27c8f10
			// pre processing
			for (int i=0; i<4; i++) fRec29_tmp[i]=fRec29_perm[i];
			// exec code
			for (int i=0; i<count; i++) {
				fRec29[i] = max(fZec41[i], ((fSlow74 * fRec29[i-1]) + (fSlow75 * fZec41[i])));
			}
			// post processing
			for (int i=0; i<4; i++) fRec29_perm[i]=fRec29_tmp[count+i];
			
			// SECTION : 25
			// LOOP 0x27c8b70
			// pre processing
			for (int i=0; i<4; i++) fRec28_tmp[i]=fRec28_perm[i];
			// exec code
			for (int i=0; i<count; i++) {
				fRec28[i] = ((fSlow77 * fRec28[i-1]) + (fSlow78 * fRec29[i]));
			}
			// post processing
			for (int i=0; i<4; i++) fRec28_perm[i]=fRec28_tmp[count+i];
			
			// SECTION : 26
			// LOOP 0x27c8850
			// pre processing
			for (int i=0; i<4; i++) fRec27_tmp[i]=fRec27_perm[i];
			// exec code
			for (int i=0; i<count; i++) {
				fRec27[i] = ((fSlow79 * fRec27[i-1]) + (fSlow80 * max((6 + (8.685889638065037f * ((8.262958288192749e-08f * float(pun_float_to_int(fRec28[i]))) - 87.989971088f))), 0.0f)));
			}
			// post processing
			for (int i=0; i<4; i++) fRec27_perm[i]=fRec27_tmp[count+i];
			
			// SECTION : 27
			// LOOP 0x27fe4c0
			// exec code
			for (int i=0; i<count; i++) {
				fZec42[i] = pun_int_to_float((8388608 * (126.94269504f + max(-126.0f, (0.16609640464202244f * fRec27[i])))));
			}
			
			// SECTION : 28
			// LOOP 0x27853c0
			// exec code
			for (int i=0; i<count; i++) {
				output0[i] = (FAUSTFLOAT)(fSlow82 * ((iSlow72)?fZec39[i]:(fSlow81 * (fZec40[i] * fZec42[i]))));
			}
			
			// LOOP 0x2803970
			// exec code
			for (int i=0; i<count; i++) {
				output1[i] = (FAUSTFLOAT)(fSlow82 * ((iSlow72)?fZec37[i]:(fSlow81 * (fZec38[i] * fZec42[i]))));
			}
			
		}
	}
};




#include "Faust_plugins_template2.cpp"

