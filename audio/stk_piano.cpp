//-----------------------------------------------------
// name: "Commuted Piano"
// author: "Romain Michon (rmichon@ccrma.stanford.edu)"
// copyright: "Romain Michon"
// version: "1.0"
//
// Code generated with Faust 0.9.73 (http://faust.grame.fr)
//-----------------------------------------------------
/* link with : "" */
#include <math.h>
#include <piano.h>
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

/*
#include "/usr/share/faust/audio/dsp.h"
#include "/usr/share/faust/gui/UI.h"
*/

// We use faust1 here.

struct Meta
{
    void declare (const char* key, const char* value) { }
};

#include "faudiostream/architecture/faust/audio/dsp.h"
#include "faudiostream/architecture/faust/gui/UI.h"


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
#define FAUSTCLASS Piano_dsp
#endif

class Piano_dsp : public dsp {
  private:
	FAUSTFLOAT 	fentry0;
	int 	iConst0;
	float 	fConst1;
	FAUSTFLOAT 	fslider0;
	FAUSTFLOAT 	fbutton0;
	FAUSTFLOAT 	fslider1;
	float 	fConst2;
	float 	fConst3;
	float 	fRec11[2];
	float 	fConst4;
	float 	fConst5;
	float 	fConst6;
	float 	fRec10[2];
	FAUSTFLOAT 	fentry1;
	float 	fConst7;
	float 	fRec12[2];
	int 	iRec13[2];
	float 	fVec0[2];
	float 	fVec1[2];
	float 	fRec9[2];
	float 	fRec8[2];
	float 	fRec7[2];
	float 	fRec6[2];
	float 	fRec5[2];
	float 	fConst8;
	float 	fRec4[3];
	float 	fRec3[3];
	float 	fRec2[3];
	float 	fRec1[3];
	float 	fRec0[2];
	float 	fRec24[2];
	float 	fRec23[2];
	float 	fRec22[2];
	float 	fRec21[2];
	float 	fRec20[2];
	float 	fRec25[2];
	float 	fVec2[2];
	FAUSTFLOAT 	fslider2;
	float 	fRec19[2];
	float 	fRec18[2];
	int 	IOTA;
	float 	fRec17[4096];
	FAUSTFLOAT 	fslider3;
	float 	fConst9;
	float 	fVec3[2];
	float 	fRec29[2];
	float 	fRec28[2];
	float 	fRec27[4096];
	float 	fVec4[2];
	float 	fRec26[2];
	float 	fRec14[2];
	float 	fRec15[2];
	int 	iConst10;
	float 	fConst11;
	float 	fConst12;
	float 	fRec30[3];
	float 	fConst13;
	float 	fVec5[4096];
	FAUSTFLOAT 	fslider4;
	FAUSTFLOAT 	fslider5;
	float 	fConst14;
  public:
	static void metadata(Meta* m) 	{ 
		m->declare("name", "Commuted Piano");
		m->declare("description", "WaveGuide Commuted Piano");
		m->declare("author", "Romain Michon (rmichon@ccrma.stanford.edu)");
		m->declare("copyright", "Romain Michon");
		m->declare("version", "1.0");
		m->declare("licence", "STK-4.3");
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
		m->declare("instrument.lib/name", "Faust-STK Tools Library");
		m->declare("instrument.lib/author", "Romain Michon (rmichon@ccrma.stanford.edu)");
		m->declare("instrument.lib/copyright", "Romain Michon");
		m->declare("instrument.lib/version", "1.0");
		m->declare("instrument.lib/licence", "STK-4.3");
		m->declare("filter.lib/name", "Faust Filter Library");
		m->declare("filter.lib/author", "Julius O. Smith (jos at ccrma.stanford.edu)");
		m->declare("filter.lib/copyright", "Julius O. Smith III");
		m->declare("filter.lib/version", "1.29");
		m->declare("filter.lib/license", "STK-4.3");
		m->declare("filter.lib/reference", "https://ccrma.stanford.edu/~jos/filters/");
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

	virtual int getNumInputs() 	{ return 0; }
	virtual int getNumOutputs() 	{ return 2; }
	static void classInit(int samplingFreq) {
	}
	virtual void instanceInit(int samplingFreq) {
		fSamplingFreq = samplingFreq;
		fentry0 = 4.4e+02f;
		iConst0 = min(192000, max(1, fSamplingFreq));
		fConst1 = (0.05f / float(iConst0));
		fslider0 = 0.0f;
		fbutton0 = 0.0;
		fslider1 = 0.1f;
		fConst2 = float(iConst0);
		fConst3 = (0.1f * fConst2);
		for (int i=0; i<2; i++) fRec11[i] = 0;
		fConst4 = expf((0 - (5.0f / fConst2)));
		fConst5 = expf((0 - (0.5f / fConst2)));
		fConst6 = (1e+01f / fConst2);
		for (int i=0; i<2; i++) fRec10[i] = 0;
		fentry1 = 1.0f;
		fConst7 = (float(7) / fConst2);
		for (int i=0; i<2; i++) fRec12[i] = 0;
		for (int i=0; i<2; i++) iRec13[i] = 0;
		for (int i=0; i<2; i++) fVec0[i] = 0;
		for (int i=0; i<2; i++) fVec1[i] = 0;
		for (int i=0; i<2; i++) fRec9[i] = 0;
		for (int i=0; i<2; i++) fRec8[i] = 0;
		for (int i=0; i<2; i++) fRec7[i] = 0;
		for (int i=0; i<2; i++) fRec6[i] = 0;
		for (int i=0; i<2; i++) fRec5[i] = 0;
		fConst8 = (6.283185307179586f / float(iConst0));
		for (int i=0; i<3; i++) fRec4[i] = 0;
		for (int i=0; i<3; i++) fRec3[i] = 0;
		for (int i=0; i<3; i++) fRec2[i] = 0;
		for (int i=0; i<3; i++) fRec1[i] = 0;
		for (int i=0; i<2; i++) fRec0[i] = 0;
		for (int i=0; i<2; i++) fRec24[i] = 0;
		for (int i=0; i<2; i++) fRec23[i] = 0;
		for (int i=0; i<2; i++) fRec22[i] = 0;
		for (int i=0; i<2; i++) fRec21[i] = 0;
		for (int i=0; i<2; i++) fRec20[i] = 0;
		for (int i=0; i<2; i++) fRec25[i] = 0;
		for (int i=0; i<2; i++) fVec2[i] = 0;
		fslider2 = 0.28f;
		for (int i=0; i<2; i++) fRec19[i] = 0;
		for (int i=0; i<2; i++) fRec18[i] = 0;
		IOTA = 0;
		for (int i=0; i<4096; i++) fRec17[i] = 0;
		fslider3 = 0.1f;
		fConst9 = (0.15915494309189535f * iConst0);
		for (int i=0; i<2; i++) fVec3[i] = 0;
		for (int i=0; i<2; i++) fRec29[i] = 0;
		for (int i=0; i<2; i++) fRec28[i] = 0;
		for (int i=0; i<4096; i++) fRec27[i] = 0;
		for (int i=0; i<2; i++) fVec4[i] = 0;
		for (int i=0; i<2; i++) fRec26[i] = 0;
		for (int i=0; i<2; i++) fRec14[i] = 0;
		for (int i=0; i<2; i++) fRec15[i] = 0;
		iConst10 = faustpower<2>(iConst0);
		fConst11 = (1.0f / float(iConst10));
		fConst12 = (2.0f / float(iConst0));
		for (int i=0; i<3; i++) fRec30[i] = 0;
		fConst13 = (0.5f / float(iConst10));
		for (int i=0; i<4096; i++) fVec5[i] = 0;
		fslider4 = 0.6f;
		fslider5 = 0.5f;
		fConst14 = (0.5f * iConst0);
	}
	virtual void init(int samplingFreq) {
		classInit(samplingFreq);
		instanceInit(samplingFreq);
	}
	virtual void buildUserInterface(UI* faust_interface) {
		faust_interface->openVerticalBox("0x00");
		faust_interface->openHorizontalBox("Basic_Parameters");
		faust_interface->declare(&fentry0, "1", "");
		faust_interface->declare(&fentry0, "tooltip", "Tone frequency");
		faust_interface->declare(&fentry0, "unit", "Hz");
		faust_interface->addNumEntry("freq", &fentry0, 4.4e+02f, 2e+01f, 2e+04f, 1.0f);
		faust_interface->declare(&fentry1, "1", "");
		faust_interface->declare(&fentry1, "tooltip", "Gain (value between 0 and 1)");
		faust_interface->addNumEntry("gain", &fentry1, 1.0f, 0.0f, 1.0f, 0.01f);
		faust_interface->declare(&fbutton0, "1", "");
		faust_interface->declare(&fbutton0, "tooltip", "noteOn = 1, noteOff = 0");
		faust_interface->addButton("gate", &fbutton0);
		faust_interface->closeBox();
		faust_interface->openVerticalBox("Physical_Parameters");
		faust_interface->declare(&fslider0, "2", "");
		faust_interface->declare(&fslider0, "tooltip", "A value between 0 and 1");
		faust_interface->addHorizontalSlider("Brightness_Factor", &fslider0, 0.0f, 0.0f, 1.0f, 0.01f);
		faust_interface->declare(&fslider3, "2", "");
		faust_interface->declare(&fslider3, "tooltip", "A value between 0 and 1");
		faust_interface->addHorizontalSlider("Detuning_Factor", &fslider3, 0.1f, 0.0f, 1.0f, 0.01f);
		faust_interface->declare(&fslider1, "2", "");
		faust_interface->declare(&fslider1, "tooltip", "A value between 0 and 1");
		faust_interface->addHorizontalSlider("Hammer_Hardness", &fslider1, 0.1f, 0.0f, 1.0f, 0.01f);
		faust_interface->declare(&fslider2, "2", "");
		faust_interface->declare(&fslider2, "tooltip", "A value between 0 and 1");
		faust_interface->addHorizontalSlider("Stiffness_Factor", &fslider2, 0.28f, 0.0f, 1.0f, 0.01f);
		faust_interface->closeBox();
		faust_interface->openVerticalBox("Spat");
		faust_interface->addHorizontalSlider("pan angle", &fslider4, 0.6f, 0.0f, 1.0f, 0.01f);
		faust_interface->addHorizontalSlider("spatial width", &fslider5, 0.5f, 0.0f, 1.0f, 0.01f);
		faust_interface->closeBox();
		faust_interface->closeBox();
	}
	virtual void compute (int count, FAUSTFLOAT** input, FAUSTFLOAT** output) {
		float 	fSlow0 = float(fentry0);
		int 	iSlow1 = int(((17.31234049066756f * (logf(fSlow0) - 6.0867747269123065f)) + 69.5f));
		float 	fSlow2 = getValueDCBa1(iSlow1);
		float 	fSlow3 = powf(10,(fConst1 * getValuer1_2db(iSlow1)));
		float 	fSlow4 = faustpower<2>(fSlow3);
		float 	fSlow5 = getValueLoudPole(iSlow1);
		float 	fSlow6 = (0.25f * float(fslider0));
		float 	fSlow7 = (fSlow6 - (0.02f + fSlow5));
		float 	fSlow8 = float(fbutton0);
		int 	iSlow9 = (fSlow8 > 0);
		float 	fSlow10 = float(fslider1);
		float 	fSlow11 = (fConst3 * fSlow10);
		float 	fSlow12 = (0 - (fSlow8 - 1));
		float 	fSlow13 = (fConst4 * fSlow12);
		float 	fSlow14 = (fSlow13 - 1);
		float 	fSlow15 = expf((0 - (fConst6 / fSlow10)));
		float 	fSlow16 = (0.2f * getValueSustainPedalLevel(iSlow1));
		int 	iSlow17 = (fSlow8 < 1);
		float 	fSlow18 = expf((0 - (fConst7 / (float(fentry1) * getValueDryTapAmpT60(iSlow1)))));
		int 	iSlow19 = (iSlow1 >= 88);
		float 	fSlow20 = (1.1641532188114492e-10f * iSlow19);
		float 	fSlow21 = (2.3283064376228985e-10f * iSlow19);
		float 	fSlow22 = (1 - fSlow2);
		float 	fSlow23 = (0.5f * fSlow22);
		float 	fSlow24 = (0 - fSlow23);
		float 	fSlow25 = (((fSlow6 + 0.98f) - fSlow5) * getValueLoudGain(iSlow1));
		float 	fSlow26 = (2.0f * getValueBq4_gEarBalled(iSlow1));
		float 	fSlow27 = powf(10,(fConst1 * getValuer3db(iSlow1)));
		float 	fSlow28 = faustpower<2>(fSlow27);
		float 	fSlow29 = ((0 - (2 * fSlow27)) * cosf((fConst8 * (fSlow0 * getValueThirdPartialFactor(iSlow1)))));
		float 	fSlow30 = powf(10,(fConst1 * getValuer2db(iSlow1)));
		float 	fSlow31 = faustpower<2>(fSlow30);
		float 	fSlow32 = ((0 - (2 * fSlow30)) * cosf((fConst8 * (fSlow0 * getValueSecondPartialFactor(iSlow1)))));
		float 	fSlow33 = powf(10,(fConst1 * getValuer1_1db(iSlow1)));
		float 	fSlow34 = faustpower<2>(fSlow33);
		float 	fSlow35 = cosf((fConst8 * fSlow0));
		float 	fSlow36 = (fSlow35 * (0 - (2 * fSlow33)));
		float 	fSlow37 = powf(10,(0.05f * getValueSecondStageAmpRatio(iSlow1)));
		float 	fSlow38 = (1 - fSlow37);
		float 	fSlow39 = ((fSlow37 * fSlow34) + (fSlow4 * fSlow38));
		float 	fSlow40 = (0 - (2 * fSlow3));
		float 	fSlow41 = (0 - (2 * ((fSlow37 * fSlow33) + (fSlow3 * fSlow38))));
		float 	fSlow42 = (1.396983862573739e-09f * (fSlow25 * (iSlow1 < 88)));
		float 	fSlow43 = (0.0010000000000000009f * ((0.9996f * fSlow8) + (0.9f * (fSlow12 * getValueReleaseLoopGain(iSlow1)))));
		float 	fSlow44 = getValueStiffnessCoefficient(iSlow1);
		float 	fSlow45 = float(fslider2);
		float 	fSlow46 = (fSlow45 * fSlow44);
		float 	fSlow47 = (5.0f * (float(fslider3) * getValueDetuningHz(iSlow1)));
		float 	fSlow48 = (fSlow0 + fSlow47);
		float 	fSlow49 = getValueSingleStringPole(iSlow1);
		float 	fSlow50 = (powf(10,(0.05f * (getValueSingleStringDecayRate(iSlow1) / fSlow0))) * (1 - fSlow49));
		float 	fSlow51 = getValueSingleStringZero(iSlow1);
		float 	fSlow52 = (1 - fSlow51);
		float 	fSlow53 = ((3 * fSlow52) - fSlow50);
		float 	fSlow54 = (fSlow49 * fSlow52);
		float 	fSlow55 = (3 * fSlow54);
		float 	fSlow56 = (fSlow50 * fSlow51);
		float 	fSlow57 = (fSlow54 - fSlow56);
		float 	fSlow58 = (4 * fSlow57);
		float 	fSlow59 = ((fSlow56 + fSlow58) - fSlow55);
		float 	fSlow60 = (fConst8 * fSlow48);
		float 	fSlow61 = cosf(fSlow60);
		float 	fSlow62 = ((fSlow50 + fSlow51) - 1);
		float 	fSlow63 = (4 * fSlow62);
		float 	fSlow64 = (1 + ((fSlow63 + (fSlow61 * fSlow59)) / fSlow53));
		float 	fSlow65 = (fSlow56 - fSlow55);
		float 	fSlow66 = (0 - (fSlow59 / fSlow53));
		float 	fSlow67 = (1 + ((fSlow65 * fSlow61) / fSlow53));
		float 	fSlow68 = sinf(fSlow60);
		float 	fSlow69 = faustpower<2>(fSlow53);
		float 	fSlow70 = faustpower<2>(fSlow68);
		float 	fSlow71 = (13.690000000000001f * (faustpower<2>(fSlow45) * faustpower<2>(fSlow44)));
		float 	fSlow72 = (fSlow71 - 1.0f);
		float 	fSlow73 = (1.0f + fSlow71);
		float 	fSlow74 = (7.4f * fSlow46);
		float 	fSlow75 = (3 * atan2f((fSlow72 * fSlow68),(fSlow74 + (fSlow73 * fSlow61))));
		int 	iSlow76 = int((fConst9 * ((6.283185307179586f + (fSlow75 + atan2f((fSlow68 * ((fSlow67 * fSlow66) + ((fSlow65 * fSlow64) / fSlow53))),((fSlow67 * fSlow64) + (((fSlow65 * fSlow70) * fSlow59) / fSlow69))))) / fSlow48)));
		int 	iSlow77 = int((iSlow76 & 4095));
		float 	fSlow78 = (fSlow65 + fSlow58);
		float 	fSlow79 = (1 + ((fSlow63 + (fSlow78 * fSlow61)) / fSlow53));
		float 	fSlow80 = (0 - (fSlow78 / fSlow53));
		float 	fSlow81 = (fSlow65 * fSlow78);
		float 	fSlow82 = (fConst9 * ((6.283185307179586f + (fSlow75 + atan2f((fSlow68 * ((fSlow80 * fSlow67) + ((fSlow65 * fSlow79) / fSlow53))),((fSlow67 * fSlow79) + ((fSlow81 * fSlow70) / fSlow69))))) / fSlow48));
		int 	iSlow83 = int(fSlow82);
		float 	fSlow84 = ((1 + iSlow83) - fSlow82);
		float 	fSlow85 = (fSlow0 - fSlow47);
		float 	fSlow86 = (fConst8 * fSlow85);
		float 	fSlow87 = cosf(fSlow86);
		float 	fSlow88 = (1 + ((fSlow63 + (fSlow87 * fSlow59)) / fSlow53));
		float 	fSlow89 = (1 + ((fSlow65 * fSlow87) / fSlow53));
		float 	fSlow90 = sinf(fSlow86);
		float 	fSlow91 = faustpower<2>(fSlow90);
		float 	fSlow92 = (3 * atan2f((fSlow72 * fSlow90),(fSlow74 + (fSlow73 * fSlow87))));
		int 	iSlow93 = int((fConst9 * ((6.283185307179586f + (fSlow92 + atan2f((fSlow90 * ((fSlow89 * fSlow66) + ((fSlow65 * fSlow88) / fSlow53))),((fSlow89 * fSlow88) + (((fSlow65 * fSlow91) * fSlow59) / fSlow69))))) / fSlow85)));
		int 	iSlow94 = int((int((1 + iSlow93)) & 4095));
		float 	fSlow95 = (1 + (((fSlow78 * fSlow87) + fSlow63) / fSlow53));
		float 	fSlow96 = (fConst9 * ((6.283185307179586f + (fSlow92 + atan2f((fSlow90 * ((fSlow80 * fSlow89) + ((fSlow65 * fSlow95) / fSlow53))),((fSlow89 * fSlow95) + ((fSlow81 * fSlow91) / fSlow69))))) / fSlow85));
		int 	iSlow97 = int(fSlow96);
		float 	fSlow98 = (fSlow96 - iSlow97);
		int 	iSlow99 = int((iSlow93 & 4095));
		float 	fSlow100 = ((1 + iSlow97) - fSlow96);
		int 	iSlow101 = int((int((1 + iSlow76)) & 4095));
		float 	fSlow102 = (fSlow82 - iSlow83);
		float 	fSlow103 = (1.0f / fSlow53);
		float 	fSlow104 = getValueEQBandWidthFactor(iSlow1);
		float 	fSlow105 = (faustpower<2>(fSlow0) * faustpower<2>(fSlow104));
		float 	fSlow106 = (fConst11 * fSlow105);
		float 	fSlow107 = ((0 - (fConst12 * (fSlow0 * fSlow104))) * cosf((fConst8 * (fSlow0 / getValueStrikePosition(iSlow1)))));
		float 	fSlow108 = getValueEQGain(iSlow1);
		float 	fSlow109 = (fConst13 * fSlow105);
		float 	fSlow110 = (0.5f - fSlow109);
		float 	fSlow111 = (fSlow109 - 0.5f);
		float 	fSlow112 = float(fslider4);
		float 	fSlow113 = (12 * (1.0f - fSlow112));
		int 	iSlow114 = int((int((fConst14 * (float(fslider5) / fSlow0))) & 4095));
		float 	fSlow115 = (12 * fSlow112);
		FAUSTFLOAT* output0 = output[0];
		FAUSTFLOAT* output1 = output[1];
		for (int i=0; i<count; i++) {
			fRec11[0] = (1 + (fSlow8 * fRec11[1]));
			float fTemp0 = (fRec11[0] - 1);
			int iTemp1 = (fTemp0 < fSlow11);
			float fTemp2 = (fSlow8 * ((fSlow15 * iTemp1) + (fConst5 * (fTemp0 >= fSlow11))));
			fRec10[0] = ((fRec10[1] * (fTemp2 + fSlow13)) + (fSlow16 * ((0 - (fTemp2 + fSlow14)) * (iTemp1 & iSlow9))));
			int iTemp3 = ((fTemp0 < 2.0f) & iSlow9);
			float fTemp4 = ((0.0301973834223185f * iTemp3) + (fSlow18 * ((fTemp0 >= 2.0f) | iSlow17)));
			fRec12[0] = ((fRec12[1] * fTemp4) + (0.15f * (iTemp3 * (1 - fTemp4))));
			iRec13[0] = (12345 + (1103515245 * iRec13[1]));
			float fTemp5 = (iRec13[0] * (fRec12[0] + fRec10[0]));
			fVec0[0] = (fSlow21 * fTemp5);
			float fTemp6 = (0 - ((0.5f * fVec0[1]) + (fSlow20 * fTemp5)));
			fVec1[0] = fTemp6;
			fRec9[0] = (((fSlow23 * fVec1[0]) + (fSlow24 * fVec1[1])) - (fSlow2 * fRec9[1]));
			fRec8[0] = ((fSlow25 * fRec9[0]) - (fSlow7 * fRec8[1]));
			fRec7[0] = ((fSlow25 * fRec8[0]) - (fSlow7 * fRec7[1]));
			fRec6[0] = ((fSlow25 * fRec7[0]) - (fSlow7 * fRec6[1]));
			fRec5[0] = ((fSlow25 * fRec6[0]) - (fSlow7 * fRec5[1]));
			fRec4[0] = (0 - (((fSlow29 * fRec4[1]) + (fSlow28 * fRec4[2])) - (fSlow26 * ((0.5f * fRec5[0]) - (0.5f * fRec5[1])))));
			fRec3[0] = (0 - (((fSlow32 * fRec3[1]) + (fSlow31 * fRec3[2])) - (fSlow26 * fRec4[0])));
			fRec2[0] = (0 - (((fSlow36 * fRec2[1]) + (fSlow34 * fRec2[2])) - fRec3[0]));
			fRec1[0] = (((fSlow35 * ((fSlow41 * fRec2[1]) - (fSlow40 * fRec1[1]))) + (fRec2[0] + (fSlow39 * fRec2[2]))) - (fSlow4 * fRec1[2]));
			fRec0[0] = ((fSlow22 * fRec1[0]) - (fSlow2 * fRec0[1]));
			fRec24[0] = ((fSlow42 * fTemp5) - (fSlow7 * fRec24[1]));
			fRec23[0] = ((fSlow25 * fRec24[0]) - (fSlow7 * fRec23[1]));
			fRec22[0] = ((fSlow25 * fRec23[0]) - (fSlow7 * fRec22[1]));
			fRec21[0] = ((fSlow25 * fRec22[0]) - (fSlow7 * fRec21[1]));
			fRec20[0] = (((fSlow23 * fRec21[0]) + (fSlow24 * fRec21[1])) - (fSlow2 * fRec20[1]));
			fRec25[0] = ((0.999f * fRec25[1]) + fSlow43);
			float fTemp7 = (fRec25[0] * (fRec20[0] + fRec14[1]));
			fVec2[0] = fTemp7;
			fRec19[0] = (fVec2[1] + (fSlow46 * ((3.7f * fVec2[0]) - (3.7f * fRec19[1]))));
			fRec18[0] = (fRec19[1] + (fSlow46 * ((3.7f * fRec19[0]) - (3.7f * fRec18[1]))));
			fRec17[IOTA&4095] = (fRec18[1] + (fSlow46 * ((3.7f * fRec18[0]) - (3.7f * fRec17[(IOTA-1)&4095]))));
			float fTemp8 = (fSlow84 * fRec17[(IOTA-iSlow77)&4095]);
			float fTemp9 = (fRec20[0] + (fRec25[0] * fRec15[1]));
			fVec3[0] = fTemp9;
			fRec29[0] = (fVec3[1] + (fSlow46 * ((3.7f * fVec3[0]) - (3.7f * fRec29[1]))));
			fRec28[0] = (fRec29[1] + (fSlow46 * ((3.7f * fRec29[0]) - (3.7f * fRec28[1]))));
			fRec27[IOTA&4095] = (fRec28[1] + (fSlow46 * ((3.7f * fRec28[0]) - (3.7f * fRec27[(IOTA-1)&4095]))));
			float fTemp10 = (fSlow98 * fRec27[(IOTA-iSlow94)&4095]);
			float fTemp11 = (fSlow100 * fRec27[(IOTA-iSlow99)&4095]);
			float fTemp12 = (fSlow102 * fRec17[(IOTA-iSlow101)&4095]);
			float fTemp13 = (fTemp12 + ((fTemp11 + fTemp10) + fTemp8));
			fVec4[0] = fTemp13;
			fRec26[0] = (fSlow103 * ((2 * ((fSlow62 * fVec4[0]) + (fSlow57 * fVec4[1]))) - (fSlow65 * fRec26[1])));
			fRec14[0] = (fTemp12 + (fRec26[0] + fTemp8));
			fRec15[0] = (fTemp10 + (fRec26[0] + fTemp11));
			float 	fRec16 = fVec4[0];
			fRec30[0] = ((fSlow108 * fRec16) - ((fSlow107 * fRec30[1]) + (fSlow106 * fRec30[2])));
			float fTemp14 = ((fSlow111 * fRec30[2]) + ((fSlow110 * fRec30[0]) + (fRec16 + fRec0[0])));
			fVec5[IOTA&4095] = fTemp14;
			output0[i] = (FAUSTFLOAT)(fSlow113 * fVec5[IOTA&4095]);
			output1[i] = (FAUSTFLOAT)(fSlow115 * fVec5[(IOTA-iSlow114)&4095]);
			// post processing
			fRec30[2] = fRec30[1]; fRec30[1] = fRec30[0];
			fRec15[1] = fRec15[0];
			fRec14[1] = fRec14[0];
			fRec26[1] = fRec26[0];
			fVec4[1] = fVec4[0];
			fRec28[1] = fRec28[0];
			fRec29[1] = fRec29[0];
			fVec3[1] = fVec3[0];
			IOTA = IOTA+1;
			fRec18[1] = fRec18[0];
			fRec19[1] = fRec19[0];
			fVec2[1] = fVec2[0];
			fRec25[1] = fRec25[0];
			fRec20[1] = fRec20[0];
			fRec21[1] = fRec21[0];
			fRec22[1] = fRec22[0];
			fRec23[1] = fRec23[0];
			fRec24[1] = fRec24[0];
			fRec0[1] = fRec0[0];
			fRec1[2] = fRec1[1]; fRec1[1] = fRec1[0];
			fRec2[2] = fRec2[1]; fRec2[1] = fRec2[0];
			fRec3[2] = fRec3[1]; fRec3[1] = fRec3[0];
			fRec4[2] = fRec4[1]; fRec4[1] = fRec4[0];
			fRec5[1] = fRec5[0];
			fRec6[1] = fRec6[0];
			fRec7[1] = fRec7[0];
			fRec8[1] = fRec8[0];
			fRec9[1] = fRec9[0];
			fVec1[1] = fVec1[0];
			fVec0[1] = fVec0[0];
			iRec13[1] = iRec13[0];
			fRec12[1] = fRec12[0];
			fRec10[1] = fRec10[0];
			fRec11[1] = fRec11[0];
		}
	}
};




#include "Faust_plugins_template2.cpp"

