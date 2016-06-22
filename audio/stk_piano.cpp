/* ------------------------------------------------------------
author: "Romain Michon (rmichon@ccrma.stanford.edu)"
copyright: "Romain Michon"
name: "Commuted Piano"
version: "1.0"
Code generated with Faust 2.0.a43 (http://faust.grame.fr)
------------------------------------------------------------ */

#ifndef  __Piano_dsp_H__
#define  __Piano_dsp_H__

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
#include <math.h>
#include <piano.h>

static float faustpower2_f(float value) {
	return (value * value);
	
}

#ifndef FAUSTCLASS 
#define FAUSTCLASS Piano_dsp
#endif

class Piano_dsp : public dsp {
	
  private:
	
	float fRec4[4096];
	float fRec18[4096];
	float fVec5[4096];
	float fRec0[3];
	float fRec25[3];
	float fRec24[3];
	float fRec23[3];
	float fRec22[3];
	float fRec7[2];
	int iRec13[2];
	float fRec15[2];
	float fRec14[2];
	float fRec16[2];
	float fRec12[2];
	float fRec11[2];
	float fRec10[2];
	float fRec9[2];
	float fRec8[2];
	float fVec0[2];
	float fRec6[2];
	float fRec5[2];
	float fVec1[2];
	float fRec20[2];
	float fRec19[2];
	float fVec2[2];
	float fRec17[2];
	float fRec1[2];
	float fRec2[2];
	float fVec3[2];
	float fVec4[2];
	float fRec30[2];
	float fRec29[2];
	float fRec28[2];
	float fRec27[2];
	float fRec26[2];
	float fRec21[2];
	FAUSTFLOAT fHslider0;
	int fSamplingFreq;
	float fConst0;
	float fConst1;
	float fConst2;
	FAUSTFLOAT fEntry0;
	float fConst3;
	FAUSTFLOAT fHslider1;
	float fConst4;
	FAUSTFLOAT fHslider2;
	FAUSTFLOAT fButton0;
	FAUSTFLOAT fHslider3;
	float fConst5;
	FAUSTFLOAT fEntry1;
	float fConst6;
	FAUSTFLOAT fHslider4;
	float fConst7;
	float fConst8;
	float fConst9;
	int IOTA;
	float fConst10;
	float fConst11;
	float fConst12;
	float fConst13;
	FAUSTFLOAT fHslider5;
	
  public:
	
	void static metadata(Meta* m) { 
		m->declare("author", "Romain Michon (rmichon@ccrma.stanford.edu)");
		m->declare("copyright", "Romain Michon");
		m->declare("description", "WaveGuide Commuted Piano");
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
		m->declare("instrument.lib/author", "Romain Michon (rmichon@ccrma.stanford.edu)");
		m->declare("instrument.lib/copyright", "Romain Michon");
		m->declare("instrument.lib/licence", "STK-4.3");
		m->declare("instrument.lib/name", "Faust-STK Tools Library");
		m->declare("instrument.lib/version", "1.0");
		m->declare("licence", "STK-4.3");
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
		m->declare("name", "Commuted Piano");
		m->declare("version", "1.0");
	}

	virtual int getNumInputs() {
		return 0;
		
	}
	virtual int getNumOutputs() {
		return 2;
		
	}
	virtual int getInputRate(int channel) {
		int rate;
		switch (channel) {
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
		fHslider0 = FAUSTFLOAT(0.6f);
		fConst0 = min(1.92e+05f, max(1.0f, float(fSamplingFreq)));
		fConst1 = faustpower2_f(fConst0);
		fConst2 = (0.5f / fConst1);
		fEntry0 = FAUSTFLOAT(4.4e+02f);
		fConst3 = (0.15915494f * fConst0);
		fHslider1 = FAUSTFLOAT(0.28f);
		fConst4 = (6.2831855f / fConst0);
		fHslider2 = FAUSTFLOAT(0.1f);
		fButton0 = FAUSTFLOAT(0.0f);
		for (int i0 = 0; (i0 < 2); i0 = (i0 + 1)) {
			fRec7[i0] = 0.0f;
			
		}
		fHslider3 = FAUSTFLOAT(0.0f);
		for (int i1 = 0; (i1 < 2); i1 = (i1 + 1)) {
			iRec13[i1] = 0;
			
		}
		for (int i2 = 0; (i2 < 2); i2 = (i2 + 1)) {
			fRec15[i2] = 0.0f;
			
		}
		fConst5 = (7.0f / fConst0);
		fEntry1 = FAUSTFLOAT(1.0f);
		for (int i3 = 0; (i3 < 2); i3 = (i3 + 1)) {
			fRec14[i3] = 0.0f;
			
		}
		fConst6 = (1e+01f / fConst0);
		fHslider4 = FAUSTFLOAT(0.1f);
		fConst7 = (0.1f * fConst0);
		fConst8 = expf((0.0f - (0.5f / fConst0)));
		fConst9 = expf((0.0f - (5.0f / fConst0)));
		for (int i4 = 0; (i4 < 2); i4 = (i4 + 1)) {
			fRec16[i4] = 0.0f;
			
		}
		for (int i5 = 0; (i5 < 2); i5 = (i5 + 1)) {
			fRec12[i5] = 0.0f;
			
		}
		for (int i6 = 0; (i6 < 2); i6 = (i6 + 1)) {
			fRec11[i6] = 0.0f;
			
		}
		for (int i7 = 0; (i7 < 2); i7 = (i7 + 1)) {
			fRec10[i7] = 0.0f;
			
		}
		for (int i8 = 0; (i8 < 2); i8 = (i8 + 1)) {
			fRec9[i8] = 0.0f;
			
		}
		for (int i9 = 0; (i9 < 2); i9 = (i9 + 1)) {
			fRec8[i9] = 0.0f;
			
		}
		for (int i10 = 0; (i10 < 2); i10 = (i10 + 1)) {
			fVec0[i10] = 0.0f;
			
		}
		for (int i11 = 0; (i11 < 2); i11 = (i11 + 1)) {
			fRec6[i11] = 0.0f;
			
		}
		for (int i12 = 0; (i12 < 2); i12 = (i12 + 1)) {
			fRec5[i12] = 0.0f;
			
		}
		IOTA = 0;
		for (int i13 = 0; (i13 < 4096); i13 = (i13 + 1)) {
			fRec4[i13] = 0.0f;
			
		}
		for (int i14 = 0; (i14 < 2); i14 = (i14 + 1)) {
			fVec1[i14] = 0.0f;
			
		}
		for (int i15 = 0; (i15 < 2); i15 = (i15 + 1)) {
			fRec20[i15] = 0.0f;
			
		}
		for (int i16 = 0; (i16 < 2); i16 = (i16 + 1)) {
			fRec19[i16] = 0.0f;
			
		}
		for (int i17 = 0; (i17 < 4096); i17 = (i17 + 1)) {
			fRec18[i17] = 0.0f;
			
		}
		for (int i18 = 0; (i18 < 2); i18 = (i18 + 1)) {
			fVec2[i18] = 0.0f;
			
		}
		for (int i19 = 0; (i19 < 2); i19 = (i19 + 1)) {
			fRec17[i19] = 0.0f;
			
		}
		for (int i20 = 0; (i20 < 2); i20 = (i20 + 1)) {
			fRec1[i20] = 0.0f;
			
		}
		for (int i21 = 0; (i21 < 2); i21 = (i21 + 1)) {
			fRec2[i21] = 0.0f;
			
		}
		fConst10 = (2.0f / fConst0);
		fConst11 = (1.0f / fConst1);
		for (int i22 = 0; (i22 < 3); i22 = (i22 + 1)) {
			fRec0[i22] = 0.0f;
			
		}
		fConst12 = (0.05f / fConst0);
		for (int i23 = 0; (i23 < 2); i23 = (i23 + 1)) {
			fVec3[i23] = 0.0f;
			
		}
		for (int i24 = 0; (i24 < 2); i24 = (i24 + 1)) {
			fVec4[i24] = 0.0f;
			
		}
		for (int i25 = 0; (i25 < 2); i25 = (i25 + 1)) {
			fRec30[i25] = 0.0f;
			
		}
		for (int i26 = 0; (i26 < 2); i26 = (i26 + 1)) {
			fRec29[i26] = 0.0f;
			
		}
		for (int i27 = 0; (i27 < 2); i27 = (i27 + 1)) {
			fRec28[i27] = 0.0f;
			
		}
		for (int i28 = 0; (i28 < 2); i28 = (i28 + 1)) {
			fRec27[i28] = 0.0f;
			
		}
		for (int i29 = 0; (i29 < 2); i29 = (i29 + 1)) {
			fRec26[i29] = 0.0f;
			
		}
		for (int i30 = 0; (i30 < 3); i30 = (i30 + 1)) {
			fRec25[i30] = 0.0f;
			
		}
		for (int i31 = 0; (i31 < 3); i31 = (i31 + 1)) {
			fRec24[i31] = 0.0f;
			
		}
		for (int i32 = 0; (i32 < 3); i32 = (i32 + 1)) {
			fRec23[i32] = 0.0f;
			
		}
		for (int i33 = 0; (i33 < 3); i33 = (i33 + 1)) {
			fRec22[i33] = 0.0f;
			
		}
		for (int i34 = 0; (i34 < 2); i34 = (i34 + 1)) {
			fRec21[i34] = 0.0f;
			
		}
		for (int i35 = 0; (i35 < 4096); i35 = (i35 + 1)) {
			fVec5[i35] = 0.0f;
			
		}
		fConst13 = (0.5f * fConst0);
		fHslider5 = FAUSTFLOAT(0.5f);
		
	}
	
	virtual void init(int samplingFreq) {
		classInit(samplingFreq);
		instanceInit(samplingFreq);
	}
	
	virtual void buildUserInterface(UI* interface) {
		interface->openVerticalBox("0x00");
		interface->openHorizontalBox("Basic_Parameters");
		interface->declare(&fEntry0, "1", "");
		interface->declare(&fEntry0, "tooltip", "Tone frequency");
		interface->declare(&fEntry0, "unit", "Hz");
		interface->addNumEntry("freq", &fEntry0, 4.4e+02f, 2e+01f, 2e+04f, 1.0f);
		interface->declare(&fEntry1, "1", "");
		interface->declare(&fEntry1, "tooltip", "Gain (value between 0 and 1)");
		interface->addNumEntry("gain", &fEntry1, 1.0f, 0.0f, 1.0f, 0.01f);
		interface->declare(&fButton0, "1", "");
		interface->declare(&fButton0, "tooltip", "noteOn = 1, noteOff = 0");
		interface->addButton("gate",&fButton0);
		interface->closeBox();
		interface->openVerticalBox("Physical_Parameters");
		interface->declare(&fHslider3, "2", "");
		interface->declare(&fHslider3, "tooltip", "A value between 0 and 1");
		interface->addHorizontalSlider("Brightness_Factor", &fHslider3, 0.0f, 0.0f, 1.0f, 0.01f);
		interface->declare(&fHslider2, "2", "");
		interface->declare(&fHslider2, "tooltip", "A value between 0 and 1");
		interface->addHorizontalSlider("Detuning_Factor", &fHslider2, 0.1f, 0.0f, 1.0f, 0.01f);
		interface->declare(&fHslider4, "2", "");
		interface->declare(&fHslider4, "tooltip", "A value between 0 and 1");
		interface->addHorizontalSlider("Hammer_Hardness", &fHslider4, 0.1f, 0.0f, 1.0f, 0.01f);
		interface->declare(&fHslider1, "2", "");
		interface->declare(&fHslider1, "tooltip", "A value between 0 and 1");
		interface->addHorizontalSlider("Stiffness_Factor", &fHslider1, 0.28f, 0.0f, 1.0f, 0.01f);
		interface->closeBox();
		interface->openVerticalBox("Spat");
		interface->addHorizontalSlider("pan angle", &fHslider0, 0.6f, 0.0f, 1.0f, 0.01f);
		interface->addHorizontalSlider("spatial width", &fHslider5, 0.5f, 0.0f, 1.0f, 0.01f);
		interface->closeBox();
		interface->closeBox();
		
	}
	
	virtual void compute(int count, FAUSTFLOAT** inputs, FAUSTFLOAT** outputs) {
		FAUSTFLOAT* output0 = outputs[0];
		FAUSTFLOAT* output1 = outputs[1];
		float fSlow0 = float(fHslider0);
		float fSlow1 = (12.0f * (1.0f - fSlow0));
		float fSlow2 = float(fEntry0);
		int iSlow3 = int(((17.31234f * (logf(fSlow2) - 6.086775f)) + 69.5f));
		float fSlow4 = float(getValueEQBandWidthFactor(float(iSlow3)));
		float fSlow5 = (faustpower2_f(fSlow2) * faustpower2_f(fSlow4));
		float fSlow6 = (fConst2 * fSlow5);
		float fSlow7 = (fSlow6 - 0.5f);
		float fSlow8 = float(getValueEQGain(float(iSlow3)));
		float fSlow9 = float(fHslider1);
		float fSlow10 = float(getValueStiffnessCoefficient(float(iSlow3)));
		float fSlow11 = (13.69f * (faustpower2_f(fSlow9) * faustpower2_f(fSlow10)));
		float fSlow12 = (fSlow11 - 1.0f);
		float fSlow13 = (5.0f * (float(fHslider2) * float(getValueDetuningHz(float(iSlow3)))));
		float fSlow14 = (fSlow2 + fSlow13);
		float fSlow15 = (fConst4 * fSlow14);
		float fSlow16 = sinf(fSlow15);
		float fSlow17 = (fSlow9 * fSlow10);
		float fSlow18 = (7.4f * fSlow17);
		float fSlow19 = (1.0f + fSlow11);
		float fSlow20 = cosf(fSlow15);
		float fSlow21 = (3.0f * atan2f((fSlow12 * fSlow16), (fSlow18 + (fSlow19 * fSlow20))));
		float fSlow22 = float(getValueSingleStringPole(float(iSlow3)));
		float fSlow23 = (powf(1e+01f, (0.05f * (float(getValueSingleStringDecayRate(float(iSlow3))) / fSlow2))) * (1.0f - fSlow22));
		float fSlow24 = float(getValueSingleStringZero(float(iSlow3)));
		float fSlow25 = (fSlow23 * fSlow24);
		float fSlow26 = (1.0f - fSlow24);
		float fSlow27 = (fSlow22 * fSlow26);
		float fSlow28 = (3.0f * fSlow27);
		float fSlow29 = (fSlow25 - fSlow28);
		float fSlow30 = (fSlow27 - fSlow25);
		float fSlow31 = (4.0f * fSlow30);
		float fSlow32 = (fSlow29 + fSlow31);
		float fSlow33 = ((3.0f * fSlow26) - fSlow23);
		float fSlow34 = (0.0f - (fSlow32 / fSlow33));
		float fSlow35 = (1.0f + ((fSlow29 * fSlow20) / fSlow33));
		float fSlow36 = ((fSlow23 + fSlow24) - 1.0f);
		float fSlow37 = (4.0f * fSlow36);
		float fSlow38 = (1.0f + ((fSlow37 + (fSlow32 * fSlow20)) / fSlow33));
		float fSlow39 = (fSlow29 * fSlow32);
		float fSlow40 = faustpower2_f(fSlow16);
		float fSlow41 = faustpower2_f(fSlow33);
		float fSlow42 = (fConst3 * ((6.2831855f + (fSlow21 + atan2f((fSlow16 * ((fSlow34 * fSlow35) + ((fSlow29 * fSlow38) / fSlow33))), ((fSlow35 * fSlow38) + ((fSlow39 * fSlow40) / fSlow41))))) / fSlow14));
		int iSlow43 = int(fSlow42);
		float fSlow44 = (fSlow42 - float(iSlow43));
		float fSlow45 = float(fButton0);
		float fSlow46 = (0.0f - (fSlow45 - 1.0f));
		float fSlow47 = (0.001f * ((0.9996f * fSlow45) + (0.9f * (fSlow46 * float(getValueReleaseLoopGain(float(iSlow3)))))));
		float fSlow48 = float(getValueDCBa1(float(iSlow3)));
		float fSlow49 = (1.0f - fSlow48);
		float fSlow50 = (0.5f * fSlow49);
		float fSlow51 = (0.25f * float(fHslider3));
		float fSlow52 = float(getValueLoudPole(float(iSlow3)));
		float fSlow53 = ((fSlow51 + 0.98f) - fSlow52);
		float fSlow54 = float(getValueLoudGain(float(iSlow3)));
		float fSlow55 = (fSlow53 * fSlow54);
		float fSlow56 = (1.3969839e-09f * ((float((iSlow3 < 88)) * fSlow53) * fSlow54));
		int iSlow57 = (fSlow45 > 0.0f);
		float fSlow58 = expf((0.0f - (fConst5 / (float(fEntry1) * float(getValueDryTapAmpT60(float(iSlow3)))))));
		int iSlow59 = (fSlow45 < 1.0f);
		float fSlow60 = float(fHslider4);
		float fSlow61 = expf((0.0f - (fConst6 / fSlow60)));
		float fSlow62 = (fConst7 * fSlow60);
		float fSlow63 = (fConst9 * fSlow46);
		float fSlow64 = (0.2f * float(getValueSustainPedalLevel(float(iSlow3))));
		float fSlow65 = (fSlow63 - 1.0f);
		float fSlow66 = (fSlow51 - (0.02f + fSlow52));
		float fSlow67 = (0.0f - fSlow50);
		float fSlow68 = ((fSlow25 + fSlow31) - fSlow28);
		float fSlow69 = (0.0f - (fSlow68 / fSlow33));
		float fSlow70 = (1.0f + (((fSlow68 * fSlow20) + fSlow37) / fSlow33));
		float fSlow71 = (fSlow68 * fSlow29);
		int iSlow72 = int((fConst3 * (((atan2f((((fSlow69 * fSlow35) + ((fSlow70 * fSlow29) / fSlow33)) * fSlow16), ((fSlow70 * fSlow35) + ((fSlow71 * fSlow40) / fSlow41))) + fSlow21) + 6.2831855f) / fSlow14)));
		int iSlow73 = ((1 + iSlow72) & 4095);
		float fSlow74 = (1.0f / fSlow33);
		float fSlow75 = (fSlow2 - fSlow13);
		float fSlow76 = (fConst4 * fSlow75);
		float fSlow77 = sinf(fSlow76);
		float fSlow78 = cosf(fSlow76);
		float fSlow79 = (3.0f * atan2f((fSlow12 * fSlow77), (fSlow18 + (fSlow19 * fSlow78))));
		float fSlow80 = (1.0f + ((fSlow29 * fSlow78) / fSlow33));
		float fSlow81 = (1.0f + (((fSlow32 * fSlow78) + fSlow37) / fSlow33));
		float fSlow82 = faustpower2_f(fSlow77);
		float fSlow83 = (fConst3 * ((6.2831855f + (fSlow79 + atan2f((fSlow77 * ((fSlow34 * fSlow80) + ((fSlow29 * fSlow81) / fSlow33))), ((fSlow80 * fSlow81) + ((fSlow39 * fSlow82) / fSlow41))))) / fSlow75));
		int iSlow84 = int(fSlow83);
		float fSlow85 = (float((1 + iSlow84)) - fSlow83);
		float fSlow86 = (1.0f + (((fSlow68 * fSlow78) + fSlow37) / fSlow33));
		int iSlow87 = int((fConst3 * (((atan2f((((fSlow69 * fSlow80) + ((fSlow86 * fSlow29) / fSlow33)) * fSlow77), ((fSlow86 * fSlow80) + ((fSlow71 * fSlow82) / fSlow41))) + fSlow79) + 6.2831855f) / fSlow75)));
		int iSlow88 = (iSlow87 & 4095);
		float fSlow89 = (fSlow83 - float(iSlow84));
		int iSlow90 = ((1 + iSlow87) & 4095);
		float fSlow91 = (float((1 + iSlow43)) - fSlow42);
		int iSlow92 = (iSlow72 & 4095);
		float fSlow93 = ((0.0f - (fConst10 * (fSlow2 * fSlow4))) * cosf((fConst4 * (fSlow2 / float(getValueStrikePosition(float(iSlow3)))))));
		float fSlow94 = (fConst11 * fSlow5);
		float fSlow95 = (0.5f - fSlow6);
		float fSlow96 = cosf((fConst4 * fSlow2));
		float fSlow97 = powf(1e+01f, (0.05f * float(getValueSecondStageAmpRatio(float(iSlow3)))));
		float fSlow98 = powf(1e+01f, (fConst12 * float(getValuer1_1db(float(iSlow3)))));
		float fSlow99 = powf(1e+01f, (fConst12 * float(getValuer1_2db(float(iSlow3)))));
		float fSlow100 = (1.0f - fSlow97);
		float fSlow101 = (0.0f - (2.0f * ((fSlow97 * fSlow98) + (fSlow99 * fSlow100))));
		float fSlow102 = (fSlow96 * (0.0f - (2.0f * fSlow98)));
		float fSlow103 = faustpower2_f(fSlow98);
		float fSlow104 = powf(1e+01f, (fConst12 * float(getValuer2db(float(iSlow3)))));
		float fSlow105 = ((0.0f - (2.0f * fSlow104)) * cosf((fConst4 * (fSlow2 * float(getValueSecondPartialFactor(float(iSlow3)))))));
		float fSlow106 = faustpower2_f(fSlow104);
		float fSlow107 = (2.0f * float(getValueBq4_gEarBalled(float(iSlow3))));
		float fSlow108 = powf(1e+01f, (fConst12 * float(getValuer3db(float(iSlow3)))));
		float fSlow109 = ((0.0f - (2.0f * fSlow108)) * cosf((fConst4 * (fSlow2 * float(getValueThirdPartialFactor(float(iSlow3)))))));
		float fSlow110 = faustpower2_f(fSlow108);
		int iSlow111 = (iSlow3 >= 88);
		float fSlow112 = (2.3283064e-10f * float(iSlow111));
		float fSlow113 = (1.1641532e-10f * float(iSlow111));
		float fSlow114 = (0.0f - (2.0f * fSlow99));
		float fSlow115 = faustpower2_f(fSlow99);
		float fSlow116 = ((fSlow97 * fSlow103) + (fSlow115 * fSlow100));
		float fSlow117 = (12.0f * fSlow0);
		int iSlow118 = (int((fConst13 * (float(fHslider5) / fSlow2))) & 4095);
		for (int i = 0; (i < count); i = (i + 1)) {
			fRec7[0] = ((0.999f * fRec7[1]) + fSlow47);
			iRec13[0] = (12345 + (1103515245 * iRec13[1]));
			fRec15[0] = (1.0f + (fSlow45 * fRec15[1]));
			float fTemp0 = (fRec15[0] - 1.0f);
			int iTemp1 = ((fTemp0 < 2.0f) & iSlow57);
			float fTemp2 = ((0.030197384f * float(iTemp1)) + (fSlow58 * float(((fTemp0 >= 2.0f) | iSlow59))));
			fRec14[0] = ((fRec14[1] * fTemp2) + (0.15f * (float(iTemp1) * (1.0f - fTemp2))));
			int iTemp3 = (fTemp0 < fSlow62);
			float fTemp4 = (fSlow45 * ((fSlow61 * float(iTemp3)) + (fConst8 * float((fTemp0 >= fSlow62)))));
			fRec16[0] = ((fRec16[1] * (fTemp4 + fSlow63)) + (fSlow64 * ((0.0f - (fTemp4 + fSlow65)) * float((iTemp3 & iSlow57)))));
			float fTemp5 = (float(iRec13[0]) * (fRec14[0] + fRec16[0]));
			fRec12[0] = ((fSlow56 * fTemp5) - (fSlow66 * fRec12[1]));
			fRec11[0] = ((fSlow55 * fRec12[0]) - (fSlow66 * fRec11[1]));
			fRec10[0] = ((fSlow55 * fRec11[0]) - (fSlow66 * fRec10[1]));
			fRec9[0] = ((fSlow55 * fRec10[0]) - (fSlow66 * fRec9[1]));
			fRec8[0] = (((fSlow50 * fRec9[0]) + (fSlow67 * fRec9[1])) - (fSlow48 * fRec8[1]));
			float fTemp6 = (fRec7[0] * (fRec8[0] + fRec1[1]));
			fVec0[0] = fTemp6;
			fRec6[0] = (fVec0[1] + (fSlow17 * ((3.7f * fTemp6) - (3.7f * fRec6[1]))));
			fRec5[0] = (fRec6[1] + (fSlow17 * ((3.7f * fRec6[0]) - (3.7f * fRec5[1]))));
			fRec4[(IOTA & 4095)] = (fRec5[1] + (fSlow17 * ((3.7f * fRec5[0]) - (3.7f * fRec4[((IOTA - 1) & 4095)]))));
			float fTemp7 = (fSlow44 * fRec4[((IOTA - iSlow73) & 4095)]);
			float fTemp8 = (fRec8[0] + (fRec7[0] * fRec2[1]));
			fVec1[0] = fTemp8;
			fRec20[0] = (fVec1[1] + (fSlow17 * ((3.7f * fTemp8) - (3.7f * fRec20[1]))));
			fRec19[0] = (fRec20[1] + (fSlow17 * ((3.7f * fRec20[0]) - (3.7f * fRec19[1]))));
			fRec18[(IOTA & 4095)] = (fRec19[1] + (fSlow17 * ((3.7f * fRec19[0]) - (3.7f * fRec18[((IOTA - 1) & 4095)]))));
			float fTemp9 = (fSlow85 * fRec18[((IOTA - iSlow88) & 4095)]);
			float fTemp10 = (fSlow89 * fRec18[((IOTA - iSlow90) & 4095)]);
			float fTemp11 = (fSlow91 * fRec4[((IOTA - iSlow92) & 4095)]);
			float fTemp12 = (fTemp7 + ((fTemp9 + fTemp10) + fTemp11));
			fVec2[0] = fTemp12;
			fRec17[0] = (fSlow74 * ((2.0f * ((fSlow36 * fTemp12) + (fSlow30 * fVec2[1]))) - (fSlow29 * fRec17[1])));
			fRec1[0] = (fTemp7 + (fRec17[0] + fTemp11));
			fRec2[0] = (fTemp10 + (fRec17[0] + fTemp9));
			float fRec3 = fTemp12;
			fRec0[0] = ((fSlow8 * fRec3) - ((fSlow93 * fRec0[1]) + (fSlow94 * fRec0[2])));
			fVec3[0] = (fSlow112 * fTemp5);
			float fTemp13 = (0.0f - ((0.5f * fVec3[1]) + (fSlow113 * fTemp5)));
			fVec4[0] = fTemp13;
			fRec30[0] = (((fSlow50 * fTemp13) + (fSlow67 * fVec4[1])) - (fSlow48 * fRec30[1]));
			fRec29[0] = ((fSlow55 * fRec30[0]) - (fSlow66 * fRec29[1]));
			fRec28[0] = ((fSlow55 * fRec29[0]) - (fSlow66 * fRec28[1]));
			fRec27[0] = ((fSlow55 * fRec28[0]) - (fSlow66 * fRec27[1]));
			fRec26[0] = ((fSlow55 * fRec27[0]) - (fSlow66 * fRec26[1]));
			fRec25[0] = (0.0f - (((fSlow109 * fRec25[1]) + (fSlow110 * fRec25[2])) - (fSlow107 * ((0.5f * fRec26[0]) - (0.5f * fRec26[1])))));
			fRec24[0] = (0.0f - (((fSlow105 * fRec24[1]) + (fSlow106 * fRec24[2])) - (fSlow107 * fRec25[0])));
			fRec23[0] = (0.0f - (((fSlow102 * fRec23[1]) + (fSlow103 * fRec23[2])) - fRec24[0]));
			fRec22[0] = (((fSlow96 * ((fSlow101 * fRec23[1]) - (fSlow114 * fRec22[1]))) + (fRec23[0] + (fSlow116 * fRec23[2]))) - (fSlow115 * fRec22[2]));
			fRec21[0] = ((fSlow49 * fRec22[0]) - (fSlow48 * fRec21[1]));
			float fTemp14 = ((fSlow7 * fRec0[2]) + ((fSlow95 * fRec0[0]) + (fRec3 + fRec21[0])));
			fVec5[(IOTA & 4095)] = fTemp14;
			output0[i] = FAUSTFLOAT((fSlow1 * fTemp14));
			output1[i] = FAUSTFLOAT((fSlow117 * fVec5[((IOTA - iSlow118) & 4095)]));
			fRec7[1] = fRec7[0];
			iRec13[1] = iRec13[0];
			fRec15[1] = fRec15[0];
			fRec14[1] = fRec14[0];
			fRec16[1] = fRec16[0];
			fRec12[1] = fRec12[0];
			fRec11[1] = fRec11[0];
			fRec10[1] = fRec10[0];
			fRec9[1] = fRec9[0];
			fRec8[1] = fRec8[0];
			fVec0[1] = fVec0[0];
			fRec6[1] = fRec6[0];
			fRec5[1] = fRec5[0];
			IOTA = (IOTA + 1);
			fVec1[1] = fVec1[0];
			fRec20[1] = fRec20[0];
			fRec19[1] = fRec19[0];
			fVec2[1] = fVec2[0];
			fRec17[1] = fRec17[0];
			fRec1[1] = fRec1[0];
			fRec2[1] = fRec2[0];
			fRec0[2] = fRec0[1];
			fRec0[1] = fRec0[0];
			fVec3[1] = fVec3[0];
			fVec4[1] = fVec4[0];
			fRec30[1] = fRec30[0];
			fRec29[1] = fRec29[0];
			fRec28[1] = fRec28[0];
			fRec27[1] = fRec27[0];
			fRec26[1] = fRec26[0];
			fRec25[2] = fRec25[1];
			fRec25[1] = fRec25[0];
			fRec24[2] = fRec24[1];
			fRec24[1] = fRec24[0];
			fRec23[2] = fRec23[1];
			fRec23[1] = fRec23[0];
			fRec22[2] = fRec22[1];
			fRec22[1] = fRec22[0];
			fRec21[1] = fRec21[0];
			
		}
		
	}

	
};



#include "Faust_plugins_template2.cpp"


#endif
