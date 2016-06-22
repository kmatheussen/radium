/* ------------------------------------------------------------
author: "Romain Michon"
copyright: "Romain Michon (rmichon@ccrma.stanford.edu)"
name: "UniBar"
version: "1.0"
Code generated with Faust 2.0.a43 (http://faust.grame.fr)
------------------------------------------------------------ */

#ifndef  __Uni_Bar_dsp_H__
#define  __Uni_Bar_dsp_H__

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

#include <math.h>

static float faustpower2_f(float value) {
	return (value * value);
	
}
static float faustpower4_f(float value) {
	return (((value * value) * value) * value);
	
}

#ifndef FAUSTCLASS 
#define FAUSTCLASS Uni_Bar_dsp
#endif

class Uni_Bar_dsp : public dsp {
	
  private:
	
	float fVec0[4096];
	float fVec1[4096];
	float fVec4[4096];
	float fVec2[2048];
	float fVec3[2048];
	float fRec7[3];
	float fRec12[3];
	float fRec14[3];
	float fRec16[3];
	int iRec0[2];
	float fRec1[2];
	float fRec8[2];
	int iRec9[2];
	float fRec10[2];
	float fRec6[2];
	float fRec2[2];
	float fRec11[2];
	float fRec3[2];
	float fRec13[2];
	float fRec4[2];
	float fRec15[2];
	float fRec5[2];
	FAUSTFLOAT fHslider0;
	FAUSTFLOAT fButton0;
	FAUSTFLOAT fHslider1;
	int fSamplingFreq;
	float fConst0;
	FAUSTFLOAT fHslider2;
	float fConst1;
	float fConst2;
	float fConst3;
	float fConst4;
	float fConst5;
	float fConst6;
	FAUSTFLOAT fEntry0;
	FAUSTFLOAT fEntry1;
	FAUSTFLOAT fEntry2;
	FAUSTFLOAT fHslider3;
	FAUSTFLOAT fHslider4;
	float fConst7;
	float fConst8;
	float fConst9;
	FAUSTFLOAT fHslider5;
	int IOTA;
	float fConst10;
	float fConst11;
	float fConst12;
	float fConst13;
	float fConst14;
	float fConst15;
	float fConst16;
	float fConst17;
	FAUSTFLOAT fHslider6;
	
  public:
	
	void static metadata(Meta* m) { 
		m->declare("author", "Romain Michon");
		m->declare("copyright", "Romain Michon (rmichon@ccrma.stanford.edu)");
		m->declare("description", "Nonlinear Banded Waveguide Models");
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
		m->declare("name", "UniBar");
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
		fButton0 = FAUSTFLOAT(0.0f);
		for (int i0 = 0; (i0 < 2); i0 = (i0 + 1)) {
			iRec0[i0] = 0;
			
		}
		fHslider1 = FAUSTFLOAT(0.01f);
		fConst0 = min(1.92e+05f, max(1.0f, float(fSamplingFreq)));
		fHslider2 = FAUSTFLOAT(0.05f);
		for (int i1 = 0; (i1 < 2); i1 = (i1 + 1)) {
			fRec1[i1] = 0.0f;
			
		}
		fConst1 = (1.0f - (100.53097f / fConst0));
		fConst2 = faustpower2_f(fConst1);
		fConst3 = (0.5f * fConst2);
		fConst4 = (0.5f - fConst3);
		fConst5 = (0.0f - (2.0f * fConst1));
		fConst6 = (6.2831855f / fConst0);
		fEntry0 = FAUSTFLOAT(4.4e+02f);
		fEntry1 = FAUSTFLOAT(0.0f);
		fEntry2 = FAUSTFLOAT(0.8f);
		for (int i2 = 0; (i2 < 2); i2 = (i2 + 1)) {
			fRec8[i2] = 0.0f;
			
		}
		fHslider3 = FAUSTFLOAT(1.0f);
		fHslider4 = FAUSTFLOAT(0.0f);
		for (int i3 = 0; (i3 < 2); i3 = (i3 + 1)) {
			iRec9[i3] = 0;
			
		}
		fConst7 = (5e+01f / fConst0);
		fConst8 = (1.0f - powf(9e+01f, (2e+02f / fConst0)));
		fConst9 = (1.0f - (1.0f / powf(9e+04f, (1e+02f / fConst0))));
		for (int i4 = 0; (i4 < 2); i4 = (i4 + 1)) {
			fRec10[i4] = 0.0f;
			
		}
		fHslider5 = FAUSTFLOAT(0.2f);
		IOTA = 0;
		for (int i5 = 0; (i5 < 4096); i5 = (i5 + 1)) {
			fVec0[i5] = 0.0f;
			
		}
		for (int i6 = 0; (i6 < 3); i6 = (i6 + 1)) {
			fRec7[i6] = 0.0f;
			
		}
		fConst10 = (fConst3 - 0.5f);
		for (int i7 = 0; (i7 < 2); i7 = (i7 + 1)) {
			fRec6[i7] = 0.0f;
			
		}
		for (int i8 = 0; (i8 < 2); i8 = (i8 + 1)) {
			fRec2[i8] = 0.0f;
			
		}
		fConst11 = (17.31646f / fConst0);
		for (int i9 = 0; (i9 < 4096); i9 = (i9 + 1)) {
			fVec1[i9] = 0.0f;
			
		}
		fConst12 = (0.3628447f * fConst0);
		for (int i10 = 0; (i10 < 3); i10 = (i10 + 1)) {
			fRec12[i10] = 0.0f;
			
		}
		for (int i11 = 0; (i11 < 2); i11 = (i11 + 1)) {
			fRec11[i11] = 0.0f;
			
		}
		for (int i12 = 0; (i12 < 2); i12 = (i12 + 1)) {
			fRec3[i12] = 0.0f;
			
		}
		fConst13 = (33.954334f / fConst0);
		for (int i13 = 0; (i13 < 2048); i13 = (i13 + 1)) {
			fVec2[i13] = 0.0f;
			
		}
		fConst14 = (0.18504812f * fConst0);
		for (int i14 = 0; (i14 < 3); i14 = (i14 + 1)) {
			fRec14[i14] = 0.0f;
			
		}
		for (int i15 = 0; (i15 < 2); i15 = (i15 + 1)) {
			fRec13[i15] = 0.0f;
			
		}
		for (int i16 = 0; (i16 < 2); i16 = (i16 + 1)) {
			fRec4[i16] = 0.0f;
			
		}
		fConst15 = (56.127693f / fConst0);
		for (int i17 = 0; (i17 < 2048); i17 = (i17 + 1)) {
			fVec3[i17] = 0.0f;
			
		}
		fConst16 = (0.111944474f * fConst0);
		for (int i18 = 0; (i18 < 3); i18 = (i18 + 1)) {
			fRec16[i18] = 0.0f;
			
		}
		for (int i19 = 0; (i19 < 2); i19 = (i19 + 1)) {
			fRec15[i19] = 0.0f;
			
		}
		for (int i20 = 0; (i20 < 2); i20 = (i20 + 1)) {
			fRec5[i20] = 0.0f;
			
		}
		for (int i21 = 0; (i21 < 4096); i21 = (i21 + 1)) {
			fVec4[i21] = 0.0f;
			
		}
		fConst17 = (0.5f * fConst0);
		fHslider6 = FAUSTFLOAT(0.5f);
		
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
		interface->declare(&fEntry2, "1", "");
		interface->declare(&fEntry2, "tooltip", "Gain (value between 0 and 1)");
		interface->addNumEntry("gain", &fEntry2, 0.8f, 0.0f, 1.0f, 0.01f);
		interface->declare(&fButton0, "1", "");
		interface->declare(&fButton0, "tooltip", "noteOn = 1, noteOff = 0");
		interface->addButton("gate",&fButton0);
		interface->closeBox();
		interface->openHorizontalBox("Envelopes_and_Vibrato");
		interface->openVerticalBox("Global_Envelope_Parameters");
		interface->declare(&fHslider1, "3", "");
		interface->declare(&fHslider1, "tooltip", "Global envelope attack duration");
		interface->declare(&fHslider1, "unit", "s");
		interface->addHorizontalSlider("Glob_Env_Attack", &fHslider1, 0.01f, 0.0f, 2.0f, 0.01f);
		interface->declare(&fHslider2, "3", "");
		interface->declare(&fHslider2, "tooltip", "Global envelope release duration");
		interface->declare(&fHslider2, "unit", "s");
		interface->addHorizontalSlider("Glob_Env_Release", &fHslider2, 0.05f, 0.0f, 2.0f, 0.01f);
		interface->closeBox();
		interface->closeBox();
		interface->openHorizontalBox("Physical_and_Nonlinearity");
		interface->openVerticalBox("Physical_Parameters");
		interface->declare(&fHslider3, "2", "");
		interface->declare(&fHslider3, "tooltip", "A value between 0 and 1");
		interface->addHorizontalSlider("Base_Gain", &fHslider3, 1.0f, 0.0f, 1.0f, 0.01f);
		interface->declare(&fHslider5, "2", "");
		interface->declare(&fHslider5, "tooltip", "Bow pressure on the instrument (Value between 0 and 1)");
		interface->addHorizontalSlider("Bow_Pressure", &fHslider5, 0.2f, 0.0f, 1.0f, 0.01f);
		interface->declare(&fEntry1, "2", "");
		interface->declare(&fEntry1, "tooltip", "0=Bow; 1=Strike");
		interface->addNumEntry("Excitation_Selector", &fEntry1, 0.0f, 0.0f, 1.0f, 1.0f);
		interface->declare(&fHslider4, "2", "");
		interface->declare(&fHslider4, "tooltip", "A value between 0 and 1");
		interface->addHorizontalSlider("Integration_Constant", &fHslider4, 0.0f, 0.0f, 1.0f, 0.01f);
		interface->closeBox();
		interface->closeBox();
		interface->openVerticalBox("Spat");
		interface->addHorizontalSlider("pan angle", &fHslider0, 0.6f, 0.0f, 1.0f, 0.01f);
		interface->addHorizontalSlider("spatial width", &fHslider6, 0.5f, 0.0f, 1.0f, 0.01f);
		interface->closeBox();
		interface->closeBox();
		
	}
	
	virtual void compute(int count, FAUSTFLOAT** inputs, FAUSTFLOAT** outputs) {
		FAUSTFLOAT* output0 = outputs[0];
		FAUSTFLOAT* output1 = outputs[1];
		float fSlow0 = float(fHslider0);
		float fSlow1 = (7.0f * (1.0f - fSlow0));
		float fSlow2 = float(fButton0);
		int iSlow3 = (fSlow2 > 0.0f);
		float fSlow4 = float(fHslider1);
		float fSlow5 = (1.0f / (float((fSlow4 == 0.0f)) + (fConst0 * fSlow4)));
		float fSlow6 = float(fHslider2);
		float fSlow7 = (1.0f - (1.0f / powf(1e+05f, (1.0f / (float((fSlow6 == 0.0f)) + (fConst0 * fSlow6))))));
		int iSlow8 = (fSlow2 <= 0.0f);
		float fSlow9 = float(fEntry0);
		float fSlow10 = (fConst5 * cosf((fConst6 * fSlow9)));
		float fSlow11 = float(fEntry1);
		float fSlow12 = (0.001f * float(fEntry2));
		float fSlow13 = (0.0f - (fSlow11 - 1.0f));
		float fSlow14 = (0.9f + (0.1f * float(fHslider3)));
		float fSlow15 = float(fHslider4);
		float fSlow16 = (1e+01f - (9.0f * float(fHslider5)));
		int iSlow17 = (int((fConst0 / fSlow9)) & 4095);
		float fSlow18 = (fConst5 * cosf((fConst11 * fSlow9)));
		int iSlow19 = (int((fConst12 / fSlow9)) & 4095);
		float fSlow20 = (fConst5 * cosf((fConst13 * fSlow9)));
		int iSlow21 = (int((fConst14 / fSlow9)) & 4095);
		float fSlow22 = (fConst5 * cosf((fConst15 * fSlow9)));
		int iSlow23 = (int((fConst16 / fSlow9)) & 4095);
		float fSlow24 = (7.0f * fSlow0);
		int iSlow25 = (int((fConst17 * (float(fHslider6) / fSlow9))) & 4095);
		for (int i = 0; (i < count); i = (i + 1)) {
			iRec0[0] = (iSlow3 & (iRec0[1] | (fRec1[1] >= 1.0f)));
			int iTemp0 = (iSlow8 & (fRec1[1] > 0.0f));
			fRec1[0] = (((fSlow5 * float((((iRec0[1] == 0) & iSlow3) & (fRec1[1] < 1.0f)))) + (fRec1[1] * (1.0f - (fSlow7 * float(iTemp0))))) * float(((iTemp0 == 0) | (fRec1[1] >= 1e-06f))));
			fRec8[0] = ((0.999f * fRec8[1]) + fSlow12);
			float fTemp1 = (fSlow11 * fRec8[0]);
			iRec9[0] = (iSlow3 & (iRec9[1] | (fRec10[1] >= 1.0f)));
			int iTemp2 = (iSlow8 & (fRec10[1] > 0.0f));
			fRec10[0] = (((fConst7 * float((((iRec9[1] == 0) & iSlow3) & (fRec10[1] < 1.0f)))) + (fRec10[1] * ((1.0f - (fConst8 * float((iRec9[1] & (fRec10[1] > 9e+01f))))) - (fConst9 * float(iTemp2))))) * float(((iTemp2 == 0) | (fRec10[1] >= 1e-06f))));
			float fTemp3 = (0.0f - (((fSlow14 * ((fRec2[1] + fRec4[1]) + (fRec3[1] + fRec5[1]))) + fSlow15) - ((0.03f + (0.1f * fRec8[0])) * fRec10[0])));
			float fTemp4 = faustpower4_f((0.75f + fabsf((fSlow16 * fTemp3))));
			float fTemp5 = (1.0f / fTemp4);
			float fTemp6 = (fSlow13 * (fTemp3 * (float((fTemp5 > 1.0f)) + (float((fTemp5 <= 1.0f)) / fTemp4))));
			fVec0[(IOTA & 4095)] = (fTemp1 + (fTemp6 + (4.0f * fRec6[1])));
			fRec7[0] = (0.0f - (((fSlow10 * fRec7[1]) + (fConst2 * fRec7[2])) - (0.225f * fVec0[((IOTA - iSlow17) & 4095)])));
			fRec6[0] = ((fConst4 * fRec7[0]) + (fConst10 * fRec7[2]));
			fRec2[0] = fRec6[0];
			float fTemp7 = (fTemp6 + fTemp1);
			fVec1[(IOTA & 4095)] = (fTemp7 + (4.0f * fRec11[1]));
			fRec12[0] = (0.0f - (((fSlow18 * fRec12[1]) + (fConst2 * fRec12[2])) - (0.2025f * fVec1[((IOTA - iSlow19) & 4095)])));
			fRec11[0] = ((fConst4 * fRec12[0]) + (fConst10 * fRec12[2]));
			fRec3[0] = fRec11[0];
			fVec2[(IOTA & 2047)] = (fTemp7 + (4.0f * fRec13[1]));
			fRec14[0] = (0.0f - (((fSlow20 * fRec14[1]) + (fConst2 * fRec14[2])) - (0.18225f * fVec2[((IOTA - iSlow21) & 2047)])));
			fRec13[0] = ((fConst4 * fRec14[0]) + (fConst10 * fRec14[2]));
			fRec4[0] = fRec13[0];
			fVec3[(IOTA & 2047)] = (fTemp7 + (4.0f * fRec15[1]));
			fRec16[0] = (0.0f - (((fSlow22 * fRec16[1]) + (fConst2 * fRec16[2])) - (0.164025f * fVec3[((IOTA - iSlow23) & 2047)])));
			fRec15[0] = ((fConst4 * fRec16[0]) + (fConst10 * fRec16[2]));
			fRec5[0] = fRec15[0];
			float fTemp8 = (fRec1[0] * (fRec5[0] + ((fRec2[0] + fRec4[0]) + fRec3[0])));
			fVec4[(IOTA & 4095)] = fTemp8;
			output0[i] = FAUSTFLOAT((fSlow1 * fTemp8));
			output1[i] = FAUSTFLOAT((fSlow24 * fVec4[((IOTA - iSlow25) & 4095)]));
			iRec0[1] = iRec0[0];
			fRec1[1] = fRec1[0];
			fRec8[1] = fRec8[0];
			iRec9[1] = iRec9[0];
			fRec10[1] = fRec10[0];
			IOTA = (IOTA + 1);
			fRec7[2] = fRec7[1];
			fRec7[1] = fRec7[0];
			fRec6[1] = fRec6[0];
			fRec2[1] = fRec2[0];
			fRec12[2] = fRec12[1];
			fRec12[1] = fRec12[0];
			fRec11[1] = fRec11[0];
			fRec3[1] = fRec3[0];
			fRec14[2] = fRec14[1];
			fRec14[1] = fRec14[0];
			fRec13[1] = fRec13[0];
			fRec4[1] = fRec4[0];
			fRec16[2] = fRec16[1];
			fRec16[1] = fRec16[0];
			fRec15[1] = fRec15[0];
			fRec5[1] = fRec5[0];
			
		}
		
	}

	
};



#include "Faust_plugins_template2.cpp"


#endif
