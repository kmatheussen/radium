/* ------------------------------------------------------------
author: "Romain Michon"
copyright: "Romain Michon (rmichon@ccrma.stanford.edu)"
name: "Bass"
version: "1.0"
Code generated with Faust 2.0.a43 (http://faust.grame.fr)
------------------------------------------------------------ */

#ifndef  __Bass_dsp_H__
#define  __Bass_dsp_H__

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
#include <bass.h>
#include <math.h>


class Bass_dspSIG0 {
	
  private:
	
	int iRec13[2];
	
  public:
	
	int getNumInputsBass_dspSIG0() {
		return 0;
		
	}
	int getNumOutputsBass_dspSIG0() {
		return 1;
		
	}
	int getInputRateBass_dspSIG0(int channel) {
		int rate;
		switch (channel) {
			default: {
				rate = -1;
				break;
			}
			
		}
		return rate;
		
	}
	int getOutputRateBass_dspSIG0(int channel) {
		int rate;
		switch (channel) {
			case 0: {
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
	
	void instanceInitBass_dspSIG0(int samplingFreq) {
		for (int i12 = 0; (i12 < 2); i12 = (i12 + 1)) {
			iRec13[i12] = 0;
			
		}
		
	}
	
	void fillBass_dspSIG0(int count, float* output) {
		for (int i = 0; (i < count); i = (i + 1)) {
			iRec13[0] = (1 + iRec13[1]);
			output[i] = sinf((9.58738e-05f * float((iRec13[0] - 1))));
			iRec13[1] = iRec13[0];
			
		}
		
	}
};

Bass_dspSIG0* newBass_dspSIG0() { return (Bass_dspSIG0*)new Bass_dspSIG0(); }
void deleteBass_dspSIG0(Bass_dspSIG0* dsp) { delete dsp; }

static float ftbl0Bass_dspSIG0[65536];
static float faustpower2_f(float value) {
	return (value * value);
	
}

#ifndef FAUSTCLASS 
#define FAUSTCLASS Bass_dsp
#endif

class Bass_dsp : public dsp {
	
  private:
	
	float fRec3[8192];
	float fVec3[4096];
	float fRec2[3];
	int iRec0[2];
	float fRec1[2];
	float fRec7[2];
	float fRec6[2];
	int iRec8[2];
	float fRec9[2];
	float fVec0[2];
	float fRec5[2];
	float fRec4[2];
	float fRec11[2];
	float fVec1[2];
	float fRec12[2];
	float fRec15[2];
	float fRec14[2];
	float fRec21[2];
	float fRec20[2];
	float fRec19[2];
	float fRec18[2];
	float fRec17[2];
	float fRec16[2];
	float fRec27[2];
	float fRec26[2];
	float fRec25[2];
	float fRec24[2];
	float fRec23[2];
	float fRec22[2];
	float fVec2[2];
	float fRec10[2];
	int iRec29[2];
	float fRec28[2];
	FAUSTFLOAT fHslider0;
	FAUSTFLOAT fButton0;
	FAUSTFLOAT fHslider1;
	int fSamplingFreq;
	float fConst0;
	FAUSTFLOAT fHslider2;
	float fConst1;
	float fConst2;
	FAUSTFLOAT fHslider3;
	FAUSTFLOAT fEntry0;
	FAUSTFLOAT fEntry1;
	FAUSTFLOAT fEntry2;
	float fConst3;
	FAUSTFLOAT fHslider4;
	float fConst4;
	FAUSTFLOAT fHslider5;
	float fConst5;
	int IOTA;
	float fConst6;
	float fConst7;
	FAUSTFLOAT fHslider6;
	
  public:
	
	void static metadata(Meta* m) { 
		m->declare("author", "Romain Michon");
		m->declare("copyright", "Romain Michon (rmichon@ccrma.stanford.edu)");
		m->declare("description", "Nonlinear WaveGuide Acoustic Bass");
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
		m->declare("name", "Bass");
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
		Bass_dspSIG0* sig0 = newBass_dspSIG0();
		sig0->instanceInitBass_dspSIG0(samplingFreq);
		sig0->fillBass_dspSIG0(65536, ftbl0Bass_dspSIG0);
		deleteBass_dspSIG0(sig0);
		
	}
	
	virtual void instanceInit(int samplingFreq) {
		fSamplingFreq = samplingFreq;
		fHslider0 = FAUSTFLOAT(0.6f);
		fButton0 = FAUSTFLOAT(0.0f);
		for (int i0 = 0; (i0 < 2); i0 = (i0 + 1)) {
			iRec0[i0] = 0;
			
		}
		fHslider1 = FAUSTFLOAT(0.0f);
		fConst0 = min(1.92e+05f, max(1.0f, float(fSamplingFreq)));
		fHslider2 = FAUSTFLOAT(0.1f);
		for (int i1 = 0; (i1 < 2); i1 = (i1 + 1)) {
			fRec1[i1] = 0.0f;
			
		}
		for (int i2 = 0; (i2 < 2); i2 = (i2 + 1)) {
			fRec7[i2] = 0.0f;
			
		}
		fConst1 = expf((0.0f - (3.5e+02f / fConst0)));
		for (int i3 = 0; (i3 < 2); i3 = (i3 + 1)) {
			fRec6[i3] = 0.0f;
			
		}
		for (int i4 = 0; (i4 < 2); i4 = (i4 + 1)) {
			iRec8[i4] = 0;
			
		}
		fConst2 = (3.5f / fConst0);
		fHslider3 = FAUSTFLOAT(0.15f);
		fEntry0 = FAUSTFLOAT(1.0f);
		for (int i5 = 0; (i5 < 2); i5 = (i5 + 1)) {
			fRec9[i5] = 0.0f;
			
		}
		for (int i6 = 0; (i6 < 2); i6 = (i6 + 1)) {
			fVec0[i6] = 0.0f;
			
		}
		for (int i7 = 0; (i7 < 2); i7 = (i7 + 1)) {
			fRec5[i7] = 0.0f;
			
		}
		for (int i8 = 0; (i8 < 2); i8 = (i8 + 1)) {
			fRec4[i8] = 0.0f;
			
		}
		fEntry1 = FAUSTFLOAT(1.2e+02f);
		fEntry2 = FAUSTFLOAT(0.0f);
		fConst3 = expf((0.0f - (7e+02f / fConst0)));
		for (int i9 = 0; (i9 < 2); i9 = (i9 + 1)) {
			fRec11[i9] = 0.0f;
			
		}
		for (int i10 = 0; (i10 < 2); i10 = (i10 + 1)) {
			fVec1[i10] = 0.0f;
			
		}
		fHslider4 = FAUSTFLOAT(0.0f);
		for (int i11 = 0; (i11 < 2); i11 = (i11 + 1)) {
			fRec12[i11] = 0.0f;
			
		}
		fConst4 = (1.0f / fConst0);
		fHslider5 = FAUSTFLOAT(2.2e+02f);
		for (int i13 = 0; (i13 < 2); i13 = (i13 + 1)) {
			fRec15[i13] = 0.0f;
			
		}
		for (int i14 = 0; (i14 < 2); i14 = (i14 + 1)) {
			fRec14[i14] = 0.0f;
			
		}
		for (int i15 = 0; (i15 < 2); i15 = (i15 + 1)) {
			fRec21[i15] = 0.0f;
			
		}
		for (int i16 = 0; (i16 < 2); i16 = (i16 + 1)) {
			fRec20[i16] = 0.0f;
			
		}
		for (int i17 = 0; (i17 < 2); i17 = (i17 + 1)) {
			fRec19[i17] = 0.0f;
			
		}
		for (int i18 = 0; (i18 < 2); i18 = (i18 + 1)) {
			fRec18[i18] = 0.0f;
			
		}
		for (int i19 = 0; (i19 < 2); i19 = (i19 + 1)) {
			fRec17[i19] = 0.0f;
			
		}
		for (int i20 = 0; (i20 < 2); i20 = (i20 + 1)) {
			fRec16[i20] = 0.0f;
			
		}
		for (int i21 = 0; (i21 < 2); i21 = (i21 + 1)) {
			fRec27[i21] = 0.0f;
			
		}
		for (int i22 = 0; (i22 < 2); i22 = (i22 + 1)) {
			fRec26[i22] = 0.0f;
			
		}
		for (int i23 = 0; (i23 < 2); i23 = (i23 + 1)) {
			fRec25[i23] = 0.0f;
			
		}
		for (int i24 = 0; (i24 < 2); i24 = (i24 + 1)) {
			fRec24[i24] = 0.0f;
			
		}
		for (int i25 = 0; (i25 < 2); i25 = (i25 + 1)) {
			fRec23[i25] = 0.0f;
			
		}
		for (int i26 = 0; (i26 < 2); i26 = (i26 + 1)) {
			fRec22[i26] = 0.0f;
			
		}
		for (int i27 = 0; (i27 < 2); i27 = (i27 + 1)) {
			fVec2[i27] = 0.0f;
			
		}
		for (int i28 = 0; (i28 < 2); i28 = (i28 + 1)) {
			fRec10[i28] = 0.0f;
			
		}
		for (int i29 = 0; (i29 < 2); i29 = (i29 + 1)) {
			iRec29[i29] = 0;
			
		}
		fConst5 = expf((0.0f - (1.4e+02f / fConst0)));
		for (int i30 = 0; (i30 < 2); i30 = (i30 + 1)) {
			fRec28[i30] = 0.0f;
			
		}
		IOTA = 0;
		for (int i31 = 0; (i31 < 8192); i31 = (i31 + 1)) {
			fRec3[i31] = 0.0f;
			
		}
		fConst6 = (0.0f - (1.994f * cosf((678.584f / fConst0))));
		for (int i32 = 0; (i32 < 3); i32 = (i32 + 1)) {
			fRec2[i32] = 0.0f;
			
		}
		for (int i33 = 0; (i33 < 4096); i33 = (i33 + 1)) {
			fVec3[i33] = 0.0f;
			
		}
		fConst7 = (0.5f * fConst0);
		fHslider6 = FAUSTFLOAT(0.5f);
		
	}
	
	virtual void init(int samplingFreq) {
		classInit(samplingFreq);
		instanceInit(samplingFreq);
	}
	
	virtual void buildUserInterface(UI* interface) {
		interface->openVerticalBox("0x00");
		interface->openHorizontalBox("Basic_Parameters");
		interface->declare(&fEntry1, "1", "");
		interface->declare(&fEntry1, "tooltip", "Tone frequency");
		interface->declare(&fEntry1, "unit", "Hz");
		interface->addNumEntry("freq", &fEntry1, 1.2e+02f, 2e+01f, 2e+04f, 1.0f);
		interface->declare(&fEntry0, "1", "");
		interface->declare(&fEntry0, "tooltip", "Gain (value between 0 and 1)");
		interface->addNumEntry("gain", &fEntry0, 1.0f, 0.0f, 1.0f, 0.01f);
		interface->declare(&fButton0, "1", "");
		interface->declare(&fButton0, "tooltip", "noteOn = 1, noteOff = 0");
		interface->addButton("gate",&fButton0);
		interface->closeBox();
		interface->openHorizontalBox("Envelopes_and_Vibrato");
		interface->openVerticalBox("Global_Envelope_Parameters");
		interface->declare(&fHslider1, "4", "");
		interface->declare(&fHslider1, "tooltip", "Global envelope attack duration");
		interface->declare(&fHslider1, "unit", "s");
		interface->addHorizontalSlider("Glob_Env_Attack", &fHslider1, 0.0f, 0.0f, 2.0f, 0.01f);
		interface->declare(&fHslider2, "4", "");
		interface->declare(&fHslider2, "tooltip", "Global envelope release duration");
		interface->declare(&fHslider2, "unit", "s");
		interface->addHorizontalSlider("Glob_Env_Release", &fHslider2, 0.1f, 0.0f, 2.0f, 0.01f);
		interface->closeBox();
		interface->closeBox();
		interface->openVerticalBox("Nonlinear_Filter_Parameters");
		interface->declare(&fHslider5, "3", "");
		interface->declare(&fHslider5, "tooltip", "Frequency of the sine wave for the modulation of theta (works if Modulation Type=3)");
		interface->declare(&fHslider5, "unit", "Hz");
		interface->addHorizontalSlider("Modulation_Frequency", &fHslider5, 2.2e+02f, 2e+01f, 1e+03f, 0.1f);
		interface->declare(&fEntry2, "3", "");
		interface->declare(&fEntry2, "tooltip", "0=theta is modulated by the incoming signal; 1=theta is modulated by the averaged incoming signal; 2=theta is modulated by the squared incoming signal; 3=theta is modulated by a sine wave of frequency freqMod; 4=theta is modulated by a sine wave of frequency freq;");
		interface->addNumEntry("Modulation_Type", &fEntry2, 0.0f, 0.0f, 4.0f, 1.0f);
		interface->declare(&fHslider4, "3", "");
		interface->declare(&fHslider4, "tooltip", "Nonlinearity factor (value between 0 and 1)");
		interface->addHorizontalSlider("Nonlinearity", &fHslider4, 0.0f, 0.0f, 1.0f, 0.01f);
		interface->closeBox();
		interface->openVerticalBox("Physical_Parameters");
		interface->declare(&fHslider3, "2", "");
		interface->declare(&fHslider3, "tooltip", "A value between 0 and 1");
		interface->addHorizontalSlider("Touch_Length", &fHslider3, 0.15f, 0.0f, 1.0f, 0.01f);
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
		float fSlow1 = (2.0f * (1.0f - fSlow0));
		float fSlow2 = float(fButton0);
		int iSlow3 = (fSlow2 > 0.0f);
		float fSlow4 = float(fHslider1);
		float fSlow5 = (1.0f / (float((fSlow4 == 0.0f)) + (fConst0 * fSlow4)));
		float fSlow6 = float(fHslider2);
		float fSlow7 = (1.0f - (1.0f / powf(1e+05f, (1.0f / (float((fSlow6 == 0.0f)) + (fConst0 * fSlow6))))));
		int iSlow8 = (fSlow2 <= 0.0f);
		int iSlow9 = (fSlow2 < 1.0f);
		float fSlow10 = expf((0.0f - (fConst2 / float(fHslider3))));
		float fSlow11 = float(fEntry0);
		float fSlow12 = float(fEntry1);
		int iSlow13 = int(((17.31234f * (logf(fSlow12) - 6.086775f)) + 69.5f));
		float fSlow14 = float(getValueBassLoopFilterb0(float(iSlow13)));
		float fSlow15 = float(fEntry2);
		int iSlow16 = (fSlow15 >= 3.0f);
		float fSlow17 = (fConst0 / fSlow12);
		float fSlow18 = (0.001f * float(fHslider4));
		int iSlow19 = (fSlow15 != 4.0f);
		float fSlow20 = (0.001f * float(fHslider5));
		float fSlow21 = (fSlow12 * float((fSlow15 == 4.0f)));
		int iSlow22 = (fSlow15 < 3.0f);
		float fSlow23 = (3.1415927f * float((fSlow15 == 0.0f)));
		float fSlow24 = (1.5707964f * float((fSlow15 == 1.0f)));
		float fSlow25 = (3.1415927f * float((fSlow15 == 2.0f)));
		float fSlow26 = float(getValueBassLoopFilterb1(float(iSlow13)));
		float fSlow27 = float(getValueBassLoopFiltera1(float(iSlow13)));
		int iSlow28 = (iSlow9 > 0);
		int iSlow29 = (iSlow9 < 1);
		float fSlow30 = (2.0f * fSlow0);
		int iSlow31 = (int((fConst7 * (float(fHslider6) / fSlow12))) & 4095);
		for (int i = 0; (i < count); i = (i + 1)) {
			iRec0[0] = (iSlow3 & (iRec0[1] | (fRec1[1] >= 1.0f)));
			int iTemp0 = (iSlow8 & (fRec1[1] > 0.0f));
			fRec1[0] = (((fSlow5 * float((((iRec0[1] == 0) & iSlow3) & (fRec1[1] < 1.0f)))) + (fRec1[1] * (1.0f - (fSlow7 * float(iTemp0))))) * float(((iTemp0 == 0) | (fRec1[1] >= 1e-06f))));
			fRec7[0] = (1.0f + (fSlow2 * fRec7[1]));
			float fTemp1 = (fRec7[0] - 1.0f);
			int iTemp2 = ((fTemp1 < 2.0f) & iSlow3);
			float fTemp3 = (0.030197384f * float(iTemp2));
			int iTemp4 = ((fTemp1 >= 2.0f) | iSlow9);
			float fTemp5 = (fTemp3 + (fConst1 * float(iTemp4)));
			int iTemp6 = (iTemp2 < 1);
			fRec6[0] = ((fRec6[1] * fTemp5) + ((1.0f - fTemp5) * (0.0f - ((0.5f * float(iTemp2)) + (0.985f * float(iTemp6))))));
			iRec8[0] = (12345 + (1103515245 * iRec8[1]));
			float fTemp7 = (fTemp3 + (fSlow10 * float(iTemp4)));
			fRec9[0] = ((fRec9[1] * fTemp7) + (fSlow11 * (float(iTemp2) * (1.0f - fTemp7))));
			fVec0[0] = (float(iRec8[0]) * fRec9[0]);
			fRec5[0] = ((0.035f * ((4.656613e-10f * (((1.0f + fRec6[0]) * float(iRec8[0])) * fRec9[0])) - (4.656613e-10f * (fRec6[0] * fVec0[1])))) + (0.965f * fRec5[1]));
			fRec4[0] = ((0.035f * fRec5[0]) + (0.965f * fRec4[1]));
			float fTemp8 = (fTemp3 + (fConst3 * float(iTemp4)));
			fRec11[0] = ((fRec11[1] * fTemp8) + (fSlow17 * (float(iTemp6) * (1.0f - fTemp8))));
			int iTemp9 = int(fRec11[0]);
			int iTemp10 = (1 + iTemp9);
			float fTemp11 = ((fRec3[((IOTA - (1 + (iTemp9 & 4095))) & 8191)] * (float(iTemp10) - fRec11[0])) + ((fRec11[0] - float(iTemp9)) * fRec3[((IOTA - (1 + (iTemp10 & 4095))) & 8191)]));
			fVec1[0] = fTemp11;
			fRec12[0] = ((0.999f * fRec12[1]) + fSlow18);
			fRec15[0] = ((0.999f * fRec15[1]) + fSlow20);
			float fTemp12 = (fRec14[1] + (fConst4 * ((float(iSlow19) * fRec15[0]) + fSlow21)));
			fRec14[0] = (fTemp12 - floorf(fTemp12));
			float fTemp13 = (3.1415927f * (fRec12[0] * ftbl0Bass_dspSIG0[int((65536.0f * fRec14[0]))]));
			float fTemp14 = sinf(fTemp13);
			float fTemp15 = (0.0f - fTemp14);
			float fTemp16 = cosf(fTemp13);
			float fTemp17 = ((fRec16[1] * fTemp15) + (fTemp11 * fTemp16));
			float fTemp18 = ((fTemp15 * fRec17[1]) + (fTemp16 * fTemp17));
			float fTemp19 = ((fTemp15 * fRec18[1]) + (fTemp16 * fTemp18));
			float fTemp20 = ((fTemp15 * fRec19[1]) + (fTemp16 * fTemp19));
			float fTemp21 = ((fTemp15 * fRec20[1]) + (fTemp16 * fTemp20));
			fRec21[0] = ((fTemp15 * fRec21[1]) + (fTemp16 * fTemp21));
			fRec20[0] = ((fTemp14 * fTemp21) + (fTemp16 * fRec21[1]));
			fRec19[0] = ((fTemp14 * fTemp20) + (fTemp16 * fRec20[1]));
			fRec18[0] = ((fTemp14 * fTemp19) + (fTemp16 * fRec19[1]));
			fRec17[0] = ((fTemp14 * fTemp18) + (fTemp16 * fRec18[1]));
			fRec16[0] = ((fTemp14 * fTemp17) + (fTemp16 * fRec17[1]));
			float fTemp22 = (fRec12[0] * (((fSlow23 * fTemp11) + (fSlow24 * (fTemp11 + fVec1[1]))) + (fSlow25 * faustpower2_f(fTemp11))));
			float fTemp23 = sinf(fTemp22);
			float fTemp24 = (0.0f - fTemp23);
			float fTemp25 = cosf(fTemp22);
			float fTemp26 = ((fRec22[1] * fTemp24) + (fTemp11 * fTemp25));
			float fTemp27 = ((fTemp24 * fRec23[1]) + (fTemp25 * fTemp26));
			float fTemp28 = ((fTemp24 * fRec24[1]) + (fTemp25 * fTemp27));
			float fTemp29 = ((fTemp24 * fRec25[1]) + (fTemp25 * fTemp28));
			float fTemp30 = ((fTemp24 * fRec26[1]) + (fTemp25 * fTemp29));
			fRec27[0] = ((fTemp24 * fRec27[1]) + (fTemp25 * fTemp30));
			fRec26[0] = ((fTemp23 * fTemp30) + (fTemp25 * fRec27[1]));
			fRec25[0] = ((fTemp23 * fTemp29) + (fTemp25 * fRec26[1]));
			fRec24[0] = ((fTemp23 * fTemp28) + (fTemp25 * fRec25[1]));
			fRec23[0] = ((fTemp23 * fTemp27) + (fTemp25 * fRec24[1]));
			fRec22[0] = ((fTemp23 * fTemp26) + (fTemp25 * fRec23[1]));
			float fTemp31 = ((float(iSlow16) * ((fTemp11 * fTemp14) + (fRec16[1] * fTemp16))) + (float(iSlow22) * ((fRec12[0] * ((fTemp11 * fTemp23) + (fRec22[1] * fTemp25))) + ((1.0f - fRec12[0]) * fTemp11))));
			fVec2[0] = fTemp31;
			fRec10[0] = (((fSlow14 * fTemp31) + (fSlow26 * fVec2[1])) - (fSlow27 * fRec10[1]));
			iRec29[0] = (1 + (iSlow9 * iRec29[1]));
			int iTemp32 = (iRec29[0] - 1);
			int iTemp33 = ((float(iTemp32) < 2.0f) & iSlow28);
			float fTemp34 = ((0.030197384f * float(iTemp33)) + (fConst5 * float(((float(iTemp32) >= 2.0f) | iSlow29))));
			fRec28[0] = ((fRec28[1] * fTemp34) + ((1.0f - fTemp34) * (float(iTemp33) + (0.9f * float((iTemp33 < 1))))));
			fRec3[(IOTA & 8191)] = (fRec4[0] + (fRec10[0] * ((float(iSlow9) * fRec28[0]) + fSlow2)));
			fRec2[0] = (fRec3[((IOTA - 0) & 8191)] - ((fConst6 * fRec2[1]) + (0.994009f * fRec2[2])));
			float fTemp35 = (fRec1[0] * ((1.5f * ((0.0029955f * fRec2[0]) - (0.0029955f * fRec2[2]))) + (0.5f * fRec3[((IOTA - 0) & 8191)])));
			fVec3[(IOTA & 4095)] = fTemp35;
			output0[i] = FAUSTFLOAT((fSlow1 * fTemp35));
			output1[i] = FAUSTFLOAT((fSlow30 * fVec3[((IOTA - iSlow31) & 4095)]));
			iRec0[1] = iRec0[0];
			fRec1[1] = fRec1[0];
			fRec7[1] = fRec7[0];
			fRec6[1] = fRec6[0];
			iRec8[1] = iRec8[0];
			fRec9[1] = fRec9[0];
			fVec0[1] = fVec0[0];
			fRec5[1] = fRec5[0];
			fRec4[1] = fRec4[0];
			fRec11[1] = fRec11[0];
			fVec1[1] = fVec1[0];
			fRec12[1] = fRec12[0];
			fRec15[1] = fRec15[0];
			fRec14[1] = fRec14[0];
			fRec21[1] = fRec21[0];
			fRec20[1] = fRec20[0];
			fRec19[1] = fRec19[0];
			fRec18[1] = fRec18[0];
			fRec17[1] = fRec17[0];
			fRec16[1] = fRec16[0];
			fRec27[1] = fRec27[0];
			fRec26[1] = fRec26[0];
			fRec25[1] = fRec25[0];
			fRec24[1] = fRec24[0];
			fRec23[1] = fRec23[0];
			fRec22[1] = fRec22[0];
			fVec2[1] = fVec2[0];
			fRec10[1] = fRec10[0];
			iRec29[1] = iRec29[0];
			fRec28[1] = fRec28[0];
			IOTA = (IOTA + 1);
			fRec2[2] = fRec2[1];
			fRec2[1] = fRec2[0];
			
		}
		
	}

	
};



#include "Faust_plugins_template2.cpp"


#endif
