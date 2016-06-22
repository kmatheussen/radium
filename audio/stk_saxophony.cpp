/* ------------------------------------------------------------
author: "Romain Michon"
copyright: "Romain Michon (rmichon@ccrma.stanford.edu)"
name: "Saxophone"
version: "1.0"
Code generated with Faust 2.0.a43 (http://faust.grame.fr)
------------------------------------------------------------ */

#ifndef  __Saxophony_dsp_H__
#define  __Saxophony_dsp_H__

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


class Saxophony_dspSIG0 {
	
  private:
	
	int iRec6[2];
	
  public:
	
	int getNumInputsSaxophony_dspSIG0() {
		return 0;
		
	}
	int getNumOutputsSaxophony_dspSIG0() {
		return 1;
		
	}
	int getInputRateSaxophony_dspSIG0(int channel) {
		int rate;
		switch (channel) {
			default: {
				rate = -1;
				break;
			}
			
		}
		return rate;
		
	}
	int getOutputRateSaxophony_dspSIG0(int channel) {
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
	
	void instanceInitSaxophony_dspSIG0(int samplingFreq) {
		for (int i4 = 0; (i4 < 2); i4 = (i4 + 1)) {
			iRec6[i4] = 0;
			
		}
		
	}
	
	void fillSaxophony_dspSIG0(int count, float* output) {
		for (int i = 0; (i < count); i = (i + 1)) {
			iRec6[0] = (1 + iRec6[1]);
			output[i] = sinf((9.58738e-05f * float((iRec6[0] - 1))));
			iRec6[1] = iRec6[0];
			
		}
		
	}
};

Saxophony_dspSIG0* newSaxophony_dspSIG0() { return (Saxophony_dspSIG0*)new Saxophony_dspSIG0(); }
void deleteSaxophony_dspSIG0(Saxophony_dspSIG0* dsp) { delete dsp; }

static float ftbl0Saxophony_dspSIG0[65536];
static float faustpower2_f(float value) {
	return (value * value);
	
}

#ifndef FAUSTCLASS 
#define FAUSTCLASS Saxophony_dsp
#endif

class Saxophony_dsp : public dsp {
	
  private:
	
	float fRec0[8192];
	float fVec2[4096];
	float fVec3[4096];
	float fRec4[2];
	int iRec2[2];
	float fRec3[2];
	int iRec5[2];
	float fRec7[2];
	float fVec0[2];
	float fRec8[2];
	int iRec9[2];
	float fRec10[2];
	float fRec12[2];
	float fRec11[2];
	float fRec18[2];
	float fRec17[2];
	float fRec16[2];
	float fRec15[2];
	float fRec14[2];
	float fRec13[2];
	float fRec24[2];
	float fRec23[2];
	float fRec22[2];
	float fRec21[2];
	float fRec20[2];
	float fRec19[2];
	float fVec1[2];
	FAUSTFLOAT fEntry0;
	FAUSTFLOAT fHslider0;
	FAUSTFLOAT fHslider1;
	FAUSTFLOAT fButton0;
	int fSamplingFreq;
	float fConst0;
	FAUSTFLOAT fHslider2;
	FAUSTFLOAT fHslider3;
	FAUSTFLOAT fHslider4;
	FAUSTFLOAT fHslider5;
	float fConst1;
	FAUSTFLOAT fHslider6;
	FAUSTFLOAT fEntry1;
	FAUSTFLOAT fEntry2;
	FAUSTFLOAT fHslider7;
	FAUSTFLOAT fHslider8;
	FAUSTFLOAT fHslider9;
	FAUSTFLOAT fHslider10;
	int IOTA;
	FAUSTFLOAT fHslider11;
	float fConst2;
	FAUSTFLOAT fHslider12;
	
  public:
	
	void static metadata(Meta* m) { 
		m->declare("author", "Romain Michon");
		m->declare("copyright", "Romain Michon (rmichon@ccrma.stanford.edu)");
		m->declare("description", "Nonlinear WaveGuide Saxophone");
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
		m->declare("name", "Saxophone");
		m->declare("reference", "https://ccrma.stanford.edu/~jos/pasp/Woodwinds.html");
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
		Saxophony_dspSIG0* sig0 = newSaxophony_dspSIG0();
		sig0->instanceInitSaxophony_dspSIG0(samplingFreq);
		sig0->fillSaxophony_dspSIG0(65536, ftbl0Saxophony_dspSIG0);
		deleteSaxophony_dspSIG0(sig0);
		
	}
	
	virtual void instanceInit(int samplingFreq) {
		fSamplingFreq = samplingFreq;
		fEntry0 = FAUSTFLOAT(1.0f);
		fHslider0 = FAUSTFLOAT(0.6f);
		fHslider1 = FAUSTFLOAT(1.0f);
		fButton0 = FAUSTFLOAT(0.0f);
		for (int i0 = 0; (i0 < 2); i0 = (i0 + 1)) {
			fRec4[i0] = 0.0f;
			
		}
		for (int i1 = 0; (i1 < 2); i1 = (i1 + 1)) {
			iRec2[i1] = 0;
			
		}
		fConst0 = min(1.92e+05f, max(1.0f, float(fSamplingFreq)));
		fHslider2 = FAUSTFLOAT(0.05f);
		fHslider3 = FAUSTFLOAT(0.01f);
		for (int i2 = 0; (i2 < 2); i2 = (i2 + 1)) {
			fRec3[i2] = 0.0f;
			
		}
		fHslider4 = FAUSTFLOAT(0.05f);
		for (int i3 = 0; (i3 < 2); i3 = (i3 + 1)) {
			iRec5[i3] = 0;
			
		}
		fHslider5 = FAUSTFLOAT(0.1f);
		fConst1 = (1.0f / fConst0);
		fHslider6 = FAUSTFLOAT(6.0f);
		for (int i5 = 0; (i5 < 2); i5 = (i5 + 1)) {
			fRec7[i5] = 0.0f;
			
		}
		fEntry1 = FAUSTFLOAT(0.0f);
		fEntry2 = FAUSTFLOAT(4.4e+02f);
		fHslider7 = FAUSTFLOAT(0.5f);
		for (int i6 = 0; (i6 < 2); i6 = (i6 + 1)) {
			fVec0[i6] = 0.0f;
			
		}
		fHslider8 = FAUSTFLOAT(0.0f);
		for (int i7 = 0; (i7 < 2); i7 = (i7 + 1)) {
			fRec8[i7] = 0.0f;
			
		}
		for (int i8 = 0; (i8 < 2); i8 = (i8 + 1)) {
			iRec9[i8] = 0;
			
		}
		fHslider9 = FAUSTFLOAT(0.1f);
		for (int i9 = 0; (i9 < 2); i9 = (i9 + 1)) {
			fRec10[i9] = 0.0f;
			
		}
		fHslider10 = FAUSTFLOAT(2.2e+02f);
		for (int i10 = 0; (i10 < 2); i10 = (i10 + 1)) {
			fRec12[i10] = 0.0f;
			
		}
		for (int i11 = 0; (i11 < 2); i11 = (i11 + 1)) {
			fRec11[i11] = 0.0f;
			
		}
		for (int i12 = 0; (i12 < 2); i12 = (i12 + 1)) {
			fRec18[i12] = 0.0f;
			
		}
		for (int i13 = 0; (i13 < 2); i13 = (i13 + 1)) {
			fRec17[i13] = 0.0f;
			
		}
		for (int i14 = 0; (i14 < 2); i14 = (i14 + 1)) {
			fRec16[i14] = 0.0f;
			
		}
		for (int i15 = 0; (i15 < 2); i15 = (i15 + 1)) {
			fRec15[i15] = 0.0f;
			
		}
		for (int i16 = 0; (i16 < 2); i16 = (i16 + 1)) {
			fRec14[i16] = 0.0f;
			
		}
		for (int i17 = 0; (i17 < 2); i17 = (i17 + 1)) {
			fRec13[i17] = 0.0f;
			
		}
		for (int i18 = 0; (i18 < 2); i18 = (i18 + 1)) {
			fRec24[i18] = 0.0f;
			
		}
		for (int i19 = 0; (i19 < 2); i19 = (i19 + 1)) {
			fRec23[i19] = 0.0f;
			
		}
		for (int i20 = 0; (i20 < 2); i20 = (i20 + 1)) {
			fRec22[i20] = 0.0f;
			
		}
		for (int i21 = 0; (i21 < 2); i21 = (i21 + 1)) {
			fRec21[i21] = 0.0f;
			
		}
		for (int i22 = 0; (i22 < 2); i22 = (i22 + 1)) {
			fRec20[i22] = 0.0f;
			
		}
		for (int i23 = 0; (i23 < 2); i23 = (i23 + 1)) {
			fRec19[i23] = 0.0f;
			
		}
		for (int i24 = 0; (i24 < 2); i24 = (i24 + 1)) {
			fVec1[i24] = 0.0f;
			
		}
		IOTA = 0;
		for (int i25 = 0; (i25 < 4096); i25 = (i25 + 1)) {
			fVec2[i25] = 0.0f;
			
		}
		fHslider11 = FAUSTFLOAT(0.3f);
		for (int i26 = 0; (i26 < 8192); i26 = (i26 + 1)) {
			fRec0[i26] = 0.0f;
			
		}
		for (int i27 = 0; (i27 < 4096); i27 = (i27 + 1)) {
			fVec3[i27] = 0.0f;
			
		}
		fConst2 = (0.5f * fConst0);
		fHslider12 = FAUSTFLOAT(0.5f);
		
	}
	
	virtual void init(int samplingFreq) {
		classInit(samplingFreq);
		instanceInit(samplingFreq);
	}
	
	virtual void buildUserInterface(UI* interface) {
		interface->openVerticalBox("0x00");
		interface->openHorizontalBox("Basic_Parameters");
		interface->declare(&fEntry2, "1", "");
		interface->declare(&fEntry2, "tooltip", "Tone frequency");
		interface->declare(&fEntry2, "unit", "Hz");
		interface->addNumEntry("freq", &fEntry2, 4.4e+02f, 2e+01f, 2e+04f, 1.0f);
		interface->declare(&fEntry0, "1", "");
		interface->declare(&fEntry0, "tooltip", "Gain (value between 0 and 1)");
		interface->addNumEntry("gain", &fEntry0, 1.0f, 0.0f, 1.0f, 0.01f);
		interface->declare(&fButton0, "1", "");
		interface->declare(&fButton0, "tooltip", "noteOn = 1, noteOff = 0");
		interface->addButton("gate",&fButton0);
		interface->closeBox();
		interface->openHorizontalBox("Envelopes_and_Vibrato");
		interface->openVerticalBox("Envelope_Parameters");
		interface->declare(&fHslider2, "5", "");
		interface->declare(&fHslider2, "tooltip", "Envelope attack duration");
		interface->declare(&fHslider2, "unit", "s");
		interface->addHorizontalSlider("Envelope_Attack", &fHslider2, 0.05f, 0.0f, 2.0f, 0.01f);
		interface->declare(&fHslider3, "5", "");
		interface->declare(&fHslider3, "tooltip", "Envelope release duration");
		interface->declare(&fHslider3, "unit", "s");
		interface->addHorizontalSlider("Envelope_Release", &fHslider3, 0.01f, 0.0f, 2.0f, 0.01f);
		interface->closeBox();
		interface->openVerticalBox("Vibrato_Parameters");
		interface->declare(&fHslider6, "4", "");
		interface->declare(&fHslider6, "unit", "Hz");
		interface->addHorizontalSlider("Vibrato_Freq", &fHslider6, 6.0f, 1.0f, 15.0f, 0.1f);
		interface->declare(&fHslider5, "4", "");
		interface->declare(&fHslider5, "tooltip", "A value between 0 and 1");
		interface->addHorizontalSlider("Vibrato_Gain", &fHslider5, 0.1f, 0.0f, 1.0f, 0.01f);
		interface->closeBox();
		interface->closeBox();
		interface->openHorizontalBox("Physical_and_Nonlinearity");
		interface->openVerticalBox("Nonlinear_Filter_Parameters");
		interface->declare(&fHslider10, "3", "");
		interface->declare(&fHslider10, "tooltip", "Frequency of the sine wave for the modulation of theta (works if Modulation Type=3)");
		interface->declare(&fHslider10, "unit", "Hz");
		interface->addHorizontalSlider("Modulation_Frequency", &fHslider10, 2.2e+02f, 2e+01f, 1e+03f, 0.1f);
		interface->declare(&fEntry1, "3", "");
		interface->declare(&fEntry1, "tooltip", "0=theta is modulated by the incoming signal; 1=theta is modulated by the averaged incoming signal; 2=theta is modulated by the squared incoming signal; 3=theta is modulated by a sine wave of frequency freqMod; 4=theta is modulated by a sine wave of frequency freq;");
		interface->addNumEntry("Modulation_Type", &fEntry1, 0.0f, 0.0f, 4.0f, 1.0f);
		interface->declare(&fHslider8, "3", "");
		interface->declare(&fHslider8, "tooltip", "Nonlinearity factor (value between 0 and 1)");
		interface->addHorizontalSlider("Nonlinearity", &fHslider8, 0.0f, 0.0f, 1.0f, 0.01f);
		interface->declare(&fHslider9, "3", "");
		interface->declare(&fHslider9, "Attack duration of the nonlinearity", "");
		interface->declare(&fHslider9, "unit", "s");
		interface->addHorizontalSlider("Nonlinearity_Attack", &fHslider9, 0.1f, 0.0f, 2.0f, 0.01f);
		interface->closeBox();
		interface->openVerticalBox("Physical_Parameters");
		interface->declare(&fHslider7, "2", "");
		interface->declare(&fHslider7, "tooltip", "A value between 0 and 1");
		interface->addHorizontalSlider("Blow_Position", &fHslider7, 0.5f, 0.0f, 1.0f, 0.01f);
		interface->addHorizontalSlider("Noise_Gain", &fHslider4, 0.05f, 0.0f, 1.0f, 0.01f);
		interface->declare(&fHslider1, "2", "");
		interface->declare(&fHslider1, "tooltip", "Breath pressure (a value between 0 and 1)");
		interface->addHorizontalSlider("Pressure", &fHslider1, 1.0f, 0.0f, 1.0f, 0.01f);
		interface->declare(&fHslider11, "2", "");
		interface->declare(&fHslider11, "tooltip", "A value between 0 and 1");
		interface->addHorizontalSlider("Reed_Stiffness", &fHslider11, 0.3f, 0.0f, 1.0f, 0.01f);
		interface->closeBox();
		interface->closeBox();
		interface->openVerticalBox("Spat");
		interface->addHorizontalSlider("pan angle", &fHslider0, 0.6f, 0.0f, 1.0f, 0.01f);
		interface->addHorizontalSlider("spatial width", &fHslider12, 0.5f, 0.0f, 1.0f, 0.01f);
		interface->closeBox();
		interface->closeBox();
		
	}
	
	virtual void compute(int count, FAUSTFLOAT** inputs, FAUSTFLOAT** outputs) {
		FAUSTFLOAT* output0 = outputs[0];
		FAUSTFLOAT* output1 = outputs[1];
		float fSlow0 = float(fEntry0);
		float fSlow1 = float(fHslider0);
		float fSlow2 = (fSlow0 * (1.0f - fSlow1));
		float fSlow3 = float(fHslider1);
		float fSlow4 = (0.55f + (0.3f * fSlow3));
		float fSlow5 = (0.001f * float(fButton0));
		float fSlow6 = (float(fHslider2) * fSlow3);
		float fSlow7 = (1.0f / ((fConst0 * fSlow6) + float((fSlow6 == 0.0f))));
		float fSlow8 = float(fHslider3);
		float fSlow9 = (fSlow8 * fSlow3);
		float fSlow10 = (1.0f - (1.0f / powf(1e+05f, (1.0f / ((fConst0 * fSlow9) + float((fSlow9 == 0.0f)))))));
		float fSlow11 = (4.656613e-10f * float(fHslider4));
		float fSlow12 = float(fHslider5);
		float fSlow13 = (fConst1 * float(fHslider6));
		float fSlow14 = float(fEntry1);
		int iSlow15 = (fSlow14 >= 3.0f);
		float fSlow16 = float(fEntry2);
		float fSlow17 = ((fConst0 / fSlow16) - 3.0f);
		float fSlow18 = float(fHslider7);
		float fSlow19 = (fSlow17 * (1.0f - fSlow18));
		int iSlow20 = int(fSlow19);
		int iSlow21 = (1 + iSlow20);
		float fSlow22 = (float(iSlow21) - fSlow19);
		int iSlow23 = (1 + (iSlow20 & 4095));
		float fSlow24 = (fSlow19 - float(iSlow20));
		int iSlow25 = (1 + (iSlow21 & 4095));
		float fSlow26 = (0.001f * float(fHslider8));
		float fSlow27 = float(fHslider9);
		float fSlow28 = (1.0f / (float((fSlow27 == 0.0f)) + (fConst0 * fSlow27)));
		float fSlow29 = (1.0f - (1.0f / powf(1e+05f, (1.0f / (float((fSlow8 == 0.0f)) + (fConst0 * fSlow8))))));
		int iSlow30 = (fSlow14 != 4.0f);
		float fSlow31 = (0.001f * float(fHslider10));
		float fSlow32 = (fSlow16 * float((fSlow14 == 4.0f)));
		int iSlow33 = (fSlow14 < 3.0f);
		float fSlow34 = (3.1415927f * float((fSlow14 == 0.0f)));
		float fSlow35 = (1.5707964f * float((fSlow14 == 1.0f)));
		float fSlow36 = (3.1415927f * float((fSlow14 == 2.0f)));
		float fSlow37 = (fSlow17 * fSlow18);
		float fSlow38 = (1.0f + fSlow37);
		int iSlow39 = int(fSlow38);
		float fSlow40 = (float(iSlow39) - fSlow37);
		int iSlow41 = (iSlow39 & 4095);
		float fSlow42 = (fSlow38 - float(iSlow39));
		int iSlow43 = ((1 + iSlow39) & 4095);
		float fSlow44 = (0.1f + (0.4f * float(fHslider11)));
		int iSlow45 = (int((fConst2 * (float(fHslider12) / fSlow16))) & 4095);
		for (int i = 0; (i < count); i = (i + 1)) {
			fRec4[0] = ((0.999f * fRec4[1]) + fSlow5);
			int iTemp0 = (fRec4[0] > 0.0f);
			iRec2[0] = (iTemp0 & (iRec2[1] | (fRec3[1] >= 1.0f)));
			int iTemp1 = (fRec4[0] <= 0.0f);
			int iTemp2 = (iTemp1 & (fRec3[1] > 0.0f));
			fRec3[0] = (((fSlow7 * float((((iRec2[1] == 0) & iTemp0) & (fRec3[1] < 1.0f)))) + (fRec3[1] * (1.0f - (fSlow10 * float(iTemp2))))) * float(((iTemp2 == 0) | (fRec3[1] >= 1e-06f))));
			iRec5[0] = (12345 + (1103515245 * iRec5[1]));
			float fTemp3 = (fRec7[1] + fSlow13);
			fRec7[0] = (fTemp3 - floorf(fTemp3));
			float fTemp4 = (fSlow4 * ((fRec3[0] * (1.0f + (fSlow11 * float(iRec5[0])))) * (1.0f + (fSlow12 * ftbl0Saxophony_dspSIG0[int((65536.0f * fRec7[0]))]))));
			float fTemp5 = ((fSlow22 * fRec0[((IOTA - iSlow23) & 8191)]) + (fSlow24 * fRec0[((IOTA - iSlow25) & 8191)]));
			fVec0[0] = fTemp5;
			fRec8[0] = ((0.999f * fRec8[1]) + fSlow26);
			iRec9[0] = (iTemp0 & (iRec9[1] | (fRec10[1] >= 1.0f)));
			int iTemp6 = (iTemp1 & (fRec10[1] > 0.0f));
			fRec10[0] = (((fSlow28 * float((((iRec9[1] == 0) & iTemp0) & (fRec10[1] < 1.0f)))) + (fRec10[1] * (1.0f - (fSlow29 * float(iTemp6))))) * float(((iTemp6 == 0) | (fRec10[1] >= 1e-06f))));
			float fTemp7 = (fRec8[0] * fRec10[0]);
			fRec12[0] = ((0.999f * fRec12[1]) + fSlow31);
			float fTemp8 = (fRec11[1] + (fConst1 * ((float(iSlow30) * fRec12[0]) + fSlow32)));
			fRec11[0] = (fTemp8 - floorf(fTemp8));
			float fTemp9 = (3.1415927f * (fTemp7 * ftbl0Saxophony_dspSIG0[int((65536.0f * fRec11[0]))]));
			float fTemp10 = sinf(fTemp9);
			float fTemp11 = (0.0f - fTemp10);
			float fTemp12 = cosf(fTemp9);
			float fTemp13 = ((fRec13[1] * fTemp11) + (fTemp5 * fTemp12));
			float fTemp14 = ((fTemp11 * fRec14[1]) + (fTemp12 * fTemp13));
			float fTemp15 = ((fTemp11 * fRec15[1]) + (fTemp12 * fTemp14));
			float fTemp16 = ((fTemp11 * fRec16[1]) + (fTemp12 * fTemp15));
			float fTemp17 = ((fTemp11 * fRec17[1]) + (fTemp12 * fTemp16));
			fRec18[0] = ((fTemp11 * fRec18[1]) + (fTemp12 * fTemp17));
			fRec17[0] = ((fTemp10 * fTemp17) + (fTemp12 * fRec18[1]));
			fRec16[0] = ((fTemp10 * fTemp16) + (fTemp12 * fRec17[1]));
			fRec15[0] = ((fTemp10 * fTemp15) + (fTemp12 * fRec16[1]));
			fRec14[0] = ((fTemp10 * fTemp14) + (fTemp12 * fRec15[1]));
			fRec13[0] = ((fTemp10 * fTemp13) + (fTemp12 * fRec14[1]));
			float fTemp18 = (fTemp7 * (((fSlow34 * fTemp5) + (fSlow35 * (fTemp5 + fVec0[1]))) + (fSlow36 * faustpower2_f(fTemp5))));
			float fTemp19 = sinf(fTemp18);
			float fTemp20 = (0.0f - fTemp19);
			float fTemp21 = cosf(fTemp18);
			float fTemp22 = ((fRec19[1] * fTemp20) + (fTemp5 * fTemp21));
			float fTemp23 = ((fTemp20 * fRec20[1]) + (fTemp21 * fTemp22));
			float fTemp24 = ((fTemp20 * fRec21[1]) + (fTemp21 * fTemp23));
			float fTemp25 = ((fTemp20 * fRec22[1]) + (fTemp21 * fTemp24));
			float fTemp26 = ((fTemp20 * fRec23[1]) + (fTemp21 * fTemp25));
			fRec24[0] = ((fTemp20 * fRec24[1]) + (fTemp21 * fTemp26));
			fRec23[0] = ((fTemp19 * fTemp26) + (fTemp21 * fRec24[1]));
			fRec22[0] = ((fTemp19 * fTemp25) + (fTemp21 * fRec23[1]));
			fRec21[0] = ((fTemp19 * fTemp24) + (fTemp21 * fRec22[1]));
			fRec20[0] = ((fTemp19 * fTemp23) + (fTemp21 * fRec21[1]));
			fRec19[0] = ((fTemp19 * fTemp22) + (fTemp21 * fRec20[1]));
			float fTemp27 = (0.0f - (0.95f * ((float(iSlow15) * ((fTemp5 * fTemp10) + (fRec13[1] * fTemp12))) + (float(iSlow33) * ((fRec8[0] * ((fTemp5 * fTemp19) + (fRec19[1] * fTemp21))) + ((1.0f - fRec8[0]) * fTemp5))))));
			fVec1[0] = fTemp27;
			float fTemp28 = (fTemp27 + fVec1[1]);
			fVec2[(IOTA & 4095)] = fTemp28;
			float fTemp29 = (0.5f * fTemp28);
			float fTemp30 = (0.5f * ((fSlow40 * fVec2[((IOTA - iSlow41) & 4095)]) + (fSlow42 * fVec2[((IOTA - iSlow43) & 4095)])));
			float fTemp31 = ((fTemp4 + fTemp30) - fTemp29);
			float fTemp32 = (0.7f + (fSlow44 * fTemp31));
			float fTemp33 = (float((fTemp32 > 1.0f)) + (fTemp32 * float((fTemp32 <= 1.0f))));
			fRec0[(IOTA & 8191)] = (fTemp4 - (fTemp29 + (fTemp31 * ((fTemp33 * float((fTemp33 >= -1.0f))) - float((fTemp33 < -1.0f))))));
			float fRec1 = (fTemp29 - fTemp30);
			output0[i] = FAUSTFLOAT((fSlow2 * fRec1));
			fVec3[(IOTA & 4095)] = (fSlow0 * fRec1);
			output1[i] = FAUSTFLOAT((fSlow1 * fVec3[((IOTA - iSlow45) & 4095)]));
			fRec4[1] = fRec4[0];
			iRec2[1] = iRec2[0];
			fRec3[1] = fRec3[0];
			iRec5[1] = iRec5[0];
			fRec7[1] = fRec7[0];
			fVec0[1] = fVec0[0];
			fRec8[1] = fRec8[0];
			iRec9[1] = iRec9[0];
			fRec10[1] = fRec10[0];
			fRec12[1] = fRec12[0];
			fRec11[1] = fRec11[0];
			fRec18[1] = fRec18[0];
			fRec17[1] = fRec17[0];
			fRec16[1] = fRec16[0];
			fRec15[1] = fRec15[0];
			fRec14[1] = fRec14[0];
			fRec13[1] = fRec13[0];
			fRec24[1] = fRec24[0];
			fRec23[1] = fRec23[0];
			fRec22[1] = fRec22[0];
			fRec21[1] = fRec21[0];
			fRec20[1] = fRec20[0];
			fRec19[1] = fRec19[0];
			fVec1[1] = fVec1[0];
			IOTA = (IOTA + 1);
			
		}
		
	}

	
};



#include "Faust_plugins_template2.cpp"


#endif
