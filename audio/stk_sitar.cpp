/* ------------------------------------------------------------
author: "Romain Michon (rmichon@ccrma.stanford.edu)"
copyright: "Romain Michon"
name: "Sitar"
version: "1.0"
Code generated with Faust 2.0.a43 (http://faust.grame.fr)
------------------------------------------------------------ */

#ifndef  __Sitar_dsp_H__
#define  __Sitar_dsp_H__

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


#ifndef FAUSTCLASS 
#define FAUSTCLASS Sitar_dsp
#endif

class Sitar_dsp : public dsp {
	
  private:
	
	float fRec0[8192];
	int iRec2[2];
	float fRec1[2];
	int iRec3[2];
	float fRec4[2];
	float fVec0[2];
	FAUSTFLOAT fHslider0;
	FAUSTFLOAT fHslider1;
	FAUSTFLOAT fEntry0;
	int fSamplingFreq;
	float fConst0;
	FAUSTFLOAT fEntry1;
	FAUSTFLOAT fButton0;
	float fConst1;
	float fConst2;
	float fConst3;
	int IOTA;
	float fConst4;
	FAUSTFLOAT fHslider2;
	
  public:
	
	void static metadata(Meta* m) { 
		m->declare("author", "Romain Michon (rmichon@ccrma.stanford.edu)");
		m->declare("copyright", "Romain Michon");
		m->declare("description", "WaveGuide Sitar");
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
		m->declare("name", "Sitar");
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
		fHslider1 = FAUSTFLOAT(0.7f);
		fEntry0 = FAUSTFLOAT(4.4e+02f);
		fConst0 = min(1.92e+05f, max(1.0f, float(fSamplingFreq)));
		for (int i0 = 0; (i0 < 2); i0 = (i0 + 1)) {
			iRec2[i0] = 0;
			
		}
		for (int i1 = 0; (i1 < 2); i1 = (i1 + 1)) {
			fRec1[i1] = 0.0f;
			
		}
		fEntry1 = FAUSTFLOAT(1.0f);
		fButton0 = FAUSTFLOAT(0.0f);
		for (int i2 = 0; (i2 < 2); i2 = (i2 + 1)) {
			iRec3[i2] = 0;
			
		}
		fConst1 = (1e+03f / fConst0);
		fConst2 = (1.0f - powf(0.001f, (25.0f / fConst0)));
		fConst3 = (1.0f - (1.0f / powf(1.0f, (2.0f / fConst0))));
		for (int i3 = 0; (i3 < 2); i3 = (i3 + 1)) {
			fRec4[i3] = 0.0f;
			
		}
		for (int i4 = 0; (i4 < 2); i4 = (i4 + 1)) {
			fVec0[i4] = 0.0f;
			
		}
		IOTA = 0;
		for (int i5 = 0; (i5 < 8192); i5 = (i5 + 1)) {
			fRec0[i5] = 0.0f;
			
		}
		fConst4 = (0.5f * fConst0);
		fHslider2 = FAUSTFLOAT(0.5f);
		
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
		interface->declare(&fHslider1, "2", "");
		interface->declare(&fHslider1, "tooltip", "A value between 0 and 1");
		interface->addHorizontalSlider("Resonance", &fHslider1, 0.7f, 0.0f, 1.0f, 0.01f);
		interface->closeBox();
		interface->openVerticalBox("Spat");
		interface->addHorizontalSlider("pan angle", &fHslider0, 0.6f, 0.0f, 1.0f, 0.01f);
		interface->addHorizontalSlider("spatial width", &fHslider2, 0.5f, 0.0f, 1.0f, 0.01f);
		interface->closeBox();
		interface->closeBox();
		
	}
	
	virtual void compute(int count, FAUSTFLOAT** inputs, FAUSTFLOAT** outputs) {
		FAUSTFLOAT* output0 = outputs[0];
		FAUSTFLOAT* output1 = outputs[1];
		float fSlow0 = float(fHslider0);
		float fSlow1 = (8.0f * (1.0f - fSlow0));
		float fSlow2 = float(fEntry0);
		float fSlow3 = (0.895f + ((0.1f * float(fHslider1)) + (5e-07f * fSlow2)));
		float fSlow4 = (0.990099f * fSlow3);
		float fSlow5 = (fConst0 / fSlow2);
		float fSlow6 = (4.656613e-11f * float(fEntry1));
		float fSlow7 = float(fButton0);
		int iSlow8 = (fSlow7 > 0.0f);
		int iSlow9 = (fSlow7 <= 0.0f);
		float fSlow10 = (8.0f * fSlow0);
		int iSlow11 = (int((fConst4 * (float(fHslider2) / fSlow2))) & 4095);
		for (int i = 0; (i < count); i = (i + 1)) {
			iRec2[0] = (12345 + (1103515245 * iRec2[1]));
			fRec1[0] = ((0.9992f * fRec1[1]) + (0.0008f * (1.0f + (2.3283064e-10f * float(iRec2[0])))));
			iRec3[0] = (iSlow8 & (iRec3[1] | (fRec4[1] >= 1.0f)));
			int iTemp0 = (fRec4[1] > 0.0f);
			int iTemp1 = (iSlow9 & iTemp0);
			fRec4[0] = (((fConst1 * float((((iRec3[1] == 0) & iSlow8) & (fRec4[1] < 1.0f)))) + (fRec4[1] * ((1.0f - (fConst2 * float((iRec3[1] & iTemp0)))) - (fConst3 * float(iTemp1))))) * float(((iTemp1 == 0) | (fRec4[1] >= 1e-06f))));
			fVec0[0] = (fSlow3 * fRec0[((IOTA - (1 + (int((fSlow5 * fRec1[0])) & 4095))) & 8191)]);
			fRec0[(IOTA & 8191)] = (((fSlow4 * fRec0[((IOTA - (1 + (int((fSlow5 * fRec1[0])) & 4095))) & 8191)]) + (fSlow6 * (float(iRec2[0]) * fRec4[0]))) - (0.00990099f * fVec0[1]));
			output0[i] = FAUSTFLOAT((fSlow1 * fRec0[((IOTA - 0) & 8191)]));
			output1[i] = FAUSTFLOAT((fSlow10 * fRec0[((IOTA - iSlow11) & 8191)]));
			iRec2[1] = iRec2[0];
			fRec1[1] = fRec1[0];
			iRec3[1] = iRec3[0];
			fRec4[1] = fRec4[0];
			fVec0[1] = fVec0[0];
			IOTA = (IOTA + 1);
			
		}
		
	}

	
};



#include "Faust_plugins_template2.cpp"


#endif
