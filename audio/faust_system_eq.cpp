/* ------------------------------------------------------------
Code generated with Faust 2.0.a43 (http://faust.grame.fr)
------------------------------------------------------------ */

#ifndef  __System_Eq_dsp_H__
#define  __System_Eq_dsp_H__

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
#define FAUSTCLASS System_Eq_dsp
#endif

class System_Eq_dsp : public dsp {
	
  private:
	
	float fRec1[3];
	float fRec0[2];
	float fRec2[2];
	float fRec3[2];
	int fSamplingFreq;
	float fConst0;
	FAUSTFLOAT fVslider0;
	FAUSTFLOAT fVslider1;
	
  public:
	
	void static metadata(Meta* m) { 
		m->declare("filter_smoothing.lib/author", "Julius O. Smith (jos at ccrma.stanford.edu)");
		m->declare("filter_smoothing.lib/copyright", "Julius O. Smith III");
		m->declare("filter_smoothing.lib/license", "STK-4.3");
		m->declare("filter_smoothing.lib/name", "Faust Filter Library");
		m->declare("filter_smoothing.lib/reference", "https://ccrma.stanford.edu/~jos/filters/");
		m->declare("filter_smoothing.lib/version", "1.29");
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
		return 1;
		
	}
	virtual int getNumOutputs() {
		return 1;
		
	}
	virtual int getInputRate(int channel) {
		int rate;
		switch (channel) {
			case 0: {
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
	virtual int getOutputRate(int channel) {
		int rate;
		switch (channel) {
			case 0: {
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
		fConst0 = (6.2831855f / min(1.92e+05f, max(1.0f, float(fSamplingFreq))));
		fVslider0 = FAUSTFLOAT(315.0f);
		fVslider1 = FAUSTFLOAT(0.0f);
		for (int i0 = 0; (i0 < 2); i0 = (i0 + 1)) {
			fRec0[i0] = 0.0f;
			
		}
		for (int i1 = 0; (i1 < 2); i1 = (i1 + 1)) {
			fRec2[i1] = 0.0f;
			
		}
		for (int i2 = 0; (i2 < 3); i2 = (i2 + 1)) {
			fRec1[i2] = 0.0f;
			
		}
		for (int i3 = 0; (i3 < 2); i3 = (i3 + 1)) {
			fRec3[i3] = 0.0f;
			
		}
		
	}
	
	virtual void init(int samplingFreq) {
		classInit(samplingFreq);
		instanceInit(samplingFreq);
	}
	
	virtual void buildUserInterface(UI* interface) {
		interface->declare(0, "3", "");
		interface->openHorizontalBox("RM Peaking Equalizer 1");
		interface->declare(&fVslider0, "1", "");
		interface->declare(&fVslider0, "style", "knob");
		interface->declare(&fVslider0, "tooltip", "Center-frequency of second-order Regalia-Mitra peaking equalizer section 1");
		interface->declare(&fVslider0, "unit", "Hz");
		interface->addVerticalSlider("Eq1 Freq", &fVslider0, 315.0f, 4e+01f, 2e+04f, 1.0f);
		interface->declare(&fVslider1, "2", "");
		interface->declare(&fVslider1, "style", "knob");
		interface->declare(&fVslider1, "tooltip", "Peak level in dB of second-order Regalia-Mitra peaking equalizer section 1");
		interface->declare(&fVslider1, "unit", "dB");
		interface->addVerticalSlider("Eq1 Level", &fVslider1, 0.0f, -35.0f, 35.0f, 0.1f);
		interface->closeBox();
		
	}
	
	virtual void compute(int count, FAUSTFLOAT** inputs, FAUSTFLOAT** outputs) {
		FAUSTFLOAT* input0 = inputs[0];
		FAUSTFLOAT* output0 = outputs[0];
		float fSlow0 = float(fVslider0);
		float fSlow1 = powf(1e+01f, (0.05f * float(fVslider1)));
		float fSlow2 = (fConst0 * (fSlow0 / sqrtf(max(0.0f, fSlow1))));
		float fSlow3 = (0.001f * ((1.0f - fSlow2) / (1.0f + fSlow2)));
		float fSlow4 = (0.001f * (0.0f - cosf((fConst0 * fSlow0))));
		float fSlow5 = (0.0005f * fSlow1);
		for (int i = 0; (i < count); i = (i + 1)) {
			fRec0[0] = ((0.999f * fRec0[1]) + fSlow3);
			fRec2[0] = ((0.999f * fRec2[1]) + fSlow4);
			float fTemp0 = ((fRec2[0] * (1.0f + fRec0[0])) * fRec1[1]);
			float fTemp1 = float(input0[i]);
			fRec1[0] = (0.0f - ((fTemp0 + (fRec0[0] * fRec1[2])) - fTemp1));
			float fTemp2 = ((fRec0[0] * fRec1[0]) + (fTemp0 + fRec1[2]));
			fRec3[0] = ((0.999f * fRec3[1]) + fSlow5);
			output0[i] = FAUSTFLOAT(((0.5f * (fTemp2 + fTemp1)) + (fRec3[0] * (fTemp2 - fTemp1))));
			fRec0[1] = fRec0[0];
			fRec2[1] = fRec2[0];
			fRec1[2] = fRec1[1];
			fRec1[1] = fRec1[0];
			fRec3[1] = fRec3[0];
			
		}
		
	}

	
};



#include "Faust_plugins_template2.cpp"


#endif
