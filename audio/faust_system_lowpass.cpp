/* ------------------------------------------------------------
Code generated with Faust 2.0.a43 (http://faust.grame.fr)
------------------------------------------------------------ */

#ifndef  __System_Lowpass_dsp_H__
#define  __System_Lowpass_dsp_H__

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

#ifndef FAUSTCLASS 
#define FAUSTCLASS System_Lowpass_dsp
#endif

class System_Lowpass_dsp : public dsp {
	
  private:
	
	float fRec0[3];
	float fRec1[2];
	int fSamplingFreq;
	float fConst0;
	FAUSTFLOAT fVslider0;
	
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
		fConst0 = (3.1415927f / min(1.92e+05f, max(1.0f, float(fSamplingFreq))));
		fVslider0 = FAUSTFLOAT(315.0f);
		for (int i0 = 0; (i0 < 2); i0 = (i0 + 1)) {
			fRec1[i0] = 0.0f;
			
		}
		for (int i1 = 0; (i1 < 3); i1 = (i1 + 1)) {
			fRec0[i1] = 0.0f;
			
		}
		
	}
	
	virtual void init(int samplingFreq) {
		classInit(samplingFreq);
		instanceInit(samplingFreq);
	}
	
	virtual void buildUserInterface(UI* interface) {
		interface->openVerticalBox("0x00");
		interface->declare(&fVslider0, "1", "");
		interface->declare(&fVslider0, "style", "knob");
		interface->declare(&fVslider0, "unit", "Hz");
		interface->addVerticalSlider("Eq1 Freq", &fVslider0, 315.0f, 4e+01f, 2e+04f, 1.0f);
		interface->closeBox();
		
	}
	
	virtual void compute(int count, FAUSTFLOAT** inputs, FAUSTFLOAT** outputs) {
		FAUSTFLOAT* input0 = inputs[0];
		FAUSTFLOAT* output0 = outputs[0];
		float fSlow0 = (0.001f / tanf((fConst0 * float(fVslider0))));
		for (int i = 0; (i < count); i = (i + 1)) {
			fRec1[0] = ((0.999f * fRec1[1]) + fSlow0);
			float fTemp0 = (1.0f + (fRec1[0] * (fRec1[0] + 1.4142135f)));
			fRec0[0] = (float(input0[i]) - ((((1.0f + (fRec1[0] * (fRec1[0] - 1.4142135f))) * fRec0[2]) + (2.0f * ((1.0f - faustpower2_f(fRec1[0])) * fRec0[1]))) / fTemp0));
			output0[i] = FAUSTFLOAT(((fRec0[2] + (fRec0[0] + (2.0f * fRec0[1]))) / fTemp0));
			fRec1[1] = fRec1[0];
			fRec0[2] = fRec0[1];
			fRec0[1] = fRec0[0];
			
		}
		
	}

	
};



#include "Faust_plugins_template2.cpp"


#endif
