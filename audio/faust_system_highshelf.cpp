/* ------------------------------------------------------------
Code generated with Faust 2.0.a43 (http://faust.grame.fr)
------------------------------------------------------------ */

#ifndef  __System_Highshelf_dsp_H__
#define  __System_Highshelf_dsp_H__

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
#define FAUSTCLASS System_Highshelf_dsp
#endif

class System_Highshelf_dsp : public dsp {
	
  private:
	
	float fRec2[3];
	float fRec4[3];
	float fRec0[2];
	float fRec1[2];
	float fVec0[2];
	float fRec3[2];
	float fRec5[2];
	FAUSTFLOAT fVslider0;
	int fSamplingFreq;
	float fConst0;
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
		fVslider0 = FAUSTFLOAT(0.0f);
		for (int i0 = 0; (i0 < 2); i0 = (i0 + 1)) {
			fRec0[i0] = 0.0f;
			
		}
		fConst0 = (3.1415927f / min(1.92e+05f, max(1.0f, float(fSamplingFreq))));
		fVslider1 = FAUSTFLOAT(315.0f);
		for (int i1 = 0; (i1 < 2); i1 = (i1 + 1)) {
			fRec1[i1] = 0.0f;
			
		}
		for (int i2 = 0; (i2 < 2); i2 = (i2 + 1)) {
			fVec0[i2] = 0.0f;
			
		}
		for (int i3 = 0; (i3 < 2); i3 = (i3 + 1)) {
			fRec3[i3] = 0.0f;
			
		}
		for (int i4 = 0; (i4 < 3); i4 = (i4 + 1)) {
			fRec2[i4] = 0.0f;
			
		}
		for (int i5 = 0; (i5 < 2); i5 = (i5 + 1)) {
			fRec5[i5] = 0.0f;
			
		}
		for (int i6 = 0; (i6 < 3); i6 = (i6 + 1)) {
			fRec4[i6] = 0.0f;
			
		}
		
	}
	
	virtual void init(int samplingFreq) {
		classInit(samplingFreq);
		instanceInit(samplingFreq);
	}
	
	virtual void buildUserInterface(UI* interface) {
		interface->openVerticalBox("0x00");
		interface->declare(&fVslider1, "0", "");
		interface->declare(&fVslider1, "unit", "Hz");
		interface->addVerticalSlider("Freq", &fVslider1, 315.0f, 4e+01f, 2e+03f, 1.0f);
		interface->declare(&fVslider0, "1", "");
		interface->declare(&fVslider0, "unit", "dB");
		interface->addVerticalSlider("Level", &fVslider0, 0.0f, -35.0f, 35.0f, 0.1f);
		interface->closeBox();
		
	}
	
	virtual void compute(int count, FAUSTFLOAT** inputs, FAUSTFLOAT** outputs) {
		FAUSTFLOAT* input0 = inputs[0];
		FAUSTFLOAT* output0 = outputs[0];
		float fSlow0 = (0.001f * powf(1e+01f, (0.05f * float(fVslider0))));
		float fSlow1 = (0.001f / tanf((fConst0 * float(fVslider1))));
		for (int i = 0; (i < count); i = (i + 1)) {
			fRec0[0] = ((0.999f * fRec0[1]) + fSlow0);
			fRec1[0] = ((0.999f * fRec1[1]) + fSlow1);
			float fTemp0 = faustpower2_f(fRec1[0]);
			float fTemp1 = (1.0f + fRec1[0]);
			float fTemp2 = (0.0f - ((1.0f - fRec1[0]) / fTemp1));
			float fTemp3 = float(input0[i]);
			fVec0[0] = fTemp3;
			fRec3[0] = ((fTemp2 * fRec3[1]) + (((fRec1[0] * fTemp3) + (fVec0[1] * (0.0f - fRec1[0]))) / fTemp1));
			float fTemp4 = (1.0f + (fRec1[0] * (fRec1[0] - 1.0f)));
			float fTemp5 = (1.0f - fTemp0);
			float fTemp6 = (1.0f + (fRec1[0] * (fRec1[0] + 1.0f)));
			fRec2[0] = (fRec3[0] - (((fTemp4 * fRec2[2]) + (2.0f * (fTemp5 * fRec2[1]))) / fTemp6));
			fRec5[0] = ((fTemp2 * fRec5[1]) + ((fVec0[1] + fTemp3) / fTemp1));
			fRec4[0] = (fRec5[0] - (((fTemp4 * fRec4[2]) + (2.0f * (fTemp5 * fRec4[1]))) / fTemp6));
			output0[i] = FAUSTFLOAT((((fRec0[0] * (((fTemp0 * fRec2[0]) + (2.0f * (fRec2[1] * (0.0f - fTemp0)))) + (fTemp0 * fRec2[2]))) + (fRec4[2] + (fRec4[0] + (2.0f * fRec4[1])))) / fTemp6));
			fRec0[1] = fRec0[0];
			fRec1[1] = fRec1[0];
			fVec0[1] = fVec0[0];
			fRec3[1] = fRec3[0];
			fRec2[2] = fRec2[1];
			fRec2[1] = fRec2[0];
			fRec5[1] = fRec5[0];
			fRec4[2] = fRec4[1];
			fRec4[1] = fRec4[0];
			
		}
		
	}

	
};



#include "Faust_plugins_template2.cpp"


#endif
