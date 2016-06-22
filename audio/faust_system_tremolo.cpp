/* ------------------------------------------------------------
Code generated with Faust 2.0.a43 (http://faust.grame.fr)
------------------------------------------------------------ */

#ifndef  __System_Tremolo_dsp_H__
#define  __System_Tremolo_dsp_H__

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


class System_Tremolo_dspSIG0 {
	
  private:
	
	int iRec1[2];
	
  public:
	
	int getNumInputsSystem_Tremolo_dspSIG0() {
		return 0;
		
	}
	int getNumOutputsSystem_Tremolo_dspSIG0() {
		return 1;
		
	}
	int getInputRateSystem_Tremolo_dspSIG0(int channel) {
		int rate;
		switch (channel) {
			default: {
				rate = -1;
				break;
			}
			
		}
		return rate;
		
	}
	int getOutputRateSystem_Tremolo_dspSIG0(int channel) {
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
	
	void instanceInitSystem_Tremolo_dspSIG0(int samplingFreq) {
		for (int i1 = 0; (i1 < 2); i1 = (i1 + 1)) {
			iRec1[i1] = 0;
			
		}
		
	}
	
	void fillSystem_Tremolo_dspSIG0(int count, float* output) {
		for (int i = 0; (i < count); i = (i + 1)) {
			iRec1[0] = (1 + iRec1[1]);
			output[i] = sinf((9.58738e-05f * float((iRec1[0] - 1))));
			iRec1[1] = iRec1[0];
			
		}
		
	}
};

System_Tremolo_dspSIG0* newSystem_Tremolo_dspSIG0() { return (System_Tremolo_dspSIG0*)new System_Tremolo_dspSIG0(); }
void deleteSystem_Tremolo_dspSIG0(System_Tremolo_dspSIG0* dsp) { delete dsp; }

static float ftbl0System_Tremolo_dspSIG0[65536];

#ifndef FAUSTCLASS 
#define FAUSTCLASS System_Tremolo_dsp
#endif

class System_Tremolo_dsp : public dsp {
	
  private:
	
	float fRec0[2];
	float fRec3[2];
	float fRec2[2];
	FAUSTFLOAT fHslider0;
	int fSamplingFreq;
	float fConst0;
	FAUSTFLOAT fHslider1;
	
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
		return 2;
		
	}
	virtual int getNumOutputs() {
		return 2;
		
	}
	virtual int getInputRate(int channel) {
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
		System_Tremolo_dspSIG0* sig0 = newSystem_Tremolo_dspSIG0();
		sig0->instanceInitSystem_Tremolo_dspSIG0(samplingFreq);
		sig0->fillSystem_Tremolo_dspSIG0(65536, ftbl0System_Tremolo_dspSIG0);
		deleteSystem_Tremolo_dspSIG0(sig0);
		
	}
	
	virtual void instanceInit(int samplingFreq) {
		fSamplingFreq = samplingFreq;
		fHslider0 = FAUSTFLOAT(0.0f);
		for (int i0 = 0; (i0 < 2); i0 = (i0 + 1)) {
			fRec0[i0] = 0.0f;
			
		}
		fConst0 = (1.0f / min(1.92e+05f, max(1.0f, float(fSamplingFreq))));
		fHslider1 = FAUSTFLOAT(5.0f);
		for (int i2 = 0; (i2 < 2); i2 = (i2 + 1)) {
			fRec3[i2] = 0.0f;
			
		}
		for (int i3 = 0; (i3 < 2); i3 = (i3 + 1)) {
			fRec2[i3] = 0.0f;
			
		}
		
	}
	
	virtual void init(int samplingFreq) {
		classInit(samplingFreq);
		instanceInit(samplingFreq);
	}
	
	virtual void buildUserInterface(UI* interface) {
		interface->openVerticalBox("0x00");
		interface->declare(&fHslider1, "1", "");
		interface->addHorizontalSlider("frequency", &fHslider1, 5.0f, 0.1f, 15.0f, 0.01f);
		interface->declare(&fHslider0, "2", "");
		interface->addHorizontalSlider("depth", &fHslider0, 0.0f, 0.0f, 1.0f, 0.01f);
		interface->closeBox();
		
	}
	
	virtual void compute(int count, FAUSTFLOAT** inputs, FAUSTFLOAT** outputs) {
		FAUSTFLOAT* input0 = inputs[0];
		FAUSTFLOAT* input1 = inputs[1];
		FAUSTFLOAT* output0 = outputs[0];
		FAUSTFLOAT* output1 = outputs[1];
		float fSlow0 = (0.001f * float(fHslider0));
		float fSlow1 = (0.001f * float(fHslider1));
		for (int i = 0; (i < count); i = (i + 1)) {
			fRec0[0] = ((0.999f * fRec0[1]) + fSlow0);
			fRec3[0] = ((0.999f * fRec3[1]) + fSlow1);
			float fTemp0 = (fRec2[1] + (fConst0 * fRec3[0]));
			fRec2[0] = (fTemp0 - floorf(fTemp0));
			float fTemp1 = (1.0f - (0.5f * (fRec0[0] * (1.0f + ftbl0System_Tremolo_dspSIG0[int((65536.0f * fRec2[0]))]))));
			output0[i] = FAUSTFLOAT((fTemp1 * float(input0[i])));
			output1[i] = FAUSTFLOAT((fTemp1 * float(input1[i])));
			fRec0[1] = fRec0[0];
			fRec3[1] = fRec3[0];
			fRec2[1] = fRec2[0];
			
		}
		
	}

	
};



#include "Faust_plugins_template2.cpp"


#endif
