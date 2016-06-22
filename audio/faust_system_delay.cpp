/* ------------------------------------------------------------
author: "Yann Orlarey"
copyright: "Grame"
license: "STK-4.3"
name: "SmoothDelay"
version: "1.0"
Code generated with Faust 2.0.a43 (http://faust.grame.fr)
------------------------------------------------------------ */

#ifndef  __System_Delay_dsp_H__
#define  __System_Delay_dsp_H__

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
#define FAUSTCLASS System_Delay_dsp
#endif

class System_Delay_dsp : public dsp {
	
  private:
	
	float fVec0[16384];
	float fRec0[2];
	float fRec1[2];
	float fRec2[2];
	float fRec3[2];
	int fSamplingFreq;
	float fConst0;
	FAUSTFLOAT fHslider0;
	int IOTA;
	
  public:
	
	void static metadata(Meta* m) { 
		m->declare("author", "Yann Orlarey");
		m->declare("copyright", "Grame");
		m->declare("license", "STK-4.3");
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
		m->declare("name", "SmoothDelay");
		m->declare("version", "1.0");
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
		fConst0 = (0.001f * min(1.92e+05f, max(1.0f, float(fSamplingFreq))));
		fHslider0 = FAUSTFLOAT(0.0f);
		for (int i0 = 0; (i0 < 2); i0 = (i0 + 1)) {
			fRec0[i0] = 0.0f;
			
		}
		for (int i1 = 0; (i1 < 2); i1 = (i1 + 1)) {
			fRec1[i1] = 0.0f;
			
		}
		for (int i2 = 0; (i2 < 2); i2 = (i2 + 1)) {
			fRec2[i2] = 0.0f;
			
		}
		for (int i3 = 0; (i3 < 2); i3 = (i3 + 1)) {
			fRec3[i3] = 0.0f;
			
		}
		IOTA = 0;
		for (int i4 = 0; (i4 < 16384); i4 = (i4 + 1)) {
			fVec0[i4] = 0.0f;
			
		}
		
	}
	
	virtual void init(int samplingFreq) {
		classInit(samplingFreq);
		instanceInit(samplingFreq);
	}
	
	virtual void buildUserInterface(UI* interface) {
		interface->openVerticalBox("0x00");
		interface->declare(&fHslider0, "style", "knob");
		interface->declare(&fHslider0, "unit", "ms");
		interface->addHorizontalSlider("delay", &fHslider0, 0.0f, 0.0f, 5e+01f, 0.1f);
		interface->closeBox();
		
	}
	
	virtual void compute(int count, FAUSTFLOAT** inputs, FAUSTFLOAT** outputs) {
		FAUSTFLOAT* input0 = inputs[0];
		FAUSTFLOAT* output0 = outputs[0];
		float fSlow0 = (fConst0 * float(fHslider0));
		for (int i = 0; (i < count); i = (i + 1)) {
			float fSel0 = 0.0f;
			if (((fRec1[1] == 0.0f) & (fSlow0 != fRec2[1])) != 0) {
				fSel0 = 3.0517578e-05f;
				
			} else {
				fSel0 = (((fRec1[1] == 1.0f) & (fSlow0 != fRec3[1]))?-3.0517578e-05f:0.0f);
				
			}
			float fSel1 = 0.0f;
			if ((fRec0[1] != 0.0f) != 0) {
				fSel1 = (((fRec1[1] > 0.0f) & (fRec1[1] < 1.0f))?fRec0[1]:0.0f);
				
			} else {
				fSel1 = fSel0;
				
			}
			fRec0[0] = fSel1;
			fRec1[0] = max(0.0f, min(1.0f, (fRec1[1] + fSel1)));
			fRec2[0] = (((fRec1[1] >= 1.0f) & (fRec3[1] != fSlow0))?fSlow0:fRec2[1]);
			fRec3[0] = (((fRec1[1] <= 0.0f) & (fRec2[1] != fSlow0))?fSlow0:fRec3[1]);
			float fTemp0 = float(input0[i]);
			fVec0[(IOTA & 16383)] = fTemp0;
			output0[i] = FAUSTFLOAT((((1.0f - fRec1[0]) * fVec0[((IOTA - (int(fRec2[0]) & 9599)) & 16383)]) + (fRec1[0] * fVec0[((IOTA - (int(fRec3[0]) & 9599)) & 16383)])));
			fRec0[1] = fRec0[0];
			fRec1[1] = fRec1[0];
			fRec2[1] = fRec2[0];
			fRec3[1] = fRec3[0];
			IOTA = (IOTA + 1);
			
		}
		
	}

	
};



#include "Faust_plugins_template2.cpp"


#endif
