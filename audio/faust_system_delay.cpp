//-----------------------------------------------------
// name: "SmoothDelay"
// author: "Yann Orlarey"
// copyright: "Grame"
// version: "1.0"
// license: "STK-4.3"
//
// Code generated with Faust 0.9.73 (http://faust.grame.fr)
//-----------------------------------------------------
/* link with  */
#include <math.h>
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


#ifndef FAUSTCLASS 
#define FAUSTCLASS System_Delay_dsp
#endif

class System_Delay_dsp : public dsp {
  private:
	int 	IOTA;
	float 	fVec0[16384];
	FAUSTFLOAT 	fslider0;
	float 	fConst0;
	float 	fRec0[2];
	float 	fRec1[2];
	float 	fRec2[2];
	float 	fRec3[2];
  public:
	static void metadata(Meta* m) 	{ 
		m->declare("name", "SmoothDelay");
		m->declare("author", "Yann Orlarey");
		m->declare("copyright", "Grame");
		m->declare("version", "1.0");
		m->declare("license", "STK-4.3");
		m->declare("music.lib/name", "Music Library");
		m->declare("music.lib/author", "GRAME");
		m->declare("music.lib/copyright", "GRAME");
		m->declare("music.lib/version", "1.0");
		m->declare("music.lib/license", "LGPL with exception");
		m->declare("math.lib/name", "Math Library");
		m->declare("math.lib/author", "GRAME");
		m->declare("math.lib/copyright", "GRAME");
		m->declare("math.lib/version", "1.0");
		m->declare("math.lib/license", "LGPL with exception");
	}

	virtual int getNumInputs() 	{ return 1; }
	virtual int getNumOutputs() 	{ return 1; }
	static void classInit(int samplingFreq) {
	}
	virtual void instanceInit(int samplingFreq) {
		fSamplingFreq = samplingFreq;
		IOTA = 0;
		for (int i=0; i<16384; i++) fVec0[i] = 0;
		fslider0 = 0.0f;
		fConst0 = (0.001f * min(192000, max(1, fSamplingFreq)));
		for (int i=0; i<2; i++) fRec0[i] = 0;
		for (int i=0; i<2; i++) fRec1[i] = 0;
		for (int i=0; i<2; i++) fRec2[i] = 0;
		for (int i=0; i<2; i++) fRec3[i] = 0;
	}
	virtual void init(int samplingFreq) {
		classInit(samplingFreq);
		instanceInit(samplingFreq);
	}
	virtual void buildUserInterface(UI* faust_interface) {
		faust_interface->openVerticalBox("0x00");
		faust_interface->declare(&fslider0, "style", "knob");
		faust_interface->declare(&fslider0, "unit", "ms");
		faust_interface->addHorizontalSlider("delay", &fslider0, 0.0f, 0.0f, 5e+01f, 0.1f);
		faust_interface->closeBox();
	}
	virtual void compute (int count, FAUSTFLOAT** input, FAUSTFLOAT** output) {
		float 	fSlow0 = (fConst0 * float(fslider0));
		FAUSTFLOAT* input0 = input[0];
		FAUSTFLOAT* output0 = output[0];
		for (int i=0; i<count; i++) {
			float fTemp0 = (float)input0[i];
			fVec0[IOTA&16383] = fTemp0;
			float fTemp1 = ((int((fRec0[1] != 0.0f)))?((int(((fRec1[1] > 0.0f) & (fRec1[1] < 1.0f))))?fRec0[1]:0):((int(((fRec1[1] == 0.0f) & (fSlow0 != fRec2[1]))))?3.0517578125e-05f:((int(((fRec1[1] == 1.0f) & (fSlow0 != fRec3[1]))))?-3.0517578125e-05f:0)));
			fRec0[0] = fTemp1;
			fRec1[0] = max(0.0f, min(1.0f, (fRec1[1] + fTemp1)));
			fRec2[0] = ((int(((fRec1[1] >= 1.0f) & (fRec3[1] != fSlow0))))?fSlow0:fRec2[1]);
			fRec3[0] = ((int(((fRec1[1] <= 0.0f) & (fRec2[1] != fSlow0))))?fSlow0:fRec3[1]);
			output0[i] = (FAUSTFLOAT)(((1.0f - fRec1[0]) * fVec0[(IOTA-int((int(fRec2[0]) & 9599)))&16383]) + (fRec1[0] * fVec0[(IOTA-int((int(fRec3[0]) & 9599)))&16383]));
			// post processing
			fRec3[1] = fRec3[0];
			fRec2[1] = fRec2[0];
			fRec1[1] = fRec1[0];
			fRec0[1] = fRec0[0];
			IOTA = IOTA+1;
		}
	}
};




#include "Faust_plugins_template2.cpp"

