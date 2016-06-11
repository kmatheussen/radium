//-----------------------------------------------------
//
// Code generated with Faust 0.9.73 (http://faust.grame.fr)
//-----------------------------------------------------
/* link with  */
#include <math.h>

/*
#include "/usr/share/faust/audio/dsp.h"
#include "/usr/share/faust/gui/UI.h"
*/

// We use faust1 here.

struct Meta
{
    void declare (const char* key, const char* value) { }
};

#include "faudiostream/architecture/faust/audio/dsp.h"
#include "faudiostream/architecture/faust/gui/UI.h"


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
#define FAUSTCLASS System_Eq_dsp
#endif

class System_Eq_dsp : public dsp {
  private:
	FAUSTFLOAT 	fslider0;
	FAUSTFLOAT 	fslider1;
	float 	fConst0;
	float 	fRec1[2];
	float 	fRec2[2];
	float 	fRec0[3];
	float 	fRec3[2];
  public:
	static void metadata(Meta* m) 	{ 
		m->declare("filter_smoothing.lib/name", "Faust Filter Library");
		m->declare("filter_smoothing.lib/author", "Julius O. Smith (jos at ccrma.stanford.edu)");
		m->declare("filter_smoothing.lib/copyright", "Julius O. Smith III");
		m->declare("filter_smoothing.lib/version", "1.29");
		m->declare("filter_smoothing.lib/license", "STK-4.3");
		m->declare("filter_smoothing.lib/reference", "https://ccrma.stanford.edu/~jos/filters/");
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
		fslider0 = 0.0f;
		fslider1 = 315.0f;
		fConst0 = (6.283185307179586f / float(min(192000, max(1, fSamplingFreq))));
		for (int i=0; i<2; i++) fRec1[i] = 0;
		for (int i=0; i<2; i++) fRec2[i] = 0;
		for (int i=0; i<3; i++) fRec0[i] = 0;
		for (int i=0; i<2; i++) fRec3[i] = 0;
	}
	virtual void init(int samplingFreq) {
		classInit(samplingFreq);
		instanceInit(samplingFreq);
	}
	virtual void buildUserInterface(UI* faust_interface) {
		faust_interface->declare(0, "3", "");
		faust_interface->openHorizontalBox("RM Peaking Equalizer 1");
		faust_interface->declare(&fslider1, "1", "");
		faust_interface->declare(&fslider1, "style", "knob");
		faust_interface->declare(&fslider1, "tooltip", "Center-frequency of second-order Regalia-Mitra peaking equalizer section 1");
		faust_interface->declare(&fslider1, "unit", "Hz");
		faust_interface->addVerticalSlider("Eq1 Freq", &fslider1, 315.0f, 4e+01f, 2e+04f, 1.0f);
		faust_interface->declare(&fslider0, "2", "");
		faust_interface->declare(&fslider0, "style", "knob");
		faust_interface->declare(&fslider0, "tooltip", "Peak level in dB of second-order Regalia-Mitra peaking equalizer section 1");
		faust_interface->declare(&fslider0, "unit", "dB");
		faust_interface->addVerticalSlider("Eq1 Level", &fslider0, 0.0f, -35.0f, 35.0f, 0.1f);
		faust_interface->closeBox();
	}
	virtual void compute (int count, FAUSTFLOAT** input, FAUSTFLOAT** output) {
		float 	fSlow0 = powf(10,(0.05f * float(fslider0)));
		float 	fSlow1 = float(fslider1);
		float 	fSlow2 = (fConst0 * (fSlow1 / sqrtf(max((float)0, fSlow0))));
		float 	fSlow3 = (0.0010000000000000009f * ((1.0f - fSlow2) / (1.0f + fSlow2)));
		float 	fSlow4 = (0.0010000000000000009f * (0 - cosf((fConst0 * fSlow1))));
		float 	fSlow5 = (0.0005000000000000004f * fSlow0);
		FAUSTFLOAT* input0 = input[0];
		FAUSTFLOAT* output0 = output[0];
		for (int i=0; i<count; i++) {
			float fTemp0 = (float)input0[i];
			fRec1[0] = (fSlow3 + (0.999f * fRec1[1]));
			fRec2[0] = (fSlow4 + (0.999f * fRec2[1]));
			float fTemp1 = ((fRec2[0] * (1 + fRec1[0])) * fRec0[1]);
			fRec0[0] = (0 - ((fTemp1 + (fRec1[0] * fRec0[2])) - fTemp0));
			float fTemp2 = (fRec1[0] * fRec0[0]);
			fRec3[0] = ((0.999f * fRec3[1]) + fSlow5);
			output0[i] = (FAUSTFLOAT)((0.5f * (fTemp2 + (fRec0[2] + (fTemp0 + fTemp1)))) + (fRec3[0] * ((fTemp2 + (fTemp1 + fRec0[2])) - fTemp0)));
			// post processing
			fRec3[1] = fRec3[0];
			fRec0[2] = fRec0[1]; fRec0[1] = fRec0[0];
			fRec2[1] = fRec2[0];
			fRec1[1] = fRec1[0];
		}
	}
};




#include "Faust_plugins_template2.cpp"

