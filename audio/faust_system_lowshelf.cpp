//-----------------------------------------------------
//
// Code generated with Faust 0.9.73 (http://faust.grame.fr)
//-----------------------------------------------------
/* link with  */
#include <math.h>
#ifndef FAUSTPOWER
#define FAUSTPOWER
#include <cmath>
template <int N> inline float faustpower(float x)          { return powf(x,N); } 
template <int N> inline double faustpower(double x)        { return pow(x,N); }
template <int N> inline int faustpower(int x)              { return faustpower<N/2>(x) * faustpower<N-N/2>(x); } 
template <> 	 inline int faustpower<0>(int x)            { return 1; }
template <> 	 inline int faustpower<1>(int x)            { return x; }
template <> 	 inline int faustpower<2>(int x)            { return x*x; }
#endif

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
#define FAUSTCLASS System_Lowshelf_dsp
#endif

class System_Lowshelf_dsp : public dsp {
  private:
	FAUSTFLOAT 	fslider0;
	float 	fConst0;
	float 	fRec0[2];
	float 	fVec0[2];
	float 	fRec2[2];
	float 	fRec1[3];
	FAUSTFLOAT 	fslider1;
	float 	fRec3[2];
	float 	fRec5[2];
	float 	fRec4[3];
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
		fslider0 = 315.0f;
		fConst0 = (3.141592653589793f / float(min(192000, max(1, fSamplingFreq))));
		for (int i=0; i<2; i++) fRec0[i] = 0;
		for (int i=0; i<2; i++) fVec0[i] = 0;
		for (int i=0; i<2; i++) fRec2[i] = 0;
		for (int i=0; i<3; i++) fRec1[i] = 0;
		fslider1 = 0.0f;
		for (int i=0; i<2; i++) fRec3[i] = 0;
		for (int i=0; i<2; i++) fRec5[i] = 0;
		for (int i=0; i<3; i++) fRec4[i] = 0;
	}
	virtual void init(int samplingFreq) {
		classInit(samplingFreq);
		instanceInit(samplingFreq);
	}
	virtual void buildUserInterface(UI* faust_interface) {
		faust_interface->openVerticalBox("0x00");
		faust_interface->declare(&fslider0, "0", "");
		faust_interface->declare(&fslider0, "unit", "Hz");
		faust_interface->addVerticalSlider("Lowshelf Freq", &fslider0, 315.0f, 4e+01f, 2e+03f, 1.0f);
		faust_interface->declare(&fslider1, "1", "");
		faust_interface->declare(&fslider1, "unit", "dB");
		faust_interface->addVerticalSlider("Level", &fslider1, 0.0f, -35.0f, 35.0f, 0.1f);
		faust_interface->closeBox();
	}
	virtual void compute (int count, FAUSTFLOAT** input, FAUSTFLOAT** output) {
		float 	fSlow0 = (0.0010000000000000009f / tanf((fConst0 * float(fslider0))));
		float 	fSlow1 = (0.0010000000000000009f * powf(10,(0.05f * float(fslider1))));
		FAUSTFLOAT* input0 = input[0];
		FAUSTFLOAT* output0 = output[0];
		for (int i=0; i<count; i++) {
			fRec0[0] = (fSlow0 + (0.999f * fRec0[1]));
			float fTemp0 = (1 + (fRec0[0] * (1.0000000000000004f + fRec0[0])));
			float fTemp1 = faustpower<2>(fRec0[0]);
			float fTemp2 = (1 - fTemp1);
			float fTemp3 = (1 + (fRec0[0] * (fRec0[0] - 1.0000000000000004f)));
			float fTemp4 = (1 + fRec0[0]);
			float fTemp5 = (float)input0[i];
			fVec0[0] = fTemp5;
			float fTemp6 = (0 - ((1 - fRec0[0]) / fTemp4));
			fRec2[0] = ((fTemp6 * fRec2[1]) + ((fVec0[0] + fVec0[1]) / fTemp4));
			fRec1[0] = (fRec2[0] - (((fTemp3 * fRec1[2]) + (2 * (fTemp2 * fRec1[1]))) / fTemp0));
			fRec3[0] = ((0.999f * fRec3[1]) + fSlow1);
			fRec5[0] = ((fTemp6 * fRec5[1]) + (((fVec0[0] * fRec0[0]) + (fVec0[1] * (0 - fRec0[0]))) / fTemp4));
			fRec4[0] = (fRec5[0] - (((fTemp3 * fRec4[2]) + (2 * (fTemp2 * fRec4[1]))) / fTemp0));
			output0[i] = (FAUSTFLOAT)(((((fTemp1 * fRec4[0]) + (2 * (fRec4[1] * (0 - fTemp1)))) + (fTemp1 * fRec4[2])) + (fRec3[0] * (fRec1[2] + (fRec1[0] + (2 * fRec1[1]))))) / fTemp0);
			// post processing
			fRec4[2] = fRec4[1]; fRec4[1] = fRec4[0];
			fRec5[1] = fRec5[0];
			fRec3[1] = fRec3[0];
			fRec1[2] = fRec1[1]; fRec1[1] = fRec1[0];
			fRec2[1] = fRec2[0];
			fVec0[1] = fVec0[0];
			fRec0[1] = fRec0[0];
		}
	}
};




#include "Faust_plugins_template2.cpp"

