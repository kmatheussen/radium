//-----------------------------------------------------
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
#define FAUSTCLASS System_Tremolo_dsp
#endif

class System_Tremolo_dsp : public dsp {
  private:
	class SIG0 {
	  private:
		int 	fSamplingFreq;
		int 	iRec0[2];
	  public:
		int getNumInputs() 	{ return 0; }
		int getNumOutputs() 	{ return 1; }
		void init(int samplingFreq) {
			fSamplingFreq = samplingFreq;
			for (int i=0; i<2; i++) iRec0[i] = 0;
		}
		void fill (int count, float output[]) {
			for (int i=0; i<count; i++) {
				iRec0[0] = (1 + iRec0[1]);
				output[i] = sinf((9.587379924285257e-05f * float((iRec0[0] - 1))));
				// post processing
				iRec0[1] = iRec0[0];
			}
		}
	};


	static float 	ftbl0[65536];
	FAUSTFLOAT 	fslider0;
	float 	fRec2[2];
	float 	fConst0;
	float 	fRec1[2];
	FAUSTFLOAT 	fslider1;
	float 	fRec3[2];
  public:
	static void metadata(Meta* m) 	{ 
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
		m->declare("filter_smoothing.lib/name", "Faust Filter Library");
		m->declare("filter_smoothing.lib/author", "Julius O. Smith (jos at ccrma.stanford.edu)");
		m->declare("filter_smoothing.lib/copyright", "Julius O. Smith III");
		m->declare("filter_smoothing.lib/version", "1.29");
		m->declare("filter_smoothing.lib/license", "STK-4.3");
		m->declare("filter_smoothing.lib/reference", "https://ccrma.stanford.edu/~jos/filters/");
	}

	virtual int getNumInputs() 	{ return 2; }
	virtual int getNumOutputs() 	{ return 2; }
	static void classInit(int samplingFreq) {
		SIG0 sig0;
		sig0.init(samplingFreq);
		sig0.fill(65536,ftbl0);
	}
	virtual void instanceInit(int samplingFreq) {
		fSamplingFreq = samplingFreq;
		fslider0 = 5.0f;
		for (int i=0; i<2; i++) fRec2[i] = 0;
		fConst0 = (1.0f / float(min(192000, max(1, fSamplingFreq))));
		for (int i=0; i<2; i++) fRec1[i] = 0;
		fslider1 = 0.0f;
		for (int i=0; i<2; i++) fRec3[i] = 0;
	}
	virtual void init(int samplingFreq) {
		classInit(samplingFreq);
		instanceInit(samplingFreq);
	}
	virtual void buildUserInterface(UI* faust_interface) {
		faust_interface->openVerticalBox("0x00");
		faust_interface->declare(&fslider0, "1", "");
		faust_interface->addHorizontalSlider("frequency", &fslider0, 5.0f, 0.1f, 15.0f, 0.01f);
		faust_interface->declare(&fslider1, "2", "");
		faust_interface->addHorizontalSlider("depth", &fslider1, 0.0f, 0.0f, 1.0f, 0.01f);
		faust_interface->closeBox();
	}
	virtual void compute (int count, FAUSTFLOAT** input, FAUSTFLOAT** output) {
		float 	fSlow0 = (0.0010000000000000009f * float(fslider0));
		float 	fSlow1 = (0.0010000000000000009f * float(fslider1));
		FAUSTFLOAT* input0 = input[0];
		FAUSTFLOAT* input1 = input[1];
		FAUSTFLOAT* output0 = output[0];
		FAUSTFLOAT* output1 = output[1];
		for (int i=0; i<count; i++) {
			fRec2[0] = (fSlow0 + (0.999f * fRec2[1]));
			float fTemp0 = (fRec1[1] + (fConst0 * fRec2[0]));
			fRec1[0] = (fTemp0 - floorf(fTemp0));
			fRec3[0] = ((0.999f * fRec3[1]) + fSlow1);
			float fTemp1 = (1 - (0.5f * (fRec3[0] * (1.0f + ftbl0[int((65536.0f * fRec1[0]))]))));
			output0[i] = (FAUSTFLOAT)((float)input0[i] * fTemp1);
			output1[i] = (FAUSTFLOAT)((float)input1[i] * fTemp1);
			// post processing
			fRec3[1] = fRec3[0];
			fRec1[1] = fRec1[0];
			fRec2[1] = fRec2[0];
		}
	}
};


float 	System_Tremolo_dsp::ftbl0[65536];


#include "Faust_plugins_template2.cpp"

