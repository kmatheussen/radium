//-----------------------------------------------------
// name: "Sitar"
// author: "Romain Michon (rmichon@ccrma.stanford.edu)"
// copyright: "Romain Michon"
// version: "1.0"
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
#define FAUSTCLASS Sitar_dsp
#endif

class Sitar_dsp : public dsp {
  private:
	int 	iRec2[2];
	float 	fRec1[2];
	FAUSTFLOAT 	fentry0;
	int 	iConst0;
	FAUSTFLOAT 	fslider0;
	float 	fVec0[2];
	FAUSTFLOAT 	fbutton0;
	int 	iRec3[2];
	float 	fConst1;
	float 	fConst2;
	float 	fConst3;
	float 	fRec4[2];
	FAUSTFLOAT 	fentry1;
	int 	IOTA;
	float 	fRec0[8192];
	FAUSTFLOAT 	fslider1;
	FAUSTFLOAT 	fslider2;
	float 	fConst4;
  public:
	static void metadata(Meta* m) 	{ 
		m->declare("name", "Sitar");
		m->declare("description", "WaveGuide Sitar");
		m->declare("author", "Romain Michon (rmichon@ccrma.stanford.edu)");
		m->declare("copyright", "Romain Michon");
		m->declare("version", "1.0");
		m->declare("licence", "STK-4.3");
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
		m->declare("instrument.lib/name", "Faust-STK Tools Library");
		m->declare("instrument.lib/author", "Romain Michon (rmichon@ccrma.stanford.edu)");
		m->declare("instrument.lib/copyright", "Romain Michon");
		m->declare("instrument.lib/version", "1.0");
		m->declare("instrument.lib/licence", "STK-4.3");
		m->declare("filter.lib/name", "Faust Filter Library");
		m->declare("filter.lib/author", "Julius O. Smith (jos at ccrma.stanford.edu)");
		m->declare("filter.lib/copyright", "Julius O. Smith III");
		m->declare("filter.lib/version", "1.29");
		m->declare("filter.lib/license", "STK-4.3");
		m->declare("filter.lib/reference", "https://ccrma.stanford.edu/~jos/filters/");
		m->declare("effect.lib/name", "Faust Audio Effect Library");
		m->declare("effect.lib/author", "Julius O. Smith (jos at ccrma.stanford.edu)");
		m->declare("effect.lib/copyright", "Julius O. Smith III");
		m->declare("effect.lib/version", "1.33");
		m->declare("effect.lib/license", "STK-4.3");
		m->declare("effect.lib/exciter_name", "Harmonic Exciter");
		m->declare("effect.lib/exciter_author", "Priyanka Shekar (pshekar@ccrma.stanford.edu)");
		m->declare("effect.lib/exciter_copyright", "Copyright (c) 2013 Priyanka Shekar");
		m->declare("effect.lib/exciter_version", "1.0");
		m->declare("effect.lib/exciter_license", "MIT License (MIT)");
	}

	virtual int getNumInputs() 	{ return 0; }
	virtual int getNumOutputs() 	{ return 2; }
	static void classInit(int samplingFreq) {
	}
	virtual void instanceInit(int samplingFreq) {
		fSamplingFreq = samplingFreq;
		for (int i=0; i<2; i++) iRec2[i] = 0;
		for (int i=0; i<2; i++) fRec1[i] = 0;
		fentry0 = 4.4e+02f;
		iConst0 = min(192000, max(1, fSamplingFreq));
		fslider0 = 0.7f;
		for (int i=0; i<2; i++) fVec0[i] = 0;
		fbutton0 = 0.0;
		for (int i=0; i<2; i++) iRec3[i] = 0;
		fConst1 = (1 - (1.0f / powf(1.0f,(2.0f / float(iConst0)))));
		fConst2 = (1 - powf(0.001f,(25.0f / float(iConst0))));
		fConst3 = (1e+03f / float(iConst0));
		for (int i=0; i<2; i++) fRec4[i] = 0;
		fentry1 = 1.0f;
		IOTA = 0;
		for (int i=0; i<8192; i++) fRec0[i] = 0;
		fslider1 = 0.6f;
		fslider2 = 0.5f;
		fConst4 = (0.5f * iConst0);
	}
	virtual void init(int samplingFreq) {
		classInit(samplingFreq);
		instanceInit(samplingFreq);
	}
	virtual void buildUserInterface(UI* faust_interface) {
		faust_interface->openVerticalBox("0x00");
		faust_interface->openHorizontalBox("Basic_Parameters");
		faust_interface->declare(&fentry0, "1", "");
		faust_interface->declare(&fentry0, "tooltip", "Tone frequency");
		faust_interface->declare(&fentry0, "unit", "Hz");
		faust_interface->addNumEntry("freq", &fentry0, 4.4e+02f, 2e+01f, 2e+04f, 1.0f);
		faust_interface->declare(&fentry1, "1", "");
		faust_interface->declare(&fentry1, "tooltip", "Gain (value between 0 and 1)");
		faust_interface->addNumEntry("gain", &fentry1, 1.0f, 0.0f, 1.0f, 0.01f);
		faust_interface->declare(&fbutton0, "1", "");
		faust_interface->declare(&fbutton0, "tooltip", "noteOn = 1, noteOff = 0");
		faust_interface->addButton("gate", &fbutton0);
		faust_interface->closeBox();
		faust_interface->openVerticalBox("Physical_Parameters");
		faust_interface->declare(&fslider0, "2", "");
		faust_interface->declare(&fslider0, "tooltip", "A value between 0 and 1");
		faust_interface->addHorizontalSlider("Resonance", &fslider0, 0.7f, 0.0f, 1.0f, 0.01f);
		faust_interface->closeBox();
		faust_interface->openVerticalBox("Spat");
		faust_interface->addHorizontalSlider("pan angle", &fslider1, 0.6f, 0.0f, 1.0f, 0.01f);
		faust_interface->addHorizontalSlider("spatial width", &fslider2, 0.5f, 0.0f, 1.0f, 0.01f);
		faust_interface->closeBox();
		faust_interface->closeBox();
	}
	virtual void compute (int count, FAUSTFLOAT** input, FAUSTFLOAT** output) {
		float 	fSlow0 = float(fentry0);
		float 	fSlow1 = (float(iConst0) / fSlow0);
		float 	fSlow2 = (0.895f + ((0.1f * float(fslider0)) + (5e-07f * fSlow0)));
		float 	fSlow3 = float(fbutton0);
		int 	iSlow4 = (fSlow3 > 0);
		int 	iSlow5 = (fSlow3 <= 0);
		float 	fSlow6 = (4.656612875245797e-11f * float(fentry1));
		float 	fSlow7 = (0.9900990099009901f * fSlow2);
		float 	fSlow8 = float(fslider1);
		float 	fSlow9 = (8 * (1.0f - fSlow8));
		int 	iSlow10 = int((int((fConst4 * (float(fslider2) / fSlow0))) & 4095));
		float 	fSlow11 = (8 * fSlow8);
		FAUSTFLOAT* output0 = output[0];
		FAUSTFLOAT* output1 = output[1];
		for (int i=0; i<count; i++) {
			iRec2[0] = (12345 + (1103515245 * iRec2[1]));
			fRec1[0] = ((0.9992f * fRec1[1]) + (0.0008000000000000229f * (1 + (2.3283064376228985e-10f * iRec2[0]))));
			float fTemp0 = fRec0[(IOTA-int((1 + int((int((fSlow1 * fRec1[0])) & 4095)))))&8191];
			fVec0[0] = (fSlow2 * fTemp0);
			iRec3[0] = (iSlow4 & (iRec3[1] | (fRec4[1] >= 1)));
			int iTemp1 = (fRec4[1] > 0);
			int iTemp2 = (iSlow5 & iTemp1);
			fRec4[0] = (((fConst3 * (((iRec3[1] == 0) & iSlow4) & (fRec4[1] < 1))) + (fRec4[1] * ((1 - (fConst2 * (iRec3[1] & iTemp1))) - (fConst1 * iTemp2)))) * ((iTemp2 == 0) | (fRec4[1] >= 1e-06f)));
			fRec0[IOTA&8191] = (((fSlow7 * fTemp0) + (fSlow6 * (iRec2[0] * fRec4[0]))) - (0.009900990099009901f * fVec0[1]));
			output0[i] = (FAUSTFLOAT)(fSlow9 * fRec0[(IOTA-0)&8191]);
			output1[i] = (FAUSTFLOAT)(fSlow11 * fRec0[(IOTA-iSlow10)&8191]);
			// post processing
			IOTA = IOTA+1;
			fRec4[1] = fRec4[0];
			iRec3[1] = iRec3[0];
			fVec0[1] = fVec0[0];
			fRec1[1] = fRec1[0];
			iRec2[1] = iRec2[0];
		}
	}
};




#include "Faust_plugins_template2.cpp"

