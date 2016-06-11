//-----------------------------------------------------
// name: "Harpsichord"
// author: "Romain Michon (rmichon@ccrma.stanford.edu)"
// copyright: "Romain Michon"
// version: "1.0"
//
// Code generated with Faust 0.9.73 (http://faust.grame.fr)
//-----------------------------------------------------
/* link with : "" */
#include <harpsichord.h>
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
#define FAUSTCLASS Harpsi_dsp
#endif

class Harpsi_dsp : public dsp {
  private:
	class SIG0 {
	  private:
		int 	fSamplingFreq;
		int 	iRec12[2];
	  public:
		int getNumInputs() 	{ return 0; }
		int getNumOutputs() 	{ return 1; }
		void init(int samplingFreq) {
			fSamplingFreq = samplingFreq;
			for (int i=0; i<2; i++) iRec12[i] = 0;
		}
		void fill (int count, float output[]) {
			for (int i=0; i<count; i++) {
				iRec12[0] = (1 + iRec12[1]);
				output[i] = sinf((9.587379924285257e-05f * float((iRec12[0] - 1))));
				// post processing
				iRec12[1] = iRec12[0];
			}
		}
	};


	int 	iRec2[2];
	FAUSTFLOAT 	fbutton0;
	float 	fRec4[2];
	FAUSTFLOAT 	fentry0;
	FAUSTFLOAT 	fentry1;
	int 	iConst0;
	float 	fConst1;
	float 	fConst2;
	float 	fRec3[2];
	FAUSTFLOAT 	fslider0;
	float 	fRec5[2];
	FAUSTFLOAT 	fentry2;
	float 	fRec11[2];
	float 	fRec10[2];
	float 	fRec9[2];
	float 	fRec8[2];
	float 	fRec7[2];
	float 	fRec6[2];
	static float 	ftbl0[65536];
	FAUSTFLOAT 	fslider1;
	float 	fRec14[2];
	float 	fConst3;
	float 	fRec13[2];
	float 	fRec20[2];
	float 	fRec19[2];
	float 	fRec18[2];
	float 	fRec17[2];
	float 	fRec16[2];
	float 	fRec15[2];
	float 	fRec21[2];
	int 	IOTA;
	float 	fVec0[4096];
	float 	fRec1[3];
	float 	fRec0[4096];
	FAUSTFLOAT 	fslider2;
	FAUSTFLOAT 	fslider3;
	float 	fConst4;
  public:
	static void metadata(Meta* m) 	{ 
		m->declare("name", "Harpsichord");
		m->declare("description", "Nonlinear WaveGuide Commuted Harpsichord");
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
		SIG0 sig0;
		sig0.init(samplingFreq);
		sig0.fill(65536,ftbl0);
	}
	virtual void instanceInit(int samplingFreq) {
		fSamplingFreq = samplingFreq;
		for (int i=0; i<2; i++) iRec2[i] = 0;
		fbutton0 = 0.0;
		for (int i=0; i<2; i++) fRec4[i] = 0;
		fentry0 = 4.4e+02f;
		fentry1 = 0.8f;
		iConst0 = min(192000, max(1, fSamplingFreq));
		fConst1 = float(iConst0);
		fConst2 = (float(7) / fConst1);
		for (int i=0; i<2; i++) fRec3[i] = 0;
		fslider0 = 0.0f;
		for (int i=0; i<2; i++) fRec5[i] = 0;
		fentry2 = 0.0f;
		for (int i=0; i<2; i++) fRec11[i] = 0;
		for (int i=0; i<2; i++) fRec10[i] = 0;
		for (int i=0; i<2; i++) fRec9[i] = 0;
		for (int i=0; i<2; i++) fRec8[i] = 0;
		for (int i=0; i<2; i++) fRec7[i] = 0;
		for (int i=0; i<2; i++) fRec6[i] = 0;
		fslider1 = 2.2e+02f;
		for (int i=0; i<2; i++) fRec14[i] = 0;
		fConst3 = (1.0f / fConst1);
		for (int i=0; i<2; i++) fRec13[i] = 0;
		for (int i=0; i<2; i++) fRec20[i] = 0;
		for (int i=0; i<2; i++) fRec19[i] = 0;
		for (int i=0; i<2; i++) fRec18[i] = 0;
		for (int i=0; i<2; i++) fRec17[i] = 0;
		for (int i=0; i<2; i++) fRec16[i] = 0;
		for (int i=0; i<2; i++) fRec15[i] = 0;
		for (int i=0; i<2; i++) fRec21[i] = 0;
		IOTA = 0;
		for (int i=0; i<4096; i++) fVec0[i] = 0;
		for (int i=0; i<3; i++) fRec1[i] = 0;
		for (int i=0; i<4096; i++) fRec0[i] = 0;
		fslider2 = 0.6f;
		fslider3 = 0.5f;
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
		faust_interface->addNumEntry("gain", &fentry1, 0.8f, 0.0f, 1.0f, 0.01f);
		faust_interface->declare(&fbutton0, "1", "");
		faust_interface->declare(&fbutton0, "tooltip", "noteOn = 1, noteOff = 0");
		faust_interface->addButton("gate", &fbutton0);
		faust_interface->closeBox();
		faust_interface->openHorizontalBox("Physical_and_Nonlinearity");
		faust_interface->openVerticalBox("Nonlinear_Filter_Parameters");
		faust_interface->declare(&fslider1, "2", "");
		faust_interface->declare(&fslider1, "tooltip", "Frequency of the sine wave for the modulation of theta (works if Modulation Type=3)");
		faust_interface->declare(&fslider1, "unit", "Hz");
		faust_interface->addHorizontalSlider("Modulation_Frequency", &fslider1, 2.2e+02f, 2e+01f, 1e+03f, 0.1f);
		faust_interface->declare(&fentry2, "2", "");
		faust_interface->declare(&fentry2, "tooltip", "0=theta is modulated by the incoming signal; 1=theta is modulated by the averaged incoming signal; 2=theta is modulated by the squared incoming signal; 3=theta is modulated by a sine wave of frequency freqMod; 4=theta is modulated by a sine wave of frequency freq;");
		faust_interface->addNumEntry("Modulation_Type", &fentry2, 0.0f, 0.0f, 4.0f, 1.0f);
		faust_interface->declare(&fslider0, "2", "");
		faust_interface->declare(&fslider0, "tooltip", "Nonlinearity factor (value between 0 and 1)");
		faust_interface->addHorizontalSlider("Nonlinearity", &fslider0, 0.0f, 0.0f, 1.0f, 0.01f);
		faust_interface->closeBox();
		faust_interface->closeBox();
		faust_interface->openVerticalBox("Spat");
		faust_interface->addHorizontalSlider("pan angle", &fslider2, 0.6f, 0.0f, 1.0f, 0.01f);
		faust_interface->addHorizontalSlider("spatial width", &fslider3, 0.5f, 0.0f, 1.0f, 0.01f);
		faust_interface->closeBox();
		faust_interface->closeBox();
	}
	virtual void compute (int count, FAUSTFLOAT** input, FAUSTFLOAT** output) {
		float 	fSlow0 = float(fbutton0);
		int 	iSlow1 = (fSlow0 < 1);
		float 	fSlow2 = float(fentry0);
		int 	iSlow3 = int(((17.31234049066756f * (logf(fSlow2) - 6.0867747269123065f)) + 69.5f));
		float 	fSlow4 = expf((0 - (fConst2 / (float(fentry1) * getValueDryTapAmpT60(iSlow3)))));
		int 	iSlow5 = (fSlow0 > 0);
		float 	fSlow6 = (0.0010000000000000009f * float(fslider0));
		float 	fSlow7 = float(fentry2);
		float 	fSlow8 = (3.141592653589793f * (fSlow7 == 2));
		float 	fSlow9 = (1.5707963267948966f * (fSlow7 == 1));
		float 	fSlow10 = (3.141592653589793f * (fSlow7 == 0));
		int 	iSlow11 = (fSlow7 < 3);
		float 	fSlow12 = (0.0010000000000000009f * float(fslider1));
		int 	iSlow13 = (fSlow7 != 4);
		float 	fSlow14 = (fSlow2 * (fSlow7 == 4));
		int 	iSlow15 = (fSlow7 >= 3);
		float 	fSlow16 = (0.0010000000000000009f * ((0.9996f * fSlow0) + (0.9f * (iSlow1 * getValueReleaseLoopGain(iSlow3)))));
		int 	iSlow17 = int((int((float(iConst0) / fSlow2)) & 4095));
		float 	fSlow18 = getValueLoopFiltera2(iSlow3);
		float 	fSlow19 = getValueLoopFiltera1(iSlow3);
		float 	fSlow20 = getValueLoopFilterb2(iSlow3);
		float 	fSlow21 = getValueLoopFilterb1(iSlow3);
		float 	fSlow22 = getValueLoopFilterb0(iSlow3);
		float 	fSlow23 = float(fslider2);
		float 	fSlow24 = (1.0f - fSlow23);
		int 	iSlow25 = int((int((fConst4 * (float(fslider3) / fSlow2))) & 4095));
		FAUSTFLOAT* output0 = output[0];
		FAUSTFLOAT* output1 = output[1];
		for (int i=0; i<count; i++) {
			iRec2[0] = (12345 + (1103515245 * iRec2[1]));
			fRec4[0] = (1 + (fSlow0 * fRec4[1]));
			float fTemp0 = (fRec4[0] - 1);
			int iTemp1 = ((fTemp0 < 2.0f) & iSlow5);
			float fTemp2 = ((0.0301973834223185f * iTemp1) + (fSlow4 * ((fTemp0 >= 2.0f) | iSlow1)));
			fRec3[0] = ((fRec3[1] * fTemp2) + (0.15f * (iTemp1 * (1 - fTemp2))));
			float fTemp3 = fRec0[(IOTA-1)&4095];
			fRec5[0] = (fSlow6 + (0.999f * fRec5[1]));
			float fTemp4 = (fRec5[0] * (((fSlow10 * fTemp3) + (fSlow9 * (fTemp3 + fRec0[(IOTA-2)&4095]))) + (fSlow8 * faustpower<2>(fTemp3))));
			float fTemp5 = cosf(fTemp4);
			float fTemp6 = sinf(fTemp4);
			float fTemp7 = (0 - fTemp6);
			float fTemp8 = ((fRec6[1] * fTemp7) + (fTemp3 * fTemp5));
			float fTemp9 = ((fTemp7 * fRec7[1]) + (fTemp5 * fTemp8));
			float fTemp10 = ((fTemp7 * fRec8[1]) + (fTemp5 * fTemp9));
			float fTemp11 = ((fTemp7 * fRec9[1]) + (fTemp5 * fTemp10));
			float fTemp12 = ((fTemp7 * fRec10[1]) + (fTemp5 * fTemp11));
			fRec11[0] = ((fTemp7 * fRec11[1]) + (fTemp5 * fTemp12));
			fRec10[0] = ((fTemp6 * fTemp12) + (fTemp5 * fRec11[1]));
			fRec9[0] = ((fTemp6 * fTemp11) + (fTemp5 * fRec10[1]));
			fRec8[0] = ((fTemp6 * fTemp10) + (fTemp5 * fRec9[1]));
			fRec7[0] = ((fTemp6 * fTemp9) + (fTemp5 * fRec8[1]));
			fRec6[0] = ((fTemp6 * fTemp8) + (fTemp5 * fRec7[1]));
			fRec14[0] = (fSlow12 + (0.999f * fRec14[1]));
			float fTemp13 = (fRec13[1] + (fConst3 * (fSlow14 + (iSlow13 * fRec14[0]))));
			fRec13[0] = (fTemp13 - floorf(fTemp13));
			float fTemp14 = (3.141592653589793f * (fRec5[0] * ftbl0[int((65536.0f * fRec13[0]))]));
			float fTemp15 = cosf(fTemp14);
			float fTemp16 = sinf(fTemp14);
			float fTemp17 = (0 - fTemp16);
			float fTemp18 = ((fRec15[1] * fTemp17) + (fTemp3 * fTemp15));
			float fTemp19 = ((fTemp17 * fRec16[1]) + (fTemp15 * fTemp18));
			float fTemp20 = ((fTemp17 * fRec17[1]) + (fTemp15 * fTemp19));
			float fTemp21 = ((fTemp17 * fRec18[1]) + (fTemp15 * fTemp20));
			float fTemp22 = ((fTemp17 * fRec19[1]) + (fTemp15 * fTemp21));
			fRec20[0] = ((fTemp17 * fRec20[1]) + (fTemp15 * fTemp22));
			fRec19[0] = ((fTemp16 * fTemp22) + (fTemp15 * fRec20[1]));
			fRec18[0] = ((fTemp16 * fTemp21) + (fTemp15 * fRec19[1]));
			fRec17[0] = ((fTemp16 * fTemp20) + (fTemp15 * fRec18[1]));
			fRec16[0] = ((fTemp16 * fTemp19) + (fTemp15 * fRec17[1]));
			fRec15[0] = ((fTemp16 * fTemp18) + (fTemp15 * fRec16[1]));
			fRec21[0] = ((0.999f * fRec21[1]) + fSlow16);
			fVec0[IOTA&4095] = ((fRec21[0] * ((iSlow15 * ((fTemp3 * fTemp16) + (fRec15[1] * fTemp15))) + (iSlow11 * ((fRec5[0] * ((fTemp3 * fTemp6) + (fRec6[1] * fTemp5))) + ((1 - fRec5[0]) * fTemp3))))) + (4.656612875245797e-10f * (fRec3[0] * iRec2[0])));
			fRec1[0] = (0 - (((fSlow19 * fRec1[1]) + (fSlow18 * fRec1[2])) - fVec0[(IOTA-iSlow17)&4095]));
			fRec0[IOTA&4095] = (((fSlow22 * fRec1[0]) + (fSlow21 * fRec1[1])) + (fSlow20 * fRec1[2]));
			output0[i] = (FAUSTFLOAT)(fSlow24 * fRec0[(IOTA-0)&4095]);
			output1[i] = (FAUSTFLOAT)(fSlow23 * fRec0[(IOTA-iSlow25)&4095]);
			// post processing
			fRec1[2] = fRec1[1]; fRec1[1] = fRec1[0];
			IOTA = IOTA+1;
			fRec21[1] = fRec21[0];
			fRec15[1] = fRec15[0];
			fRec16[1] = fRec16[0];
			fRec17[1] = fRec17[0];
			fRec18[1] = fRec18[0];
			fRec19[1] = fRec19[0];
			fRec20[1] = fRec20[0];
			fRec13[1] = fRec13[0];
			fRec14[1] = fRec14[0];
			fRec6[1] = fRec6[0];
			fRec7[1] = fRec7[0];
			fRec8[1] = fRec8[0];
			fRec9[1] = fRec9[0];
			fRec10[1] = fRec10[0];
			fRec11[1] = fRec11[0];
			fRec5[1] = fRec5[0];
			fRec3[1] = fRec3[0];
			fRec4[1] = fRec4[0];
			iRec2[1] = iRec2[0];
		}
	}
};


float 	Harpsi_dsp::ftbl0[65536];


#include "Faust_plugins_template2.cpp"

