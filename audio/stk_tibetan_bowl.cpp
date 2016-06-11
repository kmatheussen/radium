//-----------------------------------------------------
// name: "Tibetan Bowl"
// author: "Romain Michon"
// copyright: "Romain Michon (rmichon@ccrma.stanford.edu)"
// version: "1.0"
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
#define FAUSTCLASS Tibetan_Bowl_dsp
#endif

class Tibetan_Bowl_dsp : public dsp {
  private:
	class SIG0 {
	  private:
		int 	fSamplingFreq;
		int 	iRec43[2];
	  public:
		int getNumInputs() 	{ return 0; }
		int getNumOutputs() 	{ return 1; }
		void init(int samplingFreq) {
			fSamplingFreq = samplingFreq;
			for (int i=0; i<2; i++) iRec43[i] = 0;
		}
		void fill (int count, float output[]) {
			for (int i=0; i<count; i++) {
				iRec43[0] = (1 + iRec43[1]);
				output[i] = sinf((9.587379924285257e-05f * float((iRec43[0] - 1))));
				// post processing
				iRec43[1] = iRec43[0];
			}
		}
	};


	FAUSTFLOAT 	fentry0;
	FAUSTFLOAT 	fbutton0;
	int 	iRec14[2];
	int 	iConst0;
	float 	fConst1;
	float 	fConst2;
	float 	fConst3;
	float 	fRec15[2];
	FAUSTFLOAT 	fentry1;
	FAUSTFLOAT 	fslider0;
	FAUSTFLOAT 	fslider1;
	FAUSTFLOAT 	fslider2;
	int 	IOTA;
	float 	fVec0[4096];
	FAUSTFLOAT 	fentry2;
	float 	fConst4;
	float 	fConst5;
	float 	fConst6;
	float 	fConst7;
	float 	fConst8;
	float 	fRec13[3];
	float 	fConst9;
	float 	fConst10;
	float 	fConst11;
	float 	fRec12[2];
	float 	fRec0[2];
	float 	fVec1[4096];
	float 	fConst12;
	float 	fConst13;
	float 	fRec17[3];
	float 	fRec16[2];
	float 	fRec1[2];
	float 	fVec2[4096];
	float 	fConst14;
	float 	fConst15;
	float 	fRec19[3];
	float 	fRec18[2];
	float 	fRec2[2];
	float 	fVec3[4096];
	float 	fConst16;
	float 	fConst17;
	float 	fRec21[3];
	float 	fRec20[2];
	float 	fRec3[2];
	float 	fVec4[2048];
	float 	fConst18;
	float 	fConst19;
	float 	fRec23[3];
	float 	fRec22[2];
	float 	fRec4[2];
	float 	fRec5[2];
	float 	fVec5[2048];
	float 	fConst20;
	float 	fConst21;
	float 	fRec25[3];
	float 	fRec24[2];
	float 	fRec6[2];
	float 	fVec6[2048];
	float 	fConst22;
	float 	fConst23;
	float 	fRec27[3];
	float 	fRec26[2];
	float 	fRec7[2];
	float 	fVec7[1024];
	float 	fConst24;
	float 	fConst25;
	float 	fRec29[3];
	float 	fRec28[2];
	float 	fRec8[2];
	float 	fVec8[1024];
	float 	fConst26;
	float 	fConst27;
	float 	fRec31[3];
	float 	fRec30[2];
	float 	fRec9[2];
	float 	fVec9[1024];
	float 	fConst28;
	float 	fConst29;
	float 	fRec33[3];
	float 	fRec32[2];
	float 	fRec10[2];
	float 	fVec10[512];
	float 	fConst30;
	float 	fConst31;
	float 	fRec35[3];
	float 	fRec34[2];
	float 	fRec11[2];
	float 	fVec11[2];
	FAUSTFLOAT 	fslider3;
	float 	fRec36[2];
	FAUSTFLOAT 	fentry3;
	float 	fRec42[2];
	float 	fRec41[2];
	float 	fRec40[2];
	float 	fRec39[2];
	float 	fRec38[2];
	float 	fRec37[2];
	static float 	ftbl0[65536];
	FAUSTFLOAT 	fslider4;
	float 	fRec45[2];
	float 	fConst32;
	float 	fRec44[2];
	float 	fRec51[2];
	float 	fRec50[2];
	float 	fRec49[2];
	float 	fRec48[2];
	float 	fRec47[2];
	float 	fRec46[2];
	int 	iRec52[2];
	FAUSTFLOAT 	fslider5;
	FAUSTFLOAT 	fslider6;
	float 	fRec53[2];
	float 	fVec12[4096];
	FAUSTFLOAT 	fslider7;
	FAUSTFLOAT 	fslider8;
	float 	fConst33;
  public:
	static void metadata(Meta* m) 	{ 
		m->declare("name", "Tibetan Bowl");
		m->declare("description", "Banded Waveguide Modeld Tibetan Bowl");
		m->declare("author", "Romain Michon");
		m->declare("copyright", "Romain Michon (rmichon@ccrma.stanford.edu)");
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
		fentry0 = 0.0f;
		fbutton0 = 0.0;
		for (int i=0; i<2; i++) iRec14[i] = 0;
		iConst0 = min(192000, max(1, fSamplingFreq));
		fConst1 = (1 - (1.0f / powf(9e+04f,(1e+02f / float(iConst0)))));
		fConst2 = (1 - powf(9e+01f,(2e+02f / float(iConst0))));
		fConst3 = (5e+01f / float(iConst0));
		for (int i=0; i<2; i++) fRec15[i] = 0;
		fentry1 = 0.8f;
		fslider0 = 1.0f;
		fslider1 = 0.0f;
		fslider2 = 0.2f;
		IOTA = 0;
		for (int i=0; i<4096; i++) fVec0[i] = 0;
		fentry2 = 4.4e+02f;
		fConst4 = (1.0039068601557664f * iConst0);
		fConst5 = (1 - (100.53096491487338f / float(iConst0)));
		fConst6 = faustpower<2>(fConst5);
		fConst7 = (6.258733311379789f / float(iConst0));
		fConst8 = (0 - (2 * fConst5));
		for (int i=0; i<3; i++) fRec13[i] = 0;
		fConst9 = (0.5f * fConst6);
		fConst10 = (fConst9 - 0.5f);
		fConst11 = (0.5f - fConst9);
		for (int i=0; i<2; i++) fRec12[i] = 0;
		for (int i=0; i<2; i++) fRec0[i] = 0;
		for (int i=0; i<4096; i++) fVec1[i] = 0;
		fConst12 = (0.9961234300773741f * iConst0);
		fConst13 = (6.30763730423602f / float(iConst0));
		for (int i=0; i<3; i++) fRec17[i] = 0;
		for (int i=0; i<2; i++) fRec16[i] = 0;
		for (int i=0; i<2; i++) fRec1[i] = 0;
		for (int i=0; i<4096; i++) fVec2[i] = 0;
		fConst14 = (0.335663058736336f * iConst0);
		fConst15 = (18.718727437072666f / float(iConst0));
		for (int i=0; i<3; i++) fRec19[i] = 0;
		for (int i=0; i<2; i++) fRec18[i] = 0;
		for (int i=0; i<2; i++) fRec2[i] = 0;
		for (int i=0; i<4096; i++) fVec3[i] = 0;
		fConst16 = (0.3340797041411521f * iConst0);
		fConst17 = (18.80744394015889f / float(iConst0));
		for (int i=0; i<3; i++) fRec21[i] = 0;
		for (int i=0; i<2; i++) fRec20[i] = 0;
		for (int i=0; i<2; i++) fRec3[i] = 0;
		for (int i=0; i<2048; i++) fVec4[i] = 0;
		fConst18 = (0.1753016766553562f * iConst0);
		fConst19 = (35.8421289919112f / float(iConst0));
		for (int i=0; i<3; i++) fRec23[i] = 0;
		for (int i=0; i<2; i++) fRec22[i] = 0;
		for (int i=0; i<2; i++) fRec4[i] = 0;
		for (int i=0; i<2; i++) fRec5[i] = 0;
		for (int i=0; i<2048; i++) fVec5[i] = 0;
		fConst20 = (0.11113333777866684f * iConst0);
		fConst21 = (56.537358031063356f / float(iConst0));
		for (int i=0; i<3; i++) fRec25[i] = 0;
		for (int i=0; i<2; i++) fRec24[i] = 0;
		for (int i=0; i<2; i++) fRec6[i] = 0;
		for (int i=0; i<2048; i++) fVec6[i] = 0;
		fConst22 = (0.11092011579181602f * iConst0);
		fConst23 = (56.64603992094982f / float(iConst0));
		for (int i=0; i<3; i++) fRec27[i] = 0;
		for (int i=0; i<2; i++) fRec26[i] = 0;
		for (int i=0; i<2; i++) fRec7[i] = 0;
		for (int i=0; i<1024; i++) fVec7[i] = 0;
		fConst24 = (0.07792391976018134f * iConst0);
		fConst25 = (80.63230554259485f / float(iConst0));
		for (int i=0; i<3; i++) fRec29[i] = 0;
		for (int i=0; i<2; i++) fRec28[i] = 0;
		for (int i=0; i<2; i++) fRec8[i] = 0;
		for (int i=0; i<1024; i++) fVec8[i] = 0;
		fConst26 = (0.07807996981740686f * iConst0);
		fConst27 = (80.4711544058363f / float(iConst0));
		for (int i=0; i<3; i++) fRec31[i] = 0;
		for (int i=0; i<2; i++) fRec30[i] = 0;
		for (int i=0; i<2; i++) fRec9[i] = 0;
		for (int i=0; i<1024; i++) fVec9[i] = 0;
		fConst28 = (0.05786761797481404f * iConst0);
		fConst29 = (108.57860625806721f / float(iConst0));
		for (int i=0; i<3; i++) fRec33[i] = 0;
		for (int i=0; i<2; i++) fRec32[i] = 0;
		for (int i=0; i<2; i++) fRec10[i] = 0;
		for (int i=0; i<512; i++) fVec10[i] = 0;
		fConst30 = (0.04550412965560287f * iConst0);
		fConst31 = (138.07945245264008f / float(iConst0));
		for (int i=0; i<3; i++) fRec35[i] = 0;
		for (int i=0; i<2; i++) fRec34[i] = 0;
		for (int i=0; i<2; i++) fRec11[i] = 0;
		for (int i=0; i<2; i++) fVec11[i] = 0;
		fslider3 = 0.0f;
		for (int i=0; i<2; i++) fRec36[i] = 0;
		fentry3 = 0.0f;
		for (int i=0; i<2; i++) fRec42[i] = 0;
		for (int i=0; i<2; i++) fRec41[i] = 0;
		for (int i=0; i<2; i++) fRec40[i] = 0;
		for (int i=0; i<2; i++) fRec39[i] = 0;
		for (int i=0; i<2; i++) fRec38[i] = 0;
		for (int i=0; i<2; i++) fRec37[i] = 0;
		fslider4 = 2.2e+02f;
		for (int i=0; i<2; i++) fRec45[i] = 0;
		fConst32 = (1.0f / float(iConst0));
		for (int i=0; i<2; i++) fRec44[i] = 0;
		for (int i=0; i<2; i++) fRec51[i] = 0;
		for (int i=0; i<2; i++) fRec50[i] = 0;
		for (int i=0; i<2; i++) fRec49[i] = 0;
		for (int i=0; i<2; i++) fRec48[i] = 0;
		for (int i=0; i<2; i++) fRec47[i] = 0;
		for (int i=0; i<2; i++) fRec46[i] = 0;
		for (int i=0; i<2; i++) iRec52[i] = 0;
		fslider5 = 0.1f;
		fslider6 = 0.02f;
		for (int i=0; i<2; i++) fRec53[i] = 0;
		for (int i=0; i<4096; i++) fVec12[i] = 0;
		fslider7 = 0.6f;
		fslider8 = 0.5f;
		fConst33 = (0.5f * iConst0);
	}
	virtual void init(int samplingFreq) {
		classInit(samplingFreq);
		instanceInit(samplingFreq);
	}
	virtual void buildUserInterface(UI* faust_interface) {
		faust_interface->openVerticalBox("0x00");
		faust_interface->openHorizontalBox("Basic_Parameters");
		faust_interface->declare(&fentry2, "1", "");
		faust_interface->declare(&fentry2, "tooltip", "Tone frequency");
		faust_interface->declare(&fentry2, "unit", "Hz");
		faust_interface->addNumEntry("freq", &fentry2, 4.4e+02f, 2e+01f, 2e+04f, 1.0f);
		faust_interface->declare(&fentry1, "1", "");
		faust_interface->declare(&fentry1, "tooltip", "Gain (value between 0 and 1)");
		faust_interface->addNumEntry("gain", &fentry1, 0.8f, 0.0f, 1.0f, 0.01f);
		faust_interface->declare(&fbutton0, "1", "");
		faust_interface->declare(&fbutton0, "tooltip", "noteOn = 1, noteOff = 0");
		faust_interface->addButton("gate", &fbutton0);
		faust_interface->closeBox();
		faust_interface->openHorizontalBox("Envelopes_and_Vibrato");
		faust_interface->openVerticalBox("Global_Envelope_Parameters");
		faust_interface->declare(&fslider6, "4", "");
		faust_interface->declare(&fslider6, "tooltip", "Global envelope attack duration");
		faust_interface->declare(&fslider6, "unit", "s");
		faust_interface->addHorizontalSlider("Glob_Env_Attack", &fslider6, 0.02f, 0.0f, 2.0f, 0.01f);
		faust_interface->declare(&fslider5, "4", "");
		faust_interface->declare(&fslider5, "tooltip", "Global envelope release duration");
		faust_interface->declare(&fslider5, "unit", "s");
		faust_interface->addHorizontalSlider("Glob_Env_Release", &fslider5, 0.1f, 0.0f, 2.0f, 0.01f);
		faust_interface->closeBox();
		faust_interface->closeBox();
		faust_interface->openHorizontalBox("Physical_and_Nonlinearity");
		faust_interface->openVerticalBox("Nonlinear_Filter_Parameters");
		faust_interface->declare(&fslider4, "3", "");
		faust_interface->declare(&fslider4, "tooltip", "Frequency of the sine wave for the modulation of theta (works if Modulation Type=3)");
		faust_interface->declare(&fslider4, "unit", "Hz");
		faust_interface->addHorizontalSlider("Modulation_Frequency", &fslider4, 2.2e+02f, 2e+01f, 1e+03f, 0.1f);
		faust_interface->declare(&fentry3, "3", "");
		faust_interface->declare(&fentry3, "tooltip", "0=theta is modulated by the incoming signal; 1=theta is modulated by the averaged incoming signal; 2=theta is modulated by the squared incoming signal; 3=theta is modulated by a sine wave of frequency freqMod; 4=theta is modulated by a sine wave of frequency freq;");
		faust_interface->addNumEntry("Modulation_Type", &fentry3, 0.0f, 0.0f, 4.0f, 1.0f);
		faust_interface->declare(&fslider3, "3", "");
		faust_interface->declare(&fslider3, "tooltip", "Nonlinearity factor (value between 0 and 1)");
		faust_interface->addHorizontalSlider("Nonlinearity", &fslider3, 0.0f, 0.0f, 1.0f, 0.01f);
		faust_interface->closeBox();
		faust_interface->openVerticalBox("Physical_Parameters");
		faust_interface->declare(&fslider0, "2", "");
		faust_interface->declare(&fslider0, "tooltip", "A value between 0 and 1");
		faust_interface->addHorizontalSlider("Base_Gain", &fslider0, 1.0f, 0.0f, 1.0f, 0.01f);
		faust_interface->declare(&fslider2, "2", "");
		faust_interface->declare(&fslider2, "tooltip", "Bow pressure on the instrument (Value between 0 and 1)");
		faust_interface->addHorizontalSlider("Bow_Pressure", &fslider2, 0.2f, 0.0f, 1.0f, 0.01f);
		faust_interface->declare(&fentry0, "2", "");
		faust_interface->declare(&fentry0, "tooltip", "0=Bow; 1=Strike");
		faust_interface->addNumEntry("Excitation_Selector", &fentry0, 0.0f, 0.0f, 1.0f, 1.0f);
		faust_interface->declare(&fslider1, "2", "");
		faust_interface->declare(&fslider1, "tooltip", "A value between 0 and 1");
		faust_interface->addHorizontalSlider("Integration_Constant", &fslider1, 0.0f, 0.0f, 1.0f, 0.01f);
		faust_interface->closeBox();
		faust_interface->closeBox();
		faust_interface->openVerticalBox("Spat");
		faust_interface->addHorizontalSlider("pan angle", &fslider7, 0.6f, 0.0f, 1.0f, 0.01f);
		faust_interface->addHorizontalSlider("spatial width", &fslider8, 0.5f, 0.0f, 1.0f, 0.01f);
		faust_interface->closeBox();
		faust_interface->closeBox();
	}
	virtual void compute (int count, FAUSTFLOAT** input, FAUSTFLOAT** output) {
		float 	fSlow0 = float(fentry0);
		float 	fSlow1 = (1.1900357f * fSlow0);
		float 	fSlow2 = float(fbutton0);
		int 	iSlow3 = (fSlow2 > 0);
		int 	iSlow4 = (fSlow2 <= 0);
		float 	fSlow5 = (0.03f + (0.1f * float(fentry1)));
		float 	fSlow6 = (0.8999999999999999f + (0.1f * float(fslider0)));
		float 	fSlow7 = float(fslider1);
		float 	fSlow8 = (10 - (9 * float(fslider2)));
		float 	fSlow9 = (0.08333333333333333f * (0 - (fSlow0 - 1)));
		float 	fSlow10 = float(fentry2);
		int 	iSlow11 = int((int((fConst4 / fSlow10)) & 4095));
		float 	fSlow12 = (fConst8 * cosf((fConst7 * fSlow10)));
		int 	iSlow13 = int((int((fConst12 / fSlow10)) & 4095));
		float 	fSlow14 = (fConst8 * cosf((fConst13 * fSlow10)));
		float 	fSlow15 = (1.0914886f * fSlow0);
		int 	iSlow16 = int((int((fConst14 / fSlow10)) & 4095));
		float 	fSlow17 = (fConst8 * cosf((fConst15 * fSlow10)));
		int 	iSlow18 = int((int((fConst16 / fSlow10)) & 4095));
		float 	fSlow19 = (fConst8 * cosf((fConst17 * fSlow10)));
		float 	fSlow20 = (4.2995041f * fSlow0);
		int 	iSlow21 = int((int((fConst18 / fSlow10)) & 4095));
		float 	fSlow22 = (fConst8 * cosf((fConst19 * fSlow10)));
		float 	fSlow23 = (4.0063034f * fSlow0);
		int 	iSlow24 = int((int((fConst20 / fSlow10)) & 4095));
		float 	fSlow25 = (fConst8 * cosf((fConst21 * fSlow10)));
		int 	iSlow26 = int((int((fConst22 / fSlow10)) & 4095));
		float 	fSlow27 = (fConst8 * cosf((fConst23 * fSlow10)));
		float 	fSlow28 = (0.7063034f * fSlow0);
		int 	iSlow29 = int((int((fConst24 / fSlow10)) & 4095));
		float 	fSlow30 = (fConst8 * cosf((fConst25 * fSlow10)));
		int 	iSlow31 = int((int((fConst26 / fSlow10)) & 4095));
		float 	fSlow32 = (fConst8 * cosf((fConst27 * fSlow10)));
		float 	fSlow33 = (5.7063034f * fSlow0);
		int 	iSlow34 = int((int((fConst28 / fSlow10)) & 4095));
		float 	fSlow35 = (fConst8 * cosf((fConst29 * fSlow10)));
		int 	iSlow36 = int((int((fConst30 / fSlow10)) & 4095));
		float 	fSlow37 = (fConst8 * cosf((fConst31 * fSlow10)));
		float 	fSlow38 = (0.0010000000000000009f * float(fslider3));
		float 	fSlow39 = float(fentry3);
		float 	fSlow40 = (3.141592653589793f * (fSlow39 == 2));
		float 	fSlow41 = (1.5707963267948966f * (fSlow39 == 1));
		float 	fSlow42 = (3.141592653589793f * (fSlow39 == 0));
		int 	iSlow43 = (fSlow39 < 3);
		float 	fSlow44 = (0.0010000000000000009f * float(fslider4));
		int 	iSlow45 = (fSlow39 != 4);
		float 	fSlow46 = (fSlow10 * (fSlow39 == 4));
		int 	iSlow47 = (fSlow39 >= 3);
		float 	fSlow48 = float(fslider5);
		float 	fSlow49 = (1 - (1.0f / powf(1e+05f,(1.0f / ((fSlow48 == 0.0f) + (iConst0 * fSlow48))))));
		float 	fSlow50 = float(fslider6);
		float 	fSlow51 = (1.0f / ((fSlow50 == 0.0f) + (iConst0 * fSlow50)));
		float 	fSlow52 = float(fslider7);
		float 	fSlow53 = (0.5f * (1.0f - fSlow52));
		int 	iSlow54 = int((int((fConst33 * (float(fslider8) / fSlow10))) & 4095));
		float 	fSlow55 = (0.5f * fSlow52);
		FAUSTFLOAT* output0 = output[0];
		FAUSTFLOAT* output1 = output[1];
		for (int i=0; i<count; i++) {
			iRec14[0] = (iSlow3 & (iRec14[1] | (fRec15[1] >= 1)));
			int iTemp0 = (iSlow4 & (fRec15[1] > 0));
			fRec15[0] = (((fConst3 * (((iRec14[1] == 0) & iSlow3) & (fRec15[1] < 1))) + (fRec15[1] * ((1 - (fConst2 * (iRec14[1] & (fRec15[1] > 90)))) - (fConst1 * iTemp0)))) * ((iTemp0 == 0) | (fRec15[1] >= 1e-06f)));
			float fTemp1 = (0 - ((fSlow7 + (fSlow6 * ((((((fRec0[1] + fRec2[1]) + fRec4[1]) + fRec6[1]) + fRec8[1]) + fRec10[1]) + (((((fRec1[1] + fRec3[1]) + fRec5[1]) + fRec7[1]) + fRec9[1]) + fRec11[1])))) - (fSlow5 * fRec15[0])));
			float fTemp2 = faustpower<4>((0.75f + fabsf((fSlow8 * fTemp1))));
			float fTemp3 = (1.0f / fTemp2);
			float fTemp4 = (fSlow9 * (fTemp1 * ((fTemp3 > 1) + (float((fTemp3 <= 1)) / fTemp2))));
			fVec0[IOTA&4095] = ((fRec12[1] + fTemp4) + fSlow1);
			fRec13[0] = (0 - (((fSlow12 * fRec13[1]) + (fConst6 * fRec13[2])) - (0.999925960128219f * fVec0[(IOTA-iSlow11)&4095])));
			fRec12[0] = ((fConst11 * fRec13[0]) + (fConst10 * fRec13[2]));
			fRec0[0] = fRec12[0];
			fVec1[IOTA&4095] = (fSlow1 + (fTemp4 + fRec16[1]));
			fRec17[0] = (0 - (((fSlow14 * fRec17[1]) + (fConst6 * fRec17[2])) - (0.999925960128219f * fVec1[(IOTA-iSlow13)&4095])));
			fRec16[0] = ((fConst11 * fRec17[0]) + (fConst10 * fRec17[2]));
			fRec1[0] = fRec16[0];
			fVec2[IOTA&4095] = ((fTemp4 + fRec18[1]) + fSlow15);
			fRec19[0] = (0 - (((fSlow17 * fRec19[1]) + (fConst6 * fRec19[2])) - (0.999982774366897f * fVec2[(IOTA-iSlow16)&4095])));
			fRec18[0] = ((fConst11 * fRec19[0]) + (fConst10 * fRec19[2]));
			fRec2[0] = fRec18[0];
			fVec3[IOTA&4095] = (fSlow15 + (fTemp4 + fRec20[1]));
			fRec21[0] = (0 - (((fSlow19 * fRec21[1]) + (fConst6 * fRec21[2])) - (0.999982774366897f * fVec3[(IOTA-iSlow18)&4095])));
			fRec20[0] = ((fConst11 * fRec21[0]) + (fConst10 * fRec21[2]));
			fRec3[0] = fRec20[0];
			fVec4[IOTA&2047] = ((fTemp4 + fRec22[1]) + fSlow20);
			fRec23[0] = (0 - (((fSlow22 * fRec23[1]) + (fConst6 * fRec23[2])) - fVec4[(IOTA-iSlow21)&2047]));
			fRec22[0] = ((fConst11 * fRec23[0]) + (fConst10 * fRec23[2]));
			fRec4[0] = fRec22[0];
			fRec5[0] = fRec22[0];
			fVec5[IOTA&2047] = ((fTemp4 + fRec24[1]) + fSlow23);
			fRec25[0] = (0 - (((fSlow25 * fRec25[1]) + (fConst6 * fRec25[2])) - fVec5[(IOTA-iSlow24)&2047]));
			fRec24[0] = ((fConst11 * fRec25[0]) + (fConst10 * fRec25[2]));
			fRec6[0] = fRec24[0];
			fVec6[IOTA&2047] = (fSlow23 + (fTemp4 + fRec26[1]));
			fRec27[0] = (0 - (((fSlow27 * fRec27[1]) + (fConst6 * fRec27[2])) - fVec6[(IOTA-iSlow26)&2047]));
			fRec26[0] = ((fConst11 * fRec27[0]) + (fConst10 * fRec27[2]));
			fRec7[0] = fRec26[0];
			fVec7[IOTA&1023] = ((fTemp4 + fRec28[1]) + fSlow28);
			fRec29[0] = (0 - (((fSlow30 * fRec29[1]) + (fConst6 * fRec29[2])) - (0.999965497558225f * fVec7[(IOTA-iSlow29)&1023])));
			fRec28[0] = ((fConst11 * fRec29[0]) + (fConst10 * fRec29[2]));
			fRec8[0] = fRec28[0];
			fVec8[IOTA&1023] = (fSlow28 + (fTemp4 + fRec30[1]));
			fRec31[0] = (0 - (((fSlow32 * fRec31[1]) + (fConst6 * fRec31[2])) - (0.999965497558225f * fVec8[(IOTA-iSlow31)&1023])));
			fRec30[0] = ((fConst11 * fRec31[0]) + (fConst10 * fRec31[2]));
			fRec9[0] = fRec30[0];
			fVec9[IOTA&1023] = ((fTemp4 + fRec32[1]) + fSlow33);
			fRec33[0] = (0 - (((fSlow35 * fRec33[1]) + (fConst6 * fRec33[2])) - fVec9[(IOTA-iSlow34)&1023]));
			fRec32[0] = ((fConst11 * fRec33[0]) + (fConst10 * fRec33[2]));
			fRec10[0] = fRec32[0];
			fVec10[IOTA&511] = (fSlow33 + (fTemp4 + fRec34[1]));
			fRec35[0] = (0 - (((fSlow37 * fRec35[1]) + (fConst6 * fRec35[2])) - fVec10[(IOTA-iSlow36)&511]));
			fRec34[0] = ((fConst11 * fRec35[0]) + (fConst10 * fRec35[2]));
			fRec11[0] = fRec34[0];
			float fTemp5 = (fRec11[0] + (fRec9[0] + (fRec7[0] + (fRec5[0] + (fRec3[0] + ((((((fRec0[0] + fRec2[0]) + fRec4[0]) + fRec6[0]) + fRec8[0]) + fRec10[0]) + fRec1[0]))))));
			fVec11[0] = fTemp5;
			fRec36[0] = (fSlow38 + (0.999f * fRec36[1]));
			float fTemp6 = (fRec36[0] * (((fSlow42 * fVec11[0]) + (fSlow41 * (fVec11[0] + fVec11[1]))) + (fSlow40 * faustpower<2>(fVec11[0]))));
			float fTemp7 = cosf(fTemp6);
			float fTemp8 = sinf(fTemp6);
			float fTemp9 = (0 - fTemp8);
			float fTemp10 = ((fRec37[1] * fTemp9) + (fVec11[0] * fTemp7));
			float fTemp11 = ((fTemp9 * fRec38[1]) + (fTemp7 * fTemp10));
			float fTemp12 = ((fTemp9 * fRec39[1]) + (fTemp7 * fTemp11));
			float fTemp13 = ((fTemp9 * fRec40[1]) + (fTemp7 * fTemp12));
			float fTemp14 = ((fTemp9 * fRec41[1]) + (fTemp7 * fTemp13));
			fRec42[0] = ((fTemp9 * fRec42[1]) + (fTemp7 * fTemp14));
			fRec41[0] = ((fTemp8 * fTemp14) + (fTemp7 * fRec42[1]));
			fRec40[0] = ((fTemp8 * fTemp13) + (fTemp7 * fRec41[1]));
			fRec39[0] = ((fTemp8 * fTemp12) + (fTemp7 * fRec40[1]));
			fRec38[0] = ((fTemp8 * fTemp11) + (fTemp7 * fRec39[1]));
			fRec37[0] = ((fTemp8 * fTemp10) + (fTemp7 * fRec38[1]));
			fRec45[0] = (fSlow44 + (0.999f * fRec45[1]));
			float fTemp15 = (fRec44[1] + (fConst32 * (fSlow46 + (iSlow45 * fRec45[0]))));
			fRec44[0] = (fTemp15 - floorf(fTemp15));
			float fTemp16 = (3.141592653589793f * (fRec36[0] * ftbl0[int((65536.0f * fRec44[0]))]));
			float fTemp17 = cosf(fTemp16);
			float fTemp18 = sinf(fTemp16);
			float fTemp19 = (0 - fTemp18);
			float fTemp20 = ((fRec46[1] * fTemp19) + (fVec11[0] * fTemp17));
			float fTemp21 = ((fTemp19 * fRec47[1]) + (fTemp17 * fTemp20));
			float fTemp22 = ((fTemp19 * fRec48[1]) + (fTemp17 * fTemp21));
			float fTemp23 = ((fTemp19 * fRec49[1]) + (fTemp17 * fTemp22));
			float fTemp24 = ((fTemp19 * fRec50[1]) + (fTemp17 * fTemp23));
			fRec51[0] = ((fTemp19 * fRec51[1]) + (fTemp17 * fTemp24));
			fRec50[0] = ((fTemp18 * fTemp24) + (fTemp17 * fRec51[1]));
			fRec49[0] = ((fTemp18 * fTemp23) + (fTemp17 * fRec50[1]));
			fRec48[0] = ((fTemp18 * fTemp22) + (fTemp17 * fRec49[1]));
			fRec47[0] = ((fTemp18 * fTemp21) + (fTemp17 * fRec48[1]));
			fRec46[0] = ((fTemp18 * fTemp20) + (fTemp17 * fRec47[1]));
			iRec52[0] = (iSlow3 & (iRec52[1] | (fRec53[1] >= 1)));
			int iTemp25 = (iSlow4 & (fRec53[1] > 0));
			fRec53[0] = (((fSlow51 * (((iRec52[1] == 0) & iSlow3) & (fRec53[1] < 1))) + (fRec53[1] * (1 - (fSlow49 * iTemp25)))) * ((iTemp25 == 0) | (fRec53[1] >= 1e-06f)));
			float fTemp26 = (fRec53[0] * ((iSlow47 * ((fVec11[0] * fTemp18) + (fRec46[1] * fTemp17))) + (iSlow43 * ((fRec36[0] * ((fVec11[0] * fTemp8) + (fRec37[1] * fTemp7))) + ((1 - fRec36[0]) * fVec11[0])))));
			fVec12[IOTA&4095] = fTemp26;
			output0[i] = (FAUSTFLOAT)(fSlow53 * fVec12[IOTA&4095]);
			output1[i] = (FAUSTFLOAT)(fSlow55 * fVec12[(IOTA-iSlow54)&4095]);
			// post processing
			fRec53[1] = fRec53[0];
			iRec52[1] = iRec52[0];
			fRec46[1] = fRec46[0];
			fRec47[1] = fRec47[0];
			fRec48[1] = fRec48[0];
			fRec49[1] = fRec49[0];
			fRec50[1] = fRec50[0];
			fRec51[1] = fRec51[0];
			fRec44[1] = fRec44[0];
			fRec45[1] = fRec45[0];
			fRec37[1] = fRec37[0];
			fRec38[1] = fRec38[0];
			fRec39[1] = fRec39[0];
			fRec40[1] = fRec40[0];
			fRec41[1] = fRec41[0];
			fRec42[1] = fRec42[0];
			fRec36[1] = fRec36[0];
			fVec11[1] = fVec11[0];
			fRec11[1] = fRec11[0];
			fRec34[1] = fRec34[0];
			fRec35[2] = fRec35[1]; fRec35[1] = fRec35[0];
			fRec10[1] = fRec10[0];
			fRec32[1] = fRec32[0];
			fRec33[2] = fRec33[1]; fRec33[1] = fRec33[0];
			fRec9[1] = fRec9[0];
			fRec30[1] = fRec30[0];
			fRec31[2] = fRec31[1]; fRec31[1] = fRec31[0];
			fRec8[1] = fRec8[0];
			fRec28[1] = fRec28[0];
			fRec29[2] = fRec29[1]; fRec29[1] = fRec29[0];
			fRec7[1] = fRec7[0];
			fRec26[1] = fRec26[0];
			fRec27[2] = fRec27[1]; fRec27[1] = fRec27[0];
			fRec6[1] = fRec6[0];
			fRec24[1] = fRec24[0];
			fRec25[2] = fRec25[1]; fRec25[1] = fRec25[0];
			fRec5[1] = fRec5[0];
			fRec4[1] = fRec4[0];
			fRec22[1] = fRec22[0];
			fRec23[2] = fRec23[1]; fRec23[1] = fRec23[0];
			fRec3[1] = fRec3[0];
			fRec20[1] = fRec20[0];
			fRec21[2] = fRec21[1]; fRec21[1] = fRec21[0];
			fRec2[1] = fRec2[0];
			fRec18[1] = fRec18[0];
			fRec19[2] = fRec19[1]; fRec19[1] = fRec19[0];
			fRec1[1] = fRec1[0];
			fRec16[1] = fRec16[0];
			fRec17[2] = fRec17[1]; fRec17[1] = fRec17[0];
			fRec0[1] = fRec0[0];
			fRec12[1] = fRec12[0];
			fRec13[2] = fRec13[1]; fRec13[1] = fRec13[0];
			IOTA = IOTA+1;
			fRec15[1] = fRec15[0];
			iRec14[1] = iRec14[0];
		}
	}
};


float 	Tibetan_Bowl_dsp::ftbl0[65536];


#include "Faust_plugins_template2.cpp"

