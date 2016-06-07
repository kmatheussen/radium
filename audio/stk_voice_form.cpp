//-----------------------------------------------------
// name: "Voice Formant"
// author: "Romain Michon (rmichon@ccrma.stanford.edu)"
// copyright: "Romain Michon"
// version: "1.0"
//
// Code generated with Faust 0.9.73 (http://faust.grame.fr)
//-----------------------------------------------------
/* link with : "" */
#include <math.h>
#include <phonemes.h>
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
#define FAUSTCLASS Voice_Form_dsp
#endif

class Voice_Form_dsp : public dsp {
  private:
	class SIG0 {
	  private:
		int 	fSamplingFreq;
		int 	iRec11[2];
	  public:
		int getNumInputs() 	{ return 0; }
		int getNumOutputs() 	{ return 1; }
		void init(int samplingFreq) {
			fSamplingFreq = samplingFreq;
			for (int i=0; i<2; i++) iRec11[i] = 0;
		}
		void fill (int count, float output[]) {
			for (int i=0; i<count; i++) {
				iRec11[0] = (1 + iRec11[1]);
				output[i] = sinf((9.587379924285257e-05f * float((iRec11[0] - 1))));
				// post processing
				iRec11[1] = iRec11[0];
			}
		}
	};


	FAUSTFLOAT 	fslider0;
	int 	iVec0[2];
	float 	fRec1[2];
	FAUSTFLOAT 	fbutton0;
	int 	iRec2[2];
	FAUSTFLOAT 	fslider1;
	int 	iConst0;
	FAUSTFLOAT 	fslider2;
	float 	fRec3[2];
	int 	iRec4[2];
	FAUSTFLOAT 	fentry0;
	int 	iRec6[2];
	FAUSTFLOAT 	fslider3;
	FAUSTFLOAT 	fslider4;
	float 	fRec7[2];
	float 	fRec8[2];
	float 	fConst1;
	float 	fConst2;
	float 	fConst3;
	float 	fConst4;
	float 	fConst5;
	float 	fConst6;
	float 	fConst7;
	static float 	ftbl0[65536];
	FAUSTFLOAT 	fslider5;
	float 	fConst8;
	float 	fConst9;
	float 	fRec12[2];
	int 	iRec13[2];
	int 	iRec14[2];
	FAUSTFLOAT 	fslider6;
	FAUSTFLOAT 	fslider7;
	FAUSTFLOAT 	fslider8;
	float 	fRec15[2];
	FAUSTFLOAT 	fslider9;
	FAUSTFLOAT 	fentry1;
	float 	fRec16[2];
	float 	fVec1[2];
	float 	fVec2[2];
	float 	fConst10;
	float 	fRec10[3];
	float 	fConst11;
	float 	fConst12;
	float 	fConst13;
	float 	fVec3[2];
	float 	fConst14;
	float 	fConst15;
	float 	fConst16;
	float 	fRec9[2];
	float 	fVec4[2];
	float 	fRec5[2];
	float 	fRec17[2];
	float 	fRec18[2];
	float 	fConst17;
	float 	fRec0[3];
	float 	fRec20[2];
	float 	fRec21[2];
	float 	fRec19[3];
	float 	fRec23[2];
	float 	fRec24[2];
	float 	fRec22[3];
	float 	fRec26[2];
	float 	fRec27[2];
	float 	fRec25[3];
	int 	IOTA;
	float 	fVec5[4096];
	FAUSTFLOAT 	fslider10;
	FAUSTFLOAT 	fslider11;
	float 	fConst18;
  public:
	static void metadata(Meta* m) 	{ 
		m->declare("name", "Voice Formant");
		m->declare("description", "Voice Formant Instrument");
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
		m->declare("oscillator.lib/name", "Faust Oscillator Library");
		m->declare("oscillator.lib/author", "Julius O. Smith (jos at ccrma.stanford.edu)");
		m->declare("oscillator.lib/copyright", "Julius O. Smith III");
		m->declare("oscillator.lib/version", "1.11");
		m->declare("oscillator.lib/license", "STK-4.3");
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
		fslider0 = 4.0f;
		for (int i=0; i<2; i++) iVec0[i] = 0;
		for (int i=0; i<2; i++) fRec1[i] = 0;
		fbutton0 = 0.0;
		for (int i=0; i<2; i++) iRec2[i] = 0;
		fslider1 = 0.001f;
		iConst0 = min(192000, max(1, fSamplingFreq));
		fslider2 = 0.001f;
		for (int i=0; i<2; i++) fRec3[i] = 0;
		for (int i=0; i<2; i++) iRec4[i] = 0;
		fentry0 = 1.0f;
		for (int i=0; i<2; i++) iRec6[i] = 0;
		fslider3 = 0.01f;
		fslider4 = 0.01f;
		for (int i=0; i<2; i++) fRec7[i] = 0;
		for (int i=0; i<2; i++) fRec8[i] = 0;
		fConst1 = tanf((10367.255756846318f / float(iConst0)));
		fConst2 = faustpower<2>(fConst1);
		fConst3 = (2 * (1.412270893774204f - (1.0f / fConst2)));
		fConst4 = (1.0f / fConst1);
		fConst5 = (1.412270893774204f + ((fConst4 - 0.80263676416103f) / fConst1));
		fConst6 = (1.412270893774204f + ((0.80263676416103f + fConst4) / fConst1));
		fConst7 = (1.0f / fConst6);
		fslider5 = 6.0f;
		fConst8 = float(iConst0);
		fConst9 = (1.0f / fConst8);
		for (int i=0; i<2; i++) fRec12[i] = 0;
		for (int i=0; i<2; i++) iRec13[i] = 0;
		for (int i=0; i<2; i++) iRec14[i] = 0;
		fslider6 = 0.1f;
		fslider7 = 0.05f;
		fslider8 = 0.5f;
		for (int i=0; i<2; i++) fRec15[i] = 0;
		fslider9 = 0.05f;
		fentry1 = 4.4e+02f;
		for (int i=0; i<2; i++) fRec16[i] = 0;
		for (int i=0; i<2; i++) fVec1[i] = 0;
		for (int i=0; i<2; i++) fVec2[i] = 0;
		fConst10 = (0.5f * fConst8);
		for (int i=0; i<3; i++) fRec10[i] = 0;
		fConst11 = (0.019809144837789f / fConst2);
		fConst12 = (1.161516418982696f + fConst11);
		fConst13 = (2 * (1.161516418982696f - fConst11));
		for (int i=0; i<2; i++) fVec3[i] = 0;
		fConst14 = (0.822445908998816f + fConst4);
		fConst15 = (1.0f / (fConst14 * fConst6));
		fConst16 = (0 - ((0.822445908998816f - fConst4) / fConst14));
		for (int i=0; i<2; i++) fRec9[i] = 0;
		for (int i=0; i<2; i++) fVec4[i] = 0;
		for (int i=0; i<2; i++) fRec5[i] = 0;
		for (int i=0; i<2; i++) fRec17[i] = 0;
		for (int i=0; i<2; i++) fRec18[i] = 0;
		fConst17 = (6.283185307179586f / float(iConst0));
		for (int i=0; i<3; i++) fRec0[i] = 0;
		for (int i=0; i<2; i++) fRec20[i] = 0;
		for (int i=0; i<2; i++) fRec21[i] = 0;
		for (int i=0; i<3; i++) fRec19[i] = 0;
		for (int i=0; i<2; i++) fRec23[i] = 0;
		for (int i=0; i<2; i++) fRec24[i] = 0;
		for (int i=0; i<3; i++) fRec22[i] = 0;
		for (int i=0; i<2; i++) fRec26[i] = 0;
		for (int i=0; i<2; i++) fRec27[i] = 0;
		for (int i=0; i<3; i++) fRec25[i] = 0;
		IOTA = 0;
		for (int i=0; i<4096; i++) fVec5[i] = 0;
		fslider10 = 0.6f;
		fslider11 = 0.5f;
		fConst18 = (0.5f * iConst0);
	}
	virtual void init(int samplingFreq) {
		classInit(samplingFreq);
		instanceInit(samplingFreq);
	}
	virtual void buildUserInterface(UI* faust_interface) {
		faust_interface->openVerticalBox("0x00");
		faust_interface->openHorizontalBox("Basic_Parameters");
		faust_interface->declare(&fentry1, "1", "");
		faust_interface->declare(&fentry1, "tooltip", "Tone frequency");
		faust_interface->declare(&fentry1, "unit", "Hz");
		faust_interface->addNumEntry("freq", &fentry1, 4.4e+02f, 2e+01f, 2e+04f, 1.0f);
		faust_interface->declare(&fentry0, "1", "");
		faust_interface->declare(&fentry0, "tooltip", "Gain (value between 0 and 1)");
		faust_interface->addNumEntry("gain", &fentry0, 1.0f, 0.0f, 1.0f, 0.01f);
		faust_interface->declare(&fbutton0, "1", "");
		faust_interface->declare(&fbutton0, "tooltip", "noteOn = 1, noteOff = 0");
		faust_interface->addButton("gate", &fbutton0);
		faust_interface->closeBox();
		faust_interface->openHorizontalBox("Envelopes_and_Vibrato");
		faust_interface->openVerticalBox("Envelope_Parameters");
		faust_interface->declare(&fslider2, "4", "");
		faust_interface->declare(&fslider2, "tooltip", "Noised sounds attack duration");
		faust_interface->declare(&fslider2, "unit", "s");
		faust_interface->addHorizontalSlider("Noised_Attack", &fslider2, 0.001f, 0.0f, 2.0f, 0.001f);
		faust_interface->declare(&fslider1, "4", "");
		faust_interface->declare(&fslider1, "tooltip", "Noised sounds release duration");
		faust_interface->declare(&fslider1, "unit", "s");
		faust_interface->addHorizontalSlider("Noised_Release", &fslider1, 0.001f, 0.0f, 2.0f, 0.001f);
		faust_interface->declare(&fslider4, "4", "");
		faust_interface->declare(&fslider4, "tooltip", "Voiced sounds attack duration");
		faust_interface->declare(&fslider4, "unit", "s");
		faust_interface->addHorizontalSlider("Voiced_Attack", &fslider4, 0.01f, 0.0f, 2.0f, 0.01f);
		faust_interface->declare(&fslider3, "4", "");
		faust_interface->declare(&fslider3, "tooltip", "Voiced sounds release duration");
		faust_interface->declare(&fslider3, "unit", "s");
		faust_interface->addHorizontalSlider("Voiced_Release", &fslider3, 0.01f, 0.0f, 2.0f, 0.01f);
		faust_interface->closeBox();
		faust_interface->openVerticalBox("Vibrato_Parameters");
		faust_interface->declare(&fslider8, "3", "");
		faust_interface->declare(&fslider8, "tooltip", "Vibrato attack duration");
		faust_interface->declare(&fslider8, "unit", "s");
		faust_interface->addHorizontalSlider("Vibrato_Attack", &fslider8, 0.5f, 0.0f, 2.0f, 0.01f);
		faust_interface->declare(&fslider7, "3", "");
		faust_interface->declare(&fslider7, "tooltip", "Vibrato silence duration before attack");
		faust_interface->declare(&fslider7, "unit", "s");
		faust_interface->addHorizontalSlider("Vibrato_Begin", &fslider7, 0.05f, 0.0f, 2.0f, 0.01f);
		faust_interface->declare(&fslider5, "3", "");
		faust_interface->declare(&fslider5, "unit", "Hz");
		faust_interface->addHorizontalSlider("Vibrato_Freq", &fslider5, 6.0f, 1.0f, 15.0f, 0.1f);
		faust_interface->declare(&fslider9, "3", "");
		faust_interface->declare(&fslider9, "tooltip", "A value between 0 and 1");
		faust_interface->addHorizontalSlider("Vibrato_Gain", &fslider9, 0.05f, 0.0f, 1.0f, 0.01f);
		faust_interface->declare(&fslider6, "3", "");
		faust_interface->declare(&fslider6, "tooltip", "Vibrato release duration");
		faust_interface->declare(&fslider6, "unit", "s");
		faust_interface->addHorizontalSlider("Vibrato_Release", &fslider6, 0.1f, 0.0f, 2.0f, 0.01f);
		faust_interface->closeBox();
		faust_interface->closeBox();
		faust_interface->openVerticalBox("Physical_Parameters");
		faust_interface->declare(&fslider0, "2", "");
		faust_interface->declare(&fslider0, "tooltip", "0->eee, 1->ihh, 2->ehh, 3->aaa, 4->ahh, 5->aww, 6->ohh, 7->uhh, 8->uuu, 9->ooo, 10->rrr, 11->lll, 12->mmm, 13->nnn, 14->nng, 15->ngg, 16->fff, 17->sss, 18->thh, 19->shh, 20->xxx, 21->hee, 22->hoo, 23->hah, 24->bbb, 25->ddd, 26->jjj, 27->ggg, 28->vvv, 29->zzz, 30->thz, 31->zhh");
		faust_interface->addHorizontalSlider("Phoneme", &fslider0, 4.0f, 0.0f, 31.0f, 1.0f);
		faust_interface->closeBox();
		faust_interface->openVerticalBox("Spat");
		faust_interface->addHorizontalSlider("pan angle", &fslider10, 0.6f, 0.0f, 1.0f, 0.01f);
		faust_interface->addHorizontalSlider("spatial width", &fslider11, 0.5f, 0.0f, 1.0f, 0.01f);
		faust_interface->closeBox();
		faust_interface->closeBox();
	}
	virtual void compute (int count, FAUSTFLOAT** input, FAUSTFLOAT** output) {
		float 	fSlow0 = float(fslider0);
		float 	fSlow1 = (0.0010000000000000009f * loadPhonemeGains(fSlow0, 1));
		float 	fSlow2 = float(fbutton0);
		int 	iSlow3 = (fSlow2 > 0);
		int 	iSlow4 = (fSlow2 <= 0);
		float 	fSlow5 = float(fslider1);
		float 	fSlow6 = (1 - (1.0f / powf(1e+05f,(1.0f / ((fSlow5 == 0.0f) + (iConst0 * fSlow5))))));
		float 	fSlow7 = float(fslider2);
		float 	fSlow8 = (1.0f / ((fSlow7 == 0.0f) + (iConst0 * fSlow7)));
		float 	fSlow9 = (0.2f * float(fentry0));
		float 	fSlow10 = (fSlow9 - 0.97f);
		float 	fSlow11 = float(fslider3);
		float 	fSlow12 = (1 - (1.0f / powf(1e+05f,(1.0f / ((fSlow11 == 0.0f) + (iConst0 * fSlow11))))));
		float 	fSlow13 = float(fslider4);
		float 	fSlow14 = (1.0f / ((fSlow13 == 0.0f) + (iConst0 * fSlow13)));
		float 	fSlow15 = (0.0010000000000000009f * loadPhonemeGains(fSlow0, 0));
		float 	fSlow16 = (fConst9 * float(fslider5));
		float 	fSlow17 = float(fslider6);
		float 	fSlow18 = (1 - (1.0f / powf(1e+05f,(1.0f / ((fSlow17 == 0.0f) + (iConst0 * fSlow17))))));
		float 	fSlow19 = float(fslider7);
		float 	fSlow20 = (iConst0 * fSlow19);
		float 	fSlow21 = ((fSlow19 == 0.0f) + fSlow20);
		float 	fSlow22 = float(fslider8);
		float 	fSlow23 = (1.0f / ((fSlow22 == 0.0f) + (iConst0 * fSlow22)));
		float 	fSlow24 = (100 * float(fslider9));
		float 	fSlow25 = float(fentry1);
		float 	fSlow26 = (fSlow9 + 0.030000000000000027f);
		float 	fSlow27 = (0.0010000000000000009f * powf(10,(0.05f * loadPhonemeParameters(fSlow0, 1, 2))));
		float 	fSlow28 = loadPhonemeParameters(fSlow0, 1, 1);
		float 	fSlow29 = faustpower<2>(fSlow28);
		float 	fSlow30 = (0.0010000000000000009f * loadPhonemeParameters(fSlow0, 1, 0));
		float 	fSlow31 = (0 - (2 * fSlow28));
		float 	fSlow32 = (0.5f * fSlow29);
		float 	fSlow33 = (0.5f - fSlow32);
		float 	fSlow34 = (0.0010000000000000009f * powf(10,(0.05f * loadPhonemeParameters(fSlow0, 2, 2))));
		float 	fSlow35 = loadPhonemeParameters(fSlow0, 2, 1);
		float 	fSlow36 = faustpower<2>(fSlow35);
		float 	fSlow37 = (0.0010000000000000009f * loadPhonemeParameters(fSlow0, 2, 0));
		float 	fSlow38 = (0 - (2 * fSlow35));
		float 	fSlow39 = (0.5f * fSlow36);
		float 	fSlow40 = (0.5f - fSlow39);
		float 	fSlow41 = (0.0010000000000000009f * powf(10,(0.05f * loadPhonemeParameters(fSlow0, 0, 2))));
		float 	fSlow42 = loadPhonemeParameters(fSlow0, 0, 1);
		float 	fSlow43 = faustpower<2>(fSlow42);
		float 	fSlow44 = (0.0010000000000000009f * loadPhonemeParameters(fSlow0, 0, 0));
		float 	fSlow45 = (0 - (2 * fSlow42));
		float 	fSlow46 = (0.5f * fSlow43);
		float 	fSlow47 = (fSlow46 - 0.5f);
		float 	fSlow48 = (0.5f - fSlow46);
		float 	fSlow49 = (fSlow39 - 0.5f);
		float 	fSlow50 = (fSlow32 - 0.5f);
		float 	fSlow51 = (0.0010000000000000009f * powf(10,(0.05f * loadPhonemeParameters(fSlow0, 3, 2))));
		float 	fSlow52 = loadPhonemeParameters(fSlow0, 3, 1);
		float 	fSlow53 = faustpower<2>(fSlow52);
		float 	fSlow54 = (0.0010000000000000009f * loadPhonemeParameters(fSlow0, 3, 0));
		float 	fSlow55 = (0 - (2 * fSlow52));
		float 	fSlow56 = (0.5f * fSlow53);
		float 	fSlow57 = (0.5f - fSlow56);
		float 	fSlow58 = (fSlow56 - 0.5f);
		float 	fSlow59 = float(fslider10);
		float 	fSlow60 = (1.0f - fSlow59);
		int 	iSlow61 = int((int((fConst18 * (float(fslider11) / fSlow25))) & 4095));
		FAUSTFLOAT* output0 = output[0];
		FAUSTFLOAT* output1 = output[1];
		for (int i=0; i<count; i++) {
			iVec0[0] = 1;
			fRec1[0] = ((0.999f * fRec1[1]) + fSlow1);
			iRec2[0] = (iSlow3 & (iRec2[1] | (fRec3[1] >= 1)));
			int iTemp0 = (iSlow4 & (fRec3[1] > 0));
			fRec3[0] = (((fSlow8 * (((iRec2[1] == 0) & iSlow3) & (fRec3[1] < 1))) + (fRec3[1] * (1 - (fSlow6 * iTemp0)))) * ((iTemp0 == 0) | (fRec3[1] >= 1e-06f)));
			iRec4[0] = (12345 + (1103515245 * iRec4[1]));
			iRec6[0] = (iSlow3 & (iRec6[1] | (fRec7[1] >= 1)));
			int iTemp1 = (iSlow4 & (fRec7[1] > 0));
			fRec7[0] = (((fSlow14 * (((iRec6[1] == 0) & iSlow3) & (fRec7[1] < 1))) + (fRec7[1] * (1 - (fSlow12 * iTemp1)))) * ((iTemp1 == 0) | (fRec7[1] >= 1e-06f)));
			fRec8[0] = ((0.999f * fRec8[1]) + fSlow15);
			float fTemp2 = (fSlow16 + fRec12[1]);
			fRec12[0] = (fTemp2 - floorf(fTemp2));
			iRec13[0] = (iSlow3 & (iRec13[1] | (fRec15[1] >= 1)));
			iRec14[0] = (iSlow3 * (1 + iRec14[1]));
			int iTemp3 = (iSlow4 & (fRec15[1] > 0));
			fRec15[0] = (((fSlow23 * (((((iRec13[1] == 0) & iSlow3) & (fRec15[1] < 1)) & (iRec14[1] > fSlow20)) * (1 - (iRec14[1] < fSlow21)))) + (fRec15[1] * (1 - (fSlow18 * iTemp3)))) * ((iTemp3 == 0) | (fRec15[1] >= 1e-06f)));
			float fTemp4 = float((fSlow25 + (fSlow24 * (fRec15[0] * ftbl0[int((65536.0f * fRec12[0]))]))));
			fRec16[0] = fmodf((fRec16[1] + (fConst9 * fTemp4)),1);
			float fTemp5 = faustpower<2>(((2 * fRec16[0]) - 1));
			fVec1[0] = fTemp5;
			float fTemp6 = ((iVec0[1] * (fVec1[0] - fVec1[1])) / fTemp4);
			fVec2[0] = fTemp6;
			fRec10[0] = ((1 + (fConst10 * ((0.25f * fVec2[1]) - (0.25f * fVec2[0])))) - (iVec0[1] + (fConst7 * ((fConst5 * fRec10[2]) + (fConst3 * fRec10[1])))));
			float fTemp7 = (((fConst12 * fRec10[0]) + (fConst13 * fRec10[1])) + (fConst12 * fRec10[2]));
			fVec3[0] = fTemp7;
			fRec9[0] = ((fConst16 * fRec9[1]) + (fConst15 * (fVec3[0] + fVec3[1])));
			float fTemp8 = ((fRec9[0] * fRec8[0]) * fRec7[0]);
			fVec4[0] = fTemp8;
			fRec5[0] = ((fSlow26 * ((0.47368421052631576f * fVec4[1]) + (0.5263157894736842f * fVec4[0]))) - (fSlow10 * fRec5[1]));
			float fTemp9 = (fRec5[0] + (4.656612875245797e-10f * ((iRec4[0] * fRec3[0]) * fRec1[0])));
			fRec17[0] = ((0.999f * fRec17[1]) + fSlow27);
			fRec18[0] = (fSlow30 + (0.999f * fRec18[1]));
			fRec0[0] = (0 - (((fSlow31 * (cosf((fConst17 * fRec18[0])) * fRec0[1])) + (fSlow29 * fRec0[2])) - (fRec17[0] * fTemp9)));
			fRec20[0] = ((0.999f * fRec20[1]) + fSlow34);
			fRec21[0] = (fSlow37 + (0.999f * fRec21[1]));
			fRec19[0] = (0 - (((fSlow38 * (cosf((fConst17 * fRec21[0])) * fRec19[1])) + (fSlow36 * fRec19[2])) - (fRec20[0] * fTemp9)));
			fRec23[0] = ((0.999f * fRec23[1]) + fSlow41);
			fRec24[0] = (fSlow44 + (0.999f * fRec24[1]));
			fRec22[0] = (0 - (((fSlow45 * (cosf((fConst17 * fRec24[0])) * fRec22[1])) + (fSlow43 * fRec22[2])) - (fRec23[0] * fTemp9)));
			fRec26[0] = ((0.999f * fRec26[1]) + fSlow51);
			fRec27[0] = (fSlow54 + (0.999f * fRec27[1]));
			fRec25[0] = (0 - (((fSlow55 * (cosf((fConst17 * fRec27[0])) * fRec25[1])) + (fSlow53 * fRec25[2])) - (fRec26[0] * fTemp9)));
			float fTemp10 = ((fSlow58 * fRec25[2]) + ((fSlow57 * fRec25[0]) + ((fSlow50 * fRec0[2]) + (((fSlow49 * fRec19[2]) + (((fSlow48 * fRec22[0]) + (fSlow47 * fRec22[2])) + (fSlow40 * fRec19[0]))) + (fSlow33 * fRec0[0])))));
			fVec5[IOTA&4095] = fTemp10;
			output0[i] = (FAUSTFLOAT)(fSlow60 * fVec5[IOTA&4095]);
			output1[i] = (FAUSTFLOAT)(fSlow59 * fVec5[(IOTA-iSlow61)&4095]);
			// post processing
			IOTA = IOTA+1;
			fRec25[2] = fRec25[1]; fRec25[1] = fRec25[0];
			fRec27[1] = fRec27[0];
			fRec26[1] = fRec26[0];
			fRec22[2] = fRec22[1]; fRec22[1] = fRec22[0];
			fRec24[1] = fRec24[0];
			fRec23[1] = fRec23[0];
			fRec19[2] = fRec19[1]; fRec19[1] = fRec19[0];
			fRec21[1] = fRec21[0];
			fRec20[1] = fRec20[0];
			fRec0[2] = fRec0[1]; fRec0[1] = fRec0[0];
			fRec18[1] = fRec18[0];
			fRec17[1] = fRec17[0];
			fRec5[1] = fRec5[0];
			fVec4[1] = fVec4[0];
			fRec9[1] = fRec9[0];
			fVec3[1] = fVec3[0];
			fRec10[2] = fRec10[1]; fRec10[1] = fRec10[0];
			fVec2[1] = fVec2[0];
			fVec1[1] = fVec1[0];
			fRec16[1] = fRec16[0];
			fRec15[1] = fRec15[0];
			iRec14[1] = iRec14[0];
			iRec13[1] = iRec13[0];
			fRec12[1] = fRec12[0];
			fRec8[1] = fRec8[0];
			fRec7[1] = fRec7[0];
			iRec6[1] = iRec6[0];
			iRec4[1] = iRec4[0];
			fRec3[1] = fRec3[0];
			iRec2[1] = iRec2[0];
			fRec1[1] = fRec1[0];
			iVec0[1] = iVec0[0];
		}
	}
};


float 	Voice_Form_dsp::ftbl0[65536];


#include "Faust_plugins_template2.cpp"

