/* ------------------------------------------------------------
author: "Romain Michon (rmichon@ccrma.stanford.edu)"
copyright: "Romain Michon"
name: "Voice Formant"
version: "1.0"
Code generated with Faust 2.0.a43 (http://faust.grame.fr)
------------------------------------------------------------ */

#ifndef  __Voice_Form_dsp_H__
#define  __Voice_Form_dsp_H__

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

/* link with : "" */
#include <math.h>
#include <phonemes.h>


class Voice_Form_dspSIG0 {
	
  private:
	
	int iRec10[2];
	
  public:
	
	int getNumInputsVoice_Form_dspSIG0() {
		return 0;
		
	}
	int getNumOutputsVoice_Form_dspSIG0() {
		return 1;
		
	}
	int getInputRateVoice_Form_dspSIG0(int channel) {
		int rate;
		switch (channel) {
			default: {
				rate = -1;
				break;
			}
			
		}
		return rate;
		
	}
	int getOutputRateVoice_Form_dspSIG0(int channel) {
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
	
	void instanceInitVoice_Form_dspSIG0(int samplingFreq) {
		for (int i6 = 0; (i6 < 2); i6 = (i6 + 1)) {
			iRec10[i6] = 0;
			
		}
		
	}
	
	void fillVoice_Form_dspSIG0(int count, float* output) {
		for (int i = 0; (i < count); i = (i + 1)) {
			iRec10[0] = (1 + iRec10[1]);
			output[i] = sinf((9.58738e-05f * float((iRec10[0] - 1))));
			iRec10[1] = iRec10[0];
			
		}
		
	}
};

Voice_Form_dspSIG0* newVoice_Form_dspSIG0() { return (Voice_Form_dspSIG0*)new Voice_Form_dspSIG0(); }
void deleteVoice_Form_dspSIG0(Voice_Form_dspSIG0* dsp) { delete dsp; }

static float faustpower2_f(float value) {
	return (value * value);
	
}
static float ftbl0Voice_Form_dspSIG0[65536];

#ifndef FAUSTCLASS 
#define FAUSTCLASS Voice_Form_dsp
#endif

class Voice_Form_dsp : public dsp {
	
  private:
	
	float fVec5[4096];
	float fRec5[3];
	float fRec0[3];
	float fRec19[3];
	float fRec22[3];
	float fRec25[3];
	int iVec0[2];
	float fRec1[2];
	float fRec2[2];
	int iRec7[2];
	int iRec8[2];
	float fRec9[2];
	float fRec11[2];
	float fRec6[2];
	float fVec1[2];
	float fVec2[2];
	float fVec3[2];
	float fRec4[2];
	float fRec12[2];
	int iRec13[2];
	float fRec14[2];
	float fVec4[2];
	float fRec3[2];
	int iRec15[2];
	int iRec16[2];
	float fRec17[2];
	float fRec18[2];
	float fRec20[2];
	float fRec21[2];
	float fRec23[2];
	float fRec24[2];
	float fRec26[2];
	float fRec27[2];
	FAUSTFLOAT fHslider0;
	FAUSTFLOAT fHslider1;
	int fSamplingFreq;
	float fConst0;
	float fConst1;
	FAUSTFLOAT fEntry0;
	float fConst2;
	float fConst3;
	float fConst4;
	float fConst5;
	float fConst6;
	float fConst7;
	float fConst8;
	float fConst9;
	float fConst10;
	float fConst11;
	float fConst12;
	FAUSTFLOAT fHslider2;
	FAUSTFLOAT fButton0;
	FAUSTFLOAT fHslider3;
	FAUSTFLOAT fHslider4;
	FAUSTFLOAT fHslider5;
	float fConst13;
	FAUSTFLOAT fHslider6;
	FAUSTFLOAT fEntry1;
	float fConst14;
	float fConst15;
	float fConst16;
	float fConst17;
	FAUSTFLOAT fHslider7;
	FAUSTFLOAT fHslider8;
	FAUSTFLOAT fHslider9;
	FAUSTFLOAT fHslider10;
	int IOTA;
	float fConst18;
	FAUSTFLOAT fHslider11;
	
  public:
	
	void static metadata(Meta* m) { 
		m->declare("author", "Romain Michon (rmichon@ccrma.stanford.edu)");
		m->declare("copyright", "Romain Michon");
		m->declare("description", "Voice Formant Instrument");
		m->declare("effect.lib/author", "Julius O. Smith (jos at ccrma.stanford.edu)");
		m->declare("effect.lib/copyright", "Julius O. Smith III");
		m->declare("effect.lib/exciter_author", "Priyanka Shekar (pshekar@ccrma.stanford.edu)");
		m->declare("effect.lib/exciter_copyright", "Copyright (c) 2013 Priyanka Shekar");
		m->declare("effect.lib/exciter_license", "MIT License (MIT)");
		m->declare("effect.lib/exciter_name", "Harmonic Exciter");
		m->declare("effect.lib/exciter_version", "1.0");
		m->declare("effect.lib/license", "STK-4.3");
		m->declare("effect.lib/name", "Faust Audio Effect Library");
		m->declare("effect.lib/version", "1.33");
		m->declare("filter.lib/author", "Julius O. Smith (jos at ccrma.stanford.edu)");
		m->declare("filter.lib/copyright", "Julius O. Smith III");
		m->declare("filter.lib/license", "STK-4.3");
		m->declare("filter.lib/name", "Faust Filter Library");
		m->declare("filter.lib/reference", "https://ccrma.stanford.edu/~jos/filters/");
		m->declare("filter.lib/version", "1.29");
		m->declare("instrument.lib/author", "Romain Michon (rmichon@ccrma.stanford.edu)");
		m->declare("instrument.lib/copyright", "Romain Michon");
		m->declare("instrument.lib/licence", "STK-4.3");
		m->declare("instrument.lib/name", "Faust-STK Tools Library");
		m->declare("instrument.lib/version", "1.0");
		m->declare("licence", "STK-4.3");
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
		m->declare("name", "Voice Formant");
		m->declare("oscillator.lib/author", "Julius O. Smith (jos at ccrma.stanford.edu)");
		m->declare("oscillator.lib/copyright", "Julius O. Smith III");
		m->declare("oscillator.lib/license", "STK-4.3");
		m->declare("oscillator.lib/name", "Faust Oscillator Library");
		m->declare("oscillator.lib/version", "1.11");
		m->declare("version", "1.0");
	}

	virtual int getNumInputs() {
		return 0;
		
	}
	virtual int getNumOutputs() {
		return 2;
		
	}
	virtual int getInputRate(int channel) {
		int rate;
		switch (channel) {
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
		Voice_Form_dspSIG0* sig0 = newVoice_Form_dspSIG0();
		sig0->instanceInitVoice_Form_dspSIG0(samplingFreq);
		sig0->fillVoice_Form_dspSIG0(65536, ftbl0Voice_Form_dspSIG0);
		deleteVoice_Form_dspSIG0(sig0);
		
	}
	
	virtual void instanceInit(int samplingFreq) {
		fSamplingFreq = samplingFreq;
		fHslider0 = FAUSTFLOAT(0.6f);
		fHslider1 = FAUSTFLOAT(4.0f);
		for (int i0 = 0; (i0 < 2); i0 = (i0 + 1)) {
			iVec0[i0] = 0;
			
		}
		fConst0 = min(1.92e+05f, max(1.0f, float(fSamplingFreq)));
		fConst1 = (6.2831855f / fConst0);
		for (int i1 = 0; (i1 < 2); i1 = (i1 + 1)) {
			fRec1[i1] = 0.0f;
			
		}
		for (int i2 = 0; (i2 < 2); i2 = (i2 + 1)) {
			fRec2[i2] = 0.0f;
			
		}
		fEntry0 = FAUSTFLOAT(1.0f);
		fConst2 = tanf((10367.256f / fConst0));
		fConst3 = (1.0f / fConst2);
		fConst4 = (0.8224459f + fConst3);
		fConst5 = (0.0f - ((0.8224459f - fConst3) / fConst4));
		fConst6 = (1.4122709f + ((0.80263674f + fConst3) / fConst2));
		fConst7 = (1.0f / (fConst4 * fConst6));
		fConst8 = faustpower2_f(fConst2);
		fConst9 = (0.019809145f / fConst8);
		fConst10 = (1.1615164f + fConst9);
		fConst11 = (0.5f * fConst0);
		fConst12 = (1.0f / fConst0);
		fHslider2 = FAUSTFLOAT(0.05f);
		fButton0 = FAUSTFLOAT(0.0f);
		for (int i3 = 0; (i3 < 2); i3 = (i3 + 1)) {
			iRec7[i3] = 0;
			
		}
		for (int i4 = 0; (i4 < 2); i4 = (i4 + 1)) {
			iRec8[i4] = 0;
			
		}
		fHslider3 = FAUSTFLOAT(0.5f);
		fHslider4 = FAUSTFLOAT(0.05f);
		fHslider5 = FAUSTFLOAT(0.1f);
		for (int i5 = 0; (i5 < 2); i5 = (i5 + 1)) {
			fRec9[i5] = 0.0f;
			
		}
		fConst13 = (1.0f / fConst0);
		fHslider6 = FAUSTFLOAT(6.0f);
		for (int i7 = 0; (i7 < 2); i7 = (i7 + 1)) {
			fRec11[i7] = 0.0f;
			
		}
		fEntry1 = FAUSTFLOAT(4.4e+02f);
		for (int i8 = 0; (i8 < 2); i8 = (i8 + 1)) {
			fRec6[i8] = 0.0f;
			
		}
		for (int i9 = 0; (i9 < 2); i9 = (i9 + 1)) {
			fVec1[i9] = 0.0f;
			
		}
		for (int i10 = 0; (i10 < 2); i10 = (i10 + 1)) {
			fVec2[i10] = 0.0f;
			
		}
		fConst14 = (1.0f / fConst6);
		fConst15 = (1.4122709f + ((fConst3 - 0.80263674f) / fConst2));
		fConst16 = (2.0f * (1.4122709f - (1.0f / fConst8)));
		for (int i11 = 0; (i11 < 3); i11 = (i11 + 1)) {
			fRec5[i11] = 0.0f;
			
		}
		fConst17 = (2.0f * (1.1615164f - fConst9));
		for (int i12 = 0; (i12 < 2); i12 = (i12 + 1)) {
			fVec3[i12] = 0.0f;
			
		}
		for (int i13 = 0; (i13 < 2); i13 = (i13 + 1)) {
			fRec4[i13] = 0.0f;
			
		}
		for (int i14 = 0; (i14 < 2); i14 = (i14 + 1)) {
			fRec12[i14] = 0.0f;
			
		}
		for (int i15 = 0; (i15 < 2); i15 = (i15 + 1)) {
			iRec13[i15] = 0;
			
		}
		fHslider7 = FAUSTFLOAT(0.01f);
		fHslider8 = FAUSTFLOAT(0.01f);
		for (int i16 = 0; (i16 < 2); i16 = (i16 + 1)) {
			fRec14[i16] = 0.0f;
			
		}
		for (int i17 = 0; (i17 < 2); i17 = (i17 + 1)) {
			fVec4[i17] = 0.0f;
			
		}
		for (int i18 = 0; (i18 < 2); i18 = (i18 + 1)) {
			fRec3[i18] = 0.0f;
			
		}
		for (int i19 = 0; (i19 < 2); i19 = (i19 + 1)) {
			iRec15[i19] = 0;
			
		}
		for (int i20 = 0; (i20 < 2); i20 = (i20 + 1)) {
			iRec16[i20] = 0;
			
		}
		fHslider9 = FAUSTFLOAT(0.001f);
		fHslider10 = FAUSTFLOAT(0.001f);
		for (int i21 = 0; (i21 < 2); i21 = (i21 + 1)) {
			fRec17[i21] = 0.0f;
			
		}
		for (int i22 = 0; (i22 < 2); i22 = (i22 + 1)) {
			fRec18[i22] = 0.0f;
			
		}
		for (int i23 = 0; (i23 < 3); i23 = (i23 + 1)) {
			fRec0[i23] = 0.0f;
			
		}
		for (int i24 = 0; (i24 < 2); i24 = (i24 + 1)) {
			fRec20[i24] = 0.0f;
			
		}
		for (int i25 = 0; (i25 < 2); i25 = (i25 + 1)) {
			fRec21[i25] = 0.0f;
			
		}
		for (int i26 = 0; (i26 < 3); i26 = (i26 + 1)) {
			fRec19[i26] = 0.0f;
			
		}
		for (int i27 = 0; (i27 < 2); i27 = (i27 + 1)) {
			fRec23[i27] = 0.0f;
			
		}
		for (int i28 = 0; (i28 < 2); i28 = (i28 + 1)) {
			fRec24[i28] = 0.0f;
			
		}
		for (int i29 = 0; (i29 < 3); i29 = (i29 + 1)) {
			fRec22[i29] = 0.0f;
			
		}
		for (int i30 = 0; (i30 < 2); i30 = (i30 + 1)) {
			fRec26[i30] = 0.0f;
			
		}
		for (int i31 = 0; (i31 < 2); i31 = (i31 + 1)) {
			fRec27[i31] = 0.0f;
			
		}
		for (int i32 = 0; (i32 < 3); i32 = (i32 + 1)) {
			fRec25[i32] = 0.0f;
			
		}
		IOTA = 0;
		for (int i33 = 0; (i33 < 4096); i33 = (i33 + 1)) {
			fVec5[i33] = 0.0f;
			
		}
		fConst18 = (0.5f * fConst0);
		fHslider11 = FAUSTFLOAT(0.5f);
		
	}
	
	virtual void init(int samplingFreq) {
		classInit(samplingFreq);
		instanceInit(samplingFreq);
	}
	
	virtual void buildUserInterface(UI* interface) {
		interface->openVerticalBox("0x00");
		interface->openHorizontalBox("Basic_Parameters");
		interface->declare(&fEntry1, "1", "");
		interface->declare(&fEntry1, "tooltip", "Tone frequency");
		interface->declare(&fEntry1, "unit", "Hz");
		interface->addNumEntry("freq", &fEntry1, 4.4e+02f, 2e+01f, 2e+04f, 1.0f);
		interface->declare(&fEntry0, "1", "");
		interface->declare(&fEntry0, "tooltip", "Gain (value between 0 and 1)");
		interface->addNumEntry("gain", &fEntry0, 1.0f, 0.0f, 1.0f, 0.01f);
		interface->declare(&fButton0, "1", "");
		interface->declare(&fButton0, "tooltip", "noteOn = 1, noteOff = 0");
		interface->addButton("gate",&fButton0);
		interface->closeBox();
		interface->openHorizontalBox("Envelopes_and_Vibrato");
		interface->openVerticalBox("Envelope_Parameters");
		interface->declare(&fHslider9, "4", "");
		interface->declare(&fHslider9, "tooltip", "Noised sounds attack duration");
		interface->declare(&fHslider9, "unit", "s");
		interface->addHorizontalSlider("Noised_Attack", &fHslider9, 0.001f, 0.0f, 2.0f, 0.001f);
		interface->declare(&fHslider10, "4", "");
		interface->declare(&fHslider10, "tooltip", "Noised sounds release duration");
		interface->declare(&fHslider10, "unit", "s");
		interface->addHorizontalSlider("Noised_Release", &fHslider10, 0.001f, 0.0f, 2.0f, 0.001f);
		interface->declare(&fHslider7, "4", "");
		interface->declare(&fHslider7, "tooltip", "Voiced sounds attack duration");
		interface->declare(&fHslider7, "unit", "s");
		interface->addHorizontalSlider("Voiced_Attack", &fHslider7, 0.01f, 0.0f, 2.0f, 0.01f);
		interface->declare(&fHslider8, "4", "");
		interface->declare(&fHslider8, "tooltip", "Voiced sounds release duration");
		interface->declare(&fHslider8, "unit", "s");
		interface->addHorizontalSlider("Voiced_Release", &fHslider8, 0.01f, 0.0f, 2.0f, 0.01f);
		interface->closeBox();
		interface->openVerticalBox("Vibrato_Parameters");
		interface->declare(&fHslider3, "3", "");
		interface->declare(&fHslider3, "tooltip", "Vibrato attack duration");
		interface->declare(&fHslider3, "unit", "s");
		interface->addHorizontalSlider("Vibrato_Attack", &fHslider3, 0.5f, 0.0f, 2.0f, 0.01f);
		interface->declare(&fHslider4, "3", "");
		interface->declare(&fHslider4, "tooltip", "Vibrato silence duration before attack");
		interface->declare(&fHslider4, "unit", "s");
		interface->addHorizontalSlider("Vibrato_Begin", &fHslider4, 0.05f, 0.0f, 2.0f, 0.01f);
		interface->declare(&fHslider6, "3", "");
		interface->declare(&fHslider6, "unit", "Hz");
		interface->addHorizontalSlider("Vibrato_Freq", &fHslider6, 6.0f, 1.0f, 15.0f, 0.1f);
		interface->declare(&fHslider2, "3", "");
		interface->declare(&fHslider2, "tooltip", "A value between 0 and 1");
		interface->addHorizontalSlider("Vibrato_Gain", &fHslider2, 0.05f, 0.0f, 1.0f, 0.01f);
		interface->declare(&fHslider5, "3", "");
		interface->declare(&fHslider5, "tooltip", "Vibrato release duration");
		interface->declare(&fHslider5, "unit", "s");
		interface->addHorizontalSlider("Vibrato_Release", &fHslider5, 0.1f, 0.0f, 2.0f, 0.01f);
		interface->closeBox();
		interface->closeBox();
		interface->openVerticalBox("Physical_Parameters");
		interface->declare(&fHslider1, "2", "");
		interface->declare(&fHslider1, "tooltip", "0->eee, 1->ihh, 2->ehh, 3->aaa, 4->ahh, 5->aww, 6->ohh, 7->uhh, 8->uuu, 9->ooo, 10->rrr, 11->lll, 12->mmm, 13->nnn, 14->nng, 15->ngg, 16->fff, 17->sss, 18->thh, 19->shh, 20->xxx, 21->hee, 22->hoo, 23->hah, 24->bbb, 25->ddd, 26->jjj, 27->ggg, 28->vvv, 29->zzz, 30->thz, 31->zhh");
		interface->addHorizontalSlider("Phoneme", &fHslider1, 4.0f, 0.0f, 31.0f, 1.0f);
		interface->closeBox();
		interface->openVerticalBox("Spat");
		interface->addHorizontalSlider("pan angle", &fHslider0, 0.6f, 0.0f, 1.0f, 0.01f);
		interface->addHorizontalSlider("spatial width", &fHslider11, 0.5f, 0.0f, 1.0f, 0.01f);
		interface->closeBox();
		interface->closeBox();
		
	}
	
	virtual void compute(int count, FAUSTFLOAT** inputs, FAUSTFLOAT** outputs) {
		FAUSTFLOAT* output0 = outputs[0];
		FAUSTFLOAT* output1 = outputs[1];
		float fSlow0 = float(fHslider0);
		float fSlow1 = (1.0f - fSlow0);
		float fSlow2 = float(fHslider1);
		float fSlow3 = float(loadPhonemeParameters(int(fSlow2), 3, 1));
		float fSlow4 = faustpower2_f(fSlow3);
		float fSlow5 = (0.5f * fSlow4);
		float fSlow6 = (fSlow5 - 0.5f);
		float fSlow7 = (0.0f - (2.0f * fSlow3));
		float fSlow8 = (0.001f * float(loadPhonemeParameters(int(fSlow2), 3, 0)));
		float fSlow9 = (0.001f * powf(1e+01f, (0.05f * float(loadPhonemeParameters(int(fSlow2), 3, 2)))));
		float fSlow10 = (0.2f * float(fEntry0));
		float fSlow11 = (fSlow10 + 0.03f);
		float fSlow12 = (1e+02f * float(fHslider2));
		float fSlow13 = float(fButton0);
		int iSlow14 = (fSlow13 > 0.0f);
		float fSlow15 = float(fHslider3);
		float fSlow16 = (1.0f / (float((fSlow15 == 0.0f)) + (fConst0 * fSlow15)));
		float fSlow17 = float(fHslider4);
		float fSlow18 = (fConst0 * fSlow17);
		float fSlow19 = (float((fSlow17 == 0.0f)) + fSlow18);
		float fSlow20 = float(fHslider5);
		float fSlow21 = (1.0f - (1.0f / powf(1e+05f, (1.0f / (float((fSlow20 == 0.0f)) + (fConst0 * fSlow20))))));
		int iSlow22 = (fSlow13 <= 0.0f);
		float fSlow23 = (fConst13 * float(fHslider6));
		float fSlow24 = float(fEntry1);
		float fSlow25 = (0.001f * float(loadPhonemeGains(int(fSlow2), 0)));
		float fSlow26 = float(fHslider7);
		float fSlow27 = (1.0f / (float((fSlow26 == 0.0f)) + (fConst0 * fSlow26)));
		float fSlow28 = float(fHslider8);
		float fSlow29 = (1.0f - (1.0f / powf(1e+05f, (1.0f / (float((fSlow28 == 0.0f)) + (fConst0 * fSlow28))))));
		float fSlow30 = (fSlow10 - 0.97f);
		float fSlow31 = float(fHslider9);
		float fSlow32 = (1.0f / (float((fSlow31 == 0.0f)) + (fConst0 * fSlow31)));
		float fSlow33 = float(fHslider10);
		float fSlow34 = (1.0f - (1.0f / powf(1e+05f, (1.0f / (float((fSlow33 == 0.0f)) + (fConst0 * fSlow33))))));
		float fSlow35 = (0.001f * float(loadPhonemeGains(int(fSlow2), 1)));
		float fSlow36 = (0.5f - fSlow5);
		float fSlow37 = float(loadPhonemeParameters(int(fSlow2), 1, 1));
		float fSlow38 = faustpower2_f(fSlow37);
		float fSlow39 = (0.5f * fSlow38);
		float fSlow40 = (fSlow39 - 0.5f);
		float fSlow41 = (0.0f - (2.0f * fSlow37));
		float fSlow42 = (0.001f * float(loadPhonemeParameters(int(fSlow2), 1, 0)));
		float fSlow43 = (0.001f * powf(1e+01f, (0.05f * float(loadPhonemeParameters(int(fSlow2), 1, 2)))));
		float fSlow44 = float(loadPhonemeParameters(int(fSlow2), 2, 1));
		float fSlow45 = faustpower2_f(fSlow44);
		float fSlow46 = (0.5f * fSlow45);
		float fSlow47 = (fSlow46 - 0.5f);
		float fSlow48 = (0.0f - (2.0f * fSlow44));
		float fSlow49 = (0.001f * float(loadPhonemeParameters(int(fSlow2), 2, 0)));
		float fSlow50 = (0.001f * powf(1e+01f, (0.05f * float(loadPhonemeParameters(int(fSlow2), 2, 2)))));
		float fSlow51 = float(loadPhonemeParameters(int(fSlow2), 0, 1));
		float fSlow52 = faustpower2_f(fSlow51);
		float fSlow53 = (0.5f * fSlow52);
		float fSlow54 = (0.5f - fSlow53);
		float fSlow55 = (0.0f - (2.0f * fSlow51));
		float fSlow56 = (0.001f * float(loadPhonemeParameters(int(fSlow2), 0, 0)));
		float fSlow57 = (0.001f * powf(1e+01f, (0.05f * float(loadPhonemeParameters(int(fSlow2), 0, 2)))));
		float fSlow58 = (fSlow53 - 0.5f);
		float fSlow59 = (0.5f - fSlow46);
		float fSlow60 = (0.5f - fSlow39);
		int iSlow61 = (int((fConst18 * (float(fHslider11) / fSlow24))) & 4095);
		for (int i = 0; (i < count); i = (i + 1)) {
			iVec0[0] = 1;
			fRec1[0] = ((0.999f * fRec1[1]) + fSlow8);
			fRec2[0] = ((0.999f * fRec2[1]) + fSlow9);
			iRec7[0] = (iSlow14 & (iRec7[1] | (fRec9[1] >= 1.0f)));
			iRec8[0] = (iSlow14 * (1 + iRec8[1]));
			int iTemp0 = (iSlow22 & (fRec9[1] > 0.0f));
			fRec9[0] = (((fSlow16 * float((((((iRec7[1] == 0) & iSlow14) & (fRec9[1] < 1.0f)) & (float(iRec8[1]) > fSlow18)) * (1 - (float(iRec8[1]) < fSlow19))))) + (fRec9[1] * (1.0f - (fSlow21 * float(iTemp0))))) * float(((iTemp0 == 0) | (fRec9[1] >= 1e-06f))));
			float fTemp1 = (fRec11[1] + fSlow23);
			fRec11[0] = (fTemp1 - floorf(fTemp1));
			float fTemp2 = max(2e+01f, fabsf(((fSlow12 * (fRec9[0] * ftbl0Voice_Form_dspSIG0[int((65536.0f * fRec11[0]))])) + fSlow24)));
			float fTemp3 = (fRec6[1] + (fConst12 * fTemp2));
			fRec6[0] = (fTemp3 - floorf(fTemp3));
			float fTemp4 = faustpower2_f(((2.0f * fRec6[0]) - 1.0f));
			fVec1[0] = fTemp4;
			float fTemp5 = (((fTemp4 - fVec1[1]) * float(iVec0[1])) / fTemp2);
			fVec2[0] = fTemp5;
			fRec5[0] = ((1.0f + (fConst11 * ((0.25f * fVec2[1]) - (0.25f * fTemp5)))) - ((fConst14 * ((fConst15 * fRec5[2]) + (fConst16 * fRec5[1]))) + float(iVec0[1])));
			float fTemp6 = (((fConst10 * fRec5[0]) + (fConst17 * fRec5[1])) + (fConst10 * fRec5[2]));
			fVec3[0] = fTemp6;
			fRec4[0] = ((fConst5 * fRec4[1]) + (fConst7 * (fTemp6 + fVec3[1])));
			fRec12[0] = ((0.999f * fRec12[1]) + fSlow25);
			iRec13[0] = (iSlow14 & (iRec13[1] | (fRec14[1] >= 1.0f)));
			int iTemp7 = (iSlow22 & (fRec14[1] > 0.0f));
			fRec14[0] = (((fSlow27 * float((((iRec13[1] == 0) & iSlow14) & (fRec14[1] < 1.0f)))) + (fRec14[1] * (1.0f - (fSlow29 * float(iTemp7))))) * float(((iTemp7 == 0) | (fRec14[1] >= 1e-06f))));
			float fTemp8 = ((fRec4[0] * fRec12[0]) * fRec14[0]);
			fVec4[0] = fTemp8;
			fRec3[0] = ((fSlow11 * ((0.47368422f * fVec4[1]) + (0.5263158f * fTemp8))) - (fSlow30 * fRec3[1]));
			iRec15[0] = (12345 + (1103515245 * iRec15[1]));
			iRec16[0] = (iSlow14 & (iRec16[1] | (fRec17[1] >= 1.0f)));
			int iTemp9 = (iSlow22 & (fRec17[1] > 0.0f));
			fRec17[0] = (((fSlow32 * float((((iRec16[1] == 0) & iSlow14) & (fRec17[1] < 1.0f)))) + (fRec17[1] * (1.0f - (fSlow34 * float(iTemp9))))) * float(((iTemp9 == 0) | (fRec17[1] >= 1e-06f))));
			fRec18[0] = ((0.999f * fRec18[1]) + fSlow35);
			float fTemp10 = (fRec3[0] + (4.656613e-10f * ((float(iRec15[0]) * fRec17[0]) * fRec18[0])));
			fRec0[0] = (0.0f - (((fSlow7 * (cosf((fConst1 * fRec1[0])) * fRec0[1])) + (fSlow4 * fRec0[2])) - (fRec2[0] * fTemp10)));
			fRec20[0] = ((0.999f * fRec20[1]) + fSlow42);
			fRec21[0] = ((0.999f * fRec21[1]) + fSlow43);
			fRec19[0] = (0.0f - (((fSlow41 * (cosf((fConst1 * fRec20[0])) * fRec19[1])) + (fSlow38 * fRec19[2])) - (fRec21[0] * fTemp10)));
			fRec23[0] = ((0.999f * fRec23[1]) + fSlow49);
			fRec24[0] = ((0.999f * fRec24[1]) + fSlow50);
			fRec22[0] = (0.0f - (((fSlow48 * (cosf((fConst1 * fRec23[0])) * fRec22[1])) + (fSlow45 * fRec22[2])) - (fRec24[0] * fTemp10)));
			fRec26[0] = ((0.999f * fRec26[1]) + fSlow56);
			fRec27[0] = ((0.999f * fRec27[1]) + fSlow57);
			fRec25[0] = (0.0f - (((fSlow55 * (cosf((fConst1 * fRec26[0])) * fRec25[1])) + (fSlow52 * fRec25[2])) - (fRec27[0] * fTemp10)));
			float fTemp11 = ((fSlow6 * fRec0[2]) + ((fSlow36 * fRec0[0]) + ((fSlow40 * fRec19[2]) + (((fSlow47 * fRec22[2]) + (((fSlow54 * fRec25[0]) + (fSlow58 * fRec25[2])) + (fSlow59 * fRec22[0]))) + (fSlow60 * fRec19[0])))));
			fVec5[(IOTA & 4095)] = fTemp11;
			output0[i] = FAUSTFLOAT((fSlow1 * fTemp11));
			output1[i] = FAUSTFLOAT((fSlow0 * fVec5[((IOTA - iSlow61) & 4095)]));
			iVec0[1] = iVec0[0];
			fRec1[1] = fRec1[0];
			fRec2[1] = fRec2[0];
			iRec7[1] = iRec7[0];
			iRec8[1] = iRec8[0];
			fRec9[1] = fRec9[0];
			fRec11[1] = fRec11[0];
			fRec6[1] = fRec6[0];
			fVec1[1] = fVec1[0];
			fVec2[1] = fVec2[0];
			fRec5[2] = fRec5[1];
			fRec5[1] = fRec5[0];
			fVec3[1] = fVec3[0];
			fRec4[1] = fRec4[0];
			fRec12[1] = fRec12[0];
			iRec13[1] = iRec13[0];
			fRec14[1] = fRec14[0];
			fVec4[1] = fVec4[0];
			fRec3[1] = fRec3[0];
			iRec15[1] = iRec15[0];
			iRec16[1] = iRec16[0];
			fRec17[1] = fRec17[0];
			fRec18[1] = fRec18[0];
			fRec0[2] = fRec0[1];
			fRec0[1] = fRec0[0];
			fRec20[1] = fRec20[0];
			fRec21[1] = fRec21[0];
			fRec19[2] = fRec19[1];
			fRec19[1] = fRec19[0];
			fRec23[1] = fRec23[0];
			fRec24[1] = fRec24[0];
			fRec22[2] = fRec22[1];
			fRec22[1] = fRec22[0];
			fRec26[1] = fRec26[0];
			fRec27[1] = fRec27[0];
			fRec25[2] = fRec25[1];
			fRec25[1] = fRec25[0];
			IOTA = (IOTA + 1);
			
		}
		
	}

	
};



#include "Faust_plugins_template2.cpp"


#endif
