/* ------------------------------------------------------------
Code generated with Faust 2.0.a43 (http://faust.grame.fr)
------------------------------------------------------------ */

#ifndef  __Zita_dsp_H__
#define  __Zita_dsp_H__

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

static float faustpower2_f(float value) {
	return (value * value);
	
}

#ifndef FAUSTCLASS 
#define FAUSTCLASS Zita_dsp
#endif

class Zita_dsp : public dsp {
	
  private:
	
	float fVec0[16384];
	float fVec3[16384];
	float fVec7[16384];
	float fVec1[8192];
	float fVec5[8192];
	float fVec9[8192];
	float fVec10[8192];
	float fVec12[8192];
	float fVec14[8192];
	float fVec16[8192];
	float fVec4[2048];
	float fVec6[2048];
	float fVec8[2048];
	float fVec13[2048];
	float fVec15[2048];
	float fVec2[1024];
	float fVec11[1024];
	float fVec17[1024];
	float fRec0[3];
	float fRec1[3];
	float fRec2[3];
	float fRec3[3];
	float fRec4[3];
	float fRec5[3];
	float fRec6[3];
	float fRec7[3];
	float fRec11[2];
	float fRec10[2];
	float fRec8[2];
	float fRec15[2];
	float fRec14[2];
	float fRec12[2];
	float fRec19[2];
	float fRec18[2];
	float fRec16[2];
	float fRec23[2];
	float fRec22[2];
	float fRec20[2];
	float fRec27[2];
	float fRec26[2];
	float fRec24[2];
	float fRec31[2];
	float fRec30[2];
	float fRec28[2];
	float fRec35[2];
	float fRec34[2];
	float fRec32[2];
	float fRec39[2];
	float fRec38[2];
	float fRec36[2];
	int fSamplingFreq;
	float fConst0;
	float fConst1;
	float fConst2;
	FAUSTFLOAT fVslider0;
	float fConst3;
	FAUSTFLOAT fVslider1;
	FAUSTFLOAT fVslider2;
	float fConst4;
	FAUSTFLOAT fVslider3;
	int IOTA;
	float fConst5;
	int iConst6;
	float fConst7;
	FAUSTFLOAT fVslider4;
	int iConst8;
	float fConst9;
	float fConst10;
	float fConst11;
	int iConst12;
	int iConst13;
	float fConst14;
	float fConst15;
	float fConst16;
	int iConst17;
	int iConst18;
	float fConst19;
	float fConst20;
	float fConst21;
	int iConst22;
	int iConst23;
	float fConst24;
	float fConst25;
	float fConst26;
	int iConst27;
	int iConst28;
	float fConst29;
	float fConst30;
	float fConst31;
	int iConst32;
	int iConst33;
	float fConst34;
	float fConst35;
	float fConst36;
	int iConst37;
	int iConst38;
	float fConst39;
	float fConst40;
	float fConst41;
	int iConst42;
	int iConst43;
	
  public:
	
	void static metadata(Meta* m) { 
		m->declare("filter.lib/author", "Julius O. Smith (jos at ccrma.stanford.edu)");
		m->declare("filter.lib/copyright", "Julius O. Smith III");
		m->declare("filter.lib/license", "STK-4.3");
		m->declare("filter.lib/name", "Faust Filter Library");
		m->declare("filter.lib/reference", "https://ccrma.stanford.edu/~jos/filters/");
		m->declare("filter.lib/version", "1.29");
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
	}

	virtual int getNumInputs() {
		return 2;
		
	}
	virtual int getNumOutputs() {
		return 2;
		
	}
	virtual int getInputRate(int channel) {
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
		
	}
	
	virtual void instanceInit(int samplingFreq) {
		fSamplingFreq = samplingFreq;
		fConst0 = min(1.92e+05f, max(1.0f, float(fSamplingFreq)));
		fConst1 = floorf((0.5f + (0.219991f * fConst0)));
		fConst2 = ((0.0f - (6.9077554f * fConst1)) / fConst0);
		fVslider0 = FAUSTFLOAT(2.0f);
		fConst3 = (6.2831855f / fConst0);
		fVslider1 = FAUSTFLOAT(6e+03f);
		fVslider2 = FAUSTFLOAT(3.0f);
		fConst4 = (3.1415927f / fConst0);
		fVslider3 = FAUSTFLOAT(2e+02f);
		for (int i0 = 0; (i0 < 2); i0 = (i0 + 1)) {
			fRec11[i0] = 0.0f;
			
		}
		for (int i1 = 0; (i1 < 2); i1 = (i1 + 1)) {
			fRec10[i1] = 0.0f;
			
		}
		IOTA = 0;
		for (int i2 = 0; (i2 < 16384); i2 = (i2 + 1)) {
			fVec0[i2] = 0.0f;
			
		}
		fConst5 = floorf((0.5f + (0.019123f * fConst0)));
		iConst6 = (int((fConst1 - fConst5)) & 16383);
		for (int i3 = 0; (i3 < 8192); i3 = (i3 + 1)) {
			fVec1[i3] = 0.0f;
			
		}
		fConst7 = (0.001f * fConst0);
		fVslider4 = FAUSTFLOAT(0.0f);
		for (int i4 = 0; (i4 < 1024); i4 = (i4 + 1)) {
			fVec2[i4] = 0.0f;
			
		}
		iConst8 = (int((fConst5 - 1.0f)) & 1023);
		for (int i5 = 0; (i5 < 2); i5 = (i5 + 1)) {
			fRec8[i5] = 0.0f;
			
		}
		fConst9 = floorf((0.5f + (0.256891f * fConst0)));
		fConst10 = ((0.0f - (6.9077554f * fConst9)) / fConst0);
		for (int i6 = 0; (i6 < 2); i6 = (i6 + 1)) {
			fRec15[i6] = 0.0f;
			
		}
		for (int i7 = 0; (i7 < 2); i7 = (i7 + 1)) {
			fRec14[i7] = 0.0f;
			
		}
		for (int i8 = 0; (i8 < 16384); i8 = (i8 + 1)) {
			fVec3[i8] = 0.0f;
			
		}
		fConst11 = floorf((0.5f + (0.027333f * fConst0)));
		iConst12 = (int((fConst9 - fConst11)) & 16383);
		for (int i9 = 0; (i9 < 2048); i9 = (i9 + 1)) {
			fVec4[i9] = 0.0f;
			
		}
		iConst13 = (int((fConst11 - 1.0f)) & 2047);
		for (int i10 = 0; (i10 < 2); i10 = (i10 + 1)) {
			fRec12[i10] = 0.0f;
			
		}
		fConst14 = floorf((0.5f + (0.192303f * fConst0)));
		fConst15 = ((0.0f - (6.9077554f * fConst14)) / fConst0);
		for (int i11 = 0; (i11 < 2); i11 = (i11 + 1)) {
			fRec19[i11] = 0.0f;
			
		}
		for (int i12 = 0; (i12 < 2); i12 = (i12 + 1)) {
			fRec18[i12] = 0.0f;
			
		}
		for (int i13 = 0; (i13 < 8192); i13 = (i13 + 1)) {
			fVec5[i13] = 0.0f;
			
		}
		fConst16 = floorf((0.5f + (0.029291f * fConst0)));
		iConst17 = (int((fConst14 - fConst16)) & 8191);
		for (int i14 = 0; (i14 < 2048); i14 = (i14 + 1)) {
			fVec6[i14] = 0.0f;
			
		}
		iConst18 = (int((fConst16 - 1.0f)) & 2047);
		for (int i15 = 0; (i15 < 2); i15 = (i15 + 1)) {
			fRec16[i15] = 0.0f;
			
		}
		fConst19 = floorf((0.5f + (0.210389f * fConst0)));
		fConst20 = ((0.0f - (6.9077554f * fConst19)) / fConst0);
		for (int i16 = 0; (i16 < 2); i16 = (i16 + 1)) {
			fRec23[i16] = 0.0f;
			
		}
		for (int i17 = 0; (i17 < 2); i17 = (i17 + 1)) {
			fRec22[i17] = 0.0f;
			
		}
		for (int i18 = 0; (i18 < 16384); i18 = (i18 + 1)) {
			fVec7[i18] = 0.0f;
			
		}
		fConst21 = floorf((0.5f + (0.024421f * fConst0)));
		iConst22 = (int((fConst19 - fConst21)) & 16383);
		for (int i19 = 0; (i19 < 2048); i19 = (i19 + 1)) {
			fVec8[i19] = 0.0f;
			
		}
		iConst23 = (int((fConst21 - 1.0f)) & 2047);
		for (int i20 = 0; (i20 < 2); i20 = (i20 + 1)) {
			fRec20[i20] = 0.0f;
			
		}
		fConst24 = floorf((0.5f + (0.125f * fConst0)));
		fConst25 = ((0.0f - (6.9077554f * fConst24)) / fConst0);
		for (int i21 = 0; (i21 < 2); i21 = (i21 + 1)) {
			fRec27[i21] = 0.0f;
			
		}
		for (int i22 = 0; (i22 < 2); i22 = (i22 + 1)) {
			fRec26[i22] = 0.0f;
			
		}
		for (int i23 = 0; (i23 < 8192); i23 = (i23 + 1)) {
			fVec9[i23] = 0.0f;
			
		}
		fConst26 = floorf((0.5f + (0.013458f * fConst0)));
		iConst27 = (int((fConst24 - fConst26)) & 8191);
		for (int i24 = 0; (i24 < 8192); i24 = (i24 + 1)) {
			fVec10[i24] = 0.0f;
			
		}
		for (int i25 = 0; (i25 < 1024); i25 = (i25 + 1)) {
			fVec11[i25] = 0.0f;
			
		}
		iConst28 = (int((fConst26 - 1.0f)) & 1023);
		for (int i26 = 0; (i26 < 2); i26 = (i26 + 1)) {
			fRec24[i26] = 0.0f;
			
		}
		fConst29 = floorf((0.5f + (0.127837f * fConst0)));
		fConst30 = ((0.0f - (6.9077554f * fConst29)) / fConst0);
		for (int i27 = 0; (i27 < 2); i27 = (i27 + 1)) {
			fRec31[i27] = 0.0f;
			
		}
		for (int i28 = 0; (i28 < 2); i28 = (i28 + 1)) {
			fRec30[i28] = 0.0f;
			
		}
		for (int i29 = 0; (i29 < 8192); i29 = (i29 + 1)) {
			fVec12[i29] = 0.0f;
			
		}
		fConst31 = floorf((0.5f + (0.031604f * fConst0)));
		iConst32 = (int((fConst29 - fConst31)) & 8191);
		for (int i30 = 0; (i30 < 2048); i30 = (i30 + 1)) {
			fVec13[i30] = 0.0f;
			
		}
		iConst33 = (int((fConst31 - 1.0f)) & 2047);
		for (int i31 = 0; (i31 < 2); i31 = (i31 + 1)) {
			fRec28[i31] = 0.0f;
			
		}
		fConst34 = floorf((0.5f + (0.174713f * fConst0)));
		fConst35 = ((0.0f - (6.9077554f * fConst34)) / fConst0);
		for (int i32 = 0; (i32 < 2); i32 = (i32 + 1)) {
			fRec35[i32] = 0.0f;
			
		}
		for (int i33 = 0; (i33 < 2); i33 = (i33 + 1)) {
			fRec34[i33] = 0.0f;
			
		}
		for (int i34 = 0; (i34 < 8192); i34 = (i34 + 1)) {
			fVec14[i34] = 0.0f;
			
		}
		fConst36 = floorf((0.5f + (0.022904f * fConst0)));
		iConst37 = (int((fConst34 - fConst36)) & 8191);
		for (int i35 = 0; (i35 < 2048); i35 = (i35 + 1)) {
			fVec15[i35] = 0.0f;
			
		}
		iConst38 = (int((fConst36 - 1.0f)) & 2047);
		for (int i36 = 0; (i36 < 2); i36 = (i36 + 1)) {
			fRec32[i36] = 0.0f;
			
		}
		fConst39 = floorf((0.5f + (0.153129f * fConst0)));
		fConst40 = ((0.0f - (6.9077554f * fConst39)) / fConst0);
		for (int i37 = 0; (i37 < 2); i37 = (i37 + 1)) {
			fRec39[i37] = 0.0f;
			
		}
		for (int i38 = 0; (i38 < 2); i38 = (i38 + 1)) {
			fRec38[i38] = 0.0f;
			
		}
		for (int i39 = 0; (i39 < 8192); i39 = (i39 + 1)) {
			fVec16[i39] = 0.0f;
			
		}
		fConst41 = floorf((0.5f + (0.020346f * fConst0)));
		iConst42 = (int((fConst39 - fConst41)) & 8191);
		for (int i40 = 0; (i40 < 1024); i40 = (i40 + 1)) {
			fVec17[i40] = 0.0f;
			
		}
		iConst43 = (int((fConst41 - 1.0f)) & 1023);
		for (int i41 = 0; (i41 < 2); i41 = (i41 + 1)) {
			fRec36[i41] = 0.0f;
			
		}
		for (int i42 = 0; (i42 < 3); i42 = (i42 + 1)) {
			fRec0[i42] = 0.0f;
			
		}
		for (int i43 = 0; (i43 < 3); i43 = (i43 + 1)) {
			fRec1[i43] = 0.0f;
			
		}
		for (int i44 = 0; (i44 < 3); i44 = (i44 + 1)) {
			fRec2[i44] = 0.0f;
			
		}
		for (int i45 = 0; (i45 < 3); i45 = (i45 + 1)) {
			fRec3[i45] = 0.0f;
			
		}
		for (int i46 = 0; (i46 < 3); i46 = (i46 + 1)) {
			fRec4[i46] = 0.0f;
			
		}
		for (int i47 = 0; (i47 < 3); i47 = (i47 + 1)) {
			fRec5[i47] = 0.0f;
			
		}
		for (int i48 = 0; (i48 < 3); i48 = (i48 + 1)) {
			fRec6[i48] = 0.0f;
			
		}
		for (int i49 = 0; (i49 < 3); i49 = (i49 + 1)) {
			fRec7[i49] = 0.0f;
			
		}
		
	}
	
	virtual void init(int samplingFreq) {
		classInit(samplingFreq);
		instanceInit(samplingFreq);
	}
	
	virtual void buildUserInterface(UI* interface) {
		interface->declare(0, "0", "");
		interface->declare(0, "tooltip", "~ ZITA REV1 FEEDBACK DELAY NETWORK (FDN) & SCHROEDER ALLPASS-COMB REVERBERATOR (8x8). See Faust's effect.lib for documentation and references");
		interface->openHorizontalBox("Zita_Rev1");
		interface->declare(0, "1", "");
		interface->openHorizontalBox("Input");
		interface->declare(&fVslider4, "1", "");
		interface->declare(&fVslider4, "style", "knob");
		interface->declare(&fVslider4, "tooltip", "Delay in ms before reverberation begins");
		interface->declare(&fVslider4, "unit", "ms");
		interface->addVerticalSlider("In Delay", &fVslider4, 0.0f, 0.0f, 1e+02f, 1.0f);
		interface->closeBox();
		interface->declare(0, "2", "");
		interface->openHorizontalBox("Decay Times in Bands (see tooltips)");
		interface->declare(&fVslider3, "1", "");
		interface->declare(&fVslider3, "style", "knob");
		interface->declare(&fVslider3, "tooltip", "Crossover frequency (Hz) separating low and middle frequencies");
		interface->declare(&fVslider3, "unit", "Hz");
		interface->addVerticalSlider("LF X", &fVslider3, 2e+02f, 5e+01f, 1e+03f, 1.0f);
		interface->declare(&fVslider2, "2", "");
		interface->declare(&fVslider2, "style", "knob");
		interface->declare(&fVslider2, "tooltip", "T60 = time (in seconds) to decay 60dB in low-frequency band");
		interface->declare(&fVslider2, "unit", "s");
		interface->addVerticalSlider("Low RT60", &fVslider2, 3.0f, 1.0f, 8.0f, 0.1f);
		interface->declare(&fVslider0, "3", "");
		interface->declare(&fVslider0, "style", "knob");
		interface->declare(&fVslider0, "tooltip", "T60 = time (in seconds) to decay 60dB in middle band");
		interface->declare(&fVslider0, "unit", "s");
		interface->addVerticalSlider("Mid RT60", &fVslider0, 2.0f, 1.0f, 8.0f, 0.1f);
		interface->declare(&fVslider1, "4", "");
		interface->declare(&fVslider1, "style", "knob");
		interface->declare(&fVslider1, "tooltip", "Frequency (Hz) at which the high-frequency T60 is half the middle-band's T60");
		interface->declare(&fVslider1, "unit", "Hz");
		interface->addVerticalSlider("HF Damping", &fVslider1, 6e+03f, 1.5e+03f, 2.352e+04f, 1.0f);
		interface->closeBox();
		interface->closeBox();
		
	}
	
	virtual void compute(int count, FAUSTFLOAT** inputs, FAUSTFLOAT** outputs) {
		FAUSTFLOAT* input0 = inputs[0];
		FAUSTFLOAT* input1 = inputs[1];
		FAUSTFLOAT* output0 = outputs[0];
		FAUSTFLOAT* output1 = outputs[1];
		float fSlow0 = float(fVslider0);
		float fSlow1 = expf((fConst2 / fSlow0));
		float fSlow2 = faustpower2_f(fSlow1);
		float fSlow3 = cosf((fConst3 * float(fVslider1)));
		float fSlow4 = (1.0f - (fSlow2 * fSlow3));
		float fSlow5 = (1.0f - fSlow2);
		float fSlow6 = (fSlow4 / fSlow5);
		float fSlow7 = sqrtf(max(0.0f, ((faustpower2_f(fSlow4) / faustpower2_f(fSlow5)) - 1.0f)));
		float fSlow8 = (fSlow6 - fSlow7);
		float fSlow9 = (((1.0f + fSlow7) - fSlow6) * fSlow1);
		float fSlow10 = float(fVslider2);
		float fSlow11 = ((expf((fConst2 / fSlow10)) / fSlow1) - 1.0f);
		float fSlow12 = (1.0f / tanf((fConst4 * float(fVslider3))));
		float fSlow13 = (1.0f + fSlow12);
		float fSlow14 = (0.0f - ((1.0f - fSlow12) / fSlow13));
		float fSlow15 = (1.0f / fSlow13);
		int iSlow16 = (int((fConst7 * float(fVslider4))) & 8191);
		float fSlow17 = expf((fConst10 / fSlow0));
		float fSlow18 = faustpower2_f(fSlow17);
		float fSlow19 = (1.0f - (fSlow3 * fSlow18));
		float fSlow20 = (1.0f - fSlow18);
		float fSlow21 = (fSlow19 / fSlow20);
		float fSlow22 = sqrtf(max(0.0f, ((faustpower2_f(fSlow19) / faustpower2_f(fSlow20)) - 1.0f)));
		float fSlow23 = (fSlow21 - fSlow22);
		float fSlow24 = (((1.0f + fSlow22) - fSlow21) * fSlow17);
		float fSlow25 = ((expf((fConst10 / fSlow10)) / fSlow17) - 1.0f);
		float fSlow26 = expf((fConst15 / fSlow0));
		float fSlow27 = faustpower2_f(fSlow26);
		float fSlow28 = (1.0f - (fSlow3 * fSlow27));
		float fSlow29 = (1.0f - fSlow27);
		float fSlow30 = (fSlow28 / fSlow29);
		float fSlow31 = sqrtf(max(0.0f, ((faustpower2_f(fSlow28) / faustpower2_f(fSlow29)) - 1.0f)));
		float fSlow32 = (fSlow30 - fSlow31);
		float fSlow33 = (((1.0f + fSlow31) - fSlow30) * fSlow26);
		float fSlow34 = ((expf((fConst15 / fSlow10)) / fSlow26) - 1.0f);
		float fSlow35 = expf((fConst20 / fSlow0));
		float fSlow36 = faustpower2_f(fSlow35);
		float fSlow37 = (1.0f - (fSlow3 * fSlow36));
		float fSlow38 = (1.0f - fSlow36);
		float fSlow39 = (fSlow37 / fSlow38);
		float fSlow40 = sqrtf(max(0.0f, ((faustpower2_f(fSlow37) / faustpower2_f(fSlow38)) - 1.0f)));
		float fSlow41 = (fSlow39 - fSlow40);
		float fSlow42 = (((1.0f + fSlow40) - fSlow39) * fSlow35);
		float fSlow43 = ((expf((fConst20 / fSlow10)) / fSlow35) - 1.0f);
		float fSlow44 = expf((fConst25 / fSlow0));
		float fSlow45 = faustpower2_f(fSlow44);
		float fSlow46 = (1.0f - (fSlow3 * fSlow45));
		float fSlow47 = (1.0f - fSlow45);
		float fSlow48 = (fSlow46 / fSlow47);
		float fSlow49 = sqrtf(max(0.0f, ((faustpower2_f(fSlow46) / faustpower2_f(fSlow47)) - 1.0f)));
		float fSlow50 = (fSlow48 - fSlow49);
		float fSlow51 = (((1.0f + fSlow49) - fSlow48) * fSlow44);
		float fSlow52 = ((expf((fConst25 / fSlow10)) / fSlow44) - 1.0f);
		float fSlow53 = expf((fConst30 / fSlow0));
		float fSlow54 = faustpower2_f(fSlow53);
		float fSlow55 = (1.0f - (fSlow3 * fSlow54));
		float fSlow56 = (1.0f - fSlow54);
		float fSlow57 = (fSlow55 / fSlow56);
		float fSlow58 = sqrtf(max(0.0f, ((faustpower2_f(fSlow55) / faustpower2_f(fSlow56)) - 1.0f)));
		float fSlow59 = (fSlow57 - fSlow58);
		float fSlow60 = (((1.0f + fSlow58) - fSlow57) * fSlow53);
		float fSlow61 = ((expf((fConst30 / fSlow10)) / fSlow53) - 1.0f);
		float fSlow62 = expf((fConst35 / fSlow0));
		float fSlow63 = faustpower2_f(fSlow62);
		float fSlow64 = (1.0f - (fSlow3 * fSlow63));
		float fSlow65 = (1.0f - fSlow63);
		float fSlow66 = (fSlow64 / fSlow65);
		float fSlow67 = sqrtf(max(0.0f, ((faustpower2_f(fSlow64) / faustpower2_f(fSlow65)) - 1.0f)));
		float fSlow68 = (fSlow66 - fSlow67);
		float fSlow69 = (((1.0f + fSlow67) - fSlow66) * fSlow62);
		float fSlow70 = ((expf((fConst35 / fSlow10)) / fSlow62) - 1.0f);
		float fSlow71 = expf((fConst40 / fSlow0));
		float fSlow72 = faustpower2_f(fSlow71);
		float fSlow73 = (1.0f - (fSlow3 * fSlow72));
		float fSlow74 = (1.0f - fSlow72);
		float fSlow75 = (fSlow73 / fSlow74);
		float fSlow76 = sqrtf(max(0.0f, ((faustpower2_f(fSlow73) / faustpower2_f(fSlow74)) - 1.0f)));
		float fSlow77 = (fSlow75 - fSlow76);
		float fSlow78 = (((1.0f + fSlow76) - fSlow75) * fSlow71);
		float fSlow79 = ((expf((fConst40 / fSlow10)) / fSlow71) - 1.0f);
		for (int i = 0; (i < count); i = (i + 1)) {
			fRec11[0] = ((fSlow14 * fRec11[1]) + (fSlow15 * (fRec7[1] + fRec7[2])));
			fRec10[0] = ((fSlow8 * fRec10[1]) + (fSlow9 * (fRec7[1] + (fSlow11 * fRec11[0]))));
			fVec0[(IOTA & 16383)] = ((0.35355338f * fRec10[0]) + 1e-20f);
			fVec1[(IOTA & 8191)] = float(input1[i]);
			float fTemp0 = (0.3f * fVec1[((IOTA - iSlow16) & 8191)]);
			float fTemp1 = (((0.6f * fRec8[1]) + fVec0[((IOTA - iConst6) & 16383)]) - fTemp0);
			fVec2[(IOTA & 1023)] = fTemp1;
			fRec8[0] = fVec2[((IOTA - iConst8) & 1023)];
			float fRec9 = (0.0f - (0.6f * fTemp1));
			fRec15[0] = ((fSlow14 * fRec15[1]) + (fSlow15 * (fRec3[1] + fRec3[2])));
			fRec14[0] = ((fSlow23 * fRec14[1]) + (fSlow24 * (fRec3[1] + (fSlow25 * fRec15[0]))));
			fVec3[(IOTA & 16383)] = ((0.35355338f * fRec14[0]) + 1e-20f);
			float fTemp2 = (((0.6f * fRec12[1]) + fVec3[((IOTA - iConst12) & 16383)]) - fTemp0);
			fVec4[(IOTA & 2047)] = fTemp2;
			fRec12[0] = fVec4[((IOTA - iConst13) & 2047)];
			float fRec13 = (0.0f - (0.6f * fTemp2));
			fRec19[0] = ((fSlow14 * fRec19[1]) + (fSlow15 * (fRec5[1] + fRec5[2])));
			fRec18[0] = ((fSlow32 * fRec18[1]) + (fSlow33 * (fRec5[1] + (fSlow34 * fRec19[0]))));
			fVec5[(IOTA & 8191)] = ((0.35355338f * fRec18[0]) + 1e-20f);
			float fTemp3 = (fVec5[((IOTA - iConst17) & 8191)] + (fTemp0 + (0.6f * fRec16[1])));
			fVec6[(IOTA & 2047)] = fTemp3;
			fRec16[0] = fVec6[((IOTA - iConst18) & 2047)];
			float fRec17 = (0.0f - (0.6f * fTemp3));
			fRec23[0] = ((fSlow14 * fRec23[1]) + (fSlow15 * (fRec1[1] + fRec1[2])));
			fRec22[0] = ((fSlow41 * fRec22[1]) + (fSlow42 * (fRec1[1] + (fSlow43 * fRec23[0]))));
			fVec7[(IOTA & 16383)] = ((0.35355338f * fRec22[0]) + 1e-20f);
			float fTemp4 = (fTemp0 + ((0.6f * fRec20[1]) + fVec7[((IOTA - iConst22) & 16383)]));
			fVec8[(IOTA & 2047)] = fTemp4;
			fRec20[0] = fVec8[((IOTA - iConst23) & 2047)];
			float fRec21 = (0.0f - (0.6f * fTemp4));
			fRec27[0] = ((fSlow14 * fRec27[1]) + (fSlow15 * (fRec6[1] + fRec6[2])));
			fRec26[0] = ((fSlow50 * fRec26[1]) + (fSlow51 * (fRec6[1] + (fSlow52 * fRec27[0]))));
			fVec9[(IOTA & 8191)] = ((0.35355338f * fRec26[0]) + 1e-20f);
			fVec10[(IOTA & 8191)] = float(input0[i]);
			float fTemp5 = (0.3f * fVec10[((IOTA - iSlow16) & 8191)]);
			float fTemp6 = (fVec9[((IOTA - iConst27) & 8191)] - (fTemp5 + (0.6f * fRec24[1])));
			fVec11[(IOTA & 1023)] = fTemp6;
			fRec24[0] = fVec11[((IOTA - iConst28) & 1023)];
			float fRec25 = (0.6f * fTemp6);
			fRec31[0] = ((fSlow14 * fRec31[1]) + (fSlow15 * (fRec2[1] + fRec2[2])));
			fRec30[0] = ((fSlow59 * fRec30[1]) + (fSlow60 * (fRec2[1] + (fSlow61 * fRec31[0]))));
			fVec12[(IOTA & 8191)] = ((0.35355338f * fRec30[0]) + 1e-20f);
			float fTemp7 = (fVec12[((IOTA - iConst32) & 8191)] - (fTemp5 + (0.6f * fRec28[1])));
			fVec13[(IOTA & 2047)] = fTemp7;
			fRec28[0] = fVec13[((IOTA - iConst33) & 2047)];
			float fRec29 = (0.6f * fTemp7);
			fRec35[0] = ((fSlow14 * fRec35[1]) + (fSlow15 * (fRec4[1] + fRec4[2])));
			fRec34[0] = ((fSlow68 * fRec34[1]) + (fSlow69 * (fRec4[1] + (fSlow70 * fRec35[0]))));
			fVec14[(IOTA & 8191)] = ((0.35355338f * fRec34[0]) + 1e-20f);
			float fTemp8 = ((fTemp5 + fVec14[((IOTA - iConst37) & 8191)]) - (0.6f * fRec32[1]));
			fVec15[(IOTA & 2047)] = fTemp8;
			fRec32[0] = fVec15[((IOTA - iConst38) & 2047)];
			float fRec33 = (0.6f * fTemp8);
			fRec39[0] = ((fSlow14 * fRec39[1]) + (fSlow15 * (fRec0[1] + fRec0[2])));
			fRec38[0] = ((fSlow77 * fRec38[1]) + (fSlow78 * (fRec0[1] + (fSlow79 * fRec39[0]))));
			fVec16[(IOTA & 8191)] = ((0.35355338f * fRec38[0]) + 1e-20f);
			float fTemp9 = ((fVec16[((IOTA - iConst42) & 8191)] + fTemp5) - (0.6f * fRec36[1]));
			fVec17[(IOTA & 1023)] = fTemp9;
			fRec36[0] = fVec17[((IOTA - iConst43) & 1023)];
			float fRec37 = (0.6f * fTemp9);
			float fTemp10 = (fRec37 + fRec33);
			float fTemp11 = (fRec25 + (fRec29 + fTemp10));
			fRec0[0] = (fRec8[1] + (fRec12[1] + (fRec16[1] + (fRec20[1] + (fRec24[1] + (fRec28[1] + (fRec32[1] + (fRec36[1] + (fRec9 + (fRec13 + (fRec17 + (fRec21 + fTemp11))))))))))));
			fRec1[0] = (0.0f - ((fRec8[1] + (fRec12[1] + (fRec16[1] + (fRec20[1] + (fRec9 + (fRec13 + (fRec21 + fRec17))))))) - (fRec24[1] + (fRec28[1] + (fRec32[1] + (fRec36[1] + fTemp11))))));
			float fTemp12 = (fRec29 + fRec25);
			fRec2[0] = (0.0f - ((fRec8[1] + (fRec12[1] + (fRec24[1] + (fRec28[1] + (fRec9 + (fRec13 + fTemp12)))))) - (fRec16[1] + (fRec20[1] + (fRec32[1] + (fRec36[1] + (fRec17 + (fRec21 + fTemp10))))))));
			fRec3[0] = (0.0f - ((fRec16[1] + (fRec20[1] + (fRec24[1] + (fRec28[1] + (fRec17 + (fRec21 + fTemp12)))))) - (fRec8[1] + (fRec12[1] + (fRec32[1] + (fRec36[1] + (fRec9 + (fRec13 + fTemp10))))))));
			float fTemp13 = (fRec33 + fRec25);
			float fTemp14 = (fRec37 + fRec29);
			fRec4[0] = (0.0f - ((fRec8[1] + (fRec16[1] + (fRec24[1] + (fRec32[1] + (fRec9 + (fRec17 + fTemp13)))))) - (fRec12[1] + (fRec20[1] + (fRec28[1] + (fRec36[1] + (fRec13 + (fRec21 + fTemp14))))))));
			fRec5[0] = (0.0f - ((fRec12[1] + (fRec20[1] + (fRec24[1] + (fRec32[1] + (fRec13 + (fRec21 + fTemp13)))))) - (fRec8[1] + (fRec16[1] + (fRec28[1] + (fRec36[1] + (fRec9 + (fRec17 + fTemp14))))))));
			float fTemp15 = (fRec33 + fRec29);
			float fTemp16 = (fRec37 + fRec25);
			fRec6[0] = (0.0f - ((fRec12[1] + (fRec16[1] + (fRec28[1] + (fRec32[1] + (fRec13 + (fRec17 + fTemp15)))))) - (fRec8[1] + (fRec20[1] + (fRec24[1] + (fRec36[1] + (fRec9 + (fRec21 + fTemp16))))))));
			fRec7[0] = (0.0f - ((fRec8[1] + (fRec20[1] + (fRec28[1] + (fRec32[1] + (fRec9 + (fRec21 + fTemp15)))))) - (fRec12[1] + (fRec16[1] + (fRec24[1] + (fRec36[1] + (fRec13 + (fRec17 + fTemp16))))))));
			output0[i] = FAUSTFLOAT((0.37f * (fRec1[0] + fRec2[0])));
			output1[i] = FAUSTFLOAT((0.37f * (fRec1[0] - fRec2[0])));
			fRec11[1] = fRec11[0];
			fRec10[1] = fRec10[0];
			IOTA = (IOTA + 1);
			fRec8[1] = fRec8[0];
			fRec15[1] = fRec15[0];
			fRec14[1] = fRec14[0];
			fRec12[1] = fRec12[0];
			fRec19[1] = fRec19[0];
			fRec18[1] = fRec18[0];
			fRec16[1] = fRec16[0];
			fRec23[1] = fRec23[0];
			fRec22[1] = fRec22[0];
			fRec20[1] = fRec20[0];
			fRec27[1] = fRec27[0];
			fRec26[1] = fRec26[0];
			fRec24[1] = fRec24[0];
			fRec31[1] = fRec31[0];
			fRec30[1] = fRec30[0];
			fRec28[1] = fRec28[0];
			fRec35[1] = fRec35[0];
			fRec34[1] = fRec34[0];
			fRec32[1] = fRec32[0];
			fRec39[1] = fRec39[0];
			fRec38[1] = fRec38[0];
			fRec36[1] = fRec36[0];
			fRec0[2] = fRec0[1];
			fRec0[1] = fRec0[0];
			fRec1[2] = fRec1[1];
			fRec1[1] = fRec1[0];
			fRec2[2] = fRec2[1];
			fRec2[1] = fRec2[0];
			fRec3[2] = fRec3[1];
			fRec3[1] = fRec3[0];
			fRec4[2] = fRec4[1];
			fRec4[1] = fRec4[0];
			fRec5[2] = fRec5[1];
			fRec5[1] = fRec5[0];
			fRec6[2] = fRec6[1];
			fRec6[1] = fRec6[0];
			fRec7[2] = fRec7[1];
			fRec7[1] = fRec7[0];
			
		}
		
	}

	
};



#include "Faust_plugins_template2.cpp"


#endif
