/* ------------------------------------------------------------
name: "system_compressor"
Code generated with Faust 2.20.2 (https://faust.grame.fr)
Compilation options: -lang cpp -vec -lv 0 -vs 32 -ftz 0 -mcd 16
------------------------------------------------------------ */

#ifndef  __Faust_system_compressor_H__
#define  __Faust_system_compressor_H__

#ifndef FAUSTFLOAT
#define FAUSTFLOAT float
#endif 

/* link with : "" */
#include "typepunning.h"
#include <algorithm>
#include <cmath>
#include <math.h>


#ifndef FAUSTCLASS 
#define FAUSTCLASS Faust_system_compressor
#endif

#ifdef __APPLE__ 
#define exp10f __exp10f
#define exp10 __exp10
#endif

class Faust_system_compressor : public dsp {
	
 private:
	
	FAUSTFLOAT fHslider0;
	float fRec0_perm[4];
	int fSampleRate;
	float fConst0;
	float fConst1;
	FAUSTFLOAT fHslider1;
	float fRec3_perm[4];
	FAUSTFLOAT fHslider2;
	float fRec2_perm[4];
	float fConst2;
	FAUSTFLOAT fHslider3;
	FAUSTFLOAT fHbargraph0;
	FAUSTFLOAT fHslider4;
	FAUSTFLOAT fHbargraph1;
	float fRec1_perm[4];
	
 public:
	
	void metadata(Meta* m) { 
		m->declare("effect.lib/author", "Julius O. Smith (jos at ccrma.stanford.edu)");
		m->declare("effect.lib/copyright", "Julius O. Smith III");
		m->declare("effect.lib/deprecated", "This library is deprecated and is not maintained anymore. It will be removed in August 2017.");
		m->declare("effect.lib/exciter_author", "Priyanka Shekar (pshekar@ccrma.stanford.edu)");
		m->declare("effect.lib/exciter_copyright", "Copyright (c) 2013 Priyanka Shekar");
		m->declare("effect.lib/exciter_license", "MIT License (MIT)");
		m->declare("effect.lib/exciter_name", "Harmonic Exciter");
		m->declare("effect.lib/exciter_version", "1.0");
		m->declare("effect.lib/license", "STK-4.3");
		m->declare("effect.lib/name", "Faust Audio Effect Library");
		m->declare("effect.lib/version", "1.33");
		m->declare("filename", "system_compressor.dsp");
		m->declare("filter.lib/author", "Julius O. Smith (jos at ccrma.stanford.edu)");
		m->declare("filter.lib/copyright", "Julius O. Smith III");
		m->declare("filter.lib/deprecated", "This library is deprecated and is not maintained anymore. It will be removed in August 2017.");
		m->declare("filter.lib/license", "STK-4.3");
		m->declare("filter.lib/name", "Faust Filter Library");
		m->declare("filter.lib/reference", "https://ccrma.stanford.edu/~jos/filters/");
		m->declare("filter.lib/version", "1.29");
		m->declare("math.lib/author", "GRAME");
		m->declare("math.lib/copyright", "GRAME");
		m->declare("math.lib/deprecated", "This library is deprecated and is not maintained anymore. It will be removed in August 2017.");
		m->declare("math.lib/license", "LGPL with exception");
		m->declare("math.lib/name", "Math Library");
		m->declare("math.lib/version", "1.0");
		m->declare("music.lib/author", "GRAME");
		m->declare("music.lib/copyright", "GRAME");
		m->declare("music.lib/deprecated", "This library is deprecated and is not maintained anymore. It will be removed in August 2017.");
		m->declare("music.lib/license", "LGPL with exception");
		m->declare("music.lib/name", "Music Library");
		m->declare("music.lib/version", "1.0");
		m->declare("name", "system_compressor");
	}

	virtual int getNumInputs() {
		return 2;
	}
	virtual int getNumOutputs() {
		return 2;
	}
	virtual int getInputRate(int channel) {
		int rate;
		switch ((channel)) {
			case 0: {
				rate = 0;
				break;
			}
			case 1: {
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
	virtual int getOutputRate(int channel) {
		int rate;
		switch ((channel)) {
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
	
	static void classInit(int sample_rate) {
	}
	
	virtual void instanceConstants(int sample_rate) {
		fSampleRate = sample_rate;
		fConst0 = std::min<float>(192000.0f, std::max<float>(1.0f, float(fSampleRate)));
		fConst1 = (1.0f / fConst0);
		fConst2 = (2.0f / fConst0);
	}
	
	virtual void instanceResetUserInterface() {
		fHslider0 = FAUSTFLOAT(0.0f);
		fHslider1 = FAUSTFLOAT(100.23699999999999f);
		fHslider2 = FAUSTFLOAT(50.148000000000003f);
		fHslider3 = FAUSTFLOAT(2.0f);
		fHslider4 = FAUSTFLOAT(-20.0f);
	}
	
	virtual void instanceClear() {
		for (int l0 = 0; (l0 < 4); l0 = (l0 + 1)) {
			fRec0_perm[l0] = 0.0f;
		}
		for (int l1 = 0; (l1 < 4); l1 = (l1 + 1)) {
			fRec3_perm[l1] = 0.0f;
		}
		for (int l2 = 0; (l2 < 4); l2 = (l2 + 1)) {
			fRec2_perm[l2] = 0.0f;
		}
		for (int l3 = 0; (l3 < 4); l3 = (l3 + 1)) {
			fRec1_perm[l3] = 0.0f;
		}
	}
	
	virtual void init(int sample_rate) {
		classInit(sample_rate);
		instanceInit(sample_rate);
	}
	virtual void instanceInit(int sample_rate) {
		instanceConstants(sample_rate);
		instanceResetUserInterface();
		instanceClear();
	}
	
	virtual Faust_system_compressor* clone() {
		return new Faust_system_compressor();
	}
	
	virtual int getSampleRate() {
		return fSampleRate;
	}
	
	virtual void buildUserInterface(UI* ui_interface) {
		ui_interface->openVerticalBox("system_compressor");
		ui_interface->declare(&fHslider3, "0", "");
		ui_interface->declare(&fHslider3, "style", "slider");
		ui_interface->declare(&fHslider3, "tooltip", "A compression Ratio of N means that for each N dB increase in input signal level above Threshold, the output level goes up 1 dB");
		ui_interface->addHorizontalSlider("Ratio", &fHslider3, 2.0f, 1.0f, 20.0f, 0.100000001f);
		ui_interface->declare(&fHslider4, "1", "");
		ui_interface->declare(&fHslider4, "style", "slider");
		ui_interface->declare(&fHslider4, "tooltip", "When the signal level exceeds the Threshold (in dB), its level is compressed according to the Ratio");
		ui_interface->declare(&fHslider4, "unit", "dB");
		ui_interface->addHorizontalSlider("Threshold", &fHslider4, -20.0f, -20.0f, 20.0f, 0.100000001f);
		ui_interface->declare(&fHslider2, "2", "");
		ui_interface->declare(&fHslider2, "style", "slider");
		ui_interface->declare(&fHslider2, "tooltip", "Time constant in ms (1/e smoothing time) for the compression gain to approach (exponentially) a new lower target level (the compression `kicking in')");
		ui_interface->declare(&fHslider2, "unit", "ms");
		ui_interface->addHorizontalSlider("Attack", &fHslider2, 50.1479988f, 0.0f, 500.0f, 0.100000001f);
		ui_interface->declare(&fHslider1, "3", "");
		ui_interface->declare(&fHslider1, "style", "slider");
		ui_interface->declare(&fHslider1, "tooltip", "Time constant in ms (1/e smoothing time) for the compression gain to approach (exponentially) a new higher target level (the compression 'releasing')");
		ui_interface->declare(&fHslider1, "unit", "ms");
		ui_interface->addHorizontalSlider("Release", &fHslider1, 100.237f, 0.0f, 1000.0f, 0.100000001f);
		ui_interface->declare(&fHslider0, "5", "");
		ui_interface->declare(&fHslider0, "tooltip", "The compressed-signal output level is increased by this amount (in dB) to make up for the level lost due to compression");
		ui_interface->declare(&fHslider0, "unit", "dB");
		ui_interface->addHorizontalSlider("Output Gain", &fHslider0, 0.0f, -40.0f, 40.0f, 0.100000001f);
		ui_interface->declare(&fHbargraph0, "0", "");
		ui_interface->declare(&fHbargraph0, "7", "");
		ui_interface->addHorizontalBargraph("Gakk", &fHbargraph0, -50.0f, 10.0f);
		ui_interface->declare(&fHbargraph1, "1", "");
		ui_interface->declare(&fHbargraph1, "7", "");
		ui_interface->addHorizontalBargraph("Gakk", &fHbargraph1, -50.0f, 10.0f);
		ui_interface->closeBox();
	}
	
	virtual void compute(int count, FAUSTFLOAT** inputs, FAUSTFLOAT** outputs) {
		FAUSTFLOAT* input0_ptr = inputs[0];
		FAUSTFLOAT* input1_ptr = inputs[1];
		FAUSTFLOAT* output0_ptr = outputs[0];
		FAUSTFLOAT* output1_ptr = outputs[1];
		float fSlow0 = (0.00100000005f * std::pow(10.0f, (0.0500000007f * float(fHslider0))));
		float fRec0_tmp[36];
		float* fRec0 = &fRec0_tmp[4];
		float fZec0[32];
		float fSlow1 = std::max<float>(fConst1, (0.00100000005f * float(fHslider1)));
		float fSlow2 = ((fSlow1 > 0.0f) ? std::exp((0.0f - (fConst1 / fSlow1))) : 0.0f);
		float fSlow3 = (1.0f - fSlow2);
		float fRec3_tmp[36];
		float* fRec3 = &fRec3_tmp[4];
		float fSlow4 = std::max<float>(fConst1, (0.00100000005f * float(fHslider2)));
		float fSlow5 = ((fSlow4 > 0.0f) ? std::exp((0.0f - (fConst1 / fSlow4))) : 0.0f);
		float fSlow6 = (1.0f - fSlow5);
		float fRec2_tmp[36];
		float* fRec2 = &fRec2_tmp[4];
		float fSlow7 = (((0.5f * fSlow4) > 0.0f) ? std::exp((0.0f - (fConst2 / fSlow4))) : 0.0f);
		float fSlow8 = ((1.0f / float(fHslider3)) + -1.0f);
		float fZec1[32];
		float fSlow9 = float(fHslider4);
		float fZec2[32];
		float fSlow10 = (1.0f - fSlow7);
		float fRec1_tmp[36];
		float* fRec1 = &fRec1_tmp[4];
		float fZec3[32];
		int vindex = 0;
		/* Main loop */
		for (vindex = 0; (vindex <= (count - 32)); vindex = (vindex + 32)) {
			FAUSTFLOAT* input0 = &input0_ptr[vindex];
			FAUSTFLOAT* input1 = &input1_ptr[vindex];
			FAUSTFLOAT* output0 = &output0_ptr[vindex];
			FAUSTFLOAT* output1 = &output1_ptr[vindex];
			int vsize = 32;
			/* Vectorizable loop 0 */
			/* Compute code */
			for (int i = 0; (i < vsize); i = (i + 1)) {
				fZec0[i] = std::fabs((std::fabs(float(input1[i])) + std::fabs(float(input0[i]))));
			}
			/* Recursive loop 1 */
			/* Pre code */
			for (int j2 = 0; (j2 < 4); j2 = (j2 + 1)) {
				fRec3_tmp[j2] = fRec3_perm[j2];
			}
			/* Compute code */
			for (int i = 0; (i < vsize); i = (i + 1)) {
				fRec3[i] = std::max<float>(fZec0[i], ((fRec3[(i - 1)] * fSlow2) + (fZec0[i] * fSlow3)));
			}
			/* Post code */
			for (int j3 = 0; (j3 < 4); j3 = (j3 + 1)) {
				fRec3_perm[j3] = fRec3_tmp[(vsize + j3)];
			}
			/* Recursive loop 2 */
			/* Pre code */
			for (int j4 = 0; (j4 < 4); j4 = (j4 + 1)) {
				fRec2_tmp[j4] = fRec2_perm[j4];
			}
			/* Compute code */
			for (int i = 0; (i < vsize); i = (i + 1)) {
				fRec2[i] = ((fRec2[(i - 1)] * fSlow5) + (fRec3[i] * fSlow6));
			}
			/* Post code */
			for (int j5 = 0; (j5 < 4); j5 = (j5 + 1)) {
				fRec2_perm[j5] = fRec2_tmp[(vsize + j5)];
			}
			/* Vectorizable loop 3 */
			/* Compute code */
			for (int i = 0; (i < vsize); i = (i + 1)) {
				fZec1[i] = (8.68588924f * ((8.26295832e-08f * float(int(pun_float_to_int(float(fRec2[i]))))) + -87.9899673f));
			}
			/* Vectorizable loop 4 */
			/* Compute code */
			for (int i = 0; (i < vsize); i = (i + 1)) {
				fHbargraph0 = FAUSTFLOAT(fZec1[i]);
				fZec2[i] = (fSlow8 * std::max<float>((fZec1[i] - fSlow9), 0.0f));
			}
			/* Recursive loop 5 */
			/* Pre code */
			for (int j6 = 0; (j6 < 4); j6 = (j6 + 1)) {
				fRec1_tmp[j6] = fRec1_perm[j6];
			}
			/* Compute code */
			for (int i = 0; (i < vsize); i = (i + 1)) {
				fHbargraph1 = FAUSTFLOAT(fZec2[i]);
				fRec1[i] = ((fRec1[(i - 1)] * fSlow7) + (fZec2[i] * fSlow10));
			}
			/* Post code */
			for (int j7 = 0; (j7 < 4); j7 = (j7 + 1)) {
				fRec1_perm[j7] = fRec1_tmp[(vsize + j7)];
			}
			/* Recursive loop 6 */
			/* Pre code */
			for (int j0 = 0; (j0 < 4); j0 = (j0 + 1)) {
				fRec0_tmp[j0] = fRec0_perm[j0];
			}
			/* Compute code */
			for (int i = 0; (i < vsize); i = (i + 1)) {
				fRec0[i] = (fSlow0 + (0.999000013f * fRec0[(i - 1)]));
			}
			/* Post code */
			for (int j1 = 0; (j1 < 4); j1 = (j1 + 1)) {
				fRec0_perm[j1] = fRec0_tmp[(vsize + j1)];
			}
			/* Vectorizable loop 7 */
			/* Compute code */
			for (int i = 0; (i < vsize); i = (i + 1)) {
				fZec3[i] = float(pun_int_to_float(int((8388608.0f * (std::max<float>(-126.0f, (0.166096404f * fRec1[i])) + 126.942696f)))));
			}
			/* Vectorizable loop 8 */
			/* Compute code */
			for (int i = 0; (i < vsize); i = (i + 1)) {
				output0[i] = FAUSTFLOAT(((float(input0[i]) * fRec0[i]) * fZec3[i]));
			}
			/* Vectorizable loop 9 */
			/* Compute code */
			for (int i = 0; (i < vsize); i = (i + 1)) {
				output1[i] = FAUSTFLOAT(((float(input1[i]) * fRec0[i]) * fZec3[i]));
			}
		}
		/* Remaining frames */
		if (vindex < count) {
			FAUSTFLOAT* input0 = &input0_ptr[vindex];
			FAUSTFLOAT* input1 = &input1_ptr[vindex];
			FAUSTFLOAT* output0 = &output0_ptr[vindex];
			FAUSTFLOAT* output1 = &output1_ptr[vindex];
			int vsize = (count - vindex);
			/* Vectorizable loop 0 */
			/* Compute code */
			for (int i = 0; (i < vsize); i = (i + 1)) {
				fZec0[i] = std::fabs((std::fabs(float(input1[i])) + std::fabs(float(input0[i]))));
			}
			/* Recursive loop 1 */
			/* Pre code */
			for (int j2 = 0; (j2 < 4); j2 = (j2 + 1)) {
				fRec3_tmp[j2] = fRec3_perm[j2];
			}
			/* Compute code */
			for (int i = 0; (i < vsize); i = (i + 1)) {
				fRec3[i] = std::max<float>(fZec0[i], ((fRec3[(i - 1)] * fSlow2) + (fZec0[i] * fSlow3)));
			}
			/* Post code */
			for (int j3 = 0; (j3 < 4); j3 = (j3 + 1)) {
				fRec3_perm[j3] = fRec3_tmp[(vsize + j3)];
			}
			/* Recursive loop 2 */
			/* Pre code */
			for (int j4 = 0; (j4 < 4); j4 = (j4 + 1)) {
				fRec2_tmp[j4] = fRec2_perm[j4];
			}
			/* Compute code */
			for (int i = 0; (i < vsize); i = (i + 1)) {
				fRec2[i] = ((fRec2[(i - 1)] * fSlow5) + (fRec3[i] * fSlow6));
			}
			/* Post code */
			for (int j5 = 0; (j5 < 4); j5 = (j5 + 1)) {
				fRec2_perm[j5] = fRec2_tmp[(vsize + j5)];
			}
			/* Vectorizable loop 3 */
			/* Compute code */
			for (int i = 0; (i < vsize); i = (i + 1)) {
				fZec1[i] = (8.68588924f * ((8.26295832e-08f * float(int(pun_float_to_int(float(fRec2[i]))))) + -87.9899673f));
			}
			/* Vectorizable loop 4 */
			/* Compute code */
			for (int i = 0; (i < vsize); i = (i + 1)) {
				fHbargraph0 = FAUSTFLOAT(fZec1[i]);
				fZec2[i] = (fSlow8 * std::max<float>((fZec1[i] - fSlow9), 0.0f));
			}
			/* Recursive loop 5 */
			/* Pre code */
			for (int j6 = 0; (j6 < 4); j6 = (j6 + 1)) {
				fRec1_tmp[j6] = fRec1_perm[j6];
			}
			/* Compute code */
			for (int i = 0; (i < vsize); i = (i + 1)) {
				fHbargraph1 = FAUSTFLOAT(fZec2[i]);
				fRec1[i] = ((fRec1[(i - 1)] * fSlow7) + (fZec2[i] * fSlow10));
			}
			/* Post code */
			for (int j7 = 0; (j7 < 4); j7 = (j7 + 1)) {
				fRec1_perm[j7] = fRec1_tmp[(vsize + j7)];
			}
			/* Recursive loop 6 */
			/* Pre code */
			for (int j0 = 0; (j0 < 4); j0 = (j0 + 1)) {
				fRec0_tmp[j0] = fRec0_perm[j0];
			}
			/* Compute code */
			for (int i = 0; (i < vsize); i = (i + 1)) {
				fRec0[i] = (fSlow0 + (0.999000013f * fRec0[(i - 1)]));
			}
			/* Post code */
			for (int j1 = 0; (j1 < 4); j1 = (j1 + 1)) {
				fRec0_perm[j1] = fRec0_tmp[(vsize + j1)];
			}
			/* Vectorizable loop 7 */
			/* Compute code */
			for (int i = 0; (i < vsize); i = (i + 1)) {
				fZec3[i] = float(pun_int_to_float(int((8388608.0f * (std::max<float>(-126.0f, (0.166096404f * fRec1[i])) + 126.942696f)))));
			}
			/* Vectorizable loop 8 */
			/* Compute code */
			for (int i = 0; (i < vsize); i = (i + 1)) {
				output0[i] = FAUSTFLOAT(((float(input0[i]) * fRec0[i]) * fZec3[i]));
			}
			/* Vectorizable loop 9 */
			/* Compute code */
			for (int i = 0; (i < vsize); i = (i + 1)) {
				output1[i] = FAUSTFLOAT(((float(input1[i]) * fRec0[i]) * fZec3[i]));
			}
		}
	}

};

#endif
