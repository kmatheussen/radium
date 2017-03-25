/* ------------------------------------------------------------
Code generated with Faust 2.0.a43 (http://faust.grame.fr)
------------------------------------------------------------ */
#ifndef FAUSTFLOAT
#define FAUSTFLOAT float
#endif  

/* link with : "" */
#include "typepunning.h"
#include <math.h>


#ifndef FAUSTCLASS 
#define FAUSTCLASS Faust_system_compressor
#endif

class Faust_system_compressor : public dsp {
	
  private:
	
	float fRec0_perm[4];
	float fRec3_perm[4];
	float fRec2_perm[4];
	float fRec1_perm[4];
	FAUSTFLOAT* fInput0_ptr;
	FAUSTFLOAT* fInput1_ptr;
	FAUSTFLOAT* fOutput0_ptr;
	FAUSTFLOAT* fOutput1_ptr;
	FAUSTFLOAT fHslider0;
	int fSamplingFreq;
	float fConst0;
	float fConst1;
	FAUSTFLOAT fHslider1;
	FAUSTFLOAT fHslider2;
	float fConst2;
	FAUSTFLOAT fHslider3;
	FAUSTFLOAT fHbargraph0;
	FAUSTFLOAT fHslider4;
	FAUSTFLOAT fHbargraph1;
	
  public:
	
	void static metadata(Meta* m) { 
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
		fHslider0 = FAUSTFLOAT(0.0f);
		for (int i0 = 0; (i0 < 4); i0 = (i0 + 1)) {
			fRec0_perm[i0] = 0.0f;
			
		}
		fConst0 = min(1.92e+05f, max(1.0f, float(fSamplingFreq)));
		fConst1 = (1.0f / fConst0);
		fHslider1 = FAUSTFLOAT(100.237f);
		for (int i1 = 0; (i1 < 4); i1 = (i1 + 1)) {
			fRec3_perm[i1] = 0.0f;
			
		}
		fHslider2 = FAUSTFLOAT(50.148f);
		for (int i2 = 0; (i2 < 4); i2 = (i2 + 1)) {
			fRec2_perm[i2] = 0.0f;
			
		}
		fConst2 = (2.0f / fConst0);
		fHslider3 = FAUSTFLOAT(2.0f);
		fHslider4 = FAUSTFLOAT(-2e+01f);
		for (int i3 = 0; (i3 < 4); i3 = (i3 + 1)) {
			fRec1_perm[i3] = 0.0f;
			
		}
		
	}
	
	virtual void init(int samplingFreq) {
		classInit(samplingFreq);
		instanceInit(samplingFreq);
	}
	
	virtual void buildUserInterface(UI* interface) {
		interface->openVerticalBox("0x00");
		interface->declare(&fHslider3, "0", "");
		interface->declare(&fHslider3, "style", "slider");
		interface->declare(&fHslider3, "tooltip", "A compression Ratio of N means that for each N dB increase in input signal level above Threshold, the output level goes up 1 dB");
		interface->addHorizontalSlider("Ratio", &fHslider3, 2.0f, 1.0f, 2e+01f, 0.1f);
		interface->declare(&fHslider4, "1", "");
		interface->declare(&fHslider4, "style", "slider");
		interface->declare(&fHslider4, "tooltip", "When the signal level exceeds the Threshold (in dB), its level is compressed according to the Ratio");
		interface->declare(&fHslider4, "unit", "dB");
		interface->addHorizontalSlider("Threshold", &fHslider4, -2e+01f, -2e+01f, 2e+01f, 0.1f);
		interface->declare(&fHslider2, "2", "");
		interface->declare(&fHslider2, "style", "slider");
		interface->declare(&fHslider2, "tooltip", "Time constant in ms (1/e smoothing time) for the compression gain to approach (exponentially) a new lower target level (the compression `kicking in')");
		interface->declare(&fHslider2, "unit", "ms");
		interface->addHorizontalSlider("Attack", &fHslider2, 50.148f, 0.0f, 5e+02f, 0.1f);
		interface->declare(&fHslider1, "3", "");
		interface->declare(&fHslider1, "style", "slider");
		interface->declare(&fHslider1, "tooltip", "Time constant in ms (1/e smoothing time) for the compression gain to approach (exponentially) a new higher target level (the compression 'releasing')");
		interface->declare(&fHslider1, "unit", "ms");
		interface->addHorizontalSlider("Release", &fHslider1, 100.237f, 0.0f, 1e+03f, 0.1f);
		interface->declare(&fHslider0, "5", "");
		interface->declare(&fHslider0, "tooltip", "The compressed-signal output level is increased by this amount (in dB) to make up for the level lost due to compression");
		interface->declare(&fHslider0, "unit", "dB");
		interface->addHorizontalSlider("Output Gain", &fHslider0, 0.0f, -4e+01f, 4e+01f, 0.1f);
		interface->declare(&fHbargraph0, "0", "");
		interface->declare(&fHbargraph0, "7", "");
		interface->addHorizontalBargraph("Gakk", &fHbargraph0, -5e+01f, 1e+01f);
		interface->declare(&fHbargraph1, "1", "");
		interface->declare(&fHbargraph1, "7", "");
		interface->addHorizontalBargraph("Gakk", &fHbargraph1, -5e+01f, 1e+01f);
		interface->closeBox();
		
	}
	
	virtual void compute(int count, FAUSTFLOAT** inputs, FAUSTFLOAT** outputs) {
		float fRec0_tmp[36];
		float fRec3_tmp[36];
		float fRec2_tmp[36];
		float fRec1_tmp[36];
		float fZec0[32];
		float fZec1[32];
		float fZec2[32];
		float fZec3[32];
		FAUSTFLOAT* fInput0 = 0;
		FAUSTFLOAT* fInput1 = 0;
		FAUSTFLOAT* fOutput0 = 0;
		FAUSTFLOAT* fOutput1 = 0;
		float* fRec0 = &fRec0_tmp[4];
		float* fRec3 = &fRec3_tmp[4];
		float* fRec2 = &fRec2_tmp[4];
		float* fRec1 = &fRec1_tmp[4];
		fInput0_ptr = inputs[0];
		fInput1_ptr = inputs[1];
		fOutput0_ptr = outputs[0];
		fOutput1_ptr = outputs[1];
		float fSlow0 = (0.001f * powf(1e+01f, (0.05f * float(fHslider0))));
		float fSlow1 = expf((0.0f - (fConst1 / max(fConst1, (0.001f * float(fHslider1))))));
		float fSlow2 = (1.0f - fSlow1);
		float fSlow3 = max(fConst1, (0.001f * float(fHslider2)));
		float fSlow4 = expf((0.0f - (fConst1 / fSlow3)));
		float fSlow5 = (1.0f - fSlow4);
		float fSlow6 = expf((0.0f - (fConst2 / fSlow3)));
		float fSlow7 = (1.0f - fSlow6);
		float fSlow8 = ((1.0f / float(fHslider3)) - 1.0f);
		float fSlow9 = float(fHslider4);
		int fullcount = count;
		int index = 0;
		/* Main loop */
		for (index = 0; (index <= (fullcount - 32)); index = (index + 32)) {
			fInput0 = &fInput0_ptr[index];
			fInput1 = &fInput1_ptr[index];
			fOutput0 = &fOutput0_ptr[index];
			fOutput1 = &fOutput1_ptr[index];
			int count = 32;
			/* Recursive loop 0 */
			/* Compute code */
			for (int i = 0; (i < count); i = (i + 1)) {
				fZec0[i] = fabsf((fabsf(float(fInput1[i])) + fabsf(float(fInput0[i]))));
				
			}
			/* Recursive loop 1 */
			/* Pre code */
			for (int j1 = 0; (j1 < 4); j1 = (j1 + 1)) {
				fRec3_tmp[j1] = fRec3_perm[j1];
				
			}
			/* Compute code */
			for (int i = 0; (i < count); i = (i + 1)) {
				fRec3[i] = max(fZec0[i], ((fSlow1 * fRec3[(i - 1)]) + (fSlow2 * fZec0[i])));
				
			}
			/* Post code */
			for (int j = 0; (j < 4); j = (j + 1)) {
				fRec3_perm[j] = fRec3_tmp[(count + j)];
				
			}
			/* Recursive loop 2 */
			/* Pre code */
			for (int j2 = 0; (j2 < 4); j2 = (j2 + 1)) {
				fRec2_tmp[j2] = fRec2_perm[j2];
				
			}
			/* Compute code */
			for (int i = 0; (i < count); i = (i + 1)) {
				fRec2[i] = ((fSlow4 * fRec2[(i - 1)]) + (fSlow5 * fRec3[i]));
				
			}
			/* Post code */
			for (int j = 0; (j < 4); j = (j + 1)) {
				fRec2_perm[j] = fRec2_tmp[(count + j)];
				
			}
			/* Recursive loop 3 */
			/* Compute code */
			for (int i = 0; (i < count); i = (i + 1)) {
				fZec1[i] = (8.685889f * ((8.262958e-08f * float(int(pun_float_to_int(float(fRec2[i]))))) - 87.98997f));
				
			}
			/* Recursive loop 4 */
			/* Compute code */
			for (int i = 0; (i < count); i = (i + 1)) {
				fHbargraph0 = FAUSTFLOAT(fZec1[i]);
				fZec2[i] = (fSlow8 * max((fZec1[i] - fSlow9), 0.0f));
				
			}
			/* Recursive loop 5 */
			/* Pre code */
			for (int j0 = 0; (j0 < 4); j0 = (j0 + 1)) {
				fRec0_tmp[j0] = fRec0_perm[j0];
				
			}
			/* Compute code */
			for (int i = 0; (i < count); i = (i + 1)) {
				fRec0[i] = ((0.999f * fRec0[(i - 1)]) + fSlow0);
				
			}
			/* Post code */
			for (int j = 0; (j < 4); j = (j + 1)) {
				fRec0_perm[j] = fRec0_tmp[(count + j)];
				
			}
			/* Recursive loop 6 */
			/* Pre code */
			for (int j3 = 0; (j3 < 4); j3 = (j3 + 1)) {
				fRec1_tmp[j3] = fRec1_perm[j3];
				
			}
			/* Compute code */
			for (int i = 0; (i < count); i = (i + 1)) {
				fHbargraph1 = FAUSTFLOAT(fZec2[i]);
				fRec1[i] = ((fSlow6 * fRec1[(i - 1)]) + (fSlow7 * fZec2[i]));
				
			}
			/* Post code */
			for (int j = 0; (j < 4); j = (j + 1)) {
				fRec1_perm[j] = fRec1_tmp[(count + j)];
				
			}
			/* Recursive loop 7 */
			/* Compute code */
			for (int i = 0; (i < count); i = (i + 1)) {
				fZec3[i] = (fRec0[i] * float(pun_int_to_float(int((8388608.0f * (126.942696f + max(-126.0f, (0.1660964f * fRec1[i]))))))));
				
			}
			/* Vectorizable loop 8 */
			/* Compute code */
			for (int i = 0; (i < count); i = (i + 1)) {
				fOutput0[i] = FAUSTFLOAT((fZec3[i] * float(fInput0[i])));
				
			}
			/* Vectorizable loop 9 */
			/* Compute code */
			for (int i = 0; (i < count); i = (i + 1)) {
				fOutput1[i] = FAUSTFLOAT((fZec3[i] * float(fInput1[i])));
				
			}
			
		}
		/* Remaining frames */
		if (index < fullcount) {
			fInput0 = &fInput0_ptr[index];
			fInput1 = &fInput1_ptr[index];
			fOutput0 = &fOutput0_ptr[index];
			fOutput1 = &fOutput1_ptr[index];
			int count = (fullcount - index);
			/* Recursive loop 0 */
			/* Compute code */
			for (int i = 0; (i < count); i = (i + 1)) {
				fZec0[i] = fabsf((fabsf(float(fInput1[i])) + fabsf(float(fInput0[i]))));
				
			}
			/* Recursive loop 1 */
			/* Pre code */
			for (int j1 = 0; (j1 < 4); j1 = (j1 + 1)) {
				fRec3_tmp[j1] = fRec3_perm[j1];
				
			}
			/* Compute code */
			for (int i = 0; (i < count); i = (i + 1)) {
				fRec3[i] = max(fZec0[i], ((fSlow1 * fRec3[(i - 1)]) + (fSlow2 * fZec0[i])));
				
			}
			/* Post code */
			for (int j = 0; (j < 4); j = (j + 1)) {
				fRec3_perm[j] = fRec3_tmp[(count + j)];
				
			}
			/* Recursive loop 2 */
			/* Pre code */
			for (int j2 = 0; (j2 < 4); j2 = (j2 + 1)) {
				fRec2_tmp[j2] = fRec2_perm[j2];
				
			}
			/* Compute code */
			for (int i = 0; (i < count); i = (i + 1)) {
				fRec2[i] = ((fSlow4 * fRec2[(i - 1)]) + (fSlow5 * fRec3[i]));
				
			}
			/* Post code */
			for (int j = 0; (j < 4); j = (j + 1)) {
				fRec2_perm[j] = fRec2_tmp[(count + j)];
				
			}
			/* Recursive loop 3 */
			/* Compute code */
			for (int i = 0; (i < count); i = (i + 1)) {
				fZec1[i] = (8.685889f * ((8.262958e-08f * float(int(pun_float_to_int(float(fRec2[i]))))) - 87.98997f));
				
			}
			/* Recursive loop 4 */
			/* Compute code */
			for (int i = 0; (i < count); i = (i + 1)) {
				fHbargraph0 = FAUSTFLOAT(fZec1[i]);
				fZec2[i] = (fSlow8 * max((fZec1[i] - fSlow9), 0.0f));
				
			}
			/* Recursive loop 5 */
			/* Pre code */
			for (int j0 = 0; (j0 < 4); j0 = (j0 + 1)) {
				fRec0_tmp[j0] = fRec0_perm[j0];
				
			}
			/* Compute code */
			for (int i = 0; (i < count); i = (i + 1)) {
				fRec0[i] = ((0.999f * fRec0[(i - 1)]) + fSlow0);
				
			}
			/* Post code */
			for (int j = 0; (j < 4); j = (j + 1)) {
				fRec0_perm[j] = fRec0_tmp[(count + j)];
				
			}
			/* Recursive loop 6 */
			/* Pre code */
			for (int j3 = 0; (j3 < 4); j3 = (j3 + 1)) {
				fRec1_tmp[j3] = fRec1_perm[j3];
				
			}
			/* Compute code */
			for (int i = 0; (i < count); i = (i + 1)) {
				fHbargraph1 = FAUSTFLOAT(fZec2[i]);
				fRec1[i] = ((fSlow6 * fRec1[(i - 1)]) + (fSlow7 * fZec2[i]));
				
			}
			/* Post code */
			for (int j = 0; (j < 4); j = (j + 1)) {
				fRec1_perm[j] = fRec1_tmp[(count + j)];
				
			}
			/* Recursive loop 7 */
			/* Compute code */
			for (int i = 0; (i < count); i = (i + 1)) {
				fZec3[i] = (fRec0[i] * float(pun_int_to_float(int((8388608.0f * (126.942696f + max(-126.0f, (0.1660964f * fRec1[i]))))))));
				
			}
			/* Vectorizable loop 8 */
			/* Compute code */
			for (int i = 0; (i < count); i = (i + 1)) {
				fOutput0[i] = FAUSTFLOAT((fZec3[i] * float(fInput0[i])));
				
			}
			/* Vectorizable loop 9 */
			/* Compute code */
			for (int i = 0; (i < count); i = (i + 1)) {
				fOutput1[i] = FAUSTFLOAT((fZec3[i] * float(fInput1[i])));
				
			}
			
		}
		
	}

	
};

