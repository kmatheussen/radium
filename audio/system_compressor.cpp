//-----------------------------------------------------
//
// Code generated with Faust 0.9.55 (http://faust.grame.fr)
//-----------------------------------------------------
#ifndef FAUSTFLOAT
#define FAUSTFLOAT float
#endif  

typedef long double quad;
/* link with : "" */
#include "typepunning.h"
#include <math.h>

#ifndef FAUSTCLASS 
#define FAUSTCLASS Faust_system_compressor
#endif

class Faust_system_compressor : public dsp {
  private:
	int 	iConst0;
	float 	fConst1;
	FAUSTFLOAT 	fslider0;
	float 	fRec2_perm[4];
	FAUSTFLOAT 	fslider1;
	float 	fRec1_perm[4];
	float 	fConst2;
	FAUSTFLOAT 	fslider2;
	FAUSTFLOAT 	fbargraph0;
	FAUSTFLOAT 	fslider3;
	FAUSTFLOAT 	fbargraph1;
	float 	fRec0_perm[4];
	FAUSTFLOAT 	fslider4;
  public:
	static void metadata(Meta* m) 	{ 
		m->declare("filter.lib/name", "Faust Filter Library");
		m->declare("filter.lib/author", "Julius O. Smith (jos at ccrma.stanford.edu)");
		m->declare("filter.lib/copyright", "Julius O. Smith III");
		m->declare("filter.lib/version", "1.29");
		m->declare("filter.lib/license", "STK-4.3");
		m->declare("filter.lib/reference", "https://ccrma.stanford.edu/~jos/filters/");
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
		m->declare("effect.lib/name", "Faust Audio Effect Library");
		m->declare("effect.lib/author", "Julius O. Smith (jos at ccrma.stanford.edu)");
		m->declare("effect.lib/copyright", "Julius O. Smith III");
		m->declare("effect.lib/version", "1.33");
		m->declare("effect.lib/license", "STK-4.3");
	}

	virtual int getNumInputs() 	{ return 2; }
	virtual int getNumOutputs() 	{ return 2; }
	static void classInit(int samplingFreq) {
	}
	virtual void instanceInit(int samplingFreq) {
		fSamplingFreq = samplingFreq;
		iConst0 = min(192000, max(1, fSamplingFreq));
		fConst1 = (1.0f / float(iConst0));
		fslider0 = 100.237f;
		for (int i=0; i<4; i++) fRec2_perm[i]=0;
		fslider1 = 50.148f;
		for (int i=0; i<4; i++) fRec1_perm[i]=0;
		fConst2 = (2.0f / float(iConst0));
		fslider2 = -2e+01f;
		fslider3 = 2.0f;
		for (int i=0; i<4; i++) fRec0_perm[i]=0;
		fslider4 = 0.0f;
	}
	virtual void init(int samplingFreq) {
		classInit(samplingFreq);
		instanceInit(samplingFreq);
	}
	virtual void buildUserInterface(UI* interface) {
		interface->openVerticalBox("system_compressor");
		interface->declare(&fslider3, "0", "");
		interface->declare(&fslider3, "style", "slider");
		interface->declare(&fslider3, "tooltip", "A compression Ratio of N means that for each N dB increase in input signal level above Threshold, the output level goes up 1 dB");
		interface->addHorizontalSlider("Ratio", &fslider3, 2.0f, 1.0f, 2e+01f, 0.1f);
		interface->declare(&fslider2, "1", "");
		interface->declare(&fslider2, "style", "slider");
		interface->declare(&fslider2, "tooltip", "When the signal level exceeds the Threshold (in dB), its level is compressed according to the Ratio");
		interface->declare(&fslider2, "unit", "dB");
		interface->addHorizontalSlider("Threshold", &fslider2, -2e+01f, -2e+01f, 2e+01f, 0.1f);
		interface->declare(&fslider1, "2", "");
		interface->declare(&fslider1, "style", "slider");
		interface->declare(&fslider1, "tooltip", "Time constant in ms (1/e smoothing time) for the compression gain to approach (exponentially) a new lower target level (the compression `kicking in')");
		interface->declare(&fslider1, "unit", "ms");
		interface->addHorizontalSlider("Attack", &fslider1, 50.148f, 0.0f, 5e+02f, 0.1f);
		interface->declare(&fslider0, "3", "");
		interface->declare(&fslider0, "style", "slider");
		interface->declare(&fslider0, "tooltip", "Time constant in ms (1/e smoothing time) for the compression gain to approach (exponentially) a new higher target level (the compression 'releasing')");
		interface->declare(&fslider0, "unit", "ms");
		interface->addHorizontalSlider("Release", &fslider0, 100.237f, 0.0f, 1e+03f, 0.1f);
		interface->declare(&fslider4, "5", "");
		interface->declare(&fslider4, "tooltip", "The compressed-signal output level is increased by this amount (in dB) to make up for the level lost due to compression");
		interface->declare(&fslider4, "unit", "dB");
		interface->addHorizontalSlider("Output Gain", &fslider4, 0.0f, -4e+01f, 4e+01f, 0.1f);
		interface->declare(&fbargraph0, "0", "");
		interface->declare(&fbargraph0, "7", "");
		interface->addHorizontalBargraph("Gakk", &fbargraph0, -5e+01f, 1e+01f);
		interface->declare(&fbargraph1, "1", "");
		interface->declare(&fbargraph1, "7", "");
		interface->addHorizontalBargraph("Gakk", &fbargraph1, -5e+01f, 1e+01f);
		interface->closeBox();
	}
	virtual void compute (int fullcount, FAUSTFLOAT** input, FAUSTFLOAT** output) {
		float 	fZec0[32];
		float 	fRec2_tmp[32+4];
		float 	fRec1_tmp[32+4];
		float 	fZec1[32];
		float 	fZec2[32];
		float 	fRec0_tmp[32+4];
		float 	fZec3[32];
		float 	fSlow0 = expf((0 - (fConst1 / max(fConst1, (0.001f * fslider0)))));
		float 	fSlow1 = (1.0f - fSlow0);
		float* 	fRec2 = &fRec2_tmp[4];
		float 	fSlow2 = max(fConst1, (0.001f * fslider1));
		float 	fSlow3 = expf((0 - (fConst1 / fSlow2)));
		float 	fSlow4 = (1.0f - fSlow3);
		float* 	fRec1 = &fRec1_tmp[4];
		float 	fSlow5 = expf((0 - (fConst2 / fSlow2)));
		float 	fSlow6 = fslider2;
		float 	fSlow7 = ((1.0f / float(fslider3)) - 1.0f);
		float 	fSlow8 = (1.0f - fSlow5);
		float* 	fRec0 = &fRec0_tmp[4];
		float 	fSlow9 = powf(10,(0.05f * fslider4));
		int index;
		for (index = 0; index <= fullcount - 32; index += 32) {
			// compute by blocks of 32 samples
			const int count = 32;
			FAUSTFLOAT* input0 = &input[0][index];
			FAUSTFLOAT* input1 = &input[1][index];
			FAUSTFLOAT* output0 = &output[0][index];
			FAUSTFLOAT* output1 = &output[1][index];
			// SECTION : 1
			// LOOP 0x2eb21f0
			// exec code
			for (int i=0; i<count; i++) {
				fZec0[i] = fabsf((fabsf((float)input1[i]) + fabsf((float)input0[i])));
			}
			
			// SECTION : 2
			// LOOP 0x2eb1e80
			// pre processing
			for (int i=0; i<4; i++) fRec2_tmp[i]=fRec2_perm[i];
			// exec code
			for (int i=0; i<count; i++) {
				fRec2[i] = ((fSlow1 * fZec0[i]) + (fSlow0 * max(fZec0[i], fRec2[i-1])));
			}
			// post processing
			for (int i=0; i<4; i++) fRec2_perm[i]=fRec2_tmp[count+i];
			
			// SECTION : 3
			// LOOP 0x2eb19a0
			// pre processing
			for (int i=0; i<4; i++) fRec1_tmp[i]=fRec1_perm[i];
			// exec code
			for (int i=0; i<count; i++) {
				fRec1[i] = ((fSlow4 * fRec2[i]) + (fSlow3 * fRec1[i-1]));
			}
			// post processing
			for (int i=0; i<4; i++) fRec1_perm[i]=fRec1_tmp[count+i];
			
			// SECTION : 4
			// LOOP 0x2eb7e00
			// exec code
			for (int i=0; i<count; i++) {
				fZec1[i] = (8.685889638065037f * ((8.262958288192749e-08f * float(pun_float_to_int(fRec1[i]))) - 87.989971088f));
			}
			
			// SECTION : 5
			// LOOP 0x2eb78c0
			// exec code
			for (int i=0; i<count; i++) {
				fbargraph0 = fZec1[i];
				fZec2[i] = (fSlow7 * max((fZec1[i] - fSlow6), 0.0f));
			}
			
			// SECTION : 6
			// LOOP 0x2eb15a0
			// pre processing
			for (int i=0; i<4; i++) fRec0_tmp[i]=fRec0_perm[i];
			// exec code
			for (int i=0; i<count; i++) {
				fbargraph1 = fZec2[i];
				fRec0[i] = ((fSlow8 * fZec2[i]) + (fSlow5 * fRec0[i-1]));
			}
			// post processing
			for (int i=0; i<4; i++) fRec0_perm[i]=fRec0_tmp[count+i];
			
			// SECTION : 7
			// LOOP 0x2ebbfa0
			// exec code
			for (int i=0; i<count; i++) {
				fZec3[i] = pun_int_to_float((8388608 * (126.94269504f + max(-126.0f, (0.16609640464202244f * fRec0[i])))));
			}
			
			// SECTION : 8
			// LOOP 0x2eb13a0
			// exec code
			for (int i=0; i<count; i++) {
				output0[i] = (FAUSTFLOAT)(fSlow9 * ((float)input0[i] * fZec3[i]));
			}
			
			// LOOP 0x2ebde40
			// exec code
			for (int i=0; i<count; i++) {
				output1[i] = (FAUSTFLOAT)(fSlow9 * ((float)input1[i] * fZec3[i]));
			}
			
		}
		if (index < fullcount) {
			// compute the remaining samples if any
			int count = fullcount-index;
			FAUSTFLOAT* input0 = &input[0][index];
			FAUSTFLOAT* input1 = &input[1][index];
			FAUSTFLOAT* output0 = &output[0][index];
			FAUSTFLOAT* output1 = &output[1][index];
			// SECTION : 1
			// LOOP 0x2eb21f0
			// exec code
			for (int i=0; i<count; i++) {
				fZec0[i] = fabsf((fabsf((float)input1[i]) + fabsf((float)input0[i])));
			}
			
			// SECTION : 2
			// LOOP 0x2eb1e80
			// pre processing
			for (int i=0; i<4; i++) fRec2_tmp[i]=fRec2_perm[i];
			// exec code
			for (int i=0; i<count; i++) {
				fRec2[i] = ((fSlow1 * fZec0[i]) + (fSlow0 * max(fZec0[i], fRec2[i-1])));
			}
			// post processing
			for (int i=0; i<4; i++) fRec2_perm[i]=fRec2_tmp[count+i];
			
			// SECTION : 3
			// LOOP 0x2eb19a0
			// pre processing
			for (int i=0; i<4; i++) fRec1_tmp[i]=fRec1_perm[i];
			// exec code
			for (int i=0; i<count; i++) {
				fRec1[i] = ((fSlow4 * fRec2[i]) + (fSlow3 * fRec1[i-1]));
			}
			// post processing
			for (int i=0; i<4; i++) fRec1_perm[i]=fRec1_tmp[count+i];
			
			// SECTION : 4
			// LOOP 0x2eb7e00
			// exec code
			for (int i=0; i<count; i++) {
				fZec1[i] = (8.685889638065037f * ((8.262958288192749e-08f * float(pun_float_to_int(fRec1[i]))) - 87.989971088f));
			}
			
			// SECTION : 5
			// LOOP 0x2eb78c0
			// exec code
			for (int i=0; i<count; i++) {
				fbargraph0 = fZec1[i];
				fZec2[i] = (fSlow7 * max((fZec1[i] - fSlow6), 0.0f));
			}
			
			// SECTION : 6
			// LOOP 0x2eb15a0
			// pre processing
			for (int i=0; i<4; i++) fRec0_tmp[i]=fRec0_perm[i];
			// exec code
			for (int i=0; i<count; i++) {
				fbargraph1 = fZec2[i];
				fRec0[i] = ((fSlow8 * fZec2[i]) + (fSlow5 * fRec0[i-1]));
			}
			// post processing
			for (int i=0; i<4; i++) fRec0_perm[i]=fRec0_tmp[count+i];
			
			// SECTION : 7
			// LOOP 0x2ebbfa0
			// exec code
			for (int i=0; i<count; i++) {
				fZec3[i] = pun_int_to_float((8388608 * (126.94269504f + max(-126.0f, (0.16609640464202244f * fRec0[i])))));
			}
			
			// SECTION : 8
			// LOOP 0x2eb13a0
			// exec code
			for (int i=0; i<count; i++) {
				output0[i] = (FAUSTFLOAT)(fSlow9 * ((float)input0[i] * fZec3[i]));
			}
			
			// LOOP 0x2ebde40
			// exec code
			for (int i=0; i<count; i++) {
				output1[i] = (FAUSTFLOAT)(fSlow9 * ((float)input1[i] * fZec3[i]));
			}
			
		}
	}
};


