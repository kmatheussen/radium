//-----------------------------------------------------
// name: "Flute"
// author: "Romain Michon (rmichon@ccrma.stanford.edu)"
// copyright: "Romain Michon"
// version: "1.0"
//
// Code generated with Faust 0.9.46 (http://faust.grame.fr)
//-----------------------------------------------------
/* link with  */
#include <math.h>
#include <cmath>
template <int N> inline float faustpower(float x) 		{ return powf(x,N); } 
template <int N> inline double faustpower(double x) 	{ return pow(x,N); }
template <int N> inline int faustpower(int x) 			{ return faustpower<N/2>(x) * faustpower<N-N/2>(x); } 
template <> 	 inline int faustpower<0>(int x) 		{ return 1; }
template <> 	 inline int faustpower<1>(int x) 		{ return x; }

#include "/usr/share/faust/audio/dsp.h"
#include "/usr/share/faust/gui/UI.h"

struct Meta
{
    void declare (const char* key, const char* value) { }
};

static float scale(float x, float x1, float x2, float y1, float y2){
  return y1 + ( ((x-x1)*(y2-y1))
                /
                (x2-x1)
                );
}

// For some reason, it won't compile with the usual min/max macros.
template<typename T> static inline T min(T a,T b){return a<b ? a : b;}
template<typename T> static inline T max(T a,T b){return a>b ? a : b;}
static inline float min(float a,int b){return a<b ? a : b;}
static inline float max(float a,int b){return a>b ? a : b;}
static inline float min(int a,float b){return a<b ? a : b;}
static inline float max(int a,float b){return a>b ? a : b;}


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

typedef long double quad;

#define FAUSTCLASS my_dsp

class my_dsp : public dsp {
  private:
	class SIG0 {
	  private:
		int 	fSamplingFreq;
		int 	iRec17[2];
	  public:
		int getNumInputs() 	{ return 0; }
		int getNumOutputs() 	{ return 1; }
		void init(int samplingFreq) {
			fSamplingFreq = samplingFreq;
			for (int i=0; i<2; i++) iRec17[i] = 0;
		}
		void fill (int count, float output[]) {
			for (int i=0; i<count; i++) {
				iRec17[0] = (1 + iRec17[1]);
				output[i] = sinf((9.587379924285257e-05f * float((iRec17[0] - 1))));
				// post processing
				iRec17[1] = iRec17[0];
			}
		}
	};


	FAUSTFLOAT 	fslider0;
	int 	iConst0;
	float 	fConst1;
	float 	fConst2;
	float 	fConst3;
	float 	fConst4;
	float 	fConst5;
	float 	fConst6;
	float 	fConst7;
	float 	fConst8;
	float 	fConst9;
	float 	fRec11[2];
	float 	fConst10;
	float 	fRec10[2];
	int 	IOTA;
	float 	fVec0[8192];
	float 	fConst11;
	int 	iConst12;
	FAUSTFLOAT 	fslider1;
	float 	fRec12[2];
	FAUSTFLOAT 	fbutton0;
	int 	iRec13[2];
	FAUSTFLOAT 	fslider2;
	FAUSTFLOAT 	fslider3;
	float 	fRec14[2];
	float 	fConst13;
	float 	fConst14;
	float 	fConst15;
	static float 	ftbl0[65536];
	FAUSTFLOAT 	fslider4;
	float 	fConst16;
	float 	fRec18[2];
	int 	iRec19[2];
	int 	iRec20[2];
	FAUSTFLOAT 	fslider5;
	FAUSTFLOAT 	fslider6;
	FAUSTFLOAT 	fslider7;
	float 	fRec21[2];
	FAUSTFLOAT 	fslider8;
	FAUSTFLOAT 	fslider9;
	float 	fRec23[2];
	FAUSTFLOAT 	fentry0;
	FAUSTFLOAT 	fentry1;
	float 	fRec22[2];
	int 	iRec24[2];
	float 	fConst17;
	FAUSTFLOAT 	fslider10;
	float 	fRec25[2];
	FAUSTFLOAT 	fslider11;
	float 	fRec26[2];
	float 	fVec1[2];
	float 	fRec32[2];
	float 	fRec31[2];
	float 	fRec30[2];
	float 	fRec29[2];
	float 	fRec28[2];
	float 	fRec27[2];
	float 	fRec38[2];
	float 	fRec37[2];
	float 	fRec36[2];
	float 	fRec35[2];
	float 	fRec34[2];
	float 	fRec33[2];
	int 	iRec39[2];
	FAUSTFLOAT 	fslider12;
	FAUSTFLOAT 	fslider13;
	float 	fRec40[2];
	FAUSTFLOAT 	fcheckbox0;
	int 	iRec41[2];
	FAUSTFLOAT 	fslider14;
	FAUSTFLOAT 	fslider15;
	FAUSTFLOAT 	fslider16;
	float 	fRec42[2];
	float 	fVec2[4096];
	float 	fConst18;
	float 	fVec3[2];
	float 	fConst19;
	float 	fRec16[2];
	float 	fRec15[8192];
	FAUSTFLOAT 	fslider17;
	FAUSTFLOAT 	fentry2;
	float 	fVec4[4096];
	int 	iConst20;
	float 	fVec5[2048];
	int 	iConst21;
	float 	fRec8[2];
	float 	fConst22;
	float 	fConst23;
	float 	fConst24;
	float 	fRec46[2];
	float 	fConst25;
	float 	fRec45[2];
	float 	fVec6[8192];
	float 	fConst26;
	int 	iConst27;
	float 	fVec7[1024];
	int 	iConst28;
	float 	fRec43[2];
	float 	fConst29;
	float 	fConst30;
	float 	fConst31;
	float 	fRec50[2];
	float 	fConst32;
	float 	fRec49[2];
	float 	fVec8[8192];
	float 	fConst33;
	int 	iConst34;
	float 	fVec9[2048];
	int 	iConst35;
	float 	fRec47[2];
	float 	fConst36;
	float 	fConst37;
	float 	fConst38;
	float 	fRec54[2];
	float 	fConst39;
	float 	fRec53[2];
	float 	fVec10[8192];
	float 	fConst40;
	int 	iConst41;
	float 	fVec11[1024];
	int 	iConst42;
	float 	fRec51[2];
	float 	fConst43;
	float 	fConst44;
	float 	fConst45;
	float 	fRec58[2];
	float 	fConst46;
	float 	fRec57[2];
	float 	fVec12[16384];
	float 	fConst47;
	int 	iConst48;
	float 	fVec13[4096];
	FAUSTFLOAT 	fslider18;
	float 	fVec14[4096];
	float 	fVec15[2048];
	int 	iConst49;
	float 	fRec55[2];
	float 	fConst50;
	float 	fConst51;
	float 	fConst52;
	float 	fRec62[2];
	float 	fConst53;
	float 	fRec61[2];
	float 	fVec16[8192];
	float 	fConst54;
	int 	iConst55;
	float 	fVec17[2048];
	int 	iConst56;
	float 	fRec59[2];
	float 	fConst57;
	float 	fConst58;
	float 	fConst59;
	float 	fRec66[2];
	float 	fConst60;
	float 	fRec65[2];
	float 	fVec18[16384];
	float 	fConst61;
	int 	iConst62;
	float 	fVec19[2048];
	int 	iConst63;
	float 	fRec63[2];
	float 	fConst64;
	float 	fConst65;
	float 	fConst66;
	float 	fRec70[2];
	float 	fConst67;
	float 	fRec69[2];
	float 	fVec20[16384];
	float 	fConst68;
	int 	iConst69;
	float 	fVec21[1024];
	int 	iConst70;
	float 	fRec67[2];
	float 	fRec0[3];
	float 	fRec1[3];
	float 	fRec2[3];
	float 	fRec3[3];
	float 	fRec4[3];
	float 	fRec5[3];
	float 	fRec6[3];
	float 	fRec7[3];
  public:
	static void metadata(Meta* m) 	{ 
		m->declare("name", "Flute");
		m->declare("description", "Nonlinear WaveGuide Flute");
		m->declare("author", "Romain Michon (rmichon@ccrma.stanford.edu)");
		m->declare("copyright", "Romain Michon");
		m->declare("version", "1.0");
		m->declare("licence", "STK-4.3");
		m->declare("music.lib/name", "Music Library");
		m->declare("music.lib/author", "GRAME");
		m->declare("music.lib/copyright", "GRAME");
		m->declare("music.lib/version", "1.0");
		m->declare("music.lib/license", "LGPL");
		m->declare("math.lib/name", "Math Library");
		m->declare("math.lib/author", "GRAME");
		m->declare("math.lib/copyright", "GRAME");
		m->declare("math.lib/version", "1.0");
		m->declare("math.lib/license", "LGPL");
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
		m->declare("effect.lib/reference", "https://ccrma.stanford.edu/realsimple/faust_strings/");
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
		fslider0 = 0.72f;
		iConst0 = min(192000, max(1, fSamplingFreq));
		fConst1 = floorf((0.5f + (0.174713f * iConst0)));
		fConst2 = ((0 - (6.907755278982138f * fConst1)) / iConst0);
		fConst3 = (0.5f * fConst2);
		fConst4 = float(iConst0);
		fConst5 = cosf((37699.11184307752f / fConst4));
		fConst6 = (1.0f / tanf((628.3185307179587f / iConst0)));
		fConst7 = (1 + fConst6);
		fConst8 = (0 - ((1 - fConst6) / fConst7));
		fConst9 = (1.0f / fConst7);
		for (int i=0; i<2; i++) fRec11[i] = 0;
		fConst10 = (0.3333333333333333f * fConst2);
		for (int i=0; i<2; i++) fRec10[i] = 0;
		IOTA = 0;
		for (int i=0; i<8192; i++) fVec0[i] = 0;
		fConst11 = floorf((0.5f + (0.022904f * iConst0)));
		iConst12 = int((int((fConst1 - fConst11)) & 8191));
		fslider1 = 0.137f;
		for (int i=0; i<2; i++) fRec12[i] = 0;
		fbutton0 = 0.0;
		for (int i=0; i<2; i++) iRec13[i] = 0;
		fslider2 = 0.1f;
		fslider3 = 0.1f;
		for (int i=0; i<2; i++) fRec14[i] = 0;
		fConst13 = (1.0f / tanf((6283.185307179586f / iConst0)));
		fConst14 = (1 + fConst13);
		fConst15 = (0 - ((1 - fConst13) / fConst14));
		fslider4 = 5.0f;
		fConst16 = (1.0f / fConst4);
		for (int i=0; i<2; i++) fRec18[i] = 0;
		for (int i=0; i<2; i++) iRec19[i] = 0;
		for (int i=0; i<2; i++) iRec20[i] = 0;
		fslider5 = 0.2f;
		fslider6 = 0.1f;
		fslider7 = 0.5f;
		for (int i=0; i<2; i++) fRec21[i] = 0;
		fslider8 = 0.1f;
		fslider9 = 2.2e+02f;
		for (int i=0; i<2; i++) fRec23[i] = 0;
		fentry0 = 0.0f;
		fentry1 = 4.4e+02f;
		for (int i=0; i<2; i++) fRec22[i] = 0;
		for (int i=0; i<2; i++) iRec24[i] = 0;
		fConst17 = (1 - (1.0f / powf(1e+05f,(1e+01f / iConst0))));
		fslider10 = 0.1f;
		for (int i=0; i<2; i++) fRec25[i] = 0;
		fslider11 = 0.0f;
		for (int i=0; i<2; i++) fRec26[i] = 0;
		for (int i=0; i<2; i++) fVec1[i] = 0;
		for (int i=0; i<2; i++) fRec32[i] = 0;
		for (int i=0; i<2; i++) fRec31[i] = 0;
		for (int i=0; i<2; i++) fRec30[i] = 0;
		for (int i=0; i<2; i++) fRec29[i] = 0;
		for (int i=0; i<2; i++) fRec28[i] = 0;
		for (int i=0; i<2; i++) fRec27[i] = 0;
		for (int i=0; i<2; i++) fRec38[i] = 0;
		for (int i=0; i<2; i++) fRec37[i] = 0;
		for (int i=0; i<2; i++) fRec36[i] = 0;
		for (int i=0; i<2; i++) fRec35[i] = 0;
		for (int i=0; i<2; i++) fRec34[i] = 0;
		for (int i=0; i<2; i++) fRec33[i] = 0;
		for (int i=0; i<2; i++) iRec39[i] = 0;
		fslider12 = 0.1f;
		fslider13 = 0.9f;
		for (int i=0; i<2; i++) fRec40[i] = 0;
		fcheckbox0 = 0.0;
		for (int i=0; i<2; i++) iRec41[i] = 0;
		fslider14 = 1.0f;
		fslider15 = 0.2f;
		fslider16 = 0.05f;
		for (int i=0; i<2; i++) fRec42[i] = 0;
		for (int i=0; i<4096; i++) fVec2[i] = 0;
		fConst18 = (0.5f * iConst0);
		for (int i=0; i<2; i++) fVec3[i] = 0;
		fConst19 = (1.0f / fConst14);
		for (int i=0; i<2; i++) fRec16[i] = 0;
		for (int i=0; i<8192; i++) fRec15[i] = 0;
		fslider17 = 0.6f;
		fentry2 = 1.0f;
		for (int i=0; i<4096; i++) fVec4[i] = 0;
		iConst20 = int((int((0.02f * iConst0)) & 8191));
		for (int i=0; i<2048; i++) fVec5[i] = 0;
		iConst21 = int((int((fConst11 - 1)) & 2047));
		for (int i=0; i<2; i++) fRec8[i] = 0;
		fConst22 = floorf((0.5f + (0.153129f * iConst0)));
		fConst23 = ((0 - (6.907755278982138f * fConst22)) / iConst0);
		fConst24 = (0.5f * fConst23);
		for (int i=0; i<2; i++) fRec46[i] = 0;
		fConst25 = (0.3333333333333333f * fConst23);
		for (int i=0; i<2; i++) fRec45[i] = 0;
		for (int i=0; i<8192; i++) fVec6[i] = 0;
		fConst26 = floorf((0.5f + (0.020346f * iConst0)));
		iConst27 = int((int((fConst22 - fConst26)) & 8191));
		for (int i=0; i<1024; i++) fVec7[i] = 0;
		iConst28 = int((int((fConst26 - 1)) & 1023));
		for (int i=0; i<2; i++) fRec43[i] = 0;
		fConst29 = floorf((0.5f + (0.127837f * iConst0)));
		fConst30 = ((0 - (6.907755278982138f * fConst29)) / iConst0);
		fConst31 = (0.5f * fConst30);
		for (int i=0; i<2; i++) fRec50[i] = 0;
		fConst32 = (0.3333333333333333f * fConst30);
		for (int i=0; i<2; i++) fRec49[i] = 0;
		for (int i=0; i<8192; i++) fVec8[i] = 0;
		fConst33 = floorf((0.5f + (0.031604f * iConst0)));
		iConst34 = int((int((fConst29 - fConst33)) & 8191));
		for (int i=0; i<2048; i++) fVec9[i] = 0;
		iConst35 = int((int((fConst33 - 1)) & 2047));
		for (int i=0; i<2; i++) fRec47[i] = 0;
		fConst36 = floorf((0.5f + (0.125f * iConst0)));
		fConst37 = ((0 - (6.907755278982138f * fConst36)) / iConst0);
		fConst38 = (0.5f * fConst37);
		for (int i=0; i<2; i++) fRec54[i] = 0;
		fConst39 = (0.3333333333333333f * fConst37);
		for (int i=0; i<2; i++) fRec53[i] = 0;
		for (int i=0; i<8192; i++) fVec10[i] = 0;
		fConst40 = floorf((0.5f + (0.013458f * iConst0)));
		iConst41 = int((int((fConst36 - fConst40)) & 8191));
		for (int i=0; i<1024; i++) fVec11[i] = 0;
		iConst42 = int((int((fConst40 - 1)) & 1023));
		for (int i=0; i<2; i++) fRec51[i] = 0;
		fConst43 = floorf((0.5f + (0.210389f * iConst0)));
		fConst44 = ((0 - (6.907755278982138f * fConst43)) / iConst0);
		fConst45 = (0.5f * fConst44);
		for (int i=0; i<2; i++) fRec58[i] = 0;
		fConst46 = (0.3333333333333333f * fConst44);
		for (int i=0; i<2; i++) fRec57[i] = 0;
		for (int i=0; i<16384; i++) fVec12[i] = 0;
		fConst47 = floorf((0.5f + (0.024421f * iConst0)));
		iConst48 = int((int((fConst43 - fConst47)) & 16383));
		for (int i=0; i<4096; i++) fVec13[i] = 0;
		fslider18 = 0.5f;
		for (int i=0; i<4096; i++) fVec14[i] = 0;
		for (int i=0; i<2048; i++) fVec15[i] = 0;
		iConst49 = int((int((fConst47 - 1)) & 2047));
		for (int i=0; i<2; i++) fRec55[i] = 0;
		fConst50 = floorf((0.5f + (0.192303f * iConst0)));
		fConst51 = ((0 - (6.907755278982138f * fConst50)) / iConst0);
		fConst52 = (0.5f * fConst51);
		for (int i=0; i<2; i++) fRec62[i] = 0;
		fConst53 = (0.3333333333333333f * fConst51);
		for (int i=0; i<2; i++) fRec61[i] = 0;
		for (int i=0; i<8192; i++) fVec16[i] = 0;
		fConst54 = floorf((0.5f + (0.029291f * iConst0)));
		iConst55 = int((int((fConst50 - fConst54)) & 8191));
		for (int i=0; i<2048; i++) fVec17[i] = 0;
		iConst56 = int((int((fConst54 - 1)) & 2047));
		for (int i=0; i<2; i++) fRec59[i] = 0;
		fConst57 = floorf((0.5f + (0.256891f * iConst0)));
		fConst58 = ((0 - (6.907755278982138f * fConst57)) / iConst0);
		fConst59 = (0.5f * fConst58);
		for (int i=0; i<2; i++) fRec66[i] = 0;
		fConst60 = (0.3333333333333333f * fConst58);
		for (int i=0; i<2; i++) fRec65[i] = 0;
		for (int i=0; i<16384; i++) fVec18[i] = 0;
		fConst61 = floorf((0.5f + (0.027333f * iConst0)));
		iConst62 = int((int((fConst57 - fConst61)) & 16383));
		for (int i=0; i<2048; i++) fVec19[i] = 0;
		iConst63 = int((int((fConst61 - 1)) & 2047));
		for (int i=0; i<2; i++) fRec63[i] = 0;
		fConst64 = floorf((0.5f + (0.219991f * iConst0)));
		fConst65 = ((0 - (6.907755278982138f * fConst64)) / iConst0);
		fConst66 = (0.5f * fConst65);
		for (int i=0; i<2; i++) fRec70[i] = 0;
		fConst67 = (0.3333333333333333f * fConst65);
		for (int i=0; i<2; i++) fRec69[i] = 0;
		for (int i=0; i<16384; i++) fVec20[i] = 0;
		fConst68 = floorf((0.5f + (0.019123f * iConst0)));
		iConst69 = int((int((fConst64 - fConst68)) & 16383));
		for (int i=0; i<1024; i++) fVec21[i] = 0;
		iConst70 = int((int((fConst68 - 1)) & 1023));
		for (int i=0; i<2; i++) fRec67[i] = 0;
		for (int i=0; i<3; i++) fRec0[i] = 0;
		for (int i=0; i<3; i++) fRec1[i] = 0;
		for (int i=0; i<3; i++) fRec2[i] = 0;
		for (int i=0; i<3; i++) fRec3[i] = 0;
		for (int i=0; i<3; i++) fRec4[i] = 0;
		for (int i=0; i<3; i++) fRec5[i] = 0;
		for (int i=0; i<3; i++) fRec6[i] = 0;
		for (int i=0; i<3; i++) fRec7[i] = 0;
	}
	virtual void init(int samplingFreq) {
		classInit(samplingFreq);
		instanceInit(samplingFreq);
	}
	virtual void buildUserInterface(UI* interface) {
		interface->openVerticalBox("flute");
		interface->openHorizontalBox("Basic_Parameters");
		interface->declare(&fentry1, "1", "");
		interface->declare(&fentry1, "tooltip", "Tone frequency");
		interface->declare(&fentry1, "unit", "Hz");
		interface->addNumEntry("freq", &fentry1, 4.4e+02f, 2e+01f, 2e+04f, 1.0f);
		interface->declare(&fentry2, "1", "");
		interface->declare(&fentry2, "tooltip", "Gain (value between 0 and 1)");
		interface->addNumEntry("gain", &fentry2, 1.0f, 0.0f, 1.0f, 0.01f);
		interface->declare(&fbutton0, "1", "");
		interface->declare(&fbutton0, "tooltip", "noteOn = 1, noteOff = 0");
		interface->addButton("gate", &fbutton0);
		interface->closeBox();
		interface->openHorizontalBox("Envelopes_and_Vibrato");
		interface->openVerticalBox("Global_Envelope_Parameters");
		interface->declare(&fslider3, "6", "");
		interface->declare(&fslider3, "tooltip", "Global envelope attack duration");
		interface->declare(&fslider3, "unit", "s");
		interface->addHorizontalSlider("Glob_Env_Attack", &fslider3, 0.1f, 0.0f, 2.0f, 0.01f);
		interface->declare(&fslider2, "6", "");
		interface->declare(&fslider2, "tooltip", "Global envelope release duration");
		interface->declare(&fslider2, "unit", "s");
		interface->addHorizontalSlider("Glob_Env_Release", &fslider2, 0.1f, 0.0f, 2.0f, 0.01f);
		interface->closeBox();
		interface->openVerticalBox("Pressure_Envelope_Parameters");
		interface->declare(&fslider16, "5", "");
		interface->declare(&fslider16, "tooltip", "Pressure envelope attack duration");
		interface->declare(&fslider16, "unit", "s");
		interface->addHorizontalSlider("Press_Env_Attack", &fslider16, 0.05f, 0.0f, 2.0f, 0.01f);
		interface->declare(&fslider15, "5", "");
		interface->declare(&fslider15, "tooltip", "Pressure envelope decay duration");
		interface->declare(&fslider15, "unit", "s");
		interface->addHorizontalSlider("Press_Env_Decay", &fslider15, 0.2f, 0.0f, 2.0f, 0.01f);
		interface->declare(&fslider14, "5", "");
		interface->declare(&fslider14, "tooltip", "Pressure envelope release duration");
		interface->declare(&fslider14, "unit", "s");
		interface->addHorizontalSlider("Press_Env_Release", &fslider14, 1.0f, 0.0f, 2.0f, 0.01f);
		interface->declare(&fcheckbox0, "5", "");
		interface->declare(&fcheckbox0, "tooltip", "Activate Pressure envelope");
		interface->declare(&fcheckbox0, "unit", "s");
		interface->addCheckButton("Pressure_Env", &fcheckbox0);
		interface->closeBox();
		interface->openVerticalBox("Vibrato_Parameters");
		interface->declare(&fslider7, "4", "");
		interface->declare(&fslider7, "tooltip", "Vibrato attack duration");
		interface->declare(&fslider7, "unit", "s");
		interface->addHorizontalSlider("Vibrato_Attack", &fslider7, 0.5f, 0.0f, 2.0f, 0.01f);
		interface->declare(&fslider6, "4", "");
		interface->declare(&fslider6, "tooltip", "Vibrato silence duration before attack");
		interface->declare(&fslider6, "unit", "s");
		interface->addHorizontalSlider("Vibrato_Begin", &fslider6, 0.1f, 0.0f, 2.0f, 0.01f);
		interface->declare(&fslider4, "4", "");
		interface->declare(&fslider4, "unit", "Hz");
		interface->addHorizontalSlider("Vibrato_Freq", &fslider4, 5.0f, 1.0f, 15.0f, 0.1f);
		interface->declare(&fslider8, "4", "");
		interface->declare(&fslider8, "tooltip", "A value between 0 and 1");
		interface->addHorizontalSlider("Vibrato_Gain", &fslider8, 0.1f, 0.0f, 1.0f, 0.01f);
		interface->declare(&fslider5, "4", "");
		interface->declare(&fslider5, "tooltip", "Vibrato release duration");
		interface->declare(&fslider5, "unit", "s");
		interface->addHorizontalSlider("Vibrato_Release", &fslider5, 0.2f, 0.0f, 2.0f, 0.01f);
		interface->closeBox();
		interface->closeBox();
		interface->openHorizontalBox("Physical_and_Nonlinearity");
		interface->openVerticalBox("Nonlinear_Filter_Parameters");
		interface->declare(&fslider9, "3", "");
		interface->declare(&fslider9, "tooltip", "Frequency of the sine wave for the modulation of theta (works if Modulation Type=3)");
		interface->declare(&fslider9, "unit", "Hz");
		interface->addHorizontalSlider("Modulation_Frequency", &fslider9, 2.2e+02f, 2e+01f, 1e+03f, 0.1f);
		interface->declare(&fentry0, "3", "");
		interface->declare(&fentry0, "tooltip", "0=theta is modulated by the incoming signal; 1=theta is modulated by the averaged incoming signal; 2=theta is modulated by the squared incoming signal; 3=theta is modulated by a sine wave of frequency freqMod; 4=theta is modulated by a sine wave of frequency freq;");
		interface->addNumEntry("Modulation_Type", &fentry0, 0.0f, 0.0f, 4.0f, 1.0f);
		interface->declare(&fslider11, "3", "");
		interface->declare(&fslider11, "tooltip", "Nonlinearity factor (value between 0 and 1)");
		interface->addHorizontalSlider("Nonlinearity", &fslider11, 0.0f, 0.0f, 1.0f, 0.01f);
		interface->declare(&fslider10, "3", "");
		interface->declare(&fslider10, "Attack duration of the nonlinearity", "");
		interface->declare(&fslider10, "unit", "s");
		interface->addHorizontalSlider("Nonlinearity Attack", &fslider10, 0.1f, 0.0f, 2.0f, 0.01f);
		interface->closeBox();
		interface->openVerticalBox("Physical_Parameters");
		interface->declare(&fslider12, "2", "");
		interface->declare(&fslider12, "tooltip", "Breath noise gain (value between 0 and 1)");
		interface->addHorizontalSlider("Noise Gain", &fslider12, 0.1f, 0.0f, 1.0f, 0.01f);
		interface->declare(&fslider13, "2", "");
		interface->declare(&fslider13, "tooltip", "Breath pressure (value bewteen 0 and 1)");
		interface->addHorizontalSlider("Pressure", &fslider13, 0.9f, 0.0f, 1.5f, 0.01f);
		interface->closeBox();
		interface->closeBox();
		interface->openVerticalBox("Reverb");
		interface->addHorizontalSlider("reverbGain", &fslider1, 0.137f, 0.0f, 1.0f, 0.01f);
		interface->addHorizontalSlider("roomSize", &fslider0, 0.72f, 0.01f, 2.0f, 0.01f);
		interface->closeBox();
		interface->openVerticalBox("Spat");
		interface->addHorizontalSlider("pan angle", &fslider17, 0.6f, 0.0f, 1.0f, 0.01f);
		interface->addHorizontalSlider("spatial width", &fslider18, 0.5f, 0.0f, 1.0f, 0.01f);
		interface->closeBox();
		interface->closeBox();
	}
	virtual void compute (int count, FAUSTFLOAT** input, FAUSTFLOAT** output) {
		float 	fSlow0 = fslider0;
		float 	fSlow1 = expf((fConst3 / fSlow0));
		float 	fSlow2 = faustpower<2>(fSlow1);
		float 	fSlow3 = (1.0f - fSlow2);
		float 	fSlow4 = (1.0f - (fConst5 * fSlow2));
		float 	fSlow5 = sqrtf(max(0, ((faustpower<2>(fSlow4) / faustpower<2>(fSlow3)) - 1.0f)));
		float 	fSlow6 = (fSlow4 / fSlow3);
		float 	fSlow7 = (fSlow6 - fSlow5);
		float 	fSlow8 = ((expf((fConst10 / fSlow0)) / fSlow1) - 1);
		float 	fSlow9 = (fSlow1 * ((1.0f + fSlow5) - fSlow6));
		float 	fSlow10 = (0.0010000000000000009f * fslider1);
		int 	iSlow11 = int(fbutton0);
		int 	iSlow12 = (iSlow11 > 0);
		int 	iSlow13 = (iSlow11 <= 0);
		float 	fSlow14 = fslider2;
		float 	fSlow15 = (1 - (1.0f / powf(1e+05f,(1.0f / ((fSlow14 == 0.0f) + (iConst0 * fSlow14))))));
		float 	fSlow16 = fslider3;
		float 	fSlow17 = (1.0f / ((fSlow16 == 0.0f) + (iConst0 * fSlow16)));
		float 	fSlow18 = (fConst16 * fslider4);
		float 	fSlow19 = fslider5;
		float 	fSlow20 = (1 - (1.0f / powf(1e+05f,(1.0f / ((fSlow19 == 0.0f) + (iConst0 * fSlow19))))));
		float 	fSlow21 = fslider6;
		float 	fSlow22 = (iConst0 * fSlow21);
		float 	fSlow23 = ((fSlow21 == 0.0f) + fSlow22);
		float 	fSlow24 = fslider7;
		float 	fSlow25 = (1.0f / ((fSlow24 == 0.0f) + (iConst0 * fSlow24)));
		float 	fSlow26 = fslider8;
		float 	fSlow27 = (0.0010000000000000009f * fslider9);
		float 	fSlow28 = fentry0;
		int 	iSlow29 = (fSlow28 != 4);
		float 	fSlow30 = fentry1;
		float 	fSlow31 = (fSlow30 * (fSlow28 == 4));
		float 	fSlow32 = fslider10;
		float 	fSlow33 = (1.0f / ((fSlow32 == 0.0f) + (iConst0 * fSlow32)));
		float 	fSlow34 = (0.0010000000000000009f * fslider11);
		float 	fSlow35 = (iConst0 / fSlow30);
		int 	iSlow36 = int((fSlow35 - 2));
		int 	iSlow37 = int((1 + int((iSlow36 & 4095))));
		float 	fSlow38 = ((3 + iSlow36) - fSlow35);
		int 	iSlow39 = int((1 + int((int((1 + iSlow36)) & 4095))));
		float 	fSlow40 = (fSlow35 - (2 + iSlow36));
		int 	iSlow41 = (fSlow28 >= 3);
		float 	fSlow42 = (3.141592653589793f * (fSlow28 == 2));
		float 	fSlow43 = (3.141592653589793f * (fSlow28 == 0));
		float 	fSlow44 = (1.5707963267948966f * (fSlow28 == 1));
		int 	iSlow45 = (fSlow28 < 3);
		float 	fSlow46 = (5.122274162770377e-11f * fslider12);
		float 	fSlow47 = (0.0010000000000000009f * fslider13);
		int 	iSlow48 = (iSlow11 | int(fcheckbox0));
		int 	iSlow49 = (iSlow48 > 0);
		int 	iSlow50 = (iSlow48 <= 0);
		float 	fSlow51 = fslider14;
		float 	fSlow52 = (1 - (1.0f / powf(9e+04f,(1.0f / ((fSlow51 == 0.0f) + (iConst0 * fSlow51))))));
		float 	fSlow53 = fslider15;
		float 	fSlow54 = (1 - powf(9e+01f,(1.0f / ((fSlow53 == 0.0f) + (iConst0 * fSlow53)))));
		float 	fSlow55 = fslider16;
		float 	fSlow56 = (1.0f / ((fSlow55 == 0.0f) + (iConst0 * fSlow55)));
		float 	fSlow57 = (fConst18 / fSlow30);
		int 	iSlow58 = int((fSlow57 - 2));
		int 	iSlow59 = int((iSlow58 & 4095));
		float 	fSlow60 = ((3 + iSlow58) - fSlow57);
		int 	iSlow61 = int((int((1 + iSlow58)) & 4095));
		float 	fSlow62 = (fSlow57 - (2 + iSlow58));
		float 	fSlow63 = fslider17;
		float 	fSlow64 = fentry2;
		float 	fSlow65 = (0.5f * (fSlow64 * (1.0f - fSlow63)));
		float 	fSlow66 = expf((fConst24 / fSlow0));
		float 	fSlow67 = faustpower<2>(fSlow66);
		float 	fSlow68 = (1.0f - fSlow67);
		float 	fSlow69 = (1.0f - (fConst5 * fSlow67));
		float 	fSlow70 = sqrtf(max(0, ((faustpower<2>(fSlow69) / faustpower<2>(fSlow68)) - 1.0f)));
		float 	fSlow71 = (fSlow69 / fSlow68);
		float 	fSlow72 = (fSlow71 - fSlow70);
		float 	fSlow73 = ((expf((fConst25 / fSlow0)) / fSlow66) - 1);
		float 	fSlow74 = (fSlow66 * ((1.0f + fSlow70) - fSlow71));
		float 	fSlow75 = expf((fConst31 / fSlow0));
		float 	fSlow76 = faustpower<2>(fSlow75);
		float 	fSlow77 = (1.0f - fSlow76);
		float 	fSlow78 = (1.0f - (fConst5 * fSlow76));
		float 	fSlow79 = sqrtf(max(0, ((faustpower<2>(fSlow78) / faustpower<2>(fSlow77)) - 1.0f)));
		float 	fSlow80 = (fSlow78 / fSlow77);
		float 	fSlow81 = (fSlow80 - fSlow79);
		float 	fSlow82 = ((expf((fConst32 / fSlow0)) / fSlow75) - 1);
		float 	fSlow83 = (fSlow75 * ((1.0f + fSlow79) - fSlow80));
		float 	fSlow84 = expf((fConst38 / fSlow0));
		float 	fSlow85 = faustpower<2>(fSlow84);
		float 	fSlow86 = (1.0f - fSlow85);
		float 	fSlow87 = (1.0f - (fConst5 * fSlow85));
		float 	fSlow88 = sqrtf(max(0, ((faustpower<2>(fSlow87) / faustpower<2>(fSlow86)) - 1.0f)));
		float 	fSlow89 = (fSlow87 / fSlow86);
		float 	fSlow90 = (fSlow89 - fSlow88);
		float 	fSlow91 = ((expf((fConst39 / fSlow0)) / fSlow84) - 1);
		float 	fSlow92 = (fSlow84 * ((1.0f + fSlow88) - fSlow89));
		float 	fSlow93 = expf((fConst45 / fSlow0));
		float 	fSlow94 = faustpower<2>(fSlow93);
		float 	fSlow95 = (1.0f - fSlow94);
		float 	fSlow96 = (1.0f - (fConst5 * fSlow94));
		float 	fSlow97 = sqrtf(max(0, ((faustpower<2>(fSlow96) / faustpower<2>(fSlow95)) - 1.0f)));
		float 	fSlow98 = (fSlow96 / fSlow95);
		float 	fSlow99 = (fSlow98 - fSlow97);
		float 	fSlow100 = ((expf((fConst46 / fSlow0)) / fSlow93) - 1);
		float 	fSlow101 = (fSlow93 * ((1.0f + fSlow97) - fSlow98));
		float 	fSlow102 = (0.5f * fSlow64);
		int 	iSlow103 = int((int((fConst18 * (fslider18 / fSlow30))) & 4095));
		float 	fSlow104 = expf((fConst52 / fSlow0));
		float 	fSlow105 = faustpower<2>(fSlow104);
		float 	fSlow106 = (1.0f - fSlow105);
		float 	fSlow107 = (1.0f - (fConst5 * fSlow105));
		float 	fSlow108 = sqrtf(max(0, ((faustpower<2>(fSlow107) / faustpower<2>(fSlow106)) - 1.0f)));
		float 	fSlow109 = (fSlow107 / fSlow106);
		float 	fSlow110 = (fSlow109 - fSlow108);
		float 	fSlow111 = ((expf((fConst53 / fSlow0)) / fSlow104) - 1);
		float 	fSlow112 = (fSlow104 * ((1.0f + fSlow108) - fSlow109));
		float 	fSlow113 = expf((fConst59 / fSlow0));
		float 	fSlow114 = faustpower<2>(fSlow113);
		float 	fSlow115 = (1.0f - fSlow114);
		float 	fSlow116 = (1.0f - (fConst5 * fSlow114));
		float 	fSlow117 = sqrtf(max(0, ((faustpower<2>(fSlow116) / faustpower<2>(fSlow115)) - 1.0f)));
		float 	fSlow118 = (fSlow116 / fSlow115);
		float 	fSlow119 = (fSlow118 - fSlow117);
		float 	fSlow120 = ((expf((fConst60 / fSlow0)) / fSlow113) - 1);
		float 	fSlow121 = (fSlow113 * ((1.0f + fSlow117) - fSlow118));
		float 	fSlow122 = expf((fConst66 / fSlow0));
		float 	fSlow123 = faustpower<2>(fSlow122);
		float 	fSlow124 = (1.0f - fSlow123);
		float 	fSlow125 = (1.0f - (fConst5 * fSlow123));
		float 	fSlow126 = sqrtf(max(0, ((faustpower<2>(fSlow125) / faustpower<2>(fSlow124)) - 1.0f)));
		float 	fSlow127 = (fSlow125 / fSlow124);
		float 	fSlow128 = (fSlow127 - fSlow126);
		float 	fSlow129 = ((expf((fConst67 / fSlow0)) / fSlow122) - 1);
		float 	fSlow130 = (fSlow122 * ((1.0f + fSlow126) - fSlow127));
		FAUSTFLOAT* output0 = output[0];
		FAUSTFLOAT* output1 = output[1];
		for (int i=0; i<count; i++) {
			fRec11[0] = ((fConst9 * (fRec4[1] + fRec4[2])) + (fConst8 * fRec11[1]));
			fRec10[0] = ((fSlow9 * ((fSlow8 * fRec11[0]) + fRec4[1])) + (fSlow7 * fRec10[1]));
			fVec0[IOTA&8191] = (1e-20f + (0.35355339059327373f * fRec10[0]));
			fRec12[0] = (fSlow10 + (0.999f * fRec12[1]));
			iRec13[0] = (iSlow12 & (iRec13[1] | (fRec14[1] >= 1)));
			int iTemp0 = (iSlow13 & (fRec14[1] > 0));
			fRec14[0] = (((iTemp0 == 0) | (fRec14[1] >= 1e-06f)) * ((fSlow17 * (((iRec13[1] == 0) & iSlow12) & (fRec14[1] < 1))) + (fRec14[1] * (1 - (fSlow15 * iTemp0)))));
			float fTemp1 = (fSlow18 + fRec18[1]);
			fRec18[0] = (fTemp1 - floorf(fTemp1));
			iRec19[0] = (iSlow12 & (iRec19[1] | (fRec21[1] >= 1)));
			iRec20[0] = (iSlow12 * (1 + iRec20[1]));
			int iTemp2 = (iSlow13 & (fRec21[1] > 0));
			fRec21[0] = (((iTemp2 == 0) | (fRec21[1] >= 1e-06f)) * ((fSlow25 * ((1 - (iRec20[1] < fSlow23)) * ((((iRec19[1] == 0) & iSlow12) & (fRec21[1] < 1)) & (iRec20[1] > fSlow22)))) + (fRec21[1] * (1 - (fSlow20 * iTemp2)))));
			fRec23[0] = (fSlow27 + (0.999f * fRec23[1]));
			float fTemp3 = ((fConst16 * (fSlow31 + (iSlow29 * fRec23[0]))) + fRec22[1]);
			fRec22[0] = (fTemp3 - floorf(fTemp3));
			iRec24[0] = (iSlow12 & (iRec24[1] | (fRec25[1] >= 1)));
			int iTemp4 = (iSlow13 & (fRec25[1] > 0));
			fRec25[0] = (((iTemp4 == 0) | (fRec25[1] >= 1e-06f)) * ((fSlow33 * (((iRec24[1] == 0) & iSlow12) & (fRec25[1] < 1))) + (fRec25[1] * (1 - (fConst17 * iTemp4)))));
			fRec26[0] = (fSlow34 + (0.999f * fRec26[1]));
			float fTemp5 = (fRec26[0] * fRec25[0]);
			float fTemp6 = (3.141592653589793f * (fTemp5 * ftbl0[int((65536.0f * fRec22[0]))]));
			float fTemp7 = sinf(fTemp6);
			float fTemp8 = ((fSlow40 * fRec15[(IOTA-iSlow39)&8191]) + (fSlow38 * fRec15[(IOTA-iSlow37)&8191]));
			fVec1[0] = fTemp8;
			float fTemp9 = (0 - fTemp7);
			float fTemp10 = cosf(fTemp6);
			float fTemp11 = ((fVec1[0] * fTemp10) + (fTemp9 * fRec27[1]));
			float fTemp12 = ((fTemp10 * fTemp11) + (fTemp9 * fRec28[1]));
			float fTemp13 = ((fTemp10 * fTemp12) + (fTemp9 * fRec29[1]));
			float fTemp14 = ((fTemp10 * fTemp13) + (fTemp9 * fRec30[1]));
			float fTemp15 = ((fTemp10 * fTemp14) + (fTemp9 * fRec31[1]));
			fRec32[0] = ((fTemp10 * fTemp15) + (fTemp9 * fRec32[1]));
			fRec31[0] = ((fTemp10 * fRec32[1]) + (fTemp7 * fTemp15));
			fRec30[0] = ((fTemp10 * fRec31[1]) + (fTemp7 * fTemp14));
			fRec29[0] = ((fTemp10 * fRec30[1]) + (fTemp7 * fTemp13));
			fRec28[0] = ((fTemp10 * fRec29[1]) + (fTemp7 * fTemp12));
			fRec27[0] = ((fTemp10 * fRec28[1]) + (fTemp7 * fTemp11));
			float fTemp16 = (fTemp5 * (((fSlow44 * (fVec1[0] + fVec1[1])) + (fSlow43 * fVec1[0])) + (fSlow42 * faustpower<2>(fVec1[0]))));
			float fTemp17 = sinf(fTemp16);
			float fTemp18 = (0 - fTemp17);
			float fTemp19 = cosf(fTemp16);
			float fTemp20 = ((fVec1[0] * fTemp19) + (fTemp18 * fRec33[1]));
			float fTemp21 = ((fTemp19 * fTemp20) + (fTemp18 * fRec34[1]));
			float fTemp22 = ((fTemp19 * fTemp21) + (fTemp18 * fRec35[1]));
			float fTemp23 = ((fTemp19 * fTemp22) + (fTemp18 * fRec36[1]));
			float fTemp24 = ((fTemp19 * fTemp23) + (fTemp18 * fRec37[1]));
			fRec38[0] = ((fTemp19 * fTemp24) + (fTemp18 * fRec38[1]));
			fRec37[0] = ((fTemp19 * fRec38[1]) + (fTemp17 * fTemp24));
			fRec36[0] = ((fTemp19 * fRec37[1]) + (fTemp17 * fTemp23));
			fRec35[0] = ((fTemp19 * fRec36[1]) + (fTemp17 * fTemp22));
			fRec34[0] = ((fTemp19 * fRec35[1]) + (fTemp17 * fTemp21));
			fRec33[0] = ((fTemp19 * fRec34[1]) + (fTemp17 * fTemp20));
			float fTemp25 = (0.4f * ((iSlow45 * (((1 - fRec26[0]) * fVec1[0]) + (fRec26[0] * ((fTemp19 * fRec33[1]) + (fVec1[0] * fTemp17))))) + (iSlow41 * ((fTemp10 * fRec27[1]) + (fVec1[0] * fTemp7)))));
			iRec39[0] = (12345 + (1103515245 * iRec39[1]));
			fRec40[0] = (fSlow47 + (0.999f * fRec40[1]));
			iRec41[0] = (iSlow49 & (iRec41[1] | (fRec42[1] >= 1)));
			int iTemp26 = (iSlow50 & (fRec42[1] > 0));
			fRec42[0] = (((iTemp26 == 0) | (fRec42[1] >= 1e-06f)) * ((fSlow56 * (((iRec41[1] == 0) & iSlow49) & (fRec42[1] < 1))) + (fRec42[1] * ((1 - (fSlow54 * (iRec41[1] & (fRec42[1] > 90)))) - (fSlow52 * iTemp26)))));
			float fTemp27 = (((fRec42[0] * fRec40[0]) * (1.1f + (fSlow46 * iRec39[0]))) + (fTemp25 + (fSlow26 * (fRec21[0] * ftbl0[int((65536.0f * fRec18[0]))]))));
			fVec2[IOTA&4095] = fTemp27;
			float fTemp28 = (fSlow60 * fVec2[(IOTA-iSlow59)&4095]);
			float fTemp29 = (fSlow62 * fVec2[(IOTA-iSlow61)&4095]);
			float fTemp30 = ((fTemp28 + (fTemp25 + fTemp29)) - faustpower<3>((fTemp29 + fTemp28)));
			fVec3[0] = fTemp30;
			fRec16[0] = ((fConst19 * (fVec3[0] + fVec3[1])) + (fConst15 * fRec16[1]));
			fRec15[IOTA&8191] = fRec16[0];
			float fTemp31 = (fRec15[(IOTA-0)&8191] * fRec14[0]);
			fVec4[IOTA&4095] = (fSlow65 * (fTemp31 * fRec12[0]));
			float fTemp32 = (0.3f * fVec4[(IOTA-iConst20)&4095]);
			float fTemp33 = ((fTemp32 + fVec0[(IOTA-iConst12)&8191]) - (0.6f * fRec8[1]));
			fVec5[IOTA&2047] = fTemp33;
			fRec8[0] = fVec5[(IOTA-iConst21)&2047];
			float 	fRec9 = (0.6f * fVec5[IOTA&2047]);
			fRec46[0] = ((fConst9 * (fRec0[1] + fRec0[2])) + (fConst8 * fRec46[1]));
			fRec45[0] = ((fSlow74 * (fRec0[1] + (fSlow73 * fRec46[0]))) + (fSlow72 * fRec45[1]));
			fVec6[IOTA&8191] = (1e-20f + (0.35355339059327373f * fRec45[0]));
			float fTemp34 = ((fTemp32 + fVec6[(IOTA-iConst27)&8191]) - (0.6f * fRec43[1]));
			fVec7[IOTA&1023] = fTemp34;
			fRec43[0] = fVec7[(IOTA-iConst28)&1023];
			float 	fRec44 = (0.6f * fVec7[IOTA&1023]);
			float fTemp35 = (fRec44 + fRec9);
			fRec50[0] = ((fConst9 * (fRec2[1] + fRec2[2])) + (fConst8 * fRec50[1]));
			fRec49[0] = ((fSlow83 * (fRec2[1] + (fSlow82 * fRec50[0]))) + (fSlow81 * fRec49[1]));
			fVec8[IOTA&8191] = (1e-20f + (0.35355339059327373f * fRec49[0]));
			float fTemp36 = (fVec8[(IOTA-iConst34)&8191] - (fTemp32 + (0.6f * fRec47[1])));
			fVec9[IOTA&2047] = fTemp36;
			fRec47[0] = fVec9[(IOTA-iConst35)&2047];
			float 	fRec48 = (0.6f * fVec9[IOTA&2047]);
			fRec54[0] = ((fConst9 * (fRec6[1] + fRec6[2])) + (fConst8 * fRec54[1]));
			fRec53[0] = ((fSlow92 * (fRec6[1] + (fSlow91 * fRec54[0]))) + (fSlow90 * fRec53[1]));
			fVec10[IOTA&8191] = (1e-20f + (0.35355339059327373f * fRec53[0]));
			float fTemp37 = (fVec10[(IOTA-iConst41)&8191] - (fTemp32 + (0.6f * fRec51[1])));
			fVec11[IOTA&1023] = fTemp37;
			fRec51[0] = fVec11[(IOTA-iConst42)&1023];
			float 	fRec52 = (0.6f * fVec11[IOTA&1023]);
			float fTemp38 = (fRec52 + (fRec48 + fTemp35));
			fRec58[0] = ((fConst9 * (fRec1[1] + fRec1[2])) + (fConst8 * fRec58[1]));
			fRec57[0] = ((fSlow101 * (fRec1[1] + (fSlow100 * fRec58[0]))) + (fSlow99 * fRec57[1]));
			fVec12[IOTA&16383] = (1e-20f + (0.35355339059327373f * fRec57[0]));
			fVec13[IOTA&4095] = (fSlow102 * fTemp31);
			float fTemp39 = fVec13[(IOTA-iSlow103)&4095];
			fVec14[IOTA&4095] = (fSlow63 * (fRec12[0] * fTemp39));
			float fTemp40 = (0.3f * fVec14[(IOTA-iConst20)&4095]);
			float fTemp41 = ((fTemp40 + fVec12[(IOTA-iConst48)&16383]) + (0.6f * fRec55[1]));
			fVec15[IOTA&2047] = fTemp41;
			fRec55[0] = fVec15[(IOTA-iConst49)&2047];
			float 	fRec56 = (0 - (0.6f * fVec15[IOTA&2047]));
			fRec62[0] = ((fConst9 * (fRec5[1] + fRec5[2])) + (fConst8 * fRec62[1]));
			fRec61[0] = ((fSlow112 * (fRec5[1] + (fSlow111 * fRec62[0]))) + (fSlow110 * fRec61[1]));
			fVec16[IOTA&8191] = (1e-20f + (0.35355339059327373f * fRec61[0]));
			float fTemp42 = ((fTemp40 + fVec16[(IOTA-iConst55)&8191]) + (0.6f * fRec59[1]));
			fVec17[IOTA&2047] = fTemp42;
			fRec59[0] = fVec17[(IOTA-iConst56)&2047];
			float 	fRec60 = (0 - (0.6f * fVec17[IOTA&2047]));
			fRec66[0] = ((fConst9 * (fRec3[1] + fRec3[2])) + (fConst8 * fRec66[1]));
			fRec65[0] = ((fSlow121 * (fRec3[1] + (fSlow120 * fRec66[0]))) + (fSlow119 * fRec65[1]));
			fVec18[IOTA&16383] = (1e-20f + (0.35355339059327373f * fRec65[0]));
			float fTemp43 = ((fVec18[(IOTA-iConst62)&16383] + (0.6f * fRec63[1])) - fTemp40);
			fVec19[IOTA&2047] = fTemp43;
			fRec63[0] = fVec19[(IOTA-iConst63)&2047];
			float 	fRec64 = (0 - (0.6f * fVec19[IOTA&2047]));
			fRec70[0] = ((fConst9 * (fRec7[1] + fRec7[2])) + (fConst8 * fRec70[1]));
			fRec69[0] = ((fSlow130 * (fRec7[1] + (fSlow129 * fRec70[0]))) + (fSlow128 * fRec69[1]));
			fVec20[IOTA&16383] = (1e-20f + (0.35355339059327373f * fRec69[0]));
			float fTemp44 = ((fVec20[(IOTA-iConst69)&16383] + (0.6f * fRec67[1])) - fTemp40);
			fVec21[IOTA&1023] = fTemp44;
			fRec67[0] = fVec21[(IOTA-iConst70)&1023];
			float 	fRec68 = (0 - (0.6f * fVec21[IOTA&1023]));
			fRec0[0] = (fRec43[1] + (fRec8[1] + (fRec47[1] + (fRec51[1] + (fRec55[1] + (fRec59[1] + (fRec63[1] + (fRec67[1] + (fRec68 + (fRec64 + (fRec60 + (fRec56 + fTemp38))))))))))));
			fRec1[0] = (0 - ((fRec55[1] + (fRec59[1] + (fRec63[1] + (fRec67[1] + (fRec68 + (fRec64 + (fRec56 + fRec60))))))) - (fRec43[1] + (fRec8[1] + (fRec47[1] + (fRec51[1] + fTemp38))))));
			float fTemp45 = (fRec48 + fRec52);
			fRec2[0] = (0 - ((fRec47[1] + (fRec51[1] + (fRec63[1] + (fRec67[1] + (fRec68 + (fRec64 + fTemp45)))))) - (fRec43[1] + (fRec8[1] + (fRec55[1] + (fRec59[1] + (fRec60 + (fRec56 + fTemp35))))))));
			fRec3[0] = (0 - ((fRec47[1] + (fRec51[1] + (fRec55[1] + (fRec59[1] + (fRec60 + (fRec56 + fTemp45)))))) - (fRec43[1] + (fRec8[1] + (fRec63[1] + (fRec67[1] + (fRec68 + (fRec64 + fTemp35))))))));
			float fTemp46 = (fRec44 + fRec48);
			float fTemp47 = (fRec9 + fRec52);
			fRec4[0] = (0 - ((fRec8[1] + (fRec51[1] + (fRec59[1] + (fRec67[1] + (fRec68 + (fRec60 + fTemp47)))))) - (fRec43[1] + (fRec47[1] + (fRec55[1] + (fRec63[1] + (fRec64 + (fRec56 + fTemp46))))))));
			fRec5[0] = (0 - ((fRec8[1] + (fRec51[1] + (fRec55[1] + (fRec63[1] + (fRec64 + (fRec56 + fTemp47)))))) - (fRec43[1] + (fRec47[1] + (fRec59[1] + (fRec67[1] + (fRec68 + (fRec60 + fTemp46))))))));
			float fTemp48 = (fRec44 + fRec52);
			float fTemp49 = (fRec9 + fRec48);
			fRec6[0] = (0 - ((fRec8[1] + (fRec47[1] + (fRec59[1] + (fRec63[1] + (fRec64 + (fRec60 + fTemp49)))))) - (fRec43[1] + (fRec51[1] + (fRec55[1] + (fRec67[1] + (fRec68 + (fRec56 + fTemp48))))))));
			fRec7[0] = (0 - ((fRec8[1] + (fRec47[1] + (fRec55[1] + (fRec67[1] + (fRec68 + (fRec56 + fTemp49)))))) - (fRec43[1] + (fRec51[1] + (fRec59[1] + (fRec63[1] + (fRec64 + (fRec60 + fTemp48))))))));
			float fTemp50 = (1 - fRec12[0]);
			output0[i] = (FAUSTFLOAT)((fSlow65 * (fTemp31 * fTemp50)) + (0.37f * (fRec1[0] + fRec2[0])));
			output1[i] = (FAUSTFLOAT)((fSlow63 * (fTemp50 * fTemp39)) + (0.37f * (fRec1[0] - fRec2[0])));
			// post processing
			fRec7[2] = fRec7[1]; fRec7[1] = fRec7[0];
			fRec6[2] = fRec6[1]; fRec6[1] = fRec6[0];
			fRec5[2] = fRec5[1]; fRec5[1] = fRec5[0];
			fRec4[2] = fRec4[1]; fRec4[1] = fRec4[0];
			fRec3[2] = fRec3[1]; fRec3[1] = fRec3[0];
			fRec2[2] = fRec2[1]; fRec2[1] = fRec2[0];
			fRec1[2] = fRec1[1]; fRec1[1] = fRec1[0];
			fRec0[2] = fRec0[1]; fRec0[1] = fRec0[0];
			fRec67[1] = fRec67[0];
			fRec69[1] = fRec69[0];
			fRec70[1] = fRec70[0];
			fRec63[1] = fRec63[0];
			fRec65[1] = fRec65[0];
			fRec66[1] = fRec66[0];
			fRec59[1] = fRec59[0];
			fRec61[1] = fRec61[0];
			fRec62[1] = fRec62[0];
			fRec55[1] = fRec55[0];
			fRec57[1] = fRec57[0];
			fRec58[1] = fRec58[0];
			fRec51[1] = fRec51[0];
			fRec53[1] = fRec53[0];
			fRec54[1] = fRec54[0];
			fRec47[1] = fRec47[0];
			fRec49[1] = fRec49[0];
			fRec50[1] = fRec50[0];
			fRec43[1] = fRec43[0];
			fRec45[1] = fRec45[0];
			fRec46[1] = fRec46[0];
			fRec8[1] = fRec8[0];
			fRec16[1] = fRec16[0];
			fVec3[1] = fVec3[0];
			fRec42[1] = fRec42[0];
			iRec41[1] = iRec41[0];
			fRec40[1] = fRec40[0];
			iRec39[1] = iRec39[0];
			fRec33[1] = fRec33[0];
			fRec34[1] = fRec34[0];
			fRec35[1] = fRec35[0];
			fRec36[1] = fRec36[0];
			fRec37[1] = fRec37[0];
			fRec38[1] = fRec38[0];
			fRec27[1] = fRec27[0];
			fRec28[1] = fRec28[0];
			fRec29[1] = fRec29[0];
			fRec30[1] = fRec30[0];
			fRec31[1] = fRec31[0];
			fRec32[1] = fRec32[0];
			fVec1[1] = fVec1[0];
			fRec26[1] = fRec26[0];
			fRec25[1] = fRec25[0];
			iRec24[1] = iRec24[0];
			fRec22[1] = fRec22[0];
			fRec23[1] = fRec23[0];
			fRec21[1] = fRec21[0];
			iRec20[1] = iRec20[0];
			iRec19[1] = iRec19[0];
			fRec18[1] = fRec18[0];
			fRec14[1] = fRec14[0];
			iRec13[1] = iRec13[0];
			fRec12[1] = fRec12[0];
			IOTA = IOTA+1;
			fRec10[1] = fRec10[0];
			fRec11[1] = fRec11[0];
		}
	}
};


float 	my_dsp::ftbl0[65536];



#include "../common/nsmtracker.h"
#include "SoundPlugin.h"
#include "SoundPlugin_proc.h"

#include "Mixer_proc.h"
#include "SoundPluginRegistry_proc.h"

class MyUI;


class MyUI : public UI
{

 public:

  MyUI() 
    : _num_effects(0)
    , _effect_tooltip("")
  {	}

  ~MyUI() {	}

  struct Controller{
    float* control_port;

    float min_value;
    float default_value;
    float max_value;

    const char *name;
    int type;

    const char *tooltip;
    const char *unit;
  };

  std::vector<Controller> _controllers;

  int get_controller_num(float *control_port){
    for(unsigned int i=0;i<_controllers.size();i++){
      if(control_port == _controllers.at(i).control_port)
        return i;
    }

    Controller controller;
    controller.control_port = control_port;
    controller.min_value = 0.0f;
    controller.default_value = 0.5f;
    controller.max_value = 1.0f;
    controller.name = "<no name>";
    controller.type = EFFECT_FORMAT_FLOAT;
    controller.tooltip = "";
    controller.unit = "";

    _controllers.push_back(controller);
    _num_effects++;

    return _controllers.size()-1;
  }

  // We don't use passive items.
  void remove_last_item(){
    _controllers.pop_back();
    _num_effects--;
  }
  

  int _num_effects;

  const char *_effect_tooltip;


  // -- widget's layouts
  
  void openFrameBox(const char* label) {}
  void openTabBox(const char* label) {}
  void openHorizontalBox(const char* label) {}
  void openVerticalBox(const char* label) {}
  void closeBox() {}
  
  // -- active widgets

  void addEffect(const char *name, float* control_port, int type, float min_value, float default_value, float max_value){
    int effect_num = get_controller_num(control_port);

    Controller *controller = &_controllers.at(effect_num);

    controller->name = name;
    controller->type = type;
    controller->min_value = min_value;
    controller->default_value = default_value;
    controller->max_value = max_value;
  }

  void addButton(const char* label, float* zone) {
    addEffect(label, zone, EFFECT_FORMAT_BOOL, 0, 0, 1);
  }
  void addToggleButton(const char* label, float* zone) {
    addEffect(label, zone, EFFECT_FORMAT_BOOL, 0, 0, 1);
  }
  void addCheckButton(const char* label, float* zone) {
    addEffect(label, zone, EFFECT_FORMAT_BOOL, 0, 0, 1);
  }
  void addVerticalSlider(const char* label, float* zone, float init, float min, float max, float step) {
    addEffect(label, zone, EFFECT_FORMAT_FLOAT, min, init, max);
  }
  void addHorizontalSlider(const char* label, float* zone, float init, float min, float max, float step) {
    addEffect(label, zone, EFFECT_FORMAT_FLOAT, min, init, max);
  }
  void addNumEntry(const char* label, float* zone, float init, float min, float max, float step) {
    addEffect(label, zone, EFFECT_FORMAT_FLOAT, min, init, max); // The INT format is not supported.
  }
  
  // -- passive widgets

  void addNumDisplay(const char* label, float* zone, int precision) {remove_last_item();}
  void addTextDisplay(const char* label, float* zone, const char* names[], float min, float max) {remove_last_item();}
  void addHorizontalBargraph(const char* label, float* zone, float min, float max) {remove_last_item();}
  void addVerticalBargraph(const char* label, float* zone, float min, float max) {remove_last_item();}
  
  // -- metadata declarations
  
  void declare(float* control_port, const char* key, const char* value) {
    if(control_port==NULL){
      if(!strcmp(key,"tooltip"))
        _effect_tooltip = value;
    } else {
      int effect_num = get_controller_num(control_port);
      Controller *controller = &_controllers.at(effect_num);
      if(!strcmp(key,"tooltip"))
        controller->tooltip = value;
      else if(!strcmp(key,"unit"))
        controller->unit = value;
    }
  }
};

struct Data{
  dsp *dsp_instance;
  MyUI myUI;
};

static void RT_process(SoundPlugin *plugin, int64_t time, int num_frames, float **inputs, float **outputs){
  //SoundPluginType *type = plugin->type;
  Data *data = (Data*)plugin->data;
  data->dsp_instance->compute(num_frames, inputs, outputs);
  //printf("in00: %f, in10: %f\n",inputs[0][0],inputs[1][0]);
  //printf("out00: %f, out10: %f\n",outputs[0][0],outputs[1][0]);
}

static void *create_plugin_data(const SoundPluginType *plugin_type, struct SoundPlugin *plugin, float samplerate, int blocksize){
  Data *data = new Data;
  data->dsp_instance = new my_dsp;
  data->dsp_instance->init(samplerate);
  data->dsp_instance->buildUserInterface(&data->myUI);
  
  return data;
}

static void cleanup_plugin_data(SoundPlugin *plugin){
  Data *data = (Data*)plugin->data;
  delete data->dsp_instance;
  delete data;
}

static const char *get_effect_name(const struct SoundPluginType *plugin_type, int effect_num){
  Data *data = (Data*)plugin_type->data;
  MyUI::Controller *controller = &data->myUI._controllers.at(effect_num);
  return controller->name;
}

static void set_effect_value(struct SoundPlugin *plugin, int64_t time, int effect_num, float value){
  Data *data = (Data*)plugin->data;
  MyUI::Controller *controller = &data->myUI._controllers.at(effect_num);

  float min = controller->min_value;
  float max = controller->max_value;

  *(controller->control_port) = scale(value,0,1,min,max);
}

static float get_effect_value(struct SoundPlugin *plugin, int effect_num){

  Data *data = (Data*)plugin->data;
  MyUI::Controller *controller = &data->myUI._controllers.at(effect_num);

  float min = controller->min_value;
  float max = controller->max_value;

  return scale(*(controller->control_port),min,max,0.0f,1.0f);
}

static void get_display_value_string(struct SoundPlugin *plugin, int effect_num, char *buffer, int buffersize){
  Data *data = (Data*)plugin->data;
  MyUI::Controller *controller = &data->myUI._controllers.at(effect_num);

  snprintf(buffer,buffersize-1,"%f %s",*(controller->control_port), controller->unit);
}

static const char *get_effect_description(const struct SoundPluginType *plugin_type, int effect_num){
  Data *data = (Data*)plugin_type->data;
  MyUI::Controller *controller = &data->myUI._controllers.at(effect_num);

  return controller->tooltip;
}

static int get_effect_num(const struct SoundPluginType *plugin_type, const char *effect_name){
  Data *data = (Data*)plugin_type->data;
  for(unsigned int i=0;i<data->myUI._controllers.size();i++){
    MyUI::Controller *controller = &data->myUI._controllers.at(i);
    if(!strcmp(controller->name,effect_name))
      return i;
  }
  RError("Couldn't find effect name \"%s\" in plugin %s/%s\n",plugin_type->type_name,plugin_type->name);
  return 0;
}

static void fill_type(SoundPluginType *type){
 type->type_name                = "Faust";
 type->is_instrument            = false;
 type->note_handling_is_RT      = false;
 type->get_effect_format        = NULL;
 type->get_effect_name          = get_effect_name;
 type->effect_is_RT             = NULL;
 type->create_plugin_data       = create_plugin_data;
 type->cleanup_plugin_data      = cleanup_plugin_data;

 type->RT_process       = RT_process;
 type->set_effect_value = set_effect_value;
 type->get_effect_value = get_effect_value;
 type->get_display_value_string = get_display_value_string;
 type->get_effect_description = get_effect_description;

 type->get_effect_num = get_effect_num;

 type->data                     = NULL;
};

static SoundPluginType faust_type = {0};

void create_faust_plugins(void){
  fill_type(&faust_type);

  Data *data = (Data*)create_plugin_data(&faust_type, NULL, MIXER_get_sample_rate(), MIXER_get_buffer_size());
  faust_type.data = data;
  faust_type.num_effects = data->myUI._num_effects;

  faust_type.name = DSP_NAME;

  faust_type.num_inputs = data->dsp_instance->getNumInputs();
  faust_type.num_outputs = data->dsp_instance->getNumOutputs();


  PR_add_plugin_type(&faust_type);
}
