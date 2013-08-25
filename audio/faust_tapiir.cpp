//-----------------------------------------------------
// name: "tapiir"
// version: "1.0"
// author: "Grame"
// license: "BSD"
// copyright: "(c)GRAME 2006"
//
// Code generated with Faust 0.9.55 (http://faust.grame.fr)
//-----------------------------------------------------
/* link with  */
#include <math.h>
#include <math.h>
#include <string>

/*
#include "/usr/share/faust/audio/dsp.h"
#include "/usr/share/faust/gui/UI.h"
*/

#include "faudiostream/architecture/faust/audio/dsp.h"
#include "faudiostream/architecture/faust/gui/UI.h"

#include "../common/nsmtracker.h"

struct Meta
{
    void declare (const char* key, const char* value) { }
};

static void RT_fade_out(float *sound, int num_frames){
  float num_frames_plus_1 = num_frames+1.0f;
  for(int i=0;i<num_frames;i++)
    sound[i] *= (num_frames-i)/num_frames_plus_1;
}


#if 0
static float linear2db(float val){
  if(val<=0.0f)
    return 0.0f;

  float db = 20*log10(val);
  if(db<-70)
    return 0.0f;
  else if(db>40)
    return 1.0f;
  else
    return scale(db,-70,40,0,1);
}
#endif

// input is between 0 and 1.
// output is between 0 and 1.
static float velocity2gain(float val){
  if(val<=0.0f)
    return 0.0f;
  else if(val>=1.0f)
    return 1.0f;
  else
    return powf(10, scale(val,0.0, 1.0 ,-40, 20) / 20.0f) / 10.0f;
}


static double midi_to_hz(float midi){
  if(midi<=0)
    return 0;
  else
    //  return 1;
  return 8.17579891564*(expf(.0577622650*midi));
}

#if 0
// For some reason, it won't compile with the usual min/max macros.
template<typename T> static inline T min(T a,T b){return a<b ? a : b;}
template<typename T> static inline T max(T a,T b){return a>b ? a : b;}
static inline float min(float a,int b){return a<b ? a : b;}
static inline float max(float a,int b){return a>b ? a : b;}
static inline float min(int a,float b){return a<b ? a : b;}
static inline float max(int a,float b){return a>b ? a : b;}
#endif

inline int 	max (unsigned int a, unsigned int b) { return (a>b) ? a : b; }
inline int 	max (int a, int b)		{ return (a>b) ? a : b; }

inline long 	max (long a, long b) 		{ return (a>b) ? a : b; }
inline long 	max (int a, long b) 		{ return (a>b) ? a : b; }
inline long 	max (long a, int b) 		{ return (a>b) ? a : b; }

inline float 	max (float a, float b) 		{ return (a>b) ? a : b; }
inline float 	max (int a, float b) 		{ return (a>b) ? a : b; }
inline float 	max (float a, int b) 		{ return (a>b) ? a : b; }
inline float 	max (long a, float b) 		{ return (a>b) ? a : b; }
inline float 	max (float a, long b) 		{ return (a>b) ? a : b; }

inline double 	max (double a, double b) 	{ return (a>b) ? a : b; }
inline double 	max (int a, double b) 		{ return (a>b) ? a : b; }
inline double 	max (double a, int b) 		{ return (a>b) ? a : b; }
inline double 	max (long a, double b) 		{ return (a>b) ? a : b; }
inline double 	max (double a, long b) 		{ return (a>b) ? a : b; }
inline double 	max (float a, double b) 	{ return (a>b) ? a : b; }
inline double 	max (double a, float b) 	{ return (a>b) ? a : b; }


inline int	min (int a, int b)		{ return (a<b) ? a : b; }

inline long 	min (long a, long b) 		{ return (a<b) ? a : b; }
inline long 	min (int a, long b) 		{ return (a<b) ? a : b; }
inline long 	min (long a, int b) 		{ return (a<b) ? a : b; }

inline float 	min (float a, float b) 		{ return (a<b) ? a : b; }
inline float 	min (int a, float b) 		{ return (a<b) ? a : b; }
inline float 	min (float a, int b) 		{ return (a<b) ? a : b; }
inline float 	min (long a, float b) 		{ return (a<b) ? a : b; }
inline float 	min (float a, long b) 		{ return (a<b) ? a : b; }

inline double 	min (double a, double b) 	{ return (a<b) ? a : b; }
inline double 	min (int a, double b) 		{ return (a<b) ? a : b; }
inline double 	min (double a, int b) 		{ return (a<b) ? a : b; }
inline double 	min (long a, double b) 		{ return (a<b) ? a : b; }
inline double 	min (double a, long b) 		{ return (a<b) ? a : b; }
inline double 	min (float a, double b) 	{ return (a<b) ? a : b; }
inline double 	min (double a, float b) 	{ return (a<b) ? a : b; }


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

#ifndef FAUSTCLASS 
#define FAUSTCLASS Tapiir_dsp
#endif

class Tapiir_dsp : public dsp {
  private:
	FAUSTFLOAT 	fslider0;
	FAUSTFLOAT 	fslider1;
	FAUSTFLOAT 	fslider2;
	FAUSTFLOAT 	fslider3;
	FAUSTFLOAT 	fslider4;
	FAUSTFLOAT 	fslider5;
	FAUSTFLOAT 	fslider6;
	FAUSTFLOAT 	fslider7;
	FAUSTFLOAT 	fslider8;
	FAUSTFLOAT 	fslider9;
	FAUSTFLOAT 	fslider10;
	int 	IOTA;
	float 	fVec0[524288];
	FAUSTFLOAT 	fslider11;
	int 	iConst0;
	float 	fRec0[2];
	FAUSTFLOAT 	fslider12;
	FAUSTFLOAT 	fslider13;
	FAUSTFLOAT 	fslider14;
	FAUSTFLOAT 	fslider15;
	FAUSTFLOAT 	fslider16;
	FAUSTFLOAT 	fslider17;
	FAUSTFLOAT 	fslider18;
	FAUSTFLOAT 	fslider19;
	FAUSTFLOAT 	fslider20;
	float 	fVec1[524288];
	FAUSTFLOAT 	fslider21;
	float 	fRec1[2];
	FAUSTFLOAT 	fslider22;
	FAUSTFLOAT 	fslider23;
	FAUSTFLOAT 	fslider24;
	FAUSTFLOAT 	fslider25;
	FAUSTFLOAT 	fslider26;
	FAUSTFLOAT 	fslider27;
	FAUSTFLOAT 	fslider28;
	FAUSTFLOAT 	fslider29;
	FAUSTFLOAT 	fslider30;
	float 	fVec2[524288];
	FAUSTFLOAT 	fslider31;
	float 	fRec2[2];
	FAUSTFLOAT 	fslider32;
	FAUSTFLOAT 	fslider33;
	FAUSTFLOAT 	fslider34;
	FAUSTFLOAT 	fslider35;
	FAUSTFLOAT 	fslider36;
	FAUSTFLOAT 	fslider37;
	FAUSTFLOAT 	fslider38;
	FAUSTFLOAT 	fslider39;
	FAUSTFLOAT 	fslider40;
	float 	fVec3[524288];
	FAUSTFLOAT 	fslider41;
	float 	fRec3[2];
	FAUSTFLOAT 	fslider42;
	FAUSTFLOAT 	fslider43;
	FAUSTFLOAT 	fslider44;
	FAUSTFLOAT 	fslider45;
	FAUSTFLOAT 	fslider46;
	FAUSTFLOAT 	fslider47;
	FAUSTFLOAT 	fslider48;
	FAUSTFLOAT 	fslider49;
	FAUSTFLOAT 	fslider50;
	float 	fVec4[524288];
	FAUSTFLOAT 	fslider51;
	float 	fRec4[2];
	FAUSTFLOAT 	fslider52;
	FAUSTFLOAT 	fslider53;
	FAUSTFLOAT 	fslider54;
	FAUSTFLOAT 	fslider55;
	FAUSTFLOAT 	fslider56;
	FAUSTFLOAT 	fslider57;
	FAUSTFLOAT 	fslider58;
	FAUSTFLOAT 	fslider59;
	FAUSTFLOAT 	fslider60;
	float 	fVec5[524288];
	FAUSTFLOAT 	fslider61;
	float 	fRec5[2];
	FAUSTFLOAT 	fslider62;
	FAUSTFLOAT 	fslider63;
	FAUSTFLOAT 	fslider64;
	FAUSTFLOAT 	fslider65;
	FAUSTFLOAT 	fslider66;
	FAUSTFLOAT 	fslider67;
	FAUSTFLOAT 	fslider68;
	FAUSTFLOAT 	fslider69;
	FAUSTFLOAT 	fslider70;
	FAUSTFLOAT 	fslider71;
	FAUSTFLOAT 	fslider72;
	FAUSTFLOAT 	fslider73;
	FAUSTFLOAT 	fslider74;
	FAUSTFLOAT 	fslider75;
	FAUSTFLOAT 	fslider76;
	FAUSTFLOAT 	fslider77;
  public:
	static void metadata(Meta* m) 	{ 
		m->declare("name", "tapiir");
		m->declare("version", "1.0");
		m->declare("author", "Grame");
		m->declare("license", "BSD");
		m->declare("copyright", "(c)GRAME 2006");
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
	}

	virtual int getNumInputs() 	{ return 2; }
	virtual int getNumOutputs() 	{ return 2; }
	static void classInit(int samplingFreq) {
	}
	virtual void instanceInit(int samplingFreq) {
		fSamplingFreq = samplingFreq;
		fslider0 = 1.0f;
		fslider1 = 1.0f;
		fslider2 = 1.0f;
		fslider3 = 1.0f;
		fslider4 = 0.0f;
		fslider5 = 0.0f;
		fslider6 = 0.0f;
		fslider7 = 0.0f;
		fslider8 = 0.0f;
		fslider9 = 0.0f;
		fslider10 = 1.0f;
		IOTA = 0;
		for (int i=0; i<524288; i++) fVec0[i] = 0;
		fslider11 = 0.0f;
		iConst0 = min(192000, max(1, fSamplingFreq));
		for (int i=0; i<2; i++) fRec0[i] = 0;
		fslider12 = 1.0f;
		fslider13 = 1.0f;
		fslider14 = 0.0f;
		fslider15 = 0.0f;
		fslider16 = 0.0f;
		fslider17 = 0.0f;
		fslider18 = 0.0f;
		fslider19 = 0.0f;
		fslider20 = 1.0f;
		for (int i=0; i<524288; i++) fVec1[i] = 0;
		fslider21 = 0.0f;
		for (int i=0; i<2; i++) fRec1[i] = 0;
		fslider22 = 1.0f;
		fslider23 = 1.0f;
		fslider24 = 0.0f;
		fslider25 = 0.0f;
		fslider26 = 0.0f;
		fslider27 = 0.0f;
		fslider28 = 0.0f;
		fslider29 = 0.0f;
		fslider30 = 1.0f;
		for (int i=0; i<524288; i++) fVec2[i] = 0;
		fslider31 = 0.0f;
		for (int i=0; i<2; i++) fRec2[i] = 0;
		fslider32 = 1.0f;
		fslider33 = 1.0f;
		fslider34 = 0.0f;
		fslider35 = 0.0f;
		fslider36 = 0.0f;
		fslider37 = 0.0f;
		fslider38 = 0.0f;
		fslider39 = 0.0f;
		fslider40 = 1.0f;
		for (int i=0; i<524288; i++) fVec3[i] = 0;
		fslider41 = 0.0f;
		for (int i=0; i<2; i++) fRec3[i] = 0;
		fslider42 = 1.0f;
		fslider43 = 1.0f;
		fslider44 = 0.0f;
		fslider45 = 0.0f;
		fslider46 = 0.0f;
		fslider47 = 0.0f;
		fslider48 = 0.0f;
		fslider49 = 0.0f;
		fslider50 = 1.0f;
		for (int i=0; i<524288; i++) fVec4[i] = 0;
		fslider51 = 0.0f;
		for (int i=0; i<2; i++) fRec4[i] = 0;
		fslider52 = 1.0f;
		fslider53 = 1.0f;
		fslider54 = 0.0f;
		fslider55 = 0.0f;
		fslider56 = 0.0f;
		fslider57 = 0.0f;
		fslider58 = 0.0f;
		fslider59 = 0.0f;
		fslider60 = 1.0f;
		for (int i=0; i<524288; i++) fVec5[i] = 0;
		fslider61 = 0.0f;
		for (int i=0; i<2; i++) fRec5[i] = 0;
		fslider62 = 0.0f;
		fslider63 = 0.0f;
		fslider64 = 0.0f;
		fslider65 = 0.0f;
		fslider66 = 0.0f;
		fslider67 = 0.0f;
		fslider68 = 1.0f;
		fslider69 = 1.0f;
		fslider70 = 1.0f;
		fslider71 = 0.0f;
		fslider72 = 0.0f;
		fslider73 = 0.0f;
		fslider74 = 0.0f;
		fslider75 = 0.0f;
		fslider76 = 0.0f;
		fslider77 = 1.0f;
	}
	virtual void init(int samplingFreq) {
		classInit(samplingFreq);
		instanceInit(samplingFreq);
	}
	virtual void buildUserInterface(UI* interface) {
		interface->openVerticalBox("Tapiir");
		interface->openTabBox("");
		interface->openHorizontalBox("Tap 0");
		interface->addVerticalSlider("delay (sec)", &fslider11, 0.0f, 0.0f, 5.0f, 0.01f);
		interface->addVerticalSlider("gain", &fslider10, 1.0f, 0.0f, 1.0f, 0.1f);
		interface->addVerticalSlider("input 0", &fslider2, 1.0f, 0.0f, 1.0f, 0.1f);
		interface->addVerticalSlider("input 1", &fslider3, 1.0f, 0.0f, 1.0f, 0.1f);
		interface->addVerticalSlider("tap 0", &fslider9, 0.0f, 0.0f, 1.0f, 0.1f);
		interface->addVerticalSlider("tap 1", &fslider8, 0.0f, 0.0f, 1.0f, 0.1f);
		interface->addVerticalSlider("tap 2", &fslider7, 0.0f, 0.0f, 1.0f, 0.1f);
		interface->addVerticalSlider("tap 3", &fslider6, 0.0f, 0.0f, 1.0f, 0.1f);
		interface->addVerticalSlider("tap 4", &fslider5, 0.0f, 0.0f, 1.0f, 0.1f);
		interface->addVerticalSlider("tap 5", &fslider4, 0.0f, 0.0f, 1.0f, 0.1f);
		interface->closeBox();
		interface->openHorizontalBox("Tap 1");
		interface->addVerticalSlider("delay (sec)", &fslider21, 0.0f, 0.0f, 5.0f, 0.01f);
		interface->addVerticalSlider("gain", &fslider20, 1.0f, 0.0f, 1.0f, 0.1f);
		interface->addVerticalSlider("input 0", &fslider12, 1.0f, 0.0f, 1.0f, 0.1f);
		interface->addVerticalSlider("input 1", &fslider13, 1.0f, 0.0f, 1.0f, 0.1f);
		interface->addVerticalSlider("tap 0", &fslider19, 0.0f, 0.0f, 1.0f, 0.1f);
		interface->addVerticalSlider("tap 1", &fslider18, 0.0f, 0.0f, 1.0f, 0.1f);
		interface->addVerticalSlider("tap 2", &fslider17, 0.0f, 0.0f, 1.0f, 0.1f);
		interface->addVerticalSlider("tap 3", &fslider16, 0.0f, 0.0f, 1.0f, 0.1f);
		interface->addVerticalSlider("tap 4", &fslider15, 0.0f, 0.0f, 1.0f, 0.1f);
		interface->addVerticalSlider("tap 5", &fslider14, 0.0f, 0.0f, 1.0f, 0.1f);
		interface->closeBox();
		interface->openHorizontalBox("Tap 2");
		interface->addVerticalSlider("delay (sec)", &fslider31, 0.0f, 0.0f, 5.0f, 0.01f);
		interface->addVerticalSlider("gain", &fslider30, 1.0f, 0.0f, 1.0f, 0.1f);
		interface->addVerticalSlider("input 0", &fslider22, 1.0f, 0.0f, 1.0f, 0.1f);
		interface->addVerticalSlider("input 1", &fslider23, 1.0f, 0.0f, 1.0f, 0.1f);
		interface->addVerticalSlider("tap 0", &fslider29, 0.0f, 0.0f, 1.0f, 0.1f);
		interface->addVerticalSlider("tap 1", &fslider28, 0.0f, 0.0f, 1.0f, 0.1f);
		interface->addVerticalSlider("tap 2", &fslider27, 0.0f, 0.0f, 1.0f, 0.1f);
		interface->addVerticalSlider("tap 3", &fslider26, 0.0f, 0.0f, 1.0f, 0.1f);
		interface->addVerticalSlider("tap 4", &fslider25, 0.0f, 0.0f, 1.0f, 0.1f);
		interface->addVerticalSlider("tap 5", &fslider24, 0.0f, 0.0f, 1.0f, 0.1f);
		interface->closeBox();
		interface->openHorizontalBox("Tap 3");
		interface->addVerticalSlider("delay (sec)", &fslider41, 0.0f, 0.0f, 5.0f, 0.01f);
		interface->addVerticalSlider("gain", &fslider40, 1.0f, 0.0f, 1.0f, 0.1f);
		interface->addVerticalSlider("input 0", &fslider32, 1.0f, 0.0f, 1.0f, 0.1f);
		interface->addVerticalSlider("input 1", &fslider33, 1.0f, 0.0f, 1.0f, 0.1f);
		interface->addVerticalSlider("tap 0", &fslider39, 0.0f, 0.0f, 1.0f, 0.1f);
		interface->addVerticalSlider("tap 1", &fslider38, 0.0f, 0.0f, 1.0f, 0.1f);
		interface->addVerticalSlider("tap 2", &fslider37, 0.0f, 0.0f, 1.0f, 0.1f);
		interface->addVerticalSlider("tap 3", &fslider36, 0.0f, 0.0f, 1.0f, 0.1f);
		interface->addVerticalSlider("tap 4", &fslider35, 0.0f, 0.0f, 1.0f, 0.1f);
		interface->addVerticalSlider("tap 5", &fslider34, 0.0f, 0.0f, 1.0f, 0.1f);
		interface->closeBox();
		interface->openHorizontalBox("Tap 4");
		interface->addVerticalSlider("delay (sec)", &fslider51, 0.0f, 0.0f, 5.0f, 0.01f);
		interface->addVerticalSlider("gain", &fslider50, 1.0f, 0.0f, 1.0f, 0.1f);
		interface->addVerticalSlider("input 0", &fslider42, 1.0f, 0.0f, 1.0f, 0.1f);
		interface->addVerticalSlider("input 1", &fslider43, 1.0f, 0.0f, 1.0f, 0.1f);
		interface->addVerticalSlider("tap 0", &fslider49, 0.0f, 0.0f, 1.0f, 0.1f);
		interface->addVerticalSlider("tap 1", &fslider48, 0.0f, 0.0f, 1.0f, 0.1f);
		interface->addVerticalSlider("tap 2", &fslider47, 0.0f, 0.0f, 1.0f, 0.1f);
		interface->addVerticalSlider("tap 3", &fslider46, 0.0f, 0.0f, 1.0f, 0.1f);
		interface->addVerticalSlider("tap 4", &fslider45, 0.0f, 0.0f, 1.0f, 0.1f);
		interface->addVerticalSlider("tap 5", &fslider44, 0.0f, 0.0f, 1.0f, 0.1f);
		interface->closeBox();
		interface->openHorizontalBox("Tap 5");
		interface->addVerticalSlider("delay (sec)", &fslider61, 0.0f, 0.0f, 5.0f, 0.01f);
		interface->addVerticalSlider("gain", &fslider60, 1.0f, 0.0f, 1.0f, 0.1f);
		interface->addVerticalSlider("input 0", &fslider52, 1.0f, 0.0f, 1.0f, 0.1f);
		interface->addVerticalSlider("input 1", &fslider53, 1.0f, 0.0f, 1.0f, 0.1f);
		interface->addVerticalSlider("tap 0", &fslider59, 0.0f, 0.0f, 1.0f, 0.1f);
		interface->addVerticalSlider("tap 1", &fslider58, 0.0f, 0.0f, 1.0f, 0.1f);
		interface->addVerticalSlider("tap 2", &fslider57, 0.0f, 0.0f, 1.0f, 0.1f);
		interface->addVerticalSlider("tap 3", &fslider56, 0.0f, 0.0f, 1.0f, 0.1f);
		interface->addVerticalSlider("tap 4", &fslider55, 0.0f, 0.0f, 1.0f, 0.1f);
		interface->addVerticalSlider("tap 5", &fslider54, 0.0f, 0.0f, 1.0f, 0.1f);
		interface->closeBox();
		interface->closeBox();
		interface->openVerticalBox("outputs");
		interface->openHorizontalBox("output 0");
		interface->addVerticalSlider("gain", &fslider68, 1.0f, 0.0f, 1.0f, 0.1f);
		interface->addVerticalSlider("input 0", &fslider0, 1.0f, 0.0f, 1.0f, 0.1f);
		interface->addVerticalSlider("input 1", &fslider1, 1.0f, 0.0f, 1.0f, 0.1f);
		interface->addVerticalSlider("tap 0", &fslider67, 0.0f, 0.0f, 1.0f, 0.1f);
		interface->addVerticalSlider("tap 1", &fslider66, 0.0f, 0.0f, 1.0f, 0.1f);
		interface->addVerticalSlider("tap 2", &fslider65, 0.0f, 0.0f, 1.0f, 0.1f);
		interface->addVerticalSlider("tap 3", &fslider64, 0.0f, 0.0f, 1.0f, 0.1f);
		interface->addVerticalSlider("tap 4", &fslider63, 0.0f, 0.0f, 1.0f, 0.1f);
		interface->addVerticalSlider("tap 5", &fslider62, 0.0f, 0.0f, 1.0f, 0.1f);
		interface->closeBox();
		interface->openHorizontalBox("output 1");
		interface->addVerticalSlider("gain", &fslider77, 1.0f, 0.0f, 1.0f, 0.1f);
		interface->addVerticalSlider("input 0", &fslider69, 1.0f, 0.0f, 1.0f, 0.1f);
		interface->addVerticalSlider("input 1", &fslider70, 1.0f, 0.0f, 1.0f, 0.1f);
		interface->addVerticalSlider("tap 0", &fslider76, 0.0f, 0.0f, 1.0f, 0.1f);
		interface->addVerticalSlider("tap 1", &fslider75, 0.0f, 0.0f, 1.0f, 0.1f);
		interface->addVerticalSlider("tap 2", &fslider74, 0.0f, 0.0f, 1.0f, 0.1f);
		interface->addVerticalSlider("tap 3", &fslider73, 0.0f, 0.0f, 1.0f, 0.1f);
		interface->addVerticalSlider("tap 4", &fslider72, 0.0f, 0.0f, 1.0f, 0.1f);
		interface->addVerticalSlider("tap 5", &fslider71, 0.0f, 0.0f, 1.0f, 0.1f);
		interface->closeBox();
		interface->closeBox();
		interface->closeBox();
	}
	virtual void compute (int count, FAUSTFLOAT** input, FAUSTFLOAT** output) {
		float 	fSlow0 = fslider0;
		float 	fSlow1 = fslider1;
		float 	fSlow2 = fslider2;
		float 	fSlow3 = fslider3;
		float 	fSlow4 = fslider4;
		float 	fSlow5 = fslider5;
		float 	fSlow6 = fslider6;
		float 	fSlow7 = fslider7;
		float 	fSlow8 = fslider8;
		float 	fSlow9 = fslider9;
		float 	fSlow10 = fslider10;
		int 	iSlow11 = int((int((iConst0 * fslider11)) & 524287));
		float 	fSlow12 = fslider12;
		float 	fSlow13 = fslider13;
		float 	fSlow14 = fslider14;
		float 	fSlow15 = fslider15;
		float 	fSlow16 = fslider16;
		float 	fSlow17 = fslider17;
		float 	fSlow18 = fslider18;
		float 	fSlow19 = fslider19;
		float 	fSlow20 = fslider20;
		int 	iSlow21 = int((int((iConst0 * fslider21)) & 524287));
		float 	fSlow22 = fslider22;
		float 	fSlow23 = fslider23;
		float 	fSlow24 = fslider24;
		float 	fSlow25 = fslider25;
		float 	fSlow26 = fslider26;
		float 	fSlow27 = fslider27;
		float 	fSlow28 = fslider28;
		float 	fSlow29 = fslider29;
		float 	fSlow30 = fslider30;
		int 	iSlow31 = int((int((iConst0 * fslider31)) & 524287));
		float 	fSlow32 = fslider32;
		float 	fSlow33 = fslider33;
		float 	fSlow34 = fslider34;
		float 	fSlow35 = fslider35;
		float 	fSlow36 = fslider36;
		float 	fSlow37 = fslider37;
		float 	fSlow38 = fslider38;
		float 	fSlow39 = fslider39;
		float 	fSlow40 = fslider40;
		int 	iSlow41 = int((int((iConst0 * fslider41)) & 524287));
		float 	fSlow42 = fslider42;
		float 	fSlow43 = fslider43;
		float 	fSlow44 = fslider44;
		float 	fSlow45 = fslider45;
		float 	fSlow46 = fslider46;
		float 	fSlow47 = fslider47;
		float 	fSlow48 = fslider48;
		float 	fSlow49 = fslider49;
		float 	fSlow50 = fslider50;
		int 	iSlow51 = int((int((iConst0 * fslider51)) & 524287));
		float 	fSlow52 = fslider52;
		float 	fSlow53 = fslider53;
		float 	fSlow54 = fslider54;
		float 	fSlow55 = fslider55;
		float 	fSlow56 = fslider56;
		float 	fSlow57 = fslider57;
		float 	fSlow58 = fslider58;
		float 	fSlow59 = fslider59;
		float 	fSlow60 = fslider60;
		int 	iSlow61 = int((int((iConst0 * fslider61)) & 524287));
		float 	fSlow62 = fslider62;
		float 	fSlow63 = fslider63;
		float 	fSlow64 = fslider64;
		float 	fSlow65 = fslider65;
		float 	fSlow66 = fslider66;
		float 	fSlow67 = fslider67;
		float 	fSlow68 = fslider68;
		float 	fSlow69 = fslider69;
		float 	fSlow70 = fslider70;
		float 	fSlow71 = fslider71;
		float 	fSlow72 = fslider72;
		float 	fSlow73 = fslider73;
		float 	fSlow74 = fslider74;
		float 	fSlow75 = fslider75;
		float 	fSlow76 = fslider76;
		float 	fSlow77 = fslider77;
		FAUSTFLOAT* input0 = input[0];
		FAUSTFLOAT* input1 = input[1];
		FAUSTFLOAT* output0 = output[0];
		FAUSTFLOAT* output1 = output[1];
		for (int i=0; i<count; i++) {
			float fTemp0 = (float)input0[i];
			float fTemp1 = (float)input1[i];
			fVec0[IOTA&524287] = (fSlow10 * ((fSlow9 * fRec0[1]) + ((fSlow8 * fRec1[1]) + ((fSlow7 * fRec2[1]) + ((fSlow6 * fRec3[1]) + ((fSlow5 * fRec4[1]) + ((fSlow4 * fRec5[1]) + ((fSlow3 * fTemp1) + (fSlow2 * fTemp0)))))))));
			fRec0[0] = fVec0[(IOTA-iSlow11)&524287];
			fVec1[IOTA&524287] = (fSlow20 * ((fSlow19 * fRec0[1]) + ((fSlow18 * fRec1[1]) + ((fSlow17 * fRec2[1]) + ((fSlow16 * fRec3[1]) + ((fSlow15 * fRec4[1]) + ((fSlow14 * fRec5[1]) + ((fSlow13 * fTemp1) + (fSlow12 * fTemp0)))))))));
			fRec1[0] = fVec1[(IOTA-iSlow21)&524287];
			fVec2[IOTA&524287] = (fSlow30 * ((fSlow29 * fRec0[1]) + ((fSlow28 * fRec1[1]) + ((fSlow27 * fRec2[1]) + ((fSlow26 * fRec3[1]) + ((fSlow25 * fRec4[1]) + ((fSlow24 * fRec5[1]) + ((fSlow23 * fTemp1) + (fSlow22 * fTemp0)))))))));
			fRec2[0] = fVec2[(IOTA-iSlow31)&524287];
			fVec3[IOTA&524287] = (fSlow40 * ((fSlow39 * fRec0[1]) + ((fSlow38 * fRec1[1]) + ((fSlow37 * fRec2[1]) + ((fSlow36 * fRec3[1]) + ((fSlow35 * fRec4[1]) + ((fSlow34 * fRec5[1]) + ((fSlow33 * fTemp1) + (fSlow32 * fTemp0)))))))));
			fRec3[0] = fVec3[(IOTA-iSlow41)&524287];
			fVec4[IOTA&524287] = (fSlow50 * ((fSlow49 * fRec0[1]) + ((fSlow48 * fRec1[1]) + ((fSlow47 * fRec2[1]) + ((fSlow46 * fRec3[1]) + ((fSlow45 * fRec4[1]) + ((fSlow44 * fRec5[1]) + ((fSlow43 * fTemp1) + (fSlow42 * fTemp0)))))))));
			fRec4[0] = fVec4[(IOTA-iSlow51)&524287];
			fVec5[IOTA&524287] = (fSlow60 * ((fSlow59 * fRec0[1]) + ((fSlow58 * fRec1[1]) + ((fSlow57 * fRec2[1]) + ((fSlow56 * fRec3[1]) + ((fSlow55 * fRec4[1]) + ((fSlow54 * fRec5[1]) + ((fSlow53 * fTemp1) + (fSlow52 * fTemp0)))))))));
			fRec5[0] = fVec5[(IOTA-iSlow61)&524287];
			output0[i] = (FAUSTFLOAT)(fSlow68 * ((fSlow67 * fRec0[0]) + ((fSlow66 * fRec1[0]) + ((fSlow65 * fRec2[0]) + ((fSlow64 * fRec3[0]) + ((fSlow63 * fRec4[0]) + ((fSlow62 * fRec5[0]) + ((fSlow1 * fTemp1) + (fSlow0 * fTemp0)))))))));
			output1[i] = (FAUSTFLOAT)(fSlow77 * ((fSlow76 * fRec0[0]) + ((fSlow75 * fRec1[0]) + ((fSlow74 * fRec2[0]) + ((fSlow73 * fRec3[0]) + ((fSlow72 * fRec4[0]) + ((fSlow71 * fRec5[0]) + ((fSlow70 * fTemp1) + (fSlow69 * fTemp0)))))))));
			// post processing
			fRec5[1] = fRec5[0];
			fRec4[1] = fRec4[0];
			fRec3[1] = fRec3[0];
			fRec2[1] = fRec2[0];
			fRec1[1] = fRec1[0];
			fRec0[1] = fRec0[0];
			IOTA = IOTA+1;
		}
	}
};





#include "SoundPlugin.h"
#include "SoundPlugin_proc.h"

#include "Mixer_proc.h"
#include "SoundPluginRegistry_proc.h"

#include "Faust_plugins_proc.h"


namespace{

class MyUI : public UI
{

 public:

  MyUI() 
    : next_peak(NULL)
    , _gate_control(NULL)
    , _freq_control(NULL)
    , _gain_control(NULL)
    , _num_effects(0)
    , _effect_tooltip("")
    , _curr_box_name(NULL)
  { }

  ~MyUI() {	}

  float *next_peak;

  float *_gate_control;
  float *_freq_control;
  float *_gain_control;

  struct Controller{
    float* control_port;

    float *peak_port;

    float min_value;
    float default_value;
    float max_value;

    std::string name;
    int type;

    const char *tooltip;
    const char *unit;

    Controller(float *control_port)
      : control_port(control_port)
      , peak_port(NULL)
      , min_value(0.0f)
      , default_value(0.5f)
      , max_value(1.0f)
      , name("<no name>")
      , type(EFFECT_FORMAT_FLOAT)
      , tooltip("")
      , unit("")
    { }
  };

  std::vector<Controller> _controllers;

  int get_controller_num(float *control_port){
    for(unsigned int i=0;i<_controllers.size();i++){
      if(control_port == _controllers.at(i).control_port)
        return i;
    }

    Controller controller(control_port);

    _controllers.push_back(controller);
    _num_effects++;

    return _controllers.size()-1;
  }

  bool is_instrument(){
    if(_gate_control!=NULL && _freq_control!=NULL && _gain_control!=NULL)
      return true;
    else
      return false;
  }

  // Remove gain/gate/freq sliders for instruments.
  void remove_instrument_notecontrol_effects(){
    if(is_instrument()){
      _controllers.erase(_controllers.begin() + get_controller_num(_gate_control));
      _controllers.erase(_controllers.begin() + get_controller_num(_freq_control));
      _controllers.erase(_controllers.begin() + get_controller_num(_gain_control));
      _num_effects -= 3;
    }
  }

  // We don't use passive items. (it would have been nice to have a passive effect telling when an instrument is finished playing)
  void remove_last_item(){
    _controllers.pop_back();
    _num_effects--;
  }
  

  int _num_effects;

  const char *_effect_tooltip;

  const char* _curr_box_name;

  // -- widget's layouts
  
  void openFrameBox(const char* label) {_curr_box_name = label;}
  void openTabBox(const char* label) {_curr_box_name = label;}
  void openHorizontalBox(const char* label) {_curr_box_name = label;}
  void openVerticalBox(const char* label) {_curr_box_name = label;}
  void closeBox() {_curr_box_name = NULL;}
  
  // -- active widgets

  void addEffect(const char *name, float* control_port, int type, float min_value, float default_value, float max_value){
    int effect_num = get_controller_num(control_port);

    Controller *controller = &_controllers.at(effect_num);

    if(_curr_box_name != NULL && strlen(_curr_box_name) < 10){
      controller->name = std::string(_curr_box_name) + ": " + name;
    }else{
      controller->name = name;
    }
    //printf("Controller name: \"%s\"\n",controller->name.c_str());
    controller->type = type;
    controller->min_value = min_value;
    controller->default_value = default_value;
    controller->max_value = max_value;

    if(next_peak != NULL){
      controller->peak_port = next_peak;
      next_peak = NULL;
    }

    if(!strcmp(name,"gate"))
      _gate_control = control_port;

    if(!strcmp(name,"freq"))
      _freq_control = control_port;

    if(!strcmp(name,"gain"))
      _gain_control = control_port;
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
    addEffect(label, zone,  step==1.0f ? EFFECT_FORMAT_INT : EFFECT_FORMAT_FLOAT, min, init, max);
  }
  void addHorizontalSlider(const char* label, float* zone, float init, float min, float max, float step) {
    addEffect(label, zone,  step==1.0f ? EFFECT_FORMAT_INT : EFFECT_FORMAT_FLOAT, min, init, max);
  }
  void addNumEntry(const char* label, float* zone, float init, float min, float max, float step) {
    addEffect(label, zone, step==1.0f ? EFFECT_FORMAT_INT : EFFECT_FORMAT_FLOAT, min, init, max); // The INT effect format might not work. Need to go through the code first.
  }
  
  // -- passive widgets

  void addNumDisplay(const char* label, float* zone, int precision) {remove_last_item();}
  void addTextDisplay(const char* label, float* zone, const char* names[], float min, float max) {remove_last_item();}
  void addHorizontalBargraph(const char* label, float* zone, float min, float max) {
    remove_last_item(); // remove metadata
    next_peak = zone;
  }
  void addVerticalBargraph(const char* label, float* zone, float min, float max) {
    remove_last_item(); // remove metadata
    next_peak = zone;
  }
  
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


#define MAX_POLYPHONY 32

struct Voice{
  struct Voice *prev;
  struct Voice *next;
  dsp *dsp_instance;
  MyUI myUI;
  int note_num;

  int frames_since_stop;

  int delta_pos_at_start; // Within the current block. Set when starting a note.
  int delta_pos_at_end; // Within the current block. Set when stopping a note.

  Voice()
    : prev(NULL)
    , next(NULL)
    , dsp_instance(NULL)
    , note_num(0)
    , delta_pos_at_start(0)
    , delta_pos_at_end(-1)
  { }
};

struct Data{
  Voice *voices_playing; // not used by effects
  Voice *voices_not_playing; // not used by effects
  Voice voices[MAX_POLYPHONY];   // Only voices[0] is used by effects.
  float samplerate;
  Data()
    : voices_playing(NULL)
    , voices_not_playing(NULL)
  {}
};

} // end anonymous namespace

static void RT_add_voice(Voice **root, Voice *voice){
  voice->next = *root;
  if(*root!=NULL)
    (*root)->prev = voice;
  *root = voice;
  voice->prev = NULL;
}

static void RT_remove_voice(Voice **root, Voice *voice){
  if(voice->prev!=NULL)
    voice->prev->next = voice->next;
  else
    *root=voice->next;

  if(voice->next!=NULL)
    voice->next->prev = voice->prev;
}

static bool RT_is_silent(float *sound, int num_frames){
  for(int i=0;i<num_frames;i++)
    if(sound[i]>0.05f)
      return false;
  return true;
}

enum VoiceOp{
  VOICE_KEEP,
  VOICE_REMOVE
};

static void RT_process_between(Voice *voice, float **inputs, float **outputs, int start, int end){
  if(end==start)
    return;

  int num_inputs = voice->dsp_instance->getNumInputs();
  float *offsetted_inputs[num_inputs];
  for(int ch=0;ch<num_inputs; ch++)
    offsetted_inputs[ch] = &inputs[ch][start];

  int num_outputs = voice->dsp_instance->getNumOutputs();
  float *offsetted_outputs[num_outputs];
  for(int ch=0;ch<num_outputs; ch++)
    offsetted_outputs[ch] = &outputs[ch][start];

  //printf("Computing Delta start / end: %d / %d\n",start,end);

  voice->dsp_instance->compute(end-start, offsetted_inputs, offsetted_outputs);
}

static VoiceOp RT_play_voice(Data *data, Voice *voice, int num_frames, float **inputs, float **outputs, int *start_process){

  int delta_pos_at_start = voice->delta_pos_at_start;
  int delta_pos_at_end = voice->delta_pos_at_end;

  if(delta_pos_at_start==0 && delta_pos_at_end==-1){

    voice->dsp_instance->compute(num_frames, inputs, outputs);

    *start_process = 0;

    if( *(voice->myUI._gate_control)==0.0f){
      if(false
         || RT_is_silent(outputs[0],num_frames)
         || voice->frames_since_stop > data->samplerate) // Safety mechanism. Force voice to stop after about 1 second.
        return VOICE_REMOVE;

      voice->frames_since_stop += num_frames;
    }

  }else if(delta_pos_at_start>0 && delta_pos_at_end==-1){

    RT_process_between(voice, inputs, outputs, delta_pos_at_start, num_frames);

    *start_process = delta_pos_at_start;
    voice->delta_pos_at_start = 0;

  }else{
    //printf("Delta start / end: %d / %d\n",delta_pos_at_start,delta_pos_at_end);

    RT_process_between(voice, inputs, outputs, delta_pos_at_start, delta_pos_at_end);
    {
      *(voice->myUI._gate_control)=0.0f;
    }
    RT_process_between(voice, inputs, outputs, delta_pos_at_end, num_frames);

    voice->frames_since_stop = num_frames-delta_pos_at_end;

    *start_process = delta_pos_at_start;
    voice->delta_pos_at_start = 0;
    voice->delta_pos_at_end = -1;

  }

  return VOICE_KEEP;
}

static void RT_process_instrument(SoundPlugin *plugin, int64_t time, int num_frames, float **inputs, float **outputs){
  Data *data = (Data*)plugin->data;
  int num_outputs = plugin->type->num_outputs;

  for(int i=0;i<num_outputs;i++)
    memset(outputs[i],0,num_frames*sizeof(float));

  float *tempsounds[num_outputs];
  float tempdata[num_outputs][num_frames];
  for(int i=0;i<num_outputs;i++)
    tempsounds[i] = &tempdata[i][0];

  Voice *voice = data->voices_playing;
  //printf("Voices? %s\n",voice==NULL?"No":"Yes");

  while(voice!=NULL){
    Voice *next = voice->next;
    int start_process;

    if(RT_play_voice(data,voice,num_frames,inputs,tempsounds,&start_process)==VOICE_REMOVE){
      RT_remove_voice(&data->voices_playing, voice);
      RT_add_voice(&data->voices_not_playing, voice);

      for(int ch=0;ch<num_outputs;ch++)
        RT_fade_out(tempsounds[ch], num_frames-start_process);
    }

    for(int ch=0;ch<num_outputs;ch++){
      float *source = tempsounds[ch];
      float *target = outputs[ch];
      for(int i=start_process;i<num_frames;i++)
        target[i] += source[i];
    }

    voice=next;
  }
}

static void play_note(struct SoundPlugin *plugin, int64_t time, int note_num, float volume, float pan){
  Data *data = (Data*)plugin->data;

  //printf("Playing %d\n",note_num);

  Voice *voice = data->voices_not_playing;

  if(voice==NULL){
    printf("no more free voices\n");
    return;
  }

  RT_remove_voice(&data->voices_not_playing, voice);
  RT_add_voice(&data->voices_playing, voice);

  //voice->dsp_instance->init((int)data->samplerate);

  *(voice->myUI._gate_control) = 1.0f;
  *(voice->myUI._freq_control) = midi_to_hz(note_num);
  *(voice->myUI._gain_control) = velocity2gain(volume);

  voice->note_num = note_num;

  voice->frames_since_stop = 0;
  voice->delta_pos_at_start = time;
  voice->delta_pos_at_end = -1;
}

static void set_note_volume(struct SoundPlugin *plugin, int64_t time, int note_num, float volume){
  Data *data = (Data*)plugin->data;
  Voice *voice = data->voices_playing;
  //printf("Setting volume %f / %f\n",volume,velocity2gain(volume));
  while(voice!=NULL){
    if(voice->note_num==note_num)
      *(voice->myUI._gain_control) = velocity2gain(volume);
    voice=voice->next;
  }
}

static void stop_note(struct SoundPlugin *plugin, int64_t time, int note_num, float volume){
  Data *data = (Data*)plugin->data;
  Voice *voice = data->voices_playing;
  while(voice!=NULL){
    if(voice->note_num==note_num)
      voice->delta_pos_at_end = time;
    voice=voice->next;
  }
}

static void RT_process_effect(SoundPlugin *plugin, int64_t time, int num_frames, float **inputs, float **outputs){
  //SoundPluginType *type = plugin->type;
  Data *data = (Data*)plugin->data;

  data->voices[0].dsp_instance->compute(num_frames, inputs, outputs);
  //printf("in00: %f, in10: %f\n",inputs[0][0],inputs[1][0]);
  //printf("out00: %f, out10: %f\n",outputs[0][0],outputs[1][0]);
}

static void *create_effect_plugin_data(const SoundPluginType *plugin_type, struct SoundPlugin *plugin, float samplerate, int blocksize){
  Data *data = new Data;
  data->samplerate = samplerate;

  Voice *voice = &data->voices[0];
  voice->dsp_instance = new CLASSNAME;
  //printf("Creating %s / %s. samplerate: %d\n",plugin_type->type_name,plugin_type->name,(int)samplerate);
  voice->dsp_instance->init(samplerate);
  voice->dsp_instance->buildUserInterface(&voice->myUI);
  return data;
}

static void *create_instrument_plugin_data(const SoundPluginType *plugin_type, struct SoundPlugin *plugin, float samplerate, int blocksize){
  Data *data = new Data;
  data->samplerate = samplerate;

  for(int i=0;i<MAX_POLYPHONY;i++){
    Voice *voice = &data->voices[i];
    voice->dsp_instance = new CLASSNAME;
    voice->dsp_instance->init(samplerate);
    voice->dsp_instance->buildUserInterface(&voice->myUI);
    voice->myUI.remove_instrument_notecontrol_effects();

    RT_add_voice(&data->voices_not_playing, voice);
  }


  return data;
}

static void cleanup_plugin_data(SoundPlugin *plugin){
  Data *data = (Data*)plugin->data;

  for(int i=0;i<MAX_POLYPHONY;i++){
    Voice *voice = &data->voices[i];
    if(voice->dsp_instance==NULL) // an effect
      break;
    else
      delete voice->dsp_instance;
  }

  delete data;
}

#ifdef FAUST_THAT_ONE
float *FAUST_get_peak_value_pointer(SoundPlugin *plugin, int effect_num){
  Data *data = (Data*)plugin->data;
  MyUI::Controller *controller = &data->voices[0].myUI._controllers.at(effect_num);
  if(controller->peak_port!=NULL)
    return controller->peak_port;
  else
    return NULL;
}
#endif

static int get_effect_format(struct SoundPlugin *plugin, int effect_num){
  const struct SoundPluginType *plugin_type = plugin->type;
  Data *data = (Data*)plugin_type->data;
  Voice *voice = &data->voices[0];
  MyUI::Controller *controller = &voice->myUI._controllers.at(effect_num);
  return controller->type;
}

static const char *get_effect_name(struct SoundPlugin *plugin, int effect_num){
  const struct SoundPluginType *plugin_type = plugin->type;
  Data *data = (Data*)plugin_type->data;
  Voice *voice = &data->voices[0];
  MyUI::Controller *controller = &voice->myUI._controllers.at(effect_num);
  return controller->name.c_str();
}

static void set_effect_value(struct SoundPlugin *plugin, int64_t time, int effect_num, float value, enum ValueFormat value_format){
  Data *data = (Data*)plugin->data;
  float scaled_value;

  if(value_format==PLUGIN_FORMAT_SCALED){
#ifdef DONT_NORMALIZE_EFFECT_VALUES
    scaled_value = value;
#else
    MyUI::Controller *controller = &data->voices[0].myUI._controllers.at(effect_num);
    float min = controller->min_value;
    float max = controller->max_value;
    scaled_value = scale(value,0,1,min,max);
#endif
  }else{
    scaled_value = value;
  }

  //printf("Setting effect %d to %f. input: %f\n",effect_num,scaled_value,value);

  for(int i=0;i<MAX_POLYPHONY;i++){
    Voice *voice = &data->voices[i];
    if(voice->dsp_instance==NULL) // an effect
      break;
    MyUI::Controller *controller = &voice->myUI._controllers.at(effect_num);
    *(controller->control_port) = scaled_value;
  }
}

static float get_effect_value(struct SoundPlugin *plugin, int effect_num, enum ValueFormat value_format){
  Data *data = (Data*)plugin->data;
  Voice *voice = &data->voices[0];
  MyUI::Controller *controller = &voice->myUI._controllers.at(effect_num);

  if(value_format==PLUGIN_FORMAT_SCALED){
#ifdef DONT_NORMALIZE_EFFECT_VALUES
    return *(controller->control_port);
#else
    float min = controller->min_value;
    float max = controller->max_value;
    return scale(*(controller->control_port),min,max,0.0f,1.0f);
#endif
  }else{
    return *(controller->control_port);
  }
}

static void get_display_value_string(struct SoundPlugin *plugin, int effect_num, char *buffer, int buffersize){
  Data *data = (Data*)plugin->data;
  Voice *voice = &data->voices[0];
  MyUI::Controller *controller = &voice->myUI._controllers.at(effect_num);

  if(controller->type==EFFECT_FORMAT_INT)
    snprintf(buffer,buffersize-1,"%d %s",(int)*(controller->control_port), controller->unit);
  else
    snprintf(buffer,buffersize-1,"%.2f %s",*(controller->control_port), controller->unit);
}

static const char *get_effect_description(const struct SoundPluginType *plugin_type, int effect_num){
  Data *data = (Data*)plugin_type->data;
  Voice *voice = &data->voices[0];
  MyUI::Controller *controller = &voice->myUI._controllers.at(effect_num);

  return controller->tooltip;
}

static int get_effect_num(struct SoundPlugin *plugin, const char *effect_name){
  const struct SoundPluginType *plugin_type = plugin->type;
  Data *data = (Data*)plugin_type->data;
  Voice *voice = &data->voices[0];
  for(unsigned int i=0;i<voice->myUI._controllers.size();i++){
    MyUI::Controller *controller = &voice->myUI._controllers.at(i);
    if(!strcmp(controller->name.c_str(),effect_name))
      return i;
  }
  RError("Couldn't find effect name \"%s\" in plugin %s/%s\n",plugin_type->type_name,plugin_type->name);
  return 0;
}

static void fill_type(SoundPluginType *type){
 type->type_name                = "Faust";
 type->note_handling_is_RT      = false;
 type->get_effect_format        = get_effect_format;
 type->get_effect_name          = get_effect_name;
 type->effect_is_RT             = NULL;
 type->cleanup_plugin_data      = cleanup_plugin_data;

 type->play_note       = play_note;
 type->set_note_volume = set_note_volume;
 type->stop_note       = stop_note;

 type->set_effect_value         = set_effect_value;
 type->get_effect_value         = get_effect_value;
 type->get_display_value_string = get_display_value_string;
 type->get_effect_description   = get_effect_description;

 type->get_effect_num = get_effect_num;

 type->data                     = NULL;
};

static SoundPluginType faust_type = {0};

void CREATE_NAME (void){
  fill_type(&faust_type);

  Data *data = (Data*)create_effect_plugin_data(&faust_type, NULL, MIXER_get_sample_rate(), MIXER_get_buffer_size());
  faust_type.data = data;

  faust_type.name = DSP_NAME;

  faust_type.num_inputs = data->voices[0].dsp_instance->getNumInputs();
  faust_type.num_outputs = data->voices[0].dsp_instance->getNumOutputs();

  if(data->voices[0].myUI.is_instrument()){

    faust_type.is_instrument      = true;

    faust_type.RT_process         = RT_process_instrument;
    faust_type.create_plugin_data = create_instrument_plugin_data;

    data->voices[0].myUI.remove_instrument_notecontrol_effects();

  }else{

    faust_type.is_instrument      = false;

    faust_type.RT_process         = RT_process_effect;
    faust_type.create_plugin_data = create_effect_plugin_data;

    faust_type.play_note          = NULL;
    faust_type.set_note_volume    = NULL;
    faust_type.stop_note          = NULL;

  }

  faust_type.num_effects = data->voices[0].myUI._num_effects;

  PR_add_plugin_type(&faust_type);
}
