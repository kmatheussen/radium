//-----------------------------------------------------
// name: "tapiir"
// version: "1.0"
// author: "Grame"
// license: "BSD"
// copyright: "(c)GRAME 2006"
//
// Code generated with Faust 0.9.73 (http://faust.grame.fr)
//-----------------------------------------------------
/* link with  */
#include <math.h>
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
	virtual void buildUserInterface(UI* faust_interface) {
		faust_interface->openVerticalBox("Tapiir");
		faust_interface->openTabBox("0x00");
		faust_interface->openHorizontalBox("Tap 0");
		faust_interface->addVerticalSlider("delay (sec)", &fslider11, 0.0f, 0.0f, 5.0f, 0.01f);
		faust_interface->addVerticalSlider("gain", &fslider10, 1.0f, 0.0f, 1.0f, 0.1f);
		faust_interface->addVerticalSlider("input 0", &fslider3, 1.0f, 0.0f, 1.0f, 0.1f);
		faust_interface->addVerticalSlider("input 1", &fslider2, 1.0f, 0.0f, 1.0f, 0.1f);
		faust_interface->addVerticalSlider("tap 0", &fslider9, 0.0f, 0.0f, 1.0f, 0.1f);
		faust_interface->addVerticalSlider("tap 1", &fslider8, 0.0f, 0.0f, 1.0f, 0.1f);
		faust_interface->addVerticalSlider("tap 2", &fslider7, 0.0f, 0.0f, 1.0f, 0.1f);
		faust_interface->addVerticalSlider("tap 3", &fslider6, 0.0f, 0.0f, 1.0f, 0.1f);
		faust_interface->addVerticalSlider("tap 4", &fslider5, 0.0f, 0.0f, 1.0f, 0.1f);
		faust_interface->addVerticalSlider("tap 5", &fslider4, 0.0f, 0.0f, 1.0f, 0.1f);
		faust_interface->closeBox();
		faust_interface->openHorizontalBox("Tap 1");
		faust_interface->addVerticalSlider("delay (sec)", &fslider21, 0.0f, 0.0f, 5.0f, 0.01f);
		faust_interface->addVerticalSlider("gain", &fslider20, 1.0f, 0.0f, 1.0f, 0.1f);
		faust_interface->addVerticalSlider("input 0", &fslider13, 1.0f, 0.0f, 1.0f, 0.1f);
		faust_interface->addVerticalSlider("input 1", &fslider12, 1.0f, 0.0f, 1.0f, 0.1f);
		faust_interface->addVerticalSlider("tap 0", &fslider19, 0.0f, 0.0f, 1.0f, 0.1f);
		faust_interface->addVerticalSlider("tap 1", &fslider18, 0.0f, 0.0f, 1.0f, 0.1f);
		faust_interface->addVerticalSlider("tap 2", &fslider17, 0.0f, 0.0f, 1.0f, 0.1f);
		faust_interface->addVerticalSlider("tap 3", &fslider16, 0.0f, 0.0f, 1.0f, 0.1f);
		faust_interface->addVerticalSlider("tap 4", &fslider15, 0.0f, 0.0f, 1.0f, 0.1f);
		faust_interface->addVerticalSlider("tap 5", &fslider14, 0.0f, 0.0f, 1.0f, 0.1f);
		faust_interface->closeBox();
		faust_interface->openHorizontalBox("Tap 2");
		faust_interface->addVerticalSlider("delay (sec)", &fslider31, 0.0f, 0.0f, 5.0f, 0.01f);
		faust_interface->addVerticalSlider("gain", &fslider30, 1.0f, 0.0f, 1.0f, 0.1f);
		faust_interface->addVerticalSlider("input 0", &fslider23, 1.0f, 0.0f, 1.0f, 0.1f);
		faust_interface->addVerticalSlider("input 1", &fslider22, 1.0f, 0.0f, 1.0f, 0.1f);
		faust_interface->addVerticalSlider("tap 0", &fslider29, 0.0f, 0.0f, 1.0f, 0.1f);
		faust_interface->addVerticalSlider("tap 1", &fslider28, 0.0f, 0.0f, 1.0f, 0.1f);
		faust_interface->addVerticalSlider("tap 2", &fslider27, 0.0f, 0.0f, 1.0f, 0.1f);
		faust_interface->addVerticalSlider("tap 3", &fslider26, 0.0f, 0.0f, 1.0f, 0.1f);
		faust_interface->addVerticalSlider("tap 4", &fslider25, 0.0f, 0.0f, 1.0f, 0.1f);
		faust_interface->addVerticalSlider("tap 5", &fslider24, 0.0f, 0.0f, 1.0f, 0.1f);
		faust_interface->closeBox();
		faust_interface->openHorizontalBox("Tap 3");
		faust_interface->addVerticalSlider("delay (sec)", &fslider41, 0.0f, 0.0f, 5.0f, 0.01f);
		faust_interface->addVerticalSlider("gain", &fslider40, 1.0f, 0.0f, 1.0f, 0.1f);
		faust_interface->addVerticalSlider("input 0", &fslider33, 1.0f, 0.0f, 1.0f, 0.1f);
		faust_interface->addVerticalSlider("input 1", &fslider32, 1.0f, 0.0f, 1.0f, 0.1f);
		faust_interface->addVerticalSlider("tap 0", &fslider39, 0.0f, 0.0f, 1.0f, 0.1f);
		faust_interface->addVerticalSlider("tap 1", &fslider38, 0.0f, 0.0f, 1.0f, 0.1f);
		faust_interface->addVerticalSlider("tap 2", &fslider37, 0.0f, 0.0f, 1.0f, 0.1f);
		faust_interface->addVerticalSlider("tap 3", &fslider36, 0.0f, 0.0f, 1.0f, 0.1f);
		faust_interface->addVerticalSlider("tap 4", &fslider35, 0.0f, 0.0f, 1.0f, 0.1f);
		faust_interface->addVerticalSlider("tap 5", &fslider34, 0.0f, 0.0f, 1.0f, 0.1f);
		faust_interface->closeBox();
		faust_interface->openHorizontalBox("Tap 4");
		faust_interface->addVerticalSlider("delay (sec)", &fslider51, 0.0f, 0.0f, 5.0f, 0.01f);
		faust_interface->addVerticalSlider("gain", &fslider50, 1.0f, 0.0f, 1.0f, 0.1f);
		faust_interface->addVerticalSlider("input 0", &fslider43, 1.0f, 0.0f, 1.0f, 0.1f);
		faust_interface->addVerticalSlider("input 1", &fslider42, 1.0f, 0.0f, 1.0f, 0.1f);
		faust_interface->addVerticalSlider("tap 0", &fslider49, 0.0f, 0.0f, 1.0f, 0.1f);
		faust_interface->addVerticalSlider("tap 1", &fslider48, 0.0f, 0.0f, 1.0f, 0.1f);
		faust_interface->addVerticalSlider("tap 2", &fslider47, 0.0f, 0.0f, 1.0f, 0.1f);
		faust_interface->addVerticalSlider("tap 3", &fslider46, 0.0f, 0.0f, 1.0f, 0.1f);
		faust_interface->addVerticalSlider("tap 4", &fslider45, 0.0f, 0.0f, 1.0f, 0.1f);
		faust_interface->addVerticalSlider("tap 5", &fslider44, 0.0f, 0.0f, 1.0f, 0.1f);
		faust_interface->closeBox();
		faust_interface->openHorizontalBox("Tap 5");
		faust_interface->addVerticalSlider("delay (sec)", &fslider61, 0.0f, 0.0f, 5.0f, 0.01f);
		faust_interface->addVerticalSlider("gain", &fslider60, 1.0f, 0.0f, 1.0f, 0.1f);
		faust_interface->addVerticalSlider("input 0", &fslider53, 1.0f, 0.0f, 1.0f, 0.1f);
		faust_interface->addVerticalSlider("input 1", &fslider52, 1.0f, 0.0f, 1.0f, 0.1f);
		faust_interface->addVerticalSlider("tap 0", &fslider59, 0.0f, 0.0f, 1.0f, 0.1f);
		faust_interface->addVerticalSlider("tap 1", &fslider58, 0.0f, 0.0f, 1.0f, 0.1f);
		faust_interface->addVerticalSlider("tap 2", &fslider57, 0.0f, 0.0f, 1.0f, 0.1f);
		faust_interface->addVerticalSlider("tap 3", &fslider56, 0.0f, 0.0f, 1.0f, 0.1f);
		faust_interface->addVerticalSlider("tap 4", &fslider55, 0.0f, 0.0f, 1.0f, 0.1f);
		faust_interface->addVerticalSlider("tap 5", &fslider54, 0.0f, 0.0f, 1.0f, 0.1f);
		faust_interface->closeBox();
		faust_interface->closeBox();
		faust_interface->openVerticalBox("outputs");
		faust_interface->openHorizontalBox("output 0");
		faust_interface->addVerticalSlider("gain", &fslider68, 1.0f, 0.0f, 1.0f, 0.1f);
		faust_interface->addVerticalSlider("input 0", &fslider1, 1.0f, 0.0f, 1.0f, 0.1f);
		faust_interface->addVerticalSlider("input 1", &fslider0, 1.0f, 0.0f, 1.0f, 0.1f);
		faust_interface->addVerticalSlider("tap 0", &fslider67, 0.0f, 0.0f, 1.0f, 0.1f);
		faust_interface->addVerticalSlider("tap 1", &fslider66, 0.0f, 0.0f, 1.0f, 0.1f);
		faust_interface->addVerticalSlider("tap 2", &fslider65, 0.0f, 0.0f, 1.0f, 0.1f);
		faust_interface->addVerticalSlider("tap 3", &fslider64, 0.0f, 0.0f, 1.0f, 0.1f);
		faust_interface->addVerticalSlider("tap 4", &fslider63, 0.0f, 0.0f, 1.0f, 0.1f);
		faust_interface->addVerticalSlider("tap 5", &fslider62, 0.0f, 0.0f, 1.0f, 0.1f);
		faust_interface->closeBox();
		faust_interface->openHorizontalBox("output 1");
		faust_interface->addVerticalSlider("gain", &fslider77, 1.0f, 0.0f, 1.0f, 0.1f);
		faust_interface->addVerticalSlider("input 0", &fslider70, 1.0f, 0.0f, 1.0f, 0.1f);
		faust_interface->addVerticalSlider("input 1", &fslider69, 1.0f, 0.0f, 1.0f, 0.1f);
		faust_interface->addVerticalSlider("tap 0", &fslider76, 0.0f, 0.0f, 1.0f, 0.1f);
		faust_interface->addVerticalSlider("tap 1", &fslider75, 0.0f, 0.0f, 1.0f, 0.1f);
		faust_interface->addVerticalSlider("tap 2", &fslider74, 0.0f, 0.0f, 1.0f, 0.1f);
		faust_interface->addVerticalSlider("tap 3", &fslider73, 0.0f, 0.0f, 1.0f, 0.1f);
		faust_interface->addVerticalSlider("tap 4", &fslider72, 0.0f, 0.0f, 1.0f, 0.1f);
		faust_interface->addVerticalSlider("tap 5", &fslider71, 0.0f, 0.0f, 1.0f, 0.1f);
		faust_interface->closeBox();
		faust_interface->closeBox();
		faust_interface->closeBox();
	}
	virtual void compute (int count, FAUSTFLOAT** input, FAUSTFLOAT** output) {
		float 	fSlow0 = float(fslider0);
		float 	fSlow1 = float(fslider1);
		float 	fSlow2 = float(fslider2);
		float 	fSlow3 = float(fslider3);
		float 	fSlow4 = float(fslider4);
		float 	fSlow5 = float(fslider5);
		float 	fSlow6 = float(fslider6);
		float 	fSlow7 = float(fslider7);
		float 	fSlow8 = float(fslider8);
		float 	fSlow9 = float(fslider9);
		float 	fSlow10 = float(fslider10);
		int 	iSlow11 = int((int((iConst0 * float(fslider11))) & 524287));
		float 	fSlow12 = float(fslider12);
		float 	fSlow13 = float(fslider13);
		float 	fSlow14 = float(fslider14);
		float 	fSlow15 = float(fslider15);
		float 	fSlow16 = float(fslider16);
		float 	fSlow17 = float(fslider17);
		float 	fSlow18 = float(fslider18);
		float 	fSlow19 = float(fslider19);
		float 	fSlow20 = float(fslider20);
		int 	iSlow21 = int((int((iConst0 * float(fslider21))) & 524287));
		float 	fSlow22 = float(fslider22);
		float 	fSlow23 = float(fslider23);
		float 	fSlow24 = float(fslider24);
		float 	fSlow25 = float(fslider25);
		float 	fSlow26 = float(fslider26);
		float 	fSlow27 = float(fslider27);
		float 	fSlow28 = float(fslider28);
		float 	fSlow29 = float(fslider29);
		float 	fSlow30 = float(fslider30);
		int 	iSlow31 = int((int((iConst0 * float(fslider31))) & 524287));
		float 	fSlow32 = float(fslider32);
		float 	fSlow33 = float(fslider33);
		float 	fSlow34 = float(fslider34);
		float 	fSlow35 = float(fslider35);
		float 	fSlow36 = float(fslider36);
		float 	fSlow37 = float(fslider37);
		float 	fSlow38 = float(fslider38);
		float 	fSlow39 = float(fslider39);
		float 	fSlow40 = float(fslider40);
		int 	iSlow41 = int((int((iConst0 * float(fslider41))) & 524287));
		float 	fSlow42 = float(fslider42);
		float 	fSlow43 = float(fslider43);
		float 	fSlow44 = float(fslider44);
		float 	fSlow45 = float(fslider45);
		float 	fSlow46 = float(fslider46);
		float 	fSlow47 = float(fslider47);
		float 	fSlow48 = float(fslider48);
		float 	fSlow49 = float(fslider49);
		float 	fSlow50 = float(fslider50);
		int 	iSlow51 = int((int((iConst0 * float(fslider51))) & 524287));
		float 	fSlow52 = float(fslider52);
		float 	fSlow53 = float(fslider53);
		float 	fSlow54 = float(fslider54);
		float 	fSlow55 = float(fslider55);
		float 	fSlow56 = float(fslider56);
		float 	fSlow57 = float(fslider57);
		float 	fSlow58 = float(fslider58);
		float 	fSlow59 = float(fslider59);
		float 	fSlow60 = float(fslider60);
		int 	iSlow61 = int((int((iConst0 * float(fslider61))) & 524287));
		float 	fSlow62 = float(fslider62);
		float 	fSlow63 = float(fslider63);
		float 	fSlow64 = float(fslider64);
		float 	fSlow65 = float(fslider65);
		float 	fSlow66 = float(fslider66);
		float 	fSlow67 = float(fslider67);
		float 	fSlow68 = float(fslider68);
		float 	fSlow69 = float(fslider69);
		float 	fSlow70 = float(fslider70);
		float 	fSlow71 = float(fslider71);
		float 	fSlow72 = float(fslider72);
		float 	fSlow73 = float(fslider73);
		float 	fSlow74 = float(fslider74);
		float 	fSlow75 = float(fslider75);
		float 	fSlow76 = float(fslider76);
		float 	fSlow77 = float(fslider77);
		FAUSTFLOAT* input0 = input[0];
		FAUSTFLOAT* input1 = input[1];
		FAUSTFLOAT* output0 = output[0];
		FAUSTFLOAT* output1 = output[1];
		for (int i=0; i<count; i++) {
			float fTemp0 = (float)input1[i];
			float fTemp1 = (float)input0[i];
			fVec0[IOTA&524287] = (fSlow10 * ((((((((fSlow9 * fRec0[1]) + (fSlow8 * fRec1[1])) + (fSlow7 * fRec2[1])) + (fSlow6 * fRec3[1])) + (fSlow5 * fRec4[1])) + (fSlow4 * fRec5[1])) + (fSlow3 * fTemp1)) + (fSlow2 * fTemp0)));
			fRec0[0] = fVec0[(IOTA-iSlow11)&524287];
			fVec1[IOTA&524287] = (fSlow20 * ((((((((fSlow19 * fRec0[1]) + (fSlow18 * fRec1[1])) + (fSlow17 * fRec2[1])) + (fSlow16 * fRec3[1])) + (fSlow15 * fRec4[1])) + (fSlow14 * fRec5[1])) + (fSlow13 * fTemp1)) + (fSlow12 * fTemp0)));
			fRec1[0] = fVec1[(IOTA-iSlow21)&524287];
			fVec2[IOTA&524287] = (fSlow30 * ((((((((fSlow29 * fRec0[1]) + (fSlow28 * fRec1[1])) + (fSlow27 * fRec2[1])) + (fSlow26 * fRec3[1])) + (fSlow25 * fRec4[1])) + (fSlow24 * fRec5[1])) + (fSlow23 * fTemp1)) + (fSlow22 * fTemp0)));
			fRec2[0] = fVec2[(IOTA-iSlow31)&524287];
			fVec3[IOTA&524287] = (fSlow40 * ((((((((fSlow39 * fRec0[1]) + (fSlow38 * fRec1[1])) + (fSlow37 * fRec2[1])) + (fSlow36 * fRec3[1])) + (fSlow35 * fRec4[1])) + (fSlow34 * fRec5[1])) + (fSlow33 * fTemp1)) + (fSlow32 * fTemp0)));
			fRec3[0] = fVec3[(IOTA-iSlow41)&524287];
			fVec4[IOTA&524287] = (fSlow50 * ((((((((fSlow49 * fRec0[1]) + (fSlow48 * fRec1[1])) + (fSlow47 * fRec2[1])) + (fSlow46 * fRec3[1])) + (fSlow45 * fRec4[1])) + (fSlow44 * fRec5[1])) + (fSlow43 * fTemp1)) + (fSlow42 * fTemp0)));
			fRec4[0] = fVec4[(IOTA-iSlow51)&524287];
			fVec5[IOTA&524287] = (fSlow60 * ((((((((fSlow59 * fRec0[1]) + (fSlow58 * fRec1[1])) + (fSlow57 * fRec2[1])) + (fSlow56 * fRec3[1])) + (fSlow55 * fRec4[1])) + (fSlow54 * fRec5[1])) + (fSlow53 * fTemp1)) + (fSlow52 * fTemp0)));
			fRec5[0] = fVec5[(IOTA-iSlow61)&524287];
			output0[i] = (FAUSTFLOAT)(fSlow68 * ((((((((fSlow67 * fRec0[0]) + (fSlow66 * fRec1[0])) + (fSlow65 * fRec2[0])) + (fSlow64 * fRec3[0])) + (fSlow63 * fRec4[0])) + (fSlow62 * fRec5[0])) + (fSlow1 * fTemp1)) + (fSlow0 * fTemp0)));
			output1[i] = (FAUSTFLOAT)(fSlow77 * ((((((((fSlow76 * fRec0[0]) + (fSlow75 * fRec1[0])) + (fSlow74 * fRec2[0])) + (fSlow73 * fRec3[0])) + (fSlow72 * fRec4[0])) + (fSlow71 * fRec5[0])) + (fSlow70 * fTemp1)) + (fSlow69 * fTemp0)));
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




#include "Faust_plugins_template2.cpp"

