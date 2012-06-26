
#include "../common/nsmtracker.h"
#include "../common/visual_proc.h"

int DUMMYgetMaxVelocity(struct Tracks *track){
  return 127;
}

int DUMMYgetFX(struct Tracker_Windows *window,struct Tracks *track,struct FX *fx){
  //  char *ai[5]={"fx1","fx2","fx3","fx4","fx5"};
  //  GFX_Menu(window,0,"selecta",5,ai);
  fx->min=0;fx->max=127;
  return FX_SUCCESS;
}

int DUMMYgetPatch(struct Tracker_Windows *window,ReqType reqtype,struct Tracks *track,struct Patch *patch){
  return PATCH_FAILED;
}

void DUMMYtreatSpecialCommand(char *command,struct Tracks *track){
}

void DUMMYCloseInstrument(struct Instruments *instrument){
}

void DUMMYInitTrack(struct Instruments *instrument,struct Tracks *track){
}

void DUMMYStopPlaying(struct Instruments *instrument){
}

int DUMMYgetStandardVelocity(struct Tracks *track){
  return 0x50;
}

void DUMMYPP_Update(struct Instruments *instrument,struct Patch *patch){
}

/* Necesarry for undo. */
void *DUMMYCopyInstrumentData(struct Tracks *track){
  return NULL;
}

void DUMMYPlayFromStartHook(struct Instruments *instrument){
}

void *DUMMYLoadFX(struct FX *fx,struct Tracks *track){
  return NULL;
}

int InitInstrumentPlugIn(struct Instruments *instrument){
  instrument->instrumentname="CAMD instrument";
  instrument->getStandardVelocity= &DUMMYgetStandardVelocity;
  instrument->getMaxVelocity= &DUMMYgetMaxVelocity;
  instrument->getFX= &DUMMYgetFX;
  instrument->getPatch= &DUMMYgetPatch;
  instrument->CloseInstrument=DUMMYCloseInstrument;
  instrument->InitTrack=DUMMYInitTrack;
  instrument->StopPlaying=DUMMYStopPlaying;
  instrument->PP_Update=DUMMYPP_Update;
  instrument->CopyInstrumentData=DUMMYCopyInstrumentData;
  instrument->PlayFromStartHook=DUMMYPlayFromStartHook;
  instrument->LoadFX=DUMMYLoadFX;
  return INSTRUMENT_SUCCESS;
}

