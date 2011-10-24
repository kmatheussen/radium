/* Copyright 2003 Kjetil S. Matheussen

This program is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public License
as published by the Free Software Foundation; either version 2
of the License, or (at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA. */



#include "../common/nsmtracker.h"
#include "../common/visual_proc.h"

#include "../common/disk.h"

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

void DUMMYSelectTrackInstrument(struct Tracks *track,struct Instruments *instrument){
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
  printf("\t\tDummyLoadFX_start\n");
  DC_SkipBlock();
  DC_fgets();

  printf("\t\tDummyLoadFX_end\n");
  return NULL;
}

int InitInstrumentPlugIn(struct Instruments *instrument){
  instrument->instrumentname="CAMD instrument";
  instrument->getStandardVelocity= &DUMMYgetStandardVelocity;
  instrument->getMaxVelocity= &DUMMYgetMaxVelocity;
  instrument->getFX= &DUMMYgetFX;
  instrument->getPatch= &DUMMYgetPatch;
  instrument->CloseInstrument=DUMMYCloseInstrument;
  instrument->SelectTrackInstrument=DUMMYSelectTrackInstrument;
  instrument->StopPlaying=DUMMYStopPlaying;
  instrument->PP_Update=DUMMYPP_Update;
  instrument->CopyInstrumentData=DUMMYCopyInstrumentData;
  instrument->PlayFromStartHook=DUMMYPlayFromStartHook;
  instrument->LoadFX=DUMMYLoadFX;
  return INSTRUMENT_SUCCESS;
}

