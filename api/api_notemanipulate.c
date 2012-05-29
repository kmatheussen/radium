/* Copyright 2001 Kjetil S. Matheussen

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


#include "Python.h"

#include "../common/nsmtracker.h"
#include "../common/transpose_proc.h"
#include "../common/quantitize_proc.h"
#include "../common/new/backwards_proc.h"
#include "../common/new/invert_proc.h"
#include "../common/new/pitchexpand_proc.h"
#include "../common/new/glissando_proc.h"

#include "api_common_proc.h"



/************** Transpose ******************/

void transposeTrack(int transpose,int windownum){
  struct Tracker_Windows *window=getWindowFromNum(windownum);
  if(window==NULL) return;

  TransposeTrack_CurrPos(window,transpose);
}

void transposeBlock(int transpose,int windownum){
  struct Tracker_Windows *window=getWindowFromNum(windownum);
  if(window==NULL) return;

  TransposeBlock_CurrPos(window,transpose);
}

void transposeRange(int transpose,int windownum){
  struct Tracker_Windows *window=getWindowFromNum(windownum);
  if(window==NULL) return;


  TransposeRange_CurrPos(window,transpose);
}

void transposeNote(int transpose,int windownum){
  struct Tracker_Windows *window=getWindowFromNum(windownum);
  if(window==NULL) return;

  TransposeNote_CurrPos(window,transpose);
}



/************** Invert ******************/

void invertTrack(int windownum){
  struct Tracker_Windows *window=getWindowFromNum(windownum);
  if(window==NULL) return;

  InvertTrack_CurrPos(window);
}

void invertBlock(int windownum){
  struct Tracker_Windows *window=getWindowFromNum(windownum);
  if(window==NULL) return;

  InvertBlock_CurrPos(window);
}

void invertRange(int windownum){
  struct Tracker_Windows *window=getWindowFromNum(windownum);
  if(window==NULL) return;

  InvertRange_CurrPos(window);
}


/************** PitchExpand ******************/

void pexpandTrack(int f,int windownum){
  struct Tracker_Windows *window=getWindowFromNum(windownum);
  if(window==NULL) return;

  PExpandTrack_CurrPos(window,(float)((float)f/1000.0f));
}

void pexpandBlock(int f,int windownum){
  struct Tracker_Windows *window=getWindowFromNum(windownum);
  if(window==NULL) return;
  PExpandBlock_CurrPos(window,(float)((float)f/1000.0f));
}

void pexpandRange(int f,int windownum){
  struct Tracker_Windows *window=getWindowFromNum(windownum);
  if(window==NULL) return;

  PExpandRange_CurrPos(window,(float)((float)f/1000.0f));
}


/************** Backwards ******************/

void backwardsTrack(int windownum){
  struct Tracker_Windows *window=getWindowFromNum(windownum);
  if(window==NULL) return;

  BackWardsTrack_CurrPos(window);
}

void backwardsBlock(int windownum){
  struct Tracker_Windows *window=getWindowFromNum(windownum);
  if(window==NULL) return;

  BackWardsBlock_CurrPos(window);
}

void backwardsRange(int windownum){
  struct Tracker_Windows *window=getWindowFromNum(windownum);
  if(window==NULL) return;

  BackWardsRange_CurrPos(window);
}


/************** Quantitize ******************/

void quantitizeTrack(int windownum){
  struct Tracker_Windows *window=getWindowFromNum(windownum);
  if(window==NULL) return;

  Quantitize_track_CurrPos(window);
}

void quantitizeBlock(int windownum){
  struct Tracker_Windows *window=getWindowFromNum(windownum);
  if(window==NULL) return;

  Quantitize_block_CurrPos(window);
}

void quantitizeRange(int windownum){
  struct Tracker_Windows *window=getWindowFromNum(windownum);
  if(window==NULL) return;

  Quantitize_range_CurrPos(window);
}


/************** Glissando ******************/

void glissando(int windownum){
  struct Tracker_Windows *window=getWindowFromNum(windownum);
  if(window==NULL) return;

  Glissando_CurrPos(window);
}

