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
#include "../common/placement_proc.h"
#include "../common/transpose_proc.h"
#include "../common/quantitize_proc.h"
#include "../common/fxtext_proc.h"
#include "../common/new/backwards_proc.h"
#include "../common/new/invert_proc.h"
#include "../common/new/pitchexpand_proc.h"
#include "../common/new/glissando_proc.h"

#include "api_common_proc.h"


extern struct Root *root;


/************** Transpose ******************/

void transposeTrack(int transpose,int windownum){
  struct Tracker_Windows *window=getWindowFromNum(windownum);
  if(window==NULL) return;

  TransposeTrack_CurrPos(window,transpose);
  window->must_redraw_editor = true;
}

void transposeBlock(int transpose,int windownum){
  struct Tracker_Windows *window=getWindowFromNum(windownum);
  if(window==NULL) return;

  TransposeBlock_CurrPos(window,transpose);
  window->must_redraw_editor = true;
}

void transposeRange(int transpose,int windownum){
  struct Tracker_Windows *window=getWindowFromNum(windownum);
  if(window==NULL) return;


  TransposeRange_CurrPos(window,transpose);
  window->must_redraw_editor = true;
}

void transposeNote(int transpose,int windownum){
  struct Tracker_Windows *window=getWindowFromNum(windownum);
  if(window==NULL) return;

  TransposeNote_CurrPos(window,transpose);
  window->must_redraw_editor = true;
}



/************** Invert ******************/

void invertTrack(int windownum){
  struct Tracker_Windows *window=getWindowFromNum(windownum);
  if(window==NULL) return;

  InvertTrack_CurrPos(window);
  window->must_redraw_editor = true;
}

void invertBlock(int windownum){
  struct Tracker_Windows *window=getWindowFromNum(windownum);
  if(window==NULL) return;

  InvertBlock_CurrPos(window);
  window->must_redraw_editor = true;
}

void invertRange(int windownum){
  struct Tracker_Windows *window=getWindowFromNum(windownum);
  if(window==NULL) return;

  InvertRange_CurrPos(window);
  window->must_redraw_editor = true;
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
  struct Tracker_Windows *window;
  struct WBlocks *wblock = getWBlockFromNumA(windownum, &window, -1);
  if (wblock==NULL)
    return;

  if (wblock->isranged==false){
    showAsyncMessage("No range in block. Select range by using Left Meta + b");
    return;
  }

  PExpandRange_CurrPos(window,(float)((float)f/1000.0f));
}


/************** Backwards ******************/

void backwardsTrack(int windownum){
  struct Tracker_Windows *window=getWindowFromNum(windownum);
  if(window==NULL) return;

  BackWardsTrack_CurrPos(window);
  window->must_redraw = true;
}

void backwardsBlock(int windownum){
  struct Tracker_Windows *window=getWindowFromNum(windownum);
  if(window==NULL) return;

  BackWardsBlock_CurrPos(window);
  window->must_redraw = true;
}

void backwardsRange(int windownum){
  struct Tracker_Windows *window=getWindowFromNum(windownum);
  if(window==NULL) return;

  BackWardsRange_CurrPos(window);
  window->must_redraw = true;
}


/************** Quantitize ******************/

// quantitize notes only
void quantitizeTrack(int windownum){
  struct Tracker_Windows *window=getWindowFromNum(windownum);
  if(window==NULL) return;

  Quantitize_track_CurrPos(window);
  window->must_redraw = true;
}

// quantitize notes if placed in the notes subtrack, fxs if placed in fx subtrack. (cents and velocities not supported yet)
void generalTrackQuantitize(int windownum){
  struct Tracker_Windows *window;
  struct WBlocks *wblock;
  struct WTracks *wtrack = getWTrackFromNumA(windownum, &window, -1, &wblock, -1);
  if (wtrack==NULL)
    return;

  struct FXs *to_fxs;
  int subtrack = FXTEXT_subsubtrack(window, wtrack, &to_fxs);

  if (subtrack < 0)
    quantitizeTrack(windownum);
  else {
    Quantitize_fxs(window, wblock, wtrack, to_fxs);
    window->must_redraw = true;
  }
}

void quantitizeBlock(int windownum){
  struct Tracker_Windows *window=getWindowFromNum(windownum);
  if(window==NULL) return;

  Quantitize_block_CurrPos(window);
  window->must_redraw = true;
}

void quantitizeRange(int windownum){
  struct Tracker_Windows *window=getWindowFromNum(windownum);
  if(window==NULL) return;

  Quantitize_range_CurrPos(window);
  window->must_redraw = true;
}

// TODO. Should return ratio
dyn_t getQuantitize(bool as_string){
  if (as_string)
    return DYN_create_string_dont_copy(STATIC_RATIO_as_string(root->quantitize_options.quant));
  else
    return DYN_create_ratio(make_ratio_from_static_ratio(root->quantitize_options.quant));
}

void setQuantitize(dyn_t dyn){
  if (!DYN_is_liberal_ratio(dyn)){
    handleError("setQuantitize: Expected number or string. Found %s", DYN_type_name(dyn.type));
    return;
  }
  root->quantitize_options.quant = DYN_get_static_ratio(dyn);
}

int getQuantitizeType(void){
  return root->quantitize_options.type;
}

void setQuantitizeType(int type){
  if (type>5 || type<1){
    handleError("setQuantitizeType: type must be 1, 2, 3, 4, or 5. (got %d)", type);
  }else
    root->quantitize_options.type = type;
}
  

// TODO. Should return ratio
Place getGrid(void){
  return p_Create(0, (int)root->grid_numerator, (int)root->grid_denominator);
}

void configQuantitize(int windownum){
  struct Tracker_Windows *window=getWindowFromNum(windownum);
  if(window==NULL) return;

  SetQuantitize_CurrPos(window);
}

/************** Glissando ******************/

void glissando(int windownum){
  struct Tracker_Windows *window=getWindowFromNum(windownum);
  if(window==NULL) return;

  Glissando_CurrPos(window);
  window->must_redraw = true;
}

