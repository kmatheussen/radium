/* Copyright 2014 Kjetil S. Matheussen

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
#include "../common/undo_reltemposlider_proc.h"
#include "../common/gfx_wblocks_reltempo_proc.h"
#include "../common/gfx_statusbar_proc.h"

#include "api_common_proc.h"

extern struct Root *root;

int getReltempoSliderX1(void){
  return root->song->tracker_windows->wblock->reltempo.x1;
}
int getReltempoSliderY1(void){
  return root->song->tracker_windows->wblock->reltempo.y1;
}
int getReltempoSliderX2(void){
  return root->song->tracker_windows->wblock->reltempo.x2;
}
int getReltempoSliderY2(void){
return root->song->tracker_windows->wblock->reltempo.y2;
}

float getReltempo(int blocknum, int windownum){
  struct WBlocks *wblock = getWBlockFromNum(windownum, blocknum);
  if (wblock==NULL)
    return 0.0f;
  else
    return wblock->block->reltempo;
}

void undoReltempo(void){
  struct Tracker_Windows *window = root->song->tracker_windows;
  Undo_RelTempoSlider(window,window->wblock);
}

static void update_statusbar(struct Tracker_Windows *window){
  struct WBlocks *wblock = window->wblock;
  GFX_SetChangeInt(window,wblock,"Block RelTempo 0.001*",(int)(wblock->block->reltempo*1000));
  GFX_DrawStatusBar(window,wblock);
}

void setReltempo(float reltempo){
  struct Tracker_Windows *window = root->song->tracker_windows;
  struct WBlocks *wblock = window->wblock;
  wblock->block->reltempo=R_BOUNDARIES(
    MINBLOCKRELTIME,
    reltempo,
    MAXBLOCKRELTIME
  );

  update_statusbar(window);
  DrawBlockRelTempo(window,wblock);
}

void showReltempoInStatusbar(void){
  struct Tracker_Windows *window = root->song->tracker_windows;
  update_statusbar(window);
}

float getMinReltempo(void){
  return MINBLOCKRELTIME;
}

float getMaxReltempo(void){
  return MAXBLOCKRELTIME;
}
