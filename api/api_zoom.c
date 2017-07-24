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
#include "../common/reallines_proc.h"
#include "../common/font_proc.h"
#include "../common/expand_proc.h"
#include "../common/expand_proc.h"
#include "../common/OS_visual_input.h"
#include "../common/visual_proc.h"

#include "api_common_proc.h"


void expandBlock(int blocknum){
  struct Tracker_Windows *window;
  struct WBlocks *wblock = getWBlockFromNumA(-1, &window, blocknum);
  if (wblock==NULL)
    return;

  int num_lines_before = wblock->block->num_lines;
  char temp[1024];
  sprintf(temp, "Num lines (now %d) >",num_lines_before);
  int num_lines_after = GFX_GetInteger(window,NULL,temp, 1, 100000,true);
  if (num_lines_after<1)
    return;

  EXPAND_Block_CurrPos(window, wblock, num_lines_after);
}

void expandRange(int blocknum, int windownum){
  struct Tracker_Windows *window;
  struct WBlocks *wblock = getWBlockFromNumA(windownum, &window, blocknum);
  if (wblock==NULL)
    return;

  if (wblock->isranged==false){
    showAsyncMessage("No range in block. Select range by using Left Meta + b");
    return;
  }

  float num_lines_before = 0.0f;

  if (p_Greater_Or_Equal(wblock->rangey2, wblock->rangey1)){
    Place duration = p_Sub(wblock->rangey2, wblock->rangey1);
    num_lines_before = p_float(duration);
  }

  //printf("********* realline1: %d, realline2: %d\n",realline1,realline2);
  //printf("line1: %d, line2: %d, num_lines_before: %d\n",line1,line2,num_lines_before);

  char temp[1024];
  sprintf(temp, "Num lines in range (now %f) >",num_lines_before);
  float num_lines_after = GFX_GetFloat(window,NULL,temp, 0.1, 100000,true);
  if (num_lines_after<=0)
    return;
  
  Place new_duration = p_FromFloat(num_lines_after);

  EXPAND_Block_from_range_CurrPos(window, wblock, new_duration);
}

void lineZoomBlock(int numlines, int blocknum, int windownum){
  struct Tracker_Windows *window;
  struct WBlocks *wblock = getWBlockFromNumA(windownum, &window, blocknum);
  if (wblock==NULL)
    return;

  LineZoomBlockInc(window, wblock, numlines);
}

int getLineZoomBlock(int blocknum, int windownum){
  struct WBlocks *wblock = getWBlockFromNum(windownum, blocknum);
  if (wblock==NULL)
    return 1;
  
  return GetLineZoomBlock(wblock);
}

dyn_t getLineZoomBlockRatio(int blocknum, int windownum){
  int lz = getLineZoomBlock(blocknum, windownum);

  R_ASSERT(lz!=0);
  
  if (lz > 0)
    return DYN_create_int(lz);
  else
    return DYN_create_ratio(make_ratio(1, -lz));
}

void expandLine(int numlines,int windownum){
  struct Tracker_Windows *window=getWindowFromNum(windownum);
  if(window==NULL) return;

  Zoom(window,window->wblock,numlines);
}


void splitLine(int numlines,int windownum){
  struct Tracker_Windows *window=getWindowFromNum(windownum);
  if(window==NULL) return;

  ExpandLineCurrPos(window,numlines);
}

void unsplitLine(int windownum){
  struct Tracker_Windows *window=getWindowFromNum(windownum);
  if(window==NULL) return;

  UnexpandCurrPos(window);
}


void zoom(int incfontsize,int windownum){
  struct Tracker_Windows *window=getWindowFromNum(windownum);
  if(window==NULL) return;

  IncFontSize_CurrPos(window,incfontsize);
}

void unzoom(int windownum){
  struct Tracker_Windows *window=getWindowFromNum(windownum);
  if(window==NULL) return;

  SetFontSizeNormal_CurrPos(window);
}


