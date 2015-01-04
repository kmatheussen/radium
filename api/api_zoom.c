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
  int num_lines_after = GFX_GetInteger(window,NULL,temp, 1, 100000);
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
    GFX_Message(NULL, "No range in block. Select range by pressing Left Meta + b");
    return;
  }

  int realline1 = wblock->rangey1;
  int realline2 = wblock->rangey2;

  int line1 = wblock->reallines[realline1]->l.p.line;
  int line2 = wblock->reallines[realline2]->l.p.line;

  int num_lines_before = line2-line1;

  if(num_lines_before==0)
    num_lines_before++;

  printf("********* realline1: %d, realline2: %d\n",realline1,realline2);
  printf("line1: %d, line2: %d, num_lines_before: %d\n",line1,line2,num_lines_before);

  char temp[1024];
  sprintf(temp, "Num lines in range (now %d) >",num_lines_before);
  int num_lines_after = GFX_GetInteger(window,NULL,temp, 1, 100000);
  if (num_lines_after<1)
    return;

  EXPAND_Block_from_range_CurrPos(window, wblock, num_lines_after);
}

void lineZoomBlock(int numlines, int blocknum, int windownum){
  struct Tracker_Windows *window;
  struct WBlocks *wblock = getWBlockFromNumA(windownum, &window, blocknum);
  if (wblock==NULL)
    return;

  LineZoomBlockInc(window, wblock, numlines);
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


