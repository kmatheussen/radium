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

#include "api_common_proc.h"


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


