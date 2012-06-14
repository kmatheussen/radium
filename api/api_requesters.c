/* Copyright 2001-2012 Kjetil S. Matheussen

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
#include "radium_proc.h"

#include "../common/nsmtracker.h"

#include "../common/visual_proc.h"
#include "../common/window_config_proc.h"
#include "../common/block_properties_proc.h"


#ifdef _AMIGA
#include "Amiga_colors_proc.h"
#endif

#include "api_common_proc.h"

extern struct Root *root;


void configColors(int windownum){
#ifdef _AMIGA
  Amiga_ConfigColors();
#else
  struct Tracker_Windows *window=getWindowFromNum(windownum);if(window==NULL) return;
  GFX_ConfigColors(window);
#endif
}

void configWindow(int windownum){
  struct Tracker_Windows *window=getWindowFromNum(windownum);if(window==NULL) return;
  Window_config(window);
}

void configBlock(int windownum){
  struct Tracker_Windows *window=getWindowFromNum(windownum);if(window==NULL) return;
  Block_Properties_CurrPos(window);
}


char *getLoadFilename(char *text, char *dir){
  struct Tracker_Windows *window=getWindowFromNum(-1);if(window==NULL) return "";
  return GFX_GetLoadFileName(window, NULL, text, dir);
}

char *getSaveFilename(char *text, char *dir){
  struct Tracker_Windows *window=getWindowFromNum(-1);if(window==NULL) return "";
  return GFX_GetSaveFileName(window, NULL, text, dir);
}

