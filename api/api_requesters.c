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
#include "../common/OS_visual_input.h"

#include "../midi/midi_i_plugin_proc.h"

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

void setDefaultColors(void){
  GFX_SetDefaultColors(getWindowFromNum(-1));
}

void configSystemFont(void){
  GFX_ConfigSystemFont();
}

void configFonts(int windownum){
  struct Tracker_Windows *window=getWindowFromNum(windownum);if(window==NULL) return;
  GFX_ConfigFonts(window);
}

void setDefaultEditorFont(void){
  struct Tracker_Windows *window=getWindowFromNum(-1);if(window==NULL) return;
  GFX_SetDefaultFont(window);
}

void setDefaultSystemFont(void){
  struct Tracker_Windows *window=getWindowFromNum(-1);if(window==NULL) return;
  GFX_SetDefaultSystemFont(window);
}

void configWindow(int windownum){
  struct Tracker_Windows *window=getWindowFromNum(windownum);if(window==NULL) return;
  Window_config(window);
}

void configBlock(int windownum){
  struct Tracker_Windows *window=getWindowFromNum(windownum);if(window==NULL) return;
  Block_Properties_CurrPos(window);
}

void configVST(int windownum){
  struct Tracker_Windows *window=getWindowFromNum(windownum);if(window==NULL) return;
  OS_VST_config(window);
}

const char *getLoadFilename(char *text, char *dir){
  struct Tracker_Windows *window=getWindowFromNum(-1);if(window==NULL) return "";
  const char *ret = GFX_GetLoadFileName(window, NULL, text, dir);
  if(ret==NULL)
    return "";
  else
    return ret;
}

const char *getSaveFilename(char *text, char *dir){
  struct Tracker_Windows *window=getWindowFromNum(-1);if(window==NULL) return "";
  const char *ret = GFX_GetSaveFileName(window, NULL, text, dir);
  if(ret==NULL)
    return "";
  else
    return ret;
}


static ReqType requester = NULL;

void openRequester(char *text, int width, int height){
  struct Tracker_Windows *window=getWindowFromNum(-1);if(window==NULL) return;

  requester = GFX_OpenReq(window,width,height,text);
}

void closeRequester(){
  struct Tracker_Windows *window=getWindowFromNum(-1);if(window==NULL) return;

  if(requester!=NULL){
    GFX_CloseReq(window, requester);
    requester = NULL;
  }
}

int requestInteger(char *text, int min, int max){
  struct Tracker_Windows *window=getWindowFromNum(-1);if(window==NULL) return min-1;
  return GFX_GetInteger(window, requester, text, min, max);
}

float requestFloat(char *text, float min, float max){
  struct Tracker_Windows *window=getWindowFromNum(-1);if(window==NULL) return min-1.0f;
  return GFX_GetInteger(window, requester, text, min, max);
}

char* requestString(char *text){
  struct Tracker_Windows *window=getWindowFromNum(-1);if(window==NULL) return "";
  char *ret = GFX_GetString(window, requester, text);
  if(ret==NULL)
    ret="";
  return ret;
}

int requestMenu(char *text, PyObject* arguments){
  RError("requestMenu not implemented");
  return 0;
}

char* requestMidiPort(void){
  struct Tracker_Windows *window=getWindowFromNum(-1);if(window==NULL) return "";
  char *ret = MIDIrequestPortName(window, requester, false);
  if(ret==NULL)
    ret="";
  return ret;
}

void showWarning(char *text){
  RWarning(text);
}

void showError(char *text){
  RError(text);
}
