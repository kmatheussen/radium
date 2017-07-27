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
#include "../common/OS_string_proc.h"
#include "../embedded_scheme/s7extra_proc.h"

#include "../midi/midi_i_plugin_proc.h"

#ifdef _AMIGA
#include "Amiga_colors_proc.h"
#endif

#include "api_common_proc.h"

extern struct Root *root;


void setDefaultColors1(void){
  GFX_SetDefaultColors1(getWindowFromNum(-1));
}

void setDefaultColors2(void){
  GFX_SetDefaultColors2(getWindowFromNum(-1));
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
  //RWarning("Warning! (?)"); // warning window test
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

const char *getLoadFilename(const_char *text, const_char *filetypes, const_char *dir, const_char *type){
  struct Tracker_Windows *window=getWindowFromNum(-1);if(window==NULL) return "";
  if (!strcmp(type,""))
    type = NULL;

  const wchar_t *ret;
  
  ret = GFX_GetLoadFileName(window, NULL, text, STRING_create(dir), filetypes, type, true);
  
  if(ret==NULL)
    return "";
  else
    return STRING_get_chars(ret);
}

const char *getSaveFilename(const_char *text, const_char *filetypes, const_char *dir, const_char *type){
  struct Tracker_Windows *window=getWindowFromNum(-1);if(window==NULL) return "";
  if (!strcmp(type,""))
    type = NULL;
  const wchar_t *ret;

  ret = GFX_GetSaveFileName(window, NULL, text, STRING_create(dir), filetypes, type, true);
    
  if(ret==NULL)
    return "";
  else
    return STRING_get_chars(ret);
}


static ReqType g_requester = NULL;

void openRequester(const_char *text, int width, int height){
  struct Tracker_Windows *window=getWindowFromNum(-1);if(window==NULL) return;

  g_requester = GFX_OpenReq(window,width,height,text);
}

void closeRequester(void){
  struct Tracker_Windows *window=getWindowFromNum(-1);if(window==NULL) return;

  if(g_requester!=NULL){
    GFX_CloseReq(window, g_requester);
    g_requester = NULL;
  }
}

int requestInteger(const_char *text, int min, int max, bool standalone){
  if (standalone)
    return GFX_GetInteger(NULL, g_requester, text, min, max, true);

  struct Tracker_Windows *window=getWindowFromNum(-1);if(window==NULL) return min-1;
  return GFX_GetInteger(window, g_requester, text, min, max, true);
}

float requestFloat(const_char *text, float min, float max, bool standalone){
  if (standalone)
    return GFX_GetFloat(NULL, g_requester, text, min, max, true);

  struct Tracker_Windows *window=getWindowFromNum(-1);if(window==NULL) return min-1.0f;
  return GFX_GetFloat(window, g_requester, text, min, max, true);
}

const_char* requestString(const_char *text, bool standalone){
  char *ret;

  if (standalone)
    ret = GFX_GetString(NULL, g_requester, text, true);
  else {
    struct Tracker_Windows *window=getWindowFromNum(-1);if(window==NULL) return "";
    ret = GFX_GetString(window, g_requester, text, true);
  }

  if(ret==NULL)
    ret="";
  return ret;
}

int requestMenu(const_char *text, PyObject* arguments){
  handleError("requestMenu not implemented");
  return 0;
}

int simplePopupMenu(const char *texts){
  struct Tracker_Windows *window=getWindowFromNum(-1);
  const vector_t vec = GFX_MenuParser(texts, "%");
  return GFX_Menu(window, NULL,"",vec,true);
}

void popupMenu(dyn_t strings, func_t* callback){
  struct Tracker_Windows *window=getWindowFromNum(-1);
  
  if (strings.type != ARRAY_TYPE){
    handleError("popupMenu: Excpected array as first argument, found %s", DYN_type_name(strings.type));
    return;
  }
  dynvec_t *dynvec = strings.array;

  vector_t vec = VECTOR_create(dynvec->num_elements);

  for(int i=0;i<dynvec->num_elements;i++){
    if (dynvec->elements[i].type != STRING_TYPE){
      handleError("popupMenu: Element #%d is not a string. Found: %s", i, DYN_type_name(dynvec->elements[i].type));
      return;
    }
    vec.elements[i] = STRING_get_chars(dynvec->elements[i].string);
  }

  //printf("   NUM_elements: %d\n", vec.num_elements);
  
  GFX_Menu2(window, NULL, "", vec, callback, true, true);
}


void colorDialog(const char *initial_color, int64_t parentguinum, func_t* callback){
  GFX_color_dialog(initial_color, parentguinum, callback);
}

const_char* requestMidiPort(void){
  struct Tracker_Windows *window=getWindowFromNum(-1);if(window==NULL) return "";
  char *ret = MIDIrequestPortName(window, g_requester, false, true);
  if(ret==NULL)
    ret="";
  return ret;
}

const_char* showMessage(const char *text, dyn_t buttons){
  if (buttons.type==UNINITIALIZED_TYPE){
    GFX_Message2(NULL, true, "%s", text);
    return "Ok";
  }

  if (buttons.type!=ARRAY_TYPE){
    handleError("showMessage: Argument 1: Expected ARRAY_TYPE, found %s", DYN_type_name(buttons.type));
    return "";
  }
  
  vector_t v={0};
  for(int i=0;i<buttons.array->num_elements;i++){
    dyn_t button = buttons.array->elements[i];
    if (button.type!=STRING_TYPE){
      handleError("showMessage: Button #%d: Expected STRING_TYPE, found %s", i, DYN_type_name(button.type));
      return "";
    }
    VECTOR_push_back(&v, STRING_get_chars(button.string));
  }

  int ret = GFX_SafeMessage(&v, "%s", text);
  if (ret<0 || ret>= buttons.array->num_elements) // don't think this can happen though.
    return "";

  return STRING_get_chars(buttons.array->elements[ret].string);
}

const_char* showMessage2(const char *text){
  return showMessage(text, g_uninitialized_dyn);
}
  
extern bool g_qtgui_has_stopped;

void addMessage(const char *text){
  const char *funcname = "add-message-window-message";
  
  static bool gotit = false;
  if (g_qtgui_has_stopped==false && (gotit || s7extra_is_defined(funcname))){
    S7CALL2(void_charpointer,funcname, text);
    gotit = true;
  } else {
    showMessage(text, g_uninitialized_dyn);
  }
}

void showAsyncMessage(const_char* text){
  evalScheme(talloc_format("(show-async-message :text \"%s\")", text));
}

void showWarning(const_char *text){
  RWarning("%s", text);
}

void showError(const_char *text){
  RError("%s", text);
}

void openProgressWindow(const char *message){
  GFX_OpenProgress(message);
}
void showProgressWindowMessage(const char *message){
  GFX_ShowProgressMessage(message);
}
void closeProgressWindow(void){
  GFX_CloseProgress();
}

void showHomeHelpWindow(void){
  evalScheme("(FROM-C-show-help-window \"help/home.html\")");
}
void showMixerHelpWindow(void){
  evalScheme("(FROM-C-show-help-window \"help/mixer.html\")");
}
void showChanceHelpWindow(void){
  evalScheme("(FROM-C-show-help-window \"help/chancetext.html\")");
}
void showVelocityHelpWindow(void){
  evalScheme("(FROM-C-show-help-window \"help/velocitytext.html\")");
}
void showFXHelpWindow(void){
  evalScheme("(FROM-C-show-help-window \"help/fxtext.html\")");
}
void showSwingHelpWindow(void){
  evalScheme("(FROM-C-show-help-window \"help/swingtext.html\")");
}
void showKeybindingsHelpWindow(void){
  evalScheme("(FROM-C-show-help-window \"help/keybindings.html\")");
}
