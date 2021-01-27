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

#include <QFont>
#include <QApplication>

#include <QSet> // needed to get ProtectedS7FuncVector

#include "../common/nsmtracker.h"

#include "../common/visual_proc.h"
#include "../common/window_config_proc.h"
#include "../common/block_properties_proc.h"
#include "../common/OS_visual_input.h"
#include "../common/OS_string_proc.h"
#include "../embedded_scheme/s7extra_proc.h"

#include "../midi/midi_instrument_proc.h"

#ifdef _AMIGA
#include "Amiga_colors_proc.h"
#endif

#include "api_requesters_proc.h"

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

void setEditorFont(const_char* fontdescr){
  GFX_SetEditorFont(fontdescr);
}

void setSystemFont(const_char* fontdescr){
  GFX_SetSystemFont(fontdescr);
}

const_char* getEditorFont(void){
  return GFX_GetEditorFont();
}

const_char* getSystemFont(void){
  return GFX_GetSystemFont();
}

const_char* getSampleBrowserFont(bool mono_font){
  static bool s_has_inited = false;
  static char *s_f, *s_f_mono;
  static int s_last_point_size = -1;
  int point_size = QApplication::font().pointSize();

  if(s_has_inited==false || s_last_point_size!=point_size){
    QFont soundfile_font_mono, soundfile_font;

    soundfile_font_mono.setFamily("Bitstream Vera Sans Mono");
    soundfile_font_mono.setBold(true);
    soundfile_font_mono.setStyleName("Bold");

    soundfile_font.fromString("Lato,10,-1,5,75,0,0,0,0,0,Bold");
    soundfile_font.setBold(true);
    soundfile_font.setStyleName("Bold");

    soundfile_font_mono.setPointSize(point_size);
    soundfile_font.setPointSize(point_size);

    s_f_mono = strdup(soundfile_font_mono.toString().toUtf8().constData());
    s_f = strdup(soundfile_font.toString().toUtf8().constData());
    
    s_has_inited = true;
    s_last_point_size = point_size;
  }

  if(mono_font)
    return s_f_mono;
  else
    return s_f;
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

void configBlock(int blocknum, int windownum){
  struct WBlocks *wblock = getWBlockFromNum(windownum, blocknum);
  if (wblock != NULL)
    Block_Properties_CurrPos(wblock->block);
}

void configVST(int windownum){
  struct Tracker_Windows *window=getWindowFromNum(windownum);if(window==NULL) return;
  OS_VST_config(window);
}

bool vstConfigOpen(void){
  return OS_VST_config_visible();
}

filepath_t getLoadFilename(const_char *text, const_char *filetypes, filepath_t dir, const_char *type){
  struct Tracker_Windows *window=getWindowFromNum(-1);if(window==NULL) return make_filepath(L"");
  if (!strcmp(type,""))
    type = NULL;

  return GFX_GetLoadFileName(window, NULL, text, dir, filetypes, type, true);
}

filepath_t getSaveFilename(const_char *text, const_char *filetypes, filepath_t dir, const_char *type, const_char *default_suffix){
  struct Tracker_Windows *window=getWindowFromNum(-1);if(window==NULL) return make_filepath(L"");
  if (!strcmp(type,""))
    type = NULL;

  return GFX_GetSaveFileName(window, NULL, text, dir, filetypes, type, default_suffix, true);
}

static int g_req_counter = 0;       // these two variables shouldn't be necessary, but it's to avoid crash
static int g_num_req_requests = 0;  // if opening several requesters at the same time or closing the requester while waiting for the result of a request. (need to make the requesters async)

static ReqType g_requester = NULL;

void openRequester(const_char *text, int width, int height){
  struct Tracker_Windows *window=getWindowFromNum(-1);if(window==NULL) return;

  g_req_counter++;

  if(g_requester == NULL) {
    R_ASSERT(g_req_counter==1);
    g_requester = GFX_OpenReq(window,width,height,text);    
  }else
    R_ASSERT(g_req_counter!=1);
}

void closeRequester(void){
  struct Tracker_Windows *window=getWindowFromNum(-1);if(window==NULL) return;

  if (g_req_counter <= 0){
    R_ASSERT_NON_RELEASE(g_req_counter==0);
    handleError("closeRequester: The requester is not open");
    return;
  }
  
  g_req_counter--;

  if (g_req_counter==0){    
    if(g_requester!=NULL){
      if (g_num_req_requests==0){
        GFX_CloseReq(window, g_requester);
        g_requester = NULL;
      }else
        handleError("closeRequester: calling closeRequester while still doing request. Tip: Use safeToCallCloseRequester()");
    }else{
      R_ASSERT(false);
    }
  }
}

bool safeToCallCloseRequester(void){
  if (g_req_counter==1)
    return g_num_req_requests==0;

  else if (g_req_counter > 1)
    return true;

  else{
    R_ASSERT_NON_RELEASE(false);
    return false;
  }
}

#define PREREQ(Retval)                                                  \
  struct Tracker_Windows *window=NULL;                                  \
  if (!standalone){                                                     \
    window = getWindowFromNum(-1);                                      \
    if(window==NULL)                                                    \
      return Retval;                                                    \
  }                                                                     \
                                                                        \
  bool custom_requester = false;                                        \
  ReqType requester;                                                    \
                                                                        \
  if (g_requester==NULL){                                               \
    requester = GFX_OpenReq(window,100, 100, "");                       \
    custom_requester = true;                                            \
  } else                                                                \
    requester = g_requester;                                            \
                                                                        \
  g_num_req_requests++;                                                 \
                                                                        \
  if (strcmp(default_value, "")){                                       \
    printf("Setting default to -%s-\n",default_value);GFX_SetString(requester, default_value);}


#define POSTREQ(ret)                            \
  g_num_req_requests--;                         \
  if (custom_requester)                         \
    GFX_CloseReq(window, requester);


int requestInteger(const_char *text, int min, int max, bool standalone, const char *default_value){
  PREREQ(min-1);
  
  int ret = GFX_GetInteger(NULL, g_requester, text, min, max, true);

  POSTREQ();
  
  return ret;
}

float requestFloat(const_char *text, float min, float max, bool standalone, const_char *default_value){
  PREREQ(min-1);

  float ret = GFX_GetFloat(NULL, requester, text, min, max, true);
  
  POSTREQ();
  
  return ret;
}

const_char* requestString(const_char *text, bool standalone, const_char* default_value){
  PREREQ("");

  const char *ret = GFX_GetString(window, requester, text, true);

  POSTREQ();
  
  if(ret==NULL)
    return "";

  return ret;
}

const_char* requestWString(const_char *text, bool standalone, const_char* default_w_value){
  const_char* default_value = STRING_get_chars(w_path_to_path(default_w_value));

  PREREQ("");

  const wchar_t *ret = GFX_GetWString(window, requester, text, true);

  POSTREQ();

  return path_to_w_path(ret);
}

bool requestWasCancelled(void){
  return g_reqtype_cancelled;
}

int64_t API_simplePopupMenu(const char *texts, std::function<void(int,bool)> callback3){
  const vector_t vec = GFX_MenuParser(texts, "%");
  return GFX_Menu3(vec,callback3);
}

/*
int simplePopupMenu(const char *texts){
  struct Tracker_Windows *window=getWindowFromNum(-1);
  const vector_t vec = GFX_MenuParser(texts, "%");
  return GFX_Menu(window, NULL,"",vec,true);
}
*/

int64_t popupMenu(dynvec_t strings, func_t* callback){
  struct Tracker_Windows *window=getWindowFromNum(-1);
  
  vector_t vec = VECTOR_create(strings.num_elements);

  for(int i=0;i<strings.num_elements;i++){
    if (strings.elements[i].type != STRING_TYPE){
      handleError("popupMenu: Element #%d is not a string. Found: %s", i, DYN_type_name(strings.elements[i].type));
      return -1;
    }
    vec.elements[i] = STRING_get_chars(strings.elements[i].string);
  }

  //printf("   NUM_elements: %d\n", vec.num_elements);
  
  return GFX_Menu2(window, NULL, "", vec, callback, true, true);
}

static radium::ProtectedS7FuncVector g_popupmenu_closed_callbacks(true);

void addPopupMenuClosedCallback(func_t* callback){
  if (g_popupmenu_closed_callbacks.push_back(callback)==false)
    handleError("addPopupMenuClosedCallback: Callback %p already added\n", callback);
}

static bool removePopupMenuClosedCallback2(func_t *callback){
  int num_removed = g_popupmenu_closed_callbacks.removeAll(callback);
  R_ASSERT_NON_RELEASE(num_removed==0 || num_removed==1);
  
  return num_removed > 0;
}

void removePopupMenuClosedCallback(func_t* callback){
  if (!removePopupMenuClosedCallback2(callback))
    handleError("removePopupMenuClosedCallback: Could not find deleted callback %p\n", callback);
}

void API_call_me_when_a_popup_menu_has_been_closed(void){
  QVector<func_t*> to_remove;

  g_popupmenu_closed_callbacks.safe_for_all(true, [&to_remove](func_t *callback){

      if (S7CALL(bool_void, callback)==false)
        to_remove.push_back(callback);

      return true;
      
    });

  for(auto *callback : to_remove){
    printf("   API_call_me_when_a_popup_menu_has_been_closed: Calling removePopupMenuClosedCallback for %p\n", callback);
    removePopupMenuClosedCallback2(callback);
  }
}

int64_t getLastHoveredPopupMenuEntry(void){
  return g_last_hovered_menu_entry_guinum;
}

void hoverPopupMenuEntry(int entryid){
  GFX_HoverMenuEntry(entryid);
}

void rightclickPopupMenuEntry(int entryid){
  GFX_RightclickMenuEntry(entryid);
}

void colorDialog(const char *initial_color, int64_t parentguinum, func_t* callback){
  GFX_color_dialog(initial_color, parentguinum, callback);
}

const_char* requestMidiPort(void){
  struct Tracker_Windows *window=getWindowFromNum(-1);if(window==NULL) return "";
  const char *ret = MIDIrequestPortName(window, g_requester, false, true);
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
  
  vector_t v={};
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

// Note! This function is called from the error handler.
void addMessage(const char *html){
  const char *funcname = "add-message-window-message";
  
  static bool gotit = false;
  if (g_qtgui_has_stopped==false && (gotit || s7extra_is_defined(funcname))){
    S7CALL2_NO_HISTORY(void_charpointer,funcname, html);
    gotit = true;
  } else {
    showMessage(html, g_uninitialized_dyn);
  }
}

void showAsyncMessage(const_char* text){
  evalScheme(talloc_format("(show-async-message :text (list \"%s\"))", toBase64(text)));
}

void showWarning(const_char *text){
  RWarning("%s", text);
}

void showError(const_char *text){
  RError("%s", text);
}

void openProgressWindow(const char *message, bool message_is_base64){
  if (message_is_base64)
    GFX_OpenProgress2(STRING_fromBase64(STRING_create(message)));
  else
    GFX_OpenProgress(message);
}
void showProgressWindowMessage(const char *message, bool force_show){
  if (progressWindowIsOpen()==false){
    handleError("Progress window is not open");
    return;
  }
  GFX_ShowProgressMessage(message, force_show);
}
void showProgressWindowMessageIfOpen(const char *message, bool force_show){
  if (progressWindowIsOpen()==false)
    return;
  else
    GFX_ShowProgressMessage(message, force_show);
}
void closeProgressWindow(void){
  GFX_CloseProgress();
}

bool progressWindowIsOpen(void){
  return GFX_ProgressIsOpen();
}

void showHomeHelpWindow(void){
  S7CALL2(void_charpointer, "FROM-C-show-help-window", "help/gui_framed.html");
}
void showMixerHelpWindow(void){
  S7CALL2(void_charpointer, "FROM-C-show-help-window", "help/mixer_framed.html");
}
void showChanceHelpWindow(void){
  S7CALL2(void_charpointer, "FROM-C-show-help-window", "help/chancetext_editor_framed.html");
}
void showVelocityHelpWindow(void){
  S7CALL2(void_charpointer, "FROM-C-show-help-window", "help/velocitytext_editor_framed.html");
}
void showFXTextHelpWindow(void){
  S7CALL2(void_charpointer, "FROM-C-show-help-window", "help/fxtext_editor_framed.html");
}
void showSwingHelpWindow(void){
  S7CALL2(void_charpointer, "FROM-C-show-help-window", "help/swingtext_editor_framed.html");
}
void showKeybindingsHelpWindow(void){
  S7CALL2(void_charpointer, "FROM-C-show-help-window", "help/keybindings_framed.html");
}

void showBpmTextHelpWindow(void){
  S7CALL2(void_charpointer, "FROM-C-show-help-window", "help/bpmtext_editor_framed.html");
}
void showLpbTextHelpWindow(void){
  S7CALL2(void_charpointer, "FROM-C-show-help-window", "help/lpbtext_editor_framed.html");
}
void showSignatureTextHelpWindow(void){
  S7CALL2(void_charpointer, "FROM-C-show-help-window", "help/signaturetext_editor_framed.html");
}
void showLzTrackHelpWindow(void){
  S7CALL2(void_charpointer, "FROM-C-show-help-window", "help/lztrack_editor_framed.html");
}
void showTempoAutomationTrackHelpWindow(void){
  S7CALL2(void_charpointer, "FROM-C-show-help-window", "help/tempoautomationtrack_editor_framed.html");
}
