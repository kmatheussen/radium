/* Copyright 2003 Kjetil S. Matheussen

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

#pragma once


#include "visual_proc.h"


extern LANGSPEC void GFX_ConfigSystemFont(void);
//extern LANGSPEC char *GFX_SelectEditFont(struct Tracker_Windows *tvisual);
extern LANGSPEC void GFX_ResetFontSize(struct Tracker_Windows *tvisual);
extern LANGSPEC void GFX_IncFontSize(struct Tracker_Windows *tvisual, int pixels);

extern LANGSPEC void GFX_SetDefaultFont(struct Tracker_Windows *tvisual);
extern LANGSPEC void GFX_SetDefaultSystemFont(struct Tracker_Windows *tvisual);

extern LANGSPEC void GFX_SetSystemFont(const char *fontdescr);
extern LANGSPEC const char *GFX_GetSystemFont(void);
extern LANGSPEC void GFX_SetEditorFont(const char *fontdescr);
extern LANGSPEC const char *GFX_GetEditorFont(void);

extern LANGSPEC void CancelMaybeNavigateMenus(void);

#ifdef USE_QT4
class QWidget;
void RT_RTWIDGET_mark_needing_update(int pos);
int RTWIDGET_allocate_slot(QWidget *widget);
void RTWIDGET_release_slot(int pos);
#endif

extern LANGSPEC int64_t GFX_SetStatusBar(const char *title);
extern LANGSPEC void GFX_RemoveStatusbarText(int64_t id);
extern LANGSPEC void GFX_SetWindowTitle(struct Tracker_Windows *tvisual,const wchar_t *title);

// Better to use these functions than to check tevent.keyswitch, since tevent.keyswitch is resetted when the mouse enters and leaves the window.
extern LANGSPEC bool ControlPressed(void); // linux/windows: Ctrl, osx: Cmd
extern LANGSPEC bool ShiftPressed(void);
extern LANGSPEC bool AltPressed(void);
extern LANGSPEC bool MetaPressed(void);
extern LANGSPEC bool Control2Pressed(void);
extern LANGSPEC bool HorizontalModifierPressed(void);
extern LANGSPEC bool VerticalModifierPressed(void);

#ifdef USE_QT4
bool Control2Pressed(Qt::KeyboardModifiers modifiers);
bool HorizontalModifierPressed(Qt::KeyboardModifiers modifiers);
bool VerticalModifierPressed(Qt::KeyboardModifiers modifiers);
#endif


#ifdef USE_QT4
class QDropEvent;
void handleDropEvent(QString filename, float x);
#endif

extern LANGSPEC void SetNormalPointer(int64_t guinum);
extern LANGSPEC void SetPointingPointer(int64_t guinum);
extern LANGSPEC void SetOpenHandPointer(int64_t guinum);
extern LANGSPEC void SetClosedHandPointer(int64_t guinum);
extern LANGSPEC void SetBlankPointer(int64_t guinum);
extern LANGSPEC void SetDiagResizePointer(int64_t guinum);
extern LANGSPEC void SetHorizResizePointer(int64_t guinum);
extern LANGSPEC void SetHorizSplitPointer(int64_t guinum);
extern LANGSPEC void SetVerticalResizePointer(int64_t guinum);
extern LANGSPEC void SetVerticalSplitPointer(int64_t guinum);
extern LANGSPEC void MovePointer(struct Tracker_Windows *tvisual, float x, float y);
extern LANGSPEC void MoveAbsPointer(struct Tracker_Windows *tvisual, float x, float y);
extern LANGSPEC WPoint GetPointerPos(struct Tracker_Windows *tvisual);
extern LANGSPEC WPoint GetAbsPointerPos(struct Tracker_Windows *tvisual);
extern LANGSPEC uint64_t GetMouseButtons(void); // Returns TR_LEFTMOUSEDOWN<<2|TR_RIGHTMOUSEDOWN<<2|TR_MIDDLEOUSEDOWN<<2;
extern LANGSPEC double GetDoubleClickInterval(void);
extern LANGSPEC Area GetScreenSize(struct Tracker_Windows *tvisual);

extern LANGSPEC void MouseMoveRelative(float x, float y, float dx, float dy); // Called from windows.
  
extern bool g_reqtype_cancelled; // Set to true if user pressed Escape or closed the window. Can be checked after calling ReadString/GetInteger/GetFloat/GetString. Variable is valid until next call to any of those functions.

// Note that For the ReqType functions, tvisual might be NULL. The reasons is that it can be called when loading, or when starting up.
extern LANGSPEC ReqType GFX_OpenReq(struct Tracker_Windows *tvisual,int width,int height,const char *title);
extern LANGSPEC void GFX_CloseReq(struct Tracker_Windows *tvisual,ReqType reqtype);

extern LANGSPEC void GFX_WriteString(ReqType reqtype,const char *text);
extern LANGSPEC void GFX_SetString(ReqType das_reqtype,const char *text);
extern LANGSPEC void GFX_ReadString(ReqType reqtype,char *buffer,int bufferlength,bool program_state_is_valid);
extern LANGSPEC const wchar_t *GFX_ReadWString(ReqType das_reqtype, bool program_state_is_valid);
extern LANGSPEC int GFX_ReqTypeMenu(
                                    struct Tracker_Windows *tvisual,
                                    ReqType reqtype,
                                    const char *seltext,
                                    const vector_t v,
                                    bool program_state_is_valid
                                    );

extern LANGSPEC int GFX_GetInteger(struct Tracker_Windows *tvisual,ReqType reqtype,const char *text,int min,int max,bool program_state_is_valid);

extern LANGSPEC float GFX_GetFloat(struct Tracker_Windows *tvisual,ReqType reqtype,const char *text,float min,float max,bool program_state_is_valid);

extern LANGSPEC const char *GFX_GetString(struct Tracker_Windows *tvisual,ReqType reqtype,const char *text,bool program_state_is_valid);
extern LANGSPEC const wchar_t *GFX_GetWString(struct Tracker_Windows *tvisual,ReqType reqtype,const char *text,bool program_state_is_valid);
  
#ifdef __cplusplus

#include <functional>

// returns guinum of popup menu
int64_t GFX_Menu3(
                  const vector_t &v,
                  std::function<void(int,bool)> callback
                  );

#include "../api/api_requesters_proc.h"

// returns guinum of popup menu
static inline int64_t GFX_SimpleMenu(const char *texts, std::function<void(int,bool)> callback3){
  return API_simplePopupMenu(texts, callback3);
}

#endif

extern int64_t g_last_hovered_menu_entry_guinum;
extern LANGSPEC void GFX_HoverMenuEntry(int entryid);
extern LANGSPEC void GFX_RightclickMenuEntry(int entryid);

// is_async: returns guinum. !is_async: returns selection.
extern LANGSPEC int64_t GFX_Menu2(
                              struct Tracker_Windows *tvisual,
                              ReqType reqtype,
                              const char *seltext,
                              const vector_t v,
                              func_t *callback,
                              bool is_async,
                              bool program_state_is_valid
                              );

extern LANGSPEC int GFX_Menu(
                             struct Tracker_Windows *tvisual,
                             ReqType reqtype,
                             const char *seltext,
                             const vector_t v,
                             bool program_state_is_valid
                             );

extern LANGSPEC vector_t GFX_MenuParser(const char *texts, const char *separator);

extern LANGSPEC filepath_t GFX_GetLoadFileName(
                                               struct Tracker_Windows *tvisual,
                                               ReqType reqtype,
                                               const char *seltext,
                                               const filepath_t dir,
                                               const char *postfixes,
                                               const char *type,
                                               bool program_state_is_valid
                                               );

extern LANGSPEC filepath_t GFX_GetSaveFileName(
                                               struct Tracker_Windows *tvisual,
                                               ReqType reqtype,
                                               const char *seltext,
                                               const filepath_t dir,
                                               const char *postfixes,
                                               const char *type,
                                               const char *default_suffix,
                                               bool program_state_is_valid
                                               );

extern LANGSPEC void GFX_OS_update_bottombar(void);
extern LANGSPEC void GFX_OS_UpdateKeyOctave(void);
extern LANGSPEC void OS_GFX_NumUndosHaveChanged(int num_undos, bool redos_are_available, bool has_unsaved_undos);
extern LANGSPEC void OS_GFX_SetVolume(int value);
extern LANGSPEC void OS_GFX_IncVolume(int how_much);

extern LANGSPEC void GFX_update_instrument_patch_gui(struct Patch *patch);
extern LANGSPEC void GFX_remove_patch_gui(struct Patch *patch); // Also deletes the audio object itself. (yes, it's messy)

struct SoundPlugin;
extern LANGSPEC void GFX_OS_set_system_volume_plugin(struct SoundPlugin *plugin);
extern LANGSPEC bool GFX_OS_patch_is_system_out(struct Patch *patch);
extern LANGSPEC struct Patch *GFX_OS_get_system_out(void);
//extern LANGSPEC void GFX_OS_set_system_volume_peak_pointers(float *pointers, int num_channels);

extern LANGSPEC void GFX_update_all_instrument_widgets(void);

extern LANGSPEC void GFX_PP_Update(struct Patch *patch, bool is_loading);
extern LANGSPEC void GFX_PP_Update_even_if_locked(struct Patch *patch, bool is_loading);

extern LANGSPEC void *OS_GFX_get_native_main_window(void);
extern LANGSPEC bool OS_GFX_main_window_has_focus(void);

extern LANGSPEC int OS_get_main_window_width(void);
extern LANGSPEC int OS_get_main_window_height(void);


extern LANGSPEC void OS_VST_config(struct Tracker_Windows *window);
extern LANGSPEC bool OS_VST_config_visible(void);

extern LANGSPEC void MIDILEARN_PREFS_open(void);

#ifdef __cplusplus
namespace radium{
  struct MidiLearn;
}
extern void MIDILEARN_PREFS_add(struct radium::MidiLearn *midi_learn);
extern void MIDILEARN_PREFS_remove(struct radium::MidiLearn *midi_learn);
#endif

extern LANGSPEC void TOOLS_open(void);
