/* Copyright 2000 Kjetil S. Matheussen

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

#ifndef RADIUM_VISUAL_PROC_H
#define RADIUM_VISUAL_PROC_H

#include "patch_proc.h"
#include "OS_visual_input.h"

#define TEXT_IGNORE_WIDTH -99999 // Can be used instead of width, not a flag
// flags:
#define TEXT_NOFLAGS 0
#define TEXT_CLEAR 1
#define TEXT_INVERT 2
#define TEXT_CENTER 4
#define TEXT_CLIPRECT 8
#define TEXT_NOTEXT 16
#define TEXT_BOLD 32
#define TEXT_SCALE 64 // Scales the text into the box.

#if 0
// better not expose this function. Many of the GFX_Message messages are never tested, and this particular function can only be called from the main thread.
#ifdef USE_QT4
#include <QString>
int GFX_Message(vector_t *buttons, QString message);
#endif
#endif

extern LANGSPEC void GFX_OpenProgress2(const wchar_t *message);
extern LANGSPEC void GFX_OpenProgress(const char *message);
extern LANGSPEC void GFX_ShowProgressMessage(const char *message, bool force_show);
extern LANGSPEC bool GFX_ProgressIsOpen(void);
extern LANGSPEC void GFX_HideProgress(void);
extern LANGSPEC void GFX_ShowProgress(void);
extern LANGSPEC void GFX_CloseProgress(void);

extern LANGSPEC void GFX_DisablePainting(void);
extern LANGSPEC void GFX_EnablePainting(void);

extern LANGSPEC bool GFX_Message_ignore_questionmark(void);
extern LANGSPEC bool GFX_Message_ask_ignore_question_questionmark(void);
extern LANGSPEC void GFX_Message_call_after_showing(bool clicked_ignore);

// NOTE: Might return -1
#define GFX_SafeMessage(buttons, ...) GFX_Message2(buttons, true, __VA_ARGS__)

//extern LANGSPEC void GFX_addMessage_internal(const char *fmt,...) FORMAT_ATTRIBUTE(1,2);
extern LANGSPEC void GFX_addMessage_internal(const char *message);
//#define GFX_addMessage(...) do{(void)donothing(0 && printf(__VA_ARGS__)); GFX_addMessage_internal(__VA_ARGS__);}while(0) // Add a "printf" call to make the C compiler show warning/error,
#define GFX_addMessage(...) GFX_addMessage_internal(talloc_format_internal(__VA_ARGS__))

extern LANGSPEC const char *GFX_qVersion(void);

extern LANGSPEC void GFX_AddMenuItem(struct Tracker_Windows *tvisual, const char *name, const char *python_command);
extern LANGSPEC void GFX_AddCheckableMenuItem(struct Tracker_Windows *tvisual, const char *name, const char *python_command, int checkval);
extern LANGSPEC void GFX_AddMenuSeparator(struct Tracker_Windows *tvisual);
extern LANGSPEC void GFX_AddMenuMenu(struct Tracker_Windows *tvisual, const char *name, const char *command);
extern LANGSPEC void GFX_GoPreviousMenuLevel(struct Tracker_Windows *tvisual);

extern LANGSPEC bool GFX_MenuVisible(struct Tracker_Windows *tvisual);
extern LANGSPEC void GFX_ShowMenu(struct Tracker_Windows *tvisual);
extern LANGSPEC void GFX_HideMenu(struct Tracker_Windows *tvisual);

extern LANGSPEC void GFX_set_bottom_widget_height(int new_height);


extern LANGSPEC void GFX_disable_mouse_keyboard(void);
extern LANGSPEC void GFX_enable_mouse_keyboard(void);

extern LANGSPEC int GFX_CreateVisual(struct Tracker_Windows *tvisual);
extern LANGSPEC int GFX_ShutDownVisual(struct Tracker_Windows *tvisual);
extern LANGSPEC void GFX_SetMinimumWindowWidth(struct Tracker_Windows *tvisual, int width);

extern LANGSPEC void GFX_PositionUpperLeftArea(struct Tracker_Windows *tvisual, struct WBlocks *wblock);
extern LANGSPEC void GFX_UpdateUpperLeft(struct Tracker_Windows *window, struct WBlocks *wblock);

/*
extern LANGSPEC void GFX_ScheduleRedraw(void);
extern LANGSPEC void GFX_ScheduleEditorRedraw(void);
*/


extern DEFINE_ATOMIC(bool, atomic_must_redraw);
extern DEFINE_ATOMIC(bool, atomic_must_redraw_editor);
extern DEFINE_ATOMIC(struct Patch*, atomic_must_redraw_instrument);
extern DEFINE_ATOMIC(bool, atomic_must_calculate_coordinates);

extern bool g_rt_do_rerendering;

// RT function.
static inline void GFX_ForceScheduleRedraw(void){
  ATOMIC_SET(atomic_must_redraw, true);
}

// RT function.
static inline void GFX_ScheduleRedraw(void){
  if(g_rt_do_rerendering)
    ATOMIC_SET(atomic_must_redraw, true);
}

// RT function.
static inline void GFX_ScheduleEditorRedraw(void){
  if(g_rt_do_rerendering)
    ATOMIC_SET(atomic_must_redraw_editor, true);
}

// RT function.
static inline void GFX_ForceScheduleEditorRedraw(void){  
  ATOMIC_SET(atomic_must_redraw_editor, true);
}

// RT function.
static inline void GFX_ScheduleEditorRedrawIfPatchIsCurrentlyVisible(const struct Patch *patch){
  if (ATOMIC_GET(atomic_must_redraw_editor)==true)
    return;

  const struct Blocks *block = ATOMIC_GET(g_curr_block);
  if (block==NULL)
    return;

  const struct Tracks *track = block->tracks;
  while(track != NULL){
    if (track->patch==patch && track->notes!=NULL){
      GFX_ScheduleEditorRedraw();
      //printf("Updating\n");
      return;
    }
    track=NextTrack(track);
  }
}

// RT function.
static inline void GFX_ScheduleCurrentInstrumentRedraw(void){
  ATOMIC_SET(atomic_must_redraw_instrument, PATCH_get_current());
}

static inline void GFX_ScheduleInstrumentRedraw(struct Patch *patch){
  if (patch==PATCH_get_current())
    ATOMIC_SET(atomic_must_redraw_instrument, patch);
}

static inline void GFX_ScheduleCalculateCoordinates(void){
  ATOMIC_SET(atomic_must_calculate_coordinates, true);
  GFX_ScheduleRedraw();
}


extern LANGSPEC void GFX_EditorWindowToFront(struct Tracker_Windows *tvisual);
extern LANGSPEC bool GFX_PlaylistWindowIsVisible(void);
extern LANGSPEC void GFX_PlayListWindowToFront(void);
extern LANGSPEC void GFX_PlayListWindowToBack(void);
extern LANGSPEC void GFX_InstrumentWindowToFront(void);
extern LANGSPEC void GFX_InstrumentWindowToBack(void);

extern LANGSPEC void GFX_toggleFullScreen(struct Tracker_Windows *tvisual);
extern LANGSPEC void GFX_toggleCurrWindowFullScreen(void);
extern LANGSPEC void GFX_showHideInstrumentWidget(struct Tracker_Windows *window);
extern LANGSPEC void GFX_showHidePlaylist(struct Tracker_Windows *window);
extern LANGSPEC void GFX_showHideMixerStrip(struct Tracker_Windows *window);
extern LANGSPEC void GFX_showHideEditor(void);
extern LANGSPEC void GFX_showHideMixerWidget(void);

extern LANGSPEC void GFX_SetMinimalInstrumentWindow(void);

extern LANGSPEC bool GFX_EditorIsVisible(void);
extern LANGSPEC void GFX_ShowEditor(void);
extern LANGSPEC void GFX_HideEditor(void);

extern LANGSPEC bool GFX_SequencerIsVisible(void);
extern LANGSPEC void GFX_ShowSequencer(void);
extern LANGSPEC void GFX_HideSequencer(void);

extern LANGSPEC bool GFX_MixerIsVisible(void);
extern LANGSPEC void GFX_ShowMixer(void);
extern LANGSPEC void GFX_HideMixer(void);

extern LANGSPEC bool GFX_InstrumentWindowIsVisible(void);

//extern LANGSPEC void GFX_ConfigColors(struct Tracker_Windows *tvisual);
#if USE_QT4
#ifdef QFONT_H
extern void GFX_SetSystemFont(QFont font);
#endif
#endif
extern LANGSPEC void GFX_ConfigFonts(struct Tracker_Windows *tvisual);

extern LANGSPEC void GFX_SetDefaultColors1(struct Tracker_Windows *tvisual);
extern LANGSPEC void GFX_SetDefaultColors2(struct Tracker_Windows *tvisual);

extern LANGSPEC unsigned int GFX_mix_colors(unsigned int c1, unsigned int c2, float how_much);
extern LANGSPEC unsigned int GFX_get_color(enum ColorNums colornum);
extern LANGSPEC unsigned int GFX_get_color_from_colorname(const char *colorname);
extern LANGSPEC const char *GFX_get_colorname_from_color(unsigned int colornum);
extern LANGSPEC void GFX_color_dialog(const char *initial_color, int64_t parentguinum, func_t *callback);

extern LANGSPEC void GFX_SetCustomColor(struct Tracker_Windows *tvisual, void *color);
extern LANGSPEC unsigned int GFX_MakeRandomColor(void);
extern LANGSPEC unsigned int GFX_MakeRandomBlockColor(void);
extern LANGSPEC int GFX_MakeRandomCustomColor(int colornum);

//bool GFX_SelectEditFont(struct Tracker_Windows *tvisual){

extern LANGSPEC int GFX_get_text_width(struct Tracker_Windows *tvisual, const char *text);
extern LANGSPEC int GFX_get_num_characters(struct Tracker_Windows *tvisual, const char *text, int max_width);


#endif

