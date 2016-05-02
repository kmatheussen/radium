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

#include "visual_proc.h"


extern LANGSPEC void GFX_ConfigSystemFont(void);
//extern LANGSPEC char *GFX_SelectEditFont(struct Tracker_Windows *tvisual);
extern LANGSPEC void GFX_ResetFontSize(struct Tracker_Windows *tvisual);
extern LANGSPEC void GFX_IncFontSize(struct Tracker_Windows *tvisual, int pixels);

extern LANGSPEC void GFX_SetDefaultFont(struct Tracker_Windows *tvisual);
extern LANGSPEC void GFX_SetDefaultSystemFont(struct Tracker_Windows *tvisual);

extern LANGSPEC void RT_message(const char *fmt,...);

extern LANGSPEC void GFX_SetStatusBar(struct Tracker_Windows *tvisual,const char *title);
extern LANGSPEC void GFX_SetWindowTitle(struct Tracker_Windows *tvisual,const wchar_t *title);

extern LANGSPEC void SetNormalPointer(struct Tracker_Windows *tvisual);
extern LANGSPEC void SetPointingPointer(struct Tracker_Windows *tvisual);
extern LANGSPEC void SetBlankPointer(struct Tracker_Windows *tvisual);
extern LANGSPEC void SetDiagResizePointer(struct Tracker_Windows *tvisual);
extern LANGSPEC void SetHorizResizePointer(struct Tracker_Windows *tvisual);
extern LANGSPEC void SetVerticalResizePointer(struct Tracker_Windows *tvisual);
extern LANGSPEC void MovePointer(struct Tracker_Windows *tvisual, float x, float y);
extern LANGSPEC WPoint GetPointerPos(struct Tracker_Windows *tvisual);

// Note that For the ReqType functions, tvisual might be NULL. The reasons is that it can be called when loading, or when starting up.
extern LANGSPEC ReqType GFX_OpenReq(struct Tracker_Windows *tvisual,int width,int height,const char *title);
extern LANGSPEC void GFX_CloseReq(struct Tracker_Windows *tvisual,ReqType reqtype);

extern LANGSPEC void GFX_WriteString(ReqType reqtype,const char *text);
extern LANGSPEC void GFX_SetString(ReqType das_reqtype,const char *text);
extern LANGSPEC void GFX_ReadString(ReqType reqtype,char *buffer,int bufferlength);
extern LANGSPEC int GFX_ReqTypeMenu(
	struct Tracker_Windows *tvisual,
	ReqType reqtype,
	const char *seltext,
        vector_t *v
	);

extern LANGSPEC int GFX_GetInteger(struct Tracker_Windows *tvisual,ReqType reqtype,const char *text,int min,int max);

extern LANGSPEC float GFX_GetFloat(struct Tracker_Windows *tvisual,ReqType reqtype,const char *text,float min,float max);

extern LANGSPEC char *GFX_GetString(struct Tracker_Windows *tvisual,ReqType reqtype,const char *text);

extern LANGSPEC int GFX_Menu2(
                              struct Tracker_Windows *tvisual,
                              ReqType reqtype,
                              const char *seltext,
                              vector_t *v,
                              func_t *callback
                              );

extern LANGSPEC int GFX_Menu(
	struct Tracker_Windows *tvisual,
	ReqType reqtype,
	const char *seltext,
        vector_t *v
);

extern LANGSPEC vector_t *GFX_MenuParser(const char *texts, const char *separator);

extern LANGSPEC const wchar_t *GFX_GetLoadFileName(
                                                   struct Tracker_Windows *tvisual,
                                                   ReqType reqtype,
                                                   const char *seltext,
                                                   wchar_t *dir,
                                                   const char *postfixes
                                                   );

extern LANGSPEC const wchar_t *GFX_GetSaveFileName(
                                                   struct Tracker_Windows *tvisual,
                                                   ReqType reqtype,
                                                   const char *seltext,
                                                   wchar_t *dir,
                                                   const char *postfixes
                                                   );

extern LANGSPEC void GFX_OS_update_bottombar(void);
extern LANGSPEC void GFX_OS_UpdateKeyOctave(void);
extern LANGSPEC void OS_GFX_NumUndosHaveChanged(int num_undos, bool redos_are_available, bool has_unsaved_undos);
extern LANGSPEC void OS_GFX_SetVolume(int value);
extern LANGSPEC void OS_GFX_IncVolume(int how_much);

extern LANGSPEC void GFX_update_instrument_patch_gui(struct Patch *patch);
extern LANGSPEC void GFX_remove_patch_gui(struct Patch *patch); // Also deletes the audio object itself. (yes, it's messy)

extern LANGSPEC void GFX_OS_set_system_volume_peak_pointers(float *pointers, int num_channels);

extern LANGSPEC void GFX_update_all_instrument_widgets(void);

extern LANGSPEC void GFX_PP_Update(struct Patch *patch);

extern LANGSPEC void OS_VST_config(struct Tracker_Windows *window);

extern LANGSPEC void TOOLS_open(void);

extern LANGSPEC void *PIANOROLLHEADER_create(void);
extern LANGSPEC void PIANOROLLHEADER_assignTrack(void *pianorollheader, int blocknum, int tracknum);
extern LANGSPEC void PIANOROLLHEADER_show(void *pianorollheader, int x, int y, int x2, int y2);
extern LANGSPEC void PIANOROLLHEADER_hide(void *pianorollheader);
