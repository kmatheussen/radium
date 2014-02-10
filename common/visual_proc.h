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

extern LANGSPEC int GFX_Message(vector_t *buttons,const char *fmt,...);

extern LANGSPEC void GFX_AddMenuItem(struct Tracker_Windows *tvisual, const char *name, const char *python_command);
extern LANGSPEC void GFX_AddCheckableMenuItem(struct Tracker_Windows *tvisual, const char *name, const char *python_command, int checkval);
extern LANGSPEC void GFX_AddMenuSeparator(struct Tracker_Windows *tvisual);
extern LANGSPEC void GFX_AddMenuMenu(struct Tracker_Windows *tvisual, const char *name, const char *command);
extern LANGSPEC void GFX_GoPreviousMenuLevel(struct Tracker_Windows *tvisual);


extern LANGSPEC void QUEUE_GFX_C2V_bitBlt(
				    struct Tracker_Windows *window,
				    int from_x1,int from_x2,
				    int to_y
				    );


/* window,x1,x2,x3,x4,height, y pixmap */
extern LANGSPEC void QUEUE_GFX_C_DrawCursor(
				      struct Tracker_Windows *window,
				      int x1,int x2,int x3,int x4,int height,
				      int y_pixmap
				      );

extern LANGSPEC void QUEUE_GFX_P2V_bitBlt(
				struct Tracker_Windows *window,
				int from_x,int from_y,
				int to_x,int to_y,
				int width,int height
			);

extern LANGSPEC void QUEUE_GFX_P_FilledBox(struct Tracker_Windows *tvisual,int color,int x,int y,int x2,int y2);

extern LANGSPEC void GFX_disable_mouse_keyboard(void);
extern LANGSPEC void GFX_enable_mouse_keyboard(void);

extern LANGSPEC int GFX_CreateVisual(struct Tracker_Windows *tvisual);
extern LANGSPEC int GFX_ShutDownVisual(struct Tracker_Windows *tvisual);
extern LANGSPEC void GFX_SetMinimumWindowWidth(struct Tracker_Windows *tvisual, int width);

extern LANGSPEC void GFX_EditorWindowToFront(struct Tracker_Windows *tvisual);
extern LANGSPEC void GFX_PlayListWindowToFront(void);
extern LANGSPEC void GFX_PlayListWindowToBack(void);
extern LANGSPEC void GFX_InstrumentWindowToFront(void);
extern LANGSPEC void GFX_InstrumentWindowToBack(void);
extern LANGSPEC void GFX_HelpWindowToFront(void);

extern LANGSPEC void GFX_toggleFullScreen(struct Tracker_Windows *tvisual);
extern LANGSPEC void GFX_showHideInstrumentWidget(struct Tracker_Windows *window);
extern LANGSPEC void GFX_showHidePlaylist(struct Tracker_Windows *window);
extern LANGSPEC void GFX_showHideEditor(void);
extern LANGSPEC void GFX_showHideMixerWidget(void);

extern LANGSPEC void GFX_SetMinimalInstrumentWindow(void);

extern LANGSPEC bool GFX_EditorIsVisible(void);
extern LANGSPEC void GFX_ShowEditor(void);
extern LANGSPEC void GFX_HideEditor(void);

extern LANGSPEC bool GFX_MixerIsVisible(void);
extern LANGSPEC void GFX_ShowMixer(void);
extern LANGSPEC void GFX_HideMixer(void);

extern LANGSPEC bool GFX_InstrumentWindowIsVisible(void);

extern LANGSPEC void GFX_MaximizeEditorWindow(struct Tracker_Windows *tvisual);
extern LANGSPEC void GFX_MinimizeEditorWindow(struct Tracker_Windows *tvisual);

extern LANGSPEC void GFX_ConfigColors(struct Tracker_Windows *tvisual);
extern LANGSPEC void GFX_ConfigFonts(struct Tracker_Windows *tvisual);

extern LANGSPEC void GFX_SetDefaultColors(struct Tracker_Windows *tvisual);

extern LANGSPEC void GFX_SetCustomColor(struct Tracker_Windows *tvisual, void *color);
extern LANGSPEC int GFX_MakeRandomCustomColor(struct Tracker_Windows *tvisual, int colornum);

//bool GFX_SelectEditFont(struct Tracker_Windows *tvisual){

extern LANGSPEC void QUEUE_GFX_FilledBox(struct Tracker_Windows *tvisual,int color,int x,int y,int x2,int y2, int where);

extern LANGSPEC void QUEUE_GFX_Box(struct Tracker_Windows *tvisual,int color,int x,int y,int x2,int y2, int where);

extern LANGSPEC void QUEUE_GFX_SetClipRect(
                                           struct Tracker_Windows *tvisual,
                                           int x,int y,
                                           int x2,int y2,
                                           int where
                                           );
extern LANGSPEC void QUEUE_GFX_CancelClipRect(struct Tracker_Windows *tvisual, int where);

extern LANGSPEC void PREOS_GFX_Line(struct Tracker_Windows *window,int color,int x,int y,int x2,int y2,int where);
extern LANGSPEC void QUEUE_GFX_Line(struct Tracker_Windows *tvisual,int color,int x,int y,int x2,int y2,int where);

extern LANGSPEC void GFX_Point(struct Tracker_Windows *tvisual,int color,int brightness,int x,int y,int where); // brigtness is between 0 and MAX_BRIGHTNESS. Used by aa lines.
extern LANGSPEC void QUEUE_GFX_Point(struct Tracker_Windows* tvisual,int color,int brightness,int x,int y,int where);
extern LANGSPEC void QUEUE_GFX_Points(struct Tracker_Windows* tvisual,int color,int brightness,int num_points, uint16_t *x,uint16_t *y,int where);
extern LANGSPEC void OS_GFX_Point(
                                  struct Tracker_Windows *tvisual,
                                  int color,
                                  int brightness,
                                  int x,int y,
                                  int where
                                  );
extern LANGSPEC void OS_GFX_Points(
                                   struct Tracker_Windows *tvisual,
                                   int color,
                                   int brightness,
                                   int num_points,
                                   uint16_t *x,uint16_t *y,
                                   int where
                                   );

extern LANGSPEC void OS_GFX_SetMixColor(struct Tracker_Windows *tvisual,int color1,int color2,int mix_factor);
extern LANGSPEC void QUEUE_GFX_SetMixColor(struct Tracker_Windows *tvisual,int color1,int color2,int mix_factor);

extern LANGSPEC void OS_GFX_Polygon(
                                    struct Tracker_Windows *tvisual,
                                    int color,
                                    int x1, int y1, int x2, int y2,
                                    int num_points,
                                    APoint *peaks,
                                    int where
                                    );

extern LANGSPEC void OS_GFX_Polyline(
                                     struct Tracker_Windows *tvisual,
                                     int color,
                                     int x1, int y1, int x2, int y2,
                                     int num_points,
                                     APoint *peaks,
                                     int where
                                     );

extern LANGSPEC void QUEUE_GFX_Polygon(
                                    struct Tracker_Windows *tvisual,
                                    int color,
                                    int x1, int y1, int x2, int y2,
                                    int num_points,
                                    APoint *peaks,
                                    int where
                                    );

extern LANGSPEC void QUEUE_GFX_Polyline(
                                     struct Tracker_Windows *tvisual,
                                     int color,
                                     int x1, int y1, int x2, int y2,
                                     int num_points,
                                     APoint *peaks,
                                     int where
                                     );

extern LANGSPEC int GFX_get_text_width(struct Tracker_Windows *tvisual, const char *text);
extern LANGSPEC int GFX_get_num_characters(struct Tracker_Windows *tvisual, const char *text, int max_width);

extern LANGSPEC void PREOS_GFX_Text(
	struct Tracker_Windows *tvisual,
	int color,
	const char *text,
	int x,
	int y,
        int width,
        int flags,
        int where
	);
extern LANGSPEC void QUEUE_GFX_Text(
	struct Tracker_Windows *tvisual,
	int color,
	const char *text,
	int x,
	int y,
        int width,
        int flags,
        int where
	);

extern LANGSPEC void GFX_DrawTrackBorderSingle(
	struct Tracker_Windows *tvisual,
	int x, int y, int y2,
        int where
);

extern LANGSPEC void GFX_DrawTrackBorderDouble(
	struct Tracker_Windows *tvisual,
	int x, int y, int y2,
        int where
);

extern LANGSPEC void QUEUE_GFX_BitBlt(
	struct Tracker_Windows *tvisual,
	int dx,int dy,
	int x,int y,
	int x2,int y2
	);

extern LANGSPEC int GFX_ResizeWindow(struct Tracker_Windows *tvisual,int x,int y);



void GFXS_LineType(
	     void (*GFX_OSFunc)(
				struct Tracker_Windows *window,
				int color,
				int x,int y,int x2,int y2,
                                int where
				),
	     struct Tracker_Windows *window,
	     int color,
	     int x,int y,int x2,int y2,
             int where
	     );
void GFXS_BoxType(
	     void (*GFX_OSFunc)(
				struct Tracker_Windows *window,
				int color,
				int x,int y,int x2,int y2,
                                int where
				),
	     struct Tracker_Windows *window,
	     int color,
	     int x,int y,int x2,int y2,
             int where
	     );

void GFXS_TextType(
	     void (*GFX_OSFunc)(
				struct Tracker_Windows *window,
				int color,const char *text,
				int x,int y,
                                int width,
                                int flags,
                                int where
				),
	     struct Tracker_Windows *window,
	     int color,const char *text,
	     int x,int y,
             int width,
	     int flags,
             int where
	     );

void GFXS_BorderType(
		     void (*GFX_P_OSFunc)(
                                          struct Tracker_Windows *tvisual,
                                          int x, int y, int y2,
                                          int where
                                          ),
		     struct Tracker_Windows *tvisual,
		     int x, int y, int y2,
                     int where
		     );

void GFXS_BorderType2(
		     void (*GFX_P_OSFunc)(
                                          struct Tracker_Windows *tvisual,
                                          int x, int y, int y2,
                                          int where
                                          ),
		     struct Tracker_Windows *tvisual,
		     int x, int y, int y2,
                     int where
		     );

void GFXS_BitBltType(
		     void (*GFX_P_OSFunc)(
					  struct Tracker_Windows *tvisual,
					  int dx,int dy,
					  int x,int y,
					  int x2,int y2
					  ),
		     struct Tracker_Windows *tvisual,
		     int dx,int dy,
		     int x,int y,
		     int x2,int y2
		     );

void GFXST_LineType(
	     void (*GFX_OSFunc)(
				struct Tracker_Windows *window,
				int color,
				int x,int y,int x2,int y2,
                                int where
				),
	     struct Tracker_Windows *window,
	     int color,
	     int x,int y,int x2,int y2,
             int where
	     );

void GFXST_BoxType(
	     void (*GFX_OSFunc)(
				struct Tracker_Windows *window,
				int color,
				int x,int y,int x2,int y2,
                                int where
				),
	     struct Tracker_Windows *window,
	     int color,
	     int x,int y,int x2,int y2,
             int where
	     );
void GFXST_TextType(
	     void (*GFX_OSFunc)(
				struct Tracker_Windows *window,
				int color,const char *text,
				int x,int y,
                                int width,
                                int flags,
                                int where
				),
	     struct Tracker_Windows *window,
	     int color,const char *text,
	     int x,int y,
             int width,
             int flags,
             int where
	     );
void GFXST_BorderType(
		     void (*GFX_P_OSFunc)(
							 struct Tracker_Windows *window,
							 int x, int y, int y2,
                                                         int where
							 ),
		     struct Tracker_Windows *window,
		     int x, int y, int y2,
                     int where
		     );
void GFXST_BorderType2(
		     void (*GFX_P_OSFunc)(
							 struct Tracker_Windows *window,
							 int x, int y, int y2,
                                                         int where
							 ),
		     struct Tracker_Windows *window,
		     int x, int y, int y2,
                     int where
		     );

#ifndef GFX_DONTSHRINK

#define GFX_FilledBox(a,b,c,d,e,f,g) GFXS_BoxType(QUEUE_GFX_FilledBox,a,b,c,d,e,f,g)
#define GFX_Box(a,b,c,d,e,f,g) GFXS_BoxType(QUEUE_GFX_Box,a,b,c,d,e,f,g)
#define GFX_Line(a,b,c,d,e,f,g) GFXS_LineType(QUEUE_GFX_Line,a,b,c,d,e,f,g)

#define GFX_Text(a,b,c,d,e,f,g,h) GFXS_TextType(QUEUE_GFX_Text,a,b,c,d,e,f,g,h)

#define GFX_DrawTrackBorderSingle(a,b,c,d,e) GFXS_BorderType(GFX_DrawTrackBorderSingle,a,b,c,d,e)
#define GFX_DrawTrackBorderDouble(a,b,c,d,e) GFXS_BorderType2(GFX_DrawTrackBorderDouble,a,b,c,d,e)

#define GFX_BitBlt(a,b,c,d,e,f,g) GFXS_BitBltType(QUEUE_GFX_BitBlt,a,b,c,d,e,f,g)

#define GFX_T_FilledBox(a,b,c,d,e,f,g) GFXST_BoxType(QUEUE_GFX_FilledBox,a,b,c,d,e,f,g)
#define GFX_T_Box(a,b,c,d,e,f,g) GFXST_BoxType(QUEUE_GFX_Box,a,b,c,d,e,f,g)
#define GFX_T_Line(a,b,c,d,e,f,g) GFXST_LineType(QUEUE_GFX_Line,a,b,c,d,e,f,g)

#define GFX_T_Text(a,b,c,d,e,f,g,h) GFXST_TextType(QUEUE_GFX_Text,a,b,c,d,e,f,g,h)

#define GFX_T_DrawTrackBorderSingle(a,b,c,d,e) GFXST_BorderType(GFX_DrawTrackBorderSingle,a,b,c,d,e)
#define GFX_T_DrawTrackBorderDouble(a,b,c,d,e) GFXST_BorderType2(GFX_DrawTrackBorderDouble,a,b,c,d,e)

#define GFX_SetMixColor QUEUE_GFX_SetMixColor

#define GFX_Polygon QUEUE_GFX_Polygon
#define GFX_Polyline QUEUE_GFX_Polyline

#define GFX_SetClipRect QUEUE_GFX_SetClipRect
#define GFX_CancelClipRect QUEUE_GFX_CancelClipRect

#endif

#endif

