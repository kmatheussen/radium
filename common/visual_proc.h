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



extern LANGSPEC void GFX_C2V_bitBlt(
				    struct Tracker_Windows *window,
				    int from_x1,int from_x2,
				    int to_y
				    );


/* window,x1,x2,x3,x4,height, y pixmap */
extern LANGSPEC void GFX_C_DrawCursor(
				      struct Tracker_Windows *window,
				      int x1,int x2,int x3,int x4,int height,
				      int y_pixmap
				      );

extern LANGSPEC void GFX_P2V_bitBlt(
				struct Tracker_Windows *window,
				int from_x,int from_y,
				int to_x,int to_y,
				int width,int height
			);

extern LANGSPEC void GFX_P_FilledBox(struct Tracker_Windows *tvisual,int color,int x,int y,int x2,int y2);

extern LANGSPEC int GFX_CreateVisual(struct Tracker_Windows *tvisual);
extern LANGSPEC int GFX_ShutDownVisual(struct Tracker_Windows *tvisual);

extern LANGSPEC void GFX_EditorWindowToFront(struct Tracker_Windows *tvisual);
extern LANGSPEC void GFX_PlayListWindowToFront(void);
extern LANGSPEC void GFX_InstrumentWindowToFront(void);
extern LANGSPEC void GFX_HelpWindowToFront(void);

extern LANGSPEC void GFX_MaximizeEditorWindow(struct Tracker_Windows *tvisual);
extern LANGSPEC void GFX_MinimizeEditorWindow(struct Tracker_Windows *tvisual);

extern LANGSPEC void GFX_ConfigColors(struct Tracker_Windows *tvisual);

//bool GFX_SelectEditFont(struct Tracker_Windows *tvisual){

extern LANGSPEC void GFX_P_FilledBox(struct Tracker_Windows *tvisual,int color,int x,int y,int x2,int y2);

extern LANGSPEC void GFX_P_Box(struct Tracker_Windows *tvisual,int color,int x,int y,int x2,int y2);


extern LANGSPEC void GFX_P_Line(struct Tracker_Windows *tvisual,int color,int x,int y,int x2,int y2);
extern LANGSPEC void GFX_P_Point(struct Tracker_Windows *tvisual,int color,int x,int y);

extern LANGSPEC void GFX_P_Text(
	struct Tracker_Windows *tvisual,
	int color,
	char *text,
	int x,
	int y,
	bool clear
	);

extern LANGSPEC void GFX_Line(struct Tracker_Windows *tvisual,int color,int x,int y,int x2,int y2);
extern LANGSPEC void GFX_All_Line(struct Tracker_Windows *tvisual,int color,int x,int y,int x2,int y2);
extern LANGSPEC void GFX_Box(struct Tracker_Windows *tvisual,int color,int x,int y,int x2,int y2);
extern LANGSPEC void GFX_FilledBox(struct Tracker_Windows *tvisual,int color,int x,int y,int x2,int y2);
extern LANGSPEC void GFX_Slider_FilledBox(struct Tracker_Windows *tvisual,int color,int x,int y,int x2,int y2);
extern LANGSPEC void GFX_All_FilledBox(struct Tracker_Windows *tvisual,int color,int x,int y,int x2,int y2);

extern LANGSPEC void GFX_Text(
	struct Tracker_Windows *tvisual,
	int color,
	char *text,
	int x,
	int y,
	bool clear
);

extern LANGSPEC void GFX_P_InvertText(
	struct Tracker_Windows *tvisual,
	int color,
	char *text,
	int x,
	int y,
	bool clear
);


extern LANGSPEC void GFX_P_InvertTextNoText(
	struct Tracker_Windows *tvisual,
	int color,
	int len,
	int x,
	int y,
	bool clear
);

extern LANGSPEC void GFX_InitDrawCurrentLine(
	struct Tracker_Windows *tvisual,
	int x, int y, int x2, int y2
);
extern LANGSPEC void GFX_InitDrawCurrentLine2(
	struct Tracker_Windows *tvisual,
	int x, int y, int x2, int y2
);

extern LANGSPEC void GFX_DrawCursorPos(
	struct Tracker_Windows *tvisual,
	int fx, int fy, int fx2, int fy2,
	int x, int y, int x2, int y2
);

extern LANGSPEC void GFX_P_DrawTrackBorderSingle(
	struct Tracker_Windows *tvisual,
	int x, int y, int y2
);

extern LANGSPEC void GFX_P_DrawTrackBorderDouble(
	struct Tracker_Windows *tvisual,
	int x, int y, int y2
);

extern LANGSPEC void GFX_V_DrawTrackBorderSingle(
	struct Tracker_Windows *tvisual,
	int x, int y, int y2
);

extern LANGSPEC void GFX_V_DrawTrackBorderDouble(
	struct Tracker_Windows *tvisual,
	int x, int y, int y2
);

extern LANGSPEC void GFX_Scroll(
	struct Tracker_Windows *tvisual,
	int dx,int dy,
	int x,int y,
	int x2,int y2
);

extern LANGSPEC void GFX_P_Scroll(
	struct Tracker_Windows *tvisual,
	int dx,int dy,
	int x,int y,
	int x2,int y2
	);

extern LANGSPEC void GFX_ScrollDown(
	struct Tracker_Windows *tvisual,
	int dx,int dy,
	int x,int y,
	int x2,int y2
);

extern LANGSPEC void GFX_P_ClearWindow(struct Tracker_Windows *tvisual);

extern LANGSPEC void GFX_ClearWindow(struct Tracker_Windows *tvisual);

extern LANGSPEC int GFX_ResizeWindow(struct Tracker_Windows *tvisual,int x,int y);



void GFXS_LineType(
	     void (*GFX_OSFunc)(
				struct Tracker_Windows *window,
				int color,
				int x,int y,int x2,int y2
				),
	     struct Tracker_Windows *window,
	     int color,
	     int x,int y,int x2,int y2
	     );
void GFXS_BoxType(
	     void (*GFX_OSFunc)(
				struct Tracker_Windows *window,
				int color,
				int x,int y,int x2,int y2
				),
	     struct Tracker_Windows *window,
	     int color,
	     int x,int y,int x2,int y2
	     );

void GFXS_TextType(
	     void (*GFX_OSFunc)(
				struct Tracker_Windows *window,
				int color,char *text,
				int x,int y,
				bool clear
				),
	     struct Tracker_Windows *window,
	     int color,char *text,
	     int x,int y,
	     bool clear
	     );
void GFXS_TextType2(
	     void (*GFX_OSFunc)(
				struct Tracker_Windows *window,
				int color,int len,
				int x,int y,
				bool clear
				),
	     struct Tracker_Windows *window,
	     int color,int len,
	     int x,int y,
	     bool clear
	     );

void GFXS_BorderType(
		     void (*GFX_P_OSFunc)(
							 struct Tracker_Windows *tvisual,
							 int x, int y, int y2
							 ),
		     struct Tracker_Windows *tvisual,
		     int x, int y, int y2
		     );

void GFXS_BorderType2(
		     void (*GFX_P_OSFunc)(
							 struct Tracker_Windows *tvisual,
							 int x, int y, int y2
							 ),
		     struct Tracker_Windows *tvisual,
		     int x, int y, int y2
		     );

void GFXS_ScrollType(
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
				int x,int y,int x2,int y2
				),
	     struct Tracker_Windows *window,
	     int color,
	     int x,int y,int x2,int y2
	     );

void GFXST_BoxType(
	     void (*GFX_OSFunc)(
				struct Tracker_Windows *window,
				int color,
				int x,int y,int x2,int y2
				),
	     struct Tracker_Windows *window,
	     int color,
	     int x,int y,int x2,int y2
	     );
void GFXST_TextType(
	     void (*GFX_OSFunc)(
				struct Tracker_Windows *window,
				int color,char *text,
				int x,int y,
				bool clear
				),
	     struct Tracker_Windows *window,
	     int color,char *text,
	     int x,int y,
	     bool clear
	     );
void GFXST_TextType2(
	     void (*GFX_OSFunc)(
				struct Tracker_Windows *window,
				int color,int len,
				int x,int y,
				bool clear
				),
	     struct Tracker_Windows *window,
	     int color,int len,
	     int x,int y,
	     bool clear
	     );
void GFXST_BorderType(
		     void (*GFX_P_OSFunc)(
							 struct Tracker_Windows *window,
							 int x, int y, int y2
							 ),
		     struct Tracker_Windows *window,
		     int x, int y, int y2
		     );
void GFXST_BorderType2(
		     void (*GFX_P_OSFunc)(
							 struct Tracker_Windows *window,
							 int x, int y, int y2
							 ),
		     struct Tracker_Windows *window,
		     int x, int y, int y2
		     );


#ifndef GFX_DONTSHRINK

#define GFX_P_FilledBox(a,b,c,d,e,f) GFXS_BoxType(GFX_P_FilledBox,a,b,c,d,e,f)
#define GFX_P_Box(a,b,c,d,e,f) GFXS_BoxType(GFX_P_Box,a,b,c,d,e,f)
#define GFX_P_Line(a,b,c,d,e,f) GFXS_LineType(GFX_P_Line,a,b,c,d,e,f)
#define GFX_Line(a,b,c,d,e,f) GFXS_LineType(GFX_Line,a,b,c,d,e,f)
#define GFX_All_Line(a,b,c,d,e,f) GFXS_LineType(GFX_All_Line,a,b,c,d,e,f)
#define GFX_Box(a,b,c,d,e,f) GFXS_BoxType(GFX_Box,a,b,c,d,e,f)
#define GFX_FilledBox(a,b,c,d,e,f) GFXS_BoxType(GFX_FilledBox,a,b,c,d,e,f)
#define GFX_All_FilledBox(a,b,c,d,e,f) GFXS_BoxType(GFX_All_FilledBox,a,b,c,d,e,f)

#define GFX_P_Text(a,b,c,d,e,f) GFXS_TextType(GFX_P_Text,a,b,c,d,e,f)
#define GFX_Text(a,b,c,d,e,f) GFXS_TextType(GFX_Text,a,b,c,d,e,f)
#define GFX_P_InvertText(a,b,c,d,e,f) GFXS_TextType(GFX_P_InvertText,a,b,c,d,e,f)
#define GFX_P_InvertTextNoText(a,b,c,d,e,f) GFXS_TextType2(GFX_P_InvertTextNoText,a,b,c,d,e,f)

#define GFX_P_DrawTrackBorderSingle(a,b,c,d) GFXS_BorderType(GFX_P_DrawTrackBorderSingle,a,b,c,d)
#define GFX_P_DrawTrackBorderDouble(a,b,c,d) GFXS_BorderType2(GFX_P_DrawTrackBorderDouble,a,b,c,d)

#define GFX_Scroll(a,b,c,d,e,f,g) GFXS_ScrollType(GFX_Scroll,a,b,c,d,e,f,g)
#define GFX_P_Scroll(a,b,c,d,e,f,g) GFXS_ScrollType(GFX_P_Scroll,a,b,c,d,e,f,g)

#define GFX_P_T_FilledBox(a,b,c,d,e,f) GFXST_BoxType(GFX_P_FilledBox,a,b,c,d,e,f)
#define GFX_P_T_Box(a,b,c,d,e,f) GFXST_BoxType(GFX_P_Box,a,b,c,d,e,f)
#define GFX_P_T_Line(a,b,c,d,e,f) GFXST_LineType(GFX_P_Line,a,b,c,d,e,f)
#define GFX_T_Line(a,b,c,d,e,f) GFXST_LineType(GFX_Line,a,b,c,d,e,f)
#define GFX_T_All_Line(a,b,c,d,e,f) GFXST_LineType(GFX_All_Line,a,b,c,d,e,f)
#define GFX_T_Box(a,b,c,d,e,f) GFXST_BoxType(GFX_Box,a,b,c,d,e,f)
#define GFX_T_FilledBox(a,b,c,d,e,f) GFXST_BoxType(GFX_FilledBox,a,b,c,d,e,f)
#define GFX_T_All_FilledBox(a,b,c,d,e,f) GFXST_BoxType(GFX_All_FilledBox,a,b,c,d,e,f)

#define GFX_P_T_Text(a,b,c,d,e,f) GFXST_TextType(GFX_P_Text,a,b,c,d,e,f)
#define GFX_T_Text(a,b,c,d,e,f) GFXST_TextType(GFX_Text,a,b,c,d,e,f)
#define GFX_P_T_InvertText(a,b,c,d,e,f) GFXST_TextType(GFX_P_InvertText,a,b,c,d,e,f)
#define GFX_P_T_InvertTextNoText(a,b,c,d,e,f) GFXST_TextType2(GFX_P_InvertTextNoText,a,b,c,d,e,f)

#define GFX_P_T_DrawTrackBorderSingle(a,b,c,d) GFXST_BorderType(GFX_P_DrawTrackBorderSingle,a,b,c,d)
#define GFX_P_T_DrawTrackBorderDouble(a,b,c,d) GFXST_BorderType2(GFX_P_DrawTrackBorderDouble,a,b,c,d)


#endif
#endif

