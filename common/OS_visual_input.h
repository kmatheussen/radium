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


//extern LANGSPEC char *GFX_SelectEditFont(struct Tracker_Windows *tvisual);
extern LANGSPEC void GFX_ResetFontSize(struct Tracker_Windows *tvisual);
extern LANGSPEC void GFX_IncFontSize(struct Tracker_Windows *tvisual, int pixels);

extern LANGSPEC void GFX_SetStatusBar(struct Tracker_Windows *tvisual,char *title);
extern LANGSPEC void GFX_SetWindowTitle(struct Tracker_Windows *tvisual,char *title);
extern LANGSPEC void SetNormalPointer(struct Tracker_Windows *tvisual);
extern LANGSPEC void SetResizePointer(struct Tracker_Windows *tvisual);

extern LANGSPEC ReqType GFX_OpenReq(struct Tracker_Windows *tvisual,int width,int height,char *title);
extern LANGSPEC void GFX_CloseReq(struct Tracker_Windows *tvisual,ReqType reqtype);

extern LANGSPEC void GFX_WriteString(ReqType reqtype,char *text);
extern LANGSPEC void GFX_ReadString(ReqType reqtype,char *buffer,int bufferlength);
extern LANGSPEC int GFX_ReqTypeMenu(
	struct Tracker_Windows *tvisual,
	ReqType reqtype,
	char *seltext,
	int num_sel,
	char **menutext
	);

extern LANGSPEC int GFX_GetInteger(struct Tracker_Windows *tvisual,ReqType reqtype,char *text,int min,int max);

extern LANGSPEC float GFX_GetFloat(struct Tracker_Windows *tvisual,ReqType reqtype,char *text,float min,float max);

extern LANGSPEC char *GFX_GetString(struct Tracker_Windows *tvisual,ReqType reqtype,char *text);

extern LANGSPEC int GFX_Menu(
	struct Tracker_Windows *tvisual,
	ReqType reqtype,
	char *seltext,
	int num_sel,
	char **menutext
);

extern LANGSPEC char *GFX_GetLoadFileName(
	struct Tracker_Windows *tvisual,
	ReqType reqtype,
	char *seltext,
	char *dir
);

extern LANGSPEC char *GFX_GetSaveFileName(
	struct Tracker_Windows *tvisual,
	ReqType reqtype,
	char *seltext,
	char *dir
);
