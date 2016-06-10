/* Copyright 2012 Kjetil S. Matheussen

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


#if USE_GTK_VISUAL

#ifdef __linux__
typedef int64_t socket_type_t;
#endif

#ifdef FOR_WINDOWS
typedef void* socket_type_t;
#endif

#ifdef FOR_MACOSX
typedef void* socket_type_t;
#endif



extern LANGSPEC void setFontValues(struct Tracker_Windows *window);

extern LANGSPEC void GTK_SetColor(int colornum, int red, int green, int blue);

extern LANGSPEC socket_type_t GTK_CreateVisual(socket_type_t socket_id);

extern LANGSPEC void GTK_SetPlugSize(int width, int height);
extern LANGSPEC void GTK_SetSize(int width, int height);
extern LANGSPEC void GTK_SetFocus(void);

extern LANGSPEC bool GTK_HasPendingEvents(void);
extern LANGSPEC void GTK_HandleEvents(void);

extern LANGSPEC void GTK_MainQuit(void);
extern LANGSPEC void GTK_MainLoop(void);
#endif

extern LANGSPEC void GTK_Init(int argc, char **argv);

