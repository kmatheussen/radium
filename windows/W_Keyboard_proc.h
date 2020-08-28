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

extern LANGSPEC bool W_windows_key_down(void);
extern LANGSPEC void W_KeyboardHandlerShutDown(void);
extern LANGSPEC void W_RegisterRawInputHandler(void *hwnd); // hwnd is g_main_window->effectiveWinId(). Call this function during initialization after g_main_window has started.
extern LANGSPEC bool W_HasDeltaMouse(void);
extern LANGSPEC bool W_CanMovePointer(void);

