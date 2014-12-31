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

#ifndef X11_KEYBOARD_PROC
#define X11_KEYBOARD_PROC

#include <X11/Xlib.h>

extern LANGSPEC int X11_get_keynum(void *focus_widget, XKeyEvent *key_event);
extern LANGSPEC void X11_init_keyboard(void);

/*
extern LANGSPEC int X11Event_KeyPress(int keynum,int keystate,struct Tracker_Windows *window);
extern LANGSPEC int X11_KeyPress(XKeyEvent *event,struct Tracker_Windows *window);

extern LANGSPEC int X11Event_KeyRelease(int keynum,int keystate,struct Tracker_Windows *window);
extern LANGSPEC int X11_KeyRelease(XKeyEvent *event,struct Tracker_Windows *window);
*/

extern LANGSPEC void X11_ResetKeysUpDowns(void);

extern LANGSPEC void X11_XEventPreHandler(void *focused_widget, XEvent *event);
extern LANGSPEC bool X11_KeyboardFilter(void *focus_widget, XEvent *event);

#endif
