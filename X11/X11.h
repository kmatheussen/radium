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


#include <stdio.h>
#include <X11/Xos.h>
#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <X11/Xproto.h>
#include <X11/keysym.h>
#include <X11/XKBlib.h>

#include <sys/ipc.h>
#include <sys/shm.h>
#include <X11/extensions/XShm.h>


#include <ctype.h>
#include <stdio.h>
#include <string.h>


extern Display *x11_display;
extern int x11_screen;


/* Some (atm. all) of these values are hardcoded from python. So dont change them. */
#define X11EVENT_PLAYLISTINSERT 0
#define X11EVENT_PLAYLISTDELETE 1
#define X11EVENT_KEYBOARDDOWN 2
#define X11EVENT_KEYBOARDUP 3
#define X11EVENT_MIDIEVENT 4

#define X11EVENT_UPDATESONGPOS 5
