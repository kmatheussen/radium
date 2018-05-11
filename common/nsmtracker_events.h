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


#ifndef TRACKER_EVENTS_DEFINE
#define TRACKER_EVENTS_DEFINE 1

#include "keyboard_sub_ids.h"

/* Event IDs: */

#define API_MOUSE_PRESSING 1
#define API_MOUSE_MOVING 2
#define API_MOUSE_RELEASING 3
#define API_MOUSE_LEAVING 4

#define TR_MOUSEMOVE 0
#define TR_LEFTMOUSEDOWN 1
#define TR_LEFTMOUSEUP 2
#define TR_MIDDLEMOUSEDOWN 3
#define TR_MIDDLEMOUSEUP 4
#define TR_RIGHTMOUSEDOWN 5
#define TR_RIGHTMOUSEUP 6

#define TR_KEYBOARD 7

#define TR_WINDOWMOVE 8
#define TR_WINDOWRESIZE 9
#define TR_WINDOWVISIBLE 10
#define TR_WINDOWNOTVISIBLE 11
#define TR_WINDOWCLOSE 12

#define TR_KEYBOARDUP 13
#define TR_AUTOREPEAT 14



/* KEYSWITCHes: */

#define EVENT_LEFTCTRL (1<<EVENT_CTRL_L)
#define EVENT_RIGHTCTRL (1<<EVENT_CTRL_R)
#define EVENT_CAPSLOCK (1<<EVENT_CAPS)
#define EVENT_LEFTSHIFT (1<<EVENT_SHIFT_L)
#define EVENT_RIGHTSHIFT (1<<EVENT_SHIFT_R)
#define EVENT_LEFTALT (1<<EVENT_ALT_L)
#define EVENT_RIGHTALT (1<<EVENT_ALT_R)

#define EVENT_LEFTEXTRA1 (1<<EVENT_EXTRA_L) /*Amiga: Amiga, PC: "Meta"/Windows key */
#define EVENT_RIGHTEXTRA1 (1<<EVENT_EXTRA_R) /*Amiga: Amiga, PC: "Meta"/Windows key */
#define EVENT_UP2 (1<<EVENT_UP) /* Up key, i.e. key is released. */

#define EVENT_MOUSE_SEQUENCER2 (1<<EVENT_MOUSE_SEQUENCER)
#define EVENT_MOUSE_MIXER2 (1<<EVENT_MOUSE_MIXER)
#define EVENT_MOUSE_MIXERSTRIPS2 (1<<EVENT_MOUSE_MIXERSTRIPS)
#define EVENT_MOUSE_EDITOR2 (1<<EVENT_MOUSE_EDITOR)

#define NoSwitch(a) (a==0)

#define LeftCtrl(a) (a&EVENT_LEFTCTRL)
#define RightCtrl(a) (a&EVENT_RIGHTCTRL)
#define AnyCtrl(a) (LeftCtrl(a) | RightCtrl(a))

#define CapsLock(a) (a&EVENT_CAPSLOCK)

#define LeftShift(a) (a&EVENT_LEFTSHIFT)
#define RightShift(a) (a&EVENT_RIGHTSHIFT)
#define AnyShift(a) (LeftShift(a) | RightShift(a))

#define LeftAlt(a) (a&EVENT_LEFTALT)
#define RightAlt(a) (a&EVENT_RIGHTALT)
#define AnyAlt(a) (LeftAlt(a) | RightAlt(a))

#define LeftExtra(a) (a&EVENT_LEFTEXTRA1)
#define RightExtra(a) (a&EVENT_RIGHTEXTRA1)
#define AnyExtra(a) (LeftExtra(a) | RightExtra(a))

#define OnlyLeftShift(a) (a==EVENT_LEFTSHIFT)
#define OnlyLeftAlt(a) (a==EVENT_LEFTALT)

#define AnyModifierKeyPressed(a) (a!=0 && a!=EVENT_MOUSE_SEQUENCER2 && a!=EVENT_MOUSE_MIXER2 && a!=EVENT_MOUSE_EDITOR2 && a!=EVENT_MOUSE_MIXERSTRIPS2)



struct TEvent{
	int ID;
	int SubID;
	uint32_t keyswitch;
	float x;
	float y;
};

struct TEventFIFO{
	struct TEventFIFO *next;
	struct TEvent t;
};




struct WrapFuncList{
	char *funcname;
	void *func;
};


#endif











