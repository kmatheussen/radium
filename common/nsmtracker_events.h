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


struct TEvent{
	int ID;
	int SubID;
	uint32_t keyswitch;
	int x;
	int y;
};

struct TEventFIFO{
	struct TEventFIFO *next;
	struct TEvent t;
};

/* Event IDs: */

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


/* Keyboard Sub IDs: */

#define EVENT_NO 0

#define EVENT_LEFT0 0 // Dont use this one.
#define EVENT_LEFT1 1
#define EVENT_LEFT2 2
#define EVENT_LEFT3 3
#define EVENT_LEFT4 4
#define EVENT_LEFT5 5
#define EVENT_LEFT6 6
#define EVENT_LEFT7 7
#define EVENT_LEFT8 8
#define EVENT_LEFT9 9
#define EVENT_LEFT10 10
#define EVENT_LEFT11 11
#define EVENT_LEFT12 12
#define EVENT_LEFT13 13
#define EVENT_LEFT14 14
#define EVENT_LEFT15 15
#define EVENT_LEFT16 16
#define EVENT_LEFT17 17
#define EVENT_LEFT18 18
#define EVENT_LEFT19 19
#define EVENT_LEFT20 20

#define EVENT_ESC 21

#define EVENT_F1 22
#define EVENT_F2 23
#define EVENT_F3 24
#define EVENT_F4 25
#define EVENT_F5 26
#define EVENT_F6 27
#define EVENT_F7 28
#define EVENT_F8 29
#define EVENT_F9 30
#define EVENT_F10 31
#define EVENT_F11 32
#define EVENT_F12 33
#define EVENT_F13 34
#define EVENT_F14 35
#define EVENT_F15 36
#define EVENT_F16 37
#define EVENT_F20 38

#define EVENT_1L1 39
#define EVENT_1 40
#define EVENT_2 41
#define EVENT_3 42
#define EVENT_4 43
#define EVENT_5 44
#define EVENT_6 45
#define EVENT_7 46
#define EVENT_8 47
#define EVENT_9 48
#define EVENT_0 49
#define EVENT_0R1 50
#define EVENT_0R2 51
#define EVENT_0R3 52
#define EVENT_BACKSPACE 53

#define EVENT_TAB 54
#define EVENT_Q 55
#define EVENT_W 56
#define EVENT_E 57
#define EVENT_R 58
#define EVENT_T 59
#define EVENT_Y 60
#define EVENT_U 61
#define EVENT_I 62
#define EVENT_O 63
#define EVENT_P 64
#define EVENT_PR1 65
#define EVENT_PR2 66
#define EVENT_RETURN 67

#define EVENT_A 68
#define EVENT_S 69
#define EVENT_D 70
#define EVENT_F 71
#define EVENT_G 72
#define EVENT_H 73
#define EVENT_J 74
#define EVENT_K 75
#define EVENT_L 76
#define EVENT_LR1 77
#define EVENT_LR2 78
#define EVENT_LR3 79

#define EVENT_ZL1 80
#define EVENT_Z 81
#define EVENT_X 82
#define EVENT_C 83
#define EVENT_V 84
#define EVENT_B 85
#define EVENT_N 86
#define EVENT_M 87
#define EVENT_MR1 88
#define EVENT_MR2 89
#define EVENT_MR3 90

#define EVENT_MIDDLE1 91		/* PC: Insert    */
#define EVENT_MIDDLE2 92		/* PC: Home      */
#define EVENT_MIDDLE3 93		/* PC: Page Up   */
#define EVENT_MIDDLE4 94		/* PC/Amiga: Delete    */
#define EVENT_DEL 94
#define EVENT_MIDDLE5 95		/* PC: End, Amiga: Help       */
#define EVENT_HELP 95
#define EVENT_MIDDLE6 96		/* PC: Page Down */

#define EVENT_DOWNARROW 97
#define EVENT_UPARROW 98
#define EVENT_RIGHTARROW 99
#define EVENT_LEFTARROW 100

#define EVENT_KP_E1 101		/* Amiga: the one above 7 on the keypad */
#define EVENT_KP_E2 102		/* -------------------- 8 ------------- */

#define EVENT_KP_DIV 103
#define EVENT_KP_MUL 104
#define EVENT_KP_SUB 105
#define EVENT_KP_ADD 106

#define EVENT_KP_0 107
#define EVENT_KP_DOT 108
#define EVENT_KP_ENTER 109

#define EVENT_KP_1 110
#define EVENT_KP_2 111
#define EVENT_KP_3 112

#define EVENT_KP_4 113
#define EVENT_KP_5 114
#define EVENT_KP_6 115

#define EVENT_KP_7 116
#define EVENT_KP_8 117
#define EVENT_KP_9 118

#define EVENT_SPACE 119


#define EVENT_MAX 119
/*...Room for more, update EVENT_MAX to allways be the last number... */



/* KEYSWITCHes: */

#define EVENT_LEFTCTRL (1<<1)
#define EVENT_RIGHTCTRL (1<<2)
#define EVENT_CAPSLOCK (1<<3)
#define EVENT_LEFTSHIFT (1<<4)
#define EVENT_RIGHTSHIFT (1<<5)
#define EVENT_LEFTALT (1<<6)
#define EVENT_RIGHTALT (1<<7)

#define EVENT_LEFTEXTRA1 (1<<8) /*Amiga: Amiga, PC: "Meta"/Windows key */
#define EVENT_RIGHTEXTRA1 (1<<9) /*Amiga: Amiga, PC: "Meta"/Windows key */

/*...Room for more... */

#define EVENT_LEFTCTRL_S "120"
#define EVENT_RIGHTCTRL_S "121"
#define EVENT_CAPSLOCK_S "122"
#define EVENT_LEFTSHIFT_S "123"
#define EVENT_RIGHTSHIFT_S "124"
#define EVENT_LEFTALT_S "125"
#define EVENT_RIGHTALT_S "126"
#define EVENT_LEFTEXTRA_S "127"
#define EVENT_RIGHTEXTRA_S "128"
#define EVENT_UP_S "129"

#define EVENT_LEFTCTRL_I 120
#define EVENT_RIGHTCTRL_I 121
#define EVENT_CAPSLOCK_I 122
#define EVENT_LEFTSHIFT_I 123
#define EVENT_RIGHTSHIFT_I 124
#define EVENT_LEFTALT_I 125
#define EVENT_RIGHTALT_I 126
#define EVENT_LEFTEXTRA1_I 127
#define EVENT_RIGHTEXTRA1_I 128
#define EVENT_UP_I 129

#define EVENT_MAX_I 129


struct WrapFuncList{
	char *funcname;
	void *func;
};

#define NoSwitch(a) (a==0)
#define LeftCtrl(a) (a&EVENT_LEFTCTRL)
#define RightCtrl(a) (a&EVENT_RIGHTCTRL)
#define CapsLock(a) (a&EVENT_CAPSLOCK)
#define LeftShift(a) (a&EVENT_LEFTSHIFT)
#define RightShift(a) (a&EVENT_RIGHTSHIFT)
#define AnyShift(a) (LeftShift(a) | RightShift(a))
#define LeftAlt(a) (a&EVENT_LEFTALT)
#define RightAlt(a) (a&EVENT_RIGHTALT)
#define LeftExtra(a) (a&EVENT_LEFTEXTRA1)
#define RightExtra(a) (a&EVENT_RIGHTEXTRA1)

#define OnlyLeftShift(a) (a==EVENT_LEFTSHIFT)
#define OnlyLeftAlt(a) (a==EVENT_LEFTALT)

#endif











