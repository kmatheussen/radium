keysub=(

"NO", # Default value. Means that it's not used

# Qualifier keys ("UP" is for keyboard release). These must be placed here, starting at position 1.
"CTRL_L","CTRL_R","CAPS","SHIFT_L","SHIFT_R","ALT_L","ALT_R","EXTRA_L","EXTRA_R","UP",
#   1       2         3     4         5        6        7         8         9      10
"MOUSE_EDITOR", "MOUSE_MIXER", "MOUSE_SEQUENCER", "MOUSE_MIXERSTRIPS", "FOCUS_EDITOR", "FOCUS_MIXER", "FOCUS_SEQUENCER", "FOCUS_MIXERSTRIPS",
#  11               12            13                 14                    15             16               17                18
    
    
"FIRST_NON_QUALIFIER", # also not used. keys lower than this are qualifier, higher are normal keys.

"ESC",

"F1",
"F2",
"F3",
"F4",
"F5",
"F6",
"F7",
"F8",
"F9",
"F10",
"F11",
"F12",
"F13",
"F14",
"F15",
"F16",
"F20",

"1L1",
"1", # Note: order between "1" and "0" can not change.
"2",
"3",
"4",
"5",
"6",
"7",
"8",
"9",
"0",
"0R1",
"0R2",
"0R3",
"BACKSPACE",

"TAB",
"Q",
"W",
"E",
"R",
"T",
"Y",
"U",
"I",
"O",
"P",
"PR1",
"PR2",
"RETURN",

"A",
"S",
"D",
"F",
"G",
"H",
"J",
"K",
"L",
"LR1",
"LR2",
"LR3",

"ZL1",
"Z",
"X",
"C",
"V",
"B",
"N",
"M",
"MR1",
"MR2",
"MR3",

"INSERT",
"HOME",
"PAGE_UP",
"DEL",
"END",
"PAGE_DOWN",

"DOWNARROW",
"UPARROW",
"RIGHTARROW",
"LEFTARROW",

"KP_E1",		# Amiga: the one above 7, on the keypad */
"KP_E2",		# -------------------- 8, ------------- */

"KP_DIV",
"KP_MUL",
"KP_SUB",
"KP_ADD",

"KP_0",
"KP_DOT",
"KP_ENTER",

"KP_1",
"KP_2",
"KP_3",

"KP_4",
"KP_5",
"KP_6",

"KP_7",
"KP_8",
"KP_9",

"MENU",
"VOLUME_DOWN",
"VOLUME_UP",
"MUTE",
"PLAY",
"STOP",

"CALCULATOR",
"MAIL",
"HOMEPAGE",

"SPACE",

"QWERTY_1", "QWERTY_2", "QWERTY_3", "QWERTY_4", "QWERTY_5", "QWERTY_6", "QWERTY_7", "QWERTY_8", "QWERTY_9", "QWERTY_0",
"QWERTY_Q", "QWERTY_W", "QWERTY_E", "QWERTY_R", "QWERTY_T", "QWERTY_Y", "QWERTY_U", "QWERTY_I", "QWERTY_O", "QWERTY_P",
"QWERTY_A", "QWERTY_S", "QWERTY_D", "QWERTY_F", "QWERTY_G", "QWERTY_H", "QWERTY_J", "QWERTY_K", "QWERTY_L",
"QWERTY_Z", "QWERTY_X", "QWERTY_C", "QWERTY_V", "QWERTY_B", "QWERTY_N", "QWERTY_M",

"EAT_BUT_DO_NOTHING" # This event is eaten, but does nothing.
)


#  // middle1 = insert
#  // middle2 = home
#  // middle3 = page up
#  // middle4 = del
#  // middle5 = end / help
#  // middle6 = page down

#keysub+=("/CTRL_L","/CTRL_R","/CAPS","/SHIFT_L","/SHIFT_R","/ALT_L","/ALT_R","/EXTRA_L","/EXTRA_R")

# qualsub must be in the same order as the qualifier keys. (it's just shortcuts)
qualsub=("CL","CR","CAPS","SL","SR","AL","AR","EL","ER")
#qualsub=("/CL","/CR","/CAPS","/SL","/SR","/AL","/AR","/EL","/ER")

