keysub=(

"NO", # Default value. Means that it's not used

# Qualifier keys ("UP" is for keyboard release). These must be placed here, starting at position 1.
"CTRL_L","CTRL_R","CAPS","SHIFT_L","SHIFT_R","ALT_L","ALT_R","EXTRA_L","EXTRA_R","UP",

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
"1",
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

"SPACE"

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

