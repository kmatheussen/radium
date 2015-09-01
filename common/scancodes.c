
#include "nsmtracker.h"

#include "scancodes_proc.h"


/*
virtual key (vk) to scancode: https://msdn.microsoft.com/en-us/library/windows/desktop/ms646306%28v=vs.85%29.aspx
scancode table: https://msdn.microsoft.com/en-us/library/Aa299374%28v=VS.60%29.aspx
http://www.quadibloc.com/comp/scan.htm
*/
static int scancode[0x100] = {EVENT_NO};
static void init_scancodes(void){
#define s(a,b) scancode[0x##a] = EVENT_##b
  
  s(01, 	ESC);  s(21,    F);    s(41,	F7);
  s(02, 	1);    s(22, 	G);    s(42, 	F8);
  s(03, 	2);    s(23, 	H);    s(43, 	F9);
  s(04, 	3);    s(24, 	J);    s(44, 	F10);
  s(05, 	4);    s(25, 	K);    s(57, 	F11); // s(84, printscreen)
  s(06, 	5);    s(26, 	L);    s(58,    F12); //  s(70, scrollock)
  s(07, 	6);    s(27, 	LR1);  s(47, 	HOME);
  s(08, 	7);    s(28, 	LR2);  s(48, 	UPARROW);
  s(09, 	8);    s(29, 	1L1);  s(49, 	PAGE_UP);
  s(0A, 	9);
  s(0B,	        0);    s(2B,    LR3);  s(4B, 	LEFTARROW);
  s(0C, 	0R1);  s(2C, 	Z);
  s(0D, 	0R2);  s(2D, 	X);    s(4D, 	RIGHTARROW);
  s(0E, 	BACKSPACE); s(2E,C);
  s(0F, 	TAB);  s(2F, 	V);    s(4F, 	END);
  s(10, 	Q);    s(30, 	B);    s(50, 	DOWNARROW);
  s(11, 	W);    s(31, 	N);    s(51,  	PAGE_DOWN);
  s(12, 	E);    s(32, 	M);    s(52, 	INSERT);
  s(13, 	R);    s(33, 	MR1);  s(53, 	DEL);
  s(14, 	T);    s(34, 	MR2);
  s(15, 	Y);    s(35, 	MR3);
  s(16, 	U);
  s(17, 	I);
  s(18, 	O);
  s(19, 	P);    s(39, 	SPACE);
  s(1A, 	PR1);
  s(1B, 	PR2);  s(3B, 	F1);
  s(1C,  	RETURN); s(3C, 	F2);
                       s(3D, 	F3);
  s(1E, 	A);    s(3E, 	F4);
  s(1F, 	S);    s(3F, 	F5);
  s(20,         D);    s(40,    F6);

#undef s
}
  
int get_subID_from_scancode(int scancode_code){
  static bool scancodes_inited = false;

  if (scancodes_inited==false){
    init_scancodes();
    scancodes_inited=true;
  }

  R_ASSERT_RETURN_IF_FALSE2(scancode >= 0, EVENT_NO);
  
  if (scancode_code >= 0x100)
    return EVENT_NO;

  int subID = scancode[scancode_code];
  
  printf(" SCANCODE: %x, subID: %d\n", scancode_code, subID);

  return subID;
}
