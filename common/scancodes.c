
#include "nsmtracker.h"

#include "scancodes_proc.h"


/*
virtual key (vk) to scancode: https://msdn.microsoft.com/en-us/library/windows/desktop/ms646306%28v=vs.85%29.aspx
scancode table: https://msdn.microsoft.com/en-us/library/Aa299374%28v=VS.60%29.aspx
http://www.quadibloc.com/comp/scan.htm
ordinary scan codes: http://www.win.tue.nl/~aeb/linux/kbd/scancodes-1.html
*/
static int *scancode;

__attribute__((constructor)) static void initialize_scancode(void) {
	scancode = calloc(0x100, sizeof(int));
	for(int i=0;i<0x100;i++)
		scancode[i] = EVENT_NO;
}

static void init_scancodes(void){
#define s(a,b) scancode[0x##a] = EVENT_##b
  
  s(01, 	ESC);         s(21,     QWERTY_F);    s(41,	F7);
  s(02, 	QWERTY_1);    s(22, 	QWERTY_G);    s(42, 	F8);
  s(03, 	QWERTY_2);    s(23, 	QWERTY_H);    s(43, 	F9);
  s(04, 	QWERTY_3);    s(24, 	QWERTY_J);    s(44, 	F10);
  s(05, 	QWERTY_4);    s(25, 	QWERTY_K);    s(57, 	F11); // s(84, printscreen)
  s(06, 	QWERTY_5);    s(26, 	QWERTY_L);    s(58,     F12); //  s(70, scrollock)
  s(07, 	QWERTY_6);    s(27, 	LR1);         s(47, 	HOME);
  s(08, 	QWERTY_7);    s(28, 	LR2);         s(48, 	UPARROW);
  s(09, 	QWERTY_8);    s(29, 	1L1);         s(49, 	PAGE_UP);
  s(0A, 	QWERTY_9);
  s(0B,	        QWERTY_0);    s(2B,     LR3);         s(4B, 	LEFTARROW);
  s(0C, 	0R1);         s(2C, 	QWERTY_Z);
  s(0D, 	0R2);         s(2D, 	QWERTY_X);    s(4D, 	RIGHTARROW);
  s(0E, 	BACKSPACE);   s(2E,     QWERTY_C);
  s(0F, 	TAB);         s(2F, 	QWERTY_V);    s(4F, 	END);
  s(10, 	QWERTY_Q);    s(30, 	QWERTY_B);    s(50, 	DOWNARROW);
  s(11, 	QWERTY_W);    s(31, 	QWERTY_N);    s(51,  	PAGE_DOWN);
  s(12, 	QWERTY_E);    s(32, 	QWERTY_M);    s(52, 	INSERT);
  s(13, 	QWERTY_R);    s(33, 	MR1);         s(53, 	DEL);
  s(14, 	QWERTY_T);    s(34, 	MR2);
  s(15, 	QWERTY_Y);    s(35, 	MR3);
  s(16, 	QWERTY_U);
  s(17, 	QWERTY_I);
  s(18, 	QWERTY_O);
  s(19, 	QWERTY_P);    s(39, 	SPACE);
  s(1A, 	PR1);         s(3A,     CAPS);
  s(1B, 	PR2);         s(3B, 	F1);
  s(1C,  	RETURN);      s(3C, 	F2);
                              s(3D, 	F3);
  s(1E, 	QWERTY_A);    s(3E, 	F4);
  s(1F, 	QWERTY_S);    s(3F, 	F5);
  s(20,         QWERTY_D);    s(40,    F6);

  s(56,         ZL1);

  // Extra:
  /*
  s(69, LEFTARROW);
  s(6A, LEFTARROW);
  s(67, UPARROW);
  s(6c, DOWNARROW);

  s(68, PAGE_UP);
  s(6d, PAGE_DOWN);
  */
  
#undef s
}
  
int get_subID_from_scancode(int scancode_code){
  static bool scancodes_inited = false;

  if (scancodes_inited==false){
    init_scancodes();
    scancodes_inited=true;
  }

  R_ASSERT_RETURN_IF_FALSE2(scancode_code >= 0, EVENT_NO);
  
  if (scancode_code >= 0x100)
    return EVENT_NO;

  int subID = scancode[scancode_code];
  
  //printf(" SCANCODE: %x, subID: %d\n", scancode_code, subID);

  return subID;
}
