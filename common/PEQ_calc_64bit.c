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






#ifdef _AMIGA
#include <proto/utility.h>
#include <dos.h>
#endif

#include <stdio.h>
#include <stdlib.h>

#include "PEQ_calc_64bit_proc.h"


/**************************************
  FUNCTION
    Returns a*b/c. a*b may be bigger
    than a 32 bit variable can hold.

  NOTE
    The 68020 and higher versions is
    defined in the 64bit.a file.
    (a bit smaller, that one. :)
    Probably noone that is ever going
    to use this routine (very unlikely
    that anyone wants to use the
    program on an 000), but I wrote
    this before I knew there was an asm
    routines that does this directly.

  NOTE2
    This function is VERY hard and
    heavy on an 000.

  NOTE3
    68060 version allso uses this function.
    muls and divs is emulated on an
    060, so this function is twice
    as fast as the other one.
**************************************/


#ifndef _AMIGA

int Mul32Div64_000(const int a,const int b,const int c){
  int64_t a2=a;
  int64_t b2=b;
  int64_t c2=c;

  a2=a2*b2;
  return (int)(a2/c2);
}

#else

int Mul32Div64_000(const int a,const int b,const int c){

#ifdef _AMIGA
	unsigned long l32=UMult64((unsigned long)abs(a),(unsigned long)abs(b));
	unsigned long m32=getreg(REG_D1);
#else
	THIS IS TOTALLY WRONG! (UMult64 does not divide)
	unsigned long l32=(unsigned long)abs(a)/(unsigned long)abs(b);
	unsigned long m32=(unsigned long)abs(a)%(unsigned long)abs(b);
#endif

	unsigned long result;
	unsigned long modulo;
	unsigned long uc;

	//printf("Mul32Div64_000 %d %d %d\n",a,b,c);

	if( m32==0 && ( (1<<31 & l32)==0 ) ){
		return (a<0)^(b<0) ? -(int)(l32)/c : (int)(l32)/c;
	}
//	printf("storre\n");

	uc=(unsigned long)abs(c);

#ifdef _AMIGA
	result=UDivMod32(1<<31,uc);
	modulo=getreg(REG_D1);
#else
	result=(1<<31)/uc;
	modulo=(1<<31)%uc;
#endif

	modulo*=2;
	result*=2;

	if(modulo>=uc){
		modulo-=uc;
		result=(result+1)*m32;
	}else{
		result*=m32;
	}

#ifdef _AMIGA
	m32=UDivMod32(m32*modulo,uc);
	modulo=getreg(REG_D1);

	l32=UDivMod32(l32,uc);
	modulo+=getreg(REG_D1);
#else
	m32=(m32*modulo)/uc;
	modulo=(m32*modulo)&uc;

	l32=l32/uc;
	modulo+=132%uc;
#endif

	return (int)(
		(a<0)^(b<0)^(c<0) ?
		-(long)(
			result + m32 + l32 + (modulo>=uc)
		):(
			result + m32 + l32 + (modulo>=uc)
		)
	);


}
#endif


/*
int main(){
	int a,b,c,d=0;

	for(a=25000;a<25010;a++){
		printf("a: %d\n",a);
		for(b=184000;b<185000;b++){
			for(c=200000;c<200100;c++){
				d+=Mul32Div64_000(a,b,c);
			}
		}
	}
	printf("d: %d\n",d);
	d=0;

	for(a=25000;a<25010;a++){
		printf("b: %d\n",a);
		for(b=184000;b<185000;b++){
			for(c=200000;c<200100;c++){
				d+=Mul32Div64_020(a,b,c);
			}
		}
	}
	printf("d: %d\n",d);

//	printf("d: %d, %x\n",16383*-20569/185620,Mul32Div64(a,b,c));

	return 0;
}

*/

