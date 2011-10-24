
#ifndef TRACKER_INCLUDE

int Mul32Div64_000(const int a,const int b,const int c);

#ifdef _AMIGA
extern int __asm Mul32Div64_020(
	register __d0 int a,
	register __d1 int b,
	register __d2 int c
);
#endif

#endif



#define Mul32Div64(a,b,c) Mul32Div64_000(a,b,c)


#ifdef _M68020

#ifndef _M68060		//muls and divs are emulated on 68060, so Mul32Div64_000 is actually twice as fast as Mul32Div64_020

#undef Mul32Div64
#define Mul32Div64(a,b,c) Mul32Div64_020(a,b,c)

#endif

#endif


