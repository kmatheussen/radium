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






#include "nsmtracker.h"
#include "playerclass.h"
#include "placement_proc.h"
#include "time_proc.h"
#include "PEQ_calc_64bit_proc.h"

#include "PEQ_calc_proc.h"


/********************************************************
  FUNCTION
    Finds both the current x, and returns the next time
    x should be changed. Returns time2 if x1==x2.

  NOTE
    Inputs must be: time1<=time<time2.
  NOTE2
    Total mess? Yes, but it works. (hopefully)
********************************************************/

STime PEQ_CalcNextEvent(
	STime time1,
	STime time,
	STime time2,
	int x1,
	int *x,
	int x2
){
	STime ret;
	STime timesubtime1;

	x2-=x1;

	if(x2==0){
		*x=x1;
		return time2;
	}

	time2-=time1;


	if(time2==0 || x2==0){
		RError("Too fast! time2 or x2 in PEQ_calc.c is 0. time2: %d, x2: %d\n",time2,x2);
		return time2+time1;
	}

	// Adding one in here is a quick way to avoid problems with rounding errors.
	// The solution might be /to/ quick... I'm not shure. Seems to work well for now:
	// (Doesn't allways work very well (no that was an overflow problem))

	timesubtime1=time-time1;

	*x=x1+Mul32Div64(
			 x2,
			 (int)(timesubtime1+1),
			 (int)(time2)
			 );

/*
		if(*x>=0x2000 || *x<-0x2000){
			Pdebug("oops, x: %d, x1: %d, x2: %d, time: %d, time1: %d, time2: %d\n",*x,x1,x2,time,time1,time2);
		}
*/

	//printf(" *x: %d, x1: %d\n",*x,x1);
	ret=time1 + Mul32Div64(
			       (int)time2,
			       *x + (x2<0 ? -1 : 1) - x1,
			       x2
			       );

//	ret= time1 + ( time2*( *x + (x2<0 ? -1 : 1) - x1 ) /  x2 ) ;

	//This! should work. 

	if(ret<=time){
		RError("Error in file PEQ_calc.c. ret: %d, time: %d\n",ret,time);
		ret=time+1;
	}

	// Do some "hacking" then.. (strange, it didn't help!)

/*
	while(ret<=time){
		*x += (x2<0 ? -1 : 1);
		ret= time1 + ( time2*( *x + (x2<0 ? -1 : 1) - x1 ) /  x2 ) ;
	}
*/

	return ret;
}





/*
time1=0
time=172520
time2=476307
x1=127
x2=0

x2=-127
time2=476307

*x=127 + (-127*172520/476307 = 127 - 45 = 82

time= 0 + (476307*(82 -1 - 127)/-127 = 476307*(-46)/(-127) = 172520



*/



