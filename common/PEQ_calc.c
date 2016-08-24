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
********************************************************/

STime PEQ_CalcNextEvent(
	STime time1,
	STime time,
	STime time2,
	int x1,
	int *x,
	int x2,
        int logtype
){

  STime ret_time;
  
#ifndef RELEASE
  //R_ASSERT(time2>time1);
  R_ASSERT(time>=time1);
  R_ASSERT(time2>=time);
#endif
  
  if (time==time2){
    *x=x2;
    ret_time = time2+1;
    goto exit;
  }

  if (logtype == LOGTYPE_HOLD){
    *x = x1;
    ret_time = time2-1;
    
    if (ret_time==time)
      ret_time = time2;
    
    goto exit;
  }

  if(x1==x2){
    *x=x2;
    ret_time = time2;
    goto exit;
  }

  int new_x = (int)scale_int64(time,time1,time2,x1,x2);
  *x = new_x;

  if (x2<x1)
    ret_time = (int)scale_int64(new_x-1, x1, x2, time1, time2);
  else
    ret_time = (int)scale_int64(new_x+1, x1, x2, time1, time2);

  if(ret_time <= time){
    //RError("Error in file PEQ_calc.c. ret: %d, time: %d\n",(int)ret_time,(int)time); // this doesn't look like an error. It's just telling us that the x resolution is bigger than the time resolution.
    ret_time = time+1;
  }

 exit:
  
#ifndef RELEASE
  R_ASSERT(ret_time >= time1);
  R_ASSERT(ret_time > time);
#endif

  // Ensurance. Not supposed to happen.
  if (ret_time==time)
    ret_time = time2;

  return ret_time;
}


#if 0

STime PEQ_CalcNextEvent_old(
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
          //RError("Too fast! time2 or x2 in PEQ_calc.c is 0. time2: %d, x2: %d\n",time2,x2);
		return time2+time1;
	}

	// Adding one in here is a quick way to avoid problems with rounding errors.
	// The solution might be /to/ quick... I'm not shure. Seems to work well for now:
	// (Doesn't allways work very well (no that was an overflow problem))

	timesubtime1=time-time1;

	*x=x1+Mul32Div64(
			 x2,
			 timesubtime1 + 1,
			 time2
			 );

/*
		if(*x>=0x2000 || *x<-0x2000){
			Pdebug("oops, x: %d, x1: %d, x2: %d, time: %d, time1: %d, time2: %d\n",*x,x1,x2,time,time1,time2);
		}
*/

	//printf(" *x: %d, x1: %d\n",*x,x1);
	ret=time1 + Mul32Div64(
			       time2,
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

#endif




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



