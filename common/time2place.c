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
#include "time_proc.h"
#include "playerclass.h"
#include "PEQcommon_proc.h"
#include "placement_proc.h"

#include "time2place_proc.h"



/*******************************************************

  FUNCTION
    Does the opposite of Place2STime. (pretty obvious).
    Necesarry for being able to edit while playing.

    When the player edits when playing, the player stops,
    convert the current time to place with this function,
    and when finished editing, built up new PEQelements,
    and continue playing. The timer is not stopped, so
    it might be lots of notes coming at once if it is
    a function that takes some time.

    Is allso used by editing-functions that want to
    set a note/fxnode/velocity/etc. at the current
    place being played.

  HOW IT WORKS
    It works by first finding the line by using a
    binary-search, then check the timechanges if any.

*******************************************************/


extern struct Root *root;
/*
void STime2Place(
	struct Blocks *block,
	STime time,
	Place *toplace
){
	struct STimes *stimes=block->times;
	struct STimes *stime;
	struct STimeChanges *timechange;
	struct STimeChanges *prev;
	int low=0,mid,high=block->num_lines-1;
	STime lasttime=getBlockSTimeLength(block);
	STime time1,time2,orgtime2;
	float fp1,fp2;

	time=PC_TimeToRelBlockStart(time);
	if(time>lasttime)
		time=PC_TimeToRelBlockStart(time);
	
	if(high==1){
		if(time>=stimes[1].time){
			mid=1;
		}else{
			mid=0;
		}
	}else{
		mid=high/2;

		while(time<stimes[mid].time || time>=stimes[mid+1].time){	// NB. stimes[num_lines] is defined. (length of block in STime format actually)

			if(time<stimes[mid].time){
				high=mid-1;
			}else{
				low=mid+1;
			}
			if((high+low)/2==mid){
				RError("Error in function 'time2place" in file 'time2place.c'\n");
				RError("Ooops, line: %d, low: %d, mid: %d, high: %d\n",root->song->tracker_windows->wblock->curr_realline,low,mid,high);
				RError("Time: %d, midtime: %d, midtime+1: %d\n",time,stimes[mid].time,stimes[mid+1].time);
				break;
			}
			mid=(high+low)/2;
		}
	}

	toplace->line=mid;

	stime=&stimes[mid];

	time1=stime->time;

	timechange=stime->timechanges;
	if(timechange==NULL){
		time2=stimes[mid+1].time;
		toplace->counter=time-time1;
		toplace->dividor=time2-time1;
		PlaceHandleOverflow(toplace);
	}else{
		time2=time1;
		prev=NULL;
		for(;;){
			time1=time2;
			time2=timechange->time;
			if(time2>time){
				break;
			}
			prev=timechange;
			timechange=NextSTimeChange(timechange);
			if(timechange==NULL){
				time1=time2;
				time2=stimes[mid+1].time;
				break;
			}
		}
		if(prev==NULL){
			toplace->counter=time-time1;
			toplace->dividor=time2-time1;
			PlaceHandleOverflow(toplace);
		}else{
			if( 0.0f != prev->tempo1 ){
			}else{
				fp1=GetfloatFromPlace(&prev->l.p);
				if(timechange==NULL){
					fp2=(float)(mul+1);
				}else{
					fp2=GetfloatFromPlace(&timechange->l.p);
				}
				toplace->counter=time;
				toplace->dividor=time2-time1;
			}
		}
	}


	Pdebug("line: %d, time: %d, lasttime: %d\n",mid,time,lasttime);
}
*/

/*
6

0 - 0
1 - 1
2 - 4
3 - 6
4 - 9
5 -11

1.
low=0
mid=5*(11-6)/11=5*5/11=25/11=2
high=5

2.while(time<=4 || time>6) = true.

3.if(time<4) = false;
low=2
mid=(5+2)/2=7/2=3

4.while(time<=6 || time>6) = false

mid=3 ->Correct.

-----------------------

Problem:

Tid kan rett som det er være mer enn lasttime.
Hva skjer så?

-Løsninga blir jo da å gå til neste
 block, men block er jo input.

-Kanskje block ikke skulle vært input, men
 en retur-verdi.

-Men samtidig er det jo kaller sin oppgave å sørge
 for at man er i riktig block. Kanskje dette burde
 være denne procedure sin oppgave også.

*/


