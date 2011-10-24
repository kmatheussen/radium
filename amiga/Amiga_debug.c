/* This file is at the moment amiga-spesific. This will change. */



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


#include <stdio.h>
#include <stdarg.h>
#include <proto/exec.h>
#include <exec/semaphores.h>

#include "../common/debug_proc.h"



int dodebug=1;

ULONG debugsig;
char debugg[500];




#ifdef TRACKER_DEBUG

struct SignalSemaphore PdebugSemaphore={0};

char debugg[500]="testing\n";

int lastpdebug=0;
char pdebugg[20][200];

extern struct Task *mytask;

void debug(const char *fmt, ...){
	va_list argp;

	if(dodebug==0) return;

	va_start(argp,fmt);
/*	vfprintf(stderr,fmt,argp); */
	vfprintf(stdout,fmt,argp);
	va_end(argp);
}

int semaphoreinit=0;

void DoPdebug(void){
	int lokke;

	if(semaphoreinit==0){
		InitSemaphore(&PdebugSemaphore);
		semaphoreinit=1;
	}
	ObtainSemaphore(&PdebugSemaphore);

	for(lokke=0;lokke<lastpdebug;lokke++){
		fprintf(stderr,"%d.%s",lokke,&pdebugg[lokke][0]);
	}
	lastpdebug=0;

	ReleaseSemaphore(&PdebugSemaphore);
}

void __saveds Pdebug(const char *fmt, ...){
	va_list argp;

	if(dodebug==0) return;

	if(0==semaphoreinit){
		InitSemaphore(&PdebugSemaphore);
		semaphoreinit=1;
	}

	va_start(argp,fmt);

	ObtainSemaphore(&PdebugSemaphore);

		if(lastpdebug<19){

			vsprintf(&pdebugg[lastpdebug][0],fmt,argp);
			lastpdebug++;

			Signal(mytask,debugsig);
		}

	ReleaseSemaphore(&PdebugSemaphore);

	va_end(argp);
}


#endif

#ifndef TRACKER_DEBUG

__inline void debug(const char *fmt, ...){
	return;
}

__inline void Pdebug(const char *fmt, ...){
	return;
}

#endif
