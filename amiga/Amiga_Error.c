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


#include "../config/config.h"

#ifdef NOPLAYER

#include "../common/nsmtracker.h"
#include "../common/OS_error_proc.h"

#include <stdio.h>
#include <stdarg.h>

bool Error_init(void){return true;}

void RError(const char *fmt,...){
  va_list argp;
  
  va_start(argp,fmt);
  /*	vfprintf(stderr,fmt,argp); */
  vfprintf(stderr,fmt,argp);
  va_end(argp);
}

void Error_uninit(void){}


#else

#include <stdarg.h>
#include <proto/exec.h>
#include <proto/dos.h>
#include <proto/graphics.h>
#include <proto/intuition.h>
#include <clib/alib_protos.h>
#include <string.h>
#include <dos/dostags.h>
#include <dos/dos.h>

#include "nsmtracker.h"


struct SignalSemaphore ErrorSemaphore={0};

char errorstring[1000];

ULONG ErrorSig=-1;
struct Task *ErrorTask=NULL;
int errorprocessalive=0;

int hassent=0;

extern char *screenname;
char *errortaskname;

extern struct Screen *mainscreen;
struct Window *errorwindow=NULL;

__saveds void ErrorProcess(void)
{
	char constring[200];
	BPTR file=NULL;
	ULONG sig;
	ULONG lock;
	ErrorSig=AllocSignal(ErrorSig);
	hassent=1;
	errorprocessalive=1;
	ErrorTask=FindTask(NULL);

	sprintf(constring,"CON:%d/%d/%d/%d/%s/SCREEN%s",20,20,500,300,"radium.nfo",screenname);

	for(;;){
		sig=Wait(1L<<ErrorSig|SIGBREAKF_CTRL_C);
		if(sig&1L<<ErrorSig){
			if(file==NULL){
				file=Open(constring,0x3ee);
			}
			if(file!=NULL && errorwindow==NULL){
				if(mainscreen!=NULL){
					struct Window *tempwindow;
					lock=LockIBase(0);
						tempwindow=mainscreen->FirstWindow;
						for(;;){
							if(tempwindow==NULL) break;
							if(!strcmp(tempwindow->Title,"radium.nfo")){
								errorwindow=tempwindow;
								break;
							}
							tempwindow=tempwindow->NextWindow;
						}
					UnlockIBase(lock);
 				}
			}

			if(file==NULL){
				fprintf(stderr,errorstring);
			}else{
				FPuts(file,errorstring);
			}
			hassent=1;

			if(errorwindow!=NULL){
				WindowToFront(errorwindow);
			}
		}
		if(sig&SIGBREAKF_CTRL_C) break;
	}

	if(file!=NULL){
		Close(file);
	}
	FreeSignal(ErrorSig);
}


bool Error_init(void){

	errortaskname=malloc(400);

	sprintf(errortaskname,"ErrorPortProc for: %s",screenname);

	InitSemaphore(&ErrorSemaphore);


	if(
		CreateNewProcTags(
			NP_Entry,ErrorProcess,
			NP_Name,errortaskname,
//			NP_Output,Open(constring,0x3ee),
			TAG_END
		)==NULL
	){
		fprintf(stderr,"Could not open Error Process\n");
		return false;
	}

	do{
		Delay(1);
	}while(ErrorTask==NULL);

	hassent=0;

	return true;
}



/*************************************************
   FUNCTION
     A thread-safe function that displays a text
     to stderr (will hopefully be moved to its
     own window that pops up later). Can be
     called both from a task and a process.
   NOTE
     May use some time. Only ment for
     displaying errors.
*************************************************/

void RError(const char *fmt,...){
	va_list argp;

	ObtainSemaphore(&ErrorSemaphore);

	va_start(argp,fmt);

	vsprintf(errorstring,fmt,argp);
	if(errorprocessalive==0){			//Should only happen before the player-task has been opened and after it has been closed.
		fprintf(stderr,"Error: Errorprocess not alive\n");
		fputs(errorstring,stderr);
	}else{
		Signal(ErrorTask,1L<<ErrorSig);
		while(hassent==0){
			WaitTOF();
		}
		hassent=0;
	}

	va_end(argp);

	ReleaseSemaphore(&ErrorSemaphore);
}

void ErrorProces_stop(void){
	struct Task *task;

	errorprocessalive=0;

	Signal(ErrorTask,SIGBREAKF_CTRL_C);

	do{
		Delay(4);
		Forbid();
			task=FindTask(errortaskname);
		Permit();
	}while(task!=NULL);

}

void Error_uninit(void){

	ErrorProces_stop();
}

/*
void main(){
	if( ! Error_init()) return;

	DisplayError("hallo hallo\n");
	DisplayError("hallo hallo1\n");
	DisplayError("hallo hallo2\n");
	DisplayError("hallo hallo3\n");
	Error_uninit();
}
*/

#endif

