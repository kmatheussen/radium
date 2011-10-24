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



/* This file is a direct c-translation of the readstr.a source from
   the NSM package. Tried to to it the ansi/iso-c way first, but it didn't
   work that well. But that doesn't matter as this is amiga-
   spesific code anyway.
*/


#include <string.h>
#include <intuition/screens.h>
#include <proto/intuition.h>
#include <proto/dos.h>
#include "nsmtracker.h"

#include "Amiga_readstr_proc.h"

extern struct Screen *Scr;

BPTR OpenCon(int width,int height,char *title,char *screenname){
	WORD left_edge;
	WORD top_edge;
	struct Screen *screen;
	char constring[200];
	BPTR ret;

/*
	screen=LockPubScreen(screenname);
	if(screen==NULL){
		screen=LockPubScreen(NULL);
	}
*/
	screen=Scr;
/* Find the left-edge. */
	left_edge=screen->LeftEdge;
	left_edge= -left_edge;
	left_edge+=50;

/* Find the top-edge */

	top_edge=screen->TopEdge;
	top_edge= -top_edge;
/*
	UnlockPubScreen(screenname,screen);
*/
	sprintf(constring,"CON:%d/%d/%d/%d/%s/SCREEN%s",left_edge,top_edge,width,height,title,screenname);

	ret=Open(constring,0x3ee);

	if(ret==0){
		RError("Error. Could not open Con. ReturnCode from IoErr(): %d\n",IoErr());
		RError("(Check out include:dos/dos.h for explanation\n");
	}

	return ret;
}

void closefile(BPTR file){
	Close(file);
}

void readstring(BPTR file,char *buffer,int length){
	int actuallength=Read(file,buffer,(long)length);
	buffer[actuallength-1]='\0';
}

void writestring(BPTR file,char *buffer){
	Write(file,buffer,(long)strlen(buffer));
}


/*
int main(){
	char buffer[50];
	BPTR fil=OpenCon(50,100,"jess",NULL);
	readstring(fil,buffer,40);
	writestring(fil,"testing");
	readstring(fil,buffer,40);
	writestring(fil,"testing");
	readstring(fil,buffer,40);
	printf("%s\n",buffer);
	closefile(fil);
	return 0;
}
*/














