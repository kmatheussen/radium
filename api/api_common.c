/* Copyright 2001 Kjetil S. Matheussen

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




#include "../common/nsmtracker.h"
#include "../common/list_proc.h"
#include "../common/placement_proc.h"
#include "../common/notes_proc.h"

#include "api_common_proc.h"

#include <Python.h>

extern struct Root *root;



/* Hmm, well, okey, I put the init_radium function here. */


PyObject *gotkeyFunc=NULL;

int
#ifdef _AMIGA
__stdargs __saveds
#endif
radium_main(char *arg);

void init_radium(char *arg,PyObject *gkf){
	static bool started=false;

	if(started==true) return;

	started=true;

	if(!PyCallable_Check(gkf)){
		fprintf(stderr,"gotKey is definitely not gotKey. Check start.py\n");
		return;
	}

	Py_XINCREF(gkf);

	gotkeyFunc=gkf;

	radium_main(arg);

	Py_XDECREF(gkf);
}



/*******************************************
	Common operations
*******************************************/

struct Tracker_Windows *getWindowFromNum(int windownum){
	if(windownum==-1) return root->song->tracker_windows;
	return (struct Tracker_Windows *)ListFindElement1_r0(&root->song->tracker_windows->l,(NInt)windownum);
}


struct WBlocks *getWBlockFromNum(int windownum,int wblocknum){
	struct Tracker_Windows *window=getWindowFromNum(windownum);
	if(window==NULL) return NULL;

	if(wblocknum==-1) return window->wblock;
	return (struct WBlocks *)ListFindElement1_num(&window->wblocks->l,(NInt)wblocknum);
}


struct WBlocks *getWBlockFromNumA(
	int windownum,
	struct Tracker_Windows **window,
	int blocknum
){
	*window=getWindowFromNum(windownum);
	if(*window==NULL) return NULL;

	if(blocknum==-1) return (*window)->wblock;
	return (struct WBlocks *)ListFindElement1_num(&(*window)->wblocks->l,(NInt)blocknum);
}


struct Blocks *getBlockFromNum(int blocknum){
  return (struct Blocks *)ListFindElement1_r0(&root->song->blocks->l,(NInt)blocknum);
}


struct Tracks *getTrackFromNum(int blocknum,int tracknum){
	struct Blocks *block=getBlockFromNum(blocknum);
	if(block==NULL) return NULL;

	return (struct Tracks *)ListFindElement1_num(&block->tracks->l,(NInt)tracknum);
}

struct WTracks *getWTrackFromNum(
	int windownum,
	int wblocknum,
	int wtracknum
){
	struct WBlocks *wblock=getWBlockFromNum(windownum,wblocknum);
	if(wblock==NULL) return NULL;
	if(wtracknum==-1) return wblock->wtrack;
	return (struct WTracks *)ListFindElement1_num(&wblock->wtracks->l,(NInt)wtracknum);
}

struct WTracks *getWTrackFromNumA(
	int windownum,
	struct Tracker_Windows **window,
	int wblocknum,
	struct WBlocks **wblock,
	int wtracknum
){
	(*wblock)=getWBlockFromNumA(windownum,window,wblocknum);

	if(wblock==NULL){
		fprintf(stderr,"Warning, might be a program-error, wblock==NULL at function getWTrackFromNumA in file api/api_common.c\n");
		return NULL;
	}
	if(wtracknum==-1) return (*wblock)->wtrack;
//	printf("So far.. %d,%d\n",wblocknum,wtracknum);
	return (struct WTracks *)ListFindElement1_num(&(*wblock)->wtracks->l,(NInt)wtracknum);
}

struct Notes *getNoteFromNum(int blocknum,int tracknum,int notenum){
	struct Tracks *track=getTrackFromNum(blocknum,tracknum);
	if(track==NULL) return NULL;

	if(notenum==-1) notenum=0;

	return (struct Notes *)ListFindElement3_num(&track->notes->l,(NInt)notenum);
	
}


struct Notes *getNoteFromNumA(int windownum,int blocknum,int tracknum,int notenum){
	struct WBlocks *wblock=NULL;

	if(notenum==-1){
		return GetCurrNote(getWindowFromNum(windownum));
	}else{
		if(blocknum==-1 || tracknum==-1){
			wblock=getWBlockFromNum(windownum,blocknum);
			blocknum=wblock->l.num;
			if(tracknum==-1){
				tracknum=wblock->wtrack->l.num;
			}
		}
		return getNoteFromNum(blocknum,tracknum,notenum);
	}

}




