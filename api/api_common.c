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


#include "../common/includepython.h"

#include "../common/nsmtracker.h"
#include "../common/list_proc.h"
#include "../common/placement_proc.h"
#include "../common/notes_proc.h"
#include "../common/instruments_proc.h"

#include "api_common_proc.h"


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
	struct Tracker_Windows *ret = ListFindElement1_r0(&root->song->tracker_windows->l,(NInt)windownum);
        if (ret==NULL)
          RError("Window #%d does not exist", windownum);
        return ret;
}


struct WBlocks *getWBlockFromNum(int windownum,int wblocknum){
	struct Tracker_Windows *window=getWindowFromNum(windownum);
	if(window==NULL) return NULL;

	if(wblocknum==-1) return window->wblock;
	struct WBlocks *ret = ListFindElement1_num(&window->wblocks->l,(NInt)wblocknum);
        if (ret==NULL)
          RError("WBlock #%d does not exist", wblocknum);
        return ret;
}


struct WBlocks *getWBlockFromNumA(
	int windownum,
	struct Tracker_Windows **window,
	int blocknum
){
	*window=getWindowFromNum(windownum);
	if(*window==NULL) return NULL;

	if(blocknum==-1) return (*window)->wblock;
	struct WBlocks *ret = ListFindElement1_num(&(*window)->wblocks->l,(NInt)blocknum);
        if (ret==NULL)
          RError("WBlock #%d does not exist", blocknum);
        return ret;
}

struct Blocks *getBlockFromNum(int blocknum){
  struct Blocks *ret = ListFindElement1_r0(&root->song->blocks->l,(NInt)blocknum);
  if (ret==NULL)
    RError("Block #%d does not exist",blocknum);
  return ret;
}


struct Tracks *getTrackFromNum(int blocknum,int tracknum){
	struct Blocks *block=getBlockFromNum(blocknum);
	if(block==NULL) return NULL;

	struct Tracks *ret = ListFindElement1_num(&block->tracks->l,(NInt)tracknum);
        if (ret==NULL)
          RError("Track #%d in Block #%d does not exist",tracknum,blocknum);
        return ret;
}

struct WTracks *getWTrackFromNum(
	int windownum,
	int wblocknum,
	int wtracknum
){
	struct WBlocks *wblock=getWBlockFromNum(windownum,wblocknum);
	if(wblock==NULL) return NULL;
	if(wtracknum==-1) return wblock->wtrack;
	struct WTracks *ret = ListFindElement1_num(&wblock->wtracks->l,(NInt)wtracknum);
        if (ret==NULL)
          RError("WTrack #%d in WBlock #%d in window #%d does not exist",wtracknum, wblocknum, windownum);
        return ret;
}

struct WTracks *getWTrackFromNumA(
	int windownum,
	struct Tracker_Windows **window,
	int wblocknum,
	struct WBlocks **wblock,
	int wtracknum
){
	if(wblock==NULL){
		RError("Warning, might be a program-error, wblock==NULL at function getWTrackFromNumA in file api/api_common.c\n");
		return NULL;
	}

      	(*wblock)=getWBlockFromNumA(windownum,window,wblocknum);

	if(wtracknum==-1) return (*wblock)->wtrack;
//	printf("So far.. %d,%d\n",wblocknum,wtracknum);
	struct WTracks *ret = ListFindElement1_num(&(*wblock)->wtracks->l,(NInt)wtracknum);
        if (ret==NULL)
          RError("WTrack #%d in WBlock %d does not exist",wtracknum, wblocknum);
        return ret;
}

                             
struct Notes *getNoteFromNumA(int windownum,struct Tracker_Windows **window, int blocknum, struct WBlocks **wblock, int tracknum, struct WTracks **wtrack, int notenum){
  (*wtrack) = getWTrackFromNumA(windownum, window, blocknum, wblock, tracknum);
  if ((*wtrack)==NULL)
    return NULL;

  if(notenum==-1){
    
    struct Notes *note = GetCurrNote(*window);
    if (note==NULL) {
      RError("Note #%d in track #%d in block #%d in window #%d does not exist",notenum,tracknum,blocknum,windownum);
      return NULL;
    }else
      return note;
    
  }else{
    
    struct Tracks *track = (*wtrack)->track;
    struct Notes *ret = ListFindElement3_num(&track->notes->l,(NInt)notenum);
    if (ret==NULL)
      RError("Note #%d in track #%d in block #%d does not exist",notenum,tracknum,blocknum);
    
    return ret;
  }
}

struct Notes *getNoteFromNum(int windownum,int blocknum,int tracknum,int notenum){
  struct Tracker_Windows *window;
  struct WBlocks *wblock;
  struct WTracks *wtrack;
  return getNoteFromNumA(windownum, &window, blocknum, &wblock, tracknum, &wtrack, notenum);
}

struct FXs *getFXsFromNumA(int windownum,struct Tracker_Windows **window, int blocknum, struct WBlocks **wblock, int tracknum, struct WTracks **wtrack, int fxnum){
  (*wtrack) = getWTrackFromNumA(windownum, window, blocknum, wblock, tracknum);
  if ((*wtrack)==NULL)
    return NULL;

  struct Tracks *track = (*wtrack)->track;
  struct FXs *ret = ListFindElement1_num(&track->fxs->l,fxnum);
  if (ret==NULL)
    RError("FX #%d in track #%d in block #%d does not exist",fxnum,tracknum,blocknum);
  
  return ret;
}

struct FXs *getFXsFromNum(int windownum,int blocknum,int tracknum,int fxnum){
  struct Tracker_Windows *window;
  struct WBlocks *wblock;
  struct WTracks *wtrack;
  return getFXsFromNumA(windownum, &window, blocknum, &wblock, tracknum, &wtrack, fxnum);
}


const static int num_instrument_types = 1;

int getInstrumentPatchNum(int instrument_num, int patch_num){
  return (patch_num*num_instrument_types) + instrument_num;
}

 static int getPatchNum(int instrument_num, int instrument_patch_num){
  return (instrument_patch_num - instrument_num) / num_instrument_types;
}

struct Instruments *getInstrumentFromNum(int instrument_patch_num){
  if(instrument_patch_num==-1)
    return get_default_instrument();

  int instrument_num = instrument_patch_num % num_instrument_types;

  if(instrument_num==0)
    return  get_MIDI_instrument();

#if 0
  if(instrument_num==1)
    return  get_audio_instrument();
#endif

  RError("Unknown instrument_num: %d", instrument_num);
  return get_default_instrument();
}

struct Patch *getPatchFromNum(int instrument_patch_num){
  int instrument_num = instrument_patch_num % num_instrument_types;
  int patch_num = getPatchNum(instrument_num, instrument_patch_num);

  struct Instruments *instrument = getInstrumentFromNum(instrument_num);

  struct Patch *patch = instrument->patches.elements[patch_num];
  if(patch==NULL)
    RError("Instrument #%d does not exist",instrument_patch_num);

  return patch;
}

