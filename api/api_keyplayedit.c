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

#include "Python.h"

#include "../common/nsmtracker.h"
#include "../common/list_proc.h"
#include "../common/placement_proc.h"
#include "../common/patch_proc.h"
#include "../common/notes_proc.h"
#include "../common/gfx_upperleft_proc.h"
#include "../common/visual_proc.h"

#include "api_common_proc.h"


extern struct Root *root;



/*******************************************
  Computer Keyboard Note Playing/Editing 
*******************************************/

void keyDownPlay(int notenum,int windownum){
	struct Tracker_Windows *window=getWindowFromNum(windownum);

	notenum+=root->keyoct;

	if(notenum<=0 || notenum>127) return;
	if(window==NULL || window->curr_track<0) return;

	PATCH_playNoteCurrPos(window,notenum);
        if(root->editonoff)
          InsertNoteCurrPos(window,notenum,0);
}

void polyKeyDownPlay(int notenum,int windownum){
	struct Tracker_Windows *window=getWindowFromNum(windownum);

	notenum+=root->keyoct;

	if(notenum<=0 || notenum>127) return;
	if(window==NULL || window->curr_track<0) return;

	PATCH_playNoteCurrPos(window,notenum);
        if(root->editonoff)
          InsertNoteCurrPos(window,notenum,1);
}

void keyUpPlay(int notenum,int windownum){
	struct Tracker_Windows *window=getWindowFromNum(windownum);

	notenum+=root->keyoct;

	if(notenum<=0 || notenum>127) return;
	if(window==NULL || window->curr_track<0) return;

        PATCH_stopNoteCurrPos(window,notenum);
}

void setKeyAdd(int addnum){
#if 0
  if(root->keyoct==addnum)
    root->keyoct=addnum+12;
  else if(root->keyoct==addnum+12 && addnum==72)
    root->keyoct=addnum+24;
  else if(root->keyoct==addnum+24 && addnum==72)
    root->keyoct=addnum+36;
  else if(root->keyoct==addnum+36 && addnum==72)
    root->keyoct=addnum+48;
  else
#endif
    root->keyoct=addnum;

  GFX_UpdateKeyOctave(root->song->tracker_windows,root->song->tracker_windows->wblock);
}

void incKeyAdd(int incaddnum){
	root->keyoct+=incaddnum;
	if(root->keyoct>127 || root->keyoct<0){
		root->keyoct-=incaddnum;
	}else{
		GFX_UpdateKeyOctave(root->song->tracker_windows,root->song->tracker_windows->wblock);
	}
}

void decKeyAdd(int decaddnum){
	incKeyAdd(-decaddnum);
}

void switchEditOnOff(void){
	struct Tracker_Windows *window=getWindowFromNum(-1);
	root->editonoff=root->editonoff?false:true;
        char temp[1000];
        sprintf(temp,"Midi Input %s",root->editonoff?"On":"Off");
        GFX_SetStatusBar(window,temp);
}

void switchScrollPlayOnOff(void){
	root->scrollplayonoff=root->scrollplayonoff?false:true;
}

void switchSoundScrollOnOff(int windownum){
	struct Tracker_Windows *window=getWindowFromNum(windownum);
	if(window==NULL) return;

	window->playalong=window->playalong?false:true;
}

