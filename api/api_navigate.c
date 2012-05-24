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
#include "../common/cursor_proc.h"
#include "../common/cursor_updown_proc.h"
#include "../common/wblocks_proc.h"


#include "api_common_proc.h"


extern struct Root *root;



/*******************************************
  Navigating 
*******************************************/

void cursorDown(int numlines,int windownum){
	struct Tracker_Windows *window=getWindowFromNum(windownum);
	if(window==NULL) return;

	ScrollEditorDown(window,numlines);
}

void cursorUp(int numlines,int windownum){
	struct Tracker_Windows *window=getWindowFromNum(windownum);
	if(window==NULL) return;

	ScrollEditorUp(window,numlines);
}

void cursorNextNote(int windownum){
	struct Tracker_Windows *window=getWindowFromNum(windownum);
	if(window==NULL) return;

	ScrollEditorNextNote(window);
}

void cursorPrevNote(int windownum){
	struct Tracker_Windows *window=getWindowFromNum(windownum);
	if(window==NULL) return;

	ScrollEditorPrevNote(window);
}

void cursorPercentLine(int percent,int windownum){
	struct Tracker_Windows *window=getWindowFromNum(windownum);
	if(window==NULL) return;

	if(percent<0 || percent>99) return;

	ScrollEditorToPercentLine_CurrPos(window,percent);
}

void selectNextBlock(int windownum){
	struct Tracker_Windows *window=getWindowFromNum(windownum);
	if(window==NULL) return;

	SelectNextWBlock(window);
}

void selectPrevBlock(int windownum){
	struct Tracker_Windows *window=getWindowFromNum(windownum);
	if(window==NULL) return;

	SelectPrevWBlock(window);
}

void selectNextPlaylistBlock(void){
	SelectNextPlaylistWBlock(root->song->tracker_windows);
}

void selectPrevPlaylistBlock(void){
	SelectPrevPlaylistWBlock(root->song->tracker_windows);
}

void selectTrack(int tracknum,int windownum){
	struct Tracker_Windows *window=getWindowFromNum(windownum);
	if(window==NULL) return;

	SetCursorPosConcrete_CurrPos(window,(NInt)tracknum);
}

void cursorRight(int windownum){
	struct Tracker_Windows *window=getWindowFromNum(windownum);
	if(window==NULL) return;

	CursorRight_CurrPos(window);
}

void cursorNextTrack(int windownum){
	struct Tracker_Windows *window=getWindowFromNum(windownum);
	if(window==NULL) return;

	CursorNextTrack_CurrPos(window);
}

void cursorLeft(int windownum){
	struct Tracker_Windows *window=getWindowFromNum(windownum);
	if(window==NULL) return;

	CursorLeft_CurrPos(window);
}

void cursorPrevTrack(int windownum){
	struct Tracker_Windows *window=getWindowFromNum(windownum);
	if(window==NULL) return;

	CursorPrevTrack_CurrPos(window);
}


