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
#include "../common/OS_visual_input.h"
#include "../common/realline_calc_proc.h"
#include "../Qt/Rational.h"

#include "api_common_proc.h"


extern struct Root *root;

extern int g_downscroll;
static int getScrollMultiplication(void){
  if (doScrollEditLines())
      return g_downscroll;
  else
    return 1;
}

/*******************************************
  Navigating 
*******************************************/

void cursorDown(int numlines,int windownum){
	struct Tracker_Windows *window=getWindowFromNum(windownum);
	if(window==NULL) return;

        window->must_redraw = false;

	ScrollEditorDown(window,numlines * getScrollMultiplication());
}

void cursorUp(int numlines,int windownum){
	struct Tracker_Windows *window=getWindowFromNum(windownum);
	if(window==NULL) return;

        window->must_redraw = false;

	ScrollEditorUp(window,numlines * getScrollMultiplication());
}

void cursorNextNote(int windownum){
	struct Tracker_Windows *window=getWindowFromNum(windownum);
	if(window==NULL) return;

        window->must_redraw = false;

	ScrollEditorNextNote(window);
}

void cursorPrevNote(int windownum){
	struct Tracker_Windows *window=getWindowFromNum(windownum);
	if(window==NULL) return;

        window->must_redraw = false;

	ScrollEditorPrevNote(window);
}

void cursorPercentLine(int percent,int windownum){
	struct Tracker_Windows *window=getWindowFromNum(windownum);
	if(window==NULL) return;

	if(percent<0 || percent>99) return;

        window->must_redraw = false;

	ScrollEditorToPercentLine_CurrPos(window,percent);
}

static int get_realline_from_line(struct WBlocks *wblock, int line){
  int realline=0;
  while(wblock->reallines[realline]->Tline!=line)
    realline++;
  return realline;
}

void cursorUserInputLine(void){
  struct Tracker_Windows *window=getWindowFromNum(-1);
  struct WBlocks *wblock = window->wblock;
  struct Blocks *block = wblock->block;
  
  int line = GFX_GetInteger(window,NULL,"Jump to line: >",0,block->num_lines-1);
  if (line==-1)
    return;

  ScrollEditorToRealLine(window, wblock, get_realline_from_line(wblock, line));
}

static int get_realline_from_beat(struct WBlocks *wblock, int barnum, int beatnum){
  struct Beats *beat = wblock->block->beats;

  while (beat != NULL) {
    if (beat->bar_num == barnum && beat->beat_num==beatnum)
      break;
    beat = NextBeat(beat);
  }

  if (beat == NULL)
    return -1;

  return FindRealLineFor(wblock, 0, &beat->l.p);
}

void cursorUserInputBeat(void){
  struct Tracker_Windows *window=getWindowFromNum(-1);
  struct WBlocks *wblock = window->wblock;
  
  char *line = GFX_GetString(window,NULL,"Jump to Bar/Beat (e.g. \"2/1\" or just \"2\"): >");
  if (line==NULL)
    return;

  Place p = get_rational_from_string(line);
  if (p.dividor==0)
    return;

  int realline = get_realline_from_beat(wblock, p.counter, p.dividor);
  if (realline==-1)
    return;

  ScrollEditorToRealLine(window, wblock, realline);
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


