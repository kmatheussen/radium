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
#include "../common/wtracks_proc.h"
#include "../common/OS_visual_input.h"
#include "../common/realline_calc_proc.h"
#include "../Qt/Rational.h"

#include "api_common_proc.h"


extern struct TEvent tevent;


/*******************************************
  Navigating 
*******************************************/

void cursorDown(int numlines,int windownum){
  //  printf("cursorDOWN: doautorepeat: %d, autorepeat: %d\n", doAutoRepeat(), AutoRepeat(tevent.keyswitch));
        
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

void cursorNextNote(int tracknum, int blocknum, int windownum){
  struct Tracker_Windows *window;
  struct WBlocks *wblock;
  struct WTracks *wtrack = getWTrackFromNumA(windownum, &window, blocknum, &wblock, tracknum);
  if (wtrack==NULL)
    return;

  window->must_redraw = false;

  ScrollEditorNextNote(window, wblock, wtrack);
}

void cursorPrevNote(int tracknum, int blocknum, int windownum){
  struct Tracker_Windows *window;
  struct WBlocks *wblock;
  struct WTracks *wtrack = getWTrackFromNumA(windownum, &window, blocknum, &wblock, tracknum);
  if (wtrack==NULL)
    return;

  window->must_redraw = false;

  ScrollEditorPrevNote(window, wblock, wtrack);
}

void cursorNextWaveform(int polyphony_num, int tracknum, int blocknum, int windownum){
  struct Tracker_Windows *window;
  struct WBlocks *wblock;
  struct WTracks *wtrack = getWTrackFromNumA(windownum, &window, blocknum, &wblock, tracknum);
  if (wtrack==NULL)
    return;

  if (polyphony_num==-1)
    polyphony_num = R_MAX(0, window->curr_track_sub - WTRACK_num_non_polyphonic_subtracks(wtrack));
      
  window->must_redraw = false;

  ScrollEditorNextWaveform(window, wblock, wtrack, polyphony_num);
}

void cursorPrevWaveform(int polyphony_num, int tracknum, int blocknum, int windownum){
  struct Tracker_Windows *window;
  struct WBlocks *wblock;
  struct WTracks *wtrack = getWTrackFromNumA(windownum, &window, blocknum, &wblock, tracknum);
  if (wtrack==NULL)
    return;

  if (polyphony_num==-1)
    polyphony_num = R_MAX(0, window->curr_track_sub - WTRACK_num_non_polyphonic_subtracks(wtrack));
      
  window->must_redraw = false;

  ScrollEditorPrevWaveform(window, wblock, wtrack, polyphony_num);
}

void cursorNextVelocity(int tracknum, int blocknum, int windownum){
  struct Tracker_Windows *window;
  struct WBlocks *wblock;
  struct WTracks *wtrack = getWTrackFromNumA(windownum, &window, blocknum, &wblock, tracknum);
  if (wtrack==NULL)
    return;

  window->must_redraw = false;

  ScrollEditorNextVelocity(window, wblock, wtrack);
}

void cursorPrevVelocity(int tracknum, int blocknum, int windownum){
  struct Tracker_Windows *window;
  struct WBlocks *wblock;
  struct WTracks *wtrack = getWTrackFromNumA(windownum, &window, blocknum, &wblock, tracknum);
  if (wtrack==NULL)
    return;

  window->must_redraw = false;

  ScrollEditorPrevVelocity(window, wblock, wtrack);
}

void cursorNextFx(int fxnum, int tracknum, int blocknum, int windownum){
  struct Tracker_Windows *window;
  struct WBlocks *wblock;
  struct WTracks *wtrack;
  struct FXs *fxs = getFXsFromNumA(windownum, &window, blocknum, &wblock, tracknum, &wtrack, fxnum);
  if (fxs==NULL)
    return;

  window->must_redraw = false;

  ScrollEditorNextFx(window, wblock, wtrack, fxs);
}

void cursorPrevFx(int fxnum, int tracknum, int blocknum, int windownum){
  struct Tracker_Windows *window;
  struct WBlocks *wblock;
  struct WTracks *wtrack;
  struct FXs *fxs = getFXsFromNumA(windownum, &window, blocknum, &wblock, tracknum, &wtrack, fxnum);
  if (fxs==NULL)
    return;

  window->must_redraw = false;

  ScrollEditorPrevFx(window, wblock, wtrack, fxs);
}

void cursorNextSomething(int tracknum, int blocknum, int windownum){
  struct Tracker_Windows *window;
  struct WBlocks *wblock;
  struct WTracks *wtrack = getWTrackFromNumA(windownum, &window, blocknum, &wblock, tracknum);
  if (wtrack==NULL)
    return;

  window->must_redraw = false;

  ScrollEditorNextSomething(window, wblock, wtrack);
}

void cursorPrevSomething(int tracknum, int blocknum, int windownum){
  struct Tracker_Windows *window;
  struct WBlocks *wblock;
  struct WTracks *wtrack = getWTrackFromNumA(windownum, &window, blocknum, &wblock, tracknum);
  if (wtrack==NULL)
    return;

  window->must_redraw = false;

  ScrollEditorPrevSomething(window, wblock, wtrack);
}

void cursorPercentLine(int percent,int windownum){
	struct Tracker_Windows *window=getWindowFromNum(windownum);
	if(window==NULL) return;

	if(percent<0 || percent>100) return;

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

int currentBlock(int windownum){
  struct Tracker_Windows *window=getWindowFromNum(windownum);
  if(window==NULL)
    return 0;
  else
    return window->wblock->l.num;
}

int currentTrack(int blocknum, int windownum){
  if (blocknum==-1){
    
    struct Tracker_Windows *window=getWindowFromNum(windownum);
    if(window==NULL)
      return 0;
    else
      return window->curr_track;
    
  } else {
    
    struct Tracker_Windows *window;
    struct WBlocks *wblock = getWBlockFromNumA(windownum, &window, blocknum);
    if(wblock==NULL)
      return 0;
    else
      return wblock->wtrack->l.num;
  }
}

int currentLine(int windownum){
  struct Tracker_Windows *window=getWindowFromNum(windownum);
  if(window==NULL) return 0;

  return window->wblock->curr_realline;
}

void setCurrentLine(int linenum, int windownum){
  struct Tracker_Windows *window=getWindowFromNum(windownum);
  if(window==NULL) return;

  ScrollEditorToRealLine(window, window->wblock, linenum);
}
