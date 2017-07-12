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
  if (line<0)
    return 0;
  
  int realline=0;
  do{
    if (realline==wblock->num_reallines-1)
      return realline;
    
    if (wblock->reallines[realline]->Tline >= line)
      return realline;
    
    realline++;
  }while(true);
  
  return realline;
}

int getCurrRealline(int blocknum, int windownum){
  struct Tracker_Windows *window;
  struct WBlocks *wblock = getWBlockFromNumA(windownum, &window, blocknum);
  if(wblock==NULL)
    return 0;

  return wblock->curr_realline;
}

void setCurrRealline(int realline, int blocknum, int windownum){
  struct Tracker_Windows *window;
  struct WBlocks *wblock = getWBlockFromNumA(windownum, &window, blocknum);
  if(wblock==NULL)
    return;

  ScrollEditorToRealLine(window, wblock, realline);
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
  const struct Beats *beat = wblock->block->beats;

  while (beat != NULL) {
    if (beat->bar_num == barnum && beat->beat_num==beatnum)
      break;
    beat = NextBeat(beat);
  }

  if (beat == NULL)
    return -1;

  return FindRealLineFor(wblock, 0, &beat->l.p);
}

static int string_charpos(char *s, char c){
  int pos=0;
  while(s[pos]!=0){
    if(s[pos]==c)
      return pos;
    pos++;
  }
  return -1;
}

void requestCursorMove(void){
  struct Tracker_Windows *window=getWindowFromNum(-1);
  struct WBlocks *wblock = window->wblock;

  ReqType reqtype = GFX_OpenReq(window, 50, 4, "");
  
  char *line = GFX_GetString(window,reqtype,"Move cursor (write \"h\" to get examples): >");
  //char *line = GFX_GetString(window,NULL,"'2' jumps to bar 2. '2/3' jumps to bar 2, beat 3. '2/3,4' jumps to bar 2, beat 3, track 4: >");
  if (line==NULL)
    goto exit;

  if (line[0]=='h'){
    GFX_WriteString(reqtype, " To move cursor to bar 2, write \"2\"\n");
    GFX_WriteString(reqtype, " To move cursor to bar 2, beat 3, write \"2/3\"\n");
    GFX_WriteString(reqtype, " To move cursor to bar 2, beat 3, track 4, write \"2/3,4\"\n");
    GFX_WriteString(reqtype, "\n");
    GFX_WriteString(reqtype, " To move cursor to track 4, write \",4\"\n");
    GFX_WriteString(reqtype, "\n");
    GFX_WriteString(reqtype, " To move cursor to line 5, write \"l5\"\n");
    GFX_WriteString(reqtype, " To move cursor to line 5, track 6, write \"l5,6\"\n");
    line = GFX_GetString(window,reqtype,">");
    if (line==NULL)
      goto exit;
  }
  
  int len = (int)strlen(line);
  if (len==0)
    goto exit;
  
  int split_pos = string_charpos(line,',');
  //printf("split_pos: %d, len: %d, string: -%s-\n",split_pos, len, line);
  
  if (split_pos != -1){
    if (split_pos>=len-1)
      goto exit;
    
    char *trackstring = &line[split_pos+1];
    line[split_pos] = 0;
    
    int tracknum = atoi(trackstring);
    if (tracknum >= 0)
      SetCursorPosConcrete_CurrPos(window,(NInt)tracknum);
  }

  if (line[0]=='l'){

    if(len>1){
      int linenum = atoi(&line[1]);
      if (linenum >= 0)
        ScrollEditorToRealLine(window, wblock, get_realline_from_line(wblock, linenum));
    }
    
  } else {
  
    Ratio ratio = RATIO_from_string(STRING_create(line));
    if (ratio.denominator==0)
      goto exit;
    
    int realline = get_realline_from_beat(wblock, (int)ratio.numerator, (int)ratio.denominator);
    if (realline==-1)
      goto exit;

    ScrollEditorToRealLine(window, wblock, realline);
  }
  
 exit:
  GFX_CloseReq(window, reqtype);
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

int currentLine(int blocknum, int windownum){
  struct Tracker_Windows *window;
  struct WBlocks *wblock = getWBlockFromNumA(windownum, &window, blocknum);
  if(wblock==NULL)
    return 0;
  else
    return wblock->curr_realline;
}

void setCurrentLine(int linenum, int windownum){
  struct Tracker_Windows *window=getWindowFromNum(windownum);
  if(window==NULL) return;

  ScrollEditorToRealLine(window, window->wblock, linenum);
}
