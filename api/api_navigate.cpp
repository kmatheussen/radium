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
#include "../common/notes_proc.h"

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

	ScrollEditorDown(window,numlines * getScrollMultiplication(), -1);
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

  if (blocknum!=-1 && blocknum != window->wblock->l.num)
    return;
  
  ScrollEditorToRealLine(window, /* wblock, */ realline);
}

void cursorUserInputLine(void){
  struct Tracker_Windows *window=getWindowFromNum(-1);
  struct WBlocks *wblock = window->wblock;
  struct Blocks *block = wblock->block;
  
  int line = GFX_GetInteger(window,NULL,"Jump to line: >",0,block->num_lines-1,true);
  if (line==-1)
    return;

  ScrollEditorToRealLine(window, /* wblock, */ get_realline_from_line(wblock, line));
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

static int string_charpos(const char *s, char c){
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

  int len, split_pos;
  
  const char *line = GFX_GetString(window,reqtype,"Move cursor (write \"h\" to get examples): >",true);
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
    line = GFX_GetString(window,reqtype,">",true);
    if (line==NULL)
      goto exit;
  }

  len = (int)strlen(line);
  if (len==0)
    goto exit;
  
  split_pos = string_charpos(line,',');
  //printf("split_pos: %d, len: %d, string: -%s-\n",split_pos, len, line);
  
  if (split_pos != -1){
    if (split_pos>=len-1)
      goto exit;
    
    const char *trackstring = &line[split_pos+1];
    {
      char *line2 = talloc_strdup(line);
      line2[split_pos] = 0;
      line = line2;
    }
    
    int tracknum = atoi(trackstring);
    if (tracknum >= 0)
      setCurrentTrack(tracknum, -2, window->l.num);
  }

  if (strlen(line) > 0) {
    if (line[0]=='l'){
      
      if(len>1){
        int linenum = atoi(&line[1]);
        if (linenum >= 0)
          ScrollEditorToRealLine(window, /* wblock, */ get_realline_from_line(wblock, linenum));
      }
      
    } else {
      
      StaticRatio ratio = STATIC_RATIO_from_string(STRING_create(line));
      if (ratio.denominator==0)
        goto exit;
      
      int realline = get_realline_from_beat(wblock, (int)ratio.numerator, (int)ratio.denominator);
      if (realline==-1)
        goto exit;
      
      ScrollEditorToRealLine(window, /* wblock, */ realline);
    }
  }
  
 exit:
  GFX_CloseReq(window, reqtype);
}

void selectNextBlock(int windownum){
	struct Tracker_Windows *window=getWindowFromNum(windownum);
	if(window==NULL) return;

        selectBlock(R_MIN(getNumBlocks()-1, currentBlock(windownum)+1), windownum, false);
	//SelectNextWBlock(window);
}

void selectPrevBlock(int windownum){
	struct Tracker_Windows *window=getWindowFromNum(windownum);
	if(window==NULL) return;

        //EVENTLOG_add_event("selectPrevBlock");        
	//SelectPrevWBlock(window);
        selectBlock(R_MAX(0, currentBlock(windownum)-1), windownum, false);
}

void selectNextPlaylistBlock(void){
  SelectNextPlaylistWBlock(root->song->tracker_windows, true);
}

void selectPrevPlaylistBlock(void){
  SelectPrevPlaylistWBlock(root->song->tracker_windows, true);
}

void selectTrack(int tracknum,int windownum){
  setCurrentTrack(tracknum, -2, windownum);
}

void setCurrentTrack(int tracknum, int subtrack, int windownum){
  struct Tracker_Windows *window=getWindowFromNum(windownum);
  if(window==NULL) return;

  struct WBlocks *wblock = window->wblock;
  //struct Blocks *block = wblock->block;
  

  // fix default subtrack value
  if (subtrack==-2) {
    int current_track = getCurrentTrack(windownum);
    
    if (tracknum < 0) {

      if (current_track==tracknum)
        return;
      else
        subtrack = 0;
      
    } else {
      int current_subtrack = getCurrentSubtrack(windownum);
      int num_tracks = getNumTracks(wblock->l.num);
      if (tracknum >= num_tracks){
        handleError("setCurrentTrack: Illegal track #%d in current block", tracknum);
        return;
      }
      int num_subtracks = getNumSubtracks(tracknum, wblock->l.num, windownum);
      if (current_subtrack >= num_subtracks)
        subtrack = num_subtracks - 1;
      else
        subtrack = current_subtrack;
    }      
  }
  
  //printf("setCurrentTrack: %d / %d. Can move: %d\n", tracknum, subtrack, canCursorMoveToTrack(tracknum, subtrack, -1, windownum));
  
  if (!canCursorMoveToTrack(tracknum, subtrack, -1, windownum)){
    handleError("setCurrentTrack: Illegal track #%d, subtrack #%d in current block", tracknum, subtrack);
    return;
  }

  int prevcurrtrack = window->curr_track;

  ATOMIC_WRITE(window->curr_track, tracknum);

  /*
  curr_track_sub er alltid 0 når tracknun < 0.
  curr_othertrack_sub er alltid 2 når tracknum >= 0.
  */

  if (tracknum < 0) {
    window->curr_track_sub = 0; // In the old system, this value was always 0 when tracknum<0. Probably makes no difference though.
    window->curr_othertrack_sub = subtrack;
  } else {
    window->curr_track_sub = subtrack;
    window->curr_othertrack_sub = 2; // In the old system, this value was always 2 when tracknum>=0. Probably makes no difference though.
  }
  
  struct WTracks *wtrack = tracknum < 0 ? wblock->wtracks : getWTrackFromNum(windownum, wblock->l.num, tracknum);
  if (wtrack==NULL){
    R_ASSERT(false);
  } else {
    if (wblock->wtrack != wtrack)
      ATOMIC_WRITE(wblock->wtrack, wtrack);
  }

  GFX_adjust_skew_x(window, wblock, prevcurrtrack);
  GFX_update_instrument_patch_gui(wtrack->track->patch);
  GFX_show_curr_track_in_statusbar(window, wblock);
  
  window->must_redraw = true;
}

int getCurrentTrack(int windownum){
  struct Tracker_Windows *window=getWindowFromNum(windownum);
  if(window==NULL)
    return 0;

  return window->curr_track;
}

int getCurrentSubtrack(int windownum){
  struct Tracker_Windows *window=getWindowFromNum(windownum);
  if(window==NULL)
    return 0;

  int tracknum = getCurrentTrack(windownum);
  
  int subtracknum;
  
  if (window->curr_track < 0)
    subtracknum = window->curr_othertrack_sub;
  else
    subtracknum = window->curr_track_sub;

  int num_subtracks = getNumSubtracks(tracknum, -1, windownum);

  if (tracknum >= 0)
    return R_BOUNDARIES(-1, subtracknum, num_subtracks-1);
  else
    return R_BOUNDARIES(0, subtracknum, num_subtracks-1);
}

int getNumSubtracks(int tracknum, int blocknum, int windownum){
  switch(tracknum){
    case TEMPOCOLORTRACK: return 0;
    case TEMPOTRACK: return 4;
    case LPBTRACK: return 2;
    case SIGNATURETRACK: return 1;
    case LINENUMBTRACK: return 0;
    case SWINGTRACK: return 3;
    case TEMPONODETRACK: return 1;
    default:
      break;
  }
  
  struct Tracker_Windows *window;
  struct WBlocks *wblock;
  struct WTracks *wtrack = getWTrackFromNumA(windownum, &window, blocknum, &wblock, tracknum);
  if (wtrack==NULL)
    return 1;

  return GetNumSubtracks(wtrack);
}

bool getTrackVisible(int tracknum, int blocknum, int windownum){
  if (tracknum >= 0)
    return true;
  
  struct Tracker_Windows *window=getWindowFromNum(windownum);
  if(window==NULL) return false;
        
  switch(tracknum){
    case TEMPOCOLORTRACK: return true;
    case TEMPOTRACK: return window->show_bpm_track;
    case LPBTRACK: return window->show_lpb_track;
    case SIGNATURETRACK: return window->show_signature_track;
    case LINENUMBTRACK: return true;
    case SWINGTRACK: return window->show_swing_track;
    case TEMPONODETRACK: return window->show_reltempo_track;
    default:
      return true;
  }
}

bool canCursorMoveToTrack(int tracknum, int subtrack, int blocknum, int windownum){
  if (subtrack==-1 && tracknum < 0) // fix default subtrack value
    subtrack = 0;
  
  if (tracknum < LEFTMOSTTRACK)
    return false;
  
  struct Tracker_Windows *window;
  struct WBlocks *wblock = getWBlockFromNumA(windownum, &window, blocknum);
  if(wblock==NULL)
    return false;

  if (tracknum >= wblock->block->num_tracks)
    return false;
  
  int num_subtracks = getNumSubtracks(tracknum, blocknum, windownum);

  if (subtrack < 0){
    if (subtrack < -1)
      return false;
    
    if (tracknum < 0)
      return false;
  }

  if (subtrack >= num_subtracks)
    return false;
  
  if (tracknum >= 0)
    return true;

  if (getTrackVisible(tracknum, blocknum, windownum)==false)
    return false;

  switch(tracknum){
    case TEMPOCOLORTRACK: return false;
    case LINENUMBTRACK: return false;
    default:
      return true;
  }
}

int getLeftmostCursorTrack(int blocknum, int windownum){
  return TEMPOTRACK;
}

/*
int getSubtracknum(const_char* subtracktype, int subtracktypenum, int tracknum, int blocknum, int windownum){
}

const_char* getSubtrackType(int subtracknum, int tracknum, int blocknum, int windownum){
}

int getSubtrackTypeNum(int subtracknum, int tracknum, int blocknum, int windownum){
}
*/


static int get_next_legal_track(int tracknum, int num_tracks, int windownum){
  for(;;) {
    tracknum++;
  
    if (tracknum>=num_tracks)
      return NOTRACK;
    
    if (canCursorMoveToTrack(tracknum, -1, -1, windownum))
      return tracknum;
  }
}


void cursorRight(int windownum){
	struct Tracker_Windows *window=getWindowFromNum(windownum);
	if(window==NULL) return;

        int tracknum = getCurrentTrack(windownum);
        int subtracknum = getCurrentSubtrack(windownum);

        //printf("right. tracknum: %d. subtracknum: %d\n", tracknum, subtracknum);
        
        int num_tracks = getNumTracks(-1);
        int num_subtracks = getNumSubtracks(tracknum, -1, windownum);

        if (tracknum >= 0 && swingtextVisible(tracknum, -1, windownum)) {

          if (subtracknum==-1)
            subtracknum = 3;
          else if (subtracknum==2)
            subtracknum = -1;
          else
            subtracknum++;
            
        } else {
          
          subtracknum++;
          
        }

        if (subtracknum==num_subtracks){
          
          tracknum = get_next_legal_track(tracknum, num_tracks, windownum);
            
          if (tracknum==NOTRACK)
            return;
          
          if (tracknum < 0 || swingtextVisible(tracknum, -1, windownum))
            subtracknum = 0;
          else
            subtracknum = -1;
        }
        
        if (!canCursorMoveToTrack(tracknum, subtracknum, -1, windownum))
          return;

        setCurrentTrack(tracknum, subtracknum, windownum);
}

void cursorNextTrack(int windownum){
	struct Tracker_Windows *window=getWindowFromNum(windownum);
	if(window==NULL) return;

        int tracknum = getCurrentTrack(windownum);
        int org_tracknum = tracknum;
        int subtracknum = getCurrentSubtrack(windownum);

        int num_tracks = getNumTracks(-1);
        
        //printf("next track. tracknum: %d. subtracknum: %d\n", tracknum, subtracknum);

        tracknum = get_next_legal_track(tracknum, num_tracks, windownum);
        
        if (tracknum==NOTRACK)
          tracknum = get_next_legal_track(LEFTMOSTTRACK, num_tracks, windownum);

        R_ASSERT_RETURN_IF_FALSE(tracknum != NOTRACK);
          
        if (org_tracknum < 0 || tracknum < 0) {

          if (tracknum >= 0)
            subtracknum = -1;
          else
            subtracknum = 0;              

        } else {

          bool org_track_has_swing = swingtextVisible(org_tracknum, -1, windownum);
          bool new_track_has_swing = swingtextVisible(tracknum, -1, windownum);
            
          if (org_track_has_swing && !new_track_has_swing && subtracknum <= 2) {
            
            subtracknum = -1;

          } else if (new_track_has_swing && !org_track_has_swing && subtracknum <= 2 && (subtracknum != -1)) {
            
            subtracknum += 3;
            
          }

          int num_subtracks = getNumSubtracks(tracknum, -1, windownum);
            
          if (subtracknum >= num_subtracks)
            subtracknum = num_subtracks -1;
        }
        
        setCurrentTrack(tracknum, subtracknum, windownum);
}

static int get_previous_legal_track(int tracknum, int windownum){
  for(;;) {
    tracknum--;
  
    if (tracknum<=LEFTMOSTTRACK)
      return NOTRACK;
    
    if (canCursorMoveToTrack(tracknum, -1, -1, windownum))
      return tracknum;
  }
}

void cursorLeft(int windownum){
	struct Tracker_Windows *window=getWindowFromNum(windownum);
	if(window==NULL) return;

        int tracknum = getCurrentTrack(windownum);
        int subtracknum = getCurrentSubtrack(windownum);

        //printf("cursorLeft. tracknum: %d. subtracknum: %d\n", tracknum, subtracknum);
        
        //int num_tracks = getNumTracks(-1);

        bool move_prev_track = false;
        
        if (tracknum >= 0 && swingtextVisible(tracknum, -1, windownum)) {
          
          if (subtracknum==-1)
            subtracknum = 2;
          else if (subtracknum==3)
            subtracknum = -1;
          else if (subtracknum==0){
            subtracknum = -1;
            move_prev_track = true;
          }
          else
            subtracknum--;
            
        } else {
          
          subtracknum--;
          
        }

        if (!move_prev_track) {
          
          if (tracknum >= 0) {
            
            if (!swingtextVisible(tracknum, -1, windownum)) {
              
              if (subtracknum < -1){
                R_ASSERT_NON_RELEASE(subtracknum==-2);
                move_prev_track = true;
              }
            }
            
          } else {
            
            if (subtracknum < 0){
              R_ASSERT_NON_RELEASE(subtracknum==-1);
              move_prev_track = true;
            }
            
          }

        }

        
        if (move_prev_track) {
          
          tracknum = get_previous_legal_track(tracknum, windownum);

          if (tracknum==NOTRACK)
            return;
          
          if (!canCursorMoveToTrack(tracknum, -1, -1, windownum))
            return;

          int num_subtracks = getNumSubtracks(tracknum, -1, windownum);

          subtracknum = num_subtracks -1;
        }
        
        if (!canCursorMoveToTrack(tracknum, subtracknum, -1, windownum))
          return;

        setCurrentTrack(tracknum, subtracknum, windownum);
}

void cursorPrevTrack(int windownum){
	struct Tracker_Windows *window=getWindowFromNum(windownum);
	if(window==NULL) return;

        int tracknum = getCurrentTrack(windownum);
        int org_tracknum = tracknum;
        int subtracknum = getCurrentSubtrack(windownum);

        int num_tracks = getNumTracks(-1);
        
        //printf("prev track. tracknum: %d. subtracknum: %d\n", tracknum, subtracknum);

        tracknum = get_previous_legal_track(tracknum, windownum);
        if (tracknum==NOTRACK)
          tracknum = num_tracks -1;

        if (org_tracknum < 0 || tracknum < 0) {

          if (tracknum >= 0)
            subtracknum = -1;
          else
            subtracknum = 0;              

        } else {

          bool org_track_has_swing = swingtextVisible(org_tracknum, -1, windownum);
          bool new_track_has_swing = swingtextVisible(tracknum, -1, windownum);
            
          if (org_track_has_swing && !new_track_has_swing && subtracknum <= 2) {
            
            subtracknum = -1;

          } else if (new_track_has_swing && !org_track_has_swing && subtracknum <= 2 && (subtracknum != -1)) {
            
            subtracknum += 3;
            
          }

          int num_subtracks = getNumSubtracks(tracknum, -1, windownum);
            
          if (subtracknum >= num_subtracks)
            subtracknum = num_subtracks -1;
        }
        
        setCurrentTrack(tracknum, subtracknum, windownum);
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

  ScrollEditorToRealLine(window, /* window->wblock, */ linenum);
}
