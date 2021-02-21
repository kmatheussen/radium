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




#include "nsmtracker.h"
#include "visual_proc.h"
#include "common_proc.h"
#include "gfx_subtrack_proc.h"
#include "list_proc.h"
#include "windows_proc.h"
#include "wtracks_proc.h"
#include "sliders_proc.h"
#include "wblocks_proc.h"
#include "../midi/midi_i_input_proc.h"
#include "notes_proc.h"
#include "veltext_proc.h"
#include "fxtext_proc.h"
#include "centtext_proc.h"
#include "chancetext_proc.h"
#include "bpmtext_proc.h"
#include "lpbtext_proc.h"
#include "swingtext_proc.h"
#include "signaturetext_proc.h"

#include "../api/api_proc.h"

#include "cursor_proc.h"


bool EDITOR_is_legal_track(const struct Tracker_Windows *window, int tracknum){
  if (tracknum==TEMPONODETRACK && window->show_reltempo_track==false)
    return false;
  
  if (tracknum==SWINGTRACK && window->show_swing_track==false)
    return false;
  
  if (tracknum==LINENUMBTRACK)
    return false;
  
  if (tracknum==SIGNATURETRACK && window->show_signature_track==false)
    return false;
  
  if (tracknum==LPBTRACK && window->show_lpb_track==false)
    return false;
  
  if (tracknum==TEMPOTRACK && window->show_bpm_track==false)
    return false;

  if (tracknum < TEMPOTRACK)
    return false;
  
  return true;
}

static void make_cursor_visible(struct Tracker_Windows *window,struct WBlocks *wblock,int prevcursorpos){

  if (window->curr_track < 0){
    if (prevcursorpos>=0)
      wblock->skew_x = 0;
    return;
  }

  NInt track    = window->curr_track;
  int  subtrack = window->curr_track_sub;

  int track_x1 = wblock->wtrack->x;
  int track_x2 = wblock->wtrack->x2;
  int track_width = track_x2 - track_x1;

  int subtrack_x1 = GetXSubTrack_B1(wblock,track,subtrack)-1;
  int subtrack_x2 = GetXSubTrack_B2(wblock,track,subtrack) + 10; // I don't know why we have to add 10 here. The number does not seem to be related to font size, left slider width, or number of tracks.

  
  int x1;

  if (track_width >= (wblock->t.x2 - wblock->t.x1))
    x1 = subtrack_x1;
  else
    x1 = track_x1;
      
  //printf("x1: %d, wblock->t.x1: %d\n", x1, wblock->t.x1);

  if (x1 < wblock->t.x1){
    int bef = wblock->skew_x;
    wblock->skew_x -= x1 - wblock->t.x1;
    if (0)
      printf("Skewing left: %d -> %d\n", bef, wblock->skew_x);
    return;
  }

  
  int x2;
  
  if (track_width >= (wblock->t.x2 - wblock->t.x1))
    x2 = subtrack_x2;
  else
    x2 = track_x2;

  if (x2 > wblock->t.x2){
    int bef = wblock->skew_x;
    wblock->skew_x -= x2 - wblock->t.x2;
    struct WTracks *wtrack = ListFindElement1(&wblock->wtracks->l,track);
    if (0)
      printf("Skewing right: %d -> %d. x2: %d. wblock->t.x: %d -> %d. wtrack->notearea.x2: %d, track: %d. subtrack: %d\n",
             bef, wblock->skew_x,
             x2, wblock->t.x1, wblock->t.x2, wtrack->notearea.x2,
             track, subtrack);
    return;
  }
}

void GFX_adjust_skew_x(struct Tracker_Windows *window,struct WBlocks *wblock, int prevcursorpos){
  make_cursor_visible(window, wblock, prevcursorpos);
}


void GFX_show_curr_track_in_statusbar(struct Tracker_Windows *window,struct WBlocks *wblock){
  struct WTracks *wtrack = wblock->wtrack;
  const char *message = NULL;
  struct FXs *fxs;

#define PRE "" //Moved cursor to "

  if (VELTEXT_subsubtrack(window, wtrack) >= 0){
    message = talloc_format(PRE "Velocity text, track #%d", wtrack->l.num);;

  } else if (FXTEXT_subsubtrack(window, wtrack, &fxs) >= 0){
    message = talloc_format(PRE "\"%s\", track #%d", fxs->fx->name, wtrack->l.num);;

  } else if (CHANCETEXT_subsubtrack(window, wtrack) >= 0){
    message = talloc_format(PRE "Chance text, track #%d", wtrack->l.num);;

  } else if (CENTTEXT_subsubtrack(window, wtrack) >= 0) {
    message = talloc_format(PRE "Cent text, track #%d", wtrack->l.num);;

  } else if (BPMTEXT_subsubtrack(window) >= 0) {
    message = PRE "BPM track";

  } else if (LPBTEXT_subsubtrack(window) >= 0) {
    message = PRE "LPB track";

  } else if (SIGNATURETEXT_subsubtrack(window) >= 0) {
    message = PRE "Signature track";

  } else if (SWINGTEXT_subsubtrack(window, window->curr_track < 0 ? NULL : wtrack) >= 0) {
    if (window->curr_track >= 0)
      message = talloc_format(PRE "Swing text sub track. track #%d", window->curr_track);
    else
      message = PRE "Global Swing track";

  } else if (window->curr_track >= 0) {
    if (window->curr_track_sub==-1)
      message = talloc_format(PRE "Note text, track #%d", wtrack->l.num);
    else {
      message = talloc_format(PRE "Automation area, track #%d", wtrack->l.num);
      //int veltracknum = (int)scale_int64(window->curr_track_sub, 0, GetNumSubtracks(wtrack) - ;
    }
      
  } else {
    //message = talloc_format(PRE "the %s track", get_track_name(window->curr_track));
    message = talloc_format(PRE "%s track", get_track_name(window->curr_track));
  }
  
  setStatusbarText(message);

#undef PRE
}

