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
#include "../common/placement_proc.h"
#include "../common/clipboard_range_proc.h"
#include "../common/clipboard_range_copy_proc.h"
#include "../common/clipboard_range_paste_proc.h"
#include "../common/clipboard_range_cut_proc.h"
#include "../common/clipboard_track_copy_proc.h"
#include "../common/clipboard_track_paste_proc.h"
#include "../common/clipboard_track_cut_proc.h"
#include "../common/clipboard_block_copy_proc.h"
#include "../common/clipboard_block_paste_proc.h"
#include "../common/undo_tracks_proc.h"

#include "api_common_proc.h"



void cancelRange(int windownum){
  struct Tracker_Windows *window=getWindowFromNum(windownum);if(window==NULL) return;
  CancelRange_CurrPos(window);
}

void cutRange(int windownum){
  struct Tracker_Windows *window=getWindowFromNum(windownum);if(window==NULL) return;
  CutRange_CurrPos(window);
}

void cutTrack(int tracknum, int blocknum, int windownum){
  if (tracknum==-1 && blocknum==-1){
    struct Tracker_Windows *window=getWindowFromNum(windownum);if(window==NULL) return;
    CB_CutTrack_CurrPos(window);
  }

  struct Tracker_Windows *window=NULL;
  struct WTracks *wtrack;
  struct WBlocks *wblock;

  wtrack=getWTrackFromNumA(
                           windownum,
                           &window,
                           blocknum,
                           &wblock,
                           tracknum
                           );

  if(wtrack==NULL) return;

  cb_wtrack = CB_CutTrack(window, wblock, wtrack);

  window->must_redraw = true;
}

void copyRange(int windownum){
  struct Tracker_Windows *window=getWindowFromNum(windownum);if(window==NULL) return;
  CopyRange_CurrPos(window);
}

void copyTrack(int tracknum, int blocknum, int windownum){
  if (tracknum==-1 && blocknum==-1){
    struct Tracker_Windows *window=getWindowFromNum(windownum);if(window==NULL) return;
    CB_CopyTrack_CurrPos(window);
  }

  struct Tracker_Windows *window=NULL;
  struct WTracks *wtrack;
  struct WBlocks *wblock;

  wtrack=getWTrackFromNumA(
                           windownum,
                           &window,
                           blocknum,
                           &wblock,
                           tracknum
                           );
  
  if(wtrack==NULL) return;

  cb_wtrack = CB_CopyTrack(wblock, wtrack);

  window->must_redraw = true;
}

void copyBlock(int windownum){
  struct Tracker_Windows *window=getWindowFromNum(windownum);if(window==NULL) return;
  CB_CopyBlock_CurrPos(window);
}

void pasteRange(int windownum){
  struct Tracker_Windows *window=getWindowFromNum(windownum);if(window==NULL) return;
  PasteRange_CurrPos(window);
}

void pasteTrack(int tracknum, int blocknum, int windownum){
  if (tracknum==-1 && blocknum==-1){
    struct Tracker_Windows *window=getWindowFromNum(windownum);if(window==NULL) return;
    CB_PasteTrack_CurrPos(window);
    return;
  }

  struct Tracker_Windows *window=NULL;
  struct WTracks *wtrack;
  struct WBlocks *wblock;

  wtrack=getWTrackFromNumA(
	windownum,
	&window,
	blocknum,
	&wblock,
	tracknum
	);

  if(wtrack==NULL) return;

  if (cb_wtrack != NULL) {
    Undo_Open_rec();{
      ADD_UNDO(Track(window, wblock, wtrack, wblock->curr_realline));
      co_CB_PasteTrack(wblock, cb_wtrack, wtrack);
    }Undo_Close();
  }
    
  window->must_redraw = true;
}

void pasteBlock(int windownum){
  struct Tracker_Windows *window=getWindowFromNum(windownum);if(window==NULL) return;
  CB_PasteBlock_CurrPos(window);
}

void markRange(int windownum){
  struct Tracker_Windows *window=getWindowFromNum(windownum);if(window==NULL) return;
  MarkRange_CurrPos(window);
}


#include "../common/clipboard_range.h"

extern struct Range *range;


int getNumTracksInRange(void){
  if (range==NULL)
    return 0;

  return range->num_tracks;
}

Place getRangeLength(void){
  if (range==NULL)
    return place(0,0,1);

  return range->length;
}
