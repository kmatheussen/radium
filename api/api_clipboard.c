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
#include "../common/clipboard_range.h"
#include "../common/disk_saveload_blocktrack_proc.h"
#include "../mixergui/QM_MixerWidget.h"
#include "../common/undo_tracks_proc.h"
#include "../common/visual_proc.h"
#include "../common/seqtrack_proc.h"
#include "../config/config.h"

#include "api_common_proc.h"


extern struct Range *range;



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
  } else {

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
}

void copyRange(int windownum){
  struct Tracker_Windows *window=getWindowFromNum(windownum);if(window==NULL) return;
  CopyRange_CurrPos(window);
}

void copyTrack(int tracknum, int blocknum, int windownum){
  struct Tracker_Windows *window=getWindowFromNum(windownum);if(window==NULL) return;
  
  if (tracknum==-1 && blocknum==-1){
    
    CB_CopyTrack_CurrPos(window);
    
  } else {
    
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
    
  }
  
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
    UNDO_OPEN_REC();{
      ADD_UNDO(Track_CurrPos(wblock->l.num, wtrack->l.num));
      co_CB_PasteTrack(wblock, cb_wtrack, wtrack);
    }UNDO_CLOSE();
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


int getNumTracksInRange(void){
  if (range==NULL)
    return 0;

  return range->num_tracks;
}

Place getRangeLength(void){
  if (range==NULL)
    return p_Create(0,0,1);

  return range->length;
}


// # These functions works on the currently marked range, not the clipboard. The functions above works on the range copied into the clipboard.

bool hasRange(int blocknum, int windownum){
  struct Tracker_Windows *window;
  struct WBlocks *wblock;

  wblock=getWBlockFromNumA(
                           windownum,
                           &window,
                           blocknum
                           );

  if(wblock==NULL)
    return false;

  return wblock->isranged;
}

int getRangeStartTrack(int blocknum, int windownum){
  struct Tracker_Windows *window;
  struct WBlocks *wblock;

  wblock=getWBlockFromNumA(
                           windownum,
                           &window,
                           blocknum
                           );

  if(wblock==NULL)
    return 0;

  if (wblock->rangex1 < 0)
    return 0;
  if (wblock->rangex1 >= wblock->block->num_tracks)
    return wblock->block->num_tracks-1;

  return wblock->rangex1;
}

int getRangeEndTrack(int blocknum, int windownum){
  struct Tracker_Windows *window;
  struct WBlocks *wblock;

  wblock=getWBlockFromNumA(
                           windownum,
                           &window,
                           blocknum
                           );

  if(wblock==NULL)
    return 0;

  if (wblock->rangex1 < 0)
    return 0;
  if (wblock->rangex1 >= wblock->block->num_tracks)
    return wblock->block->num_tracks-1;

  return wblock->rangex2+1;
}

Place getRangeStartPlace(int blocknum, int windownum){
  struct Tracker_Windows *window;
  struct WBlocks *wblock;

  wblock=getWBlockFromNumA(
                           windownum,
                           &window,
                           blocknum
                           );

  if(wblock==NULL)
    return p_Create(0,0,1);

  if (p_Greater_Than(wblock->rangey1, p_Last_Pos(wblock->block)))
    return p_Last_Pos(wblock->block);

  return wblock->rangey1;
}

Place getRangeEndPlace(int blocknum, int windownum){
  struct Tracker_Windows *window;
  struct WBlocks *wblock;

  wblock=getWBlockFromNumA(
                           windownum,
                           &window,
                           blocknum
                           );

  if(wblock==NULL)
    return p_Create(1,0,1);

  if (p_Greater_Than(wblock->rangey1, p_Create(wblock->block->num_lines, 0, 1)))
    return p_Create(wblock->block->num_lines, 0, 1);

  return wblock->rangey2;
}



// Mixer

void copySelectedMixerObjects(void){
  MW_copy();
}

void deleteSelectedMixerObjects(void){
  MW_delete();
}

void cutSelectedMixerObjects(void){
  MW_cut();
}

int64_t pasteMixerObjects(float x, float y){
  int64_t ret;

  //printf("   X: %f,    Y: %f\n", x,  y);
  UNDO_OPEN();{
    ret = MW_paste(x, y);
  }UNDO_CLOSE();

  return ret;
}

void cutGeneral(void){
  if (MW_has_mouse_pointer())
    cutSelectedMixerObjects();
  else if (SEQUENCER_has_mouse_pointer())
    cutSelectedSeqblocks();
}

void copyGeneral(void){
  if (MW_has_mouse_pointer())
    copySelectedMixerObjects();
  else if (SEQUENCER_has_mouse_pointer())
    copySelectedSeqblocks();
  else
    copyBlock(-1);
}

void pasteGeneral(void){
  if (MW_has_mouse_pointer())
    pasteMixerObjects(-10000,-10000);
  else if (SEQUENCER_has_mouse_pointer())
    pasteSeqblocks(-1,-1);
  else
    pasteBlock(-1);
}


void saveBlock(const char *filename, int blocknum, int windownum){
  struct Tracker_Windows *window;
  struct WBlocks *wblock;

  wblock=getWBlockFromNumA(
                           windownum,
                           &window,
                           blocknum
                           );

  if(wblock==NULL)
    return;

  SaveBlockToDisk(filename, wblock);
}


void loadBlock(const char *filename_c){
  LoadBlockFromDisk(filename_c);
}


void saveTrack(const char *filename, int tracknum, int blocknum, int windownum){  
  struct Tracker_Windows *window=getWindowFromNum(windownum);if(window==NULL) return;
  
  if (window->curr_track < 0){
    GFX_Message2(NULL, true, "Saving a %s track is not supported", get_track_name(window->curr_track));
    return;
  }

  struct WTracks *wtrack=getWTrackFromNum(windownum, blocknum, tracknum);

  if(wtrack==NULL)
    return;

  SaveTrackToDisk(filename, wtrack);
}


void loadTrack(const char *filename_c, int tracknum, int blocknum, int windownum){
  struct Tracker_Windows *window=getWindowFromNum(windownum);if(window==NULL) return;
  
  if (window->curr_track < 0){
    GFX_Message2(NULL, true, "Loading a %s track is not supported", get_track_name(window->curr_track));
    return;
  }

  struct WTracks *wtrack;
  struct WBlocks *wblock;
  
  wtrack=getWTrackFromNumA(
                           windownum,
                           &window,
                           blocknum,
                           &wblock,
                           tracknum
                           );
  
  if(wtrack==NULL)
    return;

  LoadTrackFromDisk(filename_c, window, wblock, wtrack);
}


