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
#include "../common/player_pause_proc.h"
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
#include "../common/sequencer_proc.h"
#include "../config/config.h"

#include "api_common_proc.h"

static bool legal_range(int rangetype){
  if (rangetype < 0 || rangetype > 4){
    handleError("Illegal rangetype: %d", rangetype);
    return false;
  }

  return true;
}

static struct Range *get_range(int rangetype){
  if (!legal_range(rangetype))
    return NULL;
  
  return g_ranges[rangetype];
}

void cancelRange(int windownum){
  struct Tracker_Windows *window=getWindowFromNum(windownum);if(window==NULL) return;
  CancelRange_CurrPos(window);
}

void cutRange(int rangetype, int windownum){
  struct Tracker_Windows *window=getWindowFromNum(windownum);if(window==NULL) return;

  if (!legal_range(rangetype))
    return;
  
  CutRange_CurrPos(window, rangetype);
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

    ADD_UNDO(Track_CurrPos(wblock->l.num, wtrack->l.num));
    
    cb_wtrack = CB_CutTrack(window, wblock, wtrack);

    TIME_block_swings_have_changed(wblock->block);
    
    window->must_redraw_editor = true;
  }
}

void clearTrack(int tracknum, int blocknum, int windownum){
  if (tracknum==-1 && blocknum==-1){
    
    struct Tracker_Windows *window=getWindowFromNum(windownum);if(window==NULL) return;
    CB_ClearTrack_CurrPos(window);
    
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

    ADD_UNDO(Track_CurrPos(wblock->l.num, wtrack->l.num));

    PC_Pause();{
      CB_ClearTrack_Force(wblock->block, wtrack->track);
    }PC_StopPause(window);

    TIME_block_swings_have_changed(wblock->block);
    
    window->must_redraw = true;
  }
}

void copyRange(int rangetype, int windownum){
  struct Tracker_Windows *window=getWindowFromNum(windownum);if(window==NULL) return;

  if (!legal_range(rangetype))
    return;

  CopyRange_CurrPos(window, rangetype);
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

void pasteRange(int rangetype, int windownum){  
  struct Tracker_Windows *window=getWindowFromNum(windownum);if(window==NULL) return;
  struct Range *range = get_range(rangetype);
  if (range != NULL)
    PasteRange_CurrPos(window, range);
}


void pasteRange2(Place startplace, int starttracknum, int rangetype, int blocknum, int windownum){
  struct Tracker_Windows *window;
  struct WBlocks *wblock;

  wblock=getWBlockFromNumA(
                           windownum,
                           &window,
                           blocknum
                           );

  if(wblock==NULL)
    return;

  struct Blocks *block = wblock->block;
  
  if (starttracknum < 0 || starttracknum >= block->num_tracks){
    handleError("pasteRange2: Illegal starttracknum: %d", starttracknum);
    return;
  }

  if (!PlaceLegal(block, &startplace)){
    handleError("pasteRange2: Illegal startplace: %s", PlaceToString(&startplace));
    return;
  }

  struct Range *range = get_range(rangetype);
  if (range != NULL)
    PasteRange(block, starttracknum, &startplace, range);
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

    TIME_block_swings_have_changed(wblock->block);
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


int getNumTracksInRange(int rangetype){
  struct Range *range = get_range(rangetype);
  if (!range)
    return 0;

  return range->num_tracks;
}

Place getRangeLength(int rangetype){
  struct Range *range = get_range(rangetype);
  if (!range)
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
    return 1;
  
  if (wblock->rangex1 >= wblock->block->num_tracks)
    return wblock->block->num_tracks;

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


static bool range_is_legal(const struct WBlocks *wblock, const Place p1, const Place p2, int start_track, int end_track){
  return
    end_track > start_track &&
    start_track >= 0 &&
    end_track <= wblock->block->num_tracks &&
    p_Greater_Than(p2, p1) &&
    p_Greater_Or_Equal(p1, p_Create(0,0,1)) &&
    p_Less_Or_Equal(p2, p_Create(wblock->block->num_lines, 0, 1));
}

void setRange(Place p1, Place p2, int start_track, int end_track, int blocknum, int windownum){
  struct Tracker_Windows *window;
  struct WBlocks *wblock;

  wblock=getWBlockFromNumA(
                           windownum,
                           &window,
                           blocknum
                           );

  if(wblock==NULL)
    return;

  if (!range_is_legal(wblock, p1,p2,start_track,end_track)) {

    handleError("setRange: Illegal range. p1: %s. p2: %s. start_track: %d. end_track: %d", p_ToString(p1), p_ToString(p2), start_track, end_track);
    wblock->isranged=false;
    
  } else {
    
    SetRange(window, wblock, start_track, end_track-1, p1, p2);
    
    MakeRangeLegal(wblock);

    if (!range_is_legal(wblock, wblock->rangey1, wblock->rangey2, wblock->rangex1, wblock->rangex2+1))
      wblock->isranged=false;
    
  }
  
  window->must_redraw = true;
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

instrument_t pasteMixerObjects(float x, float y){
  instrument_t ret;

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


void saveBlock(filepath_t filename, int blocknum, int windownum){
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


void loadBlock(filepath_t filename){
  LoadBlockFromDisk(filename);
}


void saveTrack(filepath_t filename, int tracknum, int blocknum, int windownum){  
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


void loadTrack(filepath_t filename, int tracknum, int blocknum, int windownum){
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

  LoadTrackFromDisk(filename, window, wblock, wtrack);
}


