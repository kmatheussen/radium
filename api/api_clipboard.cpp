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
#include "../common/undo_range_proc.h"
#include "../common/disk_saveload_blocktrack_proc.h"
#include "../mixergui/QM_MixerWidget.h"
#include "../common/undo_tracks_proc.h"
#include "../common/visual_proc.h"
#include "../common/sequencer_proc.h"
#include "../config/config.h"

#include "api_common_proc.h"

static bool legal_rangetype(int rangetype){
  if (rangetype < 0 || rangetype > 4){
    handleError("Illegal rangetype: %d", rangetype);
    return false;
  }

  return true;
}

static struct RangeClip *get_range_clip(int rangetype){
  if (!legal_rangetype(rangetype))
    return NULL;
  
  return g_range_clips[rangetype];
}

void cancelRange(int windownum){
  struct Tracker_Windows *window=getWindowFromNum(windownum);if(window==NULL) return;
  CancelRange_CurrPos(window);
}

void clearRange(int windownum){
  struct Tracker_Windows *window=getWindowFromNum(windownum);if(window==NULL) return;

  struct WBlocks *wblock = window->wblock;
  
  const range_t range = wblock->range;
  
  if(! range.enabled)
    return;

  clearRange2(range.y1, range.y2, range.x1, range.x2+1, wblock->l.num, windownum);
}

void clearRange2(Place p1, Place p2, int start_track, int end_track, int blocknum, int windownum){
  struct Tracker_Windows *window;
  struct WBlocks *wblock;

  wblock=getWBlockFromNumA(
                           windownum,
                           &window,
                           blocknum
                           );

  if(wblock==NULL)
    return;
  
  range_t range = make_range(true, start_track, end_track, p1, p2);

  if (!range_is_legal3(wblock, range)){
    handleError("clearRange2: Illegal range. p1: %s. p2: %s. start_track: %d. end_track: %d", p_ToString(range.y1), p_ToString(range.y2), range.x1, range.x2);
    return;
  }

  ADD_UNDO(Range(
                 window,
                 wblock,
                 range.x1,
                 range.x2+1,
                 window->wblock->curr_realline
                 ));

  ClearRange2(wblock->block, range);

  window->must_redraw = true;
}

void cutRange(int rangetype, int windownum){
  struct Tracker_Windows *window=getWindowFromNum(windownum);if(window==NULL) return;

  if (!legal_rangetype(rangetype))
    return;
  
  CutRange_CurrPos(window, rangetype);
}

void cutRange2(Place p1, Place p2, int start_track, int end_track, int rangetype, int blocknum, int windownum){
  struct Tracker_Windows *window;
  struct WBlocks *wblock;

  wblock=getWBlockFromNumA(
                           windownum,
                           &window,
                           blocknum
                           );

  if(wblock==NULL)
    return;

  if (!legal_rangetype(rangetype))
    return;
  
  range_t range = make_range(true, start_track, end_track, p1, p2);

  if (!range_is_legal3(wblock, range)){
    handleError("cutRange2: Illegal range. p1: %s. p2: %s. start_track: %d. end_track: %d", p_ToString(range.y1), p_ToString(range.y2), range.x1, range.x2);
    return;
  }

  ADD_UNDO(Range(
                 window,
                 wblock,
                 range.x1,
                 range.x2+1,
                 window->wblock->curr_realline
                 ));

  CutRange(wblock->block, range, rangetype);

  window->must_redraw = true;
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

    radium::PlayerPauseOnlyIfNeeded pause_player;
    bool swings_have_changed = false;
    CB_ClearTrack_Force(wblock->block, wtrack->track, pause_player, swings_have_changed);

    if (swings_have_changed)
      TIME_block_swings_have_changed(wblock->block);
    
    window->must_redraw = true;
  }
}

void copyRange(int rangetype, int windownum){
  struct Tracker_Windows *window=getWindowFromNum(windownum);if(window==NULL) return;

  if (!legal_rangetype(rangetype))
    return;

  CopyRange_CurrPos(window, rangetype);
}

void copyRange2(Place p1, Place p2, int start_track, int end_track, int rangetype, int blocknum, int windownum){
  struct Tracker_Windows *window;
  struct WBlocks *wblock;

  wblock=getWBlockFromNumA(
                           windownum,
                           &window,
                           blocknum
                           );

  if(wblock==NULL)
    return;

  if (!legal_rangetype(rangetype))
    return;
  
  range_t range = make_range(true, start_track, end_track, p1, p2);

  if (!range_is_legal3(wblock, range)){
    handleError("copyRange2: Illegal range. p1: %s. p2: %s. start_track: %d. end_track: %d", p_ToString(range.y1), p_ToString(range.y2), range.x1, range.x2);
    return;
  }
  
  CopyRange(wblock->block, range, rangetype);
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
  struct RangeClip *range_clip = get_range_clip(rangetype);
  if (range_clip != NULL)
    PasteRange_CurrPos(window, range_clip);
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

  struct RangeClip *range_clip = get_range_clip(rangetype);
  if (range_clip == NULL)
    return;

  ADD_UNDO(Range(
                 window,
                 wblock,
                 starttracknum,
                 starttracknum + range_clip->num_tracks,
                 window->wblock->curr_realline
                 ));
  
  PasteRange(block, starttracknum, &startplace, range_clip);
}


void pasteTrack(int tracknum, int blocknum, int windownum){
  if (tracknum==-1 && blocknum==-1){
    struct Tracker_Windows *window=getWindowFromNum(windownum);if(window==NULL) return;
    CB_PasteTrack_CurrPos(window);
    window->must_redraw = true;
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
  struct RangeClip *range_clip = get_range_clip(rangetype);
  if (!range_clip)
    return 0;

  return range_clip->num_tracks;
}

Place getRangeLength(int rangetype){
  struct RangeClip *range_clip = get_range_clip(rangetype);
  if (!range_clip)
    return p_Create(0,0,1);

  return range_clip->length;
}

bool hasRangeInClipboard(int rangetype){
  return get_range_clip(rangetype) != NULL;
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

  return wblock->range.enabled;
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

  if (wblock->range.x1 < 0)
    return 0;
  
  if (wblock->range.x1 >= wblock->block->num_tracks)
    return wblock->block->num_tracks-1;

  return wblock->range.x1;
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

  if (wblock->range.x1 < 0)
    return 1;
  
  if (wblock->range.x1 >= wblock->block->num_tracks)
    return wblock->block->num_tracks;

  return wblock->range.x2+1;
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

  if (p_Greater_Than(wblock->range.y1, p_Last_Pos(wblock->block)))
    return p_Last_Pos(wblock->block);

  return wblock->range.y1;
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

  if (p_Greater_Than(wblock->range.y1, p_Create(wblock->block->num_lines, 0, 1)))
    return p_Create(wblock->block->num_lines, 0, 1);

  return wblock->range.y2;
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
    return;
    
    
  } 
    
  SetRange(window, wblock, start_track, end_track-1, p1, p2);
    
  MakeRangeLegal(wblock);

  if (!range_is_legal2(wblock)){
    R_ASSERT_NON_RELEASE(false);
    wblock->range.enabled=false;
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


