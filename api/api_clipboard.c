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
#include "../mixergui/QM_MixerWidget.h"
#include "../common/undo_tracks_proc.h"
#include "../common/visual_proc.h"
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

void copySelectedMixerObjects(void){
  MW_copy();
}

void deleteSelectedMixerObjects(void){
  MW_delete();
}

void cutSelectedMixerObjects(void){
  MW_cut();
}

int pasteMixerObjects(float x, float y){
  int64_t ret;
  
  Undo_Open();{
    ret = MW_paste(x, y);
  }Undo_Close();

  return ret;
}

void cutGeneral(void){
  if (MW_has_mouse_pointer())
    cutSelectedMixerObjects();
}

void copyGeneral(void){
  if (MW_has_mouse_pointer())
    copySelectedMixerObjects();
  else
    copyBlock(-1);
}

void pasteGeneral(void){
  if (MW_has_mouse_pointer())
    pasteMixerObjects(-10000,-10000);
  else
    pasteBlock(-1);
}

#if 0
// nah

#include "../common/disk.h"
#include "../common/disk_save_proc.h"
#include "../common/disk_load_proc.h"
#include "../common/disk_wblock_proc.h"
#include "../common/disk_block_proc.h"
#include "../common/undo_block_insertdelete_proc.h"
#include "../common/undo.h"
#include "../OpenGL/Widget_proc.h"

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

  if (filename==NULL || !strcmp(filename, ""))
    filename = "/tmp/block.block";

  if (Save_Initialize(STRING_create(filename), "RADIUM BLOCK")==false)
    return;

  SaveWBlock(wblock);
  SaveBlock(wblock->block);

  if( ! dc.success){
    GFX_Message(NULL, "Problems writing to file.\n");
  }
  
  DISK_close_and_delete(dc.file);
}

void loadBlock(const char *filename_c){
  struct Tracker_Windows *window=getWindowFromNum(-1);if(window==NULL) return;
    
  bool success = false;
  bool have_made_undo = false;
  
  if (filename_c==NULL || !strcmp(filename_c, ""))
    filename_c = "/tmp/block.block";

  const wchar_t *filename = STRING_create(filename_c);
  
  int num_blocks = root->song->num_blocks;
    
  if (Load_Initialize(filename, "RADIUM BLOCK")==false) {
    goto exit;
    return;
  }
        
  if(strcmp(dc.ls,"WBLOCK")){
    GFX_Message(NULL, "Loading failed.\nExpected \"WBLOCK\", but found instead: '%s'.\nFile: '%s'\n",dc.ls,STRING_get_chars(filename));
    DISK_close_and_delete(dc.file);
    goto exit;
    return;
  }

  struct WBlocks *wblock = LoadWBlock();
  wblock->l.num = num_blocks;

  DC_Next();
  if(strcmp(dc.ls,"BLOCK")){
    GFX_Message(NULL, "Loading failed.\nExpected \"BLOCK\", but found instead: '%s'.\nFile: '%s'\n",dc.ls,STRING_get_chars(filename));
    DISK_close_and_delete(dc.file);
    goto exit;
  }

  struct Blocks *block = LoadBlock();
  block->l.num = num_blocks;
  
  DISK_close_and_delete(dc.file);

  if(!dc.success){
    GFX_Message(NULL, "Loading failed.\n");
    goto exit;
  }
  
  printf("Got it: %p / %p\n",wblock,block);


  ADD_UNDO(Block_Insert(num_blocks));
  have_made_undo = true;

  /*
  DC_ListAdd1(&root->song->blocks,block);
  DC_ListAdd1(&root->song->tracker_windows->wblocks, wblock);
  */

  wblock->block = block;
  window->curr_track = 0;

  DLoadBlocks(root, block);
  DLoadWBlocks(window, window, wblock);  

  CB_PasteBlock(window, wblock, window->wblock);

  success = true;
  
 exit:
  if (success==false)
    if (have_made_undo)
      Undo_CancelLastUndo();
}
#endif
