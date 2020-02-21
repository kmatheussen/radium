
#include "nsmtracker.h"
#include "disk.h"
#include "disk_save_proc.h"
#include "disk_load_proc.h"
#include "disk_wblock_proc.h"
#include "disk_block_proc.h"
#include "disk_wtrack_proc.h"
#include "disk_track_proc.h"
#include "patch_proc.h"
#include "block_insert_proc.h"
#include "undo.h"
#include "undo_block_insertdelete_proc.h"
#include "undo_tracks_proc.h"
#include "OS_Bs_edit_proc.h"
#include "visual_proc.h"
#include "player_pause_proc.h"
#include "wblocks_proc.h"
#include "track_insert_proc.h"
#include "clipboard_track_paste_proc.h"

#include "../config/config.h"

#include "../OpenGL/Widget_proc.h"

#include "../api/api_proc.h"

#include "disk_saveload_blocktrack_proc.h"


static void add_track_patches(vector_t *v, const struct Tracks *track){
  if (track->patch != NULL && !VECTOR_is_in_vector(v, track->patch))
    VECTOR_push_back(v, track->patch);
  
  VECTOR_FOR_EACH(struct FXs *fxs, &track->fxs){
    struct FX *fx = fxs->fx;
    if (fx->patch != NULL && !VECTOR_is_in_vector(v, fx->patch))
      VECTOR_push_back(v, fx->patch);
  }END_VECTOR_FOR_EACH;
}

static void save_block_patches(const struct Blocks *block){
  vector_t v = {0};

  const struct Tracks *track = block->tracks;
  while(track!=NULL){
    add_track_patches(&v, track);
    track = NextTrack(track);
  }

  hash_t *state = PATCHES_get_state(&v, true);
  HASH_save(state, dc.file);
}

static void save_track_patches(const struct Tracks *track){
  vector_t v = {0};

  add_track_patches(&v, track);
  
  hash_t *state = PATCHES_get_state(&v, true);
  HASH_save(state, dc.file);
}

static void remove_all_patches_and_fxs_from_track(struct Tracks *track){
  if (track->patch != NULL)
    track->patch->id = make_instrument(-1);
  
  vector_t fxs = {0};
  track->fxs = fxs;
}
  

/******** Block **********************/

void SaveBlockToDisk(filepath_t filename, const struct WBlocks *wblock){
  struct Tracker_Windows *window=root->song->tracker_windows;
  
  R_ASSERT_RETURN_IF_FALSE(wblock!=NULL);

  if (isIllegalFilepath(filename))
    filename = GFX_GetSaveFileName(window,NULL,"Select filename for block to save", createIllegalFilepath(), "*.rad_block", "Block files", true);

  if (isIllegalFilepath(filename))
    return;

  if (Save_Initialize(filename, "RADIUM BLOCK")==false)
    return;
  
  DC_SSF("blockdiskversion",BLOCKDISKVERSION);
  
  save_block_patches(wblock->block);
  SaveWBlock(wblock, false);
  SaveBlock(wblock->block, false);

  if( ! dc.success){
    GFX_Message2(NULL, true, "Problems writing to file.\n");
  }
  
  DISK_close_and_delete(dc.file);
}

static void remove_all_patches_and_fxs_from_loaded_block(struct Blocks *block){
  struct Tracks *track = block->tracks;
  while(track != NULL){
    
    remove_all_patches_and_fxs_from_track(track);
      
    track = NextTrack(track);
  }
}

void LoadBlockFromDisk(filepath_t filename){
  struct Tracker_Windows *window=root->song->tracker_windows;
    
  bool success = false;
  bool have_made_undo = false;

  if (isIllegalFilepath(filename) || !wcscmp(filename.id, L""))
    filename = GFX_GetLoadFileName(window,NULL,"Select block to load", createIllegalFilepath(), "*.rad_block", "Block files", true);

  if (isIllegalFilepath(filename)){
    goto exit;
    return;
  }
    

  NInt blockpos=window->wblock->l.num+1;
  
  if (Load_Initialize(filename, "RADIUM BLOCK")==false) {
    goto exit;
    return;
  }

  if(dc.type!=LS_VARIABLE){
    R_ASSERT(false);
    goto exit;
    return;
  }

  float block_version = DC_LoadF();
  if (block_version>BLOCKDISKVERSION+0.05){
    GFX_Message2(NULL,true,"Need a newer version of Radium to load this file. The file version is %f, while this program only supports %f.\n",block_version,BLOCKDISKVERSION);
    goto exit;
    return;
  }
    
  HASH_load(dc.file); // We don't use saved instruments yet. But they are saved to disk.

  DC_Next();  
  if (dc.success==false){
    GFX_Message2(NULL,true,"Loading failed. File too short. (3)\n");
    goto exit;
    return;
  }

  if(strcmp(dc.ls,"WBLOCK")){
    GFX_Message(NULL, "Loading failed.\nExpected \"WBLOCK\", but found instead: '%s'.\nFile: '%S'\n",dc.ls,filename.id);
    DISK_close_and_delete(dc.file);
    goto exit;
    return;
  }

  struct WBlocks *wblock = LoadWBlock();
  wblock->l.num = blockpos;

  DC_Next();
  if(strcmp(dc.ls,"BLOCK")){
    GFX_Message(NULL, "Loading failed.\nExpected \"BLOCK\", but found instead: '%s'.\nFile: '%S'\n",dc.ls,filename.id);
    DISK_close_and_delete(dc.file);
    goto exit;
  }

  struct Blocks *block = LoadBlock();
  block->l.num = blockpos;

  remove_all_patches_and_fxs_from_loaded_block(block);
  
  DISK_close_and_delete(dc.file);

  if(!dc.success){
    GFX_Message2(NULL, true, "Loading failed.\n");
    goto exit;
  }
  
  printf("Got it: %p / %p\n",wblock,block);


  g_is_loading = true;

    
  ADD_UNDO(Block_Insert(blockpos));
  have_made_undo = true;

  PC_Pause();{
    wblock->block = block;
    window->curr_track = 0;
    
    InsertBlock_IncBlockNums(blockpos);
    ListAddElement1(&root->song->blocks,&block->l);
    ListAddElement1(&window->wblocks, &wblock->l);
    
    DLoadBlocks(root, block, false);
    DLoadWBlocks(root, window, wblock, false);  

    SelectWBlock(window,wblock,true);
              
    BS_UpdateBlockList();
    BS_UpdatePlayList();

    // Not necessary since current opengl realline is set when rendering.
    //GE_set_curr_realline(window->wblock->curr_realline);
  }PC_StopPause(NULL);
  
  window->must_redraw = true;

  success = true;
  
 exit:
  if (success==false)
    if (have_made_undo)
      UNDO_CANCEL_LAST_UNDO();

  g_is_loading = false;
}



/******** Track **********************/


void SaveTrackToDisk(filepath_t filename, const struct WTracks *wtrack){
  struct Tracker_Windows *window=root->song->tracker_windows;
  
  R_ASSERT_RETURN_IF_FALSE(wtrack!=NULL);

  if (isIllegalFilepath(filename) || !wcscmp(filename.id, L""))
    filename=GFX_GetSaveFileName(window,NULL,"Select filename for track to save", createIllegalFilepath(), "*.rad_track", "Track files", true);
  
  if (isIllegalFilepath(filename))
    return;

  if (Save_Initialize(filename, "RADIUM TRACK")==false)
    return;
  
  DC_SSF("trackdiskversion",TRACKDISKVERSION);
  
  save_track_patches(wtrack->track);
  SaveWTrack(wtrack, false);
  SaveTrack(wtrack->track, false);

  if( ! dc.success){
    GFX_Message2(NULL, true, "Problems writing to file.\n");
  }
  
  DISK_close_and_delete(dc.file);
}

void LoadTrackFromDisk(filepath_t filename, struct Tracker_Windows *window, struct WBlocks *wblock, struct WTracks *old_wtrack){
  
  bool success = false;
  bool have_made_undo = false;

  if (isIllegalFilepath(filename) || !wcscmp(filename.id, L""))
    filename=GFX_GetLoadFileName(window,NULL,"Select track to load", createIllegalFilepath(), "*.rad_track", "Track files", true);
  
  if (isIllegalFilepath(filename)){
    goto exit;
    return;
  }
    

  if (Load_Initialize(filename, "RADIUM TRACK")==false) {
    goto exit;
    return;
  }

  if(dc.type!=LS_VARIABLE){
    R_ASSERT(false);
    goto exit;
    return;
  }

  float track_version = DC_LoadF();
  if (track_version>TRACKDISKVERSION+0.05){
    GFX_Message2(NULL,true,"Need a newer version of Radium to load this file. The file version is %f, while this program only supports %f.\n",track_version,TRACKDISKVERSION);
    goto exit;
    return;
  }
    
  HASH_load(dc.file); // We don't use saved instruments yet. But they are saved to disk.

  DC_Next();  
  if (dc.success==false){
    GFX_Message2(NULL,true,"Loading failed. File too short. (3)\n");
    goto exit;
    return;
  }

  if(strcmp(dc.ls,"WTRACK")){
    GFX_Message2(NULL, true,"Loading failed.\nExpected \"WTRACK\", but found instead: '%s'.\nFile: '%S'\n",dc.ls,filename.id);
    DISK_close_and_delete(dc.file);
    goto exit;
    return;
  }

  struct WTracks *wtrack = LoadWTrack();
  wtrack->l.num = old_wtrack->l.num;

  DC_Next();
  if(strcmp(dc.ls,"TRACK")){
    GFX_Message(NULL, "Loading failed.\nExpected \"TRACK\", but found instead: '%s'.\nFile: '%S'\n",dc.ls,filename.id);
    DISK_close_and_delete(dc.file);
    goto exit;
  }

  struct Tracks *track = LoadTrack();
  track->l.num = old_wtrack->l.num;

  DISK_close_and_delete(dc.file);

  g_is_loading = true;
    
  remove_all_patches_and_fxs_from_track(track);
  DLoadTracks(root, track, false);
  DLoadWTracks(root, window, wblock, wtrack, false);  
  wtrack->track = track; // DLoadWTracks overrides wtrack->track.

  if(!dc.success){
    GFX_Message2(NULL, true,"Loading failed.\n");
    goto exit;
  }
  
  printf("Got it: %p / %p\n",wtrack,track);

  ADD_UNDO(Track_CurrPos(wblock->l.num, old_wtrack->l.num));
  have_made_undo = true;

  PC_Pause();{
    co_CB_PasteTrack(wblock, wtrack, old_wtrack);
  }PC_StopPause(NULL);
  
  window->must_redraw = true;

  success = true;
  
 exit:
  if (success==false)
    if (have_made_undo)
      UNDO_CANCEL_LAST_UNDO();
  
  g_is_loading = false;
}


