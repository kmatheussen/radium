
#include "nsmtracker.h"
#include "disk.h"
#include "disk_save_proc.h"
#include "disk_load_proc.h"
#include "disk_wblock_proc.h"
#include "disk_block_proc.h"
#include "patch_proc.h"
#include "block_insert_proc.h"
#include "undo.h"
#include "undo_block_insertdelete_proc.h"
#include "OS_Bs_edit_proc.h"
#include "visual_proc.h"

#include "../config/config.h"

#include "../OpenGL/Widget_proc.h"

#include "disk_saveload_blocktrack_proc.h"



static void save_block_patches(const struct Blocks *block){
  vector_t v = {0};

  const struct Tracks *track = block->tracks;
  while(track!=NULL){

    if (track->patch != NULL && !VECTOR_is_in_vector(&v, track->patch))
      VECTOR_push_back(&v, track->patch);

    VECTOR_FOR_EACH(struct FXs *fxs, &track->fxs){
      struct FX *fx = fxs->fx;
      if (fx->patch != NULL && !VECTOR_is_in_vector(&v, fx->patch))
        VECTOR_push_back(&v, fx->patch);
    }END_VECTOR_FOR_EACH;

    track = NextTrack(track);
  }

  // Must convert patch->id to uuid.

  hash_t *state = PATCHES_get_state(&v, true);
  HASH_save(state, dc.file);
}


void SaveBlockToDisk(const char *filename_c, struct WBlocks *wblock){
  struct Tracker_Windows *window=root->song->tracker_windows;
  
  R_ASSERT_RETURN_IF_FALSE(wblock!=NULL);

  const wchar_t *filename = NULL;
  
  if (filename_c==NULL || !strcmp(filename_c, ""))
    filename=GFX_GetSaveFileName(window,NULL,"Select filename for block to save", NULL, "*.rad_block", "Block files");
  else
    filename = STRING_create(filename_c);
  
  if (filename==NULL)
    return;

  if (Save_Initialize(filename, "RADIUM BLOCK")==false)
    return;
  
  DC_SSF("blockdiskversion",BLOCKDISKVERSION);
  
  save_block_patches(wblock->block);
  SaveWBlock(wblock, false);
  SaveBlock(wblock->block, false);

  if( ! dc.success){
    GFX_Message(NULL, "Problems writing to file.\n");
  }
  
  DISK_close_and_delete(dc.file);
}

static void remove_all_patches_and_fxs_from_loaded_block(struct Blocks *block){
  struct Tracks *track = block->tracks;
  while(track != NULL){
    
    if (track->patch != NULL)
      track->patch->id = -1;

    vector_t fxs = {0};
    track->fxs = fxs;
      
    track = NextTrack(track);
  }
}

void LoadBlockFromDisk(const char *filename_c){
  struct Tracker_Windows *window=root->song->tracker_windows;
    
  bool success = false;
  bool have_made_undo = false;

  const wchar_t *filename = NULL;
  
  if (filename_c==NULL || !strcmp(filename_c, ""))
    filename=GFX_GetLoadFileName(window,NULL,"Select block to load", NULL, "*.rad_block", "Block files");
  else
    filename = STRING_create(filename_c);
  
  if (filename==NULL){
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
    GFX_Message(NULL,"Need a newer version of Radium to load this file. The file version is %f, while this program only supports %f.\n",block_version,BLOCKDISKVERSION);
    goto exit;
    return;
  }
    
  HASH_load(dc.file); // We don't use saved instruments yet. But they are saved to disk.

  DC_Next();  
  if (dc.success==false){
    GFX_Message(NULL,"Loading failed. File too short. (3)\n");
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
  wblock->l.num = blockpos;

  DC_Next();
  if(strcmp(dc.ls,"BLOCK")){
    GFX_Message(NULL, "Loading failed.\nExpected \"BLOCK\", but found instead: '%s'.\nFile: '%s'\n",dc.ls,STRING_get_chars(filename));
    DISK_close_and_delete(dc.file);
    goto exit;
  }

  struct Blocks *block = LoadBlock();
  block->l.num = blockpos;

  remove_all_patches_and_fxs_from_loaded_block(block);
  
  DISK_close_and_delete(dc.file);

  if(!dc.success){
    GFX_Message(NULL, "Loading failed.\n");
    goto exit;
  }
  
  printf("Got it: %p / %p\n",wblock,block);


  ADD_UNDO(Block_Insert(blockpos));
  have_made_undo = true;

  wblock->block = block;
  window->curr_track = 0;

  InsertBlock_IncBlockNums(blockpos);
  ListAddElement1(&root->song->blocks,&block->l);
  ListAddElement1(&window->wblocks, &wblock->l);

  DLoadBlocks(root, block, false);
  DLoadWBlocks(root, window, wblock, false);  

  BS_UpdateBlockList();
  BS_UpdatePlayList();

  GE_set_curr_realline(window->wblock->curr_realline);

  window->must_redraw = true;

  success = true;
  
 exit:
  if (success==false)
    if (have_made_undo)
      Undo_CancelLastUndo();
}
