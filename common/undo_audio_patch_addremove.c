/* Copyright 2012-2016 Kjetil S. Matheussen

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
#include "undo.h"
#include "hashmap_proc.h"
#include "patch_proc.h"

#include "../audio/audio_instrument_proc.h"

#include "../api/api_proc.h"

#include "undo_audio_patch_addremove_proc.h"


extern struct Root *root;

struct Undo_Audio_Patch_AddRemove{
  struct Patch *patch;
  bool is_present;
  bool was_current;
  
  hash_t *audio_patch_state;
};

static void *Undo_Do_Audio_Patch_AddRemove(
	struct Tracker_Windows *window,
	struct WBlocks *wblock,
	struct WTracks *wtrack,
	int realline,
	void *pointer
);

static void Undo_Audio_Patch_AddRemove(
	struct Tracker_Windows *window,
	struct WBlocks *wblock,
        struct Patch *patch,
        hash_t *state,
        bool going_to_add,
        bool is_current_patch
){

  struct Undo_Audio_Patch_AddRemove *u_rt=talloc(sizeof(struct Undo_Audio_Patch_AddRemove));

  u_rt->patch = patch;

  u_rt->audio_patch_state = state;

  if(going_to_add==true){
    u_rt->is_present = true;
  }else{
    u_rt->is_present = false;
  }

  u_rt->was_current = is_current_patch;
  
  Undo_Add_dont_stop_playing(
                             window->l.num,
                             wblock->l.num,
                             wblock->wtrack->l.num,
                             wblock->curr_realline,
                             u_rt,
                             Undo_Do_Audio_Patch_AddRemove,
                             talloc_format("%s %s", going_to_add ? "Add audio_patch" : "Remove audio_patch", patch->name)
                             );
}

void ADD_UNDO_FUNC(Audio_Patch_Add_CurrPos(struct Patch *patch, bool is_current_patch)){
  struct Tracker_Windows *window = root->song->tracker_windows;
  Undo_Audio_Patch_AddRemove(window,window->wblock,patch, AUDIO_get_audio_patch_state(patch), true, is_current_patch);
}

void ADD_UNDO_FUNC(Audio_Patch_Remove_CurrPos(struct Patch *patch, hash_t *state, bool is_current_patch)){
  struct Tracker_Windows *window = root->song->tracker_windows;
  Undo_Audio_Patch_AddRemove(window,window->wblock,patch, state, false, is_current_patch);
}

static void *Undo_Do_Audio_Patch_AddRemove(
	struct Tracker_Windows *window,
	struct WBlocks *wblock,
	struct WTracks *wtrack,
	int realline,
	void *pointer
){
  struct Undo_Audio_Patch_AddRemove *u_rt=pointer;
  bool is_present = u_rt->is_present;

  fprintf(stderr,"         UNDO_DO_AUDIO_PATCH %d\n",is_present);
  
  if(is_present==true){

    fprintf(stderr,"         UNDO_DO_AUDIO_PATCH 2 %d\n",is_present);
    
    PATCH_make_inactive(u_rt->patch);
    u_rt->is_present=false;
    
  }else{

    fprintf(stderr,"         UNDO_DO_AUDIO_PATCH 3. is_present: %d. set_to_current: %d.\n",is_present, u_rt->was_current);
    
    if (PATCH_make_active_audio(u_rt->patch, NULL, NULL, u_rt->audio_patch_state, u_rt->was_current, 0, 0)==false)
      R_ASSERT(false);

    u_rt->is_present=true;
    
  }

  remakeMixerStrips(u_rt->patch->id);
                    
  return u_rt;
}
