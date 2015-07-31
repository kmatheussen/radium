/* Copyright 2012 Kjetil S. Matheussen

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


#include "../common/nsmtracker.h"
#include "../common/undo.h"
#include "../common/hashmap_proc.h"

#include "../audio/Mixer_proc.h"

#include "QM_MixerWidget.h"
#include "QM_chip.h"

#include "undo_chip_addremove_proc.h"


extern struct Root *root;

struct Undo_Chip_AddRemove{
  struct Patch *patch;
  bool is_present;

  hash_t *chip_state;
  hash_t *connections_state;
};

static void *Undo_Do_Chip_AddRemove(
	struct Tracker_Windows *window,
	struct WBlocks *wblock,
	struct WTracks *wtrack,
	int realline,
	void *pointer
);

static void Undo_Chip_AddRemove(
	struct Tracker_Windows *window,
	struct WBlocks *wblock,
        struct Patch *patch,
        bool going_to_add
){

  struct Undo_Chip_AddRemove *u_rt=talloc(sizeof(struct Undo_Chip_AddRemove));

  u_rt->patch = patch;
//u_rt->connections_state = MW_get_connections_state();

  if(going_to_add==true){
    u_rt->is_present = true;
  }else{
    u_rt->is_present = false;
    u_rt->chip_state = CHIP_get_chip_state_from_patch(u_rt->patch);
  }

  Undo_Add_dont_stop_playing(
           window->l.num,
           wblock->l.num,
           wblock->wtrack->l.num,
           wblock->curr_realline,
           u_rt,
           Undo_Do_Chip_AddRemove
           );
}

void Undo_Chip_Add_CurrPos(struct Patch *patch){
  struct Tracker_Windows *window = root->song->tracker_windows;
  Undo_Chip_AddRemove(window,window->wblock,patch, true);
}

void Undo_Chip_Remove_CurrPos(struct Patch *patch){
  struct Tracker_Windows *window = root->song->tracker_windows;
  Undo_Chip_AddRemove(window,window->wblock,patch, false);
}

static void *Undo_Do_Chip_AddRemove(
	struct Tracker_Windows *window,
	struct WBlocks *wblock,
	struct WTracks *wtrack,
	int realline,
	void *pointer
){
  struct Undo_Chip_AddRemove *u_rt=pointer;
  bool is_present = u_rt->is_present;

//hash_t *new_connections_state = MW_get_connections_state();
  {
    if(is_present==true){
      u_rt->chip_state = CHIP_get_chip_state_from_patch(u_rt->patch);
      MW_delete_plugin((SoundPlugin *)u_rt->patch->patchdata);
      u_rt->is_present=false;
    }else{
      CHIP_create_from_state(u_rt->chip_state, MIXER_get_buses());
      u_rt->is_present=true;
    }
    
    //MW_create_connections_from_state(u_rt->connections_state);
  }
//u_rt->connections_state = new_connections_state;

  return u_rt;
}
