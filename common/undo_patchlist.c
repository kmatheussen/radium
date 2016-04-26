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





#include <string.h>

#include "nsmtracker.h"
#include "undo.h"
#include "vector_proc.h"
#include "instruments_proc.h"

#include "undo_patchlist_proc.h"


extern struct Root *root;

static void *Undo_Do_Patches(
	struct Tracker_Windows *window,
	struct WBlocks *wblock,
	struct WTracks *wtrack,
	int realline,
	void *pointer
);

static vector_t *get_all_patches(void){
  vector_t *patches = talloc(sizeof(vector_t));
  VECTOR_push_back(patches,VECTOR_copy(&get_MIDI_instrument()->patches));
  VECTOR_push_back(patches,VECTOR_copy(&get_audio_instrument()->patches));
  return patches;
}

void Undo_Patchlist(
                  struct Tracker_Windows *window,
                  struct WBlocks *wblock,
                  source_pos_t source_pos
){

  Undo_Add_dont_stop_playing(
                             window->l.num,
                             wblock->l.num,
                             wblock->wtrack->l.num,
                             wblock->curr_realline,
                             get_all_patches(),
                             Undo_Do_Patches,
                             "Patches",
                             source_pos
                             );
}

void Undo_Patchlist_CurrPos(source_pos_t source_pos){
  Undo_Patchlist(root->song->tracker_windows,root->song->tracker_windows->wblock,source_pos);
}

static void *Undo_Do_Patches(
	struct Tracker_Windows *window,
	struct WBlocks *wblock,
	struct WTracks *wtrack,
	int realline,
	void *pointer
){
  vector_t *ret = get_all_patches();

  {
    vector_t *all_patches = pointer;
  
    VECTOR_clean(&get_MIDI_instrument()->patches);
    VECTOR_clean(&get_audio_instrument()->patches);

    VECTOR_append(&get_MIDI_instrument()->patches,all_patches->elements[0]);
    VECTOR_append(&get_audio_instrument()->patches,all_patches->elements[1]);

    VECTOR_FOR_EACH(struct Patch *patch,all_patches->elements[0]){
      patch->is_usable = true;
    }END_VECTOR_FOR_EACH;
    VECTOR_FOR_EACH(struct Patch *patch,all_patches->elements[1]){
      patch->is_usable = true;
    }END_VECTOR_FOR_EACH;
  }

  return ret;
}


