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

#include "SoundPlugin.h"
#include "SoundPlugin_proc.h"

#include "../Qt/Qt_instruments_proc.h"

#include "undo_audio_effect_proc.h"


extern struct Root *root;

struct Undo_Sample{
  struct Patch *patch;
  hash_t *state;
};

static void *Undo_Do_Sample(
                                 struct Tracker_Windows *window,
                                 struct WBlocks *wblock,
                                 struct WTracks *wtrack,
                                 int realline,
                                 void *pointer
                                 );

static void Undo_Sample(
                             struct Tracker_Windows *window,
                             struct WBlocks *wblock,
                             struct Patch *patch
                             )
{
  struct Undo_Sample *undo_ae=talloc(sizeof(struct Undo_Sample));
  SoundPlugin *plugin = patch->patchdata;
  
  undo_ae->patch = patch;
  undo_ae->state = HASH_create(3);

  plugin->type->create_state(plugin,undo_ae->state);

  Undo_Add_dont_stop_playing(
                             window->l.num,
                             wblock->l.num,
                             wblock->wtrack->l.num,
                             wblock->curr_realline,
                             undo_ae,
                             Undo_Do_Sample
                             );

}

void Undo_Sample_CurrPos(struct Patch *patch){
  struct Tracker_Windows *window = root->song->tracker_windows;

  Undo_Sample(window,window->wblock, patch);
}

static void *Undo_Do_Sample(
	struct Tracker_Windows *window,
	struct WBlocks *wblock,
	struct WTracks *wtrack,
	int realline,
	void *pointer
){

  struct Undo_Sample *undo_ae=pointer;
  SoundPlugin *plugin = undo_ae->patch->patchdata;

  hash_t *new_state = HASH_create(3);
  plugin->type->create_state(plugin, new_state);

  //printf("Calling Undo_do for %d. Current value: %f. Now setting it back to %f\n",undo_ae->effect_num,new_value,undo_ae->value);

  plugin->type->recreate_from_state(plugin, undo_ae->state);

  GFX_update_instrument_widget(undo_ae->patch);

  undo_ae->state = new_state;

  return undo_ae;
}

