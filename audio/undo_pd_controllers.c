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
#include "../common/OS_Player_proc.h"

#include "SoundPlugin.h"
#include "SoundPlugin_proc.h"

#include "../Qt/Qt_instruments_proc.h"

#include "Pd_plugin_proc.h"

#include "undo_pd_controllers_proc.h"


extern struct Root *root;

struct Undo_PdControllers{
  struct Patch *patch;
  hash_t *state;
};

static void *Undo_Do_PdControllers(
                                 struct Tracker_Windows *window,
                                 struct WBlocks *wblock,
                                 struct WTracks *wtrack,
                                 int realline,
                                 void *pointer
                                 );

static void Undo_PdControllers(
                             struct Tracker_Windows *window,
                             struct WBlocks *wblock,
                             struct Patch *patch
                             )
{
  struct Undo_PdControllers *undo_ae=talloc(sizeof(struct Undo_PdControllers));
  SoundPlugin *plugin = patch->patchdata;
  R_ASSERT_RETURN_IF_FALSE(plugin!=NULL);
  
  undo_ae->patch = patch;
  undo_ae->state = HASH_create(plugin->type->num_effects);
  PD_put_controllers_to_state(plugin, undo_ae->state);

  printf("********* Storing pd controllers undo.\n");

  Undo_Add_dont_stop_playing(
                             window->l.num,
                             wblock->l.num,
                             wblock->wtrack->l.num,
                             wblock->curr_realline,
                             undo_ae,
                             Undo_Do_PdControllers,
                             "Pd controllers"
                             );

}

void ADD_UNDO_FUNC(PdControllers_CurrPos(struct Patch *patch)){
  struct Tracker_Windows *window = root->song->tracker_windows;
  printf("Undo_PdControllers_CurrPos\n");
  Undo_PdControllers(window,window->wblock, patch);
}

static void *Undo_Do_PdControllers(
	struct Tracker_Windows *window,
	struct WBlocks *wblock,
	struct WTracks *wtrack,
	int realline,
	void *pointer
){

  struct Undo_PdControllers *undo_ae=pointer;
  SoundPlugin *plugin = undo_ae->patch->patchdata;
  R_ASSERT_RETURN_IF_FALSE2(plugin!=NULL, pointer);
  
  hash_t *new_state = HASH_create(plugin->type->num_effects);
  PD_put_controllers_to_state(plugin, new_state);

  printf("Calling Undo_do for Pd controllers\n");

  PD_recreate_controllers_from_state(plugin, undo_ae->state);

  undo_ae->state = new_state;

  return undo_ae;
}
