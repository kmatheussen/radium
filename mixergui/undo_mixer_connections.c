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

#include "QM_MixerWidget.h"

#include "undo_mixer_connections_proc.h"


extern struct Root *root;

#if 0
struct Undo_RelTempoMax{
  vector_t *state;
};
#endif

static void *Undo_Do_MixerConnections(
	struct Tracker_Windows *window,
	struct WBlocks *wblock,
	struct WTracks *wtrack,
	int realline,
	void *pointer
);

static void Undo_MixerConnections(
                                  struct Tracker_Windows *window,
                                  struct WBlocks *wblock
){
  //struct Undo_MixerConnections *u_rt=talloc(sizeof(struct Undo_MixerConnections));
  //u_rt->reltempomax=wblock->reltempomax;

  Undo_Add_dont_stop_playing(
                             window->l.num,
                             wblock->l.num,
                             wblock->wtrack->l.num,
                             wblock->curr_realline,
                             MW_get_connections_state(NULL, true, true, true),
                             Undo_Do_MixerConnections,
                             "Mixer connections"
                             );

}

void ADD_UNDO_FUNC(MixerConnections_CurrPos(void)){
  struct Tracker_Windows *window = root->song->tracker_windows;
  Undo_MixerConnections(window,window->wblock);
}

static void *Undo_Do_MixerConnections(
	struct Tracker_Windows *window,
	struct WBlocks *wblock,
	struct WTracks *wtrack,
	int realline,
	void *pointer
){

  hash_t *current_state = MW_get_connections_state(NULL, true, true, true);
  MW_create_connections_from_state(pointer);

  return current_state;
}


